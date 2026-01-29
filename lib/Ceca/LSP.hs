{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP where

import Ceca.AST (ExprNode(..), PatternNode(..), Span(..), TypeNode(..), spanNode)
import Ceca.Parser (parseProgram)
import Ceca.Parser.Expr (parseExpr)
import Ceca.TypeChecker (Env, generalize, typeCheck, typeCheckWithEnv)
import Ceca.Types (decomposeRecord)
import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import System.Exit
import Text.Megaparsec (runParser)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)

type DocState = IORef (Map LSP.Uri T.Text)

completionDummyField :: T.Text
completionDummyField = "__ceca_dummy__"

data ApplyRule = ApplyLeftRight | ApplyRightLeft deriving (Eq, Show)

type OpRules = Map T.Text ApplyRule

runLSPServer :: IO ()
runLSPServer = do
  docState <- newIORef Map.empty
  ec <- runServer (serverDefinition docState)
  case ec of
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- LSP server for Ceca language with record field autocompletion
serverDefinition :: DocState -> ServerDefinition ()
serverDefinition docState =
  ServerDefinition
    { parseConfig = const $ const $ Right ()
    , onConfigChange = const $ pure ()
    , defaultConfig = ()
    , configSection = "ceca"
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = \_caps -> handlers docState
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options =
        defaultOptions
          { optTextDocumentSync =
              Just
                LSP.TextDocumentSyncOptions
                  { _openClose = Just True
                  , _change = Just LSP.TextDocumentSyncKind_Incremental
                  , _willSave = Nothing
                  , _willSaveWaitUntil = Nothing
                  , _save = Nothing
                  }
          , optCompletionTriggerCharacters = Just ['.']
          }
    }

-- LSP handlers for Ceca language
handlers :: DocState -> Handlers (LspM ())
handlers docState =
  mconcat
     [ notificationHandler SMethod_TextDocumentDidOpen $ \not -> do
         let params = case not of TNotificationMessage _ _ p -> p
             LSP.DidOpenTextDocumentParams { _textDocument = textDoc } = params
             LSP.TextDocumentItem { _uri = uri, _text = text } = textDoc
         liftIO $ modifyIORef docState $ Map.insert uri text
     , notificationHandler SMethod_TextDocumentDidChange $ \not -> do
         let params = case not of TNotificationMessage _ _ p -> p
             LSP.DidChangeTextDocumentParams { _textDocument = docId, _contentChanges = contentChanges } = params
             LSP.VersionedTextDocumentIdentifier { _uri = uri } = docId
         liftIO $ do
           current <- readIORef docState
           let currentText = Map.findWithDefault "" uri current
           let newText = foldl applyContentChange currentText contentChanges
           modifyIORef docState $ Map.insert uri newText
     , notificationHandler SMethod_Initialized $ \_not -> pure ()
     , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not -> pure ()
     , requestHandler SMethod_TextDocumentCompletion $ completionHandler docState
     ]

applyContentChange :: T.Text -> LSP.TextDocumentContentChangeEvent -> T.Text
applyContentChange current (LSP.TextDocumentContentChangeEvent change) =
  case change of
    LSP.InR LSP.TextDocumentContentChangeWholeDocument { _text = newText } ->
      newText
    LSP.InL LSP.TextDocumentContentChangePartial { _range = range, _text = newText } ->
      case range of
        LSP.Range { _start = startPos, _end = endPos } ->
          let startOffset = positionToOffset current startPos
              endOffset = positionToOffset current endPos
          in T.take startOffset current <> newText <> T.drop endOffset current

positionToOffset :: T.Text -> LSP.Position -> Int
positionToOffset text pos =
  let LSP.Position { _line = line, _character = ch } = pos
      lineIndex = fromIntegral line
      charIndex = fromIntegral ch
      lines = T.splitOn "\n" text
      lineCount = length lines
      clampLine = max 0 lineIndex
  in if clampLine >= lineCount
       then T.length text
       else
         let (pre, rest) = splitAt clampLine lines
             preLen = sum (map T.length pre) + length pre
             currentLine = case rest of
               [] -> ""
               (l:_) -> l
             col = min charIndex (T.length currentLine)
         in preLen + col

completionHandler :: DocState -> TRequestMessage Method_TextDocumentCompletion -> (Either (TResponseError Method_TextDocumentCompletion) ([LSP.CompletionItem] LSP.|? (LSP.CompletionList LSP.|? LSP.Null)) -> LspT () IO ()) -> LspT () IO ()
completionHandler docState req responder = do
  let params = case req of TRequestMessage _ _ _ p -> p
      LSP.CompletionParams { _textDocument = textDoc, _position = pos } = params
      LSP.TextDocumentIdentifier { _uri = uri } = textDoc
  text <- liftIO $ Map.lookup uri <$> readIORef docState
  case text of
    Nothing -> responder $ Right $ LSP.InR $ LSP.InR LSP.Null
    Just txt -> do
      let (docEnv, _) = buildEnvForPosition pos txt
      let lines = T.lines txt
          LSP.Position { _line = ln, _character = ch } = pos
          currentLine = if fromIntegral ln < length lines then lines !! fromIntegral ln else ""
          beforeCursor = T.take (fromIntegral ch) currentLine
          (exprWithDot, fieldPrefix) = T.breakOnEnd "." beforeCursor
          hasDot = T.isSuffixOf "." exprWithDot
          exprText = T.dropEnd 1 exprWithDot
      if not hasDot || T.null exprText then
        responder $ Right $ LSP.InR $ LSP.InR LSP.Null
      else do
        let completionExpr =
              case findProjectionTargetAtPosition pos txt of
                Just expr -> Just expr
                Nothing -> case runParser parseExpr "" exprText of
                  Left _ -> Nothing
                  Right expr -> Just expr
        case completionExpr of
          Nothing -> responder $ Right $ LSP.InR $ LSP.InR LSP.Null
          Just expr -> do
            let fields = case typeCheckWithEnv docEnv expr of
                  Right typ -> recordFields typ
                  Left _ -> []
            let filteredFields =
                  if T.null fieldPrefix then fields else filter (T.isPrefixOf fieldPrefix) fields
            let items = map (\f -> LSP.CompletionItem { _label = f, _kind = Nothing, _labelDetails = Nothing, _tags = Nothing, _detail = Nothing, _documentation = Nothing, _deprecated = Nothing, _preselect = Nothing, _sortText = Nothing, _filterText = Nothing, _insertText = Nothing, _insertTextFormat = Nothing, _insertTextMode = Nothing, _textEdit = Nothing, _textEditText = Nothing, _additionalTextEdits = Nothing, _commitCharacters = Nothing, _command = Nothing, _data_ = Nothing }) filteredFields
            responder $ Right $ LSP.InR $ LSP.InL $ LSP.CompletionList False Nothing items

buildEnvForPosition :: LSP.Position -> T.Text -> (Env, OpRules)
buildEnvForPosition pos txt =
  case runParser parseProgram "" txt of
    Right fullExpr -> collectEnvAtPosition pos fullExpr
    Left _ ->
      let txt' = insertDummyAtPosition pos completionDummyField txt
      in case runParser parseProgram "" txt' of
           Right fullExpr -> collectEnvAtPosition pos fullExpr
           Left _ -> (Map.empty, Map.empty)

insertDummyAtPosition :: LSP.Position -> T.Text -> T.Text -> T.Text
insertDummyAtPosition pos dummy txt =
  let offset = positionToOffset txt pos
      (before, after) = T.splitAt offset txt
  in before <> dummy <> after

findProjectionTargetAtPosition :: LSP.Position -> T.Text -> Maybe (Span ExprNode)
findProjectionTargetAtPosition pos txt =
  let txt' = insertDummyAtPosition pos completionDummyField txt
  in case runParser parseProgram "" txt' of
       Right fullExpr -> findDummyProjection pos fullExpr
       Left _ -> Nothing

findDummyProjection :: LSP.Position -> Span ExprNode -> Maybe (Span ExprNode)
findDummyProjection pos expr = go expr
  where
    go node = case spanNode node of
      EProj base label
        | label == completionDummyField && posInSpan pos node -> Just base
        | otherwise -> go base
      ELet _ _ e1 e2 -> firstJust (go e1) (go e2)
      EApp e1 e2 -> firstJust (go e1) (go e2)
      EAbs _ body -> go body
      EAnnot e _ -> go e
      EExtend base _ val -> firstJust (go base) (go val)
      ERestrict base _ -> go base
      ERecord fields -> goList (map snd fields)
      ETuple es -> goList es
      EArray es -> goList es
      _ -> Nothing

    goList exprs = foldr (\e acc -> firstJust (go e) acc) Nothing exprs

    firstJust left right = case left of
      Just _ -> left
      Nothing -> right

collectEnvAtPosition :: LSP.Position -> Span ExprNode -> (Env, OpRules)
collectEnvAtPosition pos expr = go (Map.empty, Map.empty) expr
  where
    go (env, rules) node = case spanNode node of
      ELet pat _ e1 e2
        | posInSpan pos e1 -> go (env, rules) e1
        | posInSpan pos e2 ->
            let (env', rules') = extendEnv env rules pat e1
            in go (env', rules') e2
        | otherwise -> (env, rules)
      EApp e1 e2 ->
        case spanNode e1 of
          EApp e11 e12 ->
            case spanNode e11 of
              EAbs pat body
                | posInSpan pos e12 -> go (env, rules) e12
                | posInSpan pos e2 -> go (env, rules) e2
                | posInSpan pos body ->
                    let env' = extendEnvWithArg env pat e12
                    in handleApply env' rules body e2
                | posInSpan pos e11 -> go (env, rules) e11
                | otherwise -> (env, rules)
              EVar opName ->
                case Map.lookup opName rules of
                  Just ApplyLeftRight -> handleApply env rules e12 e2
                  Just ApplyRightLeft -> handleApply env rules e2 e12
                  Nothing
                    | posInSpan pos e1 -> go (env, rules) e1
                    | posInSpan pos e2 -> go (env, rules) e2
                    | otherwise -> (env, rules)
              _ | posInSpan pos e1 -> go (env, rules) e1
                | posInSpan pos e2 -> go (env, rules) e2
                | otherwise -> (env, rules)
          EAbs pat body
            | posInSpan pos body ->
                let env' = extendEnvWithArg env pat e2
                in go (env', rules) body
          _ | posInSpan pos e1 -> go (env, rules) e1
            | posInSpan pos e2 -> go (env, rules) e2
            | otherwise -> (env, rules)
      EAbs _ body
        | posInSpan pos body -> go (env, rules) body
        | otherwise -> (env, rules)
      EAnnot e _ | posInSpan pos e -> go (env, rules) e
      EProj e _ | posInSpan pos e -> go (env, rules) e
      EExtend base _ val
        | posInSpan pos val -> go (env, rules) val
        | posInSpan pos base -> go (env, rules) base
        | otherwise -> (env, rules)
      ERestrict base _ | posInSpan pos base -> go (env, rules) base
      ERecord fields -> goList (env, rules) (map snd fields)
      ETuple es -> goList (env, rules) es
      EArray es -> goList (env, rules) es
      _ -> (env, rules)

    goList acc exprs = case exprs of
      [] -> acc
      (e:rest)
        | posInSpan pos e -> go acc e
        | otherwise -> goList acc rest

    handleApply env rules fn arg =
      case spanNode fn of
        EAbs pat body
          | posInSpan pos body ->
              let env' = extendEnvWithArg env pat arg
              in go (env', rules) body
        _ | posInSpan pos fn -> go (env, rules) fn
          | posInSpan pos arg -> go (env, rules) arg
          | otherwise -> (env, rules)

extendEnv :: Env -> OpRules -> Span PatternNode -> Span ExprNode -> (Env, OpRules)
extendEnv env rules pat expr =
  let rules' =
        case spanNode pat of
          PVar name -> case detectApplyRule expr of
            Just rule -> Map.insert name rule rules
            Nothing -> rules
          _ -> rules
  in case spanNode pat of
  PVar name ->
    case typeCheckWithEnv env expr of
      Right ty -> (Map.insert name (generalize env ty) env, rules')
      Left _ -> (env, rules')
  _ -> (env, rules')

extendEnvWithArg :: Env -> Span PatternNode -> Span ExprNode -> Env
extendEnvWithArg env pat argExpr = case spanNode pat of
  PVar name ->
    case typeCheckWithEnv env argExpr of
      Right ty -> Map.insert name (generalize env ty) env
      Left _ -> env
  _ -> env

detectApplyRule :: Span ExprNode -> Maybe ApplyRule
detectApplyRule expr = case spanNode expr of
  EAbs p1 body1 ->
    case spanNode body1 of
      EAbs p2 body2 ->
        case spanNode body2 of
          EApp f arg ->
            case (spanNode f, spanNode arg, patternVarName p1, patternVarName p2) of
              (EVar fName, EVar argName, Just n1, Just n2)
                | fName == n1 && argName == n2 -> Just ApplyLeftRight
                | fName == n2 && argName == n1 -> Just ApplyRightLeft
                | otherwise -> Nothing
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
  _ -> Nothing

patternVarName :: Span PatternNode -> Maybe T.Text
patternVarName pat = case spanNode pat of
  PVar name -> Just name
  _ -> Nothing

recordFields :: Span TypeNode -> [T.Text]
recordFields typ = case spanNode typ of
  TRecordEmpty -> []
  TRecordExtend {} -> Map.keys (fst (decomposeRecord (spanNode typ)))
  TVar {} -> Map.keys (fst (decomposeRecord (spanNode typ)))
  _ -> []

posInSpan :: LSP.Position -> Span a -> Bool
posInSpan pos span =
  let (line, col) = lspPositionToLineCol pos
      startPos = spanStart span
      endPos = spanEnd span
  in posGeq (line, col) startPos && posLeq (line, col) endPos

posLeq :: (Int, Int) -> SourcePos -> Bool
posLeq (line, col) sp =
  let spLine = unPos (sourceLine sp)
      spCol = unPos (sourceColumn sp)
  in line < spLine || (line == spLine && col <= spCol)

posGeq :: (Int, Int) -> SourcePos -> Bool
posGeq (line, col) sp =
  let spLine = unPos (sourceLine sp)
      spCol = unPos (sourceColumn sp)
  in line > spLine || (line == spLine && col >= spCol)

lspPositionToLineCol :: LSP.Position -> (Int, Int)
lspPositionToLineCol LSP.Position { _line = line, _character = ch } =
  (fromIntegral line + 1, fromIntegral ch + 1)
