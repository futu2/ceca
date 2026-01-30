{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.SemanticTokens
  ( semanticTokensHandler
  , semanticTokensRegistrationOptions
  , semanticTokensOptions
  ) where

import Ceca.LSP.Types (DocState)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.IORef (readIORef)
import Data.List (findIndex, isPrefixOf, tails)
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (LspT)

data TokenClass
  = TokKeyword
  | TokType
  | TokFunction
  | TokProperty
  | TokString
  | TokNumber
  | TokComment
  | TokOperator
  | TokVariable
  | TokMacro
  | TokTypeParameter
  deriving (Eq, Show)

data Token = Token Int Int Int TokenClass

semanticTokensOptions :: LSP.SemanticTokensOptions
semanticTokensOptions =
  LSP.SemanticTokensOptions
    { _workDoneProgress = Nothing
    , _legend = semanticTokensLegend
    , _range = Nothing
    , _full = Just (LSP.InL True)
    }

semanticTokensRegistrationOptions :: LSP.SemanticTokensRegistrationOptions
semanticTokensRegistrationOptions =
  LSP.SemanticTokensRegistrationOptions
    { _documentSelector = LSP.InL cecaDocumentSelector
    , _workDoneProgress = Nothing
    , _legend = semanticTokensLegend
    , _range = Nothing
    , _full = Just (LSP.InL True)
    , _id = Nothing
    }

cecaDocumentSelector :: LSP.DocumentSelector
cecaDocumentSelector =
  LSP.DocumentSelector
    [ LSP.DocumentFilter $
        LSP.InL $
          LSP.TextDocumentFilter $
            LSP.InL $
              LSP.TextDocumentFilterLanguage "ceca" Nothing Nothing
    ]

semanticTokensLegend :: LSP.SemanticTokensLegend
semanticTokensLegend =
  LSP.SemanticTokensLegend
    { _tokenTypes = map (LSP.toEnumBaseType . snd) tokenLegend
    , _tokenModifiers = []
    }

tokenLegend :: [(TokenClass, LSP.SemanticTokenTypes)]
tokenLegend =
  [ (TokKeyword, LSP.SemanticTokenTypes_Keyword)
  , (TokType, LSP.SemanticTokenTypes_Type)
  , (TokFunction, LSP.SemanticTokenTypes_Function)
  , (TokProperty, LSP.SemanticTokenTypes_Property)
  , (TokString, LSP.SemanticTokenTypes_String)
  , (TokNumber, LSP.SemanticTokenTypes_Number)
  , (TokComment, LSP.SemanticTokenTypes_Comment)
  , (TokOperator, LSP.SemanticTokenTypes_Operator)
  , (TokVariable, LSP.SemanticTokenTypes_Variable)
  , (TokMacro, LSP.SemanticTokenTypes_Macro)
  , (TokTypeParameter, LSP.SemanticTokenTypes_TypeParameter)
  ]

tokenTypeIndex :: TokenClass -> Int
tokenTypeIndex tok =
  case tok of
    TokKeyword -> 0
    TokType -> 1
    TokFunction -> 2
    TokProperty -> 3
    TokString -> 4
    TokNumber -> 5
    TokComment -> 6
    TokOperator -> 7
    TokVariable -> 8
    TokMacro -> 9
    TokTypeParameter -> 10

semanticTokensHandler
  :: DocState
  -> TRequestMessage Method_TextDocumentSemanticTokensFull
  -> (Either (TResponseError Method_TextDocumentSemanticTokensFull) (LSP.SemanticTokens LSP.|? LSP.Null) -> LspT () IO ())
  -> LspT () IO ()
semanticTokensHandler docState req responder = do
  let params = case req of TRequestMessage _ _ _ p -> p
      LSP.SemanticTokensParams { _textDocument = textDoc } = params
      LSP.TextDocumentIdentifier { _uri = uri } = textDoc
  text <- liftIO $ Map.lookup uri <$> readIORef docState
  let tokens = maybe [] scanTokens text
  responder $ Right $ LSP.InL LSP.SemanticTokens
    { _resultId = Nothing
    , _data_ = encodeTokens tokens
    }

encodeTokens :: [Token] -> [LSP.UInt]
encodeTokens tokens = go 0 0 tokens
  where
    go _ _ [] = []
    go prevLine prevStart (Token line start len klass : rest) =
      let deltaLine = line - prevLine
          deltaStart = if deltaLine == 0 then start - prevStart else start
      in map fromIntegral [deltaLine, deltaStart, len, tokenTypeIndex klass, 0]
           <> go line start rest

scanTokens :: T.Text -> [Token]
scanTokens text = go 0 False False (T.splitOn "\n" text)
  where
    go _ _ _ [] = []
    go line inBlock pendingLet (ln:rest) =
      let (tokens, inBlock', pendingLet') = scanLine line inBlock pendingLet (T.unpack ln)
      in tokens <> go (line + 1) inBlock' pendingLet' rest

scanLine :: Int -> Bool -> Bool -> String -> ([Token], Bool, Bool)
scanLine line inBlock pendingLet lineText = go 0 inBlock pendingLet Nothing lineText []
  where
    go _ inBlock' pendingLet' _ [] acc = (reverse acc, inBlock', pendingLet')
    go col True pendingLet' prevNonSpace chars acc =
      case findSubstr "*/" chars of
        Nothing ->
          let len = length chars
              acc' = addToken acc (Token line col len TokComment)
          in (reverse acc', True, pendingLet')
        Just idx ->
          let len = idx + 2
              rest = drop len chars
              acc' = addToken acc (Token line col len TokComment)
          in go (col + len) False pendingLet' prevNonSpace rest acc'
    go col False pendingLet' prevNonSpace chars acc =
      case chars of
        _ | startsWith "//" chars ->
              let len = length chars
                  acc' = addToken acc (Token line col len TokComment)
              in (reverse acc', False, pendingLet')
          | startsWith "/*" chars ->
              let afterStart = drop 2 chars
              in case findSubstr "*/" afterStart of
                   Nothing ->
                     let len = 2 + length afterStart
                         acc' = addToken acc (Token line col len TokComment)
                     in (reverse acc', True, pendingLet')
                   Just idx ->
                     let len = 2 + idx + 2
                         rest = drop (idx + 2) afterStart
                         acc' = addToken acc (Token line col len TokComment)
                     in go (col + len) False pendingLet' prevNonSpace rest acc'
        '"' : rest ->
              let (len, remaining) = consumeString rest
                  acc' = addToken acc (Token line col len TokString)
              in go (col + len) False pendingLet' (Just '"') remaining acc'
        '%' : rest | startsWith "%" rest ->
              let afterStart = drop 1 rest
              in case findSubstr "%%" afterStart of
                   Nothing ->
                     let len = 2 + length afterStart
                         acc' = addToken acc (Token line col len TokMacro)
                     in (reverse acc', False, pendingLet')
                   Just idx ->
                     let len = 2 + idx + 2
                         remaining = drop (idx + 2) afterStart
                         acc' = addToken acc (Token line col len TokMacro)
                     in go (col + len) False pendingLet' (Just '%') remaining acc'
        '?' : rest ->
              case rest of
                (r:_) | isIdentContinue r ->
                  let (name, remaining) = span isIdentContinue rest
                      len = 1 + length name
                      acc' = addToken acc (Token line col len TokTypeParameter)
                  in go (col + len) False pendingLet' (Just (lastChar name '?')) remaining acc'
                _ ->
                  let acc' = addToken acc (Token line col 1 TokOperator)
                  in go (col + 1) False pendingLet' (Just '?') rest acc'
        c : rest
          | isSpace c -> go (col + 1) False pendingLet' prevNonSpace rest acc
          | isDigit c ->
              let (len, remaining) = consumeNumber (c : rest)
                  numStr = take len (c : rest)
                  acc' = addToken acc (Token line col len TokNumber)
                  pendingLet'' = if pendingLet' then False else pendingLet'
              in go (col + len) False pendingLet'' (Just (lastChar numStr c)) remaining acc'
          | isIdentStart c ->
              let (name, remaining) = span isIdentContinue (c : rest)
                  (tokClass, pendingLet'') = classifyIdent pendingLet' prevNonSpace name remaining
                  acc' = addToken acc (Token line col (length name) tokClass)
              in go (col + length name) False pendingLet'' (Just (last name)) remaining acc'
          | isOperatorChar c ->
              let (op, remaining) = span isOperatorChar (c : rest)
                  acc' = addToken acc (Token line col (length op) TokOperator)
              in go (col + length op) False pendingLet' (Just (last op)) remaining acc'
          | otherwise -> go (col + 1) False pendingLet' prevNonSpace rest acc

addToken :: [Token] -> Token -> [Token]
addToken acc tok@(Token _ _ len _)
  | len <= 0 = acc
  | otherwise = tok : acc

consumeNumber :: String -> (Int, String)
consumeNumber chars =
  let (digits, rest) = span isDigit chars
  in case rest of
       '.' : r@(d:_) | isDigit d ->
         let (frac, rest') = span isDigit r
         in (length digits + 1 + length frac, rest')
       _ -> (length digits, rest)

consumeString :: String -> (Int, String)
consumeString = go 1
  where
    go len [] = (len, [])
    go len (c:cs)
      | c == '"' = (len + 1, cs)
      | c == '\\' =
          case cs of
            [] -> (len + 1, [])
            (_:rest) -> go (len + 2) rest
      | otherwise = go (len + 1) cs

classifyIdent :: Bool -> Maybe Char -> String -> String -> (TokenClass, Bool)
classifyIdent pendingLet prevNonSpace name remaining
  | name `elem` keywords =
      if name == "let"
        then (TokKeyword, True)
        else (TokKeyword, False)
  | name `elem` typeNames = (TokType, pendingLet)
  | isOperatorIdentifier name = (TokOperator, pendingLet)
  | startsUpper name = (TokType, pendingLet)
  | pendingLet = (TokVariable, False)
  | isPropertyContext prevNonSpace remaining = (TokProperty, False)
  | isFunctionContext remaining = (TokFunction, False)
  | otherwise = (TokVariable, False)

isPropertyContext :: Maybe Char -> String -> Bool
isPropertyContext prevNonSpace remaining =
  prevNonSpace == Just '.' || nextNonSpaceChar remaining `elem` [Just ':', Just '=']

isFunctionContext :: String -> Bool
isFunctionContext remaining =
  nextNonSpaceChar remaining == Just '('

nextNonSpaceChar :: String -> Maybe Char
nextNonSpaceChar rest =
  case dropWhile isSpace rest of
    (c:_) -> Just c
    [] -> Nothing

lastChar :: String -> Char -> Char
lastChar [] fallback = fallback
lastChar xs _ = last xs

startsUpper :: String -> Bool
startsUpper name =
  case name of
    (c:_) -> isUpper c
    [] -> False

keywords :: [String]
keywords = ["let", "true", "false"]

typeNames :: [String]
typeNames = ["string", "int", "float", "bool"]

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentContinue :: Char -> Bool
isIdentContinue c = isAlphaNum c || c == '_' || c == '\''

isOperatorIdentifier :: String -> Bool
isOperatorIdentifier name =
  let len = length name
  in len > 4
     && "__" `isPrefixOf` name
     && "__" `isPrefixOf` reverse name
     && all isOperatorChar (take (len - 4) (drop 2 name))

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

startsWith :: String -> String -> Bool
startsWith prefix value = prefix `isPrefixOf` value

findSubstr :: String -> String -> Maybe Int
findSubstr needle haystack =
  findIndex (isPrefixOf needle) (tails haystack)
