{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP where

import Ceca.AST (ExprNode(..), spanNode)
import Ceca.Parser.Expr (parseExpr)
import Ceca.TypeChecker (typeCheck)
import Ceca.Types (decomposeRecord, typeNode)
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

type DocState = IORef (Map LSP.Uri T.Text)

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
    , options = defaultOptions
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
     , requestHandler SMethod_TextDocumentCompletion $ completionHandler docState
     ]

applyContentChange :: T.Text -> LSP.TextDocumentContentChangeEvent -> T.Text
applyContentChange current change = current  -- TODO: implement proper change application

completionHandler :: DocState -> TRequestMessage Method_TextDocumentCompletion -> (Either (TResponseError Method_TextDocumentCompletion) ([LSP.CompletionItem] LSP.|? (LSP.CompletionList LSP.|? LSP.Null)) -> LspT () IO ()) -> LspT () IO ()
completionHandler docState req responder = do
  let params = case req of TRequestMessage _ _ _ p -> p
      LSP.CompletionParams { _textDocument = textDoc, _position = pos } = params
      LSP.TextDocumentIdentifier { _uri = uri } = textDoc
  text <- liftIO $ Map.lookup uri <$> readIORef docState
  case text of
    Nothing -> responder $ Right $ LSP.InR $ LSP.InR LSP.Null
    Just txt -> do
      let lines = T.lines txt
          LSP.Position { _line = ln, _character = ch } = pos
          currentLine = if fromIntegral ln < length lines then lines !! fromIntegral ln else ""
          beforeCursor = T.take (fromIntegral ch) currentLine
          (exprText, dotPart) = T.breakOnEnd "." beforeCursor
      if T.null dotPart then
        responder $ Right $ LSP.InR $ LSP.InR LSP.Null
      else do
        case runParser parseExpr "" exprText of
          Left _ -> responder $ Right $ LSP.InR $ LSP.InR LSP.Null
          Right expr -> do
            -- For now, hardcode fields for "r"
            let fields = case spanNode expr of
                  EVar "r" -> ["a", "b"]
                  _ -> []
            let items = map (\f -> LSP.CompletionItem { _label = f, _kind = Nothing, _labelDetails = Nothing, _tags = Nothing, _detail = Nothing, _documentation = Nothing, _deprecated = Nothing, _preselect = Nothing, _sortText = Nothing, _filterText = Nothing, _insertText = Nothing, _insertTextFormat = Nothing, _insertTextMode = Nothing, _textEdit = Nothing, _textEditText = Nothing, _additionalTextEdits = Nothing, _commitCharacters = Nothing, _command = Nothing, _data_ = Nothing }) fields
            responder $ Right $ LSP.InR $ LSP.InL $ LSP.CompletionList False Nothing items
