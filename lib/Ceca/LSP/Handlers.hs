{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.Handlers
  ( handlers
  ) where

import Ceca.LSP.Completion (completionHandler)
import Ceca.LSP.Diagnostics (clearDiagnosticsForDoc, publishDiagnosticsForDoc)
import Ceca.LSP.Position (positionToOffset)
import Ceca.LSP.Types (DocState)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef, readIORef)
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Handlers, LspM, notificationHandler, requestHandler)

-- LSP handlers for Ceca language
handlers :: DocState -> Handlers (LspM ())
handlers docState =
  mconcat
     [ notificationHandler SMethod_TextDocumentDidOpen $ \not -> do
         let params = case not of TNotificationMessage _ _ p -> p
             LSP.DidOpenTextDocumentParams { _textDocument = textDoc } = params
             LSP.TextDocumentItem { _uri = uri, _text = text } = textDoc
         liftIO $ modifyIORef docState $ Map.insert uri text
         publishDiagnosticsForDoc uri text
     , notificationHandler SMethod_TextDocumentDidChange $ \not -> do
         let params = case not of TNotificationMessage _ _ p -> p
             LSP.DidChangeTextDocumentParams { _textDocument = docId, _contentChanges = contentChanges } = params
             LSP.VersionedTextDocumentIdentifier { _uri = uri } = docId
         newText <- liftIO $ do
           current <- readIORef docState
           let currentText = Map.findWithDefault "" uri current
           let newText = foldl applyContentChange currentText contentChanges
           modifyIORef docState $ Map.insert uri newText
           return newText
         publishDiagnosticsForDoc uri newText
     , notificationHandler SMethod_TextDocumentDidClose $ \not -> do
         let params = case not of TNotificationMessage _ _ p -> p
             LSP.DidCloseTextDocumentParams { _textDocument = textDoc } = params
             LSP.TextDocumentIdentifier { _uri = uri } = textDoc
         liftIO $ modifyIORef docState $ Map.delete uri
         clearDiagnosticsForDoc uri
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
