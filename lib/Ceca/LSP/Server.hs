{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.Server
  ( runLSPServer
  , serverDefinition
  ) where

import Ceca.LSP.Handlers (handlers)
import Ceca.LSP.Types (DocState)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import Data.Map qualified as Map
import Language.LSP.Protocol.Types (TextDocumentSyncOptions(..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import System.Exit (ExitCode(..), exitSuccess, exitWith)

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
