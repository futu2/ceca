{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

-- Placeholder LSP server for record field autocompletion
runLSPServer :: IO ()
runLSPServer = putStrLn "LSP server: Basic autocompletion for record fields like 'name', 'cino', etc. when typing after '.'"