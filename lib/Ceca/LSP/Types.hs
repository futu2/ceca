{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.Types
  ( DocState
  , completionDummyField
  , ApplyRule(..)
  , OpRules
  ) where

import Data.IORef (IORef)
import Data.Map (Map)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP

type DocState = IORef (Map LSP.Uri T.Text)

completionDummyField :: T.Text
completionDummyField = "__ceca_dummy__"

data ApplyRule = ApplyLeftRight | ApplyRightLeft deriving (Eq, Show)

type OpRules = Map T.Text ApplyRule
