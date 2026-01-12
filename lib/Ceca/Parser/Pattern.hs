{-# LANGUAGE OverloadedStrings #-}

module Ceca.Parser.Pattern where

import Ceca.AST
import Ceca.Parser.Basic
import Control.Monad (void)
import Data.Text (Text)
import Text.Megaparsec

-- Parser for patterns
parsePatternNode :: Parser PatternNode
parsePatternNode =
  choice
    [ try parseRecordPattern
    , try parseTuplePattern
    , try parseArrayPattern
    , parseLiteralPattern
    , try parseVarPattern
    , parseWildcardPattern
    ]

parseVarPattern :: Parser PatternNode
parseVarPattern = do
  name <- identifier -- This will parse _a as a whole identifier
  if name == "_"
    then fail "Single underscore is wildcard" -- backtrack
    else return $ PVar name

parseWildcardPattern :: Parser PatternNode
parseWildcardPattern = PWildcard <$ symbol "_"

parseLiteralPattern :: Parser PatternNode
parseLiteralPattern = PLit <$> parseLiteral

parseTuplePattern :: Parser PatternNode
parseTuplePattern = do
  pats <- parens (parsePattern `sepBy1` symbol ",")
  return $ PTuple pats

parseArrayPattern :: Parser PatternNode
parseArrayPattern = do
  pats <- brackets (parsePattern `sepBy` symbol ",")
  return $ PArray pats

parseRecordPattern :: Parser PatternNode
parseRecordPattern =
  braces $
    do
      -- Check if empty
      (lookAhead (void $ symbol "}") >> return (PRecord [] Nothing))
      <|> do
        -- Parse fields
        fields <- manyTill parseRecordFieldPattern (lookAhead restOrEnd)
        -- Parse optional rest
        mrest <- optional $ do
          void $ symbol "|"
          pos <- getSourcePos
          let defaultPat = MkSpan pos pos PWildcard
          pat <- option defaultPat parsePattern
          return pat
        return $ PRecord fields mrest -- NO symbol "}" here!
  where
    restOrEnd :: Parser ()
    restOrEnd = void (symbol "|") <|> void (symbol "}") -- For lookAhead only
    parseRecordFieldPattern :: Parser (Text, Pattern)
    parseRecordFieldPattern = do
      name <- identifier
      mpat <- optional (symbol "=" >> parsePattern)
      optional (symbol ",") -- Consume optional comma after field
      case mpat of
        Just pat -> return (name, pat)
        Nothing -> do
          start <- getSourcePos
          return (name, MkSpan start start (PVar name))

parsePattern :: Parser Pattern
parsePattern = withSpan parsePatternNode
