{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Ceca.Parser where

import Ceca.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.List (foldl')

type Parser = Parsec Void Text

-- Lexer configuration
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- Bracketing helpers
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- Basic tokens
identifier :: Parser Text
identifier = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_' <|> char '\'')
    return $ T.pack (first:rest)

typeVar :: Parser Text
typeVar = lexeme $ do
    char '?'
    name <- some (alphaNumChar <|> char '_' <|> char '\'')
    return $ T.pack ('?':name)

stringLiteral :: Parser Text
stringLiteral = lexeme $ do
    char '"'
    content <- manyTill L.charLiteral (char '"')
    return $ T.pack content

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

bool :: Parser Bool
bool = lexeme $
    (string "true" >> return True) <|>
    (string "false" >> return False)

-- Helper to capture source positions
withSpan :: Parser a -> Parser (Span a)
withSpan p = do
    start <- getSourcePos
    node <- p
    end <- getSourcePos
    return $ MkSpan start end node