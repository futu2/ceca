{-# LANGUAGE OverloadedStrings #-}

module Ceca.Parser.Basic where

import Ceca.AST
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad (guard)

type Parser = Parsec Void Text

-- Lexer configuration
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
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
identifier =
  choice
    [ try operatorIdentifier
    , try normalIdentifier
    ]

normalIdentifier :: Parser Text
normalIdentifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  return $ T.pack (first : rest)

operatorIdentifier :: Parser Text
operatorIdentifier = lexeme $ do
  _ <- char '_' >> char '_'
  op <- operatorName
  _ <- char '_' >> char '_'
  return $ "__" <> op <> "__"

infixOperator :: Parser Text
infixOperator = lexeme $ do
  operatorName

operatorName :: Parser Text
operatorName = do
  opName <- some $ oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)
  guard $ opName /= "="
  guard $ opName /= "|"
  return $ T.pack opName

typeVar :: Parser Text
typeVar = lexeme $ do
  _ <- char '?'
  name <- some (alphaNumChar <|> char '_' <|> char '\'')
  return $ T.pack ('?' : name)

escapeChar :: Parser Char
escapeChar =
  char '\\'
    *> choice
      [ '\n' <$ char 'n' -- newline
      , '\"' <$ char '\"' -- double quote
      , '\\' <$ char '\\' -- backslash
      , '\t' <$ char 't' -- tab
      , '\r' <$ char 'r' -- carriage return
      , '\f' <$ char 'f' -- form feed
      , '\b' <$ char 'b' -- backspace
      ]

-- Parse a string literal with escape sequences
stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

integer :: Parser Integer
integer =
  lexeme $
    L.signed (return ()) L.decimal

float :: Parser Double
float =
  lexeme $
    L.signed (return ()) L.float

bool :: Parser Bool
bool =
  lexeme $
    (string "true" >> return True)
      <|> (string "false" >> return False)

-- Parser for literals
parseLiteral :: Parser Literal
parseLiteral =
  lexeme $
    choice
      [ try $ LFloat <$> float
      , LInt <$> integer
      , LString <$> stringLiteral
      , LBool <$> bool
      ]

parseImportExpr :: Parser ExprNode
parseImportExpr = lexeme $ do
  _ <- symbol "@"
  path <- some (alphaNumChar <|> char '.' <|> char '/' <|> char '_' <|> char '-')
  return $ EImport path

-- Helper to capture source positions
withSpan :: Parser a -> Parser (Span a)
withSpan p = do
  start <- getSourcePos
  node <- p
  end <- getSourcePos
  return $ MkSpan start end node
