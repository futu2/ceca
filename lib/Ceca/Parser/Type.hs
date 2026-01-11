{-# LANGUAGE OverloadedStrings #-}

module Ceca.Parser.Type where

import Ceca.AST
import Ceca.Parser.Basic
import Data.Text (Text)
import Text.Megaparsec

parseType :: Parser Type
parseType = withSpan parseTypeNode

-- Parser for types
parseTypeNode :: Parser TypeNode
parseTypeNode = do
  choice
    [ try parseArrowType
    , try parseBasicType
    , parseTypeVar
    ]

parseTypeVar :: Parser TypeNode
parseTypeVar = TVar <$> typeVar

parseArrayType :: Parser TypeNode
parseArrayType = TArray <$> brackets parseType

parseTupleType :: Parser TypeNode
parseTupleType = do
  types <- parens (parseType `sepBy1` symbol ",")
  return $ TTuple types

parseParensType :: Parser TypeNode
parseParensType = parens parseTypeNode

parseBasicType :: Parser TypeNode
parseBasicType =
  choice
    [ TCon "string" <$ symbol "string"
    , TCon "int" <$ symbol "int"
    , TCon "float" <$ symbol "float"
    , TCon "bool" <$ symbol "bool"
    , try parseArrayType
    , try parseTupleType
    , try parseRecordType
    , parseParensType
    ]

parseArrowType :: Parser TypeNode
parseArrowType = do
  left <- parseNonArrowType
  _ <- symbol "->"
  right <- parseType
  return $ TArrow left right

parseNonArrowType :: Parser Type
parseNonArrowType =
  withSpan $
    choice
      [ parseBasicType
      , parseTypeVar
      , parens parseTypeNode
      ]

parseRecordType :: Parser TypeNode
parseRecordType = braces $ do
  -- Try to parse fields, if not, it's empty
  fds <- optional (try parseRecordFields)
  return $ maybe TRecordEmpty (uncurry buildRecordType) fds

parseRecordFields :: Parser ([(Text, Type)], Maybe Type)
parseRecordFields = do
  -- Parse zero or more fields (each consumes its own comma)
  fields <- many parseField

  -- Check for pipe
  mpipe <- optional (symbol "|")
  case mpipe of
    Nothing ->
      if null fields
        then empty
        else return (fields, Nothing)
    Just _ -> do
      rest <- parseType
      return (fields, Just rest)

parseField :: Parser (Text, Type)
parseField = try $ do
  name <- identifier
  _ <- symbol ":"
  typ <- parseType
  _ <- optional $ symbol ","
  return (name, typ)

buildRecordType :: [(Text, Type)] -> Maybe Type -> TypeNode
buildRecordType [] Nothing = TRecordEmpty
buildRecordType [] (Just rest) = spanNode rest
buildRecordType ((l, t) : fs) rest =
  TRecordExtend l t (MkSpan (spanStart t) (spanEnd t) $ buildRecordType fs rest)
