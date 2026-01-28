{-# LANGUAGE OverloadedStrings #-}

module Ceca.Parser.Type where

import Ceca.AST
import Ceca.Parser.Basic
import Control.Monad (guard)
import Data.Text (Text)
import Text.Megaparsec

parseType :: Parser Type
parseType = withSpan parseTypeNode

-- Parser for types
parseTypeNode :: Parser TypeNode
parseTypeNode =
  choice
    [ try parseArrowType
    , parseTypeAtom
    ]

parseTypeAtom :: Parser TypeNode
parseTypeAtom =
  choice
    [ parseBasicType
    , parseTypeVar
    ]

parseTypeVar :: Parser TypeNode
parseTypeVar = TVar <$> typeVar

parseArrayType :: Parser TypeNode
parseArrayType = TArray <$> brackets parseType

parseTupleType :: Parser TypeNode
parseTupleType = do
  types <- parens (parseType `sepBy1` symbol ",")
  guard (length types > 1)
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
    , try parseParensType
    , try parseTupleType
    , try parseRecordType
    ]

parseArrowType :: Parser TypeNode
parseArrowType = do
  left <- parseNonArrowType
  _ <- symbol "->"
  right <- parseType
  return $ TArrow left right

parseNonArrowType :: Parser Type
parseNonArrowType = withSpan parseTypeAtom

parseRecordType :: Parser TypeNode
parseRecordType = braces $ do
  fields <- many parseRecordField
  mrest <- optional (symbol "|" *> parseType)
  case (fields, mrest) of
    ([], Nothing) -> return TRecordEmpty
    _ -> return $ buildRecordType fields mrest
  where
    parseRecordField = try $ do
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
