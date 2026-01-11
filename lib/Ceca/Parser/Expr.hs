{-# LANGUAGE OverloadedStrings #-}

module Ceca.Parser.Expr where

import Ceca.AST
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Text.Megaparsec

import Ceca.Parser.Basic
import Ceca.Parser.Pattern (parsePattern)
import Ceca.Parser.Type
import Control.Monad (guard, void)

-- Helper for reverse application (like Haskell's &)
reverseBinary :: Expr -> Expr -> Expr
reverseBinary e1 e2 = MkSpan (spanStart e1) (spanEnd e2) (EApp e2 e1)

-- Helper for binary application
binary :: Expr -> Expr -> Expr
binary e1 e2 = MkSpan (spanStart e1) (spanEnd e2) (EApp e1 e2)

-- Operator table for infix operators
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [InfixL (reverseBinary <$ symbol "&")]
  , [InfixR (binary <$ symbol "$")]
  ]

-- Parser for expressions without infix
parseTerm :: Parser Expr
parseTerm =
  choice
    [ try (withSpan parseLambdaExpr)
    , try (withSpan parseLetExpr)
    , try (withSpan parseExtendRestrictExpr)
    , try (withSpan parseRecordExpr)
    , try (withSpan parseTupleExpr)
    , try (withSpan parseArrayExpr)
    , try (withSpan parseImportExpr)
    , try parseProjExpr
    , try (withSpan parseAppExpr)
    ]

parseAtomicExpr :: Parser Expr
parseAtomicExpr =
  withSpan $
    choice
      [ ELit <$> parseLiteral
      , EVar <$> identifier
      , try parseRecordExpr
      , parens parseExprNode
      ]

parseProjExpr :: Parser Expr
parseProjExpr = do
  expr <- parseAtomicExpr
  more <- some parseProjSuffix
  return $ foldl' (\e f -> f e) expr more
  where
    parseProjSuffix :: Parser (Expr -> Expr)
    parseProjSuffix =
      choice
        [ do
            _ <- symbol "."
            name <- identifier
            return $ \e -> MkSpan (spanStart e) (spanEnd e) (EProj e name)
        , do
            args <- parens (some parseAtomicExpr)
            return $ \e ->
              foldl'
                ( \f arg ->
                    let start = spanStart f
                        end = spanEnd arg
                     in MkSpan start end (EApp f arg)
                )
                e
                args
        ]

parseExtendRestrictExpr :: Parser ExprNode
parseExtendRestrictExpr = braces $ do
  ops <- parseOps
  _ <- symbol "|"
  baseExpr <- parseExpr
  let result = foldl applyOp baseExpr ops
  return $ spanNode result
  where
    parseOps :: Parser [Expr -> Expr]
    parseOps =
      many $
        choice
          [ do
              opStart <- getSourcePos
              _ <- symbol "-"
              name <- identifier
              -- opEnd <- getSourcePos
              _ <- optional $ symbol ","
              return $ \e ->
                MkSpan opStart (spanEnd e) (ERestrict e name)
          , do
              opStart <- getSourcePos
              name <- identifier
              _ <- symbol "="
              val <- parseExpr
              -- opEnd <- getSourcePos
              _ <- optional $ symbol ","
              return $ \e ->
                MkSpan opStart (spanEnd e) (EExtend e name val)
          ]

    applyOp :: Expr -> (Expr -> Expr) -> Expr
    applyOp expr op = op expr

parseRecordExpr :: Parser ExprNode
parseRecordExpr = braces $ do
  -- Check if record is empty
  isEmpty <- optional (lookAhead (void (symbol "}")))
  case isEmpty of
    Just _ -> return $ ERecord []
    Nothing -> do
      fields <- parseRecordField `sepBy` symbol ","
      optional (symbol ",")
      return $ ERecord fields
  where
    parseRecordField = do
      name <- identifier
      symbol "="
      expr <- parseExpr
      return (name, expr)

parseTupleExpr :: Parser ExprNode
parseTupleExpr = do
  exprs <- parens (parseExpr `sepBy1` symbol ",")
  guard $ length exprs > 1
  return $ ETuple exprs

parseArrayExpr :: Parser ExprNode
parseArrayExpr = do
  exprs <- brackets (parseExpr `sepBy` symbol ",")
  return $ EArray exprs

parseLambdaExpr :: Parser ExprNode
parseLambdaExpr = do
  pat <- parsePattern
  _ <- symbol "=>"
  expr <- parseExpr
  return $ EAbs pat expr

parseLetExpr :: Parser ExprNode
parseLetExpr = do
  _ <- symbol "let"
  name <- identifier
  typ <- optional (symbol ":" >> parseType)
  _ <- symbol "="
  val <- parseExpr
  _ <- symbol ";"
  body <- parseExpr
  return $ ELet name typ val body

parseAnnotExpr :: Parser ExprNode
parseAnnotExpr = do
  expr <- makeExprParser parseTerm operatorTable
  _ <- symbol ":"
  typ <- parseType
  return $ EAnnot expr typ

parseAppExpr :: Parser ExprNode
parseAppExpr = do
  func <- parseAtomicExpr
  args <- many parseAtomicExpr
  let resultExpr =
        foldl'
          ( \f arg ->
              let start = spanStart f
                  end = spanEnd arg
               in MkSpan start end (EApp f arg)
          )
          func
          args
  return $ spanNode resultExpr

parseExprNode :: Parser ExprNode
parseExprNode = fmap spanNode parseExpr

parseExpr :: Parser Expr
parseExpr =
  choice
    [ try (withSpan parseAnnotExpr)
    , makeExprParser parseTerm operatorTable
    ]
