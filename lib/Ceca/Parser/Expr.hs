{-# LANGUAGE OverloadedStrings #-}

module Ceca.Parser.Expr where

import Ceca.AST
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Text.Megaparsec

import Ceca.Parser.Basic
import Ceca.Parser.Pattern (parsePattern)
import Ceca.Parser.Type
import Control.Monad (guard)

-- Helper for binary application
binary :: Text -> Expr -> Expr -> Expr
binary opName e1 e2 = MkSpan (spanStart e1) (spanEnd e2) $ EApp firstPart e2
  where
    firstPart =
      MkSpan (spanStart e1) (spanStart e2) $
        EApp
          (MkSpan (spanEnd e1) (spanStart e2) $ EVar $ "__" <> opName <> "__")
          e1

-- Operator table for infix operators
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ -- Level 9: Function application

    [ InfixR (binary <$> symbol "$")
    ]
  , -- Level 8: Exponentiation (right associative)

    [ InfixR (binary <$> symbol "^")
    , InfixR (binary <$> symbol "^^")
    , InfixR (binary <$> symbol "**")
    ]
  , -- Level 7: Multiplication/Division

    [ InfixL (binary <$> symbol "*")
    , InfixL (binary <$> symbol "/")
    , InfixL (binary <$> symbol "%")
    -- ,InfixL (binary <$ symbol "`div`")
    -- ,InfixL (binary <$ symbol "`mod`")
    -- ,InfixL (binary <$ symbol "`rem`")
    -- ,InfixL (binary <$ symbol "`quot`")
    ]
  , -- Level 6: Addition/Subtraction

    [ InfixL (binary <$> symbol "+")
    , InfixL (binary <$> symbol "-")
    ]
  , -- Level 5: Append
    [InfixR (binary <$> symbol "++")]
  , -- Level 4: Comparison (non-associative)

    [ InfixN (binary <$> symbol "==")
    , InfixN (binary <$> symbol "/=")
    , InfixN (binary <$> symbol "<")
    , InfixN (binary <$> symbol "<=")
    , InfixN (binary <$> symbol ">")
    , InfixN (binary <$> symbol ">=")
    -- ,InfixN (binary <$ symbol "`elem`")
    -- ,InfixN (binary <$ symbol "`notElem`")
    ]
  , -- Level 3: Logical AND (right associative)
    [InfixR (binary <$> symbol "&&")]
  , -- Level 2: Logical OR (right associative)
    [InfixR (binary <$> symbol "||")]
  , -- Level 1: Sequencing

    [ InfixR (binary <$> symbol ">>")
    , InfixR (binary <$> symbol ">>=")
    ]
  , -- Level 0: Reverse application, cons, assignment

    [ InfixL (binary <$> symbol "&")
    -- ,InfixR (binary <$> symbol ":")
    -- ,InfixR (binary <$> symbol "=")
    ]
  , -- Level -1: Custom operator

    [ InfixN $ binary <$> try infixOperator
    ]
  ]

applyApp :: Expr -> Expr -> Expr
applyApp func arg = MkSpan (spanStart func) (spanEnd arg) (EApp func arg)

applyApps :: Expr -> [Expr] -> Expr
applyApps = foldl' applyApp

-- Parser for expressions without infix
parseTerm :: Parser Expr
parseTerm =
  choice
    [ spanned parseLambdaExpr
    , spanned parseLetExpr
    , spanned parseExtendRestrictExpr
    , spanned parseRecordExpr
    , spanned parseTupleExpr
    , spanned parseArrayExpr
    , spanned parseImportExpr
    , spanned parseAppExpr
    ]
  where
    spanned = try . withSpan

parseAtomicExpr :: Parser Expr
parseAtomicExpr =
  withSpan $
    choice
      [ try (EBuiltin <$> (symbol "%%" >> identifier <* symbol "%%"))
      , try (ELit <$> parseLiteral)
      , EVar <$> identifier
      , try parseRecordExpr
      , parens parseExprNode
      ]

parsePostfixExpr :: Parser Expr
parsePostfixExpr = do
  expr <- parseAtomicExpr
  suffixes <- many parsePostfixSuffix
  return $ foldl' (\e f -> f e) expr suffixes
  where
    parsePostfixSuffix :: Parser (Expr -> Expr)
    parsePostfixSuffix =
      choice
        [ try $ do
            _ <- symbol "."
            name <- identifier
            end <- getSourcePos
            return $ \e -> MkSpan (spanStart e) end (EProj e name)
        , do
            arg <- parens parseExpr
            return $ \e -> applyApp e arg
        ]

parseExtendRestrictExpr :: Parser ExprNode
parseExtendRestrictExpr = braces $ do
  ops <- many parseOp
  _ <- symbol "|"
  baseExpr <- parseExpr
  return $ spanNode (foldl' (\e op -> op e) baseExpr ops)
  where
    parseOp =
      choice
        [ do
            opStart <- getSourcePos
            _ <- symbol "-"
            name <- identifier
            _ <- optional $ symbol ","
            return $ \e -> MkSpan opStart (spanEnd e) (ERestrict e name)
        , do
            opStart <- getSourcePos
            name <- identifier
            _ <- symbol "="
            val <- parseExpr
            _ <- optional $ symbol ","
            return $ \e -> MkSpan opStart (spanEnd e) (EExtend e name val)
        ]

parseRecordExpr :: Parser ExprNode
parseRecordExpr = braces $ do
  fields <- parseRecordField `sepBy` symbol ","
  _ <- optional (symbol ",")
  return $ ERecord fields
  where
    parseRecordField = do
      name <- identifier
      _ <- symbol "="
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
  pat <- parsePattern
  typ <- optional (symbol ":" *> parseType)
  _ <- symbol "="
  val <- parseExpr
  _ <- symbol ";"
  body <- parseExpr
  return $ ELet pat typ val body

parseAnnotExpr :: Parser ExprNode
parseAnnotExpr = do
  expr <- makeExprParser parseTerm operatorTable
  _ <- symbol ":"
  typ <- parseType
  return $ EAnnot expr typ

parseAppExpr :: Parser ExprNode
parseAppExpr = do
  func <- parsePostfixExpr
  args <- many parsePostfixExpr
  return $ spanNode (applyApps func args)

parseExprNode :: Parser ExprNode
parseExprNode = fmap spanNode parseExpr

parseExpr :: Parser Expr
parseExpr = try (withSpan parseAnnotExpr) <|> makeExprParser parseTerm operatorTable
