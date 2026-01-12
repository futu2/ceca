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
      [ try (EBuiltin <$> (symbol "%%" >> identifier <* symbol "%%"))
      , try $ ELit <$> parseLiteral
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
  symbol "let"
  pat <- parsePattern
  typ <- optional (symbol ":" >> parseType)
  symbol "="
  val <- parseExpr
  symbol ";"
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