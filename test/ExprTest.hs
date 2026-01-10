{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ExprTest where

import Ceca.AST
import Ceca.Parser
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Megaparsec hiding (failure)

patternExprTestMain :: IO ()
patternExprTestMain = do
  putStrLn "Running Ceca Pattern and Expression Parser Tests..."
  success <-
    checkParallel $
      Group
        "Ceca.PatternExprParser"
        [ ("prop_parseLiterals", prop_parseLiterals)
        , ("prop_parseVariables", prop_parseVariables)
        , ("prop_parseTupleExprs", prop_parseTupleExprs)
        , ("prop_parseArrayExprs", prop_parseArrayExprs)
        , ("prop_parseRecordExprs", prop_parseRecordExprs)
        , ("prop_parseApplication", prop_parseApplication)
        , ("prop_parseInfix", prop_parseInfix)
        , ("prop_parseLambda", prop_parseLambda)
        , ("prop_parseLet", prop_parseLet)
        , ("prop_parseAnnotation", prop_parseAnnotation)
        , ("prop_parsePatterns", prop_parsePatterns)
        , ("prop_parseRecordPatterns", prop_parseRecordPatterns)
        , ("prop_parseExtendRestrict", prop_parseExtendRestrict)
        , ("prop_exprRoundtrip", prop_exprRoundtrip)
        ]
  if success
    then putStrLn "All tests passed!"
    else putStrLn "Some tests failed."

  -- Run specific unit tests
  putStrLn "\nRunning unit tests..."

  runUnitTestsExpr

runUnitTestsExpr :: IO ()
runUnitTestsExpr = do
  test_parseSimpleExpr
  test_parseComplexRecord
  test_parseLambdaExpr
  test_parseApplication
  test_parseApplicationLambdaToLambda
  test_parseInfix
  test_parseProjection
  test_parseLetExpr
  test_parseExtendRestrict
  test_parseRecordPattern
  test_parseInvalidExprFails
  putStrLn "All unit tests passed!"

-- Generators for testing

-- Basic literal generators
genIntLiteral :: Gen Text
genIntLiteral = do
  n <- Gen.int Range.linearBounded
  return $ T.pack (show n)

genFloatLiteral :: Gen Text
genFloatLiteral = do
  n <- Gen.double (Range.linearFrac (-1000.0) 1000.0)
  return $ T.pack (show n)

genStringLiteral :: Gen Text
genStringLiteral = do
  -- Simple string without escapes for now
  chars <- Gen.list (Range.linear 0 10) Gen.alphaNum
  return $ "\"" <> T.pack chars <> "\""

genBoolLiteral :: Gen Text
genBoolLiteral = Gen.element ["true", "false"]

genLiteral :: Gen Text
genLiteral =
  Gen.choice
    [ genIntLiteral
    , genFloatLiteral
    , genStringLiteral
    , genBoolLiteral
    ]

genIdentifier :: Gen Text
genIdentifier = do
  first <- Gen.choice [Gen.element ['a' .. 'z'], Gen.element ['A' .. 'Z'], pure '_']
  rest <-
    Gen.list (Range.linear 0 3) $
      Gen.choice
        [ Gen.element ['a' .. 'z']
        , Gen.element ['A' .. 'Z']
        , Gen.element ['0' .. '9']
        , pure '_'
        , pure '\''
        ]
  return $ T.pack (first : rest)

-- Pattern generators
genVarPattern :: Gen Text
genVarPattern = genIdentifier

genWildcardPattern :: Gen Text
genWildcardPattern = pure "_"

genLiteralPattern :: Gen Text
genLiteralPattern = genLiteral

genTuplePattern :: Gen Text
genTuplePattern = do
  patterns <- Gen.list (Range.linear 2 4) genSimplePattern
  return $ "(" <> T.intercalate ", " patterns <> ")"

genArrayPattern :: Gen Text
genArrayPattern = do
  patterns <- Gen.list (Range.linear 0 4) genSimplePattern
  return $ "[" <> T.intercalate ", " patterns <> "]"

genRecordPattern :: Gen Text
genRecordPattern = do
  fields <- Gen.list (Range.linear 0 3) genIdentifier
  hasRest <- Gen.bool

  if null fields && not hasRest
    then pure "{}"
    else do
      let fieldStrs = map (\f -> f <> " : " <> f) fields -- Bind to same name
      if hasRest
        then do
          restVar <- genIdentifier
          let fieldsPart = if null fieldStrs then "" else T.intercalate ", " fieldStrs <> ", "
          return $ "{" <> fieldsPart <> "... " <> restVar <> "}"
        else
          return $ "{" <> T.intercalate ", " fieldStrs <> "}"

genSimplePattern :: Gen Text
genSimplePattern =
  Gen.choice
    [ genVarPattern
    , genWildcardPattern
    , genLiteralPattern
    ]

genPattern :: Gen Text
genPattern = Gen.sized $ \size ->
  if size <= 1
    then genSimplePattern
    else
      Gen.recursive
        Gen.choice
        [ genSimplePattern
        , genRecordPattern
        ]
        [ genTuplePattern
        , genArrayPattern
        ]

-- Expression generators
genSimpleExpr :: Gen Text
genSimpleExpr =
  Gen.choice
    [ genLiteral
    , genIdentifier
    ]

genRecordExpr :: Gen Text
genRecordExpr = do
  fields <- Gen.list (Range.linear 0 3) $ do
    name <- genIdentifier
    expr <- genSimpleExpr
    return (name, expr)

  if null fields
    then pure "{}"
    else do
      let fieldStrs = map (\(n, e) -> n <> " = " <> e) fields
      return $ "{" <> T.intercalate ", " fieldStrs <> "}"

genTupleExpr :: Gen Text
genTupleExpr = do
  exprs <- Gen.list (Range.linear 2 4) genSimpleExpr
  return $ "(" <> T.intercalate ", " exprs <> ")"

genArrayExpr :: Gen Text
genArrayExpr = do
  exprs <- Gen.list (Range.linear 0 4) genSimpleExpr
  return $ "[" <> T.intercalate ", " exprs <> "]"

genApplication :: Gen Text
genApplication = do
  func <- genIdentifier
  args <- Gen.list (Range.linear 1 3) genSimpleExpr
  return $ T.unwords (func : args)

genInfix :: Gen Text
genInfix = do
  e1 <- genSimpleExpr
  op <- Gen.element ["&", "$"]
  e2 <- genSimpleExpr
  return $ e1 <> " " <> op <> " " <> e2

genLambda :: Gen Text
genLambda = do
  pat <- genPattern
  body <- genSimpleExpr
  return $ pat <> " => " <> body

genLetExpr :: Gen Text
genLetExpr = do
  name <- genIdentifier
  expr1 <- genSimpleExpr
  expr2 <- genSimpleExpr
  return $ "let " <> name <> " = " <> expr1 <> "; " <> expr2

genAnnotation :: Gen Text
genAnnotation = do
  expr <- genSimpleExpr
  typ <- Gen.choice $ pure <$> ["int", "string", "bool", "float"]
  return $ expr <> " : " <> typ

genExtendRestrict :: Gen Text
genExtendRestrict = do
  ops <-
    Gen.list (Range.linear 1 3) $
      Gen.choice
        [ do
            name <- genIdentifier
            return $ "-" <> name
        , do
            name <- genIdentifier
            expr <- genSimpleExpr
            return $ name <> " = " <> expr
        ]
  base <- genIdentifier
  let opsStr = T.intercalate ", " ops
  return $ "{" <> opsStr <> " | " <> base <> "}"

genExpr :: Gen Text
genExpr = Gen.sized $ \size ->
  if size <= 1
    then genSimpleExpr
    else
      Gen.recursive
        Gen.choice
        [ genSimpleExpr
        , genRecordExpr
        , genTupleExpr
        , genArrayExpr
        , genApplication
        , genLambda
        , genLetExpr
        , genAnnotation
        , genExtendRestrict
        ]
        []

-- No deeper recursion for now

-- Property tests
prop_parseLiterals :: Property
prop_parseLiterals = property $ do
  lit <- forAll genLiteral
  let result = parse parseProgram "test" lit
  annotateShow result
  case result of
    Right (MkSpan _ _ (ELit _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseVariables :: Property
prop_parseVariables = property $ do
  var <- forAll genIdentifier
  let result = parse parseProgram "test" var
  annotateShow result
  case result of
    Right (MkSpan _ _ (EVar name)) -> do
      assert $ name == var
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseTupleExprs :: Property
prop_parseTupleExprs = property $ do
  expr <- forAll genTupleExpr
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (ETuple _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseArrayExprs :: Property
prop_parseArrayExprs = property $ do
  expr <- forAll genArrayExpr
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (EArray _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseRecordExprs :: Property
prop_parseRecordExprs = property $ do
  expr <- forAll genRecordExpr
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (ERecord _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseApplication :: Property
prop_parseApplication = property $ do
  expr <- forAll genApplication
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (EApp _ _)) -> success
    Right (MkSpan _ _ (EVar _)) -> success -- Single identifier is valid
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseInfix :: Property
prop_parseInfix = property $ do
  expr <- forAll genInfix
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right _ -> success
    Left err -> do
      annotateShow err
      failure

prop_parseLambda :: Property
prop_parseLambda = property $ do
  expr <- forAll genLambda
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (EAbs _ _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseLet :: Property
prop_parseLet = property $ do
  expr <- forAll genLetExpr
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (ELet _ _ _ _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseAnnotation :: Property
prop_parseAnnotation = property $ do
  expr <- forAll genAnnotation
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (EAnnot _ _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parsePatterns :: Property
prop_parsePatterns = property $ do
  pat <- forAll genPattern
  let result = parse parsePatternOnly "test" pat
  annotateShow result
  case result of
    Right _ -> success
    Left err -> do
      annotateShow err
      failure

prop_parseRecordPatterns :: Property
prop_parseRecordPatterns = property $ do
  pat <- forAll genRecordPattern
  let result = parse parsePatternOnly "test" pat
  annotateShow result
  case result of
    Right (MkSpan _ _ (PRecord _ _)) -> success
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

prop_parseExtendRestrict :: Property
prop_parseExtendRestrict = property $ do
  expr <- forAll genExtendRestrict
  let result = parse parseProgram "test" expr
  annotateShow result
  case result of
    Right (MkSpan _ _ (EExtend _ _ _)) -> success
    Right (MkSpan _ _ (ERestrict _ _)) -> success
    Right (MkSpan _ _ (EVar _)) -> success -- Base could be just a variable
    Right other -> do
      annotateShow other
      failure
    Left err -> do
      annotateShow err
      failure

-- Roundtrip property for expressions
prop_exprRoundtrip :: Property
prop_exprRoundtrip = property $ do
  expr <- forAll $ Gen.small genExpr
  let parsed = parse parseProgram "test" expr
  case parsed of
    Right e -> do
      -- Re-parse the pretty-printed version
      let printed = prettyPrintExpr e
      let reparsed = parse parseProgram "test" printed
      case reparsed of
        Right e2 -> do
          -- Check if expressions are equivalent
          annotateShow $ spanNode e
          annotateShow $ spanNode e2
          assert $ exprNodeEq (spanNode e) (spanNode e2)
        Left err2 -> do
          annotate $ "Failed to re-parse: " <> T.unpack printed
          annotateShow err2
          failure
    Left err -> do
      annotateShow err
      failure
 where
  exprNodeEq :: ExprNode -> ExprNode -> Bool
  exprNodeEq (EVar a) (EVar b) = a == b
  exprNodeEq (ELit a) (ELit b) = litEq a b
  exprNodeEq (EAbs p1 e1) (EAbs p2 e2) =
    patternEq (spanNode p1) (spanNode p2) && exprNodeEq (spanNode e1) (spanNode e2)
  exprNodeEq (EApp e1 e2) (EApp f1 f2) =
    exprNodeEq (spanNode e1) (spanNode f1) && exprNodeEq (spanNode e2) (spanNode f2)
  exprNodeEq (ERecord fs1) (ERecord fs2) =
    length fs1 == length fs2 && all (uncurry fieldEq) (zip fs1 fs2)
   where
    fieldEq (n1, e1) (n2, e2) = n1 == n2 && exprNodeEq (spanNode e1) (spanNode e2)
  exprNodeEq (ETuple es1) (ETuple es2) =
    length es1 == length es2 && all (uncurry exprNodeEq) (zip (map spanNode es1) (map spanNode es2))
  exprNodeEq (EArray es1) (EArray es2) =
    length es1 == length es2 && all (uncurry exprNodeEq) (zip (map spanNode es1) (map spanNode es2))
  exprNodeEq (EProj e1 l1) (EProj e2 l2) =
    l1 == l2 && exprNodeEq (spanNode e1) (spanNode e2)
  exprNodeEq (EExtend e1 l1 v1) (EExtend e2 l2 v2) =
    l1 == l2 && exprNodeEq (spanNode e1) (spanNode e2) && exprNodeEq (spanNode v1) (spanNode v2)
  exprNodeEq (ERestrict e1 l1) (ERestrict e2 l2) =
    l1 == l2 && exprNodeEq (spanNode e1) (spanNode e2)
  exprNodeEq (ELet n1 t1 e1 b1) (ELet n2 t2 e2 b2) =
    n1 == n2 && typeOptEq t1 t2 && exprNodeEq (spanNode e1) (spanNode e2) && exprNodeEq (spanNode b1) (spanNode b2)
   where
    typeOptEq Nothing Nothing = True
    typeOptEq (Just (MkSpan _ _ t1)) (Just (MkSpan _ _ t2)) = typeNodeEq t1 t2
    typeOptEq _ _ = False
  exprNodeEq (EAnnot e1 t1) (EAnnot e2 t2) =
    exprNodeEq (spanNode e1) (spanNode e2) && typeNodeEq (spanNode t1) (spanNode t2)
  exprNodeEq (EImport p1) (EImport p2) = p1 == p2
  exprNodeEq _ _ = False

  litEq :: Literal -> Literal -> Bool
  litEq (LInt a) (LInt b) = a == b
  litEq (LFloat a) (LFloat b) = a == b
  litEq (LString a) (LString b) = a == b
  litEq (LBool a) (LBool b) = a == b
  litEq _ _ = False

  patternEq :: PatternNode -> PatternNode -> Bool
  patternEq (PVar a) (PVar b) = a == b
  patternEq PWildcard PWildcard = True
  patternEq (PLit a) (PLit b) = litEq a b
  patternEq (PRecord fs1 r1) (PRecord fs2 r2) =
    length fs1 == length fs2
      && all (uncurry fieldPatEq) (zip fs1 fs2)
      && restPatEq r1 r2
   where
    fieldPatEq (n1, p1) (n2, p2) = n1 == n2 && patternEq (spanNode p1) (spanNode p2)
    restPatEq Nothing Nothing = True
    restPatEq (Just (MkSpan _ _ p1)) (Just (MkSpan _ _ p2)) = patternEq p1 p2
    restPatEq _ _ = False
  patternEq (PTuple ps1) (PTuple ps2) =
    length ps1 == length ps2 && all (uncurry patternEq) (zip (map spanNode ps1) (map spanNode ps2))
  patternEq (PArray ps1) (PArray ps2) =
    length ps1 == length ps2 && all (uncurry patternEq) (zip (map spanNode ps1) (map spanNode ps2))
  patternEq _ _ = False

  typeNodeEq :: TypeNode -> TypeNode -> Bool
  typeNodeEq (TVar a) (TVar b) = a == b
  typeNodeEq (TCon a) (TCon b) = a == b
  typeNodeEq (TArrow a1 a2) (TArrow b1 b2) =
    typeNodeEq (spanNode a1) (spanNode b1) && typeNodeEq (spanNode a2) (spanNode b2)
  typeNodeEq (TTuple as) (TTuple bs) =
    length as == length bs && all (uncurry typeNodeEq) (zip (map spanNode as) (map spanNode bs))
  typeNodeEq (TArray a) (TArray b) = typeNodeEq (spanNode a) (spanNode b)
  typeNodeEq TRecordEmpty TRecordEmpty = True
  typeNodeEq (TRecordExtend l1 t1 r1) (TRecordExtend l2 t2 r2) =
    l1 == l2 && typeNodeEq (spanNode t1) (spanNode t2) && typeNodeEq (spanNode r1) (spanNode r2)
  typeNodeEq _ _ = False

-- Helper to pretty print expressions
prettyPrintExpr :: Expr -> Text
prettyPrintExpr (MkSpan _ _ node) = prettyPrintExprNode node

prettyPrintExprNode :: ExprNode -> Text
prettyPrintExprNode (EVar name) = name
prettyPrintExprNode (ELit lit) = prettyPrintLiteral lit
prettyPrintExprNode (EAbs pat body) = prettyPrintPattern pat <> " => " <> prettyPrintExpr body
prettyPrintExprNode (EApp e1 e2) = prettyPrintExpr e1 <> " " <> prettyPrintExpr e2
prettyPrintExprNode (ERecord fields) =
  "{" <> T.intercalate ", " (map (\(n, e) -> n <> " = " <> prettyPrintExpr e) fields) <> "}"
prettyPrintExprNode (ETuple exprs) =
  "(" <> T.intercalate ", " (map prettyPrintExpr exprs) <> ")"
prettyPrintExprNode (EArray exprs) =
  "[" <> T.intercalate ", " (map prettyPrintExpr exprs) <> "]"
prettyPrintExprNode (EProj e field) = prettyPrintExpr e <> "." <> field
prettyPrintExprNode (EExtend e field value) =
  "{" <> field <> " = " <> prettyPrintExpr value <> " | " <> prettyPrintExpr e <> "}"
prettyPrintExprNode (ERestrict e field) =
  "{-" <> field <> " | " <> prettyPrintExpr e <> "}"
prettyPrintExprNode (ELet name mtype expr body) =
  let typePart = case mtype of
        Just t -> " : " <> prettyPrintType t
        Nothing -> ""
   in "let " <> name <> typePart <> " = " <> prettyPrintExpr expr <> "; " <> prettyPrintExpr body
prettyPrintExprNode (EAnnot e t) = prettyPrintExpr e <> " : " <> prettyPrintType t
prettyPrintExprNode (EImport path) = "@" <> T.pack path

prettyPrintPattern :: Pattern -> Text
prettyPrintPattern (MkSpan _ _ node) = prettyPrintPatternNode node

prettyPrintPatternNode :: PatternNode -> Text
prettyPrintPatternNode (PVar name) = name
prettyPrintPatternNode PWildcard = "_"
prettyPrintPatternNode (PLit lit) = prettyPrintLiteral lit
prettyPrintPatternNode (PRecord fields mrest) =
  let fieldStrs = map (\(n, p) -> n <> " : " <> prettyPrintPattern p) fields
      restStr = case mrest of
        Just rest -> "... " <> prettyPrintPattern rest
        Nothing -> ""
      allStrs = fieldStrs ++ [restStr | not (T.null restStr)]
   in "{" <> T.intercalate ", " allStrs <> "}"
prettyPrintPatternNode (PTuple pats) =
  "(" <> T.intercalate ", " (map prettyPrintPattern pats) <> ")"
prettyPrintPatternNode (PArray pats) =
  "[" <> T.intercalate ", " (map prettyPrintPattern pats) <> "]"

prettyPrintLiteral :: Literal -> Text
prettyPrintLiteral (LInt n) = T.pack (show n)
prettyPrintLiteral (LFloat n) = T.pack (show n)
prettyPrintLiteral (LString s) = "\"" <> s <> "\""
prettyPrintLiteral (LBool b) = if b then "true" else "false"

prettyPrintType :: Type -> Text
prettyPrintType (MkSpan _ _ node) = prettyPrintTypeNode node

prettyPrintTypeNode :: TypeNode -> Text
prettyPrintTypeNode (TVar name) = name
prettyPrintTypeNode (TCon name) = name
prettyPrintTypeNode (TArrow t1 t2) =
  prettyPrintTypeNode (spanNode t1) <> " -> " <> prettyPrintType t2
prettyPrintTypeNode (TTuple ts) =
  "(" <> T.intercalate ", " (map prettyPrintType ts) <> ")"
prettyPrintTypeNode (TArray t) = "[" <> prettyPrintType t <> "]"
prettyPrintTypeNode TRecordEmpty = "{}"
prettyPrintTypeNode (TRecordExtend label typ rest) =
  let (fields, mrest) = collectRecordFields [(label, typ)] rest
      fieldsPart = T.intercalate ", " (map (\(l, t) -> l <> ": " <> prettyPrintType t) fields)
   in case mrest of
        Nothing -> "{" <> fieldsPart <> "}"
        Just rest' -> "{" <> fieldsPart <> " | " <> prettyPrintType rest' <> "}"
 where
  collectRecordFields :: [(Text, Type)] -> Type -> ([(Text, Type)], Maybe Type)
  collectRecordFields acc (MkSpan _ _ TRecordEmpty) = (reverse acc, Nothing)
  collectRecordFields acc (MkSpan _ _ (TVar name)) =
    (reverse acc, Just (MkSpan (initialPos "") (initialPos "") (TVar name)))
  collectRecordFields acc (MkSpan _ _ (TRecordExtend l t r)) =
    collectRecordFields ((l, t) : acc) r
  collectRecordFields acc _ = (reverse acc, Nothing)

-- Unit tests
test_parseSimpleExpr :: IO ()
test_parseSimpleExpr = do
  let tests =
        [ ("42", ELit (LInt 42))
        , ("\"hello\"", ELit (LString "hello"))
        , ("true", ELit (LBool True))
        , ("x", EVar "x")
        ]

  mapM_
    ( \(input, expectedNode) -> do
        let result = parse parseProgram "test" input
        case result of
          Right (MkSpan _ _ node) ->
            if node == expectedNode
              then return ()
              else error $ "test_parseSimpleExpr failed for " ++ show input ++ ": got " ++ show node
          Left err -> error $ "test_parseSimpleExpr failed for " ++ show input ++ ": " ++ errorBundlePretty err
    )
    tests

test_parseComplexRecord :: IO ()
test_parseComplexRecord = do
  let result = parse parseProgram "test" "{ x = 1, y = true }"
  case result of
    Right (MkSpan _ _ (ERecord fields)) -> do
      if length fields /= 2
        then error $ "test_parseComplexRecord failed: Should have 2 fields, got " ++ show (length fields)
        else do
          let [(n1, e1), (n2, e2)] = fields
          if not (n1 == "x" && n2 == "y")
            then error $ "test_parseComplexRecord failed: Field names should be x and y, got " ++ show n1 ++ " and " ++ show n2
            else do
              case spanNode e1 of
                ELit (LInt 1) -> return ()
                _ -> error "test_parseComplexRecord failed: First field should be 1"
              case spanNode e2 of
                ELit (LBool True) -> return ()
                _ -> error "test_parseComplexRecord failed: Second field should be true"
    Right other -> error $ "test_parseComplexRecord failed: Expected ERecord, got: " ++ show other
    Left err -> error $ "test_parseComplexRecord failed: " ++ errorBundlePretty err

test_parseLambdaExpr :: IO ()
test_parseLambdaExpr = do
  let result = parse parseProgram "test" "x => x"
  case result of
    Right (MkSpan _ _ (EAbs pat body)) -> do
      case spanNode pat of
        PVar "x" -> return ()
        _ -> error "test_parseLambdaExpr failed: Pattern should be x"
      case spanNode body of
        EVar "x" -> return ()
        _ -> error "test_parseLambdaExpr failed: Body should be x"
    Right other -> error $ "test_parseLambdaExpr failed: Expected EAbs, got: " ++ show other
    Left err -> error $ "test_parseLambdaExpr failed: " ++ errorBundlePretty err

test_parseApplication :: IO ()
test_parseApplication = do
  let result = parse parseProgram "test" "(x => x) 42"
  print result
  case result of
    Right (MkSpan _ _ (EApp func arg)) -> do
      case spanNode func of
        EAbs pat body -> do
          case spanNode pat of
            PVar "x" -> return ()
            _ -> error "test_parseApplication failed: Lambda pattern should be x"
          case spanNode body of
            EVar "x" -> return ()
            _ -> error "test_parseApplication failed: Lambda body should be x"
        _ -> error "test_parseApplication failed: Function should be lambda"
      case spanNode arg of
        ELit (LInt 42) -> return ()
        _ -> error "test_parseApplication failed: Argument should be 42"
    Right other -> error $ "test_parseApplication failed: Expected EApp, got: " ++ show other
    Left err -> error $ "test_parseApplication failed: " ++ errorBundlePretty err

test_parseApplicationLambdaToLambda :: IO ()
test_parseApplicationLambdaToLambda = do
  let result = parse parseProgram "test" "(x => x) (x => x)"
  case result of
    Right (MkSpan _ _ (EApp func arg)) -> do
      let checkLambda node errPrefix = case spanNode node of
            EAbs pat body -> do
              case spanNode pat of
                PVar "x" -> return ()
                _ -> error $ errPrefix ++ ": Lambda pattern should be x"
              case spanNode body of
                EVar "x" -> return ()
                _ -> error $ errPrefix ++ ": Lambda body should be x"
            _ -> error $ errPrefix ++ ": Should be lambda"
      checkLambda func "test_parseApplicationLambdaToLambda failed: Function"
      checkLambda arg "test_parseApplicationLambdaToLambda failed: Argument"
    Right other -> error $ "test_parseApplicationLambdaToLambda failed: Expected EApp, got: " ++ show other
    Left err -> error $ "test_parseApplicationLambdaToLambda failed: " ++ errorBundlePretty err

test_parseInfix :: IO ()
test_parseInfix = do
  -- Test & (left associative, reverse application)
  let result1 = parse parseProgram "test" "x & y"
  case result1 of
    Right (MkSpan _ _ (EApp (MkSpan _ _ (EVar "y")) (MkSpan _ _ (EVar "x")))) -> return ()
    Right other -> error $ "test_parseInfix failed for &: Expected EApp y x, got: " ++ show other
    Left err -> error $ "test_parseInfix failed for &: " ++ errorBundlePretty err

  -- Test $ (right associative)
  let result2 = parse parseProgram "test" "x $ y"
  case result2 of
    Right (MkSpan _ _ (EApp (MkSpan _ _ (EVar "x")) (MkSpan _ _ (EVar "y")))) -> return ()
    Right other -> error $ "test_parseInfix failed for $: Expected EApp x y, got: " ++ show other
    Left err -> error $ "test_parseInfix failed for $: " ++ errorBundlePretty err

test_parseProjection :: IO ()
test_parseProjection = do
  -- Test simple projection
  let result = parse parseProgram "test" "x.y"
  case result of
    Right (MkSpan _ _ (EProj (MkSpan _ _ (EVar "x")) "y")) -> return ()
    Right other -> error $ "test_parseProjection failed: Expected EProj x y, got: " ++ show other
    Left err -> error $ "test_parseProjection failed: " ++ errorBundlePretty err

  -- Test projection with parentheses
  let result2 = parse parseProgram "test" "(x).y"
  case result2 of
    Right (MkSpan _ _ (EProj (MkSpan _ _ (EVar "x")) "y")) -> return ()
    Right other -> error $ "test_parseProjection failed for parentheses: Expected EProj x y, got: " ++ show other
    Left err -> error $ "test_parseProjection failed for parentheses: " ++ errorBundlePretty err

test_parseLetExpr :: IO ()
test_parseLetExpr = do
  let result = parse parseProgram "test" "let x = 42; x"
  case result of
    Right (MkSpan _ _ (ELet name Nothing val body)) -> do
      if name /= "x"
        then error $ "test_parseLetExpr failed: Name should be x, got " ++ show name
        else do
          case spanNode val of
            ELit (LInt 42) -> return ()
            _ -> error "test_parseLetExpr failed: Value should be 42"
          case spanNode body of
            EVar "x" -> return ()
            _ -> error "test_parseLetExpr failed: Body should be x"
    Right other -> error $ "test_parseLetExpr failed: Expected ELet, got: " ++ show other
    Left err -> error $ "test_parseLetExpr failed: " ++ errorBundlePretty err

test_parseExtendRestrict :: IO ()
test_parseExtendRestrict = do
  let tests =
        [
          ( "{-x | r}"
          , \case
              ERestrict e "x" -> case spanNode e of
                EVar "r" -> True
                _ -> False
              _ -> False
          )
        ,
          ( "{x = 1 | r}"
          , \case
              EExtend e "x" val -> case (spanNode e, spanNode val) of
                (EVar "r", ELit (LInt 1)) -> True
                _ -> False
              _ -> False
          )
        ,
          ( "{-x, y = 2 | r}"
          , \node -> case node of
              EExtend e2 "y" val2 -> case (spanNode e2, spanNode val2) of
                (ERestrict e1 "x", ELit (LInt 2)) -> case spanNode e1 of
                  EVar "r" -> True
                  _ -> False
                _ -> False
              _ -> False
          )
        ]

  mapM_
    ( \(input, checker) -> do
        let result = parse parseProgram "test" input
        case result of
          Right (MkSpan _ _ node) ->
            if checker node
              then return ()
              else error $ "test_parseExtendRestrict failed for " ++ show input ++ ": got " ++ show node
          Left err -> error $ "test_parseExtendRestrict failed for " ++ show input ++ ": " ++ errorBundlePretty err
    )
    tests

test_parseRecordPattern :: IO ()
test_parseRecordPattern = do
  let result = parse parsePatternOnly "test" "{x, y : z, ...rest}"
  case result of
    Right (MkSpan _ _ (PRecord fields mrest)) -> do
      if length fields /= 2
        then error $ "test_parseRecordPattern failed: Should have 2 fields, got " ++ show (length fields)
        else do
          let [(n1, p1), (n2, p2)] = fields
          if not (n1 == "x" && n2 == "y")
            then error $ "test_parseRecordPattern failed: Field names should be x and y, got " ++ show n1 ++ " and " ++ show n2
            else do
              case spanNode p1 of
                PVar "x" -> return ()
                _ -> error "test_parseRecordPattern failed: First field should bind to x"
              case spanNode p2 of
                PVar "z" -> return ()
                _ -> error "test_parseRecordPattern failed: Second field should bind to z"
              case mrest of
                Just (MkSpan _ _ (PVar "rest")) -> return ()
                _ -> error "test_parseRecordPattern failed: Should have rest pattern binding to rest"
    Right other -> error $ "test_parseRecordPattern failed: Expected PRecord, got: " ++ show other
    Left err -> error $ "test_parseRecordPattern failed: " ++ errorBundlePretty err

test_parseInvalidExprFails :: IO ()
test_parseInvalidExprFails = do
  let invalidExprs =
        [ ("unterminated record", "{")
        , ("missing equals", "{ x 1 }")
        , ("missing lambda body", "x =>")
        , ("missing pattern", "=> x")
        , ("unclosed tuple", "(1, 2")
        , ("unclosed array", "[1")
        , ("invalid extend", "{x | r}") -- Missing = for extend
        , ("invalid restrict", "{- | r}") -- Missing field name
        ]

  mapM_
    ( \(desc, expr) -> do
        let result = parse parseProgram "test" expr
        case result of
          Right _ -> error $ "test_parseInvalidExprFails failed (" ++ desc ++ "): Should have failed to parse: " ++ T.unpack expr
          Left _ -> return ()
    )
    invalidExprs
