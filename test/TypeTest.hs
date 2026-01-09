{-# LANGUAGE OverloadedStrings #-}

module TypeTest where

import Ceca.AST
import Ceca.Parser
import Ceca.Parser.Basic
import Ceca.Parser.Type
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Megaparsec hiding (failure)

typeTestMain :: IO ()
typeTestMain = do
    putStrLn "Running Ceca Parser Tests..."
    success <-
        checkParallel $
            Group
                "Ceca.TypeParser"
                [ ("prop_parseTypeVars", prop_parseTypeVars)
                , ("prop_parseTupleTypes", prop_parseTupleTypes)
                , ("prop_parseBasicTypes", prop_parseBasicTypes)
                , ("prop_parseArrayTypes", prop_parseArrayTypes)
                , ("prop_parseArrowTypes", prop_parseArrowTypes)
                , ("prop_parseRecordTypes", prop_parseRecordTypes)
                , ("prop_parseNestedTypes", prop_parseNestedTypes)
                , ("prop_typeRoundtrip", prop_typeRoundtrip)
                ]
    if success
        then putStrLn "All tests passed!"
        else putStrLn "Some tests failed."

    -- Run specific unit tests
    putStrLn "\nRunning unit tests..."
    runUnitTests

runUnitTests :: IO ()
runUnitTests = do
    test_parseEmptyRecord
    test_parseRecordExtension
    test_parseComplexRecord
    test_parseFunctionType
    test_parseNestedRecord
    test_parseInvalidFails
    putStrLn "All unit tests passed!"

-- Generators for testing (same as before)
genTypeVarName :: Gen Text
genTypeVarName = do
    first <- Gen.element ['a' .. 'z']
    rest <- Gen.list (Range.linear 0 3) $ Gen.element $ ['a' .. 'z'] ++ ['0' .. '9'] ++ "_"
    return $ T.pack ('?' : first : rest)

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

genBasicType :: Gen Text
genBasicType = Gen.element ["string", "int", "float", "bool"]

genLiteralType :: Gen Text
genLiteralType =
    Gen.choice
        [ genBasicType
        , T.append "?" <$> genIdentifier
        ]

genSimpleType :: Gen Text
genSimpleType =
    Gen.choice
        [ genLiteralType
        , do
            inner <- genLiteralType
            return $ "[" <> inner <> "]"
        , do
            types <- Gen.list (Range.linear 2 4) genLiteralType
            return $ "(" <> T.intercalate ", " types <> ")"
        ]

genField :: Gen (Text, Text)
genField = do
    name <- genIdentifier
    typ <- genSimpleType
    return (name, typ)

genRecordType :: Gen Text
genRecordType = do
    fields <- Gen.list (Range.linear 0 3) genField
    hasRest <- Gen.bool

    if null fields && not hasRest
        then pure "{}"
        else do
            let fieldStrs = map (\(n, t) -> n <> ": " <> t) fields
            if hasRest
                then do
                    restVar <- genTypeVarName
                    let fieldsPart = if null fieldStrs then "" else T.intercalate ", " fieldStrs <> ", "
                    return $ "{ " <> fieldsPart <> "| " <> restVar <> " }"
                else do
                    return $ "{ " <> T.intercalate ", " fieldStrs <> " }"

genArrowType :: Gen Text
genArrowType = do
    left <- genNonArrowType
    right <- genSimpleType
    return $ left <> " -> " <> right

genNonArrowType :: Gen Text
genNonArrowType =
    Gen.choice
        [ genLiteralType
        , do
            inner <- genSimpleType
            return $ "(" <> inner <> ")"
        , genRecordType
        ]

genTypeString :: Gen Text
genTypeString = Gen.sized $ \size ->
    if size <= 1
        then genSimpleType
        else
            Gen.recursive
                Gen.choice
                [ genSimpleType
                , genRecordType
                ]
                [ do
                    left <- genNonArrowType
                    right <- genTypeString
                    return $ left <> " -> " <> right
                , do
                    inner <- genTypeString
                    return $ "[" <> inner <> "]"
                , do
                    types <- Gen.list (Range.linear 2 4) genTypeString
                    return $ "(" <> T.intercalate ", " types <> ")"
                ]

-- Property tests
prop_parseBasicTypes :: Property
prop_parseBasicTypes = property $ do
    typ <- forAll genBasicType
    let result = parse parseTypeOnly "test" typ
    annotateShow result
    case result of
        Right (MkSpan _ _ (TCon name)) -> do
            assert $ name == typ
        Right other -> do
            annotateShow other
            failure
        Left err -> do
            annotateShow err
            failure

prop_parseTypeVars :: Property
prop_parseTypeVars = property $ do
    varName <- forAll $ do
        name <- genIdentifier
        return $ "?" <> name
    let result = parse parseTypeOnly "test" varName
    annotateShow result
    case result of
        Right (MkSpan _ _ (TVar name)) -> do
            assert $ name == varName
        Right other -> do
            annotateShow other
            failure
        Left err -> do
            annotateShow err
            failure

prop_parseTupleTypes :: Property
prop_parseTupleTypes = property $ do
    types <- forAll $ do
        ts <- Gen.list (Range.linear 2 10) genLiteralType
        return $ "(" <> T.intercalate ", " ts <> ")"

    let result = parse parseTypeOnly "test" types
    annotateShow result

    case result of
        Right (MkSpan _ _ (TTuple tvs)) -> do
            let expectedCount = length $ T.splitOn ", " types
            assert $ length tvs == expectedCount
        Right other -> do
            annotateShow other
            failure
        Left err -> do
            annotateShow err
            failure

prop_parseArrayTypes :: Property
prop_parseArrayTypes = property $ do
    inner <- forAll genLiteralType
    let arrayType = "[" <> inner <> "]"
    let result = parse parseTypeOnly "test" arrayType
    annotateShow result
    case result of
        Right (MkSpan _ _ (TArray innerType)) -> do
            case spanNode innerType of
                TCon name -> assert $ name == inner
                TVar name -> assert $ name == inner
                _ -> failure
        Right other -> do
            annotateShow other
            failure
        Left err -> do
            annotateShow err
            failure

prop_parseArrowTypes :: Property
prop_parseArrowTypes = property $ do
    typ <- forAll genArrowType
    let result = parse parseTypeOnly "test" typ
    annotateShow result
    case result of
        Right (MkSpan _ _ (TArrow _ _)) -> success
        Right other -> do
            annotateShow other
            failure
        Left err -> do
            annotateShow err
            failure

prop_parseRecordTypes :: Property
prop_parseRecordTypes = property $ do
    typ <- forAll genRecordType
    let result = parse parseTypeOnly "test" typ
    annotateShow result
    case result of
        Right (MkSpan _ _ TRecordEmpty) -> do
            assert $ typ == "{}"
        Right (MkSpan _ _ (TRecordExtend _ _ _)) -> success
        Right (MkSpan _ _ (TVar _)) -> success -- Accept TVar for { | ?a }
        Right other -> do
            annotateShow other
            failure
        Left err -> do
            annotateShow err
            failure

prop_parseNestedTypes :: Property
prop_parseNestedTypes = property $ do
    typ <- forAll $ Gen.small genTypeString
    let result = parse parseTypeOnly "test" typ
    annotateShow result
    case result of
        Right _ -> success
        Left err -> do
            annotateShow err
            failure

-- Roundtrip property
prop_typeRoundtrip :: Property
prop_typeRoundtrip = property $ do
    typ <- forAll $ Gen.small genTypeString
    let parsed = parse parseTypeOnly "test" typ
    case parsed of
        Right t -> do
            -- Re-parse the pretty-printed version
            let printed = prettyPrintType t
            let reparsed = parse parseTypeOnly "test" printed
            case reparsed of
                Right t2 -> do
                    -- Check if types are equivalent (simplified check)
                    annotateShow $ spanNode t
                    annotateShow $ spanNode t2
                    assert $ typeNodeEq (spanNode t) (spanNode t2)
                Left err2 -> do
                    annotate $ "Failed to re-parse: " <> T.unpack printed
                    annotateShow err2
                    failure
        Left err -> do
            annotateShow err
            failure
  where
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

-- Helper to pretty print types for roundtrip test
prettyPrintType :: Type -> Text
prettyPrintType (MkSpan _ _ spanNode) = prettyPrintTypeNode spanNode

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

-- Unit tests (now as IO actions)
test_parseEmptyRecord :: IO ()
test_parseEmptyRecord = do
    let result = parse parseTypeOnly "test" "{}"
    case result of
        Right (MkSpan _ _ TRecordEmpty) -> return ()
        Right other -> error $ "test_parseEmptyRecord failed: Expected TRecordEmpty, got: " ++ show other
        Left err -> error $ "test_parseEmptyRecord failed: " ++ errorBundlePretty err

test_parseRecordExtension :: IO ()
test_parseRecordExtension = do
    let result = parse parseTypeOnly "test" "{ x: int | ?r }"
    case result of
        Right (MkSpan _ _ (TRecordExtend "x" t rest)) -> do
            case spanNode t of
                TCon "int" -> return ()
                _ -> error $ "test_parseRecordExtension failed: Expected int type, got: " ++ show (spanNode t)
            case spanNode rest of
                TVar "?r" -> return ()
                _ -> error $ "test_parseRecordExtension failed: Expected ?r type variable, got: " ++ show (spanNode rest)
        Right other -> error $ "test_parseRecordExtension failed: Expected TRecordExtend, got: " ++ show other
        Left err -> error $ "test_parseRecordExtension failed: " ++ errorBundlePretty err

test_parseComplexRecord :: IO ()
test_parseComplexRecord = do
    let result = parse parseTypeOnly "test" "{ x: int, y: string | ?rest }"
    case result of
        Right (MkSpan _ _ (TRecordExtend "x" t1 rest1)) -> do
            case spanNode t1 of
                TCon "int" -> return ()
                _ -> error "test_parseComplexRecord failed: First field should be int"
            case spanNode rest1 of
                TRecordExtend "y" t2 rest2 -> do
                    case spanNode t2 of
                        TCon "string" -> return ()
                        _ -> error "test_parseComplexRecord failed: Second field should be string"
                    case spanNode rest2 of
                        TVar "?rest" -> return ()
                        _ -> error "test_parseComplexRecord failed: Should end with ?rest"
                _ -> error "test_parseComplexRecord failed: Expected nested TRecordExtend"
        Right other -> error $ "test_parseComplexRecord failed: Expected TRecordExtend, got: " ++ show other
        Left err -> error $ "test_parseComplexRecord failed: " ++ errorBundlePretty err

test_parseFunctionType :: IO ()
test_parseFunctionType = do
    let result = parse parseTypeOnly "test" "int -> string"
    case result of
        Right (MkSpan _ _ (TArrow t1 t2)) -> do
            case spanNode t1 of
                TCon "int" -> return ()
                _ -> error "test_parseFunctionType failed: Left side should be int"
            case spanNode t2 of
                TCon "string" -> return ()
                _ -> error "test_parseFunctionType failed: Right side should be string"
        Right other -> error $ "test_parseFunctionType failed: Expected TArrow, got: " ++ show other
        Left err -> error $ "test_parseFunctionType failed: " ++ errorBundlePretty err

test_parseNestedRecord :: IO ()
test_parseNestedRecord = do
    let result = parse parseTypeOnly "test" "{ x: { y: int }, z: string }"
    case result of
        Right (MkSpan _ _ (TRecordExtend "x" t1 rest1)) -> do
            case spanNode t1 of
                TRecordExtend "y" t2 rest2 -> do
                    case spanNode t2 of
                        TCon "int" -> return ()
                        _ -> error "test_parseNestedRecord failed: Inner field should be int"
                    case spanNode rest2 of
                        TRecordEmpty -> return ()
                        _ -> error "test_parseNestedRecord failed: Inner record should be empty"
                _ -> error "test_parseNestedRecord failed: Expected inner record"
            case spanNode rest1 of
                TRecordExtend "z" t3 rest3 -> do
                    case spanNode t3 of
                        TCon "string" -> return ()
                        _ -> error "test_parseNestedRecord failed: Second field should be string"
                    case spanNode rest3 of
                        TRecordEmpty -> return ()
                        _ -> error "test_parseNestedRecord failed: Should end with empty record"
                _ -> error "test_parseNestedRecord failed: Expected second field"
        Right other -> error $ "test_parseNestedRecord failed: Expected TRecordExtend, got: " ++ show other
        Left err -> error $ "test_parseNestedRecord failed: " ++ errorBundlePretty err

test_parseInvalidFails :: IO ()
test_parseInvalidFails = do
    let invalidTypes =
            [ ("unterminated record", "{")
            , ("missing colon", "{ x int }")
            , ("missing return type", "int ->")
            , ("missing argument type", "-> int")
            , ("unclosed tuple", "(int, string")
            , ("unclosed array", "[int")
            ]

    mapM_
        ( \(desc, typ) -> do
            let result = parse parseTypeOnly "test" typ
            case result of
                Right _ -> error $ "test_parseInvalidFails failed (" ++ desc ++ "): Should have failed to parse: " ++ T.unpack typ
                Left _ -> return ()
        )
        invalidTypes