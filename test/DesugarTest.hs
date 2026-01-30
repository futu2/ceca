module DesugarTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Ceca.AST
import Ceca.Core
import Data.Text (pack)
import Text.Megaparsec.Pos (initialPos)

desugarTests :: TestTree
desugarTests = testGroup "Desugar" [
    testProperty "identity lambda" testIdentityLambda,
    testProperty "tuple pattern" testTuplePattern,
    testProperty "record pattern" testRecordPattern,
    testProperty "let desugaring" testLetDesugaring
  ]

testIdentityLambda :: Property
testIdentityLambda = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        desugared = desugar e
    case desugared of
        Right _ -> success
        Left _ -> failure

testTuplePattern :: Property
testTuplePattern = property $ do
    let pat1 = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        pat2 = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "y"))
        pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PTuple [pat1, pat2])
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        desugared = desugar e
    case desugared of
        Right _ -> success
        Left _ -> failure

testRecordPattern :: Property
testRecordPattern = property $ do
    let pat1 = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        pat2 = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "y"))
        pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PRecord [(pack "a", pat1), (pack "b", pat2)] Nothing)
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        desugared = desugar e
    case desugared of
        Right _ -> success
        Left _ -> failure

testLetDesugaring :: Property
testLetDesugaring = property $ do
    let e1 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 1))
        e2 = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (ELet (MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))) Nothing e1 e2)
        desugared = desugar e
    case desugared of
        Right _ -> success
        Left _ -> failure
