module DesugarTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Ceca.AST
import Ceca.Desugar
import Data.Text (pack)
import Text.Megaparsec.Pos (initialPos)

desugarTests :: TestTree
desugarTests = testGroup "Desugar" [
    testProperty "identity lambda" testIdentityLambda,
    testProperty "tuple pattern" testTuplePattern
  ]

testIdentityLambda :: Property
testIdentityLambda = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        desugared = desugar e
    case coreExpr (unCore desugared) of
        Just _ -> success
        Nothing -> failure

testTuplePattern :: Property
testTuplePattern = property $ do
    let pat1 = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        pat2 = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "y"))
        pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PTuple [pat1, pat2])
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        desugared = desugar e
    case coreExpr (unCore desugared) of
        Just _ -> success
        Nothing -> failure