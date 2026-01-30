module NormalizerTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Ceca.AST
import Ceca.Core
import Ceca.Normalizer
import Data.Text (pack)
import Text.Megaparsec.Pos (initialPos)

normalizerTests :: TestTree
normalizerTests = testGroup "Normalizer" [
    testProperty "identity application" testIdentityApplication,
    testProperty "record projection" testRecordProjection
  ]

testIdentityApplication :: Property
testIdentityApplication = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        lam = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        arg = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 42))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lam arg)
        core = desugar e
    case core of
        Left _ -> failure
        Right coreExpr -> case normalize coreExpr of
            CoreExpr _ (CLit (LInt n)) | n == 42 -> success
            _ -> failure

testRecordProjection :: Property
testRecordProjection = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EProj (MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))) (pack "name"))
        lam = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        rec = MkSpan (initialPos "dummy") (initialPos "dummy") (ERecord [(pack "name", MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LString (pack "yahoo"))))])
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lam rec)
        core = desugar e
    case core of
        Left _ -> failure
        Right coreExpr -> case normalize coreExpr of
            CoreExpr _ (CLit (LString s)) | s == pack "yahoo" -> success
            _ -> failure
