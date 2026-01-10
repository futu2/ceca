module NormalizerTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Ceca.AST
import Ceca.Desugar
import Ceca.Normalizer
import Data.Text (pack)
import Text.Megaparsec.Pos (initialPos)

normalizerTests :: TestTree
normalizerTests = testGroup "Normalizer" [
    testProperty "identity application" testIdentityApplication
  ]

testIdentityApplication :: Property
testIdentityApplication = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        lam = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        arg = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 42))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lam arg)
        core = desugar e
        normalized = normalize core
    case coreExpr (unCore normalized) of
        Just _ -> success
        Nothing -> failure