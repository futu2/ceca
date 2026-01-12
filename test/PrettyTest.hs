module PrettyTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Ceca.AST
import Ceca.Pretty
import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec.Pos (initialPos)

dummySpan :: a -> Span a
dummySpan = MkSpan dummyPos dummyPos
  where dummyPos = initialPos "<dummy>"

test_prettyVar :: TestTree
test_prettyVar = testProperty "pretty var" $ property $ do
  name <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let expr = dummySpan (EVar name)
      pretty = prettyPrintExpr expr
  pretty === name

test_prettyLitInt :: TestTree
test_prettyLitInt = testProperty "pretty lit int" $ property $ do
  n <- forAll $ Gen.integral (Range.linear (-1000) 1000)
  let expr = dummySpan (ELit (LInt n))
      pretty = prettyPrintExpr expr
  pretty === T.pack (show n)

test_prettyLitFloat :: TestTree
test_prettyLitFloat = testProperty "pretty lit float" $ property $ do
  n <- forAll $ Gen.double (Range.linearFrac (-1000) 1000)
  let expr = dummySpan (ELit (LFloat n))
      pretty = prettyPrintExpr expr
  pretty === T.pack (show n)

test_prettyLitString :: TestTree
test_prettyLitString = testProperty "pretty lit string" $ property $ do
  s <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode
  let expr = dummySpan (ELit (LString s))
      pretty = prettyPrintExpr expr
  pretty === T.pack "\"" <> s <> T.pack "\""

test_prettyLitBool :: TestTree
test_prettyLitBool = testProperty "pretty lit bool" $ property $ do
  b <- forAll Gen.bool
  let expr = dummySpan (ELit (LBool b))
      pretty = prettyPrintExpr expr
      expected = if b then T.pack "true" else T.pack "false"
  pretty === expected

test_prettyApp :: TestTree
test_prettyApp = testProperty "pretty app" $ property $ do
  name1 <- forAll $ Gen.text (Range.linear 1 5) Gen.alpha
  name2 <- forAll $ Gen.text (Range.linear 1 5) Gen.alpha
  let expr = dummySpan (EApp (dummySpan (EVar name1)) (dummySpan (EVar name2)))
      pretty = prettyPrintExpr expr
  pretty === name1 <> T.pack " " <> name2

test_prettyRecord :: TestTree
test_prettyRecord = testProperty "pretty record" $ property $ do
  fields <- forAll $ Gen.list (Range.linear 0 5) $ do
    k <- Gen.text (Range.linear 1 5) Gen.alpha
    v <- Gen.text (Range.linear 1 5) Gen.alpha
    pure (k, dummySpan (EVar v))
  let expr = dummySpan (ERecord fields)
      pretty = prettyPrintExpr expr
      expected = T.pack "{" <> T.intercalate (T.pack ", ") (map (\(k, e) -> k <> T.pack " = " <> prettyPrintExpr e) fields) <> T.pack "}"
  pretty === expected

test_prettyTuple :: TestTree
test_prettyTuple = testProperty "pretty tuple" $ property $ do
  exprs <- forAll $ Gen.list (Range.linear 2 5) $ dummySpan . EVar <$> Gen.text (Range.linear 1 5) Gen.alpha
  let expr = dummySpan (ETuple exprs)
      pretty = prettyPrintExpr expr
      expected = T.pack "(" <> T.intercalate (T.pack ", ") (map prettyPrintExpr exprs) <> T.pack ")"
  pretty === expected

test_prettyArray :: TestTree
test_prettyArray = testProperty "pretty array" $ property $ do
  exprs <- forAll $ Gen.list (Range.linear 0 5) $ dummySpan . EVar <$> Gen.text (Range.linear 1 5) Gen.alpha
  let expr = dummySpan (EArray exprs)
      pretty = prettyPrintExpr expr
      expected = T.pack "[" <> T.intercalate (T.pack ", ") (map prettyPrintExpr exprs) <> T.pack "]"
  pretty === expected

test_prettyPatternVar :: TestTree
test_prettyPatternVar = testProperty "pretty pattern var" $ property $ do
  name <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let pat = dummySpan (PVar name)
      pretty = prettyPrintPattern pat
  pretty === name

test_prettyTypeVar :: TestTree
test_prettyTypeVar = testProperty "pretty type var" $ property $ do
  name <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let typ = dummySpan (TVar name)
      pretty = prettyPrintType typ
  pretty === name

test_prettyTypeCon :: TestTree
test_prettyTypeCon = testProperty "pretty type con" $ property $ do
  name <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
  let typ = dummySpan (TCon name)
      pretty = prettyPrintType typ
  pretty === name

tests :: TestTree
tests = testGroup "Pretty" [
    test_prettyVar,
    test_prettyLitInt,
    test_prettyLitFloat,
    test_prettyLitString,
    test_prettyLitBool,
    test_prettyApp,
    test_prettyRecord,
    test_prettyTuple,
    test_prettyArray,
    test_prettyPatternVar,
    test_prettyTypeVar,
    test_prettyTypeCon
  ]