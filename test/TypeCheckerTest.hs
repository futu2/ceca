module TypeCheckerTest where

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Ceca.AST
import Ceca.TypeChecker
import Ceca.Types
import Data.Text (pack)
import Text.Megaparsec.Pos (initialPos)

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "TypeChecker" [
    testProperty "literal int" testLiteralInt,
    testProperty "literal bool" testLiteralBool,
    testProperty "variable unbound" testUnboundVar,
    testProperty "record" testRecord,
    testProperty "lambda identity" testLambdaIdentity,
    testProperty "application" testApplication,
    testProperty "let binding" testLetBinding,
    testProperty "tuple" testTuple,
    testProperty "array" testArray,
    testProperty "record with function" testRecordWithFunction,
     testProperty "type annotation" testAnnotation,
     testProperty "complex expression" testComplexExpr,
      testProperty "unification error" testUnificationError
    , testProperty "builtin" testBuiltin
    , testProperty "builtin annotation" testBuiltinAnnotation
    , testProperty "let binding with annotation" testLetBindingAnnotation
    ]

testLiteralInt :: Property
testLiteralInt = property $ do
    let e = MkSpan undefined undefined (ELit (LInt 42))
    case typeCheck e of
        Right t -> typeNode t === TCon (pack "int")
        Left _ -> failure

testLiteralBool :: Property
testLiteralBool = property $ do
    let e = MkSpan undefined undefined (ELit (LBool True))
    case typeCheck e of
        Right t -> typeNode t === TCon (pack "bool")
        Left _ -> failure

testUnboundVar :: Property
testUnboundVar = property $ do
    let e = MkSpan undefined undefined (EVar (pack "x"))
    case typeCheck e of
        Left (UnboundVariable _) -> success
        Left (TypeErrorAt _ (UnboundVariable _)) -> success
        _ -> failure

testRecord :: Property
testRecord = property $ do
    let e = MkSpan (initialPos "dummy") (initialPos "dummy") (ERecord [(pack "x", MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 1))), (pack "y", MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LBool True)))])
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testLambdaIdentity :: Property
testLambdaIdentity = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testApplication :: Property
testApplication = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        lam = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        arg = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 42))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lam arg)
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testLetBinding :: Property
testLetBinding = property $ do
    let e1 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 42))
        e2 = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (ELet (MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))) Nothing e1 e2)
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testTuple :: Property
testTuple = property $ do
    let e1 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 1))
        e2 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LBool True))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (ETuple [e1, e2])
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testArray :: Property
testArray = property $ do
    let e1 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 1))
        e2 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 2))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EArray [e1, e2])
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testRecordWithFunction :: Property
testRecordWithFunction = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        lam = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body)
        num = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 1))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (ERecord [(pack "f", lam), (pack "n", num)])
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testAnnotation :: Property
testAnnotation = property $ do
    let e_inner = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 42))
        ty = MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "int"))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAnnot e_inner ty)
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testUnificationError :: Property
testUnificationError = property $ do
  let e1 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 42))
      e2 = MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LBool True))
      e = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp e1 e2)
  case typeCheck e of
    Left _ -> success
    Right _ -> failure

testBuiltin :: Property
testBuiltin = property $ do
  let e = MkSpan (initialPos "dummy") (initialPos "dummy") (EBuiltin (pack "test"))
  case typeCheck e of
    Right t -> case typeNode t of
      TVar _ -> success
      _ -> failure
    Left _ -> failure

testBuiltinAnnotation :: Property
testBuiltinAnnotation = property $ do
  let e_inner = MkSpan (initialPos "dummy") (initialPos "dummy") (EBuiltin (pack "test"))
      ty = MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "int"))
      e = MkSpan (initialPos "dummy") (initialPos "dummy") (EAnnot e_inner ty)
  case typeCheck e of
    Right t -> typeNode t === TCon (pack "int")
    Left _ -> failure

testComplexExpr :: Property
testComplexExpr = property $ do
    let pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))
        body1 = MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))
        lambda1 = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body1)
        record = MkSpan (initialPos "dummy") (initialPos "dummy") (ERecord [(pack "name", MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LString (pack "yahoo1")))), (pack "no", MkSpan (initialPos "dummy") (initialPos "dummy") (ELit (LInt 1)))])
        body2 = MkSpan (initialPos "dummy") (initialPos "dummy") (ETuple [MkSpan (initialPos "dummy") (initialPos "dummy") (EProj (MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))) (pack "name")), MkSpan (initialPos "dummy") (initialPos "dummy") (EProj (MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))) (pack "no"))])
        lambda2 = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs pat body2)
        innerApp = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lambda2 record)
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lambda1 innerApp)
    case typeCheck e of
        Right _ -> success
        Left _ -> failure

testLetBindingAnnotation :: Property
testLetBindingAnnotation = property $ do
    let recordTy = MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend (pack "cino") (MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "string"))) (MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend (pack "accno") (MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "string"))) (MkSpan (initialPos "dummy") (initialPos "dummy") (TRecordExtend (pack "balance") (MkSpan (initialPos "dummy") (initialPos "dummy") (TCon (pack "float"))) (MkSpan (initialPos "dummy") (initialPos "dummy") TRecordEmpty))))))
        e1 = MkSpan (initialPos "dummy") (initialPos "dummy") (EBuiltin (pack "some_builtin"))
        pat = MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "y"))
        lambda = MkSpan (initialPos "dummy") (initialPos "dummy") (EAbs (MkSpan (initialPos "dummy") (initialPos "dummy") (PVar (pack "x"))) (MkSpan (initialPos "dummy") (initialPos "dummy") (EProj (MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "x"))) (pack "cino"))))
        e2 = MkSpan (initialPos "dummy") (initialPos "dummy") (EApp lambda (MkSpan (initialPos "dummy") (initialPos "dummy") (EVar (pack "y"))))
        e = MkSpan (initialPos "dummy") (initialPos "dummy") (ELet pat (Just recordTy) e1 e2)
    case typeCheck e of
        Right t -> typeNode t === TCon (pack "string")
        Left _ -> failure
