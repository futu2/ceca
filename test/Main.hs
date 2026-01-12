module Main where

import TypeTest (typeTestMain)
import ExprTest (patternExprTestMain)
import Test.Tasty
import TypeCheckerTest (typeCheckerTests)
import DesugarTest (desugarTests)
import NormalizerTest (normalizerTests)
import PrettyTest (tests)

main :: IO ()
main = do
    typeTestMain
    patternExprTestMain
    defaultMain $ testGroup "Ceca" [
        tests,
        typeCheckerTests,
        desugarTests,
        normalizerTests
      ]