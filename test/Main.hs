module Main where

import TypeTest (typeTestMain)
import ExprTest (patternExprTestMain)
import Test.Tasty
import TypeCheckerTest (typeCheckerTests)
import DesugarTest (desugarTests)
import NormalizerTest (normalizerTests)

main :: IO ()
main = do
    typeTestMain
    patternExprTestMain
    defaultMain $ testGroup "Ceca" [
        typeCheckerTests,
        desugarTests,
        normalizerTests
      ]