module Main where

import TypeTest ( typeTestMain )
import ExprTest (patternExprTestMain)

main :: IO ()
main = do
    typeTestMain
    patternExprTestMain