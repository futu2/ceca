module Main (main) where

import Ceca.Desugar (desugar, unCore)
import Ceca.Normalizer (normalize)
import Ceca.Parser (parseProgram)
import Ceca.TypeChecker (typeCheck)
import Ceca.Types (TypeError)
import Data.Text (pack)
import System.Environment (getArgs)
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  -- print $ runParser parseProgram "testtt.ceca" $ pack "(x=>x) (x=>x)"
  args <- getArgs
  case args of
    [file] -> processFile file
    _ -> putStrLn "Usage: ceca <file.ceca>"

processFile :: FilePath -> IO ()
processFile file = do
  content <- readFile file
  case runParser parseProgram file (pack content) of
    Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
    Right expr -> case typeCheck expr of
      Left typeErr -> putStrLn $ "Type error: " ++ show typeErr
      Right ty -> do
        putStrLn $ "Type: " ++ show ty
        let core = desugar expr
            normalized = normalize core
        putStrLn $ "Normalized core AST:"
        putStrLn $ show (unCore normalized)
