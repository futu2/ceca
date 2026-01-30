module Main (main) where

import Ceca.Core (desugar)
import Ceca.Importer (resolveImports)
import Ceca.Normalizer (normalize)
import Ceca.Parser (parseProgram)
import Ceca.TypeChecker (typeCheck)
import qualified Data.Set as Set
import Data.Text (pack)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import Text.Megaparsec (runParser)
import Ceca.Pretty (prettyPrintExpr)
import qualified Data.Text as T
import Options.Applicative
import Ceca.LSP (runLSPServer)

data Mode = Full | TypeCheck | LSP deriving (Show, Eq, Read)

data Options = Options
  { mode :: Mode
  , file :: Maybe FilePath
  } deriving (Show)

parseMode :: Parser Mode
parseMode = option (eitherReader parseModeFromString)
  ( long "mode"
  <> short 'm'
  <> metavar "MODE"
  <> value Full
  <> help "Mode: full (default), type-check, lsp"
  )
  where
    parseModeFromString "full" = Right Full
    parseModeFromString "type-check" = Right TypeCheck
    parseModeFromString "lsp" = Right LSP
    parseModeFromString s = Left $ "Invalid mode: " ++ s

parseFile :: Parser (Maybe FilePath)
parseFile = optional $ strOption
  ( long "file"
  <> short 'f'
  <> metavar "FILE"
  <> help "Input Ceca file"
  )

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> parseFile

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
  ( fullDesc
  <> progDesc "Ceca language processor"
  <> header "ceca - a functional programming language"
  )

main :: IO ()
main = do
  options <- execParser opts
  case mode options of
    Full -> case file options of
      Just f -> processFile f
      Nothing -> putStrLn "Error: --file required for full mode"
    TypeCheck -> case file options of
      Just f -> typeCheckFile f
      Nothing -> putStrLn "Error: --file required for type-check mode"
    LSP -> runLSP

processFile :: FilePath -> IO ()
processFile file = do
    content <- readFile file
    case runParser parseProgram file (pack content) of
      Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
      Right expr -> do
        resolved <- resolveImports (takeDirectory file) expr
        putStrLn $ T.unpack $ prettyPrintExpr resolved
        case typeCheck resolved of
          Left typeErr -> putStrLn $ "Type error: " ++ show typeErr
          Right ty -> do
            putStrLn $ "Type: " ++ show ty
            case desugar resolved of
              Left err -> putStrLn $ "Core desugar error: " ++ show err
              Right core -> do
                let normalized = normalize core
                putStrLn $ "Normalized core AST:"
                putStrLn $ show normalized

typeCheckFile :: FilePath -> IO ()
typeCheckFile file = do
    content <- readFile file
    case runParser parseProgram file (pack content) of
      Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
      Right expr -> do
        resolved <- resolveImports (takeDirectory file) expr
        case typeCheck resolved of
          Left typeErr -> putStrLn $ "Type error: " ++ show typeErr
          Right ty -> putStrLn $ "Type: " ++ show ty

runLSP :: IO ()
runLSP = runLSPServer
