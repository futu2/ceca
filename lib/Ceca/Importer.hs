module Ceca.Importer where

import Ceca.AST
import Ceca.Parser
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (isPrefixOf)
import System.FilePath
import Text.Megaparsec (runParser)
import Data.Text (pack)
import Control.Exception (throwIO)

-- Resolve all EImport nodes in the AST by replacing them with the parsed expressions from the imported files.
-- visited: set of absolute paths currently being processed to detect cycles.
-- baseDir: base directory for resolving relative import paths.
-- expr: the expression to process.
resolveImports :: FilePath -> Expr -> IO Expr
resolveImports = resolveExpr Set.empty

resolveExpr :: Set FilePath -> FilePath -> Expr -> IO Expr
resolveExpr visited baseDir (MkSpan spanStart spanEnd node) = case node of
  EImport path -> do
    let absPath = if "./" `isPrefixOf` path then combine baseDir (drop 2 path) else combine baseDir path
    if absPath `Set.member` visited then
      throwIO $ userError $ "Import cycle detected: " ++ absPath
    else
      return ()
    content <- readFile absPath
    case runParser parseProgram absPath (pack content) of
      Left err -> throwIO $ userError $ "Parse error in imported file " ++ absPath ++ ": " ++ show err
      Right importedExpr -> resolveExpr (Set.insert absPath visited) (takeDirectory absPath) importedExpr
  EVar x -> return $ MkSpan spanStart spanEnd (EVar x)
  ELit l -> return $ MkSpan spanStart spanEnd (ELit l)
  EBuiltin t -> return $ MkSpan spanStart spanEnd (EBuiltin t)
  EAbs pat body -> do
    body' <- resolveExpr visited baseDir body
    return $ MkSpan spanStart spanEnd (EAbs pat body')
  EApp e1 e2 -> do
    e1' <- resolveExpr visited baseDir e1
    e2' <- resolveExpr visited baseDir e2
    return $ MkSpan spanStart spanEnd (EApp e1' e2')
  ERecord fields -> do
    fields' <- mapM (\(l, e) -> do e' <- resolveExpr visited baseDir e; return (l, e')) fields
    return $ MkSpan spanStart spanEnd (ERecord fields')
  ETuple es -> do
    es' <- mapM (resolveExpr visited baseDir) es
    return $ MkSpan spanStart spanEnd (ETuple es')
  EArray es -> do
    es' <- mapM (resolveExpr visited baseDir) es
    return $ MkSpan spanStart spanEnd (EArray es')
  EProj e l -> do
    e' <- resolveExpr visited baseDir e
    return $ MkSpan spanStart spanEnd (EProj e' l)
  EExtend e l e2 -> do
    e' <- resolveExpr visited baseDir e
    e2' <- resolveExpr visited baseDir e2
    return $ MkSpan spanStart spanEnd (EExtend e' l e2')
  ERestrict e l -> do
    e' <- resolveExpr visited baseDir e
    return $ MkSpan spanStart spanEnd (ERestrict e' l)
  ELet pat mty e1 e2 -> do
    e1' <- resolveExpr visited baseDir e1
    e2' <- resolveExpr visited baseDir e2
    return $ MkSpan spanStart spanEnd (ELet pat mty e1' e2')
  EAnnot e ty -> do
    e' <- resolveExpr visited baseDir e
    return $ MkSpan spanStart spanEnd (EAnnot e' ty)
