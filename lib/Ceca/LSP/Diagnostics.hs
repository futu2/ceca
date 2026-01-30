{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.Diagnostics
  ( publishDiagnosticsForDoc
  , clearDiagnosticsForDoc
  ) where

import Ceca.AST (Expr, ExprNode(..), Span, Type, spanEnd, spanStart, spanNode)
import Ceca.Importer (resolveImports)
import Ceca.LSP.Position (offsetToPosition, positionToOffset, sourcePosToPosition)
import Ceca.Parser (parseProgram)
import Ceca.TypeChecker (typeCheck)
import Ceca.Types (TypeError(..))
import Control.Exception (SomeException, displayException, evaluate, toException, try)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.LSP.Diagnostics (DiagnosticsBySource, partitionBySource)
import Language.LSP.Protocol.Types (Diagnostic(..), DiagnosticSeverity(..), Position(..), Range(..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (LspM, publishDiagnostics)
import System.FilePath (takeDirectory)
import Text.Megaparsec (ParseErrorBundle, runParser, bundleErrors)
import Text.Megaparsec.Error (ParseError(..))

publishDiagnosticsForDoc :: LSP.Uri -> T.Text -> LspM () ()
publishDiagnosticsForDoc uri text = do
  diagnostics <- liftIO $ diagnosticsForTextIO uri text
  let bySource = diagnosticsBySource diagnostics
  publishDiagnostics maxDiagnostics (LSP.toNormalizedUri uri) Nothing bySource

clearDiagnosticsForDoc :: LSP.Uri -> LspM () ()
clearDiagnosticsForDoc uri =
  publishDiagnostics maxDiagnostics (LSP.toNormalizedUri uri) Nothing (diagnosticsBySource [])

maxDiagnostics :: Int
maxDiagnostics = 100

diagnosticsForTextIO :: LSP.Uri -> T.Text -> IO [LSP.Diagnostic]
diagnosticsForTextIO uri text =
  let sourceName = case LSP.uriToFilePath uri of
        Just path -> path
        Nothing -> "<lsp>"
  in case runParser parseProgram sourceName text of
    Left bundle -> pure [parseErrorDiagnostic text bundle]
    Right expr -> do
      resolved <- resolveImportsForDiagnostics uri expr
      case resolved of
        Left err -> pure [importErrorDiagnostic text err]
        Right resolvedExpr -> do
          result <- try (evaluate (typeCheck resolvedExpr)) :: IO (Either SomeException (Either TypeError Type))
          case result of
            Left err -> pure [internalErrorDiagnostic text err]
            Right (Left err) -> pure [typeErrorDiagnostic text err]
            Right (Right _) -> pure []

resolveImportsForDiagnostics :: LSP.Uri -> Expr -> IO (Either SomeException Expr)
resolveImportsForDiagnostics uri expr =
  case LSP.uriToFilePath uri of
    Nothing ->
      if exprHasImport expr
        then pure $ Left (toException (userError "Cannot resolve imports for non-file URI"))
        else pure $ Right expr
    Just path -> try (resolveImports (takeDirectory path) expr)

exprHasImport :: Expr -> Bool
exprHasImport expr = case spanNode expr of
  EImport _ -> True
  EVar _ -> False
  ELit _ -> False
  EBuiltin _ -> False
  EAbs _ body -> exprHasImport body
  EApp e1 e2 -> exprHasImport e1 || exprHasImport e2
  ERecord fields -> any (exprHasImport . snd) fields
  ETuple es -> any exprHasImport es
  EArray es -> any exprHasImport es
  EProj e _ -> exprHasImport e
  EExtend e1 _ e2 -> exprHasImport e1 || exprHasImport e2
  ERestrict e _ -> exprHasImport e
  ELet _ _ e1 e2 -> exprHasImport e1 || exprHasImport e2
  EAnnot e _ -> exprHasImport e

parseErrorDiagnostic :: T.Text -> ParseErrorBundle T.Text e -> LSP.Diagnostic
parseErrorDiagnostic text bundle =
  let err = NE.head (bundleErrors bundle)
      offset = parseErrorOffset err
      startPos = offsetToPosition text offset
      endOffset = min (offset + 1) (T.length text)
      endPos = offsetToPosition text endOffset
      range = Range { _start = startPos, _end = endPos }
      Position { _line = line, _character = ch } = startPos
      message =
        "Parse error at line "
          <> T.pack (show (line + 1))
          <> ", column "
          <> T.pack (show (ch + 1))
  in mkDiagnostic range message

parseErrorOffset :: ParseError s e -> Int
parseErrorOffset err = case err of
  TrivialError offset _ _ -> offset
  FancyError offset _ -> offset

typeErrorDiagnostic :: T.Text -> TypeError -> LSP.Diagnostic
typeErrorDiagnostic text err =
  let range = case typeErrorSpan err of
        Just span -> spanToRangeWithText text span
        Nothing -> fallbackRange text
      message = typeErrorMessage err
  in mkDiagnostic range message

typeErrorSpan :: TypeError -> Maybe (Span ExprNode)
typeErrorSpan terr = case terr of
  TypeErrorAt span _ -> Just span
  _ -> Nothing

typeErrorMessage :: TypeError -> T.Text
typeErrorMessage terr = case terr of
  UnboundVariable name -> "Unbound variable: " <> name
  InfiniteType name ty -> "Infinite type: " <> name <> " in " <> T.pack (show ty)
  UnificationFail t1 t2 -> "Type mismatch: " <> T.pack (show t1) <> " vs " <> T.pack (show t2)
  TypeErrorAt _ inner -> typeErrorMessage inner

spanToRangeWithText :: T.Text -> Span a -> LSP.Range
spanToRangeWithText text span =
  let startPos = sourcePosToPosition (spanStart span)
      endPos = sourcePosToPosition (spanEnd span)
  in if startPos == endPos
       then
         let startOffset = positionToOffset text startPos
             endOffset = min (startOffset + 1) (T.length text)
             endPos' = offsetToPosition text endOffset
         in Range { _start = startPos, _end = endPos' }
       else Range { _start = startPos, _end = endPos }

fallbackRange :: T.Text -> LSP.Range
fallbackRange text =
  let startPos = Position 0 0
      endPos = offsetToPosition text (min 1 (T.length text))
  in Range { _start = startPos, _end = endPos }

mkDiagnostic :: Range -> T.Text -> LSP.Diagnostic
mkDiagnostic range message =
  Diagnostic
    { _range = range
    , _severity = Just DiagnosticSeverity_Error
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "ceca"
    , _message = message
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

internalErrorDiagnostic :: T.Text -> SomeException -> LSP.Diagnostic
internalErrorDiagnostic text err =
  mkDiagnostic (fallbackRange text) ("Internal error: " <> T.pack (displayException err))

importErrorDiagnostic :: T.Text -> SomeException -> LSP.Diagnostic
importErrorDiagnostic text err =
  mkDiagnostic (fallbackRange text) ("Import error: " <> T.pack (displayException err))

diagnosticsBySource :: [LSP.Diagnostic] -> DiagnosticsBySource
diagnosticsBySource diagnostics
  | null diagnostics = Map.singleton (Just "ceca") mempty
  | otherwise = partitionBySource diagnostics
