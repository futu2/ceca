{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.Position
  ( positionToOffset
  , posInSpan
  , posLeq
  , posGeq
  , lspPositionToLineCol
  ) where

import Ceca.AST (Span, spanEnd, spanStart)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position(..))
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, unPos)

positionToOffset :: T.Text -> Position -> Int
positionToOffset text pos =
  let Position { _line = line, _character = ch } = pos
      lineIndex = fromIntegral line
      charIndex = fromIntegral ch
      lines = T.splitOn "\n" text
      lineCount = length lines
      clampLine = max 0 lineIndex
  in if clampLine >= lineCount
       then T.length text
       else
         let (pre, rest) = splitAt clampLine lines
             preLen = sum (map T.length pre) + length pre
             currentLine = case rest of
               [] -> ""
               (l:_) -> l
             col = min charIndex (T.length currentLine)
         in preLen + col

posInSpan :: Position -> Span a -> Bool
posInSpan pos span =
  let (line, col) = lspPositionToLineCol pos
      startPos = spanStart span
      endPos = spanEnd span
  in posGeq (line, col) startPos && posLeq (line, col) endPos

posLeq :: (Int, Int) -> SourcePos -> Bool
posLeq (line, col) sp =
  let spLine = unPos (sourceLine sp)
      spCol = unPos (sourceColumn sp)
  in line < spLine || (line == spLine && col <= spCol)

posGeq :: (Int, Int) -> SourcePos -> Bool
posGeq (line, col) sp =
  let spLine = unPos (sourceLine sp)
      spCol = unPos (sourceColumn sp)
  in line > spLine || (line == spLine && col >= spCol)

lspPositionToLineCol :: Position -> (Int, Int)
lspPositionToLineCol Position { _line = line, _character = ch } =
  (fromIntegral line + 1, fromIntegral ch + 1)
