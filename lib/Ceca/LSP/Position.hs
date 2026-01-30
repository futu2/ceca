{-# LANGUAGE OverloadedStrings #-}

module Ceca.LSP.Position
  ( positionToOffset
  , offsetToPosition
  , posInSpan
  , posLeq
  , posGeq
  , lspPositionToLineCol
  , sourcePosToPosition
  , spanToRange
  ) where

import Ceca.AST (Span, spanEnd, spanStart)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position(..), Range(..))
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

offsetToPosition :: T.Text -> Int -> Position
offsetToPosition text offset =
  let clamped = max 0 (min offset (T.length text))
      (before, _) = T.splitAt clamped text
      lines = T.splitOn "\n" before
      lineIndex = max 0 (length lines - 1)
      colIndex = case reverse lines of
        [] -> 0
        (l:_) -> T.length l
  in Position { _line = fromIntegral lineIndex, _character = fromIntegral colIndex }

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

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition sp =
  let line = unPos (sourceLine sp) - 1
      col = unPos (sourceColumn sp) - 1
  in Position { _line = fromIntegral (max 0 line), _character = fromIntegral (max 0 col) }

spanToRange :: Span a -> Range
spanToRange span =
  Range
    { _start = sourcePosToPosition (spanStart span)
    , _end = sourcePosToPosition (spanEnd span)
    }
