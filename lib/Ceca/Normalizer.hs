module Ceca.Normalizer where

import Ceca.Core
import Data.List (lookup)
import Data.Text (Text, unpack)
import Text.Read (readMaybe)

normalize :: CoreExpr -> CoreExpr
normalize = normalizeExpr

normalizeExpr :: CoreExpr -> CoreExpr
normalizeExpr e@(CoreExpr t node) = case node of
  CApp e1 e2 -> case coreNode (normalizeExpr e1) of
    CAbs x body -> normalizeExpr $ substitute x (normalizeExpr e2) body
    _ ->
      let e1' = normalizeExpr e1
          e2' = normalizeExpr e2
       in CoreExpr t (CApp e1' e2')
  CAbs x body -> CoreExpr t (CAbs x (normalizeExpr body))
  CRecord fields ->
    CoreExpr t (CRecord (map (\(l, ex) -> (l, normalizeExpr ex)) fields))
  CTuple es -> CoreExpr t (CTuple (map normalizeExpr es))
  CArray es -> CoreExpr t (CArray (map normalizeExpr es))
  CProj ex l ->
    let ex' = normalizeExpr ex
     in case coreNode ex' of
          CRecord fields -> case lookup l fields of
            Just val -> normalizeExpr val
            Nothing -> CoreExpr t (CProj ex' l)
          CTuple es -> case parseIndex l of
            Just i | i >= 0 && i < length es -> normalizeExpr (es !! i)
            _ -> CoreExpr t (CProj ex' l)
          _ -> CoreExpr t (CProj ex' l)
  CExtend ex l val -> CoreExpr t (CExtend (normalizeExpr ex) l (normalizeExpr val))
  CRestrict ex l -> CoreExpr t (CRestrict (normalizeExpr ex) l)
  CAnnot ex ty -> CoreExpr t (CAnnot (normalizeExpr ex) ty)
  _ -> e

substitute :: Text -> CoreExpr -> CoreExpr -> CoreExpr
substitute x replacement e@(CoreExpr t node) = case node of
  CVar y | x == y -> replacement
  CAbs y body
    | x == y -> e
    | otherwise -> CoreExpr t (CAbs y (substitute x replacement body))
  CApp e1 e2 -> CoreExpr t (CApp (substitute x replacement e1) (substitute x replacement e2))
  CRecord fields ->
    CoreExpr t (CRecord (map (\(l, ex) -> (l, substitute x replacement ex)) fields))
  CTuple es -> CoreExpr t (CTuple (map (substitute x replacement) es))
  CArray es -> CoreExpr t (CArray (map (substitute x replacement) es))
  CProj ex l -> CoreExpr t (CProj (substitute x replacement ex) l)
  CExtend ex l val -> CoreExpr t (CExtend (substitute x replacement ex) l (substitute x replacement val))
  CRestrict ex l -> CoreExpr t (CRestrict (substitute x replacement ex) l)
  CAnnot ex ty -> CoreExpr t (CAnnot (substitute x replacement ex) ty)
  _ -> e

parseIndex :: Text -> Maybe Int
parseIndex = readMaybe . unpack
