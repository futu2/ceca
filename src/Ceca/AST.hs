{-# LANGUAGE DeriveDataTypeable #-}

module Ceca.AST where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos)
import Data.Data ( Data )
import Data.Generics.Uniplate.Direct

data Span a = MkSpan
  { spanStart :: SourcePos
  , spanEnd :: SourcePos
  , spanNode :: a
  }
  deriving (Eq, Show, Data)

-- Types (simplified - no explicit forall)
type Type = Span TypeNode
data TypeNode
  = TVar Text          -- Type variable: ?a, ?b (note: with ? prefix per your syntax, and it could be use as both normal type and row type)
  | TCon Text          -- Type constructor: string, int, float, bool
  | TArrow Type Type   -- Function type: t1 -> t2
  | TTuple [Type]      -- Tuple type: (t1, t2, ..., tn)
  | TArray Type        -- Array type: [t]
  | TRecordEmpty       -- Empty row: {}
  | TRecordExtend Text Type Type  -- Row extension: {l: T | R}
  -- which syntax sugar should be {l1: T1, l2 : T2 | R} for multiple extend
  deriving (Eq, Show, Data)

type Expr = Span ExprNode
data ExprNode
  = EVar Text            -- Variable: x
  | ELit Literal         -- Literal: 42, 1.0, "hello", true
  | EAbs Pattern Expr    -- Lambda: pattern => expr
  | EApp Expr Expr       -- Application: e1 e2
  | ERecord [(Text, Expr)] -- Record: {l1 = e1, ..., ln = en}
  | ETuple [Expr]        -- Tuple: (e1, e2, ..., en)
  | EArray [Expr]        -- Array: [e1, e2, ..., en]
  | EProj Expr Text      -- Projection: e.l
  | EExtend Expr Text Expr -- Extension: {l = e' | e} (for record extension)
  | ERestrict Expr Text  -- Restriction: {-l | e} means drop field l for record e
    -- and in syntax sugar, extend and restrict could be used mixed
    -- e.g. {-l, l = e' | e} means remove old l key-value and attach new l wiht value e'
    -- same for {-l1, l1 = e', -l2, l3 = e'' | e }
  | ELet Text (Maybe Type) Expr Expr  -- Let binding: x : t? = e1 ; e2 
    -- semi-colum could be ommited in the end of line and following a newline, that means the following code is equal
    -- x : t?
    --   = e1
    --
    -- e2
  | EAnnot Expr Type     -- Type annotation: e : t
  | EImport FilePath     -- Import: @./test.ceca (first-class import expression)
  deriving (Eq, Show, Data)

data Literal
  = LInt Integer
  | LFloat Double
  | LString Text
  | LBool Bool
  deriving (Eq, Show, Data)

type Pattern = Span PatternNode
data PatternNode
  = PVar Text                -- Variable pattern: x
  | PWildcard                -- Wildcard pattern: _, ..._ (for rest pattern)
  | PLit Literal             -- Literal pattern: 42, "hello", etc.
  | PRecord [(Text, Pattern)] (Maybe Pattern) -- Record pattern: {a, b, ...rest}
    -- First list: fields to match (can be just field names without binding)
    -- Maybe Pattern: optional rest pattern
  | PTuple [Pattern]         -- Tuple pattern: (a, b, c)
  | PArray [Pattern]         -- Array pattern: [a, b, c]
  deriving (Eq, Show, Data)