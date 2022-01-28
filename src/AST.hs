module AST where

import Data.Text
import Data.List.NonEmpty

data Expr
  = Id Type Text
  | Lit Literal
  | Let Type Pattern Expr Expr
  | LetRec Type Pattern Expr Expr
  | Lam Type Pattern Expr
  | Case Type Expr (NonEmpty (Pattern, Expr))
  | App Expr (NonEmpty Expr)
  deriving (Show)

data Literal
  = Number Int
  | String Text
  | Tuple [Literal]
  deriving (Show)

data Pattern 
  = Id Text
  | Lit Literal
  | Deconstruct (NonEmpty Pattern)
  deriving (Show)

data Type
  = Type Text
  | Func Type Type
  deriving (Show)
