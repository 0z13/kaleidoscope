module Syntax where

type Name = String

data Expr 
  = Float Double
  | Times Expr Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | UMinus Expr
  | Divide Expr Expr
  | Var String
  | Call Name [Expr]
  | Function Name [Expr] Expr
  | Extern Name [Expr]
  deriving Show


