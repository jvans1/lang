module Parser.Types where

data Type = Integer deriving (Show, Eq)
data Expr =
  Function String [Type] Type [Expr] Expr
  | Assignment Expr Expr
  | Call String [Expr]
  | Var String
  | Digit Integer deriving (Show, Eq)

