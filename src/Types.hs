module Types where
import Data.Text
import Base
import Data.HashMap.Strict(HashMap)

data Type = Integer deriving (Show, Eq)

data Program = Program {
  functions :: HashMap Text Expr
} deriving (Show, Eq)

data Expr =
  Function {
      fnName :: Text 
      , fnArgTypes :: [Type] 
      , retType :: Type 
      , fnArgs :: [Expr] 
      , fnbody :: Expr
    }
  | Assignment Expr Expr
  | Call Text [Expr]
  | Var Text
  | Digit Integer deriving (Show, Eq)
