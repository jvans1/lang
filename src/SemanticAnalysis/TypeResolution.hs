module SemanticAnalysis.TypeResolution where
import qualified Parser.Types as P
import Parser.Types(Type, Expr(Var))
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as Map

data TypedVar = Typed Type String deriving (Show, Eq)
data TypedExpr = Function String Type [TypedVar] Expr deriving (Show, Eq)

resolve :: HashMap String Expr -> Either a (HashMap String TypedExpr)
resolve expr = return $ Map.map addTypes expr
  where
    addTypes :: Expr -> TypedExpr
    addTypes (P.Function name argTypes retType args body) = Function name retType typedArgs body
      where
        typedArgs :: [TypedVar]
                                --TODO: Making an assumption here that these are of Var constructor
        typedArgs = map (\(typ, Var var) -> Typed typ var ) (zip argTypes args)
