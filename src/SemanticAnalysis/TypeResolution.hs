module SemanticAnalysis.TypeResolution(resolve) where
import qualified Parser.Types as P
import Data.Text(pack, Text)
import Parser.Types(Type, Expr(Var))
import SemanticAnalysis.Types
import Data.HashMap.Strict(HashMap, toList, fromList)
import qualified Data.HashMap.Lazy as Map

data TypedVar = Typed Type String deriving (Show, Eq)

resolve :: HashMap String Expr -> Either a Program
resolve expr = return . Program .  stringKeysToText $ Map.mapWithKey evalTop expr
  where
    evalTop :: String -> P.Expr -> Function
    evalTop name (P.Function _ argTypes retType args body) = Function (pack name) (error "arguments here") (argVars argTypes args) (error "body here")
    stringKeysToText :: HashMap String a -> HashMap Text a
    stringKeysToText = fromList . fmap (\(x, y) -> (pack x, y)) . toList


argVars :: [Type] -> [Expr] -> [Argument]
argVars types vars = map mkArg (zip types vars)
  where
    mkArg :: (Type, Expr) -> Argument
    mkArg (t, Var varname)  = Argument t (pack varname)
    mkArg _ = error "non exhaustive pattern match in make arg"
  --TODO: While we can guarantee this is never hit,
  --hit, this still sucks we should represent this with the type system
