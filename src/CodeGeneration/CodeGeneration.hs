module CodeGeneration.CodeGeneration where
import SemanticAnalysis.TypeResolution
import Parser.Types(Type(..), Expr(Digit, Call))
import Data.HashMap.Lazy(HashMap, elems)
import qualified Data.HashMap.Lazy as Map

generate :: HashMap String TypedExpr -> Either a String
generate xs = return . mconcat . elems $ Map.map toString xs

toString :: TypedExpr -> String
toString (Function name retType varArgs body) = typeToC retType ++ " " ++ name ++ "()\n{" ++ "return " ++ bodyToC body ++ ";" ++ "\n}"
toString _ = error "Need to finish this definition"

typeToC :: Type -> String
typeToC Integer = "int"
typeToC _ = error "type to c"

bodyToC :: Expr -> String
bodyToC (Digit a) = show a
bodyToC (Call "+" (arg1:arg2:[])) = "\n" ++ bodyToC arg1 ++ " + " ++ bodyToC arg2 ++ "\n"
bodyToC a = error $ show a
