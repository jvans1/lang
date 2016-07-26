module CodeGeneration.CodeGeneration where
import SemanticAnalysis.TypeResolution
import SemanticAnalysis.Types(Functions(..))
import Parser.Types(Type(..), Expr(Digit, Call))
import SemanticAnalysis.Types(Program(..), Function(..))
import Data.HashMap.Lazy(HashMap, elems)
import qualified Data.HashMap.Lazy as Map

generate :: Program -> Either a String
generate xs = return . mconcat . elems $ Map.map toString (functions xs)

toString :: Function -> String
toString (Function name vars args inst) =  typeToC retType ++ " " ++ name ++ "()\n{" ++ "return " ++ bodyToC body ++ ";" ++ "\n}"
toString _ = error "Need to finish this definition"

typeToC :: Type -> String
typeToC Integer = "int"
typeToC _ = error "type to c"

bodyToC :: Expr -> String
bodyToC (Digit a) = show a
bodyToC (Call "+" (arg1:arg2:[])) = "\n" ++ bodyToC arg1 ++ " + " ++ bodyToC arg2 ++ "\n"
bodyToC a = error $ show a
