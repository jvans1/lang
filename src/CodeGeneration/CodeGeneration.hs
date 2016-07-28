module CodeGeneration.CodeGeneration where
import SemanticAnalysis.TypeResolution
import CodeGeneration.Types
import SemanticAnalysis.Types(Functions(..))
import Parser.Types(Type(..), Expr(Digit, Call))
import SemanticAnalysis.Types(Program(..), Function(..))
import Data.HashMap.Lazy(HashMap, elems)
import qualified Data.HashMap.Lazy as Map

generate :: Program -> Either a String
generate xs = error "not used"

generate' :: Program -> [CFunction]
generate' (Program fns) = fmap go fns
  where
    go :: Function -> CFunction
    go (Function name []) = CFunction Nothing [] name
    go (Function name a) = CFunction (Just a) (argsIn a) name

argsIn :: [Instruction] -> [(Text, Type)]
argsIn xs = concat $ fmap argsIn' [] xs
  where
    argsIn' :: Instruction -> [(Text, Type)]
    argsIn' (FnCall _ inst1 inst2) = argsIn' inst1 ++ argsIn' inst2
    argsIn' (Argument var ty)      = (var, ty)


toString :: Function -> String
toString (Function name vars) = error "toString "  --typeToC retType ++ " " ++ name ++ "()\n{" ++ "return " ++ bodyToC body ++ ";" ++ "\n}"
toString _ = error "Need to finish this definition"

typeToC :: Type -> String
typeToC Integer = "int"
typeToC _ = error "type to c"

bodyToC :: Expr -> String
bodyToC (Digit a) = show a
bodyToC (Call "+" (arg1:arg2:[])) = "\n" ++ bodyToC arg1 ++ " + " ++ bodyToC arg2 ++ "\n"
bodyToC a = error $ show a
