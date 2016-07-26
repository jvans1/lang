module Store where
import Data.HashMap.Lazy(empty, insert, HashMap)
--Insert 
--Check for duplicate function declarations
--as well as top level items that are not functions
--
--
import Parser.Types(Expr(..))

--TODO: Check duplicate functions
--TODO: Check top level declarations that are not functions

store :: [Expr] -> Either a (HashMap String Expr)
store xs = return $ foldl put empty xs
  where 
    put :: (HashMap String Expr) -> Expr -> (HashMap String Expr)
    put hm fn@(Function name _ _ _ _) = insert name fn hm
    put _ _ = error "Top level is not a function! Make a better error messag here"
