{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.CGenerator(generate) where
import Base
import Types

generate :: Program -> Text
generate program = foldr (<>) "" (fmap generateFn program) 

generateFn :: TypedFunction -> Text
generateFn (TypedFunction _ name Integer fnargs body) =  "int " ++ name ++ defineArgs fnargs ++ (defineBody body)

defineBody :: [TypedExpr] -> Text
defineBody ((_, Lit (Digit val) _):_) = "{ return " ++ (tshow val) ++ "; }"
defineBody ((_, Var val _):_)  = "{ return " ++ val ++ "; }"

defineArgs :: [TypedExpr] -> Text
defineArgs args = mconcat ["(", argdefs , ")" ]
  where
    argdefs :: Text
    argdefs = mconcat $ intersperse ", "  $ fmap typeFor args
    typeFor :: TypedExpr -> Text
    typeFor (Integer, Var a _) = "int " ++ a
--TODO can we rep that this is only a Var on type level
