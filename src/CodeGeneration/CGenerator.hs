{-# LANGUAGE OverloadedStrings #-}
module CodeGeneration.CGenerator(generate) where
import Base
import Types



generate :: [TypedFunction] -> Text
generate program = mconcat (map generateFn program)


generateFn :: TypedFunction -> Text
generateFn (TypedFunction _ name Integer _ body) =  "int " ++ name ++ "()" ++ (defineBody body)



defineBody :: [TypedExpr] -> Text
defineBody ((_, Lit (Digit val) _):_) = "{ return " ++ (tshow val) ++ "; }"
