{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(resolve) where
import qualified Data.List.NonEmpty as NE
import Types
import Data.HashMap.Strict(mapWithKey)
import Base hiding (mapWithKey)


resolve :: Program -> Either Text TypedProgram
resolve (Program fns) = TypedProgram <$> mapM addExplicitTypes fns 

addExplicitTypes :: Function -> Either Text TypedFunction
addExplicitTypes  Function{..} = do
  let (returnStatement, mlist) = NE.uncons fnbody
  returnStatementType <- assignType returnStatement
  typedFnArgs         <- mapM assignType fnArgs
  fbody               <- fnBody mlist
  return TypedFunction {
            retStatement = returnStatementType
            , typedFnName = fnName
            , tyfnArgTypes = fnArgTypes
            , tyRetType = retType
            , tyFnArgs = typedFnArgs
            , tyFnbody = fbody
         }

fnBody :: Maybe (NonEmpty Expr) -> Either Text [TypedExpr]
fnBody (Just a) = mapM assignType $ NE.toList a
fnBody Nothing  = return []

assignType :: Expr -> Either Text TypedExpr
assignType ex@(Assignment expr1 expr2 ) = return (typeOf expr2, ex)
assignType ex@(Call _ []) = error "typed expression not finished"

typeOf :: Expr -> Type
typeOf = error "typeof"
