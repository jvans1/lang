{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(resolve) where
import Data.Foldable(foldrM)
import qualified Data.List.NonEmpty as NE
import Types
import Data.HashMap.Strict(mapWithKey)
import Base hiding (mapWithKey)

resolve :: [Expr] -> Either Text TypedProgram
resolve fns = foldrM addExplicitTypes empty fns

addExplicitTypes :: Expr -> TypedProgram -> Either Text TypedProgram
addExplicitTypes  Function{..} prgrm = do
  let (returnStatement, mlist) = NE.uncons fnbody
  returnStatementType <- assignType returnStatement
  typedFnArgs         <- mapM assignType fnArgs
  fbody               <- fnBody mlist
  return $ insert fnName TypedFunction {
            retStatement = returnStatementType
            , typedFnName = fnName
            , tyfnArgTypes = fnArgTypes
            , tyRetType = retType
            , tyFnArgs = typedFnArgs
            , tyFnbody = fbody
         } prgrm
addExplicitTypes  _ _ = error "invalid rep"

fnBody :: Maybe (NonEmpty Expr) -> Either Text [TypedExpr]
fnBody (Just a) = mapM assignType $ NE.toList a
fnBody Nothing  = return []

assignType :: Expr -> Either Text TypedExpr
assignType ex@(Assignment expr1 expr2 ) = return (typeOf expr2, ex)
assignType ex@(Call _ []) = error "typed expression not finished"

typeOf :: Expr -> Type
typeOf = error "typeof"
