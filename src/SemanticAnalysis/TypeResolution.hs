{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(typeChecker) where
import Control.Monad.Extra(findM)
import Control.Monad.Except(throwError)
import Control.Monad.Reader(ask)
import Data.Foldable(foldrM)
import qualified Data.List.NonEmpty as NE
import Types
import Data.HashMap.Strict(mapWithKey)
import Base hiding (mapWithKey)

typeChecker :: TypeChecker Program
typeChecker = ask >>= foldrM addExplicitTypes empty

addExplicitTypes :: Expr -> Program -> TypeChecker Program
addExplicitTypes expr prgm = do
  fn <- addFnType expr
  return $ insert (typedFnName fn) fn prgm

addFnType :: Expr -> TypeChecker TypedFunction
addFnType Function{..} = do
  let (returnStatement, mlist) = NE.uncons fnbody
  returnStatementType <- assignType returnStatement
  typedFnArgs         <- mapM assignType fnArgs
  fbody               <- fnBody mlist
  return $ TypedFunction {
            retStatement = returnStatementType
            , typedFnName = fnName
            , tyfnArgTypes = fnArgTypes
            , tyRetType = retType
            , tyFnArgs = typedFnArgs
            , tyFnbody = fbody
         }
addFnType a = throwError $ NakedExpression (location a) (source a)

fnBody :: Maybe (NonEmpty Expr) -> TypeChecker [TypedExpr]
fnBody (Just a) = mapM assignType $ NE.toList a
fnBody Nothing  = return []

assignType :: Expr -> TypeChecker TypedExpr
assignType ex = typeOf ex >>= return . (flip (,) ex)


isNamed :: Text -> Expr -> TypeChecker Bool
isNamed name Function{..} = return $ fnName == name
--TODO: Account for invalid top level declarations here and short circuit
--If we have a TLD that is not a function this will fail 

typeOf :: Expr -> TypeChecker Type
typeOf ex@(Assignment expr1 expr2 _) = typeOf expr2
typeOf ex@(Function{..})             = return retType
typeOf ex@(Lit (StringLit _) _)      = return String
typeOf ex@(Lit (Digit _) _)          = return Integer
typeOf ex@(Var _ _)                  = return Integer
typeOf ex@(Call name _ _)            = typeOfFn name ex

typeOfFn :: Text -> Expr -> TypeChecker Type
typeOfFn name ex = do
  xs <- ask
  mfn <- findM (isNamed name) xs
  case mfn of
    Just fn -> typeOf fn
    Nothing -> throwError $ UnknownFunction name (location ex)

