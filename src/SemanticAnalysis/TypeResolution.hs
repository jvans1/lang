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
assignType ex@(Assignment expr1 expr2 _) = do
  ttype <- typeOf expr2
  return (ttype, ex)
assignType ex@(Function{..})             = return (retType, ex)
assignType ex@(Lit (StringLit _) _)      = return (String, ex)
assignType ex@(Lit (Digit _) _)          = return (Integer, ex)
assignType ex@(Var _ _)                  = return (Integer, ex)
assignType ex@(Call name _ _)            = do 
  ttype <- typeOfFn name ex
  return (ttype, ex)

typeOfFn :: Text -> Expr -> TypeChecker Type
typeOfFn name ex = do
  xs <- ask
  mfn <- findM (isNamed name) xs
  case mfn of
    Just fn -> typeOf fn
    Nothing -> throwError $ UnknownFunction name (location ex)


isNamed :: Text -> Expr -> TypeChecker Bool
isNamed name Function{..} = return $ fnName == name
--TODO: I don't like this because we don't actually need to add the fn type
--but we want to reuse the pattern matching that short circuits when a
--top level declation is not a function


typeOf :: Expr -> TypeChecker Type
typeOf ex@(Assignment expr1 expr2 _) = typeOf expr2
typeOf ex@(Function{..})             = return retType
typeOf ex@(Lit (StringLit _) _)      = return String
typeOf ex@(Lit (Digit _) _)          = return Integer
typeOf ex@(Var _ _)                  = return Integer
typeOf ex@(Call name _ _)            = typeOfFn name ex
