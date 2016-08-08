{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module SemanticAnalysis.TypeResolution(parseTop, assignTypes) where
import Control.Monad.State.Lazy(MonadState, State, evalState)
import Debug.Trace(trace)
import qualified Data.Text as T
import Control.Monad.Extra(findM)
import Control.Monad.Reader(ask)
import Data.Foldable(foldrM)
import qualified Data.List.NonEmpty as NE
import Types
import Control.Monad.Writer.Lazy(tell, runWriter, Writer, runWriterT)
import Control.Monad.State.Lazy(evalState)
import Control.Monad.State.Lazy(get)
import Data.HashMap.Strict(mapWithKey)
import Base hiding (mapWithKey)
import Control.Monad.Writer.Lazy(MonadWriter, WriterT, runWriterT)

typeAssignment :: TypeAssignment Program
typeAssignment = get >>= foldrM addExplicitTypes empty

newtype TypeAssignment a = TypeAssignment {
  _runTypeAssignment :: WriterT [TypeError] (State Scope) a
} deriving (Functor, Applicative, Monad, MonadState Scope, MonadWriter [TypeError])

runTypeAssignment :: HashMap Text (Type, Expr) -> (Program, [TypeError])
runTypeAssignment scope = flip evalState scope (runWriterT (_runTypeAssignment typeAssignment))

--TODO:This should always be a function due to the previous step
--but it is still an incomplete function. Should try
--to represent that this is always a function on the type level
--GADT's seem to lead to too much complexity atm

assignTypes :: HashMap Text (Type, Expr) ->  Either [TypeError] Program
assignTypes exprs = case runTypeAssignment exprs of
                          (pgrm, [])  -> return pgrm
                          (_, xs)     -> Left xs

{- case runTypeAssignment exprs of -}
                      {- (typedFn, []) -> -}
{- case evalState (runWriterT $ runTypeAssignment typeAssignment) exprs of -}
                      {- (prgm, []) -> Right prgm -}
                      {- (_, errs)    -> Left errs -}

parseTop :: [Expr] -> Either [TypeError] (HashMap Text (Type, Expr))
parseTop exprs = 
  case runWriter (assignFnTypes exprs) of
    (fncts, []) -> Right fncts
    (_, xs)     -> Left xs

assignFnTypes :: [Expr] -> Writer [TypeError] (HashMap Text (Type, Expr))
assignFnTypes exprs = foldrM addTopLevelDeclarations empty $ reverse exprs 
  where
    addTopLevelDeclarations :: Expr -> HashMap Text (Type, Expr) -> Writer [TypeError] (HashMap Text (Type, Expr))
    addTopLevelDeclarations fn@Function{..} hmap = do
      case lookup fnName hmap of
        Just _ -> tell [DuplicateDeclaration fnName fn] >> return hmap
        Nothing -> return $ insert fnName (retType, fn) hmap
    addTopLevelDeclarations expr hmap = tell [NakedExpression expr] >> return hmap
  

addExplicitTypes :: (Type, Expr) -> Program -> TypeAssignment Program
addExplicitTypes expr prgm = do
  mfn <- addFnType expr
  case mfn of
    Just fn -> return $ insert (typedFnName fn) fn prgm
    Nothing -> return prgm

addFnType :: (Type, Expr) -> TypeAssignment (Maybe TypedFunction)
addFnType (ftype, Function{..}) = error "addFnType"
  {- let (returnStatement, mlist) = NE.uncons fnbody -}
  {- typedFnArgs         <- mapM assignType fnArgs -}
  {- fbody               <- fnBody mlist -}
  {- mrType <-  typeOf returnStatement -}
  {- case mrType of -}
    {- Just rType -> return . Just $ TypedFunction { -}
            {- retStatement = (rType, returnStatement) -}
            {- , typedFnName = fnName -}
            {- , tyfnArgTypes = fnArgTypes -}
            {- , tyRetType = retType -}
            {- , tyFnArgs = typedFnArgs -}
            {- , tyFnbody = fbody -}
         {- } -}
    {- Nothing ->  return Nothing -}
    --TOOO: error message here?
addFnType _ = tell [InvalidEntry] >> return Nothing

fnBody :: Maybe (NonEmpty Expr) -> TypeAssignment [Maybe TypedExpr]
fnBody (Just a) = mapM assignType $ NE.toList a
fnBody Nothing  = return []

assignType :: Expr -> TypeAssignment (Maybe TypedExpr)
assignType ex = do
    mtype <- typeOf ex 
    case mtype of
      Just ttype -> return $ Just (ttype, ex)
      Nothing -> return Nothing

isNamed :: Text -> Expr -> TypeAssignment Bool
isNamed name Function{..} = return $ fnName == name
--TODO: Account for invalid top level declarations here and short circuit
--If we have a TLD that is not a function this will fail 

typeOf :: Expr -> TypeAssignment (Maybe Type)
typeOf ex@(Assignment expr1 expr2 _) = typeOf expr2
typeOf ex@(Function{..})             = return (Just retType)
typeOf ex@(Lit (StringLit _) _)      = return (Just String)
typeOf ex@(Lit (Digit _) _)          = return (Just Integer)
typeOf ex@(Var _ _)                  = return (Just Integer)
typeOf ex@(Call name _ _)            = typeOfFn name ex

typeOfFn :: Text -> Expr -> TypeAssignment (Maybe Type)
typeOfFn name ex = do
  scope <- get
  let mfn = lookup name scope
  case mfn of
    Just (_, fn) -> typeOf fn
    Nothing -> tell [UnknownFunction name (location ex)] >> return Nothing
