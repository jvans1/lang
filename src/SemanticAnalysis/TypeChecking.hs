{-# LANGUAGE RecordWildCards #-}

module SemanticAnalysis.TypeChecking where
import SemanticAnalysis.TypeResolution
import Types
import Base

check :: TypedProgram -> Either Text TypedProgram
check fns = error "check" -- typeCheck fns



{- typeCheck :: HashMap Text TypedFunction -> Either Text (HashMap Text TypedFunction) -}
{- typeCheck hm = forM hm $ \fn@(TypedFunction {..})-> -}
                  {- if typeOf retStatement /= tyRetType then -}
                    {- Left $ "Function declared return type of " ++ tshow tyRetType ++ " but actually returned typed"  ++ tshow (typeOf retStatement) -}
                  {- else -}
                    {- return fn -}
                
{- typeOf :: TypedExpr -> Type -}
{- typeOf = fst -}
