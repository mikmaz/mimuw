module Interpreter.Util where

    import qualified Data.Map as Map

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Grammar.AbsCPC

    import Interpreter.Env

    storeIdent :: Ident -> CPCVal -> Eval Env
    storeIdent (Ident ident) cpcVal = do
        env <- ask
        (store, freeLoc) <- get
        put (Map.insert freeLoc cpcVal store, freeLoc + 1)
        asks (Map.insert ident freeLoc)

    updateIdentVal :: Ident -> CPCVal -> Eval ()
    updateIdentVal (Ident ident) newCpcVal = do
        env <- ask
        let Just identLoc = Map.lookup ident env
        (store, freeLoc) <- get
        put (Map.insert identLoc newCpcVal store, freeLoc)
        return ()

    tupleValsToStr :: [CPCVal] -> String
    tupleValsToStr [] = ""
    tupleValsToStr (v:vs) = 
        if null vs then
            cpcValToStr v
        else
            cpcValToStr v ++ ", " ++ tupleValsToStr vs

    cpcValToStr :: CPCVal -> String
    cpcValToStr (CPCInt i) = show i
    cpcValToStr (CPCStr str) = str
    cpcValToStr (CPCBool b) = if b then "true" else "false"
    cpcValToStr CPCVoid = "void"
    cpcValToStr (CPCTuple tupleVals) = "<" ++ tupleValsToStr tupleVals ++ ">"


    defaultTypeVal :: Type -> CPCVal
    defaultTypeVal (Int _) = CPCInt 0
    defaultTypeVal (Str _) = CPCStr ""
    defaultTypeVal (Bool _) = CPCBool False
    defaultTypeVal (Const _ constType) = defaultTypeVal constType
    defaultTypeVal (TupleT _ tupleTypes) = CPCTuple (map defaultTypeVal tupleTypes)

    readIdentVal :: Ident -> Eval CPCVal
    readIdentVal (Ident ident) = do
        env <- ask
        let Just identLoc = Map.lookup ident env
        (store, freeLoc) <- get
        let Just val = Map.lookup identLoc store
        return val
