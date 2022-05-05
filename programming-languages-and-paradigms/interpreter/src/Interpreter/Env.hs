module Interpreter.Env where

    import Data.Map as Map

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Grammar.AbsCPC

    import Interpreter.Exception
    
    type CPCFunVal = (Type, [Arg], [Stmt], Env)
    type CPCTupleVal = [CPCVal]

    data CPCVal 
        = CPCBool Bool 
        | CPCInt Integer 
        | CPCStr String 
        | CPCVoid
        | CPCConst CPCVal
        | CPCFun CPCFunVal 
        | CPCTuple CPCTupleVal deriving Show

    type RetVal = Maybe CPCVal

    type Loc = Integer

    type StoreMap = Map.Map Loc CPCVal

    type Store = (StoreMap, Loc)

    type VarName = String

    type Env = Map.Map VarName Loc

    type Eval = ReaderT Env (StateT Store (ExceptT PosRuntimeExc IO))

    initEnv :: Env
    initEnv = Map.empty

    initStore :: Store
    initStore = (Map.empty, 0)
