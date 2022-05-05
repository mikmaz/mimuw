module TypeChecker.Util where

    import Data.Map as Map

    import Control.Monad.Reader
    import Control.Monad.Except

    import Grammar.AbsCPC

    import TypeChecker.TCEnv
    import TypeChecker.Exception

    (===) :: Type -> Type -> Bool
    Int _ === Int _ = True
    Str _ === Str _ = True
    Bool _ === Bool _ = True
    Void _ === Void _ = True
    Const _ t === Const _ t' = t === t'
    Const _ t === t' = t === t'
    t === Const _ t' = t === t'
    TupleT _ ts === TupleT _ ts' = checkTupleTypes ts ts'
    _ === _ = False

    checkTupleTypes :: [Type] -> [Type] -> Bool
    checkTupleTypes [] [] = True
    checkTupleTypes (t1:ts1) (t2:ts2) = (t1 === t2) && checkTupleTypes ts1 ts2
    checkTupleTypes _ _ = False

    insertArgTypes :: [Arg] -> TC TCEnv
    insertArgTypes [] = ask
    insertArgTypes (Arg pos argType (Ident ident):args) = do
        checkValidVarType argType
        (env, scope, retType, ret) <- insertArgTypes args
        let updatedEnv = Map.insert ident (scope, argType) env
        return (updatedEnv, scope, retType, ret) 

    putScopeType :: Ident -> ScopeType -> TC TCEnv
    putScopeType (Ident ident) (identScope, identType) = do
        (env, scope, retType, ret) <- ask
        case Map.lookup ident env of
            Nothing -> do
                let updatedEnv = Map.insert ident (identScope, identType) env
                return (updatedEnv, scope, retType, ret) 
            Just (otherScope, otherType) ->
                if otherScope == identScope then
                    throwError (hasPosition identType, IdentRedeclaration ident)
                else do
                    let updatedEnv = Map.insert ident (identScope, identType) env
                    return (updatedEnv, scope, retType, ret)

    unwrapConst :: Type -> Type
    unwrapConst (Const _ t) = t
    unwrapConst t = t

    ensureTypesMatch :: Type -> Type -> TC ()
    ensureTypesMatch t1 t2 = unless (t1 === t2) (throwError (hasPosition t1, TypeMismatch t1 t2))

    isBasicType :: Type -> Bool
    isBasicType (Int _) = True
    isBasicType (Str _) = True
    isBasicType (Bool _) = True
    isBasicType (TupleT _ _) = True
    isBasicType _ = False
    
    constCheckTupleTypes :: [Type] -> TC ()
    constCheckTupleTypes [] = return ()
    constCheckTupleTypes (t:ts) = do
        checkValidVarType t
        constCheckTupleTypes ts

    checkBadConstType :: Type -> TC ()
    checkBadConstType t = case t of
        Const _ t' -> case t' of
            TupleT _ tupleTypes -> constCheckTupleTypes tupleTypes
            _ ->  unless (isBasicType t') (throwError (hasPosition t, InvalidDeclarationType t))
        TupleT _ tupleTypes -> constCheckTupleTypes tupleTypes
        _ -> return ()

    checkValidVarType :: Type -> TC ()
    checkValidVarType varType = 
        if varType === Void Nothing then
            throwError (hasPosition varType, VoidInNonFunDecl)
        else
            checkBadConstType varType

    getIdentType :: BNFC'Position -> Ident -> TC Type
    getIdentType pos (Ident ident) = do
        (env, _, _, _) <- ask
        case Map.lookup ident env of
            Nothing -> throwError (pos, UndefinedIdent ident)
            Just (_, identType) -> return identType

    setTypePos :: Type -> BNFC'Position -> Type
    setTypePos (Int _) pos = Int pos
    setTypePos (Str _) pos = Str pos
    setTypePos (Bool _) pos = Bool pos
    setTypePos (Void _) pos = Void pos
    setTypePos (Fun _ retType argTypes) pos = Fun pos retType argTypes
    setTypePos (Const _ t) pos = Const pos t
    setTypePos (TupleT _ ts) pos = TupleT pos ts
