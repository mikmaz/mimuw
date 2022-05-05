module TypeChecker.TypeCheck where

    import Data.Maybe

    import Control.Monad.Reader
    import Control.Monad.Except

    import Grammar.AbsCPC

    import TypeChecker.TCEnv
    import TypeChecker.Util
    import TypeChecker.Exception


    checkFunsArgsMatch :: BNFC'Position -> [Type] -> [Expr] -> TC ()
    checkFunsArgsMatch _ [] [] = return ()
    checkFunsArgsMatch pos (argType:argTypes) (expr:exprs) = do
        exprType <- typeCheckExpr expr
        ensureTypesMatch exprType argType
        checkFunsArgsMatch pos argTypes exprs
    checkFunsArgsMatch pos _ _ = throwError (pos, FunArgsCountMismatch)


    typeCheckExpr :: Expr -> TC Type
    typeCheckExpr (EVar pos varName) = getIdentType pos varName

    typeCheckExpr (ELitInt pos _) = return (Int pos)

    typeCheckExpr (ELitTrue pos) = return (Bool pos)

    typeCheckExpr (ELitFalse pos) = return (Bool pos)

    typeCheckExpr (EApp pos (Ident fname) argExprs) = do
                funType <- getIdentType pos (Ident fname)
                case funType of
                    (Fun _ fretType argTypes) -> do
                        checkFunsArgsMatch pos argTypes argExprs
                        return (setTypePos fretType pos)
                    _ -> throwError (pos, IdentAppExc fname funType)

    typeCheckExpr (EString pos _) = return (Str pos)

    typeCheckExpr (Neg pos expr) = do
        exprType <- typeCheckExpr expr
        ensureTypesMatch exprType (Int Nothing)
        return (Int pos)

    typeCheckExpr (Not pos expr) = do
        exprType <- typeCheckExpr expr
        ensureTypesMatch exprType (Bool Nothing)
        return (Bool pos)

    typeCheckExpr (EMul pos e1 _ e2) = do
        e1Type <- typeCheckExpr e1
        e2type <- typeCheckExpr e2
        ensureTypesMatch e1Type (Int Nothing)
        ensureTypesMatch e2type (Int Nothing)
        return (Int pos)
    
    typeCheckExpr (EAdd pos e1 _ e2) = do
        e1Type <- typeCheckExpr e1
        e2type <- typeCheckExpr e2
        ensureTypesMatch e1Type (Int Nothing)
        ensureTypesMatch e2type (Int Nothing)
        return (Int pos)
    
    typeCheckExpr (ERel pos e1 _ e2) = do
        e1Type <- typeCheckExpr e1
        e2type <- typeCheckExpr e2
        ensureTypesMatch e1Type (Int Nothing)
        ensureTypesMatch e2type (Int Nothing)
        return (Bool pos)

    typeCheckExpr (EAnd pos e1 e2) = do
        e1Type <- typeCheckExpr e1
        e2type <- typeCheckExpr e2
        ensureTypesMatch e1Type (Bool Nothing)
        ensureTypesMatch e2type (Bool Nothing)
        return (Bool pos)
    
    typeCheckExpr (EOr pos e1 e2) = do
        e1Type <- typeCheckExpr e1
        e2type <- typeCheckExpr e2
        ensureTypesMatch e1Type (Bool Nothing)
        ensureTypesMatch e2type (Bool Nothing)
        return (Bool pos)

    typeCheckExpr (ELitTuple pos exprs) = do
        ts <- getLitTupleType exprs
        return (TupleT pos ts)

    getLitTupleType :: [Expr] -> TC [Type]
    getLitTupleType [] = return []
    getLitTupleType (expr:exprs) = do
        ts <- getLitTupleType exprs
        exprType <- typeCheckExpr expr
        return (exprType:ts)


    insertDeclVarsType :: Type -> [Item] -> TC TCEnv
    insertDeclVarsType _ [] = ask
    insertDeclVarsType varsType (item:items) = do
        (env, scope, retType, ret) <- insertDeclVarsType varsType items
        case item of
            (NoInit _ ident) -> local (const (env, scope, retType, ret)) (putScopeType ident (scope, varsType))
            (Init _ ident expr) -> do
                exprType <- typeCheckExpr expr
                ensureTypesMatch exprType varsType
                local (const (env, scope, retType, ret)) (putScopeType ident (scope, varsType))


    typeCheckStmt :: Stmt -> TC TCEnv
    typeCheckStmt (Empty _) = ask

    typeCheckStmt (BStmt _ (Block _ stmts)) = do
        (env, scope, retType, ret) <- ask
        (_, _, _, ret') <- local (const (env, scope + 1, retType, ret)) (typeCheckStmts stmts)
        return (env, scope, retType, ret')

    typeCheckStmt (Decl _ varsType vars) = do
        checkValidVarType varsType
        insertDeclVarsType varsType vars
 
    typeCheckStmt (Ass pos (Ident var) expr) = do
        varType <- getIdentType pos (Ident var)
        case varType of
            (Const _ _) -> throwError (pos, ConstModification var)
            _ -> do 
                exprType <- typeCheckExpr expr
                ensureTypesMatch exprType varType
                ask

    typeCheckStmt (Incr pos (Ident var)) = do
        varType <- getIdentType pos (Ident var)
        case varType of
            (Const _ _) -> throwError (pos, ConstModification var)
            _ -> do
                ensureTypesMatch varType (Int Nothing)
                ask
    
    typeCheckStmt (Decr pos (Ident var)) = do
        varType <- getIdentType pos (Ident var)
        case varType of
            (Const _ _) -> throwError (pos, ConstModification var)
            _ -> do
                ensureTypesMatch varType (Int Nothing)
                ask

    typeCheckStmt (Ret pos expr) = do
        (env, scope, maybeRetType, _) <- ask
        case maybeRetType of
            Nothing -> throwError (pos, NonVoidRetInVoidFun)
            (Just retType) -> do
                exprType <- typeCheckExpr expr
                ensureTypesMatch exprType retType
                return (env, scope, Just retType, True)
    
    typeCheckStmt (VRet pos) = do
        (env, scope, maybeRetType, ret) <- ask
        case maybeRetType of
            (Just _) -> throwError (pos, VoidRetInNonVoidFun)
            Nothing -> ask
       
    typeCheckStmt (Cond _ expr (Block _ stmts)) = do
        exprType <- typeCheckExpr expr
        ensureTypesMatch exprType (Bool Nothing)
        (env, scope, retType, ret) <- ask
        _ <- local (const (env, scope + 1, retType, ret)) (typeCheckStmts stmts)
        ask
 
    typeCheckStmt (CondElse _ expr (Block _ ifStmts) (Block _ elseStmts)) = do
        exprType <- typeCheckExpr expr
        ensureTypesMatch exprType (Bool Nothing)
        (env, scope, retType, ret) <- ask
        (_, _, _, ifRet) <- local (const (env, scope + 1, retType, ret)) (typeCheckStmts ifStmts)
        (_, _, _, elseRet) <- local (const (env, scope + 1, retType, ret)) (typeCheckStmts elseStmts)
        case (ifRet, elseRet) of
            (True, True) -> return (env, scope, retType, True)
            _ -> return (env, scope, retType, ret)
    
    typeCheckStmt (While _ expr (Block _ stmts)) = do
        exprType <- typeCheckExpr expr
        ensureTypesMatch exprType (Bool Nothing)
        (env, scope, retType, ret) <- ask
        _ <- local (const (env, scope + 1, retType, ret)) (typeCheckStmts stmts)
        ask

    typeCheckStmt (SExp _ expr) = do
        typeCheckExpr expr
        ask

    typeCheckStmt (FnDef pos funRetType (Ident ident) args (Block _ stmts)) = do
        checkBadConstType funRetType
        argTypes <- mapM (\(Arg _ argType ident) -> return argType) args
        (env, scope, retType, ret) <- ask
        (envWithArgs, _, _, _) <- local (const (env, scope + 1, retType, ret)) (insertArgTypes args)
        let tcEnvWithArgs = (envWithArgs, scope, retType, ret)
            funType = Fun pos funRetType argTypes
        (updatedEnv, _, _, _) <- local (const tcEnvWithArgs) (putScopeType (Ident ident) (scope, funType))
        let retType' = if funRetType === Void Nothing then Nothing else Just funRetType
        (_, _, _, ret) <- local (const (updatedEnv, scope + 1, retType', False)) (typeCheckStmts stmts)
        if (funRetType === Void Nothing) || ret then
            putScopeType (Ident ident) (scope, Fun pos funRetType argTypes)
        else
            throwError (pos, NoReturn ident)
    
    typeCheckStmt (Print _ []) = ask
    
    typeCheckStmt (Print pos (expr:exprs)) = do
        typeCheckExpr expr
        typeCheckStmt (Print pos exprs)
    
    typeCheckStmt (For pos (Ident varName) startValExpr endValExpr (Block _ stmts)) = do
        startValType <- typeCheckExpr startValExpr
        ensureTypesMatch startValType (Int Nothing)
        endValType <- typeCheckExpr endValExpr
        ensureTypesMatch endValType (Int Nothing)
        (env, scope, retType, ret) <- ask
        (envWIter, _, _, _) <- putScopeType (Ident varName) (scope + 1, Const pos (Int pos))
        _ <- local (const (envWIter, scope + 1, retType, ret)) (typeCheckStmts stmts)
        return (env, scope, retType, ret)


    typeCheckStmt (AssTuple pos tElems expr) = do
        rightTupleType <- typeCheckExpr expr
        case unwrapConst rightTupleType of
            TupleT _ tts -> typeCheckTupleAss pos tElems tts
            _ -> throwError (pos, SimpleTypeToTupleMismatch rightTupleType)
        ask

    typeCheckTupleAss :: BNFC'Position -> [Tuple] -> [Type] -> TC ()
    typeCheckTupleAss _ [] [] = return ()
    typeCheckTupleAss pos (tuple:tuples) (t:types) = case tuple of
        Tuple nestedPos nestedTuple -> case unwrapConst t of
            TupleT _ nestedTypes -> do
                typeCheckTupleAss nestedPos nestedTuple nestedTypes
                typeCheckTupleAss pos tuples types
            _ -> throwError (hasPosition t, SimpleTypeToTupleMismatch t)
        TupleElem elemPos (Ident ident) -> do
            identType <- getIdentType elemPos (Ident ident)
            case identType of
                Const _ _ -> throwError (elemPos, ConstModification ident)
                _ -> do
                    ensureTypesMatch (setTypePos identType elemPos) t
                    typeCheckTupleAss pos tuples types
    typeCheckTupleAss pos _ _ = throwError (pos, TupleIdentCountMismatch)


    typeCheckStmts :: [Stmt] -> TC TCEnv
    typeCheckStmts [] = ask
    typeCheckStmts (stmt:stmts) = do
        (env, scope, retType, ret) <- ask
        (updatedEnv, _, _, newRet) <- typeCheckStmt stmt
        let ret' = if newRet then newRet else ret
        local (const (updatedEnv, scope, retType, ret')) (typeCheckStmts stmts)


    typeCheckTopDef :: TopDef -> TC TCEnv
    typeCheckTopDef (TopFnDef pos funRetType (Ident ident) args (Block _ stmts)) = do
        checkBadConstType funRetType
        argTypes <- mapM (\(Arg _ argType _) -> return argType) args
        (env, scope, retType, ret) <- ask
        (envWithArgs, _, _, _) <- local (const (env, scope + 1, retType, ret)) (insertArgTypes args)
        let tcEnvWithArgs = (envWithArgs, scope, retType, ret)
            funType = Fun pos funRetType argTypes
        (updatedEnv, _, _, _) <- local (const tcEnvWithArgs) (putScopeType (Ident ident) (scope, funType))
        let retType' = if funRetType === Void Nothing then Nothing else Just funRetType
        (_, _, _, ret) <- local (const (updatedEnv, scope + 1, retType', False)) (typeCheckStmts stmts)
        if (funRetType === Void Nothing) || ret then
            putScopeType (Ident ident) (scope, Fun pos funRetType argTypes)
        else
            throwError (pos, NoReturn ident)

    checkMainDecl :: TopDef -> Bool -> TC Bool
    checkMainDecl (TopFnDef pos _ (Ident "main") _ _) True = 
        throwError (pos, MultipleMainDeclarations)
    checkMainDecl (TopFnDef _ (Int _) (Ident "main") [] _) False =
        return True
    checkMainDecl (TopFnDef pos _ (Ident "main") _ _) False = 
        throwError (pos, InvalidMainDeclaration)
    checkMainDecl _ mainDeclared = return mainDeclared


    typeCheckProgram :: [TopDef] -> Bool -> TC ()
    typeCheckProgram (td:tds) mainDeclared = do
        mainDeclared <- checkMainDecl td mainDeclared
        tcEnv <- typeCheckTopDef td
        local (const tcEnv) (typeCheckProgram tds mainDeclared)
    typeCheckProgram [] True = return ()
    typeCheckProgram [] False = throwError (Nothing, MainNotDeclared)

    runTypeChecker :: [TopDef] ->  IO (Either PosTCExc ())
    runTypeChecker program = runExceptT (runReaderT (typeCheckProgram program False) initTCEnv)
