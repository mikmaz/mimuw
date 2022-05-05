module TypeChecker.Exception where

    import Grammar.AbsCPC

    type PosTCExc = (BNFC'Position, TCException)

    typeToStr :: Type -> String
    typeToStr (Int _) = "int"
    typeToStr (Str _) = "string"
    typeToStr (Bool _) = "boolean"
    typeToStr (Void _) = "void"
    typeToStr (Const _ t) = "const " ++ typeToStr t
    typeToStr (TupleT _ tts) = "tuple <" ++ tupleTypeToStr tts ++ ">"
    
    tupleTypeToStr :: [Type] -> String
    tupleTypeToStr [] = ""
    tupleTypeToStr (v:vs) = 
        if null vs then
            typeToStr v
        else
            typeToStr v ++ ", " ++ tupleTypeToStr vs

    putApostrophes :: String -> String
    putApostrophes s = "'" ++ s ++ "'"

    showType :: Type -> String
    showType t = putApostrophes $ typeToStr t

    data TCException
        = TypeMismatch Type Type
        | UndefinedIdent String 
        | MultipleMainDeclarations
        | InvalidMainDeclaration
        | MainNotDeclared 
        | InvalidDeclarationType Type 
        | FunArgsCountMismatch
        | IdentAppExc String Type
        | IdentRedeclaration String
        | NoReturn String
        | ConstModification String
        | NonVoidRetInVoidFun
        | VoidRetInNonVoidFun 
        | VoidInNonFunDecl
        | SimpleTypeToTupleMismatch Type 
        | TupleIdentCountMismatch deriving Show


    showTCExc :: TCException -> String
    showTCExc e = case e of
        TypeMismatch t1 t2 -> "mismatch of types " ++ showType t1 ++ " and " ++ showType t2
        UndefinedIdent i -> putApostrophes i ++ " is undefined"
        MultipleMainDeclarations -> "multiple 'main' declarations"
        InvalidMainDeclaration -> "invalid 'main' declaration"
        MainNotDeclared -> "couldn't find 'main' top definition in the file"
        InvalidDeclarationType t -> "invalid declaration type: " ++ showType t
        FunArgsCountMismatch -> "arguments count mismatch in function application"
        IdentAppExc i t -> "identifier " ++ putApostrophes i ++ " of type " ++ showType t ++ " isn't a function"
        IdentRedeclaration i -> "redeclaration of " ++ putApostrophes i
        NoReturn i -> "missing return in " ++ putApostrophes i
        ConstModification i -> "modification of read-only variable " ++ putApostrophes i
        NonVoidRetInVoidFun -> "non-void return in void function"
        VoidRetInNonVoidFun -> "void return in non-void function"
        VoidInNonFunDecl -> "void can only be used as return type in function declaration"
        SimpleTypeToTupleMismatch t -> "can't use type " ++ showType t ++ " as tuple"
        TupleIdentCountMismatch -> "elements count mismatch in tuple assignment"

    getTcErrMsg :: PosTCExc -> String
    getTcErrMsg (pos, tcExc) = case pos of
        Nothing -> "type-check error: " ++ showTCExc tcExc
        Just (l, c) -> "type-check error:" ++ show l ++ ":" ++ show c ++ ": " ++ showTCExc tcExc
