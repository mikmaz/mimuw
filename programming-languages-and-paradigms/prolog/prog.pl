/* Author: Mikolaj Mazurczyk */

:- ensure_loaded(library(lists)).

/**
 * repeat(+N, +Val, -RepList)
 * 
 * Gets list of 'Val' values repeated 'N' times.
 */
repeat(0, _, []).
repeat(N, Val, [Val | Vals]) :- M is N - 1, repeat(M, Val, Vals).

/**
 * bindVars(+VarNames, +InitVal, -BoundedVars)
 * 
 * Gets list of variables from 'VarNames' list bounded to 'InitVal' value.
 * 
 * @param VarNames    List of terms describing variable names.
 * @param InitVal     Value (list or integer) to wich variables should be 
 *                    bound.
 * @param BoundedVars List of bounded variables in the form of 
 *                    '[binding(VarName, InitVal), ... ]'.
 */
bindVars([], _, []).
bindVars([VName | VNames], Val, [binding(VName, Val) | InitializedVars]) :-
   bindVars(VNames, Val, InitializedVars).

/**
 * initState(+Declarations, +N, -InitailState)
 * 
 * Gets initial state of the program.
 *
 * @param Declarations Two lists of variables' and arrays' names in form of 
 *                     'decls(VarNames, ArrNames)'.
 * @param N            Number of processes executing program.
 * @param InitailState Resulting state in form of 
 *                     'state(Variables, Arrays, Positions)', where:
 *                     - 'Variables' is a list of variables bounded to their
 *                       default values.
 *                     - 'Arrays' is a list of arrays bounded to their
 *                       containers represented as lists of 'N' size.
 *                     - 'Positions' is a list of 'N' size, indexed by process'
 *                       Pid describing its current position in the program.
 */
initState(Declarations, N, InitailState) :-
  decls(VarNames, ArrNames) = Declarations,
  bindVars(VarNames, 0, InitializedVars),
   repeat(N, 0, ZeroedArr),
   bindVars(ArrNames, ZeroedArr, InitializedArrs),
   repeat(N, 1, InitPositions),
   InitailState = state(InitializedVars, InitializedArrs, InitPositions).

/**
 * currentProcInstr(+Instructions, +State, +Pid, -CurrentInstr) 
 * 
 * Gets process' current instruction.
 * 
 * @param Instructions List of program's instructions.
 * @param State        Current program's state.
 * @param Pid          Process' id.
 * @param CurrentInstr Current instruction of process specified by 'Pid'.
 */
currentProcInstr(Instructions, State, Pid, CurrentInstr) :-
  state(_, _, Positions) = State,
  nth0(Pid, Positions, CurrentPos),
  nth1(CurrentPos, Instructions, CurrentInstr).

:- op(700, xfx, <>).

/**
 * convertOp(+ProgramOp, -PrologOp)
 * 
 * Gets Prolog's operator equivalent to program's operator.
 * 
 * @param ProgramOp Program's operator.
 * @param PrologOp  Prolog's equivalent.
 */
convertOp(+, +).
convertOp(-, -).
convertOp(*, *).
convertOp(/, div).
convertOp(<, <).
convertOp(=, =:=).
convertOp(<>, =\=).

/**
 * evalExpr(+Expr, +Pid, +State, -ExprVal)
 * 
 * Gets result of expression evaluated by process.
 * 
 * @param Expr    Expression to be evaluated.
 * @param Pid     Process' id.
 * @param State   Current program's state.
 * @param ExprVal Value of evaluated expression.
 */
evalExpr(Expr, _, _, Ret) :-
  integer(Expr),
  Ret is Expr.
evalExpr(pid, Pid, _, Ret) :-
  Ret is Pid.
evalExpr(VName, _, State, Ret) :-
  atom(VName),
  state(Vars, _, _) = State,
  member(binding(VName, VVal), Vars),
  Ret is VVal.
evalExpr(array(ArrName, IdxExpr), Pid, State, Ret) :-
  state(_, Arrays, _) = State,
  evalExpr(IdxExpr, Pid, State, Idx),
  member(binding(ArrName, Array), Arrays),
  nth0(Idx, Array, ArrElem),
  Ret is ArrElem.
evalExpr(Expr, Pid, State, Ret) :-
  Expr =.. [Op, LExpr, RExpr],
  evalExpr(LExpr, Pid, State, LRet),
  evalExpr(RExpr, Pid, State, RRet),
  convertOp(Op, ConvertedOp),
  ConvertedExpr =.. [ConvertedOp, LRet, RRet],
  Ret is ConvertedExpr.

/**
 * evalBExpr(+BExpr, +Pid, +State)
 * 
 * Succeeds if result of expression evaluated by process equals true.
 */
evalBExpr(BExpr, Pid, State) :-
  BExpr =.. [Op, LExpr, RExpr],
  evalExpr(LExpr, Pid, State, LRet),
  evalExpr(RExpr, Pid, State, RRet),
  convertOp(Op, ConvertedOp),
  ConvertedExpr =.. [ConvertedOp, LRet, RRet],
  ConvertedExpr.

updateVarVal(_, _, [], []).
updateVarVal(VName, NewVal, [binding(VName, _) | Vars], 
             [binding(VName, NewVal) | Vars]).
updateVarVal(VName, NewVal, [Var | Vars], [Var| UpdatedVars]) :-
  binding(OtherVName, _) = Var,
  VName \== OtherVName,
  updateVarVal(VName, NewVal, Vars, UpdatedVars).

updateArr(_, _, _, [], []).
updateArr(ArrName, Idx, NewVal, [binding(ArrName, OldArr) | Arrs],
          [binding(ArrName, NewArr) | Arrs]) :-
  updateAtIdx(Idx, NewVal, OldArr, NewArr).
updateArr(ArrName, Idx, NewVal, [Arr | Arrs], [Arr | UpdatedArrs]) :-
  binding(OtherArrName, _) = Arr,
  ArrName \== OtherArrName,
  updateArr(ArrName, Idx, NewVal, Arrs, UpdatedArrs).

updateAtIdx(0, NewVal, [_ | LS], [NewVal | LS]).
updateAtIdx(Idx, NewVal, [L | LS], [L | UpdatedLS]) :- 
  DecrIdx is Idx - 1,
  updateAtIdx(DecrIdx, NewVal, LS, UpdatedLS).

/**
 * evalInstr(+Instruction, +InState, +Pid, -OutState)
 * 
 * Returns state being the result of expression's evaluation by process.
 */
evalInstr(assign(VName, Expr), InState, Pid, OutState) :-
  atom(VName),
  state(Vars, Arrays, Positions) = InState,
  evalExpr(Expr, Pid, InState, NewVal),
  updateVarVal(VName, NewVal, Vars, UpdatedVars),
  nth0(Pid, Positions, CurrentPos),
  UpdatedPos is CurrentPos + 1,
  updateAtIdx(Pid, UpdatedPos, Positions, UpdatedPositions),
  OutState = state(UpdatedVars, Arrays, UpdatedPositions).
evalInstr(assign(array(ArrName, IdxExpr), Expr), InState, Pid, OutState) :-
  state(Vars, Arrays, Positions) = InState,
  evalExpr(IdxExpr, Pid, InState, Idx),
  evalExpr(Expr, Pid, InState, NewVal),
  updateArr(ArrName, Idx, NewVal, Arrays, UpdatedArrs),
  nth0(Pid, Positions, CurrentPos),
  UpdatedPos is CurrentPos + 1,
  updateAtIdx(Pid, UpdatedPos, Positions, UpdatedPositions),
  OutState = state(Vars, UpdatedArrs, UpdatedPositions).
evalInstr(goto(JumpPos), InState, Pid, OutState) :-
  state(Vars, Arrays, Positions) = InState,
  updateAtIdx(Pid, JumpPos, Positions, UpdatedPositions),
  OutState = state(Vars, Arrays, UpdatedPositions).
evalInstr(condGoto(BExpr, JumpPos), InState, Pid, OutState) :-
  (evalBExpr(BExpr, Pid, InState) -> 
    evalInstr(goto(JumpPos), InState, Pid, OutState)
  ;
    (state(Vars, Arrays, Positions) = InState,
    nth0(Pid, Positions, CurrentPos),
    UpdatedPos is CurrentPos + 1,
    updateAtIdx(Pid, UpdatedPos, Positions, UpdatedPositions),
    OutState = state(Vars, Arrays, UpdatedPositions))
  ).
evalInstr(sekcja, InState, Pid, OutState) :-
  state(Vars, Arrays, Positions) = InState,
  nth0(Pid, Positions, CurrentPos),
  UpdatedPos is CurrentPos + 1,
  updateAtIdx(Pid, UpdatedPos, Positions, UpdatedPositions),
  OutState = state(Vars, Arrays, UpdatedPositions).

step(Instructions, InState, Pid, OutState) :-
  currentProcInstr(Instructions, InState, Pid, CurrentInstr),
  evalInstr(CurrentInstr, InState, Pid, OutState).

/**
 * procInSection(+Instructions, +Positions, +ProcN, -InSection)
 * 
 * Gets list of pids of processes that are currently in the critical section.
 * 
 * @param Instructions List of program's instructions.
 * @param Positions    List of current processes' positions.
 * @param ProcN        Pid of process which position is the firs element of
 *                     Positions list.
 * @param InSection    List of processes that are currently in critical 
 *                     section.
 */
procInSection(_, [], _, []).
procInSection(Instructions, [Pos | Positions], ProcN, InSection) :-
  NextProcN is ProcN + 1,
  procInSection(Instructions, Positions, NextProcN, InSectionTail),
  (nth1(Pos, Instructions, sekcja)->
    InSection = [ProcN | InSectionTail]
  ; 
    InSection = InSectionTail
  ).

safeState(Instructions, State) :-
  state(_, _, Positions) = State,
  procInSection(Instructions, Positions, 0, InSection),
  length(InSection, Len),
  Len < 2.

/**
 * procPos(+State, +Pid, -ProcPos)
 * 
 * Gets current process' position.
 */
procPos(State, Pid, ProcPos) :-
  state(_, _, Positions) = State,
  nth0(Pid, Positions, ProcPos).

/**
 * dfsStates(+ProgInfo, +GraphInfo, +State, +Pid, -Ret)
 * 
 * Performs DFS algorithm on program's states, where states are graph's nodes
 * and steps of consecutive processes are edges. Gets unsafe state if one
 * exists and path to it, otherwise gets infromation that program is safe.
 * 
 * @param ProgInfo  Information about the program in form of
 *                  progInfo(N, Instructions), where:
 *                  - N is the number of processes executing program,
 *                  - Instructions is a list of program's instructions.
 * @param GraphInfo Informations about the graph used in DFS algorithm in form
 *                  of graphInfo(VisitedStates, Path), where:
 *                  - VisitedStates is list of states already visited by the
 *                    DFS,
 *                  - Path is the current path of DFS algorithm.
 * @param State     Current state (node).
 * @param Pid       PID of process for which the step should be executed
 *                  (current edge).
 * @param Ret       Information whether subgraph is safe in form of
 *                  safe(VisitedStates) or unsafe(Path, UnsafeState), where:
 *                  - VisitedStates is a list of states in the subgraph, used
 *                    to update VisitedStates inside GraphInfo,
 *                  - Path is a path that led to unsafe state.
 *                  - UnsafeState is a state where more than 1 process is in
 *                    critical section.
 */
dfsStates(ProgInfo, GraphInfo, _, Pid, Ret) :-  % can't search deeper,
                                                % finish safe
  progInfo(N, _) = ProgInfo,
  N =:= Pid,
  graphInfo(VisitedStates, _) = GraphInfo,
  Ret = safe(VisitedStates).

dfsStates(ProgInfo, GraphInfo, State, Pid, Ret) :-
  progInfo(N, Instructions) = ProgInfo,
  Pid < N,
  step(Instructions, State, Pid, OutState),
  graphInfo(VisitedStates, CurrentPath) = GraphInfo,
  (member(OutState, VisitedStates) ->                     % state visited
    (IncrPid is Pid + 1,
    dfsStates(ProgInfo, GraphInfo, State, IncrPid, Ret))  % check next edge
  ;
    (procPos(State, Pid, ProcPos),                        
    UpdatedPath = [edge(Pid, ProcPos) | CurrentPath],                                                    
    (safeState(Instructions, OutState) ->                 % new and safe state
      (UpdatedVisited = [OutState | VisitedStates],
      UpdatedGraph = graphInfo(UpdatedVisited, 
                               UpdatedPath),
      dfsStates(ProgInfo, UpdatedGraph,                   % search deeper
        OutState, 0, NewRet), 
      (safe(NewVisited) = NewRet ->                       % deeper search safe
        (IncrPid is Pid + 1,
        NextPidGraph = graphInfo(NewVisited,             
                                 CurrentPath),
        dfsStates(ProgInfo, NextPidGraph,                 % check next edge
          State, IncrPid, Ret))
      ; 
        Ret = NewRet                                      % deeper search not
                                                          % safe, finish
      ))
    ;
      Ret = unsafe(UpdatedPath, OutState)                 % new state unsafe,
                                                           % finish
    ))
  ).

verifyN(N) :-
  (integer(N), N > 0);
  write("Error: Parameter N should be integer greater than 0.\n"), 
  false.

verifyArgsCount(Args, N, FileName) :-
  ([_, N, FileName] = Args);
  write("Error: Wrong number of arguments.\n"),
  false.

convertN(N, ConvertedN) :-
  atom_number(N, ConvertedN);
  write("Error: N is not a number.\n"),
  false.
  
verifyFile(FileName) :-
  Recover = (format("Error: File ~p can't be opened.~n", [FileName]), false),
  catch(open(FileName, read, Stream), _, Recover) -> close(Stream).

verifyFile(FileName, Stream) :-
  Recover = (format("Error: File ~p can't be opened.~n", [FileName]), false),
  catch(open(FileName, read, Stream), _, Recover).

writeListElems([]).
writeListElems([L | []]) :-
  write(L).
writeListElems([L | LS]) :-
  LS \= [],
  format("~p, ", [L]),
  writeListElems(LS).

writeProcInSection(Instructions, UnsafeState) :-
  write("Processes in the critical section: "),
  state(_, _, Positions) = UnsafeState,
  procInSection(Instructions, Positions, 0, ProcInSection),
  writeListElems(ProcInSection),
  write(".\n").

writeSteps([]).
writeSteps([edge(Pid, ProcPos) | Path]) :-
  writeSteps(Path),
  format("   Process ~p: ~p ~n", [Pid, ProcPos]).

verify(N, FileName) :-
  ((verifyN(N), !, verifyFile(FileName, Stream)) -> 
    (read(Stream, variables(Vars)),
    read(Stream, arrays(Arrays)),
    read(Stream, program(Instructions)),
    close(Stream),
    initState(decls(Vars, Arrays), N, InitialState),
    (safeState(Instructions, InitialState) ->
      ProgInfo = progInfo(N, Instructions),
      InitialGraphInfo = graphInfo([], []),
      dfsStates(ProgInfo, InitialGraphInfo, InitialState, 0, Ret),
      (Ret = unsafe(Path, UnsafeState) ->
        (write("Program is unsafe.\n"),
        write("Steps that led to unsafe state:\n"),
        writeSteps(Path),
        writeProcInSection(Instructions, UnsafeState))
      ;  
        write("Program is safe.\n")
      )
    ;
      (write("Program is unsafe.\n"),
      writeProcInSection(Instructions, InitialState))
    ))
  ;
    true
  ).

verify() :-
  (   (current_prolog_flag(argv, Args),
      ((verifyArgsCount(Args, N, FileName), !,
      (convertN(N, IntN) -> verifyN(IntN)), !,
      verifyFile(FileName)) -> 
        verify(IntN, FileName)
      ))
  ;   true
  ).

