/* program instructions */

countProgInstr(instrHalt(), _) -->
    moveConstToAcc(0),
    [sysSymb].      

countProgInstr(instrRead(Var), InstrInfo) -->  
    moveConstToAcc(1),
    [sysSymb],
    saveAccToVar(Var, InstrInfo).
  
countProgInstr(instrWrite(ArithExpr), InstrInfo) -->  
    countArithInstr(ArithExpr, InstrInfo),
    [swapdSymb],
    moveConstToAcc(2),
    [sysSymb].
  
countProgInstr(instrAssgn(Var, ArithExpr), InstrInfo) -->
    countArithInstr(ArithExpr, InstrInfo),
    saveAccToVar(Var, InstrInfo).
  
countProgInstr(instrCall(Call), InstrInfo) -->
    countCallInstr(Call, InstrInfo).      

countProgInstr(instrIf(BoolExpr, IfInstr), InstrInfo) -->
    countBoolInstr(BoolExpr, InstrInfo),
    [swapaSymb],
    moveConstToAcc(addrStart(AfterLabel)),
    [swapaSymb, branchzSymb],
    countProcInstr(IfInstr, InstrInfo),
    [addrSymb(AfterLabel)].

countProgInstr(instrIfElse(BoolExpr, IfInstr, ElseInstr), InstrInfo) -->
    countBoolInstr(BoolExpr, InstrInfo),
    [swapaSymb],
    moveConstToAcc(addrStart(ElseLabel)),
    [swapaSymb, branchzSymb],
    countProcInstr(IfInstr, InstrInfo),
    moveConstToAcc(addrStart(AfterLabel)),
    [jumpSymb, addrSymb(ElseLabel)],
    countProcInstr(ElseInstr, InstrInfo),  
    [addrSymb(AfterLabel)].   
  
countProgInstr(instrWhile(BoolExpr, Instr), InstrInfo) -->
    [addrSymb(StartWhile)],
    countBoolInstr(BoolExpr, InstrInfo),
    [swapaSymb],
    moveConstToAcc(addrStart(AfterWhile)),
    [swapaSymb, branchzSymb],
    countProcInstr(Instr, InstrInfo),  
    moveConstToAcc(addrStart(StartWhile)),
    [jumpSymb, addrSymb(AfterWhile)].  
  
countProgInstr(instrInit(), ((_, Args, Locals), _)) -->
    {length(Args, ArgsLength)},
    {length(Locals, LocalsLength)},
    {Offset is 3 + ArgsLength + LocalsLength},
    setAddrOnFrame(Offset),
    setStackPointerOnAddr().
  
countProgInstr(instrReturn(ArithExpr), InstrInfo) -->
    countArithInstr(ArithExpr, InstrInfo),
    setAddrOnFrame(),
    [storeSymb],
    setAddrOnFrame(3),
    setStackPointerOnAddr(),
    getAccFromStack(),
    [jumpSymb].          
  
/* procedure instructions */

countCallFrame(RetAddr) -->
    putAccOnStack(),
    setAddrOnFrame(),
    [swapaSymb],
    putAccOnStack(),
    moveConstToAcc(addrStart(RetAddr)),
    putAccOnStack(),      
    moveConstToAcc(3),
    [swapdSymb, swapaSymb, addSymb, swapaSymb],  
    setFramePointer().

countCallArgs([], _) -->
    [].

countCallArgs([H|T], InstrInfo) -->
    countArithInstr(H, InstrInfo),
    putAccOnStack(),
    countCallArgs(T, InstrInfo).

countCallArgsList([], _) -->
    !,
    [].
  
countCallArgsList(ArgsList, InstrInfo) -->
    {length(ArgsList, ArgsListLength)},
    setAddrOnStack(),
    moveStackPointer(65533),
    countCallArgs(ArgsList, InstrInfo),
    setAddrOnStack(),
    {Offset is 3 + ArgsListLength},
    moveStackPointer(Offset).  
  
countCallJump(ProcName, ProcArgs, (_, Procs)) -->
    {length(ProcArgs, ProcArgsLength)},
    {member(procInfo(ProcName, ProcArgsLength, Addr), Procs)},
    !,
    moveConstToAcc(addrStart(Addr)),
    [jumpSymb].  

countRetJump(RetAddr) -->
    [addrSymb(RetAddr)],      
    getAccFromStack(),
    [swapaSymb],
    setFramePointer(),
    getAccFromStack().  

countCallInstr((ProcName, ProcArgs), InstrInfo) -->
    countCallArgsList(ProcArgs, InstrInfo),
    countCallFrame(RetAddr),
    countCallJump(ProcName, ProcArgs, InstrInfo),  
    countRetJump(RetAddr).  

countProcInstr([], _) -->
    [].

countProcInstr([H|T], InstrInfo) -->
    countProgInstr(H, InstrInfo),
    countProcInstr(T, InstrInfo).
      
countProcInstr((_, _, block(_, Instr)), ProcInfo, procInfo(_, _, Addr)) -->
    [addrSymb(Addr)],
    countProcInstr(Instr, ProcInfo).
  
/* get vars info */  
  
addVars([], _, Acc, Acc) -->
    [].

addVars([H|T], Num, Acc, Vars) -->
    {NextAcc = [varInfo(H, Num)|Acc]},
    {NextNum is Num + 1},
    addVars(T, NextNum, NextAcc, Vars).

getVarsInfo([], _, Acc, Acc) -->
    [].

getVarsInfo([declVars(Vars)|T], Num, Acc, Result) -->
    !,
    addVars(Vars, Num, Acc, NextAcc),
    {length(Vars, VarsLength)},
    {NextNum is Num + VarsLength},
    getVarsInfo(T, NextNum, NextAcc, Result).
  
getVarsInfo([_|T], Num, Acc, Result) -->
    getVarsInfo(T, Num, Acc, Result).  

getVarsInfo(Decls, Vars) -->
    getVarsInfo(Decls, 0, [], Vars).
      
/* get procedures info */

getProcsInfo([], []) -->
    [].

getProcsInfo([declProc((ProcName, ProcArgs, _))|T], [Proc|Result]) -->
    !,
    {length(ProcArgs, ProcArgsLength)},
    {Proc = procInfo(ProcName, ProcArgsLength, _)},
    getProcsInfo(T, Result).

getProcsInfo([_|T], Result) -->
    getProcsInfo(T, Result).  
  
/* program instructions */

countMainInfo(Decls, ((GlobalVars, [], []), Procs)) -->
    getVarsInfo(Decls, GlobalVars),
    {length(GlobalVars, GlobalVarsLength)},
    getProcsInfo(Decls, Procs),
    stackEntryPoint(StackEntry),
    {StackPosition is StackEntry - GlobalVarsLength},
    setStackPointer(StackPosition).

createArgsInfo([], _, []) -->
    [].

createArgsInfo([H|T], Counter, [varInfo(Arg, Counter)|Result]) -->
    {H = valArg(Arg)},
    !,
    {NextCounter is Counter + 1},
    createArgsInfo(T, NextCounter, Result).

createArgsInfo([_|T], Counter, Result) -->
    createArgsInfo(T, Counter, Result).

getProcLocals(Proc, Args, Locals) -->
    {Proc = (_, ProcArgs, block(Decls, _))},
    createArgsInfo(ProcArgs, 0, Args),
    getVarsInfo(Decls, Locals).
  
getProcAddrInfo((ProcName, _, _), (_, Procs), ProcInfo) -->
    {member(procInfo(ProcName, ArgsLength, Addr), Procs)},
    {ProcInfo = procInfo(ProcName, ArgsLength, Addr)},
    !.

countProcsInfo([], _, _, _) -->
    [].
  
countProcsInfo([declProc(Proc)|T], Procs, Vars, MainInfo) -->
    !,
    getProcAddrInfo(Proc, MainInfo, ProcAddrInfo),
    getProcLocals(Proc, Args, Locals),
    {ProcInfo = ((Vars, Args, Locals), [ProcAddrInfo|Procs])},
    countProcInstr(Proc, ProcInfo, ProcAddrInfo),
    countProcsInfo(T, [ProcAddrInfo|Procs], Vars, MainInfo).      

countProcsInfo([declVars(Vars)|T], ProcsInfo, VarsInfo, MainInfo) -->
    {length(VarsInfo, VarsInfoLength)},
    addVars(Vars, VarsInfoLength, VarsInfo, NextVarsInfo),
    countProcsInfo(T, ProcsInfo, NextVarsInfo, MainInfo).  

countProgramInstr(program(_, block(Decls, Instr))) -->
    countMainInfo(Decls, MainInfo),
    countProcInstr(Instr, MainInfo),
    countProcsInfo(Decls, [], [], MainInfo).          
  

      

