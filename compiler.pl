
/* lexical analysis */

lexer(TokenList) -->
     whiteSpace,
     !,
     lexer(TokenList).
    
lexer(TokenList) -->
     commentSpace,
     !,
     lexer(TokenList).
    
lexer([Token|RestList]) -->
     lexerToken(Token),
     !,
     lexer(RestList).          
    
lexer([]) -->
    [].
      
lexerToken(Token) -->  
    "+", !, {Token = tokPlus};
    "-", !, {Token = tokMinus};
    "*", !, {Token = tokMult};
    "<=", !, {Token = tokLessEq};
    ">=", !, {Token = tokGreaterEq};
    "<>", !, {Token = tokNeq};
    "<", !, {Token = tokLess};
    ">", !, {Token = tokGreater};
    "=", !, {Token = tokEq};
    ":=", !, {Token = tokAssgn};
    ";", !, {Token = tokColon};
    ",", !, {Token = tokComma};
    "(", !, {Token = tokLeftPar};
    ")", !, {Token = tokRightPar}.
  
lexerToken(tokNumber(Number)) -->
    digit(Digit),
    !,
    number(Digit, Number).
  
lexerToken(Token) -->
     letter(Letter),
     identifier(Letter, Id),
     {member((Id, Token), [
     (and, tokAnd),
     (begin, tokBegin),
     (call, tokCall),
     (div, tokDiv),    
     (do, tokDo),
     (done, tokDone),
     (else, tokElse),    
     (end, tokEnd),    
     (fi, tokFi),
     (if, tokIf),    
     (local, tokLocal),
     (mod, tokMod),
     (not, tokNot),    
     (or, tokOr),    
     (procedure, tokProcedure),
     (program, tokProgram),    
     (read, tokRead),
     (return, tokReturn),
     (then, tokThen),    
     (value, tokValue),
     (while, tokWhile),
     (write, tokWrite)])},
     !.

lexerToken(tokVar(Id)) -->              
    letter(Letter),
    identifier(Letter, Id).
         
whiteChar(32) -->
    [].
  
whiteChar(9) -->
    [].

whiteChar(10) -->
    [].
         
whiteSpace -->
   [Char],
   whiteChar(Char).
 
commentContent -->
    "*)",
    !.
  
commentContent -->
    [_],
    commentContent.
 
commentSpace -->
    "(*",
    commentContent.
 
digit(Digit) -->
   [Digit],
   { code_type(Digit, digit) }.

digits([Head|Tail]) -->
   digit(Head),
   !,
   digits(Tail).
 
digits([]) -->
   [].

number(Digit, Number) -->
   digits(Digits),
   { number_chars(Number, [Digit|Digits]) }.      

letter(Letter) -->
   [Letter],
   { code_type(Letter, alpha) }.

alphaChar(Char) -->
    {code_type(Char, alnum)},
    [].
  
alphaChar(95) -->
    [].
  
alphaChar(39) -->
    [].

alphanum([Head|Tail]) -->
    [Head],
    alphaChar(Head),
    !,
    alphanum(Tail). 
 
alphanum([]) -->
   [].

identifier(Letter, Id) -->
   alphanum(Alpha),
   { atom_codes(Id, [Letter|Alpha]) }.

/*create abstract tree */

programParse(program(ProgName, Block)) -->
    [tokProgram],
    [tokVar(ProgName)],
    blockParse(Block).  
      
blockParse(block(Decl, Instr)) -->
    declsParse(Decl),
    [tokBegin],
    complInstrParse(Instr),
    [tokEnd].
  
declsParse([Head|Tail]) -->
    declParse(Head),
    !,
    declsParse(Tail).
  
declsParse([]) -->
    [].  
  
declParse(declVars(Vars)) -->
    declVarsParse(Vars),
    !.  
      
declParse(declProc(Proc)) -->
    procParse(Proc).
  
declVarsParse(Vars) -->
    [tokLocal],
    varsParse(Vars).
  
varsParse([Head|Tail]) -->
    varParse(Head),
    [tokComma],
    !,
    varsParse(Tail).

varsParse([Var]) -->
    varParse(Var).
  
varParse(Var) -->
    [tokVar(Var)].
  
procParse((ProcName, ProcArgs, ProcBlock)) -->
    [tokProcedure],
    procNameParse(ProcName),
    [tokLeftPar],
    formArgsParse(ProcArgs),
    [tokRightPar],
    blockParse(ProcBlock).
  
procNameParse(ProcName) -->
    [tokVar(ProcName)].
  
formArgsParse(Args) -->
    formArgsStringParse(Args),
    !.
      
formArgsParse([]) -->
    [].  
  
formArgsStringParse([Head|Tail]) -->
    formArgParse(Head),
    [tokComma],
    !,
    formArgsStringParse(Tail).
  
formArgsStringParse([Arg]) -->
    formArgParse(Arg).
  
formArgParse(valArg(Arg)) -->
    [tokValue],
    !,
    varParse(Arg).
  
formArgParse(nameArg(Arg)) -->
    varParse(Arg).

complInstrParse([Head|Tail]) -->
    instrParse(Head),
    [tokColon],
    !,
    complInstrParse(Tail).
  
complInstrParse([Instr]) -->
    instrParse(Instr).
      
instrParse(instrAssgn(Var, ArithExpr)) -->
    varParse(Var),
    [tokAssgn],
    !,
    arithExprParse(ArithExpr).
  
instrParse(instrIf(BoolExpr, Instr)) -->
    [tokIf],
    boolExprParse(BoolExpr),
    [tokThen],
    complInstrParse(Instr),
    [tokFi],
    !.
          
instrParse(instrIfElse(BoolExpr, IfInstr, ElseInstr)) -->
    [tokIf],
    !,
    boolExprParse(BoolExpr),
    [tokThen],
    complInstrParse(IfInstr),
    [tokElse],
    complInstrParse(ElseInstr),  
    [tokFi].

instrParse(instrWhile(BoolExpr, Instr)) -->
    [tokWhile],
    !,
    boolExprParse(BoolExpr),
    [tokDo],
    complInstrParse(Instr),  
    [tokDone].  

instrParse(instrCall(CallProc)) -->
    [tokCall],
    !,
    callProcParse(CallProc).
  
instrParse(instrReturn(ArithExpr)) -->
    [tokReturn],
    !,
    arithExprParse(ArithExpr).
  
instrParse(instrRead(Var)) -->
    [tokRead],
    !,
    varParse(Var).
  
instrParse(instrWrite(ArithExpr)) -->
    [tokWrite],
    arithExprParse(ArithExpr).

arithExprParse(ArithExpr) -->
    memberParse(Member),
    arithExprParse(Member, ArithExpr).
  
arithExprParse(Acc, ArithExpr) -->
    additiveOpParse(Op),
    !,
    memberParse(Member),
    {NextAcc = arith(Acc, Op, Member)},
    arithExprParse(NextAcc, ArithExpr).
  
arithExprParse(Acc, Acc) -->
    [].
  
additiveOpParse(plusOp) -->
    [tokPlus],
    !.
  
additiveOpParse(minusOp) -->
    [tokMinus].

memberParse(ArithExpr) -->
    factorParse(Factor),
    memberParse(Factor, ArithExpr).
  
memberParse(Acc, ArithExpr) -->
    multOpParse(Op),
    !,
    factorParse(Factor),
    {NextAcc = arith(Acc, Op, Factor)},
    memberParse(NextAcc, ArithExpr).

memberParse(Acc, Acc) -->
    [].
  
multOpParse(multOp) -->
    [tokMult],
    !.
  
multOpParse(divOp) -->
    [tokDiv],
    !.
  
multOpParse(modOp) -->
    [tokMod].  

factorParse(arith(neqOp, Expr)) -->
    [tokMinus],
    !,
    simpleExprParse(Expr).
  
factorParse(Expr) -->
    simpleExprParse(Expr).
  
simpleExprParse(Expr) -->
    [tokLeftPar],
    !,
    arithExprParse(Expr),
    [tokRightPar].
  
simpleExprParse(Expr) -->
    atomicExprParse(Expr).
      
atomicExprParse(atomCall(Call)) -->
    callProcParse(Call),
    !.

atomicExprParse(atomNum(Num)) -->
    [tokNumber(Num)],
    !.
  
atomicExprParse(atomVar(Var)) -->
    varParse(Var).
          
callProcParse((ProcName, ProcArgs)) -->
    procNameParse(ProcName),
    [tokLeftPar],
    actArgsParse(ProcArgs),
    [tokRightPar].
  
actArgsParse(ProcArgs) -->
    actArgsStringParse(ProcArgs),
    !.

actArgsParse([]) -->
    [].

actArgsStringParse([Head|Tail]) -->
    actArgParse(Head),
    [tokComma],
    !,
    actArgsStringParse(Tail).
  
actArgsStringParse([Arg]) -->
    actArgParse(Arg).
  
actArgParse(Arg) -->
    arithExprParse(Arg).
  
boolExprParse(BoolExpr) -->
    conjExprParse(Head),
    boolExprParse(Head, BoolExpr).
  
boolExprParse(Acc, BoolExpr) -->  
    [tokOr],
    !,
    conjExprParse(Head),
    {NextAcc = logic(Acc, orOp, Head)},
    boolExprParse(NextAcc, BoolExpr).
  
boolExprParse(Acc, Acc) -->
    [].
  
conjExprParse(ConjExpr) -->
    condExprParse(Head),
    conjExprParse(Head, ConjExpr).
  
conjExprParse(Acc, ConjExpr) -->  
    [tokAnd],
    !,
    condExprParse(Head),
    {NextAcc = logic(Acc, andOp, Head)},  
    conjExprParse(NextAcc, ConjExpr).
  
conjExprParse(Acc, Acc) -->
    [].
  
condExprParse(logic(neg, Expr)) -->
    [tokNot],
    !,
    relExprParse(Expr).
  
condExprParse(Expr) -->
    relExprParse(Expr).

relExprParse(Expr) -->
    [tokLeftPar],
    boolExprParse(Expr),
    [tokRightPar],
	!.

relExprParse(relExpr(LeftExpr, Op, RightExpr)) -->
    arithExprParse(LeftExpr),
    relOp(Op),
    arithExprParse(RightExpr).
      
relOp(lessOp) -->
    [tokLess],
    !.
  
relOp(lessEqOp) -->
    [tokLessEq],
    !.

relOp(greaterOp) -->
    [tokGreater],
    !.

relOp(greaterEqOp) -->
    [tokGreaterEq],
    !.

relOp(eqOp) -->
    [tokEq],
    !.

relOp(neqOp) -->
    [tokNeq].  

/* edit abstract tree */

editProcInstr(Instr, [instrInit()|Instr]) :-
    last(Instr, instrReturn(_)),
    !.

editProcInstr(Instr, [instrInit()|EditedInstr]) :-
    append(Instr, [instrReturn(atomNum(0))], EditedInstr).

editProcBlock(block(Decls, Instr), block(Decls, EditedInstr)) :-
    editProcInstr(Instr, EditedInstr).

editDecl(declVars(X), declVars(X)).

editDecl(declProc((N, A, B)), declProc((N, A, NewB))) :-
    editProcBlock(B, NewB).

editDecls(Decls, EditedDecls) :-
    maplist(editDecl, Decls, EditedDecls).

editMainInstr(Instr, EditedInstr) :-
    append(Instr, [instrHalt()], EditedInstr).

editMainBlock(block(Decls, Instr), block(EditedDecls, EditedInstr)) :-
    editDecls(Decls, EditedDecls),
    editMainInstr(Instr, EditedInstr).

editTree(program(Name, Block), program(Name, EditedBlock)) :-
    editMainBlock(Block, EditedBlock).  

/* stack instructions */

stackPointerAddr(65535) -->
    [].  
  
stackEntryPoint(65525) -->
    [].

moveConstToAcc(Num) -->
    [constSymb, Num].  

setStackPointerOnAddr() -->
    [swapaSymb, swapdSymb],
    stackPointerAddr(PointerAddr),  
    moveConstToAcc(PointerAddr),
    [swapaSymb, swapdSymb, storeSymb].

setStackPointer(Num) -->
    stackPointerAddr(PointerAddr),  
    moveConstToAcc(PointerAddr),
    [swapaSymb],
    moveConstToAcc(Num),
    [storeSymb].

setAddrOnStack -->
    stackPointerAddr(PointerAddr),  
    moveConstToAcc(PointerAddr),
    [swapaSymb, loadSymb, swapaSymb].  

moveStackPointer(Num) -->
    moveConstToAcc(Num),
    [swapdSymb, swapaSymb, addSymb, swapdSymb],
    stackPointerAddr(PointerAddr),  
    moveConstToAcc(PointerAddr),
    [swapaSymb, swapdSymb, storeSymb, swapaSymb].

putAccOnStack -->
    [swapdSymb],
    setAddrOnStack(),
    [swapdSymb, storeSymb],
    moveStackPointer(65535).
      
getAccFromStack -->
    setAddrOnStack(),
    moveStackPointer(1),
    [loadSymb].          

/* tmp vars instructions */

tmpVarEntryPoint(65533) -->
	[].
	
tmpVarAddr(Num, Addr) -->
	{Num < 8},
	tmpVarEntryPoint(EntryPoint),
	{Addr is EntryPoint - Num}.	

loadTmpVar(Num) -->
	tmpVarAddr(Num, Addr),
	moveConstToAcc(Addr),
	[swapaSymb, loadSymb].
	
saveTmpVar(Num) -->
	tmpVarAddr(Num, Addr),
	[swapaSymb],
	moveConstToAcc(Addr),
	[swapaSymb, storeSymb].	

/* frame instructions */

framePointerAddr(65534) -->
    [].

setAddrOnFrame -->
    framePointerAddr(PointerAddr),
    [swapdSymb],
     moveConstToAcc(PointerAddr),
    [swapaSymb, loadSymb, swapaSymb, swapdSymb].      
  
setAddrOnFrame(Offset) -->
    setAddrOnFrame(),
    [swapaSymb, swapdSymb],
    moveConstToAcc(Offset),
    [swapdSymb, subSymb, swapaSymb].  
  
setFramePointer -->
    framePointerAddr(PointerAddr),
     moveConstToAcc(PointerAddr),
     [swapaSymb, storeSymb].  
  
/* vars instructions */

setLocalAddr(Var, Args, _) -->
    {member(varInfo(Var, Pos), Args)},
    !,  
    {Offset is 3 + Pos},
    setAddrOnFrame(Offset).

setLocalAddr(Var, Args, Locals) -->
    {member(varInfo(Var, Pos), Locals)},
    !,  
    {length(Args, ArgsLength)},
    {Offset is 3 + ArgsLength + Pos},
    setAddrOnFrame(Offset).  

getGlobalAddr(Var, Globals, Addr) -->
    {member(varInfo(Var, Pos), Globals)},
    !,
    stackEntryPoint(StackEntry),
    {Addr is StackEntry - Pos}.

saveAccToVar(Var, ((_, Args, Locals), _)) -->
    setLocalAddr(Var, Args, Locals),
    !,
    [storeSymb].
  
saveAccToVar(Var, ((Globals, _, _), _)) -->
    getGlobalAddr(Var, Globals, Addr),
    [swapdSymb],
    moveConstToAcc(Addr),
    [swapaSymb, swapdSymb, storeSymb].
      
loadVarToAcc(Var, ((_, Args, Locals), _)) -->
    setLocalAddr(Var, Args, Locals),
    !,
    [loadSymb].      
      
loadVarToAcc(Var, ((Globals, _, _), _)) -->
    getGlobalAddr(Var, Globals, Addr),
    moveConstToAcc(Addr),
    [swapaSymb, loadSymb].  

/* arithmetic instructions */

arithOp(plusOp) -->
    [addSymb].
  
arithOp(minusOp) -->
    [subSymb].

arithOp(multOp) -->
    [mulSymb].
  
arithOp(divOp) -->
    [divSymb].

countArithChildren(atomNum(Arg1), Arg2, InstrInfo) -->
    !,
    countArithInstr(Arg2, InstrInfo),
    [swapdSymb],
    moveConstToAcc(Arg1).
  
countArithChildren(Arg1, atomNum(Arg2), InstrInfo) -->
    !,
    countArithInstr(Arg1, InstrInfo),
    [swapdSymb],
    moveConstToAcc(Arg2),
    [swapdSymb].
  
countArithChildren(Arg1, Arg2, InstrInfo) -->
    countArithInstr(Arg1, InstrInfo),
    putAccOnStack(),
    countArithInstr(Arg2, InstrInfo),
    [swapdSymb],
    getAccFromStack().  

countArithInstr(atomNum(X), _) -->
    moveConstToAcc(X).

countArithInstr(atomVar(Var), InstrInfo) -->
    loadVarToAcc(Var, InstrInfo).

countArithInstr(atomCall(Call), InstrInfo) -->
    countCallInstr(Call, InstrInfo).

countArithInstr(arith(Arg1, Op, Arg2), InstrInfo) -->
    countArithChildren(Arg1, Arg2, InstrInfo),
    arithOp(Op),
    !.
      
countArithInstr(arith(Arg1, modOp, Arg2), InstrInfo) -->
    countArithChildren(Arg1, Arg2, InstrInfo),
    [divSymb],
    moveConstToAcc(65520),
    [swapdSymb],
    [shiftSymb].
  
countArithInstr(arith(neqOp, Arg), InstrInfo) -->
    countArithInstr(Arg, InstrInfo),
    [swapdSymb],
    moveConstToAcc(65535),
    [mulSymb].      
  
/* bool instructions */

countBoolNeg() -->
    [swapdSymb],
    moveConstToAcc(1),
    [nandSymb].
  
countBoolAnd() -->
    [nandSymb],
    countBoolNeg().
      
countBoolLess(Arg1, Arg2, InstrInfo) -->
    countArithChildren(Arg1, Arg2, InstrInfo),  
    saveTmpVar(0),
    [swapdSymb],
    saveTmpVar(1),
    [swapaSymb],
    moveConstToAcc(2),
    [swapdSymb, divSymb, swapaSymb, swapdSymb],
    moveConstToAcc(2),
    [swapdSymb, divSymb, swapdSymb, swapaSymb, subSymb, swapaSymb].

countBoolLessZeroCase() -->
    moveConstToAcc(addrStart(AddrZero)),
    [swapaSymb, branchzSymb, swapdSymb],   
    moveConstToAcc(65521),
    [swapdSymb, shiftSymb, swapdSymb],
    moveConstToAcc(addrStart(AddrEnd)),
    [jumpSymb, addrSymb(AddrZero)],              
	loadTmpVar(1),
	[swapdSymb],
	loadTmpVar(0),	    
    [subSymb, swapdSymb],
    moveConstToAcc(65521),   
    [swapdSymb, shiftSymb, swapdSymb, addrSymb(AddrEnd), swapdSymb].   
      
countBoolLogic(logic(neg, Arg), InstrInfo) -->
    countBoolLogic(Arg, InstrInfo),
    countBoolNeg().

countBoolLogic(logic(Arg1, andOp, Arg2), InstrInfo) -->
    !,
    countBoolLogic(Arg1, InstrInfo),
    putAccOnStack(),
    countBoolLogic(Arg2, InstrInfo),
    [swapdSymb],
    getAccFromStack(),
    countBoolAnd().

countBoolLogic(logic(Arg1, orOp, Arg2), InstrInfo) -->
    countBoolLogic(logic(neg, Arg1), InstrInfo),
    putAccOnStack(),
    countBoolLogic(logic(neg, Arg2), InstrInfo),
    [swapdSymb],
    getAccFromStack(),
    [nandSymb].

countBoolLogic(relExpr(Arg1, neqOp, Arg2), InstrInfo) -->
    !,
    countArithChildren(Arg1, Arg2, InstrInfo),
    [subSymb, swapaSymb],
    moveConstToAcc(addrStart(Addr)),   
    [swapaSymb, branchzSymb],
    moveConstToAcc(1),
    [addrSymb(Addr)].
   
countBoolLogic(relExpr(Arg1, eqOp, Arg2), InstrInfo) -->
    !,
    countBoolLogic(relExpr(Arg1, neqOp, Arg2), InstrInfo),
    countBoolNeg().

countBoolLogic(relExpr(Arg1, lessOp, Arg2), InstrInfo) -->
    !,    
	countBoolLess(Arg1, Arg2, InstrInfo),
	countBoolLessZeroCase().

countBoolLogic(relExpr(Arg1, greaterOp, Arg2), InstrInfo) -->
    !,
    countBoolLogic(relExpr(Arg2, lessOp, Arg1), InstrInfo).
       
countBoolLogic(relExpr(Arg1, lessEqOp, Arg2), InstrInfo) -->
    !,
    countBoolLogic(relExpr(Arg1, greaterOp, Arg2), InstrInfo),
    countBoolNeg().    
   
countBoolLogic(relExpr(Arg1, greaterEqOp, Arg2), InstrInfo) -->
    !,
    countBoolLogic(relExpr(Arg1, lessOp, Arg2), InstrInfo),
    countBoolNeg().                  
      
countBoolInstr(BoolInstr, InstrInfo) -->
    countBoolLogic(BoolInstr, InstrInfo),
    [swapdSymb],
    moveConstToAcc(2),
    [swapdSymb, divSymb],
    moveConstToAcc(65520),
    [swapdSymb, shiftSymb].
  
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
  
/* create SextiumAsm from SextiumList */

nopList(0) -->
    !,
    [].
  
nopList(Count) -->
    {NextCount is Count - 1},
    [nopSymb],
    nopList(NextCount).

newLineSymb(constSymb) -->
    [].

newLineSymb(jumpSymb) -->
    [].

swapSextiumAsm(0, Rest, Rest, []) -->
    !,
    [].

swapSextiumAsm(Counter, [constSymb, Num|T], Rest, [Num|NumList]) -->
    !,
    {NextCounter is Counter - 1},
    [constSymb],
    swapSextiumAsm(NextCounter, T, Rest, NumList).

swapSextiumAsm(Counter, [H|T], Rest, NumList) -->
    {NextCounter is Counter - 1},
    {\+ H = jumpSymb},
    {\+ H = addrSymb(_)},
    [H],
    swapSextiumAsm(NextCounter, T, Rest, NumList).

toSextiumAsm([], Counter) -->
    {NopCount is (4 - Counter mod 4) mod 4},
    nopList(NopCount).

toSextiumAsm([Number|T], Counter) -->
    {number(Number)},
    !,
    [Number],
    {NextCounter is Counter + 4},
    toSextiumAsm(T, NextCounter).  

toSextiumAsm([addrStart(Number)|T], Counter) -->
    !,
    [Number],
    {NextCounter is Counter + 4},
    toSextiumAsm(T, NextCounter).  
   
toSextiumAsm([constSymb, Num|T], Counter) -->
    [constSymb],
    {SwapCount is (4 - (Counter + 1) mod 4) mod 4},
    swapSextiumAsm(SwapCount, T, RestList, NumList),
    {append([Num|NumList], RestList, X)},
    !,   
    {NextCounter is Counter + 1 + SwapCount},   
    toSextiumAsm(X, NextCounter).   

toSextiumAsm([Symb|T], Counter) -->
    newLineSymb(Symb),
    !,
    [Symb],
    {NopCount is (4 - (Counter + 1) mod 4) mod 4},
    {NextCounter is Counter + 1 + NopCount},  
    nopList(NopCount),  
    toSextiumAsm(T, NextCounter).  
  
toSextiumAsm([addrSymb(Addr)|T], Counter) -->
    !,
    {NopCount is (4 - Counter mod 4) mod 4},
    {NextCounter is Counter + NopCount},
    {Addr is NextCounter / 4},
    nopList(NopCount),
    toSextiumAsm(T, NextCounter).          
  
toSextiumAsm([H|T], Counter) -->
    [H],
    {NextCounter is Counter + 1},
    toSextiumAsm(T, NextCounter).

toSextiumAsm(SextiumList, SextiumAsm) :-
    phrase(toSextiumAsm(SextiumList, 0), SextiumAsm).

/*create SextiumBin from SextiumAsm */

symbolCode(nopSymb, 0).
symbolCode(sysSymb, 1).
symbolCode(loadSymb, 2).
symbolCode(storeSymb, 3).
symbolCode(swapaSymb, 4).
symbolCode(swapdSymb, 5).
symbolCode(branchzSymb, 6).
symbolCode(branchnSymb, 7).
symbolCode(jumpSymb, 8).
symbolCode(constSymb, 9).
symbolCode(addSymb, 10).
symbolCode(subSymb, 11).
symbolCode(mulSymb, 12).
symbolCode(divSymb, 13).
symbolCode(shiftSymb, 14).
symbolCode(nandSymb, 15).

encodeCommand([], _, Acc, Acc).

encodeCommand([H|T], Mult, Acc, Result) :-
    NewAcc is Acc + (H * Mult),
    NewMult is Mult * 16,
    encodeCommand(T, NewMult, NewAcc, Result).
  
toSextiumBin([], []).  

toSextiumBin([H1, H2, H3, H4|T], [Dec|SubResult]) :-
    symbolCode(H1, C1),
    symbolCode(H2, C2),
    symbolCode(H3, C3),
    symbolCode(H4, C4),
    !,
    encodeCommand([C4, C3, C2, C1], 1, 0, Dec),
    toSextiumBin(T, SubResult).

toSextiumBin([H|T], [H|SubResult]) :-
    toSextiumBin(T, SubResult).
      
/* main */

createTree(CodeList, EditedTree) :-
    phrase(lexer(TokenList), CodeList),
    phrase(programParse(Tree), TokenList),
    editTree(Tree, EditedTree).

algol16(Source, SextiumBin) :-
    createTree(Source, Tree),
    phrase(countProgramInstr(Tree), SextiumList),  
    toSextiumAsm(SextiumList, SextiumAsm),
    toSextiumBin(SextiumAsm, SextiumBin).  
  
/* tests */   

saveIntToFile([], _).

saveIntToFile([H|T], File) :-
    write(File, H),
    nl(File),  
    saveIntToFile(T, File).

saveToFile(List) :-
    open("sample_program.sasm", write, File),
    saveIntToFile(List, File),
    close(File).  

compileFile(FileName) :-  
    open(FileName, read, File),
    read_string(File, _, String),
    string_to_list(String, CodeList),
    algol16(CodeList, Result),
    saveToFile(Result).
