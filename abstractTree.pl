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

