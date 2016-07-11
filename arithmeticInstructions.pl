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
