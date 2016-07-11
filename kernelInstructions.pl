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
