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

