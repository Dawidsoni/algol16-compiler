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

