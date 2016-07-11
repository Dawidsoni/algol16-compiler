
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
