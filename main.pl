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

