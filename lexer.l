%{
#include <cstdio>
#include <iostream>
#define YY_DECL extern "C" int yylex()

#include "parser.tab.h" // to get the token types that we return

%}
%%
[ \t\n]         ;
"("             { std::cout << "LPAREN "; }
")"             { std::cout << "RPAREN "; }
[0-9]+          { yylval.ival = atoi(yytext); return INT; }
"true"          { yylval.ival = true; return BOOL; }
"false"         { yylval.ival = false; return BOOL; }
"+"             { std::cout << "PLUS "; }
"-"             { std::cout << "MINUS "; }
"*"             { std::cout << "TIMES "; }
"/"             { std::cout << "DIVIDE "; }
"<="            { std::cout << "LEQ "; }
"if"            { std::cout << "IF "; }
.               { std::cerr << "Lexing error" << std::endl; exit(EXIT_FAILURE); }
%%
