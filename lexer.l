%option noyywrap

%{
#include <iostream>
#define YY_DECL extern "C" int yylex()

#include "parser.tab.h" // to get the token types that we return

%}
%%
[ \t]            ;
\n               { return NEWLINE; }
"("              { return RPAREN; }
")"              { return LPAREN; }
[0-9]+           { yylval.ival = atoi(yytext); return INT; }
"true"           { std::cout << "TRUE "; }
"false"          { std::cout << "FALSE "; }
"+ "             { return PLUS; }
"- "             { return MINUS; }
"* "             { return TIMES; }
"/ "             { return DIVIDE; }
"<= "            { std::cout << "LEQ "; }
"if "            { std::cout << "IF "; }
"then "          { std::cout << "THEN "; }
"else "          { std::cout << "ELSE "; }
"quit"           { return QUIT; }
.                { std::cerr << "Lexing error" << std::endl; exit(EXIT_FAILURE); }
%%
