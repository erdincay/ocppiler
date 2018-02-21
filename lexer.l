%option noyywrap

%{
#include <iostream>
#define YY_DECL extern "C" int yylex()

#include "parser.tab.h" // to get the token types that we return

%}
%%
[ \t]            ;
"if"             { return IF; }
"then"           { return THEN; }
"else"           { return ELSE; }
"let"            { return LET; }
"in"             { return IN; }
"fun"            { return FUN; }
[0-9]+           { yylval.ival = atoi(yytext); return INT; }
"true"           { yylval.bval = true; return BOOL; }
"false"          { yylval.bval = false; return BOOL; }
^[a-z]$          { yylval.cval = *yytext; return VAR; }
"->"             { return RARROW; }
\n               { return NEWLINE; }
"("              { return LPAREN; }
")"              { return RPAREN; }
"+"              { return PLUS; }
"-"              { return MINUS; }
"*"              { return TIMES; }
"/"              { return DIVIDE; }
"<="             { return LEQ; }
"="              { return EQUALS; }
.                { std::cerr << "Lexing error" << std::endl; exit(EXIT_FAILURE); }
%%
