%option noyywrap

%{
#include <iostream>
#define YY_DECL extern "C" int yylex()

#include "parser.tab.h" // to get the token types that we return

%}
%%
[ \t]            ;
[0-9]+           { yylval.ival = atoi(yytext); return INT; }
"true"           { yylval.bval = true; return BOOL; }
"false"          { yylval.bval = false; return BOOL;  }
\n               { return NEWLINE; }
"+"              { return PLUS; }
"-"              { return MINUS; }
"*"              { return TIMES; }
"/"              { return DIVIDE; }
"("              { return LPAREN; }
")"              { return RPAREN; }
"<="             { return LEQ; }
"if"             { return IF; }
"then"           { return THEN; }
"else"           { return ELSE; }
.                { std::cerr << "Lexing error" << std::endl; exit(EXIT_FAILURE); }
%%
