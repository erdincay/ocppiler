%{
#include <cstdio>
#include <iostream>

// stuff from flex that bison needs to know about:
extern "C" int yylex();
extern "C" int yyparse();
extern "C" FILE *yyin;

void yyerror(const char *s);
%}

/* Bison fundamentally works by asking flex to get the next token, which it
 returns as an object of type "yystype". But tokens could be of any
 arbitrary data type!  So we deal with that in Bison by defining a C union
 holding each of the types of tokens that Flex could return, and have Bison
 use that union instead of "int" for the definition of "yystype": */
%union {
	int ival;
  bool bval;
}

/* define the "terminal symbol" token types to use (in CAPS by convention)
and associate each with a field of the union: */
%token <ival> INT
%token <bval> BOOL

%%
/* this is the actual grammar that bison will parse, but for right now it's just
 something silly to echo to the screen what bison gets from flex.  We'll
 make a real one shortly: */
lexer:
	INT lexer        { std::cout << $1 << std::endl; }
	| BOOL lexer     { std::cout << $1 << std::endl; }
	| INT            { std::cout << $1 << std::endl; }
	;
%%

int main(int argc, char** argv) {

  // open a file handle to a particular file:
  FILE *input = fopen(argv[1], "r");
  // make sure it's valid:
  if (!input) {
    std::cout << "Error opening file" << std::endl;
    exit(EXIT_FAILURE);
  }
  // set lex to read from it instead of defaulting to STDIN:
  yyin = input;

	// lex through the input:
	yylex();
}

void yyerror(const char *s) {
	std::cerr << "Parse error " << s << std::endl;
	// might as well halt now:
	exit(EXIT_FAILURE);
}
