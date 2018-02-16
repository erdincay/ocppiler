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
  /* bool bval; */
}

/* define the "terminal symbol" token types to use (in CAPS by convention)
and associate each with a field of the union: */
%token<ival> INT
/* %token<bval> BOOL */
%token PLUS MINUS TIMES DIVIDE LPAREN RPAREN
%token NEWLINE QUIT
%left PLUS MINUS
%left MULTIPLY DIVIDE

%type<ival> expression

%start calculation

%% /* GRAMMAR */

calculation:
      	   | calculation line
;

line: NEWLINE
    | expression NEWLINE { printf("\tResult: %i\n", $1); }
    | QUIT NEWLINE { exit(EXIT_SUCCESS); }
;

expression: INT				                   { $$ = $1; }
          | expression PLUS expression	 { $$ = $1 + $3; }
	        | expression MINUS expression	 { $$ = $1 - $3; }
	        | expression TIMES expression	 { $$ = $1 * $3; }
          | expression DIVIDE expression { $$ = $1 / $3; }
	        | LPAREN expression RPAREN		 { $$ = $2; }
;
%%
int main(int argc, char** argv) {

	// open a file handle to a particular file:
	FILE *input = fopen(argv[1], "r");
	// make sure it's valid:
	if (!input) {
		std::cout << "Error opening file." << std::endl;
		exit(EXIT_FAILURE);
	}
	// set lex to read from it instead of defaulting to STDIN:
	yyin = input;

  // parse through the input until there is no more:
  do {
    yyparse();
  } while (!feof(yyin));

  exit(EXIT_SUCCESS);
}
