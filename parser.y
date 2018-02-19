%{
// PREPROCESSOR
#include <cstdio>
#include <iostream>

// REQUIRED FLEX METHOD STUBS
extern "C" int yylex();
extern "C" int yyparse();
extern "C" FILE *yyin;

// THIS NEEDS TO BE HERE I GUESS
void yyerror(const char *s);
%}

// UNION FOR GIVING TYPES TO THE YYSTYPE TOKENS FLEX RETURNS
%union {
	int ival;
  bool bval;
}

// ASSOCIATE TOKEN NAMES WITH THOSE TYPES
%token<ival> INT
%token<bval> BOOL

// OTHER TOKEN DECLARATIONS
%token PLUS MINUS TIMES DIVIDE LEQ IF THEN ELSE LPAREN RPAREN
%token NEWLINE QUIT

// OPERATOR PRECEDENCE (ASCENDING)
%left LEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE

%nonassoc IF
%nonassoc THEN
%nonassoc ELSE

// TYPING EXPRESSIONS
%type<ival> iexp
%type<bval> bexp

%start calculation
%%
// GRAMMAR
calculation:
      	   | calculation line
;

line: NEWLINE
    | iexp NEWLINE { std::cout << "\tResult: " << $1 << std::endl; }
    | bexp NEWLINE { std::cout << "\tResult: " << $1 << std::endl; }
;

bexp: BOOL               { $$ = $1; }
   ;

iexp: INT				         { $$ = $1; }
    | LPAREN IF bexp THEN iexp ELSE iexp RPAREN { if ($3) {$$ = $5;} else {$$ = $7;} }
    | iexp LEQ iexp      { $$ = $1 <= $3; }
    | iexp PLUS iexp	   { $$ = $1 + $3; }
	  | iexp MINUS iexp	   { $$ = $1 - $3; }
	  | iexp TIMES iexp	   { $$ = $1 * $3; }
    | iexp DIVIDE iexp   { $$ = $1 / $3; }
	  | LPAREN iexp RPAREN { $$ = $2; }
;

%%
int main(int argc, char** argv) {

  // flex doesn't like iostreams
	FILE *input = fopen(argv[1], "r");
	if (!input) {
		std::cout << "Error opening file." << std::endl;
		exit(EXIT_FAILURE);
	}
	yyin = input;

  // parse through it all
  do {
    yyparse();
  } while (!feof(yyin));

  exit(EXIT_SUCCESS);
}

void yyerror(const char *s) {
	std::cout << s << std::endl;
	exit(EXIT_FAILURE);
}