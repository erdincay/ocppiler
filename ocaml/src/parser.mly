%{
open Lang
%}

%token <int> INT

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token TIMES      (* * *)
%token DIVIDE     (* / *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token LEQ        (* <= *)
%token IF         (* if *)
%token TRUE       (* true *)
%token FALSE      (* false *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | LPAREN e1=exp TIMES e2=exp RPAREN   { ETim (e1, e2) }
  | LPAREN e1=exp DIVIDE e2=exp RPAREN  { EDiv (e1, e2) }
  | LPAREN e1=exp PLUS e2=exp RPAREN    { EAdd (e1, e2) }
  | LPAREN e1=exp MINUS e2=exp RPAREN   { ESub (e1, e2) }
  | n=INT                               { EInt n }
