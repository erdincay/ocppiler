%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%token LET    (* let  *)
%token IN     (* in   *)
%token FUN    (* fun  *)
%token FIX    (* fix  *)
%token RARROW (* ->   *)
%token IF     (* if   *)
%token THEN   (* then *)
%token ELSE   (* else *)
%token LEQ    (* <=   *)
%token EQ     (* =    *)
%token SUB    (* -    *)
%token ADD    (* +    *)
%token DIV    (* /    *)
%token MUL    (* *    *)
%token LPAREN (* (    *)
%token RPAREN (* )    *)
%token EOF

%left LEQ
%left SUB ADD
%left DIV MUL

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                         { e }

exp:
  | n=INT                             { EInt n }
  | b=BOOL                            { EBool b }
  | LET vr= VAR EQ e1=exp IN e2=exp    { ELet (Var vr, e1, e2) }
  | FUN vr= VAR RARROW e=exp           { EFun (Var vr, e) }
  | FIX vr1= VAR vr2= VAR RARROW e=exp  { EFix (Var vr1, Var vr2, e) }
  | IF e1=exp THEN e2=exp ELSE e3=exp { EIf (e1, e2, e3) }
  | e1=exp LEQ e2=exp                 { EOp (e1, OLEq, e2) }
  | e1=exp SUB e2=exp                 { EOp (e1, OSub, e2) }
  | e1=exp ADD e2=exp                 { EOp (e1, OAdd, e2) }
  | e1=exp DIV e2=exp                 { EOp (e1, ODiv, e2) }
  | e1=exp MUL e2=exp                 { EOp (e1, OMul, e2) }
  | LPAREN e=exp RPAREN               { e }
