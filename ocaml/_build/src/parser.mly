%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%token TINT
%token TBOOL
%token TUNIT

%token LET    (* let  *)
%token IN     (* in   *)
%token FUN    (* fun  *)
%token FIX    (* fix  *)
%token RARROW (* ->   *)
%token COMMA  (* ,    *)
%token IF     (* if   *)
%token THEN   (* then *)
%token ELSE   (* else *)
%token LEQ    (* <=   *)
%token EQ     (* =    *)
%token SUB    (* -    *)
%token ADD    (* +    *)
%token DIV    (* /    *)
%token MUL    (* *    *)
%token FIRST  (* fst  *)
%token SECOND (* snd  *)
%token COLON  (* :    *)
%token LPAREN (* (    *)
%token RPAREN (* )    *)
%token EOF

%right RARROW
%left LEQ
%left SUB ADD
%left DIV MUL
%left FIRST SECOND

%start <Lang.exp> prog

%%

prog: e=exp EOF                                                               { e }

exp: n=INT                                                                    { EInt n }
   | b=BOOL                                                                   { EBool b }
   | vr=VAR                                                                   { EVar (Var vr) }
   | LPAREN RPAREN                                                            { EUnit }
   | LPAREN e1=exp COMMA e2=exp RPAREN                                        { EPair (e1, e2) }
   | e1=exp LEQ e2=exp                                                        { EOp (e1, OLEq, e2) }
   | e1=exp SUB e2=exp                                                        { EOp (e1, OSub, e2) }
   | e1=exp ADD e2=exp                                                        { EOp (e1, OAdd, e2) }
   | e1=exp DIV e2=exp                                                        { EOp (e1, ODiv, e2) }
   | e1=exp MUL e2=exp                                                        { EOp (e1, OMul, e2) }
   | FIRST e=exp                                                              { EFst e }
   | SECOND e=exp                                                             { ESnd e }
   | LPAREN e=exp RPAREN                                                      { e }
   | f=fexp                                                                   { f }

fexp: LET vr=VAR COLON t=typ EQ e1=exp IN e2=exp                               { ELet (Var vr, t, e1, e2) }
    | FUN LPAREN vr=VAR COLON t1=typ RPAREN COLON t2=typ RARROW e=exp          { EFun (Var vr, t1, t2, e) }
    | FIX vr1=VAR LPAREN vr2=VAR COLON t1=typ RPAREN COLON t2=typ RARROW e=exp { EFix (Var vr1, Var vr2, t1, t2, e) }
    | IF e1=exp THEN e2=exp ELSE e3=exp                                        { EIf (e1, e2, e3) }
    | e1=exp e2=exp                                                            { EApp (e1, e2) }

typ: TINT                            { TInt }
   | TBOOL                           { TBool }
   | TUNIT                           { TUnit }
   | t1=typ RARROW t2=typ            { TConv (t1, t2) }
   | LPAREN t1=typ MUL t2=typ RPAREN { TPair (t1, t2) }
   | LPAREN t=typ RPAREN             { t }
