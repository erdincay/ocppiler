%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%token TUNIT
%token TINT
%token TBOOL

%token LPAREN (* (    *)
%token RPAREN (* )    *)
%token COLON  (* :    *)
%token LET    (* let  *)
%token IN     (* in   *)
%token FUN    (* fun  *)
%token FIX    (* fix  *)
%token RARROW (* ->   *)
%token SEMI   (* ;    *)
%token IF     (* if   *)
%token THEN   (* then *)
%token ELSE   (* else *)
%token ASS    (* :=   *)
%token REF    (* ref  *)
%token BANG   (* !    *)
%token COMMA  (* ,    *)
%token EQ     (* =    *)
%token LANGL  (* <    *)
%token RANGL  (* >    *)
%token LEQ    (* <=   *)
%token SUB    (* -    *)
%token ADD    (* +    *)
%token DIV    (* /    *)
%token MUL    (* *    *)

%token FIRST  (* fst  *)
%token SECOND (* snd  *)

%token EOF

%right RARROW
%right SEMI
%right ASS
%left LEQ
%left SUB ADD
%left DIV MUL
%left FIRST SECOND

%start <Lang.exp> prog

%%

prog: e=exp EOF                                                               { e }

exp: LPAREN RPAREN                                                            { EUnit }
   | n=INT                                                                    { EInt n }
   | b=BOOL                                                                   { EBool b }
   | vr=VAR                                                                   { EVar (Var vr) }
   | LPAREN e1=exp COMMA e2=exp RPAREN                                        { EPair (e1, e2) }
   | LPAREN e=exp RPAREN                                                      { e }
   | LET vr=VAR COLON t=typ EQ e1=exp IN e2=exp                               { ELet (Var vr, t, e1, e2) }
   | FUN LPAREN vr=VAR COLON t1=typ RPAREN COLON t2=typ RARROW e=exp          { EFun (Var vr, t1, t2, e) }
   | FIX vr1=VAR LPAREN vr2=VAR COLON t1=typ RPAREN COLON t2=typ RARROW e=exp { EFix (Var vr1, Var vr2, t1, t2, e) }
   | e1=exp SEMI e2=exp                                                       { ESeq (e1, e2) }
   | IF e1=exp THEN e2=exp ELSE e3=exp                                        { EIf (e1, e2, e3) }
   | e1=exp ASS e2=exp                                                        { EAssign (e1, e2) }
   | REF e=exp                                                                { ERef e }
   | BANG e=exp                                                               { EDeref e }
   | e1=exp LEQ e2=exp                                                        { EOp (e1, OLEq, e2) }
   | e1=exp SUB e2=exp                                                        { EOp (e1, OSub, e2) }
   | e1=exp ADD e2=exp                                                        { EOp (e1, OAdd, e2) }
   | e1=exp DIV e2=exp                                                        { EOp (e1, ODiv, e2) }
   | e1=exp MUL e2=exp                                                        { EOp (e1, OMul, e2) }
   | e1=exp e2=exp                                                            { EApp (e1, e2) }
   | FIRST e=exp                                                              { EFst e }
   | SECOND e=exp                                                             { ESnd e }

typ: TUNIT                { TUnit }
   | TINT                 { TInt }
   | TBOOL                { TBool }
   | t1=typ RARROW t2=typ { TConv (t1, t2) }
   | t1=typ MUL t2=typ    { TPair (t1, t2) }
   | LANGL t=typ RANGL    { TRef t }
   | LPAREN t=typ RPAREN  { t }
