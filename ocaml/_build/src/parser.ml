
type token = 
  | VAR of (
# 7 "src/parser.mly"
       (string)
# 7 "src/parser.ml"
)
  | TUNIT
  | TINT
  | THEN
  | TBOOL
  | SUB
  | SEMI
  | SECOND
  | RPAREN
  | REF
  | RARROW
  | RANGL
  | MUL
  | LPAREN
  | LET
  | LEQ
  | LANGL
  | INT of (
# 5 "src/parser.mly"
       (int)
# 28 "src/parser.ml"
)
  | IN
  | IF
  | FUN
  | FIX
  | FIRST
  | EQ
  | EOF
  | ELSE
  | DIV
  | COMMA
  | COLON
  | BOOL of (
# 6 "src/parser.mly"
       (bool)
# 44 "src/parser.ml"
)
  | BANG
  | ASS
  | ADD

# 1 "src/parser.mly"
  
open Lang

# 54 "src/parser.ml"

let menhir_begin_marker =
  0

and (xv_typ, xv_prog, xv_exp) =
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (t : 'tv_typ) (_startpos_t_ : Lexing.position) (_endpos_t_ : Lexing.position) (_startofs_t_ : int) (_endofs_t_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 85 "src/parser.mly"
                          ( t )
# 64 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (t : 'tv_typ) (_startpos_t_ : Lexing.position) (_endpos_t_ : Lexing.position) (_startofs_t_ : int) (_endofs_t_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 84 "src/parser.mly"
                          ( TRef t )
# 70 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (t2 : 'tv_typ) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (t1 : 'tv_typ) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) ->
    (
# 83 "src/parser.mly"
                          ( TPair (t1, t2) )
# 76 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (t2 : 'tv_typ) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (t1 : 'tv_typ) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) ->
    (
# 82 "src/parser.mly"
                          ( TConv (t1, t2) )
# 82 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 81 "src/parser.mly"
                          ( TUnit )
# 88 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 80 "src/parser.mly"
                          ( TBool )
# 94 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 79 "src/parser.mly"
                          ( TInt )
# 100 "src/parser.ml"
     : 'tv_typ) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) ->
    (
# 54 "src/parser.mly"
                                                                              ( e )
# 106 "src/parser.ml"
     : (
# 50 "src/parser.mly"
       (Lang.exp)
# 110 "src/parser.ml"
    )) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 77 "src/parser.mly"
                                                                              ( ESnd e )
# 116 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 76 "src/parser.mly"
                                                                              ( EFst e )
# 122 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 75 "src/parser.mly"
                                                                              ( EApp (e1, e2) )
# 128 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 74 "src/parser.mly"
                                                                              ( EOp (e1, OMul, e2) )
# 134 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 73 "src/parser.mly"
                                                                              ( EOp (e1, ODiv, e2) )
# 140 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 72 "src/parser.mly"
                                                                              ( EOp (e1, OAdd, e2) )
# 146 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 71 "src/parser.mly"
                                                                              ( EOp (e1, OSub, e2) )
# 152 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 70 "src/parser.mly"
                                                                              ( EOp (e1, OLEq, e2) )
# 158 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 69 "src/parser.mly"
                                                                              ( EDeref e )
# 164 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 68 "src/parser.mly"
                                                                              ( ERef e )
# 170 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 67 "src/parser.mly"
                                                                              ( EAss (e1, e2) )
# 176 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e3 : 'tv_exp) (_startpos_e3_ : Lexing.position) (_endpos_e3_ : Lexing.position) (_startofs_e3_ : int) (_endofs_e3_ : int) (_5 : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 66 "src/parser.mly"
                                                                              ( EIf (e1, e2, e3) )
# 182 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) ->
    (
# 65 "src/parser.mly"
                                                                              ( ESeq)
# 188 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_10 : unit) (_startpos__10_ : Lexing.position) (_endpos__10_ : Lexing.position) (_startofs__10_ : int) (_endofs__10_ : int) (t2 : 'tv_typ) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_8 : unit) (_startpos__8_ : Lexing.position) (_endpos__8_ : Lexing.position) (_startofs__8_ : int) (_endofs__8_ : int) (_7 : unit) (_startpos__7_ : Lexing.position) (_endpos__7_ : Lexing.position) (_startofs__7_ : int) (_endofs__7_ : int) (t1 : 'tv_typ) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) (_5 : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (vr2 : (
# 7 "src/parser.mly"
       (string)
# 193 "src/parser.ml"
  )) (_startpos_vr2_ : Lexing.position) (_endpos_vr2_ : Lexing.position) (_startofs_vr2_ : int) (_endofs_vr2_ : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (vr1 : (
# 7 "src/parser.mly"
       (string)
# 197 "src/parser.ml"
  )) (_startpos_vr1_ : Lexing.position) (_endpos_vr1_ : Lexing.position) (_startofs_vr1_ : int) (_endofs_vr1_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 64 "src/parser.mly"
                                                                              ( EFix (Var vr1, Var vr2, t1, t2, e) )
# 202 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_9 : unit) (_startpos__9_ : Lexing.position) (_endpos__9_ : Lexing.position) (_startofs__9_ : int) (_endofs__9_ : int) (t2 : 'tv_typ) (_startpos_t2_ : Lexing.position) (_endpos_t2_ : Lexing.position) (_startofs_t2_ : int) (_endofs_t2_ : int) (_7 : unit) (_startpos__7_ : Lexing.position) (_endpos__7_ : Lexing.position) (_startofs__7_ : int) (_endofs__7_ : int) (_6 : unit) (_startpos__6_ : Lexing.position) (_endpos__6_ : Lexing.position) (_startofs__6_ : int) (_endofs__6_ : int) (t1 : 'tv_typ) (_startpos_t1_ : Lexing.position) (_endpos_t1_ : Lexing.position) (_startofs_t1_ : int) (_endofs_t1_ : int) (_4 : unit) (_startpos__4_ : Lexing.position) (_endpos__4_ : Lexing.position) (_startofs__4_ : int) (_endofs__4_ : int) (vr : (
# 7 "src/parser.mly"
       (string)
# 207 "src/parser.ml"
  )) (_startpos_vr_ : Lexing.position) (_endpos_vr_ : Lexing.position) (_startofs_vr_ : int) (_endofs_vr_ : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 63 "src/parser.mly"
                                                                              ( EFun (Var vr, t1, t2, e) )
# 212 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_7 : unit) (_startpos__7_ : Lexing.position) (_endpos__7_ : Lexing.position) (_startofs__7_ : int) (_endofs__7_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) (_5 : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (t : 'tv_typ) (_startpos_t_ : Lexing.position) (_endpos_t_ : Lexing.position) (_startofs_t_ : int) (_endofs_t_ : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (vr : (
# 7 "src/parser.mly"
       (string)
# 217 "src/parser.ml"
  )) (_startpos_vr_ : Lexing.position) (_endpos_vr_ : Lexing.position) (_startofs_vr_ : int) (_endofs_vr_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 62 "src/parser.mly"
                                                                              ( ELet (Var vr, t, e1, e2) )
# 222 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (e : 'tv_exp) (_startpos_e_ : Lexing.position) (_endpos_e_ : Lexing.position) (_startofs_e_ : int) (_endofs_e_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 61 "src/parser.mly"
                                                                              ( e )
# 228 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_5 : unit) (_startpos__5_ : Lexing.position) (_endpos__5_ : Lexing.position) (_startofs__5_ : int) (_endofs__5_ : int) (e2 : 'tv_exp) (_startpos_e2_ : Lexing.position) (_endpos_e2_ : Lexing.position) (_startofs_e2_ : int) (_endofs_e2_ : int) (_3 : unit) (_startpos__3_ : Lexing.position) (_endpos__3_ : Lexing.position) (_startofs__3_ : int) (_endofs__3_ : int) (e1 : 'tv_exp) (_startpos_e1_ : Lexing.position) (_endpos_e1_ : Lexing.position) (_startofs_e1_ : int) (_endofs_e1_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 60 "src/parser.mly"
                                                                              ( EPair (e1, e2) )
# 234 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (_2 : unit) (_startpos__2_ : Lexing.position) (_endpos__2_ : Lexing.position) (_startofs__2_ : int) (_endofs__2_ : int) (_1 : unit) (_startpos__1_ : Lexing.position) (_endpos__1_ : Lexing.position) (_startofs__1_ : int) (_endofs__1_ : int) ->
    (
# 59 "src/parser.mly"
                                                                              ( EUnit )
# 240 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (vr : (
# 7 "src/parser.mly"
       (string)
# 245 "src/parser.ml"
  )) (_startpos_vr_ : Lexing.position) (_endpos_vr_ : Lexing.position) (_startofs_vr_ : int) (_endofs_vr_ : int) ->
    (
# 58 "src/parser.mly"
                                                                              ( EVar (Var vr) )
# 250 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (b : (
# 6 "src/parser.mly"
       (bool)
# 255 "src/parser.ml"
  )) (_startpos_b_ : Lexing.position) (_endpos_b_ : Lexing.position) (_startofs_b_ : int) (_endofs_b_ : int) ->
    (
# 57 "src/parser.mly"
                                                                              ( EBool b )
# 260 "src/parser.ml"
     : 'tv_exp) in
  let _ = fun (_eRR : exn) (_startpos : Lexing.position) (_endpos : Lexing.position) (_endpos__0_ : Lexing.position) (_symbolstartpos : Lexing.position) (_startofs : int) (_endofs : int) (_endofs__0_ : int) (_symbolstartofs : int) (n : (
# 5 "src/parser.mly"
       (int)
# 265 "src/parser.ml"
  )) (_startpos_n_ : Lexing.position) (_endpos_n_ : Lexing.position) (_startofs_n_ : int) (_endofs_n_ : int) ->
    (
# 56 "src/parser.mly"
                                                                              ( EInt n )
# 270 "src/parser.ml"
     : 'tv_exp) in
  (raise Not_found : 'tv_typ * (
# 50 "src/parser.mly"
       (Lang.exp)
# 275 "src/parser.ml"
  ) * 'tv_exp)

and menhir_end_marker =
  0

# 219 "/usr/share/menhir/standard.mly"
  


# 285 "src/parser.ml"
