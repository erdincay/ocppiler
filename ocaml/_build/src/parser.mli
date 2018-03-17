
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR of (string)
  | TUNIT
  | TINT
  | THEN
  | TBOOL
  | SUB
  | SEMI
  | RPAREN
  | REF
  | RARROW
  | RANGL
  | MUL
  | LPAREN
  | LET
  | LEQ
  | LANGL
  | INT of (int)
  | IN
  | IF
  | GET
  | FUN
  | FIX
  | EQ
  | EOF
  | END
  | ELSE
  | DO
  | DIV
  | COMMA
  | COLON
  | BOOL of (bool)
  | BANG
  | ASS
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
