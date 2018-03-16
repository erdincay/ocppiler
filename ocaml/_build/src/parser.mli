
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
  | INT of (int)
  | IN
  | IF
  | FUN
  | FIX
  | FIRST
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
