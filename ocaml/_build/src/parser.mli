
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TUNIT
  | TINT
  | THEN
  | TBOOL
  | SUB
  | SECOND
  | RPAREN
  | RARROW
  | MUL
  | LPAREN
  | LET
  | LEQ
  | INT of (int)
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
  | BOOL of (bool)
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
