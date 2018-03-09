
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TIMES
  | THEN
  | RPAREN
  | RARROW
  | PLUS
  | MINUS
  | LPAREN
  | LET
  | LEQ
  | INT of (int)
  | IN
  | IF
  | FUN
  | EQUAL
  | EOF
  | ELSE
  | DIVIDE
  | BOOL of (bool)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
