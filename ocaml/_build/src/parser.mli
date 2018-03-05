
(* The type of tokens. *)

type token = 
  | TRUE
  | TIMES
  | RPAREN
  | PLUS
  | MINUS
  | LPAREN
  | LEQ
  | INT of (int)
  | IF
  | FALSE
  | EOF
  | DIVIDE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
