
(* The type of tokens. *)

type token = 
  | TIMES
  | THEN
  | RPAREN
  | PLUS
  | MINUS
  | LPAREN
  | LEQ
  | INT of (int)
  | IF
  | EOF
  | ELSE
  | DIVIDE
  | BOOL of (bool)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
