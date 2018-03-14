
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TINT
  | THEN
  | TBOOL
  | SUB
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
  | EQ
  | EOF
  | ELSE
  | DIV
  | COLON
  | BOOL of (bool)
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp)
