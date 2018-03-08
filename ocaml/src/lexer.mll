{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("(", LPAREN)
  ; (")", RPAREN)
  ; ("*", TIMES)
  ; ("/", DIVIDE)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("<=", LEQ)
  ; ("if", IF)
  ; ("then", THEN)
  ; ("else", ELSE)
  ; ("true", BOOL true)
  ; ("false", BOOL false)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols

let create_int lexbuf = lexeme lexbuf |> int_of_string
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']

rule token = parse
  | eof                       { EOF }
  | digit+                    { INT (int_of_string (lexeme lexbuf)) }
  | whitespace+ | newline+    { token lexbuf }
  | '(' | ')' | '*' | '/' | '+' | '-' | "<=" | "if" | "then" | "else" | "true" | "false" { create_symbol lexbuf }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }
