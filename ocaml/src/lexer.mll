{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("true", BOOL true)
  ; ("false", BOOL false)
  ; ("let", LET)
  ; ("in", IN)
  ; ("fun", FUN)
  ; ("fix", FIX)
  ; ("->", RARROW)
  ; ("if", IF)
  ; ("then", THEN)
  ; ("else", ELSE)
  ; ("<=", LEQ)
  ; ("=", EQ)
  ; ("-", SUB)
  ; ("+", ADD)
  ; ("/", DIV)
  ; ("*", MUL)
  ; ("(", LPAREN)
  ; (")", RPAREN)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols

let create_int lexbuf = lexeme lexbuf |> int_of_string
}

let newline      = '\n' | ('\r' '\n') | '\r'
let whitespace   = ['\t' ' ']
let digit        = ['0'-'9']
let booleans     = "true" | "false"
let char_tokens  = '=' | '-' | '+' | '/' | '*' | '(' | ')'
let string_tokens = "let"  | "in"  | "fun" | "fix" | "->" | "if" | "then" | "else" | "<="
let var_chars    = ['a'-'z'] | ['A'-'Z'] | '_' | digit

rule token = parse
  | digit+                    { INT (int_of_string (lexeme lexbuf)) }
  | booleans                  { BOOL (bool_of_string (lexeme lexbuf)) }
  | char_tokens               { create_symbol lexbuf }
  | string_tokens             { create_symbol lexbuf }
  | var_chars+                { VAR (lexeme lexbuf) }
  | whitespace+ | newline+    { token lexbuf }
  | eof                       { EOF }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }
