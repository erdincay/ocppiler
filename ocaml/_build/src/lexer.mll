{
open Lexing
open Parser

exception Lexer_error of string

let create_char_token c =
  match c with
  | ';' -> SEMI
  | ',' -> COMMA
  | '=' -> EQ
  | '<' -> LANGL
  | '>' -> RANGL
  | '-' -> SUB
  | '+' -> ADD
  | '/' -> DIV
  | '*' -> MUL
  | ':' -> COLON
  | '!' -> BANG
  | '(' -> LPAREN
  | ')' -> RPAREN
  | _   -> failwith "Expected single char, found string."

let create_str_token str =
  match str with
  | ":="   -> ASS
  | "<="   -> LEQ
  | "->"   -> RARROW
  | _      -> failwith "Expected nonalphabetic string."

let create_alpha_token str =
  match str with
  | "true"  -> BOOL true
  | "false" -> BOOL false
  | "int"   -> TINT
  | "bool"  -> TBOOL
  | "unit"  -> TUNIT
  | "ref"   -> REF
  | "let"   -> LET
  | "in"    -> IN
  | "fun"   -> FUN
  | "fix"   -> FIX
  | "if"    -> IF
  | "then"  -> THEN
  | "else"  -> ELSE
  | "get"   -> GET
  | "while" -> WHILE
  | "do"    -> DO
  | "end"   -> END
  | _       -> VAR (str)
}

let newline      = '\n' | ('\r' '\n') | '\r'
let whitespace   = ['\t' ' ']
let digit        = ['0'-'9']
let char_tokens  = ';' | ',' | '=' | '<' | '>' |'-' | '+' | '/' | '*' | ':' | '!' | '(' | ')'
let str_tokens   = ":=" | "<=" | "->"
let var_chars    = ['a'-'z'] | ['A'-'Z'] | '_' | digit

rule token = parse
  | digit+                    { INT (int_of_string (lexeme lexbuf)) }
  | char_tokens as c          { create_char_token c }
  | str_tokens                { create_str_token (lexeme lexbuf) }
  | var_chars+                { create_alpha_token (lexeme lexbuf) }
  | whitespace+ | newline+    { token lexbuf }
  | eof                       { EOF }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }
