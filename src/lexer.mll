(* This file organizes the way that we read our langauge and turn it into tokens. 
 *
 * Resources 
 * On understanding General Format:
 *    https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
 *    https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lexer.mll
 * In class:
 *    Professor Peter Michael Osera
 *)
{
  open Lexing
  open Parser

  exception Lexer_error of string

  let symbols : (string * Parser.token) list = 
  [("(", LPAREN);
   (")", RPAREN);
   ("+", PLUS);
   ("-", MINUS);
   ("*", TIMES);
   ("/", FSLASH);
   ("if", IF);
   ("then", THEN);
   ("else", ELSE);
   ("<=", LEQ);
   ("let", LET);
   ("=", EQUAL);
   ("in", IN);
   ("fun", FUN);
   ("->", ARROW);
   ("fix", FIX)
  ]

  let create_symbol lexbuf =
    let str = lexeme lexbuf in 
    List.assoc str symbols
}
  let newline     = '\n' | ('\r' '\n') | '\r'
  let whitespace  = ['\t' ' ']
  let digit       = ['0'-'9'] 
  let boolean     = "true" | "false"
  let symbol      = '(' | ')' | '+' | '-' | '*' | '/' | "if" | "then" | "else" | "<=" | "let" | "=" | "in" | "fun" | "->" | "fix"
  let name        = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

  rule token = parse 
  | eof                     { EOF }
  | whitespace+ | newline+  { token lexbuf }
  | digit+                  { INT  (int_of_string (lexeme lexbuf)) }
  | boolean                 { BOOL (bool_of_string (lexeme lexbuf)) }
  | symbol                  { create_symbol lexbuf }
  | name                    { NAME (lexeme lexbuf) }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }