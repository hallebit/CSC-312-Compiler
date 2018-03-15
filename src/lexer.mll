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
   ("/", FSLASH)
  ]

  let keyphrases : (string * Parser.token) list =
  [("true", TRUE);
   ("false", FALSE);
   ("if", IF);
   ("then", THEN);
   ("else", ELSE);
   ("<=", LEQ)
  ]

  let create_symbol lexbuf =
    let str = lexeme lexbuf in 
    List.assoc str symbols

  let create_keyphrase lexbuf =
    let str = lexeme lexbuf in 
    List.assoc str keyphrases
  
  let create_int lexbuf = lexeme lexbuf |> int_of_string
}
  let newline     = '\n' | ('\r' '\n') | '\r'
  let whitespace  = ['\t' ' ']
  let digit       = ['0'-'9'] 
  let symbol      = '(' | ')' | '+' | '-' | '*' | '/' 
  let keyphrase   = "true" | "false" | "if" | "then" | "else" | "<="

  rule token = parse 
  | eof                     { EOF }
  | digit+                  { INT (int_of_string (lexeme lexbuf)) }
  | whitespace+ | newline+  { token lexbuf }
  | symbol                  { create_symbol lexbuf }
  | keyphrase               { create_keyphrase lexbuf }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }