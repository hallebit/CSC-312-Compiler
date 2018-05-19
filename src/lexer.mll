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
   (":", COLON);
   ("fix", FIX);
   ("int", TINT);
   ("bool", TBOOL);
   ("()", UNIT);
   ("unit", TUNIT);
   (",", COMMA);
   ("fst", FIRST);
   ("snd", SECOND);
   ("ref", REF);
   ("<", OPENCARROT);
   (">", CLOSECARROT);
   (":=", ASSIGN);
   ("!", BANG);
   (";", SEMI);
   ("while", WHILE);
   ("do", DO);
   ("end", END)
  ]

  let create_symbol lexbuf =
    let str = lexeme lexbuf in 
    List.assoc str symbols

  let open_c : int ref = ref 0 
}
  let newline     = '\n' | ('\r' '\n') | '\r'
  let whitespace  = ['\t' ' ']
  let digit       = ['0'-'9']
  let open_c      = "\\*" 
  let close_c     = "*/" 
  let boolean     = "true" | "false"
  let symbol     = '(' | ')' | '+' | '-' | '*' | '/' | ':' | ',' | '!' | '<' | '>' | ';'
                    | "if" | "then" | "else" | "<=" | "let" | "=" | "in" | "fun" | "->" | "fix" 
                    | "int" | "bool" | "()" | "unit" | "fst" | "snd" | "ref" | ":=" | "while" 
                    | "do" | "end" 
  let name        = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*


  rule token = parse 
  | eof                     { EOF }
  | open_c                  { open_c := !open_c + 1; token lexbuf }
  | close_c                 { open_c := !open_c - 1; token lexbuf }
  | whitespace+ | newline+  { token lexbuf }
  | digit+                  { if !open_c > 0 then token lexbuf else INT (int_of_string (lexeme lexbuf)) }
  | boolean                 { if !open_c > 0 then token lexbuf else BOOL (bool_of_string (lexeme lexbuf)) }
  | symbol                  { if !open_c > 0 then token lexbuf else create_symbol lexbuf }
  | name                    { if !open_c > 0 then token lexbuf else NAME (lexeme lexbuf) }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }