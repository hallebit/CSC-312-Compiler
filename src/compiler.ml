(* This file is the driver of the Compiler. 
 * To run the compiler on a file, pass the file name as the first command line argument.
 * 
 *
 * Resources
 * 
 * On General Format:
 *    https://github.com/psosera/csc312-example-compiler/commit/1ffe16af8d5b02a8e167ae3f4b23a9b8f0e92eb6 
 * On understanding General Format:
 *    https://stackoverflow.com/questions/11515240/whats-the-difference-between-let-and-let 
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/Sys.html 
 *    https://caml.inria.fr/pub/docs/manual-ocaml/toplevel.html
 * In class:
 *    Professor Peter Michael Osera
 *)
 let print_lex       = ref false
 let print_parse     = ref false

 let specialist = [
   ("-lex", Arg.Set print_lex, "processes the input source file through the lexing phase and prints the resulting stream of tokens");
   ("-parse", Arg.Set print_parse, "processes the input source file through the parsing phase and prints the resulting abstract syntax tree")
 ]

 let usage = "Usage: ./compiler [flags] [args]"

 let driver (filename:string) =
  let tokens = Lexer.lex (Stream.of_channel (open_in filename)) in
  if !print_lex then
    tokens 
    |> Lexer.string_of_token_list
    |> print_string  
  else if !print_parse then
    let (e, _) = Parser.parse tokens in
    Lang.string_of_exp e |> print_string
  else 
    let (e, _) = Parser.parse tokens in
    Lang.interpret e |> Lang.string_of_value |> print_endline

 let main () =
  Arg.parse
    specialist
    driver
    usage

let _ = if !Sys.interactive then () else main()