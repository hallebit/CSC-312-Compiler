(* This file is the driver of the Compiler. 
 * To run the compiler on a file, pass the file name as the first command line argument.
 * 
 *
 * Resources
 * 
 * On General Format:
 *    https://github.com/psosera/csc312-example-compiler/commit/1ffe16af8d5b02a8e167ae3f4b23a9b8f0e92eb6 
 * On understanding General Format:
 *    https://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html
 * In class:
 *    Professor Peter Michael Osera
 *)
 let print_lex       = ref false
 let print_parse     = ref false

 let specialist = [
   ("-parse", Arg.Set print_parse, "processes the input source file through the parsing phase and prints the resulting abstract syntax tree")
 ]

 let usage = "Usage: ./compiler.byte [flags] [args]"

 let driver (filename:string) =
  if !print_parse then
    filename
    |> open_in_bin
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.string_of_exp
    |> print_string
  else 
    filename
    |> open_in_bin
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.interpret
    |> Lang.string_of_value
    |> print_endline

 let main () =
  Arg.parse
    specialist
    driver
    usage

let _ = if !Sys.interactive then () else main()