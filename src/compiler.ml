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

 let main() =
  (* The command line arguments given to the process. 
   * The first element is the command name used to invoke the program. 
   * The following elements are the command-line arguments given to the program.
   *)
  let filename = Sys.argv.(1) in
  let tokens = Lexer.lex (Stream.of_channel (open_in filename)) in
  let (e, _) = Parser.parse tokens in
  (* Note that the |> operater take the result of the left-hand side
   * and feeds it as an argument to the function on the right-hand side
   *)
  Lang.interpret e |> string_of_int |> print_endline

(* Remember '_' means "I know there is something here and
 *                     I explicitly say I will not use it, 
 *                     so I don't name it."
 * Remember '()' is the symbol for unit 
 * Note that Sys.interactive is a reference that is 
 *                     initially set to fals in standalone programs
 *                     and to true if the code is being executed under
 *                     the interactive toplevel system ocaml. 
 *)
let _ = if !Sys.interactive then () else main()