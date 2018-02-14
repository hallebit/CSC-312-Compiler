(* Resources
 * 
 * On Commandline Arguments:
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
 *    https://rosettacode.org/wiki/Command-line_arguments#Using_the_Arg_module
 *    https://stackoverflow.com/questions/18704894/using-ocaml-args-in-program
 *    http://scylardor.fr/2013/10/14/ocaml-parsing-a-programs-arguments-with-the-arg-module/
 * On Strings: 
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html
 * In class:
 *    Professor Peter Michael Osera
 *)

(*Determines if args or length of args is printed*)
let lenbool = ref false

(*Specifies the flags that can be parsed and what they do*)
let speclist = [
    ("-length", Arg.Set lenbool, "prints the lengths of each of the arguments")
  ]

(*Prints arg or the arg's length depending on lenbool*)
let str_or_int arg =
  if !lenbool then (Printf.printf "%i\n" (String.length arg)) 
  else (Printf.printf "%s\n" arg) 

(*Specifies commandline argument usage*)
  let usage = "Usage: bareBonesProj [flags] [args]"

let main () =
  Arg.parse
    speclist
    str_or_int
    usage

let _ = main()