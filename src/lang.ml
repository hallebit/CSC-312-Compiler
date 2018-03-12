(* This file contains the rules for the language as it stands. 
 *
 * Resources
 * 
 * On General Format:
 *    https://github.com/psosera/csc312-example-compiler/commit/1ffe16af8d5b02a8e167ae3f4b23a9b8f0e92eb6 
 * On understanding General Format:
 *    https://ocaml.org/learn/taste.html 
 *    https://ocaml.org/learn/tutorials/modules.html
 https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html  
 * In class:
 *    Professor Peter Michael Osera
 *)

 (* The general form for pattern matching is
  *  | pattern   -> result
  *  | pattern   -> retult
  * The general form for declaring a type is as follows
  *   type pair_of_ints = {a : int; b : int };; 
  *)
type exp = 
  | EInt    of int 
  | EAddInt    of exp * exp     (*Integer Addtion*)
  | ESubInt    of exp * exp     (*Integer Subtraction*)
  | EMultiInt  of exp * exp     (*Integer Multiplication*) 
  | EDivInt    of exp * exp     (*Integer Division*) 

(* Example: Elementary function
 *  let rec fact x = 
 *    if x <= 1 then 1 else x * fact(x - 1);;
 *)
let rec interpret (e:exp) : int =
  match e with
  | EInt n             -> n 
  | EAddInt   (e1, e2) -> interpret e1 + interpret e2
  | ESubInt   (e1, e2) -> interpret e1 - interpret e2
  | EMultiInt (e1, e2) -> interpret e1 * interpret e2
  | EDivInt   (e1, e2) -> let denominator = interpret e2 in
                              if denominator != 0 then
                                interpret e1 / interpret e2
                              else 
                                failwith "Dividing by zero is bad"
(* Note that there are different operations for floating-point numbers.
 * Namely +., -., *., and /.
 * Thus, the code might look something like:
  | EFlt n        -> n
  | EAdd   (e1, e2) when e1 isa EFlt and e2 isa EFlt -> interpret e1 +. interpret e2*)