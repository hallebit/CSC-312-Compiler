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

type value =
  | VInt      of int 
  | VBool     of bool
 
type exp = 
  | EInt       of int 
  | EBool      of bool 
  | ELeqInt    of exp * exp     (*Integer Comparison: Less-than-or-equals*)
  | EAddInt    of exp * exp     (*Integer Addtion*)
  | ESubInt    of exp * exp     (*Integer Subtraction*)
  | EMultiInt  of exp * exp     (*Integer Multiplication*) 
  | EDivInt    of exp * exp     (*Integer Division*) 

let value_of_int  (n:int)  : value = VInt n
let value_of_bool (b:bool) : value = VBool b
let int_of_value  (v:value): int =
  match v with
  | VInt n  -> n 
  | _       -> failwith "Unexpected Value Provided: Not a VInt"
let bool_of_value (v:value): bool =
  match v with
  | VBool b -> b
  | _       -> failwith "Unexpected Value Provided: Not a VBool"

let rec interpret (e:exp) : value =
  match e with
  | EInt n             -> VInt n  
  | EBool b            -> VBool b
  | ELeqInt   (e1, e2) -> value_of_bool(int_of_value (interpret e1) <= int_of_value (interpret e2))
  | EAddInt   (e1, e2) -> value_of_int(int_of_value (interpret e1) + int_of_value (interpret e2))
  | ESubInt   (e1, e2) -> value_of_int(int_of_value (interpret e1) - int_of_value (interpret e2))
  | EMultiInt (e1, e2) -> value_of_int(int_of_value (interpret e1) * int_of_value (interpret e2))
  | EDivInt   (e1, e2) -> let denominator = int_of_value(interpret e2) in
                              if denominator != 0 then
                              value_of_int(int_of_value (interpret e1) / denominator)
                              else 
                                failwith "Dividing by zero is bad"
(* Note that there are different operations for floating-point numbers.
 * Namely +., -., *., and /.
 * Thus, the code might look something like:
  | EFlt n        -> n
  | EAdd   (e1, e2) when e1 isa EFlt and e2 isa EFlt -> interpret e1 +. interpret e2*)