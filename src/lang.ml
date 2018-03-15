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
  | VTrue
  | VFalse
 
type exp = 
  | EInt       of int 
  | ETrue      
  | EFalse      
  | EIf        of exp * exp * exp (*If Statment*)
  | ELeqInt    of exp * exp       (*Integer Comparison: Less-than-or-equals*)
  | EAddInt    of exp * exp       (*Integer Addtion*)
  | ESubInt    of exp * exp       (*Integer Subtraction*)
  | EMultiInt  of exp * exp       (*Integer Multiplication*) 
  | EDivInt    of exp * exp       (*Integer Division*)  

let string_of_value (v:value) : string =
  match v with
  | VInt n  -> string_of_int n
  | VTrue   -> "true"
  | VFalse  -> "false"

let rec string_of_exp (e:exp) : string = 
  match e with
  | EInt n                  -> string_of_int n     
  | ETrue                   -> string_of_bool true
  | EFalse                  -> string_of_bool false
  | EIf       (e1, e2, e3)  -> "(if " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ " " ^ string_of_exp e3 ^ ")"
  | ELeqInt   (e1, e2)      -> "(<= " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | EAddInt   (e1, e2)      -> "(+ "  ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | ESubInt   (e1, e2)      -> "(- "  ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | EMultiInt (e1, e2)      -> "(* "  ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | EDivInt   (e1, e2)      -> "(/ "  ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
    
let value_of_int  (n:int)  : value = VInt n
let value_of_bool (b:bool) : value = 
  match b with
  | true  -> VTrue
  | false -> VFalse
  | _       -> failwith "Unexpected Boolean Value Provided: Not true or false" 
let int_of_value  (v:value): int =
  match v with
  | VInt n  -> n 
  | _       -> failwith "Unexpected Value Provided: Not a VInt"
let bool_of_value (v:value): bool =
  match v with
  | VTrue   -> true
  | VFalse  -> false
  | _       -> failwith "Unexpected Value Provided: Not a VTrue or VFalse"

let rec interpret (e:exp) : value =
  match e with
  | EInt n                -> VInt n  
  | ETrue                 -> VTrue
  | EFalse                -> VFalse
  | EIf       (e1, e2, e3)-> if bool_of_value (interpret e1) then (interpret e2) else (interpret e3)
  | ELeqInt   (e1, e2)    -> value_of_bool(int_of_value (interpret e1) <= int_of_value (interpret e2))
  | EAddInt   (e1, e2)    -> value_of_int(int_of_value (interpret e1) + int_of_value (interpret e2))
  | ESubInt   (e1, e2)    -> value_of_int(int_of_value (interpret e1) - int_of_value (interpret e2))
  | EMultiInt (e1, e2)    -> value_of_int(int_of_value (interpret e1) * int_of_value (interpret e2))
  | EDivInt   (e1, e2)    -> let denominator = int_of_value(interpret e2) in
                              if denominator != 0 then
                              value_of_int(int_of_value (interpret e1) / denominator)
                              else 
                                failwith "Dividing by zero is bad"
(* Note that there are different operations for floating-point numbers.
 * Namely +., -., *., and /.
 * Thus, the code might look something like:
  | EFlt n        -> n
  | EAdd   (e1, e2) when e1 isa EFlt and e2 isa EFlt -> interpret e1 +. interpret e2*)