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
type lit =
  | LInt of int 
  | LBool of bool 

type value =
  | VLit of lit                   (*A literal value*)
  | VFun of exp * exp             (*A function value*)
and exp = 
  | EVal       of value
  | EVar       of string          (*Variable named string*)      
  | EIf        of exp * exp * exp (*If Statment*)
  | ELeqInt    of exp * exp       (*Integer Comparison: Less-than-or-equals*)
  | EAddInt    of exp * exp       (*Integer Addtion*)
  | ESubInt    of exp * exp       (*Integer Subtraction*)
  | EMultiInt  of exp * exp       (*Integer Multiplication*) 
  | EDivInt    of exp * exp       (*Integer Division*)   
  | ELet       of exp * exp * exp (*Let Binding*)
  | ERunFun    of exp * exp       (*Function call*)

(* This function substitutes every instance of s in e with v *)
let rec subst (v:value) (s:string) (e: exp) : exp =
  match e with
  | EVal v              ->  begin
                              match v with
                              | VLit v        -> EVal (VLit v)
                              | VFun (e1, e2) -> EVal (VFun (e1, (subst v s e2)))
                            end
  | EVar name           -> if (String.compare s name) = 0 then EVal v else e
  | EIf  (e1, e2, e3)   -> EIf ((subst v s e1), (subst v s e2), (subst v s e3))
  | ELeqInt   (e1, e2)  -> ELeqInt   ((subst v s e1), (subst v s e2))    
  | EAddInt   (e1, e2)  -> EAddInt   ((subst v s e1), (subst v s e2))
  | ESubInt   (e1, e2)  -> ESubInt   ((subst v s e1), (subst v s e2))
  | EMultiInt (e1, e2)  -> EMultiInt ((subst v s e1), (subst v s e2))
  | EDivInt   (e1, e2)  -> EDivInt   ((subst v s e1), (subst v s e2))
  | ELet  (e1, e2, e3)  -> ELet (e1, (subst v s e2), (subst v s e3))
  | ERunFun   (e1, e2)  -> ERunFun  ((subst v s e1), (subst v s e2))  

let rec string_of_exp (e:exp) : string = 
  match e with
  | EVal (VLit  (LInt n))  -> string_of_int  n     
  | EVal (VLit (LBool b)) -> string_of_bool b 
  | EVal (VFun (e1, e2))-> "(fun " ^ string_of_exp e1 ^ " -> " ^ string_of_exp e2 ^ ")"
  | EVar s              -> s
  | EIf   (e1, e2, e3)  -> "(if "  ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
  | ELeqInt   (e1, e2)  -> "("     ^ string_of_exp e1 ^ " <= "   ^ string_of_exp e2 ^ ")"
  | EAddInt   (e1, e2)  -> "("     ^ string_of_exp e1 ^ " + "    ^ string_of_exp e2 ^ ")"
  | ESubInt   (e1, e2)  -> "("     ^ string_of_exp e1 ^ " - "    ^ string_of_exp e2 ^ ")"
  | EMultiInt (e1, e2)  -> "("     ^ string_of_exp e1 ^ " * "    ^ string_of_exp e2 ^ ")"
  | EDivInt   (e1, e2)  -> "("     ^ string_of_exp e1 ^ " / "    ^ string_of_exp e2 ^ ")"
  | ELet   (v, e2, e3)  -> "(let " ^ string_of_exp v  ^ " = "    ^ string_of_exp e2 ^ " in " ^ string_of_exp e3 ^ ")"
  | ERunFun   (e1, e2)  -> "("     ^ string_of_exp e1 ^ " "      ^ string_of_exp e2 ^ ")"  

let string_of_value (v:value) : string =
  match v with
  | VLit (LInt  n)      -> string_of_int  n
  | VLit (LBool b)      -> string_of_bool b
  | VFun (e1, e2)       -> "(fun " ^ (string_of_exp e1) ^ " -> " ^ (string_of_exp e2) ^ ")" 

let value_of_int  (n:int)  : value = VLit (LInt  n)

let value_of_bool (b:bool) : value = VLit (LBool b)

let int_of_value  (v:value) : int =
  match v with
  | VLit (LInt  n)      -> n 
  | _                   -> failwith "Unexpected Value Provided to become int: Not a VLit"
let bool_of_value (v:value) : bool =
  match v with
  | VLit (LBool b)      -> b 
  | _                   -> failwith "Unexpected Value Provided to become bool: Not a VLit"

let rec interpret (e:exp) : value =
  match e with
  | EVal v              -> v
  | EVar s              -> failwith "Unexpected Variable Name in Value Interpretation"
  | EIf   (e1, e2, e3)  -> if bool_of_value (interpret e1) then (interpret e2) else (interpret e3)
  | ELeqInt   (e1, e2)  -> value_of_bool(int_of_value (interpret e1) <= int_of_value (interpret e2))
  | EAddInt   (e1, e2)  -> value_of_int (int_of_value (interpret e1) +  int_of_value (interpret e2))
  | ESubInt   (e1, e2)  -> value_of_int (int_of_value (interpret e1) -  int_of_value (interpret e2))
  | EMultiInt (e1, e2)  -> value_of_int (int_of_value (interpret e1) *  int_of_value (interpret e2))
  | EDivInt   (e1, e2)  -> let denominator = int_of_value(interpret e2) in
                            if denominator != 0 then
                            value_of_int(int_of_value (interpret e1) / denominator)
                            else 
                              failwith "Dividing by zero is bad"
  | ELet  (EVar e1, e2, e3)  -> interpret (subst (interpret e2) e1 e3) 
  | ERunFun   (e1, e2)  -> begin
                            match (interpret e1) with
                            | VFun ((EVar v), f) -> interpret (subst (interpret e2) v f)
                            | _                  -> failwith "Incorrect Function Syntax"
                           end
  | _ as e              -> failwith ("Unexpected Expresion " ^ string_of_exp e)