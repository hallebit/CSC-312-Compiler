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

type typ =
  | TInt                                (*int  type*)
  | TBool                               (*bool type*)
  | TFun       of typ * typ             (*A function type*)
  | TPair      of typ * typ             (*A Pair type*)
  | TUnit                               (*A Unit type*)
  | TRef       of typ                   (*A Ref type*)

type value =
  | VLit of lit                         (*A literal value*)
  | VFun of exp * typ * exp * typ       (*A function value*)
  | VFix of exp * exp * typ * exp * typ (*A recursive function value*)
  | VPair of exp * exp 
  | VUnit
and exp = 
  | EVal       of value
  | EVar       of string                (*Variable named string*)      
  | EIf        of exp * exp * exp       (*If Statment*)
  | ELeqInt    of exp * exp             (*Integer Comparison: Less-than-or-equals*)
  | EAddInt    of exp * exp             (*Integer Addtion*)
  | ESubInt    of exp * exp             (*Integer Subtraction*)
  | EMultiInt  of exp * exp             (*Integer Multiplication*) 
  | EDivInt    of exp * exp             (*Integer Division*)   
  | ELet       of exp * typ * exp * exp (*Let Binding*)
  | ERunFun    of exp * exp             (*Function call*)
  | EFirst     of exp                   (*First Element of Pair*)
  | ESecond    of exp                   (*Second Element of Pair*)

let rec string_of_typ (t:typ) : string =
  match t with
  | TInt                      -> "int"
  | TBool                     -> "bool"
  | TFun (t1, t2)             -> (string_of_typ t1) ^ " -> " ^ (string_of_typ t2)
  | TPair (t1, t2)            -> "(" ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2) ^ ")"
  | TUnit                     -> "()"
  
let rec string_of_exp (e:exp) : string = 
  match e with
  | EVal (VLit  (LInt n))                   -> string_of_int  n     
  | EVal (VLit (LBool b))                   -> string_of_bool b 
  | EVal (VFun (EVar x, xt, e, rt))         -> "(fun ("^ x ^ ":"  ^ string_of_typ xt ^ ") : " ^ string_of_typ rt ^ " -> "   ^ string_of_exp e ^ ")"
  | EVal (VFix (EVar f, EVar x, xt, e, rt)) -> "(fix " ^ f ^ " (" ^ x ^ ":" ^ string_of_typ xt ^ ") : " ^ string_of_typ rt ^ " -> "   ^ string_of_exp e ^ ")"
  | EVal (VPair (e1, e2))                   -> "("      ^ string_of_exp e1  ^ ", "     ^ string_of_exp e2 ^ ")"
  | EVal VUnit                              -> "()"
  | EVar s                                  -> s
  | EIf   (e1, e2, e3)                      -> "(if "   ^ string_of_exp e1  ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3 ^ ")"
  | ELeqInt   (e1, e2)                      -> "("      ^ string_of_exp e1  ^ " <= "   ^ string_of_exp e2 ^ ")"
  | EAddInt   (e1, e2)                      -> "("      ^ string_of_exp e1  ^ " + "    ^ string_of_exp e2 ^ ")"
  | ESubInt   (e1, e2)                      -> "("      ^ string_of_exp e1  ^ " - "    ^ string_of_exp e2 ^ ")"
  | EMultiInt (e1, e2)                      -> "("      ^ string_of_exp e1  ^ " * "    ^ string_of_exp e2 ^ ")"
  | EDivInt   (e1, e2)                      -> "("      ^ string_of_exp e1  ^ " / "    ^ string_of_exp e2 ^ ")"
  | ELet (EVar x, xt, e1, e2)               -> "(let "  ^ x ^ ":" ^ string_of_typ xt   ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
  | ERunFun   (e1, e2)                      -> "("      ^ string_of_exp e1 ^ " "       ^ string_of_exp e2 ^ ")"  
  | EFirst e1                               -> "(fst "  ^ string_of_exp e1 ^ ")"
  | ESecond e1                              -> "(snd "  ^ string_of_exp e1 ^ ")"


let string_of_value (v:value) : string =
  match v with
  | VLit (LInt  n)            -> string_of_int  n
  | VLit (LBool b)            -> string_of_bool b
  | VFun (EVar x, xt, e, rt)  -> "(fun (" ^ x ^ ":" ^ string_of_typ xt ^ ") : " ^ string_of_typ rt ^ " -> "   ^ string_of_exp e ^ ")" 
  | VUnit                     -> "()"
  | VPair (e1, e2)            -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"

let value_of_int  (n:int)  : value = VLit (LInt  n)

let value_of_bool (b:bool) : value = VLit (LBool b)

let int_of_value  (v:value) : int =
  match v with
  | VLit (LInt  n)      -> n 
  | _                   -> failwith (Printf.sprintf "Unexpected Value Provided to become int: Not a VLit")

let int_of_exp (e:exp) : int =
  match e with 
  | EVal (VLit (LInt n))-> n
  | _                   -> failwith (Printf.sprintf "Unexpected Exp Provided to become int: Not a EVal")

let bool_of_value (v:value) : bool =
  match v with
  | VLit (LBool b)      -> b 
  | _                   -> failwith (Printf.sprintf "Unexpected Value Provided to become bool: Not a VLit")  
  
let value_of_exp (e:exp) : value =
  match e with
  | EVal v              -> v
  | _                   -> failwith (Printf.sprintf "Unexpected Expression: Not a EVal")

let exp_is_value (e:exp) : bool =
  match e with
  | EVal _ -> true
  | _ -> false 

(* This function substitutes every instance of s in e with v *)
let rec subst (v:value) (s:string) (e: exp) : exp =
  match e with
  | EVal w                  ->  begin
                                  match w with
                                  | VLit l                   -> EVal (VLit l)
                                  | VFun (EVar x, xt, e1, rt)-> if (compare x s) = 0 then 
                                                                  e 
                                                                else 
                                                                  EVal (VFun (EVar x, xt, (subst v s e1), rt))
                                  | VFix (EVar f, EVar x, xt, e1, rt)-> if (compare x s) = 0 then 
                                                                  e
                                                                else if (compare x s) = 0 then
                                                                  e
                                                                else 
                                                                  EVal (VFix (EVar f, EVar x, xt, (subst v s e1), rt))
                                  | VUnit                    -> e 
                                  | VPair (e1, e2)              -> EVal (VPair ((subst v s e1), (subst v s e2)))
                                  | _                        -> failwith (Printf.sprintf "Substitution Error: Unexpected Value")
                                end
  | EVar name               -> if (String.compare s name) = 0 then EVal v else e
  | EIf   (e1, e2, e3)      -> EIf         ((subst v s e1), (subst v s e2), (subst v s e3))
  | ELeqInt   (e1, e2)      -> ELeqInt     ((subst v s e1), (subst v s e2))    
  | EAddInt   (e1, e2)      -> EAddInt     ((subst v s e1), (subst v s e2))
  | ESubInt   (e1, e2)      -> ESubInt     ((subst v s e1), (subst v s e2))
  | EMultiInt (e1, e2)      -> EMultiInt   ((subst v s e1), (subst v s e2))
  | EDivInt   (e1, e2)      -> EDivInt     ((subst v s e1), (subst v s e2))
  | ELet  (x, xt, e1, e2)   -> ELet (x, xt, (subst v s e1), (subst v s e2))
  | ERunFun   (e1, e2)      -> ERunFun     ((subst v s e1), (subst v s e2))
  | EFirst    e1            -> EFirst       (subst v s e1)
  | ESecond   e1            -> ESecond      (subst v s e1)  

let rec check_equals (t1: typ) (t2: typ) : bool =
  match t1, t2 with 
  | TInt,  TInt                   -> true
  | TBool, TBool                  -> true
  | TFun (x1, x2), TFun (y1, y2)  -> (check_equals x1 y1) && (check_equals x2 y2)
  | TUnit, TUnit                  -> true
  | TPair (x1, x2), TPair (y1, y2)-> (check_equals x1 y1) && (check_equals x2 y2)
  | _                             -> false

let rec typecheck (ctx: (string * typ) list) (e: exp) : typ =
  match e with
  | EVal (VLit (LInt  _))                     -> TInt
  | EVal (VLit (LBool _))                     -> TBool
  | EVal (VFun (EVar x, xt, ex, rt))          -> if (check_equals 
                                                        (typecheck ((x, xt) :: ctx) ex) 
                                                        rt) then
                                                    TFun (xt, rt)
                                                 else 
                                                    failwith (Printf.sprintf "Fun Typecheck Fail. %s Found: %s Expected: %s" (string_of_exp e) (string_of_typ (typecheck ((x, xt) :: ctx) ex)) (string_of_typ rt))
  | EVal (VFix (EVar f, EVar x, xt, ex, rt))  -> if (check_equals  
                                                      (typecheck ((f, rt) :: (x, xt) :: ctx) ex)
                                                      rt) then
                                                    TFun (xt, rt)
                                                 else
                                                    failwith (Printf.sprintf "Fix Typecheck Fail. %s Found: Expected: %s" (string_of_exp e) (string_of_typ rt))
  | EVal (VUnit)                              -> TUnit
  | EVal (VPair (e1, e2))                     -> TPair ((typecheck ctx e1), (typecheck ctx e2))
  | EVar s                                    -> List.assoc s ctx
  | EIf (e1, e2, e3)                          -> begin
                                                  let typechkd = (typecheck ctx e2) in
                                                    if (check_equals typechkd (typecheck ctx e3)) &&
                                                       (check_equals (typecheck ctx e1) TBool) then
                                                        typechkd
                                                    else 
                                                      failwith (Printf.sprintf "If Typecheck Fail. %s Expected: %s with %s" (string_of_exp e) (string_of_typ TBool) (string_of_typ typechkd))
                                                end
  | ELeqInt   (e1, e2)                        ->  if (check_equals (typecheck ctx e1) TInt) &&
                                                     (check_equals (typecheck ctx e2) TInt) then
                                                      TBool
                                                  else 
                                                      failwith (Printf.sprintf "Less-than-or-equals Typecheck Fail. %s Expected: %s" (string_of_exp e) (string_of_typ TBool)) 
  | EAddInt   (e1, e2)                        ->  if (check_equals (typecheck ctx e1) TInt) &&
                                                     (check_equals (typecheck ctx e2) TInt) then
                                                      TInt
                                                  else
                                                      failwith (Printf.sprintf "Addition Typecheck Fail. %s Expected: %s" (string_of_exp e) (string_of_typ TInt)) 
  | ESubInt   (e1, e2)                        ->  if (check_equals (typecheck ctx e1) TInt) &&
                                                     (check_equals (typecheck ctx e2) TInt) then
                                                      TInt
                                                  else
                                                      failwith (Printf.sprintf "Subtraction Typecheck Fail. %s Expected: %s" (string_of_exp e) (string_of_typ TInt))
  | EMultiInt (e1, e2)                        ->  if (check_equals (typecheck ctx e1) TInt) &&
                                                     (check_equals (typecheck ctx e2) TInt) then
                                                      TInt
                                                  else
                                                      failwith (Printf.sprintf "Multiplication Typecheck Fail. %s Expected: %s" (string_of_exp e) (string_of_typ TInt))
  | EDivInt   (e1, e2)                        ->  if (check_equals (typecheck ctx e1) TInt) &&
                                                     (check_equals (typecheck ctx e2) TInt) then
                                                      TInt
                                                  else
                                                      failwith (Printf.sprintf "Division Typecheck Fail. %s Expected: %s" (string_of_exp e) (string_of_typ TInt))
  | ELet (EVar x, xt, e1, e2)                 -> begin
                                                  let typechkd = (typecheck ((x, xt) :: ctx) e2) in
                                                    if (check_equals (typecheck ctx e1) xt) then
                                                      typechkd
                                                    else 
                                                      failwith (Printf.sprintf "Let Typecheck Fail. %s Expected: %s" (string_of_exp e) (string_of_typ typechkd))
                                                end
  | ERunFun (e1, e2)                          -> begin 
                                                 match (typecheck ctx e1) with 
                                                 | TFun (t1, t2)      -> if (check_equals t1 (typecheck ctx e2)) then
                                                                           t2
                                                                         else 
                                                                           failwith (Printf.sprintf "Function Excecution Typecheck Fail. %s Found: %s Expected: %s" (string_of_exp e) (string_of_typ (typecheck ctx e2)) (string_of_typ t1))
                                                 | TInt               -> TInt
                                                 | _                  -> failwith (Printf.sprintf "Function Excecution Typecheck Fail. %s Found: %s Expected: type TFun" (string_of_exp e1) (string_of_typ (typecheck ctx e1)))
                                                 end
  | EFirst ex                                 -> begin  
                                                 match (typecheck ctx ex) with
                                                 | TPair (t1, t2)     -> t1
                                                 | _                  -> failwith (Printf.sprintf "First Element Pair Typecheck Fail. %s is not a Pair" (string_of_exp e))
                                                 end
  | ESecond ex                                -> begin 
                                                 match (typecheck ctx ex) with
                                                 | TPair (t1, t2)     -> t2
                                                 | _                  -> failwith (Printf.sprintf "First Element Pair Typecheck Fail. %s is not a Pair" (string_of_exp e))
                                                 end

let rec step (e:exp) : exp = 
  match e with
  | EVal v                      -> e
  | EVar s                      -> failwith (Printf.sprintf "Unbound variable: %s" s) 
  | EIf (e1, e2, e3)            -> if not (exp_is_value e1) then 
                                      EIf ((step e1), e2, e3)
                                   else 
                                      if (bool_of_value (value_of_exp e1)) then
                                        (step e2)
                                      else
                                        (step e3)
  | ELeqInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> ELeqInt ((step e1), e2)
                                   | false, true  -> ELeqInt (e1, (step e2))
                                   | false, false -> EVal (VLit (LBool ((int_of_exp e1) <= (int_of_exp e2))))
                                  end
  | EAddInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> EAddInt ((step e1), e2)
                                   | false, true  -> EAddInt (e1, (step e2))
                                   | false, false -> EVal (VLit (LInt ((int_of_exp e1) + (int_of_exp e2))))
                                  end
  | ESubInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> ESubInt ((step e1), e2)
                                   | false, true  -> ESubInt (e1, (step e2))
                                   | false, false -> EVal (VLit (LInt ((int_of_exp e1) - (int_of_exp e2))))
                                  end
  | EMultiInt (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> EMultiInt ((step e1), e2)
                                   | false, true  -> EMultiInt (e1, (step e2))
                                   | false, false -> EVal (VLit (LInt ((int_of_exp e1) * (int_of_exp e2))))
                                  end
  | EDivInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> EDivInt ((step e1), e2)
                                   | false, true  -> EDivInt (e1, (step e2))
                                   | false, false -> let denominator = (int_of_exp e2) in
                                                      if (denominator = 0) then 
                                                        failwith (Printf.sprintf "Dividing by zero is bad")
                                                      else 
                                                        EVal (VLit (LInt ((int_of_exp e1) / denominator)))
                                   end
  | ELet (EVar x, xt, e1, e2)   -> if (not (exp_is_value e1)) then 
                                      ELet (EVar x, xt, (step e1), e2)
                                    else 
                                      subst (value_of_exp e1) x e2
  | ERunFun (e1, e2)            -> if not (exp_is_value e2) then 
                                      ERunFun (e1, (step e2))
                                    else
                                      match e1 with
                                      | ERunFun _                               -> ERunFun ((step e1), e2)
                                      | EVal (VFun (EVar x, xt, e, rt))         -> subst (value_of_exp e2) x e
                                      | EVal (VFix (EVar f, EVar x, t1, e, t2)) -> subst (value_of_exp e1) f (subst (value_of_exp e2) x e)
                                      | _                                       -> failwith (Printf.sprintf "Interpretation: issue with function formatting")
  | EFirst  ex                  -> begin
                                   match ex with 
                                   | EVal (VPair (e1, e2)) -> e1
                                   | _ as e                -> failwith (Printf.sprintf "%s" (string_of_exp e))
                                   end
  | ESecond ex                  -> begin
                                   match ex with 
                                   | EVal (VPair (e1, e2)) -> e2
                                   | _ as e                -> failwith (Printf.sprintf "%s" (string_of_exp e))
                                   end
 
  | _ as e                               -> failwith (Printf.sprintf "%s" (string_of_exp e))

let evaluate (e:exp) : value =
  let rec eval (e':exp) : exp =
    match e' with
    | EVal _    -> e'
    | _         -> eval(step e') in
  let typechkd = typecheck [] e in
  value_of_exp (eval e)    