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
 open List

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
  | VPair of exp * exp                  (*A Pair value*)
  | VUnit                               (*A Unit value*)
  | VPtr of int                         (*A Pointer value*)
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
  | ERef       of exp                   (*Ref Initialization*)
  | EAssign    of exp * exp             (*Ref Assignment*)
  | EBang      of exp                   (*Dererencing*)

let rec string_of_typ (t:typ) : string =
  match t with
  | TInt                      -> "int"
  | TBool                     -> "bool"
  | TFun (t1, t2)             -> (string_of_typ t1) ^ " -> " ^ (string_of_typ t2)
  | TPair (t1, t2)            -> "(" ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2) ^ ")"
  | TUnit                     -> "()"
  | TRef t1                   -> "<" ^ (string_of_typ t1) ^ ">"
  
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
  | ERef e1                                 -> "ref("   ^ string_of_exp e1 ^ ")"
  | EAssign   (e1, e2)                      -> "("      ^ string_of_exp e1 ^ " := "    ^ string_of_exp e2 ^ ")"
  | EBang e1                                -> "!"      ^ string_of_exp e1 
  | _                                       -> failwith (Printf.sprintf "Error. Unable to make String of exp")  

let string_of_value (v:value) : string =
  match v with
  | VLit (LInt  n)                    -> string_of_int  n
  | VLit (LBool b)                    -> string_of_bool b
  | VFun (EVar x, xt, e, rt)          -> "(fun (" ^ x ^ ":" ^ string_of_typ xt ^ ") : " ^ string_of_typ rt ^ " -> "   ^ string_of_exp e ^ ")" 
  | VFix (EVar f, EVar x, xt, e1, rt) -> "(fix " ^ f ^ "(" ^ x ^ ":" ^ (string_of_typ xt) ^ "): " ^ (string_of_typ rt) ^ "->" ^ (string_of_exp e1) ^ ")"
  | VUnit                             -> "()"
  | VPair (e1, e2)                    -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | VPtr n                            -> "(ptr, address: " ^ string_of_int n ^ ")"
  | _                                 -> failwith (Printf.sprintf "Unexpected Value to convert to String")


let value_of_int  (n:int)  : value = VLit (LInt  n)

let value_of_bool (b:bool) : value = VLit (LBool b)

let int_of_value  (v:value) : int =
  match v with
  | VLit (LInt  n)      -> n 
  | _                   -> failwith (Printf.sprintf "Unexpected Value Provided to become int: Not a VLit")

let int_of_exp (e:exp) : int =
  match e with 
  | EVal (VLit (LInt n))-> n
  | _                   -> failwith (Printf.sprintf "Unexpected Exp Provided to become int: Not a EVal.")

let bool_of_value (v:value) : bool =
  match v with
  | VLit (LBool b)      -> b 
  | _                   -> failwith (Printf.sprintf "Unexpected Value Provided to become bool: Not a VLit")  
  
let value_of_exp (e:exp) : value =
  match e with
  | EVal v              -> v
  | _                   -> failwith (Printf.sprintf "Unexpected Expression: Not a EVal Recieved %s" (string_of_exp e))

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
                                  | VPtr p                   -> e
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
  | ERef      e1            -> ERef         (subst v s e1)
  | EAssign   (e1, e2)      -> EAssign     ((subst v s e1), (subst v s e2)) 
  | EBang     e1            -> EBang        (subst v s e1)

let rec check_equals (t1: typ) (t2: typ) : bool =
  match t1, t2 with 
  | TInt,  TInt                   -> true
  | TBool, TBool                  -> true
  | TFun (x1, x2), TFun (y1, y2)  -> (check_equals x1 y1) && (check_equals x2 y2)
  | TUnit, TUnit                  -> true
  | TPair (x1, x2), TPair (y1, y2)-> (check_equals x1 y1) && (check_equals x2 y2)
  | TRef x, TRef y                -> (check_equals x y)
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
  | EVal (VPtr l)                             -> failwith("Ptr Typecheck Fail. Ptr should not be found.")
  | EVar s                                    -> List.assoc s ctx
  | ERef ex                                   -> TRef (typecheck ctx ex)
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
  | ELet (x, xt, e1, e2)                      ->  if (check_equals (typecheck ctx e1) xt) then
                                                    begin 
                                                      match x with 
                                                      | EVar n        -> (typecheck ((n, xt) :: ctx) e2)
                                                      | _             -> failwith (Printf.sprintf "Unexpected let name expression: %s" (string_of_exp x))
                                                    end
                                                  else 
                                                    failwith (Printf.sprintf "Let Typecheck Fail. %s Expected: %s" (string_of_exp e1) (string_of_typ xt))
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
  | EAssign (e1, e2)                          -> begin 
                                                 match (typecheck ctx e1), (typecheck ctx e2) with
                                                 | TRef t1, t2        -> if (check_equals t1 t2) then
                                                                            TUnit
                                                                         else 
                                                                            failwith (Printf.sprintf "Assignment Typecheck Fail. Types do not match")
                                                 | t, _               -> failwith (Printf.sprintf "Assignment Typecheck Fail. Assignment requires a ref operator")
                                                 end
  | EBang ex                                  -> begin
                                                 let ex_typ = (typecheck ctx ex) in  
                                                 match ex_typ with
                                                 | TRef t             -> t
                                                 | _                  -> failwith (Printf.sprintf "Dereferencing Typecheck Fail. Dereferencing requires a ref operator. recieved type %s" ((string_of_typ ex_typ) ^ " from Expression " ^ (string_of_exp ex)))
                                                 end 
  | _ as ex                                   -> failwith (Printf.sprintf "Typecheck unexpected input %s" (string_of_exp ex))

let rec step (env: (int * value) list) (e:exp) : (int * value) list * exp = 
  match e with
  | EVal v                      -> (env, e)
  | EVar s                      -> failwith (Printf.sprintf "Unbound variable: %s" s) 
  | EIf (e1, e2, e3)            -> if not (exp_is_value e1) then 
                                    begin
                                      match (step env e1) with 
                                      | en, e     -> (en, EIf (e, e2, e3))
                                    end
                                   else 
                                      if (bool_of_value (value_of_exp e1)) then
                                        (step env e2)
                                      else
                                        (step env e3)
  | ELeqInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> begin match (step env e1) with
                                                     | en, e        -> (env, ELeqInt (e, e2))
                                                     end
                                   | false, true  -> begin match (step env e2) with
                                                     | en, e        -> (env, ELeqInt (e1, e))
                                                     end
                                   | false, false -> (env, EVal (VLit (LBool ((int_of_exp e1) <= (int_of_exp e2)))))
                                  end
  | EAddInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> begin match (step env e1) with
                                                     | en, e        -> (env, EAddInt (e, e2))
                                                     end
                                   | false, true  -> begin match (step env e2) with
                                                     | en, e        -> (env, EAddInt (e1, e))
                                                     end
                                   | false, false -> (env, EVal (VLit (LInt ((int_of_exp e1) + (int_of_exp e2)))))
                                  end
  | ESubInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> begin match (step env e1) with
                                                     | en, e        -> (env, ESubInt (e, e2))
                                                     end
                                   | false, true  -> begin match (step env e2) with
                                                     | en, e        -> (env, ESubInt (e1, e))
                                                     end
                                   | false, false -> (env, EVal (VLit (LInt ((int_of_exp e1) - (int_of_exp e2)))))
                                  end
  | EMultiInt (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> begin match (step env e1) with
                                                     | en, e        -> (env, EMultiInt (e, e2))
                                                     end
                                   | false, true  -> begin match (step env e2) with
                                                     | en, e        -> (env, EMultiInt (e1, e))
                                                     end
                                  | false, false -> (env, EVal (VLit (LInt ((int_of_exp e1) * (int_of_exp e2)))))
                                  end
  | EDivInt   (e1, e2)          -> begin match (not (exp_is_value e1)), (not (exp_is_value e2)) with
                                   | true, _      -> begin match (step env e1) with
                                                     | en, e        -> (env, EDivInt (e, e2))
                                                     end
                                   | false, true  -> begin match (step env e2) with
                                                     | en, e        -> (env, EDivInt (e1, e))
                                                     end
                                   | false, false -> let denominator = (int_of_exp e2) in
                                                      if (denominator = 0) then 
                                                        failwith (Printf.sprintf "Dividing by zero is bad")
                                                      else 
                                                        (env, EVal (VLit (LInt ((int_of_exp e1) / denominator))))
                                   end
  | ELet (EVar x, xt, e1, e2)   -> if (not (exp_is_value e1)) then 
                                    begin 
                                      match (step env e1) with
                                      | en, e      -> (en, ELet (EVar x, xt, e, e2))
                                    end 
                                    else 
                                      (env, (subst (value_of_exp e1) x e2))
  | ERunFun (e1, e2)            -> if not (exp_is_value e2) then
                                      begin
                                        match (step env e2) with
                                        | en, ex2   -> (en, ERunFun (e1, ex2))
                                      end
                                    else
                                      begin
                                        match e1 with
                                        | ERunFun _                               -> begin 
                                                                                      match (step env e1) with 
                                                                                      | en, e  -> (en, ERunFun (e, e2))
                                                                                      end 
                                        | EVal (VFun (EVar x, xt, e, rt))         -> (env, (subst (value_of_exp e2) x e))
                                        | EVal (VFix (EVar f, EVar x, t1, e, t2)) -> (env, (subst (value_of_exp e1) f (subst (value_of_exp e2) x e)))
                                        | _                                       -> failwith (Printf.sprintf "Interpretation: issue with function formatting")
                                      end
  (*| EFirst  ex                  -> begin
                                   match ex with 
                                   | EVal (VPair (e1, e2)) -> (env, e1)
                                   | _ as e                -> failwith (Printf.sprintf "%s" (string_of_exp e))
                                   end
  | ESecond ex                  -> begin
                                   match ex with 
                                   | EVal (VPair (e1, e2)) -> (env, e2)
                                   | _ as e                -> failwith (Printf.sprintf "%s" (string_of_exp e))
                                   end *)
  | ERef ex                     -> if (not (exp_is_value ex)) then
                                      begin
                                        match (step env ex) with
                                        | en, e            -> (en, ERef e)
                                      end
                                    else
                                      let envLen = (length env) in
                                        ((cons (envLen, (value_of_exp ex)) env), EVal (VPtr envLen))
  | EAssign (e1, e2)            -> if (not (exp_is_value e1)) then
                                    begin 
                                      match (step env e1) with
                                      | en, e               -> (en, EAssign (e, e2))
                                    end
                                   else if (not (exp_is_value e2)) then 
                                      begin
                                        match (step env e2) with
                                        | en, e             -> (en, EAssign (e1, e))
                                      end
                                   else 
                                      begin 
                                        match ((value_of_exp e1), (value_of_exp e2)) with
                                        | VPtr l, v         -> ((cons (l, v) env), EVal (VUnit))
                                        | v, _              -> failwith(Printf.sprintf "Typechecking skipped := at %s" (string_of_exp e1))
                                      end
  | EBang ex                    -> if (not (exp_is_value ex)) then
                                    begin 
                                      match (step env ex) with
                                      | en, e               -> (en, EBang e)
                                    end
                                   else
                                    begin 
                                      match (value_of_exp ex) with
                                      | VPtr l              -> (env, EVal (assoc l env))
                                      | _                   -> failwith (Printf.sprintf "Typechecking skipped ! near %s" (string_of_exp ex))
                                    end
  | _ as e                      -> failwith (Printf.sprintf "%s" (string_of_exp e))

let evaluate (e:exp) : value =
  let rec eval (env: (int * value) list) (e':exp) : exp =
    if (exp_is_value e') then e' else   
      begin
        match (step env e') with
        | en, e1    -> (eval en e1) 
      end 
    in let typechkd = typecheck [] e 
    in let simplified = (eval [] e) 
    in (value_of_exp simplified)   