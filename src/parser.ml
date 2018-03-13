(* This file organizes the way that we translate tokens into expresions.
 *
 * Resources
 * 
 * On General Format:
 *    https://github.com/psosera/csc312-example-compiler/commit/1ffe16af8d5b02a8e167ae3f4b23a9b8f0e92eb6 
 * On understanding General Format:
 *    https://ocaml.org/learn/taste.html
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html 
 *    https://stackoverflow.com/questions/257605/ocaml-match-expression-inside-another-one 
 *    https://github.com/yashdavisgupta/Slang/commit/56270b7598333ee26db4d9f7b4ee2b8f976a65db
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html 
 * 
 * In class:
 *    Professor Peter Michael Osera
 *)

open Lang
open Lexer

(* Note that List.hd returns the first element of the given list
 *                   and raises Failure "hd" if the list is empty 
 *)
let rec peek : token list -> token = List.hd
(* Note that List.tl returns the given list without its first element 
 *                   and raises Failure "t1" if the list is empty
 *)
let rec advance : token list -> token list = List.tl

(* This procedure is used to check if the program uses the expected expression format*)
let rec consume (t:token) (toks:token list) : token list = 
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else 
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else  
    match peek toks with
    | TInt n  -> (EInt n, advance toks)
    | TBool b -> (EBool b, advance toks)
    | TLParen -> begin
      let toks        = consume TLParen toks in
      let operator    = peek toks in
      let toks        = match operator with
                        | TIf     -> consume TIf      toks
                        | TLeq    -> consume TLeq     toks
                        | TPlus   -> consume TPlus    toks
                        | TMinus  -> consume TMinus   toks
                        | TMulti  -> consume TMulti   toks
                        | TDivide -> consume TDivide  toks
                        | t       -> failwith (Printf.sprintf "Unexpected operation token found: %s" (string_of_token t))
      in
      if operator = TIf then
        let (e1, toks) = parse toks in
        let (e2, toks) = parse toks in
        let (e3, toks) = parse toks in 
        let toks       = consume TRParen toks in
        (EIf (e1, e2, e3), toks)
      else 
        let (e1, toks) = parse toks in
        let (e2, toks) = parse toks in 
        let toks       = consume TRParen toks in
        match operator with
        | TLeq    -> (ELeqInt     (e1, e2), toks);
        | TPlus   -> (EAddInt     (e1, e2), toks);
        | TMinus  -> (ESubInt     (e1, e2), toks);
        | TMulti  -> (EMultiInt   (e1, e2), toks);
        | TDivide -> (EDivInt  (e1, e2), toks);
        | t       -> failwith (Printf.sprintf "Unexpected operation token found: %s" (string_of_token t))
    end
    | t -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))



