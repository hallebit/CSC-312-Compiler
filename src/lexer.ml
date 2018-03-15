(* This file organizes the way that we read our langauge and turn it into tokens. 
 *
 * Resources
 * 
 * On General Format:
 *    https://github.com/psosera/csc312-example-compiler/commit/1ffe16af8d5b02a8e167ae3f4b23a9b8f0e92eb6 
 * On understanding General Format:
 *    https://ocaml.org/learn/taste.html
 *    https://ocaml.org/learn/tutorials/data_types_and_matching.html
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html    
 *    https://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html 
 * In class:
 *    Professor Peter Michael Osera
 *)

type token = 
  | TInt of int 
  | TBool of bool
  | TLParen
  | TRParen
  | TIf
  | TLeq
  | TPlus
  | TMinus
  | TMulti
  | TDivide

let string_of_token (t:token) : string = 
  match t with
  | TInt n  -> string_of_int n
  | TBool b -> string_of_bool b 
  | TLParen -> "("
  | TRParen -> ")"
  | TIf     -> "if"
  | TLeq    -> "<="
  | TPlus   -> "+"
  | TMinus  -> "-"
  | TMulti  -> "*"
  | TDivide -> "/" 

let string_of_token_list (toks:token list) : string =
  "[" ^ String.concat ", " (List.map string_of_token toks) ^ "]" 

let keywords = [("true", TBool true); ("false", TBool false); ("if", TIf); ("<=", TLeq)]

(* Peeks at the head of the stream without advancing it forward*)
let peek (src:char Stream.t) : char = 
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pops the head of the stream and returns it, advancing the stream forward*)
let advance : char Stream.t -> char = Stream.next

let is_empty (src:char Stream.t) : bool = 
  try 
    Stream.empty src; true
  with 
    Stream.Failure -> false

let is_whitespace (ch:char) : bool = 
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57 

(*This procedure returns true if the char is one of those used in a keyword*)
let is_keychar (ch:char) : bool = 
  let code = Char.code ch in
  (97 <= code && code <= 122) || (60 <= code && code <= 61)

let lex (src:char Stream.t) : token list = 
  let rec lex_num acc = 
    if is_digit (peek src) then
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      int_of_string acc
  in
  let rec lex_keyword acc = 
    if is_keychar (peek src) then
      lex_keyword (acc ^ (Char.escaped (advance src)))
    else if (List.mem_assoc acc keywords) then
      acc
    else
      failwith (Printf.sprintf "%s is not a known keyword" acc)
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
        match ch with 
        | '(' -> advance src |> ignore; TLParen :: go ()
        | ')' -> advance src |> ignore; TRParen :: go ()
        | '+' -> advance src |> ignore; TPlus   :: go ()
        | '-' -> advance src |> ignore; TMinus  :: go ()
        | '*' -> advance src |> ignore; TMulti  :: go ()
        | '/' -> advance src |> ignore; TDivide :: go ()
        | _   -> begin
          if is_whitespace ch then
            begin advance src |> ignore; go () end
          else if is_digit ch then
            let n = lex_num "" in 
            TInt n :: go ()
          else if is_keychar ch then
            let key = lex_keyword "" in
            List.assoc key keywords :: go ()
          else
            failwith (Printf.sprintf "Unexpected character found: %c" ch)
        end
    else 
      []
    in 
    go ()