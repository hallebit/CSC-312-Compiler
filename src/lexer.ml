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
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TMulti
  | TDivide

let string_of_token (t:token) : string = 
  match t with
  | TInt n  -> string_of_int n
  | TLParen -> "("
  | TRParen -> ")"
  | TPlus   -> "+"
  | TMinus   -> "-"
  | TMulti   -> "*"
  | TDivide   -> "/"

let string_of_token_list (toks:token list) : string =
  (* Note that String.concat sep sl concatenates the list of strings
   *           inserting the separator string sep between each.
   * Also note that List.map applies a function f to each element of
   *           the given list and builds a list with the results. 
   *)
  String.concat "," (List.map string_of_token toks)

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

(* Note: lex contains two nested helper functions, lex_num and go *)
let lex (src:char Stream.t) : token list = 
  let rec lex_num acc = 
    if is_digit (peek src) then
      (* Note ^ is the operator for string concatenation.*)
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      int_of_string acc
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
        match ch with
        (* Note that      :: is the cons operator
         * Also note that ignore discards the value of its argument and returns (). 
         *)
        | '(' -> advance src |> ignore; TLParen :: go ()
        | ')' -> advance src |> ignore; TRParen :: go ()
        | '+' -> advance src |> ignore; TPlus   :: go ()
        | '-' -> advance src |> ignore; TMinus  :: go ()
        | '*' -> advance src |> ignore; TMulti  :: go ()
        | '/' -> advance src |> ignore; TDivide :: go ()
        | _   ->
          if is_whitespace ch then
            begin advance src |> ignore; go () end
          else if is_digit ch then
            let n = lex_num "" in 
            TInt n :: go ()
          else 
            failwith (Printf.sprintf "Unexpected character found: %c" ch)
    else 
      []
    in 
    go ()