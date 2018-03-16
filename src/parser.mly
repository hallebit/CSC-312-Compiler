%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> NAME

%token LPAREN     (*   (  *)
%token RPAREN     (*   )  *)
%token PLUS       (*   +  *)
%token MINUS      (*   -  *)
%token TIMES      (*   *  *)
%token FSLASH     (*   /  *)
%token IF         (*  if  *)
%token THEN       (* then *)
%token ELSE       (* else *)
%token LEQ        (*  <=  *)
%token LET        (*  let *)
%token EQUAL      (*   =  *)
%token IN         (*  in  *)
%token FUN        (*  fun *)
%token ARROW      (*  ->  *)
%token FIX        (*  fix *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                               { e }

exp:
  | e1=exp PLUS e2=exp                      { EAddInt   (e1, e2) }
  | e1=exp MINUS e2=exp                     { ESubInt   (e1, e2) }
  | e1=exp TIMES e2=exp                     { EMultiInt (e1, e2) }
  | e1=exp FSLASH e2=exp                    { EDivInt   (e1, e2) }
  | e1=exp LEQ e2=exp                       { ELeqInt   (e1, e2) } 
  | IF e1=exp THEN e2=exp ELSE e3=exp       { EIf (e1, e2, e3) }
  | LET s=NAME EQUAL e1=exp IN e2=exp       { ELet (EVar s, e1, e2) }
  | e1=exp e2=exp                           { ERunFun (e1, e2) }
  | FUN s=NAME ARROW e=exp                  { EVal (VFun (EVar s, e)) }
  | FIX s1=NAME s2=NAME ARROW e=exp         { EVal (VFix (EVar s1, EVar s2, e)) }
  | LPAREN e=exp RPAREN                     { e }
  | n=INT                                   { EVal (VLit (LInt  n)) }
  | b=BOOL                                  { EVal (VLit (LBool b)) }
  | s=NAME                                  { EVar  s }  