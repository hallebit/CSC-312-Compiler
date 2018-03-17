%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> NAME

%token TINT       (*  int *)
%token TBOOL      (* bool *)
%token TUNIT      (* unit *)

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
%token COLON      (*   :  *)
%token ARROW      (*  ->  *)
%token FIX        (*  fix *)
%token COMMA      (*   ,  *)
%token UNIT       (*  ()  *)
%token FIRST      (*  fst *)
%token SECOND     (*  snd *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                               { e }

typ:
  | TINT                                    { TInt }
  | TBOOL                                   { TBool }
  | t1=typ ARROW t2=typ                     { TFun(t1, t2) }
  | TUNIT                                   { TUnit }
  | LPAREN t1=typ TIMES t2=typ RPAREN       { TPair(t1, t2) }
  | LPAREN t=typ RPAREN                     { t } 

exp:
  | e1=exp PLUS e2=exp                      { EAddInt   (e1, e2) }
  | e1=exp MINUS e2=exp                     { ESubInt   (e1, e2) }
  | e1=exp TIMES e2=exp                     { EMultiInt (e1, e2) }
  | e1=exp FSLASH e2=exp                    { EDivInt   (e1, e2) }
  | e1=exp LEQ e2=exp                       { ELeqInt   (e1, e2) } 
  | IF e1=exp THEN e2=exp ELSE e3=exp       { EIf (e1, e2, e3) }
  | LET x=NAME COLON xt=typ EQUAL 
    e1=exp IN e2=exp                        { ELet (EVar x, xt, e1, e2) }
  | e1=exp e2=exp                           { ERunFun (e1, e2) }
  | FUN LPAREN x=NAME COLON xt=typ 
    RPAREN COLON rt=typ ARROW e=exp         { EVal (VFun (EVar x, xt, e, rt)) }
  | FIX f=NAME LPAREN x=NAME COLON xt=typ
    RPAREN COLON rt=typ ARROW e=exp         { EVal (VFix (EVar f, EVar x, xt, e, rt)) }
  | LPAREN e1=exp COMMA e2=exp RPAREN       { EVal (VPair (e1, e2)) }
  | FIRST e=exp                             { EFirst e }
  | SECOND e=exp                            { ESecond e }
  | LPAREN e=exp RPAREN                     { e }
  | n=INT                                   { EVal (VLit (LInt  n)) }
  | b=BOOL                                  { EVal (VLit (LBool b)) }
  | s=NAME                                  { EVar  s }
  | UNIT                                    { EVal VUnit }