%{
open Lang
%}

%token <int> INT

%token TRUE
%token FALSE

%token LPAREN     (*   (  *)
%token RPAREN     (*   )  *)
%token PLUS       (*   +  *)
%token MINUS      (*   -  *)
%token TIMES      (*   *  *)
%token FSLASH     (*   /  *)
%token IF         (*  if  *)
%token THEN       (* then *)
%token ELSE       (* else *)
%token LEQ        (* <= *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                               { e }

exp:
  | LPAREN n=INT RPAREN                     { EInt n }
  | n=INT                                   { EInt n }
  | LPAREN TRUE RPAREN                      { EBool true  }
  | LPAREN FALSE RPAREN                     { EBool false  }
  | TRUE                                    { EBool true  }
  | FALSE                                   { EBool false  }
  | LPAREN e1=exp PLUS e2=exp RPAREN        { EAddInt (e1, e2) }
  | LPAREN e1=exp MINUS e2=exp RPAREN       { ESubInt (e1, e2) }
  | LPAREN e1=exp TIMES e2=exp RPAREN       { EMultiInt (e1, e2) }
  | LPAREN e1=exp FSLASH e2=exp RPAREN      { EDivInt (e1, e2) }
  | e1=exp PLUS e2=exp                      { EAddInt (e1, e2) }
  | e1=exp MINUS e2=exp                     { ESubInt (e1, e2) }
  | e1=exp TIMES e2=exp                     { EMultiInt (e1, e2) }
  | e1=exp FSLASH e2=exp                    { EDivInt (e1, e2) }
  | LPAREN IF e1=exp THEN e2=exp ELSE e3=exp RPAREN      { EIf (e1, e2, e3) }
  | IF e1=exp THEN e2=exp ELSE e3=exp       { EIf (e1, e2, e3) }
  | LPAREN e1=exp LEQ e2=exp RPAREN                     { ELeqInt (e1, e2) }
  | e1=exp LEQ e2=exp                       { ELeqInt (e1, e2) }