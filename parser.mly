%{

(*
Authors:
      Ángel Álvarez Rey
      Daniel Olañeta Fariña
*)

  open Lambda;;
  
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token QUOTE
%token EOF
%token CONCAT
%token PAIR
%token OPENPAIR
%token CLOSEPAIR
%token COMMA
%token FIRST
%token SECOND

%token <int> INTV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOF
        {Bind ($1,$3)}
    | term EOF
        { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
    {TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
  | CONCAT term term
    { TmConcat ($2,$3) }

    
appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | CONCAT appTerm appTerm
     { TmConcat ($2,$3) }
  | atomicTerm FIRST
   { TmFirst $1 }
  | atomicTerm SECOND
   { TmSecond $1 }
  | atomicTerm DOT STRINGV
   { TmProjRecord ($1,$3) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | QUOTE STRINGV QUOTE 
      { TmString $2 }
  | OPENPAIR term COMMA term CLOSEPAIR
      { TmPair ($2, $4) }
  | OPENPAIR record CLOSEPAIR
    { TmRecord $2 }

record :
    STRINGV EQ term
      { [($1,$3)] }
    | STRINGV EQ term COMMA record
      { ($1,$3)::$5 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

recordTy :
    STRINGV COLON ty
      { [($1,$3)] }
    | STRINGV COLON ty COMMA recordTy
      { ($1,$3)::$5 }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRINGV
      { TyString }
  | OPENPAIR ty COMMA ty CLOSEPAIR
      { TyPair ($2,$4) }
  | OPENPAIR recordTy CLOSEPAIR
      { TyRecord $2 }



