
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyPair of ty * ty
;;

(*Contexto de tipos*)
type tcontext =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmPair of term * term
  | TmFirst of term
  | TmSecond of term 
;;

(*Contexto de valores*)
type vcontext = 
  (string * term) list
;;

(*get del tipo*)
type command = 
  Eval of term
  | Bind of string * term

(*Crear contexto de tipos vacio*)
val emptytctx : tcontext;;
(*Crear contexto de valores vacio*)
val emptyvctx : vcontext;;
(*Añadir entrada a contexto de tipos*)
val addtbinding : tcontext -> string -> ty -> tcontext;;
(*Añadir entrada a contexto de valores*)
val addvbinding : vcontext -> string -> term -> vcontext;;
(*Coger entrada del contexto de tipos*)
val gettbinding : tcontext -> string -> ty;;
(*Coger entrada del contexto de tipos*)
val getvbinding : vcontext -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : vcontext -> tcontext -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : vcontext -> term -> term;;

(*val execute : vcontext *tcontext -> command -> vcontext * tcontext;;
*)
