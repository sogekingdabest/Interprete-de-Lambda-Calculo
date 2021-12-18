(*
Authors:
      Ángel Álvarez Rey
      Daniel Olañeta Fariña
*)

(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyPair of ty * ty
  | TyRecord of (string * ty) list
;;

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
  | TmRecord of (string * term) list
  | TmProjRecord of term * string
;;

type vcontext =
  (string * term) list
;;


(*get del tipo*)
type command = 
  Eval of term
  | Bind of string * term

(* CONTEXT MANAGEMENT *)

let emptytctx =
  []
;;

let emptyvctx =
  []
;;

let addtbinding tctx x ty =
  (x, ty) :: tctx
;;

let addvbinding vctx x term =
  (x, term) :: vctx
;;

let gettbinding ctx x =
  List.assoc x ctx
;;

let getvbinding ctx x =
  List.assoc x ctx
;;

(* TYPE MANAGEMENT (TYPING) *)

(*
Function that receibes two records. The first record must be the type and the 
second the subtype. The function checks if each pair in the type appears in the 
subtype. If so, it returns true and otherwise false. 
*)
let subTyping record1 record2 = 
    let rec aux list1 list2 auxBool = 
      match list1,list2 with
         [], _ -> auxBool
        | _, [] -> auxBool
        | (h::t),(h1::t2) -> if (h==h1) then aux t record2 true
                          else aux list1 t2 false
        
      in aux record1 record2 false;; 

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
    "String"
  | TyPair (ty1, ty2) ->
    "{" ^ string_of_ty ty1 ^ ", " ^ string_of_ty ty2 ^ "}"
  | TyRecord list -> 
    let rec aux str l =
      match l with
        (name, value)::t -> aux (str ^ name ^ ": " ^ string_of_ty value ^ ", ") t
        | _ -> 
          let n = String.length str in
          String.sub str 0 (n-2) ^ "}"
    in aux "{" list
  ;;

(*  *)
exception Type_error of string
;;


(*
In this function we modified the parameters that receives dividing the ctx 
(context)param in two subparams: a value ctx (vctx) and a type ctx (tctx). 
This was made in order to simplify the use of ctx param. 
We also added some new features implemented as options. Each one is desbribed
above its declaration.
*)
let rec typeof vctx tctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof vctx tctx t1 = TyBool then
        let tyT2 = typeof vctx tctx t2 in
        if typeof vctx tctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof vctx tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof vctx tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof vctx tctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding tctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let vctx' = addvbinding vctx x t2 in
      let tctx' = addtbinding tctx x tyT1 in
      let tyT2 = typeof vctx' tctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof vctx tctx t1 in
      let tyT2 = typeof vctx tctx t2 in
      (match tyT1 with
             TyArr (tyT11, tyT12) ->
              (match tyT11,tyT12 with
                (TyRecord list1),(TyRecord list2) -> if subTyping list1 list2 then tyT12
                                                     else raise (Type_error "parameter type mismatch")
                | _,_ -> if tyT2 = tyT11 then tyT12
                       else raise (Type_error "parameter type mismatch"))
              | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let vctx' = addvbinding vctx x t1 in
      let tyT1 = typeof vctx tctx t1 in
      let tctx' = addtbinding tctx x tyT1 in
      typeof vctx' tctx' t2
    (* T-Fix *)
  | TmFix t1 ->
     let tyT1 = typeof vctx tctx t1 in
     (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
          | _ -> raise (Type_error "arrow type expected"))
  (* T-Srring *)
  | TmString t ->
    TyString 
  (* T-Concat *)
  | TmConcat (t1, t2) ->
    let tyT1 = typeof vctx tctx t1 in
    let tyT2 = typeof vctx tctx t2 in
      (match (tyT1, tyT2) with
          (TyString, TyString) -> TyString
          | _ -> raise (Type_error "the term must be type string"))
  (* T-Pair *)
  | TmPair (t1, t2) ->
    let tyT1 = typeof vctx tctx t1 in
    let tyT2 = typeof vctx tctx t2 in
      TyPair (tyT1, tyT2)

  (* T-First *)
  | TmFirst t ->
    let tyTerm = typeof vctx tctx t in
    (match tyTerm with 
      TyPair (t1, t2) -> 
        t1
      | _ -> raise (Type_error "the term must be a tuple"))

  (* T-Second *)
  | TmSecond t ->
    let tyTerm = typeof vctx tctx t in
    (match tyTerm with
      TyPair (t1, t2) ->
        t2
      | _ -> raise (Type_error "the term must be a tuple"))
  
  (* T-Record *)
  | TmRecord list ->
      (let rec aux lsalida lentrada =
        match lentrada with
          (name, value)::t -> aux ((name, typeof vctx tctx value)::lsalida) t
          | _ -> TyRecord (List.rev lsalida)
      in aux [] list)
  
  (* T-Projection Record *)
  | TmProjRecord (term, strT) ->
    let tyTerm = typeof vctx tctx term in
    (match tyTerm with 
      TyRecord list ->
        (List.assoc strT list)
      | _ -> raise (Type_error "the term must be a record"))
;;
(* TERMS MANAGEMENT (EVALUATION) *)

(*
This function is were we transform a term into a string in order to print it
by terminal. We also add the corresponding prints for the terms we are implementing. 
*)
let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString t ->
    t
  | TmConcat (t1, t2) ->
    string_of_term t1 ^ string_of_term t2
  | TmPair (t1, t2) ->
    "{" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ "}"
  | TmFirst t -> 
    (match t with 
      TmPair (t1, t2) ->
        string_of_term t1
      | _ -> raise (Type_error "String_of_term Error -> the term must be a pair"))
  | TmSecond t ->
    (match t with 
      TmPair (t1, t2) ->
        string_of_term t2
      | _ -> raise (Type_error "String_of_term Error -> the term must be a pair"))
  | TmRecord list -> 
    let rec aux str l =
      match l with
        (name, value)::t -> aux (str ^ name ^ ": " ^ string_of_term value ^ ", ") t
        | _ -> 
          let n = String.length str in
          String.sub str 0 (n-2) ^ "}"
    in aux "{" list 
  | TmProjRecord (term, strT) ->
    (match term with 
      TmRecord list ->
        string_of_term (List.assoc strT list)
      | _ -> raise (Type_error "String_of_term Error -> the term must be a record"))
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;
(*
Here we added the corresponding implementation for the new terms. 
*)
let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString t ->
    [t]
  | TmConcat (t1, t2) ->
    [string_of_term t1 ^ string_of_term t2]
  | TmPair (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmFirst t ->
    (match t with 
    TmPair (t1, t2) ->
        free_vars t1
      | _ -> raise (Type_error "Free_vars Error -> the term must be a pair"))
  | TmSecond t ->
    (match t with 
    TmPair (t1, t2) ->
        free_vars t2
      | _ -> raise (Type_error "Free_vars Error -> the term must be a pair"))
  | TmRecord list -> 
    let rec aux lsalida lentrada =
      match lentrada with
        (name, value)::t -> aux ((free_vars value) @ lsalida) t
        | _ -> (List.rev lsalida)
    in aux [] list 
  | TmProjRecord (term, strT) -> 
    (match term with 
      TmRecord list ->
        free_vars (List.assoc strT list)
      | _ -> raise (Type_error "Free_vars Error -> the term must be a record"))
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
(*
Here we added the corresponding implementation for the new terms. 
*) 
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      (TmIf (subst x s t1, subst x s t2, subst x s t3))
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
        TmFix (subst x s t)   
  | TmString t ->
    TmString t
  | TmConcat (t1, t2) ->
    TmString (string_of_term (subst x s t1) ^ string_of_term (subst x s t2))
  | TmPair (t1, t2) ->
    TmPair (subst x s t1, subst x s t2)
  | TmFirst t -> 
    let tPair = subst x s t in
      (match tPair with 
        TmPair (t1, t2) ->
          subst x s t1
        | _ -> raise (Type_error "Substitution Error -> the term must be a pair"))
  | TmSecond t ->
    let tPair = subst x s t in
      (match tPair with 
        TmPair (t1, t2) ->
          subst x s t2
        | _ -> raise (Type_error "Substitution Error -> the term must be a pair"))
  | TmRecord list ->
    let rec aux lsalida lentrada =
      match lentrada with
      (name, value)::t -> aux ((name,(subst x s value))::lsalida) t
        | _ -> TmRecord (List.rev lsalida)
    in aux [] list
  | TmProjRecord (term, strT) -> 
    let trecord = subst x s term in
      (match trecord with 
      TmRecord list ->
        subst x s (List.assoc strT list)
      | _ -> raise (Type_error "Substitution Error -> the term must be a record"))
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmPair _ -> true
  | TmRecord _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;
(*
In this function we evaluate terms values. For this purpose we exchanged the ctx arg
by the vctx (value context: feature explained in typeof func definition). We've also
added the debugMode argument, used to print all the intermediate terms that the 
recursion of eval1 is generating. Obviously we added the terms we are implementing too. 
*)
let rec eval1 vctx tm debugMode = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      if debugMode==true then print_endline (string_of_term t2);
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      if debugMode==true then print_endline (string_of_term t3);
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      if debugMode==true then print_endline (string_of_term t1 ^ " " ^ string_of_term t2 ^ " " ^ string_of_term t3);
      let t1' = eval1 vctx t1 debugMode in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      if debugMode==true then print_endline (string_of_term t1);
      let t1' = eval1 vctx t1 debugMode in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      if debugMode==true then print_endline (string_of_term TmZero);
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      if debugMode==true then print_endline (string_of_term t1);
      let t1' = eval1 vctx t1 debugMode in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      if debugMode==true then print_endline (string_of_term TmTrue);
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      if debugMode==true then print_endline (string_of_term TmFalse);
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      if debugMode==true then print_endline (string_of_term TmZero);
      let t1' = eval1 vctx t1 debugMode in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      if debugMode==true then print_endline (string_of_term v1 ^ " " ^ string_of_term t2);
      let t2' = eval1 vctx t2 debugMode in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      if debugMode==true then print_endline (string_of_term t1 ^ " " ^ string_of_term t2);
      let t1' = eval1 vctx t1 debugMode in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 debugMode in
      TmLetIn (x, t1', t2) 

    (*E-FixBeta*)
  | TmFix (TmAbs (x, _, t12)) ->
    subst x tm t12

    (*E-Fix*)
  | TmFix t1 ->
      if debugMode==true then print_endline (string_of_term t1);
      let t1' = eval1 vctx t1 debugMode in
      TmFix t1'

  | TmVar y ->
    getvbinding vctx y

  | TmConcat (t1, t2) ->
    if debugMode==true then print_endline (string_of_term (TmConcat (t1, t2) ) );
    let t1' = eval1 vctx t1 debugMode in
    let t2' = eval1 vctx t2 debugMode in
    TmString (string_of_term t1' ^ string_of_term t2')
  
  | TmPair (t1, t2) ->
    if debugMode==true then print_endline (string_of_term (TmPair (t1, t2) ) );
    let t1' = eval1 vctx t1 debugMode in
    let t2' = eval1 vctx t2 debugMode in
    TmPair (t1', t2')
  
  | TmFirst t -> 
    if debugMode==true then print_endline (string_of_term t);    
    let tTerm = eval1 vctx t debugMode in
    (match tTerm with 
      TmPair (t1, t2) ->
        let t' = eval1 vctx t1 debugMode in
        if debugMode==true then print_endline (string_of_term t');
        t'
      | _ -> raise (Type_error "Evaluation Error -> the term must be a tuple"))
  | TmSecond t ->
    if debugMode==true then print_endline (string_of_term t);
    let tTerm = eval1 vctx t debugMode in
    (match tTerm with 
      TmPair (t1, t2) -> 
        let t' = eval1 vctx t2 debugMode in
        if debugMode==true then print_endline (string_of_term t');
        t'
      | _ -> raise (Type_error "Evaluation Error -> the term must be a tuple"))
  | TmRecord list ->
    
    let rec aux lsalida lentrada =
      match lentrada with
        (name, value)::t -> aux ((name,(eval1 vctx value debugMode))::lsalida) t
        | _ -> if debugMode==true then print_endline (string_of_term (TmRecord (List.rev lsalida) )); 
          TmRecord (List.rev lsalida)
    in aux [] list

  | TmProjRecord (term, strT) ->
    let tmTerm = eval1 vctx term debugMode in
    (match tmTerm with 
    TmRecord list ->
      let t' = eval1 vctx (List.assoc strT list) debugMode in
      if debugMode==true then print_endline (string_of_term t');
      t'
    | _ -> raise (Type_error "Evaluation Error -> the term must be a Record"))

  | _ ->
      raise NoRuleApplies
;;
(*
This function applies the value context to a term. For this purpose we exchanged the ctx arg
by the vctx (value context: feature explained in typeof func definition). We added the terms
we are implementing too. 
*)
let apply_ctx vctx tm =
  let rec aux vl = function
    | TmTrue -> 
        TmTrue
    | TmFalse ->
        TmFalse
    | TmIf (t1, t2, t3) ->
       TmIf (aux vl t1, aux vl t2, aux vl t3)
    | TmZero ->
      TmZero
    | TmSucc t ->
      TmSucc (aux vl t)
    | TmPred t ->
      TmPred (aux vl t)
    | TmIsZero t ->
      TmIsZero (aux vl t)
    | TmVar y ->
      if List.mem y vl then TmVar y else getvbinding vctx y
    | TmAbs (s, t, t1) -> 
      TmAbs (s, t, aux (s::vl) t1)
    | TmApp (t1, t2) ->
      TmApp (aux vl t1, aux vl t2)
    | TmLetIn (y, t1, t2) ->
      TmLetIn (y, aux vl t1, aux (y::vl) t2)
    | TmFix t ->
      TmFix (aux vl t)
    | TmString t ->
      TmString t
    | TmConcat (t1, t2) ->
      TmConcat (aux vl t1, aux vl t2)
    | TmPair (t1, t2) ->
      TmPair (aux vl t1, aux vl t2)
    | TmFirst t -> 
      let termPair = aux vl t in
      (match termPair with
        TmPair (t1, t2) ->
          aux vl t1
        | _ -> raise (Type_error "Apply Error -> the term must be a pair"))
    | TmSecond t ->
      let termPair = aux vl t in
      (match termPair with
      TmPair (t1, t2) ->
        aux vl t2
      | _ -> raise (Type_error "Apply Error -> the term must be a pair"))
    | TmRecord list ->
      let rec auxRecord lsalida lentrada =
        match lentrada with
          (name,value)::t -> auxRecord ((name,(aux vl value))::lsalida) t
          | _ -> TmRecord (List.rev lsalida)
      in auxRecord [] list
    | TmProjRecord (term, strT) -> 
      let tmTerm = aux vl term in
      (match tmTerm with
        TmRecord list ->
          aux vl (List.assoc strT list)
        | _ -> raise (Type_error "Apply Error -> the term must be a record"))
  in aux [] tm
;;

let rec eval vctx tm debugMode =
  try
    let tm' = eval1 vctx tm debugMode in
    eval vctx tm' debugMode
  with
    NoRuleApplies -> apply_ctx vctx tm
;;
