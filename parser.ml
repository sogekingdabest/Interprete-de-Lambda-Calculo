type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | PUNTO_COMA_DOBLE
  | INTV of (int)
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
  let ctx = emptyctx;;
  
# 34 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* BOOL *);
  270 (* NAT *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* DOT *);
  274 (* EQ *);
  275 (* COLON *);
  276 (* ARROW *);
    0 (* EOF *);
  277 (* PUNTO_COMA_DOBLE *);
    0|]

let yytransl_block = [|
  278 (* INTV *);
  279 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\002\000\002\000\002\000\002\000\002\000\
\004\000\004\000\004\000\004\000\004\000\006\000\006\000\006\000\
\006\000\006\000\005\000\005\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\001\000\006\000\006\000\006\000\008\000\
\001\000\002\000\002\000\002\000\002\000\003\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\024\000\000\000\
\002\000\000\000\009\000\000\000\017\000\000\000\010\000\011\000\
\012\000\000\000\000\000\000\000\000\000\001\000\013\000\000\000\
\000\000\000\000\000\000\014\000\003\000\022\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\006\000\020\000\005\000\007\000\
\000\000\000\000\008\000"

let yydgoto = "\002\000\
\015\000\016\000\017\000\018\000\041\000\019\000\042\000"

let yysindex = "\006\000\
\007\255\000\000\252\254\000\000\000\000\046\255\010\255\010\255\
\010\255\253\254\001\255\046\255\000\000\013\255\000\000\042\000\
\000\000\010\255\000\000\032\255\000\000\053\255\000\000\000\000\
\000\000\041\255\043\255\044\255\046\255\000\000\000\000\246\254\
\046\255\046\255\246\254\000\000\000\000\000\000\000\000\246\254\
\047\255\045\255\057\255\054\255\049\255\055\255\046\255\246\254\
\046\255\046\255\046\255\000\000\000\000\000\000\000\000\000\000\
\058\255\046\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\000\000\242\255\028\000\000\000"

let yytablesize = 280
let yytable = "\022\000\
\017\000\004\000\038\000\039\000\040\000\028\000\001\000\003\000\
\004\000\005\000\006\000\004\000\005\000\007\000\008\000\009\000\
\010\000\011\000\020\000\026\000\045\000\012\000\037\000\027\000\
\012\000\046\000\043\000\044\000\013\000\014\000\029\000\013\000\
\021\000\054\000\023\000\024\000\025\000\019\000\019\000\019\000\
\053\000\030\000\055\000\056\000\057\000\031\000\003\000\004\000\
\005\000\006\000\032\000\059\000\007\000\008\000\009\000\010\000\
\011\000\033\000\034\000\036\000\012\000\035\000\049\000\047\000\
\048\000\050\000\051\000\013\000\021\000\058\000\052\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\017\000\000\000\000\000\004\000\004\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\017\000\
\000\000\004\000\000\000\000\000\000\000\000\000\017\000\017\000"

let yycheck = "\006\000\
\000\000\000\000\013\001\014\001\015\001\012\000\001\000\001\001\
\002\001\003\001\004\001\002\001\003\001\007\001\008\001\009\001\
\010\001\011\001\023\001\023\001\035\000\015\001\029\000\023\001\
\015\001\040\000\033\000\034\000\022\001\023\001\018\001\022\001\
\023\001\048\000\007\000\008\000\009\000\016\001\017\001\018\001\
\047\000\000\000\049\000\050\000\051\000\018\000\001\001\002\001\
\003\001\004\001\019\001\058\000\007\001\008\001\009\001\010\001\
\011\001\005\001\018\001\016\001\015\001\019\001\006\001\017\001\
\020\001\012\001\018\001\022\001\023\001\012\001\016\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\012\001\255\255\015\001\
\255\255\016\001\255\255\255\255\255\255\255\255\022\001\023\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  BOOL\000\
  NAT\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  PUNTO_COMA_DOBLE\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 42 "parser.mly"
            ( _1 )
# 223 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'context) in
    Obj.repr(
# 43 "parser.mly"
              ( _1 )
# 230 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "parser.mly"
      ( addbinding ctx _1 TyBool )
# 238 "parser.ml"
               : 'context))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 52 "parser.mly"
      ( _1 )
# 245 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 254 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 263 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 272 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 60 "parser.mly"
    (TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 282 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 64 "parser.mly"
      ( _1 )
# 289 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 66 "parser.mly"
      ( TmSucc _2 )
# 296 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 68 "parser.mly"
      ( TmPred _2 )
# 303 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( TmIsZero _2 )
# 310 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( TmApp (_1, _2) )
# 318 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 76 "parser.mly"
      ( _2 )
# 325 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
      ( TmTrue )
# 331 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
      ( TmFalse )
# 337 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
      ( TmVar _1 )
# 344 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 84 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 354 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 91 "parser.mly"
      ( _1 )
# 361 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 93 "parser.mly"
      ( TyArr (_1, _3) )
# 369 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 97 "parser.mly"
      ( _2 )
# 376 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
      ( TyBool )
# 382 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
      ( TyNat )
# 388 "parser.ml"
               : 'atomicTy))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.term)
