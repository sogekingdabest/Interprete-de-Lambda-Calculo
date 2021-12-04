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
  | QUOTE
  | EOF
  | PUNTO_COMA_DOBLE
  | INTV of (int)
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
  
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
  277 (* QUOTE *);
    0 (* EOF *);
  278 (* PUNTO_COMA_DOBLE *);
    0|]

let yytransl_block = [|
  279 (* INTV *);
  280 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\005\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\006\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\004\000\002\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\003\000\001\000\003\000\003\000\001\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\014\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\000\000\025\000\
\000\000\000\000\008\000\000\000\016\000\000\000\009\000\010\000\
\011\000\000\000\000\000\000\000\000\000\000\000\002\000\012\000\
\000\000\000\000\000\000\000\000\013\000\018\000\000\000\022\000\
\023\000\000\000\024\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\005\000\020\000\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\016\000\017\000\018\000\044\000\019\000\045\000"

let yysindex = "\006\000\
\010\255\000\000\240\254\000\000\000\000\050\255\020\255\020\255\
\020\255\241\254\247\254\050\255\003\255\000\000\012\255\000\000\
\032\000\020\255\000\000\017\255\000\000\032\255\000\000\000\000\
\000\000\021\255\019\255\024\255\025\255\050\255\000\000\000\000\
\053\255\050\255\050\255\053\255\000\000\000\000\042\000\000\000\
\000\000\053\255\000\000\033\255\035\255\056\255\051\255\046\255\
\000\000\054\255\050\255\053\255\050\255\050\255\050\255\000\000\
\000\000\000\000\000\000\000\000\057\255\050\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\243\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\230\255\071\000\000\000"

let yytablesize = 281
let yytable = "\022\000\
\016\000\003\000\019\000\019\000\019\000\028\000\001\000\020\000\
\026\000\048\000\003\000\004\000\005\000\006\000\027\000\050\000\
\007\000\008\000\009\000\010\000\011\000\004\000\005\000\039\000\
\012\000\058\000\029\000\046\000\047\000\030\000\013\000\031\000\
\014\000\015\000\012\000\033\000\034\000\036\000\035\000\037\000\
\013\000\049\000\014\000\021\000\057\000\038\000\059\000\060\000\
\061\000\051\000\003\000\004\000\005\000\006\000\052\000\063\000\
\007\000\008\000\009\000\010\000\011\000\053\000\054\000\055\000\
\012\000\040\000\041\000\042\000\062\000\056\000\013\000\000\000\
\014\000\021\000\000\000\000\000\043\000\023\000\024\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\016\000\016\000\000\000\000\000\003\000\003\000\
\000\000\000\000\000\000\000\000\000\000\003\000\000\000\016\000\
\000\000\003\000\000\000\000\000\000\000\016\000\000\000\016\000\
\016\000"

let yycheck = "\006\000\
\000\000\000\000\016\001\017\001\018\001\012\000\001\000\024\001\
\024\001\036\000\001\001\002\001\003\001\004\001\024\001\042\000\
\007\001\008\001\009\001\010\001\011\001\002\001\003\001\030\000\
\015\001\052\000\024\001\034\000\035\000\018\001\021\001\000\000\
\023\001\024\001\015\001\019\001\005\001\019\001\018\001\016\001\
\021\001\000\000\023\001\024\001\051\000\021\001\053\000\054\000\
\055\000\017\001\001\001\002\001\003\001\004\001\020\001\062\000\
\007\001\008\001\009\001\010\001\011\001\006\001\012\001\018\001\
\015\001\013\001\014\001\015\001\012\001\016\001\021\001\255\255\
\023\001\024\001\255\255\255\255\024\001\007\000\008\000\009\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\018\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\016\001\255\255\255\255\255\255\021\001\255\255\023\001\
\024\001"

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
  QUOTE\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 43 "parser.mly"
        (Bind (_1,_3))
# 230 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 45 "parser.mly"
        ( Eval _1 )
# 237 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 49 "parser.mly"
      ( _1 )
# 244 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 51 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 253 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 53 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 262 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 55 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 271 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
    (TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 281 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 61 "parser.mly"
      ( _1 )
# 288 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 63 "parser.mly"
      ( TmSucc _2 )
# 295 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 65 "parser.mly"
      ( TmPred _2 )
# 302 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( TmIsZero _2 )
# 309 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmApp (_1, _2) )
# 317 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 73 "parser.mly"
      ( _2 )
# 324 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
      ( TmTrue )
# 330 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
      ( TmFalse )
# 336 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
      ( TmVar _1 )
# 343 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 353 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
      ( TmString _2 )
# 360 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 90 "parser.mly"
      ( _1 )
# 367 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 92 "parser.mly"
      ( TyArr (_1, _3) )
# 375 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 96 "parser.mly"
      ( _2 )
# 382 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
      ( TyBool )
# 388 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
      ( TyNat )
# 394 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "parser.mly"
      ( TyString )
# 401 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command)
