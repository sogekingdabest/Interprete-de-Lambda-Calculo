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
  | CONCAT
  | PAIR
  | OPENPAIR
  | CLOSEPAIR
  | COMMA
  | FIRST
  | SECOND
  | PUNTO_COMA_DOBLE
  | INTV of (int)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
