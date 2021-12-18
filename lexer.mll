{
  
(*
Authors:
      Ángel Álvarez Rey
      Daniel Olañeta Fariña
*)
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"     {LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "concat"    { CONCAT }
  | "pair"      { PAIR }
  | ".1"        { FIRST }
  | ".2"        { SECOND }
  | '{'         { OPENPAIR }
  | '}'         { CLOSEPAIR }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | ','         { COMMA }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "\""        { QUOTE }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 
