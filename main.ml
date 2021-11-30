
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open List;;

let rec multiline strIn strOut = match strIn with
      | [] -> multiline (String.split_on_char ';' (read_line())) strOut
      | [""] -> multiline (String.split_on_char ';' (read_line())) strOut
      | h :: t -> if (List.length (h::t))==1 then multiline (String.split_on_char ';' (read_line())) (strOut^" "^h)
                  else (strOut^" "^h)
                ;;
                
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (multiline (String.split_on_char ';' (read_line())) "")) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

