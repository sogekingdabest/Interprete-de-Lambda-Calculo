
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open List;;


let execute vctx tctx = function
  Eval tm ->
    let tyTm = typeof vctx tctx tm in
    let tm' = eval vctx tm in
    print_endline("-: " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (vctx, tctx)

  | Bind (s, tm) ->
    let tyTm = typeof vctx tctx tm in
    let tm' = eval vctx tm in
    print_endline("-: " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (addvbinding vctx s tm', addtbinding tctx s tyTm)
  ;;


let rec multiline strIn strOut = match strIn with
      | [] -> multiline (String.split_on_char ';' (read_line())) strOut
      | [""] -> multiline (String.split_on_char ';' (read_line())) strOut
      | h :: t -> if (List.length (h::t))==1 then multiline (String.split_on_char ';' (read_line())) (strOut^" "^h)
                  else (strOut^" "^h)
                ;;
                
let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (from_string (multiline (String.split_on_char ';' (read_line())) "")) in
        loop (execute vctx tctx c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyvctx,emptytctx)
  ;;

top_level_loop ()
;;

