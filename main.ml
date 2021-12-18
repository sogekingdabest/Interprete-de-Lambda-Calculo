(*
Authors:
      Ángel Álvarez Rey
      Daniel Olañeta Fariña
*)
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open Str;;
open List;;

(* 
  Function to distinguish between a term or a variable binding.
  With the Eval option, the function follows the same structure as before.
  With the Bind option, the function evaluates and stores the term in the context.
*)
let execute vctx tctx debugMode = function
  Eval tm ->
    let tyTm = typeof vctx tctx tm in
    let tm' = eval vctx tm debugMode in
    print_endline("-: " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (vctx, tctx)

  | Bind (s, tm) ->
    let tyTm = typeof vctx tctx tm in
    let tm' = eval vctx tm debugMode in
    print_endline("-: " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (addvbinding vctx s tm', addtbinding tctx s tyTm)
  ;;

(*
  This function reads an input string in search of semicolon so this allows to write a 
  term in many lines. If it finds this semicolon returns the string to evaluate.
*)
let rec multiline strIn strOut = match strIn with
      | [] -> multiline (String.split_on_char ';' (read_line())) strOut
      | [""] -> multiline (String.split_on_char ';' (read_line())) strOut
      | h :: t -> if (List.length (h::t))==1 then multiline (String.split_on_char ';' (read_line())) (strOut^" "^h)
                  else (strOut^" "^h)
;;    

(*
  Function to evaluate if a line has a semicolon or not, returning true if has and false
  if doesn't.
*)
let has_semicolon line=
  try 
    match (Str.search_forward (Str.regexp ";") line 0) 
      with _-> true  
  with Not_found -> false
;;

(*
  This function reads one document from an input channel and stores the lines in one variable 
  until finding a semicolon. It counts the lines too.
*)
let read_file input_channel nLine =
  let rec aux str count =
    let line = input_line input_channel in
    if (has_semicolon line) then
      let sLine = (hd (split_on_char ';' line)) in 
        ((str ^ " " ^ sLine),count+1)
    else
      aux (str ^ " " ^ line) (count+1)
  in aux "" nLine
;;

(*
This function is similar to top_level_loop (implemented below). The only difference is that the input
is a file that is being read on. It prints the line if an error is found and also accepts a variable to 
control whether the debug mode is enabled or not.
*)
let top_file_level_loop file debugMode =
  let input_channel = open_in file in
  try              
      let rec loop (vctx, tctx) nLine debugMode =
        try
          let input = read_file input_channel nLine in
          let c = s token (from_string (fst input)) in
          loop (execute vctx tctx debugMode c) (snd input) debugMode
        with
            Lexical_error ->
              print_endline ("lexical error in line: " ^ string_of_int nLine);
              exit 0;
          | Parse_error ->
              print_endline ("syntax error in line: " ^ string_of_int nLine);
              exit 0;
          | Type_error e ->
              print_endline ("type error: " ^ e ^ " in line: " ^ string_of_int nLine);
              exit 0;
      in
        loop (emptyvctx, emptytctx) 1 debugMode
  with End_of_file ->
    close_in input_channel
;;

(*
The new argument in top_level_loop is to check if the debug mode is activated or not. The function 
loop now accepts two contexts: one for values and other for types. It also uses the function execute
to evaluate a term or binding a variable in the context.
*)
let top_level_loop debugMode =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) debugMode =
    print_string ">> ";
    flush stdout;
    try
      let c = s token (from_string (multiline (String.split_on_char ';' (read_line())) "")) in
        loop (execute vctx tctx debugMode c) debugMode
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx) debugMode
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx) debugMode
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx) debugMode
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyvctx,emptytctx) debugMode
;;

(*
This match is to select between debugMode or not and between read from file or read from console.
*)
match (Array.length Sys.argv) with
  | 1 -> top_level_loop false
  | 2 -> if (Sys.argv.(1) = "-debugMode") then top_level_loop true
         else top_file_level_loop Sys.argv.(1) false
  | 3 -> if (Sys.argv.(1) = "-debugMode") then top_file_level_loop Sys.argv.(2) true
         else raise (Arg.Bad "Wrong arguments -> did you mean -debugMode?")
  | _ -> raise (Arg.Bad "Wrong arguments -> too many arguments!")
;;
