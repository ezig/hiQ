open Hiq_ast
open Hiq_compile 

type mapping = (int * int) list

let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv > 4
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse] [low?] [high?]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Hiq_parse.program Hiq_lex.lexer (Lexing.from_channel ch)

let _ =
    let prog = parse_file () in
    if Array.length Sys.argv = 2 then Printf.printf "%d\n" (count_gates prog)
    else let split_prog =
        split prog (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3)) in
        Printf.printf "%s" (ast_to_string split_prog)
