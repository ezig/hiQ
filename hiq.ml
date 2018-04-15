let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Hiq_parse.program Hiq_lex.lexer (Lexing.from_channel ch)

 let _ =
 	let prog = parse_file () in
 	Printf.printf "%s\n" (Hiq_ast.ast_to_string prog)