open Hiq_ast

type edge = int * int * string

let ast_to_edges ({prog; _} : program) : edge list =
	let exp_to_edges e =
		match e with
		| Measure (q, c) -> [(q, q, "measure")]
		| UGate (u, q) -> [(q, q, ugate_to_string u)]
		| BinGate (b, q1, q2) -> [(q1, q2, bingate_to_string b)] in
	let rec stmt_to_edges s =
		match s with
		| Exp e -> exp_to_edges e
		| Seq (s1, s2) -> (stmt_to_edges s1) @ (stmt_to_edges s2)
		| If (e, s1, s2o) ->
			(exp_to_edges e) @ (stmt_to_edges s1) @
			match s2o with
			| None -> []
			| Some s2 -> (stmt_to_edges s2) in
	stmt_to_edges prog

let ast_to_edge_string (p : program) : string =
	ast_to_edges p |> 
	List.fold_left (fun acc (q1, q2, g) ->
		acc ^ (Printf.sprintf "absgate(%d,%d,\"%s\"). " q1 q2 g)) ""

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
	Printf.printf "%s\n" (ast_to_edge_string prog)