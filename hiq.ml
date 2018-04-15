open Hiq_ast
open Hiq_compile 

type mapping = (int * int) list

let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 3
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse] [mapping]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Hiq_parse.program Hiq_lex.lexer (Lexing.from_channel ch)

let apply_mapping ({prog; _} as p : program) (m : mapping) : program =
	let map_exp e = 
		match e with
		| Measure (q, c) -> Measure (List.assoc q m, c)
		| UGate (u, q) -> UGate (u, List.assoc q m)
		| BinGate (b, q1, q2) -> BinGate (b, List.assoc q1 m, List.assoc q2 m) in 
	let rec map_stmt s =
		match s with
		| Exp e -> Exp (map_exp e)
		| Seq (s1, s2) -> Seq (map_stmt s1, map_stmt s2)
		| If (e, s1, s2o) ->
			If (map_exp e, map_stmt s1,
				match s2o with
				| None -> None
				| Some s2 -> Some (map_stmt s2)) in
	{p with prog = map_stmt prog}

 let _ =
 	let prog = parse_file () in
	let lst = Str.split (Str.regexp " ") Sys.argv.(2) in
	let mapping = List.map (fun s -> Scanf.sscanf s "%d,%d" (fun x y -> (x, y))) lst in
	Printf.printf "%s" (compile_to_ibm (apply_mapping prog mapping))
