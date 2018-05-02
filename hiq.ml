open Hiq_ast
open Hiq_compile 

type mapping = (int * int) list
type swap = int * int
type live_range = int * int

let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 6
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse] [ranges] [mappings] [swaps] [system]");
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

let parse_tuple s =
	Scanf.sscanf s "%d,%d" (fun x y -> (x, y))

let parse_ranges (s : string) : live_range list =
	Str.split (Str.regexp " ") s
	|> List.map parse_tuple

let parse_mappings (s : string) : mapping list =
	Str.split (Str.regexp ";") s
	|> List.map (Str.split (Str.regexp " "))
	|> List.map (List.map parse_tuple)

let parse_swaps : string -> swap list list = parse_mappings

let build_swap ((x, y) : swap) : stmt =
	Seq (Exp (BinGate(CNOT, x, y)), Seq (Exp (BinGate(CNOT, y, x)), Exp (BinGate(CNOT, x, y))))

let merge_stmt_lst l : stmt =
	match l with
	| [] -> failwith "can't reduce empty list"
	| h :: t -> List.fold_left (fun acc e -> Seq (acc, e)) h t
 
let rec interleave l1 l2 : stmt =
	match l1 with
	| [] -> failwith "empty"
	| [s] -> s
	| h1 :: h2 :: t ->
		Seq (h1, Seq (List.hd l2, interleave (h2 :: t) (List.tl l2)))

 let _ =
 	let prog = parse_file () in
 	let ranges = parse_ranges Sys.argv.(2) in
 	let mappings = parse_mappings Sys.argv.(3) in
 	let swaps = parse_swaps Sys.argv.(4) in
 	let system = Sys.argv.(5) in
 	let partial_progs = List.map (fun (s, e) -> split prog s e) ranges in
 	let mapped_progs = List.map2 apply_mapping partial_progs mappings in
 	let mapped_stmts = List.map (fun p -> p.prog) mapped_progs in
 	let swaps = 
 		swaps
 		|> List.map (List.map build_swap)
 		|> List.map merge_stmt_lst in
 	let final_stmt = interleave mapped_stmts swaps in
 	let compiler = if system = "IBM" then compile_to_ibm else compile_to_pyquil in
	Printf.printf "%s" (compiler {prog with prog = final_stmt})
