type ugate = H
type bingate = CNOT

type cbit = int
type qubit = int

type exp =
| Measure of qubit * cbit
| UGate of ugate * qubit
| BinGate of bingate * qubit * qubit

type stmt =
| Exp of exp
| Seq of stmt * stmt
| If of exp * stmt * (stmt option)

type program = {cbits : int; qubits : int; prog: stmt}

let ugate_to_string b =
	match b with
	| H -> "H" 

let bingate_to_string u =
	match u with
	| CNOT -> "CNOT"

let ast_to_string ({cbits; qubits; prog} : program) : string =
	let exp_to_string e =
		match e with
		| Measure (q, c) -> Printf.sprintf "measure(%d, %d)" q c
		| UGate (u, q) -> Printf.sprintf "%s(%d)" (ugate_to_string u) q
		| BinGate (b, q1, q2) -> Printf.sprintf "%s(%d, %d)" (bingate_to_string b) q1 q2 in
	let rec stmt_to_string s =
		match s with
		| Exp e -> exp_to_string e
		| Seq (s1, s2) -> 
			stmt_to_string s1 ^ ";\n" ^ stmt_to_string s2
		| If (e, s1, s2o) ->
			let e_str = exp_to_string e in
			let s1_str = stmt_to_string s1 in
			let if_str = "if (" ^ e_str ^ ") {\n" ^ s1_str ^ "\n}" in
			match s2o with
			| None -> if_str
			| Some s2 -> if_str ^ " else {\n" ^ stmt_to_string s2 ^ "\n}" in
	Printf.sprintf "cbit %d;\nqubit %d;\n\n%s" cbits qubits (stmt_to_string prog)
