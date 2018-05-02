open Hiq_ast

let compile_to_ibm ({cbits; qubits; prog}) : string =
	let ugate_name u =
		match u with
		| H -> "h"
		| X -> "x" in
	let bingate_name b =
		match b with
		| CNOT -> "cx" in
	let exp_to_ibm e =	
		match e with
		| Measure (q, c) -> Printf.sprintf "circ.measure(q[%d], c[%d])" q c
		| UGate (u, q) -> Printf.sprintf "circ.%s(q[%d])" (ugate_name u) q
		| BinGate (b, q1, q2) ->
			Printf.sprintf "circ.%s(q[%d], q[%d])" (bingate_name b) q1 q2 in
	let rec stmt_to_ibm s =
		match s with
		| Exp e -> exp_to_ibm e
		| Seq (s1, s2) -> Printf.sprintf "%s\n%s" (stmt_to_ibm s1) (stmt_to_ibm s2)
		| _ -> failwith "If not implemented" in
	"qp = QuantumProgram()\n" ^
	(Printf.sprintf "q = qp.create_quantum_register(\"q\", %d)\n" qubits) ^
	(Printf.sprintf "c = qp.create_classical_register(\"c\", %d)\n" cbits) ^
	"circ = qp.create_circuit(\"circ\", [q], [c])\n" ^ (stmt_to_ibm prog)

let compile_to_pyquil ({cbits; qubits; prog}) : string =
	let ugate_name u =
		match u with
		| H -> "H"
		| X -> "X"
	in
	let bingate_name b =
		match b with
		| CNOT -> "CNOT"
	in		
	let e_gates e =
		match e with
		| Measure _ -> []
		| UGate (u, _) -> [ugate_name u]
		| BinGate (b, _, _) -> [bingate_name b]
	in
	let rec s_gates prog =
		match prog with
		| Exp e -> e_gates e
		| Seq (s1, s2) -> (s_gates s1) @ (s_gates s2)
		| _ -> failwith "If not implemented"
	in
	let exp_to_pyquil e =
		match e with
		| Measure (q, c) -> Printf.sprintf "p.measure(%d, %d)" q c
		| UGate (u, q) -> Printf.sprintf "p.inst(%s(%d))" (ugate_name u) q
		| BinGate (b, q1, q2) ->
			Printf.sprintf "p.inst(%s(%d, %d))" (bingate_name b) q1 q2
	in
	let rec stmt_to_pyquil s : string =
		match s with
		| Exp e -> exp_to_pyquil e 
		| Seq (s1, s2) -> (stmt_to_pyquil s1) ^ "\n" ^ (stmt_to_pyquil s2)
		| _ -> failwith "If not implemented"
	in
	"from pyquil.quil import Program\n" ^
	"from pyquil.gates import " ^ (String.concat ", " (List.sort_uniq compare (s_gates prog))) ^ "\n\n" ^
	"p = Program()\n" ^ (stmt_to_pyquil prog)
