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
		| Measure (q, c) -> Printf.sprintf "circ.measure(q[%d], q[%d])" q c
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