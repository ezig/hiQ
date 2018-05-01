import re
import sys
import os
import json
from shutil import copyfile

cCLINGO_FILE = "circuit.lp"
cCLINGO_TMP = "tmp.lp"
cHIQ_TMP = "tmp.hiq"

class Solution:
    def __init__(self, mapping, optimizations):
        self.mapping = mapping
        self.opt = optimizations

    def get_mapping_string(self):
    	return " ".join(map(lambda x: "%d,%d" % (x[0], x[1]), self.mapping))

def parse_json(data):
	nqubits = data["qubits"]
	inputs = data["input"]
	circuit = data["circuit"]

	prog = "cbit %d\nqubit %d\n" % (nqubits, nqubits)

	for i in range(len(inputs)):
		if inputs[i] == 1:
			prog += "X(%d);\n" % i

	measures = []
	for i in range(nqubits):
		measures.append("measure(%d, %d)" % (i, i))

	for g in circuit:
		if (g["type"] == "x") and (len(g["controls"]) != 0):
			prog += "CNOT(%d, %d);\n" % (g["targets"][0], g["controls"][0])
		else:
			prog += "%s(%d);\n" % (g["type"].upper(), g["targets"][0])

	prog += ";\n".join(measures)

	with open(cHIQ_TMP, "w") as f:
		f.write(prog)

def find_swaps(gate_info):
	cnots = {}
	for l in gate_info:
		m = re.match("\((\d+), (\d+), (\d+), \"CNOT\"\)", l)
		if m is not None:
			ctl = m.groups()[0]
			targ = m.groups()[1]
			sorted_bits = sorted([ctl, targ])

			key = ",".join(sorted_bits)
			if key in cnots:
				cnots[key] += 1
			else:
				cnots[key] = 1

	swaps = []
	for bits, count in cnots.iteritems():
		if count == 2:
			swaps.append(bits)

	return swaps

def load_computer(name):
	with open("computers/%s" % name, "r") as f:
		lines = map(lambda s: s.strip(), f.readlines())
		system = lines[0]
		nqubits = int(lines[1])

	constraints = "\n".join(map(lambda s: "physgate%s." % s, lines[2:]))
	with open(cCLINGO_TMP, "a") as f:
		f.write(constraints)
		f.write("\n")

	return system, nqubits, find_swaps(lines)

def get_solutions(lines):
	sols = []
	opts = []

	for l in lines:
		if l.startswith("abstophys"):
			def get_mapping(s):
				match = re.match("abstophys\((\d+),(\d+)\)", s)
				return (int(match.groups()[0]), int(match.groups()[1]))

			sols.append(map(get_mapping, l.split()))
		elif l.startswith("Optimization"):
			m = re.match("Optimization: (\d+)", l)
			opts.append(m.groups()[0])

	return map(lambda x: Solution(x[0], x[1]), zip(sols, opts))

def compile(hiq_file, computer):
	copyfile(cCLINGO_FILE, cCLINGO_TMP)

	with open(hiq_file, "r") as f:
		_ = f.readline()

		qubits = f.readline()
		qubit_matches = re.match("qubit (\d+)", qubits)
		abs_qubits = int(qubit_matches.groups()[0])

	system, phys_qubits, swaps = load_computer(computer)

	mapper_p = os.popen("./hiq_mapper.byte %s" % (hiq_file))
	edges = set(mapper_p.read().split())
	_ = mapper_p.close()

	with open(cCLINGO_TMP, "a") as f:
		f.write("\n".join(edges))
		f.write("\n#const n_abs = %d.\n" % (abs_qubits - 1))
		f.write("#const n_phys = %d.\n" % (phys_qubits - 1))

	clingo_p = os.popen("clingo --opt-mode=enum --models 0 %s 2>&1" % cCLINGO_TMP)
	solution = clingo_p.read()
	_ = clingo_p.close()

	if re.search('UNSATISFIABLE', solution) is not None:
		print("Unsatisfiable program.")
		exit(1)

	solutions = get_solutions(solution.splitlines())
	solutions = sorted(solutions, key = lambda x: x.opt)

	compiler_p = os.popen("./hiq.byte %s \"%s\"" % (hiq_file, solutions[0].get_mapping_string()))
	res = compiler_p.read()
	_ = compiler_p.close()

	return res

def compile_from_json(json_str, computer):
	parse_json(json_str)
	return compile(cHIQ_TMP, computer)

def main():
	if sys.argv[1].endswith("json"):
		with open(sys.argv[1], "r") as data:
			data = json.load(data)
		print(compile_from_json(data, sys.argv[2]))
	else:
		print(compile(sys.argv[1], sys.argv[2]))

if __name__ == '__main__':
	main()
