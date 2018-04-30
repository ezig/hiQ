import re
import sys
import os
import json
from shutil import copyfile

cCLINGO_FILE = "circuit.lp"
cCLINGO_TMP = "tmp.lp"
cHIQ_TMP = "tmp.hiq"

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

def load_computer(name):
	with open("computers/%s" % name, "r") as f:
		lines = map(lambda s: s.strip(), f.readlines())
		system = lines[0]

	constraints = "\n".join(map(lambda s: "physgate%s." % s, lines[1:]))
	with open(cCLINGO_TMP, "a") as f:
		f.write(constraints)
		f.write("\n")

	return system

def compile(hiq_file, computer):
	system = load_computer(computer)

	mapper_p = os.popen("./hiq_mapper.byte %s" % (hiq_file))
	edges = set(mapper_p.read().split())
	_ = mapper_p.close()

	with open(cCLINGO_TMP, "a") as f:
		f.write("\n".join(edges))

	clingo_p = os.popen("clingo %s" % cCLINGO_TMP)
	solution = clingo_p.read()
	_ = clingo_p.close()

	if re.search('UNSATISFIABLE', solution) is not None:
		print("Unsatisfiable program.")
		exit(1)

	def get_mapping(s):
		match = re.match("abstophys\((\d+),(\d+)\)", s)
		return "%s,%s" % (match.groups()[0], match.groups()[1])

	mapping = " ".join(map(get_mapping, solution.splitlines()[4].split()))

	compiler_p = os.popen("./hiq.byte %s \"%s\"" % (hiq_file, mapping))
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
	copyfile(cCLINGO_FILE, cCLINGO_TMP)
	main()
