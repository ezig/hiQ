import re
import sys
import os
import json
from shutil import copyfile
import itertools

cCLINGO_FILE = "circuit.lp"
cCLINGO_TMP = "tmp.lp"
cCLINGO_TMP2 = "tmptmp.lp"
cHIQ_TMP = "tmp.hiq"
cHIQ_TMP2 = "tmptmp.hiq"

class Solution:
    def __init__(self, mapping, optimizations):
        self.mapping = mapping
        self.opt = int(optimizations)

    def get_mapping_string(self):
    	return " ".join(map(lambda x: "%d,%d" % (x[0], x[1]), self.mapping))

    def get_needed_swaps(self, other):
    	swaps = []
    	for m in self.mapping:
    		o = filter(lambda o: m[0] == o[0], other.mapping)[0]
    		if m[1] != o[1]:
    			swaps.append(tuple(sorted((m[1], o[1]))))

    	return set(swaps)
 
class Candidate:
	def __init__(self, psols, l_ranges, swaps):
		self.psols = psols
		self.l_ranges = l_ranges
		self.swaps = swaps

	def initial_sol(self):
		return self.psols[0]

	def final_sol(self):
		return self.psols[-1]

	def merge(self, other, swaps):
		return Candidate(self.psols + other.psols, self.l_ranges + other.l_ranges, self.swaps + [swaps] + other.swaps)

	def nswaps(self):
		return len(self.swaps)

	def opt(self):
		return sum(map(lambda x: x.opt, self.psols))

def parse_json(data):
	nqubits = data["qubits"]
	inputs = data["input"]
	circuit = data["circuit"]

	prog = "cbit %d\nqubit %d\n" % (nqubits, nqubits)

	for i in range(len(inputs)):
		if inputs[i] == 1:
			prog += "X(%d);\n" % i

	# measures = []
	# for i in range(nqubits):
	# 	measures.append("measure(%d, %d)" % (i, i))

	for g in circuit:
		if (g["type"] == "x") and (len(g["controls"]) != 0):
			prog += "CNOT(%d, %d);\n" % (g["targets"][0], g["controls"][0])
		elif g["type"] == "measure":
			targ = g["targets"][0]
			prog += "measure(%d, %d);\n" % (targ, targ)
		else:
			prog += "%s(%d);\n" % (g["type"].upper(), g["targets"][0])

	prog = prog[:-2]
	# prog += ";\n".join(measures)

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
			swaps.append(tuple(map(int, bits.split(","))))

	return swaps

def swap_possible(needed, possible):
	return len(set(needed).difference(set(possible))) == 0

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

def run_shell(cmd):
	p = os.popen(cmd)
	out = p.read()
	res = p.close()

	return out, res

def solve_file(hiq_file):
	copyfile(cCLINGO_TMP, cCLINGO_TMP2)

	mapper_p = os.popen("./hiq_mapper.byte %s" % (hiq_file))
	edges = set(mapper_p.read().split())
	_ = mapper_p.close()

	with open(cCLINGO_TMP2, "a") as f:
		f.write("\n".join(edges))

	clingo_p = os.popen("clingo --opt-mode=enum --models 0 %s 2>&1" % cCLINGO_TMP2)
	solution = clingo_p.read()
	_ = clingo_p.close()

	if re.search('UNSATISFIABLE', solution) is not None:
		return []

	return get_solutions(solution.splitlines())

def split_file(hiq_file, s, e):
	_ = run_shell("./hiq_splitter.byte %s %d %d > %s" % (hiq_file, s, e, cHIQ_TMP2))

def compile(hiq_file, computer):
	copyfile(cCLINGO_FILE, cCLINGO_TMP)

	with open(hiq_file, "r") as f:
		_ = f.readline()

		qubits = f.readline()
		qubit_matches = re.match("qubit (\d+)", qubits)
		abs_qubits = int(qubit_matches.groups()[0])

	system, phys_qubits, avail_swaps = load_computer(computer)

	with open(cCLINGO_TMP, "a") as f:
		f.write("\n#const n_abs = %d.\n" % (abs_qubits - 1))
		f.write("#const n_phys = %d.\n" % (phys_qubits - 1))

	ngates = int(run_shell("./hiq_splitter.byte %s" % (hiq_file))[0])

	arr = [[[] for i in range (ngates)] for j in range(ngates)]

	def d(r, c):
		split_file(hiq_file, r, c)
		full_sols = solve_file(cHIQ_TMP2)
		arr[r][c].extend(map(lambda s: Candidate([s], [(r, c)], []), full_sols))

		for j in range(r, c):
			if len(arr[r][j]) == 0:
				d(r, j)

			if len(arr[j + 1][c]) == 0:
				d(j + 1, c)

			merge_candidates = itertools.product(arr[r][j], arr[j + 1][c])

			merge_successes = []
			for cl, cr in merge_candidates:
				needed_swaps = cl.final_sol().get_needed_swaps(cr.initial_sol())
				# Solutions involving no swaps would have already been found
				if needed_swaps == []:
					continue
				if swap_possible(needed_swaps, avail_swaps):
					arr[r][c].append(cl.merge(cr, needed_swaps))

	d(0, ngates - 1)

	sols = sorted(arr[0][ngates - 1], key = lambda x : (len(x.swaps), x.opt()))
	if len(sols) == 0:
		return "Unsatisfiable"

	best_sol = sols[0]
	
	ranges = " ".join(map(lambda l: "%d,%d" % (l[0], l[1]), best_sol.l_ranges))
	mappings = ";".join(map(lambda s: s.get_mapping_string(), best_sol.psols))
	swaps = ";".join(map(lambda s: " ".join(map(lambda p: "%d,%d" % (p[0], p[1]), s)), best_sol.swaps))

	res, _ = run_shell("./hiq.byte %s \"%s\" \"%s\" \"%s\" %s" % (hiq_file, ranges, mappings, swaps, system))

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
