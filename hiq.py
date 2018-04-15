import re
import sys
import os
from shutil import copyfile

cCLINGO_FILE = "circuit.lp"
cCLINGO_TMP = "tmp.lp"

def main():
	mapper_p = os.popen("./hiq_mapper.byte %s" % (sys.argv[1]))
	edges = set(mapper_p.read().split())
	_ = mapper_p.close()

	copyfile(cCLINGO_FILE, cCLINGO_TMP)

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

	compiler_p = os.popen("./hiq.byte %s \"%s\"" % (sys.argv[1], mapping))
	res = compiler_p.read()
	_ = compiler_p.close()

	print(res)

if __name__ == '__main__':
	main()
