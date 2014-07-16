#!/usr/bin/env python

"""Get all configurable PROCESS variables."""

__author__ = "James Edwards"
__copyright__ = "Copyright 2014, CCFE"
__credits__ = ["James Edwards"]
__license__ = "*shrug*"
__version__ = "-1"
__maintainer__ = "James Edwards"
__email__ = "James.Edwards@ccfe.ac.uk"
__status__ = "Development"

import os

GLOBAL_VARS_FILE = os.path.join(os.getcwd(), os.pardir, "global_variables.f90")

def extract_var_desc(line):
	"""Return a tuple containing variable name, description."""
	#print("PROCESSING:", line)
	chunks = line.split(":")
	var_name = chunks[0].split()[1]
	description = chunks[1]
	return var_name, description

def get_stripped_lines(filename):
	"""Generator to extract all lines from filename (without spaces)."""
	with open(filename) as fh:
		for line in fh:
			yield line.strip()

def extract_all_vars(filename):
		
	variables = {}
	for line in get_stripped_lines(filename):
		if line.startswith("!+ad_vars"):
			try:
				variable_name, description = extract_var_desc(line)
				variables[variable_name] = description
			except Exception as e:
				print(e, line)
	return variables

if __name__ == "__main__":
	print(extract_all_vars(GLOBAL_VARS_FILE).keys())
