###############################################################################
#                                                                             #
#                   f90wrap patch for PROCESS under Python3.9+                #
#                                                                             #
#                                                                             #
#   For reasons as yet unclear f90wrap produces a file process/fortran.py     #
#   with some entries being of the form:                                      #
#                                                                             #
#   f90wrap_example_function():                                               #
#       \                                                                     #
#       return value                                                          #
#                                                                             #
#   the cause of the '\' is unknown however on first look it appears to       #
#   occur when there is no docstring                                          #
#                                                                             #
#   This script removes these orphaned lines and should be run as:            #
#                                                                             #
#   python f90wrap_patch.py ${PROCESS_git_rep_ROOT}/process/fortran.py        #
#                                                                             #
#   after the PROCESS build has completed.                                    #
#                                                                             #
###############################################################################

import argparse
import os

parser = argparse.ArgumentParser('F90WrapPatch')

parser.add_argument('input_file', help='f90wrap generated python script to correct')

args = parser.parse_args()

print(f'''
---------------------------------------------------

            f90wrap patch for PROCESS

                K. Zarebski, 2020

    Input File  :   {args.input_file}

----------------------------------------------------
''')

if not os.path.exists(args.input_file):
    raise FileNotFoundError(f"File '{args.input_file}' was not found")

with open(args.input_file) as f:
    lines = f.readlines()

n_corrections = 0

for i, line in enumerate(lines):
    if line.strip().replace(' ', '') == '\\':
        lines[i] = ''
        n_corrections += 1

with open(args.input_file, 'w') as f:
    f.write(''.join(i for i in lines if i))

print(f'File has been updated, Removed {n_corrections}/{len(lines)} Lines')

