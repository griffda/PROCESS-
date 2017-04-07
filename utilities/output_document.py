"""

  Create PROCESS output document

  James Morris 03/04/2017
  CCFE

"""

# Third party libraries
import sys
import json
import argparse
import collections
from subprocess import call

# PROCESS libraries
from process_io_lib.in_dat import InDat
from process_io_lib.mfile import MFile
try:
    import process_io_lib.process_dicts as proc_dict
except ImportError:
    print("The Python dictionaries have not yet been created. Please run",
          " 'make dicts'!")
    exit()

# Constants
MODULES = dict()
ICC_FULL = proc_dict.DICT_ICC_FULL
MODULES_FULL = proc_dict.DICT_MODULE

# =========================================================

def print_physics(m_file, in_file, out_file):
    """
    Print physics summary page
    """

    out_file.write("# Physics\n")

    out_file.write("## Constraints\n")

    out_file.write("| Constraint Number | Description |\n")
    out_file.write("| --- | --- |\n")

    constraints = in_file.data["icc"].value
    for item in constraints:
        if item in MODULES["physics"]:
            out_file.write("| {0} | {1} |\n".format(str(item), 
                ICC_FULL[str(item)]["name"]))

    out_file.write("## Iteration Variables\n")

    it_vars = in_file.data["ixc"].value
    # for item in it_vars:
    #     if item in MODULES_FULL["physics_variables"]:
    #         output_file.write("| {0} | {1} |\n".format(str(item), 
    #             ICC_FULL[str(item)]["name"]))

    out_file.write("## Inputs\n")

    out_file.write("## Outputs\n")

def print_modules(m_f, in_f):
    """

    """

    icc = in_f.data["icc"].value 

    for item in icc:
        module = proc_dict.DICT_ICC_MODULE[str(item)]
        MODULES.setdefault(module,[]).append(item)

    output_file = open("output_document.md", "w")

    # print_contents(m_f, in_f)

    print_physics(m_f, in_f, output_file)

    output_file.close()


def main(cargs):
    """

    """

    # read input file
    input_file = InDat(filename=cargs.f)

    # read mfile
    mfile = MFile(filename=cargs.m)

    print_modules(mfile, input_file)

    call(["grip", "output_document.md", "--export", "output_document.html"])
    #  grip --gfm --context=username/repo issue.md

    print("Over...")

    return


# =========================================================

if __name__ == "__main__":

    PARSER = argparse.ArgumentParser(
        description="Create PROCESS output document."
        "For info contact james.morris2@ukaea.uk")

    PARSER.add_argument("-f", metavar='INNAME', type=str,
                        default="", help='specify PROCESS IN.DAT')

    PARSER.add_argument("-m", metavar='MFILENAME', type=str,
                        default="", help='specify PROCESS MFILE')

    PARSER.add_argument("-s", "--save", help="Save output to file called"
                        "cfetr_comp.txt", action="store_true")

    COMMAND_ARGS = PARSER.parse_args()

    if COMMAND_ARGS.f and COMMAND_ARGS.m:
        main(COMMAND_ARGS)
    else:
        print("Please enter a reference MFILE with -m and IN.DAT with -f!")
