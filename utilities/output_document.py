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
DESCRIPTIONS = proc_dict.DICT_DESCRIPTIONS

SECTIONS = collections.OrderedDict()

# Physics
SECTIONS["Physics"] = {"con":"physics", "mod":"Physics Variables", "tag":"#physics"}

# TF Coil
SECTIONS["TF Coils"] = {"con":"tfcoil", "mod":"Tfcoil Variables", "tag":"#tf-coils"}

# PF Coil
SECTIONS["PF Coils"] = {"con":"pfcoil", "mod":"Pfcoil Variables", "tag":"#pf-coils"}

# First Wall Blanket
SECTIONS["First Wall and Blanket"] = {"con":"fwbs", "mod":"Fwbs Variables", "tag":"#first-wall-and-blanket"}

# Times
SECTIONS["Times"] = {"con":"times", "mod":"Times Variables", "tag":"#times"}

# =========================================================


def print_contents(out_file):
    """
    Print contents page
    """

    out_file.write("# Contents\n\n")

    for k, v in SECTIONS.items():
        out_file.write("[{0}]({1})\n\n".format(k, v["tag"]))


def print_section(m_file, in_file, out_file, key):
    """
    Print section
    """

    title = key
    con_mod = SECTIONS[key]["con"]
    mod_name = SECTIONS[key]["mod"]

    out_file.write("## {0}\n".format(title))

    out_file.write("### Constraints\n")

    out_file.write("| Constraint Number | Description |\n")
    out_file.write("| --- | --- |\n")

    constraints = in_file.data["icc"].value
    for item in constraints:
        if con_mod in MODULES.keys():
            if item in MODULES[con_mod]:
                out_file.write("| {0} | {1} |\n".format(str(item), 
                    ICC_FULL[str(item)]["name"]))
    
    if con_mod not in MODULES.keys():
        out_file.write("| - | - |\n")

    out_file.write("### Iteration Variables\n")

    out_file.write("### Inputs\n")

    out_file.write("| Input | Value | Description |\n")
    out_file.write("| --- | --- | --- |\n")

    for item in in_file.data.keys():
        if item in MODULES_FULL[mod_name]:
            out_file.write("| {0} | {1} | {2} |\n".
                format(item, in_file.data[item].value, DESCRIPTIONS[item].split("\n")[0]))

    out_file.write("### Outputs\n")    


def print_modules(m_f, in_f):
    """

    """

    icc = in_f.data["icc"].value 

    for item in icc:
        module = proc_dict.DICT_ICC_MODULE[str(item)]
        MODULES.setdefault(module,[]).append(item)

    output_file = open("output_document.md", "w")

    print_contents(output_file)

    for k, v in SECTIONS.items():
        print_section(m_f, in_f, output_file, k)

    output_file.close()

def main(cargs):
    """
    Main
    """

    # read mfile
    mfile = MFile(filename=cargs.f)

    # read input file
    input_file = InDat(filename=cargs.f, start_line=mfile.mfile_end)

    print_modules(mfile, input_file)

    call(["grip", "output_document.md", "--export", "output_document.html"])

    print("Over...")

    return


# =========================================================

if __name__ == "__main__":

    PARSER = argparse.ArgumentParser(
        description="Create PROCESS output document."
        "For info contact james.morris2@ukaea.uk")

    PARSER.add_argument("-f", metavar='MFILENAME', type=str,
                        default="", help='specify PROCESS MFILE')

    PARSER.add_argument("-s", "--save", help="Save output to file called"
                        "cfetr_comp.txt", action="store_true")

    COMMAND_ARGS = PARSER.parse_args()

    if COMMAND_ARGS.f:
        main(COMMAND_ARGS)
    else:
        print("Please enter a reference MFILE with -f!")