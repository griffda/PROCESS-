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
ICC_VARS = proc_dict.DICT_ICC_VARS
IXC_FULL = proc_dict.DICT_IXC_FULL
IXC_DEF = proc_dict.DICT_IXC_DEFAULT
MODULES_FULL = proc_dict.DICT_MODULE
DEFAULTS = proc_dict.DICT_DEFAULT
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

def bold(x):
    return("**" + str(x) + "**")


def code(x):
    return("`" + str(x) + "`")


def bold_italics(x):
    return("**_" + str(x) + "_**")


def get_itvar_values(mfi):
    """
    Get iteration variable values from MFILE
    """
    itvar_vals = dict()
    
    for item in mfi.data.keys():
        if item[:5] == "itvar":
            name = mfi.data[item].var_description
            value = mfi.data[item].get_scan(-1)
            itvar_vals[name] = value

    return itvar_vals


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

    out_file.write("| Constraint | Description | F-Value | F-Value Value | Limit | Limit Value | Actual Value |\n")
    out_file.write("| --- | --- | --- | --- | --- | --- | --- |\n")

    constraints = in_file.data["icc"].value
    it_vars = in_file.data["ixc"].value
    it_var_list = [IXC_FULL[str(itvar)]["name"] for itvar in it_vars]
    bounds = in_file.data["bounds"].value
    it_var_values = get_itvar_values(m_file)

    for item in constraints:
        if con_mod in MODULES.keys():

            con_name = ICC_FULL[str(item)]["name"]

            if ICC_VARS[str(item)] != "consistency" and ICC_VARS[str(item)] != "empty":
                if "f" in ICC_VARS[str(item)].keys():
                    con_f_val_name = ICC_VARS[str(item)]["f"]
                    if con_f_val_name not in m_file.data.keys():
                        if con_f_val_name in it_var_values.keys():
                            con_f_val = it_var_values[con_f_val_name]
                        else:
                            if con_f_val_name in in_file.data.keys():
                                con_f_val = in_file.data[con_f_val_name].value
                            else:
                                con_f_val = IXC_DEF[con_f_val_name]
                                con_f_val = bold(con_f_val)
                    else:
                        con_f_val = m_file.data[con_f_val_name].get_scan(-1)

                    con_lim_name = ICC_VARS[str(item)]["v"]

                    if con_lim_name in m_file.data.keys():
                        con_lim = m_file.data[con_lim_name].get_scan(-1)
                    else:
                        if con_lim_name in DEFAULTS.keys():
                            con_lim = DEFAULTS[con_lim_name]
                        else:
                            con_lim = bold("calculated")
            else:
                con_f_val_name = "-"
                con_lim_name = "-"
                con_f_val = "-"
                con_lim = "-"
            
            if con_f_val != "-" and con_lim != "-" and con_lim != bold("calculated"):
                actual_value = "{0:.2e}".format(float(con_lim)*float(con_f_val))
                con_lim = "{0:.2e}".format(float(con_lim))
                con_f_val = "{0:.2e}".format(float(con_f_val))
            else:
                actual_value = "-"

            if item in MODULES[con_mod]:
                out_file.write("| {0} | {1} | {2} | {3} | {4} | {5} | {6} |\n".
                    format(str(item), con_name, con_f_val_name, con_f_val, con_lim_name, con_lim, actual_value))
    
    if con_mod not in MODULES.keys():
        out_file.write("| - | - |\n")

    out_file.write("### Iteration Variables\n")

    out_file.write("* Bounds in **bold** are **not default** but user inputs.\n")
    out_file.write("* Initial values in **bold** have user defined starting values\n\n")

    out_file.write("| No. | Name | Final Value | Description | Starting Value | Lower Bound | Upper Bound |\n")
    out_file.write("| --- | --- | --- | --- | --- | --- | --- |\n")

    for item in it_vars:
    
        item_name = IXC_FULL[str(item)]["name"]
        item_description = DESCRIPTIONS[item_name].split("\n")[0]
        
        if item_name in it_var_values.keys():
            item_value = it_var_values[item_name]
        else:
            item_value = m_file.data[item_name].get_scan(-1)
        
        if item_name in in_file.data.keys():
            starting_value = in_file.data[item_name].value
            starting_value = bold(starting_value)
        else:
            starting_value = IXC_DEF[item_name]
        
        if item_name in MODULES_FULL[mod_name]:

            if str(item) in bounds.keys():
                if "l" in bounds[str(item)].keys():
                    low_bound = bounds[str(item)]["l"]
                    low_bound = bold(low_bound)
                else:
                    low_bound = IXC_FULL[str(item)]["lb"]

                if "u" in bounds[str(item)].keys():
                    up_bound = bounds[str(item)]["u"]
                    up_bound = bold(up_bound)
                else:
                    up_bound = IXC_FULL[str(item)]["ub"]
                
            else:
                low_bound = IXC_FULL[str(item)]["lb"]
                up_bound = IXC_FULL[str(item)]["ub"]

            out_file.write("| {0} | {1} | {2} | {3} | {4} | {5} | {6} |\n".
                format(str(item), code(item_name), bold(item_value), item_description, starting_value, low_bound, up_bound))
    
    if con_mod not in MODULES.keys():
        out_file.write("| - | - |\n")

    out_file.write("### Inputs\n")

    out_file.write("| Input | Value | Description |\n")
    out_file.write("| --- | --- | --- |\n")
    
    input_list = list()

    for item in in_file.data.keys():
        if item in MODULES_FULL[mod_name]:
            if item not in it_var_list:

                item_value = in_file.data[item].value
                item_des = DESCRIPTIONS[item].split("\n")[0]

                if item in it_var_list:
                    item_value = bold(item_value)
                
                input_list.append(item)

                out_file.write("| {0} | {1} | {2} |\n".
                    format(code(item), item_value, item_des))

    out_file.write("### Outputs\n")

    out_file.write("| Output | Value | Description |\n")
    out_file.write("| --- | --- | --- |\n")

    for item in m_file.data.keys():
        if item in MODULES_FULL[mod_name]:

            item_value = m_file.data[item].get_scan(-1)
            if item not in in_file.data.keys():

                out_file.write("| {0} | {1} | {2} |\n".
                    format(code(item), item_value, DESCRIPTIONS[item].split("\n")[0]))
            
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
        
        # markdown_name = "{0}.md".format(SECTIONS[k]["tag"][1:])
        html_name = "{0}.html".format(SECTIONS[k]["tag"][1:])

        # output_file = open(markdown_name, "w")

        print_section(m_f, in_f, output_file, k)

        # output_file.close()

        # call(["grip", "--pass", "e35a21bfce5462bebbecc2e43d12bf4ec2ba469d", 
        #     markdown_name, "--export", html_name])

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

    # call_list = list()
    # for k, v in SECTIONS.items():
    #     markdown_file = SECTIONS[k]["tag"][1:] + ".md"
    #     call_list.append(markdown_file)
    # call_list.insert(0, "pandoc")
    # call_list.insert(1, "-f")
    # call_list.insert(2, "markdown_github")
    # call_list.append("-o")
    # call_list.append("output_document.pdf")
    # print(call_list)
    # call(call_list)
    call(["grip", "--pass", "e35a21bfce5462bebbecc2e43d12bf4ec2ba469d", 
            "output_document.md", "--export", "output_document.html"])
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