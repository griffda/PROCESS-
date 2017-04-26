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
from grip import export

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
SECTIONS["Build"] = {"con":"build", 
                     "mod":"Build Variables",
                     "tag":"#build"}
SECTIONS["Buildings"] = {"con":"buildings", 
                         "mod":"Buildings Variables",
                         "tag":"#buildings"}
SECTIONS["Costs"] = {"con":"cost",
                     "mod":"Cost Variables",
                     "tag":"#costs"}
SECTIONS["Current Drive"] = {"con":"current_drive",
                         "mod":"Current Drive Variables",
                         "tag":"#current-drive"}
SECTIONS["Divertor"] = {"con":"divertor",
                        "mod":"Divertor Variables",
                        "tag":"#divertor"}
SECTIONS["Divertor Kallenbach"] = {"con":"divertor_kallenbach",
                        "mod":"Divertor Kallenbach Variables",
                        "tag":"#divertor-kallenbach"}
SECTIONS["First Wall and Blanket"] = {"con":"fwbs", 
                                     "mod":"Fwbs Variables", 
                                     "tag":"#first-wall-and-blanket"}
SECTIONS["Heat Transport"] = {"con":"heat_transport", 
                        "mod":"Heat Transport Variables", 
                        "tag":"#heat-transport"}
SECTIONS["Impurity Radiation"] = {"con":"impurity_radiation", 
                                 "mod":"Impurity Radiation Module", 
                                 "tag":"#impurity-radiation"}
SECTIONS["Numerics"] = {"con":"numerics", 
                        "mod":"Numerics", 
                        "tag":"#numerics"}
SECTIONS["PF Power"] = {"con":"pf_power", 
                        "mod":"Pf Power Variables", 
                        "tag":"#pf-power"}
SECTIONS["PF Coils"] = {"con":"pfcoil", 
                        "mod":"Pfcoil Variables", 
                        "tag":"#pf-coils"}
SECTIONS["Physics"] = {"con":"physics", 
                       "mod":"Physics Variables", 
                       "tag":"#physics"}
SECTIONS["Pulse"] = {"con":"pulse", 
                       "mod":"Pulse Variables", 
                       "tag":"#pulse"}
SECTIONS["Scan"] = {"con":"scan", 
                       "mod":"Scan Module", 
                       "tag":"#scan"}
SECTIONS["Stellarator"] = {"con":"stellarator", 
                       "mod":"Stellarator Variables", 
                       "tag":"#stellarator"}
SECTIONS["TF Coils"] = {"con":"tfcoil", 
                        "mod":"Tfcoil Variables", 
                        "tag":"#tf-coils"}
SECTIONS["Times"] = {"con":"times", 
                     "mod":"Times Variables",
                     "tag":"#times"}
SECTIONS["Vacuum"] = {"con":"vacuum", 
                     "mod":"Vacuum Variables",
                     "tag":"#vacuum"}

RADIAL_BUILD = collections.OrderedDict()
RADIAL_BUILD[1] = {"name": "bore", "des": "Machine Bore"}
RADIAL_BUILD[2] = {"name": "ohcth", "des": "Central Solenoid"}
RADIAL_BUILD[3] = {"name": "precomp", "des": "CS precompression"}
RADIAL_BUILD[4] = {"name": "gapoh", "des": "Gap between precomp and TF inboard"}
RADIAL_BUILD[5] = {"name": "tfcth", "des": "TF coil inboard leg"}
RADIAL_BUILD[6] = {"name": "deltf", "des": "Gap between TF inboard and thermal shield"}
RADIAL_BUILD[7] = {"name": "thshield", "des": "Thermal shield"}
RADIAL_BUILD[8] = {"name": "gapds", "des": "Gap between thermal shield and vacuum vessel"}
RADIAL_BUILD[9] = {"name": "ddwi + shldith", "des": "Inboard vacuum vessel (and shielding)"}
RADIAL_BUILD[10] = {"name": "vvblgap", "des": "Gap between vacuum vessel and inboard blanket"}
RADIAL_BUILD[11] = {"name": "blnkith", "des": "Inboard blanket"}
RADIAL_BUILD[12] = {"name": "fwith", "des": "Inboard first wall"}
RADIAL_BUILD[13] = {"name": "scrapli", "des": "Inboard scrape-off layer"}
RADIAL_BUILD[14] = {"name": "rminor", "des": "Plasma inboard minor radius"}
RADIAL_BUILD[15] = {"name": "rminor", "des": "Plasma outboard minor radius"}
RADIAL_BUILD[16] = {"name": "scraplo", "des": "Outboard scrape-off layer"}
RADIAL_BUILD[17] = {"name": "fwoth", "des": "Outboard first wall"}
RADIAL_BUILD[18] = {"name": "blnkoth", "des": "Outboard blanket"}
RADIAL_BUILD[19] = {"name": "vvblgap", "des": "Gap between vacuum vessel and outboard blanket"}
RADIAL_BUILD[20] = {"name": "ddwi + shldoth", "des": "Outboard vacuum vessel and shielding"}
RADIAL_BUILD[21] = {"name": "gapsto", "des": "Gap between outboard vacuum vessel and thermal shield"}
RADIAL_BUILD[22] = {"name": "thshield", "des": "Outboard thermal shield"}
RADIAL_BUILD[23] = {"name": "tftsgap", "des": "Gap between outboard thermal shield and TF outboard"}
RADIAL_BUILD[24] = {"name": "tfthko", "des": "TF coil outboard leg"}

# VERTICAL_BUILD = collections.OrderedDict()
# VERTICAL_BUILD[1] = {"name": "", "des": ""}

EXCLUSION = ["ixc", "icc"]

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


def check_empty(m_file, in_file, key):
    """
    Check not an empty section
    """
    title = key
    con_mod = SECTIONS[key]["con"]
    mod_name = SECTIONS[key]["mod"]
    tot_count = 0
    con_count = 0
    it_count = 0
    in_count = 0
    out_count = 0
    result = dict()

    constraints = in_file.data["icc"].value
    it_vars = in_file.data["ixc"].value
    it_var_list = [IXC_FULL[str(itvar)]["name"] for itvar in it_vars]

    for item in constraints:
        if con_mod in MODULES.keys():
            tot_count += 1
            con_count += 1

    for item in it_vars:
    
        item_name = IXC_FULL[str(item)]["name"]
        item_description = DESCRIPTIONS[item_name].split("\n")[0]

        if item_name in MODULES_FULL[mod_name]:
            tot_count += 1
            it_count += 1
    
    for item in in_file.data.keys():
        if item in MODULES_FULL[mod_name]:
            if item not in it_var_list:
                tot_count += 1
                in_count += 1

    for item in m_file.data.keys():
        if item in MODULES_FULL[mod_name]:
            item_value = m_file.data[item].get_scan(-1)
            if item not in in_file.data.keys():
                tot_count += 1
                out_count += 1
    
    result["tot_count"] = tot_count
    result["con_count"] = con_count
    result["it_count"] = it_count
    result["in_count"] = in_count
    result["out_count"] = out_count

    return result

def print_contents(m_file, in_file, out_file):
    """
    Print contents page
    """

    out_file.write("**Username**: {0}\n\n".format(m_file.data["username"].get_scan(-1)))
    out_file.write("**Date**: {0}\n\n".format(m_file.data["date"].get_scan(-1)))
    out_file.write("**PROCESS Version**: {0}\n\n".format(m_file.data["procver"].get_scan(-1)))
    out_file.write("**Run Description** {0}\n\n".format(m_file.data["runtitle"].get_scan(-1)))

    out_file.write("# Contents\n\n")

    out_file.write("[Machine Geometry](#machine-geometry)\n\n")

    for k, v in SECTIONS.items():

        result =  check_empty(m_file, in_file, k)

        if result["tot_count"] != 0:

            out_file.write("[{0}]({1})\n\n".format(k, v["tag"]))

def print_section(m_file, in_file, out_file, key, result):
    """
    Print section
    """

    title = key
    con_mod = SECTIONS[key]["con"]
    mod_name = SECTIONS[key]["mod"]

    out_file.write("## {0}\n".format(title))

    # Get data
    constraints = in_file.data["icc"].value
    it_vars = in_file.data["ixc"].value
    it_var_list = [IXC_FULL[str(itvar)]["name"] for itvar in it_vars]
    bounds = in_file.data["bounds"].value
    it_var_values = get_itvar_values(m_file)

    # Constraints
    if result["con_count"] != 0:
        
        out_file.write("### Constraints\n")

        out_file.write("| Constraint | Description | F-Value Name | F-Value Value | Limit Name | Limit | Value |\n")
        out_file.write("| --- | --- | --- | --- | --- | --- | --- |\n")

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
                    con_lim_name = "consistency"
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
    
    if result["it_count"] != 0:

        out_file.write("### Iteration Variables\n")

        out_file.write("* Values in **bold** are **not default** but user inputs.\n\n")

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
                    format(str(item), code(item_name), item_value, item_description, starting_value, low_bound, up_bound))
    
    if result["in_count"] != 0:

        out_file.write("### Inputs\n")

        out_file.write("| Input | Value | Description |\n")
        out_file.write("| --- | --- | --- |\n")
        
        input_list = list()

        for item in in_file.data.keys():
            if item in MODULES_FULL[mod_name]:
                if item not in it_var_list:
                    if item not in EXCLUSION:

                        item_value = in_file.data[item].value
                        item_des = DESCRIPTIONS[item].split("\n")[0]

                        if item in it_var_list:
                            item_value = bold(item_value)
                        
                        input_list.append(item)

                        out_file.write("| {0} | {1} | {2} |\n".
                            format(code(item), item_value, item_des))

    if result["out_count"] != 0:

        out_file.write("### Outputs\n")

        out_file.write("| Output | Value | Description |\n")
        out_file.write("| --- | --- | --- |\n")

        for item in m_file.data.keys():
            if item in MODULES_FULL[mod_name]:

                item_value = m_file.data[item].get_scan(-1)
                if item not in in_file.data.keys():

                    out_file.write("| {0} | {1} | {2} |\n".
                        format(code(item), item_value, DESCRIPTIONS[item].split("\n")[0]))


def print_geometry(m_f, in_f, out_f):
    """
    Print machine geometry
    """

    out_f.write("# Machine Geometry \n")

    out_f.write("### Radial Build \n")
    out_f.write("| Name | Value | Sum | Description |\n")
    out_f.write("| --- | --- | --- | --- |\n")

    r_build_sum = 0

    for k, v in RADIAL_BUILD.items():

        item_name = RADIAL_BUILD[k]["name"]
        item_des = RADIAL_BUILD[k]["des"]
        
        if "+" in item_name:
            item_1 = item_name.split("+")[0].replace(" ", "")
            item_2 = item_name.split("+")[1].replace(" ", "")
            item_1_value = m_f.data[item_1].get_scan(-1)
            item_2_value = m_f.data[item_2].get_scan(-1)
            item_value = item_1_value + item_2_value
        else:
            item_value = m_f.data[item_name].get_scan(-1)
        
        r_build_sum += item_value
        
        out_f.write("| {0} | {1:.3f} | {2:.3f} | {3} |\n".
                        format(code(item_name), item_value, r_build_sum, item_des))

    # out_f.write("### Vertical Build \n")
    # out_f.write("| Name | Value | Sum | Description |\n")
    # out_f.write("| --- | --- | --- | --- |\n")

    # v_build_sum = 0

    # for k, v in VERTICAL_BUILD.items():

    #     item_name = VERTICAL_BUILD[k]["name"]
    #     item_des = VERTICAL_BUILD[k]["des"]
        
    #     if "+" in item_name:
    #         item_1 = item_name.split("+")[0].replace(" ", "")
    #         item_2 = item_name.split("+")[1].replace(" ", "")
    #         item_1_value = m_f.data[item_1].get_scan(-1)
    #         item_2_value = m_f.data[item_2].get_scan(-1)
    #         item_value = item_1_value + item_2_value
    #     else:
    #         item_value = m_f.data[item_name].get_scan(-1)
        
    #     v_build_sum += item_value
        
    #     out_f.write("| {0} | {1:.3f} | {2:.3f} | {3} |\n".
    #                     format(code(item_name), item_value, v_build_sum, item_des))

def print_modules(m_f, in_f):
    """

    """

    icc = in_f.data["icc"].value 

    for item in icc:
        module = proc_dict.DICT_ICC_MODULE[str(item)]
        MODULES.setdefault(module,[]).append(item)

    output_file = open("output_document.md", "w")

    print_contents(m_f, in_f, output_file)

    print_geometry(m_f, in_f, output_file)

    for k, v in SECTIONS.items():
        
        html_name = "{0}.html".format(SECTIONS[k]["tag"][1:])

        result = check_empty(m_f, in_f, k)

        if result["tot_count"] != 0:

            print_section(m_f, in_f, output_file, k, result)

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

    export(path="output_document.md", password="e35a21bfce5462bebbecc2e43d12bf4ec2ba469d", 
        render_wide=True, render_inline=True, out_filename="output_document.html", 
        title="PROCESS Output")

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