"""
   This module produces the process_dicts.json file used by PROCESS Python 
   utilities. The Ford documentation program reads in the PROCESS source, 
   creates a project object which contains the structure of PROCESS used for
   documenting the code within Ford, and then calls 
   create_dicts.create_dicts(project). This module then creates dictionaries
   of variables which are used by the Python utilities. The dicts are created 
   from hardcoded values, PROCESS source parsing and the Ford project object, 
   and then dumped into the JSON file for later use.

   Utilities can then call create_dicts.get_dicts() to load the dicts from the
   saved JSON file and use them.

   This ultimately provides Python utilities with the ability to access variable
   information in the PROCESS Fortran source code.
"""

import re
import os
import logging
import copy
import argparse
from pprint import pformat
from collections import defaultdict
import sys
from rootdir import ROOTDIR
import json

#process source directory
SOURCEDIR = ROOTDIR

# Path to the process_dicts.json output file
DICTS_FILE_PATH = os.path.join(os.path.dirname(__file__),
    'process_io_lib/process_dicts.json')

# Variables, arrays and dictionaries that aren't read from the source files so
# have to be hard coded

# ifail value of a successful process run
IFAIL_SUCCESS = 1

# default values for making a plot file from MFILE.DAT
PARAMETER_DEFAULTS = ["rmajor", "aspect", "rminor", "bt", "powfmw",
    "pnetelmw", "te", "pdivt", "strtf1", "strtf2"]

# parameters that start with f, but are not f-values
NON_F_VALUES = ['fcohbop', 'fvsbrnni', 'feffcd', 'fcutfsu', 'fimpvar']

# PROCESS TF Coil types
DICT_TF_TYPE = {1: "ITER Nb3Sn", 2: "Bi-2212", 3: "NbTi", 4: "Nb3Sn", 
    5: "WST Nb3Sn", 6: "REBCO", 7: "YBCO"}

# FIMP Values
DICT_FIMP = {
    "fimp(1)":"Hydrogen (fraction calculated by code)",
    "fimp(2)":"Helium",
    "fimp(3)":"Beryllium",
    "fimp(4)":"Carbon",
    "fimp(5)":"Nitrogen",
    "fimp(6)":"Oxygen",
    "fimp(7)":"Neon",
    "fimp(8)":"Silicon",
    "fimp(9)":"Argon",
    "fimp(10)":"Iron",
    "fimp(11)":"Nickel",
    "fimp(12)":"Krypton",
    "fimp(13)":"Xenon",
    "fimp(14)":"Tungsten"
    }

# Optimisation variable dictionary
DICT_OPTIMISATION_VARS = {
    1: 'Plasma major radius',
    2: 'ratio fusion power:input power',
    3: 'neutron wall load',
    4: 'total TF + PF coil power',
    5: 'ratio fusion power:injection power',
    6: 'cost of electricity',
    7: 'constructed cost',
    8: 'aspect ratio',
    9: 'divertor heat load',
    10: 'toroidal field on axis',
    11: 'injection power',
    12: 'hydrogen production capital cost',
    13: 'hydrogen production rate',
    14: 'pulse length',
    15: 'plant availability factor',
    16: 'linear combination of major radius (minimised) and pulse length (maximised)',
    17: 'net electrical output'
    }

output_dict = {}
# Dict of nested dicts e.g. output_dict['DICT_DESCRIPTIONS'] = 
# {descriptions_dict}
# Dicts stored in output_dict are used to create other derivative dicts 

# Classes for the various dictionary types
class Dictionary(object):
    # Base Dictionary class for all dicts
    def __init__(self, name):
        self.name = name # Dict name
        self.dict = {} # Contains the dict
        self.dict[self.name] = {} # Structures this dict: key = dict name, 
        # value = nested dict of variable info

    def make_dict(self):
        # Make the dictionary
        pass

    def post_process(self):
        # Perform any processing after making the dict
        pass

    def publish(self):
        # Add the finished dictionary to the output dict
        output_dict.update(self.dict)

class ProjectDictionary(Dictionary):
    # Dicts that rely on the Ford project object
    def __init__(self, name, project, value_type):
        Dictionary.__init__(self, name)
        self.project = project # The Ford project object
        self.value_type = value_type
        # The attribute in the project to make a dict for

    def make_dict(self):
        # Assign the variable name key to the value of an attribute of 
        # the var object in the project ([var_name] = some_value_of_var)
        for module in self.project.modules:
            for var in module.variables:
                self.dict[self.name][var.name] = getattr(var, self.value_type)

class SourceDictionary(Dictionary):
    # Dictionary created from Fortran source
    def __init__(self, name, dict_creator_func):
        Dictionary.__init__(self, name)
        # Function that creates the dict
        self.dict_creator_func = dict_creator_func

    def make_dict(self):
        # Make entire nested dict from function
        self.dict[self.name] = self.dict_creator_func()

class HardcodedDictionary(Dictionary):
    # Dictionary created from a hardcoded dict in this file
    def __init__(self, name, hardcoded_dict):
        Dictionary.__init__(self, name)
        self.dict[self.name] = None
        # Hardcoded value isn't always a dict; override to None to allow the 
        # value to be set to any type
        self.hardcoded_dict = hardcoded_dict

    def make_dict(self):
        # Set the nested value to a hardcoded int, list or dict
        self.dict[self.name] = self.hardcoded_dict

class VariableDescriptions(ProjectDictionary):
    # Dictionary of variable descriptions
    def __init__(self, project):
        ProjectDictionary.__init__(self, 'DICT_DESCRIPTIONS', project, 'doc')

    def make_dict(self):
        # Assign the variable name key to the var description
        for module in self.project.modules:
            for var in module.variables:
                self.dict[self.name][var.name] = getattr(var, self.value_type)

    def post_process(self):
        for var_name, var_desc in self.dict[self.name].items():
            # Strip the <p> and </p> tags from the description; not 
            # required in output
            self.dict[self.name][var_name] = re.sub('</?p>', '', var_desc)

class DefaultValues(ProjectDictionary):
    # Dictionary of default values of variables
    def __init__(self, project):
        ProjectDictionary.__init__(self, 'DICT_DEFAULT', project, 'initial')

    def post_process(self):
        # Most default values are numbers saved as strings, but some 
        # are lists of these
        # Attempt to convert strings to sci notation: 1.57812D3 to 1.578E+03
        working_dict = self.dict[self.name]

        for key, value in working_dict.items():
            if value:
                # Guard against None
                # Is it a list?
                if type(value) is list or value[0:2] == "(/":
                    # Ford's arrays begin with "(/"
                    value = self.convert_list_to_sci_not(value)
                else:
                    value = self.convert_value_to_sci_not(value)

                working_dict[key] = value

    def convert_value_to_sci_not(self, value):
        # Convert a value to scientific notation: 1D3 to 1E+03
        original_value = value

        # Might not convert successfully; in which case return original value
        try:
            value = value.lower()
            value = value.replace('d', 'E')
            value = float(value)
            value = '{:.3E}'.format(value)
            return value
        except:
            # Failed conversion; don't change anything
            return original_value

    def convert_list_to_sci_not(self, working_list):
        # Change the formatting for lists
        # Change Ford string "(/1D3, 4D2, 2D7/)" or regular list [1D3, 4D2, 2D7]
        # to Python scientific notation list ['1E+03', '4E+02', '2E+07']
        processed_list = []

        if type(working_list) is not list:
            # Ford list: convert string to an array of strings
            working_list = working_list.replace('(/', '')
            working_list = working_list.replace('/)', '')
            working_list = working_list.split(', ')

        # Convert list values to scientific notation
        for value in working_list:
            value = self.convert_value_to_sci_not(value)
            processed_list.append(value)
        return processed_list

class Modules(ProjectDictionary):
    # Dictionary mapping modules to arrays of its module-level variables
    def __init__(self, project):
        ProjectDictionary.__init__(self, 'DICT_MODULE', project, 'name')

    def make_dict(self):
        for module in self.project.modules:
            # Create individual module dict
            self.dict[self.name][module.name] = []
            for var in module.variables:
                # Add module-level variables
                self.dict[self.name][module.name].append(var.name)

def to_type(string):
    """Given a string, attempts to convert the string to a numerical
       value. If the string can't be simply converted, the function
       looks to see if it begins with a integer and returns that (since
       some lines have clarification text after the number). If this
       also fails, return string.strip()
       Args:
            string --> String to be converted
       Returns:
            value --> Either a float, int or string depending on input
    """

    try:
        if "." in string:
            #try a float conversion
            string_mod = string.strip().lower().replace("d", "e")
            return float(string_mod)
        else:
            #try an int conversion
            return int(string.strip())
    except ValueError:
        match = re.match(r"\s*(\d+)", string)
        if match:
            #if the string starts with an integer return that
            return int(match.group(1))

        #otherwise return the string unchanged but with whitespace removed
        return string.strip()

def grep(file, regexp, flags=re.U):
    """Implements an in-python grep. Returns the lines that match
       as a list.
       Args:
            file --> Name of file to be read
            regexp --> Regular expression to search for
            flags --> re flags to use in search. Default is re.U which has
                        no effect
       Returns:
            lines --> List of matching lines
    """

    lines = []

    try:
     with open(file, "r", encoding="utf-8") as file_open:
        for line in file_open.readlines():
          if re.search(regexp, line, flags):
            lines.append(line)
        file_open.close()
    except OSError:
      logging.warning("File : %s not found\n", file)
    return lines

def slice_file(file, re1, re2):
    """Returns a slice of a file that is bounded by lines containing a
       substring matching the given regular expressions. The first match
       to re1 in the file marks the start of the slice, the first match
       to re2 after that marks the end of the slice. The list of lines
       returned includes the two bounding lines.
       Args:
            file --> Name of file to read through
            re1 --> Starting regular expression
            re2 --> Ending regular expression
       Returns:
            lines --> List of lines from file between re1 and re2 inclusive
    """

    filetext = open(file,"r",encoding="utf-8").readlines()
    start = None
    for i in range(len(filetext)):
        #look for first match
        if re.search(re1, filetext[i]):
            start = i
            break
    if start == None:
        logging.warning("Could not match %s in file %s\n", re1, file)
        return ""
    end = None
    for i in range(start, len(filetext)):
        #look for second match
        if re.search(re2, filetext[i]):
            end = i
            break
    if end == None:
        logging.warning("Could not match %s in file %s\n", \
                        re2, file)
        return ""
    #return slice
    return filetext[start:end+1]

def remove_comments(line):
    """Function to remove comments from a fortran line. Works by simply
       removing everything after the first '!' in the line. This will
       cause problems in the case of '!' characters contained within strings
       so am assuming this won't happen. Need to change this.
       Args:
            line --> Line to strip comments from
       Returns
            modified_line --> Line with comments removed
    """
    if "!" in line:
        line = line[: line.find("!")]
    line = line.strip()
    if line == "":
        return line
    if line[-1] == "&":
        line = line[:-1]
    return line

def dict_ixc2nsweep():
    """Returns a dict mapping ixc_no to nsweep_no, if both exist for a 
    particular variable. Looks in scan.f90 for mapping from nsweep_no to 
    iteration variable name, and uses ixc_full to map variable name to ixc_no.

    Example of a fragment we are looking for:
        case (1)
            aspect = swp(iscn)
            vlabel = 'aspect = ' ; xlabel = 'Aspect_ratio'

    Example dictionary entry:
        DICT_IXC2NSWEEP['1'] = '1'
    """

    ixc2nsweep = {}
    file = SOURCEDIR + "/scan.f90"
    #slice the file to get the switch statement relating to nsweep
    lines = slice_file(file, r"select case \(nwp\)", r"case default")

    #remove extra lines that aren't case(#) or varname = sweep(iscan) lines
    modlines = []
    for line in lines[1:-1]:
        if "case" in line or "swp(iscn)" in line:
            line = remove_comments(line).replace(' ', '')
            modlines.append(line)

    #create a dictionary that maps iteration variable names to ixc_no
    ixc_full = dict_ixc_full()
    ixc_simple_rev = {}
    for num, value in ixc_full.items():
        ixc_simple_rev[value["name"]] = num

    for i in range(len(modlines)):
        #get the number from the case statement
        match = re.match(r"case\((\d+)\)", modlines[i])
        if match:
            num = match.group(1)
            #if the case statement matched, get the variable name
            #from the next line
            match_2 = re.match(r"(.*?)=swp\(iscn\)", modlines[i+1])
            if not match_2:
                logging.warning("Error in dict_ixc2nsweep\n")
            else:
                name = match_2.group(1)
                if name in ixc_simple_rev:
                    ixcnum = ixc_simple_rev[name]
                    ixc2nsweep[ixcnum] = num

    return ixc2nsweep

def dict_nsweep2ixc():
    """Returns a dict mapping nsweep_no to ixc_no; the inverse of 
    dict_ixc2nsweep"""

    # Use dict_ixc2nsweep from output_dict to produce dict_nsweep2ixc
    ixc2nsweep = output_dict['DICT_IXC2NSWEEP']
    nsweep2ixc = {b:a for a, b in ixc2nsweep.items()}
    return nsweep2ixc

def dict_var_type():
    """Function to return a dictionary mapping variable name to variable type
       eg. 'real_variable' or 'int_array'. Looks in input.f90 at the process
       functions that read in variables from IN.DAT.

       Example of line we are looking for:
           call parse_real_variable('BETA', beta, 0.0D0, 1.0D0, &

       Example dictionary entry:
           DICT_VAR_TYPE['beta'] = 'real_variable'
    """
    di = {}
    regexp = r"call parse_(real|int)_(array|variable)\("
    lines = grep(SOURCEDIR + "/input.f90", regexp)
    for line in lines:
        args = line.split('(')[1]
        name = args.split(',')[1].strip()
        var_type = re.search(regexp, line).group(1)
        scalar = re.search(regexp, line).group(2)
        di[name] = var_type + "_" + scalar
    return di

def dict_icc_full():
    """Function to return a dictionary matching str(icc_no) to a dictionary
       containing the name of that constraint equation. Looks in
       numerics.f90 at !+ad_varc lines in lablcc to get icc_no and
       variable names.

       Example of a lablxc line we are looking for:
           !+ad_varc  <LI> ( 5) * beta

       Example dictionary entry:
           DICT_IXC_FULL['5'] = {'name' : 'beta'}
    """

    di = dict()

    # get slice of file from 'lablxc = (/' to '/)'
    lcctext = slice_file(SOURCEDIR + "/numerics.f90", r"lablcc = \(/", r"/\)")

    regexp = r"""
               !!               #var comment begins with !!

               .*?              #irrelevant stuff until open brackets

               \(\s*(\d+)\s*\)  #an integer in brackets possibly bounded by
                                #whitespace. Capture the number in group 1

               \s*\*?\s*        #whitespace and a possible asterix

               ([\w ]+)        #the name of the variable should be captured
                               #in group 2
              """
    lcc = []
    #ignore first and last lines
    for line in lcctext[1:-1]:
        match = re.search(regexp, line, re.VERBOSE)
        if match:
            num = int(match.group(1))
            name = match.group(2).strip()
            lcc.append(name)
            assert num == len(lcc)

    for i in range(len(lcc)):
        assign = {"name" : lcc[i]}
        di[str(i+1)] = assign

    return di

def dict_input_bounds():
    """Returns a dictionary matching variable names to dictionary containing
       upper and lower bounds that PROCESS checks variable lies between when
       reading IN.DAT. Looks in input.f90 for parse_real_variable and
       parse_int_variable.

       Example of a line we are looking for:
            call parse_real_variable('BETA', beta, 0.0D0, 1.0D0, &

       Example dictionary entry:
            DICT_INPUT_BOUNDS['beta'] = {'lb' : 0.0, 'ub' : 1.0}
    """
    di = {}
    failedlines = []
    regexp = r"call parse_(real|int)_variable\((.*)"
    lines = grep(SOURCEDIR + "/input.f90", regexp)

    for line in lines:
        match = re.search(regexp, line)
        try:
            name = match.group(2).split(',')[1].strip()
            lb = to_type(match.group(2).split(',')[2])
            ub = to_type(match.group(2).split(',')[3])
            if match.group(1) == "real":
                assert isinstance(lb, float)
            else:
                assert isinstance(lb, int)
            assert ub >= lb
            di[name] = {"lb" : lb, "ub" : ub}
        except (IndexError, AttributeError, AssertionError, TypeError):
            failedlines.append(line)

    if len(failedlines) != 0:
        warn_string = "dict_input_bounds failed to parse:\n"
        for line in failedlines:
            warn_string += "%s\n" % line.strip()
        logging.warning(warn_string)

    return di

def dict_nsweep2varname():
    # This function creates the nsweep2varname dictionary from the fortran code
    # It maps the sweep variable number to its variable name

    di = {}
    file = SOURCEDIR + "/scan.f90"

    #slice the file to get the switch statement relating to nsweep
    lines = slice_file(file, r"select case \(nwp\)", r"case default")

    #remove extra lines that aren't case(#) or varname = sweep(iscan) lines
    modlines = []
    for line in lines[1:-1]:
        if "case" in line or "swp(iscn)" in line:
            line = remove_comments(line).replace(' ', '')
            modlines.append(line)

    for i in range(len(modlines)//2):
        line1 = modlines[i*2]
        no = line1.replace('case(', '')
        no = no.replace(')', '')
        line2 = modlines[i*2+1]
        varname = line2.replace('=swp(iscn)', '')
        di[no] = varname

    return di

def dict_ixc_full():
    """Function to return a dictionary matching str(ixc_no) to a dictionary
       containing the name, lower and upper bounds of that variable. Looks in
       numerics.f90 at !+ad_varc lines in lablxc to get ixc_no and
       variable names, and looks at boundu and boundl for upper and
       lower bounds.

       Example of a lablxc line we are looking for:
           lablxc(1) = 'aspect        '

       Example of a boundl line we are looking for:
           boundl(1) = 0.0D0

       Example of a boundu line we are looking for:
           boundu(1) = 1.0D0

       Example dictionary entry:
           DICT_IXC_FULL['5'] = {'name' : 'beta', 'lb' : 0.001, 'ub' : 1.0}
    """

    with open(SOURCEDIR + "/iteration_variables.f90") as myFile:
        lines = myFile.readlines()

    ixc_full = dict()

    for lline in lines:
        if "subroutine init_itv_" in lline and "end" not in lline:
            itv_num = lline.split("_")[-1].strip("\n").replace(" ", "")
            ixc_full[itv_num] = dict()

    for line in lines:

        if "lablxc" in line and "=" in line:
            if "lablxc(i)" not in line and "lablxc(ixc(i))" not in line:
                labl_num = line.split("(")[1].split(")")[0]
                labl = line.split("=")[-1].strip("\n").replace(" ", "").replace("'", "")
                ixc_full[labl_num]["name"] = labl
        
        if "boundl(" in line and "=" in line:
            if "boundl(i)" not in line and "boundl(ixc(i))" not in line:
                boundl_num = line.split("(")[1].split(")")[0]
                boundl_val = line.split("=")[-1].strip("\n").lower().replace("d", "e")
                ixc_full[boundl_num]["lb"]= float(boundl_val)
    
        if "boundu(" in line and "=" in line:
            if "boundu(i)" not in line and "boundu(ixc(i))" not in line:
                boundu_num = line.split("(")[1].split(")")[0]
                boundu_val = line.split("=")[-1].strip("\n").lower().replace("d", "e")
                ixc_full[boundu_num]["ub"]= float(boundu_val)

    return ixc_full

def dict_ixc_bounds():
    # Returns dictionary mapping iteration variable name to bounds
    ixc_full = output_dict['DICT_IXC_FULL']
    ixc_bounds = {}
    for key, value in ixc_full.items():
        lb = value["lb"]
        ub = value["ub"]
        temp = {"lb" : lb, "ub" : ub}
        ixc_bounds[value["name"]] = temp

    return ixc_bounds

def dict_ixc_default():
    # Returns dictionary mapping iteration variable name to default value
    ixc_default = {}
    default = output_dict['DICT_DEFAULT']
    ixc_full = output_dict['DICT_IXC_FULL']

    for key, value in ixc_full.items():
        name = value["name"]

        if name in default:
            ixc_default[name] = default[name]
        else:
            logging.warning("print_dict_ixc could not find %s"\
                            " in DICT_DEFAULT\n", name)

    return ixc_default
    
def dict_ixc_simple():
    # Returns dictionary mapping ixc no to iteration variable name
    ixc_simple = {}
    ixc_full = output_dict['DICT_IXC_FULL']
    for key, value in ixc_full.items():
        ixc_simple[key] = value["name"]

    return ixc_simple

def dict_ixc_simple_rev():
    # Returns dictionary mapping iteration variable name to ixc no
    ixc_simple = output_dict['DICT_IXC_SIMPLE']
    ixc_simple_rev = {b:a for a, b in ixc_simple.items()}
    
    return ixc_simple_rev

def create_dicts(project):
    # There are 3 sources of dicts: from the Ford project object, from the 
    # Fortran source and hardcoded ones from this file

    dict_objects = []
    # Different dict objects, e.g. variable descriptions
    
    # Make dict objects
    # Some dicts depend on other dicts already existing in output_dicts, so
    # be careful if changing the order!
    dict_objects.extend([
        VariableDescriptions(project),
        DefaultValues(project),
        Modules(project),
        HardcodedDictionary('DICT_TF_TYPE', DICT_TF_TYPE),
        HardcodedDictionary('DICT_FIMP', DICT_FIMP),
        HardcodedDictionary('DICT_OPTIMISATION_VARS', DICT_OPTIMISATION_VARS),
        HardcodedDictionary('IFAIL_SUCCESS', IFAIL_SUCCESS),
        HardcodedDictionary('PARAMETER_DEFAULTS', PARAMETER_DEFAULTS),
        HardcodedDictionary('NON_F_VALUES', NON_F_VALUES),
        SourceDictionary('DICT_INPUT_BOUNDS', dict_input_bounds),
        SourceDictionary('DICT_NSWEEP2VARNAME', dict_nsweep2varname),
        SourceDictionary('DICT_VAR_TYPE', dict_var_type),
        SourceDictionary('DICT_ICC_FULL', dict_icc_full),
        SourceDictionary('DICT_IXC2NSWEEP', dict_ixc2nsweep),
        SourceDictionary('DICT_NSWEEP2IXC', dict_nsweep2ixc),
        SourceDictionary('DICT_IXC_FULL', dict_ixc_full),
        SourceDictionary('DICT_IXC_BOUNDS', dict_ixc_bounds),
        SourceDictionary('DICT_IXC_DEFAULT', dict_ixc_default),
        SourceDictionary('DICT_IXC_SIMPLE', dict_ixc_simple),
        SourceDictionary('DICT_IXC_SIMPLE_REV', dict_ixc_simple_rev)
    ])

    # Make individual dicts within dict objects, process, then add to output_dict
    for dict_object in dict_objects:
        dict_object.make_dict()
        dict_object.post_process()
        dict_object.publish()

    # Save output_dict as JSON, to be used by utilities scripts
    with open(DICTS_FILE_PATH, 'w') as dicts_file:
        json.dump(output_dict, dicts_file, indent=4, sort_keys=True)

def get_dicts():
    # Return dicts loaded from the JSON file for use in Python utilities
    try:
        with open(DICTS_FILE_PATH, 'r') as dicts_file:
            return json.load(dicts_file)
    except:
        print("Error loading the dicts JSON file")
        exit()