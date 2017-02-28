#!/usr/bin/env python3

"""
   This is a program to automatically produce the process_dicts.py file
   used by PROCESS utility programs written in python. It does this by
   scanning the fortran source code. Most dictionaries are created using
   !+ad_vars and !+ad_varc comments that are used to produce the VarDes file
   so the output of this program should be consistent with the VarDes.

   The program prints genuine output to stdout and warnings to stderr, so
   stdout should be redirected to the intended dictionary file name.
   ./create_dicts.py > process_dicts.py

   For a list of dictionaries this program currently produces, see the function
   print_header()

   Tom Miller, August-September 2014

   P J Knight:
   25/09/2014 Modified SOURCEDIR usage to use ROOTDIR

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

#process source directory
SOURCEDIR = ROOTDIR

#These two dictionaries are used to help DICT_DEFAULT. FIXEDVALS is a list of
#integers that occur as constant array lengths in the fortran code eg.
#!+ad_vars  cptdin(ngc2) /4.0e4/: current per turn input for PF coil i (A)
#This allows the function interpret_array to expand arrays to the correct length

#FIXEDDEFS are hard coded default values of variables that are inserted into
#DICT_DEFAULT. This is used for adding important variables to the dictionary
#that the script fails to parse.
FIXEDVALS = {"ngc2" : 18, "nimp" : 14}
FIXEDDEFS = {"impdir" : ROOTDIR+"/data/impuritydata",
             "sweep" : [0.0] * 200
            }

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
    for line in open(file).readlines():
        if re.search(regexp, line, flags):
            lines.append(line)
    return lines

def grep_r(search_dir, regexp, flags=re.U, extension=""):
    """Implements an in-python grep through every file in the given directory
       No longer recursive. Returns the lines that match as a list.
       Args:
            search_dir --> Directory name
            regexp --> Regular expression to search for
            flags --> re flags to use in search. Default is re.U which has
                        no effect
            extension --> only search through files with this suffix extension
       Returns:
            lines --> List of matching lines
    """

    lines = []
    for file in os.listdir(search_dir):
        path = search_dir + "/" + file
        if os.path.isdir(path):  #  ignore subdirectories
            continue
        if not file.endswith(extension):
            continue
        try:
            lines += grep(path, regexp, flags)
        except UnicodeDecodeError:
            #this occurs when attempting to read binary files
            continue
    return lines

def find(search_dir, regexp, flags=re.U):
    """Searches through the search_dir to find files
       that contain regexp. Returns matching files as a list
       Args:
            search_dir --> Path to directory to search
            regexp --> regular expression to match to
            flags --> regular expression flags to modify search
       Returns:
            files --> List of matching file paths

    """
    files = []
    for file in os.listdir(search_dir):
        path = search_dir + "/" + file
        if os.path.isdir(path):
            continue
        try:
            text = open(path).readlines()
            for line in text:
                if re.search(regexp, line, flags):
                    files.append(path)
                    break
        except UnicodeDecodeError:
            continue
    return files

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

    filetext = open(file).readlines()
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

def interpret_array(name, value):
    """Function that interprets an array listed in the VarDes style
       and returns the corresponding python list. Will pad out with last value
       to correct length if it finds '..' or '...' at end of list or length of
       value is less than the expected length. Checks that every element of
       the list is the same type.
       Args:
            name --> Name of array and size eg. 'dcond(4)' or 'cptdin(ngc2)'
            value --> String value of array eg. '1,2,3' or '4.0D4, ...'
       Returns:
            ret --> Interpreted array
    """

    #get the expected size of the array if possible
    #if the substring between the brackets is a variable name rather than
    #an integer this will only work if the variable name appears in FIXEDVALS.
    #If it doesn't work, size will remain a string
    match = re.search(r"\w+\((.*?)\)", name)
    sizestr = match.group(1)

    if sizestr in FIXEDVALS:
        size = FIXEDVALS[sizestr]
    else:
        size = int(sizestr)

    list_string = value.split(',')
    if isinstance(size, int) and len(list_string) > size:
        logging.warning("length of %s is > than %i for %s in" + \
                        " interpret_array\n", value, size, name)
        raise IndexError

    if list_string[-1] == ".." or list_string[-1] == "...":
        assert len(list_string) > 1
        list_string = list_string[:-1]

    #if we know how big the array should be and list_string is currently too
    #small, pad it out with it's last value
    if isinstance(size, int) and len(list_string) != size:
        list_string += [list_string[-1]] * (size - len(list_string))

    ret = [to_type(x) for x in list_string]

    #check all elements of array have same type
    list_type = type(ret[0])
    assert all(isinstance(y, list_type) for y in ret)

    return ret

def get_array_from_fortran(array_name):
    """Attempts to find a value for array_name by looking for an assignment
       statement in the fortran.
       Args:
            array_name: Name of array to find eg. boundl
       Returns:
            value: Default value of array_name

    """
    rexp = array_name + r" = \(/"
    # find files that look like they have an assignment in
    filelist = find(SOURCEDIR, rexp)
    if len(filelist) > 1:
        print('The regular expression ', rexp,
              '\n  does not appear exactly once in the folder \n', SOURCEDIR,
              '\n The list of files is ', filelist, file=sys.stderr)
    assert(len(filelist) == 1)
    # slice the file between the parenthesis
    arr = slice_file(filelist[0], rexp, r"/\)")

    arr = [remove_comments(x) for x in arr]
    # combine whole array onto one line
    array_line = "".join(arr)
    # regex that gets the string between the brackets
    array_regex = rexp + r"(.*?)/\)"
    val_string = re.search(array_regex, array_line).group(1)
    # split the string and convert it to the correct type
    value = [to_type(x) for x in val_string.split(",")]

    list_type = type(value[0])
    assert all(isinstance(y, list_type) for y in value)
    return value


def print_dict(di, name, comment="", lam=lambda x: x[0], dict_type="dict()"):
    """Prints a dictionary to stdout, along with a comment. The sorting
       of the dictionary can be specified using a lambda function.

       The ouput takes the form:
       #comment
       name = dict()
       name[key1]    = value1
       name[key2]    = value2
       etc

       Args:
            di --> Dictionary to be printed
            name --> Name to use for dictionary
            comment --> Optional comment to print above dictionary
            lam --> Lambda function to control sorting. The lambda should
                    take as its argument a (key, value) tuple. The default
                    lambda will sort by the dictionary keys only.
            dict_type --> A string that sets the type of the dictionary eg.
                   'dict()' or 'defaultdict(list)'
    """

    if len(di) == 0:
        return
    print("\n#" + comment)
    print(name + " = " + dict_type)
    for key, value in sorted(di.items(), key=lam):
        #repr will add quote marks around values and keys that are strings
        if isinstance(value, dict):
            valuestr = pformat(value)
        else:
            valuestr = repr(value)
        indexstr = repr(key)
        print((name + "[" + indexstr + "]").ljust(30) + " = " + valuestr)


def dict_ixc2nsweep():
    """Returns a dictionary mapping str(ixc_no) to str(nsweep_no) if both
       exist for a particular variable. Looks in scan.f90 for mapping from
       nsweep_no to iteration variable name, and uses ixc_full to map
       variable name to ixc_no.

       Example of a fragment we are looking for:
           case (1)
              aspect = sweep(iscan)
              vlabel = 'aspect = ' ; xlabel = 'Aspect_ratio'

       Example dictionary entry:
            DICT_IXC2NSWEEP['1'] = '1'
    """

    di = {}
    file = SOURCEDIR + "/scan.f90"
    #slice the file to get the switch statement relating to nsweep
    lines = slice_file(file, r"select case \(nsweep\)", r"case default")

    #remove extra lines that aren't case(#) or varname = sweep(iscan) lines
    modlines = []
    for line in lines[1:-1]:
        if "case" in line or "sweep(iscan)" in line:
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
            match_2 = re.match(r"(.*?)=sweep\(iscan\)", modlines[i+1])
            if not match_2:
                logging.warning("Error in dict_ixc2nsweep\n")
            else:
                name = match_2.group(1)
                if name in ixc_simple_rev:
                    ixcnum = ixc_simple_rev[name]
                    di[ixcnum] = num

    return di


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

def dict_ixc_full():
    """Function to return a dictionary matching str(ixc_no) to a dictionary
       containing the name, lower and upper bounds of that variable. Looks in
       numerics.f90 at !+ad_varc lines in lablxc to get ixc_no and
       variable names, and looks at boundu and boundl for upper and
       lower bounds.

       Example of a lablxc line we are looking for:
           !+ad_varc  <LI> ( 5) * beta

       Example of a boundl line we are looking for:
           0.001D0, &  !  5

       Example of a boundu line we are looking for:
           1.000D0, &  !  5

       Example dictionary entry:
           DICT_IXC_FULL['5'] = {'name' : 'beta', 'lb' : 0.001, 'ub' : 1.0}
    """

    di = {}

    #get slice of file from 'lablxc = (/' to '/)'
    lxctext = slice_file(SOURCEDIR + "/numerics.f90", r"lablxc = \(/", r"/\)")

    regexp = r"""
               !\+ad_varc       #look for !+ad_varc

               .*?              #irrelevant stuff until open brackets

               \(\s*(\d+)\s*\)  #an integer in brackets possibly bounded by
                                #whitespace. Capture the number in group 1

               \s*\*?\s*        #whitespace and a possible asterix

               ([\w ]+)        #the name of the variable should be captured
                               #in group 2
              """
    lxc = []
    #ignore first and last lines
    for line in lxctext[1:-1]:
        match = re.search(regexp, line, re.VERBOSE)
        if match:
            num = int(match.group(1))
            name = match.group(2).strip()
            lxc.append(name)
            assert num == len(lxc)


    #get slice of file from 'boundu = (/' to '/)'
    boundutext = slice_file(SOURCEDIR + "/numerics.f90", \
                                r"boundu = \(/", r"/\)")
    regexp = r"""
                ([\d\.D-]+)     #fortran style floating point number contains
                                #digits, '.', 'D' or '-' characters. Capture to
                                #group 1

                .*?             #irrelevant stuff

                !\s*            #comment begins

                (\d+)           #capture ixc no. to group 2
              """

    boundu = []
    #ignore first and last lines
    for line in boundutext[1:-1]:
        match = re.search(regexp, line, re.VERBOSE)
        if match:
            val = float(match.group(1).replace('D', 'e'))
            num = int(match.group(2))
            boundu.append(val)
            assert num == len(boundu)

    #get slice of file from 'boundl = (/' to '/)'
    boundltext = slice_file(SOURCEDIR + "/numerics.f90", \
                            r"boundl = \(/", r"/\)")
    boundl = []
    #ignore first and last lines
    for line in boundltext[1:-1]:
        match = re.search(regexp, line, re.VERBOSE)
        if match:
            val = float(match.group(1).replace('D', 'e'))
            num = int(match.group(2))
            boundl.append(val)
            assert num == len(boundl)

    assert len(lxc) == len(boundu) and len(lxc) == len(boundl)
    for i in range(len(lxc)):
        assert boundl[i] <= boundu[i]
        assign = {"name" : lxc[i], "lb" : boundl[i], "ub" : boundu[i]}
        di[str(i+1)] = assign

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
               !\+ad_varc       #look for !+ad_varc

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


def dict_default():
    """Function to return a dictionary mapping input variable names to their
       default values. Looks in every file in the source directory for
       !+ad_vars and !+ad_varc lines and looks for the default value between
       forward slashes. Ignore variables marked "FIX". Any lines that can't be
       parsed but look like a default value is given are printed as warnings.

       Example of a line we are looking for:
           !+ad_vars  beta /0.042/ : total plasma beta (iteration variable 5)

       Example dictionary entry:
           DICT_DEFAULT['beta'] = 0.042
    """
    if dict_default.DICT_DEFAULT: #avoids repeat calls, see end of function
        return copy.deepcopy(dict_default.DICT_DEFAULT)

    di = dict_default.DICT_DEFAULT
    failedlines = []
    regexp = r"""
                !\+ad_var(s|c)  #look for !+ad_var(s|c)

                \s*             #possible whitespace

                ([\w():+,-]+) #variable name may contain '(' or ')'
                                #capture to group 2

                \s*             #possible whitespace

                /(.*?)/         #capture whatever is between brackets
                                #to group 3
              """
    variables_lines = grep_r(SOURCEDIR, regexp, re.VERBOSE, ".f90")
    for line in variables_lines:
        if "FIX" in line:
            continue
        try:
            match = re.search(regexp, line.strip(), re.VERBOSE)
            name = match.group(2).strip()
            value_string = match.group(3).strip()
            if not "(" in name:
                #if the variable is not an array
                value = to_type(value_string)
                di[name] = value
            else:
                #the variable is an array
                #remove the brackets from name to get the array name
                array_name = name[:name.find("(")]
                try:
                    #try to get the value from between the / /
                    value = interpret_array(name, value_string)
                except (IndexError, AttributeError, \
                        AssertionError, TypeError, ValueError):
                    value = get_array_from_fortran(array_name)

                di[array_name] = value

        except (IndexError, AttributeError, AssertionError, TypeError):
            #anything that fails gets added to the warning list
            failedlines.append(line)


    #get boundl and boundu from fortran
    di["boundl"] = get_array_from_fortran("boundl")
    di["boundu"] = get_array_from_fortran("boundu")

    #add the fixed values
    for name, value in FIXEDDEFS.items():
        di[name] = value

    #report the failed lines
    if len(failedlines) != 0:
        warn_string = "dict_default failed to parse:\n"
        for line in failedlines:
            warn_string += "%s\n" % line.strip()
        logging.warning(warn_string)

    #check dict_default against input lines in input.f90. Report differences
    regexp2 = r"call parse_(real|int)_(array|variable)\((.*)"
    test = grep(SOURCEDIR + "/input.f90", regexp2)
    for line in test:
        args = re.search(regexp2, line).group(3)
        try:
            name = args.split(',')[1].strip()
            if name not in di:
                di[name] = None
                logging.warning(" " + str(name) + " looks like an input " + \
                "variable but cout not find a default value. Setting " + \
                "default to None")
        except IndexError:
            continue

    dict_default.DICT_DEFAULT = di
    return di

dict_default.DICT_DEFAULT = {}

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

def remove_tags(line):
    """Removes some html tags that appear in autodoc comments

    """
    line = line.replace("<LI>", "")
    line = line.replace("<UL>", "")
    line = line.replace("</UL>", "")
    line = line.replace("<OL>", "")
    line = line.replace("</OL>", "")
    return line


def dict_descriptions():
    """Returns a dictionary matching variable names to variable description.
       Looks in all source files for !+ad_vars and !+ad_varc lines.

       Example of lines we are looking for:
          !+ad_vars  cfe0 /0.0/ : seeded high-Z impurity fraction (n_highZ / n_e)
          !+ad_varc               (imprad_model=0 only) (iteration variable 43)
       Example dictionary entry:
          DICT_DESCRIPTIONS['cfe0'] = "seeded high-Z impurity fraction (n_highZ / n_e)
                                        (imprad_model=0 only) (iteration variable 43)"

    """
    di = {}
    failedlines = []
    lines = grep_r(SOURCEDIR, r"!\+ad_var(s|c)", extension=".f90")
    #group lines by variable
    sep = "".join(lines).split("!+ad_vars")
    #sep is an array of multi line strings, one entry in array
    #for each variable
    #example entry: cfe0 /0.0/ : seeded ...
    #               !+ad_varc    (imprad_model ...
    for var_entry in sep:
        try:
            firstline = remove_tags(var_entry.split('\n')[0]).strip()
            otherlines = var_entry.split('\n')[1:]
            #name is before colon, description is after it
            ma = re.search(r"(\w+).*?:\s*(.*)", firstline.strip())
            name = ma.group(1).lower()
            desc = ma.group(2)
            if otherlines:
                for line in otherlines:
                    line = remove_tags(line)
                    if line.strip() == "":
                        continue

                    #remove the !+ad_varc part and add it onto the desc
                    ma2 = re.match(r"\s*!\+ad_varc\s*(.*)\Z", line)
                    desc += "\n" + ma2.group(1).strip()
            desc = desc.strip()
            di[name] = desc.capitalize()

        except (IndexError, AttributeError, AssertionError, TypeError):
            failedlines.append(var_entry)
            continue

    #make manual changes
    #description of "isc" does not include the explanation of
    #the meaning of the values isc can take. Get it from lablmm
    isc_desc = di["isc"].split('\n')[0]
    lablmm = di["tauscl"].split('\n')[1:]
    di["isc"] = "\n".join([isc_desc] + lablmm)

    #same thing for minmax and tauscl
    minmax_desc = di["minmax"]
    tauscl = di["lablmm"].split('\n')[1:]
    di["minmax"] = "\n".join([minmax_desc] + tauscl)

    #report problems
    if len(failedlines) != 0:
        warn_string = "dict_descriptions failed to parse:\n"
        for line in failedlines:
            warn_string += "%s\n" % line.strip()
        logging.warning(warn_string)
    return di

def dict_module():
    """This function parses the fortran to return a dictionary that maps
       module names to lists of variables.

    """
    module_dict = defaultdict(list)
    currentmodule = ""
    lines = grep_r(SOURCEDIR, r"^\s*!\+ad_(vars|name|type)", extension=".f90")
    #an ad_vars line is a variable line
    #an ad_name line followed by a ad_type Module line indicates the start
    #of a new module
    i = 0
    while i < len(lines):
        try:
            ma = re.match(r"^\s*!\+ad_(vars|name|type)\s*(\w+)", lines[i])
            #capture the text
            if ma.group(1) == "name":
                if re.match(r"^\s*!\+ad_type\s*Module", lines[i+1]):
                    #an ad_name line followed by an ad_type Module line means
                    #start a new module
                    modulename = ma.group(2).replace('_', ' ').title()
                    #replace '_' with ' ', capitalize, remove Variables suffix
                    #since redundant in UI
                    currentmodule = modulename

            elif ma.group(1) == "vars":
                varname = ma.group(2)
                #only want input variables
                if varname in dict_default():
                    module_dict[currentmodule].append(varname)

        except (IndexError, AttributeError, AssertionError, TypeError):
            logging.warning(" dict_module: line failed: " + lines[i])

        i += 1

    #Every variable should have a module
    assert not "" in module_dict

    #Every input variable should appear
    for key in dict_default().keys():
        found = False
        for module in module_dict.values():
            if key in module:
                found = True
                break
        if found == False:
            logging.warning(key + " does not appear in any module")

    return module_dict


def dict_nsweep2varname():

    """
    This function creates the nsweep2varname dictionary from the fortran code
    It maps the sweep variable number to its variable name
    """


    di = {}
    file = SOURCEDIR + "/scan.f90"

    #slice the file to get the switch statement relating to nsweep
    lines = slice_file(file, r"select case \(nsweep\)", r"case default")

    #remove extra lines that aren't case(#) or varname = sweep(iscan) lines
    modlines = []
    for line in lines[1:-1]:
        if "case" in line or "sweep(iscan)" in line:
            line = remove_comments(line).replace(' ', '')
            modlines.append(line)

    for i in range(len(modlines)//2):
        line1 = modlines[i*2]
        no = line1.replace('case(', '')
        no = no.replace(')', '')
        line2 = modlines[i*2+1]
        varname = line2.replace('=sweep(iscan)', '')
        di[no] = varname

    return di


def print_header():
    """Prints the file header
    """

    #look for a line with 'Release Date'
    rel_dat_list = grep(SOURCEDIR + "/process.f90", "Release Date")
    assert len(rel_dat_list) == 1
    dat_line = rel_dat_list[0]
    #the version number is right before 'Release Date'
    version_num = int(re.search(r"(\d+)\s*Release Date", dat_line).group(1))

    header = """\"\"\"
This file contains python dictionaries for use by utility programs.

List of dictionaries:
    PARAMETER_DEFAULTS     : Default values for making a plot file from MFILE.DAT
    NON_F_VALUES           : Parameters that start with f, but are not f-values
    DICT_TF_TYPE           : PROCESS TF coil types
    DICT_OPIMISATION_VARS  : Optimisation variable dictionary
    DICT_IXC2NSWEEP        : Maps ixc no. to nsweep, if applicable
    DICT_NSWEEP2IXC        : Maps nsweep to ixc no, if applicable
    DICT_VAR_TYPE          : Maps variable name to variable type
    DICT_IXC_FULL          : Maps ixc no. to ixc variable name and bounds
    DICT_IXC_BOUNDS        : Maps ixc variable name to bounds
    DICT_IXC_DEFAULT       : Maps ixc variable name to default value
    DICT_IXC_SIMPLE        : Maps ixc no to ixc variable name
    DICT_IXC_SIMPLE_REV    : Maps ixc variable name to ixc no
    DICT_DEFAULT           : Dictionary of default values for variables
    DICT_INPUT_BOUNDS      : Dictionary of bounds used by PROCESS when reading IN.DAT
    DICT_DESCRIPTIONS      : Dictionary of variable descriptions
    DICT_MODULE            : Ordered dictionary mapping module names to list
                             of associatied variables
    DICT_NSWEEP2VARNAME    : Dictionary mapping scan variable number to name

Automatically produced by create_dicts.py for PROCESS version %i
\"\"\"

from collections import defaultdict, OrderedDict
""" % version_num
    print(header)
    print("#Version number of process dictionaries created for")
    print("DICTIONARY_VERSION = %i" % version_num)


def print_hard_coded():
    """Prints the dictionaries that aren't read from the source files so
       have to be hard coded
    """

    out = """
#ifail value of a successful process run
IFAIL_SUCCESS = 1

# default values for making a plot file from MFILE.DAT
PARAMETER_DEFAULTS = ["rmajor", "aspect", "rminor", "bt", "powfmw", "pnetelmw",
                          "te", "pdivt", "strtf1", "strtf2"]

#parameters that start with f, but are not f-values
NON_F_VALUES = ['fcohbop', 'fvsbrnni', 'feffcd', 'fcutfsu', 'fimpvar']

# PROCESS TF Coil types
DICT_TF_TYPE = {1: "ITER Nb3Sn", 2: "Bi-2212", 3: "NbTi", 4: "Nb3Sn", 5: "WST Nb3Sn"}

# Optimisation variable dictionary
DICT_OPTIMISATION_VARS = {1: 'Plasma major radius',
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
                          15: 'plant availability factor'} """

    print(out)

def print_ixc2nsweep():
    """Prints:
        DICT_IXC2NSWEEP
        DICT_NSWEEP2IXC
    """

    #lambda to sort by integer value of key
    lam = lambda x: int(x[0])
    ixc2nsweep = dict_ixc2nsweep()
    comment = "Dictionary mapping ixc no to nsweep, if applicable"
    print_dict(ixc2nsweep, "DICT_IXC2NSWEEP", comment, lam)

    nsweep2ixc = {b:a for a, b in ixc2nsweep.items()}
    comment = "Dictionary mapping to nsweep to ixc no, if applicable"
    print_dict(nsweep2ixc, "DICT_NSWEEP2IXC", comment, lam)


def print_var_type():
    """Prints:
        DICT_VAR_TYPE
    """

    var_type = dict_var_type()
    comment = "Dictionary mapping variable name to variable type"
    print_dict(var_type, "DICT_VAR_TYPE", comment)

def print_ixc():
    """Prints:
        DICT_IXC_FULL
        DICT_IXC_BOUNDS
        DICT_IXC_DEFAULT
        DICT_IXC_SIMPLE
        DICT_IXC_SIMPLE_REV
    """

    #lambda to sort by integer value of key
    lam = lambda x: int(x[0])
    ixc_full = dict_ixc_full()
    comment = "Dictionary mapping ixc no to name and bounds"
    print_dict(ixc_full, "DICT_IXC_FULL", comment, lam, "defaultdict(dict)")

    ixc_bounds = {}
    for key, value in ixc_full.items():
        lb = value["lb"]
        ub = value["ub"]
        temp = {"lb" : lb, "ub" : ub}
        ixc_bounds[value["name"]] = temp

    comment = "Dictionary mapping iteration variable name to bounds"
    print_dict(ixc_bounds, "DICT_IXC_BOUNDS", comment, \
                dict_type="defaultdict(dict)")

    ixc_default = {}
    default = dict_default()
    for key, value in ixc_full.items():
        name = value["name"]
        if name in default:
            ixc_default[name] = default[name]
        else:
            logging.warning("print_dict_ixc could not find %s"\
                            " in DICT_DEFAULT\n", name)
    comment = "Dictionary mapping iteration variable name to default value"
    print_dict(ixc_default, "DICT_IXC_DEFAULT", comment)

    ixc_simple = {}
    for key, value in ixc_full.items():
        ixc_simple[key] = value["name"]
    comment = "Dictionary mapping ixc no to iteration variable name"
    print_dict(ixc_simple, "DICT_IXC_SIMPLE", comment, lam)

    ixc_simple_rev = {b:a for a, b in ixc_simple.items()}
    comment = "Dictionary mapping iteration variable name to ixc no"
    print_dict(ixc_simple_rev, "DICT_IXC_SIMPLE_REV", comment)


def print_icc():
    """Prints:
        DICT_ICC_FULL
    """

    #lambda to sort by integer value of key
    lam = lambda x: int(x[0])
    icc_full = dict_icc_full()
    comment = "Dictionary mapping icc no to name"
    print_dict(icc_full, "DICT_ICC_FULL", comment, lam, "defaultdict(dict)")


def print_default():
    """Prints:
        DICT_DEFAULT
    """

    default = dict_default()
    comment = "Dictionary of default values of variables"
    print_dict(default, "DICT_DEFAULT", comment)

def print_input_bounds():
    """Prints:
        DICT_INPUT_BOUNDS
    """

    input_bounds = dict_input_bounds()
    comment = "Dictionary of upper and lower bounds used when reading IN.DAT"
    print_dict(input_bounds, "DICT_INPUT_BOUNDS",\
                comment, dict_type="defaultdict(dict)")

def print_descriptions():
    """Prints:
        DICT_DESCRIPTIONS
    """
    descriptions = dict_descriptions()
    comment = "Dictionary of variable descriptions"
    print_dict(descriptions, "DICT_DESCRIPTIONS", comment)

def print_module():
    """Prints:
        DICT_MODULE
    """
    module = dict_module()
    comment = "Dictionary mapping module name to list of variables"
    print_dict(module, "DICT_MODULE", comment, dict_type="OrderedDict()")


def print_nsweep2varname():

    """
    Prints:
    DICT_NSWEEP2VARNAME
    """
    lam = lambda x: int(x[0])
    nsweep2varname = dict_nsweep2varname()
    comment = 'Dictionary mapping nsweep to varname'
    print_dict(nsweep2varname, 'DICT_NSWEEP2VARNAME', comment, lam)

def print_all():
    """Prints every dictionary
    """

    print_header()
    print_hard_coded()
    print_ixc2nsweep()
    print_var_type()
    print_icc()
    print_ixc()
    print_default()
    print_input_bounds()
    print_descriptions()
    print_module()
    print_nsweep2varname()


if __name__ == "__main__":

    desc = "Creates a python dictionary file for use by utility programs. " + \
           "Prints to stdout. Redirect to output file using '>'"
    PARSER = argparse.ArgumentParser(description=desc)

    #PARSER.add_argument("dir", help="PROCESS source directory", \
    #                    nargs="?", default=".")

    ARGS = PARSER.parse_args()

    #SOURCEDIR = ARGS.dir

    print_all()
