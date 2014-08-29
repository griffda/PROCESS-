"""
A selection of functions for using the PROCESS code

Author: Steven Torrisi (storrisi@u.rochester.edu)

Date: August 2014 - initial released version

Notes:
22/08/2014 HL moved functions from process_config.py to this file

Compatible with PROCESS version 319
"""

from process_io_lib.process_dicts import DICT_IXC_SIMPLE
from process_io_lib.in_dat import INDATNew
import collections as col
import subprocess

def get_var_name_or_number(variable):
    """
    Returns the iteration variable ixc number from a variable name,
    or the iteration variable ixc number from a variable name.
    This is useful to have as some PROCESS python library functionality
    uses the variable number instead of the name.

    Arguments:
        variable-->Can be an integer or a string.

    Returns:
        if variable is string and string a key in DICT_IXC_SIMPLE:
                                Returns the integer ixc representation
        if variable is integer:
                                Returns the string name of the variable.
        if the integer or string does not represent a variable:
                                Returns FALSE
    Dependencies:
            process_dicts: DICT_IXC_SIMPLE
    """

    if isinstance(variable, str):
        for key in DICT_IXC_SIMPLE:
            if DICT_IXC_SIMPLE[key] == variable.lower():
                return int(key)

        return False
    elif isinstance(variable, int):
        try:
            return DICT_IXC_SIMPLE[str(variable)]
        except KeyError:
            return False


    return None


def get_iter_variables_from_mfile(mfile, normalized=False):
    """
    Returns the values of the iteration variables from mfile in a list.
    If normalized is specified, returns the iteration values normalized
    according to their bounds (0 at lower bound, 1 at upper bound).

    Arguments:
        mfile-----------> MFile class from mfile.py
        numberofitvars--> Integer specifying the number of iteration
                          variables to find the values of.
                          Will then return the first N number of values,
                          where N is numberofitvars.
        normalized------> Whether to return the normalized values or not.
                          Boolean.

    Returns:
        output----------> An ordered dict with keys as the name of the
                          iteration variable, and values of the variable
                          values, either normalized or not based on
                          argument normalized. Has length=numberofitvars.

    Dependencies:
        collections module for OrderedDict
    """
    output = col.OrderedDict({})

    nvar = int(mfile.data['nvar'].get_scan(-1))

    for i in range(1, nvar+1):
        itervarstring = "itvar%03i" %i

        if normalized:
            itervarstring = "n" + itervarstring
        value = mfile.data[itervarstring].get_scan(-1)
        itervarname = mfile.data[itervarstring].var_description
        output[itervarname] = value

    return output


def get_iter_vars(inputfile='IN.DAT', makeboundarydict=False):

    """
    Opens IN.DAT file at inputfile, and returns the value of ixc,
    the array of iteration variables.
    Used by the iteration variable smoothing option.
    Gets the numbers of the iteration variables and sets them in
    self.iterationvariables.

    Arguments:
        inputfile--------> What IN.DAT file to read
        makeboundarydict-> Returns a special dictionary for the boundary
                           diagnostics option.

    Returns:
        An ordered dictionary;
        if makeboundarydict is false:
            The keys will be the ixc numbers, and the values will be
            string names of the variables
        if makeboudnarydict is true:
            The keys will be the variable names, the values will be
            a list with two numbers that start at 0.

    Dependencies:
        in_dat.py
        collections module
    """

    #Stored as a dictionary, where the KEY is the number and the
    #VALUE is the name.
    indat = INDATNew(inputfile)
    output = col.OrderedDict({})
    if not makeboundarydict:
        for iternumber in indat.variables['ixc'].value:
            output[iternumber] = get_var_name_or_number(iternumber)
    else:
        for iternumber in indat.variables['ixc'].value:
            output[get_var_name_or_number(iternumber)] = [0, 0]
    return output


def backup_in_file(setting, destination="IN.DATBACKUP"):

    """
    Writes and reads the IN.DAT file before and after the scan
    respectively to a backup.

    If setting is "w", will back up the input file by attempting
    to copy IN.DAT to destination.
    If setting is "r", will attempt to copy destination to "IN.DAT".

    Arguments:
        setting-----> "r" or "w": Determines what direction the backup
                      is going to go.
        destination-> What to call the backup.

    Modifies:
        IN.DAT in the current directory.

    Dependencies:
        subprocess

    """
    if setting is "w":
        subprocess.call(["cp", "IN.DAT", destination])
    if setting is "r":
        subprocess.call(["cp", destination, "IN.DAT"])
