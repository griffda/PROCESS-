#!/usr/bin/env python
"""
Code to read from a PROCESS MFILE and write values into a csv

Author: R Chapman (rhian.chapman@ukaea.uk)
Date: 26/05/2021

Input files:
mfile (default MFILE.DAT) as output from PROCESS
variable list (default varlist.txt) as defined by user

Instructions:
- this script must be run within the python environment, in order to pick up the right modules
- command line call: python mfile_to_csv.py -m </path/to/mfile.dat> -v </path/to/varfile.txt>

"""

# == import modules ==
## standard python modules
import argparse
import csv
import os
from array import *

## PROCESS-specific modules
from process.io.mfile import MFile


# == define functions ==


def get_user_inputs():
    parser = argparse.ArgumentParser(
        description="Read from a PROCESS MFILE and write values into a csv."
    )

    parser.add_argument(
        "-m", "--mfile", default="MFILE.DAT", help="mfile name, default = MFILE.DAT"
    )

    parser.add_argument(
        "-v",
        "--varfile",
        default="varlist.txt",
        help="variable textfile name, default = varlist.txt",
    )

    args = parser.parse_args()

    return args


def get_varlist(varfile="varlist.txt"):
    print("Fetching list of variables from", varfile)
    varlist = list([])

    with open(varfile, "r") as varlist_in:
        lines = varlist_in.readlines()
        for l in lines:
            l = l.rstrip()  # strip trailing characters, e.g. /n
            varlist.append(l)

    return varlist


def read_mfile(mfilename="MFILE.DAT", varlist_in=[]):
    print("Reading from MFILE:", mfilename)

    # m_file = MFile(args.mfile)
    m_file = MFile(mfilename)

    # initialise empty arrays & lists
    names = list([])
    values = array("d", [])
    desc = list([])

    # for each variable named in the input varfile, get the description and data value
    k = 0
    for var_name in varlist_in:

        thisval = m_file.data[var_name].get_scan(-1)
        # In case of a file containing multiple scans, (scan = -1) uses the last scan value

        try:  ## mfile module doesn't currently catch a missing var_description error
            thisdescription = m_file.data[var_name].var_description
            names.insert(k, var_name)
            values.insert(k, thisval)
            desc.insert(k, thisdescription)
            k = k + 1
        except AttributeError as error:
            print(var_name, "skipped, moving on...")

    return names, values, desc


def write_to_csv(mfilename="MFILE.DAT", varnames=[], varvalues=[], vardesc=[]):

    csv_filename = "mfile_outputs.csv"
    if "/" in mfilename:  ## very simple string search for the '/' character
        # if input mfile is in a different directory, output the csv file to there
        dirname = os.path.dirname(mfilename)
        csv_outfile = dirname + "/" + csv_filename
    else:
        # otherwise save it locally
        csv_outfile = csv_filename

    print("Writing to csv file:", csv_outfile)

    with open(csv_outfile, "w") as csv_file:
        writer = csv.writer(csv_file, delimiter=",")
        writer.writerow(["Description", "varname", "value"])

        for i in range(0, len(varnames)):
            desc = vardesc[i]
            name = varnames[i]
            val = varvalues[i]

            writer.writerow([desc, name, val])


def main():

    # read from command line inputs
    args = get_user_inputs()

    # read list of required variables from input text file:
    varlist = get_varlist(args.varfile)
    ## input text file should contain a single column list of variable names

    # read required data from input mfile:
    varnames, varvalues, vardesc = read_mfile(args.mfile, varlist)

    # write to csv
    write_to_csv(args.mfile, varnames, varvalues, vardesc)

    # # print-to-screen check:
    # for i in range(0,len(varnames)):
    #     print(vardesc[i], varnames[i], varvalues[i])

    # write final line to screen
    print("Complete.")


# == program ==

if __name__ == "__main__":
    main()

# == end ==
