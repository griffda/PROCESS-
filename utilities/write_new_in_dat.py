#! /usr/bin/env python
"""

  Modifies the PROCESS input file IN.DAT so all the iteration variables are
  given their values from the output file OUT.DAT.

  James Morris 30/04/2014 based on code by Michael Kovari 9/8/13 and
  J C Rivas, 16/7/2013

  Notes:
     + JM 30/04/2014: Initial version using new libraries
     + JM 30/04/2014: Added command line arguments
     + PJK 19/08/2014: Corrected -- if "itvar" in value: -- line
     + HL 16/12/2014: Updated to use new in_dat.py library

  Compatible with PROCESS version 382

"""

import argparse
import process_io_lib.mfile as mf
import process_io_lib.in_dat as in_dat

def last_feasible_point(filename):
    """Function to determine the last feasible point in a scan

    Args: 
      filename --> name of MFILE.DAT to read

    Returns:
      scanPoint --> scan number to use when writing new file
    
    """
    mfile_data = mf.MFile(filename)
    feasible = False
    
    # Get number of scans and assign as initial value of scanPoint        
    scanPoint = int(mfile_data.data["isweep"].get_scan(-1))
    
    while feasible == False:
        print("scan point = ", scanPoint)
        for value in mfile_data.data.keys():
            if "ifail" in value:
                print("ifail = ", int(mfile_data.data[value].get_scan(scanPoint)))
                if mfile_data.data[value].get_scan(scanPoint) == 1:
                    print("found feasible")
                    feasible = True
                else:
                    scanPoint = scanPoint - 1
                break
                
    return scanPoint

def get_iteration_variables(filename, scan):
    """Function to get a list of the iteration variables and their values from
    MFILE.DAT

    Args:
      filename --> name of MFILE.DAT to read
      scan --> scan number to use

    Returns:
      iteration_vars --> dictionary of iteration variables in MFILE and their
                         values.

    """
    mfile_data = mf.MFile(filename)
    iteration_vars = {}

    for value in mfile_data.data.keys():
        if "itvar" in value and "nitvar" not in value:
            variable_name = mfile_data.data[value].var_description
            variable_value = mfile_data.data[value].get_scan(scan)
            iteration_vars[variable_name] = variable_value

    return iteration_vars


def replace_iteration_variables(iteration_vars, in_data):
    """Function to replace the iteration variables in IN.DAT if the variable
    is not defined in IN.DAT it will add the variable to the end of the file.

    Args:
      iteration_vars --> dictionary of iteration variables from MFILE.DAT and
                         their values
      in_data --> IN.DAT data object.

    """

    for name in iteration_vars.keys():
        varname = name.lower()
        in_data.add_parameter(varname, iteration_vars[name])

    return in_data

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Creates a new IN.DAT using "
                                                 "iteration variable values "
                                                 "from MFILE.DAT.")

    parser.add_argument('-f', metavar='f', type=str, default="MFILE.DAT",
                        help='File to read as MFILE.DAT (default="MFILE.DAT")')

    parser.add_argument('-i', metavar='i', type=str, default="IN.DAT",
                        help='File to read as IN.DAT (default="IN.DAT")')

    parser.add_argument('-o', metavar='o', type=str, default="new_IN.DAT",
                        help='File to write as new IN.DAT '
                             '(default="new_IN.DAT")')

    args = parser.parse_args()

    # Determine last feasible scan point
    scan = last_feasible_point(args.f)
    print("scan number = ", scan)
    #scan = 0
    if (scan == 0):
        print("No feasible points in scan")
        raise SystemExit
    
    # Get iteration variables from MFILE.DAT
    it_vars = get_iteration_variables(args.f, scan)

    # Read IN.DAT
    in_dat_data = in_dat.InDat(args.i)

    # Amend the values for the iteration variables
    in_dat_data = replace_iteration_variables(it_vars, in_dat_data)

    # Write a new IN.DAT
    in_dat_data.write_in_dat(output_filename=args.o)
