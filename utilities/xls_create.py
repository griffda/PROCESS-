#!/usr/bin/env python

"""

  Append summary data to a spreadsheet data_sumamry.xlsx or name specified.

  Michael Kovari

    + 31/07/2017: Initial version created

"""

import os
import argparse
import process_io_lib.mfile as mf
from process_io_lib.mfile import make_plot_dat
#from process_io_lib.process_dicts import PARAMETER_DEFAULTS
from openpyxl import Workbook, styles, load_workbook
from openpyxl.styles import  Font, Border, Side


def write_column_mplot_dat(spreadsheet, custom_keys, mfile_data):
    """Function to add data to a spreadsheet using MFILE"""
    try:
        num_scans = int(mfile_data.data["isweep"].get_scan(-1))
    except KeyError:
        num_scans = 1
    print('num_scans', num_scans)

    val_keys = []
    mfile_keys = mfile_data.data.keys()

    try:
        wb = load_workbook(spreadsheet)
    except:
        wb = Workbook()

    # grab the active worksheet
    ws = wb.active

    var_descriptions = ['']
    var_names = ['']
    for key in custom_keys:
        if key in mfile_keys:
            var_description = mfile_data.data[key].var_description.replace(" ", "_")
            var_descriptions = var_descriptions + [var_description]

            val_keys.append(key)
            var_names = var_names + [key]

    ws.append(var_descriptions)
    ws.append(var_names)    

    # Write rows of values. One row for each scan.
    for num in range(num_scans):
         new_row = ['']
         values = ""
         for vkey in val_keys:
             if vkey in header_variables:
                 # These are found only once at the top of the MFILE.
                 values = mfile_data.data[vkey].get_scan(-1)
             else:
                # These are present in the MFILE for each scan point
                values = mfile_data.data[vkey].get_scan(num+1)
             new_row = new_row + [values]

         ws.append(new_row)
         print('new_row')
         print(new_row)

    # Save the spreadsheet
    wb.save(spreadsheet)

#-----------------------------------------------------------------------------


if __name__ == "__main__":

    # Setup command line arguments
    parser = argparse.ArgumentParser(description='Add data to spreadsheet')

    parser.add_argument('-p', metavar='p', type=str, nargs='+',
                        help='add new variables to the output')

    parser.add_argument('-f', metavar='f', type=str,
                        help='File to read as MFILE.DAT')

    parser.add_argument('-x', metavar='x', type=str,
                        help='spreadsheet (xlsx) file to append to')

    parser.add_argument("--defaults", help="run with default params",
                        action="store_true")

    parser.add_argument("--reset-config", help="Reset xls.conf",
                        action="store_true")

    args = parser.parse_args()

    default_variables = ['runtitle','username','date','iscan', 'rmajor', 'aspect', 'powfmw']
    header_variables = ['procver','date','time','username','runtitle','tagno','isweep','nsweep']

    # If user has specified an MFILE file that isn't MFILE.DAT pass the filename to
    # MFILE() class.
    if args.f:
        M = mf.MFile(filename=args.f)
    else:
        M = mf.MFile()

    # If user has specified a spreadsheet file that isn't data_summary.xlsx
    if args.x:
        print('x', x)
        spreadsheet = args.x
    else:
        spreadsheet = 'data_summary.xlsx'
    print(args)
    print('spreadsheet name', spreadsheet)

    # Get files in current directory to check for the config file.
    current_directory = os.listdir(".")
    if "xls.conf" not in current_directory or args.reset_config:
        print('Configuration file xls.conf not found in the local directory')
        conf_file = open("xls.conf", "a")
        for item in default_variables:
            conf_file.write(item + "\n")
        conf_file.close()
        print('A default configuration file xls.conf has been written ')

    # Read the config file.
    INPUT_CONFIG = mf.read_mplot_conf("xls.conf")

    # If the user added new parameters in the command line add them to the
    # INPUT_CONFIG list to pass to make_plot_dat()
    if args.p:
        conf_file = open("xls.conf", "a")
        for item in args.p:
            if item not in INPUT_CONFIG:
                conf_file.write(item + "\n")
        conf_file.close()

    INPUT_CONFIG = mf.read_mplot_conf("xls.conf")
    print('INPUT_CONFIG')
    print(INPUT_CONFIG)

    if args.defaults:
        write_column_mplot_dat(spreadsheet, INPUT_CONFIG, M)
    else:
        write_column_mplot_dat(spreadsheet, INPUT_CONFIG, M)
