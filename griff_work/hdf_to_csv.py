"""""
Script for reading HDF5 files as into csv files for use in Bayesian Network. 

Tom Griffiths
09/12/2022
CCFE

"""""

import argparse
import pandas as pd
import csv
from pathlib import Path, PurePath
import json

def parse_args(args):
    """Parse supplied arguments.

    :param args: arguments to parse
    :type args: list, None
    :return: parsed arguments
    :rtype: Namespace
    """
    parser = argparse.ArgumentParser(
        description="Program to read and  " "convert PROCESS HDF5 to csv."
    )

    parser.add_argument(
        "-f",
        "--hdf5file",
        default="uncertainties_data.h5",
        help="input hdf5 file (default=uncertainties_data.h5)",
    )

    parser.add_argument(
        "-v",
        "--varfile",
        type=str,
        default="HDF5_to_csv_vars.json",
        help="Specify file holding variable names, default = HDF5_to_csv_vars.json",
    )
    
    return parser.parse_args(args)  

def get_vars(vfile="mfile_to_csv_vars.json"):
    """Returns variable names from identified file.

    :param args: input JSON filename
    :type args: string
    :return: variable names
    :rtype: list
    """
    print("Fetching list of variables from", vfile)

    with open(vfile, "r") as varfile:
        data = varfile.read()
        obj = json.loads(data)
        vars = obj["vars"]

    return vars

# def read_mfile(h5filename="uncertainties_data.h5", vars=[]):
#     """specified variable values from identified file.

#     :param args: input filename, variable names
#     :type args: string, list
#     :return: variable descriptions, names, and values
#     :rtype: list of tuples"""
#     print("Reading from H5File:", h5filename)

#     # m_file = MFile(h5filename)

#     output_vars = []

#     # for each variable named in the input varfile, get the description and data value
#     # for var_name in vars:
#     #     if var_name not in m_file.data.keys():
#     #         print(
#     #             "Variable '{}' not in MFILE. Skipping and moving on...".format(var_name)
#     #         )
#     #     else:
#     #         # In case of a file containing multiple scans, (scan = -1) uses the last scan value
#     #         var_val = m_file.data[var_name].get_scan(-1)
#     #         description = m_file.data[var_name].var_description
#     #         var_data = (description, var_name, var_val)
#     #         output_vars.append(var_data)
#     return output_vars

# def get_savenamepath(h5filename="uncertainties_data.h5"):
#     """Returns path/filename.csv for file saving.

#     :param args: input filename
#     :type args: string
#     :return: output filename
#     :rtype: pathlib.PurePosixPath
#     """

#     if h5filename == "uncertainties_data.h5":
#         # save it locally
#         dirname = Path.cwd()
#     else:
#         # output the csv file to the directory of the input file
#         dirname = PurePath(h5filename).parent

#     csv_filename = PurePath(h5filename).stem
#     csv_outfile = PurePath(dirname, csv_filename + ".csv")

#     return csv_outfile  

# def write_to_csv(csv_outfile, output_data=[]):
#     """Write to csv file.

#     :param args: input filename, variable data
#     :type args: string, list of tuples
#     """
#     with open(csv_outfile, "w") as csv_file:
#         print("Writing to csv file:", csv_outfile)
#         writer = csv.writer(csv_file, delimiter=",")
#         writer.writerow(["Description", "Varname", "Value"])

#         for vardesc in output_data:
#             writer.writerow(vardesc)

def main(args=None):
    """reads inputfile and creates csv as outputfile

    :param args: None
    :return: None
    """
    # if args.print:
    #     print(data_set)

    # output_names = args.vars

    # Select only the converged runs for creating KDF plots
    # see if we can only use if ifail = 1 for the KDF

    # TO DO make separate list of converged and failed runs
    # find way to display both to look at margins
    # we want to colour red for ifail = 2-6 and blue for ifail = 1
    # ioptimz = data_set["ioptimz"][0]
    # if ioptimz == -2:
    #     data_set_converge = data_set
    # else:
    #     data_set_converge = data_set[data_set["ifail"] == 1.0]

    # pd.plotting.scatter_matrix(
    #     data_set_converge[output_names], alpha=0.2, diagonal="kde"
    # )
    # read from command line inputs
    args = parse_args(args)

    # read list of required variables from input json file
    jvars = get_vars(args.varfile)

    # read required data from input mfile
    # output_data = read_mfile(args.mfile, jvars)
    data_set = pd.read_hdf(args.hdf5file, jvars)
    data_set.to_csv('hdf5.csv', index=True)

    # identify save location
    # output_file = get_savenamepath(args)

    # write to csv
    # write_to_csv(output_file, data_set)

    # write final line to screen
    print("Complete.")

if __name__ == "__main__":
    main()
