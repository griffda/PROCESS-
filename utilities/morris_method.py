"""
Code to the Morris method of elementary elements to
investiage the sensistivity of the input parameters in PROCESS

Author: A. Pearce (alexander.pearce@ukaea.uk)

Input files:
run_process.conf (config file, in the same directory as this file)
morris_method_conf.json (config file for parameters used by morris method,
                        in same directory as this file)
An IN.DAT file as specified in the config file

Output files:
All of them in the work directory specified in the config file
OUT.DAT     -  PROCESS output
PLOT.DAT    -  PROCESS output
MFILE.DAT   -  PROCESS output
process.log -  logfile of PROCESS output to stdout
capcost_sol.txt - contains lsit of all outputs
error_log.txt  - contains list of all output with ifail != 1
morris_method_output - contains the dictorany of the mean and
                        variance of the elementary effects

"""

import os
import argparse
from SALib.sample.morris import sample
from SALib.analyze import morris
from process.io.process_config import RunProcessConfig
from process.io.in_dat import InDat
from process.io.mfile import MFile
from process.io.process_funcs import set_variable_in_indat
import numpy as np
import json


def Get_Input():
    # Define the model inputs
    problem = {
        "num_vars": 14,
        "names": [
            "boundu(9)",
            "hfact",
            "coreradius",
            "fimp(2)",
            "fimp(14)",
            "psepbqarmax",
            "boundl(103)",
            "cboot",
            "peakfactrad",
            "kappa",
            "etaech",
            "feffcd",
            "etath",
            "etaiso",
        ],
        # ,'boundl(18)','pinjalw',\
        # 'alstroh','alstrtf','aspect','bmxlim','triang'],
        "bounds": [
            [1.1, 1.3],
            [1.0, 1.2],
            [0.45, 0.75],
            [0.085, 0.115],
            [1.0e-5, 1.0e-4],
            [8.7, 9.7],
            [0.75, 1.25],
            [0.985, 1.015],
            [2.0, 3.5],
            [1.83, 1.86],
            [0.3, 0.5],
            [0.5, 5.0],
            [0.36, 0.40],
            [0.75, 0.95],
        ]
        # ,
        # [3.40, 3.60],
        # [51.0, 61.0],
        # [6.0e8, 7.2e8],
        # [5.2e8, 6.4e8],
        # [3.08, 3.12],
        # [11.0, 12.0],
        # [0.475, 0.525]]
    }
    with open(DICTS_FILE_PATH, "w") as dicts_file:
        json.dump(problem, dicts_file, indent=4, sort_keys=True)

    return


def write_Morris_Method_Output(X, S):
    # open output file
    f = open("morris_method_output.txt", "w")

    # create sensistivity indices header
    f.write("Parameter mu mu_star sigma mu_star_conf\n")

    # print the sensistivity indices
    for i in range(X["num_vars"]):
        f.write(
            "%s %f %f %f %f\n"
            % (
                X["names"][i],
                S["mu"][i],
                S["mu_star"][i],
                S["sigma"][i],
                S["mu_star_conf"][i],
            )
        )


if __name__ == "__main__":

    # set up command line arguments
    PARSER = argparse.ArgumentParser(
        description="Program to evaluate\
 model sensistivity by elementary element method at a given PROCESS design point."
    )

    PARSER.add_argument(
        "-f",
        "--configfile",
        default="run_process.conf",
        type=str,
        help="configuration file, default = run_process.conf",
    )
    PARSER.add_argument(
        "-i",
        "--inputfile",
        default="morris_method_conf.json",
        type=str,
        help="input parameters file, default = morris_method_conf.json",
    )
    PARSER.add_argument(
        "-o",
        "--outputvarname",
        default="capcost",
        type=str,
        help="PROCESS output analysed, default = capcost",
    )
    PARSER.add_argument(
        "-s",
        metavar="SOLLIST",
        default="capcost_sol.txt",
        type=str,
        help="filename of PROCESS outputs, default = capcost_sol.txt",
    )
    PARSER.add_argument(
        "-e",
        metavar="ERRORLIST",
        default="error_log.txt",
        type=str,
        help="filename of failed PROCESS output, default = error_log.txt",
    )
    PARSER.add_argument(
        "-m",
        metavar="OUTPUTMEAN",
        default=8056.98,
        help="PROCESS mean model output value, default = 8056.98 (DEMO capcost)",
    )
    PARSER.add_argument(
        "-t",
        metavar="TRAJNUM",
        default=25,
        type=int,
        help="number of trajectories sampled, default = 25",
    )
    PARSER.add_argument(
        "-n",
        metavar="NUMLVLS",
        default=4,
        type=int,
        help="Number of grid levels used in hypercube sampling, default = 4",
    )

    ARGS = PARSER.parse_args()

    # main program
    DICTS_FILE_PATH = ARGS.inputfile

    # Get parameters
    # Get_Input()
    DICTS_FILE_PATH = os.path.join(os.path.dirname(__file__), "morris_method_conf.json")
    dicts_file = open(DICTS_FILE_PATH, "r")
    params_bounds = json.load(dicts_file)
    capcost_mean = (
        ARGS.m
    )  # 8056.98 used from minimsing capcost on 2018 DEMO baseline ~~CHECK!
    traj_num = ARGS.t
    sols = np.array([])
    fail = np.array([])

    # Generate samples
    params_values = sample(params_bounds, traj_num, num_levels=8)

    np.savetxt("param_values.txt", params_values)

    CONFIG = RunProcessConfig(ARGS.configfile)
    CONFIG.setup()

    in_dat = InDat()

    run_max = int((params_bounds["num_vars"] + 1.0) * traj_num)
    for run_id in range(run_max):
        print("run number =", run_id)
        i = 0
        for n in params_bounds["names"]:
            set_variable_in_indat(in_dat, n, params_values[run_id][i])
            i = i + 1

        in_dat.write_in_dat(output_filename="IN.DAT")

        CONFIG.run_process()

        m_file = MFile(filename="MFILE.DAT")

        # We need to find a way to catch failed runs
        process_status = m_file.data["ifail"].get_scan(-1)
        print("ifail =", process_status)

        if process_status == 1.0:
            # read the figure of merit from the MFILE
            capcost = m_file.data[ARGS.outputvarname].get_scan(-1)
            sols = np.append(sols, capcost)
        else:
            fail = np.append(fail, run_id)
            if run_id == 0:
                # make mean
                sols = np.append(sols, capcost_mean)
            else:
                sols = np.append(sols, sols[np.size(sols) - 1])

    params_sol = morris.analyze(params_bounds, params_values, sols)
    np.savetxt(ARGS.s, sols)
    np.savetxt(ARGS.e, fail)

    # write output file
    write_Morris_Method_Output(params_bounds, params_sol)
