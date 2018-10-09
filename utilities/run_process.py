#!/usr/bin/env python
"""
Code to run PROCESS with a variation of the iteration parameters
until a feasible solution is found.
If running in sweep mode, the allowed number of unfeasible solutions
can be changed in the config file.

Author: H. Lux (Hanni.Lux@ccfe.ac.uk)

Date  : February 2014

Input files:
run_process.conf (config file, in the same directory as this file)
An IN.DAT file as specified in the config file

Output files:
All of them in the work directory specified in the config file
OUT.DAT     -  PROCESS output
PLOT.DAT    -  PROCESS output
MFILE.DAT   -  PROCESS output
process.log - logfile of PROCESS output to stdout
README.txt  - contains comments from config file

Compatible with PROCESS version 316
"""

#######################
#imported libraries

from argparse import ArgumentParser
from process_io_lib.process_config import RunProcessConfig
from process_io_lib.process_funcs import (get_neqns_itervars,
    get_variable_range, check_input_error, process_stopped,
    no_unfeasible_mfile, vary_iteration_variables, process_warnings)

def parse_command_line():
    """Process the command line arguments to execute PROCESS."""
    parser = ArgumentParser(description="Program to run PROCESS until a "
                                        "solution is found.")

    parser.add_argument("-f", "--configfile", default="run_process.conf",
                        help="configuration file, default = run_process.conf")

    return parser.parse_args()


if __name__ == "__main__":

    ARGS = parse_command_line()
    CONFIG = RunProcessConfig(ARGS.configfile)
    CONFIG.setup()
    NEQNS, ITERVARS = get_neqns_itervars()

    LBS, UBS = get_variable_range(ITERVARS, CONFIG.factor)

    #TODO add diff ixc summary part

    for i in range(CONFIG.niter):

        print(i, end=' ')
        CONFIG.run_process()

        check_input_error()

        if not process_stopped():

            no_unfeasible = no_unfeasible_mfile()

            if no_unfeasible <= CONFIG.no_allowed_unfeasible:
                if no_unfeasible > 0:
                    print("WARNING: Non feasible point(s) in sweep, "
                          "But finished anyway! {} ".format(no_unfeasible))
                if process_warnings():
                    print("\nThere were warnings in the final PROCESS run. "
                          "Please check the log file!\n")
                break
            else:
                print("WARNING: {} non-feasible point(s) in sweep! "
                      "Rerunning!".format(no_unfeasible))
        else:
            print("PROCESS has stopped without finishing!")

        vary_iteration_variables(ITERVARS, LBS, UBS)


    CONFIG.error_status2readme()

