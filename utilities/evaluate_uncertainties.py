#!/usr/bin/env python3
"""
Code to assess uncertainties in the input parameters in PROCESS

Author: H. Lux (Hanni.Lux@ccfe.ac.uk)

Input files:
run_process.conf (config file, in the same directory as this file)
evaluate_uncertainties.json (config file for uncertainties in same
                             directory as this file)
An IN.DAT file as specified in the config file

Output files:
All of them in the work directory specified in the config file
OUT.DAT     -  PROCESS output
PLOT.DAT    -  PROCESS output
MFILE.DAT   -  PROCESS output
process.log - logfile of PROCESS output to stdout
README.txt  - contains comments from config file

Compatible with PROCESS version 368
"""

#######################
#imported libraries

import argparse
from process_io_lib.process_config import UncertaintiesConfig
from process_io_lib.process_funcs import get_neqns_itervars,\
    update_ixc_bounds, get_variable_range, check_input_error,\
    process_stopped, no_unfeasible_mfile,\
    vary_iteration_variables

if __name__ == '__main__':
############################################################
#Usage

    PARSER = argparse.ArgumentParser(description='Program to evaluate\
 uncertainties in a given PROCESS design point.')

    PARSER.add_argument("-f", "--configfile",
                        default='evaluate_uncertainties.json',
                        help="configuration file, default =\
 evaluate_uncertainties.json")

    ARGS = PARSER.parse_args()


############################################################
#main program



    CONFIG = UncertaintiesConfig(ARGS.configfile)
    CONFIG.setup()

    NEQNS, ITERVARS = get_neqns_itervars()

    update_ixc_bounds()

    LBS, UBS = get_variable_range(ITERVARS, CONFIG.factor)

    CONFIG.checks_before_run()

    CONFIG.set_sample_values()

    run_id = 0

    for j in range(CONFIG.no_samples):

        print('sample point', j, ':')
        CONFIG.go2newsamplepoint(j)

        for i in range(CONFIG.niter):

            print('  ', i, end=' ')
            CONFIG.run_process()

            check_input_error()

            if not process_stopped():

                no_unfeasible = no_unfeasible_mfile()

                if no_unfeasible <= CONFIG.no_allowed_unfeasible:
                    if no_unfeasible > 0:
                        print('WARNING: Non feasible point(s) in sweep,\
         but finished anyway! %i ' % no_unfeasible)
                    CONFIG.add_results2netcdf(run_id)
                    run_id+=1
                    break
                else:
                    print('WARNING: %i non feasible point(s) in sweep!\
         Rerunning!' % no_unfeasible)
            else:
                print('PROCESS has stopped without finishing!')

            vary_iteration_variables(ITERVARS, LBS, UBS)




    CONFIG.write_results()
