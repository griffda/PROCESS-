#!/usr/bin/env python3
"""
Code to test PROCESS Solver by choosing different starting values
for the iteration parameters around the initial values in the INPUT file

Author: H. Lux (Hanni.Lux@ccfe.ac.uk)

Date: November 2013 (Initial version)
      March 2014 (Initial release version)

- Input files -
test_process.conf  in the same directory as this file

- Output files -
(All of them in the working directory specified in the config file)
OUT.DAT     - PROCESS output of last run
PLOT.DAT    - PROCESS output of last run
MFILE.DAT   - PROCESS output of last run
process.log - logfile of PROCESS output to stdout
time.info   - stores the run time of the inner loop
SolverTest.out - 

Ifail values:
 -1: Error in logfile, no solution vector in OUT.DAT
  0: Error in VMCON: improper input parameters
  1: normal run
  2: too many function calls in VMCON
3/4: either constraint/objective function + gradients
     are not calculated well or data too noisy
  5: either no feasible solution to the problem,
     or identity matrix is a bad approximation ot the Hessian
  6: restriction by an artificial bound or singular matrix
     in quadratic problem.
"""


#######################
#imported libraries

import time
import sys
from numpy import histogram
import argparse
from process_io_lib.process_dicts import IFAIL_SUCCESS
from process_io_lib.process_config import TestProcessConfig
from process_io_lib.process_funcs import get_neqns_itervars,\
    update_ixc_bounds, get_variable_range, check_input_error,\
    vary_iteration_variables, process_stopped,\
    get_solution_from_mfile,\
    process_warnings

############################################################
#Usage/Help

parser = argparse.ArgumentParser(description='Program to test the PROCESS Solver')

parser.add_argument("-f", "--configfile", default='test_process.conf',
                    help="configuration file, default = test_process.conf")

ARGS = parser.parse_args()

#############################################################

CONFIG = TestProcessConfig(ARGS.configfile)
CONFIG.setup()

NEQNS, ITERVARS = get_neqns_itervars()

update_ixc_bounds()

LBS, UBS = get_variable_range(ITERVARS, CONFIG.factor)

#############################################################


TABLE_ERR = []
TABLE_OBJ = []
TABLE_CON = []
TABLE_IN  = []
TABLE_OUT = []

WARNING_CNT = 0

START_TIME = time.time()

for i in range(CONFIG.niter):

    #modify IN.DAT each time
    input_values = vary_iteration_variables(ITERVARS, LBS, UBS)

    TABLE_IN += [input_values]

    print(i, end=' ')

    CONFIG.run_process()

    check_input_error()

    if process_stopped():
        ifail, objective_function, constraints, table_sol, table_res \
            = -1, '0', '0', ['0']*len(ITERVARS), ['0']*NEQNS
    else:
        ifail, objective_function, constraints, table_sol, table_res \
            = get_solution_from_mfile(NEQNS, len(ITERVARS))

        if ifail == IFAIL_SUCCESS and process_warnings():
            WARNING_CNT += 1

    TABLE_OUT += [table_sol+table_res]
    TABLE_ERR += [ifail]
    TABLE_OBJ += [objective_function]
    TABLE_CON += [constraints]

    i += 1


END_TIME = time.time()

##########
TIMEFILE = open('time.info', 'w')
TIMEFILE.write('wall clock time of run %f s' %(END_TIME-START_TIME))
TIMEFILE.close()


#########
print('\n Error codes:')
ERRHIST, ERRBINS = histogram(TABLE_ERR, bins=[-1.5, -0.5, 0.5, 1.5,
                                                 2.5, 3.5, 4.5, 5.5, 6.5])

for i in range(len(ERRHIST)):
    print('code %2i: %4i out of %4i = %3.1f '
           % (int((ERRBINS[i+1]+ERRBINS[i])/2.), ERRHIST[i],
              CONFIG.niter, ERRHIST[i]*100./CONFIG.niter)+ '%')

if WARNING_CNT > 0:
    # TODO: calculate fraction of warnings in sucessful runs.
    print('\nIn %f%% of the successful runs warnings occurred.' %WARNING_CNT)

##########
OUTFILE = open('SolverTest.out', 'w')

OUTHEADER = '#Err\t ObjectiveF\t Constraints\t'
for inp_str in ITERVARS:             #input variables
    OUTHEADER += '%s_in\t' % inp_str
for var_no in range(len(ITERVARS)): #output variables
    OUTHEADER += 'itvar%03i\t' %(var_no+1)
for con_no in range(NEQNS):          #constraints
    OUTHEADER += 'constr%03i\t' %(con_no+1)
OUTHEADER += '\n'
OUTFILE.write(OUTHEADER)


for i in range(CONFIG.niter):
    outstr = '%d\t %s\t %s\t'% (TABLE_ERR[i], TABLE_OBJ[i], TABLE_CON[i])
    for valuein in TABLE_IN[i]:
        outstr += '%s\t' %valuein
    for valueout in TABLE_OUT[i]:
        outstr += '%s\t'%valueout
    outstr += '\n'
    OUTFILE.write(outstr)


OUTFILE.close()

