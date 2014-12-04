"""
A selection of functions for using the PROCESS code

Author: Hanni Lux (Hanni.Lux@ccfe.ac.uk)

Date: March 2013 - initial released version

Notes:
13/08/2014 HL updated functions to work with new error_status flag
20/08/2014 HL fixed bug in get_variable_range

Compatible with PROCESS version 319
"""

import os
from os.path import join as pjoin

from process_io_lib.process_dicts import (DICT_IXC_SIMPLE, DICT_IXC_BOUNDS,
    DICT_IXC_DEFAULT, NON_F_VALUES, IFAIL_SUCCESS)
from process_io_lib.in_dat import INDATNew, INVariable
from process_io_lib.mfile import MFile
from numpy.random import uniform


def get_neqns_itervars(wdir='.'):

    """
    returns the number of equations and a list of variable
    names of all iteration variables
    """

    in_dat = INDATNew(pjoin(wdir, "IN.DAT"))

    ixc_list = in_dat.variables['ixc'].value

    itervars = []
    for var in ixc_list:
        if var != '':
            itervars += [DICT_IXC_SIMPLE[str(var)]]

    assert in_dat.variables['nvar'].value == len(itervars)

    return in_dat.variables['neqns'].value, itervars



###############################

def update_ixc_bounds(wdir='.'):

    """
    updates the lower and upper bounds in DICT_IXC_BOUNDS
    from IN.DAT
    """

    in_dat = INDATNew(pjoin(wdir, "IN.DAT"))

    for key in in_dat.variables.keys():
        if 'bound' in key.lower():
            var = key[key.find('(')+1:key.find(')')]
            name = DICT_IXC_SIMPLE[var]

            if 'boundl' in key:
                DICT_IXC_BOUNDS[name]['lb'] = in_dat.variables[key].value

            elif 'boundu' in key:
                DICT_IXC_BOUNDS[name]['ub'] = in_dat.variables[key].value

            else:
                print('Error in update_ixc_bounds: Unexpected variable name!')
                exit()


###############################

def  get_variable_range(itervars, factor, wdir='.'):

    """
    Returns the lower and upper bounds of the variable range
    for each iteration variable.

    itervars - string list of all iteration variable names
    factor   - defines the variation range for non-f-values by
               setting them to value * factor and value / factor
               respectively while taking their process bounds
               into account.

    For f-values the allowed range is equal to their process bounds.

    """

    in_dat = INDATNew(pjoin(wdir, "IN.DAT"))

    lbs = []
    ubs = []

    for varname in itervars:

        #for f-values we set the same range as in process
        if varname[0] == 'f' and (varname not in NON_F_VALUES):
            lbs += [DICT_IXC_BOUNDS[varname]['lb']]
            ubs += [DICT_IXC_BOUNDS[varname]['ub']]

        #for non-f-values we modify the range with the factor
        else:
            #value set from IN.DAT
            if varname in in_dat.variables.keys():
                value = in_dat.variables[varname].value

            #value set from defaults
            else:
                value = DICT_IXC_DEFAULT[varname]

            # to allow the factor to have some influence
            if value == 0.:
                value = 1.

            #assure value is within bounds!
            if value < DICT_IXC_BOUNDS[varname]['lb']:
                value = DICT_IXC_BOUNDS[varname]['lb']
            elif value > DICT_IXC_BOUNDS[varname]['ub']:
                value = DICT_IXC_BOUNDS[varname]['ub']

            lbs += [max(value/factor, DICT_IXC_BOUNDS[varname]['lb'])]
            ubs += [min(value*factor, DICT_IXC_BOUNDS[varname]['ub'])]

        if lbs[-1] > ubs[-1]:
            print('Error: Iteration variable {} has BOUNDL={.f} >\
 BOUNDU={.f}\n Update process_dicts or input file!'.format(varname, lbs[-1], ubs[-1]))
            exit()
        #assert lbs[-1] < ubs[-1]

    return lbs, ubs

###############################

def check_logfile(logfile='process.log'):

    """
    Checks the log file of the PROCESS output.
    Stops, if an error occured that needs to be
    fixed before rerunning.
    XXX should be deprecated!! XXX
    """

    with open(logfile, 'r') as outlogfile:
        errormessage = 'Please check the output file for further information.'
        for line in outlogfile:
            if errormessage in line:
                print('An Error has occured. Please check the output \
                       file for more information.')
                exit()


def check_input_error(wdir='.'):

    """
    Checks, if an input error has occurred.
    Stops as a consequence.
    """

    m_file = MFile(filename=pjoin(wdir, "MFILE.DAT"))
    error_id = m_file.data['error id'].get_scan(-1)

    if error_id == 130:
        print('Error in input file. Please check OUT.DAT \
for more information.')
        exit()


########################################

def process_stopped(wdir='.'):

    """
    Checks the process Mfile whether it has
    prematurely stopped.
    """

    m_file = MFile(filename=pjoin(wdir, "MFILE.DAT"))
    error_status = m_file.data['error status'].get_scan(-1)

    if error_status >= 3:
        return True

    return False

########################################

def process_warnings(wdir='.'):

    """
    Checks the process Mfile whether any
    warnings have occurred.
    """

    m_file = MFile(filename=pjoin(wdir, "MFILE.DAT"))
    error_status = m_file.data['error status'].get_scan(-1)

    if error_status >= 2:
        return True

    return False


############################################

def no_unfeasible_mfile(wdir='.'):

    """
    returns the number of unfeasible points
    in a scan in MFILE.DAT
    """

    m_file = MFile(filename=pjoin(wdir, "MFILE.DAT"))

    #no scans
    if not m_file.data['isweep'].exists():

        if m_file.data['ifail'].get_scan(0) == IFAIL_SUCCESS:
            return 0
        else:
            return 1

    else:

        ifail = m_file.data['ifail'].get_scans()

        return len(ifail) - ifail.count(IFAIL_SUCCESS)

############################################

def no_unfeasible_outdat(wdir='.'):

    """
    returns the number of unfeasible points
    in a scan in OUT.DAT
    XXX Should be deprecated! XXX
    """

    no_unfeasible = 0
    with open(pjoin(wdir, "OUT.DAT"), "r") as outdat_fh:
        for line in outdat_fh:
            if 'UNFEASIBLE' in line:
                no_unfeasible += 1

    return no_unfeasible


################################

def vary_iteration_variables(itervars, lbs, ubs):

    """
    Routine to change the iteration variables in IN.DAT
    within given bounds.
    itervars - string list of all iteration variable names
    lbs      - float list of lower bounds for variables
    ubs      - float list of upper bounds for variables
    """

    in_dat = INDATNew()

    new_values = []

    for varname, lbnd, ubnd in zip(itervars, lbs, ubs):

        new_value = uniform(lbnd, ubnd)
        new_values += [new_value]

        if varname in in_dat.variables.keys():
            in_dat.variables[varname].value = new_value

        else:
            in_dat.variables[varname] = INVariable(varname, new_value)

    in_dat.write_in_dat(filename='IN.DAT')

    return new_values


###################################

def get_solution_from_mfile(neqns, nvars, wdir='.'):

    """
    returns
    ifail - error_value of VMCON/PROCESS
    the objective functions
    the square root of the sum of the squares of the constraints
    a list of the final iteration variable values
    a list of the final constraint residue values

    If the run was a scan, the values of the last scan point
    will be returned.
    """

    m_file = MFile(filename=pjoin(wdir, "MFILE.DAT"))


    if not m_file.data['isweep'].exists:
        ind = 0  # only one run, no scan
    else:
        ind = -1 # last scan point

    ifail = m_file.data['ifail'].get_scan(ind)

    #figure of merit objective function
    objective_function = m_file.data['f'].get_scan(ind)

    #estimate of the constraints
    constraints = m_file.data['sqsumsq'].get_scan(ind)

    table_sol = []
    for var_no in range(nvars):
        table_sol.append(m_file.data['itvar{:03}'.format(var_no+1)].get_scan(ind))

    table_res = []
    for con_no in range(neqns):
       # table_res += [m_file.data['constr%03i'%(con_no+1)].get_scan(ind)]
        table_res.append(m_file.data['normres{:03}'.format(con_no+1)].get_scan(ind))

    if ifail != IFAIL_SUCCESS:
        return ifail, '0', '0', ['0']*nvars, ['0']*neqns

    return ifail, objective_function, constraints, table_sol, table_res



#################################################
def get_solution_from_outdat(neqns, nvars):

    """
    returns
    ifail - error_value of VMCON/PROCESS
    the objective functions
    the square root of the sum of the squares of the constraints
    a list of the final iteration variable values
    a list of the final constraint residue values

    If the run was a scan, the values of the last scan point
    will be returned.
    XXX should be deprecated XXX
    """

    flag_solution_vector = False
    flag_constr_residue  = False
    with open('OUT.DAT', 'r') as outdatfile:
        for line in outdatfile:
            if  "value       change" in line:
                flag_solution_vector = True
                flag_constr_residue  = False
                cnt_sol = 0
                table_sol     = []
            elif "constraint residues should be" in line:
                flag_solution_vector = False
                flag_constr_residue  = True
                cnt_res = 0
                table_res     = []
            elif "*******************" in line:
                flag_solution_vector = False
                flag_constr_residue  = False
            elif flag_solution_vector and len(line) >= 10:
                row = line.split()
                assert int(row[0])-1 == cnt_sol
                table_sol     += [row[2]] #final value
                cnt_sol += 1
            elif flag_constr_residue and len(line) >= 10:
                row = line.split()

                assert int(row[0])-1 == cnt_res
                try:
                    float(row[-2])
                    table_res     += [row[-2]]
                except ValueError:
                    table_res     += [row[-1]]

                cnt_res += 1
            elif 'Figure of merit objective function' in line:
                buf = line.split()
                objective_function = buf[-1]
            elif 'Estimate of the constraints' in line:
                buf = line.split()
                constraints = buf[-1]
            elif 'ifail' in line:
                buf = line.split()
                ifail = int(buf[-1])
            elif "and found a feasible set of parameters." in line:
                ifail = IFAIL_SUCCESS


    if ifail != IFAIL_SUCCESS:
        return ifail, '0', '0', ['0']*nvars, ['0']*neqns

    return ifail, objective_function, constraints, table_sol, table_res

