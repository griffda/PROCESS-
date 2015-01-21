#!/usr/bin/env python
"""
  Given an initial IN.DAT file (A) and a target IN.DAT file (B)
  containing target values for a set of variables, runs PROCESS
  repeatedly with the aim of 'walking' the solution to the initial
  IN.DAT to a solution using the values given in the target IN.DAT. After
  each step the iteration variables from the previous run are used as
  the starting values of the next step. If a particular step cannot be
  completed, the values of the iteration variables are randomly varied
  about their current positions in an attempt to find working values.

  Requires in same directory:
    a_to_b.conf --> Configuration file
    a_to_b_config.py --> Class to read config file

  By default, the IN.DAT files for A and B are set as A.DAT and B.DAT in the
  current directory, but these can be changed in the configuration file
  as well as other options (see a_to_b_config.py)

  If a variable is set as an iteration variable in A but not in B,
  the bounds on the variable are progressively narrowed towards the B value to
  hopefully make the iteration variable converge on the target value.

  If a variable is set as an iteration variable in B but not in A,
  it is made an iteration variable immediately.

  Differences in ixc are made gradually. At each step between A and B,
  a_to_b may add or remove constraint equations. Constraint equations
  that need to be removed between A and B are removed before any
  new constraints are added. A large number of differences in the constraint
  equations set in A and B can make the run very likely to fail.

  PROCESS is run inside the working directory specified in a_to_b.conf. If a
  particular step is unsuccessful and a_to_b exits, the .DAT files and
  the logged output of the run will be in this directory. If a step is
  successful, the files will be copied to the output directory specified
  in a_to_b.conf and prefaced with their step number (eg '3.IN.DAT').


  Thomas Miller 24/09/2014 based on code by James Morris 30/04/2014,
  H Lux 02/14, Michael Kovari 9/8/13 and J C Rivas 16/7/2013.

"""

import os
import argparse

import process_io_lib.in_dat as inmod
import process_io_lib.mfile as mfmod
from process_io_lib.process_dicts import DICT_IXC_SIMPLE, DICT_VAR_TYPE, \
            DICT_DEFAULT, DICT_IXC_BOUNDS, IFAIL_SUCCESS, \
            DICT_INPUT_BOUNDS
from process_io_lib.process_funcs import check_logfile, \
            process_warnings, process_stopped, vary_iteration_variables
from process_io_lib.a_to_b_config import AToBConfig

def is_bound(var_name):
    """Function that tests if var_name is a valid upper
       or lower bound name eg. 'boundu(12)' or 'BOUNDL(3)'
       if the name is valid, returns +number in brackets for
       upper bounds, -number for lower bounds. If the name is
       not valid, returns False.

    """
    var_name = var_name.lower()
    if var_name[:7] == "boundu(" or var_name[:7] == "boundl(":
        if var_name[-1:] == ")":
            try:
                num = int(var_name[7:-1])
            except ValueError:
                return False
            if num < 0 or num >= len(DICT_IXC_SIMPLE):
                return False
            else:
                if var_name[:7] == "boundu(":
                    return num
                else:
                    return -num
    return False

def get_default_value(var):
    """Returns the default value of var. Looks in dictionary DICT_DEFAULT
       for iteration variables and DICT_IXC_BOUNDS for bounds. Returns None if
       no value can be found

    """
    if var in DICT_DEFAULT.keys():
        return DICT_DEFAULT[var]
    else:
        num = is_bound(var)
        if not num == False:
            if num < 0:
                return DICT_IXC_BOUNDS[DICT_IXC_SIMPLE[str(-num)]]["lb"]
            else:
                return DICT_IXC_BOUNDS[DICT_IXC_SIMPLE[str(num)]]["ub"]
    return None

def get_val(varname, in_dat):
    """Returns value of variable in in_dat if specified, otherwise returns
       default value of variable
       Args
            varname --> String name of variable
            in_dat --> INDat object

    """
    if varname in in_dat.data.keys():
        return in_dat.data[varname].value
    else:
        return get_default_value(varname)

def get_mfile_dict(filename="MFILE.DAT", filt=None):
    """Function to get a dictionary of the variables and their values from
       MFILE.DAT. Only variables that appear in filt are returned.

       Args:
           filename --> Name of MFILE.DAT to read
           filt --> List of variable names to allow. Pass None to
                 disable filter

       Returns:
           mfile_dict --> dictionary of variables in MFILE and their values.

    """
    mfile = mfmod.MFile(filename)
    mfile_dict = {}
    for key in mfile.data.keys():
        if filter == None or key in filt:
            variable_value = mfile.data[key].get_scan(-1)
            mfile_dict[key] = variable_value

    return mfile_dict

def update_infile(var_dict, in_dat):
    """Function to update the variables in in_dat to the value in the
       given dictionary

       Args:
           var_dict --> dictionary of variables and the value to be
                   updated to
           in_dat --> IN.DAT data object.

    """
    for key in var_dict.keys():
        if key in in_dat.data.keys():
            #if the variable is already in infile, update it
            in_dat.data[key].value = var_dict[key]
            in_dat.variables[key].value = var_dict[key]
        elif key in DICT_VAR_TYPE.keys() or is_bound(key):
            #if it isn't, add it to infile only if in valid variable list
            var = inmod.INVariable(key, var_dict[key])
            in_dat.add_variable(var)
        else:
            print("Bad variable %s in update_infile" % var)
            exit()

    return in_dat

def update_dict(original_dict, target_dict, frac):
    """Function that takes two dictionaries that have identical keys and
       returns a new dicionary with values interpolated between the two.
       Only works for floats

       Args:
           original_dict --> dictionary of original values
           target_dict --> dictionary of target values ie. values when
                      iteration_number == iteration_limit
           frac --> interpolation fraction between original and target ie.
               frac = 0.0 -> original value, frac = 1.0 -> target value

    """
    if sorted(original_dict.keys()) != sorted(target_dict.keys()):
        print("Differing dictionary keys passed to update_dict")
        exit()
    #not stricly neccesary but shouldn't occur in this program
    assert frac >= 0 and frac <= 1

    ret_dict = {}
    for key in original_dict.keys():
        assert isinstance(original_dict[key], float)
        delta = target_dict[key] - original_dict[key]
        ret_dict[key] = original_dict[key] + delta * frac
    return ret_dict

def run_process(path_to_process, niter=100, bound_spread=1.2):
    """Runs PROCESS in the current directory. If the run is not succesful,
       varies the iteration variables randomly between
       bound_spread * current value and current_value / bound_spread
       loops niter times before giving up. Returns True if a succesful
       run is carried out, False if niter is exceeded.

    """
    in_dat = inmod.INDATNew("IN.DAT")
    #get list of itervars
    ixc_list = in_dat.variables['ixc'].value
    itervars = []
    for var in ixc_list:
        if var != '':
            itervars += [DICT_IXC_SIMPLE[str(var)]]

    #get the lower and upper bounds between which each itervar will be varied
    lbs = []
    ubs = []
    for name in itervars:
        val = get_val(name, in_dat)
        if name in DICT_INPUT_BOUNDS:
            process_lb = DICT_INPUT_BOUNDS[name]["lb"]
            process_ub = DICT_INPUT_BOUNDS[name]["ub"]
            lbs.append(max(process_lb, val / bound_spread))
            ubs.append(min(process_ub, val * bound_spread))
        else:
            lbs.append(val / bound_spread)
            ubs.append(val * bound_spread)

    #loop niter times, varying iteration variables if no solution
    for i in range(niter + 1):
        returncode = os.system(path_to_process + " >& process.log")
        if returncode != 0:
            print('\n Error: There was a problem with PROCESS \
                        execution %i!' % returncode)
            print('Refer to process.log for more information')
            exit()
        #exit if process reported an error
        check_logfile()
        if not process_stopped():
            #if process didn't stop, check ifail in the mfile
            mfile = mfmod.MFile("MFILE.DAT")
            ifail = mfile.data["ifail"].get_scan(0)
            if ifail == IFAIL_SUCCESS:
                #if the run was succesful, return True
                if process_warnings():
                    print("There were warnings in the PROCESS run. Please"
                          " check the log file!")
                print("")
                return True
        #if process stopped prematurely or didn't find a solution, rerun
        if i != niter:
            print("varying starting values of iteration variables")
            vary_iteration_variables(itervars, lbs, ubs)

    return False

def copy_files(from_dir, target_dir, count=None):
    """Copies MFILE.DAT, OUT.DAT, IN.DAT and process.log from from_dir to
       to_dir, prepending count to each file if given.

    """
    files = ["MFILE.DAT", "OUT.DAT", "IN.DAT", "process.log"]

    if not os.path.isdir(target_dir):
        print("Can't find directory %s" % dir)
        exit()

    if count != None:
        prep = str(count).zfill(3) + "."
    else:
        prep = ""

    for file in files:
        from_file = from_dir + "/" + file
        to_file = target_dir + "/" + prep + file
        if not os.path.isfile(from_file):
            print("Can't find %s" % from_file)
            exit()
        os.system("cp " + from_file + " " + to_file)

def get_step_dicts(a_dat, b_dat, allowed_diffs=None):
    """Works out which values are to be stepped from the value in A to the
       value in B. Also checks that all integer switches that do not appear
       in allowed_diffs are identical in both input files.
       Returns two dictionaries of the variables to be stepped. original_dict
       contains the values of the variables in A, target_dict contains the
       values of the variables in B. Any variable that appears in B and a
       value cannot be found in A will be added to a_dat.
       Args
            a_dat --> INDat object corresponding to A IN.DAT
            b_dat --> INDat object corresponding to B IN.DAT
            allowed_diffs --> Set of integer switches that are allowed to
                              differ between A and B
       returns
            (original_dict, target_dict)

    """
    if allowed_diffs == None:
        allowed_diffs = {}
    #target_dict is the dictionary of target values for each variable
    target_dict = {}
    original_dict = {}
    #iterate through everything in B.
    for var in b_dat.data.keys():
        if not isinstance(var, str):
            continue
        #we want both variables and upper and lower bounds
        if var in DICT_VAR_TYPE.keys() or is_bound(var):
            original_val = get_val(var, a_dat)
            target_val = get_val(var, b_dat)

            if original_val == None:
                print("No initial value for %s in A and could"
                        " not find default value in dictionaries" % var)
                exit()
            if target_val == None:
                print("No target value for %s in B and could"
                        " not find default value in dictionaries" % var)
                exit()

            if isinstance(b_dat.data[var].value, float):
                #if float, the variable is a candidate for stepping
                if original_val == target_val:
                    #if the values are the same, we don't
                    #need to step the variable
                    continue
                else:
                    #set original and target dict
                    target_dict[var] = target_val
                    original_dict[var] = original_val

            else:
                #if the variable is allowed to differ or the values are the
                #same, nothing needs to be done
                if var in allowed_diffs or target_val == original_val:
                    continue
                print("Warning, differing values for %s " % var + \
                      "in A and B")
                print("A:", original_val, "  B:", target_val)
                print("Using value in B\n")
                if var in a_dat.data.keys():
                    a_dat.remove_variable(var)
                new_variable = inmod.INVariable(var, target_val)
                a_dat.add_variable(new_variable)
    return original_dict, target_dict

def set_iter_vals(a_dat, b_dat, original_dict, target_dict, bound_fac=1.001):
    """Finds and deals with cases where different iteration variables are
       set in A and B.
       If variable is set as iteration variable:
        In B but not A -> Set as iteration variable in A immediately
        In A but not B -> Gradually narrow the upper and lower bounds to
                           converge on the value set in B. Upper and lower
                           bounds will end up as b_val * bound_fac and
                           b_val / bound_fac.
        In A and B -> If the variable appears in the dictionaries of values to
                      be stepped, remove it
        Neither -> Nothing to be done
       Args
            a_dat --> INDat object corresponding to A IN.DAT
            b_dat --> INDat object corresponding to B IN.DAT
            original_dict --> Dictionary of values to be stepped and value in A
            target_dict --> Dictionary of values to be stepped and value in B
            bound_fac --> Separation factor of target bounds. Default = 1.001

    """
    for num in b_dat.variables["ixc"].value:
        if num not in a_dat.variables["ixc"].value:
            #if the variable is an iteration variable in B but not in
            #A, then make it an iteration variable in A
            print(DICT_IXC_SIMPLE[str(num)], "is set as iteration variable" + \
                " in B but not in A.")
            print("Setting as iteration variable\n")
            a_dat.add_iteration_variable(num)

        if DICT_IXC_SIMPLE[str(num)] in target_dict:
            #Iteration variables should not be stepped, so remove
            #from dictionaries
            del target_dict[DICT_IXC_SIMPLE[str(num)]]
            del original_dict[DICT_IXC_SIMPLE[str(num)]]

    for num in a_dat.variables["ixc"].value:
        if num not in b_dat.variables["ixc"].value:
            #if an iteration value in A, but not in B, we want to narrow
            #the limits of the variable each step. This can be done by
            #setting boundu(#) and boundl(#) in target_dict and original_dict
            name = DICT_IXC_SIMPLE[str(num)]
            tar_val = get_val(name, b_dat)
            if not tar_val:
                print("Could not find a target value for %s" % name)
                exit()
            print(name, "is set as iteration variable in A but" + \
                        " not in B")

            tar_uplimit = tar_val * bound_fac
            tar_downlimit = tar_val / bound_fac
            print("Target limits on %s set as %f and %f\n" % \
                   (name, tar_downlimit, tar_uplimit))

            upname = "boundu(%i)" % num
            target_dict[upname] = tar_uplimit
            original_dict[upname] = get_val(upname, a_dat)

            downname = "boundl(%i)" % num
            target_dict[downname] = tar_downlimit
            original_dict[downname] = get_val(downname, a_dat)

def get_icc_changes(a_dat, b_dat):
    """Returns a list of changes to ICC to make while stepping between
       A and B. Positive numbers indicate an equation to be added, negative
       numbers indicate an equation to be removed
       Args
            a_dat --> INDat object corresponding to A
            b_dat --> INDat object corresponding to B
       Returns
            icc_changes --> List of equation numbers to be changed

    """
    a_icc = a_dat.variables["icc"].value
    b_icc = b_dat.variables["icc"].value
    a_unique = [item for item in a_icc if item not in b_icc]
    b_unique = [item for item in b_icc if item not in a_icc]

    icc_changes = []
    for item in a_unique:
        icc_changes.append(-item)
    icc_changes += b_unique
    return icc_changes

def make_icc_changes(i, nsteps, icc_changes, in_dat):
    """Makes the changes to the icc array of in_dat for a particular step
       Slices the array according to the step number to calculate which
       icc changes should be made this step.
       Args
            i --> Current step counter
            nsteps --> Final number of steps
            icc_changes --> Array of all icc changes to be made from A to B
            in_dat --> INDat object
       Returns
            changes_made --> icc changes made this step

    """
    down_index = round(((i-1) * len(icc_changes)) / nsteps)
    up_index = round(((i) * len(icc_changes)) / nsteps)
    changes_made = []
    for num in icc_changes[down_index : up_index]:
        changes_made.append(num)
        if num < 0:
            print("Removing constraint equation %i ..." % -num)
            in_dat.remove_constraint_eqn(-num)
        elif num > 0:
            print("Adding constraint equation %i ..." % num)
            in_dat.add_constraint_eqn(num)
        else:
            print("0 in icc_changes")
            exit()
    return changes_made

def setup():
    """Sets up the directories for use. Reads A and B to determine
       what the program should do.
       Returns
           original_dict --> intial values of stepped variables
           target_dict --> target values of stepped variables
           icc_changes --> list of icc changes to make

    """
    #if the folders needed don't exist, make them
    if not os.path.isdir(CONF.wdir):
        os.mkdir(CONF.wdir)
    if not os.path.isdir(CONF.outdir):
        os.mkdir(CONF.outdir)

    # Read A and B
    a_dat = inmod.INDATNew(CONF.a)
    b_dat = inmod.INDATNew(CONF.b)

    if "isweep" in a_dat.data.keys():
        if a_dat.data["isweep"].value > 1:
            print("Sweep mode is enabled in A")
            exit()

    if "ioptimz" in a_dat.data.keys():
        ioptimz = a_dat.data["ioptimz"].value
        if not (ioptimz == 0 or ioptimz == 1):
            print("Warning, ioptimz != 0 or 1 in A. Continuing anyway...")

    #set up the dictionaries of values to be stepped
    #original_dict stores the A value of each variable to be stepped
    #target_dict stores the B value
    allowed_diffs = {"neqns", "nvar", "icc", "ixc"}
    original_dict, target_dict = get_step_dicts(a_dat, b_dat, allowed_diffs)

    #deal with iteration variables being set differently in A and B
    set_iter_vals(a_dat, b_dat, original_dict, target_dict, CONF.bound_gap)
    assert sorted(target_dict.keys()) == sorted(original_dict.keys())

    #get icc changes
    icc_changes = get_icc_changes(a_dat, b_dat)

    #output what the program will do
    print("%i steps, stepping values:" % CONF.nsteps)
    for key in sorted(target_dict.keys()):
        print(key, original_dict[key], "-->", target_dict[key])

    if len(icc_changes) > 0:
        print("\nThe following constraint equations will be added"
                " during the run:")
        for num in icc_changes:
            if num > 0:
                print(num)
        print("The following constraint equations will be removed"
                " during the run:")
        for num in icc_changes:
            if num < 0:
                print(-num)

    print("")

    #after making modifications, write to IN.DAT
    a_dat.write_in_dat(CONF.wdir + "/IN.DAT")
    return original_dict, target_dict, icc_changes

def main():
    """Main program routine

    """
    original_dict, target_dict, icc_changes = setup()

    os.chdir(CONF.wdir)
    in_dat = inmod.INDATNew("IN.DAT")

    iter_vars = []
    for num in in_dat.variables["ixc"].value:
        iter_vars.append(DICT_IXC_SIMPLE[str(num)])

    print("doing step 0")
    if not run_process(CONF.path_to_process, CONF.vary_niter, CONF.factor):
        print("Could not complete step 0.")
        print("Try checking OUT.DAT and process.log in wdir")
        exit()
    if CONF.keep_output:
        copy_files(CONF.wdir, CONF.outdir, 0)

    #used for debugging
    icc_changes_made = []

    for i in range(1, CONF.nsteps+1):

        print("doing step %i ..." % i)

        #get the interpolated dictionary
        current_dict = update_dict(original_dict, target_dict, i / CONF.nsteps)
        #get the values of the iteration variables from the last run of
        #process from the MFILE
        mfile_dict = get_mfile_dict("MFILE.DAT", iter_vars)
        #merge the two dictionaries
        mfile_dict.update(current_dict)
        in_dat = update_infile(mfile_dict, in_dat)

        icc_changes_made += make_icc_changes(i, CONF.nsteps,\
                                             icc_changes, in_dat)

        #write the new IN.DAT and run process
        in_dat.write_in_dat("IN.DAT")
        if not run_process(CONF.path_to_process, CONF.vary_niter, CONF.factor):
            print("Could not complete step %i" % i)
            print("Try checking OUT.DAT and process.log in wdir")
            exit()

        #make a copy of this iteration
        if CONF.keep_output:
            copy_files(CONF.wdir, CONF.outdir, i)

    assert icc_changes_made == icc_changes

if __name__ == "__main__":

    print("")
    PARSER = argparse.ArgumentParser(description=\
                                    "Steps a PROCESS solution between two " +
                                    "input files.\nRequires a_to_b.conf in " +
                                    "the working directory")
    ARGS = PARSER.parse_args()
    CONF = AToBConfig("a_to_b.conf")

    main()

    print("Done")
