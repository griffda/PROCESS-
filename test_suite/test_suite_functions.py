#!/usr/bin/env python
"""
  PROCESS Test Suite

  Test suite to be run before each commit. User gives a % difference that is
  acceptable and code will run each test input and output if the difference
  has been exceeded. Code will also report on failed runs.

  James Morris 14/11/2015
  CCFE
"""

# Third party libraries
import os
import sys
import datetime
import argparse
import subprocess
import logging
logging.basicConfig(level=logging.CRITICAL)

# PROCESS libraries
# Uncommented in next commit !
# sys.path.append(os.path.join(os.path.dirname(__file__), '../utilities/'))
# Uncommented in next commit !
from process_io_lib.mfile import MFile

# Constants
EXCLUSIONS = ["normres", "nitvar", "itvar", "xcm"]

# *********************************** #


class BColours:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def clean_test_dir():
    """ Clean test_area
    """

    # remove old test_area
    subprocess.call(["rm", "-rf", "test_area"])

    # create new test_area directory
    subprocess.call(["mkdir","-p", "test_area"])


def welcome_print(df, ars):
    """ Prints welcome message

    :param df: difference allowed
    :param ars: command line arguments
    """

    # welcome to terminal
    print(BColours.BOLD + "\nPROCESS Test Suite" + BColours.ENDC)
    print("Date: {0}".format(datetime.date.today()))
    if not ars.utilities :
        print("Diff set to:" + " {0}%\n".format(df) +
          BColours.ENDC)
    print("Using reference folder: " + BColours.BOLD + "{0}\n".format(ars.ref)
          + BColours.ENDC)

    # save welcome to "summary.log"
    save_summary("PROCESS Test Suite\n\n")
    save_summary("Date: {0}\n".format(datetime.date.today()))
    save_summary("Diff set to:" + " {0}%\n".format(df))
    save_summary("Using reference folder: " + "{0}\n".format(ars.ref))


def print_message(test_name, test_obj):
    """ Function to print output message to terminal

    :param test_name: Name of test being done
    :param test_obj: TestCase class
    """

    if test_obj.status == "OK":
        message = "Test ==>  {0:<40}".format(test_name) + BColours.OKGREEN + \
                  "{0}".format(test_obj.status) + BColours.ENDC

        log_message = "Test ==>  {0:<40}".format(test_name) + \
                      "{0}\n".format(test_obj.status)

    if test_obj.status == "DIFF":
        message = "Test ==>  {0:<40}".format(test_name) + BColours.OKBLUE + \
            "{0}".format(test_obj.status) + "({0})".format(test_obj.diff_num) \
            + BColours.ENDC

        log_message = "Test ==>  {0:<40}".format(test_name) + \
            "{0}".format(test_obj.status) + "({0})\n". \
            format(test_obj.diff_num)

    if test_obj.status == "ERROR":
        message = "Test ==>  {0:<40}".format(test_name) + BColours.FAIL + \
            "{0}".format(test_obj.status) + BColours.ENDC

        log_message = "Test ==>  {0:<40}".format(test_name) + \
            "{0}\n".format(test_obj.status)

    # print message to terminal
    print(message)

    # save message to "summary.log"
    save_summary(log_message)


def setup_executable():
    """ Prepare test

    Setup test_area directory with current version of PROCESS from the
    PROCESS folder
    """
    subprocess.call(["cp ../process.exe ."], shell=True)


def get_file_info(ar):
    """ Function to get the file structure information about the test files.

    :param ar: arguments from command line
    :return: File information as a list of paths
    """

    # List of directories in the reference folder
    dirs = os.listdir(ar.ref)
    
    # File information in dictionary form
    file_info = dict()

    # Directory address
    dir_prefix = os.getcwd() + "/{0}/".format(ar.ref)

    # Populate dictionary with file information
    for folder in dirs:

        # ignore summary file if reference folder not "test_files"
        if ".log" not in folder:

            # dictionary to hold information for given test
            test_info = dict()

            # File path
            direct = dir_prefix + folder + "/"

            # Files for test case (e.g. IN.DAT, reference MFILE.DAT)
            files = os.listdir(direct)

            # store test file names and path
            test_info["files"] = files
            test_info["path"] = direct

            # store test in dictionary of tests
            file_info[folder] = test_info

    return file_info


def get_var_lists(f1, f2):
    """ Variable lists function

    Function to compare two files with lists of parameters and
    return 3 lists. Parameters in both, parameters only in file` and
    parameters only in file2.

    :param f1: 1st file containing parameters to compare
    :param f2: 2nd file containing parameters to compare
    :return: both, in_list1, in_list2
    """

    key_list_1 = f1.data.keys()
    key_list_2 = f2.data.keys()

    in_list1 = []
    in_list2 = []
    both = []

    for var1 in key_list_1:
        status = check_exclusions(var1)
        if status:
            if var1 not in key_list_2:
                in_list1.append(var1)
            else:
                both.append(var1)

    for var2 in key_list_2:
        status = check_exclusions(var2)
        if status:
            if var2 not in key_list_1:
                in_list2.append(var2)

    return both, in_list1, in_list2


def check_exclusions(name):
    """ Remove exclusions

    Function to return

    :param name:
    :return: s: status boolean -> True if not in exclusions
                               -> False if in exclusions
    """

    for thing in EXCLUSIONS:
        if thing in name:
            return False

    return True


def mfile_compare(file1, file2, diff, scan=-1):
    """ MFILE comparison function

    :param file1: 1st file to compare
    :param file2: 2nd file to compare
    :param diff: allowable difference
    :param scan: scan to compare
    :return:
    """

    var_list, only_file1, only_file2 = get_var_lists(file1, file2)

    counter = 0
    differences = dict()

    for item in var_list:

        status = check_exclusions(item)

        if status:

            try:
                a = float(file1.data[item].get_scan(scan))
                b = float(file2.data[item].get_scan(scan))
                d = (1 - (b / a)) * 100.0

                if d > diff:
                    counter += 1
                    diff_item = dict()
                    diff_item["name"] = item
                    diff_item["ref"] = a
                    diff_item["new"] = b
                    diff_item["diff"] = d
                    differences[item] = diff_item

            except ZeroDivisionError:
                pass

            except ValueError:
                pass

    return differences, counter, only_file1, only_file2


def get_scan_num(m_file):
    """ Get scan number

    Function to get the scan number (if there is one).

    If there are scans return number of scans

    If there are no scans return 0

    :param m_file:
    :return: number of scans
    """

    try:
        return m_file.data["isweep"].get_scan(-1)
    except KeyError:
        return 0


def clean_up(drs):
    """ Clean up temporary files

    Function to clean up the test_suite folder.

    :param drs: directories
    """

    # remove .DAT files
    subprocess.call(["rm", "PLOT.DAT"])
    subprocess.call(["rm", "VFILE.DAT"])

    # move kallenbach files
    #subprocess.call(["mv", "divertor_diagnostics.txt", "test_area/kallenbach/"])
    subprocess.call("mv *_divertor*.txt test_area/kallenbach/", shell=True)
    # subprocess.call(["mv", "output_divertor.txt", "test_area/kallenbach/"])

    # remove executable
    subprocess.call(["rm", "process.exe"])


def save_summary(line):
    """ Save summary function

    Function to save a copy of the output to the command line for the PROCESS
    test run.

    :param line: line to add to the summary.log file
    """

    # if not os.path.isfile("summary.log"):
    f = open("summary.log", "a")
    f.write(line)
    f.close()


def copy_test_to_test_area(test_name, test_status, ars):
    """ copy files to test area

    Function to copy test files to test_area under directory for test case name

    :param test_name: test case name
    :param test_status: test case status
    :param ars: command line arguments
    """

    # make test case "test area" directory
    subprocess.call(["mkdir", "-p", "test_area/{0}".format(test_name)])

    # copy output files to test case folders

    # IN.DAT
    subprocess.call(["mv", "IN.DAT", "test_area/{0}/".format(test_name)])

    # ref.IN.DAT
    subprocess.call(["cp", "{0}/{1}/ref.IN.DAT".format(ars.ref, test_name),
                    "test_area/{0}/".format(test_name)])

    # new.MFILE.DAT
    subprocess.call(["mv", "MFILE.DAT", "test_area/{0}/new.MFILE.DAT".
                    format(test_name)])

    # ref.MFILE.DAT
    subprocess.call(["cp", "{0}/{1}/ref.MFILE.DAT".format(ars.ref, test_name),
                     "test_area/{0}/".format(test_name)])

    # new.OUT.DAT to test_files

    subprocess.call(["cp", "OUT.DAT", "test_files/{0}/new.OUT.DAT".
                    format(test_name)])

    # new.OUT.DAT
    subprocess.call(["mv", "OUT.DAT", "test_area/{0}/new.OUT.DAT".
                    format(test_name)])

    # ref.OUT.DAT
    subprocess.call(["cp", "{0}/{1}/ref.OUT.DAT".format(ars.ref, test_name),
                     "test_area/{0}/".format(test_name)])

    # README file
    subprocess.call(["cp", "{0}/{1}/README.txt".format(ars.ref, test_name),
                     "test_area/{0}/".format(test_name)])

    # run.log
    subprocess.call(["mv", "run.log", "test_area/{0}/".format(test_name)])

    # diff.log
    if test_status == "DIFF":
        subprocess.call(["mv", "diff.log", "test_area/{0}/".format(test_name)])


def move_summary():
    """ Move summary log to test_area
    """

    # summary.log
    subprocess.call(["mv", "summary.log", "test_area"])


def close_print(ver):
    """Function to print closing summary

    Prints closing summary to the terminal and the summary.log file.
    """

    msg = "\nPROCESS version {0} test run complete".format(ver)

    # To terminal
    print(BColours.BOLD + msg + BColours.ENDC)

    # To summary.log
    save_summary(msg)


def write_diff_log(test, diff_val, diffs, diff_n, only_ref, only_new):
    """Write diff.log file

    Function to write diff.log file.

    :param test: test name
    :param difff_val: difference value
    :param diffs: dictionary of differences in MFILEs
    :param diff_n: number of differences
    :param only_ref: variables only in reference MFILE not new MFILE
    :param only_new: variables only in new MFILE not reference MFILE
    """

    diff_file = open("diff.log", "w")
    diff_file.write("\nPROCESS Test Suite\n")
    diff_file.write("\nTest name: {0}\n".format(test))
    diff_file.write("Difference value: {0}%\n".format(diff_val))

    diff_file.write("\nTotal of {0} differences above allowed value\n\n".
                    format(diff_n))
    diff_file.write("{0:<40}\t{1:<10}\t{2:<10}\t{3:<10}\n".
                    format("Variable", "ref", "new", "diff (%)"))
    # diff_file.write("Columns: Var\tref\tnew\tdiff\n")
    diff_file.write("-"*40+"\n")

    # write differences
    for key in diffs.keys():
        df = diffs[key]["diff"]
        rf = diffs[key]["ref"]
        nw = d = diffs[key]["new"]

        diff_file.write("{0:<40}\t{1:<10.3g}\t{2:<10.3g}\t{3:<10.2f}\n".
                        format(key[:39], rf, nw, df))

    # write variables in ref but not in new
    diff_file.write("\n\nThere are {0} variables in ref NOT IN new:\n\n".
                    format(len(only_ref)))
    for item in only_ref:
        diff_file.write("{0}\n".format(item))

    # write variables in new but not in ref
    diff_file.write("\n\nThere are {0} variables in new NOT IN ref:\n\n".
                    format(len(only_new)))
    for item in only_new:
        diff_file.write("{0}\n".format(item))

    # close file
    diff_file.close()

def write_xunit_files(test, diff_val, diffs, diff_n, only_ref, only_new):
    """Write xunit file

    Function to write test.log file.

    :param test: test name
    :param difff_val: difference value
    :param diffs: dictionary of differences in MFILEs
    :param diff_n: number of differences
    :param only_ref: variables only in reference MFILE not new MFILE
    :param only_new: variables only in new MFILE not reference MFILE
    """

    ref_file = open(test+".ref.xunit", "w")
    new_file = open(test+".new.xunit", "w")
    # write differences
    for key in diffs.keys():
        df = diffs[key]["diff"]
        rf = diffs[key]["ref"]
        nw = d = diffs[key]["new"]

        ref_file.write("{0:<40}\t{1:<10}\t{2:<10.3g}\t{3:<10}\t{4:<10.2f}\n".
                        format(key[:39],"=" , rf, " *  Difference(%) = ", df))
        new_file.write("{0:<40}\t{1:<10}\t{2:<10.3g}\t{3:<10}\t{4:<10.2f}\n".
                        format(key[:39], "=", nw, " *  Difference(%) = ", df))

    # write variables in ref but not in new
##    diff_file.write("\n\nThere are {0} variables in ref NOT IN new:\n\n".
##                    format(len(only_ref)))
##    for item in only_ref:
##        diff_file.write("{0}\n".format(item))

    # write variables in new but not in ref
##    diff_file.write("\n\nThere are {0} variables in new NOT IN ref:\n\n".
##                    format(len(only_new)))
##    for item in only_new:
##        diff_file.write("{0}\n".format(item))

    # close file
    ref_file.close()
    new_file.close()

def check_process_errors():
    """Check PROCESS run errors

    Function to check errors from PROCESS run using run.log and outputs if
    there are any.

    """

    # check 1: check MFILE file length
    mf = open("MFILE.DAT", "r", encoding="utf-8")
    mf_length = len(mf.readlines())

    if mf_length == 0:
        return "ERROR"

    return "OK"


def get_version(tests_dict):
    """Get version number

    :param test: tests_dictionary
    """

    version_numbers = list()

    for key in tests_dict.keys():
        version_numbers.append(tests_dict[key].proc_ver)

    for item in version_numbers:
        if item != "":
            return item


def overwrite_references(ars, dr, vr):
    """Overwrite the reference cases with new output.

    :param ars: command line arguments
    :param dr: directories for reference cases
    :param vr: PROCESS version number
    """

    if ars.save:
        output_folder = "test_{0}".format(vr)
    else:
        output_folder = "test_area"

    for key in dr.keys():

        if "error_" not in key:
            path = dr[key]["path"]

            # copy IN.DAT to ref.IN.DAT
            subprocess.call(["cp", "{0}/{1}/IN.DAT".format(output_folder, key),
                            "{0}/IN.DAT".format(path)])
            subprocess.call(["cp", "{0}/{1}/IN.DAT".format(output_folder, key),
                            "{0}/ref.IN.DAT".format(path)])

            # copy new.OUT.DAT to ref.OUT.DAT
            subprocess.call(["cp", "{0}/{1}/new.OUT.DAT".format(output_folder,
                            key), "{0}/ref.OUT.DAT".format(path)])

            # copy new.MFILE.DAT to ref.MFILE.DAT
            subprocess.call(["cp", "{0}/{1}/new.MFILE.DAT".
                            format(output_folder, key),
                            "{0}/ref.MFILE.DAT".format(path)])


def amend_utility_log(u):
    """ Update utility log with header for utility
    """
    f = open("utilities.log", "a")
    f.write("\n## {0:<38} ##\n".format(u))
    f.write("#"*44)
    f.write("\n")
    f.close()


def test_utilities(file_dict, utils_only):
    """Testing python utilities for PROCESS

    :param file_dict: dictionary of the test files for PROCESS.
    """
    
    # utility testing message to terminal and summary.log
    print(BColours.BOLD + "\nUtilities and Python libraries testing\n" +
          BColours.ENDC)
    save_summary("\n\nUtilities and Python libraries testing\n\n")

    # utility testing message to terminal and summary.log
    print("Check utilities.log for warnings and errors.\n")
    save_summary("\n\nCheck utilities.log for warnings and errors.\n\n")

    # test mfile.py library
    test_mfile_lib(file_dict, utils_only)

    # test in_dat.py library
    test_in_dat_lib(file_dict, utils_only)

    # test make_plot_dat
    # TODO test_make_plot_dat(file_dict)

    # test plot_proc.py
    test_plot_proc(file_dict, utils_only)

    # test convert_in_dat.py
    # TODO test_convert_in_dat(file_dict)

    # move "utilities.log"
    subprocess.call(["mv", "utilities.log", "test_area"])

    return


def test_mfile_lib(fs, utils_only):
    """Test the PROCESS mfile library

    :param fs: files to test
    """

    import process_io_lib.mfile as mf

    # results list
    results = list()

    # add t utilities.log
    amend_utility_log("mfile.py")

    sys.stdout = open("utilities.log", "a")

    # test all MFILEs
    for key in fs.keys():
        if "error_" not in key:
            if utils_only:
                file_name = "test_files/{0}/ref.MFILE.DAT".format(key)
            else:
                file_name = "test_area/{0}/new.MFILE.DAT".format(key)
            results.append(mf.test(file_name))

    sys.stdout = sys.__stdout__

    # Output message
    msg = "Test ==>  {0:<40}".format("mfile.py")

    # check results
    if len(results) == results.count(True):
        lmsg = msg + "OK\n"
        msg += BColours.OKGREEN + "OK" + BColours.ENDC

    else:
        lmsg = msg + "ERROR\n"
        msg += BColours.FAIL + "ERROR" + BColours.ENDC

    print(msg)
    save_summary(lmsg)


def test_in_dat_lib(fs, utils_only):
    """Test the PROCESS in_dat library

    :param fs: files to test
    """

    import process_io_lib.in_dat as indat

    # results list
    results = list()
    files_in_order = list()

    # add to utilities.log
    amend_utility_log("in_dat.py")

    sys.stdout = open("utilities.log", "a")

    # test all MFILEs
    for key in fs.keys():
        if "error_" not in key:
            if utils_only:
                file_name = "test_files/{0}/ref.IN.DAT".format(key)
            else:
                file_name = "test_area/{0}/IN.DAT".format(key)
            files_in_order.append(file_name)
            # file_name = fs[key]["path"] + "IN.DAT"
            results.append(indat.test(file_name))

    sys.stdout = sys.__stdout__

    # Output message
    msg = "Test ==>  {0:<40}".format("in_dat.py")

    # check results
    error_status = False
    if len(results) == results.count(True):
        lmsg = msg + "OK\n"
        msg += BColours.OKGREEN + "OK" + BColours.ENDC
    else:
        lmsg = msg + "ERROR\n"
        msg += BColours.FAIL + "ERROR" + BColours.ENDC
        error_status = True

    print(msg)
    save_summary(lmsg)

    if error_status:
        print(results)
        print(files_in_order)
        sys.exit("in_dat test_suite failure")


def test_plot_proc(fs, utils_only):
    """Test the PROCESS in_dat library

    :param fs: files to test
    """

    import plot_proc as pp

    # results list
    results = list()

    # add to utilities.log
    amend_utility_log("plot_proc.py")

    # sys.stdout = open("utilities.log", "a")

    # test all MFILEs
    for key in fs.keys():
        if "error_" not in key:
            if "stellarator" not in key:
                if utils_only:
                    file_name = "test_files/{0}/ref.MFILE.DAT".format(key)
                else:
                    file_name = "test_area/{0}/new.MFILE.DAT".format(key)
                # file_name = fs[key]["path"] + "new.MFILE.DAT"
                results.append(pp.test(file_name))

            # if results[-1]:
            #    subprocess.call(["mv", "ref.SUMMARY.pdf", "test_area/{0}/".
            #                    format(key)])

    # sys.stdout = sys.__stdout__

    # Output message
    msg = "Test ==>  {0:<40}".format("plot_proc.py")

    # check results
    error_status = False
    if len(results) == results.count(True):
        lmsg = msg + "OK\n"
        msg += BColours.OKGREEN + "OK" + BColours.ENDC
    else:
        lmsg = msg + "ERROR\n"
        msg += BColours.FAIL + "ERROR" + BColours.ENDC
        error_status = True

    print(msg)
    save_summary(lmsg)

    if error_status:
        sys.exit("plot_proc test_suite failure")


class TestCase(object):
    """
    Class used for a single reference test case

    Takes input file, reference output MFILE and allowed difference as a %.
    """

    def __init__(self, test, files, diff, arguments):
        """TestCase initialisation

        :param test: test case name
        :param files: input file to compare to reference case
        :param diff: difference allowed (can be changed by user)
        :param arguments: command line arguments
        :return: (output to terminal and files)
        """
        self.test = test
        self.files = files["files"]
        self.path = files["path"]
        self.status = "OK"
        self.process_exit_code = 0
        self.diff_num = 0
        self.diff = diff
        self.diffs = dict()
        self.proc_ver = str()
        self.only_ref = list()
        self.only_new = list()
        self.arguments = arguments


    def run_test(self):
        """ Run PROCESS test

        Function to run PROCESS for the given test and then organise the
        files appropriately.
        """

        if self.test == "stellarator":
            subprocess.call(["cp {0} .".format(self.path + "device.dat")],
                            shell=True)
            subprocess.call(["cp {0} .".format(self.path + "vmec_info.dat")],
                            shell=True)
            subprocess.call(["cp {0} .".format(self.path + "vmec_Rmn.dat")],
                            shell=True)
            subprocess.call(["cp {0} .".format(self.path + "vmec_Zmn.dat")],
                            shell=True)

        # run PROCESS
        subprocess.call(["cp {0} .".format(self.path + "IN.DAT")],
                        shell=True)
        self.process_exit_code = subprocess.call(["./process.exe >> run.log"],
                                                 shell=True, timeout=1000)

        if self.test == "stellarator":
            subprocess.call(["rm device.dat"], shell=True)
            subprocess.call(["rm vmec_info.dat"], shell=True)
            subprocess.call(["rm vmec_Rmn.dat"], shell=True)
            subprocess.call(["rm vmec_Zmn.dat"], shell=True)

        # check PROCESS call exit code
        if self.process_exit_code != 0:
            self.status = "ERROR"

        # check for PROCESS errors
        self.status = check_process_errors()

        if self.status == "ERROR":
            copy_test_to_test_area(self.test, self.status, self.arguments)
            return

        # read MFILEs
        ref_mfile = MFile("{0}".format(self.path + "ref.MFILE.DAT"))
        new_mfile = MFile("{0}".format("MFILE.DAT"))

        # get process version number
        self.proc_ver = new_mfile.data["procver"].get_scan(-1)

        # check ifail
        self.ifail = new_mfile.data["ifail"].get_scan(-1)

        if self.ifail != 1:
            self.status = "ERROR"
            copy_test_to_test_area(self.test, self.status, self.arguments)
            return

        # get number of scans
        # scan_no = get_scan_num(ref_mfile)
        # if scan_no:

        # Compare MFILE versions
        self.diffs, self.diff_num, self.only_ref, self.only_new = \
            mfile_compare(ref_mfile, new_mfile, self.diff)


        return

    def check_diff_status(self):
        """Check status of comparison after comparing mfiles

        :return: True/False if there are differences
        """

        if self.diff_num > 0:
            return True

        if len(self.only_new) > 0:
            return True

        if len(self.only_ref) > 0:
            return True

        return False
    def user_run_test(self):
        """ Run PROCESS test by user
        This function will call run_test
        Function to run PROCESS for the given test and then organise the
        files appropriately.
        """
        self.run_test()
        if self.check_diff_status():

            # change test status
            self.status = "DIFF"

            # output differences to diff.log
            write_diff_log(self.test, self.diff, self.diffs, self.diff_num,
                           self.only_ref, self.only_new)
        # copy files to test_area
        copy_test_to_test_area(self.test, self.status, self.arguments)

        return
    def CI_run_test(self):
        """ Run PROCESS test by user
        This function will call run_test
        Function to run PROCESS for the given test and then organise the
        files appropriately.
        """
        self.run_test()
        if self.check_diff_status():

            # change test status
            self.status = "DIFF"

            # output differences to test.log
            write_xunit_files(self.test, self.diff, self.diffs, self.diff_num,
                           self.only_ref, self.only_new)
        # copy files to test_area
        copy_test_to_test_area(self.test, self.status, self.arguments)

        return

def main(args):
    """Main

    :param args: command line arguments
    """

    # Allowable difference set by user
    difference = args.diff

    # Print intro message
    welcome_print(difference, args)

    # clean test_area directory
    clean_test_dir()

    # Get test file information
    drs = get_file_info(args)

    # Print start of tests message
    msg = BColours.BOLD + "PROCESS Test Cases\n" + BColours.ENDC
    print(msg)
    log_msg = "\nPROCESS Test Cases\n\n"
    save_summary(log_msg)

    # setup test_area executable
    setup_executable()

    # dictionary for test cases
    tests = dict()

    # Go through and test each case
    for key in drs.keys():

        if "error_" in key:

            if args.debug:
                # initiate test object for the test case
                tests[key] = TestCase(key, drs[key], difference, args)


                # run test
                tests[key].user_run_test()

                # Output message to terminal
                print_message(key, tests[key])

            else:
                pass
        else:

            # initiate test object for the test case
            tests[key] = TestCase(key, drs[key], difference, args)

            # run test
            print("Starting test ==>  {0:<40}".format(key))
            tests[key].user_run_test()

            # Output message to terminal
            print_message(key, tests[key])

    # version number
    vrsn = get_version(tests)

    # Closing messages
    close_print(vrsn)

    # test utilities
    test_utilities(drs)

    # move summary
    move_summary()

    # Write test to folder if successful and option chosen
    if args.save:

        # delete any folders with the same name
        subprocess.call(["rm", "-rf", "test_{0}".format(vrsn)])

        # copy test results to version numbered folder
        subprocess.call(["cp", "-r", "test_area", "test_{0}".format(vrsn)])

        # remove temporary test results folder
        subprocess.call(["rm", "-rf", "test_area"])

        print("\nTest run saved to folder 'test_{0}'".format(vrsn))

    # cleanup
    clean_up(drs)

    # if option given to overwrite reference cases with new output.
    if args.overwrite:
        overwrite_references(args, drs, vrsn)

    return
