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
import datetime
import argparse
import subprocess

# PROCESS libraries
from process_io_lib.mfile import MFile

# Constants
EXCLUSIONS = ["normres", "nitvar"]

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
    subprocess.call(["mkdir", "test_area"])


def welcome_print(df):
    """ Prints welcome message

    :param df: difference allowed
    """

    # welcome to terminal
    print(BColours.BOLD + "\nPROCESS Test Suite" + BColours.ENDC)
    print("Date: {0}".format(datetime.date.today()))
    print("Diff set to:" + " {0}%\n".format(df) +
          BColours.ENDC)

    # save welcome to "summary.log"
    save_summary("PROCESS Test Suite\n\n")
    save_summary("Date: {0}\n".format(datetime.date.today()))
    save_summary("Diff set to:" + " {0}%\n".format(df))


def print_message(test_name, test_obj):
    """ Function to print output message to terminal

    :param test_name: Name of test being done
    :param test_obj: TestCase class
    """

    if test_obj.status == "OK":
        message = "Test ==>  {0:<30}".format(test_name) + BColours.OKGREEN + \
                  "{0}".format(test_obj.status) + BColours.ENDC

        log_message = "\nTest ==>  {0:<30}".format(test_name) + \
                      "{0}".format(test_obj.status)

    if test_obj.status == "DIFF":
        message = "Test ==>  {0:<30}".format(test_name) + BColours.OKBLUE + \
            "{0}".format(test_obj.status) + "({0})".format(test_obj.diff_num) \
            + BColours.ENDC

        log_message = "Test ==>  {0:<30}".format(test_name) + \
            "{0}".format(test_obj.status) + "({0})\n". \
            format(test_obj.diff_num)

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


def get_file_info():
    """ Function to get the file structure information about the test files.

    :return: File information as a list of paths
    """

    # List of directories in the test_files folder
    dirs = os.listdir("test_files/")

    # File information in dictionary form
    file_info = dict()

    # Directory address
    dir_prefix = os.getcwd() + "/test_files/"

    # Populate dictionary with file information
    for folder in dirs:
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
        if var1 not in key_list_2:
            in_list1.append(var1)
        else:
            both.append(var1)

    for var2 in key_list_2:
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

    var_list, only_file1, only_file2 = get_var_lists(file1, file1)

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


def clean_up():
    """ Clean up temporary files

    Function to clean up the test_suite folder.
    """

    # remove DAT files
    subprocess.call(["rm", "IN.DAT"])
    subprocess.call(["rm", "OUT.DAT"])
    subprocess.call(["rm", "MFILE.DAT"])
    subprocess.call(["rm", "PLOT.DAT"])

    # remove LOG files
    subprocess.call(["rm", "run.log"])
    subprocess.call(["rm", "summary.log"])
    subprocess.call(["rm", "diff.log"])

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


def copy_test_to_test_area(test_name):
    """ copy files to test area

    Function to copy test files to test_area under directory for test case name

    :param test_name: test case name
    """

    # make test case "test area" directory
    subprocess.call(["mkdir", "test_area/{0}".format(test_name)])

    # copy output files to test case folders

    # IN.DAT
    subprocess.call(["cp", "IN.DAT", "test_area/{0}/".format(test_name)])

    # ref.IN.DAT
    subprocess.call(["cp", "test_files/{0}/ref.IN.DAT".format(test_name),
                    "test_area/{0}/".format(test_name)])

    # new.MFILE.DAT
    subprocess.call(["cp", "MFILE.DAT", "test_area/{0}/new.MFILE.DAT".
                    format(test_name)])

    # ref.MFILE.DAT
    subprocess.call(["cp", "test_files/{0}/ref.MFILE.DAT".format(test_name),
                     "test_area/{0}/".format(test_name)])

    # new.OUT.DAT
    subprocess.call(["cp", "OUT.DAT", "test_area/{0}/new.OUT.DAT".
                    format(test_name)])

    # ref.OUT.DAT
    subprocess.call(["cp", "test_files/{0}/ref.OUT.DAT".format(test_name),
                     "test_area/{0}/".format(test_name)])

    # run.log
    subprocess.call(["cp", "run.log", "test_area/{0}/".format(test_name)])

    # diff.log
    subprocess.call(["cp", "diff.log", "test_area/{0}/".format(test_name)])


def move_summary():
    """ Move summary log to test_area
    """

    # summary.log
    subprocess.call(["cp", "summary.log", "test_area"])


def close_print(ver):
    """Function to print closing summary

    Prints closing summary to the terminal and the summary.log file.
    """

    msg = "\nPROCESS version r{0} test run complete".format(ver)

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
    diff_file.write("Difference value: {0}\n".format(diff_val))

    diff_file.write("\nDifferences above allowed value\n\n")
    diff_file.write("Columns: Var\tref\tnew\tdiff\n")
    diff_file.write("-"*40+"\n")

    # write differences
    for key in diffs.keys():
        df = diffs[key]["diff"]
        rf = diffs[key]["ref"]
        nw = d = diffs[key]["new"]

        diff_file.write("{0:<40}\t{1:.3g}\t{2:.3g}\t{3:.3g}\n".
                        format(key, rf, nw, df))

    # write variables in ref but not in new
    diff_file.write("Variables in ref NOT IN new:\n\n")
    for item in only_ref:
        diff_file.write("{0}\n".format(item))

    # write variables in new but not in ref
    diff_file.write("Variables in new NOT IN ref:\n\n")
    for item in only_new:
        diff_file.write("{0}\n".format(item))

    # close file
    diff_file.close()


class TestCase(object):
    """
    Class used for a single reference test case

    Takes input file, reference output MFILE and allowed difference as a %.
    """

    def __init__(self, test, files, diff):
        """TestCase initialisation

        :param test: test case name
        :param files: input file to compare to reference case
        :param diff: difference allowed (can be changed by user)
        :return: (output to terminal and files)
        """
        self.test = test
        self.files = files["files"]
        self.path = files["path"]
        self.status = "OK"
        self.diff_num = 0
        self.diff = diff
        self.diffs = dict()
        self.proc_ver = str()
        self.only_ref = list()
        self.only_new = list()

    def run_test(self):
        """ Run PROCESS test

        Function to run PROCESS for the given test and then organise the
        files appropriately.
        """

        # run PROCESS
        subprocess.call(["cp {0} .".format(self.path + "/IN.DAT")],
                        shell=True)
        subprocess.call(["process.exe >> run.log"], shell=True)

        # read MFILEs
        ref_mfile = MFile("{0}".format(self.path + "ref.MFILE.DAT"))
        new_mfile = MFile("{0}".format("MFILE.DAT"))

        # get process version number
        self.proc_ver = new_mfile.data["procver"].get_scan(-1)

        # get number of scans
        # scan_no = get_scan_num(ref_mfile)
        # if scan_no:

        # Compare MFILE versions
        self.diffs, self.diff_num, self.only_ref, self.only_new = \
            mfile_compare(ref_mfile, new_mfile, self.diff)

        # if number of differences non-zero change status
        if self.diff_num > 0:

            # change test status
            self.status = "DIFF"

            # output differences to diff.log
            write_diff_log(self.test, self.diff, self.diffs, self.diff_num,
                           self.only_ref, self.only_new)

        #  print differences
        # TODO for key in self.diffs.keys():
        #    print(key, self.diffs[key]["name"], self.diffs[key]["diff"])

        # copy files to test_area
        copy_test_to_test_area(self.test)

        # rename output files
        subprocess.call([""], shell=True)

        # print("Reference value {0}".format(ref_mfile.data["rmajor"].
        # get_scan(-1)))


def main(args):
    """
    :param args:
    """

    # Allowable difference set by user
    difference = args.diff

    # Print intro message
    welcome_print(difference)

    # clean test_area directory
    clean_test_dir()

    # Get test file information
    drs = get_file_info()

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
        # initiate test object for the test case
        tests[key] = TestCase(key, drs[key], difference)

        # run test
        tests[key].run_test()

        # Output message to terminal
        print_message(key, tests[key])

        # Output message to file
        # write_message(key, t)

    # version number
    vrsn = int(tests[list(drs.keys())[0]].proc_ver)

    # Closing messages
    close_print(vrsn)

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

        print("\nTest run saved to folder 'test_{0}'".format(k))

    # cleanup
    clean_up()

    return


if __name__ == "__main__":
    des = """PROCESS Test Suite.
           Use to test your changes to the PROCESS code against the
           reference cases. User can speicify % difference allowed.
           For info contact james.morris2@ccfe.ac.uk"""

    parser = argparse.ArgumentParser(description=des)

    parser.add_argument("-d", "--diff", help="Set allowed difference between "
                                             "two files in percentage terms.",
                                             type=float, default=5.0)

    parser.add_argument("-s", "--save", help="Save outputs to new folders for"
                        "reference case for this version of PROCESS.",
                        action="store_true")

    ag = parser.parse_args()

    main(ag)

    # Make sure terminal returns to regular colours
    print(BColours.ENDC)

# TODO: Have scans included and ouput
# TODO: If all tests successful then overwrite files with same tag & date
# TODO: User option for above?
# TODO: Output differences above user % 10% and 50%
# TODO: Save output to summary.log file
# TODO: Include in make file
