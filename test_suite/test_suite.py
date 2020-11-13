#!/usr/bin/env python
"""
  PROCESS Test Suite

  Test suite to be run before each commit. User gives a % difference that is
  acceptable and code will run each test input and output if the difference
  has been exceeded. Code will also report on failed runs.

  James Morris 14/11/2015
  CCFE
"""
from test_suite_functions import *

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
    utils_only = args.utilities
    if not utils_only : 
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
    
        errors = list()
        for ky in drs.keys():
            if "error_" not in ky:
                if tests[ky].status == "ERROR":
                    errors.append(ky)
    
        if len(errors) >= 1:
            sys.exit("ERROR in test case(s) :: {0}".format(errors))
    
        # version number
        vrsn = get_version(tests)
    
        # Closing messages
        close_print(vrsn)

    # test utilities
    test_utilities(drs, utils_only)

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
    if not utils_only:
        clean_up(drs)

    # if option given to overwrite reference cases with new output.
    if args.overwrite:
        overwrite_references(args, drs, vrsn)

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

    parser.add_argument("--debug", help="Use debugging reference cases (cases "
                        "beginning with 'error_').", action="store_true")

    parser.add_argument("--overwrite", help="Overwrite reference cases with"
                        "new output. USE WITH CAUTION.", action="store_true")

    parser.add_argument("-r", "--ref", help="Set reference folder. Default ="
                        "test_files", type=str, default="test_files")

    parser.add_argument("-u", "--utilities", help="Test utilities only", 
                        action="store_true")

    parser.add_argument("--verbose", help="List differences in terminal as well as log", 
                        action="store_true", default=False)

    ag = parser.parse_args()
    main(ag)

    # Make sure terminal returns to regular colours
    print(BColours.ENDC)
