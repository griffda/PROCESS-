"""Run Process by calling into the Fortran.

This uses a Python module called fortran.py, which uses an extension module 
called "_fortran.cpython... .so", which are both generated from 
process_module.f90. The process_module module contains the code to actually run 
Process.

This file, process.py, is now analogous to process.f90, which contains the 
Fortran "program" statement. This Python module effectively acts as the Fortran
"program".

Power Reactor Optimisation Code for Environmental and Safety Studies
P J Knight, CCFE, Culham Science Centre
J Morris, CCFE, Culham Science Centre

This is a systems code that evaluates various physics and
engineering aspects of a fusion power plant subject to given
constraints, and can optimise these parameters by minimising
or maximising a function of them, such as the fusion power or
cost of electricity.

This program is derived from the TETRA and STORAC codes produced by
Oak Ridge National Laboratory, Tennessee, USA. The main authors in
the USA were J.D.Galambos and P.C.Shipe.

The code was transferred to Culham Laboratory, Oxfordshire, UK, in
April 1992, and the physics models were updated by P.J.Knight to
include the findings of the Culham reactor studies documented in
Culham Report AEA FUS 172 (1992). The standard of the Fortran has
been thoroughly upgraded since that time, and a number of additional
models have been added.

During 2012, PROCESS was upgraded from FORTRAN 77 to Fortran 95,
to facilitate the restructuring of the code into proper modules
(with all the benefits that modern software practices bring), and to
aid the inclusion of more advanced physics and engineering models under
development as part of a number of EFDA-sponsored collaborations.

AEA FUS 251: A User's Guide to the PROCESS Systems Code
Box file F/RS/CIRE5523/PWF (up to 15/01/96)
Box file F/MI/PJK/PROCESS and F/PL/PJK/PROCESS (15/01/96 to 24/01/12)
Box file T&amp;M/PKNIGHT/PROCESS (from 24/01/12)
"""
from process import fortran
import argparse
from pathlib import Path
import sys

class Process():
    """The main Process class."""
    def __init__(self, args=None):
        """Run Process.

        :param args: Arguments to parse, defaults to None
        :type args: list, optional
        """
        self.parse_args(args)
        self.set_filenames()
        self.initialise()
        self.run_hare_tests()
        self.kallenbach_tests()
        self.kallenbach_scan()
        self.call_solver()
        self.scan()
        self.show_errors()
        self.append_input()
        self.finish()
    
    def parse_args(self, args=None):
        """Parse the command-line arguments, such as the input filename.

        :param args: Arguments to parse, defaults to None
        :type args: list, optional
        """
        parser = argparse.ArgumentParser(description=("PROCESS\n"
            "Power Reactor Optimisation Code\n"
            "Usage\n"
            "Running code with IN.DAT        : ./<path_to_executable/process.exe\n"
            "Running code with named IN.DAT  : ./<path_to_executable/process.exe <path_to_input>/<file_prefix>IN.DAT\n"
            "Help info                       : ./<path_to_executable/process.exe help\n"
        
            "Example Usage\n"
            "Executable in current dir and input called IN.DAT in current dir  : ./process.exe\n"
            "Executable in current dir and named input in current dir          : ./process.exe tokamak_IN.DAT\n"
            "Executable in other dir and named input in other dir              : ./bin/process.exe ../../ITER_IN.DAT\n"
            "Executable in other dir and input called IN.DAT in current dir    : ./bin/process.exe\n"
    
            "Input\n"
            "Input file naming convention : <file_prefix>IN.DAT\n"
        
            "Input file syntax\n"
            "Constraint equation             : icc = <constraint_number>\n"
            "Iteration variable              : ixc = <iteration_variable_number>\n"
            "Iteration variable lower bound  : boundl(<iteration_variable_number\n> = <bound_value>\n"
            "Iteration variable upper bound  : boundu(<iteration_variable_number\n> = <bound_value>\n"
            "Parameter                       : <parameter_name> = <parameter_value>\n"
            "Array                           : <array_name>(<array_index\n> = <index_value>\n"
        
            "Output\n"
            "Output files naming convention : <file_prefix>OUT.DAT\n"
            "                               : <file_prefix>MFILE.DAT\n"
            "                               : <file_prefix>PLOT.DAT\n"
            
            "Contact\n"
            "James Morris  : james.morris2@ukaea.uk\n"
            "Hanni Lux     : hanni.lux@ukaea.uk\n"
            "GitLab        : git.ccfe.ac.uk\n")
            )

        # Optional args
        parser.add_argument("--input",
            "-i",
            default="IN.DAT",
            metavar="input_file_path",
            type=str,
            help="The path to the input file that Process runs on")

        # If args is not None, then parse the supplied arguments. This is likely
        # to come from the test suite when testing command-line arguments; the 
        # method is being run from the test suite.
        # If args is None, then use actual command-line arguments (e.g. 
        # sys.argv), as the method is being run from the command-line.
        self.args = parser.parse_args(args)
        # Store namespace object of the args

    def set_filenames(self):
        """Validate the input filename and create other filenames from it."""
        self.set_input()
        self.set_output()
        self.set_mfile()

    def set_input(self):
        """Validate and set the input file path."""
        # Check input file ends in "IN.DAT", then save prefix
        # (the part before the IN.DAT)
        if self.args.input[-6:] != "IN.DAT":
            raise ValueError("Input filename must end in IN.DAT.")
        
        self.filename_prefix = self.args.input[:-6]

        # Check input file exists (path specified as CLI argument)
        input_path = Path(self.args.input)
        if input_path.exists():
            self.input_path = input_path
            # Set input as Path object
        else:
            raise FileNotFoundError("Input file not found on this path. There "
                "is no input file named", self.args.input, "in the analysis "
                "folder")

        # Set the input file in the Fortran
        fortran.global_variables.fileprefix = str(self.input_path.resolve())

    def set_output(self):
        """Set the output file name.
        
        Set Path object on the Process object, and set the prefix in the Fortran.
        """
        self.output_path = Path(self.filename_prefix + "OUT.DAT")
        fortran.global_variables.output_prefix = self.filename_prefix

    def set_mfile(self):
        """Set the mfile filename."""
        self.mfile_path = Path(self.filename_prefix + "MFILE.DAT")

    def initialise(self):
        """Run the init module to call all initialisation routines."""
        fortran.init_module.init()

    def run_hare_tests(self):
        """Run HARE tests if required to by input file."""
        # TODO This would do better in a separate input validation module.
        if fortran.global_variables.run_tests == 1:
            fortran.main_module.runtests()

    def kallenbach_tests(self):
        """Run Kallenbach tests if required."""
        if fortran.div_kal_vars.kallenbach_tests == 1:
            fortran.kallenbach_module.kallenbach_testing()
            # Exit if just running the Kallenbach tests
            sys.exit()

    def kallenbach_scan(self):
        """Run Kallenbach scan if required."""
        if fortran.div_kal_vars.kallenbach_scan_switch == 1:
            fortran.kallenbach_module.kallenbach_scan()
            # Exit if just running the scan
            sys.exit()

    def call_solver(self):
        """Call the equation solver (HYBRD)."""
        self.ifail = fortran.main_module.eqslv()

    def scan(self):
        """Call scan routine if required."""
        if fortran.numerics.ioptimz >= 0:
            fortran.scan_module.scan()
        else:
            fortran.final_module.final(self.ifail)

    def show_errors(self):
        """Report all informational/error messages encountered."""
        fortran.error_handling.show_errors()

    def append_input(self):
        """Append the input file to the output file and mfile."""
        # Read IN.DAT input file
        with open(self.input_path, 'r') as input_file:
            input_lines = input_file.readlines()

        # Append the input file to the output file
        with open(self.output_path, 'a') as output_file:
            output_file.writelines(input_lines)

        # Append the input file to the mfile
        with open(self.mfile_path, 'a') as mfile_file:
            mfile_file.write("***********************************************")
            mfile_file.writelines(input_lines)

    def finish(self):
        """Run the finish subroutine to close files open in the Fortran."""
        fortran.init_module.finish()

def main(args=None):
    """Run Process.

    The args parameter is used to control command-line arguments when running
    tests. Optional args can be supplied by different tests, which are then 
    used instead of command-line arguments by argparse. This allows testing of
    different command-line arguments from the test suite.

    :param args: Arguments to parse, defaults to None
    :type args: list, optional
    """
    Process(args)