"""Run Process by calling into the Fortran.

This uses a Python module called fortran.py, which uses an extension module 
called "_fortran.cpython... .so", which are both generated from 
process_module.f90. The process_module module contains the code to actually run 
Process.

This file, process.py, is now analogous to process.f90, which contains the 
Fortran "program" statement. This Python module effectively acts as the Fortran
"program".
"""
from process import fortran
import argparse
from pathlib import Path

class Process():
    """The main Process class."""
    def __init__(self):
        """Run Process."""
        self.parse_args()
        self.set_filenames()
        self.run()
    
    def parse_args(self):
        """Parse the command-line arguments, such as the input filename."""
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

        self.args = parser.parse_args()
        # Store namespace object of the args

    def set_filenames(self):
        """Validate the input filename and create other filenames from it."""
        self.set_input()
        self.set_output()

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
        # TODO Should this be the path, rather than the name?
        fortran.global_variables.fileprefix = self.input_path.name

    def set_output(self):
        """Set the output file name.
        
        Set Path object on the Process object, and set the prefix in the Fortran.
        """
        self.output_path = Path(self.filename_prefix + "OUT.DAT")
        fortran.global_variables.output_prefix = self.filename_prefix

    def run(self):
        """Run Process using the highest-level module, process_module."""
        fortran.process_module.process_subroutine()

def main():
    """Run Process."""
    Process()