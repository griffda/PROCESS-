#!/usr/bin/env python
"""Script to build and run PROCESS simply.

This compiles, runs and displays output from PROCESS. The aim is to provide a 
common entry point to the code in Python, and to automate a standard workflow.
"""
import subprocess
import os
import sys
import argparse
from shutil import copy
from pathlib import Path
from difflib import unified_diff
from utilities.process_io_lib import input_validator

PROCESS_EXE_PATH = "./bin/process.exe"

class Process(object):
    """A Process workflow based on command line arguments."""
    def __init__(self):
        """Parse command line arguments and run accordingly."""
        # File paths
        self.run_dir = None
        self.input = None
        self.ref_input = None
        
        # Run actions
        self.parse_args()
        self.check_root_dir()
        if self.args.build:
            self.build_process()
            self.create_dicts()

        # Find the input file and look for changes to it
        # TODO: input file needs to be mandatory; positional arg
        self.set_input()
        self.set_ref_input()
        self.input_file_diff()

        self.validate_input()
        self.run_process()
        if self.args.util:
            # Only create dicts if necessary for utilities
            self.create_dicts()
            self.run_utils()

    def parse_args(self):
        """Parse the command line arguments."""
        parser = argparse.ArgumentParser(description="Script to automate "
            "running PROCESS")

        # Optional arguments
        parser.add_argument("--input",
            "-i",
            metavar="input_file_path",
            default="IN.DAT",
            help="The path to the input file that Process runs on")
        parser.add_argument("--ref_input",
            "-r",
            metavar="reference_input",
            help="Reference input file to record changes against")
        parser.add_argument("--build",
            "-b",
            action="store_true",
            help="Rebuilds Process if present")
        parser.add_argument("--util",
            "-u",
            metavar="util_name",
            help="Utility to run after Process runs")

        self.args = parser.parse_args()
        # Store namespace object of the args

    def check_root_dir(self):
        """Check the working dir is the Process root dir

        Look for the .git dir. This is to make sure the subsequent terminal commands
        work, as they must be run from PROCESS's root dir.
        """
        if os.path.isdir(".git"):
            # Store the path for the root directory
            self.root_dir = os.getcwd()
        else:
            sys.exit("Error: not in Process root dir. Please run this script from "
                "the root directory of Process.")

    def build_process(self):
        """Build Process"""
        subprocess.run(["cmake", "-H.", "-Bbuild"])
        subprocess.run(["cmake", "--build", "build"])

    def set_input(self):
        """Validate the input file path, then store it.
        
        Also set the run directory according to the input file path.
        """
        input_path = Path(self.args.input)
        if input_path.exists():
            self.input = input_path
            # Set input as Path object
            self.run_dir = Path(input_path.parent)
            # Set run directory as dir that contains the input file
        else:
            raise FileNotFoundError("Input file not found on this path.")

    def set_ref_input(self):
        """Find an input file to use as a reference for changes.
        
        Find or create a reference input file to compare with the current 
        input file, to allow the changes to be tracked.
        """
        # self.args.ref_input: reference input file path; can already be set as 
        # command line arg
        if self.args.ref_input:
            # Ref input specified as command line arg. Check it exists
            path = Path(self.args.ref_input)
            if path.exists():
                # Store path object
                self.ref_input = path
            else:
                raise FileNotFoundError("Reference input file not found; "
                    "check the path.")
            
            # Input file exists
            if "REF_IN.DAT" in self.ref_input.name:
                # Ref file specified; got a ref file
                pass
            elif "IN.DAT" in self.ref_input.name:
                # Regular input file specified. Make a reference copy as 
                # REF_IN.DAT and update self.ref_input
                self.create_ref_input()
            else:
                # File isn't an IN.DAT or a REF_IN.DAT
                raise ValueError("Reference input file should end in IN.DAT or "
                    "REF_IN.DAT")
        else:
            # Ref input not specified: try to find one in the input file's dir
            ref_inputs = list(self.run_dir.glob("*REF_IN.DAT"))
            if len(ref_inputs) == 0:
                # No ref input files: create one from input file
                self.create_ref_input()
            elif len(ref_inputs) == 1:
                # One pre-existing ref input file: set as ref input
                self.ref_input = ref_inputs[0]
            else:
                # More than one ref file: error
                raise Exception("More than one REF_IN.DAT found in this dir. "
                    "Specifiy which one to use with the \"-r\" option, or "
                    "remove the other ones.")

        # Now either have raised an exception or have a ref input file set,
        # with suffix "REF_IN.DAT"

    def create_ref_input(self):
        """Create a reference input file from a pre-existing input file."""
        # First, ensure that self.ref_input is set as an IN.DAT to copy
        if not self.ref_input:
            # No IN.DAT specified to use as ref: use input file instead
            self.ref_input = self.input

        # Copy the ref input and save it with the REF_IN.DAT extension
        # Then set this as the new ref input
        new_ref_input_name = self.ref_input.name.replace("IN.DAT", "REF_IN.DAT")
        new_ref_input = Path(self.run_dir).joinpath(new_ref_input_name)
        copy(self.ref_input, new_ref_input)
        self.ref_input = new_ref_input

    def input_file_diff(self):
        """Perform a diff between the reference input and input files.
        
        This compares the REF_IN.DAT and IN.DAT files to highlight the 
        modifications.
        """
        # Run a diff on the files
        before_file = open(self.ref_input)
        after_file = open(self.input)
        before = before_file.readlines()
        after = after_file.readlines()
        before_file.close()
        after_file.close()
        diff = unified_diff(before, after, fromfile=self.ref_input.name, 
            tofile=self.input.name)

        # Write diff to output file
        diff_path = self.run_dir.joinpath("input.diff")
        with open(diff_path, "w") as diff_file:
            diff_file.writelines(diff)

    def create_dicts(self):
        """Create Python dictionaries"""
        subprocess.run(["cmake", "--build", "build", "--target", "dicts"])

    def validate_input(self):
        """Validate the input file using the input_validator module."""
        # Check the input file exists, then run the input validator
        # self.args.input: path to the input file from the project root dir
        if os.path.isfile(self.args.input):
            input_validator.validate(self.args.input)
        else:
            sys.exit("Input file not found; check the path.")

    def run_process(self):
        """Run Process using a given input file path."""
        subprocess.run([PROCESS_EXE_PATH, self.args.input])
        # self.args.input: Path to the input file from the project root dir

    def run_utils(self):
        """Run a utility if specified on the command line."""
        # Todo: allow multiple utils to run
        # Todo: allow options to be passed to utils
        subprocess.run(["python", "./utilities/" + self.args.util + ".py"])

def main():
    """Run Process."""
    print("Running process.py")
    Process()

if __name__ == "__main__":
    main()