#!/usr/bin/env python
"""Script to build and run PROCESS simply.

This compiles, runs and displays output from PROCESS. The aim is to provide a 
common entry point to the code in Python, and to automate a standard workflow.
"""
import subprocess
import os
import sys
import argparse
from utilities.process_io_lib import input_validator

PROCESS_EXE_PATH = "./bin/process.exe"

class Process(object):
    """A Process workflow based on command line arguments."""
    def __init__(self):
        """Parse command line arguments and run accordingly."""
        self.parse_args()
        self.check_root_dir()
        if self.args.build:
            self.build_process()
            self.create_dicts()
        
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

if __name__ == "__main__":
    Process()