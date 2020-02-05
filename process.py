#!/usr/bin/env python
"""Script to build and run PROCESS simply.

This compiles, runs and displays output from PROCESS. The aim is to provide a 
common entry point to the code in Python, and to automate a standard workflow.
"""
import subprocess
import os
import sys
import argparse
import utilities.process_io_lib.input_validator as input_validator

PROCESS_EXE_PATH = "./bin/process.exe"

def parse_args():
    """Parse the command line arguments
    
    :return: Namespace object of the args
    :rtype: object
    """
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

    args = parser.parse_args()
    return args

def check_root_dir():
    """Check the working dir is the Process root dir

    Look for the .git dir. This is to make sure the subsequent terminal commands
    work, as they must be run from PROCESS's root dir.
    """
    if os.path.isdir(".git"):
        # Store the path for the root directory
        root_dir = os.getcwd()
    else:
        sys.exit("Error: not in Process root dir. Please run this script from "
            "the root directory of Process.")

def build_process():
    """Build Process"""
    subprocess.run(["cmake", "-H.", "-Bbuild"])
    subprocess.run(["cmake", "--build", "build"])

def validate_input(input_file_path):
    """Validate the input file using the input_validator module.
    
    :param input_file_path: Path to the input file from the project root dir
    :type input_file_path: str
    """
    # Check the input file exists, then run the input validator
    if os.path.isfile(input_file_path):
        input_validator.validate(input_file_path)
    else:
        sys.exit("Input file not found; check the path.")

def run_process(input_file_path):
    """Run Process using a given input file path.
    
    :param input_file_path: Path to the input file from the project root dir
    :type input_file_path: str
    """
    subprocess.run([PROCESS_EXE_PATH, input_file_path])

def create_dicts():
    """Create Python dictionaries"""
    subprocess.run(["cmake", "--build", "build", "--target", "dicts"])

def run_utils(util):
    """Run a utility
    
    :param util: The name of the utility to run
    :type util: str
    """
    # Todo: allow multiple utils to run
    # Todo: allow options to be passed to utils
    subprocess.run(["python", "./utilities/" + util + ".py"])

def main():
    """Run Process workflow based on command line arguments"""
    args = parse_args()
    check_root_dir()
    if args.build:
        build_process()
    
    validate_input(args.input)
    run_process(args.input)
    if args.util:
        # Only create dicts if necessary for utilities
        create_dicts()
        run_utils(args.util)

main()
