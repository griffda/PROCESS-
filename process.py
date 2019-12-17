#!/usr/bin/env python
"""Script to build and run PROCESS simply.

This compiles, runs and displays output from PROCESS. The aim is to provide a 
common entry point to the code in Python, and to automate a standard workflow.
"""
import subprocess
import os
import sys
import argparse

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
        metavar="input_filename",
        default="IN.DAT",
        help="The name of the input file that Process runs on")
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

def run_process(input_filename):
    """Run Process using a given input file
    
    :param input_filename: Filename of the input file
    :type input_filename: str
    """
    # Todo: support file locations that aren't in the project root dir
    subprocess.run(["./process.exe", input_filename])

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
    run_process(args.input)
    create_dicts()
    if args.util:
        run_utils(args.util)

main()
