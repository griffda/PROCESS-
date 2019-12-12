#!/usr/bin/env python
"""Script to build and run PROCESS simply.

This compiles, runs and displays output from PROCESS. The aim is to provide a 
common entry point to the code in Python, and to automate a standard workflow.
"""
import subprocess
import os
import sys

# Check the working directory is the root project directory by looking for the 
# .git dir. This is to make sure the subsequent terminal commands work, as they
# must be run from PROCESS's root dir
if os.path.isdir(".git"):
    # Store the path for the root directory
    root_dir = os.getcwd()
else:
    sys.exit("Error: not in Process root dir. Please run this script from the "
        "root directory of Process.")

# Build Process
subprocess.run(["cmake", "-H.", "-Bbuild"])
subprocess.run(["cmake", "--build", "build"])

# Run Process
subprocess.run(["./process.exe"])

# Create Python dictionaries
subprocess.run(["cmake", "--build", "build", "--target", "dicts"])

# Display summary output in matplotlib and create SUMMARY.pdf
subprocess.run(["python", "./utilities/plot_proc.py", "--show"])
