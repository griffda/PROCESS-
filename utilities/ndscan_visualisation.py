#!/usr/bin/env python
"""
Code to visualise multi-dimensional parameter scans.

Author: S. Torrisi (storrisi@u.rochester.edu)

Date: August 2014

Input files:
Demonstrationdata.nc (netcdf file, per default in working directory)

Outputfiles:
??

Compatible with PROCESS version 382

"""


from process_io_lib.ndscan_vis import VisUtility
import argparse


if __name__ == '__main__':
############################################################
#Usage

    PARSER = argparse.ArgumentParser(description='Program to visualise a multi-\
    dimensional parameter scan.')

    PARSER.add_argument("-f", "--netcdffile", default='Demonstrationdata.nc',
                        help="netcdf file, default = Demonstrationdata.nc")

    ARGS = PARSER.parse_args()

    ############################################################
    #main program
    VISUTIL = VisUtility(ARGS.netcdffile)
    VISUTIL.main()
