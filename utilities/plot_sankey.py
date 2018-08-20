#!/usr/bin/env python
"""
Code to display the power flow of a PROCESS run in a Sankey diagram

Author: H. Lux (Hanni.Lux@ukaea.uk)

Input file:
MFILE.DAT

"""

import argparse
from pylab import show, savefig
from process_io_lib.sankey_funcs import plot_sankey, plot_simplified_sankey



if __name__ == '__main__':

###########################################################
#Usage

    PARSER = argparse.ArgumentParser(description='Program to plot\
     the power flow in PROCESS using a Sankey diagram.')

    PARSER.add_argument("-e", "--end",
                        default='pdf',
                        help="file format default = pdf")

    PARSER.add_argument("-f", "--mfile",
                        default='MFILE.DAT',
                        help="mfile, default = MFILE.DAT")

    PARSER.add_argument("-s", "--simplified", action="store_true",
                    help="Only plot a simplified version")

    ARGS = PARSER.parse_args()

    #########################################################
    #main program

    if ARGS.simplified:
        plot_simplified_sankey(ARGS.mfile)
        savefig("SankeyPowerFlow_simplified."+ARGS.end)
    else:
        plot_sankey(ARGS.mfile)
        savefig("SankeyPowerFlow."+ARGS.end)
    
    show()

