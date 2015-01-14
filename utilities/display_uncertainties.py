#!/usr/bin/env python
"""
Code to display the results of an evaluate_uncertainties.py run

Author: H. Lux (Hanni.Lux@ccfe.ac.uk)

Input file:
UNCERTAINTIES.DAT


"""


#######################
#imported libraries

import argparse
from numpy import loadtxt
from matplotlib.ticker import NullFormatter
from pylab import figure, axes, show, savefig
from process_io_lib.process_config import NETCDF_SWITCH


def fig_2dscatter_and_hist(xarr, yarr, labelx, labely):

    """ function to create a 2d scatter plot with histograms"""
    nullfmt = NullFormatter()

    figsize = (8, 8)
    left, width = 0.14, 0.61
    bottom, height = 0.14, 0.61
    bottom_h = left_h = left+width

    # start with a rectangular Figure
    figure(figsize=figsize)

    axscatter = axes([left, bottom, width, height])
    axhistx = axes([left, bottom_h, width, 0.2])
    axhisty = axes([left_h, bottom, 0.2, height])

    # no labels
    axhistx.xaxis.set_major_formatter(nullfmt)
    axhistx.yaxis.set_major_formatter(nullfmt)
    axhisty.xaxis.set_major_formatter(nullfmt)
    axhisty.yaxis.set_major_formatter(nullfmt)

    # the scatter plot:
    axscatter.scatter(xarr, yarr, edgecolors='None')
    axscatter.set_xlabel(labelx)
    axscatter.set_ylabel(labely)

    bins = 10
    axhistx.hist(xarr, bins=bins)
    axhisty.hist(yarr, bins=bins, orientation='horizontal')


if __name__ == '__main__':

###########################################################
#Usage

    PARSER = argparse.ArgumentParser(description='Program to display\
     uncertainties in a given PROCESS design point.')


    PARSER.add_argument("-e", "--end",
                        default='.pdf',
                        help="file format default =\
.pdf")


    PARSER.add_argument("variables", metavar='v', type=str,
                        default='all', nargs='*',
                        help="list of variables to be plotted; \
default = all")

    PARSER.add_argument("-f", "--filename",
                        default='uncertainties.nc',
                        help="uncertainties data file, default =\
uncertainties.nc")
    ARGS = PARSER.parse_args()

    FILENAME = ARGS.filename


############################################################
#main program

    #version with NetCDF files:
    if NETCDF_SWITCH:
        from process_io_lib.process_netcdf import NetCDFReader


        if ARGS.variables == 'all':
            with NetCDFReader(FILENAME) as ncdf_reader:
                MFILE = ncdf_reader.get_run(0)
                if len(MFILE.data) > 6:
                    print('There are too many variables stored.\
 Choose 2 for plotting!')
                    exit()

                DATA = {}
                LABELS = list(MFILE.data.keys())
                for i in range(len(MFILE.data)):
                    DATA[str(i)] = []

                for MFILE in ncdf_reader.runs(start_run=0):
                    for i in range(len(LABELS)):
                        DATA[str(i)] += [MFILE.data[LABELS[i]].get_scan(-1)]


            for i in range(len(LABELS)-1):
                XARR = DATA[str(i)]
                YARR = DATA[str(i+1)]
                fig_2dscatter_and_hist(XARR, YARR, LABELS[i], LABELS[i+1])
                savefig('Uncertainties_'+LABELS[i]+'_'+LABELS[i+1]+ARGS.end)

        else:
            XARR = []
            YARR = []

            with NetCDFReader(FILENAME) as ncdf_reader:

                # Get multiple runs
                for MFILE in ncdf_reader.runs(start_run=0):

                    XARR += [MFILE.data[ARGS.variables[0]].get_scan(-1)]
                    YARR += [MFILE.data[ARGS.variables[1]].get_scan(-1)]
            fig_2dscatter_and_hist(XARR, YARR, ARGS.variables[0],
                                   ARGS.variables[1])
            savefig('Uncertainties_'+ARGS.variables[0]+'_'+ARGS.variables[1]\
                    +ARGS.end)


    #version without NetCDF files:
    else:
        UFILE = open(FILENAME, 'r')
        LABELS = UFILE.readline()
        UFILE.close()
        LABELS = LABELS.split()

        DATA = loadtxt(FILENAME, skiprows=1)

        if ARGS.variables == 'all':
            for i in range(len(DATA[0])-1):
                XARR = DATA[:, i]
                YARR = DATA[:, i+1]

                fig_2dscatter_and_hist(XARR, YARR, LABELS[i], LABELS[i+1])
                savefig('Uncertainties_'+LABELS[i]+'_'+LABELS[i+1]+ARGS.end)
        else:
            print('Sorry, this option is not actually programmed yet!')
            exit()

show()
