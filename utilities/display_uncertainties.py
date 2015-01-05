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


def fig_2dscatter_and_hist(x,y):

    """ function to create a 2d scatter plot with histograms"""
    nullfmt   = NullFormatter()
 
    figsize=(8,8)
    left, width = 0.14, 0.61
    bottom, height = 0.14, 0.61
    bottom_h = left_h = left+width

    # start with a rectangular Figure
    figure(figsize=figsize)

    axScatter = axes([left, bottom, width, height])
    axHistx = axes([left, bottom_h, width, 0.2])
    axHisty = axes( [left_h, bottom, 0.2, height])

    # no labels
    axHistx.xaxis.set_major_formatter(nullfmt)
    axHistx.yaxis.set_major_formatter(nullfmt)
    axHisty.xaxis.set_major_formatter(nullfmt)
    axHisty.yaxis.set_major_formatter(nullfmt)

    # the scatter plot:
    axScatter.scatter(x, y,edgecolors='None')
    axScatter.set_xlabel(labels[i])
    axScatter.set_ylabel(labels[i+1])

    bins = 10
    axHistx.hist(x, bins=bins)
    axHisty.hist(y, bins=bins, orientation='horizontal')








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

    filename = ARGS.filename


############################################################
#main program

    #version with NetCDF files:
    if NETCDF_SWITCH:
        from process_io_lib.process_netcdf import NetCDFReader

        ncdf_reader = NetCDFReader(filename)
        mfile1 = ncdf_reader.get_mfile(0)
        print('Yes got so far, not finished yet!')
        #TODO! continue here!

    #version without NetCDF files:
    else:
        ufile = open(filename,'r')
        labels = ufile.readline()
        ufile.close()
        labels = labels.split()

        data = loadtxt(filename, skiprows=1)

        if ARGS.variables == 'all':
            for i in range(len(data[0])-1):
                x = data[:,i]
                y = data[:,i+1]

                fig_2dscatter_and_hist(x,y)
                savefig('Uncertainties_'+labels[i]+'_'+labels[i+1]+ARGS.end)
        else: 
            pass#TODO program this option

    
show()
