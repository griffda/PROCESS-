#!/usr/bin/env python
"""
  Radial plot comparison tool using PLASMOD-type input
  Based on radial profiling code from plot_proc.py

  Katy Ellis
  01/03/2018
  CCFE

"""

import matplotlib.pyplot as plt

#######################
#imported libraries

import argparse
from numpy import loadtxt, array, argsort, arange, argmin, linspace
from scipy.optimize import leastsq
from process_io_lib.profile_funcs import nresidual, tresidual, nprofile, \
    tprofile
from pylab import figure, xlabel, ylabel, axvline, axhline, plot, show

if __name__ == '__main__':
############################################################
#Usage

    PARSER = argparse.ArgumentParser(description='Program to fit a\
    general temperature or density profile from an ascii table.')

    PARSER.add_argument("-f", "--filename",
                        default='profile.txt',
                        help="ascii file containing data in columns,\
                        default = profile.txt")
    
    ARGS = PARSER.parse_args()
    
    USEDENSITY = True
    USETEMPERATURE = True
    
    try:
        DATA = loadtxt(ARGS.filename, unpack=True)
        print('loaded data!')
    except FileNotFoundError:
        print('Error: There is no file called', ARGS.filename)
        exit()
    except ValueError:
        print('Error: In', ARGS.filename, 'all comment rows must\
        start with a #! Columns can only contain floats!')
        exit()
        if DATA.size == 0:
            print('Error: There is no data in', ARGS.filename)
            exit()
            
    try:
        RHO = DATA[0]
        print('Inputted RHO')
        print(RHO)
    except IndexError:
        print('Error: The column for the normalised radius does not exist!\
        Remember to start counting at 0!')
        exit()

    try:
        DEN = DATA[1]
        print('Inputted DEN')
        print(DEN)
    except IndexError:
        print('Warning: The column for the density does not exist!\
        Remember to start counting at 0!')
        USEDENSITY = False
        
    fig = plt.figure()
    fig.add_subplot(111)
    plt.scatter(RHO, DEN)
    plt.show()
            
