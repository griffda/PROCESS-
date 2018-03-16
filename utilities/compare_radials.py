#!/usr/bin/env python
"""
  Radial plot comparison tool using PLASMOD-type input
  Based on radial profiling code from plot_proc.py

  Katy Ellis
  01/03/2018
  CCFE

"""

#######################
#imported libraries

import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as bpdf

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

    PARSER.add_argument("-f", "--filename0",
                        default='profile.txt',
                        help="ascii file containing data in columns,\
                        default = profile.txt")

    PARSER.add_argument("-f1", "--filename1",
                        default='profile.txt',
                        help="ascii file containing data in columns,\
                        default = profile.txt")
    
    #PARSER.add_argument("files", metavar='f', type=str,
    #                    default='all', nargs='*',
    #                    help="list of files to be plotted; \
    #                    default = all")
    
    ARGS = PARSER.parse_args()
    
    USEDENSITY = True
    USETE = True

    fileArray = [ARGS.filename0, ARGS.filename1]
    n = len(fileArray)
    print('number of files is: ' + repr(n))
    DATA = [None] * n
    RHO = [None] * n
    DEN = [None] * n
    TE = [None] * n
    
    for f in range(0,n):
        #print('{0:d}'.format(f))
        print('filename' + repr(f) + ' = ' + repr(fileArray[f]))
        try:
            DATA[f] = loadtxt(fileArray[f], unpack=True)
            print('loaded data!')
            print('data size - ' + repr(DATA[f].size))
        except FileNotFoundError:
            print('Error: There is no file called', fileArray[f])
            exit()
        except ValueError:
            print('Error: In', fileArray[f], 'all comment rows must\
            start with a #! Columns can only contain floats!')
            exit()
        if DATA[f].size == 0:
            print('Error: There is no data in', fileArray[f])
            exit()
            
        try:
            RHO[f] = DATA[f][0]
            print('Inputted RHO')
            #print(RHO)
        except IndexError:
            print('Error: The column for the normalised radius does not exist!\
            Remember to start counting at 0!')
            exit()
            
        try:
            DEN[f] = DATA[f][1]
            print('Inputted DEN')
            #print(DEN)
        except IndexError:
            print('Warning: The column for the density does not exist!\
            Remember to start counting at 0!')
            USEDENSITY = False

        try:
            TE[f] = DATA[f][2]
            print('Inputted TE (electron temp)')
            #print(TE)
        except IndexError:
            print('Warning: The column for the density does not exist!\
            Remember to start counting at 0!')
            USETE = False
                
    fig = plt.figure()
    plot_den = fig.add_subplot(121)
    plt.scatter(RHO, DEN)

    plt.xlabel('r/a')
    plt.ylabel('ne / 1e19 m-3')
    plt.suptitle('Density profile')
    
    plt.plot(RHO[0],DEN[0], label="File0")
    plt.plot(RHO[1],DEN[1], label="File1")
    plt.legend()
    
    #plt.show()

    plot_te = fig.add_subplot(122)
    plt.scatter(RHO, TE)

    plt.xlabel('r/a')
    plt.ylabel('te / keV')
    plt.suptitle('Density profile')
    
    plt.plot(RHO[0],TE[0], label="File0")
    plt.plot(RHO[1],TE[1], label="File1")
    plt.legend()
    
    #plt.show()
    
    fig.savefig('../KatyTest/profile_plots.jpg')

    page1 = plt.figure(figsize=(12, 9), dpi=80)
    with bpdf.PdfPages("../KatyTest/radial_profiles.pdf") as pdf:
        pdf.savefig(page1)
