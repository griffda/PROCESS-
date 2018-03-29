#!/usr/bin/env python
"""
  Radial plot comparison tool using PLASMOD-type input

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

def plotProfile(i,p, count):
    print('size of RHO[0] = '+ repr(len(RHO[0])))
    print('size of DATA_PROFILES[0][i] = '+ repr(len(DATA_PROFILES[0][i])))
    print(DATA_PROFILES[0][i])

    #for i in range(0,len(ASSIGN_PAGE)):
    #    if(ASSIGN_PAGE[i]==p):
            if(count==0): figure=221
            if(count==1): figure=222
            if(count==2): figure=223
            if(count==3): figure=224
            page[p].add_subplot(figure)
    
            plt.scatter(RHO[0], DATA_PROFILES[0][i])
            plt.scatter(RHO[1], DATA_PROFILES[1][i])

            plt.xlabel('r/a')
            plt.ylabel(AXIS_TITLES[i])
            plt.title(PLOT_TITLES[i])
            count=count+1
    #plt.plot(RHO[0],DATA_PROFILES[0][i], label="File0")
    #plt.plot(RHO[1],DATA_PROFILES[1][i], label="File1")
    #plt.legend()

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
    
    ARGS = PARSER.parse_args()

    LIST_PROFILES = ['DEN', 'TE', 'TI', 'DEUT', 'TRIT', 'JBS', 'JCD', 'JTOT', 'IPOL', 'Q', 'VOL', 'DVOL', 'COND']
    list_len = len(LIST_PROFILES)
    print('number of profiles to be plotted = ' + repr(list_len))
    NUM_PAGES = 4
    ASSIGN_PAGE = [1, 2, 2, 1, 1, 3, 3, 3, 3, 1, 4, 4, 2] #Which page for each plot (4 plots per page)

    PLOT_TITLES = ['Electron density']
    PLOT_TITLES.append('Electron temperature')
    PLOT_TITLES.append('Ion temperature')
    PLOT_TITLES.append('Deuterium density')
    PLOT_TITLES.append('Tritium density')
    PLOT_TITLES.append('Bootstrap current density')
    PLOT_TITLES.append('Current drive current density')
    PLOT_TITLES.append('Total current density')
    PLOT_TITLES.append('Poloidal current')
    PLOT_TITLES.append('Safety factor')
    PLOT_TITLES.append('Plasma volume')
    PLOT_TITLES.append('dVolume/dr')
    PLOT_TITLES.append('Plasma conductivity')

    AXIS_TITLES = ['ne / 1e19 m-3']
    AXIS_TITLES.append('te / keV')
    AXIS_TITLES.append('ti / keV')
    AXIS_TITLES.append('deut / 1e19 m-3')
    AXIS_TITLES.append('trit / 1e19 m-3')
    AXIS_TITLES.append('J_bs / MA/m^2')
    AXIS_TITLES.append('J_cd / MA/m^2')
    AXIS_TITLES.append('J_tot / MA/m^2')
    AXIS_TITLES.append('I_pol / MA')
    AXIS_TITLES.append('q')
    AXIS_TITLES.append('Vol / m^3')
    AXIS_TITLES.append('dVol/dr / m^2')
    AXIS_TITLES.append('Cond / MA/(V.m)')

    PAGE_TITLES = ['Radial profiles (page 1) - densities']
    PAGE_TITLES.append('Radial profiles (page 2)')
    PAGE_TITLES.append('Radial profiles (page 3) - currents')
    PAGE_TITLES.append('Radial profiles (page 4)')
    
    fileArray = [ARGS.filename0, ARGS.filename1]
    n = len(fileArray)
    print('number of files is: ' + repr(n))
    DATA = [None] * n
    RHO = [None] * n

    DATA_PROFILES = [LIST_PROFILES] * n
    
    #create vectors to store the data
    useProfile = [None] * list_len
    for i in range(0,list_len):
        useProfile[i] = True

    for f in range(0,n):  #loop over files
        print('filename' + repr(f) + ' = ' + repr(fileArray[f]))
        try:
            DATA[f] = loadtxt(fileArray[f], unpack=True)
            print('loaded data!')
            print('data size - ' + repr(DATA[f].size))
        except OSError:
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
            #print(RHO[f])
        except IndexError:
            print('Error: The column for the normalised radius does not exist!\
            Remember to start counting at 0!')
            exit()

        for i in range(1,list_len):
            try:
                #print('f = '+repr(f)+' i = '+repr(i))
                DATA_PROFILES[f][i] = DATA[f][i]
                #print(DATA_PROFILES[f][i])
                #print('Inputted ' + repr(LIST_PROFILES[i-1])) #prints entire array
            except IndexError:
                print('Warning: The column for the '+repr(LIST_PROFILES[i-1])+' in file '+repr(f)+' does not exist!\
                Remember to start counting at 0!')
                useProfile[i] = False
        print(useProfile)
        #print(RHO[0])
        #print(DATA_PROFILES[0][1])
        #print(RHO[1])
        #print(DATA_PROFILES[1][1])

    ###################
    #count1 = 0
    page = [None] * NUM_PAGES
    for p in range(0,NUM_PAGES):
        page[p] = plt.figure(figsize=(12, 9), dpi=80)
        plt.suptitle(PAGE_TITLES[p])
            
        for i in range(2,3):   #list_len
            plotProfile(i, p)
                #if(count1==1):
                #     page[p].add_subplot(222)
                #     plotProfile(i, p)

    #plot_den = page1.add_subplot(221)

    print(RHO[0])
    print(DATA_PROFILES[0][1])
    print(RHO[1])
    print(DATA_PROFILES[1][1])
    
    #page1.savefig('../KatyTest/profile_plots.jpg')

    with bpdf.PdfPages("../KatyTest/radial_profiles.pdf") as pdf:
        pdf.savefig(page[1])
   #     pdf.savefig(page[2])
   #     pdf.savefig(page[3])
   #     pdf.savefig(page[4])
    plt.show(page[1])
   # plt.show(page[2])
   # plt.show(page[3])
   # plt.show(page[4])


       
