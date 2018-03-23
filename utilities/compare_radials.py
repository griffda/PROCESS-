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
    
    USEDEN = True
    USETE = True
    USETI = True
    USEDEUT = True
    USETRIT = True
    USEJBS = True
    USEJCD = True
    USEJTOT = True
    USEIPOL = True
    USEQ = True
    USEVOL = True
    USEDVOL = True
    USECOND = True
    #USEPALPH = True
    #USEDENI = True
    #USEFPOL = True
    
    fileArray = [ARGS.filename0, ARGS.filename1]
    n = len(fileArray)
    print('number of files is: ' + repr(n))
    DATA = [None] * n
    RHO = [None] * n
    
    DEN = [None] * n
    TE = [None] * n
    TI = [None] * n
    DEUT = [None] * n
    TRIT = [None] * n
    JBS = [None] * n
    JCD = [None] * n
    JTOT = [None] * n
    IPOL = [None] * n
    Q = [None] * n
    VOL = [None] * n
    DVOL = [None] * n
    COND = [None] * n
    #PALPH = [None] * n
    #DENI = [None] * n
    #FPOL = [None] * n
    
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
            print(RHO[f])
        except IndexError:
            print('Error: The column for the normalised radius does not exist!\
            Remember to start counting at 0!')
            exit()
            
        try:
            DEN[f] = DATA[f][1]
            print('Inputted DEN')
        except IndexError:
            print('Warning: The column for the density does not exist!\
            Remember to start counting at 0!')
            USEDEN = False
        try:
            TE[f] = DATA[f][2]
            print('Inputted TE (electron temp)')
        except IndexError:
            print('Warning: The column for the electron temp does not exist!\
            Remember to start counting at 0!')
            USETE = False
        try:
            TI[f] = DATA[f][3]
            print('Inputted TI (ion temp)')
        except IndexError:
            print('Warning: The column for the ion temp does not exist!\
            Remember to start counting at 0!')
            USETI = False
        #try:
        #    DEUT[f] = DATA[f][4]
        #    print('Inputted DEUT (Deuterium density)')
        #except IndexError:
        #    print('Warning: The column for the deuterium density does not exist!\
        #    Remember to start counting at 0!')
        #    USEDEUT = False
        #try:
        #    TRIT[f] = DATA[f][5]
        #    print('Inputted TRIT (Tritium density)')
        #except IndexError:
        #    print('Warning: The column for the tritium density does not exist!\
        #    Remember to start counting at 0!')
        #    USETRIT = False    
        try:
            JBS[f] = DATA[f][4]
            print('Inputted JBS (bootstrap current)')
        except IndexError:
            print('Warning: The column for the bootstrap current does not exist!\
            Remember to start counting at 0!')
            USEJBS = False
        try:
            JCD[f] = DATA[f][5]
            print('Inputted JCD (CD current density)')
        except IndexError:
            print('Warning: The column for the CD current density does not exist!\
            Remember to start counting at 0!')
            USEJCD = False
        try:
            JTOT[f] = DATA[f][6]
            print('Inputted JTOT (Total current density)')
        except IndexError:
            print('Warning: The column for the total current density does not exist!\
            Remember to start counting at 0!')
            USEJTOT = False
        try:
            IPOL[f] = DATA[f][7]
            print('Inputted IPOL (Poloidal current)')
        except IndexError:
            print('Warning: The column for the poloidal current does not exist!\
            Remember to start counting at 0!')
            USEIPOL = False   
        try:
            Q[f] = DATA[f][8]
            print('Inputted Q (safety factor)')
        except IndexError:
            print('Warning: The column for q does not exist!\
            Remember to start counting at 0!')
            USEQ = False
        try:
            VOL[f] = DATA[f][9]
            print('Inputted VOL (Plasma volume)')
        except IndexError:
            print('Warning: The column for VOL does not exist!\
            Remember to start counting at 0!')
            USEVOL = False
        try:
            DVOL[f] = DATA[f][10]
            print('Inputted DVOL (d(volume)/dr)')
        except IndexError:
            print('Warning: The column for DVOL does not exist!\
            Remember to start counting at 0!')
            USEDVOL = False
        try:
            COND[f] = DATA[f][11]
            print('Inputted COND (Plasma conductivity)')
        except IndexError:
            print('Warning: The column for COND does not exist!\
            Remember to start counting at 0!')
            USECOND = False
        #try:
        #    PALPH[f] = DATA[f][12]
        #    print('Inputted PALPH (Alpha pressure)')
        #except IndexError:
        #    print('Warning: The column for PALPH does not exist!\
        #    Remember to start counting at 0!')
        #    USEPALPH = False
        #try:
        #    DENI[f] = DATA[f][13]
        #    print('Inputted DENI (Ion density)')
        #except IndexError:
        #    print('Warning: The column for DENI does not exist!\
        #    Remember to start counting at 0!')
        #    USEDENI = False
        #try:
        #    FPOL[f] = DATA[f][14]
        #    print('Inputted FPOL (Poloidal flux)')
        #except IndexError:
        #    print('Warning: The column for FPOL does not exist!\
        #    Remember to start counting at 0!')
        #    USEFPOL = False

    ###################
    page1 = plt.figure(figsize=(12, 9), dpi=80)
    plt.suptitle('Radial profiles (page 1) - densities')
    
    plot_den = page1.add_subplot(221)
    plt.scatter(RHO[0], DEN[0])
    plt.scatter(RHO[1], DEN[1])

    plt.xlabel('r/a')
    plt.ylabel('ne / 1e19 m-3')
    plt.title('Electron density')
    
    plt.plot(RHO[0],DEN[0], label="File0")
    plt.plot(RHO[1],DEN[1], label="File1")
    plt.legend()

    plot_deut = page1.add_subplot(222)
    #plt.scatter(RHO[0], DEUT[0])
    #plt.scatter(RHO[1], DEUT[1])
    
    plt.xlabel('r/a')
    plt.ylabel('deut / 1e19 m-3')
    plt.title('Deuterium density')
    
    #plt.plot(RHO[0],DEUT[0], label="File0")
    #plt.plot(RHO[1],DEUT[1], label="File1")
    plt.legend()

    plot_trit = page1.add_subplot(223)
    #plt.scatter(RHO[0], TRIT[0])
    #plt.scatter(RHO[1], TRIT[1])
    
    plt.xlabel('r/a')
    plt.ylabel('trit / 1e19 m-3')
    plt.title('Tritium density')
    
    #plt.plot(RHO[0],TRIT[0], label="File0")
    #plt.plot(RHO[1],TRIT[1], label="File1")
    plt.legend()

    #################
    page2 = plt.figure(figsize=(12, 9), dpi=80)
    plt.suptitle('Radial profiles (page 2) - temperatures')
    
    plot_te = page2.add_subplot(221)
    plt.scatter(RHO[0], TE[0])
    plt.scatter(RHO[1], TE[1])

    plt.xlabel('r/a')
    plt.ylabel('te / keV')
    plt.title('Electron temperature')
    
    plt.plot(RHO[0],TE[0], label="File0")
    plt.plot(RHO[1],TE[1], label="File1")
    plt.legend()

    plot_ti = page2.add_subplot(222)
    plt.scatter(RHO[0], TI[0])
    plt.scatter(RHO[1], TI[1])
    
    plt.xlabel('r/a')
    plt.ylabel('ti / keV')
    plt.title('Ion temperature')
    
    plt.plot(RHO[0],TI[0], label="File0")
    plt.plot(RHO[1],TI[1], label="File1")
    plt.legend()

    ###################
    page3 = plt.figure(figsize=(12, 9), dpi=80)
    plt.suptitle('Radial profiles (page 3) - currents')

    plot_jbs = page3.add_subplot(221)
    plt.scatter(RHO[0], JBS[0])
    plt.scatter(RHO[1], JBS[1])
    
    plt.xlabel('r/a')
    plt.ylabel('J_bs / MA/m^2')
    plt.title('Boostrap current density')
    
    plt.plot(RHO[0],JBS[0], label="File0")
    plt.plot(RHO[1],JBS[1], label="File1")
    plt.legend()

    plot_jcd = page3.add_subplot(222)
    plt.scatter(RHO[0], JCD[0])
    plt.scatter(RHO[1], JCD[1])
    
    plt.xlabel('r/a')
    plt.ylabel('J_cd / MA/m^2')
    plt.title('Current drive current density')
    
    plt.plot(RHO[0],JCD[0], label="File0")
    plt.plot(RHO[1],JCD[1], label="File1")
    plt.legend()

    plot_jtot = page3.add_subplot(223)
    plt.scatter(RHO[0], JTOT[0])
    plt.scatter(RHO[1], JTOT[1])
    
    plt.xlabel('r/a')
    plt.ylabel('J_tot / MA/m^2')
    plt.title('Total current density')
    
    plt.plot(RHO[0],JTOT[0], label="File0")
    plt.plot(RHO[1],JTOT[1], label="File1")
    plt.legend()

    # not a key quantity - Michael
    plot_ipol = page3.add_subplot(224)
    plt.scatter(RHO[0], IPOL[0])
    plt.scatter(RHO[1], IPOL[1])
    
    plt.xlabel('r/a')
    plt.ylabel('I_pol')
    plt.title('Poloidal current')
    
    plt.plot(RHO[0],IPOL[0], label="File0")
    plt.plot(RHO[1],IPOL[1], label="File1")
    plt.legend()

    ################
    page4 = plt.figure(figsize=(12, 9), dpi=80)
    plt.suptitle('Radial profiles (page 4)')
    
    plot_q = page4.add_subplot(221)
    plt.scatter(RHO[0], Q[0])
    plt.scatter(RHO[1], Q[1])
    
    plt.xlabel('r/a')
    plt.ylabel('q')
    plt.title('Safety factor')
    
    plt.plot(RHO[0],Q[0], label="File0")
    plt.plot(RHO[1],Q[1], label="File1")
    plt.legend()
    
    plot_vol = page4.add_subplot(222)
    plt.scatter(RHO[0], VOL[0])
    plt.scatter(RHO[1], VOL[1])
    
    plt.xlabel('r/a')
    plt.ylabel('Vol / m^3')
    plt.title('Plasma volume')
    
    plt.plot(RHO[0],VOL[0], label="File0")
    plt.plot(RHO[1],VOL[1], label="File1")
    plt.legend()

    plot_dvol = page4.add_subplot(223)
    plt.scatter(RHO[0], DVOL[0])
    plt.scatter(RHO[1], DVOL[1])
    
    plt.xlabel('r/a')
    plt.ylabel('dVol/dr / m^2')
    plt.title('dVolume/dr')
    
    plt.plot(RHO[0],DVOL[0], label="File0")
    plt.plot(RHO[1],DVOL[1], label="File1")
    plt.legend()

    plot_cond = page4.add_subplot(224)
    plt.scatter(RHO[0], COND[0])
    plt.scatter(RHO[1], COND[1])
    
    plt.xlabel('r/a')
    plt.ylabel('Cond / MA/(V.m)')
    plt.title('Plasma conductivity')
    
    plt.plot(RHO[0],COND[0], label="File0")
    plt.plot(RHO[1],COND[1], label="File1")
    plt.legend()
    
#    plot_palph = page3.add_subplot(224)
#    plt.scatter(RHO[0], PALPH[0])
#    plt.scatter(RHO[1], PALPH[1])
#    
#    plt.xlabel('r/a')
#    plt.ylabel('p_alpha / keV*10^10/m^3')
#    plt.title('Alpha pressure')
#    
#   plt.plot(RHO[0],PALPH[0], label="File0")
#   plt.plot(RHO[1],PALPH[1], label="File1")
#    plt.legend()


    
    page1.savefig('../KatyTest/profile_plots.jpg')

    with bpdf.PdfPages("../KatyTest/radial_profiles.pdf") as pdf:
        pdf.savefig(page1)
        pdf.savefig(page2)
        pdf.savefig(page3)
        pdf.savefig(page4)
    plt.show(page1)
    plt.show(page2)
    plt.show(page3)
    plt.show(page4)
