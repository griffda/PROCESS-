"""
Code to create plots from the output of the Morris method
of elementary elements to investiage the sensistivity of 
the input parameters in PROCESS

Author: A. Pearce (alexander.pearce@ukaea.uk)

Input files:
morris_method_output.txt (datafile output from morris_method.py,
                             in the same directory as this file)

Output files:
In the work directory specified in the config file
morris_output.pdf     -  scatter plot of mean and variance of
                        morris method output

"""

import argparse
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as bpdf
from matplotlib import rc

if __name__ == "__main__":

    # set up command line arguments
    PARSER = argparse.ArgumentParser(description='Program to plot the output of the\
        the sensistivity analysis by elementary element method at a given PROCESS design point.')

    PARSER.add_argument("-f", "--datafile", default="morris_method_output.txt", type=str,
                        help="datafile for plotting, default = morris_method_output.txt")
    PARSER.add_argument("-o", "--outputfile", default="morris_output.pdf", type=str,
                        help="filename of outputed pdf file, default = morris_output.pdf")
    
    ARGS = PARSER.parse_args()

    # setput files
    INPUTFILE = ARGS.datafile
    OUTPUTFILE = ARGS.outputfile
    pdf = bpdf.PdfPages(OUTPUTFILE)
    page = plt.figure(figsize=(12, 9), dpi=80)

    # read in data
    n = np.loadtxt(INPUTFILE,dtype=str,usecols=[0],skiprows=1)
    z = np.loadtxt(INPUTFILE,usecols=[2],skiprows=1)
    y = np.loadtxt(INPUTFILE,usecols=[3],skiprows=1)

    plt.scatter(z, y)

    for i, txt in enumerate(n):
        plt.annotate(txt, (z[i], y[i]),fontsize=16)

    plt.ylabel('$\sigma$', fontsize=22)
    plt.xlabel('$\mu^{*}$', fontsize=22)
    plt.tick_params(labelsize=20)

    pdf.savefig(page)
    plt.clf()
    pdf.close()