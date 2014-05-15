#! /usr/bin/env python3

"""
   PJK 15/05/2014: Updated to python3 using 2to3 tool

   Compatible with PROCESS version 274
"""

import sys
from pylab import *
import getopt

def usage():
    print('')
    print('Usage:')
    print(' plot_sweep [-g, -h, -o outfile] params')
    print('')
    print(' Plots normalised PROCESS sweep data.')
    print(' Must be run in the folder containing PLOT.DAT.')
    print('  -g, --graphics         plots to a file [default: plot to screen]')
    print('  -h, --help             prints this message and exits')
    print('  -o, --output outfile   specifies output filename [default=PLOT.DAT.eps]')
    print('  params                 set of outputs to plot (from position in PLOT.DAT)')
    print('')
    print('  By default the -g option prints to PLOT.DAT.eps, but providing an output filename')
    print('  using -o outfile.png, for example, will create a file of the appropriate type.')
    print('  If using -o, -g is unnecessary.')
    print('')
    print('Examples:')
    print(' plot_sweep -g 11 13 18        : reads PLOT.DAT, creates PLOT.DAT.eps with R0, IP, beta')
    print(' plot_sweep -o demo1.png 21 22 : reads PLOT.DAT, creates PNG file demo1.png with Te, ne')
    print('')
    print(' Contact richard.kemp@ccfe.ac.uk with comments or complaints.')
    print('')

# Main program below here
def main():
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "gho:", ["graphics", "help", "output="])
    except getopt.GetoptError as err:
        # print help information and exit:
        print(str(err)) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    infile = 'PLOT.DAT'
    outfile = infile + '.eps'
    isps = False
    for o, a in opts:
        if o in ("-g", "--graphics"):
            isps = True
        elif o in ("-h", "--help"):
            usage()
            sys.exit()
        elif o in ("-o", "--output"):
            outfile = a
            isps = True
        else:
            assert False, "unhandled option"
    if args == []:
        print('Please enter some arguments.')
        usage()
        sys.exit()
    try:
        with open(infile) as f: pass
    except IOError as e:
        print('File '+infile+' not found: stopping.')
        sys.exit(2)

    if isps:
        try:
            with open(outfile) as f:
                print('File '+outfile+' already exists.')
                owrite = input('Overwrite (y/n)?\n')
                if owrite in ("y", "Y", "yes", "YES"): pass
                else: sys.exit()
        except IOError as e: pass

    f = open(infile, 'r')
    data = f.readlines()
    f.close()
    if data == []:
        print('No sweep in PLOT.DAT.')
        sys.exit()

    nsweep = int(data[0])
    plots = []
    for i in args: plots.append(int(i))
    names = []
    datas = []
    for i in plots:
        line = data[i+1]
        datasb = line.split()[-nsweep:]
        datasc=[]
        for j in datasb: datasc.append(float(j))
        datas.append(datasc)
        names.append(line[:26].rstrip())

    for i in range(len(datas)):
        for j in range(len(datas[i])):
            if datas[i][0] != 0: datas[i][-(j+1)] = datas[i][-(j+1)]/datas[i][0]
    
    sweepfig = figure(figsize=(12, 9), dpi=80)
    plot0 = sweepfig.add_subplot(111)
    for i in range(len(datas)):
        plot(datas[i], label=names[i])
        
    legend(loc='upper left')
    plot0.set_ylim([0, max([2, max(max(datas))])])
    
    if isps:
        savefig(outfile, orientation='landscape')
    else:
        show()
    
if __name__ == "__main__":
    main()
