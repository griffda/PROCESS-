#!/usr/bin/env python

"""

  Plots MFILE.DAT data.

  James Morris
  CCFE

  Notes:
    + 30/04/2014: Initial version created

  Compatible with PROCESS version 274
"""

import argparse
import matplotlib.pyplot as plt
import process_io_lib.mfile as mf


def plot_scan(cargs, m_file_data):
    """Function to plot chosen variables.

    Args:
      command arguments --> arguments given at command line

    """

    x = range(1, m_file_data.get_num_scans()+1)
    data = {}
    for param in cargs.p:
        values = m_file_data.data[param].get_scans()
        if values[0] != 0.0:
            values = [val/values[0] for val in values]
        data[param] = values

    fig = plt.figure(figsize=(12, 9), dpi=100)
    ax = fig.add_subplot(111)

    for key in data.keys():
        ax.plot(x, data[key], label=key)
    ax.legend(loc="upper left")
    ax.set_xlabel("Scan Number")
    ax.set_ylabel("Parameters (Normalised to 1st scan)")
    if cargs.show:
        plt.show()
    else:
        plt.savefig(cargs.o, orientation="landscape")

if __name__ == "__main__":

    # Setup command line arguments
    parser = argparse.ArgumentParser(description='Plot sweep values from '
                                                 'MFILE.DAT')

    parser.add_argument('-f', metavar='f', type=str, default="MFILE.DAT",
                        help='File to read as MFILE.DAT')

    parser.add_argument('-o', metavar='o', type=str, default="sweep_fig.pdf",
                        help='File to save plot to')

    parser.add_argument('-p', metavar='p', type=str, nargs='+',
                        help='Variables for the plot (e.g. -p rmajor bt te ...'
                             ')')

    parser.add_argument("--show", help="Show plot to screen instead of saving "
                                       "to file", action="store_true")

    args = parser.parse_args()

    # read mfile
    mfile_data = mf.MFile(args.f)

    # plot requested parameters all normalised to the first scan
    plot_scan(args, mfile_data)
