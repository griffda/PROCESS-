#!/usr/bin/env python
"""
Code to compare costs between two runs

Stuart Muldrew (stuart.muldrew@ukaea.uk)
11/09/2018

"""

# Imported libraries
import argparse
import process_io_lib.mfile as mf
import matplotlib.pyplot as plt
import numpy as np


def comp_orig():
    
    labels = ['Magnets and\n Power Conditioning', 'Site and Buildings', 'Maintenance\n Equipment', 
    'Power Injection', 'Reactor Systems', 'Fuel Handling\n System', 'Instrumentation\n and Control', 
    'Turbine Plant\n Equipment', 'Heat Transport\n System', 'Other']
    labels2 = ['Plant Direct\n Cost', 'Indirect\n Cost', 'Total\n Contingency', 'Interest during\n Construction']
    index = np.arange(len(labels))
    index2 = np.arange(len(labels2))
    bar_width = 0.7 / len(mfile_list)
    fig, ax = plt.subplots()
    fig2, ax2 = plt.subplots()

    # Read cost data
    for id, item in enumerate(mfile_list):
        cost = np.zeros(18)
        cost[0] = item.data["c21"].get_scan(-1)           # Site and Buildings
        cost[1] = item.data["c221"].get_scan(-1)          # Reactor Systems
        cost[2] = item.data["c222"].get_scan(-1)          # Magnets
        cost[3] = item.data["c223"].get_scan(-1)          # Power Injection
        cost[4] = item.data["c224"].get_scan(-1)          # Vacuum Systems
        cost[5] = item.data["c225"].get_scan(-1)          # Power Conditioning
        cost[6] = item.data["c226"].get_scan(-1)          # Heat Transport System
        cost[7] = item.data["c227"].get_scan(-1)          # Fuel Handling System
        cost[8] = item.data["c228"].get_scan(-1)          # Instrumentation and Control
        cost[9] = item.data["c229"].get_scan(-1)          # Maintenance Equipment
        cost[10] = item.data["c23"].get_scan(-1)          # Turbine Plant Equipment
        cost[11] = item.data["c24"].get_scan(-1)          # Electric Plant Equipment
        cost[12] = item.data["c25"].get_scan(-1)          # Miscellaneous Plant Equipment
        cost[13] = item.data["c26"].get_scan(-1)          # Heat Rejection System
        cost[14] = item.data["cdirt"].get_scan(-1)        # Plant Direct Cost
        cost[15] = item.data["c9"].get_scan(-1)           # Indirect Cost
        cost[16] = item.data["ccont"].get_scan(-1)        # Total Contingency
        cost[17] = item.data["moneyint"].get_scan(-1)     # Interest during Construction
        
        sizes = [cost[2]+cost[5], cost[0], cost[9], cost[3], cost[1], cost[7],
        cost[8], cost[10], cost[6], cost[4]+cost[11]+cost[12]+cost[13]]

        sizes2 = [cost[14], cost[15], cost[16], cost[17]]

        ax.bar(index + id*bar_width, sizes, bar_width, label=args.f[id])
        ax2.bar(index2 + id*bar_width, sizes2, bar_width, label=args.f[id])

    ax.set_xticks(index + (len(mfile_list)-1)*0.5*bar_width)
    ax2.set_xticks(index2 + (len(mfile_list)-1)*0.5*bar_width)
    ax.set_xticklabels(labels, rotation=90)
    ax2.set_xticklabels(labels2, rotation=90)
    ax.legend()
    ax2.legend()
    ax.set_ylabel('1990 M$')
    ax2.set_ylabel('1990 M$')

    fig.tight_layout()
    fig2.tight_layout()

    if args.save:
        fig.savefig('direct_cost_bar.png')
        fig2.savefig('cost_bar.png')
    plt.show()

# Main code
if __name__ == '__main__':

    # Setup command line arguments
    parser = argparse. \
        ArgumentParser(description="Compares the cost breakdown of two PROCESS runs.  "
        "For more information contact Stuart.Muldrew@ukaea.uk")

    parser.add_argument('-f', metavar='f', type=str, nargs='+',
                        help='MFILEs to compare')

    parser.add_argument("-s", "--save", help="save as well as showing figure",
                        action="store_true")


    args = parser.parse_args()

    # Get file names
    mfile_list = list()
    for item in args.f:
        mfile_list.append(mf.MFile(filename=item))

    if ("c21" in mfile_list[0].data.keys()):
        comp_orig()
    elif ("s01" in mfile_list[0].data.keys()):
        print('New Cost Model not yet supported')
    else:
        print('ERROR: Problem with cost data, check MFILEs!')

    