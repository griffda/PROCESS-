#!/usr/bin/env python
"""
Code to display the cost breakdown as a pie chart

Stuart Muldrew (stuart.muldrew@ukaea.uk)
06/09/2018

"""

# Imported libraries
import argparse
import process_io_lib.mfile as mf
import matplotlib.pyplot as plt

def orig_cost_model():
    # Read Cost Values
    c21 = m_file.data["c21"].get_scan(-1)           # Site and Buildings
    c221 = m_file.data["c221"].get_scan(-1)         # Reactor Systems
    c222 = m_file.data["c222"].get_scan(-1)         # Magnets
    c223 = m_file.data["c223"].get_scan(-1)         # Power Injection
    c224 = m_file.data["c224"].get_scan(-1)         # Vacuum Systems
    c225 = m_file.data["c225"].get_scan(-1)         # Power Conditioning
    c226 = m_file.data["c226"].get_scan(-1)         # Heat Transport System
    c227 = m_file.data["c227"].get_scan(-1)         # Fuel Handling System
    c228 = m_file.data["c228"].get_scan(-1)         # Instrumentation and Control
    c229 = m_file.data["c229"].get_scan(-1)         # Maintenance Equipment
    c23 = m_file.data["c23"].get_scan(-1)           # Turbine Plant Equipment
    c24 = m_file.data["c24"].get_scan(-1)           # Electric Plant Equipment
    c25 = m_file.data["c25"].get_scan(-1)           # Miscellaneous Plant Equipment
    c26 = m_file.data["c26"].get_scan(-1)           # Heat Rejection System

    cdirt = m_file.data["cdirt"].get_scan(-1)       # Plant Direct Cost
    c9 = m_file.data["c9"].get_scan(-1)             # Indirect Cost
    ccont = m_file.data["ccont"].get_scan(-1)       # Total Contingency
    if "moneyint" in m_file.data.keys():
        moneyint = m_file.data["moneyint"].get_scan(-1) # Interest during Construction
        labels2 = ['Plant Direct Cost', 'Indirect Cost', 'Total Contingency', 'Interest during Construction']
        sizes2 = [cdirt, c9, ccont, moneyint]
    else:
        labels2 = ['Plant Direct Cost', 'Indirect Cost', 'Total Contingency']
        sizes2 = [cdirt, c9, ccont]


    labels = ['Magnets and Power Conditioning', 'Site and Buildings', 'Maintenance Equipment', 
    'Power Injection', 'Reactor Systems', 'Fuel Handling System', 'Instrumentation and Control', 
    'Turbine Plant Equipment', 'Heat Transport System', 'Other']

    sizes = [c222+c225, c21, c229, c223, c221, c227, c228, c23, c226, c224+c24+c25+c26]

    #plt.figure(figsize=(8,8))
    fig1, ax1 = plt.subplots(figsize=(8,5))
    ax1.pie(sizes, labels=labels, autopct='%1.1f%%')
    ax1.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

    fig2, ax2 = plt.subplots(figsize=(8,5))
    ax2.pie(sizes2, labels=labels2, autopct='%1.1f%%')
    ax2.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.

    #fig1.savefig('direct_cost_breakdown.png')
    #fig2.savefig('cost_breakdown.png')
    plt.show()

def new_cost_model():
    print('2014 cost model not yet supported')


# Main code
if __name__ == '__main__':

    # Setup command line arguments
    parser = argparse. \
        ArgumentParser(description="Displays the cost breakdown as a pie chart.  "
        "For more infomation contact Stuart Muldrew (stuart.muldrew@ukaea.uk)")

    parser.add_argument("-f", metavar='MFILE', type=str,
                       default="MFILE.DAT", help='specify the MFILE (default=MFILE.DAT)')

    parser.add_argument("-v", metavar='cost_model', type=int,
                       default="0", help='cost model used (0: 1990 model (default), 1: 2014 model)')

    args = parser.parse_args()

    m_file = mf.MFile(args.f)

    if args.v==0:
        orig_cost_model()
    elif args.v==1:
        new_cost_model()
    else:
        print('Invalid cost model')
