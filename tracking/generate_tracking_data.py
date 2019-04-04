"""

    Change tracking tool

    James Morris
    UKAEA

    31/10/18

"""

import sys
from collections import OrderedDict
import argparse

import pandas
from process_io_lib import mfile as mf


# List of variables to be tracked
TRACKING_LIST = [
    "procver", "time", "username", "tagno", "commsg", "ifail",
    "isweep", "nsweep", "rmajor", "rminor", "aspect", "kappa", "kappa95",
    "triang", "triang95", "sarea", "vol", "tfno", "powfmw", "plascur/1d6",
    "bt", "q95", "beta", "normalised_thermal_beta", "normalised_total_beta",
    "thermal_beta", "thermal_poloidal_beta", "te", "te0", "dene", "ne0",
    "dnla_gw", "tesep", "nesep", "teped", "neped", "ieped", "zeff", "dnz",
    "taueff", "hfact", "tauelaw", "ralpne", "wallmw", "pcoreradmw",
    "psyncpv*vol", "pedgrandmw", "pradmw", "pnucblkt", "pnucshld", "pdivt",
    "pthresh", "fwbllife", "divlife", "pthermw", "bore", "ohcth",
    "precomp", "gapoh", "tfcth", "deltf", "thshield", "gapds", "ddwi",
    "shldith", "vvblgap", "blnkith", "fwith", "scrapli", "scraplo", "fwoth",
    "blnkoth", "shldoth", "gapsto", "tftsgap", "tfthko",
    "etath", "pgrossmw", "pnetelmw", "pinjmw", "pheat", "bootipf", "faccd",
    "facoh", "gamnb", "enbeam", "powerht", "pdivr", "pdivnr", "flh", "hstar",
    "vssoft", "vstot", "tburn", "bmaxtf", "iooic", "tmarg", "tftmp", "strtf1",
    "strtf2", "alstrtf", "coe", "concost", "capcosts", "fmom", "qtarget",
    "qtargetcomplete", "totalpowerlost"
]

def add_mfile_to_database(cargs, pdat, mdat, scan_num):
    """Add MFILE entry for a given scan to the Pandas dataframe

    Arguments:
        cargs {list} -- Command line arguments
        pdat {Pandas DataFrame} -- Pandas DataFrame object
        mdat {MFILE object} -- MFILE data object
        scan_num {int} -- Scan number to use
    """

    new_entry = list()

    if "isweep" in mdat.data.keys():
        number_of_scans = mdat.data["isweep"].get_scan(-1)
    else:
        number_of_scans = 1
        scan_num = -1

    if scan_num > number_of_scans:
        print("-- Warning: scan number requested greater than number of scans")
        print("--        : requested {0} of {1} scans".
              format(scan_num, number_of_scans))
        print("--        : setting scan number to final scan")
        scan_num = -1

    for item in TRACKING_LIST:
        new_entry.append(mdat.data[item].get_scan(scan_num))

    new_entry.append(cargs.comment)

    # Create list of headers
    column_headers = TRACKING_LIST.copy()
    column_headers.append("comment")

    # Create new frame
    new_frame = pandas.DataFrame([new_entry], columns=column_headers)

    # Append frame to existing one
    new_pdat = pandas.concat([pdat, new_frame], ignore_index=True, sort=False, 
                              verify_integrity=True)
    return new_pdat

def add_latest_parametric_entry(cargs, pdat):
    """Save changes to overwrite input database

    Arguments:
        cargs {list} -- Command line arguments
        pdat {Pandas DataFrame} -- Pandas DataFrame object
    """

    # Check separator
    if cargs.sep == "tab":
        sep = "\t"
    elif cargs.sep == "comma":
        sep = ","
    else:
        sep = " "

    for var in TRACKING_LIST:
      first_val = pdat[var][0]
      if(pdat[var].dtype == "float64"):# or pdat[var].dtype == "int64"):
#        print(first_val)
        pdat[var] = pdat[var]/first_val
#        print(var, pdat[var].dtype)
      else:
        pdat.drop(var,axis=1,inplace=True)
#      print(var)
#        print(var,pdat[var].dtype)
      
    pdat.to_csv(cargs.jenkins, sep=sep)

def save_latest_entry(cargs, pdat):
    """Save changes to overwrite input database

    Arguments:
        cargs {list} -- Command line arguments
        pdat {Pandas DataFrame} -- Pandas DataFrame object
    """

    # Check separator
    if cargs.sep == "tab":
        sep = "\t"
    elif cargs.sep == "comma":
        sep = ","
    else:
        sep = " "

    pdat.to_csv(cargs.save, sep=sep)

def main(clargs):
    """Main

    Arguments:
        clargs {list} -- Command line arguments
    """

    # Open database
    try:
        pdframe = pandas.read_csv(clargs.database, delim_whitespace=True)
    except FileNotFoundError:
        # Create new frame
        pdframe = pandas.DataFrame(columns=TRACKING_LIST)
        clargs.save = clargs.database
        print("-- Warning: Tracked file doesn't exist. Creating new one.")
        print("          : saved as {0}".format(clargs.database))

    # Read MFILE
    mfile_data = mf.MFile(clargs.mfile)

    # Add MFILE entry to pandas object and return new frame
    pdframe = add_mfile_to_database(clargs, pdframe, mfile_data, clargs.scan)
    pdframe.to_csv('plot_data.csv', encoding='utf-8', index=False)
    
#df.to_csv('plot_data.csv', encoding='utf-8', mode='a', header=False, index=False)

    if clargs.save:
        save_latest_entry(clargs, pdframe)
    if clargs.jenkins:        
        add_latest_parametric_entry(clargs, pdframe)
#    pdframe.to_csv('plot_data.csv', encoding='utf-8', index=False)
#    print("",pdframe["rmajor"])
#    print(pdframe.columns)
#    print(TRACKING_LIST)
#    for var in TRACKING_LIST:
#      print(pdframe[var].size)


if __name__ == "__main__":

    # Setup command line arguments
    PARSER = argparse.ArgumentParser(
        description="Tool for recording changes to a particular PROCESS run."
                    "For info contact james.morris2@ukaea.uk"
    )

    PARSER.add_argument(
        "--mfile",
        type=str,
        default="MFILE.DAT",
        help='specify MFILE name'
    )

    PARSER.add_argument(
        "--database",
        type=str,
        default="baseline_tracking.txt",
        help='specify database file name'
    )

    PARSER.add_argument(
        "--save",
        type=str,
        default="baseline_tracking_output.txt",
        help="save output of tracking tool to the specified filename"
    )

    PARSER.add_argument(
        "--jenkins",
        type=str,
        default="jenkins_plot.csv",
        help="print parametric output of tracking tool to the specified filename"
    )

    PARSER.add_argument(
        "--plot",
        help="make pdf plots of changes",
        action="store_true"
    )

    PARSER.add_argument(
        "--scan",
        type=int,
        default=-1,
        help="Which scan to use from MFILE"
    )

    PARSER.add_argument(
        "--sep",
        type=str,
        default="space",
        help="Define separator for output file ['space', 'tab', 'comma']"
    )

    PARSER.add_argument(
        "--comment",
        type=str,
        default=" ",
        help="Comment for the entry"
    )

    PARSER.add_argument(
        "--entries",
        type=int,
        default=0,
        help="Number of entries to plot (0=all)"
    )

    ARGS = PARSER.parse_args()

    main(ARGS)
