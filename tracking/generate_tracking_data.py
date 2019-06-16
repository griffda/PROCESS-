"""
python3.6 generate_tracking_data.py --project=baseline_2018 --ref_folder=/home/jenkins_ci/tracking_ref_data --sep="comma"
    Change tracking tool

    Manoj Kumar
    James Morris
    UKAEA

    31/10/18

"""

import sys
import os.path
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
ref_sep = ","

def add_mfile_to_database(cargs, pdat, mdat, scan_num):
    """Add MFILE entry for a given scan to the Pandas dataframe

    Arguments:
        cargs {list} -- Command line arguments
        pdat {Pandas DataFrame} -- Pandas DataFrame object
        mdat {MFILE object} -- MFILE data object
        scan_num {int} -- Scan number to use
    """
    ref_folder=cargs.ref_folder# "/home/jenkins_ci/tracking_ref_data/"
    ref_file=ref_folder+cargs.project+"_ref_data.csv"
#        
    ref_data_frame = {}
    grp_size = cargs.var_group
#    grp_size = 5  
    new_entry = list()
    ref_columns = list()

    try:
        ref_data_frame = pandas.read_csv(ref_file, delimiter=ref_sep, index_col=0)
    except FileNotFoundError:
        # Create new frame with no columns
        ref_data_frame = pandas.DataFrame(columns=[])


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

#    new_entry.append(cargs.comment)

    # Create list of headers
    column_headers = TRACKING_LIST.copy()
#    column_headers.append("comment")

    # Create new frame
    new_frame = pandas.DataFrame([new_entry], columns=TRACKING_LIST)
    # Append frame to existing one
    new_pdat = pandas.concat([pdat, new_frame], sort=False, ignore_index=True)#, verify_integrity=False)

    if cargs.sep == "tab":
        sep = "\t"
    elif cargs.sep == "comma":
        sep = ","
    else:
        sep = " "
    ref_columns = list(ref_data_frame)
    ref_file_changed=False
    for var in TRACKING_LIST:
      if(new_frame[var].dtype == "float64" ):
         if var not in ref_columns:
            ref_data_frame[var] = new_frame[var]
            ref_file_changed=True
      elif(new_frame[var].dtype == "object" ):# or pdat[var].dtype == "int64"):       
          new_frame.drop(var,axis=1,inplace=True)
      else:
          new_frame.drop(var,axis=1,inplace=True)
#
    if ref_file_changed:
       ref_data_frame.to_csv(ref_file, sep=ref_sep)  # Now write updated file

    ref_columns = list(ref_data_frame)
    for var in ref_columns:
      if(ref_data_frame[var].dtype == "float64" ):
#        rat = new_frame[var]/ref_data_frame[var]
        new_frame[var] = new_frame[var]/ref_data_frame[var]
      else:
        print(var, "is not float")

    new_file=cargs.project+"_plot_data.csv"
    new_frame.to_csv(new_file, index = False, sep=ref_sep)
    ndim = new_frame.ndim
    n=new_frame.size
    grp_indx = 0
#    for i in range(0, n, grp_size):
#       grp_indx = int(1+i/grp_size)
#       ub = min(i + grp_size - 1, n)
#       new_file=cargs.project+"_"+str(grp_indx)+"_plot_data.csv"
#       new_frame.iloc[:ndim, i:ub].to_csv(new_file, index = False, sep=ref_sep)

#    print(" Created ",grp_indx," group of variables")
    return new_pdat


def main(clargs):
    """Main

    Arguments:
        clargs {list} -- Command line arguments
    """
    # Open database
    
    dtbase_folder = clargs.ref_folder#+"/" #  Database folder location
    dtbase_file=dtbase_folder+clargs.project+"_tracking_data.csv"          # Database file name
    print("-- Info : Tracked file exist. Found one.", dtbase_folder, dtbase_file)
    try:
        pdframe = pandas.read_csv(dtbase_file, delimiter=ref_sep, index_col=0)
        print("-- Info : Tracked file exist. Found one.", dtbase_folder, dtbase_file)
    except FileNotFoundError:
        # Create new frame
        pdframe = pandas.DataFrame(columns=TRACKING_LIST)
#        pdframe.to_csv(dtbase_file, sep=ref_sep)
        print("-- Warning: Tracked file doesn't exist. Creating new one.")

    # Read MFILE
    prj_mfile =  clargs.project+"_MFILE.DAT"  # Project MFILE
    mfile_data = mf.MFile(prj_mfile) #clargs.mfile)

    # Add MFILE entry to pandas object and return new frame
    pdframe = add_mfile_to_database(clargs, pdframe, mfile_data, clargs.var_group)
#    add_mfile_to_database(clargs, pdframe, mfile_data, clargs.var_group)
    pdframe.to_csv(dtbase_file, sep=ref_sep)



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
        "--plot",
        help="make pdf plots of changes",
        action="store_true"
    )

    PARSER.add_argument(
        "--var_group",
        type=int,
        default=5,
        help="Number of variables in a group for plot"
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
        default=5,
        help="Number of entries to plot (0=all)"
    )

    PARSER.add_argument(
        "--project",
        type=str,
        default="base_line",
        help="Name of project i.e. PROCESS input file name"
    )

    PARSER.add_argument(
        "--ref_folder",
        type=str,
        default="base_line",
        help="Name of reference folder where reference data is kept"
    )

    ARGS = PARSER.parse_args()

    main(ARGS)
