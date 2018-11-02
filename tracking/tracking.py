"""

    Change tracking tool

    James Morris
    UKAEA

    31/10/18

"""

import sys
from collections import OrderedDict
import argparse
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as bpdf
from matplotlib.ticker import MaxNLocator
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

PLOT_CONFIG = OrderedDict({
    "page01":{
        "title": "Plasma Geometry",
        "subplot_rows": 4,
        "subplot_columns": 2,
        "plots":{
            "1":{
                "x":"id",
                "y":["rmajor", "rminor"],
                "y_label": "m"
            },
            "2":{
                "x":"id",
                "y":["kappa", "kappa95"],
                "y_label": ""
            },
            "3":{
                "x":"id",
                "y":["triang", "triang95"],
                "y_label": ""
            },
            "4":{
                "x":"id",
                "y":["aspect"],
                "y_label": ""
            },
            "5":{
                "x":"id",
                "y":["vol"],
                "y_label": "m$^3$"
            },
            "6":{
                "x":"id",
                "y":["sarea"],
                "y_label": "m$^2$"
            }
        }
    },
    "page02":{
        "title": "Plasma Parameters",
        "subplot_rows": 4,
        "subplot_columns": 1,
        "plots":{
            "1":{
                "x":"id",
                "y":["plascur/1d6"],
                "y_label": "MA"
            },
            "2":{
                "x":"id",
                "y":["powfmw"],
                "y_label": "MW"
            },
            "3":{
                "x":"id",
                "y":["bt"],
                "y_label": "T"
            },
            "4":{
                "x":"id",
                "y":["q95"],
                "y_label": ""
            }
        }
    }
})


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

    print(new_pdat)
    return new_pdat


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

def make_plots(cargs, pdat):
    """Make PDF plots

    Arguments:
        cargs {list} -- Command line arguments
        pdat {Pandas DataFrame} -- Pandas DataFrame object
    """

    # Plot configurations still to be completed

    # Number of entries to plot
    total_entries = len(pdat.index)

    start = cargs.entries

    if start > total_entries:
        print("-- Warning: entries requested larger than total number. "
              "Plotting all")
        start_ind = 0
    else:
        if start != 0:
            start_ind = total_entries - start
        else:
            start_ind = 0


    # List of pages
    pages = list()

    for page in PLOT_CONFIG.keys():

        # Get page title
        title = PLOT_CONFIG[page]["title"]

        # Create plot page
        plot_page = plt.figure(figsize=(8, 9), dpi=150)
        plot_page.suptitle(title, fontsize=14)

        # Number of rows and columns
        rows = PLOT_CONFIG[page]["subplot_rows"]
        columns = PLOT_CONFIG[page]["subplot_columns"]

        # Total number of plots
        number_of_plots = len(PLOT_CONFIG[page]["plots"].keys())

        # Check the number of plots vs. rows columns is consistent
        if number_of_plots > rows*columns:
            print("-- Error: More plots than spaces for page: {0}".format(page))
            sys.exit(0)

        for i in range(number_of_plots):

            # Create subplot
            axis = plot_page.add_subplot(rows, columns, i+1)

            # Row number of x values
            x_values = list(pdat.index)[start_ind:]
            axis.xaxis.set_major_locator(MaxNLocator(integer=True))
            axis.set_xlabel("id")

            # List of y entries
            y_keys = PLOT_CONFIG[page]["plots"]["{0}".format(i+1)]["y"]

            # Axis label for y axis
            y_label = PLOT_CONFIG[page]["plots"]["{0}".format(i+1)]["y_label"]
            axis.set_ylabel(y_label)

            # Plot listed y values
            for item in y_keys:
                y_values = pdat[item][start_ind:]/pdat[item][0]
                axis.plot(x_values, y_values, label=item)
            axis.legend(loc="best")

        plot_page.subplots_adjust(wspace=0.5, hspace=0.5)
        pages.append(plot_page)

    with bpdf.PdfPages("{0}.pdf".format(cargs.database)) as pdf:
        for page in pages:
            pdf.savefig(page)

    for page in pages:
        plt.close(page)


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

    if clargs.save:
        save_latest_entry(clargs, pdframe)

    if clargs.plot:
        make_plots(clargs, pdframe)


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
        default="baseline_tracking.txt",
        help="save output of tracking tool to the specified filename"
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
