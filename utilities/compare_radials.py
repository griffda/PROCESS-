#!/usr/bin/env python
"""
  Radial plot comparison tool using PLASMOD-type input

  Katy Ellis
  01/03/2018
  CCFE

"""

#######################
# imported libraries

# from matplotlib import rc,use
# rc('font',size=15)
# rc('lines',lw=1.5)#is overwritten by setting coulours/linestylies
# rc(('xtick','ytick'),labelsize=15)
# rc('figure',figsize=(8,6))
# rc('figure.subplot',bottom=0.12)
# rc('figure.subplot',left=0.19)
# rc('figure.subplot',right=0.81)

import process.io.mfile as mf
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as bpdf
import argparse
import numpy as np
from numpy import loadtxt


def plotProfile(n, i, p, count):
    """Function to plot radial profiles from 1D modelling, e.g. from PLASMOD or ASTRA
    Arguments:
      n --> number of input files in column format
      i --> column number to plot
      p --> page number to add plot
      count --> position of plot on page
    """

    # for i in range(0,len(ASSIGN_PAGE)):
    #    if(ASSIGN_PAGE[i]==p):
    if count == 0:
        figure = 221
    if count == 1:
        figure = 222
    if count == 2:
        figure = 223
    if count == 3:
        figure = 224
    page[p].add_subplot(figure)

    for f in range(0, n):
        if f == 0:
            plt.plot(
                DATA_PROFILES[f][0],
                DATA_PROFILES[f][i],
                linewidth=2,
                color="C1",
                zorder=2,
                marker="",
                label=LEGEND_NAME[f],
            )
        else:
            plt.scatter(
                DATA_PROFILES[f][0],
                DATA_PROFILES[f][i],
                s=60,
                color="#808284",
                zorder=1,
                label=LEGEND_NAME[f],
            )
            # plt.scatter(DATA_PROFILES[f][0], DATA_PROFILES[f][i], s = 20, color = 'C0', label=LEGEND_NAME[f])

    if ARGS.mfile is not None:
        # Add profiles generated by PROCESS 0D (ipedestal = 1)
        print(LIST_PROFILES[i])
        if (LIST_PROFILES[i]) == "DEN":
            print("Adding density 0D")
            plot_nprofile(plt)
        if (LIST_PROFILES[i]) == "TE":
            print("Adding electron temp 0D")
            plot_tprofile(plt)
        if (LIST_PROFILES[i]) == "Q":
            print("Adding safety factor 0D")
            plot_qprofile(plt)

    plt.xlabel(AXIS_TITLES[0])
    plt.ylabel(AXIS_TITLES[i])
    plt.title(PLOT_TITLES[i])
    plt.legend()


def plot_nprofile(prof):  # cut-down version from plot_proc.py
    """Function to plot density profile
    Arguments:
      prof --> axis object to add plot to
    """
    # if ipedestal == 1:
    rhocore1 = np.linspace(0, 0.95 * rhopedn)
    rhocore2 = np.linspace(0.95 * rhopedn, rhopedn)
    rhocore = np.append(rhocore1, rhocore2)
    ncore = neped + (ne0 - neped) * (1 - rhocore**2 / rhopedn**2) ** alphan

    rhosep = np.linspace(rhopedn, 1)
    nsep = nesep + (neped - nesep) * (1 - rhosep) / (1 - min(0.9999, rhopedn))

    rho = np.append(rhocore, rhosep)
    ne = np.append(ncore, nsep)
    # else:
    #    rho1 = np.linspace(0,0.95)
    #    rho2 = np.linspace(0.95,1)
    #    rho = np.append(rho1,rho2)
    #    ne = ne0 * (1-rho**2)**alphan
    ne = ne / 1e19
    prof.plot(rho, ne, linewidth=4, linestyle="--", color="#0082CA", label="PROCESS 0D")
    prof.legend()


def plot_tprofile(prof):
    """Function to plot temperature profile
    Arguments:
      prof --> axis object to add plot to
    """
    # if ipedestal == 1:
    rhocore1 = np.linspace(0, 0.9 * rhopedt)
    rhocore2 = np.linspace(0.9 * rhopedt, rhopedt)
    rhocore = np.append(rhocore1, rhocore2)
    tcore = teped + (te0 - teped) * (1 - (rhocore / rhopedt) ** tbeta) ** alphat

    rhosep = np.linspace(rhopedt, 1)
    tsep = tesep + (teped - tesep) * (1 - rhosep) / (1 - min(0.9999, rhopedt))

    rho = np.append(rhocore, rhosep)
    te = np.append(tcore, tsep)
    # else:
    #    rho = np.linspace(0,1)
    #    te = te0 * (1-rho**2)**alphat
    prof.plot(rho, te, linewidth=4, linestyle="--", color="#0082CA", label="PROCESS 0D")
    prof.legend()


def plot_qprofile(prof):
    """Function to plot q profile, from Sauter model.
    Arguments:
      prof --> axis object to add plot to
    """
    rho = np.linspace(0, 1)
    q_r_sauter = q0 + (q95 - q0) * (rho * rho)

    prof.plot(rho, q_r_sauter, linewidth=4, color="#0082CA", label="PROCESS 0D")
    prof.legend()


def get_mfileData():
    """Function to assign data from the MFILE for use in 0D profiles
    Arguments: None
    """
    m_file = mf.MFile(ARGS.mfile)
    scan = -1  # takes last scan value
    global rhopedn
    rhopedn = m_file.data["rhopedn"].get_scan(scan)
    global neped
    neped = m_file.data["neped"].get_scan(scan)
    global alphan
    alphan = m_file.data["alphan"].get_scan(scan)
    global nesep
    nesep = m_file.data["nesep"].get_scan(scan)
    global ne0
    ne0 = m_file.data["ne0"].get_scan(scan)
    global rhopedt
    rhopedt = m_file.data["rhopedt"].get_scan(scan)
    global teped
    teped = m_file.data["teped"].get_scan(scan)
    global te0
    te0 = m_file.data["te0"].get_scan(scan)
    global tbeta
    tbeta = m_file.data["tbeta"].get_scan(scan)
    global alphat
    alphat = m_file.data["alphat"].get_scan(scan)
    global tesep
    tesep = m_file.data["tesep"].get_scan(scan)
    global q0
    q0 = m_file.data["q0"].get_scan(scan)
    global q95
    q95 = m_file.data["q95"].get_scan(scan)


if __name__ == "__main__":

    ############################################################
    # Usage

    PARSER = argparse.ArgumentParser(
        description="Program to fit a\
    general temperature or density profile from an ascii table."
    )

    PARSER.add_argument(
        "-f",
        "--filename0",
        default="profile.txt",
        help="ascii _RAD.DAT or .txt file containing data in columns,\
                        default = IN.DAT_RADP.DAT",
    )

    PARSER.add_argument(
        "-f1", "--filename1", help="optional ascii file containing data in columns"
    )

    PARSER.add_argument(
        "-m", "--mfile", help="optional ascii mfile output from PROCESS"
    )

    ARGS = PARSER.parse_args()

    # The user may wish to change these inputs:
    LEGEND_NAME = ["PROCESS 1D", "ASTRA"]  # filenames for the legend
    LIST_PROFILES = [
        "RHO",
        "DEN",
        "TE",
        "TI",
        "DEUT",
        "TRIT",
        "JBS",
        "JCD",
        "JTOT",
        "IPOL",
        "Q",
        "VOL",
        "DVOL",
        "COND",
    ]  # list of properties
    NUM_PAGES = 4  # number of pages required, e.g. 4
    PLOTS_PAGE = 4  # number of plots per page, e.g. 4

    PLOT_TITLES = [" "]
    PLOT_TITLES.append("Electron density")
    PLOT_TITLES.append("Electron temperature")
    PLOT_TITLES.append("Ion temperature")
    PLOT_TITLES.append("Deuterium density")
    PLOT_TITLES.append("Tritium density")
    PLOT_TITLES.append("Bootstrap current density")
    PLOT_TITLES.append("Current drive current density")
    PLOT_TITLES.append("Total current density")
    PLOT_TITLES.append("Poloidal current")
    PLOT_TITLES.append("Safety factor")
    PLOT_TITLES.append("Plasma volume")
    PLOT_TITLES.append("dVolume/dr")
    PLOT_TITLES.append("Plasma conductivity")

    AXIS_TITLES = ["r/a"]
    AXIS_TITLES.append(r"$n_e / 10^{19} m^{-3}$")
    AXIS_TITLES.append(r"$t_e / keV$")
    AXIS_TITLES.append(r"$t_i / keV$")
    AXIS_TITLES.append("$deut / 10^{19} m^{-3}$")
    AXIS_TITLES.append("$trit / 10^{19} m^{-3}$")
    AXIS_TITLES.append("$J_bs / MA.m^{-2}$")
    AXIS_TITLES.append("$J_cd / MA.m^{-2}$")
    AXIS_TITLES.append("$J_tot / MA.m^{-2}$")
    AXIS_TITLES.append("$I_pol / MA$")
    AXIS_TITLES.append("$q$")
    AXIS_TITLES.append("$Vol / m^{3}$")
    AXIS_TITLES.append("$dVol/dr / m^{2}$")
    AXIS_TITLES.append("$Cond / MA.V^{-1}.m^{-1})$")

    PAGE_TITLES = ["Radial profiles (page 1)"]
    PAGE_TITLES.append("Radial profiles (page 2)")
    PAGE_TITLES.append("Radial profiles (page 3)")
    PAGE_TITLES.append("Radial profiles (page 4)")
    #################################

    list_len = len(LIST_PROFILES)
    print("number of profiles to be plotted = " + repr(list_len - 1))

    # ASSIGN_PAGE = [1, 2, 2, 1, 1, 3, 3, 3, 3, 1, 4, 4, 2] #Which page for each plot (4 plots per page)

    fileArray = [ARGS.filename0]
    if ARGS.filename1 is not None:
        fileArray = [ARGS.filename0, ARGS.filename1]
    n = len(fileArray)
    print("number of files is: " + repr(n))
    DATA = [None] * n
    DATA_PROFILES = [[0] * list_len for i in range(n)]

    # create vectors to store the data
    useProfile = [None] * list_len
    for i in range(0, list_len):
        useProfile[i] = False

    for f in range(0, n):  # loop over files
        print("filename" + repr(f) + " = " + repr(fileArray[f]))
        try:
            DATA[f] = loadtxt(fileArray[f], unpack=True)
            print("loaded data!")
            print("data size - " + repr(DATA[f].size))
        except OSError:
            print("Error: There is no file called", fileArray[f])
            exit()
        except ValueError:
            print(
                "Error: In",
                fileArray[f],
                "all comment rows must\
            start with a #! Columns can only contain floats!",
            )
            exit()
        if DATA[f].size == 0:
            print("Error: There is no data in", fileArray[f])
            exit()

        for i in range(0, list_len):
            try:
                DATA_PROFILES[f][i] = DATA[f][i]
                useProfile[i] = True
            except IndexError:
                print(
                    "Warning: The column for the "
                    + repr(LIST_PROFILES[i])
                    + " in file "
                    + repr(f)
                    + " does not exist!\
                Remember to start counting at 0!"
                )
        print(useProfile)

    if ARGS.mfile is not None:
        get_mfileData()

    # Make plots
    i = 1  # i.e. don't plot rho against itself
    page = [None] * NUM_PAGES
    for p in range(0, NUM_PAGES):
        page[p] = plt.figure(figsize=(12, 9), dpi=80)
        plt.suptitle(PAGE_TITLES[p])
        count = 0
        while i < list_len:

            # plot radial profiles from column format
            plotProfile(n, i, p, count)

            i = i + 1
            count = count + 1
            # to determine plot position on page
            if count == PLOTS_PAGE:
                break

    with bpdf.PdfPages("radial_profiles.pdf") as pdf:
        pdf.savefig(page[0])
        pdf.savefig(page[1])
        pdf.savefig(page[2])
        pdf.savefig(page[3])
    plt.show(page[0])
