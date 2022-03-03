"""Python utility for plotting the output of a PROCESS scan.

Depending of the type of scans, different actions will be taken:
1D SCANS: a simple graph using the scanned variable for x axis
and the selected variable on the y axis. 
- Any number of output variables can be selected, a plot will be
made for each
- Several inputs files can be used at the same time if the same variable
is scanned. The different runs results will be plotted in the same
graph.
- If several inputs are used, the folder name or the file is used as 
a legend

- 2D SCANS: n_scan_1 graph will be plotted using the second scanned variable
as x axis and the selected output as y axis
- Only one 2D scan can be ploted at once.

Performed checks:
- Non converged points are not plotted
- Only outputs existing in the MFILE.DAT are plotted
- A LaTeX label dicts is integrated, there is a check if the requested
variable is set. Otherwise the sad and gloomy PROCESS name is used
- No plot is made if the MFILE does not exists
- If the file is a folder, the contained MFILE is used as an input.
"""

import matplotlib.pyplot as plt
import numpy as np
import os
import argparse
from argparse import RawTextHelpFormatter
from pathlib import Path

# PROCESS libraries
from process.io.python_fortran_dicts import get_dicts
import process.io.mfile as mf


def parse_args(args):
    """Parse supplied arguments.

    :param args: arguments to parse
    :type args: list, None
    :return: parsed arguments
    :rtype: Namespace
    """
    parser = argparse.ArgumentParser(
        description="Plot optimization information",
        formatter_class=RawTextHelpFormatter,
    )

    parser.add_argument(
        "-f",
        "--input_files",
        default="MFILE.DAT",
        help=(
            "Specify input file(s) path(s) (default = MFILE.DAT)\n"
            "More than one input file can be used eg: -f 'A_MFILE.DAT "
            "B_MFILE.DAT'.\nYou can only specify the folder containing the "
            "MFILE.\nThe different files scan will be plotted on the same "
            "graph.\nThe scans must use the same scan variation."
        ),
    )

    # At least one output variable must be supplied in order to plot
    parser.add_argument(
        "-yv",
        "--y_vars",
        required=True,
        help=(
            "Select the output variables\nMore than one output can be plotted "
            "eg: -yv 'var1 var2'\nA separate plot will be created for each "
            "inputs"
        ),
    )

    parser.add_argument(
        "-o",
        "--outputdir",
        default=Path.cwd(),
        help="Output directory for plots, defaults to current working directory.",
    )

    parser.add_argument(
        "-out",
        "--term_output",
        action="store_true",
        help="Option to show scans values on terminal",
    )

    parser.add_argument(
        "-sf",
        "--save_format",
        nargs="?",
        default="pdf",
        help="Output format (default='pdf') ",
    )

    parser.add_argument(
        "-as",
        "--axis_font_size",
        nargs="?",
        default=18,
        help="Axis label font size selection (default=18)",
        type=int,
    )

    return parser.parse_args(args)


def main(args=None):
    """Main plot scans script.

    :param args: optional command-line args from test function, defaults to None
    :type args: list, optional
    """
    args = parse_args(args)

    # Parameters to be used as function input
    # ---------------------------------------
    input_files = str(args.input_files)
    output_names = str(args.y_vars)
    save_format = str(args.save_format)
    term_output = args.term_output
    # ---------------------------------------

    # Input checks
    # ------------
    # Formting the inputs
    output_names = output_names.split(" ")
    while "" in output_names:
        output_names.remove("")

    input_files = input_files.split(" ")
    while "" in input_files:
        input_files.remove("")

    # If the input file is a directory, add MFILE.DAT
    for ii in range(len(input_files)):
        if os.path.isdir(input_files[ii]):
            input_files[ii] = input_files[ii].replace("/", "")
            input_files[ii] = input_files[ii] + str("/MFILE.DAT")

        # Check for the existence of the MFILE
        if not os.path.isfile(input_files[ii]):
            print(
                "ERROR : The {} MFILE does not exist, skipping it".format(
                    input_files[ii]
                )
            )
            input_files.remove(input_files[ii])

    # LaTeX labels
    # ------------
    # ToDo : WOULD BE GREAT TO HAVE IT STORED IN THE PROCESS !
    labels = dict()
    labels["shldith"] = r"$\Delta R_\mathrm{sh}$ [$m$]"
    labels["rmajor"] = r"$R_\mathrm{maj}$ [$m$]"
    labels["crypmw"] = r"$P_\mathrm{cryo}$ [$MW$]"
    labels["bt"] = r"$B_\mathrm{T}$ [$T$]"
    labels["tfcth"] = r"$\Delta R_\mathrm{TF}$ [$m$]"
    labels["powfmw"] = r"$P_\mathrm{fus}$ [$MW$]"
    labels["pinjemw"] = r"$P_\mathrm{inj}$ [$MW$]"
    labels["pnetelmw"] = r"$P_\mathrm{Net\ elec}$ [$MW$]"
    labels["taueff"] = r"$\tau_\mathrm{E}$ [s]"
    labels["ralpne"] = r"$f_\mathrm{\alpha}$"
    labels["te"] = r"$\left< T_\mathrm{e} \right>$"
    labels["taulimit"] = r"$max : \frac{\tau_\mathrm{\alpha}}{\tau_\mathrm{E}}$"
    labels["scrapli"] = r"$\Delta R_\mathrm{FW-sep}$ [$m$]"
    labels["scraplo"] = r"$\Delta R_\mathrm{FW-sep}^\mathrm{out}$ [$m$]"
    labels["vforce"] = r"$F_\mathrm{z}^\mathrm{in}$ [$N$]"
    labels["thkcas"] = r"$\Delta R_\mathrm{TF}^\mathrm{buck}$ [$m$]"
    labels["bmaxtf"] = r"$B_\mathrm{TF}^\mathrm{max}$ [$T$]"
    labels["ritfc"] = r"$I_\mathrm{TF}^\mathrm{tot}$ [$A$]"
    labels["dr_tf_wp"] = r"$\Delta R_\mathrm{TF}^\mathrm{WP}$ [$m$]"
    labels["aspect"] = r"$A$"
    labels["rminor"] = r"$a_\mathrm{min}$ [$m$]"
    labels["capcost"] = r"$C_\mathrm{cap}$ [$M\$ $]"
    labels["r_tf_outboard_mid"] = r"$\Delta R_\mathrm{TF}^\mathrm{out\ mid}$ [$m$]"
    labels["pgrossmw"] = r"$P_\mathrm{gross}^\mathrm{elec}$ [$MW$]"
    labels["htpmw"] = r"$P_\mathrm{Primary\ coolant}^\mathrm{elec}$ [$MW$]"
    labels["ppfmw"] = r"$P_\mathrm{PF}^\mathrm{elec}$ [$MW$]"
    labels["hmax"] = r"$z_\mathrm{TF}^\mathrm{pl\ side}$ [$m$]"
    labels["thicndut"] = r"\Delta l_\mathrm{steel\ jacket}^\mathrm{turn}"
    labels["cpttf"] = r"$I_\mathrm{TF}^\mathrm{turn}$ [$A$]"
    labels["boundl(2)"] = r"$B_\mathrm{T}^\mathrm{min}$ [$A$]"
    labels["pinjmw"] = r"$P_\mathrm{inj}$ [$MW$]"
    labels["hldivlim"] = r"$q_\mathrm{div}^\mathrm{max}$ [$MW.m^{-2}$]"
    labels["hfact"] = r"$f_\mathrm{H}$"
    labels["kappa"] = r"$\kappa_\mathrm{sep}$"
    labels["triang"] = r"$\delta_\mathrm{sep}$"
    labels["f_tf_steel"] = r"f_\mathrm{steel}^\mathrm{TF}"
    labels["plascur/1d6"] = r"$I_{\mathrm{p}}$[$MA$]"
    labels["n_cycle"] = r'$N_{\mathrm{cycle}}$'
    labels['alstroh'] = r'$\sigma_{\mathrm{oh}}^{\mathrm{max}}$[$Pa$]'
    labels['ohcth'] = r'$\Delta R_{\mathrm{CS}}$[$m$]'
    labels['bore'] = r'$\Delta R_{\mathrm{bore}}$[$m$]'
    labels['dnla'] = r'$\bar{n}_{\mathrm{e}}$[$m^{-3}$]'
    labels['dnla_gw'] = r'$f_{\mathrm{GW}}$'
    labels['normalised_toroidal_beta'] = r'$\beta_{N,\mathrm{tor}}$'
    labels["copperaoh_m2"] = r"$I_{cs} : CuF_{cs} [$A M$^{-2}$$]$"
    
    # ------------

    # nsweep varible dict
    # -------------------
    # TODO WOULD BE GREAT TO HAVE IT AUTOMATICALLY GENERATED ON THE PROCESS CMAKE!
    #        THE SAME WAY THE DICTS ARE
    # This needs to be kept in sync automatically; this will break frequently 
    # otherwise
    # Rem : Some variables are not in the MFILE, making the defintion rather tricky...
    nsweep_dict = dict()
    nsweep_dict[1] = "aspect"
    nsweep_dict[2] = "hldivlim"
    nsweep_dict[3] = "pnetelmw"
    nsweep_dict[4] = "hfact"
    nsweep_dict[5] = "oacdcp"
    nsweep_dict[6] = "walalw"
    nsweep_dict[7] = "beamfus0"
    nsweep_dict[8] = "fqval"
    nsweep_dict[9] = "te"
    nsweep_dict[10] = "boundu(15)"
    nsweep_dict[11] = "dnbeta"
    nsweep_dict[12] = "bscfmax"
    nsweep_dict[13] = "boundu(10)"
    nsweep_dict[14] = "fiooic"
    nsweep_dict[15] = "fjprot"
    nsweep_dict[16] = "rmajor"
    nsweep_dict[
        17
    ] = "bmaxtf"  # bmxlim the maximum T field upper limit is the scan variable
    nsweep_dict[18] = "gammax"
    nsweep_dict[19] = "boundl(16)"
    nsweep_dict[20] = "tbrnmn"
    nsweep_dict[21] = ""
    nsweep_dict[22] = "cfactr"
    nsweep_dict[23] = "boundu(72)"
    nsweep_dict[24] = "powfmax"
    nsweep_dict[25] = "kappa"
    nsweep_dict[26] = "triang"
    nsweep_dict[27] = "tbrmin"
    nsweep_dict[28] = "bt"
    nsweep_dict[29] = "coreradius"
    nsweep_dict[30] = "fimpvar"
    nsweep_dict[31] = "taulimit"
    nsweep_dict[32] = "epsvmc"
    nsweep_dict[33] = "ttarget"
    nsweep_dict[34] = "qtargettotal"
    nsweep_dict[35] = "lambda_q_omp"
    nsweep_dict[36] = "lambda_target"
    nsweep_dict[37] = "lcon_factor"
    nsweep_dict[38] = "boundu(129)"
    nsweep_dict[39] = "boundu(131)"
    nsweep_dict[40] = "boundu(135)"
    nsweep_dict[41] = "blnkoth"
    nsweep_dict[42] = "fimp(9)"
    nsweep_dict[43] = "rho_ecrh"
    nsweep_dict[44] = "alstrtf"
    nsweep_dict[45] = "tmargmin_tf"
    nsweep_dict[46] = "boundu(152)"
    nsweep_dict[47] = "impurity_enrichment(9)"
    nsweep_dict[48] = "n_pancake"
    nsweep_dict[49] = "n_layer"
    nsweep_dict[50] = "fimp(13)"
    nsweep_dict[51] = "ftar"
    nsweep_dict[52] = "rad_fraction_sol"
    nsweep_dict[54] = "b_crit_upper_nbti"
    nsweep_dict[55] = "shldith"
    nsweep_dict[56] = "crypmw_max"
    nsweep_dict[57] = "bt"  # Genuinly bt lower bound
    nsweep_dict[58] = "scrapli"
    nsweep_dict[59] = "scraplo"
    nsweep_dict[60] = "sig_tf_wp_max"
    nsweep_dict[61] = "copperaoh_m2"
    # -------------------

    # Load PROCESS dicts from JSON files
    proc_dict = get_dicts()

    # Getting the scanned variable name
    m_file = mf.MFile(filename=input_files[-1])
    nsweep_ref = int(m_file.data["nsweep"].get_scan(-1))
    scan_var_name = nsweep_dict[nsweep_ref]

    # Get the eventual second scan variable
    nsweep_2_ref = int(0)
    is_2D_scan = False
    scan_2_var_name = str()
    if "nsweep_2" in m_file.data.keys():
        is_2D_scan = True
        nsweep_2_ref = int(m_file.data["nsweep_2"].get_scan(-1))
        scan_2_var_name = nsweep_dict[nsweep_2_ref]

    # Checks
    # ------
    # Check if the nsweep dict has been updated
    if nsweep_ref > len(nsweep_dict) + 1:
        print("ERROR : nsweep = {} not supported by the utility".format(nsweep_ref))
        print("ERROR : Please update the 'nsweep_dict' dict")
        exit()

    # Check if the scan variable is present in the
    if not scan_var_name in m_file.data.keys():
        print("ERROR : `{}` does not exist in PROCESS dicts".format(scan_var_name))
        print("ERROR : The scan variable is probably an upper/lower boundary")
        print("ERROR : Please modify 'nsweep_dict' dict with the constrained var")
        exit()

    # Check if the (first) scan variable LaTeX label is set
    if not scan_var_name in labels:
        print(
            "WARNING: The {} variable LaTeX label is not defined".format(scan_var_name)
        )
        print("WARNING: Please update the 'label' dict")
        labels[scan_var_name] = scan_var_name

    if is_2D_scan:
        # Check if the second scan variable is present in the
        if not scan_2_var_name in m_file.data.keys():
            print(
                "ERROR : `{}` does not exist in PROCESS dicts".format(scan_2_var_name)
            )
            print("ERROR : The scan variable is probably an upper/lower boundary")
            print("ERROR : Please modify 'nsweep_dict' dict with the constrained var")
            exit()

        # Check if the second scan variable LaTeX label is set
        if not scan_2_var_name in labels:
            print("The {} variable LaTeX label is not defined".format(scan_2_var_name))
            print("Please update the 'label' dict")
            labels[scan_var_name] = scan_var_name

    # Only one imput must be used for a 2D scan
    if is_2D_scan and len(input_files) > 1:
        print("ERROR : Only one input file can be used for 2D scans")
        print("ERROR : Exiting")
        exit()
    # ------

    # Plot settings
    # -------------
    # Plot cosmetic settings
    axis_tick_size = 16
    legend_size = 12
    axis_font_size = 18
    # -------------

    # Case of a set of 1D scans
    # ----------------------------------------------------------------------------------------------
    if not is_2D_scan:

        # Loop over the MFILEs
        output_arrays = dict()
        scan_var_array = dict()
        for input_file in input_files:

            # Opening the MFILE.DAT
            m_file = mf.MFile(filename=input_file)

            # Check if the the scan variable is the same for all inputs
            # ---
            # Same scan var
            nsweep = int(m_file.data["nsweep"].get_scan(-1))
            if nsweep != nsweep_ref:
                print("ERROR : You must use inputs files with the same scan variables")
                print("ERROR : Exiting")
                exit()

            # No D scans
            if "nsweep_2" in m_file.data.keys():
                print("ERROR : You cannot mix 1D with 2D scans")
                print("ERROR : Exiting")
                exit()
            # ---

            # Only selecting the scans that has converged
            # ---
            # Number of scan points
            n_scan = int(m_file.data["isweep"].get_scan(-1))

            # Converged indexes
            conv_i = list()
            for ii in range(n_scan):
                ifail = m_file.data["ifail"].get_scan(ii + 1)
                if ifail == 1:
                    conv_i.append(ii + 1)
                else:
                    failed_value = m_file.data[scan_var_name].get_scan(ii + 1)
                    print(
                        "Warning : Non-convergent scan point : {} = {}".format(
                            scan_var_name, failed_value
                        )
                    )
                    print("Warning : This point will not be shown.")

            # Updating the number of scans
            n_scan = len(conv_i)
            # ---

            # Scanned variable
            scan_var_array[input_file] = np.zeros(n_scan)
            for ii in range(n_scan):
                scan_var_array[input_file][ii] = m_file.data[scan_var_name].get_scan(
                    conv_i[ii]
                )

            # output list declaration
            output_arrays[input_file] = dict()
            for output_name in output_names:
                ouput_array = np.zeros(n_scan)

                # Check if the output variable exists in the MFILE
                if not output_name in m_file.data.keys():
                    print(
                        "Warning : `{}` does not exist in PROCESS dicts".format(
                            output_name
                        )
                    )
                    print("Warning : `{}` will not be output".format(output_name))
                    continue

                # Check if the output LaTeX variable label exist
                if not output_name in labels:
                    print(
                        "Warning : The {} variable LaTeX label is not defined".format(
                            output_name
                        )
                    )
                    print("Warning : Please update the 'label' dict")
                    labels[output_name] = output_name

                for ii in range(n_scan):
                    ouput_array[ii] = m_file.data[output_name].get_scan(conv_i[ii])
                output_arrays[input_file][output_name] = ouput_array

            # Terminal output
            if term_output:
                print()
                print("{} scan output".format(input_file))
                print(
                    "scan var {} : {}".format(scan_var_name, scan_var_array[input_file])
                )
                for output_name in output_names:

                    # Check if the output variable exists in the MFILE
                    if not output_name in m_file.data.keys():
                        continue

                    print(
                        "{} : {}".format(
                            output_name, output_arrays[input_file][output_name]
                        )
                    )
                print()

        # Plot section
        # ------------
        for output_name in output_names:

            # Check if the output variable exists in the MFILE
            if not output_name in m_file.data.keys():
                continue

            # Loop over inputs
            for input_file in input_files:

                # Legend label formating
                labl = input_file
                if "/MFILE.DAT" in input_file:
                    labl = input_file[:-10]
                elif "MFILE.DAT" in input_file:
                    labl = input_file[:-9]
                labl = labl.replace("_", " ")

                # Plot the graph
                plt.plot(
                    scan_var_array[input_file],
                    output_arrays[input_file][output_name],
                    "--o",
                    label=labl,
                )

            plt.grid(True)
            plt.ylabel(labels[output_name], fontsize=axis_font_size)
            plt.xlabel(labels[scan_var_name], fontsize=axis_font_size)
            if len(input_files) != 1:
                plt.legend(loc="best", fontsize=legend_size)
            plt.xticks(size=axis_tick_size)
            plt.yticks(size=axis_tick_size)
            plt.tight_layout()
            if output_name == "plascur/1d6":
                plt.savefig(
                    "{}/scan_{}_vs_{}.{}".format(
                        args.outputdir, scan_var_name, "plascur", save_format
                    )
                )
            else:
                plt.savefig(
                    "{}/scan_{}_vs_{}.{}".format(
                        args.outputdir, scan_var_name, output_name, save_format
                    )
                )
            
            # Display plot (used in Jupyter notebooks)
            plt.show()
            plt.clf()
        # ------------

    # I case of a 2D scan
    # ----------------------------------------------------------------------------------------------
    else:

        # Opening the MFILE.DAT
        m_file = mf.MFile(filename=input_files[0])

        # Number of scan points
        n_scan_1 = int(m_file.data["isweep"].get_scan(-1))
        n_scan_2 = int(m_file.data["isweep_2"].get_scan(-1))

        # Selecting the converged runs only
        conv_ij = list()
        ii_jj = 0
        for ii in range(n_scan_1):
            conv_ij.append(list())
            for jj in range(n_scan_2):
                ii_jj += 1
                ifail = m_file.data["ifail"].get_scan(ii_jj)
                if ifail == 1:
                    conv_ij[ii].append(ii_jj)
                else:
                    failed_value_1 = m_file.data[scan_var_name].get_scan(ii_jj)
                    failed_value_2 = m_file.data[scan_2_var_name].get_scan(ii_jj)
                    print(
                        "Warning : Non-convergent scan point : ({},{}) = ({},{})".format(
                            scan_var_name,
                            scan_2_var_name,
                            failed_value_1,
                            failed_value_2,
                        )
                    )
                    print("Warning : This point will not be shown.")

        # Looping over requested outputs
        for output_name in output_names:

            # Check if the output variable exists in the MFILE
            if not output_name in m_file.data.keys():
                print(
                    "Warning : `{}` does not exist in PROCESS dicts".format(output_name)
                )
                print("Warning : `{}` will not be output".format(output_name))
                continue

            # Check if the output LaTeX variable label exist
            if not output_name in labels:
                print(
                    "Warning : The {} variable LaTeX label is not defined".format(
                        output_name
                    )
                )
                print("Warning : Please update the 'label' dict")
                labels[output_name] = output_name

            # Declaring the outputs
            scan_1_var_arrays = list()
            scan_2_var_arrays = list()
            output_arrays = list()

            # Converged indexes
            for conv_j in conv_ij:

                # Scanned variables
                scan_1_var_array = np.zeros(len(conv_j))
                scan_2_var_array = np.zeros(len(conv_j))
                output_array = np.zeros(len(conv_j))
                for jj in range(len(conv_j)):
                    scan_1_var_array[jj] = m_file.data[scan_var_name].get_scan(
                        conv_j[jj]
                    )
                    scan_2_var_array[jj] = m_file.data[scan_2_var_name].get_scan(
                        conv_j[jj]
                    )
                    output_array[jj] = m_file.data[output_name].get_scan(conv_j[jj])

                # Label formating
                labl = "{} = {}".format(labels[scan_var_name], scan_1_var_array[0])

                # Plot the graph
                plt.plot(scan_2_var_array, output_array, "--o", label=labl)

            plt.grid(True)
            plt.ylabel(labels[output_name], fontsize=axis_font_size)
            plt.xlabel(labels[scan_2_var_name], fontsize=axis_font_size)
            plt.legend(loc="best", fontsize=legend_size)
            plt.xticks(size=axis_tick_size)
            plt.yticks(size=axis_tick_size)
            plt.tight_layout()
            plt.savefig(
                "{}/scan_{}_vs_{}_{}.{}".format(
                    args.outputdir,
                    output_name,
                    scan_var_name,
                    scan_2_var_name,
                    save_format,
                )
            )
            
            # Display plot (used in Jupyter notebooks)
            plt.show()
            plt.clf()


if __name__ == "__main__":
    main()
