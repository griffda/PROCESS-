#! /usr/bin/env python3
"""

  PROCESS plot_proc using process_io_lib functions and MFILE.DAT

  James Morris
  13/04/2014
  CCFE

  Compatible with PROCESS version ???
"""

import argparse
import process_io_lib.mfile as mf
import process_io_lib.proc_plot_func as plot_func
import matplotlib.pyplot as plt


def main(fig, m_file_data, scan=-1):
    """Function to create radial and vertical build plot on given figure.

    Arguments:
      fig --> figure object to add plot to.
      m_file_data --> MFILE.DAT data to read
      scan --> scan to read from MFILE.DAT

    """

    # Plot machine picture
    plot_1 = fig.add_subplot(231, aspect='equal')
    plot_func.plot_machine_pic(plot_1, m_file_data, scan)

    # Plot TF coils
    plot_func.plot_tf_coils(plot_1, m_file_data, scan)

    # Plot PF coils
    plot_func.plot_pf_coils(plot_1, m_file_data, scan)

    # Setup params for text plots
    plt.rcParams.update({'font.size': 8})

    # Geometry
    plot_2 = fig.add_subplot(232)
    plot_func.plot_geometry_info(plot_2, m_file_data, scan)

    # Physics
    plot_3 = fig.add_subplot(233)
    plot_func.plot_physics_info(plot_3, m_file_data, scan)

    # Magnetics
    plot_4 = fig.add_subplot(234)
    plot_func.plot_magnetics_info(plot_4, m_file_data, scan)

    # power/flow economics
    plot_5 = fig.add_subplot(235)
    plot_func.plot_power_info(plot_5, m_file_data, scan)

    # Current drive
    plot_6 = fig.add_subplot(236)
    plot_func.plot_current_drive_info(plot_6, m_file_data, scan)

    fig.subplots_adjust(wspace=0.25)


if __name__ == '__main__':

    # Setup command line arguments
    parser = argparse.ArgumentParser(description="Produce a one-page summary "
                                                 "of the PROCESS MFILE file "
                                                 "for a given scan."
                                                 "For info contact "
                                                 "rich.kemp@ccfe.ac.uk or"
                                                 "james.morris2@ccfe.ac.uk")

    parser.add_argument("-f", metavar='FILENAME', type=str,
                        default="MFILE.DAT",
                        help='specify input filename')

    parser.add_argument("-o", metavar='OUTPUT', type=str,
                        default="proc_plot_out.pdf",
                        help='specify output filename')

    parser.add_argument("-s", "--show", help="show plot as well as saving "
                                             "figure",
                        action="store_true")

    args = parser.parse_args()

    # read MFILE
    m_file = mf.MFile(args.f)

    # create main plot
    all_plot = plt.figure(figsize=(12, 9), dpi=80)

    # run main
    main(all_plot, m_file)

    # show fig if option used
    if args.show:
        plt.show()

    # save figure
    all_plot.savefig(args.o)
