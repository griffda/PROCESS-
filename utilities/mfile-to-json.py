#!/usr/bin/env python
"""

Script to dump whole MFILE to file.

J. Morris
UKAEA
27.02.20

"""

import argparse
from process_io_lib.mfile import MFile

RADIAL_BUILD = [
    "bore", "ohcth", "precomp", "gapoh", "tfcth", "deltf", "thshield", "gapds",
    "ddwi", "shldith", "vvblgap", "blnkith", "fwith", "scrapli", "rminor", 
    "rminor", "scraplo", "fwoth", "blnkoth", "vvblgap", "shldoth", "ddwi", 
    "gapsto", "thshield", "tftsgap", "tfthko"
]

VERTICAL_BUILD = [
    "tfcth", "tftsgap", "thshield", "vgap2", "ddwi", "shldlth", "divfix", 
    "vgap", "rminor*kappa", "rminor*kappa", "vgaptop", "fwtth", "blnktth", 
    "vvblgap", "shldtth", "ddwi", "vgap2", "thshield", "tftsgap", "tfcth"
]

if __name__ == "__main__":
    
    # Setup command line arguments
    parser = argparse. \
        ArgumentParser(description="Produces a JSON file form an MFILE.  "
        "There are built in options for certain variable sets, by default "
        "exports entire MFILE by default use help for other options"
        "For info contact james.morris2@ukaea.uk.")

    parser.add_argument("-f", metavar='filename', type=str,
                        default="", help='specify MFILE file path')

    parser.add_argument("-n", type=int, help="Which scan to plot? (-1=last, 0=all)",
                        default=0)

    parser.add_argument("--radial_build", help="export radial build",
                        action="store_true")

    parser.add_argument("--vertical_build", help="export vertical build",
                        action="store_true")

    parser.add_argument("--all_build", help="export radial and vertical build",
                        action="store_true")
    
    parser.add_argument("--verbose", help="export more info with variables",
                        action="store_true")


    args = parser.parse_args()

    if args.f == '':
        parser.print_help()
        parser.exit(1)

    process_mfile = MFile(filename=args.f)

    if args.radial_build:
        process_mfile.write_to_json(keys_to_write=RADIAL_BUILD, scan=args.n, 
                                    verbose=args.verbose)
    elif args.vertical_build:
        process_mfile.write_to_json(keys_to_write=VERTICAL_BUILD, scan=args.n, 
                                    verbose=args.verbose)
    elif args.all_build:
        process_mfile.write_to_json(keys_to_write=RADIAL_BUILD+VERTICAL_BUILD, 
                                    scan=args.n, verbose=args.verbose)
    else:
        process_mfile.write_to_json(scan=args.n, verbose=args.verbose)

    