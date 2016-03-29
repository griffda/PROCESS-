#!/usr/bin/env python
"""

    Python tool for comparing MFILE and outputting differences

    James Morris
    14/04/15

    CCFE

"""

import scipy
import argparse
import process_io_lib.mfile as mf
from process_io_lib.process_dicts import DICT_VAR_TYPE, DICT_DESCRIPTIONS


class BColors(object):
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'


def main(arg):
    """ Main function for comparing MFILEs

    :param arg: List of arguments
    :return:
    """

    n = 2
    mfile_list = list()
    for item in arg.f:
        mfile_list.append(mf.MFile(filename=item))

    var_list = list()
    missing_vars = list()
    diff_list = list()
    within_list = list()

    key_list = mfile_list[0].data.keys()
    for var in key_list:
        store = True
        for f in mfile_list:
            if var not in f.data.keys():
                store = False
                missing_vars.append(var)

        if store:
            var_list.append(var)

    if arg.save:
        ofile = open("comp.txt", "w")

    for v in var_list:
        if "normres" in v:
            continue

        values = scipy.zeros(n)

        if v not in DICT_VAR_TYPE.keys():
            try:
                eval(mfile_list[0].data[v].get_scan(-1))
            except NameError:
                pass
            except TypeError:
                for m in range(len(mfile_list)):
                    values[m] = (mfile_list[m].data[v].get_scan(-1))
            except SyntaxError:
                pass

        elif DICT_VAR_TYPE[v] == "real_variable" or DICT_VAR_TYPE[v] == "int_variable":
            for m in range(len(mfile_list)):
                values[m] = (mfile_list[m].data[v].get_scan(-1))

        norm_vals = list()
        if values[0] != 0:
            norm_vals = values/values[0]

        if len(norm_vals) >= 1:
            key = v.strip(".").strip(" ")
            if key in DICT_DESCRIPTIONS.keys():
                des = DICT_DESCRIPTIONS[key]
            else:
                des = "-"
            a = norm_vals > 1.0 + arg.acc/100.0
            b = norm_vals < 1.0 - arg.acc/100.0
            if a[1]:
                diff_list.append(v)
                line = BColors.ENDC + v + "\t" + des + "\t" + str(values[0]) + "\t" + \
                       str(values[1]) + "\t" + BColors.FAIL + \
                       str(round((norm_vals[1]-1)*100.0, 2)) + " %"
                wline = v + "\t" + des + "\t" + "\t" + str(values[0]) + "\t" + \
                    str(values[1]) + "\t" + str(round((norm_vals[1]-1)*100.0, 2)) + " %"
                print(line)
                if arg.save:
                    ofile.write(wline + "\n")
            elif b[1]:
                diff_list.append(v)
                line = BColors.ENDC + v + "\t" + des + "\t" + str(values[0]) + "\t" + \
                       str(values[1]) + "\t" + BColors.FAIL + \
                    str(round((norm_vals[1]-1)*100.0, 2)) + " %"
                wline = v + "\t" + des + "\t" + str(values[0]) + "\t" + \
                    str(values[1]) + "\t" + str(round((norm_vals[1]-1)*100.0, 2)) + " %"
                print(line)
                if arg.save:
                    ofile.write(wline + "\n")
            else:
                within_list.append(v)
                line = BColors.ENDC + v + "\t" + des + "\t" + str(values[0]) + "\t" + \
                       str(values[1]) + "\t" + \
                    str(round((norm_vals[1]-1)*100.0, 2)) + " %"
                wline = v + "\t" + des + "\t" + str(values[0]) + "\t" + \
                    str(values[1]) + "\t" + str(round((norm_vals[1]-1)*100.0, 2)) + " %"
                if arg.verbose:
                    print(line)
                    ofile.write(wline + "\n")

    if arg.save:
        ofile.close()
    # elif DICT_VAR_TYPE[v] == "real_array":
    # if DICT_VAR_TYPE[v] == "int_array"


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Produce a comparison "
                                                 "between two PROCESS "
                                                 "MFILEs. User Can speicify "
                                                 "level of differences to show"
                                                 "For info contact "
                                                 "james.morris2@ccfe.ac.uk")

    parser.add_argument('-f', metavar='f', type=str, nargs='+',
                        help='Files to compare')

    parser.add_argument("-s", "--save", help="Save output to file called comp.txt",
                        action="store_true")

    parser.add_argument('--acc', type=float, default=5.0)

    parser.add_argument('--verbose', type=float, default=0.0)

    args = parser.parse_args()

    main(args)
    print(BColors.ENDC)

