#!/usr/bin/env python
"""
Short program to create an index file of all README files in the subdirectories

Author: H. Lux (Hanni.Lux@ccfe.ac.uk)

Date : April 2014

Output: Index.txt

Compatible with PROCESS version 274

"""

import argparse
import os


##########################################
#function definitions

def erase_duplicates_from_list(input_list):

    """ erases all duplicates from list """

    return list(set(input_list))



def sort_list(input_list):

    """ sorts list as integers, if possible, else as strings """

    try:
        input_list = [int(x) for x in input_list]
        map2str = True
    except ValueError:
        map2str = False

    input_list.sort()

    if map2str:
        input_list = [str(x) for x in input_list]

    return input_list



def create_suffixlist(suffix):

    """ creates list of individual suffix strings """

    suffixlist = []
    suffixbuffer = suffix.split(',')

    for buf in suffixbuffer:
        if '-' in buf:
            args = buf.split('-')
            #try:
            intlist = range(int(args[0]), int(args[1])+1)
            suffixlist += [str(x) for x in intlist]

        else:
            suffixlist += [buf]

    return suffixlist


def get_all_suffixes(base):

    """ gets suffixes of all subdirectories starting with base """

    suffixlist = []

    for obj in os.listdir('.'):
        if os.path.isdir(obj):
            if base == obj[:len(base)]:
                suffixlist += [obj.replace(base, '')]

    return suffixlist



if __name__ == '__main__':
############################################################
#Usage

    PARSER = argparse.ArgumentParser(description='Program to build an Index.')

    PARSER.add_argument("-b", "--base", default='Run',
                        help="Subdiretories basename, default=Run")

    PARSER.add_argument("-s", "--suffix", default=None,
                        help="List of suffixes e.g. 1-4,6,8,10-12, default=all")

    PARSER.add_argument("-r", "--readme", default='README.txt',
                        help="Name of the file to be indexed")

    PARSER.add_argument("-m", "--mode", default='w', choices=['w', 'a'],
                        help="w-write new (default), a-append")

    PARSER.add_argument("-v", "--verbose", action="store_true",
                        help="Increase verbosity.")

    ARGS = PARSER.parse_args()



    ############################################################


    if ARGS.suffix != None:
        SUFFIXLIST = create_suffixlist(ARGS.suffix)
        SUFFIXLIST = erase_duplicates_from_list(SUFFIXLIST)

    else:
        SUFFIXLIST = get_all_suffixes(ARGS.base)

    SUFFIXLIST = sort_list(SUFFIXLIST)


    INDEXFILE = open('Index.txt', ARGS.mode)

    for suf in SUFFIXLIST:

        try:
            readmefile = open(ARGS.base+suf+'/'+ARGS.readme, 'r')
        except FileNotFoundError:
            if ARGS.verbose:
                print('File %s/%s not found and ignored!'
                      %(ARGS.base+suf, ARGS.readme))
            continue

        INDEXFILE.write(ARGS.base+suf+':\n')
        for line in readmefile:
            INDEXFILE.write(line)

        readmefile.close()
        INDEXFILE.write('\n')

    INDEXFILE.close()


