#!/bin/bash
#shell script to archive process run
# PREFIX needs to be given as an argument

if [ "$#" -gt "0" ]
then

    PREFIX=$@
    `cp IN.DAT $PREFIX.IN.DAT`
    `cp OUT.DAT $PREFIX.OUT.DAT`
    `plot_proc.py`
    `cp SUMMARY.pdf $PREFIX.pdf`
    `create_csv4database.py`
    `cp process_summary.txt $PREFIX.csv`
    NO_LINES=`wc -l $PREFIX.csv`
    if [ "${NO_LINES%% *}" -ne "91" ]
    then
	echo "WARNING: The length of the csv file has changed!"
	echo "         Please consult F. Maviglia for all changes!"
    fi
else
    echo "need to give prefix as argument!"
fi
