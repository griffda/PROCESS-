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
    `output_data.py`
    `cp process_summary.txt $PREFIX.csv`
else
    echo "need to give prefix as argument!"
fi
