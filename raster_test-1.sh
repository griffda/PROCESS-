#!/bin/bash

#A script to go through aspect ratio to try to break the non-raster 2D sweep
echo ""
echo "--------------------------------"
echo "This script will try to break the"
echo " scan using aspect ratio values"
echo "--------------------------------"


#Writing in the values of the 2nd dimension variable from -/+50% of the default value
input=2.8 #The default value of the 2nd dimension variable
sed -i "s/^isweep_2 = .*$/isweep_2 = 13/g" IN.DAT
sed -i "s/^nsweep_2 = .*$/nsweep_2 = 1/g" IN.DAT
i=1
while [ $i -le 13 ]
  do
  value=$(bc -l <<< "$i * 0.02")
  value=$(bc -l <<< "$input + $value - 0.02")
    if [ $i == 1 ]; then
      sed -i "s/^sweep_2 = .*$/sweep_2 = $value/g" IN.DAT
    else
      sed -i "/^sweep_2 = / s/$/, $value/" IN.DAT
    fi
  i=$(( $i + 1 ))
done


#Making a copy of IN.DAT and moving it to the 2D_scan directory
cp IN.DAT 2D_scan


#Run PROCESS
./process.exe >&1 | tee RUNLOG.DAT


#Make a copy of the outputs and move them to the 2D_scan directory
mv MFILE.DAT 2D_scan
mv OPT.DAT 2D_scan
mv OUT.DAT 2D_scan
mv PLOT.DAT 2D_scan
mv RUNLOG.DAT 2D_scan


echo ""
echo "--------------------------------"
echo "Everything Complete!"
echo "--------------------------------"
echo ""
