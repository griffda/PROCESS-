#
tracking_jobs=("baseline_2018" "baseline_2019")
#
for track_job in ${tracking_jobs[*]}
do
 echo "running $track_job"
 export PYTHONPATH=$PYTHONPATH:$PWD/utilities/
 IN_file=tracking/"$track_job"/"$track_job""_IN.DAT" 
 if [ -e "$IN_file" ]
 then
#   cp tracking/"$track_job"/"$track_job""_IN.DAT" .
   cp "$IN_file" .
   ./process.exe "$track_job""_IN.DAT"
   rm "$track_job"/"$track_job""_IN.DAT" .
   cp "$track_job"" _MFILE.DAT" tracking
   mv "$track_job""*" tracking/"$track_job"
 fi
done
#cp tracking/baseline_2018/baseline_2018_IN.DAT .
#./process.exe baseline_2018_IN.DAT
#rm baseline_2018_IN.DAT
#cp baseline_2018_MFILE.DAT tracking
#mv baseline_2018* tracking/baseline_2018
#
#export PYTHONPATH=$PYTHONPATH:$PWD/utilities/
#cp tracking/baseline_2019/baseline_2019_IN.DAT .
#./process.exe baseline_2019_IN.DAT
#rm baseline_2019_IN.DAT
#cp baseline_2019_MFILE.DAT tracking
#mv baseline_2019* tracking/baseline_2019
#
cd tracking

for track_job in ${tracking_jobs[*]}
do
 python3.6 generate_tracking_data.py --mfile="$track_job""_MFILE.DAT" --jenkins="$track_job""_plot.csv" --sep="comma"
done
#python3.6 generate_tracking_data.py --mfile=baseline_2018_MFILE.DAT --jenkins=baseline_2018_plot.csv --sep="comma"
#python3.6 generate_tracking_data.py --mfile=baseline_2019_MFILE.DAT --jenkins=baseline_2019_plot.csv --sep="comma"
rm *_MFILE.DAT
#rm baseline_2018_MFILE.DAT
#rm baseline_2019_MFILE.DAT
