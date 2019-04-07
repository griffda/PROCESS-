#
tracking_jobs=("baseline_2018" "baseline_2019")
#
execute_process_run()
{
for track_job in ${tracking_jobs[*]}
do
 echo "running $track_job"
 IN_file=tracking/"$track_job"/"$track_job""_IN.DAT" 
 if [ -e "$IN_file" ]
 then
#   cp tracking/"$track_job"/"$track_job""_IN.DAT" .
   cp "$IN_file" .
   export PYTHONPATH=$PYTHONPATH:$PWD/utilities/
   ./process.exe "$track_job""_IN.DAT"
   rm "$track_job""_IN.DAT"
   cp "$track_job""_MFILE.DAT" tracking
   for file in $(ls ) 
   do
     if [[ -f "$file" && `echo $file | grep -c "$track_job" ` -gt 0 ]]
     then
       mv "$file" tracking/"$track_job"
       echo "$file moved to -> tracking/"$track_job" "
     fi
   done
 fi
done
}

clean_tracking_jobs()
{
cd tracking
 for track_job in ${tracking_jobs[*]}
 do
  python3.6 generate_tracking_data.py --mfile="$track_job""_MFILE.DAT" --jenkins="$track_job""_plot.csv" --sep="comma"
 done
   for file in $(ls ) 
   do
     if [[ -f "$file" && `echo $file | grep -c "_MFILE" ` -gt 0 ]]
     then
       rm "$file" 
       echo "$file removed "
     fi
   done
}

execute_process_run
clean_tracking_jobs
