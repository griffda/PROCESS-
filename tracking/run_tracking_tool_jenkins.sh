#
#
#  first argument to this script is foler where reference data is saved
#  ./run_tracking_tool_jenkins.sh -f=/home/jenkins_ci/tracking_ref_data/
#

for i in "$@"
do
case $i in
    -f=*|--folder=*)
    ref_folder="${i#*=}"
    mkdir -p ref_folder
    ;;
    --default)
    DEFAULT=YES
    ;;
    *)
            # unknown option
    ;;
esac
done

export PYTHONPATH=$PYTHONPATH:$PWD/utilities/

#tracking_jobs=("baseline_2018")
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
   rm -f  "$track_job""_plot_data.csv"  # delete old plot data
   rm -f  "$track_job""_tracking_data.csv"
  python3.6 generate_tracking_data.py --project="$track_job" --ref_folder="$1" --sep="comma"
 done
   for file in $(ls ) 
   do
     if [[ -f "$file" && `echo $file | grep -c "_MFILE" ` -gt 0 ]]
     then
       rm "$file" 
       echo "$file removed "
     fi
   done


 for track_job in ${tracking_jobs[*]}
 do
   Trk_file="$1""$track_job""_tracking_data.csv" 

   if [[ -f "$Trk_file" ]]
   then
     chmod 666 "$Trk_file"
     cp "$Trk_file" .
   fi

   ref_file="$1""$track_job""_ref_data.csv" 
   if [[ -f "$ref_file" ]]
   then
     chmod 666 "$ref_file"
   fi
 done


}

execute_process_run
clean_tracking_jobs $ref_folder 
