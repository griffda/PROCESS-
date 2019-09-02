#!/bin/bash
# A script to download and build PROCESS in one go


#Strings to check the commands successfully complete and current directory
dir=$(pwd)
comp_1=$'-- Build files have been written to: '$dir'/build'
comp_2=$'Built target process_GTest.exe'
comp_3=$'Built target dicts'


# Check to see if the build directory and tmp.1 file exist and delete them
if [ -d build ]; then
  rm -r build
fi
if [ -a tmp.1 ]; then
  rm tmp.1
fi


# Running the set-up stages
echo -e '\n--------------------------------'
echo 'This script will build PROCESS'
echo 'Run inside the PROCESS directory'
echo -e '--------------------------------\n'


# Asking for Initial run
echo 'Would you like to run PROCESS at the end of set-up? y/n'
read answer


# Running the first build command
echo -e '\n---------------------------'
echo 'Running 'cmake -H. -Bbuild''
echo -e '---------------------------\n'
touch tmp.1
cmake -H. -Bbuild | tee tmp.1
end_1=$(tail --lines=1 tmp.1)
rm tmp.1


# Running the second command if the first one ends with the correct output
if [[ $end_1 == $comp_1 ]]; then
  echo -e '\n-----------------------------'
  echo 'Running 'cmake --build build''
  echo -e '-----------------------------\n'
  touch tmp.2
  cmake --build build | tee tmp.2
  end_2=$(tail --bytes=31 tmp.2)
  rm tmp.2
else
  echo -e '\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  echo '!!!'cmake -H. -Bbuild' FAILED!!!'
  echo -e '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'
  exit 1
fi


# Creating the python dictionaries
if [[ $end_2 == $comp_2 ]]; then
  echo -e '\n--------------------------------------------'
  echo 'Running 'cmake --build build --target dicts''
  echo -e '--------------------------------------------\n'
  touch tmp.3
  cmake --build build --target dicts | tee tmp.3
  end_3=$(tail --lines=1 tmp.3)
  rm tmp.3
  rm -rf build
else
  echo -e '\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  echo '!!!'cmake --build build' FAILED!!!'
  echo -e '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'
  exit 1
fi


# Making the python path to the utilities directories
if [[ $end_3 == $comp_3 ]]; then
  echo -e '\n-----------------------------------------------'
  echo 'Setting the python path to the utilities directory'
  echo -e '-----------------------------------------------\n'
  export PYTHONPATH=$PYTHONPATH:{$dir}/utilities/
else
  echo -e '\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  echo '!!!cmake --build build --target dicts FAILED!!!'
  echo -e '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'
  exit 1
fi


echo -e '--------------------'
echo 'Set-up Complete!'
echo -e '--------------------\n'


#Initial run
if [[ $answer == 'y' ]]; then
  echo -e '--------------------'
  echo 'Initial PROCESS run'
  echo -e '--------------------\n'
  ./process.exe
else
  exit 1
fi