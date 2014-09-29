#!/bin/bash

#if django not in path
if [[ $PYTHONPATH != *django* ]]; then
    export PYTHONPATH=$PYTHONPATH':/home/tmiller/django/Django-1.7/build/lib/'
fi

#need to run manage.py from it's containing directory
dirloc=`dirname $0`
cd $dirloc

#if the user doesn't already have the server running
if [[ `ps u` != *"manage.py runserver"* ]]; then
    echo "starting server"
    python manage.py runserver &> /dev/null &
    sleep 5  #probably long enough
fi

firefox 127.0.0.1:8000 &> /dev/null &

