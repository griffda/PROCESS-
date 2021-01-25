#!/usr/bin/bash

SOURCE_FOLDER=$PWD/source/fortran/
FLINTER_EXE=$(which flint)
BADGE_LABEL="Code Quality"
ANYBADGE_EXE=$(which anybadge)
CODE_QUALITY_FILE=$PWD/code_quality.yml

# Check options and if flinter and anybadge are available
if [ -z "${FLINTER_EXE}" ]
then
    echo "ERROR: Could not find 'flint' executable, is python module 'flinter' installed?"
    exit 1
fi

if [ $# -eq 2 ]
then
    ANYBADGE_EXE="python3 $2/anybadge.py"
fi

if [ -z "${ANYBADGE_EXE}" ]
then
    echo "ERROR: Could not find 'anybadge' executable, is python module 'anybadge' installed?"
    exit 1
fi

if [ $# -eq 1 ]
then
    SOURCE_FOLDER=$1
fi

# Run the Linter on the PROCESS Source code
echo "Running flinter on Directory: ${SOURCE_FOLDER}"

SCORE=$(${FLINTER_EXE} score -d 0 ${SOURCE_FOLDER} | cut -d '|' -f 2)
SCORE_INT=$(echo ${SCORE} | cut -d '.' -f 1)
SCORE_PERC=$(bc -l <<<"${SCORE}*10")

${FLINTER_EXE} dump ${SOURCE_FOLDER} ${CODE_QUALITY_FILE}

# Get Badge Colour Based on Score

if [ ${SCORE_INT} -lt 3 ]
then
    BADGE_COLOR="red"
elif [ ${SCORE_INT} -lt 4 ]
then
    BADGE_COLOR="orange"
elif [ ${SCORE_INT} -lt 6 ]
then
    BADGE_COLOR="yellow"
elif [ ${SCORE_INT} -lt 8 ]
then
    BADGE_COLOR="yellowgreen"
else
    BADGE_COLOR="green"
fi

# Generate the badge
${ANYBADGE_EXE} --label="FORTRAN Code Quality" --value="${SCORE_PERC}%" --file=quality.svg --color=${BADGE_COLOR}
