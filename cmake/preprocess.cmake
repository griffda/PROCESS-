# Preprocess PROCESS Fortran sources before they are wrapped
# Author: Timothy Nunn
# Date: 2021-11-09

# Note: CMake verbatim commands insert quotes inappropriately meaning 
# the preprocessor directives must (annoyingly) be inserted directly inside 
# of the command

# Note: when adding new preprocessor directives, you are responsible
# for adding in appropriate ' marks for non-integer/float variables
# e.g. -Dfoo=some string of text 
# will not work and must be -Dfoo="'some string of text'"
# where the "" are for CMake to identify this as a string
# and the '' are then directly inserted into the fortran along
# with the text

MACRO(PREPROCESS)
    LIST(TRANSFORM PROCESS_SRCS PREPEND ${PROCESS_SRC_DIR}/ OUTPUT_VARIABLE PROCESS_SOURCE_FILES_PATH)
    LIST(TRANSFORM PROCESS_SRCS PREPEND ${CMAKE_BINARY_DIR}/ OUTPUT_VARIABLE PREPROCESSED_SOURCE_FILES_PATH)
    LIST(TRANSFORM PROCESS_SRCS PREPEND "preprocess_" OUTPUT_VARIABLE PREPROCESS_TARGET_NAMES)

    FOREACH(target_name source output IN ZIP_LISTS PREPROCESS_TARGET_NAMES PROCESS_SOURCE_FILES_PATH PREPROCESSED_SOURCE_FILES_PATH)
        ADD_CUSTOM_TARGET (
            ${target_name}
            DEPENDS ${output}
        )
        ADD_CUSTOM_COMMAND (
            OUTPUT ${output}
            COMMAND gfortran -E -cpp -DINSTALLDIR="'${CMAKE_SOURCE_DIR}'" -DCOMMSG="'${COMMIT_MSG}'" -Dbranch_name="'${GIT_BRANCH}'" -Dtagno="'${GIT_TAG}'" -Duntracked=${GIT_DIFF} -Ddp=8 ${source} -o ${output}
        )
    ENDFOREACH()
    
ENDMACRO(PREPROCESS)