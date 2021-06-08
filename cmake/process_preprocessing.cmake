#   PROCESS Preprocessing Variables for CMake
#   Author    :   K. Zarebski (UKAEA)
#   Date      :   last modified 2020-11-05
#
#   Retrieves all required information for setting the preprocessor
#   variables during the PROCESS build

MACRO(FindPreprocessingVars)
    EXECUTE_PROCESS (
        COMMAND bash -c "git -C ${CMAKE_SOURCE_DIR} show -s --format=format:%s"
        OUTPUT_VARIABLE COMMIT_MSG
    )
    STRING(STRIP ${COMMIT_MSG} COMMIT_MSG)
    STRING(REPLACE "#" "[hash]" COMMIT_MSG COMMIT_MSG)
    EXECUTE_PROCESS (
        COMMAND bash -c "echo \"$(git -C ${CMAKE_SOURCE_DIR} diff | wc -l)\"|tr '\n' ' '"
        OUTPUT_VARIABLE GIT_DIFF
    )
    STRING(STRIP ${GIT_DIFF} GIT_DIFF)

    EXECUTE_PROCESS (
        COMMAND bash -c "echo \"$(git -C ${CMAKE_SOURCE_DIR} describe --tags)\"|tr '\n' ' '"
        OUTPUT_VARIABLE GIT_TAG
    )
    STRING(STRIP ${GIT_TAG} GIT_TAG)

    EXECUTE_PROCESS (
        COMMAND bash -c "echo \"$(git -C ${CMAKE_SOURCE_DIR} rev-parse --abbrev-ref HEAD)\"|tr '\n' ' '"
        OUTPUT_VARIABLE GIT_BRANCH
    )
    STRING(STRIP ${GIT_BRANCH} GIT_BRANCH)

    EXECUTE_PROCESS (
    COMMAND bash -c "which gfortran | tr -d '[:space:]'"
    OUTPUT_VARIABLE CMAKE_Fortran_COMPILER
    )
    STRING(STRIP ${CMAKE_Fortran_COMPILER} CMAKE_Fortran_COMPILER)

    EXECUTE_PROCESS(
    COMMAND bash -c "echo \"$(gfortran --version | head -n 1)\"|tr '\n' ' '"
    OUTPUT_VARIABLE CMAKE_Fortran_VERSION
    )
    STRING(STRIP ${CMAKE_Fortran_VERSION} CMAKE_Fortran_VERSION)

    STRING(LENGTH ${CMAKE_Fortran_VERSION} CMAKE_Fortran_VERSION_Length)
    MATH(EXPR LAST_CHARS_FORTRAN_VERSION "${CMAKE_Fortran_VERSION_Length} - 5")
    
    # Full GFortran version number: eg. 8.4.0 or 9.3.0
    STRING(SUBSTRING "${CMAKE_Fortran_VERSION}" "${LAST_CHARS_FORTRAN_VERSION}" "5" CMAKE_Fortran_VERSION_NUMBER)
    # Main GFortran version number: eg. 8 or 9
    STRING(SUBSTRING "${CMAKE_Fortran_VERSION_NUMBER}" "0" "1" CMAKE_Fortran_MAIN_VERSION_NUM)

    IF (CMAKE_Fortran_MAIN_VERSION_NUM LESS 9)
        MESSAGE(FATAL_ERROR "GFortran version 9 or above required")
    ENDIF()

    FOREACH(VAR COMMIT_MSG GIT_DIFF GIT_TAG GIT_BRANCH CMAKE_Fortran_COMPILER CMAKE_Fortran_VERSION)
        IF(NOT VAR)
            MESSAGE(FATAL_ERROR "Failed to obtain value for '${VAR}'")
        ENDIF()
    ENDFOREACH()

    EXECUTE_PROCESS(
        COMMAND bash -c "${PYTHON_EXECUTABLE} -c \"import site, os;print(os.path.join(site.getsitepackages()[0], '${PROJECT_NAME}'))\""
        OUTPUT_VARIABLE PROCESS_MODULE_INSTALL_LOCATION
    )
    STRING(STRIP ${PROCESS_MODULE_INSTALL_LOCATION} PROCESS_MODULE_INSTALL_LOCATION)

    # ---------- Summarise Preprocessor Flags in Output ---------- #
    MESSAGE(STATUS "[Preprocessor Variables]: ")
    MESSAGE(STATUS "\tINSTALLDIR : ${CMAKE_SOURCE_DIR}")
    MESSAGE(STATUS "\tCOMMSG : ${COMMIT_MSG}")
    MESSAGE(STATUS "\tbranch_name : ${GIT_BRANCH}")
    MESSAGE(STATUS "\ttagno : ${GIT_TAG}")
    MESSAGE(STATUS "\tuntracked : ${GIT_DIFF}")
    MESSAGE(STATUS "\tGFortran_Version : ${CMAKE_Fortran_VERSION_NUMBER}")
    # ------------------------------------------------------------ #
    ADD_DEFINITIONS(-DINSTALLDIR="${CMAKE_SOURCE_DIR}")
    ADD_DEFINITIONS(-DCOMMSG="${COMMIT_MSG}")
    ADD_DEFINITIONS(-Dtagno="${GIT_TAG}")
    ADD_DEFINITIONS(-Dbranch_name="${GIT_BRANCH}")
    ADD_DEFINITIONS(-Duntracked=${GIT_DIFF})
ENDMACRO()