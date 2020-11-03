#  CMake file for PROCESS
#  Author : J. Morris (UKAEA)
#  Modified by : J. Maddock (UKAEA), K. Zarebski (UKAEA)
#  Date : last modified 2020-08-18

# Specify the minimum version for CMake
# 3.12 is required due to use of list TRANSFORM commands
cmake_minimum_required(VERSION 3.12)

# Ensure python3 interpreter is used
find_package(Python3)

# Project's name
project(PROCESS LANGUAGES Fortran)

execute_process (
    COMMAND bash -c "git -C ${CMAKE_SOURCE_DIR} show -s --format=format:%s"
    OUTPUT_VARIABLE COMMIT_MSG
)
STRING(STRIP ${COMMIT_MSG} COMMIT_MSG)

execute_process (
    COMMAND bash -c "echo \"$(git -C ${CMAKE_SOURCE_DIR} diff | wc -l)\"|tr '\n' ' '"
    OUTPUT_VARIABLE GIT_DIFF
)
STRING(STRIP ${GIT_DIFF} GIT_DIFF)

execute_process (
    COMMAND bash -c "echo \"$(git -C ${CMAKE_SOURCE_DIR} describe --tags)\"|tr '\n' ' '"
    OUTPUT_VARIABLE GIT_TAG
)
STRING(STRIP ${GIT_TAG} GIT_TAG)

execute_process (
    COMMAND bash -c "echo \"$(git -C ${CMAKE_SOURCE_DIR} rev-parse --abbrev-ref HEAD)\"|tr '\n' ' '"
    OUTPUT_VARIABLE GIT_BRANCH
)
STRING(STRIP ${GIT_BRANCH} GIT_BRANCH)

execute_process (
  COMMAND bash -c "which gfortran | tr -d '[:space:]'"
  OUTPUT_VARIABLE CMAKE_Fortran_COMPILER
)
STRING(STRIP ${CMAKE_Fortran_COMPILER} CMAKE_Fortran_COMPILER)

execute_process(
  COMMAND bash -c "echo \"$(gfortran --version | head -n 1)\"|tr '\n' ' '"
  OUTPUT_VARIABLE CMAKE_Fortran_VERSION
)
STRING(STRIP ${CMAKE_Fortran_VERSION} CMAKE_Fortran_VERSION)

# Questionable stuff to help the compiler handle preprocessor directives
# TODO Review if these directives are necessary now
# -cpp for preprocessor directives
SET(CMAKE_Fortran_FLAGS "-cpp -ffree-line-length-none")
add_definitions(-DINSTALLDIR="${CMAKE_SOURCE_DIR}")
add_definitions(-DCOMMSG="${COMMIT_MSG}")
add_definitions(-Dtagno="${GIT_TAG}")
add_definitions(-Dbranch_name="${GIT_BRANCH}")
add_definitions(-Duntracked=${GIT_DIFF})

MESSAGE(STATUS "------- Configuration -------")

# ---------- Summarise Preprocessor Flags in Output ---------- #
MESSAGE(STATUS "[Preprocessor Variables]: ")
MESSAGE(STATUS "\tINSTALLDIR : ${CMAKE_SOURCE_DIR}")
MESSAGE(STATUS "\tCOMMSG : ${COMMIT_MSG}")
MESSAGE(STATUS "\tbranch_name : ${GIT_BRANCH}")
MESSAGE(STATUS "\ttagno : ${GIT_TAG}")
MESSAGE(STATUS "\tuntracked : ${GIT_DIFF}")
# ------------------------------------------------------------ #

# Define directories
SET(FORTRAN_SOURCE_DIR ${CMAKE_SOURCE_DIR}/source/fortran)
# Set PLASMOD source code directory
SET(PLASMOD_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/PLASMOD)
# Set the REFPROP source code directory
SET(REFPROP_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/REFPROP)
# Set the Python source directory for exporting interface extension modules
SET(PYTHON_SOURCE_DIR ${CMAKE_SOURCE_DIR}/process)
# Set the Python Process IO package directory for Fortran-Python dictionary JSON
# file
SET(PYTHON_SOURCE_IO_DIR ${PYTHON_SOURCE_DIR}/io)
# Set the documentation location
SET(DOC_DIR ${CMAKE_SOURCE_DIR}/documentation)
# Set the utilities directory location
SET(UTILITIES_DIR ${CMAKE_SOURCE_DIR}/utilities)
# Set library directory
set(PROCESS_LIB_DIR ${CMAKE_SOURCE_DIR}/lib)
# Set build scripts directory
set(BUILD_SCRIPTS_DIR ${CMAKE_SOURCE_DIR}/scripts)
# Set create dicts script for creating Python-Fortran dicts JSON
set(CREATE_DICTS_SCRIPT ${BUILD_SCRIPTS_DIR}/create_dicts.py)

# Check operating systems and set OS specific options
if(CMAKE_HOST_APPLE)  # For Apple OS X system
   SET(MOVE_FILE mv)
   SET(COPY_FILE cp -p)
   SET(FORTRAN gfortran)
   SET(LIBRARY_OUTPUT_SUFFIX .dylib)
elseif(CMAKE_HOST_UNIX)  # For unix/ linux system
   SET(MOVE_FILE mv)
   SET(COPY_FILE cp -p)
   SET(FORTRAN gfortran)
   SET(LIBRARY_OUTPUT_SUFFIX .so)
endif()

cmake_host_system_information(RESULT OS_LABEL QUERY OS_NAME)
cmake_host_system_information(RESULT OS_VER QUERY OS_RELEASE)
cmake_host_system_information(RESULT OS_PLAT QUERY OS_PLATFORM)
MESSAGE(STATUS "[System Information]: ")
MESSAGE(STATUS "\tOperating System: ${OS_LABEL} ")
MESSAGE(STATUS "\tVersion: ${OS_VER}")
MESSAGE(STATUS "\tPlatform: ${OS_PLAT}")
MESSAGE(STATUS "[PROCESS Build Information]: ")
MESSAGE(STATUS "\tBinary Location: ${CMAKE_BINARY_DIR}")
MESSAGE(STATUS "\tFortran Compiler: ${CMAKE_Fortran_COMPILER}")
MESSAGE(STATUS "\tFortran Compiler Version: ${CMAKE_Fortran_VERSION}")

# ---------------- GFortran Library Retrieval -----------------#

# Get the location of the GFortran Libraries so they can
# be included with the Python module
# TODO: Ideally we do not want to do this (likely be removed
# when f90wrap is dropped)

execute_process(
	COMMAND bash -c "$(which gcc) -lgfortran -c -v 2>&1 >/dev/null | grep COMPILER_PATH | cut -d '=' -f 2 "
	OUTPUT_VARIABLE GFORTRAN_LIBRARY_DIRS
)
STRING(REGEX MATCHALL "([^\:])+" LIB_LOCS "${GFORTRAN_LIBRARY_DIRS}")
LIST(REMOVE_DUPLICATES LIB_LOCS)
foreach(LOC ${LIB_LOCS})
  FILE(GLOB_RECURSE GFORTSEARCH ${LOC}libgfortran*${LIBRARY_OUTPUT_SUFFIX})
  LIST(LENGTH GFORTSEARCH NRES)
  if(GFORTSEARCH)
    if(NRES GREATER 1)
      LIST(GET GFORTSEARCH 1 GFORTRAN_LIBRARY)
    else()
      SET(GFORTRAN_LIBRARY ${GFORTSEARCH})
    endif()
    break()
  endif()
endforeach()
    
if(NOT GFORTRAN_LIBRARY)
	MESSAGE(FATAL_ERROR "Could not retrieve location of GFortran Libraries from 'gcc -lgfortran'")
endif()

# ------------------------------------------------------------ #

MESSAGE(STATUS "\tGFortran Library Location: ${GFORTRAN_LIBRARY}")

MESSAGE(STATUS "-----------------------------")

# Define files to compile
file(GLOB FORTRAN_LIB_SOURCES ${FORTRAN_SOURCE_DIR}/*.f90 ${FORTRAN_SOURCE_DIR}/*.f)
list(APPEND FORTRAN_LIB_SOURCES ${PLASMOD_SOURCE_DIR}/e3m.f)
list(APPEND FORTRAN_LIB_SOURCES ${PLASMOD_SOURCE_DIR}/equil.f90)
list(APPEND FORTRAN_LIB_SOURCES ${PLASMOD_SOURCE_DIR}/grad_func.f90)
list(APPEND FORTRAN_LIB_SOURCES ${PLASMOD_SOURCE_DIR}/structs.f90)
list(APPEND FORTRAN_LIB_SOURCES ${PLASMOD_SOURCE_DIR}/transport_solver.f90)
list(APPEND FORTRAN_LIB_SOURCES ${PLASMOD_SOURCE_DIR}/trmodel.f90)
list(APPEND FORTRAN_LIB_SOURCES ${REFPROP_SOURCE_DIR}/refprop.f)
list(APPEND FORTRAN_LIB_SOURCES ${REFPROP_SOURCE_DIR}/refprop_interface.f90)

# Define interface source filenames to wrap
list(APPEND FORTRAN_INTERFACE_SOURCE_NAMES global_variables.f90
  init_module.f90
  main_module.f90
  div_kal_vars.f90
  kallenbach_module.f90
  numerics.f90
  scan.f90
  final_module.f90
  error_handling.f90
  availability.f90
  cost_variables.f90
  divertor_variables.f90
  fwbs_variables.f90
  physics.f90
  physics_functions.f90
  physics_variables.f90
  tfcoil_variables.f90
  times_variables.f90
  ife_variables.f90
  ife.f90
  costs.f90
  heat_transport_variables.f90
  buildings_variables.f90
  maths_library.f90
  )
# Create the interface source paths from this
list(TRANSFORM FORTRAN_INTERFACE_SOURCE_NAMES PREPEND ${FORTRAN_SOURCE_DIR}/ OUTPUT_VARIABLE FORTRAN_INTERFACE_SOURCE_PATHS)
# Define the f90wrap filenames
list(TRANSFORM FORTRAN_INTERFACE_SOURCE_NAMES PREPEND f90wrap_ OUTPUT_VARIABLE FORTRAN_INTERFACE_F90WRAP_NAMES)
# Create the f90wrap source paths from this
list(TRANSFORM FORTRAN_INTERFACE_F90WRAP_NAMES PREPEND ${CMAKE_BINARY_DIR}/ OUTPUT_VARIABLE FORTRAN_INTERFACE_F90WRAP_PATHS)

# SET names and paths for the produced Fortran interface modules
# These are the names of the products of f90wrap and f2py
SET(FORTRAN_INTERFACE_NAME "fortran")
SET(FORTRAN_INTERFACE_NAME_UNDER _${FORTRAN_INTERFACE_NAME})
# The "Pythonic" Python module to import as the Fortran interface 
# (e.g. fortran.py), created by f90wrap
SET(FORTRAN_INTERFACE_MODULE ${CMAKE_BINARY_DIR}/${FORTRAN_INTERFACE_NAME}.py)
# The underlying un-Pythonic Python module relied on by fortran.py 
# (e.g. _fortran.py), created by f2py
SET(FORTRAN_INTERFACE_MODULE_UNDER ${FORTRAN_SOURCE_DIR}/${FORTRAN_INTERFACE_NAME_UNDER}.py)

# SET name of Ford's pickled project object (Ford's output)
SET(FORD_OUTPUT "ford_project.pickle")
# Set Python-Fortran dicts filename
SET(DICTS_FILE "python_fortran_dicts.json")

# Build targets
# Stage 1: Compile Fortran sources to a shared library
# (libprocess_lib.so on Linux)
# Make sure the library is able to link to dependencies that are colocated.
# (e.g. set rpath=$ORIGIN)
add_library(process_lib SHARED ${FORTRAN_LIB_SOURCES})
SET_target_properties(process_lib PROPERTIES LINKER_LANGUAGE Fortran)
SET_target_properties(process_lib PROPERTIES BUILD_RPATH $ORIGIN)

# Stage 2: Run f90wrap
add_custom_target(f90wrap DEPENDS ${FORTRAN_INTERFACE_F90WRAP_PATHS})

# Run f90wrap for the above f90wrap target
# This wraps the interface Fortran .f90 files to generate f90wrap_*.f90 files
# It also creates the Fortran interface Python module (fortran.py)
# TODO Add variable for the kind map
add_custom_command(OUTPUT ${FORTRAN_INTERFACE_F90WRAP_PATHS}
  COMMAND f90wrap -k ${FORTRAN_SOURCE_DIR}/kind_map ${FORTRAN_INTERFACE_SOURCE_PATHS} -m ${FORTRAN_INTERFACE_NAME})

# Stage 3: Run f2py
# Builds every time to make sure the libprocess_lib.so file is up-to-date
# TODO Convert .so to non-platform specific
add_custom_target(f2py
	DEPENDS ${FORTRAN_INTERFACE_NAME_UNDER}${LIBRARY_OUTPUT_SUFFIX})

add_custom_command(OUTPUT ${FORTRAN_INTERFACE_NAME_UNDER}${LIBRARY_OUTPUT_SUFFIX}
  COMMAND echo \"run f2py\"\; LDFLAGS=-Wl,-rpath,\\$$ORIGIN/.lib f2py -c -m ${FORTRAN_INTERFACE_NAME_UNDER} -I${CMAKE_BINARY_DIR} -L${CMAKE_BINARY_DIR} -lprocess_lib ${FORTRAN_INTERFACE_F90WRAP_PATHS})
# LDFLAGS=-Wl,-rpath=$ORIGIN/.lib embeds the RPATH (run path, list of directories
# to search for shared library dependencies) into the shared object library
# ${FORTRAN_INTERFACE_NAME_UNDER}${LIBRARY_OUTPUT_SUFFIX}. This allows the dynamic linker to find
# libprocess_lib.so when loading ${FORTRAN_INTERFACE_NAME_UNDER}.so. This 
# in turn allows the package to be run from any location as long as the
# depdendencies that are not provided by the system are included in the .lib
# directory. Note that the echo at the start of this command works around the
# escaping of variables that cmake performs, otherwise the LDFLAGS would be
# treated like a path to an executable.

# Depends on process_lib, because we're linking to the libprocess_lib.so shared
# library with the -lprocess_lib option
# Depends on f90wrap because f90wrap_*.f90 source files are used, which are
# generated by f90wrap
add_dependencies(f2py process_lib f90wrap)

# Create pickled FORD project object: Ford's representation of the Fortran 
# source
add_custom_target(ford DEPENDS ${FORD_OUTPUT})

add_custom_command(OUTPUT ${FORD_OUTPUT}
  # Run FORD on a "fast" md file, which doesn't make graphs or a search index
  # These are wanted for making docs, but not when making dicts quickly
  # This produces the pickled project file
  COMMAND ford ${DOC_DIR}/ford/index_fast.md
)

# Process the pickled Ford project object to create the dicts JSON
add_custom_target(dicts DEPENDS ${DICTS_FILE})

add_custom_command(OUTPUT ${DICTS_FILE}
  # The create_dicts script needs to know the Fortran source dir, the pickled
  # Ford project object and the dicts.json file to output to
  COMMAND ${Python3_EXECUTABLE} ${CREATE_DICTS_SCRIPT} ${FORTRAN_SOURCE_DIR} ${FORD_OUTPUT} 
    ${DICTS_FILE}
)

add_dependencies(dicts ford)

# Need to change the _fortran import path in fortran.py
# Uses the f90wrap_interface_import_modifier.py script
# TODO Might want to find python interpreter first
# TODO Remove hardcoding of python script: SET as variable above
add_custom_target(modify_so_python_import_path ALL
COMMAND ${Python3_EXECUTABLE} ${BUILD_SCRIPTS_DIR}/f90wrap_interface_import_modifier.py 
    ${FORTRAN_INTERFACE_MODULE}
)

add_dependencies(modify_so_python_import_path f90wrap)

# Make sure the .lib directory exists in the Python package source
file(MAKE_DIRECTORY ${PYTHON_SOURCE_DIR}/.lib/)

# Export the required modules to the Python package
# These are _fortran.cpython....so (f2py) and fortran.py (f90wrap)
# Also export the dicts JSON file
add_custom_target(export_modules ALL
  COMMAND ${CMAKE_COMMAND} -E copy ${FORTRAN_INTERFACE_MODULE} ${PYTHON_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/${FORTRAN_INTERFACE_NAME_UNDER}*.so ${PYTHON_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/libprocess_lib${LIBRARY_OUTPUT_SUFFIX} ${PYTHON_SOURCE_DIR}/.lib/
  COMMAND ${CMAKE_COMMAND} -E copy ${GFORTRAN_LIBRARIES_DIR}/libgfortran* ${PYTHON_SOURCE_DIR}/.lib/
  COMMAND ${CMAKE_COMMAND} -E copy ${DICTS_FILE} ${PYTHON_SOURCE_IO_DIR}
)

# Only export once everything has finished
add_dependencies(export_modules modify_so_python_import_path f2py dicts)
