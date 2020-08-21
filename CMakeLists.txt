#  CMake file for PROCESS
#  Author : J. Morris (UKAEA)
#  Modified by : J. Maddock (UKAEA), K. Zarebski (UKAEA)
#  Date : last modified 2020-08-18

# Specify the minimum version for CMake
# 3.12 is required due to use of list TRANSFORM commands
cmake_minimum_required(VERSION 3.12)

# Project's name
project(PROCESS LANGUAGES Fortran)

execute_process (
    COMMAND bash -c "git show -s --format=format:%s"
    OUTPUT_VARIABLE COMMIT_MSG
)
execute_process (
    COMMAND bash -c "echo \"$(git diff | wc -l)\"|tr '\n' ' '"
    OUTPUT_VARIABLE GIT_DIFF
)
execute_process (
    COMMAND bash -c "echo \"$(git describe --tags)\"|tr '\n' ' '"
    OUTPUT_VARIABLE GIT_TAG
)
execute_process (
    COMMAND bash -c "echo \"$(git rev-parse --abbrev-ref HEAD)\"|tr '\n' ' '"
    OUTPUT_VARIABLE GIT_BRANCH
)
execute_process (
  COMMAND bash -c "which gfortran | tr -d '[:space:]'"
  OUTPUT_VARIABLE CMAKE_Fortran_COMPILER
)
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
# Set the documentation location
SET(DOC_DIR ${CMAKE_SOURCE_DIR}/documentation)
# Set the utilities directory location
SET(UTILITIES_DIR ${CMAKE_SOURCE_DIR}/utilities)
# Set library directory
set(PROCESS_LIB_DIR ${CMAKE_SOURCE_DIR}/lib)

# Check operating systems and set OS specific options
if(CMAKE_HOST_WIN32)  # For windows system
   MESSAGE(STATUS "[Operating System]: Windows ")
elseif(CMAKE_HOST_APPLE)  # For Apple OS X system
   MESSAGE(STATUS "[Operating System]: macOS ")
   MESSAGE(STATUS "[Binary Location]: ${CMAKE_BINARY_DIR}")
   MESSAGE(STATUS "[Fortran Compiler]: ${CMAKE_Fortran_COMPILER}")
   SET(MOVE_FILE mv)
   SET(COPY_FILE cp -p)
   SET(FORTRAN gfortran)
   SET(CMAKE_CALC_ENGINE libPROCESS_calc_engine.dylib)
elseif(CMAKE_HOST_UNIX)  # For unix/ linux system
   MESSAGE(STATUS "[Operating System]: Linux ")
   MESSAGE(STATUS "[Binary Location]: ${CMAKE_BINARY_DIR}")
   MESSAGE(STATUS "[Fortran Compiler]: ${CMAKE_Fortran_COMPILER}")
   SET(MOVE_FILE mv)
   SET(COPY_FILE cp -p)
   SET(FORTRAN gfortran)
   SET(CMAKE_CALC_ENGINE libPROCESS_calc_engine.so)
endif()

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
add_custom_target(f2py ALL
  DEPENDS ${FORTRAN_INTERFACE_NAME_UNDER}.so)

add_custom_command(OUTPUT ${FORTRAN_INTERFACE_NAME_UNDER}.so
  COMMAND echo \"run f2py\"\; LDFLAGS=-Wl,-rpath=\\$$ORIGIN/.lib f2py -c -m ${FORTRAN_INTERFACE_NAME_UNDER} -I${CMAKE_BINARY_DIR} -L${CMAKE_BINARY_DIR} -lprocess_lib ${FORTRAN_INTERFACE_F90WRAP_PATHS})
# LDFLAGS=-Wl,-rpath=$ORIGIN/.lib embeds the RPATH (run path, list of directories
# to search for shared library dependencies) into the shared object library
# ${FORTRAN_INTERFACE_NAME_UNDER}.so. This allows the dynamic linker to find
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

# Need to change the _fortran import path in fortran.py
# Uses the f90wrap_interface_import_modifier.py script
# TODO Might want to find python interpreter first
# TODO Remove hardcoding of python script: SET as variable above
add_custom_target(modify_so_python_import_path ALL
  COMMAND python ${CMAKE_SOURCE_DIR}/scripts/f90wrap_interface_import_modifier.py ${FORTRAN_INTERFACE_MODULE}
)

add_dependencies(modify_so_python_import_path f90wrap)

# Different OS distrubtions may have different gfortran libaries provided
# by the system, so find gfortran so that it can be distributed and linked
# to ensure consistency.
# TODO: This isn't a very portable way to find gfortran but just searching for
# gfortran fails.
find_library(GFORTRAN_LIB NAMES gfortran libgfortran.so libgfortran.so.5 REQUIRED)

# Make sure the .lib directory exists in the Python package source
file(MAKE_DIRECTORY ${PYTHON_SOURCE_DIR}/.lib/)

# Export the required modules to the Python package
# These are _fortran.cpython....so (f2py) and fortran.py (f90wrap)
# TODO Not sure that the Python package source dir is the right place for these
# Perhaps /bin?
# TODO Should only require one "ALL"?
add_custom_target(export_modules ALL
  COMMAND ${CMAKE_COMMAND} -E copy ${FORTRAN_INTERFACE_MODULE} ${PYTHON_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/${FORTRAN_INTERFACE_NAME_UNDER}*.so ${PYTHON_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/libprocess_lib.so ${PYTHON_SOURCE_DIR}/.lib/
  COMMAND ${CMAKE_COMMAND} -E copy ${GFORTRAN_LIB} ${PYTHON_SOURCE_DIR}/.lib/
)

# Only run once f2py has finished
add_dependencies(export_modules modify_so_python_import_path f2py)

# Create the Python dictionaries from FORTRAN source using FORD
add_custom_target(dicts
  # On Mac, ~/.local/bin is not in PATH, so running Ford using
  # "ford" command fails. Run using python ford.py instead
  # Add the utilities dir to the PYTHONPATH, so FORD can run create_dicts.py
  # Then run FORD on a "fast" md file, which doesn't make graphs or a search index
  # These are wanted for making docs, but not when making dicts quickly
  COMMAND ${CMAKE_COMMAND} -E env "PYTHONPATH=${UTILITIES_DIR}" ford ${DOC_DIR}/ford/index_fast.md
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  VERBATIM
)