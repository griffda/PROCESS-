#  CMake file for PROCESS
#  J. Morris
#  UKAEA

# Specify the minimum version for CMake
cmake_minimum_required(VERSION 3.5)

# Project's name
project(PROCESS LANGUAGES Fortran)

# The version number
set(process_VERSION_MAJOR 1)
set(process_VERSION_MINOR 0)

# TODO still required?
# Specify CMake root location
include(${CMAKE_ROOT}/Modules/CMakeDetermineCompiler.cmake)
include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)

# Try to set gfortran as compiler, then check if it succeeds
set(CMAKE_Fortran_Compiler gfortran)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  message(STATUS, "Intel ")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  message( FATAL_ERROR "Please unload pgi(module unload pgi) and load gfortran "
          "(module load gfortran) " )
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  execute_process(COMMAND gfortran --version
                  OUTPUT_VARIABLE GFORT_COMPILER_VERSION)
  message(STATUS, ${GFORT_COMPILER_VERSION})
else()
  message( FATAL_ERROR "Unrecognized compiler. Please use  gfortran, ifort" )
endif()

# TODO Can be combined with above if block
# CMake compiler setup
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  # using GCC
  message(STATUS, "GNU GFORTRAN ")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
  # using Intel C++
  message(STATUS, "Intel ")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  # using Visual Studio C++
  message(STATUS, "MSVC ")
endif()

# Setup file extension preferences
set(FORT_EXT .F90)

# Define directories
set(FORT_SOURCE_DIR ${CMAKE_SOURCE_DIR}/source/fortran)
set(DOC_DIR ${CMAKE_SOURCE_DIR}/documentation)
set(DATA_DIR ${CMAKE_SOURCE_DIR}/data)
set(UTILITIES_DIR ${CMAKE_SOURCE_DIR}/utilities)
# TODO html dir used any more?
set(HTML_DIR ${DOC_DIR}/html)
set(PROCESS_LIB_DIR ${CMAKE_SOURCE_DIR}/lib)
# TODO Does this need to be so odd?
set(OBJ_DIR ${CMAKE_BINARY_DIR}/CMakeFiles/process.exe.dir/source/fortran)
set(BIN_DIR ${CMAKE_BINARY_DIR})
# TODO what's this? RPATH
set(CMAKE_INSTALL_RPATH ${CMAKE_BINARY_DIR})
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# Set the output folder where the program will be created
set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/bin)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})

# TODO check through these: what's the difference?
# Set library path
set(CMAKE_LIBRARY_PATH /usr/lib)
# Set output path
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR})
# Set PLASMOD source code directory
set(PLSMOD_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/PLASMOD)
# Set the REFPROP source code directory
set(REFPROP_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/REFPROP)

# Check operating systems and set OS specific options
if(CMAKE_HOST_WIN32)  # For windows system
   message(STATUS, "Windows ")
elseif(CMAKE_HOST_APPLE)  # For Apple OS X system
   message(STATUS, "Apple OS X ")
   message(STATUS,${CMAKE_BINARY_DIR})
   set(MOVE_FILE mv)
   set(COPY_FILE cp -p)
   set(FORTRAN gfortran)
   set(CMAKE_Fortran_COMPILER /usr/local/bin/gfortran)
   set(CMAKE_CALC_ENGINE libPROCESS_calc_engine.dylib)
elseif(CMAKE_HOST_UNIX)  # For unix/ linux system
   message(STATUS, "Linux ")
   message(STATUS,${CMAKE_BINARY_DIR})
   set(MOVE_FILE mv)
   set(COPY_FILE cp -p)
   set(FORTRAN gfortran)
   set(CMAKE_Fortran_COMPILER /usr/bin/gfortran)
   set(CMAKE_CALC_ENGINE libPROCESS_calc_engine.so)
endif()

# Command line options
# TODO does this make sense now with Fortran wrapping?
# Turn debug on/off with option -Ddebug=ON
option(debug "debug" OFF)
# Turn debug on/off with option -Ddll=ON
option(dll "dll" ON)

# TODO ?
# Set CMake compiler flags
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath -Wl,/usr/local/lib64")

# TODO what do these all mean?
# Set default fortran compiler flags
if(${dll} MATCHES ON)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -g -fcheck=bounds -fbacktrace")
elseif(${dll} MATCHES OFF)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -g -pg -fcheck=bounds -fbacktrace")
endif()

# TODO are these separate flags now all really necessary, post-wrapping?
# Set custom compiler flags for particular files
message("\nFlag configuration for files")
file(GLOB FORTRAN_FILES ${FORT_SOURCE_DIR}/*.f90 ${PROCESS_LIB_DIR}/PLASMOD/*.f)
foreach(file ${FORTRAN_FILES})
  if(${file} MATCHES "fson_library.f90")
    message("-- Removed -Wconversion flag for -> " ${file})
    set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-Wunused-dummy-argument -Wunused -fcheck=bounds -fbacktrace")
  elseif(${file} MATCHES "maths_library.f90")
    message("-- Removed -Wunused-dummy-argument flag for --> " ${file})
    set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-Wconversion -Wunused -fcheck=bounds -fbacktrace")
  elseif(${file} MATCHES "superconductors.f90")
    message("-- Removed -Wunused-dummy-argument flag for --> " ${file})
    set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-Wconversion -Wunused -fcheck=bounds -fbacktrace")
  elseif(${file} MATCHES "fispact.f90")
    message("-- Use legacy flag for --> " ${file})
    set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-std=legacy -fcheck=bounds -fbacktrace")
  elseif(${file} MATCHES "TorGA_curgap.f90")
    message("-- Use -fcheck=bounds -fbacktrace flags for --> " ${file})
    set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-fcheck=bounds -fbacktrace -Wno-argument-mismatch")
  elseif(${file} MATCHES "e3m.f")
    message("-- Use -fcheck=bounds -fbacktrace -std=legacy flags for --> " ${file})
    set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-fcheck=bounds -fbacktrace -Wno-argument-mismatch -std=legacy")
  else()
    # If debug mode chosen on command line turn on -Wall and -Wextra
    if(${debug} MATCHES ON)
      set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-Wall -Wextra -Wconversion -Wunused-dummy-argument -fcheck=bounds -fbacktrace -Wno-c-binding-type -Wno-unused-function")
    else()
      set_source_files_properties(${file} PROPERTIES COMPILE_FLAGS "-Wconversion -Wunused-dummy-argument -fcheck=bounds -fbacktrace -Wno-c-binding-type -Wno-unused-function")
    endif()
  endif()
endforeach()
message("-- COMPILE_FLAGS for all other fortran files = -Wconversion -Wunused-dummy-argument -fcheck=bounds -fbacktrace -Wno-c-binding-type -Wno-unused-function")

message("\n")
message("Fortran compiler used in this build will be -> ${CMAKE_Fortran_COMPILER_ID}")  # Must be after project(project_name)
message("Fortran Flags are -> ${CMAKE_Fortran_FLAGS}")  # Must be after project(project_name)

# Setup for Process-customised FORD installation
# Check that pip3 is installed in order to install Ford
find_program(PIP3 NAMES pip3 PATHS ENV PATH NO_DEFAULT_PATH)
if(NOT PIP3)
  message(FATAL_ERROR "Can't find pip3. Please install pip3 and retry.")
endif()

# Set clone and install Ford commands if the ford folder doesn't exist
if(NOT EXISTS ${PROCESS_LIB_DIR}/ford)
  set(CLONE_FORD git clone https://github.com/jonMaddockUkaea/ford.git ${PROCESS_LIB_DIR}/ford)
  set(INSTALL_FORD pip3 install ${PROCESS_LIB_DIR}/ford/ --user)
else()
  set(CLONE_FORD echo "Ford already cloned, skipping...")
  set(INSTALL_FORD echo "Ford already installed, skipping...")
endif()

# Set build targets

# Clean bin directory
# TODO slightly strange command: is this what's wanted?
add_custom_target(cleandoc
  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_BINARY_DIR}/*
)

# TODO What's this?
# ?
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  ${FORT_SOURCE_DIR}/root.dir
  ${FORT_SOURCE_DIR}/tag.num
  ${FORT_SOURCE_DIR}/untracked.info
  ${UTILITIES_DIR}/process_io_lib/process_dicts.py
  ${UTILITIES_DIR}/processgui/dicts/gui_dicts.py
)

# Create the Python dictionaries from FORTRAN source using FORD
# TODO This could be improved. Dictionary use questionable after wrapping
add_custom_target(dicts
  # On Mac, ~/.local/bin is not in PATH, so running Ford using
  # "ford" command fails. Run using python ford.py instead
  # Add the utilities dir to the PYTHONPATH, so FORD can run create_dicts.py
  # Then run FORD on a "fast" md file, which doesn't make graphs or a search index
  # These are wanted for making docs, but not when making dicts quickly
  COMMAND ${CMAKE_COMMAND} -E env "PYTHONPATH=${UTILITIES_DIR}" python3 lib/ford/ford.py ${DOC_DIR}/ford/index_fast.md
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  DEPENDS ${CMAKE_SOURCE_DIR}/root.dir
  VERBATIM
)

# Clone and install Ford, if required
execute_process(COMMAND ${CLONE_FORD})
execute_process(COMMAND ${INSTALL_FORD})

# Git commit status information
# TODO Would it make more sense to move this to Python now, to be put together
# at runtime?
# Get Git log information to put latest commit message into executable
execute_process(
 COMMAND git log -1 --format=oneline OUTPUT_VARIABLE COM_MSG
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(SUBSTRING ${COM_MSG}, 41, 70, COM_MSG_PR)
file(WRITE ${FORT_SOURCE_DIR}/com.msg "  character(len=*), parameter :: COMMSG = \"" ${COM_MSG_PR} "\"")

# Get Git log information to put version number into executable
execute_process(
 COMMAND git describe OUTPUT_VARIABLE TAG_NO
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
file(WRITE ${FORT_SOURCE_DIR}/tag.num "  character(len=*), parameter :: tagno = \"" ${TAG_NO} "\"")

# Get git branch name
execute_process(
 COMMAND git rev-parse --abbrev-ref HEAD OUTPUT_VARIABLE GIT_BRANCH
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
file(WRITE ${FORT_SOURCE_DIR}/branch.name "  character(len=*), parameter :: branch_name = \"" ${GIT_BRANCH} "\"")

# Check Git status for untracked changes and put info into executable
execute_process(
 COMMAND git diff OUTPUT_VARIABLE UNTR_INFO
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(LENGTH "${UNTR_INFO}" UNTR_LN)  # count no of words
file(WRITE ${FORT_SOURCE_DIR}/untracked.info "  integer::untracked = " ${UNTR_LN} )

# TODO This seems like dodgy way of doing this. Still necessary?
# Create files for FORTRAN root and install directories
file(WRITE ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: ROOTDIR = \"" ${FORT_SOURCE_DIR} "\"\n")
file(APPEND ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: INSTALLDIR = \"" ${CMAKE_SOURCE_DIR} "\"")

file(WRITE ${REFPROP_SOURCE_DIR}/root.dir "      character(len=*), parameter :: ROOTDIR = \"" ${FORT_SOURCE_DIR} "\"\n")
file(APPEND ${REFPROP_SOURCE_DIR}/root.dir "      character(len=*), parameter :: INSTALLDIR = \"" ${CMAKE_SOURCE_DIR} "\"")

file(WRITE ${UTILITIES_DIR}/rootdir.py "ROOTDIR = '" ${FORT_SOURCE_DIR} "'")
file(WRITE ${CMAKE_SOURCE_DIR}/test_suite/bindir.py "BINDIR = '" ${CMAKE_BINARY_DIR} "'")
file(COPY ${FORT_SOURCE_DIR}/root.dir DESTINATION ${CMAKE_SOURCE_DIR})

# TODO Put this in a target?
# The following folders will be included
file(GLOB all_sources ${FORT_SOURCE_DIR}/*.f90 ${FORT_SOURCE_DIR}/*.f)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/e3m.f)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/equil.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/grad_func.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/structs.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/transport_solver.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/trmodel.f90)
list(APPEND all_sources ${REFPROP_SOURCE_DIR}/refprop.f)
list(APPEND all_sources ${REFPROP_SOURCE_DIR}/refprop_interface.f90)
set(autodoc_sources ${all_sources})

# Remove autodoc.f90 from compilation list
# TODO Time to delete autodoc completely
list(REMOVE_ITEM all_sources ${FORT_SOURCE_DIR}/autodoc.f90)

# Option to create DLL or single executable
# TODO Always want to create a shared object now we're wrapping
if(${dll} MATCHES ON)

  # Remove process.f90 from compilation list
  list(REMOVE_ITEM all_sources ${FORT_SOURCE_DIR}/process.f90)

  # Create dynamic library/shared object
  add_library(PROCESS_calc_engine SHARED ${all_sources})
  set_target_properties(PROCESS_calc_engine PROPERTIES LINKER_LANGUAGE Fortran)

  # add the executable source file process.f90
  add_executable(process.exe ${FORT_SOURCE_DIR}/process.f90)
  set_target_properties(process.exe PROPERTIES LINKER_LANGUAGE Fortran)

  # Link the dynamic library/shared object with the executable
  target_link_libraries(process.exe PROCESS_calc_engine)

elseif(${dll} MATCHES OFF)

  add_executable(process.exe ${all_sources})

endif()

# TODO What's this install command doing?
install(TARGETS process.exe DESTINATION ${CMAKE_SOURCE_DIR})

# TODO Probably always want a shared object now we're wrapping
if(${dll} MATCHES ON)
  set(MOVE_DLL copy ${CMAKE_BINARY_DIR}/${CMAKE_CALC_ENGINE} ${CMAKE_SOURCE_DIR})
elseif(${dll} MATCHES OFF)
  set(MOVE_DLL echo "-- No dll to copy")
endif()

# Post build commands for copying executable and dynamic library to source dir
# TODO What does this do now?
add_custom_command(TARGET process.exe
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E ${MOVE_DLL}
)
