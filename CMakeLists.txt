# CMake file for PROCESS
# UKAEA

# Specify the minimum version for CMake
cmake_minimum_required(VERSION 3.5)

# Specify CMake root location
include(${CMAKE_ROOT}/Modules/CMakeDetermineCompiler.cmake)
include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)

# Set CMake stadard to be 11
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS  -std=c++11)

# Setup GoogleTest options
set(GTEST_LIBRARY $ENV{GTEST}/lib)
set(GTEST_INCLUDE_DIR $ENV{GTEST}/include)
include_directories(${GTEST_INCLUDE_DIR})
set(GTEST_MAIN_LIBRARY $ENV{GTEST})

# Choose gfortran as compiler
set(CMAKE_Fortran_Compiler gfortran)

# Project's name
project(PROCESS LANGUAGES Fortran CXX)

# enable Fortran
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

# Setup file extension preferences
set(FORT_EXT .F90)
set(PF_EXT .pf)

# Define directories for source etc.
set(FORT_SOURCE_DIR ${CMAKE_SOURCE_DIR}/source/fortran)
set(CPP_SOURCE_DIR ${CMAKE_SOURCE_DIR}/source/cpp)
set(DOC_DIR ${CMAKE_SOURCE_DIR}/documentation)
set(DATA_DIR ${CMAKE_SOURCE_DIR}/data)
set(UTILITIES_DIR ${CMAKE_SOURCE_DIR}/utilities)
set(HTML_DIR ${DOC_DIR}/html)
set(PROCESS_LIB_DIR ${CMAKE_SOURCE_DIR}/lib)
set(TEST_DIR ${CMAKE_SOURCE_DIR}/test_files)
set(GTEST_DIR ${CMAKE_SOURCE_DIR}/test_files/gtest_files)
set(OBJ_DIR ${CMAKE_BINARY_DIR}/CMakeFiles/process.exe.dir/source/fortran)
set(BLD_DIR ${CMAKE_BINARY_DIR})
set(CMAKE_INSTALL_RPATH ${CMAKE_BINARY_DIR})
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
include_directories(${TEST_DIR}/gtest_files)

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
# Turn debug on/off with option -Ddebug=ON
option(debug "debug" OFF)

# Turn debug on/off with option -Ddll=ON
option(dll "dll" ON)

# Setup C and C++ compilers
find_program(CMAKE_C_COMPILER NAMES $ENV{CC} gcc PATHS ENV PATH NO_DEFAULT_PATH)
find_program(CMAKE_CXX_COMPILER NAMES $ENV{CXX} g++ PATHS ENV PATH NO_DEFAULT_PATH)

# Set CMake compiler flags
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath -Wl,/usr/local/lib64")

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

# Set default fortran compiler flags
if(${dll} MATCHES ON)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -g -fcheck=bounds -fbacktrace")
elseif(${dll} MATCHES OFF)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -g -pg -fcheck=bounds -fbacktrace")
endif()

# Set custom compiler flags for particular files
message("\nFlag configuration for files")
file(GLOB FORTRAN_FILES ${FORT_SOURCE_DIR}/*.f90)
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

# The version number.
set (process_VERSION_MAJOR 1)
set (process_VERSION_MINOR 0)

# Set the output folder where your program will be created
set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/bin)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})

# Set library path
set(CMAKE_LIBRARY_PATH /usr/lib)

# Check that pip3 is installed in order to install Ford
find_program(PIP3 NAMES pip3 PATHS ENV PATH NO_DEFAULT_PATH)
if(NOT PIP3)
  message(FATAL_ERROR "Can't find pip3. Please install pip3 and retry.")
endif()

# Set build targets

# Create the PROCESS developer guide PDF from LaTeX
add_custom_target(developerguide
  COMMAND pwd
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/developerguide > ${DOC_DIR}/devguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/devguide.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/developerguide > ${DOC_DIR}/devguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/devguide.log"
  COMMAND ${MOVE_FILE} developerguide.pdf developerguide.log pdf
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.out
  DEPENDS ${DOC_DIR}/developerguide.tex
  WORKING_DIRECTORY ${DOC_DIR}
)

# Create the PROCESS utilities guide PDF from LaTeX
add_custom_target(utilitiesdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/utilitiesdoc > ${DOC_DIR}/utdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/utdoc.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/utilitiesdoc > ${DOC_DIR}/utdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/utdoc.log"
  COMMAND ${MOVE_FILE} utilitiesdoc.pdf utilitiesdoc.log pdf
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.out
  DEPENDS ${DOC_DIR}/utilitiesdoc.tex
  WORKING_DIRECTORY ${DOC_DIR}
)

# Create the PROCESS optimisation solver guide PDF from LaTeX
add_custom_target(optsolverdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/optsolverdoc > ${DOC_DIR}/optdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/optdoc.log"
  COMMAND bibtex optsolverdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/optsolverdoc > ${DOC_DIR}/optdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/optdoc.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/optsolverdoc > ${DOC_DIR}/optdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/optdoc.log"
  COMMAND ${MOVE_FILE} optsolverdoc.pdf optsolverdoc.log pdf
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.out
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.bbl
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolverdoc.blg
  DEPENDS ${DOC_DIR}/optsolverdoc.tex
  WORKING_DIRECTORY ${DOC_DIR}
)

# Create the PROCESS TF coil documentation PDF from LaTeX
add_custom_target(tfdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/tfdoc > ${DOC_DIR}/tfdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/tfdoc.log"
  COMMAND bibtex tfdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/tfdoc > ${DOC_DIR}/tfdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/tfdoc.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/tfdoc > ${DOC_DIR}/tfdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/tfdoc.log"
  COMMAND ${MOVE_FILE} tfdoc.pdf tfdoc.log pdf
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.out
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.bbl
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.blg
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.fdb_latexmk
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/tfdoc.fls
  DEPENDS ${DOC_DIR}/tfdoc.tex
  WORKING_DIRECTORY ${DOC_DIR}
)

# Create test files
add_custom_target(test-builds
  COMMAND make tests
  COMMENT "building test files"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

# Cleans bin directory
add_custom_target(cleandoc
  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_BINARY_DIR}/*
)

# ?
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  ${FORT_SOURCE_DIR}/root.dir
  ${FORT_SOURCE_DIR}/tag.num
  ${FORT_SOURCE_DIR}/untracked.info
  ${UTILITIES_DIR}/process_io_lib/process_dicts.py
  ${UTILITIES_DIR}/processgui/dicts/gui_dicts.py
)

# Create the Python dictionaries from FORTRAN source using FORD
add_custom_target(dicts
  # Install Process-customised FORD
  COMMAND rm -rf ford
  COMMAND git clone https://github.com/jonMaddockUkaea/ford.git ford
  COMMAND pip3 install ford/ --user
  # On Mac, ~/.local/bin is not in PATH, so running Ford using
  # "ford" command fails. Run using python ford.py instead
  # Add the utilities dir to the PYTHONPATH, so FORD can run create_dicts.py
  # Then run FORD on a "fast" md file, which doesn't make graphs or a search index
  # These are wanted for making docs, but not when making dicts quickly
  COMMAND ${CMAKE_COMMAND} -E env "PYTHONPATH=${UTILITIES_DIR}" python3 ford/ford.py ${DOC_DIR}/ford/index_fast.md
  WORKING_DIRECTORY ${PROCESS_LIB_DIR}
  DEPENDS ${CMAKE_SOURCE_DIR}/root.dir
  VERBATIM
)

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

# Set the REFPROP source code directory
set(REFPROP_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/REFPROP)

# Create files for FORTRAN root and install directories
file(WRITE ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: ROOTDIR = \"" ${FORT_SOURCE_DIR} "\"\n")
file(APPEND ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: INSTALLDIR = \"" ${CMAKE_SOURCE_DIR} "\"")

file(WRITE ${REFPROP_SOURCE_DIR}/root.dir "      character(len=*), parameter :: ROOTDIR = \"" ${FORT_SOURCE_DIR} "\"\n")
file(APPEND ${REFPROP_SOURCE_DIR}/root.dir "      character(len=*), parameter :: INSTALLDIR = \"" ${CMAKE_SOURCE_DIR} "\"")

file(WRITE ${UTILITIES_DIR}/rootdir.py "ROOTDIR = '" ${FORT_SOURCE_DIR} "'")
file(WRITE ${CMAKE_SOURCE_DIR}/test_suite/bindir.py "BINDIR = '" ${CMAKE_BINARY_DIR} "'")
file(COPY ${FORT_SOURCE_DIR}/root.dir DESTINATION ${CMAKE_SOURCE_DIR})

# Set output path
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR})

# Set PLASMOD source code directory
set(PLSMOD_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/PLASMOD)

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
list(REMOVE_ITEM all_sources ${FORT_SOURCE_DIR}/autodoc.f90)

# Option to create DLL or single executable
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

  # Link the lapack library for the testing software
  target_link_libraries(process.exe lapack)

  # Setup for Google test software (CPP executable)
  if(DEFINED ENV{GTEST})
    add_executable(process_GTest.exe ${GTEST_DIR}/main_test.cpp)
    target_link_libraries(process_GTest.exe PROCESS_calc_engine)
    target_link_libraries(process_GTest.exe lapack)
    target_link_libraries(process_GTest.exe $ENV{GTEST}/lib/libgtest.a pthread)
  endif()

elseif(${dll} MATCHES OFF)

  add_executable(process.exe ${all_sources})

endif()

# target_compile_features(PROCESS_Testing PRIVATE cxx_std_17)
install(TARGETS process.exe DESTINATION ${CMAKE_SOURCE_DIR})

if(${dll} MATCHES ON)
  set(MOVE_DLL copy ${CMAKE_BINARY_DIR}/${CMAKE_CALC_ENGINE} ${CMAKE_SOURCE_DIR})
elseif(${dll} MATCHES OFF)
  set(MOVE_DLL echo "-- No dll to copy")
endif()

# Post build commands for copying executable and dynamic library to source dir
add_custom_command(TARGET process.exe
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/process.exe ${CMAKE_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E ${MOVE_DLL}
)
