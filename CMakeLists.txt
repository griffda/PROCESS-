cmake_minimum_required(VERSION 3.5)
# Specify the minimum version for CMake
#set(CMAKE_VERBOSE_MAKEFILE on)
include(${CMAKE_ROOT}/Modules/CMakeDetermineCompiler.cmake)
include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS  -std=c++11)
# set(GTEST_INCLUDE_DIR $ENV{GTEST}/include)
# set(GTEST_LIBRARY $ENV{GTEST}/lib)
# set(GTEST_MAIN_LIBRARY $ENV{GTEST})
#find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIR})

#include_directories($ENV{GTEST}/include)
#include_directories($(PFUNIT)/include/base.mk)
set(CMAKE_Fortran_Compiler gfortran)
# Project's name
project(PROCESS LANGUAGES Fortran CXX)
# enable Fortran
#enable_language(Fortran)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
 message(STATUS, "Intel ")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
message( FATAL_ERROR "Please unload pgi(module unload pgi) and load gfortran (module load gfortran) " )
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
execute_process(COMMAND gfortran --version OUTPUT_VARIABLE GFORT_COMPILER_VERSION)
 message(STATUS, ${GFORT_COMPILER_VERSION})
#
else()
message( FATAL_ERROR "Unrecognized compiler. Please use  gfortran, ifort" )
endif()

#set(LIB_EXT .o)
#set(LIBS $(PFUNIT)/lib/libpfunit$(LIB_EXT))
#set(OBJ_EXT .o)


set(FORT_EXT .F90)
set(PF_EXT .pf)
#set(GTEST /home/mkumar/googletest/googletest)
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
#
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
# Now define Fortran compiler flags
# add_definitions("-cpp -g -fbounds-check -Wconversion -Wunused-dummy-argument -Wunused-parameter")
# set(ENV{FC} /usr/bin/gfortran)
endif()
#
find_program(CMAKE_C_COMPILER NAMES $ENV{CC} gcc PATHS ENV PATH NO_DEFAULT_PATH)
find_program(CMAKE_CXX_COMPILER NAMES $ENV{CXX} g++ PATHS ENV PATH NO_DEFAULT_PATH)
#find_program(CMAKE_Fortran_COMPILER NAMES $ENV{FC} gfortran PATHS ENV PATH NO_DEFAULT_PATH)
#add_compile_options(-std=c++11)
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath -Wl,/usr/local/lib64")
#message(STATUS $ENV{PATH})
#message(STATUS, ${CMAKE_CXX_COMPILER_ID})
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

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect} -cpp -g -fbounds-check -Wconversion -Wunused-dummy-argument -Wunused-parameter")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -g -fbounds-check -Wconversion -Wunused-dummy-argument -Wunused-parameter")

message("Fortran compiler used in this build will be -> ${CMAKE_Fortran_COMPILER_ID}")  # Must be after project(project_name)
message("Fortran FLags are -> ${CMAKE_Fortran_FLAGS}")  # Must be after project(project_name)
# The version number.
set (process_VERSION_MAJOR 1)
set (process_VERSION_MINOR 0)
# Set the output folder where your program will be created
set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/bin)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})
set(CMAKE_LIBRARY_PATH /usr/lib)
add_custom_target(autodoc
  COMMAND ${FORTRAN} -o autodoc autodoc.f90
  WORKING_DIRECTORY ${FORT_SOURCE_DIR}
  DEPENDS ${FORT_SOURCE_DIR}/autodoc.f90
#  VERBATIM
)
add_custom_target(vardes
  COMMAND cat global_variables.f90 numerics.f90 | ./autodoc
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_SOURCE_DIR}/documentation/html
  COMMAND ${CMAKE_COMMAND} -E remove ${FORT_SOURCE_DIR}/autodoc_data.mod
  COMMAND ${CMAKE_COMMAND} -E remove ${FORT_SOURCE_DIR}/calltree_data.mod
  COMMAND ${MOVE_FILE} *.html ${HTML_DIR}
  COMMENT "Copying html files"
  DEPENDS autodoc
  WORKING_DIRECTORY ${FORT_SOURCE_DIR}
)
add_custom_target(html
  COMMAND cat *.f90 | ./autodoc
  COMMAND ${MOVE_FILE} vardes.html vardes_full.html
  COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_SOURCE_DIR}/documentation/html
  COMMAND ${CMAKE_COMMAND} -E remove ${FORT_SOURCE_DIR}/autodoc_data.mod
  COMMAND ${CMAKE_COMMAND} -E remove ${FORT_SOURCE_DIR}/calltree_data.mod
  COMMAND ${MOVE_FILE} *.html ${HTML_DIR}
  DEPENDS autodoc
  WORKING_DIRECTORY ${FORT_SOURCE_DIR}
)
add_custom_target(userguide
  COMMAND pandoc --standalone ${HTML_DIR}/vardes.html --output ${HTML_DIR}/vardes.tex
  COMMAND pdflatex -halt-on-error ${HTML_DIR}/vardes.tex > ${DOC_DIR}/vardes.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/vardes.log"

  COMMAND pdflatex -halt-on-error ${DOC_DIR}/process.tex > ${DOC_DIR}/userguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/userguide.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/process.tex > ${DOC_DIR}/userguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/userguide.log"
  COMMAND ${MOVE_FILE} -t pdf process.pdf process.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.out
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/vardes.out
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/vardes.aux
  DEPENDS vardes ${DOC_DIR}/process.tex
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(developerguide
  COMMAND pwd
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/developerguide > ${DOC_DIR}/devguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/devguide.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/developerguide > ${DOC_DIR}/devguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/devguide.log"
  COMMAND ${MOVE_FILE} -t pdf developerguide.pdf developerguide.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/developerguide.out
  DEPENDS ${DOC_DIR}/developerguide.tex
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(utilitiesdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/utilitiesdoc > ${DOC_DIR}/utdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/utdoc.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/utilitiesdoc > ${DOC_DIR}/utdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/utdoc.log"
  COMMAND ${MOVE_FILE} -t pdf utilitiesdoc.pdf utilitiesdoc.log
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.aux
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.lof
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.lot
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.toc
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/utilitiesdoc.out
  DEPENDS ${DOC_DIR}/utilitiesdoc.tex
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(optsolverdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/optsolverdoc > ${DOC_DIR}/optdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/optdoc.log"
  COMMAND bibtex optsolverdoc
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/optsolverdoc > ${DOC_DIR}/optdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/optdoc.log"
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/optsolverdoc > ${DOC_DIR}/optdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/optdoc.log"
  COMMAND ${MOVE_FILE} -t pdf optsolverdoc.pdf optsolverdoc.log
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
add_custom_target(doc
  COMMAND pwd
  COMMENT "Creating all documentation files"
  DEPENDS vardes html userguide developerguide utilitiesdoc optsolverdoc
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(all-builds
  COMMAND pwd
  COMMENT "Creating all documentation files"
  DEPENDS doc dicts
#  WORKING_DIRECTORY ${DOC_DIR}
)
#foreach(pf_file ${pf_files})lot
#  message(STATUS, ${pf_file})
#  execute_process(
#  COMMAND $ENV{PFUNIT}/bin/pFUnitParser.py test_utilities.pf test_utilities.F90
#  WORKING_DIRECTORY ${TEST_DIR}
#  )
#endforeach()lot
add_custom_target(test-builds
#include_directories("$(PFUNIT)/include/base.mk")
#  COMMAND ${CMAKE_COMMAND} -E remove ${OBJ_DIR}/process.f90.o
#  COMMAND cp ${BLD_DIR}/*.mod ${TElotST_DIR}
  COMMAND make tests
  COMMENT "building test files"
#  DEPENDS doc dicts
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
add_custom_target(cleandoc
  COMMAND ${CMAKE_COMMAND} -E remove -f ${CMAKE_BINARY_DIR}/*
)
set_property(DIRECTORY PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  ${FORT_SOURCE_DIR}/root.dir
  ${FORT_SOURCE_DIR}/tag.num
  ${FORT_SOURCE_DIR}/untracked.info
  ${UTILITIES_DIR}/process_io_lib/process_dicts.py
  ${UTILITIES_DIR}/processgui/dicts/gui_dicts.py
)
add_custom_target(dicts
  COMMAND ${CMAKE_COMMAND} -E touch process_io_lib/process_dicts.py
  COMMAND mv process_io_lib/process_dicts.py process_io_lib/process_dicts.py_prev
  COMMAND echo ${UTILITIES_DIR}
  COMMAND echo "Creating Python dictionaries... warnings are usually ignorable!"
  COMMAND python create_dicts.py > process_io_lib/process_dicts.py
  COMMAND chmod 755 process_io_lib/process_dicts.py
  WORKING_DIRECTORY ${UTILITIES_DIR}
  DEPENDS ${CMAKE_SOURCE_DIR}/root.dir
  VERBATIM
)
execute_process(
 COMMAND git log -1 --format=oneline OUTPUT_VARIABLE COM_MSG
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(SUBSTRING ${COM_MSG}, 41, 70, COM_MSG_PR)
file(WRITE ${FORT_SOURCE_DIR}/com.msg "  character(len=*), parameter :: COMMSG = \"" ${COM_MSG_PR} "\"")
execute_process(
 COMMAND git describe OUTPUT_VARIABLE TAG_NO
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
file(WRITE ${FORT_SOURCE_DIR}/tag.num "  character(len=*), parameter :: tagno = \"" ${TAG_NO} "\"")

execute_process(
 COMMAND git diff OUTPUT_VARIABLE UNTR_INFO
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(LENGTH "${UNTR_INFO}" UNTR_LN)  # count no of words
file(WRITE ${FORT_SOURCE_DIR}/untracked.info "  integer::untracked = " ${UNTR_LN} )

file(WRITE ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: ROOTDIR = \"" ${FORT_SOURCE_DIR} "\"\n")
file(APPEND ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: INSTALLDIR = \"" ${CMAKE_SOURCE_DIR} "\"")
file(WRITE ${UTILITIES_DIR}/rootdir.py "ROOTDIR = '" ${FORT_SOURCE_DIR} "'")
file(WRITE ${CMAKE_SOURCE_DIR}/test_suite/bindir.py "BINDIR = '" ${CMAKE_BINARY_DIR} "'")
file(COPY ${FORT_SOURCE_DIR}/root.dir DESTINATION ${CMAKE_SOURCE_DIR})
#file:///home/mkumar/process/lib/PLASMOD/e3m.f

set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR})
set(PLSMOD_SOURCE_DIR ${CMAKE_SOURCE_DIR}/lib/PLASMOD)
# The following folder will be included
#include_directories("${PROJECT_SOURCE_DIR}/source")
#include_directories(${FORT_SOURCE_DIR} ${PLSMOD_SOURCE_DIR})
#file(GLOB all_sources ${FORT_SOURCE_DIR}/*.f90 ${FORT_SOURCE_DIR}/*.f ${PLSMOD_SOURCE_DIR}/*.f ${PLSMOD_SOURCE_DIR}/*.f90)
file(GLOB all_sources ${FORT_SOURCE_DIR}/*.f90 ${FORT_SOURCE_DIR}/*.f)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/e3m.f)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/equil.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/grad_func.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/structs.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/transport_solver.f90)
list(APPEND all_sources ${PLSMOD_SOURCE_DIR}/trmodel.f90)
set(autodoc_sources ${all_sources})  # for autodoc
#file(GLOB_RECURSE sources ${FORT_SOURCE_DIR}/root.dir ${FORT_SOURCE_DIR}/*.f90 ${FORT_SOURCE_DIR}/*.for ${FORT_SOURCE_DIR}/*.f)
#link_directories(/usr/lib)
list(REMOVE_ITEM all_sources ${FORT_SOURCE_DIR}/autodoc.f90)
####
list(REMOVE_ITEM all_sources ${FORT_SOURCE_DIR}/process.f90)
add_library(PROCESS_calc_engine SHARED ${all_sources})
set_target_properties(PROCESS_calc_engine PROPERTIES LINKER_LANGUAGE Fortran)
# add the executable
add_executable(process.exe ${FORT_SOURCE_DIR}/process.f90)

set_target_properties(process.exe PROPERTIES LINKER_LANGUAGE Fortran)
#####add_executable(process.exe ${all_sources})  one executable
target_link_libraries(process.exe PROCESS_calc_engine)
target_link_libraries(process.exe lapack)
# Now set target for CPP exe
if(DEFINED ENV{GTEST})
   add_executable(process_GTest.exe ${GTEST_DIR}/main_test.cpp)
   target_link_libraries(process_GTest.exe PROCESS_calc_engine)
   target_link_libraries(process_GTest.exe lapack)
   target_link_libraries(process_GTest.exe $ENV{GTEST}/lib/libgtest.a pthread)
endif()
#   target_compile_features(PROCESS_Testing PRIVATE cxx_std_17)
install(TARGETS process.exe DESTINATION ${CMAKE_SOURCE_DIR})
add_custom_command(TARGET process.exe
 POST_BUILD
 COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/process.exe ${CMAKE_SOURCE_DIR} 
 COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/${CMAKE_CALC_ENGINE} ${CMAKE_SOURCE_DIR} 
)
