cmake_minimum_required(VERSION 3.6)
# Specify the minimum version for CMake
#set(CMAKE_VERBOSE_MAKEFILE on)

set(FORT_SOURCE_DIR ${CMAKE_SOURCE_DIR}/source/fortran)
set(DOC_DIR ${CMAKE_SOURCE_DIR}/documentation)
set(DATA_DIR ${CMAKE_SOURCE_DIR}/data)
set(UTILITIES_DIR ${CMAKE_SOURCE_DIR}/utilities)
set(HTML_DIR ${DOC_DIR}/html)
set(PROCESS_LIB_DIR ${CMAKE_SOURCE_DIR}/lib)
#
if(CMAKE_HOST_WIN32)  # For windows system
 message(STATUS, "Windows ")
elseif(CMAKE_HOST_UNIX)  # For unix/ linux system
 message(STATUS, "Linux ")
 set(MOVE_FILE mv)
 set(FORTRAN gfortran)
 set(CMAKE_Fortran_COMPILER /usr/bin/gfortran)
# Now define Fortran compiler flags
 add_definitions("-cpp -g -fbounds-check -Wconversion -Wunused-dummy-argument -Wunused-parameter")
# set(ENV{FC} /usr/bin/gfortran)
endif()
#
find_program(CMAKE_C_COMPILER NAMES $ENV{CC} gcc PATHS ENV PATH NO_DEFAULT_PATH)
find_program(CMAKE_CXX_COMPILER NAMES $ENV{CXX} g++ PATHS ENV PATH NO_DEFAULT_PATH)
#find_program(CMAKE_Fortran_COMPILER NAMES $ENV{FC} gfortran PATHS ENV PATH NO_DEFAULT_PATH)
#add_compile_options(-std=c++11)
SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath -Wl,/usr/local/lib64")

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect} -cpp -g -fbounds-check -Wconversion -Wunused-dummy-argument -Wunused-parameter")
# Project's name
project(PROCESS)
# enable Fortran
enable_language(Fortran)
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
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.aux 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.out 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.lof 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.log 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.lot 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/process.toc 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/vardes.out 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/vardes.aux 
  DEPENDS ${DOC_DIR}/process.tex 
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(developerguide
  COMMAND pwd 
  COMMAND pdflatex -halt-on-error ${DOC_DIR}/developerguide > ${DOC_DIR}/devguide.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/devguide.log" 
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
#  COMMAND pdflatex -halt-on-error ${DOC_DIR}/utilitiesdoc > ${DOC_DIR}/utdoc.log || ${CMAKE_COMMAND} -E echo "Error: See documentation/utdoc.log" 
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
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolveroc.aux 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolveroc.lof 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolveroc.log 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolveroc.lot 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolveroc.toc 
  COMMAND ${CMAKE_COMMAND} -E remove ${DOC_DIR}/optsolveroc.out 
  DEPENDS ${DOC_DIR}/optsolverdoc.tex 
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(doc
  COMMAND pwd
  COMMENT "Creating all documentation files"
  DEPENDS vardes html userguide developerguide utilitiesdoc #optsolverdoc
  WORKING_DIRECTORY ${DOC_DIR}
)
add_custom_target(all-builds
  COMMAND pwd
  COMMENT "Creating all documentation files"
  DEPENDS doc dicts
#  WORKING_DIRECTORY ${DOC_DIR}
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
  COMMAND ${CMAKE_COMMAND} -E touch ${UTILITIES_DIR}/process_io_lib/process_dicts.py
  COMMAND mv ${UTILITIES_DIR}/process_io_lib/process_dicts.py ${UTILITIES_DIR}/process_io_lib/process_dicts.py_prev
  COMMAND echo ${UTILITIES_DIR}
  COMMAND echo "Creating Python dictionaries... warnings are usually ignorable!"
  COMMAND ${UTILITIES_DIR}/create_dicts.py > ${UTILITIES_DIR}/process_io_lib/process_dicts.py
  COMMAND chmod 755 ${UTILITIES_DIR}/process_io_lib/process_dicts.py
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  DEPENDS ${CMAKE_SOURCE_DIR}/root.dir
  VERBATIM
)
execute_process(
 COMMAND git log -1 --format=oneline OUTPUT_VARIABLE COM_MSG 
 WORKING_DIRECTORY ${FORT_SOURCE_DIR}
 OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(SUBSTRING ${COM_MSG}, 41, 70, COM_MSG_PR)
file(WRITE ${FORT_SOURCE_DIR}/com.msg "  character(len=*), parameter :: COMMSG = '" ${COM_MSG_PR} "'") 
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
#set(UNTR_INFO 0)
string(LENGTH "${UNTR_INFO}" UNTR_LN)  # count no of words
file(WRITE ${FORT_SOURCE_DIR}/untracked.info "  integer::untracked = " ${UNTR_LN} ) 

file(WRITE ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: ROOTDIR = \"" ${FORT_SOURCE_DIR} "\"\n")
file(APPEND ${FORT_SOURCE_DIR}/root.dir "      character(len=*), parameter :: INSTALLDIR = \"" ${CMAKE_SOURCE_DIR} "\"")
file(WRITE ${UTILITIES_DIR}/rootdir.py "ROOTDIR = '" ${FORT_SOURCE_DIR} "'")
file(COPY ${FORT_SOURCE_DIR}/root.dir DESTINATION ${CMAKE_SOURCE_DIR}/root.dir)
#
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
# add the executable
add_executable(process.exe ${all_sources})
target_link_libraries(process.exe lapack)
#target_compile_features(PROCESS_Testing PRIVATE cxx_std_17)
install(TARGETS process.exe DESTINATION ${CMAKE_SOURCE_DIR})
add_custom_command(TARGET process.exe
 POST_BUILD
 COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/process.exe ${CMAKE_SOURCE_DIR}
)
