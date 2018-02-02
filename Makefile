###############################################################
#
#  Makefile for the PROCESS systems code
#
#  J Morris
#
#  Culham Centre for Fusion Energy
#  D3 Culham Science Centre
#  Abingdon
#  Oxfordshire
#  OX14 3DB
#
#  james.morris2@ukaea.uk
#
#  Instructions:
#  =============
#
#  Type 'make' to compile the code.
#
#  Type 'make debug' to compile the code with warning compilation flags.
#
#  Type 'make extra_debug' to compile the code with extra  warning compilation flags.
#
#  Type 'make clean' to clean up the directory to allow a full Fortran recompilation
#
#  Type 'make html' to produce web-compatible html files from the autodoc
#    comments embedded in the source code
#
#  Type 'make userguide' to produce dvi and pdf files from the PROCESS User Guide
#    contained in the *.tex files and associated postscript pictures
#
#  Type 'make doc' to produce both html files and the PROCESS User Guide
#
#  Type 'make tar' to produce a tar file containing all the source files
#    and primary documentation files; the file will be called process.tar.gz
#
#  Type 'make archive' to produce an archive of the latest run in this directory
#    (including input and output files); this produces a file called process_run.tar.gz
#
#  Type 'make dicts' to recreate the dictionary file 'process_dicts.py'
#    used by the Python utilities, and 'gui_dicts.py' used by the GUI.
#
#  Type 'make all' to compile the code, create all the documentation,
#    and build the dictionaries.
#
#  Windows
#  =======
#
#  Type 'make' to make the defaults on Windows
#
#  Type 'make win_dicts' to write the Python dictionaries
#
#  Type 'make win_doc' to write the html documentation for PROCESS
#
#  Type 'make win_clean' to clean up the directory to allow a full Fortran recompilation on Windows
#
###############################################################

SRC = \
 availability.f90 \
 buildings.f90 \
 caller.f90 \
 commons.for \
 comtrn.for \
 costs.f90 \
 costs_2015.f90 \
 constraint_equations.f90 \
 current_drive.f90 \
 divertor.f90 \
 divertor_ode.f90 \
 error_handling.f90 \
 evaluators.f90 \
 fispact.f90 \
 fson_library.f90 \
 fw.f90 \
 global_variables.f90 \
 hcll.f90 \
 hcpb.f90 \
 impurity_radiation.f90 \
 initial.f90 \
 input.f90 \
 iteration_variables.f90 \
 machine_build.f90 \
 maths_library.f90 \
 numerics.f90 \
 ode.f90 \
 output.f90 \
 pfcoil.f90 \
 physics.f90 \
 plant_power.f90 \
 plasma_geometry.f90 \
 plasma_profiles.f90 \
 process.f90 \
 pulse.f90 \
 read_radiation.f90 \
 read_and_get_atomic_data.f90 \
 refprop.f \
 refprop_interface.f90 \
 safety.f90 \
 scan.f90 \
 sctfcoil.f90 \
 startup.f90 \
 stellarator.f90 \
 stellarator_fwbs.f90 \
 structure.f90 \
 superconductors.f90 \
 tfcoil.f90 \
 vacuum.f90

OBJ = \
 availability.o \
 buildings.o \
 caller.o \
 constraint_equations.o \
 costs.o \
 costs_2015.o \
 current_drive.o \
 divertor.o \
 divertor_ode.o \
 error_handling.o \
 evaluators.o \
 fispact.o \
 fson_library.o \
 fw.o \
 global_variables.o \
 hcll.o \
 hcpb.o \
 impurity_radiation.o \
 initial.o \
 input.o \
 iteration_variables.o \
 machine_build.o \
 maths_library.o \
 numerics.o \
 ode.o \
 output.o \
 pfcoil.o \
 physics.o \
 plant_power.o \
 plasma_geometry.o \
 plasma_profiles.o \
 process.o \
 pulse.o \
 read_radiation.o \
 read_and_get_atomic_data.o \
 refprop.o \
 refprop_interface.o \
 safety.o \
 scan.o \
 sctfcoil.o \
 startup.o \
 stellarator.o \
 stellarator_fwbs.o \
 structure.o \
 superconductors.o \
 tfcoil.o \
 vacuum.o

 GVAR = global_variables.f90 numerics.f90

# SRC = $(wildcard *.f90 *.f)
# SRC := $(filter-out autodoc.f90, $(SRC))
# OBJ = $(SRC:.f90=.o)
# DIFF_1 = echo integer :: untracked = %(git diff | Measure-Object -line).Lines% > untracked.info


###### OS specifics #######

ifeq ($(OS),Windows_NT)
	MYROOT_1 = echo character(len=*), parameter :: ROOTDIR = "%cd%" > root.dir
	MYROOT_2 = echo ROOTDIR = "%cd%" > utilities\rootdir.py
	MYCOMMSG_0 = del com.msg
	MYROOT_3_W = echo $substring = git log -1 --format=oneline
	MYROOT_3 = echo character(len=*), parameter :: COMMSG = "%substring.Substring(41)%" > com.msg
	MYTAG_0 = del tag.num
	MYTAG_1 = echo character(len=*), parameter :: tagno = "%git describe%" > tag.num
	DIFF_0 = del untracked.info
	DIFF_1 = echo integer :: untracked = 0 > untracked.info
else
	MYROOT_1 = echo "  character(len=*), parameter :: ROOTDIR = '"`pwd`"'" > root.dir
	MYROOT_2 = echo "ROOTDIR = '"`pwd`"'" > utilities/rootdir.py
	MYCOMMSG_0 = rm -rf com.msg
	MYROOT_3_W =
	MYROOT_3 = echo "  character(len=*), parameter :: COMMSG = '"`git log -1 --format=oneline | cut -c 42-110`"'" > com.msg
	MYTAG_0 = rm -rf tag.num
	MYTAG_1 = echo "  character(len=*), parameter :: tagno = '"`git describe`"'" > tag.num
	DIFF_0 = rm -rf untracked.info
	DIFF_1 = echo "integer :: untracked = "`git diff | wc -l`"" > untracked.info
endif

###########################

FORTRAN = gfortran
FFLAGS = -cpp -g -fbounds-check -fbacktrace -Wconversion -Wunused-dummy-argument -Wunused-parameter
FFLAGS_ALT = -cpp -std=legacy
FFLAGS_LIB = -cpp -g -fbounds-check -fbacktrace

ifeq ($(MAKECMDGOALS),debug)
	override FFLAGS += -Wall
endif

ifeq ($(MAKECMDGOALS),extra_debug)
	override FFLAGS += -Wall -Wextra
endif

.SUFFIXES:
.SUFFIXES: .f .f90 .mod .o

# making objects
.f.o:
	${FORTRAN} ${FFLAGS} -c $*.f -o $*.o

.f90.o:
	${FORTRAN} ${FFLAGS} -c $*.f90 -o $*.o

# making modules
.f90.mod:
	${FORTRAN} ${FFLAGS} -c $*.f90

# default option
default: process.exe

# add additional compiler flags
debug: process.exe

# add even more additional compiler flags
extra_debug: process.exe

# object dependencies (usually via modules or header files)
availability.o: global_variables.o maths_library.o output.o
buildings.o: global_variables.o output.o
caller.o: availability.o buildings.o costs.o costs_2015.o current_drive.o divertor.o divertor_ode.o \
  global_variables.o hcll.o hcpb.o machine_build.o numerics.o output.o pfcoil.o physics.o \
  plant_power.o plasma_geometry.o pulse.o sctfcoil.o startup.o structure.o \
  stellarator.o tfcoil.o vacuum.o
constraint_equations.o: error_handling.o global_variables.o numerics.o
costs.o: buildings.o error_handling.o global_variables.o output.o
costs_2015.o: error_handling.o global_variables.o hcpb.o output.o
current_drive.o: error_handling.o global_variables.o output.o plasma_profiles.o
divertor.o: error_handling.o global_variables.o output.o
divertor_ode.o: global_variables.o input.o maths_library.o  impurity_radiation.o ode.o \
	read_and_get_atomic_data.o  read_radiation.o
error_handling.o: fson_library.o output.o root.dir
evaluators.o: constraint_equations.o error_handling.o global_variables.o numerics.o output.o
fispact.o: global_variables.o output.o
	${FORTRAN} ${FFLAGS_ALT} -c fispact.f90 -o fispact.o
fson_library.o:
	${FORTRAN} ${FFLAGS_LIB} -c fson_library.f90 -o fson_library.o
fw.o : global_variables.o output.o refprop_interface.o
global_variables.o:
hcll.o : fw.o global_variables.o output.o
hcpb.o : fw.o global_variables.o output.o maths_library.o refprop_interface.o
impurity_radiation.o: error_handling.o global_variables.o root.dir plasma_profiles.o
initial.o: error_handling.o global_variables.o output.o scan.o sctfcoil.o stellarator.o
input.o: error_handling.o global_variables.o numerics.o output.o scan.o
iteration_variables.o: error_handling.o global_variables.o numerics.o
machine_build.o: error_handling.o global_variables.o output.o
maths_library.o: global_variables.o
numerics.o: global_variables.o maths_library.o
ode.o:
output.o: global_variables.o numerics.o
pfcoil.o: error_handling.o global_variables.o maths_library.o output.o sctfcoil.o superconductors.o
physics.o: current_drive.o error_handling.o global_variables.o impurity_radiation.o \
  maths_library.o numerics.o output.o plasma_profiles.o
plant_power.o: error_handling.o global_variables.o output.o
plasma_geometry.o: global_variables.o
plasma_profiles.o: error_handling.o global_variables.o maths_library.o
process.o: availability.o buildings.o constraint_equations.o costs.o com.msg current_drive.o \
  divertor.o divertor_ode.o error_handling.o evaluators.o global_variables.o hcll.o hcpb.o \
  impurity_radiation.o input.o machine_build.o maths_library.o numerics.o output.o \
  pfcoil.o physics.o plant_power.o pulse.o scan.o sctfcoil.o startup.o \
  stellarator.o structure.o superconductors.o tag.num untracked.info tfcoil.o vacuum.o
pulse.o: error_handling.o global_variables.o maths_library.o output.o physics.o
read_and_get_atomic_data.o: maths_library.o read_radiation.o
read_radiation.o: impurity_radiation.o maths_library.o
refprop.o:
	${FORTRAN} ${FFLAGS_LIB} -c refprop.f -o refprop.o
refprop_interface.o: error_handling.o refprop.o
safety.o: global_variables.o output.o
scan.o: error_handling.o global_variables.o impurity_radiation.o numerics.o output.o
sctfcoil.o: error_handling.o global_variables.o maths_library.o ode.o output.o superconductors.o
startup.o: global_variables.o maths_library.o output.o physics.o
stellarator.o: availability.o buildings.o costs.o current_drive.o divertor.o error_handling.o \
  stellarator_fwbs.o global_variables.o maths_library.o numerics.o output.o physics.o plant_power.o \
  plasma_geometry.o plasma_profiles.o scan.o sctfcoil.o structure.o vacuum.o
stellarator_fwbs.o: machine_build.o global_variables.o maths_library.o output.o \
  plasma_geometry.o refprop_interface.o
structure.o: global_variables.o output.o
superconductors.o: global_variables.o output.o error_handling.o
tfcoil.o: error_handling.o global_variables.o machine_build.o output.o sctfcoil.o
vacuum.o: error_handling.o global_variables.o output.o

process.exe: $(OBJ)
	$(FORTRAN) -o $@ $(OBJ)

root.dir:
	${MYROOT_1}
	${MYROOT_2}

com.msg:
	${MYCOMMSG_0}
	${MYROOT_3_W}
	${MYROOT_3}

tag.num:
	${MYTAG_0}
	${MYTAG_1}

untracked.info:
	${DIFF_0}
	${DIFF_1}

### Utilities #################
.PHONY: clean cleandoc tar archive doc userguide html dicts untracked.info tag.num com.msg developerguide utilitiesdoc optsolverdoc

# Clean up directory, to force full recompilation
clean:
	rm -f process.exe *.o *.mod
	rm -f root.dir
	rm -f tag.num
	rm -f com.msg
	rm -f untracked.info
	rm -f *~
	rm -f utilities/process_io_lib/process_dicts.py
	rm -f utilities/processgui/dicts/gui_dicts.py
	rm -f *.aux
	rm -rf documentation/html

win_clean:
	del process.exe *.o *.mod
	del utilities\process_io_lib\process_dicts.py
	del utilities\processgui\dicts\gui_dicts.py
	del *.html
	del root.dir
	del com.msg
	del tag.num
	del untracked.info
	rmdir /q /s documentation/html

cleandoc:
	rm -f autodoc
	rm -f *.aux *.log process.dvi process.toc process.lof process.lot process.pdf
	cp -f vardes.html vardes.bak && rm -f *.html

# Make a tar distribution of the source and other critical files
# from the current directory
# (excludes input files IN.DAT, device.dat)
otherfiles = Makefile vardes.html \
             *.tex *.eps process.pdf \
             autodoc.f90 adheader.src adfooter.src \
             data/* \
             utilities/*.py utilities/*.conf utilities/*.json \
             utilities/process_io_lib/*.py utilities/process_io_lib/*.json \
             utilities/processgui/*

tar:
	rm -f process.tar process.tar.gz
	tar cvf process.tar $(SRC) $(otherfiles)
	gzip process.tar

# Make a tar archive of the source, input and output files
# from the latest run in the current directory
iofiles = IN.DAT OUT.DAT PLOT.DAT MFILE.DAT device.dat

archive:
	rm -f process_run.tar process_run.tar.gz
	tar cvf process_run.tar $(SRC) $(iofiles) $(otherfiles)
	gzip process_run.tar

# Documentation
autodoc: autodoc.f90
	$(FORTRAN) -o autodoc autodoc.f90

# Create two versions of the vardes:
# vardes_full.html is the old version that covers the whole code
# vardes.html is based ONLY on global_variables.f90 and numerics.f90
# Warning: some input variables may be declared outside these two files.
# These will not appear in vardes.html
vardes: autodoc
	@ cat $(GVAR) | ./autodoc
	@ mkdir -p documentation/html
	@ mv *.html documentation/html

html: autodoc
	@ cat $(SRC) | ./autodoc
	@ mv vardes.html vardes_full.html
	@ mkdir -p documentation/html
	@ mv *.html documentation/html

userguide: documentation/process.tex
	@ pandoc --standalone documentation/html/vardes.html --output documentation/html/vardes.tex
	@ pdflatex -halt-on-error documentation/html/vardes.tex > documentation/vardes.log || (echo "Error: See documentation/vardes.log"; exit 1)
	@ mv -t documentation vardes.pdf vardes.log
	@ pdflatex -halt-on-error documentation/process > documentation/userguide.log || (echo "Error: See documentation/userguide.log"; exit 1)
	@ pdflatex -halt-on-error documentation/process > documentation/userguide.log || (echo "Error: See documentation/userguide.log" ; exit 1)
	@ mv -t documentation/pdf process.pdf process.log
	@ rm process.lo* process.toc process.out *.aux
	@ rm vardes.out

developerguide: documentation/developerguide.tex
	@ pdflatex -halt-on-error documentation/developerguide > documentation/devguide.log || (echo "Error: See documentation/devguide.log"; exit 1)
	@ pdflatex -halt-on-error documentation/developerguide > documentation/devguide.log || (echo "Error: See documentation/devguide.log" ; exit 1)
	@ mv -t documentation/pdf developerguide.pdf developerguide.log
	@ rm developerguide.lo* developerguide.toc developerguide.out *.aux

utilitiesdoc: documentation/utilitiesdoc.tex
	@ pdflatex -halt-on-error documentation/utilitiesdoc > documentation/utdoc.log || (echo "Error: See documentation/utdoc.log"; exit 1)
	@ pdflatex -halt-on-error documentation/utilitiesdoc > documentation/utdoc.log || (echo "Error: See documentation/utdoc.log" ; exit 1)
	@ mv -t documentation/pdf utilitiesdoc.pdf utilitiesdoc.log
	@ rm  utilitiesdoc.toc utilitiesdoc.out *.aux

optsolverdoc: documentation/optsolverdoc.tex
	@ pdflatex -halt-on-error documentation/optsolverdoc > documentation/optdoc.log || (echo "Error: See documentation/optdoc.log"; exit 1)
	@ bibtex optsolverdoc
	@ pdflatex -halt-on-error documentation/optsolverdoc > documentation/optdoc.log || (echo "Error: See documentation/optdoc.log" ; exit 1)
	@ pdflatex -halt-on-error documentation/optsolverdoc > documentation/optdoc.log || (echo "Error: See documentation/optdoc.log"; exit 1)
	@ mv -t documentation/pdf optsolverdoc.pdf optsolverdoc.log
	@ rm  optsolverdoc.out *.aux optsolverdoc.bbl optsolverdoc.blg


doc: vardes html userguide developerguide utilitiesdoc optsolverdoc

win_doc: autodoc
	@ type $(SRC) | autodoc
	@ rmdir /q /s documentation\html
	@ mkdir documentation\html
	@ copy *.html documentation\html
	@ del *.html

help:
	$(info .)
	$(info ****************************** PROCESS Makefile *********************************)
	$(info .)
	$(info Info: see instructions below for using gfortran compiler on CCFE fusion machines.)
	$(info =>    module unload ifort/10.0.023)
	$(info =>    module load gcc/4.8.2)

dicts: root.dir
	@ touch utilities/process_io_lib/process_dicts.py
	@ mv utilities/process_io_lib/process_dicts.py utilities/process_io_lib/process_dicts.py_prev
	@ echo ''
	@ echo 'Creating Python dictionaries... warnings are usually ignorable!'
	@ echo ''
	@ rm -f *.f90*~
	utilities/create_dicts.py > utilities/process_io_lib/process_dicts.py
	@ chmod 755 utilities/process_io_lib/process_dicts.py
	# @ touch utilities/processgui/dicts/gui_dicts.py
	# @ mv utilities/processgui/dicts/gui_dicts.py utilities/processgui/dicts/gui_dicts.py_prev
	# utilities/processgui/dicts/make_gui_dicts.py > utilities/processgui/dicts/gui_dicts.py
	# @ echo ''
	@ echo 'Dictionaries have been updated'
	@ echo ''

win_dicts: root.dir
	@ echo ''
	@ echo 'Creating Python dictionaries... warnings are usually ignorable!'
	@ echo ''
	python utilities\create_dicts.py > utilities\process_io_lib\process_dicts.py
	@ echo ''
	@ echo 'Dictionaries have been updated'
	@ echo ''

all: process.exe doc dicts
