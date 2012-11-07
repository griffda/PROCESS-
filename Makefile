###############################################################
#
#  Makefile for the PROCESS systems code
#
#  $Id::                                                                $
#
#  P J Knight
#
#  Culham Centre for Fusion Energy
#  D3 Culham Science Centre
#  Abingdon
#  Oxfordshire
#  OX14 3DB
#
#  peter.knight@ccfe.ac.uk
#
#  Instructions:
#  =============
#
#  Type 'make' to compile the code.
#
#  Change the machine architecture from the default (FUN) using 'make ARCH=...'
#    (see the list of available architectures below)
#
#  Turn on full debugging using 'make ARCH=... DEBUG=YES'
#    (works for some but not all architectures)
#
#  Type 'make clean' to clean up the directory to allow a full recompilation
#
#  Type 'make latex' to produce a dvi file from the PROCESS manual
#    contained in the *.tex files and associated postscript pictures
#
#  Type 'make doc' to produce full documentation (including web-compatible
#   html files from the autodoc comments embedded in the source code)
#
#  Type 'make tar' to produce a tar file containing all the source files
#    and input files; the file will be called process.tar.gz
#
#  Type 'make archive' to produce an archive of the latest run in this directory
#    (including output files); this produces a file called process_run.tar
#
################# Start of Custom Section #####################

source = \
 avail.f90 \
 buildings.f90 \
 caller.f90 \
 constraint_equations.f90 \
 costs.f90 \
 current_drive.f90 \
 divertor.f90 \
 evaluators.f90 \
 fispact.f90 \
 fwbs.f90 \
 plasma_geometry.f90 \
 global_variables.f90 \
 ife.f90 \
 initial.f90 \
 input.f90 \
 iteration_variables.f90 \
 machine_build.f90 \
 maths_library.f90 \
 numerics.f90 \
 output.f90 \
 pfcoil.f90 \
 physics.f90 \
 plant_power.f90 \
 process.f90 \
 pulse.f90 \
 rfp.f90 \
 safety.f90 \
 scan.f90 \
 startup.f90 \
 stellarator.f90 \
 structure.f90 \
 tfcoil.f90 \
 sctfcoil.f90 \
 vacuum.f90 

object = \
 avail.o \
 buildings.o \
 caller.o \
 constraint_equations.o \
 costs.o \
 current_drive.o \
 divertor.o \
 evaluators.o \
 fispact.o \
 fwbs.o \
 global_variables.o \
 plasma_geometry.o \
 ife.o \
 initial.o \
 input.o \
 iteration_variables.o \
 machine_build.o \
 maths_library.o \
 numerics.o \
 output.o \
 pfcoil.o \
 physics.o \
 plant_power.o \
 process.o \
 pulse.o \
 rfp.o \
 safety.o \
 scan.o \
 startup.o \
 stellarator.o \
 structure.o \
 tfcoil.o \
 sctfcoil.o \
 vacuum.o 

###### Architecture specifics #######
#
# Default = FUN (Fusion Unix Network)
# Alternatives: FUN, JAC
ARCH = FUN
DEBUG = NO

###### Fusion Unix Network - Intel Fortran

FORTRAN_FUN = ifort
FFLAGS_FUN = -cpp 
LFLAGS_FUN = 
LIBS_FUN   = 
ifeq (${DEBUG},YES)
	FFLAGS_FUN = -cpp -g -check bounds -check pointers -check uninit -traceback
endif

###### JET Analysis Cluster - pgf95

FORTRAN_JAC = pgf95
FFLAGS_JAC = -Mpreprocess
LFLAGS_JAC =
LIBS_JAC   = 
ifeq (${DEBUG},YES)
	FFLAGS_JAC = -Mpreprocess -g -C -Mchkptr -traceback
endif

################### End of Custom Section #####################
#
# Leave this bit alone...
#

FORTRAN = ${FORTRAN_${ARCH}}
FFLAGS = ${FFLAGS_${ARCH}}
LFLAGS = ${LFLAGS_${ARCH}}
LIBS   = ${LIBS_${ARCH}}

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

default: process.exe

# object dependencies (usually via modules or header files)

avail.o: global_variables.o output.o
buildings.o: global_variables.o output.o
caller.o: avail.o buildings.o costs.o current_drive.o divertor.o fwbs.o \
  global_variables.o ife.o machine_build.o numerics.o output.o pfcoil.o physics.o \
  plant_power.o plasma_geometry.o pulse.o rfp.o sctfcoil.o startup.o structure.o \
  stellarator.o tfcoil.o vacuum.o
constraint_equations.o: global_variables.o numerics.o
costs.o: global_variables.o output.o
current_drive.o: global_variables.o output.o
divertor.o: global_variables.o output.o
evaluators.o: global_variables.o numerics.o
fispact.o: global_variables.o
fwbs.o: global_variables.o output.o plasma_geometry.o
plasma_geometry.o: global_variables.o
ife.o: avail.o costs.o global_variables.o output.o
initial.o: global_variables.o output.o scan.o stellarator.o
input.o: global_variables.o numerics.o output.o scan.o
iteration_variables.o: global_variables.o numerics.o
machine_build.o: global_variables.o output.o
maths_library.o: 
numerics.o: maths_library.o
output.o:
pfcoil.o: global_variables.o maths_library.o output.o
physics.o: current_drive.o global_variables.o maths_library.o output.o
plant_power.o: fwbs.o global_variables.o output.o
process.o: avail.o buildings.o costs.o current_drive.o divertor.o evaluators.o fwbs.o \
  global_variables.o ife.o input.o machine_build.o numerics.o output.o pfcoil.o \
  physics.o plant_power.o pulse.o rfp.o scan.o sctfcoil.o startup.o stellarator.o \
  structure.o tfcoil.o vacuum.o
pulse.o: global_variables.o maths_library.o output.o physics.o
rfp.o: current_drive.o input.o global_variables.o machine_build.o output.o pfcoil.o \
  physics.o
safety.o: global_variables.o output.o
scan.o: global_variables.o numerics.o output.o
sctfcoil.o: global_variables.o maths_library.o output.o
startup.o: global_variables.o maths_library.o output.o physics.o
stellarator.o: avail.o buildings.o costs.o current_drive.o divertor.o fwbs.o \
  global_variables.o maths_library.o numerics.o output.o physics.o plant_power.o \
  plasma_geometry.o scan.o sctfcoil.o structure.o vacuum.o
structure.o: global_variables.o output.o
tfcoil.o: global_variables.o machine_build.o output.o sctfcoil.o
vacuum.o: global_variables.o output.o

process.exe: $(object)
	$(FORTRAN) $(LFLAGS) -o $@ $(object) $(LIBS)

### Utilities #################

.PHONY: clean tar archive doc

# Clean up directory, to force full recompilation

clean:
	rm -f process.exe *.o *.mod
	rm -f *~
	rm -f autodoc
	rm -f *.aux *.log process.dvi process.toc

# Make a tar distribution of the source and other critical files
# from the current directory
#  (excludes IN.DAT, device.dat for now)

otherfiles = Makefile vardes.html \
             *.tex *.ps process.pdf \
             autodoc.f90 adheader.src adfooter.src

tar:
	rm -f process.tar process.tar.gz
	tar cvf process.tar $(source) $(otherfiles)
	gzip process.tar

# Make a tar archive of the source, input and output files
# from the latest run in the current directory

iofiles = IN.DAT OUT.DAT device.dat

archive:
	rm -f process_run.tar process_run.tar.gz
	tar cvf process_run.tar $(source) $(iofiles) $(otherfiles)
	gzip process_run.tar

# Documentation

autodoc: autodoc.f90
	$(FORTRAN) -o autodoc autodoc.f90

latex: process.tex
	@ latex process
	@ latex process # to make sure cross-references are included
	@ latex process # to make doubly sure cross-references are included
	@ dvipdf process

doc: autodoc latex
	@ cat $(source) | ./autodoc
