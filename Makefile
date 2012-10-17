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
#  ( TO DO... Type 'make doc' to produce full documentation (including web-compatible
#   html files from the autodoc comments embedded in the source code) )
#
#  Type 'make tar' to produce a tar file containing all the source files
#    and input files; the file will be called process.tar.gz
#
#  Type 'make archive' to produce an archive of the latest run in this directory
#    (including output files); this produces a file called process_run.tar
#
################# Start of Custom Section #####################

source = \
 aachange.f90 \
 aamain.f90   \
 acpow.f90    \
 avail.f90    \
 bldgs.f90    \
 blanket.f90  \
 caller.f90   \
 costs.f90    \
 cudriv.f90   \
 divtmod.f90  \
 evaluators.f90 \
 eqns.f90     \
 fispact.f90  \
 fwbs.f90     \
 geomty.f90   \
 global_variables.90 \
 heatpwr.f90  \
 ife.f90      \
 initial.f90  \
 input.f90    \
 maths_library.f90 \
 numerics.f90 \
 outplas.f90  \
 output.f90   \
 pfcoil.f90   \
 physics.f90  \
 pulse.f90    \
 pwrconv.f90  \
 radialb.f90  \
 rfp.f90      \
 safety.f90   \
 scan.f90     \
 stella.f90   \
 struct.f90   \
 supercond.f90 \
 tfcoil.f90   \
 sctfcoil.f90 \
 vacuum.f90   \
 xc.f90

object = \
 aachange.o   \
 aamain.o     \
 acpow.o      \
 avail.o      \
 bldgs.o      \
 blanket.o    \
 caller.o     \
 costs.o      \
 cudriv.o     \
 divtmod.o    \
 evaluators.o \
 eqns.o       \
 fispact.o    \
 fwbs.o       \
 global_variables.o \
 geomty.o     \
 heatpwr.o    \
 ife.o        \
 initial.o    \
 input.o      \
 maths_library.o \
 numerics.o   \
 outplas.o    \
 output.o     \
 pfcoil.o     \
 physics.o    \
 pulse.o      \
 pwrconv.o    \
 radialb.o    \
 rfp.o        \
 safety.o     \
 scan.o       \
 stella.o     \
 struct.o     \
 supercond.o  \
 tfcoil.o     \
 sctfcoil.o   \
 vacuum.o     \
 xc.o

headers = \
 blanket.h90   \
 bldgcom.h90   \
 bldgvol.h90   \
 build.h90     \
 cost.h90      \
 divrt.h90     \
 estocom.h90   \
 fispact.h90   \
 fwblsh.h90    \
 htpwr.h90     \
 ife.h90       \
 ineq.h90      \
 pfcoil.h90    \
 pulse.h90     \
 pwrcom.h90    \
 rfp.h90       \
 start.h90     \
 stella.h90    \
 struccom.h90  \
 tfcoil.h90    \
 times.h90     \
 torsdat.h90   \
 vaccom.h90    \
 vltcom.h90    

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

aachange.o: 
aamain.o: costs.o cudriv.o evaluators.o global_variables.o input.o numerics.o output.o \
  physics.o scan.o cost.h90 divrt.h90 htpwr.h90 ineq.h90 pfcoil.h90 \
  pwrcom.h90 tfcoil.h90
acpow.o: output.o bldgvol.h90 estocom.h90 htpwr.h90 pwrcom.h90
avail.o: global_variables.o output.o cost.h90 divrt.h90 fwblsh.h90 ife.h90 \
  pulse.h90 rfp.h90
blanket.o: global_variables.o output.o blanket.h90 build.h90 fwblsh.h90 htpwr.h90
bldgs.o: global_variables.o output.o bldgcom.h90 bldgvol.h90 build.h90 fwblsh.h90 \
  htpwr.h90 pfcoil.h90 pwrcom.h90 rfp.h90 struccom.h90 tfcoil.h90 times.h90
caller.o: costs.o cudriv.o global_variables.o numerics.o output.o physics.o ife.h90 \
  rfp.h90 stella.h90
costs.o: global_variables.o output.o blanket.h90 bldgvol.h90 build.h90 \
  cost.h90 divrt.h90 fwblsh.h90 htpwr.h90 ife.h90 pfcoil.h90 pulse.h90 pwrcom.h90 \
  rfp.h90 struccom.h90 tfcoil.h90 times.h90 torsdat.h90
cudriv.o: global_variables.o output.o
divtmod.o: global_variables.o output.o build.h90 divrt.h90
evaluators.o: global_variables.o numerics.o tfcoil.h90 cost.h90 pwrcom.h90 \
  htpwr.h90 divrt.h90
eqns.o: global_variables.o numerics.o build.h90 divrt.h90 htpwr.h90 \
  ife.h90 ineq.h90 pfcoil.h90 pulse.h90 pwrcom.h90 rfp.h90 stella.h90 tfcoil.h90 \
  times.h90 vltcom.h90
fispact.o: global_variables.o blanket.h90 build.h90 fispact.h90 fwblsh.h90 pulse.h90
fwbs.o: global_variables.o output.o blanket.h90 build.h90 cost.h90 divrt.h90 \
  fwblsh.h90 htpwr.h90 tfcoil.h90
geomty.o: global_variables.o build.h90 rfp.h90
heatpwr.o: global_variables.o output.o blanket.h90 cost.h90 fwblsh.h90 \
  htpwr.h90 pfcoil.h90 pwrcom.h90 struccom.h90 tfcoil.h90 times.h90
ife.o: costs.o global_variables.o output.o bldgcom.h90 bldgvol.h90 build.h90 cost.h90 \
  fwblsh.h90 htpwr.h90 ife.h90 pulse.h90 struccom.h90 torsdat.h90 vaccom.h90
initial.o: global_variables.o output.o scan.o blanket.h90 bldgcom.h90 bldgvol.h90 \
  build.h90 cost.h90 divrt.h90 estocom.h90 fwblsh.h90 htpwr.h90 ife.h90 \
  ineq.h90 pfcoil.h90 pulse.h90 pwrcom.h90 rfp.h90 stella.h90 struccom.h90 tfcoil.h90 \
  times.h90 torsdat.h90 vaccom.h90 vltcom.h90
input.o: global_variables.o numerics.o output.o scan.o blanket.h90 bldgcom.h90 \
  bldgvol.h90 build.h90 cost.h90 divrt.h90 estocom.h90 fwblsh.h90 htpwr.h90 \
  ife.h90 ineq.h90 pfcoil.h90 pulse.h90 rfp.h90 stella.h90 tfcoil.h90 times.h90 vaccom.h90
maths_library.o: 
numerics.o: maths_library.o
outplas.o: global_variables.o output.o ineq.h90 rfp.h90 times.h90
output.o:
pfcoil.o: global_variables.o maths_library.o output.o build.h90 pfcoil.h90 times.h90 \
  vltcom.h90
physics.o: cudriv.o global_variables.o maths_library.o output.o build.h90 divrt.h90 \
   pulse.h90 start.h90 times.h90
pulse.o: global_variables.o maths_library.o output.o physics.o build.h90 \
  cost.h90 fwblsh.h90 ineq.h90 pfcoil.h90 pulse.h90 pwrcom.h90 start.h90 times.h90 \
  vltcom.h90
pwrconv.o: global_variables.o output.o bldgvol.h90 build.h90 cost.h90 htpwr.h90 \
  pfcoil.h90 pwrcom.h90 tfcoil.h90 times.h90 vltcom.h90
radialb.o: global_variables.o output.o build.h90 divrt.h90 rfp.h90 tfcoil.h90
rfp.o: cudriv.o input.o global_variables.o output.o physics.o build.h90 divrt.h90 \
  fwblsh.h90 htpwr.h90 pfcoil.h90 pulse.h90 pwrcom.h90 rfp.h90 tfcoil.h90 times.h90 \
  vltcom.h90
safety.o: global_variables.o output.o blanket.h90 build.h90 fispact.h90 fwblsh.h90 \
  pfcoil.h90 tfcoil.h90
scan.o: global_variables.o numerics.o output.o pwrcom.h90 tfcoil.h90 pfcoil.h90 \
  ineq.h90 cost.h90 htpwr.h90 divrt.h90 
sctfcoil.o: global_variables.o maths_library.o output.o build.h90 fwblsh.h90 tfcoil.h90
stella.o: costs.o cudriv.o global_variables.o maths_library.o numerics.o output.o \
  physics.o scan.o blanket.h90 bldgcom.h90 bldgvol.h90 build.h90 cost.h90 divrt.h90 \
  estocom.h90 fwblsh.h90 htpwr.h90 ineq.h90 pfcoil.h90 pulse.h90 pwrcom.h90 stella.h90 \
  struccom.h90 tfcoil.h90 times.h90 torsdat.h90 vaccom.h90 vltcom.h90
struct.o: global_variables.o output.o build.h90 divrt.h90 fwblsh.h90 pfcoil.h90 \
  struccom.h90 tfcoil.h90
supercond.o: output.o tfcoil.h90
tfcoil.o: global_variables.o output.o build.h90 fwblsh.h90 tfcoil.h90
vacuum.o: global_variables.o output.o build.h90 tfcoil.h90 times.h90 \
  torsdat.h90 vaccom.h90
xc.o: global_variables.o numerics.o build.h90 divrt.h90 fwblsh.h90 htpwr.h90 \
  ife.h90 ineq.h90 pfcoil.h90 pulse.h90 rfp.h90 tfcoil.h90 times.h90

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

otherfiles = Makefile var.des *.tex *.ps

tar:
	rm -f process.tar process.tar.gz
	tar cvf process.tar $(source) $(headers) $(otherfiles)
	gzip process.tar

# Make a tar archive of the source, input and output files
# from the latest run in the current directory
#
# Need to move (manually) the output file into process.out first...

archive:
	rm -f process_run.tar process_run.tar.gz
	tar cvf process_run.tar $(source) $(otherfiles) \
		process.out
	gzip process_run.tar

# Documentation

autodoc: autodoc.f90
	$(FORTRAN) -o autodoc autodoc.f90

latex: process.tex
	@ latex process
	@ latex process # to make sure cross-references are included
	@ latex process # to make doubly sure cross-references are included

doc: autodoc latex
	@ cat $(source) | ./autodoc
