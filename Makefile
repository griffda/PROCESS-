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
 beams.f90    \
 bldgs.f90    \
 blanket.f90  \
 caller.f90   \
 costs.f90    \
 cudriv.f90   \
 divtmod.f90  \
 ech.f90      \
 eqns.f90     \
 eqsolv.f90   \
 fispact.f90  \
 fwbs.f90     \
 geomty.f90   \
 heatpwr.f90  \
 ife.f90      \
 induct.f90   \
 initial.f90  \
 input.f90    \
 lwhymod.f90  \
 math.f90     \
 math2.f90    \
 minpac.f90   \
 optimiz.f90  \
 outplas.f90  \
 output.f90   \
 pfcoil.f90   \
 pfscl.f90    \
 physics.f90  \
 pulse.f90    \
 pwrconv.f90  \
 radialb.f90  \
 rfp.f90      \
 safety.f90   \
 stella.f90   \
 struct.f90   \
 svd.f90      \
 supercond.f90 \
 tfcoil.f90   \
 tfcpwr.f90   \
 sctfcoil.f90 \
 vacuum.f90   \
 xc.f90

object = \
 aachange.o   \
 aamain.o     \
 acpow.o      \
 avail.o      \
 beams.o      \
 bldgs.o      \
 blanket.o    \
 caller.o     \
 costs.o      \
 cudriv.o     \
 divtmod.o    \
 ech.o        \
 eqns.o       \
 eqsolv.o     \
 fispact.o    \
 fwbs.o       \
 geomty.o     \
 heatpwr.o    \
 ife.o        \
 induct.o     \
 initial.o    \
 input.o      \
 lwhymod.o    \
 math.o       \
 math2.o      \
 minpac.o     \
 optimiz.o    \
 outplas.o    \
 output.o     \
 pfcoil.o     \
 pfscl.o      \
 physics.o    \
 pulse.o      \
 pwrconv.o    \
 radialb.o    \
 rfp.o        \
 safety.o     \
 stella.o     \
 struct.o     \
 svd.o        \
 supercond.o  \
 tfcoil.o     \
 tfcpwr.o     \
 sctfcoil.o   \
 vacuum.o     \
 xc.o

headers = \
 blanket.h    blanket.h90   \
 bldgcom.h    bldgcom.h90   \
 bldgvol.h    bldgvol.h90   \
 build.h      build.h90     \
 cdriv.h      cdriv.h90     \
 cost.h       cost.h90      \
 cost2.h      cost2.h90     \
 divrt.h      divrt.h90     \
 estocom.h    estocom.h90   \
 fispact.h    fispact.h90   \
 fwblsh.h     fwblsh.h90    \
 htpwr.h      htpwr.h90     \
 ife.h        ife.h90       \
 ineq.h       ineq.h90      \
 labels.h     labels.h90    \
 numer.h      numer.h90     \
 osections.h  osections.h90 \
 param.h      param.h90     \
 pfcoil.h     pfcoil.h90    \
 phydat.h     phydat.h90    \
 pulse.h      pulse.h90     \
 pwrcom.h     pwrcom.h90    \
 rfp.h        rfp.h90       \
 start.h      start.h90     \
 stella.h     stella.h90    \
 struccom.h   struccom.h90  \
 sweep.h      sweep.h90     \
 tfcoil.h     tfcoil.h90    \
 times.h      times.h90     \
 torsdat.h    torsdat.h90   \
 vaccom.h     vaccom.h90    \
 vltcom.h     vltcom.h90    

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
aamain.o: input.o cdriv.h90 cost.h90 divrt.h90 htpwr.h90 ineq.h90 labels.h90 numer.h90 \
  param.h90 pfcoil.h90 phydat.h90 pwrcom.h90 sweep.h90 tfcoil.h90
acpow.o: bldgvol.h90 estocom.h90 htpwr.h90 osections.h90 pwrcom.h90
avail.o: cost.h90 divrt.h90 fwblsh.h90 ife.h90 param.h90 phydat.h90 pulse.h90 rfp.h90
beams.o: cdriv.h90 osections.h90
blanket.o: blanket.h90 build.h90 fwblsh.h90 htpwr.h90 param.h90 phydat.h90
bldgs.o: bldgcom.h90 osections.h90
caller.o: ife.h90 numer.h90 param.h90 phydat.h90 rfp.h90 stella.h90
costs.o: blanket.h90 bldgvol.h90 build.h90 cdriv.h90 cost.h90 cost2.h90 divrt.h90 \
  fwblsh.h90 htpwr.h90 ife.h90 osections.h90 param.h90 pfcoil.h90 phydat.h90 \
  pulse.h90 pwrcom.h90 rfp.h90 struccom.h90 tfcoil.h90 times.h90 torsdat.h90
cudriv.o: cdriv.h90 osections.h90 param.h90 phydat.h90
divtmod.o: build.h90 divrt.h90 osections.h90 param.h90 phydat.h90
ech.o: cdriv.h90 osections.h90
eqns.o: build.h90 cdriv.h90 divrt.h90 htpwr.h90 ife.h90 ineq.h90 numer.h90 param.h90 \
  pfcoil.h90 phydat.h90 pulse.h90 pwrcom.h90 rfp.h90 stella.h90 tfcoil.h90 times.h90 \
  vltcom.h90
eqsolv.o: numer.h90 param.h90
fispact.o: blanket.h90 build.h90 fispact.h90 fwblsh.h90 numer.h90 param.h90 \
  phydat.h90 pulse.h90
fwbs.o: blanket.h90 build.h90 cost.h90 divrt.h90 fwblsh.h90 htpwr.h90 osections.h90 \
  param.h90 phydat.h90 tfcoil.h90
geomty.o: build.h90 param.h90 phydat.h90 rfp.h90
heatpwr.o: blanket.h90 cdriv.h90 cost.h90 fwblsh.h90 htpwr.h90 osections.h90 \
  param.h90 pfcoil.h90 phydat.h90 pwrcom.h90 struccom.h90 tfcoil.h90 times.h90
ife.o: bldgcom.h90 bldgvol.h90 build.h90 cost.h90 fwblsh.h90 htpwr.h90 ife.h90 \
  numer.h90 osections.h90 param.h90 phydat.h90 pulse.h90 struccom.h90 torsdat.h90 \
  vaccom.h90
induct.o: build.h90 osections.h90 param.h90 pfcoil.h90 phydat.h90 tfcoil.h90 times.h90 \
  vltcom.h90
initial.o: blanket.h90 bldgcom.h90 bldgvol.h90 build.h90 cdriv.h90 cost.h90 divrt.h90 \
  estocom.h90 fwblsh.h90 htpwr.h90 ife.h90 ineq.h90 labels.h90 numer.h90 osections.h90 \
  param.h90 pfcoil.h90 phydat.h90 pulse.h90 pwrcom.h90 rfp.h90 stella.h90 struccom.h90 \
  sweep.h90 tfcoil.h90 times.h90 torsdat.h90 vaccom.h90 vltcom.h90
input.o: blanket.h90 bldgcom.h90 bldgvol.h90 build.h90 cdriv.h90 cost.h90 divrt.h90 \
  estocom.h90 fwblsh.h90 htpwr.h90 ife.h90 ineq.h90 labels.h90 numer.h90 osections.h90 \
  param.h90 pfcoil.h90 phydat.h90 pulse.h90 rfp.h90 stella.h90 sweep.h90 tfcoil.h90 \
  times.h90 vaccom.h90
lwhymod.o: cdriv.h90 osections.h90
math2.o: 
math.o: 
minpac.o: 
optimiz.o: cdriv.h90 cost.h90 divrt.h90 htpwr.h90 numer.h90 param.h90 phydat.h90 \
  pwrcom.h90 tfcoil.h90
outplas.o: cdriv.h90 ineq.h90 labels.h90 osections.h90 param.h90 phydat.h90 \
  rfp.h90 times.h90
output.o: ife.h90 osections.h90 param.h90 phydat.h90 rfp.h90 stella.h90 times.h90
pfcoil.o: build.h90 osections.h90 param.h90 pfcoil.h90 phydat.h90 tfcoil.h90 times.h90
pfscl.o: 
physics.o: build.h90 cdriv.h90 divrt.h90 labels.h90 numer.h90 osections.h90 \
  param.h90 phydat.h90 pulse.h90 start.h90 times.h90
pulse.o: build.h90 cdriv.h90 cost.h90 fwblsh.h90 ineq.h90 numer.h90 param.h90 \
  pfcoil.h90 phydat.h90 pulse.h90 pwrcom.h90 start.h90 times.h90 vltcom.h90
pwrconv.o: build.h90 cost.h90 htpwr.h90 osections.h90 param.h90 pfcoil.h90 phydat.h90 \
  pwrcom.h90 tfcoil.h90 times.h90 vltcom.h90
radialb.o: build.h90 cdriv.h90 divrt.h90 osections.h90 param.h90 phydat.h90 rfp.h90 \
  tfcoil.h90
rfp.o: input.o build.h90 cdriv.h90 divrt.h90 fwblsh.h90 htpwr.h90 numer.h90 osections.h90 \
  param.h90 pfcoil.h90 phydat.h90 pulse.h90 pwrcom.h90 rfp.h90 tfcoil.h90 times.h90 \
  vltcom.h90
safety.o: blanket.h90 build.h90 fispact.h90 fwblsh.h90 param.h90 pfcoil.h90 phydat.h90 \
  tfcoil.h90
sctfcoil.o: build.h90 fwblsh.h90 osections.h90 param.h90 phydat.h90 tfcoil.h90
stella.o: blanket.h90 bldgcom.h90 bldgvol.h90 build.h90 cdriv.h90 cost.h90 divrt.h90 \
  estocom.h90 fwblsh.h90 htpwr.h90 ineq.h90 labels.h90 numer.h90 osections.h90 \
  param.h90 pfcoil.h90 phydat.h90 pulse.h90 pwrcom.h90 stella.h90 struccom.h90 \
  sweep.h90 tfcoil.h90 times.h90 torsdat.h90 vaccom.h90 vltcom.h90
struct.o: build.h90 divrt.h90 fwblsh.h90 osections.h90 param.h90 pfcoil.h90 \
  phydat.h90 struccom.h90 tfcoil.h90
supercond.o: osections.h90 tfcoil.h90
svd.o: 
tfcoil.o: build.h90 fwblsh.h90 osections.h90 param.h90 phydat.h90 tfcoil.h90
tfcpwr.o: bldgvol.h90 htpwr.h90 osections.h90 param.h90 phydat.h90 tfcoil.h90
vacuum.o: build.h90 osections.h90 param.h90 phydat.h90 tfcoil.h90 times.h90 \
  torsdat.h90 vaccom.h90
xc.o: build.h90 cdriv.h90 divrt.h90 fwblsh.h90 htpwr.h90 ife.h90 ineq.h90 labels.h90 \
  numer.h90 param.h90 pfcoil.h90 phydat.h90 pulse.h90 rfp.h90 tfcoil.h90 times.h90

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
