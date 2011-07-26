###############################################################
#
#  Makefile for the PROCESS systems code
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
 aachange.f   \
 aamain.f     \
 acpow.f      \
 avail.f      \
 beams.f      \
 bldgs.f      \
 blanket.f    \
 caller.f     \
 costs.f      \
 cudriv.f     \
 divtmod.f    \
 ech.f90      \
 eqns.f       \
 eqsolv.f     \
 fispact.f    \
 fwbs.f       \
 geomty.f     \
 heatpwr.f    \
 ife.f        \
 induct.f     \
 initial.f    \
 input.f      \
 lwhymod.f    \
 math.f       \
 math2.f      \
 minpac.f     \
 optimiz.f    \
 outplas.f    \
 output.f     \
 pfcoil.f     \
 pfscl.f      \
 physics.f    \
 pulse.f      \
 pwrconv.f    \
 radialb.f    \
 rfp.f        \
 safety.f     \
 stella.f     \
 struct.f     \
 svd.f        \
 supercond.f  \
 tfcoil.f     \
 tfcpwr.f     \
 sctfcoil.f   \
 vacuum.f     \
 xc.f

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
 blanket.h   \
 bldgcom.h   \
 bldgvol.h   \
 build.h     \
 cdriv.h     cdriv.h90 \
 cost.h      \
 cost2.h     \
 divrt.h     \
 estocom.h   \
 fispact.h   \
 fwblsh.h    \
 htpwr.h     \
 heattr.h    \
 heatrinp.h  \
 ife.h       \
 ineq.h      \
 labels.h    \
 numer.h     \
 osections.h osections.h90 \
 param.h     \
 pfcoil.h    \
 phydat.h    \
 pfelect.h   \
 pulse.h     \
 pwrcom.h    \
 rfp.h       \
 start.h     \
 stella.h    \
 struccom.h  \
 sweep.h     \
 tfcoil.h    \
 times.h     \
 torsdat.h   \
 vaccom.h    \
 vltcom.h

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
aamain.o: cdriv.h cost.h divrt.h htpwr.h ineq.h labels.h numer.h param.h pfcoil.h \
  phydat.h pwrcom.h sweep.h tfcoil.h
acpow.o: bldgvol.h estocom.h htpwr.h osections.h pwrcom.h
avail.o: cost.h divrt.h fwblsh.h ife.h param.h phydat.h pulse.h rfp.h
beams.o: cdriv.h osections.h
blanket.o: blanket.h build.h fwblsh.h htpwr.h param.h phydat.h
bldgs.o: bldgcom.h osections.h
caller.o: ife.h numer.h param.h phydat.h rfp.h stella.h
costs.o: blanket.h bldgvol.h build.h cdriv.h cost.h cost2.h divrt.h fwblsh.h \
  htpwr.h ife.h osections.h param.h pfcoil.h phydat.h pulse.h pwrcom.h rfp.h \
  struccom.h tfcoil.h times.h torsdat.h
cudriv.o: cdriv.h osections.h param.h phydat.h
divtmod.o: build.h divrt.h osections.h param.h phydat.h
ech.o: cdriv.h osections.h
eqns.o: build.h cdriv.h divrt.h htpwr.h ife.h ineq.h numer.h param.h pfcoil.h \
  phydat.h pulse.h pwrcom.h rfp.h stella.h tfcoil.h times.h vltcom.h
eqsolv.o: numer.h param.h
fispact.o: blanket.h build.h fispact.h fwblsh.h numer.h param.h phydat.h pulse.h
fwbs.o: blanket.h build.h cost.h divrt.h fwblsh.h htpwr.h osections.h param.h \
  phydat.h tfcoil.h
geomty.o: build.h param.h phydat.h rfp.h
heatpwr.o: blanket.h cdriv.h cost.h fwblsh.h htpwr.h osections.h param.h pfcoil.h \
  phydat.h pwrcom.h struccom.h tfcoil.h times.h
ife.o: bldgcom.h bldgvol.h build.h cost.h fwblsh.h htpwr.h ife.h numer.h osections.h \
  param.h phydat.h pulse.h struccom.h torsdat.h vaccom.h
induct.o: build.h osections.h param.h pfcoil.h phydat.h tfcoil.h times.h vltcom.h
initial.o: blanket.h bldgcom.h bldgvol.h build.h cdriv.h cost.h divrt.h estocom.h \
  fwblsh.h htpwr.h ife.h ineq.h labels.h numer.h osections.h param.h pfcoil.h phydat.h \
  pulse.h pwrcom.h rfp.h stella.h struccom.h sweep.h tfcoil.h times.h torsdat.h \
  vaccom.h vltcom.h
input.o: blanket.h bldgcom.h bldgvol.h build.h cdriv.h cost.h divrt.h estocom.h \
  fwblsh.h htpwr.h ife.h ineq.h labels.h numer.h osections.h param.h pfcoil.h \
  phydat.h pulse.h rfp.h stella.h sweep.h tfcoil.h times.h vaccom.h
lwhymod.o: cdriv.h osections.h
math2.o: 
math.o: 
minpac.o: 
optimiz.o: cdriv.h cost.h divrt.h htpwr.h numer.h param.h phydat.h pwrcom.h tfcoil.h
outplas.o: cdriv.h ineq.h labels.h osections.h param.h phydat.h rfp.h times.h
output.o: ife.h osections.h param.h phydat.h rfp.h stella.h times.h
pfcoil.o: build.h osections.h param.h pfcoil.h phydat.h tfcoil.h times.h
pfscl.o: 
physics.o: build.h cdriv.h divrt.h labels.h numer.h osections.h param.h phydat.h \
  pulse.h start.h times.h
pulse.o: build.h cdriv.h cost.h fwblsh.h ineq.h numer.h param.h pfcoil.h phydat.h \
  pulse.h pwrcom.h start.h times.h vltcom.h
pwrconv.o: build.h cost.h htpwr.h osections.h param.h pfcoil.h phydat.h pwrcom.h \
  tfcoil.h times.h vltcom.h
radialb.o: build.h cdriv.h divrt.h osections.h param.h phydat.h rfp.h tfcoil.h
rfp.o: build.h cdriv.h divrt.h fwblsh.h htpwr.h numer.h osections.h param.h pfcoil.h \
  phydat.h pulse.h pwrcom.h rfp.h tfcoil.h times.h vltcom.h
safety.o: blanket.h build.h fispact.h fwblsh.h param.h pfcoil.h phydat.h tfcoil.h
sctfcoil.o: build.h fwblsh.h osections.h param.h phydat.h tfcoil.h
stella.o: blanket.h bldgcom.h bldgvol.h build.h cdriv.h cost.h divrt.h estocom.h \
  fwblsh.h htpwr.h ineq.h labels.h numer.h osections.h param.h pfcoil.h phydat.h \
  pulse.h pwrcom.h stella.h struccom.h sweep.h tfcoil.h times.h torsdat.h vaccom.h \
  vltcom.h
struct.o: build.h divrt.h fwblsh.h osections.h param.h pfcoil.h phydat.h struccom.h \
  tfcoil.h
supercond.o: osections.h tfcoil.h
svd.o: 
tfcoil.o: build.h fwblsh.h osections.h param.h phydat.h tfcoil.h
tfcpwr.o: bldgvol.h htpwr.h osections.h param.h phydat.h tfcoil.h
vacuum.o: build.h osections.h param.h phydat.h tfcoil.h times.h torsdat.h vaccom.h
xc.o: build.h cdriv.h divrt.h fwblsh.h htpwr.h ife.h ineq.h labels.h numer.h param.h \
  pfcoil.h phydat.h pulse.h rfp.h tfcoil.h times.h

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
#  (excludes IN.DAT for now)

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
