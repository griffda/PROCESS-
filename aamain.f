CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C                                                                     C
C  Source Code Control System                                         C
C                                                                     S
C  Information header for the PROCESS systems code modules            C
C                                                                     C
C  P.J.Knight 22 May 1992                                             S
C                                                                     C
C                                                                     C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C
C  Module         : $Id$
C
C  Module name    : $RCSfile: aamain.f,v $
C  Version no.    : $Revision: 3.21 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      PROGRAM PROCESS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number (this module only) 3.000
C  (Overall code version is noted in the current PROCESS box file,
C  and in variable PROGNM in routine INFORM, file aachange.f)
C
C--Description
C  Power Reactor Optimisation Code for Environmental and Safety Studies
C
C  This is a systems code that evaluates various physics and
C  engineering aspects of a fusion power plant subject to given
C  constraints, and can optimise these parameters by minimising
C  or maximising a function of them, such as the fusion power or
C  cost of electricity.
C
C  This program is derived from the TETRA and STORAC codes produced by
C  Oak Ridge National Laboratory, Tennessee, USA. The main authors in
C  the USA were J.D.Galambos and P.C.Shipe.
C
C  The code was transferred to Culham Laboratory, Oxfordshire, UK, in
C  April 1992, and the physics models were updated by P.J.Knight to
C  include the findings of the Culham reactor studies documented in
C  Culham Report AEA FUS 172 (1992). The standard of the Fortran has
C  been thoroughly upgraded and checked using the program QA Fortran,
C  Version 6.1, produced by Programming Research Limited, Esher, Surrey.
C
C  The code is fully compatible with the ANSI Fortran 77 standard,
C  with the exception of:
C  - lower-case characters
C  - the inclusion of IMPLICIT NONE in subroutines to ensure that all
C    variables are explicitly declared (now being replaced with the
C    use of the -u compiler flag),
C  - the use of variable names up to and including eight characters
C    for the sake of readability,
C  - the use of INCLUDE files to simplify COMMON block declarations,
C  - system calls, which are limited to routine INFORM, providing
C    run-time information about the code version/date/time.
C
C--Compilation details
C  See ~peter/process/object/Makefile
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--References
C  F/MI/PJK/PROCESS/DOCUMENTATION/MANUAL:
C            A User's Guide to the PROCESS Systems Code, P. J. Knight
C  Box file F/RS/CIRE5523/PWF (up to 15/01/96)
C  Box file F/MI/PJK/PROCESS  (since 15/01/96)
C  
C--History
C  03/10/96 PJK 3.000 Upgrade of main program unit
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'

C  Local variables
      INTEGER IFAIL

C  External routines
      EXTERNAL EQSLV,FINAL,INIT,OHEADR,SCAN

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialise things

      CALL INIT

C *** Call equation solver (HYBRD)

      CALL EQSLV(IFAIL)

C *** Call routine to do optimisation sweeps

      IF (IOPTIMZ.GE.0) CALL SCAN

C *** Finish up

      IF (IOPTIMZ.LT.0) CALL FINAL(IFAIL)

      CALL OHEADR(NOUT,'End of PROCESS Output')
      CALL OHEADR(IOTTY,'End of PROCESS Output')

      CLOSE(UNIT=10)
      CLOSE(UNIT=11)
      CLOSE(UNIT=12)

      END
C----------------------------------------------------------------------
      SUBROUTINE CODEVER(NOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to print out the code's name and version, and the
C  date and time of the run
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C
C--Arguments
C  NOUT  : (INPUT)  Output unit identifier
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      INTEGER WIDTH
      PARAMETER(WIDTH = 72)

C  Arguments
      INTEGER NOUT

C  Local variables
      CHARACTER*72 PROGID(0:10)

C  External routines
      EXTERNAL INFORM,OBLNKL,OCENTR,OSTARS

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Write out banner

      CALL OBLNKL(NOUT)
      CALL OSTARS(NOUT,WIDTH)
      CALL OCENTR(NOUT,'PROCESS',WIDTH)
      CALL OCENTR(NOUT,'Power Reactor Optimisation Code',WIDTH)
      CALL OCENTR(NOUT,'for Environmental and Safety Studies',WIDTH)
      CALL OSTARS(NOUT,WIDTH)
      CALL OBLNKL(NOUT)

C *** Obtain details of this run

      CALL INFORM(PROGID)

C *** Write out details

      WRITE(NOUT,*) PROGID(1)
      WRITE(NOUT,*) PROGID(2)
      WRITE(NOUT,*) PROGID(3)
      WRITE(NOUT,*) PROGID(4)
      WRITE(NOUT,*) PROGID(5)
      WRITE(NOUT,*) PROGID(6)

      CALL OBLNKL(NOUT)
      CALL OSTARS(NOUT,WIDTH)
      CALL OBLNKL(NOUT)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE INIT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.010
C
C--Description
C  Rotuine that calls the initialisation routines
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  17 November 1997
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C  17/11/97 PJK 3.010 Changed file names to *.DAT
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      use process_input

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'divrt.h'

C  External routines
C+PJK      EXTERNAL CHECK,CODEVER,EDIT1,INITIAL,INPUT,OBLNKL,OCMMNT
      EXTERNAL CHECK,CODEVER,INITIAL,OBLNKL,OCMMNT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Open the three input/output external files

      OPEN(UNIT=10,FILE='IN.DAT',STATUS='UNKNOWN')
      OPEN(UNIT=11,FILE='PLOT.DAT',STATUS='UNKNOWN')
      OPEN(UNIT=12,FILE='OUT.DAT',STATUS='UNKNOWN')

C *** Initialise the program variables

      CALL INITIAL

C *** Print code banner + run details

      CALL CODEVER(NOUT)
      CALL CODEVER(IOTTY)

C *** Input any desired new initial values

      CALL INPUT

C *** Check input data for errors/ambiguities

      CALL CHECK

C *** Print code version

      CALL OBLNKL(NOUT)
      CALL OCMMNT(NOUT,ICASE)
      CALL OCMMNT(IOTTY,ICASE)
      CALL OBLNKL(IOTTY)

C *** Write to the output file certain relevant details about this run

      CALL EDIT1

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE EQSLV(IFAIL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to call the non-optimising equation solver
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C
C--Arguments
C  IFAIL  : (OUTPUT) Error flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'

C  Arguments
      INTEGER IFAIL

C  Local variables
      DOUBLE PRECISION SUMSQ,WA(IPTNT)
      INTEGER INN,NN,NPRINT,NX

C  External routines
      EXTERNAL EQSOLV,FCNHYB,HERROR,LOADXC,OBLNKL,OCMMNT,OHEADR,
     +     OSUBHD,OVARIN,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** If no HYBRD (non-optimisation) runs are required, exit routine

      IF (IOPTIMZ.GT.0) GOTO 1000

      NCALLS = 0
      NFEV1 = 0
      NFEV2 = 0
      NPRINT = 0

C *** Use HYBRD to find a starting point

      NN = NVAR

C+**PJK 22/10/92 Removed arguments (xcm,nn) from call to LOADXC

      CALL LOADXC
      NN = NEQNS

      CALL EQSOLV(FCNHYB,NEQNS,XCM,RCM,FTOL,EPSFCN,
     +     FACTOR,NPRINT,IFAIL,WA,IPTNT,RESDL,NFEV1)

C *** Print out information on solution

      CALL OHEADR(NOUT,'Numerics')
      CALL OCMMNT(NOUT,
     +     'PROCESS has performed a HYBRD (non-optimisation) run,')

      IF (IFAIL.NE.1) THEN
         CALL OCMMNT(NOUT,
     +        'but could not find a feasible set of parameters.')
         CALL OBLNKL(NOUT)
         CALL OVARIN(NOUT,'HYBRD error flag','(ifail)',IFAIL)

         CALL OHEADR(IOTTY,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
         CALL OVARIN(IOTTY,'HYBRD error flag','(ifail)',IFAIL)
         CALL OBLNKL(IOTTY)
      ELSE
         CALL OCMMNT(NOUT,'and found a feasible set of parameters.')
         CALL OBLNKL(NOUT)
         CALL OHEADR(IOTTY,'PROCESS found a feasible solution')
      END IF

C *** Sum the square of the residuals

      SUMSQ = 0.0D0
      DO 10 NX = 1,NEQNS
         SUMSQ = SUMSQ+RCM(NX)**2
 10   CONTINUE
      SQSUMSQ = SQRT(SUMSQ)

      CALL OVARRE(NOUT,'Estimate of the constraints','(sqsumsq)',
     +     SQSUMSQ)

C *** If necessary, write out the relevant error message

      IF (IFAIL.NE.1) THEN
         CALL OBLNKL(NOUT)
         CALL HERROR(NOUT,IOTTY,IFAIL)
         CALL OBLNKL(IOTTY)
      END IF

      CALL OSUBHD(NOUT,'The solution vector is comprised as follows :')

      WRITE(NOUT,20)
 20   FORMAT(
     +     T5,'i',
     +     T23,'final',
     +     T33,'fractional',
     +     T46,'residue')

      WRITE(NOUT,30)
 30   FORMAT(
     +     T23,'value',
     +     T35,'change')

      CALL OBLNKL(NOUT)

      DO 50 INN = 1,NEQNS
         XCS(INN) = XCM(INN)*SCAFC(INN)
         WRITE(NOUT,40) INN,LABLXC(IXC(INN)),XCS(INN),XCM(INN),
     +        RESDL(INN)
 40      FORMAT(T2,I4,T8,A8,T19,1PE12.4,1PE12.4,1PE12.4)
 50   CONTINUE

      CALL OSUBHD(NOUT,
     +    'The following constraint residues should be close to zero :')

      DO 70 INN = 1,NEQNS
         WRITE(NOUT,60) INN,LABLCC(ICC(INN)),RCM(INN)
 60      FORMAT(T2,I4,T8,A34,T45,1PE12.4)
 70   CONTINUE

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE HERROR(NOUT,IOTTY,IFAIL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to print out relevant messages in the case of an unfeasible
C  result from a HYBRD (non-optimisation) run.
C
C  The messages are written to units NOUT and IOTTY, which are
C  by default the output file and screen, respectively.
C
C  If IFAIL=1 then a feasible solution has been found and therefore
C  no error message is required.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C
C--Arguments
C  NOUT  : (INPUT) Unit specifier for main output
C  IOTTY : (INPUT) Unit specifier for terminal I/O
C  IFAIL : (INPUT) HYBRD error flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER NOUT,IOTTY,IFAIL

C  External routines
      EXTERNAL OCMMNT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IFAIL.LT.0) THEN
         CALL OCMMNT(NOUT, 'User-terminated execution of HYBRD.')
         CALL OCMMNT(IOTTY,'User-terminated execution of HYBRD.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.0) THEN
         CALL OCMMNT(NOUT,
     +        'Improper input parameters to the HYBRD routine.')
         CALL OCMMNT(NOUT,'PROCESS coding must be checked.')

         CALL OCMMNT(IOTTY,
     +        'Improper input parameters to the HYBRD routine.')
         CALL OCMMNT(IOTTY,'PROCESS coding must be checked.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.1) GOTO 1000

      IF (IFAIL.EQ.2) THEN
         CALL OCMMNT(NOUT,
     +        'The maximum number of calls has been reached without')
         CALL OCMMNT(NOUT,
     +        'solution, suggesting that the iteration is not')
         CALL OCMMNT(NOUT,'making good progress.')
         CALL OCMMNT(NOUT,'Try changing the variables in IXC.')

         CALL OCMMNT(IOTTY,
     +        'The maximum number of calls has been reached without')
         CALL OCMMNT(IOTTY,
     +        'solution, suggesting that the iteration is not')
         CALL OCMMNT(IOTTY,'making good progress.')
         CALL OCMMNT(IOTTY,'Try changing the variables in IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.3) THEN
         CALL OCMMNT(NOUT,
     +        'The tolerance is too small: No further improvement')
         CALL OCMMNT(NOUT,'in the approximate solution is possible.')
         CALL OCMMNT(NOUT,'Try raising the value of FTOL.')

         CALL OCMMNT(IOTTY,
     +        'The tolerance is too small: No further improvement')
         CALL OCMMNT(IOTTY,'in the approximate solution is possible.')
         CALL OCMMNT(IOTTY,'Try raising the value of FTOL.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.4) THEN
         CALL OCMMNT(NOUT,
     +        'The iteration is not making good progress.')
         CALL OCMMNT(NOUT,'Try changing the variables in IXC.')

         CALL OCMMNT(IOTTY,
     +        'The iteration is not making good progress.')
         CALL OCMMNT(IOTTY,'Try changing the variables in IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.GT.4) THEN
         CALL OCMMNT(NOUT,
     +        'This value of IFAIL should not be possible...')
         CALL OCMMNT(NOUT,'See source code for details.')

         CALL OCMMNT(IOTTY,
     +        'This value of ifail should not be possible...')
         CALL OCMMNT(IOTTY,'See source code for details.')
         GOTO 1000
      END IF

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE VERROR(NOUT,IOTTY,IFAIL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to print out relevant messages in the case of an unfeasible
C  result from a VMCON (optimisation) run.
C
C  The messages are written to units NOUT and IOTTY, which are
C  by default the output file and screen, respectively.
C
C  If IFAIL=1 then a feasible solution has been found and therefore
C  no error message is required.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C
C--Arguments
C  NOUT  : (INPUT) Unit specifier for main output
C  IOTTY : (INPUT) Unit specifier for terminal I/O
C  IFAIL : (INPUT) VMCON error flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER NOUT,IOTTY,IFAIL

C  External routines
      EXTERNAL OCMMNT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IFAIL.LT.0) THEN
         CALL OCMMNT(NOUT, 'User-terminated execution of VMCON.')
         CALL OCMMNT(IOTTY,'User-terminated execution of VMCON.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.0) THEN
         CALL OCMMNT(NOUT,
     +        'Improper input parameters to the VMCON routine.')
         CALL OCMMNT(NOUT,'PROCESS coding must be checked.')

         CALL OCMMNT(IOTTY,
     +        'Improper input parameters to the VMCON routine.')
         CALL OCMMNT(IOTTY,'PROCESS coding must be checked.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.1) GOTO 1000

      IF (IFAIL.EQ.2) THEN
         CALL OCMMNT(NOUT,
     +        'The maximum number of calls has been reached without')
         CALL OCMMNT(NOUT,
     +        'solution, suggesting that the iteration is not')
         CALL OCMMNT(NOUT,'making good progress.')
         CALL OCMMNT(NOUT,'Try changing or adding variables to IXC.')

         CALL OCMMNT(IOTTY,
     +        'The maximum number of calls has been reached without')
         CALL OCMMNT(IOTTY,
     +        'solution, suggesting that the iteration is not')
         CALL OCMMNT(IOTTY,'making good progress.')
         CALL OCMMNT(IOTTY,'Try changing or adding variables to IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.3) THEN
         CALL OCMMNT(NOUT,
     +        'The line search required the maximum of 10 calls.')
         CALL OCMMNT(NOUT,
     +        'A feasible solution may be difficult to achieve.')
         CALL OCMMNT(NOUT,'Try changing or adding variables to IXC.')

         CALL OCMMNT(IOTTY,
     +        'The line search required the maximum of 10 calls.')
         CALL OCMMNT(IOTTY,
     +        'A feasible solution may be difficult to achieve.')
         CALL OCMMNT(IOTTY,'Try changing or adding variables to IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.4) THEN
         CALL OCMMNT(NOUT,'An uphill search direction was found.')
         CALL OCMMNT(NOUT,'Try changing the equations in ICC, or')
         CALL OCMMNT(NOUT,'adding new variables to IXC.')

         CALL OCMMNT(IOTTY,'An uphill search direction was found.')
         CALL OCMMNT(IOTTY,'Try changing the equations in ICC, or')
         CALL OCMMNT(IOTTY,'adding new variables to IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.5) THEN
         CALL OCMMNT(NOUT,
     +        'The quadratic programming technique was unable to')
         CALL OCMMNT(NOUT,'find a feasible point.')
         CALL OCMMNT(NOUT,'Try changing or adding variables to IXC.')

         CALL OCMMNT(IOTTY,
     +        'The quadratic programming technique was unable to')
         CALL OCMMNT(IOTTY,'find a feasible point.')
         CALL OCMMNT(IOTTY,'Try changing or adding variables to IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.EQ.6) THEN
         CALL OCMMNT(NOUT,
     +        'The quadratic programming technique was restricted')
         CALL OCMMNT(NOUT,
     +        'by an artificial bound, or failed due to a singular')
         CALL OCMMNT(NOUT,'matrix.')
         CALL OCMMNT(NOUT,'Try changing the equations in ICC, or')
         CALL OCMMNT(NOUT,'adding new variables to IXC.')

         CALL OCMMNT(IOTTY,
     +        'The quadratic programming technique was restricted')
         CALL OCMMNT(IOTTY,
     +        'by an artificial bound, or failed due to a singular')
         CALL OCMMNT(IOTTY,'matrix.')
         CALL OCMMNT(IOTTY,'Try changing the equations in ICC, or')
         CALL OCMMNT(IOTTY,'adding new variables to IXC.')
         GOTO 1000
      END IF

      IF (IFAIL.GT.6) THEN
         CALL OCMMNT(NOUT,
     +        'This value of ifail should not be possible...')
         CALL OCMMNT(NOUT,'See source code for details.')

         CALL OCMMNT(IOTTY,
     +        'This value of ifail should not be possible...')
         CALL OCMMNT(IOTTY,'See source code for details.')
         GOTO 1000
      END IF

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SCAN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.040
C
C--Description
C  Call the optimisation routine VMCON over a range of values of
C  one of the variables.
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.6368
C
C--Date
C  25 May 2006
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C  01/04/98 PJK 3.010 Added POWFMAX to list of scanning variables
C  23/06/98 PJK 3.020 Added KAPPA and TRIANG to list of scanning vars
C  19/05/99 PJK 3.030 Added warning about trying to scan CFACTR with
C                     new availability model
C  25/05/06 PJK 3.040 Added implied-DO loops for sweep outputs
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'sweep.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'ineq.h'
      INCLUDE 'cost.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'divrt.h'
      INCLUDE 'numer.h'

C  Local variables
      CHARACTER*25 XLABEL,TLABEL

      DOUBLE PRECISION
     +     RMAJ(IPNSCNS),ASP(IPNSCNS),BTOR(IPNSCNS),PINJ(IPNSCNS),
     +     BET(IPNSCNS),BETL(IPNSCNS),HFIP(IPNSCNS),POWF(IPNSCNS),
     +     BQ(IPNSCNS),PCP(IPNSCNS),PHT(IPNSCNS),STR(IPNSCNS),
     +     EB(IPNSCNS),RCL(IPNSCNS),VCL(IPNSCNS),FCL(IPNSCNS),
     +     TMX(IPNSCNS),PMP(IPNSCNS),BS(IPNSCNS),T10(IPNSCNS),
     +     D20(IPNSCNS),PBF(IPNSCNS),PN(IPNSCNS),PG(IPNSCNS),
     +     CE(IPNSCNS),CEC(IPNSCNS),CEF(IPNSCNS),CEO(IPNSCNS),
     +     TFP(IPNSCNS),BTT(IPNSCNS),CCST(IPNSCNS),REC1(IPNSCNS)
      DOUBLE PRECISION
     +     HFIO(IPNSCNS),PFP(IPNSCNS),PIWP(IPNSCNS),WTF(IPNSCNS),
     +     WPF(IPNSCNS),QLM(IPNSCNS),HLD(IPNSCNS),EPBP(IPNSCNS),
     +     CDT(IPNSCNS),BLIM(IPNSCNS),WALL(IPNSCNS),CRC(IPNSCNS),
     +     CURD(IPNSCNS),IP(IPNSCNS),LQ(IPNSCNS),OCD(IPNSCNS),
     +     SQ(IPNSCNS),IFA(IPNSCNS)

      INTEGER IFAIL,I,NPLOT

C  External routines
      EXTERNAL DOOPT,FINAL,OBLNKL,OSTARS

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      TLABEL = ICASE

      IF (ISWEEP.EQ.0) THEN
         CALL DOOPT(IFAIL)
         CALL FINAL(IFAIL)
         GOTO 1000
      END IF

      IF (ISWEEP.GT.IPNSCNS) THEN
         WRITE(*,*) 'Error in routine SCAN:'
         WRITE(*,*) 'Illegal value of isweep, = ',ISWEEP
         WRITE(*,*) 'Maximum = ',IPNSCNS
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      END IF

      DO 20 I = 1,ISWEEP

         CALL OBLNKL(NOUT)
         CALL OSTARS(NOUT,72)
         WRITE(NOUT,10) I,ISWEEP
 10      FORMAT(' ***** Scan point ',I2,' of ',I2,' *****')
         CALL OSTARS(NOUT,72)

         IF (NSWEEP.EQ.1) THEN
            ASPECT = SWEEP(I)
            XLABEL = 'Aspect Ratio'
         ELSE IF (NSWEEP.EQ.2) THEN
            HLDIVLIM = SWEEP(I)
            XLABEL = 'Divertor Ht Limit MW/m2'
         ELSE IF (NSWEEP.EQ.3) THEN
            PNETELIN = SWEEP(I)
            XLABEL = 'Net Elec pwr MW'
         ELSE IF (NSWEEP.EQ.4) THEN
            HFACT = SWEEP(I)
            XLABEL = 'Confinement hfact'
         ELSE IF (NSWEEP.EQ.5) THEN
            OACDCP = SWEEP(I)
            XLABEL = 'j TF inner leg MA/m2'
         ELSE IF (NSWEEP.EQ.6) THEN
            WALALW = SWEEP(I)
            XLABEL = 'All. wall load MW/m2'
         ELSE IF (NSWEEP.EQ.7) THEN
            BEAMFUS0 = SWEEP(I)
            XLABEL = 'Beam back. multiplier'
         ELSE IF (NSWEEP.EQ.8) THEN
            FQVAL = SWEEP(I)
            XLABEL = '1/Big Q'
         ELSE IF (NSWEEP.EQ.9) THEN
            TE = SWEEP(I)
            XLABEL = 'Temperature'
         ELSE IF (NSWEEP.EQ.10) THEN
            BOUNDU(15) = SWEEP(I)
            XLABEL = 'Volt-sec upper bound'
         ELSE IF (NSWEEP.EQ.11) THEN
            DNBETA = SWEEP(I)
            XLABEL = 'Troyon coefficient'
         ELSE IF (NSWEEP.EQ.12) THEN
            BSCFMAX = SWEEP(I)
            XLABEL = 'Bootstrap Fraction'
         ELSE IF (NSWEEP.EQ.13) THEN
            BOUNDU(10) = SWEEP(I)
            XLABEL = 'H factor upper bound'
         ELSE IF (NSWEEP.EQ.14) THEN
            FIOOIC = SWEEP(I)
            XLABEL = 'Iop / Icrit f-value'
         ELSE IF (NSWEEP.EQ.15) THEN
            FJPROT = SWEEP(I)
            XLABEL = 'TFC J limit f-value'
         ELSE IF (NSWEEP.EQ.16) THEN
            RMAJOR = SWEEP(I)
            XLABEL = 'Plasma major radius'
         ELSE IF (NSWEEP.EQ.17) THEN
            BMXLIM = SWEEP(I)
            XLABEL = 'Max toroidal field'
         ELSE IF (NSWEEP.EQ.18) THEN
            GAMMAX = SWEEP(I)
            XLABEL = 'Maximum CD gamma'
         ELSE IF (NSWEEP.EQ.19) THEN
            BOUNDL(16) = SWEEP(I)
            XLABEL = 'OHC thickness lower bound'
         ELSE IF (NSWEEP.EQ.20) THEN
            TBRNMN = SWEEP(I)
            XLABEL = 'Minimum burn time'
         ELSE IF (NSWEEP.EQ.21) THEN
            SIGPFALW = SWEEP(I)
            XLABEL = 'Allowable PF coil stress'
         ELSE IF (NSWEEP.EQ.22) THEN
            IF (IAVAIL.EQ.1) THEN
               WRITE(*,*) 'Error in routine SCAN:'
               WRITE(*,*) 'Do not scan CFACTR if IAVAIL=1'
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            END IF
            CFACTR = SWEEP(I)
            XLABEL = 'Plant availability factor'
         ELSE IF (NSWEEP.EQ.23) THEN
            BOUNDU(72) = SWEEP(I)
            XLABEL = 'Ip/Irod upper bound'
         ELSE IF (NSWEEP.EQ.24) THEN
            POWFMAX = SWEEP(I)
            XLABEL = 'Fusion Power limit'
         ELSE IF (NSWEEP.EQ.25) THEN
            KAPPA = SWEEP(I)
            XLABEL = 'Elongation'
         ELSE IF (NSWEEP.EQ.26) THEN
            TRIANG = SWEEP(I)
            XLABEL = 'Triangularity'
         ELSE
            WRITE(*,*) 'Error in routine SCAN:'
            WRITE(*,*) 'Illegal scan variable number, nsweep = ',NSWEEP
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF

         CALL DOOPT(IFAIL)
         CALL FINAL(IFAIL)

C *** Store values for output

         IFA(I)  = DBLE(IFAIL)
         SQ(I)   = SQSUMSQ
         CE(I)   = COE
         CEC(I)  = COECAP
         CEF(I)  = COEFUELT
         CEO(I)  = COEOAM
         CCST(I) = CAPCOST
         CRC(I)  = C221+C222
         CDT(I)  = CDIRT / 1.0D3
         RMAJ(I) = RMAJOR
         ASP(I)  = ASPECT
         IP(I)   = 1.0D-6 * PLASCUR
         BTOR(I) = BT
         BTT(I)  = BTOT
         LQ(I)   = Q
         QLM(I)  = QLIM
         BET(I)  = BETA
         BLIM(I) = BETALIM
         EPBP(I) = BETAP / ASPECT
         T10(I)  = TE/10.0D0 * PCOEF
         D20(I)  = DENE/1.0D20
         HFIP(I) = HFAC(6)
         HFIO(I) = HFAC(7)
         POWF(I) = POWFMW
         PBF(I)  = PALPNB * 5.0D0
         WALL(I) = WALLMW
         PINJ(I) = 1.0D-6 * (PINJI + PINJE)
         PIWP(I) = PINJWP
         PHT(I)  = PHEAT * 1.0D-6
         CURD(I) = 1.0D-6*(PINJI+PINJE-PHEAT)
         BQ(I)   = BIGQ
         BS(I)   = BOOTIPF
         EB(I)   = ENBEAM/1.0D3
         HLD(I)  = HLDIV
         TFP(I)  = TFCMW
         WTF(I)  = WHTTF
         STR(I)  = SIGRAD + SIGTAN
         OCD(I)  = OACDCP/1.0D6
         TMX(I)  = TCPMAX
         PCP(I)  = TFCPMW
         FCL(I)  = FCOOLCP
         RCL(I)  = RCOOL
         VCL(I)  = VCOOL
         PMP(I)  = PPUMP/1.0D6
         PFP(I)  = 1.0D-3 * SRCKTPM
         WPF(I)  = WHTPF
         PG(I)   = PGROSSMW
         PN(I)   = PNETELMW
         IF (IREACTOR.EQ.1) THEN
            REC1(I) = (PGROSSMW-PNETELMW) / PGROSSMW
         ELSE
            REC1(I) = 0.0D0
         END IF
 20   CONTINUE

      NPLOT = 11

      WRITE(NPLOT,900) ISWEEP
      WRITE(NPLOT,901) TLABEL

C+**PJK 20/10/92 Why does this section supersede one above?

      IF (NSWEEP.EQ.8) THEN
         XLABEL = 'Big Q'
         DO 30 I = 1,ISWEEP
            SWEEP(I) = 1.0D0/SWEEP(I) 
 30      CONTINUE
      END IF

      WRITE(NPLOT,902) XLABEL,(SWEEP(I),I=1,ISWEEP)
      WRITE(NPLOT,903) (IFA(I),I=1,ISWEEP)
      WRITE(NPLOT,904) (SQ(I),I=1,ISWEEP)
      WRITE(NPLOT,905) (CE(I),I=1,ISWEEP)
      WRITE(NPLOT,906) (CEC(I),I=1,ISWEEP)
      WRITE(NPLOT,907) (CEF(I),I=1,ISWEEP)
      WRITE(NPLOT,908) (CEO(I),I=1,ISWEEP)
      WRITE(NPLOT,909) (CCST(I),I=1,ISWEEP)
      WRITE(NPLOT,910) (CRC(I),I=1,ISWEEP)
      WRITE(NPLOT,911) (CDT(I),I=1,ISWEEP)
      WRITE(NPLOT,912) (RMAJ(I),I=1,ISWEEP)
      WRITE(NPLOT,913) (ASP(I),I=1,ISWEEP)
      WRITE(NPLOT,914) (IP(I),I=1,ISWEEP)
      WRITE(NPLOT,915) (BTOR(I),I=1,ISWEEP)
      WRITE(NPLOT,916) (BTT(I),I=1,ISWEEP)
      WRITE(NPLOT,917) (LQ(I),I=1,ISWEEP)
      WRITE(NPLOT,918) (QLM(I),I=1,ISWEEP)
      WRITE(NPLOT,919) (BET(I),I=1,ISWEEP)
      WRITE(NPLOT,920) (BLIM(I),I=1,ISWEEP)
      WRITE(NPLOT,921) (EPBP(I),I=1,ISWEEP)
      WRITE(NPLOT,922) (T10(I),I=1,ISWEEP)
      WRITE(NPLOT,923) (D20(I),I=1,ISWEEP)
      WRITE(NPLOT,924) (HFIP(I),I=1,ISWEEP)
      WRITE(NPLOT,925) (HFIO(I),I=1,ISWEEP)
      WRITE(NPLOT,926) (POWF(I),I=1,ISWEEP)
      WRITE(NPLOT,927) (PBF(I),I=1,ISWEEP)
      WRITE(NPLOT,928) (WALL(I),I=1,ISWEEP)
      WRITE(NPLOT,929) (PINJ(I),I=1,ISWEEP)
      WRITE(NPLOT,930) (PIWP(I),I=1,ISWEEP)
      WRITE(NPLOT,931) (PHT(I),I=1,ISWEEP)
      WRITE(NPLOT,932) (CURD(I),I=1,ISWEEP)
      WRITE(NPLOT,933) (BQ(I),I=1,ISWEEP)
      WRITE(NPLOT,934) (BS(I),I=1,ISWEEP)
      WRITE(NPLOT,935) (EB(I),I=1,ISWEEP)
      WRITE(NPLOT,936) (HLD(I),I=1,ISWEEP)
      WRITE(NPLOT,937) (TFP(I),I=1,ISWEEP)
      WRITE(NPLOT,938) (WTF(I),I=1,ISWEEP)
      WRITE(NPLOT,939) (STR(I),I=1,ISWEEP)
      WRITE(NPLOT,940) (OCD(I),I=1,ISWEEP)
      WRITE(NPLOT,941) (TMX(I),I=1,ISWEEP)
      WRITE(NPLOT,942) (PCP(I),I=1,ISWEEP)
      WRITE(NPLOT,943) (FCL(I),I=1,ISWEEP)
      WRITE(NPLOT,944) (RCL(I),I=1,ISWEEP)
      WRITE(NPLOT,945) (VCL(I),I=1,ISWEEP)
      WRITE(NPLOT,946) (PMP(I),I=1,ISWEEP)
      WRITE(NPLOT,947) (PFP(I),I=1,ISWEEP)
      WRITE(NPLOT,948) (WPF(I),I=1,ISWEEP)
      WRITE(NPLOT,949) (PG(I),I=1,ISWEEP)
      WRITE(NPLOT,950) (PN(I),I=1,ISWEEP)
      WRITE(NPLOT,951) (REC1(I),I=1,ISWEEP)

 900  FORMAT(I8)
 901  FORMAT(A25)
 902  FORMAT(A25, 20E10.4)
 903  FORMAT('Ifail                    ', 20E10.4)
 904  FORMAT('Sqsumsq                  ', 20E10.4)
 905  FORMAT('Electric cost (mil/kwh)  ', 20E10.4)
 906  FORMAT('Capital cost (mil/kwh)   ', 20E10.4)
 907  FORMAT('Fuel cost (mil/kwh)      ', 20E10.4)
 908  FORMAT('Operations cost (mil/kwh)', 20E10.4)
 909  FORMAT('Capital cost (mil)       ', 20E10.4)
 910  FORMAT('Core costs (millions)    ', 20E10.4)
 911  FORMAT('Direct cost (billions)   ', 20E10.4)
 912  FORMAT('Major Radius (m)         ', 20E10.4)
 913  FORMAT('Aspect Ratio             ', 20E10.4)
 914  FORMAT('Plasma Current (MA)      ', 20E10.4)
 915  FORMAT('B Toroidal Axis (T)      ', 20E10.4)
 916  FORMAT('B total on axis (T)      ', 20E10.4)
 917  FORMAT('Safety Factor            ', 20E10.4)
 918  FORMAT('Should be zero...........', 20E10.4)
 919  FORMAT('Beta                     ', 20E10.4)
 920  FORMAT('Beta Limit               ', 20E10.4)
 921  FORMAT('Epsilon Beta Poloidal    ', 20E10.4)
 922  FORMAT('Average Temp x10 (KeV)   ', 20E10.4)
 923  FORMAT('Average Dens (10^20/m^3) ', 20E10.4)
 924  FORMAT('H-fact Iter Power        ', 20E10.4)
 925  FORMAT('H-fact Iter Offset       ', 20E10.4)
 926  FORMAT('Fusion Power (MW)        ', 20E10.4)
 927  FORMAT('nb Fusion Power (MW)     ', 20E10.4)
 928  FORMAT('Wall Load (MW/m^2)       ', 20E10.4)
 929  FORMAT('Injection Power (MW)     ', 20E10.4)
 930  FORMAT('Inject Pwr Wall Plug (MW)', 20E10.4)
 931  FORMAT('Heating Power (MW)       ', 20E10.4)
 932  FORMAT('Current Drive (MW)       ', 20E10.4)
 933  FORMAT('Big Q                    ', 20E10.4)
 934  FORMAT('Bootstrap Fraction       ', 20E10.4)
 935  FORMAT('Neutral Beam Energy (MeV)', 20E10.4)
 936  FORMAT('Divertor Heat (MW/m^2)   ', 20E10.4)
 937  FORMAT('TF coil Power (MW)       ', 20E10.4)
 938  FORMAT('TF coil weight (kg)      ', 20E10.4)
 939  FORMAT('TF stress (MPa)          ', 20E10.4)
 940  FORMAT('J   TF inner leg (MA/m^2)', 20E10.4)
 941  FORMAT('Should be zero...........', 20E10.4)
 942  FORMAT('Res TF inner leg Pwr (MW)', 20E10.4)
 943  FORMAT('Coolant Fraction Ctr.    ', 20E10.4)
 944  FORMAT('Should be zero...........', 20E10.4)
 945  FORMAT('Should be zero...........', 20E10.4)
 946  FORMAT('Should be zero...........', 20E10.4)
 947  FORMAT('PF coil Power (MW)       ', 20E10.4)
 948  FORMAT('PF coil weight (kg)      ', 20E10.4)
 949  FORMAT('Gross Elect Pwr (MW)     ', 20E10.4)
 950  FORMAT('Net electric Pwr (MW)    ', 20E10.4)
 951  FORMAT('Recirculating Fraction   ', 20E10.4)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE DOOPT(IFAIL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to call the optimisation routine VMCON.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C
C--Arguments
C  IFAIL  : (OUTPUT) Error flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'

C  Arguments
      INTEGER IFAIL

C  Local variables
      DOUBLE PRECISION SUMM,XCVAL,XMAXX,XMINN,F

      INTEGER II,INN,NN,IFLAG

C  External routines
      EXTERNAL BOUNDXC,LOADXC,OBLNKL,OCMMNT,OHEADR,OPTIMIZ,OSUBHD,
     +     OVARIN,OVARRE,VERROR

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** If no optimisation is required, leave the routine

      IF (IOPTIMZ.LT.0) GOTO 1000

C *** Set up variables to be iterated

      NN = NVAR

C+**PJK 22/10/92 Removed arguments to LOADXC
C+**PJK 22/10/92 call loadxc(xcm,nn) was the old coding.

      CALL LOADXC

C+**PJK 22/10/92 Removed arguments to BOUNDXC
C+**PJK 22/10/92 call boundxc(xcm,nn) was the old coding.

      CALL BOUNDXC

C+**PJK 22/10/92 Removed arguments xcm, rcm and nfev2 from
C+**PJK 22/10/92 call to optimiz. These are passed in via
C+**PJK 22/10/92 COMMON anyway.

      CALL OPTIMIZ(IFAIL,F)

C *** Check on accuracy of solution by summing the
C *** squares of the residuals

      SUMM = 0.0D0
      DO 10 II = 1,NEQNS
         SUMM = SUMM + RCM(II)*RCM(II)
 10   CONTINUE
      SQSUMSQ = SQRT(SUMM)

C *** Print out information on solution

      CALL OHEADR(NOUT,'Numerics')
      CALL OCMMNT(NOUT,
     +     'PROCESS has performed a VMCON (optimisation) run,')
      IF (IFAIL.NE.1) THEN
         CALL OCMMNT(NOUT,
     +        'but could not find a feasible set of parameters.')

         CALL OHEADR(IOTTY,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
         CALL OVARIN(IOTTY,'VMCON error flag','(ifail)',IFAIL)
         CALL OBLNKL(IOTTY)
      ELSE
         CALL OCMMNT(NOUT,'and found a feasible set of parameters.')
         CALL OHEADR(IOTTY,'PROCESS found a feasible solution')
      END IF

      CALL OBLNKL(NOUT)

C *** If necessary, write the relevant error message

      IF (IFAIL.NE.1) THEN
         CALL VERROR(NOUT,IOTTY,IFAIL)
         CALL OBLNKL(NOUT)
         CALL OBLNKL(IOTTY)
      END IF

      CALL OVARIN(NOUT,'Optimisation switch','(ioptimz)',IOPTIMZ)
      CALL OVARIN(NOUT,'Figure of merit switch','(minmax)',MINMAX)
      IF (IFAIL.NE.1) THEN
         CALL OVARIN(NOUT,'VMCON error flag','(ifail)',IFAIL)
      END IF
      CALL OVARRE(NOUT,'Figure of merit objective function','(f)',F)
      CALL OVARRE(NOUT,'Estimate of the constraints','(sqsumsq)',
     +     SQSUMSQ)
      CALL OBLNKL(NOUT)

      IF (IFAIL.EQ.1) THEN
         CALL OCMMNT(NOUT,
     +     'PROCESS has successfully optimised the program variables')
      ELSE
         CALL OCMMNT(NOUT,
     +     'PROCESS has tried to optimise the program variables')
      END IF

      IF (MINMAX.GT.0) THEN
         WRITE(NOUT,20) LABLMM(ABS(MINMAX))
 20      FORMAT(' to minimise the ',A22)
      ELSE
         WRITE(NOUT,30) LABLMM(ABS(MINMAX))
 30      FORMAT(' to maximise the ',A22)
      END IF

      CALL OBLNKL(NOUT)

C *** Check which variables are at bounds

      IFLAG = 0
      DO 60 II = 1,NVRBL
         XMINN = 1.01D0*BONDL(II)
         XMAXX = 0.99D0*BONDU(II)

         IF (XCM(II).LT.XMINN) THEN
            IF (IFLAG.EQ.0) THEN
               CALL OCMMNT(NOUT,
     +              'Certain operating limits have been reached,')
               CALL OCMMNT(NOUT,
     +              'as shown by the following variables that are')
               CALL OCMMNT(NOUT,
     +              'at the edge of their prescribed range :')
               CALL OBLNKL(NOUT)
               IFLAG = 1
            END IF
            XCVAL = XCM(II)*SCAFC(II)
            WRITE(NOUT,40) II,LABLXC(IXC(II)),XCVAL
 40         FORMAT(
     +           T4,'Variable ',I3,'  (',A8,')  is at its ',
     +           'lower bound of ',1PE12.4)
         END IF

         IF (XCM(II).GT.XMAXX) THEN
            IF (IFLAG.EQ.0) THEN
               CALL OCMMNT(NOUT,
     +              'Certain operating limits have been reached,')
               CALL OCMMNT(NOUT,
     +              'as shown by the following variables that are')
               CALL OCMMNT(NOUT,
     +              'at the edge of their prescribed range :')
               CALL OBLNKL(NOUT)
               IFLAG = 1
            END IF
            XCVAL = XCM(II)*SCAFC(II)
            WRITE(NOUT,50) II,LABLXC(IXC(II)),XCVAL
 50         FORMAT(
     +           T4,'Variable ',I3,'  (',A8,')  is at its ',
     +           'upper bound of ',1PE12.4)
         END IF

 60   CONTINUE

C *** Print out information on numerics

      CALL OSUBHD(NOUT,'The solution vector is comprised as follows :')

      WRITE(NOUT,70)
 70   FORMAT(
     +     T47,'lower',
     +     T59,'upper')

      WRITE(NOUT,80)
 80   FORMAT(
     +     T23,'final',
     +     T33,'fractional',
     +     T46,'Lagrange',
     +     T58,'Lagrange')

      WRITE(NOUT,90)
 90   FORMAT(
     +     T5,'i',
     +     T23,'value',
     +     T35,'change',
     +     T45,'multiplier',
     +     T57,'multiplier')

      CALL OBLNKL(NOUT)

      DO 110 INN = 1,NVRBL
         XCS(INN) = XCM(INN)*SCAFC(INN)
         WRITE(NOUT,100) INN,LABLXC(IXC(INN)),XCS(INN),XCM(INN),
     +        VLAM(NEQNS+INN), VLAM(NEQNS+1+INN+NVRBL)
 100     FORMAT(T2,I4,T8,A8,T19,4(1PE12.4))
 110  CONTINUE

      CALL OSUBHD(NOUT,
     +    'The following constraint residues should be close to zero :')

      DO 130 INN = 1,NEQNS
         WRITE(NOUT,120) INN,LABLCC(ICC(INN)),RCM(INN),VLAM(INN)
 120     FORMAT(T2,I4,T8,A34,T45,1PE12.4,1PE12.4)
 130  CONTINUE

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE FINAL(IFAIL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to output the final point in the scan
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  3 October 1996
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 3.000 Initial upgraded version
C
C--Arguments
C  IFAIL  : (INPUT)  VMCON error flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'

C  Arguments
      INTEGER IFAIL

C  External routines
      EXTERNAL OHEADR,OUTPUT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IFAIL.EQ.1) THEN
         CALL OHEADR(NOUT,'Final Feasible Point')
      ELSE
         CALL OHEADR(NOUT,'Final UNFEASIBLE Point')
      END IF

      CALL OUTPUT(NOUT)

      WRITE(IOTTY,10) NFEV1,NFEV2,NCALLS
 10   FORMAT(
     +     T2,'The HYBRD point required ',I5,' iterations',/,
     +     T2,'The optimisation required ',I5,' iterations',/,
     +     T2,'There were ',I6,' function calls')

      RETURN
      END
