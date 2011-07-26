C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: caller.f,v 3.14 2006/05/25 09:27:02 pknight Exp $
C  Module name    : $RCSfile: caller.f,v $
C  Version no.    : $Revision: 3.14 $
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE CALLER(XC,NVARS)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.510
C
C--Description
C  Routine to call the physics and engineering modules
C
C--Author
C  Peter Knight D3/162a Culham Laboratory, ext.6368
C
C--Date
C  24 May 2006
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved code layout
C  29/01/96 PJK 1.100 Added routine CNTRPST
C  23/01/97 PJK 1.200 Split routine POWER into POWER1 and POWER2
C  06/02/97 PJK 1.300 Added routine LOCA
C  21/03/97 PJK 1.400 Added routine IFECLL
C  18/11/97 PJK 1.410 Removed NOUT argument from FISPAC call
C  19/05/99 PJK 1.500 Added routine AVAIL
C  24/05/06 PJK 1.510 Moved call to STRUCALL after DIVCALL
C
C--Arguments
C  XC     : (INPUT)  Array of iteration variables
C  NVARS  : (INPUT)  Number of active iteration variables
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'phydat.h'
      INCLUDE 'stella.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'

C  Arguments
      DOUBLE PRECISION XC(IPNVARS)
      INTEGER NVARS

C  External routines
      EXTERNAL ACPOW,AVAIL,BLDGCALL,CNTRPST,CONVXC,COSTS,DIVCALL,ECH,
     +     FISPAC,FWBS,GEOMTY,IFECLL,INDUCT,LOCA,LWHYMOD,NBEAM,PFCOIL,
     +     PFPWR,PHYSICS,POWER1,POWER2,PULSE,RADIALB,RFPPFC,RFPPFP,
     +     RFPPHY,RFPTFC,STARTUP,STCALL,STRUCALL,TFCOIL,TFPWR,TFSPCALL,
     +     VACCALL,VBUILD,VSEC

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Increment the call counter

      NCALLS = NCALLS + 1

C *** Convert variables

      CALL CONVXC(XC,NVARS)

C *** Perform function calls
C *** ======================

C *** Stellarator calls

      IF (ISTELL.NE.0) THEN
         CALL STCALL
         GOTO 1000
      END IF

C *** Inertial Fusion Energy calls

      IF (IFE.NE.0) THEN
         CALL IFECLL
         GOTO 1000
      END IF

C *** Tokamak and RFP calls

      CALL GEOMTY

      CALL RADIALB(0,NOUT)

      CALL VBUILD

C+**PJK 06/03/96
      IF (IRFP.EQ.0) THEN
         CALL PHYSICS
      ELSE
         CALL RFPPHY
      END IF

C+**PJK 25/11/93 Comment out start-up option as it takes a long time
C+**PJK 25/11/93      CALL STARTUP(0)

      CALL ECH(NOUT,0)

      CALL LWHYMOD(NOUT,0)

      CALL NBEAM(NOUT,0) 

C+**PJK 27/02/96
      IF (IRFP.EQ.0) THEN
         CALL TFCOIL(NOUT,0)
      ELSE
         CALL RFPTFC(NOUT,0)
      END IF

      CALL TFSPCALL(NOUT,0)

C+**PJK 01/03/96
      IF (IRFP.EQ.0) THEN
         CALL PFCOIL
      ELSE
         CALL RFPPFC(NOUT,0)
      END IF

C+**PJK 24/05/06 Old location...      CALL STRUCALL(NOUT,0)

C+**PJK 01/03/96
      IF (IRFP.EQ.0) THEN
         CALL INDUCT(0,NOUT)
         CALL VSEC
      END IF

      CALL PULSE(NOUT,0)

      CALL FWBS(NOUT,0)

      CALL DIVCALL(0,NOUT)

C+**PJK 24/05/06 New location...
      CALL STRUCALL(NOUT,0)

C+**PJK 29/01/96
      IF (ITART.EQ.1) CALL CNTRPST(NOUT,0)

      CALL TFPWR(NOUT,0)

C+**PJK 01/03/96
      IF (IRFP.EQ.0) THEN
         CALL PFPWR(NOUT,0)
      ELSE
         CALL RFPPFP(NOUT,0)
      END IF

C+**PJK 23/01/97      CALL POWER(NOUT,0)

      CALL POWER1

      CALL VACCALL(NOUT,0)

      CALL BLDGCALL(NOUT,0)

      CALL ACPOW(NOUT,0)

C+**PJK 23/01/97
      CALL POWER2(NOUT,0)

C+**PJK 10/05/99
      CALL AVAIL(NOUT,0)

      CALL COSTS(NOUT,0)

C+**PJK      IF (IFISPACT.EQ.1) THEN
C+**PJK         CALL FISPAC(0)
C+**PJK         CALL LOCA(NOUT,0)
C+**PJK      END IF

 1000 CONTINUE

      RETURN
      END
