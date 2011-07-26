C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: acpow.f,v 3.3 1997/01/24 15:05:28 peter Exp $
C  Module name    : $RCSfile: acpow.f,v $
C  Version no.    : $Revision: 3.3 $
C  Creation date  : $Date: 1997/01/24 15:05:28 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE ACPOW(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.011
C
C--Description
C  Routine to calculate AC power requirements for the power plant.
C
C  The routine was drastically shortened on 23/01/90 (ORNL) from the
C  original TETRA routine to provide only the total power needs for
C  the plant. Included in STORAC in January 1992 by P.C. Shipe.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  22 January 1997
C
C--Reference
C  None
C  
C--History
C  --/--/92 PJK 1.000 Initial PROCESS version
C  20/01/97 PJK 1.010 Fixed error in pheatmw calculation, removed
C                     assignment of htpmw, and tidied up coding
C  22/01/97 PJK 1.011 Subsumed heattr.h, heatrinp.h and pfelect.h into
C                     htpwr.h
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit specifier
C  IPRINT : (INPUT)  Switch for turning off/on printing
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'bldgvol.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'estocom.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'osections.h'

C Arguments
      INTEGER IPRINT,NOUT

C  Local variables
      DOUBLE PRECISION BASEMW,BDVMW,CRYMW,PHEATMW,PKWPM2,PPFMW,PTFMW

C  External routines
      EXTERNAL OBLNKL,OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Power to TF coil power supplies, MW

      PTFMW = TFACPD

C *** Power to PF coil power supplies, MW

      PPFMW = 1.0D-3 * SRCKTPM
      IF (ISCENR.EQ.2) PPFMW = PPFMW + PEAKMVA

C *** Power to plasma heating supplies, MW

      PHEATMW = PINJWP

C *** Power to cryogenic comp. motors, MW

      CRYMW = CRYPMW

C *** Facility base load, MW (loads not dependent on floor area)

      BASEMW = BASEEL * 1.0D-6

C *** Power needed per floor area, kW/m2

      PKWPM2 = PWPM2 * 1.0D-3

C *** Power to divertor coil supplies, MW

      BDVMW = 0.0D0

C *** Total pulsed power system load, MW

      PACPMW = FMGDMW + PPFMW + BDVMW + PTFMW + CRYMW + VACHTMW +
     +     HTPMW + TRITHTMW + PHEATMW + BASEMW + EFLOOR*PKWPM2/1000.0D0

C *** Total power to facility loads, MW

      FCSHT  = BASEMW + EFLOOR*PKWPM2/1000.0D0 + 0.05D0*PACPMW

C *** Estimate of the total low voltage power, MW

      TLVPMW = FCSHT + TRITHTMW + HTPMW + VACHTMW + 0.5D0*(CRYMW+PPFMW)

      IF ((IPRINT.EQ.0).OR.(SECT17.EQ.0)) GOTO 1000

C *** Output section

      CALL OHEADR(NOUT,'AC Power')

      CALL OVARRE(NOUT,'Facility base load (MW)','(basemw)',BASEMW)
      CALL OVARRE(NOUT,'Divertor coil power supplies (MW)','(bdvmw)',
     +     BDVMW)
      CALL OVARRE(NOUT,'Cryogenic comp motors (MW)','(crymw)',CRYMW)
      CALL OVARRE(NOUT,'Total floor space (m2)','(efloor)',EFLOOR)
      CALL OVARRE(NOUT,'MGF units (MW)','(fmgdmw)',FMGDMW)
      CALL OVARRE(NOUT,'Heat transport system pump motors (MW)',
     +     '(htpmw)',HTPMW)
      CALL OVARRE(NOUT,'PF coil power supplies (MW)','(ppfmw)',PPFMW)
      CALL OVARRE(NOUT,'Power/floor area (kW/m2)','(pkwpm2)',PKWPM2)
      CALL OVARRE(NOUT,'TF coil power supplies (MW)','(ptfmw)',PTFMW)
      CALL OVARRE(NOUT,'Plasma heating supplies (MW)','(pheatmw)',
     +     PHEATMW)
      CALL OVARRE(NOUT,'Tritium processing (MW)','(trithtmw)',TRITHTMW)
      CALL OVARRE(NOUT,'Vacuum pump motors (MW)','(vachtmw)',VACHTMW)
      CALL OBLNKL(NOUT)
      CALL OVARRE(NOUT,'Total pulsed power (MW)','(pacpmw)',PACPMW)
      CALL OVARRE(NOUT,'Total facility power (MW)','(fcsht)',FCSHT)
      CALL OVARRE(NOUT,'Total low voltage power (MW)','(tlvpmw)',TLVPMW)

 1000 CONTINUE

      RETURN
      END
