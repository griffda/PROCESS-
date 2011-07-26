C----------------------------------------------------------------------
      SUBROUTINE AVAIL(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : $Id: avail.f,v 1.1 1999/05/19 09:24:42 peter Exp pknight $
C  Module name    : $RCSfile: avail.f,v $
C  Version no.    : $Revision: 1.1 $
C  Creation date  : $Date: 1999/05/19 09:24:42 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C--Description
C  Routine to calculate component lifetimes and the overall plant
C  availability
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.4181
C
C--Date
C  19 May 1999
C
C--Reference
C  F/PL/PJK/PROCESS/CODE/043
C  
C--History
C  19/05/99 PJK 1.000 Initial version
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
      INCLUDE 'param.h'
      INCLUDE 'ife.h'
      INCLUDE 'cost.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'phydat.h'
      INCLUDE 'divrt.h'
      INCLUDE 'rfp.h'
      INCLUDE 'pulse.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION LB, LD, TD
      DOUBLE PRECISION UPLANNED, UUTOT
      SAVE UPLANNED, UUTOT

      INTEGER N

C  External routines
      EXTERNAL OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 100

C *** Full power lifetimes (in years)
C *** ===============================

C *** Most of these are already calculated for an IFE device

      IF (IFE.NE.1) THEN

C *** First wall / blanket

         BKTLIFE = MIN( ABKTFLNC / WALLMW, TLIFE )
         FWLIFE = BKTLIFE

C *** Divertor

         DIVLIFE = MIN( ADIVFLNC / HLDIV, TLIFE )
         IF (IRFP.EQ.1) DIVLIFE = 1.0D0

C *** Centrepost

         IF (ITART.EQ.1) THEN
            CPLIFE = MIN( CPSTFLNC / WALLMW, TLIFE )
         END IF

      END IF

C *** Plant Availability (Use new model if IAVAIL = 1)
C *** ==================

      IF (IAVAIL.EQ.1) THEN

C *** Which component has the shorter life?

         IF (DIVLIFE.LT.BKTLIFE) THEN
            LD = DIVLIFE
            LB = BKTLIFE
            TD = TDIVREPL
         ELSE
            LD = BKTLIFE
            LB = DIVLIFE
            TD = TBKTREPL
         END IF

C *** Number of outages between each combined outage

         N = INT(LB/LD) - 1

C *** Planned unavailability

         UPLANNED = (DBLE(N)*TD + TCOMREPL) /
     +        ( DBLE(N+1)*LD + (DBLE(N)*TD + TCOMREPL) )

C *** Unplanned unavailability
C *** Rather than simply summing the individual terms, the
C *** following protects against the total availability becoming zero
C *** or negative

         UUTOT = UUBOP
         UUTOT = UUTOT + (1.0D0 - UUTOT)*UUCD
         UUTOT = UUTOT + (1.0D0 - UUTOT)*UUDIV
         UUTOT = UUTOT + (1.0D0 - UUTOT)*UUFUEL
         UUTOT = UUTOT + (1.0D0 - UUTOT)*UUFW
         UUTOT = UUTOT + (1.0D0 - UUTOT)*UUMAG
         UUTOT = UUTOT + (1.0D0 - UUTOT)*UUVES

C *** Total availability

         CFACTR = 1.0D0 - (UPLANNED + UUTOT - (UPLANNED*UUTOT))

      END IF

C *** Modify lifetimes to take account of the availability

      IF (IFE.NE.1) THEN

C *** First wall / blanket

         IF (BKTLIFE.LT.TLIFE) THEN
            BKTLIFE = MIN( BKTLIFE / CFACTR, TLIFE )
            FWLIFE = BKTLIFE
         END IF

C *** Divertor

         IF ((DIVLIFE.LT.TLIFE).AND.(IRFP.NE.1)) THEN
            DIVLIFE = MIN( DIVLIFE / CFACTR, TLIFE )
         END IF

C *** Centrepost

         IF ((CPLIFE.LT.TLIFE).AND.(ITART.EQ.1)) THEN
            CPLIFE = MIN( CPLIFE / CFACTR, TLIFE )
         END IF

      END IF

C *** Current drive (assumed equal to first wall and blanket lifetime)

      CDRLIFE = BKTLIFE

      IF (IPRINT.EQ.0) GOTO 1000
 100  CONTINUE

C *** Output section

      CALL OHEADR(NOUT,'Plant Availability')
      CALL OVARRE(NOUT,'Allowable blanket neut. fluence (MW-yr/m2)',
     +     '(abktflnc)',ABKTFLNC)
      CALL OVARRE(NOUT,'Allowable divertor heat fluence (MW-yr/m2)',
     +     '(adivflnc)',ADIVFLNC)
      CALL OVARRE(NOUT,'First wall / blanket lifetime (years)',
     +     '(bktlife)',BKTLIFE)
      CALL OVARRE(NOUT,'Divertor lifetime (years)',
     +     '(divlife)',DIVLIFE)
      IF (ITART.EQ.1) THEN
         CALL OVARRE(NOUT,'Centrepost lifetime (years)',
     +        '(cplife)',CPLIFE)
      END IF
      CALL OVARRE(NOUT,'Current drive system lifetime (years)',
     +     '(cdrlife)',CDRLIFE)
      CALL OVARRE(NOUT,'Total plant lifetime (years)',
     +     '(tlife)',TLIFE)

      IF (IAVAIL.EQ.1) THEN
         IF (DIVLIFE.LT.BKTLIFE) THEN
            CALL OVARRE(NOUT,'Time needed to replace divertor (years)',
     +           '(tdivrepl)',TDIVREPL)
         ELSE
            CALL OVARRE(NOUT,'Time needed to replace blanket (years)',
     +           '(tbktrepl)',TBKTREPL)
         END IF
         CALL OVARRE(NOUT,
     +        'Time needed to replace blkt + div (years)',
     +        '(tcomrepl)',TCOMREPL)
         CALL OVARRE(NOUT,'Planned unavailability fraction',
     +        '(uplanned)',UPLANNED)
         CALL OVARRE(NOUT,'Unplanned unavailability fraction',
     +        '(uutot)',UUTOT)
      END IF
      CALL OVARRE(NOUT,'Total plant availability fraction',
     +     '(cfactr)',CFACTR)

 1000 CONTINUE

      RETURN
      END
