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
C  Module         : $Id: supercond.f,v 3.3 1999/07/06 13:09:40 peter Exp pknight $
C
C  Module name    : $RCSfile: supercond.f,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1999/07/06 13:09:40 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      SUBROUTINE TFSPCALL(nout,iprint)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.001
C
C--Description
C  Subroutine to call the superconductor module for the TF coils.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  26 July 2011
C
C--Reference
C  None
C  
C--History
C  06/07/99 PJK 1.000 Added extra arguments to SUPERCON call
C  26/07/11 PJK 1.001 Added JCRIT_MODEL argument to SUPERCON call
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output channel identifier
C  IPRINT : (INPUT)  Switch denoting whether to write output to file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'tfcoil.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION ATURN,TFES,VDUMP
      INTEGER ISUP

C  External functions
      INTEGER  SUPERCON
      EXTERNAL SUPERCON

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Simple model

      IF (ITFMOD .EQ. 0) THEN
         VTFSKV = 20.D0
         GOTO 1000
      END IF

C *** Stored energy (J) and cross-sectional area per turn

      TFES = ESTOTF * 1.D9
      ATURN = RITFC/(JWPTF*TFNO*TURNSTF)

C+**PJK 26/07/11 New JCRIT_MODEL argument to SUPERCON
      ISUP = SUPERCON(ACSTF,ATURN,BMAXTFRP,VFTF,FCUTFSU,CPTTF,ISUMATTF,
     +     JCRIT_MODEL,STRNCON,TDMPTF,TFES,TFTMP,TMAXPRO,BCRITSC,
     +     JCRITSC,TCRITSC,IPRINT,NOUT,JWDGPRO,JWDGCRT,VDUMP,TMARGTF)

      IF (ISUP.NE.1) THEN
         WRITE(*,*) 'Error in routine TFSPCALL:'
         WRITE(*,*) 'SUPERCON returns with value ',ISUP
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      END IF

C *** Dump voltage in kV

      VTFSKV = VDUMP/1.D3

 1000 CONTINUE

      RETURN
      END

C----------------------------------------------------------------------
      INTEGER FUNCTION SUPERCON(ACS,ATURN,BMAX,FHE,FCU,
     +     IOP,ISUMAT,JCRIT_MODEL,STRAIN,TDUMP,TFES,THE,TMAX,BCRITSC,
     +     JCRITSC,TCRITSC,IPRINT,NOUT,JWDGPRO,JWDGCRT,VD,TMARG)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.001
C
C--Description
C  Function to calculate the superconductor properties.
C  Programmed by J. Galambos, 5/06/91, FEDC, ORNL (615-576-5482)
C  Algorithms provided by J. Miller. These are the ITER rules for
C  Nb3Sn modelling. The routine calculates the critical current density
C  (winding pack) and also the protection information (for a quench).
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  26 July 2011
C
C--Reference
C  None
C  
C--History
C  06/07/99 PJK 1.000 Added new generic superconductor options
C  26/07/11 PJK 1.001 Corrected denominator in JC calculation;
C                     Added option to use new Jcrit model for binary Nb3Sn
C
C--Arguments
C  ACS     : (INPUT)  cable space - inside area (m2)
C  ATURN   : (INPUT)  area per turn (i.e.  entire cable) (m2)
C  BMAX    : (INPUT)  peak field at conductor (T)
C  FHE     : (INPUT)  fraction of cable space that is for He cooling
C  FCU     : (INPUT)  fraction of conductor that is copper
C  IOP     : (INPUT)  operating current per turn (A)
C  ISUMAT  : (INPUT)  switch for conductor type:
C                     1 = binary Nb3Sn,
C                     2 = ternary Nb3Sn,
C                     3 = NbTi,
C                     4 = generic, but uses Nb3Sn current density calc.
C                     5 = generic, but uses NbTi current density calc.
C  JCRIT_MODEL : (INPUT)  switch for Jcrit model for isumat=1 only:
C                     0 = original model
C                     1 = ITER Nb3Sn critical surface implementation
C  STRAIN  : (INPUT)  strain on superconductor at operation conditions
C  TDUMP   : (INPUT)  dump time (sec)
C  TFES    : (INPUT)  energy stored in one TF coil (J)
C  THE     : (INPUT)  He temperature at peak field point (K)
C  TMAX    : (INPUT)  max conductor temperature during quench (K)
C  BCRITSC : (INPUT)  critical field (T) (isumat=4,5 only)
C  JCRITSC : (INPUT)  critical J (A/m2) (isumat=4,5 only)
C  TCRITSC : (INPUT)  critical temperature (K) (isumat=4,5 only)
C  IPRINT  : (INPUT)  switch for printing (1 = yes, 0 = no)
C  NOUT    : (INPUT)  unit for output
C  JWDGPRO : (OUTPUT) winding pack current density from temperature 
C                     rise protection (A/m2)
C  JWDGCRT : (OUTPUT) critical winding pack current density (A/m2)
C  VD      : (OUTPUT) Discharge voltage imposed on a TF coil (V)
C  TMARG   : (OUTPUT) temperature margin (K)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'osections.h'

C  Arguments
      DOUBLE PRECISION ACS,ATURN,BMAX,FHE,FCU,IOP,STRAIN,TDUMP,
     +     TFES,THE,TMAX,BCRITSC,JCRITSC,TCRITSC,JWDGPRO,JWDGCRT,
     +     VD,TMARG
      INTEGER IPRINT,ISUMAT,NOUT

C  Local variables
      DOUBLE PRECISION ASTRAIN, BBAR, BC2, BC20, BC20M, CSTRAIN,
     +     C0, FAC1, FAC2, FCOND, FSTRAIN, ICRIT, IOOIC, JC, JWDGOP, 
     +     TBAR, TC0, TC0M, TC1

C  External routines
      EXTERNAL PROTECT,OHEADR,OSUBHD,OVARRE,OCMMNT,OBLNKL

C  External functions
      DOUBLE PRECISION ITERSC
      EXTERNAL ITERSC

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Conductor fraction

      FCOND = 1.0D0 - FHE

C *** Find critical current density

      IF (ISUMAT.EQ.1) THEN

C        *** binary Nb3Sn data
         BC20M = 28.0D0
         TC0M = 18.0D0
         C0 = 1.315D10

      ELSE IF (ISUMAT.EQ.2) THEN

C        *** ternary Nb3Sn data
         BC20M = 24.0D0
         TC0M = 16.0D0
         C0 = 2.225D10

      ELSE IF (ISUMAT.EQ.3) THEN

C        *** NbTi data
         BC20M = 15.D0
         TC0M = 9.3D0
         C0 = 10.0D9

      ELSE

C        *** generic superconductor - use input values
         BC20M = BCRITSC
         TC0M = TCRITSC
         C0 = JCRITSC

      END IF

      IF (STRAIN .LT. 0.0D0) THEN
         ASTRAIN = 900.D0
      ELSE
         ASTRAIN = 1250.D0
      END IF

      FSTRAIN = 1.D0 - ASTRAIN * ABS(STRAIN)**1.7D0

      CSTRAIN = C0 * SQRT(FSTRAIN)

C *** Calculate current density, taking into account critical values

      IF ((ISUMAT.NE.3).AND.(ISUMAT.NE.5)) THEN
C        *** Nb3Sn model

C+**PJK 26/07/11 Option to use RK's new routine, if ISUMAT=1 (binary Nb3Sn) only
         IF ((ISUMAT.EQ.1).AND.(JCRIT_MODEL.EQ.1)) THEN
            JC = ITERSC(THE,BMAX,STRAIN)
         ELSE
            TC0 = TC0M * FSTRAIN ** 0.33333D0
            TC1 = TC0M * (1.D0 - BMAX/BC20M)
            BC20 = BC20M * FSTRAIN
            TBAR = THE/TC0
            TBAR = MIN(TBAR,0.999D0)
            BC2 = BC20 * (1.D0 - TBAR**2) * (1.D0 - 0.31D0*TBAR**2 *
     +           (1.D0 - 1.77D0*LOG(TBAR) ) )
            BBAR = BMAX/BC2
            BBAR = MIN(BBAR,0.999D0)
C+**PJK 25/07/11 Corrected SQRT(BBAR) to SQRT(BMAX) in denominator
            JC = CSTRAIN * (1.D0 - TBAR**2)**2 * (1.D0 - BBAR)**2 /
     +           (SQRT(BC2) * SQRT(BMAX) )
         END IF

      ELSE
C        *** NbTi model
         TC1 = TC0M * (1.D0 - BMAX/BC20M)**0.59D0
         TC1 = MAX(TC1, 0.001D0)
         TBAR = MAX( (1.D0 - THE/TC1), 0.001D0)
         JC = C0 * (1.D0 - BMAX/BC20M) * TBAR

      END IF

C *** Critical current

      ICRIT = JC * ACS * (1.D0 - FHE) * (1.D0 - FCU)

C *** Critical current density in winding pack

      JWDGCRT = ICRIT / ATURN

C *** Ratio of operating / critical current

      IOOIC = IOP / ICRIT

C *** Operating current density

      JWDGOP = IOP / ATURN

C *** Temperature margin

      FAC1 = MAX( 0.01D0, (1.D0 - IOOIC) )
      FAC2 = MAX( 0.01D0, (TC1-THE) )
      TMARG = FAC1 * FAC2

C *** Find the current density limited by the protection limit

      CALL PROTECT(IOP,TFES,ACS,ATURN,TDUMP,FCOND,FCU,THE,TMAX,
     +     JWDGPRO,VD)

      SUPERCON = 1

      IF ((IPRINT.EQ.0).OR.(SECT07.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'Superconducting TF Coils')

      IF (ISUMAT.EQ.1) THEN
         CALL OCMMNT(NOUT,'Superconductor used: Nb3Sn (binary)')
         IF (JCRIT_MODEL.EQ.0) THEN
            CALL OCMMNT(NOUT,'  (original Jcrit model)')
         ELSE
            CALL OCMMNT(NOUT,'  (ITER Jcrit model)')
         END IF
      ELSE IF (ISUMAT.EQ.2) THEN
         CALL OCMMNT(NOUT,'Superconductor used: Nb3Sn (ternary)')
      ELSE IF (ISUMAT.EQ.3) THEN
         CALL OCMMNT(NOUT,'Superconductor used: NbTi')
      ELSE IF (ISUMAT.EQ.4) THEN
         CALL OCMMNT(NOUT,
     +      'Generic superconductor used: Nb3Sn current density model')
      ELSE
         CALL OCMMNT(NOUT,
     +      'Generic superconductor used: NbTi current density model')
      END IF
      CALL OBLNKL(NOUT)

      CALL OVARRE(NOUT,'Peak field at conductor (T)','(bmax)',BMAX)
      CALL OVARRE(NOUT,'Helium temperature at peak field (K)','(the)',
     +     THE)
      CALL OVARRE(NOUT,'Helium fraction inside cable space','(fhe)',FHE)
      CALL OVARRE(NOUT,'Copper fraction of conductor','(fcu)',FCU)

      CALL OSUBHD(NOUT,'Critical Current Information :')
      CALL OVARRE(NOUT,'Operating winding pack J (A/m2)',
     +     '(jwdgop)',JWDGOP)
      CALL OVARRE(NOUT,'Critical winding pack curr. density (A/m2)',
     +     '(jwdgcrt)',JWDGCRT)
      CALL OVARRE(NOUT,'Critical current (A)','(icrit)',ICRIT)
      CALL OVARRE(NOUT,'Operating current / critical current','(iooic)',
     +     IOOIC)
      CALL OVARRE(NOUT,'Temperature margin (K)','(tmarg)',TMARG)

      CALL OSUBHD(NOUT,'Protection Information :')
      CALL OVARRE(NOUT,'Maximum temperature in quench (K)','(tmax)',
     +     TMAX)
      CALL OVARRE(NOUT,'Winding pack protection J (A/m2)','(jwdgpro)',
     +     JWDGPRO)
      CALL OVARRE(NOUT,'Dump time (s)','(tdump)',TDUMP)
      CALL OVARRE(NOUT,'Dump voltage (V)','(vd)',VD)

 1000 CONTINUE

      RETURN
      END

C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION itersc(the, bmax, strain)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 0.100
C
C--Description
C  Implementation of ITER Nb3Sn critical surface implementation.
C
C--Author
C  Richard Kemp D3/1/86 CCFE ext. 6438
C
C--Date
C  21 July 2011
C
C--Reference
C  ITER Nb3Sn critical surface parameterization (2MMF7J) (2008)
C  https://user.iter.org/?uid=2MMF7J&action=get_document
C  ITER DDD 11-7: Magnets - conductors (2NBKXY) (2009)
C  https://user.iter.org/?uid=2NBKXY&action=get_document
C  
C--History
C  21/07/11 RK 0.100 First draft of routine
C
C--Arguments
C  THE    : (INPUT)  Coolant/SC temperature
C  BMAX   : (INPUT)  Magnetic field at conductor
C  STRAIN : (INPUT)  Strain in conductor
C  ITERSC : (OUTPUT) J_crit in superconductor
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION BMAX,THE,STRAIN

C  Internal
      DOUBLE PRECISION csc, bctw, tco, cp, cq
      DOUBLE PRECISION caone, catwo, etaoa, etamax
      DOUBLE PRECISION tcrit, tred, bcrit, bred, etash, tzero
      DOUBLE PRECISION bcro, tcro, bzero, strfun, jc, jc1, jc2, jc3
      
C  Constants
      csc = 16500.d6
      bctw = 32.97d0
      tco = 16.06d0
      cp = 0.63d0
      cq = 2.1d0
      caone = 44.d0
      catwo = 4.d0
      etaoa = 0.00256d0
      etamax = -0.003253075d0
      
C  Strain function
      etash = (catwo*etaoa)/(sqrt(caone**2 - catwo**2))
      strfun = sqrt(etash**2 + etaoa**2) - sqrt((strain-etash)**2
     +     + etaoa**2)
      strfun = strfun*caone - catwo*strain
      strfun = 1.d0 + (1.d0/(1.d0 - caone*etaoa))*strfun
      
C  cros
      bcro = (bctw*strfun)
      tcro = tco*(strfun**0.333333)
      
C  tred and bred
      tzero = the/tcro
      bzero = bmax/bcro
      bcrit = bcro * (1.d0 - tzero**1.52d0)
      bred = bmax/bcrit
      tcrit = tcro * (1.d0 - bzero)**(1.d0/1.52d0)
      tred = the/tcrit

C jc
      jc1 = (csc/bmax)*strfun
      jc2 = (1.d0-tzero**1.52)*(1.d0-tzero**2.d0)
      jc3 = bred**cp * (1.d0-bred)**cq
      jc = jc1 * jc2 * jc3
            
      itersc = jc
            
      return
      end
C----------------------------------------------------------------------
      SUBROUTINE PROTECT(AIO,TFES,ACS,ATURN,TDUMP,FCOND,FCU,TBA,TMAX,
     +     AJWPRO,VD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calculates maximum conductor current density which
C  limits the peak temperature in the winding to a given limit (tmax).
C  It also finds the dump voltage.
C  These calculations are based on Miller's formulations.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  06 July 1999
C
C--Reference
C  None
C  
C--History
C  06/07/99 PJK 1.000 Initial upgraded version
C
C--Arguments
C  AIO     : (INPUT)  operating current (A)
C  TFES    : (INPUT)  energy stored in one TF coil (J)
C  ACS     : (INPUT)  cable space - inside area (m2)
C  ATURN   : (INPUT)  area per turn (i.e.  entire cable) (m2)
C  TDUMP   : (INPUT)  dump time (sec)
C  FCOND   : (INPUT)  fraction of cable space containing conductor
C  FCU     : (INPUT)  fraction of conductor that is copper
C  TBA     : (INPUT)  He temperature at peak field point (K)
C  TMAX    : (INPUT)  max conductor temperature during quench (K)
C  AJWPRO  : (OUTPUT) winding pack current density from temperature 
C                     rise protection (A/m2)
C  VD      : (OUTPUT) Discharge voltage imposed on a TF coil (V)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION AIO,VD,TFES,ATURN,ACS,TDUMP,FCOND,FCU,
     +     TBA,TMAX,AJWPRO

C  Local variables
      DOUBLE PRECISION P1(11),P2(11),P3(11)
      DOUBLE PRECISION AA,AI1,AI2,AI3,AJCP,BB,CC,DD,TAV
      INTEGER NO,NP

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Integration coefficients p1,p2,p3.

      P1(1) = 0.D0
      P1(2) = 0.8D0
      P1(3) = 1.75D0
      P1(4) = 2.4D0
      P1(5) = 2.7D0
      P1(6) = 2.95D0
      P1(7) = 3.1D0
      P1(8) = 3.2D0
      P1(9) = 3.3D0
      P1(10) = 3.4D0
      P1(11) = 3.5D0

      P2(1) = 0.D0
      P2(2) = 0.05D0
      P2(3) = 0.5D0
      P2(4) = 1.4D0
      P2(5) = 2.6D0
      P2(6) = 3.7D0
      P2(7) = 4.6D0
      P2(8) = 5.3D0
      P2(9) = 5.95D0
      P2(10) = 6.55D0
      P2(11) = 7.1D0

      P3(1) = 0.D0
      P3(2) = 0.05D0
      P3(3) = 0.5D0
      P3(4) = 1.4D0
      P3(5) = 2.6D0
      P3(6) = 3.7D0
      P3(7) = 4.6D0
      P3(8) = 5.4D0
      P3(9) = 6.05D0
      P3(10) = 6.8D0
      P3(11) = 7.2D0

C *** Dump voltage

      VD = 2.D0 * TFES/(TDUMP*AIO)

C *** Current density limited by temperature rise during quench

      TAV = 1.D0+(TMAX-TBA)/20.D0
      NO = INT(TAV)
      NP = NO+1
      NP = MIN(NP,11)
      AI1 = 1.D16*( P1(NO)+(P1(NP)-P1(NO)) * (TAV-DBLE(NO)) )
      AI2 = 1.D16*( P2(NO)+(P2(NP)-P2(NO)) * (TAV-DBLE(NO)) )
      AI3 = 1.D16*( P3(NO)+(P3(NP)-P3(NO)) * (TAV-DBLE(NO)) )

      AA = VD*AIO/TFES
      BB = (1.D0-FCOND)*FCOND*FCU*AI1
      CC = (FCU*FCOND)**2*AI2
      DD = (1.D0-FCU)*FCU*FCOND**2*AI3
      AJCP = SQRT( AA* (BB+CC+DD) )
      AJWPRO = AJCP*(ACS/ATURN)

      RETURN
      END
