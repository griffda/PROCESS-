C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: safety.f,v 1.3 1999/07/06 13:07:29 peter Exp $
C  Module name    : $RCSfile: safety.f,v $
C  Version no.    : $Revision: 1.3 $
C  Creation date  : $Date: 1999/07/06 13:07:29 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE LOCA(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.110
C
C--Description
C  Routine to link the Loss Of Coolant Accident model to the rest of
C  PROCESS. The model calculates the steady state temperatures that
C  would develop following a loss of coolant accident.
C
C--Author
C  Peter Knight D3/162a Culham Laboratory, ext.4181
C  Cleve Forty  D3/209b Culham Laboratory, ext.3232
C  Winston Han  D3/182  Culham Laboratory, ext.4202
C
C--Date
C  06 July 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK12, pp.70,71,72,73
C  Strategic Studies Note 96/30, January 1997
C  
C--History
C  06/02/97 PJK 1.000 Initial version
C  26/02/97 PJK 1.100 Corrected cases where there is no OH coil
C  06/07/99 PJK 1.110 Allowed for use of more ISUMATTF options
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit identifier
C  IPRINT : (INPUT)  Flag to turn on/off (1/0) output to file
C
C--Global variables passed via COMMON
C  ALPHA  : (OUTPUT) heat transfer coefficient at outer boundary
C  QVOL   : (OUTPUT) decay heat power in each region (W)
C  MATFRC : (OUTPUT) material fractions in each region
C  RADMID : (OUTPUT) mean radial coordinate of each region
C  RADMIN : (OUTPUT) minus (inside) radial coordinate of each region (m)
C  RADPLS : (OUTPUT) plus (outside) radial coordinate of each region (m)
C  TAIR   : (OUTPUT) ambient air temperature (K)
C  NPATH  : (OUTPUT) switch for heat rejection model:
C                    0 = convection only
C                    1 = convection + radiation
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      INTEGER NREG,NMAT
      PARAMETER (NREG = 15, NMAT = 13)

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'blanket.h'
      INCLUDE 'fispact.h'

C  Arguments
      INTEGER IPRINT,NOUT

C  Global variables
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION QVOL(NREG)
      DOUBLE PRECISION MATFRC(NREG,NMAT)
      DOUBLE PRECISION RADMID(NREG)
      DOUBLE PRECISION RADMIN(NREG)
      DOUBLE PRECISION RADPLS(NREG)
      DOUBLE PRECISION TAIR
      INTEGER NPATH

      COMMON/GEOM/RADMIN,RADPLS,RADMID,MATFRC,QVOL

      COMMON/EDGET/TAIR,ALPHA,NPATH

C  Local variables
      DOUBLE PRECISION AC,AS,AV,A1
      INTEGER I,K,IBC,ITF,IOH

C  External routines
      EXTERNAL BLCYL3,OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 40

C *** Heat transfer coefficient and air temperature

      ALPHA = 6.5D0
      TAIR = 300.0D0

C *** NPATH=1 does a convection + radiation calculation
C *** NPATH=0 does a convection only calculation

      NPATH = 0

C *** Radial build
C *** ============

      RADMIN(1)  = 1.0D-20
      RADPLS(1)  = RADMIN(1) + BORE
      RADMIN(2)  = RADPLS(1)

      IF (ITART.EQ.0) THEN
         RADPLS(2) = RADMIN(2) + OHCTH
         RADMIN(3) = RADPLS(2) + GAPOH
         RADPLS(3) = RADMIN(3) + BCYLTH
         RADMIN(4) = RADPLS(3)
         RADPLS(4) = RADMIN(4) + TFCTH
      ELSE
         RADPLS(2) = RADMIN(2) + BCYLTH
         RADMIN(3) = RADPLS(2)
         RADPLS(3) = RADMIN(3) + TFCTH
         RADMIN(4) = RADPLS(3) + GAPOH
         RADPLS(4) = RADMIN(4) + OHCTH
      END IF

      RADMIN(5)  = RADPLS(4)
      RADPLS(5)  = RADMIN(5) + DDWI
      RADMIN(6)  = RADPLS(5) + GAPDS
      RADPLS(6)  = RADMIN(6) + SHLDITH
      RADMIN(7)  = RADPLS(6)
      RADPLS(7)  = RADMIN(7) + BLNKITH
      RADMIN(8)  = RADPLS(7)
      RADPLS(8)  = RADMIN(8) + FWITH
      RADMIN(9)  = RADPLS(8)
      RADPLS(9)  = RADMIN(9) + SCRAPLI + 2.0D0*RMINOR + SCRAPLO
      RADMIN(10) = RADPLS(9)
      RADPLS(10) = RADMIN(10) + FWOTH
      RADMIN(11) = RADPLS(10)
      RADPLS(11) = RADMIN(11) + BLNKOTH
      RADMIN(12) = RADPLS(11)
      RADPLS(12) = RADMIN(12) + SHLDOTH
      RADMIN(13) = RADPLS(12) + GAPSTO
      RADPLS(13) = RADMIN(13) + DDWI
      RADMIN(14) = RADPLS(13)
      RADPLS(14) = RADMIN(14) + TFTHKO
      RADMIN(15) = RADPLS(14) + 2.0D0
      RADPLS(15) = RADMIN(15) + DDWEX

      DO 20 K = 1,NREG
         RADMID(K) = 0.5D0 * (RADMIN(K) + RADPLS(K))
         QVOL(K) = 0.0D0
         DO 10 I = 1,NMAT
            MATFRC(K,I) = 0.0D0
 10      CONTINUE
 20   CONTINUE

C *** Non-zero material fractions MATFRC(REGION,MATERIAL).
C *** The materials are:
C *** 1  = Martensitic steel
C *** 2  = Austenitic steel
C *** 3  = Lithium lead breeder
C *** 4  = Beryllium
C *** 5  = Dummy (air)
C *** 6  = Plasma
C *** 7  = Copper
C *** 8  = Lead
C *** 9  = Lithium oxide
C *** 10 = Liquid lithium
C *** 11 = Vanadium alloy
C *** 12 = Nb3Sn superconductor
C *** 13 = NbTi superconductor
C *** This list should tally with that in block data BDCOND

C *** Take account of different builds for TART and non-TART devices
C *** IBC = bucking cylinder region number
C *** ITF = inboard TF coil region number
C *** IOH = OH coil region number

      IF (ITART.EQ.0) THEN
         IBC = 3
         ITF = 4
         IOH = 2
      ELSE
         IBC = 2
         ITF = 3
         IOH = 4
      END IF

C *** Machine bore:

      MATFRC(1,5) = 1.0D0

C *** OH coil:

      IF (IOHCL.EQ.1) THEN
         IF (IPFRES.EQ.1) THEN

            MATFRC(IOH,5) = VFOHC
            MATFRC(IOH,7) = 1.0D0 - VFOHC

         ELSE

C           *** Conductor + void areas

            A1 = (RB(NOHC)-RA(NOHC))*(ZH(NOHC)-ZL(NOHC))
            AC = A1 * (1.0D0 - VFOHC)
            AV = A1 * VFOHC

C           *** Steel cross-sectional area

            AS = WTS(NOHC) / (2.0D0*PI*RPF(NOHC) * 7800.0D0)

            MATFRC(IOH,1) = AS / (AC+AV+AS) * FMSOH
            MATFRC(IOH,2) = AS / (AC+AV+AS) * (1.0D0 - FMSOH)
            MATFRC(IOH,5) = AV / (AC+AV+AS)
            MATFRC(IOH,7) = AC / (AC+AV+AS) * (1.0D0-SCCUFAC*BPF(NOHC))

            IF ((ISUMATPF.EQ.1).OR.(ISUMATPF.EQ.2)) THEN
               MATFRC(IOH,12) = AC / (AC+AV+AS) * SCCUFAC*BPF(NOHC)
            ELSE
               MATFRC(IOH,13) = AC / (AC+AV+AS) * SCCUFAC*BPF(NOHC)
            END IF

         END IF

      ELSE

         MATFRC(IOH,5) = 1.0D0

      END IF

C *** Bucking cylinder:

      MATFRC(IBC,1) = FMSBC
      MATFRC(IBC,2) = 1.0D0 - FMSBC

C *** Inboard TF coil:

      IF (ITFSUP.EQ.0) THEN

         MATFRC(ITF,5) = FCOOLCP
         MATFRC(ITF,7) = 1.0D0 - FCOOLCP

      ELSE

C        *** Approximate total cross-sectional area per coil

         A1 = ACASETF+ACOND+AVWP+ASWP

         MATFRC(ITF,1) = (ACASETF + ASWP) / A1 * FMSTF
         MATFRC(ITF,2) = (ACASETF + ASWP) / A1 * (1.0D0 - FMSTF)
         MATFRC(ITF,5) = AVWP / A1
         MATFRC(ITF,7) = ACOND / A1 * FCUTFSU

         IF (ISUMATTF.NE.3) THEN
C           *** treat generic superconductors like Nb3Sn
            MATFRC(ITF,12) = ACOND / A1 * (1.0D0 - FCUTFSU)
         ELSE
            MATFRC(ITF,13) = ACOND / A1 * (1.0D0 - FCUTFSU)
         END IF

      END IF

C *** Inboard dewar:

      MATFRC(5,1) = FMSDWI
      MATFRC(5,2) = 1.0D0 - FMSDWI

C *** Inboard shield:

      MATFRC(6,1) = (1.0D0 - VFSHLD) * FMSSH
      MATFRC(6,2) = (1.0D0 - VFSHLD) * (1.0D0 - FMSSH)
      MATFRC(6,5) = VFSHLD

C *** Inboard blanket:

      IF (LBLNKT.NE.1) THEN

C        *** Old blanket model

         MATFRC(7,1) = FBLSS * FMSBL
         MATFRC(7,2) = FBLSS * (1.0D0-FMSBL)
         MATFRC(7,4) = FBLBE
         MATFRC(7,5) = VFBLKT
         MATFRC(7,9) = FBLLI2O
         MATFRC(7,11) = FBLVD

      ELSE

C        *** New blanket model

         IF (SMSTR.EQ.1) THEN

C           *** Li2O/Be solid blanket

            MATFRC(7,1) = FBLSS * FMSBL
            MATFRC(7,2) = FBLSS * (1.0D0-FMSBL)
            MATFRC(7,4) = FBLBE
            MATFRC(7,5) = VFBLKT
            MATFRC(7,9) = FBLLI2O
            MATFRC(7,11) = FBLVD

         ELSE

C           *** LiPb/Li liquid blanket

            MATFRC(7,1) = FBLSS * FMSBL
            MATFRC(7,2) = FBLSS * (1.0D0-FMSBL)
            MATFRC(7,3) = FBLLIPB
            MATFRC(7,5) = VFBLKT
            MATFRC(7,10) = FBLLI
            MATFRC(7,11) = FBLVD

         END IF

      END IF

C *** Inboard first wall:

      MATFRC(8,1) = (1.0D0 - FWCLFR) * FMSFW
      MATFRC(8,2) = (1.0D0 - FWCLFR) * (1.0D0 - FMSFW)
      MATFRC(8,5) = FWCLFR

C *** Plasma:

      MATFRC(9,6) = 1.0D0

C *** Outboard first wall:

      MATFRC(10,1) = (1.0D0 - FWCLFR) * FMSFW
      MATFRC(10,2) = (1.0D0 - FWCLFR) * (1.0D0 - FMSFW)
      MATFRC(10,5) = FWCLFR

C *** Outboard blanket (same as inboard blanket):

      DO 30 I = 1,NMAT
         MATFRC(11,I) = MATFRC(7,I)
 30   CONTINUE

C *** Outboard shield:

      MATFRC(12,1) = (1.0D0 - VFSHLD) * FMSSH
      MATFRC(12,2) = (1.0D0 - VFSHLD) * (1.0D0 - FMSSH)
      MATFRC(12,5) = VFSHLD

C *** Outboard dewar:

      MATFRC(13,1) = FMSDWI
      MATFRC(13,2) = 1.0D0 - FMSDWI

C *** Outboard TF coil:

      IF (ITFSUP.EQ.0) THEN

         MATFRC(14,5) = VFTF
         MATFRC(14,7) = 1.0D0 - VFTF

      ELSE

C        *** Approximate total cross-sectional area per coil
C        *** including enlargement factor for case thickness

         A1 = (ACASETF*TFOOTFI)+ACOND+AVWP+ASWP

         MATFRC(14,1) = (ACASETF*TFOOTFI + ASWP) / A1 * FMSTF
         MATFRC(14,2) = (ACASETF*TFOOTFI + ASWP) / A1 * (1.0D0 - FMSTF)
         MATFRC(14,5) = AVWP / A1
         MATFRC(14,7) = ACOND / A1 * FCUTFSU

         IF (ISUMATTF.NE.3) THEN
C           *** treat generic superconductors like Nb3Sn
            MATFRC(14,12) = ACOND / A1 * (1.0D0 - FCUTFSU)
         ELSE
            MATFRC(14,13) = ACOND / A1 * (1.0D0 - FCUTFSU)
         END IF

      END IF

C *** External dewar:

      MATFRC(15,1) = FMSDWE
      MATFRC(15,2) = 1.0D0 - FMSDWE

C *** Non-zero decay heat powers (W) after 3 months
C *** =============================================

      QVOL(7)  = BLIHKW(2) * 1.0D3
      QVOL(8)  = FWIHKW(2) * 1.0D3
      QVOL(10) = FWOHKW(2) * 1.0D3
      QVOL(11) = BLOHKW(2) * 1.0D3

C *** Calculate the temperature at the plasma - outboard first wall
C *** interface (K)

      CALL BLCYL3(FWTEMP)

 40   CONTINUE

C *** Output section

      IF (IPRINT.NE.1) GOTO 1000

      CALL OHEADR(NOUT,'Loss of Coolant Accident')

      CALL OVARRE(NOUT,'First wall temperature after 3 months (K)',
     +     '(fwtemp)',FWTEMP)

 1000 CONTINUE

      RETURN
      END
C-----------------------------------------------------------------------
      BLOCK DATA BDCOND

C *** THERMAL CONDUCTIVITY BLOCK DATA

C**********************************************************************
C THIS CONTAINS THE COEFFICIENTS FOR THE THERMAL CONDUCTIVITY         *
C POLYNOMIALS AND ALSO A VALUE FOR THE DENSITY.                       *
C THE MATERIALS CONSIDERED ARE, IN ORDER:                             *
C 1)  MARTENSITIC STEEL                                               *
C 2)  AUSTENITIC STEEL                                                *
C 3)  LITHIUM LEAD BREEDER                                            *
C 4)  BERYLLIUM                                                       *
C 5)  DUMMY1 - AIR                                                    *
C 6)  PLASMA                                                          *
C 7)  COPPER                                                          *
C 8)  LEAD                                                            *
C 9)  LITHIUM OXIDE                                                   *
C 10) LIQUID LITHIUM                                                  *
C 11) VANADIUM ALLOY                                                  *
C 12) SUPERCONDUCTOR Nb3Sn (set equal to copper for now)              *
C 13) SUPERCONDUCTOR NbTi  (set equal to copper for now)              *
C**********************************************************************

      INTEGER NMAT
      PARAMETER (NMAT=13)

      DOUBLE PRECISION A(NMAT),B(NMAT),C(NMAT),D(NMAT),E(NMAT),RHO(NMAT)
      COMMON/CONDCT/A,B,C,D,E,RHO

      DATA A(1),B(1),C(1),D(1),E(1),RHO(1)/
     +     2.2867D+01,
     +     1.4546D-02,
     +     -2.3056D-05,
     +     1.4815D-08,
     +     0.000D-00,
     +     8.000D+03/

      DATA A(2),B(2),C(2),D(2),E(2),RHO(2)/
     +     9.0109D+00,
     +     1.5298D-02,
     +     0.0000D-00,
     +     0.0000D-00,
     +     0.0000D-00,
     +     8.000D+03/

      DATA A(3),B(3),C(3),D(3),E(3),RHO(3)/
     +     7.3008D+00,
     +     1.9600D-02,
     +     0.0000D-06,
     +     0.0000D+00,
     +     0.0000D+00,
     +     9.300D+03/

      DATA A(4),B(4),C(4),D(4),E(4),RHO(4)/
     +     4.3035D+02,
     +     -1.1674D-00,
     +     1.6044D-03,
     +     -1.0097D-06,
     +     2.3642D-10,
     +     1.8D+03/

      DATA A(5),B(5),C(5),D(5),E(5),RHO(5)/
     +     1.0000D-06,
     +     0.0000D+00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     1.300D-00/

      DATA A(6),B(6),C(6),D(6),E(6),RHO(6)/
     +     1.0000D+16,
     +     0.0000D+00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     1.000D+00/

      DATA A(7),B(7),C(7),D(7),E(7),RHO(7)/
     +     4.2075D+02,
     +     -6.8493D-02,
     +     0.0000D-00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     8.90D+03/

      DATA A(8),B(8),C(8),D(8),E(8),RHO(8)/
     +     4.7100D+01,
     +     -4.9000D-02,
     +     2.0600D-05,
     +     -2.2500D-09,
     +     0.000D+00,
     +     1.24D+04/

      DATA A(9),B(9),C(9),D(9),E(9),RHO(9)/
     +     3.1686D+01,
     +     -8.6498D-02,
     +     1.1881D-04,
     +     -7.9654D-08,
     +     2.0734D-11,
     +     2.013D+03/

      DATA A(10),B(10),C(10),D(10),E(10),RHO(10)/
     +     3.8966D+01,
     +     1.2250D-02,
     +     0.0000D+00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     5.34D+02/

      DATA A(11),B(11),C(11),D(11),E(11),RHO(11)/
     +     1.6734D+01,
     +     1.4678D-02,
     +     0.0000D+00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     6.10D+03/

      DATA A(12),B(12),C(12),D(12),E(12),RHO(12)/
     +     4.2075D+02,
     +     -6.8493D-02,
     +     0.0000D-00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     8.90D+03/

      DATA A(13),B(13),C(13),D(13),E(13),RHO(13)/
     +     4.2075D+02,
     +     -6.8493D-02,
     +     0.0000D-00,
     +     0.0000D+00,
     +     0.0000D+00,
     +     8.90D+03/

      END
C-----------------------------------------------------------------------

      SUBROUTINE BLCYL3(FWTEMP)

C***********************************************************************
C FUSION POWER PLANT HEAT TRANSFER MODULE                              *
C CYLINDRICAL GEOMETRY VERSION                                         *
C MODIFIED VERSION OF BLCYL3                                           *
C                                                                      *
C C.B.A. FORTY & W.E. HAN                                              *
C 17 JANUARY 1997                                                      *
C                                                                      *
C Modified by P. Knight to allow input of QVOL rather than DECPW       *
C 6 February 1997                                                      *
C                                                                      *
C***********************************************************************

C *** M = DIMENSIONING OF RADIAL NODES ***
C *** N = NUMBER OF MATERIAL TYPES ***

      INTEGER M,N
      PARAMETER (M=15)
      PARAMETER (N=13)

C  A1(M)        : part of the b-const equation
C  A2(M)        : part of the b-const equation  
C  A3(M)        : part of the b-const equation
C  ACOEFF(N)    : a coeff for thermal conductivity
C  ACONST(M)    : a-constant of integration
C  ALPHA        : heat transfer coeff
C  AVECON(M)    : average thermal conductivity
C  AVERHO(M)    : average density
C  B1(M)        : part of the b-const equation    
C  B2(M)        : part of the b-const equation
C  BCOEFF(N)    : b coeff for thermal conductivity
C  BCONST(M)    : b-constant of integration
C  C1(M)        : part of the b-const equation
C  C2(M)        : part of the b-const equation
C  CCOEFF(N)    : c coeff for thermal conductivity
C  DCOEFF(N)    : d coeff for thermal conductivity
C  DECPOW(M)    : weighted decay power used in subroutine
C  DT(M)        : dT/dr used in b-const calculation
C  ECOEFF(N)    : e coeff for thermal conduction
C  EPRIME(M)    : epsilon prime used in radiation model
C  EPSILN       : epsilon used in radiation model
C  FRCRHO(M,N)  : intermediary fraction-rho variable
C  FWTEMP       : outboard FW temperature
C  I            : no of materials
C  ITHCON(M,N)  : intermediary thermal conductivity var.
C  J            : no of convergence iterations
C  K            : no of layers
C  MATFRC(M,N)  : material fraction 
C  NN           : last of layer
C  NPATH        : switch for convection/conv+radiation calculation
C  PI           : pi
C  QFLOW(M)     : heat flowing across layer surfaces
C  QOUTER       : heat flow at outer boundary surface
C  QVOL(M)      : total decay heat in each region
C  RADGAP(M)    : radiation gap for opposite surfaces
C  RADMID(M)    : mean radial coordinate
C  RADMIN(M)    : minus radial coordinate
C  RADPLS(M)    : plus radial coordinate
C  RHO(N)       : material density
C  SAREA(M)     : surface area at radpls coordinates 
C  STEFAN       : stefan-boltzmann constant
C  SUMQVL(0:M)  : summing variable for qvol
C  T(M)         : dummy temp (i think)
C  TAIR         : ambient air temperature
C  TEMMID(M)    : mid-layer temperature
C  TEMMIN(M)    : temperature at minus coordinate   
C  TEMPLS(M)    : temperature at plus coordinate
C  THCON(M,N)   : intermediary thermal cond. var.
C  THCOND(M)    : thermal conductivity
C  TNEW         : new temperature
C  VFACT(M)     : radiation view factor

      DOUBLE PRECISION A1(M)
      DOUBLE PRECISION A2(M)
      DOUBLE PRECISION A3(M)
      DOUBLE PRECISION ACOEFF(N)
      DOUBLE PRECISION ACONST(M)
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION AVECON(M)
      DOUBLE PRECISION AVERHO(M)
      DOUBLE PRECISION B1(M)
      DOUBLE PRECISION B2(M)
      DOUBLE PRECISION BCOEFF(N)
      DOUBLE PRECISION BCONST(M)
      DOUBLE PRECISION C1(M)
      DOUBLE PRECISION C2(M)
      DOUBLE PRECISION CCOEFF(N)
      DOUBLE PRECISION DCOEFF(N)
      DOUBLE PRECISION DECPOW(M)
      DOUBLE PRECISION DT(M)
      DOUBLE PRECISION ECOEFF(N)
      DOUBLE PRECISION EPRIME(M)
      DOUBLE PRECISION EPSILN
      DOUBLE PRECISION FRCRHO(M,N)
      DOUBLE PRECISION FWTEMP
      DOUBLE PRECISION ITHCON(M,N)
      DOUBLE PRECISION MATFRC(M,N)
      DOUBLE PRECISION PI
      DOUBLE PRECISION QFLOW(M)
      DOUBLE PRECISION QOUTER
      DOUBLE PRECISION QVOL(M)
      DOUBLE PRECISION RADGAP(M)
      DOUBLE PRECISION RADMID(M)
      DOUBLE PRECISION RADMIN(M)
      DOUBLE PRECISION RADPLS(M)
      DOUBLE PRECISION RHO(N)
      DOUBLE PRECISION SAREA(M)
      DOUBLE PRECISION STEFAN
      DOUBLE PRECISION SUMQVL(0:M)
      DOUBLE PRECISION T(M)
      DOUBLE PRECISION TAIR
      DOUBLE PRECISION TEMMID(M)
      DOUBLE PRECISION TEMMIN(M)
      DOUBLE PRECISION TEMPLS(M)
      DOUBLE PRECISION THCON(M,N)
      DOUBLE PRECISION THCOND(M)
      DOUBLE PRECISION TNEW
      DOUBLE PRECISION VFACT(M)
      INTEGER I,J,K,NN,NPATH

      COMMON/CONDCT/ACOEFF,BCOEFF,CCOEFF,DCOEFF,ECOEFF,RHO
      COMMON/GEOM/RADMIN,RADPLS,RADMID,MATFRC,QVOL
      COMMON/EDGET/TAIR,ALPHA,NPATH

      EXTERNAL ROOT

      PI = ACOS(-1.0D0)
      STEFAN = 5.67D-8

C *** EPSILN assumed fixed

      EPSILN = 0.80D0

C *** Number of radial nodes used

      NN = 15

C *** INITIAL MID-POINT TEMPERATURES ***

      DO 11 K = 1,NN
         TEMMID(K) = 600.0D0
 11   CONTINUE

C *** CALCULATES THE VIEW-FACTOR BETWEEN OPPOSITE FACES ***

      VFACT(1) = 1.0D0
      DO 46 K = 2,NN
         VFACT(K) = RADPLS(K-1)/RADMIN(K)
 46   CONTINUE

C *** CALCULATES THE GEOMETRY VARIATION IN EMISSIVITY ***

      DO 248 K = 1, NN
         EPRIME(K) = (1.0D0/EPSILN+VFACT(K)*
     +        (1.0D0/EPSILN-1.0D0))**(-1.0D0)
 248  CONTINUE

C *** TOP OF ITERATION LOOP ***

      DO 77 J = 1,100

C *** INITIALISE ARRAYS ***

         DO 19 K = 1,NN
            AVERHO(K) = 0.0D0
            AVECON(K) = 0.0D0
            SUMQVL(K) = 0.0D0
 19      CONTINUE

C *** RADIALLY DISTRIBUTED AVERAGE DENSITIES ***

         DO 21 K = 1,NN
            DO 22 I = 1,N
               FRCRHO(K,I) = MATFRC(K,I)*RHO(I)
               AVERHO(K) = AVERHO(K)+FRCRHO(K,I)
 22         CONTINUE
 21      CONTINUE

C *** CALCULATE THE RADIALLY DISTRIBUTED AVERAGE DECAY POWERS ***

         SUMQVL(0) = 0.0D0
         DO 161 K = 1,NN
            DECPOW(K) = QVOL(K) /
     +           (PI*(RADPLS(K)**2-RADMIN(K)**2)*AVERHO(K))
            SUMQVL(K) = SUMQVL(K-1)+QVOL(K)
 161     CONTINUE

C *** CALCULATES THE RADIAL (RADPLS) SURFACE AREAS ***

         DO 156 K = 1,NN
            SAREA(K) = 2.0D0*PI*RADPLS(K)
 156     CONTINUE

C *** CALCULATES THE HEAT FLOWS ACROSS THE RADPLS INTERFACES ***

         DO 774 K = 1,NN
            QFLOW(K) = SUMQVL(K)/SAREA(K)
 774     CONTINUE

C *** INITIAL THERMAL CONDUCTIVITIES ***
C *** DOES A PARALLEL ADDITION OF THERMAL CONDUCTIVITIES ***

         DO 47 K = 1,NN
            DO 45 I = 1,N
               THCON(K,I) = ACOEFF(I)+BCOEFF(I)*TEMMID(K)+CCOEFF(I)
     +              *TEMMID(K)**2+DCOEFF(I)*TEMMID(K)**3+ECOEFF(I)
     +              *TEMMID(K)**4
               ITHCON(K,I) = MATFRC(K,I)*THCON(K,I)
               AVECON(K) = AVECON(K)+ITHCON(K,I)
               THCOND(K) = 1.0D0*AVECON(K)
 45         CONTINUE
 47      CONTINUE

C *** CALCULATING THE A CONSTANT FROM INSIDE-TO-OUTSIDE ***

         ACONST(1) = 0.0D0
         DO 122 K = 1,NN-1
            ACONST(K+1) = RADMIN(K+1)/THCOND(K+1)*
     +           ((DECPOW(K+1)*AVERHO(K+1)*RADMIN(K+1)/2.0D0)+
     +           (VFACT(K)*ACONST(K)*THCOND(K)/RADPLS(K))-
     +           (VFACT(K)*DECPOW(K)*AVERHO(K)*RADPLS(K)/2.0D0))
 122     CONTINUE

C *** BOUNDARY TEMPERATURE CALCULATION ***
C *** FIRST CALCULATE THE Q PASSING THROUGH THE RADIAL AREA ***

         K = NN
         QOUTER = SUMQVL(NN)/SAREA(NN)

C *** THEN DO THE REQUIRED CALCULATION ***

         IF (NPATH.EQ.1) THEN
C           *** USE THE ROOT FINDER SUBROUTINE ***

            K = NN
            CALL ROOT(QOUTER,TNEW,ALPHA,TAIR)
            TEMPLS(K) = TNEW
         ELSE
C           *** OR DO THE SIMPLE CONVECTION SOLUTION ***
            K = NN
            TEMPLS(K) = QOUTER/2.0D0/ALPHA+TAIR
         END IF

C *** CALCULATE THE OUTSIDE B CONSTANT ***

         BCONST(K) = TEMPLS(K)+
     +        (DECPOW(K)*AVERHO(K)*RADPLS(K)**2/4.0D0/THCOND(K))-
     +        (ACONST(K)*LOG(RADPLS(K)))
         DO 42 K = 2,NN
            RADGAP(K) = RADMIN(K)-RADPLS(K-1)
 42      CONTINUE

C *** CALCULATE THE REMAINING B CONSTANTS OUTSIDE-TO-INSIDE ***

         DO 33 K = NN,2,-1

            IF (RADGAP(K).GT.0.0D0) THEN

C              *** USE THE RADIATION GAP MODEL TO CALCULATE BCONST ***
C              *** SPLIT THE EQUATION INTO MANAGABLE BITS ***

               A1(K) = -DECPOW(K)*AVERHO(K)*RADMIN(K)**2/4.0D0/THCOND(K)
               A2(K) = ACONST(K)*LOG(RADMIN(K))
               A3(K) = BCONST(K)
               B1(K-1) = DECPOW(K-1)*AVERHO(K-1)*RADPLS(K-1)/2.0D0/
     +              EPRIME(K-1)/STEFAN
               B2(K-1) = -ACONST(K-1)*THCOND(K-1)/RADPLS(K-1)/
     +              EPRIME(K-1)/STEFAN
               C1(K-1) = DECPOW(K-1)*AVERHO(K-1)*RADPLS(K-1)**2/4.0D0/
     +              THCOND(K-1)
               C2(K-1) = -ACONST(K-1)*LOG(RADPLS(K-1))
               DT(K-1) = B1(K-1)+B2(K-1)
               T(K) = A1(K)+A2(K)+A3(K)
               T(K-1) = C1(K-1)+C2(K-1)
               BCONST(K-1) = (T(K)**4+DT(K-1))**0.25D0+T(K-1)

            ELSE

C              *** USE THE CONDUCTION MODEL TO CALCULATE BCONST ***

               BCONST(K-1) =
     +              (DECPOW(K-1)*AVERHO(K-1)*RADPLS(K-1)**2/4.0D0/
     +              THCOND(K-1))-(DECPOW(K)*AVERHO(K)*RADMIN(K)**2/
     +              4.0D0/THCOND(K))+(ACONST(K)*LOG(RADMIN(K))-
     +              ACONST(K-1)*LOG(RADPLS(K-1)))+BCONST(K)

            END IF

 33      CONTINUE

C *** CALCULATE NEW RADIAL TEMPERATURES ***

         DO 544 K = 1,NN
            TEMMIN(K) = (-DECPOW(K)*AVERHO(K)*RADMIN(K)**2/
     +           4.0D0/THCOND(K))+(ACONST(K)*LOG(RADMIN(K)))+(BCONST(K))
            TEMMID(K) = (-DECPOW(K)*AVERHO(K)*RADMID(K)**2/
     +           4.0D0/THCOND(K))+(ACONST(K)*LOG(RADMID(K)))+(BCONST(K))
            TEMPLS(K)=(-DECPOW(K)*AVERHO(K)*RADPLS(K)**2/
     +           4.0D0/THCOND(K))+(ACONST(K)*LOG(RADPLS(K)))+(BCONST(K))

 544     CONTINUE

 77   CONTINUE

C+**PJK Outboard first wall is now region 10 instead of 9
C+**PJK FWTEMP = TEMMIN(9)

      FWTEMP = TEMMIN(10)

      RETURN
      END

C----------------------------------------------------------------------

C**********************************************************************
C SUBROUTINES AND FUNCTIONS BORROWED FROM THE TEXT BOOK OF DeVries.   *
C                                                                     *
C A FIRST COURSE IN COMPUTATIONAL PHYSICS - PAUL L DEVRIES            *
C (JOHN WILEY & SONS, NEW YORK 1994).                                 *
C                                                                     *
C  SEE CHAPTER 2, PAGE  54.                                           *
C**********************************************************************

      SUBROUTINE ROOT(Q,X,A,T)

      DOUBLE PRECISION X, FOFX, DEROFF, Q, A, T
      EXTERNAL FOFX, DEROFF, NEWTON

      X = 0.8D0
      CALL NEWTON( X, FOFX, DEROFF, Q , A, T)

      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FOFX(X, Q, A, T)
C------------------------------------------------------------------*
C                                                                  *
C  This is an example of a nonlinear function whose                *
C  root cannot be found analytically.                              *
C                                                                  *
C------------------------------------------------------------------*
      DOUBLE PRECISION X,Q,A,T,STEFAN,EMISIV

      STEFAN = 5.67D-8
      EMISIV = 0.667D0

      FOFX = A*(X-T)+STEFAN*EMISIV*(X**4-T**4)-Q

      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION DEROFF(X,A,T)
C------------------------------------------------------------------*
C                                                                  *
C  This function is the derivative of "F of X."                    *
C                                                                  *
C------------------------------------------------------------------*

      DOUBLE PRECISION X,A,T

      DEROFF  = A + 4.0D0*3.5D-8*X**3

      END
C----------------------------------------------------------------------
      SUBROUTINE NEWTON ( X, F, FPRIME, Q , A, T)
C
C  Type declarations
C
      DOUBLE PRECISION X, F, FPRIME, DELTA, ERROR, TOL,Q,A,T
      EXTERNAL F, FPRIME
C
C  Parameter declarations
C
      PARAMETER(TOL = 5.0D-06)
C
C  Top of the loop
C

 100  CONTINUE

      DELTA = -F(X,Q,A,T)/FPRIME(X,A,T)
      X = X + DELTA

C
C  Check for the relative error condition: If too big,
C  loop again; if small enough,  end.
C
      ERROR = ABS(  DELTA / X )
      IF ( ERROR .GT. TOL) GOTO 100

      END
C----------------------------------------------------------------------
