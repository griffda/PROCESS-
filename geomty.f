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
C  Module name    : $RCSfile: geomty.f,v $
C  Version no.    : $Revision: 3.7 $
C
C  Creation date  : $Date: 2001/07/16 14:59:07 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      SUBROUTINE GEOMTY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 3.000
C
C--Description
C  Routine to find plasma geometry parameters
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  18 January 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK14, pp.41-43
C  
C--History
C  18/01/99 PJK 3.000 New version incorporating upgraded coding and
C                     improved algorithms for double-null plasmas
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
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'rfp.h'

C  Local variables
      DOUBLE PRECISION SA,SF2,SO,XSI,XSO
      DOUBLE PRECISION THETAI,THETAO,XI,XO

C  External routines
      EXTERNAL SURFA,XPARAM,XSURF

C  External functions
      DOUBLE PRECISION FVOL,PERIM,XSECT0,XSECTA,XVOL
      EXTERNAL         FVOL,PERIM,XSECT0,XSECTA,XVOL

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialize some shape quantities

      RMINOR = RMAJOR / ASPECT
      EPS = 1.0D0 / ASPECT

C *** Calculate shaping terms, rather than use input values (TART)

      IF (ISHAPE.EQ.1) THEN
         KAPPA = 2.05D0 * (1.0D0 + 0.44D0 * EPS**2.1D0)
         TRIANG = 0.53D0 * (1.0D0 + 0.77D0 * EPS**3)
         QLIM = 3.0D0 * (1.0D0 + 2.6D0*EPS**2.8D0)
      END IF

C *** Rough estimate of 95% values

      KAPPA95 = (KAPPA - 0.04D0) / 1.10D0
      TRIANG95 = TRIANG / 1.50D0

C *** Scrape-off layer thicknesses

      IF (ISCRP.EQ.0) THEN
        SCRAPLO = 0.1D0 * RMINOR
        SCRAPLI = 0.1D0 * RMINOR
      END IF

C *** RFP calculations (circular plasma)

      IF (IRFP.EQ.1) THEN

C *** Plasma poloidal perimeter

         SF2 = 2.0D0*PI*RMINOR
         SF = 1.0D0

C *** Plasma volume (kappa should be 1.0)

         VOL = CVOL * 2.0D0 * PI**2 * RMAJOR * RMINOR**2 * KAPPA

C *** Plasma surface area (very bad approx for outer area...)

         SAREA = 4.0D0 * PI**2 * RMAJOR * RMINOR * 0.5D0*(KAPPA+1.0D0)
         SAREAO = 0.5D0 * SAREA

C *** Plasma cross-sectional area

         XAREA = PI * RMINOR * RMINOR

         GOTO 1000

      END IF

C *** Double null configuration

      IF (IGEOM.EQ.0) THEN

C *** Use original methods

         SF2 = PERIM(RMINOR,KAPPA,TRIANG)
         SF = SF2 / (2.0D0*PI*RMINOR)

         VOL = CVOL * FVOL(RMAJOR,RMINOR,KAPPA,PI,TRIANG)

         CALL SURFA(RMINOR,RMAJOR,KAPPA,TRIANG,SA,SO)
         SAREAO = SO
         SAREA = SA

         XAREA = XSECT0(RMINOR,KAPPA,TRIANG)

      ELSE

C *** Find parameters of arcs describing plasma surfaces

         CALL XPARAM(RMINOR,KAPPA,TRIANG,XI,THETAI,XO,THETAO)

C *** Poloidal perimeter

         SF2 = 2.0D0 * ( XO*THETAO + XI*THETAI )
         SF = SF2 / (2.0D0*PI*RMINOR)

C *** Volume

         VOL = CVOL * XVOL(PI,RMAJOR,RMINOR,XI,THETAI,XO,THETAO)

C *** Surface area

         CALL XSURF(PI,RMAJOR,RMINOR,XI,THETAI,XO,THETAO,XSI,XSO)
         SAREAO = XSO
         SAREA = XSI+XSO

C *** Cross-sectional area

         XAREA = XSECTA(XI,THETAI,XO,THETAO)

      END IF

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SURFA(A,R,K,D,SA,SO)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Function to find the plasma surface area, using the revolution
C  of 2 intersecting arcs around the device centreline.
C  Appropriate for plasmas with a separatrix.
C  Original method in PROCESS.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  19 January 1999 
C
C--Reference
C  Peng?
C  
C--History
C  19/01/99 PJK 1.100 Initial upgraded version
C
C--Arguments
C  A      : (INPUT)  plasma minor radius (m)
C  R      : (INPUT)  plasma major radius (m)
C  K      : (INPUT)  plasma separatrix elongation
C  D      : (INPUT)  plasma separatrix triangularity
C  SA     : (OUTPUT) plasma total surface area (m2)
C  SO     : (OUTPUT) plasma outer surface area (m2)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION A,R,K,D,SA,SO

C  Local variables
      DOUBLE PRECISION B,PI,RADCI,RADCO,SI,THTI,THTO

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PI = 3.1415927D0

C *** Outer side :

      RADCO = A * (1.0D0 + (K**2 + D**2 - 1.0D0)/(2.0D0 * (1.0D0 + D)))
      B = K * A
      THTO = ASIN(B/RADCO)
      SO = 4.0D0 * PI * RADCO * ( (R + A - RADCO)*THTO + B)

C *** Inner side :

      RADCI = A * (1.0D0 + (K**2 + D**2 - 1.0D0)/(2.0D0 * (1.0D0 - D)))
      THTI = ASIN(B/RADCI)
      SI = 4.0D0 * PI * RADCI * ( (R - A + RADCI)*THTI - B)

      SA = SO + SI

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION PERIM(A,KAP,TRI)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.200
C
C--Description
C  Routine to find plasma poloidal perimeter by 2 intersecting arcs
C  method. Good for divertors.
C  Original method in PROCESS.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  16 July 2001
C
C--Reference
C  Peng?
C  F/PL/PJK/PROCESS/CODE/047
C  
C--History
C  19/01/99 PJK 1.100 Initial upgraded version
C  16/07/01 PJK 1.100 Correction to sign of TRI in DENOMI and XLI
C
C--Arguments
C  A      : (INPUT)  plasma minor radius (m)
C  KAP    : (INPUT)  plasma separatrix elongation
C  TRI    : (INPUT)  plasma separatrix triangularity
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION A,KAP,TRI

C  Local variables
      DOUBLE PRECISION DENOMI,DENOMO,THETAI,THETAO,XLI,XLO

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Inner arc :

      DENOMI = (TRI**2 + KAP**2 - 1.0D0)/( 2.0D0*(1.0D0-TRI) ) + TRI
      THETAI = ATAN(KAP/DENOMI)
      XLI = A * (DENOMI + 1.0D0 - TRI )

C *** Outer arc:

      DENOMO = (TRI**2 + KAP**2 - 1.0D0)/( 2.0D0*(1.0D0+TRI) ) - TRI
      THETAO = ATAN(KAP/DENOMO)
      XLO = A * (DENOMO + 1.0D0 + TRI )

      PERIM = 2.0D0 * ( XLO*THETAO + XLI*THETAI)

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION XSECT0(A,KAP,TRI)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to find plasma cross-sectional area by 2 intersecting arcs
C  method. Good for divertors.
C
C  The method for finding the arc radii and angles are copied from
C  routine PERIM, and are thought to be by Peng.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  16 July 2001
C
C--Reference
C  F/MI/PJK/LOGBOOK14, p.41
C  F/PL/PJK/PROCESS/CODE/047
C  
C--History
C  30/06/98 PJK 1.000 Initial version (original version of xsecta)
C  16/07/01 PJK 1.100 Correction to sign of TRI in DENOMI and XLI
C
C--Arguments
C  A      : (INPUT)  plasma minor radius (m)
C  KAP    : (INPUT)  plasma separatrix elongation
C  TRI    : (INPUT)  plasma separatrix triangularity
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION A,KAP,TRI

C  Local variables
      DOUBLE PRECISION DENOMI,DENOMO,THETAI,THETAO,XLI,XLO
      DOUBLE PRECISION CTI,STI,CTO,STO

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Find radius and half-angle of inner arc

      DENOMI = (TRI**2 + KAP**2 - 1.0D0)/( 2.0D0*(1.0D0-TRI) ) + TRI
      THETAI = ATAN(KAP/DENOMI)
      XLI = A * (DENOMI + 1.0D0 - TRI)

      CTI = COS(THETAI)
      STI = SIN(THETAI)

C *** Find radius and half-angle of outer arc

      DENOMO = (TRI**2 + KAP**2 - 1.0D0)/( 2.0D0*(1.0D0+TRI) ) - TRI
      THETAO = ATAN(KAP/DENOMO)
      XLO = A * (DENOMO + 1.0D0 + TRI )

      CTO = COS(THETAO)
      STO = SIN(THETAO)

C *** Find cross-sectional area

      XSECT0 = XLO**2 * (THETAO - CTO*STO) +
     +         XLI**2 * (THETAI - CTI*STI)

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FVOL(R,A,KAP,PI,TRI)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Find plasma volume by integrating between two swept arcs
C  (ITER method). Good for diverted plasmas.
C  Original method in PROCESS.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  19 January 1999
C
C--Reference
C  Peng?
C  
C--History
C  18/01/99 PJK 1.000 Initial upgraded version
C
C--Arguments
C  R      : (INPUT)  plasma major radius (m)
C  A      : (INPUT)  plasma minor radius (m)
C  KAP    : (INPUT)  plasma separatrix elongation
C  PI     : (INPUT)  3.14...
C  TRI    : (INPUT)  plasma separatrix triangularity
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION R,A,KAP,PI,TRI

C  Local variables
      DOUBLE PRECISION C1,C2,RC2,RCI,VIN,VOUT,ZN

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ZN = KAP * A

      RCI = ( (R + A)**2 - (R - TRI*A)**2 - ZN**2) /
     +     (2.0D0 * (1.0D0+TRI) * A)
      C1 = R + A - RCI
      VOUT = -0.66666666D0 * PI * ZN**3 + 2.0D0 * PI * ZN *
     +     (RCI**2 + C1**2) + 2.0D0 * PI * RCI *
     +     ( ZN * SQRT(C1**2 - ZN**2) + C1**2 * ASIN(ZN/C1) )

      RC2 = (-(R - A)**2 + (R - TRI*A)**2 + ZN**2) /
     +     (2.0D0 * (1.0D0-TRI) * A)
      C2 = RC2 - R + A
      VIN = -0.66666D0 * PI * ZN**3 + 2.0D0 * PI * ZN *
     +     (C2**2 + RC2**2) - 2.0D0 * PI * RC2 *
     +     ( ZN * SQRT(C2**2 - ZN**2) + C2**2 * ASIN(ZN/C2) )

      FVOL = VOUT - VIN

      RETURN
      END  
C----------------------------------------------------------------------
      SUBROUTINE XPARAM(A,KAP,TRI,XI,THETAI,XO,THETAO)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to find parameters used for calculating geometrical
C  properties for double-null plasmas.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  16 July 2001
C
C--Reference
C  F/MI/PJK/LOGBOOK14, p.42
C  F/PL/PJK/PROCESS/CODE/047
C  
C--History
C  18/01/99 PJK 1.000 Initial version
C  16/07/01 PJK 1.100 Correction of sign of TRI in XI
C
C--Arguments
C  A      : (INPUT)  plasma minor radius (m)
C  KAP    : (INPUT)  plasma separatrix elongation
C  TRI    : (INPUT)  plasma separatrix triangularity
C  XI     : (OUTPUT) radius of arc describing inboard surface (m)
C  THETAI : (OUTPUT) half-angle of arc describing inboard surface
C  XO     : (OUTPUT) radius of arc describing outboard surface (m)
C  THETAO : (OUTPUT) half-angle of arc describing outboard surface
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION A,KAP,TRI,XI,THETAI,XO,THETAO

C  Local variables
      DOUBLE PRECISION DENOMI,DENOMO,N,T

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Find radius and half-angle of inner arc

      T = 1.0D0 - TRI
      DENOMI = (KAP**2 - T**2)/(2.0D0*T)
      THETAI = ATAN(KAP/DENOMI)
      XI = A * (DENOMI + 1.0D0 - TRI )

C *** Find radius and half-angle of outer arc

      N = 1.0D0 + TRI
      DENOMO = (KAP**2 - N**2)/(2.0D0*N)
      THETAO = ATAN(KAP/DENOMO)
      XO = A * (DENOMO + 1.0D0 + TRI )

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION XSECTA(XI,THETAI,XO,THETAO)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Routine to find plasma cross-sectional area by 2 intersecting arcs
C  method. Good for divertors.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  18 January 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK14, p.41
C  
C--History
C  30/06/98 PJK 1.000 Initial version
C  18/01/99 PJK 1.010 Moved calculation of arc parameters into XPARAM
C
C--Arguments
C  XI     : (INPUT)  radius of arc describing inboard surface (m)
C  THETAI : (INPUT)  half-angle of arc describing inboard surface
C  XO     : (INPUT)  radius of arc describing outboard surface (m)
C  THETAO : (INPUT)  half-angle of arc describing outboard surface
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION XI,THETAI,XO,THETAO

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Find cross-sectional area

      XSECTA = XO**2 * (THETAO - COS(THETAO)*SIN(THETAO)) +
     +         XI**2 * (THETAI - COS(THETAI)*SIN(THETAI))

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION
     +     XVOL(PI,RMAJOR,RMINOR,XI,THETAI,XO,THETAO)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to find plasma volume (m3) by 2 intersecting arcs method.
C  Good for divertors.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  18 January 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK14, p.43
C  
C--History
C  18/01/99 PJK 1.000 Initial version
C
C--Arguments
C  PI     : (INPUT)  3.14...
C  RMAJOR : (INPUT)  plasma major radius (m)
C  RMINOR : (INPUT)  plasma minor radius (m)
C  XI     : (INPUT)  radius of arc describing inboard surface (m)
C  THETAI : (INPUT)  half-angle of arc describing inboard surface
C  XO     : (INPUT)  radius of arc describing outboard surface (m)
C  THETAO : (INPUT)  half-angle of arc describing outboard surface
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION PI,RMAJOR,RMINOR,XI,THETAI,XO,THETAO

C  Local variables
      DOUBLE PRECISION RC,THIRD,TWOPI,VIN,VOUT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      THIRD = 1.0D0/3.0D0
      TWOPI = 2.0D0*PI

      RC = RMAJOR - RMINOR + XI
      VIN = TWOPI * XI *
     +     ( RC**2*SIN(THETAI) -
     +     RC*XI*THETAI -
     +     0.5D0*RC*XI*SIN(2.0D0*THETAI) +
     +     XI*XI*SIN(THETAI) -
     +     THIRD*XI*XI*(SIN(THETAI))**3 )

      RC = RMAJOR + RMINOR - XO
      VOUT = TWOPI * XO *
     +     ( RC**2*SIN(THETAO) +
     +     RC*XO*THETAO +
     +     0.5D0*RC*XO*SIN(2.0D0*THETAO) +
     +     XO*XO*SIN(THETAO) -
     +     THIRD*XO*XO*(SIN(THETAO))**3 )

      XVOL = VOUT - VIN

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE XSURF(PI,RMAJOR,RMINOR,XI,THETAI,XO,THETAO,XSI,XSO)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to find plasma surface area by 2 intersecting arcs method.
C  Good for divertors.
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  18 January 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK14, p.43
C  
C--History
C  18/01/99 PJK 1.000 Initial version
C
C--Arguments
C  PI     : (INPUT)  3.14...
C  RMAJOR : (INPUT)  plasma major radius (m)
C  RMINOR : (INPUT)  plasma minor radius (m)
C  XI     : (INPUT)  radius of arc describing inboard surface (m)
C  THETAI : (INPUT)  half-angle of arc describing inboard surface
C  XO     : (INPUT)  radius of arc describing outboard surface (m)
C  THETAO : (INPUT)  half-angle of arc describing outboard surface
C  XSI    : (OUTPUT) inboard surface area (m2)
C  XSO    : (OUTPUT) outboard surface area (m2)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION PI,RMAJOR,RMINOR,XI,THETAI,XO,THETAO,XSI,XSO

C  Local variables
      DOUBLE PRECISION FOURPI,RC

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      FOURPI = 4.0D0 * PI

      RC = RMAJOR - RMINOR + XI
      XSI = FOURPI * XI * (RC*THETAI - XI*SIN(THETAI))

      RC = RMAJOR + RMINOR - XO
      XSO = FOURPI * XO * (RC*THETAO + XO*SIN(THETAO))

      RETURN
      END
