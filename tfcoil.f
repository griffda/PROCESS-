C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: tfcoil.f,v 3.10 1997/11/19 09:32:32 peter Exp $
C  Module name    : $RCSfile: tfcoil.f,v $
C  Version no.    : $Revision: 3.10 $
C  Creation date  : $Date: 1997/11/19 09:32:32 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C----------------------------------------------------------------------
      SUBROUTINE TFCOIL(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate various parameters for the TF coil set.
C  If the TF coils are superconducting the calculations are performed
C  in routine SCTFCOIL.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  22 October 1996
C
C--Reference
C  F/MI/PJK/PROCESS/DOCUMENTATION/MANUAL : PROCESS User's Guide
C  F/MI/PJK/PROCESS/CODE : PROCESS Project Work File
C  
C--History
C  22/10/96 PJK 1.000 Initial upgraded version
C
C--Arguments
C  NOUT   : (INPUT)  Output unit identifier
C  IPRINT : (INPUT)  Switch for printing to output file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION R1,ROUTR,RINR,TFCIND1

C  External routines
      EXTERNAL CONCOPTF,OHEADR,OSUBHD,OVARRE,PORTSZ,SCTFCOIL

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (ITFSUP.EQ.0) THEN

C *** Resistive TF coils
C *** ==================

C ***    Radius of outer edge of inboard TF coil leg

         IF (ITART.EQ.1) THEN
            RBMAX = BCYLTH + TFCTH
         ELSE
            RBMAX = RSLDI - GAPDS - DDWI
         END IF

C ***    Radius of inner edge of inboard TF coil leg

         R1 = MAX(0.0D0, (RBMAX - TFCTH) )

C ***    Radius of centre of inboard TF coil leg

         RTFCIN = RBMAX - TFCTH/2.0D0

C ***    Inboard total cross-sectional area

         TFAREAIN = PI * (RBMAX**2 - R1**2)

C ***    Total current flowing through inboard TF coil legs

         RITFC = TFAREAIN * OACDCP

C ***    Peak field

         BMAXTF = 2.0D-7 * RITFC / RBMAX

C ***    Radius of inner edge of outboard TF coil leg

         ROUTR = RTOT - TFCTH/2.0D0

C ***    Radius of outer edge of inboard TF coil leg

         RINR = RBMAX

C ***    Centering and vertical forces

         IF (BORE .EQ. 0.0D0) THEN
            CFORCE = 0.0D0
         ELSE
            CFORCE = BMAXTF * RITFC/(2.0D0*TFNO)
         END IF
         VFORCE = 0.55D0 * BT * RMAJOR * RITFC/2.0D0 *
     +        LOG(ROUTR/RINR)/TFNO

C ***    Bore (gap between inner and outer TF coil legs)

         TFBOREH = RTOT - RBMAX - TFCTH/2.0D0

C ***    Other calculations for normal-conducting TF coils

         CALL CONCOPTF(0,NOUT)

C ***    Inductance

         TFCIND1 = HMAX * RMU0/PI * LOG( ROUTR / RINR )

C ***    Stored energy per coil (GJ)

         ESTOTF = 0.5D-9 * TFCIND1 * RITFC**2/TFNO

      ELSE

C ***    Superconducting TF coils
C ***    ========================

         CALL SCTFCOIL(NOUT,IPRINT)

      END IF

C *** Port size

      CALL PORTSZ

      IF ((IPRINT.EQ.0).OR.(SECT07.EQ.0)) GOTO 1000

C *** Output section

      IF (ITFSUP.EQ.0) THEN

         CALL OHEADR(NOUT,'TF Coils')
         CALL OVARRE(NOUT,'TF coil current (A)','(ritfc)',RITFC)
         CALL OVARRE(NOUT,'Peak field at the TF coils (T)','(bmaxtf)',
     +        BMAXTF)
         CALL OVARRE(NOUT,'Ripple at plasma edge (%)','(ripple)',
     +        RIPPLE)
         CALL OVARRE(NOUT,'Allowable ripple (%)','(ripmax)',RIPMAX)
         CALL OVARRE(NOUT,'Number of TF coil legs','(tfno)',TFNO)

         CALL OSUBHD(NOUT,'Energy and Forces :')
         CALL OVARRE(NOUT,'Stored energy per coil (GJ)','(estotf)',
     +        ESTOTF)
         CALL OVARRE(NOUT,'Vertical force on inner leg (N)','(vforce)',
     +        VFORCE)
         CALL OVARRE(NOUT,'Centering force on inner leg (N/m)',
     +        '(cforce)',CFORCE)
         CALL OVARRE(NOUT,'Radial stress (Pa)','(sigrad)',SIGRAD)
         CALL OVARRE(NOUT,'Transverse stress (Pa)','(sigtan)',SIGTAN)
         CALL OVARRE(NOUT,'Vertical stress (Pa)','(sigver)',SIGVER)

         CALL CONCOPTF(IPRINT,NOUT)

      END IF

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE CONCOPTF(IPRINT,NOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Routine to calculate information for resistive TF coils
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  18 Novermber 1997
C
C--Reference
C  F/MI/PJK/PROCESS/DOCUMENTATION/MANUAL : PROCESS User's Guide
C  F/MI/PJK/PROCESS/CODE : PROCESS Project Work File
C  
C--History
C  22/10/96 PJK 1.000 Initial upgraded version. Also, moved TART
C                     centrepost volume and resistive power loss
C                     calculation into this routine
C  18/11/97 PJK 1.010 Modified RTOP,ZTOP values
C
C--Arguments
C  IPRINT : (INPUT)  Switch for printing to output file
C  NOUT   : (INPUT)  Output unit identifier
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER IPRINT,NOUT

C  Local variables
      DOUBLE PRECISION EXTRA,LTFLEG,RMID,RTOP,TDUMP,ZTOP

C  External routines
      EXTERNAL CPOST,OSUBHD,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 10

      IF (ITART.EQ.0) THEN

C *** Number of turns per leg, = 1 for TARTs

         TURNSTF = RITFC / (TFNO * CPTTF)

      ELSE

         TURNSTF = 1.0D0

C *** Current per turn
C *** (N.B. cannot set CPTTF as an iteration variable for TARTs)

         CPTTF = RITFC/(TURNSTF*TFNO)

      END IF

C *** Outer leg information (per leg)
C *** ===============================

C *** Cross-sectional area

      AREALEG = RITFC/(TFNO * CDTFLEG)
      EXTRA = SQRT(AREALEG)

C *** Length (NB. This assumes rectangular shaped coils, not D-shaped)

      LTFLEG = 2.0D0* (HMAX + EXTRA) + (2.0D0*(RTOT - RBMAX) + EXTRA )

C *** Volume

      VOLTFLEG = LTFLEG * AREALEG

C *** Resistance

      RHOTFLEG = LTFLEG * TFLEGRES/AREALEG

C *** Total weight (of all legs)

      WHTTFLGS = VOLTFLEG * TFNO * (1.0D0 - VFTF) * 8900.0D0

C *** Inner leg information (all legs)
C *** ================================

C *** Resistivity (0.92 factor for glidcop C15175)

      RHOCP = 1.0D-8 * (1.72D0 + 0.0039D0*TCPAV ) / 0.92D0

      IF (ITART.EQ.0) THEN

C ***    Conventional tokamak
C ***    ====================

C ***    Volume

         VOLCP = 2.0D0 * TFAREAIN * HMAX

C *** Resistive power losses

         PRESCP = RHOCP * (RITFC/( TFAREAIN*(1.0D0-FCOOLCP)))**2*VOLCP*
     +        (1.0D0-FCOOLCP)

      ELSE

C ***    Tight Aspect Ratio Tokamak
C ***    ==========================

C ***    Radii and vertical height from midplane

C+**PJK 18/11/97 Added DRTOP to RTOP, and DZTOP to ZTOP
         RTOP = (RMAJOR - RMINOR*TRIANG - FWITH - 3.0D0*SCRAPLI) + DRTOP
         RMID = TFCTH + BCYLTH
         RTOP = MAX(RTOP, (RMID*1.01D0))
         ZTOP = (RMINOR * KAPPA) + DZTOP

C ***    Resistivity enhancement factor

         RHOCP = RHOCP * FRHOCP

C ***    Calculate volume of TART centrepost, plus the resistive
C ***    power losses

         CALL CPOST(NOUT,PI,RTOP,ZTOP,RMID,HMAX,RITFC,RHOCP,FCOOLCP,
     +        VOLCP,PRESCP)

      END IF

C *** Weight of conductor

      WHTCP = VOLCP * 8900.0D0 * (1.0D0 - FCOOLCP)

C *** Total weight of TF coils

      WHTTF = WHTCP + WHTTFLGS

C *** Stress information (radial, tangential, vertical)

      SIGRAD = 1.0D-6 * BMAXTF**2* (5.0D0 + 0.34D0)/  (8.0D0*RMU0 *
     +     (1.0D0 - FCOOLCP) )
      SIGTAN = SIGRAD
      SIGVER = 0.0D0

 10   CONTINUE

      if ((iprint.eq.0).or.(sect07.eq.0)) goto 1000

C *** Output section

      CALL OSUBHD(NOUT,'Conventional Copper TF Coil Information :')
      CALL OVARRE(NOUT,'Inner leg current density (A/m2)','(oacdcp)',
     +     OACDCP)
      CALL OVARRE(NOUT,'Outer leg current density (A/m2)','(cdtfleg)',
     +     CDTFLEG)
      CALL OVARRE(NOUT,'Number of turns per outer leg','(turnstf)',
     +     TURNSTF)
      CALL OVARRE(NOUT,'Outer leg current per turn (A)','(cpttf)',
     +     CPTTF)
      CALL OVARRE(NOUT,'Inner leg volume (m3)','(volcp)',VOLCP)
      CALL OVARRE(NOUT,'Outer leg volume per coil (m3)','(voltfleg)',
     +     VOLTFLEG)
      CALL OVARRE(NOUT,'Mass of inner legs (kg)','(whtcp)',WHTCP)
      CALL OVARRE(NOUT,'Mass of outer legs (kg)','(whttflgs)',WHTTFLGS)
      CALL OVARRE(NOUT,'Total TF coil mass (kg)','(whttf)',WHTTF)
      CALL OVARRE(NOUT,'Inner leg resistive power (W)','(prescp)',
     +     PRESCP)
      CALL OVARRE(NOUT,'Outer leg resistance per coil (ohm)',
     +     '(rhotfleg)',RHOTFLEG)
      CALL OVARRE(NOUT,'Average inner leg temperature (C)','(tcpav)',
     +     TCPAV)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE CNTRPST(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine that evaluates the centrepost quantities.
C  The centrepost is assumed to be tapered, i.e. narrowest
C  on the midplane (z=0).
C
C  This whole routine is relevant for TARTs (Tight Aspect Ratio
C  Tokamaks) only
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  22 October 1996
C
C--Reference
C  F/MI/PJK/PROCESS/DOCUMENTATION/MANUAL: PROCESS User's Guide
C  F/MI/PJK/PROCESS/CODE : PROCESS Project Work File
C  
C--History
C  22/10/96 PJK 1.000 Initial upgraded version. Also first use of
C                     call to CPOST to correct the calculation of
C                     centrepost volume and resistive power losses
C
C--Arguments
C  NOUT   : (INPUT)  Output unit identifier
C  IPRINT : (INPUT)  Switch to turn on/off output to file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'build.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION ACOOL,ACPAV,AMID,DCOOL,DPRES,DTCNCPAV,DTCONCPMX,
     +     DTFILMAV,DTIOCOOL,FC,FRICFAC,H,LCOOL,NUSELT,PCRT,PRESIN,
     +     PRNDLT,PSAT,PTOT,REYN,RMID,RO,ROUGHRAT,SUM,TCLMX,
     +     TCLMXS,TCOOLMX,TMARG,VCOOLAV

C  External routines
      EXTERNAL OHEADR,OSUBHD,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Critical pressure in saturation pressure calculations (Pa)

      PCRT = 2.24D7

C *** Temperature margin used in calculations (K)

      TMARG = 10.0D0

C *** Midplane radius and area

      RMID = TFCTH + BCYLTH
      AMID = PI * (RMID**2 - BCYLTH**2)

C *** Average area

      ACPAV = VOLCP/(2.0D0*HMAX)

C *** Coolant channels:
C *** =================

C *** Cross-sectional area

      ACOOL = ACPAV * FCOOLCP

C *** Diameter

      DCOOL = 2.0D0 * RCOOL

C *** Length

      LCOOL = 2.0D0 * HMAX

C *** Number

      NCOOL = ACOOL/( PI * RCOOL**2)

      RO = SQRT ( ACPAV/(PI*NCOOL) )

C *** Total heating power (to be removed by coolant)

      PTOT = PRESCP + PNUCCP*1.0D6

C *** Temperature calculations

      REYN = DENH2O * VCOOL * DCOOL / MUH2O
      PRNDLT = CPH2O  * MUH2O / KH2O

C *** Temperature rise in coolant (inlet to outlet)

      VCOOLAV = VCOOL * AMID/ACPAV
      DTIOCOOL = PTOT / ( DENH2O*VCOOLAV*ACOOL*CPH2O)

C *** Film temperature rise

      NUSELT = 0.023D0 * REYN**0.8D0 * PRNDLT**0.3D0
      H = NUSELT * KH2O / DCOOL
      DTFILMAV = PTOT / (H * 2.0D0*PI*RCOOL * NCOOL * LCOOL)

C *** Temperature rise in conductor,
C *** for conduction from copper to coolant

      DTCNCPAV = (PTOT/VOLCP)/(2.0D0*KCP*(RO**2 - RCOOL**2) ) *
     +     (RO**2*RCOOL**2 - RCOOL**4/4.0D0 - 0.75D0*RO**4 + RO**4 *
     +     LOG(RO/RCOOL) )

      DTCONCPMX = (PTOT/VOLCP)/(2.0D0*KCP) * ( (RCOOL**2 
     +     - RO**2)/2.0D0 + RO**2 * LOG(RO/RCOOL) )

C *** Average conductor temperature

      TCPAV2 = TCOOLIN + DTCNCPAV + DTFILMAV + DTIOCOOL/2.0D0

C *** Peak wall temperature

      TCPMAX = TCOOLIN + DTIOCOOL + DTFILMAV + DTCONCPMX
      TCOOLMX = TCOOLIN + DTIOCOOL + DTFILMAV

C *** Thermal hydraulics: friction factor from Z. Olujic, Chemical
C *** Engineering, Dec. 1981, p. 91

      ROUGHRAT = 0.046D-3 / DCOOL
      FRICFAC = 1.0D0/ (-2.0D0 * LOG(ROUGHRAT/3.7D0 - 5.02D0/REYN *
     +     LOG( ROUGHRAT/3.7D0 + 14.5D0/REYN) ) )**2

      DPRES = FRICFAC * (LCOOL/DCOOL) * DENH2O * VCOOL**2/2.0D0
      PPUMP = DPRES * ACOOL * VCOOL / ETAPUMP

C *** Saturation pressure

      TCLMX = TCOOLMX + TMARG
      TCLMXS = MIN (TCLMX, 374.0D0)
      FC = 0.65D0 - 0.01D0 * TCLMXS
      SUM = -741.9242D0 - 29.721D0 * FC - 11.55286D0*FC**2 
     +     - 0.8685635D0*FC**3 + 0.1094098D0*FC**4 
     +     + 0.439993D0*FC**5 + 0.2520658D0*FC**6
     +     + 0.0518684D0*FC**7
      PSAT = PCRT * EXP(0.01D0/(TCLMXS + 273.0D0) * 
     +     (374.0D0 - TCLMXS) * SUM )
      PRESIN = PSAT + DPRES

      IF ((IPRINT.EQ.0).OR.(SECT07.EQ.0)) GOTO 1000

C *** Output section

      CALL OHEADR(NOUT,'Centrepost Coolant Parameters')
      CALL OVARRE(NOUT,'Centrepost coolant fraction','(fcoolcp)',
     +     FCOOLCP)
      CALL OVARRE(NOUT,'Average coolant channel diameter (m)','(dcool)'
     +     ,DCOOL)
      CALL OVARRE(NOUT,'Coolant channel length (m)','(lcool)',LCOOL)
      CALL OVARRE(NOUT,'Maximum coolant flow speed (m/s)','(vcool)',
     +     VCOOL)
      CALL OVARRE(NOUT,'Number of coolant tubes','(ncool)',NCOOL)
      CALL OVARRE(NOUT,'Reynolds number','(reyn)',REYN)
      CALL OVARRE(NOUT,'Prandlt number','(prndlt)',PRNDLT)
      CALL OVARRE(NOUT,'Nuselt number','(nuselt)',NUSELT)

      CALL OSUBHD(NOUT,'Resistive Heating :')
      CALL OVARRE(NOUT,'Average conductor resistivity (ohm.m)','(rhocp)'
     +     ,RHOCP)
      CALL OVARRE(NOUT,'Resistive heating (W)','(prescp)',PRESCP)

      CALL OSUBHD(NOUT,'Temperatures :')
      CALL OVARRE(NOUT,'Input coolant temperature (C)','(tcoolin)',
     +     TCOOLIN)
      CALL OVARRE(NOUT,'Input-output coolant temperature rise (C)',
     +     '(dtiocool)',DTIOCOOL)
      CALL OVARRE(NOUT,'Film temperature rise (C)','(dtfilmav)'
     +     ,DTFILMAV)
      CALL OVARRE(NOUT,'Average temp gradient in conductor (K/m)',
     +     '(dtcncpav)',DTCNCPAV)
      CALL OVARRE(NOUT,'Average centrepost temperature (C)','(tcpav2)',
     +     TCPAV2)
      CALL OVARRE(NOUT,'Peak centrepost temperature (C)','(tcpmax)',
     +     TCPMAX)

      CALL OSUBHD(NOUT,'Pump Power :')
      CALL OVARRE(NOUT,'Coolant pressure drop (Pa)','(dpres)',DPRES)
      CALL OVARRE(NOUT,'Coolant inlet pressure (Pa)','(presin)',PRESIN)
      CALL OVARRE(NOUT,'Pump power (W)','(ppump)',PPUMP)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE CPOST(NOUT,PI,RTOP,ZTOP,RMID,HMAX,CURR,RHO,FCOOL,
     +     VOLUME,RESPOW)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the volume and resistive power losses
C  of a TART centrepost. It is assumed to be tapered - narrowest at
C  the midplane and reaching maximum thickness at the height of the
C  plasma. Above/below the plasma, the centrepost is cylindrical.
C  The shape of the taper is assumed to be an arc of a circle.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 October 1996
C
C--Reference
C  F/MI/PJK/LOGBOOK12, pp.33,34
C  
C--History
C  21/10/96 PJK 1.000 Initial version
C
C--Arguments
C  NOUT   : (INPUT)  Output channel specifier
C  PI     : (INPUT)  3.1415...
C  RTOP   : (INPUT)  Radius of the ends of the centrepost (m)
C  ZTOP   : (INPUT)  Distance from the midplane to the top of the
C                    tapered section (m)
C  RMID   : (INPUT)  Radius of the centrepost at the midplane (m)
C  HMAX   : (INPUT)  Distance from the midplane to the top of the
C                    centrepost (m)
C  CURR   : (INPUT)  Centrepost current (A)
C  RHO    : (INPUT)  Centrepost resistivity (Ohm-m)
C  FCOOL  : (INPUT)  Coolant fraction of centrepost
C  VOLUME : (OUTPUT) Centrepost volume (m3)
C  RESPOW : (OUTPUT) Centrepost resistive power losses (W)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION PI,RTOP,ZTOP,RMID,HMAX,CURR,RHO,FCOOL
      DOUBLE PRECISION VOLUME,RESPOW
      INTEGER NOUT

C  Local variables
      DOUBLE PRECISION R1,Z1,X,Y,RC,SUM1,SUM2,DZ,R,Z
      DOUBLE PRECISION YY(0:100)
      INTEGER I

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Error traps

      IF (RTOP.LE.0.0D0) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'RTOP = ',RTOP
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

      IF (ZTOP.LE.0.0D0) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'ZTOP = ',ZTOP
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

      IF (RMID.LE.0.0D0) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'RMID = ',RMID
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

      IF (HMAX.LE.0.0D0) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'HMAX = ',HMAX
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

      IF ((FCOOL.LT.0.0D0).OR.(FCOOL.GT.1.0D0)) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'FCOOL = ',FCOOL
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

      IF (RTOP.LT.RMID) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'RTOP < RMID...'
         WRITE(NOUT,*) 'RTOP = ',RTOP
         WRITE(NOUT,*) 'RMID = ',RMID
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

      IF (HMAX.LT.ZTOP) THEN
         WRITE(NOUT,*) 'Error in routine CPOST:'
         WRITE(NOUT,*) 'HMAX < ZTOP...'
         WRITE(NOUT,*) 'HMAX = ',HMAX
         WRITE(NOUT,*) 'ZTOP = ',ZTOP
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

C *** Trivial solutions

      IF (FCOOL.EQ.1.0D0) THEN
         VOLUME = 0.0D0
         RESPOW = 0.0D0
         WRITE(NOUT,*) 'Warning from routine CPOST:'
         WRITE(NOUT,*) 'Silly answers from CPOST because FCOOL=1.0...'
         WRITE(NOUT,*) 'PROCESS continuing'
         GOTO 1000
      END IF

      IF (RMID.EQ.RTOP) THEN
         VOLUME = 2.0D0*HMAX * PI*RMID*RMID
         RESPOW = 2.0D0 * RHO * CURR*CURR *
     +        HMAX / (PI*RMID*RMID) / (1.0D0-FCOOL)
         GOTO 1000
      END IF

C *** Find centre of circle (RC,0) defining the taper's arc

C *** (R1,Z1) is midpoint of line joining (RMID,0) and (RTOP,ZTOP)

      R1 = 0.5D0*(RMID + RTOP)
      Z1 = 0.5D0*ZTOP

      X = (R1-RMID)**2 + Z1**2
      Y = ZTOP**2 / ( (RTOP-RMID)**2 + ZTOP**2 )

      RC = RMID + SQRT( X / (1.0D0-Y) )

C *** Find volume of tapered section of centrepost, and the resistive
C *** power losses, by integrating along the centrepost
C *** from the midplane

C *** Calculate centrepost radius and cross-sectional area at each Z

      DZ = ZTOP/100.0D0
      DO 10 I = 0,100

         Z = DBLE(I) * DZ
         Z = MIN(Z,ZTOP)

         R = RC - SQRT( (RC-RMID)**2 - Z*Z )

         IF (R.LE.0.0D0) THEN
            WRITE(NOUT,*) 'Error in routine CPOST:'
            WRITE(NOUT,*) 'R(Z) = ',R
            WRITE(NOUT,*) 'PROCESS stopping.'
            STOP
         END IF

C ***    Cross-sectional area at Z

         YY(I) = PI*R*R

 10   CONTINUE

C *** Perform integrals using trapezium rule

      SUM1 = 0.0D0
      SUM2 = 0.0D0
      DO 20 I = 1,99
         SUM1 = SUM1 + YY(I)
         SUM2 = SUM2 + 1.0D0/YY(I)
 20   CONTINUE

      SUM1 = 0.5D0*DZ * ( YY(0) + YY(100) + 2.0D0*SUM1 )
      SUM2 = 0.5D0*DZ * ( 1.0D0/YY(0) + 1.0D0/YY(100) + 2.0D0*SUM2 )

C *** Centrepost volume (ignoring coolant fraction)

      VOLUME = 2.0D0 * (SUM1 + (HMAX-ZTOP)*PI*RTOP*RTOP)

C *** Resistive power losses

      RESPOW = 2.0D0 * RHO * CURR*CURR *
     +     (SUM2 + (HMAX-ZTOP)/(PI*RTOP*RTOP)) / (1.0D0-FCOOL)

 1000 CONTINUE

      RETURN
      END
