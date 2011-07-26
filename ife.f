C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: ife.f,v 1.3 1999/05/19 09:29:26 peter Exp $
C  Module name    : $RCSfile: ife.f,v $
C  Version no.    : $Revision: 1.3 $
C  Creation date  : $Date: 1999/05/19 09:29:26 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE IFEINI
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to initialise the variables used by the Inertial Fusion
C  Energy model
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  10 September 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.91
C  F/MI/PJK/LOGBOOK13, p.2
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C  10/09/97 PJK 1.100 De-escalated driver costs from 1991-1990 dollars
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'ife.h'

C  Local variables
      INTEGER I,J

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Default builds and material volumes are those
C *** for the SOMBRERO device

      BLDR   = 1.0D0
      BLDZL  = 4.0D0
      BLDZU  = 4.0D0
      CDRIV0 = 154.3D0
      CDRIV1 = 163.2D0
      CDRIV2 = 244.9D0
      CHDZL  = 9.0D0
      CHDZU  = 9.0D0
      CHRAD  = 6.5D0
      CHVOL  = 0.0D0
      DCDRV0 = 111.4D0
      DCDRV1 = 78.0D0
      DCDRV2 = 59.9D0
      DRVEFF = 0.28D0
      EDRIVE = 5.0D6
      ETADRV = 0.0D0
      ETAVE(1)  = 0.082D0
      ETAVE(2)  = 0.079D0
      ETAVE(3)  = 0.076D0
      ETAVE(4)  = 0.073D0
      ETAVE(5)  = 0.069D0
      ETAVE(6)  = 0.066D0
      ETAVE(7)  = 0.062D0
      ETAVE(8)  = 0.059D0
      ETAVE(9)  = 0.055D0
      ETAVE(10) = 0.051D0
      FBREED = 0.51D0
      FBURN  = 0.3333D0
      FRRMAX = 1.0D0
      FWDR   = 0.01D0
      FWDZL  = 0.01D0
      FWDZU  = 0.01D0
      GAIN   = 0.0D0
      GAINVE(1)  = 60.0D0
      GAINVE(2)  = 95.0D0
      GAINVE(3)  = 115.0D0
      GAINVE(4)  = 125.0D0
      GAINVE(5)  = 133.0D0
      GAINVE(6)  = 141.0D0
      GAINVE(7)  = 152.0D0
      GAINVE(8)  = 160.0D0
      GAINVE(9)  = 165.0D0
      GAINVE(10) = 170.0D0
      IFEDRV = 2
      IFETYP = 0
      MCDRIV = 1.0D0
      MFLIBE = 0.0D0
      PDRIVE = 23.0D6
      PIFECR = 10.0D0
      PTARGF = 2.0D0
      R1     = 0.0D0
      R2     = 0.0D0
      R3     = 0.0D0
      R4     = 0.0D0
      R5     = 0.0D0
      R6     = 0.0D0
      R7     = 0.0D0
      REPRAT = 0.0D0
      RRMAX  = 20.0D0
      SHDR   = 1.7D0
      SHDZL  = 5.0D0
      SHDZU  = 5.0D0
      SOMBDR = 2.7D0
      SOMTDR = 2.7D0
      TDSPMW = 0.0D0
      TFACMW = 0.0D0
      TGAIN  = 85.0D0
      UCCARB = 50.0D0
      UCCONC = 0.1D0
      UCFLIB = 84.0D0
      UCTARG = 0.3D0
      V1DR   = 0.0D0
      V1DZL  = 0.0D0
      V1DZU  = 0.0D0
      V2DR   = 2.0D0
      V2DZL  = 7.0D0
      V2DZU  = 7.0D0
      V3DR   = 43.3D0
      V3DZL  = 30.0D0
      V3DZU  = 20.0D0
      ZL1    = 0.0D0
      ZL2    = 0.0D0
      ZL3    = 0.0D0
      ZL4    = 0.0D0
      ZL5    = 0.0D0
      ZL6    = 0.0D0
      ZL7    = 0.0D0
      ZU1    = 0.0D0
      ZU2    = 0.0D0
      ZU3    = 0.0D0
      ZU4    = 0.0D0
      ZU5    = 0.0D0
      ZU6    = 0.0D0
      ZU7    = 0.0D0

      DO 20 I = 0,MAXMAT
         CHMATF(I) = 0.0D0
         CHMATM(I) = 0.0D0
         CHMATV(I) = 0.0D0
         DO 10 J = 1,3
            BLMATF(J,I) = 0.0D0
            BLMATM(J,I) = 0.0D0
            BLMATV(J,I) = 0.0D0
            FWMATF(J,I) = 0.0D0
            FWMATM(J,I) = 0.0D0
            FWMATV(J,I) = 0.0D0
            SHMATF(J,I) = 0.0D0
            SHMATM(J,I) = 0.0D0
            SHMATV(J,I) = 0.0D0
            V1MATF(J,I) = 0.0D0
            V1MATM(J,I) = 0.0D0
            V1MATV(J,I) = 0.0D0
            V2MATF(J,I) = 0.0D0
            V2MATM(J,I) = 0.0D0
            V2MATV(J,I) = 0.0D0
            V3MATF(J,I) = 0.0D0
            V3MATM(J,I) = 0.0D0
            V3MATV(J,I) = 0.0D0
 10      CONTINUE
 20   CONTINUE

      CHMATF(0) = 1.0D0

      DO 30 J = 1,3
         BLMATF(J,0) = 0.05D0
         BLMATF(J,2) = 0.45D0
         BLMATF(J,4) = 0.20D0
         BLMATF(J,6) = 0.30D0
         FWMATF(J,0) = 0.05D0
         FWMATF(J,2) = 0.95D0
         SHMATF(J,0) = 0.05D0
         SHMATF(J,1) = 0.19D0
         SHMATF(J,5) = 0.665D0
         SHMATF(J,6) = 0.095D0
         V1MATF(J,0) = 1.0D0
         V2MATF(J,0) = 1.0D0
         V3MATF(J,0) = 1.0D0
 30   CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFECLL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to call the physics and engineering modules
C  relevant to inertial fusion energy power plants
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.4181
C
C--Date
C  19 May 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.66
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C  19/05/99 PJK 1.100 Added call to routine AVAIL
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
      INCLUDE 'numer.h'

C  External routines
      EXTERNAL AVAIL,COSTS,IFEACP,IFEBDG,IFEBLD,IFEFBS,IFEPHY,IFEPW1,
     +     IFEPW2,IFESTR,IFETGT,IFEVAC

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Device build

      CALL IFEBLD(NOUT,0)

C *** IFE physics

      CALL IFEPHY(NOUT,0)

C *** Device structure

      CALL IFESTR(NOUT,0)

C *** Target data

      CALL IFETGT(NOUT,0)

C *** First wall, blanket and shield

      CALL IFEFBS(NOUT,0)

C *** Primary thermal power

      CALL IFEPW1

C *** Vacuum system

      CALL IFEVAC(NOUT,0)

C *** Buildings

      CALL IFEBDG(NOUT,0)

C *** AC power requirements

      CALL IFEACP(NOUT,0)

C *** Secondary thermal power

      CALL IFEPW2(NOUT,0)

C *** Plant availability

      CALL AVAIL(NOUT,0)

C *** Costs

      CALL COSTS(NOUT,0)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEOUT(NOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to output the physics and engineering information
C  relevant to inertial fusion energy power plants
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.4181
C
C--Date
C  19 May 1999
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.66
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C  19/05/99 PJK 1.100 Added call to routine AVAIL
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit specifier
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER NOUT

C  External routines
      EXTERNAL AVAIL,COSTS,IFEACP,IFEBDG,IFEBLD,IFEFBS,IFEPHY,IFEPW1,
     +     IFEPW2,IFESTR,IFETGT,IFEVAC

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Costs

      CALL COSTS(NOUT,1)

C *** Plant availability

      CALL AVAIL(NOUT,1)

C *** IFE physics

      CALL IFEPHY(NOUT,1)

C *** Device build

      CALL IFEBLD(NOUT,1)

C *** First wall, blanket and shield

      CALL IFEFBS(NOUT,1)

C *** Device structure

      CALL IFESTR(NOUT,1)

C *** Target data

      CALL IFETGT(NOUT,1)

C *** Primary thermal power

      CALL IFEPW1

C *** Vacuum system

      CALL IFEVAC(NOUT,1)

C *** Buildings

      CALL IFEBDG(NOUT,1)

C *** AC power requirements

      CALL IFEACP(NOUT,1)

C *** Secondary thermal power

      CALL IFEPW2(NOUT,1)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEPHY(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the physics parameters of an Inertial Fusion
C  Energy power plant
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, pp.68,85
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'osections.h'
      INCLUDE 'phydat.h'
      INCLUDE 'ife.h'
      INCLUDE 'build.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION AAION,BMAX,DPP,DTHETA,EMITT,ETAI,LF,QION,
     +     SIGMA,SIGMA0,TAUF,THETA,VI
      DOUBLE PRECISION PHI,SANG
      INTEGER NBEAMS

C  External routines
      EXTERNAL DRIVER,LASDRV,IONDRV,OBLNKL,OCMMNT,OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 10

C *** Repetition rate

      REPRAT = PDRIVE / EDRIVE

C *** Driver calculations

      IF (IFEDRV.EQ.-1) THEN

C *** Target gain and driver efficiency dependences on
C *** driver energy are input

         CALL DRIVER(EDRIVE,GAINVE,ETAVE,GAIN,ETADRV)

      ELSE IF (IFEDRV.EQ.0) THEN

C *** Target gain and driver efficiency are input

         GAIN = TGAIN
         ETADRV = DRVEFF

      ELSE IF (IFEDRV.EQ.1) THEN

C *** Laser driver based on SOMBRERO design

         CALL LASDRV(EDRIVE,GAIN,ETADRV)

      ELSE IF (IFEDRV.EQ.2) THEN

C *** Heavy-ion beam driver based on OSIRIS design

         AAION = 131.0D0
         BMAX = 10.0D0
         DPP = 1.25D-3
         DTHETA = 1.3D-4
         EMITT = 1.0D-5
         ETAI = 0.8D0
         LF = 5.0D0
         NBEAMS = 12
         QION = 1.0D0
         SIGMA = 8.0D0
         SIGMA0 = 80.0D0
         TAUF = 1.0D-7
         THETA = 30.0D-3
         VI = 3.0D6

         CALL IONDRV(AAION,BMAX,DPP,DTHETA,EDRIVE,EMITT,ETAI,LF,
     +        NBEAMS,QION,REPRAT,SIGMA,SIGMA0,TAUF,THETA,VI,GAIN,ETADRV)

      ELSE

         WRITE(NOUT,*) 'Error in routine IFEPHY:'
         WRITE(NOUT,*) 'Illegal value for IFEDRV, = ',IFEDRV
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP

      END IF

C *** Fusion power (MW)

      POWFMW = 1.0D-6 * PDRIVE * GAIN

C *** Wall load (assume total fusion power applies)

      IF (IFETYP.EQ.1) THEN

C        *** OSIRIS-type build: First wall subtends a
C        *** solid angle of 2 pi * SANG

         PHI = 0.5D0*PI + ATAN(ZL1/R1)
         SANG = 1.0D0 - COS(PHI)
         WALLMW = POWFMW * 0.5D0*SANG / FWAREA

      ELSE

         WALLMW = POWFMW / FWAREA

      END IF

 10   CONTINUE

C *** Output section

      IF ((IPRINT.EQ.0).OR.(SECT03.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'Physics / Driver Issues')

      IF ((IFEDRV.EQ.0).OR.(IFEDRV.EQ.-1)) THEN
         CALL OCMMNT(NOUT,'Driver type : generic')
      ELSE IF (IFEDRV.EQ.1) THEN
         CALL OCMMNT(NOUT,'Driver type : laser')
      ELSE
         CALL OCMMNT(NOUT,'Driver type : heavy ion beam')
      END IF
      CALL OBLNKL(NOUT)

      CALL OVARRE(NOUT,'Driver energy (J)','(edrive)',EDRIVE)
      CALL OVARRE(NOUT,'Driver efficiency','(etadrv)',ETADRV)
      CALL OVARRE(NOUT,'Driver power reaching target (W)','(pdrive)',
     +     PDRIVE)
      CALL OVARRE(NOUT,'Driver repetition rate (Hz)','(reprat)',REPRAT)
      CALL OVARRE(NOUT,'Target gain','(gain)',GAIN)
      CALL OVARRE(NOUT,'Fusion power (MW)','(powfmw)',POWFMW)
      CALL OVARRE(NOUT,'Neutron wall load (MW/m2)','(wallmw)',WALLMW)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IONDRV(AAION,BMAX,DPP,DTHETA,EDRIVE,EMITT,ETAI,LF,
     +     NBEAMS,QION,REPRAT,SIGMA,SIGMA0,TAUF,THETA,VI,GAIN,ETADRV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate parameters of a heavy ion driver
C  suitable for inertial fusion energy
C  Currently, the complicated model taken from the reference is not
C  complete, so it is recommended that the simple model is used
C  (set ISIMP=1)
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  Heavy-ion Driver Design and Scaling, R. Bieri et al.,
C    Fusion Technology, vol.21 (1992) 1583
C  Meier and Bieri, Fusion Technology, vol.21 (1992) 1547
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  AAION  : (INPUT)  Ion mass (amu)
C  BMAX   : (INPUT)  Maximum field at the superconductor (T)
C  DPP    : (INPUT)  Beam momentum spread
C  DTHETA : (INPUT)  Pointing error (rad)
C  EDRIVE : (INPUT)  Driver energy (J)
C  EMITT  : (INPUT)  Normalised beam emittance (metre-rad)
C  ETAI   : (INPUT)  Axial quadrupole packing fraction at injection
C  LF     : (INPUT)  Distance from final focussing quad to target (m)
C  NBEAMS : (INPUT)  Number of beams
C  QION   : (INPUT)  Ion charge state
C  REPRAT : (INPUT)  Repetition rate (Hz)
C  SIGMA  : (INPUT)  Depressed tune (incl. space charge effects) (deg)
C  SIGMA0 : (INPUT)  Phase advance per lattice period (tune) (deg)
C  TAUF   : (INPUT)  Post-acceleration pulse duration (s)
C  THETA  : (INPUT)  Final focussing half angle (rad)
C  VI     : (INPUT)  Injection voltage (V)
C  GAIN   : (OUTPUT) Target gain
C  ETADRV : (OUTPUT) Driver efficiency
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION DEGRAD,ECHRGE,MPROT,C2,PI
      PARAMETER (
     +     DEGRAD = 0.01745329251D0,
     +     ECHRGE = 1.60217733D-19,
     +     MPROT  = 1.6726231D-27,
     +     C2     = 8.98755178737D16,
     +     PI     = 3.141592653589793D0 )

C  Arguments
      DOUBLE PRECISION AAION,BMAX,DPP,DTHETA,EDRIVE,EMITT,ETAI,LF,QION,
     +     REPRAT,SIGMA,SIGMA0,TAUF,THETA,VI,GAIN,ETADRV
      INTEGER NBEAMS

C  Local variables
      DOUBLE PRECISION CI,RION,RS1,RS2,RS3,RS4,RS,SIG,SIG0,EOMC2
      DOUBLE PRECISION TBRAD,VS,DGAP,FLOSS,FINS
      DOUBLE PRECISION     IBI, TAUI, PHII, LPI
      DOUBLE PRECISION VFO,IBFO,TAUFO,PHIFO,LPFO,XLE
      DOUBLE PRECISION VPC,IBPC,TAUPC,PHIPC,LPPC,XPC
      DOUBLE PRECISION VF, IBF,       PHIF, LPF, XHE
      DOUBLE PRECISION DRCORE,DLCORE,VSCORE,VSLE,VSPC,VSHE,LQ,LFOCUS

      INTEGER ISIMP
      INTEGER I,J
      INTEGER NCLE,NCPC,NCHE,NQLE,NQPCHE,NQARRS,NQUADS

C  Local variables for simple model
      DOUBLE PRECISION E,DE
      DOUBLE PRECISION GVE(10),EVE(10)
      INTEGER IE

C  External functions
      DOUBLE PRECISION CBEAM,BETGAM
      EXTERNAL CBEAM,BETGAM

C  External routines

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ISIMP = 1

      IF (ISIMP.EQ.1) GOTO 10

C++** Complex IONDRV model (incomplete)...
C *** ====================

C *** Electron charge / (proton mass * c**2)

      EOMC2 = ECHRGE / (MPROT*C2)

C *** Degrees to radians

      SIG = SIGMA * DEGRAD
      SIG0 = SIGMA0 * DEGRAD

C *** Beam current coefficient

      CI = CBEAM(ETAI,BMAX,AAION,QION,SIG,SIG0,EMITT,VI,EOMC2)

C *** Final voltage (V)

      VF = (EDRIVE/(DBLE(NBEAMS)*TAUF*CI))**(0.666666D0)

C *** Final beam current (A)

      IBF = CI*SQRT(VF)

C *** Ion range (g/cm^2)

      RION = (3.04D-5 * AAION + 349.0D0*AAION**(-2.21D0)) *
     +     (QION*VF/1.0D9)**(1.72D0-0.00275D0*AAION)

C *** Convert to kg/m^2

      RION = RION*10.0D0

C *** Total spot size (rms of four terms)

C *** emittance term

      RS1 = EMITT/(BETGAM(AAION,QION,VF)*THETA)

C *** dispersion (momentum spread) term

      RS2 = 8.0D0*DPP*LF*THETA

C *** alignment and jitter term

      RS3 = DTHETA*LF

C *** space charge term (inaccurate at present)

      RS4 = 1.0D-3

      RS = SQRT(RS1**2+RS2**2+RS3**2+RS4**2)

C *** Target gain (inaccurate at present)

      GAIN = 62.6D0 - 142.3D0*RS**1.25D0 * RION**0.633D0 +
     +     (32.8D0+82.1D0*RS)**0.783D0 * RION**0.647D0 *
     +     LOG(1.0D-6*EDRIVE)

C *** Source/Injector characteristics
C *** ===============================

C *** Transport beam radius (m)

      TBRAD = ( (EMITT**2 * SIG0 * C2)/(SIG**2 * BMAX/1.5D0 * ETAI *
     +     SQRT(VI)) )**(0.333333D0) * SQRT(2.0D0 * AAION * MPROT /
     +     (QION * ECHRGE))

C *** Extractor voltage (V)

      VS = 4.0D0/3.0D0 * VI

C *** Source extraction gap width (m)

      DGAP = 0.8D0

C *** Injected current per beam (A)

      IBI = 4.0D0 * PI * TBRAD**2 * 5.46D-8 *
     +     SQRT(QION*VS**3/(AAION*DGAP**4))

C *** Beam loss fraction between injector and target

      FLOSS = 0.08D0

C *** Pulse duration at injection (s)

      TAUI = 1.0D0/(1.0D0-FLOSS) * TAUF * IBF / IBI

C *** Pulse length at injection (m)

      LPI = TAUI * SQRT( 2.0D0 * QION * ECHRGE * VI / (AAION * MPROT) )

C *** Initial voltage gradient (V/m)

      PHII = 0.3D0 * VI / LPI

C *** End of Low Energy Transport Stage
C *** =================================

C *** Insulated fraction of driver length

      FINS = 0.85D0

C *** Maximum voltage gradient, limited by insulator flash-over (V/m)

      PHIFO = 1.0D6 * FINS

C *** Voltage at which this limit is reached (V)
C *** The end of the low energy stage is set by the point at which
C *** the limit is reached.

      VFO = (PHIFO/PHII)**(2.0D0/3.0D0) * VI

C *** Beam current (A)

      IBFO = IBI * (VFO/VI)

C *** Pulse duration (s)

      TAUFO = TAUI * (VI/VFO)

C *** Pulse length (m)

      LPFO = TAUI * VI / VFO * SQRT( 2.0D0 * QION * ECHRGE * VFO /
     +     (AAION * MPROT) )

C *** Length of the low energy transport stage (m)
C *** (rearrangement of integral of PHI(V).dl)

      XLE = 2.0D0*VI**1.5D0 / PHII * (1.0D0/SQRT(VI) - 1.0D0/SQRT(VFO))

C *** End of Pulse Compression Stage
C *** ==============================

C *** Pulse length 
C *** = final pulse length (m)

      LPF = TAUF * SQRT( 2.0D0 * QION * ECHRGE * VF / (AAION * MPROT) )
      LPPC = LPF

C *** Length of the pulse compression region (m)

      XPC = (LPFO - LPF)/0.3D0

C *** Voltage gradient (V/m) - limit already reached

      PHIPC = PHIFO

C *** Voltage (V)

      VPC = VFO + (XPC * PHIPC)

C *** Beam current (A)

      IBPC = IBFO * (LPFO/LPPC) * SQRT(VPC/VFO)

C *** Pulse duration (s)

      TAUPC = TAUFO * (LPPC/LPFO) * SQRT(VFO/VPC)

C *** End of High Energy Transport Stage
C *** ==================================

C *** Voltage gradient (V/m) - limit already reached

      PHIF = PHIFO

C *** Length of the high energy transport stage (m)

      XHE = (VF - VFO)/PHIFO

      WRITE(*,*) '                 Injector:'
      WRITE(*,*) '   VI = ',VI
      WRITE(*,*) '  IBI = ',IBI
      WRITE(*,*) ' TAUI = ',TAUI
      WRITE(*,*) ' PHII = ',PHII
      WRITE(*,*) '  LPI = ',LPI
      WRITE(*,*) '                 End of LET stage:'
      WRITE(*,*) '  VFO = ',VFO
      WRITE(*,*) ' IBFO = ',IBFO
      WRITE(*,*) 'TAUFO = ',TAUFO
      WRITE(*,*) 'PHIFO = ',PHIFO
      WRITE(*,*) ' LPFO = ',LPFO
      WRITE(*,*) '  XLE = ',XLE
      WRITE(*,*) '                 End of PC stage:'
      WRITE(*,*) '  VPC = ',VPC
      WRITE(*,*) ' IBPC = ',IBPC
      WRITE(*,*) 'TAUPC = ',TAUPC
      WRITE(*,*) 'PHIPC = ',PHIPC
      WRITE(*,*) ' LPPC = ',LPPC
      WRITE(*,*) '  XPC = ',XPC
      WRITE(*,*) '                 End of HET stage:'
      WRITE(*,*) '   VF = ',VF
      WRITE(*,*) '  IBF = ',IBF
      WRITE(*,*) ' TAUF = ',TAUF
      WRITE(*,*) ' PHIF = ',PHIF
      WRITE(*,*) '  LPF = ',LPF
      WRITE(*,*) '  XHE = ',XHE

C *** Volt-second requirements and number of cores
C *** ============================================

C *** Volt-seconds per core (2.4T is field swing of the core
C *** material metglas)

      DRCORE = 0.8D0
      DLCORE = 0.2D0
      VSCORE = 2.4D0 * DRCORE * DLCORE

C *** Low energy stage

      VSLE = TAUI * VI * LOG(VFO/VI)
      NCLE = 1+INT(VSLE/VSCORE)

C *** Pulse compression stage

      VSPC = 2.0D0 * TAUF * SQRT(VF) * (SQRT(VPC)-SQRT(VFO))
      NCPC = 1+INT(VSPC/VSCORE)

C *** High energy stage

      DRCORE = 0.4D0
      DLCORE = 0.1D0
      VSCORE = 2.4D0 * DRCORE * DLCORE

      VSHE = 2.0D0 * TAUF * VF * (1.0D0 - SQRT(VPC/VF))
      NCHE = 1+INT(VSHE/VSCORE)

      WRITE(*,*) '                 Number of cores: '
      WRITE(*,*) ' NCLE = ',NCLE
      WRITE(*,*) ' NCPC = ',NCPC
      WRITE(*,*) ' NCHE = ',NCHE

C *** Quadrupole requirements
C *** =======================

C *** Effective quadrupole length (m)

      LQ = ( EMITT * ETAI * SIG0**2 * SQRT(C2*VI) /
     +     (SIG*(BMAX/1.5D0)**2) )**(0.333333D0) *
     +     SQRT(2.0D0*AAION*MPROT/(QION*ECHRGE))

      WRITE(*,*) '   LQ = ',LQ

C *** Actual quadrupole length (m)

      WRITE(*,*) '  ALQ = ',LQ/ETAI

C *** Length of quadrupole focussing fields: low energy stage

      LFOCUS = ETAI * VI/PHII * (1.0D0 - VI/VFO)
      NQLE = 1+INT(LFOCUS/LQ)

C *** Pulse compression + high energy stages

      LFOCUS = 2.0D0 * ETAI * SQRT(VI)/PHIFO * (SQRT(VF) - SQRT(VFO))

      NQPCHE = 1+INT(LFOCUS/LQ)

C *** Number of quadrupole arrays

      NQARRS = NQLE + NQPCHE

C *** Number of quadrupoles

      NQUADS = NQARRS * NBEAMS

      WRITE(*,*) 'NQARRS = ',NQARRS
      WRITE(*,*) 'NQUADS = ',NQUADS

      ETADRV = 0.28D0

C *** End of complex model **********************

      GOTO 1000

 10   CONTINUE

C *** Simple model
C *** ============

C *** GVE(K): target gain at EDRIVE = K MegaJoules

      GVE(1)  = 25.0D0
      GVE(2)  = 44.0D0
      GVE(3)  = 62.0D0
      GVE(4)  = 76.0D0
      GVE(5)  = 87.0D0
      GVE(6)  = 97.0D0
      GVE(7)  = 107.0D0
      GVE(8)  = 115.0D0
      GVE(9)  = 125.0D0
      GVE(10) = 132.0D0

C *** EVE(K): driver efficiency at EDRIVE = K MegaJoules

      EVE(1)  = 0.232D0
      EVE(2)  = 0.256D0
      EVE(3)  = 0.269D0
      EVE(4)  = 0.276D0
      EVE(5)  = 0.282D0
      EVE(6)  = 0.286D0
      EVE(7)  = 0.290D0
      EVE(8)  = 0.292D0
      EVE(9)  = 0.294D0
      EVE(10) = 0.296D0

      E = 1.0D-6 * EDRIVE
      IE = INT(E)
      DE = E - DBLE(IE)

C *** Assume linear interpolations and extrapolations
C *** Would be better to prevent extrapolation

      IF (IE.LE.1) THEN

         GAIN = GVE(2) - 1.0D-6*(EDRIVE-2.0D6)*(GVE(1)-GVE(2))
         ETADRV = EVE(2) - 1.0D-6*(EDRIVE-2.0D6)*(EVE(1)-EVE(2))

      ELSE IF (IE.GE.9) THEN

         GAIN = GVE(9) + 1.0D-6*(EDRIVE-9.0D6)*(GVE(10)-GVE(9))
         ETADRV = EVE(9) + 1.0D-6*(EDRIVE-9.0D6)*(EVE(10)-EVE(9))

      ELSE

         GAIN = GVE(IE) + DE*(GVE(IE+1)-GVE(IE))
         ETADRV = EVE(IE) + DE*(EVE(IE+1)-EVE(IE))

      END IF

C *** Ensure sensible values

      GAIN = MAX(0.01D0,GAIN)
      ETADRV = MAX(0.01D0,ETADRV)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION CBEAM(ETAI,BMAX,AAION,QION,SIGMA,
     +     SIGMA0,EMITT,VI,EOMC2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to evaluate the beam current coefficient, CI
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  Heavy-ion Driver Design and Scaling, R. Bieri et al.,
C    Fusion Technology, vol.21 (1992) 1583
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  ETAI   : (INPUT)  Axial quadrupole packing fraction at injection
C  BMAX   : (INPUT)  Maximum field at the superconductor (T)
C  AAION  : (INPUT)  Ion mass (amu)
C  QION   : (INPUT)  Ion charge state
C  SIGMA  : (INPUT)  Depressed tune (incl. space charge effects) (rad)
C  SIGMA0 : (INPUT)  Phase advance per lattice period (tune) (rad)
C  EMITT  : (INPUT)  Normalised beam emittance (metre-rad)
C  VI     : (INPUT)  Injection voltage (V)
C  EOMC2  : (INPUT)  Electron charge / (proton mass * light speed**2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION ETAI,BMAX,AAION,QION,SIGMA,SIGMA0,EMITT,VI,EOMC2

C  Local variables
      DOUBLE PRECISION BBE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Maximum field at the beam edge

      BBE = BMAX/1.5D0

C *** Perform calculation

      CBEAM = 2.89D6*(1.0D0-(SIGMA/SIGMA0)**2) *
     +     (SIGMA0**4 * ETAI**2 * VI * AAION/QION * (EMITT/SIGMA)**2
     +     * BBE**2)**0.333333D0 *
     +     (2.0D0 * QION * EOMC2 / AAION)**(5.0D0/6.0D0)

      RETURN
      END
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BETGAM(AAION,QION,V)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the relativistic factor (beta*gamma) for the
C  heavy ions in the beam driver
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  Heavy-ion Driver Design and Scaling, R. Bieri et al.,
C    Fusion Technology, vol.21 (1992) 1583
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  AAION  : (INPUT)  Ion mass (amu)
C  QION   : (INPUT)  Ion charge state
C  V      : (INPUT)  Acceleration voltage (V)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION ECHRGE,MPROT,C2
      PARAMETER (
     +     ECHRGE = 1.60217733D-19,
     +     MPROT  = 1.6726231D-27,
     +     C2     = 8.98755178737D16 )

C  Arguments
      DOUBLE PRECISION AAION,QION,V

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      BETGAM = SQRT( 2.0D0*QION*ECHRGE*V / (AAION*MPROT*C2) )

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE LASDRV(EDRIVE,GAIN,ETADRV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate parameters of a laser driver
C  suitable for inertial fusion energy.
C  Gain and driver efficiency data are taken from Figures 1 and 2 of
C  Meier and Rosenberg
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--References
C  Meier and Rosenberg, Fusion Technology vol.21 (1992) p.1552
C  F/MI/PJK/LOGBOOK12, p.86
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  EDRIVE : (INPUT)  Driver energy (J)
C  GAIN   : (OUTPUT) Target gain
C  ETADRV : (OUTPUT) Driver efficiency
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION EDRIVE,ETADRV,GAIN

C  Local variables
      DOUBLE PRECISION E,DE
      DOUBLE PRECISION GVE(10),EVE(10)
      INTEGER IE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** GVE(K): target gain at EDRIVE = K MegaJoules

      GVE(1)  = 63.0D0
      GVE(2)  = 95.0D0
      GVE(3)  = 112.0D0
      GVE(4)  = 125.0D0
      GVE(5)  = 136.0D0
      GVE(6)  = 144.0D0
      GVE(7)  = 151.0D0
      GVE(8)  = 157.0D0
      GVE(9)  = 162.0D0
      GVE(10) = 166.0D0

C *** EVE(K): driver efficiency at EDRIVE = K MegaJoules

      EVE(1)  = 0.082D0
      EVE(2)  = 0.079D0
      EVE(3)  = 0.076D0
      EVE(4)  = 0.072D0
      EVE(5)  = 0.069D0
      EVE(6)  = 0.064D0
      EVE(7)  = 0.059D0
      EVE(8)  = 0.054D0
      EVE(9)  = 0.048D0
      EVE(10) = 0.042D0

      E = 1.0D-6 * EDRIVE
      IE = INT(E)
      DE = E - DBLE(IE)

C *** Assume linear interpolations and extrapolations
C *** Would be better to prevent extrapolation

      IF (IE.LE.1) THEN

         GAIN = GVE(2) - 1.0D-6*(EDRIVE-2.0D6)*(GVE(1)-GVE(2))
         ETADRV = EVE(2) - 1.0D-6*(EDRIVE-2.0D6)*(EVE(1)-EVE(2))

      ELSE IF (IE.GE.9) THEN

         GAIN = GVE(9) + 1.0D-6*(EDRIVE-9.0D6)*(GVE(10)-GVE(9))
         ETADRV = EVE(9) + 1.0D-6*(EDRIVE-9.0D6)*(EVE(10)-EVE(9))

      ELSE

         GAIN = GVE(IE) + DE*(GVE(IE+1)-GVE(IE))
         ETADRV = EVE(IE) + DE*(EVE(IE+1)-EVE(IE))

      END IF

C *** Ensure sensible values

      GAIN = MAX(0.01D0,GAIN)
      ETADRV = MAX(0.01D0,ETADRV)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE DRIVER(EDRIVE,GAINVE,ETAVE,GAIN,ETADRV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate parameters of a generic driver
C  suitable for inertial fusion energy.
C  The gain and driver efficiency are interpolated from input data
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.85
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  EDRIVE : (INPUT)  Driver energy (J)
C  GAINVE : (INPUT)  Array containing gain vs energy data
C  ETAVE  : (INPUT)  Array containing driver efficiency vs energy data
C  GAIN   : (OUTPUT) Target gain
C  ETADRV : (OUTPUT) Driver efficiency
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION EDRIVE,ETADRV,GAIN
      DOUBLE PRECISION ETAVE(10),GAINVE(10)

C  Local variables
      DOUBLE PRECISION DE,E
      INTEGER IE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** The arrays contain data points for EDRIVE = 1MJ, 2MJ, ... , 10MJ

      E = 1.0D-6 * EDRIVE
      IE = INT(E)
      DE = E - DBLE(IE)

C *** Assume linear interpolations and extrapolations

      IF (IE.LE.1) THEN

         GAIN = GAINVE(2) - 1.0D-6*(EDRIVE-2.0D6)*(GAINVE(1)-GAINVE(2))
         ETADRV = ETAVE(2) - 1.0D-6*(EDRIVE-2.0D6)*(ETAVE(1)-ETAVE(2))

      ELSE IF (IE.GE.9) THEN

         GAIN = GAINVE(9) + 1.0D-6*(EDRIVE-9.0D6)*(GAINVE(10)-GAINVE(9))
         ETADRV = ETAVE(9) + 1.0D-6*(EDRIVE-9.0D6)*(ETAVE(10)-ETAVE(9))

      ELSE

         GAIN = GAINVE(IE) + DE*(GAINVE(IE+1)-GAINVE(IE))
         ETADRV = ETAVE(IE) + DE*(ETAVE(IE+1)-ETAVE(IE))

      END IF

C *** Ensure sensible values

      GAIN = MAX(0.01D0,GAIN)
      ETADRV = MAX(0.01D0,ETADRV)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEBLD(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to create the 'build' of an inertial fusion energy device
C  and to calculate the material volumes for the device core
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.52
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'osections.h'
      INCLUDE 'ife.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      INTEGER I

C  External functions

C  External routines
      EXTERNAL OSIBLD,SOMBLD,HYLBLD,GENBLD,OHEADR,OBUILD

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 10

      IF (IFETYP.EQ.1) THEN

C ***    OSIRIS-type device

         CALL OSIBLD

      ELSE IF (IFETYP.EQ.2) THEN

C ***    SOMBRERO-type device

         CALL SOMBLD

      ELSE IF (IFETYP.EQ.3) THEN

C ***    HYLIFE-II-type device

         CALL HYLBLD

      ELSE

C ***    Generic device

         CALL GENBLD

      END IF

 10   CONTINUE

C *** Output section

      IF ((IPRINT.EQ.0).OR.(SECT06.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'Radial Build')

      WRITE(NOUT,20)
 20   FORMAT(T43,'Thickness (m)',T60,'Radius (m)')

      CALL OBUILD(NOUT,'Device centreline',0.0D0,0.0D0)
      CALL OBUILD(NOUT,'Chamber',CHRAD,R1)
      CALL OBUILD(NOUT,'First Wall',FWDR,R2)
      CALL OBUILD(NOUT,'Void 1',V1DR,R3)
      CALL OBUILD(NOUT,'Blanket',BLDR,R4)
      CALL OBUILD(NOUT,'Void 2',V2DR,R5)
      CALL OBUILD(NOUT,'Shield',SHDR,R6)
      CALL OBUILD(NOUT,'Void 3',V3DR,R7)

      CALL OHEADR(NOUT,'Vertical Build')

      WRITE(NOUT,30)
 30   FORMAT(T43,'Thickness (m)',T60,'Height (m)')

      CALL OBUILD(NOUT,'Base of device',0.0D0,-ZL7)
      CALL OBUILD(NOUT,'Void 3',V3DZL,-ZL6)
      CALL OBUILD(NOUT,'Shield',SHDZL,-ZL5)
      CALL OBUILD(NOUT,'Void 2',V2DZL,-ZL4)
      CALL OBUILD(NOUT,'Blanket',BLDZL,-ZL3)
      CALL OBUILD(NOUT,'Void 1',V1DZL,-ZL2)
      CALL OBUILD(NOUT,'First Wall',FWDZL,-ZL1)
      CALL OBUILD(NOUT,'Chamber',CHDZL,0.0D0)
      CALL OBUILD(NOUT,'Chamber',CHDZU,ZU1)
      CALL OBUILD(NOUT,'First Wall',FWDZU,ZU2)
      CALL OBUILD(NOUT,'Void 1',V1DZU,ZU3)
      CALL OBUILD(NOUT,'Blanket',BLDZU,ZU4)
      CALL OBUILD(NOUT,'Void 2',V2DZU,ZU5)
      CALL OBUILD(NOUT,'Shield',SHDZU,ZU6)
      CALL OBUILD(NOUT,'Void 3',V3DZU,ZU7)

C *** Print matrix of material volumes

      CALL OHEADR(NOUT,'Material volumes')

      WRITE (NOUT,*) '         Chamber  1st wall  Void 1  Blanket  '
     +     //' Void 2   Shield   Void 3'
      WRITE (NOUT,'(A9,7(1pe9.2))') 'void     ',
     +     CHMATV(0),
     +     (FWMATV(1,0)+FWMATV(2,0)+FWMATV(3,0)),
     +     (V1MATV(1,0)+V1MATV(2,0)+V1MATV(3,0)),
     +     (BLMATV(1,0)+BLMATV(2,0)+BLMATV(3,0)),
     +     (V2MATV(1,0)+V2MATV(2,0)+V2MATV(3,0)),
     +     (SHMATV(1,0)+SHMATV(2,0)+SHMATV(3,0)),
     +     (V3MATV(1,0)+V3MATV(2,0)+V3MATV(3,0))
      WRITE (NOUT,'(A9,7(1pe9.2))') 'steel    ',
     +     CHMATV(1),
     +     (FWMATV(1,1)+FWMATV(2,1)+FWMATV(3,1)),
     +     (V1MATV(1,1)+V1MATV(2,1)+V1MATV(3,1)),
     +     (BLMATV(1,1)+BLMATV(2,1)+BLMATV(3,1)),
     +     (V2MATV(1,1)+V2MATV(2,1)+V2MATV(3,1)),
     +     (SHMATV(1,1)+SHMATV(2,1)+SHMATV(3,1)),
     +     (V3MATV(1,1)+V3MATV(2,1)+V3MATV(3,1))

      WRITE (NOUT,'(A9,7(1pe9.2))') 'carbon   ',
     +     CHMATV(2),
     +     (FWMATV(1,2)+FWMATV(2,2)+FWMATV(3,2)),
     +     (V1MATV(1,2)+V1MATV(2,2)+V1MATV(3,2)),
     +     (BLMATV(1,2)+BLMATV(2,2)+BLMATV(3,2)),
     +     (V2MATV(1,2)+V2MATV(2,2)+V2MATV(3,2)),
     +     (SHMATV(1,2)+SHMATV(2,2)+SHMATV(3,2)),
     +     (V3MATV(1,2)+V3MATV(2,2)+V3MATV(3,2))

      WRITE (NOUT,'(A9,7(1pe9.2))') 'FLiBe    ',
     +     CHMATV(3),
     +     (FWMATV(1,3)+FWMATV(2,3)+FWMATV(3,3)),
     +     (V1MATV(1,3)+V1MATV(2,3)+V1MATV(3,3)),
     +     (BLMATV(1,3)+BLMATV(2,3)+BLMATV(3,3)),
     +     (V2MATV(1,3)+V2MATV(2,3)+V2MATV(3,3)),
     +     (SHMATV(1,3)+SHMATV(2,3)+SHMATV(3,3)),
     +     (V3MATV(1,3)+V3MATV(2,3)+V3MATV(3,3))

      WRITE (NOUT,'(A9,7(1pe9.2))') 'Li2O     ',
     +     CHMATV(4),
     +     (FWMATV(1,4)+FWMATV(2,4)+FWMATV(3,4)),
     +     (V1MATV(1,4)+V1MATV(2,4)+V1MATV(3,4)),
     +     (BLMATV(1,4)+BLMATV(2,4)+BLMATV(3,4)),
     +     (V2MATV(1,4)+V2MATV(2,4)+V2MATV(3,4)),
     +     (SHMATV(1,4)+SHMATV(2,4)+SHMATV(3,4)),
     +     (V3MATV(1,4)+V3MATV(2,4)+V3MATV(3,4))

      WRITE (NOUT,'(A9,7(1pe9.2))') 'concrete ',
     +     CHMATV(5),
     +     (FWMATV(1,5)+FWMATV(2,5)+FWMATV(3,5)),
     +     (V1MATV(1,5)+V1MATV(2,5)+V1MATV(3,5)),
     +     (BLMATV(1,5)+BLMATV(2,5)+BLMATV(3,5)),
     +     (V2MATV(1,5)+V2MATV(2,5)+V2MATV(3,5)),
     +     (SHMATV(1,5)+SHMATV(2,5)+SHMATV(3,5)),
     +     (V3MATV(1,5)+V3MATV(2,5)+V3MATV(3,5))

      WRITE (NOUT,'(A9,7(1pe9.2))') 'helium   ',
     +     CHMATV(6),
     +     (FWMATV(1,6)+FWMATV(2,6)+FWMATV(3,6)),
     +     (V1MATV(1,6)+V1MATV(2,6)+V1MATV(3,6)),
     +     (BLMATV(1,6)+BLMATV(2,6)+BLMATV(3,6)),
     +     (V2MATV(1,6)+V2MATV(2,6)+V2MATV(3,6)),
     +     (SHMATV(1,6)+SHMATV(2,6)+SHMATV(3,6)),
     +     (V3MATV(1,6)+V3MATV(2,6)+V3MATV(3,6))

      WRITE (NOUT,'(A9,7(1pe9.2))') 'xenon    ',
     +     CHMATV(7),
     +     (FWMATV(1,7)+FWMATV(2,7)+FWMATV(3,7)),
     +     (V1MATV(1,7)+V1MATV(2,7)+V1MATV(3,7)),
     +     (BLMATV(1,7)+BLMATV(2,7)+BLMATV(3,7)),
     +     (V2MATV(1,7)+V2MATV(2,7)+V2MATV(3,7)),
     +     (SHMATV(1,7)+SHMATV(2,7)+SHMATV(3,7)),
     +     (V3MATV(1,7)+V3MATV(2,7)+V3MATV(3,7))

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GENBLD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to create the 'build' of a generic inertial fusion energy
C  device, assumed to be cylindrically-symmetric, and to calculate
C  the material volumes for the device core
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.52
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION PI
      PARAMETER (
     +     PI     = 3.141592653589793D0 )

C  INCLUDE files
      INCLUDE 'build.h'
      INCLUDE 'ife.h'

C  Local variables
      INTEGER I,J

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Radial build

      R1 = CHRAD
      R2 = R1 + FWDR
      R3 = R2 + V1DR
      R4 = R3 + BLDR
      R5 = R4 + V2DR
      R6 = R5 + SHDR
      R7 = R6 + V3DR

C *** Vertical build (below midplane)

      ZL1 = CHDZL
      ZL2 = ZL1 + FWDZL
      ZL3 = ZL2 + V1DZL
      ZL4 = ZL3 + BLDZL
      ZL5 = ZL4 + V2DZL
      ZL6 = ZL5 + SHDZL
      ZL7 = ZL6 + V3DZL

C *** Vertical build (above midplane)

      ZU1 = CHDZU
      ZU2 = ZU1 + FWDZU
      ZU3 = ZU2 + V1DZU
      ZU4 = ZU3 + BLDZU
      ZU5 = ZU4 + V2DZU
      ZU6 = ZU5 + SHDZU
      ZU7 = ZU6 + V3DZU

C *** Component volumes
C *** The following notation applies below:
C *** J=1 : side part
C *** J=2 : top part
C *** J=3 : bottom part

C *** Chamber

      CHVOL = PI * R1*R1 * (ZU1 + ZL1)

C *** First wall

      FWVOL(1) = PI * (R2*R2 - R1*R1) * (ZU1 + ZL1)
      FWVOL(2) = PI * R2*R2 * (ZU2 - ZU1)
      FWVOL(3) = PI * R2*R2 * (ZL2 - ZL1)

C *** First void

      V1VOL(1) = PI * (R3*R3 - R2*R2) * (ZU2 + ZL2)
      V1VOL(2) = PI * R3*R3 * (ZU3 - ZU2)
      V1VOL(3) = PI * R3*R3 * (ZL3 - ZL2)

C *** Blanket

      BLVOL(1) = PI * (R4*R4 - R3*R3) * (ZU3 + ZL3)
      BLVOL(2) = PI * R4*R4 * (ZU4 - ZU3)
      BLVOL(3) = PI * R4*R4 * (ZL4 - ZL3)

C *** Second void

      V2VOL(1) = PI * (R5*R5 - R4*R4) * (ZU4 + ZL4)
      V2VOL(2) = PI * R5*R5 * (ZU5 - ZU4)
      V2VOL(3) = PI * R5*R5 * (ZL5 - ZL4)

C *** Shield

      SHVOL(1) = PI * (R6*R6 - R5*R5) * (ZU5 + ZL5)
      SHVOL(2) = PI * R6*R6 * (ZU6 - ZU5)
      SHVOL(3) = PI * R6*R6 * (ZL6 - ZL5)

C *** Third void

      V3VOL(1) = PI * (R7*R7 - R6*R6) * (ZU6 + ZL6)
      V3VOL(2) = PI * R7*R7 * (ZU7 - ZU6)
      V3VOL(3) = PI * R7*R7 * (ZL7 - ZL6)

C *** Material volumes

      DO 20 I = 0,MAXMAT
         CHMATV(I) = MAX(0.0D0, CHVOL * CHMATF(I))
         DO 10 J = 1,3
            FWMATV(J,I) = MAX(0.0D0, FWVOL(J) * FWMATF(J,I))
            V1MATV(J,I) = MAX(0.0D0, V1VOL(J) * V1MATF(J,I))
            BLMATV(J,I) = MAX(0.0D0, BLVOL(J) * BLMATF(J,I))
            V2MATV(J,I) = MAX(0.0D0, V2VOL(J) * V2MATF(J,I))
            SHMATV(J,I) = MAX(0.0D0, SHVOL(J) * SHMATF(J,I))
            V3MATV(J,I) = MAX(0.0D0, V3VOL(J) * V3MATF(J,I))
 10      CONTINUE
 20   CONTINUE

C *** First wall area

      FWAREA = 2.0D0*PI*R1 * ((ZU1 + ZL1) + R1)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE OSIBLD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to create the 'build' of an inertial fusion energy
C  device, based on the design of the OSIRIS study, and to calculate
C  the material volumes for the device core
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.56
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION PI
      PARAMETER (
     +     PI     = 3.141592653589793D0 )

C  INCLUDE files
      INCLUDE 'build.h'
      INCLUDE 'ife.h'

C  External routines
      EXTERNAL GENBLD

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Careful choice of thicknesses, and assuming that the FLiBe
C *** inlet radius is small, allows the generic build calculation
C *** to be roughly applicable.

      CALL GENBLD

C *** First wall area: no true first wall at bottom of chamber

      FWAREA = 2.0D0*PI*R1*(ZU1 + ZL1) + PI*R1*R1

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SOMBLD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to create the 'build' of an inertial fusion energy
C  device, based on the design of the SOMBRERO study, and to calculate
C  the material volumes for the device core
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  Sviatoslavsky et al, Fusion Technology vol.21 (1992) 1470
C  F/MI/PJK/LOGBOOK12, p.53
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION PI,THIRD
      PARAMETER (
     +     PI     = 3.141592653589793D0,
     +     THIRD  = 1.0D0/3.0D0 )

C  INCLUDE files
      INCLUDE 'build.h'
      INCLUDE 'ife.h'

C  Local variables
      DOUBLE PRECISION CHCYLH,DDZ,DVOL
      INTEGER I,J

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Radial build

      R1 = CHRAD
      R2 = R1 + FWDR
      R3 = R2 + V1DR
      R4 = R3 + BLDR
      R5 = R4 + V2DR
      R6 = R5 + SHDR
      R7 = R6 + V3DR

C *** Vertical build (below midplane)

      ZL1 = CHDZL
      ZL2 = ZL1 + FWDZL
      ZL3 = ZL2 + V1DZL
      ZL4 = ZL3 + BLDZL
      ZL5 = ZL4 + V2DZL
      ZL6 = ZL5 + SHDZL
      ZL7 = ZL6 + V3DZL

C *** Vertical build (above midplane)

      ZU1 = CHDZU
      ZU2 = ZU1 + FWDZU
      ZU3 = ZU2 + V1DZU
      ZU4 = ZU3 + BLDZU
      ZU5 = ZU4 + V2DZU
      ZU6 = ZU5 + SHDZU
      ZU7 = ZU6 + V3DZU

C *** The SOMBRERO chamber is made up of a cylindrical first wall/
C *** blanket, with conical regions above and below. Outside this is
C *** a cylindrical shield.

C *** Component volumes
C *** The following notation applies below:
C *** J=1 : side part
C *** J=2 : top part
C *** J=3 : bottom part

C *** Chamber
C *** CHCYLH is the height of the cylindrical part

      CHCYLH = CHDZU + CHDZL - 2.0D0*CHRAD

      CHVOL = PI * R1*R1 * (
     +     CHCYLH + 2.0D0*THIRD * CHRAD )

C *** First wall

      FWVOL(1) = PI * (R2*R2 - R1*R1) * CHCYLH
      FWVOL(2) = THIRD * PI * ( R2*R2*(CHRAD+FWDZU) - R1*R1*CHRAD )
      FWVOL(3) = THIRD * PI * ( R2*R2*(CHRAD+FWDZL) - R1*R1*CHRAD )

C *** First void

      V1VOL(1) = PI * (R3*R3 - R2*R2) * CHCYLH
      V1VOL(2) = THIRD * PI * ( R3*R3*(CHRAD+FWDZU+V1DZU) - 
     +     R2*R2*(CHRAD+FWDZU) )
      V1VOL(3) = THIRD * PI * ( R3*R3*(CHRAD+FWDZL+V1DZL) - 
     +     R2*R2*(CHRAD+FWDZL) )

C *** Blanket
C *** SOMTDR and SOMBDR are the radii of the cylindrical sections at the
C *** top/bottom of the blanket
C *** DDZ = Height of top cylindrical section (by similar triangles)
C *** DVOL = Volume of top cylindrical section, less the internal cone

      BLVOL(1) = PI * (R4*R4 - R3*R3) * CHCYLH

      BLVOL(2) = THIRD * PI * ( R4*R4*(CHRAD+FWDZU+V1DZU+BLDZU) - 
     +     R3*R3*(CHRAD+FWDZU+V1DZU) )
      DDZ = (CHRAD+FWDZU+V1DZU+BLDZU)/(CHRAD+FWDR+V1DR+BLDR)*SOMTDR
      DVOL = 2.0D0*THIRD * PI * SOMTDR*SOMTDR * DDZ

      BLVOL(2) = BLVOL(2) + DVOL

C *** Ditto for bottom region...

      BLVOL(3) = THIRD * PI * ( R4*R4*(CHRAD+FWDZL+V1DZL+BLDZL) - 
     +     R3*R3*(CHRAD+FWDZL+V1DZL) )
      DDZ = (CHRAD+FWDZL+V1DZL+BLDZL)/(CHRAD+FWDR+V1DR+BLDR)*SOMBDR
      DVOL = 2.0D0*THIRD * PI * SOMBDR*SOMBDR * DDZ

      BLVOL(3) = BLVOL(3) + DVOL

C *** Second void

      V2VOL(1) = PI * (R5*R5 - R4*R4) * CHCYLH
      V2VOL(2) = PI * R5*R5 * (ZU5 - CHDZU + CHRAD) - (
     +     FWVOL(2) + V1VOL(2) + BLVOL(2) + (THIRD*PI*R1*R1*CHRAD) )
      V2VOL(3) = PI * R5*R5 * (ZL5 - CHDZL + CHRAD) - (
     +     FWVOL(3) + V1VOL(3) + BLVOL(3) + (THIRD*PI*R1*R1*CHRAD) )

C *** Shield

      SHVOL(1) = PI * (R6*R6 - R5*R5) * (ZU6 + ZL6)
      SHVOL(2) = PI * R5*R5 * (ZU6 - ZU5)
      SHVOL(3) = PI * R5*R5 * (ZL6 - ZL5)

C *** Third void

      V3VOL(1) = PI * (R7*R7 - R6*R6) * (ZU7 + ZL7)
      V3VOL(2) = PI * R6*R6 * (ZU7 - ZU6)
      V3VOL(3) = PI * R6*R6 * (ZL7 - ZL6)

C *** Material volumes

      DO 20 I = 0,MAXMAT
         CHMATV(I) = MAX(0.0D0, CHVOL * CHMATF(I))
         DO 10 J = 1,3
            FWMATV(J,I) = MAX(0.0D0, FWVOL(J) * FWMATF(J,I))
            V1MATV(J,I) = MAX(0.0D0, V1VOL(J) * V1MATF(J,I))
            BLMATV(J,I) = MAX(0.0D0, BLVOL(J) * BLMATF(J,I))
            V2MATV(J,I) = MAX(0.0D0, V2VOL(J) * V2MATF(J,I))
            SHMATV(J,I) = MAX(0.0D0, SHVOL(J) * SHMATF(J,I))
            V3MATV(J,I) = MAX(0.0D0, V3VOL(J) * V3MATF(J,I))
 10      CONTINUE
 20   CONTINUE

C *** First wall area

      FWAREA = 2.0D0*PI*R1*( (ZU1 + ZL1) + R1*SQRT(2.0D0) )

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE HYLBLD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to create the 'build' of an inertial fusion energy
C  device, based on the design of the HYLIFE-II study, and to calculate
C  the material volumes for the device core
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  Moir, Fusion Technology vol.21 (1992) 1475
C  F/MI/PJK/LOGBOOK12, p.57
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION PI,THIRD
      PARAMETER (
     +     PI     = 3.141592653589793D0,
     +     THIRD  = 1.0D0/3.0D0 )

C  INCLUDE files
      INCLUDE 'build.h'
      INCLUDE 'ife.h'

C  Local variables
      DOUBLE PRECISION CHCYLH,DDZ,DVOL
      INTEGER I,J

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Radial build

      R1 = CHRAD
      R2 = R1 + FWDR
      R3 = R2 + V1DR
      R4 = R3 + BLDR
      R5 = R4 + V2DR
      R6 = R5 + SHDR
      R7 = R6 + V3DR

C *** Vertical build (below midplane)

      ZL1 = CHDZL
      ZL2 = ZL1 + FWDZL
      ZL3 = ZL2 + V1DZL
      ZL4 = ZL3 + BLDZL
      ZL5 = ZL4 + V2DZL
      ZL6 = ZL5 + SHDZL
      ZL7 = ZL6 + V3DZL

C *** Vertical build (above midplane)

      ZU1 = CHDZU
      ZU2 = ZU1 + FWDZU
      ZU3 = ZU2 + V1DZU
      ZU4 = ZU3 + BLDZU
      ZU5 = ZU4 + V2DZU
      ZU6 = ZU5 + SHDZU
      ZU7 = ZU6 + V3DZU

C *** The HYLIFE-II chamber is assumed to be mostly cylindrical, but
C *** with a conical region below the midplane that causes the Flibe
C *** to flow downwards and outwards towards the outlet.

C *** Component volumes
C *** The following notation applies below:
C *** J=1 : side part
C *** J=2 : top part
C *** J=3 : bottom part

C *** Chamber

      CHVOL = PI * R1*R1 * ( (ZU1 + ZL5) - THIRD * (ZL5 - ZL1) )

C *** First wall
C *** FLIRAD is the radius of the Flibe inlet

      FWVOL(1) = PI * (R2*R2 - R1*R1) * (ZU2 + ZL5)
      FWVOL(2) = PI * (R1*R1 - FLIRAD*FLIRAD) * (ZU2 - ZU1)
      FWVOL(3) = THIRD * PI * ( R2*R2*(ZL5-ZL1) - R1*R1*(ZL5-ZL2) )

C *** First void

      V1VOL(1) = PI * (R3*R3 - R2*R2) * (ZU2 + ZL3)
      V1VOL(2) = PI * (R4*R4 - FLIRAD*FLIRAD) * (ZU3 - ZU2)
      V1VOL(3) = THIRD * PI * R1*R1 * (ZL3 - ZL2)

C *** Blanket

      BLVOL(1) = PI * (R4*R4 - R3*R3) * (ZU2 + ZL3)
      BLVOL(2) = PI * (R4*R4 - FLIRAD*FLIRAD) * (ZU4 - ZU3)
      BLVOL(3) = PI * R4*R4 * (ZL4 - ZL3)

C *** Second void

      V2VOL(1) = PI * (R5*R5 - R4*R4) * (ZU4 + ZL4)
      V2VOL(2) = PI * (R5*R5 - FLIRAD*FLIRAD) * (ZU5 - ZU4)
      V2VOL(3) = PI * R5*R5 * (ZL5 - ZL4)

C *** Shield

      SHVOL(1) = PI * (R6*R6 - R5*R5) * (ZU5 + ZL5)
      SHVOL(2) = PI * R6*R6 * (ZU6 - ZU5)
      SHVOL(3) = PI * R6*R6 * (ZL6 - ZL5)

C *** Third void

      V3VOL(1) = PI * (R7*R7 - R6*R6) * (ZU6 + ZL6)
      V3VOL(2) = PI * R7*R7 * (ZU7 - ZU6)
      V3VOL(3) = PI * R7*R7 * (ZL7 - ZL6)

C *** Material volumes

      DO 20 I = 0,MAXMAT
         CHMATV(I) = MAX(0.0D0, CHVOL * CHMATF(I))
         DO 10 J = 1,3
            FWMATV(J,I) = MAX(0.0D0, FWVOL(J) * FWMATF(J,I))
            V1MATV(J,I) = MAX(0.0D0, V1VOL(J) * V1MATF(J,I))
            BLMATV(J,I) = MAX(0.0D0, BLVOL(J) * BLMATF(J,I))
            V2MATV(J,I) = MAX(0.0D0, V2VOL(J) * V2MATF(J,I))
            SHMATV(J,I) = MAX(0.0D0, SHVOL(J) * SHMATF(J,I))
            V3MATV(J,I) = MAX(0.0D0, V3VOL(J) * V3MATF(J,I))
 10      CONTINUE
 20   CONTINUE

C *** First wall area

      FWAREA = 2.0D0 * PI * R1 * (ZU1 + ZL5)
      FWAREA = FWAREA + PI * (R1*R1 - FLIRAD*FLIRAD)
      FWAREA = FWAREA + PI * R1 * SQRT(R1*R1 + (ZL3-ZL1)**2)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFESTR(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the support structural masses for the core of
C  an Inertial Fusion Energy power plant.
C  Output masses are all trivially zero...
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.87
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'struccom.h'

C  Arguments
      INTEGER NOUT,IPRINT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Set all outputs to zero, as they are Magnetic Fusion specific

      AINTMASS = 0.0D0
      CLGSMASS = 0.0D0
      COLDMASS = 0.0D0
      FNCMASS  = 0.0D0
      GSMASS   = 0.0D0

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFETGT(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the power requirements of the target
C  delivery system and the target factory
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, pp.87-88
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'ife.h'

C  Arguments
      INTEGER NOUT,IPRINT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Target delivery system power (MWe) - effectively negligible

      TDSPMW = 1.0D-2

C *** Target factory power (MWe)
C *** Assumed to scale with repetition rate (not quite linearly)

      TFACMW = PTARGF * (REPRAT/6.0D0)**0.7D0

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEFBS(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the first wall, blanket and shield volumes,
C  masses and other parameters, for an Inertial Fusion Energy device
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.86
C  Moir et al., Fusion Technology, vol.25 (1994) p.5
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'fwblsh.h'
      INCLUDE 'build.h'
      INCLUDE 'cost.h'
      INCLUDE 'pulse.h'
      INCLUDE 'phydat.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION DEN,MATDEN(0:MAXMAT),LIFE
      INTEGER I,J

C  External routines
      EXTERNAL OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 70

C *** Material densities
C *** 0 = void
C *** 1 = steel
C *** 2 = carbon
C *** 3 = FLiBe (inferred from Moir et al)
C *** 4 = Li2O
C *** 5 = concrete
C *** 6 = helium (at typical coolant temperatures)
C *** 7 = xenon (taken as ten times the normal tabulated value)

      MATDEN(0) = 0.0D0
      MATDEN(1) = DENSTL
      MATDEN(2) = 2300.0D0
      MATDEN(3) = 2020.0D0
      MATDEN(4) = 2010.0D0
      MATDEN(5) = 2400.0D0
      MATDEN(6) = 1.517D0
      MATDEN(7) = 55.0D0

C *** Material masses

      DO 20 I = 0,MAXMAT
         DEN = MATDEN(I)
         CHMATM(I) = CHMATV(I) * DEN
         DO 10 J = 1,3
            FWMATM(J,I) = FWMATV(J,I) * DEN
            V1MATM(J,I) = V1MATV(J,I) * DEN
            BLMATM(J,I) = BLMATV(J,I) * DEN
            V2MATM(J,I) = V2MATV(J,I) * DEN
            SHMATM(J,I) = SHMATV(J,I) * DEN
            V3MATM(J,I) = V3MATV(J,I) * DEN
 10      CONTINUE
 20   CONTINUE

C *** Total masses of components (excluding coolant)

      FWMASS = 0.0D0
      WHTBLKT = 0.0D0
      WHTSHLD = 0.0D0
      DO 40 I = 1,5
         DO 30 J = 1,3
            FWMASS = FWMASS + FWMATM(J,I)
            WHTBLKT = WHTBLKT + BLMATM(J,I)
            WHTSHLD = WHTSHLD + SHMATM(J,I)
 30      CONTINUE
 40   CONTINUE

C *** Other masses

      WHTBLBE = 0.0D0
      WHTBLVD = 0.0D0
      WHTBLSS = 0.0D0
      WTBLLI2O = 0.0D0
      DO 50 J = 1,3
         WHTBLSS = WHTBLSS + BLMATM(J,1)
         WTBLLI2O = WTBLLI2O + BLMATM(J,4)
 50   CONTINUE

C *** Total mass of FLiBe

      MFLIBE = CHMATM(3)
      DO 60 J = 1,3
         MFLIBE = MFLIBE + FWMATM(J,3) + V1MATM(J,3) + BLMATM(J,3) +
     +        V2MATM(J,3) + SHMATM(J,3) + V3MATM(J,3)
 60   CONTINUE

C *** A fraction FBREED of the total breeder inventory is outside the
C *** core region, i.e. is in the rest of the heat transport system

      IF ((FBREED.LT.0.0D0).OR.(FBREED.GT.0.999D0)) THEN
         WRITE(NOUT,*) 'Error in routine IFEFBS:'
         WRITE(NOUT,*) 'FBREED = ',FBREED
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

C *** Following assumes that use of FLiBe and Li2O are
C *** mutually exclusive

      MFLIBE = MFLIBE / (1.0D0 - FBREED)
      WTBLLI2O = WTBLLI2O / (1.0D0 - FBREED)

C *** Blanket and first wall lifetimes (HYLIFE-II: = plant life)

      IF (IFETYP.EQ.3) THEN
         LIFE = TLIFE
      ELSE
         LIFE = MIN( TLIFE, ABKTFLNC/(WALLMW*CFACTR) )
      END IF

      BKTLIFE = LIFE
      FWLIFE = LIFE

C *** Cryostat mass (=zero)

      CRYOMASS = 0.0D0

 70   CONTINUE

C *** Output section

      IF ((IPRINT.EQ.0).OR.(SECT12.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'First Wall, Blanket, Shield')

      CALL OVARRE(NOUT,'First wall area (m2)','(fwarea)',FWAREA)
      CALL OVARRE(NOUT,'First wall mass (kg)','(fwmass)',FWMASS)
      CALL OVARRE(NOUT,'Blanket mass (kg)','(whtblkt)',WHTBLKT)
      CALL OVARRE(NOUT,'Total mass of FLiBe (kg)','(mflibe)',MFLIBE)
      CALL OVARRE(NOUT,'Shield mass (kg)','(whtshld)',WHTSHLD)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEPW1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calculates the first part of the heat transport
C  and plant power balance constituents, for an IFE power plant
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, pp.67,89
C  Bourque et al., Fusion Technology vol.21 (1992) 1465
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'fwblsh.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'phydat.h'
      INCLUDE 'ife.h'

C  Local variables
      DOUBLE PRECISION PDRVMW

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Driver power reaching target (MW)

      PDRVMW = 1.0D-6 * PDRIVE

C *** Primary nuclear heating (MW)

      PRIHEAT = EMULT * POWFMW

C *** Useful (high-grade) thermal power (MW)

      PTHERMMW = PRIHEAT * (1.0D0-FHOLE)

C *** Assume 0.24 of thermal power is intercepted by the first wall
C *** (Bourque et al)
C *** HYLIFE-II case: Assume FLiBe flows intercept all fusion power
C *** and provide the energy multiplication as though it were a
C *** conventional blanket

      IF (IFETYP.NE.3) THEN
         PFWDIV = 0.24D0 * PTHERMMW
         PNUCBLKT = PTHERMMW - PFWDIV
      ELSE
         PFWDIV = 0.0D0
         PNUCBLKT = PTHERMMW
      END IF
      PNUCSHLD = 0.0D0

C *** Lost fusion power (MW)

      PNUCLOSS = PRIHEAT - PTHERMMW

C *** Number of primary heat exchangers

      RNPHX = MAX(2.0D0, (PRIHEAT/400.0D0 + 0.8D0) )

C *** Secondary heat (some of it... rest calculated in IFEPW2)

C *** Wall plug driver power (MW)

      PINJWP = PDRVMW/ETADRV

C *** Waste driver power (MW)

      PINJHT = PINJWP - PDRVMW

C *** Cryogenic power (MW)
C *** Cryogenic temperature is assumed to be 4.5K

      CRYPMW = PIFECR
      HELPOW = 1.0D6 * CRYPMW * (0.13D0 * 4.5D0)/(293.0D0 - 4.5D0)

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEACP(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate AC power requirements for an IFE power plant.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.68
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'htpwr.h'
      INCLUDE 'osections.h'
      INCLUDE 'ife.h'

C Arguments
      INTEGER IPRINT,NOUT

C  Local variables
      DOUBLE PRECISION BASEMW,PMWPM2

C  External routines
      EXTERNAL OBLNKL,OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 10

C *** Facility base load, MW (loads not dependent on floor area)

      BASEMW = BASEEL * 1.0D-6

C *** Power needed per floor area, MW/m2

      PMWPM2 = PWPM2 * 1.0D-6

C *** Total pulsed power system load, MW

      PACPMW = FMGDMW + CRYPMW + VACHTMW + TDSPMW + TFACMW +
     +     (HTPMW*REPRAT/6.0D0) + TRITHTMW + PINJWP + BASEMW +
     +     (EFLOOR*PMWPM2)

C *** Total power to facility loads, MW

      FCSHT  = BASEMW + (EFLOOR*PMWPM2) + 0.05D0*PACPMW

C *** Estimate of the total low voltage power, MW

      TLVPMW = FCSHT + TRITHTMW + (HTPMW*REPRAT/6.0D0) + VACHTMW +
     +     0.5D0*CRYPMW + TFACMW

 10   CONTINUE

C *** Output section

      IF ((IPRINT.EQ.0).OR.(SECT17.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'AC Power')

      CALL OVARRE(NOUT,'Facility base load (MW)','(basemw)',BASEMW)
      CALL OVARRE(NOUT,'Total floor space (m2)','(efloor)',EFLOOR)
      CALL OVARRE(NOUT,'Power/floor area (MW/m2)','(pmwpm2)',PMWPM2)
      CALL OVARRE(NOUT,'MGF units (MW)','(fmgdmw)',FMGDMW)
      CALL OVARRE(NOUT,'Driver power supplies (MW)','(pinjwp)',
     +     PINJWP)
      CALL OVARRE(NOUT,'Target delivery system (MW)','(tdspmw)',
     +     TDSPMW)
      CALL OVARRE(NOUT,'Target factory (MW)','(tfacmw)',
     +     TFACMW)
      CALL OVARRE(NOUT,'Tritium processing plant (MW)','(trithtmw)',
     +     TRITHTMW)
      CALL OVARRE(NOUT,'Vacuum pump motors (MW)','(vachtmw)',VACHTMW)
      CALL OVARRE(NOUT,'Cryogenic comp motors (MW)','(crypmw)',CRYPMW)
      CALL OVARRE(NOUT,'Heat transport system pump motors (MW)',
     +     '(htpmw*reprat/6)',HTPMW*REPRAT/6.0D0)
      CALL OBLNKL(NOUT)
      CALL OVARRE(NOUT,'Total pulsed power (MW)','(pacpmw)',PACPMW)
      CALL OVARRE(NOUT,'Total facility power (MW)','(fcsht)',FCSHT)
      CALL OVARRE(NOUT,'Total low voltage power (MW)','(tlvpmw)',TLVPMW)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEPW2(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calculates the rest of the heat transport
C  and plant power balance constituents, not already calculated in
C  IFEPW1 or IFEACP.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.67
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'cost.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'osections.h'
      INCLUDE 'phydat.h'
      INCLUDE 'ife.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION PRECIR

C  External routines
      EXTERNAL OBLNKL,OHEADR,OSUBHD,OVARRE,OVARRF

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 10

C *** Facility heat removal (fcsht calculated in IFEACP)

      FACHT = FCSHT

C *** Total secondary heat

      PSECHT = PINJHT + PNUCLOSS + FACHT + VACHTMW + TRITHTMW +
     +     TDSPMW + TFACMW + CRYPMW + HTPMW

C *** Total plant heat removal

      CTHT = PRIHEAT + PSECHT 

C *** Number of intermediate heat exchangers

      RNIHX = MAX(2.0D0, (CTHT/50.0D0 + 0.8D0) )

C *** Calculate powers relevant to a power-producing plant

      IF (IREACTOR.EQ.1) THEN

C *** Gross electric power

         PGROSSMW = PTHERMMW * ETATH

C *** Balance of plant recirculating power fraction

         FGROSBOP = MIN( 0.5D0, ( FAUXBOP/(PGROSSMW/1000.0D0)**0.6D0) )

C *** Total recirculating power

         PRECIR = (FGROSBOP*PGROSSMW) + PACPMW

C *** Net electric power

         PNETELMW = PGROSSMW - PRECIR

C *** Scaling to prevent negative pnetelmw

         IF ( (PNETELMW.LT.1.0D0).AND.(IPNET.EQ.0) ) THEN
            PNETELMW = 1.0D0 / ( 1.0D0 + ABS(PNETELMW-1.0D0))
         END IF

      END IF

 10   CONTINUE

C *** Output section

      IF ((IPRINT.EQ.0).OR.(SECT14.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'Power / Heat Transport')
      CALL OVARRE(NOUT,'Fusion power (MW)','(powfmw)',POWFMW)
      CALL OVARRE(NOUT,'Fusion power escaping via holes (MW)',
     +     '(pnucloss)',PNUCLOSS)
      CALL OVARRE(NOUT,'Power multiplication factor','(emult)',EMULT)
      CALL OVARRE(NOUT,'Driver wall plug power (MW)','(pinjwp)'
     +     ,PINJWP)
      CALL OVARRE(NOUT,'First wall nuclear heating (MW)','(pfwdiv)',
     +     PFWDIV)
      CALL OVARRE(NOUT,'Blanket nuclear heating (MW)','(pnucblkt)',
     +     PNUCBLKT)
      CALL OVARRE(NOUT,'Primary heat (MW)','(pthermmw)',PTHERMMW)
      CALL OVARRE(NOUT,'Secondary heat (MW)','(psecht)',PSECHT)
      CALL OBLNKL(NOUT)
      CALL OVARRE(NOUT,'Heat removal from driver power (MW)',
     +     '(pinjht)',PINJHT)
      CALL OVARRE(NOUT,'Heat removal from cryogenic plant (MW)',
     +     '(crypmw)',CRYPMW)
      CALL OVARRE(NOUT,'Heat removal from vacuum pumps (MW)',
     +     '(vachtmw)',VACHTMW)
      CALL OVARRE(NOUT,'Heat removal from target factory (MW)',
     +     '(tfacmw)',TFACMW)
      CALL OVARRE(NOUT,'Heat removal from delivery system (MW)',
     +     '(tdspmw)',TDSPMW)
      CALL OVARRE(NOUT,'Heat removal from tritium plant (MW)',
     +     '(trithtmw)',TRITHTMW)
      CALL OVARRE(NOUT,'Heat removal from facilities (MW)','(facht)'
     +     ,FACHT)
      CALL OVARRF(NOUT,'Number of primary heat exchangers','(rnphx)'
     +     ,RNPHX)
      CALL OVARRF(NOUT,'Number of intermediate heat exchangers',
     +     '(rnihx)',RNIHX)
      CALL OVARRE(NOUT,'Total plant heat rejection (MW)','(ctht)',CTHT)

      IF (IREACTOR.NE.1) GOTO 1000

      CALL OSUBHD(NOUT,'Reactor powers :')
      CALL OVARRE(NOUT,'Gross electric power (MW)','(pgrossmw)'
     +     ,PGROSSMW)
      CALL OVARRE(NOUT,'Net electric power (MW)','(pnetelmw)',PNETELMW)
      CALL OVARRE(NOUT,'Balance of plant aux. power fraction',
     +     '(fgrosbop)',FGROSBOP)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEVAC(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate parameters of the vacuum system for an
C  Inertial Fusion Energy power plant
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.87
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'vaccom.h'
      INCLUDE 'torsdat.h'

C  Arguments
      INTEGER NOUT,IPRINT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Values are hard-wired in; based loosely on those for a tokamak
C *** of 6m major radius

      DLSCAL = 2.0D0
      NVDUCT = 16
      VACDSHM = 0.0D0
      VCDIMAX = 0.3D0
      VPUMPN = 32.0D0

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE IFEBDG(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the volumes of the buildings required for
C  an Inertial Fusion Energy power plant. The method is based
C  closely on that for tokamaks etc. in routine BLDGS
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.87
C  
C--History
C  21/03/97 PJK 1.000 Initial version
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
      INCLUDE 'bldgcom.h'
      INCLUDE 'bldgvol.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'ife.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION CRAN,CRYV,DCL,DCW,ELEV,FAC2,FAC3,HCL,HCW,HRBI,
     +     RBH,RBL,RBV,RBW,RMBH,RMBL,RMBV,RMBW,RWL,RWW,SHH,TCH,TCL,
     +     TCW,VRCI,WGTS,WSA,WSV

C  External routines
      EXTERNAL OHEADR,OVARRE

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (IPRINT.EQ.1) GOTO 10

C *** Reactor building
C *** ================

C *** Total internal height

      HRBI = ZL7 + ZU7

C *** Distance from centre of device to wall

      WRBI = R7

C *** Internal volume (square floor)

      VRCI = (2.0D0 * WRBI)**2 * HRBI

C *** External dimensions
C *** RBWT = wall thickness
C *** RBRT = roof thickness
C *** FNDT = foundation thickness

      RBW = 2.0D0 * (R7 + RBWT)
      RBL = RBW
      RBH = HRBI + RBRT + FNDT

C *** External volume

      RBV = RBW * RBL * RBH

C *** Maintenance building
C *** ====================

C *** The reactor maintenance building includes the hot cells, the
C *** decontamination chamber, the transfer corridors, and the waste
C *** treatment building.  The dimensions of these areas are scaled
C *** from a reference (tokamak) design based on the shield sector size.

C *** Shield height

      SHH = ZL6 + ZU6

C *** Transport corridor size

      TCW = R6 + 4.0D0*TRCL
      TCL = 5.0D0*TCW + 2.0D0*HCWT

C *** Decontamination cell size

      DCW = 2.0D0*TCW + 1.0D0
      DCL = 2.0D0*TCW + 1.0D0

C *** Hot cell size

      HCW = R6 + 3.0D0*HCCL + 2.0D0
      HCL = 3.0D0*R6 + 4.0D0*HCCL + TCW

C *** Radioactive waste treatment

      RWW = DCW
      RWL = HCL-DCL-HCWT

C *** Maintenance building dimensions

      RMBW = HCW + DCW + 3.0D0*HCWT
      RMBL = HCL + 2.0D0*HCWT

C *** Height

      IF (WGT2.GT.1.0D0) THEN
         WGTS = WGT2
      ELSE
         WGTS = WHTSHLD
      END IF
      CRAN = 9.41D-6*WGTS + 5.1D0
      RMBH = 10.0D0 + (ZL6+ZU6) + TRCL + CRAN + 5.1D0 + STCL + FNDT
      TCH = SHH + STCL + FNDT

C *** Volume

      FAC2 = 2.8D0
      RMBV = FAC2*RMBW*RMBL*RMBH + TCW*TCL*TCH

C *** Warm shop and hot cell gallery

      WSA = (RMBW+7.0D0)*20.0D0 + RMBL*7.0D0
      FAC3 = 1.9D0
      WSV = FAC3 * WSA*RMBH

C *** Cryogenic building volume

      CRYV = 55.0D0 * SQRT(HELPOW)

C *** Electrical building volume
C *** (set equal to power injection (i.e. driver) building volume)

      ELEV = PIBV

C *** Calculate effective floor area for ac power module

      EFLOOR = (RBV+RMBV+WSV+TRIV+ELEV+CONV+CRYV+ADMV+SHOV)/6.0D0

C *** Convert local into global variables

      ADMVOL = ADMV
      CONVOL = CONV
      ELEVOL = ELEV
      RBVOL  = RBV
      RMBVOL = RMBV
      SHOVOL = SHOV
      VOLRCI = VRCI
      WSVOL  = WSV

C *** Total volume of nuclear buildings

      VOLNUCB = ( VRCI + RMBV + WSV + TRIV + CRYV )

 10   CONTINUE

C *** Output section

      IF ((IPRINT.EQ.0).OR.(SECT16.EQ.0)) GOTO 1000

      CALL OHEADR(NOUT,'Plant Buildings System')
      CALL OVARRE(NOUT,'Internal volume of reactor building (m3)',
     +     '(vrci)',VRCI)
      CALL OVARRE(NOUT,'Dist from device centre to bldg wall (m)',
     +     '(wrbi)',WRBI)
      CALL OVARRE(NOUT,'Effective floor area (m2)','(efloor)',EFLOOR)
      CALL OVARRE(NOUT,'Reactor building volume (m3)','(rbv)',RBV)
      CALL OVARRE(NOUT,'Reactor maintenance building volume (m3)',
     +     '(rmbv)',RMBV)
      CALL OVARRE(NOUT,'Warmshop volume (m3)','(wsv)',WSV)
      CALL OVARRE(NOUT,'Tritium building volume (m3)','(triv)',TRIV)
      CALL OVARRE(NOUT,'Electrical building volume (m3)','(elev)',ELEV)
      CALL OVARRE(NOUT,'Control building volume (m3)','(conv)',CONV)
      CALL OVARRE(NOUT,'Cryogenics building volume (m3)','(cryv)',CRYV)
      CALL OVARRE(NOUT,'Administration building volume (m3)','(admv)',
     +     ADMV)
      CALL OVARRE(NOUT,'Shops volume (m3)','(shov)',SHOV)
      CALL OVARRE(NOUT,'Total volume of nuclear buildings (m3)',
     +     '(volnucb)',VOLNUCB)

 1000 CONTINUE

      RETURN
      END
