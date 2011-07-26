C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: heatpwr.f,v 3.12 2004/06/16 08:51:50 pknight Exp pknight $
C  Module name    : $RCSfile: heatpwr.f,v $
C  Version no.    : $Revision: 3.12 $
C  Creation date  : $Date: 2004/06/16 08:51:50 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE POWER1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  This subroutine calculates the first part of the heat transport
C  and plant power balance constituents.
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.6368
C
C--Date
C  15 June 2004
C
C--Reference
C  None
C  
C--History
C  23/01/97 PJK 1.000 Initial upgraded version. heattr.h and heatrinp.h
C                     subsumed into htpwr.h. Split routine POWER into
C                     this routine (POWER1) and POWER2, to overcome
C                     problem with the use of variable efloor.
C  15/06/04 PJK 1.100 Added use of IPRIMHTP
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
      INCLUDE 'blanket.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'struccom.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'times.h'

C  External routines
      EXTERNAL CRYO

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Primary nuclear heating

      PFWDIV = PFUSCMW + 1.0D-6 * (PINJE + PINJI)
      PTHERMMW = PNUCBLKT + PNUCSHLD + (1.0D0-FFWLG)*PFWDIV
      PRIHEAT = PNUCBLKT + PNUCSHLD + PFWDIV

      IF (IPRIMHTP.EQ.1) THEN
         PTHERMMW = PTHERMMW + HTPMW
         PRIHEAT = PRIHEAT + HTPMW
      END IF

C *** Number of primary heat exchangers

      RNPHX = MAX(2.0D0, (PRIHEAT/400.0D0 + 0.8D0) )

C+**CAG 20/05/93 For the Rankine cycle employed by the
C+**CAG 20/05/93 new blanket model, the number of primary
C+**CAG 20/05/93 heat exchangers is two.

      IF (LBLNKT.EQ.1) RNPHX = 2.0D0

C *** Secondary heat (some of it... rest calculated in POWER2)

C *** Wall plug injection power

      PINJWP = 1.0D-6 * (ECHPWR/ETAECH + PLHYBD/ETALH + PNBEAM/ETANBI +
     +     POFCD/ETAOF)

C *** Waste injection power 

      PINJHT = PINJWP - 1.0D-6*(PINJI + PINJE)

C *** Cryogenic power

      IF ((ITFSUP.NE.1).AND.(IPFRES.EQ.1)) THEN
         HELPOW = 0.0D0
      ELSE
         CALL CRYO(ITFSUP,IPFRES,TFSAI,COLDMASS,PTFNUC,ENSXPFM,
     +        TPULSE,CPTTF,TFNO,HELPOW)
      END IF

C *** Use 13% of ideal Carnot efficiency to fit J. Miller estimate

      CRYPMW = 1.0D-6 * (293.0D0 - TMPCRY)/(0.13D0*TMPCRY) * HELPOW

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE POWER2(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.300
C
C--Description
C  This subroutine calculates the rest of the heat transport
C  and plant power balance constituents, not already calculated in
C  ACPOW or POWER1.
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.6368
C
C--Date
C  22 May 2007
C
C--Reference
C  None
C  
C--History
C  23/01/97 PJK 1.000 Initial version
C  10/09/97 PJK 1.100 Removed IF-statement that bypassed coding if
C                     iprint=1
C  15/06/04 PJK 1.200 Added use of IPRIMHTP, added HTPMW to PRECIR
C  22/05/07 PJK 1.300 Added hydrogen plant power requirements
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
      INCLUDE 'blanket.h'
      INCLUDE 'cost.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'osections.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION PPUMPMW,PRECIR

C  External routines
      EXTERNAL BLANKET,OBLNKL,OHEADR,OSUBHD,OVARRE,OVARRF

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Centrepost coolant pump power

      PPUMPMW = PPUMP / 1.0D6

C *** Facility heat removal (fcsht calculated in ACPOW)

      FACHT = FCSHT

C *** Hydrogen plant powers

      IF (IHPLANT.EQ.0) THEN
         HELECMW = 0.0D0
         HTHERMMW = 0.0D0
         HPOWER = 0.0D0
      ELSE IF (IHPLANT.EQ.1) THEN
         HTHERMMW = 0.0D0
         HPOWER = ETAHLTE * HELECMW
      ELSE IF (IHPLANT.EQ.2) THEN
         HTHERMMW = 0.48D0 * HELECMW
         HPOWER = ETAHHTEN * HELECMW
      ELSE IF (IHPLANT.EQ.3) THEN
         HTHERMMW = 0.19D0 * HELECMW
         HPOWER = ETAHHTEX * HELECMW
      ELSE
         HELECMW = 0.0D0
         HPOWER = ETAHTH * HTHERMMW
      END IF

C *** Total secondary heat

      PSECHT = PINJHT + PNUCLOSS + FACHT + VACHTMW + TRITHTMW +
     +     TFCMW + CRYPMW + PPUMPMW + HELECMW + HTHERMMW

      IF (IPRIMHTP.EQ.0) PSECHT = PSECHT + HTPMW

C *** Total plant heat removal

      CTHT = PRIHEAT + PSECHT 

C *** Number of intermediate heat exchangers

      RNIHX = MAX(2.0D0, (CTHT/50.0D0 + 0.8D0) )

C+**CAG 20/05/93 For the Rankine cycle employed by the
C+**CAG 20/05/93 new blanket model, the number of intermediate
C+**CAG 20/05/93 heat exchangers is set to equal the number
C+**CAG 20/05/93 of feed water heater pumps.

      IF (LBLNKT.EQ.1) RNIHX = DBLE(NIPFWH+NLPFWH)

C *** Calculate powers relevant to a power-producing plant

      IF (IREACTOR.EQ.1) THEN

C *** Gross electric power

         IF (LBLNKT.EQ.1) THEN
            CALL BLANKET(2,NOUT,IPRINT)
         ELSE
            PGROSSMW = (PTHERMMW-HTHERMMW) * ETATH
         END IF

C *** Balance of plant recirculating power

         FGROSBOP = MIN( 0.5D0, ( FAUXBOP/(PGROSSMW/1000.0D0)**0.6D0) )

C *** Total recirculating power

         PRECIR = FGROSBOP * PGROSSMW + PINJWP + TFCMW + CRYPMW +
     +        PPUMPMW + HTPMW + HELECMW

C *** Net electric power

         PNETELMW = PGROSSMW - PRECIR

C *** Scaling to prevent negative pnetelmw

         IF ( (PNETELMW.LT.1.0D0).AND.(IPNET.EQ.0) ) THEN
            PNETELMW = 1.0D0 / ( 1.0D0 + ABS(PNETELMW-1.0D0))
         END IF

      END IF

      IF ((IPRINT.EQ.0).OR.(SECT14.EQ.0)) GOTO 1000

C *** Output section

      CALL OHEADR(NOUT,'Power / Heat Transport')
      CALL OVARRE(NOUT,'Fusion power (MW)','(powfmw)',POWFMW)
      CALL OVARRE(NOUT,'Charged fusion power (MW)','(pfuscmw)',PFUSCMW)
      CALL OVARRE(NOUT,'Neutron power escaping via holes (MW)',
     +     '(pnucloss)',PNUCLOSS)
      CALL OVARRE(NOUT,'Neutron power multiplication','(emult)',EMULT)
      CALL OVARRE(NOUT,'Injector wall plug power (MW)','(pinjwp)'
     +     ,PINJWP)
      CALL OVARRE(NOUT,'TF coil resistive power (MW)','(tfcmw)',TFCMW)
      CALL OVARRE(NOUT,'Centrepost coolant pump power (MW)','(ppumpmw)'
     +     ,PPUMPMW)
      CALL OVARRE(NOUT,'Primary heat (MW)','(pthermmw)',PTHERMMW)
      CALL OVARRE(NOUT,'Secondary heat (MW)','(psecht)',PSECHT)
      CALL OBLNKL(NOUT)
      CALL OVARRE(NOUT,'Heat removal from F.W./divertor (MW)',
     +     '(pfwdiv)',PFWDIV)
      CALL OVARRE(NOUT,'Heat removal from blankets (MW)',
     +     '(pnucblkt)',PNUCBLKT)
      CALL OVARRE(NOUT,'Heat removal from shield (MW)','(pnucshld)',
     +     PNUCSHLD)
      CALL OVARRE(NOUT,'Heat removal from injection power (MW)',
     +     '(pinjht)',PINJHT)
      CALL OVARRE(NOUT,'Heat removal from cryogenic plant (MW)',
     +     '(crypmw)',CRYPMW)
      CALL OVARRE(NOUT,'Heat removal from vacuum pumps (MW)',
     +     '(vachtmw)',VACHTMW)
      CALL OVARRE(NOUT,'Heat removal from tritium plant (MW)',
     +     '(trithtmw)',TRITHTMW)

      IF (IHPLANT.NE.0) THEN
         CALL OVARRE(NOUT,'Electrical pwr used for H production (MW)',
     +        '(helecmw)',HELECMW)
         CALL OVARRE(NOUT,'Thermal power used for H production (MW)',
     +        '(hthermmw)',HTHERMMW)
         CALL OVARRE(NOUT,'Hydrogen production rate (MW)',
     +        '(hpower)',HPOWER)
         CALL OVARRE(NOUT,'Hydrogen production rate (Nm3/sec)',
     +        '(hpower/13)',HPOWER/13.0D0)
      END IF

      CALL OVARRE(NOUT,'Total cryogenic load (MW)','(helpow/1.D6)'
     +     ,HELPOW/1.0D6)
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
      CALL OVARRE(NOUT,'First wall low grade heat fraction','(ffwlg)'
     +     ,FFWLG)

 1000 CONTINUE

      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE CRYO(ITFSUP,IPFRES,TFSAI,COLDMASS,PTFNUC,ENSXPFM,
     +     TPULSE,CPTTF,TFNO,HELPOW)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Subroutine to calculate cryogenic loads (in Watts)
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  23 January 1997
C
C--Reference
C  From D Slack memo SCMDG 88-5-1-059, LLNL ITER-88-054, Aug. 1988
C  
C--History
C  23/01/97 PJK 1.000 Initial upgraded version
C
C--Arguments
C  ITFSUP   : (INPUT)  Switch denoting whether TF coils are
C                      superconducting
C  IPFRES   : (INPUT)  Switch denoting whether PF coils are resistive
C  TFSAI    : (INPUT)  Inboard TF coil surface area (m2)
C  COLDMASS : (INPUT)  Mass of cold (cryogenic) components (kg),
C                      including TF coils, PF coils, cryostat, and
C                      intercoil structure
C  PTFNUC   : (INPUT)  Nuclear heating in TF coils (MW)
C  ENSXPFM  : (INPUT)  Maximum PF coil stored energy (MJ)
C  TPULSE   : (INPUT)  Pulse length of cycle (s)
C  CPTTF    : (INPUT)  Current per turn in TF coils (A)
C  TFNO     : (INPUT)  Number of TF coils
C  HELPOW   : (OUTPUT) Helium heat removal at cryo temperatures (W)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION COLDMASS,CPTTF,ENSXPFM,HELPOW,PTFNUC,TFNO,TFSAI,
     +     TPULSE
      INTEGER ITFSUP,IPFRES

C  Local variables
      DOUBLE PRECISION QAC,QCL,QNUC,QSS

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Steady state loads (W)

      QSS = 4.3D-4 * COLDMASS
      IF (ITFSUP.EQ.1) QSS = QSS + 2.0D0 * TFSAI

C *** Nuclear heating of TF coils (W) (zero if resistive)

      QNUC = 1.0D6 * PTFNUC

C *** AC losses

      QAC = 1.0D3 * ENSXPFM / TPULSE

C *** Current leads

      IF (ITFSUP.EQ.1) THEN
         QCL = 13.6D-3 * TFNO * CPTTF
      ELSE
         QCL = 0.0D0
      END IF

C *** Total includes 45% extra miscellaneous, piping and reserves

      HELPOW = MAX(0.0D0, (1.45D0 * (QSS + QNUC + QAC + QCL)) )

      RETURN
      END
