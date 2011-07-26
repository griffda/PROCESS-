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
C  Module name    : $RCSfile: outplas.f,v $
C  Version no.    : $Revision: 3.13 $
C
C  Creation date  : $Date: 2001/07/16 08:05:00 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      SUBROUTINE OUTPLAS(NOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.050
C
C--Description
C  Subroutine to output the plasma physics information
C
C--Author
C  Peter Knight D3/162a Culham, ext.4181
C
C--Date
C  16 July 2001
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  17/09/97 PJK 1.000 Upgrade to higher standard of coding. Added
C                     Greenwald density limit
C  17/11/97 PJK 1.010 Added additional beta diagnostics
C  01/04/98 PJK 1.020 Added dnla to output, and comment about ignition
C  17/07/98 PJK 1.030 Added power threshold scalings
C  19/01/99 PJK 1.040 Added powerht and minor word changes
C  16/07/01 PJK 1.050 Added kappaa
C
C--Arguments
C  NOUT     : (INPUT)  Output file channel
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'ineq.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'labels.h'
      INCLUDE 'times.h'
      INCLUDE 'osections.h'
      INCLUDE 'rfp.h'

C  Arguments
      INTEGER NOUT

C  Local variables
      DOUBLE PRECISION BETATH

C  External routines
      EXTERNAL OBLNKL,OCMMNT,OHEADR,OSUBHD,OVARRE,OVARRF

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (SECT03.EQ.0) GOTO 1000

      CALL OHEADR(NOUT,'Plasma')

      IF (IDIVRT.EQ.0) THEN
         CALL OCMMNT(NOUT,'Plasma configuration = limiter')
         CALL OBLNKL(NOUT)
      ELSE IF (IDIVRT.EQ.1) THEN
         CALL OCMMNT(NOUT,'Plasma configuration = single null divertor')
         CALL OBLNKL(NOUT)
      ELSE IF(IDIVRT.EQ.2) THEN
         CALL OCMMNT(NOUT,'Plasma configuration = double null divertor')
         CALL OBLNKL(NOUT)
      ELSE
         WRITE(NOUT,*) 'Error in routine OUTPLAS:'
         write(NOUT,*) 'Illegal value of idivrt, = ',IDIVRT
         WRITE(NOUT,*) 'PROCESS stopping.'
         STOP
      END IF

C+**PJK 08/12/95
      IF (IDHE3.EQ.0) THEN
         IF (IITER.EQ.1) THEN
            CALL OCMMNT(NOUT,
     +           'Deuterium - Tritium fusion reaction assumed')
            CALL OBLNKL(NOUT)
            CALL OCMMNT(NOUT,
     +           'ITER fusion power calculations were performed')
         END IF
      ELSE
         CALL OCMMNT(NOUT,
     +        'Deuterium - Helium3 fusion reaction assumed')
      END IF

      CALL OSUBHD(NOUT,'Plasma Geometry :')
      CALL OVARRF(NOUT,'Major radius (m)','(rmajor)',RMAJOR)
      CALL OVARRF(NOUT,'Minor radius (m)','(rminor)',RMINOR)
      CALL OVARRF(NOUT,'Aspect ratio','(aspect)',ASPECT)
      CALL OVARRF(NOUT,'Elongation, X-point','(kappa)',KAPPA)
      CALL OVARRF(NOUT,'Elongation, 95% surface','(kappa95)',KAPPA95)
      CALL OVARRF(NOUT,'Elongation, area ratio calc.','(kappaa)',KAPPAA)
      CALL OVARRF(NOUT,'Triangularity, X-point','(triang)',TRIANG)
      CALL OVARRF(NOUT,'Triangularity, 95% surface','(triang95)'
     +     ,TRIANG95)
      CALL OVARRE(NOUT,'Plasma surface area (m2)','(sarea)',SAREA)
      CALL OVARRE(NOUT,'Plasma volume (m3)','(vol)',VOL)

      CALL OSUBHD(NOUT,'Current and Field :')
      CALL OVARRF(NOUT,'Plasma current (MA)','(plascur/1D6)'
     +     ,PLASCUR/1.0D6)
      IF (IRFP.EQ.0) THEN
         CALL OVARRF(NOUT,'Vacuum toroidal field at R (T)','(bt)',BT)
         CALL OVARRF(NOUT,'Average poloidal field (T)','(bp)',BP)
      ELSE
         CALL OVARRF(NOUT,'Toroidal field at plasma edge (T)',
     +        '(-bt)',-BT)
         CALL OVARRF(NOUT,'Poloidal field at plasma edge (T)',
     +        '(bp)',BP)
         CALL OVARRF(NOUT,'Reversal parameter F','(rfpf)',RFPF)
         CALL OVARRF(NOUT,'Pinch parameter theta','(rfpth)',RFPTH)
      END IF
      CALL OVARRF(NOUT,'Total field (T)','(btot)',BTOT)
      CALL OVARRF(NOUT,'Edge safety factor','(q)',Q)
      CALL OVARRF(NOUT,'Cylindrical safety factor','(qstar)',QSTAR)

C+**PJK 29/01/96
      IF (ISHAPE.EQ.1) THEN
         CALL OVARRF(NOUT,'Lower limit for edge safety factor','(qlim)'
     +        ,QLIM)
      END IF

C+**PJK 29/01/96
       IF (ICURR.EQ.2) THEN
         CALL OVARRF(NOUT,'Safety factor at 95% flux','(q95)',Q95)
         CALL OCMMNT(NOUT,'Mean safety factor, q-bar, used for q')
      END IF

C+**PJK 17/11/97 Additional beta diagnostics
      CALL OSUBHD(NOUT,'Beta Information :')
      CALL OVARRF(NOUT,'Total plasma beta','(beta)',BETA)
      CALL OVARRF(NOUT,'Total poloidal beta','(betap)',BETAP)
      CALL OVARRF(NOUT,'Total toroidal beta',' ',BETA*(BTOT/BT)**2)

      CALL OVARRF(NOUT,'Fast alpha beta','(betaft)',BETAFT)
      CALL OVARRF(NOUT,'Beam ion beta','(betanb)',BETANB)
      BETATH = BETA-BETAFT-BETANB
      CALL OVARRF(NOUT,'Thermal beta',' ',BETATH)
      CALL OVARRF(NOUT,'Thermal poloidal beta',' ',BETATH*(BTOT/BP)**2)
      CALL OVARRF(NOUT,'Thermal toroidal beta (= beta-exp)',' ',
     +     BETATH*(BTOT/BT)**2)

      CALL OVARRF(NOUT,'2nd stability beta : beta_p / (R/a)',
     +     '(eps*betap)',EPS*BETAP)
      CALL OVARRF(NOUT,'2nd stability beta upper limit','(epbetmax)'
     +     ,EPBETMAX)

      CALL OVARRF(NOUT,'Troyon g coefficient','(dnbeta)'
     +     ,DNBETA)
      CALL OVARRF(NOUT,'Normalised beta',' ',FBETATRY*DNBETA)

      IF (ITART.EQ.1) THEN
         CALL OVARRF(NOUT,'Normalised thermal toroidal beta',
     +        ' ',FBETATRY*DNBETA*BTOT**2/BT**2)
      END IF

      IF (ICULBL.EQ.0) THEN
         CALL OVARRF(NOUT,'Limit on total beta','(betalim)',BETALIM)
      ELSE IF (ICULBL.EQ.1) THEN
         CALL OVARRF(NOUT,'Limit on thermal beta','(betalim)',BETALIM)
      ELSE
         CALL OVARRF(NOUT,'Limit on thermal + NB beta','(betalim)'
     +        ,BETALIM)
      END IF

      CALL OSUBHD(NOUT,'Temperature and Density (volume averaged) :')
      CALL OVARRF(NOUT,'Electron temperature (keV)','(te)',TE)
      CALL OVARRF(NOUT,'Ion temperature (keV)','(ti)',TI)
      CALL OVARRF(NOUT,'Electron temp., density weighted (keV)','(ten)'
     +     ,TEN)
      CALL OVARRE(NOUT,'Electron density (/m3)','(dene)',DENE)
      CALL OVARRE(NOUT,'Line-averaged electron density (/m3)','(dnla)'
     +     ,DNLA)
      CALL OVARRE(NOUT,'Ion density (/m3)','(dnitot)',DNITOT)
      CALL OVARRE(NOUT,'Fuel density (/m3)','(deni)',DENI)
      CALL OVARRE(NOUT,'High Z impurity density (/m3)','(dnz)',DNZ)
      CALL OVARRE(NOUT,'Cold alpha ash density (/m3)','(dnalp)',DNALP)
C+**PJK 16/01/96
      IF (IDHE3.EQ.1) THEN
         CALL OVARRE(NOUT,'Proton ash density (/m3)','(dnprot)',DNPROT)
      END IF
      CALL OVARRE(NOUT,'Hot beam density (/m3)','(dnbeam)',DNBEAM)
      CALL OVARRE(NOUT,'Density limit (enforced) (/m3)','(dnelimt)'
     +     ,DNELIMT)
      CALL OVARRE(NOUT,'Seeded iron concentration','(cfe0)',CFE0)
      CALL OVARRE(NOUT,'Effective charge','(zeff)',ZEFF)
      CALL OVARRE(NOUT,'Mass weighted effective charge','(zeffai)'
     +     ,ZEFFAI)
      CALL OVARRF(NOUT,'Density profile factor','(alphan)',ALPHAN)
      CALL OVARRF(NOUT,'Temperature profile factor','(alphat)',ALPHAT)

      IF (ICULDL.EQ.1) THEN
         CALL OSUBHD(NOUT,'Density Limit using different models :')
         CALL OVARRE(NOUT,'Old ASDEX model','(dlimit(1))',DLIMIT(1))
         CALL OVARRE(NOUT,'Borrass ITER model I','(dlimit(2))'
     +        ,DLIMIT(2))
         CALL OVARRE(NOUT,'Borrass ITER model II','(dlimit(3))'
     +        ,DLIMIT(3))
         CALL OVARRE(NOUT,'JET edge radiation model','(dlimit(4))'
     +        ,DLIMIT(4))
         CALL OVARRE(NOUT,'JET simplified model','(dlimit(5))'
     +        ,DLIMIT(5))
         CALL OVARRE(NOUT,'Hugill-Murakami Mq model','(dlimit(6))'
     +        ,DLIMIT(6))
C+**PJK 17/09/97
         CALL OVARRE(NOUT,'Greenwald model','(dlimit(7))'
     +        ,DLIMIT(7))
      END IF

      CALL OSUBHD(NOUT,'Fuel Constituents :')
      IF (IDHE3.EQ.0) THEN
         CALL OVARRE(NOUT,'Deuterium fuel fraction','(1-ftr)',1.0D0-FTR)
         CALL OVARRE(NOUT,'Tritium fuel fraction','(ftr)',FTR)
      ELSE
         CALL OVARRE(NOUT,'Deuterium fuel fraction','(fdeut)',FDEUT)
         CALL OVARRE(NOUT,'Tritium fuel fraction','(ftrit)',FTRIT)
         CALL OVARRE(NOUT,'3-Helium fuel fraction','(fhe3)',FHE3)
      END IF

      CALL OSUBHD(NOUT,'Fusion Power :')
      CALL OVARRE(NOUT,'Fusion power (MW)','(powfmw)',POWFMW)
      CALL OVARRE(NOUT,'Alpha power: total (MW)','(alpmw)',ALPMW)
      CALL OVARRE(NOUT,'Alpha power: beam-plasma (MW)','(palpnb)',
     +     PALPNB)
C+**PJK 08/12/95
      IF (IDHE3.EQ.1) THEN
         CALL OVARRE(NOUT,'Neutron power (MW)','(pneut*vol)',PNEUT*VOL)
         CALL OVARRE(NOUT,'Proton power (MW)','(pcharge*vol)',
     +        PCHARGE*VOL)
      END IF
      CALL OVARRE(NOUT,'Neutron wall load (MW/m2)','(wallmw)',WALLMW)
      CALL OVARRF(NOUT,'Fraction of power to electrons','(falpe)',FALPE)
      CALL OVARRF(NOUT,'Fraction of power to ions','(falpi)',FALPI)

      CALL OSUBHD(NOUT,'Plasma Power Balance :')
      CALL OVARRE(NOUT,'Ohmic heating power (MW)','(pohmpv*vol)'
     +     ,POHMPV*VOL)
      CALL OVARRE(NOUT,'Bremsstrahlung radiation power (MW)',
     +     '(pbrem*vol)',PBREM*VOL)
      CALL OVARRE(NOUT,'Synchrotron radiation power (MW)',
     +     '(psync*vol)',PSYNC*VOL)
      CALL OVARRE(NOUT,'Synchrotron reflection factor','(ssync)',SSYNC)
      CALL OVARRE(NOUT,'Scrape-off line radiation power (MW)'
     +     ,'(plrad*vol)',PLRAD*VOL)
      CALL OVARRE(NOUT,'Ion transport (MW)','(ptri*vol))',PTRI*VOL)
      CALL OVARRE(NOUT,'Electron transport (MW)','(ptre*vol)',PTRE*VOL)
      CALL OVARRE(NOUT,'Injection power to ions (MW)','(pinji/1.d6)'
     +     ,PINJI/1.D6)
      CALL OVARRE(NOUT,'Injection power to electrons (MW)'
     +     ,'(pinje/1.d6)',PINJE/1.D6)
      CALL OVARRE(NOUT,'Power to divertor (MW)','(pdivt)',PDIVT)

C+**PJK 17/07/98
      CALL OSUBHD(NOUT,'H-mode Power Threshold Scalings :')

      CALL OVARRE(NOUT,'1996 ITER Scaling: nominal (MW)','(pthrmw(1))',
     +     PTHRMW(1))
      CALL OVARRE(NOUT,'1996 ITER Scaling: upper bound (MW)',
     +     '(pthrmw(2))',PTHRMW(2))
      CALL OVARRE(NOUT,'1996 ITER Scaling: lower bound (MW)',
     +     '(pthrmw(3))',PTHRMW(3))
      CALL OVARRE(NOUT,'1997 ITER Scaling (1) (MW)','(pthrmw(4))',
     +     PTHRMW(4))
      CALL OVARRE(NOUT,'1997 ITER Scaling (2) (MW)','(pthrmw(5))',
     +     PTHRMW(5))

      CALL OSUBHD(NOUT,'Confinement :')

C+**PJK 01/04/98
      IF (IGNITE.EQ.1) THEN
         CALL OCMMNT(NOUT,
     +        'Device is assumed to be ignited for the calculation'//
     +        ' of confinement time')
         CALL OBLNKL(NOUT)
      END IF

      WRITE(NOUT,10) TAUSCL(ISC)
 10   FORMAT(' Confinement scaling law',T45,A24)

      CALL OVARRF(NOUT,'Confinement H factor','(hfact)',HFACT)
      CALL OVARRE(NOUT,'Global confinement time (s)','(taueff)',TAUEFF)
      CALL OVARRE(NOUT,'Ion confinement time (s)','(tauei)',TAUEI)
      CALL OVARRE(NOUT,'Electron confinement time (s)','(tauee)',TAUEE)
      CALL OVARRE(NOUT,'n-tau (s/m3)','(dntau)',DNTAU)
      CALL OVARRE(NOUT,'Heating power assumed (MW)','(powerht)',POWERHT)

      CALL OSUBHD(NOUT,'Plasma Volt-second Requirements :')
      CALL OVARRE(NOUT,'Total volt-second requirement (Wb)','(vsstt)'
     +     ,VSSTT)
      CALL OVARRE(NOUT,'Inductive volt-seconds (Wb)','(vsind)',VSIND)
      CALL OVARRE(NOUT,'Start-up resistive (Wb)','(vsres)',VSRES)
      CALL OVARRE(NOUT,'Flat-top resistive (Wb)','(vsbrn)',VSBRN)
      CALL OVARRF(NOUT,'Bootstrap fraction','(bootipf)',BOOTIPF)
      CALL OVARRF(NOUT,'Auxiliary current drive fraction',
     +     '(faccd)',FACCD)
      CALL OVARRE(NOUT,'Plasma resistance (ohm)','(rplas)',RPLAS)
      CALL OVARRE(NOUT,'Plasma inductance (H)','(rlp)',RLP)
      CALL OVARRE(NOUT,'Sawteeth coefficient','(csawth)',CSAWTH)
      CALL OVARRE(NOUT,'Burn time (s)','(tburn)',TBURN)

      CALL OSUBHD(NOUT,'Auxiliary Information :')
      CALL OVARRE(NOUT,'Convective loss rate (A)','(qfuel)',QFUEL)
      CALL OVARRE(NOUT,'Burn-up fraction','(burnup)',BURNUP)

 1000 CONTINUE

      RETURN
      END
