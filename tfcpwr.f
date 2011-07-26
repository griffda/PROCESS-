C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: tfcpwr.f,v 3.2 1997/01/24 14:59:57 peter Exp $
C  Module name    : $RCSfile: tfcpwr.f,v $
C  Version no.    : $Revision: 3.2 $
C  Creation date  : $Date: 1997/01/24 14:59:57 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE TFPWCALL(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calls the TF coil power conversion routine for
C  superconducting coils.
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
C  22/01/97 PJK 1.000 Initial upgraded version; heattr.h subsumed into
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
      INCLUDE 'param.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'bldgvol.h'
      INCLUDE 'htpwr.h'

C  Arguments
      INTEGER NOUT,IPRINT

C  Local variables
      DOUBLE PRECISION DRAREA,ETTFMJ,ITFKA

C  External routines
      EXTERNAL TFCPWR

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Stored energy (MJ)

      ETTFMJ = ESTOTF * 1.0D3

C *** TF coil current (kA)

      ITFKA = 1.0D-3 * CPTTF

      CALL TFCPWR(NOUT,IPRINT,TFNO,ETTFMJ,ITFKA,RHOTFLEG,
     +     VTFSKV,RMAJOR,TFCKW,TFBUSL,DRAREA,TFCBV,TFACPD)

      RETURN
      END
c______________________________________________________________________
      SUBROUTINE TFCPWR(nout,iprint,ntfc,ettfmj,itfka,
     +     rptfc,vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

c  TF coil power conversion routine
c
c  This program calculates the TF power conversion system
c  parameters:  floor space, power supplies, bussing,
c  coil protection equipment, and the associated controls
c  and instrumentation. It was written by G. Gorker,
c  FEDC/ORNL 4/1987.
c
c  modified by J. Galambos to run in TETRA
c  modified by J. Galambos 2/29/91 to improve format and 
c  use implicit none.  Also removed costing from this
c  routine (wasn't used).
c
c  Included in PROCESS in March 1992 by P. C. Shipe.
c     
c  fortran label definitions
c
c  r=resistive, sc=superconductive, x=inductive
c
c
c  INPUTS FROM OTHER CODE MODULES:
c  -------------------------------
c
c  nout    unit specifier for output device
c  iprint  switch, = 0 for calculation, = 1 for print output
c  ntfc    number of tf coils
c  ettfmj  total stored energy of one TF coils, MJ
c  itfka   design current for the TF coils, kA
c  rptfc   resistance of a TF coil, ohms
c  vtfskv  allowable voltage across a TF coil during quench, kV
c  rmajor  plasma major radius, metres
c
c  OUTPUTS TO OTHER CODE MODULES:
c  ---------------------------------
c
c  tfckw   available dc power for charging the TF coils, kW
c  tfbusl  total bus length of the TF coil system, metres
c  drarea   approx. area needed for the energy dump resistors, sq.m
c  tfcbv  approx. vol needed for the tf coil power supplies and
c  dc circuit breakers, cu. metres
c  tfacpd  steady state tf coil ac power demand, MW
c
c
c  DATA FIXED HERE :
c  -----------------
c
c  ncpbkr  number of tf coils per circuit breaker
c  rtfps   rating factor for tf coil power supplies
c  djmka   design current density of tf buss , KA/ cm2
c  fspc1   floor space coefficient for power supplies
c  fspc2   floor space coefficient for circuit breakers
c  fspc3   floor space coefficient for load centers
c
c
c  OTHER PARAMETERS INTERNAL TO THIS FILE
c  ---------------------------------------
c
c  albusa  aluminium bus section area, sq cm
c  ettfc   stored energy of all tf coils, MJ
c  lptfcs  inductance per tf coil, henries
c  ltfth   inductance of all tf coils, henries
c  ndumpr  number of dump resistors
c  nsptfc  an index operator always 1.0 = supercon, 0 = resistive
c  ntfbkr  number of circuit breakers
c  ntfpm   number of tf power modules
c  ettfr   total energy of the r coils alone, mj
c  rcoils  total resistance of the TF coils, ohms
c  rtfbus  total resistance of TF bus, ohms
c  r1dump  resistance of dump resistor, ohms
c  r1emj   energy to a dump resistor during quench, mj
c  r1ppmw  peak power to a dump resistor during quench, mw
c  tchghr  charge time of the coils, hours
c  tfbusl  total tf system bus length, m
c  tfckw   peak ac power needed to charge coils, kW
c  tfcv    charging voltage for the TF coils, volts
c  tfpmka  tf power module current, kA
c  tfpmkw  tf power module power, kW
c  tfpmv   tf coil power module voltage, volts
c  tftbv   total tf coil power conversion building volume,cu.m
c  tftsp   total tf coil power conversion building floor area,sq.m.
c  ttfsec  time constant of tf coils,seconds
c  vtfbus  total voltage drop across tf bus, volts
c  xpwrmw  total tf coil mgf peak xpower demand, MW

      INCLUDE 'osections.h'

      DOUBLE PRECISION ntfc,ettfmj,itfka,rptfc,vtfskv,rmajor,tfckw,
     +     tfbusl,drarea,tfcbv,tfacpd

      DOUBLE PRECISION albusa,albuswt,djmka,fspc1,fspc2,fspc3, 
     +     ettfc, ltfth, lptfcs, ncpbkr, ndumpr, nsptfc,
     +     ntfbkr, ntfpm, part1, part2, part3, rcoils,rpower,
     +     rtfbus,rtfps,r1dump, r1emj, r1ppmw,  tchghr, tfackw,
     +     tfcfsp, tfcv, tfpmka, tfpmkw, tfpska, tfpsv, tfpmv,
     +     tftbv, tftsp, ttfsec, vtfbus,xpower,xpwrmw,ztotal

      INTEGER nout,iprint

      EXTERNAL oheadr,ovarre

      ncpbkr = 1.0d0
      djmka = 0.125d0
      rtfps = 1.05d0
      fspc1 = 0.15d0
      fspc2 = 0.8d0
      fspc3 = 0.4d0

      if (rptfc .eq. 0.d0) then
         tchghr = 4.d0
         nsptfc = 1.d0
      else
         tchghr = 0.16667d0
         nsptfc = 0.d0
      end if

      tfacpd = 0.0d0
      ettfc = ntfc*ettfmj
      ltfth = 2.d0*ettfc/itfka**2

      ntfbkr = ntfc/ncpbkr
      lptfcs = ltfth/ntfc
      albusa = itfka/djmka
      tfbusl = 8.d0*3.1416d0*rmajor+(1.d0+ntfbkr)*(12.d0*rmajor+80.d0)+
     +     0.2d0*itfka*sqrt(ntfc*rptfc*1000.d0)
      albuswt = 2.7d0*albusa*tfbusl/1.0d4
      rtfbus = 2.62d-4*tfbusl/albusa
      vtfbus = 1000.d0*itfka*rtfbus
      rcoils = ntfc*rptfc
      ztotal = rtfbus+rcoils+ltfth/(3600.d0*tchghr)
      tfcv = 1000.d0*itfka*ztotal
      ntfpm = (itfka * (1.d0 + nsptfc) )/5.d0
      tfpmv = rtfps*tfcv/(1.d0+nsptfc)
      tfpsv = rtfps*tfcv
      tfpska = rtfps*itfka
      tfpmka = rtfps*itfka/(ntfpm/(1.d0+nsptfc))
      tfpmkw = tfpmv*tfpmka
      tfckw = tfpmkw*ntfpm
      tfackw = tfckw/0.9d0
      r1dump = nsptfc*vtfskv*ncpbkr/itfka
      ttfsec = lptfcs*ncpbkr/(r1dump*nsptfc+rptfc*(1.d0-nsptfc))
      ndumpr = ntfbkr*4.d0
      r1ppmw = nsptfc*r1dump*(itfka/2.0d0)**2
      r1emj = nsptfc*ettfc/(ndumpr+0.0001d0)
      rpower = (ntfc*rptfc+rtfbus)*itfka**2
      xpower = ltfth/(3600.d0*tchghr)*itfka**2

      part1 = fspc1*ntfpm*tfpmkw**0.667d0
      part2 = fspc2*ntfbkr*(vtfskv*itfka)**0.667d0
      part3 = fspc3*(tfackw/(2.4d0*nsptfc+13.8d0*
     +     (1.d0-nsptfc)))**0.667d0
      tfcfsp = part1 + part2 + part3
      drarea = 0.5d0*ndumpr*(1.d0+r1emj)**0.667d0
      tfcbv = 6.d0*tfcfsp
      xpwrmw = xpower/0.9d0
      tfacpd = tfacpd+rpower/0.9d0

C+**PJK 14/12/92 Rather pointless definitions
      tftsp = tfcfsp
      tftbv = tfcbv

      if ((iprint.eq.0).or.(sect13.eq.0)) goto 1000

      call oheadr(nout,'Superconducting TF Coil Power Conversion')

      call ovarre(nout,'TF coil stored energy (MJ)','(ettfmj)',ettfmj)
      call ovarre(nout,'TF coil current (kA)','(itfka)',itfka)
      call ovarre(nout,'Number of TF coils','(ntfc)',ntfc)
      call ovarre(nout,'Maximum voltage across TF coil (kV)','(vtfskv)',
     +     vtfskv)
      call ovarre(nout,'TF coil charge time (hours)','(tchghr)',tchghr)
      call ovarre(nout,'Total inductance of TF coils (H)','(ltfth)',
     +     ltfth)
      call ovarre(nout,'Total resistance of TF coils (ohm)','(rcoils)',
     +     rcoils)
      call ovarre(nout,'Inductance per TF coil (H)','(lptfcs)',lptfcs)
      call ovarre(nout,'TF coil charging voltage (V)','(tfcv)',tfcv)
      call ovarre(nout,'Number of DC circuit breakers','(ntfbkr)',
     +     ntfbkr)
      call ovarre(nout,'Number of dump resistors','(ndumpr)',ndumpr)
      call ovarre(nout,'Resistance per dump resistor (ohm)','(r1dump)',
     +     r1dump)
      call ovarre(nout,'Dump resistor peak power (MW)','(r1ppmw)',
     +     r1ppmw)
      call ovarre(nout,'Energy supplied per dump resistor (MJ)',
     +     '(r1emj)',r1emj)
      call ovarre(nout,'TF coil L/R time constant (s)','(ttfsec)',
     +     ttfsec)
      call ovarre(nout,'Power supply voltage (V)','(tfpsv)',tfpsv)
      call ovarre(nout,'Power supply current (kA)','(tfpska)',tfpska)
      call ovarre(nout,'DC power supply rating (kW)','(tfckw)',tfckw)
      call ovarre(nout,'AC power for charging (kW)','(tfackw)',tfackw)
      call ovarre(nout,'TF coil resistive power (MW)','(rpower)',rpower)
      call ovarre(nout,'TF coil inductive power (MW)','(xpower)',xpower)
      call ovarre(nout,'Aluminium bus current density (kA/cm2)',
     +     '(djmka)',djmka)
      call ovarre(nout,'Aluminium bus cross-sectional area (cm2)',
     +     '(albusa)',albusa)
      call ovarre(nout,'Total length of TF coil bussing (m)','(tfbusl)',
     +     tfbusl)
      call ovarre(nout,'Aluminium bus weight (tonnes)','(albuswt)',
     +     albuswt)
      call ovarre(nout,'Total TF coil bus resistance (ohm)','(rtfbus)',
     +     rtfbus)
      call ovarre(nout,'TF coil bus voltage drop (V)','(vtfbus)',vtfbus)
      call ovarre(nout,'Dump resistor floor area (m2)','(drarea)',
     +     drarea)
      call ovarre(nout,'TF coil power conversion floor space (m2)',
     +     '(tfcfsp)',tfcfsp)
      call ovarre(nout,'TF coil power conv. building volume (m3)',
     +     '(tfcbv)',tfcbv)
      call ovarre(nout,'TF coil AC inductive power demand (MW)',
     +     '(xpwrmw)',xpwrmw)
      call ovarre(nout,'Total steady state AC power demand (MW)',
     +     '(tfacpd)',tfacpd)

 1000 continue

      return
      end
