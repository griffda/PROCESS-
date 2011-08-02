!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfpwcall(nout,iprint)

  !+ad_name  tfpwcall
  !+ad_summ  Calls the TF coil power conversion routine for superconducting coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calls routine <CODE>tfcpwr</CODE> to calculate the power
  !+ad_desc  conversion requirements for superconducting TF coils.
  !+ad_prob  None
  !+ad_call  bldgvol.h90
  !+ad_call  htpwr.h90
  !+ad_call  param.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  tfcpwr
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'tfcoil.h90'
  include 'phydat.h90'
  include 'bldgvol.h90'
  include 'htpwr.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  real(kind(1.0D0)) :: drarea,ettfmj,itfka

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Stored energy (MJ)

  ettfmj = estotf * 1.0D3

  !  TF coil current (kA)

  itfka = 1.0D-3 * cpttf

  call tfcpwr(nout,iprint,tfno,ettfmj,itfka,rhotfleg, &
       vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

end subroutine tfpwcall

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfcpwr(nout,iprint,ntfc,ettfmj,itfka, &
     rptfc,vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

  !+ad_name  tfcpwr
  !+ad_summ  Calculates the TF coil power conversion system parameters
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_args  ntfc : input real : number of tf coils
  !+ad_args  ettfmj : input real : total stored energy of one TF coils, MJ
  !+ad_args  itfka : input real : design current for the TF coils, kA
  !+ad_args  rptfc : input real : resistance of a TF coil, ohms
  !+ad_args  vtfskv : input real : allowable voltage across a TF coil during quench, kV
  !+ad_args  rmajor : input real : plasma major radius, m
  !+ad_args  tfckw : output real : available DC power for charging the TF coils, kW
  !+ad_args  tfbusl : output real : total bus length of the TF coil system, m
  !+ad_args  drarea : output real : approx. area needed for the energy dump resistors, m2
  !+ad_args  tfcbv : output real : approx. vol needed for the TF coil power supplies
  !+ad_argc                        and DC circuit breakers, m3
  !+ad_args  tfacpd : output real : steady state TF coil AC power demand, MW
  !+ad_desc  This routine calculates the TF power conversion system
  !+ad_desc  parameters:  floor space, power supplies, bussing,
  !+ad_desc  coil protection equipment, and the associated controls
  !+ad_desc  and instrumentation. It was originally written by G. Gorker,
  !+ad_desc  FEDC/ORNL, April 1987, modified by J. Galambos in 1991 to
  !+ad_desc  run in TETRA, and included in PROCESS in 1992 by P. C. Shipe.
  !+ad_prob  None
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout, iprint
  real(kind(1.0D0)), intent(in) :: ntfc,ettfmj,itfka,rptfc,vtfskv,rmajor
  real(kind(1.0D0)), intent(out) :: tfckw,tfbusl,drarea,tfcbv,tfacpd

  !  Local variables

  real(kind(1.0D0)) :: albusa,albuswt,djmka,fspc1,fspc2,fspc3,ettfc, &
       ltfth,lptfcs,ncpbkr,ndumpr,nsptfc,ntfbkr,ntfpm,part1,part2, &
       part3,rcoils,rpower,rtfbus,rtfps,r1dump,r1emj,r1ppmw,tchghr, &
       tfackw,tfcfsp,tfcv,tfpmka,tfpmkw,tfpska,tfpsv,tfpmv,tftbv, &
       tftsp,ttfsec,vtfbus,xpower,xpwrmw,ztotal

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ncpbkr = 1.0D0   !  number of TF coils per circuit breaker
  djmka = 0.125D0  !  design current density of TF bus, kA/cm2
  rtfps = 1.05D0   !  rating factor for TF coil power supplies
  fspc1 = 0.15D0   !  floor space coefficient for power supplies
  fspc2 = 0.8D0    !  floor space coefficient for circuit breakers
  fspc3 = 0.4D0    !  floor space coefficient for load centres

  if (rptfc == 0.0D0) then
     tchghr = 4.0D0  !  charge time of the coils, hours
     nsptfc = 1.0D0  !  1.0 = superconducting, 0.0 = resistive
  else
     tchghr = 0.16667D0
     nsptfc = 0.0D0
  end if

  tfacpd = 0.0D0
  ettfc = ntfc*ettfmj  !  stored energy of all TF coils, MJ
  ltfth = 2.0D0*ettfc/itfka**2  !  inductance of all TF coils, Henries

  ntfbkr = ntfc/ncpbkr  !  number of circuit breakers
  lptfcs = ltfth/ntfc  !  inductance per TF coil, Henries
  albusa = itfka/djmka  !  aluminium bus section area, sq cm
  tfbusl = 8.0D0*3.1416D0*rmajor + &  !  total TF system bus length, m
       (1.0D0+ntfbkr)*(12.0D0*rmajor+80.0D0) + &
       0.2D0*itfka*sqrt(ntfc*rptfc*1000.0D0)
  albuswt = 2.7D0*albusa*tfbusl/1.0D4
  rtfbus = 2.62D-4*tfbusl/albusa  !  total resistance of TF bus, ohms
  vtfbus = 1000.0D0*itfka*rtfbus  !  total voltage drop across TF bus, volts
  rcoils = ntfc*rptfc  !  total resistance of the TF coils, ohms
  ztotal = rtfbus+rcoils+ltfth/(3600.0D0*tchghr)
  tfcv = 1000.0D0*itfka*ztotal  !  charging voltage for the TF coils, volts
  ntfpm = (itfka * (1.0D0 + nsptfc) )/5.0D0  !  number of TF power modules
  tfpmv = rtfps*tfcv/(1.0D0+nsptfc)  !  TF coil power module voltage, volts
  tfpsv = rtfps*tfcv
  tfpska = rtfps*itfka
  tfpmka = rtfps*itfka/(ntfpm/(1.0D0+nsptfc))  !  TF power module current, kA
  tfpmkw = tfpmv*tfpmka  !  TF power module power, kW
  tfckw = tfpmkw*ntfpm  !  peak AC power needed to charge coils, kW
  tfackw = tfckw/0.9D0
  r1dump = nsptfc*vtfskv*ncpbkr/itfka  !  resistance of dump resistor, ohms
  ttfsec = lptfcs*ncpbkr/(r1dump*nsptfc+rptfc*(1.0D0-nsptfc))  !  time constant, s
  ndumpr = ntfbkr*4.0D0  !  number of dump resistors
  r1ppmw = nsptfc*r1dump*(itfka/2.0D0)**2  !  peak power to a dump resistor
                                           !  during quench, MW
  r1emj = nsptfc*ettfc/(ndumpr+0.0001D0)  !  energy to a dump resistor during quench, MJ
  rpower = (ntfc*rptfc+rtfbus)*itfka**2
  xpower = ltfth/(3600.0D0*tchghr)*itfka**2  !  total TF coil mgf peak inductive
                                             !  power demand, MW

  part1 = fspc1*ntfpm*tfpmkw**0.667D0
  part2 = fspc2*ntfbkr*(vtfskv*itfka)**0.667D0
  part3 = fspc3*(tfackw/(2.4D0*nsptfc+13.8D0*(1.0D0-nsptfc)))**0.667D0
  tfcfsp = part1 + part2 + part3
  drarea = 0.5D0*ndumpr*(1.0D0+r1emj)**0.667D0
  tfcbv = 6.0D0*tfcfsp
  xpwrmw = xpower/0.9D0
  tfacpd = tfacpd+rpower/0.9D0

  tftsp = tfcfsp  !  total TF coil power conversion building floor area, m2
  tftbv = tfcbv   !  total TF coil power conversion building volume, m3

  !  Output section

  if ((iprint == 0).or.(sect13 == 0)) return

  call oheadr(nout,'Superconducting TF Coil Power Conversion')

  call ovarre(nout,'TF coil stored energy (MJ)','(ettfmj)',ettfmj)
  call ovarre(nout,'TF coil current (kA)','(itfka)',itfka)
  call ovarre(nout,'Number of TF coils','(ntfc)',ntfc)
  call ovarre(nout,'Maximum voltage across TF coil (kV)','(vtfskv)', &
       vtfskv)
  call ovarre(nout,'TF coil charge time (hours)','(tchghr)',tchghr)
  call ovarre(nout,'Total inductance of TF coils (H)','(ltfth)', &
       ltfth)
  call ovarre(nout,'Total resistance of TF coils (ohm)','(rcoils)', &
       rcoils)
  call ovarre(nout,'Inductance per TF coil (H)','(lptfcs)',lptfcs)
  call ovarre(nout,'TF coil charging voltage (V)','(tfcv)',tfcv)
  call ovarre(nout,'Number of DC circuit breakers','(ntfbkr)', &
       ntfbkr)
  call ovarre(nout,'Number of dump resistors','(ndumpr)',ndumpr)
  call ovarre(nout,'Resistance per dump resistor (ohm)','(r1dump)', &
       r1dump)
  call ovarre(nout,'Dump resistor peak power (MW)','(r1ppmw)', &
       r1ppmw)
  call ovarre(nout,'Energy supplied per dump resistor (MJ)', &
       '(r1emj)',r1emj)
  call ovarre(nout,'TF coil L/R time constant (s)','(ttfsec)', &
       ttfsec)
  call ovarre(nout,'Power supply voltage (V)','(tfpsv)',tfpsv)
  call ovarre(nout,'Power supply current (kA)','(tfpska)',tfpska)
  call ovarre(nout,'DC power supply rating (kW)','(tfckw)',tfckw)
  call ovarre(nout,'AC power for charging (kW)','(tfackw)',tfackw)
  call ovarre(nout,'TF coil resistive power (MW)','(rpower)',rpower)
  call ovarre(nout,'TF coil inductive power (MW)','(xpower)',xpower)
  call ovarre(nout,'Aluminium bus current density (kA/cm2)', &
       '(djmka)',djmka)
  call ovarre(nout,'Aluminium bus cross-sectional area (cm2)', &
       '(albusa)',albusa)
  call ovarre(nout,'Total length of TF coil bussing (m)', &
       '(tfbusl)',tfbusl)
  call ovarre(nout,'Aluminium bus weight (tonnes)','(albuswt)', &
       albuswt)
  call ovarre(nout,'Total TF coil bus resistance (ohm)','(rtfbus)', &
       rtfbus)
  call ovarre(nout,'TF coil bus voltage drop (V)','(vtfbus)',vtfbus)
  call ovarre(nout,'Dump resistor floor area (m2)','(drarea)', &
       drarea)
  call ovarre(nout,'TF coil power conversion floor space (m2)', &
       '(tfcfsp)',tfcfsp)
  call ovarre(nout,'TF coil power conv. building volume (m3)', &
       '(tfcbv)',tfcbv)
  call ovarre(nout,'TF coil AC inductive power demand (MW)', &
       '(xpwrmw)',xpwrmw)
  call ovarre(nout,'Total steady state AC power demand (MW)', &
       '(tfacpd)',tfacpd)

end subroutine tfcpwr
