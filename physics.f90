!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine physics

  !+ad_name  physics
  !+ad_summ  Routine to calculate tokamak plasma physics information
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calculates all the primary plasma physics
  !+ad_desc  characteristics for a tokamak device.
  !+ad_prob  None
  !+ad_call  build.h90
  !+ad_call  cdriv.h90
  !+ad_call  divrt.h90
  !+ad_call  numer.h90
  !+ad_call  param.h90
  !+ad_call  phydat.h90
  !+ad_call  pulse.h90
  !+ad_call  times.h90
  !+ad_call  beamfus
  !+ad_call  betcom
  !+ad_call  bootst
  !+ad_call  cudriv
  !+ad_call  culblm
  !+ad_call  culbst
  !+ad_call  culcur
  !+ad_call  culdlm
  !+ad_call  denlim
  !+ad_call  fnewbs
  !+ad_call  palph
  !+ad_call  palph2
  !+ad_call  pcond
  !+ad_call  phyaux
  !+ad_call  pohm
  !+ad_call  pthresh
  !+ad_call  radpwr
  !+ad_call  rether
  !+ad_call  vscalc
  !+ad_hist  20/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  04/12/95 PJK Added D-He3 relevant coding
  !+ad_hist  14/05/96 PJK Modified poloidal beta used in bootstrap formula
  !+ad_hisc               and added diamagnetic contribution
  !+ad_hist  10/06/96 PJK Added use of IWALLD in wall load calculation
  !+ad_hist  07/10/96 PJK Added new ICULBL=2 option
  !+ad_hist  17/09/97 PJK Added Greenwald density limit (added arguments
  !+ad_hisc               to CULDLM)
  !+ad_hist  01/04/98 PJK Changed PBREM to PRAD in argument list of PCOND,
  !+ad_hisc               added DNLA and IGNITE to arguments of BETCOM
  !+ad_hisc               and PCOND, and added other effects of IGNITE
  !+ad_hist  24/04/98 PJK Added IMPC, IMPFE, IMPO to arguments of BETCOM
  !+ad_hist  30/06/98 PJK Added XAREA to arguments of PCOND
  !+ad_hist  17/07/98 PJK Added call to PTHRESH
  !+ad_hist  19/01/99 PJK Added POWERHT to argument list of PCOND
  !+ad_hist  16/07/01 PJK Added KAPPAA to argument list of PCOND
  !+ad_hist  22/05/06 PJK Added IFALPHAP to argument list of PALPH2
  !+ad_hist  10/11/11 PJK Initial F90 version; retired routine CURREN
  !+ad_hisc               and switch ICULCR
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'
  include 'phydat.h90'
  include 'cdriv.h90'
  include 'times.h90'
  include 'divrt.h90'
  include 'build.h90'
  include 'pulse.h90'

  !  Arguments

  !  Local variables

  real(kind(1.0D0)) :: alphap,betat,betpth,fusrat,n0e,n0i,pht,pinj,p0, &
       sbar,sigvdt,taup,t0e,t0i,zimp,zion

  !  External functions

  real(kind(1.0D0)), external :: fnewbs,culbst

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Calculate plasma composition

  call betcom(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr,ftritbm, &
       idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff,abeam, &
       afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot,dnla, &
       dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,zion)

  !  Density-weighted temperatures

  ten = te * pcoef
  tin = ti * pcoef

  !  Central values for temperature (keV), density (m**-3) and
  !  pressure (Pa). From ideal gas law : p = nkT

  t0e = te * (1.0D0+alphat)
  t0i = ti * (1.0D0+alphat)
  n0e =   dene * (1.0D0+alphan)
  n0i = dnitot * (1.0D0+alphan)
  p0 = (n0e*t0e + n0i*t0i) * 1.6022D-16

  !  Pressure profile index
  !  From ideal gas law : p = nkT

  alphap = alphan + alphat

  !  Calculate plasma current

  call culcur(alphaj,alphap,bt,eps,icurr,kappa,kappa95,p0,q, &
       rmajor,rminor,sf,triang,triang95,bp,qstar,plascur)

  if (icurr == 2) then
     q95 = q * 1.3D0 * (1.0D0 - eps)**0.6D0
  else
     q95 = q
  end if

  btot = sqrt(bt**2 + bp**2)
  betap = beta * ( btot/bp )**2

  !  Set PF coil ramp times

  if (lpulse /= 1) then

     if (tohsin == 0.0D0) then
        tohs = plascur/5.0D5
        tramp = tohs
        tqnch = tohs
     else
        tohs = tohsin
     end if

  else

     !  tohs is set either in INITIAL or INPUT, or by being
     !  iterated using limit equation 41.

     tramp = max(tramp,tohs)
     tqnch = max(tqnch,tohs)

  end if

  !  Reset second tburn value (tburn0).
  !  Use of consistency eqn.15 will ensure that the code
  !  uses a consistent value for the burn time throughout,
  !  for the pulsed reactor case.

  tburn0 = tburn

  !  Pulse and down times : The reactor is assumed to be 'down'
  !  at all times outside of the plasma current flat-top period.
  !  The pulse length is the duration of non-zero plasma current

  tpulse = tohs + theat + tburn + tqnch
  tdown  = tramp + tohs + tqnch + tdwell

  !  Calculate bootstrap current fraction

  if (bscfmax < 0.0D0) then

     bootipf = abs(bscfmax)

  else
     if (ibss == 1) then
        call bootst(aspect,beta,btot,cboot,plascur,pi,q95,q0,rmajor, &
             vol,bootipf)

     else if (ibss == 2) then
        betat = beta * btot**2 / bt**2
        bootipf = fnewbs(alphan,alphat,betat,bt,dene,plascur,q95, &
             q0,rmajor,rminor,ten,zeff)

     else if (ibss == 3) then
        !  Uses thermal poloidal beta, not total
        betpth = (beta-betaft-betanb) * ( btot/bp )**2
        bootipf = culbst(alphaj,alphan,alphat,beta,betpth,q0,q95, &
             rmajor,rminor,itart)

     else
        write(*,*) 'Error in routine PHYSICS:'
        write(*,*) 'Illegal value of IBSS, = ',ibss
        write(*,*) 'PROCESS stopping.'
        stop
     end if

     bootipf = min(bootipf,bscfmax)

  end if

  !  Bootstrap current fraction constrained to be less than
  !  or equal to the total fraction of the plasma current
  !  produced by non-inductive means (which also includes
  !  the current drive proportion)

  bootipf = min(bootipf,fvsbrnni)

  !  Fraction of plasma current produced by inductive means

  facoh = max( 1.0D-10, (1.0D0 - fvsbrnni) )

  !  Fraction of plasma current produced by auxiliary current drive

  faccd = fvsbrnni - bootipf

  !  Do auxiliary current drive power calculations

  if (irfcd /= 0) call cudriv(nout,0)

  !  Calculate fusion power

  call palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit, &
       idhe3,iiter,pcoef,pi,ti,palp,pcharge,pneut,sigvdt)

  !  Calculate neutral beam slowing down effects
  !  If ignited, then ignore beam fusion effects

  if ((pnbeam /= 0.0D0).and.(ignite == 0)) then
     call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
          ealpha,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol, &
          zeffai,betanb,dnbeam2,palpnb)
  end if

  call palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb, &
       ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft, &
       palp,palpi,palpe,pfuscmw,powfmw)

  !  Neutron wall load

  if (iwalld == 1) then
     wallmw = ffwal * (pneut*vol) / sarea
  else
     wallmw = ffwal * (pneut*vol) / fwarea
  end if

  !  Calculate ion/electron equilibration power

  call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

  !  Calculate radiation power

  call radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,rmajor, &
       rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,pbrem,plrad, &
       prad,psync)

  !  Limit for minimum radiation power

  pht = 1.0D-6 * (pinji + pinje) + alpmw + pcharge*vol
  pbrem = max( (fradmin*pht/vol), pbrem)
  prad = pbrem + psync

  !  Calculate ohmic power

  call pohm(facoh,ires,kappa95,plascur,rmajor,rminor,ten,vol,zeff, &
       pohmpv,rpfac,rplas)

  !  Calculate L- to H-mode power threshold

  call pthresh(dene,dnla,bt,rmajor,kappa,pthrmw)

  !  Density limit

  pinj = 1.0D-6 * (pinje + pinji)/vol
  pdivt = vol * (palp + pcharge + pinj + pohmpv - prad - plrad)
  pdivt = max(0.001D0, pdivt)

  if (iculdl == 0) then  !  Use old method
     call denlim(bt,rmajor,prn1,pdivt,q95,sarea,dnelimt)

  else if (iculdl == 1) then
     call culdlm(bt,idensl,pdivt,pi,plascur,prn1,qstar,q95, &
          rmajor,rminor,sarea,zeff,dlimit,dnelimt)

  else
     write(*,*) 'Error in routine PHYSICS:'
     write(*,*) 'Illegal value for ICULDL, = ',iculdl
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  Calculate transport losses and energy confinement time using the
  !  chosen scaling law

  call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
       iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
       plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q95,qstar,vol, &
       xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

  !  Calculate volt-second requirements

  call vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rmu0,rplas, &
       plascur,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

  !  Calculate auxiliary physics related information
  !  for the rest of the code

  sbar = 1.0D0
  call phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp, &
       dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

  !  Calculate beta limit

  if (gtscale /= 0) then
     dnbeta = 2.7D0 * (1.0D0 + 5.0D0*eps**3.5D0)
  end if

  call culblm(bt,dnbeta,plascur,rminor,betalim)

end subroutine physics

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bootst(aspect,beta,bt,cboot,plascur,pi,q,q0,rmajor,vol, &
     bootipf)

  !+ad_name  bootst
  !+ad_summ  Original ITER calculation of bootstrap-driven fraction
  !+ad_summ  of the plasma current.
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  aspect  : input real : plasma aspect ratio
  !+ad_args  beta    : input real : plasma total beta
  !+ad_args  bt      : input real : toroidal field on axis (T)
  !+ad_args  cboot   : input real : bootstrap current fraction multiplier
  !+ad_args  plascur : input real : plasma current (A)
  !+ad_args  pi      : input real : famous number: 3.14...
  !+ad_args  q       : input real : safety factor at 95% surface
  !+ad_args  q0      : input real : central safety factor
  !+ad_args  rmajor  : input real : plasma major radius (m)
  !+ad_args  vol     : input real : plasma volume (m3)
  !+ad_args  bootipf : output real : bootstrap current fraction
  !+ad_desc  This routine performs the original ITER calculation of the
  !+ad_desc  plasma current bootstrap fraction.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  20/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  23/05/06 PJK Prevented negative square roots from being attempted
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: aspect, beta, bt, cboot, pi, &
       plascur, q, q0, rmajor, vol
  real(kind(1.0D0)), intent(out) :: bootipf

  !  Local variables

  real(kind(1.0D0)), parameter :: rmu0 = 1.256637D-6
  real(kind(1.0D0)) :: betapbs, bpbs, cbs, xbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  xbs = min( 10.0D0, q/q0 )
  cbs = cboot * (1.32D0 - 0.235D0*xbs + 0.0185D0*xbs**2 )
  bpbs = rmu0*plascur/(2.0D0*pi*sqrt(vol/(2.0D0* pi**2 *rmajor)) )
  betapbs = beta*bt**2 / bpbs**2

  if (betapbs <= 0.0D0) then  !  only possible if beta <= 0.0
     bootipf = 0.0D0
  else
     bootipf = cbs * ( betapbs/sqrt(aspect) )**1.3D0
  end if

end subroutine bootst

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine culcur(alphaj,alphap,bt,eps,icurr,kappa,kappa95,p0,qpsi, &
     rmajor,rminor,sf,triang,triang95,bp,qstar,plascur)

  !+ad_name  culcur
  !+ad_summ  Routine to calculate the plasma current
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  conhas
  !+ad_cont  plasc
  !+ad_args  alphaj   : input real :  current profile index
  !+ad_args  alphap   : input real :  pressure profile index
  !+ad_args  bt       : input real :  toroidal field on axis (T)
  !+ad_args  eps      : input real :  inverse aspect ratio
  !+ad_args  icurr    : input integer :  current scaling model to use
  !+ad_args                           1 = Peng analytic fit
  !+ad_args                           2 = Peng divertor scaling (TART)
  !+ad_args                           3 = simple ITER scaling
  !+ad_args                           4 = revised ITER scaling
  !+ad_args                           5 = Todd empirical scaling I
  !+ad_args                           6 = Todd empirical scaling II
  !+ad_args                           7 = Connor-Hastie model
  !+ad_args  kappa    : input real :  plasma elongation
  !+ad_args  kappa95  : input real :  plasma elongation at 95% surface
  !+ad_args  p0       : input real :  central plasma pressure (Pa)
  !+ad_args  qpsi     : input real :  plasma edge safety factor (= q-bar for icurr=2)
  !+ad_args  rmajor   : input real :  major radius (m)
  !+ad_args  rminor   : input real :  minor radius (m)
  !+ad_args  sf       : input real :  shape factor for icurr=1 (=A/pi in documentation)
  !+ad_args  triang   : input real :  plasma triangularity
  !+ad_args  triang95 : input real :  plasma triangularity at 95% surface
  !+ad_args  bp       : output real : poloidal field (T)
  !+ad_args  qstar    : output real : equivalent cylindrical safety factor (shaped)
  !+ad_args  plascur  : output real : plasma current (A)
  !+ad_desc  This routine performs the calculation of the
  !+ad_desc  plasma current, with a choice of formula for the edge
  !+ad_desc  safety factor.
  !+ad_prob  None
  !+ad_call  bpol
  !+ad_call  conhas
  !+ad_call  plasc
  !+ad_hist  20/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  29/01/96 PJK Added icurr=2 TART option
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: icurr
  real(kind(1.0D0)), intent(in) :: alphaj, alphap, bt, eps, kappa, &
       kappa95, p0, qpsi, rmajor, rminor, sf, triang, triang95
  real(kind(1.0D0)), intent(out) :: bp, qstar, plascur

  !  Local variables

  real(kind(1.0D0)) :: asp, curhat, fq

  !  External functions

  real(kind(1.0D0)), external :: bpol

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Aspect ratio

  asp = 1.0D0/eps

  !  Calculate the function Fq that scales the edge q from the
  !  circular cross-section cylindrical case.

  select case (icurr)

  case (1)  !  Peng analytical fit
     fq = (1.22D0-0.68D0*eps)/((1.0D0-eps*eps)**2) * sf**2

  case (2)  !  Peng scaling for double null divertor; TARTs
     curhat = 1.0D6 * plasc(qpsi,asp,rminor,bt,kappa,triang)/bt

  case (3)  !  Simple ITER scaling (simply the cylindrical case)
     fq = 1.0D0

  case (4)  !  ITER formula
     fq = 0.5D0 * (1.17D0-0.65D0*eps)/((1.0D0-eps*eps)**2) * &
          (1.0D0 + kappa95**2 * &
          (1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3) )

  case (5, 6) !  Todd empirical scalings

     fq = (1.0D0+2.0D0*eps*eps) * 0.5D0*(1.0D0+kappa95**2) * &
          (1.24D0-0.54D0*kappa95+0.3D0*(kappa95**2 + triang95**2) + &
          0.125D0*triang95)

     if (icurr == 6) fq = fq * (1.0D0 + ( abs(kappa95-1.2D0) )**3)

  case (7)  !  Connor-Hastie asymptotically-correct expression
     call conhas(alphaj,alphap,bt,triang,eps,kappa,p0,fq)

  case default
     write(*,*) 'Error in routine CULCUR:'
     write(*,*) 'Illegal value for ICURR, = ',icurr
     write(*,*) 'PROCESS stopping.'

  end select

  !  Calculate the ratio of plasma current to toroidal field

  if (icurr /= 2) then
     curhat = 5.0D6 * rminor**2 / (rmajor*qpsi) * fq
  end if

  !  Calculate the equivalent edge safety factor

  qstar = 5.0D6 * rminor**2 / (rmajor*curhat) * 0.5D0 * &
       (1.0D0 + kappa95**2 * &
       (1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3) )

  !  Calculate plasma current

  plascur = curhat * bt

  !  Calculate the poloidal field

  bp = bpol(qpsi,asp,bt,kappa,triang)

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function plasc(qbar,aspect,rminor,bt,kappa,delta)

    !+ad_name  plasc
    !+ad_summ  Function to calculate plasma current (Peng scaling)
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  aspect : input real :  plasma aspect ratio
    !+ad_args  bt     : input real :  toroidal field on axis (T)
    !+ad_args  delta  : input real :  plasma triangularity
    !+ad_args  kappa  : input real :  plasma elongation
    !+ad_args  qbar   : input real :  edge q-bar
    !+ad_args  rminor : input real :  plasma minor radius (m)
    !+ad_desc  This function calculates the plasma current in MA,
    !+ad_desc  using a scaling from M Peng's notes, 24 February 1989.
    !+ad_desc  It is primarily used for Tight Aspect Ratio Tokamaks and is
    !+ad_desc  selected via <CODE>icurr=2</CODE>.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: plasc

    !  Arguments

    real(kind(1.0D0)), intent(in) :: aspect,bt,delta,kappa,qbar,rminor

    !  Local variables

    real(kind(1.0D0)), parameter :: pi = 3.1415926536D0
    real(kind(1.0D0)) :: c1,c2,d1,d2,eps,e1,e2,f1,f2,ff1,ff2,g,h1,h2,y1,y2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    eps = 1.0D0/aspect

    c1 = kappa**2/(1.0D0+delta) + delta
    c2 = kappa**2/(1.0D0-delta) - delta

    d1 = (kappa/(1.0D0+delta))**2 + 1.0D0
    d2 = (kappa/(1.0D0-delta))**2 + 1.0D0

    if (aspect < c1) then
       y1 = sqrt( (c1*eps - 1.0D0)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
    else
       y1 = sqrt( (1.0D0 - c1*eps)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
    end if
    y2 = sqrt( (c2*eps+1.0D0)/(1.0D0-eps) ) * (1.0D0-delta)/kappa

    e1 = 2.0D0*kappa/(d1*(1.0D0+delta))
    e2 = 2.0D0*kappa/(d2*(1.0D0-delta))

    h2 = (1.0D0 + (c2-1.0D0)*eps/2.0D0) / &
         sqrt( (1.0D0-eps)*(c2*eps+1.0D0) )
    f2 = (d2*(1.0D0-delta)*eps) / ( (1.0D0-eps)*(c2*eps+1.0D0) )
    g = eps*kappa / (1.0D0 - eps*delta)
    ff2 = f2 * (g + 2.0D0*h2*atan(y2) )

    if (aspect < c1) then
       h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
            sqrt( (1.0D0+eps)*(c1*eps-1.0D0) )
       f1 = (d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
       ff1 = f1*(g - h1*log( (1.0D0+y1)/(1.0D0-y1) ) )
    else
       h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
            sqrt( (1.0D0+eps)*(1.0D0-c1*eps) )
       f1 = -(d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
       ff1 = f1*( -g + 2.0D0*h1*atan(y1) )
    end if

    plasc = rminor*bt/qbar * 5.0D0*kappa/(2.0D0*pi**2) * &
         ( asin(e1)/e1 + asin(e2)/e2 ) * (ff1 + ff2)

  end function plasc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine conhas(alphaj,alphap,bt,delta,eps,kappa,p0,fq)

    !+ad_name  conhas
    !+ad_summ  Routine to calculate the F coefficient used for scaling the
    !+ad_summ  plasma current
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  alphaj   : input real :  current profile index
    !+ad_args  alphap   : input real :  pressure profile index
    !+ad_args  bt       : input real :  toroidal field on axis (T)
    !+ad_args  delta    : input real :  plasma triangularity
    !+ad_args  eps      : input real :  inverse aspect ratio
    !+ad_args  kappa    : input real :  plasma elongation
    !+ad_args  p0       : input real :  central plasma pressure (Pa)
    !+ad_args  fq       : output real : scaling for edge q from circular
    !+ad_argc                           cross-section cylindrical case
    !+ad_desc  This routine calculates the F coefficient used for scaling the
    !+ad_desc  plasma current, using the Connor-Hastie scaling given in
    !+ad_desc  AEA FUS 172.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alphaj,alphap,bt,delta,eps,kappa,p0
    real(kind(1.0D0)), intent(out) :: fq

    !  Local variables

    real(kind(1.0D0)), parameter :: mu0 = 1.2566371D-6
    real(kind(1.0D0)) :: beta0, deltap, deltar, eprime, er, kap1, &
         lambda, lamp1, li, nu, tprime, tr

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Exponent in Connor-Hastie current profile - matching total
    !  current gives the following trivial relation

    lambda = alphaj

    !  Exponent in Connor-Hastie pressure profile

    nu = alphap

    !  Central plasma beta

    beta0 = 2.0D0 * mu0 * p0 / (bt**2)

    !  Plasma internal inductance

    lamp1 = 1.0D0 + lambda
    li = lamp1/lambda * (lamp1/lambda * log(lamp1) - 1.0D0)

    !  T/r in AEA FUS 172

    kap1 = kappa + 1.0D0
    tr = kappa * delta / kap1**2

    !  E/r in AEA FUS 172

    er = (kappa-1.0D0)/kap1

    !  T primed in AEA FUS 172

    tprime = 2.0D0 * tr * lamp1/(1.0D0 + 0.5D0*lambda)

    !  E primed in AEA FUS 172

    eprime = er * lamp1/(1.0D0 + lambda/3.0D0)

    !  Delta primed in AEA FUS 172

    deltap = 0.5D0*kap1 * eps * 0.5D0*li + &
         beta0/(0.5D0*kap1*eps) * lamp1**2 / (1.0D0+nu)

    !  Delta/R0 in AEA FUS 172

    deltar = beta0/6.0D0 * (1.0D0 + 5.0D0*lambda/6.0D0 + 0.25D0*lambda**2) &
         + (0.5D0*kap1*eps)**2 * 0.125D0*(1.0D0-(lambda**2)/3.0D0)

    !  F coefficient

    fq = (0.5D0*kap1)**2 * &
         ( 1.0D0 + eps**2 * (0.5D0*kap1)**2 + 0.5D0*deltap**2 + &
         2.0D0*deltar + 0.5D0*(eprime**2 + er**2) + &
         0.5D0*(tprime**2 + 4.0D0*tr**2) )

  end subroutine conhas

end subroutine culcur

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function bpol(qbar,aspect,bt,kappa,delta)

  !+ad_name  bpol
  !+ad_summ  Function to calculate poloidal field
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  qbar   : input real :  edge q-bar
  !+ad_args  aspect : input real :  plasma aspect ratio
  !+ad_args  bt     : input real :  toroidal field on axis (T)
  !+ad_args  kappa  : input real :  plasma elongation
  !+ad_args  delta  : input real :  plasma triangularity
  !+ad_desc  This function calculates the poloidal field in T,
  !+ad_desc  using a scaling from M Peng's notes, 24 February 1989.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: bpol

  !  Arguments

  real(kind(1.0D0)), intent(in) :: aspect,bt,delta,kappa,qbar

  !  Local variables

  real(kind(1.0D0)), parameter :: pi = 3.1415926536D0
  real(kind(1.0D0)) :: c1,c2,d1,d2,eps,e1,e2,f1,f2,ff1,ff2,g,h1,h2,y1,y2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  eps = 1.0D0/aspect

  c1 = kappa**2/(1.0D0+delta) + delta
  c2 = kappa**2/(1.0D0-delta) - delta

  d1 = (kappa/(1.0D0+delta))**2 + 1.0D0
  d2 = (kappa/(1.0D0-delta))**2 + 1.0D0

  if (aspect < c1) then
     y1 = sqrt( (c1*eps - 1.0D0)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
  else
     y1 = sqrt( (1.0D0 - c1*eps)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
  end if
  y2 = sqrt( (c2*eps+1.0D0)/(1.0D0-eps) ) * (1.0D0-delta)/kappa

  h2 = (1.0D0 + (c2-1.0D0)*eps/2.0D0) / &
       sqrt( (1.0D0-eps)*(c2*eps+1.0D0) )
  f2 = (d2*(1.0D0-delta)*eps) / ( (1.0D0-eps)*(c2*eps+1.0D0) )
  g = eps*kappa / (1.0D0 - eps*delta)
  ff2 = f2 * (g + 2.0D0*h2*atan(y2) )

  if (aspect < c1) then
     h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
          sqrt( (1.0D0+eps)*(c1*eps-1.0D0) )
     f1 = (d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
     ff1 = f1*(g - h1*log( (1.0D0+y1)/(1.0D0-y1) ) )
  else
     h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
          sqrt( (1.0D0+eps)*(1.0D0-c1*eps) )
     f1 = -(d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
     ff1 = f1*( -g + 2.0D0*h1*atan(y1) )
  end if

  bpol = bt * (ff1 + ff2) / (2.0D0 * pi * qbar)

end function bpol

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine culblm(bt,dnbeta,plascur,rminor,betalim)

  !+ad_name  culblm
  !+ad_summ  Troyon beta scaling limit
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  bt      : input real :  toroidal B-field on plasma axis (T)
  !+ad_args  dnbeta  : input real :  Troyon g coefficient
  !+ad_args  plascur : input real :  plasma current (A)
  !+ad_args  rminor  : input real :  plasma minor axis (m)
  !+ad_args  betalim : output real : beta limit as defined below
  !+ad_desc  This subroutine calculates the Troyon scaling beta limit, using
  !+ad_desc  the algorithm documented in AEA FUS 172.
  !+ad_desc  <P>The limit applies to beta defined with respect to the total B-field.
  !+ad_desc  Switch ICULBL determines which components of beta to include (see
  !+ad_desc  file eqns.f90 for coding):
  !+ad_desc  <UL>
  !+ad_desc  <P><LI>If ICULBL = 0, then the limit is applied to the total beta
  !+ad_desc  <P><LI>If ICULBL = 1, then the limit is applied to the thermal beta only
  !+ad_desc  <P><LI>If ICULBL = 2, then the limit is applied to the thermal +
  !+ad_desc                        neutral beam beta components
  !+ad_desc  </UL>
  !+ad_desc  The default value for the Troyon g coefficient is DNBETA = 3.5
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: bt, dnbeta, plascur, rminor
  real(kind(1.0D0)), intent(out) :: betalim

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  betalim = 0.01D0 * dnbeta * (plascur/1.0D6) / (rminor*bt)

end subroutine culblm

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine betcom(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr, &
     ftritbm,idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff, &
     abeam,afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot, &
     dnla,dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,zion)

  !+ad_name  betcom
  !+ad_summ  Calculates various plasma component fractional makeups
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphan : input real :  density profile index
  !+ad_args  alphat : input real :  temperature profile index
  !+ad_args  cfe0   : input real :  additional iron impurity fraction
  !+ad_args  dene   : input real :  electron density (/m3)
  !+ad_args  fdeut  : input real :  deuterium fraction of D-He3 fuel
  !+ad_args  ftrit  : input real :  tritium fraction of D-He3 fuel
  !+ad_args  fhe3   : input real :  helium-3 fraction of D-He3 fuel
  !+ad_args  ftr    : input real :  tritium fraction of D-T ions
  !+ad_args  ftritbm: input real :  tritium fraction of beam
  !+ad_args  idhe3  : input integer :  flag denoting whether to assume D-T or D-He3
  !+ad_args  ignite : input integer :  switch for ignited calculation
  !+ad_args  impc   : input real :  carbon impurity multiplier
  !+ad_args  impfe  : input real :  iron impurity multiplier
  !+ad_args  impo   : input real :  oxygen impurity multiplier
  !+ad_args  ralpne : input real :  thermal alpha density / electron density
  !+ad_args  rnbeam : input real :  hot beam density / electron density
  !+ad_args  te     : input real :  electron temperature (keV)
  !+ad_args  abeam  : output real : beam ion mass (amu)
  !+ad_args  afuel  : output real : average mass of fuel portion of ions (amu)
  !+ad_args  aion   : output real : average mass of all ions (amu)
  !+ad_args  deni   : output real : fuel ion density (/m3)
  !+ad_args  dlamee : output real : electron-electron coulomb logarithm
  !+ad_args  dlamie : output real : ion-electron coulomb logarithm
  !+ad_args  dnalp  : output real : alpha ash density (/m3)
  !+ad_args  dnbeam : output real : hot beam ion density (/m3)
  !+ad_args  dnitot : output real : total ion density (/m3)
  !+ad_args  dnla   : output real : line-averaged electron density (/m3)
  !+ad_args  dnprot : output real : proton ash density (/m3)
  !+ad_args  dnz    : output real : high Z ion density (/m3)
  !+ad_args  falpe  : output real : fraction of alpha energy to electrons
  !+ad_args  falpi  : output real : fraction of alpha energy to ions
  !+ad_args  pcoef  : output real : profile factor (= average T / n-weighted T)
  !+ad_args  rncne  : output real : carbon density / electron density
  !+ad_args  rnfene : output real : iron density / electron density
  !+ad_args  rnone  : output real : oxygen density / electron density
  !+ad_args  zeff   : output real : plasma effective charge
  !+ad_args  zeffai : output real : density weighted plasma effective charge
  !+ad_args  zion   : output real : density weighted charge
  !+ad_desc  This subroutine determines the various plasma component
  !+ad_desc  fractional makeups.
  !+ad_prob  None
  !+ad_call  gamfun
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  06/12/95 PJK Added D-He3 calculations
  !+ad_hist  01/04/98 PJK Added calculation of line-averaged density
  !+ad_hisc               and effects of IGNITE
  !+ad_hist  24/04/98 PJK Added IMPC, IMPFE, IMPO impurity multipliers
  !+ad_hist  23/05/06 PJK Ensured that deni is positive
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  F/MI/PJK/LOGBOOK11, p.38 for D-He3 deni calculation
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: idhe3, ignite
  real(kind(1.0D0)), intent(in) :: alphan, alphat, cfe0, dene, fdeut, &
       ftrit, fhe3, ftr, ftritbm, impc, impfe, impo, ralpne, rnbeam, te
  real(kind(1.0D0)), intent(out) :: abeam, afuel, aion, deni, dlamee, &
       dlamie, dnalp, dnbeam, dnitot, dnla, dnprot, dnz, falpe, falpi, &
       pcoef, rncne, rnfene, rnone, zeff, zeffai, zion

  !  Local variables

  real(kind(1.0D0)) :: fc, ffe, fo, znfuel

  !  External functions

  real(kind(1.0D0)), external :: gamfun

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Profile factor

  pcoef = (1.0D0 + alphan)*(1.0D0 + alphat)/(1.0D0+alphan+alphat)

  !  Line averaged electron density

  dnla = 0.886227D0 * dene * gamfun(alphan+1.0D0) / &
       gamfun(alphan+1.5D0) * (1.0D0+alphan)

  !  Ion density components
  !  ======================

  !  Alpha ash portion

  dnalp = dene * ralpne

  !  Proton ash portion

  if (idhe3 == 1) dnprot = dnalp

  !  Beam hot ion component
  !  If ignited, prevent beam fusion effects

  if (ignite == 0) then
     dnbeam = dene * rnbeam
  else
     dnbeam = 0.0D0
  end if

  !  Carbon portion

  fc = impc * (0.009D0 + 0.006D0 * (7.0D19/dene)**2.6D0)
  rncne = fc

  !  Oxygen portion

  fo = impo * 0.001D0
  rnone = fo

  !  Iron portion

  ffe = impfe * (0.0005D0 * (7.0D19/dene)**2.3D0 + cfe0)
  rnfene = ffe

  !  Fuel portion - conserve charge neutrality
  !  znfuel is the sum of Zi.ni for the three fuel ions

  znfuel = dene - 2.0D0*dnalp - dnprot - dnbeam - &
       dene*(6.0D0*fc + 8.0D0*fo + 26.0D0*ffe)

  if (idhe3 == 0) then
     deni = znfuel
  else
     deni = znfuel/(1.0D0+fhe3)
  end if

  !  Ensure that deni is never negative or zero

  deni = max(deni,1.0D0)

  !  Total ion density

  dnz = dene * (fc + fo + ffe)
  dnitot = deni + dnz + dnalp + dnprot + dnbeam

  !  Effective charge

  if (idhe3 == 0) then
     zeff = (deni + dnbeam)/dene + 4.0D0*ralpne + 36.0D0*fc + &
          64.0D0*fo + 676.0D0*ffe
  else
     zeff = (fdeut + ftrit)*deni/dene + 4.0D0*fhe3*deni/dene + &
          dnbeam/dene + 4.0D0*ralpne + dnprot/dene + 36.0D0*fc + &
          64.0D0*fo + 676.0D0*ffe
  end if

  !  Define coulomb logarithm
  !  (collisions: ion-electron, electron-electron)

  dlamee = 31.0D0 - (log(dene)/2.0D0) + log(te*1000.0D0)
  dlamie = 31.3D0 - (log(dene)/2.0D0) + log(te*1000.0D0)

  !  Fraction of alpha energy to ions and electrons
  !  From Max Fenstermacher
  !  (used with electron and ion power balance equations only)
  !  No consideration of pcharge here...

  falpe = 0.88155D0 * exp(-te*pcoef/67.4036D0)
  falpi = 1.0D0 - falpe

  !  Average atomic masses

  if (idhe3 == 0) then
     if (ftr < 1.0D-3) then
        afuel = 1.0D0
        abeam = 1.0D0
     else
        afuel = 2.0D0 * (1.0D0-ftr)     + 3.0D0 * ftr
        abeam = 2.0D0 * (1.0D0-ftritbm) + 3.0D0 * ftritbm
     end if
  else
     afuel = 2.0D0*fdeut + 3.0D0*ftrit + 3.0D0*fhe3
     abeam = 2.0D0 * (1.0D0-ftritbm) + 3.0D0 * ftritbm
  end if

  !  Density weighted masses and charges

  if (idhe3 == 0) then

     !  aion = sum over ions: A_i . n_i / total ion density

     aion = ( afuel*deni + 4.0D0*dnalp + dnprot + abeam*dnbeam + &
          dene*(6.0D0*fc + 8.0D0*fo + 28.0D0*ffe) )/ dnitot

     !+**PJK 15/01/96 Possibly more correct...
     !+**PJK 15/01/96 aion = ( afuel*deni + 4.0D0*dnalp + dnprot +
     !+**PJK 15/01/96  +  abeam*dnbeam + dene*(12.0D0*fc + 16.0D0*fo +
     !+**PJK 15/01/96  +  56.0D0*ffe) )/ dnitot

     !  zion = sum over ions: Z_i . n_i / total ion density

     zion = ( deni + 2.0D0*dnalp + dnprot + dnbeam + &
          dene*(6.0D0*fc + 8.0D0*fo + 26.0D0*ffe) )/dnitot

     !  zeffai = sum over ions: (Z_i)**2 . n_i / (A_i . n_e)

     zeffai = ( deni/afuel + dnalp + dnprot + dnbeam/abeam + &
          dene*(6.0D0*fc + 8.0D0*fo + 24.0D0*ffe) ) / dene

     !+**PJK 15/01/96 Possibly more correct...
     !+**PJK 15/01/96 zeffai = ( (1.0D0-ftr)*deni/2.0D0 + ftr*deni/3.0D0 +
     !+**PJK 15/01/96  +  dnalp + dnprot + (1.0D0-ftritbm)*dnbeam/2.0D0 +
     !+**PJK 15/01/96  +  ftritbm*dnbeam/3.0D0 + dene*(3.0D0*fc + 4.0D0*fo +
     !+**PJK 15/01/96  +  12.0D0*ffe) )/dene

  else

     aion = ( afuel*deni + 4.0D0*dnalp + dnprot + abeam*dnbeam + &
          dene*(12.0D0*fc + 16.0D0*fo + 56.0D0*ffe) )/ dnitot

     zion = ( fdeut*deni + ftrit*deni + 2.0D0*fhe3*deni + &
          2.0D0*dnalp + dnprot + dnbeam + dene * &
          (6.0D0*fc + 8.0D0*fo + 26.0D0*ffe) )/dnitot

     zeffai = ( fdeut*deni/2.0D0 + ftrit*deni/3.0D0 + &
          4.0D0*fhe3*deni/3.0D0 + dnalp + dnprot + &
          (1.0D0-ftritbm)*dnbeam/2.0D0 + ftritbm*dnbeam/3.0D0 + &
          dene*(3.0D0*fc + 4.0D0*fo + 12.0D0*ffe) )/dene

  end if

end subroutine betcom

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine denlim(bt,rmajor,prn1,pdiv,q,sarea,dnelimt)

  !+ad_name  denlim
  !+ad_summ  Calculates Borrass density limit
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  bt      : input real :  toroidal field on axis (T)
  !+ad_args  rmajor  : input real :  major radius (m)
  !+ad_args  prn1    : input real :  edge density / average plasma density
  !+ad_args  pdiv    : input real :  power flowing to the edge plasma (MW)
  !+ad_args  q       : input real :  safety factor at 95% surface
  !+ad_args  sarea   : input real :  plasma surface area (m**2)
  !+ad_args  dnelimt : output real : average plasma density limit (m**-3)
  !+ad_desc  This subroutine calculates the density limit using the
  !+ad_desc  Borrass model.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: bt, rmajor, prn1, pdiv, q, sarea
  real(kind(1.0D0)), intent(out) :: dnelimt

  !  Local variables

  real(kind(1.0D0)) :: denslim, qparll

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  qparll = pdiv/sarea
  denslim = 0.5D20 * qparll**0.57D0 * bt**0.31D0 / (q*rmajor)**0.1D0

  dnelimt = denslim/prn1

end subroutine denlim

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine culdlm(bt,idensl,pdivt,pi,plascur,prn1,qcyl,q95, &
     rmajor,rminor,sarea,zeff,dlimit,dnelimt)

  !+ad_name  culdlm
  !+ad_summ  Density limit calculation
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  bt       : input real :  toroidal field on axis (T)
  !+ad_args  idensl   : input/output integer : switch denoting which formula to enforce
  !+ad_args  pdivt    : input real :  power flowing to the edge plasma (MW)
  !+ad_args  pi       : input real :  3.14...
  !+ad_args  plascur  : input real :  plasma current (A)
  !+ad_args  prn1     : input real :  edge density / average plasma density
  !+ad_args  qcyl     : input real :  equivalent cylindrical safety factor (qstar)
  !+ad_args  q95      : input real :  safety factor at 95% surface
  !+ad_args  rmajor   : input real :  plasma major radius (m)
  !+ad_args  rminor   : input real :  plasma minor radius (m)
  !+ad_args  sarea    : input real :  plasma surface area (m**2)
  !+ad_args  zeff     : input real :  plasma effective charge
  !+ad_args  dlimit(7): output real array : average plasma density limit using
  !+ad_argc                                 seven different models (m**-3)
  !+ad_args  dnelimt  : output real : enforced average plasma density limit (m**-3)
  !+ad_desc  This routine calculates several different formulae for the
  !+ad_desc  density limit, and enforces the one chosen by the user.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  17/09/97 PJK Added Greenwald limit (idensl=7)
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(inout) :: idensl
  real(kind(1.0D0)), intent(in) :: bt, pdivt, pi, plascur, prn1, q95, &
       qcyl, rmajor, rminor, sarea, zeff
  real(kind(1.0D0)), intent(out) :: dnelimt
  real(kind(1.0D0)), dimension(7), intent(out) :: dlimit

  !  Local variables

  real(kind(1.0D0)) :: denom, dlim, qperp

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Check for illegal values of IDENSL

  if ((idensl < 1).or.(idensl > 7)) then
     write(*,*) 'Error in routine CULDLM:'
     write(*,*) 'Illegal value for IDENSL, = ',idensl
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  dlimit(:) = 0.0D0

  !  Power per unit area crossing the plasma edge

  qperp = pdivt/sarea

  !  Old ASDEX density limit formula
  !  This applies to the density at the plasma edge, so must be scaled
  !  to give the density limit applying to the average plasma density.

  dlim = 1.54D20 * qperp**0.43D0 * bt**0.31D0 /(q95*rmajor)**0.45D0
  dlimit(1) = dlim/prn1

  !  Borrass density limit model for ITER (I)
  !  This applies to the density at the plasma edge, so must be scaled
  !  to give the density limit applying to the average plasma density.

  dlim = 1.8D20 * qperp**0.53D0 * bt**0.31D0 /(q95*rmajor)**0.22D0
  dlimit(2) = dlim/prn1

  !  Borrass density limit model for ITER (II)
  !  This applies to the density at the plasma edge, so must be scaled
  !  to give the density limit applying to the average plasma density.

  dlim = 0.5D20 * qperp**0.57D0 * bt**0.31D0 /(q95*rmajor)**0.09D0
  dlimit(3) = dlim/prn1

  !  JET edge radiation density limit model
  !  This applies to the density at the plasma edge, so must be scaled
  !  to give the density limit applying to the average plasma density.
  !  qcyl=qstar here, but literature is not clear.

  denom = (zeff-1.0D0)*( 1.0D0-4.0D0/(3.0D0*qcyl) )
  if (denom <= 0.0D0) then
     if (idensl == 4) then
        write(*,*) 'Warning in routine CULDLM:'
        write(*,*) 'DENOM = ',denom,' i.e. qcyl < 4/3'
        write(*,*) 'Floating point error will occur.'
        write(*,*) 'DLIMIT(4) is artificially set to 0.0.'
        write(*,*) 'Model 5 will be used as the density limit.'
        write(*,*) 'PROCESS continuing.'
        idensl = 5
     end if
     dlimit(4) = 0.0D0
  else
     dlim = 1.0D20 * sqrt(pdivt/denom)
     dlimit(4) = dlim/prn1
  end if

  !  JET simplified density limit model
  !  This applies to the density at the plasma edge, so must be scaled
  !  to give the density limit applying to the average plasma density.

  dlim = 0.237D20 * bt * sqrt(pdivt)/rmajor
  dlimit(5) = dlim/prn1

  !  Hugill-Murakami M.q limit
  !  qcyl=qstar here, which is okay according to the literature

  dlimit(6) = 3.0D20 * bt / (rmajor*qcyl)

  !  Greenwald limit

  dlimit(7) = 1.0D14 * plascur/(pi*rminor*rminor)

  !  Enforce the chosen density limit

  dnelimt = dlimit(idensl)

end subroutine culdlm

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit, &
     idhe3,iiter,pcoef,pi,ti,palp,pcharge,pneut,sigvdt)

  !+ad_name  palph
  !+ad_summ  (Initial part of) fusion power and fast alpha pressure calculations
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphan  : input real :  density profile index
  !+ad_args  alphat  : input real :  temperature profile index
  !+ad_args  deni    : input real :  fuel ion density (/m3)
  !+ad_args  ealpha  : input real :  alpha particle birth energy (D-T) (keV)
  !+ad_args  fdeut   : input real :  deuterium fuel fraction
  !+ad_args  fhe3    : input real :  helium-3 fuel fraction
  !+ad_args  ftr     : input real :  tritium fraction of D-T ions
  !+ad_args  ftrit   : input real :  tritium fuel fraction
  !+ad_args  idhe3   : input integer :  switch for D-T or D-He3 calculations
  !+ad_args  iiter   : input integer :  switch for ITER fusion power calculations
  !+ad_args  pcoef   : input real :  profile factor (= average T / n-weighted T)
  !+ad_args  pi      : input real :  3.14...
  !+ad_args  ti      : input real :  ion temperature (keV)
  !+ad_args  palp    : output real : alpha particle fusion power (MW/m3)
  !+ad_args  pcharge : output real : other charged particle fusion power (MW/m3)
  !+ad_args  pneut   : output real : neutron fusion power (MW/m3)
  !+ad_args  sigvdt  : output real : profile averaged <sigma v DT> (m3/s)
  !+ad_desc  This subroutine numerically integrates over plasma cross-section to
  !+ad_desc  find the fusion power and fast alpha pressure.
  !+ad_prob  None
  !+ad_call  ffus
  !+ad_call  fint
  !+ad_call  fpower
  !+ad_call  quanc8
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  06/12/95 PJK Added D-He3 calculations
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: idhe3, iiter
  real(kind(1.0D0)), intent(in) :: alphan, alphat, deni, ealpha, fdeut, &
       fhe3, ftr, ftrit, pcoef, pi, ti
  real(kind(1.0D0)), intent(out) :: palp, pcharge, pneut, sigvdt

  !  Variables passed via COMMON

  !  alphand: (OUTPUT) density profile index
  !  alphatd: (OUTPUT) temperature profile index
  !  tidum  : (OUTPUT) ion temperature (keV)

  real(kind(1.0D0)) :: alphatd, alphand, tidum
  common/fintcom/tidum,alphatd,alphand

  !  Local variables

  integer :: iopt, nofun
  real(kind(1.0D0)) :: alow, bhigh, ealphaj, epsq8, errest, flag, ft, &
       rint, svdt, tn, pa, pc, pn, sigmav

  !  External functions

  real(kind(1.0D0)), external :: fint

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise local quantities

  alow = 0.0D0
  bhigh = 0.999D0
  epsq8 = 1.0D-9

  ealphaj = ealpha * 1.6022D-16  !  Alpha birth energy (Joules)

  !  Initialise shared quantities

  tidum = ti
  alphatd = alphat
  alphand = alphan

  !  Find fusion power

  if (idhe3 == 0) then

     if (iiter /= 1) then
        !  Integrate over plasma profiles to obtain fusion reaction rate
        call quanc8(fint,alow,bhigh,epsq8,epsq8,rint,errest,nofun,flag)
        sigvdt = 2.0D0 * (1.0D0 + alphan)**2 * rint
     else
        !  Use analytical formula for fusion reaction rate
        tn = ti * pcoef
        call ffus(tn,alphan,alphat,ft,svdt)
        sigvdt = svdt * ft
     end if

     palp = sigvdt * ftr*(1.0D0-ftr) * ealphaj * 1.0D-6 * deni**2
     pcharge = 0.0D0
     pneut = 4.0D0 * palp

  else  !  D-He3 option

     !  Sum over the four reactions: D-T, D-He3, first D-D, second D-D
     !  No profile averaging yet...

     palp = 0.0D0 ; pcharge = 0.0D0 ; pneut = 0.0D0
     do iopt = 1,4
        call fpower(ftrit,fdeut,fhe3,deni,ti,pi,iopt,pa,pc,pn,sigmav)
        palp = palp + pa
        pcharge = pcharge + pc
        pneut = pneut + pn
        if (iopt == 1) sigvdt = sigmav
     end do

  end if

end subroutine palph

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function fint(xy)

  !+ad_name  fint
  !+ad_summ  Integrand for alpha power integration
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  xy : input real :  Abscissa of the integration, = normalised
  !+ad_argc                     plasma minor radius (0.0 <= xy < 1.0)
  !+ad_desc  This function evaluates the integrand for the alpha power
  !+ad_desc  integration, performed using routine <A HREF="quanc8.html">QUANC8</A>
  !+ad_desc  in routine <A HREF="palph.html">PALPH</A>.
  !+ad_prob  None
  !+ad_call  svfdt
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: fint

  !  Arguments

  real(kind(1.0D0)), intent(in) :: xy

  !  Variables passed via COMMON

  !  alphand: (INPUT) density profile index
  !  alphatd: (INPUT) temperature profile index
  !  tidum  : (INPUT) ion temperature (keV)

  real(kind(1.0D0)) :: alphatd, alphand, tidum
  common/fintcom/tidum,alphatd,alphand

  !  Local variables

  real(kind(1.0D0)) :: dxy, sigv, tiofr

  !  External functions

  real(kind(1.0D0)), external :: svfdt

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!+PJK Would be good to have this function 'contained' within palph
!+PJK but the results are changed subtly

  tiofr = tidum*(1.0D0+alphatd)*(1.0D0-(xy**2))**alphatd
  sigv = svfdt(tiofr)
  dxy = (1.0D0-(xy**2))**alphand

  fint = xy*(dxy**2)*sigv

end function fint

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine fpower(ftrit,fdeut,fhe3,deni,ti,pi,iopt,palp,pcharge, &
     pneut,sigmav)

  !+ad_name  fpower
  !+ad_summ  Fusion power calculation
  !+ad_type  Subroutine
  !+ad_auth  M R Gardner, UKAEA Fusion
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  pade
  !+ad_args  ftrit   : input real :  tritium fuel fraction
  !+ad_args  fdeut   : input real :  deuterium fuel fraction
  !+ad_args  fhe3    : input real :  helium-3 fuel fraction
  !+ad_args  deni    : input real :  fuel ion number density (/m3)
  !+ad_args  ti      : input real :  ion temperature (keV)
  !+ad_args  pi      : input real :  3.14...
  !+ad_args  iopt    : input integer : flag for fusion reaction to use:
  !+ad_argc                            1 : D-T reaction
  !+ad_argc                            2 : D-3He reaction
  !+ad_argc                            3 : D-D 1st reaction (50% probability)
  !+ad_argc                            4 : D-D 2nd reaction (50% probability)
  !+ad_args  palp    : output real : alpha particle fusion power (MW/m3)
  !+ad_args  pcharge : output real : other charged particle fusion power (MW/m3)
  !+ad_args  pneut   : output real : neutron fusion power (MW/m3)
  !+ad_args  sigmav  : output real : fusion reaction rate (m3/s)
  !+ad_desc  This routine calculates the fusion power given to the alpha
  !+ad_desc  particles, neutrons and charged particles generated in one of
  !+ad_desc  four fusion processes, selected via <CODE>iopt</CODE>.
  !+ad_desc  <P>It is assumed that alphas and the protons give up all their energy
  !+ad_desc  to the plasma.
  !+ad_desc  The values are quoted to be valid for ti = 0 to 100 keV.
  !+ad_prob  None
  !+ad_call  pade
  !+ad_hist  20/04/95 MRG Initial version (ypbeta.f)
  !+ad_hist  04/05/95 MRG Added N_LOCAL and T_LOCAL to arguments
  !+ad_hist  05/12/95 PJK Modified for inclusion into PROCESS code
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  'Fusion cross sections and thermonuclear reaction rates',
  !+ad_docc  Asher Peres, J.Applied Physics 50(9), pp.5569-71, September 1979
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: iopt
  real(kind(1.0D0)), intent(in) :: ftrit,fdeut,fhe3,deni,ti,pi
  real(kind(1.0D0)), intent(out) :: palp,pcharge,pneut,sigmav

  !  Local variables

  real(kind(1.0D0)) :: const,eta,num1,num2,reactrt,theta,wht,etot
  real(kind(1.0D0)) :: mc2,p1,p2,p3,p4,p5,p6,fuspow,ch2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  const is the fine structure constant x pi

  const = pi * 7.2974D-3

  !  Check to see if ion temperature is zero

  if (ti == 0.0D0) then
     write(*,*) 'Error in routine FPOWER:'
     write(*,*) 'Ion temperature is zero.'
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  Check to see if ion temperature exceeds 200 keV for D-3He option

  if ((ti > 200.0D0).and.(iopt == 2)) then
     write(*,*) 'Error in routine FPOWER:'
     write(*,*) 'Ion temperature has exceeded 200 keV.'
     write(*,*) 'Reactivity D-3He equation is no longer valid.'
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  wht is the the probability weighting
  !  etot is the total fusion energy produced (J)
  !  ch2 is the product of the two reactant charges

  select case (iopt)

  case (1)  !  D + T --> 4He + n reaction

     num1 = fdeut*deni
     num2 = ftrit*deni
     ch2 = 1.0D0
     wht = 1.0D0
     etot = 17.58D6 * 1.6022D-19
     mc2 = 1124656.0D0
     p1 = 9.494748D-10
     p2 = 2.818421D-2
     p3 = 6.166184D-2
     p4 = 2.834474D-3
     p5 = 8.955113D-3
     p6 = -5.734052D-5

  case (2)  !  D + 3He --> 4He + p reaction

     num1 = fdeut*deni
     num2 = fhe3*deni
     ch2 = 2.0D0
     wht = 1.0D0
     etot = 18.34D6 * 1.6022D-19
     mc2 = 1124572.0D0
     p1 = 5.817887D-10
     p2 = 8.681070D-3
     p3 = 1.010217D-3
     p4 = -3.516677D-6
     p5 = 3.671273D-4
     p6 = 0.0D0

  case (3)  !  D + D --> 3He + n reaction

     num1 = fdeut*deni/2.0D0
     num2 = num1
     ch2 = 1.0D0
     wht = 0.5D0
     etot = 3.28D6 * 1.6022D-19
     mc2 = 937814.0D0
     p1 = 5.397811D-12
     p2 = 3.328985D-3
     p3 = 2.948735D-3
     p4 = 0.0D0
     p5 = 0.0D0
     p6 = 0.0D0

  case (4)  !  D + D --> T + p reaction

     num1 = fdeut*deni/2.0D0
     num2 = num1
     ch2 = 1.0D0
     wht = 0.5D0
     etot = 4.03D6 * 1.6022D-19
     mc2 = 937814.0D0
     p1 = 5.988513D-12
     p2 = 2.543079D-3
     p3 = 2.216964D-3
     p4 = 0.0D0
     p5 = 0.0D0
     p6 = 0.0D0

  case default
     write(*,*) 'Error in routine FPOWER:'
     write(*,*) 'IOPT is out of range, = ',iopt
     write(*,*) 'PROCESS stopping.'
     stop

  end select

  theta = pade(ti,p2,p3,p4,p5,p6)

  eta = (ch2**2 * const**2 * mc2 /(2.0D0*theta))**(0.333333333D0)

  !  Reaction rate <sigma.v> (cm**3 / sec)

  reactrt = p1 * theta * sqrt(eta/(mc2 * ti**3)) * exp(-3.0D0*eta)

  !  Convert to m3/s

  sigmav = reactrt * 1.0D-6

  !  Total fusion power (MW/m3)

  fuspow = 1.0D-12 * reactrt * etot * num1 * num2 * wht

  !  Split this power into the various particle powers

  select case (iopt)
  case (1)
     palp = 0.2D0 * fuspow
     pneut = 0.8D0 * fuspow
     pcharge = 0.0D0
  case (2)
     palp = 0.2D0 * fuspow
     pneut = 0.0D0
     pcharge = 0.8D0 * fuspow
  case (3)
     palp = 0.0D0
     pneut = 0.75D0 * fuspow
     pcharge = 0.25D0 * fuspow
  case (4)
     palp = 0.0D0
     pneut = 0.0D0
     pcharge = 1.0D0 * fuspow
  end select

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function pade(t,p2,p3,p4,p5,p6)

    !+ad_name  pade
    !+ad_summ  Pade approximant used by routine FPOWER
    !+ad_type  Function returning real
    !+ad_auth  M R Gardner, UKAEA Fusion
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  t  : input real : local temperature (keV)
    !+ad_args  p2 : input real : parameter
    !+ad_args  p3 : input real : parameter
    !+ad_args  p4 : input real : parameter
    !+ad_args  p5 : input real : parameter
    !+ad_args  p6 : input real : parameter
    !+ad_desc  This routine calculates the Pade approximant used by
    !+ad_desc  routine <A HREF="fpower.html">FPOWER</A> in calculating
    !+ad_desc  the fusion reaction rate.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/12/95 PJK Modified for inclusion into PROCESS code
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  'Fusion cross sections and thermonuclear reaction rates',
    !+ad_docc  Asher Peres, J.Applied Physics 50(9), pp.5569-71, September 1979
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: pade

    !  Arguments

    real(kind(1.0D0)), intent(in) :: t,p2,p3,p4,p5,p6

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    pade = t / ( 1.0D0 - t*( p2 + t*(p4 + t*p6) ) / &
         ( 1.0D0 + t*(p3 + t*p5) ) )

  end function pade

end subroutine fpower

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb, &
     ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft,palp,palpi, &
     palpe,pfuscmw,powfmw)

  !+ad_name  palph2
  !+ad_summ  (Concluding part of) fusion power and fast alpha pressure
  !+ad_summ  calculations
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  bp       : input real :  poloidal field (T)
  !+ad_args  bt       : input real :  toroidal field on axis (T)
  !+ad_args  dene     : input real :  electron density (/m3)
  !+ad_args  deni     : input real :  fuel ion density (/m3)
  !+ad_args  dnitot   : input real :  total ion density (/m3)
  !+ad_args  falpe    : input real :  fraction of alpha energy to electrons
  !+ad_args  falpi    : input real :  fraction of alpha energy to ions
  !+ad_args  ftr      : input real :  tritium D-T fraction
  !+ad_args  ifalphap : input integer :  switch for fast alpha pressure method
  !+ad_args  palpnb   : input real :  alpha power from hot neutral beam ions (MW)
  !+ad_args  pcharge  : input real :  other charged particle fusion power (MW/m3)
  !+ad_args  pcoef    : input real :  profile factor ( = average T / n-weighted T )
  !+ad_args  pneut    : input/output real neutron fusion power (MW/m3)
  !+ad_args  te       : input real :  electron temperature (keV)
  !+ad_args  ti       : input real :  ion temperature (keV)
  !+ad_args  vol      : input real :  plasma volume (m3)
  !+ad_args  alpmw    : output real : alpha power (MW)
  !+ad_args  betaft   : output real : fast alpha beta component
  !+ad_args  palp     : input/output real : alpha power per volume (MW/m3)
  !+ad_args  palpe    : output real : alpha power per volume to electrons (MW/m3)
  !+ad_args  palpi    : output real : alpha power per volume to ions (MW/m3)
  !+ad_args  pfuscmw  : output real : charged particle fusion power (MW)
  !+ad_args  powfmw   : output real : fusion power (MW)
  !+ad_desc  This subroutine completes the calculation of the fusion power
  !+ad_desc  fast alpha pressure, and determines other alpha particle quantities.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  06/12/95 PJK Added D-He3 calculations
  !+ad_hist  22/05/06 PJK Added modified fit to fast alpha pressure
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: ifalphap
  real(kind(1.0D0)), intent(in) :: bp, bt, dene, deni, dnitot, falpe, &
       falpi, ftr, palpnb, pcharge, pcoef, te, ti, vol
  real(kind(1.0D0)), intent(inout) :: palp, pneut
  real(kind(1.0D0)), intent(out) :: alpmw, betaft, palpe, palpi, &
       pfuscmw, powfmw

  !  Local variables

  real(kind(1.0D0)) :: betath, fact, fact2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Add neutral beam alpha power / volume

  palp = palp + palpnb/vol

  !  Add extra neutron power

  pneut = pneut + 4.0D0*palpnb/vol

  !  Total alpha power

  alpmw = palp*vol

  !  Total fusion power

  powfmw = alpmw + (pneut + pcharge)*vol

  !  Charged particle fusion power

  pfuscmw = alpmw + pcharge*vol

  !  Power to electrons and ions (used with electron and ion power balance
  !  equations only)
  !  No consideration of pcharge here...

  palpi = palp*falpi
  palpe = palp*falpe

  !  Determine average fast alpha density

  betath = 4.021D-22 * pcoef * (dene*te + dnitot*ti)/(bt**2 + bp**2)

  if (ifalphap == 0) then
     !  N Uckan fast alpha scaling
     fact = min( 0.30D0, &
          0.29D0*(deni/dene)**2 * ( pcoef*(te+ti)/20.0D0 - 0.37D0) )
  else
     !  Modified scaling, D J Ward, April 2006
     fact = min( 0.30D0, &
          0.26D0*(deni/dene)**2 * &
          sqrt( max(0.0D0, (pcoef*(te+ti)/20.0D0 - 0.65D0)) ) )
  end if

  fact = max(fact,0.0D0)
  if (ftr < 1.0D-3) fact = 0.0D0

  fact2 = palp/(palp-(palpnb/vol))
  betaft = betath * fact*fact2

end subroutine palph2

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ffus(t,alphan,alphat,ft,sigv)

  !+ad_name  ffus
  !+ad_summ  D-T fusion reaction rate
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphan : input real :  density profile index
  !+ad_args  alphat : input real :  temperature profile index
  !+ad_args  t      : input real :  weighted ion temperature (keV)
  !+ad_args  ft     : output real : ?
  !+ad_args  sigv   : output real : fusion reaction rate
  !+ad_desc  This subroutine calculates the D-T fusion reaction rate.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: alphan, alphat, t
  real(kind(1.0D0)), intent(out) :: ft, sigv

  !  Local variables

  real(kind(1.0D0)) :: c1, c2, y

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  c1 = -0.395D0 + 1.128D0*alphan + 3.777D0*alphat - &
       1.022D0*alphat*alphan

  c2 = 0.009D0 - 0.023D0*alphan - 0.385D0*alphat + &
       0.15D0*alphan*alphat

  ft = c1 * t**c2

  y = log(t)
  sigv = 0.977D-22 * &
       exp(0.0382D0*y**3 - 1.007D0*y**2 + 6.398D0*y - 9.75D0)

end subroutine ffus

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
     iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
     plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol, &
     xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

  !+ad_name  pcond
  !+ad_summ  Routine to calculate the confinement times and
  !+ad_summ  the transport power loss terms.
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  afuel  : input real :  average mass of fuel (amu)
  !+ad_args  alpmw  : input real :  alpha particle power (MW)
  !+ad_args  aspect : input real :  aspect ratio
  !+ad_args  bt     : input real :  toroidal field on axis (T)
  !+ad_args  dene   : input real :  volume averaged electron density (m**-3)
  !+ad_args  dnitot : input real :  total ion density (m**-3)
  !+ad_args  dnla   : input real :  line-averaged electron density (m**-3)
  !+ad_args  eps    : input real :  inverse aspect ratio
  !+ad_args  hfact  : input real :  H factor on energy confinement scalings
  !+ad_args  iinvqd : input integer :  switch for inverse quadrature
  !+ad_args  isc    : input integer :  switch for energy confinement scaling to use
  !+ad_args  ignite : input integer :  switch for ignited calculation
  !+ad_args  kappa  : input real :  plasma elongation
  !+ad_args  kappa95: input real :  plasma elongation at 95% surface
  !+ad_args  kappaa:  output real : plasma elongation calculated using area ratio
  !+ad_args  pcharge: input real :  non-alpha charged particle fusion power (MW/m3)
  !+ad_args  pinje  : input real :  auxiliary power to electrons (W)
  !+ad_args  pinji  : input real :  auxiliary power to ions (W)
  !+ad_args  plascur: input real :  plasma current (A)
  !+ad_args  pohmpv : input real :  ohmic heating per unit volume (MW/m**3)
  !+ad_args  prad   : input real :  total core radiation power (MW/m**3)
  !+ad_args  q      : input real :  edge safety factor
  !+ad_args  qstar  : input real :  equivalent cylindrical edge safety factor
  !+ad_args  rmajor : input real :  plasma major radius (m)
  !+ad_args  rminor : input real :  plasma minor radius (m)
  !+ad_args  te     : input real :  average electron temperature (keV)
  !+ad_args  ten    : input real :  density weighted average electron temp. (keV)
  !+ad_args  tin    : input real :  density weighted average ion temperature (keV)
  !+ad_args  vol    : input real :  plasma volume (m**3)
  !+ad_args  xarea  : input real :  plasma cross-sectional area (m**2)
  !+ad_args  zeff   : input real :  plasma effective charge
  !+ad_args  ptre   : output real : electron transport power (MW/m**3)
  !+ad_args  ptri   : output real : ion transport power (MW/m**3)
  !+ad_args  tauee  : output real : electron energy confinement time (s)
  !+ad_args  taueff : output real : global energy confinement time (s)
  !+ad_args  tauei  : output real : ion energy confinement time (s)
  !+ad_args  powerht: output real : heating power (MW) assumed in calculation
  !+ad_desc  This subroutine calculates the energy confinement time
  !+ad_desc  using one of a large number of scaling laws, and the
  !+ad_desc  transport power loss terms.
  !+ad_prob  Converting from IF to CASE changes the resulting machine somewhat!
  !+ad_call  param.h90
  !+ad_call  start.h90
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  30/06/94 PJK Added stellarator scaling laws 20-23
  !+ad_hist  07/12/95 PJK Added pcharge to plasma input power
  !+ad_hist  06/03/96 PJK Added RFP scaling law
  !+ad_hist  14/11/97 PJK Added ITER-97 scaling laws (26,27)
  !+ad_hist  01/04/98 PJK Added ITER-96P scaling law (28) and moved
  !+ad_hisc               calculation of dnla into BETCOM instead
  !+ad_hist  26/06/98 PJK Added scaling laws 29,30,31
  !+ad_hist  08/10/98 PJK Added scaling laws 32,33,34,35,36
  !+ad_hist  16/07/01 PJK Added KAPPAA to argument list
  !+ad_hist  23/05/06 PJK Ensured that powerht is always positive
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'start.h90'

  !  Arguments

  integer, intent(in) :: iinvqd, isc, ignite
  real(kind(1.0D0)), intent(in) :: afuel, alpmw, aspect, bt, dene, &
       dnitot, dnla, eps, hfact, kappa, kappa95, pcharge, pinje, &
       pinji, plascur, pohmpv, prad, q, qstar, rmajor, rminor, te, &
       ten, tin, vol, xarea, zeff
  real(kind(1.0D0)), intent(out) :: kappaa, powerht, ptre, ptri, &
       tauee, taueff, tauei

  !  Local variables

  real(kind(1.0D0)) :: chii,ck2,denfac,dnla19,dnla20,eps2,gjaeri, &
       n20,pcur,pi,qhat,ratio,rll,str2,str5,taueena,tauit1,tauit2, &
       term1,term2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pi = 3.141592653589793D0

  !  Neoclassical ion transport loss
  !  Calculate ion energy confinement time
  !
  !  N.B. This calculation is superseded later in the routine

  eps2 = eps/2.0D0
  str5 = (2.0D0/(1.0D0+(kappa**2)))
  ck2 = (0.66D0+(1.88D0*(sqrt(eps2)))-(1.54D0*eps2))* &
       (1.0D0+(1.5D0*(eps2**2)))
  chii = (6.5D-22)*ck2*zeff*(aspect**1.5D0)*dene*(q**2)*str5/ &
       ((sqrt(tin))*(bt**2))
  str2 = (2.0D0*(kappa**2)/(1.0D0+(kappa**2)))
  tauei = 0.375D0*rminor**2/chii*str2

  !  Calculate heating power (MW)
  !  If necessary, assume that the device is ignited, so exclude
  !  auxiliary power injected

  if (ignite == 0) then
     powerht = alpmw + pcharge*vol + (pinje+pinji)/1.0D6 &
          + pohmpv*vol - prad*vol
  else
     powerht = alpmw + pcharge*vol + pohmpv*vol - prad*vol
  end if

  !  Ensure heating power is positive

  powerht = max(powerht,1.0D-3)

  !  Line averaged electron density in scaled units

  dnla20 = dnla * 1.0D-20
  dnla19 = dnla * 1.0D-19

  !  Volume averaged electron density in units of 10**20 m**-3

  n20 = dene / 1.0D20

  !  Plasma current in MA

  pcur = plascur / 1.0D6

  !  kappaa = plasma X-sectional area/(pi*rminor*rminor) by definition

  kappaa = xarea/(pi*rminor*rminor)

  !  Calculate Neo-Alcator confinement time (used in several scalings)

  taueena = 0.07D0 * n20 * rminor * rmajor*rmajor * qstar

  !  Electron energy confinement times

!+PJK For some reason using CASE statements instead of the existing
!+PJK IF statements here result in a somewhat different machine...
!+PJK Needs further investigation!

  ! select case (isc)

  ! case (1)  !  Neo-Alcator scaling (ohmic)
  !    tauee = taueena
  !    gtaue = 0.0D0
  !    ptaue = 1.0D0
  !    qtaue = 0.0D0
  !    rtaue = 0.0D0

  ! case (2)  !  Mirnov scaling (H-mode)
  !    tauee = hfact * 0.2D0 * rminor * sqrt(kappa95) * pcur
  !    gtaue = 0.0D0
  !    ptaue = 0.0D0
  !    qtaue = 0.0D0
  !    rtaue = 0.0D0

  ! case (3)  !  Merezhkin-Muhkovatov scaling (L-mode)
  !    tauee = hfact * 3.5D-3 * rmajor**2.75D0 * rminor**0.25D0 * &
  !         kappa95**0.125D0 * qstar * dnla20 * sqrt(afuel) / &
  !         sqrt(ten/10.0D0)
  !    gtaue = 0.0D0
  !    ptaue = 1.0D0
  !    qtaue = -0.5D0
  !    rtaue = 0.0D0

  ! case (4)  !  Shimomura scaling (H-mode)
  !    tauee = hfact * 0.045D0 * rmajor * rminor * bt * sqrt(kappa95) &
  !         * sqrt(afuel)
  !    gtaue = 0.0D0
  !    ptaue = 0.0D0
  !    qtaue = 0.0D0
  !    rtaue = 0.0D0

  ! case (5)  !  Kaye-Goldston scaling (L-mode)
  !    tauee = hfact * 0.055D0 * kappa95**0.28D0 * pcur**1.24D0 * &
  !         n20**0.26D0 * rmajor**1.65D0 * sqrt(afuel/1.5D0) / &
  !         ( bt**0.09D0 * rminor**0.49D0 * powerht**0.58D0 )
  !    gtaue = 0.0D0
  !    ptaue = 0.26D0
  !    qtaue = 0.0D0
  !    rtaue = -0.58D0
  !    if (iinvqd /= 0) tauee = 1.0D0 / &
  !         sqrt(1.0D0/taueena**2 + 1.0D0/tauee**2)

  ! case (6)  !  ITER Power scaling - ITER 89-P (L-mode)
  !    tauee = hfact * 0.048D0 * pcur**0.85D0 * rmajor**1.2D0 * &
  !         rminor**0.3D0 * sqrt(kappa) * dnla20**0.1D0 * bt**0.2D0 * &
  !         sqrt(afuel) / sqrt(powerht)
  !    gtaue = 0.0D0
  !    ptaue = 0.1D0
  !    qtaue = 0.0D0
  !    rtaue = -0.5D0

  ! case (7)  !  ITER Offset linear scaling - ITER 89-O (L-mode)

  !    term1 = 0.04D0 * pcur**0.5D0 * rmajor**0.3D0 * &
  !         rminor**0.8D0 * kappa**0.6D0 * afuel**0.5D0
  !    term2 = 0.064D0 * pcur**0.8D0 * rmajor**1.6D0 * &
  !         rminor**0.6D0 * kappa**0.5D0 * dnla20**0.6D0 * &
  !         bt**0.35D0 * afuel**0.2D0 / powerht
  !    tauee = hfact * (term1 + term2)
  !    gtaue = hfact*term1
  !    ptaue = 0.6D0
  !    qtaue = 0.0D0
  !    rtaue = -1.0D0

  ! case (8)  !  Rebut-Lallia offset linear scaling (L-mode)
  !    rll = (rminor**2 * rmajor * kappa95)**0.333D0
  !    tauee = hfact * 1.65D0 * sqrt(afuel/2.0D0) * &
  !         ( 1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff) + &
  !         0.146D0 * dnla20**0.75D0 * sqrt(pcur) * sqrt(bt) * &
  !         rll**2.75D0 * zeff**0.25D0 /powerht )
  !    gtaue = hfact * 1.65D0 * sqrt(afuel/2.0D0) * &
  !         (1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff))
  !    ptaue = 0.75D0
  !    qtaue = 0.0D0
  !    rtaue = -1.0D0

  ! case (9)  !  Goldston scaling (L-mode)
  !    tauee = hfact * 0.037D0 * pcur * rmajor**1.75D0 * &
  !         rminor**(-0.37D0) * sqrt(kappa95) * sqrt(afuel/1.5D0) / &
  !         sqrt(powerht)
  !    gtaue = 0.0D0
  !    ptaue = 0.0D0
  !    qtaue = 0.0D0
  !    rtaue = -0.5D0
  !    if (iinvqd /= 0) tauee = 1.0D0 / &
  !         sqrt(1.0D0/taueena**2 + 1.0D0/tauee**2)

  ! case (10)  !  T10 scaling
  !    denfac = dnla20 * rmajor * qstar / (1.3D0*bt)
  !    denfac = min(1.0D0,denfac)
  !    tauee = hfact * 0.095D0 * rmajor * rminor * bt * &
  !         sqrt(kappa95) * denfac / powerht**0.4D0 * &
  !         ( zeff**2 * pcur**4 / &
  !         (rmajor * rminor * qstar**3 * kappa95**1.5D0) )**0.08D0
  !    gtaue = 0.0D0
  !    ptaue = 1.0D0
  !    qtaue = 0.0D0
  !    rtaue = -0.4D0

  ! case (11)  !  JAERI scaling
  !    gjaeri = zeff**0.4D0 * ((15.0D0-zeff)/20.0D0)**0.6D0 * &
  !         (3.0D0 * qstar * (qstar+5.0D0) / ((qstar+2.0D0) * &
  !         (qstar+7.0D0)))**0.6D0
  !    tauee = hfact * (0.085D0 * kappa95 * rminor**2 * sqrt(afuel) + &
  !         0.069D0 * n20**0.6D0 * pcur * bt**0.2D0 * rminor**0.4D0 * &
  !         rmajor**1.6D0 * sqrt(afuel) * gjaeri * kappa95**0.2D0 / &
  !         powerht)
  !    gtaue = hfact * 0.085D0 * kappa95 * rminor**2 * sqrt(afuel)
  !    ptaue = 0.6D0
  !    qtaue = 0.0D0
  !    rtaue = -1.0D0

  ! case (12)  !  Kaye-Big scaling
  !    tauee = hfact * 0.105D0 * sqrt(rmajor) * rminor**0.8D0 * &
  !         bt**0.3D0 * kappa95**0.25D0 * pcur**0.85D0 * &
  !         n20**0.1D0 * afuel**0.5D0 / powerht**0.5D0
  !    gtaue = 0.0D0
  !    ptaue = 0.1D0
  !    qtaue = 0.0D0
  !    rtaue = -0.5D0

  ! case (13)  !  ITER H-mode scaling - ITER H90-P
  !    tauee = hfact * 0.064D0 * pcur**0.87D0 * rmajor**1.82D0 * &
  !         rminor**(-0.12D0) * kappa**0.35D0 * dnla20**0.09D0 * &
  !         bt**0.15D0 * sqrt(afuel) / sqrt(powerht)
  !    gtaue = 0.0D0
  !    ptaue = 0.09D0
  !    qtaue = 0.0D0
  !    rtaue = -0.5D0

  ! case (14)  !  Minimum of ITER 89-P (isc=6) and ITER 89-O (isc=7)
  !    tauit1 = hfact * 0.048D0 * pcur**0.85D0 * rmajor**1.2D0 * &
  !         rminor**0.3D0 * sqrt(kappa) * dnla20**0.1D0 * bt**0.2D0 * &
  !         sqrt(afuel) / sqrt(powerht)
  !    term1 = 0.04D0 * pcur**0.5D0 * rmajor**0.3D0 * &
  !         rminor**0.8D0 * kappa**0.6D0 * afuel**0.5D0
  !    term2 = 0.064D0 * pcur**0.8D0 * rmajor**1.6D0 * &
  !         rminor**0.6D0 * kappa**0.5D0 * dnla20**0.6D0 * &
  !         bt**0.35D0 * afuel**0.2D0 / powerht
  !    tauit2 = hfact * (term1 + term2)
  !    tauee = min(tauit1,tauit2)

  !    if (tauit1 < tauit2) then
  !       gtaue = 0.0D0
  !       ptaue = 0.1D0
  !       qtaue = 0.0D0
  !       rtaue = -0.5D0
  !    else
  !       gtaue = hfact*term1
  !       ptaue = 0.6D0
  !       qtaue = 0.0D0
  !       rtaue = -1.0D0
  !    end if

  ! case (15)  !  Riedel scaling (L-mode)
  !    tauee = hfact * 0.044D0 * pcur**0.93D0 * rmajor**1.37D0 * &
  !         rminor**(-0.049D0) * kappa95**0.588D0 * dnla20**0.078D0 * &
  !         bt**0.152D0 / powerht**0.537D0
  !    gtaue = 0.0D0
  !    ptaue = 0.078D0
  !    qtaue = 0.0D0
  !    rtaue = -0.537D0

  ! case (16)  !  Christiansen et al scaling (L-mode)
  !    tauee = hfact * 0.24D0 * pcur**0.79D0 * rmajor**0.56D0 * &
  !         rminor**1.46D0 * kappa95**0.73D0 * dnla20**0.41D0 * &
  !         bt**0.29D0 / (powerht**0.79D0 * afuel**0.02D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.41D0
  !    qtaue = 0.0D0
  !    rtaue = -0.79D0

  ! case (17)  !  Lackner-Gottardi scaling (L-mode)
  !    qhat = (1.0D0+kappa95**2) * rminor**2 * bt /(0.4D0 * pcur * rmajor)
  !    tauee = hfact * 0.12D0 * pcur**0.8D0 * rmajor**1.8D0 * &
  !         rminor**0.4D0 * kappa95 * (1.0D0+kappa95)**(-0.8D0) * &
  !         dnla20**0.6D0 * qhat**0.4D0 / powerht**0.6D0
  !    gtaue = 0.0D0
  !    ptaue = 0.6D0
  !    qtaue = 0.0D0
  !    rtaue = -0.6D0

  ! case (18)  !  Neo-Kaye scaling (L-mode)
  !    tauee = hfact * 0.063D0 * pcur**1.12D0 * rmajor**1.3D0 * &
  !         rminor**(-0.04D0) * kappa95**0.28D0 * dnla20**0.14D0 * &
  !         bt**0.04D0 * sqrt(afuel) / powerht**0.59D0
  !    gtaue = 0.0D0
  !    ptaue = 0.14D0
  !    qtaue = 0.0D0
  !    rtaue = -0.59D0

  ! case (19)  !  Riedel scaling (H-mode)
  !    tauee = hfact * 0.1D0 * sqrt(afuel) * pcur**0.884D0 * &
  !         rmajor**1.24D0 * rminor**(-0.23D0) * kappa95**0.317D0 * &
  !         bt**0.207D0 * dnla20**0.105D0 / powerht**0.486D0
  !    gtaue = 0.0D0
  !    ptaue = 0.105D0
  !    qtaue = 0.0D0
  !    rtaue = -0.486D0

  ! case (20)  !  Amended version of ITER H90-P law
  !    !  Nuclear Fusion 32 (1992) 318
  !    tauee = hfact * 0.082D0 * pcur**1.02D0 * &
  !         bt**0.15D0 * sqrt(afuel) * rmajor**1.60D0 / &
  !         (powerht**0.47D0 * kappa**0.19D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.0D0
  !    qtaue = 0.0D0
  !    rtaue = -0.47D0

  ! case (21)  !  Large Helical Device scaling (stellarators)
  !    !  S.Sudo, Y.Takeiri, H.Zushi et al., Nuclear Fusion 30 (1990) 11
  !    tauee = hfact * 0.17D0 * rmajor**0.75D0 * rminor**2 * &
  !         dnla20**0.69D0 * bt**0.84D0 * powerht**(-0.58D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.69D0
  !    qtaue = 0.0D0
  !    rtaue = 0.58D0

  ! case (22)  !  Gyro-reduced Bohm scaling
  !    !  R.J.Goldston, H.Biglari, G.W.Hammett et al., Bull.Am.Phys.Society,
  !    !  volume 34, 1964 (1989)
  !    tauee = hfact * 0.25D0 * bt**0.8D0 * dnla20**0.6D0 * &
  !         powerht**(-0.6D0) * rminor**2.4D0 * rmajor**0.6D0
  !    gtaue = 0.0D0
  !    ptaue = 0.6D0
  !    qtaue = 0.0D0
  !    rtaue = -0.6D0

  ! case (23)  !  Lackner-Gottardi stellarator scaling
  !    !  (assumed q = 1/rotational transform)
  !    !  K.Lackner and N.A.O.Gottardi, Nuclear Fusion, 30, p.767 (1990)
  !    tauee = hfact * 0.17D0 * rmajor * rminor**2 * dnla20**0.6D0 * &
  !         bt**0.8D0 * powerht**(-0.6D0) * q**(-0.4D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.6D0
  !    qtaue = 0.0D0
  !    rtaue = -0.6D0

  ! case (24)  !  ITER-93H scaling (ELM-free; multiply by 0.85 for ELMy version)
  !    !  S.Kaye and the ITER Joint Central Team and Home Teams, in Plasma
  !    !  Physics and Controlled Nuclear Fusion Research (Proc. 15th
  !    !  Int. Conf., Seville, 1994) IAEA-CN-60/E-P-3
  !    tauee = hfact * 0.053D0 * pcur**1.06D0 * bt**0.32D0 * &
  !         powerht**(-0.67D0) * afuel**0.41D0 * rmajor**1.79D0 * &
  !         dnla20**0.17D0 * aspect**0.11D0 * kappa**0.66D0
  !    gtaue = 0.0D0
  !    ptaue = 0.17D0
  !    qtaue = 0.0D0
  !    rtaue = -0.67D0

  ! case (25)  !  TITAN RFP scaling
  !    !  TITAN RFP Fusion Reactor Study, Scoping Phase Report
  !    !  UCLA-PPG-1100, page 5-9, Jan 1987
  !    tauee = hfact * 0.05D0 * pcur * rminor**2
  !    gtaue = 0.0D0
  !    ptaue = 0.0D0
  !    qtaue = 0.0D0
  !    rtaue = 0.0D0

  !    !  Next two are ITER-97 H-mode scalings
  !    !  J. G. Cordey et al., EPS Berchtesgaden, 1997

  ! case (26)  !  ELM-free: ITERH-97P
  !    tauee = hfact * 0.031D0 * pcur**0.95D0 * bt**0.25D0 * &
  !         powerht**(-0.67D0) * dnla19**0.35D0 * &
  !         rmajor**1.92D0 * aspect**(-0.08D0) * kappa**0.63D0 * &
  !         afuel**0.42D0
  !    gtaue = 0.0D0
  !    ptaue = 0.35D0  !  N.B. problems with ptaue if pulsed option is used with isc=26
  !    qtaue = 0.0D0
  !    rtaue = -0.67D0

  ! case (27)  !  ELMy: ITERH-97P(y)
  !    tauee = hfact * 0.029D0 * pcur**0.90D0 * bt**0.20D0 * & 
  !         powerht**(-0.66D0) * dnla19**0.40D0 * &
  !         rmajor**2.03D0 * aspect**(-0.19D0) * kappa**0.92D0 * &
  !         afuel**0.2D0
  !    gtaue = 0.0D0
  !    ptaue = 0.4D0  ! N.B. problems with ptaue if pulsed option is used with isc=27
  !    qtaue = 0.0D0
  !    rtaue = -0.66D0

  ! case (28)  !  ITER-96P L-mode scaling
  !    !  S.M.Kaye and the ITER Confinement Database Working Group,
  !    !  Nuclear Fusion 37 (1997) 1303
  !    !  N.B. tau_th formula used
  !    tauee = hfact * 0.023D0 * pcur**0.96D0 * bt**0.03D0 * &
  !            kappa95**0.64D0 * rmajor**1.83D0 * aspect**0.06D0 * &
  !            dnla19**0.40D0 * afuel**0.20D0 * powerht**(-0.73D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.4D0  !  N.B. problems with ptaue if pulsed option is used with isc=28
  !    qtaue = 0.0D0
  !    rtaue = -0.73D0

  ! case (29)  !  Valovic modified ELMy-H mode scaling
  !    tauee = hfact * 0.067D0 * pcur**0.9D0 * bt**0.17D0 * &
  !         dnla19**0.45D0 * afuel**0.05D0 * rmajor**1.316D0 * &
  !         rminor**0.79D0 * kappa**0.56D0 * powerht**(-0.68D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.45D0  !  N.B. problems with ptaue if pulsed option is used with isc=29
  !    qtaue = 0.0D0
  !    rtaue = -0.68D0

  ! case (30)  !  Kaye PPPL Workshop April 1998 L-mode scaling
  !    tauee = hfact * 0.021D0 * pcur**0.81D0 * bt**0.14D0 * &
  !         kappa**0.7D0 * rmajor**2.01D0 * aspect**(-0.18D0) * &
  !         dnla19**0.47D0 * afuel**0.25D0 * powerht**(-0.73D0)
  !    gtaue = 0.0D0
  !    ptaue = 0.47D0  ! N.B. problems with ptaue if pulsed option is used with isc=30
  !    qtaue = 0.0D0
  !    rtaue = -0.73D0

  ! case (31)  !  ITERH-PB98P(y), ELMy H-mode scaling
  !    tauee = hfact * 0.0615D0 * pcur**0.9D0 * bt**0.1D0 * &
  !         dnla19**0.4D0 * powerht**(-0.66D0) * rmajor**2 * &
  !         kappaa**0.75D0 * aspect**(-0.66D0) * afuel**0.2D0
  !    gtaue = 0.0D0
  !    ptaue = 0.4D0  !  N.B. problems with ptaue if pulsed option is used with isc=31
  !    qtaue = 0.0D0
  !    rtaue = -0.66D0

  ! case (32)  !  IPB98(y), ELMy H-mode scaling
  !    tauee = hfact * 0.0365D0 * pcur**0.97D0 * bt**0.08D0 * &
  !         dnla19**0.41D0 * powerht**(-0.63D0) * rmajor**1.93D0 * &
  !         kappa**0.67D0 * aspect**(-0.23D0) * afuel**0.2D0
  !    gtaue = 0.0D0
  !    ptaue = 0.41D0  !  N.B. problems with ptaue if pulsed option is used with isc=32
  !    qtaue = 0.0D0
  !    rtaue = -0.63D0

  ! case (33)  !  IPB98(y,1), ELMy H-mode scaling
  !    tauee = hfact * 0.0503D0 * pcur**0.91D0 * bt**0.15D0 * &
  !         dnla19**0.44D0 * powerht**(-0.65D0) * rmajor**2.05D0 * &
  !         kappaa**0.72D0 * aspect**(-0.57D0) * afuel**0.13D0
  !    gtaue = 0.0D0
  !    ptaue = 0.44D0  !  N.B. problems with ptaue if pulsed option is used with isc=33
  !    qtaue = 0.0D0
  !    rtaue = -0.65D0

  ! case (34)  !  IPB98(y,2), ELMy H-mode scaling
  !    tauee = hfact * 0.0562D0 * pcur**0.93D0 * bt**0.15D0 * &
  !         dnla19**0.41D0 * powerht**(-0.69D0) * rmajor**1.97D0 * &
  !         kappaa**0.78D0 * aspect**(-0.58D0) * afuel**0.19D0
  !    gtaue = 0.0D0
  !    ptaue = 0.41D0  !  N.B. problems with ptaue if pulsed option is used with isc=34
  !    qtaue = 0.0D0
  !    rtaue = -0.69D0

  ! case (35)  !  IPB98(y,3), ELMy H-mode scaling
  !    tauee = hfact * 0.0564D0 * pcur**0.88D0 * bt**0.07D0 * &
  !         dnla19**0.40D0 * powerht**(-0.69D0) * rmajor**2.15D0 * &
  !         kappaa**0.78D0 * aspect**(-0.64D0) * afuel**0.20D0
  !    gtaue = 0.0D0
  !    ptaue = 0.4D0  !  N.B. problems with ptaue if pulsed option is used with isc=35
  !    qtaue = 0.0D0
  !    rtaue = -0.69D0

  ! case (36)  !  IPB98(y,4), ELMy H-mode scaling
  !    tauee = hfact * 0.0587D0 * pcur**0.85D0 * bt**0.29D0 * &
  !         dnla19**0.39D0 * powerht**(-0.70D0) * rmajor**2.08D0 * &
  !         kappaa**0.76D0 * aspect**(-0.69D0) * afuel**0.17D0
  !    gtaue = 0.0D0
  !    ptaue = 0.39D0  !  N.B. problems with ptaue if pulsed option is used with isc=36
  !    qtaue = 0.0D0
  !    rtaue = -0.70D0

  ! case default
  !    write(*,*) 'Error in routine PCOND:'
  !    write(*,*) 'Illegal value for ISC, = ',isc
  !    write(*,*) 'PROCESS stopping.'
  !    stop
  ! end select
!-PJK

  ! *** Neo-Alcator scaling (ohmic)

  if (isc.eq.1) then
     tauee = taueena
     gtaue = 0.0D0
     ptaue = 1.0D0
     qtaue = 0.0D0
     rtaue = 0.0D0
  end if

  ! *** Mirnov scaling (H-mode)

  if (isc.eq.2) then
     tauee = hfact * 0.2D0 * rminor * sqrt(kappa95) * pcur
     gtaue = 0.0D0
     ptaue = 0.0D0
     qtaue = 0.0D0
     rtaue = 0.0D0
  end if

  ! *** Merezhkin-Muhkovatov scaling (L-mode)
  !+**PJK 29/06/92 Corrected te to ten in Merez-Muhkov scaling law
  if (isc.eq.3) then
     tauee = hfact * 3.5D-3 * rmajor**2.75D0 * rminor**0.25D0 * &
          kappa95**0.125D0 * qstar * dnla20 * sqrt(afuel) / &
          sqrt(ten/10.0D0)
     gtaue = 0.0D0
     ptaue = 1.0D0
     qtaue = -0.5D0
     rtaue = 0.0D0
  end if

  ! *** Shimomura scaling (H-mode)

  if (isc.eq.4) then
     tauee = hfact * 0.045D0 * rmajor * rminor * bt * &
          sqrt(kappa95) * sqrt(afuel)
     gtaue = 0.0D0
     ptaue = 0.0D0
     qtaue = 0.0D0
     rtaue = 0.0D0
  end if

  ! *** Kaye-Goldston scaling (L-mode)

  if (isc.eq.5) then
     tauee = hfact * 0.055D0 * kappa95**0.28D0 * pcur**1.24D0 * &
          n20**0.26D0 * rmajor**1.65D0 * sqrt(afuel/1.5D0) / &
          ( bt**0.09D0 * rminor**0.49D0 * powerht**0.58D0 )
     gtaue = 0.0D0
     ptaue = 0.26D0
     qtaue = 0.0D0
     rtaue = -0.58D0
     if (iinvqd.ne.0) tauee = 1.0D0/ sqrt( 1.0D0/taueena**2 + &
          1.0D0/tauee**2)
  end if

  ! *** ITER Power scaling - ITER 89-P (L-mode)

  if ((isc.eq.6).or.(isc.eq.14)) then
     tauee = hfact * 0.048D0 * pcur**0.85D0 * rmajor**1.2D0 * &
          rminor**0.3D0 * sqrt(kappa) * dnla20**0.1D0 * bt**0.2D0 * &
          sqrt(afuel) / sqrt(powerht)
     tauit1 = tauee
     gtaue = 0.0D0
     ptaue = 0.1D0
     qtaue = 0.0D0
     rtaue = -0.5D0
  end if

  ! *** ITER Offset linear scaling - ITER 89-O (L-mode)
  !+**PJK 30/06/92 Corrected exponent on kappa in term2 from 0.2 to 0.5

  if ((isc.eq.7).or.(isc.eq.14)) then
     term1 = 0.04D0 * pcur**0.5D0 * rmajor**0.3D0 * &
          rminor**0.8D0 * kappa**0.6D0 * afuel**0.5D0
     term2 = 0.064D0 * pcur**0.8D0 * rmajor**1.6D0 * &
          rminor**0.6D0 * kappa**0.5D0 * dnla20**0.6D0 * &
          bt**0.35D0 * afuel**0.2D0 / powerht
     tauee = hfact * (term1 + term2)
     tauit2 = tauee
     gtaue = hfact*term1
     ptaue = 0.6D0
     qtaue = 0.0D0
     rtaue = -1.0D0
     if (isc.eq.14) then
        tauee = min(tauit1,tauit2)
        if (tauit1.lt.tauit2) then
           gtaue = 0.0D0
           ptaue = 0.1D0
           qtaue = 0.0D0
           rtaue = -0.5D0
        end if
     end if
  end if

  ! *** Rebut-Lallia offset linear scaling (L-mode)

  !+**PJK 10/06/92 I do not understand the additional factor of 1/2.
  !+**PJK 30/06/92 Therefore I have removed it (suggestion of Tim Hender).

  if (isc.eq.8) then
     rll = (rminor**2 * rmajor * kappa95)**0.333D0
     tauee = hfact * 1.65D0 * sqrt(afuel/2.0D0) * ( &
          1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff) + &
          0.146D0 * dnla20**0.75D0 * sqrt(pcur) * sqrt(bt) * &
          rll**2.75D0 * zeff**0.25D0 /powerht )
     gtaue = hfact * 1.65D0 * sqrt(afuel/2.0D0) * ( &
          1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff) )
     ptaue = 0.75D0
     qtaue = 0.0D0
     rtaue = -1.0D0
  end if

  ! *** Goldston scaling (L-mode)

  if (isc.eq.9) then
     tauee = hfact * 0.037D0 * pcur * rmajor**1.75D0 * &
          rminor**(-0.37D0) * sqrt(kappa95) * sqrt(afuel/1.5D0) / &
          sqrt(powerht)
     gtaue = 0.0D0
     ptaue = 0.0D0
     qtaue = 0.0D0
     rtaue = -0.5D0
     if (iinvqd.ne.0) tauee = 1.0D0/ &
          sqrt( 1.0D0/taueena**2 + 1.0D0/tauee**2)
  end if

  ! *** T10 scaling

  if (isc.eq.10) then
     denfac = dnla20 * rmajor * qstar / (1.3D0*bt)
     denfac = min(1.0D0,denfac)
     tauee = hfact * 0.095D0 * rmajor * rminor * bt * &
          sqrt(kappa95) * denfac / powerht**0.4D0 * &
          ( zeff**2 * pcur**4 / &
          (rmajor * rminor * qstar**3 * kappa95**1.5D0) )**0.08D0
     gtaue = 0.0D0
     ptaue = 1.0D0
     qtaue = 0.0D0
     rtaue = -0.4D0
  end if

  ! *** JAERI scaling

  if (isc.eq.11) then
     gjaeri = zeff**0.4D0 * ((15.0D0-zeff)/20.0D0)**0.6D0 * &
          (3.0D0 * qstar * (qstar+5.0D0) / ((qstar+2.0D0) * &
          (qstar+7.0D0)))**0.6D0
     tauee = hfact * (0.085D0 * kappa95 * rminor**2 * sqrt(afuel) + &
          0.069D0 * n20**0.6D0 * pcur * bt**0.2D0 * rminor**0.4D0 * &
          rmajor**1.6D0 * sqrt(afuel) * gjaeri * kappa95**0.2D0 / &
          powerht)
     gtaue = hfact * 0.085D0 * kappa95 * rminor**2 * sqrt(afuel)
     ptaue = 0.6D0
     qtaue = 0.0D0
     rtaue = -1.0D0
  end if

  ! *** Kaye-Big scaling

  if (isc.eq.12) then
     tauee = hfact * 0.105D0 * sqrt(rmajor) * rminor**0.8D0 * &
          bt**0.3D0 * kappa95**0.25D0 * pcur**0.85D0 * &
          n20**0.1D0 * afuel**0.5D0 / powerht**0.5D0
     gtaue = 0.0D0
     ptaue = 0.1D0
     qtaue = 0.0D0
     rtaue = -0.5D0
  end if

  ! *** ITER H-mode scaling - ITER H90-P

  if (isc.eq.13) then
     tauee = hfact * 0.064D0 * pcur**0.87D0 * rmajor**1.82D0 * &
          rminor**(-0.12D0) * kappa**0.35D0 * dnla20**0.09D0 * &
          bt**0.15D0 * sqrt(afuel) / sqrt(powerht)
     gtaue = 0.0D0
     ptaue = 0.09D0
     qtaue = 0.0D0
     rtaue = -0.5D0
  end if

  ! *** Riedel scaling (L-mode)

  if (isc.eq.15) then
     tauee = hfact * 0.044D0 * pcur**0.93D0 * rmajor**1.37D0 * &
          rminor**(-0.049D0) * kappa95**0.588D0 * dnla20**0.078D0 * &
          bt**0.152D0 / powerht**0.537D0
     gtaue = 0.0D0
     ptaue = 0.078D0
     qtaue = 0.0D0
     rtaue = -0.537D0
  end if

  ! *** Christiansen et al scaling (L-mode)

  if (isc.eq.16) then
     tauee = hfact * 0.24D0 * pcur**0.79D0 * rmajor**0.56D0 * &
          rminor**1.46D0 * kappa95**0.73D0 * dnla20**0.41D0 * &
          bt**0.29D0 / (powerht**0.79D0 * afuel**0.02D0)
     gtaue = 0.0D0
     ptaue = 0.41D0
     qtaue = 0.0D0
     rtaue = -0.79D0
  end if

  ! *** Lackner-Gottardi scaling (L-mode)

  if (isc.eq.17) then
     qhat = (1.0D0+kappa95**2) * rminor**2 * bt / &
          (0.4D0 * pcur * rmajor)
     tauee = hfact * 0.12D0 * pcur**0.8D0 * rmajor**1.8D0 * &
          rminor**0.4D0 * kappa95 * (1.0D0+kappa95)**(-0.8D0) * &
          dnla20**0.6D0 * qhat**0.4D0 / powerht**0.6D0
     gtaue = 0.0D0
     ptaue = 0.6D0
     qtaue = 0.0D0
     rtaue = -0.6D0
  end if

  ! *** Neo-Kaye scaling (L-mode)
  !+**PJK 29/06/92 Added missing afuel term to Neo-Kaye scaling law

  if (isc.eq.18) then
     tauee = hfact * 0.063D0 * pcur**1.12D0 * rmajor**1.3D0 * &
          rminor**(-0.04D0) * kappa95**0.28D0 * dnla20**0.14D0 * &
          bt**0.04D0 * sqrt(afuel) / powerht**0.59D0
     gtaue = 0.0D0
     ptaue = 0.14D0
     qtaue = 0.0D0
     rtaue = -0.59D0
  end if

  ! *** Riedel scaling (H-mode)

  if (isc.eq.19) then
     tauee = hfact * 0.1D0 * sqrt(afuel) * pcur**0.884D0 * &
          rmajor**1.24D0 * rminor**(-0.23D0) * kappa95**0.317D0 * &
          bt**0.207D0 * dnla20**0.105D0 / powerht**0.486D0
     gtaue = 0.0D0
     ptaue = 0.105D0
     qtaue = 0.0D0
     rtaue = -0.486D0
  end if

  !+**PJK 17/12/92 Added amended version of ITER H90-P law.
  !+**PJK 17/12/92 Seems to bear no relation to the original...(isc=13)
  !+**PJK 17/12/92 From Nuclear Fusion 32 (1992) 318.

  if (isc.eq.20) then
     tauee = hfact * 0.082D0 * pcur**1.02D0 * &
          bt**0.15D0 * sqrt(afuel) * rmajor**1.60D0 / &
          (powerht**0.47D0 * kappa**0.19D0)
     gtaue = 0.0D0
     ptaue = 0.0D0
     qtaue = 0.0D0
     rtaue = -0.47D0
  end if

  !+**PJK 30/06/94 Added three new scaling laws, relevant to stellarators

  ! *** Large Helical Device scaling
  ! *** S.Sudo, Y.Takeiri, H.Zushi et al., Nuclear Fusion, 30, p.11 (1990)

  if (isc.eq.21) then
     tauee = hfact * 0.17D0 * rmajor**0.75D0 * rminor**2 * &
          dnla20**0.69D0 * bt**0.84D0 * powerht**(-0.58D0)
     gtaue = 0.0D0
     ptaue = 0.69D0
     qtaue = 0.0D0
     rtaue = 0.58D0
  end if

  ! *** Gyro-reduced Bohm scaling
  ! *** R.J.Goldston, H.Biglari, G.W.h90ammett et al., Bull.Am.Phys.Society,
  ! *** volume 34, 1964 (1989)

  if (isc.eq.22) then
     tauee = hfact * 0.25D0 * bt**0.8D0 * dnla20**0.6D0 * &
          powerht**(-0.6D0) * rminor**2.4D0 * rmajor**0.6D0
     gtaue = 0.0D0
     ptaue = 0.6D0
     qtaue = 0.0D0
     rtaue = -0.6D0
  end if

  ! *** Lackner-Gottardi stellarator scaling
  ! *** (assumed q = 1/rotational transform)
  ! *** K.Lackner and N.A.O.Gottardi, Nuclear Fusion, 30, p.767 (1990)

  if (isc.eq.23) then
     tauee = hfact * 0.17D0 * rmajor * rminor**2 * dnla20**0.6D0 * &
          bt**0.8D0 * powerht**(-0.6D0) * q**(-0.4D0)
     gtaue = 0.0D0
     ptaue = 0.6D0
     qtaue = 0.0D0
     rtaue = -0.6D0
  end if

  ! *** ITER-93H scaling (ELM-free; multiply by 0.85 for ELMy version)
  ! *** S.Kaye and the ITER Joint Central Team and Home Teams, in Plasma
  ! *** Physics and Controlled Nuclear Fusion Research (Proc. 15th
  ! *** Int. Conf., Seville, 1994) IAEA-CN-60/E-P-3

  if (isc.eq.24) then
     tauee = hfact * 0.053D0 * pcur**1.06D0 * bt**0.32D0 * &
          powerht**(-0.67D0) * afuel**0.41D0 * rmajor**1.79D0 * &
          dnla20**0.17D0 * aspect**0.11D0 * kappa**0.66D0

     gtaue = 0.0D0
     ptaue = 0.17D0
     qtaue = 0.0D0
     rtaue = -0.67D0
  end if

  ! *** TITAN RFP scaling
  ! *** TITAN RFP Fusion Reactor Study, Scoping Phase Report
  ! *** UCLA-PPG-1100, page 5-9, Jan 1987

  if (isc.eq.25) then
     tauee = hfact * 0.05D0 * pcur * rminor**2

     gtaue = 0.0D0
     ptaue = 0.0D0
     qtaue = 0.0D0
     rtaue = 0.0D0
  end if

  ! *** ITER-97 H-mode scalings
  ! *** J.G.Cordey et al., EPS Berchtesgaden, 1997

  ! *** ELM-free: ITERH-97P

  if (isc.eq.26) then
     tauee = hfact * 0.031D0 * pcur**0.95D0 * bt**0.25D0 * &
          powerht**(-0.67D0) * dnla19**0.35D0 * &
          rmajor**1.92D0 * aspect**(-0.08D0) * kappa**0.63D0 * &
          afuel**0.42D0

     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=26
     ptaue = 0.35D0
     qtaue = 0.0D0
     rtaue = -0.67D0
  end if

  ! *** ELMy: ITERH-97P(y)

  if (isc.eq.27) then
     tauee = hfact * 0.029D0 * pcur**0.90D0 * bt**0.20D0 * &
          powerht**(-0.66D0) * dnla19**0.40D0 * &
          rmajor**2.03D0 * aspect**(-0.19D0) * kappa**0.92D0 * &
          afuel**0.2D0

     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=27
     ptaue = 0.4D0
     qtaue = 0.0D0
     rtaue = -0.66D0
  end if

  ! *** ITER-96P L-mode scaling
  ! *** S.M.Kaye and the ITER Confinement Database Working Group,
  ! *** Nuclear Fusion vol.37 (1997) 1303

  ! *** N.B. tau_th formula used

  if (isc.eq.28) then
     tauee = hfact * 0.023D0 * pcur**0.96D0 * bt**0.03D0 * &
          kappa95**0.64D0 * rmajor**1.83D0 * aspect**0.06D0 * &
          dnla19**0.40D0 * afuel**0.20D0 * powerht**(-0.73D0)

     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=28
     ptaue = 0.4D0
     qtaue = 0.0D0
     rtaue = -0.73D0
  end if

  ! *** Valovic modified ELMy-H mode scaling

  if (isc.eq.29) then
     tauee = hfact * 0.067D0 * pcur**0.9D0 * bt**0.17D0 * &
          dnla19**0.45D0 * afuel**0.05D0 * rmajor**1.316D0 * &
          rminor**0.79D0 * kappa**0.56D0 * powerht**(-0.68D0)
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=29
     ptaue = 0.45D0
     qtaue = 0.0D0
     rtaue = -0.68D0
  end if

  ! *** Kaye PPPL Workshop April 1998 L-mode scaling

  if (isc.eq.30) then
     tauee = hfact * 0.021D0 * pcur**0.81D0 * bt**0.14D0 * &
          kappa**0.7D0 * rmajor**2.01D0 * aspect**(-0.18D0) * &
          dnla19**0.47D0 * afuel**0.25D0 * powerht**(-0.73D0)
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=30
     ptaue = 0.47D0
     qtaue = 0.0D0
     rtaue = -0.73D0
  end if

  ! *** ITERH-PB98P(y), ELMy H-mode scaling

  if (isc.eq.31) then
     tauee = hfact * 0.0615D0 * pcur**0.9D0 * bt**0.1D0 * &
          dnla19**0.4D0 * powerht**(-0.66D0) * rmajor**2 * &
          kappaa**0.75D0 * aspect**(-0.66D0) * afuel**0.2D0
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=31
     ptaue = 0.4D0
     qtaue = 0.0D0
     rtaue = -0.66D0
  end if

  ! *** IPB98(y), ELMy H-mode scaling

  if (isc.eq.32) then
     tauee = hfact * 0.0365D0 * pcur**0.97D0 * bt**0.08D0 * &
          dnla19**0.41D0 * powerht**(-0.63D0) * rmajor**1.93D0 * &
          kappa**0.67D0 * aspect**(-0.23D0) * afuel**0.2D0
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=32
     ptaue = 0.41D0
     qtaue = 0.0D0
     rtaue = -0.63D0
  end if

  ! *** IPB98(y,1), ELMy H-mode scaling

  if (isc.eq.33) then
     tauee = hfact * 0.0503D0 * pcur**0.91D0 * bt**0.15D0 * &
          dnla19**0.44D0 * powerht**(-0.65D0) * rmajor**2.05D0 * &
          kappaa**0.72D0 * aspect**(-0.57D0) * afuel**0.13D0
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=33
     ptaue = 0.44D0
     qtaue = 0.0D0
     rtaue = -0.65D0
  end if

  ! *** IPB98(y,2), ELMy H-mode scaling

  if (isc.eq.34) then
     tauee = hfact * 0.0562D0 * pcur**0.93D0 * bt**0.15D0 * &
          dnla19**0.41D0 * powerht**(-0.69D0) * rmajor**1.97D0 * &
          kappaa**0.78D0 * aspect**(-0.58D0) * afuel**0.19D0
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=34
     ptaue = 0.41D0
     qtaue = 0.0D0
     rtaue = -0.69D0
  end if

  ! *** IPB98(y,3), ELMy H-mode scaling

  if (isc.eq.35) then
     tauee = hfact * 0.0564D0 * pcur**0.88D0 * bt**0.07D0 * &
          dnla19**0.40D0 * powerht**(-0.69D0) * rmajor**2.15D0 * &
          kappaa**0.78D0 * aspect**(-0.64D0) * afuel**0.20D0
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=35
     ptaue = 0.4D0
     qtaue = 0.0D0
     rtaue = -0.69D0
  end if

  ! *** IPB98(y,4), ELMy H-mode scaling

  if (isc.eq.36) then
     tauee = hfact * 0.0587D0 * pcur**0.85D0 * bt**0.29D0 * &
          dnla19**0.39D0 * powerht**(-0.70D0) * rmajor**2.08D0 * &
          kappaa**0.76D0 * aspect**(-0.69D0) * afuel**0.17D0
     gtaue = 0.0D0
     ! ***    N.B. problems with ptaue if pulsed option is used with isc=36
     ptaue = 0.39D0
     qtaue = 0.0D0
     rtaue = -0.70D0
  end if

  !  Ion energy confinement time
  !  N.B. Overwrites earlier calculation above

  tauei = tauee

  !  Calculation of the transport power loss terms
  !  Transport losses in Watts/m3 are 3/2 * n.e.T / tau , with T in eV
  !  (here, tin and ten are in keV, and ptre and ptri are in MW/m3)

  ptri = 2.403D-22 * dnitot*tin/tauei
  ptre = 2.403D-22 * dene*ten/tauee

  ratio = dnitot/dene * tin/ten

  !  Global energy confinement time

  taueff = ((ratio + 1.0D0)/(ratio/tauei + 1.0D0/tauee))

  ftaue = (tauee-gtaue) / &
       (n20**ptaue * (te/10.0D0)**qtaue * powerht**rtaue)

end subroutine pcond

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rmu0,rplas, &
     plascur,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

  !+ad_name  vscalc
  !+ad_summ  Volt-second requirements
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  csawth : input real :  coefficient for sawteeth effects
  !+ad_args  eps    : input real :  inverse aspect ratio
  !+ad_args  facoh  : input real :  fraction of plasma current produced inductively
  !+ad_args  gamma  : input real :  coefficient for resistive start-up V-s component
  !+ad_args  kappa  : input real :  plasma elongation
  !+ad_args  plascur: input real :  plasma current (A)
  !+ad_args  rli    : input real :  plasma normalised inductivity
  !+ad_args  rmajor : input real :  plasma major radius (m)
  !+ad_args  rmu0   : input real :  4.0D-7 * pi
  !+ad_args  rplas  : input real :  plasma resistance (ohm)
  !+ad_args  tburn  : input real :  burn time (s)
  !+ad_args  phiint : output real : internal plasma volt-seconds (Wb)
  !+ad_args  rlp    : output real : plasma inductance (H)
  !+ad_args  vsbrn  : output real : volt-seconds needed during burn (Wb)
  !+ad_args  vsind  : output real : internal and external plasma inductance V-s (Wb)
  !+ad_args  vsres  : output real : resistive losses in start-up volt-seconds (Wb)
  !+ad_args  vsstt  : output real : total volt-seconds needed (Wb)
  !+ad_desc  This subroutine calculates the volt-second requirements and some
  !+ad_desc  other related items.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: csawth, eps, facoh, gamma, kappa, &
       plascur, rli, rmajor, rmu0, rplas, tburn
  real(kind(1.0D0)), intent(out) :: phiint, rlp, vsbrn, vsind, vsres, vsstt

  !  Local variables

  real(kind(1.0D0)) :: aeps,beps,rlpext,rlpint,vburn

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Internal inductance

  rlpint = rmu0 * rmajor * rli/2.0D0
  phiint = rlpint*plascur

  !  Start-up resistive component
  !  Uses ITER formula without the 10 V-s add-on

  vsres = gamma * rmu0*plascur*rmajor

  !  Hirshman, Neilson: Physics of Fluids, 29 (1986) p790
  !  fit for external inductance

  aeps = (1.0D0 + 1.81D0*sqrt(eps)+2.05D0*eps)*log(8.0D0/eps) &
       - (2.0D0 + 9.25D0*sqrt(eps)-1.21D0*eps)
  beps = 0.73D0 * sqrt(eps) *(1.0D0 + 2.0D0*eps**4-6.0D0*eps**5 &
       + 3.7D0*eps**6)
  rlpext = rmajor*rmu0 * aeps*(1.0D0-eps)/(1.0D0-eps+beps*kappa)

  rlp = 1.25D0 * (rlpext + rlpint)

  !  Inductive V-s component

  vsind = rlp * plascur
  vsstt = vsres + vsind

  !  Loop voltage during burn

  vburn = plascur*rplas * facoh

  !  Include enhancement factor in flattop V-s requirement
  !  to account for MHD sawtooth effects.

  !  N.B. tburn on first iteration will not be correct
  !  if the pulsed reactor option is used, but the value
  !  will be correct on subsequent calls.

  vsbrn = csawth * vburn*tburn
  vsstt = vsstt + vsbrn

end subroutine vscalc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp, &
     dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

  !+ad_name  phyaux
  !+ad_summ  Auxiliary physics quantities
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  aspect : input real :  plasma aspect ratio
  !+ad_args  dene   : input real :  electron density (/m3)
  !+ad_args  deni   : input real :  fuel ion density (/m3)
  !+ad_args  dnalp  : input real :  alpha ash density (/m3)
  !+ad_args  dnprot : input real :  proton ash density (/m3)
  !+ad_args  idhe3  : input integer :  switch denoting D-T or D-He3 reaction
  !+ad_args  plascur: input real :  plasma current (A)
  !+ad_args  powfmw : input real :  fusion power (MW)
  !+ad_args  sbar   : input real :  exponent for aspect ratio (normally 1)
  !+ad_args  taueff : input real :  global energy confinement time (s)
  !+ad_args  burnup : output real : fractional plasma burnup
  !+ad_args  dntau  : output real : plasma average n-tau (s/m3)
  !+ad_args  figmer : output real : physics figure of merit
  !+ad_args  fusrat : output real : number of fusion reactions per second
  !+ad_args  qfuel  : output real : fuelling rate for D-T (A)
  !+ad_args  rndfuel: output real : fuel burnup rate (A)
  !+ad_args  taup   : output real : 5 * global energy confinement time
  !+ad_desc  This subroutine calculates extra physics related items
  !+ad_desc  needed by other parts of the code
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  07/12/95 PJK Added D-He3 calculations
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: idhe3
  real(kind(1.0D0)), intent(in) :: aspect, dene, deni, dnalp, dnprot, &
       plascur, powfmw, sbar, taueff
  real(kind(1.0D0)), intent(out) :: burnup, dntau, figmer, fusrat, &
       qfuel, rndfuel, taup

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  figmer = 1.0D-6 * plascur * aspect**sbar

  dntau = taueff*dene

  taup = taueff*5.0D0

  if (idhe3 == 0) then
     fusrat = powfmw /(17.6D0 * 1.6022D-19)
  else
     fusrat = powfmw /(18.3D0 * 1.6022D-19)
  end if

  !  Assume that the ash and fuel particle confinement times are equal

  if (idhe3 == 0) then
     burnup = 1.0D0/ (1.0D0 + deni/(dnalp*2.0D0) )
  else
     burnup = 1.0D0/ (1.0D0 + deni/(dnalp+dnprot) )
  end if

  !  Factor 2 should be changed for D-He3?

  rndfuel = 1.6022D-19 * 2.0D0 * fusrat

  qfuel = rndfuel/burnup

end subroutine phyaux

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function svfdt_orig(t)

  !+ad_name  svfdt_orig
  !+ad_summ  Routine to calculate the D-T reaction rate
  !+ad_type  Function returning real
  !+ad_auth  Ronald L. Miller, LANL
  !+ad_auth  J. Galambos, ORNL (fits by L Hivley)
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  t : input real :  temperature (keV)
  !+ad_desc  This routine calculates the D-T reaction rate in m3/s.
  !+ad_desc  The range of the fit is 0 < t(keV) < 80
  !+ad_desc  <P>
  !+ad_desc  Copyright, 1987, the Regents of the University of California.
  !+ad_desc  This software was produced under a U.S. Government contract
  !+ad_desc  (W-7405-ENG-36) by the Los Alamos National Laboratory, which
  !+ad_desc  is operated by the University of California for the U.S.
  !+ad_desc  Department of Energy.  The U.S. Government is licensed to use,
  !+ad_desc  reproduce, and distribute the software.  Permission is granted
  !+ad_desc  to the public to copy and use this software without charge,
  !+ad_desc  provided that this notice and any statement of authorship are
  !+ad_desc  reproduced on all copies.  Neither the Government nor the
  !+ad_desc  University makes any warranty, express or implied, or assumes
  !+ad_desc  any liability or responsibility for the use of this software.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  25/07/11 PJK Changed name to SVFDT_ORIG; superseded by
  !+ad_hisc               new routine <A HREF="svfdt.html">svfdt</A>.
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  Jarmie, Brown, &amp; Hardekopf, Phys Rev C 29, 2031 (1984)
  !+ad_docc                      and erratum Phys Rev C 33,  385 (1986)
  !+ad_docs  Hivley, Nuclear Fusion 17 (1977) 873
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: svfdt_orig

  !  Arguments

  real(kind(1.0D0)), intent(in) :: t

  !  Local variables

  integer :: j
  real(kind(1.0D0)) :: dummy, sdtjbh, ss, tau, z
  real(kind(1.0D0)), dimension(7)  :: acoef
  real(kind(1.0D0)), dimension(12) :: tn

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  tn(1)  =  0.115919516928D2
  tn(2)  =  0.171469900884D1
  tn(3)  = -0.192406821958D2
  tn(4)  =  0.101380998163D3
  tn(5)  = -0.276406974777D3
  tn(6)  =  0.462387793663D3
  tn(7)  = -0.495796101221D3
  tn(8)  =  0.346206957555D3
  tn(9)  = -0.155600394210D3
  tn(10) =  0.431789037054D2
  tn(11) = -0.671263871904D1
  tn(12) =  0.446674956199D0

  acoef(1) = -21.377692D0
  acoef(2) = -25.204054D0
  acoef(3) = -0.071013427D0
  acoef(4) = 1.9375451D-4
  acoef(5) = 4.9246592D-6
  acoef(6) = -3.9836572D-8
  acoef(7) = 0.2935D0

  if (t <= 20.0D0) then  !  Jarmie, Brown and Hardekopf method

     z = t**0.33333333333333D0
     ss = 0.0D0
     do j = 1,12
        ss = ss+tn(j)*z**(j-1)
     end do
     tau = 19.983D0/z

     sdtjbh = 5.967D-22*ss*tau**2*exp(-tau)*(1.0D0+5.0D0/(12.0D0*tau))
     svfdt_orig = sdtjbh

  else  !  Hivley fit

     dummy = acoef(1)/t**acoef(7) + acoef(2) + acoef(3)*t + &
          acoef(4)*t**2 + acoef(5)*t**3 + acoef(6)*t**4
     svfdt_orig = 1.0D-6 * exp(dummy)

  end if

end function svfdt_orig

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function svfdt(t)

  !+ad_name  svfdt
  !+ad_summ  Routine to calculate the D-T reaction rate
  !+ad_type  Function returning real
  !+ad_auth  R Kemp, CCFE, Culham Science Centre
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  t : input real :  Maxwellian density-weighted ion temperature (keV)
  !+ad_desc  This routine calculates the D-T reaction rate in m3/s.
  !+ad_desc  The range of the fit is 0.2 < t(keV) < 100
  !+ad_prob  Promoting two values to full double precision changes
  !+ad_prob  the resulting machine somewhat.
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  25/07/11 PJK Changed name to SVFDT_ORIG; superseded by
  !+ad_hisc               new routine <A HREF="svfdt.html">svfdt</A>.
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  Bosch and Hale, Nuclear Fusion 32 (1992) 611-631
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: svfdt

  !  Arguments

  real(kind(1.0D0)), intent(in) :: t

  !  Local variables

  real(kind(1.0D0)) :: bg, mrc2, zeta, theta1, theta
  real(kind(1.0D0)), dimension(7) :: acoef

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !+PJK 09/11/11 Converting to 34.3827D0 results in a different machine...
  bg = 34.3827
  mrc2 = 1.124656D6

  acoef(1) =  1.17302D-9
  acoef(2) =  1.51361D-2
  acoef(3) =  7.51886D-2
  acoef(4) =  4.60643D-3
  acoef(5) =  1.35000D-2
  acoef(6) = -1.06750D-4
  acoef(7) =  1.36600D-5

  theta1 = t*(acoef(2) + t*(acoef(4) + t*acoef(6))) / &
       (1.0D0 + t*(acoef(3) + t*(acoef(5) + t*acoef(7))))
  theta = t/(1.0D0 - theta1)

  !+PJK 09/11/11 Converting to 0.3333333333D0 results in a different machine...
  zeta = ((bg**2)/(4.0D0*theta))**0.3333333333

  svfdt = 1.0D-6 * acoef(1)*theta*sqrt(zeta/(mrc2*t**3)) * &
       exp(-3.0D0 * zeta)

end function svfdt

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

  !+ad_name  rether
  !+ad_summ  Routine to find the equilibration power between the
  !+ad_summ  ions and electrons
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphan : input real :  density profile index
  !+ad_args  alphat : input real :  temperature profile index
  !+ad_args  dene   : input real :  electron density (/m3)
  !+ad_args  dlamie : input real :  ion-electron coulomb logarithm
  !+ad_args  te     : input real :  electron temperature (keV)
  !+ad_args  ti     : input real :  ion temperature (keV)
  !+ad_args  zeffai : input real :  density weighted plasma effective charge
  !+ad_args  pie    : output real : ion/electron equilibration power (MW/m3)
  !+ad_desc  This routine calculates the equilibration power between the
  !+ad_desc  ions and electrons.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: alphan, alphat, dene, dlamie, &
       te, ti, zeffai
  real(kind(1.0D0)), intent(out) :: pie

  !  Local variables

  real(kind(1.0D0)) :: conie, profie

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  profie = (1.0D0+alphan)**2 / &
       ( (2.0D0*alphan - 0.5D0*alphat + 1.0D0) * sqrt(1.0D0+alphat) )

  conie = 2.42165D-41 * dlamie * dene**2 * zeffai * profie

  pie = conie*(ti-te)/(te**1.5D0)

end subroutine rether

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95, &
     rmajor,rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol, &
     pbrem,plrad,prad,psync)

  !+ad_name  radpwr
  !+ad_summ  Radiation power calculation
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  R Kemp, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphan : input real :  density profile index
  !+ad_args  alphat : input real :  temperature profile index
  !+ad_args  aspect : input real :  plasma aspect ratio
  !+ad_args  bt     : input real :  toroidal field on axis (T)
  !+ad_args  dene   : input real :  electron density (/m3)
  !+ad_args  deni   : input real :  fuel ion density (/m3)
  !+ad_args  fbfe   : input real :  fraction of iron radiation to bremsstrahlung
  !+ad_args  kappa95: input real :  plasma elongation at 95% surface
  !+ad_args  ralpne : input real :  thermal alpha density / electron density
  !+ad_args  rmajor : input real :  plasma major radius (m)
  !+ad_args  rminor : input real :  plasma minor radius (m)
  !+ad_args  rncne  : input real :  carbon density / electron density
  !+ad_args  rnfene : input real :  iron density / electron density
  !+ad_args  rnone  : input real :  oxygen density / electron density
  !+ad_args  ssync  : input real :  synchrotron wall reflectivity factor
  !+ad_args  ten    : input real :  density weighted average electron temperature (keV)
  !+ad_args  vol    : input real :  plasma volume (m3)
  !+ad_args  pbrem  : output real : bremsstrahlung radiation power/volume (MW/m3)
  !+ad_args  plrad  : output real : edge line radiation power/volume (MW/m3)
  !+ad_args  prad   : output real : total core radiation power/volume (MW/m3)
  !+ad_args  psync  : output real : synchrotron radiation power/volume (MW/m3)
  !+ad_desc  This routine finds the radiation power in MW/m3.
  !+ad_desc  The Bremsstrahlung and synchrotron powers are included.
  !+ad_prob  Promoting one exponent value to full double precision changes
  !+ad_prob  the resulting machine somewhat.
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  21/07/11 RK  Implemented Albajar for P_sync
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  Albajar, Nuclear Fusion 41 (2001) 665
  !+ad_docs  Fidone, Giruzzi, Granata, Nuclear Fusion 41 (2001) 1755
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: alphan, alphat, aspect, bt, &
       dene, deni, fbfe, kappa95, ralpne, rmajor, rminor, rncne, &
       rnfene, rnone, ssync, ten, vol
  real(kind(1.0D0)), intent(out) :: pbrem, plrad, prad, psync

  !  Local variables

  real(kind(1.0D0)) :: den20,fbc,fbhe,fbo,pbremdt,pbremz,pc,pfe, &
       phe,po,radexp,t10,vr,xfact,kfun,gfun,pao,de2o,teo,dum, &
       tbet,rpow,kap,pi

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  fbhe = 0.9D0
  fbc = 0.52D0
  fbo = 0.52D0

  radexp = (1.0D0 + alphan)**1.5D0 * sqrt(1.0D0 + alphan + alphat) / &
       (1.0D0 + 2.0D0*alphan + 0.5D0*alphat)
  den20 = dene/1.0D20
  t10 = ten/10.0D0

  !  D-T Bremsstrahlung

  pbremdt = 1.6D-2 * radexp * den20**2 * (deni/dene) * sqrt(t10)

  !  High Z Bremsstrahlung

  vr = rmajor * (rminor*(1.0D0 + kappa95)/2.0D0)**2 / (58.652D0*vol)
  phe = 65.8D0 * ralpne * (dene/7.0D19)**1.5D0 * vr
  pc  = 1120.0D0 * rncne * (dene/7.0D19)**1.5D0 * vr
  po  = 2240.0D0 * rnone * (dene/7.0D19)**1.5D0 * vr
  pfe = 44800.0D0 * rnfene * (dene/7.0D19)**2.5D0 * vr
  pbremz = fbhe*phe + fbc*pc + fbo*po + fbfe*pfe

  pbrem = pbremz + pbremdt

  !  Line radiation

  plrad = (1.0D0-fbhe)*phe + (1.0D0-fbc)*pc + (1.0D0-fbo)*po + &
       (1.0D0-fbfe)*pfe

  !  Synchrotron power

  !  Original code:
  !  xfact = 5.7D0/(aspect*sqrt(t10))
  !  psync = 1.3D-4*(bt*t10)**2.5D0 * (1.0D0+xfact)**0.5D0 &
  !          * (den20/rminor)**0.5D0 * (1.0D0-ssync)**0.5D0

  !  tbet is betaT in Albajar, not to be confused with plasma beta

  tbet = 2.0D0

  !  rpow is the (1-Rsyn) power dependence based on plasma shape
  !  (see Fidone)

  rpow = 0.62D0

  pi = acos(-1.0D0)
  kap = vol / (2.0D0 * pi**2 * rmajor * rminor**2)

  de2o = (1.0D0+alphan) * den20
  teo = ten * (1.0D0 + alphan + alphat)/(1.0D0+alphan)
  pao = 6.04D3 * (rminor*de2o)/bt
  gfun = 0.93D0 * ( 1.0D0 + 0.85D0*exp(-0.82D0 * rmajor/rminor) )
  kfun = (alphan + 3.87D0*alphat + 1.46D0)**(-0.79D0)
  kfun = kfun * (1.98D0+alphat)**1.36D0 * tbet**2.14D0
  kfun = kfun*(tbet**1.53D0 + 1.87D0*alphat - 0.16D0)**(-1.33D0)
  !+PJK 09/11/11 Converting 2nd occurrence of 0.41 to 0.41D0
  !+PJK 09/11/11 results in a different machine...
  dum = (1.0D0+0.12D0*(teo/(pao**0.41D0))*(1.0D0-ssync)**0.41)

  !  Very high T modification, from Fidone

  dum = dum**(-1.51D0)

  psync = 3.84D-8 * (1.0D0-ssync)**rpow * rmajor * rminor**1.38D0
  psync = psync * kap**0.79D0 * bt**2.62D0 * de2o**0.38D0
  psync = psync * teo *(16.0D0+teo)**2.61D0 * dum * gfun * kfun

  !  psync should be per unit volume; Albajar gives it as total

  psync = psync/vol      

  !  Total continuum radiation power

  prad = pbrem + psync

end subroutine radpwr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pohm(facoh,ires,kappa,plascur,rmajor,rminor,ten,vol, &
     zeff,pohmpv,rpfac,rplas)

  !+ad_name  pohm
  !+ad_summ  Ohmic power calculation
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  facoh  : input real :  fraction of plasma current produced inductively
  !+ad_args  ires   : input integer :  switch for neoclassical plasma resistivity
  !+ad_args  kappa  : input real :  plasma elongation
  !+ad_args  plascur: input real :  plasma current (A)
  !+ad_args  rmajor : input real :  plasma major radius (m)
  !+ad_args  rminor : input real :  plasma minor radius (m)
  !+ad_args  ten    : input real :  density weighted average electron temperature (keV)
  !+ad_args  vol    : input real :  plasma volume (m3)
  !+ad_args  zeff   : input real :  plasma effective charge
  !+ad_args  pohmpv : output real : ohmic heating power per unit volume (MW/m3)
  !+ad_args  rpfac  : output real : neoclassical resistivity enhancement factor
  !+ad_args  rplas  : output real : plasma resistance (ohm)
  !+ad_desc  This routine finds the ohmic heating power per unit volume.
  !+ad_desc  The expression is a good fit for alphan = 0.5, alphat = 1.0,
  !+ad_desc  alphaj = 1.5, aspect = 2.5 -- 4.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  25/07/11 PJK Correction to facoh coding
  !+ad_hist  09/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: ires
  real(kind(1.0D0)), intent(in) :: facoh, kappa, plascur, rmajor, &
       rminor, ten, vol, zeff
  real(kind(1.0D0)), intent(out) :: pohmpv, rpfac, rplas

  !  Local variables

  real(kind(1.0D0)) :: t10

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Density weighted electron temperature in 10 keV units
  !  (not sure why ten is used instead of te)

  t10 = ten/10.0D0

  !  Plasma resistance

  rplas = 2.15D-9 * zeff*rmajor / (kappa*rminor**2 * t10**1.5D0)

  !  Neo-classical resistivity enhancement factor
  !  Taken from  N. A. Uckan et al, Fusion Technology 13 (1988) p.411.
  !  The expression is valid for aspect ratios in the range 2.5--4.

  if (ires == 1) then
     rpfac = 4.3D0 - 0.6D0*rmajor/rminor
     rplas = rplas * rpfac
  end if

  !  Check to see if plasma resistance is negative
  !  (possible if ires = 1 and aspect ratio is too high)

  if (rplas <= 0.0D0) then
     write(*,*) 'Warning in routine POHM:'
     write(*,*) 'Plasma resistance, rplas = ',rplas
     write(*,*) 'PROCESS continuing...'
  end if

  !  Ohmic heating power per unit volume
  !  Corrected from: pohmpv = (facoh*plascur)**2 * ...

  pohmpv = facoh * plascur**2 * rplas * 1.0D-6/vol

end subroutine pohm

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pthresh(dene,dnla,bt,rmajor,kappa,pthrmw)

  !+ad_name  pthresh
  !+ad_summ  L-mode to H-mode power threshold calculation
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  dene   : input real :  volume-averaged electron density (/m3)
  !+ad_args  dnla   : input real :  line-averaged electron density (/m3)
  !+ad_args  bt     : input real :  toroidal field on axis (T)
  !+ad_args  rmajor : input real :  plasma major radius (m)
  !+ad_args  kappa  : input real :  plasma elongation
  !+ad_args  pthrmw(5) : output real array : power threshold (different scalings)
  !+ad_desc  This routine calculates the power threshold for the L-mode to
  !+ad_desc  H-mode transition.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  17/07/98 PJK New routine
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  ITER Physics Design Description Document, p.2-2
  !+ad_docs  ITER-FDR Plasma Performance Assessments, p.III-9
  !+ad_docs  Snipes, 24th EPS Conference, Berchtesgaden 1997, p.961
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: dene, dnla, bt, rmajor, kappa
  real(kind(1.0D0)), dimension(5), intent(out) :: pthrmw

  !  Local variables

  real(kind(1.0D0)) :: dene20,dnla20

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dene20 = 1.0D-20*dene
  dnla20 = 1.0D-20*dnla

  !  ITER-DDD, D.Boucher
  !  Fit to 1996 H-mode power threshold database: nominal

  pthrmw(1) = 0.45D0 * dene20**0.75D0 * bt * rmajor**2

  !  Fit to 1996 H-mode power threshold database: upper bound

  pthrmw(2) = 0.37D0 * dene20 * bt * rmajor**2.5D0

  !  Fit to 1996 H-mode power threshold database: lower bound

  pthrmw(3) = 0.54D0 * dene20**0.5D0 * bt * rmajor**1.5D0

  !  J. A. Snipes, ITER H-mode Threshold Database Working Group,
  !  Controlled Fusion and Plasma Physics, 24th EPS Conference,
  !  Berchtesgaden, June 1997, vol.21A, part III, p.961

  pthrmw(4) = 0.65D0 * dnla20**0.93D0 * bt**0.86D0 * rmajor**2.15D0

  pthrmw(5) = 0.42D0 * dnla20**0.80D0 * bt**0.90D0 * rmajor**1.99D0 &
       * kappa**0.76D0

end subroutine pthresh

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine igmarcal(nout)

  !+ad_name  igmarcal
  !+ad_summ  Routine to calculate ignition margin
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout   : input integer : Fortran output unit identifier
  !+ad_desc  This routine calculates the ignition margin at the final point
  !+ad_desc  with different scalings.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  phydat.h90
  !+ad_call  cdriv.h90
  !+ad_call  labels.h90
  !+ad_call  osections.h90
  !+ad_call  fhfac
  !+ad_call  oblnkl
  !+ad_call  osubhd
  !+ad_call  pcond
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  01/04/98 PJK Modified PCOND arguments
  !+ad_hist  30/06/98 PJK Modified PCOND arguments
  !+ad_hist  19/01/99 PJK Modified PCOND arguments
  !+ad_hist  17/07/01 PJK Modified PCOND arguments
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'cdriv.h90'
  include 'labels.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  integer :: iisc
  real(kind(1.0D0)), parameter :: d2 = 2.0D0
  real(kind(1.0D0)) :: powerhtz, ptrez, ptriz, &
       taueez, taueezz, taueffz, taueiz

  !  External functions

  real(kind(1.0D0)), external :: fhfac

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Only produce output if required

  if (sect03 == 0) return

  !  Start output

  call osubhd(nout,'Confinement times, and required H-factors :')

  write(nout,10)
10 format(t5,'scaling law', t30,'confinement time (s)', &
        t55,'H-factor for')

  write(nout,20)
20 format(t34,'for H = 2',t54,'power balance')

  call oblnkl(nout)

  !  Calculate power balances for all scaling laws assuming H = 2

  do iisc = 1,ipnlaws
     call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,d2, &
          iinvqd,iisc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
          plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol, &
          xarea,zeff,ptrez,ptriz,taueez,taueiz,taueffz,powerhtz)
     hfac(iisc) = fhfac(iisc)

     write(nout,30) tauscl(iisc),taueez,hfac(iisc)
  end do
30 format(t2,a24,t34,f7.3,t58,f7.3)

end subroutine igmarcal

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function fhfac(is)

  !+ad_name  fhfac
  !+ad_summ  Function to find H-factor for power balance
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  is : input integer : confinement time scaling law of interest
  !+ad_desc  This function calculates the H-factor required for power balance,
  !+ad_desc  using the given energy confinement scaling law.
  !+ad_prob  None
  !+ad_call  fhz
  !+ad_call  zeroin
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: fhfac

  !  Arguments

  integer, intent(in) :: is

  !  Variables passed via COMMON
  !  iscz   : (OUTPUT) Number of confinement time scaling law of interest

  integer :: iscz
  COMMON /chfac/ iscz

  !  Local variables

  real(kind(1.0D0)) :: abserr = 0.003D0  !  numerical tolerance
  real(kind(1.0D0)) :: xlow = 0.01D0     !  minimum bound on H-factor
  real(kind(1.0D0)) :: xhigh = 100.0D0   !  maximum bound on H-factor

  !  External functions

  real(kind(1.0D0)), external :: fhz, zeroin

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  iscz = is

  !  Find value of H-factor for which function FHZ is zero
  !  (this occurs at power balance)

  fhfac = zeroin(xlow,xhigh,fhz,abserr)

end function fhfac

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function fhz(hhh)

  !+ad_name  fhz
  !+ad_summ  Function used to find power balance
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  hhh : input real : test value for confinement time H-factor
  !+ad_desc  This function is used to find power balance.
  !+ad_desc  <CODE>FHZ</CODE> is zero at power balance, which is achieved
  !+ad_desc  using routine <A HREF="zeroin.html">ZEROIN</A> to adjust the
  !+ad_desc  value of <CODE>hhh</CODE>, the confinement time H-factor.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  phydat.h90
  !+ad_call  cdriv.h90
  !+ad_call  pcond
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  01/04/98 PJK Modified PCOND arguments, and adding coding for
  !+ad_hisc               use of IGNITE
  !+ad_hist  30/06/98 PJK Modified PCOND arguments
  !+ad_hist  19/01/99 PJK Modified PCOND arguments
  !+ad_hist  16/07/01 PJK Modified PCOND arguments
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: fhz

  include 'param.h90'
  include 'phydat.h90'
  include 'cdriv.h90'

  !  Arguments

  real(kind(1.0D0)), intent(in) :: hhh

  !  Variables passed via COMMON
  !  iscz   : (INPUT)  confinement time scaling law of interest

  integer :: iscz
  COMMON /chfac/ iscz

  !  Local variables

  real(kind(1.0D0)) :: powerhtz,ptrez,ptriz,taueez,taueezz,taueiz,taueffz

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hhh, &
       iinvqd,iscz,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
       plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol, &
       xarea,zeff,ptrez,ptriz,taueezz,taueiz,taueffz,powerhtz)

  if (iscz < 3) then  !  only laws 1 and 2 are affected???
     ptrez = ptrez/hhh
     ptriz = ptriz/hhh
  end if

  !  At power balance, fhz is zero.
  !  Take into account whether injected power is included in tau_e
  !  calculation (i.e. whether device is ignited)

  if (ignite == 0) then
     fhz = ptrez + ptriz + prad - palp - pcharge &
          - 1.0D-6*(pinje+pinji)/vol - pohmpv
  else
     fhz = ptrez + ptriz + prad - palp - pcharge - pohmpv
  end if

end function fhz

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
     ealpha,enbeam,fdeut,ftr,ftritbm,sigvdt,ten,tin,vol,zeffai, &
     betanb,dnbeam2,palpnb)

  !+ad_name  beamfus
  !+ad_summ  Routine to calculate beam slowing down properties
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  beamfus0: input real : multiplier for beam-background fusion calculation
  !+ad_args  betbm0 : input real :  leading coefficient for neutral beam beta fraction
  !+ad_args  bp     : input real :  poloidal field (T)
  !+ad_args  bt     : input real :  toroidal field on axis (T)
  !+ad_args  cnbeam : input real :  neutral beam current (A)
  !+ad_args  dene   : input real :  electron density (/m3)
  !+ad_args  deni   : input real :  fuel ion density (/m3)
  !+ad_args  dlamie : input real :  ion-electron coulomb logarithm
  !+ad_args  ealpha : input real :  alpha particle birth energy (D-T) (keV)
  !+ad_args  enbeam : input real :  neutral beam energy (keV)
  !+ad_args  fdeut  : input real :  deuterium fraction of main plasma
  !+ad_args  ftr    : input real :  tritium fraction of main plasma
  !+ad_args  ftritbm: input real :  tritium fraction of neutral beam
  !+ad_args  sigvdt : input real :  profile averaged <sigma v DT> (m3/s)
  !+ad_args  ten    : input real :  density weighted average electron temperature (keV)
  !+ad_args  tin    : input real :  density weighted average ion temperature (keV)
  !+ad_args  vol    : input real :  plasma volume (m3)
  !+ad_args  zeffai : input real :  density weighted plasma effective charge
  !+ad_args  betanb : output real : neutral beam beta component
  !+ad_args  dnbeam2: output real : hot beam ion density (/m3)
  !+ad_args  palpnb : output real : alpha power from hot neutral beam ions (MW)
  !+ad_desc  This routine calculates the beam slowing down properties.
  !+ad_prob  None
  !+ad_call  beamcalc
  !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  05/12/95 PJK Added ealpha to argument list
  !+ad_hist  11/12/95 PJK Added fdeut to argument list
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: beamfus0, betbm0, bp, bt, cnbeam, &
       dene, deni, dlamie, ealpha, enbeam, fdeut, ftr, ftritbm, &
       sigvdt, ten, tin, vol, zeffai
  real(kind(1.0D0)), intent(out) :: betanb, dnbeam2, palpnb

  !  Local variables

  real(kind(1.0D0)) :: denid,denit,ecritd,ecritt,ehotnb,palpdb, &
       palptb,tausl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Velocity slowing down time

  tausl = 1.99D19 * (2.0D0*(1.0D0-ftritbm) + 3.0D0*ftritbm) * &
       ten**1.5D0 / dene / dlamie

  !  Critical energy for electron/ion slowing down of the beam ion
  !  (deuterium and tritium neutral beams, respectively) (keV)

  ecritd = 14.8D0 * ten * 2.0D0 * zeffai**0.6666D0 * &
       (dlamie+4.0D0)/dlamie
  ecritt = ecritd * 1.5D0

  !  Deuterium and tritium ion densities

  denid = deni * fdeut
  denit = deni * ftr

  !  Perform beam calculations

  call beamcalc(denid,denit,ealpha,enbeam,ecritd,ecritt,tausl, &
       ftritbm,cnbeam,tin,vol,sigvdt,palpdb,palptb,dnbeam2,ehotnb)

  !  Neutral beam alpha power

  palpnb = beamfus0 * (palpdb + palptb)

  !  Neutral beam beta

  betanb = betbm0 * 4.03D-22 * 0.66666D0 * dnbeam2 * ehotnb / &
       (bt**2 + bp**2)

end subroutine beamfus

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine beamcalc(nd,nt,ealpha,ebeam,ecritd,ecritt,tausbme, &
     ftritbm,ibeam,ti,vol,svdt,palfdb,palftb,nhot,ehot)

  !+ad_name  beamcalc
  !+ad_summ  Neutral beam alpha power and ion energy
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  palphabm
  !+ad_cont  sgvhot
  !+ad_cont  xbrak
  !+ad_args  ealpha : input real :  alpha particle birth energy (D-T) (keV)
  !+ad_args  ebeam  : input real :  beam energy (keV)
  !+ad_args  ecritd : input real :  critical energy for electron/ion slowing down of
  !+ad_argc                         the beam ion (deuterium neutral beam) (keV)
  !+ad_args  ecritt : input real :  critical energy for beam slowing down
  !+ad_argc                         (tritium neutral beam) (keV)
  !+ad_args  ftritbm: input real :  beam tritium fraction (0.0 = deuterium beam)
  !+ad_args  ibeam  : input real :  beam current (A)
  !+ad_args  nd     : input real :  thermal deuterium density (/m3)
  !+ad_args  nt     : input real :  thermal tritium density   (/m3)
  !+ad_args  svdt   : input real :  profile averaged <sigma v DT> (m3/s)
  !+ad_args  tausbme: input real :  beam ion slowing down time on electrons (s)
  !+ad_args  ti     : input real :  thermal ion temperature (keV)
  !+ad_args  vol    : input real :  plasma volume (m3) (95% flux surface)
  !+ad_args  ehot   : output real : average hot beam ion energy (keV)
  !+ad_args  nhot   : output real : hot beam ion density (/m3)
  !+ad_args  palfdb : output real : alpha power from deut. beam-background fusion (MW)
  !+ad_args  palftb : output real : alpha power from trit. beam-background fusion (MW)
  !+ad_desc  This routine calculates the neutral beam alpha power and ion energy.
  !+ad_prob  None
  !+ad_call  palphabm
  !+ad_call  sgvhot
  !+ad_call  xbrak
  !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  05/12/95 PJK Added ealpha to argument list
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: ealpha, ebeam, ecritd, ecritt, &
       ftritbm, ibeam, nd, nt, svdt, tausbme, ti, vol
  real(kind(1.0D0)), intent(out) :: ehot, nhot, palfdb, palftb

  !  Local variables

  integer :: iabm
  real(kind(1.0D0)) :: ebmratd,ebmratt,ehotd,ehott,ifbmd,ifbmt, &
       ndhot,nhotmsd,nhotmst,nthot,presd,prest,s0d,s0t,svdhotn, &
       svthotn,tauseffd,tausefft,vcds,vcritd,vcritt,vcts,xcoefd, &
       xcoeft
  real(kind(1.0D0)) :: atmd,atmdt,atmt,echarge,epsabs,epsrel,xmprotn

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise shared variables

  atmd = 2.0D0   !  atomic mass of deuterium
  atmdt = 2.5D0  !  average atomic mass of D-T
  atmt = 3.0D0   !  atomic mass of tritium
  echarge = 1.6022D-19  !  electron charge (Coulomb)
  epsabs = 1.0D-7  !  absolute error
  epsrel = 1.0D-7  !  relative error
  xmprotn = 1.6726D-27  !  proton mass (kg)

  !  D and T beam current fractions

  ifbmd = ibeam * (1.0D0 - ftritbm)
  ifbmt = ibeam * ftritbm

  ebmratd = ebeam/ecritd
  vcritd = sqrt(2.0D0*echarge*1000.0D0*ecritd/(xmprotn*atmd))
  tauseffd = tausbme/3.0D0 * log(1.0D0+(ebmratd)**1.5D0)
  nhotmsd = (1.0D0-ftritbm) * ibeam * tauseffd/(echarge * vol)

  ebmratt = ebeam/ecritt
  vcritt = sqrt(2.0D0*echarge*1000.0D0*ecritt/(xmprotn*atmt))
  tausefft = tausbme/3.0D0 * log(1.0D0+(ebmratt)**1.5D0)
  nhotmst = ftritbm * ibeam * tausefft/(echarge * vol)

  nhot = nhotmsd + nhotmst
  ndhot = nhotmsd
  nthot = nhotmst

  !  Average hot ion energy from Deng & Emmert, UWFDM-718, Jan 87

  vcds = 2.0D0 * ecritd * echarge * 1000.0D0/(2.0D0 * xmprotn)
  vcts = 2.0D0 * ecritt * echarge * 1000.0D0/(3.0D0 * xmprotn)

  s0d = ifbmd/(echarge * vol)
  s0t = ifbmt/(echarge * vol)

  xcoefd = atmd * xmprotn * tausbme * vcds * s0d / &
       (echarge * 1000.0D0 * 3.0D0)
  xcoeft = atmt * xmprotn * tausbme * vcts * s0t / &
       (echarge * 1000.0D0 * 3.0D0)

  presd = xcoefd * xbrak(ebeam,ecritd)
  prest = xcoeft * xbrak(ebeam,ecritt)

  ehotd = 1.5D0 * presd/ndhot
  ehott = 1.5D0 * prest/nthot
  ehot = (ndhot*ehotd + nthot*ehott)/nhot

  iabm = 2 ; svdhotn = 1.0D-4 * sgvhot(iabm,vcritd,ebeam)
  iabm = 3 ; svthotn = 1.0D-4 * sgvhot(iabm,vcritt,ebeam)

  palfdb = palphabm(ealpha,ndhot,nt,svdhotn,vol,ti,svdt)
  palftb = palphabm(ealpha,nthot,nd,svthotn,vol,ti,svdt)

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function xbrak(e0,ec)

    !+ad_name  xbrak
    !+ad_summ  Hot ion energy parameter
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  e0 : input real :  neutral beam energy (keV)
    !+ad_args  ec : input real :  critical energy for electron/ion slowing down of
    !+ad_argc                     the beam ion (keV)
    !+ad_desc  This routine calculates something to do with the hot ion energy...
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: xbrak

    !  Arguments

    real(kind(1.0D0)), intent(in) :: e0, ec

    !  Local variables

    real(kind(1.0D0)) :: ans,t1,t2,t3,t4,xarg,xc,xcs

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xcs = e0/ec
    xc = sqrt(xcs)

    t1 = xcs/2.0D0
    t2 = (log((xcs + 2.0D0*xc + 1.0D0)/(xcs - xc + 1.0D0)))/6.0D0

    xarg = (2.0D0*xc -1.0D0)/sqrt(3.0D0)
    t3 = (atan(xarg))/sqrt(3.0D0)
    t4 = 0.3022999D0

    ans = t1 + t2 - t3 - t4
    xbrak = ans

  end function xbrak

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function palphabm(ealpha,nbm,nblk,sigv,vol,ti,svdt)

    !+ad_name  palphabm
    !+ad_summ  Alpha power from beam-background fusion
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  ealpha : input real :  alpha particle birth energy (D-T) (keV)
    !+ad_args  nblk   : input real :  thermal ion density (/m3)
    !+ad_args  nbm    : input real :  hot beam ion density (/m3)
    !+ad_args  sigv   : input real :  hot beam fusion reaction rate (m3/s)
    !+ad_args  svdt   : input real :  profile averaged <sigma v DT> (m3/s)
    !+ad_args  ti     : input real :  thermal ion temperature (keV)
    !+ad_args  vol    : input real :  plasma volume (m3)
    !+ad_desc  This routine calculates the alpha power from
    !+ad_desc  beam-background fusion.
    !+ad_prob  None
    !+ad_call  svfdt
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  05/12/95 PJK Moved ealpha to argument list
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: palphabm

    !  Arguments

    real(kind(1.0D0)) ealpha,nblk,nbm,sigv,svdt,ti,vol

    !  Local variables

    real(kind(1.0D0)) :: ans, ratio

    !  External functions

    real(kind(1.0D0)), external :: svfdt

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ratio = svdt/svfdt(ti)
    ans = echarge/1000.0D0 * nbm * nblk * sigv * ealpha * vol * ratio

    palphabm = ans

  end function palphabm

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function sgvhot(iabm,vcrx,ebeam)

    !+ad_name  sgvhot
    !+ad_summ  Hot beam fusion reaction rate
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  ebeam  : input real :  neutral beam energy (keV)
    !+ad_args  iabm   : input integer : switch denoting type of ion (2=D,3=T)
    !+ad_args  vcrx   : input real :  critical velocity for electron/ion slowing down of
    !+ad_argc                         the beam ion (m/s)
    !+ad_desc  This routine calculates the hot beam fusion reaction rate in m3/s.
    !+ad_prob  None
    !+ad_call  fsv
    !+ad_call  quanc8
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: sgvhot

    !  Arguments

    integer, intent(in) :: iabm
    real(kind(1.0D0)), intent(in) :: ebeam, vcrx

    !  Variables passed via COMMON
    !  echrge : (OUTPUT) electron charge (Coulomb)
    !  promass: (OUTPUT) proton mass (kg)
    !  vcritx : (OUTPUT) critical velocity for electron/ion slowing down of
    !                    the beam ion (m/s)

    real(kind(1.0D0)) :: echrge,promass,vcritx
    COMMON /fvcrit/ vcritx,promass,echrge

    !  Local variables

    integer :: nofun
    real(kind(1.0D0)) :: abm,abserr,epsabs1,flag,svint,t1,t2, &
         vbeam,vbeams,xv

    !  External functions

    real(kind(1.0D0)), external :: fsv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    epsabs1 = 1.0D-33

    if (iabm == 2) then
       abm = atmd
    else if (iabm == 3) then
       abm = atmt
    else
       write(*,*) 'Error in routine SGVHOT:'
       write(*,*) 'Illegal value for IABM, = ',iabm
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Initialise global variables

    echrge = echarge
    promass = xmprotn
    vcritx = vcrx

    !  Beam velocity

    vbeams = ebeam * echarge * 1000.0D0 * 2.0D0/(abm * xmprotn)
    vbeam = sqrt(vbeams)

    xv = vbeam/vcrx
    t1 = 3.0D0 * vcrx/log(1.0D0+(xv**3))

    call quanc8(fsv,0.0D0,xv,epsabs1,epsrel,svint,abserr,nofun,flag)
    t2 = svint

    sgvhot = t1 * t2

  end function sgvhot

end subroutine beamcalc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function fsv(u)

  !+ad_name  fsv
  !+ad_summ  Integrand function for the hot beam fusion reaction rate
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  sigbmfus
  !+ad_args  u : input real : abscissa of integration, = ratio of beam velocity
  !+ad_argc                   to the critical velocity
  !+ad_desc  This is the integrand function for the hot beam fusion reaction rate.
  !+ad_prob  None
  !+ad_call  sigbmfus
  !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: fsv

  !  Arguments

  real(kind(1.0D0)), intent(in) :: u

  !  Variables passed via COMMON
  !  echrge : (INPUT)  electron charge (Coulomb)
  !  promass: (INPUT)  proton mass (kg)
  !  vcritx : (INPUT)  critical velocity for electron/ion slowing down of
  !                    the beam ion (m/s)

  real(kind(1.0D0)) :: echrge,promass,vcritx
  common /fvcrit/ vcritx,promass,echrge

  !  Local variables

  real(kind(1.0D0)) :: t1,t2,xvc,xvcs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  t1 = (u**3)/(1.0D0+u**3)

  xvc = vcritx*u
  xvcs = xvc * xvc * promass/(echrge * 1000.0D0)
  t2 = sigbmfus(xvcs)

  fsv = t1 * t2

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function sigbmfus(vrelsq)

    !+ad_name  sigbmfus
    !+ad_summ  Fusion reaction cross-section
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  vrelsq : input real :  square of the speed of the beam ion (keV/amu)
    !+ad_desc  This function evaluates the fusion reaction cross-section as a
    !+ad_desc  function of beam ion velocity (squared).
    !+ad_desc  The functional form of the cross-section is in terms of the equivalent
    !+ad_desc  deuterium energy, i.e. for a tritium beam at 500 keV the energy
    !+ad_desc  used in the cross-section function is 333 keV.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: sigbmfus

    !  Arguments

    real(kind(1.0D0)), intent(in) :: vrelsq

    !  Local variables

    real(kind(1.0D0)) :: a1,a2,a3,a4,a5,ans,atmd,ebm,t1,t2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    a1 = 45.95D0
    a2 = 5.02D4
    a3 = 1.368D-2
    a4 = 1.076D0
    a5 = 4.09D2

    !  Deuterium atomic mass

    atmd = 2.0D0

    !  Beam kinetic energy

    ebm = 0.5D0 * atmd * vrelsq

    !  Set limits on cross-section at low and high beam energies

    if (ebm < 10.0D0) then
       sigbmfus = 1.0D-27
    else if (ebm > 1.0D4) then
       sigbmfus = 8.0D-26
    else
       t1 = a2/(1.0D0 + (a3 * ebm - a4)**2) + a5
       t2 = ebm * (exp (a1/sqrt(ebm)) - 1.0D0)
       sigbmfus = 1.0D-24 * t1/t2
    end if

  end function sigbmfus

end function fsv

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function fnewbs(alphan,alphat,betat,bt,dene,plascur,q95,q0,rmajor, &
     rminor,ten,zeff)

  !+ad_name  fnewbs
  !+ad_summ  Bootstrap current fraction from Nevins et al scaling
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphan : input real :  density profile index
  !+ad_args  alphat : input real :  temperature profile index
  !+ad_args  betat  : input real :  total plasma beta (with respect to the toroidal
  !+ad_argc                         field)
  !+ad_args  bt     : input real :  toroidal field on axis (T)
  !+ad_args  dene   : input real :  electron density (/m3)
  !+ad_args  plascur: input real :  plasma current (A)
  !+ad_args  q0     : input real :  central safety factor
  !+ad_args  q95    : input real :  safety factor at 95% surface
  !+ad_args  rmajor : input real :  plasma major radius (m)
  !+ad_args  rminor : input real :  plasma minor radius (m)
  !+ad_args  ten    : input real :  density weighted average plasma temperature (keV)
  !+ad_args  zeff   : input real :  plasma effective charge
  !+ad_desc  This function calculates the bootstrap current fraction,
  !+ad_desc  using the Nevins et al method, 4/11/90.
  !+ad_prob  None
  !+ad_call  bsinteg
  !+ad_call  quanc8
  !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: fnewbs

  !  Arguments

  real(kind(1.0D0)), intent(in) :: alphan,alphat,betat,bt,dene,plascur, &
       q0,q95,rmajor,rminor,ten,zeff

  !  Variables passed via COMMON
  !  alphanz: (OUTPUT) density profile index
  !  alphatz: (OUTPUT) temperature profile index
  !  betaz  : (OUTPUT) total plasma beta (with respect to the toroidal
  !                    field)
  !  btz    : (OUTPUT) toroidal field on axis (T)
  !  denez  : (OUTPUT) electron density (/m3)
  !  plascurz:(OUTPUT) plasma current (MA)
  !  qaxis  : (OUTPUT) central safety factor
  !  qpsi   : (OUTPUT) safety factor at 95% surface
  !  rmajorz: (OUTPUT) plasma major radius (m)
  !  rminorz: (OUTPUT) plasma minor radius (m)
  !  tez    : (OUTPUT) density weighted average plasma temperature (keV)
  !  zeffz  : (OUTPUT) plasma effective charge

  real(kind(1.0D0)) :: alphanz,alphatz,betaz,btz,denez,plascurz,qaxis, &
       qpsi,rmajorz,rminorz,tez,zeffz
  COMMON/bss/ alphanz,alphatz,betaz,btz,denez,plascurz,qpsi,qaxis, &
       rmajorz,tez,rminorz,zeffz

  !  Local variables

  integer :: nofun
  real(kind(1.0D0)), parameter :: rmu0 = 1.25664D-6
  real(kind(1.0D0)) :: aibs,ainteg,betae0,dum1,fibs,flag

  !  External functions

  real(kind(1.0D0)), external :: bsinteg

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise global variables

  alphanz = alphan
  alphatz = alphat
  betaz   = betat
  btz     = bt
  denez   = dene
  plascurz = plascur/1.0D6
  qaxis   = q0
  qpsi    = q95
  rmajorz = rmajor
  rminorz = rminor
  tez     = ten
  zeffz   = zeff

  !  Calculate peak electron beta

  betae0 = denez * tez *1.6022D-16/ ( btz**2 /(2.0D0*rmu0) ) * &
       (1.0D0+alphanz+alphatz)

  !  Call integration routine

  call quanc8(bsinteg,0.0D0,0.999D0,0.001D0,0.001D0,ainteg,dum1, &
       nofun,flag)

  !  Calculate bootstrap current and fraction

  aibs = 2.5D0 * betae0 * rmajor * btz * qpsi * ainteg
  fibs = aibs / plascurz

  fnewbs = fibs

end function fnewbs

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function bsinteg(y)

  !+ad_name  bsinteg
  !+ad_summ  Integrand function for Nevins et al bootstrap current scaling
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  y : input real : abscissa of integration, = normalised minor radius
  !+ad_desc  This function calculates the integrand function for the
  !+ad_desc  Nevins et al bootstrap current scaling, 4/11/90.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: bsinteg

  !  Arguments

  real(kind(1.0D0)), intent(in) :: y

  !  Variables passed via COMMON
  !  alphanz: (INPUT)  density profile index
  !  alphatz: (INPUT)  temperature profile index
  !  betaz  : (INPUT)  total plasma beta (with respect to the toroidal
  !                    field)
  !  btz    : (INPUT)  toroidal field on axis (T)
  !  denez  : (INPUT)  electron density (/m3)
  !  plascurz:(INPUT)  plasma current (MA)
  !  qaxis  : (INPUT)  central safety factor
  !  qpsi   : (INPUT)  safety factor at 95% surface
  !  rmajorz: (INPUT)  plasma major radius (m)
  !  rminorz: (INPUT)  plasma minor radius (m)
  !  tez    : (INPUT)  density weighted average plasma temperature (keV)
  !  zeffz  : (INPUT)  plasma effective charge

  real(kind(1.0D0)) :: alphanz,alphatz,betaz,btz,denez,plascurz,qaxis, &
       qpsi,rmajorz,rminorz,tez,zeffz
  COMMON/bss/ alphanz,alphatz,betaz,btz,denez,plascurz,qpsi,qaxis, &
       rmajorz,tez,rminorz,zeffz

  !  Local variables

  real(kind(1.0D0)), parameter :: rmu0 = 1.25664D-6
  real(kind(1.0D0)) :: alphai,al1,al2,a1,a2,betae,c1,c2,c3, &
       d,del,pratio,q,x,z

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Constants for fit to q-profile

  c1 = 1.0D0
  c2 = 1.0D0
  c3 = 1.0D0

  !  Compute average electron beta

  betae = denez*tez*1.6022D-16/(btz**2/(2.0D0*rmu0))

  del = rminorz*sqrt(y)/rmajorz
  x = (1.46D0*sqrt(del) + 2.4D0*del)/(1.0D0 - del)**1.5D0
  z = zeffz
  d = 1.414D0*z + z*z + x*(0.754D0 + 2.657D0*z + 2.0D0*z*z) &
       + x*x*(0.348D0 + 1.243D0*z + z*z)
  al2 = -x*(0.884D0 + 2.074D0*z)/d
  a2 = alphatz*(1.0D0-y)**(alphanz+alphatz-1.0D0)
  alphai = -1.172D0/(1.0D0+ 0.462D0*x)
  a1 = (alphanz+alphatz)*(1.0D0-y)**(alphanz+alphatz-1.0D0)
  al1 = x*(0.754D0+2.21D0*z+z*z+x*(0.348D0+1.243D0*z+z*z))/d

  !  q-profile

  q = qaxis + (qpsi-qaxis)*(c1*y + c2*y*y + c3*y**3)/(c1+c2+c3)

  pratio = (betaz - betae) / betae

  bsinteg = (q/qpsi)*(al1*(a1 + pratio*(a1+alphai*a2) ) + al2*a2 )

end function bsinteg

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function culbst(alphaj,alphan,alphat,beta,betpth,q0,qpsi, &
     rmajor,rminor,itart)

  !+ad_name  culbst
  !+ad_summ  Bootstrap current fraction from Wilson et al scaling
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  alphaj  : input real :  current profile index
  !+ad_args  alphan  : input real :  density profile index
  !+ad_args  alphat  : input real :  temperature profile index
  !+ad_args  beta    : input real :  total beta
  !+ad_args  betpth  : input real :  thermal component of poloidal beta
  !+ad_args  q0      : input real :  safety factor on axis
  !+ad_args  qpsi    : input real :  edge safety factor
  !+ad_args  rmajor  : input real :  major radius (m)
  !+ad_args  rminor  : input real :  minor radius (m)
  !+ad_args  itart   : input integer :  switch denoting tight aspect ratio option
  !+ad_desc  This function calculates the bootstrap current fraction
  !+ad_desc  using the algorithm written by Howard Wilson and described
  !+ad_desc  in AEA FUS 172.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
  !+ad_hist  14/05/96 PJK Modified to use THERMAL poloidal beta, and
  !+ad_hisc               added diamagnetic term at tight aspect ratio
  !+ad_hist  10/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: culbst

  !  Arguments

  integer, intent(in) :: itart
  real(kind(1.0D0)), intent(in) :: alphaj,alphan,alphat,beta,betpth, &
       q0,qpsi,rmajor,rminor

  !  Local variables

  integer :: i
  real(kind(1.0D0)), dimension(12) :: a, b
  real(kind(1.0D0)) :: aj,alfpnw,alftnw,alphap,eps1,r1,r2, &
       saj,seps1,sss,termj,termp,termt,term1,term2,z

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Convert density profile index to pressure profile index.
  !  From ideal gas law : p = nkT

  alphap = alphan + alphat

  !  Check for illegal argument values

  if (alphaj <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for alphaj, = ',alphaj
     write(*,*) 'PROCESS stopping.'
     stop
  end if
  if (alphap <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for alphap, = ',alphap
     write(*,*) 'PROCESS stopping.'
     stop
  end if
  if (alphat <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for alphat, = ',alphat
     write(*,*) 'PROCESS stopping.'
     stop
  end if
  if (qpsi <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for qpsi, = ',qpsi
     write(*,*) 'PROCESS stopping.'
     stop
  end if
  if (q0 <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for q0, = ',q0
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  alphap, alphat and alphaj are indices relevant to profiles of
  !  the form
  !             p = p0.(1-(r/a)**2)**alphap, etc.
  !  
  !  Convert these indices to those relevant to profiles of the form
  !             p = p0.psi**alfpnw, etc.

  term1 = log(0.5D0)
  term2 = log(q0/qpsi)

  termp = 1.0D0-0.5D0**(1.0D0/alphap)
  termt = 1.0D0-0.5D0**(1.0D0/alphat)
  termj = 1.0D0-0.5D0**(1.0D0/alphaj)

  alfpnw = term1/log( log( (q0+(qpsi-q0)*termp)/qpsi )/term2)
  alftnw = term1/log( log( (q0+(qpsi-q0)*termt)/qpsi )/term2)
  aj     = term1/log( log( (q0+(qpsi-q0)*termj)/qpsi )/term2)

  !  Crude check for NaN errors...

  if (aj /= aj) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for aj, = ',aj
     write(*,*) 'PROCESS stopping.'
     stop
  end if
  if (alfpnw /= alfpnw) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for alfpnw, = ',alfpnw
     write(*,*) 'PROCESS stopping.'
     stop
  end if
  if (alftnw /= alftnw) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for alftnw, = ',alftnw
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  if (aj <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for aj, = ',aj
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  Ratio of ionic charge to electron charge

  z = 1.0D0

  !  Inverse aspect ratio: r2 = maximum plasma radius, r1 = minimum

  r2 = rmajor+rminor
  r1 = rmajor-rminor
  eps1 = (r2-r1)/(r2+r1)

  if (eps1 <= 0.0D0) then
     write(*,*) 'Error in routine CULBST:'
     write(*,*) 'Illegal value for eps1, = ',eps1
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  Coefficients fitted using least squares techniques

  saj = sqrt(aj)

  a(1)  =    1.41D0*(1.0D0-0.28D0*saj)*(1.0D0+0.12D0/z)
  a(2)  =    0.36D0*(1.0D0-0.59D0*saj)*(1.0D0+0.8D0/z)
  a(3)  =   -0.27D0*(1.0D0-0.47D0*saj)*(1.0D0+3.0D0/z)
  a(4)  =  0.0053D0*(1.0D0+5.0D0/z)
  a(5)  =   -0.93D0*(1.0D0-0.34D0*saj)*(1.0D0+0.15D0/z)
  a(6)  =   -0.26D0*(1.0D0-0.57D0*saj)*(1.0D0-0.27D0*z)
  a(7)  =   0.064D0*(1.0D0-0.6D0*aj+0.15D0*aj*aj)*(1.0D0+7.6D0/z)
  a(8)  = -0.0011D0*(1.0D0+9.0D0/z)
  a(9)  =   -0.33D0*(1.0D0-aj+0.33D0*aj*aj)
  a(10) =   -0.26D0*(1.0D0-0.87D0/saj-0.16D0*aj)
  a(11) =   -0.14D0*(1.0D0-1.14D0/saj-0.45D0*saj)
  a(12) = -0.0069D0

  seps1 = sqrt(eps1)

  b(1)  = 1.0D0
  b(2)  = alfpnw
  b(3)  = alftnw
  b(4)  = alfpnw*alftnw
  b(5)  = seps1
  b(6)  = alfpnw*seps1
  b(7)  = alftnw*seps1
  b(8)  = alfpnw*alftnw*seps1
  b(9)  = eps1
  b(10) = alfpnw*eps1
  b(11) = alftnw*eps1
  b(12) = alfpnw*alftnw*eps1

  sss = 0.0D0
  do i = 1,12
     sss = sss + a(i)*b(i)
  end do

  !  Empirical bootstrap current fraction

  culbst = seps1 * betpth * sss

  !  Diamagnetic contribution to the bootstrap fraction
  !  at tight aspect ratio.
  !  Tim Hender fit

  if (itart == 1) then
     culbst = culbst + beta/2.8D0
  end if

end function culbst

