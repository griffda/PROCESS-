! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_module

  !+ad_name  divertor_module
  !+ad_summ  Module containing divertor routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  divcall
  !+ad_cont  divert
  !+ad_cont  divtart
  !+ad_args  N/A
  !+ad_desc  This module contains routines relevant for calculating the
  !+ad_desc  divertor parameters for a fusion power plant.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_hist  17/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use divertor_variables
  use error_handling
  use physics_variables
  use process_output

  implicit none

  private
  public :: divcall

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divcall(outfile,iprint)

    !+ad_name  divcall
    !+ad_summ  Routine to call the divertor model
    !+ad_type  Subroutine
    !+ad_auth  J Galambos, ORNL
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calls the divertor routine. This routine scales
    !+ad_desc  dimensions, powers and field levels which are used as input to
    !+ad_desc  the Harrison divertor model.
    !+ad_prob  Many of the parameters are scaled from the ~1990 ITER point
    !+ad_prob  (R=6.00, Bt = 4.85 T, Bp = 1.07 T, l_null-strike = 1.50 m).
    !+ad_prob  Variation far from these parameters is uncertain.
    !+ad_call  divert
    !+ad_call  divtart
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  27/06/89 JG  Put in ITER divertor model
    !+ad_hist  27/11/90 JG  Modified to have a ST expanded divertor option
    !+ad_hist  13/03/91 JG  Updated ITER model
    !+ad_hist  18/03/91 JG  New scalings for connection lengths
    !+ad_hist  29/01/96 PJK Added TART gaseous divertor model
    !+ad_hist  14/05/96 PJK Improved calculation of TART divertor area
    !+ad_hist  25/04/02 PJK Added ZEFFDIV; Changed DIVDUM to integer
    !+ad_hist  17/11/11 PJK Initial F90 version;
    !+ad_hisc               Moved TART model into new routine
    !+ad_hist  24/09/12 PJK Swapped argument order
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  14/11/13 PJK Removed upper limit on plsep
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  02/02/17 JM  Replaced rstrko with rspo
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: aionso,bpstk,btstk,dconl,delne, &
         delta,delw,diva,dtheta,frgd,gamdt,pdiv,plsep, &
         ppdiv,pwr,qdiv,rbpbt,rnull,xpara,xperp,zeffso

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (itart == 1) then
       call divtart(rmajor,rminor,triang,scrapli,vgap,pdivt,hldiv, &
            iprint,outfile)
       return
    end if

    !  Scale geometric quantities

    !  Perpendicular diffusivity in the plasma scrapeoff (m2/s)
    !  Assume ions transport 33% of electron power

    xperp = xpertin * 1.33D0

    !  Reference null to strike distances
    !  Only set up for outer divertor for double-null

    !plsep = min(plsepo,pi*rminor)  !  Obscure reason to set a limit...
    plsep = plsepo

    !  Scale plasma quantities

    delne = prn1 * dene * 1.0D-20  !  scrapeoff density by main plasma
    pwr = pdivt                    !  power flow to divertor (MW)
    aionso = afuel                 !  scrape-off layer ion mass

    if (divdum == 0) then  !  Divertor Zeff: scaled
       zeffso = 1.0D0 + 0.8D0*(zeff-1.0D0)
    else  !  use input value
       zeffso = zeffdiv
    end if

    !  Strike point field values

    bpstk = bp * 0.45D0
    btstk = bt * rmajor/rspo
    rbpbt = bpstk / btstk

    !  Parallel diffusivity in the plasma scrapeoff (m2/s)

    xpara = xparain/zeffso

    !  Null radius

    rnull = rmajor - rminor*triang

    !  Divertor area and radius ratio

    rsrd = (rnull + rmajor + rminor) / (rnull + rspo)
    diva = pi* (rnull + rspo) * plsep
    adas = diva/sarea

    !  Main plasma separatrix area to divertor (and power fraction)
!+PJK Is the 2 related to 2 divertors (i.e. double-null assumed)?
    frgd = (sareao)/(2.0D0*sarea)
!-PJK
    !  Power flow to divertor

    pdiv = pwr * ksic/2.0D0
    qdiv = pdiv/(sarea*frgd)

    !  Connection length scalings
    !  (2.5 factor comes from normalization to ITER 1990)

    tconl = 2.5D0 * rmajor * q * sqrt(1.0D0 + 1.0D0/(q*aspect)**2)
    dtheta = plsep/rminor
    dconl = 2.5D0 * rspo * q * dtheta * &
         sqrt(1.0D0 + 1.0D0/(q*aspect)**2)
    rconl = dconl / tconl

    !  Minimum strike angle

    minstang = 0.5D0

    !  Call divertor routine

    call divert(adas,aionso,anginc,delne,c1div,c2div,c3div,c4div,c5div, &
         delld,fdfs,fififi,frgd,frrp,minstang,omegan,qdiv,pdiv,rbpbt, &
         rconl,rmajor,rsrd,tconl,xpara,xperp,delta,delw,dendiv,densin, &
         gamdt,lamp,omlarg,ppdiv,ppdivr,ptpdiv,tdiv,tsep)

    !  Heat load

    hldiv = ppdivr

    !  Ratio of collision length to connection length

    rlclolcn = 1.44D-3 * tsep**2 / (delne*15.0D0*tconl)

    if (iprint == 0) return
    ! MDK Issue #242 Switch off divertor output
    return

    call oheadr(outfile,'Divertor')
    call ocmmnt(outfile,'Harrison (ITER) Model')
    call oblnkl(outfile)

    !  Fixed quantities to divertor model

    call ovarre(outfile,'Ion mass (amu)','(aionso)',aionso)
    call ovarre(outfile,'Fitting coefficient','(c1div)',c1div)
    call ovarre(outfile,'Fitting coefficient','(c2div)',c2div)
    call ovarre(outfile,'Fitting coefficient','(c3div)',c3div)
    call ovarre(outfile,'Fitting coefficient','(c4div)',c4div)
    call ovarre(outfile,'Fitting coefficient','(c5div)',c5div)
    call ovarre(outfile,'Fitting coefficient','(c6div)',c6div)
    call ovarin(outfile,'Divertor Zeff model','(divdum)',divdum)
    call ovarre(outfile,'Zeff in scrape-off region','(zeffso)',zeffso)
    call ovarre(outfile,'Coeff of energy distrib. along conn length', &
         '(delld)',delld)
    call ovarre(outfile,'Separatrix plasma density (10**20 m-3)', &
         '(delne)',delne)
    call ovarre(outfile,'Radial gradient ratio','(fdfs)',fdfs)
    call ovarre(outfile,'Sheath potential factor','(fgamp)',fgamp)
    call ovarre(outfile,'Parameter for sheath coefficient','(fififi)', &
         fififi)
    call ovarre(outfile,'Fraction of radiated power to plate','(frrp)', &
         frrp)
    call ovarre(outfile,'Pressure ratio - (nT)_p/(nT)_s','(omegan)', &
         omegan)
    call ovarre(outfile,'ne-edge / ne-average','(prn1)',prn1)
    call ovarre(outfile,'Parallel heat transport coefficient','(xpara)', &
         xpara)
    call ovarre(outfile,'Radial transport coefficient','(xperp)',xperp)

    !  Input quantities scaled in divertor caller (dependent on geometry,
    !  plasma parameters) - can be different for inner and outer plates

    call osubhd(outfile,'Scaled Input Quantities :')

    call ovarre(outfile,'Fraction of areas','(adas)',adas)
    call ovarre(outfile,'Angle of incidence (rad)','(anginc)',anginc)
    call ovarre(outfile,'Area of divertor / area of separatrix','(frgd)' &
         ,frgd)
    call ovarre(outfile,'Power fraction to outer divertor','(ksic)',ksic)
    call ovarre(outfile,'Power to divertor (MW)','(pdiv)',pdiv)
    call ovarre(outfile,'Null to strike length (m)','(plsep)',plsep)
    call ovarre(outfile,'B_p / B_t strike point','(rbpbtc)',rbpbt)
    call ovarre(outfile,'Connection length ratio','(rconl)',rconl)
    call ovarre(outfile,'Radius ratio R_s/R_d','(rsrd)',rsrd)
    call ovarre(outfile,'Strike radius (m)','(rspo)',rspo)
    call ovarre(outfile,'Connection length (m)','(tconl)',tconl)

    !  Quantities calculated by the Harrison model

    call osubhd(outfile,'Divertor Model Output :')
    call ovarre(outfile,'Iteration relative error','(delta)',delta)
    call ovarre(outfile,'Private flux power factor','(omlarg)',omlarg)
    call ovarre(outfile,'Separatrix temperature (eV)','(tsep)',tsep)
    call ovarre(outfile,'Divertor temperature (eV)','(tdiv)',tdiv)
    call ovarre(outfile,'Divertor plasma density (10**20 m-3)', &
         '(dendiv)',dendiv)
    call ovarre(outfile,'Peak heat load (MW/m2)','(hldiv)',hldiv)
    call ovarre(outfile,'Divertor peak temperature (eV)','(ptpdiv)', &
         ptpdiv)
    call ovarre(outfile,'D/T plate flux (10**20 m-3)','(gamdt)',gamdt)
    call ovarre(outfile,'Scrape-off thickness (m)','(delw)',delw)
    call ovarre(outfile,'Collision length / connection length', &
         '(rlclolcn)',rlclolcn)

  end subroutine divcall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divert(adas,aion,anginc,delne,c1div,c2div,c3div,c4div, &
       c5div,delld,fdfs,fififi,frgd,frrp,minstang,omegan,qdiv,pdiv, &
       rbpbt,rconl,rmaj,rsrd,tconl,xpara,xperp,delta,delw,dendiv, &
       densin,gamdt,lamp,omlarg,ppdiv,ppdivr,ptpdiv,tdiv,tsep)

    !+ad_name  divert
    !+ad_summ  Harrison-Kukushkin analytic ITER divertor model
    !+ad_type  Subroutine
    !+ad_auth  J Galambos, ORNL
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  erprcy
    !+ad_cont  ftdiv
    !+ad_cont  ftpts
    !+ad_cont  gammash
    !+ad_args  adas     : input real : divertor flux area / main plasma area
    !+ad_argc                          (long separatrix)
    !+ad_args  aion     : input real : ion mass (assumes fuel only) (AMU)
    !+ad_args  anginc   : input real : pol. angle of incidence of field line on plate (rad)
    !+ad_args  c1div    : input real : fitting coefficient for plate temperature
    !+ad_args  c2div    : input real : fitting coefficient for plate temperature
    !+ad_args  c3div    : input real : fitting coefficient for heat load
    !+ad_args  c4div    : input real : fitting coefficient for heat load
    !+ad_args  c5div    : input real : fitting coefficient for 'omlarg'
    !+ad_args  delld    : input real : coeff. for power distribution flow into scrapeoff
    !+ad_args  delne    : input real : scrapeoff density by main plasma (10**20 m-3)
    !+ad_args  fdfs     : input real : gradient ratio (private flux side/other side)
    !+ad_argc                          in 'omlarg'
    !+ad_args  fififi   : input real : coeff. used in sheath energy transfer factor calc.
    !+ad_args  frgd     : input real : separatrix area to divertor / total separatrix area
    !+ad_args  frrp     : input real : fraction of radiated power to plate
    !+ad_args  minstang : input real : minimum strike angle (total) for heat flux calc.
    !+ad_args  omegan   : input real : pressure ratio of (plate / main plasma)
    !+ad_args  qdiv     : input real : heat flux across separatrix to divertor (MW/m2)
    !+ad_args  pdiv     : input real : power flow to plate (MW)
    !+ad_args  rbpbt    : input real : ratio of toroidal / poloidal field at strike point
    !+ad_args  rconl    : input real : connection length ratio
    !+ad_argc                          (divertor region/main plasma region)
    !+ad_args  rmaj     : input real : major radius (m)
    !+ad_args  rsrd     : input real : ratio of separatrix radius / divertor radius
    !+ad_args  tconl    : input real : connection length along field line by main plasma (m)
    !+ad_args  xpara    : input real : parallel diffusivity in the plasma scrapeoff (m2/s)
    !+ad_args  xperp    : input real : perpend. diffusivity in the plasma scrapeoff (m2/s)
    !+ad_args  delta    : output real : iteration relative error
    !+ad_args  delw     : output real : energy flow thickness in scrape-off (m)
    !+ad_args  dendiv   : output real : plasma density at divertor (10**20 m-3)
    !+ad_args  densin   : output real : peak plasma density at divertor
    !+ad_argc                           (on separatrix) (10**20 m-3)
    !+ad_args  gamdt    : output real : plasma flow to plate (10**20/s)
    !+ad_args  lamp     : output real : power flow width (m)
    !+ad_args  omlarg   : output real : factor accounting for power flow
    !+ad_argc                           to private flux region
    !+ad_args  ppdiv    : output real : divertor heat load without radiation (MW/m2)
    !+ad_args  ppdivr   : output real : divertor heat load with radiation (MW/m2)
    !+ad_args  ptpdiv   : output real : peak plasma temperature at the divertor plate (eV)
    !+ad_args  tdiv     : output real : temperature at the plate (eV)
    !+ad_args  tsep     : output real : temperature at the separatrix (eV)
    !+ad_desc  This subroutine performs the iteration described in M. Harrison's
    !+ad_desc  and Kukushkin's analytic ITER divertor model.
    !+ad_prob  None
    !+ad_call  erprcy
    !+ad_call  ftdiv
    !+ad_call  ftpts
    !+ad_call  gammash
    !+ad_hist  17/11/11 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_stat  Okay
    !+ad_docs  Report ITER-IL-PH-13-9-e12
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: adas,aion,anginc,c1div,c2div,c3div, &
         c4div,c5div,delld,delne,fdfs,fififi,frgd,frrp,minstang,omegan, &
         qdiv,pdiv,rbpbt,rconl,rmaj,rsrd,tconl,xpara,xperp

    real(kind(1.0D0)), intent(out) :: delta,delw,dendiv,densin,gamdt, &
         lamp,omlarg,ppdiv,ppdivr,ptpdiv,tdiv,tsep

    !  Local variables

    real(kind(1.0D0)), parameter :: c27 = 0.2857143D0
    real(kind(1.0D0)), parameter :: ei = 13.6D0
    real(kind(1.0D0)), parameter :: epsilon = 0.001D0
    real(kind(1.0D0)), parameter :: relerr = 1.0D-9

    integer :: i
    real(kind(1.0D0)) :: angle,coefl,cp,ct,deltx,delty,deltdiv, &
         deltpts,denom,eier,facdenom,fprime,f1,f1dx,f1dy,f2,f2dx,f2dy, &
         gamdiv,tdivges,tdivp,tpts,tptsges,tptsp

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    fprime = c5div * fdfs
    facdenom = fprime * rsrd*(adas/frgd)**2 /rconl
    facdenom = max(facdenom, 0.04D0)
    omlarg = 1.0D0/ ( rsrd * exp(-facdenom) )
    omlarg = min(omlarg, 2.0D0)
    coefl = 1.0D0/delld + rconl/omlarg  !  little 'l' in Harrison model

    !  Start iteration on 2 simultaneous equations (Newton's method)

    tdivges = 150.0D0
    tptsges = 0.9D0
    tdiv = tdivges
    tpts = tptsges

    do i = 1,15

       !  Find derivatives for Newton's method

       tptsp = tpts * (1.0D0 + epsilon)
       deltx = tpts * epsilon
       tdivp = tdiv * (1.0D0 + epsilon)
       delty = tdiv * epsilon

       f1 = ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
            tconl,xpara,xperp,tpts,tdiv)
       f2 = ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
            tconl,xpara,xperp,tpts,tdiv)

       f1dx = ( ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
            tconl,xpara,xperp,tptsp,tdiv) - f1 ) / deltx
       f1dy = ( ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
            tconl,xpara,xperp,tpts,tdivp) - f1 ) / delty
       f2dx = ( ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
            tconl,xpara,xperp,tptsp,tdiv) - f2 ) / deltx
       f2dy = ( ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
            tconl,xpara,xperp,tpts,tdivp) - f2 ) / delty

       denom = f1dx*f2dy - f1dy*f2dx
       if (denom == 0.0D0) denom = 1.0D-10
       deltpts = (-f2dy*f1 + f1dy*f2) / denom
       deltdiv = ( f2dx*f1 - f1dx*f2) / denom

       !  New guess

       tdiv = tdiv + deltdiv
       tpts = tpts + deltpts
       delta = abs(deltdiv/tdiv + deltpts/tpts)

       !  Satisfied yet?

       if (delta < relerr) exit

    end do

    tdiv = max(tdiv, 0.1000D0)
    tpts = max(tpts, 0.0010D0)
    tpts = min(tpts, 0.9999D0)

    !  Some other quantities

    ct = max( 0.1D0, (c1div + c2div/(tdiv)) )
    ptpdiv = tdiv * ct
    gamdiv = gammash(fififi,tdiv)  !  sheath coefficient
    dendiv = delne / (omegan*tpts)
    eier = erprcy(tdiv,dendiv)  !  ionization + radiation energy / recycle event

    tsep = 251.0D0 * ( (qdiv*tconl)**2 /(c27*xpara* &
         (1.0D0 - tpts**3.5D0) ) * coefl/(xperp*delne))**0.2222222D0

    cp = max(0.1D0, (c3div + c4div/(tdiv)) )
    angle = sin(anginc) * rbpbt
    if (minstang /= 0.0D0) angle = max(angle, (minstang/57.3D0))

    ppdiv = 2.48D2 * (qdiv)**1.55556D0 / (xperp*delne)**0.777778D0 &
         *(c27*xpara)**0.2222222D0 * tconl**0.555556D0 * &
         ( (1.0D0-tpts**3.5D0)/coefl)**0.222222D0 / omlarg * &
         (1.0D0 + ei/(gamdiv*tdiv))/(1.0D0+eier/(gamdiv*tdiv)) * &
         angle * cp
    ppdivr = ppdiv * (1.0D0 + frrp * (eier-ei) / (gamdiv*tdiv) )
    gamdt = 6.25D4 * ppdiv / (gamdiv*ptpdiv)
    densin = omegan * tsep * delne/ptpdiv
    delw = 4.01D-3 * (delne*xperp)**0.7777778D0 * tconl**0.4444444D0 &
         * coefl**0.2222222D0 / ( (qdiv)**0.55555556D0 * &
         (c27 * xpara * (1.0D0 - tpts**3.5D0) )**0.22222D0 )
    lamp = pdiv*rsrd / (2.0D0 * pi * rmaj * ppdiv)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function erprcy(tdiv,ndiv)

      !+ad_name  erprcy
      !+ad_summ  Function providing the (energy radiated + ionized) per neutral
      !+ad_summ  recycle event from the Harrison / Kukushkin ITER model
      !+ad_type  Function returning real
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  tdiv     : input real : electron temperature at the plate (eV)
      !+ad_args  ndiv     : input real : electron density at the plate (10**20 m-3)
      !+ad_desc  This function calculates the total energy (in eV) radiated and
      !+ad_desc  ionized, per neutral recycle event, from the Harrison / Kukushkin
      !+ad_desc  ITER model.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  08/05/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Report ITER-IL-PH-13-9-e12
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: erprcy

      !  Arguments

      real(kind(1.0D0)), intent(in) :: ndiv,tdiv

      !  Local variables

      real(kind(1.0D0)) :: ans

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ans = 17.5D0 + (5.0D0 + 37.5D0/tdiv) * log10(10.0D0/ndiv)
      erprcy = max(ans, 0.001D0)

    end function erprcy

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv,tconl,xpara, &
         xperp,xx,yy)

      !+ad_name  ftdiv
      !+ad_summ  Function for divertor temperature solution
      !+ad_type  Function returning real
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  aion     : input real : ion mass (assumes fuel only) (AMU)
      !+ad_args  coefl    : input real : little 'l' in Harrison model
      !+ad_args  delne    : input real : scrapeoff density by main plasma (10**20 m-3)
      !+ad_args  fififi   : input real : coeff. used in sheath energy transfer factor calc.
      !+ad_args  omegan   : input real : pressure ratio of (plate / main plasma)
      !+ad_args  omlarg   : input real : factor accounting for power flow
      !+ad_args  qdiv     : input real : heat flux across separatrix to divertor (MW/m2)
      !+ad_args  tconl    : input real : connection length along field line by main
      !+ad_argc                          plasma (m)
      !+ad_args  xpara    : input real : parallel diffusivity in the plasma scrapeoff (m2/s)
      !+ad_args  xperp    : input real : perpend. diffusivity in the plasma scrapeoff (m2/s)
      !+ad_args  xx       : input real : T_plate / T_separatrix guess
      !+ad_args  yy       : input real : T_plate guess (eV)
      !+ad_desc  This function calculates an estimate for the divertor temperature (eV).
      !+ad_prob  None
      !+ad_call  erprcy
      !+ad_call  gammash
      !+ad_hist  08/05/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Report ITER-IL-PH-13-9-e12
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: ftdiv

      !  Arguments

      real(kind(1.0D0)), intent(in) :: aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
           tconl,xpara,xperp,xx,yy

      !  Local variables

      real(kind(1.0D0)) :: c27,dendiv,eier,ff,gamdiv,xxs,yys

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      c27 = 0.28571428D0

      xxs = max(xx, 0.001D0)
      xxs = min(xxs, 0.99999D0)
      yys = max(yy, 0.1D0)

      dendiv = delne * omegan/xxs
      gamdiv = gammash(fififi,yys)
      eier = erprcy(yys,dendiv)
      ff = 20.16D0 * aion * ((qdiv)**10 * (c27*xpara)**4 / &
           (xperp**5 * delne**14)*tconl*(1.0D0 - xxs**3.5D0)**4 &
           /coefl**4)**0.22222D0/(omegan*gamdiv*omlarg* &
           (1.0D0+eier/(gamdiv*yys)))**2

      ftdiv = yys - ff

    end function ftdiv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv,tconl, &
         xpara,xperp,xx,yy)

      !+ad_name  ftpts
      !+ad_summ  Function for divertor model temperature ratio solution
      !+ad_type  Function returning real
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  aion     : input real : ion mass (assumes fuel only) (AMU)
      !+ad_args  coefl    : input real : little 'l' in Harrison model
      !+ad_args  delne    : input real : scrapeoff density by main plasma (10**20 m-3)
      !+ad_args  fififi   : input real : coeff. used in sheath energy transfer factor calc.
      !+ad_args  omegan   : input real : pressure ratio of (plate / main plasma)
      !+ad_args  omlarg   : input real : factor accounting for power flow
      !+ad_args  qdiv     : input real : heat flux across separatrix to divertor (MW/m2)
      !+ad_args  tconl    : input real : connection length along field line by main
      !+ad_argc                          plasma (m)
      !+ad_args  xpara    : input real : parallel diffusivity in the plasma scrapeoff (m2/s)
      !+ad_args  xperp    : input real : perpend. diffusivity in the plasma scrapeoff (m2/s)
      !+ad_args  xx       : input real : T_plate / T_separatrix guess
      !+ad_args  yy       : input real : T_plate guess (eV)
      !+ad_desc  This function updates the divertor model temperature ratio solution.
      !+ad_prob  None
      !+ad_call  erprcy
      !+ad_call  gammash
      !+ad_hist  17/11/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Report ITER-IL-PH-13-9-e12
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: ftpts

      !  Arguments

      real(kind(1.0D0)), intent(in) :: aion,coefl,delne,fififi,omegan, &
           omlarg,qdiv,tconl,xpara,xperp,xx,yy

      !  Local variables

      real(kind(1.0D0)) :: dendiv,eier,ff,gamdiv,xxs,yys

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      xxs = max(xx, 0.001D0)
      xxs = min(xxs, 0.99999D0)
      yys = max(yy, 0.1D0)

      dendiv = delne * omegan/xxs
      gamdiv = gammash(fififi,yys)
      eier = erprcy(yys,dendiv)

      ff = xxs**3.5D0 + 9.66D0 * (xxs/aion)**0.9D0 * &
           (xperp/(qdiv)**2)**0.8D0 * coefl/(2.0D0 * xpara/7.0D0)* &
           tconl**0.2D0 * ( omlarg*omegan*gamdiv* &
           (1.0D0+eier/(gamdiv*yys)))**1.8D0 * delne**2.6D0

      ftpts = 1.0D0 - ff

    end function ftpts

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function gammash(gcoef,tdiv)

      !+ad_name  gammash
      !+ad_summ  Function to provide the plasma sheath energy transfer coefficient
      !+ad_type  Function returning real
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  gcoef    : input real : coefficient
      !+ad_args  tdiv     : input real : electron temperature at the plate (eV)
      !+ad_desc  This function provides the plasma sheath energy transfer coefficient
      !+ad_desc  from the Harrison / Kukushkin ITER model.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  08/05/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Report ITER-IL-PH-13-9-e12
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: gammash

      !  Arguments

      real(kind(1.0D0)), intent(in) :: gcoef,tdiv

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      gammash = ( 8.3D0 - 6.0D0*(0.07D0 - 0.18D0 * log10(3.0D0*tdiv*gcoef) ) )

    end function gammash

  end subroutine divert

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divtart(rmajor,rminor,triang,scrapli,vgap,pdivt,hldiv, &
       iprint,outfile)

    !+ad_name  divtart
    !+ad_summ  Tight aspect ratio tokamak divertor model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real : plasma major radius (m)
    !+ad_args  rminor : input real : plasma minor radius (m)
    !+ad_args  triang : input real : plasma triangularity
    !+ad_args  scrapli : input real : inboard scrape-off width (m)
    !+ad_args  vgap : input real : top scrape-off width (m)
    !+ad_args  pdivt : input real : power to the divertor (MW)
    !+ad_args  hldiv : output real : heat load on the divertor (MW/m2)
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This subroutine calculates the divertor heat load for a tight aspect
    !+ad_desc  ratio machine, by assuming that the power is evenly spread around the
    !+ad_desc  divertor chamber by the action of a gaseous target. Each divertor is
    !+ad_desc  assumed to be approximately triangular in the R,Z plane.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_call  report_error
    !+ad_hist  29/01/96 PJK Added TART gaseous divertor model
    !+ad_hist  14/05/96 PJK Improved calculation of TART divertor area
    !+ad_hist  08/05/12 PJK Initial F90 version; Moved TART model into new routine
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  16/10/12 PJK Added constants; removed argument pi
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 64: Figure 2
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile
    real(kind(1.0D0)), intent(in) :: rmajor,rminor,triang,scrapli,vgap,pdivt
    real(kind(1.0D0)), intent(out) :: hldiv

    !  Local variables

    real(kind(1.0D0)) :: r1,r2,a1,a2,a3,theta,areadv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Thickness of centrepost + first wall at divertor height

    r1 = rmajor - rminor*triang - 3.0D0*scrapli

    !  Outer radius of divertor region

    r2 = rmajor + rminor

    !  Angle of diagonal divertor plate from horizontal

    if (vgap <= 0.0D0) then
       fdiags(1) = vgap ; call report_error(22)
    end if

    theta = atan(vgap/(r2-r1))

    !  Vertical plate area

    a1 = 2.0D0 * pi * r1 * vgap

    !  Horizontal plate area

    a2 = pi * (r2*r2 - r1*r1)

    !  Diagonal plate area

    a3 = a2 / cos(theta)

    !  Total divertor area (N.B. there are two of them)

    areadv = 2.0D0 * (a1+a2+a3)

    hldiv = pdivt/areadv

    if (iprint == 0) return

    call osubhd(outfile,'Divertor Heat Load')
    call ocmmnt(outfile,'Assume an expanded divertor with a gaseous target')
    call oblnkl(outfile)
    call ovarre(outfile,'Power to the divertor (MW)','(pdivt.)',pdivt)
    call ovarre(outfile,'Divertor surface area (m2)','(areadv)',areadv)
    call ovarre(outfile,'Divertor heat load (MW/m2)','(hldiv)',hldiv)

  end subroutine divtart

end module divertor_module
