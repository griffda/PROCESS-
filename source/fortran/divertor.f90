! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_module

  !! Module containing divertor routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines relevant for calculating the
  !! divertor parameters for a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  private
  public :: divcall

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divcall(outfile,iprint)

    !! Routine to call the divertor model
    !! author: J Galambos, ORNL
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine calls the divertor routine. This routine scales
    !! dimensions, powers and field levels which are used as input to
    !! the Harrison divertor model.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: plsepo, vgap, rspo, scrapli
    use constants, only: pi 
    use divertor_variables, only: prn1, rlclolcn, c6div, c5div, ksic, omegan, &
      rconl, minstang, hldiv, rsrd, xparain, divdum, tsep, &
      densin, zeffdiv, xpertin, fgamp, adas, tconl, c4div, dendiv, tdiv, frrp, &
      c3div, fififi, ptpdiv , ppdivr, c2div, fdfs , delld, omlarg, c1div, &
      anginc, lamp 
    use physics_variables, only: dene, itart, triang, bp, zeff, q, sarea, &
      sareao, bt, afuel, rminor, aspect, rmajor, pdivt
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(8) :: aionso,bpstk,btstk,dconl,delne, &
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

    !! Harrison-Kukushkin analytic ITER divertor model
    !! author: J Galambos, ORNL
    !! author: P J Knight, CCFE, Culham Science Centre
    !! adas     : input real : divertor flux area / main plasma area
    !! (long separatrix)
    !! aion     : input real : ion mass (assumes fuel only) (AMU)
    !! anginc   : input real : pol. angle of incidence of field line on plate (rad)
    !! c1div    : input real : fitting coefficient for plate temperature
    !! c2div    : input real : fitting coefficient for plate temperature
    !! c3div    : input real : fitting coefficient for heat load
    !! c4div    : input real : fitting coefficient for heat load
    !! c5div    : input real : fitting coefficient for 'omlarg'
    !! delld    : input real : coeff. for power distribution flow into scrapeoff
    !! delne    : input real : scrapeoff density by main plasma (10**20 m-3)
    !! fdfs     : input real : gradient ratio (private flux side/other side)
    !! in 'omlarg'
    !! fififi   : input real : coeff. used in sheath energy transfer factor calc.
    !! frgd     : input real : separatrix area to divertor / total separatrix area
    !! frrp     : input real : fraction of radiated power to plate
    !! minstang : input real : minimum strike angle (total) for heat flux calc.
    !! omegan   : input real : pressure ratio of (plate / main plasma)
    !! qdiv     : input real : heat flux across separatrix to divertor (MW/m2)
    !! pdiv     : input real : power flow to plate (MW)
    !! rbpbt    : input real : ratio of toroidal / poloidal field at strike point
    !! rconl    : input real : connection length ratio
    !! (divertor region/main plasma region)
    !! rmaj     : input real : major radius (m)
    !! rsrd     : input real : ratio of separatrix radius / divertor radius
    !! tconl    : input real : connection length along field line by main plasma (m)
    !! xpara    : input real : parallel diffusivity in the plasma scrapeoff (m2/s)
    !! xperp    : input real : perpend. diffusivity in the plasma scrapeoff (m2/s)
    !! delta    : output real : iteration relative error
    !! delw     : output real : energy flow thickness in scrape-off (m)
    !! dendiv   : output real : plasma density at divertor (10**20 m-3)
    !! densin   : output real : peak plasma density at divertor
    !! (on separatrix) (10**20 m-3)
    !! gamdt    : output real : plasma flow to plate (10**20/s)
    !! lamp     : output real : power flow width (m)
    !! omlarg   : output real : factor accounting for power flow
    !! to private flux region
    !! ppdiv    : output real : divertor heat load without radiation (MW/m2)
    !! ppdivr   : output real : divertor heat load with radiation (MW/m2)
    !! ptpdiv   : output real : peak plasma temperature at the divertor plate (eV)
    !! tdiv     : output real : temperature at the plate (eV)
    !! tsep     : output real : temperature at the separatrix (eV)
    !! This subroutine performs the iteration described in M. Harrison's
    !! and Kukushkin's analytic ITER divertor model.
    !! Report ITER-IL-PH-13-9-e12
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi 
    implicit none

    !  Arguments

    real(8), intent(in) :: adas,aion,anginc,c1div,c2div,c3div, &
         c4div,c5div,delld,delne,fdfs,fififi,frgd,frrp,minstang,omegan, &
         qdiv,pdiv,rbpbt,rconl,rmaj,rsrd,tconl,xpara,xperp

    real(8), intent(out) :: delta,delw,dendiv,densin,gamdt, &
         lamp,omlarg,ppdiv,ppdivr,ptpdiv,tdiv,tsep

    !  Local variables

    real(8), parameter :: c27 = 0.2857143D0
    real(8), parameter :: ei = 13.6D0
    real(8), parameter :: epsilon = 0.001D0
    real(8), parameter :: relerr = 1.0D-9

    integer :: i
    real(8) :: angle,coefl,cp,ct,deltx,delty,deltdiv, &
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

      !! Function providing the (energy radiated + ionized) per neutral
      !! recycle event from the Harrison / Kukushkin ITER model
      !! author: J Galambos, ORNL
      !! author: P J Knight, CCFE, Culham Science Centre
      !! tdiv     : input real : electron temperature at the plate (eV)
      !! ndiv     : input real : electron density at the plate (10**20 m-3)
      !! This function calculates the total energy (in eV) radiated and
      !! ionized, per neutral recycle event, from the Harrison / Kukushkin
      !! ITER model.
      !! Report ITER-IL-PH-13-9-e12
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(8) :: erprcy

      !  Arguments

      real(8), intent(in) :: ndiv,tdiv

      !  Local variables

      real(8) :: ans

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ans = 17.5D0 + (5.0D0 + 37.5D0/tdiv) * log10(10.0D0/ndiv)
      erprcy = max(ans, 0.001D0)

    end function erprcy

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv,tconl,xpara, &
         xperp,xx,yy)

      !! Function for divertor temperature solution
      !! author: J Galambos, ORNL
      !! author: P J Knight, CCFE, Culham Science Centre
      !! aion     : input real : ion mass (assumes fuel only) (AMU)
      !! coefl    : input real : little 'l' in Harrison model
      !! delne    : input real : scrapeoff density by main plasma (10**20 m-3)
      !! fififi   : input real : coeff. used in sheath energy transfer factor calc.
      !! omegan   : input real : pressure ratio of (plate / main plasma)
      !! omlarg   : input real : factor accounting for power flow
      !! qdiv     : input real : heat flux across separatrix to divertor (MW/m2)
      !! tconl    : input real : connection length along field line by main
      !! plasma (m)
      !! xpara    : input real : parallel diffusivity in the plasma scrapeoff (m2/s)
      !! xperp    : input real : perpend. diffusivity in the plasma scrapeoff (m2/s)
      !! xx       : input real : T_plate / T_separatrix guess
      !! yy       : input real : T_plate guess (eV)
      !! This function calculates an estimate for the divertor temperature (eV).
      !! Report ITER-IL-PH-13-9-e12
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(8) :: ftdiv

      !  Arguments

      real(8), intent(in) :: aion,coefl,delne,fififi,omegan,omlarg,qdiv, &
           tconl,xpara,xperp,xx,yy

      !  Local variables

      real(8) :: c27,dendiv,eier,ff,gamdiv,xxs,yys

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

      !! Function for divertor model temperature ratio solution
      !! author: J Galambos, ORNL
      !! author: P J Knight, CCFE, Culham Science Centre
      !! aion     : input real : ion mass (assumes fuel only) (AMU)
      !! coefl    : input real : little 'l' in Harrison model
      !! delne    : input real : scrapeoff density by main plasma (10**20 m-3)
      !! fififi   : input real : coeff. used in sheath energy transfer factor calc.
      !! omegan   : input real : pressure ratio of (plate / main plasma)
      !! omlarg   : input real : factor accounting for power flow
      !! qdiv     : input real : heat flux across separatrix to divertor (MW/m2)
      !! tconl    : input real : connection length along field line by main
      !! plasma (m)
      !! xpara    : input real : parallel diffusivity in the plasma scrapeoff (m2/s)
      !! xperp    : input real : perpend. diffusivity in the plasma scrapeoff (m2/s)
      !! xx       : input real : T_plate / T_separatrix guess
      !! yy       : input real : T_plate guess (eV)
      !! This function updates the divertor model temperature ratio solution.
      !! Report ITER-IL-PH-13-9-e12
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(8) :: ftpts

      !  Arguments

      real(8), intent(in) :: aion,coefl,delne,fififi,omegan, &
           omlarg,qdiv,tconl,xpara,xperp,xx,yy

      !  Local variables

      real(8) :: dendiv,eier,ff,gamdiv,xxs,yys

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

      !! Function to provide the plasma sheath energy transfer coefficient
      !! author: J Galambos, ORNL
      !! author: P J Knight, CCFE, Culham Science Centre
      !! gcoef    : input real : coefficient
      !! tdiv     : input real : electron temperature at the plate (eV)
      !! This function provides the plasma sheath energy transfer coefficient
      !! from the Harrison / Kukushkin ITER model.
      !! Report ITER-IL-PH-13-9-e12
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(8) :: gammash

      !  Arguments

      real(8), intent(in) :: gcoef,tdiv

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      gammash = ( 8.3D0 - 6.0D0*(0.07D0 - 0.18D0 * log10(3.0D0*tdiv*gcoef) ) )

    end function gammash

  end subroutine divert

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divtart(rmajor,rminor,triang,scrapli,vgap,pdivt,hldiv, &
       iprint,outfile)

    !! Tight aspect ratio tokamak divertor model
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rmajor : input real : plasma major radius (m)
    !! rminor : input real : plasma minor radius (m)
    !! triang : input real : plasma triangularity
    !! scrapli : input real : inboard scrape-off width (m)
    !! vgap : input real : top scrape-off width (m)
    !! pdivt : input real : power to the divertor (MW)
    !! hldiv : output real : heat load on the divertor (MW/m2)
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! outfile : input integer : output file unit
    !! This subroutine calculates the divertor heat load for a tight aspect
    !! ratio machine, by assuming that the power is evenly spread around the
    !! divertor chamber by the action of a gaseous target. Each divertor is
    !! assumed to be approximately triangular in the R,Z plane.
    !! AEA FUS 64: Figure 2
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi 
    use error_handling, only: fdiags, report_error
    use tfcoil_variables, only: drtop
    use process_output, only: ocmmnt, osubhd, ovarre, oblnkl
    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile
    real(8), intent(in) :: rmajor,rminor,triang,scrapli,vgap,pdivt
    real(8), intent(out) :: hldiv

    !  Local variables

    real(8) :: r1,r2,a1,a2,a3,theta,areadv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Thickness of centrepost + first wall at divertor height

    r1 = rmajor - rminor*triang - 3.0D0*scrapli + drtop

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

    a3 = a2 / (cos(theta)*cos(theta))

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
