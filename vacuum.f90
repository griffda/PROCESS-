!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module vacuum_module

  !+ad_name  vacuum_module
  !+ad_summ  Module containing vacuum system routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  vaccall
  !+ad_cont  vacuum
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the vacuum system for a fusion power plant.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_variables
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use physics_variables
  use process_output
  use tfcoil_variables
  use times_variables
  use vacuum_variables

  implicit none

  private
  public :: vaccall

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vaccall(outfile,iprint)

    !+ad_name  vaccall
    !+ad_summ  Routine to call the vacuum module
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_args  iprint : input integer : Switch to write output (1=yes)
    !+ad_desc  This routine calls the main vacuum package.
    !+ad_prob  NBI gas load (qtorus) is currently hardwired to zero.
    !+ad_call  vacuum
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  12/06/13 PJK Modified gasld calculation with new qfuel definition
    !+ad_hist  15/08/13 PJK Changed arguments to vacuum call
    !+ad_hist  10/09/13 PJK Removed specific reference to D-T fuel
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(kind(1.0D0)) :: qtorus, gasld

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  (should be) NBI gas load (deuterons/second)

    qtorus = 0.0D0

    !  Total fuel gas load (kg/s)
    !  2 nuclei * nucleus-pairs/sec * mass/nucleus

    gasld = 2.0D0*qfuel * afuel*umass

    call vacuum(powfmw,rmajor,rminor,0.5D0*(scrapli+scraplo),sarea,vol, &
         shldoth,shldith,tfcth,rsldi-gapds-ddwi,tfno,tdwell,dene,idivrt, &
         qtorus,gasld,vpumpn,nvduct,dlscal,vacdshm,vcdimax,iprint,outfile)

  end subroutine vaccall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vacuum(pfusmw,r0,aw,dsol,plasma_sarea,plasma_vol,thshldo,thshldi, &
       thtf,ritf,tfno,tdwell,nplasma,ndiv,qtorus,gasld,pumpn,nduct,dlscalc, &
       mvdsh,dimax,iprint,outfile)

    !+ad_name  vacuum
    !+ad_summ  Routine to calculate the parameters of the vacuum system
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Haines, FEDC (originator)
    !+ad_auth  P Dubois, LLNL
    !+ad_auth  J Galambos, ORNL
    !+ad_auth  P C Shipe, ORNL
    !+ad_cont  N/A
    !+ad_args  pfusmw : input real : Fusion power (MW)
    !+ad_args  r0 : input real : Major radius (m)
    !+ad_args  aw : input real : Minor radius (m)
    !+ad_args  dsol : input real : Scrape-off layer average width (m)
    !+ad_args  plasma_sarea : input real : Plasma surface area (m2)
    !+ad_args  plasma_vol : input real : Plasma volume (m3)
    !+ad_args  thshldo : input real : Outboard shield thickness (m)
    !+ad_args  thshldi : input real : Inboard shield thickness (m)
    !+ad_args  thtf : input real : TF coil thickness (m)
    !+ad_args  ritf : input real : Radius of inboard TF leg point nearest plasma (m)
    !+ad_args  tfno : input real : Number of TF coils
    !+ad_args  tdwell : input real : Dwell time between pulses (s)
    !+ad_args  nplasma : input real : Plasma density (m**-3)
    !+ad_args  ndiv : input integer : Number of divertors with pumping
    !+ad_argc      (single null = 1, double null = 2 if pumping provided at
    !+ad_argc       both locations)
    !+ad_args  qtorus : input real : Gas load  from NBI (deuterons/second)
    !+ad_args  gasld : input real : Total D-T gas load (kg/s)
    !+ad_args  pumpn : output real : Number of high vacuum pumps
    !+ad_args  nduct : output integer : Number of ducts
    !+ad_args  dlscalc : output real : Duct-length equivalent for costing purposes (m)
    !+ad_args  mvdsh : output real : Mass of a single vacuum duct shield (kg)
    !+ad_args  dimax : output real : Diameter of passage from divertor to pumping ducts (m)
    !+ad_args  iprint : input integer : Switch to write output (1=yes)
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_desc  This routine calculates the parameters of the vacuum system.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  15/08/13 PJK Modified arguments for new chamber surface area
    !+ad_hisc               and volume calculations
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ndiv, iprint, outfile
    real(kind(1.0D0)), intent(in) :: pfusmw, r0, aw, dsol, plasma_sarea, plasma_vol
    real(kind(1.0D0)), intent(in) :: thshldo, thshldi, thtf, ritf, tfno, tdwell
    real(kind(1.0D0)), intent(in) :: nplasma, qtorus, gasld
    integer, intent(out) :: nduct
    real(kind(1.0D0)), intent(out) :: pumpn, dlscalc, mvdsh, dimax

    !  Local variables

    real(kind(1.0D0)), parameter :: k = 1.38D-23  !  Boltzmann's constant (J/K)

    integer :: i, imax, ntf, nflag
    real(kind(1.0D0)) :: a1,a1max,a2,a3,area,arsh,c1,c2,c3,cap,ccc, &
         ceff1,cmax,cnew,d1max,dc1,dc2,dc3,dcap,dd,densh,dnew,dout,dy, &
         fhe,frate,fsolid,k1,k2,k3,l1,l2,l3,ltot,ogas,pend,pfus,pstart, &
         pumpn1,pumpn2,source,sss,thcsh,thdsh,theta,volume,y
    real(kind(1.0D0)), dimension(4) :: s, d, ceff, xmult, sp, snet
    character(len=5) :: ipump

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    densh = 7900.0D0  !  Density of shielding material (kg/m2)
    fsolid = 0.9D0    !  Fraction of duct shielding that is solid material

    !  Pump type;
    !    ntype = 0 for turbomolecular pump (mag. bearing) with a nominal
    !              speed of 2.0 m^3/s (1.95 for N2, 1.8 for He, 1.8 for DT)
    !    ntype = 1 for compound cryopump with nominal speed of 10 m^3/s
    !              (9.0 for N2, 5.0 for He and 25. for DT)

    pfus = pfusmw * 1.0D6  !  Fusion power (W)
    ntf = int(tfno)

    !  Feed rate (gas load) of D-T into chamber (pellets + gas puffing +
    !     NBI + ...) = load from fueller + load from NBI
    !  frate (kg/s) = gasld (kg/s) + qtorus (D2/s) * 6.64D-27 (kg/D2)

    frate = gasld + qtorus * 6.64D-27

    !  Set duct shield thickness to zero for no biological shielding
    !  instead of thshldo/3.0D0

    thdsh = 0.0D0

    !  Shielding (m) between duct and TF coils is scaled from inboard shield
    !  thickness

    thcsh = thshldi / 3.0D0

    !  Multiplier to convert conductance from gas species i to nitrogen

    xmult(1) = 1.0D0    !  nitrogen
    xmult(2) = 0.423D0  !  D-T
    xmult(3) = 0.378D0  !  helium
    xmult(4) = xmult(2) !  D-T again

    nduct = ntf * ndiv

    !  Speed of high-vacuum pumps (m^3/s)

    if (ntype == 0) then
       sp(1) = 1.95D0  !  nitrogen
       sp(2) = 1.8D0   !  D-T
       sp(3) = 1.8D0   !  helium
    else
       sp(1) = 9.0D0   !  nitrogen
       sp(2) = 25.0D0  !  D-T
       sp(3) = 5.0D0   !  helium
    endif
    sp(4) = sp(2)  !  D-T

    !  Calculate required pumping speeds

    !  Initial pumpdown based on outgassing
    !  s(1) = net pump speed (N2) required for pumpdown to base pressure (m^3/s)
    !  area = vacuum chamber/fw area (m^2)  ;  outgassing area = 10 x area
    !  rat = outgassing rate (effective for N2) of plasma chamber surface (Pa-m/s)
    !  pbase = base pressure (Pa)

    !  Old method: area = 4.0D0 * pi*pi * r0 * aw * sqrt(0.5D0*(1.0D0 + kappa*kappa))

    area = plasma_sarea * (aw+dsol)/aw

    ogas = rat * area * 10.0D0  !  Outgassing rate (Pa-m^3/s)
    s(1) = ogas  / pbase

    !  Pumpdown between burns
    !  s(2) = net pump speed (DT) required for pumpdown between burns (m^3/s)
    !  tn = temperature of neutral gas in chamber (K)
    !  tdwell = dwell time between burns (s)

    pend = 0.5D0*nplasma * k * tn  !  pressure in plasma chamber after burn (Pa)
    pstart = 0.01D0 * pend  !  pressure in chamber before start of burn (Pa)

    !  Chamber volume (m^3)

    !  Old method: volume = 2.0D0 * pi*pi * r0 * aw*aw * kappa

    volume = plasma_vol * (aw+dsol)*(aw+dsol)/(aw*aw)

    s(2) = volume / tdwell * log(pend / pstart)

    !  Helium ash removal
    !  s(3) = net pump speed (He) required for helium ash removal (m^3/s)
    !  source = alpha production rate (pa - m^3/s)
    !  fhe = fraction of neutral gas in divertor chamber that is helium
    !  prdiv = pressure in divertor chamber during burn (Pa)

    source = pfus * 1.47D-09
    fhe = source / (frate * 4.985D5 )
    s(3) = source / prdiv / fhe

    !  Removal of dt on steady state basis
    !  s(4) = net speed (D-T) required to remove dt at fuelling rate (m^3/s)

    s(4) = (frate * 4.985D5 - source) / ( prdiv * (1.0D0 - fhe) )

    !  Calculate conductance of a single duct

    imax = 1
    cmax = 0.01D0
    pumpn = 1.0D0
    nflag = 0  !  Control option if ducts are too small in x-sectional area
    !  = 1 if problem is identified in output, but run continues
    !  = 0 otherwise

    do i = 1,4

       sss = nduct / &
            (1.0D0/sp(i)/pumpn + 1.0D0/cmax * xmult(i)/xmult(imax))
       if (sss > s(i)) cycle
       imax = i

       ccc = 2.0D0 * s(i) / nduct
       pumpn1 = 1.0D0 / ( sp(i) * (nduct/s(i) - 1.0D0/ccc) )
       pumpn2 = 1.01D0 * s(i) / (sp(i)*nduct)
       pumpn = max(pumpn, pumpn1, pumpn2)
       ceff(i) = 1.0D0 / ( nduct/s(i) - 1.0D0/(sp(i)*pumpn) )

       l1 = thshldo + thtf  !  Length of passage from divertor to ducts (m)
       l2 = thshldo + 4.0D0 !  Length of ducts from divertor passage to elbow (m)
       l3 = 2.0D0           !  Length of ducts from elbow to hi-vac pumps (m)
       ltot = l1 + l2 + l3

       !  Newton's method solution for duct diameter

       outer: do
          d(i) = 1.0D0

          inner: do
             a1 = 0.25D0*pi * d(i) * d(i)  !  Area of aperture and duct (m^2)
             a2 = 1.44D0 * a1
             a3 = a2
             k1 = 4.0D0/3.0D0 * d(i) / (l1 + 4.0D0/3.0D0 * d(i))
             k2 = 4.0D0/3.0D0 * d(i)*1.2D0 / (l2 + 4.0D0/3.0D0 * d(i)*1.2D0)
             k3 = 4.0D0/3.0D0 * d(i)*1.2D0 / (l3 + 4.0D0/3.0D0 * d(i)*1.2D0)
             cap = 119.0D0 * a1 / xmult(i)
             dcap =  2.0D0 * cap / d(i)
             c1 = 119.0D0 *  a1 * k1 / xmult(i)
             dc1 = c1 / d(i) * (3.0D0 - k1)
             c2 = 119.0D0 * a2 * k2 / xmult(i)
             dc2 = c2 / d(i) / 1.2D0 * (3.0D0 - k2)
             c3 = 119.0D0 * a3 * k3 / xmult(i)
             dc3 = c3 / d(i) / 1.2D0 * (3.0D0 - k3)
             cnew = 1.0D0 / (1.0D0/cap + 1.0D0/c1 + 1.0D0/c2 + 1.0D0/c3)
             y = -ceff(i) + cnew
             dy = cnew*cnew *(dcap/cap/cap +dc1/c1/c1 +dc2/c2/c2 +dc3/c3/c3)
             dnew = d(i) - y / dy
             dd = abs((d(i) - dnew)/d(i))
             d(i) = dnew
             if (dd <= 0.01D0) exit inner
          end do inner

          theta = pi / ntf

          !  Area between adjacent TF coils available for pump ducts
          !  ritf = outer radius of inboard leg of TF coil (m)

          a1max = (r0 + aw - ritf - thcsh / tan(theta))**2 * tan(theta)
          d1max = sqrt (4.0D0 * a1max / pi)  !  Equivalent diameter
          if (a1 < a1max) exit outer

          ceff(i) = 0.9D0 * ceff(i)
          if (ceff(i) > (1.1D0 * s(i))) then
             cycle outer
          else
             !  Ducts are not big enough. Flag and continue.
             nflag = 1
             exit outer
          end if

       end do outer

       cmax = ceff(i)

    end do

    pumpn = pumpn * nduct

    !  d(imax) = diameter of passage from divertor to pumping ducts (m)
    !  dout    = diameter of ducts from passage to hi-vac pumps (m)

    dout = d(imax) * 1.2D0

    !  Net pumping speeds provided by vacuum pumping system
    !  snet(1) - net pump speed (N2) provided (m^3/s)
    !  snet(2) - net pump speed (D-T) provided (m^3/s)
    !  snet(3) - net pump speed (He) provided (m^3/s)
    !  snet(4) - snet(2)

    do i = 1,4
       ceff1 = ceff(imax) * nduct
       snet(i) = 1.0D0/ &
            ( 1.0D0 / ( ceff1*xmult(imax)/xmult(i) ) + 1.0D0/sp(i)/pumpn )
    end do

    !  If cryopumps are used then an additional pump is required
    !  for continuous operation with regeneration.

    if (ntype == 1) pumpn = pumpn * 2.0D0

    !  Information for costing routine

    dlscalc = l1 * d(imax)**1.4D0 + (ltot-l1) * (d(imax)*1.2D0)**1.4D0

    !  Mass of duct shielding

    arsh = 0.25D0*pi*( (d(imax)*1.2D0 + thdsh)**2 - (d(imax)*1.2D0)**2 )
    mvdsh = arsh * (ltot-l1) * densh * fsolid

    dimax = d(imax)

    if ((iprint == 0).or.(sect15 == 0)) return

    !  Output section

    call oheadr(outfile,'Vacuum System')

    call ocmmnt(outfile,'Pumpdown to Base Pressure :')
    call oblnkl(outfile)
    call ovarre(outfile,'First wall outgassing rate (Pa m/s)','(rat)',rat)
    call ovarre(outfile,'Total outgassing load (Pa m3/s)','(ogas)',ogas)
    call ovarre(outfile,'Base pressure required (Pa)','(pbase)',pbase)
    call ovarre(outfile,'Required N2 pump speed (m3/s)','(s(1))',s(1))
    call ovarre(outfile,'N2 pump speed provided (m3/s)','(snet(1))',snet(1))

    call osubhd(outfile,'Pumpdown between Burns :')
    call ovarre(outfile,'Plasma chamber volume (m3)','(volume)',volume)
    call ovarre(outfile,'Chamber pressure after burn (Pa)','(pend)',pend)
    call ovarre(outfile,'Chamber pressure before burn (Pa)','(pstart)', &
         pstart)
    call ovarre(outfile,'Dwell time between burns (s)','(tdwell)',tdwell)
    call ovarre(outfile,'Required D-T pump speed (m3/s)','(s(2))',s(2))
    call ovarre(outfile,'D-T pump speed provided (m3/s)','(snet(2))',snet(2))

    call osubhd(outfile,'Helium Ash Removal :')
    call ovarre(outfile,'Divertor chamber gas pressure (Pa)','(prdiv)', &
         prdiv)
    call ovarre(outfile,'Helium gas fraction in divertor chamber','(fhe)', &
         fhe)
    call ovarre(outfile,'Required helium pump speed (m3/s)','(s(3))',s(3))
    call ovarre(outfile,'Helium pump speed provided (m3/s)','(snet(3))', &
         snet(3))

    call osubhd(outfile,'D-T Removal at Fuelling Rate :')
    call ovarre(outfile,'D-T fuelling rate (kg/s)','(frate)',frate)
    call ovarre(outfile,'Required D-T pump speed (m3/s)','(s(4))',s(4))
    call ovarre(outfile,'D-T pump speed provided (m3/s)','(snet(4))',snet(4))

    if (nflag == 1) then
       call oblnkl(outfile)
       call ocmmnt(outfile,'Vacuum pumping ducts are space limited.')
       write(outfile,10) d1max
10     format(' Maximum duct diameter is only ',f8.2,'m')
       call ocmmnt(outfile,'Conductance is inadequate.')
       call oblnkl(outfile)
    end if

    if (ntype == 1) then
       ipump = 'cryo '
    else
       ipump = 'turbo'
    end if

    call oblnkl(outfile)
    call ocmmnt(outfile,'The vacuum pumping system size is governed by the')

    select case (imax)
    case (1)
       call ocmmnt(outfile,'requirements for pumpdown to base pressure.')
    case (2)
       call ocmmnt(outfile,'requirements for pumpdown between burns.')
    case (3)
       call ocmmnt(outfile,'requirements for helium ash removal.')
    case default
       call ocmmnt(outfile,'requirements for D-T removal at fuelling rate.')
    end select

    call oblnkl(outfile)

    call ovarin(outfile,'Number of large pump ducts','(nduct)',nduct)
    call ovarre(outfile,'Passage diameter, divertor to ducts (m)', &
         '(d(imax))',d(imax))
    call ovarre(outfile,'Passage length (m)','(l1)',l1)
    call ovarre(outfile,'Diameter of ducts (m)','(dout)',dout)
    call ovarre(outfile,'Duct length, divertor to elbow (m)','(l2)',l2)
    call ovarre(outfile,'Duct length, elbow to pumps (m)','(l3)',l3)
    call ovarre(outfile,'Number of pumps','(pumpn)',pumpn)
    call oblnkl(outfile)
    write(outfile,20) ipump
20  format(' The vacuum system uses ',a5,'pumps')

  end subroutine vacuum

end module vacuum_module
