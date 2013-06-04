!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module build_module

  !+ad_name  build_module
  !+ad_summ  Module containing machine build routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  radialb
  !+ad_cont  vbuild
  !+ad_cont  divgeom
  !+ad_cont  rippl
  !+ad_cont  portsz
  !+ad_cont  dshellarea
  !+ad_cont  eshellarea
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  geometry (radial and vertical builds) of the fusion power
  !+ad_desc  plant core.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  rfp_variables
  !+ad_call  tfcoil_variables
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  09/05/13 PJK Added dshellarea, eshellarea
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use current_drive_variables
  use divertor_variables
  use fwbs_variables
  use physics_variables
  use process_output
  use rfp_variables
  use tfcoil_variables

  implicit none

  private
  public :: radialb,vbuild,portsz,dshellarea,eshellarea

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine radialb(outfile,iprint)

    !+ad_name  radialb
    !+ad_summ  Radial build
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine determines the radial build of the machine.
    !+ad_prob  None
    !+ad_call  dshellarea
    !+ad_call  eshellarea
    !+ad_call  obuild
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_call  rippl
    !+ad_hist  26/07/11 PJK Initial F90 version
    !+ad_hist  24/09/12 PJK Swapped argument order
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  18/12/12 PJK/RK Added single-null code
    !+ad_hist  02/05/13 PJK Changed snull=1 top shield thickness to shldtth
    !+ad_hist  09/05/13 PJK Changed first wall area calculation to be
    !+ad_hisc               consistent with fwbsshape switch
    !+ad_hist  15/05/13 PJK Swapped build order of vacuum vessel and gap
    !+ad_hist  22/05/13 PJK Introduced fwareaib, fwareaob; added blanket thickness
    !+ad_hisc               calculations
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: a1,a2,hbot,hfw,htop,r1,r2,r3,radius,rtotl,vbuild

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate total blanket thicknesses if blktmodel > 0

    if (blktmodel > 0) then
       blnkith = blbuith + blbmith + blbpith
       blnkoth = blbuoth + blbmoth + blbpoth
       blnktth = 0.5D0*(blnkith+blnkoth)
    end if

    !  Radial build to centre of plasma (should be equal to rmajor)

    rbld = bore + ohcth + gapoh + bcylth + tfcth + gapds + ddwi + &
         shldith + blnkith + fwith + scrapli + rminor

    !  Radius to inner edge of inboard shield

    rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

    !  Radius to outer edge of outboard shield

    rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

    !  Thickness of outboard TF coil legs

    tfthko = tfootfi*tfcth 

    !  Radius to centre of outboard TF coil legs

    rtot = rsldo + ddwi + gapomin + 0.5D0*tfthko

    !  Check ripple

    call rippl(ripmax,rmajor,rminor,rtot,tfno,ripple,rtotl)

    !  If the ripple is too large then move the outboard TF coil leg

    if (rtotl > rtot) then
       rtot = rtotl
       gapsto = rtot - 0.5D0*tfthko - ddwi - rsldo
    else
       gapsto = gapomin
    end if

    !  Calculate first wall area
    !  Old calculation... includes a mysterious factor 0.875
    !fwarea = 0.875D0 * &
    !     ( 4.0D0*pi**2*sf*rmajor*(rminor+0.5D0*(scrapli+scraplo)) )

    !  Half-height of first wall (internal surface)

    hbot = rminor*kappa + vgap + divfix - blnktth - 0.5D0*(fwith+fwoth)
    if (idivrt == 2) then  !  (i.e. snull=0)
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo)
    end if
    hfw = 0.5D0*(htop + hbot)

    if ((itart == 1).or.(fwbsshape == 1)) then  !  D-shaped

       !  Major radius to outer edge of inboard section

       r1 = rmajor - rminor - scrapli

       !  Horizontal distance between inside edges,
       !  i.e. outer radius of inboard part to inner radius of outboard part

       r2 = (rmajor + rminor + scraplo) - r1

       !  Calculate surface area, assuming 100% coverage

       call dshellarea(r1,r2,hfw,fwareaib,fwareaob,fwarea)

       !  Apply area coverage factor - uses fhole

       fwareaib = (1.0D0-fhole) * fwareaib
       fwareaob = (1.0D0-fhole) * fwareaob
       fwarea = fwareaib + fwareaob

    else  !  Cross-section is assumed to be defined by two ellipses

       !  Major radius to centre of inboard and outboard ellipses
       !  (coincident in radius with top of plasma)

       r1 = rmajor - rminor*triang

       !  Distance between r1 and outer edge of inboard section

       r2 = r1 - (rmajor - rminor - scrapli)

       !  Distance between r1 and inner edge of outboard section

       r3 = (rmajor + rminor + scraplo) - r1

       !  Calculate surface area, assuming 100% coverage

       call eshellarea(r1,r2,r3,hfw,fwareaib,fwareaob,fwarea)

       !  Apply area coverage factor - uses fhole

       fwareaib = (1.0D0-fhole) * fwareaib
       fwareaob = (1.0D0-fhole) * fwareaob
       fwarea = fwareaib + fwareaob

    end if

    if ((iprint == 0).or.(sect06 == 0)) return

    !  Print out device build

    call oheadr(outfile,'Radial Build')

    write(outfile,10)
10  format(t43,'Thickness (m)',t60,'Radius (m)')

    radius = 0.0D0
    call obuild(outfile,'Device centreline',0.0D0,radius)

    radius = radius + bore
    call obuild(outfile,'Machine bore',bore,radius)

    if (itart == 1) then

       radius = radius + bcylth
       call obuild(outfile,'Bucking cylinder',bcylth,radius)

       radius = radius + tfcth
       call obuild(outfile,'TF coil inboard leg',tfcth,radius)

       radius = radius + gapoh
       call obuild(outfile,'Gap',gapoh,radius)

       radius = radius + ohcth
       call obuild(outfile,'OH coil',ohcth,radius)

    else

       radius = radius + ohcth
       call obuild(outfile,'OH coil',ohcth,radius)

       radius = radius + gapoh
       call obuild(outfile,'Gap',gapoh,radius)

       radius = radius + bcylth
       call obuild(outfile,'Bucking cylinder',bcylth,radius)

       radius = radius + tfcth
       call obuild(outfile,'TF coil inboard leg',tfcth,radius)

    end if

    radius = radius + gapds
    call obuild(outfile,'Gap',gapds,radius)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius)

    radius = radius + shldith
    call obuild(outfile,'Inboard shield',shldith,radius)

    radius = radius + blnkith
    call obuild(outfile,'Inboard blanket',blnkith,radius)

    radius = radius + fwith
    call obuild(outfile,'Inboard first wall',fwith,radius)

    radius = radius + scrapli
    call obuild(outfile,'Inboard scrape-off',scrapli,radius)

    radius = radius + rminor
    call obuild(outfile,'Plasma geometric centre',rminor,radius)

    radius = radius + rminor
    call obuild(outfile,'Plasma outboard edge',rminor,radius)

    radius = radius + scraplo
    call obuild(outfile,'Outboard scrape-off',scraplo,radius)

    radius = radius + fwoth
    call obuild(outfile,'Outboard first wall',fwoth,radius)

    radius = radius + blnkoth
    call obuild(outfile,'Outboard blanket',blnkoth,radius)

    radius = radius + shldoth
    call obuild(outfile,'Outboard shield',shldoth,radius)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius)

    radius = radius + gapsto
    call obuild(outfile,'Gap',gapsto,radius)

    radius = radius + tfthko
    call obuild(outfile,'TF coil outboard leg',tfthko,radius)

    !  Vertical build

    call oheadr(outfile,'Vertical Build')

    if (snull == 0) then
       call ocmmnt(outfile,'Double null case')

       write(outfile,20)
20     format(t43,'Thickness (m)',t60,'Height (m)')

       vbuild = 0.0D0
       call obuild(outfile,'Midplane',0.0D0,vbuild)

       vbuild = vbuild + rminor * kappa
       call obuild(outfile,'Plasma top',rminor*kappa,vbuild)

       vbuild = vbuild + vgap
       call obuild(outfile,'Top scrape-off',vgap,vbuild)

       vbuild = vbuild + divfix
       call obuild(outfile,'Divertor structure',divfix,vbuild)

       vbuild = vbuild + shldtth
       call obuild(outfile,'Top shield',shldtth,vbuild)

       vbuild = vbuild + ddwi
       call obuild(outfile,'Vacuum vessel',ddwi,vbuild)

       vbuild = vbuild + vgap2
       call obuild(outfile,'Gap',vgap2,vbuild)

       vbuild = vbuild + tfcth
       call obuild(outfile,'TF coil',tfcth,vbuild)

    else
       call ocmmnt(outfile,'Single null case')

       write(outfile,20)

       vbuild = tfcth + vgap2 + ddwi + shldtth + blnktth + &
            0.5D0*(fwith+fwoth + scrapli+scraplo) + rminor*kappa
     
       call obuild(outfile,'TF coil',tfcth,vbuild)
       vbuild = vbuild - tfcth

       call obuild(outfile,'Gap',vgap2,vbuild)
       vbuild = vbuild - vgap2

       call obuild(outfile,'Vacuum vessel',ddwi,vbuild)
       vbuild = vbuild - ddwi

       call obuild(outfile,'Top shield',shldtth,vbuild)
       vbuild = vbuild - shldtth

       call obuild(outfile,'Top blanket',blnktth,vbuild)
       vbuild = vbuild - blnktth

       call obuild(outfile,'Top first wall',0.5D0*(fwith+fwoth),vbuild)
       vbuild = vbuild - 0.5D0*(fwith+fwoth)

       call obuild(outfile,'Top scrape-off',0.5D0*(scrapli+scraplo),vbuild)
       vbuild = vbuild - 0.5D0*(scrapli+scraplo)

       call obuild(outfile,'Plasma top',rminor*kappa,vbuild)
       vbuild = vbuild - rminor*kappa

       call obuild(outfile,'Midplane',0.0D0,vbuild)

       vbuild = vbuild - rminor*kappa
       call obuild(outfile,'Plasma bottom',rminor*kappa,vbuild)

       vbuild = vbuild - vgap
       call obuild(nout,'Lower scrape-off',vgap,vbuild)

       vbuild = vbuild - divfix
       call obuild(outfile,'Divertor structure',divfix,vbuild)

       vbuild = vbuild - shldtth
       call obuild(nout,'Lower shield',shldtth,vbuild)

       vbuild = vbuild - ddwi
       call obuild(nout,'Vacuum vessel',ddwi,vbuild)

       vbuild = vbuild - vgap2
       call obuild(nout,'Gap',vgap2,vbuild)

       vbuild = vbuild - tfcth
       call obuild(nout,'TF coil',tfcth,vbuild)
	
    end if

    !  Port size information

    call osubhd(outfile,'Port Size Information :')
    call ovarre(outfile,'Port width (m)','(prtsz)',prtsz)
    call ovarre(outfile,'Port requirement for beams (m)','(prtszreq)', &
         prtszreq)

  end subroutine radialb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vbuild

    !+ad_name  vbuild
    !+ad_summ  Vertical build
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine determines the vertical build of the machine
    !+ad_desc  inside the TF coil.
    !+ad_prob  None
    !+ad_call  divgeom
    !+ad_hist  26/07/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/12/12 PJK/RK Added single-null code
    !+ad_hist  15/05/13 PJK Swapped build order of vacuum vessel and gap
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: divht

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate the divertor geometry

    call divgeom(divht)

    if (vgaptf == 0.0D0) then
       vgap = divht
    else
       vgap = vgaptf
    end if

    !  Height to inside edge of TF coil

    if (irfp == 0) then
       hmax = rminor*kappa + vgap + divfix + shldtth + ddwi + vgap2
    else
       !  RFP: TF coil is assumed circular
       hmax = 0.5D0 * &
            (gapds+ddwi+shldith+blnkith+fwith+scrapli+rminor &
            +rminor+scraplo+fwoth+blnkoth+shldoth+ddwi+gapsto)
    end if

    !  Vertical locations of divertor coils

    if (snull == 0) then
       hpfu = hmax + tfcth
       hpfdif = 0.0D0
    else
       hpfu = tfcth + vgap2 + ddwi + shldtth + blnktth + &
            0.5D0*(fwith+fwoth + scrapli+scraplo) + rminor*kappa
       hpfdif = (hpfu - (hmax+tfcth)) / 2.0D0
    end if

  end subroutine vbuild

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divgeom(divht)

    !+ad_name  divgeom
    !+ad_summ  Divertor geometry calculation
    !+ad_type  Subroutine
    !+ad_auth  J Galambos, ORNL
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  divht : output real : divertor height (m)
    !+ad_desc  This subroutine determines the divertor geometry.
    !+ad_desc  The inboard (i) and outboard (o) plasma surfaces
    !+ad_desc  are approximated by arcs, and followed past the X-point to
    !+ad_desc  determine the maximum height.
    !+ad_prob  No evidence of any inboard plasma surface being used...
    !+ad_call  None
    !+ad_hist  29/01/96 PJK Added TART option with expanded divertor chamber
    !+ad_hist  26/07/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_stat  Okay
    !+ad_docs  TART option: Peng SOFT paper
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: divht

    !  Local variables

    real(kind(1.0D0)), parameter :: soleno = 0.2D0  !  length along outboard divertor
    !  plate that scrapeoff hits
    real(kind(1.0D0)) :: kap,tri,xpointo,rprimeo,phio,thetao
    real(kind(1.0D0)) :: yspointo,xspointo,yprimeb 

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  TART option with expanded divertor chamber

    if (itart == 1) then
       divht = 1.75D0 * rminor
       return
    end if

    !  Conventional tokamak divertor model

    kap = kappa
    tri = triang

    !  Outboard side
    !  plsepo = poloidal length along the separatrix from null to
    !           strike point on outboard [default 1.5 m]
    !  thetao = arc angle between the strike point and the null point

    xpointo = rmajor + 0.5D0*rminor*(kap**2 + tri**2 - 1.0D0) / &
         (1.0D0 - tri)
    rprimeo = (xpointo - rmajor + rminor)
    phio = asin(kap*rminor/rprimeo)
    thetao = plsepo/rprimeo

    !  Initial strike point

    yspointo = rprimeo * sin(thetao + phio)
    xspointo = xpointo - rprimeo * cos(thetao + phio)

    !  Outboard strike point radius - normalized to ITER

    rstrko = xspointo + 0.14D0

    !  Uppermost divertor strike point (end of power decay)
    !  anginc = angle of incidence of scrape-off field lines on the
    !           divertor (rad)

    !+**PJK 25/07/11 Changed sign of anginc contribution
    yprimeb = soleno * cos(thetao + phio - anginc)

    divht = yprimeb + yspointo - kap*rminor

  end subroutine divgeom

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rippl(ripmax,rmajor,rminor,rtot,tfno,ripple,rtotl)

    !+ad_name  rippl
    !+ad_summ  TF ripple calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  ripmax : input real : max ripple at plasma edge (peak to average) (%)
    !+ad_args  rmajor : input real : plasma major radius (m)
    !+ad_args  rminor : input real : plasma minor radius (m)
    !+ad_args  rtot   : input real : default radius to the outboard TF coil leg (m)
    !+ad_args  tfno   : input real(!) : number of TF coils
    !+ad_args  ripple : output real : ripple at plasma edge (%)
    !+ad_args  rtotl  : output real : required minimum radius to the centre
    !+ad_argc                         of the outboard TF coil leg (m)
    !+ad_desc  Subroutine to calculate TFC ripple and outboard TFC leg radius.
    !+ad_desc  Input the max. ripple and default outboard leg location and the
    !+ad_desc  routine checks to see if the ripple is OK. If not it moves
    !+ad_desc  the outboard leg appropriately.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: ripmax,rmajor,rminor,rtot,tfno
    real(kind(1.0D0)), intent(out) :: ripple,rtotl

    !  Local variables

    real(kind(1.0D0)) :: prip,rotrp,pripc,coeff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    coeff = 1.03333D0 &
         + 0.210480D0 * tfno &
         - 4.45253D-2 * tfno**2 &
         + 3.50210D-3 * tfno**3 &
         - 1.28945D-4 * tfno**4 &
         + 1.84776D-6 * tfno**5

    prip = 0.01D0 * ripmax/coeff
    rotrp = 1.023D0*(rmajor+rminor)/prip**(1.0D0/tfno)

    if (rotrp > rtot) then
       rtotl = rotrp
       pripc = prip * 100.0D0
       ripple = pripc * coeff
    else
       rtotl = rtot
       prip = (1.023D0*(rmajor+rminor)/rtot)**(tfno)
       pripc = prip*100.0D0
       ripple = pripc * coeff
    end if

  end subroutine rippl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine portsz

    !+ad_name  portsz
    !+ad_summ  Port size calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine finds the required distance between the TF legs
    !+ad_desc  for adequate beam access.
    !+ad_desc  <P>The outputs from the routine are
    !+ad_desc  <UL> <P><LI>prtsz : available port size (m)
    !+ad_desc       <P><LI>prtszreq : required port size (m) </UL>
    !+ad_prob  None
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: rtan,tfoll,tfolw,rl1,ang1,ang2,ang3, &
         ps1,rl2,ang4,rl3,ang5,ang6,ang7,ang8,ps2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Beam tangency radius (m)

    rtan = frbeam * rmajor

    !  Assume the outboard TF leg has a width / depth ratio of 1 / 2
    !  Depth and width calculated from the cross-sectional area

    tfoll = 1.414D0 * sqrt(arealeg)  !  depth (m)
    tfolw = sqrt(arealeg) / 1.414D0  !  width (m)

    !  beamwd = beam width (m)
    !  rtot   = radius to the centre of the outboard TF leg (m)
    !  tfno   = number of TF coils

    rl1 = sqrt ( (rtot - 0.5D0*tfthko)**2 + tfolw**2 )
    ang1 = asin ( (rtan + 0.5D0*beamwd) / rl1 )
    ang2 = 0.5D0*pi - pi/tfno + asin( tfolw/(2.0D0*rl1) )
    ang3 = ang1 + ang2 - 0.5D0*pi
    ps1 = beamwd / cos(ang3)

    if (rtan > (0.5D0*beamwd)) then
       rl2 = rtot - 0.5D0*tfthko + tfoll
       ang4 = atan(tfolw/rl2)
       rl3 = rl2/cos(ang4)
       ang5 = acos ( (rl3**2 + tfoll**2 - rl1**2)/(2.0D0*rl3*tfoll))
       ang6 = asin( (rtan - 0.5D0*beamwd) / rl3 )
       ang7 = ang6 - ang5
       ang8 = 0.5D0*pi - ang3
       ps2 = tfoll * sin(ang7) / sin(ang8)
    else
       ps2 = 0.0D0
    end if

    prtszreq = ps1 + ps2

    !  Port size available

    prtsz = ( 2.0D0 * pi * (rtot - 0.5D0*tfthko) - tfno* tfolw ) / tfno

  end subroutine portsz

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshellarea(rmajor,rminor,zminor,ain,aout,atot)

    !+ad_name  dshellarea
    !+ad_summ  Routine to calculate the inboard, outboard and total surface areas
    !+ad_summ  of a D-shaped toroidal shell
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real : major radius of inboard straight section (m)
    !+ad_args  rminor : input real : horizontal width of shell (m)
    !+ad_args  zminor : input real : vertical half-height of shell (m)
    !+ad_args  ain    : output real : surface area of inboard straight section (m3)
    !+ad_args  aout   : output real : surface area of outboard curved section (m3)
    !+ad_args  atot   : output real : total surface area of shell (m3)
    !+ad_desc  This routine calculates the surface area of the inboard and outboard
    !+ad_desc  sections of a D-shaped toroidal shell defined by the above input
    !+ad_desc  parameters.
    !+ad_desc  The inboard section is assumed to be a cylinder.
    !+ad_desc  The outboard section is defined by a semi-ellipse, centred on the
    !+ad_desc  major radius of the inboard section.
    !+ad_desc  <P>See also <A HREF="dshellvol.html"><CODE>dshellvol</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rmajor,rminor,zminor
    real(kind(1.0D0)), intent(out) :: ain,aout,atot

    !  Local variables

    real(kind(1.0D0)) :: elong

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Area of inboard cylindrical shell

    ain = 4.0D0*zminor*pi*rmajor

    !  Area of elliptical outboard section

    elong = zminor/rminor
    aout = twopi * elong * (pi*rmajor*rminor + 2.0D0*rminor*rminor)

    !  Total surface area

    atot = ain + aout

  end subroutine dshellarea

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eshellarea(rshell,rmini,rmino,zminor,ain,aout,atot)

    !+ad_name  eshellarea
    !+ad_summ  Routine to calculate the inboard, outboard and total surface areas
    !+ad_summ  of a toroidal shell comprising two elliptical sections
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rshell : input real : major radius of centre of both ellipses (m)
    !+ad_args  rmini  : input real : horizontal distance from rshell to
    !+ad_argc                        inboard elliptical shell (m)
    !+ad_args  rmino  : input real : horizontal distance from rshell to
    !+ad_argc                        outboard elliptical shell (m)
    !+ad_args  zminor : input real : vertical internal half-height of shell (m)
    !+ad_args  ain    : output real : surface area of inboard section (m3)
    !+ad_args  aout   : output real : surface area of outboard section (m3)
    !+ad_args  atot   : output real : total surface area of shell (m3)
    !+ad_desc  This routine calculates the surface area of the inboard and outboard
    !+ad_desc  sections of a toroidal shell defined by two co-centred semi-ellipses.
    !+ad_desc  <P>See also <A HREF="eshellvol.html"><CODE>eshellvol</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rshell,rmini,rmino,zminor
    real(kind(1.0D0)), intent(out) :: ain,aout,atot

    !  Local variables

    real(kind(1.0D0)) :: elong

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Inboard section

    elong = zminor/rmini
    ain = twopi * elong * (pi*rshell*rmini - 2.0D0*rmini*rmini)

    !  Outboard section

    elong = zminor/rmino
    aout = twopi * elong * (pi*rshell*rmino + 2.0D0*rmino*rmino)

    !  Total surface area

    atot = ain + aout

  end subroutine eshellarea

end module build_module
