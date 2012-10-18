!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine radialb(outfile,iprint)

  !+ad_name  radialb
  !+ad_summ  Radial build
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This subroutine determines the radial build of the machine.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_call  build.h90
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
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use physics_variables
  use process_output
  use tfcoil_variables

  implicit none

  include 'build.h90'

  !  Arguments

  integer, intent(in) :: iprint,outfile

  !  Local variables

  real(kind(1.0D0)) :: divfix,rtotl,radius,vbuild
  common/dbld/divfix  !  set in subroutine vbuild

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Radial build to centre of plasma (should be equal to rmajor)

  rbld = bore + ohcth + gapoh + bcylth + tfcth + ddwi + &
       gapds + shldith + blnkith + fwith + scrapli + rminor

  !  Radius to inner edge of inboard shield

  rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

  !  Radius to outer edge of outboard shield

  rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

  !  Thickness of outboard TF coil legs

  tfthko = tfootfi*tfcth 

  !  Radius to centre of outboard TF coil legs

  rtot = rsldo + gapomin + ddwi + 0.5d0 * tfthko

  !  Check ripple

  call rippl(ripmax,rmajor,rminor,rtot,tfno,ripple,rtotl)

  !  If the ripple is too large then move the outer TF coil leg

  if (rtotl > rtot) then
     rtot = rtotl
     gapsto = rtot - rsldo - ddwi - tfthko/2.d0
  else
     gapsto = gapomin
  end if

  !  Calculate first wall area (includes a mysterious factor 0.875...)

  fwarea = 0.875D0 * &
       ( 4.0D0*pi**2*sf*rmajor*(rminor+0.5D0*(scrapli+scraplo)) )

  if ((iprint == 0).or.(sect06 == 0)) return

  !  Print out device build

  call oheadr(outfile,'Radial Build')

  write(outfile,10)
10 format(t43,'Thickness (m)',t60,'Radius (m)')

  radius = 0.0D0
  call obuild(outfile,'Device centreline',0.0D0,radius)

  radius = radius + bore
  call obuild(outfile,'Machine bore',bore,radius)

  if (itart.eq.1) then

     radius = radius + bcylth
     call obuild(outfile,'Bucking cylinder',bcylth,radius)

     radius = radius + tfcth
     call obuild(outfile,'TF coil inner leg',tfcth,radius)

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
     call obuild(outfile,'TF coil inner leg',tfcth,radius)

  end if

  radius = radius + ddwi
  call obuild(outfile,'Vacuum vessel',ddwi,radius)

  radius = radius + gapds
  call obuild(outfile,'Gap',gapds,radius)

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
  call obuild(outfile,'Plasma outer edge',rminor,radius)

  radius = radius + scraplo
  call obuild(outfile,'Outboard scrape-off',scraplo,radius)

  radius = radius + fwoth
  call obuild(outfile,'Outboard first wall',fwoth,radius)

  radius = radius + blnkoth
  call obuild(outfile,'Outboard blanket',blnkoth,radius)

  radius = radius + shldoth
  call obuild(outfile,'Outboard shield',shldoth,radius)

  radius = radius + gapsto
  call obuild(outfile,'Gap',gapsto,radius)

  radius = radius + ddwi
  call obuild(outfile,'Vacuum vessel',ddwi,radius)

  radius = radius + tfthko
  call obuild(outfile,'TF coil outer leg',tfthko,radius)

  !  Vertical build

  call oheadr(outfile,'Vertical Build')

  call ocmmnt(outfile,'Double null case')

  write(outfile,20)
20 format(t43,'Thickness (m)',t60,'Height (m)')

  vbuild = 0.D0
  call obuild(outfile,'Midplane',0.0D0,vbuild)

  vbuild = vbuild + rminor * kappa
  call obuild(outfile,'Plasma top',rminor*kappa,vbuild)

  vbuild = vbuild + vgap
  call obuild(outfile,'Top scrape-off',vgap,vbuild)

  vbuild = vbuild + divfix
  call obuild(outfile,'Divertor structure',divfix,vbuild)

  vbuild = vbuild + shldtth
  call obuild(outfile,'Top shield',shldtth,vbuild)

  vbuild = vbuild + vgap2
  call obuild(outfile,'Gap',vgap2,vbuild)

  !+**PJK 25/07/11 Added dewar thickness to vertical build
  vbuild = vbuild + ddwi
  call obuild(outfile,'Vacuum vessel',ddwi,vbuild)

  vbuild = vbuild + tfcth
  call obuild(outfile,'TF coil',tfcth,vbuild)

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
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine determines the vertical build of the machine
  !+ad_desc  inside the TF coil.
  !+ad_prob  None
  !+ad_call  physics_variables
  !+ad_call  build.h90
  !+ad_call  rfp.h90
  !+ad_call  divgeom
  !+ad_hist  26/07/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use physics_variables

  implicit none

  include 'build.h90'
  include 'rfp.h90'

  !  Arguments

  !  Local variables

  real(kind(1.0D0)) :: divfix = 0.2D0
  common/dbld/divfix

  real(kind(1.0D0)) :: divht

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Calculated the divertor geometry

  call divgeom(divht)

  if (vgaptf == 0.0D0) then
     vgap = divht
  else
     vgap = vgaptf
  end if

  !  Height to inside edge of TF coil
  !  PJK 25/07/11 Added previously-missing ddwi

  if (irfp == 0) then
     hmax = rminor * kappa + vgap + shldtth + divfix + vgap2 + ddwi
  else
     !  RFP: TF coil is assumed circular
     hmax = 0.5D0 * &
          (ddwi+gapds+shldith+blnkith+fwith+scrapli+rminor &
          +rminor+scraplo+fwoth+blnkoth+shldoth+gapsto+ddwi)
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
  !+ad_desc  The inner (i) and outer (o) plasma surfaces
  !+ad_desc  are approximated by arcs, and followed past the X-point to
  !+ad_desc  determine the maximum height.
  !+ad_prob  No evidence of any inner plasma surface being used...
  !+ad_call  divertor_variables
  !+ad_call  physics_variables
  !+ad_call  build.h90
  !+ad_hist  29/01/96 PJK Added TART option with expanded divertor chamber
  !+ad_hist  26/07/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_stat  Okay
  !+ad_docs  TART option: Peng SOFT paper
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use divertor_variables
  use physics_variables

  implicit none

  INCLUDE 'build.h90'

  !  Arguments

  real(kind(1.0D0)), intent(out) :: divht

  !  Local variables

  real(kind(1.0D0)), parameter :: soleno = 0.2D0  !  length along outer divertor
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

  !  Outer side
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

  !  Outer strike point radius - normalized to ITER

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
  !+ad_args  rtot   : input real : default radius to the outer TF coil leg (m)
  !+ad_args  tfno   : input real(!) : number of TF coils
  !+ad_args  ripple : output real : ripple at plasma edge (%)
  !+ad_args  rtotl  : output real : required minimum radius to the centre
  !+ad_argc                         of the outer TF coil leg (m)
  !+ad_desc  Subroutine to calculate TFC ripple and outer TFC leg radius.
  !+ad_desc  Input the max. ripple and default outer leg location and the
  !+ad_desc  routine checks to see if the ripple is OK. If not it moves
  !+ad_desc  the outer leg appropriately.
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
  !+ad_call  constants
  !+ad_call  current_drive_variables
  !+ad_call  physics_variables
  !+ad_call  tfcoil_variables
  !+ad_call  build.h90
  !+ad_hist  27/07/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use current_drive_variables
  use physics_variables
  use tfcoil_variables

  implicit none

  include 'build.h90'

  !  Arguments

  !  Local variables

  real(kind(1.0D0)) :: rtan,tfoll,tfolw,rl1,ang1,ang2,ang3, &
       ps1,rl2,ang4,rl3,ang5,ang6,ang7,ang8,ps2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Beam tangency radius (m)

  rtan = frbeam * rmajor

  !  Assume the outer TF leg has a width / depth ratio of 1 / 2
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
