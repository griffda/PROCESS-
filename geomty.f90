!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine geomty

  !+ad_name  geomty
  !+ad_summ  Plasma geometry parameters
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  surfa
  !+ad_cont  xparam
  !+ad_cont  xsect0
  !+ad_cont  xsecta
  !+ad_cont  xsurf
  !+ad_cont  xvol
  !+ad_args  nout : input integer : Fortran output unit identifier
  !+ad_args  iprint : input integer : Switch to write output to file (1=yes)
  !+ad_desc  This subroutine calculates the plasma geometry parameters.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  physics_variables
  !+ad_call  build.h90
  !+ad_call  rfp.h90
  !+ad_call  fvol
  !+ad_call  perim
  !+ad_call  surfa
  !+ad_call  xparam
  !+ad_call  xsect0
  !+ad_call  xsecta
  !+ad_call  xsurf
  !+ad_call  xvol
  !+ad_hist  18/01/99 PJK New version incorporating upgraded coding and
  !+ad_hisc               improved algorithms for double-null plasmas
  !+ad_hist  14/11/11 PJK Initial F90 version
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK14, pp.41-43
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use physics_variables

  implicit none

  include 'build.h90'
  include 'rfp.h90'

  !  Arguments

  !  Local variables

  real(kind(1.0D0)) :: sa,sf2,so,xsi,xso,thetai,thetao,xi,xo

  !  External functions

  real(kind(1.0D0)), external :: perim

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialize some shape quantities

  rminor = rmajor / aspect
  eps = 1.0D0 / aspect

  !  Calculate shaping terms, rather than use input values (TART)

  if (ishape == 1) then
     kappa = 2.05D0 * (1.0D0 + 0.44D0 * eps**2.1D0)
     triang = 0.53D0 * (1.0D0 + 0.77D0 * eps**3)
     qlim = 3.0D0 * (1.0D0 + 2.6D0*eps**2.8D0)
  end if

  !  Rough estimate of 95% values

  kappa95 = (kappa - 0.04D0) / 1.10D0
  triang95 = triang / 1.50D0

  !  Scrape-off layer thicknesses

  if (iscrp == 0) then
     scraplo = 0.1D0 * rminor
     scrapli = 0.1D0 * rminor
  end if

  !  RFP calculations (circular plasma)

  if (irfp == 1) then

     !  Plasma poloidal perimeter

     sf2 = 2.0D0*pi*rminor
     sf = 1.0D0

     !  Plasma volume (kappa should be 1.0)

     vol = cvol * 2.0D0 * pi**2 * rmajor * rminor**2 * kappa

     !  Plasma surface area (very bad approx for outer area...)

     sarea = 4.0D0 * pi**2 * rmajor * rminor * 0.5D0*(kappa+1.0D0)
     sareao = 0.5D0 * sarea

     !  Plasma cross-sectional area

     xarea = pi * rminor * rminor

     return
  end if

  !  Double null configuration

  if (igeom == 0) then

     !  Use original methods

     sf2 = perim(rminor,kappa,triang)
     sf = sf2 / (2.0D0*pi*rminor)

     vol = cvol * fvol(rmajor,rminor,kappa,triang)

     call surfa(rminor,rmajor,kappa,triang,sa,so)
     sareao = so
     sarea = sa

     xarea = xsect0(rminor,kappa,triang)

  else

     !  Find parameters of arcs describing plasma surfaces

     call xparam(rminor,kappa,triang,xi,thetai,xo,thetao)

     !  Poloidal perimeter

     sf2 = 2.0D0 * ( xo*thetao + xi*thetai )
     sf = sf2 / (2.0D0*pi*rminor)

     !  Volume

     vol = cvol * xvol(rmajor,rminor,xi,thetai,xo,thetao)

     !  Surface area

     call xsurf(rmajor,rminor,xi,thetai,xo,thetao,xsi,xso)
     sareao = xso
     sarea = xsi+xso

     !  Cross-sectional area

     xarea = xsecta(xi,thetai,xo,thetao)

  end if

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine surfa(a,r,k,d,sa,so)

    !+ad_name  surfa
    !+ad_summ  Plasma surface area calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a      : input real :  plasma minor radius (m)
    !+ad_args  r      : input real :  plasma major radius (m)
    !+ad_args  k      : input real :  plasma separatrix elongation
    !+ad_args  d      : input real :  plasma separatrix triangularity
    !+ad_args  sa     : output real : plasma total surface area (m2)
    !+ad_args  so     : output real : plasma outer surface area (m2)
    !+ad_desc  This function finds the plasma surface area, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_desc  It was the original method in PROCESS.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  19/01/99 PJK Initial upgraded version
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: a,r,k,d
    real(kind(1.0D0)), intent(out) :: sa,so

    !  Local variables

    real(kind(1.0D0)) :: b,radci,radco,si,thti,thto

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Outer side

    radco = a * (1.0D0 + (k**2 + d**2 - 1.0D0)/(2.0D0 * (1.0D0 + d)))
    b = k * a
    thto = asin(b/radco)
    so = 4.0D0 * pi * radco * ( (r + a - radco)*thto + b)

    !  Inner side

    radci = a * (1.0D0 + (k**2 + d**2 - 1.0D0)/(2.0D0 * (1.0D0 - d)))
    thti = asin(b/radci)
    si = 4.0D0 * pi * radci * ( (r - a + radci)*thti - b)

    sa = so + si

  end subroutine surfa

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function xsect0(a,kap,tri)

    !+ad_name  xsect0
    !+ad_summ  Plasma cross-sectional area calculation
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a      : input real :  plasma minor radius (m)
    !+ad_args  kap    : input real :  plasma separatrix elongation
    !+ad_args  tri    : input real :  plasma separatrix triangularity
    !+ad_desc  This function finds the plasma cross-sectional area, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_desc  The method for finding the arc radii and angles are copied from
    !+ad_desc  routine <A HREF="perim.html">PERIM</A>, and are thought to be
    !+ad_desc  by Peng.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/06/98 PJK Initial version (original version of xsecta)
    !+ad_hist  16/07/01 PJK Correction to sign of TRI in DENOMI and XLI
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.41
    !+ad_docs  F/PL/PJK/PROCESS/CODE/047
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: xsect0

    !  Arguments

    real(kind(1.0D0)), intent(in) :: a,kap,tri

    !  Local variables

    real(kind(1.0D0)) :: denomi,denomo,thetai,thetao,xli,xlo
    real(kind(1.0D0)) :: cti,sti,cto,sto

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Find radius and half-angle of inner arc

    denomi = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0-tri) ) + tri
    thetai = atan(kap/denomi)
    xli = a * (denomi + 1.0D0 - tri)

    cti = cos(thetai)
    sti = sin(thetai)

    !  Find radius and half-angle of outer arc

    denomo = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0+tri) ) - tri
    thetao = atan(kap/denomo)
    xlo = a * (denomo + 1.0D0 + tri )

    cto = cos(thetao)
    sto = sin(thetao)

    !  Find cross-sectional area

    xsect0 = xlo**2 * (thetao - cto*sto) + &
         xli**2 * (thetai - cti*sti)

  end function xsect0

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function fvol(r,a,kap,tri)

    !+ad_name  fvol
    !+ad_summ  Plasma volume calculation
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  r      : input real :  plasma major radius (m)
    !+ad_args  a      : input real :  plasma minor radius (m)
    !+ad_args  kap    : input real :  plasma separatrix elongation
    !+ad_args  tri    : input real :  plasma separatrix triangularity
    !+ad_desc  This function finds the plasma volume, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  18/01/99 PJK Initial upgraded version
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.41
    !+ad_docs  F/PL/PJK/PROCESS/CODE/047
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: fvol

    !  Arguments

    real(kind(1.0D0)), intent(in) :: r,a,kap,tri

    !  Local variables

    real(kind(1.0D0)) :: c1,c2,rc2,rci,vin,vout,zn

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    zn = kap * a

    rci = ( (r + a)**2 - (r - tri*a)**2 - zn**2) / &
         (2.0D0 * (1.0D0+tri) * a)
    c1 = r + a - rci
    vout = -0.66666666D0 * pi * zn**3 + 2.0D0 * pi * zn * &
         (rci**2 + c1**2) + 2.0D0 * pi * rci * &
         ( zn * sqrt(c1**2 - zn**2) + c1**2 * asin(zn/c1) )

    rc2 = (-(r - a)**2 + (r - tri*a)**2 + zn**2) / &
         (2.0D0 * (1.0D0-tri) * a)
    c2 = rc2 - r + a
    vin = -0.66666D0 * pi * zn**3 + 2.0D0 * pi * zn * &
         (c2**2 + rc2**2) - 2.0D0 * pi * rc2 * &
         ( zn * sqrt(c2**2 - zn**2) + c2**2 * asin(zn/c2) )

    fvol = vout - vin

  end function fvol

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine xparam(a,kap,tri,xi,thetai,xo,thetao)

    !+ad_name  xparam
    !+ad_summ  Routine to find parameters used for calculating geometrical
    !+ad_summ  properties for double-null plasmas
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a      : input real :  plasma minor radius (m)
    !+ad_args  kap    : input real :  plasma separatrix elongation
    !+ad_args  tri    : input real :  plasma separatrix triangularity
    !+ad_args  xi     : output real : radius of arc describing inboard surface (m)
    !+ad_args  thetai : output real : half-angle of arc describing inboard surface
    !+ad_args  xo     : output real : radius of arc describing outboard surface (m)
    !+ad_args  thetao : output real : half-angle of arc describing outboard surface
    !+ad_desc  This function finds plasma geometrical parameters, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  18/01/99 PJK Initial upgraded version
    !+ad_hist  16/07/01 PJK Correction of sign of TRI in XI
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.42
    !+ad_docs  F/PL/PJK/PROCESS/CODE/047
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: a,kap,tri
    real(kind(1.0D0)), intent(out) :: xi,thetai,xo,thetao

    !  Local variables

    real(kind(1.0D0)) :: denomi,denomo,n,t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Find radius and half-angle of inner arc

    t = 1.0D0 - tri
    denomi = (kap**2 - t**2)/(2.0D0*t)
    thetai = atan(kap/denomi)
    xi = a * (denomi + 1.0D0 - tri )

    !  Find radius and half-angle of outer arc

    n = 1.0D0 + tri
    denomo = (kap**2 - n**2)/(2.0D0*n)
    thetao = atan(kap/denomo)
    xo = a * (denomo + 1.0D0 + tri )

  end subroutine xparam

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function xsecta(xi,thetai,xo,thetao)

    !+ad_name  xsecta
    !+ad_summ  Plasma cross-sectional area calculation
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  xi     : input real :  radius of arc describing inboard surface (m)
    !+ad_args  thetai : input real :  half-angle of arc describing inboard surface
    !+ad_args  xo     : input real :  radius of arc describing outboard surface (m)
    !+ad_args  thetao : input real :  half-angle of arc describing outboard surface
    !+ad_desc  This function finds the plasma cross-sectional area, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/06/98 PJK Initial version
    !+ad_hist  18/01/99 PJK Moved calculation of arc parameters into XPARAM
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.41
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: xsecta

    !  Arguments

    real(kind(1.0D0)), intent(in) :: xi,thetai,xo,thetao

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xsecta = xo**2 * (thetao - cos(thetao)*sin(thetao)) + &
         xi**2 * (thetai - cos(thetai)*sin(thetai))

  end function xsecta

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function xvol(rmajor,rminor,xi,thetai,xo,thetao)

    !+ad_name  xvol
    !+ad_summ  Plasma volume calculation
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  rminor : input real :  plasma minor radius (m)
    !+ad_args  xi     : input real :  radius of arc describing inboard surface (m)
    !+ad_args  thetai : input real :  half-angle of arc describing inboard surface
    !+ad_args  xo     : input real :  radius of arc describing outboard surface (m)
    !+ad_args  thetao : input real :  half-angle of arc describing outboard surface
    !+ad_desc  This function finds the plasma volume, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  18/01/99 PJK Initial version
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.43
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: xvol

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rmajor,rminor,xi,thetai,xo,thetao

    !  Local variables

    real(kind(1.0D0)) :: rc,third,vin,vout

    !--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

    third = 1.0D0/3.0D0

    rc = rmajor - rminor + xi
    vin = twopi * xi * &
         ( rc**2*sin(thetai) - &
         rc*xi*thetai - &
         0.5D0*rc*xi*sin(2.0D0*thetai) + &
         xi*xi*sin(thetai) - &
         third*xi*xi*(sin(thetai))**3 )

    rc = rmajor + rminor - xo
    vout = twopi * xo * &
         ( rc**2*sin(thetao) + &
         rc*xo*thetao + &
         0.5D0*rc*xo*sin(2.0D0*thetao) + &
         xo*xo*sin(thetao) - &
         third*xo*xo*(sin(thetao))**3 )

    xvol = vout - vin

  end function xvol

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine xsurf(rmajor,rminor,xi,thetai,xo,thetao,xsi,xso)

    !+ad_name  xsurf
    !+ad_summ  Plasma surface area calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  rminor : input real :  plasma minor radius (m)
    !+ad_args  xi     : input real :  radius of arc describing inboard surface (m)
    !+ad_args  thetai : input real :  half-angle of arc describing inboard surface
    !+ad_args  xo     : input real :  radius of arc describing outboard surface (m)
    !+ad_args  thetao : input real :  half-angle of arc describing outboard surface
    !+ad_args  xsi    : output real : inboard surface area (m2)
    !+ad_args  xso    : output real : outboard surface area (m2)
    !+ad_desc  This function finds the plasma surface area, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  18/01/99 PJK Initial version
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.43
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: xvol

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rmajor,rminor,xi,thetai,xo,thetao
    real(kind(1.0D0)), intent(out) :: xsi,xso

    !  Local variables

    real(kind(1.0D0)) :: fourpi,rc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    fourpi = 4.0D0 * pi

    rc = rmajor - rminor + xi
    xsi = fourpi * xi * (rc*thetai - xi*sin(thetai))

    rc = rmajor + rminor - xo
    xso = fourpi * xo * (rc*thetao + xo*sin(thetao))

  end subroutine xsurf

end subroutine geomty

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function perim(a,kap,tri)

  !+ad_name  perim
  !+ad_summ  Plasma poloidal perimeter calculation
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  a      : input real :  plasma minor radius (m)
  !+ad_args  kap    : input real :  plasma separatrix elongation
  !+ad_args  tri    : input real :  plasma separatrix triangularity
  !+ad_desc  This function finds the plasma poloidal perimeter, using the
  !+ad_desc  revolution of two intersecting arcs around the device centreline.
  !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  19/01/99 PJK Initial upgraded version
  !+ad_hist  16/07/01 PJK Correction to sign of TRI in DENOMI and XLI
  !+ad_hist  14/11/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/PL/PJK/PROCESS/CODE/047
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: perim

  !  Arguments

  real(kind(1.0D0)), intent(in) :: a,kap,tri

  !  Local variables

  real(kind(1.0D0)) :: denomi,denomo,thetai,thetao,xli,xlo

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Inner arc

  denomi = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0-tri) ) + tri
  thetai = atan(kap/denomi)
  xli = a * (denomi + 1.0D0 - tri )

  !  Outer arc

  denomo = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0+tri) ) - tri
  thetao = atan(kap/denomo)
  xlo = a * (denomo + 1.0D0 + tri )

  perim = 2.0D0 * ( xlo*thetao + xli*thetai)

end function perim
