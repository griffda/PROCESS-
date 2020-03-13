! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module plasma_geometry_module

  !! Module containing plasma geometry routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! plasma geometry (shape) for a fusion power plant.

  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  use build_variables
  use constants
  use physics_variables

  implicit none

  private
  public :: geomty, perim, xparam

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine geomty

    !! Plasma geometry parameters
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This subroutine calculates the plasma geometry parameters.

    !! J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
    !! unpublished internal Oak Ridge document
    !! F/MI/PJK/LOGBOOK14, pp.41-43
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !! H. Zohm et al, On the Physics Guidelines for a Tokamak DEMO,
    !! FTP/3-3, Proc. IAEA Fusion Energy Conference, October 2012, San Diego
    !! T. Hartmann and H. Zohm: Towards a 'Physics Design Guidelines for a
    !! DEMO Tokamak' Document, March 2012, EFDA Report
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(dp) :: sa,so,xsi,xso,thetai,thetao,xi,xo

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Initialize some shape quantities

    rminor = rmajor / aspect
    eps = 1.0D0 / aspect

    !  Calculate shaping terms, rather than use input values

    select case (ishape)

    case (0)  !  Use input kappa, triang values

       !  Rough estimate of 95% values
       !  Hartmann and Zohm suggestion for kappa95 (close to previous estimate
       !  of (kappa - 0.04) / 1.1 over a large kappa range)

       kappa95 = kappa / 1.12D0
       triang95 = triang / 1.50D0

    case (1)  !  ST scaling with aspect ratio [STAR Code]

       qlim = 3.0D0 * (1.0D0 + 2.6D0*eps**2.8D0)

       kappa = 2.05D0 * (1.0D0 + 0.44D0 * eps**2.1D0)
       triang = 0.53D0 * (1.0D0 + 0.77D0 * eps**3)

       kappa95 = kappa / 1.12D0  !  Hartmann and Zohm
       triang95 = triang / 1.50D0

    case (2)  !  Zohm et al. ITER scaling for elongation, input triang

       kappa = fkzohm * min(2.0D0, 1.5D0 + 0.5D0/(aspect-1.0D0))

       kappa95 = kappa / 1.12D0  !  Hartmann and Zohm
       triang95 = triang / 1.50D0

    case (3)  !  Zohm et al. ITER scaling for elongation, input triang95

       kappa = fkzohm * min(2.0D0, 1.5D0 + 0.5D0/(aspect-1.0D0))
       triang = 1.5D0 * triang95

       kappa95 = kappa / 1.12D0  !  Hartmann and Zohm

    case (4)  !  Use input kappa95, triang95 values

       kappa = 1.12D0 * kappa95  !  Hartmann and Zohm
       triang = 1.5D0 * triang95

    end select

    !  Scrape-off layer thicknesses
    if (iscrp == 0) then
       scraplo = 0.1D0 * rminor
       scrapli = 0.1D0 * rminor
    end if

    !  Double null configuration (??? MDK)

    if (igeom == 0) then     !  Use original methods

       pperim = perim(rminor,kappa,triang)
       sf = pperim / (2.0D0*pi*rminor)

       vol = cvol * fvol(rmajor,rminor,kappa,triang)
       
       call surfa(rminor,rmajor,kappa,triang,sa,so)  !  [STAR Code]    
       sareao = so
       sarea = sa

       xarea = xsect0(rminor,kappa,triang)

    else                      !  Find parameters of arcs describing plasma surfaces

       call xparam(rminor,kappa,triang,xi,thetai,xo,thetao)

       !  Poloidal perimeter
       pperim = 2.0D0 * ( xo*thetao + xi*thetai )
       sf = pperim / (2.0D0*pi*rminor)

       !  Volume
       vol = cvol * xvol(rmajor,rminor,xi,thetai,xo,thetao)

       !  Surface area
       call xsurf(rmajor,rminor,xi,thetai,xo,thetao,xsi,xso)
       sareao = xso
       sarea = xsi+xso

       !  Cross-sectional area
       xarea = xsecta(xi,thetai,xo,thetao)

    end if
    ! write(*,*)'Subroutine geomty:    Plasma volume  ', vol

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine surfa(a,r,k,d,sa,so)

      !! Plasma surface area calculation
      !! author: P J Knight, CCFE, Culham Science Centre
      !! a      : input real :  plasma minor radius (m)
      !! r      : input real :  plasma major radius (m)
      !! k      : input real :  plasma separatrix elongation
      !! d      : input real :  plasma separatrix triangularity
      !! sa     : output real : plasma total surface area (m2)
      !! so     : output real : plasma outboard surface area (m2)
      !! This function finds the plasma surface area, using the
      !! revolution of two intersecting arcs around the device centreline.
      !! This calculation is appropriate for plasmas with a separatrix.
      !! It was the original method in PROCESS.
      !! J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
      !! unpublished internal Oak Ridge document
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(dp), intent(in) :: a,r,k,d
      real(dp), intent(out) :: sa,so

      !  Local variables

      real(dp) :: b,radci,radco,si,thti,thto

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Outboard side

      radco = a * (1.0D0 + (k**2 + d**2 - 1.0D0)/(2.0D0 * (1.0D0 + d)))
      b = k * a
      thto = asin(b/radco)
      so = 4.0D0 * pi * radco * ( (r + a - radco)*thto + b)

      !  Inboard side

      radci = a * (1.0D0 + (k**2 + d**2 - 1.0D0)/(2.0D0 * (1.0D0 - d)))
      thti = asin(b/radci)
      si = 4.0D0 * pi * radci * ( (r - a + radci)*thti - b)

      sa = so + si

    end subroutine surfa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function xsect0(a,kap,tri)

      !! Plasma cross-sectional area calculation
      !! author: P J Knight, CCFE, Culham Science Centre
      !! a      : input real :  plasma minor radius (m)
      !! kap    : input real :  plasma separatrix elongation
      !! tri    : input real :  plasma separatrix triangularity
      !! This function finds the plasma cross-sectional area, using the
      !! revolution of two intersecting arcs around the device centreline.
      !! This calculation is appropriate for plasmas with a separatrix.
      !! The method for finding the arc radii and angles are copied from
      !! routine <A HREF="perim.html">PERIM</A>, and are thought to be
      !! by Peng.
      !! F/MI/PJK/LOGBOOK14, p.41
      !! F/PL/PJK/PROCESS/CODE/047
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(dp) :: xsect0

      !  Arguments

      real(dp), intent(in) :: a,kap,tri

      !  Local variables

      real(dp) :: denomi,denomo,thetai,thetao,xli,xlo
      real(dp) :: cti,sti,cto,sto

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Find radius and half-angle of inboard arc

      denomi = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0-tri) ) + tri
      thetai = atan(kap/denomi)
      xli = a * (denomi + 1.0D0 - tri)

      cti = cos(thetai)
      sti = sin(thetai)

      !  Find radius and half-angle of outboard arc

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

      !! Plasma volume calculation
      !! author: P J Knight, CCFE, Culham Science Centre
      !! r      : input real :  plasma major radius (m)
      !! a      : input real :  plasma minor radius (m)
      !! kap    : input real :  plasma separatrix elongation
      !! tri    : input real :  plasma separatrix triangularity
      !! This function finds the plasma volume, using the
      !! revolution of two intersecting arcs around the device centreline.
      !! This calculation is appropriate for plasmas with a separatrix.
      !! F/MI/PJK/LOGBOOK14, p.41
      !! F/PL/PJK/PROCESS/CODE/047
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(dp) :: fvol

      !  Arguments

      real(dp), intent(in) :: r,a,kap,tri

      !  Local variables

      real(dp) :: c1,c2,rc2,rc1,vin,vout,zn

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      zn = kap * a

      c1 = ( (r + a)**2 - (r - tri*a)**2 - zn**2) / &
           (2.0D0 * (1.0D0+tri) * a)
      rc1 = r + a - c1
      vout = -0.66666666D0 * pi * zn**3 + 2.0D0 * pi * zn * &
           (c1**2 + rc1**2) + 2.0D0 * pi * c1 * &
           ( zn * sqrt(rc1**2 - zn**2) + rc1**2 * asin(zn/rc1) )

      c2 = (-(r - a)**2 + (r - tri*a)**2 + zn**2) / &
           (2.0D0 * (1.0D0-tri) * a)
      rc2 = c2 - r + a
      vin = -0.66666D0 * pi * zn**3 + 2.0D0 * pi * zn * &
           (rc2**2 + c2**2) - 2.0D0 * pi * c2 * &
           ( zn * sqrt(rc2**2 - zn**2) + rc2**2 * asin(zn/rc2) )

      fvol = vout - vin

    end function fvol

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function xsecta(xi,thetai,xo,thetao)

      !! Plasma cross-sectional area calculation
      !! author: P J Knight, CCFE, Culham Science Centre
      !! xi     : input real :  radius of arc describing inboard surface (m)
      !! thetai : input real :  half-angle of arc describing inboard surface
      !! xo     : input real :  radius of arc describing outboard surface (m)
      !! thetao : input real :  half-angle of arc describing outboard surface
      !! This function finds the plasma cross-sectional area, using the
      !! revolution of two intersecting arcs around the device centreline.
      !! This calculation is appropriate for plasmas with a separatrix.
      !! F/MI/PJK/LOGBOOK14, p.41
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(dp) :: xsecta

      !  Arguments

      real(dp), intent(in) :: xi,thetai,xo,thetao

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      xsecta = xo**2 * (thetao - cos(thetao)*sin(thetao)) + &
           xi**2 * (thetai - cos(thetai)*sin(thetai))

    end function xsecta

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function xvol(rmajor,rminor,xi,thetai,xo,thetao)

      !! Plasma volume calculation
      !! author: P J Knight, CCFE, Culham Science Centre
      !! rmajor : input real :  plasma major radius (m)
      !! rminor : input real :  plasma minor radius (m)
      !! xi     : input real :  radius of arc describing inboard surface (m)
      !! thetai : input real :  half-angle of arc describing inboard surface
      !! xo     : input real :  radius of arc describing outboard surface (m)
      !! thetao : input real :  half-angle of arc describing outboard surface
      !! This function finds the plasma volume, using the
      !! revolution of two intersecting arcs around the device centreline.
      !! This calculation is appropriate for plasmas with a separatrix.
      !! F/MI/PJK/LOGBOOK14, p.43
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(dp) :: xvol

      !  Arguments

      real(dp), intent(in) :: rmajor,rminor,xi,thetai,xo,thetao

      !  Local variables

      real(dp) :: rc,third,vin,vout

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

      !! Plasma surface area calculation
      !! author: P J Knight, CCFE, Culham Science Centre
      !! rmajor : input real :  plasma major radius (m)
      !! rminor : input real :  plasma minor radius (m)
      !! xi     : input real :  radius of arc describing inboard surface (m)
      !! thetai : input real :  half-angle of arc describing inboard surface
      !! xo     : input real :  radius of arc describing outboard surface (m)
      !! thetao : input real :  half-angle of arc describing outboard surface
      !! xsi    : output real : inboard surface area (m2)
      !! xso    : output real : outboard surface area (m2)
      !! This function finds the plasma surface area, using the
      !! revolution of two intersecting arcs around the device centreline.
      !! This calculation is appropriate for plasmas with a separatrix.
      !! F/MI/PJK/LOGBOOK14, p.43
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(dp), intent(in) :: rmajor,rminor,xi,thetai,xo,thetao
      real(dp), intent(out) :: xsi,xso

      !  Local variables

      real(dp) :: fourpi,rc

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

    !! Plasma poloidal perimeter calculation
    !! author: P J Knight, CCFE, Culham Science Centre
    !! a      : input real :  plasma minor radius (m)
    !! kap    : input real :  plasma separatrix elongation
    !! tri    : input real :  plasma separatrix triangularity
    !! This function finds the plasma poloidal perimeter, using the
    !! revolution of two intersecting arcs around the device centreline.
    !! This calculation is appropriate for plasmas with a separatrix.
    !! F/PL/PJK/PROCESS/CODE/047
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(dp) :: perim

    !  Arguments

    real(dp), intent(in) :: a,kap,tri

    !  Local variables

    real(dp) :: denomi,denomo,thetai,thetao,xli,xlo

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Inboard arc

    denomi = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0-tri) ) + tri
    thetai = atan(kap/denomi)
    xli = a * (denomi + 1.0D0 - tri )

    !  Outboard arc

    denomo = (tri**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0+tri) ) - tri
    thetao = atan(kap/denomo)
    xlo = a * (denomo + 1.0D0 + tri )

    perim = 2.0D0 * ( xlo*thetao + xli*thetai)

  end function perim

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine xparam(a,kap,tri,xi,thetai,xo,thetao)

    !! Routine to find parameters used for calculating geometrical
    !! properties for double-null plasmas
    !! author: P J Knight, CCFE, Culham Science Centre
    !! a      : input real :  plasma minor radius (m)
    !! kap    : input real :  plasma separatrix elongation
    !! tri    : input real :  plasma separatrix triangularity
    !! xi     : output real : radius of arc describing inboard surface (m)
    !! thetai : output real : half-angle of arc describing inboard surface
    !! xo     : output real : radius of arc describing outboard surface (m)
    !! thetao : output real : half-angle of arc describing outboard surface
    !! This function finds plasma geometrical parameters, using the
    !! revolution of two intersecting arcs around the device centreline.
    !! This calculation is appropriate for plasmas with a separatrix.
    !! F/MI/PJK/LOGBOOK14, p.42
    !! F/PL/PJK/PROCESS/CODE/047
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(dp), intent(in) :: a,kap,tri
    real(dp), intent(out) :: xi,thetai,xo,thetao

    !  Local variables

    real(dp) :: denomi,denomo,n,t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Find radius and half-angle of inboard arc

    t = 1.0D0 - tri
    denomi = (kap**2 - t**2)/(2.0D0*t)
    thetai = atan(kap/denomi)
    xi = a * (denomi + 1.0D0 - tri )

    !  Find radius and half-angle of outboard arc

    n = 1.0D0 + tri
    denomo = (kap**2 - n**2)/(2.0D0*n)
    thetao = atan(kap/denomo)
    xo = a * (denomo + 1.0D0 + tri )

  end subroutine xparam


end module plasma_geometry_module
