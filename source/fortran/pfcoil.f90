! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pfcoil_module
  !! Module containing PF coil routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! author: R Kemp, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of the PF coil systems for a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   use resistive_materials, only: volume_fractions, supercon_strand
   use pfcoil_variables, only: nfixmx, ngrpmx, nclsmx, ngc2
   implicit none

   public

   !  Local variables
   
   integer :: nef,nfxf
   real(dp) :: ricpf, ssq0, sig_axial, sig_hoop
   real(dp) :: axial_force
   ! Private module variable arrays have variable dimensions; can't be wrapped
   ! with f2py if made public
   ! #TODO Temporarily hardcode dimensions in order to make public and wrap
   ! real(dp), dimension(nfixmx), private :: rfxf,zfxf,cfxf,xind
   ! real(dp), dimension(ngrpmx,nclsmx), private :: rcls,zcls
   ! real(dp), dimension(ngrpmx), private :: ccls,ccl0
   ! real(dp), dimension(ngc2), private :: bpf2
   ! real(dp), dimension(ngc2,3), private :: vsdum
   real(dp), dimension(64) :: rfxf,zfxf,cfxf,xind
   real(dp), dimension(10,2) :: rcls,zcls
   real(dp), dimension(10) :: ccls,ccl0
   real(dp), dimension(22) :: bpf2
   real(dp), dimension(22,3) :: vsdum
 
   ! pfcoil subroutine var requiring re-initialisation before each new run
   logical :: first_call
   ! outpf subroutine var requiring re-initialisation before each new run
   logical :: CSlimit

   type(volume_fractions), private :: conductorpf
   type(supercon_strand), private ::croco_strand

 contains

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine init_pfcoil_module
     !! Initialise module variables
     implicit none

     first_call = .true.
     CSlimit = .false.
     nef = 0
     nfxf = 0
     ricpf = 0.0D0
     ssq0 = 0.0D0
     sig_axial = 0.0D0
     sig_hoop = 0D0
     axial_force = 0D0
     rfxf = 0.0D0
     zfxf = 0.0D0
     cfxf = 0.0D0
     xind = 0.0D0
     rcls = 0.0D0
     zcls = 0.0D0
     ccls = 0.0D0
     ccl0 = 0.0D0
     bpf2 = 0.0D0
     vsdum = 0.0D0
   end subroutine init_pfcoil_module

   subroutine bfield(nc, rc, zc, cc, xc, rp, zp, br, bz, psi, nciszero)
      ! #TODO bfield() is called very frequently (~15k times per solver iteration);
      ! converting this to Python results in a ~30x slowdown of the code! This will
      ! be left in Fortran for the time being, but in time should be converted to Python
      ! alongside a tool such as numba to ensure performance is maintained.

     !! Calculate the field at a point due to currents in a number
     !! of circular poloidal conductor loops.
     !! author: P J Knight, CCFE, Culham Science Centre
     !! author: D Strickler, ORNL
     !! author: J Galambos, ORNL
     !! nc : input integer : number of loops
     !! rc(nc) : input real array : R coordinates of loops (m)
     !! zc(nc) : input real array : Z coordinates of loops (m)
     !! cc(nc) : input real array : Currents in loops (A)
     !! xc(nc) : output real array : Mutual inductances (H)
     !! rp, zp : input real : coordinates of point of interest (m)
     !! br : output real : radial field component at (rp,zp) (T)
     !! bz : output real : vertical field component at (rp,zp) (T)
     !! psi : output real : poloidal flux at (rp,zp) (Wb)
     !! This routine calculates the magnetic field components and
     !! the poloidal flux at an (R,Z) point, given the locations
     !! and currents of a set of conductor loops.
     !! <P>The mutual inductances between the loops and a poloidal
     !! filament at the (R,Z) point of interest is also found.
     !! None
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       use constants, only: twopi, rmu0
     implicit none

     !  Arguments

     integer, intent(in) :: nc
     real(dp), intent(in) :: rp, zp
     real(dp), dimension(nc), intent(in) :: rc, zc, cc
     logical, optional :: nciszero
     real(dp), dimension(nc), intent(out) :: xc
     real(dp), intent(out) :: br, bz, psi

     !  Local variables

     integer :: i
     real(dp) a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4
     real(dp) :: zs,dr,d,s,t,a,xk,xe,dz,sd,brx,bzx

     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     !  Elliptic integral coefficients

     a0 = 1.38629436112D0
     a1 = 0.09666344259D0
     a2 = 0.03590092383D0
     a3 = 0.03742563713D0
     a4 = 0.01451196212D0
     b0 = 0.5D0
     b1 = 0.12498593597D0
     b2 = 0.06880248576D0
     b3 = 0.03328355346D0
     b4 = 0.00441787012D0
     c1 = 0.44325141463D0
     c2 = 0.06260601220D0
     c3 = 0.04757383546D0
     c4 = 0.01736506451D0
     d1 = 0.24998368310D0
     d2 = 0.09200180037D0
     d3 = 0.04069697526D0
     d4 = 0.00526449639D0

     br  = 0.0D0
     bz  = 0.0D0
     psi = 0.0D0
 
      if (nciszero .eqv. .true.) then
         return
      endif

     do i = 1,nc
        d = (rp + rc(i))**2 + (zp - zc(i))**2
        s = 4.0D0*rp*rc(i)/d

        t = 1.0D0 - s
        a = log(1.0D0/t)

        dz = zp - zc(i)
        zs = dz**2
        dr = rp - rc(i)
        sd = sqrt(d)

        !  Evaluation of elliptic integrals

        xk = a0 + t*(a1 + t*(a2 + t*(a3 + a4*t))) &
             + a*(b0 + t*(b1 + t*(b2 + t*(b3 + b4*t))))
        xe = 1.0D0 + t*(c1 + t*(c2 + t*(c3 + c4*t))) &
             + a*t*(d1 + t*(d2 + t*(d3 + d4*t)))

        !  Mutual inductances

        xc(i) = 0.5D0*rmu0*sd*((2.0D0 - s)*xk - 2.0D0*xe)

        !  Radial, vertical fields

        brx = rmu0*cc(i)*dz/(twopi*rp*sd)*(- xk + &
             (rc(i)**2 + rp**2 + zs)/(dr**2 + zs)*xe)
        bzx = rmu0*cc(i)/(twopi*sd)*(xk + &
             (rc(i)**2 - rp**2 - zs)/(dr**2 + zs)*xe)

        !  Sum fields, flux

        br = br + brx
        bz = bz + bzx
        psi = psi + xc(i)*cc(i)

     end do

   end subroutine bfield
 end module pfcoil_module
