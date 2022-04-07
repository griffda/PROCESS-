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
 
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 
   subroutine efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts, &
        rpts,zpts,brin,bzin,nfix,rfix,zfix,cfix,ngrp,ncls,rcls,zcls, &
        alfa,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat,sigma,work2,ssq,ccls)
 
     !! Calculates field coil currents
     !! author: P J Knight, CCFE, Culham Science Centre
     !! author: D Strickler, ORNL
     !! author: J Galambos, ORNL
     !! author: P C Shipe, ORNL
     !! ngrpmx : input integer : maximum number of PF coil groups
     !! nclsmx : input integer : maximum number of coils in one group
     !! nptsmx : input integer : maximum number of points across the
     !! plasma midplane at which the magnetic
     !! field is fixed
     !! nfixmx : input integer : maximum number of fixed current coils
     !! lrow1 : input integer : row length of arrays bfix, bvec, gmat,
     !! umat, vmat; should be >= (2*nptsmx + ngrpmx)
     !! lcol1 : input integer : column length of arrays gmat, umat, vmat;
     !! should be >= ngrpmx
     !! npts : input integer : number of data points at which field is
     !! to be fixed; should be <= nptsmx
     !! rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
     !! brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
     !! data points (T)
     !! nfix : input integer : number of coils with fixed currents, <= nfixmx
     !! rfix(nfixmx),zfix(nfixmx) : input real arrays : coordinates of coils
     !! with fixed currents (m)
     !! cfix(nfixmx) : input real array : Fixed currents (A)
     !! ngrp : input integer : number of coil groups, where all coils in a
     !! group have the same current, <= ngrpmx
     !! ncls(ngrpmx+2) : input integer array : number of coils in each group,
     !! each value <= nclsmx
     !! rcls(ngrpmx,nclsmx),zcls(ngrpmx,nclsmx) : input real arrays : coords
     !! R(i,j), Z(i,j) of coil j in group i (m)
     !! alfa : input real : smoothing parameter (0 = no smoothing,
     !! 1.0D-9 = large smoothing)
     !! bfix(lrow1) : input/output real array : work array
     !! gmat(lrow1,lcol1) : input/output real array : work array
     !! bvec(lrow1) : input/output real array : work array
     !! rc(nclsmx) : input/output real array : work array
     !! zc(nclsmx) : input/output real array : work array
     !! cc(nclsmx) : input/output real array : work array
     !! xc(nclsmx) : input/output real array : work array
     !! umat(lrow1,lcol1) : input/output real array : work array
     !! vmat(lrow1,lcol1) : input/output real array : work array
     !! sigma(ngrpmx) : input/output real array : work array
     !! work2(ngrpmx) : input/output real array : work array
     !! ssq : output real : sum of squares of elements of residual vector
     !! ccls(ngrpmx) : output real array : solution vector of coil currents
     !! in each group (A)
     !! This routine calculates the currents required in a group
     !! of ring coils to produce a fixed field at prescribed
     !! locations. Additional ring coils with fixed currents are
     !! also allowed.
     !! None
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
     implicit none
 
     !  Arguments
 
     integer, intent(in) :: ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1
     integer, intent(in) :: ngrp,npts,nfix
     integer, dimension(ngrpmx+2), intent(in) :: ncls
 
     real(dp), intent(in) :: alfa
     real(dp), dimension(ngrpmx,nclsmx), intent(in) :: rcls,zcls
     real(dp), dimension(nptsmx), intent(in) :: brin,bzin,rpts,zpts
     real(dp), dimension(nfixmx), intent(in) :: rfix,zfix,cfix
 
     real(dp), dimension(lrow1), intent(inout) :: bfix,bvec
     real(dp), dimension(lrow1,lcol1), intent(inout) :: gmat,umat,vmat
     real(dp), dimension(ngrpmx), intent(inout) :: sigma,work2
     real(dp), dimension(nclsmx), intent(inout) :: rc,zc,cc,xc
 
     real(dp), intent(out) :: ssq
     real(dp), dimension(ngrpmx), intent(out) :: ccls
 
     !  Local variables
 
     real(dp) :: brssq, brnrm, bzssq, bznrm
     integer :: nrws
 
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
     !  Calculate field from the fixed current loops
 
     call fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix,zfix,cfix,bfix)
 
     !  Set up matrix equation
 
     call mtrx(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts,brin, &
          bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec,rc,zc,cc,xc)
 
     !  Solve matrix equation
 
     call solv(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat,vmat, &
          sigma,work2)
 
     !  Calculate the norm of the residual vectors
 
     call rsid(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix,ngrp,ccls, &
          brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)
 
   end subroutine efc
 
   subroutine fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix, &
      zfix,cfix,bfix)

      !! Calculates the field from the fixed current loops
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: D Strickler, ORNL
      !! author: J Galambos, ORNL
      !! nptsmx : input integer : maximum number of points across the
      !! plasma midplane at which the magnetic
      !! field is fixed
      !! nfixmx : input integer : maximum number of fixed current coils
      !! lrow1 : input integer : row length of array bfix; should be >= nptsmx
      !! npts : input integer : number of data points at which field is
      !! to be fixed; should be <= nptsmx
      !! rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
      !! nfix : input integer : number of coils with fixed currents, <= nfixmx
      !! rfix(nfixmx),zfix(nfixmx) : input real arrays : coordinates of coils
      !! with fixed currents (m)
      !! cfix(nfixmx) : input real array : Fixed currents (A)
      !! bfix(lrow1) : output real array : Fields at data points (T)
      !! This routine calculates the fields at the points specified by
      !! (rpts,zpts) from the set of coils with fixed currents.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: nptsmx, nfixmx, lrow1, npts, nfix
      real(dp), dimension(nptsmx), intent(in) :: rpts, zpts
      real(dp), dimension(nfixmx), intent(in) :: rfix, zfix, cfix
      real(dp), dimension(lrow1), intent(out) :: bfix

      !  Local variables

      integer :: i
      real(dp) brw,bzw,psw
      real(dp), dimension(nfixmx) :: work1

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do i = 1,npts
         bfix(i) = 0.0D0
         bfix(npts + i) = 0.0D0
      end do

      if (nfix <= 0) return

      do i = 1,npts
         call bfield(nfix,rfix,zfix,cfix,work1,rpts(i),zpts(i),brw,bzw,psw)
         bfix(i) = brw
         bfix(npts + i) = bzw
      end do
   end subroutine fixb

   subroutine mtrx(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts, &
      brin,bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec, &
      rc,zc,cc,xc)

      !! Set up the matrix equation to calculate the currents
      !! in a group of ring coils
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: D Strickler, ORNL
      !! author: J Galambos, ORNL
      !! nptsmx : input integer : maximum number of points across the
      !! plasma midplane at which the magnetic
      !! field is fixed
      !! ngrpmx : input integer : maximum number of PF coil groups
      !! nclsmx : input integer : maximum number of coils in one group
      !! lrow1 : input integer : row length of arrays bfix, bvec, gmat,
      !! umat, vmat; should be >= (2*nptsmx + ngrpmx)
      !! lcol1 : input integer : column length of arrays gmat, umat, vmat;
      !! should be >= ngrpmx
      !! npts : input integer : number of data points at which field is
      !! to be fixed; should be <= nptsmx
      !! rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
      !! lrow1 : input integer : row length of array bfix; should be >= nptsmx
      !! npts : input integer : number of data points at which field is
      !! to be fixed; should be <= nptsmx
      !! rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
      !! brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
      !! data points (T)
      !! ngrp : input integer : number of coil groups, where all coils in a
      !! group have the same current, <= ngrpmx
      !! ncls(ngrpmx+2) : input integer array : number of coils in each group,
      !! each value <= nclsmx
      !! rcls(ngrpmx,nclsmx),zcls(ngrpmx,nclsmx) : input real arrays : coords
      !! R(i,j), Z(i,j) of coil j in group i (m)
      !! alfa : input real : smoothing parameter (0 = no smoothing,
      !! 1.0D-9 = large smoothing)
      !! nrws : output integer : actual number of rows to use
      !! bfix(lrow1) : input real array : Fields at data points (T)
      !! gmat(lrow1,lcol1) : output real array : work array
      !! bvec(lrow1) : output real array : work array
      !! rc(nclsmx) : output real array : Coordinates of conductor loops (m)
      !! zc(nclsmx) : output real array : Coordinates of conductor loops (m)
      !! cc(nclsmx) : output real array : Currents in conductor loops (A)
      !! xc(nclsmx) : output real array : Mutual inductances (H)
      !! This routine sets up the matrix equation for calculating the
      !! currents in a group of ring coils.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: nptsmx, ngrpmx, nclsmx, lrow1, lcol1, npts, ngrp
      integer, dimension(ngrpmx+2), intent(in) :: ncls
      real(dp), intent(in) :: alfa
      real(dp), dimension(ngrpmx,nclsmx), intent(in) :: rcls, zcls
      real(dp), dimension(nptsmx), intent(in) :: brin, bzin, rpts, zpts
      real(dp), dimension(lrow1), intent(in) :: bfix

      integer, intent(out) :: nrws
      real(dp), dimension(nclsmx), intent(out) :: rc, zc, cc, xc
      real(dp), dimension(lrow1), intent(out) :: bvec
      real(dp), dimension(lrow1,lcol1), intent(out) :: gmat

      !  Local variables

      integer :: i, j, k, nc
      real(dp) brw, bzw, psw

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do i = 1,npts
         bvec(i) = brin(i) - bfix(i)
         bvec(i+npts) = bzin(i) - bfix(i + npts)
         do j = 1,ngrp
            nc = ncls(j)
            do k = 1,nc
               rc(k) = rcls(j,k)
               zc(k) = zcls(j,k)
               cc(k) = 1.0D0
            end do
            call bfield(nc,rc,zc,cc,xc,rpts(i),zpts(i),brw,bzw,psw)
            gmat(i,j) = brw
            gmat(i+npts,j) = bzw
         end do
      end do

      !  Add constraint equations

      nrws = 2 * npts

      do j = 1,ngrp
         bvec(nrws + j) = 0.0D0
         do i = 1,ngrp
            gmat(nrws + j,i) = 0.0D0
         end do
         nc = ncls(j)
         gmat(nrws + j,j) = nc * alfa
      end do

      nrws = 2*npts + ngrp

   end subroutine mtrx

   subroutine solv(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat, &
      vmat,sigma,work2)

      !! Solve a matrix using singular value decomposition
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: D Strickler, ORNL
      !! author: J Galambos, ORNL
      !! author: P C Shipe, ORNL
      !! ngrpmx : input integer : maximum number of PF coil groups
      !! lrow1 : input integer : row length of arrays bfix, bvec, gmat,
      !! umat, vmat; should be >= (2*nptsmx + ngrpmx)
      !! lcol1 : input integer : column length of arrays gmat, umat, vmat;
      !! should be >= ngrpmx
      !! ngrp : input integer : number of coil groups, where all coils in a
      !! group have the same current, <= ngrpmx
      !! ccls(ngrpmx) : output real array : solution vector of coil currents
      !! in each group (A)
      !! nrws : input integer : actual number of rows to use
      !! gmat(lrow1,lcol1) : input/output real array : work array
      !! bvec(lrow1) : input/output real array : work array
      !! umat(lrow1,lcol1) : output real array : work array
      !! vmat(lrow1,lcol1) : output real array : work array
      !! sigma(ngrpmx) : output real array : work array
      !! work2(ngrpmx) : output real array : work array
      !! This routine solves the matrix equation for calculating the
      !! currents in a group of ring coils.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use maths_library, only: svd
      implicit none

      !  Arguments

      integer, intent(in) :: ngrpmx,lrow1,lcol1,ngrp,nrws
      real(dp), dimension(lrow1), intent(inout) :: bvec
      real(dp), dimension(lrow1,lcol1), intent(inout) :: gmat
      real(dp), dimension(lrow1,lcol1), intent(out) :: umat,vmat
      real(dp), dimension(ngrpmx), intent(out) :: sigma, work2
      real(dp), dimension(ngrpmx), intent(out) :: ccls

      !  Local variables

      integer :: i,j,ierr
      real(dp) :: zvec,eps
      logical :: truth

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      truth = .true.
      eps = 1.0D-10

      call svd(lrow1,nrws,ngrp,gmat,sigma,truth,umat,truth,vmat,ierr,work2)

      do i = 1,ngrp
         work2(i) = 0.0D0
         do j = 1,nrws
            work2(i) = work2(i)+umat(j,i)*bvec(j)
         end do
      end do

      !  Compute currents

      do i = 1,ngrp
         ccls(i) = 0.0D0
         zvec = 0.0D0
         do j = 1,ngrp
            if (sigma(j) > eps) zvec = work2(j)/sigma(j)
            ccls(i) = ccls(i)+vmat(i,j)*zvec
         end do
      end do

   end subroutine solv

   subroutine rsid(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix, &
      ngrp,ccls,brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)

      !! Computes the norm of the residual vectors
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: D Strickler, ORNL
      !! author: J Galambos, ORNL
      !! author: P C Shipe, ORNL
      !! nptsmx : input integer : maximum number of points across the
      !! plasma midplane at which the magnetic
      !! field is fixed
      !! ngrpmx : input integer : maximum number of PF coil groups
      !! lrow1 : input integer : row length of arrays bfix, gmat;
      !! should be >= (2*nptsmx + ngrpmx)
      !! lcol1 : input integer : column length of array gmat; should be >= ngrpmx
      !! npts : input integer : number of data points at which field is
      !! to be fixed; should be <= nptsmx
      !! brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
      !! data points (T)
      !! nfix : input integer : number of coils with fixed currents, <= nfixmx
      !! ngrp : input integer : number of coil groups, where all coils in a
      !! group have the same current, <= ngrpmx
      !! ccls(ngrpmx) : input real array : coil currents in each group (A)
      !! brssq : output real : sum of squares of radial field residues
      !! brnrm : output real : radial field residue norm
      !! bzssq : output real : sum of squares of vertical field residues
      !! bznrm : output real : vertical field residue norm
      !! ssq : output real : sum of squares of elements of residual vector
      !! bfix(lrow1) : input real array : work array
      !! gmat(lrow1,lcol1) : input real array : work array
      !! This routine calculates the residuals from the matrix
      !! equation for calculation of the currents in a group of ring coils.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: nptsmx,ngrpmx,lrow1,lcol1,npts,nfix,ngrp

      real(dp), dimension(ngrpmx), intent(in) :: ccls
      real(dp), dimension(nptsmx), intent(in) :: brin, bzin
      real(dp), dimension(lrow1), intent(in) :: bfix
      real(dp), dimension(lrow1,lcol1), intent(in) :: gmat

      real(dp), intent(out) :: brssq,brnrm,bzssq,bznrm,ssq

      !  Local variables

      integer :: i,j
      real(dp) :: svec,rvec

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      brnrm = 0.0D0
      brssq = 0.0D0

      do i = 1,npts
         svec = 0.0D0
         if (nfix > 0) svec = bfix(i)
         do j = 1,ngrp
            svec = svec + gmat(i,j)*ccls(j)
         end do
         rvec = svec - brin(i)
         brnrm = brnrm + brin(i)**2
         brssq = brssq + rvec**2
      end do

      bznrm = 0.0D0
      bzssq = 0.0D0

      do i = 1,npts
         svec = 0.0D0
         if (nfix > 0) svec = bfix(i+npts)
         do j = 1,ngrp
            svec = svec + gmat(i+npts,j)*ccls(j)
         end do
         rvec = svec - bzin(i)
         bznrm = bznrm + bzin(i)**2
         bzssq = bzssq + rvec**2
      end do

      ssq = brssq/(1.0D0 + brnrm) + bzssq/(1.0D0+bznrm)

   end subroutine rsid

   subroutine bfield(nc, rc, zc, cc, xc, rp, zp, br, bz, psi)
 
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
 
   subroutine waveform
 
     !! Sets up the PF coil waveforms
     !! author: P J Knight, CCFE, Culham Science Centre
     !! None
     !! This routine sets up the PF coil current waveforms.
     !! <CODE>waves(i,j)</CODE> is the current in coil i, at time j,
     !! normalized to the peak current in that coil at any time.
     !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
       use pfcoil_variables, only: ric, nohc, waves, curpfb, curpff, curpfs
     implicit none
 
     !  Arguments
 
     !  Local variables
 
     integer :: ic,it,nplas
 
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
     !  Initialize plasma current waveform to 1.0
     !  (never actually used)
 
     nplas = nohc+1
     do it = 1,6
        waves(nplas,it) = 1.0D0
     end do
 
     do ic = 1,nohc
 
        !  Find where the peak current occurs
 
        !  Beginning of pulse, t = tramp
 
        if ( (abs(curpfs(ic)) >= abs(curpfb(ic))) .and. &
             (abs(curpfs(ic)) >= abs(curpff(ic))) ) &
             ric(ic) = curpfs(ic)
 
        !  Beginning of flat-top, t = tramp + tohs
 
        if ( (abs(curpff(ic)) >= abs(curpfb(ic))) .and. &
             (abs(curpff(ic)) >= abs(curpfs(ic))) ) &
             ric(ic) = curpff(ic)
 
        !  End of flat-top, t = tramp + tohs + theat + tburn
 
        if ( (abs(curpfb(ic)) >= abs(curpfs(ic))) .and. &
             (abs(curpfb(ic)) >= abs(curpff(ic))) ) &
             ric(ic) = curpfb(ic)
 
        !  Set normalized current waveforms
 
        waves(ic,1) = 0.0D0
        waves(ic,2) = curpfs(ic)/ric(ic)
        waves(ic,3) = curpff(ic)/ric(ic)
        waves(ic,4) = curpff(ic)/ric(ic)
        waves(ic,5) = curpfb(ic)/ric(ic)
        waves(ic,6) = 0.0D0
 
     end do
 
   end subroutine waveform
 
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   subroutine superconpf(bmax,fhe,fcu,jwp,isumat,fhts,strain,thelium, &
        bcritsc,tcritsc,jcritwp,jcritstr,jcritsc,tmarg)
 
     !! Routine to calculate the PF coil superconductor properties
     !! author: P J Knight, CCFE, Culham Science Centre
     !! bmax : input real : Peak field at conductor (T)
     !! fhe : input real : Fraction of cable space that is for He cooling
     !! fcu : input real : Fraction of strand that is copper
     !! jwp : input real : Actual winding pack current density (A/m2)
     !! isumat : input integer : Switch for conductor type:
     !! 1 = ITER Nb3Sn, standard parameters,
     !! 2 = Bi-2212 High Temperature Superconductor,
     !! 3 = NbTi,
     !! 4 = ITER Nb3Sn, user-defined parameters
     !! 5 = WST Nb3Sn parameterisation
     !! 7 = Durham Ginzbug-Landau Nb-Ti parameterisation
     !! fhts    : input real : Adjustment factor (<= 1) to account for strain,
     !! radiation damage, fatigue or AC losses
     !! strain : input real : Strain on superconductor at operation conditions
     !! thelium : input real : He temperature at peak field point (K)
     !! bcritsc : input real : Critical field at zero temperature and strain (T) (isumat=4 only)
     !! tcritsc : input real : Critical temperature at zero field and strain (K) (isumat=4 only)
     !! jcritwp : output real : Critical winding pack current density (A/m2)
     !! jcritstr : output real : Critical strand current density (A/m2)
     !! jcritsc : output real : Critical superconductor current density (A/m2)
     !! tmarg : output real : Temperature margin (K)
     !! This routine calculates the superconductor critical winding pack
     !! current density for the PF coils, plus the temperature margin.
     !! It is based on the TF coil version, <CODE>supercon</CODE>.
     !! None
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
       use error_handling, only: fdiags, idiags, report_error
     use superconductors, only: jcrit_nbti, wstsc, jcrit_rebco, bi2212, &
       itersc, current_sharing_rebco, Gl_nbti, GL_REBCO
       use tfcoil_variables, only: tmargmin_cs, temp_margin, b_crit_upper_nbti, t_crit_nbti 
       use maths_library, only: variable_error, secant_solve
     implicit none
 
     !  Arguments
 
     integer, intent(in) :: isumat
     real(dp), intent(in) :: bmax, fcu, fhe, fhts, jwp, &
          strain, thelium, bcritsc, tcritsc
     real(dp), intent(out) :: jcritwp, jcritstr, jcritsc, tmarg
     logical :: validity
 
     !  Local variables
 
     integer :: lap
     real(dp) :: b,bc20m,bcrit,c0,delt,jcrit0,jcritm, &
          jcritp,jsc,jstrand,jtol,t,tc0m,tcrit,ttest,ttestm,ttestp, icrit, iop
 
     real(dp) :: current_sharing_t
     real(dp)::x1,x2         ! Initial guesses for temperature
     logical::error                   ! True if the solver does not converge
     real(dp)::residual      ! Residual current density error
 
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
     !  Find critical current density in superconducting strand, jcritstr
 
     select case (isumat)
 
     case (1)  !  ITER Nb3Sn critical surface parameterization
        bc20m = 32.97D0
        tc0m = 16.06D0
 
        !  jcritsc returned by itersc is the critical current density in the
        !  superconductor - not the whole strand, which contains copper
 
        call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
        jcritstr = jcritsc * (1.0D0-fcu)
 
     case (2)  !  Bi-2212 high temperature superconductor parameterization
 
        !  Current density in a strand of Bi-2212 conductor
        !  N.B. jcrit returned by bi2212 is the critical current density
        !  in the strand, not just the superconducting portion.
        !  The parameterization for jcritstr assumes a particular strand
        !  composition that does not require a user-defined copper fraction,
        !  so this is irrelevant in this model
 
        jstrand = jwp / (1.0D0-fhe)
 
        call bi2212(bmax,jstrand,thelium,fhts,jcritstr,tmarg)
        jcritsc = jcritstr / (1.0D0-fcu)
        tcrit = thelium + tmarg
 
     case (3)  !  NbTi data
        bc20m = 15.0D0
        tc0m = 9.3D0
        c0 = 1.0D10
        call jcrit_nbti(thelium,bmax,c0,bc20m,tc0m,jcritsc,tcrit)
        jcritstr = jcritsc * (1.0D0-fcu)
 
     case (4)  !  As (1), but user-defined parameters
        bc20m = bcritsc
        tc0m = tcritsc
        call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
        jcritstr = jcritsc * (1.0D0-fcu)
 
     case (5) ! WST Nb3Sn parameterisation
          bc20m = 32.97D0
          tc0m = 16.06D0
 
          !  jcritsc returned by itersc is the critical current density in the
          !  superconductor - not the whole strand, which contains copper
 
          call wstsc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
          jcritstr = jcritsc * (1.0D0-fcu)
 
     case (6) ! "REBCO" 2nd generation HTS superconductor in CrCo strand
        call jcrit_rebco(thelium,bmax,jcritsc,validity,0)
        jcritstr = jcritsc * (1.0D0-fcu)
 
    case (7) ! Durham Ginzburg-Landau Nb-Ti parameterisation
          bc20m = b_crit_upper_nbti
          tc0m = t_crit_nbti 
          call GL_nbti(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
          jcritstr = jcritsc  * (1.0D0-fcu)
 
     case (8) ! Branch YCBO model fit to Tallahassee data
           bc20m = 429D0
           tc0m = 185D0
           call GL_REBCO(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit) 
           ! A0 calculated for tape cross section already
           jcritstr = jcritsc * (1.0D0-fcu)
  
           
       
          
 
     case default  !  Error condition
        idiags(1) = isumat ; call report_error(156)
 
     end select
 
     !  Critical current density in winding pack
 
     jcritwp = jcritstr * (1.0D0-fhe)
     jstrand = jwp / (1.0D0-fhe)
     jsc = jstrand / (1.0D0-fcu)
 
     !  Temperature margin (already calculated in bi2212 for isumat=2)
 
     if ((isumat==1).or.(isumat==4)) then
        !  Newton-Raphson method; start at requested minimum temperature margin
        ttest = thelium + tmargmin_cs
        delt = 0.01D0
        jtol = 1.0D4
        !  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
        !  when we have found the desired value of tmarg
        lap = 0
        solve_for_tmarg: do ; lap = lap+1
           if ((ttest <= 0.0D0).or.(lap > 100)) then
              idiags(1) = lap ; fdiags(1) = ttest
              call report_error(158)
              exit solve_for_tmarg
           end if
           ttestm = ttest - delt
           ttestp = ttest + delt
           select case (isumat)
           case (1,4)
              call itersc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
              if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
              call itersc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
              call itersc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
         !   case (3)
         !      call jcrit_nbti(ttest ,bmax,c0,bc20m,tc0m,jcrit0,t)
         !      write(*,*)'NbTi: ttest = ',ttest, '  jcrit0=', jcrit0, '  jsc=',jsc
         !      if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
         !      call jcrit_nbti(ttestm,bmax,c0,bc20m,tc0m,jcritm,t)
         !      call jcrit_nbti(ttestp,bmax,c0,bc20m,tc0m,jcritp,t)
         !   case (5)
         !      call wstsc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
         !      write(*,*)'WST Nb3Sn: ttest = ',ttest, '  jcrit0=', jcrit0, '  jsc=',jsc
         !      if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
         !      call wstsc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
         !      call wstsc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
           end select
           ttest = ttest - 2.0D0*delt*(jcrit0-jsc)/(jcritp-jcritm)
        end do solve_for_tmarg
        tmarg = ttest - thelium
    end if
 
 
    ! MDK 13/7/18 Use secant solver for NbTi.
    if(isumat==3) then
        x1 = 4d0  ! Initial values of temperature
        x2 = 6d0
        ! Solve for deltaj_nbti = 0
        call secant_solve(deltaj_nbti,x1,x2,current_sharing_t,error,residual,100d0)
        tmarg = current_sharing_t - thelium
        call jcrit_nbti(current_sharing_t ,bmax,c0,bc20m,tc0m,jcrit0,t)
        if(variable_error(current_sharing_t))then  ! current sharing secant solver has failed.
            write(*,'(a24, 10(a12,es12.3))')'NbTi: current sharing ', 'temperature=', current_sharing_t, '  tmarg=', tmarg, &
                                            '  jsc=',jsc, '  jcrit0=',jcrit0,  '  residual=', residual
        end if
    end if
 
    ! MDK 13/7/18 Use secant solver for WST.
    if(isumat==5) then
        ! Current sharing temperature for WST Nb3Sn
        x1 = 4d0  ! Initial values of temperature
        x2 = 6d0
        ! Solve for deltaj_wst = 0
        call secant_solve(deltaj_wst,x1,x2,current_sharing_t,error,residual,100d0)
        tmarg = current_sharing_t - thelium
        call wstsc(current_sharing_t,bmax,strain,bc20m,tc0m,jcrit0,b,t)
        if(variable_error(current_sharing_t))then  ! current sharing secant solver has failed.
            write(*,'(a24, 10(a12,es12.3))')'WST: current sharing ', 'temperature=', current_sharing_t, '  tmarg=', tmarg, &
                                            '  jsc=',jsc, '  jcrit0=',jcrit0, '  residual=', residual
        end if
    end if
 
     ! Temperature margin: An alternative method using secant solver
    if (isumat == 6) then
       call current_sharing_rebco(current_sharing_t, bmax, jsc)
       tmarg = current_sharing_t - thelium
       temp_margin = tmarg
    end if
 
    ! SCM 16/03/20 Use secant solver for GL_nbti.
    if(isumat==7) then
       ! Current sharing temperature for Durham Ginzburg-Landau Nb-Ti
       x1 = 4.0d0  ! Initial values of temperature
       x2 = 6.0d0
       ! Solve for deltaj_GL_nbti = 0
       call secant_solve(deltaj_GL_nbti,x1,x2,current_sharing_t,error,residual,100d0)
       tmarg = current_sharing_t - thelium
       call GL_nbti(current_sharing_t,bmax,strain,bc20m,tc0m,jcrit0,b,t)
       if(variable_error(current_sharing_t))then  ! current sharing secant solver has failed.
           write(*,'(a24, 10(a12,es12.3))')'Gl_nbti: current sharing ', 'temperature=', current_sharing_t, '  tmarg=', tmarg, &
                                           '  jsc=',jsc, '  jcrit0=',jcrit0, '  residual=', residual
       end if
   end if
 
 ! SCM 10/08/20 Use secant solver for GL_REBCO.
    if(isumat==8) then
     ! Current sharing temperature for Durham Ginzburg-Landau Nb-Ti
     x1 = 4.0d0  ! Initial values of temperature
     x2 = 6.0d0
     ! Solve for deltaj_GL_REBCO = 0
     call secant_solve(deltaj_GL_REBCO,x1,x2,current_sharing_t,error,residual,100d0)
     tmarg = current_sharing_t - thelium
     call GL_REBCO(current_sharing_t,bmax,strain,bc20m,tc0m,jcrit0,b,t)
     if(variable_error(current_sharing_t))then  ! current sharing secant solver has failed.
         write(*,'(a24, 10(a12,es12.3))')'Gl_REBCO: current sharing ', 'temperature=', current_sharing_t, '  tmarg=', tmarg, &
                                         '  jsc=',jsc, '  jcrit0=',jcrit0, '  residual=', residual
     end if
  end if
  
 contains
     ! These functions are required because secant_solve requires a function not a subroutine
     ! They need to follow a 'contains' statement because 'jcrit0', 'bmax' and others
     ! must be available but cannot be passed, because secant_solve requires
     ! a function of one variable.
     ! TODO This nested structure (and above limitations) should be removed
     ! due to its implicit use of parent scope
     ! Can't pass functions from Python to Fortran; these functions and
     ! secant_solv() need to remain in same language if passing function as
     ! argument
 
     function deltaj_nbti(temperature)
         real(dp), intent(in) :: temperature
         real(dp)::deltaj_nbti, jcrit0
         call jcrit_nbti(temperature,bmax,c0,bc20m,tc0m,jcrit0,t)
         if(variable_error(jcrit0))then  ! jcrit_nbti has failed.
             write(*,'(a24, 10(a12,es12.3))')'jcrit_nbti: ', 'bmax=', bmax, '  temperature=', temperature, &
                                             '  jcrit0=',jcrit0
         end if
         deltaj_nbti = jcrit0 - jsc
     end function deltaj_nbti
 
     function deltaj_wst(temperature)
         real(dp), intent(in) :: temperature
         real(dp)::deltaj_wst, jcrit0
         call wstsc(temperature,bmax,strain,bc20m,tc0m,jcrit0,b,t)
         if(variable_error(jcrit0))then  ! wstsc has failed.
             write(*,'(a24, 10(a12,es12.3))')'deltaj_wst: ', 'bmax=', bmax, '  temperature=', temperature, &
                                             '  jcrit0=',jcrit0
         end if
         deltaj_wst = jcrit0 - jsc
     end function deltaj_wst
 
     function deltaj_GL_nbti(temperature)
       real(dp), intent(in) :: temperature
       real(dp)::deltaj_Gl_nbti, jcrit0
       call GL_nbti(temperature,bmax,strain,bc20m,tc0m,jcrit0,b,t)
       if(variable_error(jcrit0))then  ! GL_Nbti has failed.
         write(*,'(a24, 10(a12,es12.3))')'deltaj_GL_nbti: ', 'bmax=', bmax, '  temperature=', temperature, &
                                           '  jcrit0=',jcrit0
       end if
       deltaj_GL_nbti = jcrit0 - jsc
   end function deltaj_GL_nbti
   
        function deltaj_GL_REBCO(temperature)
        real(dp), intent(in) :: temperature
        real(dp)::deltaj_Gl_REBCO, jcrit0
        call GL_REBCO(temperature,bmax,strain,bc20m,tc0m,jcrit0,b,t)
        if(variable_error(jcrit0))then  ! GL_REBCO has failed.
          write(*,'(a24, 10(a12,es12.3))')'deltaj_GL_REBCO: ', 'bmax=', bmax, '  temperature=', temperature, &
                                            '  jcrit0=',jcrit0
        end if
        deltaj_GL_REBCO = jcrit0 - jsc
      end function deltaj_GL_REBCO
  
 end subroutine superconpf
 
   function selfinductance(a,b,c,N)
     !! Calculates the selfinductance using Bunet's formula
     !! author: M. Kovari, CCFE
     !! a  : input real : mean radius of coil (m)
     !! b  : input real : length of coil (m) (given as l in the reference)
     !! c  : input real : radial winding thickness (m)
     !! N  : input real : number of turns
     !! This routine calculates the self inductance in Henries
     !! Radiotron Designers Handbook (4th Edition) chapter 10
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
     implicit none
 
     ! Function declarations
     real(dp) :: a, b, c, N, selfinductance
 
     ! Calculate self inductance
     selfinductance = (1.0d-6/0.0254d0)*a**2*N**2/(9.0d0*a + 10.0d0*b + 8.4d0*c + 3.2d0*c*b/a)
 
     ! OLD Formula
     ! JM - KEEP FOR NOW
     ! Formulas and tables for the calculation of mutual and self-inductance [Revised],
     ! Rosa and Grover, Scientific papers of the Bureau of Standards, No. 169, 3rd ed., 1916
     ! a = mean radius of coil
     ! b = length of coil
     ! c = radial winding thickness
     ! N = number of turns
     ! real(dp) :: a,b,c, N, selfinductance, r, r2_16a2, x, x2, q, at, lambda, mu, p
 
     ! !  Equation 88, p. 137
     ! x = b/c
     ! x2 = x**2
     ! q = log(1.0d0+x2)
     ! p = log(1.0d0+1.0d0/x2)
     ! at = atan(x)
     ! lambda = log(8.0d0*a/c) + 1/12.0d0 - pi*x/3.0d0 - 0.5d0*q + 1/(12.0d0*x2)*q &
     !           + 1/12.0d0*x2*p + 2.0d0/3.0d0*(x-1.0d0/x)*at
 
     ! mu = c**2/(96.0d0*a**2)*     &
     !      ((log(8.0d0*a/c)-0.5d0*q)*(1+3.0d0*x2) + 3.45d0*x2 + 221.0d0/60.0d0   &
     !      -1.6d0*pi*x**3 + 3.2d0*x**3*at - 0.1d0/x2*q + 0.5d0*x2**2*p)
 
     ! selfinductance = rmu0*a*N**2 * (lambda + mu)
 
     ! if((selfinductance<0.0d0).or.(selfinductance .ne. selfinductance).or.(selfinductance>1000.0d0)) then
     !     write(*,*) 'a = ',a, ' b = ', b
     !     write(*,*) 'c = ',c, ' x = ', x
     !     write(*,*) 'q = ',q, ' at = ', at
     !     write(*,*) 'x2 = ',x2, ' N = ', N
     !     write(*,*) 'lambda = ',lambda, ' mu = ', mu
     !     write(*,*) 'selfinductance = ',selfinductance
     ! end if
 
   end function selfinductance
 
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   subroutine brookscoil(outfile)
     ! http://www.nessengr.com/techdata/brooks/brooks.html
       use process_output, only: ovarre, oblnkl, ocmmnt
     implicit none
     real(dp) :: a,b,c, N, l, lp
     character(len=10) :: test
     integer, intent(in) :: outfile
 
     c = 1.0d0
     a = 1.5d0 * c
     b = c
     N = 1.0d0
 
     l = 0.025491d0 * c * 100.0d0 * N**2 * 1.0d-6
     lp = selfinductance(a,b,c,N)
     if ((l/lp < 1.05d0).and.(l/lp > 0.95d0)) then
         test = 'PASS'
     else
         test = 'FAIL'
     end if
     call ocmmnt(outfile,'Unit test of self-inductance formula: '//test)
     call ovarre(outfile,'Self-inductance of 1m Brooks coil: standard formula', '(l)',l, 'OP ')
     call ovarre(outfile,'Self-inductance of 1m Brooks coil: PROCESS formula', '(lp)',lp, 'OP ')
     call oblnkl(outfile)
     write(*,*) 'Test of self-inductance formula: '//test
 
   end subroutine brookscoil
 end module pfcoil_module
 
 