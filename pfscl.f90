!  $Id:: bldgs.f90 16 2011-08-02 08:07:34Z pknight                      $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts, &
     rpts,zpts,brin,bzin,nfix,rfix,zfix,cfix,ngrp,ncls,rcls,zcls, &
     alfa,work1,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat,sigma,work2,ssq,ccls)

  !+ad_name  efc
  !+ad_summ  Calculates field coil currents
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  D Strickler, ORNL
  !+ad_auth  J Galambos, ORNL
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
  !+ad_args  nclsmx : input integer : maximum number of coils in one group
  !+ad_args  nptsmx : input integer : maximum number of points across the
  !+ad_argc                           plasma midplane at which the magnetic
  !+ad_argc                           field is fixed
  !+ad_args  nfixmx : input integer : maximum number of fixed current coils
  !+ad_args  lrow1 : input integer : row length of arrays bfix, bvec, gmat,
  !+ad_argc                          umat, vmat; should be >= (2*nptsmx + ngrpmx)
  !+ad_args  lcol1 : input integer : column length of arrays gmat, umat, vmat;
  !+ad_argc                          should be >= ngrpmx
  !+ad_args  npts : input integer : number of data points at which field is
  !+ad_args                         to be fixed; should be <= nptsmx
  !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
  !+ad_args  brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
  !+ad_argc                                                  data points (T)
  !+ad_args  nfix : input integer : number of coils with fixed currents, <= nfixmx
  !+ad_args  rfix(nfixmx),zfix(nfixmx) : input real arrays : coordinates of coils
  !+ad_argc                                                  with fixed currents (m)
  !+ad_args  cfix(nfixmx) : input real array : Fixed currents (A)
  !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
  !+ad_argc                         group have the same current, <= ngrpmx
  !+ad_args  ncls(ngrpmx+2) : input integer array : number of coils in each group,
  !+ad_argc                   each value <= nclsmx
  !+ad_args  rcls(ngrpmx,nclsmx),zcls(ngrpmx,nclsmx) : input real arrays : coords
  !+ad_argc                               R(i,j), Z(i,j) of coil j in group i (m)
  !+ad_args  alfa : input real : smoothing parameter (0 = no smoothing,
  !+ad_argc                      1.0D-9 = large smoothing)
  !+ad_args  work1(nfixmx) : input/output real array : work array
  !+ad_args  bfix(lrow1) : input/output real array : work array
  !+ad_args  gmat(lrow1,lcol1) : input/output real array : work array
  !+ad_args  bvec(lrow1) : input/output real array : work array
  !+ad_args  rc(nclsmx) : input/output real array : work array
  !+ad_args  zc(nclsmx) : input/output real array : work array
  !+ad_args  cc(nclsmx) : input/output real array : work array
  !+ad_args  xc(nclsmx) : input/output real array : work array
  !+ad_args  umat(lrow1,lcol1) : input/output real array : work array
  !+ad_args  vmat(lrow1,lcol1) : input/output real array : work array
  !+ad_args  sigma(ngrpmx) : input/output real array : work array
  !+ad_args  work2(ngrpmx) : input/output real array : work array
  !+ad_args  ssq : output real : sum of squares of elements of residual vector
  !+ad_args  ccls(ngrpmx) : output real array : solution vector of coil currents
  !+ad_argc                                     in each group (A)
  !+ad_desc  This routine calculates the currents required in a group
  !+ad_desc  of ring coils to produce a fixed field at prescribed
  !+ad_desc  locations. Additional ring coils with fixed currents are
  !+ad_desc  also allowed.
  !+ad_prob  None
  !+ad_call  fixb
  !+ad_call  mtrx
  !+ad_call  rsid
  !+ad_call  solv
  !+ad_hist  18/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1
  integer, intent(in) :: ngrp,npts,nfix
  integer, dimension(ngrpmx+2), intent(in) :: ncls

  real(kind(1.0D0)), intent(in) :: alfa
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx), intent(in) :: rcls,zcls
  real(kind(1.0D0)), dimension(nptsmx), intent(in) :: brin,bzin,rpts,zpts
  real(kind(1.0D0)), dimension(nfixmx), intent(in) :: rfix,zfix,cfix

  real(kind(1.0D0)), dimension(lrow1), intent(inout) :: bfix,bvec
  real(kind(1.0D0)), dimension(lrow1,lcol1), intent(inout) :: gmat,umat,vmat
  real(kind(1.0D0)), dimension(ngrpmx), intent(inout) :: sigma,work2
  real(kind(1.0D0)), dimension(nclsmx), intent(inout) :: rc,zc,cc,xc
  real(kind(1.0D0)), dimension(nfixmx), intent(inout) :: work1

  real(kind(1.0D0)), intent(out) :: ssq
  real(kind(1.0D0)), dimension(ngrpmx), intent(out) :: ccls

  !  Local variables

  real(kind(1.0D0)) :: brssq, brnrm, bzssq, bznrm
  integer :: nrws

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Calculate field from the fixed current loops

  call fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix,zfix,cfix, &
       work1,bfix)

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

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine mtrx(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts, &
     brin,bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec, &
     rc,zc,cc,xc)

  !+ad_name  mtrx
  !+ad_summ  Set up the matrix equation to calculate the currents
  !+ad_summ  in a group of ring coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  D Strickler, ORNL
  !+ad_auth  J Galambos, ORNL
  !+ad_cont  N/A
  !+ad_args  nptsmx : input integer : maximum number of points across the
  !+ad_argc                           plasma midplane at which the magnetic
  !+ad_argc                           field is fixed
  !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
  !+ad_args  nclsmx : input integer : maximum number of coils in one group
  !+ad_args  lrow1 : input integer : row length of arrays bfix, bvec, gmat,
  !+ad_argc                          umat, vmat; should be >= (2*nptsmx + ngrpmx)
  !+ad_args  lcol1 : input integer : column length of arrays gmat, umat, vmat;
  !+ad_argc                          should be >= ngrpmx
  !+ad_args  npts : input integer : number of data points at which field is
  !+ad_args                         to be fixed; should be <= nptsmx
  !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
  !+ad_args  lrow1 : input integer : row length of array bfix; should be >= nptsmx
  !+ad_args  npts : input integer : number of data points at which field is
  !+ad_args                         to be fixed; should be <= nptsmx
  !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
  !+ad_args  brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
  !+ad_argc                                                  data points (T)
  !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
  !+ad_argc                         group have the same current, <= ngrpmx
  !+ad_args  ncls(ngrpmx+2) : input integer array : number of coils in each group,
  !+ad_argc                   each value <= nclsmx
  !+ad_args  rcls(ngrpmx,nclsmx),zcls(ngrpmx,nclsmx) : input real arrays : coords
  !+ad_argc                               R(i,j), Z(i,j) of coil j in group i (m)
  !+ad_args  alfa : input real : smoothing parameter (0 = no smoothing,
  !+ad_argc                      1.0D-9 = large smoothing)
  !+ad_args  nrws : output integer : actual number of rows to use
  !+ad_args  bfix(lrow1) : input real array : Fields at data points (T)
  !+ad_args  gmat(lrow1,lcol1) : output real array : work array
  !+ad_args  bvec(lrow1) : output real array : work array
  !+ad_args  rc(nclsmx) : output real array : Coordinates of conductor loops (m)
  !+ad_args  zc(nclsmx) : output real array : Coordinates of conductor loops (m)
  !+ad_args  cc(nclsmx) : output real array : Currents in conductor loops (A)
  !+ad_args  xc(nclsmx) : output real array : Mutual inductances (H)
  !+ad_desc  This routine sets up the matrix equation for calculating the
  !+ad_desc  currents in a group of ring coils.
  !+ad_prob  None
  !+ad_call  bfield
  !+ad_hist  19/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nptsmx, ngrpmx, nclsmx, lrow1, lcol1, npts, ngrp
  integer, dimension(ngrpmx+2), intent(in) :: ncls
  real(kind(1.0D0)), intent(in) :: alfa
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx), intent(in) :: rcls, zcls
  real(kind(1.0D0)), dimension(nptsmx), intent(in) :: brin, bzin, rpts, zpts
  real(kind(1.0D0)), dimension(lrow1), intent(in) :: bfix

  integer, intent(out) :: nrws
  real(kind(1.0D0)), dimension(nclsmx), intent(out) :: rc, zc, cc, xc
  real(kind(1.0D0)), dimension(lrow1), intent(out) :: bvec
  real(kind(1.0D0)), dimension(lrow1,lcol1), intent(out) :: gmat

  !  Local variables

  integer :: i, j, k, nc
  real(kind(1.0D0)) brw, bzw, psw

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
        call bfield(nclsmx,nc,rc,zc,cc,xc,rpts(i),zpts(i),brw,bzw,psw)
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
     gmat(nrws + j,j) = DBLE(nc) * alfa
  end do

  nrws = 2*npts + ngrp

end subroutine mtrx

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine solv(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat, &
     vmat,sigma,work2)

  !+ad_name  solv
  !+ad_summ  Solve a matrix using singular value decomposition
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  D Strickler, ORNL
  !+ad_auth  J Galambos, ORNL
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
  !+ad_args  lrow1 : input integer : row length of arrays bfix, bvec, gmat,
  !+ad_argc                          umat, vmat; should be >= (2*nptsmx + ngrpmx)
  !+ad_args  lcol1 : input integer : column length of arrays gmat, umat, vmat;
  !+ad_argc                          should be >= ngrpmx
  !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
  !+ad_argc                         group have the same current, <= ngrpmx
  !+ad_args  ccls(ngrpmx) : output real array : solution vector of coil currents
  !+ad_argc                                     in each group (A)
  !+ad_args  nrws : input integer : actual number of rows to use
  !+ad_args  gmat(lrow1,lcol1) : input/output real array : work array
  !+ad_args  bvec(lrow1) : input/output real array : work array
  !+ad_args  umat(lrow1,lcol1) : output real array : work array
  !+ad_args  vmat(lrow1,lcol1) : output real array : work array
  !+ad_args  sigma(ngrpmx) : output real array : work array
  !+ad_args  work2(ngrpmx) : output real array : work array
  !+ad_desc  This routine solves the matrix equation for calculating the
  !+ad_desc  currents in a group of ring coils.
  !+ad_prob  None
  !+ad_call  svd
  !+ad_hist  18/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: ngrpmx,lrow1,lcol1,ngrp,nrws
  real(kind(1.0D0)), dimension(lrow1), intent(inout) :: bvec
  real(kind(1.0D0)), dimension(lrow1,lcol1), intent(inout) :: gmat
  real(kind(1.0D0)), dimension(lrow1,lcol1), intent(out) :: umat,vmat
  real(kind(1.0D0)), dimension(ngrpmx), intent(out) :: sigma, work2
  real(kind(1.0D0)), dimension(ngrpmx), intent(out) :: ccls

  !  Local variables

  integer :: i,j,ierr
  real(kind(1.0D0)) :: zvec,eps
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

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine rsid(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix, &
     ngrp,ccls,brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)

  !+ad_name  rsid
  !+ad_summ  Computes the norm of the residual vectors
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  D Strickler, ORNL
  !+ad_auth  J Galambos, ORNL
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nptsmx : input integer : maximum number of points across the
  !+ad_argc                           plasma midplane at which the magnetic
  !+ad_argc                           field is fixed
  !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
  !+ad_args  lrow1 : input integer : row length of arrays bfix, gmat;
  !+ad_argc                          should be >= (2*nptsmx + ngrpmx)
  !+ad_args  lcol1 : input integer : column length of array gmat; should be >= ngrpmx
  !+ad_args  npts : input integer : number of data points at which field is
  !+ad_args                         to be fixed; should be <= nptsmx
  !+ad_args  brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
  !+ad_argc                                                  data points (T)
  !+ad_args  nfix : input integer : number of coils with fixed currents, <= nfixmx
  !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
  !+ad_argc                         group have the same current, <= ngrpmx
  !+ad_args  ccls(ngrpmx) : input real array : coil currents in each group (A)
  !+ad_args  brssq : output real : sum of squares of radial field residues
  !+ad_args  brnrm : output real : radial field residue norm
  !+ad_args  bzssq : output real : sum of squares of vertical field residues
  !+ad_args  bznrm : output real : vertical field residue norm
  !+ad_args  ssq : output real : sum of squares of elements of residual vector
  !+ad_args  bfix(lrow1) : input real array : work array
  !+ad_args  gmat(lrow1,lcol1) : input real array : work array
  !+ad_desc  This routine calculates the residuals from the matrix
  !+ad_desc  equation for calculation of the currents in a group of ring coils.
  !+ad_prob  None
  !+ad_call  svd
  !+ad_hist  18/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nptsmx,ngrpmx,lrow1,lcol1,npts,nfix,ngrp

  real(kind(1.0D0)), dimension(ngrpmx), intent(in) :: ccls
  real(kind(1.0D0)), dimension(nptsmx), intent(in) :: brin, bzin
  real(kind(1.0D0)), dimension(lrow1), intent(in) :: bfix
  real(kind(1.0D0)), dimension(lrow1,lcol1), intent(in) :: gmat

  real(kind(1.0D0)), intent(out) :: brssq,brnrm,bzssq,bznrm,ssq

  !  Local variables

  integer :: i,j
  real(kind(1.0D0)) :: svec,rvec

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

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix, &
     zfix,cfix,work1,bfix)

  !+ad_name  fixb
  !+ad_summ  Calculates the field from the fixed current loops
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  D Strickler, ORNL
  !+ad_auth  J Galambos, ORNL
  !+ad_cont  N/A
  !+ad_args  nptsmx : input integer : maximum number of points across the
  !+ad_argc                           plasma midplane at which the magnetic
  !+ad_argc                           field is fixed
  !+ad_args  nfixmx : input integer : maximum number of fixed current coils
  !+ad_args  lrow1 : input integer : row length of array bfix; should be >= nptsmx
  !+ad_args  npts : input integer : number of data points at which field is
  !+ad_args                         to be fixed; should be <= nptsmx
  !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
  !+ad_args  nfix : input integer : number of coils with fixed currents, <= nfixmx
  !+ad_args  rfix(nfixmx),zfix(nfixmx) : input real arrays : coordinates of coils
  !+ad_argc                                                  with fixed currents (m)
  !+ad_args  cfix(nfixmx) : input real array : Fixed currents (A)
  !+ad_args  work1(nfixmx) : output real array : work array
  !+ad_args  bfix(lrow1) : output real array : Fields at data points (T)
  !+ad_desc  This routine calculates the fields at the points specified by
  !+ad_desc  (rpts,zpts) from the set of coils with fixed currents.
  !+ad_prob  None
  !+ad_call  bfield
  !+ad_hist  19/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nptsmx, nfixmx, lrow1, npts, nfix
  real(kind(1.0D0)), dimension(nptsmx), intent(in) :: rpts, zpts
  real(kind(1.0D0)), dimension(nfixmx), intent(in) :: rfix, zfix, cfix
  real(kind(1.0D0)), dimension(lrow1), intent(out) :: bfix

  !  no need for this to be an argument at all...
  real(kind(1.0D0)), dimension(nfixmx), intent(out) :: work1

  !  Local variables

  integer :: i
  real(kind(1.0D0)) brw,bzw,psw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,npts
     bfix(i) = 0.0D0
     bfix(npts + i) = 0.0D0
  end do

  if (nfix <= 0) return

  do i = 1,npts
     call bfield(nfixmx,nfix,rfix,zfix,cfix,work1,rpts(i),zpts(i),brw,bzw,psw)
     bfix(i) = brw
     bfix(npts + i) = bzw
  end do

end subroutine fixb

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bfield(ncmax, nc, rc, zc, cc, xc, rp, zp, br, bz, psi)

  !+ad_name  bfield
  !+ad_summ  Calculate the field at a point due to currents in a number
  !+ad_summ  of circular conductor loops.
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  D Strickler, ORNL
  !+ad_auth  J Galambos, ORNL
  !+ad_cont  N/A
  !+ad_args  ncmax : input integer : maximum number of conductor loops
  !+ad_args  nc : input integer : actual number of loops
  !+ad_args  rc(ncmax) : input real array : R coordinates of loops (m)
  !+ad_args  zc(ncmax) : input real array : Z coordinates of loops (m)
  !+ad_args  cc(ncmax) : input real array : Currents in loops (A)
  !+ad_args  xc(ncmax) : output real array : Mutual inductances (H)
  !+ad_args  rp, zp : input real : coordinates of point of interest (m)
  !+ad_args  br : output real : radial field component at (rp,zp) (T)
  !+ad_args  bz : output real : vertical field component at (rp,zp) (T)
  !+ad_args  psi : output real : poloidal flux at (rp,zp) (Wb)
  !+ad_desc  This routine calculates the magnetic field components and
  !+ad_desc  the poloidal flux at an (R,Z) point, given the locations
  !+ad_desc  and currents of a set of conductor loops.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  19/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nc,ncmax
  real(kind(1.0D0)), intent(in) :: rp, zp
  real(kind(1.0D0)), dimension(ncmax), intent(in) :: rc, zc, cc
  real(kind(1.0D0)), dimension(ncmax), intent(out) :: xc
  real(kind(1.0D0)), intent(out) :: br, bz, psi

  !  Local variables

  integer :: i
  real(kind(1.0D0)) a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4
  real(kind(1.0D0)) :: xmu,twopi,zs,dr,d,s,t,a,xk,xe,dz,sd,brx,bzx

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  xmu = 1.256637062D-6
  twopi = 6.283185308D0

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

     xc(i) = 0.5D0*xmu*sd*((2.0D0 - s)*xk - 2.0D0*xe)

     !  Radial, vertical fields

     brx = xmu*cc(i)*dz/(twopi*rp*sd)*(- xk + &
          (rc(i)**2 + rp**2 + zs)/(dr**2 + zs)*xe)
     bzx = xmu*cc(i)/(twopi*sd)*(xk + &
          (rc(i)**2 - rp**2 - zs)/(dr**2 + zs)*xe)

     !  Sum fields, flux

     br = br + brx
     bz = bz + bzx
     psi = psi + xc(i)*cc(i)

  end do

end subroutine bfield
