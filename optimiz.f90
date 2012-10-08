!  $Id::                                                                $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine optimiz(ifail,f)

  !+ad_name  optimiz
  !+ad_summ  Calls the minimisation/maximisation routine VMCON
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : output integer : error flag
  !+ad_args  f       : output real    : value of objective function at the output point
  !+ad_desc  This routine calls the minimisation/maximisation routine VMCON,
  !+ad_desc  developed by Argonne National Laboratory.
  !+ad_desc  On exit, the (normalised) value of the variable being maximised
  !+ad_desc  or minimised (i.e. the figure of merit) is returned in argument
  !+ad_desc  <CODE>f</CODE>.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  fcnvmc1
  !+ad_call  fcnvmc2
  !+ad_call  vmcon
  !+ad_hist  02/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'

  !  Arguments

  integer, intent(out) :: ifail
  real(kind(1.0D0)), intent(out) :: f

  !  Local variables

  integer :: ii,lb,lcnorm,ldel,lh,liwa,lwa,m,meq,mode,n
  integer, parameter :: ippn1  = ipnvars+1
  integer, parameter :: ipldel = 7*ippn1
  integer, parameter :: iplh   = 2*ippn1
  integer, parameter :: ipvmu  = ipeqns+2*ipnvars+1
  integer, parameter :: ipliwa = 6*ippn1+ipeqns
  integer, dimension(ipliwa) :: iwa
  integer, dimension(ipnvars) :: ilower,iupper

  real(kind(1.0D0)) :: xtol
  real(kind(1.0D0)), dimension(ipnvars) :: bdelta,bndl,bndu,etav,fgrd, &
       gammv,glag,glaga,xa,xv
  real(kind(1.0D0)), dimension(ipeqns) :: cm,conf
  real(kind(1.0D0)), dimension(ippn1) :: bdl,bdu,gm
  real(kind(1.0D0)), dimension(ipvmu) :: vmu
  real(kind(1.0D0)), dimension(ipldel) :: delta
  real(kind(1.0D0)), dimension(iplh) :: wa
  real(kind(1.0D0)), dimension(ippn1,ipeqns) :: cnorm
  real(kind(1.0D0)), dimension(ippn1,ippn1) :: b
  real(kind(1.0D0)), dimension(iplh,iplh) :: h

  !  External routines

  external :: fcnvmc1, fcnvmc2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  n = nvar
  m = neqns
  meq = neqns-nineqns
  xtol = epsvmc
  mode = 0
  lb = ippn1
  lcnorm = ippn1
  ldel = ipldel
  lh = iplh
  lwa = iplh
  liwa = ipliwa

  do ii = 1,n
     ilower(ii) = 1
     iupper(ii) = 1
     bndl(ii) = bondl(ii)
     bndu(ii) = bondu(ii)
     xv(ii) = xcm(ii)
  end do

  nvrbl = nvar
  call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
       lcnorm,b,lb,xtol,maxcal,ifail,nfev2,vlam,glag,vmu,cm,glaga, &
       gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
       liwa,ilower,iupper,bndl,bndu)

  do ii = 1,n
     xcm(ii) = xv(ii)
  end do

  do ii = 1,m
     rcm(ii) = conf(ii)
  end do

end subroutine optimiz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine funfom(fc)

  !+ad_name  funfom
  !+ad_summ  Objective function evaluator for VMCON
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  fc : output real : value of objective function at the output point
  !+ad_desc  This routine evaluates the value of the objective function
  !+ad_desc  i.e. the (normalised) figure-of-merit, at the nvar-dimensional
  !+ad_desc  point of interest.
  !+ad_desc  <P>Each equation for <CODE>fc<CODE> gives a value of the
  !+ad_desc  order of unity for the sake of the numerics.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  phydat.h90
  !+ad_call  cdriv.h90
  !+ad_call  tfcoil.h90
  !+ad_call  cost.h90
  !+ad_call  pwrcom.h90
  !+ad_call  htpwr.h90
  !+ad_call  divrt.h90
  !+ad_hist  02/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'
  include 'phydat.h90'
  include 'cdriv.h90'
  include 'tfcoil.h90'
  include 'cost.h90'
  include 'pwrcom.h90'
  include 'htpwr.h90'
  include 'divrt.h90'

  !  Arguments

  real(kind(1.0D0)), intent(out) :: fc

  !  Local variables

  integer :: iab
  real(kind(1.0D0)) :: sgn

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  iab = abs(minmax)
  sgn = sign(1.0D0, real(minmax))

  !  If sgn is -1 the value of fc will be maximised
  !  If sgn is +1 the value of fc will be minimised

  select case (iab)

  case (1)  !  major radius
     fc = sgn * 0.2D0 * rmajor

  case (2)  !  fusion power / input power
     fc = sgn * powfmw/( (pinji+pinje)/1.0D6 + tfcpmw + ppump/1.0D6)

  case (3)  !  neutron wall load
     fc = sgn * wallmw

  case (4)  !  TF coil + PF coil power
     fc = sgn * (tfcmw + 1.0D-3*srcktpm)/10.0D0

  case (5)  !  fusion power / injection power
     fc = sgn * powfmw / ( (pinji+pinje)/1.0D6 )

  case (6)  !  cost of electricity
     fc = sgn * coe/100.0D0

  case (7)  !  direct/constructed/capital cost
     if (ireactor == 0) then
        fc = sgn * cdirt/1.0D3
     else
        fc = sgn * concost/1.0D3
     end if

  case (8)  !  aspect ratio
     fc = sgn * aspect

  case (9)  !  divertor heat load
     fc = sgn * hldiv

  case (10)  !  toroidal field on axis
     fc = sgn * bt

  case (11)  !  injection power
     fc = sgn * (pinje+pinji) / 1.0D6

  case (12)  !  hydrogen production capital cost
     fc = sgn * chplant / 1.0D2

  case (13)  !  hydrogen production rate
     fc = sgn * hpower / 1.0D2

  case default
     write(*,*) 'Error in routine FUNFOM :'
     write(*,*) 'No such figure of merit, ',iab
     write(*,*) 'PROCESS stopping.'
     stop

  end select

  !  Crude method of catching NaN errors

  if ((abs(fc) > 9.99D99).or.(fc /= fc)) then
     write(*,*) 'Error in routine FUNFOM:'
     write(*,*) 'NaN error in figure of merit calculation ',iab
     write(*,*) 'PROCESS stopping.'
     stop
  end if

end subroutine funfom

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine fcnvmc1(n,m,xv,objf,conf,ifail)

  !+ad_name  fcnvmc1
  !+ad_summ  Function evaluator for VMCON
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  n       : input integer     : number of variables
  !+ad_args  m       : input integer     : number of constraints
  !+ad_args  xv(n)   : input real array  : scaled variable values
  !+ad_args  objf    : output real       : objective function
  !+ad_args  conf(m) : output real array : constraint functions
  !+ad_args  ifail   : output integer    : error flag, if < 0 stops calculation
  !+ad_desc  This routine is the function evaluator for the VMCON
  !+ad_desc  maximisation/minimisation routine.
  !+ad_desc  <P>It calculates the objective and constraint functions at the
  !+ad_desc  n-dimensional point of interest <CODE>xv</CODE>.
  !+ad_desc  Note that the equality constraints must precede the inequality
  !+ad_desc  constraints in <CODE>conf</CODE>.
  !+ad_prob  None
  !+ad_call  caller
  !+ad_call  con1
  !+ad_call  funfom
  !+ad_hist  02/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: n,m
  real(kind(1.0D0)), dimension(n), intent(in) :: xv
  real(kind(1.0D0)), intent(out) :: objf
  real(kind(1.0D0)), dimension(m), intent(out) :: conf
  integer, intent(out) :: ifail

  !  Local variables

  real(kind(1.0D0)) :: fbac,ffor

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Evaluate machine parameters at xv

  call caller(xv,n)

  !  Evaluate figure of merit (objective function)

  call funfom(objf)

  !  Evaluate constraint equations

  call con1(m,conf)

  !  To stop the program, set ifail < 0 here.

  ifail = 1 * ifail

end subroutine fcnvmc1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine fcnvmc2(n,m,xv,fgrd,cnorm,lcnorm,ifail)

  !+ad_name  fcnvmc2
  !+ad_summ  Gradient function evaluator for VMCON
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  n       : input integer     : number of variables
  !+ad_args  m       : input integer     : number of constraints
  !+ad_args  xv(n)   : input real array  : scaled variable values
  !+ad_args  fgrd(n) : output real array : gradient of the objective function
  !+ad_args  cnorm(lcnorm,m) : output real array : constraint gradients, i.e.
  !+ad_argc           cnorm(i,j) is the derivative of constraint j w.r.t. variable i
  !+ad_args  lcnorm  : input integer     : number of columns in cnorm
  !+ad_args  ifail   : output integer    : error flag, if < 0 stops calculation
  !+ad_desc  This routine is the gradient function evaluator for the VMCON
  !+ad_desc  maximisation/minimisation routine.
  !+ad_desc  <P>It calculates the gradients of the objective and constraint
  !+ad_desc  functions at the n-dimensional point of interest <CODE>xv</CODE>.
  !+ad_desc  Note that the equality constraints must precede the inequality
  !+ad_desc  constraints in <CODE>conf</CODE>. The constraint gradients
  !+ad_desc  or normals are returned as the columns of <CODE>cnorm</CODE>.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  caller
  !+ad_call  con1
  !+ad_call  funfom
  !+ad_hist  02/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'

  !  Arguments

  integer, intent(in) :: n,m,lcnorm
  real(kind(1.0D0)), dimension(n), intent(in) :: xv
  real(kind(1.0D0)), dimension(n), intent(out) :: fgrd
  real(kind(1.0D0)), dimension(lcnorm,m), intent(out) :: cnorm
  integer, intent(out) :: ifail

  !  Local variables

  integer :: i,j
  real(kind(1.0D0)) :: fbac,ffor
  real(kind(1.0D0)), dimension(ipnvars) :: xfor,xbac,cfor,cbac

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,n

     do j = 1,n
        xfor(j) = xv(j)
        xbac(j) = xv(j)
        if (i == j) then
           xfor(i) = xv(j) * (1.0D0 + epsfcn)
           xbac(i) = xv(j) * (1.0D0 - epsfcn)
        end if
     end do

     !  Evaluate at (x+dx)

     call caller(xfor,n)
     call funfom(ffor)
     call con1(m,cfor)

     !  Evaluate at (x-dx)

     call caller(xbac,n)
     call funfom(fbac)
     call con1(m,cbac)

     !  Calculate finite difference gradients

     fgrd(i) = (ffor-fbac) / (xfor(i)-xbac(i))

     do j = 1,m
        cnorm(i,j) = (cfor(j)-cbac(j)) / (xfor(i)-xbac(i))
     end do

  end do

  !  To stop the program, set ifail < 0 here.

  ifail = 1 * ifail

end subroutine fcnvmc2
