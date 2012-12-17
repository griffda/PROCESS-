!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module function_evaluator

  !+ad_name  function_evaluator
  !+ad_summ  Module containing function evaluators for HYBRD and VMCON
  !+ad_summ  solvers
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  fcnhyb
  !+ad_cont  fcnvmc1
  !+ad_cont  fcnvmc2
  !+ad_cont  funfom
  !+ad_args  N/A
  !+ad_desc  This module contains the function evaluators required
  !+ad_desc  by the two equation solvers in the code.
  !+ad_prob  None
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  heat_transport_variables
  !+ad_call  numerics
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  10/10/12 PJK Initial version of module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  29/10/12 PJK Added pf_power_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  31/10/12 PJK Added cost_variables
  !+ad_hist  17/12/12 PJK Added times_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cost_variables
  use current_drive_variables
  use divertor_variables
  use heat_transport_variables
  use numerics
  use physics_variables
  use pf_power_variables
  use tfcoil_variables
  use times_variables

  implicit none

  public

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fcnhyb(n,xc,rc,iflag)

    !+ad_name  fcnhyb
    !+ad_summ  Function evaluator for EQSOLV
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  n : input integer : Number of equations and unknowns
    !+ad_args  xc(n) : input/output real array : On input XC must contain
    !+ad_argc  an initial estimate of the solution vector. On output XC
    !+ad_argc  contains the final estimate of the solution vector.
    !+ad_args  rc(n) : output real array : Functions evaluated at the output XC
    !+ad_args  iflag : input/output integer : Terminate execution of EQSOLV
    !+ad_argc                                 by setting IFLAG to a negative integer.
    !+ad_desc  This subroutine is the function evaluator for
    !+ad_desc  <A HREF="eqsolv.html">EQSOLV</A> (q.v.).
    !+ad_prob  None
    !+ad_call  caller
    !+ad_call  constraints
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_hist  06/11/12 PJK Renamed routine con1 to constraints
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n
    real(kind(1.0D0)), dimension(n), intent(inout) :: xc
    real(kind(1.0D0)), dimension(n), intent(out) :: rc
    integer, intent(inout) :: iflag

    !  Local variables

    integer :: ncon, nvars

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nvars = neqns
    ncon = neqns

    call caller(xc,nvars)
    call constraints(ncon,rc)

    !  Set iflag < 0 if program is to be terminated here.

    iflag = 1 * iflag

  end subroutine fcnhyb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    !+ad_call  constraints
    !+ad_call  funfom
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  06/11/12 PJK Renamed routine con1 to constraints
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

    call constraints(m,conf)

    !  To stop the program, set ifail < 0 here.

    ifail = 1 * ifail

  end subroutine fcnvmc1

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    !+ad_call  caller
    !+ad_call  constraints
    !+ad_call  funfom
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  06/11/12 PJK Renamed routine con1 to constraints
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

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
       call constraints(m,cfor)

       !  Evaluate at (x-dx)

       call caller(xbac,n)
       call funfom(fbac)
       call constraints(m,cbac)

       !  Calculate finite difference gradients

       fgrd(i) = (ffor-fbac) / (xfor(i)-xbac(i))

       do j = 1,m
          cnorm(i,j) = (cfor(j)-cbac(j)) / (xfor(i)-xbac(i))
       end do

    end do

    !  To stop the program, set ifail < 0 here.

    ifail = 1 * ifail

  end subroutine fcnvmc2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    !+ad_call  None
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  17/12/12 PJK Added new figure of merit 14
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

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

    case (14)  !  pulse length
       fc = sgn * tburn / 2.0D4

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

end module function_evaluator
