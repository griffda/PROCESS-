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
  !+ad_call  constraints
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  stellarator_variables
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
  !+ad_hist  19/05/14 PJK Added ife_variables, stellarator_variables
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  28/07/14 PJK Added constraints
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraints
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use heat_transport_variables
  use ife_variables
  use numerics
  use physics_variables
  use pf_power_variables
  use process_output
  use stellarator_variables
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
    !+ad_call  constraint_eqns
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_hist  06/11/12 PJK Renamed routine con1 to constraints
    !+ad_hist  17/12/13 PJK Added new argument to constraints call
    !+ad_hist  28/07/14 PJK Modified constraints call
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
    call constraint_eqns(ncon,rc,-1)

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
    !+ad_args  ifail   : input/output integer  : error flag, if < 0 stops calculation
    !+ad_desc  This routine is the function evaluator for the VMCON
    !+ad_desc  maximisation/minimisation routine.
    !+ad_desc  <P>It calculates the objective and constraint functions at the
    !+ad_desc  n-dimensional point of interest <CODE>xv</CODE>.
    !+ad_desc  Note that the equality constraints must precede the inequality
    !+ad_desc  constraints in <CODE>conf</CODE>.
    !+ad_prob  None
    !+ad_call  caller
    !+ad_call  constraint_eqns
    !+ad_call  funfom
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  06/11/12 PJK Renamed routine con1 to constraints
    !+ad_hist  17/01/13 PJK Corrected ifail to be input/output
    !+ad_hist  17/12/13 PJK Added new argument to constraints call
    !+ad_hist  06/02/14 PJK Added second call to caller to aid initialisation
    !+ad_hist  19/05/14 PJK Added tburn consistency check
    !+ad_hist  28/07/14 PJK Modified constraints call
    !+ad_hist  10/09/14 PJK Added vfile output
    !+ad_hist  01/04/15 JM  Reduced tburn consistency error output
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
    integer, intent(inout) :: ifail

    !  Local variables

    real(kind(1.0D0)) :: fbac,ffor,summ,sqsumconfsq
    logical :: first_call = .true.
    integer :: ii, loop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Evaluate machine parameters at xv

    call caller(xv,n)

    !  To ensure that, at the start of a run, all physics/engineering
    !  variables are fully initialised with consistent values, we perform
    !  a second evaluation call here

    if (first_call) then
       call caller(xv,n)
       first_call = .false.
    end if

    !  Convergence loop to ensure burn time consistency

    if ((istell /= 1).and.(ife /= 1)) then
       loop = 0
       do while ( (loop < 10).and. &
            (abs((tburn-tburn0)/max(tburn,0.01D0)) > 0.001D0) )
          loop = loop+1
          call caller(xv,n)
          if (verbose == 1) then
              write(*, '(a, 2e10.3)') 'Internal tburn consistency check: ',tburn,tburn0
          end if
       end do
       if (loop >= 10) then
            write(*,*) 'Burn time values are not consistent in iteration: ', nviter
            write(*,*) 'tburn,tburn0: ',tburn,tburn0
       end if
    end if

    !  Evaluate figure of merit (objective function)

    call funfom(objf)

    !  Evaluate constraint equations

    call constraint_eqns(m,conf,-1)

    !  To stop the program, set ifail < 0 here.

    ifail = 1 * ifail

    !  Verbose diagnostics

    if (verbose == 1) then
       summ = 0.0D0
       do ii = 1,m
          summ = summ + conf(ii)*conf(ii)
       end do
       sqsumconfsq = sqrt(summ)
       write(vfile,'(3i13,100es13.5)') nviter, (1-mod(ifail,7))-1, &
            mod(nviter,2)-1,te,coe,rmajor,powfmw,bt,tburn,sqsumconfsq,xv
    end if

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
    !+ad_args  ifail   : input/output integer  : error flag, if < 0 stops calculation
    !+ad_desc  This routine is the gradient function evaluator for the VMCON
    !+ad_desc  maximisation/minimisation routine.
    !+ad_desc  <P>It calculates the gradients of the objective and constraint
    !+ad_desc  functions at the n-dimensional point of interest <CODE>xv</CODE>.
    !+ad_desc  Note that the equality constraints must precede the inequality
    !+ad_desc  constraints in <CODE>conf</CODE>. The constraint gradients
    !+ad_desc  or normals are returned as the columns of <CODE>cnorm</CODE>.
    !+ad_prob  None
    !+ad_call  caller
    !+ad_call  constraint_eqns
    !+ad_call  funfom
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  06/11/12 PJK Renamed routine con1 to constraints
    !+ad_hist  17/01/13 PJK Corrected ifail to be input/output
    !+ad_hist  17/12/13 PJK Added new argument to constraints call
    !+ad_hist  24/04/14 PJK Corrected problem with final evaluation
    !+ad_hist  28/07/14 PJK Modified constraints call
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
    integer, intent(inout) :: ifail

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
       call constraint_eqns(m,cfor,-1)

       !  Evaluate at (x-dx)

       call caller(xbac,n)
       call funfom(fbac)
       call constraint_eqns(m,cbac,-1)

       !  Calculate finite difference gradients

       fgrd(i) = (ffor-fbac) / (xfor(i)-xbac(i))

       do j = 1,m
          cnorm(i,j) = (cfor(j)-cbac(j)) / (xfor(i)-xbac(i))
       end do

    end do

    !  Additional evaluation call to ensure that final result is consistent
    !  with the correct iteration variable values.
    !  If this is not done, the value of the nth (i.e. final) iteration
    !  variable in the solution vector is inconsistent with its value
    !  shown elsewhere in the output file, which is a factor (1-epsfcn)
    !  smaller (i.e. its xbac value above).

    call caller(xv,n)

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
    !+ad_call  report_error
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  17/12/12 PJK Added new figure of merit 14
    !+ad_hist  25/06/13 PJK Added kind() to sgn assignment
    !+ad_hist  12/02/14 PJK Added new figure of merit 15
    !+ad_hist  13/02/14 PJK Added trap if iavail /= 1 with fig of merit 15
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  06/10/14 PJK Added orbit loss power
    !+ad_hist  18/11/15  RK Major radius/burn time optimiser added
    !+ad_hist  10/12/15  RK Net electrical output added as FoM
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
    sgn = sign(1.0D0, real(minmax, kind(1.0D0)))

    !  If sgn is -1 the value of fc will be maximised
    !  If sgn is +1 the value of fc will be minimised

    select case (iab)

    case (1)  !  major radius
       fc = sgn * 0.2D0 * rmajor

    case (2)  !  fusion power / input power
        write(*,*) 'Figure of merit 2 (fusion power / input power) is not used.'
        write(*,*) 'Figure of merit 5 (fusion gain Q) is available.'
        stop
       ! fc = sgn * powfmw / (pinjmw + porbitlossmw + tfcpmw + ppump/1.0D6)

    case (3)  !  neutron wall load
       fc = sgn * wallmw

    case (4)  !  TF coil + PF coil power
       fc = sgn * (tfcmw + 1.0D-3*srcktpm)/10.0D0

    case (5)  !  fusion power / injection power
       fc = sgn * powfmw / pinjmw

    case (6)  !  cost of electricity
       fc = sgn * coe/100.0D0

    case (7)  !  direct/constructed/capital cost
       if (ireactor == 0) then
          fc = sgn * cdirt/1.0D3
       else
          fc = sgn * concost/1.0D4
       end if

    case (8)  !  aspect ratio
       fc = sgn * aspect

    case (9)  !  divertor heat load
       fc = sgn * hldiv

    case (10)  !  toroidal field on axis
       fc = sgn * bt

    case (11)  !  injection power
       fc = sgn * pinjmw

    case (12)  !  hydrogen production capital cost
       ! #506 OBSOLETE
       write(*,*) 'Figure of Merit 13 (Hydrogen production) is no longer supported.'
       stop
    case (13)  !  hydrogen production rate
       ! #506 OBSOLETE
       write(*,*) 'Figure of Merit 13 (Hydrogen production) is no longer supported.'
       stop

    case (14)  !  pulse length
       fc = sgn * tburn / 2.0D4

    case (15)  !  plant availability factor (N.B. requires iavail = 1)

       if (iavail /= 1) call report_error(23)

       fc = sgn * cfactr

    case (16)  !  major radius/burn time
       fc = sgn * ( 0.95d0 * (rmajor/9.0d0) + 0.05d0 * (7200.d0/tburn) )

    case (17)  !  net electrical output
       fc = sgn * pnetelmw / 500.0d0

    case default
       idiags(1) = iab ; call report_error(24)

    end select

    !  Crude method of catching NaN errors

    if ((abs(fc) > 9.99D99).or.(fc /= fc)) then
       idiags(1) = iab ; call report_error(25)
    end if

  end subroutine funfom

end module function_evaluator
