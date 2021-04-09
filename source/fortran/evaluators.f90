! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module function_evaluator

  !! Module containing function evaluators for HYBRD and VMCON
  !! solvers
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains the function evaluators required
  !! by the two equation solvers in the code.

  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  public
  
  logical :: first_call
  !! First call flag for subroutine fcnvmc1

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_function_evaluator
    !! Initialise module variables

    first_call = .true.
  end subroutine init_function_evaluator

  subroutine fcnhyb(n,xc,rc,iflag)

    !! Function evaluator for EQSOLV
    !! author: P J Knight, CCFE, Culham Science Centre
    !! n : input integer : Number of equations and unknowns
    !! xc(n) : input/output real array : On input XC must contain
    !! an initial estimate of the solution vector. On output XC
    !! contains the final estimate of the solution vector.
    !! rc(n) : output real array : Functions evaluated at the output XC
    !! iflag : input/output integer : Terminate execution of EQSOLV
    !! by setting IFLAG to a negative integer.
    !! This subroutine is the function evaluator for
    !! <A HREF="eqsolv.html">EQSOLV</A> (q.v.).
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constraints, only: constraint_eqns 
    use numerics, only: neqns 
    implicit none

    !  Arguments

    integer, intent(in) :: n
    real(dp), dimension(n), intent(inout) :: xc
    real(dp), dimension(n), intent(out) :: rc
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

  subroutine fcnvmc1(n,m,xv,objf,conf,ifail_in, ifail_out)
    !! Function evaluator for VMCON
    !! author: P J Knight, CCFE, Culham Science Centre
    !! n       : input integer     : number of variables
    !! m       : input integer     : number of constraints
    !! xv(n)   : input real array  : scaled variable values
    !! objf    : output real       : objective function
    !! conf(m) : output real array : constraint functions
    !! ifail_in: input integer     : error flag, if < 0 stops calculation
    !! ifail_out: output integer   : error flag, if < 0 stops calculation
    !! This routine is the function evaluator for the VMCON
    !! maximisation/minimisation routine.
    !! <P>It calculates the objective and constraint functions at the
    !! n-dimensional point of interest <CODE>xv</CODE>.
    !! Note that the equality constraints must precede the inequality
    !! constraints in <CODE>conf</CODE>.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use global_variables, only: verbose
		use constants, only: vfile
		use constraints, only: constraint_eqns 
		use cost_variables, only: coe 
		use numerics, only: nviter 
		use physics_variables, only: te ,rmajor ,powfmw ,bt 
		use stellarator_variables, only: istell 
		use times_variables, only: tburn0, tburn
    implicit none

    !  Arguments

    integer, intent(in) :: n,m
    real(dp), dimension(n), intent(in) :: xv
    real(dp), intent(out) :: objf
    real(dp), dimension(m), intent(out) :: conf
    integer, intent(in) :: ifail_in
    integer, intent(out) :: ifail_out

    !  Local variables

    real(dp) :: summ,sqsumconfsq
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

    if (istell == 0) then
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
    ! #TODO Not sure this serves any purpose
    ifail_out = 1 * ifail_in

    !  Verbose diagnostics

    if (verbose == 1) then
       summ = 0.0D0
       do ii = 1,m
          summ = summ + conf(ii)*conf(ii)
       end do
       sqsumconfsq = sqrt(summ)
       write(vfile,'(3i13,100es13.5)') nviter, (1-mod(ifail_out,7))-1, &
            mod(nviter,2)-1,te,coe,rmajor,powfmw,bt,tburn,sqsumconfsq,xv
    end if

  end subroutine fcnvmc1

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fcnvmc2(n,m,xv,fgrd,cnorm,lcnorm, ifail_in, ifail_out)

    !! Gradient function evaluator for VMCON
    !! author: P J Knight, CCFE, Culham Science Centre
    !! n       : input integer     : number of variables
    !! m       : input integer     : number of constraints
    !! xv(n)   : input real array  : scaled variable values
    !! fgrd(n) : output real array : gradient of the objective function
    !! cnorm(lcnorm,m) : output real array : constraint gradients, i.e.
    !! cnorm(i,j) is the derivative of constraint j w.r.t. variable i
    !! lcnorm  : input integer     : number of columns in cnorm
    !! ifail_in: input integer     : error flag, if < 0 stops calculation
    !! ifail_out: output integer   : error flag, if < 0 stops calculation
    !! This routine is the gradient function evaluator for the VMCON
    !! maximisation/minimisation routine.
    !! <P>It calculates the gradients of the objective and constraint
    !! functions at the n-dimensional point of interest <CODE>xv</CODE>.
    !! Note that the equality constraints must precede the inequality
    !! constraints in <CODE>conf</CODE>. The constraint gradients
    !! or normals are returned as the columns of <CODE>cnorm</CODE>.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use constraints, only: constraint_eqns 
		use numerics, only: ipnvars, epsfcn
    implicit none

    !  Arguments

    integer, intent(in) :: n,m,lcnorm
    real(dp), dimension(n), intent(in) :: xv
    real(dp), dimension(n), intent(out) :: fgrd
    real(dp), dimension(lcnorm,m), intent(out) :: cnorm
    integer, intent(in) :: ifail_in
    integer, intent(out) :: ifail_out

    !  Local variables

    integer :: i,j
    real(dp) :: fbac,ffor
    real(dp), dimension(ipnvars) :: xfor,xbac,cfor,cbac

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

    ! #TODO Not sure this serves any purpose
    ifail_out = 1 * ifail_in

  end subroutine fcnvmc2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine funfom(fc)

    !! Objective function evaluator for VMCON
    !! author: P J Knight, CCFE, Culham Science Centre
    !! fc : output real : value of objective function at the output point
    !! This routine evaluates the value of the objective function
    !! i.e. the (normalised) figure-of-merit, at the nvar-dimensional
    !! point of interest.
    !! <P>Each equation for <CODE>fc<CODE> gives a value of the
    !! order of unity for the sake of the numerics.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use global_variables, only: xlabel, iscan_global
		use constants, only: nout, iotty, mfile
		use constraints, only: constraint_eqns 
		use cost_variables, only: concost, cfactr, cdirt, ireactor, iavail, coe 
		use current_drive_variables, only: bigq, porbitlossmw, pinjmw
		use divertor_variables, only: hldiv
		use error_handling, only: idiags, fdiags, errors_on, report_error
		use heat_transport_variables, only: pnetelmw 
    use numerics, only: minmax 
		use physics_variables, only: powfmw, bt, rmajor, wallmw, aspect, pohmmw
		use pf_power_variables, only: srcktpm 
		use process_output, only: int_to_string3 
		use tfcoil_variables, only: tfcmw 
		use times_variables, only: tburn
    implicit none

    !  Arguments

    real(dp), intent(out) :: fc
!    real(c_double), intent(out) :: fc
    !  Local variables

    integer :: iab
    real(dp) :: sgn

!        write(*,*) 'Figure of merit 2 (fusion power / input power) is not used.'
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
        stop 1
       ! fc = sgn * powfmw / (pinjmw + porbitlossmw + tfcpmw + ppump/1.0D6)

    case (3)  !  neutron wall load
       fc = sgn * wallmw

    case (4)  !  TF coil + PF coil power
       fc = sgn * (tfcmw + 1.0D-3*srcktpm)/10.0D0

   case (5)  !  Q = fusion gain  Issue #540
       fc = sgn * powfmw / (pinjmw + porbitlossmw + pohmmw)
       !fc = sgn * powfmw / pinjmw

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
       stop 1
    case (13)  !  hydrogen production rate
       ! #506 OBSOLETE
       write(*,*) 'Figure of Merit 13 (Hydrogen production) is no longer supported.'
       stop 1

    case (14)  !  pulse length
       fc = sgn * tburn / 2.0D4

    case (15)  !  plant availability factor (N.B. requires iavail = 1)

       if (iavail /= 1) call report_error(23)

       fc = sgn * cfactr

    case (16)  !  major radius/burn time
       fc = sgn * ( 0.95d0 * (rmajor/9.0d0) - 0.05d0 * (tburn/7200.d0) )

    case (17)  !  net electrical output
       fc = sgn * pnetelmw / 500.0d0

   case (18)  !  Null figure of merit
      fc = 1d0
   
   case (19)  !  major radius/burn time
      fc = sgn * ( -0.5d0 * (bigq/20.0D0) - 0.5d0 * (tburn/7200.d0) )

    case default
       idiags(1) = iab ; call report_error(24)

    end select

    !  Crude method of catching NaN errors

    if ((abs(fc) > 9.99D99).or.(fc /= fc)) then
       idiags(1) = iab ; call report_error(25)
    end if

  end subroutine funfom

end module function_evaluator
