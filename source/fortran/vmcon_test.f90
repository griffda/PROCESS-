module vmcon_test
  !  Unit testing program for VMCON  
  !  MDK.  Changed from a "program" to a module.  Issue #1078
  !  Special compilation no longer required.
  use maths_library, only: nearly_equal, vmcon
  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none
  integer :: nfun ! function call counter
  integer :: itest

  !  Choose test to run by changing itest in main program below.
  !  1 to 3 are recommended, others are included to probe the code's
  !  behaviour with different initial guesses for the solution vector x
  !  Expected answers for tests 1 to 3 are given in
  !  VMCON documentation ANL-80-64

  real(dp), dimension(2) :: x_exp
  real(dp), dimension(2) :: c_exp, vlam_exp
  real(dp) :: objf_exp, errlg_exp, errlm_exp
  real(dp) :: errcom_exp, errcon_exp
  integer :: ifail_exp
  ! Issue #1078:-
  integer, parameter :: maxcal = 100
  integer :: neqns
  integer :: nfev2     ! number of calls to FCNVMC1 (VMCON function caller) made
  integer :: nineqns   !  number of inequality constraints VMCON must satisfy
  integer :: nvar      !  number of iteration variables to use
  integer :: nviter    !  number of VMCON iterations performed 
  integer :: ipvlam
  integer, parameter :: ipnvars = 175   ! Local variable
  integer, parameter :: ipeqns = 87     ! Local variable

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_vmcon_test
    !! Initialise module variables
    implicit none

    nfun = 0
    itest = 1
    neqns = 0
    nfev2 = 0
    nineqns = 0
    nvar = 0
    nviter = 0
    ipvlam = 0
  end subroutine init_vmcon_test

  subroutine inittest(nvar,neqns,nineqns,x,ilower,iupper,bndl,bndu)

    implicit none

    integer, intent(out) :: nvar
    integer, intent(out) :: neqns
    integer, intent(out) :: nineqns
    real(dp), dimension(:), intent(out) :: x
    integer, dimension(:), intent(out) :: ilower, iupper
    real(dp), dimension(:), intent(out) :: bndl, bndu
    
    ! itest is a module level variable
    select case (itest)

    case (1)

        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 - 2*x2 + 1 = 0
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
        !
        !  VMCON documentation ANL-80-64

        nvar = 2
        neqns = 1
        nineqns = 1
        x(1) = 2.0D0 ; x(2) = 2.0D0

        !  No bounds on x values set
        ilower(1) = 0 ; ilower(2) = 0
        iupper(1) = 0 ; iupper(2) = 0
        bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

        x_exp(1) = 8.2287565553287513D-01
        x_exp(2) = 9.1143782776643764D-01
        objf_exp = 1.3934649806878849D0
        c_exp(1) = 1.3877787807814457D-17
        c_exp(2) = -7.6716411001598317D-13
        vlam_exp(1) = -1.5944911182523063D0
        vlam_exp(2) = 1.8465914396061125D0
        errlg_exp = 3.3450880954077888D-12
        errlm_exp = 0.0D0
        errcom_exp = 1.4166608063379568D-12
        errcon_exp = 7.6717798780379098D-13
        ifail_exp = 1

    case (2)

        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 - 2*x2 + 1 >= 0
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
        !
        !  VMCON documentation ANL-80-64

        nvar = 2
        neqns = 0
        nineqns = 2
        x(1) = 2.0D0 ; x(2) = 2.0D0

        !  No bounds on x values set
        ilower(1) = 0 ; ilower(2) = 0
        iupper(1) = 0 ; iupper(2) = 0
        bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

        x_exp(1) = 1.6649685472365443D0
        x_exp(2) = 5.5404867491788852D-01
        objf_exp = 3.1111865868328270D-01
        c_exp(1) = 1.5568711974007674D0
        c_exp(2) = -1.0214051826551440D-14
        vlam_exp(1) = 0.0D0
        vlam_exp(2) = 8.0489557193146243D-01
        errlg_exp = 2.3433338602885101D-11
        errlm_exp = 0.0D0
        errcom_exp = 8.2212450866697197D-15
        errcon_exp = 1.0214051826551440D-14
        ifail_exp = 1

    case (3)

        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 + x2 - 3 = 0
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
        !
        !  Note that this test is supposed to fail with ifail=5
        !  as there is no feasible solution
        !
        !  VMCON documentation ANL-80-64

        nvar = 2
        neqns = 1
        nineqns = 1
        x(1) = 2.0D0 ; x(2) = 2.0D0

        !  No bounds on x values set
        ilower(1) = 0 ; ilower(2) = 0
        iupper(1) = 0 ; iupper(2) = 0
        bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

        x_exp(1) = 2.3999994310874733D0
        x_exp(2) = 6.0000056891252611D-01
        objf_exp = 3.1999908974060504D-01
        c_exp(1) = -6.6613381477509392D-16
        c_exp(2) = -8.000000000004035D-01
        vlam_exp(1) = 0.0D0
        vlam_exp(2) = 0.0D0
        errlg_exp = 1.599997724349894D0
        errlm_exp = 0.0D0
        errcom_exp = 0.0D0
        errcon_exp = 8.0000000000040417D-01
        ifail_exp = 5

    case (4)

        !  Maximise f(x1,x2) = x1 + x2
        !  subject to the following constraint:
        !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0
        !
        !  http://en.wikipedia.org/wiki/Lagrange_multiplier

        nvar = 2
        neqns = 1
        nineqns = 0

        !  N.B. results can flip to minimum instead of maximum
        !  if x(1), x(2) are initialised at different points...
        x(1) = 1.0D0 ; x(2) = 1.0D0

        !  No bounds on x values set
        ilower(1) = 0 ; ilower(2) = 0
        iupper(1) = 0 ; iupper(2) = 0
        bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

        x_exp(1) = 0.5D0*sqrt(2.0D0)
        x_exp(2) = 0.5D0*sqrt(2.0D0)
        objf_exp = sqrt(2.0D0)
        c_exp(1) = 0.0D0
        vlam_exp(1) = 1.0D0/sqrt(2.0D0)
        errlg_exp = 0.0D0
        errlm_exp = 0.0D0
        errcom_exp = 0.0D0
        errcon_exp = 0.0D0
        ifail_exp = 1

    case (5)

        !  Intersection of parabola x^2 with straight line 2x+3
        !  Unorthodox (and not recommended) method to find the root
        !  of an equation.
        !
        !  Maximise f(x1) = x1**2
        !  subject to the following constraint:
        !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0
        !
        !  Solutions to c1(x1) are x1 = -1 and x1 = 3, and depending on
        !  the initial guess for x1 either (or neither...) solution might
        !  be found. Since there is one constraint equation with one unknown
        !  the code cannot optimise properly.

        nvar = 1
        neqns = 1
        nineqns = 0

        x(1) = 5.0D0  !  Try different values, e.g. 5.0, 2.0, 1.0, 0.0...

        !  No bounds on x values set
        ilower(1) = 0
        iupper(1) = 0
        bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

        x_exp(1) = 3.0
        objf_exp = 9.0D0
        c_exp(1) = 0.0D0
        vlam_exp(1) = 1.5D0
        errlg_exp = 0.0D0
        errlm_exp = 0.0D0
        errcom_exp = 0.0D0
        errcon_exp = 0.0D0
        ifail_exp = 1

    end select

  end subroutine inittest

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine objfn(n,m,x,objf,conf, ifail)
      ! variable itest cannot be passed here as the argument format is fixed by VMCON.
      ! itest is a module-level variable.

    implicit none
    integer, intent(in) :: n,m
    real(dp), dimension(n), intent(in) :: x
    real(dp), intent(out) :: objf
    real(dp), dimension(m), intent(out) :: conf
    integer, intent(inout) :: ifail
    ! ifail is not used here, but is required to avoid an interface mismatch
    ! when calling vmcon with objfn as an argument
    select case (itest)

    case (1,2)

        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 - 2*x2 + 1 = 0  (itest = 1)
        !  c1(x1,x2) = x1 - 2*x2 + 1 >= 0 (itest = 2)
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

        objf = (x(1) - 2.0D0)**2 + (x(2) - 1.0D0)**2

        conf(1) = x(1) - 2.0D0*x(2) + 1.0D0
        conf(2) = -0.25D0*x(1)**2 - x(2)*x(2) + 1.0D0

    case (3)

        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 + x2 - 3 = 0
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

        objf = (x(1) - 2.0D0)**2 + (x(2) - 1.0D0)**2

        conf(1) = x(1) + x(2) - 3.0D0
        conf(2) = -0.25D0*x(1)**2 - x(2)*x(2) + 1.0D0

    case (4)

        !  From Wikipedia: Lagrange Multiplier article
        !  Maximise f(x1,x2) = x1 + x2
        !  subject to the following constraint:
        !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0

        objf = x(1) + x(2)

        conf(1) = x(1)*x(1) + x(2)*x(2) - 1.0D0

    case (5)

        !  Intersection of parabola x^2 with straight line 2x+3
        !  Maximise f(x1) = x1**2
        !  subject to the following constraint:
        !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

        objf = x(1)*x(1)

        conf(1) = x(1)*x(1) - 2.0D0*x(1) - 3.0D0

    end select

    nfun = nfun + 1

  end subroutine objfn

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dobjfn(n,m,x,fgrd,cnorm,lcnorm,ifail)

    implicit none

    !  Arguments

    integer, intent(in) :: n,m,lcnorm
    real(dp), dimension(n), intent(in) :: x
    real(dp), dimension(n), intent(out) :: fgrd
    real(dp), dimension(lcnorm,m), intent(out) :: cnorm
    integer, intent(inout) :: ifail
    ifail = 0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  fgrd(1...n) are the gradients of the objective function f
    !  with respect to x1...xn

    !  cnorm(1...n, 1...m) are the gradients of the constraints 1...m
    !  with respect to x1...xn

    select case (itest)

    case (1,2)
        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 - 2*x2 + 1 = 0  (itest = 1)
        !  c1(x1,x2) = x1 - 2*x2 + 1 >= 0 (itest = 2)
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

        fgrd(1) = 2.0D0*(x(1) - 2.0D0)
        fgrd(2) = 2.0D0*(x(2) - 1.0D0)

        cnorm(1,1) = 1.0D0
        cnorm(2,1) = -2.0D0
        cnorm(1,2) = -0.5D0*x(1)
        cnorm(2,2) = -2.0D0*x(2)

    case (3)
        !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
        !  subject to the following constraints:
        !  c1(x1,x2) = x1 + x2 - 3 = 0
        !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

        fgrd(1) = 2.0D0
        fgrd(2) = 2.0D0*(x(2) - 1.0D0)

        cnorm(1,1) = 1.0D0
        cnorm(2,1) = 1.0D0
        cnorm(1,2) = -0.5D0*x(1)
        cnorm(2,2) = -2.0D0*x(2)

    case (4)
        !  From Wikipedia: Lagrange Multiplier article
        !  Maximise f(x1,x2) = x1 + x2
        !  subject to the following constraint:
        !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0

        fgrd(1) = 1.0D0
        fgrd(2) = 1.0D0

        cnorm(1,1) = 2.0D0*x(1)
        cnorm(2,1) = 2.0D0*x(2)

    case (5)

        !  Intersection of parabola x^2 with straight line 2x+3
        !  Maximise f(x1) = x1**2
        !  subject to the following constraint:
        !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

        fgrd(1) = 2.0D0*x(1)

        cnorm(1,1) = 2.0D0*x(1) - 2.0D0

    end select

  end subroutine dobjfn

  subroutine run_vmcon_test(test_index, pass)
    !  Change the test being run by modifying the value of test_index
    implicit none  
    integer, intent(in) :: test_index
    logical, intent(out) :: pass
    
    integer :: ifail = 1
    real(dp) :: objf
    integer :: ii,jj,lb,lcnorm,ldel,lh,liwa,lwa,m,meq,mode,n
    integer, parameter :: ippn1 = ipnvars+1
    integer, parameter :: ipldel = 7*ippn1
    integer, parameter :: iplh  = 2*ippn1
    integer, parameter :: ipvmu = ipeqns+2*ipnvars+1
    integer, parameter :: ipvlam = ipeqns+2*ipnvars+1
    integer, parameter :: ipliwa = 6*ippn1+ipeqns
    integer, dimension(ipliwa) :: iwa
    integer, dimension(ipnvars) :: ilower,iupper
    real(dp) :: xtol
    real(dp) :: sum
    real(dp), dimension(ipnvars) :: bdelta,bndl,bndu,etav,fgrd, gammv,glag,glaga,xa,xv
    real(dp), dimension(ipeqns) :: cm,conf
    real(dp), dimension(ippn1) :: bdl,bdu,gm
    real(dp), dimension(ipvmu) :: vmu
    real(dp), dimension(ipldel) :: delta
    real(dp), dimension(iplh) :: wa
    real(dp), dimension(ippn1,ipeqns) :: cnorm
    real(dp), dimension(ippn1,ippn1) :: b
    real(dp), dimension(iplh,iplh) :: h
    real(dp) :: summ,errlg,errlm,errcom,errcon
    real(dp), dimension(ipvlam) :: vlam
    
    ! itest is a module level variable
    itest = test_index
    call inittest(nvar,neqns,nineqns,xv,ilower,iupper,bndl,bndu)

    n = nvar
    m = neqns+nineqns
    meq = neqns
    !xtol = epsvmc   Issue #1078
    xtol = 1.0D-8
    mode = 0
    lb = ippn1
    lcnorm = ippn1
    ldel = ipldel
    lh = iplh
    lwa = iplh
    liwa = ipliwa

    !write(*,*) 'Initial solution estimate:'
    !do ii = 1,n
    !   write(*,*) 'x(',ii,') = ',xv(ii)
    !end do

    call vmcon(objfn,dobjfn,mode,n,m,&
              meq,xv,objf,fgrd,conf, &
              cnorm,lcnorm,b,lb,xtol,&
              maxcal,ifail,nfev2,nviter,vlam, &
              glag,vmu,cm,glaga,gammv,&
              etav,xa,bdelta,delta,ldel, &
              gm,bdl,bdu,h,lh,&
              wa,lwa,iwa,liwa,ilower, &
              iupper,bndl,bndu,sum)

    
    pass = (ifail==ifail_exp)
    pass = pass.and.nearly_equal(objf, objf_exp,1.d-8)        ! Final objective function value: calculated vs expected
    do ii = 1,n  
        pass = pass.and.nearly_equal(xv(ii),x_exp(ii),1.d-8)   ! Final solution estimate: calculated vs expected
    end do

    ! Some tests are expected to return values of ifail not equal to 1.  
    ! These just need to return the correct value of ifail.
    pass = pass.or.((ifail==ifail_exp).and.(ifail/=1))

    if(pass)then
        write(*,*)
        write(*,*)'VMCON test ', itest, 'PASSED'
        if(itest==3)write(*,*)'(Test 3 is not supposed to converge.)'
    else
        write(*,*)'VMCON test ', itest, 'FAILED'
        write(*,*) 'ifail = ', ifail, '(expected value = ',ifail_exp,')'
        write(*,*) 'Number of function evaluations = ',nfun
        write(*,*)
        write(*,*) 'Final solution estimate: calculated vs expected'
        do ii = 1,n  
              pass = pass.and.nearly_equal(xv(ii),x_exp(ii),1.d-8) 
              write(*,*) 'x(',ii,') = ',xv(ii),x_exp(ii)
        end do
        write(*,*)
        write(*,*) 'Final objective function value: calculated vs expected',objf,objf_exp
        write(*,*)
        write(*,*) 'Constraints evaluated at x: calculated vs expected'
        do ii = 1,m
          write(*,*) conf(ii), c_exp(ii)
        end do
        write(*,*)

        write(*,*) 'Lagrange multiplier estimates: calculated vs expected'
        do ii = 1,m
          write(*,*) vlam(ii), vlam_exp(ii)
        end do
        write(*,*)

        write(*,*) 'Lagrangian gradient error: calculated vs expected'
        errlg = 0.0D0
        do ii = 1,n
        summ = fgrd(ii)
        do jj = 1,m
          summ = summ - vlam(jj)*cnorm(ii,jj)
        end do
        errlg = errlg + abs(summ)
        end do
        write(*,*) errlg, errlg_exp
        write(*,*)

        write(*,*) 'Lagrange multiplier error: calculated vs expected'
        errlm = 0.0D0
        do ii = 1,m
          if ((ii <= meq).or.(vlam(ii) >= 0.0D0)) cycle
          errlm = errlm + abs(vlam(ii))
        end do
        write(*,*) errlm, errlm_exp
        write(*,*)

        write(*,*) 'Complementarity error: calculated vs expected'
        errcom = 0.0D0
        do ii = 1,m
          errcom = errcom + abs(vlam(ii)*conf(ii))
        end do
        write(*,*) errcom, errcom_exp
        write(*,*)

        write(*,*) 'Constraint error: calculated vs expected'
        errcon = 0.0D0
        do ii = 1,m
          if ((ii > meq).and.(conf(ii) >= 0.0D0)) cycle
          errcon = errcon + abs(conf(ii))
        end do
        write(*,*) errcon, errcon_exp
        write(*,*)
    endif

  end subroutine run_vmcon_test
end module vmcon_test