module vmcon_module
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  ! Module variables that are set using the arguments to subroutine vmcon
  integer :: mode,n,m,meq,lcnorm,lb,maxfev,ldel,lh,lwa,liwa
  integer :: info,nfev,niter
  integer, dimension(:), allocatable :: iwa
  integer, dimension(:), allocatable :: ilower,iupper
  real(dp) :: objf
  real(dp) :: tol
  real(dp), dimension(:), allocatable :: x
  real(dp), dimension(:), allocatable :: bndl,bndu
  real(dp), dimension(:), allocatable :: fgrd
  real(dp), dimension(:), allocatable :: conf
  real(dp), dimension(:), allocatable :: glag,glaga,gamma,eta,xa,bdelta
  real(dp), dimension(:), allocatable :: cm
  real(dp), dimension(:), allocatable :: delta
  real(dp), dimension(:), allocatable :: wa
  real(dp), dimension(:,:), allocatable :: cnorm
  real(dp), dimension(:,:), allocatable :: h
  real(dp), dimension(:,:), allocatable :: b
  real(dp), dimension(:), allocatable :: vlam,vmu,gm,bdl,bdu
  real(dp) :: sum
  
  ! Other module variables, originally local to subroutine vmcon
  real(dp), dimension(:), allocatable :: best_solution_vector
  real(dp), dimension(:), allocatable :: delta_var           ! For opt data extraction only
  integer :: i,j,k,mact,nfinit,nls,np1,np1j,npp,nqp,nsix,nsixi
  integer :: inx,ki,ml,mlp1,mcon,mp1,mpn,mpnpp1,mpnppn
  real(dp) :: alpha,aux,auxa,calpha,dbd,dflsa,dg, &
    fls,flsa,spgdel,temp,thcomp,theta
  real(dp) :: summ, sqsumsq, sqsumsq_tol
  real(dp) :: lowest_valid_fom    
  real(dp), parameter :: zero = 0.0D0
  real(dp), parameter :: cp1 = 0.1D0
  real(dp), parameter :: cp2 = 0.2D0
  real(dp), parameter :: cp5 = 0.5D0
  real(dp), parameter :: one = 1.0D0
  character(len=20) :: iteration_progress

  ! Var from subroutine vmcon requiring re-initialisation on each new run
  real(dp) :: best_sum_so_far
  
  ! Exit code for module subroutines: determines when to return completely from 
  ! subroutine vmcon
  integer :: exit_code
  
  ! Abstract interfaces to allow pointers to external subroutines
  abstract interface
    subroutine fcnvmc1_int(n,m,xv,objf,conf,ifail)
      use, intrinsic :: iso_fortran_env, only: dp=>real64
      integer, intent(in) :: n,m
      real(dp), dimension(n), intent(in) :: xv
      real(dp), intent(out) :: objf
      real(dp), dimension(m), intent(out) :: conf
      integer, intent(inout) :: ifail
    end subroutine fcnvmc1_int
  end interface
    
  abstract interface
    subroutine fcnvmc2_int(n,m,xv,fgrd,cnorm,lcnorm,ifail)
      use, intrinsic :: iso_fortran_env, only: dp=>real64
      integer, intent(in) :: n,m,lcnorm
      real(dp), dimension(n), intent(in) :: xv
      real(dp), dimension(n), intent(out) :: fgrd
      real(dp), dimension(lcnorm,m), intent(out) :: cnorm
      integer, intent(inout) :: ifail
    end subroutine fcnvmc2_int
  end interface
    
  ! Pointers to objfn and dobjfn
  procedure(fcnvmc1_int), pointer :: fcnvmc1_ptr
  procedure(fcnvmc2_int), pointer :: fcnvmc2_ptr

  ! Format string
  character(len=20), parameter :: fmt_str = "(a,1pe10.3)"

contains
  subroutine init_vmcon_module
    !! Initialise module variables
    implicit none

    best_sum_so_far = 999d0
  end subroutine init_vmcon_module

  subroutine vmcon(fcnvmc1_,fcnvmc2_,mode_,n_,m_,&
    meq_,x_,objf_,fgrd_,conf_,&
    cnorm_,lcnorm_,b_,lb_,tol_,&
    maxfev_,info_,nfev_,niter_,vlam_,& 
    glag_,vmu_,cm_,glaga_,gamma_,&
    eta_,xa_,bdelta_,delta_,ldel_,&
    gm_,bdl_,bdu_,h_,lh_,&
    wa_,lwa_,iwa_,liwa_,ilower_,&
    iupper_,bndl_,bndu_,sum_)
    !! Calculates the least value of a function of several variables
    !! subject to linear and/or nonlinear equality and inequality
    !! constraints
    !! author: R L Crane, K E Hillstrom, M Minkoff, Argonne National Lab
    !! author: J Galambos, FEDC/ORNL
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: M D Kovari, CCFE, Culham Science Centre
    !! fcnvmc1_ : external subroutine : routine to calculate the
    !! objective and constraint functions
    !! fcnvmc2_ : external subroutine : routine to calculate the
    !! gradients of the objective and constraint functions
    !! mode_ : input integer : 1 if B is provided by the user,
    !! 0 if B is to be initialized to
    !! the identity matrix
    !! n_ : input integer : number of variables
    !! m_ : input integer : number of constraints
    !! meq_ : input integer : number of equality constraints
    !! x_(n) : input/output real array : initial/final estimates of
    !! solution vector
    !! objf_ : output real : objective function at output X
    !! fgrd_(n) : output real array : components of the gradient of
    !! the objective function at output X
    !! conf_(m) : output real array : values of the constraint
    !! functions at output X (equality constraints must precede
    !! the inequality constraints)
    !! cnorm_(lcnorm,m) : output real array : constraint normals at
    !! output X
    !! lcnorm_ : input integer : array dimension, >= n+1
    !! b_(lb,lb) : input/output real array : approximation to the
    !! second derivative matrix of the Lagrangian function.
    !! Often, an adequate initial B matrix can be obtained by
    !! approximating the hessian of the objective function.
    !! On input, the approximation is provided by the user if
    !! MODE = 1 and is set to the identity matrix if MODE = 0.
    !! lb_ : input integer : array dimension, >= n+1
    !! tol_ : input real : A normal return occurs when the
    !! objective function plus suitably weighted multiples
    !! of the constraint functions are predicted to differ from
    !! their optimal values by at most TOL.
    !! maxfev_ : input integer : maximum number of calls to FCNVMC1
    !! info_ : output integer : error flag<BR>
    !! INFO < 0 : user termination<BR>
    !! INFO = 0 : improper input parameters<BR>
    !! INFO = 1 : normal return<BR>
    !! INFO = 2 : number of calls to FCNVMC1 is at least MAXFEV<BR>
    !! INFO = 3 : line search required ten calls of FCNVMC1<BR>
    !! INFO = 4 : uphill search direction was calculated<BR>
    !! INFO = 5 : quadratic programming technique was unable to find
    !! a feasible point<BR>
    !! INFO = 6 : quadratic programming technique was restricted by
    !! an artificial bound or failed due to a singular
    !! matrix
    !! INFO = 7 : line search has been aborted
    !! nfev_ : output integer : number of calls to FCNVMC1
    !! niter_ : output integer : number of iterations
    !! vlam_(m+2n+1) : output real array : Lagrange multipliers at output X.
    !! The Lagrange multipliers provide the sensitivity of the objective
    !! function to changes in the constraint functions.
    !! Note that VLAM(M+I), I=1,...,N gives the multipliers for
    !! the lower bound constraints.  VLAM(M+N+1+I), I=1,...,N
    !! gives the multipliers for the upper bound constraints.
    !! glag_(n) : output real array : components of the gradient of
    !! the Lagrangian function at the output X.
    !! cm_(m) : output real array : work array
    !! vmu_(m+2n+1) : output real array : work array
    !! glaga_(n) : output real array : work array
    !! gamma_(n) : output real array : work array
    !! eta_(n) : output real array : work array
    !! xa_(n) : output real array : work array
    !! bdelta_(n) : output real array : work array
    !! delta_(ldel) : output real array : work array
    !! ldel_ : input integer : array dimension, >= max(7*(N+1),4*(N+1)+M)
    !! gm_(n+1) : output real array : work array
    !! bdl_(n+1) : output real array : work array
    !! bdu_(n+1) : output real array : work array
    !! h_(lh,lh) : output real array : work array
    !! lh_ : input integer : array dimension, >= 2*(N+1)
    !! wa_(lwa) : output real array : work array
    !! lwa_ : input integer : array dimension, >= 2*(N+1)
    !! iwa_(liwa) : output integer array : work array
    !! liwa_ : input integer : array dimension, >= 6*(N+1) + M
    !! ilower_(n) : input integer array : If X(I) has a lower bound,
    !! ILOWER(I) is set to 1 on input, otherwise 0
    !! bndl_(n) : input real array : lower bound of X(I)
    !! iupper_(n) : input integer array : If X(I) has an upper bound,
    !! IUPPER(I) is set to 1 on input, otherwise 0
    !! bndu_(n) : input real array : upper bound of X(I)
    !! This subroutine calculates the least value of a function of
    !! several variables subject to linear and/or nonlinear equality
    !! and inequality constraints.
    !! <P>More particularly, it solves the problem
    !! <PRE>
    !! Minimize f(x)
    !! subject to c (x) =  0.0 ,  i = 1,...,meq
    !! i
    !! and c (x) >= 0.0 ,  i = meq+1,...,m
    !! i
    !! and l <= x <= u  ,  i = 1,...,n
    !! i    i    i
    !! </PRE>
    !! The subroutine implements a variable metric method for
    !! constrained optimization developed by M.J.D. Powell.
    !! ANL-80-64: Solution of the General Nonlinear Programming Problem
    !! with Subroutine VMCON, Roger L Crane, Kenneth E Hillstrom and
    !! Michael Minkoff, Argonne National Laboratory, 1980
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use constants, only: opt_file
    implicit none

    ! Arguments
    ! Input arguments, which are assigned to corresponding module variables
    ! The trailing underscore denotes an argument
    ! e.g. input argument mode_ will be assigned to module variable mode
    integer, intent(in) :: mode_,n_,m_,meq_,lcnorm_,lb_,maxfev_,ldel_,lh_,lwa_,liwa_
    integer, dimension(n_), intent(in) :: ilower_,iupper_
    real(dp), intent(in) :: tol_
    real(dp), dimension(n_), intent(in) :: bndl_,bndu_

    ! Input/output arguments, which are assigned to module variables initially
    ! and then the final values of the module variables are assigned back to
    ! them at the end of the subroutine
    real(dp), dimension(n_), intent(inout) :: x_
    real(dp), dimension(lb_,lb_), intent(inout) :: b_

    ! Output arguments which module variables are assigned to at the end of the
    ! subroutine
    integer, intent(out) :: info_,nfev_,niter_
    integer, dimension(liwa_), intent(out) :: iwa_
    real(dp), intent(out) :: objf_
    real(dp), dimension(n_), intent(out) :: fgrd_
    real(dp), dimension(m_), intent(out) :: conf_
    real(dp), dimension(n_), intent(out) :: glag_,glaga_,gamma_,eta_,xa_,bdelta_
    real(dp), dimension(m_), intent(out) :: cm_
    real(dp), dimension(ldel_), intent(out) :: delta_
    real(dp), dimension(lwa_), intent(out) :: wa_
    real(dp), dimension(lcnorm_,m_), intent(out) :: cnorm_
    real(dp), dimension(lh_,lh_), intent(out) :: h_
    real(dp), dimension(:), intent(out) :: vlam_,vmu_,gm_,bdl_,bdu_
    real(dp), intent(out), optional :: sum_

    ! External subroutines
    external :: fcnvmc1_,fcnvmc2_
    
    ! Input
    ! Initialise module variables with their respective input arguments
    ! This allows vmcon to be called as before, but allows these variables to
    ! be shared between module subroutines
    mode = mode_
    n = n_
    m = m_
    meq = meq_
    lcnorm = lcnorm_
    lb = lb_
    tol = tol_
    maxfev = maxfev_
    ldel = ldel_
    lh = lh_
    lwa = lwa_
    liwa = liwa_
    ilower = ilower_
    iupper = iupper_
    bndl = bndl_
    bndu = bndu_

    ! Inout argument initialisations
    x = x_
    b = b_
    
    ! Assign external subroutine input arguments to module variable pointers
    ! Allows module-wide calling of these input argument subroutines
    fcnvmc1_ptr => fcnvmc1_
    fcnvmc2_ptr => fcnvmc2_
    
    ! Array allocation: only allocate what hasn't already been allocated by 
    ! assignment
    allocate(vmu(m+2*n+1))
    allocate(vlam(m+2*n+1))
    allocate(gm(n+1))
    allocate(bdl(n+1))
    allocate(bdu(n+1))
    allocate(conf(m))
    allocate(fgrd(n))
    allocate(cnorm(lcnorm,m))
    allocate(iwa(liwa))
    allocate(glag(n))
    allocate(glaga(n))
    allocate(gamma(n))
    allocate(eta(n))
    allocate(xa(n))
    allocate(bdelta(n))
    allocate(cm(m))
    allocate(delta(ldel))
    allocate(wa(lwa))
    allocate(h(lh,lh))
    allocate(delta_var(n))

    ! Run vmcon
    call run()
    
    ! Output
    ! Set inout arguments to values of module variables
    x_ = x
    b_ = b

    ! Set output arguments to values of module variables
    info_= info
    nfev_ = nfev
    niter_ = niter
    iwa_ = iwa
    objf_ = objf
    fgrd_ = fgrd
    conf_ = conf
    glag_ = glag
    glaga_ = glaga
    gamma_ = gamma
    eta_ = eta
    xa_ = xa
    bdelta_ = bdelta
    cm_ = cm
    delta_ = delta
    wa_ = wa
    cnorm_ = cnorm
    h_ = h
    vlam_ = vlam
    vmu_ = vmu
    gm_ = gm
    bdl_ = bdl
    bdu_ = bdu
    sum_ = sum

    ! Deallocate all allocatable arrays
    deallocate(iwa)
    deallocate(ilower)
    deallocate(iupper)
    deallocate(x)
    deallocate(bndl)
    deallocate(bndu)
    deallocate(fgrd)
    deallocate(conf)
    deallocate(glag)
    deallocate(glaga)
    deallocate(gamma)
    deallocate(eta)
    deallocate(xa)
    deallocate(bdelta)
    deallocate(cm)
    deallocate(delta)
    deallocate(wa)
    deallocate(cnorm)
    deallocate(h)
    deallocate(b)
    deallocate(vlam)
    deallocate(vmu)
    deallocate(gm)
    deallocate(bdl)
    deallocate(bdu)
    deallocate(best_solution_vector)
    deallocate(delta_var)
  end subroutine vmcon
  
  subroutine run()
    ! Call subroutines to actually run vmcon
    implicit none
    
    ! exit_code is used to handle exits and returns in previous vmcon() 
    ! subroutine, which is now broken up into smaller subroutines.
    ! #TODO This could be improved with further refactoring once smaller 
    ! subroutines are in place.
    ! Checked after each subroutine call:
    ! 0: OK, continue
    ! 1: return from run() and then vmcon()
    ! 2: exit the current loop
    exit_code = 0
    
    call vmcon1()
    if (exit_code.ne.0) return
    call fcnvmc1()
    if (exit_code.ne.0) return
    call vmcon2()
    if (exit_code.ne.0) return
    call fcnvmc2()
    if (exit_code.ne.0) return
    call vmcon3()
    if (exit_code.ne.0) return

    iteration: do
      call vmcon4()
      if (exit_code.ne.0) return
      
      ! Set sum to the weighted sum of infeasibilities
      ! Set fls to the line search objective function
      line_search: do
        call vmcon5()
        if (exit_code.eq.1) return
        if (exit_code.eq.2) exit
      end do line_search
     
      ! Reset exit_code; must be 2 to exit line_search
      exit_code = 0

      ! Line search is complete. Calculate gradient of Lagrangian
      ! function for use in updating hessian of Lagrangian
      call fcnvmc1()
      if (exit_code.ne.0) return
      call vmcon6()
      if (exit_code.ne.0) return
    end do iteration
  end subroutine run

  subroutine vmcon1()
    implicit none
    
    lowest_valid_fom = 999d0
    best_solution_vector = x
    np1 = n + 1
    npp = 2*np1
    info = 0
    
    !  Check input parameters for errors
    if ( &
      (n <= 0)           .or. &
      (m <= 0)           .or. &
      (meq > n)         .or. &
      (lcnorm < (n+1))  .or. &
      (lb < (n+1))      .or. &
      (tol < zero)      .or. &
      (ldel < max(7*(n+1),4*(n+1)+m)).or. &
      (lh < (2*(n+1)))  .or. &
      (lwa < (2*(n+1))) .or. &
      (liwa < (6*(n+1)+m)) &
      ) then
      exit_code = 1
      return
    endif
    
    !  Set the initial elements of b and vmu. vmu is the weighting
    !  vector to be used in the line search.
    !  Use hessian estimate provided by user if mode = 1 on input
    if (mode /= 1) then
      !  Use identity matrix for hessian estimate
      do j = 1, n
        do i = 1, n
          b(i,j) = zero
        end do
        b(j,j) = one
      end do
    end if
    
    mp1 = m + 1
    mpn = m + n
    mpnpp1 = m + np1 + 1
    mpnppn = m + np1 + n

    !  Set mcon to total number of actual constraints
    mcon = m
    do i = 1,n
      if (ilower(i) == 1) mcon = mcon + 1
    end do
    
    !  Set ml to m + number of lower bounds
    ml = mcon
    
    !  Set mlp1 to ml + 1
    mlp1 = ml + 1
    do i = 1, n
      if (iupper(i) == 1) mcon = mcon + 1
    end do
    
    do k = 1, mpnppn
      vmu(k) = zero
    end do
    
    !  Set initial values of some variables
    !  nfev is the number of calls of fcnvmc1
    !  nsix is the length of an array
    !  nqp is the number of quadratic subproblems (= number of iterations)
    nfev = 1
    nsix = 6*np1
    nqp = 0
    niter = 0
  end subroutine vmcon1

  subroutine fcnvmc1()
    ! Calculate the initial functions
    implicit none

    call fcnvmc1_ptr(n,m,x,objf,conf,info)
  end subroutine fcnvmc1

  subroutine vmcon2()
    implicit none
    
    if (info < 0) then
      exit_code = 1
      return
    endif
    
  end subroutine vmcon2
  
  subroutine fcnvmc2()
    ! Calculate the initial gradients
    implicit none
    
    call fcnvmc2_ptr(n,m,x,fgrd,cnorm,lcnorm,info)
  end subroutine fcnvmc2
  
  subroutine vmcon3
    use constants, only: iotty
    implicit none

    if (info < 0) then
      exit_code = 1
      return
    endif
    
    !  Setup line overwrite for VMCON iterations output
    open(unit=iotty)
    write(*,*) ""
    
  end subroutine vmcon3
  
  subroutine vmcon4()
    !  Start the iteration by calling the quadratic programming
    !  subroutine
    use global_variables, only: verbose, maxcal
    use maths_library, only: qpsub
    use constants, only: iotty
    implicit none

    !  Increment the quadratic subproblem counter
    nqp = nqp + 1
    niter = nqp
    
    !  Set the linear term of the quadratic problem objective function
    !  to the negative gradient of objf
    do i = 1, n
      gm(i) = -fgrd(i)
    end do
    do i = 1, mpnppn
      vlam(i) = zero
    end do
    
    call qpsub( &
      n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,info,x,delta, &
      ldel,cm,h,lh,mact,wa,lwa,iwa,liwa,ilower,iupper, &
      bndl,bndu)
    
    !  The following return is made if the quadratic problem solver
    !  failed to find a feasible point, if an artificial bound was
    !  active, or if a singular matrix was detected
    if ((info == 5).or.(info == 6)) then
      ! Issue #601 Return the best value of the solution vector - not the last value. MDK
      x = best_solution_vector
      sum = best_sum_so_far
      write(*,*)
      write(*,fmt_str)'Best solution vector will be output. Convergence parameter = ', sum
      exit_code = 1
      return
    end if
    
    !  Initialize the line search iteration counter
    nls = 0
    
    !  Calculate the Lagrange multipliers
    do j = 1, mact
      k = iwa(j) - npp
      if (k > 0) goto 59
      ki = iwa(j)
      k = ki + m
      if (ki > np1) goto 58
      if (ki == np1) cycle
      if (ilower(ki) == 1) goto 59
      cycle
58      continue
      ki = iwa(j) - np1
      k = ki + m + np1
      if (ki == np1) cycle
      if (iupper(ki) == 1) goto 59
      cycle
59      continue
      do i = 1, n
        np1j = np1 + j
        nsixi = nsix + i
        vlam(k) = vlam(k) + h(np1j,i)*delta(nsixi)
      end do
    end do
    
    !  Calculate the gradient of the Lagrangian function
    !  nfinit is the value of nfev at the start of an iteration
    nfinit = nfev
    do i = 1, n
      glag(i) = fgrd(i)
    end do
    do k = 1, m
      if (vlam(k) /= zero) then
        do i = 1, n
          glag(i) = glag(i) - cnorm(i,k)*vlam(k)
        end do
      end if
    end do
    do k = mp1, mpn
      if (vlam(k) /= zero) then
        inx = k - m
        if (ilower(inx) /= 0) glag(inx) = glag(inx) - vlam(k)
      end if
    end do
    do k = mpnpp1, mpnppn
      if (vlam(k) /= zero) then
        inx = k - m - np1
        if (iupper(inx) /= 0) glag(inx) = glag(inx) + vlam(k)
      end if
    end do
    
    !  Set spgdel to the scalar product of fgrd and delta
    !  Store the elements of glag and x
    spgdel = zero
    do i = 1, n
      spgdel = spgdel + fgrd(i)*delta(i)
      glaga(i) = glag(i)
      xa(i) = x(i)
    end do
    
    !  Revise the vector vmu and test for convergence
    sum = abs(spgdel)
    do k = 1, mpnppn
      aux = abs(vlam(k))
      vmu(k) = max(aux,cp5*(aux + vmu(k)))
      temp = 0.0D0
      if (k <= m) then
        temp = conf(k)
      else
        if (k <= mpn) then
          inx = k - m
          if (ilower(inx) == 0) cycle
          temp = x(inx) - bndl(inx)
        else
          inx = k - m - np1
          if ((inx == 0) .or. (inx > n)) cycle
          if (iupper(inx) == 0) cycle
          temp = bndu(inx) - x(inx)
        end if
      end if
      sum = sum + abs(vlam(k)*temp)
    end do
    
    !  Check convergence of constraint residuals
    summ = 0.0D0
    !do i = 1,m
    ! This only includes the equality constraints Issue #505
    do i = 1,meq
      summ = summ + conf(i)*conf(i)
    end do
    sqsumsq = sqrt(summ)
    
    !  Output to terminal number of VMCON iterations
    iteration_progress = repeat("=", floor(((niter+1)/FLOAT(maxcal))*20.0D0))
    
    write(iotty, '("==>", I5, "  vmcon iterations. Normalised FoM =", &
    &  f8.4, "  Residuals (sqsumsq) =", 1pe8.1, "  Convergence param =", 1pe8.1, a1)', &
    ADVANCE="NO") niter+1, max(objf, -objf), sqsumsq, sum, achar(13)
    
    if (verbose == 1) then
      write(*,'(a,es13.5,a,es13.5)') &
        'Constraint residuals (sqsumsq) = ',sqsumsq, &
        ' Convergence parameter = ',sum
    end if
    
    ! Writting the step results in OPT.DAT file
    do i = 1, n
      delta_var(i) = delta(i)
    end do

    ! Comment in to write optional optimisation information output file
    !  write(opt_file, '(I5,E28.10,*(E18.10))') niter+1, abs(objf), sum, sqsumsq, conf, x, delta_var
    
    !  Exit if both convergence criteria are satisfied
    !  (the original criterion, plus constraint residuals below the tolerance level)
    !  Temporarily set the two tolerances equal (should perhaps be an input parameter)
    sqsumsq_tol = tol
    
    ! Store the lowest valid FoM (ie where constraints are satisfied)
    if (sqsumsq < sqsumsq_tol)  lowest_valid_fom = min(lowest_valid_fom, objf)
    
    if ((sum <= tol).and.(sqsumsq < sqsumsq_tol)) then
      if (verbose == 1) then
        write(*,*) 'Convergence parameter < convergence criterion (epsvmc)'
        write(*,*) 'Root of sum of squares of residuals < tolerance (sqsumsq_tol)'
      end if
      exit_code = 1
      return
    end if
    
    ! The convergence criteria are not yet satisfied.
    ! Store the best value of the convergence parameter achieved
    if(sum < best_sum_so_far) then
      best_sum_so_far = sum
      best_solution_vector = x
    end if
  end subroutine vmcon4

  subroutine vmcon5()
    use global_variables, only: verbose
    implicit none

    !  Increment the line search iteration counter
    nls = nls + 1
    sum = zero
    do k = 1, mpnppn
      aux = 0.0D0
      if (k <= meq) aux = conf(k)
      temp = 0.0D0
      if (k <= m) then
        temp = conf(k)
      else
        if (k <= mpn) then
          inx = k - m
          if (ilower(inx) == 0) cycle
          temp = x(inx) - bndl(inx)
        else
          inx = k - m - np1
          if ((inx == 0) .or. (inx > n)) cycle
          if (iupper(inx) == 0) cycle
          temp = bndu(inx) - x(inx)
        end if
      end if
      sum = sum + vmu(k)*max(aux,-temp)
    end do
    fls = objf + sum
    
    if (nfev == nfinit) then

      !  Set the initial conditions for the line search
      !  flsa is the initial value of the line search function
      !  dflsa is its first derivative (if delta(np1) = 1)
      !  alpha is the next reduction in the step-length

      flsa = fls
      dflsa = spgdel - delta(np1)*sum
      if (dflsa >= zero) then
        !  Error return because uphill search direction was calculated
        info = 4
        ! Issue #601 Return the best value of the solution vector - not the last value. MDK
        write(*,*) 'info = 4'
        x = best_solution_vector
        sum = best_sum_so_far
        write(*,*)
        write(*,fmt_str)'Best solution vector will be output. Convergence parameter = ', sum
        exit_code = 1
        return
      end if

      !  Set initial multiplying factor for stepsize
      !  Set initial value of stepsize for output

      alpha = one
      calpha = one

    else

      !  Test whether line search is complete

      aux = fls - flsa

      !  Exit line search if function difference is small

      if (aux <= (cp1*dflsa)) then
        exit_code = 2
        return
      endif

      !  Escape from the line search if the line search function is increasing
      !  Outer loop is forced to repeat

      info = 1  !  reset on each iteration
      if (aux > 0.0D0) then
        if (verbose == 1) then
          write(*,*) 'VMCON optimiser line search attempt failed - retrying...'
        end if
        info = 7
        exit_code = 2
        return
      end if

      !  Exit if the line search requires ten or more function evaluations

      if (nfev >= (nfinit + 10)) then
        do i = 1, n
            x(i) = xa(i)
        end do
        nfev = nfev + 1
        call fcnvmc1()
        if (info >= 0) info = 3
        ! Error return because line search required 10 calls of fcnvmc1
        ! Issue #601 Return the best value of the solution vector - not the last value. MDK
        x = best_solution_vector
        sum = best_sum_so_far
        write(*,*)
        write(*,fmt_str)'Best solution vector will be output. Convergence parameter = ', sum
        exit_code = 1
        return
      end if

      !  Calculate next reduction in the line step assuming
      !  a quadratic fit

      alpha = max(cp1,cp5*dflsa/(dflsa - aux))

    end if

    !  Multiply delta by alpha and calculate the new x

    calpha = alpha*calpha
    do i = 1, n
      delta(i) = alpha*delta(i)
      x(i) = xa(i) + delta(i)
    end do

    dflsa = alpha*dflsa

    !  Test nfev against maxfev, call fcnvmc1 and resume line search

    if (nfev >= maxfev) then
      do i = 1, n
        x(i) = xa(i)
      end do
      !  Error return because there have been maxfev calls of fcnvmc1
      info = 2
      ! Issue #601 Return the best value of the solution vector - not the last value. MDK
      x = best_solution_vector
      sum = best_sum_so_far
      write(*,*)
      write(*,fmt_str)'Best solution vector will be output. Convergence parameter = ', sum
      exit_code = 1
      return
    end if
    
    nfev = nfev + 1
    
    call fcnvmc1()
    if (info < 0) then
      exit_code = 1
      return
    endif
  end subroutine vmcon5

  subroutine vmcon6()
    implicit none

    call fcnvmc2()
    if (info < 0) then
      exit_code = 1
      return
    endif

    do i = 1, n
      glag(i) = fgrd(i)
    end do
    do k = 1, m
      if (vlam(k) /= zero) then
        do i = 1, n
        glag(i) = glag(i) - cnorm(i,k)*vlam(k)
        end do
      end if
    end do
    do k = mp1, mpn
      if (vlam(k) /= zero) then
        inx = k - m
        if (ilower(inx) /= 0) glag(inx) = glag(inx) - vlam(k)
      end if
    end do
    do k = mpnpp1, mpnppn
      if (vlam(k) /= zero) then
        inx = k - m - np1
        if (iupper(inx) /= 0) glag(inx) = glag(inx) + vlam(k)
      end if
    end do

    !  Calculate gamma and bdelta in order to revise b
    !  Set dg to the scalar product of delta and gamma
    !  Set dbd to the scalar product of delta and bdelta

    dg = zero
    dbd = zero
    do i = 1, n
      gamma(i) = glag(i) - glaga(i)
      bdelta(i) = zero
      do j = 1, n
        bdelta(i) = bdelta(i) + b(i,j)*delta(j)
      end do
      dg = dg + delta(i)*gamma(i)
      dbd = dbd + delta(i)*bdelta(i)
    end do

    !  Calculate the vector eta for the b-f-g-s formula
    !  replace dg by the scalar product of delta and eta

    aux = cp2*dbd
    theta = one
    if (dg < aux) theta = (dbd - aux)/(dbd - dg)
    thcomp = one - theta
    do i = 1, n
      eta(i) = theta*gamma(i) + thcomp*bdelta(i)
    end do
    if (dg < aux) dg = aux

    !  Revise the matrix b and begin new iteration

    do i = 1, n
      aux = bdelta(i)/dbd
      auxa = eta(i)/dg
      do j = i, n
        b(i,j) = b(i,j) - aux*bdelta(j) + auxa*eta(j)
        b(j,i) = b(i,j)
      end do
    end do
  end subroutine vmcon6
end module vmcon_module