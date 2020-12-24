module vmcon_module
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    ! Var from subroutine vmcon requiring re-initialisation on each new run
    real(dp) :: best_sum_so_far

contains
  subroutine init_vmcon_module
    !! Initialise module variables
    implicit none

    best_sum_so_far = 999d0
  end subroutine init_vmcon_module

  recursive subroutine vmcon( &
    fcnvmc1,fcnvmc2,mode,n,m,&
    meq,x,objf,fgrd,conf,&
    cnorm,lcnorm,b,lb,tol,&
    maxfev,info,nfev,niter,vlam,& 
    glag,vmu,cm,glaga,gamma,&
    eta,xa,bdelta,delta,ldel,&
    gm,bdl,bdu,h,lh,&
    wa,lwa,iwa,liwa,ilower, &
    iupper,bndl,bndu,sum)

    !! Calculates the least value of a function of several variables
    !! subject to linear and/or nonlinear equality and inequality
    !! constraints
    !! author: R L Crane, K E Hillstrom, M Minkoff, Argonne National Lab
    !! author: J Galambos, FEDC/ORNL
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: M D Kovari, CCFE, Culham Science Centre
    !! fcnvmc1 : external subroutine : routine to calculate the
    !! objective and constraint functions
    !! fcnvmc2 : external subroutine : routine to calculate the
    !! gradients of the objective and constraint functions
    !! mode : input integer : 1 if B is provided by the user,
    !! 0 if B is to be initialized to
    !! the identity matrix
    !! n : input integer : number of variables
    !! m : input integer : number of constraints
    !! meq : input integer : number of equality constraints
    !! x(n) : input/output real array : initial/final estimates of
    !! solution vector
    !! objf : output real : objective function at output X
    !! fgrd(n) : output real array : components of the gradient of
    !! the objective function at output X
    !! conf(m) : output real array : values of the constraint
    !! functions at output X (equality constraints must precede
    !! the inequality constraints)
    !! cnorm(lcnorm,m) : output real array : constraint normals at
    !! output X
    !! lcnorm : input integer : array dimension, >= n+1
    !! b(lb,lb) : input/output real array : approximation to the
    !! second derivative matrix of the Lagrangian function.
    !! Often, an adequate initial B matrix can be obtained by
    !! approximating the hessian of the objective function.
    !! On input, the approximation is provided by the user if
    !! MODE = 1 and is set to the identity matrix if MODE = 0.
    !! lb : input integer : array dimension, >= n+1
    !! tol : input real : A normal return occurs when the
    !! objective function plus suitably weighted multiples
    !! of the constraint functions are predicted to differ from
    !! their optimal values by at most TOL.
    !! maxfev : input integer : maximum number of calls to FCNVMC1
    !! info : output integer : error flag<BR>
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
    !! nfev : output integer : number of calls to FCNVMC1
    !! niter : output integer : number of iterations
    !! vlam(m+2n+1) : output real array : Lagrange multipliers at output X.
    !! The Lagrange multipliers provide the sensitivity of the objective
    !! function to changes in the constraint functions.
    !! Note that VLAM(M+I), I=1,...,N gives the multipliers for
    !! the lower bound constraints.  VLAM(M+N+1+I), I=1,...,N
    !! gives the multipliers for the upper bound constraints.
    !! glag(n) : output real array : components of the gradient of
    !! the Lagrangian function at the output X.
    !! cm(m) : output real array : work array
    !! vmu(m+2n+1) : output real array : work array
    !! glaga(n) : output real array : work array
    !! gamma(n) : output real array : work array
    !! eta(n) : output real array : work array
    !! xa(n) : output real array : work array
    !! bdelta(n) : output real array : work array
    !! delta(ldel) : output real array : work array
    !! ldel : input integer : array dimension, >= max(7*(N+1),4*(N+1)+M)
    !! gm(n+1) : output real array : work array
    !! bdl(n+1) : output real array : work array
    !! bdu(n+1) : output real array : work array
    !! h(lh,lh) : output real array : work array
    !! lh : input integer : array dimension, >= 2*(N+1)
    !! wa(lwa) : output real array : work array
    !! lwa : input integer : array dimension, >= 2*(N+1)
    !! iwa(liwa) : output integer array : work array
    !! liwa : input integer : array dimension, >= 6*(N+1) + M
    !! ilower(n) : input integer array : If X(I) has a lower bound,
    !! ILOWER(I) is set to 1 on input, otherwise 0
    !! bndl(n) : input real array : lower bound of X(I)
    !! iupper(n) : input integer array : If X(I) has an upper bound,
    !! IUPPER(I) is set to 1 on input, otherwise 0
    !! bndu(n) : input real array : upper bound of X(I)
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
    use constants, only: iotty, opt_file
    use global_variables, only: maxcal, verbose
    use maths_library, only: qpsub
    implicit none

    interface
      subroutine fcnvmc1(n,m,xv,objf,conf,ifail)
        use, intrinsic :: iso_fortran_env, only: dp=>real64
        integer, intent(in) :: n,m
        real(dp), dimension(n), intent(in) :: xv
        real(dp), intent(out) :: objf
        real(dp), dimension(m), intent(out) :: conf
        integer, intent(inout) :: ifail
      end subroutine fcnvmc1
    end interface

    !  Arguments

    integer, intent(in) :: mode,n,m,meq,lcnorm,lb,maxfev,ldel,lh,lwa,liwa
    integer, intent(out) :: info,nfev,niter

    integer, dimension(liwa), intent(out) :: iwa
    integer, dimension(n), intent(in) :: ilower,iupper
    real(dp), intent(out) :: objf
    real(dp), intent(in) :: tol
    real(dp), dimension(n), intent(inout) :: x
    real(dp), dimension(n), intent(in) :: bndl,bndu
    real(dp), dimension(n), intent(out) :: fgrd
    real(dp), dimension(m), intent(out) :: conf
    real(dp), dimension(n), intent(out) :: glag,glaga,gamma,eta,xa,bdelta
    real(dp), dimension(m), intent(out) :: cm
    real(dp), dimension(ldel), intent(out) :: delta
    real(dp), dimension(lwa), intent(out) :: wa
    real(dp), dimension(lcnorm,m), intent(out) :: cnorm
    real(dp), dimension(lh,lh), intent(out) :: h
    real(dp), dimension(lb,lb), intent(inout) :: b
    real(dp), dimension(*), intent(out) :: vlam,vmu,gm,bdl,bdu
    real(dp), intent(out), optional :: sum

    !  Local variables
    real(dp), dimension(n) :: best_solution_vector
    real(dp), dimension(n) :: delta_var           ! For opt data extraction only
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

    !  External routines

    external :: fcnvmc1,fcnvmc2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
        ) return

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
    nqp = 0 ; niter = 0

    !  Calculate the initial functions and gradients

    call fcnvmc1(n,m,x,objf,conf,info)
    if (info < 0) return

    call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)
    if (info < 0) return

    !  Setup line overwrite for VMCON iterations output
    open(unit=iotty)
    write(*,*) ""

    !  Start the iteration by calling the quadratic programming
    !  subroutine

    iteration: do

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
        write(*,20)'Best solution vector will be output. Convergence parameter = ', sum
    20         format(a,1pe10.3)
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
    58        continue
        ki = iwa(j) - np1
        k = ki + m + np1
        if (ki == np1) cycle
        if (iupper(ki) == 1) goto 59
        cycle
    59        continue
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
        return
    end if

    ! The convergence criteria are not yet satisfied.
    ! Store the best value of the convergence parameter achieved
    if(sum < best_sum_so_far)then
        best_sum_so_far = sum
        best_solution_vector = x
    end if

    !  Set sum to the weighted sum of infeasibilities
    !  Set fls to the line search objective function

    line_search: do

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
            write(*,20)'Best solution vector will be output. Convergence parameter = ', sum
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

            if (aux <= (cp1*dflsa)) exit line_search

            !  Escape from the line search if the line search function is increasing
            !  Outer loop is forced to repeat

            info = 1  !  reset on each iteration
            if (aux > 0.0D0) then
            if (verbose == 1) then
                write(*,*) 'VMCON optimiser line search attempt failed - retrying...'
            end if
            info = 7
            exit line_search
            end if

            !  Exit if the line search requires ten or more function evaluations

            if (nfev >= (nfinit + 10)) then
            do i = 1, n
                x(i) = xa(i)
            end do
            nfev = nfev + 1
            call fcnvmc1(n,m,x,objf,conf,info)
            if (info >= 0) info = 3
            ! Error return because line search required 10 calls of fcnvmc1
            ! Issue #601 Return the best value of the solution vector - not the last value. MDK
            x = best_solution_vector
            sum = best_sum_so_far
            write(*,*)
            write(*,20)'Best solution vector will be output. Convergence parameter = ', sum
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
            write(*,20)'Best solution vector will be output. Convergence parameter = ', sum
            return
        end if

        nfev = nfev + 1

        call fcnvmc1(n,m,x,objf,conf,info)
        if (info < 0) return

    end do line_search

    !  Line search is complete. Calculate gradient of Lagrangian
    !  function for use in updating hessian of Lagrangian

    call fcnvmc1(n,m,x,objf,conf,info)
    call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)
    if (info < 0) return

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
    end do iteration

  end subroutine vmcon
end module vmcon_module