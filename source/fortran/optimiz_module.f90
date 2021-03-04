module optimiz_module
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    contains

  subroutine optimiz(ifail, f)

    !! Calls the minimisation/maximisation routine VMCON
    !! author: P J Knight, CCFE, Culham Science Centre
    !! ifail   : output integer : error flag
    !! f       : output real    : value of objective function at the output point
    !! This routine calls the minimisation/maximisation routine VMCON,
    !! developed by Argonne National Laboratory.
    !! On exit, the (normalised) value of the variable being maximised
    !! or minimised (i.e. the figure of merit) is returned in argument
    !! <CODE>f</CODE>.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use global_variables, only: maxcal, convergence_parameter, iscan_global, &
      xlabel_2
    use constants, only: mfile, nplot, nout, pi, opt_file
    use vmcon_module, only: vmcon
    use plasmod_variables, only: plasmod_i_impmodel
    use numerics, only: ipnvars, ipeqns, epsfcn, epsvmc, nineqns, nvar, neqns, &
      nfev2, nviter, vlam, bondl, bondu, xcm, ixc, rcm, icc
    use function_evaluator, only: fcnvmc1, fcnvmc2
    implicit none

    !  Arguments

    integer, intent(out) :: ifail
    real(dp), intent(out) :: f

    !  Local variables

    integer :: ii,lb,lcnorm,ldel,lh,liwa,lwa,m,meq,mode,n
    integer, parameter :: ippn1  = ipnvars+1
    integer, parameter :: ipldel = 7*ippn1
    integer, parameter :: iplh   = 2*ippn1
    integer, parameter :: ipvmu  = ipeqns+2*ipnvars+1
    integer, parameter :: ipliwa = 6*ippn1+ipeqns
    integer, dimension(ipliwa) :: iwa
    integer, dimension(ipnvars) :: ilower,iupper

    ! Array defined for optimizer data output only
    integer, dimension(nvar)          :: ixc_opt_out
    integer, dimension(neqns+nineqns) :: icc_opt_out

    real(dp), parameter :: zero = 0.0D0
    real(dp), parameter :: bfactor = 2.0D0
    real(dp) :: xtol
    real(dp), dimension(ipnvars) :: bdelta,bndl,bndu,etav,fgrd, &
         gammv,glag,glaga,xa,xv
    real(dp), dimension(ipeqns) :: cm,conf
    real(dp), dimension(ippn1) :: bdl,bdu,gm
    real(dp), dimension(ipvmu) :: vmu
    real(dp), dimension(ipldel) :: delta
    real(dp), dimension(iplh) :: wa
    real(dp), dimension(ippn1,ipeqns) :: cnorm
    real(dp), dimension(ippn1,ippn1) :: b
    real(dp), dimension(iplh,iplh) :: h

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    n = nvar
    m = neqns + nineqns
    meq = neqns
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

    ! Write the VMCON setup in OPT.DAT
    do ii = 1, m
      icc_opt_out(ii) = icc(ii)
    end do
    do ii = 1, n
      ixc_opt_out(ii) = ixc(ii)
    end do
    
    write(opt_file, *) ' number of constrains'
    write(opt_file, '(I4)') m
    write(opt_file, *) ' '
    write(opt_file, *) ' Constrains selection'
    write(opt_file, '(I3,*(I4))') icc_opt_out
    write(opt_file, *) ' '
    write(opt_file, *) ' number of variables'
    write(opt_file, '(I4)') n
    write(opt_file, *) ' '
    write(opt_file, *) ' Variables selection'    
    write(opt_file, '(I3,*(I4))') ixc_opt_out
    write(opt_file, *) ' '
    write(opt_file, *) ' '
    write(opt_file, *) ' n VMCOM iter | Figure of merit | VMCON conv      | constrains quad sum |   residual,   input values &
                    &and  FoM input gradients'
    write(opt_file, '(A,*(I18))') '  niter          abs(objf)         sum                sqsumsq ', icc_opt_out, ixc_opt_out&
                    &, ixc_opt_out
 
    call vmcon(fcnvmc1, fcnvmc2, mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
         lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
         gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
         liwa,ilower,iupper,bndl,bndu,convergence_parameter)

    write(*,*) ""

    ! If fail then alter value of epsfcn - this can be improved
    if (ifail /= 1) then
       write(*,*) 'Trying again with new epsfcn'
       epsfcn = epsfcn * 10.0D0 !try new larger value
       write(*,*) 'new epsfcn = ', epsfcn
       call vmcon(fcnvmc1, fcnvmc2, mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu,convergence_parameter)
       epsfcn = epsfcn / 10.0D0 !reset value
    end if
    if (ifail /= 1) then
       write(*,*) 'Trying again with new epsfcn'
       epsfcn = epsfcn / 10.0D0 !try new smaller value
       write(*,*) 'new epsfcn = ', epsfcn
       call vmcon(fcnvmc1, fcnvmc2, mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu,convergence_parameter)
       epsfcn = epsfcn * 10.0D0 !reset value
    end if


    !  If VMCON has exited with error code 5 try another run using a multiple of
    !  the identity matrix as input for the Hessian b(n,n).
    !  Only do this if VMCON has not iterated (nviter=1).

    if ((ifail == 5).and.(nviter < 2)) then
       mode = 1
       b(:,:) = zero
       do ii = 1, n
          b(ii,ii) = bfactor
          xv(ii) = xcm(ii)      !  Re-initialise iteration values
       end do
       ! if (verbose == 1) then
       write(*,*) 'VMCON error code = 5.  Rerunning VMCON with a new initial estimate of the second derivative matrix.'
       ! end if

       call vmcon(fcnvmc1, fcnvmc2, mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu,convergence_parameter)
    end if

    do ii = 1,n
       xcm(ii) = xv(ii)
    end do

    do ii = 1,m
       rcm(ii) = conf(ii)
    end do

  end subroutine optimiz
end module optimiz_module