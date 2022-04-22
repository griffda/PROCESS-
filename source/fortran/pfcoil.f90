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
 
   subroutine bfield(nc, rc, zc, cc, xc, rp, zp, br, bz, psi, nciszero)
      ! #TODO bfield() is called very frequently (~15k times per solver iteration);
      ! converting this to Python results in a ~30x slowdown of the code! This will
      ! be left in Fortran for the time being, but in time should be converted to Python
      ! alongside a tool such as numba to ensure performance is maintained.

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
     logical, optional :: nciszero
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
 
      if (nciszero .eqv. .true.) then
         return
      endif

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
 end module pfcoil_module
