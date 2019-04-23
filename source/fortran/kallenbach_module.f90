! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module kallenbach_module
  !+ad_name  kallenbach_module
  !+ad_summ  Module for Kallenbach model testing and scans
  !+ad_type  Module
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
  !+ad_docs  Sertoli, private communication
  !+ad_docs  Kallenbach et al., Plasma Phys. Control. Fus. 55(2013) 124041
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !---------------------------------------------------------------------------

  use process_output
  use output_module
  use constants
  use divertor_kallenbach_variables

contains

  !---------------------------------------------------------------------------
  ! This routine is outside the module, so it functions as test for the module.

  subroutine kallenbach_testing()
    !+ad_name  kallenbach_testing
    !+ad_summ  Tests for divertor kallenbach model
    !+ad_type  subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Tests of Kallenbach divertor model
    !+ad_prob  None
    !+ad_hist  23/04/19 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call osubhd(iotty,'# Running test of Kallenbach divertor model')
    select case (kallenbach_test_option)
        case (0)
          call ocmmnt(iotty, "Running test with : user inputs")
          call kallenbach_test_user_inputs()
        case(1)
          call ocmmnt(iotty, "Running test with : kallenbach paper inputs")
          call kallenbach_test_paper()
        case default
          call ocmmnt(iotty, "Running test with : user inputs")
          call kallenbach_test_user_inputs()
    end select

  end subroutine kallenbach_testing

  subroutine kallenbach_test_paper()
    !+ad_name  kallenbach_test_paper
    !+ad_summ  Test for divertor kallenbach model for matching kallenbach paper
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Test of Kallenbach divertor model
    !+ad_prob  None
    !+ad_hist  01/02/17 MDK  Initial version
    !+ad_stat  Okay
    !+ad_docs
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use divertor_ode
    use read_and_get_atomic_data
    use read_radiation
    use constants
    use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, &
                              ovarin, ovarre, ovarrf, ovarst
    use physics_variables, only: tesep

    implicit none

    integer :: i

    real(kind(1.0D0)):: rmajor, rminor, bt, plascur, q, t_target, &
                        q_target_total, target_angle, b_pol
    real(kind(1.0D0)):: dummy, dummy2, dummy3

    ! This section just for reproducing the original numbers
    rmajor = 8.0D0
    rminor = 2.75D0
    bt = 4.00972D0*(rmajor + rminor)/rmajor
    plascur = 1.33542D0*(2.0D0*pi*rminor)/rmu0
    q = 3.0D0
    t_target = 2.3D0
    q_target_total = 4.175D6
    target_angle = 30.0D0
    b_pol = 0.956d0
    tesep = 0.298
    target_spread = 7.0D-3
    netau_sol = 0.5D0
    lambda_q_omp = 0.002D0

    ! MDK Issue #494.
    ! Invert the equation for lcon to ensure lcon=100 for this test.
    ! lcon = lcon_factor * 0.395d0*pi*q*rmajor/lambda_omp**0.196
    lcon_factor = 100.0D0 / (0.395d0*pi*3.0d0*8.0D0/0.002D0**0.196)

    call oheadr(nout, 'Divertor: Kallenbach 1D Model - TESTS - ')
    call osubhd(nout, 'Inputs :')

    call ovarre(nout, 'Major radius [m]','(rmajor)', rmajor)
    call ovarre(nout, 'Minor radius [m]','(rminor)', rminor)
    call ovarre(nout, 'Toroidal field [T]','(bt)', bt, 'OP ')
    call ovarre(nout, 'Plasma current [A]','(plascur)', plascur)
    call ovarre(nout, 'q95 [A]','(q)', q)

    ! Set the impurity fractions to the test values
    do i = 2, nimp
      impurity_arr(i)%frac = 0.0D0
    enddo

    ! Set the impurity array fraction of Nitrogen 
    ! gives 0.04 in SOL, as in Kallenbach paper
    impurity_arr(5)%frac = 8.0D-3  

    call divertor_Kallenbach(rmajor=rmajor, rminor=rminor, &
                            bt=bt, plascur=plascur, &
                            q=q, &
                            verboseset=.false., &
                            ttarget=t_target, &
                            qtargettotal=q_target_total, &
                            targetangle=target_angle, &
                            unit_test=.false., &
                            bp=b_pol, &
                            psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3, &
                            outfile=nout,iprint=1)

    call ocmmnt(nout, 'Testing the reading of atomic rates and impurity radiative functions.')
    call ocmmnt(nout, 'Use "output_divertor.xlsx" in K:\Power Plant Physics and Technology\PROCESS\SOL & Divertor')

    call plot_rates()
    call ocmmnt(nout, 'Rate coefficients for deuterium - saved in "rate_coefficients.txt"')
    call ocmmnt(nout, 'Compare to Figure 2 in Kallenbach 2016.')
    call plot_Lz()
    call ocmmnt(nout, 'Radiative loss functions - saved in "radiative_loss_functions.txt"')
    call ocmmnt(nout, 'Compare to Figure 3 in Kallenbach 2016.')
    call plot_z()
    call ocmmnt(nout, 'Reads mean Z and mean Z^2 - saved in "mean_Z.tx"')
    call ocmmnt(nout, 'Compare to plots such as He_z.ps etc in /home/mkovari/sol/kallenbach/divertor_ode/LZ_NON_CORONA.')

  end subroutine kallenbach_test_paper

  subroutine kallenbach_test_user_inputs()
    !+ad_name  kallenbach_test_user_inputs
    !+ad_summ  Test for divertor kallenbach model for using input file
    !+ad_type  subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Test of Kallenbach divertor model
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use divertor_ode
    use read_and_get_atomic_data
    use read_radiation
    use constants
    use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, &
                              ovarin, ovarre, ovarrf, ovarst
    use physics_variables, only: rmajor, rminor, bt, plascur, q, aspect, &
                                 pperim, itart, kappa, triang
    use divertor_kallenbach_variables, only: ttarget, qtargettotal, targetangle
    use physics_module, only: bpol

    implicit none

    real(kind(1.0D0)):: b_pol

    real(kind(1.0D0)):: dummy, dummy2, dummy3

    real(kind(1.0D0)) :: xi, thetai, xo, thetao

    ! Calculate plasma geometry
    rminor = rmajor/aspect

    ! Find parameters of arcs describing plasma surfaces
    call local_xparam(rminor, kappa, triang, xi, thetai, xo, thetao)

    ! Poloidal perimeter
    pperim = 2.0D0 * (xo*thetao + xi*thetai)

    ! Calculate plasma current
    plascur = 1.33542D0*(2.0D0*pi*rminor)/rmu0

    ! Calculate average poloidal field
    b_pol = bpol(itart, plascur, q, aspect, bt, kappa, triang, pperim)

    call oheadr(nout, 'Divertor: Kallenbach 1D Model - Test with user input - ')
    call osubhd(nout, 'Inputs :')
    
    call ovarre(nout, 'Major radius [m]','(rmajor)', rmajor)
    call ovarre(nout, 'Aspect ratio','(aspect)', aspect)
    call ovarre(nout, 'Toroidal field [T]','(bt)', bt)
    call ovarre(nout, 'Plasma current [A]','(plascur)', plascur)
    call ovarre(nout, 'q95','(q)', q)
    call ovarre(nout, 'Plasma elongation','(kappa)', kappa)
    call ovarre(nout, 'Plasma triangularity','(triang)', triang)
    call ovarre(nout, 'Increase in SOL power fall-off length due to spreading [m]',&
                '(target_spread)', target_spread)
    call ovarre(nout, 'SOL power fall-off length at the outer midplane [m]', &
                '(lambda_q_omp)', lambda_q_omp)
    call ovarre(nout, 'Parameter describing departure from local ionisation '&
                'equil. in the sol; [ms;1e20/m3]','(netau_sol)', netau_sol)
    call ovarre(nout, 'Angle between flux surface and divertor target [deg]',&
                '(targetangle)', targetangle)
    call ovarre(nout, 'Power density on target including surface recombination [w/m2]',&
                '(qtargettotal)', qtargettotal)
    
    call divertor_Kallenbach(rmajor=rmajor, rminor=rminor, &
                            bt=bt, plascur=plascur, &
                            q=q, &
                            verboseset=.false., &
                            ttarget=ttarget, &
                            qtargettotal=qtargettotal, &
                            targetangle=targetangle, &
                            unit_test=.false., &
                            bp=b_pol, &
                            psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3, &
                            outfile=nout,iprint=1)

    call ocmmnt(nout, 'Testing the reading of atomic rates and impurity radiative functions.')
    call ocmmnt(nout, 'Use "output_divertor.xlsx" in K:\Power Plant Physics and Technology\PROCESS\SOL & Divertor')

    call plot_rates()
    call ocmmnt(nout, 'Rate coefficients for deuterium - saved in "rate_coefficients.txt"')
    call ocmmnt(nout, 'Compare to Figure 2 in Kallenbach 2016.')
    call plot_Lz()
    call ocmmnt(nout, 'Radiative loss functions - saved in "radiative_loss_functions.txt"')
    call ocmmnt(nout, 'Compare to Figure 3 in Kallenbach 2016.')
    call plot_z()
    call ocmmnt(nout, 'Reads mean Z and mean Z^2 - saved in "mean_Z.tx"')
    call ocmmnt(nout, 'Compare to plots such as He_z.ps etc in /home/mkovari/sol/kallenbach/divertor_ode/LZ_NON_CORONA.')

  end subroutine kallenbach_test_user_inputs

  subroutine kallenbach_scan()
    !+ad_name  kallenbach_scan
    !+ad_summ  Scans for divertor kallenbach model: No optimisation
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !
    use divertor_ode
    use read_and_get_atomic_data
    use read_radiation
    use constants
    use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, &
                              ovarin, ovarre, ovarrf, ovarst
    use physics_variables, only: rmajor, rminor, bt, plascur, q, aspect, &
                                 pperim, itart, kappa, triang
    use divertor_kallenbach_variables, only: ttarget, qtargettotal, targetangle
    use physics_module, only: bpol

    implicit none

    integer :: i

    real(kind(1.0D0)):: b_pol, step_value

    real(kind(1.0D0)):: dummy, dummy2, dummy3

    real(kind(1.0D0)) :: xi, thetai, xo, thetao

    ! Calculate plasma geometry
    rminor = rmajor/aspect

    ! Find parameters of arcs describing plasma surfaces
    call local_xparam(rminor, kappa, triang, xi, thetai, xo, thetao)

    ! Poloidal perimeter
    pperim = 2.0D0 * (xo*thetao + xi*thetai)

    ! Calculate plasma current
    plascur = 1.33542D0*(2.0D0*pi*rminor)/rmu0

    ! Calculate average poloidal field
    b_pol = bpol(itart, plascur, q, aspect, bt, kappa, triang, pperim)
  
    call oheadr(nout, 'Divertor: Kallenbach 1D Model - SCANS - ')
    call ovarin(nout,'Number of scan points','(isweep)',kallenbach_scan_num)
    call ovarin(nout,'Scanning variable number','(kallenbach_scan_var)',kallenbach_scan_var)
    call ovarre(nout, 'Major radius [m]','(rmajor)', rmajor)
    call ovarre(nout, 'Aspect ratio','(aspect)', aspect)
    call ovarre(nout, 'Toroidal field [T]','(bt)', bt)
    call ovarre(nout, 'Plasma current [A]','(plascur)', plascur)
    call ovarre(nout, 'q95','(q)', q)
    call ovarre(nout, 'Plasma elongation','(kappa)', kappa)
    call ovarre(nout, 'Plasma triangularity','(triang)', triang)
    call ovarre(nout, 'Increase in SOL power fall-off length due to spreading [m]',&
                '(target_spread)', target_spread)
    call ovarre(nout, 'SOL power fall-off length at the outer midplane [m]', &
                '(lambda_q_omp)', lambda_q_omp)
    call ovarre(nout, 'Parameter describing departure from local ionisation '&
                'equil. in the sol; [ms;1e20/m3]','(netau_sol)', netau_sol)
    call ovarre(nout, 'Angle between flux surface and divertor target [deg]',&
                '(targetangle)', targetangle)
    call ovarre(nout, 'Power density on target including surface recombination [w/m2]',&
                '(qtargettotal)', qtargettotal)

    select case (kallenbach_scan_var)
      case(0)
        ttarget = kallenbach_scan_start
        write(*,*)'Running kallenbach model for = ttarget'
      case(1)
        qtargettotal = kallenbach_scan_start
        write(*,*)'Running kallenbach model for = qtargettotal'
      case(2)
        targetangle = kallenbach_scan_start
        write(*,*)'Running kallenbach model for = targetangle'
      case(3)
        lambda_q_omp = kallenbach_scan_start
        write(*,*)'Running kallenbach model for = lambda_q_omp'
      case(4)
        netau_sol = kallenbach_scan_start
        write(*,*)'Running kallenbach model for = netau_sol'
      case default
        write(*,*) "Kallenbach scan variable not recognised"
        stop
    end select
  
    step_value = (kallenbach_scan_end - kallenbach_scan_start)/kallenbach_scan_num
  
    do i = 1, kallenbach_scan_num
  
        call oblnkl(nout)
        call ovarin(nout,'Scan point number','(iscan)',i)
        
        call divertor_Kallenbach(rmajor=rmajor, rminor=rminor, &
                            bt=bt, plascur=plascur, &
                            q=q, &
                            verboseset=.false., &
                            ttarget=ttarget, &
                            qtargettotal=qtargettotal, &
                            targetangle=targetangle, &
                            unit_test=.false., &
                            bp=b_pol, &
                            psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3, &
                            outfile=nout,iprint=1)
  
        select case (kallenbach_scan_var)
          case(0)
            ttarget = ttarget + step_value
          case(1)
            qtargettotal = step_value
          case(2)
            targetangle = step_value
          case(3)
            lambda_q_omp = step_value
          case(4)
            netau_sol = step_value
        end select        
    end do
  
  end subroutine Kallenbach_scan

  subroutine local_xparam(a,kap,tri,xi,thetai,xo,thetao)

    !+ad_name  local_xparam
    !+ad_summ  Routine to find parameters used for calculating geometrical
    !+ad_summ  properties for double-null plasmas
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a      : input real :  plasma minor radius (m)
    !+ad_args  kap    : input real :  plasma separatrix elongation
    !+ad_args  tri    : input real :  plasma separatrix triangularity
    !+ad_args  xi     : output real : radius of arc describing inboard surface (m)
    !+ad_args  thetai : output real : half-angle of arc describing inboard surface
    !+ad_args  xo     : output real : radius of arc describing outboard surface (m)
    !+ad_args  thetao : output real : half-angle of arc describing outboard surface
    !+ad_desc  This function finds plasma geometrical parameters, using the
    !+ad_desc  revolution of two intersecting arcs around the device centreline.
    !+ad_desc  This calculation is appropriate for plasmas with a separatrix.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  18/01/99 PJK Initial upgraded version
    !+ad_hist  16/07/01 PJK Correction of sign of TRI in XI
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK14, p.42
    !+ad_docs  F/PL/PJK/PROCESS/CODE/047
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: a,kap,tri
    real(kind(1.0D0)), intent(out) :: xi,thetai,xo,thetao

    !  Local variables

    real(kind(1.0D0)) :: denomi,denomo,n,t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Find radius and half-angle of inboard arc

    t = 1.0D0 - tri
    denomi = (kap**2 - t**2)/(2.0D0*t)
    thetai = atan(kap/denomi)
    xi = a * (denomi + 1.0D0 - tri )

    !  Find radius and half-angle of outboard arc

    n = 1.0D0 + tri
    denomo = (kap**2 - n**2)/(2.0D0*n)
    thetao = atan(kap/denomo)
    xo = a * (denomo + 1.0D0 + tri )

  end subroutine local_xparam

end module kallenbach_module

