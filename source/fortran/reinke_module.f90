module reinke_module
#ifdef use_intrinsic
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   implicit none

  !private

  !  Module-level variables

  !integer ::
  real(dp) :: vcritx

contains

  subroutine init_reinke_module
    !! Initialise module variables
    implicit none

    vcritx = 0.0D0
  end subroutine init_reinke_module

  function reinke_fzmin(bt, flh, qstar, rmajor, eps, fsep, fgw, kappa, lhat, &
    netau, tesep, impvardiv, impurity_arr_frac, impurity_enrichment)
    !! Function for calculation of Reinke minimum impurity fraction
    !! author: H Lux, CCFE/UKAEA
    !! bt                  : input real : toroidal field on axis (T)
    !! flh                 : input real : fraction of Psep/P_LH
    !! qstar               : input real : safety factor similiar to q95 (see #707)
    !! rmajor              : input real : major radius (m)
    !! eps                 : input real : inverse aspect ratio
    !! fsep                : input real : ratio of separatrix to vol. av. density
    !! fgw                 : input real : ratio of volume averaged density to n_GW
    !! kappa               : input real : elongation
    !! lhat                : input real : connection length factor
    !! netau               : input real : "non-coronal parameter" for radiative loss func [ms.1e20/m3]
    !! tesep               : input real : temperature "upstream", i.e. at separatrix [eV]
    !! impvardiv           : input real : impurity index, e.g. 7 is Argon
    !! impurity_arr_frac   : input real array : impurity fractions
    !! impurity_enrichment : input real array : enrichment factors between SOL and core
    !! This function calculates the lower limit of the impurity fraction
    !! needed in the SOL for divertor protection. It has huge uncertainties in netau.
    !! Call the reinke_tsep function first then use as an argument to this function
    !! Issue #707
    !! M.L. Reinke 2017 Nucl. Fusion 57 034004
       
    ! use divertor_ode, only: impurity_concs
    ! use div_kal_vars, only: impurity_enrichment
    use divertor_ode_var, only: impurity_concs
    use impurity_radiation_module, only: imp_dat
    use reinke_variables, only: reinke_mode
    use read_radiation, only: read_lz
    use impurity_radiation_module, only: nimp, imp_label
    
    implicit none
    real(dp) :: reinke_fzmin
    real(dp) :: bt, flh, qstar, rmajor, eps, fsep, fgw, kappa
    real(dp) :: lhat, netau, tesep, ml_div, sum_fZ_ml_other, ml_z, lz
    real(dp), dimension(14) :: impurity_arr_frac
    real(dp), dimension(14) :: impurity_enrichment
    integer(kind=4) :: impvardiv

    integer(kind=4), parameter :: N = 100
    integer(kind=4) :: i, j
    real(dp) :: binWidth, te

    binWidth = tesep / N
    ! mL =1/tesep * \int_0^tesep L\(T) sqrt(T) dT using trapezoidal rule
    ! can't start exactly at 0, as no data available!

    !calculate m_L(T) for the impurity of interest
    ml_div = 0.0D0
    do j = 1, N, 1
       te = tesep*j/N * 1.0D3 !set binning, eV
       !calculate loss function
       lz = read_lz(imp_label(impvardiv), te, netau, mean_z=.false., mean_qz=.false., verbose=.false.)
       !calculate integral
       ml_div = ml_div +  lz  * sqrt(te) * binWidth
    enddo

    ml_div = ml_div * 1.0D30 !units 10^-30 Wm^3 eV^(1/2)
    ml_div = ml_div / tesep

    !calculate and sum m_L(T) for fixed fraction impurities, weighted by concentration
    ! Get impurity concentrations every time, as they can change
    do i = 2, nimp
       !concentration at SOL
       impurity_concs(i)= impurity_arr_frac(i) * impurity_enrichment(i)
    enddo

    ! \sum_Z fZ * mL(Z, netau)
    sum_fZ_ml_other = 0.0d0

    do i = 2, nimp
       !write(*,*) 'impurity array : ', i, ', ', impurity_arr_frac(i)
       if (i .ne. impvardiv) then

          ml_z = 0.0D0
          do j = 1, N
             te = tesep*j/N * 1.0D3 !eV
             lz = read_lz(imp_label(i), te, netau, mean_z=.false., mean_qz=.false., verbose=.false.)
             ml_z = ml_z + lz  * sqrt(te) * binWidth
          enddo

          ml_z = ml_z * 1.0D30 !units 10^-30 Wm^3 eV^(1/2)
          ml_z = ml_z / tesep

          sum_fZ_ml_other = sum_fZ_ml_other + impurity_concs(i) * ml_z

       endif
    enddo

    if(reinke_mode == 1) then
      ! I-mode
      reinke_fzmin = 0.069 * bt**0.57 * flh**1.14 * rmajor**1.14 * eps**0.66

      reinke_fzmin = reinke_fzmin / (fsep**2.0 * fgw**0.86)

      reinke_fzmin = reinke_fzmin / ((1.D0 + kappa**2.)**0.29 * lhat**0.86)

      !subtract radiation contribution from fixed fraction impurities, then complete the equation
      reinke_fzmin = (reinke_fzmin - sum_fZ_ml_other )/ ml_div

    else
      ! H-mode
      reinke_fzmin = 0.014 * bt**0.88 * flh**1.14 * qstar**0.32 * rmajor**1.33

      reinke_fzmin = reinke_fzmin * eps**0.59 / fsep**2. / fgw**1.18

      reinke_fzmin = reinke_fzmin / (1.D0 + kappa**2.)**0.64 / lhat**0.86

      !subtract radiation contribution from fixed fraction impurities, then complete the equation
      reinke_fzmin = (reinke_fzmin - sum_fZ_ml_other )/ ml_div

    end if  
   !return minimum fraction required at the SOL of specified impurity fraction
  end function reinke_fzmin

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function reinke_tsep(bt, flh, qstar, rmajor, eps, fgw, kappa, lhat)

    !! Function for calculating upstream temperature(keV) in Reinke model
    !! author: H Lux, CCFE/UKAEA
    !! bt      : input real : toroidal field on axis (T)
    !! flh     : input real : fraction of Psep/P_LH
    !! qstar   : input real : safety factor similar to q95 (see #707)
    !! rmajor  : input real : major radius (m)
    !! eps     : input real : inverse aspect ratio
    !! fgw     : input real : ratio of volume averaged density to n_GW
    !! kappa   : input real : elongation
    !! lhat    : input real : connection length factor
    !! This function calculates the upstream temperature in the
    !! divertor/SoL model used for the Reinke citerion.
    !! Issue #707
    !! M.L. Reinke 2017 Nucl. Fusion 57 034004

    implicit none
    real(dp) :: reinke_tsep
    real(dp) :: bt, flh, qstar, rmajor, eps, fgw, kappa, lhat
    real(dp), parameter :: kappa_0 = 2D3 !Stangeby W/m/eV^(7/2)

    reinke_tsep = bt**0.72 * flh**0.29 * fgw**0.21 * qstar**0.08 * rmajor**0.33
    !reinke_tsep = bt**0.72 * flh**0.2857 * fgw**0.2057 * qstar**0.08 * rmajor**0.3314

    reinke_tsep = reinke_tsep * eps**0.15 * (1.D0 + kappa**2.)**0.34
    !reinke_tsep = reinke_tsep * eps**0.1486 * (1.D0 + kappa**2.)**0.34

    reinke_tsep = reinke_tsep * lhat**0.29 * kappa_0 **(-0.29) * 0.285D0
    !reinke_tsep = reinke_tsep * lhat**0.2857 * kappa_0 **(-0.2857) * 0.285D0

  end function reinke_tsep

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_reinke()
    use impurity_radiation_module, only: imp_dat, imp_label
    implicit none

    real(dp) :: testResult_fZ_DEMOBASE, testResult_fZ_ASDEXBASE, testInput_tsep
    integer :: i, j
    real(dp), parameter :: test_Bt = 5.8547
    type(imp_dat),  dimension(14), save :: test_imp_arr
    real(dp), dimension(14) :: impurity_enrichment

    do i=1,14
       test_imp_arr(i)%frac = 0.0d0
       test_imp_arr(i)%label = imp_label(i)
       impurity_enrichment(i) = 1.0d0
    end do
    test_imp_arr(1)%frac = 1.0d0
    test_imp_arr(2)%frac = 0.1d0
    test_imp_arr(9)%frac = 0.001d0
    test_imp_arr(13)%frac = 4.4d-04
    test_imp_arr(14)%frac = 5d-05


    !testResult_tsep = reinke_tsep(test_Bt, 1.0d0, 3.0d0, 1.65d0, 0.33d0, 0.8d0, 1.7d0, 4.33d0)
    !                              bt, flh, qstar, rmajor, eps, fgw, kappa, lhat
    !write(*,*) 'reinke_tsep = ', testResult_tsep

   ! Open file to output the data for the test of the fzmin function.
    open(unit=1,file='FZMIN_TEST.DAT')

   ! We will output fzmin for a range of tesep values, using both a DEMO baseline and a ASDEX like parameters
   ! The ASDEX like fZ should be compariable with data in M.L. Reinke et al, 2017
    do j = 1,39
      testInput_tsep =0.02d0+0.02d0*j
      testResult_fZ_DEMOBASE = reinke_fzmin(test_Bt, 1.4114d0, 3.0d0, 9.0019d0, 0.3225d0, 0.5651d0, 0.8d0, 1.848d0, &
         4.33d0, 0.1d0, testInput_tsep, 9, test_imp_arr%frac, impurity_enrichment)
      testResult_fZ_ASDEXBASE = reinke_fzmin(test_Bt, 1.0d0, 3.0d0, 1.65d0, 0.33d0, 1.0d0, 0.8d0, 1.7d0, &
         4.33d0, 0.1d0, testInput_tsep, 9, test_imp_arr%frac, impurity_enrichment)
      write(1,*) testInput_tsep, testResult_fZ_DEMOBASE, testResult_fZ_ASDEXBASE, test_imp_arr(9)%frac
    end do

   close(unit=1)
   write(*,*) 'fz minium data written to FZMIN_TEST.DAT'

    !testResult_fZ = reinke_fzmin(test_Bt, 1.4114, 3.0d0, 9.0019d0, 0.3225d0, 1.0d0, 0.8d0, 1.848d0, &
    !      4.33d0, 0.1d0, testInput_tsep, 9, test_imp_arr%frac, impurity_enrichment)
                                  !bt, flh, qstar, rmajor, eps, fsep, fgw, kappa,
         !lhat, netau, impvardiv, impurity_arr%frac, impurity_enrichment
    !write(*,*) 'reinke_fzmin = ', testResult_fZ

    !if(testResult_fZ /= 1.4) then
      ! call report_error(217)
    !end if

    !if(testResult_tsep /= 1.4) then
       !call report_error(217)
    !end if

  end subroutine test_reinke



end module reinke_module
