module reinke_module
    use impurity_radiation_module


!  use constants
!  use error_handling
!  use impurity_radiation_module
!  use maths_library
!  use physics_variables
!  use profiles_module
!  use read_and_get_atomic_data
   use reinke_variables

  implicit none

  !private

  !  Module-level variables

  !integer ::
  real(kind(1.0D0)) :: vcritx

contains


  function reinke_fzmin(bt, flh, qstar, rmajor, eps, fsep, fgw, kappa, lhat, &
       netau, tesep, impvardiv, impurity_arr, impurity_enrichment)

!    use divertor_ode, only: impurity_concs
    use divertor_ode_var, only: impurity_concs
    use read_radiation
!    use divertor_kallenbach_variables, only: impurity_enrichment
    !+ad_name  reinke_fzmin
    !+ad_summ  Function for calculation of Reinke minimum impurity fraction
    !+ad_type  Function returning real
    !+ad_auth  H Lux, CCFE/UKAEA
    !+ad_cont  N/A
    !+ad_args  bt                  : input real : toroidal field on axis (T)
    !+ad_args  flh                 : input real : fraction of Psep/P_LH
    !+ad_args  qstar               : input real : safety factor similiar to q95 (see #707)
    !+ad_args  rmajor              : input real : major radius (m)
    !+ad_args  eps                 : input real : inverse aspect ratio
    !+ad_args  fsep                : input real : ratio of separatrix to vol. av. density
    !+ad_args  fgw                 : input real : ratio of volume averaged density to n_GW
    !+ad_args  kappa               : input real : elongation
    !+ad_args  lhat                : input real : connection length factor
    !+ad_args  netau               : input real : "non-coronal parameter" for radiative loss func [ms.1e20/m3]
    !+ad_args  tesep               : input real : temperature "upstream", i.e. at separatrix [eV]
    !+ad_args  impvardiv           : input real : impurity index, e.g. 7 is Argon
    !+ad_args  impurity_arr        : input imp_dat array : impurity fractions
    !+ad_args  impurity_enrichment : input real array : enrichment factors between SOL and core
    !+ad_desc  This function calculates the lower limit of the impurity fraction
    !+ad_desc  needed in the SOL for divertor protection. It has huge uncertainties in netau.
    !+ad_desc  Call the reinke_tsep function first then use as an argument to this function
    !+ad_desc  Issue #707
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  16/05/18 HL Initial version of function
    !+ad_hist  19/07/18 KE Testing and bug fixes
    !+ad_stat  Okay
    !+ad_docs  M.L. Reinke 2017 Nucl. Fusion 57 034004
    implicit none
    real(kind(1.0D0)) :: reinke_fzmin
    real(kind(1.0D0)) :: bt, flh, qstar, rmajor, eps, fsep, fgw, kappa
    real(kind(1.0D0)) :: lhat, netau, tesep, ml_div, sum_fZ_ml_other, ml_z, lz
    type(imp_dat), dimension(14) :: impurity_arr
    real(kind(1.0D0)), dimension(14) :: impurity_enrichment
    integer(kind=4) :: impvardiv

    integer(kind=4) :: N = 100
    integer(kind=4) :: i, j
    real(kind(1.0D0)) :: binWidth, te

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
       impurity_concs(i)= impurity_arr(i)%frac * impurity_enrichment(i)
    enddo

    ! \sum_Z fZ * mL(Z, netau)
    sum_fZ_ml_other = 0.0d0

    do i = 2, nimp
       !write(*,*) 'impurity array : ', i, ', ', impurity_arr(i)%frac
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

    !+ad_name  reinke_tsep
    !+ad_summ  Function for calculating upstream temperature(keV) in Reinke model
    !+ad_type  Function returning real
    !+ad_auth  H Lux, CCFE/UKAEA
    !+ad_cont  N/A
    !+ad_args  bt      : input real : toroidal field on axis (T)
    !+ad_args  flh     : input real : fraction of Psep/P_LH
    !+ad_args  qstar   : input real : safety factor similar to q95 (see #707)
    !+ad_args  rmajor  : input real : major radius (m)
    !+ad_args  eps     : input real : inverse aspect ratio
    !+ad_args  fgw     : input real : ratio of volume averaged density to n_GW
    !+ad_args  kappa   : input real : elongation
    !+ad_args  lhat    : input real : connection length factor
    !+ad_desc  This function calculates the upstream temperature in the
    !+ad_desc  divertor/SoL model used for the Reinke citerion.
    !+ad_desc  Issue #707
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/05/18 HL Initial version of function
    !+ad_hist  19/07/18 KE Testing and correction on constant value
    !+ad_stat  Okay
    !+ad_docs  M.L. Reinke 2017 Nucl. Fusion 57 034004

    real(kind(1.0D0)) :: reinke_tsep
    real(kind(1.0D0)) :: bt, flh, qstar, rmajor, eps, fgw, kappa, lhat
    real(kind(1.0D0)) :: kappa_0 = 2D3 !Stangeby W/m/eV^(7/2)


    reinke_tsep = bt**0.72 * flh**0.29 * fgw**0.21 * qstar**0.08 * rmajor**0.33
    !reinke_tsep = bt**0.72 * flh**0.2857 * fgw**0.2057 * qstar**0.08 * rmajor**0.3314

    reinke_tsep = reinke_tsep * eps**0.15 * (1.D0 + kappa**2.)**0.34
    !reinke_tsep = reinke_tsep * eps**0.1486 * (1.D0 + kappa**2.)**0.34

    reinke_tsep = reinke_tsep * lhat**0.29 * kappa_0 **(-0.29) * 0.285D0
    !reinke_tsep = reinke_tsep * lhat**0.2857 * kappa_0 **(-0.2857) * 0.285D0

  end function reinke_tsep

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  subroutine test_reinke()
!    real(kind(1.0D0)) :: testResult_fZ, testResult_tsep
!    integer :: i, element = 5
!    real(kind(1.0D0)) :: test_Bt = 12.0
!    type(imp_dat),  dimension(14), save :: test_imp_arr
!    real(kind(1.0D0)), dimension(14) :: impurity_enrichment

!    do i=1,14
!       test_imp_arr(i)%frac = 0.0d0
!       test_imp_arr(i)%label = imp_label(i)
!       impurity_enrichment(i) = 1.0d0
!    end do
!    test_imp_arr(14)%frac = 0.005d0

!    testResult_tsep = reinke_tsep(test_Bt, 1.0d0, 3.0d0, 1.65d0, 0.33d0, 0.8d0, 1.7d0, 4.33d0)
                                  !bt, flh, qstar, rmajor, eps, fgw, kappa, lhat
!    write(*,*) 'reinke_tsep = ', testResult_tsep

!    testResult_fZ = reinke_fzmin(test_Bt, 1.0d0, 3.0d0, 1.65d0, 0.33d0, 1.0d0, 0.8d0, 1.7d0, &
!         4.33d0, 0.1d0, testResult_tsep, element, test_imp_arr, impurity_enrichment)
                                  !bt, flh, qstar, rmajor, eps, fsep, fgw, kappa,
         !lhat, netau, impvardiv, impurity_arr, impurity_enrichment
!    write(*,*) 'reinke_fzmin = ', testResult_fZ

!    if(testResult_fZ /= 1.4) then
!       call report_error(217)
!    end if

!    if(testResult_tsep /= 1.4) then
!       call report_error(217)
!    end if

!  end subroutine test_reinke



end module reinke_module
