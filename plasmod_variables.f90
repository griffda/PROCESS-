!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !Katy Ellis, created this file 20/02/18 to support the implementation of the
  !PLASMOD 1D transport model, coded by Emiliano Fable

module plasmod_variables
  !PLASMOD variables are defined via a structure, which may not be easy to
  !implement in the PROCESS data structure. Hence I will create dummy variables
  !here instead.

  implicit none
  
  public
  
 !Derived type numerics_transp 
  !+ad_vars  plasmod_tol /0.00001d0/ : Tolerance to be reached, in % variation at each time step
  real(kind(1.0D0)) :: plasmod_tol = 0.00001d0
  !+ad_vars  plasmod_dtmin /0.01d0/ : Min time step
  real(kind(1.0D0)) :: plasmod_dtmin = 0.01d0
  !+ad_vars  plasmod_dtmax /0.1d0/ : Max time step
  real(kind(1.0D0)) :: plasmod_dtmax = 0.1d0
  !+ad_vars  plasmod_dt /0.01d0/ : Time step
  real(kind(1.0D0)) :: plasmod_dt = 0.01d0
  !+ad_vars  plasmod_dtinc /2.0d0/ : Decrease of dt
  real(kind(1.0D0)) :: plasmod_dtinc = 2.0d0
  !+ad_vars  plasmod_ainc /1.1d0/ : Increase of dt
  real(kind(1.0D0)) :: plasmod_Ainc = 1.1d0
  !+ad_vars  plasmod_test /100000.0d0/ : Max iteration number
  real(kind(1.0D0)) :: plasmod_test = 100000.0d0
  !+ad_vars  plasmod_tolmin /10.1d0/ : Multiplier of etolm that should not be overcome
  real(kind(1.0D0)) :: plasmod_tolmin = 10.1d0
  !+ad_vars  plasmod_eopt /0.1d0/ : Exponent of jipperdo
  real(kind(1.0D0)) :: plasmod_eopt = 0.1d0
  !+ad_vars  plasmod_dtmaxmin /0.1d0/ : Exponent of jipperdo2
  real(kind(1.0D0)) :: plasmod_dtmaxmin = 0.1d0
  !+ad_vars  plasmod_capa /0.1d0/ : First radial grid point
  real(kind(1.0D0)) :: plasmod_capA = 0.1d0
  !+ad_vars  plasmod_maxa /0.0d0/ : Diagz 0 or 1
  real(kind(1.0D0)) :: plasmod_maxA = 0.0d0
  !+ad_vars  plasmod_dgy /1.0d-5/ : Newton differential
  real(kind(1.0D0)) :: plasmod_dgy = 1.0d-5
  !+ad_vars  plasmod_i_modeltype /1/ : 1 - Simple gyrobohm scaling
  integer :: plasmod_i_modeltype = 1
  !+ad_vars  plasmod_i_equiltype /1/ : 1 - EMEQ, solve equilibrium with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
  integer :: plasmod_i_equiltype = 1
  !+ad_vars  plasmod_nx /41/ : Number of interpolated grid points
  integer :: plasmod_nx = 41
  !+ad_vars  plasmod_nxt /7/ : Number of reduced grid points
  integer :: plasmod_nxt = 7
  !+ad_vars  plasmod_nchannels /3/ : Leave this at 3
  integer :: plasmod_nchannels = 3
  !+ad_vars  plasmod_ipedestal /2/ : 1 - fixed temperature pedestal. 2 - Sareelma scaling
  integer :: plasmod_ipedestal = 2
  !+ad_vars  plasmod_i_impmodel /1/ : Impurity model: 0 - fixed concentration, 1 - concentration fixed at pedestal top, then fixed density.
  integer :: plasmod_i_impmodel = 1

 !Derived type composition 
  !+ad_vars  plasmod_globtau(5) /5.0d0, 5.0d0, 5.0d0, 5.0d0, 1.0d0/ : tauparticle/tauE for D, T, He, Xe, Ar
  real(kind(1.0D0)), dimension(5) :: plasmod_globtau = (/ 5.0d0, 5.0d0, 5.0d0, 5.0d0, 1.0d0 /)
  !+ad_vars  plasmod_c_car /100.0d0/ : compression factor between div and core: e.g. 10 means there is 10 more Argon concentration in the divertor than in the core
  real(kind(1.0D0)) :: plasmod_c_car = 100.0d0
  !+ad_vars  plasmod_psepplh_inf /1.01d0/ : Psep/PLH if below this, use nbi
  real(kind(1.0D0)) :: plasmod_psepplh_inf = 1.01d0
  !+ad_vars  plasmod_psepplh_sup /12000.0d0/ : Psep/PLH if above this, use Xe
  real(kind(1.0D0)) :: plasmod_psepplh_sup = 12000.0d0
  !+ad_vars  plasmod_psep_r /12000.0d0/ : Psep/R max value
  real(kind(1.0D0)) :: plasmod_psep_r = 12000.0d0
  !+ad_vars  plasmod_qdivt /0.0d0/ : Divertor heat flux in MW/m^2, if 0, dont use SOL model
  real(kind(1.0D0)) :: plasmod_qdivt = 0.0d0  
  
 !Derived type inputs
  !+ad_vars  plasmod_qnbi_psepfac /50.0d0/ : dqnbi/d(1-Psep/PLH)
  real(kind(1.0D0)) :: plasmod_qnbi_psepfac = 50.0d0
  !+ad_vars  plasmod_cxe_psepfac /1.0d-4/ : dcxe/d(1-Psep/PLH)
  real(kind(1.0D0)) :: plasmod_cxe_psepfac = 1.0d-4
  !+ad_vars  plasmod_car_qdivt /1.0d-4/ : dcar/d(qdivt)
  real(kind(1.0D0)) :: plasmod_car_qdivt = 1.0d-4
    !deposition locations
  !+ad_vars  plasmod_x_heat(2) /0.0d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_heat = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_x_cd(2) /0.0d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_cd = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_x_fus(2) /0.0d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_fus = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_x_control(2) /0.0d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_control = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_dx_heat(2) /0.2d0, 0.03d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_heat = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_dx_cd(2) /0.2d0, 0.03/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_cd = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_dx_fus(2) /0.2d0, 0.03d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_fus = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_dx_control(2) /0.2d0, 0.03d0/ : Element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_control = (/ 0.2d0, 0.03d0 /)

  !+ad_vars  plasmod_nbi_energy /1000.0d0/ :: In keV
  real(kind(1.0D0)) :: plasmod_nbi_energy = 1000.0d0
  !+ad_vars  plasmod_v_loop /-1.0d-6/ :: Target loop voltage. If lower than -1.e5 do not use
  real(kind(1.0D0)) :: plasmod_v_loop = -1.0d-6
  !+ad_vars  plasmod_f_ni /0.0d0/ :: Required fraction of non inductive current. If 0 do not use CD
  real(kind(1.0D0)) :: plasmod_f_ni = 0.0d0
  !+ad_vars  plasmod_pfus /0.0d0/ :: If 0. not used (otherwise controlled with Pauxheat)
  real(kind(1.0D0)) :: plasmod_pfus = 0.0d0
  
end module plasmod_variables
