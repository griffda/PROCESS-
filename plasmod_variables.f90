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
  !+ad_vars  plasmod_globtau(5) /5.0d0/ : tauparticle/tauE for D, T, He, Xe, Ar
  real(kind(1.0D0)), dimension(5) :: plasmod_globtau = (/ 5.0d0, 5.0d0, 5.0d0, 5.0d0, 1.0d0 /)
  !plasmod_globtau(1) = 5.0d0
  !plasmod_globtau(2) = 5.0d0
  !plasmod_globtau(3) = 5.0d0
  !plasmod_globtau(4) = 5.0d0
  !plasmod_globtau(5) = 1.0d0
  
end module plasmod_variables
