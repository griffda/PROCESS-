! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module kallenbach_module
  !---------------------------------------------------------------------------
contains

!---------------------------------------------------------------------------
! This routine is outside the module, so it functions as test for the module.

subroutine kallenbach_test()
  !+ad_name  kallenbach_test
  !+ad_summ  Test for divertor kallenbach model
  !+ad_type  subroutine
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
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
  use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, ovarin, ovarre, ovarrf, ovarst

  implicit none

  integer :: i
  real(kind(1.0D0))::rmajor, rminor, bt, plascur, dummy, dummy2, dummy3

  ! This section just for reproducing the original numbers
  rmajor = 8.0D0
  rminor = 2.75D0
  bt = 4.00972D0*(rmajor + rminor)/rmajor
  plascur = 1.33542D0*(2.0D0*pi*rminor)/rmu0

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
  call ovarre(nout, 'q95 [A]','(q)', 3.0D0)

  ! Set the impurity fractions to the test values
  do i = 2, nimp
    impurity_arr(i)%frac = 0.0D0
  enddo

  impurity_arr(5)%frac = 8.0D-3  ! gives 0.04 in SOL, as in Kallenbach paper

  call divertor_Kallenbach(rmajor=rmajor, rminor=rminor,     &
                           bt=bt, plascur=plascur,      &
                           q=3.0D0,            &
                           verboseset=.false.,          &
                           ttarget=2.3D0,qtargettotal=4.175D6,                  &
                           targetangle=30.0D0,            &
                           unit_test=.false.,     &
                           bp = 0.956d0,   &
                           psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3, &
                           outfile=nout,iprint=1 )

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

end subroutine Kallenbach_test

!-------------------------------------------------------------------------
subroutine kallenbach_scan()
  !+ad_name  kallenbach_scan
  !+ad_summ  Scans for divertor kallenbach model: No optimisation
  !+ad_type  subroutine
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_hist  13/11/17 MDK  Initial version
  !
  ! Modify this code to run whatever scans are required (or none)
  ! The values of divertor_kallenbach_variables are taken from the input file,
  ! but can be overwritten here.
  use divertor_ode
  use divertor_kallenbach_variables
  use read_and_get_atomic_data
  use read_radiation
  use constants
  use physics_variables, only:rmajor, rminor, plascur, bp, bt, q, ralpne
  use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, ovarin, ovarre, ovarrf, ovarst

  implicit none

  integer :: i
  real(kind(1.0D0)):: dummy, dummy2, dummy3
  integer::isweep

  ! MDK Issue #494.
  ! Invert the equation for lcon to ensure lcon=100 for this test.
  ! lcon = lcon_factor * 0.395d0*pi*q*rmajor/lambda_omp**0.196
  lcon_factor = 100.0D0 / (0.395d0*pi*3.0d0*8.0D0/0.002D0**0.196)

  rmajor = 8.9384d0
  rminor = 2.883d0
  plascur = 19.075d6
  !bvert = -0.725d0
  bp = 0.921d0
  q = 3.000d0
  ttarget = 10.0d0

  ! Set the impurity fractions to the test values
  do i = 2, nimp
    impurity_arr(i)%frac = 0.0D0
  enddo
  impurity_arr(9)%frac  = 1.0d-3   ! argon
  impurity_arr(13)%frac = 2.0d-4   ! Xenon
  impurity_arr(14)%frac = 5d-05    ! Tungsten

  isweep = 21
  call oheadr(nout, 'Divertor: Kallenbach 1D Model - SCANS - ')
  call ovarin(mfile,'Number of scan points','(isweep)',isweep)
  call ovarin(mfile,'Scanning variable number','(nsweep)',33)

  do i = 1, isweep

      if(i==1) then
          ttarget = 1.0d0
      else
          ttarget = 5.0d0*(i - 1.0d0)
      endif
      !impurity_arr(9)%frac = 1.0d-3 + (i-1.0d0)/(isweep-1.0d0) *1.0d-3
      !write(*,*)'Running kallenbach model for argon = ',impurity_arr(9)%frac
      write(*,*)'Running kallenbach model for ttarget = ',ttarget
      call oblnkl(mfile)
      call ovarin(mfile,'Scan point number','(iscan)',i)
      call ovarre(nout, 'Major radius [m]','(rmajor)', rmajor)
      call ovarre(nout, 'Minor radius [m]','(rminor)', rminor)
      call ovarre(nout, 'Toroidal field [T]','(bt)', bt, 'OP ')
      call ovarre(nout, 'Average poloidal field [T]','(bp)', bp)
      call ovarre(nout, 'Plasma current [A]','(plascur)', plascur)
      call ovarre(nout, 'q95 [A]','(q)', q)
      call ovarre(nout, 'Helium ion density (thermalised ions only) / electron density','(ralpne)',ralpne)
      call ovarre(nout, 'Argon ion density / electron density','(fimp(9)',impurity_arr(9)%frac)
      call ovarre(nout, 'Xenon ion density / electron  density','(fimp(13)',impurity_arr(13)%frac)
      call ovarre(nout, 'Tungsten ion density / electron density','(fimp(14)',impurity_arr(14)%frac)

      call divertor_Kallenbach(rmajor=rmajor, rminor=rminor,     &
                           bt=bt, plascur=plascur,      &
                           q=q,            &
                           verboseset=.false.,          &
                           ttarget=ttarget, qtargettotal=qtargettotal,         &
                           targetangle=targetangle,    &
                           unit_test=.false.,  &
                           bp = bp,   &
                           psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3,  &
                           outfile=nout,iprint=1 )
   end do
end subroutine Kallenbach_scan

end module kallenbach_module

