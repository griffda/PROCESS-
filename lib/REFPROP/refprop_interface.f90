! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Uncomment #define line below to perform unit testing
!  Compile using pre-processor:
!   ifort -cpp -c refprop_interface.f90 refprop.f
!   ifort refprop_interface.o refprop.o
!
!#define unit_test
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module refprop_interface

  !! Interface routines between PROCESS and REFPROP
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains some interface routines to pass information
  !! between PROCESS and the REFPROP routines contained in <CODE>refprop.f</CODE>.
  !! REFPROP Version 9.1 User's Guide,
  !! Eric W. Lemmon, Marcia L. Huber, Mark O. McLinden,
  !! Applied Chemicals and Materials Division,
  !! National Institute of Standards and Technology,
  !! Boulder, Colorado 80305,
  !! April, 2013
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifndef unit_test
  use error_handling
#endif
  implicit none

  private
  public :: enthalpy_ps
  public :: fluid_properties
  public :: tsat_refprop

  logical :: initialised = .false.

  integer, parameter :: ncmax = 1
  real(kind(1.0D0)), dimension(ncmax) :: x = 1.0D0
  character(len=255) :: herr
  integer :: ierr
  real(kind(1.0D0)), public :: molarmass  

contains
  character(len=300) function fluids_dir()
   implicit none
   character(len=200) :: process_dir
   CALL get_environment_variable("PYTHON_PROCESS_ROOT", process_dir)
   if (process_dir == "") then
      fluids_dir = INSTALLDIR//'/process/data/fluids/'
   else
      fluids_dir = trim(process_dir)//'/data/fluids/'
   end if
  end function fluids_dir

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialise_refprop(fluid)

    !! Initialises the fluid data for future REFPROP calls
    !! author: P J Knight, CCFE, Culham Science Centre
    !! fluid : input integer : pure fluid to use; 1=helium, 2=water
    !! This routine initialises the REFPROP fluid arrays and common
    !! blocks, for a single (pure) fluid.
    !! It also calculates the fluid's molar mass for future use.
    !! <P>The routine need not be called explicitly by PROCESS, as it
    !! is called by each of the other PROCESS/REFPROP interfacing
    !! routines in this module; only the first call actually does
    !! anything other than simply return.
    !! REFPROP Version 9.1 User's Guide,
    !! Eric W. Lemmon, Marcia L. Huber, Mark O. McLinden,
    !! Applied Chemicals and Materials Division,
    !! National Institute of Standards and Technology,
    !! Boulder, Colorado 80305,
    !! April, 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: fluid

    !  Local variables

    integer :: nc = 1
    character(len=255) :: hfmix = 'hmx.bnc'
    character(len=3) :: hrf = 'DEF'
    character(len=255), dimension(ncmax) :: hf

    !  External functions

    real(kind(1.0D0)), external :: wmol

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (.not.initialised) then

       call setpath(trim((fluids_dir())))

       if (fluid == 1) then
          hf(1) = 'helium.fld'
       else
          hf(1) = 'water.fld'
       end if

       call setup(nc,hf,hfmix,hrf,ierr,herr)
       if (ierr /= 0) then
#ifndef unit_test
          idiags(1) = ierr
          call report_error(168)
#endif
          write(*,*) herr
       end if

       molarmass = wmol(x(1))  !  g/mol

       initialised = .true.
    end if

  end subroutine initialise_refprop

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fluid_properties(temperature,pressure,fluid, &
       density,enthalpy,entropy,specific_heat_const_p, &
       thermal_conductivity,viscosity, label)

    !! Calculates properties of a fluid given its temperature and
    !! pressure, using REFPROP calls
    !! author: P J Knight, CCFE, Culham Science Centre
    !! temperature : input real : fluid temperature (K)
    !! pressure : input real : fluid pressure (Pa)
    !! fluid : input integer : pure fluid to use; 1=helium, 2=water
    !! density : optional output real : fluid density (kg/m3)
    !! enthalpy : optional output real : fluid specific enthalpy (J/kg)
    !! entropy : optional output real : fluid entropy (J/kg/K)
    !! specific_heat_const_p : optional output real : fluid specific
    !! heat capacity at constant pressure (J/kg/K)
    !! thermal_conductivity : optional output real : fluid thermal conductivity (W/m/K)
    !! viscosity : optional output real : fluid viscosity (Pa.s)
    !! This routine calculates a number of properties of a fluid, given
    !! its temperature and pressure. It acts as an interface to
    !! REFPROP routines, ensuring that the correct units are passed.
    !! The quantities output may be chosen through the optional arguments.
    !! REFPROP Version 9.1 User's Guide,
    !! Eric W. Lemmon, Marcia L. Huber, Mark O. McLinden,
    !! Applied Chemicals and Materials Division,
    !! National Institute of Standards and Technology,
    !! Boulder, Colorado 80305,
    !! April, 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: temperature, pressure
    integer, intent(in) :: fluid
    real(kind(1.0D0)), optional, intent(out) :: density
    real(kind(1.0D0)), optional, intent(out) :: enthalpy
    real(kind(1.0D0)), optional, intent(out) :: entropy
    real(kind(1.0D0)), optional, intent(out) :: specific_heat_const_p
    real(kind(1.0D0)), optional, intent(out) :: thermal_conductivity
    real(kind(1.0D0)), optional, intent(out) :: viscosity
    character(len=*), optional, intent(in) :: label

    !  Local variables

    real(kind(1.0D0)) :: cp,cv,d,e,eta,h,p,q,s,t,tcx,w
    real(kind(1.0D0)), dimension(ncmax) :: dl,dv,xl,xv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ((temperature<100.0d0).or.(temperature>1500.0d0).or.(temperature/=temperature)) then
        write(*,*) 'temperature = ', temperature, 'near line ', label
    end if
    if ((pressure<1.0d5).or.(pressure>1.0d9).or.(pressure/=pressure)) then
        write(*,*) 'pressure = ', pressure, 'near line ', label
    end if

    call initialise_refprop(fluid)

    !  Input conversions

    p = pressure * 1.0D-3  !  Pa --> kPa
    t = temperature  !  K

    call tpflsh(t,p,x,d,dl,dv,xl,xv,q,e,h,s,cv,cp,w,ierr,herr)
    if (ierr /= 0) then
#ifndef unit_test
       idiags(1) = ierr
       call report_error(169)
#endif
       write(*,*) herr
    end if

    if (present(viscosity).or.present(thermal_conductivity)) then
       call trnprp(t,d,x,eta,tcx,ierr,herr)
       if (ierr /= 0) then
#ifndef unit_test
          idiags(1) = ierr
          call report_error(170)
#endif
          write(*,*) herr
       end if
    end if

    !  Convert requested outputs

    if (present(density)) then
       density = d * molarmass  !  mol/litre --> kg/m3
    end if

    if (present(enthalpy)) then
       enthalpy = h / (molarmass*1.0D-3)  !  J/mol --> J/kg
    end if

    if (present(entropy)) then
       entropy = s / (molarmass*1.0D-3)  !  J/mol/K --> J/kg/K
    end if

    if (present(specific_heat_const_p)) then
       specific_heat_const_p = cp / (molarmass*1.0D-3)  !  J/mol/K --> J/kg/K
    end if

    if (present(viscosity)) then
       viscosity = eta * 1.0D-6  !  uPa.s --> Pa.s
    end if

    if (present(thermal_conductivity)) then
       thermal_conductivity = tcx  !  W/m/K
    end if

  end subroutine fluid_properties

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine enthalpy_ps(pressure,entropy,fluid,enthalpy)

    !! Calculates the enthalpy of a fluid given its pressure and
    !! entropy, using REFPROP calls
    !! author: P J Knight, CCFE, Culham Science Centre
    !! pressure : input real : fluid pressure (Pa)
    !! entropy : input real : fluid entropy (J/kg/K)
    !! fluid : input integer : pure fluid to use; 1=helium, 2=water
    !! enthalpy : output real : fluid specific enthalpy (J/kg)
    !! This routine calculates a fluid's enthalpy, given its
    !! pressure and entropy. It acts as an interface to the relevant
    !! REFPROP routine, ensuring that the correct units are passed.
    !! REFPROP Version 9.1 User's Guide,
    !! Eric W. Lemmon, Marcia L. Huber, Mark O. McLinden,
    !! Applied Chemicals and Materials Division,
    !! National Institute of Standards and Technology,
    !! Boulder, Colorado 80305,
    !! April, 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: pressure, entropy
    integer, intent(in) :: fluid
    real(kind(1.0D0)), intent(out) :: enthalpy

    !  Local variables

    real(kind(1.0D0)) :: d,e,cv,w,q,t,cp,s,p,h
    real(kind(1.0D0)), dimension(ncmax) :: dl,dv,xl,xv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call initialise_refprop(fluid)

    !  Input conversions

    p = pressure * 1.0D-3  !  Pa --> kPa
    s = entropy * molarmass * 1.0D-3  !  J/kg/K --> J/mol/K

    call psflsh(p,s,x,t,d,dl,dv,xl,xv,q,e,h,cv,cp,w,ierr,herr)
    if (ierr /= 0) then
#ifndef unit_test
       idiags(1) = ierr
       call report_error(171)
#endif
       write(*,*) herr
    end if

    enthalpy = h / (molarmass*1.0D-3)  !  J/mol --> J/kg

  end subroutine enthalpy_ps

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function tsat_refprop(pressure, fluid)

    !! Calculates the liquid saturation temperature of a fluid
    !! given its pressure, using REFPROP calls
    !! author: P J Knight, CCFE, Culham Science Centre
    !! pressure : input real : fluid pressure (Pa)
    !! fluid : input integer : pure fluid to use; 1=helium, 2=water
    !! This routine calculates a fluid's liquid saturation temperature (K),
    !! given its pressure. It acts as an interface to the relevant
    !! REFPROP routine, ensuring that the correct units are passed.
    !! REFPROP Version 9.1 User's Guide,
    !! Eric W. Lemmon, Marcia L. Huber, Mark O. McLinden,
    !! Applied Chemicals and Materials Division,
    !! National Institute of Standards and Technology,
    !! Boulder, Colorado 80305,
    !! April, 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: tsat_refprop

    !  Arguments

    real(kind(1.0D0)), intent(in) :: pressure
    integer, intent(in) :: fluid

    !  Local variables

    real(kind(1.0D0)) :: p,ts
    real(kind(1.0D0)), dimension(ncmax) :: dl,dv,xl,xv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call initialise_refprop(fluid)

    !  Input conversion

    p = pressure * 1.0D-3  !  Pa --> kPa

    call satp(p,x,1,ts,dl,dv,xl,xv,ierr,herr)
    if (ierr /= 0) then
#ifndef unit_test
       idiags(1) = ierr
       call report_error(172)
#endif
       write(*,*) herr
    end if

    tsat_refprop = ts

  end function tsat_refprop

end module refprop_interface

! ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef unit_test
program refprop_test

  !  Compare the results with those for the same fluid at the
  !  same temperature and pressure, on the following NIST website:
  !
  !  http://webbook.nist.gov/chemistry/fluid/

  use refprop_interface

  implicit none

  integer :: coolwh = 2
  real(kind(1.0D0)) :: t,p,h,s,cp,eta,rho,kf,ts

  t = 293.0D0
  p = 1.01D5

  call fluid_properties(t,p,coolwh,enthalpy=h,entropy=s, &
       specific_heat_const_p=cp,viscosity=eta,density=rho, &
       thermal_conductivity=kf)

  write(*,*) ''
  if (coolwh == 1) then
     write(*,*) 'Fluid: Helium'
  else
     write(*,*) 'Fluid: Water'
  end if
  write(*,*) ''

  write(*,10) '   Temperature = ',t,' K'
  write(*,20) '      Pressure = ',p,' Pa, = ', &
       p*1.0D-6,' MPa'
  write(*,*) ''
  write(*,20) '      Enthalpy = ',h,' J/kg, = ', &
       h*molarmass*1.0D-6,' kJ/mol'
  write(*,20) '       Entropy = ',s,' J/kg/K, = ', &
       s*molarmass*1.0D-3,' J/mol/K'
  write(*,20) 'Spec heat (Cp) = ',cp,' J/kg/K, = ', &
       cp*molarmass*1.0D-3,' J/mol/K'
  write(*,10) '     Viscosity = ',eta,' Pa.s'
  write(*,20) '       Density = ',rho,' kg/m3, = ', &
       rho/molarmass,' mol/litre'
  write(*,10) ' Thermal cond. = ',kf,' W/m/K'
  ts = tsat_refprop(p,coolwh)
  write(*,10) '  Saturation T = ',ts,' K'

  write(*,*)
  call enthalpy_ps(p,s,coolwh,h)
  write(*,20) ' Enthalpy(p,s) = ',h,' J/kg, = ', &
       h*molarmass*1.0D-6,' kJ/mol'

10 format(a17,1pe15.6e2,a7)
20 format(a17,1pe15.6e2,a11,1pe15.6e2,a11)

end program refprop_test
#endif
