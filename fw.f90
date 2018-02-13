! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fw_module
  !+ad_name  fw_module
  !+ad_summ  Module containing first wall model
  !+ad_type  Module
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS first wall model
  !+ad_prob  None
  !+ad_hist  08/06/16 JM  Initial version of module
  !+ad_stat  Okay
  !+ad_docs  PROCESS Engineering paper (M. Kovari et al.)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Modules to import !
  !!!!!!!!!!!!!!!!!!!!!

  use constants
  use global_variables
  use fwbs_variables
  use refprop_interface
  use process_output

  implicit none

  ! Subroutine declarations !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!

  private
  public :: friction, heat_transfer, fw_thermal_conductivity, fw_temp

  ! Precision variable
  integer, parameter :: double = 8

  ! Variables for output to file

contains

  subroutine friction(reynolds, darcy_friction)
    !+ad_name  friction
    !+ad_summ  Calculate Darcy friction factor, using Haaland equation
    !+ad_type  function
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  reynolds : input real : Reynolds number
    !+ad_args  darcy_friction : output real : Darcy friction factor
    !+ad_desc  Darcy friction factor, using Haaland equation, an approximation to the
    !+ad_desc  implicit Colebrook–White equationGnielinski correlation.
    !+ad_prob  None
    !+ad_hist  26/05/16 JM  Initial tidied version
    !+ad_stat  Okay
    !+ad_docs  https://en.wikipedia.org/wiki/Darcy_friction_factor_formulae#Haaland_equation
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    !!!!!!!!!!!!!

    real(kind=double), intent(in) :: reynolds
    real(kind=double), intent(out) :: darcy_friction

    ! Local variables !
    !!!!!!!!!!!!!!!!!!!

    real(kind=double) :: bracket

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Bracketed term in Haaland equation
    bracket = (roughness/afw/3.7)**1.11d0 + 6.9/reynolds

    ! Calculate Darcy friction factor
    darcy_friction = (1.8d0 * log10(bracket))**(-2)

  end subroutine friction

  function heat_transfer(masflx, rhof, radius, cf, viscf, kf)
    !+ad_name  heat_transfer
    !+ad_summ  Calculate heat transfer coefficient using Gnielinski correlation
    !+ad_type  function
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  masflx : input real : coolant mass flux in a single channel (kg/m2/s)
    !+ad_args  rhof : input real : coolant density (average of inlet and outlet) (kg/m3)
    !+ad_args  radius : input real : coolant pipe radius (m)
    !+ad_args  cf : input real : coolant specific heat capacity (average of inlet and outlet) (J/K)
    !+ad_args  viscf : input real : coolant viscosity (average of inlet and outlet) (Pa.s)
    !+ad_args  kf : input real : thermal conductivity of coolant (average of inlet and outlet) (W/m.K)
    !+ad_desc  Gnielinski correlation. Ignore the distinction between wall and
    !+ad_desc  bulk temperatures. Valid for:3000 < Re < 5e6, 0.5 < Pr < 2000
    !+ad_prob  None
    !+ad_hist  26/05/16 JM  Initial tidied version
    !+ad_stat  Okay
    !+ad_docs  https://en.wikipedia.org/wiki/Nusselt_number#Gnielinski_correlation
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    !!!!!!!!!!!!!

    ! Function output: Heat transfer coefficient (W/m2K)
    real(kind=double) :: heat_transfer

    ! Coolant mass flux in a single channel (kg/m2/s)
    real(kind=double) :: masflx

    ! Coolant density (average of inlet and outlet) (kg/m3)
    real(kind=double) :: rhof

    ! Coolant pipe radius (m)
    real(kind=double) :: radius

    ! Coolant specific heat capacity (average of inlet and outlet) (J/K)
    real(kind=double) :: cf

    ! Coolant viscosity (average of inlet and outlet) (Pa.s)
    real(kind=double) :: viscf

    ! Thermal conductivity of coolant (average of inlet and outlet) (W/m.K)
    real(kind=double) :: kf

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Local variables !
    !!!!!!!!!!!!!!!!!!!

    ! Calculate flow velocity (m/s)
    real(kind=double) :: velocity

    ! Reynolds number
    real(kind=double) :: reynolds

    ! Prandtl number
    real(kind=double) :: pr

    ! Darcy friction factor
    real(kind=double) :: f

    ! Nusselt number
    real(kind=double) :: nusselt

    ! Pipe diameter (m)
    real(kind=double) ::diameter

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate pipe diameter (m)
    diameter = 2*radius

    ! Calculate flow velocity (m/s)
    velocity = masflx / rhof

    ! Calculate Reynolds number
    reynolds = rhof * velocity * diameter / viscf

    ! Calculate Prandtl number
    pr = cf * viscf / kf

    ! Calculate Darcy friction factor, using Haaland equation
    call friction(reynolds, f)

    ! Calculate the Nusselt number
    nusselt = (f/8.0d0)*(reynolds-1000.0d0)*pr / (1+12.7*sqrt(f/8.0d0)*(pr**0.6667-1.0d0))

    ! Calculate the heat transfer coefficient (W/m2K)
    heat_transfer = nusselt * kf / (2.0d0*radius)

    ! Check that Reynolds number is in valid range for the Gnielinski correlation
    if ((reynolds <= 3000.0d0).or.(reynolds>5.0d6)) call write_errors

    ! Check that Prandtl number is in valid range for the Gnielinski correlation
    if ((f<=0.0d0).or.(pr<=0.0d0).or.(pr<0.5d0).or.(pr>2000.0d0)) call write_errors

    contains

      subroutine write_errors
        write(*,*)'Problem in heat_transfer'
        write(*,*)'masflx = ', masflx
        write(*,*)'rhof = ', rhof
        write(*,*)'radius = ', radius
        write(*,*)'cf = ', cf
        write(*,*)'kf = ', kf
        write(*,*)'velocity = ', velocity
        write(*,*)'reynolds = ', reynolds
        write(*,*)'Prandtl = ', pr
        write(*,*)'nusselt = ', nusselt
        write(*,*)'heat_transfer = ', heat_transfer
      end subroutine

  end function heat_transfer

  function fw_thermal_conductivity(t)
    !+ad_name  fw_thermal_conductivity
    !+ad_summ  Calculates the thermal conductivity of the first wall
    !+ad_type  Function returning real
    !+ad_cont  None
    !+ad_args  t : input real : property temperature (K)
    !+ad_desc  Calculates the thermal conductivity of Eurofer (W/m/K).
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  04/09/14 PJK Added Eurofer steel fit
    !+ad_hist  01/06/15 MDK Convert to Kelvin. Added user-defined multiplier fw_th_conductivity
    !+ad_stat  Okay
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Function return value: thermal conductivity of first wall (W/m.K)
    real(kind=double) :: fw_thermal_conductivity

    ! Arguments !
    !!!!!!!!!!!!!

    real(kind=double), intent(in) :: t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Eurofer correlation, from "Fusion Demo Interim Structural Design Criteria -
    ! Appendix A Material Design Limit Data", F. Tavassoli, TW4-TTMS-005-D01, 2004
    ! t in Kelvin
    fw_thermal_conductivity = (5.4308D0 + 0.13565D0*t - 0.00023862D0*t*t + 1.3393D-7*t*t*t)*fw_th_conductivity/28.34D0

  end function fw_thermal_conductivity

  subroutine fw_temp(ip, ofile, afw, thickness, area, prad_incident, pnuc_deposited, tpeakfw, &
                     cfmean, rhofmean, massrate, label)
    !+ad_name  fw_temp
    !+ad_summ  Thermo-hydraulic calculations for the first wall
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  afw : input real : first wall coolant channel radius (m)
    !+ad_args  thickness : first wall thickness (fwith or fwoth) (m)
    !+ad_args  area : input real : area of first wall section under consideration (m2)
    !+ad_argc                      (i.e. area of inboard wall or outboard wall)
    !+ad_args  prad_incident : input real : Surface heat flux on first wall (outboard and inboard) (MW)
    !+ad_args  pnuc_deposited : input real : nuclear power deposited in FW (IB or OB) (MW)
    !+ad_args  tpeakfw : output real : peak first wall temperature (K)
    !+ad_args  cfmean : output real : coolant specific heat capacity at constant
    !+ad_argc                     pressure (J/kg/K)
    !+ad_args  rhofmean : output real : coolant density (kg/m3)
    !+ad_args  massrate : output real : coolant mass flow rate in a single channel (kg/s)
    !+ad_args  label : input string : information string
    !+ad_desc  Detailed thermal hydraulic model for the blanket (first wall +
    !+ad_desc  breeding zone).
    !+ad_desc  Given the heating incident on the first wall, and the coolant
    !+ad_desc  outlet temperature, the maximum temperature of the first wall is
    !+ad_desc  calculated to check it is below material limits (tfwmatmax).
    !+ad_desc  The routine is called separately for the inboard and outboard sides.
    !+ad_prob  None
    !+ad_hist  21/08/14 PJK Initial version
    !+ad_hist  05/11/14 PJK Corrected position of fwlifs evaluation
    !+ad_hist  27/11/15 MDK Complete rewrite with a non-iterative approach
    !+ad_stat  Okay
    !+ad_docs  The calculation of the maximum temperature is described by Gardner:
    !+ad_docc  "Temperature distribution in the first wall", K:\Power Plant Physics and
    !+ad_docc  Technology\ PROCESS\PROCESS References & Systems Codes\Pulsed option -
    !+ad_docc  Gardner.
    !+ad_docs  This is in turn taken from "Methods of First Wall Structural
    !+ad_docc  Analysis with Application to the Long Pulse Commercial Tokamak Reactor
    !+ad_docc  Design", R.J. LeClaire, MIT, PFC/RR-84-9
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    !!!!!!!!!!!!!

    integer, intent(in) :: ip, ofile
    real(kind=double), intent(in) :: pnuc_deposited, afw, thickness, area, prad_incident
    real(kind=double), intent(out) ::  tpeakfw, cfmean, rhofmean, massrate
    character(len=*) :: label

    ! Local variables !
    !!!!!!!!!!!!!!!!!!!

    integer :: coolant

    ! FW volume (inboard or outboard depending on arguments) (m3)
    real(kind=double) :: fwvol

    ! Heat transfer coefficient (W/m2K)
    real(kind=double) :: hcoeff

    ! thermal conductivity of inlet coolant, outlet coolant and mean (W/m.K)
    real(kind=double) :: kfi, kfo, kfmean

    ! Heat flux incident on the first wall surface (W/m2)
    real(kind=double) :: qpp

    ! Heat generation in the first wall due to neutron flux deposited in the material (W/m3)
    real(kind=double) :: qppp

    ! Mean temperature of the wall material on the plasma side of the coolant (K)
    real(kind=double) :: temp_k

    ! Viscosity of the inlet coolant, outlet coolant and average coolant (Pa.s)
    real(kind=double) :: viscfi, viscfo, viscfmean

    ! Heat load per unit length of one first wall pipe (MW/m)
    real(kind=double) :: load

    ! Thermal conductivity of first wall material (W/m.K)
    real(kind=double) :: tkfw

    ! Temperature drop in first-wall material (K)
    real(kind=double) :: deltat_solid

    ! Temperature drop between channel inner wall and bulk coolant. (K)
    real(kind=double) :: deltat_coolant

    ! First wall channel cross-sectional area (m2)
    real(kind=double) :: channel_area

    ! Specific heat capacity for inlet and outlet coolant (J/K)
    real(kind=double) :: cfi, cfo

    ! Density of inlet and outlet coolant (kg/m3)
    real(kind=double) :: rhofi, rhofo

    ! Outlet coolant velocity (m/s)
    real(kind=double) :: velocity

    ! Temperature drop in first-wall material (Model B)
    real(kind=double) :: deltat_solid_1D

    ! Worst case load (as above) per unit length in 1-D calculation (Model B)
    real(kind=double) :: onedload

    ! Effective area for heat transfer (m2)
    real(kind=double) :: effective_area_for_heat_transfer

    ! Maximum distance travelled by surface heat load (m)
    real(kind=double) :: diagonal

    ! Mean distance travelled by surface heat (m)
    real(kind=double) :: mean_distance

    ! Mean heat spread width (m)
    real(kind=double) :: mean_width

    ! Coolant mass flux in a single channel (kg/m2/s)
    real(kind=double) :: masflx

    ! Mean nuclear power deposited in first wall per unit area (W/m2)
    real(kind=double) :: nuclear_heat_per_area

    ! TODO sort out these variables and model A, B, C order and labels.
    ! Variables for the old FW temperature model (LeClaire)
    ! real(kind=double) :: tmthet
    ! first wall pipe outer radius (m)
    ! real(kind=double) :: bfw
    ! Coolant viscosity (Pa.s)
    ! real(kind=double) :: viscf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Catch negative values of nuclear power deposited of NaN/Inf
    if ((pnuc_deposited/=pnuc_deposited).or.(pnuc_deposited<0.0d0)) then
        write(*,*)'pnuc_deposited =', pnuc_deposited, ' at ', label
        write(*,*)'prad_incident =', prad_incident, ' at ', label
    end if

    ! First wall volume (inboard or outboard depending on arguments) (m3)
    fwvol = area*thickness

    ! First wall channel area (m2)
    channel_area = pi * afw**2

    ! Heat generation in the first wall due to neutron flux deposited in the material (W/m3)
    qppp = 1.0D6 * pnuc_deposited / fwvol

    ! Note that the full first wall volume is used including coolant even though
    ! the nuclear heating in the coolant is small. (W/m2)
    nuclear_heat_per_area = qppp * thickness

    ! Heat flux incident on the first wall surface (W/m2)
    qpp = 1.0D6 * prad_incident / area

    ! Determine coolant type: helium or water
    if (fwcoolant == 'helium') coolant=1
    if (fwcoolant == 'water') coolant=2

    ! Calculate inlet coolant fluid properties (fixed pressure)
    call fluid_properties(fwinlet, fwpressure, coolant, density=rhofi, thermal_conductivity=kfi, &
      viscosity=viscfi, specific_heat_const_p=cfi, label='1477')

    ! Calculate outlet coolant fluid properties (fixed pressure)
    call fluid_properties(fwoutlet, fwpressure, coolant, density=rhofo, thermal_conductivity=kfo, &
      viscosity=viscfo, specific_heat_const_p=cfo, label='1480')

    ! Mean properties (inlet + outlet)/2
    rhofmean  = (rhofi + rhofo)/2.0d0     ! coolant density (kg/m3)
    kfmean    = (kfi + kfo)/2.0d0         ! coolant thermal conductivity (W/m.K)
    viscfmean = (viscfi + viscfo)/2.0d0   ! coolant viscosity (Pa.s)
    cfmean    = (cfi + cfo)/2.0d0         ! coolant specific heat capacity (J/K)

    ! Heat load per unit length of one first wall pipe (W/m)
    load = (nuclear_heat_per_area + qpp) * pitch

    ! Coolant mass flow rate (kg/s) (use mean properties)
    massrate = fw_channel_length * load / cfmean / (fwoutlet - fwinlet)

    ! Coolant mass flux in a single channel (kg/m2/s)
    masflx = massrate / channel_area

    ! Conditions at the outlet, where the temperature is highest
    ! -----------------------------------------------------------

    ! Outlet coolant velocity (m/s)
    velocity = masflx/rhofo

    ! Mean temperature of the wall material on the plasma side of the coolant 'tpeak'
    ! is the estimate from the previous iteration of the wall surface temperature
    ! (underneath the armour)
    temp_k = (fwoutlet + tpeak) / 2.0d0   ! (K)

    ! Print debug info if temperature too low/high or NaN/Inf
    if ((temp_k <= 100.0d0).or.(temp_k>1500.0d0).or.(temp_k/=temp_k)) then
        write(*,*) 'temp_k = ', temp_k, 'fwoutlet = ', fwoutlet, 'tpeak = ', tpeak
        stop
    end if

    ! Thermal conductivity of first wall material (W/m.K)
    tkfw = fw_thermal_conductivity(temp_k)

    ! Heat transfer coefficient (W/m2K)
    hcoeff = heat_transfer(masflx, rhofo, afw, cfo, viscfo, kfo)

    ! Temperature drops between first-wall surface and bulk coolant !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Model C is used
    ! Model B is given for comparison
    ! Model A is not used.

    ! Model A: LeClaire formula for circular pipes (NOT USED) !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! This gives a very small temperature differential, as the wall thickness is
    ! the same all the way round, but the surface area is bigger than that of a flat wall.

    ! Calculate peak temperature - occurs at (r,theta) = (bfw,0)
    ! bfw = thickness/2.0d0

    ! call cosine_term(afw, bfw, 0.0D0, bfw, qpp, hcoeff, tkfw, tmthet)

    ! deltat_solid = bfw/tkfw * (qpp/pi + qppp*bfw/2.0D0) * log(bfw/afw) &
    !              - qppp/4.0D0/tkfw*(bfw**2-afw**2)

    ! tpeakfw = fwoutlet + deltat_solid + deltat_coolant + tmthet  !  in K

    ! Model B: Simple 1-dimensional calculation !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! This is optimistic as it neglects the higher temperature midway between the channels.
    ! I have included 1/4 of the volume load:
    ! 1/2 is absorbed in the plasma-facing wall (A)
    ! which on average has to pass through 1/2 the wall thickness.
    !   ______________
    !         A
    !   --------------
    !      'channel'
    !   --------------
    !   ______________
    !
    ! Worst case load (as above) per unit length in 1-D calculation (W/m)
    onedload = peaking_factor * (qppp * pitch * thickness / 4.0d0 + qpp * pitch )

    ! Note I do NOT assume that the channel covers the full width of the first wall:
    effective_area_for_heat_transfer = 2 * afw

    ! Temperature drop in first-wall material (K)
    deltat_solid_1D = onedload * fw_wall / (tkfw * effective_area_for_heat_transfer)

    ! Model C: A more realistic model !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate maximum distance travelled by surface heat load (m)
    ! fw_wall | Minimum distance travelled by surface heat load (m)
    diagonal = sqrt((pitch/2.0d0-afw)**2 + (afw + fw_wall)**2)

    ! Mean distance travelled by surface heat (m)
    mean_distance = (fw_wall+diagonal)/2.0d0

    ! This heat starts off spread over width = 'pitch'.
    ! It ends up spread over one half the circumference.
    ! Use the mean of these values.
    mean_width = (pitch + pi*afw) / 2.0d0   ! (m)

    ! As before, use a combined load 'onedload'
    ! Temperature drop in first-wall material (K)
    deltat_solid = onedload * mean_distance / (tkfw * mean_width)

    ! Temperature drop between channel inner wall and bulk coolant (K)
    deltat_coolant = load /(2.0D0*pi*afw*hcoeff)

    ! Peak first wall temperature (K)
    tpeakfw = fwoutlet + deltat_solid + deltat_coolant

    ! If coolant mass flux in a single channel is NaN/Inf or negative give debug info
    if ((masflx/=masflx).or.(masflx<0.0d0)) then
        write(*,*)'masflx = ', masflx, ' at ', label
        ! TODO - have this work as originall intended
        ! write(*,*)'masflx = ', masflx, ' at ', label ; ip = 1
    end if

    ! Output !
    !!!!!!!!!!

    if (ip == 0) return
    call oheadr(ofile, 'Heat transfer parameters at the coolant outlet: ' // label)
    call ovarre(ofile, 'Radius of coolant channel (m)', '(afw)', afw)
    call ovarre(ofile, 'Mean surface heat flux on first wall (W/m2) ', '(qpp)', qpp, 'OP ')
    call ovarre(ofile, 'Mean nuclear power deposited in first wall per unit area (W/m2)', '', nuclear_heat_per_area, 'OP ')
    call ovarre(ofile, 'Ratio of peak local heat load (surface and nuclear) to mean', '(peaking_factor)', peaking_factor)
    call ovarre(ofile, 'Length of a single coolant channel (all in parallel) (m)', '(fw_channel_length)', fw_channel_length)
    call ovarre(ofile, 'Pitch of coolant channels (m)', '(pitch)', pitch)
    call ovarre(ofile, 'Thermal conductivity of first wall material (W/K/m)', '(tkfw)', tkfw, 'OP ')
    call ovarre(ofile, 'Coolant density (kg/m3)', '(rhofo)', rhofo, 'OP ')
    call ovarre(ofile, 'Coolant mass flow rate in one channel (kg/s)', '(massrate)', massrate, 'OP ')
    call ovarre(ofile, 'Coolant velocity (m/s)', '(velocity)', velocity, 'OP ')
    call ovarre(ofile, 'Outlet temperature of first wall coolant (K)', '(fwoutlet)', fwoutlet)
    call ovarre(ofile, 'Heat transfer coefficient', '(hcoeff)', hcoeff, 'OP ')
    call ovarre(ofile, 'Temperature drop in the wall material (simple model)', '(deltat_solid)', deltat_solid, 'OP ')
    call ovarre(ofile, 'Temperature drop in the coolant (wall to bulk)', '(deltat_coolant)', deltat_coolant, 'OP ')
    call ovarre(ofile, 'First wall temperature (excluding armour) (K)', '(tpeakfw)', tpeakfw, 'OP ')
    call ovarre(ofile, 'Temperature drop in the wall material: 1D estimate', '(deltat_solid_1D)', deltat_solid_1D, 'OP ')

  end subroutine fw_temp

end module
