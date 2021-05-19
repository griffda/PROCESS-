! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module water_use_module

  !! Module containing estimations of water usage in secondary cooling.
  !!
  !! author: R Chapman, UKAEA
  !! 
  !! This module contains routines for calculating the water used during 
  !! operation of a fusion power plant within the secondary cooling system, 
  !! i.e. post electricity generation. This water usage depends largely on the 
  !! heat involved, and is thus based upon standard thermoelectric power plant operation.
  !! 
  !! Reference: Diehl et al. Methods for estimating water consumption for thermoelectric
  !! power plants in the United States: U.S. Geological Survey Scientific
  !! Investigations Report 2013–5188, 78 p. http://dx.doi.org/10.3133/sir20135188
  !!
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none
  
  private
  public :: waterusecall

  real(dp), parameter :: secday = 86400.0D0
  !! seconds in a day, s

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine waterusecall(outfile,iprint)

    !! Routine to call the water usage calculation routines.
    !! author: R Chapman, UKAEA
    !! outfile : input integer : Fortran output unit identifier
    !! iprint : input integer : Switch to write output (1=yes)
    !! This routine calls the different water usage routines.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use heat_transport_variables, only: pthermmw, etath
    !! need local plant_power variable rejected_main; can find from pthermmw & etath
    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    integer :: icool   
    !! switch between different water-body cooling options
    real(dp) :: rejected_heat
    !! heat rejected by main power conversion circuit (MW)
    real(dp) :: wastethermeng
    !! waste thermal energy to be rejected per [time], MJ

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   !  !! From plan_power.f90:
   !  ! Heat rejected by main power conversion circuit (MW)
   !  rejected_main = pthermmw * (1 - etath)
    rejected_heat = pthermmw * (1 - etath)

    wastethermeng = rejected_heat * secday

    !! call subroutines for cooling mechanisms:

    ! cooling towers 
    call cooling_towers(outfile,iprint,wastethermeng)

    ! water-body cooling
    icool = 1  ! small pond as a cooling body
    call cooling_water_body(outfile,iprint,icool,wastethermeng)
    icool = 2  ! large lake or reservoir as a cooling body
    call cooling_water_body(outfile,iprint,icool,wastethermeng)
    icool = 3  ! stream or river as a cooling body
    call cooling_water_body(outfile,iprint,icool,wastethermeng)


  end subroutine waterusecall

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cooling_towers(outfile,iprint,wastetherm)
   
    !! Water evaporated in cooling towers
    !! author: R Chapman, UKAEA
    !! outfile : input integer : Fortran output unit identifier
    !! iprint : input integer : Switch to write output (1=yes)
    !! wastetherm : input real : thermal energy (MJ) to be cooled by this system
   
    use water_usage_variables, only: airtemp, waterdens, latentheat, &
      volheat, evapratio, evapvol, energypervol, volperenergy
    use process_output, only: oheadr, ocmmnt, ovarre
    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(dp), intent(in) :: wastetherm

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! find evaporation ratio: ratio of the heat used to evaporate water 
    !   to the total heat discharged through the tower
    evapratio = 1 - ( (-0.000279*airtemp**3 &
       + 0.00109*airtemp**2 &
       - 0.345*airtemp &
       + 26.7) /100)
    !! Diehl et al. USGS Report 2013–5188, http://dx.doi.org/10.3133/sir20135188

    volheat = waterdens * latentheat

    energypervol = volheat / evapratio

    volperenergy = 1 / energypervol * 1000000
    
    evapvol = wastetherm * volperenergy

    !  Output section
    if (iprint == 0) return
    call oheadr(outfile,'Water usage during plant operation (secondary cooling)')
    call ocmmnt(outfile,'Estimated amount of water evaporated through different cooling system options:')
    call ocmmnt(outfile,'1. Cooling towers')
    call ocmmnt(outfile,'2. Water bodies: a) pond, b) lake, c) river')
    call ovarre(outfile,'Volume evaporated in cooling tower (m3/day)','(evapvol)',evapvol, 'OP ')

 
  end subroutine cooling_towers

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cooling_water_body(outfile,iprint,icool,wastetherm)
   
   !! Water evaporated in cooling through water bodies
   !! Based on spreadsheet from Diehl et al. USGS Report 2013–5188, which includes 
   !! cooling coefficients found through fits across a dataset containing a wide range of 
   !! temperatures, windspeeds, and heat loading: 
   !! http://pubs.usgs.gov/sir/2013/5188/appendix/sir2013-5188_appendix4_fews_version_3.104.xlsx
   !! 
   !! author: R Chapman, UKAEA
   !! outfile : input integer : Fortran output unit identifier
   !! iprint : input integer : Switch to write output (1=yes)
   !! icool: input integer : switch between different water-body cooling options
   !! wastetherm : input real : thermal energy (MJ) to be cooled by this system
     
   use water_usage_variables, only: watertemp, windspeed, waterdens, latentheat, &
      volheat, evapratio, evapvol, energypervol, volperenergy 
   use process_output, only: ovarre
   implicit none

   !  Arguments
   integer, intent(in) :: outfile, iprint
   integer, intent(in) :: icool
   real(dp), intent(in) :: wastetherm

   !  Local variables

   real(dp) :: heatload, heatloadmet, a, b, c, d, e, f, g, h, i, j
   real(dp) :: windspeedmph, heatloadimp, satvapdelta
   !! coefficients and intermediate calculation variables

   real(dp) :: heatratio
   !! ratio of resultant water temperature increase to input heat loading

   real(dp) :: watertempheated
   !! resultant temperature of the water, following waste heat introduction

   real(dp) :: windfunction
   !! strongly influences evaporation; various, all found through experimentation
  
   real(dp) :: deltaE
   !! difference in evaporative heat loss due to heating of water, J/(m2.day)

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   if ( icool == 1 ) then
      ! small pond as a cooling body
      ! heat loading, MW/acre, based on estimations from US power plants
      heatload = 0.35D0
      ! coefficients as per Brady et al. 1969:
      ! wind function coefficients
      a = 2.47D0
      b = 0D0
      c = 0.12D0
      ! fitted coefficients of heat loading
      d = 3061.331D0
      e = -48.810D0
      f = -78.559D0
      g = -291.820D0
      h = 0.267D0
      i = -0.610D0
      j = 33.497D0

   else if ( icool == 2 ) then
      ! large lake or reservoir as a cooling body
      ! heat loading, MW/acre, based on estimations from US power plants
      heatload = 0.10D0
      ! coefficients as per Webster et al. 1995:
      ! wind function coefficients
      a = 1.04D0
      b = 1.05D0
      c = 0.0D0
      ! fitted coefficients of heat loading
      d = 3876.843D0
      e = -49.071D0
      f = -295.246D0
      g = -327.935D0
      h = 0.260D0
      i = 10.528D0
      j = 40.188D0
      
   else if ( icool == 3 ) then
      ! stream or river as a cooling body
      ! heat loading, MW/acre, based on estimations from US power plants
      heatload = 0.20D0
      ! coefficients as per Gulliver et al. 1986:
      ! wind function coefficients
      a = 2.96D0
      b = 0.64D0
      c = 0.0D0
      ! fitted coefficients of heat loading
      d = 2565.009D0
      e = -43.636D0
      f = -93.834D0
      g = -203.767D0
      h = 0.257D0
      i = 2.408D0
      j = 20.596D0
   end if

   !! Unfortunately, the source spreadsheet was from the US, so the fits for 
   !!   water body heating due to heat loading and the cooling wind functions
   !!   are in non-metric units, hence the conversions required here.
   !! Limitations: maximum wind speed of ~5 m/s; initial watertemp < 25 degC

   ! convert windspeed to mph
   windspeedmph = windspeed * 2.237D0

   ! convert heat loading into cal/(cm2.sec)
   heatloadimp = heatload * 1000000.0D0 * 0.239D0 / 40469000.0D0
   
   ! estimate how heat loading will raise temperature, for this water body 
   heatratio = d + (e * watertemp) + (f * windspeedmph) + (g * heatload) &
      + (h * watertemp**2) + (i * windspeedmph**2) + (j * heatload**2)

   ! estimate resultant heated water temperature
   watertempheated = watertemp + (heatloadimp * heatratio)

   ! find wind function, m/(day.kPa), applicable to this water body:
   windfunction = ( a + (b * windspeed) + (c * windspeed**2) ) / 1000.0D0

   ! difference in saturation vapour pressure (Clausius-Clapeyron approximation)
   satvapdelta = ( &
      (0.611D0 * exp( (17.27D0 * watertempheated)/(237.3D0 + watertempheated) )) &
      - (0.611D0 * exp( (17.27D0 * watertemp)/(237.3D0 + watertemp) )) )

   ! find 'forced evaporation' driven by heat inserted into system
   deltaE = waterdens * latentheat * windfunction * satvapdelta
   
   ! convert heat loading to J/(m2.day)
   heatloadmet = heatload * 1000000D0 / 4046.85642D0 * secday

   !! find evaporation ratio: ratio of the heat used to evaporate water 
   !   to the total heat discharged through the tower
   evapratio = deltaE / heatloadmet
   !! Diehl et al. USGS Report 2013–5188, http://dx.doi.org/10.3133/sir20135188

   volheat = waterdens * latentheat

   energypervol = volheat / evapratio

   volperenergy = 1 / energypervol * 1000000
   
   evapvol = wastetherm * volperenergy

   !  Output section
   if (iprint == 0) return
   if ( icool == 1 ) then
      call ovarre(outfile,'Volume evaporated from cooling pond (m3/day)','(evapvol)',evapvol, 'OP ')
   else if ( icool == 2 ) then
      call ovarre(outfile,'Volume evaporated from cooling lake (m3/day)','(evapvol)',evapvol, 'OP ')
   else if ( icool == 3 ) then
      call ovarre(outfile,'Volume evaporated from cooling river (m3/day)','(evapvol)',evapvol, 'OP ')
   end if

  end subroutine cooling_water_body

end module water_use_module
