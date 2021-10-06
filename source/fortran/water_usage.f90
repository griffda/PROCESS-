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
   !! References: 
   !! Diehl et al. Methods for estimating water consumption for thermoelectric
   !! power plants in the United States: U.S. Geological Survey Scientific
   !! Investigations Report 2013–5188, 78 p. http://dx.doi.org/10.3133/sir20135188
   !! Diehl et al. Withdrawal and consumption of water by thermoelectric power 
   !! plants in the United States, 2010: U.S. Geological Survey Scientific 
   !! Investigations Report 2014–5184, 28 p., http://dx.doi.org/10.3133/sir20145184
   !!
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   use, intrinsic :: iso_fortran_env, only: dp=>real64
   implicit none
 
   real(8), parameter :: secday = 86400.0D0
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
     use process_output, only: oheadr, ocmmnt
     implicit none
 
     !  Arguments
 
     integer, intent(in) :: outfile, iprint
 
     !  Local variables
 
     real(8) :: rejected_heat
     !! heat rejected by main power conversion circuit (MW)
     real(8) :: wastethermeng
     !! waste thermal energy to be rejected per [time], MJ
 
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
    !  !! From plant_power:
    !  ! Heat rejected by main power conversion circuit (MW)
    !  rejected_main = pthermmw * (1 - etath)
     rejected_heat = pthermmw * (1 - etath)
 
     wastethermeng = rejected_heat * secday
 
     if (iprint == 1) then
       call oheadr(outfile,'Water usage during plant operation (secondary cooling)')
       call ocmmnt(outfile,'Estimated amount of water used through different cooling system options:')
       call ocmmnt(outfile,'1. Cooling towers')
       call ocmmnt(outfile,'2. Water bodies (pond, lake, river): recirculating or once-through')
     endif
 
     !! call subroutines for cooling mechanisms:
 
     ! cooling towers 
     call cooling_towers(outfile,iprint,wastethermeng)
 
     ! water-body cooling
     call cooling_water_body(outfile,iprint,wastethermeng)
 
   end subroutine waterusecall
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   subroutine cooling_towers(outfile,iprint,wastetherm)
    
     !! Water used in cooling towers
     !! author: R Chapman, UKAEA
     !! outfile : input integer : Fortran output unit identifier
     !! iprint : input integer : Switch to write output (1=yes)
     !! wastetherm : input real : thermal energy (MJ) to be cooled by this system
    
     use water_usage_variables, only: airtemp, waterdens, latentheat, &
       volheat, evapratio, evapvol, energypervol, volperenergy, &
       waterusetower
     use process_output, only: ovarre
     implicit none
 
     !  Arguments
     integer, intent(in) :: outfile, iprint
     real(8), intent(in) :: wastetherm
 
     !  Local variables
 
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
     !! find evaporation ratio: ratio of the heat used to evaporate water 
     !   to the total heat discharged through the tower
     evapratio = 1.0D0 - ( (-0.000279D0*airtemp**3 &
        + 0.00109D0*airtemp**2 &
        - 0.345D0*airtemp &
        + 26.7D0) /100.0D0)
     !! Diehl et al. USGS Report 2013–5188, http://dx.doi.org/10.3133/sir20135188
 
     volheat = waterdens * latentheat
 
     energypervol = volheat / evapratio
 
     volperenergy = 1.0D0 / energypervol * 1000000.0D0
     
     evapvol = wastetherm * volperenergy
 
     !! find water withdrawn from external source
     waterusetower = 1.4D0 * evapvol
     ! Estimated as a ratio to evaporated water (averaged across obervered dataset)
     !  as per Diehl et al. USGS Report 2014–5184, http://dx.doi.org/10.3133/sir20145184
 
     !  Output section
     if (iprint == 0) return
     call ovarre(outfile,'Volume used in cooling tower (m3/day)','(waterusetower)',waterusetower, 'OP ')
  
   end subroutine cooling_towers
 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   subroutine cooling_water_body(outfile,iprint,wastetherm)
    
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
       volheat, evapratio, evapvol, energypervol, volperenergy, &
       wateruserecirc, wateruseonethru
    use process_output, only: ovarre
    implicit none
 
    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(8), intent(in) :: wastetherm
 
    !  Local variables
 
    integer :: icool
    !! switch between different water-body cooling options
 
    real(8) :: heatload, heatloadmet, a, b, c, d, e, f, g, h, i, j
    real(8) :: windspeedmph, heatloadimp, satvapdelta, evapsum, evapmean
    !! coefficients and intermediate calculation variables
 
    real(8) :: heatratio
    !! ratio of resultant water temperature increase to input heat loading
 
    real(8) :: watertempheated
    !! resultant temperature of the water, following waste heat introduction
 
    real(8) :: windfunction
    !! strongly influences evaporation; various, all found through experimentation
   
    real(8) :: deltaE
    !! difference in evaporative heat loss due to heating of water (J/(m2.day))
 
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
    evapsum = 0.0D0
 
    do icool = 1, 3
 
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
       heatloadmet = heatload * 1000000.0D0 / 4046.85642D0 * secday
 
       !! find evaporation ratio: ratio of the heat used to evaporate water 
       !   to the total heat discharged through the tower
       evapratio = deltaE / heatloadmet
       !! Diehl et al. USGS Report 2013–5188, http://dx.doi.org/10.3133/sir20135188
 
       volheat = waterdens * latentheat
 
       energypervol = volheat / evapratio
 
       volperenergy = 1.0D0 / energypervol * 1000000.0D0
       
       evapvol = wastetherm * volperenergy
 
 
       !! using this method the estimates for pond, lake and river evaporation produce similar results,
       !   the average will be taken and used in the next stage of calculation
       evapsum = evapsum + evapvol
 
    end do 
 
    evapmean = evapsum / 3.0D0
 
    !! water volume withdrawn from external source depends on recirculation or 'once-through' system choice
    !   Estimated as a ratio to evaporated water (averaged across obervered dataset)
    !   as per Diehl et al. USGS Report 2014–5184, http://dx.doi.org/10.3133/sir20145184
 
    ! recirculating water system:
    wateruserecirc = 1.0D0 * evapvol
 
    ! once-through water system:
    wateruseonethru = 98.0D0 * evapvol
 
    !  Output section
    if (iprint == 0) return
    call ovarre(outfile,'Volume used in recirculating water system (m3/day)','(wateruserecirc)',wateruserecirc, 'OP ')
    call ovarre(outfile,'Volume used in once-through water system (m3/day)','(wateruseonethru)',wateruseonethru, 'OP ')
 
   end subroutine cooling_water_body
 
 end module water_use_module