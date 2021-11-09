! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module tfcoil_module

  !! Module containing resistive TF coil routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of a resistive TF coil system for a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none


  contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfcoil(outfile,iprint)

    !! TF coil module
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine calculates various parameters for the TF coil set.
    !! If the TF coils are superconducting the calculations are performed
    !! in routine <A HREF="sctfcoil.html">sctfcoil</A> instead.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code

   use build_module, only: portsz
   use process_output, only: int2char, oheadr, ovarre, osubhd, oblnkl, &
      ocmmnt
   use sctfcoil_module, only: sctfcoil
   use tfcoil_variables, only: bmaxtf, cforce, estotftgj, i_tf_sup, n_tf, &
      ripmax, ripple, ritfc, vforce, xarc, yarc
   use constants, only: mfile
    implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    
    ! TF coil calculations
    call sctfcoil(outfile,iprint)

    !  Port size calculation
    call portsz


  end subroutine tfcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cntrpst(outfile,iprint)

    !! Evaluates the properties of a TART centrepost
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine evaluates the parameters of the centrepost for a
    !! tight aspect ratio tokamak. The centrepost is assumed to be tapered,
    !! i.e. narrowest on the midplane (z=0).
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use build_variables, only: hmax, tfthko
   use fwbs_variables, only: pnuc_cp_tf
   use process_output, only: oheadr, ovarre, osubhd
   use tfcoil_variables, only: dtiocool, etapump, fcoolcp, &
      i_tf_sup, ncool, ppump, prescp, rbmax, rcool, &
      rhocp, tcoolin, tcpav, tcpav2, tcpmax, a_cp_cool, n_tf, vcool, vol_cond_cp
   use constants, only: pi, cph2o, denh2o, k_copper, kh2o, muh2o
   use error_handling, only: fdiags, report_error 
   implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint

    !  Local variables
    real(8) :: acool
    !! Total CP cooling area [m2]

    real(8) :: acpav
    !! Average conductor layer area (including cooling area) [m2]

    real(8) :: dcool
    !! Cooling channel diamater [m]

    real(8) :: lcool
    !! Cooling channel (vertical) length [m]

    real(8) :: ro
    !! Average radius covered by a cooling channel [m]

    real(8) :: dpres
    !! Pressure drop 

    real(8) :: dtcncpav
    !! Average conductor temperature rise [K]
    
    real(8) :: dtconcpmx
    !! Peak conductop temperature rise [K]
    
    real(8) :: dtfilmav
    !! Film temperature rise [K]
    
    real(8) :: fc
    !! Parameter use in saturation pressure calculation
    
    real(8) :: fricfac
    !! Friction factor
    
    real(8) :: h
    !! h factor used in film temperature rise calculation
    
    real(8) :: nuselt
    !! Nusselt number used in film temperature rise calculation
    
    real(8) :: prndtl
    !! Prandlt number used in film temperature rise calculation
    
    real(8) :: reyn
    !! Reynolds number used in film temperature rise calculation
    
    real(8) :: pcrt
    !! Critical pressure in saturation pressure calculations [Pa]
    !! Rem : Currently only input for water 
    
    real(8) :: presin
    !! Coolant pressure drop [Pa]
    
    real(8) :: psat
    !! Saturation pressure [Pa]
    !! Rem : Calcultation to be revised for Helium cooling 
    
    real(8) :: ptot
    !! Total heating to be dissipated ( resistive + nuclear ) [W]
    
    real(8) :: dptot
    !! Ptot (total power to dissipte) increment used in the coolant temperature rise [W]
    
    real(8) :: roughrat
    !! Roughting factor
    
    real(8) :: sum
    !! A sum
    
    real(8) :: tclmx
    !! Temperature used in staturation pressure calculation [K]
    
    real(8) :: tclmxs
    !! Temperature used in staturation pressure calculation [K]
    
    real(8) :: tcoolmx
    !! Maximum coolant temperature [K]
    
    real(8) :: tmarg
    !! Temperature margins used in saturation pressure calculation [K]

    real(8) :: cool_mass_flow
    !! Coolant mass flow rate [kg/s]

    real(8) :: vcool_max
    !! Maximum coolant velocity [m/s]

    real(8) :: coolant_density
    !! Coolant density [kg/m3]
    
    real(8) :: coolant_th_cond
    !! Coolant thermal conductivity [W/(m.K)]
    
    real(8) :: coolant_visco
    !! Coolant viscosity [SA]
    
    real(8) :: coolant_cp
    !! Coolant thermal capacity
    
    real(8) :: conductor_th_cond
    !! Conductor thermal conductivity [W/(m.K)]
    
    real(8) :: tcool_calc
    !! coolant temperature used in the temperature rise calculations (not an output) [K]

    real(8) :: tcool_av
    !! Average bulk coolant temperature (not an output) [K]

    real(8) :: tcool_film
    !! Coolant temperature at the pipe surface calculated from the average bulk
    !! temperature (not an output) [K]

    integer, parameter :: n_tcool_it = 20
    !! Number of integral step used for the coolant temperature rise

    integer :: ii
    !! Loop increment
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    
    !  Temperature margin used in calculations (K)
    tmarg = 10.0D0
    
    ! Coolant channels:
    acool = a_cp_cool * n_tf        !  Cooling cross-sectional area
    dcool = 2.0D0 * rcool           !  Diameter
    lcool = 2.0D0 * (hmax + tfthko) !  Length
    ncool = acool/( pi * rcool**2)  !  Number

    ! Average conductor cross-sectional area to cool (with cooling area)
    acpav = 0.5D0 * vol_cond_cp/(hmax + tfthko) + acool
    ro = sqrt( acpav/(pi*ncool) )

    !  Inner legs total heating power (to be removed by coolant)
    ptot = prescp + pnuc_cp_tf * 1.0D6

    !  Temperature calculations
    ! -------------------------
    !  Temperature rise in coolant (inlet to outlet)
    ! **********************************************
    ! Water coollant
    ! --------------
    if ( i_tf_sup ==  0 ) then
         
       ! Water coolant physical properties
       coolant_density = denh2o
       coolant_cp      = cph2o
       coolant_visco   = muh2o
       coolant_th_cond = kh2o

       ! Mass flow rate [kg/s]
       cool_mass_flow = acool * coolant_density * vcool

       ! Water temperature rise
       dtiocool = ptot / (cool_mass_flow*coolant_cp)

       ! Constant coolant velocity
       vcool_max = vcool
    ! --------------

       
    ! Helium coolant
    ! --------------
    else if ( i_tf_sup ==  2 ) then

       ! Inlet coolant density [kg/m3]
       call he_density( tcoolin, coolant_density )

       ! Mass flow rate [kg/s]
       cool_mass_flow = acool * coolant_density * vcool
 
       ! Infinitesimal power deposition used in the integral
       dptot = ptot / real( n_tcool_it, kind(1.0D0) )

       tcool_calc = tcoolin ! K
       do ii = 1, n_tcool_it

          ! Thermal capacity Cp
          call he_cp(tcool_calc, coolant_cp)

          ! Temperature infinitesimal increase
          tcool_calc = tcool_calc + dptot / ( cool_mass_flow * coolant_cp )
       end do

       ! Outlet coolant density (minimal coolant density value)
       call he_density(tcool_calc, coolant_density)
       
       ! Maxium coolant velocity
       vcool_max = cool_mass_flow / ( acool * coolant_density )

       ! Getting the global in-outlet temperature increase 
       dtiocool = tcool_calc - tcoolin
    end if
    ! --------------

    ! Average coolant temperature
    tcool_av = tcoolin + 0.5D0 * dtiocool
    ! **********************************************


    ! Film temperature rise
    ! *********************
    ! Rem : The helium cooling properties are calculated using the outlet ones
    ! this is not an exact approximation for average temperature rise

    ! Helium viscosity
    if ( i_tf_sup == 2 ) call he_visco(tcool_av, coolant_visco)

    ! Reynolds number
    reyn = coolant_density * vcool * dcool / coolant_visco
   
    ! Helium thermal conductivity [W/(m.K)]
    if ( i_tf_sup == 2 ) call he_th_cond( tcool_av, coolant_th_cond )
  
    ! Prandlt number  
    prndtl = coolant_cp * coolant_visco / coolant_th_cond

    ! Film temperature difference calculations    
    ! Originally prandtl was prndtl**0.3D0 but this is incorrect as from
    ! Dittus-Boelter correlation where the fluid is being heated it should be as below    
    nuselt = 0.023D0 * reyn**0.8D0 * prndtl**0.4D0
    h = nuselt * coolant_th_cond / dcool
    dtfilmav = ptot / (h * 2.0D0*pi*rcool * ncool * lcool)

    ! Average film temperature (in contact with te conductor)
    tcool_film = tcool_av + dtfilmav
    ! *********************


    !  Temperature rise in conductor
    ! ------------------------------
    ! Conductor thermal conductivity
    ! ******
    !  Copper conductor
    if ( i_tf_sup ==  0 ) then
       conductor_th_cond = k_copper
    
    ! Aluminium 
    else if ( i_tf_sup == 2 ) then
      call al_th_cond( tcool_film, conductor_th_cond )
    end if
    ! ******

    ! Average temperature rise : To be changed with Garry Voss' better documented formula ? 
    dtcncpav = (ptot/vol_cond_cp)/(2.0D0*conductor_th_cond*(ro**2 - rcool**2) ) * &
               ( ro**2*rcool**2 - 0.25D0*rcool**4 - 0.75D0*ro**4 + ro**4 * log(ro/rcool) )

    ! Peak temperature rise : To be changed with Garry Voss' better documented formula ?
    dtconcpmx = (ptot/vol_cond_cp)/(2.0D0*conductor_th_cond) * &
                ( (rcool**2 - ro**2)/2.0D0 + ro**2 * log(ro/rcool) )


    ! If the average conductor temperature difference is negative, set it to 0 
    if ( dtcncpav < 0.0D0 ) then 
      call report_error(249)
      dtcncpav = 0.0D0
    end if

    ! If the average conductor temperature difference is negative, set it to 0  
    if ( dtconcpmx < 0.0D0 ) then 
      call report_error(250)
      dtconcpmx = 0.0D0
    end if
    
    !  Average conductor temperature
    tcpav2 = tcoolin + dtcncpav + dtfilmav + 0.5D0*dtiocool

    !  Peak wall temperature
    tcpmax  = tcoolin + dtiocool + dtfilmav + dtconcpmx
    tcoolmx = tcoolin + dtiocool + dtfilmav
    ! -------------------------

    
    !  Thermal hydraulics: friction factor from Z. Olujic, Chemical
    !  Engineering, Dec. 1981, p. 91
    roughrat = 4.6D-5 / dcool
    fricfac  = 1.0D0/ (-2.0D0 * log10(roughrat/3.7D0 - 5.02D0/reyn  &
             * log10( roughrat/3.7D0 + 14.5D0/reyn) ) )**2

    ! Pumping efficiency
    if      ( i_tf_sup == 0 ) then ! Water cooled
      etapump = 0.8D0
    else if ( i_tf_sup == 2 ) then ! Cryogenic helium
      etapump = 0.6D0
    end if

    ! Pressure drop calculation
    dpres = fricfac * (lcool/dcool) * coolant_density * 0.5D0*vcool**2
    ppump = dpres * acool * vcool / etapump

    !  Critical pressure in saturation pressure calculations (Pa)
    pcrt = 2.24D7

    ! Saturation pressure
    ! Ref : Keenan, Keyes, Hill, Moore, steam tables, Wiley & Sons, 1969
    ! Rem 1 : ONLY VALID FOR WATER !
    ! Rem 2 : Not used anywhere else in the code ...
    tclmx = tcoolmx + tmarg
    tclmxs = min(tclmx, 374.0D0)
    fc = 0.65D0 - 0.01D0 * tclmxs
    sum = -741.9242D0 - 29.721D0*fc - 11.55286D0*fc**2 &
         - 0.8685635D0*fc**3 + 0.1094098D0*fc**4 &
         + 0.439993D0*fc**5 + 0.2520658D0*fc**6 &
         + 0.0518684D0*fc**7
    psat = pcrt * exp(0.01D0/(tclmxs + 273.0D0) * (374.0D0 - tclmxs) * sum )
    presin = psat + dpres



    !  Output section    
    if (iprint == 0) return
    call oheadr(outfile,'Centrepost Coolant Parameters')
    call ovarre(outfile,'Centrepost coolant fraction','(fcoolcp)',fcoolcp)
    call ovarre(outfile,'Average coolant channel diameter (m)','(dcool)',dcool)
    call ovarre(outfile,'Coolant channel length (m)','(lcool)',lcool)
    call ovarre(outfile,'Inlet coolant flow speed (m/s)','(vcool)',vcool)
    call ovarre(outfile,'Outlet coolant flow speed (m/s)','(vcool_max)',vcool_max)
    call ovarre(outfile,'Coolant mass flow rate (kg/s)','(cool_mass_flow)',cool_mass_flow)
    call ovarre(outfile,'Number of coolant tubes','(ncool)',ncool)
    call ovarre(outfile,'Reynolds number','(reyn)',reyn)
    call ovarre(outfile,'Prandtl number','(prndtl)',prndtl)
    call ovarre(outfile,'Nusselt number','(nuselt)',nuselt)

    call osubhd(outfile,'Resistive Heating :')
    call ovarre(outfile,'Average conductor resistivity (ohm.m)','(rhocp)',rhocp)
    call ovarre(outfile,'Resistive heating (MW)','(prescp/1.0D6)',prescp/1.0D6)
    call ovarre(outfile,'Nuclear heating (MW)','(pnuc_cp_tf)',pnuc_cp_tf)
    call ovarre(outfile,'Total heating (MW)','(ptot/1.0D6)',ptot/1.0D6)

    call osubhd(outfile,'Temperatures :')
    call ovarre(outfile,'Input coolant temperature (K)','(tcoolin)',tcoolin)
    call ovarre(outfile,'Input-output coolant temperature rise (K)','(dtiocool)',dtiocool)
    call ovarre(outfile,'Film temperature rise (K)','(dtfilmav)',dtfilmav)
    call ovarre(outfile,'Average temp gradient in conductor (K/m)','(dtcncpav)',dtcncpav)
    call ovarre(outfile,'Average centrepost temperature (K)','(tcpav2)',tcpav2)
    call ovarre(outfile,'Peak centrepost temperature (K)','(tcpmax)',tcpmax)

    call osubhd(outfile,'Pump Power :')
    call ovarre(outfile,'Coolant pressure drop (Pa)','(dpres)',dpres)
    if ( i_tf_sup == 0 ) then ! Saturation pressure calculated with Water data ...
       call ovarre(outfile,'Coolant inlet pressure (Pa)','(presin)',presin)
    end if
    call ovarre(outfile,'Pump power (W)','(ppump)',ppump)


    end subroutine cntrpst
     
    subroutine he_density( temp, density )
      !! Author : S. Kahn
      !! Subroutine calculating temperature dependent helium density at 100 bar
      !! from fit using the following data, valid in [4-50] K
      !! Ref : R.D. McCarty, Adv. Cryo. Eng., 1990, 35, 1465-1475.

      use error_handling, only: fdiags, report_error

      implicit none

      ! Input / output
      ! --------------
      real(8), intent(in) :: temp
      !! Helium temperature [K]

      real(8), intent(out) :: density
      !! Heliyn density [kg/m3]
      ! --------------


      ! Fit range validation
      if ( temp < 4.0D0 .or. temp > 50.0D0 ) then 
         fdiags(1) = temp
         call report_error(257)
      end if 

      ! Oder 3 polynomial fit
      if ( temp < 29.5D0 ) then
         density = 217.753831D0 - 1.66564525D0*temp -0.160654724D0*temp**2  &
                 + 0.00339003258D0*temp**3

      ! Linear interpolation between the fits to avoid discontinuity
      else if ( temp < 30.5D0 ) then   
            density = 231.40661479377616D0 - 3.917589985552496D0*temp

      ! Oder 2 polynomial fit
      else 
            density = 212.485251D0 - 4.18059786D0*temp + 0.0289632937D0*temp**2
      end if

    end subroutine he_density

    ! ----------------------------------------

    subroutine he_cp(temp, cp)
      !! Author : S. Kahn
      !! Subroutine calculating temperature dependent thermal capacity at 
      !! constant pressures at 100 Bar from fit using the following data
      !! valid in [4-50] K
      !! Ref : R.D. McCarty, Adv. Cryo. Eng., 1990, 35, 1465-1475.

      use error_handling, only: fdiags, report_error

      implicit none

      ! Input / output
      ! --------------
      real(8), intent(in) :: temp
      !! Helium temperature [K]

      real(8), intent(out) :: cp
      !! Themal capacity at constant pressure [K/(kg.K)]
      ! --------------


      ! Fit range validation
      if ( temp < 4.0D0 .or. temp > 50.0D0 ) then 
         fdiags(1) = temp
         call report_error(257)
      end if 

      ! Order 3 polynomial fit in [4-30] K on the dimenion [K/(g.K)]
      if ( temp < 29.5D0 ) then
         cp = - 0.834218557D0 + 0.637079569D0*temp - 0.0208839696D0*temp**2  &
            + 0.000233433748D0*temp**3
                      
      ! Linear interpolation between the fits to avoid discontinuity
      else if ( temp < 30.5D0 ) then ! Linear interpolation between the fits to avoid discontinuity
         cp = 4.924018467550791D0 + 0.028953709588498633D0*temp

      ! Linear fit in [30-60] K on the dimenion [K/(g.K)]
      else
         cp = 6.11883125D0 - 0.01022048D0*temp
      end if

      ! conversion to [K/(kg.K)]
      cp = cp*1.0D3 

    end subroutine he_cp

    ! ----------------------------------------
        
    subroutine he_visco(temp, visco)
      !! Author : S. Kahn
      !! Subroutine calculating temperature dependent He viscosity at 100 Bar
      !! from fit using the following data, valid in [4-50] K
      !! Ref : V.D. Arp,; R.D. McCarty ; Friend, D.G., Technical Note 1334, National 
      !! Institute of Standards and Technology, Boulder, CO, 1998, 0.  

      use error_handling, only: fdiags, report_error

      implicit none

      ! Input / output
      ! --------------
      real(8), intent(in) :: temp
      !! Helium temperature [K]

      real(8), intent(out) :: visco
      !! Themal capacity at constant pressure [Pa.s]
      ! --------------


      ! Fit range validation
      if ( temp < 4.0D0 .or. temp > 50.0D0 ) then 
         fdiags(1) = temp
         call report_error(257)
      end if 

      ! Order 4 polynomial exponential fit in [4-25] K
      if ( temp < 22.5D0 ) then
         visco = exp( -9.19688182D0 - 4.83007225D-1*temp + 3.47720002D-2*temp**2 &
               - 1.17501538D-3*temp**3 + 1.54218249D-5*temp**4 ) 

      ! Linear interpolation between the fits to avoid discontinuity
      else if ( temp < 27.5D0 ) then
         visco = 6.708587487790973D-6 + 5.776427353055518D-9*temp
      
      ! Linear fit in [25-60] K
      else 
         visco = 5.41565319D-6 + 5.279222D-8*temp
      end if 

    end subroutine he_visco

    ! ----------------------------------------

    subroutine he_th_cond(temp, th_cond)
      !! Author : S. Kahn
      !! Subroutine calculating temperature dependent He thermal conductivity 
      !! at 100 Bar from fit using the following data, valid in [4-50] K
      !! Ref : B.A. Hands B.A., Cryogenics, 1981, 21, 12, 697-703. 

      use error_handling, only: fdiags, report_error

      implicit none

      ! Input / output
      ! --------------
      real(8), intent(in) :: temp
      !! Helium temperature [K]

      real(8), intent(out) :: th_cond
      !! Themal conductivity [W/(m.K)]
      ! --------------


      ! Fit range validation
      if ( temp < 4.0D0 .or. temp > 50.0D0 ) then 
         fdiags(1) = temp
         call report_error(257)
      end if 

      ! Order 4 polynomial fit 
      if ( temp < 24.0D0 ) then
         th_cond = -7.56066334D-3 + 1.62626819D-2*temp - 1.3633619D-3*temp**2 &
                 +  4.84227752D-5*temp**3 - 6.31264281D-7*temp**4

      ! Linear interpolation between the fits to avoid discontinuity
      else if ( temp < 25.0D0 ) then
         th_cond = 0.05858194642349288D0 - 5.706361831471496D-5*temp
      
      ! Order 2 polynomial fit
      else if ( temp < 50.0D0 ) then
         th_cond = 0.0731268577D0 - 0.0013826223D0*temp + 3.55551245D-5*temp**2&
         & -2.32185411D-7*temp**3
         
      ! Linear interpolation between the fits to avoid discontinuity
      else if ( temp < 51.0D0 ) then
         th_cond = 4.450475632499988D-2 + 3.871124250000024D-4*temp

      ! Linear fit
      else 
         th_cond = 0.04235676D0 + 0.00042923D0*temp
      end if 

    end subroutine he_th_cond

    ! ----------------------------------------

    subroutine al_th_cond(temp, th_cond)
       !! Author : S. Kahn
       !! Subroutine calculating temperature dependent Al thermal conductivity 
       
       use error_handling, only: fdiags, report_error
 
       implicit none
 
       ! Input / output
       ! --------------
       real(8), intent(in) :: temp
       !! Helium temperature [K]
 
       real(8), intent(out) :: th_cond
       !! Themal conductivity [W/(m.K)]
       ! --------------



       ! Fiting range verification
       if ( temp < 15.0D0 .or. temp > 150.0D0 ) then 
         fdiags(1) = temp
         call report_error(258)
       end if

       ! fit 15 < T < 60 K (order 3 poly)
       if ( temp < 60.0D0 ) then
          th_cond = 16332.2073D0 - 776.91775D0*temp + 13.405688D0*temp**2 - 8.01D-02*temp**3

       ! Linear interpolation between the fits to avoid discontinuity
       else if ( temp < 70.0D0 ) then 
         th_cond = 1587.9108966527328D0 - 15.19819661087886D0 * temp 

       ! fit 70 < T < 150 K (order 2 poly)
       else if ( temp < 150.0D0 ) then 
         th_cond = 1782.77406D0 - 24.7778504D0 * temp + 9.70842050D-2 * temp**2  

       ! constant value after that set with the fit upper limit to avoid discontinuities
       else
         th_cond = 250.4911087866094D0 
       end if 

    end subroutine al_th_cond

end module tfcoil_module
