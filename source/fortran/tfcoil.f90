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
  use build_variables, only : tfthko, hmax
  use constants
  use error_handling
  use fwbs_variables
  use physics_variables
  use process_output
  use sctfcoil_module
  use tfcoil_variables

  private
  
  !! Radial position of plasma-facing edge of TF coil outboard leg [m]
  !real(dp), private :: r_tf_inboard_in
  !
  !! Radial position of plasma-facing edge of TF coil inboard leg [m]
  !real(dp), private :: r_tf_inboard_out

  public :: tfcoil, cntrpst



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


    use build_module, only : portsz
   
    implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint

    !  Local variables
    integer :: ii
    character(len=1) :: intstring
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

    implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint

    !  Local variables
    real(dp) :: acool,acpav,dcool,dpres,dtcncpav,dtconcpmx, &
         dtfilmav,fc,fricfac,h,lcool,nuselt,pcrt,presin,prndtl, &
         psat,ptot,reyn,ro,roughrat,sum,tclmx,tclmxs,tcoolmx,tmarg,vcoolav, &
         coolant_density, coolant_th_cond, coolant_visco, coolant_cp,&
         conductor_th_cond, dptot, tcool_calc
   
    integer, parameter :: n_tcool_it = 20
    integer :: ii

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
    ptot = prescp + pnuccp*1.0D6

    !  Temperature calculations
    ! -------------------------
    !  Temperature rise in coolant (inlet to outlet)
    ! **********************************************
    ! The coolant flow is now homogeneous as a fixed cooling area is used
    vcoolav = vcool 

    ! Water coolant physical properties
    if ( i_tf_sup ==  0 ) then
       coolant_density = denh2o
       coolant_cp      = cph2o
       coolant_visco   = muh2o
       coolant_th_cond = kh2o
    end if
 
    ! Water coolant
    if ( i_tf_sup ==  0 ) then
       dtiocool = ptot / (coolant_density*vcoolav*acool*coolant_cp)

    ! Helium coolant
    ! **************
    else if ( i_tf_sup ==  2 ) then
       tcool_calc = tcoolin ! K

       ! If T < 4 K -> Extrapolated data
       if ( tcool_calc < 4.0D0 ) write(*,*) 'WARNING : Helium properties extrapolated below K'
 
       ! Infinitesimal power deposition
       dptot = ptot / real( n_tcool_it, kind(1.0D0) )

       do ii = 1, n_tcool_it

          ! Helium density and thermal capacity
          ! ******
          ! Ref : R.D. McCarty, Adv. Cryo. Eng., 1990, 35, 1465-1475.  (homemade fit 14 < T < 50 K fits)
          ! Density
          if ( tcool_calc < 29.5D0 ) then
          coolant_density = 217.753831D0 - 1.66564525D0*tcool_calc -0.160654724D0*tcool_calc**2 + &
                            & 0.00339003258D0*tcool_calc**3
          else if ( tcool_calc < 30.5D0 ) then   ! Linear interpolation between the fits to avoid discontinuity
             coolant_density = 231.40661479377616D0 - 3.917589985552496D0*tcool_calc
          else 
             coolant_density = 212.485251D0 - 4.18059786D0*tcool_calc + 0.0289632937D0*tcool_calc**2
          end if
 
          ! Thermal capacity Cp
          if      ( tcool_calc < 29.5D0 ) then ! Cp fit in the 4 < T < 30 K range
             coolant_cp =  -0.834218557D0 + 0.637079569D0*tcool_calc - 0.0208839696D0*tcool_calc**2 + &
                          & 0.000233433748D0*tcool_calc**3  ! K/(g.K)
          else if ( tcool_calc < 30.5D0 ) then ! Linear interpolation between the fits to avoid discontinuity
             coolant_cp = 4.924018467550791D0 + 0.028953709588498633D0*tcool_calc
          else                                 ! Cp fit in the 30 < T < 60 K range
             coolant_cp = 6.11883125D0 - 0.01022048D0*tcool_calc 
          end if
          coolant_cp = coolant_cp*1.0D3 ! conversion to K/(kg.K)
          ! ******

          ! Temperature infinitesimal increase
          tcool_calc = tcool_calc + dptot / (coolant_density*vcoolav*acool*coolant_cp)
       end do
 
       ! Getting the global in-outlet temperature increase 
       dtiocool = tcool_calc - tcoolin
    end if
    ! **************
    ! **********************************************


    ! Film temperature rise
    ! *********************
    ! Rem : in the case of Helium cooling, tcool_calc, coolant_density and coolant_cp are the outlet Helium ones
    
    ! Helium viscosity
    ! Ref : V.D. Arp,; R.D. McCarty ; Friend, D.G., Technical Note 1334, National Institute of Standards and Technology, Boulder, CO, 1998, 0.  (homemade fit 14 < T < 50 K fit)
    if ( i_tf_sup == 2 ) then
       if      ( tcool_calc < 22.5D0 ) then  ! Fit in the 4 < T < 25 K range
          coolant_visco = exp( -9.19688182D0 - 4.83007225D-1*tcool_calc + 3.47720002D-2*tcool_calc**2 &
                       & - 1.17501538D-3*tcool_calc**3 + 1.54218249D-5*tcool_calc**4 )  ! Pa.s
       else if ( tcool_calc < 27.5D0 ) then  ! Linear interpolation between the fits to avoid discontinuity
          coolant_visco = 6.708587487790973D-6 + 5.776427353055518D-9*tcool_calc
       else                                  ! Fit in the 25 < T < 60 K range
          coolant_visco = 5.41565319D-6 + 5.279222D-8*tcool_calc
       end if 
    end if 

    ! Reynolds number
    reyn = coolant_density * vcool * dcool / coolant_visco
   
    ! Prandlt number
    if ( i_tf_sup == 2 ) then

       ! Helium thermal conductivity [W/(m.K)]
       ! ******
       ! Ref : B.A. Hands B.A., Cryogenics, 1981, 21, 12, 697-703. (homemade fit 14 < T < 50 K fit) 
       if ( tcool_calc < 24.0D0 ) then
          coolant_th_cond = -7.56066334D-3 + 1.62626819D-2*tcool_calc - 1.3633619D-3*tcool_calc**2 + &
                          & 4.84227752D-5*tcool_calc**3 - 6.31264281D-7*tcool_calc**4
       else if ( tcool_calc < 25.0D0 ) then
          coolant_th_cond = 0.05858194642349288D0 - 5.706361831471496D-5*tcool_calc
       else if ( tcool_calc < 50.0D0 ) then
          coolant_th_cond = 0.0731268577D0 - 0.0013826223D0*tcool_calc + 3.55551245D-5*tcool_calc**2&
          & -2.32185411D-7*tcool_calc**3
       else if ( tcool_calc < 51.0D0 ) then
          coolant_th_cond = 4.450475632499988D-2 + 3.871124250000024D-4*tcool_calc
       else 
          coolant_th_cond = 0.04235676D0 + 0.00042923D0*tcool_calc
       end if 
       ! ******
    end if 
    prndtl = coolant_cp * coolant_visco / coolant_th_cond

    ! Temperature difference calculations
    nuselt = 0.023D0 * reyn**0.8D0 * prndtl**0.3D0
    h = nuselt * coolant_th_cond / dcool
    dtfilmav = ptot / (h * 2.0D0*pi*rcool * ncool * lcool)
    ! *********************


    !  Temperature rise in conductor
    ! ------------------------------
    ! Conductor thermal conductivity
    ! ******
    !  Copper conductor
    if ( i_tf_sup ==  0 ) then
       conductor_th_cond = k_copper
    
    ! Cryogenic aluminium 
    else if ( i_tf_sup ==  2 ) then

       ! Ref : R.W. Powel, National Standard Reference Data Series, Nov 25 1966 (S Kahn fit 15 < T < 60 K)
       conductor_th_cond = 16332.2073D0 - 776.91775*tcpav + 13.405688D0*tcpav**2 - 8.01D-02*tcpav**3 ! W/(m.K)
    end if 
    ! ******

    ! Average temperature rise : To be changed with Garry Voss' better documented formula (or add a switch?)
    dtcncpav = (ptot/vol_cond_cp)/(2.0D0*conductor_th_cond*(ro**2 - rcool**2) ) * &
               ( ro**2*rcool**2 - 0.25D0*rcool**4 - 0.75D0*ro**4 + ro**4 * log(ro/rcool) )

    ! Peak temperature rise : To be changed with Garry Voss' better documented formula (or add a switch?)
    dtconcpmx = (ptot/vol_cond_cp)/(2.0D0*conductor_th_cond) * &
         ( (rcool**2 - ro**2)/2.0D0 + ro**2 * log(ro/rcool) )

    !  Average conductor temperature
    tcpav2 = tcoolin + dtcncpav + dtfilmav + 0.5D0*dtiocool

    !  Peak wall temperature
    tcpmax  = tcoolin + dtiocool + dtfilmav + dtconcpmx
    tcoolmx = tcoolin + dtiocool + dtfilmav
    ! -------------------------

    
    !  Thermal hydraulics: friction factor from Z. Olujic, Chemical
    !  Engineering, Dec. 1981, p. 91
    roughrat = 4.6D-5 / dcool
    fricfac  = 1.0D0/ (-2.0D0 * log10(roughrat/3.7D0 - 5.02D0/reyn * &
         log10( roughrat/3.7D0 + 14.5D0/reyn) ) )**2

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


    if (iprint == 0) return

    !  Output section
    call oheadr(outfile,'Centrepost Coolant Parameters')
    call ovarre(outfile,'Centrepost coolant fraction','(fcoolcp)',fcoolcp)
    call ovarre(outfile,'Average coolant channel diameter (m)','(dcool)',dcool)
    call ovarre(outfile,'Coolant channel length (m)','(lcool)',lcool)
    call ovarre(outfile,'Maximum coolant flow speed (m/s)','(vcool)',vcool)
    call ovarre(outfile,'Number of coolant tubes','(ncool)',ncool)
    call ovarre(outfile,'Reynolds number','(reyn)',reyn)
    call ovarre(outfile,'Prandtl number','(prndtl)',prndtl)
    call ovarre(outfile,'Nusselt number','(nuselt)',nuselt)

    call osubhd(outfile,'Resistive Heating :')
    call ovarre(outfile,'Average conductor resistivity (ohm.m)','(rhocp)',rhocp)
    call ovarre(outfile,'Resistive heating (MW)','(prescp/1.0D6)',prescp/1.0D6)
    call ovarre(outfile,'Nuclear heating (MW)','(pnuccp)',pnuccp)
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








end module tfcoil_module
