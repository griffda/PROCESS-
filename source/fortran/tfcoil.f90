! THIS MODULE IS MEANT TO BE SOON REPLACED BY A REFACTORED VERSION 
! WITH CONSISTENT GEOMETRIES AND PHYSICS
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

  use build_module
  use build_variables, only : r_tf_inboard_mid, hmax, r_tf_outboard_mid, rtop, tfcth, tfthko, bore, hpfu
  use constants
  use error_handling
  use fwbs_variables
  use physics_variables
  use process_output
  use sctfcoil_module
  use tfcoil_variables

  private
  
  ! Radial position of plasma-facing edge of TF coil outboard leg [m]
  real(kind(1.0D0)), private :: r_tf_inleg_in

  ! Radial position of plasma-facing edge of TF coil inboard leg [m]
  real(kind(1.0D0)), private :: r_tf_inleg_out

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
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint

    !  Local variables
    integer :: ii
    character(len=1) :: intstring
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! Magnet desing dependent calculations
    ! ------------------------------------
    !  Resistive TF coils
    if ( i_tf_sup /= 1 ) then  
       call concoptf(outfile,iprint)

    !  Superconducting TF coils
    else  
       call sctfcoil(outfile,iprint)
    end if
    ! ------------------------------------


    !  Port size calculation
    call portsz

    ! xarc and yarc shape calculation
    call cutfshape


    ! Printing the final results in the output file
    ! ---------------------------------------------
    if (iprint == 0) return

    !  Output section (resistive TF coils only)
    if (i_tf_sup /= 1) then

       call oheadr(outfile,'TF Coils')
       call ovarre(outfile,'TF coil current (summed over all coils) (A)','(ritfc)',ritfc)
       call ovarre(outfile,'Peak field at the TF coils (T)','(bmaxtf)',bmaxtf)
       call ovarre(outfile,'Ripple at plasma edge (%)','(ripple)',ripple)
       call ovarre(outfile,'Max allowed ripple amplitude at plasma (%)','(ripmax)',ripmax)
       call ovarre(outfile,'Number of TF coil legs','(n_tf)',n_tf)

       call osubhd(outfile,'Energy and Forces :')
       call ovarre(outfile,'Total stored energy in TF coils (GJ)','(estotftgj)',estotftgj)
       call ovarre(outfile,'Vertical force on inboard leg (N)','(vforce)',vforce)
       call ovarre(outfile,'Centering force on inboard leg (N/m)','(cforce)',cforce)
       call ovarre(outfile,'Radial stress (MPa)','(sigrad)',sigrad)
       call ovarre(outfile,'Transverse stress (MPa)','(sigtan)',sigtan)
       call ovarre(outfile,'Vertical stress (MPa)','(sigver)',sigver)
       call oblnkl(outfile)
       call ocmmnt(outfile,'TF coil inner surface shape is given by a rectangle with the')
       call ocmmnt(outfile,'following inner points (Note that this does not account')
       call ocmmnt(outfile,'for the ST tapered centrepost):')
       call oblnkl(outfile)

       write(outfile,10)
       10  format(t2,'point',t16,'x(m)',t31,'y(m)')
       do ii = 1,5
          write(outfile,20) ii,xarc(ii),yarc(ii)
          intstring = int2char(ii)
          call ovarre(mfile,'TF coil arc point '//intstring//' R (m)', '(xarc('//intstring//'))',xarc(ii))
          call ovarre(mfile,'TF coil arc point '//intstring//' Z (m)', '(yarc('//intstring//'))',yarc(ii))
       end do
       20  format(i4,t10,f10.3,t25,f10.3)

    end if
    ! ---------------------------------------------
  end subroutine tfcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine concoptf(outfile,iprint)

    !! Calculates additional parameters for resistive TF coils
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine calculates various additional parameters for a
    !! resistive TF coil set, including for TART machines.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    integer, intent(in) :: iprint,outfile

    !  Local variables
    real(kind(1.0D0)) :: r_tf_outleg_in
    real(kind(1.0D0)) :: ltfleg, rmid, ztop
    real(kind(1.0D0)) :: tfcind1
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    



    ! Inner leg(s) quantities
    ! -----------------------
    ! Radial build
    ! ******
    ! Radial position of inner/outer edge of inboard TF coil leg [m]
    r_tf_inleg_in  = r_tf_inboard_mid - 0.5D0 * tfcth
    r_tf_inleg_out = r_tf_inboard_mid + 0.5D0 * tfcth
    
    ! Position of the maximum magnetic field 
    ! No winding pack structure -> simply the innner legs plasma side radius
    rbmax = r_tf_inleg_out
    
    ! Radial position of centre of inboard TF coil leg [m]
    r_tf_inboard_mid = r_tf_inleg_in + 0.5D0*tfcth
    ! ******


    ! Toroidal thickness of TF coil outer leg [m]
    if ( itart == 1 ) then
       tftort = 2.0D0 * rtop * sin(pi/n_tf)
    else 
       tftort = 2.0D0 * r_tf_inleg_out*sin(pi/n_tf)
    end if

    ! Inboard total cross-sectional area (m2)
    tfareain = pi * (r_tf_inleg_out**2 - r_tf_inleg_in**2)


    ! Inner legs physics
    ! ******
    ! Calculation of the maximum B field on the magnet [T]
    bmaxtf = bt * rmajor / rbmax
    
    ! Calculation of the total current necessary to reach Bmax
    ! rem SK : ritcf is no longer an input
    ritfc = bmaxtf * rbmax * 5.0D6 

    !  Centering force
    if ( bore == 0.0D0 ) then
       cforce = 0.0D0
    else
       cforce = 0.5D0 * bmaxtf * ritfc / n_tf  !  N/m
    end if
    ! ******


    ! Power losses 
    ! ******
    ! Copper resistivity (0.92 factor for glidcop C15175)
    if ( i_tf_sup == 0 ) then
    rhocp = 1.0D-8 * ( 1.72D0 + 0.0039D0*(tcpav - 273.15D0) ) / 0.92D0

    ! Aluminium cryogenic resistivity
   else if ( i_tf_sup == 2 ) then
      rhocp = 2.00016D-14*tcpav**3 - 6.75384D-13*tcpav**2 + 8.89159D-12*tcpav
   end if

    ! Conventionnal tokamak (geometry invariant with hight)
    if (itart == 0) then 
       volcp = 2.0D0 * tfareain * ( hmax + tfthko ) !  volume

       !  Inner legs resistive power losses
       prescp = rhocp * ritfc**2 * 2.0D0 * ( hmax + tfthko ) / ( tfareain * (1.0D0-fcoolcp) )
      
    !  Spherical Tokamak (larger diameter at top/bottom)
    else  
       !  Radii and vertical height from midplane
       rmid = r_tf_inleg_out
       ztop = (rminor * kappa) + dztop

       !  Resistivity enhancement factor
       rhocp = rhocp * frhocp

       !  Volume and resistive power losses of TART centrepost
       call cpost(rtop,ztop,rmid,hmax+tfthko,ritfc,rhocp,fcoolcp,r_tf_inleg_in,volcp,prescp)
    end if

    ! ******
    ! -----------------------


    ! Outboard leg information (per leg)
    ! ----------------------------------    
    ! Radius of inner edge of outboard TF coil leg (m)
    r_tf_outleg_in = r_tf_outboard_mid - 0.5D0*tfthko
    
    ! Outer leg cross-sectional area 
    arealeg = tfthko*tftort

    ! Length of leg centre-line (N.B. this assumes rectangular shaped
    ! coils, not D-shaped)
    ! A more comprehensive calculation will be present in next of the module
    if ( itart == 1 ) then
       ltfleg = hmax + hpfu + 2.0D0 * ( r_tf_outboard_mid - rtop )
    else 
       ltfleg = hmax + hpfu + 2.0D0 * ( r_tf_outboard_mid - r_tf_inleg_out )
    end if

    ! Volume of the TF legs
    voltfleg = ltfleg * arealeg

    ! Outboard leg current density
    cdtfleg = ritfc/(n_tf * arealeg)

    ! Resistive power
    ! Rem : Assuming the same CP-leg resistivity (as rhotfleg = -1 by default)
    if ( abs( rhotfleg + 1 ) < epsilon(rhotfleg) ) rhotfleg = rhocp
    tflegres = ltfleg * rhotfleg/arealeg
    presleg = (ritfc/n_tf)**2 * tflegres * n_tf 
    ! ----------------------------------


    ! Inner outter common quantities
    ! -----------------------------
    ! Vertircal force    
    ! The outer radius of the inner leg and the inner radius of the outer leg is taken
    ! vforce = 0.55D0 * bt * rmajor * 0.5D0*ritfc * log(r_tf_outleg_in/r_tf_inleg_out) / n_tf 
    vforce = 0.25D0 * bmaxtf * ritfc / n_tf * ( 0.5D0 * tfcth +                                       &     ! Inner leg side component 
                                              & r_tf_inleg_out * log(r_tf_outleg_in/r_tf_inleg_out) + &     ! TF bore component
                                              & 0.5D0 * tfthko * (r_tf_inleg_out/r_tf_outleg_in) )   ! Outer leg side component

    ! Current turn information 
    if (itart == 0) then ! CT case
       !  Number of turns per leg
       turnstf = ritfc / (n_tf * cpttf)
    else                 ! ST case
       !  Current per turn (N.B. cannot set CPTTF as an iteration variable for ST)
       turnstf = 1.0D0
       cpttf = ritfc/(turnstf*n_tf)
    end if

    ! Weight of conductor (outer legs, inner legs, total)
    ! ******
    ! Coolant density assumed negligible (gaz) 
    ! Copper
    if ( i_tf_sup == 0 ) then
       whttflgs = voltfleg * n_tf * (1.0D0 - vftf) * dcopper ! outer legs
       whtcp = volcp * dcopper                               ! inner legs
       whttf = whtcp + whttflgs                              ! total

    ! Aluminium
    else if ( i_tf_sup == 2 ) then
       whttflgs = voltfleg * n_tf * (1.0D0 - vftf) * dalu  ! outer legs
       whtcp = volcp * dalu                                ! inner legs
       whttf = whtcp + whttflgs                            ! total
    end if

    !  Total weight of TF coils
    whttf = whtcp + whttflgs
    ! ******


    ! Stress information (radial, tangential, vertical)
    ! JUST WRONG !!!! TO BE RE-SET USING THE SC FORMULA
    sigrad = 1.0D-6 * bmaxtf**2 * (5.0D0 + 0.34D0)/(8.0D0*rmu0*(1.0D0-fcoolcp))
    sigtan = sigrad
    sigver = 0.0D0   ! S.K. : No vertical stress in presence of vertical forces ??

    ! Inductance 
    ! S.K. : COMPUTED WITH AN INTEGRAL IN THE SC CASE
    tfcind1 = (hmax + tfthko) * rmu0/pi * log(r_tf_outleg_in/r_tf_inleg_out)

    ! Stored energy (GJ)
    estotftgj = 0.5D-9 * tfcind1 * ritfc**2 
    ! -----------------------------


    ! Printing the final results in the output file
    ! ---------------------------------------------
    if (iprint == 0) return

    !  Output section
    call oheadr(outfile,'Resistive TF Coil Information')
    call ovarin(outfile,'Resistive TF coil option (0:copper 2:aluminium)','(i_tf_sup)',i_tf_sup)
    !call ovarre(outfile,'Inboard leg current density (A/m2)','(oacdcp)',oacdcp)
    call ovarre(outfile,'Outboard leg current density (A/m2)','(cdtfleg)',cdtfleg)
    call ovarre(outfile,'Number of turns per outboard leg','(turnstf)',turnstf)
    call ovarre(outfile,'Outboard leg current per turn (A)','(cpttf)',cpttf)
    call ovarre(outfile,'Inboard leg volume (m3)','(volcp)',volcp)
    call ovarre(outfile,'Outboard leg volume per coil (m3)','(voltfleg)',voltfleg)
    call ovarre(outfile,'Mass of inboard legs (kg)','(whtcp)',whtcp)
    call ovarre(outfile,'Mass of outboard legs (kg)','(whttflgs)',whttflgs)
    call ovarre(outfile,'Total TF coil mass (kg)','(whttf)',whttf)
    call ovarre(outfile,'Inboard leg resistive power (W)','(prescp)',prescp)
    call ovarre(outfile,'Outboard leg resistance per coil (ohm)','(tflegres)',tflegres)
    call ovarre(outfile,'Average inboard leg temperature (K)','(tcpav)',tcpav)
    if (itart==1) then
      call osubhd(outfile,'Tapered Centrepost Dimensions:')
      call ovarre(outfile,'Radius of the centrepost at the midplane (m)','(rmid)',rmid)
      call ovarre(outfile,'Radius of the ends of the centrepost (m)','(rtop)',rtop)
      call ovarre(outfile,'Distance from the midplane to the top of the tapered section (m)','(ztop)',ztop)
      call ovarre(outfile,'Distance from the midplane to the top of the centrepost (m)','(hmax)',hmax + tfthko)
    end if
    ! ---------------------------------------------

  end subroutine concoptf

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
    real(kind(1.0D0)) :: acool,acpav,amid,dcool,dpres,dtcncpav,dtconcpmx, &
         dtfilmav,fc,fricfac,h,lcool,nuselt,pcrt,presin,prndtl, &
         psat,ptot,reyn,rmid,ro,roughrat,sum,tclmx,tclmxs,tcoolmx,tmarg,vcoolav, &
         rmid_in, coolant_density, coolant_th_cond, coolant_visco, coolant_cp,&
         conductor_th_cond, dptot, tcool_calc
   
    integer, parameter :: n_tcool_it = 20
    integer :: ii

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    
    !  Temperature margin used in calculations (K)
    tmarg = 10.0D0

    !  Midplane radius and area
    rmid_in = max(0.0D0, (rbmax - tfcth) ) ! Radius of inner edge of inboard TF coil leg (m) 
    rmid    = tfcth
    amid    = pi * ( rmid**2 - rmid_in**2 )

    !  Average cross-sectional area
    acpav = volcp/(2.0D0*(hmax + tfthko))

    !  Coolant channels:
    acool = acpav * fcoolcp         !  Cross-sectional area
    dcool = 2.0D0 * rcool           !  Diameter
    lcool = 2.0D0 * (hmax + tfthko) !  Length
    ncool = acool/( pi * rcool**2)  !  Number

    ro = sqrt ( acpav/(pi*ncool) )

    !  Inner legs total heating power (to be removed by coolant)
    ptot = prescp + pnuccp*1.0D6


    !  Temperature calculations
    ! -------------------------
    !  Temperature rise in coolant (inlet to outlet)
    ! **********************************************
    vcoolav = vcool * amid/acpav

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

       ! Ref : R.W. Powel, National Standard Reference Data Series, nov 25 1966 (homemade fit 15 < T < 60 K)
       conductor_th_cond = 16332.2073D0 - 776.91775*tcpav + 13.405688D0*tcpav**2 - 8.01D-02*tcpav**3 ! W/(m.K)
    end if 
    ! ******

    ! Average temperature rise : To be changed with Gary's better documented formula (or add a switch?)
    dtcncpav = (ptot/volcp)/(2.0D0*conductor_th_cond*(ro**2 - rcool**2) ) * &
         ( ro**2*rcool**2 - 0.25D0*rcool**4 - 0.75D0*ro**4 + ro**4 * &
         log(ro/rcool) )

    ! Peak temperature rise : To be changed with Gary's better documented formula (or add a switch?)
    dtconcpmx = (ptot/volcp)/(2.0D0*conductor_th_cond) * &
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






  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cpost(rtop,ztop,rmid,hmax,curr,rho,fcool,r_tfin_inleg,volume,respow)

    !! Calculates the volume and resistive power losses of a TART centrepost
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rtop   : input real : Radius of the ends of the centrepost (m)
    !! ztop   : input real : Distance from the midplane to the top of the
    !! tapered section (m)
    !! rmid   : input real : Radius of the centrepost at the midplane (m)
    !! hmax   : input real : Distance from the midplane to the top of the
    !! centrepost (m)
    !! curr   : input real : Centrepost current (A)
    !! rho    : input real : Centrepost resistivity (Ohm-m)
    !! fcool  : input real : Coolant fraction of centrepost
    !! r_tfin_inleg : input real : Inner radius of the TF inner leg
    !! volume : output real : Centrepost volume (m3)
    !! respow : output real : Centrepost resistive power losses (W)
    !! This routine calculates the volume and resistive power losses
    !! of a TART centrepost. It is assumed to be tapered - narrowest at
    !! the midplane and reaching maximum thickness at the height of the
    !! plasma. Above/below the plasma, the centrepost is cylindrical.
    !! The shape of the taper is assumed to be an arc of a circle.
    !! F/MI/PJK/LOGBOOK12, pp.33,34
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: rtop,ztop,rmid,hmax,curr,rho,fcool,&
                                      &r_tfin_inleg
    real(kind(1.0D0)), intent(out) :: volume,respow

    !  Local variables
    real(kind(1.0D0)) :: r1,z1,x,y,rc,sum1,sum2,dz,r,z, a_tfin_hole, a_cp_cool
    real(kind(1.0D0)), dimension(0:100) :: yy
    integer :: ii

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Error traps
    ! ------------
    if (rtop <= 0.0D0) then
       fdiags(1) = rtop ; call report_error(115)
    end if

    if (ztop <= 0.0D0) then
       fdiags(1) = ztop ; call report_error(116)
    end if

    if (rmid <= 0.0D0) then
       fdiags(1) = rmid ; call report_error(117)
    end if

    if (hmax <= 0.0D0) then
       fdiags(1) = hmax ; call report_error(118)
    end if

    if ((fcool < 0.0D0).or.(fcool > 1.0D0)) then
       fdiags(1) = fcool ; call report_error(119)
    end if

    if (rtop < rmid) then
       fdiags(1) = rtop ; fdiags(2) = rmid
       call report_error(120)
    end if

    if (hmax < ztop) then
       fdiags(1) = hmax ; fdiags(2) = ztop
       call report_error(121)
    end if
    ! ------------


    !  Trivial solutions
    ! ------------------
    if (fcool == 1.0D0) then
       volume = 0.0D0
       respow = 0.0D0
       call report_error(122)
       return
    end if

    if (rmid == rtop) then
       volume = 2.0D0*hmax * pi*rmid*rmid
       respow = 2.0D0 * rho * curr*curr * hmax / (pi*rmid*rmid) / (1.0D0-fcool)
       return
    end if
    ! ------------------


    !  Find centre of circle (RC,0) defining the taper's arc
    !  (r1,z1) is midpoint of line joining (rmid,0) and (rtop,ztop)
    ! -------------------------------------------------------------
    r1 = 0.5D0*(rmid + rtop)
    z1 = 0.5D0*ztop

    x = (r1-rmid)**2 + z1**2
    y = ztop**2 / ( (rtop-rmid)**2 + ztop**2 )

    rc = rmid + sqrt( x / (1.0D0-y) )
    ! -------------------------------------------------------------


    !  Find volume of tapered section of centrepost, and the resistive
    !  power losses, by integrating along the centrepost from the midplane
    ! --------------------------------------------------------------------
    !  Calculate centrepost radius and cross-sectional area at each Z
    dz = 0.01D0*ztop

    ! Area of the innner TF central hole
    a_tfin_hole = pi*r_tfin_inleg**2
    
    ! Coolant area (invariant with z)
    a_cp_cool = tfareain * fcool

    do ii = 0,100
       z = dble(ii) * dz
       z = min(z,ztop)

       r = rc - sqrt( (rc-rmid)**2 - z*z )

       if (r <= 0.0D0) then
          fdiags(1) = r ; fdiags(2) = rc
          fdiags(3) = rmid ; fdiags(4) = z
          call report_error(123)
       end if

       !  Cross-sectional area at Z
       yy(ii) = pi*r**2 - a_tfin_hole - a_cp_cool

    end do

    !  Perform integrals using trapezium rule
    sum1 = 0.0D0
    sum2 = 0.0D0
    do ii = 1,99
       sum1 = sum1 + yy(ii)
       sum2 = sum2 + 1.0D0/yy(ii)
    end do

    sum1 = 0.5D0*dz * ( yy(0) + yy(100) + 2.0D0*sum1 )
    sum2 = 0.5D0*dz * ( 1.0D0/yy(0) + 1.0D0/yy(100) + 2.0D0*sum2 )

    !  Centrepost volume (ignoring coolant fraction)
    volume = 2.0D0 * (sum1 + (hmax-ztop)*(pi*rtop**2 - a_cp_cool - a_tfin_hole ))
    
    !  Resistive power losses
    respow = 2.0D0 * rho * curr**2 * (sum2 + (hmax-ztop)/(pi*rtop**2 - a_cp_cool - a_tfin_hole ))
    ! --------------------------------------------------------------------
  end subroutine cpost

  subroutine cutfshape

    !! Calculates the TF coil shape
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! Calculates the shape of the INSIDE of the TF coil. The coil is
    !! given by a rectangular shape.

    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xarc(1) = r_tf_inboard_mid + 0.5D0*tfcth
    xarc(2) = r_tf_outboard_mid - 0.5D0*tfthko
    xarc(3) = xarc(2)
    xarc(4) = xarc(2)
    xarc(5) = xarc(1)

    yarc(1) = hpfu - tfcth
    yarc(2) = hpfu - tfcth
    yarc(3) = 0
    yarc(4) = -hmax
    yarc(5) = -hmax


end subroutine cutfshape


end module tfcoil_module
