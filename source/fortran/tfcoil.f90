! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module tfcoil_module

  !+ad_name  tfcoil_module
  !+ad_summ  Module containing resistive TF coil routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  tfcoil
  !+ad_cont  concoptf
  !+ad_cont  cntrpst
  !+ad_cont  cpost
  !+ad_cont  cutfshape
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of a resistive TF coil system for a fusion power plant.
  !+ad_prob  None
  !+ad_call  build_module
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  sctfcoil_module
  !+ad_call  tfcoil_variables
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_hist  29/10/12 PJK Added sctfcoil_module
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  30/10/12 PJK Added build_module
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_module
  use build_variables
  use constants
  use error_handling
  use fwbs_variables
  use physics_variables
  use process_output
  use sctfcoil_module
  use tfcoil_variables

  private
  public :: tfcoil, cntrpst

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfcoil(outfile,iprint)

    !+ad_name  tfcoil
    !+ad_summ  TF coil module
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calculates various parameters for the TF coil set.
    !+ad_desc  If the TF coils are superconducting the calculations are performed
    !+ad_desc  in routine <A HREF="sctfcoil.html">sctfcoil</A> instead.
    !+ad_prob  None
    !+ad_call  concoptf
    !+ad_call  cutfshape
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_call  portsz
    !+ad_call  sctfcoil
    !+ad_hist  22/10/96 PJK Initial upgraded version
    !+ad_hist  08/05/12 PJK Initial F90 version
    !+ad_hist  08/10/12 PJK Swapped concoptf argument order
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  09/04/13 PJK Changed local variables routr to rout, rinr to rin
    !+ad_hist  18/06/13 PJK Clarified ritfc output description
    !+ad_hist  08/05/14 PJK Changed ripmax description
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  24/06/14 PJK Removed refs to bcylth
    !+ad_hist  30/07/14 PJK Added tftort calculation
    !+ad_hist  30/07/14 PJK Changed estotf output
    !+ad_hist  11/04/18 SIM Added shape output
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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
    if ( itfsup == 0 ) then  
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
    if (itfsup == 0) then

       call oheadr(outfile,'TF Coils')
       call ovarre(outfile,'TF coil current (summed over all coils) (A)','(ritfc)',ritfc)
       call ovarre(outfile,'Peak field at the TF coils (T)','(bmaxtf)',bmaxtf)
       call ovarre(outfile,'Ripple at plasma edge (%)','(ripple)',ripple)
       call ovarre(outfile,'Max allowed ripple amplitude at plasma (%)','(ripmax)',ripmax)
       call ovarre(outfile,'Number of TF coil legs','(tfno)',tfno)

       call osubhd(outfile,'Energy and Forces :')
       call ovarre(outfile,'Total stored energy in TF coils (GJ)','(estotf*tfno)',estotf*tfno)
       call ovarre(outfile,'Vertical force on inboard leg (N)','(vforce)',vforce)
       call ovarre(outfile,'Centering force on inboard leg (N/m)','(cforce)',cforce)
       call ovarre(outfile,'Radial stress (MPa)','(sigrad)',sigrad)
       call ovarre(outfile,'Transverse stress (MPa)','(sigtan)',sigtan)
       call ovarre(outfile,'Vertical stress (MPa)','(sigver)',sigver)

       call concoptf(outfile,iprint)

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

    !+ad_name  concoptf
    !+ad_summ  Calculates additional parameters for resistive TF coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calculates various additional parameters for a
    !+ad_desc  resistive TF coil set, including for TART machines.
    !+ad_prob  None
    !+ad_call  cpost
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  22/10/96 PJK Initial upgraded version
    !+ad_hist  18/11/97 PJK Modified RTOP,ZTOP values
    !+ad_hist  08/05/12 PJK Initial F90 version
    !+ad_hist  08/10/12 PJK Swapped argument order
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  28/11/13 PJK Modified ltfleg calculation
    !+ad_hist  24/04/14 PJK Calculation proceeds irrespective of iprint
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  24/06/14 PJK Removed refs to bcylth
    !+ad_hist  22/06/18 SIM Made cdtfleg an output instead of an input
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    integer, intent(in) :: iprint,outfile

    !  Local variables
    real(kind(1.0D0)) :: r_tf_inner, r_tf_outer, rout
    real(kind(1.0D0)) :: ltfleg, rmid, rtop, ztop
    real(kind(1.0D0)) :: tfcind1, deltf
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ! Inner leg(s) quantities
    ! -----------------------
    ! Radial build
    ! ******
    ! Radial position of inner edge of inboard TF coil leg [m]
    r_tf_inner = bore + ohcth + precomp + gapoh
    
    ! Radial position of plasma-facing edge of TF coil inboard leg [m]
    r_tf_outer = r_tf_inner + tfcth

    ! Position of the maximum magnetic field 
    ! No winding pack structure -> simply the innner legs plasma side radius
    rbmax = r_tf_outer
    
    ! Gap between inboard TF coil and thermal shield [m]
    deltf = tftsgap

    ! Radial position of centre of inboard TF coil leg [m]
    rtfcin = r_tf_inner + 0.5D0*tfcth
    ! ******


    ! Toroidal thickness of TF coil (m)
    tftort = 2.0D0 * rbmax*sin(pi/tfno)

    ! Inboard total cross-sectional area (m2)
    tfareain = pi * (r_tf_outer**2 - r_tf_inner**2)


    ! Inner legs physics
    ! ******
    ! Total current flowing through inboard TF coil legs (A)
    ritfc = tfareain * oacdcp

    ! Peak field (T)
    bmaxtf = 2.0D-7 * ritfc / rbmax

    !  Centering force
    if (bore == 0.0D0) then
       cforce = 0.0D0
    else
       cforce = bmaxtf * ritfc/(2.0D0*tfno)  !  N/m
    end if
    ! ******


    ! Power losses 
    ! ******
    !  Resistivity (0.92 factor for glidcop C15175)
    rhocp = 1.0D-8 * (1.72D0 + 0.0039D0*tcpav) / 0.92D0

    ! Conventionnal tokamak (geometry invariant with hight)
    if (itart == 0) then 
       volcp = 2.0D0 * tfareain * hmax  !  volume

      !  Inner legs resistive power losses
      prescp = rhocp * ( ritfc/(tfareain*(1.0D0-fcoolcp)) )**2 &
             * volcp * (1.0D0-fcoolcp)

    !  Spherical Tokamak (larger diameter at top/bottom)
    else  
       !  Radii and vertical height from midplane
       rtop = (rmajor - rminor*triang - fwith - 3.0D0*scrapli) + drtop
       rmid = r_tf_outer
       rtop = max(rtop, (rmid*1.01D0))
       ztop = (rminor * kappa) + dztop

       !  Resistivity enhancement factor
       rhocp = rhocp * frhocp

       !  Volume and resistive power losses of TART centrepost
       call cpost(rtop,ztop,rmid,hmax,ritfc,rhocp,fcoolcp,r_tf_inner,volcp,prescp)
    end if

    ! ******
    ! -----------------------


    ! Outboard leg information (per leg)
    ! ----------------------------------    
    ! Radius of inner edge of outboard TF coil leg (m)
    rout = rtot - 0.5D0*tfcth
    
    ! Cross-sectional area
    arealeg = tfthko*tftort

    ! Outboard leg current density
    cdtfleg = ritfc/(tfno * arealeg)

    ! Length of leg centre-line (N.B. this assumes rectangular shaped
    ! coils, not D-shaped)
    ltfleg = hmax + hpfu + 2.0D0*(rtot - rbmax)

    ! Volume
    voltfleg = ltfleg * arealeg

    ! Resistance
    rhotfleg = ltfleg * tflegres/arealeg
    ! ----------------------------------


    ! Inner outter common quantities
    ! -----------------------------
    !  Bore (gap between inboard and outboard TF coil legs) (m)
    tfboreh = rtot - rbmax - 0.5D0*tfcth

    ! Vertircal force
    vforce = 0.55D0 * bt * rmajor * 0.5D0*ritfc * log(rout/r_tf_outer) / tfno 

    ! Current turn information 
    if (itart == 0) then ! CT case
       !  Number of turns per leg
       turnstf = ritfc / (tfno * cpttf)
    else                 ! ST case
       !  Current per turn (N.B. cannot set CPTTF as an iteration variable for ST)
       turnstf = 1.0D0
       cpttf = ritfc/(turnstf*tfno)
    end if

    ! Weight of conductor (outer legs, inner legs, total)
    ! Coolant density assumed negligible (gaz) 
    whttflgs = voltfleg * tfno * (1.0D0 - vftf) * dcopper 
    whtcp = volcp * dcopper * (1.0D0 - fcoolcp)
    whttf = whtcp + whttflgs

    ! Stress information (radial, tangential, vertical)
    sigrad = 1.0D-6 * bmaxtf**2 * (5.0D0 + 0.34D0)/(8.0D0*rmu0*(1.0D0-fcoolcp))
    sigtan = sigrad
    sigver = 0.0D0

    ! Inductance
    tfcind1 = hmax * rmu0/pi * log(rout/r_tf_outer)

    ! Stored energy per coil (GJ)
    estotf = 0.5D-9 * tfcind1 * ritfc**2 / tfno
    ! -----------------------------


    ! Printing the final results in the output file
    ! ---------------------------------------------
    if (iprint == 0) return

    !  Output section
    call osubhd(outfile,'Conventional Copper TF Coil Information :')
    call ovarin(outfile,'Copper TF coil','(itfsup)',itfsup)
    call ovarre(outfile,'Inboard leg current density (A/m2)','(oacdcp)',oacdcp)
    call ovarre(outfile,'Outboard leg current density (A/m2)','(cdtfleg)',cdtfleg)
    call ovarre(outfile,'Number of turns per outboard leg','(turnstf)',turnstf)
    call ovarre(outfile,'Outboard leg current per turn (A)','(cpttf)',cpttf)
    call ovarre(outfile,'Inboard leg volume (m3)','(volcp)',volcp)
    call ovarre(outfile,'Outboard leg volume per coil (m3)','(voltfleg)',voltfleg)
    call ovarre(outfile,'Mass of inboard legs (kg)','(whtcp)',whtcp)
    call ovarre(outfile,'Mass of outboard legs (kg)','(whttflgs)',whttflgs)
    call ovarre(outfile,'Total TF coil mass (kg)','(whttf)',whttf)
    call ovarre(outfile,'Inboard leg resistive power (W)','(prescp)',prescp)
    call ovarre(outfile,'Outboard leg resistance per coil (ohm)','(rhotfleg)',rhotfleg)
    call ovarre(outfile,'Average inboard leg temperature (C)','(tcpav)',tcpav)
    ! ---------------------------------------------

  end subroutine concoptf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cntrpst(outfile,iprint)

    !+ad_name  cntrpst
    !+ad_summ  Evaluates the properties of a TART centrepost
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine evaluates the parameters of the centrepost for a
    !+ad_desc  tight aspect ratio tokamak. The centrepost is assumed to be tapered,
    !+ad_desc  i.e. narrowest on the midplane (z=0).
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  22/10/96 PJK Initial upgraded version
    !+ad_hist  08/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  24/06/14 PJK Removed refs to bcylth
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint

    !  Local variables
    real(kind(1.0D0)) :: acool,acpav,amid,dcool,dpres,dtcncpav,dtconcpmx, &
         dtfilmav,dtiocool,fc,fricfac,h,lcool,nuselt,pcrt,presin,prndtl, &
         psat,ptot,reyn,rmid,ro,roughrat,sum,tclmx,tclmxs,tcoolmx,tmarg,vcoolav, &
         rmid_in

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    

    !  Critical pressure in saturation pressure calculations (Pa)
    pcrt = 2.24D7

    !  Temperature margin used in calculations (K)
    tmarg = 10.0D0

    !  Midplane radius and area
    rmid_in = max(0.0D0, (rbmax - tfcth) ) ! Radius of inner edge of inboard TF coil leg (m) 
    rmid    = tfcth
    amid    = pi * ( rmid**2 - rmid_in**2 )

    !  Average cross-sectional area
    acpav = volcp/(2.0D0*hmax)

    !  Coolant channels:
    acool = acpav * fcoolcp  !  Cross-sectional area
    dcool = 2.0D0 * rcool    !  Diameter
    lcool = 2.0D0 * hmax     !  Length
    ncool = acool/( pi * rcool**2)  !  Number

    ro = sqrt ( acpav/(pi*ncool) )

    !  Inner legs total heating power (to be removed by coolant)
    ptot = prescp + pnuccp*1.0D6

    !  Temperature calculations
    reyn = denh2o * vcool * dcool / muh2o
    prndtl = cph2o  * muh2o / kh2o

    !  Temperature rise in coolant (inlet to outlet)
    vcoolav = vcool * amid/acpav
    dtiocool = ptot / (denh2o*vcoolav*acool*cph2o)

    !  Film temperature rise
    nuselt = 0.023D0 * reyn**0.8D0 * prndtl**0.3D0
    h = nuselt * kh2o / dcool
    dtfilmav = ptot / (h * 2.0D0*pi*rcool * ncool * lcool)

    !  Temperature rise in conductor,
    !  for conduction from copper to coolant
    dtcncpav = (ptot/volcp)/(2.0D0*kcp*(ro**2 - rcool**2) ) * &
         ( ro**2*rcool**2 - 0.25D0*rcool**4 - 0.75D0*ro**4 + ro**4 * &
         log(ro/rcool) )

    dtconcpmx = (ptot/volcp)/(2.0D0*kcp) * &
         ( (rcool**2 - ro**2)/2.0D0 + ro**2 * log(ro/rcool) )

    !  Average conductor temperature
    tcpav2 = tcoolin + dtcncpav + dtfilmav + 0.5D0*dtiocool

    !  Peak wall temperature
    tcpmax = tcoolin + dtiocool + dtfilmav + dtconcpmx
    tcoolmx = tcoolin + dtiocool + dtfilmav

    !  Thermal hydraulics: friction factor from Z. Olujic, Chemical
    !  Engineering, Dec. 1981, p. 91
    roughrat = 0.046D-3 / dcool
    fricfac = 1.0D0/ (-2.0D0 * log(roughrat/3.7D0 - 5.02D0/reyn * &
         log( roughrat/3.7D0 + 14.5D0/reyn) ) )**2

    dpres = fricfac * (lcool/dcool) * denh2o * 0.5D0*vcool**2
    ppump = dpres * acool * vcool / etapump

    !  Saturation pressure
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
    call ovarre(outfile,'Resistive heating (W)','(prescp)',prescp)

    call osubhd(outfile,'Temperatures :')
    call ovarre(outfile,'Input coolant temperature (C)','(tcoolin)',tcoolin)
    call ovarre(outfile,'Input-output coolant temperature rise (C)','(dtiocool)',dtiocool)
    call ovarre(outfile,'Film temperature rise (C)','(dtfilmav)',dtfilmav)
    call ovarre(outfile,'Average temp gradient in conductor (K/m)','(dtcncpav)',dtcncpav)
    call ovarre(outfile,'Average centrepost temperature (C)','(tcpav2)',tcpav2)
    call ovarre(outfile,'Peak centrepost temperature (C)','(tcpmax)',tcpmax)

    call osubhd(outfile,'Pump Power :')
    call ovarre(outfile,'Coolant pressure drop (Pa)','(dpres)',dpres)
    call ovarre(outfile,'Coolant inlet pressure (Pa)','(presin)',presin)
    call ovarre(outfile,'Pump power (W)','(ppump)',ppump)

  end subroutine cntrpst






  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cpost(rtop,ztop,rmid,hmax,curr,rho,fcool,r_tfin_inleg,volume,respow)

    !+ad_name  cpost
    !+ad_summ  Calculates the volume and resistive power losses of a TART centrepost
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rtop   : input real : Radius of the ends of the centrepost (m)
    !+ad_args  ztop   : input real : Distance from the midplane to the top of the
    !+ad_argc                        tapered section (m)
    !+ad_args  rmid   : input real : Radius of the centrepost at the midplane (m)
    !+ad_args  hmax   : input real : Distance from the midplane to the top of the
    !+ad_argc                        centrepost (m)
    !+ad_args  curr   : input real : Centrepost current (A)
    !+ad_args  rho    : input real : Centrepost resistivity (Ohm-m)
    !+ad_args  fcool  : input real : Coolant fraction of centrepost
    !+ad_args  r_tfin_inleg : input real : Inner radius of the TF inner leg
    !+ad_args  volume : output real : Centrepost volume (m3)
    !+ad_args  respow : output real : Centrepost resistive power losses (W)
    !+ad_desc  This routine calculates the volume and resistive power losses
    !+ad_desc  of a TART centrepost. It is assumed to be tapered - narrowest at
    !+ad_desc  the midplane and reaching maximum thickness at the height of the
    !+ad_desc  plasma. Above/below the plasma, the centrepost is cylindrical.
    !+ad_desc  The shape of the taper is assumed to be an arc of a circle.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  21/10/96 PJK Initial version
    !+ad_hist  08/05/12 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Added constants; removed argument pi
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK12, pp.33,34
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: rtop,ztop,rmid,hmax,curr,rho,fcool,&
                                      &r_tfin_inleg
    real(kind(1.0D0)), intent(out) :: volume,respow

    !  Local variables
    real(kind(1.0D0)) :: r1,z1,x,y,rc,sum1,sum2,dz,r,z, a_tfin_hole
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
       yy(ii) = pi*r*r - a_tfin_hole

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
    volume = 2.0D0 * (sum1 + (hmax-ztop)*pi*rtop*rtop)
    
    !  Resistive power losses
    respow = 2.0D0 * rho * curr*curr * (sum2 + (hmax-ztop)/(pi*rtop*rtop)) &
         / (1.0D0-fcool)
    ! --------------------------------------------------------------------
  end subroutine cpost

  subroutine cutfshape

    !+ad_name  cutfshape
    !+ad_summ  Calculates the TF coil shape
    !+ad_type  Subroutine
    !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
    !+ad_desc  Calculates the shape of the INSIDE of the TF coil. The coil is
    !+ad_desc  given by a rectangular shape.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/04/18 SIM Copied from the SC TF subroutine coilshap
    !+ad_hist  31/10/18 SIM Updated for a rectangular coil shape.
    !+ad_stat  Okay

    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xarc(1) = rtfcin + tfcth/2.0d0
    xarc(2) = rtot - tfthko/2.0d0
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
