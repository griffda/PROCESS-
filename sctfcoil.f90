module sctfcoil_module

!+ad_name  sctfcoil_module
!+ad_summ  Module containing superconducting TF coil routines
!+ad_type  Module
!+ad_auth  P J Knight, CCFE, Culham Science Centre
!+ad_auth  J Morris, CCFE, Culham Science Centre
!+ad_cont  bi2212
!+ad_cont  coilshap
!+ad_cont  edoeeff
!+ad_cont  eyngeff
!+ad_cont  eyngzwp
!+ad_cont  itersc
!+ad_cont  jcrit_nbti
!+ad_cont  outtf
!+ad_cont  sctfcoil
!+ad_cont  sctfjalw
!+ad_cont  sigvm
!+ad_cont  stresscl
!+ad_cont  tfcind
!+ad_cont  tfspcall
!+ad_cont  two_layer_stress
!+ad_args  N/A
!+ad_desc  This module contains routines for calculating the
!+ad_desc  parameters of a superconducting TF coil system for a
!+ad_desc  fusion power plant.
!+ad_prob  None
!+ad_call  build_variables
!+ad_call  constants
!+ad_call  error_handling
!+ad_call  fwbs_variables
!+ad_call  maths_library
!+ad_call  physics_variables
!+ad_call  process_output
!+ad_call  tfcoil_variables
!+ad_hist  29/10/12 PJK Initial version of module
!+ad_hist  16/09/14 PJK Removed myall_stress routine
!+ad_hist  14/14/15 JM  Added output of peak field fit values if run_test=1
!+ad_hist  27/02/17 JM  Added wstsc parameterisation
!+ad_stat  Okay
!+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use build_variables
use constants
use error_handling
use fwbs_variables
use maths_library
use physics_variables
use process_output
use tfcoil_variables
use superconductors
use resistive_materials
use ode_mod
implicit none


private
public :: bi2212, itersc, wstsc, jcrit_nbti, outtf, sctfcoil, stresscl, &
tfcind, tfspcall, initialise_cables

!  Module variables
!  Dimensionless winding pack width
real(kind(1.0D0)), private :: tf_fit_t

!  Dimensionless winding pack radial thickness
real(kind(1.0D0)), private :: tf_fit_z

!  Ratio of peak field with ripple to nominal axisymmetric peak field
real(kind(1.0D0)), private :: tf_fit_y

! Current in each TF coil
real(kind(1.0D0))::tfc_current

type(resistive_material):: copper
type(resistive_material):: hastelloy
type(resistive_material):: solder
type(resistive_material):: jacket
type(resistive_material):: helium
type(volume_fractions):: conductor
type(supercon_strand)::croco_strand

real(kind(1.0D0)):: T1, time2, tau2,estotft,croco_quench_factor
real(kind(1.0D0)):: jwdgpro_1, jwdgpro_2,  etamax
contains

! --------------------------------------------------------------------------
subroutine initialise_cables()
    copper%rrr = copper_rrr
    copper%density = 8960.0d0
    hastelloy%density = 8890.0d0
    ! Solder: 60EN ie 60%Sn + 40%Pb solder (approx the same as eutectic 63/37)
    solder%density = 8400.0d0
    jacket%density = 8000.0d0       ! 304 stainless
end subroutine initialise_cables
! --------------------------------------------------------------------------

subroutine sctfcoil(outfile,iprint)

    !+ad_name  sctfcoil
    !+ad_summ  Superconducting TF coil module
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calculates various parameters for a superconducting
    !+ad_desc  TF coil set. The primary outputs are coil size, shape, stress,
    !+ad_desc  and fields.
    !+ad_desc  <P>It is a variant from the original FEDC/Tokamak systems code.
    !+ad_prob  None
    !+ad_call  coilshap
    !+ad_call  outtf
    !+ad_call  peak_tf_with_ripple
    !+ad_call  report_error
    !+ad_call  stresscl
    !+ad_call  tfcind
    !+ad_hist  04/11/92 PJK Initial version
    !+ad_hist  25/07/11 PJK Simplified outboard leg cross-section
    !+ad_hist  10/05/12 PJK Initial F90 version
    !+ad_hist  18/12/12 PJK/RK Modified vertical bore for single-null cases
    !+ad_hist  06/11/13 PJK Modified coil case mass and leg area calculations
    !+ad_hist  26/02/14 PJK Changed comment in the case of too small tftort
    !+ad_hist  23/04/14 PJK Modified TF coil leg length calculation
    !+ad_hist  28/04/14 PJK/JM Corrected awpc calculation;
    !+ad_hisc  06/05/14 JM  Remove WPVF from the current density calculation
    !+ad_hisc               and from the output
    !+ad_hist  08/05/14 PJK Introduced tfc_model as the controlling switch
    !+ad_hist  24/06/14 PJK Removed obsolete dct variable and references to
    !+ad_hisc               a bucking cylinder
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  30/07/14 PJK Calculate tftort instead of using input value
    !+ad_hist  30/07/14 PJK Renamed borev to tfborev; changed tfthko calculation
    !+ad_hist  31/07/14 PJK tfthko is now set to tfcth elsewhere; added extra
    !+ad_hisc               mass calculations
    !+ad_hist  02/09/14 PJK New peak field with ripple calculation
    !+ad_hist  26/11/15 RK  Quench time calculation, WP insertion gap
    !+ad_hist  08/12/15 MDK New TF coil shape with straight vertical section
    !+ad_hist  19/01/16 JM  Updated tfboreh and tfborev for new radial build
    !+ad_hist  27/02/17 JM  Added WST Nb3Sn option for superconductor
    !+ad_hist  10/05/17 MDK Issue #478 Removed radial plate option
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    integer, intent(in) :: iprint,outfile

    !  Local variables
    !integer :: i,peaktfflag
    integer :: peaktfflag
    real(kind(1.0D0)) :: awpc,awptf,cplen
    real(kind(1.0D0)) :: radwp,rbcndut,rcoil,rcoilp,tant,thtcoil,wbtf, radvv, deltf
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Set the plasma-facing wall thickness if it has not been set explicitly
    if(casthi_is_fraction) casthi = casthi_fraction * tfcth

    ! Issue #514 Radial dimensions of inboard leg
    ! Calculate tfcth if thkwp is an iteration variable (140)
    if (any(ixc(1:nvar) == 140) ) then
        tfcth = thkwp + casthi + thkcas + 2.0D0*tinstf + 2.0d0*tfinsgap
    else
        thkwp = tfcth - casthi - thkcas - 2.0D0*tinstf - 2.0d0*tfinsgap
    endif


    !  Radius of centre of inboard TF coil leg
    if (itart == 1) then
        rtfcin = bore + 0.5D0*tfcth
    else
        rtfcin = bore + ohcth + precomp + gapoh + 0.5D0*tfcth
    end if

    !  Major radius of plasma-facing edge of inboard leg
    rcoil = rtfcin + 0.5D0*tfcth

    !  Radius of inner edge of inboard leg
    rcoilp = rcoil - tfcth

    !  Half toroidal angular extent of a single TF coil inboard leg
    thtcoil = pi/tfno
    tant = tan(thtcoil)

    !  TF coil width in toroidal direction
    tftort = 2.0D0 * rcoil*sin(thtcoil)
    !  casths: case thickness of side wall
    if(tfc_sidewall_is_fraction) casths = casths_fraction * tftort

    !  Annular area of midplane containing TF coil inboard legs
    tfareain = pi * (rcoil**2 - rcoilp**2)

    !  Total current in TF coils
    ritfc = oacdcp * tfareain

    tfc_current = ritfc/tfno
    ! Determine quench time (based on IDM: 2MBSE3)
    ! Issue #337: Force on the vessel wall due to TF coil quench

    radvv = rmajor - rminor - scrapli - fwith - blnkith - vvblgap - shldith
    taucq = (bt * ritfc * rminor * rminor) / (radvv * sigvvall)

    !  Peak toroidal field (assuming axisymmetry) and radius of its occurrence,
    !  assumed to be at the outer edge of the winding pack

    rbmax = rcoil - casthi
    bmaxtf = 2.0D-7 * ritfc / rbmax

    !  Calculation of forces : centering and vertical
    cforce = bmaxtf*ritfc/(2.0D0*tfno)
    vforce = 0.5D0 * bt * rmajor * 0.5D0*ritfc * log(rtot/rtfcin) / tfno

    !  The rest of this routine deals with superconducting coils.

    call coilshap

    !  Calculation of TF coil inductance
    call tfcind(tfcth)

    !  Total TF coil stored magnetic energy (J)
    estotft = 0.5D0*tfind * ritfc**2
    !  Total TF coil stored magnetic energy (GJ)
    estotftgj = 1.0D-9 * estotft

    !  Case thicknesses (inboard leg)
    if (tfc_model == 0) thkcas = tfcth * 0.5D0

    !  N.B. Calculations below may be spurious for tfc_model=0 (solid copper coils),
    !  but this probably does not matter

    !  thkcas: case thickness of side further away from plasma
    !  casthi: case thickness of side nearest plasma

    !  Radius of geometrical centre of winding pack
    radwp = rcoil - casthi - tfinsgap - tinstf - 0.5D0*thkwp

    !  Thickness of winding pack section at R > radwp
    wwp1 = 2.0D0 * (radwp*tant - casths - tinstf - tfinsgap)

    !  Thickness of winding pack section at R < radwp
    wwp2 = 2.0D0 * ((radwp-0.5D0*thkwp)*tant - casths - tinstf - tfinsgap)

    !  Total cross-sectional area of winding pack
    awptf = (0.5D0*thkwp)*(wwp1 + wwp2)

    !  Total cross-sectional area of winding pack,
    !  including the surrounding ground-wall insulation layer
    !  and insertion gap

    awpc = 0.5D0*thkwp*(wwp2 + 2.0D0*tinstf + 2.0d0*tfinsgap) + &
    (0.5D0*thkwp + 2.0D0*tinstf + 2.0d0*tfinsgap)*(wwp1 + 2.0D0*tinstf+ 2.0d0*tfinsgap)

    !  Total cross-sectional area of surrounding case
    acasetf = (tfareain/tfno) - awpc

    if ((awptf <= 0.0D0).or.(awpc <= 0.0D0).or.(acasetf <= 0.0D0)) then
        fdiags(1) = awptf ; fdiags(2) = awpc ; fdiags(3) = acasetf
        call report_error(99)
        write(*,*) 'Error in routine SCTFCOIL:'
        write(*,*) 'Winding pack cross-section problem'
        write(*,*) 'awptf = ',awptf
        write(*,*) 'awpc = ',awpc
        write(*,*) 'acasetf = ',acasetf
        write(*,*) ' '
    end if

    !  Area of rectangular cross-section outboard leg
    arealeg = tftort * tfthko

    !  Cross-sectional area of surrounding case, outboard leg
    acasetfo = arealeg - awpc

    !  Winding pack current density (forced to be positive)
    jwptf = max(1.0D0, ritfc/(tfno*awptf))

    !  Superconducting conductor
    !  (number of turns not required to be an integer)

    !  Radius of rounded corners of cable space inside conduit
    rbcndut = thwcndut * 0.75D0

    !  Dimension of square cross-section of each turn including inter-turn insulation
    leno = sqrt(cpttf / jwptf)

    !  Dimension of square conductor
    conductor_width = leno - 2.0D0*thicndut

    !  Dimension of square cable space inside conduit
    leni = conductor_width - 2.0D0*thwcndut

    !  Cross-sectional area of cable space per turn, taking account of rounded inside corners
    acstf = leni**2 - (4.0D0-pi)*rbcndut**2

    if (acstf <= 0.0D0) then
        if (leni < 0.0D0) then
            fdiags(1) = acstf ; fdiags(2) = leni
            call report_error(101)
            write(*,*) 'Warning in routine SCTFCOIL:'
            write(*,*) 'Cable space area, acstf = ',acstf, 'Cable space dimension, leni = ',leni
            write(*,*) ' '
        else
            fdiags(1) = acstf ; fdiags(2) = leni
            call report_error(102)
            write(*,*) 'Warning in routine SCTFCOIL:'
            write(*,*) 'Cable space area, acstf = ',acstf, 'Cable space dimension, leni = ',leni
            write(*,*) 'Reduce the upper limit for thwcndut (TF coil conduitcase thickness, iteration variable 58),'
            write(*,*) 'or remove it from the list of iteration variables.'
            write(*,*) 'Artificially set rounded corner radius to zero'
            write(*,*)
            rbcndut = 0.0D0
            acstf = leni**2
        end if
    end if

    !  Cross-sectional area of conduit jacket per turn
    acndttf = conductor_width**2 - acstf

    !  Total number of turns per TF coil (not required to be an integer)
    turnstf = awptf / (leno*leno)

    ! Central helium channel down the conductor core
    awphec = turnstf * ((pi/4.0d0)*dhecoil**2)

    !  Total conductor cross-sectional area, taking account of void area
    !  and central helium channel
    acond = acstf * turnstf * (1.0D0-vftf) - awphec

    !  Void area in conductor for He, not including central channel
    avwp = acstf * turnstf * vftf

    !  Area of inter-turn insulation: single turn
    insulation_area = leno**2 - acndttf - acstf
    !  Area of inter-turn insulation: total
    aiwp = turnstf * insulation_area

    !  Area of steel structure in winding pack
    aswp = turnstf*acndttf

    ! Coil perimeter along its cross-sectional centre (tfleng):
    ! NOW calculated in  subroutine coilshap

    !  TF coil horizontal and vertical bores
    tfborev = 2.0D0*(rminor*kappa + vgaptop + fwith + blnktth + vvblgap + &
    shldtth + ddwi+ vgap2 + thshield + tftsgap + tfcth)

    deltf = bore + ohcth + precomp + gapoh + tfcth * ((1.0d0 / cos(pi/tfno)) - 1.0d0) + tftsgap
    tfboreh = tfcth + deltf + thshield + gapds + ddwi + shldith + vvblgap + &
    blnkith + fwith + scrapli + rminor + rminor + scraplo + fwoth + &
    blnkoth + vvblgap + shldoth + ddwi + gapsto + thshield + &
    tftsgap + tfthko

    !  TF Coil areas and masses

    !  Surface areas (for cryo system)
    !  tfsai, tfsao are retained for the (obsolescent) TF coil nuclear heating calculation

    wbtf = rcoil*sin(thtcoil) - rcoilp*tant
    tfocrn = rcoilp * tant
    tficrn = tfocrn + wbtf
    tfsai = 4.0D0 * tfno * tficrn * hr1
    tfsao = 2.0D0 * tfno * tficrn * (tfleng - 2.0D0*hr1)

    !  Total surface area of two toroidal shells covering the TF coils
    !  (inside and outside surfaces)
    !  = 2 * centroid coil length * 2 pi R, where R is average of i/b and o/b centres
    !  (This will possibly be used to replace 2*tfsai in the calculation of qss
    !  in subroutine cryo - not done at present.)

    tfcryoarea = 2.0D0 * tfleng * twopi*0.5D0*(rtfcin+rtot)

    !  Mass of case
    ! -------------

    !  The length of the vertical section is that of the first (inboard) segment
    cplen = 2.0D0*(radctf(1) + 0.5D0*tfcth) * dthet(1)

    !  The 2.2 factor is used as a scaling factor to fit
    !  to the ITER-FDR value of 450 tonnes; see CCFE note T&M/PKNIGHT/PROCESS/026
    whtcas = 2.2D0 * dcase * (cplen * acasetf + (tfleng-cplen) * acasetfo)

    !  Mass of ground-wall insulation (assumed to be same density/material as
    !  conduit insulation)
    whtgw = tfleng * (awpc-awptf) * dcondins

    !  Masses of conductor constituents
    !  --------------------------------

    !  Superconductor.
    !  Includes space allowance for central helium channel, area awphec
    whtconsc = (tfleng * turnstf * acstf*(1.0D0-vftf) * (1.0D0-fcutfsu) - tfleng*awphec) &
               *dcond(isumattf)

    !  Copper
    whtconcu = (tfleng * turnstf * acstf*(1.0D0-vftf) * fcutfsu - tfleng*awphec) * dcopper

    !  Steel conduit (sheath)
    whtconsh = tfleng * turnstf * acndttf * denstl

    !  Conduit insulation (aiwp already contains turnstf)
    whtconin = tfleng * aiwp * dcondins

    !  Total conductor mass
    whtcon = whtconsc + whtconcu + whtconsh + whtconin

    !  Total TF coil mass (all coils)
    whttf = (whtcas + whtcon + whtgw) * tfno

    !  Peak field including ripple
    call peak_tf_with_ripple(tfno,wwp1,thkwp,radwp,bmaxtf,bmaxtfrp,peaktfflag)

    !  Do stress calculations
    call stresscl

    if (iprint == 1) call outtf(outfile, peaktfflag)

end subroutine sctfcoil

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine peak_tf_with_ripple(tfno,wwp1,thkwp,tfin,bmaxtf,bmaxtfrp,flag)

    !+ad_name  peak_tf_with_ripple
    !+ad_summ  Peak toroidal field calculation, incuding ripple effects
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  tfno : input real : number of TF coils
    !+ad_args  wwp1 : input real : width of plasma-facing face of winding pack (m)
    !+ad_args  thkwp : input real : radial thickness of winding pack (m)
    !+ad_args  tfin : input real : major radius of centre of winding pack (m)
    !+ad_args  bmaxtf : input real : nominal (axisymmetric) peak toroidal field (T)
    !+ad_args  bmaxtfrp : output real : peak toroidal field including ripple (T)
    !+ad_args  flag : output integer : flag warning of applicability problems
    !+ad_desc  This subroutine calculates the peak toroidal field at the
    !+ad_desc  outboard edge of the inboard TF coil winding pack, including
    !+ad_desc  the effects of ripple.
    !+ad_desc  <P>For 16, 18 or 20 coils, the calculation uses fitting formulae
    !+ad_desc  derived by M. Kovari using MAGINT calculations on coil sets based
    !+ad_desc  on a DEMO1 case.
    !+ad_desc  <P>For other numbers of coils, the original estimate using a 9%
    !+ad_desc  increase due to ripple from the axisymmetric calculation is used.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  02/09/14 PJK Initial version
    !+ad_hist  16/10/14 PJK Turned off output to screen
    !+ad_hist  14/12/15 JM  Changed t,z,y var names for output at end of routine.
    !+ad_stat  Okay
    !+ad_docs  M. Kovari, Toroidal Field Coils - Maximum Field and Ripple -
    !+ad_docc  Parametric Calculation, July 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: tfno,wwp1,thkwp,tfin,bmaxtf
    real(kind(1.0D0)), intent(out) :: bmaxtfrp
    integer, intent(out) :: flag

    !  Local variables

    real(kind(1.0D0)) :: wmax
    real(kind(1.0D0)), dimension(4) :: a

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    flag = 0

    !  Set fitting coefficients for different numbers of TF coils

    select case (nint(tfno))

    case (16)
        a(1) =  0.28101D0
        a(2) =  1.8481D0
        a(3) = -0.88159D0
        a(4) =  0.93834D0

    case (18)
        a(1) =  0.29153D0
        a(2) =  1.81600D0
        a(3) = -0.84178D0
        a(4) =  0.90426D0

    case (20)
        a(1) =  0.29853D0
        a(2) =  1.82130D0
        a(3) = -0.85031D0
        a(4) =  0.89808D0

    case default

        !  Original calculation - no fits were performed

        bmaxtfrp = 1.09D0 * bmaxtf
        return

    end select

    !  Maximum winding pack width before adjacent packs touch
    !  (ignoring the external case and ground wall thicknesses)

    wmax = (2.0D0 * tfin + thkwp) * tan(pi/tfno)

    !  Dimensionless winding pack width

    tf_fit_t = wwp1/wmax
    if ((tf_fit_t < 0.3D0).or.(tf_fit_t > 1.1D0)) then
        !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; t = ',t
        flag = 1
    end if

    !  Dimensionless winding pack radial thickness

    tf_fit_z = thkwp/wmax
    if ((tf_fit_z < 0.26D0).or.(tf_fit_z > 0.7D0)) then
        !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; z = ',z
        flag = 2
    end if

    !  Ratio of peak field with ripple to nominal axisymmetric peak field

    tf_fit_y = a(1) + a(2)*exp(-tf_fit_t) + a(3)*tf_fit_z + a(4)*tf_fit_z*tf_fit_t

    bmaxtfrp = tf_fit_y * bmaxtf

end subroutine peak_tf_with_ripple

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine stresscl

    !+ad_name  stresscl
    !+ad_summ  TF coil stress routine
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine sets up the stress calculations for the
    !+ad_desc  TF coil set.
    !+ad_prob  None
    !+ad_call  edoeeff
    !+ad_call  eyngeff
    !+ad_call  eyngzwp
    !+ad_call  report_error
    !+ad_call  sctfjalw
    !+ad_call  sigvm
    !+ad_call  two_layer_stress
    !+ad_hist  10/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/04/14 PJK/JM Added new stress model option
    !+ad_hist  08/05/14 PJK Replaced itfmod, stress_model with tfc_model;
    !+ad_hisc               split stress calls into two routines
    !+ad_hist  12/05/14 PJK Added insulator strain calculation
    !+ad_hist  12/06/14 PJK Corrections to strtf1, radtf(2) for tfc_model=2
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  16/09/14 PJK Removed myall_stress routine; changed tfc_model usage
    !+ad_hist  02/03/15 JM  Changed von Mises in winding pack region
    !+ad_hist  02/03/15 JM  Changed jeff in winding pack region
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    !integer :: i
    real(kind(1.0D0)) :: seff, tcbs, fac, svmxz, svmyz, t_ins_eff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Simple stress model option

    if (tfc_model == 0) then
        call sctfjalw(bmaxtfrp,rtfcin,rtot,rbmax,(1.0D-6*alstrtf), tdmptf,jwdgcrt)
        return
    end if

    !  Set up graded stress model call information

    seff = sqrt(cpttf/jwptf)
    if (acstf >= 0.0D0) then
        tcbs = sqrt(acstf)
    else
        tcbs = 0.0D0
    end if

    !  CCFE two-layer model
    !  Layers are labelled from inboard to outboard.
    !  The first layer is the steel casing inboard of the winding pack,
    !  while the second layer is the winding pack itself.

    radtf(1) = rtfcin - 0.5D0*tfcth
    radtf(2) = rbmax - thkwp
    radtf(3) = rbmax

    eyoung(1) = eystl

    ! include groundwall insulation + insertion gap in thicndut
    ! inertion gap is tfinsgap on 4 sides
    t_ins_eff = thicndut + ((tfinsgap+tinstf)/turnstf)

    eyoung(2) = eyngeff(eystl,eyins,t_ins_eff,thwcndut,tcbs)

    jeff(1) = 0.0D0
    jeff(2) = ritfc / ( pi * (radtf(3)**2 - radtf(2)**2))

    !  Call stress routine
    call two_layer_stress(poisson,radtf,eyoung,jeff,sigrtf,sigttf,deflect)

    !  Convert to conduit + case

    !fac = eystl*eyins*seff / &
    !     (eyins*(seff-2.0D0*thicndut) + 2.0D0*thicndut*eystl)

    fac = eystl*eyins*seff / &
    (eyins*(seff-2.0D0*t_ins_eff) + 2.0D0*t_ins_eff*eystl)

    sigrcon = sigrtf(2)/eyoung(2) * fac
    sigtcon = sigttf(2)/eyoung(2) * fac
    sigvert = vforce / (acasetf + acndttf*turnstf)

    !  Find case strain
    casestr = sigvert / eystl

    !  Find Von-Mises stresses
    !  For winding pack region take worst of two walls
    svmxz = sigvm(sigrcon, 0.0D0, sigvert, 0.0D0,0.0D0,0.0D0)
    svmyz = sigvm(0.0D0, sigtcon, sigvert, 0.0D0,0.0D0,0.0D0)

    ! von Mises stresses [Pa]
    s_vmises_case = sigvm(sigrtf(1), sigttf(1), sigvert, 0.0D0,0.0D0,0.0D0)
    s_vmises_cond = max(svmxz,svmyz)

    ! Tresca stress criterion [pa]
    s_tresca_case = max(ABS(sigrtf(1)-sigttf(1)), ABS(sigttf(1)-sigvert), ABS(sigvert-sigrtf(1)))
    s_tresca_cond = max(ABS(sigrcon-sigtcon), ABS(sigtcon-sigvert), ABS(sigvert-sigrcon))

    ! Stress to constrain
    strtf1 = s_tresca_cond
    strtf2 = s_tresca_case
    !strtf1 = s_vmises_cond
    !strtf2 = s_vmises_case

    !  Young's modulus and strain in vertical direction on winding pack
    eyzwp = eyngzwp(eystl,eyins,eywp,t_ins_eff,thwcndut,tcbs)
    windstrain = sigvert / eyzwp

    !  Radial strain in insulator
    insstrain = sigrtf(2) / eyins * &
    edoeeff(eystl,eyins,t_ins_eff,thwcndut,tcbs)

end subroutine stresscl

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine two_layer_stress(nu,rad,ey,j,sigr,sigt,deflect)

    !+ad_name  two_layer_stress
    !+ad_summ  Calculates the stresses in a superconductor TF coil
    !+ad_summ  inboard leg at the midplane
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  nu      : input real : Poisson's ratio (assumed constant over entire coil)
    !+ad_args  rad(3)  : input real array : Radius points of regions (m)
    !+ad_argc                 (region i is bounded by rad(i) and rad(i+1) )
    !+ad_args  ey(2)   : input real array : Effective Young's modulus of region i (Pa)
    !+ad_args  j(2)    : input real array : Effective current density of region i (A/m2)
    !+ad_args  sigr(2) : output real array : Radial stress in region i (Pa)
    !+ad_args  sigt(2) : output real array : Tangential stress in region i (Pa)
    !+ad_args  deflect : output real : Deflection at point rad(1) (m)
    !+ad_desc  This routine calculates the stresses in a superconductor TF coil
    !+ad_desc  inboard leg at midplane.
    !+ad_desc  <P>A two-layer model developed by CCFE is used. The first layer
    !+ad_desc  is the steel case inboard of the winding pack, and the second
    !+ad_desc  layer is the winding pack itself.
    !+ad_call  linesolv
    !+ad_hist  08/05/14 PJK/JM Initial version
    !+ad_hist  12/06/14 PJK Corrections to sigr(2), sigt(2)
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: nu
    real(kind(1.0D0)), dimension(3), intent(in) :: rad
    real(kind(1.0D0)), dimension(2), intent(in) :: ey, j
    real(kind(1.0D0)), dimension(2), intent(out) :: sigr, sigt
    real(kind(1.0D0)), intent(out) :: deflect

    !  Local variables

    real(kind(1.0D0)) :: alpha,beta,k1,k2
    real(kind(1.0D0)), dimension(4,4) :: a
    real(kind(1.0D0)), dimension(4) :: b, c

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  LHS matrix A

    k1 = ey(1)/(1.0D0 - nu*nu)
    k2 = ey(2)/(1.0D0 - nu*nu)

    a(:,:) = 0.0D0
    a(1,1) = k1 * (1.0D0+nu)
    a(1,2) = -k1 * (1.0D0-nu)/(rad(1)**2)
    a(2,1) = a(1,1)
    a(2,2) = -k1 * (1.0D0-nu)/(rad(2)**2)
    a(2,3) = -k2 * (1.0D0+nu)
    a(2,4) = k2 * (1.0D0-nu)/(rad(2)**2)
    a(3,3) = k2 * (1.0D0+nu)
    a(3,4) = -k2 * (1.0D0-nu)/(rad(3)**2)
    a(4,1) = rad(2)
    a(4,2) = 1.0D0/rad(2)
    a(4,3) = -rad(2)
    a(4,4) = -1.0D0/rad(2)

    !  RHS vector B
    !  alpha, beta only non-zero where current density is non-zero

    alpha = 0.5D0*rmu0 * j(2)*j(2) * (1.0D0 - nu*nu)/ey(2)
    beta = -alpha * rad(2)*rad(2)

    b(:) = 0.0D0
    b(2) = -k2 * ( 0.125D0*alpha*(3.0D0+nu)*rad(2)*rad(2) &
    + 0.5D0*beta*(1.0D0 + (1.0D0+nu)*log(rad(2))) )
    b(3) = k2 * ( 0.125D0*alpha*(3.0D0+nu)*rad(3)*rad(3)  &
    + 0.5D0*beta*(1.0D0 + (1.0D0+nu)*log(rad(3))) )
    b(4) = -0.125D0*alpha*(rad(2))**3 - 0.5D0*beta*rad(2)*log(rad(2))

    !  Find solution vector c:  A times c = b
    !  N.B. In Morris, Section IV, C_xy is C_x in region y
    !  Thus, array elements c(i) are as follows:
    !  c(1) = C_31 = C_3 in case
    !  c(2) = C_41 = C_4 in case
    !  c(3) = C_32 = C_3 in winding pack
    !  c(4) = C_32 = C_4 in winding pack

    c(:) = 0.0D0
    call linesolv(a, 4, b, c)

    !  Multiply c by (-1) (John Last, internal CCFE memorandum, 21/05/2013)

    c(:) = -1.0D0*c(:)

    !  Calculate stresses in each region

    sigr(:) = 0.0D0
    sigt(:) = 0.0D0

    !  Case; alpha = beta = 0 in this region

    sigr(1) = k1 * ( (1.0D0+nu)*c(1) - (1.0D0-nu)*c(2)/(rad(1)*rad(1)) )
    sigt(1) = k1 * ( (1.0D0+nu)*c(1) + (1.0D0-nu)*c(2)/(rad(1)*rad(1)) )

    !  Winding pack

    sigr(2) = k2 * ( (1.0D0+nu)*c(3) - ((1.0D0-nu)*c(4))/(rad(2)*rad(2)) &
    + 0.125D0*(3.0D0 + nu)*alpha*rad(2)*rad(2) &
    + 0.5D0*beta*(1.0D0 + (1.0D0+nu)*log(rad(2))) )

    sigt(2) = k2 * ( (1.0D0+nu)*c(3) + (1.0D0-nu)*c(4)/(rad(2)*rad(2)) &
    + 0.125D0*(1.0D0+3.0D0*nu)*alpha*rad(2)*rad(2) &
    + 0.5D0*beta*(nu + (1.0D0+nu)*log(rad(2))) )

    !  Deflection at inside edge of TF coil (m)

    deflect = c(1)*rad(1) + c(2)/rad(1)

end subroutine two_layer_stress

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function eyngeff(estl,eins,tins,tstl,tcs)

    !+ad_name  eyngeff
    !+ad_summ  Finds the effective Young's modulus of the TF coil winding pack
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  tstl : input real : thickness of steel conduit (m)
    !+ad_args  tcs  : input real : dimension of cable space area inside conduit (m)
    !+ad_desc  This routine calculates the effective Young's modulus (Pa)
    !+ad_desc  of the TF coil in the winding pack section.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/91 JG  Initial version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  30/04/14 PJK/JM Modifications for two-layer stress model
    !+ad_hist  16/09/14 PJK Removed model switch and old (Galambos/Myall) calculation
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: eyngeff

    !  Arguments

    real(kind(1.0D0)), intent(in) :: estl,eins,tins,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ed,ttot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total thickness of a turn
    ttot = tcs + 2.0D0*(tins + tstl)

    !  See Figure 8 and Section III.4, Morris

    ed = ttot / (2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)

    eyngeff = 1.0D0/ttot * 2.0D0*tstl*ed

end function eyngeff

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function edoeeff(estl,eins,tins,tstl,tcs)

    !+ad_name  edoeeff
    !+ad_summ  Returns ratio of E_d to E_eff in Morris
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  tstl : input real : thickness of steel conduit (m)
    !+ad_args  tcs  : input real : dimension of cable space area inside conduit (m)
    !+ad_desc  This routine calculates the ratio of E_d to the effective Young's
    !+ad_desc  modulus, given in Morris, Section III.4. This is used to calculate
    !+ad_desc  the strain in the insulator.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  12/05/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: edoeeff

    !  Arguments

    real(kind(1.0D0)), intent(in) :: estl,eins,tins,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ed,ttot,eeff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total thickness of a turn
    ttot = tcs + 2.0D0*(tins + tstl)

    !  Code copied from eyngeff routine
    !  See Figure 8 and Section III.4, Morris

    ed = ttot / (2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)

    eeff = 1.0D0/ttot * 2.0D0*tstl*ed

    edoeeff = ed/eeff

end function edoeeff

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function eyngzwp(estl,eins,ewp,tins,tstl,tcs)

    !+ad_name  eyngzwp
    !+ad_summ  Finds the vertical Young's modulus of the TF coil winding pack
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  ewp  : input real : Young's modulus of windings (Pa)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  tstl : input real : thickness of steel conduit (m)
    !+ad_args  tcs  : input real : dimension of cable space area inside conduit (m)
    !+ad_desc  This routine calculates the vertical Young's modulus (Pa)
    !+ad_desc  of the TF coil in the winding pack section.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/04/14 PJK/JM Initial version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: eyngzwp

    !  Arguments

    real(kind(1.0D0)), intent(in) :: estl,eins,ewp,tins,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ttot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ttot = tcs + 2.0D0*(tins + tstl)

    eyngzwp = ewp*tcs*tcs &
    + estl*( (tcs + 2.0D0*tstl)**2 - tcs*tcs ) &
    + eins*( (tcs + 2.0D0*(tstl + tins))**2 - (tcs + 2.0D0*tstl)**2 )

    eyngzwp = eyngzwp / (ttot*ttot)

end function eyngzwp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function sigvm(sx,sy,sz,txy,txz,tyz)

    !+ad_name  sigvm
    !+ad_summ  Calculates Von Mises stress in a TF coil
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  B Reimer, FEDC
    !+ad_cont  N/A
    !+ad_args  sx  : input real : in-plane stress in X direction (Pa)
    !+ad_args  sy  : input real : in-plane stress in Y direction (Pa)
    !+ad_args  sz  : input real : in-plane stress in Z direction (Pa)
    !+ad_args  txy : input real : out of plane stress in X-Y plane (Pa)
    !+ad_args  txz : input real : out of plane stress in X-Z plane (Pa)
    !+ad_args  tyz : input real : out of plane stress in Y-Z plane (Pa)
    !+ad_desc  This routine calculates the Von Mises combination of
    !+ad_desc  stresses (Pa) in a TF coil.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/07/88 BR  Original version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: sigvm

    !  Arguments

    real(kind(1.0D0)), intent(in) :: sx,sy,sz,txy,txz,tyz

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    sigvm = sqrt( 0.5D0 * ( (sx-sy)**2 + (sx-sz)**2 + (sz-sy)**2 &
    + 6.0D0*(txy**2 + txz**2 + tyz**2) ) )

end function sigvm

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine sctfjalw(bmaxtf,rtfmi,rtfmo,rtf2,sigmatf,tdump,jtfalw)

    !+ad_name  sctfjalw
    !+ad_summ  Simple J(B) model for the superconducting TF Coil
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  bmaxtf  : input real : peak field including ripple (T)
    !+ad_args  rtfmi   : input real : mean inboard leg radius (m)
    !+ad_args  rtfmo   : input real : mean outboard leg radius (m)
    !+ad_args  rtf2    : input real : radius of inboard leg point nearest plasma (m)
    !+ad_args  sigmatf : input real : allowable structure stress (MPa)
    !+ad_args  tdump   : input real : dump time (s)
    !+ad_args  jtfalw  : output real : overall allowable current density (A/m2)
    !+ad_desc  This routine using a simple model to calculate the allowable
    !+ad_desc  current density in a superconducting coil, given the magnetic
    !+ad_desc  field and the allowable stress.
    !+ad_desc  Programmed by J. Galambos from algorithms from J. Perkins.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  25/01/91 JG  Initial version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: bmaxtf,rtfmi,rtfmo,rtf2,sigmatf,tdump
    real(kind(1.0D0)), intent(out) :: jtfalw

    !  Local variables

    real(kind(1.0D0)), parameter :: tdumprf = 10.0D0  !  Reference dump time (s)

    real(kind(1.0D0)) :: sqrtdmp,temp1,temp2,temp3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    sqrtdmp = sqrt(tdump/tdumprf)
    temp1 = 125.94D0*bmaxtf*rtf2 * log(rtfmo/rtfmi) / sigmatf
    temp2 = 0.036D0*sqrt(bmaxtf) / (1.0D0-bmaxtf/23.0D0)**2
    temp3 = 0.6D0 / (1.0D0 - (1.0D0 / (16.0D0 * (1.0D0 - bmaxtf/23.0D0)-5.0D0) ) )

    jtfalw = 152.0D6 / (temp1 + temp2*temp3 + sqrtdmp)

end subroutine sctfjalw

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine coilshap

    !+ad_name  coilshap
    !+ad_summ  Calculates the TF coil shape
    !+ad_type  Subroutine
    !+ad_desc  Calculates the shape of the INSIDE of the TF coil. The coil is
    !+ad_desc  approximated by a straight inboard section and four elliptical arcs
    !+ad_desc  This is a totally ad hoc model, with no physics or engineering basis.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  19/11/15 MDK Initial version
    !+ad_stat  Okay
    implicit none
    !  Arguments
    !  Local variables
    real(kind(1.0D0)) :: fstraight, a, b
    integer :: i
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xarc(1) = rtfcin + tfcth/2.0d0
    xarc(2) = rmajor - rminor/5.0d0
    xarc(3) = rtot - tfcth/2.0d0
    xarc(4) = xarc(2)
    xarc(5) = xarc(1)
    ! Height of straight section as a fraction of the coil inner height
    fstraight = 0.6d0
    if (snull==0) then
        ! Double null
        yarc(1) = fstraight * hmax
        yarc(2) = hmax
        yarc(3) = 0
        yarc(4) = -hmax
        yarc(5) = -fstraight * hmax
    else
        ! Single null
        yarc(1) = fstraight * (hpfu - tfcth)
        yarc(2) = hpfu - tfcth
        yarc(3) = 0
        yarc(4) = -hmax
        yarc(5) = -fstraight * hmax
    end if

    ! Horizontal and vertical radii of inside edge of TF coil
    ! Arcs are numbered clockwise:
    ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard
    ! 'tfleng' is the length of the coil midline.
    tfleng = yarc(1) - yarc(5)
    do i = 1, 4
        tfa(i) = abs(xarc(i+1) - xarc(i))
        tfb(i) = abs(yarc(i+1) - yarc(i))
        ! Radii and length of midline of coil segments
        a = tfa(i) + tfcth/2.0d0
        b = tfb(i) + tfcth/2.0d0
        tfleng = tfleng + 0.25d0 * circumference(a,b)
    end do

contains
    function circumference(a,b)
        real(kind(1.0D0)) :: circumference, a, b
        !  Calculate ellipse circumference using Ramanujan approximation (m)
        circumference = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )
    end function

end subroutine coilshap

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfcind(tfthk)

    !+ad_name  tfcind
    !+ad_summ  Calculates the self inductance of a TF coil
    !+ad_type  Subroutine
    !+ad_cont  N/A
    !+ad_args  tfthk        : input real : TF coil thickness (m)
    !+ad_desc  This routine calculates the self inductance of a TF coil
    !+ad_desc  approximated by a straight inboard section and two elliptical arcs.
    !+ad_desc  The inductance of the TFC (considered as a single axisymmetric turn)
    !+ad_desc  is calculated by numerical integration over the cross-sectional area.
    !+ad_desc  The contribution from the cross-sectional area of the
    !+ad_desc  coil itself is calculated by taking the field as B(r)/2.
    !+ad_desc  The field in the bore is calculated for unit current.
    !+ad_desc  Top/bottom symmetry is assumed.
    !+ad_call  None
    !+ad_hist  19/11/15 MDK Initial version
    !+ad_stat  Okay
    implicit none
    !  Arguments
    real(kind(1.0D0)), intent(in) :: tfthk

    !  Local variables
    integer, parameter :: nintervals = 100
    integer :: i
    real(kind(1.0D0)) :: ai, ao, bi, bo, x0, y0, h_bore, h_thick, dr, r, b

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise inductance
    tfind = 0.0D0
    ! Integrate over the whole TF area, including the coil thickness.
    x0 = xarc(2)
    y0 = yarc(2)

    ! Minor and major radii of the inside and outside perimeters of the the
    ! Inboard leg and arc.
    ! Average the upper and lower halves, which are different in the
    ! single null case
    ai = xarc(2) - xarc(1)
    bi = (yarc(2)-yarc(4))/2.0d0 - yarc(1)
    ao = ai + tfthk
    bo = bi + tfthk
    ! Interval used for integration
    dr = ao / dble(nintervals)
    ! Start both integrals from the centre-point where the arcs join.
    ! Initialise major radius
    r = x0 - dr/2.0d0
    do i = 1,nintervals
        ! Field in the bore for unit current
        b = rmu0/(2.0D0*pi*r)
        ! Find out if there is a bore
        if (x0-r < ai) then
            h_bore = y0 + bi * sqrt(1 - ((r-x0)/ai)**2)
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) - h_bore
        else
            h_bore = 0.0d0
            ! Include the contribution from the straight section
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) + yarc(1)
        end if
        ! Assume B in TF coil = 1/2  B in bore
        ! Multiply by 2 for upper and lower halves of coil
        tfind = tfind + b*dr*(2.0D0*h_bore + h_thick)
        r = r - dr
    end do

    ! Outboard arc
    ai = xarc(3) - xarc(2)
    bi = (yarc(2) - yarc(4))/2.0d0
    ao = ai + tfthk
    bo = bi + tfthk
    dr = ao / dble(nintervals)
    ! Initialise major radius
    r = x0 + dr/2.0d0
    do i = 1,nintervals
        ! Field in the bore for unit current
        b = rmu0/(2.0D0*pi*r)
        ! Find out if there is a bore
        if (r-x0 < ai) then
            h_bore = y0 + bi * sqrt(1 - ((r-x0)/ai)**2)
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) - h_bore
        else
            h_bore = 0.0d0
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2)
        end if
        ! Assume B in TF coil = 1/2  B in bore
        ! Multiply by 2 for upper and lower halves of coil
        tfind = tfind + b*dr*(2.0D0*h_bore + h_thick)
        r=r+dr
    end do

end subroutine tfcind


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine outtf(outfile, peaktfflag)

    !+ad_name  outtf
    !+ad_summ  Writes superconducting TF coil output to file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  peaktfflag : input integer : warning flag from peak TF calculation
    !+ad_desc  This routine writes the superconducting TF coil results
    !+ad_desc  to the output file.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  report_error
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  11/04/13 PJK Clarified some output labels
    !+ad_hist  07/11/13 PJK Removed obsolete switch magnt; modified layout
    !+ad_hist  02/04/14 PJK Added TF coil geometry to mfile
    !+ad_hist  03/04/14 PJK Added superconductor type to mfile
    !+ad_hist  30/04/14 PJK Added output for two-layer stress model
    !+ad_hist  08/05/14 PJK Replaced stress_model with tfc_model
    !+ad_hist  08/05/14 PJK Changed ripmax description
    !+ad_hist  16/06/14 PJK Removed duplicate outputs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  30/07/14 PJK Renamed borev to tfborev; changed estotf output
    !+ad_hist  31/07/14 PJK Added acasetfo and several masses
    !+ad_hist  02/09/14 PJK Added peaktfflag usage
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, peaktfflag

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: ap, radius
    character(len=1) :: intstring

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'TF Coils')
    call ocmmnt(outfile,'Superconducting TF coils')

    call ovarin(outfile,'TF coil superconductor material','(isumattf)',isumattf)

    select case (isumattf)
    case (1)
        call ocmmnt(outfile,'  (ITER Nb3Sn critical surface model)')
    case (2)
        call ocmmnt(outfile,'  (Bi-2212 high temperature superconductor)')
    case (3)
        call ocmmnt(outfile,'  (NbTi)')
    case (4)
        call ocmmnt(outfile, &
        '  (ITER Nb3Sn critical surface model, user-defined parameters)')
    case (5)
        call ocmmnt(outfile, ' (WST Nb3Sn critical surface model)')
    end select

    call osubhd(outfile,'Wedged TF Coils, with two-step winding')

    call ocmmnt(outfile,'Current Density :')
    call oblnkl(outfile)
    call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf, 'OP ')
    call ovarre(outfile,'Overall current density (A/m2)','(oacdcp)',oacdcp)

    if (tfc_model == 0) then
        call ovarre(outfile,'Allowable overall current density (A/m2)', '(jwdgcrt)',jwdgcrt, 'OP ')
    end if

    call osubhd(outfile,'General Coil Parameters :')
    call ovarre(outfile,'Number of TF coils','(tfno)',tfno)
    call ovarre(outfile,'Cross-sectional area per coil (m2)','(tfarea/tfno)', tfareain/tfno, 'OP ')
    call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth)',tfcth)
    call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko)',tfthko)
    call ovarre(outfile,'Inboard leg outboard half-width (m)','(tficrn)',tficrn, 'OP ')
    call ovarre(outfile,'Inboard leg inboard half-width (m)','(tfocrn)',tfocrn, 'OP ')
    call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort, 'OP ')
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng, 'OP ')
    call ovarre(outfile,'Total current in all TF coils (MA)','(ritfc/1.D6)',1.0D-6*ritfc, 'OP ')
    call ovarre(outfile,'Peak field (Amperes Law,T)','(bmaxtf)',bmaxtf, 'OP ')
    call ovarre(outfile,'Peak field (with ripple,T)','(bmaxtfrp)',bmaxtfrp, 'OP ')
    call ovarre(outfile,'Max allowed ripple amplitude at plasma (%)','(ripmax)',ripmax)
    call ovarre(outfile,'Ripple amplitude at plasma (%)','(ripple)',ripple, 'OP ')
    call ovarre(outfile,'Total stored energy in TF coils (GJ)','(estotftgj)',estotftgj, 'OP ')
    call ovarre(outfile,'Total mass of TF coils (kg)','(whttf)',whttf, 'OP ')
    call ovarre(outfile,'Mass of each TF coil (kg)','(whttf/tfno)',whttf/tfno, 'OP ')
    call ovarre(outfile,'Vertical separating force per leg (N)','(vforce)',vforce, 'OP ')
    call ovarre(outfile,'Centering force per coil (N/m)','(cforce)',cforce, 'OP ')

    !  Report any applicability issues with peak field with ripple calculation

    if (peaktfflag == 1) then
        call report_error(144)
    else if (peaktfflag == 2) then
        call report_error(145)
    else
        continue
    end if

    call osubhd(outfile,'Coil Geometry :')
    call ovarre(outfile,'Inboard leg centre radius (m)','(rtfcin)',rtfcin, 'OP ')
    call ovarre(outfile,'Outboard leg centre radius (m)','(rtot)',rtot, 'OP ')
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax, 'OP ')
    call ovarre(outfile,'gap between inboard vacuum vessel and thermal shield (m)','(gapds)',gapds)

    call oblnkl(outfile)
    call ocmmnt(outfile,'TF coil inner surface shape is approximated')
    call ocmmnt(outfile,'by a straight segment and elliptical arcs between the following points :')
    call oblnkl(outfile)

    write(outfile,10)
    10  format(t2,'point',t16,'x(m)',t31,'y(m)')

    do i = 1,5
        write(outfile,20) i,xarc(i),yarc(i)
        intstring = int2char(i)
        call ovarre(mfile,'TF coil arc point '//intstring//' R (m)', '(xarc('//intstring//'))',xarc(i))
        call ovarre(mfile,'TF coil arc point '//intstring//' Z (m)', '(yarc('//intstring//'))',yarc(i))
    end do
    20  format(i4,t10,f10.3,t25,f10.3)

    call osubhd(outfile,'Quench information :')
    call ovarre(outfile,'Allowable stress in vacuum vessel (Pa)','(sigvvall)',sigvvall)
    call ovarre(outfile,'Minimum allowed quench time due to stress in VV (s)','(taucq)',taucq, 'OP ')
    call ovarre(outfile,'Actual quench time (or time constant) (s)','(tdmptf)',tdmptf)
    ! call ovarre(outfile,'Max allowed current density in winding pack due to temperature rise in quench (A/m2)', &
    !                     '(jwdgpro)', jwdgpro, 'OP ')
    call ovarre(outfile,'Actual current density in winding pack (A/m2)', '(jwptf)', jwptf, 'OP ')
    call ovarre(outfile,'Maximum allowed voltage during quench due to insulation (kV)', '(vdalw)', vdalw)
    call ovarre(outfile,'Quench voltage (kV)','(vtfskv)',vtfskv, 'OP ')

    select case (isumattf)
    case (1,2,3,4,5)
        call ovarre(outfile,'Maximum allowed temp rise during a quench (K)','(tmaxpro)', tmaxpro)
    case(6)
        call ovarre(outfile,'CroCo cable : Maximum permitted temperature in quench (K)',&
                            '(tmax_croco)', tmax_croco)
        call ovarre(outfile,'CroCo cable : Actual temp reached during a quench (K)', &
                            '(croco_quench_temperature)', croco_quench_temperature)
        !call ovarre(outfile,'Maximum temperature in quench: Jacket (K)','(tmax_jacket)', tmax_jacket)
        ! if(jwdgpro_1<jwdgpro_2)then
        !     call ocmmnt(outfile,'Strands are limiting.')
        ! else if(jwdgpro_2<jwdgpro_1)then
        !     call ocmmnt(outfile,'conductor jacket is limiting.')
        ! end if

        call ocmmnt(outfile,'Fast discharge current model: '//quench_model)
        if(quench_detection_ef>1d-10)then
            call ocmmnt(outfile,'Two-phase quench model is used')
            call ovarre(outfile,'Electric field at which TF quench is detected, discharge begins (V/m)',&
                                '(quench_detection_ef)', quench_detection_ef)
            call ovarre(outfile,'Peak temperature before quench is detected (K)','(T1)',T1,'OP ')
        else
            call ocmmnt(outfile, 'Simple one-phase quench model is used')
        endif
    end select


    call osubhd(outfile,'Conductor Information :')
    call ovarre(outfile,'Width of conductor (square) (m)','(conductor_width)',conductor_width, 'OP ')
    call ovarre(outfile,'Width of turn including inter-turn insulation (m)','(leno)',leno, 'OP ')
    call ovarre(outfile,'Width of space inside conductor (m)','(leni)',leni, 'OP ')
    call ovarre(outfile,'Conduit thickness (m)','(thwcndut)',thwcndut)
    call ovarre(outfile,'Inter-turn insulation thickness (m)','(thicndut)',thicndut)

    call ovarre(outfile,'Superconductor mass per coil (kg)','(whtconsc)',whtconsc, 'OP ')
    call ovarre(outfile,'Copper mass per coil (kg)','(whtconcu)',whtconcu, 'OP ')
    call ovarre(outfile,'Steel conduit mass per coil (kg)','(whtconsh)',whtconsh, 'OP ')
    call ovarre(outfile,'Conduit insulation mass per coil (kg)','(whtconin)',whtconin, 'OP ')
    call ovarre(outfile,'Total conductor mass per coil (kg)','(whtcon)',whtcon, 'OP ')
    call ovarre(outfile,'Cable conductor + void area (m2)','(acstf)',acstf, 'OP ')

    select case (isumattf)
    case (1,2,3,4,5)
        call osubhd(outfile,'Winding Pack Information :')
        call ovarre(outfile,'Diameter of central helium channel in cable','(dhecoil)',dhecoil)
        call ocmmnt(outfile,'Fractions by area')
        call ovarre(outfile,'Coolant fraction in conductor excluding central channel','(vftf)',vftf)
        call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcutfsu)
        call ovarre(outfile,'Superconductor fraction of conductor','(1-fcutfsu)',1-fcutfsu)
        ! TODO
        !call ovarre(outfile,'Conductor fraction of winding pack','(acond/ap)',acond/ap, 'OP ')
        !call ovarre(outfile,'Conduit fraction of winding pack','(turnstf*acndttf/ap)',turnstf*acndttf/ap, 'OP ')
        !call ovarre(outfile,'Insulator fraction of winding pack','(aiwp/ap)',aiwp/ap, 'OP ')
        !call ovarre(outfile,'Helium area fraction of winding pack excluding central channel','(avwp/ap)',avwp/ap, 'OP ')
        !call ovarre(outfile,'Central helium channel area as fraction of winding pack','(awphec/ap)',awphec/ap, 'OP ')
        ap = acond + turnstf*acndttf + aiwp + avwp + awphec
        call ovarrf(outfile,'Check total area fractions in winding pack = 1','', &
                            (acond + turnstf*acndttf + aiwp + avwp + awphec)/ap)
    end select

    call ovarre(outfile,'Winding radial thickness (m)','(thkwp)',thkwp, 'OP ')
    call ovarre(outfile,'Winding width 1 (m)','(wwp1)',wwp1, 'OP ')
    call ovarre(outfile,'Winding width 2 (m)','(wwp2)',wwp2, 'OP ')
    call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
    call ovarre(outfile,'Winding pack insertion gap (m)','(tfinsgap)',tfinsgap)
    call ovarre(outfile,'mass of ground-wall insulation layer per coil (kg)','(whtgw)',whtgw, 'OP ')
    call ovarre(outfile,'Number of turns per TF coil','(turnstf)',turnstf, 'OP ')
    call ovarre(outfile,'Current per turn (A)','(cpttf)',cpttf)

    call osubhd(outfile,'External Case Information :')

    call ovarre(outfile,'Inboard leg case outboard thickness (m)','(casthi)',casthi)
    call ovarre(outfile,'Inboard leg case inboard thickness (m)','(thkcas)',thkcas)
    call ovarre(outfile,'Inboard leg case toroidal thickness (m)','(casths)',casths)
    call ovarre(outfile,'Inboard leg case area per coil (m2)','(acasetf)',acasetf, 'OP ')
    call ovarre(outfile,'Outboard leg case area per coil (m2)','(acasetfo)',acasetfo, 'OP ')
    call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas, 'OP ')

    call osubhd(outfile,'Radial build of TF coil centre-line :')
    write(outfile,5)
5   format(t43,'Thickness (m)',t60,'Outer radius (m)')
    radius = bore + ohcth + precomp + gapoh
    call obuild(outfile,'Innermost edge of TF coil',radius,radius)
    radius = radius + thkcas
    call obuild(outfile,'Coil case ("nose")',thkcas,radius,'(thkcas)')
    radius = radius + tfinsgap
    call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')
    radius = radius + tinstf
    call obuild(outfile,'Winding pack insulation',tinstf,radius,'(tinstf)')
    radius = radius + thkwp/2d0 - tinstf
    call obuild(outfile,'Winding - first half',thkwp/2d0 - tinstf,radius,'(thkwp/2 - tinstf)')
    radius = radius + thkwp/2d0 - tinstf
    call obuild(outfile,'Winding - second half',thkwp/2d0 - tinstf,radius,'(thkwp/2 - tinstf)')
    radius = radius + tinstf
    call obuild(outfile,'Winding pack insulation',tinstf,radius,'(tinstf)')
    radius = radius + tfinsgap
    call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')
    radius = radius + casthi
    call obuild(outfile,'Coil case (plasma side)',casthi,radius,'(casthi)')
    if(abs((radius - rtfcin - 0.5D0*tfcth)) < 1d-6)then
        call ocmmnt(outfile,'TF coil dimensions are consistent')
    else
        call ocmmnt(outfile,'ERROR: TF coil dimensions are NOT consistent:')
        call ovarre(outfile,'Radius of plasma-facing side of inner leg SHOULD BE [m]','',rtfcin + 0.5D0*tfcth)
        call ovarre(outfile,'Inboard TF coil radial thickness [m]','(tfcth)',tfcth)
        thkwp = tfcth - casthi - thkcas - 2.0D0*tinstf - 2.0d0*tfinsgap
        call oblnkl(outfile)
    end if


    if (tfc_model == 0) then
        call osubhd(outfile,'TF Coil Stresses (solid copper coil model) :')
    else
        call osubhd(outfile,'TF Coil Stresses (CCFE two-layer model) :')
    end if
    call ovarin(outfile,'TF coil model','(tfc_model)',tfc_model)
    call ovarre(outfile,'Allowable Tresca stress limit (Pa)','(alstrtf)',alstrtf)
    call ovarre(outfile,'Vertical stress (Pa)','(sigvert)',sigvert, 'OP ')
    if (tfc_model == 1) then
        call ovarre(outfile,'Case radial stress (Pa)','(sigrtf(1))',sigrtf(1))
        call ovarre(outfile,'Case tangential stress (Pa)','(sigttf(1))',sigttf(1), 'OP ')
    end if
    call ovarre(outfile,'Conduit radial stress (Pa)','(sigrcon)',sigrcon, 'OP ')
    call ovarre(outfile,'Conduit tangential stress (Pa)','(sigtcon)',sigtcon, 'OP ')
    call ovarre(outfile,'Tresca stress in case (Pa)', '(s_tresca_case)', s_tresca_case, 'OP ')
    call ovarre(outfile,'Tresca stress in conduit (Pa)', '(s_tresca_cond)', s_tresca_cond, 'OP ')
    call ovarre(outfile,'von Mises stress in case (Pa)', '(s_vmises_case)', s_vmises_case, 'OP ')
    call ovarre(outfile,'von Mises stress in conduit (Pa)', '(s_vmises_cond)', s_vmises_cond, 'OP ')
    call ovarre(outfile,'Deflection at midplane (m)','(deflect)',deflect, 'OP ')
    if (tfc_model == 1) then
        call ovarre(outfile,"Winding pack vertical Young's Modulus (Pa)",'(eyzwp)', eyzwp, 'OP ')
        call ovarre(outfile,'Vertical strain on winding pack','(windstrain)', windstrain, 'OP ')
        call ovarre(outfile,'Radial strain on insulator','(insstrain)', insstrain, 'OP ')
    end if

end subroutine outtf

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfspcall(outfile,iprint)

    !+ad_name  tfspcall
    !+ad_summ  Routine to call the superconductor module for the TF coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  supercon
    !+ad_cont  protect
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_args  iprint : input integer : Switch to write output to file (1=yes)
    !+ad_desc  This routine calls the TF coil superconductor module.
    !+ad_prob  None
    !+ad_call  supercon
    !+ad_hist  06/07/99 PJK Added extra arguments to SUPERCON call
    !+ad_hist  26/07/11 PJK Added JCRIT_MODEL argument to SUPERCON call
    !+ad_hist  21/09/11 PJK Initial F90 version
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  29/10/12 PJK Moved routine and contents into sctfcoil.f90
    !+ad_hist  16/04/13 PJK Removed jcritsc from supercon argument list
    !+ad_hist  08/10/13 PJK Added bi2212; removed obsolete ifail usage
    !+ad_hist  08/05/14 PJK Replaced itfmod with tfc_model
    !+ad_stat  Okay
    implicit none

    integer, intent(in) :: outfile, iprint

    !  Local variables
    real(kind(1.0D0)) :: aturn, tfes, vdump

    !  Simple model

    if (tfc_model == 0) then
        vtfskv = 20.0D0
        return
    end if

    !  Stored energy (J) per coil and cross-sectional area per turn
    tfes = estotft / tfno
    aturn = ritfc/(jwptf*tfno*turnstf)

    call supercon(acstf,aturn,bmaxtfrp,vftf,fcutfsu,cpttf,jwptf,isumattf, &
    fhts,strncon_tf,tdmptf,tfes,tftmp,tmaxpro,bcritsc,tcritsc,iprint, &
    outfile,jwdgcrt,vdump,tmargtf)

    !  TFC Quench voltage in kV
    vtfskv = vdump/1.0D3

contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supercon(acs,aturn,bmax,fhe,fcu,iop,jwp,isumat,fhts, &
        strain,tdmptf,tfes,thelium,tmax,bcritsc,tcritsc,iprint,outfile, &
        jwdgcrt,vd,tmarg)

        !+ad_name  supercon
        !+ad_summ  Routine to calculate the TF superconducting conductor  properties
        !+ad_type  Subroutine
        !+ad_auth  P J Knight, CCFE, Culham Science Centre
        !+ad_auth  J Galambos, ORNL
        !+ad_auth  R Kemp, CCFE, Culham Science Centre
        !+ad_auth  M Kovari, CCFE, Culham Science Centre
        !+ad_auth  J Miller, ORNL
        !+ad_cont  N/A
        !+ad_args  acs : input real : Cable space - inside area (m2)
        !+ad_args  aturn : input real : Area per turn (i.e. entire jacketed conductor) (m2)
        !+ad_args  bmax : input real : Peak field at conductor (T)
        !+ad_args  fhe : input real : Fraction of cable space that is for He cooling
        !+ad_args  fcu : input real : Fraction of conductor that is copper
        !+ad_args  iop : input real : Operating current per turn (A)
        !+ad_args  jwp : input real : Actual winding pack current density (A/m2)
        !+ad_args  isumat : input integer : Switch for conductor type:
        !+ad_argc                           1 = ITER Nb3Sn, standard parameters,
        !+ad_argc                           2 = Bi-2212 High Temperature Superconductor,
        !+ad_argc                           3 = NbTi,
        !+ad_argc                           4 = ITER Nb3Sn, user-defined parameters
        !+ad_argc                           5 = WST Nb3Sn parameterisation
        !+ad_args  fhts    : input real : Adjustment factor (<= 1) to account for strain,
        !+ad_argc                         radiation damage, fatigue or AC losses
        !+ad_args  strain : input real : Strain on superconductor at operation conditions
        !+ad_args  tdmptf : input real : Dump time (sec)
        !+ad_args  tfes : input real : Energy stored in one TF coil (J)
        !+ad_args  thelium : input real : He temperature at peak field point (K)
        !+ad_args  tmax : input real : Max conductor temperature during quench (K)
        !+ad_args  bcritsc : input real : Critical field at zero temperature and strain (T) (isumat=4 only)
        !+ad_args  tcritsc : input real : Critical temperature at zero field and strain (K) (isumat=4 only)
        !+ad_args  iprint : input integer : Switch for printing (1 = yes, 0 = no)
        !+ad_args  outfile : input integer : Fortran output unit identifier
        !+ad_args  jwdgpro : output real : Winding pack current density from temperature
        !+ad_argc                          rise protection (A/m2)
        !+ad_args  jwdgcrt : output real : Critical winding pack current density (A/m2)
        !+ad_args  vd : output real : Discharge voltage imposed on a TF coil (V)
        !+ad_args  tmarg : output real : Temperature margin (K)
        !+ad_desc  This routine calculates the superconductor properties for the TF coils.
        !+ad_desc  It was originally programmed by J. Galambos 1991, from algorithms provided
        !+ad_desc  by J. Miller.
        !+ad_desc  <P>The routine calculates the critical current density (winding pack)
        !+ad_desc  and also the protection information (for a quench).
        !+ad_prob  None
        !+ad_call  bi2212
        !+ad_call  itersc
        !+ad_call  jcrit_nbti
        !+ad_call  oblnkl
        !+ad_call  ocmmnt
        !+ad_call  oheadr
        !+ad_call  osubhd
        !+ad_call  ovarre
        !+ad_call  protect
        !+ad_call  report_error
        !+ad_hist  06/07/99 PJK Added new generic superconductor options
        !+ad_hist  26/07/11 PJK Corrected denominator in JC calculation;
        !+ad_hisc               Added option to use new Jcrit model for binary Nb3Sn
        !+ad_hist  21/09/11 PJK Initial F90 version; converted to subroutine from function
        !+ad_hist  26/09/11 PJK Converted itersc to a subroutine
        !+ad_hist  09/10/12 PJK Modified to use new process_output module
        !+ad_hist  16/04/13 PJK Removed jcrit_model, jcritsc arguments;
        !+ad_hisc               redefined isumat usage; modified itersc arguments
        !+ad_hist  07/10/13 PJK Added Bi-2212 option; removed ifail
        !+ad_hist  16/06/14 PJK Removed duplicate outputs
        !+ad_hist  19/06/14 PJK Removed sect?? flags
        !+ad_hist  26/06/14 PJK Added error handling
        !+ad_hist  13/10/14 PJK Improved temperature margin calculation;
        !+ad_hisc               added jcrit,bcrit,tcrit outputs to file
        !+ad_hist  16/10/14 PJK Clarified jcrit outputs; added early exit from
        !+ad_hisc               tmargin loop if problems are occurring
        !+ad_hist  06/11/14 PJK Added local variable jcritstr; inverted
        !+ad_hisc               areas in bi2212 jstrand input
        !+ad_hist  11/11/14 PJK Shifted exit criteria for temperature margin
        !+ad_hisc               iteration to reduce calculations
        !+ad_hist  03/08/15 MDK Rename argument tdump = tdmptf as this is a global variable
        !+ad_stat  Okay
        !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

        integer, intent(in) :: isumat, iprint, outfile
        real(kind(1.0D0)), intent(in) :: acs, aturn, bmax, fcu, fhe, fhts
        real(kind(1.0D0)), intent(in) :: iop, jwp, strain, tdmptf, tfes, thelium, tmax, bcritsc, tcritsc
        real(kind(1.0D0)), intent(out) :: jwdgcrt, vd, tmarg

        !  Local variables

        integer :: lap
        real(kind(1.0D0)) :: b,bc20m,bcrit,c0,delt,fcond,icrit,iooic, &
        jcritsc,jcrit0,jcritm,jcritp,jcritstr,jsc,jstrand,jtol,jwdgop, &
        t,tc0m,tcrit,ttest,ttestm,ttestp, tdump, fhetot, total
        real(kind(1.0D0)) :: current_sharing_t

        logical:: validity

        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Rename tdmptf as it is called tdump in this routine and those called from here.
        tdump = tdmptf

        ! Helium channel (not present in CroCo conductor.)
        fhetot = fhe + (pi/4.0d0)*dhecoil*dhecoil/acs
        !  Conductor fraction (including central helium channel)
        fcond = 1.0D0 - fhetot

        !  Find critical current density in superconducting strand, jcritstr
        select case (isumat)

        case (1)  !  ITER Nb3Sn critical surface parameterization
            bc20m = 32.97D0
            tc0m = 16.06D0

            !  jcritsc returned by itersc is the critical current density in the
            !  superconductor - not the whole strand, which contains copper
            call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (2)  !  Bi-2212 high temperature superconductor parameterization

            !  Current density in a strand of Bi-2212 conductor
            !  N.B. jcrit returned by bi2212 is the critical current density
            !  in the strand, not just the superconducting portion.
            !  The parameterization for jcritstr assumes a particular strand
            !  composition that does not require a user-defined copper fraction,
            !  so this is irrelevant in this model
            jstrand = jwp * aturn / (acs*fcond)

            call bi2212(bmax,jstrand,thelium,fhts,jcritstr,tmarg)
            jcritsc = jcritstr / (1.0D0-fcu)
            tcrit = thelium + tmarg
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (3)  !  NbTi data
            bc20m = 15.0D0
            tc0m = 9.3D0
            c0 = 1.0D10
            call jcrit_nbti(thelium,bmax,c0,bc20m,tc0m,jcritsc,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (4)  !  ITER Nb3Sn parameterization, but user-defined parameters
            bc20m = bcritsc
            tc0m = tcritsc
            call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (5) ! WST Nb3Sn parameterisation
            bc20m = 32.97D0
            tc0m = 16.06D0
            !  jcritsc returned by itersc is the critical current density in the
            !  superconductor - not the whole strand, which contains copper
            call wstsc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (6) ! "REBCO" 2nd generation HTS superconductor in CrCo strand
            call jcrit_rebco(thelium,bmax,jcritsc,validity,iprint)
            ! acs : Cable space - inside area (m2)
            conductor%acs =  acstf
            conductor%area =  conductor_width**2
            conductor%jacket_fraction = acndttf/conductor%area

            call croco(jcritsc,croco_strand,conductor)
            icrit = conductor%critical_current
            jcritstr = croco_strand%critical_current / croco_strand%area

        case default  !  Error condition
            idiags(1) = isumat ; call report_error(105)

        end select

        ! Critical current density in winding pack
        ! aturn : Area per turn (i.e. entire jacketed conductor with insulation) (m2)
        jwdgcrt = icrit / aturn

        !  Ratio of operating / critical current
        iooic = iop / icrit

        !  Operating current density
        jwdgop = iop / aturn

        !  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
        !  when we have found the desired value of tmarg
        jsc = iooic * jcritsc

        if(iooic<=0d0) then
            write(*,*) 'ERROR'
            write(*,*) 'Negative Iop/Icrit for TF coil'
            write(*,*) 'jsc', jsc
            write(*,*) 'iooic', iooic
            write(*,*) 'jcritsc', jcritsc
            write(*,*) 'Check conductor dimensions. fcond likely gone negative. fcond =', fcond
        end if

        !  Temperature margin (already calculated in bi2212 for isumat=2)
        if ((isumat == 1).or.(isumat == 4).or.(isumat == 3).or.(isumat == 5)) then

            !  Newton-Raphson method; start approx at requested minimum temperature margin
            ttest = thelium + tmargmin_tf + 0.001d0
            delt = 0.01D0
            jtol = 1.0D4

            lap = 0
            solve_for_tmarg: do ; lap = lap+1
                if ((ttest <= 0.0D0).or.(lap > 100)) then
                    idiags(1) = lap ; fdiags(1) = ttest ; call report_error(157)
                    exit solve_for_tmarg
                end if
                ! Calculate derivative numerically
                ttestm = ttest - delt
                ttestp = ttest + delt

                select case (isumat)
                    ! Issue #483 to be on the safe side, check the fractional as well as the absolute error
                case (1,4)
                    call itersc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call itersc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
                    call itersc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
                case (3)
                    call jcrit_nbti(ttest ,bmax,c0,bc20m,tc0m,jcrit0,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call jcrit_nbti(ttestm,bmax,c0,bc20m,tc0m,jcritm,t)
                    call jcrit_nbti(ttestp,bmax,c0,bc20m,tc0m,jcritp,t)
                case (5)
                    call wstsc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call wstsc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
                    call wstsc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
                ! case (6)
                !     call jcrit_rebco(ttest,bmax,jcrit0,validity,iprint)
                !     if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                !     call jcrit_rebco(ttestm,bmax,jcritm,validity,iprint)
                !     call jcrit_rebco(ttestp,bmax,jcritp,validity,iprint)
                end select
                ttest = ttest - 2.0D0*delt*(jcrit0-jsc)/(jcritp-jcritm)
            end do solve_for_tmarg
            tmarg = ttest - thelium
            temp_margin = tmarg

        end if

        ! Temperature margin: An alternative method using secant solver
        if (isumat == 6) then
            call current_sharing_rebco(current_sharing_t, bmax, jsc)
            tmarg = current_sharing_t - thelium
            temp_margin = tmarg
        end if

        !  Find the current density limited by the protection limit
        !  (N.B. Unclear of this routine's relevance for Bi-2212 (isumat=2), due
        !  to presence of fcu argument, which is not used for this model above)

        select case (isumat)
        case (1,2,3,4,5)
            call protect(iop,tfes,acs,aturn,tdump,fcond,fcu,thelium,tmax,jwdgpro,vd)
        ! For the Croco HTS (REBCO) case, the Tmax is calculated and constrained.
        case(6)
            !conductor%tmax = tmax_jacket
            call croco_quench(conductor)
            vd = vtfskv
        end select

        if (iprint == 0) return       ! Output --------------------------

        if (ttest <= 0.0D0) then
            write(*,*)'ERROR: Negative TFC temperature margin'
            write(*,*)'ttest  ', ttest, 'bmax   ', bmax
            write(*,*)'jcrit0 ', jcrit0,'jsc    ', jsc
            write(*,*)'ttestp ', ttestp,'ttestm ', ttestm
            write(*,*)'jcritp ', jcritp,'jcritm ', jcritm
        endif

        call oheadr(outfile,'Superconducting TF Coils')
        call ovarin(outfile,'Superconductor switch', '(isumat)',isumat)

        select case (isumat)
        case (1)
            call ocmmnt(outfile,'Superconductor used: Nb3Sn')
            call ocmmnt(outfile,'  (ITER Jcrit model, standard parameters)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (2)
            call ocmmnt(outfile,'Superconductor used: Bi-2212 HTS')
        case (3)
            call ocmmnt(outfile,'Superconductor used: NbTi')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (4)
            call ocmmnt(outfile,'Superconductor used: Nb3Sn')
            call ocmmnt(outfile,'  (ITER Jcrit model, user-defined parameters)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (5)
            call ocmmnt(outfile,'Superconductor used: Nb3Sn')
            call ocmmnt(outfile, ' (WST Nb3Sn critical surface model)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (6)
            call ocmmnt(outfile,'Superconductor used: REBCO HTS tape in CroCo strand')

            call ovarre(outfile,'thickness of REBCO layer in tape (m)','(rebco_thickness)',rebco_thickness)
            call ovarre(outfile,'thickness of copper layer in tape (m)','(copper_thickne)', copper_thickness)
            call ovarre(outfile,'thickness of Hastelloy layer in tape (m) ','(hastelloy_thickness)', hastelloy_thickness)
            call ovarre(outfile,'Area of central copper bar, as a fraction of area inside the jacket ', &
                                 '(copper_bar)', copper_bar)

            call ovarre(outfile,'Mean width of tape (m)','(tape_width)',tape_width)
            call ovarre(outfile,'Outer diameter of CroCo strand (m) ','(croco_od)', croco_od)
            call ovarre(outfile,'Inner diameter of CroCo copper tube (m) ','(croco_id)',croco_id)


            call ovarre(outfile,'Thickness of each HTS coated tape ','(tape_thickness)',tape_thickness , 'OP ')
            call ovarre(outfile,'Thickness of stack of tapes (m) ','(stack_thickness)',stack_thickness , 'OP ')
            call ovarre(outfile,'Number of tapes in strand','(tapes)',tapes , 'OP ')
            call oblnkl(outfile)
            call ovarre(outfile,'Area of REBCO in strand (m2)','(rebco_area)',rebco_area , 'OP ')
            call ovarre(outfile,'Area of copper in strand (m2)','(copper_area)',copper_area , 'OP ')
            call ovarre(outfile,'Area of hastelloy substrate in strand (m2) ','(hastelloy_area)',hastelloy_area , 'OP ')
            call ovarre(outfile,'Area of solder in strand (m2)  ','(solder_area)',solder_area , 'OP ')
            call ovarre(outfile,'Total: area of CroCo strand (m2)  ','(croco_strand%area)',croco_strand%area , 'OP ')
            if(abs(croco_strand%area-(rebco_area+copper_area+hastelloy_area+solder_area))>1d-6)then
                call ocmmnt(outfile, "ERROR: Areas in CroCo strand do not add up")
                write(*,*)'ERROR: Areas in CroCo strand do not add up - see OUT.DAT'
            endif

            call oblnkl(outfile)
            call ocmmnt(outfile,'Cable information')
            call ovarre(outfile,'Area of cable space (m2)','(acstf)',acstf , 'OP ')
            call ovarre(outfile,'Area of central copper bar, as a fraction of cable space', '(copper_bar)', copper_bar)
            call ovarre(outfile,'Area of helium coolant, as a fraction of cable space', &
                                '(cable_helium_fraction)', cable_helium_fraction)
            call ovarrf(outfile,'Number of CroCo strands in the conductor ','(conductor%number_croco)',&
                                                                              conductor%number_croco , 'OP ')

            call oblnkl(outfile)
            call ocmmnt(outfile,'Conductor information')
            call ovarre(outfile,'Width of square conductor (m)','(conductor_width)', conductor_width , 'OP ')
            call ovarre(outfile,'Area of conductor (not incl insulation) (m2)','(area)', conductor%area , 'OP ')
            call ovarre(outfile,'REBCO fraction of conductor','(rebco_fraction)',conductor%rebco_fraction , 'OP ')
            call ovarre(outfile,'Copper fraction of conductor','(copper_fraction)',conductor%copper_fraction , 'OP ')
            call ovarre(outfile,'Hastelloy fraction of conductor','(hastelloy_fraction)',conductor%hastelloy_fraction , 'OP ')
            call ovarre(outfile,'Solder fraction of conductor','(solder_fraction)',conductor%solder_fraction , 'OP ')
            call ovarre(outfile,'Jacket fraction of conductor','(jacket_fraction)',conductor%jacket_fraction , 'OP ')
            call ovarre(outfile,'Helium fraction of conductor','(helium_fraction)',conductor%helium_fraction , 'OP ')
            total = conductor%copper_fraction+conductor%hastelloy_fraction+conductor%solder_fraction+ &
                    conductor%jacket_fraction+conductor%helium_fraction+conductor%rebco_fraction
            if(abs(total-1d0)>1d-6) then
                call ovarrf(outfile, "ERROR: conductor areas do not add up:",'(total)',total , 'OP ')
            else
                call ovarrf(outfile, "conductor area fractions add up:",'(total)',total , 'OP ')
            endif
            call ovarre(outfile,'Critical current of CroCo strand (A)','(croco_strand%critical_current)', &
                                                                         croco_strand%critical_current , 'OP ')
            call ovarre(outfile,'Critical current of cable (A) ','(conductor%critical_current)', &
                                                                   conductor%critical_current , 'OP ')
        end select ! case (isumat)

        if (run_tests==1) then
            call oblnkl(outfile)
            call ocmmnt(outfile, "PROCESS TF Coil peak field fit. Values for t, z and y:")
            call oblnkl(outfile)
            call ovarre(outfile,'Dimensionless winding pack width','(tf_fit_t)', tf_fit_t, 'OP ')
            call ovarre(outfile,'Dimensionless winding pack radial thickness','(tf_fit_z)', tf_fit_z, 'OP ')
            call ovarre(outfile,'Ratio of peak field with ripple to nominal axisymmetric peak field','(tf_fit_y)', tf_fit_y, 'OP ')
        end if

        call oblnkl(outfile)
        !call ovarre(outfile,'Peak field at conductor (T)','(bmax)',bmax, 'OP ')
        call ovarre(outfile,'Helium temperature at peak field (K)','(thelium)',thelium)
        if(isumat/=6)then
            call ovarre(outfile,'Total helium fraction inside cable space','(fhetot)',fhetot, 'OP ')
            call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcu)
            call ovarre(outfile,'Strain on superconductor','(strncon_tf)',strncon_tf)
        end if
        call ovarre(outfile,'Critical current density in superconductor (A/m2)','(jcritsc)',jcritsc, 'OP ')
        call ovarre(outfile,'Critical current density in strand (A/m2)','(jcritstr)',jcritstr, 'OP ')
        call ovarre(outfile,'Operating winding pack J (A/m2)','(jwdgop)',jwdgop, 'OP ')
        call ovarre(outfile,'Critical winding pack current density (A/m2)', '(jwdgcrt)',jwdgcrt, 'OP ')
        call ovarre(outfile,'Critical current (A)','(icrit)',icrit, 'OP ')
        call ovarre(outfile,'Operating current / critical current','(iooic)', iooic, 'OP ')
        call ovarre(outfile,'Temperature margin (K)','(tmarg)',tmarg, 'OP ')
        ! call ovarre(outfile,'Temperature margin using secant solver (K)','(tmarg2)',tmarg2, 'OP ')
        call ovarre(outfile,'Minimum permitted temperature margin (K)','(tmargmin_tf)',tmargmin_tf)

    end subroutine supercon

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine protect(aio,tfes,acs,aturn,tdump,fcond,fcu,tba,tmax,ajwpro,vd)

        !+ad_name  protect
        !+ad_summ  Finds the current density limited by the protection limit
        !+ad_type  Subroutine
        !+ad_auth  P J Knight, CCFE, Culham Science Centre
        !+ad_auth  J Miller, ORNL
        !+ad_cont  N/A
        !+ad_args  aio : input real : Operating current (A)
        !+ad_args  tfes : input real : Energy stored in one TF coil (J)
        !+ad_args  acs : input real : Cable space - inside area (m2)
        !+ad_args  aturn : input real : Area per turn (i.e.  entire cable) (m2)
        !+ad_args  tdump : input real : Dump time (sec)
        !+ad_args  fcond : input real : Fraction of cable space containing conductor
        !+ad_args  fcu : input real : Fraction of conductor that is copper
        !+ad_args  tba : input real : He temperature at peak field point (K)
        !+ad_args  tmax : input real : Max conductor temperature during quench (K)
        !+ad_args  ajwpro : output real :  Winding pack current density from temperature
        !+ad_argc                          rise protection (A/m2)
        !+ad_args  vd : output real :  Discharge voltage imposed on a TF coil (V)
        !+ad_desc  This routine calculates maximum conductor current density which
        !+ad_desc  limits the peak temperature in the winding to a given limit (tmax).
        !+ad_desc  It also finds the dump voltage.
        !+ad_desc  <P>These calculations are based on Miller's formulations.
        !+ad_prob  This routine may be misleading for the Bi-2212 superconductor model,
        !+ad_prob  as fcu is not used elsewhere in modelling this material.
        !+ad_call  None
        !+ad_hist  06/07/99 PJK Initial upgraded version
        !+ad_hist  21/09/11 PJK Initial F90 version
        !+ad_stat  Okay
        !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

        !  Arguments

        real(kind(1.0D0)), intent(in) :: aio, tfes, acs, aturn, tdump, fcond, &
        fcu,tba,tmax
        real(kind(1.0D0)), intent(out) :: ajwpro, vd

        !  Local variables

        integer :: no,np
        real(kind(1.0D0)) :: aa,ai1,ai2,ai3,ajcp,bb,cc,dd,tav
        real(kind(1.0D0)), dimension(11) :: p1, p2, p3

        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !  Integration coefficients p1,p2,p3

        p1(1) = 0.0D0
        p1(2) = 0.8D0
        p1(3) = 1.75D0
        p1(4) = 2.4D0
        p1(5) = 2.7D0
        p1(6) = 2.95D0
        p1(7) = 3.1D0
        p1(8) = 3.2D0
        p1(9) = 3.3D0
        p1(10) = 3.4D0
        p1(11) = 3.5D0

        p2(1) = 0.0D0
        p2(2) = 0.05D0
        p2(3) = 0.5D0
        p2(4) = 1.4D0
        p2(5) = 2.6D0
        p2(6) = 3.7D0
        p2(7) = 4.6D0
        p2(8) = 5.3D0
        p2(9) = 5.95D0
        p2(10) = 6.55D0
        p2(11) = 7.1D0

        p3(1) = 0.0D0
        p3(2) = 0.05D0
        p3(3) = 0.5D0
        p3(4) = 1.4D0
        p3(5) = 2.6D0
        p3(6) = 3.7D0
        p3(7) = 4.6D0
        p3(8) = 5.4D0
        p3(9) = 6.05D0
        p3(10) = 6.8D0
        p3(11) = 7.2D0

        !  Dump voltage

        vd = 2.0D0 * tfes/(tdump*aio)

        !  Current density limited by temperature rise during quench

        tav = 1.0D0 + (tmax-tba)/20.0D0
        no = int(tav)
        np = no+1
        np = min(np,11)

        ai1 = 1.0D16 * ( p1(no)+(p1(np)-p1(no)) * (tav - no) )
        ai2 = 1.0D16 * ( p2(no)+(p2(np)-p2(no)) * (tav - no) )
        ai3 = 1.0D16 * ( p3(no)+(p3(np)-p3(no)) * (tav - no) )

        aa = vd * aio/tfes
        bb = (1.0D0-fcond)*fcond*fcu*ai1
        cc = (fcu*fcond)**2 * ai2
        dd = (1.0D0-fcu)*fcu * fcond**2 * ai3
        ajcp = sqrt( aa* (bb+cc+dd) )
        ajwpro = ajcp*(acs/aturn)

    end subroutine protect

end subroutine tfspcall

! --------------------------------------------------------------------
subroutine croco_quench(conductor)

    !+ad_name  croco_quench
    !+ad_summ  Finds the current density limited by the maximum temperatures in quench
    !+ad_type  Subroutine
    !+ad_desc  It also finds the dump voltage.


    type(volume_fractions), intent(in)::conductor
    real(kind(1.0D0)):: current_density_in_conductor


    real(kind(1.0D0))::tout     !for the phase 2
    real(kind(1.0D0))::relerr= 0.01d0, abserr= 0.01d0

    integer(kind=4), parameter :: neqn = 1
    integer(kind=4) :: iflag
    integer(kind=4) :: iwork(5)

    real(kind(1.0D0)) :: work(100+21*neqn)
    real(kind(1.0D0)) :: y(neqn)

    real(kind(1.0D0))::residual, t
    logical::error

    if(quench_detection_ef>1d-10)then
        ! Two-phase quench model is used.
        ! Phase 1
        ! Issue #548, or see K:\Power Plant Physics and Technology\PROCESS\HTS\
        ! Solve for the temperature at which the quench detection field is reached.
        ! secant_solve(f,x1,x2,solution,error,residual,opt_tol)
        current_density_in_conductor = jwptf *  (leno / conductor_width)**2
        call secant_solve(detection_field_error,5d0, 70d0,T1,error,residual)
        ! T1 = Peak temperature of normal zone before quench is detected

        ! Obsolete but leave here for the moment
        ! croco_quench_factor = conductor%copper_fraction / jwptf**2

        if(T1>tmax_croco)write(*,*)'Phase 1 of quench is too hot: T1 = ',T1
    else
        ! Quench is detected instantly - no phase 1.
        T1 = tftmp
    endif

    ! vtfskv : voltage across a TF coil during quench (kV)
    ! tdmptf /10.0/ : fast discharge time for TF coil in event of quench (s) (time-dump-TF)
    ! For clarity I have copied this into 'time2' or 'tau2' depending on the model.

    if(quench_model=='linear')then
        time2 = tdmptf
        vtfskv = 2.0D0/time2 * (estotft/tfno) / cpttf
    elseif(quench_model=='exponential')then
        tau2 = tdmptf
        vtfskv = 2.0D0/tau2 * (estotft/tfno) / cpttf
    endif

    ! PHASE 2 OF QUENCH: fast discharge into resistor
    ! The field declines in proportion to the current.
    ! The operating current is iop.
    ! The peak field at the operating current is bmaxtfrp
    ! This is declared in global_variable.f90, so is in scope.
    ! Solve the set of differential equations
    ! subroutine ode ( f, neqn, y, t, tout, relerr, abserr, iflag, work, iwork )
    ! See ode.f90 for details.
    !    declare F in an external statement, supply the double precision
    !      SUBROUTINE F ( T, Y, YP )
    y(1) = T1
    tout = 2.0d0 * tau2
    iflag = 1
    ! Starting time
    t = 0d0
    ! Remember that t will be set to the finish time by the ode solver!
    ! ODE SOLVER
    call ode(dtempbydtime, neqn, y, t, tout, relerr, abserr, iflag, work, iwork)
    if(iflag /= 2)write(*,*)'ODE in subroutine croco_quench failed: iflag =', iflag

    croco_quench_temperature = y(1)


contains
    function detection_field_error(t1)
        ! Issue #548.
        ! The difference beteween the actual voltage developed during the first
        ! phase of the quench and the specified detection voltage
        real(kind(1.0D0))::detection_field_error, deltaj,jcritsc

        real(kind(1.0D0)), intent(in) :: t1
        real(kind(1.0D0)):: jc
        logical :: validity
        integer :: iprint

        call copper_properties2(t1,bmaxtf,copper)
        call jcrit_rebco(t1,bmaxtf,jcritsc,validity,iprint)

        ! Critical current density at specified temperature t1, operating maximum field bmaxtf
        jc = jcritsc * conductor%rebco_fraction

        ! By definition jc=0 below the critical temperature at operating field
        ! All the current flows in the copper
        ! Note that the copper  resisitivity is a function of temperature, so it should still
        ! be possible to solve for the correct detection voltage.
        if(jc<0) jc = 0d0

        deltaj = (current_density_in_conductor - jc)
        detection_field_error = deltaj * copper%resistivity / conductor%copper_fraction &
                                  - quench_detection_ef
    end function

end subroutine croco_quench
!-------------------------------------------------------------------
subroutine dtempbydtime ( qtime, qtemperature, derivative )
  !+ad_name  dtempbydtime
  !+ad_summ  Supplies the right hand side of the ODE for the croco quench phase 2 subroutine
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  qtime : input real : time, the independent variable
  !+ad_args  qtemperature : input real : temperature, the dependent variable
  !+ad_args  derivative : output real : the value of dtempbydtime
  !+ad_hist  14/08/17 MDK  Initial version
  !+ad_stat  Okay

  ! Time-dependent quantities during the fast discharge local to this subroutine:

  ! time, the independent variable
  real(kind(1.0D0)),intent(in) :: qtime

  ! Y(), the dependent variable
  real(kind(1.0D0)),intent(in) :: qtemperature(1)

  ! YP(), the value of the derivative
  real(kind(1.0D0)),intent(out) :: derivative(1)

  real(kind(1.0D0))::qj  ! Current density in superconductor during fast discharge
  real(kind(1.0D0))::qcurrent  ! Total current in cable during fast discharge
  real(kind(1.0D0))::qbfield  ! Peak magnetic field in cable during fast discharge
  real(kind(1.0D0))::q_crit_current ! Critical current during fast discharge
  logical :: validity
  real(kind(1.0D0))::qratio,qtemp

  !write(*,*)'subroutine dtempbydtime ( qtime, qtemperature, derivative )'
  !write(*,*)'qtime = ',qtime,' qtemperature = ',qtemperature

  ! For convenience
  qtemp = qtemperature(1)

  ! The current is a known function of time
  if(quench_model=='linear')then
      qcurrent = cpttf * (1 - qtime / time2)
  elseif(quench_model=='exponential')then
      qcurrent = cpttf * exp(- qtime / tau2)
  endif

  ! Field is proportional to current
  qbfield = bmaxtfrp * qcurrent / cpttf

  ! Critical current 'qj' given field and temperature
  call jcrit_rebco(qtemp,qbfield,qj,validity,0)
  q_crit_current = conductor%rebco_area * qj

  ! The jacket is now included in the argument list
  qratio = resistivity_over_heat_capacity(qtemp,qbfield,copper,hastelloy,solder,helium,jacket)

  ! Derivatives

  derivative(1) = (qcurrent - q_crit_current)**2 * qratio / &
                  (conductor%copper_fraction * conductor%area**2)


  !write(*,*)'subroutine dtempbydtime: derivative =',derivative(1)
  return
end subroutine dtempbydtime

!-----------------------------------------------------------------------

function resistivity_over_heat_capacity(qtemp,qbfield,copper,hastelloy,solder,helium,jacket)
    real(kind(1.0D0)),intent(in):: qtemp,qbfield
    ! Only those materials that are actually supplied in the arguments are used.
    type(resistive_material),intent(in),optional::copper,hastelloy,solder,helium,jacket
    real(kind(1.0D0))::sum,resistivity_over_heat_capacity

    sum = 0d0
    call copper_properties2(qtemp,qbfield, copper)
    if(present(copper))then
        sum = sum + conductor%copper_fraction * copper%density * copper%cp
    end if
    if(present(hastelloy))then
        call hastelloy_properties(qtemp,hastelloy)
        sum = sum + conductor%hastelloy_fraction * hastelloy%density * hastelloy%cp
    end if
    if(present(solder))then
        call solder_properties(qtemp,solder)
        sum = sum + conductor%solder_fraction    * solder%density * solder%cp
    end if
    if(present(helium))then
        call helium_properties(qtemp,helium)
        sum = sum + conductor%helium_fraction    * helium%cp_density
    end if
    if(present(jacket))then
        call jacket_properties(qtemp,jacket)
        sum = sum + conductor%jacket_fraction    * jacket%density * jacket%cp
    end if

    resistivity_over_heat_capacity = copper%resistivity / sum

    ! write(*,'(10(1pe10.3), 1x)')qtemp, copper%resistivity, sum,resistivity_over_heat_capacity
    ! write(*,'(10(1pe10.3), 1x)')conductor%copper_fraction    , copper%density ,copper%cp
    ! write(*,'(10(1pe10.3), 1x)')conductor%hastelloy_fraction , hastelloy%density , hastelloy%cp
    ! write(*,'(10(1pe10.3), 1x)')conductor%solder_fraction    , solder%density , solder%cp
    ! write(*,'(10(1pe10.3), 1x)')conductor%helium_fraction    , helium%cp_density

end function resistivity_over_heat_capacity
!--------------------------------------------------------------



end module sctfcoil_module
