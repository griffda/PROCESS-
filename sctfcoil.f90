! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  16/09/14 PJK Removed myall_stress routine
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

  private
  public :: bi2212, itersc, jcrit_nbti, outtf, sctfcoil, stresscl, &
       tfcind, tfspcall

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  18/12/12 PJK/RK Modified vertical bore for single-null cases
    !+ad_hist  06/11/13 PJK Modified coil case mass and leg area calculations
    !+ad_hist  26/02/14 PJK Changed comment in the case of too small tftort
    !+ad_hist  23/04/14 PJK Modified TF coil leg length calculation
    !+ad_hist  28/04/14 PJK/JM Corrected awpc calculation; added coding for
    !+ad_hisc               radial plates
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    integer :: i,peaktfflag
    real(kind(1.0D0)) :: awpc,awptf,bcylir,cplen,leni,leno,leno0, &
         radwp,rbcndut,rcoil,rcoilp,tant,thtcoil,wbtf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Determine layout of the inboard midplane TF coil leg

    !  Radius of centre of inboard TF coil leg

    if (itart == 1) then
       rtfcin = bore + 0.5D0*tfcth
    else
       rtfcin = bore + ohcth + gapoh + 0.5D0*tfcth
    end if

    !  Radius of outer edge of inboard leg

    rcoil = rtfcin + 0.5D0*tfcth

    !  Radius of inner edge of inboard leg

    rcoilp = rcoil - tfcth

    !  Half toroidal angular extent of a single TF coil inboard leg

    thtcoil = pi/tfno
    tant = tan(thtcoil)

    !  TF coil width in toroidal direction

    tftort = 2.0D0 * rcoil*sin(thtcoil)

    !  Annular area of midplane containing TF coil inboard legs

    tfareain = pi * (rcoil**2 - rcoilp**2) 

    !  Total current in TF coils

    ritfc = oacdcp * tfareain
    if (ritfc < 0.0D0) then
       fdiags(1) = ritfc ; fdiags(2) = oacdcp ; fdiags(3) = tfareain
       call report_error(97)
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'TF coil current is negative, ritfc = ',ritfc
       write(*,*) '   Overall current density, oacdcp = ',oacdcp
       write(*,*) '    Cross-sectional area, tfareain = ',tfareain
       write(*,*) ' '
       write(*,*) ' PROCESS continuing, ritfc forced to be +ve...'
       ritfc = abs(ritfc)  !  changed from 1.0D0
    end if

    !  Peak toroidal field (assuming axisymmetry) and radius of its occurrence,
    !  assumed to be at the outer edge of the winding pack

    rbmax = rcoil - casthi
    bmaxtf = 2.0D-7 * ritfc / rbmax

    !  Calculation of forces : centering and vertical

    cforce = bmaxtf*ritfc/(2.0D0*tfno)
    vforce = 0.5D0 * bt * rmajor * 0.5D0*ritfc * &
         log(rtot/rtfcin) / tfno

    !  The rest of this routine deals with superconducting coils.

    !  Define coil shape

    call coilshap

    !  Calculation of TF coil magnetic energy

    call tfcind(tfcth)

    !  Find TF coil energy (GJ)

    estotf = 1.0D-9 *  0.5D0*tfind / tfno * ritfc**2

    !  Case thicknesses (inboard leg)

    if (tfc_model == 0) thkcas = tfcth * 0.5D0

    !  N.B. Calculations below may be spurious for tfc_model=0 (solid copper coils),
    !  but this probably does not matter

    !  thkcas: case thickness of side further away from plasma
    !  casthi: case thickness of side nearest plasma
    !  casths: case thickness of side wall

    !  Winding pack dimensions (each inboard leg)

    !  Radial extent

    thkwp = tfcth - casthi - thkcas - 2.0D0*tinstf

    if (thkwp <= 0.0D0) then
       fdiags(1) = thkwp ; fdiags(2) = tfcth
       fdiags(3) = thkcas ; fdiags(4) = casthi
       fdiags(5) = casths ; fdiags(6) = tinstf
       call report_error(98)
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'Negative winding pack thickness:        thkwp (m) =',thkwp
       write(*,*) 'Inboard TF coil thickness is too small: tfcth (m) =',tfcth
       write(*,*) 'Increase tfcth or make it an iteration variable (13) with'
       write(*,*) 'a lower bound, or reduce the case and insulation thicknesses'
       write(*,*) 'or their upper bounds:'
       write(*,*) 'thkcas=',thkcas,' casthi=',casthi
       write(*,*) 'casths=',casths,' tinstf=',tinstf
       write(*,*) ' '
    end if

    !  Radius of geometrical centre of winding pack

    radwp = rcoil - casthi - tinstf - 0.5D0*thkwp

    !  Thickness of winding pack section at R > radwp

    wwp1 = 2.0D0 * (radwp*tant - casths - tinstf)

    !  Thickness of winding pack section at R < radwp

    wwp2 = 2.0D0 * ((radwp-0.5D0*thkwp)*tant - casths - tinstf)

    !  Total cross-sectional area of winding pack

    awptf = (0.5D0*thkwp)*(wwp1 + wwp2)

    !  Total cross-sectional area of winding pack,
    !  including the surrounding ground-wall insulation layer

    awpc = 0.5D0*thkwp*(wwp2 + 2.0D0*tinstf) + &
         (0.5D0*thkwp + 2.0D0*tinstf)*(wwp1 + 2.0D0*tinstf)

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

    !  Superconducting cable information
    !  (number of turns not required to be an integer here for numerics)

    !  Radius of rounded corners of cable space inside conduit

    rbcndut = thwcndut * 0.75D0

    !  Dimension of square cross-section of each turn

    leno0 = sqrt(cpttf / jwptf)

    !  Half-thickness of radial plates and inter-turn steel caps (m)
    !  prp is the ratio of the radial plates + caps cross-sectional area
    !  to the winding pack area awptf; solve for trp via simple quadratic eqn

    trp = 0.5D0 * (-leno0 + sqrt(leno0*leno0 + prp*awptf))

    !  Dimension of square cross-section of a turn, including the radial plate

    leno = leno0 + 2.0D0*trp

    !  Dimension of square cable space inside insulation and case of
    !  the conduit of each turn

    leni = leno0 - 2.0D0*(thwcndut + thicndut)

    if (leni <= 0.0D0) then
       fdiags(1) = leni ; fdiags(2) = leno0
       fdiags(3) = thwcndut ; fdiags(4) = thicndut
       call report_error(100)
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'Cable space dimension, leni = ',leni
       write(*,*) 'Reduce conduit case or insulation thicknesses,'
       write(*,*) 'or increase cpttf value or lower bound.'
       write(*,*) ' '
    end if

    !  Cross-sectional area of cable space per turn

    acstf = leni**2 - (4.0D0-pi)*rbcndut**2

    if (acstf <= 0.0D0) then
       if (leni < 0.0D0) then
          fdiags(1) = acstf ; fdiags(2) = leni
          call report_error(101)
          write(*,*) 'Warning in routine SCTFCOIL:'
          write(*,*) '    Cable space area, acstf = ',acstf
          write(*,*) 'Cable space dimension, leni = ',leni
          write(*,*) ' '
       else
          fdiags(1) = acstf ; fdiags(2) = leni
          call report_error(102)
          write(*,*) 'Warning in routine SCTFCOIL:'
          write(*,*) '    Cable space area, acstf = ',acstf
          write(*,*) 'Cable space dimension, leni = ',leni
          write(*,*) 'Reduce the upper limit for thwcndut (TF coil conduit'
          write(*,*) 'case thickness, iteration variable 58),'
          write(*,*) 'or remove it from the list of iteration variables.'
          write(*,*) 'Artificially set rounded corner radius to zero'
          write(*,*) ' '
          rbcndut = 0.0D0
          acstf = leni**2
       end if
    end if

    !  Cross-sectional area of conduit case per turn

    acndttf = (leni + 2.0D0*thwcndut)**2 - acstf

    !  Total number of turns per TF coil

    turnstf = awptf / (leno*leno)

    !  Total radial plate + steel cap cross-sectional area
    !  prp = (arp/turnstf) / awptf, hence the trp expression above

    arp = turnstf * 4.0D0 * trp*(trp + leno0)

    !  Total conductor cross-sectional area, taking account of void area

    acond = acstf * turnstf * (1.0D0-vftf)

    !  Void area in cable, for He

    avwp = acstf * turnstf * vftf

    !  Insulation area (not including ground-wall)

    aiwp = turnstf * (leno0**2 - acndttf - acstf)

    !  Area of steel structure in winding pack

    aswp = turnstf*acndttf + arp

    !  Coil perimeter along its cross-sectional centre
    !  N.B. tfthko = tfcth is set in radialb routine for superconducting coils

    tfleng = 0.0D0
    do i = 1,4
       tfleng = tfleng + 2.0D0*(radctf(i) + 0.5D0*tfcth) * dthet(i)
    end do

    !  TF coil horizontal and vertical bores

    tfboreh = rtot - rtfcin - tfcth  !  tfcth = 0.5D0*(tfthko + tfcth)
    tfborev = (hpfu - tfcth) + hmax

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

    !  The length of the vertical section is that of the first (inboard) segment

    cplen = 2.0D0*(radctf(1) + 0.5D0*tfcth) * dthet(1)

    !  The 2.2 factor is used as a scaling factor to fit
    !  to the ITER-FDR value of 450 tonnes; see CCFE note T&M/PKNIGHT/PROCESS/026

    whtcas = 2.2D0 * dcase * (cplen * acasetf + (tfleng-cplen) * acasetfo)

    !  Mass of radial plates + caps

    whtrp = tfleng * arp * denstl

    !  Mass of ground-wall insulation (assumed to be same density/material as
    !  conduit insulation)

    whtgw = tfleng * (awpc-awptf) * dcondins

    !  Masses of conductor constituents:

    !  Superconductor

    whtconsc = tfleng * turnstf * acstf*(1.0D0-vftf) * &
         (1.0D0-fcutfsu)*dcond(isumattf)

    !  Copper

    whtconcu = tfleng * turnstf * acstf*(1.0D0-vftf) * &
         fcutfsu*dcopper

    !  Steel conduit (sheath)

    whtconsh = tfleng * turnstf * acndttf * denstl

    !  Conduit insulation (aiwp already contains turnstf)

    whtconin = tfleng * aiwp * dcondins

    !  Total conductor mass

    whtcon = whtconsc + whtconcu + whtconsh + whtconin

    !  Total TF coil mass (all coils)

    whttf = (whtcas + whtcon + whtrp + whtgw) * tfno

    !  Peak field including ripple

    call peak_tf_with_ripple(tfno,wwp1,thkwp,radwp,bmaxtf,bmaxtfrp,peaktfflag)

    !  Do stress calculations

    call stresscl

    if (iprint == 1) call outtf(outfile, peaktfflag)

    return

20  continue

    !  Diagnostic output only (uncomment goto lines above to activate)

    write(*,*) '   tfcth = ',tfcth
    write(*,*) '  thkcas = ',thkcas
    write(*,*) '  casthi = ',casthi
    write(*,*) '  casths = ',casths
    write(*,*) '  tinstf = ',tinstf
    write(*,*) 'thwcndut = ',thwcndut
    write(*,*) 'thicndut = ',thicndut
    write(*,*) '   cpttf = ',cpttf

    stop

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

    real(kind(1.0D0)) :: t,wmax,y,z
    real(kind(1.0D0)), dimension(4) :: a

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    flag = 0

    !  Set fitting coefficients for different numbers of TF coils

    select case (nint(tfno))

    case (16)
       a(1) =  0.32715D0
       a(2) =  1.9715D0
       a(3) = -1.2326D0
       a(4) =  1.1419D0

    case (18)
       a(1) =  0.33705D0
       a(2) =  1.9517D0
       a(3) = -1.1414D0
       a(4) =  1.0661D0

    case (20)
       a(1) =  0.30288D0
       a(2) =  2.0272D0
       a(3) = -1.1348D0
       a(4) =  1.0913D0

    case default

       !  Original calculation - no fits were performed

       bmaxtfrp = 1.09D0 * bmaxtf
       return

    end select

    !  Maximum winding pack width before adjacent packs touch
    !  (ignoring the external case and ground wall thicknesses)

    wmax = 2.0D0 * (tfin - 0.5D0*thkwp) * tan(pi/tfno)

    !  Dimensionless winding pack width

    t = wwp1/wmax
    if ((t < 0.3D0).or.(t > 1.1D0)) then
       !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; t = ',t
       flag = 1
    end if

    !  Dimensionless winding pack radial thickness

    z = thkwp/wmax
    if ((z < 0.26D0).or.(z > 0.7D0)) then
       !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; z = ',z
       flag = 2
    end if

    !  Ratio of peak field with ripple to nominal axisymmetric peak field

    y = a(1) + a(2)*exp(-t) + a(3)*z + a(4)*z*t

    bmaxtfrp = y * bmaxtf

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

    integer :: i
    real(kind(1.0D0)) :: seff, tcbs, fac, svmxz, svmyz

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Young's modulus of radial plates - same as
    !  that of steel case

    eyrp = eystl

    !  Allowable stress (Pa)

    alstrtf = min( (2.0D0*csytf/3.0D0), (0.5D0*csutf) )

    !  Simple stress model option

    if (tfc_model == 0) then
       call sctfjalw(bmaxtfrp,rtfcin,rtot,rbmax,(1.0D-6*alstrtf), &
            tdmptf,jwdgcrt)
       return
    end if

    !  Set up graded stress model call information

    seff = sqrt(cpttf/jwptf) + 2.0D0*trp
    if (acstf >= 0.0D0) then
       tcbs = sqrt(acstf)
    else
       tcbs = 0.0D0
    end if

    !  CCFE two-layer model
    !  Layers are labelled from inboard to outboard
    !  The first layer is the steel casing inboard of the winding pack,
    !  while the second layer is the winding pack itself

    radtf(1) = rtfcin - 0.5D0*tfcth
    radtf(2) = rbmax - thkwp
    radtf(3) = rbmax

    eyoung(1) = eystl
    eyoung(2) = eyngeff(eystl,eyins,eywp,eyrp,trp,thicndut,seff,thwcndut,tcbs)

    jeff(1) = 0.0D0
    jeff(2) = ritfc / ( pi * (radtf(3)**2 - radtf(2)**2))

    !  Call stress routine

    call two_layer_stress(poisson,radtf,eyoung,jeff,sigrtf,sigttf,deflect)

    !  Convert to conduit + case

    fac = eystl*eyins*seff / &
         (eyins*(seff-2.0D0*thicndut) + 2.0D0*thicndut*eystl)

    sigrcon = sigrtf(2)/eyoung(2) * fac
    sigtcon = sigttf(2)/eyoung(2) * fac
    sigvert = vforce / (acasetf + acndttf*turnstf + arp)

    !  Find case strain

    casestr = sigvert / eystl

    !  Find Von-Mises stresses
    !  For winding pack region take worst of two walls
    svmxz = sigvm(sigrcon, 0.0D0, sigvert, 0.0D0,0.0D0,0.0D0)
    svmyz = sigvm(0.0D0, sigtcon, sigvert, 0.0D0,0.0D0,0.0D0)
    strtf1 = max(svmxz,svmyz)

    strtf2 = sigvm(sigrtf(1), sigttf(1), sigvert, 0.0D0,0.0D0,0.0D0)

    !  Young's modulus and strain in vertical direction on winding pack

    eyzwp = eyngzwp(eystl,eyins,eywp,eyrp,trp,thicndut,seff,thwcndut,tcbs)
    windstrain = sigvert / eyzwp

    !  Radial strain in insulator

    insstrain = sigrtf(2) / eyins * &
         edoeeff(eystl,eyins,eywp,eyrp,trp,thicndut,seff,thwcndut,tcbs)

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

  function eyngeff(estl,eins,ewp,erp,trp,tins,teff,tstl,tcs)

    !+ad_name  eyngeff
    !+ad_summ  Finds the effective Young's modulus of the TF coil winding pack
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  ewp  : input real : Young's modulus of windings (Pa)
    !+ad_args  erp  : input real : Young's modulus of radial plates (Pa)
    !+ad_args  trp  : input real : half-thickness of radial plates (m)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  teff : input real : dimension of total cable with insulator (m)
    !+ad_args  tstl : input real : thickness of steel conduit (m)
    !+ad_args  tcs  : input real : dimension of cable space area inside conduit (m)
    !+ad_desc  This routine calculates the effective Young's modulus (Pa)
    !+ad_desc  of the TF coil in the winding pack section.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/91 JG  Initial version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  30/04/14 PJK/JM Modifications for two-layer stress model
    !+ad_hist  07/05/14 PJK Changed trp comment; modified new model so that
    !+ad_hisc               only conduit and radial plate regions contribute
    !+ad_hist  16/09/14 PJK Removed model switch and old (Galambos/Myall) calculation
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: eyngeff

    !  Arguments

    real(kind(1.0D0)), intent(in) :: estl,eins,ewp,erp,trp,tins,teff,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ed,ttot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total thickness of a turn with radial plates

    ttot = tcs + 2.0D0*(trp + tins + tstl)

    !  See Figure 8 and Section III.4, Morris

    ed = ttot / (2.0D0*trp/erp + 2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)

    eyngeff = 1.0D0/ttot * ( 2.0D0*trp*erp + 2.0D0*tstl*ed )

  end function eyngeff

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function edoeeff(estl,eins,ewp,erp,trp,tins,teff,tstl,tcs)

    !+ad_name  edoeeff
    !+ad_summ  Returns ratio of E_d to E_eff in Morris
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  ewp  : input real : Young's modulus of windings (Pa)
    !+ad_args  erp  : input real : Young's modulus of radial plates (Pa)
    !+ad_args  trp  : input real : half-thickness of radial plates (m)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  teff : input real : dimension of total cable with insulator (m)
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

    real(kind(1.0D0)), intent(in) :: estl,eins,ewp,erp,trp,tins,teff,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ed,ttot,eeff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total thickness of a turn with radial plates

    ttot = tcs + 2.0D0*(trp + tins + tstl)

    !  Code copied from eyngeff routine
    !  See Figure 8 and Section III.4, Morris

    ed = ttot / (2.0D0*trp/erp + 2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)

    eeff = 1.0D0/ttot * ( 2.0D0*trp*erp + 2.0D0*tstl*ed )

    edoeeff = ed/eeff

  end function edoeeff

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function eyngzwp(estl,eins,ewp,erp,trp,tins,teff,tstl,tcs)

    !+ad_name  eyngzwp
    !+ad_summ  Finds the vertical Young's modulus of the TF coil winding pack
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  ewp  : input real : Young's modulus of windings (Pa)
    !+ad_args  erp  : input real : Young's modulus of radial plates (Pa)
    !+ad_args  trp  : input real : half-thickness of radial plates (m)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  teff : input real : dimension of total cable with insulator (m)
    !+ad_args  tstl : input real : thickness of steel conduit (m)
    !+ad_args  tcs  : input real : dimension of cable space area inside conduit (m)
    !+ad_desc  This routine calculates the vertical Young's modulus (Pa)
    !+ad_desc  of the TF coil in the winding pack section.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/04/14 PJK/JM Initial version
    !+ad_hist  07/05/14 PJK Changed trp comment
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: eyngzwp

    !  Arguments

    real(kind(1.0D0)), intent(in) :: estl,eins,ewp,erp,trp,tins,teff,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ttot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ttot = tcs + 2.0D0*(trp + tins + tstl)

    eyngzwp = ewp*tcs*tcs &
         + estl*( (tcs + 2.0D0*tstl)**2 - tcs*tcs ) &
         + eins*( (tcs + 2.0D0*(tstl + tins))**2 - (tcs + 2.0D0*tstl)**2 ) &
         + erp*( ttot*ttot - (tcs + 2.0D0*(tstl + tins))**2 )

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
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the TF coil shape. The coil is
    !+ad_desc  approximated by four arcs along the edge facing the plasma.
    !+ad_desc  The geometry is a fit to the 1989 ITER design.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/03/89 JG  Initial version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  18/12/12 PJK/RK Modified coil shape yarc(3) for single-null cases
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: thet2, thet3, thet4

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Point on inboard midplane

    xarc(1) = rtfcin + 0.5D0*tfcth
    yarc(1) = 0.0D0

    !  Point at top of coil

    xarc(3) = rmajor - 0.2D0*rminor
    yarc(3) = 0.5D0*(hpfu - tfcth + hmax)

    !  Point at top of straight section

    xarc(2) = xarc(1) + 0.07D0*rminor*kappa
    yarc(2) = yarc(3) * 0.7D0

    !  Point at outboard side

    xarc(5) = rtot - 0.5D0*tfthko
    yarc(5) = 0.0D0

    !  Point no.4

    xarc(4) = xarc(3) + farc4tf*(xarc(5) - xarc(3))
    yarc(4) = 0.72D0 * yarc(3)

    !  Find arc centres

    yctfc(4) = 0.0D0
    xctfc(4) = 0.5D0*(xarc(5)**2 - xarc(4)**2 - yarc(4)**2) &
         / (xarc(5) - xarc(4))
    thet4 = atan2(yarc(4), (xarc(4)-xctfc(4)) )
    dthet(4) = abs(thet4)
    radctf(4) = sqrt( (yarc(4)-yctfc(4))**2 + (xarc(4)-xctfc(4))**2)

    xctfc(3) = (2.0D0*(yarc(4) - yarc(3))*(yarc(4)-tan(thet4)*xarc(4)) &
         + xarc(3)**2 + yarc(3)**2 - xarc(4)**2 - yarc(4)**2 ) / &
         2.0D0/( (xarc(3) - xarc(4)) - (yarc(4) - yarc(3))*tan(thet4) )
    yctfc(3) = yarc(4) - tan(thet4) * (xarc(4) - xctfc(3))
    thet3 = atan2( (yarc(3)-yctfc(3)), (xarc(3) - xctfc(3)) )
    dthet(3) = abs(thet3 - thet4)
    radctf(3) = sqrt( (yarc(3)-yctfc(3))**2 + (xarc(3)-xctfc(3))**2 )

    xctfc(2) = (2.0D0*(yarc(3) - yarc(2))*(yarc(3)-tan(thet3)*xarc(3)) &
         + xarc(2)**2 + yarc(2)**2 - xarc(3)**2 - yarc(3)**2) / &
         2.0D0/( (xarc(2) - xarc(3)) - (yarc(3) - yarc(2))*tan(thet3) )
    yctfc(2) = yarc(3) - tan(thet3) * (xarc(3) - xctfc(2))
    thet2 = atan2( (yarc(2)-yctfc(2)), (xarc(2) - xctfc(2)) )
    dthet(2) = abs(abs(thet2) - thet3)
    radctf(2) = sqrt( (yarc(2)-yctfc(2))**2 + (xarc(2)-xctfc(2))**2 )

    xctfc(1) = ( xarc(2)**2 - xarc(1)**2 + yarc(2)**2) / &
         (2.0D0*(xarc(2)-xarc(1)))
    yctfc(1) = 0.0D0
    radctf(1) = xctfc(1) - xarc(1)
    dthet(1) = atan2(yarc(2), (xctfc(1)-xarc(1)))

    !  Half-height of TF coil inboard leg straight section

    hr1 = yarc(2)

  end subroutine coilshap

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfcind(tfthk)

    !+ad_name  tfcind
    !+ad_summ  Calculates the self inductance of a TF coil
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_auth  S S Kalsi, FEDC
    !+ad_cont  N/A
    !+ad_args  tfthk        : input real : TF coil thickness (m)
    !+ad_desc  This routine calculates the self inductance of a TF coil
    !+ad_desc  that is simulated by four arcs.
    !+ad_desc  <P>Note: arcs start on the outboard side and go counter-clockwise
    !+ad_desc  in Kalsi notation. Top/bottom symmetry is assumed.
    !+ad_prob  The code is hardwired to expect narc = 4.
    !+ad_prob  <P>This routine is very sensitive to trivial code changes...
    !+ad_call  None
    !+ad_hist  03/09/85 SSK Initial version
    !+ad_hist  27/01/88 JG  Modified to use arcs whose centres do not
    !+ad_hisc               have to lie on the radius of the adjacent arc.
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  18/10/12 PJK Removed all but one argument
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: tfthk

    !  Local variables

    integer :: ns,i,k
    integer, parameter :: narc = 4
    integer, parameter :: ntot = 100
    integer, dimension(6) :: npnt

    real(kind(1.0D0)) :: al,ax,ay,b,deltht,dr,h,hstar,r,rc,rbore,t,theta
    real(kind(1.0D0)), dimension(6) :: ang,dtht,xc,yc,xs,ys

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Convert to Kalsi notation

    ns = narc
    do i = 1,ns
       dtht(i) = dthet(ns + 1 - i)
       xs(i) = xarc(ns + 2 - i)
       ys(i) = yarc(ns + 2 - i)
       xc(i) = xctfc(ns + 1 - i)
       yc(i) = yctfc(ns + 1 - i)
    end do

    rbore = xs(1) - xs(ns)
    tfind = 0.0D0

    do i = 1,ns
       if (i < ns) npnt(i) = int(dble(ntot) * (xs(i)-xs(i+1))/rbore)
       ax = xs(i)-xc(i)
       ay = ys(i)-yc(i)
       ang(i) = atan2(ay,ax)
    end do
    npnt(ns) = 3

    do k = 1,ns
       deltht = dtht(k) / npnt(k)
       t = ang(k) - 0.5D0*deltht
       rc = sqrt( (xc(k)-xs(k))**2 + (yc(k)-ys(k))**2 )
       do i = 1,npnt(k)
          theta = t + dble(i)*deltht
          r = xc(k) + rc*cos(theta)
          b = rmu0/(2.0D0*pi*r)
          dr = rc*(cos(theta - 0.5D0*deltht) - cos(theta + 0.5D0*deltht))
          h = yc(k) + rc*sin(theta)

          !  Assume B in TF coil = 1/2  B in bore

          hstar = tfthk / sin(theta)
          al = b*dr*(2.0D0*h + hstar)
          tfind = tfind + al
       end do
    end do

    !  Add contribution in TF coil inboard leg

    tfind = tfind + b*tfthk*ys(ns)

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
    !+ad_hist  08/05/14 PJK Redefined trp; replaced stress_model with tfc_model
    !+ad_hist  08/05/14 PJK Changed ripmax description
    !+ad_hist  16/06/14 PJK Removed duplicate outputs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  30/07/14 PJK Renamed borev to tfborev; changed estotf output
    !+ad_hist  31/07/14 PJK Added acasetfo and several masses
    !+ad_hist  02/09/14 PJK Added peaktfflag usage
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, peaktfflag

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: ap
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
    end select

    call osubhd(outfile,'Wedged TF Coils, with two-step winding')

    call ocmmnt(outfile,'Current Density :')
    call oblnkl(outfile)
    call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf)
    call ovarre(outfile,'Overall current density (A/m2)','(oacdcp)',oacdcp)

    if (tfc_model == 0) then
       call ovarre(outfile,'Allowable overall current density (A/m2)', &
            '(jwdgcrt)',jwdgcrt)
    end if

    call osubhd(outfile,'General Coil Parameters :')
    call ovarre(outfile,'Number of TF coils','(tfno)',tfno)
    call ovarre(outfile,'Cross-sectional area per coil (m2)','(tfarea/tfno)', &
         tfareain/tfno)
    call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth.)',tfcth)
    call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko.)',tfthko)
    call ovarre(outfile,'Inboard leg outboard half-width (m)','(tficrn)',tficrn)
    call ovarre(outfile,'Inboard leg inboard half-width (m)','(tfocrn)',tfocrn)
    call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort)
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng)
    call ovarre(outfile,'Total current (MA)','(ritfc/1.D6)',1.0D-6*ritfc)
    call ovarre(outfile,'Peak field (Amperes Law,T)','(bmaxtf)',bmaxtf)
    call ovarre(outfile,'Peak field (with ripple,T)','(bmaxtfrp)',bmaxtfrp)
    call ovarre(outfile,'Max allowed ripple amplitude at plasma (%)','(ripmax)',ripmax)
    call ovarre(outfile,'Ripple amplitude at plasma (%)','(ripple)',ripple)
    call ovarre(outfile,'Total stored energy in TF coils (GJ)','(estotf*tfno)',estotf*tfno)
    call ovarre(outfile,'Total mass of TF coils (kg)','(whttf)',whttf)
    call ovarre(outfile,'Mass of each TF coil (kg)','(whttf/tfno)',whttf/tfno)
    call ovarre(outfile,'Vertical separating force per leg (N)','(vforce)',vforce)
    call ovarre(outfile,'Centering force per coil (N/m)','(cforce)',cforce)

    !  Report any applicability issues with peak field with ripple calculation

    if (peaktfflag == 1) then
       call report_error(144)
    else if (peaktfflag == 2) then
       call report_error(145)
    else
       continue
    end if

    call osubhd(outfile,'Coil Geometry :')
    call ovarre(outfile,'Inboard leg centre radius (m)','(rtfcin)',rtfcin)
    call ovarre(outfile,'Outboard leg centre radius (m)','(rtot)',rtot)
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax)
    call ovarre(outfile,'Clear horizontal bore (m)','(tfboreh)',tfboreh)
    call ovarre(outfile,'Clear vertical bore (m)','(tfborev)',tfborev)

    call oblnkl(outfile)
    call ocmmnt(outfile,'TF coil inner surface shape is approximated')
    call ocmmnt(outfile,'by arcs between the following points :')
    call oblnkl(outfile)

    write(outfile,10)
10  format(t2,'point',t16,'x(m)',t31,'y(m)')

    do i = 1,5
       write(outfile,20) i,xarc(i),yarc(i)

       intstring = int2char(i)
       call ovarre(mfile,'TF coil arc point '//intstring//' R (m)', &
            '(xarc('//intstring//'))',xarc(i))
       call ovarre(mfile,'TF coil arc point '//intstring//' Z (m)', &
            '(yarc('//intstring//'))',yarc(i))

    end do
20  format(i4,t10,f10.3,t25,f10.3)

    call osubhd(outfile,'The centres of the arc are :')
    write(outfile,40)
40  format(t3,'arc',t16,'x(m)',t30,'y(m)')

    do i = 1,4
       write(outfile,20) i,xctfc(i),yctfc(i)

       intstring = int2char(i)
       call ovarre(mfile,'TF coil arc '//intstring//' centre R (m)', &
            '(xctfc('//intstring//'))',xctfc(i))
       call ovarre(mfile,'TF coil arc '//intstring//' centre Z (m)', &
            '(yctfc('//intstring//'))',yctfc(i))

    end do

    call osubhd(outfile,'Conductor Information :')
    call ovarre(outfile,'Superconductor mass per coil (kg)','(whtconsc)',whtconsc)
    call ovarre(outfile,'Copper mass per coil (kg)','(whtconcu)',whtconcu)
    call ovarre(outfile,'Steel conduit mass per coil (kg)','(whtconsh)',whtconsh)
    call ovarre(outfile,'Conduit insulation mass per coil (kg)','(whtconin)',whtconin)
    call ovarre(outfile,'Total conductor cable mass per coil (kg)','(whtcon)',whtcon)
    call ovarre(outfile,'Cable conductor + void area (m2)','(acstf)',acstf)
    call ovarre(outfile,'Cable space coolant fraction','(vftf)',vftf)
    call ovarre(outfile,'Conduit case thickness (m)','(thwcndut)',thwcndut)
    call ovarre(outfile,'Conduit insulation thickness (m)','(thicndut)',thicndut)

    ap = acond + aswp + aiwp + avwp

    call osubhd(outfile,'Winding Pack Information :')
    call ovarre(outfile,'Conductor fraction of winding pack','(acond/ap)',acond/ap)
    call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcutfsu)
    call ovarre(outfile,'Structure fraction of winding pack','(aswp/ap)',aswp/ap)
    call ovarre(outfile,'Insulator fraction of winding pack','(aiwp/ap)',aiwp/ap)
    call ovarre(outfile,'Helium fraction of winding pack','(avwp/ap)',avwp/ap)
    call ovarre(outfile,'Winding radial thickness (m)','(thkwp)',thkwp)
    call ovarre(outfile,'Winding width 1 (m)','(wwp1)',wwp1)
    call ovarre(outfile,'Winding width 2 (m)','(wwp2)',wwp2)
    call ovarre(outfile,'Radial plate thickness (m)','(2*trp)',2.0D0*trp)
    call ovarre(outfile,'Mass of radial plates + caps per coil (kg)','(whtrp)',whtrp)
    call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
    call ovarre(outfile,'Ground wall mass per coil (kg)','(whtgw)',whtgw)
    call ovarre(outfile,'Number of turns per TF coil','(turnstf)',turnstf)
    call ovarre(outfile,'Current per turn (A)','(cpttf)',cpttf)

    call osubhd(outfile,'External Case Information :')

    call ovarre(outfile,'Inboard leg case outboard thickness (m)','(casthi)',casthi)
    call ovarre(outfile,'Inboard leg case inboard thickness (m)','(thkcas)',thkcas)
    call ovarre(outfile,'Inboard leg case toroidal thickness (m)','(casths)',casths)
    call ovarre(outfile,'Inboard leg case area per coil (m2)','(acasetf)',acasetf)
    call ovarre(outfile,'Outboard leg case area per coil (m2)','(acasetfo)',acasetfo)
    call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas)

    if (tfc_model == 0) then
       call osubhd(outfile,'TF Coil Stresses (solid copper coil model) :')
    else
       call osubhd(outfile,'TF Coil Stresses (CCFE two-layer model) :')
    end if
    call ovarin(outfile,'TF coil model','(tfc_model)',tfc_model)
    call ovarre(outfile,'Vertical stress (Pa)','(sigvert)',sigvert)
    call ovarre(outfile,'Conduit radial stress (Pa)','(sigrcon)',sigrcon)
    call ovarre(outfile,'Conduit tangential stress (Pa)','(sigtcon)',sigtcon)
    call ovarre(outfile,'Conduit Von Mises combination stress (Pa)','(strtf1)',strtf1)
    if (tfc_model == 1) then
       call ovarre(outfile,'Case radial stress (Pa)','(sigrtf(1))',sigrtf(1))
       call ovarre(outfile,'Case tangential stress (Pa)','(sigttf(1))',sigttf(1))
    end if
    call ovarre(outfile,'Case Von Mises combination stress (Pa)','(strtf2)',strtf2)
    call ovarre(outfile,'Allowable stress (Pa)','(alstrtf)',alstrtf)
    call ovarre(outfile,'Deflection at midplane (m)','(deflect)',deflect)
    if (tfc_model == 1) then
       call ovarre(outfile,"Winding pack vertical Young's Modulus (Pa)",'(eyzwp)', &
            eyzwp)
       call ovarre(outfile,'Vertical strain on winding pack','(windstrain)', &
            windstrain)
       call ovarre(outfile,'Radial strain on insulator','(insstrain)', &
            insstrain)
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
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(kind(1.0D0)) :: aturn, tfes, vdump

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Simple model

    if (tfc_model == 0) then
       vtfskv = 20.0D0
       return
    end if

    !  Stored energy (J) and cross-sectional area per turn

    tfes = estotf * 1.0D9
    aturn = ritfc/(jwptf*tfno*turnstf)

    call supercon(acstf,aturn,bmaxtfrp,vftf,fcutfsu,cpttf,jwptf,isumattf, &
         fhts,strncon,tdmptf,tfes,tftmp,tmaxpro,bcritsc,tcritsc,iprint, &
         outfile,jwdgpro,jwdgcrt,vdump,tmargtf)

    !  Dump voltage in kV

    vtfskv = vdump/1.0D3

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supercon(acs,aturn,bmax,fhe,fcu,iop,jwp,isumat,fhts, &
         strain,tdump,tfes,thelium,tmax,bcritsc,tcritsc,iprint,outfile, &
         jwdgpro,jwdgcrt,vd,tmarg)

      !+ad_name  supercon
      !+ad_summ  Routine to calculate the TF coil superconductor properties
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  R Kemp, CCFE, Culham Science Centre
      !+ad_auth  M Kovari, CCFE, Culham Science Centre
      !+ad_auth  J Miller, ORNL
      !+ad_cont  N/A
      !+ad_args  acs : input real : Cable space - inside area (m2)
      !+ad_args  aturn : input real : Area per turn (i.e. entire jacketed cable) (m2)
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
      !+ad_args  fhts    : input real : Adjustment factor (<= 1) to account for strain,
      !+ad_argc                         radiation damage, fatigue or AC losses
      !+ad_args  strain : input real : Strain on superconductor at operation conditions
      !+ad_args  tdump : input real : Dump time (sec)
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
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: isumat, iprint, outfile
      real(kind(1.0D0)), intent(in) :: acs, aturn, bmax, fcu, fhe, fhts, &
           iop, jwp, strain, tdump, tfes, thelium, tmax, bcritsc, tcritsc
      real(kind(1.0D0)), intent(out) :: jwdgpro, jwdgcrt, vd, tmarg

      !  Local variables

      integer :: lap
      real(kind(1.0D0)) :: b,bc20m,bcrit,c0,delt,fcond,icrit,iooic, &
           jcritsc,jcrit0,jcritm,jcritp,jcritstr,jsc,jstrand,jtol,jwdgop, &
           t,tc0m,tcrit,ttest,ttestm,ttestp

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Conductor fraction

      fcond = 1.0D0 - fhe

      !  Find critical current density in superconducting strand, jcritstr

      select case (isumat)

      case (1)  !  ITER Nb3Sn critical surface parameterization
         bc20m = 32.97D0
         tc0m = 16.06D0

         !  jcritsc returned by itersc is the critical current density in the
         !  superconductor - not the whole strand, which contains copper

         call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
         jcritstr = jcritsc * (1.0D0-fcu)

      case (2)  !  Bi-2212 high temperature superconductor parameterization

         !  Current density in a strand of Bi-2212 conductor
         !  N.B. jcrit returned by bi2212 is the critical current density
         !  in the strand, not just the superconducting portion.
         !  The parameterization for jcritstr assumes a particular strand
         !  composition that does not require a user-defined copper fraction,
         !  so this is irrelevant in this model

         !  Previously (wrongly) jstrand = jwp * acs*(1.0D0-fhe)/aturn
         jstrand = jwp * aturn / (acs*(1.0D0-fhe))

         call bi2212(bmax,jstrand,thelium,fhts,jcritstr,tmarg)
         jcritsc = jcritstr / (1.0D0-fcu)
         tcrit = thelium + tmarg

      case (3)  !  NbTi data
         bc20m = 15.0D0
         tc0m = 9.3D0
         c0 = 1.0D10
         call jcrit_nbti(thelium,bmax,c0,bc20m,tc0m,jcritsc,tcrit)
         jcritstr = jcritsc * (1.0D0-fcu)

      case (4)  !  As (1), but user-defined parameters
         bc20m = bcritsc
         tc0m = tcritsc
         call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
         jcritstr = jcritsc * (1.0D0-fcu)

      case default  !  Error condition
         idiags(1) = isumat ; call report_error(105)

      end select

      !  Critical current

      icrit = jcritstr * acs * (1.0D0-fhe)

      !  Critical current density in winding pack

      jwdgcrt = icrit / aturn

      !  Ratio of operating / critical current

      iooic = iop / icrit

      !  Operating current density

      jwdgop = iop / aturn

      !  Temperature margin (already calculated in bi2212 for isumat=2)

      if (isumat /= 2) then

         !  Newton-Raphson method; start at requested minimum temperature margin

         ttest = thelium + tmargmin
         delt = 0.01D0
         jtol = 1.0D4

         !  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
         !  when we have found the desired value of tmarg

         jsc = iooic * jcritsc

         lap = 0
         solve_for_tmarg: do ; lap = lap+1
            if ((ttest <= 0.0D0).or.(lap > 100)) then
               idiags(1) = lap ; fdiags(1) = ttest
               call report_error(157)
               exit solve_for_tmarg
            end if
            ttestm = ttest - delt
            ttestp = ttest + delt
            select case (isumat)
            case (1,4)
               call itersc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
               if (abs(jsc-jcrit0) <= jtol) exit solve_for_tmarg
               call itersc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
               call itersc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
            case (3)
               call jcrit_nbti(ttest ,bmax,c0,bc20m,tc0m,jcrit0,t)
               if (abs(jsc-jcrit0) <= jtol) exit solve_for_tmarg
               call jcrit_nbti(ttestm,bmax,c0,bc20m,tc0m,jcritm,t)
               call jcrit_nbti(ttestp,bmax,c0,bc20m,tc0m,jcritp,t)
            end select
            ttest = ttest - 2.0D0*delt*(jcrit0-jsc)/(jcritp-jcritm)
         end do solve_for_tmarg

         tmarg = ttest - thelium
         temp_margin = tmarg

      end if

      !  Find the current density limited by the protection limit
      !  (N.B. Unclear of this routine's relevance for Bi-2212 (isumat=2), due
      !  to presence of fcu argument, which is not used for this model above)

      call protect(iop,tfes,acs,aturn,tdump,fcond,fcu,thelium,tmax,jwdgpro,vd)

      if (iprint == 0) return

      call oheadr(outfile,'Superconducting TF Coils')

      select case (isumat)

      case (1)
         call ocmmnt(outfile,'Superconductor used: Nb3Sn')
         call ocmmnt(outfile,'  (ITER Jcrit model, standard parameters)')
      case (2)
         call ocmmnt(outfile,'Superconductor used: Bi-2212 HTS')
      case (3)
         call ocmmnt(outfile,'Superconductor used: NbTi')
      case (4)
         call ocmmnt(outfile,'Superconductor used: Nb3Sn')
         call ocmmnt(outfile,'  (ITER Jcrit model, user-defined parameters)')
      case default

      end select

      call oblnkl(outfile)
      call ovarre(outfile,'Peak field at conductor (T)','(bmax)',bmax)
      call ovarre(outfile,'Helium temperature at peak field (K)','(thelium)',thelium)
      call ovarre(outfile,'Helium fraction inside cable space','',fhe)
      call ovarre(outfile,'Copper fraction of conductor','(fcu)',fcu)
      call ovarre(outfile,'Strain on superconductor','(strain)',strain)

      call osubhd(outfile,'Critical Current Information :')
      if (isumat /= 2) then
         call ovarre(outfile,'Critical field at zero temperature and strain (T)', &
              '(bc20m)',bc20m)
         call ovarre(outfile,'Critical temperature at zero field and strain (K)', &
              '(tc0m)',tc0m)
      end if
      call ovarre(outfile,'Critical current density in superconductor (A/m2)','(jcritsc)',jcritsc)
      call ovarre(outfile,'Critical current density in strand (A/m2)','(jcritstr)',jcritstr)
      if ((isumat == 1).or.(isumat == 4)) then
         call ovarre(outfile,'Critical field (T)','(bcrit)',bcrit)
      end if
      call ovarre(outfile,'Critical temperature (K)','(tcrit)',tcrit)

      call ovarre(outfile,'Operating winding pack J (A/m2)','(jwdgop)',jwdgop)
      call ovarre(outfile,'Critical winding pack current density (A/m2)', &
           '(jwdgcrt)',jwdgcrt)
      call ovarre(outfile,'Critical current (A)','(icrit)',icrit)
      call ovarre(outfile,'Operating current / critical current','(iooic)', &
           iooic)
      call ovarre(outfile,'Temperature margin (K)','(tmarg)',tmarg)

      call osubhd(outfile,'Protection Information :')
      call ovarre(outfile,'Maximum temperature in quench (K)','(tmax)', &
           tmax)
      call ovarre(outfile,'Winding pack protection J (A/m2)','(jwdgpro)', &
           jwdgpro)
      call ovarre(outfile,'Dump time (s)','(tdump)',tdump)
      call ovarre(outfile,'Dump voltage (V)','(vd)',vd)

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine itersc(thelium,bmax,strain,bc20max,tc0max,jcrit,bcrit,tcrit)

    !+ad_name  itersc
    !+ad_summ  Implementation of ITER Nb3Sn critical surface implementation
    !+ad_type  Subroutine
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  thelium : input real : Coolant/SC temperature (K)
    !+ad_args  bmax : input real : Magnetic field at conductor (T)
    !+ad_args  strain : input real : Strain in superconductor
    !+ad_args  bc20max : input real : Upper critical field (T) for superconductor
    !+ad_argc                      at zero temperature and strain
    !+ad_args  tc0max : input real : Critical temperature (K) at zero field and strain
    !+ad_args  jcrit : output real : Critical current density in superconductor (A/m2)
    !+ad_args  bcrit : output real : Critical field (T)
    !+ad_args  tcrit : output real : Critical temperature (K)
    !+ad_desc  This routine calculates the critical current density and
    !+ad_desc  temperature in the superconducting TF coils using the
    !+ad_desc  ITER Nb3Sn critical surface model.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  21/07/11 RK  First draft of routine
    !+ad_hist  21/09/11 PJK Initial F90 version
    !+ad_hist  26/09/11 PJK Changed two exponents to double precision (which
    !+ad_hisc               may lead to surprisingly large changes in the result
    !+ad_hisc               if the Jcrit limit is being reached).
    !+ad_hisc               Added range-checking for tzero, bred
    !+ad_hist  26/09/11 PJK Converted to a subroutine, and added jcrit, tcrit
    !+ad_hisc               arguments
    !+ad_hist  16/04/13 PJK Converted bctw, tco to arguments instead of hardwired.
    !+ad_hisc               Corrected problems with jcrit and tcrit formulae
    !+ad_hist  08/10/14 PJK Clarified variable names; added Bottura reference
    !+ad_hist  12/11/14 PJK Added warning messages if limits reached
    !+ad_stat  Okay
    !+ad_docs  $J_C(B,T,\epsilon)$ Parameterization for ITER Nb3Sn production,
    !+ad_docc    L. Bottura, CERN-ITER Collaboration Report, Version 2, April 2nd 2008
    !+ad_docc    (distributed by Arnaud Devred, ITER, 10th April 2008)
    !+ad_docs  ITER Nb3Sn critical surface parameterization (2MMF7J) (2008),
    !+ad_docc    https://user.iter.org/?uid=2MMF7J&action=get_document
    !+ad_docs  ITER DDD 11-7: Magnets - conductors (2NBKXY) (2009),
    !+ad_docc    https://user.iter.org/?uid=2NBKXY&action=get_document
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: thelium, bmax, strain, bc20max, tc0max
    real(kind(1.0D0)), intent(out) :: jcrit, bcrit, tcrit

    !  Local variables

    !  Parameters named in Bottura

    real(kind(1.0D0)), parameter :: csc = 16500.0D6 !  scaling constant C
    real(kind(1.0D0)), parameter :: p = 0.63D0      !  low field exponent p
    real(kind(1.0D0)), parameter :: q = 2.1D0       !  high field exponent q
    real(kind(1.0D0)), parameter :: ca1 = 44.0D0    !  strain fitting constant C_{a1}
    real(kind(1.0D0)), parameter :: ca2 = 4.0D0     !  strain fitting constant C_{a2}
    real(kind(1.0D0)), parameter :: eps0a = 0.00256D0  !  epsilon_{0,a}
    real(kind(1.0D0)), parameter :: epsmax = -0.003253075D0  !  epsilon_{max} (not used)

    real(kind(1.0D0)), parameter :: diter = 0.82D0  !  ITER strand diameter (mm)
    real(kind(1.0D0)), parameter :: cuiter = 0.5D0  !  ITER strand copper fraction

    real(kind(1.0D0)) :: bred, epssh, t, bc20eps, &
         tc0eps, bzero, strfun, jc1, jc2, jc3, scalefac

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  $\epsilon_{sh}$

    epssh = (ca2*eps0a)/(sqrt(ca1**2 - ca2**2))

    !  Strain function $s(\epsilon)$
    !  0.83 < s < 1.0, for -0.005 < strain < 0.005

    strfun = sqrt(epssh**2 + eps0a**2) - sqrt((strain-epssh)**2 + eps0a**2)
    strfun = strfun*ca1 - ca2*strain
    strfun = 1.0D0 + (1.0D0/(1.0D0 - ca1*eps0a))*strfun

    !  $B^*_{C2} (0,\epsilon)$

    bc20eps = bc20max*strfun

    !  $T^*_C (0,\epsilon)$

    tc0eps = tc0max * strfun**(1.0D0/3.0D0)

    !  Reduced temperature, restricted to be < 1
    !  Should remain < 1 for thelium < 0.94*tc0max (i.e. 15 kelvin for isumattf=1)

    if (thelium/tc0eps >= 1.0D0) then
       fdiags(1) = thelium ; fdiags(2) = tc0eps
       call report_error(159)
    end if
    t = min(thelium/tc0eps, 0.9999D0)

    !  Reduced magnetic field at zero temperature
    !  Should remain < 1 for bmax < 0.83*bc20max (i.e. 27 tesla for isumattf=1)

    if (bmax/bc20eps >= 1.0D0) then
       fdiags(1) = bmax ; fdiags(2) = bc20eps
       call report_error(160)
    end if
    bzero = min(bmax/bc20eps, 0.9999D0)

    !  Critical temperature (K)

    tcrit = tc0eps * (1.0D0 - bzero)**(1.0D0/1.52D0)  !  bzero must be < 1 to avoid NaNs

    !  Critical field (T)

    bcrit = bc20eps * (1.0D0 - t**1.52D0)

    !  Reduced magnetic field, restricted to be < 1

    if (bmax/bcrit >= 1.0D0) then
       fdiags(1) = bmax ; fdiags(2) = bcrit
       call report_error(161)
    end if
    bred = min(bmax/bcrit, 0.9999D0)

    !  Critical current density in superconductor (A/m2)
    !  ITER parameterization is for the current in a single strand,
    !  not per unit area, so scalefac converts to current density

    scalefac = pi * (0.5D0*diter)**2 * (1.0D0-cuiter)

    jc1 = (csc/bmax)*strfun
    jc2 = (1.0D0-t**1.52D0) * (1.0D0-t**2)  !  t must be < 1 to avoid NaNs
    jc3 = bred**p * (1.0D0-bred)**q  !  bred must be < 1 to avoid NaNs

    jcrit = jc1 * jc2 * jc3 / scalefac

  end subroutine itersc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine jcrit_nbti(thelium,bmax,c0,bc20max,tc0max,jcrit,tcrit)

    !+ad_name  jcrit_nbti
    !+ad_summ  Critical current density in a NbTi superconductor strand
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  thelium : input real : Coolant/SC temperature (K)
    !+ad_args  bmax : input real : Magnetic field at conductor (T)
    !+ad_args  c0   : input real : Scaling constant (A/m2)
    !+ad_args  bc20max : input real : Upper critical field (T) for superconductor
    !+ad_argc                      at zero temperature and strain
    !+ad_args  tc0max : input real : Critical temperature (K) at zero field and strain
    !+ad_args  jcrit : output real : Critical current density in superconductor (A/m2)
    !+ad_args  tcrit : output real : Critical temperature (K)
    !+ad_desc  This routine calculates the critical current density and
    !+ad_desc  temperature in superconducting TF coils using NbTi
    !+ad_desc  as the superconductor.
    !+ad_prob  Results will be misleading unless bmax < bc20max, and
    !+ad_prob  thelium is sufficiently low.
    !+ad_call  None
    !+ad_hist  09/10/14 PJK Initial version, taken from inline code in supercon
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: thelium, bmax, c0, bc20max, tc0max
    real(kind(1.0D0)), intent(out) :: jcrit, tcrit

    !  Local variables

    real(kind(1.0D0)) :: bratio, tbar

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    bratio = min(bmax/bc20max, 0.9999D0)  !  avoids NaNs below

    !  Critical temperature (K)

    tcrit = tc0max * (1.0D0 - bratio)**0.59D0

    tbar = max((1.0D0 - thelium/tcrit), 0.001D0)

    !  Critical current density (A/m2)

    jcrit = c0 * (1.0D0 - bratio) * tbar

  end subroutine jcrit_nbti

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bi2212(bmax,jstrand,tsc,fhts,jcrit,tmarg)

    !+ad_name  bi2212
    !+ad_summ  Fitted parameterization to Bi-2212 superconductor properties
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  bmax    : input real : Magnetic field at conductor (T)
    !+ad_args  jstrand : input real : Current density in strand (A/m2)
    !+ad_args  tsc     : input real : Superconductor temperature (K)
    !+ad_args  fhts    : input real : Adjustment factor (<= 1) to account for strain,
    !+ad_argc                         radiation damage, fatigue or AC losses
    !+ad_args  jcrit : output real : Critical current density in strand (A/m2)
    !+ad_args  tmarg : output real : Temperature margin (K)
    !+ad_desc  This routine calculates the critical current density and
    !+ad_desc  the temperature margin for Bi-2212 superconductor in the TF coils
    !+ad_desc  using a fit by M. Kovari to measurements described in the reference,
    !+ad_desc  specifically from the points shown in Figure 6.
    !+ad_desc  <P>Bi-2212 (Bi<SUB>2</SUB>Sr<SUB>2</SUB>CaCu<SUB>2</SUB>O<SUB>8-x</SUB>)
    !+ad_desc  is a first-generation high temperature superconductor; it still needs
    !+ad_desc  to be operated below about 10K, but remains superconducting at much
    !+ad_desc  higher fields at that temperature than Nb3Sn etc.
    !+ad_desc  The model's range of validity is T &lt; 20K, adjusted field
    !+ad_desc  b &lt; 104 T, B &gt; 6 T.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  08/10/13 PJK Initial version
    !+ad_hist  05/03/14 PJK Added comment about range of validity
    !+ad_hist  06/03/14 PJK Added warning if range of validity is violated
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  A transformative superconducting magnet technology for fields well
    !+ad_docc  above 30 T using isotropic round wire multifilament
    !+ad_docc  Bi2Sr2CaCu2O8-x conductor, D. C. Larbalestier et al., preprint,
    !+ad_docc  9th April 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: bmax, jstrand, tsc, fhts
    real(kind(1.0D0)), intent(out) :: jcrit, tmarg

    !  Local variables

    real(kind(1.0D0)) :: b, tcrit

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Adjusted field (T)

    b = bmax / exp(-0.168D0*(tsc-4.2D0))

    !  Engineering (i.e. strand) critical current density (A/m2)

    jcrit = fhts * (1.175D9*exp(-0.02115D0*b) - 1.288D8)

    !  Temperature margin (K)
    !  Simple inversion of above calculation, using actual current density
    !  in strand instead of jcrit

    tmarg = 1.0D0/0.168D0 * &
         log( log(1.175D9/(jstrand/fhts + 1.288D8)) / (0.02115*bmax) ) &
         + 4.2D0 - tsc

    !  Check if ranges of validity have been violated

    if ((tsc > 20.0D0).or.(bmax < 6.0D0).or.(b > 104.0D0)) then
       fdiags(1) = tsc ; fdiags(2) = bmax ; fdiags(3) = b
       call report_error(106)
       write(*,*) 'Warning in routine BI2212:'
       write(*,*) 'Range of validity of the HTS Bi-2212 model has been violated:'
       write(*,*) '   S/C temperature (K) = ',tsc, ' (should be < 20 K)'
       write(*,*) 'Field at conductor (T) = ',bmax, ' (should be > 6 T)'
       write(*,*) '    Adjusted field (T) = ',b, ' (should be < 104 T)'
       write(*,*) ' '
    end if

  end subroutine bi2212

end module sctfcoil_module
