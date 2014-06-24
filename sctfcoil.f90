! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module sctfcoil_module

  !+ad_name  sctfcoil_module
  !+ad_summ  Module containing superconducting TF coil routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  coilshap
  !+ad_cont  edoeeff
  !+ad_cont  eyngeff
  !+ad_cont  eyngzwp
  !+ad_cont  myall_stress
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
  !+ad_call  fwbs_variables
  !+ad_call  maths_library
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_stat  Okay
  !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use fwbs_variables
  use maths_library
  use physics_variables
  use process_output
  use tfcoil_variables

  private
  public :: outtf, sctfcoil, stresscl, tfcind, tfspcall

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
    !+ad_call  tfcind
    !+ad_call  stresscl
    !+ad_call  outtf
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    integer :: i
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
    ! PJK 08/05/14 Uncomment the following line to calculate tftort
    ! rather than use it as an input quantity
    !tftort = 2.0D0 * rcoil*sin(thtcoil)

    !  Cross-sectional area of outboard leg
    !assumed same width as inboard leg

    arealeg = tfthko*tftort

    !  Annular area of midplane containing TF coil inboard legs

    tfareain = pi * (rcoil**2 - rcoilp**2) 

    !  Total current in TF coils

    ritfc = oacdcp * tfareain
    if (ritfc < 0.0D0) then
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'TF coil current is negative, ritfc = ',ritfc
       write(*,*) '   Overall current density, oacdcp = ',oacdcp
       write(*,*) '    Cross-sectional area, tfareain = ',tfareain
       write(*,*) ' '
       write(*,*) ' PROCESS continuing, ritfc forced to be +ve...'
       ritfc = abs(ritfc)  !  changed from 1.0D0
    end if

    !  Peak toroidal field and radius of its occurrence,
    !  assumed to be at the outer edge of the winding pack

    rbmax = rcoil - casthi
    bmaxtf = 2.0D-7 * ritfc / rbmax

    !  Peak field including ripple (assumed 9%)

    bmaxtfrp = bmaxtf * 1.09D0

    !  Calculation of forces : centering and vertical

    cforce = bmaxtf*ritfc/(2.0D0*tfno)
    vforce = 0.5D0 * bt * rmajor * 0.5D0*ritfc * &
         log(rtot/rtfcin) / tfno

    !  Horizontal and vertical bores

    tfboreh = rtot - rtfcin - 0.5D0*(tfthko + tfcth)
    borev = (hpfu - tfcth) + hmax

    !  The rest of this routine deals with superconducting coils.

    !  Define coil shape

    call coilshap

    !  Calculation of TF coil magnetic energy

    call tfcind(tfcth)

    !  Find TF coil energy (GJ)

    estotf = 1.0D-9 *  0.5D0*tfind / tfno * ritfc**2

    !  Coil inside perimeter

    tfleng = 0.0D0
    do i = 1,2
       tfleng = tfleng + 2.0D0*(radctf(i) + 0.5D0*tfcth) * dthet(i)
    end do
    do i = 3,4
       tfleng = tfleng + 2.0D0*(radctf(i) + 0.5D0*tfthko) * dthet(i)
    end do

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
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'Negative winding pack thickness:        thkwp (m) =',thkwp
       write(*,*) 'Inboard TF coil thickness is too small: tfcth (m) =',tfcth
       write(*,*) 'Increase tfcth or make it an iteration variable (13) with'
       write(*,*) 'a lower bound, or reduce the case and insulation thicknesses'
       write(*,*) 'or their upper bounds:'
       write(*,*) 'thkcas=',thkcas,' casthi=',casthi
       write(*,*) 'casths=',casths,' tinstf=',tinstf
       write(*,*) ' '
       !write(*,*) 'PROCESS stopping.'
       !goto 20
    end if

    !  Radius of geometrical centre of winding pack

    radwp = rcoil - casthi - tinstf - 0.5D0*thkwp

    !  Thickness of winding pack section at R > radwp

    wwp1 = 2.0D0 * (radwp*tant - casths - tinstf)

    !  Thickness of winding pack section at R < radwp

    wwp2 = 2.0D0 * ((radwp-0.5D0*thkwp)*tant - casths - tinstf)

    !  Total cross-sectional area of winding pack

    awptf = (0.5D0*thkwp)*(wwp1 + wwp2)

    !  Total cross-sectional area of winding pack, including insulation

    awpc = 0.5D0*thkwp*(wwp2 + 2.0D0*tinstf) + &
         (0.5D0*thkwp + 2.0D0*tinstf)*(wwp1 + 2.0D0*tinstf)

    !  Total cross-sectional area of surrounding case

    acasetf = (tfareain/tfno) - awpc

    if ((awptf <= 0.0D0).or.(awpc <= 0.0D0).or.(acasetf <= 0.0D0)) then
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'Winding pack cross-section problem'
       write(*,*) 'awptf = ',awptf
       write(*,*) 'awpc = ',awpc
       write(*,*) 'acasetf = ',acasetf
       write(*,*) ' '
       !write(*,*) 'PROCESS stopping.'
       !goto 20
    end if

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
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'Cable space dimension, leni = ',leni
       write(*,*) 'Reduce conduit case or insulation thicknesses,'
       write(*,*) 'or increase cpttf value or lower bound.'
       write(*,*) ' '
       !write(*,*) 'PROCESS stopping.'
       !goto 20
    end if

    !  Cross-sectional area of cable space per turn

    acstf = leni**2 - (4.0D0-pi)*rbcndut**2

    if (acstf <= 0.0D0) then
       if (leni < 0.0D0) then
          write(*,*) 'Warning in routine SCTFCOIL:'
          write(*,*) '    Cable space area, acstf = ',acstf
          write(*,*) 'Cable space dimension, leni = ',leni
          write(*,*) ' '
       else
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

    !  Check of outboard leg toroidal thickness, tftort
    !  Must be thicker than the width of the winding pack etc., which
    !  is likely to be the same width as at the inboard side

    if ( (tftort < (wwp1 + 2.0D0*tinstf)).and.(iprint == 1) ) then
       write(*,*) 'Warning in routine SCTFCOIL:'
       write(*,*) '  TF outboard leg toroidal thickness, tftort = ',tftort
       write(*,*) '             Winding pack + insulation width = ', &
            wwp1 + 2.0D0*tinstf
       write(*,*) 'Consider raising tftort in input file,'
       write(*,*) 'or setting it as an iteration variable (77)'
       write(*,*) 'and turning on constraint 57 and iteration variable 99'
       write(*,*) 'to force the coil to be wide enough.'
    end if

    !  TF Coil areas and masses

    !  Surface areas (for cryo system)

    wbtf = rcoil*sin(thtcoil) - rcoilp*tant
    tfocrn = rcoilp * tant
    tficrn = tfocrn + wbtf
    tfsai = 4.0D0 * tfno * tficrn * hr1
    tfsao = 2.0D0 * tfno * tficrn * (tfleng - 2.0D0*hr1)

    !  Mass of case

    !  The length of the vertical section is that of the first (inboard) segment

    cplen = 2.0D0*(radctf(1) + 0.5D0*tfcth) * dthet(1)

    !  The outboard to inboard case area ratio is casfact
    !  The 1.4 factor is a scaling factor to fit to the ITER-FDR value
    !  of 450 tonnes; see CCFE note T&M/PKNIGHT/PROCESS/022

    whtcas = 1.4D0 * dcase * acasetf * ( cplen + (tfleng-cplen)*casfact )

    !  Masses of conductor constituents:

    !  Superconductor

    whtconsc = tfleng * turnstf * acstf*(1.0D0-vftf) * &
         (1.0D0-fcutfsu)*dcond(isumattf)

    !  Copper

    whtconcu = tfleng * turnstf * acstf*(1.0D0-vftf) * &
         fcutfsu*dcopper

    !  Steel conduit (sheath)

    whtconsh = tfleng * turnstf * acndttf * denstl

    !  Total conductor mass

    whtcon = whtconsc + whtconcu + whtconsh

    !  Total TF coil mass

    whttf = (whtcas+whtcon) * tfno

    !  Do stress calculations

    call stresscl

    if (iprint == 1) call outtf(outfile)

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
    !+ad_call  sctfjalw
    !+ad_call  sigvm
    !+ad_call  myall_stress
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
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: dummyv,seff,svmxz,svmyz,tcbs,fac

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

    if (tfc_model == 1) then
       !  Myall 5-layer model
       !  Layers are labelled from outboard to inboard
       !  Layers 1,2,3 are the winding pack, layers 4,5 are the inboard steel casing
       !  (although the 5th layer is of zero thickness)

       radtf(1) = rbmax

       do i = 1,3
          eyoung(i) = eyngeff(tfc_model,eystl,eyins,eywp,eyrp,trp,thicndut,seff, &
               thwcndut,tcbs)
          radtf(i+1) = radtf(i) - thkwp/3.0D0
          dummyv = max(0.001D0, (1.0D0 - casths*tfno/(pi*radtf(i)) ) )
          jeff(i) = jwptf * dummyv
       end do

       !  Outer ring section (actually inner ring...)

       do i = 4,5
          radtf(i+1) = rtfcin - 0.5D0*tfcth
          eyoung(i) = eystl
          jeff(i) = 0.0D0
       end do

       !  Call Myall stress routine

       call myall_stress(poisson,radtf,eyoung,jeff,sigrtf,sigttf,deflect)

    else if (tfc_model == 2) then
       !  CCFE two-layer model
       !  Layers are labelled from inboard to outboard
       !  The first layer is the steel casing inboard of the winding pack,
       !  while the second layer is the winding pack itself

       radtf(1) = rtfcin - 0.5D0*tfcth
       radtf(2) = rbmax - thkwp
       radtf(3) = rbmax

       eyoung(1) = eystl
       eyoung(2) = eyngeff(tfc_model,eystl,eyins,eywp,eyrp,trp,thicndut,seff, &
            thwcndut,tcbs)

       jeff(1) = 0.0D0
       jeff(2) = jwptf

       !  Call stress routine

       call two_layer_stress(poisson,radtf(1:3),eyoung(1:2),jeff(1:2), &
            sigrtf(1:2),sigttf(1:2),deflect)

    else  !  should never get here
       write(*,*) 'Error in routine STRESSCL:'
       write(*,*) 'Model not accounted for; tfc_model = ',tfc_model
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Convert to conduit + case

    fac = eystl*eyins*seff / &
         (eyins*(seff-2.0D0*thicndut) + 2.0D0*thicndut*eystl)

    if (tfc_model == 1) then
       sigrcon = sigrtf(3)/eyoung(3) * fac
       sigtcon = sigttf(3)/eyoung(3) * fac
    else if (tfc_model == 2) then
       sigrcon = sigrtf(2)/eyoung(2) * fac
       sigtcon = sigttf(2)/eyoung(2) * fac
    end if

    sigvert = vforce / (acasetf + acndttf*turnstf + arp)

    !  Find case strain

    casestr = sigvert / eystl

    !  Find Von-Mises stresses

    if (tfc_model == 1) then

       !  Conduit walls (take worst case of 2 walls)

       svmxz = sigvm(sigrcon, 0.0D0, sigvert, 0.0D0,0.0D0,0.0D0)
       svmyz = sigvm(0.0D0, sigtcon, sigvert, 0.0D0,0.0D0,0.0D0)
       strtf1 = max(svmxz,svmyz)

       !  Case

       strtf2 = sigvm(sigrtf(5), sigttf(5), sigvert, 0.0D0,0.0D0,0.0D0)

    else if (tfc_model == 2) then
       strtf1 = sigvm(sigrcon, sigtcon, sigvert, 0.0D0,0.0D0,0.0D0)
       strtf2 = sigvm(sigrtf(1), sigttf(1), sigvert, 0.0D0,0.0D0,0.0D0)
    end if

    !  Young's modulus and strain in vertical direction on winding pack

    if (tfc_model == 2) then
       eyzwp = eyngzwp(eystl,eyins,eywp,eyrp,trp,thicndut,seff,thwcndut,tcbs)
       windstrain = sigvert / eyzwp
    end if

    !  Radial strain in insulator

    if (tfc_model == 2) then
       insstrain = sigrtf(2) / eyins * &
            edoeeff(eystl,eyins,eywp,eyrp,trp,thicndut,seff,thwcndut,tcbs)
    end if

  end subroutine stresscl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine myall_stress(nu,rad,ey,j,sigr,sigt,deflect)

    !+ad_name  myall_stress
    !+ad_summ  Calculates the stresses in a superconductor TF coil
    !+ad_summ  inboard leg at the midplane
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  nu      : input real : Poisson's ratio (assumed constant over entire coil)
    !+ad_args  rad(6)  : input real array : Radius points of regions (m)
    !+ad_argc                 (region i is bounded by rad(i) and rad(i+1) )
    !+ad_args  ey(5)   : input real array : Effective Young's modulus of region i (Pa)
    !+ad_args  j(5)    : input real array : Effective current density of region i (A/m2)
    !+ad_args  sigr(5) : output real array : Radial stress in region i (Pa)
    !+ad_args  sigt(5) : output real array : Tangential stress in region i (Pa)
    !+ad_args  deflect : output real : Deflection at point rad(6) (m)
    !+ad_desc  This routine calculates the stresses in a superconductor TF coil
    !+ad_desc  inboard leg at midplane.
    !+ad_desc  <P>The analysis of J. Myall (Aug. 1987) is followed (obtained from
    !+ad_desc  J. Miller of LLNL 5/91).
    !+ad_desc  This model allows 5 regions in the coil, so graded conductors
    !+ad_desc  are possible. Regions 1-3 are the winding pack regions (going
    !+ad_desc  from high to low field), and regions 4-5 are the solid steel ring
    !+ad_desc  and intermittent steel ring regions, respectively.
    !+ad_desc  A conventional nongraded ring can be modelled by inputting the
    !+ad_desc  same values of ey and j for regions 1-3, and regions 4-5.
    !+ad_prob  This routine is very sensitive to code changes.
    !+ad_prob  Simple (correct) reordering of assignment statements or changing
    !+ad_prob  <CODE>else if (i == 3) then</CODE> to simply <CODE>else</CODE>
    !+ad_prob  causes the output values to change.
    !+ad_call  linesolv
    !+ad_hist  09/05/91 JG  Initial version
    !+ad_hist  10/05/12 PJK Initial F90 version;
    !+ad_hisc               Converted from integer function to subroutine
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  30/04/14 PJK/JM Added two-layer model
    !+ad_hist  07/05/14 PJK Corrections from comparison with JM's python test code
    !+ad_hist  08/05/14 PJK Changed routine name from <CODE>tfstress</CODE>;
    !+ad_hisc               moved CCFE model into new routine
    !+ad_stat  Okay
    !+ad_docs  Myall_1987_TF_stress_calc.pdf; scanned copy of Myall's original notes
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: nu
    real(kind(1.0D0)), dimension(6), intent(in) :: rad
    real(kind(1.0D0)), dimension(5), intent(in) :: ey, j
    real(kind(1.0D0)), dimension(5), intent(out) :: sigr, sigt
    real(kind(1.0D0)), intent(out) :: deflect

    !  Local variables

    integer :: i, ii, ia
    real(kind(1.0D0)) :: b03,b05,b07,k2,k3,kk1,kk2,l4,l5,m6,m7, &
         n8,n9,nufac,p1,p2,p3,q2,q3,q4,q5,r4,r5,r6,r7,s6,s7,s8,s9, &
         t10,t8,t9
    real(kind(1.0D0)), dimension(10,10) :: a
    real(kind(1.0D0)), dimension(10) :: b, c

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set up LHS matrix A elements (Myall notation)

    nufac = (1.0D0 - nu)/(1.0D0 + nu)
    t10 = -nufac / rad(6)**2
    n9 = -ey(5) / ey(4)
    n8 = -1.0D0
    s9 = -nufac / rad(5)**2
    s8 = 1.0D0 / rad(5)**2
    t9 = n9 * s9
    t8 = -s8
    m7 = -ey(4) / ey(3)
    m6 = -1.0D0
    r7 = -nufac / rad(4)**2
    r6 = 1.0D0 / rad(4)**2
    s7 = m7*r7
    s6 = -r6
    l5 = -ey(3) / ey(2)
    l4 = -1.0D0
    q5 = -nufac / rad(3)**2
    q4 = 1.0D0 / rad(3)**2
    r5 = l5 * q5
    r4 = -q4
    k3 = -ey(2) / ey(1)
    k2 = -1.0D0
    p3 = -nufac / rad(2)**2
    p2 = 1.0D0 / rad(2)**2
    q3 = k3 * p3
    q2 = -p2
    p1 = -nufac / rad(1)**2

    !  RHS vector b :

    b(10) = 0.0D0
    b(9) = 0.0D0
    b(8) = 0.0D0

    b07 = rmu0 * (1.0D0-nu**2) * j(3)**2 * rad(4)**2/(16.0D0 * ey(3))
    b(7) = b07 * (4.0D0 * log(rad(4)) + nufac )

    b(6) = b07 * (4.0D0 * log(rad(4)) - 1.0D0)

    b05 = rmu0 * (1.0D0-nu**2) * j(2)**2 * rad(3)**2/(16.0D0 * ey(2))
    b(5) = b05 * ( (1.0D0 - j(3)/j(2) + j(3)/j(2)* &
         (rad(4)/rad(3))**2 - (j(3)/j(2))**2* &
         (rad(4)/rad(3))**2 ) * ( 4.0D0*log(rad(3)) &
         + 4.0D0/(1.0D0+nu) ) - &
         ( 1.0D0-( j(3)/j(2) )**2 ) * (3.0D0+nu)/(1.0D0+nu) )

    b(4) = b05 * ( ( 1.0D0 - j(3)/j(2) + j(3)/j(2) * &
         (rad(4)/rad(3))**2 - (j(3)/j(2))**2 * &
         (rad(4)/rad(3))**2*ey(2)/ey(3) )*4.0D0*log(rad(3)) - &
         ( 1.0D0 - (j(3)/j(2))**2*ey(2)/ey(3) ) )

    b03 = rmu0 * (1.0D0 - nu**2) * j(1)**2 * rad(2)**2/ (16.0D0*ey(1) )
    b(3) = b03 * ( ( 1.0D0 - j(2)/j(1) + (j(2)/j(1) - &
         j(3)/j(1) ) * (rad(3)/rad(2))**2 + j(3)/j(1) * &
         (rad(4)/rad(2))**2 - (j(2)/j(1))**2 * (rad(3)/rad(2))**2 &
         + j(3)/j(1)*j(2)/j(1)*(rad(3)/rad(2))**2 - &
         j(2)/j(1)*j(3)/j(1)*(rad(4)/rad(2))**2 ) * &
         ( 4.0D0*log(rad(2)) + 4.0D0/(1.0D0+nu) ) &
         - (1.0D0 - (j(2)/j(1) )**2 ) * (3.0D0 + nu)/(1.0D0 + nu) )

    b(2) = b03 * ( ( 1.0D0 - j(2)/j(1) + ( j(2)/j(1) - j(3)/j(1) ) * &
         (rad(3)/rad(2))**2 + j(3)/j(1)*( rad(4)/rad(2) )**2 - &
         ey(1)/ey(2)*( j(2)/j(1) )**2 * ( rad(3)/rad(2) )**2  + &
         ey(1)/ey(2) * j(2)/j(1)* j(3)/j(1) * (rad(3)/rad(2) )**2 &
         - ey(1)/ey(2) * j(2)/j(1) * j(3)/j(1) *(rad(4)/rad(2) )**2 ) &
         * 4.0D0*log(rad(2)) - ( 1.0D0-ey(1)/ey(2)*(j(2)/j(1))**2 ) )

    b(1) = b03 * ( ( 1.0D0 - j(2)/j(1) + ( j(2)/j(1) - j(3)/j(1) ) * &
         (rad(3)/rad(2))**2 + j(3)/j(1) * (rad(4)/rad(2) )**2 ) &
         * (4.0D0*log(rad(1)) + 4.0D0/(1.0D0+nu) ) - &
         (3.0D0+nu)/(1.0D0+nu) * ( rad(1) / rad(2) )**2 )

    !  LHS matrix A :

    a(:,:) = 0.0D0

    a(1,1) = 1.0D0
    a(1,6) = p1
    a(2,1) = 1.0D0
    a(2,2) = k2
    a(2,6) = p2
    a(2,7) = q2
    a(3,1) = 1.0D0
    a(3,2) = k3
    a(3,6) = p3
    a(3,7) = q3
    a(4,2) = 1.0D0
    a(4,3) = l4
    a(4,7) = q4
    a(4,8) = r4
    a(5,2) = 1.0D0
    a(5,3) = l5
    a(5,7) = q5
    a(5,8) = r5
    a(6,3) = 1.0D0
    a(6,4) = m6
    a(6,8) = r6
    a(6,9) = s6
    a(7,3) = 1.0D0
    a(7,4) = m7
    a(7,8) = r7
    a(7,9) = s7
    a(8,4) = 1.0D0
    a(8,5) = n8
    a(8,9) = s8
    a(8,10) = t8
    a(9,4) = 1.0D0
    a(9,5) = n9
    a(9,9) = s9
    a(9,10) = t9
    a(10,5) = 1.0D0
    a(10,10) = t10

    !  Find solution vector c:  A times c = b

    ia = 10 ; call linesolv(a,ia,b,c)

    !  Find stresses in winding pack region

    do i = 1,3

       if (i == 1) then
          kk2 = rmu0 * (1.0D0 - nu**2) * j(i) / (2.0D0 * ey(i) ) * &
               ( (j(1) - j(2) )*(rad(2))**2 + (j(2) - j(3)) * &
               (rad(3))**2 + j(3)*(rad(4))**2 )
       else if (i == 2) then
          kk2 = rmu0 * (1.0D0 - nu**2) * j(i) / (2.0D0 * ey(i) ) * &
               ( (j(2) - j(3)) * (rad(3))**2 + j(3)*(rad(4))**2 )
       else if (i == 3) then  !  Truncating after the 'else' causes results to change!
          kk2 = rmu0 * (1.0D0 - nu**2) * j(i) / (2.0D0 * ey(i) ) * &
               j(3)*(rad(4))**2
       end if

       kk1 = rmu0 * (1.0D0 - nu**2) * j(i)**2 / (2.0D0 * ey(i))

       sigt(i) = ey(i) / (1.0D0 - nu**2) * ( (1.0D0+nu) * c(i) + &
            (1.0D0-nu) * c(5+i)/(rad(i+1))**2 + &
            (1.0D0+3.0D0*nu)/8.0D0*kk1*rad(i+1)**2 &
            - (1.0D0+nu)/2.0D0*kk2*log(rad(i+1)) - nu*kk2/2.0D0 )

       sigr(i) = ey(i) / (1.0D0 - nu**2) * ( (1.0D0+nu) * c(i) - &
            (1.0D0-nu) * c(5+i)/(rad(i+1))**2 + &
            (3.0D0+nu)/8.0D0*kk1*(rad(i+1))**2 &
            - (1.0D0+nu)/2.0D0*kk2*log(rad(i+1)) - kk2/2.0D0 )

    end do

    !  Stress in rings
    !  In practice, sigr(4) and sigr(5) are always close to zero...

    do i = 4,5
       sigr(i) = ey(i) / (1.0D0 - nu**2) * ( (1.0D0+nu)*c(i) - &
            (1.0D0-nu) * c(5+i)/(rad(i+1))**2 )
       sigt(i) = ey(i) / (1.0D0 - nu**2) * ( (1.0D0+nu)*c(i) + &
            (1.0D0-nu) * c(5+i)/(rad(i+1))**2 )
    end do

    !  Deflection

    deflect = c(5) * rad(6) + c(10)/rad(6)

  end subroutine myall_stress

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

  function eyngeff(model,estl,eins,ewp,erp,trp,tins,teff,tstl,tcs)

    !+ad_name  eyngeff
    !+ad_summ  Finds the effective Young's modulus of the TF coil winding pack
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  model : input integer : stress model; 1 = original, 2 = two-layer
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
    !+ad_desc  <P>Model 1 was programmed by J. Galambos from a Lotus spreadsheet
    !+ad_desc  by J. Miller.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/91 JG  Initial version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  30/04/14 PJK/JM Modifications for two-layer stress model
    !+ad_hist  07/05/14 PJK Changed trp comment; modified new model so that
    !+ad_hisc               only conduit and radial plate regions contribute
    !+ad_stat  Okay
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: eyngeff

    !  Arguments

    integer, intent(in) :: model
    real(kind(1.0D0)), intent(in) :: estl,eins,ewp,erp,trp,tins,teff,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: ed,tcond,ttot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (model == 1) then

       !  Thickness of cable area + conduit

       tcond = teff - 2.0D0 * tins

       eyngeff = 2.0D0*eins*tins/teff + &
            2.0D0*eins*estl*tstl / (eins*tcond + 2.0D0*tins*estl) + &
            estl*eins*ewp*tcs / (estl*eins*tcs + 2.0D0*tins*eins*estl + &
            2.0D0*tins*estl*ewp)
    else

       !  Total thickness of a turn with radial plates

       ttot = tcs + 2.0D0*(trp + tins + tstl)

       !  See Figure 8 and Section III.4, Morris

       ed = ttot / (2.0D0*trp/erp + 2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)

       eyngeff = 1.0D0/ttot * ( 2.0D0*trp*erp + 2.0D0*tstl*ed )

    end if

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

  subroutine outtf(outfile)

    !+ad_name  outtf
    !+ad_summ  Writes superconducting TF coil output to file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine writes the superconducting TF coil results
    !+ad_desc  to the output file.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

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
    call ovarre(outfile,'Stored energy per coil (GJ)','(estotf)',estotf)
    call ovarre(outfile,'Total mass of TF coils (kg)','(whttf)',whttf)
    call ovarre(outfile,'Vertical separating force per coil (N)','(vforce)',vforce)
    call ovarre(outfile,'Centering force per coil (N/m)','(cforce)',cforce)

    call osubhd(outfile,'Coil Geometry :')
    call ovarre(outfile,'Inboard leg centre radius (m)','(rtfcin)',rtfcin)
    call ovarre(outfile,'Outboard leg centre radius (m)','(rtot)',rtot)
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax)
    call ovarre(outfile,'Clear bore (m)','(tfboreh)',tfboreh)
    call ovarre(outfile,'Clear vertical bore (m)','(borev)',borev)

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
    call ovarre(outfile,'Total conductor cable mass per coil (kg)','(whtcon)',whtcon)
    call ovarre(outfile,'Cable conductor + void area (m2)','(acstf)',acstf)
    call ovarre(outfile,'Cable space coolant fraction','(vftf)',vftf)
    call ovarre(outfile,'Conduit case thickness (m)','(thwcndut)',thwcndut)
    call ovarre(outfile,'Cable insulation thickness (m)','(thicndut)',thicndut)
    call ovarre(outfile,'Cable radial/toroidal aspect ratio','(aspcstf)',aspcstf)

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
    call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
    call ovarre(outfile,'Number of turns per TF coil','(turnstf)',turnstf)
    call ovarre(outfile,'Current per turn (A)','(cpttf)',cpttf)

    call osubhd(outfile,'External Case Information :')

    call ovarre(outfile,'Inboard leg case outboard thickness (m)','(casthi)',casthi)
    call ovarre(outfile,'Inboard leg case inboard thickness (m)','(thkcas)',thkcas)
    call ovarre(outfile,'Inboard leg case toroidal thickness (m)','(casths)',casths)
    call ovarre(outfile,'Inboard leg case area per coil (m2)','(acasetf)',acasetf)
    call ovarre(outfile,'Outboard leg case area per coil (m2)', &
         '(...*casfact)',acasetf*casfact)
    call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas)

    if (tfc_model == 1) then
       call osubhd(outfile,'TF Coil Stresses (Myall 5-layer model) :')
    else if (tfc_model == 2) then
       call osubhd(outfile,'TF Coil Stresses (CCFE two-layer model) :')
    end if
    call ovarin(outfile,'TF coil model','(tfc_model)',tfc_model)
    call ovarre(outfile,'Vertical stress (Pa)','(sigvert)',sigvert)
    call ovarre(outfile,'Conduit radial stress (Pa)','(sigrcon)',sigrcon)
    call ovarre(outfile,'Conduit tangential stress (Pa)','(sigtcon)',sigtcon)
    call ovarre(outfile,'Conduit Von Mises combination stress (Pa)','(strtf1)',strtf1)
    if (tfc_model == 1) then
       call ovarre(outfile,'Case radial stress (Pa)','(sigrtf(5))',sigrtf(5))
       call ovarre(outfile,'Case tangential stress (Pa)','(sigttf(5))',sigttf(5))
    else if (tfc_model == 2) then
       call ovarre(outfile,'Case radial stress (Pa)','(sigrtf(1))',sigrtf(1))
       call ovarre(outfile,'Case tangential stress (Pa)','(sigttf(1))',sigttf(1))
    end if
    call ovarre(outfile,'Case Von Mises combination stress (Pa)','(strtf2)',strtf2)
    call ovarre(outfile,'Allowable stress (Pa)','(alstrtf)',alstrtf)
    call ovarre(outfile,'Deflection at midplane (m)','(deflect)',deflect)
    if (tfc_model == 2) then
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
    !+ad_cont  itersc
    !+ad_cont  bi2212
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
      !+ad_args  bcritsc : input real : Critical field (T) (isumat=4 only)
      !+ad_args  tcritsc : input real : Critical temperature (K) (isumat=4 only)
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
      !+ad_call  oblnkl
      !+ad_call  ocmmnt
      !+ad_call  oheadr
      !+ad_call  osubhd
      !+ad_call  ovarre
      !+ad_call  protect
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

      real(kind(1.0D0)) :: bc20m,c0,fac1,fac2,fcond,icrit,iooic,jc, &
           jstrand,jwdgop,tbar,tc0m,tc1

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Conductor fraction

      fcond = 1.0D0 - fhe

      !  Find critical current density

      select case (isumat)

      case (1)  !  ITER Nb3Sn critical surface parametrization
         bc20m = 32.97D0  !  previously 28.0D0 for old model
         tc0m = 16.06D0   !  previously 18.0D0 for old model
         !c0 = 1.315D10   !  obsolete

      case (2)  !  Bi-2212 high temperature superconductor parametrization
	      continue  !  ...see below

      case (3)  !  NbTi data
         bc20m = 15.0D0
         tc0m = 9.3D0
         c0 = 10.0D9

      case (4)  !  As (1), but user-defined parameters
         bc20m = bcritsc
         tc0m = tcritsc

      case default  !  Error condition
         write(*,*) 'Error in routine SUPERCON:'
         write(*,*) 'Illegal value for isumattf, = ',isumattf
         write(*,*) ' '
         write(*,*) 'PROCESS stopping.'
         stop

      end select

      !  Calculate critical current density and temperature

      select case (isumat)

      case (1,4)  !  ITER Nb3Sn critical surface model

         call itersc(thelium,bmax,strain,bc20m,tc0m,jc,tc1)

      case (2)  !  Bi-2212 high temperature superconductor model

         !  Current density in a strand of Bi-2212 conductor
         !  N.B. the parametrization for jc assumes a particular strand composition
         !  that does not require a user-defined copper fraction, so this is
         !  irrelevant in this model

         jstrand = jwp * acs*(1.0D0-fhe)/aturn

         call bi2212(bmax,jstrand,thelium,fhts,jc,tmarg)

      case (3)  !  NbTi model

         tc1 = tc0m * (1.0D0 - bmax/bc20m)**0.59D0
         tc1 = max(tc1, 0.001D0)
         tbar = max((1.0D0 - thelium/tc1), 0.001D0)
         jc = c0 * (1.0D0 - bmax/bc20m) * tbar

      end select

      !  Critical current

      if (isumat /= 2) then
         icrit = jc * acs * (1.0D0 - fhe) * (1.0D0 - fcu)
      else
         icrit = jc * acs * (1.0D0 - fhe)  !  see comment above
      end if

      !  Critical current density in winding pack

      jwdgcrt = icrit / aturn

      !  Ratio of operating / critical current

      iooic = iop / icrit

      !  Operating current density

      jwdgop = iop / aturn

      !  Temperature margin (already calculated in bi2212 for isumat=2)

      if (isumat /= 2) then
         fac1 = max( 0.01D0, (1.0D0 - iooic) )
         fac2 = max( 0.01D0, (tc1-thelium) )
         tmarg = fac1 * fac2
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

      call osubhd(outfile,'Critical Current Information :')
      if (isumat /= 2) then
         call ovarre(outfile,'Critical field at zero temp., strain (T)', &
              '(bc20m)',bc20m)
         call ovarre(outfile,'Critical temp. at zero field, strain (K)', &
              '(tc0m)',tc0m)
      end if
      call ovarre(outfile,'Operating winding pack J (A/m2)','(jwdgop)',jwdgop)
      call ovarre(outfile,'Critical winding pack curr. density (A/m2)', &
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

    subroutine itersc(thelium,bmax,strain,bctw,tco,jcrit,tcrit)

      !+ad_name  itersc
      !+ad_summ  Implementation of ITER Nb3Sn critical surface implementation
      !+ad_type  Subroutine
      !+ad_auth  R Kemp, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  thelium : input real : Coolant/SC temperature (K)
      !+ad_args  bmax : input real : Magnetic field at conductor (T)
      !+ad_args  strain : input real : Strain in superconductor
      !+ad_args  bctw : input real : Upper critical field (T) for superconductor
      !+ad_argc                      at zero temperature and strain
      !+ad_args  tco : input real : Critical temperature (K) at zero field and strain
      !+ad_args  jcrit : output real : Critical current density (A/m2)
      !+ad_args  tcrit : output real : Critical temperature (K)
      !+ad_desc  This routine calculates the critical current density and
      !+ad_desc  temperature in the superconducting TF coils using the
      !+ad_desc  ITER Nb3Sn critical surface model.
      !+ad_prob  None
      !+ad_call  None
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
      !+ad_stat  Okay
      !+ad_docs  ITER Nb3Sn critical surface parametrization (2MMF7J) (2008),
      !+ad_docc    https://user.iter.org/?uid=2MMF7J&action=get_document
      !+ad_docs  ITER DDD 11-7: Magnets - conductors (2NBKXY) (2009),
      !+ad_docc    https://user.iter.org/?uid=2NBKXY&action=get_document
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: thelium, bmax, strain, bctw, tco
      real(kind(1.0D0)), intent(out) :: jcrit, tcrit

      !  Local variables

      real(kind(1.0D0)), parameter :: csc = 16500.0D6
      real(kind(1.0D0)), parameter :: cp = 0.63D0
      real(kind(1.0D0)), parameter :: cq = 2.1D0
      real(kind(1.0D0)), parameter :: caone = 44.0D0
      real(kind(1.0D0)), parameter :: catwo = 4.0D0
      real(kind(1.0D0)), parameter :: etaoa = 0.00256D0
      real(kind(1.0D0)), parameter :: etamax = -0.003253075D0
      real(kind(1.0D0)), parameter :: diter = 0.82D0  !  ITER strand diameter (mm)
      real(kind(1.0D0)), parameter :: cuiter = 0.5D0  !  ITER strand copper fraction

      real(kind(1.0D0)) :: tred, bcrit, bred, etash, tzero, bcro, &
           tcro, bzero, strfun, jc1, jc2, jc3, scalefac

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Strain function

      etash = (catwo*etaoa)/(sqrt(caone**2 - catwo**2))
      strfun = sqrt(etash**2 + etaoa**2) - sqrt((strain-etash)**2 + etaoa**2)
      strfun = strfun*caone - catwo*strain
      strfun = 1.0D0 + (1.0D0/(1.0D0 - caone*etaoa))*strfun

      !  cros

      bcro = (bctw*strfun)
      tcro = tco * strfun**(1.0D0/3.0D0)

      !  tred and bred

      tzero = thelium/tcro
      bzero = bmax/bcro
      bcrit = bcro * (1.0D0 - tzero**1.52D0)
      bred = bmax/bcrit

      !  New correction to prevent NaNs
      if (bzero < 1.0D0) then
         tcrit = tcro * (1.0D0 - bzero)**(1.0D0/1.52D0)
      else
         tcrit = thelium * (1.0D0 - bzero)  !  ??? tcrit will be zero or negative!
      end if

      tred = thelium/tcrit

      !  Enforce upper limits on tzero and bred

      tzero = min(tzero, 0.9999D0)
      bred = min(bred, 0.9999D0)

      !  Critical current density (A/m2)
      !  ITER parametrization is for the current in a single strand,
      !  not per unit area, so scalefac converts to current density

      scalefac = pi * (0.5D0*diter)**2 * (1.0D0-cuiter)

      jc1 = (csc/bmax)*strfun
      jc2 = (1.0D0-tzero**1.52D0)*(1.0D0-tzero**2)
      jc3 = bred**cp * (1.0D0-bred)**cq

      jcrit = jc1 * jc2 * jc3 / scalefac

    end subroutine itersc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine bi2212(bmax,jstrand,tsc,fhts,jcrit,tmarg)

      !+ad_name  bi2212
      !+ad_summ  Fitted parametrization to Bi-2212 superconductor properties
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
      !+ad_call  None
      !+ad_hist  08/10/13 PJK Initial version
      !+ad_hist  05/03/14 PJK Added comment about range of validity
      !+ad_hist  06/03/14 PJK Added warning if range of validity is violated
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
         write(*,*) 'Warning in routine BI2212:'
         write(*,*) 'Range of validity of the HTS Bi-2212 model has been violated:'
         write(*,*) '   S/C temperature (K) = ',tsc, ' (should be < 20 K)'
         write(*,*) 'Field at conductor (T) = ',bmax, ' (should be > 6 T)'
         write(*,*) '    Adjusted field (T) = ',b, ' (should be < 104 T)'
         write(*,*) ' '
      end if

    end subroutine bi2212

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

end module sctfcoil_module
