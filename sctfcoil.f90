!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module sctfcoil_module

  !+ad_name  sctfcoil_module
  !+ad_summ  Module containing superconducting TF coil routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  sctfcoil
  !+ad_cont  stresscl
  !+ad_cont  tfstress
  !+ad_cont  eyngeff
  !+ad_cont  sigvm
  !+ad_cont  sctfjalw
  !+ad_cont  coilshap
  !+ad_cont  tfcind
  !+ad_cont  outtf
  !+ad_cont  tfspcall
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
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: awpc,awptf,bcylir,dct,leni,leno,radwp,rbcndut, &
         rcoil,rcoilp,tant,tcan1,tcan2,tcan3,thtcoil,wbtf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cross-sectional area of outboard leg
    !  Original opaque algorithm...
    !  arealeg = 4.0D0 * tfthko * (rtot-0.5D0*tfthko) * sin(pi/tfno)

    !  Rectangular shape, aspect ratio = 2

    arealeg = 0.5D0 * tfthko*tfthko

    !  Determine layout of the inboard midplane TF coil leg

    !  Radius of centre of inboard TF coil leg

    if (itart == 1) then
       rtfcin = bore + bcylth + 0.5D0*tfcth
    else
       rtfcin = bore + ohcth + gapoh + bcylth + 0.5D0*tfcth
    end if

    !  Radius of outer edge of inboard leg

    rcoil = rtfcin + 0.5D0*tfcth

    !  Radius of inner edge of inboard leg

    rcoilp = rcoil - tfcth

    !  Half toroidal angular extent of a single TF coil inboard leg

    thtcoil = pi/tfno
    tant = tan(thtcoil)

    !  Circle/trapezoid distance

    dct = 0.0D0

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

    !  Peak toroidal field and radius of its occurrence

    rbmax = rcoil
    bmaxtf = ritfc * rmu0 / (2.0D0*pi*rbmax)

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
    do i = 1,4
       tfleng = tfleng + 2.0D0*(radctf(i) + 0.5D0*tfcth) * dthet(i)
    end do

    !  Case thicknesses

    if (itfmod /= 1) thkcas = tfcth * 0.5D0

    !  Case thickness of side nearest torus centreline

    tcan1 = thkcas

    !  Case thickness of side nearest plasma

    tcan2 = casthi

    !  Case thickness of side wall

    tcan3 = casths

    !  Winding pack dimensions (each leg)

    !  Radial extent

    thkwp = tfcth - dct - tcan2 - tcan1 - 2.0D0*tinstf

    if (thkwp <= 0.0D0) then
       write(*,*) 'Error in routine SCTFCOIL:'
       write(*,*) 'Winding pack thickness thkwp = ',thkwp
       write(*,*) 'Reduce case or insulation thicknesses,'
       write(*,*) 'or increase tfcth value or lower bound.'
       write(*,*) ' '
       !write(*,*) 'PROCESS stopping.'
       !goto 20
    end if

    !  Radius of geometrical centre of winding pack

    radwp = rcoil - dct - tcan2 - tinstf - 0.5D0*thkwp

    !  Thickness of winding pack section at R > radwp

    wwp1 = 2.0D0 * (radwp*tant - tcan3 - tinstf)

    !  Thickness of winding pack section at R < radwp

    wwp2 = 2.0D0 * ((radwp-0.5D0*thkwp)*tant - tcan3 - tinstf)

    !  Total cross-sectional area of winding pack

    awptf = (0.5D0*thkwp)*(wwp1 + wwp2)

    !  Total cross-sectional area of winding pack, including insulation

    awpc = (wwp1 + 2.0D0*tinstf) * (thkwp + 2.0D0*tinstf)

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

    jwptf = max(1.0D0, ritfc/(tfno*awptf*(1.0D0-wpvf)) )

    !  Superconducting cable information
    !  (number of turns not required to be an integer here for numerics)

    !  Radius of rounded corners of cable space inside conduit

    rbcndut = thwcndut * 0.75D0

    !  Dimension of square cross-section of each turn

    leno = sqrt(cpttf / jwptf)

    !  Dimension of square cable space inside insulation and case of
    !  the conduit of each turn

    leni = leno - 2.0D0*(thwcndut + thicndut)

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
    !  (with fudging to keep things positive...)

    acstf = sign(1.0D0,leni) * ( leni**2 - (4.0D0-pi)*rbcndut**2 )

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
          write(*,*) 'Artificially set rounded corner radius to zero'
          write(*,*) ' '
          rbcndut = 0.0D0
          acstf = leni**2
       end if
    end if

    !  Cross-sectional area of conduit case per turn

    acndttf = (leni + 2.0D0*thwcndut)**2 - acstf

    !  Total number of turns per TF coil

    turnstf = ritfc / (cpttf * tfno)

    !  Number of turns per pancake (NEVER USED)

    rnltf = thkwp / (leno*sqrt(aspcstf))

    !  Total conductor cross-sectional area, taking account of void area

    acond = acstf * turnstf * (1.0D0-vftf)

    !  Void area in cable, for He

    avwp = acstf * turnstf * vftf

    !  Insulation area (not including ground-wall)

    aiwp = turnstf * (leno**2 - acndttf - acstf)

    !  Structure area for cable

    aswp = (turnstf*acndttf)

    !  TF Coil areas and masses

    !  Surface areas (for cryo system)

    wbtf = rcoil*sin(thtcoil) - rcoilp*tant
    tfocrn = rcoilp * tant
    tficrn = tfocrn + wbtf
    tfsai = 4.0D0 * tfno * tficrn * hr1
    tfsao = 2.0D0 * tfno * tficrn * (tfleng - 2.0D0*hr1)

    !  Mass of case : add correction factor to maintain enough
    !  structure when the inboard case gets small
    !  (normalized to 1990 ITER)

    whtcas = casfact * tfleng * acasetf * dcase * &
         sqrt( 0.41D0 * tfareain/(acasetf *tfno) )

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

    !  Bucking cylinder

    bcylir = rcoilp - bcylth
    wtbc = pi *( (bcylir+bcylth)**2 - bcylir**2) * hr1 * 2.0D0*7800.0D0

    whttf = (whtcas+whtcon) * tfno

    !  Do stress calculations

    call stresscl

    if (iprint == 1) call outtf(outfile)

    return

20  continue

    !  Diagnostic output only (uncomment goto lines above to activate)

    write(*,*) '           tfcth = ',tfcth
    write(*,*) 'thkcas (= tcan1) = ',thkcas
    write(*,*) 'casthi (= tcan2) = ',casthi
    write(*,*) 'casths (= tcan3) = ',casths
    write(*,*) '          tinstf = ',tinstf
    write(*,*) '        thwcndut = ',thwcndut
    write(*,*) '        thicndut = ',thicndut
    write(*,*) '           cpttf = ',cpttf

    stop

  end subroutine sctfcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stresscl

    !+ad_name  stresscl
    !+ad_summ  TF coil stress routine
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine sets up the stress calculations for the
    !+ad_desc  TF coil set.
    !+ad_prob  None
    !+ad_call  eyngeff
    !+ad_call  sctfjalw
    !+ad_call  sigvm
    !+ad_call  tfstress
    !+ad_hist  10/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: i,itfst
    real(kind(1.0D0)) :: dummyv,seff,svmxz,svmyz,tcbs,fac

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Allowable stress (Pa)

    alstrtf = min( (2.0D0*csytf/3.0D0), (0.5D0*csutf) )

    !  Simple stress model option

    if (itfmod /= 1 ) then
       call sctfjalw(bmaxtfrp,rtfcin,rtot,rbmax,(1.0D-6*alstrtf), &
            tdmptf,jwdgcrt)
       return
    end if

    !  Set up graded stress model call information

    seff = sqrt(cpttf/jwptf)
    if (acstf >= 0.0D0) then
       tcbs = sqrt(acstf)
    else
       tcbs = 0.0D0
    end if
    radtf(1) = rbmax

    do i = 1,3
       eyoung(i) = eyngeff(eystl,eyins,eywp,thicndut,seff,thwcndut,tcbs)
       radtf(i+1) = radtf(i) - thkwp/3.0D0
       dummyv = max(0.001D0, (1.0D0 - casths*tfno/(pi*radtf(i)) ) )
       jeff(i) = jwptf * dummyv
    end do

    !  Outer ring section

    do i = 4,5
       radtf(i+1) = rtfcin - 0.5D0*tfcth
       eyoung(i) = eystl
       jeff(i) = 0.0D0
    end do

    !  Call stress routine

    call tfstress(poisson,radtf,eyoung,jeff,sigrtf,sigttf,deflect,itfst)

    if (itfst /= 1) then
       write(*,*) 'Error in routine STRESSCL:'
       write(*,*) 'TFSTRESS returned with value ',itfst
       write(*,*) 'PROCESS stopping.'
       STOP
    end if

    !  Convert to conduit + case

    fac = eystl*eyins*seff / &
         (eyins*(seff-2.0D0*thicndut) + 2.0D0*thicndut*eystl)
    sigrcon = sigrtf(3)/eyoung(3) * fac
    sigtcon = sigttf(3)/eyoung(3) * fac
    sigvert = vforce / (acasetf + acndttf*turnstf)

    !  Find case strain

    casestr = sigvert / eystl

    !  Find Von-Mises combinations in case

    strtf2 = sigvm(sigrtf(5), sigttf(5), sigvert, 0.0D0,0.0D0,0.0D0)

    !  Find Von-Mises combinations in conduit walls (take worst case
    !  of 2 walls)

    svmxz = sigvm(sigrcon, 0.0D0, sigvert, 0.0D0,0.0D0,0.0D0)
    svmyz = sigvm(0.0D0, sigtcon, sigvert, 0.0D0,0.0D0,0.0D0)
    strtf1 = max(svmxz,svmyz)

  end subroutine stresscl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfstress(nu,rad,ey,j,sigr,sigt,deflect,iflag)

    !+ad_name  tfstress
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
    !+ad_args  iflag   : output integer : 1 for successful call, 0 for failure
    !+ad_desc  This routine calculates the stresses in a superconductor TF coil
    !+ad_desc  inboard leg at midplane. The analysis of J. Myall (Aug. 1987) is
    !+ad_desc  followed (obtained from J. Miller of LLNL 5/91). This model allows
    !+ad_desc  5 regions in the coil, so graded conductors are possible. Regions
    !+ad_desc  1-3 are the winding pack regions (going from high to low field),
    !+ad_desc  and regions 4-5 are the solid steel ring and intermittent steel ring
    !+ad_desc  regions, respectively. A conventional nongraded ring can be modelled
    !+ad_desc  by inputting the same values of ey and j for regions 1-3, and
    !+ad_desc  regions 4-5.
    !+ad_prob  This routine is very sensitive to code changes.
    !+ad_prob  Simple (correct) reordering of assignment statements or changing
    !+ad_prob  <CODE>else if (i == 3) then</CODE> to simply <CODE>else</CODE>
    !+ad_prob  causes the output values to change.
    !+ad_call  linesolv
    !+ad_hist  09/05/91 JG  Initial version
    !+ad_hist  10/05/12 PJK Initial F90 version;
    !+ad_hisc               Converted from integer function to subroutine
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: nu
    real(kind(1.0D0)), dimension(6), intent(in) :: rad
    real(kind(1.0D0)), dimension(5), intent(in) :: ey, j
    real(kind(1.0D0)), dimension(5), intent(out) :: sigr, sigt
    real(kind(1.0D0)), intent(out) :: deflect
    integer, intent(out) :: iflag

    !  Local variables

    integer :: i, ii, ia
    real(kind(1.0D0)) :: b03,b05,b07,k2,k3,kk1,kk2,l4,l5,m6,m7, &
         n8,n9,nufac,p1,p2,p3,q2,q3,q4,q5,r4,r5,r6,r7,s6,s7,s8,s9, &
         t10,t8,t9
    real(kind(1.0D0)), dimension(10,10) :: a
    real(kind(1.0D0)), dimension(10) :: b, c

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    iflag = 0

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
       else if (i == 3) then  !  Truncating after the 'else' causes the results to change!
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

    iflag = 1

  end subroutine tfstress

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function eyngeff(estl,eins,ewp,tins,teff,tstl,tcs)

    !+ad_name  eyngeff
    !+ad_summ  Finds the effective Young's modulus of the TF coil winding pack
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_cont  N/A
    !+ad_args  estl : input real : Young's modulus of steel (Pa)
    !+ad_args  eins : input real : Young's modulus of insulator (Pa)
    !+ad_args  ewp  : input real : Young's modulus of windings (Pa)
    !+ad_args  tins : input real : insulator wrap thickness (m)
    !+ad_args  teff : input real : dimension of total cable with insulator (m)
    !+ad_args  tstl : input real : thickness of steel conduit (m)
    !+ad_args  tcs  : input real : dimension of cable space area inside conduit (m)
    !+ad_desc  This routine calculates the effective Young's modulus (Pa)
    !+ad_desc  of the TF coil in the winding pack section.
    !+ad_desc  Programmed by J. Galambos from a Lotus spreadsheet by J. Miller.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/91 JG  Initial version
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: eyngeff

    !  Arguments

    real(kind(1.0D0)) estl,eins,ewp,tins,teff,tstl,tcs

    !  Local variables

    real(kind(1.0D0)) :: tcond

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    tcond = teff - 2.0D0 * tins

    eyngeff = 2.0D0*eins*tins/teff + &
         2.0D0*eins*estl*tstl / (eins*tcond + 2.0D0*tins*estl) + &
         estl*eins*ewp*tcs / (estl*eins*tcs + 2.0D0*tins*eins*estl + &
         2.0D0*tins*estl*ewp)

  end function eyngeff

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
    !+ad_args  rtfmi   : input real : mean inner leg radius (m)
    !+ad_args  rtfmo   : input real : mean outer leg radius (m)
    !+ad_args  rtf2    : input real : radius of inner leg point nearest plasma (m)
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

    !  Add contribution in TF coil inner leg

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
    !+ad_call  ovarre
    !+ad_hist  14/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: ap

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (sect07 == 0) return

    call oheadr(outfile,'TF Coils')
    call ocmmnt(outfile,'Superconducting TF coils')

    if (magnt == 2) then
       call osubhd(outfile,'Wedged TF Coils, with two-step winding')
    else
       call osubhd(outfile,'Bucked TF Coils, with two-step winding')
    end if

    call ocmmnt(outfile,'Current Density :')
    call oblnkl(outfile)
    call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf)
    call ovarre(outfile,'Overall current density (A/m2)','(oacdcp)',oacdcp)

    if (itfmod /= 1) then
       call ovarre(outfile,'Allowable overall current density (A/m2)', &
            '(jwdgcrt)',jwdgcrt)
    end if

    call osubhd(outfile,'General Coil Parameters :')
    call ovarre(outfile,'Area per coil (m2)','(tfarea/tfno)',tfareain/tfno)
    call ovarre(outfile,'Total inner leg radial thickness (m)','(tfcth)',tfcth)
    call ovarre(outfile,'Inside half-width (m)','(tficrn)',tficrn)
    call ovarre(outfile,'Outside half width (m)','(tfocrn)',tfocrn)
    call ovarre(outfile,'Total current (MA)','(ritfc/1.D6)',1.0D-6*ritfc)
    call ovarre(outfile,'Vertical separating force per coil (N)','(vforce)',vforce)
    call ovarre(outfile,'Centering force per coil (N/m)','(cforce)',cforce)
    call ovarre(outfile,'Peak field (Amperes Law,T)','(bmaxtf)',bmaxtf)
    call ovarre(outfile,'Peak field (with ripple,T)','(bmaxtfrp)',bmaxtfrp)
    call ovarre(outfile,'Stored energy per coil (GJ)','(estotf)',estotf)
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng)
    call ovarre(outfile,'Number of TF coils','(tfno)',tfno)
    call ovarre(outfile,'Outer coil case thickness (m)','(thkcas)',thkcas)
    call ovarre(outfile,'Outer coil case area (m2)','(acasetf)',acasetf)

    call osubhd(outfile,'Coil Geometry :')
    call ovarre(outfile,'Inner leg centre radius (m)','(rtfcin)',rtfcin)
    call ovarre(outfile,'Outer leg centre radius (m)','(rtot)',rtot)
    call ovarre(outfile,'Maximum inner edge height (m)','(hmax)',hmax)
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
    end do
20  format(i4,t10,f10.3,t25,f10.3)

    call osubhd(outfile,'The centres of the arc are :')
    write(outfile,40)
40  format(t3,'arc',t16,'x(m)',t30,'y(m)')

    do i = 1,4
       write(outfile,20) i,xctfc(i),yctfc(i)
    end do

    call osubhd(outfile,'Conductor Information :')
    call ovarre(outfile,'Total mass of TF coils (kg)','(whttf)',whttf)
    call ovarre(outfile,'Superconductor mass per coil (kg)','(whtconsc)',whtconsc)
    call ovarre(outfile,'Copper mass per coil (kg)','(whtconcu)',whtconcu)
    call ovarre(outfile,'Steel conduit mass per coil (kg)','(whtconsh)',whtconsh)
    call ovarre(outfile,'Total conductor cable mass per coil (kg)','(whtcon)',whtcon)
    call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas)
    call ovarre(outfile,'Cable conductor + void area (m2)','(acstf)',acstf)
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
    call ovarre(outfile,'Winding thickness (m)','(thkwp)',thkwp)
    call ovarre(outfile,'Winding width 1 (m)','(wwp1)',wwp1)
    call ovarre(outfile,'Winding width 2 (m)','(wwp2)',wwp2)
    call ovarre(outfile,'Number of turns per TF coil','(turnstf)',turnstf)
    call ovarre(outfile,'Current per turn (A)','(cpttf)',cpttf)

    call osubhd(outfile,'TF Coil Stresses :')
    call ovarre(outfile,'Vertical stress (Pa)','(sigvert)',sigvert)
    call ovarre(outfile,'Conduit radial stress (Pa)','(sigrcon)',sigrcon)
    call ovarre(outfile,'Conduit tangential stress (Pa)','(sigtcon)',sigtcon)
    call ovarre(outfile,'Conduit Von Mises combination stress (Pa)','(strtf1)',strtf1)
    call ovarre(outfile,'Case radial stress (Pa)','(sigrtf(5))',sigrtf(5))
    call ovarre(outfile,'Case tangential stress (Pa)','(sigttf(5))',sigttf(5))
    call ovarre(outfile,'Case Von Mises combination stress (Pa)','(strtf2)',strtf2)
    call ovarre(outfile,'Allowable stress (Pa)','(alstrtf)',alstrtf)
    call ovarre(outfile,'Deflection at midplane (m)','(deflect)',deflect)

  end subroutine outtf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfspcall(outfile,iprint)

    !+ad_name  tfspcall
    !+ad_summ  Routine to call the superconductor module for the TF coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  supercon
    !+ad_cont  itersc
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(kind(1.0D0)) :: aturn, tfes, vdump
    integer :: ifail

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Simple model

    if (itfmod == 0) then
       vtfskv = 20.0D0
       return
    end if

    !  Stored energy (J) and cross-sectional area per turn

    tfes = estotf * 1.0D9
    aturn = ritfc/(jwptf*tfno*turnstf)

    ifail = 0
    call supercon(acstf,aturn,bmaxtfrp,vftf,fcutfsu,cpttf,isumattf, &
         jcrit_model,strncon,tdmptf,tfes,tftmp,tmaxpro,bcritsc,jcritsc, &
         tcritsc,iprint,outfile,jwdgpro,jwdgcrt,vdump,tmargtf,ifail)

    if (ifail /= 0) then
       write(*,*) 'Error in routine TFSPCALL:'
       write(*,*) 'SUPERCON returns with value ',ifail
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Dump voltage in kV

    vtfskv = vdump/1.0D3

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supercon(acs,aturn,bmax,fhe,fcu,iop,isumat,jcrit_model, &
         strain,tdump,tfes,the,tmax,bcritsc,jcritsc,tcritsc,iprint,outfile, &
         jwdgpro,jwdgcrt,vd,tmarg,ifail)

      !+ad_name  supercon
      !+ad_summ  Routine to calculate the TF coil superconductor properties
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  J Miller, ORNL
      !+ad_cont  N/A
      !+ad_args  acs : input real : Cable space - inside area (m2)
      !+ad_args  aturn : input real : Area per turn (i.e.  entire cable) (m2)
      !+ad_args  bmax : input real : Peak field at conductor (T)
      !+ad_args  fhe : input real : Fraction of cable space that is for He cooling
      !+ad_args  fcu : input real : Fraction of conductor that is copper
      !+ad_args  iop : input real : Operating current per turn (A)
      !+ad_args  isumat : input integer : Switch for conductor type:
      !+ad_argc                           1 = binary Nb3Sn,
      !+ad_argc                           2 = ternary Nb3Sn,
      !+ad_argc                           3 = NbTi,
      !+ad_argc                           4 = generic, but uses Nb3Sn current density calc.
      !+ad_argc                           5 = generic, but uses NbTi current density calc.
      !+ad_args  jcrit_model : input integer : Switch for Jcrit model for isumat=1 only:
      !+ad_argc                                0 = original model
      !+ad_argc                                1 = ITER Nb3Sn critical surface implementation
      !+ad_args  strain : input real : Strain on superconductor at operation conditions
      !+ad_args  tdump : input real : Dump time (sec)
      !+ad_args  tfes : input real : Energy stored in one TF coil (J)
      !+ad_args  the : input real : He temperature at peak field point (K)
      !+ad_args  tmax : input real : Max conductor temperature during quench (K)
      !+ad_args  bcritsc : input real : Critical field (T) (isumat=4,5 only)
      !+ad_args  jcritsc : input real : Critical J (A/m2) (isumat=4,5 only)
      !+ad_args  tcritsc : input real : Critical temperature (K) (isumat=4,5 only)
      !+ad_args  iprint : input integer : Switch for printing (1 = yes, 0 = no)
      !+ad_args  outfile : input integer : Fortran output unit identifier
      !+ad_args  jwdgpro : output real : Winding pack current density from temperature 
      !+ad_argc                          rise protection (A/m2)
      !+ad_args  jwdgcrt : output real : Critical winding pack current density (A/m2)
      !+ad_args  vd : output real : Discharge voltage imposed on a TF coil (V)
      !+ad_args  tmarg : output real : Temperature margin (K)
      !+ad_args  ifail : input/output integer : error flag (though never returns an error!)
      !+ad_desc  This routine calculates the superconductor properties for the TF coils.
      !+ad_desc  Programmed by J. Galambos 1991, from algorithms provided by J. Miller.
      !+ad_desc  <P>These are the ITER rules for Nb3Sn modelling. The routine calculates
      !+ad_desc  the critical current density (winding pack) and also the protection
      !+ad_desc  information (for a quench).
      !+ad_prob  None
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
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: isumat, jcrit_model, iprint, outfile
      real(kind(1.0D0)), intent(in) :: acs, aturn, bmax, fhe, fcu, iop, strain, &
           tdump, tfes, the, tmax, bcritsc, jcritsc, tcritsc
      integer, intent(inout) :: ifail
      real(kind(1.0D0)), intent(out) :: jwdgpro, jwdgcrt, vd, tmarg

      !  Local variables

      real(kind(1.0D0)) :: astrain, bbar, bc2, bc20, bc20m, cstrain, c0, &
           fac1, fac2, fcond, fstrain, icrit, iooic, jc, jwdgop, tbar, tc0, &
           tc0m, tc1

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Conductor fraction

      fcond = 1.0D0 - fhe

      !  Find critical current density

      select case (isumat)

      case (1)  !  binary Nb3Sn data
         bc20m = 28.0D0
         tc0m = 18.0D0
         c0 = 1.315D10

      case (2)  !  ternary Nb3Sn data
         bc20m = 24.0D0
         tc0m = 16.0D0
         c0 = 2.225D10

      case (3)  !  NbTi data
         bc20m = 15.0D0
         tc0m = 9.3D0
         c0 = 10.0D9

      case default  !  generic superconductor - use input values
         bc20m = bcritsc
         tc0m = tcritsc
         c0 = jcritsc

      end select

      if (strain <  0.0D0) then
         astrain = 900.0D0
      else
         astrain = 1250.0D0
      end IF

      fstrain = 1.0D0 - astrain * abs(strain)**1.7D0
      cstrain = c0 * sqrt(fstrain)

      !  Calculate current density, taking into account critical values

      if ((isumat /= 3).and.(isumat /= 5)) then  !  Nb3Sn model

         if ((isumat == 1).and.(jcrit_model == 1)) then

            !  Use ITER Nb3Sn critical surface implementation model

            call itersc(the,bmax,strain,jc,tc1)

         else
            tc0 = tc0m * fstrain**(1.0D0/3.0D0)
            tc1 = tc0m * (1.0D0 - bmax/bc20m)
            bc20 = bc20m * fstrain
            tbar = the/tc0
            tbar = min(tbar,0.999D0)
            bc2 = bc20 * (1.0D0 - tbar**2) * &
                 ( 1.0D0 - 0.31D0*tbar**2 * (1.0D0 - 1.77D0*log(tbar)) )
            bbar = bmax/bc2
            bbar = min(bbar,0.999D0)

            !+**PJK 25/07/11 Corrected SQRT(BBAR) to SQRT(BMAX) in denominator
            jc = cstrain * (1.0D0 - tbar**2)**2 * (1.0D0 - bbar)**2 &
                 / (sqrt(bc2) * sqrt(bmax))
         end if

      else  !  NbTi model

         tc1 = tc0m * (1.0D0 - bmax/bc20m)**0.59D0
         tc1 = max(tc1, 0.001D0)
         tbar = max((1.0D0 - the/tc1), 0.001D0)
         jc = c0 * (1.0D0 - bmax/bc20m) * tbar

      end if

      !  Critical current

      icrit = jc * acs * (1.0D0 - fhe) * (1.0D0 - fcu)

      !  Critical current density in winding pack

      jwdgcrt = icrit / aturn

      !  Ratio of operating / critical current

      iooic = iop / icrit

      !  Operating current density

      jwdgop = iop / aturn

      !  Temperature margin

      fac1 = max( 0.01D0, (1.0D0 - iooic) )
      fac2 = max( 0.01D0, (tc1-the) )
      tmarg = fac1 * fac2

      !  Find the current density limited by the protection limit

      call protect(iop,tfes,acs,aturn,tdump,fcond,fcu,the,tmax,jwdgpro,vd)

      ifail = 0

      if ((iprint == 0).or.(sect07 == 0)) return

      call oheadr(outfile,'Superconducting TF Coils')

      select case (isumat)

      case (1)
         call ocmmnt(outfile,'Superconductor used: Nb3Sn (binary)')
         if (jcrit_model == 0) then
            call ocmmnt(outfile,'  (original Jcrit model)')
         else
            call ocmmnt(outfile,'  (ITER Jcrit model)')
         end if
      case (2)
         call ocmmnt(outfile,'Superconductor used: Nb3Sn (ternary)')
      case (3)
         call ocmmnt(outfile,'Superconductor used: NbTi')
      case (4)
         call ocmmnt(outfile, &
              'Generic superconductor used: Nb3Sn current density model')
      case default
         call ocmmnt(outfile, &
              'Generic superconductor used: NbTi current density model')

      end select

      call oblnkl(outfile)
      call ovarre(outfile,'Peak field at conductor (T)','(bmax)',bmax)
      call ovarre(outfile,'Helium temperature at peak field (K)','(the)',the)
      call ovarre(outfile,'Helium fraction inside cable space','(fhe)',fhe)
      call ovarre(outfile,'Copper fraction of conductor','(fcu)',fcu)

      call osubhd(outfile,'Critical Current Information :')
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

    subroutine itersc(the,bmax,strain,jcrit,tcrit)

      !+ad_name  itersc
      !+ad_summ  Implementation of ITER Nb3Sn critical surface implementation
      !+ad_type  Function returning real
      !+ad_auth  R Kemp, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  the : input real : Coolant/SC temperature (K)
      !+ad_args  bmax : input real : Magnetic field at conductor (T)
      !+ad_args  strain : input real : Strain in superconductor
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
      !+ad_stat  Okay
      !+ad_docs  ITER Nb3Sn critical surface parameterization (2MMF7J) (2008),
      !+ad_docc    https://user.iter.org/?uid=2MMF7J&action=get_document
      !+ad_docs  ITER DDD 11-7: Magnets - conductors (2NBKXY) (2009),
      !+ad_docc    https://user.iter.org/?uid=2NBKXY&action=get_document
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: the, bmax, strain
      real(kind(1.0D0)), intent(out) :: jcrit, tcrit

      !  Local variables

      real(kind(1.0D0)), parameter :: csc = 16500.0D6
      real(kind(1.0D0)), parameter :: bctw = 32.97D0
      real(kind(1.0D0)), parameter :: tco = 16.06D0
      real(kind(1.0D0)), parameter :: cp = 0.63D0
      real(kind(1.0D0)), parameter :: cq = 2.1D0
      real(kind(1.0D0)), parameter :: caone = 44.0D0
      real(kind(1.0D0)), parameter :: catwo = 4.0D0
      real(kind(1.0D0)), parameter :: etaoa = 0.00256D0
      real(kind(1.0D0)), parameter :: etamax = -0.003253075D0
      real(kind(1.0D0)) :: tred, bcrit, bred, etash, tzero, bcro, &
           tcro, bzero, strfun, jc1, jc2, jc3

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

      tzero = the/tcro
      bzero = bmax/bcro
      bcrit = bcro * (1.0D0 - tzero**1.52D0)
      bred = bmax/bcrit
      tcrit = tcro * (1.0D0 - bzero)**(1.0D0/1.52D0)
      tred = the/tcrit

      !  Enforce upper limits on tzero and bred

      tzero = min(tzero, 0.9999D0)
      bred = min(bred, 0.9999D0)

      !  Critical current density (A/m2)

      jc1 = (csc/bmax)*strfun
      jc2 = (1.0D0-tzero**1.52D0)*(1.0D0-tzero**2)
      jc3 = bred**cp * (1.0D0-bred)**cq

      jcrit = jc1 * jc2 * jc3

    end subroutine itersc

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
      !+ad_prob  None
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
