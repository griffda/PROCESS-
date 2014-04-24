!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  block data bdcond

     !  Thermal conductivity block data

     !**********************************************************************
     ! This contains the coefficients for the thermal conductivity         *
     ! polynomials and also a value for the density.                       *
     ! The materials considered are, in order:                             *
     ! 1)  martensitic steel                                               *
     ! 2)  austenitic steel                                                *
     ! 3)  lithium lead breeder                                            *
     ! 4)  beryllium                                                       *
     ! 5)  dummy1 - air                                                    *
     ! 6)  plasma                                                          *
     ! 7)  copper                                                          *
     ! 8)  lead                                                            *
     ! 9)  lithium oxide                                                   *
     ! 10) liquid lithium                                                  *
     ! 11) vanadium alloy                                                  *
     ! 12) superconductor Nb3Sn (set equal to copper for now)              *
     ! 13) superconductor NbTi  (set equal to copper for now)              *
     !**********************************************************************

     integer, parameter :: nmat=13

     real(kind(1.0D0)), dimension(nmat) :: a,b,c,d,e,rho
     common/condct/a,b,c,d,e,rho

     data a(1),b(1),c(1),d(1),e(1),rho(1) &
          /2.2867D+01, &
          1.4546D-02, &
          -2.3056D-05, &
          1.4815D-08, &
          0.000D-00, &
          8.000D+03/

     data a(2),b(2),c(2),d(2),e(2),rho(2) &
          /9.0109D+00, &
          1.5298D-02, &
          0.0000D-00, &
          0.0000D-00, &
          0.0000D-00, &
          8.000D+03/

     data a(3),b(3),c(3),d(3),e(3),rho(3) &
          /7.3008D+00, &
          1.9600D-02, &
          0.0000D-06, &
          0.0000D+00, &
          0.0000D+00, &
          9.300D+03/

     data a(4),b(4),c(4),d(4),e(4),rho(4) &
          /4.3035D+02, &
          -1.1674D-00, &
          1.6044D-03, &
          -1.0097D-06, &
          2.3642D-10, &
          1.8D+03/

     data a(5),b(5),c(5),d(5),e(5),rho(5) &
          /1.0000D-06, &
          0.0000D+00, &
          0.0000D+00, &
          0.0000D+00, &
          0.0000D+00, &
          1.300D-00/

     data a(6),b(6),c(6),d(6),e(6),rho(6) &
          /1.0000D+16, &
          0.0000D+00, &
          0.0000D+00, &
          0.0000D+00, &
          0.0000D+00, &
          1.000D+00/

     data a(7),b(7),c(7),d(7),e(7),rho(7) &
          /4.2075D+02, &
          -6.8493D-02, &
          0.0000D-00, &
          0.0000D+00, &
          0.0000D+00, &
          8.90D+03/

     data a(8),b(8),c(8),d(8),e(8),rho(8) &
          /4.7100D+01, &
          -4.9000D-02, &
          2.0600D-05, &
          -2.2500D-09, &
          0.000D+00, &
          1.24D+04/

     data a(9),b(9),c(9),d(9),e(9),rho(9) &
          /3.1686D+01, &
          -8.6498D-02, &
          1.1881D-04, &
          -7.9654D-08, &
          2.0734D-11, &
          2.013D+03/

     data a(10),b(10),c(10),d(10),e(10),rho(10) &
          /3.8966D+01, &
          1.2250D-02, &
          0.0000D+00, &
          0.0000D+00, &
          0.0000D+00, &
          5.34D+02/

     data a(11),b(11),c(11),d(11),e(11),rho(11) &
          /1.6734D+01, &
          1.4678D-02, &
          0.0000D+00, &
          0.0000D+00, &
          0.0000D+00, &
          6.10D+03/

     data a(12),b(12),c(12),d(12),e(12),rho(12) &
          /4.2075D+02, &
          -6.8493D-02, &
          0.0000D-00, &
          0.0000D+00, &
          0.0000D+00, &
          8.90D+03/

     data a(13),b(13),c(13),d(13),e(13),rho(13) &
          /4.2075D+02, &
          -6.8493D-02, &
          0.0000D-00, &
          0.0000D+00, &
          0.0000D+00, &
          8.90D+03/

  end block data

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine loca(outfile,iprint)

    !+ad_name  loca
    !+ad_summ  Interface to Loss Of Coolant Accident model
    !+ad_type  Subroutine
    !+ad_auth  C B A Forty, Culham Laboratory
    !+ad_auth  W E Han, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  blcyl3
    !+ad_cont  deroff
    !+ad_cont  fofx
    !+ad_cont  newton
    !+ad_cont  root
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine links the Loss Of Coolant Accident model to the rest of
    !+ad_desc  PROCESS. The model calculates the steady state temperatures that
    !+ad_desc  would develop following a loss of coolant accident.
    !+ad_prob  None
    !+ad_call  build_variables
    !+ad_call  constants
    !+ad_call  fispact_variables
    !+ad_call  fwbs_variables
    !+ad_call  pfcoil_variables
    !+ad_call  physics_variables
    !+ad_call  process_output
    !+ad_call  tfcoil_variables
    !+ad_call  blcyl3
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  06/02/97 PJK Initial version
    !+ad_hist  26/02/97 PJK Corrected cases where there is no OH coil
    !+ad_hist  06/07/99 PJK Allowed for use of more ISUMATTF options
    !+ad_hist  19/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  06/11/12 PJK Added fispact_variables
    !+ad_hist  09/04/13 PJK Comment changes
    !+ad_hist  24/04/14 PJK Calculation proceeds irrespective of iprint
    !+ad_stat  This routine is untested in F90...
    !+ad_docs  F/MI/PJK/LOGBOOK12, pp.70,71,72,73
    !+ad_docs  Strategic Studies Note 96/30, January 1997
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables
    use constants
    use fispact_variables
    use fwbs_variables
    use pfcoil_variables
    use physics_variables
    use process_output
    use tfcoil_variables

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: ac,as,av,a1
    integer :: i,k,ibc,itf,ioh

    integer, parameter ::  nreg = 15, nmat = 13

    !  Global variables passed via COMMON

    real(kind(1.0D0)), dimension(nreg) :: qvol    !  decay heat power in each region (W)
    real(kind(1.0D0)), dimension(nreg) :: radmid  !  mean radial coord of each region (m)
    real(kind(1.0D0)), dimension(nreg) :: radmin  !  inside radial coord of each region (m)
    real(kind(1.0D0)), dimension(nreg) :: radpls  !  outside radial coord of each region (m)
    real(kind(1.0D0)), dimension(nreg,nmat) :: matfrc  !  material fractions in each region
    common/geom/radmin,radpls,radmid,matfrc,qvol

    real(kind(1.0D0)) :: alpha  !  heat transfer coefficient at outer boundary
    real(kind(1.0D0)) :: tair  !  ambient air temperature (K)
    integer :: npath  !  switch for heat rejection model:
    !                      0 = convection only
    !                      1 = convection + radiation
    common/edget/tair,alpha,npath

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Heat transfer coefficient and air temperature

    alpha = 6.5D0
    tair = 300.0D0

    !  NPATH=1 does a convection + radiation calculation
    !  NPATH=0 does a convection only calculation

    npath = 0

    !  Radial build

    radmin(1)  = 1.0D-20
    radpls(1)  = radmin(1) + bore
    radmin(2)  = radpls(1)

    if (itart == 0) then
       radpls(2) = radmin(2) + ohcth
       radmin(3) = radpls(2) + gapoh
       radpls(3) = radmin(3) + bcylth
       radmin(4) = radpls(3)
       radpls(4) = radmin(4) + tfcth
    else
       radpls(2) = radmin(2) + bcylth
       radmin(3) = radpls(2)
       radpls(3) = radmin(3) + tfcth
       radmin(4) = radpls(3) + gapoh
       radpls(4) = radmin(4) + ohcth
    end if

    radmin(5)  = radpls(4)
    radpls(5)  = radmin(5) + ddwi
    radmin(6)  = radpls(5) + gapds
    radpls(6)  = radmin(6) + shldith
    radmin(7)  = radpls(6)
    radpls(7)  = radmin(7) + blnkith
    radmin(8)  = radpls(7)
    radpls(8)  = radmin(8) + fwith
    radmin(9)  = radpls(8)
    radpls(9)  = radmin(9) + scrapli + 2.0D0*rminor + scraplo
    radmin(10) = radpls(9)
    radpls(10) = radmin(10) + fwoth
    radmin(11) = radpls(10)
    radpls(11) = radmin(11) + blnkoth
    radmin(12) = radpls(11)
    radpls(12) = radmin(12) + shldoth
    radmin(13) = radpls(12) + gapsto
    radpls(13) = radmin(13) + ddwi
    radmin(14) = radpls(13)
    radpls(14) = radmin(14) + tfthko
    radmin(15) = radpls(14) + 2.0D0
    radpls(15) = radmin(15) + ddwex

    do k = 1,nreg
       radmid(k) = 0.5D0 * (radmin(k) + radpls(k))
       qvol(k) = 0.0D0
       do i = 1,nmat
          matfrc(k,i) = 0.0D0
       end do
    end do

    !  Non-zero material fractions MATFRC(REGION,MATERIAL)
    !  The materials are:
    !  1  = Martensitic steel
    !  2  = Austenitic steel
    !  3  = Lithium lead breeder
    !  4  = Beryllium
    !  5  = Dummy (air)
    !  6  = Plasma
    !  7  = Copper
    !  8  = Lead
    !  9  = Lithium oxide
    !  10 = Liquid lithium
    !  11 = Vanadium alloy
    !  12 = Nb3Sn superconductor
    !  13 = NbTi superconductor
    !  This list should tally with that in block data BDCOND

    !  Take account of different builds for TART and non-TART devices
    !  IBC = bucking cylinder region number
    !  ITF = inboard TF coil region number
    !  IOH = OH coil region number

    if (itart == 0) then
       ibc = 3
       itf = 4
       ioh = 2
    else
       ibc = 2
       itf = 3
       ioh = 4
    end if

    !  Machine bore

    matfrc(1,5) = 1.0D0

    !  OH coil

    if (iohcl == 1) then

       if (ipfres == 1) then  !  resistive PF coils
          matfrc(ioh,5) = vfohc
          matfrc(ioh,7) = 1.0D0 - vfohc

       else  !  superconducting PF coils

          !  Conductor + void areas

          a1 = (rb(nohc)-ra(nohc))*(zh(nohc)-zl(nohc))
          ac = a1 * (1.0D0 - vfohc)
          av = a1 * vfohc

          !  Steel cross-sectional area

          as = wts(nohc) / (2.0D0*pi*rpf(nohc) * 7800.0D0)

          matfrc(ioh,1) = as / (ac+av+as) * fmsoh
          matfrc(ioh,2) = as / (ac+av+as) * (1.0D0 - fmsoh)
          matfrc(ioh,5) = av / (ac+av+as)
          matfrc(ioh,7) = ac / (ac+av+as) * (1.0D0-sccufac*bpf(nohc))

          if ((isumatpf == 1).or.(isumatpf == 2)) then
             matfrc(ioh,12) = ac / (ac+av+as) * sccufac*bpf(nohc)
          else
             matfrc(ioh,13) = ac / (ac+av+as) * sccufac*bpf(nohc)
          end if

       end if

    else

       matfrc(ioh,5) = 1.0D0

    end if

    !  Bucking cylinder

    matfrc(ibc,1) = fmsbc
    matfrc(ibc,2) = 1.0D0 - fmsbc

    !  Inboard TF coil

    if (itfsup == 0) then  !  resistive TF coils
       matfrc(itf,5) = fcoolcp
       matfrc(itf,7) = 1.0D0 - fcoolcp

    else  !  superconducting TF coils

       !  Approximate total cross-sectional area per coil

       a1 = acasetf+acond+avwp+aswp

       matfrc(itf,1) = (acasetf + aswp) / a1 * fmstf
       matfrc(itf,2) = (acasetf + aswp) / a1 * (1.0D0 - fmstf)
       matfrc(itf,5) = avwp / a1
       matfrc(itf,7) = acond / a1 * fcutfsu

       if (isumattf /= 3) then  !  treat generic superconductors like Nb3Sn
          matfrc(itf,12) = acond / a1 * (1.0D0 - fcutfsu)
       else
          matfrc(itf,13) = acond / a1 * (1.0D0 - fcutfsu)
       end if

    end if

    !  Inboard vacuum vessel

    matfrc(5,1) = fmsdwi
    matfrc(5,2) = 1.0D0 - fmsdwi

    !  Inboard shield

    matfrc(6,1) = (1.0D0 - vfshld) * fmssh
    matfrc(6,2) = (1.0D0 - vfshld) * (1.0D0 - fmssh)
    matfrc(6,5) = vfshld

    !  Inboard blanket

    if (lblnkt /= 1) then  !  Old blanket model

       matfrc(7,1) = fblss * fmsbl
       matfrc(7,2) = fblss * (1.0D0-fmsbl)
       matfrc(7,4) = fblbe
       matfrc(7,5) = vfblkt
       matfrc(7,9) = fblli2o
       matfrc(7,11) = fblvd

    else  !  New blanket model

       if (smstr == 1) then

          !  Li2O/Be solid blanket

          matfrc(7,1) = fblss * fmsbl
          matfrc(7,2) = fblss * (1.0D0-fmsbl)
          matfrc(7,4) = fblbe
          matfrc(7,5) = vfblkt
          matfrc(7,9) = fblli2o
          matfrc(7,11) = fblvd

       else

          !  LiPb/Li liquid blanket

          matfrc(7,1) = fblss * fmsbl
          matfrc(7,2) = fblss * (1.0D0-fmsbl)
          matfrc(7,3) = fbllipb
          matfrc(7,5) = vfblkt
          matfrc(7,10) = fblli
          matfrc(7,11) = fblvd

       end if

    end if

    !  Inboard first wall

    matfrc(8,1) = (1.0D0 - fwclfr) * fmsfw
    matfrc(8,2) = (1.0D0 - fwclfr) * (1.0D0 - fmsfw)
    matfrc(8,5) = fwclfr

    !  Plasma

    matfrc(9,6) = 1.0D0

    !  Outboard first wall

    matfrc(10,1) = (1.0D0 - fwclfr) * fmsfw
    matfrc(10,2) = (1.0D0 - fwclfr) * (1.0D0 - fmsfw)
    matfrc(10,5) = fwclfr

    !  Outboard blanket (same as inboard blanket)

    matfrc(11,:) = matfrc(7,:)

    !  Outboard shield

    matfrc(12,1) = (1.0D0 - vfshld) * fmssh
    matfrc(12,2) = (1.0D0 - vfshld) * (1.0D0 - fmssh)
    matfrc(12,5) = vfshld

    !  Outboard vacuum vessel

    matfrc(13,1) = fmsdwi
    matfrc(13,2) = 1.0D0 - fmsdwi

    !  Outboard TF coil

    if (itfsup == 0) then  !  resistive TF coils

       matfrc(14,5) = vftf
       matfrc(14,7) = 1.0D0 - vftf

    else  !  superconducting TF coils

       !  Approximate total cross-sectional area per coil
       !  including enlargement factor for case thickness

       a1 = (acasetf*tfootfi)+acond+avwp+aswp

       matfrc(14,1) = (acasetf*tfootfi + aswp) / a1 * fmstf
       matfrc(14,2) = (acasetf*tfootfi + aswp) / a1 * (1.0D0 - fmstf)
       matfrc(14,5) = avwp / a1
       matfrc(14,7) = acond / a1 * fcutfsu

       if (isumattf /= 3) then  !  treat generic superconductors like Nb3Sn
          matfrc(14,12) = acond / a1 * (1.0D0 - fcutfsu)
       else
          matfrc(14,13) = acond / a1 * (1.0D0 - fcutfsu)
       end if

    end if

    !  External cryostat

    matfrc(15,1) = fmsdwe
    matfrc(15,2) = 1.0D0 - fmsdwe

    !  Non-zero decay heat powers (W) after 3 months

    qvol(7)  = blihkw(2) * 1.0D3
    qvol(8)  = fwihkw(2) * 1.0D3
    qvol(10) = fwohkw(2) * 1.0D3
    qvol(11) = blohkw(2) * 1.0D3

    !  Calculate the temperature at the plasma - outboard first wall
    !  interface (K)

    call blcyl3(fwtemp)

    !  Output section

    if (iprint == 1) then

       call oheadr(outfile,'Loss of Coolant Accident')
       call ovarre(outfile,'First wall temperature after 3 months (K)', &
            '(fwtemp)',fwtemp)

    end if

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine blcyl3(fwtemp)

      !+ad_name  blcyl3
      !+ad_summ  Fusion power plant heat transfer module
      !+ad_type  Subroutine
      !+ad_auth  C B A Forty, Culham Laboratory
      !+ad_auth  W E Han, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  fwtemp : output real :  Outboard first wall temperature (K)
      !+ad_desc  This routine calculates the steady state temperatures that
      !+ad_desc  would develop following a loss of coolant accident.
      !+ad_desc  Cylindrical geometry is assumed.
      !+ad_prob  None
      !+ad_call  root
      !+ad_hist  17/01/97 CBAF Initial version
      !+ad_hist  06/02/97 PJK Modified to allow input of QVOL rather than DECPW
      !+ad_hist  19/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  F/MI/PJK/LOGBOOK12, pp.70,71,72,73
      !+ad_docs  Strategic Studies Note 96/30, January 1997
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(out) :: fwtemp

      !  Local variables

      integer, parameter :: nreg = 15  !  dimensioning of radial nodes
      integer, parameter :: nmat = 13  !  number of material types

      real(kind(1.0D0)), dimension(nreg) :: a1,a2,a3,aconst,avecon,averho, &
           b1,b2,bconst,c1,c2,decpow,dt,eprime,qflow,radgap,sarea,t, &
           temmid,temmin,templs,thcond,vfact
      real(kind(1.0D0)), dimension(nreg,nmat) :: frcrho,ithcon,thcon
      real(kind(1.0D0)), dimension(0:nreg) :: sumqvl

      real(kind(1.0D0)) :: epsiln,qouter,stefan,tnew
      integer ::  i,j,k

      !  Global variables passed via COMMON

      real(kind(1.0D0)), dimension(nreg) :: qvol    !  decay heat power in each region (W)
      real(kind(1.0D0)), dimension(nreg) :: radmid  !  mean radial coord of each region (m)
      real(kind(1.0D0)), dimension(nreg) :: radmin  !  inside radial coord of each region (m)
      real(kind(1.0D0)), dimension(nreg) :: radpls  !  outside radial coord of each region (m)
      real(kind(1.0D0)), dimension(nreg,nmat) :: matfrc  !  material fractions in each region
      common/geom/radmin,radpls,radmid,matfrc,qvol

      real(kind(1.0D0)) :: alpha  !  heat transfer coefficient at outer boundary
      real(kind(1.0D0)) :: tair  !  ambient air temperature (K)
      integer :: npath  !  switch for heat rejection model:
      !                      0 = convection only
      !                      1 = convection + radiation
      common/edget/tair,alpha,npath

      !  Thermal conductivity polynomial coefficients, material density
      real(kind(1.0D0)), dimension(nmat) :: acoeff,bcoeff,ccoeff,dcoeff,ecoeff,rho
      common/condct/acoeff,bcoeff,ccoeff,dcoeff,ecoeff,rho

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      stefan = 5.67D-8  !  Stefan-Boltzmann constant

      !  epsilon used in radiation model (assumed fixed)

      epsiln = 0.80D0

      !  Initial mid-point temperatures

      temmid(:) = 600.0D0

      !  Calculate the radiation view factor between opposite faces

      vfact(1) = 1.0D0
      do k = 2,nreg
         vfact(k) = radpls(k-1)/radmin(k)
      end do

      !  Calculate the geometry variation in emissivity

      do k = 1,nreg
         eprime(k) = 1.0D0 / (1.0D0/epsiln + vfact(k)*(1.0D0/epsiln-1.0D0))
      end do

      do j = 1,100  !  Iteration loop

         !  Radially distributed average densities

         averho(:) = 0.0D0
         do k = 1,nreg
            do i = 1,nmat
               frcrho(k,i) = matfrc(k,i)*rho(i)
               averho(k) = averho(k) + frcrho(k,i)
            end do
         end do

         !  Calculate the radially distributed average decay powers

         sumqvl(:) = 0.0D0
         do k = 1,nreg
            decpow(k) = qvol(k) / &
                 (pi*(radpls(k)**2-radmin(k)**2) * averho(k))
            sumqvl(k) = sumqvl(k-1) + qvol(k)
         end do

         !  Calculate the radial (radpls) surface areas

         sarea(:) = 2.0D0*pi*radpls(:)

         !  Calculate the heat flows across the radpls interfaces

         do k = 1,nreg
            qflow(k) = sumqvl(k)/sarea(k)
         end do

         !  Initial thermal conductivities
         !  Does a parallel addition of thermal conductivities

         avecon(:) = 0.0D0
         do k = 1,nreg
            do i = 1,nmat
               thcon(k,i) = acoeff(i) + &
                    bcoeff(i)*temmid(k) + &
                    ccoeff(i)*temmid(k)**2 + &
                    dcoeff(i)*temmid(k)**3 + &
                    ecoeff(i)*temmid(k)**4
               ithcon(k,i) = matfrc(k,i)*thcon(k,i)
               avecon(k) = avecon(k) + ithcon(k,i)
               thcond(k) = 1.0D0*avecon(k)
            end do
         end do

         !  Calculate the A constant from inside-to-outside

         aconst(1) = 0.0D0
         do k = 1,nreg-1
            aconst(k+1) = radmin(k+1)/thcond(k+1) * &
                 ( (decpow(k+1)*averho(k+1)*radmin(k+1)/2.0D0) + &
                 (vfact(k)*aconst(k)*thcond(k)/radpls(k)) - &
                 (vfact(k)*decpow(k)*averho(k)*radpls(k)/2.0D0) )
         end do

         !  Boundary temperature calculation
         !  First calculate the Q passing through the radial area

         k = nreg
         qouter = sumqvl(nreg)/sarea(nreg)

         !  Then do the required calculation

         if (npath == 1) then  !  Use the root finder subroutine
            k = nreg
            call root(qouter,tnew,alpha,tair)
            templs(k) = tnew
         else
            !  Do the simple convection solution
            k = nreg
            templs(k) = qouter/2.0D0/alpha + tair
         end if

         !  Calculate the outside B constant

         bconst(k) = templs(k) + &
              (decpow(k)*averho(k)*radpls(k)**2 /4.0D0 /thcond(k)) - &
              (aconst(k)*log(radpls(k)))
         do k = 2,nreg
            radgap(k) = radmin(k) - radpls(k-1)
         end do

         !  Calculate the remaining B constants outside-to-inside

         do k = nreg,2,-1

            if (radgap(k) > 0.0D0) then

               !  Use the radiation gap model to calculate BCONST
               !  Split the equation into managable bits

               a1(k) = -decpow(k)*averho(k)*radmin(k)**2 /4.0D0 /thcond(k)
               a2(k) = aconst(k)*log(radmin(k))
               a3(k) = bconst(k)
               b1(k-1) = decpow(k-1)*averho(k-1)*radpls(k-1)/2.0D0 &
                    /eprime(k-1) /stefan
               b2(k-1) = -aconst(k-1)*thcond(k-1)/radpls(k-1) &
                    /eprime(k-1) /stefan
               c1(k-1) = decpow(k-1)*averho(k-1)*radpls(k-1)**2 /4.0D0 &
                    /thcond(k-1)
               c2(k-1) = -aconst(k-1)*log(radpls(k-1))
               dt(k-1) = b1(k-1) + b2(k-1)
               t(k) = a1(k) + a2(k) + a3(k)
               t(k-1) = c1(k-1) + c2(k-1)
               bconst(k-1) = (t(k)**4 + dt(k-1))**0.25D0 + t(k-1)

            else

               !  Use the conduction model to calculate BCONST

               bconst(k-1) = &
                    ( decpow(k-1)*averho(k-1)*radpls(k-1)**2 /4.0D0 &
                    /thcond(k-1) ) &
                    - ( decpow(k)*averho(k)*radmin(k)**2 &
                    /4.0D0 /thcond(k) ) + &
                    ( aconst(k)*log(radmin(k)) &
                    - aconst(k-1)*log(radpls(k-1)) ) + bconst(k)

            end if

         end do

         !  Calculate new radial temperatures

         do k = 1,nreg
            temmin(k) = (-decpow(k)*averho(k)*radmin(k)**2 &
                 /4.0D0 /thcond(k)) + (aconst(k)*log(radmin(k))) + bconst(k)
            temmid(k) = (-decpow(k)*averho(k)*radmid(k)**2 &
                 /4.0D0 /thcond(k)) + (aconst(k)*log(radmid(k))) + bconst(k)
            templs(k) = (-decpow(k)*averho(k)*radpls(k)**2 &
                 /4.0D0 /thcond(k)) + (aconst(k)*log(radpls(k))) + bconst(k)
         end do

      end do  !  end of iteration loop

      fwtemp = temmin(10)

    end subroutine blcyl3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine root(q,x,a,t)

      !+ad_name  root
      !+ad_summ  Root finding routine for the LOCA model
      !+ad_type  Subroutine
      !+ad_auth  C B A Forty, Culham Laboratory
      !+ad_auth  W E Han, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  q : input real : heat power (W)
      !+ad_args  x : output real : temperature (K)
      !+ad_args  a : input real : heat transfer coefficient
      !+ad_args  t : input real : ambient air temperature (K)
      !+ad_desc  This routine calculates the temperature at a layer given
      !+ad_desc  the local thermal characteristics, via a Newton-Raphson
      !+ad_desc  root-finding method.
      !+ad_prob  None
      !+ad_call  deroff
      !+ad_call  fofx
      !+ad_call  newton
      !+ad_hist  17/01/97 CBAF Initial version
      !+ad_hist  19/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  A First Course in Computational Physics - Paul L DeVries,
      !+ad_docc  John Wiley & Sons, New York 1994: Chapter 2, p.54
      !+ad_docs  Strategic Studies Note 96/30, January 1997
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: q,a,t
      real(kind(1.0D0)), intent(out) :: x

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      x = 0.8D0
      call newton(x, fofx, deroff, q, a, t)

    end subroutine root

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function fofx(x, q, a, t)

      !+ad_name  fofx
      !+ad_summ  Radiation power function for the LOCA model
      !+ad_type  Function returning real
      !+ad_auth  C B A Forty, Culham Laboratory
      !+ad_auth  W E Han, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  x : input real : temperature (K)
      !+ad_args  q : input real : heat power (W)
      !+ad_args  a : input real : heat transfer coefficient
      !+ad_args  t : input real : ambient air temperature (K)
      !+ad_desc  This routine evaluates the radiation power function...
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  17/01/97 CBAF Initial version
      !+ad_hist  19/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  A First Course in Computational Physics - Paul L DeVries,
      !+ad_docc  John Wiley & Sons, New York 1994: Chapter 2, p.54
      !+ad_docs  Strategic Studies Note 96/30, January 1997
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: fofx

      !  Arguments

      real(kind(1.0D0)), intent(in) :: x,q,a,t

      !  Local variables

      real(kind(1.0D0)), parameter :: stefan = 5.67D-8
      real(kind(1.0D0)), parameter :: emisiv = 0.667D0

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      fofx = a*(x-t) + stefan*emisiv*(x**4-t**4) - q

    end function fofx

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function deroff(x,a,t)

      !+ad_name  deroff
      !+ad_summ  Derivative of function fofx with respect to x
      !+ad_type  Function returning real
      !+ad_auth  C B A Forty, Culham Laboratory
      !+ad_auth  W E Han, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  x : input real : temperature (K)
      !+ad_args  a : input real : heat transfer coefficient
      !+ad_args  t : input real : ambient air temperature (K)
      !+ad_desc  This routine evaluates the derivative with respect to x
      !+ad_desc  of the function <A HREF="fofx.html">fofx</A>.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  17/01/97 CBAF Initial version
      !+ad_hist  19/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  A First Course in Computational Physics - Paul L DeVries,
      !+ad_docc  John Wiley & Sons, New York 1994: Chapter 2, p.54
      !+ad_docs  Strategic Studies Note 96/30, January 1997
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: deroff

      !  Arguments

      real(kind(1.0D0)), intent(in) :: x,a,t

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      deroff  = a + 4.0D0 * 3.5D-8*x**3

    end function deroff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine newton(x,f,fprime,q,a,t)

      !+ad_name  newton
      !+ad_summ  Newton-Raphson root-finder for the LOCA model
      !+ad_type  Subroutine
      !+ad_auth  C B A Forty, Culham Laboratory
      !+ad_auth  W E Han, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  x : input/output real : x coordinate
      !+ad_args  f : real function : f(x)
      !+ad_args  fprime : real function : derivative d/dx (f(x))
      !+ad_args  q : input real : heat power (W)
      !+ad_args  a : input real : heat transfer coefficient
      !+ad_args  t : input real : ambient air temperature (K)
      !+ad_desc  This routine is the Newton-Raphson root-finder for the
      !+ad_desc  LOCA model.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  17/01/97 CBAF Initial version
      !+ad_hist  19/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  A First Course in Computational Physics - Paul L DeVries,
      !+ad_docc  John Wiley & Sons, New York 1994: Chapter 2, p.54
      !+ad_docs  Strategic Studies Note 96/30, January 1997
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(inout) :: x
      real(kind(1.0D0)), intent(in) :: q,a,t
      real(kind(1.0D0)), external :: f, fprime

      !  Local variables

      real(kind(1.0D0)), parameter :: tol = 5.0D-6
      real(kind(1.0D0)) :: delta,error

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do
         delta = -f(x,q,a,t) / fprime(x,a,t)
         x = x + delta

         error = abs(delta/x)
         if (error <= tol) exit
      end do

    end subroutine newton

  end subroutine loca

