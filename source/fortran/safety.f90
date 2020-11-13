!  $Id:: safety.f90 258 2014-04-24 12:28:55Z pknight                    $
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

    !! Interface to Loss Of Coolant Accident model
    !! author: C B A Forty, Culham Laboratory
    !! author: W E Han, CCFE, Culham Science Centre
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine links the Loss Of Coolant Accident model to the rest of
    !! PROCESS. The model calculates the steady state temperatures that
    !! would develop following a loss of coolant accident.
    !! F/MI/PJK/LOGBOOK12, pp.70,71,72,73
    !! Strategic Studies Note 96/30, January 1997
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: blnkith, blnkoth, bore, ddwex, d_vv_in, d_vv_out, &
      fmsbl, fmsdwe, fmsdwi, fmsfw, fmsoh, fmssh, fmstf, fwith, fwoth, gapds, &
      gapoh, gapsto, iohcl, ohcth, precomp, scrapli, scraplo, shldith, shldoth, &
      tfcth, tfootfi, tfthko, tftsgap, thshield, vvblgap
    use fispact_variables, only: fwtemp, blihkw, fwihkw, fwohkw, blohkw
    use fwbs_variables, only: blkttype, denstl, fblbe, fblli, fblli2o, &
      fbllipb, fblss, fblvd, fwclfr, vfblkt, vfshld
    use heat_transport_variables, only: ipowerflow
    use pfcoil_variables, only: fcuohsu, ipfres, isumatoh, nohc, vfohc, zl, &
      rpf, zh, wts, ra, rb
    use physics_variables, only: rminor
    use process_output, only: oheadr, ovarre
    use tfcoil_variables, only: acasetf, acond, aswp, avwp, fcoolcp, fcutfsu, &
      i_tf_sup, n_tf, vftf, i_tf_sc_mat
    use constants, only: pi

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables
    real(kind(1.0D0)) :: ac,as,av,a1
    integer :: i,k,ibc,itf,ioh

    integer, parameter ::  nreg = 24, nmat = 13

    !  Global variables passed via COMMON

    real(kind(1.0D0)), dimension(nreg) :: qvol    !  decay heat power in each region (W)
    real(kind(1.0D0)), dimension(nreg) :: radmid  !  mean radial coord of each region (m)
    real(kind(1.0D0)), dimension(nreg) :: radmin  !  inside radial coord of each region (m)
    real(kind(1.0D0)), dimension(nreg) :: radpls  !  outside radial coord of each region (m)
    real(kind(1.0D0)), dimension(nreg,nmat) :: matfrc  !  material fractions in each region
    common/geom/radmin,radpls,radmid,matfrc,qvol

    real(kind(1.0D0)) :: alpha    !  heat transfer coefficient at outer boundary
    real(kind(1.0D0)) :: tair     !  ambient air temperature (K)
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
    ! -------------
    ! Bore centre
    radmin(1) = 1.0D-20
    radpls(1) = radmin(1) + bore  

    ! CS coil
    radmin(2) = radpls(1)
    radpls(2) = radmin(2) + ohcth  
    
    ! CS precompresser
    radmin(3) = radpls(2)
    radpls(3) = radmin(3) + precomp

    ! Inner CS precompressor - TF gap
    radmin(4) = radpls(3) + gapoh
    radpls(4) = radmin(4)

    ! Inner TF coil
    radmin(5) = radpls(4)
    radpls(5) = radmin(5) + tfcth

    ! Inner TF - thermal shield gap    
    radmin(6) = radpls(5) + tftsgap
    radpls(6) = radmin(6)

    ! Inner thermal shield 
    radmin(7) = radpls(6)
    radpls(7) = radmin(7) + thshield

    ! Inner Thermal shiled - Vaccum vessel gap
    radmin(8) = radpls(7) + gapds
    radpls(8) = radmin(8)

    ! Inner Vaccum vessel
    radmin(9) = radpls(8)
    radpls(9) = radmin(9) + d_vv_in

    ! Inner neutron shield
    radmin(10) = radpls(9)
    radpls(10) = radmin(10) + shldith

    ! Inner VV - (tritium breeding) blanket gap
    radmin(11) = radpls(10) + vvblgap
    radpls(11) = radmin(11)
    
    ! Inner (tritium breeding) blanket
    radmin(12) = radpls(11)
    radpls(12) = radmin(12) + blnkith

    ! Inner first wall
    radmin(13) = radpls(12)
    radpls(13) = radmin(13) + fwith

    ! Plasma (SOL+CORE)
    radmin(14) = radpls(13)
    radpls(14) = radmin(14) + scrapli + 2.0D0*rminor + scraplo

    ! Outer first wall
    radmin(15) = radpls(14)
    radpls(15) = radmin(15) + fwoth

    ! Outer (tritium breeding) blanket
    radmin(16) = radpls(15)
    radpls(16) = radmin(16) + blnkoth

    ! Outer Blanket - VV gap
    radmin(17) = radpls(16) + vvblgap
    radpls(17) = radmin(17) 

    ! Outer neutron shield
    radmin(18) = radpls(17)
    radpls(18) = radmin(18) + shldoth

    ! Outer VV
    radmin(19) = radpls(18)
    radpls(19) = radmin(19) + d_vv_out

    ! Outer VV - thermal shield gap 
    radmin(20) = radpls(19) + gapsto
    radpls(20) = radmin(20)

    ! Outer thermal shield 
    radmin(21) = radpls(20)
    radpls(21) = radmin(21) + thshield

    ! Outer themal shiled - TF gap
    radmin(22) = radpls(21) + tftsgap
    radpls(22) = radmin(22)

    ! Outer TF coil
    radmin(23) = radpls(22)
    radpls(23) = radmin(23) + tfthko

    ! Cryostat, 2 meters away from TF coil
    radmin(24) = radpls(23) + 2.0D0
    radpls(24) = radmin(24) + ddwex
    ! -------------
    
   
    ! Computing the middle radial position of each region and var initialization 
    do k = 1,nreg
       radmid(k) = 0.5D0 * (radmin(k) + radpls(k))
       qvol(k) = 0.0D0
       do i = 1,nmat
          matfrc(k,i) = 0.0D0
       end do
    end do
   

    ! Layers compositions 
    ! !!!!!!!!!!!!!!!!!!!
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


    !  Machine bore
    ! -------------
    matfrc(1,5) = 1.0D0
    ! -------------


    !  Central Solenoid
    !------------------
    if (iohcl == 1) then

       !  Copper resistive CS coils
       ! ******
       if (ipfres == 1) then  
          matfrc(2,5) = vfohc
          matfrc(2,7) = 1.0D0 - vfohc
       ! ******

       !  superconducting CS coils
       ! ******
       else 
          !  Conductor + void areas
          a1 = (rb(nohc)-ra(nohc))*(zh(nohc)-zl(nohc))
          ac = a1 * (1.0D0 - vfohc)
          av = a1 * vfohc

          !  Steel cross-sectional area
          as = wts(nohc) / (2.0D0*pi*rpf(nohc) * denstl)

          matfrc(2,1) = as / (ac+av+as) * fmsoh
          matfrc(2,2) = as / (ac+av+as) * (1.0D0 - fmsoh)
          matfrc(2,5) = av / (ac+av+as)
          matfrc(2,7) = ac / (ac+av+as) * fcuohsu

          if (isumatoh /= 3) then  !  treat generic superconductors like Nb3Sn
             matfrc(2,12) = ac / (ac+av+as) * (1.0D0 - fcuohsu)
          else
             matfrc(2,13) = ac / (ac+av+as) * (1.0D0 - fcuohsu)
          end if
       end if
       ! ******
    
    ! No CS coils
    ! ******
    else
       matfrc(2,5) = 1.0D0
    end if
    ! ******
    !------------------

    ! CS coil precompressor
    matfrc(3,2) = 1.0D0    ! Austenistic steel
    
    ! CS coil precompressor - TF gap
    matfrc(4,5) = 1.0D0    ! Dummy


    !  Inboard TF coil
    ! ----------------
    ! resistive copper TF coils
    if ( i_tf_sup /= 1 ) then  
      matfrc(5,5) = fcoolcp         ! Dummy 
      matfrc(5,7) = 1.0D0 - fcoolcp ! Copper

    !  superconducting TF coils
    else  
      !  Approximate total cross-sectional area per coil
      a1 = acasetf+acond+avwp+aswp

      matfrc(5,1) = (acasetf + aswp) / a1 * fmstf
      matfrc(5,2) = (acasetf + aswp) / a1 * (1.0D0 - fmstf)
      matfrc(5,5) = avwp / a1
      matfrc(5,7) = acond / a1 * fcutfsu

      if (i_tf_sc_mat /= 3) then  !  treat generic superconductors like Nb3Sn
         matfrc(5,12) = acond / a1 * (1.0D0 - fcutfsu)
      else
         matfrc(5,13) = acond / a1 * (1.0D0 - fcutfsu)
      end if
   end if
   ! ----------------


    ! Inner TF - Thermal shield gap
    matfrc(6,5) = 1.0D0    ! Dummy

    ! Inner Thermal shield (same composition as neutron shield)
    matfrc(7,1) = (1.0D0 - vfshld) * fmssh
    matfrc(7,2) = (1.0D0 - vfshld) * (1.0D0 - fmssh)
    matfrc(7,5) = vfshld

    ! Inner Thermal shield - VV gap
    matfrc(8,5) = 1.0D0    ! Dummy

    ! Inner VV
    matfrc(9,1) = fmsdwi
    matfrc(9,2) = 1.0D0 - fmsdwi

    !  Inner neutron shield (same composition as thermal shield)
    matfrc(10,1) = (1.0D0 - vfshld) * fmssh
    matfrc(10,2) = (1.0D0 - vfshld) * (1.0D0 - fmssh)
    matfrc(10,5) = vfshld

    ! Inner VV + neutron shield - (tritium breeding) blanket gap 
    matfrc(11,5) = 1.0D0    ! Dummy


    !  Inboard blanket
    ! ----------------
    !if (lblnkt /= 1) then 
    !  Old blanket model
    ! ******
    if (ipowerflow == 0) then
       matfrc(12,1) = fblss * fmsbl
       matfrc(12,2) = fblss * (1.0D0-fmsbl)
       matfrc(12,4) = fblbe
       matfrc(12,5) = vfblkt
       matfrc(12,9) = fblli2o
       matfrc(12,11) = fblvd
    ! ******

    !  New blanket model
    ! ******
    else  
       !  Li2O/Be solid blanket
       if (blkttype == 3) then
          matfrc(12,1) = fblss * fmsbl
          matfrc(12,2) = fblss * (1.0D0-fmsbl)
          matfrc(12,4) = fblbe
          matfrc(12,5) = vfblkt
          matfrc(12,9) = fblli2o
          matfrc(12,11) = fblvd

       !  LiPb/Li liquid blanket
       else
          matfrc(12,1) = fblss * fmsbl
          matfrc(12,2) = fblss * (1.0D0-fmsbl)
          matfrc(12,3) = fbllipb
          matfrc(12,5) = vfblkt
          matfrc(12,10) = fblli
          matfrc(12,11) = fblvd
       end if
    end if
    ! ******
    ! ----------------


    !  Inboard first wall
    matfrc(13,1) = (1.0D0 - fwclfr) * fmsfw
    matfrc(13,2) = (1.0D0 - fwclfr) * (1.0D0 - fmsfw)
    matfrc(13,5) = fwclfr

    !  Plasma
    matfrc(14,6) = 1.0D0

    ! Outer first wall
    matfrc(15,1) = (1.0D0 - fwclfr) * fmsfw
    matfrc(15,2) = (1.0D0 - fwclfr) * (1.0D0 - fmsfw)
    matfrc(15,5) = fwclfr

    ! Outer blanket (same as inboard blanket)
    matfrc(16,:) = matfrc(12,:)

    ! Outer blanket - neutron shield gap
    matfrc(17,5) = 1.0D0  ! Dummy

    ! Outer neutron shield
    matfrc(18,1) = (1.0D0 - vfshld) * fmssh
    matfrc(18,2) = (1.0D0 - vfshld) * (1.0D0 - fmssh)
    matfrc(18,5) = vfshld

    ! Outer VV
    matfrc(19,1) = fmsdwi
    matfrc(19,2) = 1.0D0 - fmsdwi

    ! Outer VV - thermal shield gap
    matfrc(20,5) = 1.0D0   ! Dummy

    ! Outer Thermal shield (same composition as neutron shield)
    matfrc(21,1) = (1.0D0 - vfshld) * fmssh
    matfrc(21,2) = (1.0D0 - vfshld) * (1.0D0 - fmssh)
    matfrc(21,5) = vfshld

    ! Outer thermal shield - TF coil gap
    matfrc(22,5) = 1.0D0   ! Dummy


    ! Outboard TF coil
    ! ----------------
    !  resistive copper TF coils
    if ( i_tf_sup /= 1 ) then 
       matfrc(23,5) = vftf
       matfrc(23,7) = 1.0D0 - vftf

    ! superconducting TF coils
    ! ******
    else  
       !  Approximate total cross-sectional area per coil
       !  including enlargement factor for case thickness
       a1 = (acasetf*tfootfi)+acond+avwp+aswp

       matfrc(23,1) = (acasetf*tfootfi + aswp) / a1 * fmstf
       matfrc(23,2) = (acasetf*tfootfi + aswp) / a1 * (1.0D0 - fmstf)
       matfrc(23,5) = avwp / a1
       matfrc(23,7) = acond / a1 * fcutfsu

       if (i_tf_sc_mat /= 3) then  !  treat generic superconductors like Nb3Sn
          matfrc(23,12) = acond / a1 * (1.0D0 - fcutfsu)
       else
          matfrc(23,13) = acond / a1 * (1.0D0 - fcutfsu)
       end if
    end if
    ! ******
    ! ----------------

    !  External cryostat
    matfrc(24,1) = fmsdwe
    matfrc(24,2) = 1.0D0 - fmsdwe 
    ! !!!!!!!!!!!!!!!!!!!


    ! Non-zero decay heat powers (W) after 3 months
    qvol(12) = blihkw(2) * 1.0D3  ! Inner blanket
    qvol(13) = fwihkw(2) * 1.0D3  ! Inner FW
    qvol(15) = fwohkw(2) * 1.0D3  ! Outer FW
    qvol(16) = blohkw(2) * 1.0D3  ! Outer blanket


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

      !! Fusion power plant heat transfer module
      !! author: C B A Forty, Culham Laboratory
      !! author: W E Han, CCFE, Culham Science Centre
      !! author: P J Knight, CCFE, Culham Science Centre
      !! fwtemp : output real :  Outboard first wall temperature (K)
      !! This routine calculates the steady state temperatures that
      !! would develop following a loss of coolant accident.
      !! Cylindrical geometry is assumed.
      !! F/MI/PJK/LOGBOOK12, pp.70,71,72,73
      !! Strategic Studies Note 96/30, January 1997
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(out) :: fwtemp

      !  Local variables

      integer, parameter :: nreg = 24  !  dimensioning of radial nodes
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

      !! Root finding routine for the LOCA model
      !! author: C B A Forty, Culham Laboratory
      !! author: W E Han, CCFE, Culham Science Centre
      !! author: P J Knight, CCFE, Culham Science Centre
      !! q : input real : heat power (W)
      !! x : output real : temperature (K)
      !! a : input real : heat transfer coefficient
      !! t : input real : ambient air temperature (K)
      !! This routine calculates the temperature at a layer given
      !! the local thermal characteristics, via a Newton-Raphson
      !! root-finding method.
      !! A First Course in Computational Physics - Paul L DeVries,
      !! John Wiley & Sons, New York 1994: Chapter 2, p.54
      !! Strategic Studies Note 96/30, January 1997
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

      !! Radiation power function for the LOCA model
      !! author: C B A Forty, Culham Laboratory
      !! author: W E Han, CCFE, Culham Science Centre
      !! author: P J Knight, CCFE, Culham Science Centre
      !! x : input real : temperature (K)
      !! q : input real : heat power (W)
      !! a : input real : heat transfer coefficient
      !! t : input real : ambient air temperature (K)
      !! This routine evaluates the radiation power function...
      !! A First Course in Computational Physics - Paul L DeVries,
      !! John Wiley & Sons, New York 1994: Chapter 2, p.54
      !! Strategic Studies Note 96/30, January 1997
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

    function deroff(x,a)

      !! Derivative of function fofx with respect to x
      !! author: C B A Forty, Culham Laboratory
      !! author: W E Han, CCFE, Culham Science Centre
      !! author: P J Knight, CCFE, Culham Science Centre
      !! x : input real : temperature (K)
      !! a : input real : heat transfer coefficient
      !! This routine evaluates the derivative with respect to x
      !! of the function <A HREF="fofx.html">fofx</A>.
      !! A First Course in Computational Physics - Paul L DeVries,
      !! John Wiley & Sons, New York 1994: Chapter 2, p.54
      !! Strategic Studies Note 96/30, January 1997
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: deroff

      !  Arguments

      real(kind(1.0D0)), intent(in) :: x,a

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      deroff  = a + 4.0D0 * 3.5D-8*x**3

    end function deroff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine newton(x,f,fprime,q,a,t)

      !! Newton-Raphson root-finder for the LOCA model
      !! author: C B A Forty, Culham Laboratory
      !! author: W E Han, CCFE, Culham Science Centre
      !! author: P J Knight, CCFE, Culham Science Centre
      !! x : input/output real : x coordinate
      !! f : real function : f(x)
      !! fprime : real function : derivative d/dx (f(x))
      !! q : input real : heat power (W)
      !! a : input real : heat transfer coefficient
      !! t : input real : ambient air temperature (K)
      !! This routine is the Newton-Raphson root-finder for the
      !! LOCA model.
      !! A First Course in Computational Physics - Paul L DeVries,
      !! John Wiley & Sons, New York 1994: Chapter 2, p.54
      !! Strategic Studies Note 96/30, January 1997
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
         delta = -f(x,q,a,t) / fprime(x,a)
         x = x + delta

         error = abs(delta/x)
         if (error <= tol) exit
      end do

    end subroutine newton

  end subroutine loca
