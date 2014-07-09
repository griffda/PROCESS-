! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pfcoil_module

  !+ad_name  pfcoil_module
  !+ad_summ  Module containing PF coil routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  R Kemp, CCFE, Culham Science Centre
  !+ad_cont  pfcoil
  !+ad_cont  ohcalc
  !+ad_cont  efc
  !+ad_cont  bfield
  !+ad_cont  peakb
  !+ad_cont  bfmax
  !+ad_cont  waveform
  !+ad_cont  pfjalw
  !+ad_cont  vsec
  !+ad_cont  induct
  !+ad_cont  outpf
  !+ad_cont  outvolt
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the PF coil systems for a fusion power plant.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  maths_library
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Moved local common variables into module header
  !+ad_hist  15/04/13 PJK Added fwbs_variables
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use error_handling
  use fwbs_variables
  use maths_library
  use pfcoil_variables
  use physics_variables
  use process_output
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: pfcoil, outpf, outvolt, induct, vsec, bfield

  !  Local variables

  integer :: nef,nfxf
  real(kind(1.0D0)) :: ricpf, ssq0
  real(kind(1.0D0)), dimension(nfixmx) :: rfxf,zfxf,cfxf,xind
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls,zcls
  real(kind(1.0D0)), dimension(ngrpmx) :: ccls,ccls2,ccl0
  real(kind(1.0D0)), dimension(ngc2) :: bpf2
  real(kind(1.0D0)), dimension(ngc2,3) :: vsdum

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pfcoil

    !+ad_name  pfcoil
    !+ad_summ  Routine to perform calculations for the PF and OH coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine performs the calculations for the PF and
    !+ad_desc  OH coils, to determine their size, location, current waveforms,
    !+ad_desc  stresses etc.
    !+ad_prob  On the very first call, the inductance matrix sxlg has not
    !+ad_prob  previously been calculated by routine induct, so could in theory
    !+ad_prob  contain any values... This is currently dealt with by setting
    !+ad_prob  the matrix to unity during the first call to prevent problems.
    !+ad_call  efc
    !+ad_call  ohcalc
    !+ad_call  peakb
    !+ad_call  pfjalw
    !+ad_call  report_error
    !+ad_call  waveform
    !+ad_hist  01/02/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  11/10/12 PJK Removed work1 argument from efc
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/12/12 PJK/RK Added single-null coding
    !+ad_hist  08/04/13 PJK Comment change
    !+ad_hist  15/04/13 PJK Modified PF coil case area for superconducting coils
    !+ad_hist  16/04/13 PJK Replaced sigpfalw by sigpfcalw in above calculation
    !+ad_hist  17/04/13 PJK Removed cohbof calculation
    !+ad_hist  26/11/13 PJK Added fix for first lap inductance matrix values;
    !+ad_hisc               new (but commented-out) CS flux swing requirement calc.
    !+ad_hist  27/11/13 PJK Moved pfrmax, pfmmax calculations from buildings module
    !+ad_hist  12/02/14 PJK Added turns array to first lap fix
    !+ad_hist  23/04/14 PJK Added bvert assignment
    !+ad_hist  01/05/14 PJK Removed redundant xctfc(5) terms
    !+ad_hist  24/06/14 PJK Removed refs to bcylth
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer, parameter :: lrow1 = 2*nptsmx + ngrpmx
    integer, parameter :: lcol1 = ngrpmx

    integer :: i,ii,iii,ij,it,j,k,ncl,nfxf0,ng2,ngrp0,nng,nocoil,npts,npts0
    integer :: ccount, snswit
    integer, dimension(ngrpmx) :: pcls0
    integer, dimension(ngrpmx+2) :: ncls0

    real(kind(1.0D0)) :: area,areaspf,bri,bro,bzi,bzo,curstot,drpt, &
         dx,dz,forcepf,rclsnorm,respf,rll,rpt0,ssqef,volpf, &
         pfflux,csflux,dics,ddics
    real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls0,zcls0
    real(kind(1.0D0)), dimension(ngrpmx/2) :: ccls0
    real(kind(1.0D0)), dimension(ngrpmx) :: sigma,work2
    real(kind(1.0D0)), dimension(nclsmx) :: rc,zc,cc,xc
    real(kind(1.0D0)), dimension(nptsmx) :: brin,bzin,rpts,zpts
    real(kind(1.0D0)), dimension(lrow1) :: bfix,bvec
    real(kind(1.0D0)), dimension(lrow1,lcol1) :: gmat,umat,vmat
    real(kind(1.0D0)), dimension(2) :: signn

    logical :: first_call = .true.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Single-null configuration switch

    snswit = 1

    !  Set up the number of PF coils including the OH coil (nohc),
    !  and the number of PF circuits including the plasma (ncirt)

    if (ngrp > ngrpmx) then
       idiags(1) = ngrp ; idiags(2) = ngrpmx
       call report_error(64)
    end if

    !  Total the number of PF coils in all groups, and check that none
    !  exceeds the limit

    nohc = 0
    do i = 1,ngrp
       if (ncls(i) > nclsmx) then
          idiags(1) = i ; idiags(2) = ncls(i) ; idiags(3) = nclsmx
          call report_error(65)
       end if
       nohc = nohc + ncls(i)
    end do

    !  Add one if an OH coil is present, and make an extra group

    if (iohcl /= 0) then 
       nohc = nohc + 1
       ncls(ngrp+1) = 1
    end if

    !  Add one for the plasma

    ncirt = nohc + 1

    !  Current in the OH coil at beginning of pulse

    cohbop = coheof * fcohbop

    !  Set up array of times

    tim(1) = 0.0D0
    tim(2) = tramp
    tim(3) = tim(2) + tohs
    tim(4) = tim(3) + theat
    tim(5) = tim(4) + tburn
    tim(6) = tim(5) + tqnch

    !  Set up call to MHD scaling routine for coil currents.
    !  First break up OH solenoid into 'filaments'

    !  OH coil radius

    if (itart == 1) then
       rohc = bore + tfcth + gapoh + 0.5D0*ohcth
    else
       rohc = bore + 0.5D0*ohcth
    end if

    !  nfxf is the total no of filaments into which the OH coil is split,
    !  if present

    if (iohcl == 0) then
       nfxf = 0
       curstot = 0.0D0
    else
       nfxf = 2*nfxfh
       curstot = -hmax*ohhghf*ohcth*2.0D0*coheof

       if (nfxf > nfixmx) then
          idiags(1) = nfxf ; idiags(2) = nfixmx
          call report_error(66)
       end if

       !  Symmetric up/down OH coil : Find (R,Z) and current of each filament

       do nng = 1,nfxfh
          rfxf(nng) = rohc
          rfxf(nng+nfxfh) = rfxf(nng)
          zfxf(nng) = hmax*ohhghf/nfxfh * (nng-0.5D0)
          zfxf(nng+nfxfh) = -zfxf(nng)
          cfxf(nng) = -curstot/nfxf * fcohbop
          cfxf(nng+nfxfh) = cfxf(nng)
       end do
    end if

    !  Scale PF coil locations

    signn(1) =  1.0D0
    signn(2) = -1.0D0
    rclsnorm = rtot + tfthko/2.0D0 + routr

    !  N.B. Problems here if k=ncls(group) is greater than 2.

    do j = 1,ngrp

       if (ipfloc(j) == 1) then

          !  PF coil is stacked on top of the OH coil

          do k = 1,ncls(j)
             rcls(j,k) = rohc + rpf1

             !  Z coordinate of coil enforced so as not 
             !  to occupy the same space as the OH coil

             zcls(j,k) = signn(k) * ( hmax*ohhghf + 0.1D0 + &
                  0.5D0 * ( hmax*(1.0D0-ohhghf) + tfcth + 0.1D0) )
          end do

       else if (ipfloc(j) == 2) then

          !  PF coil is on top of the TF coil

          do k = 1,ncls(j)
             rcls(j,k) = rmajor + rpf2*triang*rminor
             if (itart == 1) then
                zcls(j,k) = (hmax-zref(j)) * signn(k)
             else
                !zcls(j,k) = (hmax + tfcth + 0.86D0) * signn(k)
                if (snswit == 1) then
                   zcls(j,k) = hpfu + 0.86D0
                   snswit = -1 * snswit
                else
                   zcls(j,k) = -1.0D0 * (hpfu - 2.0D0*hpfdif + 0.86D0)
                   snswit = -1 * snswit
                end if
             end if
          end do

       else if (ipfloc(j) == 3) then

          !  PF coil is radially outside the TF coil

          do k = 1,ncls(j)
             zcls(j,k) = rminor * zref(j) * signn(k)
             !  Changed to follow TF coil curve
             !  rcls(j,k) = rtot + 0.5D0*tfthko + routr
             rcls(j,k) = sqrt(rclsnorm**2 - zcls(j,k)**2)
          end do

       else
          idiags(1) = j ; idiags(2) = ipfloc(j)
          call report_error(67)
       end if

    end do

    if (cohbop /= 0.0D0) then

       !  Find currents for plasma initiation to null field across plasma

       npts = 32  !  Number of test points across plasma midplane
       if (npts > nptsmx) then
          idiags(1) = npts ; idiags(2) = nptsmx
          call report_error(68)
       end if

       !  Position and B-field at each test point

       drpt = 2.0D0 * rminor / (npts-1)
       rpt0 = rmajor - rminor

       do i = 1,npts
          rpts(i) = rpt0 + (i-1)*drpt
          zpts(i) = 0.0D0
          brin(i) = 0.0D0
          bzin(i) = 0.0D0
       end do

       !  Calculate currents in coils to produce the given field

       call efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts,rpts, &
            zpts,brin,bzin,nfxf,rfxf,zfxf,cfxf,ngrp,ncls,rcls,zcls, &
            alfapf,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat,sigma, &
            work2,ssq0,ccl0)

    end if

    !  Simple coil current scaling for TARTs (good only for A < about 1.8)

    if (itart == 1) then

       do i = 1,ngrp

          if (ipfloc(i) == 1) then

             !  PF coil is stacked on top of the OH coil

             ccls(i) = 0.0D0
             idiags(1) = i ; call report_error(69)

          else if (ipfloc(i) == 2) then

             !  PF coil is on top of the TF coil

             ccls(i) = 0.3D0 * aspect**1.6D0 * plascur

          else if (ipfloc(i) == 3) then

             !  PF coil is radially outside the TF coil

             ccls(i) = -0.4D0 * plascur

          else
             idiags(1) = i ; idiags(2) = ipfloc(i)
             call report_error(70)
          end if

       end do

       !  Vertical field (T)

       bvert = -1.0D-7 * plascur/rmajor * &
            (log(8.0D0*aspect) + betap + (rli/2.0D0) - 1.5D0)

    else

       !  Conventional aspect ratio scaling

       nfxf0 = 0 ; ngrp0 = 0 ; nocoil = 0
       do i = 1,ngrp

          if (ipfloc(i) == 1) then

             !  PF coil is stacked on top of the OH coil
             !  This coil is to balance OH coil flux and should not be involved
             !  in equilibrium calculation -- RK 07/12
             !  This is a fixed current for this calculation
             !  ccls(i) = 0.2D0 * plascur

             ccls(i) = 0.0D0
             nfxf0 = nfxf0 + ncls(i)
             do ccount = 1,ncls(i)
                nocoil = nocoil + 1
                rfxf(nocoil) = rcls(i, ccount)
                zfxf(nocoil) = zcls(i, ccount)
                cfxf(nocoil) = ccls(i)
             end do

          else if (ipfloc(i) == 2) then

             !  PF coil is on top of the TF coil; divertor coil
             !  This is a fixed current for this calculation -- RK 07/12
             !  ccls(i) = 0.2D0 * plascur

             ccls(i) = plascur * 2.0D0 * &
                  ( 1.0D0 - (kappa * rminor)/abs(zcls(i,1)) )
             nfxf0 = nfxf0 + ncls(i)
             do ccount = 1, ncls(i)
                nocoil = nocoil + 1
                rfxf(nocoil) = rcls(i, ccount)
                zfxf(nocoil) = zcls(i, ccount)
                cfxf(nocoil) = ccls(i)
             end do

          else if (ipfloc(i) == 3) then

             !  PF coil is radially outside the TF coil
             !  This is a free current and must be solved for

             ngrp0 = ngrp0 + 1
             pcls0(ngrp0) = i
	    
          else
             idiags(1) = i ; idiags(2) = ipfloc(i)
             call report_error(70)
          end if

       end do

       do ccount = 1, ngrp0
          ncls0(ccount) = 2
          rcls0(ccount,1) = rcls(pcls0(ccount),1)
          rcls0(ccount,2) = rcls(pcls0(ccount),2)
          zcls0(ccount,1) = zcls(pcls0(ccount),1)
          zcls0(ccount,2) = zcls(pcls0(ccount),2)
       end do

       npts0 = 1
       rpts(1) = rmajor
       zpts(1) = 0.0D0
       brin(1) = 0.0D0

       !  Added rli term correctly -- RK 07/12

       bzin(1) = -1.0D-7 * plascur/rmajor * &
            (log(8.0D0*aspect) + betap + (rli/2.0D0) - 1.5D0)

       bvert = bzin(1)

       call efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts0, &
            rpts,zpts,brin,bzin,nfxf0,rfxf,zfxf,cfxf,ngrp0,ncls0, &
            rcls0,zcls0,alfapf,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat, &
            sigma,work2,ssqef,ccls0)

       do ccount = 1,ngrp0
          ccls(pcls0(ccount)) = ccls0(ccount)
       end do

    end if
      
    !  Flux swing from vertical field

    !  If this is the first visit to the routine the inductance matrix
    !  sxlg and the turns array have not yet been calculated, so we set
    !  them to (very) approximate values to avoid strange behaviour...

    if (first_call) then
       sxlg(:,:) = 1.0D0
       turns(:) = 100.0D0
       first_call = .false.
    end if

    pfflux = 0.0D0
    nocoil = 0
    do ccount = 1, ngrp
       do i = 1, ncls(ccount)
          nocoil = nocoil + 1
          pfflux = pfflux + (ccls(ccount) * sxlg(nocoil,ncirt) &
               / turns(nocoil))
       end do
    end do

    !  Flux swing required from CS coil

    csflux = -(vsres + vsind) - pfflux 

    if (iohcl == 1) then

       !  Required current change in CS coil

       !  Proposed new calculation...
       !dics = csflux / sxlg(nohc,ncirt)
       !  BUT... sxlg(nohc,ncirt) is around 2000 times ddics below...

       ddics = 4.0D-7 * pi*pi * ( &
            (bore*bore) + (ohcth*ohcth)/6.0D0 + (ohcth*bore)/2.0D0 ) &
            / (hmax*ohhghf*2.0D0)
       dics = csflux / ddics

    else
       dics = 0.0D0
       call report_error(71)
    end if
      
    fcohbof = ((-curstot * fcohbop) + dics)/curstot
    fcohbof = min(fcohbof,  1.0D0)
    fcohbof = max(fcohbof, -1.0D0)

    !  Split groups of coils into one set containing ncl coils

    ncl = 0
    do nng = 1,ngrp
       do ng2 = 1,ncls(nng)
          ncl = ncl + 1
          rpf(ncl) = rcls(nng,ng2)
          zpf(ncl) = zcls(nng,ng2)

          !  Currents at different times:

          !  Beginning of pulse: t = tramp
          curpfs(ncl) = 1.0D-6 * ccl0(nng)

          !  Beginning of flat-top: t = tramp+tohs
          curpff(ncl) = 1.0D-6 * (ccls(nng) - (ccl0(nng) * fcohbof/fcohbop))

          !  End of flat-top: t = tramp+tohs+theat+tburn
          curpfb(ncl) = 1.0D-6 * (ccls(nng) - (ccl0(nng) * (1.0D0/fcohbop)))
       end do
    end do

    !  Current in OH coil as a function of time
    !  N.B. If the OH coil is not present then curstot is zero.

    curpfs(ncl+1) = -1.0D-6 * curstot * fcohbop
    curpff(ncl+1) = 1.0D-6 * curstot * fcohbof
    curpfb(ncl+1) = 1.0D-6 * curstot

    !  Set up coil current waveforms, normalised to the peak current in
    !  each coil

    call waveform

    !  Calculate PF coil geometry, current and number of turns

    i = 0
    pfrmax = 0.0D0

    do ii = 1,ngrp
       do ij = 1,ncls(ii)
          i = i + 1

          if (ipfloc(ii) == 1) then

             !  PF coil is stacked on top of the OH coil

             dx = 0.5D0 * ohcth
             dz = 0.5D0 * (hmax*(1.0D0-ohhghf) + tfcth + 0.1D0)  !  ???
             area = 4.0D0 * dx * dz

             !  Actual current density

             rjconpf(i) = 1.0D6*abs(ric(i))/area

             !  Location of edges of each coil:
             !  ra = inner radius, rb = outer radius
             !  zl = 'lower' edge z (i.e. edge nearer to midplane)
             !  zh = 'upper' edge z (i.e. edge further from midplane)

             ra(i) = rpf(i) - dx
             rb(i) = rpf(i) + dx

             zl(i) = zpf(i) - dz
             if (zpf(i) < 0.0D0) zl(i) = zpf(i) + dz

             zh(i) = zpf(i) + dz
             if (zpf(i) < 0.0D0) zh(i) = zpf(i) - dz

          else

             !  Other coils. N.B. Current density RJCONPF(I) is defined in
             !  routine INITIAL for these coils.

             area = abs(ric(i)*1.0D6/rjconpf(i))
             dx = 0.5D0 * sqrt(area)  !  square cross-section

             ra(i) = rpf(i) - dx
             rb(i) = rpf(i) + dx

             zl(i) = zpf(i) - dx
             if (zpf(i) < 0.0D0) zl(i) = zpf(i) + dx

             zh(i) = zpf(i) + dx
             if (zpf(i) < 0.0D0) zh(i) = zpf(i) - dx

          end if

          !  Calculate number of turns
          !  CPTDIN(I) is the current per turn, as defined in routine INITIAL.

          turns(i) = abs( (ric(i)*1.0D6)/cptdin(i) )

          !  Outside radius of largest PF coil (m)

          pfrmax = max(pfrmax, rb(i))

       end do
    end do

    !  Calculate peak field, allowable current density, resistive
    !  power losses and volumes and weights for each PF coil

    i = 0
    powpfres = 0.0D0
    pfmmax = 0.0D0

    do ii = 1,ngrp
       iii = ii
       do ij = 1,ncls(ii)
          i = i + 1

          !  Peak field

          if (ij == 1) call peakb(i,iii,it,bri,bro,bzi,bzo)

          !  Allowable current density

          rjpfalw(i) = pfjalw(bpf(i),bpf2(i),ra(i),rb(i),sigpfalw)

          !  Length of conductor

          rll = 2.0D0*pi*rpf(i)

          !  Resistive coils

          if (ipfres == 1) then
             area = abs(ric(i)*1.0D6/rjconpf(i))

             !  Coil resistance (vf is the void fraction)

             respf = pfclres * rll / ( area * (1.0D0-vf(i)) )

             !  Sum resistive power losses

             powpfres = powpfres + respf * (1.0D6 * curpfb(i))**2
          end if

          !  Coil volume

          volpf = abs(ric(i)*1.0D6/rjconpf(i)) * rll

          !  Coil weight (vf is the void fraction, conductor density is 8990.0)

          wtc(i) = volpf * 8990.0D0 * (1.0D0-vf(i))

          !  (J x B) force on coil

          forcepf = 0.5D6 * (bpf(i) + bpf2(i)) * abs(ric(i)) * rpf(i)

          !  Stress ==> cross-sectional area of supporting steel to use

          if (ipfres /= 1) then

             !  Superconducting coil
             !  Previous assumptions: 500 MPa stress limit with 2/3 of the force
             !  supported in the outer (steel) case.
             !  Now, 500 MPa replaced by sigpfcalw, 2/3 factor replaced by sigpfcf

             areaspf = sigpfcf * forcepf / (sigpfcalw*1.0D6)

          else
             areaspf = 0.0D0  !  Resistive coil - no steel needed
          end if

          !  Weight of steel

          wts(i) = areaspf * 2.0D0*pi*rpf(i) * denstl

          !  Mass of heaviest PF coil (tonnes)

          pfmmax = max(pfmmax, (1.0D-3*(wtc(i)+wts(i))) )

       end do
    end do

    !  Find OH coil information

    if (iohcl /= 0) call ohcalc

    !  Summation of weights and current

    whtpf = 0.0D0 ; whtpfs = 0.0D0 ; ricpf = 0.0D0
    do i = 1,nohc
       whtpf = whtpf + wtc(i)
       whtpfs = whtpfs + wts(i)
       ricpf = ricpf + abs(ric(i))
    end do

    !  Plasma size and shape

    zh(nohc+1) =  rminor*kappa
    zl(nohc+1) = -rminor*kappa
    ra(nohc+1) = rmajor - rminor
    rb(nohc+1) = rmajor + rminor
    turns(nohc+1) = 1.0D0

    !  Generate coil currents as a function of time using
    !  user-provided waveforms etc. (cptdin, fcohbop, fcohbof)

    do k = 1,6  !  time points
       do i = 1,ncirt-1
          cpt(i,k) = waves(i,k) * sign(cptdin(i), ric(i))
       end do
    end do

    !  Plasma wave form

    cpt(ncirt,1) = 0.0D0
    cpt(ncirt,2) = 0.0D0
    cpt(ncirt,3) = plascur
    cpt(ncirt,4) = plascur
    cpt(ncirt,5) = plascur
    cpt(ncirt,6) = 0.0D0

  end subroutine pfcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ohcalc

    !+ad_name  ohcalc
    !+ad_summ  Routine to perform calculations for the OH solenoid
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine performs the calculations for the
    !+ad_desc  OH solenoid coil.
    !+ad_prob  None
    !+ad_call  bfmax
    !+ad_call  peakb
    !+ad_call  pfjalw
    !+ad_hist  01/02/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  25/11/13 PJK Simplified (R,Z) calculation
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: itt,nohc1

    real(kind(1.0D0)) :: areaspf,aroh,bmaxoh2,bohci,bohco,bri,bro, &
         bzi,bzo,forcepf,hohc,sgn,volohc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Height of OH coil

    hohc = hmax * ohhghf

    !  Z coordinates of coil edges

    zh(nohc) = hohc
    zl(nohc) = -zh(nohc)

    !  (R,Z) coordinates of coil centre

    rpf(nohc) = rohc
    zpf(nohc) = 0.0D0

    !  Radius of outer edge

    rb(nohc) = rohc + 0.5D0*ohcth

    !  Radius of inner edge

    ra(nohc) = rb(nohc) - ohcth

    !  Cross-sectional area

    aroh = 2.0D0 * hohc * ohcth

    !  Peak field due to OH coil

    bmaxoh2 = bfmax(coheof,ra(nohc),rb(nohc),hohc)

    !  Peak field due to all other coils as well

    nohc1 = nohc
    itt = 5 ; call peakb(nohc1,99,itt,bri,bro,bzi,bzo)

    !  Peak field at the End-Of-Flattop (EOF)

    bmaxoh = sqrt ( (-bmaxoh2+bzi)**2 )
    bohci = bmaxoh

    !  Peak field on outboard side of OH coil

    bohco = abs(bzo)

    !  Allowable current density

    rjohc = pfjalw(bohci,bohco,ra(nohc),rb(nohc),sigpfalw)

    !  Peak field at the Beginning-Of-Pulse (BOP) (see above)

    bmaxoh0 = bfmax(cohbop,ra(nohc),rb(nohc),hohc)
    itt = 2 ; call peakb(nohc1,99,itt,bri,bro,bzi,bzo)
    bmaxoh0 = sqrt( (bmaxoh0 + bzi)**2 )

    rjpfalw(nohc) = pfjalw(bmaxoh0,abs(bzo),ra(nohc),rb(nohc),sigpfalw)
    rjohc0 = rjpfalw(nohc)

    !  Maximum field values

    bpf(nohc) = max(bmaxoh,abs(bmaxoh0))
    bpf2(nohc) = max(bohco,abs(bzo))

    !  Current in OH coil at BOP and EOF

    if (cohbop > coheof) then
       sgn = 1.0D0
       ric(nohc) = sgn * 1.0D-6*cohbop*(zh(nohc)-zl(nohc))*ohcth
    else
       sgn = -1.0D0
       ric(nohc) = sgn * 1.0D-6*coheof*(zh(nohc)-zl(nohc))*ohcth
    end if

    !  Volume of OH coil

    volohc = (zh(nohc)-zl(nohc)) * pi * (rb(nohc)**2-ra(nohc)**2)

    !  Number of turns

    turns(nohc) = 1.0D6 * abs(ric(nohc))/cptdin(nohc)

    !  Void fraction

    vf(nohc) = vfohc

    !  Weight of OH coil

    wtc(nohc) = volohc * 8990.0D0 * (1.0D0-vfohc)

    !  (J x B) force on OH coil

    forcepf = 0.5D6 * (bpf(nohc)+bpf2(nohc))*abs(ric(nohc))*rpf(nohc)

    !  Stress ==> cross-sectional area of supporting steel to use

    if (ipfres == 0) then

       !  Superconducting coil : Assume 500 MPa stress limit, 2/3 of the
       !  force is supported in the outer (steel) case

       areaspf = 0.666D0 * forcepf / 5.0D8

    else
       areaspf = 0.0D0  !  Resistive OH coil - no steel needed
    end if

    !  Weight of steel

    wts(nohc) = areaspf * 2.0D0*pi*rpf(nohc) * 7800.0D0

    !  Resistive power losses (non-superconducting coil)

    if (ipfres /= 0) then
       powohres = 2.0D0 * pi * rohc * pfclres / &
            (aroh * (1.0D0-vfohc)) * (1.0D6*ric(nohc))**2
       powpfres = powpfres + powohres
    end if

  end subroutine ohcalc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts, &
       rpts,zpts,brin,bzin,nfix,rfix,zfix,cfix,ngrp,ncls,rcls,zcls, &
       alfa,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat,sigma,work2,ssq,ccls)

    !+ad_name  efc
    !+ad_summ  Calculates field coil currents
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  D Strickler, ORNL
    !+ad_auth  J Galambos, ORNL
    !+ad_auth  P C Shipe, ORNL
    !+ad_call  fixb
    !+ad_call  mtrx
    !+ad_call  rsid
    !+ad_call  solv
    !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
    !+ad_args  nclsmx : input integer : maximum number of coils in one group
    !+ad_args  nptsmx : input integer : maximum number of points across the
    !+ad_argc                           plasma midplane at which the magnetic
    !+ad_argc                           field is fixed
    !+ad_args  nfixmx : input integer : maximum number of fixed current coils
    !+ad_args  lrow1 : input integer : row length of arrays bfix, bvec, gmat,
    !+ad_argc                          umat, vmat; should be >= (2*nptsmx + ngrpmx)
    !+ad_args  lcol1 : input integer : column length of arrays gmat, umat, vmat;
    !+ad_argc                          should be >= ngrpmx
    !+ad_args  npts : input integer : number of data points at which field is
    !+ad_args                         to be fixed; should be <= nptsmx
    !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
    !+ad_args  brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
    !+ad_argc                                                  data points (T)
    !+ad_args  nfix : input integer : number of coils with fixed currents, <= nfixmx
    !+ad_args  rfix(nfixmx),zfix(nfixmx) : input real arrays : coordinates of coils
    !+ad_argc                                                  with fixed currents (m)
    !+ad_args  cfix(nfixmx) : input real array : Fixed currents (A)
    !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
    !+ad_argc                         group have the same current, <= ngrpmx
    !+ad_args  ncls(ngrpmx+2) : input integer array : number of coils in each group,
    !+ad_argc                   each value <= nclsmx
    !+ad_args  rcls(ngrpmx,nclsmx),zcls(ngrpmx,nclsmx) : input real arrays : coords
    !+ad_argc                               R(i,j), Z(i,j) of coil j in group i (m)
    !+ad_args  alfa : input real : smoothing parameter (0 = no smoothing,
    !+ad_argc                      1.0D-9 = large smoothing)
    !+ad_args  bfix(lrow1) : input/output real array : work array
    !+ad_args  gmat(lrow1,lcol1) : input/output real array : work array
    !+ad_args  bvec(lrow1) : input/output real array : work array
    !+ad_args  rc(nclsmx) : input/output real array : work array
    !+ad_args  zc(nclsmx) : input/output real array : work array
    !+ad_args  cc(nclsmx) : input/output real array : work array
    !+ad_args  xc(nclsmx) : input/output real array : work array
    !+ad_args  umat(lrow1,lcol1) : input/output real array : work array
    !+ad_args  vmat(lrow1,lcol1) : input/output real array : work array
    !+ad_args  sigma(ngrpmx) : input/output real array : work array
    !+ad_args  work2(ngrpmx) : input/output real array : work array
    !+ad_args  ssq : output real : sum of squares of elements of residual vector
    !+ad_args  ccls(ngrpmx) : output real array : solution vector of coil currents
    !+ad_argc                                     in each group (A)
    !+ad_desc  This routine calculates the currents required in a group
    !+ad_desc  of ring coils to produce a fixed field at prescribed
    !+ad_desc  locations. Additional ring coils with fixed currents are
    !+ad_desc  also allowed.
    !+ad_prob  None
    !+ad_call  fixb
    !+ad_call  mtrx
    !+ad_call  rsid
    !+ad_call  solv
    !+ad_hist  18/08/11 PJK Initial F90 version
    !+ad_hist  11/10/12 PJK Moved from pfscl.f90 to pfcoil.f90, and 'contained'
    !+ad_hisc               the four called routines. Removed work1 argument
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1
    integer, intent(in) :: ngrp,npts,nfix
    integer, dimension(ngrpmx+2), intent(in) :: ncls

    real(kind(1.0D0)), intent(in) :: alfa
    real(kind(1.0D0)), dimension(ngrpmx,nclsmx), intent(in) :: rcls,zcls
    real(kind(1.0D0)), dimension(nptsmx), intent(in) :: brin,bzin,rpts,zpts
    real(kind(1.0D0)), dimension(nfixmx), intent(in) :: rfix,zfix,cfix

    real(kind(1.0D0)), dimension(lrow1), intent(inout) :: bfix,bvec
    real(kind(1.0D0)), dimension(lrow1,lcol1), intent(inout) :: gmat,umat,vmat
    real(kind(1.0D0)), dimension(ngrpmx), intent(inout) :: sigma,work2
    real(kind(1.0D0)), dimension(nclsmx), intent(inout) :: rc,zc,cc,xc

    real(kind(1.0D0)), intent(out) :: ssq
    real(kind(1.0D0)), dimension(ngrpmx), intent(out) :: ccls

    !  Local variables

    real(kind(1.0D0)) :: brssq, brnrm, bzssq, bznrm
    integer :: nrws

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate field from the fixed current loops

    call fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix,zfix,cfix,bfix)

    !  Set up matrix equation

    call mtrx(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts,brin, &
         bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec,rc,zc,cc,xc)

    !  Solve matrix equation

    call solv(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat,vmat, &
         sigma,work2)

    !  Calculate the norm of the residual vectors

    call rsid(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix,ngrp,ccls, &
         brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine mtrx(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts, &
         brin,bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec, &
         rc,zc,cc,xc)

      !+ad_name  mtrx
      !+ad_summ  Set up the matrix equation to calculate the currents
      !+ad_summ  in a group of ring coils
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  D Strickler, ORNL
      !+ad_auth  J Galambos, ORNL
      !+ad_cont  N/A
      !+ad_args  nptsmx : input integer : maximum number of points across the
      !+ad_argc                           plasma midplane at which the magnetic
      !+ad_argc                           field is fixed
      !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
      !+ad_args  nclsmx : input integer : maximum number of coils in one group
      !+ad_args  lrow1 : input integer : row length of arrays bfix, bvec, gmat,
      !+ad_argc                          umat, vmat; should be >= (2*nptsmx + ngrpmx)
      !+ad_args  lcol1 : input integer : column length of arrays gmat, umat, vmat;
      !+ad_argc                          should be >= ngrpmx
      !+ad_args  npts : input integer : number of data points at which field is
      !+ad_args                         to be fixed; should be <= nptsmx
      !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
      !+ad_args  lrow1 : input integer : row length of array bfix; should be >= nptsmx
      !+ad_args  npts : input integer : number of data points at which field is
      !+ad_args                         to be fixed; should be <= nptsmx
      !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
      !+ad_args  brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
      !+ad_argc                                                  data points (T)
      !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
      !+ad_argc                         group have the same current, <= ngrpmx
      !+ad_args  ncls(ngrpmx+2) : input integer array : number of coils in each group,
      !+ad_argc                   each value <= nclsmx
      !+ad_args  rcls(ngrpmx,nclsmx),zcls(ngrpmx,nclsmx) : input real arrays : coords
      !+ad_argc                               R(i,j), Z(i,j) of coil j in group i (m)
      !+ad_args  alfa : input real : smoothing parameter (0 = no smoothing,
      !+ad_argc                      1.0D-9 = large smoothing)
      !+ad_args  nrws : output integer : actual number of rows to use
      !+ad_args  bfix(lrow1) : input real array : Fields at data points (T)
      !+ad_args  gmat(lrow1,lcol1) : output real array : work array
      !+ad_args  bvec(lrow1) : output real array : work array
      !+ad_args  rc(nclsmx) : output real array : Coordinates of conductor loops (m)
      !+ad_args  zc(nclsmx) : output real array : Coordinates of conductor loops (m)
      !+ad_args  cc(nclsmx) : output real array : Currents in conductor loops (A)
      !+ad_args  xc(nclsmx) : output real array : Mutual inductances (H)
      !+ad_desc  This routine sets up the matrix equation for calculating the
      !+ad_desc  currents in a group of ring coils.
      !+ad_prob  None
      !+ad_call  bfield
      !+ad_hist  19/09/11 PJK Initial F90 version
      !+ad_hist  20/09/11 PJK Removed dble call
      !+ad_hist  26/11/13 PJK Removed obsolete argument to bfield
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: nptsmx, ngrpmx, nclsmx, lrow1, lcol1, npts, ngrp
      integer, dimension(ngrpmx+2), intent(in) :: ncls
      real(kind(1.0D0)), intent(in) :: alfa
      real(kind(1.0D0)), dimension(ngrpmx,nclsmx), intent(in) :: rcls, zcls
      real(kind(1.0D0)), dimension(nptsmx), intent(in) :: brin, bzin, rpts, zpts
      real(kind(1.0D0)), dimension(lrow1), intent(in) :: bfix

      integer, intent(out) :: nrws
      real(kind(1.0D0)), dimension(nclsmx), intent(out) :: rc, zc, cc, xc
      real(kind(1.0D0)), dimension(lrow1), intent(out) :: bvec
      real(kind(1.0D0)), dimension(lrow1,lcol1), intent(out) :: gmat

      !  Local variables

      integer :: i, j, k, nc
      real(kind(1.0D0)) brw, bzw, psw

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do i = 1,npts
         bvec(i) = brin(i) - bfix(i)
         bvec(i+npts) = bzin(i) - bfix(i + npts)
         do j = 1,ngrp
            nc = ncls(j)
            do k = 1,nc
               rc(k) = rcls(j,k)
               zc(k) = zcls(j,k)
               cc(k) = 1.0D0
            end do
            call bfield(nc,rc,zc,cc,xc,rpts(i),zpts(i),brw,bzw,psw)
            gmat(i,j) = brw
            gmat(i+npts,j) = bzw
         end do
      end do

      !  Add constraint equations

      nrws = 2 * npts

      do j = 1,ngrp
         bvec(nrws + j) = 0.0D0
         do i = 1,ngrp
            gmat(nrws + j,i) = 0.0D0
         end do
         nc = ncls(j)
         gmat(nrws + j,j) = nc * alfa
      end do

      nrws = 2*npts + ngrp

    end subroutine mtrx

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine solv(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat, &
         vmat,sigma,work2)

      !+ad_name  solv
      !+ad_summ  Solve a matrix using singular value decomposition
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  D Strickler, ORNL
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  P C Shipe, ORNL
      !+ad_cont  N/A
      !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
      !+ad_args  lrow1 : input integer : row length of arrays bfix, bvec, gmat,
      !+ad_argc                          umat, vmat; should be >= (2*nptsmx + ngrpmx)
      !+ad_args  lcol1 : input integer : column length of arrays gmat, umat, vmat;
      !+ad_argc                          should be >= ngrpmx
      !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
      !+ad_argc                         group have the same current, <= ngrpmx
      !+ad_args  ccls(ngrpmx) : output real array : solution vector of coil currents
      !+ad_argc                                     in each group (A)
      !+ad_args  nrws : input integer : actual number of rows to use
      !+ad_args  gmat(lrow1,lcol1) : input/output real array : work array
      !+ad_args  bvec(lrow1) : input/output real array : work array
      !+ad_args  umat(lrow1,lcol1) : output real array : work array
      !+ad_args  vmat(lrow1,lcol1) : output real array : work array
      !+ad_args  sigma(ngrpmx) : output real array : work array
      !+ad_args  work2(ngrpmx) : output real array : work array
      !+ad_desc  This routine solves the matrix equation for calculating the
      !+ad_desc  currents in a group of ring coils.
      !+ad_prob  None
      !+ad_call  svd
      !+ad_hist  18/08/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: ngrpmx,lrow1,lcol1,ngrp,nrws
      real(kind(1.0D0)), dimension(lrow1), intent(inout) :: bvec
      real(kind(1.0D0)), dimension(lrow1,lcol1), intent(inout) :: gmat
      real(kind(1.0D0)), dimension(lrow1,lcol1), intent(out) :: umat,vmat
      real(kind(1.0D0)), dimension(ngrpmx), intent(out) :: sigma, work2
      real(kind(1.0D0)), dimension(ngrpmx), intent(out) :: ccls

      !  Local variables

      integer :: i,j,ierr
      real(kind(1.0D0)) :: zvec,eps
      logical :: truth

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      truth = .true.
      eps = 1.0D-10

      call svd(lrow1,nrws,ngrp,gmat,sigma,truth,umat,truth,vmat,ierr,work2)

      do i = 1,ngrp
         work2(i) = 0.0D0
         do j = 1,nrws
            work2(i) = work2(i)+umat(j,i)*bvec(j)
         end do
      end do

      !  Compute currents

      do i = 1,ngrp
         ccls(i) = 0.0D0
         zvec = 0.0D0
         do j = 1,ngrp
            if (sigma(j) > eps) zvec = work2(j)/sigma(j)
            ccls(i) = ccls(i)+vmat(i,j)*zvec
         end do
      end do

    end subroutine solv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine rsid(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix, &
         ngrp,ccls,brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)

      !+ad_name  rsid
      !+ad_summ  Computes the norm of the residual vectors
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  D Strickler, ORNL
      !+ad_auth  J Galambos, ORNL
      !+ad_auth  P C Shipe, ORNL
      !+ad_cont  N/A
      !+ad_args  nptsmx : input integer : maximum number of points across the
      !+ad_argc                           plasma midplane at which the magnetic
      !+ad_argc                           field is fixed
      !+ad_args  ngrpmx : input integer : maximum number of PF coil groups
      !+ad_args  lrow1 : input integer : row length of arrays bfix, gmat;
      !+ad_argc                          should be >= (2*nptsmx + ngrpmx)
      !+ad_args  lcol1 : input integer : column length of array gmat; should be >= ngrpmx
      !+ad_args  npts : input integer : number of data points at which field is
      !+ad_args                         to be fixed; should be <= nptsmx
      !+ad_args  brin(nptsmx),bzin(nptsmx) : input real arrays : field components at
      !+ad_argc                                                  data points (T)
      !+ad_args  nfix : input integer : number of coils with fixed currents, <= nfixmx
      !+ad_args  ngrp : input integer : number of coil groups, where all coils in a
      !+ad_argc                         group have the same current, <= ngrpmx
      !+ad_args  ccls(ngrpmx) : input real array : coil currents in each group (A)
      !+ad_args  brssq : output real : sum of squares of radial field residues
      !+ad_args  brnrm : output real : radial field residue norm
      !+ad_args  bzssq : output real : sum of squares of vertical field residues
      !+ad_args  bznrm : output real : vertical field residue norm
      !+ad_args  ssq : output real : sum of squares of elements of residual vector
      !+ad_args  bfix(lrow1) : input real array : work array
      !+ad_args  gmat(lrow1,lcol1) : input real array : work array
      !+ad_desc  This routine calculates the residuals from the matrix
      !+ad_desc  equation for calculation of the currents in a group of ring coils.
      !+ad_prob  None
      !+ad_call  svd
      !+ad_hist  18/08/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: nptsmx,ngrpmx,lrow1,lcol1,npts,nfix,ngrp

      real(kind(1.0D0)), dimension(ngrpmx), intent(in) :: ccls
      real(kind(1.0D0)), dimension(nptsmx), intent(in) :: brin, bzin
      real(kind(1.0D0)), dimension(lrow1), intent(in) :: bfix
      real(kind(1.0D0)), dimension(lrow1,lcol1), intent(in) :: gmat

      real(kind(1.0D0)), intent(out) :: brssq,brnrm,bzssq,bznrm,ssq

      !  Local variables

      integer :: i,j
      real(kind(1.0D0)) :: svec,rvec

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      brnrm = 0.0D0
      brssq = 0.0D0

      do i = 1,npts
         svec = 0.0D0
         if (nfix > 0) svec = bfix(i)
         do j = 1,ngrp
            svec = svec + gmat(i,j)*ccls(j)
         end do
         rvec = svec - brin(i)
         brnrm = brnrm + brin(i)**2
         brssq = brssq + rvec**2
      end do

      bznrm = 0.0D0
      bzssq = 0.0D0

      do i = 1,npts
         svec = 0.0D0
         if (nfix > 0) svec = bfix(i+npts)
         do j = 1,ngrp
            svec = svec + gmat(i+npts,j)*ccls(j)
         end do
         rvec = svec - bzin(i)
         bznrm = bznrm + bzin(i)**2
         bzssq = bzssq + rvec**2
      end do

      ssq = brssq/(1.0D0 + brnrm) + bzssq/(1.0D0+bznrm)

    end subroutine rsid

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix, &
         zfix,cfix,bfix)

      !+ad_name  fixb
      !+ad_summ  Calculates the field from the fixed current loops
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  D Strickler, ORNL
      !+ad_auth  J Galambos, ORNL
      !+ad_cont  N/A
      !+ad_args  nptsmx : input integer : maximum number of points across the
      !+ad_argc                           plasma midplane at which the magnetic
      !+ad_argc                           field is fixed
      !+ad_args  nfixmx : input integer : maximum number of fixed current coils
      !+ad_args  lrow1 : input integer : row length of array bfix; should be >= nptsmx
      !+ad_args  npts : input integer : number of data points at which field is
      !+ad_args                         to be fixed; should be <= nptsmx
      !+ad_args  rpts(nptsmx),zpts(nptsmx) : input real arrays : coords of data points (m)
      !+ad_args  nfix : input integer : number of coils with fixed currents, <= nfixmx
      !+ad_args  rfix(nfixmx),zfix(nfixmx) : input real arrays : coordinates of coils
      !+ad_argc                                                  with fixed currents (m)
      !+ad_args  cfix(nfixmx) : input real array : Fixed currents (A)
      !+ad_args  bfix(lrow1) : output real array : Fields at data points (T)
      !+ad_desc  This routine calculates the fields at the points specified by
      !+ad_desc  (rpts,zpts) from the set of coils with fixed currents.
      !+ad_prob  None
      !+ad_call  bfield
      !+ad_hist  19/09/11 PJK Initial F90 version
      !+ad_hist  11/10/12 PJK Changed work1 argument to local array
      !+ad_hist  26/11/13 PJK Removed obsolete argument to bfield
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: nptsmx, nfixmx, lrow1, npts, nfix
      real(kind(1.0D0)), dimension(nptsmx), intent(in) :: rpts, zpts
      real(kind(1.0D0)), dimension(nfixmx), intent(in) :: rfix, zfix, cfix
      real(kind(1.0D0)), dimension(lrow1), intent(out) :: bfix

      !  Local variables

      integer :: i
      real(kind(1.0D0)) brw,bzw,psw
      real(kind(1.0D0)), dimension(nfixmx) :: work1

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do i = 1,npts
         bfix(i) = 0.0D0
         bfix(npts + i) = 0.0D0
      end do

      if (nfix <= 0) return

      do i = 1,npts
         call bfield(nfix,rfix,zfix,cfix,work1,rpts(i),zpts(i),brw,bzw,psw)
         bfix(i) = brw
         bfix(npts + i) = bzw
      end do

    end subroutine fixb

  end subroutine efc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bfield(nc, rc, zc, cc, xc, rp, zp, br, bz, psi)

    !+ad_name  bfield
    !+ad_summ  Calculate the field at a point due to currents in a number
    !+ad_summ  of circular poloidal conductor loops.
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  D Strickler, ORNL
    !+ad_auth  J Galambos, ORNL
    !+ad_cont  N/A
    !+ad_args  nc : input integer : number of loops
    !+ad_args  rc(nc) : input real array : R coordinates of loops (m)
    !+ad_args  zc(nc) : input real array : Z coordinates of loops (m)
    !+ad_args  cc(nc) : input real array : Currents in loops (A)
    !+ad_args  xc(nc) : output real array : Mutual inductances (H)
    !+ad_args  rp, zp : input real : coordinates of point of interest (m)
    !+ad_args  br : output real : radial field component at (rp,zp) (T)
    !+ad_args  bz : output real : vertical field component at (rp,zp) (T)
    !+ad_args  psi : output real : poloidal flux at (rp,zp) (Wb)
    !+ad_desc  This routine calculates the magnetic field components and
    !+ad_desc  the poloidal flux at an (R,Z) point, given the locations
    !+ad_desc  and currents of a set of conductor loops.
    !+ad_desc  <P>The mutual inductances between the loops and a poloidal
    !+ad_desc  filament at the (R,Z) point of interest is also found.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  19/09/11 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: nc
    real(kind(1.0D0)), intent(in) :: rp, zp
    real(kind(1.0D0)), dimension(nc), intent(in) :: rc, zc, cc
    real(kind(1.0D0)), dimension(nc), intent(out) :: xc
    real(kind(1.0D0)), intent(out) :: br, bz, psi

    !  Local variables

    integer :: i
    real(kind(1.0D0)) a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4
    real(kind(1.0D0)) :: zs,dr,d,s,t,a,xk,xe,dz,sd,brx,bzx

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Elliptic integral coefficients

    a0 = 1.38629436112D0
    a1 = 0.09666344259D0
    a2 = 0.03590092383D0
    a3 = 0.03742563713D0
    a4 = 0.01451196212D0
    b0 = 0.5D0
    b1 = 0.12498593597D0
    b2 = 0.06880248576D0
    b3 = 0.03328355346D0
    b4 = 0.00441787012D0
    c1 = 0.44325141463D0
    c2 = 0.06260601220D0
    c3 = 0.04757383546D0
    c4 = 0.01736506451D0
    d1 = 0.24998368310D0
    d2 = 0.09200180037D0
    d3 = 0.04069697526D0
    d4 = 0.00526449639D0

    br  = 0.0D0
    bz  = 0.0D0
    psi = 0.0D0

    do i = 1,nc
       d = (rp + rc(i))**2 + (zp - zc(i))**2
       s = 4.0D0*rp*rc(i)/d

       t = 1.0D0 - s
       a = log(1.0D0/t)

       dz = zp - zc(i)
       zs = dz**2
       dr = rp - rc(i)
       sd = sqrt(d)

       !  Evaluation of elliptic integrals

       xk = a0 + t*(a1 + t*(a2 + t*(a3 + a4*t))) &
            + a*(b0 + t*(b1 + t*(b2 + t*(b3 + b4*t))))
       xe = 1.0D0 + t*(c1 + t*(c2 + t*(c3 + c4*t))) &
            + a*t*(d1 + t*(d2 + t*(d3 + d4*t)))

       !  Mutual inductances

       xc(i) = 0.5D0*rmu0*sd*((2.0D0 - s)*xk - 2.0D0*xe)

       !  Radial, vertical fields

       brx = rmu0*cc(i)*dz/(twopi*rp*sd)*(- xk + &
            (rc(i)**2 + rp**2 + zs)/(dr**2 + zs)*xe)
       bzx = rmu0*cc(i)/(twopi*sd)*(xk + &
            (rc(i)**2 - rp**2 - zs)/(dr**2 + zs)*xe)

       !  Sum fields, flux

       br = br + brx
       bz = bz + bzx
       psi = psi + xc(i)*cc(i)

    end do

  end subroutine bfield

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine peakb(i,ii,it,bri,bro,bzi,bzo)

    !+ad_name  peakb
    !+ad_summ  Calculates the peak field at a PF coil
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  i : input integer : coil number
    !+ad_args  ii : input integer : group number
    !+ad_args  it : input/output integer : time point at which field is highest
    !+ad_args  bri : output real : radial field at inner edge (T)
    !+ad_args  bro : output real : radial field at outer edge (T)
    !+ad_args  bzi : output real : vertical field at inner edge (T)
    !+ad_args  bzo : output real : vertical field at outer edge (T)
    !+ad_desc  This routine calculates the peak magnetic field components
    !+ad_desc  at the inner and outer edges of a given PF coil.
    !+ad_desc  The calculation includes the effects from all the coils
    !+ad_desc  and the plasma.
    !+ad_prob  None
    !+ad_call  bfield
    !+ad_call  report_error
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  26/11/13 PJK Removed obsolete argument to bfield
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: i,ii
    integer, intent(inout) :: it
    real(kind(1.0D0)), intent(out) :: bri,bro,bzi,bzo

    !  Local variables

    integer :: iii,iohc,jj,jjj,kk,n
    real(kind(1.0D0)) :: bpfin,bpfout,dzpf,psi,sgn

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ((iohcl /= 0).and.(i == nohc)) then  !  OH coil
       kk = 0
    else

       !  Check different times for maximum current

       if (abs(curpfs(i)-ric(i)) < 1.0D-12) then
          it = 2
       else if (abs(curpff(i)-ric(i)) < 1.0D-12) then
          it = 4
       else if (abs(curpfb(i)-ric(i)) < 1.0D-12) then
          it = 5
       else
          idiags(1) = it ; call report_error(72)
       end if

       if (iohcl == 0) then
          !  No OH coil
          kk = 0
       else
          if (cohbop > coheof) then
             sgn = 1.0D0
          else
             sgn = -1.0D0
          end if

          !  Current in each filament representing part of the OH coil

          do iohc = 1,nfxf
             cfxf(iohc) = waves(nohc,it)*coheof*sgn*ohcth*ohhghf &
                  * hmax / nfxf * 2.0D0
          end do
          kk = nfxf
       end if

    end if

    !  Loop over all coils

    jj = 0
    do iii = 1,ngrp
       do jjj = 1,ncls(iii)
          jj = jj+1

          !  Radius, z-coordinate and current for each coil

          if (iii == ii) then
             !  Self field from coil (Lyle's Method)

             kk = kk + 1
             dzpf = zh(jj) - zl(jj)
             rfxf(kk) = rpf(jj)
             zfxf(kk) = zpf(jj) + dzpf * 0.125D0
             cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
             kk = kk + 1
             rfxf(kk) = rpf(jj)
             zfxf(kk) = zpf(jj) + dzpf * 0.375D0
             cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
             kk = kk + 1
             rfxf(kk) = rpf(jj)
             zfxf(kk) = zpf(jj) - dzpf * 0.125D0
             cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
             kk = kk + 1
             rfxf(kk) = rpf(jj)
             zfxf(kk) = zpf(jj) - dzpf * 0.375D0
             cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6

          else
             !  Field from different coil

             kk = kk + 1
             rfxf(kk) = rpf(jj)
             zfxf(kk) = zpf(jj)
             cfxf(kk) = ric(jj)*waves(jj,it)*1.0D6
          end if
       end do
    end do

    !  Plasma effect
    if (it > 2) then
       kk = kk + 1
       rfxf(kk) = rmajor
       zfxf(kk) = 0.0D0
       cfxf(kk) = plascur
    end if

    call bfield(kk,rfxf,zfxf,cfxf,xind,ra(i),zpf(i),bri,bzi,psi)
    call bfield(kk,rfxf,zfxf,cfxf,xind,rb(i),zpf(i),bro,bzo,psi)

    !  Peak field at OH coil is dealt with in BFMAX

    if ((iohcl /= 0).and.(i == nohc)) return

    bpfin  = sqrt(bri**2 + bzi**2)
    bpfout = sqrt(bro**2 + bzo**2)
    do n = 1,ncls(ii)
       bpf(i-1+n) = bpfin
       bpf2(i-1+n) = bpfout
    end do

  end subroutine peakb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bfmax(rj,a,b,h)

    !+ad_name  bfmax
    !+ad_summ  Calculates the maximum field of a solenoid
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rj : input real : overall current density (A/m2)
    !+ad_args  a  : input real : solenoid inner radius (m)
    !+ad_args  b  : input real : solenoid outer radius (m)
    !+ad_args  h  : input real : solenoid half height (m)
    !+ad_desc  This routine calculates the peak field (T) at a solenoid's
    !+ad_desc  inner radius, using fits taken from the figure
    !+ad_desc  on p.22 of M. Wilson's book Superconducting Magnets,
    !+ad_desc  Clarendon Press, Oxford, N.Y., 1983
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_stat  Okay
    !+ad_docs  See above
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bfmax

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rj,a,b,h

    !  Local variables

    real(kind(1.0D0)) :: alpha,b0,b1,beta,f,rat

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    beta = h/a
    alpha = b/a

    !  Fits are for 1 < alpha < 2 , and 0.5 < beta < very large

    b0 = rj*rmu0*h * log( (alpha + sqrt(alpha**2+beta**2)) &
         / (1.0D0 + sqrt(1.0D0 + beta**2)) )

    if (beta > 3.0D0) then

       b1 = rmu0*rj*(b-a)
       f = (3.0D0/beta)**2
       bfmax = f*b0*(1.007D0 + (alpha-1.0D0)*0.0055D0) + (1.0D0-f)*b1

    else if (beta > 2.0D0) then

       rat = (1.025D0 - (beta-2.0D0)*0.018D0) + (alpha-1.0D0) * &
            (0.01D0 - (beta-2.0D0)*0.0045D0)
       bfmax = rat*b0

    else if (beta > 1.0D0) then

       rat = (1.117D0 - (beta-1.0D0)*0.092D0) + (alpha-1.0D0) * &
            (beta-1.0D0)*0.01D0
       bfmax = rat*b0

    else if (beta > 0.75D0) then

       rat = (1.30D0 - 0.732D0 * (beta-0.75D0)) + (alpha-1.0D0) * &
            (0.2D0*(beta-0.75D0) - 0.05D0)
       bfmax = rat * b0

    else

       rat = (1.65D0 - 1.4D0*(beta-0.5D0)) + (alpha - 1.0D0) * &
            (0.6D0*(beta-0.5D0) - 0.20D0)
       bfmax = rat * b0

    end if

  end function bfmax

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine waveform

    !+ad_name  waveform
    !+ad_summ  Sets up the PF coil waveforms
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine sets up the PF coil current waveforms.
    !+ad_desc  <CODE>waves(i,j)</CODE> is the current in coil i, at time j,
    !+ad_desc  normalized to the peak current in that coil at any time.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/12/12 PJK Modified if-logic to >= throughout
    !+ad_hist  27/03/13 PJK Changed comment: TF coil to plasma
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: ic,it,nplas

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Initialize plasma current waveform to 1.0
    !  (never actually used)

    nplas = nohc+1
    do it = 1,6
       waves(nplas,it) = 1.0D0
    end do

    do ic = 1,nohc

       !  Find where the peak current occurs

       !  Beginning of pulse, t = tramp

       if ( (abs(curpfs(ic)) >= abs(curpfb(ic))) .and. &
            (abs(curpfs(ic)) >= abs(curpff(ic))) ) &
            ric(ic) = curpfs(ic)

       !  Beginning of flat-top, t = tramp + tohs

       if ( (abs(curpff(ic)) >= abs(curpfb(ic))) .and. &
            (abs(curpff(ic)) >= abs(curpfs(ic))) ) &
            ric(ic) = curpff(ic)

       !  End of flat-top, t = tramp + tohs + theat + tburn

       if ( (abs(curpfb(ic)) >= abs(curpfs(ic))) .and. &
            (abs(curpfb(ic)) >= abs(curpff(ic))) ) &
            ric(ic) = curpfb(ic)

       !  Set normalized current waveforms

       waves(ic,1) = 0.0D0
       waves(ic,2) = curpfs(ic)/ric(ic)
       waves(ic,3) = curpff(ic)/ric(ic)
       waves(ic,4) = curpff(ic)/ric(ic)
       waves(ic,5) = curpfb(ic)/ric(ic)
       waves(ic,6) = 0.0D0

    end do

  end subroutine waveform

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function pfjalw(bi,bo,ri,ro,sigalw)

    !+ad_name  pfjalw
    !+ad_summ  Calculates maximum allowable current density in a
    !+ad_summ  superconducting PF coil
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, ORNL
    !+ad_auth  J Miller, ORNL
    !+ad_cont  N/A
    !+ad_args  bi : input real : peak field at inner edge (T)
    !+ad_args  bo : input real : peak field at outer edge (T)
    !+ad_args  ri : input real : radius of inner edge (m)
    !+ad_args  ro : input real : radius of outer edge (m)
    !+ad_args  sigalw : input real : allowable stress (tangential) (MPa)
    !+ad_desc  This function calculates the maximum allowable current
    !+ad_desc  density in a superconducting PF coil, using J. Miller's
    !+ad_desc  quick method. Originally programmed by J. Galambos 1991.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: pfjalw

    !  Arguments

    real(kind(1.0D0)), intent(in) :: bi,bo,ri,ro,sigalw

    !  Local variables

    real(kind(1.0D0)) :: r2fact,rfact,yy,zz
    real(kind(1.0D0)), parameter :: xbig = 32.0D0
    real(kind(1.0D0)), parameter :: y1 = 1.0D0  !  temperature margin (K)
    real(kind(1.0D0)), parameter :: y2 = 16.0D0 !  critical temperature at zero field (K)
    real(kind(1.0D0)), parameter :: y3 = 23.0D0 !  critical field at zero temperature (T)
    real(kind(1.0D0)), parameter :: y4 = 5.0D0  !  bulk He temperature (K)

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    rfact = ro/ri
    r2fact = -0.053D0 * rfact**2 + 0.925D0*rfact + 0.1D0
    yy = 1.0D0 - y1 / (y2*(1.0D0-bi/y3) - y4)
    yy = max(yy, 0.001D0)
    zz = 0.036D0 * sqrt(bi)/(1.0D0-bi/23.0D0)**2

    pfjalw = 1.45D8/( xbig/sigalw * r2fact * (bi+bo)*(ri+ro) + &
         1.0D0 + 0.6D0/yy * zz )

  end function pfjalw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vsec

    !+ad_name  vsec
    !+ad_summ  Calculation of volt-second capability of PF system
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the volt-second capability of the PF
    !+ad_desc  coil system.
    !+ad_prob  None
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  PF volt-seconds during start-up

    if (iohcl.eq.0) then
       !  No OH coil
       nef = ncirt - 1
    else
       nef = ncirt - 2
    end if

    vsefsu = 0.0D0

    do i = 1,nef
       vsdum(i,1) = sxlg(ncirt,i) * cpt(i,2)
       vsdum(i,2) = sxlg(ncirt,i) * cpt(i,3)
       vsefsu = vsefsu + ( vsdum(i,2) - vsdum(i,1) )
    end do

    !  OH startup volt-seconds

    if (iohcl /= 0) then
       vsdum(nohc,1) = sxlg(ncirt,ncirt-1) * cpt(ncirt-1,2)
       vsdum(nohc,2) = sxlg(ncirt,ncirt-1) * cpt(ncirt-1,3)
       vsohsu = vsdum(nohc,2) - vsdum(nohc,1)
    end if

    !  Total available volt-seconds for start-up

    vssu = vsohsu + vsefsu

    !  Burn volt-seconds

    if (iohcl /= 0) then
       vsdum(nohc,3) = sxlg(ncirt,ncirt-1) * cpt(ncirt-1,5)
       vsohbn = vsdum(nohc,3) - vsdum(nohc,2)
    end if

    !  PF volt-seconds during burn

    vsefbn = 0.0D0
    do i = 1,nef
       vsdum(i,3) = sxlg(ncirt,i) * cpt(i,5)
       vsefbn = vsefbn + (vsdum(i,3) - vsdum(i,2) )
    end do

    vsbn = vsohbn + vsefbn

    vstot = vssu + vsbn
    vseft = vsefsu + vsefbn
    vsoh = vsohbn + vsohsu

  end subroutine vsec

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine induct(outfile,iprint)

    !+ad_name  induct
    !+ad_summ  Calculates PF coil set mutual inductance matrix
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the mutual inductances between all the
    !+ad_desc  PF coils.
    !+ad_prob  None
    !+ad_call  bfield
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  report_error
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  20/09/11 PJK Removed dble calls
    !+ad_hist  24/09/12 PJK Swapped argument order
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  19/11/13 PJK Fixed problem with array bounds if ncls(1)=1
    !+ad_hist  26/11/13 PJK Improved OH coil self inductance, and OH-plasma
    !+ad_hisc               mutual inductance calculations;
    !+ad_hisc               Removed obsolete argument to bfield calls
    !+ad_hist  25/02/14 PJK Raised nohmax, and added warning message
    !+ad_hisc               if noh is too large
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    integer, parameter :: nohmax = 200 !  Maximum no. of segments for the OH coil
    integer, parameter :: nplas = 1 !  Number of filaments describing the plasma

    real(kind(1.0D0)), allocatable, dimension(:) :: roh,zoh
    real(kind(1.0D0)), dimension(nplas) :: rplasma,zplasma
    real(kind(1.0D0)), dimension(ngc2+nohmax) :: rc,zc,xc,cc,xcin,xcout
    real(kind(1.0D0)) :: a,b,c,br,bz,deltar,delzoh,psi,r,reqv,rl,rp,r2_16a2
    real(kind(1.0D0)) :: xohpf,xohpl,xpfpl,zp
    integer :: i,ig,ii,ij,j,jj,k,nc,ncoilj,ncoils,nef,noh

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    br = 0.0D0
    bz = 0.0D0
    psi = 0.0D0

    rc(:) = 0.0D0
    zc(:) = 0.0D0
    xc(:) = 0.0D0
    cc(:) = 0.0D0

    sxlg(:,:) = 0.0D0

    !  Break OH coil into noh segments
    !
    !  Choose noh so that the radial thickness of the coil is not thinner
    !  than each segment is tall, i.e. the segments are pancake-like,
    !  for the benefit of the mutual inductance calculations later

    noh = int( ceiling( 2.0D0*zh(nohc) / (rb(nohc)-ra(nohc)) ) )

    if (noh > nohmax) then
       idiags(1) = noh ; idiags(2) = nohmax
       fdiags(1) = ohcth
       call report_error(73)
    end if

    noh = min(noh,nohmax)

    allocate(roh(noh), zoh(noh))

    if (iohcl /= 0) then

       roh(:) = rohc

       delzoh = 2.0D0 * zh(nohc) / noh  !  zh(nohc) is the half-height of the coil
       do i = 1,noh
          zoh(i) = zh(nohc) - delzoh*(0.5D0+i-1)
       end do

    end if

    rplasma(1) = rmajor  !  assumes nplas==1
    zplasma(1) = 0.0D0

    !  OH coil / plasma mutual inductance
    !
    !  Improved calculation: Each OH segment is now split into two filaments,
    !  of radius reqv+deltar and reqv-deltar, respectively. The mutual inductance
    !  of the segment with a plasma circuit is the mean of that calculated
    !  using the two equivalent filaments.
    !  Formulas and tables for the calculation of mutual and self-inductance
    !  [Revised], Rosa and Grover, Scientific papers of the Bureau of Standards,
    !  No. 169, 3rd ed., 1916. page 33

    nc = nplas
    do i = 1,nplas
       rc(i) = rplasma(i)
       zc(i) = zplasma(i)
    end do

    if (iohcl /= 0) then
       xohpl = 0.0D0
       if (ohcth >= delzoh) then
          deltar = sqrt((ohcth**2 - delzoh**2)/12.0D0)
       else
          fdiags(1) = ohcth ; fdiags(2) = delzoh
          call report_error(74)
       end if
       do i = 1,noh
          rp = roh(i)
          zp = zoh(i)

          reqv = rp*(1.0D0 + delzoh**2 / (24.0D0*rp**2))

          call bfield(nc,rc,zc,cc,xcin, reqv-deltar,zp,br,bz,psi)
          call bfield(nc,rc,zc,cc,xcout,reqv+deltar,zp,br,bz,psi)

          do ii = 1,nplas
             xc(ii) = 0.5D0*(xcin(ii) + xcout(ii))
             xohpl = xohpl + xc(ii)
          end do
       end do

       sxlg(ncirt,nohc) = xohpl / (nplas*noh) * turns(nohc)
       sxlg(nohc,ncirt) = sxlg(ncirt,nohc)
    end if

    !  Plasma self inductance

    sxlg(ncirt,ncirt) = rlp

    !  PF coil / plasma mutual inductances

    ncoils = 0
    nc = nplas
    do i = 1,ngrp
       xpfpl = 0.0D0
       ncoils = ncoils + ncls(i)
       rp = rpf(ncoils)
       zp = zpf(ncoils)
       call bfield(nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
       do ii = 1,nplas
          xpfpl = xpfpl + xc(ii)
       end do
       do j = 1,ncls(i)
          ncoilj = ncoils + 1 - j
          sxlg(ncoilj,ncirt) = xpfpl / nplas * turns(ncoilj)
          sxlg(ncirt,ncoilj) = sxlg(ncoilj,ncirt)
       end do
    end do

    if (iohcl /= 0) then

       !  OH coil self inductance
       !  Equation 86, p. 316 of  Formulas and tables for the calculation
       !  of mutual and self-inductance [Revised], Rosa and Grover,
       !  Scientific papers of the Bureau of Standards, No. 169, 3rd ed., 1916

       a = rohc            !  mean radius of coil
       b = 2.0D0*zh(nohc)  !  length of coil
       c = rb(nohc) - ra(nohc)  !  radial winding thickness
       r = 0.2235D0*(b + c)  !  approx geometric mean distance of cross-section (Grover)
       r2_16a2 = r*r / (16.0D0*a*a)

       sxlg(nohc,nohc) = rmu0 * a * turns(nohc)**2 * &
            (log(8.0D0*a/r)*(1.0D0 + 3.0D0*r2_16a2) - (2.0D0 + r2_16a2))

       !  OH coil / PF coil mutual inductances

       nc = noh
       do i = 1,noh
          rc(i) = roh(i)
          zc(i) = zoh(i)
       end do

       ncoils = 0
       do i = 1,ngrp
          xohpf = 0.0D0
          ncoils = ncoils + ncls(i)
          rp = rpf(ncoils)
          zp = zpf(ncoils)
          call bfield(nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
          do ii = 1,noh
             xohpf = xohpf + xc(ii)
          end do
          do j = 1,ncls(i)
             ncoilj = ncoils + 1 - j
             sxlg(ncoilj,nohc) = xohpf * turns(ncoilj) * turns(nohc)/noh
             sxlg(nohc,ncoilj) = sxlg(ncoilj,nohc)
          end do
       end do

    end if

    !  PF coil - PF coil inductances

    if (iohcl == 0) then
       nef = nohc
    else
       nef = nohc-1
    end if

    nc = nef-1
    do i = 1,nef
       do j = 1,nef-1
          if (j >= i) then
             jj = j+1
          else
             jj = j
          end if
          zc(j) = zpf(jj)
          rc(j) = rpf(jj)
       end do
       rp = rpf(i)
       zp = zpf(i)
       call bfield(nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
       do k = 1,nef
          if (k < i)  then
             sxlg(i,k) = xc(k) * turns(k) * turns(i)
          else if (k == i)  then
             rl = abs(zh(k) - zl(k))/sqrt(pi)
             sxlg(k,k) = rmu0 * turns(k)**2 * rpf(k) * &
                  (log(8.0D0*rpf(k)/rl) - 1.75D0)
          else
             sxlg(i,k) = xc(k-1) * turns(k) * turns(i)
          end if
       end do
    end do

    deallocate(roh,zoh)

    !  Output section

    if (iprint == 0) return

    call oheadr(outfile,'PF Coil Inductances')
    call ocmmnt(outfile,'Inductance matrix (Henries-turns**2) :')
    call oblnkl(outfile)

    do ig = 1,nef
       write(outfile,210) ig,(sxlg(ij,ig),ij=1,ncirt)
    end do
210 format(t3,i2,t9,20(1pe8.1))

    if (iohcl /= 0) write(outfile,230) (sxlg(ij,ncirt-1),ij=1,ncirt)
230 format(' OH coil',t9,20(1pe8.1))

    write(outfile,240) (sxlg(ij,ncirt),ij=1,ncirt)
240 format(' Plasma',t9,20(1pe8.1))

  end subroutine induct

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine outpf(outfile)

    !+ad_name  outpf
    !+ad_summ  Routine to write output from PF coil module to file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine writes the PF coil information to the output file.
    !+ad_prob  None
    !+ad_call  int_to_string2
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_call  report_error
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  02/04/14 PJK Added coil geometry to mfile
    !+ad_hist  03/04/14 PJK Added coil currents and fields to mfile
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  09/07/14 PJK Added info message if OH coil current density is
    !+ad_hist               not reaching its upper limit
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    integer :: k,nef
    character(len=2) :: intstring

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'PF Coils')

    !  Print out OH coil stress

    if (iohcl == 0) then
       call ocmmnt(outfile,'No OH coil included')
       call oblnkl(outfile)
    else
       call ocmmnt(outfile,'OH Coil Stress Calculations :')
       call oblnkl(outfile)
       call ovarre(outfile,'Maximum field at End Of Flattop (T)', &
            '(bmaxoh)',bmaxoh)
       call ovarre(outfile,'Maximum field at Beginning Of Pulse (T)', &
            '(bmaxoh0)',bmaxoh0)
       call ovarre(outfile,'Allowable current density at EOF (A/m2)', &
            '(rjohc)',rjohc)
       call ovarre(outfile,'Actual current density at EOF (A/m2)', &
            '(coheof)',coheof)
       call ovarre(outfile,'Allowable current density at BOP (A/m2)', &
            '(rjohc0)',rjohc0)
       call ovarre(outfile,'Actual current density at BOP (A/m2)', &
            '(cohbop)',cohbop)
       call ovarre(outfile,'Allowable stress at BOP (MPa)', &
            '(sigpfalw)',sigpfalw)

       if ( (abs(coheof) < 0.99D0*abs(rjohc)).or. &
            (abs(cohbop) < 0.99D0*abs(rjohc0)) ) then
          call report_error(135)
       end if

    end if

    if (ipfres /= 0) then
       call osubhd(outfile,'Resistive Power :')
       call ovarre(outfile,'PF coil resistive power (W)','(powpfres)', &
            powpfres)
       call ovarre(outfile,'OH coil resistive power (W)','(powohres)', &
            powohres)
    else
       call osubhd(outfile,'Coil Case Stress :')
       call ovarre(outfile,'Maximum permissible tensile stress (MPa)', &
            '(sigpfcalw)',sigpfcalw)
       call ovarre(outfile,'JxB hoop force fraction supported by case', &
            '(sigpfcf)',sigpfcf)
    end if

    !  nef is the number of coils excluding the OH coil

    nef = nohc
    if (iohcl /= 0) nef = nef - 1

    call osubhd(outfile, 'Geometry of PF coils, OH coil and plasma :')

    write(outfile,10)
10  format(' coil',t17,'R(m)',t29,'Z(m)',t41,'dR(m)',t53,'dZ(m)', &
         t65,'turns')
    call oblnkl(outfile)

    !  PF coils

    write(outfile,20) (k,rpf(k),zpf(k),(rb(k)-ra(k)),abs(zh(k)-zl(k)), &
         turns(k),k=1,nef)
20  format('  PF',i1,t10,5f12.2)

    do k = 1,nef
       intstring = int_to_string2(k)
       call ovarre(mfile,'PF coil '//intstring//' radius (m)', &
            '(rpf('//intstring//'))',rpf(k))
       call ovarre(mfile,'PF coil '//intstring//' vertical position (m)', &
            '(zpf('//intstring//'))',zpf(k))
       call ovarre(mfile,'PF coil '//intstring//' radial thickness (m)', &
            '(pfdr'//intstring//')',(rb(k)-ra(k)))
       call ovarre(mfile,'PF coil '//intstring//' vertical thickness (m)', &
            '(pfdz'//intstring//')',(zh(k)-zl(k)))
       call ovarre(mfile,'PF coil '//intstring//' turns', &
            '(turns('//intstring//'))',turns(k))
       call ovarre(mfile,'PF coil '//intstring//' current (MA)', &
            '(ric('//intstring//'))',ric(k))
       call ovarre(mfile,'PF coil '//intstring//' field (T)', &
            '(bpf('//intstring//'))',bpf(k))
    end do

    !  OH coil, if present

    if (iohcl.ne.0) then
       write(outfile,30) rpf(nohc),zpf(nohc),(rb(nohc)-ra(nohc)), &
            abs(zh(nohc)-zl(nohc)),turns(nohc)
30     format('  OH',t10,5f12.2)
       call ovarre(mfile,'OH coil radius (m)', &
            '(rpf(nohc))',rpf(nohc))
       call ovarre(mfile,'OH coil vertical position (m)', &
            '(zpf(nohc))',zpf(nohc))
       call ovarre(mfile,'OH coil radial thickness (m)', &
            '(ohdr)',(rb(nohc)-ra(nohc)))
       call ovarre(mfile,'OH coil vertical thickness (m)', &
            '(ohdz)',(zh(nohc)-zl(nohc)))
       call ovarre(mfile,'OH coil turns', &
            '(turns(nohc))',turns(nohc))
       call ovarre(mfile,'OH coil current (MA)', &
            '(ric(nohc))',ric(nohc))
       call ovarre(mfile,'OH coil field (T)', &
            '(bpf(nohc))',bpf(nohc))
    end if

    !  Plasma

    write(outfile,40) rmajor,0.0D0,2.0D0*rminor,2.0D0*rminor*kappa,1.0D0
40  format(' Plasma',t10,5f12.2)

    call osubhd(outfile,'PF Coil Information :')

    write(outfile,50)
50  format(' coil', &
         t8, 'current', &
         t17,'allowed J', &
         t28,'actual J', &
         t39,'J', &
         t43,'coil weight', &
         t56,'steel weight', &
         t71,'field')

    write(outfile,60)
60  format( &
         t10,'(MA)', &
         t18,'(A/m2)', &
         t29,'(A/m2)', &
         t37,'ratio', &
         t46,'(kg)', &
         t60,'(kg)', &
         t72,'(T)')

    call oblnkl(outfile)

    !  PF coils

    do k = 1,nef
       write(outfile,90) k,ric(k),rjpfalw(k),rjconpf(k), &
            (rjconpf(k)/rjpfalw(k)),wtc(k),wts(k),bpf(k)
    end do

    !  The 0p syntax is needed here and on line 100
    !  to prevent a known compiler 'feature' from apparently
    !  multiplying the f-formatted numbers by 10.

90  format('  PF',i1,f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,1pe13.3)

    !  OH coil, if present

    if (iohcl /= 0) then

       write(outfile,100) ric(nohc),rjpfalw(nohc),cohbop, &
            (cohbop/rjpfalw(nohc)),wtc(nohc),wts(nohc), &
            bpf(nohc)

100    format('  OH ',f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,1pe13.3)
    end if

    !  Miscellaneous totals

    write(outfile,110)
110 format(t8,'------',t43,'---------',t55,'---------')

    write(outfile,120) ricpf,whtpf,whtpfs
120 format(t6,f8.2,t41,1pe11.3,1pe12.3)

    call osubhd(outfile,'PF coil current scaling information :')
    call ovarre(outfile,'Sum of squares of residuals ','(ssq0)',ssq0)
    call ovarre(outfile,'Smoothing parameter ','(alfapf)',alfapf)

  end subroutine outpf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine outvolt(outfile)

    !+ad_name  outvolt
    !+ad_summ  Writes volt-second information to output file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine writes the PF coil volt-second data to the
    !+ad_desc  output file.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  18/12/12 PJK/RK Modified for new PF coil current calculations
    !+ad_hist  15/05/14 PJK Added vstot to output
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    integer :: jj,k

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Volt Second Consumption')

    write(outfile,10) vsefsu,vsefbn,vseft,vsohsu,vsohbn,vsoh,vssu,vsbn,vstot
10  format(t15,'volt-sec',t30,'volt-sec',t45,'volt-sec'/ &
         t15,  'start-up',t32,'burn',t46,'total'// &
         t2,'PF coils :',t13,3(f10.2,5x)/ &
         t2,'OH coil  :',t13,3(f10.2,5x)/ &
         t15,8('-'),t30,8('-'),t45,8('-')/ &
         t2,'Total :   ',t13,3(f10.2,5x) )

    call oblnkl(outfile)
    call ovarre(outfile,'Total volt-second consumption by coils (Wb)','(vstot)',vstot)

    call osubhd(outfile, &
         'Summary of volt-second consumption by circuit (Wb) :')

    write(outfile,20)
20  format(' circuit', t16,'BOP',t31,'BOF',t46,'EOF')

    call oblnkl(outfile)

    write(outfile,30) (k,vsdum(k,1),vsdum(k,2),vsdum(k,3),k=1,nef)
30  format(t4,i3,t10,f10.3,5x,f10.3,5x,f10.3)

    write(outfile,40) vsdum(nohc,1),vsdum(nohc,2),vsdum(nohc,3)
40  format(' OH coil',t10,f10.3,5x,f10.3,5x,f10.3)

    call oshead(outfile,'Waveforms')
    call ocmmnt(outfile,'Currents (Amps/coil) as a function of time :')
    call oblnkl(outfile)

    write(outfile,50)(tim(k),k=1,6)
50  format(t40,'time (sec)'//t10,6f11.2)

    call ocmmnt(outfile,'circuit')

    do k = 1,ncirt-1
       write(outfile,60) k,((cpt(k,jj)*turns(k)),jj=1,6)
    end do
60  format(t3,i2,t12,6(1pe11.3))

    write(outfile,70) (cpt(ncirt,jj),jj=1,6)
70  format(' Plasma (A)',t12,6(1pe11.3))

    call oblnkl(outfile)
    call ocmmnt(outfile,'This consists of: CS coil field balancing:')
    do k = 1,ncirt-1
       write(outfile,80) k,cpt(k,1)*turns(k), &
            cpt(k,2)*turns(k), &
            -cpt(k,2)*turns(k)*(fcohbof/fcohbop), &
            -cpt(k,2)*turns(k)*(fcohbof/fcohbop), &
            -cpt(k,2)*turns(k)*(1.0D0/fcohbop), &
            cpt(k,6)*turns(k)
    end do
80  format(t3,i2,t12,6(1pe11.3))

    call oblnkl(outfile)
    call ocmmnt(outfile,'And: equilibrium field:')
    do k = 1,ncirt-1
       write(outfile,90) k,0.0D0, &
            0.0D0, &
            (cpt(k,3)+cpt(k,2)*(fcohbof/fcohbop))*turns(k), &
            (cpt(k,4)+cpt(k,2)*(fcohbof/fcohbop))*turns(k), &
            (cpt(k,5)+cpt(k,2)*(1.0D0/fcohbop))*turns(k), &
            0.0D0
    end do
90  format(t3,i2,t12,6(1pe11.3))

    call oblnkl(outfile)
    write(outfile,100) fcohbop, fcohbof
100 format(' fcohbop:',f10.3,5x,'fcohbof:',f10.3)

  end subroutine outvolt

end module pfcoil_module
