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
  !+ad_cont  superconpf
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
  !+ad_hist  16/10/14 PJK Added sctfcoil_module
  !+ad_hist  22/02/17 JM  Changed function selfinductance to Bunet's formula
  !+ad_hist  27/02/17 JM  Added WST Nb3Sn as option for superconductor
  !+ad_hist  22/03/17 JM  Removed pfjalw as no longer used anywhere
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
  use sctfcoil_module
  use superconductors
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: pfcoil, outpf, outvolt, induct, vsec, bfield, brookscoil

  !  Local variables

  integer :: nef,nfxf
  real(kind(1.0D0)) :: ricpf, ssq0, sig_axial, sig_hoop
  real(kind(1.0D0)) :: axial_force
  real(kind(1.0D0)), dimension(nfixmx) :: rfxf,zfxf,cfxf,xind
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls,zcls
  real(kind(1.0D0)), dimension(ngrpmx) :: ccls,ccl0
  real(kind(1.0D0)), dimension(ngc2) :: bpf2
  real(kind(1.0D0)), dimension(ngc2,3) :: vsdum

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pfcoil

    !+ad_name  pfcoil
    !+ad_summ  Routine to perform calculations for the PF and Central Solenoid coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine performs the calculations for the PF and
    !+ad_desc  Central Solenoid coils, to determine their size, location, current waveforms,
    !+ad_desc  stresses etc.
    !+ad_prob  On the very first call, the inductance matrix sxlg has not
    !+ad_prob  previously been calculated by routine induct, so could in theory
    !+ad_prob  contain any values... This is currently dealt with by setting
    !+ad_prob  the matrix to unity during the first call to prevent problems.
    !+ad_call  efc
    !+ad_call  ohcalc
    !+ad_call  peakb
    !+ad_call  report_error
    !+ad_call  superconpf
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
    !+ad_hist  22/09/14 PJK Renamed snswit to top_bottom
    !+ad_hist  16/10/14 PJK New calculation for critical current density
    !+ad_hisc               and steel case thickness
    !+ad_hist  17/11/14 PJK Removed aturn argument from superconpf
    !+ad_hist  24/11/14 PJK Corrected wtc for resistive coils
    !+ad_hist  16/01/15 JM  Added variable "itr_sum" for new costs module
    !+ad_hist  16/01/15 JM  Added counter variables "c", "m" & "n" for new costs module
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer, parameter :: lrow1 = 2*nptsmx + ngrpmx
    integer, parameter :: lcol1 = ngrpmx

    integer :: i,ii,iii,ij,it,j,k,c,m,n,ncl,nfxf0,ng2,ngrp0,nng,nocoil,npts,npts0
    integer :: ccount, top_bottom
    integer, dimension(ngrpmx) :: pcls0
    integer, dimension(ngrpmx+2) :: ncls0

    real(kind(1.0D0)) :: area,areaspf,bmax,bri,bro,bzi,bzo,ioheof, &
         drpdz,drpt,dx,dz,forcepf,rclsnorm,respf,rll,rpt0,ssqef,volpf, &
         pfflux,csflux,dics,ddics,jstrand,jsc,tmarg
    real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls0,zcls0
    real(kind(1.0D0)), dimension(ngrpmx/2) :: ccls0
    real(kind(1.0D0)), dimension(ngrpmx) :: sigma,work2
    real(kind(1.0D0)), dimension(nclsmx) :: rc,zc,cc,xc
    real(kind(1.0D0)), dimension(nptsmx) :: brin,bzin,rpts,zpts
    real(kind(1.0D0)), dimension(lrow1) :: bfix,bvec
    real(kind(1.0D0)), dimension(lrow1,lcol1) :: gmat,umat,vmat
    real(kind(1.0D0)), dimension(2) :: signn

    real(kind(1.0D0)), dimension(ngc2) :: aturn

    logical :: first_call = .true.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Toggle switch for ipfloc()=2 coils above/below midplane

    top_bottom = 1

    !  Set up the number of PF coils including the Central Solenoid (nohc),
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

    !  Add one if an Central Solenoid is present, and make an extra group

    if (iohcl /= 0) then
       nohc = nohc + 1
       ncls(ngrp+1) = 1
    end if

    !  Add one for the plasma

    ncirt = nohc + 1

    !  Overall current density in the Central Solenoid at beginning of pulse

    cohbop = coheof * fcohbop

    !  Set up array of times

    tim(1) = 0.0D0
    tim(2) = tramp
    tim(3) = tim(2) + tohs
    tim(4) = tim(3) + theat
    tim(5) = tim(4) + tburn
    tim(6) = tim(5) + tqnch

    !  Set up call to MHD scaling routine for coil currents.
    !  First break up Central Solenoid solenoid into 'filaments'

    !  Central Solenoid radius

    if (itart == 1) then
       rohc = bore + tfcth + gapoh + 0.5D0*ohcth
    else
       rohc = bore + 0.5D0*ohcth
    end if

    !  nfxf is the total no of filaments into which the Central Solenoid is split,
    !  if present

    if (iohcl == 0) then
       nfxf = 0
       ioheof = 0.0D0
    else
       nfxf = 2*nfxfh
       ioheof = -hmax*ohhghf*ohcth*2.0D0*coheof  !  total Central Solenoid current at EOF

       if (nfxf > nfixmx) then
          idiags(1) = nfxf ; idiags(2) = nfixmx
          call report_error(66)
       end if

       !  Symmetric up/down Central Solenoid : Find (R,Z) and current of each filament at BOP

       do nng = 1,nfxfh
          rfxf(nng) = rohc
          rfxf(nng+nfxfh) = rfxf(nng)
          zfxf(nng) = hmax*ohhghf/nfxfh * (nng-0.5D0)
          zfxf(nng+nfxfh) = -zfxf(nng)
          cfxf(nng) = -ioheof/nfxf * fcohbop
          cfxf(nng+nfxfh) = cfxf(nng)
       end do
    end if

    !  Scale PF coil locations

    signn(1) =  1.0D0
    signn(2) = -1.0D0
    rclsnorm = rtot + 0.5D0*tfthko + routr

    !  N.B. Problems here if k=ncls(group) is greater than 2.

    do j = 1,ngrp

       if (ipfloc(j) == 1) then

          !  PF coil is stacked on top of the Central Solenoid

          do k = 1,ncls(j)
             rcls(j,k) = rohc + rpf1

             !  Z coordinate of coil enforced so as not
             !  to occupy the same space as the Central Solenoid

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
                if (top_bottom == 1) then  !  this coil is above midplane
                   zcls(j,k) = hpfu + 0.86D0
                   top_bottom = -1
                else  !  this coil is below midplane
                   zcls(j,k) = -1.0D0 * (hpfu - 2.0D0*hpfdif + 0.86D0)
                   top_bottom = 1
                end if
             end if
          end do

       else if (ipfloc(j) == 3) then

          !  PF coil is radially outside the TF coil

          do k = 1,ncls(j)
             zcls(j,k) = rminor * zref(j) * signn(k)
             !  coil radius follows TF coil curve
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

    !  Simple coil current scaling for STs (good only for A < about 1.8)

    if (itart == 1) then

       do i = 1,ngrp

          if (ipfloc(i) == 1) then

             !  PF coil is stacked on top of the Central Solenoid

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

             !  PF coil is stacked on top of the Central Solenoid
             !  This coil is to balance Central Solenoid flux and should not be involved
             !  in equilibrium calculation -- RK 07/12

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

    fcohbof = ((-ioheof * fcohbop) + dics)/ioheof
    fcohbof = min(fcohbof,  1.0D0)  !  constrains abs(fcohbof) <= 1.0;
    fcohbof = max(fcohbof, -1.0D0)  !  probably un-necessary

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

    !  Current in Central Solenoid as a function of time
    !  N.B. If the Central Solenoid is not present then ioheof is zero.

    curpfs(ncl+1) = -1.0D-6 * ioheof * fcohbop
    curpff(ncl+1) = 1.0D-6 * ioheof * fcohbof
    curpfb(ncl+1) = 1.0D-6 * ioheof

    !  Set up coil current waveforms, normalised to the peak current in
    !  each coil

    call waveform  !  returns ric(), waves()

    !  Calculate PF coil geometry, current and number of turns
    !  Dimensions are those of the winding pack, and exclude
    !  the steel supporting case

    i = 0
    pfrmax = 0.0D0

    do ii = 1,ngrp
       do ij = 1,ncls(ii)
          i = i + 1

          if (ipfloc(ii) == 1) then

             !  PF coil is stacked on top of the Central Solenoid

             dx = 0.5D0 * ohcth
             dz = 0.5D0 * (hmax*(1.0D0-ohhghf) + tfcth + 0.1D0)  !  ???
             area = 4.0D0 * dx * dz

             !  Number of turns
             !  CPTDIN(I) is the current per turn (input)

             turns(i) = abs( (ric(i)*1.0D6)/cptdin(i) )
             aturn(i) = area / turns(i)

             !  Actual winding pack current density

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

             turns(i) = abs( (ric(i)*1.0D6)/cptdin(i) )
             aturn(i) = area / turns(i)

             dx = 0.5D0 * sqrt(area)  !  square cross-section

             ra(i) = rpf(i) - dx
             rb(i) = rpf(i) + dx

             zl(i) = zpf(i) - dx
             if (zpf(i) < 0.0D0) zl(i) = zpf(i) + dx

             zh(i) = zpf(i) + dx
             if (zpf(i) < 0.0D0) zh(i) = zpf(i) - dx

          end if

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

          if (ij == 1) call peakb(i,iii,it,bri,bro,bzi,bzo)  !  returns bpf, bpf2

          !  Allowable current density (for superconducting coils)

          if (ipfres == 0) then
             bmax = max(abs(bpf(i)), abs(bpf2(i)))
             call superconpf(bmax,vf(i),fcupfsu,rjconpf(i),isumatpf,fhts, &
                  strncon_pf,tftmp,bcritsc,tcritsc,rjpfalw(i),jstrand,jsc,tmarg)
          end if

          !  Length of conductor

          rll = 2.0D0*pi*rpf(i)*turns(i)

          !  Resistive coils

          if (ipfres == 1) then

             !  Coil resistance (vf is the void fraction)

             respf = pfclres * rll / ( aturn(i) * (1.0D0-vf(i)) )

             !  Sum resistive power losses

             powpfres = powpfres + respf * (1.0D6 * curpfb(i)/turns(i))**2

          end if

          !  Winding pack volume

          volpf = aturn(i) * rll

          !  Conductor weight (vf is the void fraction)

          if (ipfres == 0) then
             wtc(i) = volpf * dcond(isumatpf) * (1.0D0-vf(i))
          else
             wtc(i) = volpf * dcopper * (1.0D0-vf(i))
          end if

          !  (J x B) force on coil

          forcepf = 0.5D6 * (bpf(i) + bpf2(i)) * abs(ric(i)) * rpf(i)

          !  Stress ==> cross-sectional area of supporting steel to use

          if (ipfres == 0) then

             !  Superconducting coil
             !  Previous assumptions: 500 MPa stress limit with 2/3 of the force
             !  supported in the outer (steel) case.
             !  Now, 500 MPa replaced by sigpfcalw, 2/3 factor replaced by sigpfcf

             areaspf = sigpfcf * forcepf / (sigpfcalw*1.0D6)

             !  Assume a case of uniform thickness around coil cross-section
             !  Thickness found via a simple quadratic equation

             drpdz = rb(i) - ra(i) + abs(zh(i) - zl(i))  !  dr + dz
             pfcaseth(i) = 0.25D0 * (-drpdz + sqrt(drpdz*drpdz + 4.0D0*areaspf))

          else
             areaspf = 0.0D0  !  Resistive coil - no steel needed
             pfcaseth(i) = 0.0D0
          end if

          !  Weight of steel case

          wts(i) = areaspf * 2.0D0*pi*rpf(i) * denstl

          !  Mass of heaviest PF coil (tonnes)

          pfmmax = max(pfmmax, (1.0D-3*(wtc(i)+wts(i))) )

       end do
    end do

    !  Find sum of current x turns x radius for all coils for 2015 costs model
    c = 0
    itr_sum = 0.0D0
    do m=1, ngrp
       do n=1, ncls(m)
          c = c + 1
          itr_sum = itr_sum + (rpf(c) * turns(c) * cptdin(c))
       end do
    end do
    itr_sum = itr_sum + ((bore + 0.5*ohcth) * turns(nohc) * cptdin(nohc))

    !  Find Central Solenoid information

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
    !+ad_summ  Routine to perform calculations for the Central Solenoid solenoid
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine performs the calculations for the
    !+ad_desc  Central Solenoid solenoid coil.
    !+ad_prob  None
    !+ad_call  bfmax
    !+ad_call  peakb
    !+ad_call  superconpf
    !+ad_hist  01/02/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  25/11/13 PJK Simplified (R,Z) calculation
    !+ad_hist  16/10/14 PJK New calculation for critical current density
    !+ad_hisc               and steel case area
    !+ad_hist  06/11/14 PJK Used strncon to specify strain in Central Solenoid superconductor
    !+ad_hist  10/11/14 PJK Clarified comments
    !+ad_hist  13/11/14 PJK Added fudge to ensure positive conductor area
    !+ad_hist  17/11/14 PJK Removed aturn argument from superconpf
    !+ad_hist  24/11/14 PJK Corrected wtc for resistive coils
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: timepoint

    real(kind(1.0D0)) :: areaspf, bmaxoh2, bohci, bohco, bri, bro, &
         bzi, bzo, da, forcepf, hohc, jcritwp, sgn, tmarg1, tmarg2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Half-height of Central Solenoid

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

    !  Total cross-sectional area

    areaoh = 2.0D0 * hohc * ohcth

    !  Maximum current (MA-turns) in Central Solenoid, at either BOP or EOF

    if (cohbop > coheof) then
       sgn = 1.0D0
       ric(nohc) = sgn * 1.0D-6*cohbop*areaoh
    else
       sgn = -1.0D0
       ric(nohc) = sgn * 1.0D-6*coheof*areaoh
    end if

    !  Number of turns

    turns(nohc) = 1.0D6 * abs(ric(nohc))/cptdin(nohc)

    !  Non-steel area void fraction for coolant

    vf(nohc) = vfohc

    !  Peak field at the End-Of-Flattop (EOF)
    !  Occurs at inner edge of coil; bmaxoh2 and bzi are of opposite sign at EOF

    !  Peak field due to Central Solenoid itself

    bmaxoh2 = bfmax(coheof,ra(nohc),rb(nohc),hohc)

    !  Peak field due to other PF coils plus plasma

    timepoint = 5 ; call peakb(nohc,99,timepoint,bri,bro,bzi,bzo)

    bmaxoh = abs(bzi - bmaxoh2)
    bohci = bmaxoh

    !  Peak field on outboard side of Central Solenoid
    !  (self-field is assumed to be zero - long solenoid approximation)

    bohco = abs(bzo)

    !  Peak field at the Beginning-Of-Pulse (BOP)
    !  Occurs at inner edge of coil; bmaxoh0 and bzi are of same sign at BOP

    bmaxoh0 = bfmax(cohbop,ra(nohc),rb(nohc),hohc)
    timepoint = 2 ; call peakb(nohc,99,timepoint,bri,bro,bzi,bzo)

    bmaxoh0 = abs(bmaxoh0 + bzi)

    !  Maximum field values

    bpf(nohc) = max(bmaxoh, abs(bmaxoh0))
    bpf2(nohc) = max(bohco, abs(bzo))

    !  (J x B) hoop force on Central Solenoid (N)

    forcepf = 0.5D6 * (bpf(nohc)+bpf2(nohc))*abs(ric(nohc))*rpf(nohc)

    !  Stress ==> cross-sectional area of supporting steel to use

    if (ipfres == 0) then

       !  Superconducting coil

       ! New calculation from M. N. Wilson for hoop stress
       call hoop_stress(ra(nohc), sig_hoop)

       ! New calculation from Y. Iwasa for axial stress
       call axial_stress(sig_axial,axial_force)

       !  Allowable (hoop) stress (Pa) alstroh
       ! Now a user input
       ! alstroh = min( (2.0D0*csytf/3.0D0), (0.5D0*csutf) )

       ! Now steel area fraction is iteration variable and constraint
       ! equation is used for Central Solenoid stress

       ! Area of steel in Central Solenoid
       areaspf = oh_steel_frac*areaoh
       ! areaspf = forcepf / alstroh

       if (i_cs_stress == 1) then
           s_tresca_oh = max(ABS(sig_hoop-sig_axial), ABS(sig_axial-0.0D0), ABS(0.0D0 - sig_hoop))
       else
           s_tresca_oh = max(ABS(sig_hoop-0.0D0), ABS(0.0D0-0.0D0), ABS(0.0D0 - sig_hoop))
       end if

       !  Thickness of hypothetical steel cylinders assumed to encase the CS along
       !  its inside and outside edges; in reality, the steel is distributed
       !  throughout the conductor
       pfcaseth(nohc) = 0.25D0 * areaspf/hohc

    else
       areaspf = 0.0D0  !  Resistive Central Solenoid - no steel needed
       pfcaseth(nohc) = 0.0D0
    end if

    !  Weight of steel

    wts(nohc) = areaspf * 2.0D0*pi*rpf(nohc) * denstl

    !  Non-steel cross-sectional area

    awpoh = areaoh - areaspf

    !  Issue #97. Fudge to ensure awpoh is positive; result is continuous, smooth and
    !  monotonically decreases

    da = 0.0001D0  !  1 cm^2
    if (awpoh < da) awpoh = da*da / (2.0D0*da - awpoh)

    !  Weight of conductor in Central Solenoid

    if (ipfres == 0) then
       wtc(nohc) = awpoh * (1.0D0-vfohc) * 2.0D0*pi*rpf(nohc) * dcond(isumatoh)
    else
       wtc(nohc) = awpoh * (1.0D0-vfohc) * 2.0D0*pi*rpf(nohc) * dcopper
    end if

    if (ipfres == 0) then

       !  Allowable coil overall current density at EOF
       !  (superconducting coils only)

       call superconpf(bmaxoh,vfohc,fcuohsu,(abs(ric(nohc))/awpoh)*1.0D6, &
            isumatoh,fhts,strncon_cs,tftmp,bcritsc,tcritsc,jcritwp, &
            jstrandoh_eof,jscoh_eof,tmarg1)

       rjohc = jcritwp * awpoh/areaoh

       !  Allowable coil overall current density at BOP

       call superconpf(bmaxoh0,vfohc,fcuohsu,(abs(ric(nohc))/awpoh)*1.0D6, &
            isumatoh,fhts,strncon_cs,tftmp,bcritsc,tcritsc,jcritwp, &
            jstrandoh_bop,jscoh_bop,tmarg2)

       rjpfalw(nohc) = jcritwp * awpoh/areaoh
       rjohc0 = rjpfalw(nohc)

       tmargoh = min(tmarg1, tmarg2)

    else
       !  Resistive power losses (non-superconducting coil)

       powohres = 2.0D0 * pi * rohc * pfclres / &
            (areaoh * (1.0D0-vfohc)) * (1.0D6*ric(nohc))**2
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
    !+ad_stat  Okay; results agree with Culham MAGLIB routines
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

    !  Central Solenoid contribution

    if ((iohcl /= 0).and.(i == nohc)) then
       !  Peak field is to be calculated at the Central Solenoid itself,
       !  so exclude its own contribution; its self field is
       !  dealt with externally using routine BFMAX
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
          !  No Central Solenoid
          kk = 0
       else
          if (cohbop > coheof) then
             sgn = 1.0D0
          else
             sgn = -1.0D0
          end if

          !  Current in each filament representing part of the Central Solenoid

          do iohc = 1,nfxf
             cfxf(iohc) = waves(nohc,it)*coheof*sgn*ohcth*ohhghf &
                  * hmax / nfxf * 2.0D0
          end do
          kk = nfxf
       end if

    end if

    !  Non-Central Solenoid coils' contributions

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

    !  Plasma contribution

    if (it > 2) then
       kk = kk + 1
       rfxf(kk) = rmajor
       zfxf(kk) = 0.0D0
       cfxf(kk) = plascur
    end if

    !  Calculate the field at the inner and outer edges
    !  of the coil of interest

    call bfield(kk,rfxf,zfxf,cfxf,xind,ra(i),zpf(i),bri,bzi,psi)
    call bfield(kk,rfxf,zfxf,cfxf,xind,rb(i),zpf(i),bro,bzo,psi)

    !  bpf and bpf2 for the Central Solenoid are calculated in OHCALC

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

  subroutine superconpf(bmax,fhe,fcu,jwp,isumat,fhts,strain,thelium, &
       bcritsc,tcritsc,jcritwp,jcritstr,jcritsc,tmarg)

    !+ad_name  superconpf
    !+ad_summ  Routine to calculate the PF coil superconductor properties
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  bmax : input real : Peak field at conductor (T)
    !+ad_args  fhe : input real : Fraction of cable space that is for He cooling
    !+ad_args  fcu : input real : Fraction of strand that is copper
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
    !+ad_args  thelium : input real : He temperature at peak field point (K)
    !+ad_args  bcritsc : input real : Critical field at zero temperature and strain (T) (isumat=4 only)
    !+ad_args  tcritsc : input real : Critical temperature at zero field and strain (K) (isumat=4 only)
    !+ad_args  jcritwp : output real : Critical winding pack current density (A/m2)
    !+ad_args  jcritstr : output real : Critical strand current density (A/m2)
    !+ad_args  jcritsc : output real : Critical superconductor current density (A/m2)
    !+ad_args  tmarg : output real : Temperature margin (K)
    !+ad_desc  This routine calculates the superconductor critical winding pack
    !+ad_desc  current density for the PF coils, plus the temperature margin.
    !+ad_desc  It is based on the TF coil version, <CODE>supercon</CODE>.
    !+ad_prob  The conduit and insulation around each turn is neglected.
    !+ad_call  bi2212
    !+ad_call  itersc
    !+ad_call  jcrit_nbti
    !+ad_call  report_error
    !+ad_hist  16/10/14 PJK Initial version
    !+ad_hist  06/11/14 PJK Added jcritstr and jcritsc outputs; inverted
    !+ad_hisc               areas in bi2212 jstrand input
    !+ad_hist  11/11/14 PJK Added temperature margin calculation
    !+ad_hist  17/11/14 PJK Removed aturn argument
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: isumat
    real(kind(1.0D0)), intent(in) :: bmax, fcu, fhe, fhts, jwp, &
         strain, thelium, bcritsc, tcritsc
    real(kind(1.0D0)), intent(out) :: jcritwp, jcritstr, jcritsc, tmarg

    !  Local variables

    integer :: lap
    real(kind(1.0D0)) :: b,bc20m,bcrit,c0,delt,jcrit0,jcritm, &
         jcritp,jsc,jstrand,jtol,t,tc0m,tcrit,ttest,ttestm,ttestp

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

       jstrand = jwp / (1.0D0-fhe)

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

    case (5) ! WST Nb3Sn parameterisation
         bc20m = 32.97D0
         tc0m = 16.06D0

         !  jcritsc returned by itersc is the critical current density in the
         !  superconductor - not the whole strand, which contains copper

         call wstsc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
         jcritstr = jcritsc * (1.0D0-fcu)

    case default  !  Error condition
       idiags(1) = isumat ; call report_error(156)

    end select

    !  Critical current density in winding pack

    jcritwp = jcritstr * (1.0D0-fhe)

    !  Temperature margin (already calculated in bi2212 for isumat=2)

    if (isumat /= 2) then

       !  Newton-Raphson method; start at requested minimum temperature margin

       ttest = thelium + tmargmin_cs
       delt = 0.01D0
       jtol = 1.0D4

       !  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
       !  when we have found the desired value of tmarg

       jstrand = jwp / (1.0D0-fhe)
       jsc = jstrand / (1.0D0-fcu)

       lap = 0
       solve_for_tmarg: do ; lap = lap+1
          if ((ttest <= 0.0D0).or.(lap > 100)) then
             idiags(1) = lap ; fdiags(1) = ttest
             call report_error(158)
             exit solve_for_tmarg
          end if
          ttestm = ttest - delt
          ttestp = ttest + delt
          select case (isumat)
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
          end select
          ttest = ttest - 2.0D0*delt*(jcrit0-jsc)/(jcritp-jcritm)
       end do solve_for_tmarg

       tmarg = ttest - thelium

    end if

  end subroutine superconpf

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
       !  No Central Solenoid
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

    !  Central Solenoid startup volt-seconds

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

  subroutine hoop_stress(r, s_hoop)
    !+ad_name  hoop_stress
    !+ad_summ  Calculation of hoop stress of central solenoid
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  r : input real : radial position a < r < b
    !+ad_args  s_hoop : output real : hoop stress (MPa)
    !+ad_desc  This routine calculates the hoop stress of the central solenoid
    !+ad_desc  from "Superconducting magnets", M. N. Wilson OUP
    !+ad_prob  None
    !+ad_hist  24/02/17 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: r
    real(kind(1.0D0)), intent(out) :: s_hoop

    !  Local variables
    real(kind(1.0D0)) :: K, M, a, b, B_a, B_b, alpha, epsilon, j

    real(kind(1.0D0)) :: hp_term_1, hp_term_2, hp_term_3, hp_term_4

    real(kind(1.0D0)) :: s_hoop_nom

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Inner radius of Central Solenoid [m]
    a = ra(nohc)

    ! Outer radius of Central Solenoid [m]
    b = rb(nohc)

    ! alpha
    alpha = b/a

    ! epsilon
    epsilon = r/a

    ! Field at inner radius of coil [T]
    B_a = bmaxoh0

    ! Field at outer radius of coil [T]
    ! Assume to be 0 for now
    B_b = 0.0D0

    ! current density [A/m^2]
    j = cohbop

    ! K term
    K = ((alpha*B_a - B_b)*j*a)/(alpha - 1.0D0)

    ! M term
    M = ((B_a - B_b)*j*a)/(alpha - 1.0D0)

    ! calculate hoop stress terms
    hp_term_1 = K*((2.0D0 + poisson)/(3.0D0*(alpha + 1.0D0)))

    hp_term_2 = alpha**2 + alpha + 1.0D0 + alpha**2/epsilon**2 - &
      epsilon*(((1.0D0 + 2.0D0*poisson)*(alpha + 1.0D0)) / (2.0D0 + poisson))

    hp_term_3 = M*((3.0D0 + poisson) / (8.0D0))

    hp_term_4 = alpha**2 + 1.0D0 + alpha**2/epsilon**2 - &
            epsilon**2*((1.0D0 + 3.0D0*poisson)/(3.0D0 + poisson))

    s_hoop_nom = hp_term_1*hp_term_2 - hp_term_3*hp_term_4

    s_hoop = s_hoop_nom/oh_steel_frac

  end subroutine hoop_stress

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine axial_stress(s_axial,axial_force)
    !+ad_name  axial_stress
    !+ad_summ  Calculation of axial stress of central solenoid
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  r : input real : radial position a < r < b
    !+ad_args  s_hoop : output real : hoop stress (MPa)
    !+ad_desc  This routine calculates the axial stress of the central solenoid
    !+ad_desc  from "Case studies in superconducting magnets", Y. Iwasa, Springer
    !+ad_prob  None
    !+ad_hist  27/02/17 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(out) :: s_axial,axial_force

    !  Local variables
    real(kind(1.0D0)) :: b, hl, ni, area_ax

    real(kind(1.0D0)) :: kb2, k2b2, ekb2_1, ekb2_2, ek2b2_1, ek2b2_2

  !real(kind(1.0D0)) :: kb, k2b

    real(kind(1.0D0)) :: axial_term_1, axial_term_2, axial_term_3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Outer radius of Central Solenoid [m]
    b = rb(nohc)

    ! Half height of Central Solenoid [m]
    hl = zh(nohc)

    ! Central Solenoid current [A]
    ni = ric(nohc)*1.0E6

    ! kb term for elliptical integrals
    ! kb2 = SQRT((4.0D0*b**2)/(4.0D0*b**2 + hl**2))
    kb2 = (4.0D0*b**2)/(4.0D0*b**2 + hl**2)

    ! k2b term for elliptical integrals
    !k2b2 = SQRT((4.0D0*b**2)/(4.0D0*b**2 + 4.0D0*hl**2))
    k2b2 = (4.0D0*b**2)/(4.0D0*b**2 + 4.0D0*hl**2)

    ! term 1
    axial_term_1 = -(rmu0/2.0D0)*(ni/(2.0D0*hl))**2

    ! term 2
    call ellipke(kb2, ekb2_1, ekb2_2)
    axial_term_2 = 2.0D0*hl*(SQRT(4.0D0*b**2 + hl**2))*(ekb2_1 - ekb2_2)

    ! term 3
    call ellipke(k2b2, ek2b2_1, ek2b2_2)
    axial_term_3 = 2.0D0*hl*(SQRT(4.0D0*b**2 + 4.0D0*hl**2))*(ek2b2_1 - ek2b2_2)

    ! calculate axial force [N]
    axial_force = axial_term_1*(axial_term_2 - axial_term_3)

    ! axial area [m2]
    area_ax = pi*(rb(nohc)**2 - ra(nohc)**2)

    ! calculate unsmeared axial stress [MPa]
    s_axial = axial_force/(oh_steel_frac*0.5*area_ax)

  end subroutine axial_stress

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
    !+ad_hist  26/11/13 PJK Improved Central Solenoid self inductance, and Central Solenoid-plasma
    !+ad_hisc               mutual inductance calculations;
    !+ad_hisc               Removed obsolete argument to bfield calls
    !+ad_hist  25/02/14 PJK Raised nohmax, and added warning message
    !+ad_hisc               if noh is too large
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  06/01/16 MDK Put the self-inductance formula in a function,
    !+ad_hist  06/01/16 MDK added Brooks coil test
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    integer, parameter :: nohmax = 200 !  Maximum no. of segments for the Central Solenoid
    integer, parameter :: nplas = 1 !  Number of filaments describing the plasma

    real(kind(1.0D0)), allocatable, dimension(:) :: roh,zoh
    real(kind(1.0D0)), dimension(nplas) :: rplasma,zplasma
    real(kind(1.0D0)), dimension(ngc2+nohmax) :: rc,zc,xc,cc,xcin,xcout
    real(kind(1.0D0)) :: a,b,c,br,bz,deltar,delzoh,psi,reqv,rl,rp
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

    !  Break Central Solenoid into noh segments
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

    !  Central Solenoid / plasma mutual inductance
    !
    !  Improved calculation: Each Central Solenoid segment is now split into two filaments,
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

       !  Central Solenoid self inductance
       a = rohc                 !  mean radius of coil
       b = 2.0D0*zh(nohc)       !  length of coil
       c = rb(nohc) - ra(nohc)  !  radial winding thickness
       sxlg(nohc,nohc) = selfinductance(a,b,c,turns(nohc))

       !  Central Solenoid / PF coil mutual inductances

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
230 format('  CS',t9,20(1pe8.1))

    write(outfile,240) (sxlg(ij,ncirt),ij=1,ncirt)
240 format(' Plasma',t9,20(1pe8.1))

  end subroutine induct

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function selfinductance(a,b,c,N)
    !+ad_name  selfinductance
    !+ad_summ  Calculates the selfinductance using Bunet's formula
    !+ad_type  Function returning real
    !+ad_auth  M. Kovari, CCFE
    !+ad_cont  N/A
    !+ad_args  a  : input real : mean radius of coil (m)
    !+ad_args  b  : input real : length of coil (m) (given as l in the reference)
    !+ad_args  c  : input real : radial winding thickness (m)
    !+ad_args  N  : input real : number of turns
    !+ad_desc  This routine calculates the self inductance in Henries
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  22/02/17 JM  Initial version (issue #396)
    !+ad_stat  Okay
    !+ad_docs  Radiotron Designers Handbook (4th Edition) chapter 10
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Function declarations
    real(kind(1.0D0)) :: a, b, c, N, selfinductance

    ! Calculate self inductance
    selfinductance = (1.0d-6/0.0254d0)*a**2*N**2/(9.0d0*a + 10.0d0*b + 8.4d0*c + 3.2d0*c*b/a)

    ! OLD Formula
    ! JM - KEEP FOR NOW
    ! Formulas and tables for the calculation of mutual and self-inductance [Revised],
    ! Rosa and Grover, Scientific papers of the Bureau of Standards, No. 169, 3rd ed., 1916
    ! a = mean radius of coil
    ! b = length of coil
    ! c = radial winding thickness
    ! N = number of turns
    ! real(kind(1.0D0)) :: a,b,c, N, selfinductance, r, r2_16a2, x, x2, q, at, lambda, mu, p

    ! !  Equation 88, p. 137
    ! x = b/c
    ! x2 = x**2
    ! q = log(1.0d0+x2)
    ! p = log(1.0d0+1.0d0/x2)
    ! at = atan(x)
    ! lambda = log(8.0d0*a/c) + 1/12.0d0 - pi*x/3.0d0 - 0.5d0*q + 1/(12.0d0*x2)*q &
    !           + 1/12.0d0*x2*p + 2.0d0/3.0d0*(x-1.0d0/x)*at

    ! mu = c**2/(96.0d0*a**2)*     &
    !      ((log(8.0d0*a/c)-0.5d0*q)*(1+3.0d0*x2) + 3.45d0*x2 + 221.0d0/60.0d0   &
    !      -1.6d0*pi*x**3 + 3.2d0*x**3*at - 0.1d0/x2*q + 0.5d0*x2**2*p)

    ! selfinductance = rmu0*a*N**2 * (lambda + mu)

    ! if((selfinductance<0.0d0).or.(selfinductance .ne. selfinductance).or.(selfinductance>1000.0d0)) then
    !     write(*,*) 'a = ',a, ' b = ', b
    !     write(*,*) 'c = ',c, ' x = ', x
    !     write(*,*) 'q = ',q, ' at = ', at
    !     write(*,*) 'x2 = ',x2, ' N = ', N
    !     write(*,*) 'lambda = ',lambda, ' mu = ', mu
    !     write(*,*) 'selfinductance = ',selfinductance
    ! end if

  end function selfinductance

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine brookscoil(outfile)
    ! http://www.nessengr.com/techdata/brooks/brooks.html
    real(kind(1.0D0)) :: a,b,c, N, l, lp
    character(len=10) :: test
    integer, intent(in) :: outfile

    c = 1.0d0
    a = 1.5d0 * c
    b = c
    N = 1.0d0

    l = 0.025491d0 * c * 100.0d0 * N**2 * 1.0d-6
    lp = selfinductance(a,b,c,N)
    if ((l/lp < 1.05d0).and.(l/lp > 0.95d0)) then
        test = 'PASS'
    else
        test = 'FAIL'
    end if
    call ocmmnt(outfile,'Unit test of self-inductance formula: '//test)
    call ovarre(outfile,'Self-inductance of 1m Brooks coil: standard formula', '(l)',l, 'OP ')
    call ovarre(outfile,'Self-inductance of 1m Brooks coil: PROCESS formula', '(lp)',lp, 'OP ')
    call oblnkl(outfile)
    write(*,*) 'Test of self-inductance formula: '//test

  end subroutine brookscoil


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
    !+ad_hist  09/07/14 PJK Added info message if Central Solenoid current density is
    !+ad_hist               not reaching its upper limit
    !+ad_hist  01/09/14 PJK Changed .or. to .and. for the info message test
    !+ad_hist  15/10/14 PJK Added more outputs
    !+ad_hist  20/10/14 PJK Minor changes to output wording
    !+ad_hist  06/11/14 PJK Added extra diagnostic outputs
    !+ad_hist  10/11/14 PJK Removed critical current density output for resistive coils
    !+ad_hist  11/11/14 PJK Added extra area outputs
    !+ad_hist  13/10/15 MDK Issue #328 use max current in the PF Coil Table.
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
    logical :: CSlimit=.false.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Central Solenoid and PF Coils')

    if (iohcl == 0) then
       call ocmmnt(outfile,'No central solenoid included')
       call oblnkl(outfile)
    else
       if (ipfres == 0) then
          call ocmmnt(outfile,'Superconducting central solenoid')

          call ovarin(outfile,'Central solenoid superconductor material', &
               '(isumatoh)',isumatoh)

          select case (isumatoh)
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

          call osubhd(outfile,'Central Solenoid Current Density Limits :')
          call ovarre(outfile,'Maximum field at Beginning Of Pulse (T)', &
               '(bmaxoh0)',bmaxoh0, 'OP ')
          call ovarre(outfile,'Critical superconductor current density at BOP (A/m2)', &
               '(jscoh_bop)',jscoh_bop, 'OP ')
          call ovarre(outfile,'Critical strand current density at BOP (A/m2)', &
               '(jstrandoh_bop)',jstrandoh_bop, 'OP ')
          call ovarre(outfile,'Allowable overall current density at BOP (A/m2)', &
               '(rjohc0)',rjohc0, 'OP ')
          call ovarre(outfile,'Actual overall current density at BOP (A/m2)', &
               '(cohbop)',cohbop, 'OP ')
          call oblnkl(outfile)
          call ovarre(outfile,'Maximum field at End Of Flattop (T)', &
               '(bmaxoh)',bmaxoh, 'OP ')
          call ovarre(outfile,'Critical superconductor current density at EOF (A/m2)', &
               '(jscoh_eof)',jscoh_eof, 'OP ')
          call ovarre(outfile,'Critical strand current density at EOF (A/m2)', &
               '(jstrandoh_eof)',jstrandoh_eof, 'OP ')
          call ovarre(outfile,'Allowable overall current density at EOF (A/m2)', &
               '(rjohc)',rjohc, 'OP ')
          call ovarre(outfile,'Actual overall current density at EOF (A/m2)', '(coheof)',coheof)
          call oblnkl(outfile)
          ! MDK add ohcth, bore and gapoh as they can be iteration variables
          call ovarre(outfile,'CS inside radius (m)', '(bore.)',bore)
          call ovarre(outfile,'CS thickness (m)', '(ohcth.)',ohcth)
          call ovarre(outfile,'Gap between central solenoid and TF coil (m)', '(gapoh)',gapoh)
          call ovarre(outfile,'CS overall cross-sectional area (m2)', &
               '(areaoh)',areaoh, 'OP ')
          call ovarre(outfile,'CS conductor+void cross-sectional area (m2)', &
               '(awpoh)',awpoh, 'OP ')
          call ovarre(outfile,'   CS conductor cross-sectional area (m2)', &
               '(awpoh*(1-vfohc))',awpoh*(1.0D0-vfohc), 'OP ')
          call ovarre(outfile,'   CS void cross-sectional area (m2)', &
               '(awpoh*vfohc)',awpoh*vfohc, 'OP ')
          call ovarre(outfile,'CS steel cross-sectional area (m2)', &
               '(areaoh-awpoh)',areaoh-awpoh, 'OP ')
          call ovarre(outfile,'CS steel area fraction', &
               '(oh_steel_frac)',oh_steel_frac)
          if (i_cs_stress == 1) then
            call ocmmnt(outfile, 'Hoop + axial stress considered')
          else
            call ocmmnt(outfile, 'Only hoop stress considered')
          end if
          call ovarin(outfile,'Switch for CS stress calculation', &
               '(i_cs_stress)',i_cs_stress)
          call ovarre(outfile,'Allowable stress in CS steel (Pa)', &
               '(alstroh)',alstroh)
          call ovarre(outfile,'Hoop stress in CS steel (Pa)', &
               '(sig_hoop)', sig_hoop, 'OP ')
          call ovarre(outfile,'Axial stress in CS steel (Pa)', &
               '(sig_axial)', sig_axial, 'OP ')
          call ovarre(outfile,'Tresca stress in CS steel (Pa)', &
               '(s_tresca_oh)', s_tresca_oh, 'OP ')
          call ovarre(outfile,'Axial force in CS (N)', &
               '(axial_force)', axial_force, 'OP ')
          call ovarre(outfile,'Strain on CS superconductor', &
               '(strncon_cs)',strncon_cs)
          call ovarre(outfile,'Copper fraction in strand', &
               '(fcuohsu)',fcuohsu)
          call ovarre(outfile,'Void (coolant) fraction in conductor', &
               '(vfohc)',vfohc)
          call ovarre(outfile,'Helium coolant temperature (K)', &
               '(tftmp)',tftmp)
          call ovarre(outfile,'CS temperature margin (K)', &
               '(tmargoh)',tmargoh, 'OP ')
          call ovarre(outfile,'Minimum permitted temperature margin (K)', &
               '(tmargmin_cs)',tmargmin_cs)
          ! Check whether CS coil is hitting any limits
          ! iteration variable (39) fjohc0
          ! iteration variable(38) fjohc
          if ( (abs(coheof) > 0.99D0*abs(boundu(38)*rjohc)).or. &
               (abs(cohbop) > 0.99D0*abs(boundu(39)*rjohc0)) ) CSlimit=.true.
          if (tmargoh < 1.01D0*tmargmin_cs) CSlimit=.true.
          if (.not.CSlimit) call report_error(135)

       else
          call ocmmnt(outfile,'Resistive central solenoid')
       end if

    end if

    if (ipfres == 0) then
       call oblnkl(outfile)
       call ocmmnt(outfile,'Superconducting PF coils')

       call ovarin(outfile,'PF coil superconductor material','(isumatpf)',isumatpf)

       select case (isumatpf)
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

       call ovarre(outfile,'Copper fraction in conductor','(fcupfsu)',fcupfsu)

       call osubhd(outfile,'PF Coil Case Stress :')
       call ovarre(outfile,'Maximum permissible tensile stress (MPa)', &
            '(sigpfcalw)',sigpfcalw)
       call ovarre(outfile,'JxB hoop force fraction supported by case', &
            '(sigpfcf)',sigpfcf)

    else
       call oblnkl(outfile)
       call ocmmnt(outfile,'Resistive PF coils')

       call osubhd(outfile,'Resistive Power :')
       call ovarre(outfile,'PF coil resistive power (W)','(powpfres)', powpfres, 'OP ')
       if (iohcl /= 0) then
          call ovarre(outfile,'Central solenoid resistive power (W)','(powohres)', &
               powohres, 'OP ')
       end if

    end if

    !  nef is the number of coils excluding the Central Solenoid

    nef = nohc
    if (iohcl /= 0) nef = nef - 1

    call osubhd(outfile, 'Geometry of PF coils, central solenoid and plasma :')

    write(outfile,10)
10  format(' coil',t17,'R(m)',t29,'Z(m)',t41,'dR(m)',t53,'dZ(m)', &
         t65,'turns',t75,'steel thickness(m)')
    call oblnkl(outfile)

    !  PF coils

    write(outfile,20) (k,rpf(k),zpf(k),(rb(k)-ra(k)),abs(zh(k)-zl(k)), &
         turns(k),pfcaseth(k),k=1,nef)
20  format('  PF',i1,t10,6f12.2)

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

    !  Central Solenoid, if present

    if (iohcl /= 0) then
       write(outfile,30) rpf(nohc),zpf(nohc),(rb(nohc)-ra(nohc)), &
            abs(zh(nohc)-zl(nohc)),turns(nohc),pfcaseth(nohc)
30     format('  CS',t10,6f12.2)
       call ovarre(mfile,'Central solenoid radius (m)', &
            '(rpf(nohc))',rpf(nohc))
       call ovarre(mfile,'Central solenoid vertical position (m)', &
            '(zpf(nohc))',zpf(nohc))
       call ovarre(mfile,'Central solenoid radial thickness (m)', &
            '(ohdr)',(rb(nohc)-ra(nohc)))
       call ovarre(mfile,'Central solenoid vertical thickness (m)', &
            '(ohdz)',(zh(nohc)-zl(nohc)))
       call ovarre(mfile,'Central solenoid turns', &
            '(turns(nohc))',turns(nohc))
       call ovarre(mfile,'Central solenoid current (MA)', &
            '(ric(nohc))',ric(nohc))
       call ovarre(mfile,'Central solenoid field (T)', &
            '(bpf(nohc))',bpf(nohc))
    end if

    !  Plasma

    write(outfile,40) rmajor,0.0D0,2.0D0*rminor,2.0D0*rminor*kappa,1.0D0
40  format(' Plasma',t10,5f12.2)

    call osubhd(outfile,'PF Coil Information at Peak Current:')

    write(outfile,50)
50  format(' coil', &
         t8, 'current', &
         t17,'allowed J', &
         t28,'actual J', &
         t39,'J', &
         t43,'cond. mass', &
         t56,'steel mass', &
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
       if (ipfres == 0) then
          write(outfile,90) k,ric(k),rjpfalw(k),rjconpf(k), &
               (rjconpf(k)/rjpfalw(k)),wtc(k),wts(k),bpf(k)
       else
          write(outfile,90) k,ric(k),-1.0D0,rjconpf(k), &
               1.0D0,wtc(k),wts(k),bpf(k)
       end if
    end do

    !  The 0p syntax is needed here and on line 100
    !  to prevent a known compiler 'feature' from apparently
    !  multiplying the f-formatted numbers by 10.

90  format('  PF',i1,f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,1pe13.3)

    !  Central Solenoid, if present

    if (iohcl /= 0) then
       if (ipfres == 0) then
       ! Issue #328
          write(outfile,100) ric(nohc),rjpfalw(nohc),max(abs(cohbop),abs(coheof)), &
               (max(abs(cohbop),abs(coheof))/rjpfalw(nohc)),wtc(nohc),wts(nohc), &
               bpf(nohc)
       else
          write(outfile,100) ric(nohc),-1.0D0,max(abs(cohbop),abs(coheof)), &
               1.0D0,wtc(nohc),wts(nohc),bpf(nohc)
       end if
    end if

100    format('  CS ',f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,1pe13.3)

    !  Miscellaneous totals

    write(outfile,110)
110 format(t8,'------',t43,'---------',t55,'---------')

    write(outfile,120) ricpf,whtpf,whtpfs
120 format(t6,f8.2,t41,1pe11.3,1pe12.3)

    call osubhd(outfile,'PF coil current scaling information :')
    call ovarre(outfile,'Sum of squares of residuals ','(ssq0)',ssq0, 'OP ')
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
    !+ad_hist  20/10/14 PJK Central Solenoid to CS
    !+ad_hist  03/08/15 MDK Change in output format for fcohop, fcohbof
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables
    character(len=50) :: circuit_name, circuit_var_name
    integer :: jj,k,jjj


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Volt Second Consumption')

    write(outfile,10) vsefsu,vsefbn,vseft,vsohsu,vsohbn,vsoh,vssu,vsbn,vstot
10  format(t15,'volt-sec',t30,'volt-sec',t45,'volt-sec'/ &
         t15,  'start-up',t32,'burn',t46,'total'// &
         t2,'PF coils :',t13,3(f10.2,5x)/ &
         t2,'CS coil  :',t13,3(f10.2,5x)/ &
         t15,8('-'),t30,8('-'),t45,8('-')/ &
         t2,'Total :   ',t13,3(f10.2,5x) )

    call oblnkl(outfile)
    call ovarre(outfile,'Total volt-second consumption by coils (Wb)','(vstot)',vstot, 'OP ')

    call osubhd(outfile, &
         'Summary of volt-second consumption by circuit (Wb) :')

    write(outfile,20)
20  format(' circuit', t16,'BOP',t31,'BOF',t46,'EOF')

    call oblnkl(outfile)

    write(outfile,30) (k,vsdum(k,1),vsdum(k,2),vsdum(k,3),k=1,nef)
30  format(t4,i3,t10,f10.3,5x,f10.3,5x,f10.3)

    write(outfile,40) vsdum(nohc,1),vsdum(nohc,2),vsdum(nohc,3)
40  format(' CS coil',t10,f10.3,5x,f10.3,5x,f10.3)

    call oshead(outfile,'Waveforms')
    call ocmmnt(outfile,'Currents (Amps/coil) as a function of time :')
    call oblnkl(outfile)

    write(outfile,50)(tim(k),k=1,6)
50  format(t40,'time (sec)'//t10,6f11.2)

    write(outfile,51)(timelabel(k),k=1,6)
51  format(t16, 6a11)
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
    call ovarre(outfile,'Ratio of central solenoid current at beginning of Pulse / end of flat-top','(fcohbop)',fcohbop)
    call ovarre(outfile,'Ratio of central solenoid current at beginning of Flat-top / end of flat-top','(fcohbof)',fcohbof, 'OP ')

    call oshead(outfile,'PF Circuit Waveform Data')
    call ovarin(outfile,'Number of PF circuits including CS and plasma','(ncirt)',ncirt)
    do k = 1,ncirt
      do jjj = 1, 6
        if (k == ncirt) then
          circuit_name = 'Plasma ' // ' - Time point ' // int_to_string2(jjj) // ' (A)'
          circuit_var_name = '(plasma' // 't' // int_to_string2(jjj) // ')'
        else if (k == ncirt-1) then
          circuit_name = 'CS Circuit ' // ' - Time point ' // int_to_string2(jjj) // ' (A)'
          circuit_var_name = '(cs' // 't' // int_to_string2(jjj) // ')'
        else
          circuit_name = 'PF Circuit ' // int_to_string2(k) // ' - Time point ' // int_to_string2(jjj) // ' (A)'
          circuit_var_name = '(pfc' // int_to_string2(k) // 't' // int_to_string2(jjj) // ')'
        end if
        call ovarre(outfile,circuit_name,circuit_var_name, cpt(k,jjj)*turns(k))
      end do
    end do

  end subroutine outvolt

end module pfcoil_module
