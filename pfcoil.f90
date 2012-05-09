!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pfcoil

  !+ad_name  pfcoil
  !+ad_summ  Routine to perform calculations for the PF and OH coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine performs the calculations for the PF and
  !+ad_desc  OH coils, to determine their size, location, current waveforms,
  !+ad_desc  stresses etc.
  !+ad_prob  None
  !+ad_call  build.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  times.h90
  !+ad_call  efc
  !+ad_call  ohcalc
  !+ad_call  peakb
  !+ad_call  pfjalw
  !+ad_call  waveform
  !+ad_hist  01/02/96 PJK Initial version
  !+ad_hist  09/05/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'pfcoil.h90'
  include 'tfcoil.h90'
  include 'build.h90'
  include 'times.h90'

  !  Arguments

  !  Local variables

  integer, parameter :: lrow1 = 2*nptsmx + ngrpmx
  integer, parameter :: lcol1 = ngrpmx

  integer :: i,ido,ii,iii,ij,it,j,k,ncl,nfxf,nfxf0,ng2,ngrp0,nng,npts,npts0
  integer, dimension(ngrpmx+2) :: ncls0

  real(kind(1.0D0)) :: area,areaspf,bri,bro,bzi,bzo,curstot,drpt, &
       dx,dz,forcepf,respf,ricpf,rll,rpt0,ssq0,ssqef,volpf
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls,zcls,rcls0,zcls0
  real(kind(1.0D0)), dimension(ngrpmx/2) :: ccls0
  real(kind(1.0D0)), dimension(ngrpmx) :: ccls,ccls2,ccl0,sigma,work2
  real(kind(1.0D0)), dimension(nclsmx) :: rc,zc,cc,xc
  real(kind(1.0D0)), dimension(nptsmx) :: brin,bzin,rpts,zpts
  real(kind(1.0D0)), dimension(nfixmx) :: rfxf,zfxf,cfxf,xind,work1
  real(kind(1.0D0)), dimension(lrow1) :: bfix,bvec
  real(kind(1.0D0)), dimension(lrow1,lcol1) :: gmat,umat,vmat
  real(kind(1.0D0)), dimension(2) :: signn
  real(kind(1.0D0)), dimension(ngc2) :: bpf2

  real(kind(1.0D0)), external :: pfjalw

  common/pfout/ssq0, ricpf
  common/pfcom1/ rfxf,zfxf,cfxf,rcls,zcls,ccls,ccls2,ccl0,bpf2,xind,nfxf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Set up the number of PF coils including the OH coil (nohc),
  !  and the number of PF circuits including the plasma (ncirt)

  if (ngrp > ngrpmx) then
     write(*,*) 'Error in routine PFCOIL:'
     write(*,*) 'ngrp is larger than ngrpmx = ',ngrpmx
     write(*,*) 'Reduce the value of ngrp.'
     write(*,*) 'PROCESS stopping.'
     STOP
  end if

  !  Total the number of PF coils in all groups, and check that none
  !  exceeds the limit

  nohc = 0
  do i = 1,ngrp
     if (ncls(i) > nclsmx) then
        write(*,*) 'Error in routine PFCOIL:'
        write(*,*) 'PF coil group ',i,' contains ',ncls(i),' coils.'
        write(*,*) 'Maximum number allowed is ',nclsmx
        write(*,*) 'PROCESS stopping.'
        STOP
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

  !  Currents in the OH coil at various times

  cohbop = coheof * fcohbop
  cohbof = coheof * fcohbof

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
     rohc = bore + bcylth + tfcth + gapoh + 0.5D0*ohcth
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
        write(*,*) 'Error in routine PFCOIL:'
        write(*,*) 'nfxf is larger than nfixmx. nfxf = ',nfxf
        write(*,*) 'Reduce the value of nfxfh.'
        write(*,*) 'PROCESS stopping.'
        STOP
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
              zcls(j,k) = (hmax + tfcth + 0.86D0) * signn(k)
           end if
        end do

     else if (ipfloc(j) == 3) then

        !  PF coil is radially outside the TF coil

        do k = 1,ncls(j)
           rcls(j,k) = rtot + 0.5D0*tfthko + routr
           zcls(j,k) = rminor * zref(j) * signn(k)
        end do

     else
        write(*,*) 'Error in routine PFCOIL :'
        write(*,*) 'Illegal value of IPFLOC(',j,'), = ',ipfloc(j)
        write(*,*) 'PROCESS stopping.'
        STOP
     end if

  end do

  if (cohbop /= 0.0D0) then

     !  Find currents for plasma initiation to null field across plasma

     npts = 32  !  Number of test points across plasma midplane
     if (npts > nptsmx) then
        write(*,*) 'Error in routine PFCOIL:'
        write(*,*) 'npts is larger than nptsmx = ',nptsmx
        write(*,*) 'PROCESS stopping.'
        STOP
     end if

     !  Position and B-field at each test point

     drpt = 2.0D0 * rminor / (npts-1)
     rpt0 = rmajor - rminor

     do ido = 1,npts
        rpts(ido) = rpt0 + (ido-1)*drpt
        zpts(ido) = 0.0D0
        brin(ido) = 0.0D0
        bzin(ido) = 0.0D0
     end do

     !  Calculate currents in coils to produce the given field

     call efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts,rpts, &
          zpts,brin,bzin,nfxf,rfxf,zfxf,cfxf,ngrp,ncls,rcls,zcls, &
          alfapf,work1,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat,sigma, &
          work2,ssq0,ccl0)

  end if

  !  Simple coil current scaling for TARTs (good only for A < about 1.8)

  if (itart == 1) then

     do ido = 1,ngrp

        if (ipfloc(ido) == 1) then

           !  PF coil is stacked on top of the OH coil

           ccls(ido) = 0.0D0

           write(*,*) 'Error in routine PFCOIL :'
           write(*,*) 'IPFLOC(',ido,') should not be one if ITART=1'
           write(*,*) 'PROCESS stopping.'
           STOP

        else if (ipfloc(ido) == 2) then

           !  PF coil is on top of the TF coil

           ccls(ido) = 0.3D0 * aspect**1.6D0 * plascur

        else if (ipfloc(ido) == 3) then

           !  PF coil is radially outside the TF coil

           ccls(ido) = -0.4D0 * plascur

        else
           write(*,*) 'Error in routine PFCOIL :'
           write(*,*) 'Illegal value of IPFLOC(',ido,'), = ',ipfloc(ido)
           write(*,*) 'PROCESS stopping.'
           STOP
        end if

     end do

  else

     !  Conventional aspect ratio scaling

     do ido = 1,ngrp

        if (ipfloc(ido) == 1) then

           !  PF coil is stacked on top of the OH coil

           ccls(ido) = 0.2D0 * plascur

        else if (ipfloc(ido) == 2) then

           !  PF coil is on top of the TF coil

           ccls(ido) = 0.2D0 * plascur

        else if (ipfloc(ido) == 3) then

           !  PF coil is radially outside the TF coil

           npts0 = 1
           rpts(1) = rmajor
           zpts(1) = 0.0D0
           brin(1) = 0.0D0
           bzin(1) = -1.0D-7 * plascur/rmajor * &
                ( log(8.0D0*aspect) + betap - 1.125D0)
           nfxf0 = 0
           ngrp0 = 1
           ncls0(1) = 2
           rcls0(1,1) = rcls(ido,1)
           rcls0(1,2) = rcls(ido,2)
           zcls0(1,1) = zcls(ido,1)
           zcls0(1,2) = zcls(ido,2)

           call efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1, &
                npts0,rpts,zpts,brin,bzin,nfxf0,rfxf,zfxf,cfxf, &
                ngrp0,ncls0,rcls0,zcls0,alfapf,work1,bfix,gmat, &
                bvec,rc,zc,cc,xc,umat,vmat,sigma,work2,ssqef, &
                ccls0)
           ccls(ido) = ccls0(1)

        else
           write(*,*) 'Error in routine PFCOIL :'
           write(*,*) 'Illegal value of IPFLOC(',ido,'), = ',ipfloc(ido)
           write(*,*) 'PROCESS stopping.'
           STOP
        end if
     end do

  end if

  !  Split groups of coils into one set containing ncl coils

  ncl = 0
  do nng = 1,ngrp
     do ng2 = 1,ncls(nng)
        ncl = ncl + 1
        rpf(ncl) = rcls(nng,ng2)
        zpf(ncl) = zcls(nng,ng2)

        !  Currents at different times

        curpfb(ncl) = 1.0D-6 * ccls(nng)
        curpff(ncl) = 1.0D-6 * ccls(nng) * fcohbof
        curpfs(ncl) = 1.0D-6 * ccl0(nng)
     end do
  end do

  !  Current in OH coil as a function of time
  !  N.B. If the OH coil is not present then curstot is zero.

  curpfs(ncl+1) =  1.0D-6 * curstot * cohbop/coheof
  curpff(ncl+1) = -1.0D-6 * curstot * cohbof/coheof
  curpfb(ncl+1) = -1.0D-6 * curstot

  !  Set up coil current waveforms, normalised to the peak current in
  !  each coil

  call waveform

  !  Calculate PF coil geometry, current and number of turns

  i = 0
  do ii = 1,ngrp
     do ij = 1,ncls(ii)
        i = i + 1

        if (ipfloc(ii).eq.1) then

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

     end do
  end do

  !  Calculate peak field, allowable current density, resistive
  !  power losses and volumes and weights for each PF coil

  powpfres = 0.0D0
  i = 0

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

           !  Superconducting coil : Assume 500 MPa stress limit, 2/3 of the
           !  force is supported in the outer (steel) case

           areaspf = 0.666D0 * forcepf / 5.0D8

        else
           areaspf = 0.0D0  !  Resistive coil - no steel needed
        end if

        !  Weight of steel

        wts(i) = areaspf * 2.0D0*pi*rpf(i) * 7800.0D0
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
  !  input current wave forms

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
  !+ad_call  build.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  bfmax
  !+ad_call  peakb
  !+ad_call  pfjalw
  !+ad_hist  01/02/96 PJK Initial version
  !+ad_hist  09/05/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'pfcoil.h90'
  include 'tfcoil.h90'
  include 'build.h90'

  !  Arguments

  !  Local variables

  integer :: itt,nfxf,nohc1

  real(kind(1.0D0)) :: areaspf,aroh,bmaxoh2,bohci,bohco,bri,bro, &
       bzi,bzo,forcepf,hohc,sgn,volohc

  real(kind(1.0D0)), dimension(nfixmx) :: rfxf,zfxf,cfxf,xind
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls,zcls
  real(kind(1.0D0)), dimension(ngrpmx) :: ccls,ccls2,ccl0
  real(kind(1.0D0)), dimension(ngc2) :: bpf2

  real(kind(1.0D0)), external :: bfmax,pfjalw

  common/pfcom1/ rfxf,zfxf,cfxf,rcls,zcls,ccls,ccls2,ccl0,bpf2,xind,nfxf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Height of OH coil

  hohc = hmax * ohhghf

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

  !  Z coordinates of coil edges

  zh(nohc) = hohc
  zl(nohc) = -zh(nohc)

  !  (R,Z) coordinates of coil centre

  rpf(nohc) = 0.5D0 * (rb(nohc)+ra(nohc))
  zpf(nohc) = 0.5D0 * (zl(nohc)+zh(nohc))

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
  !+ad_call  build.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  bfield
  !+ad_hist  09/05/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'pfcoil.h90'
  include 'tfcoil.h90'
  include 'phydat.h90'
  include 'build.h90'

  !  Arguments

  integer, intent(in) :: i,ii
  integer, intent(inout) :: it
  real(kind(1.0D0)), intent(out) :: bri,bro,bzi,bzo

  !  Local variables

  integer :: iii,iohc,jj,jjj,kk,n,nfxf
  real(kind(1.0D0)) :: bpfin,bpfout,dzpf,psi,sgn

  real(kind(1.0D0)), dimension(nfixmx) :: rfxf,zfxf,cfxf,xind
  real(kind(1.0D0)), dimension(ngrpmx,nclsmx) :: rcls,zcls
  real(kind(1.0D0)), dimension(ngrpmx) :: ccls,ccls2,ccl0
  real(kind(1.0D0)), dimension(ngc2) :: bpf2

  common/pfcom1/ rfxf,zfxf,cfxf,rcls,zcls,ccls,ccls2,ccl0,bpf2,xind,nfxf

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
        write(*,*) 'Error in routine PEAKB :'
        write(*,*) 'Illegal value of it, = ',it
        write(*,*) 'Possible rounding error - change program logic...'
        write(*,*) 'PROCESS stopping.'
        STOP
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

  call bfield(nfixmx,kk,rfxf,zfxf,cfxf,xind,ra(i),zpf(i),bri,bzi,psi)
  call bfield(nfixmx,kk,rfxf,zfxf,cfxf,xind,rb(i),zpf(i),bro,bzo,psi)

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
  real(kind(1.0D0)), parameter :: rmu = 1.256637D-6

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  beta = h/a
  alpha = b/a

  !  Fits are for 1 < alpha < 2 , and 0.5 < beta < very large

  b0 = rj*rmu*h * log( (alpha + sqrt(alpha**2+beta**2)) &
       / (1.0D0 + sqrt(1.0D0 + beta**2)) )

  if (beta > 3.0D0) then

     b1 = rmu*rj*(b-a)
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
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_hist  09/05/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'pfcoil.h90'

  !  Arguments

  !  Local variables

  integer :: ic,it,nntf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialize TF coil waveform to 1.0
  !  (TF coil??? More likely OH coil!)

  nntf = nohc+1
  do it = 1,6
     waves(nntf,it) = 1.0D0
  end do

  do ic = 1,nohc

     !  Find where the peak current occurs

     if ( (abs(curpfs(ic)) > abs(curpfb(ic))) .and. &
          (abs(curpfs(ic)) > abs(curpff(ic))) ) &
          ric(ic) = curpfs(ic)

     if ( (abs(curpff(ic)) > abs(curpfb(ic))) .and. &
          (abs(curpff(ic)) > abs(curpfs(ic))) ) &
          ric(ic) = curpff(ic)

     if ( (abs(curpfb(ic)) >= abs(curpfs(ic))) .and. &
          (abs(curpfb(ic)) >  abs(curpff(ic))) ) &
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

subroutine outpf(nout)

  !+ad_name  outpf
  !+ad_summ  Routine to write output from PF coil module to file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_desc  This routine writes the PF coil information to the output file.
  !+ad_prob  None
  !+ad_call  build.h90
  !+ad_call  osections.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarre
  !+ad_hist  09/05/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'pfcoil.h90'
  include 'tfcoil.h90'
  include 'build.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  integer :: k,nef
  real(kind(1.0D0)) :: ricpf,ssq0

  common /pfout/ssq0, ricpf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (sect08 == 0) return

  call oheadr(nout,'PF Coils')

  !  Print out OH coil stress

  if (iohcl == 0) then
     call ocmmnt(nout,'No OH coil included')
     call oblnkl(nout)
  else
     call ocmmnt(nout,'OH Coil Stress Calculations :')
     call oblnkl(nout)
     call ovarre(nout,'Maximum field at End Of Flattop (T)', &
          '(bmaxoh)',bmaxoh)
     call ovarre(nout,'Maximum field at Beginning Of Pulse (T)', &
          '(bmaxoh0)',bmaxoh0)
     call ovarre(nout,'Allowable current density at EOF (A/m2)', &
          '(rjohc)',rjohc)
     call ovarre(nout,'Actual current density at EOF (A/m2)', &
          '(coheof)',coheof)
     call ovarre(nout,'Allowable current density at BOP (A/m2)', &
          '(rjohc0)',rjohc0)
     call ovarre(nout,'Actual current density at BOP (A/m2)', &
          '(cohbop)',cohbop)
     call ovarre(nout,'Allowable stress at BOP (MPa)', &
          '(sigpfalw)',sigpfalw)
  end if

  if (ipfres /= 0) then
     call osubhd(nout,'Resistive Power :')
     call ovarre(nout,'PF coil resistive power (W)','(powpfres)', &
          powpfres)
     call ovarre(nout,'OH coil resistive power (W)','(powohres)', &
          powohres)
  end if

  !  nef is the number of coils excluding the OH coil

  nef = nohc
  if (iohcl /= 0) nef = nef - 1

  call osubhd(nout, 'Geometry of PF coils, OH coil and plasma :')

  write(nout,10)
10 format(' coil',t17,'R(m)',t29,'Z(m)',t41,'dR(m)',t53,'dZ(m)', &
       t65,'turns')
  call oblnkl(nout)

  !  PF coils

  write(nout,20) (k,rpf(k),zpf(k),(rb(k)-ra(k)),abs(zh(k)-zl(k)), &
       turns(k),k=1,nef)
20 format('  PF',i1,t10,5f12.2)

  !  OH coil, if present

  if (iohcl.ne.0) then
     write(nout,30) rpf(nohc),zpf(nohc),(rb(nohc)-ra(nohc)), &
          abs(zh(nohc)-zl(nohc)),turns(nohc)
30   format('  OH',t10,5f12.2)
  end if

  !  Plasma

  write(nout,40) rmajor,0.0D0,2.0D0*rminor,2.0D0*rminor*kappa,1.0D0
40 format(' Plasma',t10,5f12.2)

  call osubhd(nout,'PF Coil Information :')

  write(nout,50)
50 format(' coil', &
       t8, 'current', &
       t17,'allowed J', &
       t28,'actual J', &
       t39,'J', &
       t43,'coil weight', &
       t56,'steel weight', &
       t71,'field')

  write(nout,60)
60 format( &
       t10,'(MA)', &
       t18,'(A/m2)', &
       t29,'(A/m2)', &
       t37,'ratio', &
       t46,'(kg)', &
       t60,'(kg)', &
       t72,'(T)')

  call oblnkl(nout)

  !  PF coils

  do k = 1,nef
     write(nout,90) k,ric(k),rjpfalw(k),rjconpf(k), &
          (rjconpf(k)/rjpfalw(k)),wtc(k),wts(k),bpf(k)
  end do

  !  The 0p syntax is needed here and on line 100
  !  to prevent a known compiler 'feature' from apparently
  !  multiplying the f-formatted numbers by 10.

90 format('  PF',i1,f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,1pe13.3)

  !  OH coil, if present

  if (iohcl /= 0) then

     write(nout,100) ric(nohc),rjpfalw(nohc),cohbop, &
          (cohbop/rjpfalw(nohc)),wtc(nohc),wts(nohc), &
          bpf(nohc)

100  format('  OH ',f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,1pe13.3)
  end if

  !  Miscellaneous totals

  write(nout,110)
110 format(t8,'------',t43,'---------',t55,'---------')

  write(nout,120) ricpf,whtpf,whtpfs
120 format(t6,f8.2,t41,1pe11.3,1pe12.3)

  call osubhd(nout,'PF coil current scaling information :')
  call ovarre(nout,'Sum of squares of residuals ','(ssq0)',ssq0)
  call ovarre(nout,'Smoothing parameter ','(alfapf)',alfapf)

end subroutine outpf
