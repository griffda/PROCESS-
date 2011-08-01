!  $Id::                                                                $
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
  !+ad_call  build.h90
  !+ad_call  pfcoil.h90
  !+ad_call  vltcom.h90
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'pfcoil.h90'
  include 'vltcom.h90'
  include 'build.h90'

  !  Arguments

  !  Local variables

  integer :: i

  !  COMMON variables

  real(kind(1.0D0)), dimension(ngc2,3) :: vsdum
  common/ind2a/ vsdum

  integer :: nef
  common/ind2b/ nef

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

subroutine outvolt(nout)

  !+ad_name  outvolt
  !+ad_summ  Writes volt-second information to output file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_desc  This routine writes the PF coil volt-second data to the
  !+ad_desc  output file.
  !+ad_prob  None
  !+ad_call  osections.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  times.h90
  !+ad_call  vltcom.h90
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  oshead
  !+ad_call  osubhd
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'pfcoil.h90'
  include 'vltcom.h90'
  include 'times.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  integer :: jj,k

  !  COMMON variables

  real(kind(1.0D0)), dimension(ngc2,3) :: vsdum
  common/ind2a/ vsdum

  integer :: nef
  common/ind2b/ nef

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (sect09 == 0) return

  call oheadr(nout,'Volt Second Consumption')

  write(nout,10) vsefsu,vsefbn,vseft,vsohsu,vsohbn,vsoh,vssu,vsbn,vstot
10 format(t15,'volt-sec',t30,'volt-sec',t45,'volt-sec'/ &
        t15,  'start-up',t32,'burn',t46,'total'// &
        t2,'PF coils :',t13,3(f10.2,5x)/ &
        t2,'OH coil  :',t13,3(f10.2,5x)/ &
        t15,8('-'),t30,8('-'),t45,8('-')/ &
        t2,'Total :   ',t13,3(f10.2,5x) )

  call osubhd(nout, &
       'Summary of volt-second consumption by circuit (Wb) :')

  write(nout,20)
20 format(' circuit', t16,'BOP',t31,'BOF',t46,'EOF')

  call oblnkl(nout)

  write(nout,30) (k,vsdum(k,1),vsdum(k,2),vsdum(k,3),k=1,nef)
30 format(t4,i3,t10,f10.3,5x,f10.3,5x,f10.3)

  write(nout,40) vsdum(nohc,1),vsdum(nohc,2),vsdum(nohc,3)
40 format(' OH coil',t10,f10.3,5x,f10.3,5x,f10.3)

  call oshead(nout,'Waveforms')
  call ocmmnt(nout,'Currents (Amps/coil) as a function of time :')
  call oblnkl(nout)

  write(nout,50)(tim(k),k=1,6)
50 format(t40,'time (sec)'//t10,6f11.2)

  call ocmmnt(nout,'circuit')

  do k = 1,ncirt-1
     write(nout,60) k,((cpt(k,jj)*turns(k)),jj=1,6)
  end do
60 format(t3,i2,t12,6(1pe11.3))

  write(nout,80) (cpt(ncirt,jj),jj=1,6)
80 format(' Plasma (A)',t12,6(1pe11.3))

end subroutine outvolt

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine induct(iprint,nout)

  !+ad_name  induct
  !+ad_summ  Calculates PF coil set mutual inductance matrix
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_args  nout : input integer : output file unit
  !+ad_desc  This routine calculates the mutual inductances between all the
  !+ad_desc  PF coils.
  !+ad_prob  None
  !+ad_call  build.h90
  !+ad_call  osections.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  vltcom.h90
  !+ad_call  bfield
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  INCLUDE 'param.h90'
  INCLUDE 'phydat.h90'
  INCLUDE 'pfcoil.h90'
  INCLUDE 'vltcom.h90'
  INCLUDE 'tfcoil.h90'
  INCLUDE 'build.h90'
  INCLUDE 'osections.h90'

  !  Arguments

  integer, intent(in) :: iprint, nout

  !  Local variables

  integer, parameter :: noh   = 8 !  Number of segments the OH coil is split into
  integer, parameter :: nplas = 1 !  Number of filaments describing the plasma

  real(kind(1.0D0)), dimension(noh) :: roh,zoh,rplasma,zplasma
  real(kind(1.0D0)), dimension(ngc2) :: rc,zc,xc,cc
  real(kind(1.0D0)) :: br,bz,delzoh,psi,rl,rp,xohpf,xohpl,xpfpl,zp
  integer :: i,ig,ii,ij,j,jj,k,nc,ncoilj,ncoils,nef

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

  if (iohcl /= 0) then
     delzoh = zh(nohc) / dble(noh/2)

     do i = 1,noh
        if (itart == 1) then
           roh(i) = bore + bcylth + tfcth + gapoh + 0.5D0*ohcth
        else
           roh(i) = bore + 0.5D0*ohcth
        end if
        zoh(i) = zh(nohc) - delzoh*(0.5D0 + dble(i-1) )
     end do
  end if

  rplasma(1) = rmajor  !  assumes nplas==1
  zplasma(1) = 0.0D0

  !  OH coil / plasma mutual inductance

  do i = 1,nplas
     rc(i) = rplasma(i)
     zc(i) = zplasma(i)
  end do

  if (iohcl /= 0) then
     nc = nplas
     xohpl = 0.0D0

     do i = 1,noh
        rp = roh(i)
        zp = zoh(i)
        call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
        do ii = 1,nplas
           xohpl = xohpl + xc(ii)
        end do
     end do

     sxlg(ncirt,nohc) = xohpl / dble(nplas*noh) * turns(nohc)
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
     rp = rpf(ncoils-1)
     zp = zpf(ncoils-1)
     call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
     do ii = 1,nplas
        xpfpl = xpfpl + xc(ii)
     end do
     do j = 1,ncls(i)
        ncoilj = ncoils + 1 - j
        sxlg(ncoilj,ncirt) = xpfpl / dble(nplas) * turns(ncoilj)
        sxlg(ncirt,ncoilj) = sxlg(ncoilj,ncirt)
     end do
  end do

  if (iohcl /= 0) then

     !  OH coil self inductance

     nc = noh
     do i = 1,noh
        rc(i) = roh(i)
        zc(i) = zoh(i)
     end do

     sxlg(nohc,nohc) = turns(nohc)**2 * rmu0/3.0D0 * &
          (rb(nohc)**3 - ra(nohc)**3) / (rb(nohc)**2 - ra(nohc)**2)

     !  OH coil / PF coil mutual inductances

     ncoils = 0
     do i = 1,ngrp
        xohpf = 0.0D0
        ncoils = ncoils + ncls(i)
        rp = rpf(ncoils)
        zp = zpf(ncoils)
        call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
        do ii = 1,noh
           xohpf = xohpf + xc(ii)
        end do
        do j = 1,ncls(i)
           ncoilj = ncoils + 1 - j
           sxlg(ncoilj,nohc) = xohpf * turns(ncoilj) * turns(nohc) &
                / dble(noh)
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
     call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
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

  !  Output section

  if ((iprint == 0).or.(sect11 == 0)) return

  call oheadr(nout,'PF Coil Inductances')
  call ocmmnt(nout,'Inductance matrix (Henries-turns**2) :')
  call oblnkl(nout)

  do ig = 1,nef
     write(nout,210) ig,(sxlg(ij,ig),ij=1,ncirt)
  end do
210 format(t3,i2,t9,20(1pe8.1))

  if (iohcl /= 0) write(nout,230) (sxlg(ij,ncirt-1),ij=1,ncirt)
230 format(' OH coil',t9,20(1pe8.1))

  write(nout,240) (sxlg(ij,ncirt),ij=1,ncirt)
240 format(' Plasma',t9,20(1pe8.1))

end subroutine induct
