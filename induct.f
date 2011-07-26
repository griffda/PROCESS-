CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C                                                                     C
C  Source Code Control System                                         C
C                                                                     S
C  Information header for the PROCESS systems code modules            C
C                                                                     C
C  P.J.Knight 22 May 1992                                             S
C                                                                     C
C                                                                     C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C
C  Module         : $Id: induct.f,v 3.3 1996/02/01 15:06:55 peter Exp $
C
C  Module name    : $RCSfile: induct.f,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1996/02/01 15:06:55 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE VSEC

c  Calculation of volt-second capability of PF system

      IMPLICIT NONE

      INCLUDE 'pfcoil.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'build.h'

      DOUBLE PRECISION vsdum(ngc2,3)
      COMMON/ind2a/    vsdum

      INTEGER       nef
      COMMON/ind2b/ nef

      INTEGER i

c  PF volt-seconds during start-up
      if (iohcl.eq.0) then
C        No OH coil
         nef = ncirt - 1
      else
         nef = ncirt - 2
      end if

      vsefsu = 0.0D0
      do 50 i=1,nef
         vsdum(i,1) = sxlg(ncirt,i) * cpt(i,2)
         vsdum(i,2) = sxlg(ncirt,i) * cpt(i,3)
         vsefsu = vsefsu + ( vsdum(i,2) - vsdum(i,1) )
 50   continue

c  OH startup volt-seconds

      if (iohcl.ne.0) then
         vsdum(nohc,1) = sxlg(ncirt,ncirt-1) * cpt(ncirt-1,2)
         vsdum(nohc,2) = sxlg(ncirt,ncirt-1) * cpt(ncirt-1,3)
         vsohsu = vsdum(nohc,2) - vsdum(nohc,1)
      end if

c  Total available volt-seconds for start-up
      vssu = vsohsu + vsefsu

c  Burn volt-seconds

      if (iohcl.ne.0) then
         vsdum(nohc,3) = sxlg(ncirt,ncirt-1) * cpt(ncirt-1,5)
         vsohbn = vsdum(nohc,3) - vsdum(nohc,2)
      end if

c  PF volt-seconds during burn

      vsefbn = 0.0D0
      do 70 i = 1,nef
         vsdum(i,3) = sxlg(ncirt,i) * cpt(i,5)
         vsefbn = vsefbn + (vsdum(i,3) - vsdum(i,2) )
 70   continue

      vsbn = vsohbn + vsefbn

      vstot = vssu + vsbn
      vseft = vsefsu + vsefbn
      vsoh = vsohbn + vsohsu

      return
      end
c_______________________________________________________________________
      SUBROUTINE OUTVOLT(nout)

C  Routine to print the volt-second information

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'times.h'
      INCLUDE 'osections.h'

      INTEGER jj,k,nout

      DOUBLE PRECISION vsdum(ngc2,3)
      COMMON/ind2a/    vsdum

      INTEGER       nef
      COMMON/ind2b/ nef

      if (sect09.eq.0) goto 1000

      call oheadr(nout,'Volt Second Consumption')

      write(nout,10) vsefsu,vsefbn,vseft,vsohsu,vsohbn,vsoh,vssu,
     +     vsbn,vstot
 10   format(t15,'volt-sec',t30,'volt-sec',t45,'volt-sec'/
     +     t15,  'start-up',t32,'burn',t46,'total'//
     +     t2,'PF coils :',t13,3(f10.2,5x)/
     +     t2,'OH coil  :',t13,3(f10.2,5x)/
     +     t15,8('-'),t30,8('-'),t45,8('-')/
     +     t2,'Total :   ',t13,3(f10.2,5x) )

      call osubhd(nout,
     +     'Summary of volt-second consumption by circuit (Wb) :')

      write(nout,20)
 20   format(' circuit', t16,'BOP',t31,'BOF',t46,'EOF')

      call oblnkl(nout)

      write(nout,30) (k,vsdum(k,1),vsdum(k,2),vsdum(k,3),k=1,nef)
 30   format(t4,i3,t10,f10.3,5x,f10.3,5x,f10.3)

      write(nout,40) vsdum(nohc,1),vsdum(nohc,2),vsdum(nohc,3)
 40   format(' OH coil',t10,f10.3,5x,f10.3,5x,f10.3)

      call oshead(nout,'Waveforms')
      call ocmmnt(nout,'Currents (Amps/coil) as a function of time :')
      call oblnkl(nout)

      write(nout,50)(tim(k),k=1,6)
 50   format(t40,'time (sec)'//t10,6f11.2)

      call ocmmnt(nout,'circuit')

      do 70 k=1,ncirt-1
         write(nout,60) k,((cpt(k,jj)*turns(k)),jj=1,6)
 60      format(t3,i2,t12,6(1pe11.3))
 70   continue

      write(nout,80) (cpt(ncirt,jj),jj=1,6)
 80   format(' Plasma (A)',t12,6(1pe11.3))

 1000 continue

      return
      end
c_______________________________________________________________________
      SUBROUTINE INDUCT(iprint,nout)

c  Subroutine to calculate mutual PF coil set inductance matrix

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'osections.h'

C  Set the number of segments that the OH coil is split into.
      INTEGER noh
      PARAMETER (noh = 8)

      DOUBLE PRECISION roh(noh),zoh(noh),rplasma(noh),zplasma(noh)
      DOUBLE PRECISION rc(ngc2),zc(ngc2),xc(ngc2),cc(ngc2)

      DOUBLE PRECISION br,bz,delzoh,psi,rl,rp,xohpf,xohpl,xpfpl,zp

      INTEGER i,ig,ii,ij,iprint,j,jj,k,nc,ncoilj,ncoils,nef,nout,nplas

      nplas = 1

      br = 0.0D0
      bz = 0.0D0
      psi = 0.0D0

      do 10 i = 1,ngc2
         rc(i) = 0.0D0
         zc(i) = 0.0D0
         xc(i) = 0.0D0
         cc(i) = 0.0D0
 10   continue

c  Break OH coil into noh segments

      do 30 i = 1,ncirt
         do 20 j = 1,ncirt
            sxlg(i,j) = 0.0D0
 20      continue
 30   continue

      if (iohcl.ne.0) then
         delzoh = zh(nohc) / dble(noh/2)

         do 40 i = 1,noh

C+**PJK 29/01/96
            if (itart.eq.1) then
               roh(i) = bore + bcylth + tfcth + gapoh + ohcth/2.0D0
            else
               roh(i) = bore + ohcth/2.0D0
            end if
            zoh(i) = zh(nohc) - delzoh*(0.5D0 + dble(i-1) )
 40      continue
      end if

      rplasma(1) = rmajor
      zplasma(1) = 0.0D0

c  OH coil / plasma mutual inductance

      do 50 i=1,nplas
         rc(i) = rplasma(i)
         zc(i) = zplasma(i)
 50   continue

      if (iohcl.ne.0) then
         nc = nplas
         xohpl = 0.0D0
         do 70 i = 1,noh
            rp = roh(i)
            zp = zoh(i)
            call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
            do 60 ii = 1,nplas
               xohpl = xohpl + xc(ii)
 60         continue
 70      continue
         sxlg(ncirt,nohc) = xohpl / dble(nplas*noh) * turns(nohc)
         sxlg(nohc,ncirt) = sxlg(ncirt,nohc)
      end if

c  Plasma self inductance

      sxlg(ncirt,ncirt) = rlp

c  PF coil / plasma mutual inductances

      ncoils = 0
      nc = nplas
      do 100 i = 1,ngrp
         xpfpl = 0.0D0
         ncoils = ncoils + ncls(i)
         rp = rpf(ncoils-1)
         zp = zpf(ncoils-1)
         call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
         do 80 ii = 1,nplas
            xpfpl = xpfpl + xc(ii)
 80      continue
         do 90 j = 1,ncls(i)
            ncoilj = ncoils + 1 - j
            sxlg(ncoilj,ncirt) = xpfpl / dble(nplas) * turns(ncoilj)
            sxlg(ncirt,ncoilj) = sxlg(ncoilj,ncirt)
 90      continue
 100  continue

c  OH coil self inductance

      if (iohcl.ne.0) then
         nc = noh
         do 110 i = 1,noh
            rc(i) = roh(i)
            zc(i) = zoh(i)
 110     continue

         sxlg(nohc,nohc) = turns(nohc)**2 * rmu0/3.0D0 *
     +        (rb(nohc)**3 - ra(nohc)**3) / (rb(nohc)**2 - ra(nohc)**2)

c  OH coil / PF coil mutual inductances

         ncoils = 0
         do 140 i = 1,ngrp
            xohpf = 0.0D0
            ncoils = ncoils + ncls(i)
            rp = rpf(ncoils)
            zp = zpf(ncoils)
            call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
            do 120 ii = 1,noh
               xohpf = xohpf + xc(ii)
 120        continue
            do 130 j = 1,ncls(i)
               ncoilj = ncoils + 1 - j
               sxlg(ncoilj,nohc) = xohpf * turns(ncoilj) * turns(nohc)
     +              / dble(noh)
               sxlg(nohc,ncoilj) = sxlg(ncoilj,nohc)
 130        continue
 140     continue
      end if

c  PF coil - PF coil inductances

      if (iohcl.eq.0) then
         nef = nohc
      else
         nef = nohc-1
      end if

      nc = nef-1
      do 180 i = 1,nef
         do 150 j=1,nef-1
            if (j.ge.i) then
               jj = j+1
            else
               jj = j
            end if
            zc(j) = zpf(jj)
            rc(j) = rpf(jj)
 150     continue
         rp = rpf(i)
         zp = zpf(i)
         call bfield(ngc2,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
         do 170 k = 1,nef
            if (k.lt.i)  then
               sxlg(i,k) = xc(k) * turns(k) * turns(i)
               goto 160
            end if
            if (k.eq.i)  then
               rl = abs(zh(k) - zl(k))/sqrt(pi)
               sxlg(k,k) = rmu0 * turns(k)**2 * rpf(k) *
     +              (log(8.0D0*rpf(k)/rl) - 1.75D0)
               goto 160
            end if
            sxlg(i,k) = xc(k-1) * turns(k) * turns(i)
 160        continue
 170     continue
 180  continue

c  Output section

      if ((iprint.eq.0).or.(sect11.eq.0)) goto 1000

      call oheadr(nout,'PF Coil Inductances')
      call ocmmnt(nout,'Inductance matrix (Henries-turns**2) :')
      call oblnkl(nout)

      do 220 ig = 1,nef
         write(nout,210) ig,(sxlg(ij,ig),ij=1,ncirt)
 210     format(t3,i2,t9,20(1pe8.1))
 220  continue

      if (iohcl.ne.0) write(nout,230) (sxlg(ij,ncirt-1),ij=1,ncirt)
 230  format(' OH coil',t9,20(1pe8.1))

      write(nout,240) (sxlg(ij,ncirt),ij=1,ncirt)
 240  format(' Plasma',t9,20(1pe8.1))

 1000 continue

      return
      end
