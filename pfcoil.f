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
C  Module         : $Id$
C
C  Module name    : $RCSfile: pfcoil.f,v $
C  Version no.    : $Revision: 3.6 $
C
C  Creation date  : $Date: 1996/02/01 15:07:26 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C______________________________________________________________________
      SUBROUTINE PFCOIL

C  Routine to perform calculations for the PF and OH coils, to determine
C  their size, location, current waveforms, stresses etc.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'times.h'

C  ngrpmx is the maximum number of PF coil groups
C  nclsmx is the maximum number of coils in one group
C  nptsmx is the maximum number of points across the plasma midplane
C         at which the magnetic field is fixed
C  nfixmx is the maximum number of fixed current coils

      INTEGER lrow1,lcol1
      PARAMETER (lrow1 = 2*nptsmx + ngrpmx, lcol1 = ngrpmx)

      INTEGER ncls0(ngrpmx+2)

      DOUBLE PRECISION
     +     rcls(ngrpmx,nclsmx), zcls(ngrpmx,nclsmx),
     +     rcls0(ngrpmx,nclsmx), zcls0(ngrpmx,nclsmx)

      DOUBLE PRECISION
     +     ccls0(ngrpmx/2), ccls(ngrpmx), ccls2(ngrpmx), ccl0(ngrpmx),
     +     sigma(ngrpmx), work2(ngrpmx)

      DOUBLE PRECISION
     +     rc(nclsmx), zc(nclsmx), cc(nclsmx), xc(nclsmx)

      DOUBLE PRECISION
     +     brin(nptsmx), bzin(nptsmx), rpts(nptsmx), zpts(nptsmx)

      DOUBLE PRECISION
     +     rfxf(nfixmx), zfxf(nfixmx), cfxf(nfixmx),
     +     xind(nfixmx), work1(nfixmx)

      DOUBLE PRECISION
     +     bfix(lrow1), bvec(lrow1),
     +     gmat(lrow1,lcol1), umat(lrow1,lcol1), vmat(lrow1,lcol1)

      INTEGER i,ido,ii,iii,ij,it,j,k,ncl,nfxf,nfxf0,ng2,ngrp0,
     +     nng,npts,npts0

      DOUBLE PRECISION signn(2), bpf2(ngc2)

      DOUBLE PRECISION area,areaspf,bri,bro,bzi,bzo,curstot,drpt,
     +     dx,dz,forcepf,respf,ricpf,rll,rpt0,ssq0,ssqef,volpf

      DOUBLE PRECISION pfjalw
      EXTERNAL         pfjalw

      common/pfout/ssq0, ricpf
      common/pfcom1/ rfxf,zfxf,cfxf,rcls,zcls,
     +     ccls,ccls2,ccl0,bpf2,xind,nfxf

C  Set up the number of PF coils including the OH coil (nohc),
C  and the number of PF circuits including the plasma (ncirt)

      nohc = 0

      if (ngrp.gt.ngrpmx) then
         write(*,*) 'Error in routine PFCOIL:'
         write(*,*) 'ngrp is larger than ngrpmx = ',ngrpmx
         write(*,*) 'Reduce the value of ngrp.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C  Total the number of PF coils in all groups, and check that none
C  exceeds the limit
      do 10 i = 1,ngrp
         if (ncls(i).gt.nclsmx) then
            write(*,*) 'Error in routine PFCOIL:'
            write(*,*) 'PF coil group ',i,' contains ',ncls(i),' coils.'
            write(*,*) 'Maximum number allowed is ',nclsmx
            write(*,*) 'PROCESS stopping.'
            STOP
         end if
         nohc = nohc + ncls(i)
 10   continue

C  Add one if an OH coil is present, and make an extra group
      if (iohcl.ne.0) then 
         nohc = nohc + 1
         ncls(ngrp+1) = 1
      end if

C  Add one for the plasma
      ncirt = nohc + 1

C  Currents in the OH coil at various times
      cohbop = coheof * fcohbop
      cohbof = coheof * fcohbof

C  Zero the weight and current totals
      whtpf = 0.0D0
      whtpfs = 0.0D0
      ricpf = 0.0D0

C  Set up array of times
      tim(1) = 0.0D0
      tim(2) = tramp
      tim(3) = tim(2) + tohs
      tim(4) = tim(3) + theat
      tim(5) = tim(4) + tburn
      tim(6) = tim(5) + tqnch

C  Implementation to set up call to MHD
C  scaling routine for coil currents.
C  First break up OH solenoid into 'filaments'

C  OH coil radius
C+**PJK 29/01/96
      if (itart.eq.1) then
         rohc = bore + bcylth + tfcth + gapoh + 0.5D0*ohcth
      else
         rohc = bore + 0.5D0*ohcth
      end if

C  nfxf is the total no of filaments into which the OH coil is split,
C  if present
      if (iohcl.eq.0) then
         nfxf = 0
         curstot = 0.0D0
      else
         nfxf = 2*nfxfh
         curstot = -hmax*ohhghf*ohcth*2.0D0*coheof

         if (nfxf.gt.nfixmx) then
            write(*,*) 'Error in routine PFCOIL:'
            write(*,*) 'nfxf is larger than nfixmx. nfxf = ',nfxf
            write(*,*) 'Reduce the value of nfxfh.'
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Symmetric up/down OH coil : Find (R,Z) and current of each filament
         do 20 nng = 1,nfxfh
            rfxf(nng) = rohc
            zfxf(nng) = hmax*ohhghf/DBLE(nfxfh)*(DBLE(nng)-0.5D0)
            zfxf(nng+nfxfh) = -zfxf(nng)
            rfxf(nng+nfxfh) = rfxf(nng)
            cfxf(nng) = -curstot/DBLE(nfxf) * fcohbop
            cfxf(nng+nfxfh) = cfxf(nng)
 20      continue
      end if

C  Scale PF coil locations

      signn(1) =  1.0D0
      signn(2) = -1.0D0

C+**PJK 15/10/92 Problems here if k=ncls(group) is greater than 2.

      do 60 j=1,ngrp

         if (ipfloc(j) .eq. 1) then

C  PF coil is stacked on top of the OH coil

            do 30 k = 1,ncls(j)
               rcls(j,k) = rohc + rpf1

C+**PJK 14/04/94 Z coordinate of coil amended so that it does not
C+**PJK 14/04/94 occupy the same space as the OH coil
C+**PJK 14/04/94 Original coding:
C+**PJK 14/04/94  zcls(j,k) = (hmax*ohhghf+0.3D0) * signn(k) 

               zcls(j,k) = signn(k) * ( hmax*ohhghf + 0.1D0 +
     +              0.5D0 * ( hmax*(1.0D0-ohhghf) + tfcth + 0.1D0) )

 30         continue

         else if (ipfloc(j) .eq. 2) then

C  PF coil is on top of the TF coil

            do 40 k = 1,ncls(j)
               rcls(j,k) = rmajor + rpf2*triang*rminor
C+**PJK 29/01/96
               if (itart.eq.1) then
                  zcls(j,k) = (hmax-zref(j)) * signn(k)
               else
                  zcls(j,k) = (hmax + tfcth + 0.86D0) * signn(k)
               end if
 40         continue

         else if (ipfloc(j) .eq. 3) then

C  PF coil is radially outside the TF coil

            do 50 k = 1,ncls(j)

C+**PJK 11/03/93 Changed tfcth to tfthko

               rcls(j,k) = rtot + tfthko/2.0D0 + routr
               zcls(j,k) = rminor * zref(j) * signn(k)
 50         continue

         else
            WRITE(*,*) 'Error in routine PFCOIL :'
            WRITE(*,*) 'Illegal value of IPFLOC(',j,'), = ',ipfloc(j)
            WRITE(*,*) 'PROCESS stopping.'
            STOP

         end if

 60   continue

      if (cohbop .ne. 0.0D0) then

C  Find currents for plasma initiation to null field across plasma

C  Number of test points across plasma midplane
         npts = 32

         if (npts.gt.nptsmx) then
            write(*,*) 'Error in routine PFCOIL:'
            write(*,*) 'npts is larger than nptsmx = ',nptsmx
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Position and B-field at each test point
         drpt = 2.0D0 * rminor / (dble(npts-1))
         rpt0 = rmajor - rminor

         do 70 ido = 1,npts
            rpts(ido) = rpt0 + dble(ido-1) * drpt
            zpts(ido) = 0.0D0
            brin(ido) = 0.0D0
            bzin(ido) = 0.0D0
 70      continue

C  Calculate currents in coils to produce the given field
         call efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,npts,rpts,
     +        zpts,brin,bzin,nfxf,rfxf,zfxf,cfxf,ngrp,ncls,rcls,zcls,
     +        alfapf,work1,bfix,gmat,bvec,rc,zc,cc,xc,umat,vmat,sigma,
     +        work2,ssq0,ccl0)
      end if

C+**PJK 29/01/96
C *** Simple coil current scaling for TARTs
C *** (Good only for A < about 1.8)

      if (itart.eq.1) then

         do 80 ido = 1,ngrp

            if (ipfloc(ido) .eq.1) then

c  PF coil is stacked on top of the OH coil

               ccls(ido) = 0.0D0

               WRITE(*,*) 'Error in routine PFCOIL :'
               WRITE(*,*) 'IPFLOC(',ido,') should not be one if ITART=1'
               WRITE(*,*) 'PROCESS stopping.'
               STOP

            else if (ipfloc(ido) .eq.2) then

c  PF coil is on top of the TF coil

               ccls(ido) = 0.3D0 * aspect**1.6D0 * plascur

            else if (ipfloc(ido) .eq.3) then

c  PF coil is radially outside the TF coil

               ccls(ido) = -0.4D0 * plascur

            else
               WRITE(*,*) 'Error in routine PFCOIL :'
               WRITE(*,*) 'Illegal value of IPFLOC(',ido,'), = ',
     +              ipfloc(ido)
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            end if

 80      continue

      else

C *** Conventional aspect ratio scaling

         do 90 ido = 1,ngrp

            if (ipfloc(ido) .eq. 1) then

C  PF coil is stacked on top of the OH coil

               ccls(ido) = 0.2D0 * plascur

            else if (ipfloc(ido) .eq.2) then

C  PF coil is on top of the TF coil

               ccls(ido) = 0.2D0 * plascur

            else if (ipfloc(ido) .eq. 3) then

C  PF coil is radially outside the TF coil

               npts0 = 1
               rpts(1) = rmajor
               zpts(1) = 0.0D0
               brin(1) = 0.0D0
               bzin(1) = -1.0D-7 * plascur/rmajor *
     +              ( log(8.0D0*aspect) + betap - 1.125D0)
               nfxf0 = 0
               ngrp0 = 1
               ncls0(1) = 2
               rcls0(1,1) = rcls(ido,1)
               rcls0(1,2) = rcls(ido,2)
               zcls0(1,1) = zcls(ido,1)
               zcls0(1,2) = zcls(ido,2)

               call efc(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,
     +              npts0,rpts,zpts,brin,bzin,nfxf0,rfxf,zfxf,cfxf,
     +              ngrp0,ncls0,rcls0,zcls0,alfapf,work1,bfix,gmat,
     +              bvec,rc,zc,cc,xc,umat,vmat,sigma,work2,ssqef,
     +              ccls0)
               ccls(ido) = ccls0(1)

            else
               WRITE(*,*) 'Error in routine PFCOIL :'
               WRITE(*,*) 'Illegal value of IPFLOC(',ido,'), = ',
     +              ipfloc(ido)
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            end if
 90      continue

      end if

C  Split groups of coils into one set containing ncl coils

      ncl = 0
      do 110 nng = 1,ngrp
         do 100 ng2 = 1,ncls(nng)
            ncl = ncl + 1
            rpf(ncl) = rcls(nng,ng2)
            zpf(ncl) = zcls(nng,ng2)

C  Currents at different times
            curpfb(ncl) = 1.0D-6 * ccls(nng)
            curpff(ncl) = 1.0D-6 * ccls(nng) * fcohbof
            curpfs(ncl) = 1.0D-6 * ccl0(nng)
 100     continue
 110  continue

C  Current in OH coil as a function of time
C  N.B. If the OH coil is not present then curstot is zero.

      curpfs(ncl+1) =  1.0D-6 * curstot * cohbop/coheof
      curpff(ncl+1) = -1.0D-6 * curstot * cohbof/coheof
      curpfb(ncl+1) = -1.0D-6 * curstot

C  Set up coil current waveforms, normalised to the peak current in
C  each coil

      call waveform

C  Calculate PF coil geometry, current and number of turns

      i = 0
      do 130 ii = 1,ngrp
         do 120 ij = 1,ncls(ii)
            i = i + 1

            if (ipfloc(ii).eq.1) then

C  PF coil is stacked on top of the OH coil

C+**PJK 11/03/93 Highly improbable that dx,dz calculations are correct
C+**PJK 11/03/93 for PF coil cross-section, ipfloc=1

C+**PJK 14/04/94 Radial cross-section dx halved here so that PF coil
C+**PJK 14/04/94 has same radial thickness as OH coil

               dx = 0.5D0 * ohcth
               dz = 0.5D0 * (hmax*(1.0D0-ohhghf) + tfcth + 0.1D0)
               area = 4.0D0 * dx * dz

C  Actual current density
               rjconpf(i) = 1.0D6*abs(ric(i))/area

C  Location of edges of each coil:
C  ra = inner radius
C  rb = outer radius
C  zl = 'lower' edge z (i.e. edge nearer to midplane)
C  zh = 'upper' edge z (i.e. edge further from midplane)

               ra(i) = rpf(i) - dx
               rb(i) = rpf(i) + dx

               zl(i) = zpf(i) - dz
               if (zpf(i).lt.0.0D0) zl(i) = zpf(i) + dz

               zh(i) = zpf(i) + dz
               if (zpf(i).lt.0.0D0) zh(i) = zpf(i) - dz

            else

C  Other coils. N.B. Current density RJCONPF(I) is defined in
C  routine INITIAL for these coils.

               area = abs(ric(i)*1.0D6/rjconpf(i))
               dx = sqrt(area)/2.0D0

               ra(i) = rpf(i) - dx
               rb(i) = rpf(i) + dx

               zl(i) = zpf(i) - dx
               if (zpf(i).lt.0.0D0) zl(i) = zpf(i) + dx

               zh(i) = zpf(i) + dx
               if (zpf(i).lt.0.0D0) zh(i) = zpf(i) - dx

            end if

C  Calculate number of turns
C  CPTDIN(I) is the current per turn, as defined in routine INITIAL.

            turns(i) = abs( (ric(i)*1.0D6)/cptdin(i) )

 120     continue
 130  continue

C  Calculate peak field, allowable current density, resistive
C  power losses and volumes and weights for each PF coil

      powpfres = 0.0D0
      i = 0
      do 150 ii = 1,ngrp
         iii = ii
         do 140 ij = 1,ncls(ii)
            i = i + 1

C  Peak field
            if (ij.eq.1) call peakb(i,iii,it,bri,bro,bzi,bzo)

C  Allowable current density
            rjpfalw(i) = pfjalw(bpf(i),bpf2(i),ra(i),rb(i),sigpfalw)

C  Length of conductor
            rll = 2.0D0*pi*rpf(i)

C  Resistive coils
            if (ipfres .eq. 1) then
               area = abs(ric(i)*1.0D6/rjconpf(i))

C  Coil resistance (vf is the void fraction)
               respf = pfclres * rll / ( area * (1.0D0 - vf(i)) )

C  Sum resistive power losses
               powpfres = powpfres + respf * (1.0D6 * curpfb(i))**2
            end if

C  Coil volume
            volpf = abs(ric(i)*1.0D6/rjconpf(i)) * rll

C  Coil weight (vf is the void fraction, conductor density is 8990.0)
            wtc(i) = volpf * 8990.0D0 * (1.0D0-vf(i))

C  (J x B) force on coil
            forcepf = 0.5D6 * (bpf(i) + bpf2(i)) * abs(ric(i)) * rpf(i)

C  Stress ==> cross-sectional area of supporting steel to use

            if (ipfres. ne. 1) then

C  Superconducting coil : Assume 500 MPa stress limit, 2/3 of the
C  force is supported in the outer (steel) case
               areaspf = 0.666D0 * forcepf / 5.0D8
            else

C  Resistive coil - no steel needed
               areaspf = 0.0D0

            end if

C  Weight of steel
            wts(i) = areaspf * 2.0D0*pi*rpf(i) * 7800.0D0

 140     continue
 150  continue

C  Find OH coil information

      if (iohcl.ne.0) call ohcalc

C  Summation of weights and current
C+**PJK 16/10/92 (why current?)

      do 160 i = 1,nohc
         whtpf = whtpf+wtc(i)
         whtpfs = whtpfs+wts(i)
         ricpf = ricpf+abs(ric(i))
 160  continue

C  Plasma size and shape

      zh(nohc+1) = rminor*kappa
      zl(nohc+1) = -rminor*kappa
      ra(nohc+1) = rmajor-rminor
      rb(nohc+1) = rmajor+rminor
      turns(nohc+1) = 1.0D0

C  Generate coil currents as a function of time (point k) using
C  input current wave forms

      do 180 i=1,ncirt-1
         do 170 k=1,6
            cpt(i,k) = waves(i,k) * sign(cptdin(i), ric(i) )
 170     continue
 180  continue

C  Plasma wave form

      cpt(ncirt,1) = 0.0D0
      cpt(ncirt,2) = 0.0D0
      cpt(ncirt,3) = plascur
      cpt(ncirt,4) = plascur
      cpt(ncirt,5) = plascur
      cpt(ncirt,6) = 0.0D0

      return
      end
C______________________________________________________________________
      SUBROUTINE OHCALC

C  Calculate information for the OH solenoid.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'

      INTEGER itt,nfxf,nohc1

      DOUBLE PRECISION areaspf,aroh,bmaxoh2,bohci,bohco,bri,bro,
     +     bzi,bzo,forcepf,hohc,sgn,volohc

      DOUBLE PRECISION
     +     rfxf(nfixmx), zfxf(nfixmx), cfxf(nfixmx),
     +     rcls(ngrpmx,nclsmx), zcls(ngrpmx,nclsmx),
     +     ccls(ngrpmx), ccls2(ngrpmx), ccl0(ngrpmx),
     +     bpf2(ngc2), xind(nfixmx)

      DOUBLE PRECISION bfmax,pfjalw
      EXTERNAL         bfmax,pfjalw

      common/pfcom1/ rfxf,zfxf,cfxf,rcls,zcls,
     +     ccls,ccls2,ccl0,bpf2,xind,nfxf

C  Height of OH coil
      hohc = hmax * ohhghf

C  Radius of outer edge
      rb(nohc) = rohc + ohcth/2.0D0

C  Radius of inner edge
      ra(nohc) = rb(nohc) - ohcth

C  Cross-sectional area
      aroh = 2.0D0 * hohc * ohcth

C  Peak field due to OH coil
      bmaxoh2 = bfmax(coheof,ra(nohc),rb(nohc),hohc)

C  Peak field due to all other coils as well
      nohc1 = nohc
      itt = 5
      call peakb(nohc1,99,itt,bri,bro,bzi,bzo)

C  Peak field at the End-Of-Flattop (EOF)
      bmaxoh = sqrt ( (-bmaxoh2+bzi)**2 )
      bohci = bmaxoh

C  Peak field on outboard side of OH coil
      bohco = abs(bzo)

C  Allowable current density
      rjohc = pfjalw(bohci,bohco,ra(nohc),rb(nohc),sigpfalw)

C  Peak field at the Beginning-Of-Pulse (BOP) (see above)
      bmaxoh0 = bfmax(cohbop,ra(nohc),rb(nohc),hohc)
      itt = 2
      call peakb(nohc1,99,itt,bri,bro,bzi,bzo)
      bmaxoh0 = sqrt( (bmaxoh0 + bzi)**2 )

C+**PJK 23/05/94 Added ABS
      rjpfalw(nohc) = pfjalw(bmaxoh0,abs(bzo),ra(nohc),rb(nohc),
     +     sigpfalw)
      rjohc0 = rjpfalw(nohc)

C+**PJK 19/05/94 Peak field values
      bpf(nohc) = max(bmaxoh,abs(bmaxoh0))
      bpf2(nohc) = max(bohco,abs(bzo))

C  Z coordinates of coil edges
      zh(nohc) = hohc
      zl(nohc) = -zh(nohc)

C  (R,Z) coordinates of coil centre
      rpf(nohc) = (rb(nohc)+ra(nohc))/2.0D0
      zpf(nohc) = (zl(nohc)+zh(nohc))/2.0D0

C  Current in OH coil at BOP and EOF
      if (cohbop.gt.coheof) then
         sgn = 1.0D0
         ric(nohc) = sgn * 1.0D-6*cohbop*(zh(nohc)-zl(nohc))*ohcth
      else
         sgn = -1.0D0
         ric(nohc) = sgn * 1.0D-6*coheof*(zh(nohc)-zl(nohc))*ohcth
      end if

C  Volume of OH coil
      volohc = (zh(nohc)-zl(nohc)) * pi * (rb(nohc)**2-ra(nohc)**2)

C  Number of turns
      turns(nohc) = 1.0D6 * abs(ric(nohc) )/cptdin(nohc)

C  Void fraction
      vf(nohc) = vfohc

C  Weight of OH coil
      wtc(nohc) = volohc * 8990.0D0 * (1.0D0-vfohc)

C  (J x B) force on OH coil
      forcepf = 0.5D6 * (bpf(nohc)+bpf2(nohc))*abs(ric(nohc))*rpf(nohc)

C  Stress ==> cross-sectional area of supporting steel to use

      if (ipfres .eq. 0) then

C  Superconducting coil : Assume 500 MPa stress limit, 2/3 of the
C  force is supported in the outer (steel) case
         areaspf = 0.666D0 * forcepf / 5.0D8
      else

C  Resistive OH coil - no steel needed
         areaspf = 0.0D0

      end if

C  Weight of steel
      wts(nohc) = areaspf * 2.0D0*pi*rpf(nohc) * 7800.0D0

C  Resistive power losses (non-superconducting coil)
      if ( ipfres .ne. 0) then
         powohres = 2.0D0 * pi * rohc * pfclres /
     +        (aroh * (1.0D0 - vfohc)) * (1.0D6*ric(nohc) )**2
         powpfres = powpfres + powohres
      end if

      return
      end
C______________________________________________________________________
      DOUBLE PRECISION FUNCTION PFJALW(bi,bo,ri,ro,sigalw)

C  Allowable current density (J. Miller's Quick method)
C
C  y1      - temperature margin (K)
C  y2      - critical temperature at zero field (K)
C  y3      - critical field at zero temperature (T)
C  y4      - bulk He temperature (K)
C  sigalw  - allowable stress (tangential) (MPa)

      IMPLICIT NONE

      DOUBLE PRECISION bi,bo,r2fact,rfact,ri,ro,sigalw,xbig,y1,y2,
     +     y3,y4,yy,zz

      xbig = 32.0D0
      y1 = 1.0D0
      y2 = 16.0D0
      y3 = 23.0D0
      y4 = 5.0D0

      rfact = ro/ri
      r2fact = (-0.053D0 * rfact**2 + 0.925D0*rfact + 0.1D0 )
      yy = 1.0D0 - (y1/(y2*(1.0D0 - bi/y3)-y4))
      yy = max(yy, 0.001D0)
      zz = 0.036D0 * sqrt(bi)/(1.0D0-bi/23.0D0)**2

      pfjalw = 1.45D8/( xbig/sigalw * r2fact * (bi+bo)*(ri+ro) +
     +     1.0D0 + 0.6D0/yy * zz )

      return
      end
C______________________________________________________________________
      SUBROUTINE PEAKB(i,ii,it,bri,bro,bzi,bzo)

C  Find the peak field of coil i, group ii, at time it.
C  br = radial component, bz = axial component.
C  'i' is at the inner surface, 'o' is at the outer surface.
C
C  This calculation includes the effects from all
C  the coils + the plasma

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'

      DOUBLE PRECISION bpfin,bpfout,bri,bro,bzi,bzo,dzpf,psi,sgn

      DOUBLE PRECISION
     +     rfxf(nfixmx), zfxf(nfixmx), cfxf(nfixmx),
     +     rcls(ngrpmx,nclsmx), zcls(ngrpmx,nclsmx),
     +     ccls(ngrpmx), ccls2(ngrpmx), ccl0(ngrpmx),
     +     bpf2(ngc2), xind(nfixmx)

      INTEGER i,ii,iii,iohc,it,jj,jjj,kk,n,nfxf

      common/pfcom1/ rfxf,zfxf,cfxf,rcls,zcls,
     +     ccls,ccls2,ccl0,bpf2,xind,nfxf

C  OH coil
      if ((iohcl.ne.0).and.(i.eq.nohc)) then
         kk = 0
         goto 20
      end if

C  Check different times for maximum current
      if (ABS(curpfs(i)-ric(i)).LT.1.0D-12) it = 2
      if (ABS(curpff(i)-ric(i)).LT.1.0D-12) it = 4
      if (ABS(curpfb(i)-ric(i)).LT.1.0D-12) it = 5
C+PJK 26/09/11 corrected it.ne.3 to it.ne.4 in following line...
      if ((it.ne.2).AND.(it.ne.4).AND.(it.ne.5)) then
         WRITE(*,*) 'Error in routine PEAKB :'
         WRITE(*,*) 'Illegal value of it, = ',it
         WRITE(*,*) 'Possible rounding error - change program logic...'
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

      if (iohcl.eq.0) then
C  No OH coil
         kk = 0
      else
         if (cohbop.gt.coheof) then
            sgn = 1.0D0
         else
            sgn = -1.0D0
         end if

C  Current in each filament representing part of the OH coil
         do 10 iohc = 1,nfxf
            cfxf(iohc) = waves(nohc,it)*coheof*sgn*ohcth*ohhghf*hmax/
     +           DBLE(nfxf) * 2.0D0
 10      continue

         kk = nfxf
      end if

C  Loop over all coils

 20   continue
      jj = 0
      do 40 iii = 1,ngrp
         do 30 jjj = 1,ncls(iii)
            jj = jj+1

C  Radius, z-coordinate and current for each coil

C  Self field from coil (Lyle's Method)
            if (iii.eq.ii) then
               kk = kk + 1
               dzpf = zh(jj) - zl(jj)
               rfxf(kk) = rpf(jj)
               zfxf(kk) = zpf(jj) + dzpf/8.0D0
               cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
               kk = kk + 1
               rfxf(kk) = rpf(jj)
               zfxf(kk) = zpf(jj) + dzpf * 0.375D0
               cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
               kk = kk + 1
               rfxf(kk) = rpf(jj)
               zfxf(kk) = zpf(jj) - dzpf/8.0D0
               cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
               kk = kk + 1
               rfxf(kk) = rpf(jj)
               zfxf(kk) = zpf(jj) - dzpf * 0.375D0
               cfxf(kk) = ric(jj)*waves(jj,it) * 0.25D6
            else

C  Field from different coil
               kk = kk + 1
               rfxf(kk) = rpf(jj)
               zfxf(kk) = zpf(jj)
               cfxf(kk) = ric(jj)*waves(jj,it)*1.0D6
            end if
 30      continue
 40   continue

C  Plasma effect
      if (it.gt.2) then
         kk = kk + 1
         rfxf(kk) = rmajor
         zfxf(kk) = 0.0D0
         cfxf(kk) = plascur
      end if

      call bfield(nfixmx,kk,rfxf,zfxf,cfxf,xind,ra(i),zpf(i),
     +     bri,bzi,psi)

      call bfield(nfixmx,kk,rfxf,zfxf,cfxf,xind,rb(i),zpf(i),
     +     bro,bzo,psi)

C  Peak field at OH coil is dealt with in BFMAX
      if ((iohcl.ne.0).and.(i.eq.nohc)) goto 1000

      bpfin = sqrt(bri**2+bzi**2)
      bpfout = sqrt(bro**2+bzo**2)
      do 50 n = 1,ncls(ii)
         bpf(i-1+n) = bpfin
         bpf2(i-1+n) = bpfout
 50   continue

 1000 continue

      return
      end
C______________________________________________________________________
      DOUBLE PRECISION FUNCTION BFMAX(rj,a,b,h)

C  Subroutine to calculate the maximum field of a solenoid
C
C  rj    - overall current density (A/m2)
C  a     - solenoid inner radius (m)
C  b     - solenoid outer radius (m)
C  h     - solenoid half height (m)
C  bfmax - peak field at solenoid inner radius (T)
C
C  These fits are taken from the fig. on p.22 of M. Wilson's book
C  Superconducting Magnets, Clarendon Press, Oxford, N.Y., 1983

      IMPLICIT NONE

      double precision a,alpha,b,b0,b1,beta,f,h,rat,rj,rmu

      rmu = 1.256637D-6

      beta = h/a
      alpha = b/a

C  Presently, fits are for 1 < alpha < 2 , and 0.5 < beta < very large

      b0 = rj*rmu*h*log( (alpha + sqrt(alpha**2+beta**2)) /
     +     (1.0D0 +sqrt(1.0D0 + beta**2)) )

      if (beta.gt.3.0D0) then
         b1 = rmu*rj*(b-a)
         f = (3.0D0/beta)**2
         bfmax = f*b0*(1.007D0 + (alpha-1.0D0)*0.0055D0) +
     +        (1.0D0-f)*b1
         goto 1000
      end if

      if (beta.gt.2.0D0) then
         rat = (1.025D0 - (beta-2.0D0)*0.018D0) + (alpha-1.0D0) *
     +        (0.01D0 - (beta-2.0D0)*0.0045D0)
         bfmax = rat*b0
         goto 1000
      end if

      if (beta.gt.1.0D0) then
         rat = (1.117D0 - (beta-1.0D0)*0.092D0) + (alpha-1.0D0) *
     +        (beta-1.0D0)*0.01D0
         bfmax = rat*b0
         goto 1000
      end if

      if (beta.gt.0.75D0) then
         rat = (1.30D0 - 0.732D0 * (beta-0.75D0)) + (alpha-1.0D0) *
     +        (0.2D0*(beta-0.75D0) - 0.05D0)
         bfmax = rat * b0
         goto 1000
      end if

      rat = (1.65D0 - 1.4D0*(beta-0.5D0)) + (alpha - 1.0D0) *
     +     (0.6D0*(beta-0.5D0) - 0.20D0)
      bfmax = rat * b0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE WAVEFORM

C  Set up the PF coil current waveforms. waves(i,j) is the
C  current in coil i, at time j, normalized to the peak
C  current in that coil at any time.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'

      INTEGER ic,it,nntf

C  Initialize TF coil waveform to 1.0
      nntf = nohc+1
      do 10 it = 1,6
         waves(nntf,it) = 1.0D0
 10   continue

      do 20 ic = 1,nohc

C  Find where the peak current occurs

         if ( (abs(curpfs(ic)).gt.abs(curpfb(ic))) .and.
     +        (abs(curpfs(ic)).gt.abs(curpff(ic))) )
     +        ric(ic) = curpfs(ic)

         if ( (abs(curpff(ic)).gt.abs(curpfb(ic))) .and.
     +        (abs(curpff(ic)).gt.abs(curpfs(ic))) )
     +        ric(ic) = curpff(ic)

         if ( (abs(curpfb(ic)).ge.abs(curpfs(ic))) .and.
     +        (abs(curpfb(ic)).gt.abs(curpff(ic))) )
     +        ric(ic) = curpfb(ic)

C  Set normalized current waveforms

         waves(ic,1) = 0.0D0
         waves(ic,2) = curpfs(ic)/ric(ic)
         waves(ic,3) = curpff(ic)/ric(ic)
         waves(ic,4) = curpff(ic)/ric(ic)
         waves(ic,5) = curpfb(ic)/ric(ic)
         waves(ic,6) = 0.0D0

 20   continue

      return
      end
C______________________________________________________________________
      SUBROUTINE OUTPF(nout)

C  PF coil output

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'osections.h'

      INTEGER k,nef,nout

      DOUBLE PRECISION ricpf,ssq0

      COMMON /pfout/ssq0, ricpf

      if (sect08.eq.0) goto 1000

      call oheadr(nout,'PF Coils')

C  Print out PF coil stress

      if (iohcl.eq.0) then
         call ocmmnt(nout,'No OH coil included')
         call oblnkl(nout)
      else
         call ocmmnt(nout,'OH Coil Stress Calculations :')
         call oblnkl(nout)
         call ovarre(nout,'Maximum field at End Of Flattop (T)'
     +        ,'(bmaxoh)',bmaxoh)
         call ovarre(nout,'Maximum field at Beginning Of Pulse (T)'
     +        ,'(bmaxoh0)',bmaxoh0)
         call ovarre(nout,'Allowable current density at EOF (A/m2)'
     +        ,'(rjohc)',rjohc)
         call ovarre(nout,'Actual current density at EOF (A/m2)'
     +        ,'(coheof)',coheof)
         call ovarre(nout,'Allowable current density at BOP (A/m2)'
     +        ,'(rjohc0)',rjohc0)
         call ovarre(nout,'Actual current density at BOP (A/m2)'
     +        ,'(cohbop)',cohbop)
         call ovarre(nout,'Allowable stress at BOP (MPa)','(sigpfalw)'
     +        ,sigpfalw)
      end if

      if (ipfres.ne.0) then
         call osubhd(nout,'Resistive Power :')
         call ovarre(nout,'PF coil resistive power (W)','(powpfres)'
     +        ,powpfres)
         call ovarre(nout,'OH coil resistive power (W)','(powohres)'
     +        ,powohres)
      end if

C  nef is the number of coils excluding the OH coil

      nef = nohc
      if (iohcl.ne.0) nef = nef - 1


C+**PJK 19/05/94 Changed output from giving edge locations of coils
C+**PJK 19/05/94 to giving centre locations and cross-sections

      call osubhd(nout,
     +     'Geometry of PF coils, OH coil and plasma :')

      write(nout,10)
 10   format(' coil',t17,'R(m)',t29,'Z(m)',t41,'dR(m)',t53,'dZ(m)',
     +     t65,'turns')
      call oblnkl(nout)

C  PF coils
      write(nout,20) (k,rpf(k),zpf(k),(rb(k)-ra(k)),abs(zh(k)-zl(k)),
     +     turns(k),k=1,nef)
 20   format('  PF',i1,t10,5f12.2)

C  OH coil, if present
      if (iohcl.ne.0) then
         write(nout,30) rpf(nohc),zpf(nohc),(rb(nohc)-ra(nohc)),
     +        abs(zh(nohc)-zl(nohc)),turns(nohc)
 30      format('  OH',t10,5f12.2)
      end if

C  Plasma
      write(nout,40) rmajor,0.0D0,2.0D0*rminor,2.0D0*rminor*kappa,1.0D0
 40   format(' Plasma',t10,5f12.2)

      call osubhd(nout,'PF Coil Information :')

C+**PJK 19/05/94 Modified following outputs to show both allowed and
C+**PJK 19/05/94 actual current densities

      write(nout,50)
 50   format(' coil',
     +     t8, 'current',
     +     t17,'allowed J',
     +     t28,'actual J',
     +     t39,'J',
     +     t43,'coil weight',
     +     t56,'steel weight',
     +     t71,'field')

      write(nout,60)
 60   format(
     +     t10,'(MA)',
     +     t18,'(A/m2)',
     +     t29,'(A/m2)',
     +     t37,'ratio',
     +     t46,'(kg)',
     +     t60,'(kg)',
     +     t72,'(T)')

      call oblnkl(nout)

C  PF coils

C+**PJK 19/05/94      if (ipfres.eq.0) then
C+**PJK 19/05/94         do 70 k = 1,nef
C+**PJK 19/05/94            write(nout,90) k,ric(k),rjpfalw(k),
C+**PJK 19/05/94     +           rpf(k),zpf(k),wtc(k),wts(k),bpf(k)
C+**PJK 19/05/94 70      continue
C+**PJK 19/05/94      else
C+**PJK 19/05/94         do 80 k = 1,nef
C+**PJK 19/05/94            write(nout,90) k,ric(k),rjconpf(k),
C+**PJK 19/05/94     +           rpf(k),zpf(k),wtc(k),wts(k),bpf(k)
C+**PJK 19/05/94 80      continue
C+**PJK 19/05/94      end if

      do 85 k = 1,nef
         write(nout,90) k,ric(k),rjpfalw(k),rjconpf(k),
     +        (rjconpf(k)/rjpfalw(k)),wtc(k),wts(k),bpf(k)
 85   continue

C+**PJK 10/12/92 The 0p syntax is needed here and on line 100
C+**PJK 10/12/92 to prevent a known compiler bug from apparently
C+**PJK 10/12/92 multiplying the f-formatted numbers by 10.

 90   format('  PF',i1,f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,
     +     1pe13.3)

C  OH coil, if present

      if (iohcl.ne.0) then

C+**PJK 19/05/94 write(nout,100) ric(nohc),rjpfalw(nohc),
C+**PJK 19/05/94 + rpf(nohc),zpf(nohc),wtc(nohc),wts(nohc),bpf(nohc)

         write(nout,100) ric(nohc),rjpfalw(nohc),cohbop,
     +        (cohbop/rjpfalw(nohc)),wtc(nohc),wts(nohc),
     +        bpf(nohc)

 100     format('  OH ',f8.2,2(1pe11.3),0p,f6.2,1pe10.3,1pe12.3,
     +     1pe13.3)
      end if

C  Miscellaneous totals

      write(nout,110)
 110  format(t8,'------',t43,'---------',t55,'---------')

      write(nout,120) ricpf,whtpf,whtpfs
 120  format(t6,f8.2,t41,1pe11.3,1pe12.3)

      call osubhd(nout,'PF coil current scaling information :')
      call ovarre(nout,'Sum of squares of residuals ','(ssq0)',ssq0)
      call ovarre(nout,'Smoothing parameter ','(alfapf)',alfapf)

 1000 continue

      return
      end       
