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
C  Module name    : $RCSfile: xc.f,v $
C  Version no.    : $Revision: 3.16 $
C
C  Creation date  : $Date: 1997/03/21 16:07:58 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE LOADXC

c  This subroutine loads the physics and engineering variables to the
c  optimisation variables array XCM.
c
c+**PJK 22/10/92 Removed original arguments (xc,nn)
c+**PJK 22/10/92 xc --> xcm, nn --> nvar, both in COMMON.
C+**PJK 14/11/11 Changed NaN error check

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'
      INCLUDE 'phydat.h'
      INCLUDE 'ineq.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'divrt.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'times.h'
      INCLUDE 'pulse.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'
      INCLUDE 'htpwr.h'

      INTEGER i

      do 10 i = 1,nvar
         if (ixc(i).eq.1) xcm(i)  = aspect
         if (ixc(i).eq.2) xcm(i)  = bt
         if (ixc(i).eq.3) xcm(i)  = rmajor
         if (ixc(i).eq.4) xcm(i)  = te
         if (ixc(i).eq.5) xcm(i)  = beta
         if (ixc(i).eq.6) xcm(i)  = dene
         if (ixc(i).eq.7) xcm(i)  = rnbeam
         if (ixc(i).eq.8) xcm(i)  = fbeta
         if (ixc(i).eq.9) xcm(i)  = fdene
         if (ixc(i).eq.10) xcm(i) = hfact
         if (ixc(i).eq.11) xcm(i) = pheat
         if (ixc(i).eq.12) xcm(i) = oacdcp
         if (ixc(i).eq.13) xcm(i) = tfcth
         if (ixc(i).eq.14) xcm(i) = fwalld
         if (ixc(i).eq.15) xcm(i) = fvs
         if (ixc(i).eq.16) xcm(i) = ohcth
         if (ixc(i).eq.17) xcm(i) = tdwell
         if (ixc(i).eq.18) xcm(i) = q
         if (ixc(i).eq.19) xcm(i) = enbeam
         if (ixc(i).eq.20) xcm(i) = tcpav
         if (ixc(i).eq.21) xcm(i) = ftburn
         if (ixc(i).eq.22) xcm(i) = tbrnmn
         if (ixc(i).eq.23) xcm(i) = fcoolcp
         if (ixc(i).eq.24) then
            xcm(i) = cdtfleg
C+**PJK 28/02/96
            if (irfp.eq.1) then
               write(*,*) 'Error in routine LOADXC:'
               write(*,*) 'CDTFLEG should not be used as an iteration'
               write(*,*) 'variable if IRFP = 1.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
         end if
         if (ixc(i).eq.25) xcm(i) = fpnetel
         if (ixc(i).eq.26) xcm(i) = ffuspow
         if (ixc(i).eq.27) xcm(i) = fhldiv
         if (ixc(i).eq.28) xcm(i) = fjtfc
         if (ixc(i).eq.29) xcm(i) = bore
         if (ixc(i).eq.30) xcm(i) = fmva
         if (ixc(i).eq.31) xcm(i) = gapomin
         if (ixc(i).eq.32) xcm(i) = frminor
         if (ixc(i).eq.33) xcm(i) = fportsz
         if (ixc(i).eq.34) xcm(i) = fdivcol
         if (ixc(i).eq.35) xcm(i) = fpeakb
         if (ixc(i).eq.36) xcm(i) = fbetatry
         if (ixc(i).eq.37) xcm(i) = coheof
         if (ixc(i).eq.38) xcm(i) = fjohc
         if (ixc(i).eq.39) xcm(i) = fjohc0
         if (ixc(i).eq.40) xcm(i) = fgamcd
         if (ixc(i).eq.41) xcm(i) = fcohbop
         if (ixc(i).eq.42) xcm(i) = gapoh
         if (ixc(i).eq.43) xcm(i) = cfe0
         if (ixc(i).eq.44) xcm(i) = fvsbrnni
         if (ixc(i).eq.45) xcm(i) = fqval
         if (ixc(i).eq.46) xcm(i) = fpinj
         if (ixc(i).eq.47) xcm(i) = feffcd
         if (ixc(i).eq.48) xcm(i) = fstrcase
         if (ixc(i).eq.49) xcm(i) = fstrcond
         if (ixc(i).eq.50) xcm(i) = fiooic
         if (ixc(i).eq.51) xcm(i) = fvdump
         if (ixc(i).eq.52) xcm(i) = vdalw
         if (ixc(i).eq.53) xcm(i) = fjprot
         if (ixc(i).eq.54) xcm(i) = ftmargtf
         if (ixc(i).eq.55) xcm(i) = tmargmin
         if (ixc(i).eq.56) xcm(i) = tdmptf
         if (ixc(i).eq.57) then
            xcm(i) = thkcas
C+**PJK 14/04/94 Added error check: Numerical problems if itfmod.ne.1
C+**PJK 14/04/94 as thkcas will be calculated elsewhere
            if (itfmod.ne.1) then
               write(*,*) 'Error in routine LOADXC:'
               write(*,*) 'THKCAS cannot be used as an iteration'
               write(*,*) 'variable if ITFMOD .ne. 1, as it is'
               write(*,*) 'calculated elsewhere.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
         end if
         if (ixc(i).eq.58) xcm(i) = thwcndut
         if (ixc(i).eq.59) xcm(i) = fcutfsu
         if (ixc(i).eq.60) then
            xcm(i) = cpttf
C+**PJK 14/04/94 Added error check: Numerical problems if itfsup=0
C+**PJK 14/04/94 and irfp=0 as cpttf will be calculated elsewhere
            if ((irfp.eq.0).and.(itfsup.eq.0)) then
               write(*,*) 'Error in routine LOADXC:'
               write(*,*) 'CPTTF cannot be used as an iteration'
               write(*,*) 'variable if ITFSUP = 0, as it is'
               write(*,*) 'calculated elsewhere.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
         end if
         if (ixc(i).eq.61) xcm(i) = gapds
         if (ixc(i).eq.62) xcm(i) = fdtmp
         if (ixc(i).eq.63) xcm(i) = ftpeak
         if (ixc(i).eq.64) xcm(i) = fauxmn
         if (ixc(i).eq.65) then
            xcm(i) = tohs
C+**PJK 14/04/94 Added error check: Numerical problems if lpulse.ne.1
C+**PJK 14/04/94 as tohs will be calculated elsewhere
            if (lpulse.ne.1) then
               write(*,*) 'Error in routine LOADXC:'
               write(*,*) 'TOHS cannot be used as an iteration'
               write(*,*) 'variable if LPULSE .ne. 1, as it is'
               write(*,*) 'calculated elsewhere.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
         end if
         if (ixc(i).eq.66) xcm(i) = ftohs
         if (ixc(i).eq.67) xcm(i) = ftcycl
C+**PJK 29/01/96
         if (ixc(i).eq.68) xcm(i) = fptemp
         if (ixc(i).eq.69) xcm(i) = rcool
         if (ixc(i).eq.70) xcm(i) = vcool
         if (ixc(i).eq.71) xcm(i) = fq
         if (ixc(i).eq.72) xcm(i) = fipir
         if (ixc(i).eq.73) xcm(i) = scrapli
         if (ixc(i).eq.74) xcm(i) = scraplo
         if (ixc(i).eq.75) xcm(i) = tfootfi
C+**PJK 28/02/96
         if (ixc(i).eq.76) xcm(i) = frfptf
         if (ixc(i).eq.77) xcm(i) = tftort
         if (ixc(i).eq.78) xcm(i) = rfpth
         if (ixc(i).eq.79) xcm(i) = fbetap
         if (ixc(i).eq.80) xcm(i) = frfpf
C+**PJK 21/03/97
         if (ixc(i).eq.81) xcm(i) = edrive
         if (ixc(i).eq.82) xcm(i) = drveff
         if (ixc(i).eq.83) xcm(i) = tgain
         if (ixc(i).eq.84) xcm(i) = chrad
         if (ixc(i).eq.85) xcm(i) = pdrive
         if (ixc(i).eq.86) xcm(i) = frrmax
C+**PJK 22/05/07
         if (ixc(i).eq.87) then
            xcm(i) = helecmw
            if ((ihplant.lt.1).or.(ihplant.gt.3)) then
               write(*,*) 'Error in routine LOADXC :'
               write(*,*) 'HELECMW cannot be used as an iteration'
               write(*,*) 'variable if IHPLANT=',IHPLANT
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
         end if
         if (ixc(i).eq.88) then
            xcm(i) = hthermmw
            if (ihplant.lt.4) then
               write(*,*) 'Error in routine LOADXC :'
               write(*,*) 'HTHERMMW cannot be used as an iteration'
               write(*,*) 'variable if IHPLANT=',IHPLANT
               write(*,*) 'as it is calculated elsewhere.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
         end if

         if ((ixc(i).lt.1).or.(ixc(i).gt.ipnvars)) then
            write(*,*) 'Error in routine LOADXC :'
            write(*,*) 'Illegal variable number, = ',ixc(i)
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Check that no iteration variable is zero

         if (abs(xcm(i)).le.1.0D-12) then
            write(*,*) 'Error in routine LOADXC :'
            write(*,*)
     +           'Iteration variable ',ixc(i),' (',lablxc(ixc(i)),
     +           ') is zero.'
            write(*,*) 'Change initial value or lower bound.'
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Crude method of catching NaN errors

         if ( (abs(xcm(i)).gt.9.99D99).or.
     +        (xcm(i).ne.xcm(i)) ) then
            write(*,*) 'Error in routine LOADXC :'
            write(*,*) 'NaN error for iteration variable ',ixc(i)
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Check that all iteration variables lie within bounds
C+**PJK 14/11/11 This check has been moved into CONVXC
C         if (ioptimz.ge.0) then
C            if ( ((boundl(ixc(i))-xcm(i)).ge.1.0D-12).or.
C     +           ((xcm(i)-boundu(ixc(i))).ge.1.0D-12) ) then
C               write(*,*) ' '
C               write(*,*) 'Warning in routine LOADXC :'
C               write(*,*)
C     +              'Iteration variable ',ixc(i),' (',lablxc(ixc(i)),
C     +              ') is outside its bounds.'
C               write(*,*) lablxc(ixc(i)), ' = ',xcm(i)
C            end if
C-**PJK         end if

 10   continue

      do 20 i = 1,nvar
         scale(i) = 1.d0
         if (xcm(i).ne.0.d0) scale(i) = 1.d0/xcm(i)
         scafc(i) = 1.d0/scale(i)
         xcm(i) = xcm(i)*scale(i)
 20   continue

      return
      end
c______________________________________________________________________
      SUBROUTINE CONVXC(xc,nn)

c     This subroutine converts scaled optimisation variables back to
c     their real values

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'
      INCLUDE 'phydat.h'
      INCLUDE 'ineq.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'divrt.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'times.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'
      INCLUDE 'htpwr.h'

      double precision xc(ipnvars)
      integer i,nn

      do 10 i = 1,nn

C+**PJK 14/11/11 Enforcing bounds here, instead of in LOADXC
         if (ioptimz.ge.0) then
            if ( ((boundl(ixc(i))-xc(i)/scale(i)).gt.1.0D-12).or.
     +           ((xc(i)/scale(i)-boundu(ixc(i))).gt.1.0D-12) ) then
               write(*,*) ' '
               write(*,*) 'Warning in routine CONVXP :'
               write(*,*)
     +              'Iteration variable ',ixc(i),' (',lablxc(ixc(i)),
     +              ') is outside its bounds.'
               write(*,*) lablxc(ixc(i)), ' = ',xc(i)/scale(i)
               write(*,*) 'Enforcing bound...'
               if ((boundl(ixc(i))-xc(i)/scale(i)).gt.1.0D-12) then
                  xc(i) = boundl(ixc(i))*scale(i)
               else
                  xc(i) = boundu(ixc(i))*scale(i)
               end if
               write(*,*) lablxc(ixc(i)), ' = ',xc(i)/scale(i)
            end if
         end if

         if (ixc(i).eq.1)  aspect   = xc(i)/scale(i)
         if (ixc(i).eq.2)  bt       = xc(i)/scale(i)
         if (ixc(i).eq.3)  rmajor   = xc(i)/scale(i)
         if (ixc(i).eq.4)  te       = xc(i)/scale(i)
         if (ixc(i).eq.5)  beta     = xc(i)/scale(i)
         if (ixc(i).eq.6)  dene     = xc(i)/scale(i)
         if (ixc(i).eq.7)  rnbeam   = xc(i)/scale(i)
         if (ixc(i).eq.8)  fbeta    = xc(i)/scale(i)
         if (ixc(i).eq.9)  fdene    = xc(i)/scale(i)
         if (ixc(i).eq.10) hfact    = xc(i)/scale(i)
         if (ixc(i).eq.11) pheat    = xc(i)/scale(i)
         if (ixc(i).eq.12) oacdcp   = xc(i)/scale(i)
         if (ixc(i).eq.13) tfcth    = xc(i)/scale(i)
         if (ixc(i).eq.14) fwalld   = xc(i)/scale(i)
         if (ixc(i).eq.15) fvs      = xc(i)/scale(i)
         if (ixc(i).eq.16) ohcth    = xc(i)/scale(i)
         if (ixc(i).eq.17) tdwell   = xc(i)/scale(i)
         if (ixc(i).eq.18) q        = xc(i)/scale(i)
         if (ixc(i).eq.19) enbeam   = xc(i)/scale(i)
         if (ixc(i).eq.20) tcpav    = xc(i)/scale(i)
         if (ixc(i).eq.21) ftburn   = xc(i)/scale(i)
         if (ixc(i).eq.22) tbrnmn   = xc(i)/scale(i)
         if (ixc(i).eq.23) fcoolcp  = xc(i)/scale(i)
         if (ixc(i).eq.24) cdtfleg  = xc(i)/scale(i)
         if (ixc(i).eq.25) fpnetel  = xc(i)/scale(i)
         if (ixc(i).eq.26) ffuspow  = xc(i)/scale(i)
         if (ixc(i).eq.27) fhldiv   = xc(i)/scale(i)
         if (ixc(i).eq.28) fjtfc    = xc(i)/scale(i)
         if (ixc(i).eq.29) bore     = xc(i)/scale(i)
         if (ixc(i).eq.30) fmva     = xc(i)/scale(i)
         if (ixc(i).eq.31) gapomin  = xc(i)/scale(i)
         if (ixc(i).eq.32) frminor  = xc(i)/scale(i)
         if (ixc(i).eq.33) fportsz  = xc(i)/scale(i)
         if (ixc(i).eq.34) fdivcol  = xc(i)/scale(i)
         if (ixc(i).eq.35) fpeakb   = xc(i)/scale(i)
         if (ixc(i).eq.36) fbetatry = xc(i)/scale(i)
         if (ixc(i).eq.37) coheof   = xc(i)/scale(i)
         if (ixc(i).eq.38) fjohc    = xc(i)/scale(i)
         if (ixc(i).eq.39) fjohc0   = xc(i)/scale(i)
         if (ixc(i).eq.40) fgamcd   = xc(i)/scale(i)
         if (ixc(i).eq.41) fcohbop  = xc(i)/scale(i)
         if (ixc(i).eq.42) gapoh    = xc(i)/scale(i)
         if (ixc(i).eq.43) cfe0     = xc(i)/scale(i)
         if (ixc(i).eq.44) fvsbrnni = xc(i)/scale(i)
         if (ixc(i).eq.45) fqval    = xc(i)/scale(i)
         if (ixc(i).eq.46) fpinj    = xc(i)/scale(i)
         if (ixc(i).eq.47) feffcd   = xc(i)/scale(i)
         if (ixc(i).eq.48) fstrcase = xc(i)/scale(i)
         if (ixc(i).eq.49) fstrcond = xc(i)/scale(i)
         if (ixc(i).eq.50) fiooic   = xc(i)/scale(i)
         if (ixc(i).eq.51) fvdump   = xc(i)/scale(i)
         if (ixc(i).eq.52) vdalw    = xc(i)/scale(i)
         if (ixc(i).eq.53) fjprot   = xc(i)/scale(i)
         if (ixc(i).eq.54) ftmargtf = xc(i)/scale(i)
         if (ixc(i).eq.55) tmargmin = xc(i)/scale(i)
         if (ixc(i).eq.56) tdmptf   = xc(i)/scale(i)
         if (ixc(i).eq.57) thkcas   = xc(i)/scale(i)
         if (ixc(i).eq.58) thwcndut = xc(i)/scale(i)
         if (ixc(i).eq.59) fcutfsu  = xc(i)/scale(i)
         if (ixc(i).eq.60) cpttf    = xc(i)/scale(i)
         if (ixc(i).eq.61) gapds    = xc(i)/scale(i)
         if (ixc(i).eq.62) fdtmp    = xc(i)/scale(i)
         if (ixc(i).eq.63) ftpeak   = xc(i)/scale(i)
         if (ixc(i).eq.64) fauxmn   = xc(i)/scale(i)
         if (ixc(i).eq.65) tohs     = xc(i)/scale(i)
         if (ixc(i).eq.66) ftohs    = xc(i)/scale(i)
         if (ixc(i).eq.67) ftcycl   = xc(i)/scale(i)
C+**PJK 29/01/96
         if (ixc(i).eq.68) fptemp   = xc(i)/scale(i)
         if (ixc(i).eq.69) rcool    = xc(i)/scale(i)
         if (ixc(i).eq.70) vcool    = xc(i)/scale(i)
         if (ixc(i).eq.71) fq       = xc(i)/scale(i)
         if (ixc(i).eq.72) fipir    = xc(i)/scale(i)
         if (ixc(i).eq.73) scrapli  = xc(i)/scale(i)
         if (ixc(i).eq.74) scraplo  = xc(i)/scale(i)
         if (ixc(i).eq.75) tfootfi  = xc(i)/scale(i)
C+**PJK 28/02/96
         if (ixc(i).eq.76) frfptf   = xc(i)/scale(i)
         if (ixc(i).eq.77) tftort   = xc(i)/scale(i)
         if (ixc(i).eq.78) rfpth    = xc(i)/scale(i)
         if (ixc(i).eq.79) fbetap   = xc(i)/scale(i)
         if (ixc(i).eq.80) frfpf    = xc(i)/scale(i)
C+**PJK 21/03/97
         if (ixc(i).eq.81) edrive   = xc(i)/scale(i)
         if (ixc(i).eq.82) drveff   = xc(i)/scale(i)
         if (ixc(i).eq.83) tgain    = xc(i)/scale(i)
         if (ixc(i).eq.84) chrad    = xc(i)/scale(i)
         if (ixc(i).eq.85) pdrive   = xc(i)/scale(i)
         if (ixc(i).eq.86) frrmax   = xc(i)/scale(i)
C+**PJK 22/05/07
         if (ixc(i).eq.87) helecmw  = xc(i)/scale(i)
         if (ixc(i).eq.88) hthermmw = xc(i)/scale(i)

C+**PJK 22/08/12 .and. changed to .or.
         if ((ixc(i).lt.1).or.(ixc(i).gt.ipnvars)) then
            write(*,*) 'Error in routine CONVXC :'
            write(*,*) 'Illegal variable number, = ',ixc(i)
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Check that no iteration variable is zero

         if (abs(xc(i)).le.1.0D-12) then
            write(*,*) 'Error in routine CONVXC :'
            write(*,*)
     +           'Iteration variable ',ixc(i),' (',lablxc(ixc(i)),
     +           ') is zero.'
            write(*,*) 'Change initial value or lower bound.'
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

C  Crude method of catching NaN errors

         if ((abs(xc(i)).gt.9.99D99).or.(xc(i).ne.xc(i))) then
            write(*,*) 'Error in routine CONVXC :'
            write(*,*) 'NaN error for iteration variable ',ixc(i)
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

         if (scale(i).eq.0.0D0) then
            write(*,*) 'Error in routine CONVXC :'
            write(*,*) 'scale(i) = 0 for iteration variable ',ixc(i)
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

 10   continue

c  Reset some other values

      if (tratio.gt.0.0D0) ti = tratio*te

      return
      end
c______________________________________________________________________
      SUBROUTINE BOUNDXC

c  Reset bounds to real values
c
c+**PJK 22/10/92 Removed arguments (xc,nn)
c+**PJK 22/10/92 xc was never used anyway
c+**PJK 22/10/92 nn replaced with nvar (which is in COMMON)

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'

      integer i

      do 10 i = 1,nvar
         bondl(i) = boundl(ixc(i))*scale(i)
         bondu(i) = boundu(ixc(i))*scale(i)
 10   continue

      return
      end        
