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
C  Module name    : $RCSfile: optimiz.f,v $
C  Version no.    : $Revision: 3.4 $
C
C  Creation date  : $Date: 1996/02/01 15:07:03 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE OPTIMIZ(ifail,f)

c  This routine calls the minimisation/maximisation routine VMCON.
c
c+**PJK 22/10/92 Replaced original arguments (xc,rco,ifail,nfev)
c+**PJK 15/12/92 with just (ifail,f). The others are passed into
c+**PJK 22/10/92 the routine via COMMON (file numer.h).
c
c+**PJK 22/10/92 xc --> xcm ; rco --> rcm ; nfev --> nfev2

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'

      INTEGER ifail,ii,ipldel,iplh,ipliwa,ippn1,ipvmu,lb,lcnorm,ldel,
     +     lh,liwa,lwa,m,meq,mode,n

      PARAMETER (
     +     ippn1  = ipnvars+1,
     +     ipldel = 7*ippn1,
     +     iplh   = 2*ippn1,
     +     ipvmu  = ipeqns+2*ipnvars+1,
     +     ipliwa = 6*ippn1+ipeqns)

      DOUBLE PRECISION xv(ipnvars),
     +     fgrd(ipnvars),conf(ipeqns),cnorm(ippn1,ipeqns),
     +     b(ippn1,ippn1),glag(ipnvars),cm(ipeqns),vmu(ipvmu),
     +     delta(ipldel),bdelta(ipnvars),glaga(ipnvars),gammv(ipnvars),
     +     etav(ipnvars),xa(ipnvars),gm(ippn1),bdl(ippn1),bdu(ippn1),
     +     h(iplh,iplh),wa(iplh),bndl(ipnvars),bndu(ipnvars)
      DOUBLE PRECISION f,xtol

      INTEGER iwa(ipliwa),ilower(ipnvars),iupper(ipnvars)

      EXTERNAL fcnvmc1, fcnvmc2

      n = nvar
      m = neqns
      meq = neqns-nineqns
      xtol = epsvmc

c  This section determines the input used to call the
c  Argonne National Lab minimization/maximization routine VMCON.

c  Input parameters

      mode = 0
      lb = ippn1
      lcnorm = ippn1
      ldel = ipldel
      lh = iplh
      lwa = iplh
      liwa = ipliwa

      do 10 ii = 1,n
         ilower(ii) = 1
         iupper(ii) = 1
         bndl(ii) = bondl(ii)
         bndu(ii) = bondu(ii)
         xv(ii) = xcm(ii)
 10   continue

      nvrbl = nvar

      call VMCON(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm,
     +     lcnorm,b,lb,xtol,maxcal,ifail,nfev2,vlam,glag,vmu,cm,glaga,
     +     gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa,
     +     liwa,ilower,iupper,bndl,bndu)

      do 20 ii = 1,n
         xcm(ii) = xv(ii)
 20   continue

      do 30 ii=1,m
         rcm(ii) = conf(ii)
 30   continue

      return
      end
c______________________________________________________________________
      SUBROUTINE FUNFOM(fc)

c  Objective function evaluator for VMCON
c  i.e. figure-of-merit evaluator.
c
c  Each equation for fc should give a value of the order of unity for
c  the sake of the numerics.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'cost.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'divrt.h'

      INTEGER iab

      DOUBLE PRECISION fc,sgn

      iab = abs(minmax)
      sgn = DBLE(minmax)/DBLE(iab)

C  If sgn is negative the value of fc will be maximised.
C  If sgn is positive the value of fc will be minimised.

C  Minimise/maximise major radius

      if (iab .eq. 1) then
         fc = sgn * 0.2D0 * rmajor

C  Minimise/maximise fusion power / input power

      else if (iab .eq. 2 ) then
C+**PJK 29/01/96 Added ppump
         fc = sgn * powfmw/( (pinji+pinje)/1.0D6 + tfcpmw + ppump/1.0D6)

C  Minimise/maximise neutron wall load

      else if (iab .eq. 3 ) then
         fc = sgn * wallmw

C  Minimise/maximise TF coil + PF coil power

      else if (iab .eq. 4 ) then
         fc = sgn * ( tfcmw + 1.0D-3 * srcktpm)/10.0D0

C  Minimise/maximise fusion power / injection power

      else if (iab .eq. 5 ) then
         fc = sgn * powfmw/( (pinji+pinje)/1.0D6 )

C  Minimise/maximise cost of electricity

      else if (iab .eq. 6 ) then
         fc = sgn * coe/100.0D0

C  Minimise/maximise direct/constructed/capital cost

      else if (iab .eq. 7 ) then

         if (ireactor.eq.0) then
            fc = sgn * cdirt/1.0D3
         else
            fc = sgn * concost/1.0D3

         end if

C  Minimise/maximise aspect ratio

      else if (iab .eq. 8 ) then
         fc = sgn * aspect

C  Minimise/maximise divertor heat load

      else if (iab .eq. 9 ) then
         fc = sgn * hldiv

C  Minimise/maximise toroidal field on axis

      else if (iab .eq. 10 ) then
         fc = sgn * bt

C  Minimise/maximise injection power

      else if (iab .eq. 11 ) then
         fc = sgn * (pinje+pinji) / 1.0D6

C  Minimise/maximise hydrogen production capital cost

      else if (iab .eq. 12 ) then
         fc = sgn * chplant / 1.0D2

C  Minimise/maximise hydrogen production rate

      else if (iab .eq. 13 ) then
         fc = sgn * hpower / 1.0D2

      else
         WRITE(*,*) 'Error in routine FUNFOM :'
         WRITE(*,*) 'No such figure of merit, ',iab
         WRITE(*,*) 'PROCESS stopping.'
         STOP

      end if

C  Crude method of catching NaN errors

      if (abs(fc).gt.9.99D99) then
         WRITE(*,*) 'Error in routine FUNFOM:'
         WRITE(*,*) 'NaN error in figure of merit calculation ',iab
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

      return
      end
c______________________________________________________________________
      SUBROUTINE FCNVMC1(n,m,xv,objf,conf,ifail)

c  Function evaluator for VMCON. It calculates the objective and
c  constraint functions at x. Note that the equality constraints
c  must precede the inequality constraints in conf.
c
c  INPUT :
c  n     - number of variables
c  m     - number of constraints
c  xv    - scaled variable values
c
c  OUTPUT :
c  objf  - the objective function
c  conf  - the constraint functions
c  ifail  - flag, if < 0 stops calculation

      IMPLICIT NONE

      INTEGER n,m,ifail

      DOUBLE PRECISION xv(n),conf(m)
      DOUBLE PRECISION fbac,ffor,objf

      call caller(xv,n)
      call funfom(objf)

c+**PJK 23/10/92 Removed redundant arguments (n,.,xv,....)
c+**PJK 23/10/92 from call to CON1.

      call con1(m,conf)

c  To stop the program, set ifail < 0 here.

      ifail = 1 * ifail

      return
      end
c______________________________________________________________________
      SUBROUTINE FCNVMC2(n,m,xv,fgrd,cnorm,lcnorm,ifail)

c  Function evaluator for VMCON. It calculates the gradients of the
c  objective and constraint functions at x.
c  Note that the equality constraints must precede the
c  inequality constraints in conf. The constraint gradients
c  or normals are returned as the columns of cnorm.
c
c  INPUT :
c  n      - number of variable
c  m      - number of constraints
c  xv     - scaled variable values
c  lcnorm - number of columns in cnorm
c
c  OUTPUT :
c  fgrd   - gradient of the objective function
c  cnorm  - the constraint gradients (i.e. cnorm(i,j) is the derivative
c           of constraint j  w.r.t.  variable i )
c  ifail  - flag, if < 0 stops calculation

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'

      INTEGER i,j,n,m,lcnorm,ifail

      DOUBLE PRECISION xfor(ipnvars),xbac(ipnvars),cfor(ipnvars),
     +     cbac(ipnvars),xv(n),fgrd(n),cnorm(lcnorm,m),fbac,ffor

      do 30 i = 1,n
         do 10 j = 1,n
            xfor(j) = xv(j)
            xbac(j) = xv(j)
            if (i.eq.j) then
               xfor(i) = xv(j)*(1.0D0+epsfcn)
               xbac(i) = xv(j)*(1.0D0-epsfcn)
            end if
 10      continue

         call caller(xfor,n)
         call funfom(ffor)

c+**PJK 23/10/92 Removed redundant arguments (n,.,xfor,....)
c+**PJK 23/10/92 from call to CON1.

         call con1(m,cfor)

         call caller(xbac,n)
         call funfom(fbac)

c+**PJK 23/10/92 Removed redundant arguments (n,.,xbac,....)
c+**PJK 23/10/92 from call to CON1.

         call con1(m,cbac)

         fgrd(i)=(ffor-fbac)/(xfor(i)-xbac(i))
         do 20 j=1,m
            cnorm(i,j)=(cfor(j)-cbac(j))/(xfor(i)-xbac(i))
 20      continue
 30   continue

c  To stop the program, set ifail < 0 here.

      ifail = 1 * ifail

      return
      end
