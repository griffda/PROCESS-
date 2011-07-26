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
C  Module         : $Id: lwhymod.f,v 3.1 1993/06/11 14:28:01 peter Exp $
C
C  Module name    : $RCSfile: lwhymod.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:28:01 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE LWHYMOD(nout,iprint)

c  Lower Hybrid module
c
c  This routine is a simplified dummy model of the wall
c  plug power necessary for lwhymod.  It was included in
c  PROCESS in January 1992 by P. C . Shipe.    
c
c  INPUT (and typical values) :
c  etalh,   lh wall plug to plasma efficiency            0.5
c  plhybd,  lh injection power, W 
c
c  OUTPUT :
c  pwplh,   lh wall plug power, W

      IMPLICIT NONE

      INCLUDE 'cdriv.h'
      INCLUDE 'osections.h'

      INTEGER nout,iprint

      pwplh = plhybd / etalh

      if ((iprint.eq.0).or.(sect20.eq.0)) goto 1000

      call oheadr(nout,'Lower Hybrid Heating')
      call ovarre(nout,'Lower hybrid wall plug efficiency','(etalh)'
     +     ,etalh)
      call ovarre(nout,'Lower hybrid injection power (W)','(plhybd)'
     +     ,plhybd)
      call ovarre(nout,'Lower hybrid wall plug power (W)','(pwplh)'
     +     ,pwplh)

 1000 continue

      return
      end
