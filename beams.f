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
C  Module         : $Id: beams.f,v 3.1 1993/06/11 14:27:38 peter Exp $
C
C  Module name    : $RCSfile: beams.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:27:38 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE NBEAM(nout,iprint)

c  NEUTRAL BEAM PROGRAM (BEAMS)
c
c  Module Description
c
c  This routine was amputated on 3/28/89 to provide only a
c  simplified dummy model of the wall plug power necessary
c  for the neutral beams.  It was included in PROCESS in
c  January 1992 by P. C. Shipe.
c
c  Input (and typical values) :
c
c  cnbeam,  nb current, A
c  enbeam,  nb energy, keV                         1.3d3
c  etanbi,  nb wall plug to injector efficiency    0.5
c
c  Output :
c  pwpnb,   nb wall plug power, W

      IMPLICIT NONE

      INCLUDE 'cdriv.h'
      INCLUDE 'osections.h'

      INTEGER nout,iprint

      pwpnb = enbeam * cnbeam * 1.d3/etanbi

      if ((iprint.eq.0).or.(sect18.eq.0)) goto 1000

      call oheadr(nout,'Neutral Beams')

      call ovarre(nout,'Neutral beam current (A)','(cnbeam)',cnbeam)
      call ovarre(nout,'Neutral beam energy (keV)','(enbeam)',enbeam)
      call ovarre(nout,'Neutral beam wall plug efficiency','(etanbi)',
     +     etanbi)
      call ovarre(nout,'Neutral beam wall plug power (W)','(pwpnb)',
     +     pwpnb)

 1000 continue

      return
      end
