CSCCS-*-Fortran-*-CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
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
C  Module name    : $RCSfile: blanket.h,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1998/04/27 14:01:59 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision xtfi,xtfo,xtb,xpf,xdo,xdi,ph,pr,pin,
     +                 pc,etahp,etainp,etalp,etafp,etacp,fkblkt,
     +                 sgeff
      integer nipfwh,nlpfwh,lblnkt
      integer estr,astr,bstr,costr,smstr

      common/blkre/xtfi,xtfo,xtb,xpf,xdo,xdi,ph,pr,pin,
     +                 pc,etahp,etainp,etalp,etafp,etacp,fkblkt,
     +                 sgeff

      common/blki/nipfwh,nlpfwh,lblnkt,estr,astr,bstr,costr,smstr
