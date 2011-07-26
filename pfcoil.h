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
C  Module         : $Id: pfcoil.h,v 3.4 1994/03/31 09:40:52 peter Exp $
C
C  Module name    : $RCSfile: pfcoil.h,v $
C  Version no.    : $Revision: 3.4 $
C
C  Creation date  : $Date: 1994/03/31 09:40:52 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      integer ngrpmx,nclsmx,nptsmx,nfixmx,ngc,ngc2
      parameter (
     +     ngrpmx = 8,
     +     nclsmx = 2,
     +     nptsmx = 32,
     +     nfixmx = 64,
     +     ngc = ngrpmx*nclsmx,
     +     ngc2 = ngc+2 )

      double precision
     +     acsoh, ac1oh, alfapf, bmaxoh, bmaxoh0, cohbof, cohbop,
     +     coheof, cptoh, fcohbof, fcohbop, fcuoh, ohhghf, pfclres,
     +     powohres, powpfres, rjohc, rjohc0, rohc, rpf1, rpf2,
     +     sccufac, sigpfalw, vfohc, whtpf, whtpfs
      common /pfc0/
     +     acsoh, ac1oh, alfapf, bmaxoh, bmaxoh0, cohbof, cohbop,
     +     coheof, cptoh, fcohbof, fcohbop, fcuoh, ohhghf, pfclres,
     +     powohres, powpfres, rjohc, rjohc0, rohc, rpf1, rpf2,
     +     sccufac, sigpfalw, vfohc, whtpf, whtpfs

      integer       ipfres, isumatpf, ncirt, ngrp, nohc
      common /pfc1/ ipfres, isumatpf, ncirt, ngrp, nohc

      double precision
     +     bpf(ngc2),
     +     cpt(ngc2,6),
     +     cptdin(ngc2),
     +     curpfb(ngc2),
     +     curpff(ngc2),
     +     curpfs(ngc2)
      double precision
     +     ra(ngc2),
     +     rb(ngc2),
     +     ric(ngc2),
     +     rjconpf(ngc2),
     +     rjpfalw(ngc2),
     +     rpf(ngc2)
      double precision
     +     turns(ngc2),
     +     vf(ngc2),
     +     waves(ngc2,6),
     +     wtc(ngc2),
     +     wts(ngc2),
     +     zh(ngc2),
     +     zl(ngc2),
     +     zpf(ngc2)
      common /pfc2/
     +     bpf, cpt, cptdin, curpfb, curpff, curpfs, ra, rb, ric,
     +     rjconpf, rjpfalw, rpf, turns, vf, waves, wtc, wts, zh,
     +     zl, zpf

      integer ipfloc(ngc), ncls(ngrpmx+2)
      common /pfc3/ ipfloc, ncls

C  PF scaling variables :

      integer         nfxfh
      common /pfscl1/ nfxfh

      double precision routr
      common /pfscl2/ routr

      double precision zref(ngrpmx)
      common /pfscl3/ zref

