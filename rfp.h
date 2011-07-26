C--*-Fortran-*---------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : $Id$
C  Module name    : $RCSfile: rfp.h,v $
C  Version no.    : $Revision: 1.1 $
C  Creation date  : $Date: 1996/03/07 16:10:39 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C--Description
C  INCLUDE file for reversed-field pinch module in PROCESS.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  27 February 1996
C
C--Reference
C  None
C  
C--History
C  27/02/96 PJK 1.000 Initial version
C
C--Contents
C  irfp   : Switch for rfp option (0=off)
C  nrfppf : number of RFP PF coils
C  rrpf   : radius of each RFP PF coil (m)
C  zzpf   : vertical position of each RFP PF coil (m)
C  drpf   : radial cross-section of each RFP PF coil (m)
C  dzpf   : vertical cross-section of each RFP PF coil (m)
C  nturns : number of turns of each RFP PF coil
C  cptrfp : current per turn in each RFP PF coil (A/m2)
C  resrfp : resistance of each RFP PF coil
C  tftort : TF coil toroidal thickness (m)
C  pfrmax : radius of largest PF coil (m)
C  pfmmax : mass of heaviest PF coil (tonnes)
C  rfpf   : reversal parameter F
C  rfpth  : pinch parameter theta
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      integer nrfppf
      parameter (nrfppf = 16)

      double precision
     +     rrpf(nrfppf),zzpf(nrfppf),drpf(nrfppf),dzpf(nrfppf),
     +     nturns(nrfppf),cptrfp(nrfppf),resrfp(nrfppf)
      common/rfpdar/
     +     rrpf,zzpf,drpf,dzpf,nturns,cptrfp,resrfp

      double precision
     +     tftort,pfrmax,pfmmax,rfpf,rfpth
      common/rfpdbl/
     +     tftort,pfrmax,pfmmax,rfpf,rfpth

      integer
     +     irfp
      common/rfpint/
     +     irfp

