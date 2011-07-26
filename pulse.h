C-----*-Fortran-*------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--SCCS information
C  Module         : $Id: pulse.h,v 1.4 1994/04/11 11:13:27 peter Exp $
C  Module name    : $RCSfile: pulse.h,v $
C  Version no.    : $Revision: 1.4 $
C  Creation date  : $Date: 1994/04/11 11:13:27 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C--Description
C  Include file containing pulsed reactor variables
C
C--Author
C  Chris Gardner, c/o
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  11 April 1994
C
C--Reference
C  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
C  
C--History
C  08/11/93 PJK 1.000 Initial version
C  11/04/94 PJK 1.100 Changed ITPULS to ITCYCL
C
C--Contents
C  afw    : inner radius of each first wall structural cylinder (m)
C  bfw    : outer radius of each first wall structural cylinder (m)
C  bctmp  : bulk coolant temperature (C)
C  coolp  : coolant pressure (Pa)
C  dtstor : maximum allowable temperature change within the stainless
C           steel thermal storage block (K)
C  fwlife : first wall lifetime (yrs)
C  tmprse : temperature rise in coolant along toroidal
C           extent of first wall (C)
C  tpeak  : peak temperature in first wall (C)
C  istore : switch for thermal storage method (1/2/3)
C  itcycl : switch for first wall axial stress model (1/2/3)
C  lpulse : switch for reactor model : 1 = pulsed, 0 = continuous
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      double precision afw,bfw,bctmp,coolp,dtstor,fwlife,tmprse,tpeak
      common/pulse1/   afw,bfw,bctmp,coolp,dtstor,fwlife,tmprse,tpeak

      integer       istore,itcycl,lpulse
      common/pulse2/istore,itcycl,lpulse
