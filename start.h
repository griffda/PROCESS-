C-----*-Fortran-*------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : $Id: start.h,v 1.3 1994/03/31 09:40:56 peter Exp $
C  Module name    : $RCSfile: start.h,v $
C  Version no.    : $Revision: 1.3 $
C  Creation date  : $Date: 1994/03/31 09:40:56 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C--Description
C  INCLUDE file for plasma start-up routine
C
C--Author
C  Peter Knight D3/012 Culham Laboratory, ext.3330
C
C--Date
C  08 November 1993
C
C--Reference
C  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
C  
C--History
C  08/11/93 PJK 1.000 Initial version
C
C--Contents
C  nign   : electron density at start-up (m**-3)
C  tign   : electron temperature at start-up (keV)
C  ptaue  : exponent in taue formula
C  qtaue  : exponent in taue formula
C  rtaue  : exponent in taue formula
C  gtaue  : factor in taue formula
C  ftaue  : factor in taue formula
C  aa     : constant
C  bb     : constant
C  cc     : constant
C  dd     : constant
C  s      : constant
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DOUBLE PRECISION
     +     nign,tign,ptaue,qtaue,rtaue,gtaue,ftaue,aa,bb,cc,dd

      COMMON/strt1/
     +     nign,tign,ptaue,qtaue,rtaue,gtaue,ftaue,aa,bb,cc,dd

      INTEGER      s
      COMMON/strt2/s
