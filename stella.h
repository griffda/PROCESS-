C-----*-Fortran-*------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : $Id: stella.h,v 1.1 1994/08/01 10:15:52 peter Exp $
C  Module name    : $RCSfile: stella.h,v $
C  Version no.    : $Revision: 1.1 $
C  Creation date  : $Date: 1994/08/01 10:15:52 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C--Description
C  INCLUDE file for stellarator module in PROCESS.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Initial version
C
C--Contents
C  istell : Switch for stellarator option (0=off)
C  isthtr : Switch for different auxiliary heating methods
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER
     +     istell,isthtr
      COMMON/stlint/
     +     istell,isthtr
