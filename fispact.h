C--*-Fortran-*---------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : $Id$
C  Module name    : $RCSfile: fispact.h,v $
C  Version no.    : $Revision: 1.1 $
C  Creation date  : $Date: 1997/02/19 13:54:41 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C--Description
C  INCLUDE file containing values calculated by FISPACT routines
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  19 February 1997
C
C--Reference
C  None
C  
C--History
C  06/02/97 PJK 1.000 Initial version
C
C--Contents
C  BLIIZP : Inboard blanket integrated zone power
C  BLIMZP : Inboard blanket mean zone power density
C  BLOIZP : Outboard blanket integrated zone power
C  BLOMZP : Outboard blanket mean zone power density
C  FWIIZP : Inboard first wall integrated zone power
C  FWIMZP : Inboard first wall mean zone power density
C  FWOIZP : Outboard first wall integrated zone power
C  FWOMZP : Outboard first wall mean zone power density
C  BLIACT : Inboard blanket total activity (Bq)
C  BLIGDR : Inboard blanket total gamma dose rate (Sv/hr)
C  BLIHKW : Inboard blanket total heat output (kW)
C  BLOACT : Outboard blanket total activity (Bq)
C  BLOGDR : Outboard blanket total gamma dose rate (Sv/hr)
C  BLOHKW : Outboard blanket total heat output (kW)
C  FWIACT : Inboard first wall total activity (Bq)
C  FWIGDR : Inboard first wall total gamma dose rate (Sv/hr)
C  FWIHKW : Inboard first wall total heat output (kW)
C  FWOACT : Outboard first wall total activity (Bq)
C  FWOGDR : Outboard first wall total gamma dose rate (Sv/hr)
C  FWOHKW : Outboard first wall total heat output (kW)
C  FWTEMP : Outboard first wall temperature after a LOCA (K)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DOUBLE PRECISION
     +     BLIIZP,BLIMZP,BLOIZP,BLOMZP,FWIIZP,FWIMZP,FWOIZP,FWOMZP,
     +     FWTEMP
      COMMON /FISP1/
     +     BLIIZP,BLIMZP,BLOIZP,BLOMZP,FWIIZP,FWIMZP,FWOIZP,FWOMZP,
     +     FWTEMP

      DOUBLE PRECISION 
     +     BLIACT(3),BLIGDR(3),BLIHKW(3),
     +     BLOACT(3),BLOGDR(3),BLOHKW(3),
     +     FWIACT(3),FWIGDR(3),FWIHKW(3),
     +     FWOACT(3),FWOGDR(3),FWOHKW(3)
      COMMON /FISP2/
     +     BLIACT,BLIGDR,BLIHKW,BLOACT,BLOGDR,BLOHKW,
     +     FWIACT,FWIGDR,FWIHKW,FWOACT,FWOGDR,FWOHKW
