C-----*-Fortran-*------------------------------------------------------
C--SCCS information
C  Module         : $Id: ife.h,v 1.1 1997/03/21 16:14:31 peter Exp $
C  Module name    : $RCSfile: ife.h,v $
C  Version no.    : $Revision: 1.1 $
C  Creation date  : $Date: 1997/03/21 16:14:31 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C----------------------------------------------------------------------

C *** Main switches

      INTEGER
     +     IFE,IFETYP,IFEDRV
      COMMON /IFEI/
     +     IFE,IFETYP,IFEDRV

C *** Limits, f-values

      DOUBLE PRECISION
     +     FRRMAX,RRMAX
      COMMON /IFELIM/
     +     FRRMAX,RRMAX

C *** Physics

      DOUBLE PRECISION
     +     DRVEFF,EDRIVE,FBURN,PDRIVE,TGAIN,GAIN,ETADRV,REPRAT
      COMMON /IFEP1/
     +     DRVEFF,EDRIVE,FBURN,PDRIVE,TGAIN,GAIN,ETADRV,REPRAT

      DOUBLE PRECISION
     +     ETAVE(10),GAINVE(10)
      COMMON /IFEP2/
     +     ETAVE,GAINVE

C *** Costs

      DOUBLE PRECISION
     +     UCTARG,UCCARB,UCCONC,UCFLIB,CDRIV0,CDRIV1,CDRIV2,DCDRV0,
     +     DCDRV1,DCDRV2,MCDRIV
      COMMON /IFEC1/
     +     UCTARG,UCCARB,UCCONC,UCFLIB,CDRIV0,CDRIV1,CDRIV2,DCDRV0,
     +     DCDRV1,DCDRV2,MCDRIV

C *** Device build and material fractions and masses

      INTEGER MAXMAT
      PARAMETER (MAXMAT = 7)

      DOUBLE PRECISION
     +     BLDR,BLDZL,BLDZU,
     +     CHRAD,CHDZL,CHDZU,CHVOL,
     +     FWDR,FWDZL,FWDZU,
     +     SHDR,SHDZL,SHDZU,
     +     V1DR,V1DZL,V1DZU,
     +     V2DR,V2DZL,V2DZU,
     +     V3DR,V3DZL,V3DZU,
     +     SOMBDR,SOMTDR,FLIRAD,MFLIBE,FBREED
      COMMON /IFEB1/
     +     BLDR,BLDZL,BLDZU,
     +     CHRAD,CHDZL,CHDZU,CHVOL,
     +     FWDR,FWDZL,FWDZU,
     +     SHDR,SHDZL,SHDZU,
     +     V1DR,V1DZL,V1DZU,
     +     V2DR,V2DZL,V2DZU,
     +     V3DR,V3DZL,V3DZU,
     +     SOMBDR,SOMTDR,FLIRAD,MFLIBE,FBREED

      DOUBLE PRECISION
     +     BLMATF(3,0:MAXMAT),BLMATM(3,0:MAXMAT),BLMATV(3,0:MAXMAT),
     +     CHMATF(0:MAXMAT),CHMATM(0:MAXMAT),CHMATV(0:MAXMAT),
     +     FWMATF(3,0:MAXMAT),FWMATM(3,0:MAXMAT),FWMATV(3,0:MAXMAT),
     +     SHMATF(3,0:MAXMAT),SHMATM(3,0:MAXMAT),SHMATV(3,0:MAXMAT),
     +     V1MATF(3,0:MAXMAT),V1MATM(3,0:MAXMAT),V1MATV(3,0:MAXMAT),
     +     V2MATF(3,0:MAXMAT),V2MATM(3,0:MAXMAT),V2MATV(3,0:MAXMAT),
     +     V3MATF(3,0:MAXMAT),V3MATM(3,0:MAXMAT),V3MATV(3,0:MAXMAT),
     +     BLVOL(3),FWVOL(3),SHVOL(3),V1VOL(3),V2VOL(3),V3VOL(3)

      COMMON/IFEB2/
     +     BLMATF,BLMATM,BLMATV,BLVOL,
     +     CHMATF,CHMATM,CHMATV,
     +     FWMATF,FWMATM,FWMATV,FWVOL,
     +     SHMATF,SHMATM,SHMATV,SHVOL,
     +     V1MATF,V1MATM,V1MATV,V1VOL,
     +     V2MATF,V2MATM,V2MATV,V2VOL,
     +     V3MATF,V3MATM,V3MATV,V3VOL

      DOUBLE PRECISION
     +     R1, R2, R3, R4, R5, R6, R7,
     +     ZL1,ZL2,ZL3,ZL4,ZL5,ZL6,ZL7,
     +     ZU1,ZU2,ZU3,ZU4,ZU5,ZU6,ZU7

      COMMON/IFEB3/
     +     R1, R2, R3, R4, R5, R6, R7,
     +     ZL1,ZL2,ZL3,ZL4,ZL5,ZL6,ZL7,
     +     ZU1,ZU2,ZU3,ZU4,ZU5,ZU6,ZU7

C *** Heat transport

      DOUBLE PRECISION
     +     PIFECR,TDSPMW,TFACMW,PTARGF
      COMMON /IFEP1/
     +     PIFECR,TDSPMW,TFACMW,PTARGF
