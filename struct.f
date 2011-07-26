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
C  Module         : $Id: struct.f,v 3.2 1996/02/01 15:04:17 peter Exp $
C
C  Module name    : $RCSfile: struct.f,v $
C  Version no.    : $Revision: 3.2 $
C
C  Creation date  : $Date: 1996/02/01 15:04:17 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE STRUCALL(nout,iprint)

c  Subroutine to call the structure module.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'divrt.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'struccom.h'
      INCLUDE 'tfcoil.h'

      DOUBLE PRECISION twhtpf
      INTEGER nout,iprint

c  Total weight of the PF coil conductor and its structure
      twhtpf = whtpf + whtpfs

C+**PJK 01/02/96 Added itfsup,ipfres to argument list of STRUCT

      call struct(plascur,rmajor,rminor,kappa,bt,itfsup,ipfres,tfboreh,
     +     hmax,whtshld,divmas,twhtpf,whttf,fwmass,whtblkt,coolmass,
     +     wtbc,dewmkg,nout,iprint,fncmass,aintmass,clgsmass,coldmass,
     +     gsmass)

      return
      end
c______________________________________________________________________
      SUBROUTINE STRUCT(ai,r0,a,akappa,b0,itfsup,ipfres,boreh,tfhmax,
     +     shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass,
     +     wtbc,dewmass,nout,iprint,fncmass,aintmass,clgsmass,coldmass,
     +     gsm)

c  Module to calculate mass of support structure
c
c  Version 2.0  9/90 . Reprogrammed by J. Galambos to match
c  the ITER (i.e. B. Spears) rules.
c
c  Modified 2/19/91 to use implicit none
c  by J. Galambos (from Spears model)
c
c  Included in PROCESS in January 1992.
c 
c  INPUTS
c  ------
c
c  Variable   Source  Description
c  --------   ------  -----------
c  ai         physics plasma current (max design value) (A)
c  r0         physics major radius (m)
c  a          physics minor radius (m)
c  akappa     physics elongation
c  b0         physics axial B-field (T)
c  boreh      tfcoil  TF Coil horizontal bore (m)
c  tfhmax     tfcoil  TF coil max  height (m)
c
c  itfsup     switch denoting whether TF coils are superconducting
c  ipfres     switch denoting whether PF coils are resistive
c
c  shldmass   shield  total mass of shield (kg)
c  dvrtmass   torus   total mass of divertor and assoc. structure (kg)
c  pfmass     pf mag  total mass of pf coils plus cases (kg)
c  tfmass     tf mag  total mass of tf coils plus cases (kg)
c  blmass     torus   blanket mass (kg)
c  fwmass     torus   first wall mass (kg)
c  coolmass   torus   total water coolant mass (kg)
c  wtbc      buck-cyl  bucking cylinder weight
c  dewmass    torus    dewar mass (kg)
c  nout       main     logical unit number for output print file
c  iprint     main     instruction to print results: 0/1 = no/yes
c
c  OUTPUTS
c  -------
c
c  fncmass    mass of outer pf coil support fence (kg)
c  aintmass   mass of intercoil support (kg)
c  clgsmass   coil gravity support mass (kg)
c  coldmass   total mass of cryogenic temp. stuff (kg)
c  gsm        gravity support for magnets, and shield/blanket (kg)

      IMPLICIT NONE

      INCLUDE 'osections.h'

      DOUBLE PRECISION ai,r0,a,akappa,b0,boreh,tfhmax,
     +     shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass,
     +     wtbc,dewmass,fncmass,aintmass,clgsmass,coldmass,gsm
      DOUBLE PRECISION dens,gsm1,gsm2,gsm3,sigal,ws1,ws2,coilmass

      INTEGER nout,iprint,itfsup,ipfres

c  Outer PF coil fence (fit to ITER 9/07/90)
      fncmass = 2.1d-11*ai*ai*r0*akappa*a

c  Intercoil support between TF coils to react overturning moment
c  Changed 9/08/90 to fit ITER scaling)
      aintmass = 1.4d6 * (ai/2.2d7) * b0/4.85d0 * boreh**2/50.d0

C+**PJK 01/02/96 Recalculated coldmass taking into account which
C+**PJK 01/02/96 coils are superconducting and therefore are cold

c  Total mass of coils plus support plus dewar
      coilmass = tfmass + pfmass + aintmass + dewmass + wtbc

c  Total mass of cooled components
      coldmass = 0.0D0
      if (itfsup.eq.1) coldmass = coldmass + tfmass + aintmass
     +     + dewmass + wtbc
      if (ipfres.ne.1) coldmass = coldmass + pfmass

c  Coil gravity support mass
c  Set density (kg/m3) and allowable stress (Pa)
      dens = 7.8d3
      sigal = 2.5d7
C+**PJK 01/02/96 coilmass replaces coldmass here
      clgsmass = coilmass * (r0/6.d0)*9.1d0 * 9.807d0*dens/sigal

c  Gravity support masses scaled from Spears algorithms (9/90) :

c  Torus leg support
      ws1 = coolmass + fwmass + blmass + shldmass + dvrtmass
      gsm1 = 5.d0 * 9.807d0 * ws1 * dens/sigal

c  Ring beam
      ws2 = ws1 + tfmass + pfmass + aintmass + clgsmass
      gsm2 = 1.d-3 * 34.77d0 * (r0+1.d0) * sqrt(ws2/1000.d0)*dens

c  Ring legs (this term may be too big, need to check)
      gsm3 = 1.d-6 * 0.3d0 * (tfhmax + 2.d0) * ws2 * dens

      gsm = gsm1 + gsm2 + gsm3

c  Print results

      if ((iprint.eq.0).or.(sect10.eq.0)) goto 1000

      call oheadr(nout,'Support Structure')
      call ovarre(nout,'Outer PF coil fence mass (kg)','(fncmass)',
     +     fncmass)
      call ovarre(nout,'Intercoil support structure mass (kg)',
     +     '(aintmass)',aintmass)
      call ovarre(nout,'Mass of cooled components (kg)',
     +     '(coldmass)',coldmass)
      call ovarre(nout,'Gravity support structure mass (kg)',
     +     '(clgsmass)',clgsmass)
      call ovarre(nout,'Torus leg support mass (kg)','(gsm1)',gsm1)
      call ovarre(nout,'Ring beam mass (kg)','(gsm2)',gsm2)
      call ovarre(nout,'Ring legs mass (kg)','(gsm3)',gsm3)

 1000 continue

      return
      end
