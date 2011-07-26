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
C  Module name    : $RCSfile: osections.h,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1994/05/23 11:32:15 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C  Switches for turning on/off output sections
C
C  sect01 : Power Reactor Costs
C  sect02 : Detailed Costings
C  sect03 : Plasma
C  sect04 : Current Drive System
C  sect05 : Divertor
C  sect06 : Machine Build
C  sect07 : TF Coils
C  sect08 : PF Coils
C  sect09 : Volt Second Consumption
C  sect10 : Support Structure
C  sect11 : PF Coil Inductances
C  sect12 : Shield / Blanket
C  sect13 : Power Conversion
C  sect14 : Power / Heat Transport
C  sect15 : Vacuum System
C  sect16 : Plant Buildings System
C  sect17 : AC Power
C  sect18 : Neutral Beams
C  sect19 : Electron Cyclotron Heating
C  sect20 : Lower Hybrid Heating
C  sect21 : Times

      integer
     +     sect01,sect02,sect03,sect04,sect05,sect06,sect07,sect08,
     +     sect09,sect10,sect11,sect12,sect13,sect14,sect15,sect16,
     +     sect17,sect18,sect19,sect20,sect21

      common/osect1/
     +     sect01,sect02,sect03,sect04,sect05,sect06,sect07,sect08,
     +     sect09,sect10,sect11,sect12,sect13,sect14,sect15,sect16,
     +     sect17,sect18,sect19,sect20,sect21

