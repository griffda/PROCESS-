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
C  Module         : $Id$
C
C  Module name    : $RCSfile: aachange.f,v $
C  Version no.    : $Revision: 3.30 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      SUBROUTINE INFORM(PROGID)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.029
C
C--Description
C  Routine to obtain information about the program being executed.
C  Uses UNIX system calls out of necessity, so contains non-standard
C  coding.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 May 2007
C
C--Reference
C  None
C  
C--History
C  03/10/96 PJK 1.000 Initial version
C  07/10/96 PJK 1.001 PROCESS 3001
C  08/10/96 PJK 1.002 PROCESS 3002
C  22/10/96 PJK 1.003 PROCESS 3003
C  20/01/97 PJK 1.004 PROCESS 3004
C  24/01/97 PJK 1.005 PROCESS 3005
C  05/02/97 PJK 1.006 PROCESS 3006
C  19/02/97 PJK 1.007 PROCESS 3007
C  26/02/97 PJK 1.008 PROCESS 3008
C  21/03/97 PJK 1.009 PROCESS 3009
C  10/09/97 PJK 1.010 PROCESS 3010
C  17/09/97 PJK 1.011 PROCESS 3011
C  19/11/97 PJK 1.012 PROCESS 3012
C  01/04/98 PJK 1.013 PROCESS 3013
C  24/04/98 PJK 1.014 PROCESS 3014
C  23/06/98 PJK 1.015 PROCESS 3015
C  26/06/98 PJK 1.016 PROCESS 3016
C  17/07/98 PJK 1.017 PROCESS 3017
C  08/10/98 PJK 1.018 PROCESS 3018
C  19/01/99 PJK 1.019 PROCESS 3019
C  17/05/99 PJK 1.020 PROCESS 3020
C  06/07/99 PJK 1.021 PROCESS 3021
C  16/06/00 PJK 1.022 PROCESS 3022: Modified 'whoami' call in this
C                                   routine, and made a single Makefile
C                                   suitable for both AIX and Linux
C  04/05/01 PJK 1.023 PROCESS 3023
C  03/07/01 PJK 1.024 PROCESS 3024
C  16/07/01 PJK 1.025 PROCESS 3025
C  25/04/02 PJK 1.026 PROCESS 3026
C  16/06/04 PJK 1.027 PROCESS 3027
C  22/05/06 PJK 1.028 PROCESS 3028
C  22/05/07 PJK 1.029 PROCESS 3029
C
C--Arguments
C  PROGID : (OUTPUT) Character string array containing useful info
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*72 PROGID(0:10)

C  Local variables
      CHARACTER*10 PROGRM,PROGNM
      CHARACTER*52 ID(10)

C  External functions
      INTEGER LENSTR
      EXTERNAL LENSTR

C  External routines
      EXTERNAL SYSTEM

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Program name and version number

      PROGRM = 'PROCESS'
      PROGNM = '3029'

C *** Create temporary data file

      CALL SYSTEM('/bin/rm -f PROGID.DAT'//CHAR(0))
      CALL SYSTEM('/bin/touch PROGID.DAT'//CHAR(0))

C *** Write information to data file
C *** ==============================

C *** Current date and time

      CALL SYSTEM('/bin/date >> PROGID.DAT'//CHAR(0))

C *** User name

      CALL SYSTEM('/usr/bin/whoami >> PROGID.DAT'//CHAR(0))

C *** Computer identifier

      CALL SYSTEM('/bin/hostname >> PROGID.DAT'//CHAR(0))

C *** Working directory

      CALL SYSTEM('/bin/pwd >> PROGID.DAT'//CHAR(0))

C *** Read back information into ID array

      OPEN(UNIT=1,FILE='PROGID.DAT',STATUS='OLD')
      READ(1,'(A)') ID(1)
      READ(1,'(A)') ID(2)
      READ(1,'(A)') ID(3)
      READ(1,'(A)') ID(4)
      CLOSE(UNIT=1)

C *** Delete data file

      CALL SYSTEM('/bin/rm -f PROGID.DAT'//CHAR(0))

C *** Annotate information and store in PROGID character array
C *** for use in other program units via the routine argument

      PROGID(1) = '  Program : '//PROGRM
      PROGID(2) = '  Version : '//PROGNM
      PROGID(3) = 'Date/time : '//ID(1)
      PROGID(4) = '     User : '//ID(2)
      PROGID(5) = ' Computer : '//ID(3)
      PROGID(6) = 'Directory : '//ID(4)

C *** Summarise most useful data, and store in PROGID(0)

      PROGID(0) = PROGRM(1:LENSTR(PROGRM,10))//' '//
     +     PROGNM(1:LENSTR(PROGNM,10))//' : Run at '//
     +     ID(1)(1:LENSTR(ID(1),52))

      RETURN
      END
c______________________________________________________________________
      SUBROUTINE DUMMY

C  Change Log
C
C  11/12/90 /JDG/ Changes made to get the code running on the CRAY again
C
C    - change "call opens" back to "call link" in subroutine initial
C    - changed "real*8 function"  to "function" on functions :
C      enorm, spmpar, zeroin, and sdot
C    - removed routines to do time, date - (timdate, timused,
C      qqdate, qqtime)
C    - remove subroutines that weren't used - sgefa, sgedi
C    - remove subroutine svd (use CRAY lib version)
C    - reinstate call to "ioscale" in routine init
C    - change data statments in routine spmpar from Prime to Cray.
C    - change IOTTY setting from 1 to 59 in routine initial.
C    - change "call exit" to "call exit(1)" in main program
C      (to keep dropfile)
C    - the routines that were removed are stored in a file called
C      "extra"
C
C  11/13/90  change dimension statements in VMCON and called routines
C            to use parameter statement instead of fixed values -
C            *** NOTE **** should try and get vmcon back to original
C            form
C            cleaned up call arguments to "con1" and "funct1"
C            moved kappa,triang scaling from "physics" to "geomty"
C            was bug)
C  11/26/90  put in increased vertical build option (istok) for
C            gaseous divertor concept. Change the centerpost shape
C            from straignt to "tappered  along the plasma height.
C  12/02/90  add simple pf coil current scaling option
C            (use istokpf = 1 - which is default)
C  12/11/91 - 12/13/91 
C            put in changes identified by J. Hicks: fix printout
C            of confinement scaling, remove "redundant" printout
C            of "ixc,
C            icc and bounds" near the beginning of the output, remove
C            printout of the input guess on the PF coil current density,
C            remove "reactive" component of the TF coil voltage, allow
C            for specification of the bootstrap current via "bscfmax",
C            remove breaker component of TF coil power supply, make
C            the centerpost cost a "replaceable fuel" category item,
C            reinstate the 1 year minimum for the depreciation
C            factor in the costing routine.
C  1/04/91   put in switch IOHCIE to permit internal/external
C            OH coil option
C  1/07/91   put in switch ITFSUP switch for superconducting TF coils.
C            this switch activates the superconducting TF coil
C            calculations
C            and the conventional shield/blanket volume calculations.
C            still need coding for the superconducting TF coil makeup,
C            costing and allowable J.
C  1/08/91   add cryogenic module for superconducting option.
C  3/05/91   add TF supercoducting (Nb3Sn) option (still needs
C            cost, power
C            supply + NbTi)
C  3/10/91   add option to do conventional copper cases (no cost though)
C            with ipfres = 1 and itfsup = 2 switch.
C  3/11/91   updateITER divertor model and density limit routines
C  3/18/91   add port size equation and minor radius limit equation
C  3/19/91   add equation for the divertor plasma collisionality.
C  3/20/91   add routine to do simple cost for experiment (ireactor = 3)
C            and minmax = 7 option, change PF shaping coil current
C            scaling
C            and slight change to elongation scaling.
C  3/21/91   add option to have minimum radiation fraction (fradmin)
C            (STAR2.5)
C  3/22/91   polish routine "costexp", add option for H fuel
C  3/22/91   add aspect to FOM choices (minmax = 8)
C  3/25/91   change 1st wall area algorithm and ireacto=3 TF coil
C            cost scaling
C  3/27/91   changed ireactor = 3 cost scaling (divertor)
C  3/29/91   modified minmax = 7 FOM scaling
C  4/22/91   add TF superconductor costs, and power conversion options.
C            fix PF power conversion costing
C            add eq. 23, var. 35
C            fixed divertor surface area.
C
C  bugs found / changes  affecting ST cases :
C
C  1/07/91   switched blanket/shield volume calculations to include
C            elongation (about a factor of 2 effect at low A, not much
C            effect at high A).
C  1/08/91   changed intercoil structure algorithm to match that of
C            latest ITER.
C  3/05/91 - 3/07/91  
C            changed confimenet scalings for isc = 1,2 to
C            not use hfactor. changed plasma inductance scaling to
C            fit ITER.
C            fixed PF coil area calculation. chaged PF coil location
C            (for ipfloc = 2) to be more general.
C            Fixed PF coil weight calculation for inner PF coils
C  3/11/91   fix PF coil resistive power. Clean up FOM routine (funfom)
C            and change minmax = 4 chice. fix PF location for
C            ipfloc=2,iohcie=1
C  3/13/91   update density limit model, and put bounds on "omlarg",
C            and "plsep" in the ITER divertor module.
C  3/20/91   changed PF coil current scaling for shaping coil.
C  3/25/91   changed 1st wall area scaling (about 30% bigger now) -
C            doesn't affect wall load, just cost.
C  4/22/91   fix PF power conversion costs, and divertor surface area
C  5/27/91   make unit costs input
C  5/29/91   change power scalings in cost routine to work with no 
C            electric power.
C  5/30/91   make direct cost a FOM (minmax=6) when ireactor = 0
C  5/31/91   mvalim now equal to total mw necessary to run reactor.
C            (inc. ppump)
C  6/03/91   take out complicated old PF coil current scaling, and 
C            simplify
C            location scaling for ipfloc = 3
C  6/04/91   separate Troyon and epsioln beta-Poloidal constraints.
C  6/05/91   fix bug in beam/background fusion. put "beamfus0"
C            multiplier in too. fix B-max OHC bug. 
C  6/06/91   put in "standalone" version of VMCON - no need for nag 
C            routine (STAR3.0)
C  6/07/91   put in capability to scale Troyon factor dnbeta
C            (gtscale /= 0)
C            scale q95 with q, for icurr = 2.
C  6/11/91   change tf bus length , mass , cost scaling for ST's
C
C
C            Work begun by P.C. Shipe
C
C
C  6/26/91 - Began working with STORAC code.  John Galambos
C            transferred it from the CRAY to the IBM RISC 6000
C            workstation.
C
C  7/01/91 - Corrected divtmod.f.  It had several -qdpc changes.
C            (In a -qdpc change, all real*8 variables which have
C            an 'e' are changed to not have an 'e'.  Real*4 
C            variables are changed to have an 'e'.  The reason
C            for this is that the -qdpc switch will correctly
C            promote only real*8 constants and leave real*4
C            constants alone.) 
C
C  7/02/91 - Made one -qdpc change to pfcoil.f.  Changed the name
C            of a common block from coilshp to cshape in tfcoil.h.
C
C  7/03/91 - Made one -qdpc change to physics.f and several changes  
C            of data statements to simple assignments. (These changes
C            were necessary since the variables were in common blocks.)
C            Changed minpac.f.  Changed a few amin1/amax1 to min/max,
C            commented out the rmach data statements which were CRAY
C            specific and made a few -qdpc changes.
C     
C  7/09/91 - Made many data statement changes to costs.f.  Made one
C            data statement change to radialb.f.  Changed 1d2000 to
C            (1d200)**10 in several places in minpac.f.
C
C  7/10/91 - Made a large number of data statement changes to initial.f.
C
C  7/16/91 - Included svd.f as part of the STORAC source code. 
C 
C  7/17/91 - Removed calls to ioscale, timeused and timedate and 
C            uncommented the OPEN statements in aamain.f.
C
C  7/19/91 - Changed ECON.DATA to work on the workstation.  STORAC 
C            finally ran, but gave different results in some cases.
C
C  7/23/91 - Changed back to 1d2000 in minpac.f.
C
C  7/29/91 - Added minstang in divrt.h.  Put minstang=0 in initial.f.
C            Made lamp and minstang real*8 in divrt.h.
C
C  7/30/91 - Deleted real*8 lamp,minstang in divtmod.f.  Changed pslepo
C            to plsepo in divrt.h.  Changed real in divtmod.f to real*8
C            and also deleted 'e' in real*8 constant.
C
C  7/31/91 - Changed 'e's to d's in costs.f.  Transferred assignments
C            from costs.f to initial.f.  Duplicated the iiter run of
C            STORAC. 
C
C  8/01/91 - Backed up /u2/p5l/storac.
C
C  8/05/91 - Worked on a readme file for the storac directory.
C
C  8/07/91 - Ran storac using instasp.  Many discrepancies exist.
C
C  8/08/91 - Included the -C option for array bound checking.  
C            Changed many 1's to *'s in dimension declarations.
C
C  8/12/91 - Got rid of e's in sgefa.  Got rid of real*8 amax in
C            minpac.f.  Changed dmax1 and dmin1 to max and min
C            in minpac.f.  Changed the functions enorm and spmpar 
C            to real*8 and took out all the e's that I could find
C            in minpac.f.  Commented out all implicit none's in
C            minpac.f.
C
C  8/13/91 - Added implicit real*8 (a-h,o-z) in all of minpac.f.
C            Changed more 1's to *'s in dimension declarations in
C            minpac.f.  Changed a lone REAL to REAL*8 in minpac.f.
C            This last change caused instasp to run correctly.
C            John and I went over the output and compared it to the 
C            CRAY version.  It corresponds well.
C
C  8/14/91 - Changed format of the output into ECON.DBG in the
C            aamain.f file.  Backed up the system.
C
C  8/15/91 - Completed rplot.f.
C
C  8/19/91 - Put implicit none back in all of minpac.f.  Backed
C            up system.  Copied over new var.des.  Began changes
C            to bring system up to date:  Added crctcore and
C            ipnet to cost.h in common/cost/.
C
C            After the system was backed up on 8/19/91, it was
C            frozen and we will now begin new modifications.
C
C  8/20/91 - Continued making changes to include files:  Added
C            fhole to fwblsh.h.  Added fgrosbop and ppmphemw to 
C            htpwr.h.  Added fjohc0 to ineq.h.  Changed ipnvars
C            from 40 to 45 and ipeqns from 25 to 30 in param.h.
C            Added rjohc and rjohc0 to pfcoil.h in common/pfcoix/.
C 
C            Began making changes to source files:
C
C            costs.f:
C
C            added      crctcore=c221+c222+c223         l469
C            added      .and.ipnet.eq.0)                l634
C            added      write of crctcore               l762
C            exchanged  fgrosbop for fauxbop            l934
C            edited     format statement                l948
C
C            cudriv.f:
C            
C            added      if(irfcd.eq.0) return           l28
C            removed    1    continue                  
C
C            eqns.f:
C
C            added      INCLUDE 'pfcoil.h'              l18
C            changed    cc(i)=1.+fvs*vstot/vsstt        l88
C            added      four equations for              l157
C                       icc = 25,26,27,28  
C
C            fwbs.f:
C
C            added      INCLUDE 'htpwr.h'               l16
C            added      pnucloss assignments and        l43
C                       modified pneut2 assignments
C            changed    1430 to 2020                    l159
C
C  8/21/91 - Continued making changes to source code: 
C 
C            heatpwr.f:
C
C            added      pnholemw assignment             l45
C                       modified psecht assignment
C            added      stuff under 7/6/91              l57
C                       stuff under 7/5/91 to line 100
C            added      pnholemw                        l77
C            added      write of pnholemw               l90
C            changed    fauxbop to fgrosbop             l97
C
C            initial.f:
C
C            changed    boundl(37) (factor of 10**8)    l65
C            added      boundl(41-45)                   l69
C            changed    boundu(37) (factor of 10**8)    l111
C            added      boundu(41-45)                   l115
C            added      lablxc(37-45)                   l160
C            added      lablcc(25-28)                   l194
C            added      fjohc0=1                        l315
C            added      fhole=0.15                      l479
C            added      ipnet=0                         l504
C
C            input.f:
C
C            added      fjohc0                          l45
C            added      fhole                           l92
C            added      ipnet                           l101
C
C            pfcoil.f:
C
C            replaced   bpf(nohc) with bohci            l342
C                       bpf2(nohc) with bohco
C                       rjpfalw(nohc) with rjohc
C            added      rjohc0=rjpfalw(nohc)            l355
C            replaced   rjpfalw(nohc) with rjohc        l647
C            added      rjohc0                          l648
C            added      rjohc and rjohc0                l655
C
C            optimiz.f:
C
C            changed to fc=sgn*concost/2.e3             l121
C            
C            outplas.f:
C
C            added      INCLUDE 'times.h'               l12
C            added      cfe0                            l74
C            added      cfe0                            l86
C            added      tburn                           l137
C            added      tburn                           l148
C
C            xc.f:
C
C            added      INCLUDE 'pfcoil.h'              l14
C                       INCLUDE 'divrt.h'
C                       INCLUDE 'fwblsh.h'
C            added      if ixc(i).eq.(37-45)            l58
C            added      INCLUDE 'pfcoil.h'              l92
C                       INCLUDE 'divrt.h'
C                       INCLUDE 'fwblsh.h'
C            added      ixc(i).eq.(37-45)               l138
C 
C  8/23/91 - Changed curpfb to curpfs in pfcoil.f line 444.
C            This allowed isu.test to run (it had previously
C            crashed) but not correctly.
C
C            Uncommented 2 lines in pfcoil.f line 344.  Rjohc
C            assignment.
C
C            Changed isu.test.  Took out 10 in ixc assignment.
C            changed nvars=23 to 22.  Isu.test ran succesfully.
C
C            Added pnucloss to fwblsh.h.
C
C            Changed pnholemw to pnucloss in heatpwr.f line 47.
C
C            Deleted lines 44-45 in heatpwr.f.  Pnholemw
C            assignment.
C
C            Changed pnholemw to pnucloss in lines 74 and 87.
C
C            Added a '+' at the end of line 44 in heatpwr.f.
C            This seemed to solve many problems but ipstd2 is
C            still not working quite right.
C
C  8/26/91 - Changed the numeric factor in line 121 of optimiz.f
C            in an attempt to get ipstd2 to work properly.
C
C  8/28/91 - Added an nsweep option 8 in aamain.f to allow sweeps
C            on fqval.  Added INCLUDE's for param.h and numer.h.
C
C  8/29/91 - Changed in aamain.f the sweep assignment from 
C            boundu(45) to fqval.
C
C  9/04/91 - Changed 0 to 1 in line 308 of aamain.f so that all plots 
C            would be plotted as the default.
C 
C  9/06/91 - Changed aamain.f to print out wallmw.
C 
C            Changed xlabels in aamain.f.
C 
C  9/11/91 - Changed real*8 to real in line 210 in aamain.f.  This is 
C            to print out data into ECON.DBG.
C 
C            Added c221 and c222 in cost.h.  Changed aamain.f so that
C            crc=c221 + c222 not crctcore.
C 
C  9/17/91 - In preparing to make the scan of aspect ratio while holding
C            rinboard constant,  the following source code changes were
C            made:  Included rinboard in build.h.  Added equation 29,
C            cc(i)=1.-(rmajor-rminor)/rinboard, to eqns.f.  Checked
C            param.h to verify that ipeqns was high enough.  It was 3 .
C            Added description of rinboard to var.des.  Included
C            rinboard =  .651 in initial.f.  Included rinboard in
C            namelist in input.f.  Added lablcc(29)='Rinboard' in 
C            initial.f.  Added =29 in line 77 of var.des.
C 
C  9/18/91 - Made changes to code which John had compiled.  Multiplied
C            taueemv by hfact in l519 of physics.f.  Changed  
C            (dene*1.e-2 ) to dnla in l526 of physics.f.  Changed q to
C            qstar in l526 of physics.f.  Multiplied taumm by 
C            kappa95** .125 in l527 of physics.f.  Changed 1.e-20*dene
C            to dnla in l6 1 of physics.f  Multiplied tauih by bt**0.15
C            in l639 of physics.f. Included a second option in the if of
C            l32 of pfcoil.f, ncls(ngrp+1)=1.  Removed   as an option
C            of istokpf in l561 of var.des and added 2 which is good 
C            for higher aspect ratios.
C 
C            Made 3 changes which Peter had given me.  Changed ireator 
C            to ireactor in l29 of pwrconv.f.  Added real*8 ltfleg in
C            l286 of tfcoil.f. Added save pfbuspwr in l1 7 of pwrconv.f.
C 
C            Changed the program name from STAR to STORAC.
C 
C            In aamain.f.  Added ifa(1 ),sq(10),ifatemp(10).  Wrote
C            out ifail and sqsumsq.  Changed nam from character*1  to 
C            character*25 and wrote out as a25.
C 
C            Changes to add a new equation and variable.  Added pinjalw
C            to cdriv.h.  Added fpinj to ineq.h.  Added pinjalw=25. to
C            initial.f.  Added pinjalw to input.f.  Added pinjalw to
C            var.des.  Added equation 30, cc(i)=1-fpinj*1.e6*pinjalw/
C            (pinji+pinje).  Changed ipnvars to 50  and ipeqns to 40 in
C            param.h. Added boundu(46)=1, boundl(46)= .001 in initial.f.
C            Added lablcc(30)='Allowable pinj' in initial.f. Added fpinj
C            as variable 46 in 2 places in xc.f. Added fpinj to var.des.
C            Added fpinj=1. in initial.f.  Added lablxc(46) = 'fpinj' to
C            initial.f.  Added =30  Allowable pinj to initial.f.  Added
C            =46 fpinj to initial.f.
C 
C            Changed aamain.f to correct ifail write.
C 
C 
C  9/20/91 - Made some changes to the code to accomodate Sheffield's 
C            run.  Changed rmid=tfcth+bcylth in l37 of tfcoil.f.  
C            Changed amid=pi(rmid**2-bcylth**2) in l4  of tfcoil.f.
C 
C  9/23/91 - Made some changes to the code:  Added feffcd twice in xc.f 
C            as variable 47.  Put in boundu(47)=1.  and boundl(47)=0.001
C            in initial.f, as well as lablxc(47)='feffcd' in initial.f.
C            Added =47 to var.des.  Corrected spelling of fpinj in xc.f
C            and added fpinj to input.f.
C 
C 10/07/91 - Made some changes to the code after attempting to port to
C            to the CRAY F:  Changed the ',\n' to '/'in the write
C            statements in subroutine scan.  Set nplot to 11 and
C            changed all write(7 to write(nplot in subroutine scan.  
C            Removed all link calls and timedate calls in aamain.f.
C            changed input numbers from 5,6,7 to 10,11,12.
C  
C 10/08/91 - Froze the code as version 1.2.  Changed aamain.f to have
C            a header with version 1.2x.  Changed aamain.f to remove
C            a duplicate set of print statements from eqslv.  Changed
C            crymw to crypmw in heatpwr.f.  Added q95 to phydat.h.
C            Added INCLUDE 'cost.h' to pwrconv.f.
C  
C 10/11/91 - Removed format statement 900 from subroutine scan in
C            aamain.f.  Removed \'s  in write statements in
C            subroutine scan.  Removed the real *8 declarator in 
C            front of the function statement for enorm and spmpar.
C            Changed Case 1.0 to STORAC 1.2x in initial.f.
C
C 10/16/91 - Changed initial value of ptempalw to 90 in initial.f
C            and var.des.
C
C 10/23/91 - Changed to version 1.3 of STORAC in initial.f.
C            Archived Storac1.3 and changed code to 1.3x.
C
C 10/25/91 - Overhauled subroutine scan in aamain.f.
C 
C 10/29/91 - Incremented to Storac 1.4.  Added ibetin to namelist
C            phydat in input.f.
C
C 10/30/91 - Put in implicit real*8 (a-h,o-z) in subroutine concoptf
C            of tfcoil.f.  Put in an option for ireactor = 3 so that
C            fc=sgn*capcost/3 in subroutine funfom of optimiz.f.
C            Removed an assignment of ric(nohc) in one place and
C            put it in 2 other places so that the maximum current
C            could be at the beginning of the pulse.
C
C 10/31/91 - Changed subroutine scan in aamain.f to allow for 20 
C            points to be plotted instead of 10.  Changed sweep(10)
C            to sweep(20) in sweep.h.
C
C 11/04/91 - Added hldiv as FOM .  Added INCLUDE divrt.h and assignment
C            of hldiv in optimiz.f.  Added documentation of minmax
C            in var.des.  Added assignment of lablmm in initial.f.
C
C            Added an nsweep = 9 option to scan te in aamain.f.
C            Documented nsweep option in var.des.  Multiplied rmajor
C            by 2 in minmax=1 option in optimiz.f.
C
C 11/06/91 - Added option 10 to nsweep in aamain.f to sweep on
C            boundu(15).  Documented in var.des. 
C
C 11/12/91 - Incremented version number to 1.5.  Changed to 1.5 in 
C            initial.f and in aamain.f.
C
C            Added in subroutine palph of physics.f an assignment of
C            fact2 and multiplied by fact2 in the next line.  Also
C            changed aamain.f so that tlabel was output and so that
C            sweep was inverted when nsweep is 8.
C
C 12/02/91 - Modified acpow.f.  Removed beginning and ending labels,
C            changed real to real*8, changed tlvmw to tlvpmw, changed
C            vacmw to vachtmw, changed pheatwp to pinjwp, changed 
C            cryht to crypmw.
C
C 12/02/91 - Changed acpow.f.  Changed e's to d's in several 
C            assignments, added real*8 declaration of all variables,
C            made bldgvol.h, made heattr.h, made heatrinp.h, made
C            estocom.h, made pfelect.h, put includes in acpow.f and
C            added integer iscenr.
C
C 12/09/91 - Made code changes.  Changed eq 19 in eqns.f 'totmva = 
C            tfcpmw + tflegmw'.  Changed pwrconv.f 'tfbusl = 300'
C            when itfsup.neq.1.  Added option 10 for bt in funfom.
C            Documented in var.des and put in lablmm in initial.f.
C            Changed icase to 1.6 in initial.f
C
C 12/11/91 - Changed icase to 1.7 in initial.f.
C
C 12/16/91 - Added 2 variables rpf1 and rpf2.  Documented in var.des
C            and initialized in initial.f and added to input.f.  Also
C            changed pfcoil.f to modify the radius and height of
C            the PF coils.
C
C 12/18/91 - Changed icase to 1.8 in initial.f.
C
C 12/20/91 - Added pinje+pinji as F.O.M.  Checked that cdriv.h was
C            included in optimiz.f.  Added lablmm(11) to initial.f.   
C            Changed dimension of lablmm in labels.h.  Added
C            documentation to var.des.
C
C 12/23/91 - Changed the minmax of 11 option so that the function
C            is divided by 1.e6.
C 
C 12/27/91 - Changed icase to 1.9 in initial.f.
C
C 01/07/92 - Added correct calling sequence to acpow.f and implicit
C            double precision.
C
C            Added to var.des, Bldgvol, Heattr, Heatrinp, Estocom
C            and Pfelect.
C
C 01/08/92 - Code changes: Added includes and assignments to initial.f.
C            Added include of pfelect.h to pwrcom.f so that peakmva
C            could be passed.  Added includes, namelist, assignments
C            and reads to input.f.  Added call to acpow in caller.
C            Added new includes and acpow.f in makefile.  Added call
C            to acpow in output.f.  Added output write statements in
C            acpow.f.
C
C 01/09/92 - Added write statements of the input to acpow.f.
C
C 01/10/92 - Added some documentation to acpow.f. Deleted declarations.
C
C            Changed pfcoil.f, added *signn(k) in line 97.
C
C            Changed acpow.f.  Removed *1.d-6 from crymw assignment.
C
C            Changed icase to 2.0 in initial.f.
C
C 01/13/92 - Modified beams.f.  Included pwpnb in cdriv.h.  Documented
C            pwpnb in var.des.  Called nbeam from caller and output.
C
C            Changed icase from 2.0 to 2.1 in initial.f.
C
C 01/14/92 - Modified beams.f.  Changed 1.e3 to 1.d3.
C
C            Added documentation to beams.f.  Changed call to nbeam in
C            caller.f and output.f.  
C
C            Modified documentation in acpow.f.
C
C            Modified ech.f to include in storac.
C
C 01/15/92 - Put ech in caller.f and in output.f.  
C
C            Changed 2.1 to 2.2 in initial.f.
C
C            Included pwplh in cdriv.h.  Put pwplh in var.des. 
C            Modified lwhymod.f to include in storac.  Called
C            lwhymod from caller.f and from output.f.
C
C 01/21/92 - Created bldgcom.h.  Put those variables in var.des.
C            Put these variables in initial.f.  Put them in a 
C            namelist and read in input.f.  Worked on modifying
C            subroutine bldgs.
C
C 01/22/92 - Added the additional variables from subroutine
C            bldgcall to include files build, fwblsh, times and
C            bldgvol.  Added these variables to var.des.
C
C            Modified subroutine bldgcall.
C
C            Put calls to bldgcall in caller and in output.
C
C 01/23/92 - Removed rbvol from reactcmp.f.
C
C            Added rbvol to bldgvol.h, removed it from rctcmp.h.
C
C            Added include of bldgvol.h in costs.f.
C
C 01/28/92 - Worked on including struct.f:  Included variables in
C            divrt.h and fwblsh.h.  Created struccom.h.  Took 
C            coldmass out of rctcmp.h.  Added includes of struccom
C            in bldgs.f,heatpwr.f and reactcmp.f.  Updated var.des.
C            Modified struct.f.  Put wtbc in tfcoil.h and in var.des.
C            
C            Changed icase from 2.4 to 2.5 in initial.f.
C
C 02/03/92 - Made changes to include vacuum.f:  Included tdwell in
C            times.h, var.des, initial.f and input.f.  Created  
C            torsdat.h and put in variables identified in vaccall
C            trace.  Also included these in var.des.  Added torsdat
C            to input.f.  Created vaccom.h and added these variables
C            to var.des,initial.f and input.f.  Added includes of
C            struccom.h, torsdat.h and vaccom.h to initial.f and to
C            input.f.  Modified vacuum.f. 
C
C            Changed icase from 2.5 to 2.6 in initial.f.
C
C 02/06/92 - Modified code to include supercond.f:  Included variables
C            from supercond trace in tfcoil.h.  Put these variables in
C            var.des.  Included 6 of these in initial.f and input.f.
C            Modified supercond.f.
C
C            Put calls in caller and in output.
C
C            Changed icase to 2.7 in initial.f.
C
C 02/18/92 - Began making code changes to include sctfcoil.f and 
C            math2.f into STORAC:  Put hr1 in build.h and var.des.
C            Corrected documentation of rtfcin in var.des.  Added 43  
C            variables to tfcoil.h and to var.des.  Added 17 of these
C            to initial.f and input.f.
C
C 02/19/92 - Modified tfcoil.h.  Added mgnt to tfcoil.h, var.des,
C            initial.f and input.f.  Modified sctfcoil.f and math2.f.
C            Changed the makefile.
C
C            The program crashed at first.  One mistake was that a 0
C            was used as a acontinuation character.
C
C
C 02/20/92 - Continued to debug STORAC:  Forgot implicit none in
C            math2.f.  Mispelled implicit and precision.  Declared
C            igo integer.  Changed the pi data statement to an
C            assignment in stresscall and a line in sctfcoil went
C            over 72 characters after variable names were changed.
C
C            Made other changes in math2 and sctfcoil, mostly
C            changing 1's to *'s and typing functions.
C
C 02/21/92 - Removed what used to be jcontf from sctfcoil.  Put 
C            cpttf in initial and in input.  Changed whtcastf in 
C            costs.f.  Put in rtfcin assignment.
C
C            Talked to John and tweaked IN and initial.f, as well
C            as adding variable tfootfi in order to duplicate 
C            Tetra results.
C
C            Made a correction to vmcon in minpac.f.
C
C 02/24/92 - Tracked down prtszreq problem and added an arealeg   
C            assignment in sctfcoil.f.
C
C 02/25/92 - Made changes to code to include equations 31-36 and
C            variables 48-60.
C
C 02/26/92 - Incremented from 2.8 to 2.9.
C
C 02/28/92 - Began on 02/27/92 modifying costs.f.  Changed the 
C            following sections:  Structure, reactor, magnets,
C            vacuum,power conditioning,fuel handling,instrumentation
C            and control,maintenance and electrical.
C
C 03/04/92 - Modified fwbs.f in benchmark of costs.f
C
C            Changed the name of STORAC to PROCESS. 
C
C 03/10/92 - Changed from 2.9 to 3.0.
C
C            Added the tfcpwr routine from Tetra's pwrconv.f.
C
C            Changed the power conditioning section of costs.f.
C
C 03/11/92 - Changed 3.0 to 3.1.
C  
C 03/12/92 - Modified heatpwr.f and installed a new heat transport
C            section to costs.f.
C
C 03/16/92 - Incremented to PROCESS3.2.
C
C            Included the new pfscl.f in PROCESS.
C
C 03/18/92 - Found the bug in pfscl.f.  Subroutine bfield was 
C            not identified as implicit none or otherwise.
C
C 03/19/92 - Incremented to PROCESS3.3.
C
C            Solved negative energy problem.  Small change in 
C            pfcoil.f.
C
C 03/23/92 - Changed the magnet portion of costs some.
C
C 03/25/92 - Finished documentation of the benchmark of costs.  
C
C            Incremented to PROCESS3.4.
C
C 03/26/92 - Incremented to PROCESS3.5.
C
C            Removed reactcmp etc. and second printing of TFC stuff.
C
C 04/03/92 - Worked on a few of the checklist items.
C
C 04/06/92 - Modified output to file OUT.
C
C 04/15/92 - Added two nsweep scan options to aamain.f.
C   
C
C
C
C
C
C+**PJK 02/11/92 This file has not been kept up-to-date, as a
C+**PJK 02/11/92 Project Work File exists which contains all the
C+**PJK 02/11/92 relevant detail.

      END
