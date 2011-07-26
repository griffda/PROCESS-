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
C  Module         : $Id: eqns.f,v 3.17 1998/04/01 11:54:43 peter Exp pknight $
C
C  Module name    : $RCSfile: eqns.f,v $
C  Version no.    : $Revision: 3.17 $
C
C  Creation date  : $Date: 1998/04/01 11:54:43 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      SUBROUTINE CON1(m,cc)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.101
C
C--Description
C  Routine that formulates the constraint equations.
C  The code attempts to make cc(i) = 0 for all i=1 to m equations.
C  All relevant consistency equations should be active in order
C  to make a self-consistent machine.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  25 July 2011
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  01/07/94 PJK 1.000 Improved layout and added stellarator constraints
C  08/12/94 PJK 1.010 Added stellarator radial build consistency
C  27/02/96 PJK 1.020 Added rfp equation 47.
C  07/10/96 PJK 1.030 Added ICULBL=2 option to constraint no.24
C  21/03/97 PJK 1.040 Added IFE equation 50.
C  01/04/98 PJK 1.100 Modified equations 2,3,4,7 and 28 to take into
C                     account IGNITE (ignition switch) setting
C  25/07/11 PJK 1.101 Applied Greenwald density limit to line-averaged
C                     rather than volume-averaged density
C
C--Arguments
C  m      : (INPUT)  Number of constraint equations
C  cc(i)  : (OUTPUT) Residual error in equation i
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'phydat.h'
      INCLUDE 'ineq.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'divrt.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'pulse.h'
      INCLUDE 'times.h'
      INCLUDE 'stella.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'

C  Arguments
      INTEGER m
      DOUBLE PRECISION cc(m)

C  Local variables
      DOUBLE PRECISION cratmx,tcycle,totmva,acoil
      INTEGER i

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      do 100 i = 1,m

C *** Relationship between beta, temperature and density
C *** This is a consistency equation.

         if (icc(i).eq.1) then
            cc(i) = 1.0D0 - (betaft + betanb + 3.204D-16
     +           * rmu0 * (dene*ten + dnitot*tin)/btot**2 )/beta

C *** Global plasma power balance equation
C *** This is a consistency equation.

         else if (icc(i).eq.2) then
            if (ignite.eq.0) then
               cc(i) = 1.0D0 - dign*(ptre+ptri+prad)/( falpha*palp
     +              + pcharge + pohmpv + 1.0D-6*(pinji+pinje)/vol )
            else
               cc(i) = 1.0D0 - dign*(ptre+ptri+prad)/
     +              (falpha*palp + pcharge + pohmpv)
            end if

C *** Global power balance equations for ions and electrons separately

         else if (icc(i).eq.3) then
            if (ignite.eq.0) then
               cc(i) = 1.0D0 - dign*(ptri+pie)/
     +              ( falpha*palpi + 1.0D-6*pinji/vol )
            else
               cc(i) = 1.0D0 - dign*(ptri+pie)/(falpha*palpi)
            end if

         else if (icc(i).eq.4) then
            if (ignite.eq.0) then
               cc(i) = 1.0D0 - dign*(ptre+prad)/
     +              ( (falpha*palpe)+pie + 1.0D-6*pinje/vol )
            else
               cc(i) = 1.0D0 - dign*(ptre+prad)/( (falpha*palpe)+pie )
            end if

C *** Equation for density limit

         else if (icc(i).eq.5) then
C+**PJK 25/07/11 Apply Greenwald limit to line-averaged density
            if (idensl.eq.7) then
               cc(i) = 1.0D0 - dnla/(fdene*dnelimt)
            else
               cc(i) = 1.0D0 - dene/(fdene*dnelimt)
            end if

C *** Equation for epsilon beta-poloidal limit

         else if (icc(i).eq.6) then
            cc(i) = 1.0D0 - eps*betap/(fbeta*epbetmax)

C *** Equation for hot beam ion density
C *** This is a consistency equation (NBI).

         else if (icc(i).eq.7) then
            if (ignite.eq.0) then
               cc(i) = 1.0D0 - dnbeam2/dnbeam
            else
               WRITE(*,*) 'Error in routine CON1:'
               WRITE(*,*) 'Do not use constraint 7 if IGNITE=1.'
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            end if

C *** Equation for neutron wall load limit

         else if (icc(i).eq.8)  then
            cc(i) = 1.0D0 - wallmw/(fwalld*walalw)

C *** Equation for fusion power limit

         else if (icc(i).eq.9) then
            cc(i) = 1.0D0 - ffuspow * powfmax / powfmw

C *** Equation for field at TF coil
C *** This is a consistency equation.

         else if (icc(i).eq.10) then
            cc(i) = 1.0D0 - (rmajor*bt)/(rbmax*bmaxtf)

C *** Equation for radial build
C *** This is a consistency equation.

         else if (icc(i).eq.11) then
            cc(i) = 1.0D0 - rbld/rmajor

C *** Equation for volt-second capability

         else if (icc(i).eq.12) then
            cc(i) = 1.0D0 + fvs * vstot/vsstt

C *** Equation to limit minimum burn time

         else if (icc(i).eq.13) then
            cc(i) = 1.0D0 - ftburn * tburn/tbrnmn

C *** Equation for beam energy

         else if (icc(i).eq.14) then
            cc(i) = 1.0D0 - taubeam/tbeamin

C+**PJK 30/03/94 Equation for burn time consistency

         else if (icc(i).eq.15) then
            cc(i) = 1.0D0 - tburn0/tburn

C *** Equation for net electric power

         else if (icc(i).eq.16) then
            cc(i) = 1.0D0 - fpnetel * pnetelmw / pnetelin

C *** Equation for stellarator radial build consistency

         else if (icc(i).eq.17) then
            acoil = hmax + 0.5D0*tfcth
            cc(i) = 1.0D0 - acoil / (2.6D0*rminor)

C *** Equation for divertor heat load

         else if (icc(i).eq.18) then
            cc(i) = 1.0D0 - fhldiv * hldivlim/hldiv

C *** Equation for MVA limit

         else if (icc(i).eq.19) then
            totmva = tfcpmw + tflegmw
            cc(i) = 1.0D0 - fmva * mvalim/totmva

C *** Equation for port size

         else if (icc(i).eq.20) then
            cc(i) = 1.0D0 - prtszreq/( fportsz * prtsz)

C *** Equation for minimum minor radius

         else if (icc(i).eq.21) then
            cc(i) = 1.0D0 - frminor * rminor/aplasmin

C *** Equation for divertor collisionality

         else if (icc(i).eq.22) then
            cc(i) = 1.0D0 - fdivcol * rlenmax / rlclolcn

C *** Equation for allowable TF Coil current density

         else if (icc(i).eq.23) then
            cc(i) = 1.0D0 - fjtfc * rjtfsual / oacdcp

C *** Equation for beta limit (Troyon limit in tokamaks)

         else if (icc(i).eq. 24) then

            if ((iculbl.eq.0).or.(istell.ne.0)) then

C ***          Include all beta components
C+**PJK 01/07/94 Relevant for both tokamaks and stellarators

               cc(i) = 1.0D0 - beta / (fbetatry*betalim)

            else if (iculbl.eq.1) then

C+**PJK 03/06/92 Added coding for new Troyon beta limit algorithm.
C+**PJK 03/06/92 Here, the beta limit applies to only the thermal
C+**PJK 03/06/92 component, not the fast alpha or neutral beam parts.

               cc(i) = 1.0D0 - (beta-betaft-betanb) / (fbetatry*betalim)

            else if (iculbl.eq.2) then

C+**PJK 07/10/96 Troyon beta limit applies to thermal + neutral beam
C+**PJK 07/10/96 components of the total beta, i.e. excludes alphas

               cc(i) = 1.0D0 - (beta-betaft) / (fbetatry*betalim)

            else
               WRITE(*,*) 'Error in routine CON1:'
               WRITE(*,*) 'Illegal value for ICULBL, = ',iculbl
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            end if

C *** Equation for peak toroidal field limit

         else if (icc(i).eq.25) then
            cc(i) = 1.0D0 - bmaxtf/(fpeakb*bmxlim)

C *** Equation for J-OHC at EOF

         else if (icc(i).eq.26) then
            cc(i) = 1.0D0 - fjohc*rjohc/coheof

C *** Equation for J-OHC at BOP

         else if (icc(i).eq.27) then
            cc(i) = 1.0D0 - fjohc0 * rjohc0/cohbop

C *** Equation for Big Q

         else if (icc(i).eq.28) then
            if (ignite.eq.0) then
               cc(i) = 1.0D0 - fqval * powfmw / (1.0D-6 * (pinji+pinje))
            else
               WRITE(*,*) 'Error in routine CON1:'
               WRITE(*,*) 'Do not use constraint 28 if IGNITE=1.'
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            end if

C *** Equation for inboard major radius

         else if (icc(i).eq.29) then
            cc(i) = 1.0D0 - (rmajor - rminor) / rinboard

C *** Equation to limit injection power

         else if (icc(i).eq.30) then
            cc(i) = 1.0D0 - fpinj * 1.0D6 * pinjalw / (pinji + pinje)

C *** Equation to limit TFC case stress (SCTF)

         else if (icc(i).eq.31) then
            cc(i) = 1.0D0 - fstrcase * alstrtf/strtf2

C *** Equation to limit TFC conduit stress (SCTF)

         else if (icc(i).eq.32) then
            cc(i) = 1.0D0 - fstrcond * alstrtf/strtf1

C *** Equation to limit iop/icrit (SCTF)

         else if (icc(i).eq.33) then
            cc(i) = 1.0D0 - fiooic * jwdgcrt/jwptf

C *** Equation to limit dump voltage (SCTF)

         else if (icc(i).eq.34) then
            cc(i) = 1.0D0 - fvdump * vdalw/vtfskv

C *** Equation to limit J_wp/J_prot (SCTF)

         else if (icc(i).eq.35) then
            cc(i) = 1.0D0 - fjprot * jwdgpro/jwptf

C *** Equation to limit temp margin (SCTF)

         else if (icc(i).eq.36) then
            cc(i) = 1.0D0 - ftmargtf * tmargtf/tmargmin

C+**PJK 04/12/92 Equation to limit current drive gamma

         else if (icc(i).eq.37) then
            cc(i) = 1.0D0 - fgamcd * gammax/gamcd

C+**PJK 09/11/93 Equation to limit coolant temperature rise
C+**PJK 09/11/93 in first wall

         else if (icc(i).eq.38) then
            cc(i) = 1.0D0 - fdtmp * dtmpmx/tmprse

C+**PJK 09/11/93 Equation to limit temperature in first wall

         else if (icc(i).eq.39) then
            if (tpeak.eq.0.0D0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'tpeak = 0, indicating that the pulsed'
               write(*,*) 'reactor option is not turned on.'
               write(*,*) 'Do not use constraint 39 if lpulse=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            cc(i) = 1.0D0 - ftpeak * tpkmax/tpeak

C+**PJK 09/11/93 Equation to limit minimum auxiliary power

         else if (icc(i).eq.40) then
            cc(i) = 1.0D0 - fauxmn * 1.0D-6 * (pinje+pinji)/auxmin

C+**PJK 10/11/93 Equation to limit minimum OH swing time

         else if (icc(i).eq.41) then
            cc(i) = 1.0D0 - ftohs * tohs/tohsmn

C+**PJK 15/11/93 Equation to limit minimum cycle time

         else if (icc(i).eq.42) then
            if (tcycmn.eq.0.0D0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'tcycmn = 0, indicating that the pulsed'
               write(*,*) 'reactor option is not turned on.'
               write(*,*) 'Do not use constraint 42 if lpulse=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            tcycle = tramp + tohs + theat + tburn + tqnch + tdwell
            cc(i) = 1.0D0 - ftcycl * tcycle/tcycmn

C+**PJK 29/01/96
C *** Equation for average centerpost temperature
C *** This is a consistency equation (TART).

         else if (icc(i).eq.43) then
            if (itart.eq.0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'Do not use constraint 43 if itart=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            cc(i) = 1.0D0 - tcpav/tcpav2

C+**PJK 29/01/96
C *** Equation for peak centerpost temperature limit

         else if (icc(i).eq.44) then
            if (itart.eq.0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'Do not use constraint 44 if itart=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            cc(i) = 1.0D0 - fptemp * ptempalw / tcpmax

C+**PJK 29/01/96
C *** Equation for edge safety factor limit (TART)

         else if (icc(i).eq.45) then
            if (itart.eq.0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'Do not use constraint 45 if itart=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            cc(i) = 1.0D0 - fq*q/qlim

C+**PJK 30/01/96
C *** Equation for Ip/Irod limit (TART)
C *** This is a q-edge type limit for certain aspect ratios

         else if (icc(i).eq.46) then
            if (itart.eq.0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'Do not use constraint 46 if itart=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
C ***           Exclude cases which are not valid
C ***           if ((ABS(kappa95-2.3D0).gt.1.0D-6).or.
C ***   +           (aspect.lt.1.099D0).or.(aspect.gt.1.601D0)) then
C ***             write(*,*) ' '
C ***             write(*,*) 'kappa95 = ',kappa95
C ***             write(*,*) ' aspect = ',aspect
C ***             write(*,*) ' '
C ***             write(*,*) 'Error in CON1 :'
C ***             write(*,*) 'Constraint eqn.24 should not be'
C ***             write(*,*) 'used for this situation.'
C ***             write(*,*) 'PROCESS stopping.'
C ***             STOP
C ***          end if

C *** Maximum ratio of plasma current to centrepost current.
            cratmx = 1.0D0 + 4.91D0*(eps-0.62D0)
            cc(i) = 1.0D0 - fipir * cratmx * ritfc/plascur

C+**PJK 28/02/96
C *** Equation for maximum TF coil toroidal thickness (tftort)
C *** Relevant only to reversed field pinch devices

         else if (icc(i).eq.47) then
            if (irfp.eq.0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'Do not use constraint 47 if irfp=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            cc(i) = 1.0D0 - frfptf*(2.0D0*(rbmax-tfcth)*tan(pi/tfno))
     +           /tftort

C+**PJK 06/03/96
C *** Equation for poloidal beta limit

         else if (icc(i).eq.48) then
            cc(i) = 1.0D0 - fbetap*betpmx/betap

C+**PJK 06/03/96
C *** Equation to ensure RFP reversal parameter F is negative

         else if (icc(i).eq.49) then
            cc(i) = 1.0D0 + frfpf*rfpf/0.001D0

C+**PJK 21/03/97
C *** Equation for maximum IFE repetition rate (reprat)
C *** Relevant only to inertial fusion energy devices

         else if (icc(i).eq.50) then
            if (ife.eq.0) then
               write(*,*) 'Error in routine EQNS:'
               write(*,*) 'Do not use constraint 50 if ife=0.'
               write(*,*) 'PROCESS stopping.'
               STOP
            end if
            cc(i) = 1.0D0 - frrmax*rrmax/reprat

         else
            WRITE(*,*) 'Error in routine CON1:'
            WRITE(*,*) 'No such equation number as ',icc(i)
            WRITE(*,*) 'PROCESS stopping.'
            STOP

         end if

C *** Crude method of catching NaN errors

         if (abs(cc(i)).gt.9.99D99) then
            WRITE(*,*) 'Error in routine CON1:'
            WRITE(*,*) 'NaN error for constraint equation ',icc(i)
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         end if

 100  continue

      return
      end
