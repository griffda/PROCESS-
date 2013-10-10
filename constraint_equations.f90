!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine constraints(m,cc)

  !+ad_name  constraints
  !+ad_summ  Routine that formulates the constraint equations
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  m : input integer : Number of constraint equations
  !+ad_args  cc(m) : output real array : Residual error in equation i
  !+ad_desc  This routine formulates the constraint equations.
  !+ad_desc  The code attempts to make cc(i) = 0 for all i=1 to m equations.
  !+ad_desc  All relevant consistency equations should be active in order
  !+ad_desc  to make a self-consistent machine.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  pulse_variables
  !+ad_call  rfp_variables
  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  01/07/94 PJK Improved layout and added stellarator constraints
  !+ad_hist  08/12/94 PJK Added stellarator radial build consistency
  !+ad_hist  27/02/96 PJK Added rfp equation 47.
  !+ad_hist  07/10/96 PJK Added ICULBL=2 option to constraint no.24
  !+ad_hist  21/03/97 PJK Added IFE equation 50.
  !+ad_hist  01/04/98 PJK Modified equations 2,3,4,7 and 28 to take into
  !+ad_hisc               account IGNITE (ignition switch) setting
  !+ad_hist  25/07/11 PJK Applied Greenwald density limit to line-averaged
  !+ad_hisc               rather than volume-averaged density
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  29/10/12 PJK Added pf_power_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added constraint_variables
  !+ad_hist  31/10/12 PJK Added stellarator_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_hist  05/11/12 PJK Added pulse_variables
  !+ad_hist  06/11/12 PJK Renamed routine from con1 to constraints,
  !+ad_hisc               and the source file itself from eqns.f90 to
  !+ad_hisc               constraint_equations.f90
  !+ad_hist  17/12/12 PJK Eqn 30 inverted to prevent problems if pinj=0;
  !+ad_hisc               Added new eqn 51, plus debug lines
  !+ad_hist  23/01/13 PJK Allowed eqn.47 to be used for stellarators
  !+ad_hist  04/06/13 PJK Added fwbs_variables, eqns 52-55
  !+ad_hist  11/06/13 PJK Changed wording for eqn 41; added note about
  !+ad_hisc               dign range
  !+ad_hist  18/06/13 PJK Removed dign from power balance equations
  !+ad_hist  27/06/13 PJK Removed Troyon as a beta limit descriptor
  !+ad_hist  25/09/13 PJK Modified eqn.20 from port size limit to
  !+ad_hisc               neutral beam tangency radius limit
  !+ad_hist  30/09/13 PJK Added new eqn 56
  !+ad_hist  10/10/13 PJK Made multiplier in beta equation explicit
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use constraint_variables
  use current_drive_variables
  use divertor_variables
  use fwbs_variables
  use heat_transport_variables
  use ife_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use pulse_variables
  use rfp_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables

  implicit none

  !  Arguments

  integer, intent(in) :: m
  real(kind(1.0D0)), dimension(m), intent(out) :: cc

  !  Local variables

  integer :: i
  real(kind(1.0D0)) :: cratmx, tcycle, totmva, acoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,m

     select case (icc(i))

     case (1)  !  Relationship between beta, temperature (keV) and density
               !  This is a consistency equation.

        cc(i) = 1.0D0 - (betaft + betanb + 2.0D3*rmu0*echarge &
             * (dene*ten + dnitot*tin)/btot**2 )/beta

     case (2)  !  Global plasma power balance equation
               !  This is a consistency equation.

        if (ignite == 0) then
           cc(i) = 1.0D0 - (ptre+ptri+prad)/( falpha*palp &
                + pcharge + pohmpv + 1.0D-6*(pinji+pinje)/vol )
        else
           cc(i) = 1.0D0 - (ptre+ptri+prad)/ &
                (falpha*palp + pcharge + pohmpv)
        end if

     case (3)  !  Global power balance equation for ions

        if (ignite == 0) then
           cc(i) = 1.0D0 - (ptri+pie)/ &
                ( falpha*palpi + 1.0D-6*pinji/vol )
        else
           cc(i) = 1.0D0 - (ptri+pie)/(falpha*palpi)
        end if

     case (4)  !  Global power balance equation for electrons

        if (ignite == 0) then
           cc(i) = 1.0D0 - (ptre+prad)/ &
                ( (falpha*palpe)+pie + 1.0D-6*pinje/vol )
        else
           cc(i) = 1.0D0 - (ptre+prad)/( (falpha*palpe)+pie )
        end if

     case (5)  !  Equation for density limit

        if (idensl == 7) then  !  Apply Greenwald limit to line-averaged density
           cc(i) = 1.0D0 - dnla/(fdene*dnelimt)
        else
           cc(i) = 1.0D0 - dene/(fdene*dnelimt)
        end if

     case (6)  !  Equation for epsilon beta-poloidal limit

        cc(i) = 1.0D0 - eps*betap/(fbeta*epbetmax)

     case (7)  !  Equation for hot beam ion density
               !  This is a consistency equation (NBI).

        if (ignite == 0) then
           cc(i) = 1.0D0 - dnbeam2/dnbeam
        else
           write(*,*) 'Error in routine CONSTRAINTS:'
           write(*,*) 'Do not use constraint 7 if IGNITE=1.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if

     case (8)  !  Equation for neutron wall load limit

        cc(i) = 1.0D0 - wallmw/(fwalld*walalw)

     case (9)  !  Equation for fusion power limit

        cc(i) = 1.0D0 - ffuspow * powfmax / powfmw

     case (10)  !  Equation for field at TF coil
                !  This is a consistency equation.

        cc(i) = 1.0D0 - (rmajor*bt)/(rbmax*bmaxtf)

     case (11)  !  Equation for radial build
                !  This is a consistency equation.

        cc(i) = 1.0D0 - rbld/rmajor

     case (12)  !  Equation for volt-second capability

        cc(i) = 1.0D0 + fvs * vstot/vsstt

     case (13)  !  Equation to limit minimum burn time

        cc(i) = 1.0D0 - ftburn * tburn/tbrnmn

     case (14)  !  Equation for beam energy

        cc(i) = 1.0D0 - taubeam/tbeamin

     case (15)  !  Equation for burn time consistency

        cc(i) = 1.0D0 - tburn0/tburn

     case (16)  !  Equation for net electric power

        cc(i) = 1.0D0 - fpnetel * pnetelmw / pnetelin

     case (17)  !  Equation for stellarator radial build consistency

        acoil = hmax + 0.5D0*tfcth
        cc(i) = 1.0D0 - acoil / (2.6D0*rminor)

     case (18)  !  Equation for divertor heat load

        cc(i) = 1.0D0 - fhldiv * hldivlim/hldiv

     case (19)  !  Equation for MVA limit

        totmva = tfcpmw + tflegmw
        cc(i) = 1.0D0 - fmva * mvalim/totmva

     case (20)  !  Equation for neutral beam tangency radius limit

        cc(i) = 1.0D0 - rtanbeam/(fportsz * rtanmax)

     case (21)  !  Equation for minimum minor radius

        cc(i) = 1.0D0 - frminor * rminor/aplasmin

     case (22)  !  Equation for divertor collisionality

        cc(i) = 1.0D0 - fdivcol * rlenmax / rlclolcn

     case (23)  !  Equation for allowable TF Coil current density
        !  This equation is redundant... pre-1992!

        write(*,*) 'Constraint equation 23 is redundant -'
        write(*,*) 'please use eqns.33 and/or 35 instead.'
        write(*,*) 'PROCESS stopping'
        stop

     case (24)  !  Equation for beta limit

        if ((iculbl == 0).or.(istell /= 0)) then

           !  Include all beta components
           !  Relevant for both tokamaks and stellarators

           cc(i) = 1.0D0 - beta / (fbetatry*betalim)

        else if (iculbl == 1) then

           !  Here, the beta limit applies to only the thermal
           !  component, not the fast alpha or neutral beam parts

           cc(i) = 1.0D0 - (beta-betaft-betanb) / (fbetatry*betalim)

        else if (iculbl == 2) then

           !  Beta limit applies to thermal + neutral beam
           !  components of the total beta, i.e. excludes alphas

           cc(i) = 1.0D0 - (beta-betaft) / (fbetatry*betalim)

        else
           write(*,*) 'Error in routine CONSTRAINTS:'
           write(*,*) 'Illegal value for ICULBL, = ',iculbl
           write(*,*) 'PROCESS stopping.'
           stop
        end if

     case (25)  !  Equation for peak toroidal field limit

        cc(i) = 1.0D0 - bmaxtf/(fpeakb*bmxlim)

     case (26)  !  Equation for J-OHC at EOF

        cc(i) = 1.0D0 - fjohc*rjohc/coheof

     case (27)  !  Equation for J-OHC at BOP

        cc(i) = 1.0D0 - fjohc0 * rjohc0/cohbop

     case (28)  !  Equation for Big Q

        if (ignite == 0) then
           cc(i) = 1.0D0 - fqval * powfmw / (1.0D-6 * (pinji+pinje))
        else
           write(*,*) 'Error in routine CONSTRAINTS:'
           write(*,*) 'Do not use constraint 28 if IGNITE=1.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if

     case (29)  !  Equation for inboard major radius

        cc(i) = 1.0D0 - (rmajor - rminor) / rinboard

     case (30)  !  Equation to limit injection power

        !  Inverted from usual form to prevent problems
        !  with zero injected power

        cc(i) = 1.0D0 - (pinji + pinje) / (fpinj * 1.0D6 * pinjalw)

     case (31)  !  Equation to limit TFC case stress (SCTF)

        cc(i) = 1.0D0 - fstrcase * alstrtf/strtf2

     case (32)  !  Equation to limit TFC conduit stress (SCTF)

        cc(i) = 1.0D0 - fstrcond * alstrtf/strtf1

     case (33)  !  Equation to limit iop/icrit (SCTF)

        cc(i) = 1.0D0 - fiooic * jwdgcrt/jwptf

     case (34)  !  Equation to limit dump voltage (SCTF)

        cc(i) = 1.0D0 - fvdump * vdalw/vtfskv

     case (35)  !  Equation to limit J_wp/J_prot (SCTF)

        cc(i) = 1.0D0 - fjprot * jwdgpro/jwptf

     case (36)  !  Equation to limit temp margin (SCTF)

        cc(i) = 1.0D0 - ftmargtf * tmargtf/tmargmin

     case (37)  !  Equation to limit current drive gamma

        cc(i) = 1.0D0 - fgamcd * gammax/gamcd

     case (38)  !  Equation to limit coolant temperature rise
                !  in first wall

        cc(i) = 1.0D0 - fdtmp * dtmpmx/tmprse

     case (39)  !  Equation to limit temperature in first wall

        if (tpeak == 0.0D0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'tpeak = 0, indicating that the pulsed'
           write(*,*) 'reactor option is not turned on.'
           write(*,*) 'Do not use constraint 39 if lpulse=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cc(i) = 1.0D0 - ftpeak * tpkmax/tpeak

     case (40)  !  Equation to limit minimum auxiliary power

        cc(i) = 1.0D0 - fauxmn * 1.0D-6 * (pinje+pinji)/auxmin

     case (41)  !  Equation to limit minimum plasma current ramp-up time

        cc(i) = 1.0D0 - ftohs * tohs/tohsmn

     case (42)  !  Equation to limit minimum cycle time

        if (tcycmn == 0.0D0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'tcycmn = 0, indicating that the pulsed'
           write(*,*) 'reactor option is not turned on.'
           write(*,*) 'Do not use constraint 42 if lpulse=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        tcycle = tramp + tohs + theat + tburn + tqnch + tdwell
        cc(i) = 1.0D0 - ftcycl * tcycle/tcycmn

     case (43)  !  Equation for average centerpost temperature
                !  This is a consistency equation (TART).

        if (itart == 0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 43 if itart=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cc(i) = 1.0D0 - tcpav/tcpav2

     case (44)  !  Equation for peak centerpost temperature limit

        if (itart == 0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 44 if itart=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cc(i) = 1.0D0 - fptemp * ptempalw / tcpmax

     case (45)  !  Equation for edge safety factor limit (TART)

        if (itart == 0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 45 if itart=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cc(i) = 1.0D0 - fq*q/qlim

     case (46)  !  Equation for Ip/Irod limit (TART)
                !  This is a q-edge type limit for certain aspect ratios

        if (itart == 0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 46 if itart=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if

        !  Maximum ratio of plasma current to centrepost current
        cratmx = 1.0D0 + 4.91D0*(eps-0.62D0)
        cc(i) = 1.0D0 - fipir * cratmx * ritfc/plascur

     case (47)  !  Equation for maximum TF coil toroidal thickness (tftort)
                !  Relevant only to reversed field pinch or stellarator devices

        if ((irfp == 0).and.(istell == 0)) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 47 if irfp=0 and istell=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cc(i) = 1.0D0 - frfptf*(2.0D0*(rbmax-tfcth)*tan(pi/tfno)) &
             / tftort

     case (48)  !  Equation for poloidal beta limit

        cc(i) = 1.0D0 - fbetap*betpmx/betap

     case (49)  !  Equation to ensure RFP reversal parameter F is negative

        cc(i) = 1.0D0 + frfpf*rfpf/0.001D0

     case (50)  !  Equation for maximum IFE repetition rate (reprat)
                !  Relevant only to inertial fusion energy devices

        if (ife == 0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 50 if ife=0.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cc(i) = 1.0D0 - frrmax*rrmax/reprat

     case (51)  !  Equation to enforce startup flux = available startup flux

        cc(i) = 1.0D0 - (vsres+vsind) / vssu

     case (52)  !  Equation to limit minimum tritium breeding ratio

        cc(i) = 1.0D0 - ftbr * tbr/tbrmin

     case (53)  !  Equation for fast neutron fluence on TF coil limit

        cc(i) = 1.0D0 - fflutf * nflutfmax/nflutf

     case (54)  !  Equation for peak TF coil nuclear heating limit

        cc(i) = 1.0D0 - fptfnuc * ptfnucmax/ptfnucpm3

     case (55)  !  Equation for final helium concentration in vacuum vessel limit

        cc(i) = 1.0D0 - fvvhe * vvhealw/vvhemax

     case (56)  !  Equation for power through separatrix / major radius limit

        cc(i) = 1.0D0 - fpsepr * pseprmax / (pdivt/rmajor)

     case default

        write(*,*) 'Error in routine CONSTRAINTS:'
        write(*,*) 'No such equation number as ',icc(i)
        write(*,*) 'PROCESS stopping.'
        stop

     end select

     !  Crude method of catching NaN errors

     if ((abs(cc(i)) > 9.99D99).or.(cc(i) /= cc(i))) then
        write(*,*) 'Error in routine CONSTRAINTS:'
        write(*,*) 'NaN/infty error for constraint equation ',icc(i)
        write(*,*) 'Value is ',cc(i)
        write(*,*) ' '

        !  Add debugging lines as appropriate...

        select case (icc(i))

        case (1)
           write(*,*) 'betaft = ', betaft
           write(*,*) 'betanb = ', betanb
           write(*,*) 'dene = ', dene
           write(*,*) 'ten = ', ten
           write(*,*) 'dnitot = ', dnitot
           write(*,*) 'tin = ', tin
           write(*,*) 'btot = ',btot
           write(*,*) 'beta = ', beta

        case (16)
           write(*,*) 'fpnetel = ', fpnetel
           write(*,*) 'pnetelmw = ', pnetelmw
           write(*,*) 'pnetelin = ', pnetelin

        case (30)
           write(*,*) 'fpinj = ', fpinj
           write(*,*) 'pinjalw = ', pinjalw
           write(*,*) 'pinji = ', pinji
           write(*,*) 'pinje = ', pinje
 
        end select

        write(*,*) ' '
        write(*,*) 'PROCESS stopping.'
        stop
     end if

  end do

end subroutine constraints
