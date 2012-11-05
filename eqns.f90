!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine con1(m,cc)

  !+ad_name  con1
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
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  rfp_variables
  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  pulse.h90
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use constraint_variables
  use current_drive_variables
  use divertor_variables
  use heat_transport_variables
  use ife_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use rfp_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables

  implicit none

  include 'pulse.h90'

  !  Arguments

  integer, intent(in) :: m
  real(kind(1.0D0)), dimension(m), intent(out) :: cc

  !  Local variables

  integer :: i
  real(kind(1.0D0)) :: cratmx, tcycle, totmva, acoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,m

     select case (icc(i))

     case (1)  !  Relationship between beta, temperature and density
               !  This is a consistency equation.

        cc(i) = 1.0D0 - (betaft + betanb + 3.204D-16 &
             * rmu0 * (dene*ten + dnitot*tin)/btot**2 )/beta

     case (2)  !  Global plasma power balance equation
               !  This is a consistency equation.

        if (ignite == 0) then
           cc(i) = 1.0D0 - dign*(ptre+ptri+prad)/( falpha*palp &
                + pcharge + pohmpv + 1.0D-6*(pinji+pinje)/vol )
        else
           cc(i) = 1.0D0 - dign*(ptre+ptri+prad)/ &
                (falpha*palp + pcharge + pohmpv)
        end if

     case (3)  !  Global power balance equation for ions

        if (ignite == 0) then
           cc(i) = 1.0D0 - dign*(ptri+pie)/ &
                ( falpha*palpi + 1.0D-6*pinji/vol )
        else
           cc(i) = 1.0D0 - dign*(ptri+pie)/(falpha*palpi)
        end if

     case (4)  !  Global power balance equation for electrons

        if (ignite == 0) then
           cc(i) = 1.0D0 - dign*(ptre+prad)/ &
                ( (falpha*palpe)+pie + 1.0D-6*pinje/vol )
        else
           cc(i) = 1.0D0 - dign*(ptre+prad)/( (falpha*palpe)+pie )
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
           write(*,*) 'Error in routine CON1:'
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

     case (20)  !  Equation for port size

        cc(i) = 1.0D0 - prtszreq/(fportsz * prtsz)

     case (21)  !  Equation for minimum minor radius

        cc(i) = 1.0D0 - frminor * rminor/aplasmin

     case (22)  !  Equation for divertor collisionality

        cc(i) = 1.0D0 - fdivcol * rlenmax / rlclolcn

     case (23)  !  Equation for allowable TF Coil current density

        cc(i) = 1.0D0 - fjtfc * rjtfsual / oacdcp

     case (24)  !  Equation for beta limit (Troyon limit in tokamaks)

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
           write(*,*) 'Error in routine CON1:'
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
           write(*,*) 'Error in routine CON1:'
           write(*,*) 'Do not use constraint 28 if IGNITE=1.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if

     case (29)  !  Equation for inboard major radius

        cc(i) = 1.0D0 - (rmajor - rminor) / rinboard

     case (30)  !  Equation to limit injection power

        cc(i) = 1.0D0 - fpinj * 1.0D6 * pinjalw / (pinji + pinje)

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

     case (41)  !  Equation to limit minimum OH swing time

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
                !  Relevant only to reversed field pinch devices

        if (irfp == 0) then
           write(*,*) 'Error in routine EQNS:'
           write(*,*) 'Do not use constraint 47 if irfp=0.'
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

     case default

        write(*,*) 'Error in routine CON1:'
        write(*,*) 'No such equation number as ',icc(i)
        write(*,*) 'PROCESS stopping.'
        stop

     end select

     !  Crude method of catching NaN errors

     if ((abs(cc(i)) > 9.99D99).or.(cc(i) /= cc(i))) then
        write(*,*) 'Error in routine CON1:'
        write(*,*) 'NaN error for constraint equation ',icc(i)
        write(*,*) 'PROCESS stopping.'
        stop
     end if

  end do

end subroutine con1
