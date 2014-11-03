!  $Id:: availability.f90 258 2014-04-24 12:28:55Z pknight              $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module availability_module

  !+ad_name  availability_module
  !+ad_summ  Module containing plant availability routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  avail
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  plant availability and component lifetimes for a fusion power plant.
  !+ad_prob  None
  !+ad_call  cost_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  ife_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  pulse_variables
  !+ad_call  rfp_variables
  !+ad_hist  06/11/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cost_variables
  use divertor_variables
  use fwbs_variables
  use ife_variables
  use physics_variables
  use process_output
  use pulse_variables
  use rfp_variables
  use tfcoil_variables

  implicit none

  private
  public :: avail
  public :: avail_new

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine avail(outfile,iprint)

    !+ad_name  avail
    !+ad_summ  Routine to calculate component lifetimes and the overall plant
    !+ad_summ  availability
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the component lifetimes and the overall
    !+ad_desc  plant availability.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_hist  20/09/11 PJK Removed dble calls
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  31/10/12 PJK Added cost_variables
    !+ad_hist  05/11/12 PJK Added rfp_variables
    !+ad_hist  05/11/12 PJK Added ife_variables
    !+ad_hist  05/11/12 PJK Added pulse_variables
    !+ad_hist  23/05/13 PJK Removed bktlife calculation if blktmodel>0
    !+ad_hist  05/06/13 PJK Removed abktflnc output if blktmodel>0
    !+ad_hist  15/08/13 PJK Changed cdrlife description
    !+ad_hist  24/04/14 PJK Calculation proceeds irrespective of iprint,
    !+ad_hisc               thus correcting erroneous lifetimes shown
    !+ad_hisc               in the output file
    !+ad_stat  Okay
    !+ad_docs  F/PL/PJK/PROCESS/CODE/043
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: lb, ld, td
    real(kind(1.0D0)), save :: uplanned, uutot
    integer :: n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Full power lifetimes (in years)
    !  Most of these are already calculated for an IFE device

    if (ife /= 1) then

       !  First wall / blanket

       if (blktmodel == 0) bktlife = min( abktflnc/wallmw, tlife )
       fwlife = bktlife

       !  Divertor

       divlife = min( adivflnc/hldiv, tlife )
       if (irfp == 1) divlife = 1.0D0

       !  Centrepost

       if (itart == 1) then
          cplife = min( cpstflnc/wallmw, tlife )
       end if

    end if

    !  Plant Availability (Use new model if IAVAIL = 1)

    if (iavail == 1) then

       !  Which component has the shorter life?

       if (divlife < bktlife) then
          ld = divlife
          lb = bktlife
          td = tdivrepl
       else
          ld = bktlife
          lb = divlife
          td = tbktrepl
       end if

       !  Number of outages between each combined outage

       n = int(lb/ld) - 1

       !  Planned unavailability

       uplanned = (n*td + tcomrepl) / &
            ( (n+1)*ld + (n*td + tcomrepl) )

       !  Unplanned unavailability
       !  Rather than simply summing the individual terms, the
       !  following protects against the total availability becoming zero
       !  or negative

       uutot = uubop
       uutot = uutot + (1.0D0 - uutot)*uucd
       uutot = uutot + (1.0D0 - uutot)*uudiv
       uutot = uutot + (1.0D0 - uutot)*uufuel
       uutot = uutot + (1.0D0 - uutot)*uufw
       uutot = uutot + (1.0D0 - uutot)*uumag
       uutot = uutot + (1.0D0 - uutot)*uuves

       !  Total availability

       cfactr = 1.0D0 - (uplanned + uutot - (uplanned*uutot))

    end if

    !  Modify lifetimes to take account of the availability

    if (ife /= 1) then

       !  First wall / blanket

       if (bktlife < tlife) then
          bktlife = min( bktlife/cfactr, tlife )
          fwlife = bktlife
       end if

       !  Divertor

       if ((divlife < tlife).and.(irfp /= 1)) then
          divlife = min( divlife/cfactr, tlife )
       end if

       !  Centrepost

       if ((itart == 1).and.(cplife < tlife)) then
          cplife = min( cplife/cfactr, tlife )
       end if

    end if

    !  Current drive (assumed equal to first wall and blanket lifetime)

    cdrlife = bktlife

    if (iprint /= 1) return

    !  Output section

    call oheadr(outfile,'Plant Availability')
    if (blktmodel == 0) then
       call ovarre(outfile,'Allowable blanket neutron fluence (MW-yr/m2)', &
            '(abktflnc)',abktflnc)
    end if
    call ovarre(outfile,'Allowable divertor heat fluence (MW-yr/m2)', &
         '(adivflnc)',adivflnc)
    call ovarre(outfile,'First wall / blanket lifetime (years)', &
         '(bktlife)',bktlife)
    call ovarre(outfile,'Divertor lifetime (years)', &
         '(divlife)',divlife)

    if (itart == 1) then
       call ovarre(outfile,'Centrepost lifetime (years)','(cplife)',cplife)
    end if

    call ovarre(outfile,'Heating/CD system lifetime (years)', &
         '(cdrlife)',cdrlife)
    call ovarre(outfile,'Total plant lifetime (years)','(tlife)',tlife)

    if (iavail == 1) then
       if (divlife < bktlife) then
          call ovarre(outfile,'Time needed to replace divertor (years)', &
               '(tdivrepl)',tdivrepl)
       else
          call ovarre(outfile,'Time needed to replace blanket (years)', &
               '(tbktrepl)',tbktrepl)
       end if
       call ovarre(outfile,'Time needed to replace blkt + div (years)', &
            '(tcomrepl)',tcomrepl)
       call ovarre(outfile,'Planned unavailability fraction', &
            '(uplanned)',uplanned)
       call ovarre(outfile,'Unplanned unavailability fraction', &
            '(uutot)',uutot)
    end if

    call ovarre(outfile,'Total plant availability fraction', &
         '(cfactr)',cfactr)

  end subroutine avail

  subroutine avail_new(outfile,iprint)

    !+ad_name  avail_new
    !+ad_summ  Routine to calculate component lifetimes and the overall plant
    !+ad_summ  availability
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the component lifetimes and the overall
    !+ad_desc  plant availability using an updated model linked to the 2014 EUROfusion
    !+ad_desc  RAMI task
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  03/11/14 PJK Initial F90 version
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: lb, ld, td, num_rh_systems
    real(kind(1.0D0)), save :: u_planned, u_unplanne
    real(kind(1.0D0)) :: mttr_blanket, mttr_divertor, mttr_shortest
    real(kind(1.0D0)) :: lifetime_shortest, lifetime_longest
    real(kind(1.0D0)) :: u_unplanned_magnets, mag_temp_marg
    integer :: n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Full power lifetimes (in years)
    !  Most of these are already calculated for an IFE device

    if (ife /= 1) then

       !  First wall / blanket

       if (blktmodel == 0) bktlife = min( abktflnc/wallmw, tlife )
       fwlife = bktlife

       !  Divertor

       divlife = min( adivflnc/hldiv, tlife )
       if (irfp == 1) divlife = 1.0D0

       !  Centrepost

       if (itart == 1) then
          cplife = min( cpstflnc/wallmw, tlife )
       end if

    end if

    !  Plant Availability

    !  Calculate the blanket and divertor replacement times

    !  Blanket replacement time
    !  ( Calculated using scaling from 2014 EUROfusion RAMI report )
    
    !  num_rh_systems should be a gloabl variable with integer values 
    !  ranging from 1-10
    num_rh_systems = 4
    
    !  Mean time to repair blanket is same as replacing both blanket and divertor
    mttr_blanket = 21 * num_rh_systems**(-0.9)

    !  Mean time to repair divertor is 70% of time taken to replace blanket
    !  this is taken from Oliver Crofts 2014 paper
    mttr_divertor = 0.7*mttr_blanket

    !  Which component has the shorter life?

    if (divlife < bktlife) then
       lifetime_shortest = divlife
       lifetime_longest = bktlife
       mttr_shortest = mttr_divertor
    else
       lifetime_shortest = bktlife
       lifetime_longest = divlife
       mttr_shortest = mttr_blanket
    end if

    !  Number of outages between each combined outage

    n = int(lifetime_longest/lifetime_shortest) - 1

    !  Planned unavailability
    u_planned = (n*mttr_shortest + mttr_blanket) / &
         ( (n+1)*lifetime_shortest + (n*mttr_shortest + mttr_blanket) )

    !  Un-planned unavailability

    !  Magnets
    call calc_u_unplanned_magnets(u_unplanned_magnets)
    
  end subroutine avail_new

  subroutine calc_u_unplanned_magnets(u_unplanned_magnets)

    !+ad_name  calc_u_unplanned_magnets
    !+ad_summ  Calculates the unplanned unavailability of the magnets using
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  u_unplanned_magnets : output real : unplanned unavailability of magnets

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(out) :: u_unplanned_magnets

    !  Local Variables
    real(kind(1.0D0)) :: m, dy, dx, c, y_i
    real(kind(1.0D0)) :: conf_level_magnets, mag_temp_marg_limit, mag_temp_marg, mag_main_time
    real(kind(1.0D0)) :: mag_min_u_unplanned, start_of_risk

    !  Magnet temperature margin limit (k)
    mag_temp_marg_limit = tmargmin

    !  Magnet temperature margin (K)
    mag_temp_marg = temp_margin

    !  Magnet maintenance time (years)
    mag_main_time = 0.5

    !  Minimum unplanned unavailability
    mag_min_u_unplanned = mag_main_time / (tlife + mag_main_time)

    !  If new linear model chosen
    if (iavail == 2) then

       !  confidence level for magnet system. WILL BE AN INPUT PARAMETER WITH
       !  DEFAULT VALUE
       conf_level_magnets = 0.95

       !  point at which risk of unplanned unavailability increases
       start_of_risk = mag_temp_marg_limit / conf_level_magnets
       
       !  Determine if temperature margin is in region with risk of 
       !  unplanned unavailability
       if (temp_margin >= start_of_risk) then

         u_unplanned_magnets = mag_min_u_unplanned

       else

          ! gradient of line
          dy = mag_min_u_unplanned - 1.0
          dx = start_of_risk - tmargmin
          m = dy/dx
          c = 1.0 - m*tmargmin
          u_unplanned_magnets = m*temp_margin + c 

       end if
     
    !  If new non-linear model chosen
    else
       
       !  confidence level for magnet system. WILL BE AN INPUT PARAMETER WITH
       !  DEFAULT VALUE
       conf_level_magnets = 0.95

       

       

    end if
    
  end subroutine calc_u_unplanned_magnets


end module availability_module
