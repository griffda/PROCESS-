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
  !+ad_hist  24/11/14 JM  New version of availability model
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
  use vacuum_variables

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
    !+ad_hist  03/11/14 JM  Initial version
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables
    
    real(kind(1.0D0)) :: u_planned
    real(kind(1.0D0)) :: u_unplanned
    real(kind(1.0D0)) :: u_unplanned_magnets
    real(kind(1.0D0)) :: u_unplanned_div
    real(kind(1.0D0)) :: u_unplanned_fwbs
    real(kind(1.0D0)) :: u_unplanned_bop
    real(kind(1.0D0)) :: u_unplanned_hcd
    real(kind(1.0D0)) :: u_unplanned_vacuum

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Plant Availability

    !  Planned unavailability
    call calc_u_planned(outfile, iprint, u_planned)

    !  Un-planned unavailability
    !  Magnets
    call calc_u_unplanned_magnets(outfile,iprint, u_unplanned_magnets)

    !  Divertor
    call calc_u_unplanned_divertor(outfile,iprint, u_unplanned_div)

    !  First wall and blanket
    call calc_u_unplanned_fwbs(outfile,iprint, u_unplanned_fwbs)

    !  Balance of plant
    call calc_u_unplanned_bop(outfile,iprint, u_unplanned_bop)

    !  Heating and current drive
    call calc_u_unplanned_hcd(outfile,iprint, u_unplanned_hcd)

    !  Vacuum systems
    call calc_u_unplanned_vacuum(outfile,iprint, u_unplanned_vacuum)

    !  Total unplanned unavailability    
    u_unplanned = min( 1.0, u_unplanned_magnets + &
         u_unplanned_div + u_unplanned_fwbs + &
         u_unplanned_bop + u_unplanned_hcd + u_unplanned_vacuum)  
       
    !  Total availability
    cfactr = max(1 - (u_planned + u_unplanned + u_planned*u_unplanned), 0.0)

    !  Modify lifetimes to take account of the availability
    call modify_lifetimes()

    ! Output
    if (iprint /= 1) return

    call ocmmnt(outfile,'Total unavailability:')
    call oblnkl(outfile) 
    call ovarre(outfile,'Total planned unavailability', &
         '(u_planned)', u_planned)
    call ovarre(outfile,'Total unplanned unavailability', &
         '(u_unplanned)', u_unplanned)
    call oblnkl(outfile)
    call ovarre(outfile,'Total plant availability fraction', &
         '(cfactr)',cfactr)
   
  end subroutine avail_new

  subroutine calc_u_planned(outfile, iprint, u_planned)

    !+ad_name  calc_u_unplanned_magnets
    !+ad_summ  Calculates the planned availability of the plant
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_planned : output real : planned unavailability of plant

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_planned

    !  Local variables
    real(kind(1.0D0)) :: lb, ld, td
    real(kind(1.0D0)) :: mttr_blanket, mttr_divertor, mttr_shortest
    real(kind(1.0D0)) :: lifetime_shortest, lifetime_longest
    integer :: n

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

    !  Calculate the blanket and divertor replacement times

    !  Blanket replacement time
    !  ( Calculated using scaling from 2014 EUROfusion RAMI report )
    
    !  num_rh_systems is a global variable with integer values 
    !  ranging from 1-10
    !  num_rh_systems = 4
    
    !  Mean time to repair blanket is same as replacing both blanket and divertor
    !  +2.0 at the end is for the 1 month cooldown and pump down at either end
    !  of the maintenance period
    mttr_blanket = (21 * num_rh_systems**(-0.9) + 2.0D0) / 12.0D0

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

    if (iprint /= 1) return

    call oheadr(outfile,'Plant Availability (2014 Model)')

    call ocmmnt(outfile,'Planned unavailability:')
    call oblnkl(outfile)
    call ovarre(outfile,'Allowable blanket neutron fluence (MW-yr/m2)', &
         '(abktflnc)',abktflnc)
    call ovarre(outfile,'Allowable divertor heat fluence (MW-yr/m2)', &
         '(adivflnc)',adivflnc)
    call ovarre(outfile,'First wall / blanket lifetime (years)', &
         '(bktlife)',bktlife)
    call ovarre(outfile,'Divertor lifetime (years)', &
         '(divlife)',divlife)

    call ovarin(outfile,'Number of remote handling systems', &
         '(num_rh_systems)',num_rh_systems)
    call ovarre(outfile,'Time needed to replace divertor (years)', &
         '(mttr_divertor)',mttr_divertor)
    call ovarre(outfile,'Time needed to replace blanket (years)', &
         '(mttr_blanket)',mttr_blanket)
    call ovarre(outfile,'Time needed to replace blkt + div (years)', &
         '(mttr_blanket)',mttr_blanket)
    call ovarre(outfile,'Total planned unavailability', &
         '(uplanned)',u_planned)
    call oblnkl(outfile)

  end subroutine calc_u_planned

  subroutine modify_lifetimes

    !+ad_name  calc_u_unplanned_magnets
    !+ad_summ  Calculates the planned availability of the plant
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None

    implicit none

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

  end subroutine modify_lifetimes

  subroutine calc_u_unplanned_magnets(outfile, iprint, u_unplanned_magnets)

    !+ad_name  calc_u_unplanned_magnets
    !+ad_summ  Calculates the unplanned unavailability of the magnets using
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_magnets : output real : unplanned unavailability of magnets

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_magnets

    !  Local Variables
    real(kind(1.0D0)) :: m, dy, dx, c, y_i, e, lam, u_diff, t_diff, u_diff_e
    real(kind(1.0D0)) :: mag_temp_marg_limit, mag_temp_marg, mag_main_time
    real(kind(1.0D0)) :: mag_min_u_unplanned, start_of_risk, t_life

    !  Magnet temperature margin limit (k)
    mag_temp_marg_limit = tmargmin

    !  Magnet temperature margin (K)
    mag_temp_marg = temp_margin

    !  Magnet maintenance time (years)
    mag_main_time = 0.5

    !  Minimum unplanned unavailability
    mag_min_u_unplanned = mag_main_time / (tlife + mag_main_time)
    
    !  point at which risk of unplanned unavailability increases
    !  conf_mag is magnet availability confidence level (global var)
    start_of_risk = mag_temp_marg_limit / conf_mag

    !  Determine if temperature margin is in region with risk of 
    !  unplanned unavailability
    if (temp_margin >= start_of_risk) then

       u_unplanned_magnets = mag_min_u_unplanned

    else

       !  Linear decrease in expected lifetime when approaching the limit
       !
       t_life = max( 0.0, (tlife/(start_of_risk - tmargmin))*(temp_margin - tmargmin) )
       u_unplanned_magnets = mag_main_time/(t_life + mag_main_time)

    end if

    if (iprint /= 1) return

    call ocmmnt(outfile,'Magnets:')
    call oblnkl(outfile)
    call ovarre(outfile,'Minimum temperature margin (K)', &
         '(tmargmin)',tmargmin)
    call ovarre(outfile,'Confidence level (%)', &
         '(conf_mag)',conf_mag)
    call ovarre(outfile,'Temperature Margin (K)', &
         '(temp_margin)',temp_margin)
    call ovarre(outfile,'Magnets unplanned unavailability', &
         '(u_unplanned_magnets)',u_unplanned_magnets)
    call oblnkl(outfile)
    
  end subroutine calc_u_unplanned_magnets

  subroutine calc_u_unplanned_divertor(outfile, iprint, u_unplanned_div)

    !+ad_name  calc_u_unplanned_divertor
    !+ad_summ  Calculates the unplanned unavailability of the divertor "system"
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_div : output real : unplanned unavailability of divertor

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_div

    !  Local Variables
    real(kind(1.0D0)) :: div_num_cycles, div_main_time, div_min_u_unplanned
    real(kind(1.0D0)) :: t_life

    !  Number of cycles in divertor lifetime
    div_num_cycles = (divlife*365.25*24*60*60)/tcycle

    !  Divertor maintenance time (years)
    !  For replacing a faulty divertor cassette you might as well replace the whole set
    !  
    div_main_time = 0.25

    !  Minimum unplanned unavailability
    div_min_u_unplanned = div_main_time / (tlife + div_main_time)
    
    !  Determine if the number of cycles is under the design criteria
    !  Cycle limit is a global var
    if (div_num_cycles <= div_cycle_lim) then

       u_unplanned_div = div_min_u_unplanned

    else

       ! Linear decrease in expected lifetime when approaching the limit
       !
       t_life = max( 0.0, tlife + (-tlife/ & 
            (div_cycle_lim*conf_div - div_cycle_lim))* &
            (div_num_cycles - div_cycle_lim) )

       u_unplanned_div = div_main_time/(t_life + div_main_time)

    end if

    if (iprint /= 1) return

    call ocmmnt(outfile,'Divertor:')
    call oblnkl(outfile)
    call ovarin(outfile,'Design criteria number of cycles', &
         '(div_cycle_lim)',div_cycle_lim)
    call ovarre(outfile,'Number of cycles', &
         '(div_num_cycles)',div_num_cycles)
    call ovarre(outfile,'Divertor unplanned unavailability', &
         '(u_unplanned_div)',u_unplanned_div)
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_divertor

  subroutine calc_u_unplanned_fwbs(outfile, iprint, u_unplanned_fwbs)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the first wall and blanket "system"
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_fwbs : output real : unplanned unavailability of first wall and blanket

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_fwbs

    !  Local Variables
    real(kind(1.0D0)) :: fwbs_num_cycles, fwbs_main_time, fwbs_min_u_unplanned
    real(kind(1.0D0)) :: t_life

    !  Number of cycles in divertor lifetime
    fwbs_num_cycles = (bktlife*365.25*24*60*60)/tcycle

    !  Divertor maintenance time (years)
    !  For replacing a faulty divertor cassette you might as well replace the whole set
    !  
    fwbs_main_time = 0.5

    !  Minimum unplanned unavailability
    fwbs_min_u_unplanned = fwbs_main_time / (tlife + fwbs_main_time)
 
    !  Determine if the number of cycles is under the design criteria
    if (fwbs_num_cycles <= fwbs_cycle_lim) then

       u_unplanned_fwbs = fwbs_min_u_unplanned

    else

       !  Linear decrease in expected lifetime when approaching the limit
       !
       t_life = max( 0.0, tlife + (-tlife/ & 
            (fwbs_cycle_lim*conf_fwbs - fwbs_cycle_lim))* &
            (fwbs_num_cycles - fwbs_cycle_lim) )

       u_unplanned_fwbs = fwbs_main_time/(t_life + fwbs_main_time)

    end if   

    if (iprint /= 1) return

    call ocmmnt(outfile,'Blanket:')
    call oblnkl(outfile)
    call ovarin(outfile,'Design criteria number of cycles', &
         '(fwbs_cycle_lim)',fwbs_cycle_lim)
    call ovarre(outfile,'Number of cycles', &
         '(fwbs_num_cycles)',fwbs_num_cycles)
    call ovarre(outfile,'fwbs unplanned unavailability', &
         '(u_unplanned_fwbs)',u_unplanned_fwbs)
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_fwbs

  subroutine calc_u_unplanned_bop(outfile, iprint, u_unplanned_bop)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the balance of plant
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_bop : output real : unplanned unavailability of balance of plant

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_bop

    u_unplanned_bop = 0.0D0

    
    

  end subroutine calc_u_unplanned_bop

  subroutine calc_u_unplanned_hcd(outfile, iprint, u_unplanned_hcd)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the heating and current drive system
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_hcd : output real : unplanned unavailability of hcd

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_hcd

    u_unplanned_hcd = 0.0D0

  end subroutine calc_u_unplanned_hcd

  subroutine calc_u_unplanned_vacuum(outfile, iprint, u_unplanned_vacuum)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the vacuum "system"
    !+ad_dumm  methodology outlines in 2014 EUROfusion RAMI report "Availability in PROCESS"
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_vacuum : output real : unplanned unavailability of vacuum system

    implicit none

    !  Arguments
    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_vacuum

    ! Local variables
    real(kind(1.0D0)) :: cryo_failure_rate, num_redundancy_pumps
    real(kind(1.0D0)) :: pump_failures, pump_maintenance_time

    !  Cryopump failure rate per machine lifetime
    !  From "Selected component failure rate values from fusion 
    !  safety assessment tasks", Cadwallader (1994)
    cryo_failure_rate = 2D-6 * 365.25 * 24 *tlife

    !  Redundancy pumps 
    !  Redundancy % will be a user input
    num_redundancy_pumps = (redun_vac/100.0)*vpumpn
  
    !  Number of failures for all pumps
    pump_failures = anint((vpumpn - num_redundancy_pumps)*cryo_failure_rate)

    !  Pump replacement time (years)
    !  Taken value ot be 2 months. Minimum time for in-vessel 
    !  activities with normal RH equipment
    pump_maintenance_time = 1.0D0/6.0D0
 
    !  Total vacuum unplanned unavailability
    u_unplanned_vacuum = (pump_maintenance_time*pump_failures)/(tlife)
    
    if (iprint /= 1) return

    call ocmmnt(outfile,'Vacuum:')    
    call oblnkl(outfile)
    call ovarre(outfile,'Number of pumps', &
         '(vpumpn)', vpumpn)
    call ovarre(outfile,'Number of pump failures over lifetime', &
         '(pump_failures)', pump_failures)
    call ovarin(outfile,'Redundancy percentage', &
         '(redun_vac)', redun_vac)
    call ovarre(outfile,'Number of redundancy pumps', &
         '(num_redundancy_pumps)', num_redundancy_pumps)
    call ovarre(outfile,'Vacuum unplanned unavailability', &
         '(u_unplanned_vacuum)', u_unplanned_vacuum)
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_vacuum

end module availability_module
