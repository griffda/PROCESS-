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
  !+ad_call  times_variables
  !+ad_call  vacuum_variables
  !+ad_call  maths_library
  !+ad_hist  06/11/12 PJK Initial version of module
  !+ad_hist  24/11/14 JM  New version of availability model
  !+ad_hist  18/12/14 JM  Update availability calculation with corrections
  !+ad_hist  09/02/15 JM  Update to divertor and blanket models
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
  use times_variables
  use vacuum_variables
  use maths_library

  implicit none

  private
  public :: avail
  public :: avail_new
  real(kind(1.0D0)), parameter :: year = 31557600.0D0
  real(kind(1.0D0)), parameter :: day = 86400.0D0

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
    !+ad_hist  22/10/14 PJK Modified blanket and first wall lifetime
    !+ad_hisc               calculation; fwlife is calculated in fwbs now
    !+ad_hist  09/02/15 JM  Changed int function to ceiling
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

       if (blktmodel == 0) then
          bktlife = min(fwlife, abktflnc/wallmw, tlife)
       end if

       !  Divertor

       divlife = min(adivflnc/hldiv, tlife)
       if (irfp == 1) divlife = 1.0D0

       !  Centrepost

       if (itart == 1) then
          cplife = min(cpstflnc/wallmw, tlife)
       end if

    end if

    !  Plant Availability (Use new model if IAVAIL = 2)

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

       n = ceiling(lb/ld) - 1

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    !+ad_call  calc_u_planned
    !+ad_call  calc_u_unplanned_bop
    !+ad_call  calc_u_unplanned_divertor
    !+ad_call  calc_u_unplanned_fwbs
    !+ad_call  calc_u_unplanned_hc
    !+ad_call  calc_u_unplanned_magnets
    !+ad_call  calc_u_unplanned_vacuum
    !+ad_call  modify_lifetimes
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  03/11/14 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
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

    t_operation = tlife * (1.0D0-u_planned)

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
    ! Number of redundant pumps MDK
    redun_vac = floor(vpumpn*redun_vacp/100.0 + 0.5D0)
    call calc_u_unplanned_vacuum(outfile,iprint, u_unplanned_vacuum)

    !  Total unplanned unavailability    

    u_unplanned = min(1.0D0, u_unplanned_magnets + &
         u_unplanned_div + u_unplanned_fwbs + &
         u_unplanned_bop + u_unplanned_hcd + u_unplanned_vacuum)  
       
    !  Total availability

    cfactr = max(1.0D0 - (u_planned + u_unplanned + u_planned*u_unplanned), 0.0D0)

    !  Capacity factor
    cpfact = cfactr * (tburn / tcycle)   

    !  Modify lifetimes to take account of the availability

    !call modify_lifetimes()

    ! Output

    if (iprint /= 1) return

    call ocmmnt(outfile,'Total unavailability:')
    call oblnkl(outfile) 
    call ovarre(outfile,'Total planned unavailability', '(u_planned)', u_planned)
    call ovarre(outfile,'Total unplanned unavailability', '(u_unplanned)', u_unplanned)
    call oblnkl(outfile)
    call ovarre(outfile,'Total plant availability fraction', '(cfactr)',cfactr)
    call ovarre(outfile,'Total DT operational time (years)','(t_operation)',t_operation)
    call ovarre(outfile,'Total plant lifetime (years)','(tlife)',tlife)
    call ovarre(outfile,'Capacity factor: total lifetime electrical energy output / output power','(cpfact)',cpfact)
   
  end subroutine avail_new

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_planned(outfile, iprint, u_planned)

    !+ad_name  calc_u_planned
    !+ad_summ  Calculates the planned unavailability of the plant
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_planned : output real : planned unavailability of plant
    !+ad_desc  This routine calculates the planned unavailability of the
    !+ad_desc  plant, using the methodology outlined in the 2014 EUROfusion
    !+ad_desc  RAMI report.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_planned

    !  Local variables

    real(kind(1.0D0)) :: lb, ld, td
    real(kind(1.0D0)) :: mttr_blanket, mttr_divertor, mttr_shortest
    real(kind(1.0D0)) :: lifetime_shortest, lifetime_longest
    integer :: n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Full power lifetimes (in years)
    !  Most of these are already calculated for an IFE device

    if (ife /= 1) then

       !  First wall / blanket

       !bktlife = min(fwlife, abktflnc/wallmw, tlife)       
       bktlife =  min(abktflnc/wallmw, tlife)

       !  Divertor

       divlife = min( adivflnc/hldiv, tlife )
       if (irfp == 1) divlife = 1.0D0

       !  Centrepost

       if (itart == 1) then
          cplife = min( cpstflnc/wallmw, tlife )
       end if

    end if

    !  Current drive (assumed equal to first wall and blanket lifetime)

    cdrlife = bktlife

    !  Calculate the blanket and divertor replacement times

    !  Blanket replacement time
    !  ( Calculated using scaling from 2014 EUROfusion RAMI report )
    
    !  Mean time to repair blanket is same as replacing both blanket and divertor.
    !  The +2.0 at the end is for the 1 month cooldown and pump down at either end
    !  of the maintenance period

    mttr_blanket = (21.0D0 * num_rh_systems**(-0.9D0) + 2.0D0) / 12.0D0

    !  Mean time to repair divertor is 70% of time taken to replace blanket
    !  This is taken from Oliver Crofts 2014 paper

    mttr_divertor = 0.7D0*mttr_blanket

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

    n = ceiling(lifetime_longest/lifetime_shortest) - 1

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
    call ovarre(outfile,'First wall / blanket lifetime (FPY)', &
         '(bktlife)',bktlife)
    call ovarre(outfile,'Divertor lifetime (FPY)', &
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  subroutine modify_lifetimes

!    !+ad_name  modify_lifetimes
!    !+ad_summ  Calculate the component lifetimes
!    !+ad_type  Subroutine
!    !+ad_auth  J Morris, CCFE, Culham Science Centre
!    !+ad_cont  None
!    !+ad_args  None
!    !+ad_desc  This routine calculates the lifetimes of the main fusion power core
!    !+ad_desc  components.
!    !+ad_prob  None
!    !+ad_call  None
!    !+ad_hist  02/12/14 JM  Initial version
!    !+ad_stat  Okay
!    !+ad_docs  None
!    !
!    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
!    implicit none

!    !  Arguments

!    !  Local variables

!    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
!    if (ife /= 1) then

!       !  First wall / blanket

!       if (bktlife < tlife) then
!          bktlife = min( bktlife/cfactr, tlife )
!          fwlife = bktlife
!       end if

!       !  Divertor

!       if ((divlife < tlife).and.(irfp /= 1)) then
!          divlife = min( divlife/cfactr, tlife )
!       end if

!       !  Centrepost

!       if ((itart == 1).and.(cplife < tlife)) then
!          cplife = min( cplife/cfactr, tlife )
!       end if

!    end if

!  end subroutine modify_lifetimes

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_magnets(outfile, iprint, u_unplanned_magnets)

    !+ad_name  calc_u_unplanned_magnets
    !+ad_summ  Calculates the unplanned unavailability of the magnets
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_magnets : output real : unplanned unavailability of magnets
    !+ad_desc  This routine calculates the unplanned unavailability of the magnets,
    !+ad_desc  using the methodology outlined in the 2014 EUROfusion
    !+ad_desc  RAMI report.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  ovarre
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_hist  08/12/14 JM  Corrections to calculation for divertor
    !+ad_hist  08/01/15 JM  Corrections to calculation for divertor
    !+ad_hist  09/02/15 JM  More corrections to calculation for divertor
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_magnets

    !  Local Variables

    real(kind(1.0D0)) :: m, dy, dx, c, y_i, e, lam, u_diff, t_diff, u_diff_e
    real(kind(1.0D0)) :: mag_temp_marg_limit, mag_temp_marg, mag_main_time
    real(kind(1.0D0)) :: mag_min_u_unplanned, start_of_risk, t_life

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Magnet temperature margin limit (K)

    mag_temp_marg_limit = tmargmin

    !  Magnet temperature margin (K)

    mag_temp_marg = temp_margin

    !  Magnet maintenance time (years)

    mag_main_time = 0.5D0

    !  Minimum unplanned unavailability

    mag_min_u_unplanned = mag_main_time / (t_operation + mag_main_time)
    
    !  Point at which risk of unplanned unavailability increases
    !  conf_mag is the c factor, which determines the temperature margin at which lifetime starts to decline.

    start_of_risk = mag_temp_marg_limit / conf_mag

    !  Determine if temperature margin is in region with risk of 
    !  unplanned unavailability

    if (temp_margin >= start_of_risk) then

       u_unplanned_magnets = mag_min_u_unplanned

    else

       !  Linear decrease in expected lifetime when approaching the limit

       t_life = max( 0.0D0, (t_operation/(start_of_risk - tmargmin))*(temp_margin - tmargmin) )
       u_unplanned_magnets = mag_main_time/(t_life + mag_main_time)

    end if

    if (iprint /= 1) return

    call ocmmnt(outfile,'Magnets:')
    call oblnkl(outfile)
    call ovarre(outfile,'Minimum temperature margin (K)', &
         '(tmargmin)',tmargmin)
    call ovarre(outfile,'c parameter, determining the temperature margin where lifetime declines', &
         '(conf_mag)',conf_mag)
    call ovarre(outfile,'Temperature Margin (K)', &
         '(temp_margin)',temp_margin)
    call ovarre(outfile,'Magnets unplanned unavailability', &
         '(u_unplanned_magnets)',u_unplanned_magnets)
    call oblnkl(outfile)
    
  end subroutine calc_u_unplanned_magnets

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_divertor(outfile, iprint, u_unplanned_div)

    !+ad_name  calc_u_unplanned_divertor
    !+ad_summ  Calculates the unplanned unavailability of the divertor
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_div : output real : unplanned unavailability of divertor
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_hist  06/05/15 MDK Rewrote routine to match engineering paper
    !+ad_stat  Okay
    !+ad_docs  
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_div

    !  Local Variables

    real(kind(1.0D0)) :: a0, div_avail, n, pf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate cycle limit in terms of days 
    ! Number of cycles between planned blanket replacements, N
    n = divlife * year / tcycle
    ! The probability of failure in one pulse cycle 
    ! (before the reference cycle life)    
    pf = (div_prob_fail / day) * tcycle
    a0 = 1.0D0 - pf * div_umain_time * year / tcycle
    
    !  Integrating the instantaneous availability gives the mean 
    !  availability over the planned cycle life N
    if (div_nu <= div_nref) then
        write(*,*) 'div_nu <= div_nref'
        write(*,*) 'The cycle when the divertor fails with 100% probability <= Reference value for cycle cycle life of divertor'
        call ocmmnt(outfile,'EROROR: The cycle when the divertor fails with 100% probability <= Reference value for cycle cycle life of divertor')
    end if
    
    if (n <= div_nref) then
       div_avail = a0
    else if (n >= div_nu) then       
       div_avail = 0.0D0
    else    
       div_avail = (a0/(div_nu-div_nref))*(div_nu - 0.5D0*div_nref**2.0D0/n -0.5D0*n)              
    end if   
    
    u_unplanned_div = 1.0D0 - div_avail  

    if (iprint /= 1) return
    call ocmmnt(outfile,'Divertor:')
    call oblnkl(outfile)   
    call ovarre(outfile,'Probability of failure per operational day', '(div_prob_fail)',div_prob_fail)
    call ovarre(outfile,'Repair time (years)', '(div_umain_time)',div_umain_time)
    call ovarre(outfile,'Reference value for cycle life', '(div_nref)',div_nref)
    call ovarre(outfile,'The cycle when failure is with 100% certain', '(div_nu)',div_nu)
    call ovarre(outfile,'Number of cycles between planned replacements', '(n)',n)
    call ovarre(outfile,'Unplanned unavailability', '(u_unplanned_div)', u_unplanned_div)   
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_divertor

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_fwbs(outfile, iprint, u_unplanned_fwbs)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the first wall and blanket
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_fwbs : output real : unplanned unavailability of first wall and blanket
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_hist  08/12/14 JM  Corrections to calculation for blanket lifetime
    !+ad_hist  09/02/15 JM  More corrections to calculation for divertor lifetime
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_fwbs

    !  Local Variables

    real(kind(1.0D0)) ::  fwbs_main_time
    real(kind(1.0D0)) :: a0, fwbs_avail, n, pf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate cycle limit in terms of days
    
	 ! MDK Rewrote routine to match the engineering paper
    ! Number of cycles between planned blanket replacements, N
    n = bktlife * year / tcycle

    ! The probability of failure in one pulse cycle 
    ! (before the reference cycle life)    
    pf = (fwbs_prob_fail / day) * tcycle
    a0 = 1.0D0 - pf * fwbs_umain_time * year / tcycle
 
    if (fwbs_nu <= fwbs_nref) then
        write(*,*) 'fwbs_nu <= fwbs_nref'
        write(*,*) 'The cycle when the blanket fails with 100% probability <= Reference value for cycle life of blanket'
        call ocmmnt(outfile,'EROROR: The cycle when the blanket fails with 100% probability <= Reference value for cycle life of blanket')
    end if
 
    !  Integrating the instantaneous availability gives the mean 
    !  availability over the planned cycle life N
    if (n <= fwbs_nref) then
       fwbs_avail = a0
    else if (n >= fwbs_nu) then       
       fwbs_avail = 0.0D0
    else    
       fwbs_avail = (a0/(fwbs_nu-fwbs_nref))*(fwbs_nu - 0.5D0*fwbs_nref**2.0D0/n -0.5D0*n)              
    end if   
    
    u_unplanned_fwbs = 1.0D0 - fwbs_avail  
   
    if (iprint /= 1) return
    call ocmmnt(outfile,'First wall / Blanket:')
    call oblnkl(outfile)
    call ovarre(outfile,'Probability of failure per operational day', '(fwbs_prob_fail)',fwbs_prob_fail)
    call ovarre(outfile,'Repair time (years)', '(fwbs_umain_time)',fwbs_umain_time)
    call ovarre(outfile,'Reference value for cycle life', '(fwbs_nref)',fwbs_nref)
    call ovarre(outfile,'The cycle when failure is with 100% certain', '(fwbs_nu)',fwbs_nu)
    call ovarre(outfile,'Number of cycles between planned replacements', '(n)',n)
    call ovarre(outfile,'Unplanned unavailability', '(u_unplanned_fwbs)', u_unplanned_fwbs)
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_fwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_bop(outfile, iprint, u_unplanned_bop)

    !+ad_name  calc_u_unplanned_bop
    !+ad_summ  Calculates the unplanned unavailability of the balance of plant
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_bop : output real : unplanned unavailability of balance of plant
    !+ad_desc  This routine calculates the unplanned unavailability of the balance of plant,
    !+ad_desc  using the methodology outlined in the 2014 EUROfusion
    !+ad_desc  RAMI report.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_bop

    !  Local variables

    real(kind(1.0D0)) :: bop_fail_rate, bop_mttr
    integer :: bop_num_failures

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Balance of plant failure rate (failures per hour)
    !  ENEA study WP13-DTM02-T01

    bop_fail_rate = 9.39D-5

    !  Number of balance of plant failures in plant operational lifetime

    bop_num_failures = nint(bop_fail_rate * 365.25D0 * 24.0D0 * t_operation)

    !  Balance of plant mean time to repair (years)
    !  ENEA study WP13-DTM02-T01

    bop_mttr = 96.0D0 / (24.0D0 * 365.25D0)

    !  Unplanned downtime balance of plant

    u_unplanned_bop = (bop_mttr * bop_num_failures)/(t_operation)

    if (iprint /= 1) return

    call ocmmnt(outfile,'Balance of plant:')
    call oblnkl(outfile)
    call ovarre(outfile,'Failure rate (1/h)', &
         '(bop_fail_rate)',bop_fail_rate)
    call ovarin(outfile,'Number of failures in lifetime', &
         '(bop_num_failures)',bop_num_failures)
    call ovarre(outfile,'Balance of plant MTTR', &
         '(bop_mttr)',bop_mttr)
    call ovarre(outfile,'Balance of plant unplanned unavailability', &
         '(u_unplanned_bop)',u_unplanned_bop)
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_bop

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_hcd(outfile, iprint, u_unplanned_hcd)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the heating and current drive system
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  u_unplanned_hcd : output real : unplanned unavailability of hcd
    !+ad_desc  This routine calculates the unplanned unavailability of the heating
    !+ad_desc  and current drive system, using the methodology outlined in the
    !+ad_desc  2014 EUROfusion RAMI report.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_hcd

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Currently just a fixed value until more information available or Q. Tran's response provides
    !  useful data.

    u_unplanned_hcd = 0.02D0

  end subroutine calc_u_unplanned_hcd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_vacuum(outfile, iprint,&
       & u_unplanned_vacuum)

    !+ad_name  calc_u_unplanned_fwbs
    !+ad_summ  Calculates the unplanned unavailability of the vacuum
    ! system
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output
    ! file (1=yes)
    !+ad_args  u_unplanned_vacuum : output real : unplanned
    ! unavailability of vacuum system
    !+ad_desc  This routine calculates the unplanned unavailability
    ! of the vacuum system,
    !+ad_desc  using the methodology outlined in the 2014 EUROfusion
    !+ad_desc  RAMI report.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  gamfun
    !+ad_hist  02/12/14 JM  Initial version
    !+ad_hist  18/12/14 JM  Corrected calculation now with binomial coefficients
    !+ad_stat  Okay
    !+ad_docs  2014 EUROfusion RAMI report, &quot;Availability in
    ! PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_vacuum

    ! Local variables

    integer :: i, j, k, total_pumps
    real(kind(1.0D0)) :: cryo_failure_rate, num_redundancy_pumps, cryo_main_time
    real(kind(1.0D0)) :: cryo_nfailure_rate, t_down
    real(kind(1.0D0)) :: pump_failures, n_shutdown, t_op_bt, sum_vals

    real(kind(1.0D0)), dimension(vpumpn + redun_vac + 1) :: coefficients, vac_fail_pdf
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Number of shutdowns
    n_shutdown = anint((tlife - t_operation)/ ((21.0D0 *&
         & num_rh_systems**(-0.9D0) + 2.0D0) / 12.0D0))

    !  Operational time between shutdowns
    t_op_bt = t_operation/(n_shutdown + 1.0D0)

    !  Cryopump maintenance time
    cryo_main_time = 1.0D0/6.0D0

    !  Total pumps = pumps + redundant pumps
    total_pumps = vpumpn + redun_vac

    !  Cryopump failure rate per machine operational period
    !  From "Selected component failure rate values from fusion 
    !  safety assessment tasks", Cadwallader (1994)
     
    !  probability of pump failure per operational period
    cryo_failure_rate = 2.0D-6 * 365.25D0 * 24.0D0 * t_op_bt

    !  probability of no pump failure per operational period
    cryo_nfailure_rate = 1.0D0 - cryo_failure_rate

    !  binomial coefficients
    do i = 0, total_pumps
       coefficients(i+1) = gamfun(total_pumps+1.0D0)/(gamfun(i+1.0D0)*&
            gamfun(total_pumps - i + 1.0D0))
    end do

    !  calculate probability density function
    do j = 0, total_pumps
       vac_fail_pdf(j+1) = coefficients(j+1)*(cryo_nfailure_rate**(total_pumps-j))*(cryo_failure_rate**j)
    end do

    !  calculate sum
    sum_vals = 0.0D0
    do k = 0, total_pumps
       if (k > redun_vac) then
          sum_vals = sum_vals + ((k - redun_vac)*vac_fail_pdf(k+1))
       end if
    end do

    !  Number of pump failures in one operational period.
    pump_failures = sum_vals
    ! Total down time in reactor life
    t_down = sum_vals*cryo_main_time*(n_shutdown + 1.0D0)
    
    !  Total vacuum unplanned unavailability
    u_unplanned_vacuum = max(0.005, t_down / (t_operation + t_down))
    
    if (iprint /= 1) return

    call ocmmnt(outfile,'Vacuum:')    
    call oblnkl(outfile)
    call ovarin(outfile,'Number of pumps (excluding redundant pumps)', '(vpumpn)', vpumpn)
    call ovarin(outfile,'Number of redundant pumps', '(redun_vac)', redun_vac)
    call ovarre(outfile,'Expected number of pump failures in one operational period', '(pump_failures)', pump_failures)
    call ovarre(outfile,'Vacuum unplanned unavailability', '(u_unplanned_vacuum)', u_unplanned_vacuum)
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_vacuum

end module availability_module
