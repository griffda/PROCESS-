! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module availability_module

  !! Module containing plant availability routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! plant availability and component lifetimes for a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Modules to import !
  ! !!!!!!!!!!!!!!!!!!!!

  use cost_variables
  use divertor_variables
  use fwbs_variables
  use ife_variables
  use physics_variables
  use process_output
  use pulse_variables
  use tfcoil_variables
  use times_variables
  use vacuum_variables
  use maths_library
  use global_variables

  use iso_c_binding
  implicit none

  ! Module subroutine and variable declarations !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  private
  public :: avail
  public :: avail_2
  real(kind(1.0D0)), parameter :: year = 31557600.0D0
  real(kind(1.0D0)), parameter :: day = 86400.0D0

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine avail(outfile,iprint)

    !! Routine to calculate component lifetimes and the overall plant availability
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the component lifetimes and the overall
    !! plant availability.
    !! F/PL/PJK/PROCESS/CODE/043
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments !
    ! !!!!!!!!!!!!!

    integer, intent(in) :: outfile,iprint

    !  Local variables !
    ! !!!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: lb, ld, td
    real(kind(1.0D0)), save :: uplanned, uutot
    integer :: n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Full power lifetime (in years) !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ife /= 1) then

    ! First wall / blanket lifetime (years)

    ! TODO MDK Do this calculation whatever the value of blktmodel (whatever that is)
    ! For some reason fwlife is not always calculated, so ignore it if it is still zero.
    if (fwlife < 0.0001D0) then
        bktlife = min(abktflnc/wallmw, tlife)
    else
        bktlife = min(fwlife, abktflnc/wallmw, tlife)
    end if

! SJP Issue #834
! Add a test for hldiv=0

    if (hldiv < 1.0d-10) hldiv=1.0d-10

    ! Divertor lifetime (years)
    divlife = max(0.0, min(adivflnc/hldiv, tlife))

    ! Centrepost lifetime (years) (ST machines only)
    if (itart == 1) then
      cplife = min(cpstflnc/wallmw, tlife)
    end if

    end if

    ! Plant Availability (iavail=0,1) !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! if iavail = 0 use input value for cfactr

    ! Taylor and Ward 1999 model (iavail=1)
    if (iavail == 1) then

       ! Which component has the shorter life?
       if (divlife < bktlife) then
          ld = divlife
          lb = bktlife
          td = tdivrepl
       else
          ld = bktlife
          lb = divlife
          td = tbktrepl
       end if

       ! Number of outages between each combined outage
       n = ceiling(lb/ld) - 1

       ! Planned unavailability
       uplanned = (n*td + tcomrepl) / ( (n+1)*ld + (n*td + tcomrepl) )

       ! Unplanned unavailability
       ! Rather than simply summing the individual terms, the following protects
       ! against the total availability becoming zero or negative

       uutot = uubop                            ! balance of plant
       uutot = uutot + (1.0D0 - uutot)*uucd     ! current drive
       uutot = uutot + (1.0D0 - uutot)*uudiv    ! divertor
       uutot = uutot + (1.0D0 - uutot)*uufuel   ! fuel system
       uutot = uutot + (1.0D0 - uutot)*uufw     ! first wall + blanket
       uutot = uutot + (1.0D0 - uutot)*uumag    ! magnets
       uutot = uutot + (1.0D0 - uutot)*uuves    ! vacuum vessel

       ! Total availability
       cfactr = 1.0D0 - (uplanned + uutot - (uplanned*uutot))

    end if

    ! Capacity factor
    ! Using the amount of time burning for a given pulse cycle
    cpfact = cfactr * (tburn / tcycle)

    ! Modify lifetimes to take account of the availability

    if (ife /= 1) then

       ! First wall / blanket
       if (bktlife < tlife) then
          bktlife = min( bktlife/cfactr, tlife )
       end if

       ! Divertor
       if (divlife < tlife) then
          divlife = min( divlife/cfactr, tlife )
       end if

       ! Centrepost
       if ((itart == 1).and.(cplife < tlife)) then
          cplife = min( cplife/cfactr, tlife )
       end if

    end if

    ! Current drive system lifetime (assumed equal to first wall and blanket lifetime)
    cdrlife = bktlife

    if (iprint /= 1) return

    !  Output section !
    ! !!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Plant Availability')
    if (blktmodel == 0) then
       call ovarre(outfile,'Allowable blanket neutron fluence (MW-yr/m2)', '(abktflnc)', abktflnc)
    end if
    call ovarre(outfile,'Allowable divertor heat fluence (MW-yr/m2)', '(adivflnc)', adivflnc)
    call ovarre(outfile,'First wall / blanket lifetime (years)', '(bktlife)', bktlife, 'OP ')
    call ovarre(outfile,'Divertor lifetime (years)', '(divlife)', divlife, 'OP ')

    if (itart == 1) then
       call ovarre(outfile,'Centrepost lifetime (years)', '(cplife)', cplife, 'OP ')
    end if

    call ovarre(outfile,'Heating/CD system lifetime (years)', '(cdrlife)', cdrlife, 'OP ')
    call ovarre(outfile,'Total plant lifetime (years)', '(tlife)', tlife)

    if (iavail == 1) then
       if (divlife < bktlife) then
          call ovarre(outfile,'Time needed to replace divertor (years)', '(tdivrepl)', tdivrepl)
       else
          call ovarre(outfile,'Time needed to replace blanket (years)', '(tbktrepl)', tbktrepl)
       end if
       call ovarre(outfile,'Time needed to replace blkt + div (years)', '(tcomrepl)', tcomrepl)
       call ovarre(outfile,'Planned unavailability fraction', '(uplanned)', uplanned, 'OP ')
       call ovarre(outfile,'Unplanned unavailability fraction', '(uutot)', uutot, 'OP ')
    end if

    if (iavail == 0) then
        call ovarre(outfile,'Total plant availability fraction', '(cfactr)', cfactr)
    else
        call ovarre(outfile,'Total plant availability fraction', '(cfactr)', cfactr, 'OP ')
    end if

  end subroutine avail

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine avail_2(outfile,iprint)

    !! Routine to calculate component lifetimes and the overall plant availability
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the component lifetimes and the overall
    !! plant availability using an updated model linked to the 2014 EUROfusion
    !! RAMI task
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments !
    ! !!!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint

    !  Local variables !
    ! !!!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: u_planned
    real(kind(1.0D0)) :: u_unplanned
    real(kind(1.0D0)) :: u_unplanned_magnets
    real(kind(1.0D0)) :: u_unplanned_div
    real(kind(1.0D0)) :: u_unplanned_fwbs
    real(kind(1.0D0)) :: u_unplanned_bop
    real(kind(1.0D0)) :: u_unplanned_hcd
    real(kind(1.0D0)) :: u_unplanned_vacuum

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Plant Availability !
    ! !!!!!!!!!!!!!!!!!!!!!!

    ! Planned unavailability

    call calc_u_planned(outfile, iprint, u_planned)

    ! Operational time (years)
    t_operation = tlife * (1.0D0-u_planned)

    !  Un-planned unavailability

    ! Magnets
    call calc_u_unplanned_magnets(outfile,iprint, u_unplanned_magnets)

    ! Divertor
    call calc_u_unplanned_divertor(outfile,iprint, u_unplanned_div)

    ! First wall and blanket
    call calc_u_unplanned_fwbs(outfile,iprint, u_unplanned_fwbs)

    ! Balance of plant
    call calc_u_unplanned_bop(outfile,iprint, u_unplanned_bop)

    ! Heating and current drive
    call calc_u_unplanned_hcd(u_unplanned_hcd)

    ! Vacuum systems

    ! Number of redundant pumps
    redun_vac = floor(vpumpn*redun_vacp/100.0 + 0.5D0)

    call calc_u_unplanned_vacuum(outfile,iprint, u_unplanned_vacuum)

    ! Total unplanned unavailability
    u_unplanned = min(1.0D0, u_unplanned_magnets + &
         u_unplanned_div + u_unplanned_fwbs + &
         u_unplanned_bop + u_unplanned_hcd + u_unplanned_vacuum)

    ! Total availability
    cfactr = max(1.0D0 - (u_planned + u_unplanned + u_planned*u_unplanned), 0.0D0)

    ! Capacity factor
    cpfact = cfactr * (tburn / tcycle)

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return

    call ocmmnt(outfile,'Total unavailability:')
    call oblnkl(outfile)
    call ovarre(outfile,'Total planned unavailability', '(u_planned)', u_planned, 'OP ')
    call ovarre(outfile,'Total unplanned unavailability', '(u_unplanned)', u_unplanned, 'OP ')
    call oblnkl(outfile)
    call ovarre(outfile,'Total plant availability fraction', '(cfactr)',cfactr, 'OP ')
    call ovarre(outfile,'Total DT operational time (years)','(t_operation)',t_operation, 'OP ')
    call ovarre(outfile,'Total plant lifetime (years)','(tlife)',tlife)
    call ovarre(outfile,'Capacity factor: total lifetime electrical energy output / output power','(cpfact)',cpfact, 'OP ')

  end subroutine avail_2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_planned(outfile, iprint, u_planned) &
   bind(C, name = "c_calc_u_planned")

    !! Calculates the planned unavailability of the plant
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_planned : output real : planned unavailability of plant
    !! This routine calculates the planned unavailability of the
    !! plant, using the methodology outlined in the 2014 EUROfusion
    !! RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_planned

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: mttr_blanket, mttr_divertor, mttr_shortest
    real(kind(1.0D0)) :: lifetime_shortest, lifetime_longest
    integer :: n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Full power lifetimes (in years) !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       ! First wall / blanket lifetime (years)
       bktlife =  min(abktflnc/wallmw, tlife)

       ! Divertor lifetime (years)
       divlife = min( adivflnc/hldiv, tlife )

       ! Centrepost lifetime (years) (ST only)
       if (itart == 1) then
          cplife = min( cpstflnc/wallmw, tlife )
       end if

    ! Current drive lifetime (assumed equal to first wall and blanket lifetime)
    cdrlife = bktlife

    ! Calculate the blanket and divertor replacement times !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Blanket replacement time
    ! ( Calculated using scaling from 2014 EUROfusion RAMI report )

    ! Mean time to repair blanket is same as replacing both blanket and divertor.
    ! The +2.0 at the end is for the 1 month cooldown and pump down at either end
    ! of the maintenance period
    mttr_blanket = (21.0D0 * num_rh_systems**(-0.9D0) + 2.0D0) / 12.0D0
    

    ! Mean time to repair divertor is 70% of time taken to replace blanket
    ! This is taken from Oliver Crofts 2014 paper
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

    ! Number of outages between each combined outage
    n = ceiling(lifetime_longest/lifetime_shortest) - 1

    ! Planned unavailability
    u_planned = (n*mttr_shortest + mttr_blanket) / &
         ( (n+1)*lifetime_shortest + (n*mttr_shortest + mttr_blanket) )

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return

    call oheadr(outfile,'Plant Availability (2014 Model)')

    call ocmmnt(outfile,'Planned unavailability:')
    call oblnkl(outfile)
    call ovarre(outfile,'Allowable blanket neutron fluence (MW-yr/m2)', '(abktflnc)', abktflnc)
    call ovarre(outfile,'Allowable divertor heat fluence (MW-yr/m2)', '(adivflnc)', adivflnc)
    call ovarre(outfile,'First wall / blanket lifetime (FPY)', '(bktlife)', bktlife, 'OP ')
    call ovarre(outfile,'Divertor lifetime (FPY)', '(divlife)', divlife, 'OP ')
    
    call ovarin(outfile,'Number of remote handling systems', '(num_rh_systems)', num_rh_systems)
    call ovarre(outfile,'Time needed to replace divertor (years)', '(mttr_divertor)', mttr_divertor)
    call ovarre(outfile,'Time needed to replace blanket (years)', '(mttr_blanket)', mttr_blanket)
    call ovarre(outfile,'Time needed to replace blkt + div (years)', '(mttr_blanket)', mttr_blanket)
    call ovarre(outfile,'Total planned unavailability', '(uplanned)', u_planned, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_planned

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_magnets(outfile, iprint, u_unplanned_magnets) &
   bind(C, name = "c_calc_u_unplanned_magnets")

    !! Calculates the unplanned unavailability of the magnets
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_magnets : output real : unplanned unavailability of magnets
    !! This routine calculates the unplanned unavailability of the magnets,
    !! using the methodology outlined in the 2014 EUROfusion
    !! RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_magnets

    ! Local Variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: mag_temp_marg_limit, mag_temp_marg, mag_main_time
    real(kind(1.0D0)) :: mag_min_u_unplanned, start_of_risk, t_life
    real(kind(1.0D0)) :: tmargmin

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Magnet temperature margin limit (K)
    ! Use the lower of the two values.  Issue #526
    tmargmin = min(tmargmin_tf,tmargmin_cs)
    mag_temp_marg_limit = tmargmin

    ! Magnet temperature margin (K)
    mag_temp_marg = temp_margin

    ! Magnet maintenance time (years)
    mag_main_time = 0.5D0

    ! Minimum unplanned unavailability
    mag_min_u_unplanned = mag_main_time / (t_operation + mag_main_time)

    ! Point at which risk of unplanned unavailability increases
    ! conf_mag is the c factor, which determines the temperature margin at which lifetime starts to decline.
    start_of_risk = mag_temp_marg_limit / conf_mag

    ! Determine if temperature margin is in region with risk of unplanned unavailability
    if (temp_margin >= start_of_risk) then

       u_unplanned_magnets = mag_min_u_unplanned

    else

       ! Linear decrease in expected lifetime when approaching the limit
       t_life = max( 0.0D0, (t_operation/(start_of_risk - tmargmin))*(temp_margin - tmargmin) )
       u_unplanned_magnets = mag_main_time/(t_life + mag_main_time)

    end if

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return

    call ocmmnt(outfile,'Magnets:')
    call oblnkl(outfile)
    call ovarre(outfile,'Minimum temperature margin (K)', '(tmargmin)', tmargmin)
    call ovarre(outfile,'c parameter, determining the temperature margin where lifetime declines', '(conf_mag)', conf_mag)
    call ovarre(outfile,'Temperature Margin (K)', '(temp_margin)', temp_margin, 'OP ')
    call ovarre(outfile,'Magnets unplanned unavailability', '(u_unplanned_magnets)', u_unplanned_magnets, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_magnets

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_divertor(outfile, iprint, u_unplanned_div) & 
   bind(C, name = "c_calc_u_unplanned_divertor")

    !! Calculates the unplanned unavailability of the divertor
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_div : output real : unplanned unavailability of divertor
    !! 
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_div

    ! Local Variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: a0, div_avail, n, pf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate cycle limit in terms of days
    ! Number of cycles between planned blanket replacements, N
    n = divlife * year / tcycle

    ! The probability of failure in one pulse cycle (before the reference cycle life)
    pf = (div_prob_fail / day) * tcycle
    a0 = 1.0D0 - pf * div_umain_time * year / tcycle

    ! Integrating the instantaneous availability gives the mean
    ! availability over the planned cycle life N

    if (div_nu <= div_nref) then
        write(*,*) 'div_nu <= div_nref'
        write(*,*) 'The cycle when the divertor fails with 100% probability <= Reference value for cycle life of divertor'
        call ocmmnt(outfile,'ERROR: The cycle when the divertor fails with 100% probability&
            & <= Reference value for cycle cycle life of divertor')
    end if

    ! Check number of cycles

    ! Less than reference (availability is min availability)
    if (n <= div_nref) then

       div_avail = a0

    ! Greater than cycle number with 100% failure rate
    else if (n >= div_nu) then

       div_avail = 0.0D0

    ! Else number of cycles is inbetween and is given by formula below
    else

       div_avail = (a0/(div_nu-div_nref))*(div_nu - 0.5D0*div_nref**2.0D0/n -0.5D0*n)

    end if

    ! Unplanned unavailability for divertor
    u_unplanned_div = 1.0D0 - div_avail

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return
    call ocmmnt(outfile,'Divertor:')
    call oblnkl(outfile)
    call ovarre(outfile,'Probability of failure per operational day', '(div_prob_fail)',div_prob_fail)
    call ovarre(outfile,'Repair time (years)', '(div_umain_time)',div_umain_time)
    call ovarre(outfile,'Reference value for cycle life', '(div_nref)',div_nref)
    call ovarre(outfile,'The cycle when failure is 100% certain', '(div_nu)',div_nu)
    call ovarre(outfile,'Number of cycles between planned replacements', '(n)',n)
    call ovarre(outfile,'Unplanned unavailability', '(u_unplanned_div)', u_unplanned_div, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_divertor

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_fwbs(outfile, iprint, u_unplanned_fwbs) &
   bind(C, name = "c_calc_u_unplanned_fwbs")

    !! Calculates the unplanned unavailability of the first wall and blanket
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_fwbs : output real : unplanned unavailability of first wall and blanket
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_fwbs

    ! Local Variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: a0, fwbs_avail, n, pf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate cycle limit in terms of days !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Number of cycles between planned blanket replacements, N
    n = bktlife * year / tcycle

    ! The probability of failure in one pulse cycle
    ! (before the reference cycle life)
    pf = (fwbs_prob_fail / day) * tcycle
    a0 = 1.0D0 - pf * fwbs_umain_time * year / tcycle

    if (fwbs_nu <= fwbs_nref) then
        write(*,*) 'fwbs_nu <= fwbs_nref'
        write(*,*) 'The cycle when the blanket fails with 100% probability <= Reference value for cycle life of blanket'
        call ocmmnt(outfile,'EROROR: The cycle when the blanket fails with 100% probability&
            & <= Reference value for cycle life of blanket')
    end if

    ! Integrating the instantaneous availability gives the mean
    ! availability over the planned cycle life N
    if (n <= fwbs_nref) then
       fwbs_avail = a0
    else if (n >= fwbs_nu) then
       fwbs_avail = 0.0D0
    else
       fwbs_avail = (a0/(fwbs_nu-fwbs_nref))*(fwbs_nu - 0.5D0*fwbs_nref**2.0D0/n -0.5D0*n)
    end if

    ! First wall / blanket unplanned unavailability
    u_unplanned_fwbs = 1.0D0 - fwbs_avail

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return
    call ocmmnt(outfile,'First wall / Blanket:')
    call oblnkl(outfile)
    call ovarre(outfile,'Probability of failure per operational day', '(fwbs_prob_fail)',fwbs_prob_fail)
    call ovarre(outfile,'Repair time (years)', '(fwbs_umain_time)',fwbs_umain_time)
    call ovarre(outfile,'Reference value for cycle life', '(fwbs_nref)',fwbs_nref)
    call ovarre(outfile,'The cycle when failure is 100% certain', '(fwbs_nu)',fwbs_nu)
    call ovarre(outfile,'Number of cycles between planned replacements', '(n)',n)
    call ovarre(outfile,'Unplanned unavailability', '(u_unplanned_fwbs)', u_unplanned_fwbs, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_fwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_bop(outfile, iprint, u_unplanned_bop) &
   bind(c,name= "c_calc_u_unplanned_bop")
    !! Calculates the unplanned unavailability of the balance of plant
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_bop : output real : unplanned unavailability of balance of plant
    !! This routine calculates the unplanned unavailability of the balance of plant,
    !! using the methodology outlined in the 2014 EUROfusion
    !! RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_bop

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: bop_fail_rate, bop_mttr
    integer :: bop_num_failures

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Balance of plant failure rate (failures per hour)
    ! ENEA study WP13-DTM02-T01
    bop_fail_rate = 9.39D-5

    ! Number of balance of plant failures in plant operational lifetime
    bop_num_failures = nint(bop_fail_rate * 365.25D0 * 24.0D0 * t_operation)

    ! Balance of plant mean time to repair (years)
    ! ENEA study WP13-DTM02-T01
    bop_mttr = 96.0D0 / (24.0D0 * 365.25D0)

    ! Unplanned downtime balance of plant
    u_unplanned_bop = (bop_mttr * bop_num_failures)/(t_operation)

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return
    call ocmmnt(outfile,'Balance of plant:')
    call oblnkl(outfile)
    call ovarre(outfile,'Failure rate (1/h)', '(bop_fail_rate)', bop_fail_rate)
    call ovarin(outfile,'Number of failures in lifetime', '(bop_num_failures)', bop_num_failures, 'OP ')
    call ovarre(outfile,'Balance of plant MTTR', '(bop_mttr)', bop_mttr)
    call ovarre(outfile,'Balance of plant unplanned unavailability', '(u_unplanned_bop)', u_unplanned_bop, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_bop

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_hcd(u_unplanned_hcd) &
   bind(C,name="c_calc_u_unplanned_hcd")

    !! Calculates the unplanned unavailability of the heating and current drive system
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_hcd : output real : unplanned unavailability of hcd
    !! This routine calculates the unplanned unavailability of the heating
    !! and current drive system, using the methodology outlined in the
    !! 2014 EUROfusion RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    real(kind(1.0D0)), intent(out) :: u_unplanned_hcd

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Currently just a fixed value until more information available or Q.
    ! Tran's response provides useful data.
    u_unplanned_hcd = 0.02D0

  end subroutine calc_u_unplanned_hcd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_vacuum(outfile, iprint, u_unplanned_vacuum)

    !! Calculates the unplanned unavailability of the vacuum system
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_vacuum : output real : unplanned unavailability of vacuum system
    !! This routine calculates the unplanned unavailability of the vacuum system,
    !! using the methodology outlined in the 2014 EUROfusion
    !! RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in
    ! PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: outfile, iprint
    real(kind(1.0D0)), intent(out) :: u_unplanned_vacuum

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    integer :: total_pumps, n
    real(kind(1.0D0)) :: cryo_failure_rate, cryo_main_time
    real(kind(1.0D0)) :: cryo_nfailure_rate, t_down
    real(kind(1.0D0)) :: n_shutdown, t_op_bt, sum_prob

    real(kind(1.0D0)), dimension(vpumpn + redun_vac + 1) :: vac_fail_p

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Number of shutdowns
    n_shutdown = anint((tlife - t_operation)/ ((21.0D0 *&
         & num_rh_systems**(-0.9D0) + 2.0D0) / 12.0D0))

    ! Operational time between shutdowns
    t_op_bt = t_operation/(n_shutdown + 1.0D0)

    ! Cryopump maintenance time (y) = 2 months
    cryo_main_time = 1.0D0/6.0D0

    ! Total pumps = pumps + redundant pumps
    total_pumps = vpumpn + redun_vac

    ! Cryopump failure rate per machine operational period
    ! From "Selected component failure rate values from fusion
    ! safety assessment tasks", Cadwallader (1994)

    ! probability of pump failure per operational period
    cryo_failure_rate = 2.0D-6 * 365.25D0 * 24.0D0 * t_op_bt

    ! probability of no pump failure per operational period
    cryo_nfailure_rate = 1.0D0 - cryo_failure_rate

    sum_prob = 0.0D0
    do n = redun_vac + 1, total_pumps

        ! Probability for n failures in the operational period, n > number of redundant pumps
        vac_fail_p(n) = binomial(total_pumps,n)*(cryo_nfailure_rate**(total_pumps-n))*(cryo_failure_rate**n)

        ! calculate sum in formula for downtime
        sum_prob = sum_prob + vac_fail_p(n) * (n - redun_vac)

    end do

    ! Total down-time in reactor life
    t_down = (n_shutdown + 1.0D0) * cryo_main_time * sum_prob

    ! Total vacuum unplanned unavailability
    u_unplanned_vacuum = max(0.005, t_down / (t_operation + t_down))

    ! Output !
    ! !!!!!!!!!

    if (iprint /= 1) return
    call ocmmnt(outfile,'Vacuum:')
    call oblnkl(outfile)
    call ovarin(outfile,'Number of pumps (excluding redundant pumps)', '(vpumpn)', vpumpn, 'OP ')
    call ovarin(outfile,'Number of redundant pumps', '(redun_vac)', redun_vac, 'OP ')
    call ovarre(outfile,'Total unplanned down-time due to pumps, excl fixed 0.5% (years)', '(t_down)', t_down, 'OP ')
    call ovarre(outfile,'Vacuum unplanned unavailability', '(u_unplanned_vacuum)', u_unplanned_vacuum, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_vacuum

end module availability_module
