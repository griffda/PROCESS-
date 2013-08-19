!  $Id::                                                                $
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

  implicit none

  private
  public :: avail

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

    if (iprint /= 1) then

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

    else

       !  Output section

       call oheadr(outfile,'Plant Availability')
       if (blktmodel == 0) then
          call ovarre(outfile,'Allowable blanket neut. fluence (MW-yr/m2)', &
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

    end if

  end subroutine avail

end module availability_module
