!  $Id:: structure.f90 152 2013-04-09 13:52:00Z pknight                 $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module structure_module

  !+ad_name  structure_module
  !+ad_summ  Module containing support structure calculations
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  strucall
  !+ad_cont  struct
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the support structure for a
  !+ad_desc  fusion power plant.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use divertor_variables
  use fwbs_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use structure_variables
  use tfcoil_variables

  implicit none

  private
  public :: strucall, struct

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine strucall(outfile,iprint)

    !+ad_name  strucall
    !+ad_summ  Structure calculation caller
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calls the support structure mass calculations.
    !+ad_prob  None
    !+ad_call  struct
    !+ad_hist  28/07/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  24/06/14 PJK Removed refs to bucking cylinder
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: twhtpf

    !  Total weight of the PF coil conductor and its structure

    twhtpf = whtpf + whtpfs

    call struct(plascur,rmajor,rminor,kappa,bt,i_tf_sup,ipfres,dr_tf_inner_bore+tfthko+tfcth, &
         hmax,whtshld,divmas,twhtpf,whttf,fwmass,whtblkt,coolmass, &
         dewmkg,outfile,iprint,fncmass,aintmass,clgsmass,coldmass, &
         gsmass)

  end subroutine strucall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine struct(ai,r0,a,akappa,b0,i_tf_sup,ipfres,tf_h_width,tfhmax, &
       shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass, &
       dewmass,outfile,iprint,fncmass,aintmass,clgsmass,coldmass, &
       gsm)

    !+ad_name  struct
    !+ad_summ  Module to calculate mass of support structure
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Galambos, ORNL
    !+ad_cont  N/A
    !+ad_args  ai : input real : plasma current (max design value) (A)
    !+ad_args  r0 : input real : plasma major radius (m)
    !+ad_args  a : input real : plasma minor radius (m)
    !+ad_args  akappa : input real : plasma elongation
    !+ad_args  b0 : input real : axial B-field (T)
    !+ad_args  i_tf_sup : input integer : switch denoting whether TF coils
    !+ad_argc                           are superconducting
    !+ad_args  ipfres : input integer : switch denoting whether PF coils
    !+ad_argc                           are resistive
    !+ad_args  tf_h_width : input real : TF coil horizontal bore (m)
    !+ad_args  tfhmax : input real : TF coil max height (m)
    !+ad_args  shldmass : input real : total mass of shield (kg)
    !+ad_args  dvrtmass : input real : total mass of divertor and assoc. structure (kg)
    !+ad_args  pfmass : input real : total mass of PF coils plus cases (kg)
    !+ad_args  tfmass : input real : total mass of TF coils plus cases (kg)
    !+ad_args  blmass : input real : blanket mass (kg)
    !+ad_args  fwmass : input real : first wall mass (kg)
    !+ad_args  coolmass : input real : total water coolant mass (kg)
    !+ad_args  dewmass : input real : vacuum vessel + cryostat mass (kg)
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  fncmass : output real : mass of outer pf coil support fence (kg)
    !+ad_args  aintmass : output real : mass of intercoil support (kg)
    !+ad_args  clgsmass : output real : coil gravity support mass (kg)
    !+ad_args  coldmass : output real : total mass of cryogenic temp. stuff (kg)
    !+ad_args  gsm : output real : gravity support for magnets, and shield/blanket (kg)
    !+ad_desc  Reprogrammed by J. Galambos to match the ITER (i.e. B. Spears) rules.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  28/07/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  09/04/13 PJK Comment changes
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  24/06/14 PJK Removed wtbc argument
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: ai,r0,a,akappa,b0,tf_h_width,tfhmax, &
         shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass,dewmass
    integer, intent(in) :: outfile,iprint,i_tf_sup,ipfres
    real(kind(1.0D0)), intent(out) :: fncmass,aintmass,clgsmass,coldmass,gsm

    !  Local variables

    real(kind(1.0D0)) :: dens,gsm1,gsm2,gsm3,sigal,ws1,ws2,coilmass

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Outer PF coil fence (1990 ITER fit)
    fncmass = 2.1D-11*ai*ai*r0*akappa*a

    !  Intercoil support between TF coils to react overturning moment
    !  (scaled to 1990 ITER fit)
    aintmass = 1.4D6 * (ai/2.2D7) * b0/4.85D0 * tf_h_width**2/50.0D0

    !  Total mass of coils plus support plus vacuum vessel + cryostat
    coilmass = tfmass + pfmass + aintmass + dewmass

    !  Total mass of cooled components
    coldmass = 0.0D0
    if (i_tf_sup == 1) coldmass = coldmass + tfmass + aintmass + dewmass
    if (ipfres /= 1) coldmass = coldmass + pfmass

    !  Coil gravity support mass
    !  Set density (kg/m3) and allowable stress (Pa)

    dens = 7.8D3 ; sigal = 2.5D7
    clgsmass = coilmass * (r0/6.0D0)*9.1D0 * 9.807D0*dens/sigal

    !  Gravity support masses scaled from Spears algorithms (9/90) :

    !  Torus leg support

    ws1 = coolmass + fwmass + blmass + shldmass + dvrtmass
    gsm1 = 5.0D0 * 9.807D0 * ws1 * dens/sigal

    !  Ring beam

    ws2 = ws1 + tfmass + pfmass + aintmass + clgsmass
    gsm2 = 1.0D-3 * 34.77D0 * (r0+1.0D0) * sqrt(0.001D0*ws2)*dens

    !  Ring legs (JG: this term may be too big, need to check)

    gsm3 = 1.0D-6 * 0.3D0 * (tfhmax + 2.0D0) * ws2 * dens

    gsm = gsm1 + gsm2 + gsm3

    !  Output section

    if (iprint == 0) return

    call oheadr(outfile,'Support Structure')
    call ovarre(outfile,'Outer PF coil fence mass (kg)', '(fncmass)',fncmass, 'OP ')
    call ovarre(outfile,'Intercoil support structure mass (kg)', '(aintmass)',aintmass, 'OP ')
    call ovarre(outfile,'Mass of cooled components (kg)', '(coldmass)',coldmass, 'OP ')
    call ovarre(outfile,'Gravity support structure mass (kg)', '(clgsmass)',clgsmass, 'OP ')
    call ovarre(outfile,'Torus leg support mass (kg)','(gsm1)',gsm1, 'OP ')
    call ovarre(outfile,'Ring beam mass (kg)','(gsm2)',gsm2, 'OP ')
    call ovarre(outfile,'Ring legs mass (kg)','(gsm3)',gsm3, 'OP ')

  end subroutine struct

end module structure_module
