!  $Id:: structure.f90 152 2013-04-09 13:52:00Z pknight                 $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module structure_module

  !! Module containing support structure calculations
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of the support structure for a
  !! fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  private
  public :: strucall, struct

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine strucall(outfile,iprint)

    !! Structure calculation caller
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine calls the support structure mass calculations.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use build_variables, only: dr_tf_inner_bore, hmax, tfcth, tfthko
    use divertor_variables, only: divmas
    use fwbs_variables, only: coolmass, dewmkg, fwmass, whtblkt, whtshld
    use pfcoil_variables, only: ipfres, whtpf, whtpfs
    use physics_variables, only: bt, kappa, plascur, rmajor, rminor
    use structure_variables, only: aintmass, clgsmass, coldmass, fncmass, &
      gsmass
    use tfcoil_variables, only: i_tf_sup, whttf
    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(dp) :: twhtpf

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

    !! Module to calculate mass of support structure
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Galambos, ORNL
    !! ai : input real : plasma current (max design value) (A)
    !! r0 : input real : plasma major radius (m)
    !! a : input real : plasma minor radius (m)
    !! akappa : input real : plasma elongation
    !! b0 : input real : axial B-field (T)
    !! itfsup : input integer : switch denoting whether TF coils
    !! are superconducting
    !! ipfres : input integer : switch denoting whether PF coils
    !! are resistive
    !! tf_h_width : input real : TF coil horizontal bore (m)
    !! tfhmax : input real : TF coil max height (m)
    !! shldmass : input real : total mass of shield (kg)
    !! dvrtmass : input real : total mass of divertor and assoc. structure (kg)
    !! pfmass : input real : total mass of PF coils plus cases (kg)
    !! tfmass : input real : total mass of TF coils plus cases (kg)
    !! blmass : input real : blanket mass (kg)
    !! fwmass : input real : first wall mass (kg)
    !! coolmass : input real : total water coolant mass (kg)
    !! dewmass : input real : vacuum vessel + cryostat mass (kg)
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! fncmass : output real : mass of outer pf coil support fence (kg)
    !! aintmass : output real : mass of intercoil support (kg)
    !! clgsmass : output real : coil gravity support mass (kg)
    !! coldmass : output real : total mass of cryogenic temp. stuff (kg)
    !! gsm : output real : gravity support for magnets, and shield/blanket (kg)
    !! Reprogrammed by J. Galambos to match the ITER (i.e. B. Spears) rules.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use process_output, only: oheadr, ovarre
    implicit none

    !  Arguments
    real(dp), intent(in) :: ai,r0,a,akappa,b0,tf_h_width,tfhmax, &
         shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass,dewmass
    integer, intent(in) :: outfile,iprint,i_tf_sup,ipfres
    real(dp), intent(out) :: fncmass,aintmass,clgsmass,coldmass,gsm

    !  Local variables

    real(dp) :: dens,gsm1,gsm2,gsm3,sigal,ws1,ws2,coilmass

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
