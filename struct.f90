!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine strucall(outfile,iprint)

  !+ad_name  strucall
  !+ad_summ  Structure module caller
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This subroutine calls the structure module.
  !+ad_prob  None
  !+ad_call  divertor_variables
  !+ad_call  physics_variables
  !+ad_call  build.h90
  !+ad_call  fwblsh.h90
  !+ad_call  pfcoil.h90
  !+ad_call  struccom.h90
  !+ad_call  tfcoil.h90
  !+ad_call  struct
  !+ad_hist  28/07/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use divertor_variables
  use physics_variables

  implicit none

  include 'build.h90'
  include 'fwblsh.h90'
  include 'pfcoil.h90'
  include 'struccom.h90'
  include 'tfcoil.h90'

  !  Arguments

  integer, intent(in) :: outfile,iprint

  !  Local variables

  real(kind(1.0D0)) :: twhtpf

  !  Total weight of the PF coil conductor and its structure

  twhtpf = whtpf + whtpfs

  call struct(plascur,rmajor,rminor,kappa,bt,itfsup,ipfres,tfboreh, &
       hmax,whtshld,divmas,twhtpf,whttf,fwmass,whtblkt,coolmass, &
       wtbc,dewmkg,outfile,iprint,fncmass,aintmass,clgsmass,coldmass, &
       gsmass)

end subroutine strucall

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine struct(ai,r0,a,akappa,b0,itfsup,ipfres,boreh,tfhmax, &
     shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass, &
     wtbc,dewmass,outfile,iprint,fncmass,aintmass,clgsmass,coldmass, &
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
  !+ad_args  itfsup : input integer : switch denoting whether TF coils
  !+ad_argc                           are superconducting
  !+ad_args  ipfres : input integer : switch denoting whether PF coils
  !+ad_argc                           are resistive
  !+ad_args  boreh : input real : TF coil horizontal bore (m)
  !+ad_args  tfhmax : input real : TF coil max height (m)
  !+ad_args  shldmass : input real : total mass of shield (kg)
  !+ad_args  dvrtmass : input real : total mass of divertor and assoc. structure (kg)
  !+ad_args  pfmass : input real : total mass of PF coils plus cases (kg)
  !+ad_args  tfmass : input real : total mass of TF coils plus cases (kg)
  !+ad_args  blmass : input real : blanket mass (kg)
  !+ad_args  fwmass : input real : first wall mass (kg)
  !+ad_args  coolmass : input real : total water coolant mass (kg)
  !+ad_args  wtbc : input real : bucking cylinder mass (kg)
  !+ad_args  dewmass : input real : dewar mass (kg)
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_args  fncmass : output real : mass of outer pf coil support fence (kg)
  !+ad_args  aintmass : output real : mass of intercoil support (kg)
  !+ad_args  clgsmass : output real : coil gravity support mass (kg)
  !+ad_args  coldmass : output real : total mass of cryogenic temp. stuff (kg)
  !+ad_args  gsm : output real : gravity support for magnets, and shield/blanket (kg)
  !+ad_desc  Reprogrammed by J. Galambos to match the ITER (i.e. B. Spears) rules.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  28/07/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: ai,r0,a,akappa,b0,boreh,tfhmax, &
       shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass, &
       wtbc,dewmass
  integer, intent(in) :: outfile,iprint,itfsup,ipfres
  real(kind(1.0D0)), intent(out) :: fncmass,aintmass,clgsmass,coldmass,gsm

  !  Local variables

  real(kind(1.0D0)) :: dens,gsm1,gsm2,gsm3,sigal,ws1,ws2,coilmass

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Outer PF coil fence (1990 ITER fit)

  fncmass = 2.1D-11*ai*ai*r0*akappa*a

  !  Intercoil support between TF coils to react overturning moment
  !  (scaled to 1990 ITER fit)

  aintmass = 1.4D6 * (ai/2.2D7) * b0/4.85D0 * boreh**2/50.0D0

  !  Total mass of coils plus support plus dewar

  coilmass = tfmass + pfmass + aintmass + dewmass + wtbc

  !  Total mass of cooled components

  coldmass = 0.0D0
  if (itfsup == 1) coldmass = coldmass + tfmass + aintmass + dewmass + wtbc
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

  if ((iprint == 0).or.(sect10 == 0)) return

  call oheadr(outfile,'Support Structure')
  call ovarre(outfile,'Outer PF coil fence mass (kg)', &
       '(fncmass)',fncmass)
  call ovarre(outfile,'Intercoil support structure mass (kg)', &
       '(aintmass)',aintmass)
  call ovarre(outfile,'Mass of cooled components (kg)', &
       '(coldmass)',coldmass)
  call ovarre(outfile,'Gravity support structure mass (kg)', &
       '(clgsmass)',clgsmass)
  call ovarre(outfile,'Torus leg support mass (kg)','(gsm1)',gsm1)
  call ovarre(outfile,'Ring beam mass (kg)','(gsm2)',gsm2)
  call ovarre(outfile,'Ring legs mass (kg)','(gsm3)',gsm3)

end subroutine struct
