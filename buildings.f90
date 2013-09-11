!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module buildings_module

  !+ad_name  buildings_module
  !+ad_summ  Module containing plant buildings routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  bldgcall
  !+ad_cont  bldgs
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the fusion power plant buildings.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  pf_power_variables
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  rfp_variables
  !+ad_call  stellarator_variables
  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  24/01/13 PJK Added stellarator_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use fwbs_variables
  use heat_transport_variables
  use pf_power_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use rfp_variables
  use stellarator_variables
  use structure_variables
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: bldgcall

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bldgcall(outfile,iprint)

    !+ad_name  bldgcall
    !+ad_summ  Calls the buildings module
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calls the buildings calculations.
    !+ad_prob  None
    !+ad_call  bldgs
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  29/10/12 PJK Added structure_variables
    !+ad_hist  29/10/12 PJK Added pf_power_variables
    !+ad_hist  30/10/12 PJK Added heat_transport_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added buildings_variables
    !+ad_hist  24/01/13 PJK Corrected cryostat radius for stellarators
    !+ad_hist  09/04/13 PJK Used rdewex instead of tfro+2 for cryostat radius
    !+ad_hist  09/04/13 PJK Modified use of tfmtn to be mass of one TF coil
    !+ad_hist  10/04/13 PJK Modified shield height definition
    !+ad_hist  27/06/13 PJK Used rdewex directly in all cases in call to bldgs
    !+ad_hist  11/09/13 PJK Removed idhe3 from bldgs call
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile

    !  Local variables

    real(kind(1.0D0)) :: tfh,tfmtn,tfri,tfro
    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Find maximum PF coil radius and mass (tonnes)
    !  (already calculated for RFPs)

    if (irfp == 0) then
       pfrmax = 0.0D0
       pfmmax = 0.0D0
       do i=1,ncirt
          pfrmax = max(pfrmax,rb(i) )
          pfmmax = max(pfmmax, (1.0D-3*(wtc(i)+wts(i))) )
       end do
    end if

    tfmtn = 1.0D-3 * whttf/tfno  !  mass per TF coil, tonnes
    tfro = rtot + 0.5D0*tfthko
    tfri = rtfcin - 0.5D0*tfcth
    tfh = (hmax + tfcth)*2.0D0

    !  Reactor vault wall and roof thicknesses are hardwired

    call bldgs(pfrmax,pfmmax,tfro,tfri,tfh,tfmtn,tfno,rsldo, &
         rsldi,2.0D0*(hmax-ddwi-vgap2),whtshld,rdewex,helpow,iprint, &
         outfile,cryvol,volrci,rbvol,rmbvol,wsvol,elevol)

  end subroutine bldgcall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bldgs(pfr,pfm,tfro,tfri,tfh,tfm,tfno,shro,shri, &
       shh,shm,crr,helpow,iprint,outfile,cryv,vrci,rbv,rmbv,wsv,elev)

    !+ad_name  bldgs
    !+ad_summ  Determines the sizes of the plant buildings
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  P C Shipe, ORNL
    !+ad_cont  N/A
    !+ad_args  pfr : input/output real :  largest PF coil outer radius, m
    !+ad_args  pfm : : input real : largest PF coil mass, tonne
    !+ad_args  tfro : input real : outer radius of TF coil, m
    !+ad_args  tfri : input real : inner radius of TF coil, m
    !+ad_args  tfh : input real : full height of TF coil, m
    !+ad_args  tfm : input real : mass of one TF coil, tonne
    !+ad_args  tfno : input real : number of tf coils
    !+ad_args  shro : input real : outer radius of attached shield, m
    !+ad_args  shri : input real : inner radius of attached shield, m
    !+ad_args  shh : input real : height of attached shield, m
    !+ad_args  shm : input real : total mass of attached shield, kg
    !+ad_args  crr : input real : outer radius of common cryostat, m
    !+ad_args  helpow : input real : total cryogenic load, W
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  cryv : output real : volume of cryogenic building, m3
    !+ad_args  vrci : output real : inner volume of reactor building, m3
    !+ad_args  rbv : output real : outer volume of reactor building, m3
    !+ad_args  rmbv : output real : volume of reactor maintenance building, m3
    !+ad_args  wsv : output real : volume of warm shop, m3
    !+ad_args  elev : output real : volume of electrical buildings, m3
    !+ad_desc  This routine determines the size of the plant buildings.
    !+ad_desc  The reactor building and maintenance building are sized
    !+ad_desc  based on the tokamak dimensions. The cryogenic building volume is
    !+ad_desc  scaled based on the total cryogenic load. The other building
    !+ad_desc  sizes are input from other modules or by the user.
    !+ad_desc  This routine was modified to include fudge factors (fac1,2,...)
    !+ad_desc  to fit the ITER design, September 1990 (J. Galambos).
    !+ad_desc  This routine was included in PROCESS in January 1992 by 
    !+ad_desc  P. C. Shipe.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  30/10/12 PJK Added buildings_variables
    !+ad_hist  09/04/13 PJK Removed extra tfh term from hrbi
    !+ad_hist  09/04/13 PJK Converted pfm, tfm to kg in wt calculation;
    !+ad_hisc               removed extraneous 5.1m from rmbh calculation;
    !+ad_hisc               building volume multipliers now input parameters
    !+ad_hist  11/04/13 PJK Comment change
    !+ad_hist  18/06/13 PJK Added back extra tfh term to hrbi
    !+ad_hist  11/09/13 PJK Removed obsolete argument idhe3
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile
    real(kind(1.0D0)), intent(inout) :: pfr
    real(kind(1.0D0)), intent(in) :: pfm,tfro,tfri,tfh,tfm,tfno,shro, &
         shri,shh,shm,crr,helpow

    real(kind(1.0D0)), intent(out) :: cryv,vrci,rbv,rmbv,wsv,elev

    !  Local variables

    real(kind(1.0D0)) :: ang, bmr, coill, crcl, cran, dcl,dcw, drbi, &
         hcl, hcw, hrbi, hy, rbh, rbl, rbw, rmbh, rmbl, rmbw, rwl, rww, &
         sectl, tch, tcl, tcw, wgts, wsa, wt

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Reactor building

    !  Determine basic machine radius

    bmr = max(crr,pfr,tfro)

    !  Determine largest transported piece

    sectl = shro - shri
    coill = tfro - tfri
    sectl = max(coill, sectl)

    !  Calculate half width of building
    !  rxcl : clearance around reactor, m
    !  trcl : transportation clearance between components, m
    !  row  : clearance to building wall for crane operation, m

    wrbi = bmr + rxcl + sectl + trcl + row

    !  Calculate length to allow PF or cryostat laydown

    pfr = max(crr,pfr)
    hy = bmr + rxcl + sectl + trcl + pfr
    ang = (wrbi-trcl-pfr)/hy
    if (abs(ang) > 1.0D0) ang = abs(ang)/ang
    drbi = trcl + pfr + hy*sin(acos(ang)) + wrbi

    !  Crane height based on maximum lift
    !  wgt : reactor building crane capacity (kg)
    !        Calculated if 0 is input
    !  shmf : fraction of shield mass per TF coil to be moved in
    !         the maximum shield lift

    if (wgt > 1.0D0) then
       wt = wgt
    else
       wt = shmf*shm/tfno
       wt = max(wt, 1.0D3*pfm, 1.0D3*tfm)
    end if
    crcl = 9.41D-6*wt + 5.1D0

    !  Building height
    !  clh1 : (minimum) clearance from TF coil to cryostat top, m
    !         Slight inconsistency, as elsewhere this clearance may have been
    !         extended to ensure the PF coils lie within the cryostat.
    !  clh2 : clearance beneath TF coil to foundation, including basement, m
    !  stcl : clearance above crane to roof, m
    !  Additional tfh allows TF coil to be lifted right out

    hrbi = clh2 + 2.0D0*tfh + clh1 + trcl + crcl + stcl

    !  Internal volume

    vrci = rbvfac * 2.0D0*wrbi*drbi*hrbi

    !  External dimensions of reactor building
    !  rbwt : reactor building wall thickness, m
    !  rbrt : reactor building roof thickness, m
    !  fndt : foundation thickness, m

    rbw = 2.0D0*wrbi + 2.0D0*rbwt
    rbl = drbi + 2.0D0*rbwt
    rbh = hrbi + rbrt + fndt
    rbv = rbvfac * rbw*rbl*rbh

    !  Maintenance building
    !  The reactor maintenance building includes the hot cells, the
    !  decontamination chamber, the transfer corridors, and the waste
    !  treatment building.  The dimensions of these areas are scaled
    !  from a reference design based on the shield sector size.

    !  Transport corridor size
    !  hcwt : hot cell wall thickness, m

    tcw = shro-shri + 4.0D0*trcl
    tcl = 5.0D0*tcw + 2.0D0*hcwt

    !  Decontamination cell size

    dcw = 2.0D0*tcw + 1.0D0
    dcl = 2.0D0*tcw + 1.0D0

    !  Hot cell size
    !  hccl : clearance around components in hot cell, m

    hcw = shro-shri + 3.0D0*hccl + 2.0D0
    hcl = 3.0D0*(shro-shri) + 4.0D0*hccl + tcw

    !  Radioactive waste treatment

    rww = dcw
    rwl = hcl - dcl - hcwt

    !  Maintenance building dimensions

    rmbw = hcw + dcw + 3.0D0*hcwt
    rmbl = hcl + 2.0D0*hcwt

    !  Height
    !  wgt2 : hot cell crane capacity (kg)
    !         Calculated if 0 is input

    if (wgt2 >  1.0D0) then
       wgts = wgt2
    else
       wgts = shmf*shm/tfno
    end if
    cran = 9.41D-6*wgts + 5.1D0
    rmbh = 10.0D0 + shh + trcl + cran + stcl + fndt
    tch = shh + stcl + fndt

    !  Volume

    rmbv = mbvfac * rmbw*rmbl*rmbh + tcw*tcl*tch

    !  Warm shop and hot cell gallery

    wsa = (rmbw+7.0D0)*20.0D0 + rmbl*7.0D0
    wsv = wsvfac * wsa*rmbh

    !  Cryogenic building volume

    cryv = 55.0D0 * sqrt(helpow)

    !  Other building volumes
    !  pibv : power injection building volume, m3
    !  esbldgm3 is forced to be zero if no energy storage is required (lpulse=0)

    elev = tfcbv + pfbldgm3 + esbldgm3 + pibv

    !  Calculate effective floor area for ac power module

    efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0
    admvol = admv
    shovol = shov
    convol = conv

    !  Total volume of nuclear buildings

    volnucb = ( vrci + rmbv + wsv + triv + cryv )

    !  Output section

    if ((iprint == 0).or.(sect16 == 0)) return

    call oheadr(outfile,'Plant Buildings System')
    call ovarre(outfile,'Internal volume of reactor building (m3)', &
         '(vrci)',vrci)
    call ovarre(outfile,'Dist from centre of torus to bldg wall (m)', &
         '(wrbi)',wrbi)
    call ovarre(outfile,'Effective floor area (m2)','(efloor)',efloor)
    call ovarre(outfile,'Reactor building volume (m3)','(rbv)',rbv)
    call ovarre(outfile,'Reactor maintenance building volume (m3)', &
         '(rmbv)',rmbv)
    call ovarre(outfile,'Warmshop volume (m3)','(wsv)',wsv)
    call ovarre(outfile,'Tritium building volume (m3)','(triv)',triv)
    call ovarre(outfile,'Electrical building volume (m3)','(elev)',elev)
    call ovarre(outfile,'Control building volume (m3)','(conv)',conv)
    call ovarre(outfile,'Cryogenics building volume (m3)','(cryv)',cryv)
    call ovarre(outfile,'Administration building volume (m3)','(admv)',admv)
    call ovarre(outfile,'Shops volume (m3)','(shov)',shov)
    call ovarre(outfile,'Total volume of nuclear buildings (m3)', &
         '(volnucb)',volnucb)

  end subroutine bldgs

end module buildings_module
