!  $Id:: buildings.f90 209 2013-11-27 16:14:28Z pknight                 $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module buildings_module

  !! Module containing plant buildings routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of the fusion power plant buildings.
  
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Import modules !
  ! !!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use fwbs_variables
  use heat_transport_variables
  use pf_power_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use stellarator_variables
  use structure_variables
  use tfcoil_variables
  use times_variables

  implicit none

  ! Module subroutine and variable declrations !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  private
  public :: bldgcall

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bldgcall(outfile,iprint)

    !! Calls the buildings module
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calls the buildings calculations.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: iprint, outfile

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: tfh,tfmtn,tfri,tfro

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! mass per TF coil (tonnes)
    tfmtn = 1.0D-3 * whttf/n_tf

    ! TF coil inner and outer radial position (m)
    tfro = r_tf_outboard_mid + 0.5D0*tfthko
    tfri = r_tf_inboard_mid - 0.5D0*tfcth

    ! TF coil vertical height (m)
    ! Rem : SK not valid for single null
    tfh = (hmax + tfcth)*2.0D0

    ! Reactor vault wall and roof thicknesses are hardwired
    call bldgs(pfrmax,pfmmax,tfro,tfri,tfh,tfmtn,n_tf,rsldo, &
         rsldi,2.0D0*(hmax-ddwi-vgap2),whtshld,rdewex,helpow,iprint, &
         outfile,cryvol,volrci,rbvol,rmbvol,wsvol,elevol)

  end subroutine bldgcall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bldgs(pfr,pfm,tfro,tfri,tfh,tfm,n_tf,shro,shri, &
       shh,shm,crr,helpow,iprint,outfile,cryv,vrci,rbv,rmbv,wsv,elev)

    !! Determines the sizes of the plant buildings
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: P C Shipe, ORNL
    !! pfr : input/output real :  largest PF coil outer radius, m
    !! pfm : : input real : largest PF coil mass, tonne
    !! tfro : input real : outer radius of TF coil, m
    !! tfri : input real : inner radius of TF coil, m
    !! tfh : input real : full height of TF coil, m
    !! tfm : input real : mass of one TF coil, tonne
    !! tfno : input real : number of tf coils
    !! shro : input real : outer radius of attached shield, m
    !! shri : input real : inner radius of attached shield, m
    !! shh : input real : height of attached shield, m
    !! shm : input real : total mass of attached shield, kg
    !! crr : input real : outer radius of common cryostat, m
    !! helpow : input real : total cryogenic load, W
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! outfile : input integer : output file unit
    !! cryv : output real : volume of cryogenic building, m3
    !! vrci : output real : inner volume of reactor building, m3
    !! rbv : output real : outer volume of reactor building, m3
    !! rmbv : output real : volume of reactor maintenance building, m3
    !! wsv : output real : volume of warm shop, m3
    !! elev : output real : volume of electrical buildings, m3
    !! This routine determines the size of the plant buildings.
    !! The reactor building and maintenance building are sized
    !! based on the tokamak dimensions. The cryogenic building volume is
    !! scaled based on the total cryogenic load. The other building
    !! sizes are input from other modules or by the user.
    !! This routine was modified to include fudge factors (fac1,2,...)
    !! to fit the ITER design, September 1990 (J. Galambos).
    !! This routine was included in PROCESS in January 1992 by
    !! P. C. Shipe.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    integer, intent(in) :: iprint, outfile
    real(kind(1.0D0)), intent(inout) :: pfr
    real(kind(1.0D0)), intent(in) :: pfm,tfro,tfri,tfh,tfm,n_tf,shro, &
         shri,shh,shm,crr,helpow

    real(kind(1.0D0)), intent(out) :: cryv,vrci,rbv,rmbv,wsv,elev

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: ang, bmr, coill, crcl, cran, dcl,dcw, drbi, &
         hcl, hcw, hrbi, hy, layl, rbh, rbl, rbw, rmbh, rmbl, rmbw, rwl, rww, &
         sectl, tch, tcl, tcw, wgts, wsa, wt

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Reactor building

    ! Determine basic machine radius (m)
    ! crr  :  cryostat radius (m)
    ! pfr  :  radius of largest PF coil (m)
    ! tfro :  outer radius of TF coil (m)
    bmr = max(crr,pfr,tfro)

    ! Determine largest transported piece
    sectl = shro - shri  ! Shield thicknes (m)
    coill = tfro - tfri  ! TF coil thickness (m)
    sectl = max(coill, sectl)

    ! Calculate half width of building (m)
    ! rxcl : clearance around reactor, m
    ! trcl : transportation clearance between components, m
    ! row  : clearance to building wall for crane operation, m
    wrbi = bmr + rxcl + sectl + trcl + row

    ! Calculate length to allow PF or cryostat laydown (m)

    ! Laydown length (m)
    layl = max(crr,pfr)

    ! Diagnoal length (m)
    hy = bmr + rxcl + sectl + trcl + layl

    ! Angle between diagnoal length and floor (m)
    ang = (wrbi-trcl-layl)/hy

    ! Cap angle at 1
    if (abs(ang) > 1.0D0) then
      ang = abs(ang)/ang
    end if

    ! Length to allow laydown (m)
    drbi = trcl + layl + hy*sin(acos(ang)) + wrbi

    ! Crane height based on maximum lift (m)
    ! wgt : reactor building crane capacity (kg)
    !       Calculated if 0 is input
    ! shmf : fraction of shield mass per TF coil to be moved in
    !        the maximum shield lift
    if (wgt > 1.0D0) then
       wt = wgt
    else
       wt = shmf*shm/n_tf
       wt = max(wt, 1.0D3*pfm, 1.0D3*tfm)
    end if

    ! Crane height (m)
    crcl = 9.41D-6*wt + 5.1D0

    ! Building height (m)
    ! clh1 : clearance from TF coil to cryostat top, m
    ! clh2 : clearance beneath TF coil to foundation, including basement, m
    ! stcl : clearance above crane to roof, m
    ! Additional tfh allows TF coil to be lifted right out
    hrbi = clh2 + 2.0D0*tfh + clh1 + trcl + crcl + stcl

    ! Internal volume (m3)
    vrci = rbvfac * 2.0D0*wrbi*drbi*hrbi

    ! External dimensions of reactor building (m)
    ! rbwt : reactor building wall thickness, m
    ! rbrt : reactor building roof thickness, m
    ! fndt : foundation thickness, m
    rbw = 2.0D0*wrbi + 2.0D0*rbwt
    rbl = drbi + 2.0D0*rbwt
    rbh = hrbi + rbrt + fndt
    rbv = rbvfac * rbw*rbl*rbh

    ! Maintenance building
    ! The reactor maintenance building includes the hot cells, the
    ! decontamination chamber, the transfer corridors, and the waste
    ! treatment building.  The dimensions of these areas are scaled
    ! from a reference design based on the shield sector size.

    ! Transport corridor size
    ! hcwt : hot cell wall thickness, m
    tcw = shro-shri + 4.0D0*trcl
    tcl = 5.0D0*tcw + 2.0D0*hcwt

    ! Decontamination cell size
    dcw = 2.0D0*tcw + 1.0D0
    dcl = 2.0D0*tcw + 1.0D0

    ! Hot cell size
    ! hccl : clearance around components in hot cell, m
    hcw = shro-shri + 3.0D0*hccl + 2.0D0
    hcl = 3.0D0*(shro-shri) + 4.0D0*hccl + tcw

    ! Radioactive waste treatment
    rww = dcw
    rwl = hcl - dcl - hcwt

    ! Maintenance building dimensions
    rmbw = hcw + dcw + 3.0D0*hcwt
    rmbl = hcl + 2.0D0*hcwt

    ! Height
    ! wgt2 : hot cell crane capacity (kg)
    !        Calculated if 0 is input
    if (wgt2 >  1.0D0) then
       wgts = wgt2
    else
       wgts = shmf*shm/n_tf
    end if
    cran = 9.41D-6*wgts + 5.1D0
    rmbh = 10.0D0 + shh + trcl + cran + stcl + fndt
    tch = shh + stcl + fndt

    ! Volume
    rmbv = mbvfac * rmbw*rmbl*rmbh + tcw*tcl*tch

    ! Warm shop and hot cell gallery
    wsa = (rmbw+7.0D0)*20.0D0 + rmbl*7.0D0
    wsv = wsvfac * wsa*rmbh

    ! Cryogenic building volume
    cryv = 55.0D0 * sqrt(helpow)

    ! Other building volumes
    ! pibv : power injection building volume, m3
    ! esbldgm3 is forced to be zero if no energy storage is required (lpulse=0)
    elev = tfcbv + pfbldgm3 + esbldgm3 + pibv

    ! Calculate effective floor area for ac power module
    efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0
    admvol = admv
    shovol = shov
    convol = conv

    ! Total volume of nuclear buildings
    volnucb = ( vrci + rmbv + wsv + triv + cryv )

    ! Output !
    ! !!!!!!!!!
    
    if (iprint == 0) return
    call oheadr(outfile,'Plant Buildings System')
    call ovarre(outfile,'Internal volume of reactor building (m3)', '(vrci)', vrci)
    call ovarre(outfile,'Dist from centre of torus to bldg wall (m)', '(wrbi)', wrbi)
    call ovarre(outfile,'Effective floor area (m2)','(efloor)',efloor)
    call ovarre(outfile,'Reactor building volume (m3)','(rbv)',rbv)
    call ovarre(outfile,'Reactor maintenance building volume (m3)', '(rmbv)', rmbv)
    call ovarre(outfile,'Warmshop volume (m3)','(wsv)',wsv)
    call ovarre(outfile,'Tritium building volume (m3)','(triv)',triv)
    call ovarre(outfile,'Electrical building volume (m3)','(elev)',elev)
    call ovarre(outfile,'Control building volume (m3)','(conv)',conv)
    call ovarre(outfile,'Cryogenics building volume (m3)','(cryv)',cryv)
    call ovarre(outfile,'Administration building volume (m3)','(admv)',admv)
    call ovarre(outfile,'Shops volume (m3)','(shov)',shov)
    call ovarre(outfile,'Total volume of nuclear buildings (m3)', '(volnucb)', volnucb)

  end subroutine bldgs

end module buildings_module
