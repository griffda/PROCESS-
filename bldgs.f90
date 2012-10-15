!  $Id::                                                                $
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
  !+ad_call  physics_variables
  !+ad_call  bldgvol.h90
  !+ad_call  build.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  pfcoil.h90
  !+ad_call  pwrcom.h90
  !+ad_call  rfp.h90
  !+ad_call  struccom.h90
  !+ad_call  tfcoil.h90
  !+ad_call  times.h90
  !+ad_call  bldgs
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use physics_variables

  implicit none

  include 'pfcoil.h90'
  include 'tfcoil.h90'
  include 'build.h90'
  include 'fwblsh.h90'
  include 'pwrcom.h90'
  include 'times.h90'
  include 'htpwr.h90'
  include 'bldgvol.h90'
  include 'struccom.h90'
  include 'rfp.h90'

  !  Arguments

  integer, intent(in) :: iprint, outfile

  !  Local variables

  real(kind(1.0D0)) :: crrad,tfh,tfmtn,tfri,tfro
  integer :: i

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Find maximum PF coil radius and mass
  !  (already calculated for RFPs)

  if (irfp == 0) then
     pfrmax = 0.0D0
     pfmmax = 0.0D0
     do i=1,ncirt
        pfrmax = max(pfrmax,rb(i) )
        pfmmax = max(pfmmax, (1.d-3*(wtc(i)+wts(i))) )
     end do
  end if

  tfmtn = 1.0D-3 * whttf
  tfro = rtot + 0.5D0*tfthko
  tfri = rtfcin - 0.5D0*tfcth
  tfh = (hmax + tfcth)*2.0D0
  crrad = pfrmax + 0.5D0

  !  Reactor vault wall and roof thicknesses are hardwired

  call bldgs(idhe3,pfrmax,pfmmax,tfro,tfri,tfh,tfmtn,tfno,rsldo, &
       rsldi,(hmax*2.0D0),whtshld,crrad,tfcbv,pfbldgm3, &
       esbldgm3,helpow,iprint,outfile, &
       cryvol,triv,volrci,efloor,rbvol,rmbvol,wsvol,elevol,wrbi, &
       admvol,shovol,convol,volnucb)

end subroutine bldgcall

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bldgs(idhe3,pfr,pfm,tfro,tfri,tfh,tfm,tfno,shro,shri, &
     shh,shm,crr,tfcbv,pfbldgm3,esbldgm3,helpow,iprint,outfile, &
     cryv,triv,vrci,efloor,rbv,rmbv,wsv,elev,wrbi,admvol,shovol, &
     convol,volnucb)

  !+ad_name  bldgs
  !+ad_summ  Determines the sizes of the plant buildings
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  idhe3 : input integer : switch for D-He3 operation (1=yes)
  !+ad_args  pfr : input/output real :  largest PF coil outer radius, m
  !+ad_args  pfm : : input real : largest PF coil mass, tonne
  !+ad_args  tfro : input real : outer radius of TF coil, m
  !+ad_args  tfri : input real : inner radius of TF coil, m
  !+ad_args  tfh : input real : height of TF coil, m
  !+ad_args  tfm : input real : mass of one TF coil, tonne
  !+ad_args  tfno : input real : number of tf coils
  !+ad_args  shro : input real : outer radius of attached shield, m
  !+ad_args  shri : input real : inner radius of attached shield, m
  !+ad_args  shh : input real : height of attached shield, m
  !+ad_args  shm : input real : total mass of attached shield, kg
  !+ad_args  crr : input real : outer radius of common cryostat, m
  !+ad_args  tfcbv : input real : volume of TF coil power supply building, m3
  !+ad_args  pfbldgm3 : input real : volume of PF coil power supply building, m3
  !+ad_args  esbldgm3 : input real : volume of energy storage building, m3
  !+ad_args  helpow : input real : total cryogenic load, W
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  cryv : output real : volume of cryogenic building, m3
  !+ad_args  triv : input real : volume of tritium building, m3
  !+ad_args  vrci : output real : inner volume of reactor building, m3
  !+ad_args  efloor : output real : effective floor area of buildings, m2
  !+ad_args  rbv : output real : outer volume of reactor building, m3
  !+ad_args  rmbv : output real : volume of reactor maintenance building, m3
  !+ad_args  wsv : output real : volume of warm shop, m3
  !+ad_args  elev : output real : volume of electrical buildings, m3
  !+ad_args  wrbi : output real : distance from centre of tokamak to reactor
  !+ad_argc                       building wall, m
  !+ad_args  admvol : output real : administration building volume, m3
  !+ad_args  shovol : output real : shops and warehouse volume, m3
  !+ad_args  convol : output real : control building volume, m3
  !+ad_args  volnucb : output real : volume of nuclear controlled buildings, m3
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
  !+ad_call  process_output
  !+ad_call  bldgcom.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  include 'bldgcom.h90'

  !  Arguments

  integer, intent(in) :: idhe3, iprint, outfile
  real(kind(1.0D0)), intent(inout) :: pfr
  real(kind(1.0D0)), intent(in) :: pfm,tfro,tfri,tfh,tfm,tfno,shro, &
       shri,shh,shm,crr,tfcbv,pfbldgm3,esbldgm3,helpow,triv

  real(kind(1.0D0)), intent(out) :: cryv,vrci,efloor,rbv,rmbv,wsv, &
       elev,wrbi,admvol,shovol,convol,volnucb

  !  Local variables

  real(kind(1.0D0)) :: ang, bmr, coill, crcl, cran, dcl,dcw, drbi, &
       fac1, fac2, fac3, hcl, hcw, hrbi, hy, rbh, rbl, rbw, rmbh, &
       rmbl, rmbw, rwl, rww, sectl, tch, tcl, tcw, wgts, wsa, wt

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
     wt = max(wt,pfm,tfm)
  end if
  crcl = 9.41D-6*wt + 5.1D0

  !  Building height
  !  clh1 : clearance from TF coil to cryostat top, m
  !  clh2 : clearance beneath TF coil to foundation, including basement, m
  !  stcl : clearance above crane to roof, m

  hrbi = clh2 + tfh + clh1 + trcl + tfh + crcl + stcl

  !  Internal volume

  fac1 = 1.6D0
  vrci = fac1 * 2.0D0*wrbi*drbi*hrbi

  !  External dimensions of reactor building
  !  rbwt : reactor building wall thickness, m
  !  rbrt : reactor building roof thickness, m
  !  fndt : foundation thickness, m

  rbw = 2.0D0*wrbi + 2.0D0*rbwt
  rbl = drbi + 2.0D0*rbwt
  rbh = hrbi + rbrt + fndt
  rbv = fac1 * rbw*rbl*rbh

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
  rmbh = 10.0D0 + shh + trcl + cran + 5.1D0 + stcl + fndt
  tch = shh + stcl + fndt

  !  Volume

  fac2 = 2.8D0
  rmbv = fac2 * rmbw*rmbl*rmbh + tcw*tcl*tch

  !  Warm shop and hot cell gallery

  wsa = (rmbw+7.0D0)*20.0D0 + rmbl*7.0D0
  fac3 = 1.9D0
  wsv = fac3 * wsa*rmbh

  !  Cryogenic building volume

  cryv = 55.0D0 * sqrt(helpow)

  !  Other building volumes
  !  pibv : power injection building volume, m3

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
