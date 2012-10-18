!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine fwbs(outfile,iprint)

  !+ad_name  fwbs
  !+ad_summ  First wall, blanket and shield module
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  C A Gardner, UKAEA Fusion
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : Fortran output unit identifier
  !+ad_args  iprint : input integer : Switch to write output to file (1=yes)
  !+ad_desc  This subroutine calculates the nuclear heating in the blanket /
  !+ad_desc  shield, and estimates the volume and masses of the first wall,
  !+ad_desc  blanket and shield.
  !+ad_desc  <P>The arrays <CODE>coef(i,j)</CODE> and <CODE>decay(i,j)</CODE>
  !+ad_desc  are used for exponential decay approximations of the
  !+ad_desc  (superconducting) TF coil nuclear parameters.
  !+ad_desc  <UL><P><LI><CODE>j = 1</CODE> : stainless steel shield
  !+ad_desc      <P><LI><CODE>j = 2</CODE> : tungsten shield.</UL>
  !+ad_desc  Note: Costing and mass calculations elsewhere assume
  !+ad_desc  stainless steel only.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  build.h90
  !+ad_call  cost.h90
  !+ad_call  htpwr.h90
  !+ad_call  tfcoil.h90
  !+ad_call  blanket
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarre
  !+ad_hist  14/11/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added fwbs_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use divertor_variables
  use fwbs_variables
  use physics_variables
  use process_output

  implicit none

  include 'build.h90'
  include 'tfcoil.h90'
  include 'cost.h90'
  include 'htpwr.h90'

  !  Arguments

  integer, intent(in) :: outfile, iprint

  !  Local variables

  real(kind(1.0D0)), dimension(5) :: fact
  real(kind(1.0D0)), dimension(5,2) :: coef
  real(kind(1.0D0)), dimension(7,2) :: decay

  integer :: ishmat
  real(kind(1.0D0)) :: coilhtmx,decaybl,dpacop,dshieq,dshoeq,elong, &
       flumax,fpsdt,fpydt,frachit,hb1,hblnkt,hecan,ht1,htheci, &
       pheci,pheco,pneut1,pneut2,ptfi,ptfiwp,ptfo,ptfowp,r1,r2, &
       r3,r4,raddose,rdewex,volshldi,volshldo,wpthk

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  fact(1) = 8.0D0
  fact(2) = 8.0D0
  fact(3) = 6.0D0
  fact(4) = 4.0D0
  fact(5) = 4.0D0

  coef(1,1) = 10.3D0
  coef(2,1) = 11.6D0
  coef(3,1) = 7.08D5
  coef(4,1) = 2.19D18
  coef(5,1) = 3.33D-7
  coef(1,2) = 8.32D0
  coef(2,2) = 10.6D0
  coef(3,2) = 7.16D5
  coef(4,2) = 2.39D18
  coef(5,2) = 3.84D-7

  decay(1,1) = 10.05D0
  decay(2,1) = 17.61D0
  decay(3,1) = 13.82D0
  decay(4,1) = 13.24D0
  decay(5,1) = 14.31D0
  decay(6,1) = 13.26D0
  decay(7,1) = 13.25D0
  decay(1,2) = 10.02D0
  decay(2,2) = 3.33D0
  decay(3,2) = 15.45D0
  decay(4,2) = 14.47D0
  decay(5,2) = 15.87D0
  decay(6,2) = 15.25D0
  decay(7,2) = 17.25D0

  !  Neutron power from plasma

  pneut1 = pneut*vol

  !  Neutron power lost through 'holes'

  pnucloss = pneut1 * fhole

  !  TART centrepost nuclear heating. Estimate fraction hitting from a
  !  point source at the plasma centre, and assume average path length
  !  of 2*tfcth, and e-fold decay length of 0.08m (copper water mixture).

  if (itart == 1) then
     frachit = hmax / sqrt(hmax**2 + (rmajor-tfcth)**2 ) * &
          atan(tfcth/(rmajor-tfcth) )/pi
     pnuccp = pneut1 * frachit * (1.0D0 - exp(-2.0D0*tfcth/0.08D0))
  else
     pnuccp = 0.0D0
  end if

  !  Energy-multiplied neutron power

  pneut2 = (pneut1 - pnucloss - pnuccp) * emult

  !  Nuclear heating in the blanket

  if (lblnkt == 1) then
     if (smstr == 1) then  !  solid blanket
        decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
     else  !  liquid blanket
        decaybl = 0.075D0 / (1.0D0 - vfblkt - fbllipb - fblli)
     end if
  else  !  original blanket model - solid blanket
     decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
  end if

  pnucblkt = pneut2 * (1.0D0 - exp(-blnkoth/decaybl) )

  !  Nuclear heating in the shield

  pnucshld = pneut2 - pnucblkt

  !  Full power DT operation years for replacement of TF Coil
  !  (or Plant Life)

  fpydt = cfactr * tlife
  fpsdt = fpydt * 3.154D7

  !  Superconducting TF coil shielding calculations

  if (itfsup == 1) then

     dshieq = shldith + fwith + blnkith
     dshoeq = shldoth + fwoth + blnkoth

     !  Assume case thickness on plasma side = 1/2 average thickness

     hecan = 0.5D0*thkcas
     wpthk = tfcth - 1.5D0 * thkcas

     !  Nuclear heating rate in inboard TF coil (MW/m**3)
     !  Set shield material to stainless steel

     ishmat = 1
     coilhtmx = fact(1) * wallmw * coef(1,ishmat) * &
          exp(-decay(6,ishmat) * (dshieq + hecan))

     !  Total nuclear heating (MW)

     ptfiwp = coilhtmx * tfsai * &
          (1.0D0-exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)
     ptfowp = fact(1) * wallmw * coef(1,ishmat) * &
          exp(-decay(6,ishmat) * (dshoeq + hecan)) * tfsao * &
          (1.0D0 - exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)

     !  Nuclear heating in He can (MW)

     htheci = fact(2) * wallmw * coef(2,ishmat) * &
          exp(-decay(7,ishmat) * dshieq)
     pheci = htheci * tfsai * (1.0D0-exp(-decay(2,ishmat)*hecan))/ &
          decay(2,ishmat)
     pheco = fact(2) * wallmw * coef(2,ishmat) * &
          exp(-decay(7,ishmat) * dshoeq) * tfsao * &
          (1.0D0-exp(-decay(2,ishmat)*hecan))/decay(2,ishmat)
     ptfi = ptfiwp + pheci
     ptfo = ptfowp + pheco
     ptfnuc = ptfi + ptfo

     !  Insulator dose (rad)

     raddose = coef(3,ishmat) * fpsdt * fact(3) * wallmw * &
          exp(-decay(3,ishmat) * (dshieq+hecan))

     !  Maximum neutron fluence in superconductor (n/m**2)

     flumax = fpsdt * fact(4) * wallmw * coef(4,ishmat) * &
          exp(-decay(4,ishmat) * (dshieq+hecan))

     !  Atomic displacement in copper stabilizer

     dpacop = fpsdt * fact(5) * wallmw * coef(5,ishmat) * &
          exp(-decay(5,ishmat) * (dshieq + hecan) )

  else  !  Resistive TF coils
     dshieq = 0.0D0
     dshoeq = 0.0D0
     hecan = 0.0D0
     wpthk = 0.0D0
     coilhtmx = 0.0D0
     ptfiwp = 0.0D0
     ptfowp = 0.0D0
     htheci = 0.0D0
     pheci = 0.0D0
     pheco = 0.0D0
     ptfi = 0.0D0
     ptfo = 0.0D0
     ptfnuc = 0.0D0
     raddose = 0.0D0
     flumax = 0.0D0
     dpacop = 0.0D0
  end if

  !  Divertor mass

  divsur = fdiva * 2.0D0 * pi * rmajor * rminor
  if (idivrt == 2) divsur = divsur * 2.0D0
  divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

  !  Start adding components of the coolant mass

  coolmass = divsur * divclfr * divplt

  !  Blanket and shield volumes and masses

  if (itart == 1) then

     elong = hmax/rtot
     r1 = rsldo - shldoth
     r2 = rsldi + shldith
     volblkto = fvolbo * 1.333D0 * pi * elong * &
          ( r1**3  - (r1-blnkoth)**3)
     volshldo = fvolso * 1.333D0 * pi * elong * (rsldo**3 - r1**3)

     !  Approximate TART inboard shield and blanket volumes by
     !  hollow cylinders the same height as the plasma

     volshldi = fvolsi * 2.0D0 * rminor*kappa * pi * (r2**2 - rsldi**2)
     volblkti = fvolbi * 2.0D0 * rminor*kappa * pi * &
          ( (r2+blnkith)**2 - r2**2 )

     volblkt = volblkto + volblkti
     volshld = volshldo + volshldi

  else

     r1 = rsldo - rmajor + rminor
     r2 = r1 - shldoth
     r3 = r2 - blnkoth
     r4 = rsldi + shldith
     ht1 = rminor*kappa + vgap2 + vgap

     if (idivrt == 2) then
        hb1 = ht1
     else
        hb1 = ht1-vgap
     end if

     hblnkt = 0.5D0*(ht1+hb1)

     volblkto = 1.333D0 * fvolbo * pi**2 * (r2**3 - r3**3)
     volblkti = fvolbi * hblnkt * 2.0D0*pi*((r4+blnkith)**2 - r4**2)
     volshldo = 1.333D0 * fvolso * pi**2 * (r1**3 - r2**3)
     volshldi = fvolsi * hblnkt * 2.0D0*pi*((r4**2 - rsldi**2) + &
          ((r4+shldith)**2 - r4**2) /2.0D0)
     volshld = volshldi + volshldo
     volblkt = volblkti + volblkto

  end if

  !  Blanket - stainless steel, Vanadium, Li2O, and Be options
  !  (assume 65% packing fraction for Be)

  whtblss = volblkt * denstl * fblss
  whtblbe = volblkt * 1900.0D0  * fblbe 
  whtblvd = volblkt * 5870.0D0  * fblvd
  wtblli2o = volblkt * 2010.0D0  * fblli2o
  whtblkt = whtblss + whtblvd + wtblli2o + whtblbe

  whtshld = volshld * denstl * (1.0D0 - vfshld)

  !  New blanket model (supersedes above calculations)

  if (lblnkt == 1) then
     call blanket(1,outfile,iprint)

     !  Improved approximation for inboard/outboard
     !  blanket volumes: assume cylinders of equal heights

     r1 = rsldi + shldith + 0.5D0*blnkith
     r2 = rsldo - shldoth - 0.5D0*blnkoth
     volblkti = volblkt * (r1*blnkith)/((r1*blnkith)+(r2*blnkoth))
     volblkto = volblkt * (r2*blnkoth)/((r1*blnkith)+(r2*blnkoth))

  end if

  !  Penetration shield (set = internal shield)

  wpenshld = whtshld
  coolmass = coolmass + volblkt*vfblkt + volshld*vfshld

  !  First wall mass
  !  (first wall area is calculated elsewhere)

  fwmass = fwarea * (fwith+fwoth)/2.0D0 * denstl * (1.0D0-fwclfr)

  !  Cryostat mass

  cryomass = fvolcry * 4.0D0 * (2.0D0*(rtot-rsldi) + 2.0D0*hmax) * &
       2.0D0 * pi * rmajor * ddwi * denstl

  !  Surface areas adjacent to plasma

  coolmass = coolmass + fwarea * (fwith+fwoth)/2.0D0 * fwclfr

  !  Mass of coolant = volume * density at typical coolant
  !  temperatures and pressures

  if (costr == 1) then  !  gaseous helium coolant
     coolmass = coolmass*1.517D0
  else  !  pressurised water coolant
     coolmass = coolmass*806.719D0
  end if

  !  Dewar volumes and mass

  rdewex = rtot + 0.5D0*tfthko + 2.0D0
  vdewex = ( (2.0D0*pi*rdewex) * (2.0D0*hmax + tfcth + 5.0D0) + &
       (2.0D0*pi*rdewex**2) ) * ddwex

  !  Factor of 2 to account for outside part of TF coil
  !  fvoldw accounts for ports, support, etc. additions

  vdewin = (2.0D0*(2.0D0*hmax) + 2.0D0 * (rsldo-rsldi)) * &
       2.0D0 * pi * rmajor * ddwi * 2.0D0 * fvoldw
  dewmkg = (vdewin + vdewex) * denstl

  if ((iprint == 0).or.(sect12 == 0)) return

  !  Output section

  call oheadr(outfile,'Shield / Blanket')
  call ovarre(outfile,'Average neutron wall load (MW)','(wallmw)', wallmw)
  call ovarre(outfile,'DT full power TF coil operation (yrs)', &
       '(fpydt)',fpydt)
  call ovarre(outfile,'Inner shield thickness (m)','(shldith)',shldith)
  call ovarre(outfile,'Outer shield thickness (m)','(shldoth)',shldoth)
  call ovarre(outfile,'Inner blanket thickness (m)','(blnkith)', blnkith)
  call ovarre(outfile,'Outer blanket thickness (m)','(blnkoth)', blnkoth)
  call ovarre(outfile,'Inner side TF coil case thickness (m)', &
       '(hecan)',hecan)

  if (itart == 1) then
     call osubhd(outfile,'(Copper centrepost used)')
     call ovarre(outfile,'Centrepost heating (MW)','(pnuccp)',pnuccp)
  else
     call osubhd(outfile,'TF coil nuclear parameters :')
     call ovarre(outfile,'Peak magnet heating (MW/m3)','(coilhtmx)', &
          coilhtmx)
     call ovarre(outfile,'Inner TF coil winding pack heating (MW)', &
          '(ptfiwp)',ptfiwp)
     call ovarre(outfile,'Outer TF coil winding pack heating (MW)', &
          '(ptfowp)',ptfowp)
     call ovarre(outfile,'Peak He can heating (MW/m3)','(htheci)', &
          htheci)
     call ovarre(outfile,'Inner He can heating (MW)','(pheci)',pheci)
     call ovarre(outfile,'Outer He can heating (MW)','(pheco)',pheco)
     call ovarre(outfile,'Insulator dose (rad)','(raddose)',raddose)
     call ovarre(outfile,'Maximum neutron fluence (n/m2)','(flumax)', &
          flumax)
     call ovarre(outfile,'Copper stabiliser displacements/atom', &
          '(dpacop)',dpacop)
  end if

  call osubhd(outfile,'Nuclear heating :')
  call ovarre(outfile,'Blanket heating (MW)','(pnucblkt)',pnucblkt)
  call ovarre(outfile,'Shield heating (MW)','(pnucshld)',pnucshld)

  call osubhd(outfile,'Blanket / shield volumes and weights :')

  if (lblnkt == 1) then
     if (smstr == 1) then
        write(outfile,600) volblkti, volblkto, volblkt,  &
             whtblkt, vfblkt, fblbe, whtblbe, fblli2o, wtblli2o,  &
             fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
             volshld, whtshld, vfshld, wpenshld
     else
        write(outfile,601) volblkti, volblkto, volblkt,  &
             whtblkt, vfblkt, fbllipb, wtbllipb, fblli, whtblli,  &
             fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
             volshld, whtshld, vfshld, wpenshld
     end if
  else
     write(outfile,600) volblkti, volblkto, volblkt, whtblkt, vfblkt, &
          fblbe, whtblbe, fblli2o, wtblli2o, fblss, whtblss, fblvd, &
          whtblvd, volshldi, volshldo, volshld, whtshld, vfshld, &
          wpenshld
  end if

600 format( &
       t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
       t32,'-----------',t45,'------------',t62,'-----------'/ &
       '    Inner blanket' ,t32,1pe10.3,/ &
       '    Outer blanket' ,t32,1pe10.3,/ &
       '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
       '       Void fraction' ,t45,1pe10.3,/ &
       '       Blanket Be   ',t45,1pe10.3,t62,1pe10.3/ &
       '       Blanket Li2O ',t45,1pe10.3,t62,1pe10.3/ &
       '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
       '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
       '    Inner shield'  ,t32,1pe10.3,/ &
       '    Outer shield'  ,t32,1pe10.3,/ &
       '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
       '       Void fraction' ,t45,1pe10.3,/ &
       '    Penetration shield'        ,t62,1pe10.3)

601 format( &
       t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
       t32,'-----------',t45,'------------',t62,'-----------'/ &
       '    Inner blanket' ,t32,1pe10.3,/ &
       '    Outer blanket' ,t32,1pe10.3,/ &
       '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
       '       Void fraction' ,t45,1pe10.3,/ &
       '       Blanket LiPb ',t45,1pe10.3,t62,1pe10.3/ &
       '       Blanket Li   ',t45,1pe10.3,t62,1pe10.3/ &
       '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
       '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
       '    Inner shield'  ,t32,1pe10.3,/ &
       '    Outer shield'  ,t32,1pe10.3,/ &
       '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
       '       Void fraction' ,t45,1pe10.3,/ &
       '    Penetration shield'        ,t62,1pe10.3)

  call osubhd(outfile,'Other volumes, masses and areas :')
  call ovarre(outfile,'First wall area (m2)','(fwarea)',fwarea)
  call ovarre(outfile,'First wall mass (kg)','(fwmass)',fwmass)
  call ovarre(outfile,'External dewar volume (m3)','(vdewex)',vdewex)
  call ovarre(outfile,'External dewar mass (kg)','(dewmkg)',dewmkg)
  call ovarre(outfile,'Internal dewar volume (m3)','(vdewin)',vdewin)
  call ovarre(outfile,'Cryostat mass (kg)','(cryomass)',cryomass)
  call ovarre(outfile,'Divertor area (m2)','(divsur)',divsur)
  call ovarre(outfile,'Divertor mass (kg)','(divmas)',divmas)

end subroutine fwbs
