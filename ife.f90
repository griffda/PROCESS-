!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifeini

  !+ad_name  ifeini
  !+ad_summ  Routine to initialise the variables used by the Inertial Fusion
  !+ad_summ  Energy model
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine initialises the variables used by the Inertial Fusion
  !+ad_desc  Energy model.
  !+ad_prob  None
  !+ad_call  ife.h90
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  10/09/97 PJK De-escalated driver costs from 1991-1990 dollars
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.91
  !+ad_docs  F/MI/PJK/LOGBOOK13, p.2
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'ife.h90'

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Default builds and material volumes are those
  !  for the SOMBRERO device

  bldr   = 1.0D0
  bldzl  = 4.0D0
  bldzu  = 4.0D0
  cdriv0 = 154.3D0
  cdriv1 = 163.2D0
  cdriv2 = 244.9D0
  chdzl  = 9.0D0
  chdzu  = 9.0D0
  chrad  = 6.5D0
  chvol  = 0.0D0
  dcdrv0 = 111.4D0
  dcdrv1 = 78.0D0
  dcdrv2 = 59.9D0
  drveff = 0.28D0
  edrive = 5.0D6
  etadrv = 0.0D0
  etave(1)  = 0.082D0
  etave(2)  = 0.079D0
  etave(3)  = 0.076D0
  etave(4)  = 0.073D0
  etave(5)  = 0.069D0
  etave(6)  = 0.066D0
  etave(7)  = 0.062D0
  etave(8)  = 0.059D0
  etave(9)  = 0.055D0
  etave(10) = 0.051D0
  fbreed = 0.51D0
  fburn  = 0.3333D0
  frrmax = 1.0D0
  fwdr   = 0.01D0
  fwdzl  = 0.01D0
  fwdzu  = 0.01D0
  gain   = 0.0D0
  gainve(1)  = 60.0D0
  gainve(2)  = 95.0D0
  gainve(3)  = 115.0D0
  gainve(4)  = 125.0D0
  gainve(5)  = 133.0D0
  gainve(6)  = 141.0D0
  gainve(7)  = 152.0D0
  gainve(8)  = 160.0D0
  gainve(9)  = 165.0D0
  gainve(10) = 170.0D0
  ifedrv = 2
  ifetyp = 0
  mcdriv = 1.0D0
  mflibe = 0.0D0
  pdrive = 23.0D6
  pifecr = 10.0D0
  ptargf = 2.0D0
  r1     = 0.0D0
  r2     = 0.0D0
  r3     = 0.0D0
  r4     = 0.0D0
  r5     = 0.0D0
  r6     = 0.0D0
  r7     = 0.0D0
  reprat = 0.0D0
  rrmax  = 20.0D0
  shdr   = 1.7D0
  shdzl  = 5.0D0
  shdzu  = 5.0D0
  sombdr = 2.7D0
  somtdr = 2.7D0
  tdspmw = 0.0D0
  tfacmw = 0.0D0
  tgain  = 85.0D0
  uccarb = 50.0D0
  ucconc = 0.1D0
  ucflib = 84.0D0
  uctarg = 0.3D0
  v1dr   = 0.0D0
  v1dzl  = 0.0D0
  v1dzu  = 0.0D0
  v2dr   = 2.0D0
  v2dzl  = 7.0D0
  v2dzu  = 7.0D0
  v3dr   = 43.3D0
  v3dzl  = 30.0D0
  v3dzu  = 20.0D0
  zl1    = 0.0D0
  zl2    = 0.0D0
  zl3    = 0.0D0
  zl4    = 0.0D0
  zl5    = 0.0D0
  zl6    = 0.0D0
  zl7    = 0.0D0
  zu1    = 0.0D0
  zu2    = 0.0D0
  zu3    = 0.0D0
  zu4    = 0.0D0
  zu5    = 0.0D0
  zu6    = 0.0D0
  zu7    = 0.0D0

  chmatf(:) = 0.0D0
  chmatf(0) = 1.0D0

  chmatm(:) = 0.0D0
  chmatv(:) = 0.0D0

  blmatf(:,:) = 0.0D0
  blmatf(:,0) = 0.05D0
  blmatf(:,2) = 0.45D0
  blmatf(:,4) = 0.20D0
  blmatf(:,6) = 0.30D0

  blmatm(:,:) = 0.0D0
  blmatv(:,:) = 0.0D0

  fwmatf(:,:) = 0.0D0
  fwmatf(:,0) = 0.05D0
  fwmatf(:,2) = 0.95D0

  fwmatm(:,:) = 0.0D0
  fwmatv(:,:) = 0.0D0

  shmatf(:,:) = 0.0D0
  shmatf(:,0) = 0.05D0
  shmatf(:,1) = 0.19D0
  shmatf(:,5) = 0.665D0
  shmatf(:,6) = 0.095D0

  shmatm(:,:) = 0.0D0
  shmatv(:,:) = 0.0D0

  v1matf(:,:) = 0.0D0
  v1matf(:,0) = 1.0D0

  v1matm(:,:) = 0.0D0
  v1matv(:,:) = 0.0D0

  v2matf(:,:) = 0.0D0
  v2matf(:,0) = 1.0D0

  v2matm(:,:) = 0.0D0
  v2matv(:,:) = 0.0D0

  v3matf(:,:) = 0.0D0
  v3matf(:,0) = 1.0D0

  v3matm(:,:) = 0.0D0
  v3matv(:,:) = 0.0D0


end subroutine ifeini

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifecll

  !+ad_name  ifecll
  !+ad_summ  Routine to call the physics and engineering modules
  !+ad_summ  relevant to inertial fusion energy power plants
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calls the physics and engineering modules
  !+ad_desc  relevant to inertial fusion energy power plants.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  avail
  !+ad_call  costs
  !+ad_call  ifeacp
  !+ad_call  ifebdg
  !+ad_call  ifebld
  !+ad_call  ifefbs
  !+ad_call  ifephy
  !+ad_call  ifepw1
  !+ad_call  ifepw2
  !+ad_call  ifestr
  !+ad_call  ifetgt
  !+ad_call  ifevac
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  19/05/99 PJK Added call to routine AVAIL
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.66
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Device build

  call ifebld(nout,0)

  !  IFE physics

  call ifephy(nout,0)

  !  Device structure

  call ifestr(nout,0)

  !  Target data

  call ifetgt(nout,0)

  !  First wall, blanket and shield

  call ifefbs(nout,0)

  !  Primary thermal power

  call ifepw1

  !  Vacuum system

  call ifevac(nout,0)

  !  Buildings

  call ifebdg(nout,0)

  !  AC power requirements

  call ifeacp(nout,0)

  !  Secondary thermal power

  call ifepw2(nout,0)

  !  Plant availability

  call avail(nout,0)

  !  Costs

  call costs(nout,0)

end subroutine ifecll

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifeout(nout)

  !+ad_name  ifeout
  !+ad_summ  Routine to output the physics and engineering information
  !+ad_summ  relevant to inertial fusion energy power plants
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_desc  This routine outputs the physics and engineering information
  !+ad_desc  relevant to inertial fusion energy power plants.
  !+ad_prob  None
  !+ad_call  avail
  !+ad_call  costs
  !+ad_call  ifeacp
  !+ad_call  ifebdg
  !+ad_call  ifebld
  !+ad_call  ifefbs
  !+ad_call  ifephy
  !+ad_call  ifepw1
  !+ad_call  ifepw2
  !+ad_call  ifestr
  !+ad_call  ifetgt
  !+ad_call  ifevac
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  19/05/99 PJK Added call to routine AVAIL
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.66
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Costs

  call costs(nout,1)

  !  Plant availability

  call avail(nout,1)

  !  IFE physics

  call ifephy(nout,1)

  !  Device build

  call ifebld(nout,1)

  !  First wall, blanket and shield

  call ifefbs(nout,1)

  !  Device structure

  call ifestr(nout,1)

  !  Target data

  call ifetgt(nout,1)

  !  Primary thermal power

  call ifepw1

  !  Vacuum system

  call ifevac(nout,1)

  !  Buildings

  call ifebdg(nout,1)

  !  AC power requirements

  call ifeacp(nout,1)

  !  Secondary thermal power

  call ifepw2(nout,1)

end subroutine ifeout

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifephy(nout,iprint)

  !+ad_name  ifephy
  !+ad_summ  Routine to calculate the physics parameters of an Inertial Fusion
  !+ad_summ  Energy power plant
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the physics parameters of an Inertial Fusion
  !+ad_desc  Energy power plant.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  osections.h90
  !+ad_call  phydat.h90
  !+ad_call  ife.h90
  !+ad_call  build.h90
  !+ad_call  driver
  !+ad_call  lasdrv
  !+ad_call  iondrv
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, pp.68,85
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'osections.h90'
  include 'phydat.h90'
  include 'ife.h90'
  include 'build.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  real(kind(1.0D0)) :: aaion,bmax,dpp,dtheta,emitt,etai,lf,phi,qion, &
       sang,sigma,sigma0,tauf,theta,vi
  integer :: nbeams

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (iprint /= 1) then

     !  Repetition rate

     reprat = pdrive / edrive

     !  Driver calculations

     select case (ifedrv)

     case (-1)
        !  Target gain and driver efficiency dependencies on
        !  driver energy are input

        call driver(edrive,gainve,etave,gain,etadrv)

     case (0)  !  Target gain and driver efficiency are input

        gain = tgain
        etadrv = drveff

     case (1)  !  Laser driver based on SOMBRERO design

        call lasdrv(edrive,gain,etadrv)

     case (2)  !  Heavy-ion beam driver based on OSIRIS design

        aaion = 131.0D0
        bmax = 10.0D0
        dpp = 1.25D-3
        dtheta = 1.3D-4
        emitt = 1.0D-5
        etai = 0.8D0
        lf = 5.0D0
        nbeams = 12
        qion = 1.0D0
        sigma = 8.0D0
        sigma0 = 80.0D0
        tauf = 1.0D-7
        theta = 30.0D-3
        vi = 3.0D6

        call iondrv(aaion,bmax,dpp,dtheta,edrive,emitt,etai,lf, &
             nbeams,qion,reprat,sigma,sigma0,tauf,theta,vi,gain,etadrv)

     case default
        write(nout,*) 'Error in routine IFEPHY:'
        write(nout,*) 'Illegal value for IFEDRV, = ',ifedrv
        write(nout,*) 'PROCESS stopping.'
        stop

     end select

     !  Fusion power (MW)

     powfmw = 1.0D-6 * pdrive * gain

     !  Wall load (assume total fusion power applies)

     if (ifetyp == 1) then

        !  OSIRIS-type build: First wall subtends a solid angle of 2 pi * SANG

        phi = 0.5D0*pi + atan(zl1/r1)
        sang = 1.0D0 - cos(phi)
        wallmw = powfmw * 0.5D0*sang / fwarea

     else
        wallmw = powfmw / fwarea
     end if

  end if

  if ((iprint == 0).or.(sect03 == 0)) return

  !  Output section

  call oheadr(nout,'Physics / Driver Issues')

  select case (ifedrv)

  case (-1, 0)
     call ocmmnt(nout,'Driver type : generic')
  case (1)
     call ocmmnt(nout,'Driver type : laser')
  case (2)
     call ocmmnt(nout,'Driver type : heavy ion beam')
  end select
  call oblnkl(nout)

  call ovarre(nout,'Driver energy (J)','(edrive)',edrive)
  call ovarre(nout,'Driver efficiency','(etadrv)',etadrv)
  call ovarre(nout,'Driver power reaching target (W)','(pdrive)', &
       pdrive)
  call ovarre(nout,'Driver repetition rate (Hz)','(reprat)',reprat)
  call ovarre(nout,'Target gain','(gain)',gain)
  call ovarre(nout,'Fusion power (MW)','(powfmw)',powfmw)
  call ovarre(nout,'Neutron wall load (MW/m2)','(wallmw)',wallmw)

end subroutine ifephy

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine iondrv(aaion,bmax,dpp,dtheta,edrive,emitt,etai,lf, &
     nbeams,qion,reprat,sigma,sigma0,tauf,theta,vi,gain,etadrv)

  !+ad_name  iondrv
  !+ad_summ  Routine to calculate parameters of a heavy ion driver
  !+ad_summ  suitable for inertial fusion energy
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  cbeam
  !+ad_cont  betgam
  !+ad_args  aaion  : input real : Ion mass (amu)
  !+ad_args  bmax   : input real : Maximum field at the superconductor (T)
  !+ad_args  dpp    : input real : Beam momentum spread
  !+ad_args  dtheta : input real : Pointing error (rad)
  !+ad_args  edrive : input real : Driver energy (J)
  !+ad_args  emitt  : input real : Normalised beam emittance (metre-rad)
  !+ad_args  etai   : input real : Axial quadrupole packing fraction at injection
  !+ad_args  lf     : input real : Distance from final focussing quad to target (m)
  !+ad_args  nbeams : input integer : Number of beams
  !+ad_args  qion   : input real : Ion charge state
  !+ad_args  reprat : input real : Repetition rate (Hz)
  !+ad_args  sigma  : input real : Depressed tune (incl. space charge effects) (deg)
  !+ad_args  sigma0 : input real : Phase advance per lattice period (tune) (deg)
  !+ad_args  tauf   : input real : Post-acceleration pulse duration (s)
  !+ad_args  theta  : input real : Final focussing half angle (rad)
  !+ad_args  vi     : input real : Injection voltage (V)
  !+ad_args  gain   : output real : Target gain
  !+ad_args  etadrv : output real : Driver efficiency
  !+ad_desc  This routine calculates the parameters of a heavy ion driver
  !+ad_desc  suitable for inertial fusion energy.
  !+ad_desc  <P>Currently, the complicated model taken from the reference is not
  !+ad_desc  complete, so it is recommended that the simple model is used
  !+ad_desc  (set <CODE>ISIMP=1</CODE>)
  !+ad_prob  See above
  !+ad_call  cbeam
  !+ad_call  betgam
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  Heavy-ion Driver Design and Scaling, R. Bieri et al.,
  !+ad_docc      Fusion Technology, vol.21 (1992) 1583
  !+ad_docs  Meier and Bieri, Fusion Technology, vol.21 (1992) 1547
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: aaion,bmax,dpp,dtheta,edrive,emitt,etai,lf,qion, &
       reprat,sigma,sigma0,tauf,theta,vi
  integer, intent(in) :: nbeams
  real(kind(1.0D0)), intent(out) :: gain,etadrv

  !  Local variables

  real(kind(1.0D0)), parameter :: degrad = 0.01745329251D0
  real(kind(1.0D0)), parameter :: echrge = 1.60217733D-19
  real(kind(1.0D0)), parameter :: mprot  = 1.6726231D-27
  real(kind(1.0D0)), parameter :: c2     = 8.98755178737D16
  real(kind(1.0D0)), parameter :: pi     = 3.141592653589793D0
  integer, parameter :: isimp = 1  !  Switch for simple model (1=yes)

  real(kind(1.0D0)) :: ci,de,dgap,dlcore,drcore,e,eomc2,fins,floss, &
       ibf,ibfo,ibi,ibpc,lfocus,lpf,lpfo,lpi,lppc,lq,phif,phifo, &
       phii,phipc,rion,rs,rs1,rs2,rs3,rs4,sig,sig0,taufo,taui, &
       taupc,tbrad,vf,vfo,vpc,vs,vscore,vshe,vsle,vspc,xhe,xle,xpc
  real(kind(1.0D0)), dimension(10) :: eve,gve
  integer :: i,ie,j,nche,ncle,ncpc,nqarrs,nqle,nqpche,nquads

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (isimp /= 1) then

     !  Complex IONDRV model (incomplete)...
     !  ====================

     !  Electron charge / (proton mass * c**2)

     eomc2 = echrge / (mprot*c2)

     !  Degrees to radians

     sig = sigma * degrad
     sig0 = sigma0 * degrad

     !  Beam current coefficient

     ci = cbeam(etai,bmax,aaion,qion,sig,sig0,emitt,vi,eomc2)

     !  Final voltage (V)

     vf = (edrive/(dble(nbeams)*tauf*ci))**(0.666666D0)

     !  Final beam current (A)

     ibf = ci*sqrt(vf)

     !  Ion range (g/cm^2)

     rion = (3.04D-5 * aaion + 349.0D0*aaion**(-2.21D0)) * &
          (qion*vf/1.0D9)**(1.72D0-0.00275D0*aaion)

     !  Convert to kg/m^2

     rion = rion*10.0D0

     !  Total spot size (rms of four terms):
     !  (1) Emittance term

     rs1 = emitt / (betgam(aaion,qion,vf)*theta)

     !  (2) Dispersion (momentum spread) term

     rs2 = 8.0D0*dpp*lf*theta

     !  (3) Alignment and jitter term

     rs3 = dtheta*lf

     !  (4) Space charge term (inaccurate at present)

     rs4 = 1.0D-3

     rs = sqrt(rs1**2 + rs2**2 + rs3**2 + rs4**2)

     !  Target gain (inaccurate at present)

     gain = 62.6D0 - 142.3D0*rs**1.25D0 * rion**0.633D0 + &
          (32.8D0+82.1D0*rs)**0.783D0 * rion**0.647D0 * &
          log(1.0D-6*edrive)

     !  Source/Injector characteristics:
     !  Transport beam radius (m)

     tbrad = ( (emitt**2 * sig0 * c2)/(sig**2 * bmax/1.5D0 * etai * &
          sqrt(vi)) )**(0.333333D0) * sqrt(2.0D0 * aaion * mprot / &
          (qion * echrge))

     !  Extractor voltage (V)

     vs = 4.0D0/3.0D0 * vi

     !  Source extraction gap width (m)

     dgap = 0.8D0

     !  Injected current per beam (A)

     ibi = 4.0D0 * pi * tbrad**2 * 5.46D-8 * &
          sqrt( qion*vs**3/(aaion*dgap**4) )

     !  Beam loss fraction between injector and target

     floss = 0.08D0

     !  Pulse duration at injection (s)

     taui = 1.0D0/(1.0D0-floss) * tauf * ibf / ibi

     !  Pulse length at injection (m)

     lpi = taui * sqrt( 2.0D0 * qion * echrge * vi / (aaion * mprot) )

     !  Initial voltage gradient (V/m)

     phii = 0.3D0 * vi / lpi

     !  End of Low Energy Transport Stage:
     !  Insulated fraction of driver length

     fins = 0.85D0

     !  Maximum voltage gradient, limited by insulator flash-over (V/m)

     phifo = 1.0D6 * fins

     !  Voltage at which this limit is reached (V)
     !  The end of the low energy stage is set by the point at which
     !  the limit is reached.

     vfo = (phifo/phii)**(2.0D0/3.0D0) * vi

     !  Beam current (A)

     ibfo = ibi * (vfo/vi)

     !  Pulse duration (s)

     taufo = taui * (vi/vfo)

     !  Pulse length (m)

     lpfo = taui * vi / vfo * sqrt( 2.0D0 * qion * echrge * vfo / &
          (aaion * mprot) )

     !  Length of the low energy transport stage (m)
     !  (rearrangement of integral of PHI(V).dl)

     xle = 2.0D0*vi**1.5D0 / phii * (1.0D0/sqrt(vi) - 1.0D0/sqrt(vfo))

     !  End of Pulse Compression Stage:
     !  Pulse length = final pulse length (m)

     lpf = tauf * sqrt( 2.0D0 * qion * echrge * vf / (aaion * mprot) )
     lppc = lpf

     !  Length of the pulse compression region (m)

     xpc = (lpfo - lpf)/0.3D0

     !  Voltage gradient (V/m) - limit already reached

     phipc = phifo

     !  Voltage (V)

     vpc = vfo + (xpc*phipc)

     !  Beam current (A)

     ibpc = ibfo * (lpfo/lppc) * sqrt(vpc/vfo)

     !  Pulse duration (s)

     taupc = taufo * (lppc/lpfo) * sqrt(vfo/vpc)

     !  End of High Energy Transport Stage:
     !  Voltage gradient (V/m) - limit already reached

     phif = phifo

     !  Length of the high energy transport stage (m)

     xhe = (vf - vfo)/phifo

     write(*,*) '                 Injector:'
     write(*,*) '   vi = ',vi
     write(*,*) '  ibi = ',ibi
     write(*,*) ' taui = ',taui
     write(*,*) ' phii = ',phii
     write(*,*) '  lpi = ',lpi
     write(*,*) '                 End of LET stage:'
     write(*,*) '  vfo = ',vfo
     write(*,*) ' ibfo = ',ibfo
     write(*,*) 'taufo = ',taufo
     write(*,*) 'phifo = ',phifo
     write(*,*) ' lpfo = ',lpfo
     write(*,*) '  xle = ',xle
     write(*,*) '                 End of PC stage:'
     write(*,*) '  vpc = ',vpc
     write(*,*) ' ibpc = ',ibpc
     write(*,*) 'taupc = ',taupc
     write(*,*) 'phipc = ',phipc
     write(*,*) ' lppc = ',lppc
     write(*,*) '  xpc = ',xpc
     write(*,*) '                 End of HET stage:'
     write(*,*) '   vf = ',vf
     write(*,*) '  ibf = ',ibf
     write(*,*) ' tauf = ',tauf
     write(*,*) ' phif = ',phif
     write(*,*) '  lpf = ',lpf
     write(*,*) '  xhe = ',xhe

     !  Volt-second requirements and number of cores

     !  Volt-seconds per core (2.4T is field swing of the core
     !  material metglas)

     drcore = 0.8D0
     dlcore = 0.2D0
     vscore = 2.4D0 * drcore * dlcore

     !  Low energy stage

     vsle = taui * vi * log(vfo/vi)
     ncle = 1 + int(vsle/vscore)

     !  Pulse compression stage

     vspc = 2.0D0 * tauf * sqrt(vf) * (sqrt(vpc)-sqrt(vfo))
     ncpc = 1 + int(vspc/vscore)

     !  High energy stage

     drcore = 0.4D0
     dlcore = 0.1D0
     vscore = 2.4D0 * DRCORE * DLCORE

     vshe = 2.0D0 * tauf * vf * (1.0D0 - sqrt(vpc/vf))
     nche = 1 + int(vshe/vscore)

     write(*,*) '                 Number of cores: '
     write(*,*) ' ncle = ',ncle
     write(*,*) ' ncpc = ',ncpc
     write(*,*) ' nche = ',nche

     !  Quadrupole requirements

     !  Effective quadrupole length (m)

     lq = ( emitt * etai * sig0**2 * sqrt(c2*vi) / &
          (sig*(bmax/1.5D0)**2) )**(0.333333D0) * &
          sqrt(2.0D0*aaion*mprot/(qion*echrge))

     write(*,*) '   lq = ',lq

     !  Actual quadrupole length (m)

     write(*,*) '  alq = ',lq/etai

     !  Length of quadrupole focussing fields: low energy stage

     lfocus = etai * vi/phii * (1.0D0 - vi/vfo)
     nqle = 1 + int(lfocus/lq)

     !  Pulse compression + high energy stages

     lfocus = 2.0D0 * etai * sqrt(vi)/phifo * (sqrt(vf) - sqrt(vfo))

     nqpche = 1 + int(lfocus/lq)

     !  Number of quadrupole arrays

     nqarrs = nqle + nqpche

     !  Number of quadrupoles

     nquads = nqarrs * nbeams

     write(*,*) 'nqarrs = ',nqarrs
     write(*,*) 'nquads = ',nquads

     etadrv = 0.28D0

  else

     !  Simple model
     !  ============

     !  gve(k): target gain at edrive = k MegaJoules

     gve(1)  = 25.0D0
     gve(2)  = 44.0D0
     gve(3)  = 62.0D0
     gve(4)  = 76.0D0
     gve(5)  = 87.0D0
     gve(6)  = 97.0D0
     gve(7)  = 107.0D0
     gve(8)  = 115.0D0
     gve(9)  = 125.0D0
     gve(10) = 132.0D0

     !  eve(k): driver efficiency at edrive = k MegaJoules

     eve(1)  = 0.232D0
     eve(2)  = 0.256D0
     eve(3)  = 0.269D0
     eve(4)  = 0.276D0
     eve(5)  = 0.282D0
     eve(6)  = 0.286D0
     eve(7)  = 0.290D0
     eve(8)  = 0.292D0
     eve(9)  = 0.294D0
     eve(10) = 0.296D0

     e = 1.0D-6 * edrive
     ie = int(e)
     de = e - dble(IE)

     !  Assume linear interpolations and extrapolations
     !  Would be better to prevent extrapolation

     if (ie <= 1) then

        gain   = gve(2) - 1.0D-6*(edrive-2.0D6)*(gve(1)-gve(2))
        etadrv = eve(2) - 1.0D-6*(edrive-2.0d6)*(eve(1)-eve(2))

     else if (ie >= 9) then

        gain   = gve(9) + 1.0D-6*(edrive-9.0D6)*(gve(10)-gve(9))
        etadrv = eve(9) + 1.0D-6*(edrive-9.0D6)*(eve(10)-eve(9))

     else

        gain   = gve(ie) + de*(gve(ie+1)-gve(ie))
        etadrv = eve(ie) + de*(eve(ie+1)-eve(ie))

     end if

     !  Ensure sensible values

     gain = max(0.01D0,gain)
     etadrv = max(0.01D0,etadrv)

  end if

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function cbeam(etai,bmax,aaion,qion,sigma,sigma0,emitt,vi,eomc2)

    !+ad_name  cbeam
    !+ad_summ  Routine to evaluate the beam current coefficient, CI
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  etai   : input real : Axial quadrupole packing fraction at injection
    !+ad_args  bmax   : input real : Maximum field at the superconductor (T)
    !+ad_args  aaion  : input real : Ion mass (amu)
    !+ad_args  qion   : input real : Ion charge state
    !+ad_args  sigma  : input real : Depressed tune (incl. space charge effects) (rad)
    !+ad_args  sigma0 : input real : Phase advance per lattice period (tune) (rad)
    !+ad_args  emitt  : input real : Normalised beam emittance (metre-rad)
    !+ad_args  vi     : input real : Injection voltage (V)
    !+ad_args  eomc2  : input real : Electron charge / (proton mass * light speed**2)
    !+ad_desc  This routine calculates the beam current coefficient
    !+ad_desc  of a heavy ion driver suitable for inertial fusion energy.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/03/97 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  Heavy-ion Driver Design and Scaling, R. Bieri et al.,
    !+ad_docc      Fusion Technology, vol.21 (1992) 1583
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: cbeam

    !  Arguments

    real(kind(1.0D0)), intent(in) :: etai,bmax,aaion,qion,sigma,sigma0, &
         emitt,vi,eomc2

    !  Local variables

    real(kind(1.0D0)) :: bbe

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Maximum field at the beam edge

    bbe = bmax/1.5D0

    !  Perform calculation

    cbeam = 2.89D6*(1.0D0-(sigma/sigma0)**2) * &
         ( sigma0**4 * etai**2 * vi * aaion/qion * (emitt/sigma)**2 &
         * bbe**2 )**0.333333D0 &
         * (2.0D0 * qion * eomc2 / aaion)**(5.0D0/6.0D0)

  end function cbeam

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function betgam(aaion,qion,v)

    !+ad_name  betgam
    !+ad_summ  Routine to calculate the relativistic factor (beta*gamma) for the
    !+ad_summ  heavy ions in the beam driver
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  aaion  : input real : Ion mass (amu)
    !+ad_args  qion   : input real : Ion charge state
    !+ad_args  v      : input real : Acceleration voltage (V)
    !+ad_desc  This routine calculates the relativistic factor (beta*gamma) for the
    !+ad_desc  heavy ions in a beam driver suitable for inertial fusion energy.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/03/97 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  Heavy-ion Driver Design and Scaling, R. Bieri et al.,
    !+ad_docc      Fusion Technology, vol.21 (1992) 1583
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: betgam

    !  Arguments

    real(kind(1.0D0)), intent(in) :: aaion,qion,v

    !  Local variables

    real(kind(1.0D0)), parameter :: echrge = 1.60217733D-19
    real(kind(1.0D0)), parameter :: mprot = 1.6726231D-27
    real(kind(1.0D0)), parameter :: c2 = 8.98755178737D16

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    betgam = sqrt( 2.0D0*qion*echrge*v / (aaion*mprot*c2) )

  end function betgam

end subroutine iondrv

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine lasdrv(edrive,gain,etadrv)

  !+ad_name  lasdrv
  !+ad_summ  Routine to calculate parameters of a laser driver
  !+ad_summ  suitable for inertial fusion energy
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  edrive : input real : Driver energy (J)
  !+ad_args  gain   : output real : Target gain
  !+ad_args  etadrv : output real : Driver efficiency
  !+ad_desc  This routine calculates the parameters of a laser driver
  !+ad_desc  suitable for inertial fusion energy.
  !+ad_desc  Gain and driver efficiency data are taken from Figures 1 and 2 of
  !+ad_desc  Meier and Rosenberg.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  Meier and Rosenberg, Fusion Technology vol.21 (1992) p.1552
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.86
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: edrive
  real(kind(1.0D0)), intent(out) :: etadrv,gain

  !  Local variables

  real(kind(1.0D0)) :: e,de
  real(kind(1.0D0)), dimension(10) :: gve,eve
  integer :: ie

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  GVE(K): target gain at EDRIVE = K MegaJoules

  gve(1)  = 63.0D0
  gve(2)  = 95.0D0
  gve(3)  = 112.0D0
  gve(4)  = 125.0D0
  gve(5)  = 136.0D0
  gve(6)  = 144.0D0
  gve(7)  = 151.0D0
  gve(8)  = 157.0D0
  gve(9)  = 162.0D0
  gve(10) = 166.0D0

  !  EVE(K): driver efficiency at EDRIVE = K MegaJoules

  eve(1)  = 0.082D0
  eve(2)  = 0.079D0
  eve(3)  = 0.076D0
  eve(4)  = 0.072D0
  eve(5)  = 0.069D0
  eve(6)  = 0.064D0
  eve(7)  = 0.059D0
  eve(8)  = 0.054D0
  eve(9)  = 0.048D0
  eve(10) = 0.042D0

  e = 1.0D-6 * edrive
  ie = int(e)
  de = e - dble(ie)

  !  Assume linear interpolations and extrapolations
  !  Would be better to prevent extrapolation

  if (ie <= 1) then

     gain   = gve(2) - 1.0D-6*(edrive-2.0D6)*(gve(1)-gve(2))
     etadrv = eve(2) - 1.0D-6*(edrive-2.0D6)*(eve(1)-eve(2))

  else if (ie >= 9) then

     gain   = gve(9) + 1.0D-6*(edrive-9.0D6)*(gve(10)-gve(9))
     etadrv = eve(9) + 1.0D-6*(edrive-9.0D6)*(eve(10)-eve(9))

  else

     gain = gve(ie)   + de*(gve(ie+1)-gve(ie))
     etadrv = eve(ie) + de*(eve(ie+1)-eve(ie))

  end if

  !  Ensure sensible values

  gain = max(0.01D0,gain)
  etadrv = max(0.01D0,etadrv)

end subroutine lasdrv

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine driver(edrive,gainve,etave,gain,etadrv)

  !+ad_name  driver
  !+ad_summ  Routine to calculate parameters of a generic driver
  !+ad_summ  suitable for inertial fusion energy
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  edrive : input real : Driver energy (J)
  !+ad_args  gainve(10) : input real array : Gain vs energy data
  !+ad_args  etave(10) : input real array : Driver efficiency vs energy data
  !+ad_args  gain   : output real : Target gain
  !+ad_args  etadrv : output real : Driver efficiency
  !+ad_desc  This routine calculates the parameters of a generic driver
  !+ad_desc  suitable for inertial fusion energy.
  !+ad_desc  Gain and driver efficiency data are interpolated from input data.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.85
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: edrive
  real(kind(1.0D0)), dimension(10), intent(in) :: etave,gainve
  real(kind(1.0D0)), intent(out) :: etadrv,gain

  !  Local variables

  real(kind(1.0D0)) :: de,e
  integer :: ie

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  The arrays contain data points for EDRIVE = 1MJ, 2MJ, ... , 10MJ

  e = 1.0D-6 * edrive
  ie = int(e)
  de = e - dble(ie)

  !  Assume linear interpolations and extrapolations

  if (ie <= 1) then

     gain =  gainve(2) - 1.0D-6*(edrive-2.0D6)*(gainve(1)-gainve(2))
     etadrv = etave(2) - 1.0D-6*(edrive-2.0D6)*(etave(1)-etave(2))

  else if (ie >= 9) then

     gain =  gainve(9) + 1.0D-6*(edrive-9.0D6)*(gainve(10)-gainve(9))
     etadrv = etave(9) + 1.0D-6*(edrive-9.0D6)*(etave(10)-etave(9))

  else

     gain =  gainve(ie) + de*(gainve(ie+1)-gainve(ie))
     etadrv = etave(ie) + de*(etave(ie+1)-etave(ie))

  end if

  !  Ensure sensible values

  gain = max(0.01D0,gain)
  etadrv = max(0.01D0,etadrv)

end subroutine driver

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifebld(nout,iprint)

  !+ad_name  ifebld
  !+ad_summ  Routine to create the build of an inertial fusion energy device
  !+ad_summ  and to calculate the material volumes for the device core
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  genbld
  !+ad_cont  hylbld
  !+ad_cont  osibld
  !+ad_cont  sombld
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine constructs the build of an inertial fusion energy device
  !+ad_desc  and calculates the material volumes for the device core.
  !+ad_prob  None
  !+ad_call  osections.h90
  !+ad_call  ife.h90
  !+ad_call  genbld
  !+ad_call  hylbld
  !+ad_call  obuild
  !+ad_call  oheadr
  !+ad_call  osibld
  !+ad_call  sombld
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.52
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'osections.h90'
  include 'ife.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (iprint /= 1) then

     select case (ifetyp)
     case (1)  !  OSIRIS-type device
        call osibld
     case (2)  !  SOMBRERO-type device
        call sombld
     case (3)  !  HYLIFE-II-type device
        call hylbld
     case default  !  Generic device
        call genbld
     end select

  end if

  if ((iprint == 0).or.(sect06 == 0)) return

  !  Output section

  call oheadr(nout,'Radial Build')
  write(nout,20)
20 format(T43,'Thickness (m)',T60,'Radius (m)')

  call obuild(nout,'Device centreline',0.0D0,0.0D0)
  call obuild(nout,'Chamber',chrad,r1)
  call obuild(nout,'First Wall',fwdr,r2)
  call obuild(nout,'Void 1',v1dr,r3)
  call obuild(nout,'Blanket',bldr,r4)
  call obuild(nout,'Void 2',v2dr,r5)
  call obuild(nout,'Shield',shdr,r6)
  call obuild(nout,'Void 3',v3dr,r7)

  call oheadr(nout,'Vertical Build')
  write(nout,30)
30 format(T43,'Thickness (m)',T60,'Height (m)')

  call obuild(nout,'Base of device',0.0D0,-zl7)
  call obuild(nout,'Void 3',v3dzl,-zl6)
  call obuild(nout,'Shield',shdzl,-zl5)
  call obuild(nout,'Void 2',v2dzl,-zl4)
  call obuild(nout,'Blanket',bldzl,-zl3)
  call obuild(nout,'Void 1',v1dzl,-zl2)
  call obuild(nout,'First Wall',fwdzl,-zl1)
  call obuild(nout,'Chamber',chdzl,0.0D0)
  call obuild(nout,'Chamber',chdzu,zu1)
  call obuild(nout,'First Wall',fwdzu,zu2)
  call obuild(nout,'Void 1',v1dzu,zu3)
  call obuild(nout,'Blanket',bldzu,zu4)
  call obuild(nout,'Void 2',v2dzu,zu5)
  call obuild(nout,'Shield',shdzu,zu6)
  call obuild(nout,'Void 3',v3dzu,zu7)

  !  Print matrix of material volumes

  call oheadr(nout,'Material volumes')

  write(nout,*) '         Chamber  1st wall  Void 1  Blanket  ' &
       //' Void 2   Shield   Void 3'
  write(nout,'(A9,7(1pe9.2))') 'void     ', &
       chmatv(0), &
       (fwmatv(1,0)+fwmatv(2,0)+fwmatv(3,0)), &
       (v1matv(1,0)+v1matv(2,0)+v1matv(3,0)), &
       (blmatv(1,0)+blmatv(2,0)+blmatv(3,0)), &
       (v2matv(1,0)+v2matv(2,0)+v2matv(3,0)), &
       (shmatv(1,0)+shmatv(2,0)+shmatv(3,0)), &
       (v3matv(1,0)+v3matv(2,0)+v3matv(3,0))
  write(nout,'(A9,7(1pe9.2))') 'steel    ', &
       chmatv(1), &
       (fwmatv(1,1)+fwmatv(2,1)+fwmatv(3,1)), &
       (v1matv(1,1)+v1matv(2,1)+v1matv(3,1)), &
       (blmatv(1,1)+blmatv(2,1)+blmatv(3,1)), &
       (v2matv(1,1)+v2matv(2,1)+v2matv(3,1)), &
       (shmatv(1,1)+shmatv(2,1)+shmatv(3,1)), &
       (v3matv(1,1)+v3matv(2,1)+v3matv(3,1))

  write(nout,'(A9,7(1pe9.2))') 'carbon   ', &
       chmatv(2), &
       (fwmatv(1,2)+fwmatv(2,2)+fwmatv(3,2)), &
       (v1matv(1,2)+v1matv(2,2)+v1matv(3,2)), &
       (blmatv(1,2)+blmatv(2,2)+blmatv(3,2)), &
       (v2matv(1,2)+v2matv(2,2)+v2matv(3,2)), &
       (shmatv(1,2)+shmatv(2,2)+shmatv(3,2)), &
       (v3matv(1,2)+v3matv(2,2)+v3matv(3,2))

  write(nout,'(A9,7(1pe9.2))') 'FLiBe    ', &
       chmatv(3), &
       (fwmatv(1,3)+fwmatv(2,3)+fwmatv(3,3)), &
       (v1matv(1,3)+v1matv(2,3)+v1matv(3,3)), &
       (blmatv(1,3)+blmatv(2,3)+blmatv(3,3)), &
       (v2matv(1,3)+v2matv(2,3)+v2matv(3,3)), &
       (shmatv(1,3)+shmatv(2,3)+shmatv(3,3)), &
       (v3matv(1,3)+v3matv(2,3)+v3matv(3,3))

  write(nout,'(A9,7(1pe9.2))') 'Li2O     ', &
       chmatv(4), &
       (fwmatv(1,4)+fwmatv(2,4)+fwmatv(3,4)), &
       (v1matv(1,4)+v1matv(2,4)+v1matv(3,4)), &
       (blmatv(1,4)+blmatv(2,4)+blmatv(3,4)), &
       (v2matv(1,4)+v2matv(2,4)+v2matv(3,4)), &
       (shmatv(1,4)+shmatv(2,4)+shmatv(3,4)), &
       (v3matv(1,4)+v3matv(2,4)+v3matv(3,4))

  write(nout,'(A9,7(1pe9.2))') 'concrete ', &
       chmatv(5), &
       (fwmatv(1,5)+fwmatv(2,5)+fwmatv(3,5)), &
       (v1matv(1,5)+v1matv(2,5)+v1matv(3,5)), &
       (blmatv(1,5)+blmatv(2,5)+blmatv(3,5)), &
       (v2matv(1,5)+v2matv(2,5)+v2matv(3,5)), &
       (shmatv(1,5)+shmatv(2,5)+shmatv(3,5)), &
       (v3matv(1,5)+v3matv(2,5)+v3matv(3,5))

  write(nout,'(A9,7(1pe9.2))') 'helium   ', &
       chmatv(6), &
       (fwmatv(1,6)+fwmatv(2,6)+fwmatv(3,6)), &
       (v1matv(1,6)+v1matv(2,6)+v1matv(3,6)), &
       (blmatv(1,6)+blmatv(2,6)+blmatv(3,6)), &
       (v2matv(1,6)+v2matv(2,6)+v2matv(3,6)), &
       (shmatv(1,6)+shmatv(2,6)+shmatv(3,6)), &
       (v3matv(1,6)+v3matv(2,6)+v3matv(3,6))

  write(nout,'(A9,7(1pe9.2))') 'xenon    ', &
       chmatv(7), &
       (fwmatv(1,7)+fwmatv(2,7)+fwmatv(3,7)), &
       (v1matv(1,7)+v1matv(2,7)+v1matv(3,7)), &
       (blmatv(1,7)+blmatv(2,7)+blmatv(3,7)), &
       (v2matv(1,7)+v2matv(2,7)+v2matv(3,7)), &
       (shmatv(1,7)+shmatv(2,7)+shmatv(3,7)), &
       (v3matv(1,7)+v3matv(2,7)+v3matv(3,7))

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine genbld

    !+ad_name  genbld
    !+ad_summ  Routine to create the build of a generic inertial fusion energy
    !+ad_summ  device, and to calculate the material volumes for the device core
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine constructs the build of a generic inertial fusion energy
    !+ad_desc  device, assumed to be cylindrically-symmetric, and to calculate
    !+ad_desc  the material volumes for the device core.
    !+ad_prob  None
    !+ad_call  build.h90
    !+ad_call  ife.h90
    !+ad_hist  21/03/97 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK12, p.52
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    include 'build.h90'
    include 'ife.h90'

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: pi = 3.141592653589793D0
    integer :: i,j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Radial build

    r1 = chrad
    r2 = r1 + fwdr
    r3 = r2 + v1dr
    r4 = r3 + bldr
    r5 = r4 + v2dr
    r6 = r5 + shdr
    r7 = r6 + v3dr

    !  Vertical build (below midplane)

    zl1 = chdzl
    zl2 = zl1 + fwdzl
    zl3 = zl2 + v1dzl
    zl4 = zl3 + bldzl
    zl5 = zl4 + v2dzl
    zl6 = zl5 + shdzl
    zl7 = zl6 + v3dzl

    !  Vertical build (above midplane)

    zu1 = chdzu
    zu2 = zu1 + fwdzu
    zu3 = zu2 + v1dzu
    zu4 = zu3 + bldzu
    zu5 = zu4 + v2dzu
    zu6 = zu5 + shdzu
    zu7 = zu6 + v3dzu

    !  Component volumes
    !  The following notation applies below:
    !  J=1 : side part
    !  J=2 : top part
    !  J=3 : bottom part

    !  Chamber

    chvol = pi * r1*r1 * (zu1 + zl1)

    !  First wall

    fwvol(1) = pi * (r2*r2 - r1*r1) * (zu1 + zl1)
    fwvol(2) = pi * r2*r2 * (zu2 - zu1)
    fwvol(3) = pi * r2*r2 * (zl2 - zl1)

    !  First void

    v1vol(1) = pi * (r3*r3 - r2*r2) * (zu2 + zl2)
    v1vol(2) = pi * r3*r3 * (zu3 - zu2)
    v1vol(3) = pi * r3*r3 * (zl3 - zl2)

    !  Blanket

    blvol(1) = pi * (r4*r4 - r3*r3) * (zu3 + zl3)
    blvol(2) = pi * r4*r4 * (zu4 - zu3)
    blvol(3) = pi * r4*r4 * (zl4 - zl3)

    !  Second void

    v2vol(1) = pi * (r5*r5 - r4*r4) * (zu4 + zl4)
    v2vol(2) = pi * r5*r5 * (zu5 - zu4)
    v2vol(3) = pi * r5*r5 * (zl5 - zl4)

    !  Shield

    shvol(1) = pi * (r6*r6 - r5*r5) * (zu5 + zl5)
    shvol(2) = pi * r6*r6 * (zu6 - zu5)
    shvol(3) = pi * r6*r6 * (zl6 - zl5)

    !  Third void

    v3vol(1) = pi * (r7*r7 - r6*r6) * (zu6 + zl6)
    v3vol(2) = pi * r7*r7 * (zu7 - zu6)
    v3vol(3) = pi * r7*r7 * (zl7 - zl6)

    !  Material volumes

    do i = 0,maxmat
       chmatv(i) = max(0.0D0, chvol * chmatf(i))
       do j = 1,3
          fwmatv(j,i) = max(0.0D0, fwvol(j) * fwmatf(j,i))
          v1matv(j,i) = max(0.0D0, v1vol(j) * v1matf(j,i))
          blmatv(j,i) = max(0.0D0, blvol(j) * blmatf(j,i))
          v2matv(j,i) = max(0.0D0, v2vol(j) * v2matf(j,i))
          shmatv(j,i) = max(0.0D0, shvol(j) * shmatf(j,i))
          v3matv(j,i) = max(0.0D0, v3vol(j) * v3matf(j,i))
       end do
    end do

    !  First wall area

    fwarea = 2.0D0*pi*r1 * ((zu1 + zl1) + r1)

  end subroutine genbld

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine osibld

    !+ad_name  osibld
    !+ad_summ  Routine to create the build of an inertial fusion energy
    !+ad_summ  device, based on the design of the OSIRIS study,
    !+ad_summ  and to calculate the material volumes for the device core
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine constructs the build of an inertial fusion energy
    !+ad_desc  device, based on the design of the OSIRIS study, and to calculate
    !+ad_desc  the material volumes for the device core.
    !+ad_prob  None
    !+ad_call  build.h90
    !+ad_call  ife.h90
    !+ad_call  genbld
    !+ad_hist  21/03/97 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/MI/PJK/LOGBOOK12, p.56
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    include 'build.h90'
    include 'ife.h90'

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: pi = 3.141592653589793D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Careful choice of thicknesses, and assuming that the FLiBe
    !  inlet radius is small, allows the generic build calculation
    !  to be roughly applicable.

    call genbld

    !  First wall area: no true first wall at bottom of chamber

    fwarea = 2.0D0*pi*r1*(zu1 + zl1) + pi*r1*r1

  end subroutine osibld

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sombld

    !+ad_name  sombld
    !+ad_summ  Routine to create the build of an inertial fusion energy
    !+ad_summ  device, based on the design of the SOMBRERO study,
    !+ad_summ  and to calculate the material volumes for the device core
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine constructs the build of an inertial fusion energy
    !+ad_desc  device, based on the design of the SOMBRERO study, and to calculate
    !+ad_desc  the material volumes for the device core.
    !+ad_prob  None
    !+ad_call  build.h90
    !+ad_call  ife.h90
    !+ad_hist  21/03/97 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  Sviatoslavsky et al, Fusion Technology vol.21 (1992) 1470
    !+ad_docs  F/MI/PJK/LOGBOOK12, p.53
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    include 'build.h90'
    include 'ife.h90'

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: pi = 3.141592653589793D0
    real(kind(1.0D0)), parameter :: third  = 1.0D0/3.0D0
    real(kind(1.0D0)) :: chcylh,ddz,dvol
    integer :: i,j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Radial build

    r1 = chrad
    r2 = r1 + fwdr
    r3 = r2 + v1dr
    r4 = r3 + bldr
    r5 = r4 + v2dr
    r6 = r5 + shdr
    r7 = r6 + v3dr

    !  Vertical build (below midplane)

    zl1 = chdzl
    zl2 = zl1 + fwdzl
    zl3 = zl2 + v1dzl
    zl4 = zl3 + bldzl
    zl5 = zl4 + v2dzl
    zl6 = zl5 + shdzl
    zl7 = zl6 + v3dzl

    !  Vertical build (above midplane)

    zu1 = chdzu
    zu2 = zu1 + fwdzu
    zu3 = zu2 + v1dzu
    zu4 = zu3 + bldzu
    zu5 = zu4 + v2dzu
    zu6 = zu5 + shdzu
    zu7 = zu6 + v3dzu

    !  The SOMBRERO chamber is made up of a cylindrical first wall/
    !  blanket, with conical regions above and below. Outside this is
    !  a cylindrical shield.

    !  Component volumes
    !  The following notation applies below:
    !  J=1 : side part
    !  J=2 : top part
    !  J=3 : bottom part

    !  Chamber : CHCYLH is the height of the cylindrical part

    chcylh = chdzu + chdzl - 2.0D0*chrad

    chvol = pi * r1*r1 * (chcylh + 2.0D0*third * chrad)

    !  First wall

    fwvol(1) = pi * (r2*r2 - r1*r1) * chcylh
    fwvol(2) = third * pi * ( r2*r2*(chrad+fwdzu) - r1*r1*chrad )
    fwvol(3) = third * pi * ( r2*r2*(chrad+fwdzl) - r1*r1*chrad )

    !  First void

    v1vol(1) = pi * (r3*r3 - r2*r2) * chcylh
    v1vol(2) = third * pi * ( r3*r3*(chrad+fwdzu+v1dzu) -  &
         r2*r2*(chrad+fwdzu) )
    v1vol(3) = third * pi * ( r3*r3*(chrad+fwdzl+v1dzl) -  &
         r2*r2*(chrad+fwdzl) )

    !  Blanket:  SOMTDR and SOMBDR are the radii of the cylindrical
    !  sections at the top/bottom of the blanket
    !  DDZ = Height of top cylindrical section (by similar triangles)
    !  DVOL = Volume of top cylindrical section, less the internal cone

    blvol(1) = pi * (r4*r4 - r3*r3) * chcylh

    blvol(2) = third * pi * ( r4*r4*(chrad+fwdzu+v1dzu+bldzu) -  &
         r3*r3*(chrad+fwdzu+v1dzu) )
    ddz = (chrad+fwdzu+v1dzu+bldzu)/(chrad+fwdr+v1dr+bldr)*somtdr
    dvol = 2.0D0*third * pi * somtdr*somtdr * ddz

    blvol(2) = blvol(2) + dvol

    !  Ditto for bottom region...

    blvol(3) = third * pi * ( r4*r4*(chrad+fwdzl+v1dzl+bldzl) -  &
         r3*r3*(chrad+fwdzl+v1dzl) )
    ddz = (chrad+fwdzl+v1dzl+bldzl)/(chrad+fwdr+v1dr+bldr)*sombdr
    dvol = 2.0D0*third * pi * sombdr*sombdr * ddz

    blvol(3) = blvol(3) + dvol

    !  Second void

    v2vol(1) = pi * (r5*r5 - r4*r4) * chcylh
    v2vol(2) = pi * r5*r5 * (zu5 - chdzu + chrad) - ( &
         fwvol(2) + v1vol(2) + blvol(2) + (third*pi*r1*r1*chrad) )
    v2vol(3) = pi * r5*r5 * (zl5 - chdzl + chrad) - ( &
         fwvol(3) + v1vol(3) + blvol(3) + (third*pi*r1*r1*chrad) )

    !  Shield

    shvol(1) = pi * (r6*r6 - r5*r5) * (zu6 + zl6)
    shvol(2) = pi * r5*r5 * (zu6 - zu5)
    shvol(3) = pi * r5*r5 * (zl6 - zl5)

    !  Third void

    v3vol(1) = pi * (r7*r7 - r6*r6) * (zu7 + zl7)
    v3vol(2) = pi * r6*r6 * (zu7 - zu6)
    v3vol(3) = pi * r6*r6 * (zl7 - zl6)

    !  Material volumes

    do i = 0,maxmat
       chmatv(i) = max(0.0D0, chvol * chmatf(i))
       do j = 1,3
          fwmatv(j,i) = max(0.0D0, fwvol(j) * fwmatf(j,i))
          v1matv(j,i) = max(0.0D0, v1vol(j) * v1matf(j,i))
          blmatv(j,i) = max(0.0D0, blvol(j) * blmatf(j,i))
          v2matv(j,i) = max(0.0D0, v2vol(j) * v2matf(j,i))
          shmatv(j,i) = max(0.0D0, shvol(j) * shmatf(j,i))
          v3matv(j,i) = max(0.0D0, v3vol(j) * v3matf(j,i))
       end do
    end do

    !  First wall area

    fwarea = 2.0D0*pi*r1*( (zu1 + zl1) + r1*sqrt(2.0D0) )

  end subroutine sombld

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine hylbld

    !+ad_name  hylbld
    !+ad_summ  Routine to create the build of an inertial fusion energy
    !+ad_summ  device, based on the design of the HYLIFE-II study,
    !+ad_summ  and to calculate the material volumes for the device core
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine constructs the build of an inertial fusion energy
    !+ad_desc  device, based on the design of the HYLIFE-II study, and to calculate
    !+ad_desc  the material volumes for the device core.
    !+ad_prob  None
    !+ad_call  build.h90
    !+ad_call  ife.h90
    !+ad_hist  21/03/97 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  Moir, Fusion Technology vol.21 (1992) 1475
    !+ad_docs  F/MI/PJK/LOGBOOK12, p.57
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    include 'build.h90'
    include 'ife.h90'

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: pi = 3.141592653589793D0
    real(kind(1.0D0)), parameter :: third = 1.0D0/3.0D0
    real(kind(1.0D0)) :: chcylh,ddz,dvol
    integer :: i,j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Radial build

    r1 = chrad
    r2 = r1 + fwdr
    r3 = r2 + v1dr
    r4 = r3 + bldr
    r5 = r4 + v2dr
    r6 = r5 + shdr
    r7 = r6 + v3dr

    !  Vertical build (below midplane)

    zl1 = chdzl
    zl2 = zl1 + fwdzl
    zl3 = zl2 + v1dzl
    zl4 = zl3 + bldzl
    zl5 = zl4 + v2dzl
    zl6 = zl5 + shdzl
    zl7 = zl6 + v3dzl

    !  Vertical build (above midplane)

    zu1 = chdzu
    zu2 = zu1 + fwdzu
    zu3 = zu2 + v1dzu
    zu4 = zu3 + bldzu
    zu5 = zu4 + v2dzu
    zu6 = zu5 + shdzu
    zu7 = zu6 + v3dzu

    !  The HYLIFE-II chamber is assumed to be mostly cylindrical, but
    !  with a conical region below the midplane that causes the Flibe
    !  to flow downwards and outwards towards the outlet.

    !  Component volumes
    !  The following notation applies below:
    !  J=1 : side part
    !  J=2 : top part
    !  J=3 : bottom part

    chvol = pi * r1*r1 * ( (zu1 + zl5) - third*(zl5 - zl1) )

    !  First wall
    !  FLIRAD is the radius of the Flibe inlet

    fwvol(1) = pi * (r2*r2 - r1*r1) * (zu2 + zl5)
    fwvol(2) = pi * (r1*r1 - flirad*flirad) * (zu2 - zu1)
    fwvol(3) = third * pi * ( r2*r2*(zl5-zl1) - r1*r1*(zl5-zl2) )

    !  First void

    v1vol(1) = pi * (r3*r3 - r2*r2) * (zu2 + zl3)
    v1vol(2) = pi * (r4*r4 - flirad*flirad) * (zu3 - zu2)
    v1vol(3) = third * pi * r1*r1 * (zl3 - zl2)

    !  Blanket

    blvol(1) = pi * (r4*r4 - r3*r3) * (zu2 + zl3)
    blvol(2) = pi * (r4*r4 - flirad*flirad) * (zu4 - zu3)
    blvol(3) = pi * r4*r4 * (zl4 - zl3)

    !  Second void

    v2vol(1) = pi * (r5*r5 - r4*r4) * (zu4 + zl4)
    v2vol(2) = pi * (r5*r5 - flirad*flirad) * (zu5 - zu4)
    v2vol(3) = pi * r5*r5 * (zl5 - zl4)

    !  Shield

    shvol(1) = pi * (r6*r6 - r5*r5) * (zu5 + zl5)
    shvol(2) = pi * r6*r6 * (zu6 - zu5)
    shvol(3) = pi * r6*r6 * (zl6 - zl5)

    !  Third void

    v3vol(1) = pi * (r7*r7 - r6*r6) * (zu6 + zl6)
    v3vol(2) = pi * r7*r7 * (zu7 - zu6)
    v3vol(3) = pi * r7*r7 * (zl7 - zl6)

    !  Material volumes

    do i = 0,maxmat
       chmatv(i) = max(0.0D0, chvol * chmatf(i))
       do j = 1,3
          fwmatv(j,i) = max(0.0D0, fwvol(j) * fwmatf(j,i))
          v1matv(j,i) = max(0.0D0, v1vol(j) * v1matf(j,i))
          blmatv(j,i) = max(0.0D0, blvol(j) * blmatf(j,i))
          v2matv(j,i) = max(0.0D0, v2vol(j) * v2matf(j,i))
          shmatv(j,i) = max(0.0D0, shvol(j) * shmatf(j,i))
          v3matv(j,i) = max(0.0D0, v3vol(j) * v3matf(j,i))
       end do
    end do

    !  First wall area

    fwarea = 2.0D0 * pi * r1 * (zu1 + zl5)
    fwarea = fwarea + pi * (r1*r1 - flirad*flirad)
    fwarea = fwarea + pi * r1 * sqrt(r1*r1 + (zl3-zl1)**2)

  end subroutine hylbld

end subroutine ifebld

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifestr(nout,iprint)

  !+ad_name  ifestr
  !+ad_summ  Routine to calculate the support structural masses for the core of
  !+ad_summ  an Inertial Fusion Energy power plant
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the support structural masses for the core of
  !+ad_desc  an Inertial Fusion Energy power plant.
  !+ad_desc  <P>In fact, the output masses are all trivially zero, as they are
  !+ad_desc  magnetic fusion specific.
  !+ad_prob  None
  !+ad_call  struccom.h90
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.87
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'struccom.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Set all outputs to zero, as they are magnetic fusion specific

  aintmass = 0.0D0
  clgsmass = 0.0D0
  coldmass = 0.0D0
  fncmass  = 0.0D0
  gsmass   = 0.0D0

end subroutine ifestr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifetgt(nout,iprint)

  !+ad_name  ifetgt
  !+ad_summ  Routine to calculate the power requirements of the target
  !+ad_summ  delivery system and the target factory
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the power requirements of the target
  !+ad_desc  delivery system and the target factory, for an Inertial
  !+ad_desc  Fusion Energy power plant.
  !+ad_prob  None
  !+ad_call  ife.h90
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, pp.87-88
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'ife.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Target delivery system power (MWe) - effectively negligible

  tdspmw = 1.0D-2

  !  Target factory power (MWe)
  !  Assumed to scale with repetition rate (not quite linearly)

  tfacmw = ptargf * (reprat/6.0D0)**0.7D0

end subroutine ifetgt

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifefbs(nout,iprint)

  !+ad_name  ifefbs
  !+ad_summ  Routine to calculate the first wall, blanket and shield volumes,
  !+ad_summ  masses and other parameters, for an Inertial Fusion Energy device
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the first wall, blanket and shield volumes,
  !+ad_desc  masses and other parameters, for an Inertial Fusion Energy device.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  ife.h90
  !+ad_call  fwblsh.h90
  !+ad_call  build.h90
  !+ad_call  cost.h90
  !+ad_call  pulse.h90
  !+ad_call  phydat.h90
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.86
  !+ad_docs  Moir et al., Fusion Technology, vol.25 (1994) p.5
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'ife.h90'
  include 'fwblsh.h90'
  include 'build.h90'
  include 'cost.h90'
  include 'pulse.h90'
  include 'phydat.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  real(kind(1.0D0)) :: den,life
  real(kind(1.0D0)), dimension(0:maxmat) :: matden
  integer :: i,j

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (iprint /= 1) then

     !  Material densities
     !  0 = void
     !  1 = steel
     !  2 = carbon
     !  3 = FLiBe (inferred from Moir et al)
     !  4 = Li2O
     !  5 = concrete
     !  6 = helium (at typical coolant temperatures)
     !  7 = xenon (taken as ten times the normal tabulated value)

     matden(0) = 0.0D0
     matden(1) = denstl
     matden(2) = 2300.0D0
     matden(3) = 2020.0D0
     matden(4) = 2010.0D0
     matden(5) = 2400.0D0
     matden(6) = 1.517D0
     matden(7) = 55.0D0

     !  Material masses

     do i = 0,maxmat
        den = matden(i)
        chmatm(i) = chmatv(i) * den
        do j = 1,3
           fwmatm(j,i) = fwmatv(j,i) * den
           v1matm(j,i) = v1matv(j,i) * den
           blmatm(j,i) = blmatv(j,i) * den
           v2matm(j,i) = v2matv(j,i) * den
           shmatm(j,i) = shmatv(j,i) * den
           v3matm(j,i) = v3matv(j,i) * den
        end do
     end do

     !  Total masses of components (excluding coolant)

     fwmass = 0.0D0
     whtblkt = 0.0D0
     whtshld = 0.0D0
     do i = 1,5
        do j = 1,3
           fwmass = fwmass + fwmatm(j,i)
           whtblkt = whtblkt + blmatm(j,i)
           whtshld = whtshld + shmatm(j,i)
        end do
     end do

     !  Other masses

     whtblbe = 0.0D0
     whtblvd = 0.0D0
     whtblss = 0.0D0
     wtblli2o = 0.0D0
     do j = 1,3
        whtblss = whtblss + blmatm(j,1)
        wtblli2o = wtblli2o + blmatm(j,4)
     end do

     !  Total mass of FLiBe

     mflibe = chmatm(3)
     do j = 1,3
        mflibe = mflibe + fwmatm(j,3) + v1matm(j,3) + blmatm(j,3) + &
             v2matm(j,3) + shmatm(j,3) + v3matm(j,3)
     end do

     !  A fraction FBREED of the total breeder inventory is outside the
     !  core region, i.e. is in the rest of the heat transport system

     if ((fbreed < 0.0D0).or.(fbreed > 0.999D0)) then
        write(nout,*) 'Error in routine IFEFBS:'
        write(nout,*) 'FBREED = ',fbreed
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

     !  Following assumes that use of FLiBe and Li2O are
     !  mutually exclusive

     mflibe = mflibe / (1.0D0 - fbreed)
     wtblli2o = wtblli2o / (1.0D0 - fbreed)

     !  Blanket and first wall lifetimes (HYLIFE-II: = plant life)

     if (ifetyp == 3) THEN
        life = tlife
     else
        life = min( tlife, abktflnc/(wallmw*cfactr) )
     end if

     bktlife = life
     fwlife = life

     !  Cryostat mass (=zero)

     cryomass = 0.0D0

  end if

  if ((iprint == 0).or.(sect12 == 0)) return

  !  Output section

  call oheadr(nout,'First Wall, Blanket, Shield')

  call ovarre(nout,'First wall area (m2)','(fwarea)',fwarea)
  call ovarre(nout,'First wall mass (kg)','(fwmass)',fwmass)
  call ovarre(nout,'Blanket mass (kg)','(whtblkt)',whtblkt)
  call ovarre(nout,'Total mass of FLiBe (kg)','(mflibe)',mflibe)
  call ovarre(nout,'Shield mass (kg)','(whtshld)',whtshld)

end subroutine ifefbs

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifepw1

  !+ad_name  ifepw1
  !+ad_summ  Routine to calculate the first part of the heat transport
  !+ad_summ  and plant power balance constituents, for an IFE power plant
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  None
  !+ad_desc  This routine calculates the first part of the heat transport
  !+ad_desc  and plant power balance constituents, for an IFE power plant.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  ife.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  phydat.h90
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  24/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, pp.67,89
  !+ad_docs  Bourque et al., Fusion Technology vol.21 (1992) 1465
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'fwblsh.h90'
  include 'htpwr.h90'
  include 'phydat.h90'
  include 'ife.h90'

  !  Arguments

  !  Local variables

  real(kind(1.0D0)) :: pdrvmw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Driver power reaching target (MW)

  pdrvmw = 1.0D-6 * pdrive

  !  Primary nuclear heating (MW)

  priheat = emult * powfmw

  !  Useful (high-grade) thermal power (MW)

  pthermmw = priheat * (1.0D0-fhole)

  !  Assume 0.24 of thermal power is intercepted by the first wall
  !  (Bourque et al)
  !  HYLIFE-II case: Assume FLiBe flows intercept all fusion power
  !  and provide the energy multiplication as though it were a
  !  conventional blanket

  if (ifetyp /= 3) then
     pfwdiv = 0.24D0 * pthermmw
     pnucblkt = pthermmw - pfwdiv
  else
     pfwdiv = 0.0D0
     pnucblkt = pthermmw
  end if
  pnucshld = 0.0D0

  !  Lost fusion power (MW)

  pnucloss = priheat - pthermmw

  !  Number of primary heat exchangers

  rnphx = max(2.0D0, (priheat/400.0D0 + 0.8D0) )

  !  Secondary heat (some of it... rest calculated in IFEPW2)

  !  Wall plug driver power (MW)

  pinjwp = pdrvmw/etadrv

  !  Waste driver power (MW)

  pinjht = pinjwp - pdrvmw

  !  Cryogenic power (MW)
  !  Cryogenic temperature is assumed to be 4.5K

  crypmw = pifecr
  helpow = 1.0D6 * crypmw * (0.13D0 * 4.5D0)/(293.0D0 - 4.5D0)

end subroutine ifepw1

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifeacp(nout,iprint)

  !+ad_name  ifeacp
  !+ad_summ  Routine to calculate AC power requirements for an IFE power plant
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the AC power requirements for an IFE power plant.
  !+ad_prob  None
  !+ad_call  bldgvol.h90
  !+ad_call  htpwr.h90
  !+ad_call  osections.h90
  !+ad_call  ife.h90
  !+ad_call  oblnkl
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  25/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.68
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'bldgvol.h90'
  include 'htpwr.h90'
  include 'osections.h90'
  include 'ife.h90'

  ! Arguments

  integer, intent(in) :: iprint,nout

  !  Local variables

  real(kind(1.0D0)), save :: basemw,pmwpm2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (iprint /= 1) then

     !  Facility base load, MW (loads not dependent on floor area)

     basemw = baseel * 1.0D-6

     !  Power needed per floor area, MW/m2

     pmwpm2 = pwpm2 * 1.0D-6

     !  Total pulsed power system load, MW

     pacpmw = fmgdmw + crypmw + vachtmw + tdspmw + tfacmw + &
          (htpmw*reprat/6.0D0) + trithtmw + pinjwp + basemw + &
          (efloor*pmwpm2)

     !  Total power to facility loads, MW

     fcsht  = basemw + (efloor*pmwpm2) + 0.05D0*pacpmw

     !  Estimate of the total low voltage power, MW

     tlvpmw = fcsht + trithtmw + (htpmw*reprat/6.0D0) + vachtmw + &
          0.5D0*crypmw + tfacmw

  end if

  if ((iprint == 0).or.(sect17 == 0)) return

  !  Output section

  call oheadr(nout,'AC Power')

  call ovarre(nout,'Facility base load (MW)','(basemw)',basemw)
  call ovarre(nout,'Total floor space (m2)','(efloor)',efloor)
  call ovarre(nout,'Power/floor area (MW/m2)','(pmwpm2)',pmwpm2)
  call ovarre(nout,'MGF units (MW)','(fmgdmw)',fmgdmw)
  call ovarre(nout,'Driver power supplies (MW)','(pinjwp)', &
       pinjwp)
  call ovarre(nout,'Target delivery system (MW)','(tdspmw)', &
       tdspmw)
  call ovarre(nout,'Target factory (MW)','(tfacmw)', &
       tfacmw)
  call ovarre(nout,'Tritium processing plant (MW)','(trithtmw)', &
       trithtmw)
  call ovarre(nout,'Vacuum pump motors (MW)','(vachtmw)',vachtmw)
  call ovarre(nout,'Cryogenic comp motors (MW)','(crypmw)',crypmw)
  call ovarre(nout,'Heat transport system pump motors (MW)', &
       '(htpmw*reprat/6)',htpmw*reprat/6.0D0)
  call oblnkl(nout)
  call ovarre(nout,'Total pulsed power (MW)','(pacpmw)',pacpmw)
  call ovarre(nout,'Total facility power (MW)','(fcsht)',fcsht)
  call ovarre(nout,'Total low voltage power (MW)','(tlvpmw)',tlvpmw)

end subroutine ifeacp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifepw2(nout,iprint)

  !+ad_name  ifepw2
  !+ad_summ  Routine to calculate the rest of the IFE heat transport
  !+ad_summ  and plant power balance constituents, not already calculated in
  !+ad_summ  IFEPW1 or IFEACP
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the rest of the IFE heat transport
  !+ad_desc  and plant power balance constituents, not already calculated in
  !+ad_desc  routines <A HREF="ifepw1.html">IFEPW1</A> or
  !+ad_desc  <A HREF="ifeacp.html">IFEACP</A>.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  cost.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  osections.h90
  !+ad_call  phydat.h90
  !+ad_call  ife.h90
  !+ad_call  oblnkl
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarre
  !+ad_call  ovarrf
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  25/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.67
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'cost.h90'
  include 'fwblsh.h90'
  include 'htpwr.h90'
  include 'osections.h90'
  include 'phydat.h90'
  include 'ife.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  real(kind(1.0D0)) :: precir

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (iprint /= 1) then

     !  Facility heat removal (fcsht calculated in IFEACP)

     facht = fcsht

     !  Total secondary heat

     psecht = pinjht + pnucloss + facht + vachtmw + trithtmw + &
          tdspmw + tfacmw + crypmw + htpmw

     !  Total plant heat removal

     ctht = priheat + psecht 

     !  Number of intermediate heat exchangers

     rnihx = max(2.0D0, (ctht/50.0D0 + 0.8D0) )

     !  Calculate powers relevant to a power-producing plant

     if (ireactor == 1) then

        !  Gross electric power

        pgrossmw = pthermmw * etath

        !  Balance of plant recirculating power fraction

        fgrosbop = min( 0.5D0, ( fauxbop/(pgrossmw/1000.0D0)**0.6D0) )

        !  Total recirculating power

        precir = (fgrosbop*pgrossmw) + pacpmw

        !  Net electric power

        pnetelmw = pgrossmw - precir

        !  Scaling to prevent negative pnetelmw

        if ( (pnetelmw < 1.0D0).and.(ipnet == 0) ) then
           pnetelmw = 1.0D0 / ( 1.0D0 + abs(pnetelmw-1.0D0))
        end if

     end if

  end if

  if ((iprint == 0).or.(sect14 == 0)) return

  !  Output section

  call oheadr(nout,'Power / Heat Transport')
  call ovarre(nout,'Fusion power (MW)','(powfmw)',powfmw)
  call ovarre(nout,'Fusion power escaping via holes (MW)', &
       '(pnucloss)',pnucloss)
  call ovarre(nout,'Power multiplication factor','(emult)',emult)
  call ovarre(nout,'Driver wall plug power (MW)','(pinjwp)' &
       ,pinjwp)
  call ovarre(nout,'First wall nuclear heating (MW)','(pfwdiv)', &
       pfwdiv)
  call ovarre(nout,'Blanket nuclear heating (MW)','(pnucblkt)', &
       pnucblkt)
  call ovarre(nout,'Primary heat (MW)','(pthermmw)',pthermmw)
  call ovarre(nout,'Secondary heat (MW)','(psecht)',psecht)
  call oblnkl(nout)
  call ovarre(nout,'Heat removal from driver power (MW)', &
       '(pinjht)',pinjht)
  call ovarre(nout,'Heat removal from cryogenic plant (MW)', &
       '(crypmw)',crypmw)
  call ovarre(nout,'Heat removal from vacuum pumps (MW)', &
       '(vachtmw)',vachtmw)
  call ovarre(nout,'Heat removal from target factory (MW)', &
       '(tfacmw)',tfacmw)
  call ovarre(nout,'Heat removal from delivery system (MW)', &
       '(tdspmw)',tdspmw)
  call ovarre(nout,'Heat removal from tritium plant (MW)', &
       '(trithtmw)',trithtmw)
  call ovarre(nout,'Heat removal from facilities (MW)','(facht)' &
       ,facht)
  call ovarrf(nout,'Number of primary heat exchangers','(rnphx)' &
       ,rnphx)
  call ovarrf(nout,'Number of intermediate heat exchangers', &
       '(rnihx)',rnihx)
  call ovarre(nout,'Total plant heat rejection (MW)','(ctht)',ctht)

  if (ireactor /= 1) return

  call osubhd(nout,'Reactor powers :')
  call ovarre(nout,'Gross electric power (MW)','(pgrossmw)' &
       ,pgrossmw)
  call ovarre(nout,'Net electric power (MW)','(pnetelmw)',pnetelmw)
  call ovarre(nout,'Balance of plant aux. power fraction', &
       '(fgrosbop)',fgrosbop)

end subroutine ifepw2

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifevac(nout,iprint)

  !+ad_name  ifevac
  !+ad_summ  Routine to calculate parameters of the vacuum system for an
  !+ad_summ  Inertial Fusion Energy power plant
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the parameters of the vacuum system for an
  !+ad_desc  Inertial Fusion Energy power plant.
  !+ad_desc  <P>The calculated values are hard-wired; they are based loosely
  !+ad_desc  on those for a tokamak of 6m major radius.
  !+ad_prob  None
  !+ad_call  vaccom.h90
  !+ad_call  torsdat.h90
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  25/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.87
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'vaccom.h90'
  include 'torsdat.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dlscal = 2.0D0
  nvduct = 16
  vacdshm = 0.0D0
  vcdimax = 0.3D0
  vpumpn = 32.0D0

end subroutine ifevac

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ifebdg(nout,iprint)

  !+ad_name  ifebdg
  !+ad_summ  Routine to calculate the volumes of the buildings required for
  !+ad_summ  an Inertial Fusion Energy power plant
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the volumes of the buildings required for
  !+ad_desc  an Inertial Fusion Energy power plant. The method is based
  !+ad_desc  closely on that for tokamaks etc. in routine
  !+ad_desc  <A HREF="bldgs.html">BLDGS</A>.
  !+ad_prob  None
  !+ad_call  bldgcom.h90
  !+ad_call  bldgvol.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  ife.h90
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  21/03/97 PJK Initial version
  !+ad_hist  25/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  F/MI/PJK/LOGBOOK12, p.87
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'bldgcom.h90'
  include 'bldgvol.h90'
  include 'fwblsh.h90'
  include 'htpwr.h90'
  include 'ife.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  real(kind(1.0D0)) :: cran,dcl,dcw,fac2,fac3,hcl,hcw,hrbi,rbh,rbl,rbw, &
       rmbh,rmbl,rmbw,rwl,rww,shh,tch,tcl,tcw,wgts,wsa
  real(kind(1.0D0)), save :: cryv,elev,rbv,rmbv,vrci,wsv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (iprint /= 1) then

     !  Reactor building
     !  ================

     !  Total internal height

     hrbi = zl7 + zu7

     !  Distance from centre of device to wall

     wrbi = r7

     !  Internal volume (square floor)

     vrci = (2.0D0 * wrbi)**2 * hrbi

     !  External dimensions
     !  RBWT = wall thickness
     !  RBRT = roof thickness
     !  FNDT = foundation thickness

     rbw = 2.0D0 * (r7 + rbwt)
     rbl = rbw
     rbh = hrbi + rbrt + fndt

     !  External volume

     rbv = rbw * rbl * rbh

     !  Maintenance building
     !  ====================

     !  The reactor maintenance building includes the hot cells, the
     !  decontamination chamber, the transfer corridors, and the waste
     !  treatment building.  The dimensions of these areas are scaled
     !  from a reference (tokamak) design based on the shield sector size.

     !  Shield height

     shh = zl6 + zu6

     !  Transport corridor size

     tcw = r6 + 4.0D0*trcl
     tcl = 5.0D0*tcw + 2.0D0*hcwt

     !  Decontamination cell size

     dcw = 2.0D0*tcw + 1.0D0
     dcl = 2.0D0*tcw + 1.0D0

     !  Hot cell size

     hcw = r6 + 3.0D0*hccl + 2.0D0
     hcl = 3.0D0*r6 + 4.0D0*hccl + tcw

     !  Radioactive waste treatment

     rww = dcw
     rwl = hcl-dcl-hcwt

     !  Maintenance building dimensions

     rmbw = hcw + dcw + 3.0D0*hcwt
     rmbl = hcl + 2.0D0*hcwt

     !  Height

     if (wgt2 > 1.0D0) then
        wgts = wgt2
     else
        wgts = whtshld
     end if
     cran = 9.41D-6*wgts + 5.1D0
     rmbh = 10.0D0 + (zl6+zu6) + trcl + cran + 5.1D0 + stcl + fndt
     tch = shh + stcl + fndt

     !  Volume

     fac2 = 2.8D0
     rmbv = fac2*rmbw*rmbl*rmbh + tcw*tcl*tch

     !  Warm shop and hot cell gallery

     wsa = (rmbw+7.0D0)*20.0D0 + rmbl*7.0D0
     fac3 = 1.9D0
     wsv = fac3 * wsa*rmbh

     !  Cryogenic building volume

     cryv = 55.0D0 * sqrt(helpow)

     !  Electrical building volume
     !  (set equal to power injection (i.e. driver) building volume)

     elev = pibv

     !  Calculate effective floor area for ac power module

     efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0

     !  Convert local into global variables

     admvol = admv
     convol = conv
     elevol = elev
     rbvol  = rbv
     rmbvol = rmbv
     shovol = shov
     volrci = vrci
     wsvol  = wsv

     !  Total volume of nuclear buildings

     volnucb = ( vrci + rmbv + wsv + triv + cryv )

  end if

  if ((iprint == 0).or.(sect16 == 0)) return

  !  Output section

  call oheadr(nout,'Plant Buildings System')
  call ovarre(nout,'Internal volume of reactor building (m3)', &
       '(vrci)',vrci)
  call ovarre(nout,'Dist from device centre to bldg wall (m)', &
       '(wrbi)',wrbi)
  call ovarre(nout,'Effective floor area (m2)','(efloor)',efloor)
  call ovarre(nout,'Reactor building volume (m3)','(rbv)',rbv)
  call ovarre(nout,'Reactor maintenance building volume (m3)', &
       '(rmbv)',rmbv)
  call ovarre(nout,'Warmshop volume (m3)','(wsv)',wsv)
  call ovarre(nout,'Tritium building volume (m3)','(triv)',triv)
  call ovarre(nout,'Electrical building volume (m3)','(elev)',elev)
  call ovarre(nout,'Control building volume (m3)','(conv)',conv)
  call ovarre(nout,'Cryogenics building volume (m3)','(cryv)',cryv)
  call ovarre(nout,'Administration building volume (m3)','(admv)', &
       admv)
  call ovarre(nout,'Shops volume (m3)','(shov)',shov)
  call ovarre(nout,'Total volume of nuclear buildings (m3)', &
       '(volnucb)',volnucb)

end subroutine ifebdg
