module fwbs_variables
  !! author: J. Morris (UKAEA), M. Kovari (UKAEA)
  !! 
  !! Module containing global variables relating to the first wall, blanket and 
  !! shield components
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(8) :: bktlife
  !! Full power blanket lifetime (years)

  real(8) :: coolmass
  !! mass of water coolant (in shield, blanket, first wall, divertor) (kg)

  real(8) :: vvmass
  !! vacuum vessel mass (kg)

  real(8) :: denstl
  !! density of steel (kg/m3)
  !#TODO: should this be in constants. Is currently an input. Should be a list of preapproved options?

  real(8) :: denw
  !! density of tungsten (kg/m3)
  !#TODO: same as above with steel?

  real(8) :: dewmkg
  !! total mass of vacuum vessel + cryostat (kg) (calculated if blktmodel>0)
  !# TODO: blktmodel needs consolidating with iblanket

  real(8) :: emult
  !! energy multiplication in blanket and shield

  real(8) :: emultmw
  !! power due to energy multiplication in blanket and shield [MW]

  real(8) :: fblss
  !! KIT blanket model: steel fraction of breeding zone

  real(8) :: fdiv
  !! Solid angle fraction taken by one divertor

  real(8) :: fhcd
  !! area fraction covered by heating/current drive apparatus plus diagnostics

  real(8) :: fhole
  !! area fraction taken up by other holes (IFE)

  integer :: fwbsshape
  !! switch for first wall, blanket, shield and vacuum vessel shape:
  !!
  !! - =1 D-shaped (cylinder inboard + ellipse outboard)
  !! - =2 defined by two ellipses
  !#TODO: change to adopt switch naming convention

  real(8) :: fwlife
  !! first wall full-power year lifetime (y)

  real(8) :: fwmass
  !! first wall mass (kg)

  real(8) :: fw_armour_mass
  !! first wall armour mass (kg)

  real(8) :: fw_armour_thickness
  !! first wall armour thickness (m)

  real(8) :: fw_armour_vol
  !! first wall armour volume (m3)

  integer :: iblanket
  !! switch for blanket model:
  !!
  !! - =1 CCFE HCPB model
  !! - =2 KIT HCPB model
  !! - =3 CCFE HCPB model with Tritium Breeding Ratio calculation
  !! - =4 KIT HCLL model

  integer :: iblnkith
  !! switch for inboard blanket:
  !!
  !! - =0 No inboard blanket (blnkith=0.0)
  !! - =1 Inboard blanket present

  integer :: inuclear
  !! switch for nuclear heating in the coils:
  !!
  !! - =0 Frances Fox model (default)
  !! - =1 Fixed by user (qnuc)

  real(8) :: qnuc
  !! nuclear heating in the coils (W) (`inuclear=1`)

  real(8) :: li6enrich
  !! lithium-6 enrichment of breeding material (%)

  real(8) :: pnucblkt
  !! nuclear heating in the blanket (MW)

  real(8) :: pnuc_cp
  !! Total nuclear heating in the ST centrepost (MW)
  
  real(8) :: pnuc_cp_sh
  !! Neutronic shield nuclear heating in the ST centrepost (MW)
  
  real(8) :: pnuc_cp_tf
  !! TF neutronic nuclear heating in the ST centrepost (MW)

  real(8) :: pnucdiv
  !! nuclear heating in the divertor (MW)

  real(8) :: pnucfw
  !! nuclear heating in the first wall (MW)

  real(8) :: pnuchcd
  !! nuclear heating in the HCD apparatus and diagnostics (MW)

  real(8) :: pnucloss
  !! nuclear heating lost via holes (MW)

  real(8) :: pnucvvplus
  !! nuclear heating to vacuum vessel and beyond(MW)

  real(8) :: pnucshld
  !! nuclear heating in the shield (MW)

  real(8) :: whtblkt
  !! mass of blanket (kg)

  real(8) :: whtblss
  !! mass of blanket - steel part (kg)

  real(8) :: armour_fw_bl_mass
  !! Total mass of armour, first wall and blanket (kg)

  ! CCFE HCPB Blanket Model (with or without TBR calculation) iblanket=1,3
  ! ----------

  real(8) :: breeder_f
  !! Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (`iteration variable 108`)
  
  real(8) :: breeder_multiplier
  !! combined breeder/multipler fraction of blanket by volume
  
  real(8) :: vfcblkt
  !! He coolant fraction of blanket by volume (`iblanket= 1,3` (CCFE HCPB))
  
  real(8) :: vfpblkt
  !! He purge gas fraction of blanket by volume (`iblanket= 1,3` (CCFE HCPB))

  real(8) :: whtblli4sio4
  !! mass of lithium orthosilicate in blanket (kg) (`iblanket=1,3` (CCFE HCPB))
  
  real(8) :: whtbltibe12
  !! mass of titanium beryllide in blanket (kg) (`iblanket=1,3` (CCFE HCPB))

  real(8) :: neut_flux_cp
  !! Centrepost TF fast neutron flux (E > 0.1 MeV) [m^(-2).^(-1)]
  !! This variable is only calculated for superconducting (i_tf_sup = 1 )
  !! spherical tokamal magnet designs (itart = 0)

  real(8) :: f_neut_shield
  !! Fraction of nuclear power shielded before the CP magnet (ST)
  !! ( neut_absorb = -1 --> a fit on simplified MCNP neutronic
  !! calculation is used assuming water cooled (13%) tungesten carbyde )
  
  !  KIT HCPB blanket model (iblanket = 2)
  ! ----------

  integer :: breedmat
  !! breeder material switch (iblanket=2 (KIT HCPB)):
  !!
  !! - =1 Lithium orthosilicate
  !! - =2 Lithium methatitanate
  !! - =3 Lithium zirconate

  real(8) :: densbreed
  !! density of breeder material (kg/m3) (`iblanket=2` (KIT HCPB))

  real(8) :: fblbe
  !! beryllium fraction of blanket by volume (if `iblanket=2`, is Be fraction of breeding zone)

  real(8) :: fblbreed
  !! breeder fraction of blanket breeding zone by volume (`iblanket=2` (KIT HCPB))

  real(8) :: fblhebmi
  !! helium fraction of inboard blanket box manifold by volume (`iblanket=2` (KIT HCPB))

  real(8) :: fblhebmo
  !! helium fraction of outboard blanket box manifold by volume (`iblanket=2` (KIT HCPB))

  real(8) :: fblhebpi
  !! helium fraction of inboard blanket back plate by volume (`iblanket=2` (KIT HCPB))

  real(8) :: fblhebpo
  !! helium fraction of outboard blanket back plate by volume (`iblanket=2` (KIT HCPB))
  
  integer :: hcdportsize
  !! switch for size of heating/current drive ports (`iblanket=2` (KIT HCPB)):
  !!
  !! - =1 'small'
  !! - =2 'large'
  !#TODO: switch name and also large and small not descriptive enough
  
  real(8) :: nflutf
  !! peak fast neutron fluence on TF coil superconductor (n/m2) (`iblanket=2` (KIT HCPB))

  integer :: npdiv
  !! number of divertor ports (`iblanket=2` (KIT HCPB))

  integer :: nphcdin 
  !! number of inboard ports for heating/current drive (`iblanket=2` (KIT HCPB))

  integer :: nphcdout
  !! number of outboard ports for heating/current drive (`iblanket=2` (KIT HCPB))

  real(8) :: tbr
  !! tritium breeding ratio (`iblanket=2,3` (KIT HCPB/HCLL))

  real(8) :: tritprate
  !! tritium production rate (g/day) (`iblanket=2` (KIT HCPB))

  real(8) :: vvhemax
  !! maximum helium concentration in vacuum vessel at end of plant life (appm) 
  !! (`iblanket=2` (KIT HCPB))

  real(8) :: wallpf
  !! neutron wall load peaking factor (`iblanket=2` (KIT HCPB))

  real(8) :: whtblbreed 
  !! mass of blanket - breeder part (kg) (`iblanket=2` (KIT HCPB))

  real(8) :: whtblbe
  !! mass of blanket - beryllium part (kg)

  ! CCFE HCPB model with Tritium Breeding Ratio calculation (iblanket=3)
  ! ---------------

  integer :: iblanket_thickness
  !! Blanket thickness switch (Do not set blnkith, blnkoth, fwith or fwoth when `iblanket=3`):
  !!
  !! - =1 thin    0.53 m inboard, 0.91 m outboard
  !! - =2 medium  0.64 m inboard, 1.11 m outboard
  !! - =3 thick   0.75 m inboard, 1.30 m outboard

  integer :: primary_pumping
  !! Switch for pumping power for primary coolant (mechanical power only and peak first wall 
  !! temperature is only calculated if `primary_pumping=2`):
  !!
  !! - =0 User sets pump power directly (htpmw_blkt, htpmw_fw, htpmw_div, htpmw_shld)
  !! - =1 User sets pump power as a fraction of thermal power (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)
  !! - =2 Mechanical pumping power is calculated
  !! - =3 Mechanical pumping power is calculated using specified pressure drop

  integer :: secondary_cycle
  !! Switch for power conversion cycle:
  !!
  !! - =0 Set efficiency for chosen blanket, from detailed models (divertor heat not used)
  !! - =1 Set efficiency for chosen blanket, from detailed models (divertor heat used)
  !! - =2 user input thermal-electric efficiency (etath)
  !! - =3 steam Rankine cycle
  !! - =4 supercritical CO2 cycle

  integer :: coolwh
  !! Switch for blanket coolant (set via blkttype):
  !!
  !! - =1 helium
  !! - =2 pressurized water
  !#TODO: change switch name to satisfy convention

  real(8) :: afwi
  !! inner radius of inboard first wall/blanket coolant channels (stellarator only) (m)
  !#TODO move to stellarator?

  real(8) :: afwo
  !! inner radius of outboard first wall/blanket coolant channels (stellarator only) (m)
  !#TODO move to stellarator?

  character(len=6) :: fwcoolant
  !! switch for first wall coolant (can be different from blanket coolant):
  !!
  !! - 'helium' 
  !! - 'water'

  real(8) :: fw_wall
  !! wall thickness of first wall coolant channels (m)

  real(8) :: afw
  !! radius of first wall cooling channels (m)
  
  real(8) :: pitch
  !! pitch of first wall cooling channels (m)

  real(8) :: fwinlet
  !! inlet temperature of first wall coolant (K)

  real(8) :: fwoutlet
  !! outlet temperature of first wall coolant (K)

  real(8) :: fwpressure
  !! first wall coolant pressure (Pa) (`secondary_cycle>1`)

  real(8) :: tpeak
  !! peak first wall temperature (K)

  real(8) :: roughness
  !! first wall channel roughness epsilon (m)

  real(8) :: fw_channel_length
  !! Length of a single first wall channel (all in parallel) (m)
  !! (`iteration variable 114`, useful for `constraint equation 39`)

  real(8) :: peaking_factor
  !! peaking factor for first wall heat loads. (Applied separately to inboard and outboard loads.
  !! Applies to both neutron and surface loads. Only used to calculate peak temperature - not 
  !! the coolant flow rate.)

  real(8) :: blpressure
  !! blanket coolant pressure (Pa) (`secondary_cycle>1`)

  real(8) :: inlet_temp
  !! inlet temperature of blanket coolant  (K) (`secondary_cycle>1`)

  real(8) :: outlet_temp
  !! Outlet temperature of blanket coolant (K) (`secondary_cycle>1`)
  !!
  !! - input if `coolwh=1` (helium)
  !! - calculated if `coolwh=2` (water)

  real(8) :: coolp
  !! blanket coolant pressure (Pa) (stellarator only)

  integer :: nblktmodpo
  !! number of outboard blanket modules in poloidal direction (`secondary_cycle>1`)

  integer :: nblktmodpi
  !! number of inboard blanket modules in poloidal direction (`secondary_cycle>1`)

  integer :: nblktmodto
  !! number of outboard blanket modules in toroidal direction (`secondary_cycle>1`)

  integer :: nblktmodti
  !! number of inboard blanket modules in toroidal direction (`secondary_cycle>1`)

  real(8) :: tfwmatmax
  !! maximum temperature of first wall material (K) (`secondary_cycle>1`)

  real(8) :: fw_th_conductivity
  !! thermal conductivity of first wall material at 293 K (W/m/K) (Temperature dependence 
  !! is as for unirradiated Eurofer)

  real(8) :: fvoldw
  !! area coverage factor for vacuum vessel volume

  real(8) :: fvolsi
  !! area coverage factor for inboard shield volume

  real(8) :: fvolso
  !! area coverage factor for outboard shield volume

  real(8) :: fwclfr
  !! first wall coolant fraction (calculated if `lpulse=1` or `ipowerflow=1`)

  real(8) :: praddiv
  !! Radiation power incident on the divertor (MW)

  real(8) :: pradfw
  !! Radiation power incident on the first wall (MW)

  real(8) :: pradhcd
  !! Radiation power incident on the heating and current drive system (MW)

  real(8) :: pradloss
  !! Radiation power lost through holes (eventually hits shield) (MW)
  !! Only used for stellarator

  real(8) :: ptfnuc
  !! nuclear heating in the TF coil (MW)

  real(8) :: ptfnucpm3
  !! nuclear heating in the TF coil (MW/m3) (`blktmodel>0`)
  !#TODO: check usage of old blktmodel. Update to iblanket

  real(8) :: rdewex
  !! cryostat radius (m)

  real(8) :: zdewex
  !! cryostat height (m)

  real(8) :: rpf2dewar
  !! radial distance between outer edge of largest (`ipfloc=3`) PF coil (or stellarator 
  !! modular coil) and cryostat (m)

  real(8) :: vdewex
  !! cryostat volume (m3)

  real(8) :: vdewin
  !! vacuum vessel volume (m3)

  real(8) :: vfshld
  !! coolant void fraction in shield

  real(8) :: volblkt
  !! volume of blanket (m3)

  real(8) :: volblkti
  !! volume of inboard blanket (m3)

  real(8) :: volblkto
  !! volume of outboard blanket (m3)

  real(8) :: volshld
  !! volume of shield (m3)

  real(8) :: whtshld
  !! mass of shield (kg)

  real(8) :: wpenshld
  !! mass of the penetration shield (kg)

  real(8) :: wtshldi
  !! mass of inboard shield (kg)

  real(8) :: wtshldo
  !! mass of outboard shield (kg)
  
  integer :: irefprop
  !! Switch to use REFPROP routines (stellarator only)
  !#TODO: number of stellarator only items here. Also appear in fispact. Tidy needed

  real(8) :: fblli
  !! lithium fraction of blanket by volume (stellarator only)

  real(8) :: fblli2o
  !! lithium oxide fraction of blanket by volume (stellarator only)

  real(8) :: fbllipb
  !! lithium lead fraction of blanket by volume (stellarator only)

  real(8) :: fblvd
  !! vanadium fraction of blanket by volume (stellarator only)

  real(8) :: wtblli2o
  !! mass of blanket - Li_2O part (kg)

  real(8) :: wtbllipb
  !! mass of blanket - Li-Pb part (kg)
  
  real(8) :: whtblvd
  !! mass of blanket - vanadium part (kg)

  real(8) :: whtblli
  !! mass of blanket - lithium part (kg)

  real(8) :: vfblkt
  !! coolant void fraction in blanket (`blktmodel=0`), (calculated if `blktmodel > 0`)

  integer :: blktmodel
  !! switch for blanket/tritium breeding model (see iblanket):
  !!
  !! - =0 original simple model
  !! - =1 KIT model based on a helium-cooled pebble-bed blanket (HCPB) reference design
  !#TODO: this needs investigating and removing after any required functionality is in iblanket

  real(8) :: declblkt
  !! neutron power deposition decay length of blanket structural material (m) (stellarators only)

  real(8) :: declfw
  !! neutron power deposition decay length of first wall structural material (m) (stellarators only)

  real(8) :: declshld
  !! neutron power deposition decay length of shield structural material (m) (stellarators only)

  integer :: blkttype
  !! Switch for blanket type:
  !!
  !! - =1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA_D_2M97B7
  !! - =2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !! - =3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !#TODO: this needs to be merged into iblanket and then removed.

  real(8) :: etaiso
  !! isentropic efficiency of FW and blanket coolant pumps

  real(8) :: etahtp
  !! electrical efficiency of primary coolant pumps

  contains

  subroutine init_fwbs_variables
    !! Initialise fwbs variables
    implicit none

    bktlife = 0.0D0  
    coolmass = 0.0D0  
    vvmass = 0.0D0  
    denstl = 7800.0D0
    denw = 19250.0D0 
    dewmkg = 0.0D0   
    emult = 1.269D0  
    emultmw = 0.0D0  
    fblss = 0.09705D0  
    fdiv = 0.115D0  
    fhcd = 0.0D0  
    fhole = 0.0D0  
    fwbsshape = 2
    fwlife = 0.0D0  
    fwmass = 0.0D0  
    fw_armour_mass = 0.0D0  
    fw_armour_thickness = 0.005D0  
    fw_armour_vol = 0.0D0  
    iblanket = 1  
    iblnkith = 1  
    inuclear = 0  
    qnuc = 0.0D0  
    li6enrich = 30.0D0  
    pnucblkt = 0.0D0  
    pnucdiv = 0.0D0  
    pnucfw = 0.0D0  
    pnuchcd = 0.0D0  
    pnucloss = 0.0D0  
    pnucvvplus = 0.0D0  
    pnucshld = 0.0D0  
    whtblkt = 0.0D0  
    whtblss = 0.0D0  
    armour_fw_bl_mass = 0.0D0  
    breeder_f = 0.5D0    
    breeder_multiplier = 0.75D0    
    vfcblkt = 0.05295D0    
    vfpblkt = 0.1D0  
    whtblli4sio4 = 0.0D0    
    whtbltibe12 = 0.0D0  
    f_neut_shield = -1.0D0    
    breedmat = 1  
    densbreed = 0.0D0  
    fblbe = 0.6D0  
    fblbreed = 0.154D0  
    fblhebmi = 0.4D0  
    fblhebmo = 0.4D0  
    fblhebpi = 0.6595D0  
    fblhebpo = 0.6713D0    
    hcdportsize = 1
    nflutf = 0.0D0  
    npdiv = 2  
    nphcdin = 2  
    nphcdout = 2  
    tbr = 0.0D0  
    tritprate = 0.0D0  
    vvhemax = 0.0D0  
    wallpf = 1.21D0  
    whtblbreed = 0.0D0  
    whtblbe = 0.0D0  
    iblanket_thickness = 2  
    primary_pumping = 2  
    secondary_cycle = 0  
    coolwh = 1
    afwi = 0.008D0
    afwo = 0.008D0
    fwcoolant = 'helium'  
    fw_wall = 0.003D0  
    afw = 0.006D0    
    pitch = 0.020D0  
    fwinlet = 573.0D0  
    fwoutlet = 823.0D0  
    fwpressure = 15.5D6  
    tpeak = 873.0D0  
    roughness = 1.0D-6  
    fw_channel_length = 4.0D0  
    peaking_factor = 1.0D0  
    blpressure = 15.5D6  
    inlet_temp = 573.0D0  
    outlet_temp = 823.0D0  
    coolp = 15.5D6  
    nblktmodpo = 8  
    nblktmodpi = 7  
    nblktmodto = 48  
    nblktmodti = 32  
    tfwmatmax = 823.0D0  
    fw_th_conductivity = 28.34D0  
    fvoldw = 1.74D0  
    fvolsi = 1.0D0  
    fvolso = 0.64D0  
    fwclfr = 0.15D0  
    praddiv = 0.0D0  
    pradfw = 0.0D0  
    pradhcd = 0.0D0  
    pradloss = 0.0D0  
    ptfnuc = 0.0D0  
    ptfnucpm3 = 0.0D0
    rdewex = 0.0D0  
    zdewex = 0.0D0  
    rpf2dewar = 0.5D0  
    vdewex = 0.0D0  
    vdewin = 0.0D0  
    vfshld = 0.25D0  
    volblkt = 0.0D0  
    volblkti = 0.0D0  
    volblkto = 0.0D0  
    volshld = 0.0D0  
    whtshld = 0.0D0  
    wpenshld = 0.0D0  
    wtshldi = 0.0D0    
    wtshldo = 0.0D0    
    irefprop = 1
    fblli = 0.0D0  
    fblli2o = 0.08D0  
    fbllipb = 0.68D0  
    fblvd = 0.0D0  
    wtblli2o = 0.0D0  
    wtbllipb = 0.0D0    
    whtblvd = 0.0D0  
    whtblli = 0.0D0  
    vfblkt = 0.25D0  
    blktmodel = 0
    declblkt = 0.075D0  
    declfw = 0.075D0  
    declshld = 0.075D0  
    blkttype = 3
    etaiso = 0.85D0  
    etahtp = 0.95D0  
    pnuc_cp = 0.0D0
    pnuc_cp_sh = 0.0D0
    pnuc_cp_tf = 0.0D0
    neut_flux_cp = 0.0D0
  end subroutine init_fwbs_variables
end module fwbs_variables