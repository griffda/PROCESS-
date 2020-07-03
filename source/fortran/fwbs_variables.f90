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

  real(dp) :: bktlife = 0.0D0
  !! blanket lifetime (years)

  real(dp) :: coolmass = 0.0D0
  !! mass of water coolant (in shield, blanket, first wall, divertor) (kg)

  real(dp) :: vvmass = 0.0D0
  !! vacuum vessel mass (kg)

  real(dp) :: denstl = 7800.0D0
  !! density of steel (kg/m3)
  !#TODO: should this be in constants. Is currently an input. Should be a list of preapproved options?

  real(dp) :: denw = 19250.0D0
  !! density of tungsten (kg/m3)
  !#TODO: same as above with steel?

  real(dp) :: dewmkg = 0.0D0
  !! total mass of vacuum vessel + cryostat (kg) (calculated if blktmodel>0)
  !# TODO: blktmodel needs consolidating with iblanket

  real(dp) :: emult = 1.269D0
  !! energy multiplication in blanket and shield

  real(dp) :: emultmw = 0.0D0
  !! power due to energy multiplication in blanket and shield [MW]

  real(dp) :: fblss = 0.09705D0
  !! KIT blanket model: steel fraction of breeding zone

  real(dp) :: fdiv = 0.115D0
  !! area fraction taken up by divertor

  real(dp) :: fhcd = 0.0D0
  !! area fraction covered by heating/current drive apparatus plus diagnostics

  real(dp) :: fhole = 0.0D0
  !! area fraction taken up by other holes (IFE)

  integer :: fwbsshape = 2
  !! switch for first wall, blanket, shield and vacuum vessel shape:
  !!
  !! - =1 D-shaped (cylinder inboard + ellipse outboard)
  !! - =2 defined by two ellipses
  !#TODO: change to adopt switch naming convention

  real(dp) :: fwlife = 0.0D0
  !! first wall full-power lifetime (y)

  real(dp) :: fwmass = 0.0D0
  !! first wall mass (kg)

  real(dp) :: fw_armour_mass = 0.0D0
  !! first wall armour mass (kg)

  real(dp) :: fw_armour_thickness = 0.005D0
  !! first wall armour thickness (m)

  real(dp) :: fw_armour_vol = 0.0D0
  !! first wall armour volume (m3)

  integer :: iblanket = 1
  !! switch for blanket model:
  !!
  !! - =1 CCFE HCPB model
  !! - =2 KIT HCPB model
  !! - =3 CCFE HCPB model with Tritium Breeding Ratio calculation
  !! - =4 KIT HCLL model

  integer :: iblnkith = 1
  !! switch for inboard blanket:
  !!
  !! - =0 No inboard blanket (blnkith=0.0)
  !! - =1 Inboard blanket present

  integer :: inuclear = 0
  !! switch for nuclear heating in the coils:
  !!
  !! - =0 Frances Fox model (default)
  !! - =1 Fixed by user (qnuc)

  real(dp) :: qnuc = 0.0D0
  !! nuclear heating in the coils (W) (`inuclear=1`)

  real(dp) :: li6enrich = 30.0D0
  !! lithium-6 enrichment of breeding material (%)

  real(dp), bind(C) :: pnucblkt = 0.0D0
  !! nuclear heating in the blanket (MW)

  real(dp) :: pnuccp = 0.0D0
  !! nuclear heating in the ST centrepost (MW)

  real(dp) :: pnucdiv = 0.0D0
  !! nuclear heating in the divertor (MW)

  real(dp) :: pnucfw = 0.0D0
  !! nuclear heating in the first wall (MW)

  real(dp) :: pnuchcd = 0.0D0
  !! nuclear heating in the HCD apparatus and diagnostics (MW)

  real(dp) :: pnucloss = 0.0D0
  !! nuclear heating lost via holes (MW)

  real(dp) :: pnucvvplus = 0.0D0
  !! nuclear heating to vacuum vessel and beyond(MW)

  real(dp), bind(C) :: pnucshld = 0.0D0
  !! nuclear heating in the shield (MW)

  real(dp) :: whtblkt = 0.0D0
  !! mass of blanket (kg)

  real(dp) :: whtblss = 0.0D0
  !! mass of blanket - steel part (kg)

  real(dp) :: armour_fw_bl_mass = 0.0D0
  !! Total mass of armour, first wall and blanket (kg)

  ! CCFE HCPB Blanket Model (with or without TBR calculation) iblanket=1,3
  ! ----------

  real(dp) :: breeder_f = 0.5D0
  !! Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (`iteration variable 108`)
  
  real(dp) :: breeder_multiplier = 0.75D0
  !! combined breeder/multipler fraction of blanket by volume
  
  real(dp) :: vfcblkt = 0.05295D0
  !! He coolant fraction of blanket by volume (`iblanket= 1,3` (CCFE HCPB))
  
  real(dp) :: vfpblkt = 0.1D0
  !! He purge gas fraction of blanket by volume (`iblanket= 1,3` (CCFE HCPB))

  real(dp) :: whtblli4sio4 = 0.0D0
  !! mass of lithium orthosilicate in blanket (kg) (`iblanket=1,3` (CCFE HCPB))
  
  real(dp) :: whtbltibe12 = 0.0D0
  !! mass of titanium beryllide in blanket (kg) (`iblanket=1,3` (CCFE HCPB))

  real(dp) :: f_neut_shield = -1.0D0
  !! Fraction of nuclear power shielded before the CP magnet (ST)
  !! ( neut_absorb = -1 --> a fit on simplified MCNP neutronic
  !! calculation is used assuming water cooled (13%) tungesten carbyde )
  
  !  KIT HCPB blanket model (iblanket = 2)
  ! ----------

  integer :: breedmat = 1
  !! breeder material switch (iblanket=2 (KIT HCPB)):
  !!
  !! - =1 Lithium orthosilicate
  !! - =2 Lithium methatitanate
  !! - =3 Lithium zirconate

  real(dp) :: densbreed = 0.0D0
  !! density of breeder material (kg/m3) (`iblanket=2` (KIT HCPB))

  real(dp) :: fblbe = 0.6D0
  !! beryllium fraction of blanket by volume (if `iblanket=2`, is Be fraction of breeding zone)

  real(dp) :: fblbreed = 0.154D0
  !! breeder fraction of blanket breeding zone by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebmi = 0.4D0
  !! helium fraction of inboard blanket box manifold by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebmo = 0.4D0
  !! helium fraction of outboard blanket box manifold by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebpi = 0.6595D0
  !! helium fraction of inboard blanket back plate by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebpo = 0.6713D0
  !! helium fraction of outboard blanket back plate by volume (`iblanket=2` (KIT HCPB))
  integer :: hcdportsize = 1
  !! switch for size of heating/current drive ports (`iblanket=2` (KIT HCPB)):
  !!
  !! - =1 'small'
  !! - =2 'large'
  !#TODO: switch name and also large and small not descriptive enough
  
  real(dp) :: nflutf = 0.0D0
  !! peak fast neutron fluence on TF coil superconductor (n/m2) (`iblanket=2` (KIT HCPB))

  integer :: npdiv = 2
  !! number of divertor ports (`iblanket=2` (KIT HCPB))

  integer :: nphcdin = 2
  !! number of inboard ports for heating/current drive (`iblanket=2` (KIT HCPB))

  integer :: nphcdout = 2
  !! number of outboard ports for heating/current drive (`iblanket=2` (KIT HCPB))

  real(dp) :: tbr = 0.0D0
  !! tritium breeding ratio (`iblanket=2,3` (KIT HCPB/HCLL))

  real(dp) :: tritprate = 0.0D0
  !! tritium production rate (g/day) (`iblanket=2` (KIT HCPB))

  real(dp) :: vvhemax = 0.0D0
  !! maximum helium concentration in vacuum vessel at end of plant life (appm) 
  !! (`iblanket=2` (KIT HCPB))

  real(dp) :: wallpf = 1.21D0
  !! neutron wall load peaking factor (`iblanket=2` (KIT HCPB))

  real(dp) :: whtblbreed = 0.0D0
  !! mass of blanket - breeder part (kg) (`iblanket=2` (KIT HCPB))

  real(dp) :: whtblbe = 0.0D0
  !! mass of blanket - beryllium part (kg)

  ! CCFE HCPB model with Tritium Breeding Ratio calculation (iblanket=3)
  ! ---------------

  integer :: iblanket_thickness = 2
  !! Blanket thickness switch (Do not set blnkith, blnkoth, fwith or fwoth when `iblanket=3`):
  !!
  !! - =1 thin    0.53 m inboard, 0.91 m outboard
  !! - =2 medium  0.64 m inboard, 1.11 m outboard
  !! - =3 thick   0.75 m inboard, 1.30 m outboard

  integer :: primary_pumping = 2
  !! Switch for pumping power for primary coolant (mechanical power only and peak first wall 
  !! temperature is only calculated if `primary_pumping=2`):
  !!
  !! - =0 User sets pump power directly (htpmw_blkt, htpmw_fw, htpmw_div, htpmw_shld)
  !! - =1 User sets pump power as a fraction of thermal power (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)
  !! - =2 Mechanical pumping power is calculated
  !! - =3 Mechanical pumping power is calculated using specified pressure drop

  integer :: secondary_cycle = 0
  !! Switch for power conversion cycle:
  !!
  !! - =0 Set efficiency for chosen blanket, from detailed models (divertor heat not used)
  !! - =1 Set efficiency for chosen blanket, from detailed models (divertor heat used)
  !! - =2 user input thermal-electric efficiency (etath)
  !! - =3 steam Rankine cycle
  !! - =4 supercritical CO2 cycle

  integer :: coolwh = 1
  !! Switch for blanket coolant (set via blkttype):
  !!
  !! - =1 helium
  !! - =2 pressurized water
  !#TODO: change switch name to satisfy convention

  real(dp) :: afwi = 0.008D0
  !! inner radius of inboard first wall/blanket coolant channels (stellarator only) (m)
  !#TODO move to stellarator?

  real(dp) :: afwo = 0.008D0
  !! inner radius of outboard first wall/blanket coolant channels (stellarator only) (m)
  !#TODO move to stellarator?

  character(len=6) :: fwcoolant = 'helium'
  !! switch for first wall coolant (can be different from blanket coolant):
  !!
  !! - 'helium' 
  !! - 'water'

  real(dp) :: fw_wall = 0.003D0
  !! wall thickness of first wall coolant channels (m)

  real(dp) :: afw = 0.006D0
  !! radius of first wall cooling channels (m)
  
  real(dp) :: pitch = 0.020D0
  !! pitch of first wall cooling channels (m)

  real(dp) :: fwinlet = 573.0D0
  !! inlet temperature of first wall coolant (K)

  real(dp) :: fwoutlet = 823.0D0
  !! outlet temperature of first wall coolant (K)

  real(dp) :: fwpressure = 15.5D6
  !! first wall coolant pressure (Pa) (`secondary_cycle>1`)

  real(dp) :: tpeak = 873.0D0
  !! peak first wall temperature (K)

  real(dp) :: roughness = 1.0D-6
  !! first wall channel roughness epsilon (m)

  real(dp) :: fw_channel_length = 4.0D0
  !! Length of a single first wall channel (all in parallel) (m)
  !! (`iteration variable 114`, useful for `constraint equation 39`)

  real(dp) :: peaking_factor = 1.0D0
  !! peaking factor for first wall heat loads. (Applied separately to inboard and outboard loads.
  !! Applies to both neutron and surface loads. Only used to calculate peak temperature - not 
  !! the coolant flow rate.)

  real(dp) :: blpressure = 15.5D6
  !! blanket coolant pressure (Pa) (`secondary_cycle>1`)

  real(dp) :: inlet_temp = 573.0D0
  !! inlet temperature of blanket coolant  (K) (`secondary_cycle>1`)

  real(dp) :: outlet_temp = 823.0D0
  !! Outlet temperature of blanket coolant (K) (`secondary_cycle>1`)
  !!
  !! - input if `coolwh=1` (helium)
  !! - calculated if `coolwh=2` (water)

  real(dp) :: coolp = 15.5D6
  !! blanket coolant pressure (Pa) (stellarator only)

  integer :: nblktmodpo = 8
  !! number of outboard blanket modules in poloidal direction (`secondary_cycle>1`)

  integer :: nblktmodpi = 7
  !! number of inboard blanket modules in poloidal direction (`secondary_cycle>1`)

  integer :: nblktmodto = 48
  !! number of outboard blanket modules in toroidal direction (`secondary_cycle>1`)

  integer :: nblktmodti = 32
  !! number of inboard blanket modules in toroidal direction (`secondary_cycle>1`)

  real(dp) :: tfwmatmax = 823.0D0
  !! maximum temperature of first wall material (K) (`secondary_cycle>1`)

  real(dp) :: fw_th_conductivity = 28.34D0
  !! thermal conductivity of first wall material at 293 K (W/m/K) (Temperature dependence 
  !! is as for unirradiated Eurofer)

  real(dp) :: fvoldw = 1.74D0
  !! area coverage factor for vacuum vessel volume

  real(dp) :: fvolsi = 1.0D0
  !! area coverage factor for inboard shield volume

  real(dp) :: fvolso = 0.64D0
  !! area coverage factor for outboard shield volume

  real(dp) :: fwclfr = 0.15D0
  !! first wall coolant fraction (calculated if `lpulse=1` or `ipowerflow=1`)

  real(dp) :: praddiv = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: pradfw = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: pradhcd = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: pradloss = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: ptfnuc = 0.0D0
  !! nuclear heating in the TF coil (MW)

  real(dp) :: ptfnucpm3 = 0.0D0
  !! nuclear heating in the TF coil (MW/m3) (`blktmodel>0`)
  !#TODO: check usage of old blktmodel. Update to iblanket

  real(dp) :: rdewex = 0.0D0
  !! cryostat radius (m)

  real(dp) :: zdewex = 0.0D0
  !! cryostat height (m)

  real(dp) :: rpf2dewar = 0.5D0
  !! radial distance between outer edge of largest (`ipfloc=3`) PF coil (or stellarator 
  !! modular coil) and cryostat (m)

  real(dp) :: vdewex = 0.0D0
  !! cryostat volume (m3)

  real(dp) :: vdewin = 0.0D0
  !! vacuum vessel volume (m3)

  real(dp) :: vfshld = 0.25D0
  !! coolant void fraction in shield

  real(dp) :: volblkt = 0.0D0
  !! volume of blanket (m3)

  real(dp) :: volblkti = 0.0D0
  !! volume of inboard blanket (m3)

  real(dp) :: volblkto = 0.0D0
  !! volume of outboard blanket (m3)

  real(dp) :: volshld = 0.0D0
  !! volume of shield (m3)

  real(dp) :: whtshld = 0.0D0
  !! mass of shield (kg)

  real(dp) :: wpenshld = 0.0D0
  !! mass of the penetration shield (kg)

  real(dp) :: wtshldi = 0.0D0
  !! mass of inboard shield (kg)
  real(dp) :: wtshldo = 0.0D0
  !! mass of outboard shield (kg)
  
  integer :: irefprop = 1
  !! Switch to use REFPROP routines (stellarator only)
  !#TODO: number of stellarator only items here. Also appear in fispact. Tidy needed

  real(dp) :: fblli = 0.0D0
  !! lithium fraction of blanket by volume (stellarator only)

  real(dp) :: fblli2o = 0.08D0
  !! lithium oxide fraction of blanket by volume (stellarator only)

  real(dp) :: fbllipb = 0.68D0
  !! lithium lead fraction of blanket by volume (stellarator only)

  real(dp) :: fblvd = 0.0D0
  !! vanadium fraction of blanket by volume (stellarator only)

  real(dp) :: wtblli2o = 0.0D0
  !! mass of blanket - Li_2O part (kg)

  real(dp) :: wtbllipb = 0.0D0
  !! mass of blanket - Li-Pb part (kg)
  
  real(dp) :: whtblvd = 0.0D0
  !! mass of blanket - vanadium part (kg)

  real(dp) :: whtblli = 0.0D0
  !! mass of blanket - lithium part (kg)

  real(dp) :: vfblkt = 0.25D0
  !! coolant void fraction in blanket (`blktmodel=0`), (calculated if `blktmodel > 0`)

  integer :: blktmodel = 0
  !! switch for blanket/tritium breeding model (see iblanket):
  !!
  !! - =0 original simple model
  !! - =1 KIT model based on a helium-cooled pebble-bed blanket (HCPB) reference design
  !#TODO: this needs investigating and removing after any required functionality is in iblanket

  real(dp) :: declblkt = 0.075D0
  !! neutron power deposition decay length of blanket structural material (m) (stellarators only)

  real(dp) :: declfw = 0.075D0
  !! neutron power deposition decay length of first wall structural material (m) (stellarators only)

  real(dp) :: declshld = 0.075D0
  !! neutron power deposition decay length of shield structural material (m) (stellarators only)

  integer :: blkttype = 3
  !! Switch for blanket type:
  !!
  !! - =1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA_D_2M97B7
  !! - =2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !! - =3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !#TODO: this needs to be merged into iblanket and then removed.

  real(dp) :: etaiso = 0.85D0
  !! isentropic efficiency of FW and blanket coolant pumps

  real(dp) :: etahtp = 0.95D0
  !! electrical efficiency of primary coolant pumps

end module fwbs_variables