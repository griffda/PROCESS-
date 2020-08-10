module stellarator_variables
  !! author: S. Muldrew (UKAEA), F. Warmer, J. Lion (IPP Greifswald)
  !!
  !! Module containing global variables relating to the stellarator model
  !!
  !!### References
  !!
  !! - Stellarator Plasma Geometry Model for the Systems Code PROCESS, F. Warmer, 19/06/2013
  !! - Stellarator Divertor Model for the Systems Code PROCESS, F. Warmer, 21/06/2013
  !! - Stellarator Coil Model for the Systems Code PROCESS, F. Warmer and F. Schauer, 07/10/2013

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer :: istell = 0
  !! Switch for stellarator option (set via `device.dat`):
  !!
  !! - =0 use tokamak model
  !! - =1 use stellarator model: Helias5-b
  !! - =2 use stellarator model: Helias4-b
  !! - =3 use stellarator model: Helias3-b

  real(dp) :: bmn = 1.0D-3
  !! relative radial field perturbation

  real(dp) :: f_asym = 1.0D0
  !! divertor heat load peaking factor

  real(dp) :: f_rad = 0.85D0
  !! radiated power fraction in SOL

  real(dp) :: f_w = 0.5D0
  !! island size fraction factor

  real(dp) :: fdivwet = 0.333333333333333D0
  !! wetted fraction of the divertor area

  real(dp) :: flpitch = 1.0D-3
  !! field line pitch (rad)

  real(dp) :: hportamax = 0.0D0
  !! maximum available area for horizontal ports (m2)

  real(dp) :: hportpmax = 0.0D0
  !! maximum available poloidal extent for horizontal ports (m)

  real(dp) :: hporttmax = 0.0D0
  !! maximum available toroidal extent for horizontal ports (m)

  real(dp) :: iotabar = 1.0D0
  !! rotational transform (reciprocal of tokamak q) for stellarator confinement time scaling laws

  integer :: isthtr = 3
  !! Switch for stellarator auxiliary heating method:
  !!
  !! - = 1electron cyclotron resonance heating
  !! - = 2lower hybrid heating
  !! - = 3neutral beam injection

  integer :: m_res = 5
  !! poloidal resonance number

  integer :: n_res = 5
  !! toroidal resonance number

  real(dp) :: shear = 0.5D0
  !! magnetic shear, derivative of iotabar

  real(dp) :: vportamax = 0.0D0
  !! maximum available area for vertical ports (m2)

  real(dp) :: vportpmax = 0.0D0
  !! maximum available poloidal extent for vertical ports (m)

  real(dp) :: vporttmax = 0.0D0
  !! maximum available toroidal extent for vertical ports (m)
  
end module stellarator_variables

module pulse_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the pulsed reactor model
  !!
  !!### References
  !!
  !! - Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: bctmp = 320.0D0
  !! first wall bulk coolant temperature (C)

  real(dp) :: bfw = 0.0D0
  !! outer radius of each first wall structural tube (m) (0.5 * average of fwith and fwoth)

  real(dp) :: dtstor = 300.0D0
  !! maximum allowable temperature change in stainless steel thermal storage block (K) (`istore=3`)

  integer :: istore = 1
  !! Switch for thermal storage method:
  !!
  !! - =1 option 1 of Electrowatt report, AEA FUS 205
  !! - =2 option 2 of Electrowatt report, AEA FUS 205
  !! - =3 stainless steel block

  integer :: itcycl = 1
  !! Switch for first wall axial stress model:
  !!
  !! - =1 total axial constraint, no bending
  !! - =2 no axial constraint, no bending
  !! - =3 no axial constraint, bending

  integer :: lpulse = 0
  !! Switch for reactor model:
  !!
  !! - =0 continuous operation
  !! - =1 pulsed operation

end module pulse_variables

module startup_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the plasma start-up model
  !!
  !!### References
  !!
  !! - Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: ftaue = 0.0D0
  !! factor in energy confinement time formula

  real(dp) :: gtaue  = 0.0D0
  !! offset term in energy confinement time scaling

  real(dp) :: nign  = 0.0D0
  !! electron density at ignition (start-up) (/m3)

  real(dp) :: ptaue  = 0.0D0
  !! exponent for density term in energy confinement time formula

  real(dp) :: qtaue  = 0.0D0
  !! exponent for temperature term in energy confinement time formula

  real(dp) :: rtaue  = 0.0D0
  !! exponent for power term in energy confinement time formula

  real(dp) :: tign  = 0.0D0
  !! electron temperature at ignition (start-up) (keV)

end module startup_variables

module fispact_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the fispact routines
  !!
  !!### References
  !!
  !! -AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  !! Fispact arrays with 3 elements contain the results at the following times:
  !!
  !! - (1) - at end of component life
  !! - (2) - after 3 months cooling time
  !! - (3) - 100 years after end of plant life

  real(dp), dimension(3) :: bliact = 0.0D0
  !! inboard blanket total activity (Bq)

  real(dp), dimension(3) :: bligdr = 0.0D0
  !! inboard blanket total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: blihkw = 0.0D0
  !! inboard blanket total heat output (kW)

  real(dp) :: bliizp = 0.0D0
  !! inboard blanket integrated zone power / neutron

  real(dp) :: blimzp = 0.0D0
  !! inboard blanket mean zone power density / neutron

  real(dp), dimension(3) :: bloact = 0.0D0
  !! outboard blanket total activity (Bq)

  real(dp), dimension(3) :: blogdr = 0.0D0
  !! outboard blanket total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: blohkw = 0.0D0
  !! outboard blanket total heat output (kW)

  real(dp) :: bloizp = 0.0D0
  !! outboard blanket integrated zone power / neutron

  real(dp) :: blomzp = 0.0D0
  !! outboard blanket mean zone power density / neutron

  real(dp), dimension(3) :: fwiact = 0.0D0
  !! inboard first wall total activity (Bq)

  real(dp), dimension(3) :: fwigdr = 0.0D0
  !! inboard first wall total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: fwihkw = 0.0D0
  !! inboard first wall total heat output (kW)

  real(dp) :: fwiizp = 0.0D0
  !! inboard first wall integrated zone power / neutron

  real(dp) :: fwimzp = 0.0D0
  !! inboard first wall mean zone power density/neutron

  real(dp), dimension(3) :: fwoact = 0.0D0
  !! outboard first wall total activity (Bq)

  real(dp), dimension(3) :: fwogdr = 0.0D0
  !! outboard first wall total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: fwohkw = 0.0D0
  !! outboard first wall total heat output (kW)

  real(dp) :: fwoizp = 0.0D0
  !! outboard first wall integrated zone power / neutron

  real(dp) :: fwomzp = 0.0D0
  !! outboard first wall mean zone power density/neutron

  real(dp) :: fwtemp = 0.0D0
  !! outboard first wall temperature after a LOCA (K)

end module fispact_variables

module rebco_variables
  !! author: M. Kovari
  !! 
  !! Module for the REBCO HTS superconductor variables
  !!
  !! Variables relating to the REBCO HTS tape, strand and conductor
  !! Conduit information is in the modules relating to each coil.
  !!
  !!### References
  !!
  !! - Updated 13/11/18 using data from Lewandowska et al 2018.

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  real(dp) :: rebco_thickness = 1.0D-6
  !! thickness of REBCO layer in tape (m) (`iteration variable 138`)

  real(dp) :: copper_thick = 100.0D-6
  !! thickness of copper layer in tape (m) (`iteration variable 139`)

  real(dp) :: hastelloy_thickness = 50.0D-6
  !! thickness of Hastelloy layer in tape (m)

  real(dp) :: tape_width = 0.0D0
  !! Mean width of tape (m)
  
  real(dp) :: croco_od = 0.0D0
  !! Outer diameter of CroCo strand (m)

  real(dp) :: croco_id = 0.0D0
  !! Inner diameter of CroCo copper tube (m)

  real(dp) :: croco_thick = 2.5D-3
  !! Thickness of CroCo copper tube (m) (`iteration variable 158`)
  
  real(dp) :: copper_rrr = 100d0
  !! residual resistivity ratio copper in TF superconducting cable

  real(dp) :: coppera_m2_max = 1D8
  !! Maximum TF coil current / copper area (A/m2)

  real(dp) :: f_coppera_m2 = 1d0
  !! f-value for constraint 75: TF coil current / copper area < copperA_m2_max

  !#TODO: variables need descriptions and units
  real(dp) :: tape_thickness
  real(dp) :: stack_thickness
  real(dp) :: tapes
  real(dp) :: rebco_area
  real(dp) :: copper_area
  real(dp) :: hastelloy_area
  real(dp) :: solder_area
  real(dp) :: croco_area
  real(dp) :: copperA_m2       
  !! TF coil current / copper area (A/m2) 

end module rebco_variables

module resistive_materials
  !! author: M. Kovari
  !!  
  !! Variables relating to resistive materials in superconducting conductors
  
  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  type resistive_material
    real(dp) :: cp            
    !! Specific heat capacity J/(K kg)

    real(dp) :: rrr           
    !! Residual resistivity ratio

    real(dp) :: resistivity   
    !! Resistivity [ohm.m]

    real(dp) :: density       
    !! kg/m3

    real(dp) :: cp_density    
    !! Cp x density J/K/m3
  end type resistive_material

  type supercon_strand
     real(dp) :: area
     !! Superconducting strand area [m2]

     real(dp) :: critical_current
     !! Superconducting strand critical current [A]
  end type supercon_strand

  !#TODO: variables need descriptions
  type volume_fractions
    real(dp) :: copper_area,  copper_fraction
    real(dp) :: copper_bar_area
    real(dp) :: hastelloy_area, hastelloy_fraction
    real(dp) :: helium_area, helium_fraction
    real(dp) :: solder_area, solder_fraction
    real(dp) :: jacket_area, jacket_fraction
    real(dp) :: rebco_area,  rebco_fraction
    real(dp) :: critical_current
    real(dp) :: acs                  
    !! Area of cable space inside jacket
    real(dp) :: area
  end type volume_fractions
end module resistive_materials

module reinke_variables
  !! author: S. Muldrew (UKAEA)
  !!
  !! This module contains global variables relating to the minimum impurity fraction 
  !! for detached divertor conditions Reinke criterion. It furthermore uses 
  !! several parameters from Kallenbach model like netau and empurity_enrichment.
  !!
  !!### References
  !!
  !! - M.L. Reinke 2017 Nucl. Fusion 57 034004

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer       :: impvardiv = 9
  !! Index of impurity to be iterated for Reinke divertor detachment criterion

  real(dp) :: lhat = 4.33D0
  !! Connection length factor L|| = lhat qstar R for Reinke criterion, default value from
  !! Post et al. 1995 J. Nucl. Mat.  220-2 1014

  real(dp) :: fzmin = 0.0D0
  !! Minimum impurity fraction necessary for detachment. This is the impurity at the SOL/Div.

  real(dp) :: fzactual = 0.001D0
  !! Actual impurity fraction of divertor impurity (impvardiv) in the SoL (taking 
  !! impurity_enrichment into account) (`iteration variable 148`)

  integer       :: reinke_mode = 0
  !! Switch for Reinke criterion H/I mode:
  !!
  !! - =0 H-mode
  !! - =1 I-mode

end module reinke_variables