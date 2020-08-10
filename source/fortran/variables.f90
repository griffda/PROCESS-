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