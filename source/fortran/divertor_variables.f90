module divertor_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the tokamak divertor components
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: adas = 0.0D0
  !! area divertor / area main plasma (along separatrix)

  real(dp) :: anginc = 0.262D0
  !! angle of incidence of field line on plate (rad)

  real(dp) :: betai = 1.0D0
  !! poloidal plane angle between divertor plate and leg, inboard (rad)

  real(dp) :: betao = 1.0D0
  !! poloidal plane angle between divertor plate and leg, outboard (rad)

  real(dp) :: bpsout = 0.60D0
  !! reference B_p at outboard divertor strike point (T)

  real(dp) :: c1div = 0.45D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c2div = -7.0D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c3div = 0.54D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c4div = -3.6D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c5div = 0.7D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c6div = 0.0D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: delld = 1.0D0
  !! coeff for power distribution along main plasma

  real(dp) :: dendiv = 0.0D0
  !! plasma density at divertor (10**20 /m3)

  real(dp) :: densin = 0.0D0
  !! density at plate (on separatrix) (10**20 /m3)

  real(dp) :: divclfr = 0.3D0
  !! divertor coolant fraction

  real(dp) :: divdens = 1.0D4
  !! divertor structure density (kg/m3)

  integer :: divdum = 0
  !! switch for divertor Zeff model:
  !!
  !! - =0 calc
  !! - =1 input
  !#TODO: switch name should be changed to i_<something>

  real(dp) :: divfix = 0.2D0
  !! divertor structure vertical thickness (m)

  real(dp) :: divmas = 0.0D0
  !! divertor plate mass (kg)

  real(dp) :: divplt = 0.035D0
  !! divertor plate thickness (m) (from Spears, Sept 1990)

  real(dp) :: divsur = 0.0D0
  !! divertor surface area (m2)

  real(dp) :: fdfs = 10.0D0
  !! radial gradient ratio

  real(dp) :: fdiva = 1.11D0
  !! divertor area fudge factor (for ITER, Sept 1990)

  real(dp) :: fgamp = 1.0D0
  !! sheath potential factor (not used)

  real(dp) :: fhout = 0.0D0
  !! fraction of power to outboard divertor (for single null)

  real(dp) :: fififi = 4.0D-3
  !! coefficient for gamdiv
  !#TODO: what the hell is this variable name...

  real(dp) :: frrp = 0.4D0
  !! fraction of radiated power to plate

  real(dp) :: hldiv = 0.0D0
  !! divertor heat load (MW/m2)

  real(dp) :: hldivlim = 5.0D0
  !! heat load limit (MW/m2)

  real(dp) :: ksic = 0.8D0
  !! power fraction for outboard double-null scrape-off plasma

  real(dp) :: lamp = 0.0D0
  !! power flow width (m)

  real(dp) :: minstang = 0.0D0
  !! minimum strike angle for heat flux calculation

  real(dp) :: omegan = 1.0D0
  !! pressure ratio (nT)_plasma / (nT)_scrape-off

  real(dp) :: omlarg = 0.0D0
  !! power spillage to private flux factor

  real(dp) :: ppdivr = 0.0D0
  !! peak heat load at plate (with radiation) (MW/m2)

  real(dp) :: prn1 = 0.285D0
  !! n-scrape-off / n-average plasma; (input for `ipedestal=0`, = nesep/dene if `ipedestal>=1`)
  
  real(dp) :: ptpdiv = 0.0D0
  !! peak temperature at the plate (eV)

  real(dp) :: rconl = 0.0D0
  !! connection length ratio, outboard side

  real(dp) :: rlclolcn = 0.0D0
  !! ratio of collision length / connection length

  real(dp) :: rlenmax = 0.5D0
  !! maximum value for length ratio (rlclolcn) (`constraintg eqn 22`)

  real(dp) :: rsrd = 0.0D0
  !! effective separatrix/divertor radius ratio

  real(dp) :: tconl = 0.0D0
  !! main plasma connection length (m)

  real(dp) :: tdiv = 2.0D0
  !! temperature at divertor (eV) (input for stellarator only, calculated for tokamaks)

  real(dp) :: tsep = 0.0D0
  !! temperature at the separatrix (eV)

  real(dp) :: xparain = 2.1D3
  !! parallel heat transport coefficient (m2/s)

  real(dp) :: xpertin = 2.0D0
  !! perpendicular heat transport coefficient (m2/s)

  real(dp) :: zeffdiv = 1.0D0
  !! Zeff in the divertor region (if `divdum /= 0`)
  
end module divertor_variables