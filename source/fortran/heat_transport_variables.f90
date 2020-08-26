module heat_transport_variables
    !! author: J. Morris, M. Kovari (UKAEA)
    !!
    !! This module contains global variables relating to the heat transport system 
    !! of a fusion power plant, and also those for a hydrogen production plant.
    !!
    !!### References 
    !!
    !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code
  
    use, intrinsic :: iso_fortran_env, only: dp=>real64
  
    implicit none
  
    public
  
    real(dp) :: baseel = 5.0D6
    !! base plant electric load (W)
  
    real(dp) :: crypmw = 0.0D0
    !! cryogenic plant power (MW)
  
    real(dp) :: etatf = 0.9D0
    !! AC to resistive power conversion for TF coils
  
    real(dp) :: etath = 0.35D0
    !! thermal to electric conversion efficiency if `secondary_cycle=2`; otherwise calculated.
  
    real(dp) :: fachtmw = 0.0D0
    !! facility heat removal (MW)
  
    real(dp) :: fcsht = 0.0D0
    !! total baseline power required at all times (MW)
  
    real(dp) :: fgrosbop = 0.0D0
    !! scaled fraction of gross power to balance-of-plant
  
    real(dp) :: fmgdmw = 0.0D0
    !! power to mgf (motor-generator flywheel) units (MW) (ignored if `iscenr=2`)
  
    real(dp) :: fpumpblkt = 0.005D0
    !! fraction of total blanket thermal power required to drive the blanket 
    !! coolant pumps (default assumes water coolant) (`secondary_cycle=0`)
  
    real(dp) :: fpumpdiv = 0.005D0
    !! fraction of total divertor thermal power required to drive the divertor 
    !! coolant pumps (default assumes water coolant)
  
    real(dp) :: fpumpfw = 0.005D0
    !! fraction of total first wall thermal power required to drive the FW coolant 
    !! pumps (default assumes water coolant) (`secondary_cycle=0`)
  
    real(dp) :: fpumpshld = 0.005D0
    !! fraction of total shield thermal power required to drive the shield coolant 
    !! pumps (default assumes water coolant)
  
    real(dp) :: htpmw_min = 0.0D0
    !! Minimum total electrical power for primary coolant pumps (MW) (NOT RECOMMENDED)
  
    real(dp) :: helpow = 0.0D0
    !! heat removal at cryogenic temperatures (W)
  
    real(dp) :: htpmw = 0.0D0
    !! heat transport system electrical pump power (MW)
  
    real(dp) :: htpmw_blkt = 0.0D0
    !! blanket coolant mechanical pumping power (MW)
  
    real(dp) :: htpmw_div = 0.0D0
    !! divertor coolant mechanical pumping power (MW)
  
    real(dp) :: htpmw_fw = 0.0D0
    !! first wall coolant mechanical pumping power (MW)
  
    real(dp) :: htpmw_shld = 0.0D0
    !! shield and vacuum vessel coolant mechanical pumping power (MW)
  
    real(dp) :: htpsecmw = 0.0D0
    !! Waste power lost from primary coolant pumps (MW)
  
    integer :: ipowerflow = 1
    !! switch for power flow model:
    !!
    !! - =0 pre-2014 version
    !! - =1 comprehensive 2014 model
  
    integer :: iprimnloss = 0
    !! switch for lost neutron power through holes destiny (ipowerflow=0):
    !!
    !! - =0 does not contribute to energy generation cycle
    !! - =1 contributes to energy generation cycle
  
    integer :: iprimshld = 1
    !! Switch for shield thermal power destiny:
    !!
    !! - =0 does not contribute to energy generation cycle
    !! - =1 contributes to energy generation cycle
  
    integer :: nphx = 0
    !! number of primary heat exchangers
  
    real(dp) :: pacpmw = 0.0D0
    !! total pulsed power system load (MW)
  
    real(dp) :: peakmva = 0.0D0
    !! peak MVA requirement
  
    real(dp) :: pfwdiv = 0.0D0
    !! heat removal from first wall/divertor (MW)
  
    real(dp) :: pgrossmw = 0.0D0
    !! gross electric power (MW)
  
    real(dp) :: pinjht = 0.0D0
    !! power dissipated in heating and current drive system (MW)
  
    real(dp) :: pinjmax = 120.0D0
    !! maximum injector power during pulse (heating and ramp-up/down phase) (MW)
  
    real(dp) :: pinjwp = 0.0D0
    !! injector wall plug power (MW)
  
    real(dp) :: pinjwpfix = 0.0D0
    !! secondary injector wall plug power (MW)
  
    real(dp) :: pnetelmw = 0.0D0
    !! net electric power (MW)
  
    real(dp) :: precircmw = 0.0D0
    !! recirculating electric power (MW)
  
    real(dp) :: priheat = 0.0D0
    !! total thermal power removed from fusion core (MW)
  
    real(dp) :: psecdiv = 0.0D0
    !! Low-grade heat lost in divertor (MW)
  
    real(dp) :: psechcd = 0.0D0
    !! Low-grade heat lost into HCD apparatus (MW)
  
    real(dp) :: psechtmw = 0.0D0
    !! Low-grade heat (MW)
  
    real(dp) :: pseclossmw = 0.0D0
    !! Low-grade heat (VV + lost)(MW)
  
    real(dp) :: psecshld = 0.0D0
    !! Low-grade heat deposited in shield (MW)
  
    real(dp) :: pthermmw = 0.0D0
    !! High-grade heat useful for electric production (MW)
  
    real(dp) :: pwpm2 = 150.0D0
    !! base AC power requirement per unit floor area (W/m2)
  
    real(dp) :: tfacpd = 0.0D0
    !! total steady state TF coil AC power demand (MW)
  
    real(dp) :: tlvpmw = 0.0D0
    !! estimate of total low voltage power (MW)
  
    real(dp) :: trithtmw = 15.0D0
    !! power required for tritium processing (MW)
  
    real(dp) :: tturb = 0.0D0
    !! coolant temperature at turbine inlet (K) (`secondary_cycle = 3,4`)
  
    real(dp) :: vachtmw = 0.5D0
    !! vacuum pump power (MW)
  
  end module heat_transport_variables