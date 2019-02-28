! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_star_module

  !+ad_name  costs_star_module
  !+ad_summ  Module containing STAR fusion power plant costing algorithms
  !+ad_type  Module
  !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
  !+ad_cont  costs
  !+ad_cont  coelc
  !+ad_cont  acc21
  !+ad_cont  acc22
  !+ad_cont  acc221
  !+ad_cont  acc2211
  !+ad_cont  acc2212
  !+ad_cont  acc2213
  !+ad_cont  acc2214
  !+ad_cont  acc2215
  !+ad_cont  acc222
  !+ad_cont  acc2221
  !+ad_cont  acc2222
  !+ad_cont  acc2221
  !+ad_cont  acc2223
  !+ad_cont  acc223
  !+ad_cont  acc224
  !+ad_cont  acc225
  !+ad_cont  acc2251
  !+ad_cont  acc2252
  !+ad_cont  acc2253
  !+ad_cont  acc226
  !+ad_cont  acc227
  !+ad_cont  acc228
  !+ad_cont  acc229
  !+ad_cont  acc23
  !+ad_cont  acc24
  !+ad_cont  acc25
  !+ad_cont  acc26
  !+ad_cont  acchyd
  !+ad_cont  acc9
  !+ad_args  N/A
  !+ad_desc  This module contains the STAR fusion power plant costing model,
  !+ad_desc  developed by Nizar Ben Ayed, Stuart Muldrew and Tim Hender, based
  !+ad_desc  on the STARFIRE costing framework.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  constants
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  pulse_variables

  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_variables
  !+ad_hist  28/02/19 SIM Initial version of module
  !+ad_stat  Okay
  !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use constants
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables

  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output
  use pulse_variables

  use structure_variables
  use tfcoil_variables
  use times_variables
  use vacuum_variables

  implicit none

  private
  public :: costs_star

  !  Various cost account values (M$)

  real(kind(1.0D0)) :: ccont,cindrt

  real(kind(1.0D0)) :: star20, star21, star23, star24

  real(kind(1.0D0)) :: vfi,vfi_star,ptherm_star

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine costs_star(outfile,iprint)

    !+ad_name  costs_star
    !+ad_summ  STAR cost accounting for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the cost accounting for a fusion power plant.
    !+ad_desc  The direct costs are calculated based on parameters input
    !+ad_desc  from other sections of the code.
    !+ad_desc  <P>The code is arranged in the order of the standard accounts.
    !+ad_prob  None
    !+ad_call  star_a21
    !+ad_call  acc22
    !+ad_call  acc23
    !+ad_call  acc24
    !+ad_call  acc25
    !+ad_call  acc26
    !+ad_call  acchyd
    !+ad_call  acc9
    !+ad_call  coelc
    !+ad_call  oblnkl
    !+ad_call  ocosts
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  28/02/19 SIM Initial version
    !+ad_stat  Okay
    !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Fusion Island Volume as defined by Sheffield et al. (1986)

    vfi = pi * (rtot + 0.5D0*tfthko)**2 * (hpfu + hmax + tfcth)

    !  STARFIRE Reference Value

    vfi_star = 5.1D3
    ptherm_star = 4.15D3

    if (iprint==1) then
    call oheadr(outfile,'STAR Costing Model (1983 US$)')
    end if
    
    !  Account 20 : Land and Rights

    call star_a20(outfile,iprint)

    !  Account 21 : Building and Site Service Infrastructure

    call star_a21(outfile,iprint)

    !  Account 23 : Turbine Plant Equipment

    call star_a23(outfile,iprint)

    !  Account 24 : Electric Plant Equipment

    call star_a24(outfile,iprint)

    !  Total plant direct cost

    cdirt = star20 + star21 + star23

    !  Indirect costs

    cindrt = cfind(lsa) * cdirt * (1.0D0 + cowner)

    !  Contingency costs

    ccont = fcontng * (cdirt + cindrt)

    !  Constructed cost

    concost = cdirt + cindrt + ccont

    if (iprint == 1) then
    call oshead(outfile,'Plant Direct Cost')
    call ocosts(outfile,'(cdirt)','Plant direct cost (M$)',cdirt)

    call oshead(outfile,'Indirect Cost')
    call ocosts(outfile,'(c9)','Indirect cost (M$)',cindrt)

    call oshead(outfile,'Total Contingency')
    call ocosts(outfile,'(ccont)','Total contingency (M$)',ccont)

    call oshead(outfile,'Constructed Cost')
    call ocosts(outfile,'(concost)','Constructed cost (M$)',concost)

    if (ireactor == 1) then
       call oshead(outfile,'Interest during Construction')
       call ocosts(outfile,'(moneyint)','Interest during construction (M$)',moneyint)

       call oshead(outfile,'Total Capital Investment')
       call ocosts(outfile,'(capcost)','Total capital investment (M$)',capcost)
    end if
   end if

    !  Cost of electricity

    ! if ((ireactor == 1).and.(ipnet == 0)) call coelc(outfile,iprint)

  end subroutine costs_star


  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine star_a20(outfile,iprint)

   !+ad_name  star_a20
   !+ad_summ  Account 20 : Land and Rights
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 20 (Land and Rights)
   !+ad_desc  costs.
   !+ad_prob  None
   !+ad_call  None
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: star2001, star2002

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   star20 = 0.0D0
   
   ! 21.01 Land
   ! Original STARFIRE value, no scaling
   star2001 = 3.0D0
   star20 = star20 + star2001

   ! 21.02 Site Preparation
   ! Original STARFIRE value, no scaling
   star2002 = 3.0D-1
   star20 = star20 + star2002

   if (iprint == 1) then
   call oshead(outfile,'20. Land and Rights')
   call ocosts(outfile,'(star2001)','Land (M$)', star2001)
   call ocosts(outfile,'(star2002)','Site Preparation (M$)', star2002)
   call oblnkl(outfile)
   call ocosts(outfile,'(star20)','Total account 20 cost (M$)', star20)
   end if

 end subroutine star_a20

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine star_a21(outfile,iprint)

   !+ad_name  star_a21
   !+ad_summ  Account 21 : Building and Site Service Infrastructure
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 21 (Building and Site
   !+ad_desc  Service Infrastructure) costs.
   !+ad_prob  None
   !+ad_call  None
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: &
   star2101, star2102, star2103, star2104, star2105, star2106, &
   star2107, star2108, star2109, star2110, star2111, star2112, &
   star2113, star2114, star2115, star2116, star2117, star2198, &
   star2199

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   star21 = 0.0D0
   
   ! 21.01 Site Improvements
   ! Original STARFIRE value, no scaling
   star2101 = 1.115D1
   star21 = star21 + star2101

   ! 21.02 Reactor Building
   ! Original STARFIRE value, scaling with fusion island volume
   star2102 = 1.5744D2 * (vfi / vfi_star)**(2.0D0 / 3.0D0)
   star21 = star21 + star2102

   ! 21.03 Turbine Building
   ! Original STARFIRE value, scaling with thermal power
   star2103 = 3.592D1 * (pthermmw / ptherm_star)
   star21 = star21 + star2103

   ! 21.04 Cooling System Structures
   ! Original STARFIRE value, scaling with thermal power
   star2104 = 7.96D0 * (pthermmw / ptherm_star)
   star21 = star21 + star2104

   ! 21.05 Electrical Equipment and Power Supply Building
   ! Original STARFIRE value, scaling with thermal power
   star2105 = 9.16D0 * (pthermmw / ptherm_star)
   star21 = star21 + star2105

   ! 21.06 Auxiliary Services Building
   ! Original STARFIRE value, scaling with thermal power
   star2106 = 3.26D0 * (pthermmw / ptherm_star)  
   star21 = star21 + star2106

   ! 21.07 Hot Cell
   ! Original STARFIRE value, scaling with fusion island volume
   star2107 = 5.369D1 * (vfi / vfi_star)**(2.0D0/3.0D0)
   star21 = star21 + star2107

   ! 21.08 Reactor Service Building
   ! Original STARFIRE value, scaling with fusion island volume
   star2108 = 1.88D0 * (vfi / vfi_star)**(2.0D0/3.0D0)
   star21 = star21 + star2108

   ! 21.09 Service Water Building
   ! Original STARFIRE value, scaling with thermal power
   star2109 = 6.6D-1 * (pthermmw / ptherm_star)  
   star21 = star21 + star2109

   ! 21.10 Fuel Handling and Storage Building
   ! Original STARFIRE value, scaling with thermal power
   star2110 = 8.63D0 * (pthermmw / ptherm_star)  
   star21 = star21 + star2110

   ! 21.11 Control Room
   ! Original STARFIRE value, no scaling
   star2111 = 3.1D0 
   star21 = star21 + star2111

   ! 21.12 AC Power Supply Building
   ! Original STARFIRE value, no scaling
   star2112 = 2.05D0 
   star21 = star21 + star2112

   ! 21.13 Admin Building
   ! Original STARFIRE value, no scaling
   star2113 = 8.7D-1
   star21 = star21 + star2113

   ! 21.14 Site Service
   ! Original STARFIRE value, scaling with thermal power
   star2114 = 8.7D-1 * (pthermmw / ptherm_star)  
   star21 = star21 + star2114

   ! 21.15 Cryogenics and Inert Gas Storage Building
   ! Original STARFIRE value, no scaling
   star2115 = 9.1D-1
   star21 = star21 + star2115

   ! 21.16 Security Building
   ! Original STARFIRE value, no scaling
   star2116 = 3.1D-1
   star21 = star21 + star2116

   ! 21.17 Ventilation Stack
   ! Original STARFIRE value, scaling with thermal power
   star2117 = 1.81D0 * (pthermmw / ptherm_star)  
   star21 = star21 + star2117

   ! 21.98 Spares
   ! Original STARFIRE value, no scaling
   star2198 = 1.96D0 
   star21 = star21 + star2198

   ! 21.99 Contingency
   ! STARFIRE 15%
   star2199 = 1.5D-1 * star21
   star21 = star21 + star2199

   if (iprint == 1) then
   call oshead(outfile,'21. Building and Site Service Infrastructure')
   call ocosts(outfile,'(star2101)','Site Improvements (M$)', star2101)
   call ocosts(outfile,'(star2102)','Reactor Building (M$)', star2102)
   call ocosts(outfile,'(star2103)','Turbine Building (M$)', star2103)
   call ocosts(outfile,'(star2104)','Cooling System Structures (M$)', star2104)
   call ocosts(outfile,'(star2105)','Electrical Equipment and Power Supply Building (M$)', star2105)
   call ocosts(outfile,'(star2106)','Auxiliary Services Building (M$)', star2106)
   call ocosts(outfile,'(star2107)','Hot Cell (M$)', star2107)
   call ocosts(outfile,'(star2108)','Reactor Service Building (M$)', star2108)
   call ocosts(outfile,'(star2109)','Service Water Building (M$)', star2109)
   call ocosts(outfile,'(star2110)','Fuel Handling and Storage Building (M$)', star2110)
   call ocosts(outfile,'(star2111)','Control Room (M$)', star2111)
   call ocosts(outfile,'(star2112)','AC Power Supply Building (M$)', star2112)
   call ocosts(outfile,'(star2113)','Admin Building (M$)', star2113)
   call ocosts(outfile,'(star2114)','Site Service (M$)', star2114)
   call ocosts(outfile,'(star2115)','Cryogenics and Inert Gas Storage Building (M$)', star2115)
   call ocosts(outfile,'(star2116)','Security Building (M$)', star2116)
   call ocosts(outfile,'(star2117)','Ventilation Stack (M$)', star2117)
   call ocosts(outfile,'(star2198)','Spares (M$)', star2198)
   call ocosts(outfile,'(star2199)','Contingency (M$)', star2199)
   call oblnkl(outfile)
   call ocosts(outfile,'(star21)','Total account 21 cost (M$)', star21)
   end if

 end subroutine star_a21


   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine star_a23(outfile,iprint)

   !+ad_name  star_a23
   !+ad_summ  Account 23 : Turbine Plant Equipment
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 23 (Turbine Plant Equipment)
   !+ad_desc  costs.
   !+ad_prob  None
   !+ad_call  None
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: &
   star2301, star2302, star2303, star2304, star2305, star2306, &
   star2307, star2398, star2399

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   star23 = 0.0D0
   
   ! 23.01 Turbine Generators
   ! Original STARFIRE value, scaling with thermal power
   star2301 = 7.733D1 * (pthermmw / ptherm_star)
   star23 = star23 + star2301

   ! 23.02 Steam System
   ! Original STARFIRE value, scaling with thermal power
   star2302 = 4.37D0 * (pthermmw / ptherm_star)
   star23 = star23 + star2302

   ! 23.03 Heat Rejection
   ! Original STARFIRE value, scaling with thermal power
   star2303 = 4.434D1 * (pthermmw / ptherm_star)
   star23 = star23 + star2303

   ! 23.04 Condensing System
   ! Original STARFIRE value, scaling with thermal power
   star2304 = 1.918D1 * (pthermmw / ptherm_star)
   star23 = star23 + star2304

   ! 23.05 Feedwater Heating System
   ! Original STARFIRE value, scaling with thermal power
   star2305 = 9.39D0 * (pthermmw / ptherm_star)
   star23 = star23 + star2305

   ! 23.06 Other Turbine Equipment
   ! Original STARFIRE value, scaling with thermal power
   star2306 = 5.084D1 * (pthermmw / ptherm_star)  
   star23 = star23 + star2306

   ! 23.07 Instrumentation and Control
   ! Original STARFIRE value, no scaling
   star2307 = 8.7D0
   star23 = star23 + star2307

   ! 23.98 Spares
   ! Original STARFIRE value, no scaling
   star2398 = 3.0D0 
   star23 = star23 + star2398

   ! 23.99 Contingency
   ! STARFIRE 15%
   star2399 = 1.5D-1 * star23
   star23 = star23 + star2399

   if (iprint == 1) then
   call oshead(outfile,'23. Turbine Plant Equipment')
   call ocosts(outfile,'(star2301)','Turbine Generators (M$)', star2301)
   call ocosts(outfile,'(star2302)','Steam System (M$)', star2302)
   call ocosts(outfile,'(star2303)','Heat Rejection (M$)', star2303)
   call ocosts(outfile,'(star2304)','Condensing System (M$)', star2304)
   call ocosts(outfile,'(star2305)','Feedwater Heating System (M$)', star2305)
   call ocosts(outfile,'(star2306)','Other Turbine Equipment (M$)', star2306)
   call ocosts(outfile,'(star2307)','Instrumentation and Control (M$)', star2307)
   call ocosts(outfile,'(star2398)','Spares (M$)', star2398)
   call ocosts(outfile,'(star2399)','Contingency (M$)', star2399)
   call oblnkl(outfile)
   call ocosts(outfile,'(star23)','Total account 23 cost (M$)', star23)
   end if

 end subroutine star_a23

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine star_a24(outfile,iprint)

   !+ad_name  star_a24
   !+ad_summ  Account 24 : Electric Plant Equipment
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 24 (Electric Plant 
   !+ad_desc  Equipment) costs.
   !+ad_prob  None
   !+ad_call  None
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: &
   star2401, star2402, star2403, star2404, star2405, star2406, &
   star2407, star2498, star2499

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   star24 = 0.0D0
   
   ! 24.01 Switch Gear
   ! Original STARFIRE value, no scaling
   star2401 = 1.239D1
   star24 = star24 + star2401

   ! 24.02 Station Service Equipment
   ! Original STARFIRE value, no scaling
   star2402 = 1.704D1
   star24 = star24 + star2402

   ! 24.03 Switchboards
   ! Original STARFIRE value, no scaling
   star2403 = 7.8D0
   star24 = star24 + star2403

   ! 24.04 Protective Equipment
   ! Original STARFIRE value, no scaling
   star2404 = 2.11D0
   star24 = star24 + star2404

   ! 24.05 Electrical Structures
   ! Original STARFIRE value, no scaling
   star2405 = 1.74D1
   star24 = star24 + star2405

   ! 24.06 Power and Control Wiring
   ! Original STARFIRE value, no scaling
   star2406 = 3.599D1
   star24 = star24 + star2406

   ! 24.07 Electric Lighting
   ! Original STARFIRE value, no scaling
   star2407 = 8.2D0
   star24 = star24 + star2407

   ! 24.98 Spares
   ! Original STARFIRE value, no scaling
   star2498 = 1.05D0 
   star24 = star24 + star2498

   ! 24.99 Contingency
   ! STARFIRE 15%
   star2499 = 1.5D-1 * star24
   star24 = star24 + star2499

   if (iprint == 1) then
   call oshead(outfile,'24. Electric Plant Equipment')
   call ocosts(outfile,'(star2401)','Switch Gear (M$)', star2401)
   call ocosts(outfile,'(star2402)','Station Service Equipment (M$)', star2402)
   call ocosts(outfile,'(star2403)','Switchboards (M$)', star2403)
   call ocosts(outfile,'(star2404)','Protective Equipment (M$)', star2404)
   call ocosts(outfile,'(star2405)','Electrical Structures (M$)', star2405)
   call ocosts(outfile,'(star2406)','Power and Control Wiring (M$)', star2406)
   call ocosts(outfile,'(star2407)','Electric Lighting (M$)', star2407)
   call ocosts(outfile,'(star2498)','Spares (M$)', star2498)
   call ocosts(outfile,'(star2499)','Contingency (M$)', star2499)
   call oblnkl(outfile)
   call ocosts(outfile,'(star24)','Total account 23 cost (M$)', star24)
   end if

 end subroutine star_a24


  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine coelc(outfile,iprint)

    !+ad_name  coelc
    !+ad_summ  Routine to calculate the cost of electricity for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the calculation of the cost of electricity
    !+ad_desc  for a fusion power plant.
    !+ad_desc  <P>Annual costs are in megadollars/year, electricity costs are in
    !+ad_desc  millidollars/kWh, while other costs are in megadollars.
    !+ad_desc  All values are based on 1990 dollars.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_call  osubhd
    !+ad_call  ovarrf
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  11/09/13 PJK Modified annfuel cost calculation
    !+ad_hist  17/02/14 PJK Modified output format for some quantities
    !+ad_hist  15/05/14 PJK Longer output line lengths
    !+ad_hist  05/06/14 PJK Moved some power outputs to plant_power.f90
    !+ad_hist  16/06/14 PJK Removed duplicate outputs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  12/11/14 PJK tburn factor incorporated into cost of electricity
    !+ad_hist  17/11/14 PJK Added output_costs switch
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
         annfuelt,annfwbl,annoam,anntot,annwst,coecdr, &
         coecp,coedecom,coediv,coefuel,coefwbl,coewst,crfcdr,crfcp, &
         crfdiv,crffwbl,fefcdr,fefcp,fefdiv,feffwbl,fwbllife,kwhpy
         ! annoam1,
         
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Number of kWh generated each year

    kwhpy = 1.0D3 * pnetelmw * (24.0D0*365.0D0) * cfactr * tburn/tcycle

    !  Costs due to reactor plant
    !  ==========================

    !  Interest on construction costs

    moneyint = concost * (fcap0 - 1.0D0)

    !  Capital costs

    capcost = concost + moneyint

    !  Annual cost of plant capital cost

    anncap = capcost * fcr0

    !  Cost of electricity due to plant capital cost

    coecap = 1.0D9 * anncap / kwhpy

    !  Costs due to first wall and blanket renewal
    !  ===========================================

    !  Operational life

    fwbllife = bktlife

    !  Compound interest factor

    feffwbl = (1.0D0 + ratecdol)**fwbllife

    !  Capital recovery factor

    crffwbl = (feffwbl*ratecdol) / (feffwbl-1.0D0)

    !  Annual cost of replacements

    annfwbl = (fwallcst + blkcst) * &
         (1.0D0+cfind(lsa)) * fcap0cp * crffwbl



    !  Cost of electricity due to first wall/blanket replacements

    coefwbl = 1.0D9 * annfwbl / kwhpy

    !  Costs due to divertor renewal
    !  =============================

       !  Compound interest factor

       fefdiv = (1.0D0 + ratecdol)**divlife

       !  Capital recovery factor

       crfdiv = (fefdiv*ratecdol) / (fefdiv-1.0D0)

       !  Annual cost of replacements

       anndiv = divcst * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfdiv

       !  Cost of electricity due to divertor replacements

       coediv = 1.0D9 * anndiv / kwhpy

    !  Costs due to centrepost renewal
    !  ===============================

    if (itart == 1) then

       !  Compound interest factor

       fefcp = (1.0D0 + ratecdol)**cplife

       !  Capital recovery factor

       crfcp = (fefcp*ratecdol) / (fefcp-1.0D0)

       !  Annual cost of replacements

       anncp = cpstcst * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfcp

       !  Cost of electricity due to centrepost replacements

       coecp = 1.0D9 * anncp / kwhpy

    else
       anncp = 0.0D0
       coecp = 0.0D0
    end if

    !  Costs due to partial current drive system renewal
    !  =================================================

    !  Compound interest factor

    fefcdr = (1.0D0 + ratecdol)**cdrlife

    !  Capital recovery factor

    crfcdr = (fefcdr*ratecdol) / (fefcdr-1.0D0)

    !  Annual cost of replacements

    if (ifueltyp == 0) then
       anncdr = 0.0D0
    else
       anncdr = cdcost * fcdfuel/(1.0D0-fcdfuel) * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfcdr
    end if

    !  Cost of electricity due to current drive system replacements

    coecdr = 1.0D9 * anncdr / kwhpy

    !  Costs due to operation and maintenance
    !  ======================================

    !  Annual cost of operation and maintenance

    annoam = ucoam(lsa) * sqrt(pnetelmw/1200.0D0)

    !  Additional cost due to pulsed reactor thermal storage
    !  See F/MPE/MOD/CAG/PROCESS/PULSE/0008
    !
    !      if (lpulse.eq.1) then
    !         if (istore.eq.1) then
    !            annoam1 = 51.0D0
    !         else if (istore.eq.2) then
    !            annoam1 = 22.2D0
    !         else
    !            continue
    !         end if
    !
    !  Scale with net electric power
    !
    !         annoam1 = annoam1 * pnetelmw/1200.0D0
    !
    !  It is necessary to convert from 1992 pounds to 1990 dollars
    !  Reasonable guess for the exchange rate + inflation factor
    !  inflation = 5% per annum; exchange rate = 1.5 dollars per pound
    !
    !         annoam1 = annoam1 * 1.36D0
    !
    !         annoam = annoam + annoam1
    !
    !      end if

    !  Cost of electricity due to operation and maintenance

    coeoam = 1.0D9 * annoam / kwhpy

    !  Costs due to reactor fuel
    !  =========================

    !  Annual cost of fuel

       !  Sum D-T fuel cost and He3 fuel cost
       annfuel = ucfuel * pnetelmw/1200.0D0 + &
            1.0D-6 * fhe3 * wtgpd * 1.0D-3 * uche3 * 365.0D0 * cfactr

    !  Cost of electricity due to reactor fuel

    coefuel = 1.0D9 * annfuel / kwhpy

    !  Costs due to waste disposal
    !  ===========================

    !  Annual cost of waste disposal

    annwst = ucwst(lsa) * sqrt(pnetelmw/1200.0D0)

    !  Cost of electricity due to waste disposal

    coewst = 1.0D9 * annwst / kwhpy

    !  Costs due to decommissioning fund
    !  =================================

    !  Annual contributions to fund for decommissioning
    !  A fraction decomf of the construction cost is set aside for
    !  this purpose at the start of the plant life.
    !  Final factor takes into account inflation over the plant lifetime
    !  (suggested by Tim Hender 07/03/96)
    !  Difference (dintrt) between borrowing and saving interest rates is
    !  included, along with the possibility of completing the fund dtlife
    !  years before the end of the plant's lifetime

    anndecom = decomf * concost * fcr0 / &
         (1.0D0+ratecdol-dintrt)**(tlife-dtlife)

    !  Cost of electricity due to decommissioning fund

    coedecom = 1.0D9 * anndecom / kwhpy

    !  Total costs
    !  ===========

    !  Annual costs due to 'fuel-like' components

    annfuelt = annfwbl + anndiv + anncdr + anncp + annfuel + annwst

    !  Total cost of electricity due to 'fuel-like' components

    coefuelt = coefwbl + coediv + coecdr + coecp + coefuel + coewst

    !  Total annual costs

    anntot = anncap + annfuelt + annoam + anndecom

    !  Total cost of electricity

    coe = coecap + coefuelt + coeoam + coedecom

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section

    call oheadr(outfile,'Power Reactor Costs (1990 US$)')

    call ovarrf(outfile,'First wall / blanket life (years)','(fwbllife)', &
         fwbllife)


       call ovarrf(outfile,'Divertor life (years)','(divlife.)',divlife)
       if (itart == 1) then
          call ovarrf(outfile,'Centrepost life (years)','(cplife.)',cplife)
       end if


    call ovarrf(outfile,'Cost of electricity (m$/kWh)','(coe)',coe)

    call osubhd(outfile,'Power Generation Costs :')

    if ((annfwbl /= annfwbl).or.(annfwbl > 1.0D10).or.(annfwbl < 0.0D0)) then
        write(outfile,*)'Problem with annfwbl'
        write(outfile,*)'fwallcst=', fwallcst, '  blkcst=', blkcst
        write(outfile,*)'crffwbl=', crffwbl,   '  fcap0cp=', fcap0cp
        write(outfile,*)'feffwbl=', feffwbl,   '  fwbllife=', fwbllife
    end if

    write(outfile,200) &
         anncap,coecap, &
         annoam,coeoam, &
         anndecom,coedecom, &
         annfwbl,coefwbl, &
         anndiv,coediv, &
         anncp,coecp, &
         anncdr,coecdr, &
         annfuel,coefuel, &
         annwst,coewst, &
         annfuelt,coefuelt, &
         anntot,coe

200 format( &
         t76,'Annual Costs, M$       COE, m$/kWh'// &
         1x,'Capital Investment',t80,f10.2,10x,f10.2/ &
         1x,'Operation & Maintenance',t80,f10.2,10x,f10.2/ &
         1x,'Decommissioning Fund',t80,f10.2,10x,f10.2/ &
         1x,'Fuel Charge Breakdown'// &
         5x,'Blanket & first wall',t72,f10.2,10x,f10.2/ &
         5x,'Divertors',t72,f10.2,10x,f10.2/ &
         5x,'Centrepost (TART only)',t72,f10.2,10x,f10.2/ &
         5x,'Auxiliary Heating',t72,f10.2,10x,f10.2/ &
         5x,'Actual Fuel',t72,f10.2,10x,f10.2/ &
         5x,'Waste Disposal',t72,f10.2,10x,f10.2/ &
         1x,'Total Fuel Cost',t80,f10.2,10x,f10.2// &
         1x,'Total Cost',t80,f10.2,10x,f10.2 )

    if (ifueltyp == 1) then
       call oshead(outfile,'Replaceable Components Direct Capital Cost')
       call ovarrf(outfile,'First wall direct capital cost (M$)', &
            '(fwallcst)',fwallcst)
       call ovarrf(outfile,'Blanket direct capital cost (M$)', &
            '(blkcst)',blkcst)

          call ovarrf(outfile,'Divertor direct capital cost (M$)', &
               '(divcst)',divcst)
          if (itart == 1) then
             call ovarrf(outfile,'Centrepost direct capital cost (M$)', &
                  '(cpstcst)',cpstcst)
          end if
          call ovarrf(outfile,'Plasma heating/CD system cap cost (M$)', &
               '',cdcost*fcdfuel/(1.0D0-fcdfuel))
          call ovarrf(outfile,'Fraction of CD cost --> fuel cost', &
               '(fcdfuel)',fcdfuel)

    end if

  end subroutine coelc

end module costs_star_module
