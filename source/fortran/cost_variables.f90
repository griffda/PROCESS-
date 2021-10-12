module cost_variables
  !! author: J. Morris, S. Muldrew, M. Kovari (UKAEA)
  !!
  !! Module containing global variables relating to the costing algorithms of a fusion power plant.
  !!
  !!### References 
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(8) :: abktflnc
  !! allowable first wall/blanket neutron fluence (MW-yr/m2) (`blktmodel=0`)

  real(8) :: adivflnc
  !! allowable divertor heat fluence (MW-yr/m2)

  real(8) :: blkcst
  !! blanket direct cost (M$)

  real(8) :: c221
  !! total account 221 cost (M$) - first wall, blanket, shield, support structure and div plates

  real(8) :: c222
  !! total account 222 cost (M$) - TF coils + PF coils

  real(8) :: capcost
  !! total capital cost including interest (M$)

  real(8) :: cconfix
  !! fixed cost of superconducting cable ($/m)

  real(8) :: cconshpf
  !! cost of PF coil steel conduit/sheath ($/m)

  real(8) :: cconshtf
  !! cost of TF coil steel conduit/sheath ($/m)

  real(8) :: cdcost
  !! current drive direct costs (M$)

  real(8) :: cdirt
  !! total plant direct cost (M$)

  real(8) :: cdrlife
  !! lifetime of heating/current drive system (y)

  real(8) :: cfactr
  !! Total plant availability fraction; input if `iavail=0`

  real(8) :: cpfact
  !! Total plant capacity factor
  
  real(8), dimension(4) :: cfind
  !! indirect cost factor (func of lsa) (cost model = 0)

  real(8) :: cland
  !! cost of land (M$)

  real(8) :: coe
  !! cost of electricity ($/MW-hr)

  real(8) :: coecap
  !! capital cost of electricity (m$/kW-hr)

  real(8) :: coefuelt
  !! 'fuel' (including replaceable components) contribution to cost of electricity (m$/kW-hr)

  real(8) :: coeoam
  !! operation and maintenance contribution to cost of electricity (m$/kW-hr)

  real(8) :: concost
  !! plant construction cost (M$)

  real(8) :: costexp
  !! cost exponent for scaling in 2015 costs model

  real(8) :: costexp_pebbles
  !! cost exponent for pebbles in 2015 costs model

  real(8) :: cost_factor_buildings
  !! cost scaling factor for buildings

  real(8) :: cost_factor_land
  !! cost scaling factor for land

  real(8) :: cost_factor_tf_coils
  !! cost scaling factor for TF coils

  real(8) :: cost_factor_fwbs
  !! cost scaling factor for fwbs

  real(8) :: cost_factor_rh
  !! cost scaling factor for remote handling

  real(8) :: cost_factor_vv
  !! cost scaling factor for vacuum vessel

  real(8) :: cost_factor_bop
  !! cost scaling factor for energy conversion system

  real(8) :: cost_factor_misc
  !! cost scaling factor for remaining subsystems

  real(8) :: maintenance_fwbs
  !! Maintenance cost factor: first wall, blanket, shield, divertor

  real(8) :: maintenance_gen
  !! Maintenance cost factor: All other components except coils, vacuum vessel, 
  !! thermal shield, cryostat, land

  real(8) :: amortization
  !! amortization factor (fixed charge factor) "A" (years)

  integer :: cost_model
  !! Switch for cost model:
  !!
  !! - =0 use $ 1990 PROCESS model
  !! - =1 use $ 2014 Kovari model
  !! - =2 use $ 1980 STEP model (NOT RECOMMENDED - Under Development)

  integer :: i_cp_lifetime
  !! Switch for the centrepost lifetime constraint 
  !!  0 : The CP full power year lifetime is set by the user
  !!  1 : The CP lifetime is equal to the divertor lifetime
  !!  2 : The CP lifetime is equal to the breeding blankets lifetime
  !!  3 : The CP lifetime is equal to the plant lifetime

  real(8) :: cowner
  !! owner cost factor

  real(8) :: cplife_input
  !! User input full power year lifetime of the centrepost (years)

  real(8) :: cplife
  !! Calculated full power year lifetime of centrepost (years)

  real(8) :: cpstcst
  !! ST centrepost direct cost (M$)

  real(8) :: cpstflnc
  !! allowable ST centrepost neutron fluence (MW-yr/m2)

  real(8) :: crctcore
  !! reactor core costs (categories 221, 222 and 223)

  real(8) :: csi
  !! allowance for site costs (M$)

  real(8) :: cturbb
  !! cost of turbine building (M$)

  real(8) :: decomf
  !! proportion of constructed cost required for decommissioning fund

  real(8) :: dintrt
  !! diff between borrowing and saving interest rates

  real(8) :: divcst
  !! divertor direct cost (M$)

  real(8) :: divlife
  !! Full power lifetime of divertor (y)

  real(8) :: dtlife
  !! period prior to the end of the plant life that the decommissioning fund is used (years)

  real(8) :: fcap0
  !! average cost of money for construction of plant assuming design/construction time of six years

  real(8) :: fcap0cp
  !! average cost of money for replaceable components assuming lead time for these of two years

  real(8) :: fcdfuel
  !! fraction of current drive cost treated as fuel (if `ifueltyp = 1`)

  real(8) :: fcontng
  !! project contingency factor

  real(8) :: fcr0
  !! fixed charge rate during construction

  real(8) :: fkind
  !! multiplier for Nth of a kind costs

  real(8) :: fwallcst
  !! first wall cost (M$)

  integer :: iavail
  !! Switch for plant availability model:
  !!
  !! - =0 use input value for cfactr
  !! - =1 calculate cfactr using Taylor and Ward 1999 model
  !! - =2 calculate cfactr using new (2015) model

  real(8) :: avail_min
  !! Minimum availability (`constraint equation 61`)

  real(8) :: tok_build_cost_per_vol
  !! Unit cost for tokamak complex buildings, including building and site services ($/m3)

  real(8) :: light_build_cost_per_vol
  !! Unit cost for unshielded non-active buildings ($/m3)

  real(8) :: favail
  !! F-value for minimum availability (`constraint equation 61`)

  integer :: num_rh_systems
  !! Number of remote handling systems (1-10)

  real(8) :: conf_mag
  !! c parameter, which determines the temperature margin at which magnet lifetime starts to decline

  real(8) :: div_prob_fail
  !! Divertor probability of failure (per op day)

  real(8) :: div_umain_time
  !! Divertor unplanned maintenance time (years)

  real(8) :: div_nref
  !! Reference value for cycle cycle life of divertor

  real(8) :: div_nu
  !! The cycle when the divertor fails with 100% probability

  real(8) :: fwbs_nref
  !! Reference value for cycle life of blanket

  real(8) :: fwbs_nu
  !! The cycle when the blanket fails with 100% probability

  real(8) :: fwbs_prob_fail
  !! Fwbs probability of failure (per op day)

  real(8) :: fwbs_umain_time
  !! Fwbs unplanned maintenance time (years)

  real(8) :: redun_vacp
  !! Vacuum system pump redundancy level (%)

  integer :: redun_vac
  !! Number of redundant vacuum pumps

  real(8) :: t_operation
  !! Operational time (yrs)

  real(8) :: tbktrepl
  !! time taken to replace blanket (y) (`iavail=1`)

  real(8) :: tcomrepl
  !! time taken to replace both blanket and divertor (y) (`iavail=1`)

  real(8) :: tdivrepl
  !! time taken to replace divertor (y) (`iavail=1`)

  real(8) :: uubop
  !! unplanned unavailability factor for balance of plant (`iavail=1`)

  real(8) :: uucd
  !! unplanned unavailability factor for current drive (`iavail=1`)

  real(8) :: uudiv
  !! unplanned unavailability factor for divertor (`iavail=1`)

  real(8) :: uufuel
  !! unplanned unavailability factor for fuel system (`iavail=1`)

  real(8) :: uufw
  !! unplanned unavailability factor for first wall (`iavail=1`)

  real(8) :: uumag
  !! unplanned unavailability factor for magnets (`iavail=1`)

  real(8) :: uuves
  !! unplanned unavailability factor for vessel (`iavail=1`)

  integer :: ifueltyp
  !! Switch for fuel type:
  !!
  !! - =2 treat initial blanket, divertor, first wall
  !!   as capital costs. Treat all later items and 
  !!   fraction fcdfuel of CD equipment as fuel costs
  !! - =1 treat blanket divertor, first wall and
  !!   fraction fcdfuel of CD equipment as fuel cost
  !! - =0 treat these as capital cost

  integer :: ipnet
  !! Switch for net electric power calculation:
  !!
  !! - =0 scale so that always > 0
  !! - =1 let go < 0 (no c-o-e)

  integer :: ireactor
  !! Switch for net electric power and cost of electricity calculations:
  !!
  !! - =0 do not calculate MW(electric) or c-o-e
  !! - =1 calculate MW(electric) and c-o-e

  integer :: lsa
  !! Level of safety assurance switch (generally, use 3 or 4):
  !!
  !! - =1 truly passively safe plant
  !! - =2,3 in-between
  !! - =4 like current fission plant

  real(8) :: moneyint
  !! interest portion of capital cost (M$)

  integer :: output_costs
  !! Switch for costs output:
  !!
  !! - =0 do not write cost-related outputs to file
  !! - =1 write cost-related outputs to file

  real(8) :: discount_rate
  !! effective cost of money in constant dollars

  real(8) :: sitecost
  !! fixed value for site cost (2017 US$)

  real(8), dimension(62) :: site_imp_uc
  !! Unit costs for Site Improvement activities (cost model 2)

  real(8) :: step_con
  !! Contingency Percentage

  real(8) :: step_cconfix 
  !! fixed cost of superconducting cable ($/m) (if cost model = 2) 

  real(8) :: step_cconshpf
  !! cost of PF coil steel conduit/sheath ($/m) (if cost model = 2) 

  character(len=50) :: step_currency
  !! description of the constant dollar year used

  real(8) :: step_ucblbe
  !! unit cost for blanket Be ($/kg) (if cost model = 2)

  real(8) :: step_ucblbreed 
  !! unit cost for blanket breeder material ($/kg) (if cost model = 2) 

  real(8) :: step_ucblss
  !! unit cost for blanket stainless steel ($/kg) (if cost model = 2)

  real(8) :: step_ucblvd 
  !! Unit cost for blanket Vd ($/kg) (if cost model = 2)

  real(8) :: step_uccase
  !! cost of superconductor case ($/kg) (if cost model = 2)

  real(8) :: step_uccu
  !! unit cost for copper in superconducting cable ($/kg) (if cost model = 2)

  real(8) :: step_ucfwa 
  !! first wall armour cost ($/kg) (if cost model = 2)

  real(8) :: step_ucfws 
  !! first wall structure cost ($/kg) (if cost model = 2)

  real(8) :: step_ucfwps 
  !! first wall passive stabiliser cost ($) (if cost model = 2)

  real(8) :: step_ucshw
  !! unit cost for shield tungsten ($/kg) (if cost model = 2)

  real(8) :: step_ucshwc
  !! unit cost for shield tungsten carbide ($/kg) (if cost model = 2)

  real(8) :: step_ucoam
  !! annual cost of operation and maintenance (M$/year/1200MW**0.5)

  real(8) :: step_ucwst
  !! cost of waste disposal (M$/y/1200MW)

  real(8), dimension(8) :: step_ucsc
  !! cost of superconductor ($/kg) (if cost model = 2)

  real(8) :: step_ucfnc
  !! outer PF coil fence support cost ($/kg) (if cost model = 2)

  real(8) :: step_ucwindpf 
  !! cost of PF coil superconductor windings ($/m) (if cost model = 2)

  real(8) :: step_rh_costfrac
  !! fraction of capital cost for remote handling (if cost_model = 2)
  
  real(8), dimension(68) :: step_ref
  !! Reference values for cost model 2

  real(8) :: step91_per
  !! Percentage of cdirt used in calculating step91 (3.0D-1 = 30%)
  
  real(8) :: step92_per
  !! Percentage of cdirt used in calculating step92 (3.0D-1 = 30%)
  
  real(8) :: step93_per
  !! Percentage of cdirt used in calculating step93 (3.0D-1 = 30%)

  real(8) :: tlife
  !! Full power year plant lifetime (years)

  real(8), parameter :: ucad = 180.0D0
  !! unit cost for administration buildings (M$/m3)

  real(8), parameter :: ucaf = 1.5D6
  !! unit cost for aux facility power equipment ($)

  real(8), parameter :: ucahts = 31.0D0
  !! unit cost for aux heat transport equipment ($/W**exphts)

  real(8), parameter :: ucap = 17.0D0
  !! unit cost of auxiliary transformer ($/kVA)

  real(8) :: ucblbe
  !! unit cost for blanket beryllium ($/kg)

  real(8) :: ucblbreed
  !! unit cost for breeder material ($/kg) (`blktmodel>0`)

  real(8) :: ucblli
  !! unit cost for blanket lithium ($/kg) (30% Li6)

  real(8) :: ucblli2o
  !! unit cost for blanket Li_2O ($/kg)

  real(8) :: ucbllipb
  !! unit cost for blanket Li-Pb ($/kg) (30% Li6)

  real(8) :: ucblss
  !! unit cost for blanket stainless steel ($/kg)

  real(8) :: ucblvd
  !! unit cost for blanket vanadium ($/kg)

  real(8), parameter :: ucbpmp = 2.925D5
  !! vacuum system backing pump cost ($)

  real(8) :: ucbus
  !! cost of aluminium bus for TF coil ($/A-m)

  real(8) :: uccase
  !! cost of superconductor case ($/kg)

  real(8), parameter :: ucco = 350.0D0
  !! unit cost for control buildings (M$/m3)

  real(8) :: uccpcl1
  !! cost of high strength tapered copper ($/kg)

  real(8) :: uccpclb
  !! cost of TF outboard leg plate coils ($/kg)

  real(8), parameter :: uccpmp = 3.9D5
  !! vacuum system cryopump cost ($)

  real(8), parameter :: uccr = 460.0D0
  !! unit cost for cryogenic building (M$/vol)

  real(8) :: uccry
  !! heat transport system cryoplant costs ($/W**expcry)

  real(8) :: uccryo
  !! unit cost for vacuum vessel ($/kg)

  real(8) :: uccu
  !! unit cost for copper in superconducting cable ($/kg)

  real(8), parameter :: ucdgen = 1.7D6
  !! cost per 8 MW diesel generator ($)

  real(8) :: ucdiv
  !! cost of divertor blade ($)

  real(8), parameter :: ucdtc = 13.0D0
  !! detritiation, air cleanup cost ($/10000m3/hr)

  real(8), parameter :: ucduct = 4.225D4
  !! vacuum system duct cost ($/m)

  real(8) :: ucech
  !! ECH system cost ($/W)

  real(8), parameter :: ucel = 380.0D0
  !! unit cost for electrical equipment building (M$/m3)

  real(8), parameter :: uces1 = 3.2D4
  !! MGF (motor-generator flywheel) cost factor ($/MVA**0.8)
  
  real(8), parameter :: uces2 = 8.8D3
  !! MGF (motor-generator flywheel) cost factor ($/MJ**0.8)

  real(8) :: ucf1
  !! cost of fuelling system ($)

  real(8) :: ucfnc
  !! outer PF coil fence support cost ($/kg)

  real(8), parameter :: ucfpr = 4.4D7
  !! cost of 60g/day tritium processing unit ($)

  real(8) :: ucfuel
  !! unit cost of D-T fuel (M$/year/1200MW)

  real(8), parameter :: ucfwa = 6.0D4
  !! first wall armour cost ($/m2)

  real(8), parameter :: ucfwps = 1.0D7
  !! first wall passive stabiliser cost ($)

  real(8), parameter :: ucfws = 5.3D4
  !! first wall structure cost ($/m2)
  
  real(8), parameter :: ucgss = 35.0D0
  !! cost of reactor structure ($/kg)

  real(8) :: uche3
  !! cost of helium-3 ($/kg)

  real(8) :: uchrs
  !! cost of heat rejection system ($)

  real(8), dimension(2) :: uchts
  !! cost of heat transport system equipment per loop ($/W); dependent on coolant type (coolwh)

  real(8) :: uciac
  !! cost of instrumentation, control & diagnostics ($)

  real(8) :: ucich
  !! ICH system cost ($/W)

  real(8), parameter :: ucint = 35.0D0
  !! superconductor intercoil structure cost ($/kg)

  real(8) :: uclh
  !! lower hybrid system cost ($/W)

  real(8), parameter :: uclv = 16.0D0
  !! low voltage system cost ($/kVA)

  real(8), parameter :: ucmb = 260.0D0
  !! unit cost for reactor maintenance building (M$/m3)

  real(8) :: ucme
  !! cost of maintenance equipment ($)

  real(8) :: ucmisc
  !! miscellaneous plant allowance ($)

  real(8) :: ucnbi
  !! NBI system cost ($/W)

  real(8), parameter :: ucnbv = 1000.0D0
  !! cost of nuclear building ventilation ($/m3)

  real(8), dimension(4) :: ucoam
  !! annual cost of operation and maintenance (M$/year/1200MW**0.5)

  real(8) :: ucpens
  !! penetration shield cost ($/kg)

  real(8) :: ucpfb
  !! cost of PF coil buses ($/kA-m)

  real(8) :: ucpfbk
  !! cost of PF coil DC breakers ($/MVA**0.7)

  real(8) :: ucpfbs
  !! cost of PF burn power supplies ($/kW**0.7)

  real(8) :: ucpfcb
  !! cost of PF coil AC breakers ($/circuit)

  real(8) :: ucpfdr1
  !! cost factor for dump resistors ($/MJ)

  real(8) :: ucpfic
  !! cost of PF instrumentation and control ($/channel)

  real(8) :: ucpfps
  !! cost of PF coil pulsed power supplies ($/MVA)

  real(8), parameter :: ucphx = 15.0D0
  !! primary heat transport cost ($/W**exphts)

  real(8), parameter :: ucpp = 48.0D0
  !! cost of primary power transformers ($/kVA**0.9)

  real(8) :: ucrb
  !! cost of reactor building (M$/m3)

  real(8), dimension(8) :: ucsc
  !! cost of superconductor ($/kg)

  real(8) :: step_uc_cryo_al
  !! Unit cost of cryo aluminium ($/kg). Only used in costs_step_module

  real(8) :: step_mc_cryo_al_per
  !! Manufacturing cost percentage for cryo aluminium (%). 0.2 means a 20%
  !! manufacturing cost. Only used in costs_step_module

  real(8), parameter :: ucsh = 115.0D0
  !! cost of shops and warehouses (M$/m3)

  real(8) :: ucshld
  !! cost of shield structural steel ($/kg)

  real(8), parameter :: ucswyd = 1.84D7
  !! switchyard equipment costs ($)

  real(8) :: uctfbr
  !! cost of TF coil breakers ($/W**0.7)

  real(8) :: uctfbus
  !! cost of TF coil bus ($/kg)

  real(8), parameter :: uctfdr = 1.75D-4
  !! cost of TF coil dump resistors ($/J)

  real(8), parameter :: uctfgr = 5000.0D0
  !! additional cost of TF coil dump resistors ($/coil)

  real(8), parameter :: uctfic = 1.0D4
  !! cost of TF coil instrumentation and control ($/coil/30)

  real(8) :: uctfps
  !! cost of TF coil power supplies ($/W**0.7)

  real(8) :: uctfsw
  !! cost of TF coil slow dump switches ($/A)

  real(8), parameter :: uctpmp = 1.105D5
  !! cost of turbomolecular pump ($)

  real(8), parameter :: uctr = 370.0D0
  !! cost of tritium building ($/m3)

  real(8), dimension(2) :: ucturb
  !! cost of turbine plant equipment ($) (dependent on coolant type coolwh)

  real(8), parameter :: ucvalv = 3.9D5
  !! vacuum system valve cost ($)

  real(8), parameter :: ucvdsh = 26.0D0
  !! vacuum duct shield cost ($/kg)

  real(8), parameter :: ucviac = 1.3D6
  !! vacuum system instrumentation and control cost ($)

  real(8) :: ucwindpf
  !! cost of PF coil superconductor windings ($/m)

  real(8) :: ucwindtf
  !! cost of TF coil superconductor windings ($/m)

  real(8), parameter :: ucws = 460.0D0
  !! cost of active assembly shop ($/m3)

  real(8), dimension(4) :: ucwst
  !! cost of waste disposal (M$/y/1200MW)

  real(8) :: wfbuilding
  !! fixed value for waste facility buildings (2017 US$)

  real(dp) :: whole_site_area
  !! area of entire plant site (m2); default 1.0E6 m2 (i.e. 1 km2)
  
  contains

  subroutine init_cost_variables
    !! Initialise cost variables
    implicit none

    abktflnc = 5.0D0
    adivflnc = 7.0D0
    blkcst = 0.0D0
    c221 = 0.0D0
    c222 = 0.0D0
    capcost = 0.0D0
    cconfix = 80.0D0
    cconshpf = 70.0D0
    cconshtf = 75.0D0
    cdcost = 0.0D0
    cdirt = 0.0D0
    cdrlife = 0.0D0
    cfactr = 0.75D0
    cpfact = 0.0D0
    cfind = (/0.244D0, 0.244D0, 0.244D0, 0.29D0/)
    cland = 19.2D0
    coe = 0.0D0
    coecap = 0.0D0
    coefuelt = 0.0D0
    coeoam = 0.0D0
    concost = 0.0D0
    costexp = 0.8D0
    costexp_pebbles = 0.6D0
    cost_factor_buildings = 1.0D0
    cost_factor_land = 1.0D0
    cost_factor_tf_coils = 1.0D0
    cost_factor_fwbs = 1.0D0
    cost_factor_rh = 1.0D0
    cost_factor_vv = 1.0D0
    cost_factor_bop = 1.0D0
    cost_factor_misc = 1.0D0
    maintenance_fwbs = 0.2D0
    maintenance_gen = 0.05D0
    amortization = 13.6D0
    cost_model = 1
    cowner = 0.15D0
    cplife = 0.0D0
    cpstcst = 0.0D0
    cpstflnc = 10.0D0
    crctcore = 0.0D0
    csi = 16.0D0
    cturbb = 38.0D0
    decomf = 0.1D0
    dintrt = 0.0D0
    divcst = 0.0D0
    divlife = 0.0D0
    dtlife = 0.0D0
    fcap0 = 1.165D0
    fcap0cp = 1.08D0
    fcdfuel = 0.1D0
    fcontng = 0.195D0
    fcr0 = 0.0966D0
    fkind = 1.0D0
    fwallcst = 0.0D0
    iavail= 2
    avail_min = 0.75D0
    tok_build_cost_per_vol = 1283.0D0
    light_build_cost_per_vol = 270.0D0
    favail = 1.0D0
    num_rh_systems = 4
    conf_mag = 0.99D0
    div_prob_fail = 0.0002D0
    div_umain_time = 0.25D0
    div_nref = 7000.0D0
    div_nu = 14000.0D0
    fwbs_nref = 20000.0D0
    fwbs_nu = 40000.0D0
    fwbs_prob_fail = 0.0002D0
    fwbs_umain_time = 0.25D0
    redun_vacp = 25.0D0
    redun_vac = 0
    t_operation = 0.0D0
    tbktrepl = 0.5D0
    tcomrepl = 0.5D0
    tdivrepl = 0.25D0
    uubop = 0.02D0
    uucd = 0.02D0
    uudiv = 0.04D0
    uufuel = 0.02D0
    uufw = 0.04D0
    uumag = 0.02D0
    uuves = 0.04D0
    ifueltyp = 0
    ipnet = 0
    ireactor = 1
    lsa = 4
    moneyint = 0.0D0
    output_costs = 1
    discount_rate = 0.0435D0
    site_imp_uc = &
      (/ 6.0D0, 24.0D0, 60.0D0, 43.0D0, 14.915863D6, 20.882208D6, 671.214D3, 2.0D0, 12.0D0, &
      400.938D3, 12.0D0, 38.0D0, 8.0D0, 5.0D0, 5.0D0, 2.0D0, 37.0D0, 4.0D0, 210.618026D6, &
      1.283D3, 1.090D3, 6.156922D6, 6.285191D6, 1.410961D6, 3.848076D6, 38.0D0, 105.110266D6, &
      4.758017D6, 11.932690D6, 20.285573D6, 131.259593D6, 21.478843D6, 8.949518D6, &
      19.688939D6, 323.375907D6, 27.445188D6, 17.899035D6, 751.759D3, 2.231294D6, 1.746230D6, &
      18.137689D6, 15.448657D6, 5.773513D6, 11.345602D6, 961.357D3, 536.971D3, 1.933096D6, &
      238.236D3, 553.796D3, 85.199D3, 55.845D3, 1.774988D6, 97.737325D6, 44.111064D6, &
      409.009163D6, 156.641807D6, 143.192283D6, 59.663451D6, 26.550236D6, 181.496219D6, &
      93.074984D6, 77.562487D6/)
    step_con = 1.5D-1
    step_cconfix = 217.0D0  
    step_cconshpf = 91.0D0
    step_currency = "2017 US$"
    step_ucblbe = 8400.4D0
    step_ucblbreed = 802.2D0 
    step_ucblss = 488.3D0 
    step_ucblvd = 200.0D0 
    step_uccase = 0.0D0
    step_uccu = 82.0D0
    step_ucfwa = 774.05D0
    step_ucfws = 5115.7D0 
    step_ucfwps = 0.0D0
    step_ucshw = 269.638D0
    step_ucshwc = 930.251D0
    step_ucsc = (/ 600.0D0, 600.0D0, 443.0D0, 600.0D0, 600.0D0, 600.0D0,300.0D0,1200.0D0 /)
    step_ucfnc = 104.3D0 
    step_ucoam = 74.4D0
    step_ucwst = 7.88D0
    step_ucwindpf = 465.0D0
    step_rh_costfrac = 7.5D-2
    step_ref = &
      (/ 8.92D0, 8.9D-1, 3.317D1, 7.4491D2, 1.0685D2, 2.368D1, 2.725D1, 9.7D0, 2.5403D2, &
      8.9D0, 1.96D0, 4.083D1, 1.467D1, 6.1D0, 2.59D0, 2.59D0, 2.71D0, 9.2D-1, 8.56D0, &
      6.7025D2, 1.51424D3, 1.02302D3, 2.8154D2, 5.9D1, 3.254D1, 2.7254D2, 4.292D2, 3.955D1, &
      4.305D2, 1.994D1, 2.295D1, 1.364D2, 5.6836D2, 3.643D1, 1.703D1, 1.325D1, 3.79D0, 1.383D1, &
      1.465D1, 1.058D1, 3.1413D2, 0.0D0, 0.0D0, 1.95D0, 6.5D-2, 0.0D0, 1.628D1, 1.603D1, 9.44D0, &
      1.9051D2, 1.9585D2, 1.107D1, 1.319D2, 4.858D1, 2.793D1, 1.2876D2, 2.588D1, 3.01D1, &
      4.14D1, 1.895D1, 5.13D0, 4.228D1, 8.744D1, 1.992D1, 4.664D1, 3.674D1, 1.85D1, 2.23D0 /)
    step91_per = 3.0D-1
    step92_per = 3.25D-1
    step93_per = 1.5D-1
    tlife = 30.0D0
    ucblbe = 260.0D0
    ucblbreed = 875.0D0
    ucblli = 875.0D0
    ucblli2o = 600.0D0
    ucbllipb = 10.3D0
    ucblss = 90.0D0
    ucblvd = 200.0D0
    ucbus = 0.123D0
    uccase = 50.0D0
    uccpcl1 = 250.0D0
    uccpclb = 150.0D0
    uccry = 9.3D4
    uccryo = 32.0D0
    uccu = 75.0D0
    ucdiv = 2.8D5
    ucech = 3.0D0
    ucf1 = 2.23D7
    ucfnc = 35.0D0
    ucfuel = 3.45D0
    uche3 = 1.0D6
    uchrs = 87.9D6
    uchts = (/15.3D0, 19.1D0/)
    uciac = 1.5D8
    ucich = 3.0D0
    uclh = 3.3D0
    ucme = 1.25D8
    ucmisc = 2.5D7
    ucnbi = 3.3D0
    ucoam = (/68.8D0, 68.8D0, 68.8D0, 74.4D0/)
    ucpens = 32.0D0
    ucpfb = 210.0D0
    ucpfbk = 1.66D4
    ucpfbs = 4.9D3
    ucpfcb = 7.5D4
    ucpfdr1 = 150.0D0
    ucpfic = 1.0D4
    ucpfps = 3.5D4
    ucrb = 400.0D0
    ucsc = &
      (/600.0D0, 600.0D0, 300.0D0, 600.0D0, 600.0D0, 600.0D0,300.0D0,1200.0D0/)
    ucshld = 32.0D0
    uctfbr = 1.22D0
    uctfbus = 100.0D0
    uctfps = 24.0D0
    uctfsw = 1.0D0
    ucturb = (/230.0D6, 245.0D6/)
    ucwindpf = 465.0D0
    ucwindtf = 480.0D0
    ucwst = (/0.0D0, 3.94D0, 5.91D0, 7.88D0/)
    i_cp_lifetime = 0
    cplife_input = 2.0D0
    step_uc_cryo_al = 8.1D1
    step_mc_cryo_al_per = 2.0D-1
    sitecost = 1.0D8
    wfbuilding = 1.0D8
    whole_site_area = 1.0D6

  end subroutine init_cost_variables
end module cost_variables