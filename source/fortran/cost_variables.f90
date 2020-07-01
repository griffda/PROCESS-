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

  real(dp) :: abktflnc = 5.0D0
  !! allowable first wall/blanket neutron fluence (MW-yr/m2) (`blktmodel=0`)

  real(dp) :: adivflnc = 7.0D0
  !! allowable divertor heat fluence (MW-yr/m2)

  real(dp) :: blkcst = 0.0D0
  !! blanket direct cost (M$)

  real(dp) :: c221 = 0.0D0
  !! total account 221 cost (M$) - first wall, blanket, shield, support structure and div plates

  real(dp) :: c222 = 0.0D0
  !! total account 222 cost (M$) - TF coils + PF coils

  real(dp) :: capcost = 0.0D0
  !! total capital cost including interest (M$)

  real(dp) :: cconfix = 80.0D0
  !! fixed cost of superconducting cable ($/m)

  real(dp) :: cconshpf = 70.0D0
  !! cost of PF coil steel conduit/sheath ($/m)

  real(dp) :: cconshtf = 75.0D0
  !! cost of TF coil steel conduit/sheath ($/m)

  real(dp) :: cdcost = 0.0D0
  !! current drive direct costs (M$)

  real(dp) :: cdirt = 0.0D0
  !! total plant direct cost (M$)

  real(dp) :: cdrlife = 0.0D0
  !! lifetime of heating/current drive system (y)

  real(dp) :: cfactr = 0.75D0
  !! Total plant availability fraction; input if `iavail=0`

  real(dp) :: cpfact = 0.0D0
  !! Total plant capacity factor
  
  real(dp), dimension(4) :: cfind = (/0.244D0, 0.244D0, 0.244D0, 0.29D0/)
  !! indirect cost factor (func of lsa)

  real(dp) :: cland = 19.2D0
  !! cost of land (M$)

  real(dp) :: coe = 0.0D0
  !! cost of electricity ($/MW-hr)

  real(dp) :: coecap = 0.0D0
  !! capital cost of electricity (m$/kW-hr)

  real(dp) :: coefuelt = 0.0D0
  !! 'fuel' (including replaceable components) contribution to cost of electricity (m$/kW-hr)

  real(dp) :: coeoam = 0.0D0
  !! operation and maintenance contribution to cost of electricity (m$/kW-hr)

  real(dp) :: concost = 0.0D0
  !! plant construction cost (M$)

  real(dp) :: costexp = 0.8D0
  !! cost exponent for scaling in 2015 costs model

  real(dp) :: costexp_pebbles = 0.6D0
  !! cost exponent for pebbles in 2015 costs model

  real(dp) :: cost_factor_buildings = 1.0D0
  !! cost scaling factor for buildings

  real(dp) :: cost_factor_land = 1.0D0
  !! cost scaling factor for land

  real(dp) :: cost_factor_tf_coils = 1.0D0
  !! cost scaling factor for TF coils

  real(dp) :: cost_factor_fwbs = 1.0D0
  !! cost scaling factor for fwbs

  real(dp) :: cost_factor_rh = 1.0D0
  !! cost scaling factor for remote handling

  real(dp) :: cost_factor_vv = 1.0D0
  !! cost scaling factor for vacuum vessel

  real(dp) :: cost_factor_bop = 1.0D0
  !! cost scaling factor for energy conversion system

  real(dp) :: cost_factor_misc = 1.0D0
  !! cost scaling factor for remaining subsystems

  real(dp) :: maintenance_fwbs = 0.2D0
  !! Maintenance cost factor: first wall, blanket, shield, divertor

  real(dp) :: maintenance_gen = 0.05D0
  !! Maintenance cost factor: All other components except coils, vacuum vessel, 
  !! thermal shield, cryostat, land

  real(dp) :: amortization = 13.6D0
  !! amortization factor (fixed charge factor) "A" (years)

  integer :: cost_model = 1
  !! Switch for cost model:
  !!
  !! - =0 use $ 1990 PROCESS model
  !! - =1 use $ 2014 Kovari model
  !! - =2 use $ 1980 STEP model (NOT RECOMMENDED - Under Development)

  real(dp) :: cowner = 0.15D0
  !! owner cost factor

  real(dp) :: cplife = 0.0D0
  !! lifetime of centrepost (y)

  real(dp) :: cpstcst = 0.0D0
  !! ST centrepost direct cost (M$)

  real(dp) :: cpstflnc = 10.0D0
  !! allowable ST centrepost neutron fluence (MW-yr/m2)

  real(dp) :: crctcore = 0.0D0
  !! reactor core costs (categories 221, 222 and 223)

  real(dp) :: csi = 16.0D0
  !! allowance for site costs (M$)

  real(dp) :: cturbb = 38.0D0
  !! cost of turbine building (M$)

  real(dp) :: decomf = 0.1D0
  !! proportion of constructed cost required for decommissioning fund

  real(dp) :: dintrt = 0.0D0
  !! diff between borrowing and saving interest rates

  real(dp) :: divcst = 0.0D0
  !! divertor direct cost (M$)

  real(dp) :: divlife = 0.0D0
  !! lifetime of divertor (y)

  real(dp) :: dtlife = 0.0D0
  !! period prior to the end of the plant life that the decommissioning fund is used (years)

  real(dp) :: fcap0 = 1.165D0
  !! average cost of money for construction of plant assuming design/construction time of six years

  real(dp) :: fcap0cp = 1.08D0
  !! average cost of money for replaceable components assuming lead time for these of two years

  real(dp) :: fcdfuel = 0.1D0
  !! fraction of current drive cost treated as fuel (if `ifueltyp = 1`)

  real(dp) :: fcontng = 0.195D0
  !! project contingency factor

  real(dp) :: fcr0 = 0.0966D0
  !! fixed charge rate during construction

  real(dp) :: fkind = 1.0D0
  !! multiplier for Nth of a kind costs

  real(dp) :: fwallcst = 0.0D0
  !! first wall cost (M$)

  integer :: iavail= 2
  !! Switch for plant availability model:
  !!
  !! - =0 use input value for cfactr
  !! - =1 calculate cfactr using Taylor and Ward 1999 model
  !! - =2 calculate cfactr using new (2015) model

  real(dp) :: avail_min = 0.75D0
  !! Minimum availability (`constraint equation 61`)

  real(dp) :: tok_build_cost_per_vol = 1283.0D0
  !! Unit cost for tokamak complex buildings, including building and site services ($/m3)

  real(dp) :: light_build_cost_per_vol = 270.0D0
  !! Unit cost for unshielded non-active buildings ($/m3)

  real(dp) :: favail = 1.0D0
  !! F-value for minimum availability (`constraint equation 61`)

  integer :: num_rh_systems = 4
  !! Number of remote handling systems (1-10)

  real(dp) :: conf_mag = 0.99D0
  !! c parameter, which determines the temperature margin at which magnet lifetime starts to decline

  real(dp) :: div_prob_fail = 0.0002D0
  !! Divertor probability of failure (per op day)

  real(dp) :: div_umain_time = 0.25D0
  !! Divertor unplanned maintenance time (years)

  real(dp) :: div_nref = 7000.0D0
  !! Reference value for cycle cycle life of divertor

  real(dp) :: div_nu = 14000.0D0
  !! The cycle when the divertor fails with 100% probability

  real(dp) :: fwbs_nref = 20000.0D0
  !! Reference value for cycle life of blanket

  real(dp) :: fwbs_nu = 40000.0D0
  !! The cycle when the blanket fails with 100% probability

  real(dp) :: fwbs_prob_fail = 0.0002D0
  !! Fwbs probability of failure (per op day)

  real(dp) :: fwbs_umain_time = 0.25D0
  !! Fwbs unplanned maintenance time (years)

  real(dp) :: redun_vacp = 25.0D0
  !! Vacuum system pump redundancy level (%)

  integer :: redun_vac = 0
  !! Number of redundant vacuum pumps

  real(dp) :: t_operation = 0.0D0
  !! Operational time (yrs)

  real(dp) :: tbktrepl = 0.5D0
  !! time taken to replace blanket (y) (`iavail=1`)

  real(dp) :: tcomrepl = 0.5D0
  !! time taken to replace both blanket and divertor (y) (`iavail=1`)

  real(dp) :: tdivrepl = 0.25D0
  !! time taken to replace divertor (y) (`iavail=1`)

  real(dp) :: uubop = 0.02D0
  !! unplanned unavailability factor for balance of plant (`iavail=1`)

  real(dp) :: uucd = 0.02D0
  !! unplanned unavailability factor for current drive (`iavail=1`)

  real(dp) :: uudiv = 0.04D0
  !! unplanned unavailability factor for divertor (`iavail=1`)

  real(dp) :: uufuel = 0.02D0
  !! unplanned unavailability factor for fuel system (`iavail=1`)

  real(dp) :: uufw = 0.04D0
  !! unplanned unavailability factor for first wall (`iavail=1`)

  real(dp) :: uumag = 0.02D0
  !! unplanned unavailability factor for magnets (`iavail=1`)

  real(dp) :: uuves = 0.04D0
  !! unplanned unavailability factor for vessel (`iavail=1`)

  integer :: ifueltyp = 0
  !! Switch for fuel type:
  !!
  !! - =2 treat initial blanket, divertor, first wall
  !!   as capital costs. Treat all later items and 
  !!   fraction fcdfuel of CD equipment as fuel costs
  !! - =1 treat blanket divertor, first wall and
  !!   fraction fcdfuel of CD equipment as fuel cost
  !! - =0 treat these as capital cost

  integer :: ipnet = 0
  !! Switch for net electric power calculation:
  !!
  !! - =0 scale so that always > 0
  !! - =1 let go < 0 (no c-o-e)

  integer :: ireactor = 1
  !! Switch for net electric power and cost of electricity calculations:
  !!
  !! - =0 do not calculate MW(electric) or c-o-e
  !! - =1 calculate MW(electric) and c-o-e

  integer :: lsa = 4
  !! Level of safety assurance switch (generally, use 3 or 4):
  !!
  !! - =1 truly passively safe plant
  !! - =2,3 in-between
  !! - =4 like current fission plant

  real(dp) :: moneyint = 0.0D0
  !! interest portion of capital cost (M$)

  integer :: output_costs = 1
  !! Switch for costs output:
  !!
  !! - =0 do not write cost-related outputs to file
  !! - =1 write cost-related outputs to file

  real(dp) :: ratecdol = 0.0435D0
  !! effective cost of money in constant dollars

  real(dp) :: step_con = 1.5D-1
  !! Contingency Percentage

  real(dp), dimension(68) :: step_ref = &
    (/ 3.0D0, 3.0D-1, 1.115D1, 1.5744D2, 3.592D1, 7.96D0, 9.16D0, 3.26D0, 5.369D1, &
    1.88D0, 6.6D-1, 8.63D0, 3.1D0, 2.05D0, 8.7D-1, 8.7D-1, 9.1D-1, 3.1D-1, 1.81D0, &
    8.236D1, 1.8607D2, 1.2572D2, 3.46D1, 7.25D0, 4.0D0, 3.349D1, 5.274D1, 4.86D0, &
    5.29D1, 2.45D0, 2.82D0, 1.676D1, 6.984D1, 7.7D0, 3.6D0, 2.8D0, 8.0D-1, 1.7D0, &
    1.8D0, 1.3D0, 3.86D1, 3.83D1, 0.0D0, 2.4D-1, 8.0D-2, 0.0D0, 2.0D0, 1.97D0, 1.16D0, &
    2.341D1, 7.733D1, 4.37D0, 4.434D1, 1.918D1, 9.39D0, 5.084D1, 8.7D0, 1.239D1, &
    1.704D1, 7.8D0, 2.11D0, 1.74D1, 3.599D1, 8.2D0, 1.568D1, 1.235D1, 6.22D0, 7.5D-1 /)
  !! Reference values for cost model 2

  real(dp) :: tlife = 30.0D0
  !! plant life (years)

  real(dp), parameter :: ucad = 180.0D0
  !! unit cost for administration buildings (M$/m3)

  real(dp), parameter :: ucaf = 1.5D6
  !! unit cost for aux facility power equipment ($)

  real(dp), parameter :: ucahts = 31.0D0
  !! unit cost for aux heat transport equipment ($/W**exphts)

  real(dp), parameter :: ucap = 17.0D0
  !! unit cost of auxiliary transformer ($/kVA)

  real(dp) :: ucblbe = 260.0D0
  !! unit cost for blanket beryllium ($/kg)

  real(dp) :: ucblbreed = 875.0D0
  !! unit cost for breeder material ($/kg) (`blktmodel>0`)

  real(dp) :: ucblli = 875.0D0
  !! unit cost for blanket lithium ($/kg) (30% Li6)

  real(dp) :: ucblli2o = 600.0D0
  !! unit cost for blanket Li_2O ($/kg)

  real(dp) :: ucbllipb = 10.3D0
  !! unit cost for blanket Li-Pb ($/kg) (30% Li6)

  real(dp) :: ucblss = 90.0D0
  !! unit cost for blanket stainless steel ($/kg)

  real(dp) :: ucblvd = 200.0D0
  !! unit cost for blanket vanadium ($/kg)

  real(dp), parameter :: ucbpmp = 2.925D5
  !! vacuum system backing pump cost ($)

  real(dp) :: ucbus = 0.123D0
  !! cost of aluminium bus for TF coil ($/A-m)

  real(dp) :: uccase = 50.0D0
  !! cost of superconductor case ($/kg)

  real(dp), parameter :: ucco = 350.0D0
  !! unit cost for control buildings (M$/m3)

  real(dp) :: uccpcl1 = 250.0D0
  !! cost of high strength tapered copper ($/kg)

  real(dp) :: uccpclb = 150.0D0
  !! cost of TF outboard leg plate coils ($/kg)

  real(dp), parameter :: uccpmp = 3.9D5
  !! vacuum system cryopump cost ($)

  real(dp), parameter :: uccr = 460.0D0
  !! unit cost for cryogenic building (M$/vol)

  real(dp) :: uccry = 9.3D4
  !! heat transport system cryoplant costs ($/W**expcry)

  real(dp) :: uccryo = 32.0D0
  !! unit cost for vacuum vessel ($/kg)

  real(dp) :: uccu = 75.0D0
  !! unit cost for copper in superconducting cable ($/kg)

  real(dp), parameter :: ucdgen = 1.7D6
  !! cost per 8 MW diesel generator ($)

  real(dp) :: ucdiv = 2.8D5
  !! cost of divertor blade ($)

  real(dp), parameter :: ucdtc = 13.0D0
  !! detritiation, air cleanup cost ($/10000m3/hr)

  real(dp), parameter :: ucduct = 4.225D4
  !! vacuum system duct cost ($/m)

  real(dp) :: ucech = 3.0D0
  !! ECH system cost ($/W)

  real(dp), parameter :: ucel = 380.0D0
  !! unit cost for electrical equipment building (M$/m3)

  real(dp), parameter :: uces1 = 3.2D4
  !! MGF (motor-generator flywheel) cost factor ($/MVA**0.8)
  
  real(dp), parameter :: uces2 = 8.8D3
  !! MGF (motor-generator flywheel) cost factor ($/MJ**0.8)

  real(dp) :: ucf1 = 2.23D7
  !! cost of fuelling system ($)

  real(dp) :: ucfnc = 35.0D0
  !! outer PF coil fence support cost ($/kg)

  real(dp), parameter :: ucfpr = 4.4D7
  !! cost of 60g/day tritium processing unit ($)

  real(dp) :: ucfuel = 3.45D0
  !! unit cost of D-T fuel (M$/year/1200MW)

  real(dp), parameter :: ucfwa = 6.0D4
  !! first wall armour cost ($/m2)

  real(dp), parameter :: ucfwps = 1.0D7
  !! first wall passive stabiliser cost ($)

  real(dp), parameter :: ucfws = 5.3D4
  !! first wall structure cost ($/m2)
  
  real(dp), parameter :: ucgss = 35.0D0
  !! cost of reactor structure ($/kg)

  real(dp) :: uche3 = 1.0D6
  !! cost of helium-3 ($/kg)

  real(dp) :: uchrs = 87.9D6
  !! cost of heat rejection system ($)

  real(dp), dimension(2) :: uchts = (/15.3D0, 19.1D0/)
  !! cost of heat transport system equipment per loop ($/W); dependent on coolant type (coolwh)

  real(dp) :: uciac = 1.5D8
  !! cost of instrumentation, control & diagnostics ($)

  real(dp) :: ucich = 3.0D0
  !! ICH system cost ($/W)

  real(dp), parameter :: ucint = 35.0D0
  !! superconductor intercoil structure cost ($/kg)

  real(dp) :: uclh = 3.3D0
  !! lower hybrid system cost ($/W)

  real(dp), parameter :: uclv = 16.0D0
  !! low voltage system cost ($/kVA)

  real(dp), parameter :: ucmb = 260.0D0
  !! unit cost for reactor maintenance building (M$/m3)

  real(dp) :: ucme = 1.25D8
  !! cost of maintenance equipment ($)

  real(dp) :: ucmisc = 2.5D7
  !! miscellaneous plant allowance ($)

  real(dp) :: ucnbi = 3.3D0
  !! NBI system cost ($/W)

  real(dp), parameter :: ucnbv = 1000.0D0
  !! cost of nuclear building ventilation ($/m3)

  real(dp), dimension(4) :: ucoam = (/68.8D0, 68.8D0, 68.8D0, 74.4D0/)
  !! annual cost of operation and maintenance (M$/year/1200MW**0.5)

  real(dp) :: ucpens = 32.0D0
  !! penetration shield cost ($/kg)

  real(dp) :: ucpfb = 210.0D0
  !! cost of PF coil buses ($/kA-m)

  real(dp) :: ucpfbk = 1.66D4
  !! cost of PF coil DC breakers ($/MVA**0.7)

  real(dp) :: ucpfbs = 4.9D3
  !! cost of PF burn power supplies ($/kW**0.7)

  real(dp) :: ucpfcb = 7.5D4
  !! cost of PF coil AC breakers ($/circuit)

  real(dp) :: ucpfdr1 = 150.0D0
  !! cost factor for dump resistors ($/MJ)

  real(dp) :: ucpfic = 1.0D4
  !! cost of PF instrumentation and control ($/channel)

  real(dp) :: ucpfps = 3.5D4
  !! cost of PF coil pulsed power supplies ($/MVA)

  real(dp), parameter :: ucphx = 15.0D0
  !! primary heat transport cost ($/W**exphts)

  real(dp), parameter :: ucpp = 48.0D0
  !! cost of primary power transformers ($/kVA**0.9)

  real(dp) :: ucrb = 400.0D0
  !! ucrb /400.0/ : cost of reactor building (M$/m3)
  real(dp), dimension(7) :: ucsc = &
  !! ucsc(6) /600.0,600.0,300.0,600.0/ : cost of superconductor ($/kg)
        (/600.0D0, 600.0D0, 300.0D0, 600.0D0, 600.0D0, 600.0D0,300.0D0/)
  real(dp), parameter :: ucsh = 115.0D0
  !! cost of shops and warehouses (M$/m3)

  real(dp) :: ucshld = 32.0D0
  !! cost of shield structural steel ($/kg)

  real(dp), parameter :: ucswyd = 1.84D7
  !! switchyard equipment costs ($)

  real(dp) :: uctfbr = 1.22D0
  !! cost of TF coil breakers ($/W**0.7)

  real(dp) :: uctfbus = 100.0D0
  !! cost of TF coil bus ($/kg)

  real(dp), parameter :: uctfdr = 1.75D-4
  !! cost of TF coil dump resistors ($/J)

  real(dp), parameter :: uctfgr = 5000.0D0
  !! additional cost of TF coil dump resistors ($/coil)

  real(dp), parameter :: uctfic = 1.0D4
  !! cost of TF coil instrumentation and control ($/coil/30)

  real(dp) :: uctfps = 24.0D0
  !! cost of TF coil power supplies ($/W**0.7)

  real(dp) :: uctfsw = 1.0D0
  !! cost of TF coil slow dump switches ($/A)

  real(dp), parameter :: uctpmp = 1.105D5
  !! cost of turbomolecular pump ($)

  real(dp), parameter :: uctr = 370.0D0
  !! cost of tritium building ($/m3)

  real(dp), dimension(2) :: ucturb = (/230.0D6, 245.0D6/)
  !! cost of turbine plant equipment ($) (dependent on coolant type coolwh)

  real(dp), parameter :: ucvalv = 3.9D5
  !! vacuum system valve cost ($)

  real(dp), parameter :: ucvdsh = 26.0D0
  !! vacuum duct shield cost ($/kg)

  real(dp), parameter :: ucviac = 1.3D6
  !! vacuum system instrumentation and control cost ($)

  real(dp) :: ucwindpf = 465.0D0
  !! cost of PF coil superconductor windings ($/m)

  real(dp) :: ucwindtf = 480.0D0
  !! cost of TF coil superconductor windings ($/m)

  real(dp), parameter :: ucws = 460.0D0
  !! cost of active assembly shop ($/m3)

  real(dp), dimension(4) :: ucwst = (/0.0D0, 3.94D0, 5.91D0, 7.88D0/)
  !! cost of waste disposal (M$/y/1200MW)
  
end module cost_variables