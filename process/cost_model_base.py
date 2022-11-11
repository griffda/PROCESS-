from process.variables import AnnotatedVariable

from process.fortran import constants
from process.fortran import build_variables as bv
from process.fortran import cost_variables as cv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import buildings_variables as bldgsv
from process.fortran import current_drive_variables as cdv
from process.fortran import tfcoil_variables as tfv
from process.fortran import pfcoil_variables as pfv
from process.fortran import structure_variables as sv
from process.fortran import divertor_variables as dv


class CostModelBase:
    """Cost Model Base

    Module containing costing algorithms for both the STEP prototype power plant,
    as well as for potential future commerical power plants.
    STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    Sheffield et al. (1986), Fusion Technology, 9, 199
    Sheffield & Milora (2016), Fusion Science and Technology, 70, 14
    """

    def __init__(self) -> None:
        # Scaling Properties
        self.vfi = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.vfi_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.ptherm_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.rmajor_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.rminor_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.pth = AnnotatedVariable(float, 0.0, docstring="", units="")

    def site_permits_costs(self):
        """Site Permits
        author: J A Foster, CCFE, Culham Science Centre
        This method evaluates the Site Permits costs.
        """
        # Site Permits
        # Fixed cost (2017 M$), read from input, default = 100 M$
        self.site_permits_cost = cv.site_permits / 1e6

    def plant_license_costs(self):
        """Plant Licensing
        author: J A Foster, CCFE, Culham Science Centre
        This method evaluates the Plant Licensing costs.
        """
        # Plant Licensing
        # Fixed cost (2017 M$), read from input, default = 100 M$
        self.plant_license_cost = cv.plant_licensing / 1e6

    def land_and_rights_costs(self):
        """Land and Rights
        author: S I Muldrew, CCFE, Culham Science Centre
        This method evaluates the Land and Rights costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Land
        # Fixed site cost (2017 M$); read from input, default = 100 M$
        self.land_cost = cv.sitecost / 1e6

        # Site Preparation
        # Original STARFIRE value
        self.site_prep_cost = cv.step_ref[1]

        # Total
        self.land_and_rights_cost = self.land_cost + self.site_prep_cost

    def site_improvements_costs(self):
        """Site Improvements
        Author: R Chapman, UKAEA
        This routine evaluates the Site Improvements costs.
        Accounts are a mixture of unit costings that scale with site area
        and fixed provisional allowances, for which more precise costings
        should be completed following site selection.
        """
        # Initialise total Site Improvement costs
        self.site_improvements_cost = 0.0e0

        # Site Improvements - general

        # Grading to surface level
        # Movement of existing material around site to facilitate construction activities
        # (cost per m2) * (site area)
        site_improvements0101 = cv.site_imp_uc[0] * cv.whole_site_area
        self.site_improvements01 = site_improvements0101

        # General Excavation
        # Excavation up to actual construction depth including transport on site
        # and double handling material; assumed 1m depth excavation
        # (cost per m2) * (site area)
        site_improvements0102 = cv.site_imp_uc[1] * cv.whole_site_area
        self.site_improvements01 += site_improvements0102

        # Soil treatment
        # Soil treatment; remediation and contamination
        # Provisional allowance only; site specific
        # (cost per m2) * (site area)
        site_improvements0103 = cv.site_imp_uc[2] * cv.whole_site_area
        self.site_improvements01 += site_improvements0103

        # Filling & backfill
        # Filling and backfill site-won material and imported granular fill;
        # asssumed 2m deep average
        # (cost per m3) * (site area) * (2m depth)
        site_improvements0104 = cv.site_imp_uc[3] * cv.whole_site_area * 2.0e0
        self.site_improvements01 += site_improvements0104

        # 21.01.01.05 Disposal
        # Provisional allowance only; site specific; fixed cost
        # Allowance for disposal offsite, non-hazardous contaminated material
        site_improvements010501 = cv.site_imp_uc[4]
        # Allowance for disposal offsite, hazardous contaminated material
        site_improvements010502 = cv.site_imp_uc[5]
        # Disposal off site inert and contaminated soil in excavation below
        # actual construction depth as additional settlement mitigation
        site_improvements010503 = cv.site_imp_uc[6]
        site_improvements0105 = (
            site_improvements010501 + site_improvements010502 + site_improvements010503
        )
        self.site_improvements01 += site_improvements0105

        # Site Clearance
        # (cost per m2) * (site area)
        site_improvements0106 = cv.site_imp_uc[7] * cv.whole_site_area
        self.site_improvements01 += site_improvements0106

        # Removal of Trees & Vegetation
        # Provisional allowance only; site specific
        # Vegetation & tree clearance
        # (cost per m2) * (site area) * (proportion of site)
        site_improvements010701 = cv.site_imp_uc[8] * cv.whole_site_area * 0.2e0
        # Allowance for removal of trees
        site_improvements010702 = cv.site_imp_uc[9]
        site_improvements0107 = site_improvements010701 + site_improvements010702
        self.site_improvements01 += site_improvements0107

        # New Landscaping
        # (cost per m2) * (site area) * (proportion of site)
        # Landscaping
        site_improvements010801 = cv.site_imp_uc[10] * cv.whole_site_area * 0.2e0
        # Hedges
        site_improvements010802 = cv.site_imp_uc[11] * cv.whole_site_area * 2.3e-3
        site_improvements0108 = site_improvements010801 + site_improvements010802
        self.site_improvements01 += site_improvements0108

        # Constraints and Mitigation
        # Provisional allowance only; site specific
        # (cost per m2) * (site area)
        # Environmental
        site_improvements010901 = cv.site_imp_uc[12] * cv.whole_site_area
        # Unexploded Ordnance (UXO)
        site_improvements010902 = cv.site_imp_uc[13] * cv.whole_site_area
        # Archaeological
        site_improvements010903 = cv.site_imp_uc[14] * cv.whole_site_area
        # Local Stakeholders (including landowners and rights of way etc.)
        site_improvements010904 = cv.site_imp_uc[15] * cv.whole_site_area
        site_improvements0109 = (
            site_improvements010901
            + site_improvements010902
            + site_improvements010903
            + site_improvements010904
        )
        self.site_improvements01 += site_improvements0109

        # Roads, pavements and parking areas (Main Site-wide road network)
        # Site internal access; Main road onsite including secondary roads,
        # road surface water network and parking and utilities
        # (cost per m2) * (site area)
        site_improvements0110 = cv.site_imp_uc[16] * cv.whole_site_area
        self.site_improvements01 += site_improvements0110

        # External Lighting
        # Street lighting columns, including 44W and 67W LED street lighting luminaires,
        # ducting, cables and excavation works, backfilling, pull pit chambers,
        # feeder pillars, earth blocks and MCCBs to lighting circuits and columns
        # (cost per m2) * (site area)
        site_improvements0111 = cv.site_imp_uc[17] * cv.whole_site_area
        self.site_improvements01 += site_improvements0111

        if cv.igridconn == 0:
            # Connection to Electricity grid (outgoing)
            # Includes transformers, busbars, cabling installation, fire protection, earthing.
            # Based upon a circa 3GW power production plant.
            site_improvements0112 = cv.site_imp_uc[18]
            self.site_improvements01 += site_improvements0112

        # Retaining Walls
        # Site specific; retaining walls varying height from 1.2m to 3m high.
        # Note: NOT sea defence solution.
        # (cost per m) * SQRT(site area)
        site_improvements0113 = cv.site_imp_uc[19] * (cv.whole_site_area) ** 0.5
        self.site_improvements01 += site_improvements0113

        # Fencing
        # Acoustic Fencing, height 5m; Construction Fence 3m height with mesh panel frame,
        # independent gate posts, post and rail type wooden fence; proposed CCTV cameras,
        # card site access control posts for vehicles, pedestrians; hostile vehicle mitigation
        # (cost per m) * SQRT(site area)
        site_improvements0114 = cv.site_imp_uc[20] * (cv.whole_site_area) ** 0.5
        self.site_improvements01 += site_improvements0114

        # Railings
        # Excluded (no benchmark cost data available)
        site_improvements0115 = 0.0e0
        self.site_improvements01 += site_improvements0115

        # Gateways
        # Excluded (no benchmark cost data available)
        site_improvements0116 = 0.0e0
        self.site_improvements01 += site_improvements0116

        # Sanitary Sewer System (Foul water system)
        # Foul water drainage network including 6 pumping stations (duty and standby),
        # instrumentations and controls. Assumes local water authority has sufficient
        # capacity to absorb effluent without need for additional reinforcement.
        site_improvements0117 = cv.site_imp_uc[21]
        self.site_improvements01 += site_improvements0117

        # Yard Drainage and Storm Sewer System (Surface water system)
        # Surface water system
        # Carrier drains, filter drains, excavations, bedding, manholes, swales to
        # main areas of the site (excludes surface water to power station and buildings)
        site_improvements011801 = cv.site_imp_uc[22]
        # Groundwater & surface water treatment buildings
        # Includes for groundwater treatment plant, surface water treatment plant,
        # surface water pumping station and connection to existing system
        site_improvements011802 = cv.site_imp_uc[23]
        # Allowance for a single Water Management Zone
        # Provisional allowance
        site_improvements011803 = cv.site_imp_uc[24]
        site_improvements0118 = (
            site_improvements011801 + site_improvements011802 + site_improvements011803
        )
        self.site_improvements01 += site_improvements0118

        # Foundation Preparation for construction area
        # Geogrid Reinforcement, including geotextile separation layer
        # & concrete hardstanding 0.5m thick
        # (cost per m2) * (site area)
        site_improvements0119 = cv.site_imp_uc[25] * cv.whole_site_area
        self.site_improvements01 += site_improvements0119

        # Diaphragm Wall
        # Cut off Diaphragm Wall approx. 1,000m long, 52m deep, 1.5m thick
        site_improvements012001 = cv.site_imp_uc[26]
        # Dewatering to cut off wall
        site_improvements012002 = cv.site_imp_uc[27]
        site_improvements0120 = site_improvements012001 + site_improvements012002
        self.site_improvements01 += site_improvements0120

        # Running total (M$)
        self.site_improvements01 = self.site_improvements01 / 1.0e6
        self.site_improvements_cost += self.site_improvements01

        # Diversions
        # Awaiting appropriate estimate
        self.site_improvements02 = 0.0e0

        # Running total (M$)
        self.site_improvements02 = self.site_improvements02 / 1.0e6
        self.site_improvements_cost += self.site_improvements02

        # Relocation of Buildings & services
        # Demolition of existing buildings
        site_improvements0301 = cv.site_imp_uc[28]
        # Access to site and internal transportation
        # Access road from existing highway; main road onsite & secondary roads
        site_improvements0302 = cv.site_imp_uc[29]
        # Incoming utilities & construction site setup
        # Includes all incoming services for entire site
        site_improvements0303 = cv.site_imp_uc[30]
        self.site_improvements03 = (
            site_improvements0301 + site_improvements0302 + site_improvements0303
        )

        # Running total (M$)
        self.site_improvements03 = self.site_improvements03 / 1.0e6
        self.site_improvements_cost += self.site_improvements03

        # Associated developments
        self.site_improvements04 = 0.0e0

        # Park & Ride facilities
        # Includes 2457 car park spaces, external works, services, landscaping, etc.
        site_improvements0401 = cv.site_imp_uc[31]
        self.site_improvements04 += site_improvements0401

        # Other site facilities/activities
        # Include vehicle inspection areas, security facilities for lorry parking, etc.
        site_improvements0402 = cv.site_imp_uc[32]
        self.site_improvements04 += site_improvements0402

        # Entrance Plaza
        # Site prep. & ext. works: hardstandings, gantries for freight management, bus stops.
        site_improvements0403 = cv.site_imp_uc[33]
        self.site_improvements04 += site_improvements0403

        # Campus Accommodation
        if cv.isiteaccomm == 0:
            # Residential
            # Campus accommodation including canteen, shops, laundry,
            # internal recreation facilities, medical centre and sports pitches.
            site_improvements040401 = cv.site_imp_uc[34]
            self.site_improvements04 += site_improvements040401
        # Bus service
        # Bus station building and site maintenance and services.
        site_improvements040402 = cv.site_imp_uc[35]
        self.site_improvements04 += site_improvements040402

        # Multi-storey Car parks
        site_improvements0405 = cv.site_imp_uc[36]
        self.site_improvements04 += site_improvements0405

        # Ancillary buildings
        # Generic costs from BCIS building rates
        # Medical Centre​
        site_improvements040601 = cv.site_imp_uc[37]
        # Public information centre
        site_improvements040602 = cv.site_imp_uc[38]
        # Indoor sports and entertainment centre​
        site_improvements040603 = cv.site_imp_uc[39]
        self.site_improvements04 += (
            site_improvements040601 + site_improvements040602 + site_improvements040603
        )

        # Permanent configuration buildings - ancillary
        # Generic costs from BCIS building rates
        # Operation staff buildings/Archive and Documentation building/Ground samples Warehouse/Occupational  Medical Centre
        site_improvements040701 = cv.site_imp_uc[40]
        # Main site office
        site_improvements040702 = cv.site_imp_uc[41]
        # Fire Training Centre for Plant Staff
        site_improvements040703 = cv.site_imp_uc[42]
        # Fire fighting Water building
        site_improvements040704 = cv.site_imp_uc[43]
        # Fire and rescue centre
        site_improvements040705 = cv.site_imp_uc[44]
        # Administrative Office Building/ Administrative warehouse/In-site Restaurant
        site_improvements040706 = cv.site_imp_uc[45]
        # Maintenance Staff Office
        site_improvements040707 = cv.site_imp_uc[46]
        # Meteorological station
        site_improvements040708 = cv.site_imp_uc[47]
        # Goods Entry Relay Store
        site_improvements040709 = cv.site_imp_uc[48]
        # Offsite Vehicle Search Area
        site_improvements040710 = cv.site_imp_uc[49]
        # Vehicle Inspection Cabins
        site_improvements040711 = cv.site_imp_uc[50]
        # Security search facility
        site_improvements040712 = cv.site_imp_uc[51]
        # Service Water Pump Building
        site_improvements040713 = cv.site_imp_uc[52]
        # Simulator Training Building
        site_improvements040714 = cv.site_imp_uc[53]

        site_improvements0407 = (
            site_improvements040701
            + site_improvements040702
            + site_improvements040703
            + site_improvements040704
            + site_improvements040705
            + site_improvements040706
            + site_improvements040707
            + site_improvements040708
            + site_improvements040709
            + site_improvements040710
            + site_improvements040711
            + site_improvements040712
            + site_improvements040713
            + site_improvements040714
        )

        self.site_improvements04 += site_improvements0407

        # Running total (M$)
        self.site_improvements04 = self.site_improvements04 / 1.0e6
        self.site_improvements_cost += self.site_improvements04

        # Waterfront Improvements
        self.site_improvements05 = 0.0e0
        if cv.isitetype == 1:
            # Marine
            # Cooling installations - offshore intake, discharge, fish return, heat sink.
            # (Scaling used relates to thermal power of Hinckley C)
            site_improvements050101 = cv.site_imp_uc[54] * htv.pthermmw * (0.5e0 / 4524)
            # Breakwater - harbour breakwater construction
            site_improvements050102 = cv.site_imp_uc[55]
            site_improvements0501 = site_improvements050101 + site_improvements050102
            self.site_improvements05 += site_improvements0501
            # Material Offloading Facility
            # Marine transportation - jetty complete with conveyor (inc. dismantling at project end)
            site_improvements0502 = cv.site_imp_uc[56]
            self.site_improvements05 += site_improvements0502
        if cv.isitetype != 2:
            # Sea/Flood Defence
            # Armour and mass fill to increase height of existing defences in line with flood analysis.
            site_improvements0503 = cv.site_imp_uc[57]
            self.site_improvements05 += site_improvements0503
        if cv.isitetype == 1:
            # Other Waterfront Improvements
            # Excluded (no benchmark cost data available)
            site_improvements0504 = 0.0e0
            self.site_improvements05 += site_improvements0504

        # Running total (M$)
        self.site_improvements05 = self.site_improvements05 / 1.0e6
        self.site_improvements_cost += self.site_improvements05

        # Reinstatement & Landscaping
        # Reinstatement of campus
        site_improvements0601 = cv.site_imp_uc[58]
        # Reinstatement of construction plot area
        site_improvements0602 = cv.site_imp_uc[59]
        self.site_improvements06 = site_improvements0601 + site_improvements0602

        # Running total (M$)
        self.site_improvements06 = self.site_improvements06 / 1.0e6
        self.site_improvements_cost += self.site_improvements06

        # Transport access
        self.site_improvements07 = 0.0e0
        # Highway Access - offsite highway improvements
        # Includes new roads & junctions, widening, upgrade, etc.
        site_improvements0701 = cv.site_imp_uc[60]
        self.site_improvements07 += site_improvements0701
        if cv.irailaccess == 0:
            # Rail Access
            # Includes new track and associated provisions, interface with existing rail
            site_improvements0702 = cv.site_imp_uc[61]
            self.site_improvements07 += site_improvements0702
        # Air Access
        # Excluded (no benchmark cost data available)
        site_improvements0703 = 0.0e0
        self.site_improvements07 += site_improvements0703

        # Running total (M$)
        self.site_improvements07 = self.site_improvements07 / 1.0e6
        self.site_improvements_cost += self.site_improvements07

    def reactor_bldg_costs(self):
        """Reactor Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Reactor Building costs.

        :return: reactor_bldg_cost
        :rtype: float
        """
        # Reactor Building
        self.reactor_bldg_cost = 8.665e3 * bldgsv.a_reactor_bldg**1.2132 * 1.0e-6

    def turbine_bldg_costs(self):
        """Turbine Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Turbine Building costs.

        :return: turbine_bldg_cost
        :rtype: float
        """
        # Turbine Building
        self.turbine_bldg_cost = 3.14310e5 * htv.pgrossmw * 1.0e-6

    def cooling_sys_struct_costs(self):
        """Cooling System Structures
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Cooling System Structures costs.

        :return: cooling_sys_struct_cost
        :rtype: float
        """
        # Cooling System Structures
        self.cooling_sys_struct_cost = 1.08155e5 * htv.pgrossmw * 1.0e-6

    def electrical_and_power_bldg_costs(self):
        """Electrical Equipment and Power Supply Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Electrical Equipment and Power Supply Building costs.

        :return: electrical_and_power_bldg_cost
        :rtype: float
        """
        # Electrical Equipment and Power Supply Building
        self.electrical_and_power_bldg_cost = (
            (4.688e3 * bldgsv.a_ee_ps_bldg) + 3.185967e6
        ) * 1.0e-6

    def aux_services_bldg_costs(self):
        """Auxiliary Services Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Auxiliary Services Building costs.

        :return: aux_services_bldg_cost
        :rtype: float
        """
        # Auxiliary Services Building
        self.aux_services_bldg_cost = (
            (3.107e3 * bldgsv.a_aux_services_bldg) + 1.206225e6
        ) * 1.0e-6

    def hot_cell_costs(self):
        """Hot Cell
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Hot Cell costs.

        :return: hot_cell_cost
        :rtype: float
        """
        # Hot Cell
        self.hot_cell_cost = ((1.9773e4 * bldgsv.a_hot_cell_bldg) + 5.975425e6) * 1.0e-6

    def reactor_serv_bldg_costs(self):
        """Reactor Service Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Reactor Service Building costs.

        :return: reactor_serv_bldg_cost
        :rtype: float
        """
        # Reactor Service Building
        self.reactor_serv_bldg_cost = (
            (8.563e3 * bldgsv.a_reactor_service_bldg) + 3.657324e6
        ) * 1.0e-6

    def service_water_bldg_costs(self):
        """Service Water Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Service Water Building costs.

        :return: service_water_bldg_cost
        :rtype: float
        """
        # Service Water Building
        self.service_water_bldg_cost = (
            (3.288e3 * bldgsv.a_service_water_bldg) + 3.19189e5
        ) * 1.0e-6

    def fuel_handling_bldg_costs(self):
        """Fuel Handling and Storage Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Fuel Handling and Storage Building costs.

        :return: fuel_handling_bldg_cost
        :rtype: float
        """
        # Fuel Handling and Storage Building
        self.fuel_handling_bldg_cost = (
            (3.1528e4 * bldgsv.a_fuel_handling_bldg) + 9.181501e6
        ) * 1.0e-6

    def control_room_costs(self):
        """Control Room
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Control Room costs.

        :return: control_room_cost
        :rtype: float
        """
        # Control Room
        self.control_room_cost = (
            (1.2393e4 * bldgsv.a_control_room_bldg) + 1.924890e6
        ) * 1.0e-6

    def ac_power_bldg_costs(self):
        """AC Power Supply Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the AC Power Supply Building costs.

        :return: ac_power_bldg_cost
        :rtype: float
        """
        # AC Power Supply Building
        self.ac_power_bldg_cost = (
            (4.9755e4 * bldgsv.a_ac_ps_bldg) + 1.1591271e7
        ) * 1.0e-6

    def admin_bldg_costs(self):
        """Admin Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Admin Building costs.

        :return: admin_bldg_cost
        :rtype: float
        """
        # Admin Building
        self.admin_bldg_cost = ((3.417e3 * bldgsv.a_admin_bldg) + 3.017077e6) * 1.0e-6

    def site_service_bldg_costs(self):
        """Site Service
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Site Service costs.

        :return: site_service_cost
        :rtype: float
        """
        # Site Service
        self.site_service_bldg_cost = (
            (3.842e3 * bldgsv.a_site_service_bldg) + 1.193549e6
        ) * 1.0e-6

    def cryo_and_inert_gas_bldg_costs(self):
        """Cryogenics and Inert Gas Storage Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Cryogenics and Inert Gas Storage Building costs.

        :return: cryo_and_inert_gas_bldg_cost
        :rtype: float
        """
        # Cryogenics and Inert Gas Storage Building
        self.cryo_and_inert_gas_bldg_cost = (
            (7.031e3 * bldgsv.a_cryo_inert_gas_bldg) + 8.19004e5
        ) * 1.0e-6

    def security_bldg_costs(self):
        """Security Building
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Security Building costs.

        :return: security_bldg_cost
        :rtype: float
        """
        # Security Building
        self.security_bldg_cost = (
            (3.227e3 * bldgsv.a_security_bldg) + 2.06804e5
        ) * 1.0e-6

    def vent_stack_costs(self):
        """Ventilation Stack
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Ventilation Stack costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: vent_stack_cost
        :rtype: float
        """
        # Ventilation Stack
        # Original STARFIRE value, scaling with thermal power
        self.vent_stack_cost = cv.step_ref[18] * (self.pth / self.ptherm_star) ** 0.6e0

    def waste_fac_bldg_costs(self):
        """Waste Facilities Buildings
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Waste Facilities Buildings costs.

        :return: waste_fac_bldg_cost
        :rtype: float
        """
        # Waste Facilities Buildings
        # Fixed cost (2017 M$); read from input, default = 100 M$
        self.waste_fac_bldg_cost = cv.wfbuilding / 1.0e6

    def blanket_first_wall_costs(self):
        """Blanket and First Wall
        author: A J Pearce, CCFE, Culham Science Centre
        This routine evaluates the BB+FW costs.
        If ifueltyp = 0, the blanket cost is treated as capital cost
        If ifueltyp = 1, the blanket cost is treated as a fuel cost,
        rather than as a capital cost.
        If ifueltyp = 2, the initial blanket is included as a capital cost
        and the replacement blanket costs are treated as a fuel cost.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :return: fw_blk_total_cost and sub-costs
        :rtype: tuple[float, float, float, float, float, float,]
        """

        # First wall
        self.fw_cost = 1.0e-6 * (
            fwbsv.fw_armour_mass * cv.step_ucfwa + fwbsv.fwmass * cv.step_ucfws
        )

        self.fw_cost = cv.fkind * self.fw_cost

        if cv.ifueltyp == 1:
            cv.fwallcst = self.fw_cost
            self.fw_cost = 0.0e0
        elif cv.ifueltyp == 2:
            cv.fwallcst = self.fw_cost
        else:
            cv.fwallcst = 0.0e0

        # Breeder Blanket

        if htv.ipowerflow == 0:

            # Blanket Multiplier Material
            self.blk_multi_mat_cost = 1.0e-6 * fwbsv.whtblbe * cv.step_ucblbe

            # Blanket Breeder Material
            if fwbsv.blktmodel == 0:
                self.blk_breed_mat_cost = 1.0e-6 * fwbsv.wtblli2o * cv.step_ucblbreed
            else:
                self.blk_breed_mat_cost = 1.0e-6 * fwbsv.whtblbreed * cv.step_ucblbreed
        else:
            if fwbsv.blkttype == 1 or fwbsv.blkttype == 2:
                # Liquid blanket (LiPb + Li)
                # Blanket Multiplier Material
                self.blk_multi_mat_cost = 1.0e-6 * fwbsv.wtbllipb * cv.ucbllipb * 2.99e0
                # Blanket Breeder Material
                self.blk_breed_mat_cost = 1.0e-6 * fwbsv.whtblli * cv.ucblli * 2.99e0
            else:
                # Solid blanket (Li2O + Be)
                # Blanket Multiplier Material
                self.blk_multi_mat_cost = 1.0e-6 * fwbsv.whtblbe * cv.step_ucblbe
                # Blanket Breeder Material
                self.blk_breed_mat_cost = 1.0e-6 * fwbsv.wtblli2o * cv.step_ucblbreed

        self.blk_multi_mat_cost = cv.fkind * self.blk_multi_mat_cost
        self.blk_breed_mat_cost = cv.fkind * self.blk_breed_mat_cost

        # Blanket Steel Costs
        self.blk_steel_cost = 1.0e-6 * fwbsv.whtblss * cv.step_ucblss
        self.blk_steel_cost = cv.fkind * self.blk_steel_cost

        # Blanket Vanadium Costs
        blk_vanadium_cost = 1.0e-6 * fwbsv.whtblvd * cv.step_ucblvd
        blk_vanadium_cost = cv.fkind * blk_vanadium_cost

        # Blanket Carbon Cloth Costs
        blk_carbon_cost = 0.0e0
        blk_carbon_cost = cv.fkind * blk_carbon_cost

        # Blanket Concrete Costs
        blk_concrete_cost = 0.0e0
        blk_concrete_cost = cv.fkind * blk_concrete_cost

        # Blanket FLiBe Costs
        blk_flibe_cost = 0.0e0
        blk_flibe_cost = cv.fkind * blk_flibe_cost

        self.blk_total_cost = (
            self.blk_multi_mat_cost
            + self.blk_breed_mat_cost
            + self.blk_steel_cost
            + blk_vanadium_cost
            + blk_carbon_cost
            + blk_concrete_cost
            + blk_flibe_cost
        )

        if cv.ifueltyp == 1:
            cv.blkcst = self.blk_total_cost
            self.blk_total_cost = 0.0e0
        elif cv.ifueltyp == 2:
            cv.blkcst = self.blk_total_cost
        else:
            cv.blkcst = 0.0e0

        # Total for FW & Blanket
        self.fw_blk_total_cost = self.fw_cost + self.blk_total_cost

        return (
            self.fw_blk_total_cost,
            self.fw_cost,
            self.blk_total_cost,
            self.blk_multi_mat_cost,
            self.blk_breed_mat_cost,
            self.blk_steel_cost,
        )

    def ib_shield_costs(self):
        """Inboard shield.
        Note: outboard shield costs currently set to zero
        """
        # Volume of inboard shield found using same method as in CCFE HCPB blanket model:
        # inboard shield is assumed to be a cylinder of uniform thickness

        # Calculate shield internal half-height (m)
        hbot = pv.rminor * pv.kappa + bv.vgap + dv.divfix
        # if a double null machine then symmetric otherwise asymmetric
        if pv.idivrt == 2:
            htop = hbot
        else:
            htop = (
                pv.rminor * pv.kappa
                + 0.5e0 * (bv.scrapli + bv.scraplo + bv.fwith + bv.fwoth)
                + bv.blnktth
            )

        # Average of top and bottom (m)
        hshld = 0.5e0 * (htop + hbot)

        # Radius to outer edge of inboard shield (m)
        r1 = bv.rsldi + bv.shldith

        # Corrected shield thickness: allows for 300mm vacuum vessel
        # Justification: requirement from K. Taylor, neutronics
        # TODO: replace this correction when valid (VV + shield) is used
        shldith_corr = (bv.d_vv_in + bv.shldith) - 0.3e0
        # Volume of inboard cylindrical shell (m3)
        inb_sh_v = (
            2.0e0
            * (hshld + bv.shldtth)
            * constants.pi
            * (r1**2 - (r1 - shldith_corr) ** 2)
        )

        # Scale shield material volume (allow for 10% volume coolant, 5% steel)
        inb_sh_v_mtl = 0.85e0 * inb_sh_v

        # Define shield material density (sh_mtl_d [kg/m3]) and cost (sh_mtl_c [$/kg])
        if fwbsv.i_shield_mat == 1:
            # tungsten carbide
            sh_mtl_d = fwbsv.denwc
            sh_mtl_c = cv.step_ucshwc
        else:
            # tungsten (default)
            sh_mtl_d = fwbsv.denw
            sh_mtl_c = cv.step_ucshw

        # Find inboard shield mass (kg)
        inb_sh_m = inb_sh_v_mtl * sh_mtl_d

        # Find inboard shield cost (converted to M$2017)
        self.ib_shield_cost = (inb_sh_m * sh_mtl_c) / 1.0e6 * (229.0e0 / 264.71e0)

        self.ib_shield_cost = cv.fkind * self.ib_shield_cost

        # Note: outboard shield costs currently set to zero

    def tf_coils_costs(self):
        """TF Coils"""
        # Initialise local vars
        c_tf_inboard_legs = 0.0e0
        c_tf_outboard_legs = 0.0e0

        costtfsc = 0.0e0
        costtfcu = 0.0e0
        ctfconpm = 0.0e0
        ctfcontot = 0.0e0
        costtfwind = 0.0e0
        costtfcas = 0.0e0
        costtfint = 0.0e0
        costtfgss = 0.0e0

        self.tf_coils_cost = 0.0e0

        # Copper coils
        if tfv.i_tf_sup == 0:
            # Calculation taken from cost model 0: simply the cost of copper conductor masses.
            # Inflating from 1990 $ to 2017 $ at nuclear rate equates to a factor of 2.99.

            # Inboard TF coil legs
            c_tf_inboard_legs = 1.0e-6 * tfv.whtcp * cv.uccpcl1 * 2.99e0

            # Outboard TF coil legs
            c_tf_outboard_legs = 1.0e-6 * tfv.whttflgs * cv.uccpclb * 2.99e0

            # Total TF coil cost
            self.tf_coils_cost = c_tf_inboard_legs + c_tf_outboard_legs

            self.tf_coils_cost = cv.fkind * self.tf_coils_cost

        # Superconducting coils
        elif tfv.i_tf_sup == 1:
            # Calculation taken from cost model 0:
            # Superconductor magnets are costed using a method devised by R. Hancox, 1994.

            # Conductor
            # Superconductor material cost ($/m)
            costtfsc = (
                cv.step_ucsc[tfv.i_tf_sc_mat - 1]
                * tfv.whtconsc
                / (tfv.tfleng * tfv.n_tf_turn)
            )
            # Copper material cost ($/m)
            costtfcu = cv.step_uccu * tfv.whtconcu / (tfv.tfleng * tfv.n_tf_turn)
            # Cost/metre of whole conductor: superconductor + copper + sheath + fixed costs
            ctfconpm = costtfsc + costtfcu + cv.step_cconshtf + cv.step_cconfix
            # Total conductor costs (M$)
            ctfcontot = 1.0e-6 * ctfconpm * tfv.n_tf * tfv.tfleng * tfv.n_tf_turn

            # Winding (M$)
            costtfwind = (
                1.0e-6 * cv.step_ucwindtf * tfv.n_tf * tfv.tfleng * tfv.n_tf_turn
            )

            # Case (M$)
            costtfcas = 1.0e-6 * (tfv.whtcas * cv.step_uccase) * tfv.n_tf

            # Intercoil structure (M$)
            costtfint = 1.0e-6 * sv.aintmass * cv.step_ucint

            # Gravity support structure (M$)
            costtfgss = 1.0e-6 * sv.clgsmass * cv.step_ucgss

            # Total superconducting TF coil costs
            self.tf_coils_cost = (
                ctfcontot + costtfwind + costtfcas + costtfint + costtfgss
            )

            self.tf_coils_cost = cv.fkind * self.tf_coils_cost

        # Cryogenic aluminium coils
        elif tfv.i_tf_sup == 2:
            # Cost approximated as the material cost of conducting Al * a
            # manufacturing cost factor
            # Al conductor mass per coil * number of coils * cost per kilo *
            # manufacturing cost factor, converted to M$
            # step_mc_cryo_al_per = 0.2: 20% manufacturing cost
            self.tf_coils_cost = (
                (tfv.whtconal * tfv.n_tf * cv.step_uc_cryo_al)
                * (cv.step_mc_cryo_al_per + 1.0e0)
                * 1.0e-6
            )

            self.tf_coils_cost = cv.fkind * self.tf_coils_cost

        # ifueltyp: consider centrepost cost as fuel, capital or both?
        # cpstcst used later in coelc_step()
        cv.cpstcst = 0.0e0  # TART centrepost
        if pv.itart == 1:
            if cv.ifueltyp == 1:
                # Treat centrepost cost as fuel cost
                cv.cpstcst = c_tf_inboard_legs
                if tfv.i_tf_sup == 0:
                    # Subtract from capital cost
                    self.tf_coils_cost = self.tf_coils_cost - (
                        cv.fkind * c_tf_inboard_legs
                    )
            elif cv.ifueltyp == 2:
                # Treat centrepost cost as capital and fuel cost
                cv.cpstcst = c_tf_inboard_legs

    def pf_coils_costs(self):
        """PF Coils : PF magnet assemblies
        author: A J Pearce, CCFE, Culham Science Centre
        This routine evaluates the PF magnet costs.
        Conductor costs previously used an algorithm devised by R. Hancox,
        January 1994, under contract to Culham, which took into
        account the fact that the superconductor/copper ratio in
        the conductor is proportional to the maximum field that
        each coil will experience. Now, the input copper fractions
        are used instead.
        Maximum values for current, current density and field
        are used.
        """
        # Total length of PF coil windings (m)
        pfwndl = 0.0e0

        for i in range(pfv.nohc):
            pfwndl += constants.twopi * pfv.rpf[i] * pfv.turns[i]

        # Conductor

        # The following lines take care of resistive coils.
        # costpfsh is the cost per metre of the steel conduit/sheath around
        # each superconducting cable (so is zero for resistive coils)

        if pfv.ipfres == 1:
            costpfsh = 0.0e0
        else:
            costpfsh = cv.step_cconshpf

        # Non-Central Solenoid coils

        if bv.iohcl == 1:
            npf = pfv.nohc - 1
        else:
            npf = pfv.nohc

        pf_coils_cost = 0.0e0

        for i in range(npf):

            # Superconductor ($/m)
            if pfv.ipfres == 0:
                costpfsc = (
                    cv.step_ucsc[pfv.isumatpf - 1]
                    * (1.0e0 - pfv.fcupfsu)
                    * (1.0e0 - pfv.vf[i])
                    * abs(pfv.ric[i] / pfv.turns[i])
                    * 1.0e6
                    / pfv.rjconpf[i]
                    * tfv.dcond[pfv.isumatpf - 1]
                )
            else:
                costpfsc = 0.0e0

            # Copper ($/m)
            if pfv.ipfres == 0:
                costpfcu = (
                    cv.step_uccu
                    * pfv.fcupfsu
                    * (1.0e0 - pfv.vf[i])
                    * abs(pfv.ric[i] / pfv.turns[i])
                    * 1.0e6
                    / pfv.rjconpf[i]
                    * constants.dcopper
                )
            else:
                costpfcu = (
                    cv.step_uccu
                    * (1.0e0 - pfv.vf[i])
                    * abs(pfv.ric[i] / pfv.turns[i])
                    * 1.0e6
                    / pfv.rjconpf[i]
                    * constants.dcopper
                )

            # Total cost/metre of superconductor and copper wire
            costwire = costpfsc + costpfcu

            # Total cost/metre of conductor (including sheath and fixed costs)
            cpfconpm = costwire + costpfsh + cv.step_cconfix

            # Total account 222.2.1 (PF coils excluding Central Solenoid)
            pf_coils_cost = pf_coils_cost + (
                1.0e-6 * constants.twopi * pfv.rpf[i] * pfv.turns[i] * cpfconpm
            )

        pf_coils_cost = cv.fkind * pf_coils_cost

        # Winding
        pf_winding_cost = 1.0e-6 * cv.step_ucwindpf * pfwndl
        pf_winding_cost = cv.fkind * pf_winding_cost

        # Steel case - will be zero for resistive coils
        pf_case_cost = 1.0e-6 * cv.step_uccase * pfv.whtpfs
        pf_case_cost = cv.fkind * pf_case_cost

        # Support structure
        pf_structure_cost = 1.0e-6 * cv.step_ucfnc * sv.fncmass
        pf_structure_cost = cv.fkind * pf_structure_cost

        # Total PF Coils Cost
        self.pf_total_cost = (
            pf_coils_cost + pf_winding_cost + pf_case_cost + pf_structure_cost
        )

    def central_sol_costs(self):
        """Central Solenoid
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Central Solenoid costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Original STARFIRE value, scaling with fusion island volume
        self.central_sol_cost = cv.step_ref[23] * (self.vfi / self.vfi_star)
        self.central_sol_cost = cv.fkind * self.central_sol_cost

    def control_coils_costs(self):
        """Control Coils
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Control Coils costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Original STARFIRE value, scaling with fusion island volume
        self.control_coils_cost = cv.step_ref[24] * (self.vfi / self.vfi_star)
        self.control_coils_cost = cv.fkind * self.control_coils_cost

    def hcd_costs(self):
        """Auxiliary Heating and Current Drive
        Returns cost of auxiliary HCD
        HCD cost = cost per injected Watt of power * injected Watts
        """
        # Cost per Watt depends on technology/hardware used;
        # inflation adjustment applied as appropriate to source for costs
        # (tech adjusted from 1990 $ is costed as per Cost Model 0)
        # Notes:
        # NBI and EC/EBW calculations will be zero if this tech is not included.
        # HCD requirements for start-up and ramp-down calculated in
        # relation to requirements for flat-top operation.

        # Total injected power [W] =
        #      1.0e-6 *
        #      (flat-top operation [MW] +
        #       (startupratio * flat-top operation [MW])
        #       )
        totinjpow_nbi = 0.0e0
        totinjpow_ec = 0.0e0
        totinjpow_ic = 0.0e0
        # Cost calculated from 'cost per injected Watt', coverted to 2017 M$.

        # NBI cost per injected Watt (adjusted from 2020 $):
        totinjpow_nbi = 1.0e-6 * (cdv.pnbitot + (cv.startupratio * cdv.pnbitot))
        self.hcd_cost = (
            totinjpow_nbi * cv.step_ref[68] * (229.0e0 / 258.84e0)
        ) / 1.0e-6

        # EC or EBW cost per injected Watt (adjusted from 2020 $):
        totinjpow_ec = 1.0e-6 * (cdv.echpwr + (cv.startupratio * cdv.echpwr))
        self.hcd_cost += (
            totinjpow_ec * cv.step_ref[69] * (229.0e0 / 258.84e0)
        ) / 1.0e-6

        if cdv.iefrf == 2 or cdv.iefrffix == 2:
            # if primary *or* secondary current drive efficiency model is
            # Ion Cyclotron current drive (adjusted from 1990 $):
            totinjpow_ic = 1.0e-6 * (cdv.plhybd + (cv.startupratio * cdv.plhybd))
            self.hcd_cost += (totinjpow_ic * cv.ucich * (229.0e0 / 76.7e0)) / 1.0e-6

        # if primary current drive efficiency model is any of the following...
        if (
            (cdv.iefrf == 1)
            or (cdv.iefrf == 1)
            or (cdv.iefrf == 4)
            or (cdv.iefrf == 4)
            or (cdv.iefrf == 6)
            or (cdv.iefrf == 6)
        ):
            # ...use calculation for Lower Hybrid system (adjusted from 1990 $):
            totinjpow_ic = 1.0e-6 * (cdv.plhybd + (cv.startupratio * cdv.plhybd))
            self.hcd_cost += (totinjpow_ic * cv.uclh * (229.0e0 / 76.7e0)) / 1.0e-6

        if cv.ifueltyp == 1:
            # fraction `fcdfuel` of HCD cost treated as fuel cost
            # self.self.hcd_cost and cv.cdcost: Cost of HCD in M$
            self.hcd_cost *= 1.0e0 - cv.fcdfuel
            cv.cdcost = self.hcd_cost

        self.hcd_cost = cv.fkind * self.hcd_cost

    def primary_structure_costs(self):
        """Primary Structure and Support
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Control Coils costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Original STARFIRE value, scaling with fusion island volume
        self.primary_structure_cost = cv.step_ref[26] * (self.vfi / self.vfi_star)
        self.primary_structure_cost = cv.fkind * self.primary_structure_cost

    def reactor_vacuum_sys_costs(self):
        """Reactor Vacuum System
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Reactor Vacuum System costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Original STARFIRE value, scaling with fusion island volume
        self.reactor_vacuum_sys_cost = cv.step_ref[27] * (self.vfi / self.vfi_star) ** (
            2.0e0 / 3.0e0
        )
        self.reactor_vacuum_sys_cost = cv.fkind * self.reactor_vacuum_sys_cost

    def power_supplies_costs(self):
        """Power Supplies
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Power Supplies costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Original STARFIRE value, scaling with fusion island volume
        self.power_supplies_cost = cv.step_ref[28] * (self.vfi / self.vfi_star) ** (
            2.0e0 / 3.0e0
        )

    def divertor_costs(self):
        """Divertor
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Divertor costs.
        """
        # Caveat: rough estimate, using rmajor (rather than e.g. rnull) and
        # treating the divertor limbs as vertically oriented cylinders.
        # Greater precision in geometry would provide more accurate results,
        # but (at the time of writing) the divertor design is not yet
        # advanced enough to warrant such precision.
        # Reference cost and area values are for ITER.

        # Use 2D profile of divertor legs to estimate surface area (m2)
        div_profile_length = dv.divleg_profile_inner + dv.divleg_profile_outer
        if pv.idivrt == 2:
            # double-null = double divertor length
            div_profile_length = div_profile_length * 2.0e0
        div_sarea = 2.0e0 * constants.pi * pv.rmajor * div_profile_length

        # divertor cost = ref * (div area / ref area)**0.8
        self.divertor_cost = cv.step_ref[31] * (div_sarea / 60.0e0) ** 0.8e0
        # adjust to 2017$ (from 2014$) using CPI index
        self.divertor_cost = self.divertor_cost * (229.0e0 / 228.0e0)

        if cv.ifueltyp == 1:
            cv.divcst = self.divertor_cost
            self.divertor_cost = 0.0e0
        else:
            cv.divcst = 0.0e0

        self.divertor_cost = cv.fkind * self.divertor_cost

    def heat_transfer_sys_costs(self):
        """Heat Transfer System
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Heat Transfer System costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # pgrossmw is gross electric power of the plant in MW
        self.heat_transfer_sys_cost = 9.2238e4 * htv.pgrossmw * 1.0e-6
        self.heat_transfer_sys_cost = cv.fkind * self.heat_transfer_sys_cost

    def cryo_sys_costs(self):
        """Cryogenic Cooling System
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Account Cryogenic Cooling System costs.
        """
        # Cryoplant - will be zero for resistive coils
        # Parametric costing of cryo systems based on refrigeration capacity produced at Helium temp of 4.5K
        self.cryo_sys_cost = 6.14e0 * tfv.cryo_cool_req**0.63

    def waste_disposal_costs(self):
        """Waste Treatment and Disposal
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Waste Treatment and Disposal costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Liquid Waste
        # Original STARFIRE value, scaling with thermal power
        self.liquid_waste_cost = (
            cv.step_ref[37] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.liquid_waste_cost = cv.fkind * self.liquid_waste_cost
        self.waste_disposal_cost = self.liquid_waste_cost

        # Gaseous Waste
        # Original STARFIRE value, scaling with thermal power
        self.gas_waste_cost = cv.step_ref[38] * (self.pth / self.ptherm_star) ** 0.6e0
        self.gas_waste_cost = cv.fkind * self.gas_waste_cost
        self.waste_disposal_cost += self.gas_waste_cost

        # Solid Waste
        # Original STARFIRE value, scaling with thermal power
        self.solid_waste_cost = cv.step_ref[39] * (self.pth / self.ptherm_star) ** 0.6e0
        self.solid_waste_cost = cv.fkind * self.solid_waste_cost
        self.waste_disposal_cost += self.solid_waste_cost

    def fuel_handling_costs(self):
        """Fuel Handling and Storage
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Fuel Handling and Storage costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: self.fuel_handling_cost and spares
        :rtype: tuple[float, float]
        """
        # Fuel Handling and Storage
        # Original STARFIRE value, scaling with thermal power
        self.fuel_handling_cost = (
            cv.step_ref[40] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.fuel_handling_cost = cv.fkind * self.fuel_handling_cost

        # STARFIRE percentage for self.spares
        spares = 5.026e-2 * self.fuel_handling_cost

        return self.fuel_handling_cost, spares

    def other_reactor_equip_costs(self):
        """Other Reactor Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22.06 (Other Reactor
        Plant Equipment) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: self.other_reactor_equip_cost and spares
        :rtype: tuple[float, float]
        """

        # Maintenance Equipment
        # Original STARFIRE value, scaling with fusion island volume
        # Depreciated by the remote handling scaling in cost account 27.
        self.maintenance_equip_cost = (
            0.0  # step_ref(42) * (vfi / vfi_star)**(2.0e0/3.0e0)
        )
        self.other_reactor_equip_cost = self.maintenance_equip_cost
        # STARFIRE percentage for self.spares
        spares = 4.308e-1 * self.maintenance_equip_cost

        # Special Heating Systems
        # Original STARFIRE value, scaling with thermal power
        self.special_heating_sys_cost = (
            cv.step_ref[42] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.special_heating_sys_cost = cv.fkind * self.special_heating_sys_cost
        self.other_reactor_equip_cost += self.special_heating_sys_cost

        # Coolant Storage
        # Original STARFIRE value, scaling with thermal power
        self.coolant_store_cost = (
            cv.step_ref[43] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.coolant_store_cost = cv.fkind * self.coolant_store_cost
        self.other_reactor_equip_cost += self.coolant_store_cost

        # Gas System
        # Original STARFIRE value, scaling with fusion island volume
        self.gas_sys_cost = cv.step_ref[44] * (self.vfi / self.vfi_star) ** (
            2.0e0 / 3.0e0
        )
        self.gas_sys_cost = cv.fkind * self.gas_sys_cost
        self.other_reactor_equip_cost += self.gas_sys_cost

        # Inert Atmosphere System
        # Original STARFIRE value, scaling with thermal power
        self.inert_atmos_sys_cost = (
            cv.step_ref[45] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.inert_atmos_sys_cost = cv.fkind * self.inert_atmos_sys_cost
        self.other_reactor_equip_cost += self.inert_atmos_sys_cost

        # Fluid Leak Detection
        # Original STARFIRE value, scaling with thermal power
        self.fluid_leak_detect_cost = (
            cv.step_ref[46] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.fluid_leak_detect_cost = cv.fkind * self.fluid_leak_detect_cost
        self.other_reactor_equip_cost += self.fluid_leak_detect_cost

        # Closed Loop Coolant System
        # Original STARFIRE value, scaling with thermal power
        self.closed_loop_coolant_sys_cost = (
            cv.step_ref[47] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.closed_loop_coolant_sys_cost = cv.fkind * self.closed_loop_coolant_sys_cost
        self.other_reactor_equip_cost += self.closed_loop_coolant_sys_cost
        # STARFIRE percentage for self.spares
        spares += 8.3e-1 * (self.pth / self.ptherm_star) ** 0.6e0

        # Standby Cooling System
        # Original STARFIRE value, scaling with thermal power
        self.standby_cooling_sys_cost = (
            cv.step_ref[48] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.standby_cooling_sys_cost = cv.fkind * self.standby_cooling_sys_cost
        self.other_reactor_equip_cost += self.standby_cooling_sys_cost

        return self.other_reactor_equip_cost, spares

    def instrument_and_control_costs(self):
        """Instrumentation and Control
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Instrumentation and Control costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Instrumentation and Control
        # Original STARFIRE value, scaling with thermal power
        self.instrument_and_control_cost = (
            cv.step_ref[49] * (self.pth / self.ptherm_star) ** 0.6e0
        )
        self.instrument_and_control_cost = cv.fkind * self.instrument_and_control_cost

    def turbine_sys_costs(self):
        """Turbine System
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Turbine System costs.
        """
        # Turbine Generators
        # Steam System
        # Condensing System
        # Feedwater Heating System
        # Other Turbine Equipment
        # Instrumentation and Control
        # self.turbine_sys_cost is the sum of the above accounts: total turbine system
        # cost, not treating cooling towers as part of the turbine system
        self.turbine_sys_cost = 5.55440e5 * htv.pgrossmw * 1.0e-6

    def heat_reject_costs(self):
        """Heat Rejection
        author: J A Foster, CCFE, Culham Science Centre
        This routine evaluates the Heat Rejection costs.
        """
        # Heat Rejection
        self.heat_reject_cost = ((8.0437e4 * htv.pgrossmw) + 2.2264895e7) * 1.0e-6

    def electric_plant_equip_costs(self):
        """Electric Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Electric Plant Equipment costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Switch Gear
        self.switch_gear_cost = 1.8906e4 * htv.pgrossmw * 1.0e-6
        self.electric_plant_equip_cost = self.switch_gear_cost

        # Station Service Equipment
        self.station_service_equip_cost = 5.1412e4 * htv.pgrossmw * 1.0e-6
        self.electric_plant_equip_cost += self.station_service_equip_cost

        # Switchboards
        self.switchboards_cost = 2.985e3 * htv.pgrossmw * 1.0e-6
        self.electric_plant_equip_cost += self.switchboards_cost

        # Protective Equipment
        self.protective_equip_cost = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 18.0e0)
            + (4.0e6 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.electric_plant_equip_cost = (
            self.electric_plant_equip_cost + self.protective_equip_cost
        )

        # Electrical Structures
        self.electric_structures_cost = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 1.3e2)
            + (4.0e6 * 9.0e0 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.electric_plant_equip_cost = (
            self.electric_plant_equip_cost + self.electric_structures_cost
        )

        # Power and Control Wiring
        self.power_and_control_wiring_cost = 2.8989e4 * htv.pgrossmw * 1.0e-6
        self.electric_plant_equip_cost += self.power_and_control_wiring_cost

        # Electric Lighting
        self.electric_lighting_cost = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 2.0e2)
            + (4.0e6 * 4.0e0 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.electric_plant_equip_cost += self.electric_lighting_cost

        # Spares
        # STARFIRE percentage
        self.spares = 1.0403e-2 * self.electric_plant_equip_cost
        self.electric_plant_equip_cost += self.spares

        # Contingency
        # STARFIRE 15%
        self.contingency = cv.step_con * self.electric_plant_equip_cost
        self.electric_plant_equip_cost += self.contingency

    def misc_equip_costs(self):
        """Miscellaneous Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Miscellaneous Plant Equipment costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # TODO Need to add reference for cost calculations

        # Transport and Lifting Equipment
        self.transport_equip_cost = (
            (3.8005e4 * (bldgsv.wgt / 1.0e3)) + 1.529727e6
        ) * 1.0e-6
        # wgt is reactor building crane capacity (kg)
        # #TODO Check that wgt is the correct variable to use here
        self.misc_equip_cost = self.transport_equip_cost

        # Air and Water Service System
        self.air_water_sys_cost = 1.20689e5 * htv.pgrossmw * 1.0e-6
        self.misc_equip_cost += self.air_water_sys_cost

        # Communications Equipment
        self.comms_equip_cost = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 2.18e2)
            + (4.0e6 * 3.0e0 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.misc_equip_cost += self.comms_equip_cost

        # Furnishing and Fixtures
        self.furnishing_cost = 3.0e3 * htv.pgrossmw * 1.0e-6
        self.misc_equip_cost += self.furnishing_cost

        # Spares
        # Original STARFIRE value, no scaling
        self.spares = 1.286e-2 * self.misc_equip_cost
        self.misc_equip_cost += self.spares

        # Contingency
        # STARFIRE 15%
        self.contingency = cv.step_con * self.misc_equip_cost
        self.misc_equip_cost += self.contingency

    def remote_handling_costs(self):
        """Remote Handling
        author: A J Pearce, CCFE, Culham Science Centre
        This routine evaluates the Account 27 Remote Handling costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # Remote Handling
        # From report by T. Hender CD-STEP-01030, scales with direct capital costs
        self.remote_handling_cost = cv.step_rh_costfrac * cv.cdirt
        self.remote_handling_cost = cv.fkind * self.remote_handling_cost

    def indirect_costs(self):
        """Indirect costs
        Calculate the indirect costs.
        """
        # Construction Facilities, Equipment and Services (default 30%)
        self.construct_facs_cost = cv.step91_per * cv.cdirt

        # Engineering and Costruction Management Services (default 32.5%)
        self.eng_construct_manage_cost = cv.step92_per * cv.cdirt

        # Other Costs (default 5%)
        self.other_cost = cv.step93_per * cv.cdirt
