from process.variables import AnnotatedVariable

from process import fortran as ft
from process.fortran import constants
from process.fortran import build_variables as bv
from process.fortran import cost_variables as cv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import process_output as po
from process.fortran import buildings_variables as bldgsv
from process.fortran import times_variables as tv
from process.utilities.f2py_string_patch import f2py_compatible_to_string
from process.fortran import current_drive_variables as cdv
from process.fortran import tfcoil_variables as tfv
from process.fortran import pfcoil_variables as pfv
from process.fortran import structure_variables as sv
from process.fortran import divertor_variables as dv


class CostsStep:
    """STEP fusion power plant costings.

    Module containing STEP fusion power plant costing algorithms
    author: S I Muldrew, CCFE, Culham Science Centre
    N/A
    This module contains the STEP fusion power plant costing model,
    developed by Nizar Ben Ayed, Tim Hender and Stuart Muldrew, based
    on the STARFIRE costing framework.
    STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    Sheffield et al. (1986), Fusion Technology, 9, 199
    Sheffield & Milora (2016), Fusion Science and Technology, 70, 14
    """

    def __init__(self):
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        self.iprint = 0  # switch for writing to output file (1=yes)

        # Various cost account values (M$)
        self.step20 = AnnotatedVariable(
            float, 0.0, docstring="step20 account cost", units="M$"
        )
        self.step21 = AnnotatedVariable(
            float, 0.0, docstring="step21 account cost", units="M$"
        )
        self.step22 = AnnotatedVariable(
            float, 0.0, docstring="step22 account cost", units="M$"
        )
        self.step23 = AnnotatedVariable(
            float, 0.0, docstring="step23 account cost", units="M$"
        )
        self.step24 = AnnotatedVariable(
            float, 0.0, docstring="step24 account cost", units="M$"
        )
        self.step25 = AnnotatedVariable(
            float, 0.0, docstring="step25 account cost", units="M$"
        )
        self.step27 = AnnotatedVariable(
            float, 0.0, docstring="step27 account cost", units="M$"
        )
        self.step91 = AnnotatedVariable(
            float, 0.0, docstring="step91 account cost", units="M$"
        )
        self.step92 = AnnotatedVariable(
            float, 0.0, docstring="step92 account cost", units="M$"
        )
        self.step93 = AnnotatedVariable(
            float, 0.0, docstring="step93 account cost", units="M$"
        )
        # TODO provide appropriate docstring for this variable
        self.fwblkcost = AnnotatedVariable(
            float, 0.0, docstring="account cost", units="M$"
        )

        # Scaling Properties
        self.vfi = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.vfi_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.ptherm_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.rmajor_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.rminor_star = AnnotatedVariable(float, 0.0, docstring="", units="")
        self.pth = AnnotatedVariable(float, 0.0, docstring="", units="")

    def run(self):
        """Run main costs_step subroutine."""
        self.iprint = 0
        self.costs_step()

    def output(self):
        """Run main costs_step subroutine and write output."""
        self.iprint = 1
        self.costs_step()

    def costs_step(self):
        """STEP cost accounting for a fusion power plant.

        This method performs the cost accounting for a fusion power plant.
        The direct costs are calculated based on parameters input
        from other sections of the code.

        The code is arranged in the order of the standard accounts.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        Sheffield et al. (1986), Fusion Technology, 9, 199
        Sheffield & Milora (2016), Fusion Science and Technology, 70, 14
        """
        # Fusion Island Volume as defined by Sheffield & Milora (2016)
        self.vfi = (
            constants.pi
            * (bv.r_tf_outboard_mid + 0.5 * bv.tfthko) ** 2
            * (bv.hpfu + bv.hmax + bv.tfcth)
        )
        self.pth = pv.powfmw + fwbsv.emultmw + htv.pinjwp

        # STARFIRE Reference Values
        self.vfi_star = 6.737e3  # Volume of Fusion Island (m3)
        self.ptherm_star = 4.15e3  # Thermal Power (MW)
        self.rmajor_star = 7.0  # Major Radius (m)
        self.rminor_star = self.rmajor_star / 3.6  # Minor Radius (m)

        # Output header
        if self.iprint == 1 and cv.output_costs == 1:
            title = (
                "STEP Costing Model ("
                + f2py_compatible_to_string(cv.step_currency)
                + ")"
            )
            po.oheadr(self.outfile, title.strip())

        # Account 20 : Land and Rights
        self.step_a20()

        # Account 21 : Building and Site Service Infrastructure
        self.step_a21()

        # Account 22 : Reactor Plant Equipment
        self.step_a22()

        # Account 23 : Turbine Plant Equipment
        self.step_a23()

        # Account 24 : Electric Plant Equipment
        self.step_a24()

        # Account 25 : Miscellaneous Plant Equipment
        self.step_a25()

        # Total plant direct cost without remote handling
        cv.cdirt = (
            self.step20
            + self.step21
            + self.step22
            + self.step23
            + self.step24
            + self.step25
        )

        # Account 27 : Remote Handling
        self.step_a27()

        # Total plant direct cost with remote handling
        cv.cdirt = cv.cdirt + self.step27
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Plant Direct Cost")
            po.ocosts(self.outfile, "(cdirt)", "Plant direct cost (M$)", cv.cdirt)

        # Accounts 91-93: Indirect costs
        self.step_indirect_costs()

        # Constructed cost
        cv.concost = cv.cdirt + self.step91 + self.step92 + self.step93
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Constructed Cost")
            po.ocosts(self.outfile, "(concost)", "Constructed Cost (M$)", cv.concost)

        # Cost of electricity
        if cv.ireactor == 1 and cv.ipnet == 0:
            self.coelc_step()

    def step_a20(self):
        """Account 20 : Land and Rights
        author: S I Muldrew, CCFE, Culham Science Centre
        This method evaluates the Account 20 (Land and Rights)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # 20.01 Land
        # Fixed site cost (2017 M$); read from input, default = 100 M$
        step2001 = cv.sitecost / 1e6

        # step20 should be 0 at this point so I have removed
        # step20 = step20 + step2001
        self.step20 = step2001

        # 20.02 Site Preparation
        # Original STARFIRE value
        step2002 = cv.step_ref[1]
        self.step20 += step2002

        # Write output
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "20. Land and Rights")
            po.ocosts(self.outfile, "(step2001)", "Land (M$)", step2001)
            po.ocosts(self.outfile, "(step2002)", "Site Preparation (M$)", step2002)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step20)", "Total Account 20 Cost (M$)", self.step20
            )

    def step_a21(self):
        """Account 21 : Building and Site Service Infrastructure.
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 21 (Building and Site
        Service Infrastructure) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # TODO Add reference for STEP cost values
        # Floor areas in m^2 for buildings
        # pgrossmw is gross electric power of the plant in MW

        # 21.01 Site Improvements
        # Original STARFIRE value
        step2101 = self.step_a2101()
        self.step21 = step2101

        # 21.02 Reactor Building
        step2102 = 8.665e3 * bldgsv.a_reactor_bldg**1.2132 * 1.0e-6
        # * 1.0e-6 converts to M$
        self.step21 += step2102

        # 21.03 Turbine Building
        step2103 = 3.14310e5 * htv.pgrossmw * 1.0e-6
        self.step21 += step2103

        # 21.04 Cooling System Structures
        step2104 = 1.08155e5 * htv.pgrossmw * 1.0e-6
        self.step21 += step2104

        # 21.05 Electrical Equipment and Power Supply Building
        step2105 = ((4.688e3 * bldgsv.a_ee_ps_bldg) + 3.185967e6) * 1.0e-6
        self.step21 += step2105

        # 21.06 Auxiliary Services Building
        step2106 = ((3.107e3 * bldgsv.a_aux_services_bldg) + 1.206225e6) * 1.0e-6
        self.step21 += step2106

        # 21.07 Hot Cell
        step2107 = ((1.9773e4 * bldgsv.a_hot_cell_bldg) + 5.975425e6) * 1.0e-6
        self.step21 += step2107

        # 21.08 Reactor Service Building
        step2108 = ((8.563e3 * bldgsv.a_reactor_service_bldg) + 3.657324e6) * 1.0e-6
        self.step21 += step2108

        # 21.09 Service Water Building
        step2109 = ((3.288e3 * bldgsv.a_service_water_bldg) + 3.19189e5) * 1.0e-6
        self.step21 += step2109

        # 21.10 Fuel Handling and Storage Building
        step2110 = ((3.1528e4 * bldgsv.a_fuel_handling_bldg) + 9.181501e6) * 1.0e-6
        self.step21 += step2110

        # 21.11 Control Room
        step2111 = ((1.2393e4 * bldgsv.a_control_room_bldg) + 1.924890e6) * 1.0e-6
        self.step21 += step2111

        # 21.12 AC Power Supply Building
        step2112 = ((4.9755e4 * bldgsv.a_ac_ps_bldg) + 1.1591271e7) * 1.0e-6
        self.step21 += step2112

        # 21.13 Admin Building
        step2113 = ((3.417e3 * bldgsv.a_admin_bldg) + 3.017077e6) * 1.0e-6
        self.step21 += step2113

        # 21.14 Site Service
        step2114 = ((3.842e3 * bldgsv.a_site_service_bldg) + 1.193549e6) * 1.0e-6
        self.step21 += step2114

        # 21.15 Cryogenics and Inert Gas Storage Building
        step2115 = ((7.031e3 * bldgsv.a_cryo_inert_gas_bldg) + 8.19004e5) * 1.0e-6
        self.step21 += step2115

        # 21.16 Security Building
        step2116 = ((3.227e3 * bldgsv.a_security_bldg) + 2.06804e5) * 1.0e-6
        self.step21 += step2116

        # 21.17 Ventilation Stack
        # Original STARFIRE value, scaling with thermal power
        step2117 = cv.step_ref[18] * (self.pth / self.ptherm_star) ** 0.6e0
        self.step21 += step2117

        # 21.18 Waste Facilities Buildings
        # Fixed cost (2017 M$); read from input, default = 100 M$
        step2118 = cv.wfbuilding / 1.0e6
        self.step21 += step2118

        # 21.98 Spares
        # 2% as per Atkins estimation
        step2198 = 0.02e0 * self.step21
        self.step21 += step2198

        # 21.99 Contingency
        # STARFIRE 15%
        step2199 = cv.step_con * self.step21
        self.step21 += step2199

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "21. Building and Site Service Infrastructure")
            po.ocosts(self.outfile, "(step2101)", "Site Improvements (M$)", step2101)
            po.ocosts(self.outfile, "(step2102)", "Reactor Building (M$)", step2102)
            po.ocosts(self.outfile, "(step2103)", "Turbine Building (M$)", step2103)
            po.ocosts(
                self.outfile, "(step2104)", "Cooling System Structures (M$)", step2104
            )
            po.ocosts(
                self.outfile,
                "(step2105)",
                "Electrical Equipment and Power Supply Building (M$)",
                step2105,
            )
            po.ocosts(
                self.outfile, "(step2106)", "Auxiliary Services Building (M$)", step2106
            )
            po.ocosts(self.outfile, "(step2107)", "Hot Cell (M$)", step2107)
            po.ocosts(
                self.outfile, "(step2108)", "Reactor Service Building (M$)", step2108
            )
            po.ocosts(
                self.outfile, "(step2109)", "Service Water Building (M$)", step2109
            )
            po.ocosts(
                self.outfile,
                "(step2110)",
                "Fuel Handling and Storage Building (M$)",
                step2110,
            )
            po.ocosts(self.outfile, "(step2111)", "Control Room (M$)", step2111)
            po.ocosts(
                self.outfile, "(step2112)", "AC Power Supply Building (M$)", step2112
            )
            po.ocosts(self.outfile, "(step2113)", "Admin Building (M$)", step2113)
            po.ocosts(self.outfile, "(step2114)", "Site Service (M$)", step2114)
            po.ocosts(
                self.outfile,
                "(step2115)",
                "Cryogenics and Inert Gas Storage Building (M$)",
                step2115,
            )
            po.ocosts(self.outfile, "(step2116)", "Security Building (M$)", step2116)
            po.ocosts(self.outfile, "(step2117)", "Ventilation Stack (M$)", step2117)
            po.ocosts(
                self.outfile, "(step2118)", "Waste Facilities Buildings (M$)", step2118
            )
            po.ocosts(self.outfile, "(step2198)", "Spares (M$)", step2198)
            po.ocosts(self.outfile, "(step2199)", "Contingency (M$)", step2199)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step21)", "Total Account 21 Cost (M$)", self.step21
            )

    def step_a2101(self):
        """Account 21.01 : Site Improvements
        Author: R Chapman, UKAEA
        This routine evaluates the Account 21.01 (Site Improvements) costs,
        following defined Cost Breakdown Structure.
        Accounts are a mixture of unit costings that scale with site area
        and fixed provisional allowances, for which more precise costings
        should be completed following site selection.
        """
        # Initialise total Site Improvement costs
        step2101 = 0.0e0

        # 21.01.01 Site Improvements - general

        # 21.01.01.01 Grading to surface level
        # Movement of existing material around site to facilitate construction activities
        # (cost per m2) * (site area)
        step21010101 = cv.site_imp_uc[0] * cv.whole_site_area
        step210101 = step21010101

        # 21.01.01.02 General Excavation
        # Excavation up to actual construction depth including transport on site
        # and double handling material; assumed 1m depth excavation
        # (cost per m2) * (site area)
        step21010102 = cv.site_imp_uc[1] * cv.whole_site_area
        step210101 += step21010102

        # 21.01.01.03 Soil treatment
        # Soil treatment; remediation and contamination
        # Provisional allowance only; site specific
        # (cost per m2) * (site area)
        step21010103 = cv.site_imp_uc[2] * cv.whole_site_area
        step210101 += step21010103

        # 21.01.01.04 Filling & backfill
        # Filling and backfill site-won material and imported granular fill;
        # asssumed 2m deep average
        # (cost per m3) * (site area) * (2m depth)
        step21010104 = cv.site_imp_uc[3] * cv.whole_site_area * 2.0e0
        step210101 += step21010104

        # 21.01.01.05 Disposal
        # Provisional allowance only; site specific; fixed cost
        # 21.01.01.05.01 Allowance for disposal offsite, non-hazardous contaminated material
        step2101010501 = cv.site_imp_uc[4]
        # 21.01.01.05.02 Allowance for disposal offsite, hazardous contaminated material
        step2101010502 = cv.site_imp_uc[5]
        # 21.01.01.05.03 Disposal off site inert and contaminated soil in excavation below
        # actual construction depth as additional settlement mitigation
        step2101010503 = cv.site_imp_uc[6]
        step21010105 = step2101010501 + step2101010502 + step2101010503
        step210101 += step21010105

        # 21.01.01.06 Site Clearance
        # (cost per m2) * (site area)
        step21010106 = cv.site_imp_uc[7] * cv.whole_site_area
        step210101 += step21010106

        # 21.01.01.07 Removal of Trees & Vegetation
        # Provisional allowance only; site specific
        # 21.01.01.07.01 Vegetation & tree clearance
        # (cost per m2) * (site area) * (proportion of site)
        step2101010701 = cv.site_imp_uc[8] * cv.whole_site_area * 0.2e0
        # 21.01.01.07.02 Allowance for removal of trees
        step2101010702 = cv.site_imp_uc[9]
        step21010107 = step2101010701 + step2101010702
        step210101 += step21010107

        # 21.01.01.08 New Landscaping
        # (cost per m2) * (site area) * (proportion of site)
        # 21.01.01.08.01 Landscaping
        step2101010801 = cv.site_imp_uc[10] * cv.whole_site_area * 0.2e0
        # 21.01.01.08.02 Hedges
        step2101010802 = cv.site_imp_uc[11] * cv.whole_site_area * 2.3e-3
        step21010108 = step2101010801 + step2101010802
        step210101 += step21010108

        # 21.01.01.09 Constraints and Mitigation
        # Provisional allowance only; site specific
        # (cost per m2) * (site area)
        # 21.01.01.09.01 Environmental
        step2101010901 = cv.site_imp_uc[12] * cv.whole_site_area
        # 21.01.01.09.02 Unexploded Ordnance (UXO)
        step2101010902 = cv.site_imp_uc[13] * cv.whole_site_area
        # 21.01.01.09.03 Archaeological
        step2101010903 = cv.site_imp_uc[14] * cv.whole_site_area
        # 21.01.01.09.04 Local Stakeholders (including landowners and rights of way etc.)
        step2101010904 = cv.site_imp_uc[15] * cv.whole_site_area
        step21010109 = step2101010901 + step2101010902 + step2101010903 + step2101010904
        step210101 += step21010109

        # 21.01.01.10 Roads, pavements and parking areas (Main Site-wide road network)
        # Site internal access; Main road onsite including secondary roads,
        # road surface water network and parking and utilities
        # (cost per m2) * (site area)
        step21010110 = cv.site_imp_uc[16] * cv.whole_site_area
        step210101 += step21010110

        # 21.01.01.11 External Lighting
        # Street lighting columns, including 44W and 67W LED street lighting luminaires,
        # ducting, cables and excavation works, backfilling, pull pit chambers,
        # feeder pillars, earth blocks and MCCBs to lighting circuits and columns
        # (cost per m2) * (site area)
        step21010111 = cv.site_imp_uc[17] * cv.whole_site_area
        step210101 += step21010111

        # 21.01.01.12 Connection to Electricity grid (outgoing)
        # Includes transformers, busbars, cabling installation, fire protection, earthing.
        # Based upon a circa 3GW power production plant.
        step21010112 = cv.site_imp_uc[18]
        step210101 += step21010112

        # 21.01.01.13 Retaining Walls
        # Site specific; retaining walls varying height from 1.2m to 3m high.
        # Note: NOT sea defence solution.
        # (cost per m) * SQRT(site area)
        step21010113 = cv.site_imp_uc[19] * (cv.whole_site_area) ** 0.5
        step210101 += step21010113

        # 21.01.01.14 Fencing
        # Acoustic Fencing, height 5m; Construction Fence 3m height with mesh panel frame,
        # independent gate posts, post and rail type wooden fence; proposed CCTV cameras,
        # card site access control posts for vehicles, pedestrians; hostile vehicle mitigation
        # (cost per m) * SQRT(site area)
        step21010114 = cv.site_imp_uc[20] * (cv.whole_site_area) ** 0.5
        step210101 += step21010114

        # 21.01.01.15 Railings
        # Excluded (no benchmark cost data available)
        step21010115 = 0.0e0
        step210101 += step21010115

        # 21.01.01.16 Gateways
        # Excluded (no benchmark cost data available)
        step21010116 = 0.0e0
        step210101 += step21010116

        # 21.01.01.17 Sanitary Sewer System (Foul water system)
        # Foul water drainage network including 6 pumping stations (duty and standby),
        # instrumentations and controls. Assumes local water authority has sufficient
        # capacity to absorb effluent without need for additional reinforcement.
        step21010117 = cv.site_imp_uc[21]
        step210101 += step21010117

        # 21.01.01.18 Yard Drainage and Storm Sewer System (Surface water system)
        # 21.01.01.18.01 Surface water system
        # Carrier drains, filter drains, excavations, bedding, manholes, swales to
        # main areas of the site (excludes surface water to power station and buildings)
        step2101011801 = cv.site_imp_uc[22]
        # 21.01.01.18.02 Groundwater & surface water treatment buildings
        # Includes for groundwater treatment plant, surface water treatment plant,
        # surface water pumping station and connection to existing system
        step2101011802 = cv.site_imp_uc[23]
        # 21.01.01.18.03 Allowance for a single Water Management Zone
        # Provisional allowance
        step2101011803 = cv.site_imp_uc[24]
        step21010118 = step2101011801 + step2101011802 + step2101011803
        step210101 += step21010118

        # 21.01.01.19 Foundation Preparation for construction area
        # Geogrid Reinforcement, including geotextile separation layer
        # & concrete hardstanding 0.5m thick
        # (cost per m2) * (site area)
        step21010119 = cv.site_imp_uc[25] * cv.whole_site_area
        step210101 += step21010119

        # 21.01.01.20 Diaphragm Wall
        # 21.01.01.20.01 Cut off Diaphragm Wall approx. 1,000m long, 52m deep, 1.5m thick
        step2101012001 = cv.site_imp_uc[26]
        # 21.01.01.20.02 Dewatering to cut off wall
        step2101012002 = cv.site_imp_uc[27]
        step21010120 = step2101012001 + step2101012002
        step210101 += step21010120

        # Running total (M$)
        step2101 = step2101 + (step210101 / 1.0e6)

        # 21.01.02 Diversions
        # Awaiting appropriate estimate
        step210102 = 0.0e0

        # Running total (M$)
        step2101 = step2101 + (step210102 / 1.0e6)

        # 21.01.03 Relocation of Buildings & services
        # 21.01.03.01 Demolition of existing buildings
        step21010301 = cv.site_imp_uc[28]
        # 21.01.03.02 Access to site and internal transportation
        # Access road from existing highway; main road onsite & secondary roads
        step21010302 = cv.site_imp_uc[29]
        # 21.01.03.03 Incoming utilities & construction site setup
        # Includes all incoming services for entire site
        step21010303 = cv.site_imp_uc[30]
        step210103 = step21010301 + step21010302 + step21010303

        # Running total (M$)
        step2101 += step210103 / 1.0e6

        # 21.01.04 Associated developments
        step210104 = 0.0e0

        # 21.01.04.01 Park & Ride facilities
        # Includes 2457 car park spaces, external works, services, landscaping, etc.
        step21010401 = cv.site_imp_uc[31]
        step210104 += step21010401

        # 21.01.04.02 Other site facilities/activities
        # Include vehicle inspection areas, security facilities for lorry parking, etc.
        step21010402 = cv.site_imp_uc[32]
        step210104 += step21010402

        # 21.01.04.03 Entrance Plaza
        # Site prep. & ext. works: hardstandings, gantries for freight management, bus stops.
        step21010403 = cv.site_imp_uc[33]
        step210104 += step21010403

        # 21.01.04.04 Campus Accommodation
        # 21.01.04.04.01 Residential
        # Campus accommodation including canteen, shops, laundry,
        # internal recreation facilities, medical centre and sports pitches.
        step2101040401 = cv.site_imp_uc[34]
        # 21.01.04.04.02 Bus service
        # Bus station building and site maintenance and services.
        step2101040402 = cv.site_imp_uc[35]
        step210104 += step2101040401 + step2101040402

        # 21.01.04.05 Multi-storey Car parks
        step21010405 = cv.site_imp_uc[36]
        step210104 += step21010405

        # 21.01.04.06 Ancillary buildings
        # Generic costs from BCIS building rates
        # 21.01.04.06 Medical Centre​
        step2101040601 = cv.site_imp_uc[37]
        # 21.01.04.06 Public information centre
        step2101040602 = cv.site_imp_uc[38]
        # 21.01.04.06 Indoor sports and entertainment centre​
        step2101040603 = cv.site_imp_uc[39]
        step210104 += step2101040601 + step2101040602 + step2101040603

        # 21.01.04.07 Permanent configuration buildings - ancillary
        # Generic costs from BCIS building rates
        # 21.01.04.07.01 Operation staff buildings/Archive and Documentation building/Ground samples Warehouse/Occupational  Medical Centre
        step2101040701 = cv.site_imp_uc[40]
        # 21.01.04.07.02 Main site office
        step2101040702 = cv.site_imp_uc[41]
        # 21.01.04.07.03 Fire Training Centre for Plant Staff
        step2101040703 = cv.site_imp_uc[42]
        # 21.01.04.07.04 Fire fighting Water building
        step2101040704 = cv.site_imp_uc[43]
        # 21.01.04.07.05 Fire and rescue centre
        step2101040705 = cv.site_imp_uc[44]
        # 21.01.04.07.06 Administrative Office Building/ Administrative warehouse/In-site Restaurant
        step2101040706 = cv.site_imp_uc[45]
        # 21.01.04.07.07 Maintenance Staff Office
        step2101040707 = cv.site_imp_uc[46]
        # 21.01.04.07.08 Meteorological station
        step2101040708 = cv.site_imp_uc[47]
        # 21.01.04.07.09 Goods Entry Relay Store
        step2101040709 = cv.site_imp_uc[48]
        # 21.01.04.07.10 Offsite Vehicle Search Area
        step2101040710 = cv.site_imp_uc[49]
        # 21.01.04.07.11 Vehicle Inspection Cabins
        step2101040711 = cv.site_imp_uc[50]
        # 21.01.04.07.12 Security search facility
        step2101040712 = cv.site_imp_uc[51]
        # 21.01.04.07.13 Service Water Pump Building
        step2101040713 = cv.site_imp_uc[52]
        # 21.01.04.07.14 Simulator Training Building
        step2101040714 = cv.site_imp_uc[53]

        step21010407 = (
            step2101040701
            + step2101040702
            + step2101040703
            + step2101040704
            + step2101040705
            + step2101040706
            + step2101040707
            + step2101040708
            + step2101040709
            + step2101040710
            + step2101040711
            + step2101040712
            + step2101040713
            + step2101040714
        )

        step210104 += step21010407

        # Running total (M$)
        step2101 = step2101 + (step210104 / 1.0e6)

        # 21.01.05 Waterfront Improvements
        # 21.01.05.01 Marine
        # 21.01.05.01.01 Cooling installations - offshore intake, discharge, fish return, heat sink.
        # (Scaling used relates to thermal power of Hinckley C)
        step2101050101 = cv.site_imp_uc[54] * htv.pthermmw * (0.5e0 / 4524)
        # 21.01.05.01.02 Breakwater - harbour breakwater construction
        step2101050102 = cv.site_imp_uc[55]
        step21010501 = step2101050101 + step2101050102
        # 21.01.05.02 Material Offloading Facility
        # Marine transportation - jetty complete with conveyor (inc. dismantling at project end)
        step21010502 = cv.site_imp_uc[56]
        # 21.01.05.03 Sea Defence
        # Armour and mass fill to increase height of existing defences in line with flood analysis.
        step21010503 = cv.site_imp_uc[57]
        # 21.01.05.04 Other Waterfront Improvements
        # Excluded (no benchmark cost data available)
        step21010504 = 0.0e0

        step210105 = step21010501 + step21010502 + step21010503 + step21010504

        # Running total (M$)
        step2101 += step210105 / 1.0e6

        # 21.01.06 Reinstatement & Landscaping
        # 21.01.06.01 Reinstatement of campus
        step21010601 = cv.site_imp_uc[58]
        # 21.01.06.02 Reinstatement of construction plot area
        step21010602 = cv.site_imp_uc[59]
        step210106 = step21010601 + step21010602

        # Running total (M$)
        step2101 += step210106 / 1.0e6

        # 21.01.07 Transport access
        # 21.01.07.01 Highway Access - offsite highway improvements
        # Includes new roads & junctions, widening, upgrade, etc.
        step21010701 = cv.site_imp_uc[60]
        # 21.01.07.02 Rail Access
        # Includes new track and associated provisions, interface with existing rail
        step21010702 = cv.site_imp_uc[61]
        # 21.01.07.03 Air Access
        # Excluded (no benchmark cost data available)
        step21010703 = 0.0e0
        step210107 = step21010701 + step21010702 + step21010703

        # Running total (M$)
        step2101 += step210107 / 1.0e6

        return step2101

    def step_a22(self):
        """Account 22 : Reactor Plant Equipment.

        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22 (Reactor Plant Equipment)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "22. Reactor Plant Equipment")

        # Account 22.01 : Reactor Equipment
        step2201, spares = self.step_a2201()
        self.step22 = step2201
        step2298 = spares

        #  Account 22.02 : Heat Transfer Systems
        self.step22 += self.step_a2202()

        #  Account 22.03 : Cryogenic Cooling System
        self.step22 += self.step_a2203()

        #  Account 22.04 : Waste Treatment and Disposal
        self.step22 += self.step_a2204()

        #  Account 22.05 : Fuel Handling and Storage
        step2205, spares = self.step_a2205()
        self.step22 += step2205
        step2298 += spares

        #  Account 22.06 : Other Reactor Plant Equipment
        step2206, spares = self.step_a2206()
        self.step22 += step2206
        step2298 += spares

        #  Account 22.07 : Instrumentation and Control
        self.step22 += self.step_a2207()

        # 22.98 Spares
        # STARFIRE percentage of components
        self.step22 += step2298

        # 21.99 Contingency
        # STARFIRE 15%
        step2299 = cv.step_con * self.step22
        self.step22 += step2299

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.ostars(self.outfile, 20)
            po.ocosts(self.outfile, "(step2298)", "Spares (M$)", step2298)
            po.ocosts(self.outfile, "(step2299)", "Contingency (M$)", step2299)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step22)", "Total Account 22 Cost (M$)", self.step22
            )

    def step_a23(self):
        """Account 23 : Turbine Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 23 (Turbine Plant Equipment)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # 23.01 Turbine Generators
        # 23.02 Steam System
        # 23.04 Condensing System
        # 23.05 Feedwater Heating System
        # 23.06 Other Turbine Equipment
        # 23.07 Instrumentation and Control
        # step23a is the sum of the above accounts: total turbine system
        # cost, not treating cooling towers as part of the turbine system
        step23a = 5.55440e5 * htv.pgrossmw * 1.0e-6
        self.step23 = step23a

        # 23.03 Heat Rejection
        step2303 = ((8.0437e4 * htv.pgrossmw) + 2.2264895e7) * 1.0e-6
        self.step23 += step2303

        # 23.98 Spares
        # STARFIRE percentage
        step2398 = 1.401e-2 * self.step23
        self.step23 += step2398

        # 23.99 Contingency
        # STARFIRE 15%
        step2399 = cv.step_con * self.step23
        self.step23 += step2399

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "23. Turbine Plant Equipment")
            po.ocosts(self.outfile, "(step23a)", "Turbine System (M$)", step23a)
            po.ocosts(self.outfile, "(step2303)", "Heat Rejection (M$)", step2303)
            po.ocosts(self.outfile, "(step2398)", "Spares (M$)", step2398)
            po.ocosts(self.outfile, "(step2399)", "Contingency (M$)", step2399)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step23)", "Total Account 23 Cost (M$)", self.step23
            )

    def step_a24(self):
        """Account 24 : Electric Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 24 (Electric Plant
        Equipment) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # 24.01 Switch Gear
        step2401 = 1.8906e4 * htv.pgrossmw * 1.0e-6
        self.step24 = step2401

        # 24.02 Station Service Equipment
        step2402 = 5.1412e4 * htv.pgrossmw * 1.0e-6
        self.step24 += step2402

        # 24.03 Switchboards
        step2403 = 2.985e3 * htv.pgrossmw * 1.0e-6
        self.step24 += step2403

        # 24.04 Protective Equipment
        step2404 = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 18.0e0)
            + (4.0e6 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.step24 = self.step24 + step2404

        # 24.05 Electrical Structures
        step2405 = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 1.3e2)
            + (4.0e6 * 9.0e0 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.step24 = self.step24 + step2405

        # 24.06 Power and Control Wiring
        step2406 = 2.8989e4 * htv.pgrossmw * 1.0e-6
        self.step24 += step2406

        # 24.07 Electric Lighting
        step2407 = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 2.0e2)
            + (4.0e6 * 4.0e0 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.step24 += step2407

        # 24.98 Spares
        # STARFIRE percentage
        step2498 = 1.0403e-2 * self.step24
        self.step24 += step2498

        # 24.99 Contingency
        # STARFIRE 15%
        step2499 = cv.step_con * self.step24
        self.step24 += step2499

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "24. Electric Plant Equipment")
            po.ocosts(self.outfile, "(step2401)", "Switch Gear (M$)", step2401)
            po.ocosts(
                self.outfile, "(step2402)", "Station Service Equipment (M$)", step2402
            )
            po.ocosts(self.outfile, "(step2403)", "Switchboards (M$)", step2403)
            po.ocosts(self.outfile, "(step2404)", "Protective Equipment (M$)", step2404)
            po.ocosts(
                self.outfile, "(step2405)", "Electrical Structures (M$)", step2405
            )
            po.ocosts(
                self.outfile, "(step2406)", "Power and Control Wiring (M$)", step2406
            )
            po.ocosts(self.outfile, "(step2407)", "Electric Lighting (M$)", step2407)
            po.ocosts(self.outfile, "(step2498)", "Spares (M$)", step2498)
            po.ocosts(self.outfile, "(step2499)", "Contingency (M$)", step2499)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step24)", "Total Account 24 Cost (M$)", self.step24
            )

    def step_a25(self):
        """Account 25 : Miscellaneous Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 25 (Miscellaneous Plant
        Equipment) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # TODO Need to add reference for cost calculations

        # 25.01 Transport and Lifting Equipment
        step2501 = ((3.8005e4 * (bldgsv.wgt / 1.0e3)) + 1.529727e6) * 1.0e-6
        # wgt is reactor building crane capacity (kg)
        # #TODO Check that wgt is the correct variable to use here
        self.step25 = step2501

        # 25.02 Air and Water Service System
        step2502 = 1.20689e5 * htv.pgrossmw * 1.0e-6
        self.step25 += step2502

        # 25.03 Communications Equipment
        step2503 = (
            (3.05e4 * (htv.pgrossmw / 1.2e3) * 2.18e2)
            + (4.0e6 * 3.0e0 * (htv.pgrossmw / 1.2e3))
        ) * 1.0e-6
        self.step25 += step2503

        # 25.04 Furnishing and Fixtures
        step2504 = 3.0e3 * htv.pgrossmw * 1.0e-6
        self.step25 += step2504

        # 25.98 Spares
        # Original STARFIRE value, no scaling
        step2598 = 1.286e-2 * self.step25
        self.step25 += step2598

        # 25.99 Contingency
        # STARFIRE 15%
        step2599 = cv.step_con * self.step25
        self.step25 += step2599

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "25. Miscellaneous Plant Equipment")
            po.ocosts(
                self.outfile,
                "(step2501)",
                "Transport and Lifting Equipment (M$)",
                step2501,
            )
            po.ocosts(
                self.outfile,
                "(step2502)",
                "Air and Water Service System (M$)",
                step2502,
            )
            po.ocosts(
                self.outfile, "(step2503)", "Communications Equipment (M$)", step2503
            )
            po.ocosts(
                self.outfile, "(step2504)", "Furnishing and Fixtures (M$)", step2504
            )
            po.ocosts(self.outfile, "(step2598)", "Spares (M$)", step2598)
            po.ocosts(self.outfile, "(step2599)", "Contingency (M$)", step2599)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step25)", "Total Account 25 Cost (M$)", self.step25
            )

    def step_a27(self):
        """Account 27 : Remote Handling
        author: A J Pearce, CCFE, Culham Science Centre
        This routine evaluates the Account 27 (Remote Handling)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # 27.01 Remote Handling
        # From report by T. Hender CD-STEP-01030, scales with direct capital costs
        step2701 = cv.step_rh_costfrac * cv.cdirt

        self.step27 = step2701

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "27. Remote Handling")
            po.ocosts(self.outfile, "(step2701)", "Remote Handing (M$)", step2701)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step27)", "Total Account 27 Cost (M$)", self.step27
            )

    def step_indirect_costs(self):
        """Accounts 91-93: Indirect costs
        Calculate the indirect costs and print
        """
        # Account 91 : Construction Facilities, Equipment and Services (default 30%)
        self.step91 = cv.step91_per * cv.cdirt

        # Account 92 : Engineering and Costruction Management Services (default 32.5%)
        self.step92 = cv.step92_per * cv.cdirt

        # Account 93 : Other Costs (default 5%)
        self.step93 = cv.step93_per * cv.cdirt

        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Indirect Cost")
            po.ocosts(
                self.outfile,
                "(step91)",
                "Construction Facilities, Equipment and Services (M$)",
                self.step91,
            )
            po.ocosts(
                self.outfile,
                "(step92)",
                "Engineering and Costruction Management Services (M$)",
                self.step92,
            )
            po.ocosts(self.outfile, "(step93)", "Other Costs (M$)", self.step93)

    def coelc_step(self):
        """Routine to calculate the cost of electricity for a fusion power plant
        author: S I Muldrew,  CCFE, Culham Science Centre
        This routine performs the calculation of the cost of electricity
        for a fusion power plant.
        Annual costs are in megadollars/year, electricity costs are in
        millidollars/kWh, while other costs are in megadollars.
        All values are based on 1980 dollars.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """

        # Number of kWh generated each year
        kwhpy = (
            1.0e3
            * htv.pnetelmw
            * (24.0e0 * constants.n_day_year)
            * cv.cfactr
            * tv.tburn
            / tv.tcycle
        )

        # Costs due to reactor plant
        # ==========================

        # Interest on construction costs
        cv.moneyint = cv.concost * (cv.fcap0 - 1.0e0)

        # Capital costs
        cv.capcost = cv.concost + cv.moneyint

        # Annual cost of plant capital cost
        anncap = cv.capcost * cv.fcr0

        # Cost of electricity due to plant capital cost
        cv.coecap = 1.0e9 * anncap / kwhpy

        # Costs due to first wall and blanket renewal
        # ===========================================

        # Operational life
        fwbllife = fwbsv.bktlife

        # Compound interest factor
        feffwbl = (1.0e0 + cv.discount_rate) ** fwbllife

        # Capital recovery factor
        crffwbl = (feffwbl * cv.discount_rate) / (feffwbl - 1.0e0)

        # Annual cost of replacements
        annfwbl = self.fwblkcost * cv.fcap0cp * crffwbl

        # Cost of electricity due to first wall/blanket replacements
        coefwbl = 1.0e9 * annfwbl / kwhpy

        # Costs due to divertor renewal
        # =============================

        # Compound interest factor
        fefdiv = (1.0e0 + cv.discount_rate) ** cv.divlife

        # Capital recovery factor
        crfdiv = (fefdiv * cv.discount_rate) / (fefdiv - 1.0e0)

        # Annual cost of replacements
        anndiv = cv.divcst * cv.fcap0cp * crfdiv

        # Cost of electricity due to divertor replacements
        coediv = 1.0e9 * anndiv / kwhpy

        # Costs due to centrepost renewal
        # ===============================

        if pv.itart == 1:
            # Compound interest factor
            fefcp = (1.0e0 + cv.discount_rate) ** cv.cplife

            # Capital recovery factor
            crfcp = (fefcp * cv.discount_rate) / (fefcp - 1.0e0)

            # Annual cost of replacements
            anncp = cv.cpstcst * cv.fcap0cp * crfcp

            # Cost of electricity due to centrepost replacements
            coecp = 1.0e9 * anncp / kwhpy
        else:
            anncp = 0.0e0
            coecp = 0.0e0

        # Costs due to partial current drive system renewal
        # =================================================

        # Compound interest factor
        fefcdr = (1.0e0 + cv.discount_rate) ** cv.cdrlife

        # Capital recovery factor
        crfcdr = (fefcdr * cv.discount_rate) / (fefcdr - 1.0e0)

        # Annual cost of replacements
        if cv.ifueltyp == 0:
            anncdr = 0.0e0
        else:
            anncdr = cv.cdcost * cv.fcdfuel / (1.0e0 - cv.fcdfuel) * cv.fcap0cp * crfcdr

        # Cost of electricity due to current drive system replacements
        coecdr = 1.0e9 * anncdr / kwhpy

        # Costs due to operation and maintenance
        # ======================================

        # Annual cost of operation and maintenance
        annoam = cv.step_ucoam * (htv.pnetelmw / 1200.0e0) ** 0.5

        # Additional cost due to pulsed reactor thermal storage
        # See F/MPE/MOD/CAG/PROCESS/PULSE/0008

        # if (lpulse.eq.1) then
        #   if (istore.eq.1) then
        #     annoam1 = 51.0e0
        #   else if (istore.eq.2) then
        #     annoam1 = 22.2e0
        #   else:
        #     continue
        #   end if

        #   Scale with net electric power
        #   annoam1 = annoam1 * pnetelmw/1200.0e0

        #   It is necessary to convert from 1992 pounds to 1990 dollars
        #   Reasonable guess for the exchange rate + inflation factor
        #   inflation = 5% per annum; exchange rate = 1.5 dollars per pound

        #   annoam1 = annoam1 * 1.36e0

        #   annoam = annoam + annoam1

        #  end if

        #  Cost of electricity due to operation and maintenance
        cv.coeoam = 1.0e9 * annoam / kwhpy

        #  Costs due to reactor fuel
        #  =========================

        #  Annual cost of fuel

        #  Sum D-T fuel cost and He3 fuel cost
        annfuel = (
            cv.ucfuel * htv.pnetelmw / 1200.0e0
            + 1.0e-6
            * pv.fhe3
            * pv.wtgpd
            * 1.0e-3
            * cv.uche3
            * constants.n_day_year
            * cv.cfactr
        )

        #  Cost of electricity due to reactor fuel
        coefuel = 1.0e9 * annfuel / kwhpy

        #  Costs due to waste disposal
        #  ===========================

        #  Annual cost of waste disposal
        annwst = cv.step_ucwst * (htv.pnetelmw / 1200.0e0) ** 0.5

        #  Cost of electricity due to waste disposal
        coewst = 1.0e9 * annwst / kwhpy

        #  Costs due to decommissioning fund
        #  =================================

        #  Annual contributions to fund for decommissioning
        #  A fraction decomf of the construction cost is set aside for
        #  this purpose at the start of the plant life.
        #  Final factor takes into account inflation over the plant lifetime
        #  (suggested by Tim Hender 07/03/96)
        #  Difference (dintrt) between borrowing and saving interest rates is
        #  included, along with the possibility of completing the fund dtlife
        #  years before the end of the plant's lifetime
        anndecom = (
            cv.decomf
            * cv.concost
            * cv.fcr0
            / (1.0e0 + cv.discount_rate - cv.dintrt) ** (cv.tlife - cv.dtlife)
        )

        #  Cost of electricity due to decommissioning fund
        coedecom = 1.0e9 * anndecom / kwhpy

        #  Total costs
        #  ===========

        #  Annual costs due to 'fuel-like' components
        annfuelt = annfwbl + anndiv + anncdr + anncp + annfuel + annwst

        #  Total cost of electricity due to 'fuel-like' components
        cv.coefuelt = coefwbl + coediv + coecdr + coecp + coefuel + coewst

        #  Total annual costs
        anntot = anncap + annfuelt + annoam + anndecom

        #  Total cost of electricity
        cv.coe = cv.coecap + cv.coefuelt + cv.coeoam + coedecom

        if self.iprint != 0 and cv.output_costs != 0:
            # Output section
            po.oshead(self.outfile, "Interest during Construction")
            po.ocosts(
                self.outfile,
                "(moneyint)",
                "Interest during construction (M$)",
                cv.moneyint,
            )
            po.oshead(self.outfile, "Total Capital Investment")
            po.ocosts(
                self.outfile, "(capcost)", "Total capital investment (M$)", cv.capcost
            )

            title = "Cost of Electricity, " + f2py_compatible_to_string(
                cv.step_currency
            )
            po.oheadr(self.outfile, title)

            po.ovarrf(
                self.outfile,
                "First wall / blanket life (years)",
                "(fwbllife)",
                fwbllife,
            )
            po.ovarrf(self.outfile, "Divertor life (years)", "(divlife.)", cv.divlife)
            if pv.itart == 1:
                po.ovarrf(
                    self.outfile, "Centrepost life (years)", "(cplife.)", cv.cplife
                )

            po.ovarrf(self.outfile, "Cost of electricity (m$/kWh)", "(coe)", cv.coe)
            po.osubhd(self.outfile, "Power Generation Costs :")

            if annfwbl != annfwbl or annfwbl > 1.0e10 or annfwbl < 0.0:
                po.ocmmnt(self.outfile, "Problem with annfwbl")
                po.ocmmnt(self.outfile, "fwblkcost=", cv.fwallcst)
                po.ocmmnt(self.outfile, "crffwbl=", crffwbl, "  fcap0cp=", cv.fcap0cp)
                po.ocmmnt(self.outfile, "feffwbl=", feffwbl, "  fwbllife=", fwbllife)

            po.write(
                self.outfile,
                "\t" * 9 + "Annual Costs, M$" + "\t" * 1 + " " * 4 + "COE, m$/kWh",
            )
            po.dblcol(self.outfile, "Capital Investment", anncap, cv.coecap)
            po.dblcol(self.outfile, "Operation & Maintenance", annoam, cv.coeoam)
            po.dblcol(self.outfile, "Decommissioning Fund", anndecom, coedecom)
            po.write(self.outfile, "Fuel Charge Breakdown")
            po.dblcol(self.outfile, "\tBlanket & first wall", annfwbl, coefwbl)
            po.dblcol(self.outfile, "\tDivertors", anndiv, coediv)
            po.dblcol(self.outfile, "\tCentrepost (TART only)", anncp, coecp)
            po.dblcol(self.outfile, "\tAuxiliary Heating", anncdr, coecdr)
            po.dblcol(self.outfile, "\tActual Fuel", annfuel, coefuel)
            po.dblcol(self.outfile, "\tWaste Disposal", annwst, coewst)
            po.dblcol(self.outfile, "Total Fuel Cost", annfuelt, cv.coefuelt)
            po.dblcol(self.outfile, "Total Cost", anntot, cv.coe)

            if cv.ifueltyp == 1:
                po.oshead(self.outfile, "Replaceable Components Direct Capital Cost")
                po.ovarrf(
                    self.outfile,
                    "First wall and Blanket direct capital cost (M$)",
                    "(fwblkcost)",
                    self.fwblkcost,
                )
                po.ovarrf(
                    self.outfile,
                    "Divertor direct capital cost (M$)",
                    "(divcst)",
                    cv.divcst,
                )
                if pv.itart == 1:
                    po.ovarrf(
                        self.outfile,
                        "Centrepost direct capital cost (M$)",
                        "(cpstcst)",
                        cv.cpstcst,
                    )

                po.ovarrf(
                    self.outfile,
                    "Plasma heating/CD system cap cost (M$)",
                    "",
                    cv.cdcost * cv.fcdfuel / (1.0e0 - cv.fcdfuel),
                )
                po.ovarrf(
                    self.outfile,
                    "Fraction of CD cost --> fuel cost",
                    "(fcdfuel)",
                    cv.fcdfuel,
                )

    def step_a2201(self):
        """Account 22.01 : Reactor Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 22.01 (Reactor Equipment)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: 2201 cost and spares
        :rtype: tuple[float, float]
        """
        (
            step220101,
            step22010101,
            step22010102,
            step2201010201,
            step2201010202,
            step2201010203,
        ) = self.step_a220101()

        step220102 = self.step_a220102()

        step22010301 = self.step_a22010301()
        step22010302 = self.step_a22010302()
        step220104 = self.step_a220104()

        # 22.01.01 Blanket and First Wall
        step2201 = step220101

        # 22.01.02 Shield
        # Inboard shield costs:
        # Note: outboard shield costs currently set to zero.
        # Add shield cost to total cost, step2201, in M$
        step2201 += step220102
        # STARFIRE percentage for spares
        spares = 9.985e-2 * step220102

        # 22.01.03.01 TF Coils
        # Add TF coil cost to total cost, step2201, in M$
        step2201 += step22010301

        # 22.01.03.02 PF Coils
        step2201 += step22010302
        # STARFIRE percentage for spares
        spares += 3.269e-1 * step22010302

        # 22.01.03.03 Central Solenoid
        # Original STARFIRE value, scaling with fusion island volume
        step22010303 = cv.step_ref[23] * (self.vfi / self.vfi_star)
        step2201 += step22010303
        # STARFIRE percentage for spares
        spares += 6.124e-1 * step22010303

        # 22.01.03.04 Control Coils
        # Original STARFIRE value, scaling with fusion island volume
        step22010304 = cv.step_ref[24] * (self.vfi / self.vfi_star)
        step2201 += step22010304
        # STARFIRE percentage for spares
        spares += 1.075e-1 * step22010304

        # 22.01.04 Auxiliary Heating and Current Drive
        # HCD cost = cost per injected Watt of power * injected Watts
        step2201 += step220104
        # STARFIRE percentage for spares
        spares += 2.335e-1 * step220104

        # 22.01.05 Primary Structure and Support
        # Original STARFIRE value, scaling with fusion island volume
        step220105 = cv.step_ref[26] * (self.vfi / self.vfi_star)
        step2201 += step220105
        # STARFIRE percentage for spares
        spares += 6.824e-2 * step220105

        # 22.01.06 Reactor Vacuum System
        # Original STARFIRE value, scaling with fusion island volume
        step220106 = cv.step_ref[27] * (self.vfi / self.vfi_star) ** (2.0e0 / 3.0e0)
        step2201 += step220106
        # STARFIRE percentage for spares
        spares += 1.893e-1 * step220106

        # 22.01.07 Power Supplies
        # Original STARFIRE value, scaling with fusion island volume
        step220107 = cv.step_ref[28] * (self.vfi / self.vfi_star) ** (2.0e0 / 3.0e0)
        step2201 += step220107

        # 22.01.08 Impurity Control
        # Superseded and deleted

        # 22.01.09 ECRH Plasma Breakdown
        # Superseded and deleted

        # 22.01.10 Divertor
        # Cost Model 0 cost for STARFIRE sized device
        # 58.62% increase between 1980 and 1990
        # http://www.in2013dollars.com/1980-dollars-in-1990
        # Scaling with product of rmajor and rminor
        step220110 = cv.step_ref[31] * (
            (pv.rmajor * pv.rminor) / (self.rmajor_star * self.rminor_star)
        )
        if cv.ifueltyp == 1:
            cv.divcst = step220110
            step220110 = 0.0e0
        else:
            cv.divcst = 0.0e0

        step2201 += step220110

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.write(self.outfile, "******************* 22.01 Reactor Equipment")
            if cv.ifueltyp == 0:
                po.write(
                    self.outfile,
                    "******************* 22.01.01 Blanket and First Wall Equipment",
                )
                po.ocosts(
                    self.outfile,
                    "(step22010101)",
                    "Total First Wall Cost (M$)",
                    step22010101,
                )
                po.oblnkl(self.outfile)
                po.ocosts(
                    self.outfile,
                    "(step2201010201)",
                    "Blanket Multiplier Material (M$)",
                    step2201010201,
                )
                po.ocosts(
                    self.outfile,
                    "(step2201010202)",
                    "Blanket Breeder Material (M$)",
                    step2201010202,
                )
                po.ocosts(
                    self.outfile,
                    "(step2201010203)",
                    "Blanket Steel Costs (M$)",
                    step2201010203,
                )
                po.ocosts(
                    self.outfile,
                    "(step22010102)",
                    "Total Blanket Cost (M$)",
                    step22010102,
                )
                po.oblnkl(self.outfile)
                po.ocosts(
                    self.outfile,
                    "(step220101)",
                    "Total Account 22.01.01 Cost (M$)",
                    step220101,
                )
                po.oblnkl(self.outfile)
            elif cv.ifueltyp == 1:
                po.ocosts(
                    self.outfile,
                    "(step220101)",
                    "Blanket and First Wall (Treated as Fuel) (M$)",
                    step220101,
                )

            po.ocosts(self.outfile, "(step220102)", "Shield (M$)", step220102)
            po.ocosts(self.outfile, "(step22010301)", "TF Coils (M$)", step22010301)
            po.ocosts(self.outfile, "(step22010302)", "PF Coils (M$)", step22010302)
            po.ocosts(
                self.outfile, "(step22010303)", "Central Solenoid (M$)", step22010303
            )
            po.ocosts(
                self.outfile, "(step22010304)", "Control Coils (M$)", step22010304
            )

            if cv.ifueltyp == 0:
                po.ocosts(
                    self.outfile,
                    "(step220104)",
                    "Auxiliary Heating and Current Drive (M$)",
                    step220104,
                )
            elif cv.ifueltyp == 1:
                po.ocosts(
                    self.outfile,
                    "(step220104)",
                    "Auxiliary Heating and Current Drive (Fraction as Fuel) (M$)",
                    step220104,
                )

            po.ocosts(
                self.outfile,
                "(step220105)",
                "Primary Structure and Support (M$)",
                step220105,
            )
            po.ocosts(
                self.outfile, "(step220106)", "Reactor Vacuum System (M$)", step220106
            )
            po.ocosts(self.outfile, "(step220107)", "Power Supplies (M$)", step220107)
            po.ocosts(self.outfile, "(step220110)", "Divertor (M$)", step220110)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2201)", "Total Account 22.01 Cost (M$)", step2201
            )
            po.oblnkl(self.outfile)

        return step2201, spares

    def step_a220101(self):
        """Account 22.01.01 : Blanket and First Wall
        author: A J Pearce, CCFE, Culham Science Centre
        This routine evaluates the Account 22.01.01 (BB+FW) costs.
        If ifueltyp = 0, the blanket cost is treated as capital cost
        If ifueltyp = 1, the blanket cost is treated as a fuel cost,
        rather than as a capital cost.
        If ifueltyp = 2, the initial blanket is included as a capital cost
        and the replacement blanket costs are treated as a fuel cost.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :return: 220101 cost and sub-costs
        :rtype: tuple[float, float, float, float, float, float,]
        """

        # Account 22.01.01.01 : First wall
        step22010101 = 1.0e-6 * (
            fwbsv.fw_armour_mass * cv.step_ucfwa + fwbsv.fwmass * cv.step_ucfws
        )

        if cv.ifueltyp == 1:
            cv.fwallcst = step22010101
            step22010101 = 0.0e0
        elif cv.ifueltyp == 2:
            cv.fwallcst = step22010101
        else:
            cv.fwallcst = 0.0e0

        # Account 22.01.01.02 : Breeder Blanket

        if htv.ipowerflow == 0:

            # Account 22.01.01.02.01 : Blanket Multiplier Material
            step2201010201 = 1.0e-6 * fwbsv.whtblbe * cv.step_ucblbe

            # Account 22.01.01.02.02 : Blanket Breeder Material
            if fwbsv.blktmodel == 0:
                step2201010202 = 1.0e-6 * fwbsv.wtblli2o * cv.step_ucblbreed
            else:
                step2201010202 = 1.0e-6 * fwbsv.whtblbreed * cv.step_ucblbreed
        else:
            if fwbsv.blkttype == 1 or fwbsv.blkttype == 2:
                # Liquid blanket (LiPb + Li)
                # Account 22.01.01.02.01 : Blanket Multiplier Material
                step2201010201 = 1.0e-6 * fwbsv.wtbllipb * cv.ucbllipb * 2.99e0
                # Account 22.01.01.02.02 : Blanket Breeder Material
                step2201010202 = 1.0e-6 * fwbsv.whtblli * cv.ucblli * 2.99e0
            else:
                #  Solid blanket (Li2O + Be)
                # Account 22.01.01.02.01 : Blanket Multiplier Material
                step2201010201 = 1.0e-6 * fwbsv.whtblbe * cv.step_ucblbe
                # Account 22.01.01.02.02 : Blanket Breeder Material
                step2201010202 = 1.0e-6 * fwbsv.wtblli2o * cv.step_ucblbreed

        # Account 22.01.01.02.03 : Blanket Steel Costs
        step2201010203 = 1.0e-6 * fwbsv.whtblss * cv.step_ucblss

        # Account 22.01.01.02.04 : Blanket Vanadium Costs
        step2201010204 = 1.0e-6 * fwbsv.whtblvd * cv.step_ucblvd

        # Account 22.01.01.02.05 : Blanket Carbon Cloth Costs
        step2201010205 = 0.0e0

        # Account 22.01.01.02.06 : Blanket Concrete Costs
        step2201010206 = 0.0e0

        # Account 22.01.01.02.07 : Blanket FLiBe Costs
        step2201010207 = 0.0e0

        step22010102 = (
            step2201010201
            + step2201010202
            + step2201010203
            + step2201010204
            + step2201010205
            + step2201010206
            + step2201010207
        )

        if cv.ifueltyp == 1:
            cv.blkcst = step22010102
            step22010102 = 0.0e0
        elif cv.ifueltyp == 2:
            cv.blkcst = step22010102
        else:
            cv.blkcst = 0.0e0

        # Total for Account 22.01.01
        step220101 = step22010101 + step22010102

        return (
            step220101,
            step22010101,
            step22010102,
            step2201010201,
            step2201010202,
            step2201010203,
        )

    def step_a220102(self):
        """22.01.02 Inboard shield.
        Note: outboard shield costs currently set to zero

        :return: 220102 cost
        :rtype: float
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
        step220102 = (inb_sh_m * sh_mtl_c) / 1.0e6 * (229.0e0 / 264.71e0)

        # Note: outboard shield costs currently set to zero

        return step220102

    def step_a22010301(self):
        """22.01.03.01 TF Coils.

        :return: cost of 22010301
        :rtype: float
        """
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

        step22010301 = 0.0e0

        # Copper coils
        if tfv.i_tf_sup == 0:
            # Calculation taken from cost model 0: simply the cost of copper conductor masses.
            # Inflating from 1990 $ to 2017 $ at nuclear rate equates to a factor of 2.99.

            # Inboard TF coil legs
            c_tf_inboard_legs = 1.0e-6 * tfv.whtcp * cv.uccpcl1 * 2.99e0

            # Outboard TF coil legs
            c_tf_outboard_legs = 1.0e-6 * tfv.whttflgs * cv.uccpclb * 2.99e0

            # Total TF coil cost
            step22010301 = c_tf_inboard_legs + c_tf_outboard_legs

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
            step22010301 = ctfcontot + costtfwind + costtfcas + costtfint + costtfgss

        # Cryogenic aluminium coils
        elif tfv.i_tf_sup == 2:
            # Cost approximated as the material cost of conducting Al * a
            # manufacturing cost factor
            # Al conductor mass per coil * number of coils * cost per kilo *
            # manufacturing cost factor, converted to M$
            # step_mc_cryo_al_per = 0.2: 20% manufacturing cost
            step22010301 = (
                (tfv.whtconal * tfv.n_tf * cv.step_uc_cryo_al)
                * (cv.step_mc_cryo_al_per + 1.0e0)
                * 1.0e-6
            )

        # step22010301 = step2201030101 + step2201030102 + step2201030103

        # ifueltyp: consider centrepost cost as fuel, capital or both?
        # cpstcst used later in coelc_step()
        cv.cpstcst = 0.0e0  # TART centrepost
        if pv.itart == 1:
            if cv.ifueltyp == 1:
                # Treat centrepost cost as fuel cost
                cv.cpstcst = c_tf_inboard_legs
                if tfv.i_tf_sup == 0:
                    # Subtract from capital cost
                    step22010301 = step22010301 - c_tf_inboard_legs
            elif cv.ifueltyp == 2:
                # Treat centrepost cost as capital and fuel cost
                cv.cpstcst = c_tf_inboard_legs

        return step22010301

    def step_a22010302(self):
        """Account 22.01.03.02 PF Coils : PF magnet assemblies
        author: A J Pearce, CCFE, Culham Science Centre
        This routine evaluates the Account 22.01.03.02 (PF magnet) costs.
        Conductor costs previously used an algorithm devised by R. Hancox,
        January 1994, under contract to Culham, which took into
        account the fact that the superconductor/copper ratio in
        the conductor is proportional to the maximum field that
        each coil will experience. Now, the input copper fractions
        are used instead.
        Maximum values for current, current density and field
        are used.

        :return: cost 22010302
        :rtype: float
        """
        # Total length of PF coil windings (m)
        pfwndl = 0.0e0

        for i in range(pfv.nohc):
            pfwndl += constants.twopi * pfv.rpf[i] * pfv.turns[i]

        # Account 22.01.03.02.01 : Conductor

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

        step2201030201 = 0.0e0

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
            step2201030201 = step2201030201 + (
                1.0e-6 * constants.twopi * pfv.rpf[i] * pfv.turns[i] * cpfconpm
            )

        # Account 22.01.03.02.02 : Winding
        step2201030202 = 1.0e-6 * cv.step_ucwindpf * pfwndl

        # Account 22.01.03.02.03 : Steel case - will be zero for resistive coils
        step2201030203 = 1.0e-6 * cv.step_uccase * pfv.whtpfs

        # Account 22.01.03.02.04 : Support structure
        step2201030204 = 1.0e-6 * cv.step_ucfnc * sv.fncmass

        # Total account 22.01.03.02
        step22010302 = step2201030201 + step2201030202 + step2201030203 + step2201030204

        return step22010302

    def step_a220104(self):
        """22.01.04 Auxiliary Heating and Current Drive
        Returns cost of auxiliary HCD
        HCD cost = cost per injected Watt of power * injected Watts

        :return: cost 220104
        :rtype: float
        """
        # Cost per Watt depends on technology/hardware used;
        # inflation adjustment applied as appropriate to source for costs
        # (tech adjusted from 1990 $ is costed as per Cost Model 0)
        # Note: NBI and EC/EBW acounts will be zero if this tech is not included.

        # HCD requirements for flat-top operation

        # NBI cost per injected Watt (adjusted from 2020 $):
        step220104 = cdv.pnbitot * cv.step_ref[68] * (229.0e0 / 258.84e0)
        # (if this method is not used, result is zero)

        # EC or EBW cost per injected Watt (adjusted from 2020 $):
        step220104 += cdv.echpwr * cv.step_ref[69] * (229.0e0 / 258.84e0)
        # (if this method is not used, result is zero)

        if cdv.iefrf == 2 or cdv.iefrffix == 2:
            # if primary *or* secondary current drive efficiency model is
            # Ion Cyclotron current drive (adjusted from 1990 $):
            step220104 += 1.0e-6 * cv.ucich * (1.0e6 * cdv.plhybd) * (229.0e0 / 76.7e0)

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
            step220104 += 1.0e-6 * cv.uclh * (1.0e6 * cdv.plhybd) * (229.0e0 / 76.7e0)

        # HCD requirements for start-up and ramp-down
        cv.startuppwr = step220104 * cv.startupratio
        step220104 += cv.startuppwr

        if cv.ifueltyp == 1:
            # fraction `fcdfuel` of HCD cost treated as fuel cost
            # step220104 and cv.cdcost: Cost of HCD in M$
            step220104 *= 1.0e0 - cv.fcdfuel
            cv.cdcost = step220104

        return step220104

    def step_a2202(self):
        """Account 22.02 : Heat Transfer System
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22.02 (Heat Transfer System)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: cost 2202
        :rtype: float
        """
        # pgrossmw is gross electric power of the plant in MW
        step2202 = 9.2238e4 * htv.pgrossmw * 1.0e-6

        # Output costs
        if (self.iprint == 1) and (cv.output_costs == 1):
            po.write(self.outfile, "******************* 22.02 Heat Transfer System")
            po.ocosts(self.outfile, "(step2202)", "Heat Transfer System (M$)", step2202)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2202)", "Total Account 22.02 Cost (M$)", step2202
            )
            po.oblnkl(self.outfile)

        return step2202

    def step_a2203(self):
        """Account 22.03 : Cryogenic Cooling System
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 22.03 (Cryogenic Cooling
        System) costs.

        :return: cost 2203
        :rtype: float
        """
        # Cryoplant - will be zero for resistive coils
        # Parametric costing of cryo systems based on refrigeration capacity produced at Helium temp of 4.5K
        step2203 = 6.14e0 * tfv.cryo_cool_req**0.63

        # Output costs
        if (self.iprint == 1) and (cv.output_costs == 1):
            po.write(self.outfile, "******************* 22.03 Cryogenic Cooling System")
            po.ocosts(self.outfile, "(step2203)", "Cryoplant (M$)", step2203)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2203)", "Total Account 22.03 Cost (M$)", step2203
            )
            po.oblnkl(self.outfile)

        return step2203

    def step_a2204(self):
        """Account 22.04 : Waste Treatment and Disposal
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22.04 (Waste Treatment
        and Disposal) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: cost 2204
        :rtype: float
        """
        # 22.04.01 Liquid Waste
        # Original STARFIRE value, scaling with thermal power
        step220401 = cv.step_ref[37] * (self.pth / self.ptherm_star) ** 0.6e0
        step2204 = step220401

        # 22.04.02 Gaseous Waste
        # Original STARFIRE value, scaling with thermal power
        step220402 = cv.step_ref[38] * (self.pth / self.ptherm_star) ** 0.6e0
        step2204 += step220402

        # 22.04.03 Solid Waste
        # Original STARFIRE value, scaling with thermal power
        step220403 = cv.step_ref[39] * (self.pth / self.ptherm_star) ** 0.6e0
        step2204 += step220403

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.write(
                self.outfile, "******************* 22.04 Waste Treatment and Disposal"
            )
            po.ocosts(self.outfile, "(step220401)", "Liquid Waste (M$)", step220401)
            po.ocosts(self.outfile, "(step220402)", "Gaseous Waste (M$)", step220402)
            po.ocosts(self.outfile, "(step220403)", "Solid Waste (M$)", step220403)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2204)", "Total Account 22.04 Cost (M$)", step2204
            )
            po.oblnkl(self.outfile)

        return step2204

    def step_a2205(self):
        """Account 22.05 : Fuel Handling and Storage
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22.05 (Fuel Handling
        and Storage) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: 2205 cost and spares
        :rtype: tuple[float, float]
        """
        # 22.05 Fuel Handling and Storage
        # Original STARFIRE value, scaling with thermal power
        step2205 = cv.step_ref[40] * (self.pth / self.ptherm_star) ** 0.6e0

        # STARFIRE percentage for spares
        spares = 5.026e-2 * step2205

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.write(
                self.outfile, "******************* 22.05 Fuel Handling and Storage"
            )
            po.ocosts(
                self.outfile, "(step2205)", "Fuel Handling and Storage (M$)", step2205
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2205)", "Total Account 22.05 Cost (M$)", step2205
            )
            po.oblnkl(self.outfile)

        return step2205, spares

    def step_a2206(self):
        """Account 22.06 : Other Reactor Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22.06 (Other Reactor
        Plant Equipment) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: 2206 cost and spares
        :rtype: tuple[float, float]
        """

        # 22.06.01 Maintenance Equipment
        # Original STARFIRE value, scaling with fusion island volume
        # Depreciated by the remote handling scaling in cost account 27.
        step220601 = 0.0  # step_ref(42) * (vfi / vfi_star)**(2.0e0/3.0e0)
        step2206 = step220601
        # STARFIRE percentage for spares
        spares = 4.308e-1 * step220601

        # 22.06.02 Special Heating Systems
        # Original STARFIRE value, scaling with thermal power
        step220602 = cv.step_ref[42] * (self.pth / self.ptherm_star) ** 0.6e0
        step2206 += step220602

        # 22.06.03 Coolant Storage
        # Original STARFIRE value, scaling with thermal power
        step220603 = cv.step_ref[43] * (self.pth / self.ptherm_star) ** 0.6e0
        step2206 += step220603

        # 22.06.04 Gas System
        # Original STARFIRE value, scaling with fusion island volume
        step220604 = cv.step_ref[44] * (self.vfi / self.vfi_star) ** (2.0e0 / 3.0e0)
        step2206 += step220604

        # 22.06.05 Inert Atmosphere System
        # Original STARFIRE value, scaling with thermal power
        step220605 = cv.step_ref[45] * (self.pth / self.ptherm_star) ** 0.6e0
        step2206 += step220605

        # 22.06.06 Fluid Leak Detection
        # Original STARFIRE value, scaling with thermal power
        step220606 = cv.step_ref[46] * (self.pth / self.ptherm_star) ** 0.6e0
        step2206 += step220606

        # 22.06.07 Closed Loop Coolant System
        # Original STARFIRE value, scaling with thermal power
        step220607 = cv.step_ref[47] * (self.pth / self.ptherm_star) ** 0.6e0
        step2206 += step220607
        # STARFIRE percentage for spares
        spares += 8.3e-1 * (self.pth / self.ptherm_star) ** 0.6e0

        # 22.06.08 Standby Cooling System
        # Original STARFIRE value, scaling with thermal power
        step220608 = cv.step_ref[48] * (self.pth / self.ptherm_star) ** 0.6e0
        step2206 += step220608

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.write(
                self.outfile, "******************* 22.06 Other Reactor Plant Equipment"
            )
            po.ocosts(
                self.outfile, "(step220601)", "Maintenance Equipment (M$)", step220601
            )
            po.ocosts(
                self.outfile, "(step220602)", "Special Heating Systems (M$)", step220602
            )
            po.ocosts(self.outfile, "(step220603)", "Coolant Storage (M$)", step220603)
            po.ocosts(self.outfile, "(step220604)", "Gas System (M$)", step220604)
            # po.ocosts(self.outfile,'(step220605)','Inert Atmosphere System (M$)', step220605)
            po.ocosts(
                self.outfile, "(step220606)", "Fluid Leak Detection (M$)", step220606
            )
            po.ocosts(
                self.outfile,
                "(step220607)",
                "Closed Loop Coolant System (M$)",
                step220607,
            )
            po.ocosts(
                self.outfile, "(step220608)", "Standby Cooling System (M$)", step220608
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2206)", "Total Account 22.06 Cost (M$)", step2206
            )
            po.oblnkl(self.outfile)

        return step2206, spares

    def step_a2207(self):
        """Account 22.07 : Instrumentation and Control
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22.07 (Instrumentation
        and Control) costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: cost 2207
        :rtype: float
        """
        # 22.07 Instrumentation and Control
        # Original STARFIRE value, scaling with thermal power
        step2207 = cv.step_ref[49] * (self.pth / self.ptherm_star) ** 0.6e0

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.write(
                self.outfile, "******************* 22.07 Instrumentation and Control"
            )
            po.ocosts(
                self.outfile, "(step2207)", "Instrumentation and Control (M$)", step2207
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2207)", "Total Account 22.07 Cost (M$)", step2207
            )
            po.oblnkl(self.outfile)

        return step2207
