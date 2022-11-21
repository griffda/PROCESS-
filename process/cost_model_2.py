from process.variables import AnnotatedVariable

from process.cost_model_base import CostModelBase

from process import fortran as ft
from process.fortran import constants
from process.fortran import build_variables as bv
from process.fortran import cost_variables as cv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import process_output as po
from process.fortran import times_variables as tv
from process.utilities.f2py_string_patch import f2py_compatible_to_string


class CostModel2(CostModelBase):
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
        super().__init__()
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        self.iprint = 0  # switch for writing to output file (1=yes)

        # Various cost account values (M$)
        self.step12 = AnnotatedVariable(
            float, 0.0, docstring="step12 account cost", units="M$"
        )
        self.step13 = AnnotatedVariable(
            float, 0.0, docstring="step13 account cost", units="M$"
        )
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

    def run(self):
        """Run main cost_model_2 subroutine."""
        self.iprint = 0
        self.cost_model_2()

    def output(self):
        """Run main cost_model_2 subroutine and write output."""
        self.iprint = 1
        self.cost_model_2()
        self.print_output()

    def bldg_and_site_service_infra_costs(self):
        """Account 21 : Building and Site Service Infrastructure.
        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Building and Site
        Service Infrastructure costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # TODO Add reference for STEP cost values
        # Floor areas in m^2 for buildings
        # pgrossmw is gross electric power of the plant in MW

        # 21.01 Site Improvements
        # Original STARFIRE value
        self.site_improvements_costs()
        self.bldg_and_site_service_infra_cost = self.site_improvements_cost

        # 21.02 Reactor Building
        self.reactor_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.reactor_bldg_cost

        # 21.03 Turbine Building
        self.turbine_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.turbine_bldg_cost

        # 21.04 Cooling System Structures
        self.cooling_sys_struct_costs()
        self.bldg_and_site_service_infra_cost += self.cooling_sys_struct_cost

        # 21.05 Electrical Equipment and Power Supply Building
        self.electrical_and_power_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.electrical_and_power_bldg_cost

        # 21.06 Auxiliary Services Building
        self.aux_services_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.aux_services_bldg_cost

        # 21.07 Hot Cell
        self.hot_cell_costs()
        self.bldg_and_site_service_infra_cost += self.hot_cell_cost

        # 21.08 Reactor Service Building
        self.reactor_serv_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.reactor_serv_bldg_cost

        # 21.09 Service Water Building
        self.service_water_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.service_water_bldg_cost

        # 21.10 Fuel Handling and Storage Building
        self.fuel_handling_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.fuel_handling_bldg_cost

        # 21.11 Control Room
        self.control_room_costs()
        self.bldg_and_site_service_infra_cost += self.control_room_cost

        # 21.12 AC Power Supply Building
        self.ac_power_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.ac_power_bldg_cost

        # 21.13 Admin Building
        self.admin_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.admin_bldg_cost

        # 21.14 Site Service
        self.site_service_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.site_service_bldg_cost

        # 21.15 Cryogenics and Inert Gas Storage Building
        self.cryo_and_inert_gas_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.cryo_and_inert_gas_bldg_cost

        # 21.16 Security Building
        self.security_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.security_bldg_cost

        # 21.17 Ventilation Stack
        self.vent_stack_costs()
        self.bldg_and_site_service_infra_cost += self.vent_stack_cost

        # 21.18 Waste Facilities Buildings
        self.waste_fac_bldg_costs()
        self.bldg_and_site_service_infra_cost += self.waste_fac_bldg_cost

        # 21.98 Spares
        # 2% as per Atkins estimation
        self.spares = 0.02e0 * self.bldg_and_site_service_infra_cost
        self.bldg_and_site_service_infra_cost += self.spares

        # 21.99 Contingency
        # STARFIRE 15%
        self.contingency = cv.step_con * self.bldg_and_site_service_infra_cost
        self.bldg_and_site_service_infra_cost += self.contingency

    def reactor_plant_equip_costs(self):
        """Account 22 : Reactor Plant Equipment.

        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Reactor Plant Equipment costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        # 22.01 Reactor Equipment
        self.reactor_equip_cost, self.spares = self.reactor_equip_costs()
        self.reactor_plant_equip_cost = self.reactor_equip_cost
        self.spares_total = self.spares

        # 22.02 Heat Transfer Systems
        self.heat_transfer_sys_costs()
        self.reactor_plant_equip_cost += self.heat_transfer_sys_cost

        # 22.03 Cryogenic Cooling System
        self.cryo_sys_costs()
        self.reactor_plant_equip_cost += self.cryo_sys_cost

        # 22.04 Waste Treatment and Disposal
        self.waste_disposal_costs()
        self.reactor_plant_equip_cost += self.waste_disposal_cost

        # 22.05 Fuel Handling and Storage
        self.fuel_handling_cost, self.spares = self.fuel_handling_costs()
        self.reactor_plant_equip_cost += self.fuel_handling_cost
        self.spares_total += self.spares

        # 22.06 Other Reactor Plant Equipment
        self.other_reactor_equip_cost, self.spares = self.other_reactor_equip_costs()
        self.reactor_plant_equip_cost += self.other_reactor_equip_cost
        self.spares_total += self.spares

        # 22.07 Instrumentation and Control
        self.instrument_and_control_costs()
        self.reactor_plant_equip_cost += self.instrument_and_control_cost

        # 22.98 Spares
        # STARFIRE percentage of components
        self.reactor_plant_equip_cost += self.spares_total

        # 22.99 Contingency
        # STARFIRE 15%
        self.contingency = cv.step_con * self.reactor_plant_equip_cost
        self.reactor_plant_equip_cost += self.contingency

    def reactor_equip_costs(self):
        """Account 22.01 Reactor Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Reactor Equipment costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: 2201 cost and self.spares
        :rtype: tuple[float, float]
        """
        (
            self.fw_blk_total_cost,
            self.fw_cost,
            self.blk_total_cost,
            self.blk_multi_mat_cost,
            self.blk_breed_mat_cost,
            self.blk_steel_cost,
        ) = self.blanket_first_wall_costs()

        self.ib_shield_costs()

        self.tf_coils_costs()
        self.pf_coils_costs()
        self.central_sol_costs()
        self.control_coils_costs()
        self.hcd_costs()
        self.primary_structure_costs()
        self.reactor_vacuum_sys_costs()
        self.power_supplies_costs()
        self.divertor_costs()

        # 22.01.01 Blanket and First Wall
        self.reactor_equip_cost = self.fw_blk_total_cost

        # 22.01.02 Shield
        # Inboard shield costs:
        # Note: outboard shield costs currently set to zero.
        # Add shield cost to total cost, self.reactor_equip_cost, in M$
        self.reactor_equip_cost += self.ib_shield_cost
        # STARFIRE percentage for self.spares
        self.spares = 9.985e-2 * self.ib_shield_cost

        # 22.01.03.01 TF Coils
        # Add TF coil cost to total cost, self.reactor_equip_cost, in M$
        self.reactor_equip_cost += self.tf_coils_cost

        # 22.01.03.02 PF Coils
        self.reactor_equip_cost += self.pf_total_cost
        # STARFIRE percentage for self.spares
        self.spares += 3.269e-1 * self.pf_total_cost

        # 22.01.03.03 Central Solenoid
        self.reactor_equip_cost += self.central_sol_cost
        # STARFIRE percentage for self.spares
        self.spares += 6.124e-1 * self.central_sol_cost

        # 22.01.03.04 Control Coils
        self.reactor_equip_cost += self.control_coils_cost
        # STARFIRE percentage for self.spares
        self.spares += 1.075e-1 * self.control_coils_cost

        # 22.01.04 Auxiliary Heating and Current Drive
        # HCD cost = cost per injected Watt of power * injected Watts
        self.reactor_equip_cost += self.hcd_cost
        # STARFIRE percentage for self.spares
        self.spares += 2.335e-1 * self.hcd_cost

        # 22.01.05 Primary Structure and Support
        self.reactor_equip_cost += self.primary_structure_cost
        # STARFIRE percentage for self.spares
        self.spares += 6.824e-2 * self.primary_structure_cost

        # 22.01.06 Reactor Vacuum System
        self.reactor_equip_cost += self.reactor_vacuum_sys_cost
        # STARFIRE percentage for self.spares
        self.spares += 1.893e-1 * self.reactor_vacuum_sys_cost

        # 22.01.07 Power Supplies
        self.reactor_equip_cost += self.power_supplies_cost

        # 22.01.08 Impurity Control
        # Superseded and deleted

        # 22.01.09 ECRH Plasma Breakdown
        # Superseded and deleted

        # 22.01.10 Divertor
        self.reactor_equip_cost += self.divertor_cost

        return self.reactor_equip_cost, self.spares

    def turbine_plant_equip_costs(self):
        """Account 23 : Turbine Plant Equipment
        author: S I Muldrew, CCFE, Culham Science Centre
        None
        This routine evaluates the Turbine Plant Equipment costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)

        :return: turbine_equip_cost
        :rtype: float
        """
        # 23.01-07 Turbine System
        self.turbine_sys_costs()
        self.turbine_plant_equip_cost = self.turbine_sys_cost

        # 23.03 Heat Rejection
        self.heat_reject_costs()
        self.turbine_plant_equip_cost += self.heat_reject_cost

        # 23.98 Spares
        # STARFIRE percentage
        self.spares = 1.401e-2 * self.turbine_plant_equip_cost
        self.turbine_plant_equip_cost += self.spares

        # 23.99 Contingency
        # STARFIRE 15%
        self.contingency = cv.step_con * self.turbine_plant_equip_cost
        self.turbine_plant_equip_cost += self.contingency

    def cost_model_2(self):
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

        # Account 12 : Site Permits
        self.site_permits_costs()
        self.step12 = self.site_permits_cost

        # Account 13 : Plant Licensing
        self.plant_license_costs()
        self.step13 = self.plant_license_cost

        # Account 20 : Land and Rights
        self.land_and_rights_costs()

        # 20.01 Land
        self.step2001 = self.land_cost

        # 20.02 Site Preparation
        self.step2002 = self.site_prep_cost

        # Total Account 20 Cost
        self.step20 = self.land_and_rights_cost

        # Account 21 : Building and Site Service Infrastructure
        self.bldg_and_site_service_infra_costs()

        # 21.01 Site Improvements
        self.step2101 = self.site_improvements_cost

        # 21.02 Reactor Building
        self.step2102 = self.reactor_bldg_cost

        # 21.03 Turbine Building
        self.step2103 = self.turbine_bldg_cost

        # 21.04 Cooling System Structures
        self.step2104 = self.cooling_sys_struct_cost

        # 21.05 Electrical Equipment and Power Supply Building
        self.step2105 = self.electrical_and_power_bldg_cost

        # 21.06 Auxiliary Services Building
        self.step2106 = self.aux_services_bldg_cost

        # 21.07 Hot Cell
        self.step2107 = self.hot_cell_cost

        # 21.08 Reactor Service Building
        self.step2108 = self.reactor_serv_bldg_cost

        # 21.09 Service Water Building
        self.step2109 = self.service_water_bldg_cost

        # 21.10 Fuel Handling and Storage Building
        self.step2110 = self.fuel_handling_bldg_cost

        # 21.11 Control Room
        self.step2111 = self.control_room_cost

        # 21.12 AC Power Supply Building
        self.step2112 = self.ac_power_bldg_cost

        # 21.13 Admin Building
        self.step2113 = self.admin_bldg_cost

        # 21.14 Site Service
        self.step2114 = self.site_service_bldg_cost

        # 21.15 Cryogenics and Inert Gas Storage Building
        self.step2115 = self.cryo_and_inert_gas_bldg_cost

        # 21.16 Security Building
        self.step2116 = self.security_bldg_cost

        # 21.17 Ventilation Stack
        self.step2117 = self.vent_stack_cost

        # 21.18 Waste Facilities Buildings
        self.step2118 = self.waste_fac_bldg_cost

        # 21.98 Spares
        self.spares21 = self.spares

        # 21.99 Contingency
        self.contingency21 = self.contingency

        # Total Account 21 Cost
        self.step21 = self.bldg_and_site_service_infra_cost

        # Account 22 : Reactor Plant Equipment
        self.reactor_plant_equip_costs()

        # 22.01 Reactor Equipment

        # 22.01.01 Blanket and First Wall Equipment

        # 22.01.01.01 Total First Wall Cost
        self.step22010101 = self.fw_cost

        # 22.01.01.02.01 Blanket Multiplier Material
        self.step2201010201 = self.blk_multi_mat_cost

        # 22.01.01.02.02 Blanket Breeder Material
        self.step2201010202 = self.blk_breed_mat_cost

        # 22.01.01.02.03 Blanket Steel Costs
        self.step2201010203 = self.blk_steel_cost

        # 22.01.01.02 Total Blanket Cost
        self.step22010102 = self.blk_total_cost

        # Total Account 22.01.01 Cost
        self.step220101 = self.fw_blk_total_cost

        # 22.01.02 Shield
        self.step220102 = self.ib_shield_cost

        # 22.01.03.01 TF Coils
        self.step22010301 = self.tf_coils_cost

        # 22.01.03.02 PF Coils
        self.step22010302 = self.pf_total_cost

        # 22.01.03.03 Central Solenoid
        self.step22010303 = self.central_sol_cost

        # 22.01.03.04 Control Coils
        self.step22010304 = self.control_coils_cost

        # 22.01.04 Auxiliary Heating and Current Drive
        self.step220104 = self.hcd_cost

        # 22.01.05 Primary Structure and Support
        self.step220105 = self.primary_structure_cost

        # 22.01.06 Reactor Vacuum System
        self.step220106 = self.reactor_vacuum_sys_cost

        # 22.01.07 Power Supplies
        self.step220107 = self.power_supplies_cost

        # 22.01.10 Divertor
        self.step220110 = self.divertor_cost

        # Total Account 22.01 Cost
        self.step2201 = self.reactor_equip_cost

        # 22.02 Heat Transfer System
        self.step2202 = self.heat_transfer_sys_cost

        # 22.03 Cryogenic Cooling System
        self.step2203 = self.cryo_sys_cost

        # 22.04 Waste Treatment and Disposal

        # 22.04.01 Liquid Waste
        self.step220401 = self.liquid_waste_cost

        # 22.04.02 Gaseous Waste
        self.step220402 = self.gas_waste_cost

        # 22.04.03 Solid Waste
        self.step220403 = self.solid_waste_cost

        # Total Account 22.04 Cost
        self.step2204 = self.waste_disposal_cost

        # 22.05 Fuel Handling and Storage
        self.step2205 = self.fuel_handling_cost

        # 22.06 Other Reactor Plant Equipment

        # 22.06.01 Maintenance Equipment
        self.step220601 = self.maintenance_equip_cost

        # 22.06.02 Special Heating Systems
        self.step220602 = self.special_heating_sys_cost

        # 22.06.03 Coolant Storage
        self.step220603 = self.coolant_store_cost

        # 22.06.04 Gas System
        self.step220604 = self.gas_sys_cost

        # 22.06.06 Fluid Leak Detection
        self.step220606 = self.fluid_leak_detect_cost

        # 22.06.07 Closed Loop Coolant System
        self.step220607 = self.closed_loop_coolant_sys_cost

        # 22.06.08 Standy Cooling System
        self.step220608 = self.standby_cooling_sys_cost

        # Total Account 22.06 Cost
        self.step2206 = self.other_reactor_equip_cost

        # 22.07 Instrumentation and Control
        self.step2207 = self.instrument_and_control_cost

        # 22.98 Spares
        self.step2298 = self.spares_total

        # 22.99 Contingency
        self.step2299 = self.contingency

        # Total Account 22 Cost
        self.step22 = self.reactor_plant_equip_cost

        # Account 23 : Turbine Plant Equipments
        self.turbine_plant_equip_costs()

        # 23.a Turbine System
        self.step23a = self.turbine_sys_cost

        # 23.03 Heat Rejection
        self.step2303 = self.heat_reject_cost

        # 23.98 Spares
        self.step2398 = self.spares

        # 23.99 Contingency
        self.step2399 = self.contingency

        # Total Account 23 Cost
        self.step23 = self.turbine_plant_equip_cost

        # Account 24 : Electric Plant Equipment
        self.electric_plant_equip_costs()

        # 24.01 Switch Gear
        self.step2401 = self.switch_gear_cost

        # 24.02 Station Service Equipment
        self.step2402 = self.station_service_equip_cost

        # 24.03 Switchboards
        self.step2403 = self.switchboards_cost

        # 24.04 Protective Equipment
        self.step2404 = self.protective_equip_cost

        # 24.05 Electrical Structures
        self.step2405 = self.electric_structures_cost

        # 24.06 Power and Control Wiring
        self.step2406 = self.power_and_control_wiring_cost

        # 24.07 Electric Lighting
        self.step2407 = self.electric_lighting_cost

        # 24.98 Spares
        self.step2498 = self.spares

        # 24.99 Contingency
        self.step2499 = self.contingency

        # Total Account 24 Cost
        self.step24 = self.electric_plant_equip_cost

        # Account 25 : Miscellaneous Plant Equipment
        self.misc_equip_costs()

        # 25.01 Transport and Lifting Equipment
        self.step2501 = self.transport_equip_cost

        # 25.02 Air and Water Service System
        self.step2502 = self.air_water_sys_cost

        # 25.03 Communications Equipment
        self.step2503 = self.comms_equip_cost

        # 25.04 Furnishing and Fixtures
        self.step2504 = self.furnishing_cost

        # 25.98 Spares
        self.step2598 = self.spares

        # 25.99 Contingency
        self.step2599 = self.contingency

        # Total Account 25 Cost
        self.step25 = self.misc_equip_cost

        # Total plant direct cost without remote handling
        cv.cdirt = (
            self.step12
            + self.step13
            + self.step20
            + self.step21
            + self.step22
            + self.step23
            + self.step24
            + self.step25
        )

        # Account 27 : Remote Handling
        self.remote_handling_costs()

        # 27.01 Remote Handling
        self.step2701 = self.remote_handling_cost

        # Total Account 27 Cost
        self.step27 = self.remote_handling_cost

        # Total plant direct cost with remote handling
        cv.cdirt = cv.cdirt + self.step27

        # Accounts 91-93: Indirect costs
        self.indirect_costs()

        self.step91 = self.construct_facs_cost

        self.step92 = self.eng_construct_manage_cost

        self.step93 = self.other_cost

        # Constructed cost
        cv.concost = cv.cdirt + self.step91 + self.step92 + self.step93

        # Cost of electricity
        if cv.ireactor == 1 and cv.ipnet == 0:
            self.coelc_step()

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
        self.anncap = cv.capcost * cv.fcr0

        # Cost of electricity due to plant capital cost
        cv.coecap = 1.0e9 * self.anncap / kwhpy

        # Costs due to first wall and blanket renewal
        # ===========================================

        # Operational life
        self.fwbllife = fwbsv.bktlife

        # Compound interest factor
        self.feffwbl = (1.0e0 + cv.discount_rate) ** self.fwbllife

        # Capital recovery factor
        self.crffwbl = (self.feffwbl * cv.discount_rate) / (self.feffwbl - 1.0e0)

        # Annual cost of replacements
        self.annfwbl = self.fwblkcost * cv.fcap0cp * self.crffwbl

        # Cost of electricity due to first wall/blanket replacements
        self.coefwbl = 1.0e9 * self.annfwbl / kwhpy

        # Costs due to divertor renewal
        # =============================

        # Compound interest factor
        fefdiv = (1.0e0 + cv.discount_rate) ** cv.divlife

        # Capital recovery factor
        crfdiv = (fefdiv * cv.discount_rate) / (fefdiv - 1.0e0)

        # Annual cost of replacements
        self.anndiv = cv.divcst * cv.fcap0cp * crfdiv

        # Cost of electricity due to divertor replacements
        self.coediv = 1.0e9 * self.anndiv / kwhpy

        # Costs due to centrepost renewal
        # ===============================

        if pv.itart == 1:
            # Compound interest factor
            fefcp = (1.0e0 + cv.discount_rate) ** cv.cplife

            # Capital recovery factor
            crfcp = (fefcp * cv.discount_rate) / (fefcp - 1.0e0)

            # Annual cost of replacements
            self.anncp = cv.cpstcst * cv.fcap0cp * crfcp

            # Cost of electricity due to centrepost replacements
            self.coecp = 1.0e9 * self.anncp / kwhpy
        else:
            self.anncp = 0.0e0
            self.coecp = 0.0e0

        # Costs due to partial current drive system renewal
        # =================================================

        # Compound interest factor
        fefcdr = (1.0e0 + cv.discount_rate) ** cv.cdrlife

        # Capital recovery factor
        crfcdr = (fefcdr * cv.discount_rate) / (fefcdr - 1.0e0)

        # Annual cost of replacements
        if cv.ifueltyp == 0:
            self.anncdr = 0.0e0
        else:
            self.anncdr = (
                cv.cdcost * cv.fcdfuel / (1.0e0 - cv.fcdfuel) * cv.fcap0cp * crfcdr
            )

        # Cost of electricity due to current drive system replacements
        self.coecdr = 1.0e9 * self.anncdr / kwhpy

        # Costs due to operation and maintenance
        # ======================================

        # Annual cost of operation and maintenance
        self.annoam = cv.step_ucoam * (htv.pnetelmw / 1200.0e0) ** 0.5

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
        cv.coeoam = 1.0e9 * self.annoam / kwhpy

        #  Costs due to reactor fuel
        #  =========================

        #  Annual cost of fuel

        #  Sum D-T fuel cost and He3 fuel cost
        self.annfuel = (
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
        self.coefuel = 1.0e9 * self.annfuel / kwhpy

        #  Costs due to waste disposal
        #  ===========================

        #  Annual cost of waste disposal
        self.annwst = cv.step_ucwst * (htv.pnetelmw / 1200.0e0) ** 0.5

        #  Cost of electricity due to waste disposal
        self.coewst = 1.0e9 * self.annwst / kwhpy

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
        self.anndecom = (
            cv.decomf
            * cv.concost
            * cv.fcr0
            / (1.0e0 + cv.discount_rate - cv.dintrt) ** (cv.tlife - cv.dtlife)
        )

        #  Cost of electricity due to decommissioning fund
        self.coedecom = 1.0e9 * self.anndecom / kwhpy

        #  Total costs
        #  ===========

        #  Annual costs due to 'fuel-like' components
        self.annfuelt = (
            self.annfwbl
            + self.anndiv
            + self.anncdr
            + self.anncp
            + self.annfuel
            + self.annwst
        )

        #  Total cost of electricity due to 'fuel-like' components
        cv.coefuelt = (
            self.coefwbl
            + self.coediv
            + self.coecdr
            + self.coecp
            + self.coefuel
            + self.coewst
        )

        #  Total annual costs
        self.anntot = self.anncap + self.annfuelt + self.annoam + self.anndecom

        #  Total cost of electricity
        cv.coe = cv.coecap + cv.coefuelt + cv.coeoam + self.coedecom

    def print_output(self):
        if self.iprint == 1 and cv.output_costs == 1:

            # Output header

            title = (
                "STEP Costing Model ("
                + f2py_compatible_to_string(cv.step_currency)
                + ")"
            )
            po.oheadr(self.outfile, title.strip())

            po.oshead(self.outfile, "12. Site Permits")
            po.ocosts(self.outfile, "(step12)", "Site Permits (M$)", self.step12)

            po.oshead(self.outfile, "13. Plant Licensing")
            po.ocosts(self.outfile, "(step13)", "Plant Licensing (M$)", self.step13)

            po.oshead(self.outfile, "20. Land and Rights")
            po.ocosts(self.outfile, "(step2001)", "Land (M$)", self.step2001)
            po.ocosts(
                self.outfile, "(step2002)", "Site Preparation (M$)", self.step2002
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step20)", "Total Account 20 Cost (M$)", self.step20
            )

            po.oshead(self.outfile, "21. Building and Site Service Infrastructure")
            po.ocosts(
                self.outfile, "(step2101)", "Site Improvements (M$)", self.step2101
            )
            po.ocosts(
                self.outfile, "(step2102)", "Reactor Building (M$)", self.step2102
            )
            po.ocosts(
                self.outfile, "(step2103)", "Turbine Building (M$)", self.step2103
            )
            po.ocosts(
                self.outfile,
                "(step2104)",
                "Cooling System Structures (M$)",
                self.step2104,
            )
            po.ocosts(
                self.outfile,
                "(step2105)",
                "Electrical Equipment and Power Supply Building (M$)",
                self.step2105,
            )
            po.ocosts(
                self.outfile,
                "(step2106)",
                "Auxiliary Services Building (M$)",
                self.step2106,
            )
            po.ocosts(self.outfile, "(step2107)", "Hot Cell (M$)", self.step2107)
            po.ocosts(
                self.outfile,
                "(step2108)",
                "Reactor Service Building (M$)",
                self.step2108,
            )
            po.ocosts(
                self.outfile, "(step2109)", "Service Water Building (M$)", self.step2109
            )
            po.ocosts(
                self.outfile,
                "(step2110)",
                "Fuel Handling and Storage Building (M$)",
                self.step2110,
            )
            po.ocosts(self.outfile, "(step2111)", "Control Room (M$)", self.step2111)
            po.ocosts(
                self.outfile,
                "(step2112)",
                "AC Power Supply Building (M$)",
                self.step2112,
            )
            po.ocosts(self.outfile, "(step2113)", "Admin Building (M$)", self.step2113)
            po.ocosts(self.outfile, "(step2114)", "Site Service (M$)", self.step2114)
            po.ocosts(
                self.outfile,
                "(step2115)",
                "Cryogenics and Inert Gas Storage Building (M$)",
                self.step2115,
            )
            po.ocosts(
                self.outfile, "(step2116)", "Security Building (M$)", self.step2116
            )
            po.ocosts(
                self.outfile, "(step2117)", "Ventilation Stack (M$)", self.step2117
            )
            po.ocosts(
                self.outfile,
                "(step2118)",
                "Waste Facilities Buildings (M$)",
                self.step2118,
            )
            po.ocosts(self.outfile, "(step2198)", "Spares (M$)", self.spares21)
            po.ocosts(
                self.outfile, "(step2199)", "Contingency (M$)", self.contingency21
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step21)", "Total Account 21 Cost (M$)", self.step21
            )

            po.oshead(self.outfile, "22. Reactor Plant Equipment")
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
                    self.step22010101,
                )
                po.oblnkl(self.outfile)
                po.ocosts(
                    self.outfile,
                    "(step2201010201)",
                    "Blanket Multiplier Material (M$)",
                    self.step2201010201,
                )
                po.ocosts(
                    self.outfile,
                    "(step2201010202)",
                    "Blanket Breeder Material (M$)",
                    self.step2201010202,
                )
                po.ocosts(
                    self.outfile,
                    "(step2201010203)",
                    "Blanket Steel Costs (M$)",
                    self.step2201010203,
                )
                po.ocosts(
                    self.outfile,
                    "(step22010102)",
                    "Total Blanket Cost (M$)",
                    self.step22010102,
                )
                po.oblnkl(self.outfile)
                po.ocosts(
                    self.outfile,
                    "(step220101)",
                    "Total Account 22.01.01 Cost (M$)",
                    self.step220101,
                )
                po.oblnkl(self.outfile)
            elif cv.ifueltyp == 1:
                po.ocosts(
                    self.outfile,
                    "(step220101)",
                    "Blanket and First Wall (Treated as Fuel) (M$)",
                    self.step220101,
                )

            po.ocosts(self.outfile, "(step220102)", "Shield (M$)", self.step220102)
            po.ocosts(
                self.outfile, "(step22010301)", "TF Coils (M$)", self.step22010301
            )
            po.ocosts(
                self.outfile, "(step22010302)", "PF Coils (M$)", self.step22010302
            )
            po.ocosts(
                self.outfile,
                "(step22010303)",
                "Central Solenoid (M$)",
                self.step22010303,
            )
            po.ocosts(
                self.outfile, "(step22010304)", "Control Coils (M$)", self.step22010304
            )

            if cv.ifueltyp == 0:
                po.ocosts(
                    self.outfile,
                    "(step220104)",
                    "Auxiliary Heating and Current Drive (M$)",
                    self.step220104,
                )
            elif cv.ifueltyp == 1:
                po.ocosts(
                    self.outfile,
                    "(step220104)",
                    "Auxiliary Heating and Current Drive (Fraction as Fuel) (M$)",
                    self.step220104,
                )

            po.ocosts(
                self.outfile,
                "(step220105)",
                "Primary Structure and Support (M$)",
                self.step220105,
            )
            po.ocosts(
                self.outfile,
                "(step220106)",
                "Reactor Vacuum System (M$)",
                self.step220106,
            )
            po.ocosts(
                self.outfile, "(step220107)", "Power Supplies (M$)", self.step220107
            )
            po.ocosts(self.outfile, "(step220110)", "Divertor (M$)", self.step220110)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2201)",
                "Total Account 22.01 Cost (M$)",
                self.step2201,
            )
            po.oblnkl(self.outfile)

            po.write(self.outfile, "******************* 22.02 Heat Transfer System")
            po.ocosts(
                self.outfile, "(step2202)", "Heat Transfer System (M$)", self.step2202
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2202)",
                "Total Account 22.02 Cost (M$)",
                self.step2202,
            )
            po.oblnkl(self.outfile)

            po.write(self.outfile, "******************* 22.03 Cryogenic Cooling System")
            po.ocosts(self.outfile, "(step2203)", "Cryoplant (M$)", self.step2203)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2203)",
                "Total Account 22.03 Cost (M$)",
                self.step2203,
            )
            po.oblnkl(self.outfile)

            po.write(
                self.outfile, "******************* 22.04 Waste Treatment and Disposal"
            )
            po.ocosts(
                self.outfile, "(step220401)", "Liquid Waste (M$)", self.step220401
            )
            po.ocosts(
                self.outfile, "(step220402)", "Gaseous Waste (M$)", self.step220402
            )
            po.ocosts(self.outfile, "(step220403)", "Solid Waste (M$)", self.step220403)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2204)",
                "Total Account 22.04 Cost (M$)",
                self.step2204,
            )
            po.oblnkl(self.outfile)

            po.write(
                self.outfile, "******************* 22.05 Fuel Handling and Storage"
            )
            po.ocosts(
                self.outfile,
                "(step2205)",
                "Fuel Handling and Storage (M$)",
                self.step2205,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2205)",
                "Total Account 22.05 Cost (M$)",
                self.step2205,
            )
            po.oblnkl(self.outfile)

            po.write(
                self.outfile, "******************* 22.06 Other Reactor Plant Equipment"
            )
            po.ocosts(
                self.outfile,
                "(step220601)",
                "Maintenance Equipment (M$)",
                self.step220601,
            )
            po.ocosts(
                self.outfile,
                "(step220602)",
                "Special Heating Systems (M$)",
                self.step220602,
            )
            po.ocosts(
                self.outfile, "(step220603)", "Coolant Storage (M$)", self.step220603
            )
            po.ocosts(self.outfile, "(step220604)", "Gas System (M$)", self.step220604)
            # po.ocosts(self.outfile,'(step220605)','Inert Atmosphere System (M$)', step220605)
            po.ocosts(
                self.outfile,
                "(step220606)",
                "Fluid Leak Detection (M$)",
                self.step220606,
            )
            po.ocosts(
                self.outfile,
                "(step220607)",
                "Closed Loop Coolant System (M$)",
                self.step220607,
            )
            po.ocosts(
                self.outfile,
                "(step220608)",
                "Standby Cooling System (M$)",
                self.step220608,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2206)",
                "Total Account 22.06 Cost (M$)",
                self.step2206,
            )
            po.oblnkl(self.outfile)

            po.write(
                self.outfile, "******************* 22.07 Instrumentation and Control"
            )
            po.ocosts(
                self.outfile,
                "(step2207)",
                "Instrumentation and Control (M$)",
                self.step2207,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(step2207)",
                "Total Account 22.07 Cost (M$)",
                self.step2207,
            )
            po.oblnkl(self.outfile)
            po.ostars(self.outfile, 20)
            po.ocosts(self.outfile, "(step2298)", "Spares (M$)", self.step2298)
            po.ocosts(self.outfile, "(step2299)", "Contingency (M$)", self.step2299)
            po.oblnkl(self.outfile)

            po.ocosts(
                self.outfile, "(step22)", "Total Account 22 Cost (M$)", self.step22
            )

            po.oshead(self.outfile, "23. Turbine Plant Equipment")
            po.ocosts(self.outfile, "(step23a)", "Turbine System (M$)", self.step23a)
            po.ocosts(self.outfile, "(step2303)", "Heat Rejection (M$)", self.step2303)
            po.ocosts(self.outfile, "(step2398)", "Spares (M$)", self.step2398)
            po.ocosts(self.outfile, "(step2399)", "Contingency (M$)", self.step2399)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step23)", "Total Account 23 Cost (M$)", self.step23
            )

            po.oshead(self.outfile, "24. Electric Plant Equipment")
            po.ocosts(self.outfile, "(step2401)", "Switch Gear (M$)", self.step2401)
            po.ocosts(
                self.outfile,
                "(step2402)",
                "Station Service Equipment (M$)",
                self.step2402,
            )
            po.ocosts(self.outfile, "(step2403)", "Switchboards (M$)", self.step2403)
            po.ocosts(
                self.outfile, "(step2404)", "Protective Equipment (M$)", self.step2404
            )
            po.ocosts(
                self.outfile, "(step2405)", "Electrical Structures (M$)", self.step2405
            )
            po.ocosts(
                self.outfile,
                "(step2406)",
                "Power and Control Wiring (M$)",
                self.step2406,
            )
            po.ocosts(
                self.outfile, "(step2407)", "Electric Lighting (M$)", self.step2407
            )
            po.ocosts(self.outfile, "(step2498)", "Spares (M$)", self.step2498)
            po.ocosts(self.outfile, "(step2499)", "Contingency (M$)", self.step2499)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step24)", "Total Account 24 Cost (M$)", self.step24
            )

            po.oshead(self.outfile, "25. Miscellaneous Plant Equipment")
            po.ocosts(
                self.outfile,
                "(step2501)",
                "Transport and Lifting Equipment (M$)",
                self.step2501,
            )
            po.ocosts(
                self.outfile,
                "(step2502)",
                "Air and Water Service System (M$)",
                self.step2502,
            )
            po.ocosts(
                self.outfile,
                "(step2503)",
                "Communications Equipment (M$)",
                self.step2503,
            )
            po.ocosts(
                self.outfile,
                "(step2504)",
                "Furnishing and Fixtures (M$)",
                self.step2504,
            )
            po.ocosts(self.outfile, "(step2598)", "Spares (M$)", self.step2598)
            po.ocosts(self.outfile, "(step2599)", "Contingency (M$)", self.step2599)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step25)", "Total Account 25 Cost (M$)", self.step25
            )

            po.oshead(self.outfile, "27. Remote Handling")
            po.ocosts(self.outfile, "(step2701)", "Remote Handing (M$)", self.step2701)
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step27)", "Total Account 27 Cost (M$)", self.step27
            )

            po.oshead(self.outfile, "Plant Direct Cost")
            po.ocosts(self.outfile, "(cdirt)", "Plant direct cost (M$)", cv.cdirt)

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

            po.oshead(self.outfile, "Constructed Cost")
            po.ocosts(self.outfile, "(concost)", "Constructed Cost (M$)", cv.concost)

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
                self.fwbllife,
            )
            po.ovarrf(self.outfile, "Divertor life (years)", "(divlife.)", cv.divlife)
            if pv.itart == 1:
                po.ovarrf(
                    self.outfile, "Centrepost life (years)", "(cplife.)", cv.cplife
                )

            po.ovarrf(self.outfile, "Cost of electricity (m$/kWh)", "(coe)", cv.coe)
            po.osubhd(self.outfile, "Power Generation Costs :")

            if (
                self.annfwbl != self.annfwbl
                or self.annfwbl > 1.0e10
                or self.annfwbl < 0.0
            ):
                po.ocmmnt(self.outfile, "Problem with annfwbl")
                po.ocmmnt(self.outfile, "fwblkcost=", cv.fwallcst)
                po.ocmmnt(
                    self.outfile, "crffwbl=", self.crffwbl, "  fcap0cp=", cv.fcap0cp
                )
                po.ocmmnt(
                    self.outfile, "feffwbl=", self.feffwbl, "  fwbllife=", self.fwbllife
                )

            po.write(
                self.outfile,
                "\t" * 9 + "Annual Costs, M$" + "\t" * 1 + " " * 4 + "COE, m$/kWh",
            )
            po.dblcol(self.outfile, "Capital Investment", self.anncap, cv.coecap)
            po.dblcol(self.outfile, "Operation & Maintenance", self.annoam, cv.coeoam)
            po.dblcol(
                self.outfile, "Decommissioning Fund", self.anndecom, self.coedecom
            )
            po.write(self.outfile, "Fuel Charge Breakdown")
            po.dblcol(
                self.outfile, "\tBlanket & first wall", self.annfwbl, self.coefwbl
            )
            po.dblcol(self.outfile, "\tDivertors", self.anndiv, self.coediv)
            po.dblcol(self.outfile, "\tCentrepost (TART only)", self.anncp, self.coecp)
            po.dblcol(self.outfile, "\tAuxiliary Heating", self.anncdr, self.coecdr)
            po.dblcol(self.outfile, "\tActual Fuel", self.annfuel, self.coefuel)
            po.dblcol(self.outfile, "\tWaste Disposal", self.annwst, self.coewst)
            po.dblcol(self.outfile, "Total Fuel Cost", self.annfuelt, cv.coefuelt)
            po.dblcol(self.outfile, "Total Cost", self.anntot, cv.coe)

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
