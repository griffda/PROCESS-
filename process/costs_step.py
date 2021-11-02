from process import fortran as ft
from process.fortran import costs_step_module as cs
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
        
        self.step20: float = 0
        self.step21: float = 0
        self.step22: float = 0
        self.step23: float = 0
        self.step24: float = 0
        self.step25: float = 0
        self.step27: float = 0
        self.step91: float = 0
        self.step92: float = 0
        self.step93: float = 0
        self.fwblkcost: float = 0
        self.vfi: float = 0
        self.vfi_star: float = 0
        self.ptherm_star: float = 0
        self.rmajor_star: float = 0
        self.rminor_star: float = 0
        self.pth: float = 0

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
        cs.pth = pv.powfmw + fwbsv.emultmw + htv.pinjwp

        # STARFIRE Reference Values
        self.vfi_star = 6.737e3  # Volume of Fusion Island (m3)
        cs.ptherm_star = 4.15e3  # Thermal Power (MW)
        cs.rmajor_star = 7.0  # Major Radius (m)
        cs.rminor_star = cs.rmajor_star / 3.6  # Minor Radius (m)

        # Output header
        if self.iprint == 1 and cv.output_costs == 1:
            title = "STEP Costing Model (" + str(cv.step_currency).strip() + ")"
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
        cv.cdirt = self.step20 + self.step21 + cs.step22 + cs.step23 + cs.step24 + cs.step25

        # Account 27 : Remote Handling
        self.step_a27()

        # Total plant direct cost with remote handling
        cv.cdirt = cv.cdirt + cs.step27
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Plant Direct Cost")
            po.ocosts(self.outfile, "(cdirt)", "Plant direct cost (M$)", cv.cdirt)

        # Accounts 91-93: Indirect costs
        self.step_indirect_costs()

        # Constructed cost
        cv.concost = cv.cdirt + cs.step91 + cs.step92 + cs.step93
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
            po.ocosts(self.outfile, "(step20)", "Total Account 20 Cost (M$)", self.step20)

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
        step2101 = cv.step_ref[2]
        # step21 should be 0 at this point so I have removed
        # step21 = step21 + step2101
        self.step21 = step2101
        
        # 21.02 Reactor Building
        step2102 = 8.665e3 * bldgsv.a_reactor_bldg**1.2132 * 1.0e-6
        # * 1.0D-6 converts to M$
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
        step2117 = cv.step_ref[18] * (cs.pth / cs.ptherm_star)**0.6e0  
        self.step21 += step2117

        # 21.18 Waste Facilities Buildings
        # Fixed cost (2017 M$); read from input, default = 100 M$
        step2118 = cv.wfbuilding / 1.0e6
        self.step21 += step2118

        # 21.98 Spares
        # STARFIRE percentage
        step2198 = 6.541e-3 * self.step21
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
            po.ocosts(self.outfile, "(step21)", "Total Account 21 Cost (M$)", self.step21)

    def step_a22(self):
        """Account 22 : Reactor Plant Equipment.

        author: S I Muldrew, CCFE, Culham Science Centre
        This routine evaluates the Account 22 (Reactor Plant Equipment)
        costs.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        """
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "22. Reactor Plant Equipment")

        cs.step22 = 0.0
        step2298 = 0.0

        # Account 22.01 : Reactor Equipment
        step2201, spares = self.step_a2201()
        cs.step22 += step2201
        step2298 += spares

        #  Account 22.02 : Heat Transfer Systems
        cs.step22 += self.step_a2202()

        #  Account 22.03 : Cryogenic Cooling System
        cs.step22 += self.step_a2203()

        #  Account 22.04 : Waste Treatment and Disposal
        cs.step22 += self.step_a2204()

        #  Account 22.05 : Fuel Handling and Storage
        step2205, spares = self.step_a2205()
        cs.step22 += step2205
        step2298 += spares

        #  Account 22.06 : Other Reactor Plant Equipment
        step2206, spares = self.step_a2206()
        cs.step22 += step2206
        step2298 += spares

        #  Account 22.07 : Instrumentation and Control
        cs.step22 += self.step_a2207()

        # 22.98 Spares
        # STARFIRE percentage of components
        cs.step22 += step2298

        # 21.99 Contingency
        # STARFIRE 15%
        step2299 = cv.step_con * cs.step22
        cs.step22 += step2299

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.ostars(self.outfile, 20)
            po.ocosts(self.outfile, "(step2298)", "Spares (M$)", step2298)
            po.ocosts(self.outfile, "(step2299)", "Contingency (M$)", step2299)
            po.oblnkl(self.outfile)
            po.ocosts(self.outfile, "(step22)", "Total Account 22 Cost (M$)", cs.step22)

    def step_a23(self):
        """Account 23 : Turbine Plant Equipment."""
        step23a, step2303, step2398, step2399, cs.step23 = cs.step_a23(
            cv.step_ref, cv.step_con, htv.pgrossmw
        )

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "23. Turbine Plant Equipment")
            po.ocosts(self.outfile, "(step23a)", "Turbine System (M$)", step23a)
            po.ocosts(self.outfile, "(step2303)", "Heat Rejection (M$)", step2303)
            po.ocosts(self.outfile, "(step2398)", "Spares (M$)", step2398)
            po.ocosts(self.outfile, "(step2399)", "Contingency (M$)", step2399)
            po.oblnkl(self.outfile)
            po.ocosts(self.outfile, "(step23)", "Total Account 23 Cost (M$)", cs.step23)

    def step_a24(self):
        """Account 24 : Electric Plant Equipment."""
        (
            step2401,
            step2402,
            step2403,
            step2404,
            step2405,
            step2406,
            step2407,
            step2498,
            step2499,
            cs.step24,
        ) = cs.step_a24(cv.step_ref, cv.step_con, htv.pgrossmw)

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
            po.ocosts(self.outfile, "(step24)", "Total Account 24 Cost (M$)", cs.step24)

    def step_a25(self):
        """Account 25 : Miscellaneous Plant Equipment."""
        (
            step2501,
            step2502,
            step2503,
            step2504,
            step2598,
            step2599,
            cs.step25,
        ) = cs.step_a25(cv.step_ref, cv.step_con, htv.pgrossmw, bldgsv.wgt)

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
            po.ocosts(self.outfile, "(step25)", "Total Account 25 Cost (M$)", cs.step25)

    def step_a27(self):
        """Account 27: Remote Handling."""
        step2701, cs.step27 = cs.step_a27(cv.cdirt, cv.step_rh_costfrac)

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "27. Remote Handling")
            po.ocosts(self.outfile, "(step2701)", "Remote Handing (M$)", step2701)
            po.oblnkl(self.outfile)
            po.ocosts(self.outfile, "(step27)", "Total Account 27 Cost (M$)", cs.step27)

    def step_indirect_costs(self):
        """Accounts 91-93: Indirect costs."""
        cs.step91, cs.step92, cs.step93 = cs.step_indirect_costs(
            cv.cdirt, cv.step91_per, cv.step92_per, cv.step93_per
        )

        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Indirect Cost")
            po.ocosts(
                self.outfile,
                "(step91)",
                "Construction Facilities, Equipment and Services (M$)",
                cs.step91,
            )
            po.ocosts(
                self.outfile,
                "(step92)",
                "Engineering and Costruction Management Services (M$)",
                cs.step92,
            )
            po.ocosts(self.outfile, "(step93)", "Other Costs (M$)", cs.step93)

    def coelc_step(self):
        """Cost of electricity."""
        (
            anncap,
            anncdr,
            anncp,
            anndecom,
            anndiv,
            annfuel,
            annfuelt,
            annfwbl,
            annoam,
            anntot,
            annwst,
            coecdr,
            coecp,
            coedecom,
            coediv,
            coefuel,
            coefwbl,
            coewst,
            crffwbl,
            feffwbl,
            fwbllife,
            cv.moneyint,
            cv.capcost,
            cv.coecap,
            cv.coeoam,
            cv.coefuelt,
            cv.coe,
            title,
        ) = cs.coelc_step(
            cv.discount_rate,
            cv.tlife,
            cv.ucfuel,
            cv.uche3,
            cv.cdcost,
            cv.divcst,
            cv.fcdfuel,
            cv.ifueltyp,
            cv.fwallcst,
            cv.fcr0,
            cv.fcap0cp,
            cv.fcap0,
            cv.dtlife,
            cv.divlife,
            cv.dintrt,
            cv.decomf,
            cv.cpstcst,
            cv.cplife,
            cv.concost,
            cv.cfactr,
            cv.cdrlife,
            cv.step_ref,
            cv.step_currency,
            cv.step_ucoam,
            cv.step_ucwst,
            fwbsv.bktlife,
            htv.pnetelmw,
            pv.fhe3,
            pv.itart,
            pv.wtgpd,
            tv.tburn,
            tv.tcycle,
            constants.n_day_year,
        )

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
                self.outfile, "\t" * 36 + "Annual Costs, M$" + "\t" * 6 + "COE, m$/kWh"
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
                    cv.fwblkcost,
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
        """Account 22.01 : Reactor Equipment.

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

        (
            step2201,
            spares,
            cv.divcst,
            cv.cdcost,
            step22010303,
            step22010304,
            step220105,
            step220106,
            step220107,
            step220108,
            step220109,
            step220110,
        ) = cs.step_a2201(
            cv.step_ref,
            cv.ifueltyp,
            cv.fcdfuel,
            pv.rmajor,
            pv.rminor,
            step220101,
            step220102,
            step22010301,
            step22010302,
            step220104,
            self.vfi,
            self.vfi_star
        )

        # Output costs
        if (self.iprint == 1) and (cv.output_costs == 1):
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
            po.ocosts(self.outfile, "(step220108)", "Impurity Control (M$)", step220108)
            po.ocosts(
                self.outfile, "(step220109)", "ECRH Plasma Breakdown (M$)", step220109
            )
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
        step22010101 = 1.0e-6 * (fwbsv.fw_armour_mass * cv.step_ucfwa + fwbsv.fwmass * cv.step_ucfws) 

        if (cv.ifueltyp == 1):
            cv.fwallcst = step22010101
            step22010101 = 0.0e0
        elif (cv.ifueltyp == 2):
           cv.fwallcst = step22010101
        else:
            cv.fwallcst = 0.0e0

        # Account 22.01.01.02 : Breeder Blanket

        if (htv.ipowerflow == 0):
      
            #Account 22.01.01.02.01 : Blanket Multiplier Material 
            step2201010201 = 1.0e-6 * fwbsv.whtblbe * cv.step_ucblbe
                
            #Account 22.01.01.02.02 : Blanket Breeder Material
            if (fwbsv.blktmodel == 0):
                    step2201010202 = 1.0e-6 * fwbsv.wtblli2o * cv.step_ucblbreed
            else:
                    step2201010202 = 1.0e-6 * fwbsv.whtblbreed * cv.step_ucblbreed
        else:
            if ((fwbsv.blkttype == 1) or (fwbsv.blkttype == 2)):
                # Liquid blanket (LiPb + Li)
                # Account 22.01.01.02.01 : Blanket Multiplier Material 
                step2201010201 = 1.0e-6 * fwbsv.wtbllipb * cv.ucbllipb * 2.99e0
                # Account 22.01.01.02.02 : Blanket Breeder Material 
                step2201010202 = 1.0e-6 * fwbsv.whtblli * cv.ucblli * 2.99e0
            else:
                #  Solid blanket (Li2O + Be)
                #Account 22.01.01.02.01 : Blanket Multiplier Material 
                step2201010201 = 1.0e-6 * fwbsv.whtblbe * cv.step_ucblbe
                #Account 22.01.01.02.02 : Blanket Breeder Material
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

        step22010102 = step2201010201 + step2201010202 + step2201010203 + step2201010204 \
        + step2201010205 + step2201010206 + step2201010207

        if (cv.ifueltyp == 1):
            cv.blkcst = step22010102
            step22010102 = 0.0e0
        elif (cv.ifueltyp == 2):
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
        hbot = pv.rminor*pv.kappa + bv.vgap + dv.divfix
        # if a double null machine then symmetric otherwise asymmetric
        if ( pv.idivrt == 2 ):
            htop = hbot
        else:
            htop = pv.rminor*pv.kappa + 0.5e0*(bv.scrapli+bv.scraplo + bv.fwith+bv.fwoth) + bv.blnktth

        # Average of top and bottom (m)
        hshld = 0.5e0*(htop + hbot)

        #Radius to outer edge of inboard shield (m)
        r1 = bv.rsldi + bv.shldith

        #Corrected shield thickness: allows for 300mm vacuum vessel
        #Justification: requirement from K. Taylor, neutronics
        ## TODO: replace this correction when valid (VV + shield) is used
        shldith_corr = (bv.d_vv_in + bv.shldith) - 0.3e0
        #Volume of inboard cylindrical shell (m3)
        inb_sh_v = 2.0e0*(hshld+bv.shldtth) * constants.pi*(r1**2 - (r1-shldith_corr)**2)

        #Scale shield material volume (allow for 10% volume coolant, 5% steel)
        inb_sh_v_mtl = 0.85e0 * inb_sh_v
        
        #Define shield material density (sh_mtl_d [kg/m3]) and cost (sh_mtl_c [$/kg])
        if ( fwbsv.i_shield_mat == 1 ):
            #tungsten carbide
            sh_mtl_d = fwbsv.denwc
            sh_mtl_c = cv.step_ucshwc
        else:
            #tungsten (default)
            sh_mtl_d = fwbsv.denw
            sh_mtl_c = cv.step_ucshw

        #Find inboard shield mass (kg) 
        inb_sh_m = inb_sh_v_mtl * sh_mtl_d

        #Find inboard shield cost (converted to M$2017)
        step220102 = (inb_sh_m * sh_mtl_c) / 1.0e6*(229.0e0/264.71e0)

        #Note: outboard shield costs currently set to zero

        return step220102

    def step_a22010301(self):
        """22.01.03.01 TF Coils.

        :return: cost of 22010301
        :rtype: float
        """
        # Initialise local vars
        c_tf_inboard_legs = 0.0e0
        c_tf_outboard_legs = 0.0e0
        
        # Copper coils
        if (tfv.i_tf_sup == 0):
            # Calculation taken from cost model 0: simply the cost of copper conductor
            # masses
            # Inflating from 1990 $ to 2017 $ at nuclear rate equates to a factor of 
            # 2.99
            # Inboard TF coil legs
            c_tf_inboard_legs = 1.0e-6 * tfv.whtcp * cv.uccpcl1 * 2.99e0
            
            # Outboard TF coil legs
            c_tf_outboard_legs = 1.0e-6 * tfv.whttflgs * cv.uccpclb * 2.99e0
            
            # Total TF coil cost
            step22010301 = c_tf_inboard_legs + c_tf_outboard_legs
        
        # Superconducting coils
        elif (tfv.i_tf_sup == 1):
            # Original STARFIRE value in M$, scaling with fusion island volume
            step22010301 = cv.step_ref[21] * (self.vfi / self.vfi_star)
        
        # Cryogenic aluminium coils
        elif (tfv.i_tf_sup == 2):
            # Cost approximated as the material cost of conducting Al * a 
            # manufacturing cost factor
            # Al conductor mass per coil * number of coils * cost per kilo *
            # manufacturing cost factor, converted to M$
            # step_mc_cryo_al_per = 0.2: 20% manufacturing cost
            step22010301 = (tfv.whtconal * tfv.n_tf * cv.step_uc_cryo_al) * \
                (cv.step_mc_cryo_al_per + 1.0e0) * 1.0e-6

        # ifueltyp: consider centrepost cost as fuel, capital or both?
        # cpstcst used later in coelc_step()
        cv.cpstcst = 0.0e0  # TART centrepost
        if (pv.itart == 1):
            if (cv.ifueltyp == 1):
                # Treat centrepost cost as fuel cost
                cv.cpstcst = c_tf_inboard_legs
                if (tfv.i_tf_sup == 0):
                    # Subtract from capital cost
                    step22010301 = step22010301 - c_tf_inboard_legs
            elif (cv.ifueltyp == 2):
                # Treat centrepost cost as capital and fuel cost
                cv.cpstcst = c_tf_inboard_legs

        return step22010301

    def step_a22010302(self):
        """Account 22.01.03.02 PF Coils: PF magnet assemblies.

        :return: cost 22010302
        :rtype: float
        """
        step22010302 = cs.step_a22010302(
            bv.iohcl,
            constants.twopi,
            constants.dcopper,
            cv.step_uccase,
            cv.step_uccu,
            cv.step_cconshpf,
            cv.step_ucfnc,
            cv.step_cconfix,
            cv.step_ucsc,
            cv.step_ucwindpf,
            pfv.rjconpf,
            pfv.ipfres,
            pfv.vfohc,
            pfv.nohc,
            pfv.turns,
            pfv.isumatpf,
            pfv.whtpfs,
            pfv.ric,
            pfv.rpf,
            pfv.isumatoh,
            pfv.fcupfsu,
            pfv.fcuohsu,
            pfv.vf,
            pfv.awpoh,
            sv.fncmass,
            tfv.dcond,
        )

        return step22010302

    def step_a220104(self):
        """22.01.04 Auxiliary Heating and Current Drive.

        :return: cost 220104
        :rtype: float
        """
        step220104, cv.cdcost = cs.step_a220104(
            cv.step_ref,
            cv.fcdfuel,
            cv.ucich,
            cv.uclh,
            cv.ifueltyp,
            cdv.iefrf,
            cdv.iefrffix,
            cdv.echpwr,
            cdv.pnbitot,
            cdv.plhybd,
        )

        return step220104

    def step_a2202(self):
        """Account 22.02: Heat Transfer System.

        :return: cost 2202
        :rtype: float
        """
        step2202 = cs.step_a2202(htv.pgrossmw)

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
        """Account 22.03: Cryogenic Cooling System.

        :return: cost 2203
        :rtype: float
        """
        (step220301, step220302, step220303, step220304, step2203) = cs.step_a2203(
            cv.step_ref, self.vfi, self.vfi_star
        )

        # Output costs
        if (self.iprint == 1) and (cv.output_costs == 1):
            po.write(self.outfile, "******************* 22.03 Cryogenic Cooling System")
            po.ocosts(
                self.outfile, "(step220301)", "Helium Refrigerator (M$)", step220301
            )
            po.ocosts(
                self.outfile,
                "(step220302)",
                "Liquid Helium Transfer and Storage (M$)",
                step220302,
            )
            po.ocosts(
                self.outfile, "(step220303)", "Gas Helium Storage (M$)", step220303
            )
            po.ocosts(
                self.outfile, "(step220304)", "Liquid Nitrogen Storage (M$)", step220304
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(step2203)", "Total Account 22.03 Cost (M$)", step2203
            )
            po.oblnkl(self.outfile)

        return step2203

    def step_a2204(self):
        """Account 22.04: Waste Treatment and Disposal.

        :return: cost 2204
        :rtype: float
        """
        step2204, step220401, step220402, step220403 = cs.step_a2204(
            cv.step_ref, cs.pth, cs.ptherm_star
        )

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
        """Account 22.05: Fuel Handling and Storage.

        :return: 2205 cost and spares
        :rtype: tuple[float, float]
        """
        step2205, spares = cs.step_a2205(cv.step_ref, cs.pth, cs.ptherm_star)

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
        """Account 22.06: Other Reactor Plant Equipment.

        :return: 2206 cost and spares
        :rtype: tuple[float, float]
        """
        (
            step2206,
            spares,
            step220601,
            step220602,
            step220603,
            step220604,
            step220605,
            step220606,
            step220607,
            step220608,
        ) = cs.step_a2206(cv.step_ref, cs.pth, cs.ptherm_star, self.vfi, self.vfi_star)

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
        """Account 22.07: Instrumentation and Control.

        :return: cost 2207
        :rtype: float
        """
        step2207 = cs.step_a2207(cv.step_ref, cs.pth, cs.ptherm_star)

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
