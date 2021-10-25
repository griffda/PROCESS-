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


class CostsStep:
    """STEP fusion power plant costings."""

    def __init__(self):
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        self.iprint = 0  # switch for writing to output file (1=yes)
        cs.init_costs_step()

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
        cs.vfi = (
            constants.pi
            * (bv.r_tf_outboard_mid + 0.5 * bv.tfthko) ** 2
            * (bv.hpfu + bv.hmax + bv.tfcth)
        )
        cs.pth = pv.powfmw + fwbsv.emultmw + htv.pinjwp

        # STARFIRE Reference Values
        # vfi_star = 5.1  # Volume of Fusion Island (m3)
        cs.vfi_star = 6.737e3  # Volume of Fusion Island (m3)
        cs.ptherm_star = 4.15e3  # Thermal Power (MW)
        cs.pinjmw_star = 9.04e1  # Auxiliary Power (MW)
        # fwarea_star = 7.8D2  # First Wall Area (m2)
        cs.fwarea_star = 9.42e2  # First Wall Area (m2)
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
        cv.cdirt = cs.step20 + cs.step21 + cs.step22 + cs.step23 + cs.step24 + cs.step25

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
        """Account 20: Land and Rights."""
        step2001, step2002, cs.step20 = cs.step_a20(cv.step_ref, cv.sitecost)

        # Write output
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "20. Land and Rights")
            po.ocosts(self.outfile, "(step2001)", "Land (M$)", step2001)
            po.ocosts(self.outfile, "(step2002)", "Site Preparation (M$)", step2002)
            po.oblnkl(self.outfile)
            po.ocosts(self.outfile, "(step20)", "Total Account 20 Cost (M$)", cs.step20)

    def step_a21(self):
        """Account 21 : Building and Site Service Infrastructure."""
        (
            step2101,
            step2102,
            step2103,
            step2104,
            step2105,
            step2106,
            step2107,
            step2108,
            step2109,
            step2110,
            step2111,
            step2112,
            step2113,
            step2114,
            step2115,
            step2116,
            step2117,
            step2118,
            step2198,
            step2199,
            cs.step21,
        ) = cs.step_a21(
            cv.step_ref,
            cv.step_con,
            cv.wfbuilding,
            bldgsv.a_reactor_bldg,
            bldgsv.a_ee_ps_bldg,
            bldgsv.a_aux_services_bldg,
            bldgsv.a_hot_cell_bldg,
            bldgsv.a_reactor_service_bldg,
            bldgsv.a_service_water_bldg,
            bldgsv.a_fuel_handling_bldg,
            bldgsv.a_control_room_bldg,
            bldgsv.a_ac_ps_bldg,
            bldgsv.a_admin_bldg,
            bldgsv.a_site_service_bldg,
            bldgsv.a_cryo_inert_gas_bldg,
            bldgsv.a_security_bldg,
            htv.pgrossmw,
            cs.pth,
            cs.ptherm_star,
        )

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
            po.ocosts(self.outfile, "(step21)", "Total Account 21 Cost (M$)", cs.step21)

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
            crfcdr,
            crfcp,
            crfdiv,
            crffwbl,
            fefcdr,
            fefcp,
            fefdiv,
            feffwbl,
            fwbllife,
            kwhpy,
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

        if self.iprint == 0 or cv.output_costs == 0:
            return

        # Output section
        po.oshead(self.outfile, "Interest during Construction")
        po.ocosts(
            self.outfile, "(moneyint)", "Interest during construction (M$)", cv.moneyint
        )
        po.oshead(self.outfile, "Total Capital Investment")
        po.ocosts(
            self.outfile, "(capcost)", "Total capital investment (M$)", cv.capcost
        )

        title = "Cost of Electricity, " + f2py_compatible_to_string(cv.step_currency)
        po.oheadr(self.outfile, title)

        po.ovarrf(
            self.outfile, "First wall / blanket life (years)", "(fwbllife)", fwbllife
        )
        po.ovarrf(self.outfile, "Divertor life (years)", "(divlife.)", cv.divlife)
        if pv.itart == 1:
            po.ovarrf(self.outfile, "Centrepost life (years)", "(cplife.)", cv.cplife)

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
                self.outfile, "Divertor direct capital cost (M$)", "(divcst)", cv.divcst
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

        step22010301 = self.step_a22010301()
        step22010302 = self.step_a22010302()

        (
            step2201,
            spares,
            cv.divcst,
            cv.cdcost,
            step220102,
            step22010303,
            step22010304,
            step220104,
            step220105,
            step220106,
            step220107,
            step220108,
            step220109,
            step220110,
        ) = cs.step_a2201(
            cv.step_ref,
            bv.fwarea,
            cv.ifueltyp,
            cv.fcdfuel,
            cdv.pinjmw,
            pv.rmajor,
            pv.rminor,
            step220101,
            step22010301,
            step22010302,
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
        """22.01.01 Blanket and First Wall.

        :return: 220101 cost and sub-costs
        :rtype: tuple[float, float, float, float, float, float,]
        """
        (
            step220101,
            step22010101,
            step22010102,
            step2201010201,
            step2201010202,
            step2201010203,
            cv.fwallcst,
            cv.blkcst,
        ) = cs.step_a220101(
            cv.step_ucblss,
            cv.step_ucblbreed,
            cv.step_ucblbe,
            cv.ucblli,
            cv.step_ucblvd,
            cv.ucblli2o,
            cv.ucbllipb,
            cv.ifueltyp,
            cv.step_ucfws,
            cv.step_ucfwps,
            cv.step_ucfwa,
            fwbsv.blktmodel,
            fwbsv.whtblli,
            fwbsv.blkttype,
            fwbsv.wtblli2o,
            fwbsv.whtblbreed,
            fwbsv.whtblvd,
            fwbsv.whtblbe,
            fwbsv.whtblss,
            fwbsv.wtbllipb,
            fwbsv.fw_armour_mass,
            fwbsv.fwmass,
            bv.fwarea,
            htv.ipowerflow,
        )

        return (
            step220101,
            step22010101,
            step22010102,
            step2201010201,
            step2201010202,
            step2201010203,
        )

    def step_a22010301(self):
        """22.01.03.01 TF Coils.

        :return: cost of 22010301
        :rtype: float
        """
        step22010301, cv.cpstcst = cs.step_a22010301(
            cv.step_ref,
            cv.ifueltyp,
            cv.step_uc_cryo_al,
            cv.step_mc_cryo_al_per,
            cv.uccpcl1,
            cv.uccpclb,
            tfv.i_tf_sup,
            tfv.whtconal,
            tfv.n_tf,
            tfv.whttflgs,
            tfv.whtcp,
            pv.itart,
        )

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
            cv.step_ref, cs.vfi, cs.vfi_star
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
        ) = cs.step_a2206(cv.step_ref, cs.pth, cs.ptherm_star)

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
