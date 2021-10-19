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
        cs.step_a23(self.outfile, self.iprint)

        # Account 24 : Electric Plant Equipment
        cs.step_a24(self.outfile, self.iprint)

        # Account 25 : Miscellaneous Plant Equipment
        cs.step_a25(self.outfile, self.iprint)

        # Total plant direct cost without remote handling
        cv.cdirt = cs.step20 + cs.step21 + cs.step22 + cs.step23 + cs.step24 + cs.step25

        # Account 27 : Remote Handling
        cs.step_a27(self.outfile, self.iprint)

        # Total plant direct cost with remote handling
        cv.cdirt = cv.cdirt + cs.step27
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Plant Direct Cost")
            po.ocosts(self.outfile, "(cdirt)", "Plant direct cost (M$)", cv.cdirt)

        # Accounts 91-93: Indirect costs
        cs.step_indirect_costs(self.outfile, self.iprint)

        # Constructed cost
        cv.concost = cv.cdirt + cs.step91 + cs.step92 + cs.step93
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "Constructed Cost")
            po.ocosts(self.outfile, "(concost)", "Constructed Cost (M$)", cv.concost)

        #  Cost of electricity
        if cv.ireactor == 1 and cv.ipnet == 0:
            cs.coelc_step(self.outfile, self.iprint)

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
        # Account 22 : Reactor Plant Equipment
        if self.iprint == 1 and cv.output_costs == 1:
            po.oshead(self.outfile, "22. Reactor Plant Equipment")

        step2298, step2299, cs.step22 = cs.step_a22(
            self.outfile, self.iprint, cv.step_con
        )

        # Output costs
        if self.iprint == 1 and cv.output_costs == 1:
            po.ostars(self.outfile, 20)
            po.ocosts(self.outfile, "(step2298)", "Spares (M$)", step2298)
            po.ocosts(self.outfile, "(step2299)", "Contingency (M$)", step2299)
            po.oblnkl(self.outfile)
            po.ocosts(self.outfile, "(step22)", "Total Account 22 Cost (M$)", cs.step22)
