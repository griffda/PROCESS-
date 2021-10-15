from process import fortran as ft
from process.fortran import costs_step_module as cs
from process.fortran import constants
from process.fortran import build_variables as bv
from process.fortran import cost_variables as cv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import process_output


class CostsStep:
    """STEP fusion power plant costings."""

    def __init__(self):
        """Initialise Fortran module variables."""
        cs.init_costs_step()

    def run(self):
        """Run main costs_step subroutine."""
        self.costs_step(ft.constants.nout, 0)

    def output(self):
        """Run main costs_step subroutine and write output."""
        self.costs_step(ft.constants.nout, 1)

    def costs_step(self, outfile, iprint):
        """STEP cost accounting for a fusion power plant.

        This method performs the cost accounting for a fusion power plant.
        The direct costs are calculated based on parameters input
        from other sections of the code.

        The code is arranged in the order of the standard accounts.
        STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
        Sheffield et al. (1986), Fusion Technology, 9, 199
        Sheffield & Milora (2016), Fusion Science and Technology, 70, 14
        :param outfile: output file unit
        :type outfile: int
        :param iprint: switch for writing to output file (1=yes)
        :type iprint: int
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
        if iprint == 1 and cv.output_costs == 1:
            title = "STEP Costing Model (" + str(cv.step_currency).strip() + ")"
            process_output.oheadr(outfile, title.strip())

        # Account 20 : Land and Rights
        cs.step_a20(outfile, iprint)

        # Account 21 : Building and Site Service Infrastructure
        cs.step_a21(outfile, iprint)

        # Account 22 : Reactor Plant Equipment
        cs.step_a22(outfile, iprint)

        # Account 23 : Turbine Plant Equipment
        cs.step_a23(outfile, iprint)

        # Account 24 : Electric Plant Equipment
        cs.step_a24(outfile, iprint)

        # Account 25 : Miscellaneous Plant Equipment
        cs.step_a25(outfile, iprint)

        # Total plant direct cost without remote handling
        cv.cdirt = cs.step20 + cs.step21 + cs.step22 + cs.step23 + cs.step24 + cs.step25

        # Account 27 : Remote Handling
        cs.step_a27(outfile, iprint)

        # Total plant direct cost with remote handling
        cv.cdirt = cv.cdirt + cs.step27
        if iprint == 1 and cv.output_costs == 1:
            process_output.oshead(outfile, "Plant Direct Cost")
            process_output.ocosts(
                outfile, "(cdirt)", "Plant direct cost (M$)", cv.cdirt
            )

        # Accounts 91-93: Indirect costs
        cs.step_indirect_costs(outfile, iprint)

        # Constructed cost
        cv.concost = cv.cdirt + cs.step91 + cs.step92 + cs.step93
        if iprint == 1 and cv.output_costs == 1:
            process_output.oshead(outfile, "Constructed Cost")
            process_output.ocosts(
                outfile, "(concost)", "Constructed Cost (M$)", cv.concost
            )

        #  Cost of electricity
        if cv.ireactor == 1 and cv.ipnet == 0:
            cs.coelc_step(outfile, iprint)
