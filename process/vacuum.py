import logging
import math

from process.utilities.f2py_string_patch import f2py_compatible_to_string

from process.fortran import constants
from process.fortran import physics_variables as pv
from process.fortran import vacuum_variables as vacv
from process.fortran import vacuum_module as vac
from process.fortran import build_variables as buv
from process.fortran import tfcoil_variables as tfv
from process.fortran import times_variables as tv
from process.fortran import process_output as po

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


class Vacuum:
    """Module containing vacuum system routines
    author: P J Knight, CCFE, Culham Science Centre

    This module contains routines for calculating the
    parameters of the vacuum system for a fusion power plant.
    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    """

    def __init__(self) -> None:
        self.outfile: int = constants.nout

    def run(self, output: bool) -> None:
        """Routine to call the vacuum module
        author: P J Knight, CCFE, Culham Science Centre

        This routine calls the main vacuum package.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :param output: indicate whether output should be written to the output file, or not
        :type output: boolean
        """
        # (should be) NBI gas load (deuterons/second)

        qtorus = 0.0e0

        #  Total fuel gas load (kg/s)
        #  2 nuclei * nucleus-pairs/sec * mass/nucleus

        # MDK Check this!!
        gasld = 2.0e0 * pv.qfuel * pv.afuel * constants.umass

        self.vacuum_model = f2py_compatible_to_string(vacv.vacuum_model)

        # vacuum_model required to be compared to a b string
        # as this is what f2py returns
        if self.vacuum_model == "old":
            pumpn, vacv.nvduct, vacv.dlscal, vacv.vacdshm, vacv.vcdimax = vac.vacuum(
                pv.powfmw,
                pv.rmajor,
                pv.rminor,
                0.5e0 * (buv.scrapli + buv.scraplo),
                pv.sarea,
                pv.vol,
                buv.shldoth,
                buv.shldith,
                buv.tfcth,
                buv.rsldi - buv.gapds - buv.d_vv_in,
                tfv.n_tf,
                tv.tdwell,
                pv.dene,
                pv.idivrt,
                qtorus,
                gasld,
                int(output),
                self.outfile,
            )
            # MDK pumpn is real: convert to integer by rounding.
            vacv.vpumpn = math.floor(pumpn + 0.5e0)
        elif self.vacuum_model == "simple":
            vacv.niterpump = self.vacuum_simple(int(output), self.outfile)
        else:
            logger.warning(f"vacuum_model seems to be invalid: {vacv.vacuum_model}")
            po.ocmmnt(
                self.outfile,
                f'ERROR "vacuum_model" seems to be invalid: {vacv.vacuum_model}',
            )

    def vacuum_simple(self, output) -> float:
        """Simple model of vacuum pumping system
        author: MD Kovari, CCFE, Culham Science Centre

        :param output: indicate whether output should be written to the output file, or not
        :type output: boolean

        :return npump: number of pumps for pumpdown and steady-state
        :type npump: real
        """

        # Steady-state model (super simple)
        # One ITER torus cryopump has a throughput of 50 Pa m3/s = 1.2155e+22 molecules/s
        # Issue #304
        niterpump = pv.qfuel / vacv.pumptp

        # Pump-down:
        # Pumping speed per pump m3/s
        pumpspeed = (
            vacv.pumpspeedmax
            * vacv.pumpareafraction
            * vacv.pumpspeedfactor
            * pv.sarea
            / tfv.n_tf
        )

        wallarea = (pv.sarea / 1084.0e0) * 2000.0e0
        # Required pumping speed for pump-down
        pumpdownspeed = (vacv.outgasfactor * wallarea / vacv.pbase) * tv.tdwell ** (
            -vacv.outgasindex
        )
        # Number of pumps required for pump-down
        npumpdown = pumpdownspeed / pumpspeed

        # Combine the two (somewhat inconsistent) models
        # Note that 'npump' can be constrained by constraint equation 63
        npump = max(niterpump, npumpdown)

        #  Output section
        if output:

            po.oheadr(self.outfile, "Vacuum System")
            po.ovarst(
                self.outfile,
                "Switch for vacuum pumping model",
                "(vacuum_model)",
                '"' // self.vacuum_model // '"',
            )
            po.ocmmnt(
                self.outfile,
                "Simple steady-state model with comparison to ITER cryopumps",
            )
            po.ovarre(
                self.outfile,
                "Plasma fuelling rate (nucleus-pairs/s)",
                "(qfuel)",
                pv.qfuel,
                "OP ",
            )
            po.ocmmnt(
                self.outfile, "Number of high vacuum pumps, each with the throughput"
            )
            po.ocmmnt(
                self.outfile,
                " of one ITER cryopump (50 Pa m3 s-1 = 1.2e+22 molecules/s),",
            )
            po.ovarre(
                self.outfile,
                " all operating at the same time",
                "(niterpump)",
                niterpump,
                "OP ",
            )

            po.ovarre(self.outfile, "Dwell time", "(tdwell)", tv.tdwell)
            po.ovarre(
                self.outfile,
                "Number of pumps required for pump-down",
                "(npumpdown)",
                npumpdown,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Number of pumps required overall",
                "(npump)",
                npump,
                "OP ",
            )

        return npump
