import math

from process import fortran as ft
from process.fortran import cost_variables as cv
from process.fortran import physics_variables as pv
from process.fortran import ife_variables as ifev
from process.fortran import fwbs_variables as fwbsv
from process.fortran import divertor_variables as dv
from process.fortran import tfcoil_variables as tfv
from process.fortran import constraint_variables as ctv
from process.fortran import times_variables as tv
from process.fortran import process_output as po
from process.fortran import availability_module as av
from process.fortran import vacuum_variables as vacv

DAY = 60 * 60 * 24
"""Seconds in a day [s]"""

YEAR = DAY * 365.25
"""Seconds in a year [s]"""


class Availability:
    """Module containing plant availability routines
    author: P J Knight, CCFE, Culham Science Centre

    This module contains routines for calculating the
    plant availability and component lifetimes for a fusion power plant.
    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    """

    def __init__(self) -> None:
        self.outfile = ft.constants.nout  # output file unit
        self.iprint = 0  # switch for writing to output file (1=yes)

    def run(self, output: bool = False):
        """Run appropriate availability model

        Availability switch values
        No.  |  model
        ---- | ------
        0    |  Input value for cfactr
        1    |  Ward and Taylor model (1999)
        2    |  Morris model (2015)

        :param output: indicate whether output should be written to the output file, or not
        :type output: boolean
        """
        self.iprint = 1 if output else 0

        if cv.iavail > 1:
            self.avail_2()  # Morris model (2015)
        else:
            self.avail()  # Taylor and Ward model (1999)

    def avail(self):
        """Routine to calculate component lifetimes and the overall plant availability
        author: P J Knight, CCFE, Culham Science Centre

        This routine calculates the component lifetimes and the overall
        plant availability.
        F/PL/PJK/PROCESS/CODE/043
        """

        # Full power lifetime (in years)
        if ifev.ife != 1:
            # First wall / blanket lifetime (years)

            # TODO MDK Do this calculation whatever the value of blktmodel (whatever that is)
            # For some reason fwlife is not always calculated, so ignore it if it is still zero.
            if fwbsv.fwlife < 0.0001e0:
                fwbsv.bktlife = min(cv.abktflnc / pv.wallmw, cv.tlife)
            else:
                fwbsv.bktlife = min(fwbsv.fwlife, cv.abktflnc / pv.wallmw, cv.tlife)

            # TODO Issue #834
            # Add a test for hldiv=0
            if dv.hldiv < 1.0e-10:
                dv.hldiv = 1.0e-10

            # Divertor lifetime (years)
            cv.divlife = max(0.0, min(cv.adivflnc / dv.hldiv, cv.tlife))

            # Centrepost lifetime (years) (ST machines only)
            if pv.itart == 1:
                # SC magnets CP lifetime
                # Rem : only the TF maximum fluence is considered for now
                if tfv.i_tf_sup == 1:
                    cv.cplife = min(
                        ctv.nflutfmax / (fwbsv.neut_flux_cp * YEAR), cv.tlife
                    )

                # Aluminium/Copper magnets CP lifetime
                # For now, we keep the original def, developped for GLIDCOP magnets ...
                else:
                    cv.cplife = min(cv.cpstflnc / pv.wallmw, cv.tlife)

        # Plant Availability (iavail=0,1)

        # if iavail = 0 use input value for cfactr

        # Taylor and Ward 1999 model (iavail=1)
        if cv.iavail == 1:
            # Which component has the shorter life?
            if cv.divlife < fwbsv.bktlife:
                ld = cv.divlife
                lb = fwbsv.bktlife
                td = cv.tdivrepl
            else:
                ld = fwbsv.bktlife
                lb = cv.divlife
                td = cv.tbktrepl

            # Number of outages between each combined outage
            n = math.ceil(lb / ld) - 1

            # Planned unavailability
            uplanned = (n * td + cv.tcomrepl) / ((n + 1) * ld + (n * td + cv.tcomrepl))

            # Unplanned unavailability
            # Rather than simply summing the individual terms, the following protects
            # against the total availability becoming zero or negative

            uutot = cv.uubop  # balance of plant
            uutot = uutot + (1.0e0 - uutot) * cv.uucd  # current drive
            uutot = uutot + (1.0e0 - uutot) * cv.uudiv  # divertor
            uutot = uutot + (1.0e0 - uutot) * cv.uufuel  # fuel system
            uutot = uutot + (1.0e0 - uutot) * cv.uufw  # first wall + blanket
            uutot = uutot + (1.0e0 - uutot) * cv.uumag  # magnets
            uutot = uutot + (1.0e0 - uutot) * cv.uuves  # vacuum vessel

            # Total availability
            cv.cfactr = 1.0e0 - (uplanned + uutot - (uplanned * uutot))

        # Capacity factor
        # Using the amount of time burning for a given pulse cycle
        cv.cpfact = cv.cfactr * (tv.tburn / tv.tcycle)

        # Modify lifetimes to take account of the availability
        if ifev.ife != 1:
            # First wall / blanket
            if fwbsv.bktlife < cv.tlife:
                fwbsv.bktlife = min(fwbsv.bktlife / cv.cfactr, cv.tlife)

            # Divertor
            if cv.divlife < cv.tlife:
                cv.divlife = min(cv.divlife / cv.cfactr, cv.tlife)

            # Centrepost
            if pv.itart == 1 and cv.cplife < cv.tlife:
                cv.cplife = min(cv.cplife / cv.cfactr, cv.tlife)

        # Current drive system lifetime (assumed equal to first wall and blanket lifetime)
        cv.cdrlife = fwbsv.bktlife

        # Output section
        if self.iprint != 1:
            return

        po.oheadr(self.outfile, "Plant Availability")
        if fwbsv.blktmodel == 0:
            po.ovarre(
                self.outfile,
                "Allowable blanket neutron fluence (MW-yr/m2)",
                "(abktflnc)",
                cv.abktflnc,
            )

        po.ovarre(
            self.outfile,
            "Allowable divertor heat fluence (MW-yr/m2)",
            "(adivflnc)",
            cv.adivflnc,
        )
        po.ovarre(
            self.outfile,
            "First wall / blanket lifetime (years)",
            "(bktlife)",
            fwbsv.bktlife,
            "OP ",
        )
        po.ovarre(
            self.outfile, "Divertor lifetime (years)", "(divlife)", cv.divlife, "OP "
        )

        if pv.itart == 1:
            po.ovarre(
                self.outfile,
                "Centrepost lifetime (years)",
                "(cplife)",
                cv.cplife,
                "OP ",
            )

        po.ovarre(
            self.outfile,
            "Heating/CD system lifetime (years)",
            "(cdrlife)",
            cv.cdrlife,
            "OP ",
        )
        po.ovarre(self.outfile, "Total plant lifetime (years)", "(tlife)", cv.tlife)

        if cv.iavail == 1:
            if cv.divlife < fwbsv.bktlife:
                po.ovarre(
                    self.outfile,
                    "Time needed to replace divertor (years)",
                    "(tdivrepl)",
                    cv.tdivrepl,
                )
            else:
                po.ovarre(
                    self.outfile,
                    "Time needed to replace blanket (years)",
                    "(tbktrepl)",
                    cv.tbktrepl,
                )

            po.ovarre(
                self.outfile,
                "Time needed to replace blkt + div (years)",
                "(tcomrepl)",
                cv.tcomrepl,
            )
            po.ovarre(
                self.outfile,
                "Planned unavailability fraction",
                "(uplanned)",
                uplanned,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Unplanned unavailability fraction",
                "(uutot)",
                uutot,
                "OP ",
            )

        if cv.iavail == 0:
            po.ovarre(
                self.outfile, "Total plant availability fraction", "(cfactr)", cv.cfactr
            )
        else:
            po.ovarre(
                self.outfile,
                "Total plant availability fraction",
                "(cfactr)",
                cv.cfactr,
                "OP ",
            )

    def avail_2(self):
        """Routine to calculate component lifetimes and the overall plant availability
        author: J Morris, CCFE, Culham Science Centre
        outfile : input integer : output file unit
        iprint : input integer : switch for writing to output file (1=yes)
        This routine calculates the component lifetimes and the overall
        plant availability using an updated model linked to the 2014 EUROfusion
        RAMI task
        2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
        """

        # Plant Availability

        # Planned unavailability

        u_planned = av.calc_u_planned(self.outfile, self.iprint)

        # Operational time (years)
        cv.t_operation = cv.tlife * (1.0e0 - u_planned)

        # Un-planned unavailability

        # Magnets
        u_unplanned_magnets = av.calc_u_unplanned_magnets(self.outfile, self.iprint)

        # Divertor
        u_unplanned_div = av.calc_u_unplanned_divertor(self.outfile, self.iprint)

        # First wall and blanket
        u_unplanned_fwbs = av.calc_u_unplanned_fwbs(self.outfile, self.iprint)

        # Balance of plant
        u_unplanned_bop = av.calc_u_unplanned_bop(self.outfile, self.iprint)

        # Heating and current drive
        u_unplanned_hcd = av.calc_u_unplanned_hcd()

        # Vacuum systems

        # Number of redundant pumps
        redun_vac = math.floor(vacv.vpumpn * cv.redun_vacp / 100.0 + 0.5e0)

        u_unplanned_vacuum = av.calc_u_unplanned_vacuum(self.outfile, self.iprint)

        # Total unplanned unavailability
        u_unplanned = min(
            1.0e0,
            u_unplanned_magnets
            + u_unplanned_div
            + u_unplanned_fwbs
            + u_unplanned_bop
            + u_unplanned_hcd
            + u_unplanned_vacuum,
        )

        # Total availability
        cv.cfactr = max(
            1.0e0 - (u_planned + u_unplanned + u_planned * u_unplanned), 0.0e0
        )

        # Capacity factor
        cpfact = cv.cfactr * (tv.tburn / tv.tcycle)

        # Output
        if self.iprint != 1:
            return

        po.ocmmnt(self.outfile, "Total unavailability:")
        po.oblnkl(self.outfile)
        po.ovarre(
            self.outfile,
            "Total planned unavailability",
            "(u_planned)",
            u_planned,
            "OP ",
        )
        po.ovarre(
            self.outfile,
            "Total unplanned unavailability",
            "(u_unplanned)",
            u_unplanned,
            "OP ",
        )
        po.oblnkl(self.outfile)
        po.ovarre(
            self.outfile,
            "Total plant availability fraction",
            "(cfactr)",
            cv.cfactr,
            "OP ",
        )
        po.ovarre(
            self.outfile,
            "Total DT operational time (years)",
            "(t_operation)",
            cv.t_operation,
            "OP ",
        )
        po.ovarre(self.outfile, "Total plant lifetime (years)", "(tlife)", tv.tlife)
        po.ovarre(
            self.outfile,
            "Capacity factor: total lifetime elec. energy output / output power",
            "(cpfact)",
            cpfact,
            "OP ",
        )
