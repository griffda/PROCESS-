import math
import logging

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

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)

DAY_SECONDS = 60 * 60 * 24
"""Number of seconds in a day [s]"""

DAYS_IN_YEAR = 365.25
"""Number of days in a year"""

YEAR_SECONDS = DAY_SECONDS * DAYS_IN_YEAR
"""Number of seconds in a year [s]"""


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
                        ctv.nflutfmax / (fwbsv.neut_flux_cp * YEAR_SECONDS), cv.tlife
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

        u_planned = self.calc_u_planned(self.outfile, self.iprint)

        # Operational time (years)
        cv.t_operation = cv.tlife * (1.0e0 - u_planned)

        # Un-planned unavailability

        # Magnets
        u_unplanned_magnets = self.calc_u_unplanned_magnets()

        # Divertor
        u_unplanned_div = self.calc_u_unplanned_divertor()

        # First wall and blanket
        u_unplanned_fwbs = self.calc_u_unplanned_fwbs()

        # Balance of plant
        u_unplanned_bop = self.calc_u_unplanned_bop()

        # Heating and current drive
        u_unplanned_hcd = av.calc_u_unplanned_hcd()

        # Vacuum systems

        # Number of redundant pumps
        cv.redun_vac = math.floor(vacv.vpumpn * cv.redun_vacp / 100.0 + 0.5e0)

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

    def calc_u_planned(self) -> float:
        """Calculates the planned unavailability of the plant
        author: J Morris, CCFE, Culham Science Centre

        This routine calculates the planned unavailability of the
        plant, using the methodology outlined in the 2014 EUROfusion
        RAMI report.
        2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;

        :return u_planned: planned unavailability of plant
        :type u_planned: float
        """

        # Full power lifetimes (in years) !

        # First wall / blanket lifetime (years)
        fwbsv.bktlife = min(cv.abktflnc / pv.wallmw, cv.tlife)

        # Divertor lifetime (years)
        cv.divlife = min(cv.adivflnc / dv.hldiv, cv.tlife)

        # Centrepost lifetime (years) (ST only)
        if pv.itart == 1:
            # SC magnets CP lifetime
            # Rem : only the TF maximum fluence is considered for now
            if tfv.i_tf_sup == 1:
                cv.cplife = min(
                    ctv.nflutfmax / (fwbsv.neut_flux_cp * YEAR_SECONDS), cv.tlife
                )

            # Aluminium/Copper magnets CP lifetime
            # For now, we keep the original def, developped for GLIDCOP magnets ...
            else:
                cv.cplife = min(cv.cpstflnc / pv.wallmw, cv.tlife)

        # Current drive lifetime (assumed equal to first wall and blanket lifetime)
        cv.cdrlife = fwbsv.bktlife

        # Calculate the blanket and divertor replacement times !

        # Blanket replacement time
        # ( Calculated using scaling from 2014 EUROfusion RAMI report )

        # Mean time to repair blanket is same as replacing both blanket and divertor.
        # The +2.0 at the end is for the 1 month cooldown and pump down at either end
        # of the maintenance period
        mttr_blanket = (21.0e0 * cv.num_rh_systems ** (-0.9e0) + 2.0e0) / 12.0e0

        # Mean time to repair divertor is 70% of time taken to replace blanket
        # This is taken from Oliver Crofts 2014 paper
        mttr_divertor = 0.7e0 * mttr_blanket

        #  Which component has the shorter life?
        if cv.divlife < fwbsv.bktlife:
            lifetime_shortest = cv.divlife
            lifetime_longest = fwbsv.bktlife
            mttr_shortest = mttr_divertor
        else:
            lifetime_shortest = fwbsv.bktlife
            lifetime_longest = cv.divlife
            mttr_shortest = mttr_blanket

        # Number of outages between each combined outage
        n = math.ceil(lifetime_longest / lifetime_shortest) - 1

        # Planned unavailability
        u_planned = (n * mttr_shortest + mttr_blanket) / (
            (n + 1) * lifetime_shortest + (n * mttr_shortest + mttr_blanket)
        )

        # Output
        if self.iprint == 1:

            po.oheadr(self.outfile, "Plant Availability (2014 Model)")

            po.ocmmnt(self.outfile, "Planned unavailability:")
            po.oblnkl(self.outfile)
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
                "First wall / blanket lifetime (FPY)",
                "(bktlife)",
                fwbsv.bktlife,
                "OP ",
            )
            po.ovarre(
                self.outfile, "Divertor lifetime (FPY)", "(divlife)", cv.divlife, "OP "
            )

            po.ovarin(
                self.outfile,
                "Number of remote handling systems",
                "(num_rh_systems)",
                cv.num_rh_systems,
            )
            po.ovarre(
                self.outfile,
                "Time needed to replace divertor (yrs)",
                "(mttr_divertor)",
                mttr_divertor,
            )
            po.ovarre(
                self.outfile,
                "Time needed to replace blanket (yrs)",
                "(mttr_blanket)",
                mttr_blanket,
            )
            po.ovarre(
                self.outfile,
                "Time needed to replace blkt + div (yrs)",
                "(mttr_blanket)",
                mttr_blanket,
            )
            po.ovarre(
                self.outfile,
                "Total planned unavailability",
                "(uplanned)",
                u_planned,
                "OP ",
            )
            po.oblnkl(self.outfile)

        return u_planned

    def calc_u_unplanned_magnets(self) -> float:
        """Calculates the unplanned unavailability of the magnets
        author: J Morris, CCFE, Culham Science Centre

        This routine calculates the unplanned unavailability of the magnets,
        using the methodology outlined in the 2014 EUROfusion
        RAMI report.
        2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;

        :return u_unplanned_magnets: unplanned unavailability of magnets
        :type u_unplanned_magnets: float
        """

        # Magnet temperature margin limit (K)
        # Use the lower of the two values.  Issue #526
        tmargmin = min(tfv.tmargmin_tf, tfv.tmargmin_cs)
        mag_temp_marg_limit = tmargmin

        # Magnet temperature margin (K)
        mag_temp_marg = tfv.temp_margin

        # Magnet maintenance time (years)
        mag_main_time = 0.5e0

        # Minimum unplanned unavailability
        mag_min_u_unplanned = mag_main_time / (cv.t_operation + mag_main_time)

        # Point at which risk of unplanned unavailability increases
        # conf_mag is the c factor, which determines the temperature margin at which
        # lifetime starts to decline.
        start_of_risk = mag_temp_marg_limit / cv.conf_mag

        # Determine if temperature margin is in region with risk of unplanned unavailability
        if tfv.temp_margin >= start_of_risk:
            u_unplanned_magnets = mag_min_u_unplanned
        else:
            # Linear decrease in expected lifetime when approaching the limit
            t_life = max(
                0.0e0,
                (cv.t_operation / (start_of_risk - tmargmin))
                * (tfv.temp_margin - tmargmin),
            )
            u_unplanned_magnets = mag_main_time / (t_life + mag_main_time)

        # Output !
        # !!!!!!!!!

        if self.iprint == 1:

            po.ocmmnt(self.outfile, "Magnets:")
            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile, "Minimum temperature margin (K)", "(tmargmin)", tmargmin
            )
            po.ovarre(
                self.outfile,
                "c parameter, determining the temp margin where lifetime declines",
                "(conf_mag)",
                cv.conf_mag,
            )
            po.ovarre(
                self.outfile,
                "Temperature Margin (K)",
                "(temp_margin)",
                tfv.temp_margin,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Magnets unplanned unavailability",
                "(u_unplanned_magnets)",
                u_unplanned_magnets,
                "OP ",
            )
            po.oblnkl(self.outfile)

        return u_unplanned_magnets

    def calc_u_unplanned_divertor(self) -> float:
        """Calculates the unplanned unavailability of the divertor
        author: J Morris, CCFE, Culham Science Centre

        :return u_unplanned_divertor: unplanned unavailability of the divertor
        :type u_unplanned_divertor: float
        """

        # Calculate cycle limit in terms of days
        # Number of cycles between planned blanket replacements, N
        n = cv.divlife * YEAR_SECONDS / tv.tcycle

        # The probability of failure in one pulse cycle (before the reference cycle life)
        pf = (cv.div_prob_fail / DAY_SECONDS) * tv.tcycle
        a0 = 1.0e0 - pf * cv.div_umain_time * YEAR_SECONDS / tv.tcycle

        # Integrating the instantaneous availability gives the mean
        # availability over the planned cycle life N
        if cv.div_nu <= cv.div_nref:
            logger.error(
                """div_nu <= div_nref
            The cycle when the divertor fails with 100% probability <= & Reference value for cycle life of divertor
            """
            )
            po.ocmmnt(
                self.outfile,
                "ERROR: The cycle when the divertor fails with 100% probability & <= Reference value for cycle cycle life of divertor",
            )

        # Check number of cycles

        # Less than reference (availability is min availability)
        if n <= cv.div_nref:
            div_avail = a0

        # Greater than cycle number with 100% failure rate
        elif n >= cv.div_nu:
            div_avail = 0.0e0

        # Else number of cycles is inbetween and is given by formula below
        else:
            div_avail = (a0 / (cv.div_nu - cv.div_nref)) * (
                cv.div_nu - 0.5e0 * cv.div_nref ** 2.0e0 / n - 0.5e0 * n
            )

        # Unplanned unavailability for divertor
        u_unplanned_div = 1.0e0 - div_avail

        # Output
        if self.iprint == 1:
            po.ocmmnt(self.outfile, "Divertor:")
            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile,
                "Probability of failure per operational day",
                "(div_prob_fail)",
                cv.div_prob_fail,
            )
            po.ovarre(
                self.outfile,
                "Repair time (years)",
                "(div_umain_time)",
                cv.div_umain_time,
            )
            po.ovarre(
                self.outfile,
                "Reference value for cycle life",
                "(div_nref)",
                cv.div_nref,
            )
            po.ovarre(
                self.outfile,
                "The cycle when failure is 100% certain",
                "(div_nu)",
                cv.div_nu,
            )
            po.ovarre(
                self.outfile, "Number of cycles between planned replacements", "(n)", n
            )
            po.ovarre(
                self.outfile,
                "Unplanned unavailability",
                "(u_unplanned_div)",
                u_unplanned_div,
                "OP ",
            )
            po.oblnkl(self.outfile)

        return u_unplanned_div

    def calc_u_unplanned_fwbs(self) -> float:
        """Calculates the unplanned unavailability of the first wall and blanket
        author: J Morris, CCFE, Culham Science Centre

        2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;

        :return u_unplanned_fwbs: unplanned unavailability of first wall and blanket
        :type u_unplanned_fwbs: float
        """

        # Calculate cycle limit in terms of days

        # Number of cycles between planned blanket replacements, N
        n = fwbsv.bktlife * YEAR_SECONDS / tv.tcycle

        # The probability of failure in one pulse cycle
        # (before the reference cycle life)
        pf = (cv.fwbs_prob_fail / DAY_SECONDS) * tv.tcycle
        a0 = 1.0e0 - pf * cv.fwbs_umain_time * YEAR_SECONDS / tv.tcycle

        if cv.fwbs_nu <= cv.fwbs_nref:
            logger.error(
                """fwbs_nu <= fwbs_nref
            The cycle when the blanket fails with 100% probability <= &Reference value for cycle life of blanket
            """
            )
            po.ocmmnt(
                self.outfile,
                "EROROR: The cycle when the blanket fails with 100% probability& <= Reference value for cycle life of blanket",
            )

        # Integrating the instantaneous availability gives the mean
        # availability over the planned cycle life N
        if n <= cv.fwbs_nref:
            fwbs_avail = a0
        elif n >= cv.fwbs_nu:
            fwbs_avail = 0.0e0
        else:
            fwbs_avail = (a0 / (cv.fwbs_nu - cv.fwbs_nref)) * (
                cv.fwbs_nu - 0.5e0 * cv.fwbs_nref ** 2.0e0 / n - 0.5e0 * n
            )

        # First wall / blanket unplanned unavailability
        u_unplanned_fwbs = 1.0e0 - fwbs_avail

        # Output
        if self.iprint == 1:
            po.ocmmnt(self.outfile, "First wall / Blanket:")
            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile,
                "Probability of failure per operational day",
                "(fwbs_prob_fail)",
                cv.fwbs_prob_fail,
            )
            po.ovarre(
                self.outfile,
                "Repair time (years)",
                "(fwbs_umain_time)",
                cv.fwbs_umain_time,
            )
            po.ovarre(
                self.outfile,
                "Reference value for cycle life",
                "(fwbs_nref)",
                cv.fwbs_nref,
            )
            po.ovarre(
                self.outfile,
                "The cycle when failure is 100% certain",
                "(fwbs_nu)",
                cv.fwbs_nu,
            )
            po.ovarre(
                self.outfile, "Number of cycles between planned replacements", "(n)", n
            )
            po.ovarre(
                self.outfile,
                "Unplanned unavailability",
                "(u_unplanned_fwbs)",
                u_unplanned_fwbs,
                "OP ",
            )
            po.oblnkl(self.outfile)

        return u_unplanned_fwbs

    def calc_u_unplanned_bop(self) -> float:
        """Calculates the unplanned unavailability of the balance of plant
        author: J Morris, CCFE, Culham Science Centre

        This routine calculates the unplanned unavailability of the balance of plant,
        using the methodology outlined in the 2014 EUROfusion
        RAMI report.
        2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;

        :return u_unplanned_bop: unplanned unavailability of balance of plant
        :type u_unplanned_bop: float
        """

        # Balance of plant failure rate (failures per hour)
        # ENEA study WP13-DTM02-T01
        bop_fail_rate = 9.39e-5

        # Number of balance of plant failures in plant operational lifetime
        bop_num_failures = math.ceil(
            bop_fail_rate * DAYS_IN_YEAR * 24.0e0 * cv.t_operation
        )

        # Balance of plant mean time to repair (years)
        # ENEA study WP13-DTM02-T01
        bop_mttr = 96.0e0 / (24.0e0 * DAYS_IN_YEAR)

        # Unplanned downtime balance of plant
        u_unplanned_bop = (bop_mttr * bop_num_failures) / (cv.t_operation)

        # Output
        if self.iprint == 1:
            po.ocmmnt(self.outfile, "Balance of plant:")
            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile, "Failure rate (1/h)", "(bop_fail_rate)", bop_fail_rate
            )
            po.ovarin(
                self.outfile,
                "Number of failures in lifetime",
                "(bop_num_failures)",
                bop_num_failures,
                "OP ",
            )
            po.ovarre(self.outfile, "Balance of plant MTTR", "(bop_mttr)", bop_mttr)
            po.ovarre(
                self.outfile,
                "Balance of plant unplanned unavailability",
                "(u_unplanned_bop)",
                u_unplanned_bop,
                "OP ",
            )
            po.oblnkl(self.outfile)

        return u_unplanned_bop
