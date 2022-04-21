import numpy
import logging

from process.fortran import rebco_variables
from process.fortran import global_variables
from process.fortran import superconductors
from process.fortran import tfcoil_variables
from process.fortran import physics_variables
from process.fortran import build_variables
from process.fortran import constants
from process.fortran import sctfcoil_module
from process.fortran import process_output as po
from process.fortran import error_handling

from process.utilities.f2py_string_patch import f2py_compatible_to_string

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


class Sctfcoil:
    def __init__(self):
        self.outfile = constants.nout

    def run(self, output: bool):
        """
        Routine to call the superconductor module for the TF coils
        """
        tfes = sctfcoil_module.estotft / tfcoil_variables.n_tf
        # Cross-sectional area per turn
        aturn = tfcoil_variables.ritfc / (
            tfcoil_variables.jwptf * tfcoil_variables.n_tf * tfcoil_variables.n_tf_turn
        )

        if tfcoil_variables.i_tf_sc_mat == 6:
            (tfcoil_variables.jwdgcrt, tfcoil_variables.tmargtf,) = self.supercon_croco(
                aturn,
                tfcoil_variables.bmaxtfrp,
                tfcoil_variables.cpttf,
                tfcoil_variables.tftmp,
                output=output,
            )

            tfcoil_variables.vtfskv = (
                self.croco_voltage() / 1.0e3
            )  # TFC Quench voltage in kV

        else:
            (
                tfcoil_variables.jwdgcrt,
                vdump,
                tfcoil_variables.tmargtf,
            ) = self.supercon(
                tfcoil_variables.acstf,
                aturn,
                tfcoil_variables.bmaxtfrp,
                tfcoil_variables.vftf,
                tfcoil_variables.fcutfsu,
                tfcoil_variables.cpttf,
                tfcoil_variables.jwptf,
                tfcoil_variables.i_tf_sc_mat,
                tfcoil_variables.fhts,
                tfcoil_variables.tdmptf,
                tfes,
                tfcoil_variables.tftmp,
                tfcoil_variables.tmaxpro,
                tfcoil_variables.bcritsc,
                tfcoil_variables.tcritsc,
                output=output,
            )

            tfcoil_variables.vtfskv = vdump / 1.0e3  # TFC Quench voltage in kV

    def croco_voltage(self) -> float:
        if f2py_compatible_to_string(tfcoil_variables.quench_model) == "linear":
            sctfcoil_module.time2 = tfcoil_variables.tdmptf
            croco_voltage = (
                2.0e0
                / sctfcoil_module.time2
                * (sctfcoil_module.estotft / tfcoil_variables.n_tf)
                / tfcoil_variables.cpttf
            )
        elif f2py_compatible_to_string(tfcoil_variables.quench_model) == "exponential":
            sctfcoil_module.tau2 = tfcoil_variables.tdmptf
            croco_voltage = (
                2.0e0
                / sctfcoil_module.tau2
                * (sctfcoil_module.estotft / tfcoil_variables.n_tf)
                / tfcoil_variables.cpttf
            )
        else:
            return 0.0

        return croco_voltage

    def supercon_croco(self, aturn, bmax, iop, thelium, output: bool):
        """TF superconducting CroCo conductor using REBCO tape
        author: M Kovari, CCFE, Culham Science Centre
        bmax : input real : Peak field at conductor (T)
        iop : input real : Operating current per turn (A)
        thelium : input real : He temperature at peak field point (K)
        iprint : input integer : Switch for printing (1 = yes, 0 = no)
        outfile : input integer : Fortran output unit identifier
        jwdgcrt : output real : Critical winding pack current density (A/m2)
        tmarg : output real : Temperature margin (K)
        """

        jcritsc: float = 0.0
        #  Find critical current density in superconducting strand, jcritstr
        jcritsc, _ = superconductors.jcrit_rebco(thelium, bmax, int(output))
        # tfcoil_variables.acstf : Cable space - inside area (m2)
        # Set new rebco_variables.croco_od
        # allowing for scaling of rebco_variables.croco_od
        rebco_variables.croco_od = (
            tfcoil_variables.t_conductor / 3.0e0
            - tfcoil_variables.thwcndut * (2.0e0 / 3.0e0)
        )
        sctfcoil_module.conductor_acs = (
            9.0e0 / 4.0e0 * numpy.pi * rebco_variables.croco_od**2
        )
        tfcoil_variables.acstf = sctfcoil_module.conductor_acs
        sctfcoil_module.conductor_area = (
            tfcoil_variables.t_conductor**2
        )  # does this not assume it's a sqaure???

        sctfcoil_module.conductor_jacket_area = (
            sctfcoil_module.conductor_area - sctfcoil_module.conductor_acs
        )
        tfcoil_variables.acndttf = sctfcoil_module.conductor_jacket_area

        sctfcoil_module.conductor_jacket_fraction = (
            sctfcoil_module.conductor_jacket_area / sctfcoil_module.conductor_area
        )
        superconductors.croco(
            jcritsc,
            sctfcoil_module.croco_strand_area,
            sctfcoil_module.croco_strand_critical_current,
            sctfcoil_module.conductor_copper_area,
            sctfcoil_module.conductor_copper_fraction,
            sctfcoil_module.conductor_copper_bar_area,
            sctfcoil_module.conductor_hastelloy_area,
            sctfcoil_module.conductor_hastelloy_fraction,
            sctfcoil_module.conductor_helium_area,
            sctfcoil_module.conductor_helium_fraction,
            sctfcoil_module.conductor_solder_area,
            sctfcoil_module.conductor_solder_fraction,
            sctfcoil_module.conductor_rebco_area,
            sctfcoil_module.conductor_rebco_fraction,
            sctfcoil_module.conductor_critical_current,
            sctfcoil_module.conductor_area,
            rebco_variables.croco_od,
            rebco_variables.croco_thick,
        )
        rebco_variables.coppera_m2 = iop / sctfcoil_module.conductor_copper_area

        icrit = sctfcoil_module.conductor_critical_current
        jcritstr = (
            sctfcoil_module.croco_strand_critical_current
            / sctfcoil_module.croco_strand_area
        )

        # Critical current density in winding pack
        # aturn : Area per turn (i.e. entire jacketed conductor with insulation) (m2)
        jwdgcrt = icrit / aturn
        #  Ratio of operating / critical current
        iooic = iop / icrit
        #  Operating current density
        jwdgop = iop / aturn
        #  Actual current density in superconductor,
        # which should be equal to jcrit(thelium+tmarg)

        #  when we have found the desired value of tmarg
        jsc = iooic * jcritsc

        # Temperature margin using secant solver
        current_sharing_t = superconductors.current_sharing_rebco(bmax, jsc)
        tmarg = current_sharing_t - thelium
        tfcoil_variables.temp_margin = (
            tmarg  # Only used in the availabilty routine - see comment to Issue #526
        )

        if output:  # Output ----------------------------------

            total = (
                sctfcoil_module.conductor_copper_area
                + sctfcoil_module.conductor_hastelloy_area
                + sctfcoil_module.conductor_solder_area
                + sctfcoil_module.conductor_jacket_area
                + sctfcoil_module.conductor_helium_area
                + sctfcoil_module.conductor_rebco_area
            )

            if tfcoil_variables.temp_margin <= 0.0e0:
                logger.warning(
                    f"""Negative TFC temperature margin
                temp_margin: {tfcoil_variables.temp_margin}
                bmax: {bmax}"""
                )

            po.oheadr(self.outfile, "Superconducting TF Coils")
            po.ovarin(self.outfile, "Superconductor switch", "(isumat)", 6)
            po.ocmmnt(
                self.outfile, "Superconductor used: REBCO HTS tape in CroCo strand"
            )

            po.ovarre(
                self.outfile,
                "Thickness of REBCO layer in tape (m)",
                "(rebco_thickness)",
                rebco_variables.rebco_thickness,
            )
            po.ovarre(
                self.outfile,
                "Thickness of copper layer in tape (m)",
                "(copper_thick  )",
                rebco_variables.copper_thick,
            )
            po.ovarre(
                self.outfile,
                "Thickness of Hastelloy layer in tape (m) ",
                "(hastelloy_thickness)",
                rebco_variables.hastelloy_thickness,
            )

            po.ovarre(
                self.outfile,
                "Mean width of tape (m)",
                "(tape_width)",
                rebco_variables.tape_width,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Outer diameter of CroCo copper tube (m) ",
                "(croco_od)",
                rebco_variables.croco_od,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Inner diameter of CroCo copper tube (m) ",
                "(croco_id)",
                rebco_variables.croco_id,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Thickness of CroCo copper tube (m) ",
                "(croco_thick)",
                rebco_variables.croco_thick,
            )

            po.ovarre(
                self.outfile,
                "Thickness of each HTS tape ",
                "(tape_thickness)",
                rebco_variables.tape_thickness,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Thickness of stack of rebco_variables.tapes (m) ",
                "(stack_thickness)",
                rebco_variables.stack_thickness,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Number of rebco_variables.tapes in strand",
                "(tapes)",
                rebco_variables.tapes,
                "OP ",
            )
            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile,
                "Area of REBCO in strand (m2)",
                "(rebco_area)",
                rebco_variables.rebco_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Area of copper in strand (m2)",
                "(copper_area)",
                rebco_variables.copper_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Area of hastelloy substrate in strand (m2) ",
                "(hastelloy_area)",
                rebco_variables.hastelloy_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Area of solder in strand (m2)  ",
                "(solder_area)",
                rebco_variables.solder_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Total: area of CroCo strand (m2)  ",
                "(croco_strand_area)",
                sctfcoil_module.croco_strand_area,
                "OP ",
            )
            if (
                abs(
                    sctfcoil_module.croco_strand_area
                    - (
                        rebco_variables.rebco_area
                        + rebco_variables.copper_area
                        + rebco_variables.hastelloy_area
                        + rebco_variables.solder_area
                    )
                )
                > 1e-6
            ):
                po.ocmmnt(self.outfile, "ERROR: Areas in CroCo strand do not add up")
                logger.warning("Areas in CroCo strand do not add up - see OUT.DAT")

            po.oblnkl(self.outfile)
            po.ocmmnt(self.outfile, "Cable information")
            po.ovarin(
                self.outfile,
                "Number of CroCo strands in the cable (fixed) ",
                "",
                6,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Total area of cable space (m2)",
                "(acstf)",
                tfcoil_variables.acstf,
                "OP ",
            )

            po.oblnkl(self.outfile)
            po.ocmmnt(
                self.outfile,
                "Conductor information (includes jacket, not including insulation)",
            )
            po.ovarre(
                self.outfile,
                "Width of square conductor (cable + steel jacket) (m)",
                "(t_conductor)",
                tfcoil_variables.t_conductor,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Area of conductor (m2)",
                "(area)",
                sctfcoil_module.conductor_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "REBCO area of conductor (mm2)",
                "(rebco_area)",
                sctfcoil_module.conductor_rebco_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Area of central copper bar (mm2)",
                "(copper_bar_area)",
                sctfcoil_module.conductor_copper_bar_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Total copper area of conductor, total (mm2)",
                "(copper_area)",
                sctfcoil_module.conductor_copper_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Hastelloy area of conductor (mm2)",
                "(hastelloy_area)",
                sctfcoil_module.conductor_hastelloy_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Solder area of conductor (mm2)",
                "(solder_area)",
                sctfcoil_module.conductor_solder_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Jacket area of conductor (mm2)",
                "(jacket_area)",
                sctfcoil_module.conductor_jacket_area,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Helium area of conductor (mm2)",
                "(helium_area)",
                sctfcoil_module.conductor_helium_area,
                "OP ",
            )
            if abs(total - sctfcoil_module.conductor_area) > 1e-8:
                po.ovarre(
                    self.outfile,
                    "ERROR: conductor areas do not add up:",
                    "(total)",
                    total,
                    "OP ",
                )
                logger.warning(f"conductor areas do not add up. total: {total}")

            po.ovarre(
                self.outfile,
                "Critical current of CroCo strand (A)",
                "(croco_strand_critical_current)",
                sctfcoil_module.croco_strand_critical_current,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Critical current of conductor (A) ",
                "(conductor_critical_current)",
                sctfcoil_module.conductor_critical_current,
                "OP ",
            )

            if global_variables.run_tests == 1:
                po.oblnkl(self.outfile)
                po.ocmmnt(
                    self.outfile,
                    "PROCESS TF Coil peak field fit. Values for t, z and y:",
                )
                po.oblnkl(self.outfile)
                po.ovarre(
                    self.outfile,
                    "Dimensionless winding pack width",
                    "(tf_fit_t)",
                    sctfcoil_module.tf_fit_t,
                    "OP ",
                )
                po.ovarre(
                    self.outfile,
                    "Dimensionless winding pack radial thickness",
                    "(tf_fit_z)",
                    sctfcoil_module.tf_fit_z,
                    "OP ",
                )
                po.ovarre(
                    self.outfile,
                    "Ratio of actual peak field to nominal axisymmetric peak field",
                    "(tf_fit_y)",
                    sctfcoil_module.tf_fit_y,
                    "OP ",
                )

            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile,
                "Helium temperature at peak field (= superconductor temperature) (K)",
                "(thelium)",
                thelium,
            )
            po.ovarre(
                self.outfile,
                "Critical current density in superconductor (A/m2)",
                "(jcritsc)",
                jcritsc,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Critical current density in strand (A/m2)",
                "(jcritstr)",
                jcritstr,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Critical current density in winding pack (A/m2)",
                "(jwdgcrt)",
                jwdgcrt,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Actual current density in winding pack (A/m2)",
                "(jwdgop)",
                jwdgop,
                "OP ",
            )

            po.ovarre(
                self.outfile,
                "Minimum allowed temperature margin in superconductor (K)",
                "(tmargmin_tf)",
                tfcoil_variables.tmargmin_tf,
            )
            po.ovarre(
                self.outfile,
                "Actual temperature margin in superconductor (K)",
                "(tmarg)",
                tmarg,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Current sharing temperature (K)",
                "(current_sharing_t)",
                current_sharing_t,
                "OP ",
            )
            po.ovarre(self.outfile, "Critical current (A)", "(icrit)", icrit, "OP ")
            po.ovarre(
                self.outfile,
                "Actual current (A)",
                "(cpttf)",
                tfcoil_variables.cpttf,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Actual current / critical current",
                "(iooic)",
                iooic,
                "OP ",
            )

        return jwdgcrt, tmarg

    def supercon(
        self,
        acs,
        aturn,
        bmax,
        fhe,
        fcu,
        iop,
        jwp,
        isumat,
        fhts,
        tdmptf,
        tfes,
        thelium,
        tmax,
        bcritsc,
        tcritsc,
        output: bool,
    ):
        """Routine to calculate the TF superconducting conductor  properties
        author: P J Knight, CCFE, Culham Science Centre
        author: J Galambos, ORNL
        author: R Kemp, CCFE, Culham Science Centre
        author: M Kovari, CCFE, Culham Science Centre
        author: J Miller, ORNL
        acs : input real : Cable space - inside area (m2)
        aturn : input real : Area per turn (i.e. entire jacketed conductor) (m2)
        bmax : input real : Peak field at conductor (T)
        fhe : input real : Fraction of cable space that is for He cooling
        fcu : input real : Fraction of conductor that is copper
        iop : input real : Operating current per turn (A)
        jwp : input real : Actual winding pack current density (A/m2)
        isumat : input integer : Switch for conductor type:
        1 = ITER Nb3Sn, standard parameters,
        2 = Bi-2212 High Temperature Superconductor,
        3 = NbTi,
        4 = ITER Nb3Sn, user-defined parameters
        5 = WST Nb3Sn parameterisation
        7 = Durham Ginzburg-Landau Nb-Ti parameterisation
        fhts    : input real : Adjustment factor (<= 1) to account for strain,
        radiation damage, fatigue or AC losses
        tdmptf : input real : Dump time (sec)
        tfes : input real : Energy stored in one TF coil (J)
        thelium : input real : He temperature at peak field point (K)
        tmax : input real : Max conductor temperature during quench (K)
        bcritsc : input real : Critical field at zero temperature and strain (T) (isumat=4 only)
        tcritsc : input real : Critical temperature at zero field and strain (K) (isumat=4 only)
        iprint : input integer : Switch for printing (1 = yes, 0 = no)
        outfile : input integer : Fortran output unit identifier
        jwdgpro : output real : Winding pack current density from temperature
        rise protection (A/m2)
        jwdgcrt : output real : Critical winding pack current density (A/m2)
        vd : output real : Discharge voltage imposed on a TF coil (V)
        tmarg : output real : Temperature margin (K)
        This routine calculates the superconductor properties for the TF coils.
        It was originally programmed by J. Galambos 1991, from algorithms provided
        by J. Miller.
        <P>The routine calculates the critical current density (winding pack)
        and also the protection information (for a quench).
        NOT used for the Croco conductor
        """
        tdump = tdmptf

        # Helium channel
        fhetot = (
            fhe
            + (numpy.pi / 4.0e0)
            * tfcoil_variables.dhecoil
            * tfcoil_variables.dhecoil
            / acs
        )
        #  Conductor fraction (including central helium channel)
        fcond = 1.0e0 - fhetot

        if tfcoil_variables.i_str_wp == 0:
            strain = tfcoil_variables.str_tf_con_res
        else:
            strain = tfcoil_variables.str_wp

        #  Find critical current density in superconducting strand, jcritstr

        if isumat == 1:  # ITER Nb3Sn critical surface parameterization
            bc20m = 32.97e0
            tc0m = 16.06e0
            # If strain limit achieved, throw a warning and use the lower strain
            if abs(strain) > 0.5e-2:
                error_handling.fdiags[0] = strain
                error_handling.report_error(261)
                strain = numpy.sign(strain) * 0.5e-2

            #  jcritsc returned by superconductors.itersc is the critical current density in the
            #  superconductor - not the whole strand, which contains copper
            jcritsc, bcrit, tcrit = superconductors.itersc(
                thelium, bmax, strain, bc20m, tc0m
            )
            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable
            icrit = jcritstr * acs * fcond

        elif isumat == 2:  # Bi-2212 high temperature superconductor parameterization

            #  Current density in a strand of Bi-2212 conductor
            #  N.B. jcrit returned by superconductors.bi2212 is the critical current density
            #  in the strand, not just the superconducting portion.
            #  The parameterization for jcritstr assumes a particular strand
            #  composition that does not require a user-defined copper fraction,
            #  so this is irrelevant in this model
            jstrand = jwp * aturn / (acs * fcond)

            jcritstr, tmarg = superconductors.bi2212(bmax, jstrand, thelium, fhts)
            jcritsc = jcritstr / (1.0e0 - fcu)
            tcrit = thelium + tmarg
            #  Critical current in cable
            icrit = jcritstr * acs * fcond

        elif isumat == 3:  # NbTi data
            bc20m = 15.0e0
            tc0m = 9.3e0
            c0 = 1.0e10
            jcritsc, tcrit = superconductors.jcrit_nbti(thelium, bmax, c0, bc20m, tc0m)
            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable
            icrit = jcritstr * acs * fcond

        elif isumat == 4:  # ITER Nb3Sn parameterization, but user-defined parameters
            bc20m = bcritsc
            tc0m = tcritsc
            # If strain limit achieved, throw a warning and use the lower strain
            if abs(strain) > 0.5e-2:
                error_handling.fdiags[0] = strain
                error_handling.report_error(261)
                strain = numpy.sign(strain) * 0.5e-2

            jcritsc, bcrit, tcrit = superconductors.itersc(
                thelium, bmax, strain, bc20m, tc0m
            )
            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable
            icrit = jcritstr * acs * fcond

        elif isumat == 5:  # WST Nb3Sn parameterisation
            bc20m = 32.97e0
            tc0m = 16.06e0
            # If strain limit achieved, throw a warning and use the lower strain
            if abs(strain) > 0.5e-2:
                error_handling.fdiags[0] = strain
                error_handling.report_error(261)
                strain = numpy.sign(strain) * 0.5e-2

            #  jcritsc returned by superconductors.itersc is the critical current density in the
            #  superconductor - not the whole strand, which contains copper
            jcritsc, bcrit, tcrit = superconductors.wstsc(
                thelium, bmax, strain, bc20m, tc0m
            )
            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable
            icrit = jcritstr * acs * fcond

        elif isumat == 6:  # "REBCO" 2nd generation HTS superconductor in CrCo strand
            raise ValueError(
                "sctfcoil.supercon has been called but tfcoil_variables.i_tf_sc_mat=6"
            )

        elif isumat == 7:  # Durham Ginzburg-Landau Nb-Ti parameterisation
            bc20m = tfcoil_variables.b_crit_upper_nbti
            tc0m = tfcoil_variables.t_crit_nbti
            jcritsc, bcrit, tcrit = superconductors.gl_nbti(
                thelium, bmax, strain, bc20m, tc0m
            )
            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable
            icrit = jcritstr * acs * fcond

        elif isumat == 8:  # Branch YCBO model fit to Tallahassee data
            bc20m = 430
            tc0m = 185
            # If strain limit achieved, throw a warning and use the lower strain
            if abs(strain) > 0.7e-2:
                error_handling.fdiags[0] = strain
                error_handling.report_error(261)
                strain = numpy.sign(strain) * 0.7e-2

            jcritsc, bcrit, tcrit = superconductors.GL_REBCO(
                thelium, bmax, strain, bc20m, tc0m
            )
            # A0 calculated for tape cross section already
            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable (copper added at this stage in HTS cables)
            icrit = jcritstr * acs * fcond

        elif isumat == 9:  # High Current Density REBCO tape
            bc20m = 138
            tc0m = 92
            # If strain limit achieved, throw a warning and use the lower strain
            if abs(strain) > 0.7e-2:
                error_handling.fdiags[0] = strain
                error_handling.report_error(261)
                strain = numpy.sign(strain) * 0.7e-2

            # 'high current density' as per parameterisation described in Wolf,
            #  and based on Hazelton experimental data and Zhai conceptual model;
            #  see subroutine for full references
            jcritsc, bcrit, tcrit = superconductors.HIJC_REBCO(
                thelium, bmax, strain, bc20m, tc0m
            )

            jcritstr = jcritsc * (1.0e0 - fcu)
            #  Critical current in cable (copper added at this stage in HTS cables)
            icrit = jcritstr * acs * fcond

        else:
            error_handling.idiags[0] = isumat
            error_handling.report_error(105)

        # Critical current density in winding pack
        # aturn : Area per turn (i.e. entire jacketed conductor with insulation) (m2)
        jwdgcrt = icrit / aturn
        #  Ratio of operating / critical current
        iooic = iop / icrit
        #  Operating current density
        jwdgop = iop / aturn
        #  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
        #  when we have found the desired value of tmarg
        jsc = iooic * jcritsc

        if iooic <= 0e0:
            logger.warning(
                f"""Negative Iop/Icrit for TF coil
            jsc: {jsc}
            iooic: {iooic}
            jcritsc: {jcritsc}
            Check conductor dimensions. fcond likely gone negative. fcond: {fcond}
            """
            )

        # REBCO measurements from 2 T to 14 T, extrapolating outside this
        if (isumat == 8) and (tfcoil_variables.bmaxtfrp >= 14):
            error_handling.report_error(266)

        #  Temperature margin (already calculated in superconductors.bi2212 for isumat=2)
        if (
            (isumat == 1)
            or (isumat == 3)
            or (isumat == 4)
            or (isumat == 5)
            or (isumat == 7)
            or (isumat == 8)
            or (isumat == 9)
        ):

            #  Newton-Raphson method; start approx at requested minimum temperature margin
            ttest = thelium + tfcoil_variables.tmargmin_tf + 0.001e0
            delt = 0.01e0
            jtol = 1.0e4

            for lap in range(100):
                if ttest <= 0.0e0:
                    error_handling.idiags[0] = lap
                    error_handling.fdiags[1] = ttest
                    error_handling.report_error(157)
                    break

                # Calculate derivative numerically
                ttestm = ttest - delt
                ttestp = ttest + delt

                # Issue #483 to be on the safe side, check the fractional as well as the absolute error
                if isumat in (1, 2, 3, 4):
                    jcrit0, b, t = superconductors.itersc(
                        ttest, bmax, strain, bc20m, tc0m
                    )
                    if (abs(jsc - jcrit0) <= jtol) and (
                        abs((jsc - jcrit0) / jsc) <= 0.01
                    ):
                        break
                    jcritm, b, t = superconductors.itersc(
                        ttestm, bmax, strain, bc20m, tc0m
                    )
                    jcritp, b, t = superconductors.itersc(
                        ttestp, bmax, strain, bc20m, tc0m
                    )
                elif isumat == 3:
                    jcrit0, t = superconductors.jcrit_nbti(ttest, bmax, c0, bc20m, tc0m)
                    if (abs(jsc - jcrit0) <= jtol) and (
                        abs((jsc - jcrit0) / jsc) <= 0.01
                    ):
                        break
                    jcritm, t = superconductors.jcrit_nbti(
                        ttestm, bmax, c0, bc20m, tc0m
                    )
                    jcritp, t = superconductors.jcrit_nbti(
                        ttestp, bmax, c0, bc20m, tc0m
                    )
                elif isumat == 5:
                    jcrit0, b, t = superconductors.wstsc(
                        ttest, bmax, strain, bc20m, tc0m
                    )
                    if (abs(jsc - jcrit0) <= jtol) and (
                        abs((jsc - jcrit0) / jsc) <= 0.01
                    ):
                        break
                    jcritm, b, t = superconductors.wstsc(
                        ttestm, bmax, strain, bc20m, tc0m
                    )
                    jcritp, b, t = superconductors.wstsc(
                        ttestp, bmax, strain, bc20m, tc0m
                    )
                elif isumat == 7:
                    jcrit0, b, t = superconductors.gl_nbti(
                        ttest, bmax, strain, bc20m, tc0m
                    )
                    if (abs(jsc - jcrit0) <= jtol) and (
                        abs((jsc - jcrit0) / jsc) <= 0.01
                    ):
                        break
                    jcritm, b, t = superconductors.gl_nbti(
                        ttestm, bmax, strain, bc20m, tc0m
                    )
                    jcritp, b, t = superconductors.gl_nbti(
                        ttestp, bmax, strain, bc20m, tc0m
                    )
                elif isumat == 8:
                    jcrit0, b, t = superconductors.GL_REBCO(
                        ttest, bmax, strain, bc20m, tc0m
                    )
                    if (abs(jsc - jcrit0) <= jtol) and (
                        abs((jsc - jcrit0) / jsc) <= 0.01
                    ):
                        break
                    jcritm, b, t = superconductors.GL_REBCO(
                        ttestm, bmax, strain, bc20m, tc0m
                    )
                    jcritp, b, t = superconductors.GL_REBCO(
                        ttestp, bmax, strain, bc20m, tc0m
                    )
                elif isumat == 9:
                    jcrit0, b, t = superconductors.HIJC_REBCO(
                        ttest, bmax, strain, bc20m, tc0m
                    )
                    if (abs(jsc - jcrit0) <= jtol) and (
                        abs((jsc - jcrit0) / jsc) <= 0.01
                    ):
                        break
                    jcritm, b, t = superconductors.HIJC_REBCO(
                        ttestm, bmax, strain, bc20m, tc0m
                    )
                    jcritp, b, t = superconductors.HIJC_REBCO(
                        ttestp, bmax, strain, bc20m, tc0m
                    )

                ttest = ttest - 2.0e0 * delt * (jcrit0 - jsc) / (jcritp - jcritm)
            else:
                error_handling.idiags[0] = lap
                error_handling.fdiags[1] = ttest
                error_handling.report_error(157)

            tmarg = ttest - thelium
            tfcoil_variables.temp_margin = tmarg

        #  Find the current density limited by the protection limit
        #  (N.B. Unclear of this routine's relevance for Bi-2212 (isumat=2), due
        #  to presence of fcu argument, which is not used for this model above)

        tfcoil_variables.jwdgpro, vd = self.protect(
            iop, tfes, acs, aturn, tdump, fcond, fcu, thelium, tmax
        )

        if output:  # Output --------------------------
            if ttest <= 0.0e0:
                logger.warning(
                    """Negative TFC temperature margin
                ttest: {ttest}
                bmax: {bmax}
                jcrit0: {jcrit0}
                jsc: {jsc}
                ttestp: {ttestp}
                ttestm: {ttestm}
                jcritp: {jcritp}
                jcritm: {jcritm}
                """
                )

            po.oheadr(self.outfile, "Superconducting TF Coils")
            po.ovarin(self.outfile, "Superconductor switch", "(isumat)", isumat)

            if isumat == 1:
                po.ocmmnt(self.outfile, "Superconductor used: Nb3Sn")
                po.ocmmnt(self.outfile, "  (ITER Jcrit model, standard parameters)")
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )
            if isumat == 2:
                po.ocmmnt(self.outfile, "Superconductor used: Bi-2212 HTS")
            if isumat == 3:
                po.ocmmnt(self.outfile, "Superconductor used: NbTi")
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )
            if isumat == 4:
                po.ocmmnt(self.outfile, "Superconductor used: Nb3Sn")
                po.ocmmnt(self.outfile, "  (ITER Jcrit model, user-defined parameters)")
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )
            if isumat == 5:
                po.ocmmnt(self.outfile, "Superconductor used: Nb3Sn")
                po.ocmmnt(self.outfile, " (WST Nb3Sn critical surface model)")
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )
            if isumat == 7:
                po.ocmmnt(self.outfile, "Superconductor used: Nb-Ti")
                po.ocmmnt(
                    self.outfile, " (Durham Ginzburg-Landau critical surface model)"
                )
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )
            if isumat == 8:
                po.ocmmnt(self.outfile, "Superconductor used: REBCO")
                po.ocmmnt(
                    self.outfile, " (Durham Ginzburg-Landau critical surface model)"
                )
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )
            if isumat == 9:
                po.ocmmnt(self.outfile, "Superconductor used: REBCO")
                po.ocmmnt(
                    self.outfile,
                    " (Hazelton experimental data + Zhai conceptual model)",
                )
                po.ovarre(
                    self.outfile,
                    "Critical field at zero temperature and strain (T)",
                    "(bc20m)",
                    bc20m,
                )
                po.ovarre(
                    self.outfile,
                    "Critical temperature at zero field and strain (K)",
                    "(tc0m)",
                    tc0m,
                )

            if global_variables.run_tests == 1:
                po.oblnkl(self.outfile)
                po.ocmmnt(
                    self.outfile,
                    "PROCESS TF Coil peak field fit. Values for t, z and y:",
                )
                po.oblnkl(self.outfile)
                po.ovarre(
                    self.outfile,
                    "Dimensionless winding pack width",
                    "(tf_fit_t)",
                    sctfcoil_module.tf_fit_t,
                    "OP ",
                )
                po.ovarre(
                    self.outfile,
                    "Dimensionless winding pack radial thickness",
                    "(tf_fit_z)",
                    sctfcoil_module.tf_fit_z,
                    "OP ",
                )
                po.ovarre(
                    self.outfile,
                    "Ratio of peak field with ripple to nominal axisymmetric peak field",
                    "(tf_fit_y)",
                    sctfcoil_module.tf_fit_y,
                    "OP ",
                )

            po.oblnkl(self.outfile)
            po.ovarre(
                self.outfile,
                "Helium temperature at peak field (= superconductor temperature) (K)",
                "(thelium)",
                thelium,
            )
            po.ovarre(
                self.outfile,
                "Total helium fraction inside cable space",
                "(fhetot)",
                fhetot,
                "OP ",
            )
            po.ovarre(self.outfile, "Copper fraction of conductor", "(fcutfsu)", fcu)
            po.ovarre(
                self.outfile,
                "Residual manufacturing strain on superconductor",
                "(str_tf_con_res)",
                tfcoil_variables.str_tf_con_res,
            )
            po.ovarre(
                self.outfile,
                "Self-consistent strain on superconductor",
                "(str_wp)",
                tfcoil_variables.str_wp,
            )
            po.ovarre(
                self.outfile,
                "Critical current density in superconductor (A/m2)",
                "(jcritsc)",
                jcritsc,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Critical current density in strand (A/m2)",
                "(jcritstr)",
                jcritstr,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Critical current density in winding pack (A/m2)",
                "(jwdgcrt)",
                jwdgcrt,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Actual current density in winding pack (A/m2)",
                "(jwdgop)",
                jwdgop,
                "OP ",
            )

            po.ovarre(
                self.outfile,
                "Minimum allowed temperature margin in superconductor (K)",
                "(tmargmin_tf)",
                tfcoil_variables.tmargmin_tf,
            )
            po.ovarre(
                self.outfile,
                "Actual temperature margin in superconductor (K)",
                "(tmarg)",
                tmarg,
                "OP ",
            )
            po.ovarre(self.outfile, "Critical current (A)", "(icrit)", icrit, "OP ")
            po.ovarre(
                self.outfile,
                "Actual current (A)",
                "(cpttf)",
                tfcoil_variables.cpttf,
                "OP ",
            )
            po.ovarre(
                self.outfile,
                "Actual current / critical current",
                "(iooic)",
                iooic,
                "OP ",
            )

        return jwdgcrt, vd, tmarg

    def sctfcoil(self, output: bool):
        """TF coil module
        author: P J Knight, CCFE, Culham Science Centre
        author: J Galambos, FEDC/ORNL
        author: R Kemp, CCFE, Culham Science Centre
        author: J Morris, CCFE, Culham Science Centre
        author: S Kahn, CCFE, Culham Science Centre
        This subroutine calculates various parameters for a TF coil set.
        The primary outputs are coil size, shape, weight, stress and and fields.
        It is a variant from the original FEDC/Tokamak systems code.
        """
        peaktfflag = 0

        self.tf_global_geometry()

        # Calculation of the TF current from bt
        self.tf_current()

        # Conductor section internal geometry
        # ---
        # Superconducting magnets
        if tfcoil_variables.i_tf_sup == 1:
            sctfcoil_module.sc_tf_internal_geom(
                tfcoil_variables.i_tf_wp_geom,
                tfcoil_variables.i_tf_case_geom,
                tfcoil_variables.i_tf_turns_integer,
            )

        # Resitive magnets
        else:
            self.res_tf_internal_geom()

        # ---

        # Coil vertical geometry
        self.coilshap()

        # TF resistive heating (res TF only)
        if tfcoil_variables.i_tf_sup != 1:
            self.tf_res_heating()

        # Vertical force
        self.tf_field_and_force()

        # TF coil inductance
        # ---
        if physics_variables.itart == 0 and tfcoil_variables.i_tf_shape == 1:
            self.tfcind(build_variables.tfcth)
        else:
            tfcoil_variables.tfind = (
                (build_variables.hmax + build_variables.tfthko)
                * constants.rmu0
                / constants.pi
                * numpy.log(
                    build_variables.r_tf_outboard_mid / build_variables.r_tf_inboard_mid
                )
            )

        # Total TF coil stored magnetic energy [J]
        sctfcoil_module.estotft = (
            0.5e0 * tfcoil_variables.tfind * tfcoil_variables.ritfc**2
        )

        # Total TF coil stored magnetic energy [Gigajoule]
        tfcoil_variables.estotftgj = 1.0e-9 * sctfcoil_module.estotft
        # ---

        # Calculate TF coil areas and masses
        sctfcoil_module.tf_coil_area_and_masses()

        # Peak field including ripple
        # Rem : as resistive magnets are axisymmetric, no inboard ripple is present
        if tfcoil_variables.i_tf_sup == 1:
            tfcoil_variables.bmaxtfrp, peaktfflag = sctfcoil_module.peak_tf_with_ripple(
                tfcoil_variables.n_tf,
                tfcoil_variables.wwp1,
                tfcoil_variables.dr_tf_wp
                - 2.0e0 * (tfcoil_variables.tinstf + tfcoil_variables.tfinsgap),
                sctfcoil_module.r_wp_centre,
                tfcoil_variables.bmaxtf,
            )
        else:
            tfcoil_variables.bmaxtfrp = tfcoil_variables.bmaxtf

        # Do stress calculations (writes the stress output)
        if output:
            tfcoil_variables.n_rad_per_layer = 500

        sctfcoil_module.stresscl(
            tfcoil_variables.n_tf_stress_layers,
            tfcoil_variables.n_rad_per_layer,
            int(output),
            self.outfile,
        )

        if output:
            sctfcoil_module.outtf(self.outfile, peaktfflag)

    def protect(self, aio, tfes, acs, aturn, tdump, fcond, fcu, tba, tmax):
        """Finds the current density limited by the protection limit
        author: P J Knight, CCFE, Culham Science Centre
        author: J Miller, ORNL
        aio : input real : Operating current (A)
        tfes : input real : Energy stored in one TF coil (J)
        acs : input real : Cable space - inside area (m2)
        aturn : input real : Area per turn (i.e.  entire cable) (m2)
        tdump : input real : Dump time (sec)
        fcond : input real : Fraction of cable space containing conductor
        fcu : input real : Fraction of conductor that is copper
        tba : input real : He temperature at peak field point (K)
        tmax : input real : Max conductor temperature during quench (K)
        ajwpro : output real :  Winding pack current density from temperature
        rise protection (A/m2)
        vd : output real :  Discharge voltage imposed on a TF coil (V)
        This routine calculates maximum conductor current density which
        limits the peak temperature in the winding to a given limit (tmax).
        It also finds the dump voltage.
        <P>These calculations are based on Miller's formulations.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        # Integration coefficients p1,p2,p3
        p1 = (
            0.0e0,
            0.8e0,
            1.75e0,
            2.4e0,
            2.7e0,
            2.95e0,
            3.1e0,
            3.2e0,
            3.3e0,
            3.4e0,
            3.5e0,
        )
        p2 = (
            0.0e0,
            0.05e0,
            0.5e0,
            1.4e0,
            2.6e0,
            3.7e0,
            4.6e0,
            5.3e0,
            5.95e0,
            6.55e0,
            7.1e0,
        )
        p3 = (
            0.0e0,
            0.05e0,
            0.5e0,
            1.4e0,
            2.6e0,
            3.7e0,
            4.6e0,
            5.4e0,
            6.05e0,
            6.8e0,
            7.2e0,
        )

        #  Dump voltage

        vd = 2.0e0 * tfes / (tdump * aio)

        #  Current density limited by temperature rise during quench

        tav = 1.0e0 + (tmax - tba) / 20.0e0
        no = int(tav)
        np = no + 1
        np = min(np, 11)

        ai1 = 1.0e16 * (p1[no - 1] + (p1[np - 1] - p1[no - 1]) * (tav - no))
        ai2 = 1.0e16 * (p2[no - 1] + (p2[np - 1] - p2[no - 1]) * (tav - no))
        ai3 = 1.0e16 * (p3[no - 1] + (p3[np - 1] - p3[no - 1]) * (tav - no))

        aa = vd * aio / tfes
        bb = (1.0e0 - fcond) * fcond * fcu * ai1
        cc = (fcu * fcond) ** 2 * ai2
        dd = (1.0e0 - fcu) * fcu * fcond**2 * ai3
        ajcp = numpy.sqrt(aa * (bb + cc + dd))
        ajwpro = ajcp * (acs / aturn)

        return ajwpro, vd

    def tf_global_geometry(self):
        """Subroutine for calculating the TF coil geometry
        This includes:
        - Overall geometry of coil (radii and toroidal planes area)
        - Winding Pack NOT included
        """

        sctfcoil_module.theta_coil = numpy.pi / tfcoil_variables.n_tf
        sctfcoil_module.tan_theta_coil = numpy.tan(sctfcoil_module.theta_coil)

        # TF coil inboard legs mid-plane cross-section area (WP + casing ) [m2]
        if tfcoil_variables.i_tf_case_geom == 0:
            # Circular front case
            tfcoil_variables.tfareain = numpy.pi * (
                build_variables.r_tf_inboard_out**2
                - build_variables.r_tf_inboard_in**2
            )
        else:
            # Straight front case
            tfcoil_variables.tfareain = (
                tfcoil_variables.n_tf
                * numpy.sin(sctfcoil_module.theta_coil)
                * numpy.cos(sctfcoil_module.theta_coil)
                * build_variables.r_tf_inboard_out**2
                - numpy.pi * build_variables.r_tf_inboard_in**2
            )

        # Vertical distance from the midplane to the top of the tapered section [m]
        if physics_variables.itart == 1:
            sctfcoil_module.h_cp_top = (
                physics_variables.rminor * physics_variables.kappa
                + tfcoil_variables.dztop
            )
        # ---

        # Outer leg geometry
        # ---
        # Mid-plane inner/out radial position of the TF coil outer leg [m]

        sctfcoil_module.r_tf_outboard_in = (
            build_variables.r_tf_outboard_mid - build_variables.tfthko * 0.5e0
        )
        sctfcoil_module.r_tf_outboard_out = (
            build_variables.r_tf_outboard_mid + build_variables.tfthko * 0.5e0
        )

        # TF coil width in toroidal direction at inboard leg outer edge [m]
        # ***
        # Sliding joints geometry
        if physics_variables.itart == 1 and tfcoil_variables.i_tf_sup != 1:
            tfcoil_variables.tftort = (
                2.0e0 * build_variables.r_cp_top * numpy.sin(sctfcoil_module.theta_coil)
            )

        # Default thickness, initially written for DEMO SC magnets
        elif physics_variables.itart == 1 and tfcoil_variables.i_tf_sup == 1:
            tfcoil_variables.tftort = (
                2.0e0
                * build_variables.r_tf_inboard_out
                * numpy.sin(sctfcoil_module.theta_coil)
            )
        else:
            tfcoil_variables.tftort = (
                2.0e0
                * build_variables.r_tf_inboard_out
                * numpy.sin(sctfcoil_module.theta_coil)
            )

        # Area of rectangular cross-section TF outboard leg [m2]
        tfcoil_variables.arealeg = tfcoil_variables.tftort * build_variables.tfthko
        # ---

    def tf_current(self):
        """
        Calculation of the maximum B field and the corresponding TF current
        """
        if tfcoil_variables.casthi_is_fraction:
            tfcoil_variables.casthi = (
                tfcoil_variables.casthi_fraction * build_variables.tfcth
            )

        # Case thickness of side wall [m]
        if tfcoil_variables.tfc_sidewall_is_fraction:
            tfcoil_variables.casths = (
                tfcoil_variables.casths_fraction
                * (build_variables.r_tf_inboard_in + tfcoil_variables.thkcas)
                * numpy.tan(numpy.pi / tfcoil_variables.n_tf)
            )

        # Radial position of peak toroidal field [m]
        if tfcoil_variables.i_tf_sup == 1:
            # SC : conservative assumption as the radius is calculated with the
            # WP radial distances defined at the TF middle (cos)
            tfcoil_variables.rbmax = (
                build_variables.r_tf_inboard_out * numpy.cos(sctfcoil_module.theta_coil)
                - tfcoil_variables.casthi
                - tfcoil_variables.tinstf
                - tfcoil_variables.tfinsgap
            )
        else:
            # Resistive coils : No approx necessary as the symmetry is cylindrical
            # The turn insulation th (tfcoil_variables.thicndut) is also subtracted too here
            tfcoil_variables.rbmax = (
                build_variables.r_tf_inboard_out
                - tfcoil_variables.casthi
                - tfcoil_variables.thicndut
                - tfcoil_variables.tinstf
            )

        # Calculation of the maximum B field on the magnet [T]
        tfcoil_variables.bmaxtf = (
            physics_variables.bt * physics_variables.rmajor / tfcoil_variables.rbmax
        )

        # Total current in TF coils [A]
        # rem SK : ritcf is no longer an input
        tfcoil_variables.ritfc = (
            tfcoil_variables.bmaxtf * tfcoil_variables.rbmax * 5.0e6
        )

        # Current per TF coil [A]
        sctfcoil_module.tfc_current = tfcoil_variables.ritfc / tfcoil_variables.n_tf

        # Global inboard leg average current in TF coils [A/m2]
        tfcoil_variables.oacdcp = tfcoil_variables.ritfc / tfcoil_variables.tfareain

    def coilshap(self):
        """Calculates the TF coil shape
        Calculates the shape of the INSIDE of the TF coil. The coil is
        approximated by a straight inboard section and four elliptical arcs
        This is a totally ad hoc model, with no physics or engineering basis.

        The referenced equations can be found in draft/unpublished document
        attached in GitLab to issue #1328.
        """
        FSTRAIGHT = 0.6
        if tfcoil_variables.i_tf_shape == 1 and physics_variables.itart == 0:
            # PROCESS D-shape parametrisation

            # X position of the arcs, eq(21)
            # The tfcoil_variables.xarc/tfcoil_variables.yarc are defined in the INSIDE part of the TF
            tfcoil_variables.xarc[0] = build_variables.r_tf_inboard_out
            tfcoil_variables.xarc[1] = (
                physics_variables.rmajor - 0.2e0 * physics_variables.rminor
            )
            tfcoil_variables.xarc[2] = sctfcoil_module.r_tf_outboard_in
            tfcoil_variables.xarc[3] = tfcoil_variables.xarc[1]
            tfcoil_variables.xarc[4] = tfcoil_variables.xarc[0]

            # Height of straight section as a fraction of the coil inner height
            if physics_variables.i_single_null == 0:
                # Double null
                tfcoil_variables.yarc[0] = FSTRAIGHT * build_variables.hmax
                tfcoil_variables.yarc[1] = build_variables.hmax
                tfcoil_variables.yarc[2] = 0
                tfcoil_variables.yarc[3] = -build_variables.hmax
                tfcoil_variables.yarc[4] = -FSTRAIGHT * build_variables.hmax
            else:
                # Single null
                tfcoil_variables.yarc[0] = FSTRAIGHT * (
                    build_variables.hpfu - build_variables.tfcth
                )
                tfcoil_variables.yarc[1] = build_variables.hpfu - build_variables.tfcth
                tfcoil_variables.yarc[2] = 0
                tfcoil_variables.yarc[3] = -build_variables.hmax
                tfcoil_variables.yarc[4] = -FSTRAIGHT * build_variables.hmax

            # Horizontal and vertical radii of inside edge of TF coil
            # Arcs are numbered clockwise:
            # 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard
            # 'tfleng' is the length of the coil midline.
            tfcoil_variables.tfleng = (
                tfcoil_variables.yarc[0] - tfcoil_variables.yarc[4]
            )
            for ii in range(4):
                tfcoil_variables.tfa[ii] = abs(
                    tfcoil_variables.xarc[ii + 1] - tfcoil_variables.xarc[ii]
                )
                tfcoil_variables.tfb[ii] = abs(
                    tfcoil_variables.yarc[ii + 1] - tfcoil_variables.yarc[ii]
                )
                # Radii and length of midline of coil segments
                aa = tfcoil_variables.tfa[ii] + 0.5e0 * build_variables.tfcth
                bb = tfcoil_variables.tfb[ii] + 0.5e0 * build_variables.tfcth
                tfcoil_variables.tfleng = (
                    tfcoil_variables.tfleng + 0.25e0 * self.circumference(aa, bb)
                )
                # note: final tfcoil_variables.tfleng includes inboard leg length; eq(22)

        # Centrepost with D-shaped
        # ---
        elif tfcoil_variables.i_tf_shape == 1 and physics_variables.itart == 1:

            # X position of the arcs, eq(23) and text before it
            tfcoil_variables.yarc[0] = build_variables.r_cp_top
            tfcoil_variables.yarc[1] = (
                physics_variables.rmajor - 0.2e0 * physics_variables.rminor
            )
            tfcoil_variables.yarc[2] = sctfcoil_module.r_tf_outboard_in
            tfcoil_variables.yarc[3] = tfcoil_variables.xarc[1]
            tfcoil_variables.yarc[4] = tfcoil_variables.xarc[0]

            # Double null, eq(23) and text before it
            tfcoil_variables.yarc[0] = build_variables.hpfu - build_variables.tfcth
            tfcoil_variables.yarc[1] = build_variables.hpfu - build_variables.tfcth
            tfcoil_variables.yarc[2] = 0
            tfcoil_variables.yarc[3] = -build_variables.hmax
            tfcoil_variables.yarc[4] = -build_variables.hmax

            # TF middle circumference
            tfcoil_variables.tfleng = 2 * (
                tfcoil_variables.xarc[1] - tfcoil_variables.xarc[0]
            )

            for ii in range(1, 3):
                tfcoil_variables.tfa[ii] = abs(
                    tfcoil_variables.xarc[ii + 1] - tfcoil_variables.xarc[ii]
                )
                tfcoil_variables.tfb[ii] = abs(
                    tfcoil_variables.yarc[ii + 1] - tfcoil_variables.yarc[ii]
                )

                # Radii and length of midline of coil segments
                aa = tfcoil_variables.tfa[ii] + 0.5e0 * build_variables.tfthko
                bb = tfcoil_variables.tfb[ii] + 0.5e0 * build_variables.tfthko
                tfcoil_variables.tfleng = (
                    tfcoil_variables.tfleng + 0.25e0 * self.circumference(aa, bb)
                )
                # IMPORTANT : THE CENTREPOST LENGTH IS NOT INCLUDED IN TFLENG FOR TART; eq(24)
        # ---

        # Picture frame coil
        # ---
        elif tfcoil_variables.i_tf_shape == 2:

            # X position of the arcs
            if physics_variables.itart == 0:
                tfcoil_variables.xarc[0] = build_variables.r_tf_inboard_out
            if physics_variables.itart == 1:
                tfcoil_variables.xarc[0] = build_variables.r_cp_top
            tfcoil_variables.xarc[1] = sctfcoil_module.r_tf_outboard_in
            tfcoil_variables.xarc[2] = tfcoil_variables.xarc[1]
            tfcoil_variables.xarc[3] = tfcoil_variables.xarc[1]
            tfcoil_variables.xarc[4] = tfcoil_variables.xarc[0]

            # Y position of the arcs
            tfcoil_variables.yarc[0] = build_variables.hpfu - build_variables.tfcth
            tfcoil_variables.yarc[1] = build_variables.hpfu - build_variables.tfcth
            tfcoil_variables.yarc[2] = 0
            tfcoil_variables.yarc[3] = -build_variables.hmax
            tfcoil_variables.yarc[4] = -build_variables.hmax

            # TF middle circumference
            # IMPORTANT : THE CENTREPOST LENGTH IS NOT INCLUDED IN TFLENG FOR TART
            if physics_variables.itart == 0:
                tfcoil_variables.tfleng = 2.0e0 * (
                    2.0e0 * build_variables.hmax
                    + build_variables.tfcth
                    + build_variables.r_tf_outboard_mid
                    - build_variables.r_tf_inboard_mid
                )  # eq(25)
            elif physics_variables.itart == 1:
                tfcoil_variables.tfleng = (
                    build_variables.hmax
                    + build_variables.hpfu
                    + 2.0e0
                    * (build_variables.r_tf_outboard_mid - build_variables.r_cp_top)
                )  # eq(26)

        # ---

    @staticmethod
    def circumference(aaa, bbb):
        """Calculate ellipse arc circumference using Ramanujan approximation (m)
        See https://www.johndcook.com/blog/2013/05/05/ramanujan-circumference-ellipse/
        for a discussion of the precision of the formula

        An ellipse has the following formula: (x/a)² + (y/b)² = 1

        :param aaa: the value of a in the formula of the ellipse.
        :type aaa: float

        :param bbb: the value of b in the formula of the ellipse.
        :type bbb: float

        :returns: an approximation of the circumference of the ellipse
        :rtype: float
        """
        hh = (aaa - bbb) ** 2 / (aaa + bbb) ** 2
        return (
            numpy.pi
            * (aaa + bbb)
            * (1.0e0 + (3.0e0 * hh) / (10.0e0 + numpy.sqrt(4.0e0 - 3.0e0 * hh)))
        )

    def tf_res_heating(self):
        """
        Resitive magnet resitive heating calculations
        Rem SK : Clamped joined superconductors might have resistive power losses on the joints
        Rem SK : Sliding joints might have a region of high resistivity
        """
        if tfcoil_variables.i_tf_sup == 0:
            tfcoil_variables.rhocp = (
                (tfcoil_variables.frhocp / 0.92e0)
                * (1.72e0 + 0.0039e0 * (tfcoil_variables.tcpav - 273.15e0))
                * 1.0e-8
            )

        # Aluminium
        if tfcoil_variables.i_tf_sup == 2:
            tfcoil_variables.rhocp = tfcoil_variables.frhocp * (
                2.00016e-14 * tfcoil_variables.tcpav**3
                - 6.75384e-13 * tfcoil_variables.tcpav**2
                + 8.89159e-12 * tfcoil_variables.tcpav
            )

        # Calculations dedicated for configurations with CP
        if physics_variables.itart == 1:

            # Tricky trick to make the leg / CP tempearture the same
            if (
                abs(tfcoil_variables.tlegav + 1.0e0)
                < numpy.finfo(float(tfcoil_variables.tlegav)).eps
            ):
                sctfcoil_module.is_leg_cp_temp_same = 1
                tfcoil_variables.tlegav = tfcoil_variables.tcpav

            # Leg resistivity (different leg temperature as separate cooling channels)
            if tfcoil_variables.i_tf_sup == 0:
                tfcoil_variables.rhotfleg = (
                    (tfcoil_variables.frholeg / 0.92e0)
                    * (1.72e0 + 0.0039e0 * (tfcoil_variables.tlegav - 273.15e0))
                    * 1.0e-8
                )
            elif tfcoil_variables.i_tf_sup == 2:
                tfcoil_variables.rhotfleg = tfcoil_variables.frholeg * (
                    2.00016e-14 * tfcoil_variables.tlegav**3
                    - 6.75384e-13 * tfcoil_variables.tlegav**2
                    + 8.89159e-12 * tfcoil_variables.tlegav
                )

            # Tricky trick to make the leg / CP tempearture the same
            if sctfcoil_module.is_leg_cp_temp_same == 1:
                tfcoil_variables.tlegav = -1.0e0

            # Centrepost resisitivity and conductor/insulation volume

            (
                tfcoil_variables.a_cp_cool,
                tfcoil_variables.vol_cond_cp,
                tfcoil_variables.prescp,
                sctfcoil_module.vol_ins_cp,
                sctfcoil_module.vol_case_cp,
                sctfcoil_module.vol_gr_ins_cp,
            ) = self.cpost(
                build_variables.r_tf_inboard_in,
                build_variables.r_tf_inboard_out,
                build_variables.r_cp_top,
                sctfcoil_module.h_cp_top,
                build_variables.hmax + build_variables.tfthko,
                tfcoil_variables.thkcas,
                tfcoil_variables.casthi,
                tfcoil_variables.tinstf,
                tfcoil_variables.thicndut,
                tfcoil_variables.n_tf_turn,
                tfcoil_variables.ritfc,
                tfcoil_variables.rhocp,
                tfcoil_variables.fcoolcp,
            )

        # Leg cross-section areas
        # Rem : For physics_variables.itart = 1, these quantitire corresponds to the outer leg only
        # ---
        # Leg ground insulation area per coil [m2]
        sctfcoil_module.a_leg_gr_ins = tfcoil_variables.arealeg - (
            tfcoil_variables.tftort - 2.0e0 * tfcoil_variables.tinstf
        ) * (build_variables.tfthko - 2.0e0 * tfcoil_variables.tinstf)

        # Outboard leg turns insulation area per coil [m2]
        sctfcoil_module.a_leg_ins = 2.0e0 * tfcoil_variables.thicndut * (
            tfcoil_variables.tftort - 2.0e0 * tfcoil_variables.tinstf
        ) + 2.0e0 * tfcoil_variables.thicndut * tfcoil_variables.n_tf_turn * (
            build_variables.tfthko
            - 2.0e0 * (tfcoil_variables.thicndut + tfcoil_variables.tinstf)
        )  # toroidal direction + radial direction

        # Exact TF outboard leg conductor area per coil [m2]
        sctfcoil_module.a_leg_cond = (1.0e0 - tfcoil_variables.fcoolleg) * (
            tfcoil_variables.arealeg
            - sctfcoil_module.a_leg_gr_ins
            - sctfcoil_module.a_leg_ins
        )
        # ---

        if physics_variables.itart == 1:

            # Outer leg resistive power loss
            # ---
            # TF outboard leg's resistance calculation (per leg) [ohm]
            tfcoil_variables.tflegres = (
                tfcoil_variables.rhotfleg
                * tfcoil_variables.tfleng
                / sctfcoil_module.a_leg_cond
            )

            # TF outer leg resistive power (TOTAL) [W]
            tfcoil_variables.presleg = (
                tfcoil_variables.tflegres
                * tfcoil_variables.ritfc**2
                / tfcoil_variables.n_tf
            )
            # ---

            # Sliding joints resistive heating
            # ---
            if tfcoil_variables.i_cp_joints != 0:

                # Number of contact area per joint (all legs)
                n_contact_tot = (
                    tfcoil_variables.n_tf_joints_contact
                    * numpy.round(tfcoil_variables.n_tf_turn)
                    * numpy.round(tfcoil_variables.n_tf)
                )

                # Area of joint contact (all legs)
                a_joints = (
                    build_variables.tfthko
                    * tfcoil_variables.th_joint_contact
                    * n_contact_tot
                )

                # Total joints resistive power losses
                tfcoil_variables.pres_joints = (
                    tfcoil_variables.n_tf_joints
                    * tfcoil_variables.rho_tf_joints
                    * tfcoil_variables.ritfc**2
                    / a_joints
                )
            else:
                # Joints resistance to be evaluated for SC
                tfcoil_variables.pres_joints = 0.0e0

            # ---

        # Case of a resistive magnet without joints
        # ***
        else:

            # TF resistive powers
            tfcoil_variables.prescp = (
                tfcoil_variables.rhocp
                * tfcoil_variables.ritfc**2
                * tfcoil_variables.tfleng
                / (sctfcoil_module.a_leg_cond * tfcoil_variables.n_tf)
            )

            # tfcoil_variables.prescp containts the the total resistive power losses
            tfcoil_variables.presleg = 0.0e0

            # No joints if physics_variables.itart = 0
            tfcoil_variables.pres_joints = 0.0e0

    def cpost(
        self,
        r_tf_inboard_in,
        r_tf_inboard_out,
        r_cp_top,
        ztop,
        hmaxi,
        cas_in_th,
        cas_out_th,
        gr_ins_th,
        ins_th,
        n_tf_turn,
        curr,
        rho,
        fcool,
    ):
        """
        author: P J Knight, CCFE, Culham Science Centre
        Calculates the volume and resistive power losses of a TART centrepost
        This routine calculates the volume and resistive power losses
        of a TART centrepost. It is assumed to be tapered - narrowest at
        the midplane and reaching maximum thickness at the height of the
        plasma. Above/below the plasma, the centrepost is cylindrical.
        The shape of the taper is assumed to be an arc of a circle.
        P J Knight, CCFE, Culham Science Centre
        21/10/96 PJK Initial version
        08/05/12 PJK Initial F90 version
        16/10/12 PJK Added constants; removed argument pi
        26/06/14 PJK Added error handling
        12/11/19 SK Using fixed cooling cross-section area along the CP
        26/11/19 SK added the coolant area, the conuctor/isulator/outer casing volume
        30/11/20 SK added the ground outer ground insulation volume
        F/MI/PJK/LOGBOOK12, pp.33,34
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        yy_ins = numpy.zeros((101,))  # Exact conductor area (to be integrated)
        yy_cond = numpy.zeros((101,))  # Turn insulation area (to be integrated)
        yy_gr_ins = numpy.zeros(
            (101,)
        )  # Outter ground insulation area (to be integrated)
        yy_casout = numpy.zeros((101,))  # Outter case area (to be integrated)

        rtop = r_cp_top - cas_out_th - gr_ins_th

        # Conductor outer radius at CP mid-plane [m]
        rmid = r_tf_inboard_out - cas_out_th - gr_ins_th

        # Conductor inner radius [m]
        r_tfin_inleg = r_tf_inboard_in + cas_in_th + gr_ins_th
        # -#

        #  Error traps
        # ------------
        if rtop <= 0.0e0:
            error_handling.fdiags[0] = rtop
            error_handling.report_error(115)

        if ztop <= 0.0e0:
            error_handling.fdiags[0] = ztop
            error_handling.report_error(116)

        if rmid <= 0.0e0:
            error_handling.fdiags[0] = rmid
            error_handling.report_error(117)

        if build_variables.hmax <= 0.0e0:
            error_handling.fdiags[0] = build_variables.hmax
            error_handling.report_error(118)

        if (fcool < 0.0e0) or (fcool > 1.0e0):
            error_handling.fdiags[0] = fcool
            error_handling.report_error(119)

        if rtop < rmid:
            error_handling.fdiags[0] = rtop
            error_handling.fdiags[1] = rmid
            error_handling.report_error(120)

        if build_variables.hmax < ztop:
            error_handling.fdiags[0] = build_variables.hmax
            error_handling.fdiags[1] = ztop
            error_handling.report_error(121)

        # ------------

        # Mid-plane area calculations
        # ---------------------------
        # Total number of CP turns
        n_turns_tot = tfcoil_variables.n_tf * n_tf_turn

        # Area of the innner TF central hole [m2]
        a_tfin_hole = numpy.pi * r_tfin_inleg**2

        # Mid-plane outer casing cross-section area [m2]
        a_casout = numpy.pi * (
            (rmid + gr_ins_th + cas_out_th) ** 2 - (rmid + gr_ins_th) ** 2
        )

        # Mid-plane outter ground insulation thickness [m2]
        a_cp_gr_ins = (
            numpy.pi * ((rmid + gr_ins_th) ** 2 - rmid**2)
            + 2.0e0 * gr_ins_th * (rmid - r_tfin_inleg) * tfcoil_variables.n_tf
        )

        # Mid-plane turn layer cross-section area [m2]
        a_cp_ins = (
            numpy.pi
            * ((r_tfin_inleg + ins_th) ** 2 - r_tfin_inleg**2)  # Inner layer volume
            + numpy.pi * (rmid**2 - (rmid - ins_th) ** 2)  # Outter layer volume
            + 2.0e0 * n_turns_tot * ins_th * (rmid - r_tfin_inleg - 2.0e0 * ins_th)
        )  # inter turn separtion

        # Cooling pipes cross-section per coil [m2]
        a_cp_cool = fcool * (
            (numpy.pi * rmid**2 - a_tfin_hole - a_cp_ins) / tfcoil_variables.n_tf
            - 2.0e0 * gr_ins_th * (rmid - r_tfin_inleg)
        )  # Wedge ground insulation
        # ---------------------------

        #  Trivial solutions
        # ------------------
        if abs(fcool) < numpy.finfo(float(fcool)).eps:
            vol_cond_cp = 0.0e0
            respow = 0.0e0
            vol_case_cp = 0.0e0
            vol_gr_ins_cp = 0.0e0
            vol_ins_cp = 0.0e0
            error_handling.report_error(122)
            return (
                a_cp_cool,
                vol_cond_cp,
                respow,
                vol_ins_cp,
                vol_case_cp,
                vol_gr_ins_cp,
            )

        if abs(rmid - rtop) < numpy.finfo(float(rtop)).eps:

            # Exact conductor cross-section
            a_cond_midplane = (
                numpy.pi * rmid**2
                - a_tfin_hole
                - tfcoil_variables.n_tf * a_cp_cool
                - a_cp_ins
            )

            # Volumes and resisitive losses calculations
            vol_cond_cp = 2.0e0 * hmaxi * a_cond_midplane
            vol_ins_cp = 2.0e0 * hmaxi * a_cp_ins
            vol_gr_ins_cp = 2.0e0 * hmaxi * a_cp_gr_ins
            respow = 2.0e0 * hmaxi * curr**2 * rho / a_cond_midplane
            vol_case_cp = 2.0e0 * hmaxi * a_casout

            return (
                a_cp_cool,
                vol_cond_cp,
                respow,
                vol_ins_cp,
                vol_case_cp,
                vol_gr_ins_cp,
            )

        # ------------------

        # Find centre of circle (RC,0) defining the taper's arc
        # (r1,z1) is midpoint of line joining (rmid,0) and (rtop,ztop)
        # Rem : The taper arc is defined using the outer radius of the
        #       conductor including turn unsulation
        # -------------------------------------------------------------
        r1 = 0.5e0 * (rmid + rtop)
        z1 = 0.5e0 * ztop

        x = (r1 - rmid) ** 2 + z1**2
        y = ztop**2 / ((rtop - rmid) ** 2 + ztop**2)

        rc = rmid + numpy.sqrt(x / (1.0e0 - y))
        # -------------------------------------------------------------

        #  Find volume of tapered section of centrepost, and the resistive
        #  power losses, by integrating along the centrepost from the midplane
        # --------------------------------------------------------------------
        #  Calculate centrepost radius and cross-sectional areas at each Z
        dz = 0.01e0 * ztop

        for ii in range(101):
            z = ii * dz
            z = min(z, ztop)

            r = rc - numpy.sqrt((rc - rmid) ** 2 - z * z)

            if r <= 0.0e0:
                error_handling.fdiags[0] = r
                error_handling.fdiags[1] = rc
                error_handling.fdiags[2] = rmid
                error_handling.fdiags[3] = z

                error_handling.report_error(123)

            # Insulation cross-sectional area at z
            yy_ins[ii] = (
                numpy.pi * ((r_tfin_inleg + ins_th) ** 2 - r_tfin_inleg**2)
                + numpy.pi * (r**2 - (r - ins_th) ** 2)  # Inner layer volume
                + 2.0e0  # Outter layer volume
                * ins_th
                * (r - r_tfin_inleg - 2.0e0 * ins_th)
                * n_turns_tot
            )  # inter turn layers

            #  Conductor cross-sectional area at z
            yy_cond[ii] = (
                numpy.pi * r**2
                - a_tfin_hole
                - tfcoil_variables.n_tf * a_cp_cool
                - yy_ins[ii]
                - 2.0e0 * tfcoil_variables.n_tf * gr_ins_th * (r - r_tfin_inleg)
            )  # Wedge ground insulation

            #  Outer ground insulation area at z
            yy_gr_ins[ii] = numpy.pi * (
                (r + gr_ins_th) ** 2 - r**2
            ) + 2.0e0 * tfcoil_variables.n_tf * gr_ins_th * (r - r_tfin_inleg)

            #  Outer casing Cross-sectional area at z
            yy_casout[ii] = numpy.pi * (
                (r + gr_ins_th + cas_out_th) ** 2 - (r + gr_ins_th) ** 2
            )

        #  Perform integrals using trapezium rule
        sum1 = 0.0e0
        sum2 = 0.0e0
        sum3 = 0.0e0
        sum4 = 0.0e0
        sum5 = 0.0e0
        for ii in range(1, 100):
            sum1 = sum1 + yy_cond[ii]
            sum2 = sum2 + 1.0e0 / yy_cond[ii]
            sum3 = sum3 + yy_ins[ii]
            sum4 = sum4 + yy_casout[ii]
            sum5 = sum5 + yy_gr_ins[ii]

        sum1 = 0.5e0 * dz * (yy_cond[0] + yy_cond[100] + 2.0e0 * sum1)
        sum2 = 0.5e0 * dz * (1.0e0 / yy_cond[0] + 1.0e0 / yy_cond[100] + 2.0e0 * sum2)
        sum3 = 0.5e0 * dz * (yy_ins[0] + yy_ins[100] + 2.0e0 * sum3)
        sum4 = 0.5e0 * dz * (yy_casout[0] + yy_casout[100] + 2.0e0 * sum4)
        sum5 = 0.5e0 * dz * (yy_gr_ins[0] + yy_gr_ins[100] + 2.0e0 * sum5)

        # Turn insulation layer cross section at CP top  [m2]
        a_cp_ins = (
            numpy.pi * ((r_tfin_inleg + ins_th) ** 2 - r_tfin_inleg**2)
            + numpy.pi * (rtop**2 - (rtop - ins_th) ** 2)  # Inner layer volume
            + 2.0e0  # Outter layer volume
            * ins_th
            * (rtop - r_tfin_inleg - 2.0e0 * ins_th)
            * n_turns_tot
        )  # turn separtion layers

        # Ground insulation layer cross-section at CP top [m2]
        a_cp_gr_ins = (
            numpy.pi * ((rtop + gr_ins_th) ** 2 - rtop**2)
            + 2.0e0 * gr_ins_th * (rtop - r_tfin_inleg) * tfcoil_variables.n_tf
        )

        # Outer casing cross-section area at CP top [m2]
        a_casout = numpy.pi * (
            (rmid + gr_ins_th + cas_out_th) ** 2 - (rmid + gr_ins_th) ** 2
        )

        # Centrepost volume (ignoring coolant fraction) [m3]
        vol_cond_cp = 2.0e0 * sum1 + 2.0e0 * (  # Tapered section
            hmaxi - ztop
        ) * (  # Straight section vertical height
            numpy.pi * rtop**2
            - a_tfin_hole
            - a_cp_ins
            - tfcoil_variables.n_tf * a_cp_cool
            - 2.0e0 * tfcoil_variables.n_tf * gr_ins_th * (rtop - r_tfin_inleg)
        )  # subtracting ground insulation wedge separation

        # Resistive power losses in taped section (variable radius section) [W]
        res_taped = rho * curr**2 * sum2

        # Centrepost insulator volume [m3]
        vol_ins_cp = 2.0e0 * (sum3 + (hmaxi - ztop) * a_cp_ins)

        # Ground insulation volume [m3]
        vol_gr_ins_cp = 2.0e0 * (
            sum5
            + (hmaxi - ztop) * a_cp_gr_ins
            + hmaxi * numpy.pi * (r_tfin_inleg**2 - (r_tfin_inleg - gr_ins_th) ** 2)
        )

        # CP casing volume [m3]
        vol_case_cp = 2.0e0 * (
            sum4
            + (hmaxi - ztop) * a_casout
            + hmaxi
            * numpy.pi
            * (
                (r_tfin_inleg - gr_ins_th) ** 2
                - (r_tfin_inleg - gr_ins_th - cas_in_th) ** 2
            )
        )

        # Resistive power losses in cylindrical section (constant radius) [W]
        res_cyl = (
            rho
            * curr**2
            * (
                (hmaxi - ztop)
                / (
                    numpy.pi * rtop**2
                    - a_tfin_hole
                    - a_cp_ins
                    - tfcoil_variables.n_tf * a_cp_cool
                    - 2.0e0 * tfcoil_variables.n_tf * gr_ins_th * (rtop - r_tfin_inleg)
                )
            )
        )  # ground insulation separation

        # Total CP resistive power [W]
        respow = 2.0e0 * (res_cyl + res_taped)

        return (
            a_cp_cool,
            vol_cond_cp,
            respow,
            vol_ins_cp,
            vol_case_cp,
            vol_gr_ins_cp,
        )

    def tf_field_and_force(self):
        """
        Calculate the TF coil field, force and VV quench consideration, and the resistive magnets resistance/volume
        """
        if tfcoil_variables.i_tf_sup == 1:
            tfcoil_variables.taucq = (
                physics_variables.bt
                * tfcoil_variables.ritfc
                * physics_variables.rminor
                * physics_variables.rminor
            ) / (build_variables.r_vv_inboard_out * tfcoil_variables.sigvvall)

        # Outer/inner WP radius removing the ground insulation layer and the insertion gap [m]
        if tfcoil_variables.i_tf_sup == 1:
            r_out_wp = (
                sctfcoil_module.r_wp_outer
                - tfcoil_variables.tinstf
                - tfcoil_variables.tfinsgap
            )
            r_in_wp = (
                sctfcoil_module.r_wp_inner
                + tfcoil_variables.tinstf
                + tfcoil_variables.tfinsgap
            )
        else:
            r_out_wp = sctfcoil_module.r_wp_outer - tfcoil_variables.tinstf
            r_in_wp = sctfcoil_module.r_wp_inner + tfcoil_variables.tinstf

        # Associated WP thickness
        dr_wp = r_out_wp - r_in_wp

        # In plane forces
        # ---
        # Centering force = net inwards radial force per meters per TF coil [N/m]
        tfcoil_variables.cforce = (
            0.5e0
            * tfcoil_variables.bmaxtf
            * tfcoil_variables.ritfc
            / tfcoil_variables.n_tf
        )

        # Vertical force per coil [N]
        # ***
        # Rem : this force does not depends on the TF shape or the presence of
        #        sliding joints, the in/outboard vertical tension repartition is
        # -#
        # Ouboard leg WP plasma side radius without ground insulation/insertion gat [m]
        if tfcoil_variables.i_tf_sup == 1:
            r_in_outwp = (
                sctfcoil_module.r_tf_outboard_in
                + tfcoil_variables.casthi
                + tfcoil_variables.tinstf
                + tfcoil_variables.tfinsgap
            )
        else:
            r_in_outwp = sctfcoil_module.r_tf_outboard_in + tfcoil_variables.tinstf

        # If the TF coil has no bore it would induce division by 0.
        # In this situation, the bore radius is set to a very small value : 1.0e-9 m
        if abs(r_in_wp) < numpy.finfo(float(r_in_wp)).eps:
            r_in_wp = 1.0e-9

        # May the force be with you
        vforce_tot = (
            0.5e0
            * (physics_variables.bt * physics_variables.rmajor * tfcoil_variables.ritfc)
            / (tfcoil_variables.n_tf * dr_wp**2)
            * (
                r_out_wp**2 * numpy.log(r_out_wp / r_in_wp)
                + r_in_outwp**2 * numpy.log((r_in_outwp + dr_wp) / r_in_outwp)
                + dr_wp**2 * numpy.log((r_in_outwp + dr_wp) / r_in_wp)
                - dr_wp * (r_out_wp + r_in_outwp)
                + 2.0e0
                * dr_wp
                * (
                    r_out_wp * numpy.log(r_in_wp / r_out_wp)
                    + r_in_outwp * numpy.log((r_in_outwp + dr_wp) / r_in_outwp)
                )
            )
        )

        # Case of a centrepost (physics_variables.itart == 1) with sliding joints (the CP vertical are separated from the leg ones)
        # Rem SK : casing/insulation thickness not subtracted as part of the CP is genuinely connected to the legs..
        if physics_variables.itart == 1 and tfcoil_variables.i_cp_joints == 1:

            # CP vertical tension [N]
            tfcoil_variables.vforce = (
                0.25e0
                * (
                    physics_variables.bt
                    * physics_variables.rmajor
                    * tfcoil_variables.ritfc
                )
                / (tfcoil_variables.n_tf * dr_wp**2)
                * (
                    2.0e0 * r_out_wp**2 * numpy.log(r_out_wp / r_in_wp)
                    + 2.0e0 * dr_wp**2 * numpy.log(build_variables.r_cp_top / r_in_wp)
                    + 3.0e0 * dr_wp**2
                    - 2.0e0 * dr_wp * r_out_wp
                    + 4.0e0 * dr_wp * r_out_wp * numpy.log(r_in_wp / r_out_wp)
                )
            )

            # Vertical tension applied on the outer leg [N]
            tfcoil_variables.vforce_outboard = vforce_tot - tfcoil_variables.vforce

            # Inboard vertical tension fraction
            tfcoil_variables.f_vforce_inboard = tfcoil_variables.vforce / vforce_tot

        # Case of TF without joints or with clamped joints vertical tension
        else:

            # Inboard vertical tension [N]
            tfcoil_variables.vforce = tfcoil_variables.f_vforce_inboard * vforce_tot

            # Ouboard vertical tension [N]
            tfcoil_variables.vforce_outboard = tfcoil_variables.vforce * (
                (1.0e0 / tfcoil_variables.f_vforce_inboard) - 1.0e0
            )

        # ***

        # Total vertical force
        sctfcoil_module.vforce_inboard_tot = (
            tfcoil_variables.vforce * tfcoil_variables.n_tf
        )

    def tfcind(self, tfthk):
        """Calculates the self inductance of a TF coil
        This routine calculates the self inductance of a TF coil
        approximated by a straight inboard section and two elliptical arcs.
        The inductance of the TFC (considered as a single axisymmetric turn)
        is calculated by numerical integration over the cross-sectional area.
        The contribution from the cross-sectional area of the
        coil itself is calculated by taking the field as B(r)/2.
        The field in the bore is calculated for unit current.
        Top/bottom symmetry is assumed.

        :param tfthk: TF coil thickness (m)
        :type tfthk: float
        """
        NINTERVALS = 100

        tfcoil_variables.tfind = 0.0e0
        # Integrate over the whole TF area, including the coil thickness.
        x0 = tfcoil_variables.xarc[1]
        y0 = tfcoil_variables.yarc[1]

        # Minor and major radii of the inside and outside perimeters of the the
        # Inboard leg and arc.
        # Average the upper and lower halves, which are different in the
        # single null case
        ai = tfcoil_variables.xarc[1] - tfcoil_variables.xarc[0]
        bi = (
            tfcoil_variables.yarc[1] - tfcoil_variables.yarc[3]
        ) / 2.0e0 - tfcoil_variables.yarc[0]
        ao = ai + tfthk
        bo = bi + tfthk
        # Interval used for integration
        dr = ao / NINTERVALS
        # Start both integrals from the centre-point where the arcs join.
        # Initialise major radius
        r = x0 - dr / 2.0e0

        for i in range(NINTERVALS):
            # Field in the bore for unit current
            b = constants.rmu0 / (2.0e0 * numpy.pi * r)
            # Find out if there is a bore
            if x0 - r < ai:
                h_bore = y0 + bi * numpy.sqrt(1 - ((r - x0) / ai) ** 2)
                h_thick = bo * numpy.sqrt(1 - ((r - x0) / ao) ** 2) - h_bore
            else:
                h_bore = 0.0e0
                # Include the contribution from the straight section
                h_thick = (
                    bo * numpy.sqrt(1 - ((r - x0) / ao) ** 2) + tfcoil_variables.yarc[0]
                )

            # Assume B in TF coil = 1/2  B in bore
            # Multiply by 2 for upper and lower halves of coil
            tfcoil_variables.tfind = tfcoil_variables.tfind + b * dr * (
                2.0e0 * h_bore + h_thick
            )
            r = r - dr

        # Outboard arc
        ai = tfcoil_variables.xarc[2] - tfcoil_variables.xarc[1]
        bi = (tfcoil_variables.yarc[1] - tfcoil_variables.yarc[3]) / 2.0e0
        ao = ai + tfthk
        bo = bi + tfthk
        dr = ao / NINTERVALS
        # Initialise major radius
        r = x0 + dr / 2.0e0

        for i in range(NINTERVALS):
            # Field in the bore for unit current
            b = constants.rmu0 / (2.0e0 * numpy.pi * r)
            # Find out if there is a bore
            if r - x0 < ai:
                h_bore = y0 + bi * numpy.sqrt(1 - ((r - x0) / ai) ** 2)
                h_thick = bo * numpy.sqrt(1 - ((r - x0) / ao) ** 2) - h_bore
            else:
                h_bore = 0.0e0
                h_thick = bo * numpy.sqrt(1 - ((r - x0) / ao) ** 2)

            # Assume B in TF coil = 1/2  B in bore
            # Multiply by 2 for upper and lower halves of coil
            tfcoil_variables.tfind = tfcoil_variables.tfind + b * dr * (
                2.0e0 * h_bore + h_thick
            )
            r = r + dr

    def res_tf_internal_geom(self):
        """
        Author : S. Kahn
        Resisitve TF turn geometry, equivalent to winding_pack subroutines

        """
        sctfcoil_module.r_wp_inner = (
            build_variables.r_tf_inboard_in + tfcoil_variables.thkcas
        )
        sctfcoil_module.r_wp_outer = (
            build_variables.r_tf_inboard_out - tfcoil_variables.casthi
        )

        # Conductor layer radial thickness at centercollumn top [m]
        if physics_variables.itart == 1:
            sctfcoil_module.dr_tf_wp_top = (
                build_variables.r_cp_top
                - tfcoil_variables.casthi
                - tfcoil_variables.thkcas
                - build_variables.r_tf_inboard_in
            )

        # Number of turns
        # Set by user (no turn structure by default, i.e. tfcoil_variables.n_tf_turn = 1 )
        if (
            abs(tfcoil_variables.n_tf_turn)
            < numpy.finfo(float(tfcoil_variables.n_tf_turn)).eps
        ):
            tfcoil_variables.n_tf_turn = 1.0e0

        # Total mid-plane cross-sectional area of winding pack, [m2]
        # including the surrounding ground-wall insulation layer
        sctfcoil_module.awpc = (
            numpy.pi
            * (sctfcoil_module.r_wp_outer**2 - sctfcoil_module.r_wp_inner**2)
            / tfcoil_variables.n_tf
        )

        # Area of the front case, the plasma-facing case of the inner TF coil [m2]
        sctfcoil_module.a_case_front = (
            numpy.pi
            * (
                (sctfcoil_module.r_wp_outer + tfcoil_variables.casthi) ** 2
                - sctfcoil_module.r_wp_outer**2
            )
            / tfcoil_variables.n_tf
        )

        # WP mid-plane cross-section excluding ground insulation per coil [m2]
        sctfcoil_module.awptf = numpy.pi * (
            (sctfcoil_module.r_wp_outer - tfcoil_variables.tinstf) ** 2
            - (sctfcoil_module.r_wp_inner + tfcoil_variables.tinstf) ** 2
        ) / tfcoil_variables.n_tf - 2.0e0 * tfcoil_variables.tinstf * (
            tfcoil_variables.dr_tf_wp - 2.0e0 * tfcoil_variables.tinstf
        )

        # Ground insulation cross-section area per coil [m2]
        sctfcoil_module.a_ground_ins = sctfcoil_module.awpc - sctfcoil_module.awptf

        # Exact mid-plane cross-section area of the conductor per TF coil [m2]
        a_tf_cond = numpy.pi * (
            (
                sctfcoil_module.r_wp_outer
                - tfcoil_variables.tinstf
                - tfcoil_variables.thicndut
            )
            ** 2
            - (
                sctfcoil_module.r_wp_inner
                + tfcoil_variables.tinstf
                + tfcoil_variables.thicndut
            )
            ** 2
        ) / tfcoil_variables.n_tf - (
            tfcoil_variables.dr_tf_wp
            - 2.0e0 * (tfcoil_variables.tinstf + tfcoil_variables.thicndut)
        ) * 2.0e0 * (
            tfcoil_variables.tinstf
            + tfcoil_variables.thicndut * tfcoil_variables.n_tf_turn
        )
        a_tf_cond = a_tf_cond * (1.0e0 - tfcoil_variables.fcoolcp)

        # Inter turn insulation area per coil [m2]
        tfcoil_variables.aiwp = sctfcoil_module.awptf - a_tf_cond / (
            1.0e0 - tfcoil_variables.fcoolcp
        )

        # Total insulation cross-section per coil [m2]
        sctfcoil_module.a_tf_ins = tfcoil_variables.aiwp + sctfcoil_module.a_ground_ins

        # Insulation fraction [-]
        sctfcoil_module.f_tf_ins = (
            tfcoil_variables.n_tf * sctfcoil_module.a_tf_ins / tfcoil_variables.tfareain
        )

        # Total cross-sectional area of the bucking cylindre and the outer support
        # support structure per coil [m2]
        # physics_variables.itart = 1 : Only valid at mid-plane
        tfcoil_variables.acasetf = (
            tfcoil_variables.tfareain / tfcoil_variables.n_tf
        ) - sctfcoil_module.awpc

        # Current per turn
        tfcoil_variables.cpttf = tfcoil_variables.ritfc / (
            tfcoil_variables.n_tf_turn * tfcoil_variables.n_tf
        )

        # Exact current density on TF oubard legs
        tfcoil_variables.cdtfleg = tfcoil_variables.ritfc / (
            (1.0e0 - tfcoil_variables.fcoolcp)
            * (
                tfcoil_variables.tftort
                - 2.0e0
                * (
                    tfcoil_variables.n_tf_turn * tfcoil_variables.thicndut
                    + tfcoil_variables.tinstf
                )
            )
            * (
                build_variables.tfthko
                - 2.0e0 * (tfcoil_variables.thicndut + tfcoil_variables.tinstf)
            )
        )

        # Reporting negative WP areas issues
        if sctfcoil_module.awpc < 0.0e0:
            error_handling.fdiags[0] = sctfcoil_module.awpc
            error_handling.fdiags[0] = tfcoil_variables.dr_tf_wp
            error_handling.report_error(99)

        elif sctfcoil_module.awptf < 0.0e0:
            error_handling.fdiags[0] = sctfcoil_module.awptf
            error_handling.report_error(101)

        ### end break
