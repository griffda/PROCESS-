import numpy
import logging

from process.fortran import rebco_variables
from process.fortran import fwbs_variables
from process.fortran import numerics
from process.fortran import maths_library
from process.fortran import global_variables
from process.fortran import superconductors
from process.fortran import tfcoil_variables
from process.fortran import physics_variables
from process.fortran import build_variables
from process.fortran import constants
from process.fortran import sctfcoil_module
from process.fortran import process_output as po

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

            # (
            #     tfcoil_variables.jwdgcrt,
            #     tfcoil_variables.tmargtf,
            # ) = sctfcoil_module.supercon_croco(
            #     aturn,
            #     tfcoil_variables.bmaxtfrp,
            #     tfcoil_variables.cpttf,
            #     tfcoil_variables.tftmp,
            #     int(0),
            #     self.outfile,
            # )

            tfcoil_variables.vtfskv = (
                self.croco_voltage() / 1.0e3
            )  #  TFC Quench voltage in kV

        else:
            (
                tfcoil_variables.jwdgcrt,
                vdump,
                tfcoil_variables.tmargtf,
            ) = sctfcoil_module.supercon(
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
                int(output),
                self.outfile,
            )

            tfcoil_variables.vtfskv = vdump / 1.0e3  #  TFC Quench voltage in kV

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
        # Set new rebco_variables.croco_od - allowing for scaling of rebco_variables.croco_od
        rebco_variables.croco_od = (
            tfcoil_variables.t_conductor / 3.0e0
            - tfcoil_variables.thwcndut * (2.0e0 / 3.0e0)
        )
        sctfcoil_module.conductor_acs = (
            9.0e0 / 4.0e0 * numpy.pi * rebco_variables.croco_od ** 2
        )
        tfcoil_variables.acstf = sctfcoil_module.conductor_acs
        sctfcoil_module.conductor_area = (
            tfcoil_variables.t_conductor ** 2
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
        #  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
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

        sctfcoil_module.tf_global_geometry()

        # Calculation of the TF current from bt
        sctfcoil_module.tf_current()

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
            sctfcoil_module.res_tf_internal_geom()

        # ---

        # Coil vertical geometry
        sctfcoil_module.coilshap()

        # TF resistive heating (res TF only)
        if tfcoil_variables.i_tf_sup != 1:
            sctfcoil_module.tf_res_heating()

        # Vertical force
        sctfcoil_module.tf_field_and_force()

        # TF coil inductance
        # ---
        if physics_variables.itart == 0 and tfcoil_variables.i_tf_shape == 1:
            sctfcoil_module.tfcind(build_variables.tfcth)
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
            0.5e0 * tfcoil_variables.tfind * tfcoil_variables.ritfc ** 2
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
