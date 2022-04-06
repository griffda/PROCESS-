import numpy

# from process.fortran import rebco_variables
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
            (
                tfcoil_variables.jwdgcrt,
                tfcoil_variables.tmargtf,
            ) = sctfcoil_module.supercon_croco(
                aturn,
                tfcoil_variables.bmaxtfrp,
                tfcoil_variables.cpttf,
                tfcoil_variables.tftmp,
                int(output),
                self.outfile,
            )

            tfcoil_variables.vtfskv = (
                sctfcoil_module.croco_voltage() / 1.0e3
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
