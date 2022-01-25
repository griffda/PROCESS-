from process.fortran import structure_module as st
from process.fortran import structure_variables as stv
from process.fortran import pfcoil_variables as pfv
from process.fortran import physics_variables as pv
from process.fortran import tfcoil_variables as tfv
from process.fortran import build_variables as bv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import divertor_variables as divv
from process.fortran import constants


class Structure:
    """Class containing support structure calculations
    author: P J Knight, CCFE, Culham Science Centre

    This class contains routines for calculating the
    parameters of the support structure for a
    fusion power plant.
    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    """

    def __init__(self) -> None:
        self.outfile = constants.nout  # output file unit

    def run(self, output: bool = False) -> None:
        """!! Structure calculation caller
        author: P J Knight, CCFE, Culham Science Centre

        This subroutine calls the support structure mass calculations.
        """

        # Total weight of the PF coil conductor and its structure
        total_weight_pf = pfv.whtpf + pfv.whtpfs

        (
            stv.fncmass,
            stv.aintmass,
            stv.clgsmass,
            stv.coldmass,
            stv.gsmass,
        ) = st.structure(
            pv.plascur,
            pv.rmajor,
            pv.rminor,
            pv.kappa,
            pv.bt,
            tfv.i_tf_sup,
            pfv.ipfres,
            bv.dr_tf_inner_bore + bv.tfthko + bv.tfcth,
            bv.hmax,
            fwbsv.whtshld,
            divv.divmas,
            total_weight_pf,
            tfv.whttf,
            fwbsv.fwmass,
            fwbsv.whtblkt,
            fwbsv.coolmass,
            fwbsv.dewmkg,
            self.outfile,
            int(output),
        )
