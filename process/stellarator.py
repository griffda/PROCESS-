from process.fortran import constants
from process.fortran import costs_module as cs
from process.fortran import physics_module as ph
from process.fortran import buildings_module as bm
from process.fortran import power_module as pw
from process.fortran import stellarator_module as st

# currently the ife module is only partially wrapped
# to unblock the wrapping of availability
class Stellarator:
    """Module containing stellarator routines
    author: P J Knight, CCFE, Culham Science Centre
    N/A
    This module contains routines for calculating the
    parameters of the first wall, blanket and shield components
    of a fusion power plant.

    AEA FUS 251: A User's Guide to the PROCESS Systems Code

    NOTE: currently the IFE module is only partially wrapped to unblock the wrapping of availability
    """

    def __init__(self, parent_modules_class) -> None:
        """Initialises the IFE module's variables

        :param parent_modules_class: a pointer to the parent Models class, hence allowing for access to other models in the same instantiated parent
        :type parent_modules_class: process.main.Models
        """

        self.outfile: int = constants.nout
        self.parent_modules_class = parent_modules_class

    def run(self, output: bool):
        """Routine to call the physics and engineering modules
        relevant to stellarators
        author: P J Knight, CCFE, Culham Science Centre
        author: F Warmer, IPP Greifswald

        This routine is the caller for the stellarator models.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :param output: indicate whether output should be written to the output file, or not
        :type output: boolean
        """

        if output:
            # original routine contained the following debug
            # print statements that have been removed due to
            # f2py derived-type limitations.
            # print *,"Used stellarator configuration: ", config%name
            # print *,"Deviation from reference point"
            # print *,"aspect ratio",aspect/config%aspect_ref
            # print *,"major radius",rmajor/config%rmajor_ref
            # print *,"n_tf (should be 1)", n_tf/(config%coilspermodule*config%symmetry)

            cs.costs(self.outfile, 1)
            # TODO: can it be assumed that in this case, iavail <= 1
            # and I can just call run with output=True?
            self.parent_modules_class.availability.iprint = 1
            self.parent_modules_class.availability.avail()
            ph.outplas(self.outfile)
            st.stigma(self.outfile)
            st.stheat(self.outfile, 1)
            st.stdiv(self.outfile, 1)
            st.stbild(self.outfile, 1)
            st.stcoil(self.outfile, 1)
            st.ststrc(self.outfile, 1)
            st.stfwbs(self.outfile, 1)

            pw.tfpwr(self.outfile, 1)
            self.parent_modules_class.vacuum.run(output=True)
            bm.bldgcall(self.outfile, 1)
            pw.acpow(self.outfile, 1)
            pw.power2(self.outfile, 1)

            return

        st.stnewconfig()
        st.stgeom()
        st.stphys()
        st.stcoil(self.outfile, 0)
        st.stbild(self.outfile, 0)
        st.ststrc(self.outfile, 0)
        st.stfwbs(self.outfile, 0)
        st.stdiv(self.outfile, 0)

        pw.tfpwr(self.outfile, 0)
        pw.power1()
        self.parent_modules_class.vacuum.run(output=False)
        bm.bldgcall(self.outfile, 0)
        pw.acpow(self.outfile, 0)
        pw.power2(self.outfile, 0)
        # TODO: can it be assumed that in this case, iavail > 1
        # and I can just call run with output=False?
        self.parent_modules_class.availability.iprint = 0
        self.parent_modules_class.availability.avail()
        cs.costs(self.outfile, 0)

        st.first_call = False
