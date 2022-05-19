from process.fortran import constants
from process.fortran import physics_module as ph
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

    def __init__(self, availability, vacuum, buildings, costs) -> None:
        """Initialises the IFE module's variables

        :param availability: a pointer to the availability model, allowing use of availability's variables/methods
        :type availability: process.availability.Availability
        :param buildings: a pointer to the buildings model, allowing use of buildings's variables/methods
        :type buildings: process.buildings.Buildings
        :param Vacuum: a pointer to the vacuum model, allowing use of vacuum's variables/methods
        :type Vacuum: process.vacuum.Vacuum
        :param Costs: a pointer to the costs model, allowing use of costs' variables/methods
        :type Costs: process.costs.Costs
        """

        self.outfile: int = constants.nout
        self.availability = availability
        self.buildings = buildings
        self.vacuum = vacuum
        self.costs = costs

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

            self.costs.costs(output=True)
            # TODO: should availability.run be called
            # rather than availability.avail?
            self.availability.avail(output=True)
            ph.outplas(self.outfile)
            st.stigma(self.outfile)
            st.stheat(self.outfile, 1)
            st.stdiv(self.outfile, 1)
            st.stbild(self.outfile, 1)
            st.stcoil(self.outfile, 1)
            st.ststrc(self.outfile, 1)
            st.stfwbs(self.outfile, 1)

            pw.tfpwr(self.outfile, 1)
            self.buildings.run(output=True)
            self.vacuum.run(output=True)
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
        self.buildings.run(output=False)
        self.vacuum.run(output=False)
        pw.acpow(self.outfile, 0)
        pw.power2(self.outfile, 0)
        # TODO: should availability.run be called
        # rather than availability.avail?
        self.availability.avail(output=False)
        self.costs.costs(output=False)

        st.first_call = False
