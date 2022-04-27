from process.fortran import constants
from process.fortran import costs_module as cs
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

    def __init__(self, availability, vacuum, buildings) -> None:
        """Initialises the IFE module's variables

        :param availability: a pointer to the availability model, allowing use of availability's variables/methods
        :type availability: process.availability.Availability
        :param buildings: a pointer to the buildings model, allowing use of buildings's variables/methods
        :type buildings: process.buildings.Buildings
        """

        self.outfile: int = constants.nout
        self.availability = availability
        self.buildings = buildings
        self.vacuum = vacuum

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
            cs.costs(self.outfile, 1)
            # TODO: should availability.run be called
            # rather than availability.avail?
            self.availability.avail(output=True)
            ph.outplas(self.outfile)
            st.stigma(self.outfile)
            st.stheat(self.outfile, 1)
            st.stphys(self.outfile, 1)
            st.stopt(self.outfile, 1)
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
        st.stphys(self.outfile, 0)
        st.stopt(self.outfile, 0)
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
        cs.costs(self.outfile, 0)

        st.first_call = False
