from process.fortran import constants
from process.fortran import availability_module as av
from process.fortran import costs_module as cs
from process.fortran import ife_module as ife


# currently the ife module is only partially wrapped
# to unblock the wrapping of availability
class IFE:
    """Module containing Inertial Fusion Energy device routines
    author: P J Knight, CCFE, Culham Science Centre
    N/A
    This module contains routines for calculating the
    parameters of an Inertial Fusion Energy power plant.
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
        """Routine to output the physics and engineering information
        relevant to inertial fusion energy power plants
        author: P J Knight, CCFE, Culham Science Centre

        This routine outputs the physics and engineering information
        relevant to inertial fusion energy power plants.
        F/MI/PJK/LOGBOOK12, p.66
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :param output: indicate whether output should be written to the output file, or not
        :type output: boolean
        """

        # write to output file
        if output:
            # Costs
            cs.costs(self.outfile, 1)

            # Plant availability
            self.parent_modules_class.availability.avail(self.outfile, 1)

            # IFE physics
            ife.ifephy(self.outfile, 1)

            # Device build
            ife.ifebld(self.outfile, 1)

            # First wall, blanket and shield
            ife.ifefbs(self.outfile, 1)

            # Device structure
            ife.ifestr()

            # Target data
            ife.ifetgt()

            # Primary thermal power
            ife.ifepw1()

            # Vacuum system
            ife.ifevac()
            
            # Buildings
            ife.ifebdg(self.outfile, 1)

            # AC power requirements
            ife.ifeacp(self.outfile, 1)

            # Secondary thermal power
            ife.ifepw2(self.outfile, 1)

            return

        # Device build
        ife.ifebld(constants.nout, 0)

        # IFE physics
        ife.ifephy(constants.nout, 0)

        # Device structure
        ife.ifestr()

        # Target data
        ife.ifetgt()

        # First wall, blanket and shield
        ife.ifefbs(constants.nout, 0)

        # Primary thermal power
        ife.ifepw1()

        # Vacuum system
        ife.ifevac()

        # Buildings
        ife.ifebdg(constants.nout, 0)

        # AC power requirements
        ife.ifeacp(constants.nout, 0)

        # Secondary thermal power
        ife.ifepw2(constants.nout, 0)

        # Plant availability
        self.parent_modules_class.availability.avail(constants.nout, 0)

        # Costs
        cs.costs(constants.nout, 0)
        