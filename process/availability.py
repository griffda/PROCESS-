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

DAY = 60*60*24
"""Seconds in a day [s]"""

YEAR = DAY*365.25
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

    def run(self, output:bool = False):
        """Run appropriate availability model
        
        Availability switch values
        No.  |  model
        ---- | ------
        0    |  Input value for cfactr
        1    |  Ward and Taylor model (1999)
        2    |  Morris model (2015)
        """
        self.iprint = 1 if output else 0

        if cv.iavail > 1:
            ft.availability_module.avail_2(ft.constants.nout, self.iprint)  # Morris model (2015)
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
        if (ifev.ife != 1) :
        # First wall / blanket lifetime (years)

            # TODO MDK Do this calculation whatever the value of blktmodel (whatever that is)
            # For some reason fwlife is not always calculated, so ignore it if it is still zero.
            if (fwbsv.fwlife < 0.0001e0) :
                fwbsv.bktlife = min(cv.abktflnc/pv.wallmw, cv.tlife)
            else:
                fwbsv.bktlife = min(fwbsv.wlife, cv.abktflnc/pv.wallmw, cv.tlife)

            # TODO Issue #834
            # Add a test for hldiv=0
            if (dv.hldiv < 1.0e-10):
                dv.hldiv=1.0e-10

            # Divertor lifetime (years)
            cv.divlife = max(0.0, min(cv.adivflnc/dv.hldiv, cv.tlife))

            # Centrepost lifetime (years) (ST machines only)
            if ( pv.itart ==  1) :
            # SC magnets CP lifetime
            # Rem : only the TF maximum fluence is considered for now
                if ( tfv.i_tf_sup == 1 ) :
                    cv.cplife = min( ctv.nflutfmax / ( fwbsv.neut_flux_cp * YEAR ), cv.tlife )
                
            # Aluminium/Copper magnets CP lifetime
            # For now, we keep the original def, developped for GLIDCOP magnets ...
                else :
                    cv.cplife = min( cv.cpstflnc / pv.wallmw, cv.tlife )


        # Plant Availability (iavail=0,1)

        # if iavail = 0 use input value for cfactr

        # Taylor and Ward 1999 model (iavail=1)
        if (cv.iavail == 1) :
        # Which component has the shorter life?
            if (cv.ivlife < fwbsv.bktlife) :
                ld = cv.divlife
                lb = fwbsv.bktlife
                td = cv.tdivrepl
            else:
                ld = fwbsv.bktlife
                lb = cv.divlife
                td = cv.tbktrepl

            # Number of outages between each combined outage
            n = math.ceil(lb/ld) - 1

            # Planned unavailability
            uplanned = (n*td + cv.tcomrepl) / ( (n+1)*ld + (n*td + cv.tcomrepl) )

            # Unplanned unavailability
            # Rather than simply summing the individual terms, the following protects
            # against the total availability becoming zero or negative

            uutot = cv.uubop                           # balance of plant
            uutot = uutot + (1.0e0 - uutot)*cv.uucd    # current drive
            uutot = uutot + (1.0e0 - uutot)*cv.uudiv   # divertor
            uutot = uutot + (1.0e0 - uutot)*cv.uufuel  # fuel system
            uutot = uutot + (1.0e0 - uutot)*cv.uufw    # first wall + blanket
            uutot = uutot + (1.0e0 - uutot)*cv.uumag   # magnets
            uutot = uutot + (1.0e0 - uutot)*cv.uuves   # vacuum vessel

            # Total availability
            cv.cfactr = 1.0e0 - (uplanned + uutot - (uplanned*uutot))

        # Capacity factor
        # Using the amount of time burning for a given pulse cycle
        cv.cpfact = cv.cfactr * (tv.tburn / tv.tcycle)

        # Modify lifetimes to take account of the availability
        if (ifev.ife != 1) :
            # First wall / blanket
            if (fwbsv.bktlife < cv.tlife) :
                fwbsv.bktlife = min( fwbsv.bktlife/cv.cfactr, cv.tlife )

            # Divertor
            if (cv.divlife < cv.tlife) :
                cv.divlife = min( cv.divlife/cv.cfactr, cv.tlife )

            # Centrepost
            if ( pv.itart == 1 and cv.cplife < cv.tlife ) :
                cv.cplife = min( cv.cplife/cv.cfactr, cv.tlife )


    # Current drive system lifetime (assumed equal to first wall and blanket lifetime)
        cv.cdrlife = fwbsv.bktlife

    # Output section
        if (self.iprint != 1) :
            return

        po.oheadr(self.outfile,'Plant Availability')
        if (fwbsv.blktmodel == 0) :
            po.ovarre(self.outfile,'Allowable blanket neutron fluence (MW-yr/m2)', '(abktflnc)', cv.abktflnc)
        
        po.ovarre(self.outfile,'Allowable divertor heat fluence (MW-yr/m2)', '(adivflnc)', cv.adivflnc)
        po.ovarre(self.outfile,'First wall / blanket lifetime (years)', '(bktlife)', fwbsv.bktlife, 'OP ')
        po.ovarre(self.outfile,'Divertor lifetime (years)', '(divlife)', cv.divlife, 'OP ')

        if (pv.itart == 1) :
            po.ovarre(self.outfile,'Centrepost lifetime (years)', '(cplife)', cv.cplife, 'OP ')
        

        po.ovarre(self.outfile,'Heating/CD system lifetime (years)', '(cdrlife)', cv.cdrlife, 'OP ')
        po.ovarre(self.outfile,'Total plant lifetime (years)', '(tlife)', cv.tlife)

        if (cv.iavail == 1) :
            if (cv.divlife < fwbsv.bktlife) :
                po.ovarre(self.outfile,'Time needed to replace divertor (years)', '(tdivrepl)', cv.tdivrepl)
            else:
                po.ovarre(self.outfile,'Time needed to replace blanket (years)', '(tbktrepl)', cv.tbktrepl)

            po.ovarre(self.outfile,'Time needed to replace blkt + div (years)', '(tcomrepl)', cv.tcomrepl)
            po.ovarre(self.outfile,'Planned unavailability fraction', '(uplanned)', uplanned, 'OP ')
            po.ovarre(self.outfile,'Unplanned unavailability fraction', '(uutot)', uutot, 'OP ')

        if (cv.iavail == 0) :
            po.ovarre(self.outfile,'Total plant availability fraction', '(cfactr)', cv.cfactr)
        else:
            po.ovarre(self.outfile,'Total plant availability fraction', '(cfactr)', cv.cfactr, 'OP ')