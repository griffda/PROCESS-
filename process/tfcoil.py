import numpy as np
import copy

from process import fortran as ft
from process.fortran import tfcoil_module as tf
from process.fortran import tfcoil_variables as tfv
from process.fortran import sctfcoil_module as sctf
from process.fortran import build_module as bm
from process.fortran import build_variables as bv
from process.fortran import constants
from process.fortran import fwbs_variables as fwbsv
from process.fortran import error_handling as eh
from process.fortran import process_output as po

class TFcoil:
    """Calculates the parameters of a resistive TF coil system for a fusion power plant"""

    def __init__(self):
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        self.iprint = 0  # switch for writing to output file (1=yes)

    def run(self):
        """Run main tfcoil subroutine without outputting."""
        self.iprint = 0
        self.tfcoil()

    def output(self):
        """Run main tfcoil subroutine and write output."""
        self.iprint = 1
        self.tfcoil()
    
    def tfcoil(self):
        """TF coil module
        author: P J Knight, CCFE, Culham Science Centre
        outfile : input integer : output file unit
        iprint : input integer : switch for writing to output file (1=yes)
        This subroutine calculates various parameters for the TF coil set.
        If the TF coils are superconducting the calculations are performed
        in routine <A HREF="sctfcoil.html">sctfcoil</A> instead.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        
        # TF coil calculations
        sctf.sctfcoil(self.outfile, self.iprint)

        # Port size calculation
        bm.portsz()
    
    def cntrpst(self):
        """
        Evaluates the properties of a TART centrepost
        author: P J Knight, CCFE, Culham Science Centre
        outfile : input integer : output file unit
        iprint : input integer : switch for writing to output file (1=yes)
        This subroutine evaluates the parameters of the centrepost for a
        tight aspect ratio tokamak. The centrepost is assumed to be tapered,
        i.e. narrowest on the midplane (z=0).
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """

        # Temperature margin used in calculations (K)
        tmarg = 10.0e0
        
        # Number of integral step used for the coolant temperature rise
        n_tcool_it = 20
        
        # Coolant channels:
        acool = tfv.a_cp_cool * tfv.n_tf        # Cooling cross-sectional area
        dcool = 2.0e0 * tfv.rcool           # Diameter
        lcool = 2.0e0 * (bv.hmax + bv.tfthko) # Length
        tfv.ncool = acool/( constants.pi * tfv.rcool**2)  # Number

        # Average conductor cross-sectional area to cool (with cooling area)
        acpav = 0.5e0 * tfv.vol_cond_cp/(bv.hmax + bv.tfthko) + acool
        ro = ( acpav/(constants.pi*tfv.ncool) )**0.5

        # Inner legs total heating power (to be removed by coolant)
        ptot = tfv.prescp + fwbsv.pnuc_cp_tf * 1.0e6

        # Temperature calculations
        # -------------------------
        # Temperature rise in coolant (inlet to outlet)
        # **********************************************
        # Water coollant
        # --------------
        if tfv.i_tf_sup ==  0:
            
            # Water coolant physical properties
            coolant_density = constants.denh2o
            coolant_cp      = constants.cph2o
            coolant_visco   = constants.muh2o
            coolant_th_cond = constants.kh2o

            # Mass flow rate [kg/s]
            cool_mass_flow = acool * coolant_density * tfv.vcool

            # Water temperature rise
            tfv.dtiocool = ptot / (cool_mass_flow*coolant_cp)

            # Constant coolant velocity
            vcool_max = tfv.vcool
            # --------------

        
        # Helium coolant
        # --------------
        elif ( tfv.i_tf_sup ==  2 ):

            # Inlet coolant density [kg/m3]
            coolant_density = tf.he_density( tfv.tcoolin )

            # Mass flow rate [kg/s]
            cool_mass_flow = acool * coolant_density * tfv.vcool
        
            # Infinitesimal power deposition used in the integral
            dptot = ptot / n_tcool_it

            tcool_calc = copy.copy(tfv.tcoolin) # K
            for i in range(n_tcool_it):

                # Thermal capacity Cp
                coolant_cp = tf.he_cp(tcool_calc)

                # Temperature infinitesimal increase
                tcool_calc += dptot / ( cool_mass_flow * coolant_cp )

            
            # Outlet coolant density (minimal coolant density value)
            coolant_density = tf.he_density(tcool_calc)
            
            # Maxium coolant velocity
            vcool_max = cool_mass_flow / ( acool * coolant_density )

            # Getting the global in-outlet temperature increase 
            tfv.dtiocool = tcool_calc - tfv.tcoolin
        # --------------

        # Average coolant temperature
        tcool_av = tfv.tcoolin + 0.5e0 * tfv.dtiocool
        # **********************************************


        # Film temperature rise
        # *********************
        # Rem : The helium cooling properties are calculated using the outlet ones
        # this is not an exact approximation for average temperature rise

        # Helium viscosity
        if ( tfv.i_tf_sup == 2 ): coolant_visco = tf.he_visco(tcool_av)

        # Reynolds number
        reyn = coolant_density * tfv.vcool * dcool / coolant_visco
    
        # Helium thermal conductivity [W/(m.K)]
        if ( tfv.i_tf_sup == 2 ): coolant_th_cond = tf.he_th_cond( tcool_av )
    
        # Prandlt number  
        prndtl = coolant_cp * coolant_visco / coolant_th_cond

        # Film temperature difference calculations    
        # Originally prandtl was prndtl**0.3e0 but this is incorrect as from
        # Dittus-Boelter correlation where the fluid is being heated it should be as below    
        nuselt = 0.023e0 * reyn**0.8e0 * prndtl**0.4e0
        h = nuselt * coolant_th_cond / dcool
        dtfilmav = ptot / (h * 2.0e0*constants.pi*tfv.rcool * tfv.ncool * lcool)

        # Average film temperature (in contact with te conductor)
        tcool_film = tcool_av + dtfilmav
        # *********************


        # Temperature rise in conductor
        # ------------------------------
        # Conductor thermal conductivity
        # ******
        # Copper conductor
        if ( tfv.i_tf_sup ==  0 ):
            conductor_th_cond = constants.k_copper
        
        # Aluminium 
        elif ( tfv.i_tf_sup == 2 ):
            conductor_th_cond = tf.al_th_cond( tcool_film )
        # ******

        # Average temperature rise : To be changed with Garry Voss' better documented formula ? 
        dtcncpav = (ptot/tfv.vol_cond_cp)/(2.0e0*conductor_th_cond*(ro**2 - tfv.rcool**2) ) * ( ro**2*tfv.rcool**2 - 0.25e0*tfv.rcool**4 - 0.75e0*ro**4 + ro**4 * np.log(ro/tfv.rcool) )

        # Peak temperature rise : To be changed with Garry Voss' better documented formula ?
        dtconcpmx = (ptot/tfv.vol_cond_cp)/(2.0e0*conductor_th_cond) * ( (tfv.rcool**2 - ro**2)/2.0e0 + ro**2 * np.log(ro/tfv.rcool) )


        # If the average conductor temperature difference is negative, set it to 0 
        if ( dtcncpav < 0.0e0 ): 
            eh.report_error(249)
            dtcncpav = 0.0e0


        # If the average conductor temperature difference is negative, set it to 0  
        if ( dtconcpmx < 0.0e0 ): 
            eh.report_error(250)
            dtconcpmx = 0.0e0
      
        
        # Average conductor temperature
        tfv.tcpav2 = tfv.tcoolin + dtcncpav + dtfilmav + 0.5e0*tfv.dtiocool

        # Peak wall temperature
        tfv.tcpmax  = tfv.tcoolin + tfv.dtiocool + dtfilmav + dtconcpmx
        tcoolmx = tfv.tcoolin + tfv.dtiocool + dtfilmav
        # -------------------------

        
        # Thermal hydraulics: friction factor from Z. Olujic, Chemical
        # Engineering, Dec. 1981, p. 91
        roughrat = 4.6e-5 / dcool
        fricfac  = 1.0e0/ (-2.0e0 * np.log10(roughrat/3.7e0 - 5.02e0/reyn  * np.log10( roughrat/3.7e0 + 14.5e0/reyn) ) )**2

        # Pumping efficiency
        if      ( tfv.i_tf_sup == 0 ): # Water cooled
            tfv.etapump = 0.8e0
        elif ( tfv.i_tf_sup == 2 ): # Cryogenic helium
            tfv.etapump = 0.6e0

        # Pressure drop calculation
        dpres = fricfac * (lcool/dcool) * coolant_density * 0.5e0*tfv.vcool**2
        tfv.ppump = dpres * acool * tfv.vcool / tfv.etapump

        # Critical pressure in saturation pressure calculations (Pa)
        pcrt = 2.24e7

        # Saturation pressure
        # Ref : Keenan, Keyes, Hill, Moore, steam tables, Wiley & Sons, 1969
        # Rem 1 : ONLY VALID FOR WATER !
        # Rem 2 : Not used anywhere else in the code ...
        tclmx = tcoolmx + tmarg
        tclmxs = min(tclmx, 374.0e0)
        fc = 0.65e0 - 0.01e0 * tclmxs
        sum = -741.9242e0 - 29.721e0*fc - 11.55286e0*fc**2 - 0.8685635e0*fc**3 + 0.1094098e0*fc**4 + 0.439993e0*fc**5 + 0.2520658e0*fc**6 + 0.0518684e0*fc**7
        psat = pcrt * np.exp(0.01e0/(tclmxs + 273.0e0) * (374.0e0 - tclmxs) * sum )
        presin = psat + dpres



        # Output section    
        if self.iprint == 1:
            po.oheadr(self.outfile,'Centrepost Coolant Parameters')
            po.ovarre(self.outfile,'Centrepost coolant fraction','(fcoolcp)',tfv.fcoolcp)
            po.ovarre(self.outfile,'Average coolant channel diameter (m)','(dcool)',dcool)
            po.ovarre(self.outfile,'Coolant channel length (m)','(lcool)',lcool)
            po.ovarre(self.outfile,'Inlet coolant flow speed (m/s)','(tfv.vcool)',tfv.vcool)
            po.ovarre(self.outfile,'Outlet coolant flow speed (m/s)','(vcool_max)',vcool_max)
            po.ovarre(self.outfile,'Coolant mass flow rate (kg/s)','(cool_mass_flow)',cool_mass_flow)
            po.ovarre(self.outfile,'Number of coolant tubes','(ncool)',tfv.ncool)
            po.ovarre(self.outfile,'Reynolds number','(reyn)',reyn)
            po.ovarre(self.outfile,'Prandtl number','(prndtl)',prndtl)
            po.ovarre(self.outfile,'Nusselt number','(nuselt)',nuselt)

            po.osubhd(self.outfile,'Resistive Heating :')
            po.ovarre(self.outfile,'Average conductor resistivity (ohm.m)','(rhocp)',tfv.rhocp)
            po.ovarre(self.outfile,'Resistive heating (MW)','(prescp/1.0e6)',tfv.prescp/1.0e6)
            po.ovarre(self.outfile,'Nuclear heating (MW)','(pnuc_cp_tf)',fwbsv.pnuc_cp_tf)
            po.ovarre(self.outfile,'Total heating (MW)','(ptot/1.0e6)',ptot/1.0e6)

            po.osubhd(self.outfile,'Temperatures :')
            po.ovarre(self.outfile,'Input coolant temperature (K)','(tfv.tcoolin)',tfv.tcoolin)
            po.ovarre(self.outfile,'Input-output coolant temperature rise (K)','(dtiocool)',tfv.dtiocool)
            po.ovarre(self.outfile,'Film temperature rise (K)','(dtfilmav)',dtfilmav)
            po.ovarre(self.outfile,'Average temp gradient in conductor (K/m)','(dtcncpav)',dtcncpav)
            po.ovarre(self.outfile,'Average centrepost temperature (K)','(tcpav2)',tfv.tcpav2)
            po.ovarre(self.outfile,'Peak centrepost temperature (K)','(tcpmax)',tfv.tcpmax)

            po.osubhd(self.outfile,'Pump Power :')
            po.ovarre(self.outfile,'Coolant pressure drop (Pa)','(dpres)',dpres)
            if ( tfv.i_tf_sup == 0 ): # Saturation pressure calculated with Water data ...
                po.ovarre(self.outfile,'Coolant inlet pressure (Pa)','(presin)',presin)
  
            po.ovarre(self.outfile,'Pump power (W)','(ppump)',tfv.ppump)
