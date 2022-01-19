from process.fortran import divertor_module as divm
from process.fortran import build_variables as bv
from process.fortran import divertor_variables as dv
from process.fortran import physics_variables as pv
from process.fortran import constants


class Divertor:
    """Module containing divertor routines
    author: P J Knight, CCFE, Culham Science Centre

    This module contains routines relevant for calculating the
    divertor parameters for a fusion power plant.
    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    """

    def __init__(self) -> None:
        self.outfile = constants.nout  # output file unit

    def run(self, output: bool) -> None:
        """Routine to call the divertor model
        author: J Galambos, ORNL
        author: P J Knight, CCFE, Culham Science Centre
        
        This subroutine calls the divertor routine. This routine scales
        dimensions, powers and field levels which are used as input to
        the Harrison divertor model.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :param output: indicate whether output should be written to the output file, or not
        :type output: boolean
        """
        if pv.itart == 1:
            divm.divtart(
                pv.rmajor,
                pv.rminor,
                pv.triang,
                bv.scrapli,
                bv.vgap,
                pv.pdivt,
                dv.hldiv,
                int(output),
                self.outfile,
            )
            return

        #  Scale geometric quantities

        #  Perpendicular diffusivity in the plasma scrapeoff (m2/s)
        #  Assume ions transport 33% of electron power

        xperp = dv.xpertin * 1.33e0

        #  Reference null to strike distances
        #  Only set up for outer divertor for double-null

        # plsep = min(plsepo,pi*rminor) #  Obscure reason to set a limit...
        plsep = bv.plsepo

        #  Scale plasma quantities

        delne = dv.prn1 * pv.dene * 1.0e-20  #  scrapeoff density by main plasma
        pwr = pv.pdivt  #  power flow to divertor (MW)
        aionso = pv.afuel  #  scrape-off layer ion mass

        if dv.divdum == 0:  #  Divertor Zeff: scaled
            zeffso = 1.0e0 + 0.8e0 * (pv.zeff - 1.0e0)
        else:  #  use input value
            zeffso = dv.zeffdiv

        #  Strike point field values

        bpstk = pv.bp * 0.45e0
        btstk = pv.bt * pv.rmajor / bv.rspo
        rbpbt = bpstk / btstk

        #  Parallel diffusivity in the plasma scrapeoff (m2/s)

        xpara = dv.xparain / zeffso

        #  Null radius

        rnull = pv.rmajor - pv.rminor * pv.triang

        #  Divertor area and radius ratio

        rsrd = (rnull + pv.rmajor + pv.rminor) / (rnull + bv.rspo)
        diva = constants.pi * (rnull + bv.rspo) * plsep
        adas = diva / pv.sarea

        #  Main plasma separatrix area to divertor (and power fraction)
        # +PJK Is the 2 related to 2 divertors (i.e. double-null assumed)?
        frgd = (pv.sareao) / (2.0e0 * pv.sarea)
        # -PJK
        #  Power flow to divertor

        pdiv = pwr * dv.ksic / 2.0e0
        qdiv = pdiv / (pv.sarea * frgd)

        #  Connection length scalings
        #  (2.5 factor comes from normalization to ITER 1990)

        tconl = (
            2.5e0 * pv.rmajor * pv.q * (1.0e0 + 1.0e0 / (pv.q * pv.aspect) ** 2) ** 0.5
        )
        dtheta = plsep / pv.rminor
        dconl = (
            2.5e0
            * bv.rspo
            * pv.q
            * dtheta
            * (1.0e0 + 1.0e0 / (pv.q * pv.aspect) ** 2) ** 0.5
        )
        rconl = dconl / tconl

        #  Minimum strike angle

        minstang = 0.5e0

        #  Call divertor routine

        (
            delta,
            delw,
            dv.dendiv,
            dv.densin,
            gamdt,
            dv.lamp,
            omlarg,
            dv.ppdiv,
            dv.ppdivr,
            dv.ptpdiv,
            dv.tdiv,
            dv.tsep,
        ) = divm.divert(
            adas,
            aionso,
            dv.anginc,
            delne,
            dv.c1div,
            dv.c2div,
            dv.c3div,
            dv.c4div,
            dv.c5div,
            dv.delld,
            dv.fdfs,
            dv.fififi,
            frgd,
            dv.frrp,
            minstang,
            dv.omegan,
            qdiv,
            pdiv,
            rbpbt,
            rconl,
            pv.rmajor,
            rsrd,
            tconl,
            xpara,
            xperp,
        )

        #  Heat load

        dv.hldiv = dv.ppdivr

        #  Ratio of collision length to connection length

        dv.rlclolcn = 1.44e-3 * dv.tsep ** 2 / (delne * 15.0e0 * tconl)

        # output disabled until verdict from MDK in #242
        # if (output):
        # # MDK Issue #242 Switch off divertor output
        #     return

        #     po.oheadr(self.outfile,'Divertor')
        #     po.ocmmnt(self.outfile,'Harrison (ITER) Model')
        #     po.oblnkl(self.outfile)

        # #  Fixed quantities to divertor model

        #     po.ovarre(self.outfile,'Ion mass (amu)','(aionso)',aionso)
        #     po.ovarre(self.outfile,'Fitting coefficient','(c1div)',c1div)
        #     po.ovarre(self.outfile,'Fitting coefficient','(c2div)',c2div)
        #     po.ovarre(self.outfile,'Fitting coefficient','(c3div)',c3div)
        #     po.ovarre(self.outfile,'Fitting coefficient','(c4div)',c4div)
        #     po.ovarre(self.outfile,'Fitting coefficient','(c5div)',c5div)
        #     po.ovarre(self.outfile,'Fitting coefficient','(c6div)',c6div)
        #     po.ovarin(self.outfile,'Divertor Zeff model','(divdum)',divdum)
        #     po.ovarre(self.outfile,'Zeff in scrape-off region','(zeffso)',zeffso)
        #     po.ovarre(self.outfile,'Coeff of energy distrib. along conn length','(delld)',delld)
        #     po.ovarre(self.outfile,'Separatrix plasma density (10**20 m-3)','(delne)',delne)
        #     po.ovarre(self.outfile,'Radial gradient ratio','(fdfs)',fdfs)
        #     po.ovarre(self.outfile,'Sheath potential factor','(fgamp)',fgamp)
        #     po.ovarre(self.outfile,'Parameter for sheath coefficient','(fififi)',fififi)
        #     po.ovarre(self.outfile,'Fraction of radiated power to plate','(frrp)',frrp)
        #     po.ovarre(self.outfile,'Pressure ratio - (nT)_p/(nT)_s','(omegan)',omegan)
        #     po.ovarre(self.outfile,'ne-edge / ne-average','(prn1)',prn1)
        #     po.ovarre(self.outfile,'Parallel heat transport coefficient','(xpara)',xpara)
        #     po.ovarre(self.outfile,'Radial transport coefficient','(xperp)',xperp)

        # #  Input quantities scaled in divertor caller (dependent on geometry,
        # #  plasma parameters) - can be different for inner and outer plates

        #     po.osubhd(self.outfile,'Scaled Input Quantities :')

        #     po.ovarre(self.outfile,'Fraction of areas','(adas)',adas)
        #     po.ovarre(self.outfile,'Angle of incidence (rad)','(anginc)',anginc)
        #     po.ovarre(self.outfile,'Area of divertor / area of separatrix','(frgd)',frgd)
        #     po.ovarre(self.outfile,'Power fraction to outer divertor','(ksic)',ksic)
        #     po.ovarre(self.outfile,'Power to divertor (MW)','(pdiv)',pdiv)
        #     po.ovarre(self.outfile,'Null to strike length (m)','(plsep)',plsep)
        #     po.ovarre(self.outfile,'B_p / B_t strike point','(rbpbtc)',rbpbt)
        #     po.ovarre(self.outfile,'Connection length ratio','(rconl)',rconl)
        #     po.ovarre(self.outfile,'Radius ratio R_s/R_d','(rsrd)',rsrd)
        #     po.ovarre(self.outfile,'Strike radius (m)','(rspo)',rspo)
        #     po.ovarre(self.outfile,'Connection length (m)','(tconl)',tconl)

        # #  Quantities calculated by the Harrison model

        #     po.osubhd(self.outfile,'Divertor Model Output :')
        #     po.ovarre(self.outfile,'Iteration relative error','(delta)',delta)
        #     po.ovarre(self.outfile,'Private flux power factor','(omlarg)',omlarg)
        #     po.ovarre(self.outfile,'Separatrix temperature (eV)','(tsep)',tsep)
        #     po.ovarre(self.outfile,'Divertor temperature (eV)','(tdiv)',tdiv)
        #     po.ovarre(self.outfile,'Divertor plasma density (10**20 m-3)','(dendiv)',dendiv)
        #     po.ovarre(self.outfile,'Peak heat load (MW/m2)','(hldiv)',hldiv)
        #     po.ovarre(self.outfile,'Divertor peak temperature (eV)','(ptpdiv)',ptpdiv)
        #     po.ovarre(self.outfile,'D/T plate flux (10**20 m-3)','(gamdt)',gamdt)
        #     po.ovarre(self.outfile,'Scrape-off thickness (m)','(delw)',delw)
        #     po.ovarre(self.outfile,'Collision length / connection length','(rlclolcn)',rlclolcn)
