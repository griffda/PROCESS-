import math

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
        ) = self.divert(
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

        # output deleted as per discussion in #242.
        # tnunn: 19/01/2022

    def divert(
        self,
        adas,
        aion,
        anginc,
        delne,
        c1div,
        c2div,
        c3div,
        c4div,
        c5div,
        delld,
        fdfs,
        fififi,
        frgd,
        frrp,
        minstang,
        omegan,
        qdiv,
        pdiv,
        rbpbt,
        rconl,
        rmaj,
        rsrd,
        tconl,
        xpara,
        xperp,
    ):
        """Harrison-Kukushkin analytic ITER divertor model
        author: J Galambos, ORNL
        author: P J Knight, CCFE, Culham Science Centre

        This subroutine performs the iteration described in M. Harrison's
        and Kukushkin's analytic ITER divertor model.
        Report ITER-IL-PH-13-9-e12
        AEA FUS 251: A User's Guide to the PROCESS Systems Code

        :param adas: divertor flux area / main plasma area (long separatrix)
        :type adas: float

        :param aion: ion mass (assumes fuel only) (AMU)
        :type aion: float


        :param anginc: pol. angle of incidence of field line on plate (rad)
        :type anginc: float

        :param c1div: fitting coefficient for plate temperature
        :type c1div: float

        :param c2div: fitting coefficient for plate temperature
        :type c2div: float

        :param c3div: fitting coefficient for heat load
        :type c3div: float

        :param c4div: fitting coefficient for heat load
        :type c4div: float

        :param c5div: fitting coefficient for 'omlarg'
        :type c5div: float

        :param delld: coeff. for power distribution flow into scrapeoff
        :type delld: float

        :param delne: scrapeoff density by main plasma (10**20 m-3)
        :type delne: float

        :param fdfs: gradient ratio (private flux side/other side) in 'omlarg'
        :type fdfs: float

        :param fififi: coeff. used in sheath energy transfer factor calc.
        :type fififi: float

        :param frgd: separatrix area to divertor / total separatrix area
        :type frgd: float

        :param frrp: fraction of radiated power to plate
        :type frrp: float

        :param minstang: minimum strike angle (total) for heat flux calc.
        :type minstang: float

        :param omegan: pressure ratio of (plate / main plasma)
        :type omegan: float

        :param qdiv: heat flux across separatrix to divertor (MW/m2)
        :type qdiv: float

        :param pdiv: power flow to plate (MW)
        :type pdiv: float

        :param rbpbt: ratio of toroidal / poloidal field at strike point
        :type rbpbt: float

        :param rconl: connection length ratio (divertor region/main plasma region)
        :type rconl: float

        :param rmaj: major radius (m)
        :type rmaj: float

        :param rsrd: ratio of separatrix radius / divertor radius
        :type rsrd: float

        :param tconl: connection length along field line by main plasma (m)
        :type tconl: float

        :param xpara: parallel diffusivity in the plasma scrapeoff (m2/s)
        :type xpara: float

        :param xperp: perpend. diffusivity in the plasma scrapeoff (m2/s)
        :type xperp: float

        :returns:
            - delta (`float`) iteration relative error
            - delw (`float`) energy flow thickness in scrape-off (m)
            - dendiv (`float`) plasma density at divertor (10**20 m-3)
            - densin (`float`) peak plasma density at divertor (on separatrix) (10**20 m-3)
            - gamdt (`float`) plasma flow to plate (10**20/s)
            - lamp (`float`) power flow width (m)
            - omlarg (`float`) factor accounting for power flow to private flux region
            - ppdiv (`float`) divertor heat load without radiation (MW/m2)
            - ppdivr (`float`) divertor heat load with radiation (MW/m2)
            - ptpdiv (`float`) peak plasma temperature at the divertor plate (eV)
            - tdiv (`float`) temperature at the plate (eV)
            - tsep (`float`) temperature at the separatrix (eV)
        """

        c27 = 0.2857143e0
        ei = 13.6e0
        epsilon = 0.001e0
        relerr = 1.0e-9

        fprime = c5div * fdfs
        facdenom = fprime * rsrd * (adas / frgd) ** 2 / rconl
        facdenom = max(facdenom, 0.04e0)
        omlarg = 1.0e0 / (rsrd * math.exp(-facdenom))
        omlarg = min(omlarg, 2.0e0)
        coefl = 1.0e0 / delld + rconl / omlarg  #  little 'l' in Harrison model

        #  Start iteration on 2 simultaneous equations (Newton's method)

        tdivges = 150.0e0
        tptsges = 0.9e0
        tdiv = tdivges
        tpts = tptsges

        for i in range(15):

            #  Find derivatives for Newton's method

            tptsp = tpts * (1.0e0 + epsilon)
            deltx = tpts * epsilon
            tdivp = tdiv * (1.0e0 + epsilon)
            delty = tdiv * epsilon

            f1 = divm.ftpts(
                aion,
                coefl,
                delne,
                fififi,
                omegan,
                omlarg,
                qdiv,
                tconl,
                xpara,
                xperp,
                tpts,
                tdiv,
            )
            f2 = divm.ftdiv(
                aion,
                coefl,
                delne,
                fififi,
                omegan,
                omlarg,
                qdiv,
                tconl,
                xpara,
                xperp,
                tpts,
                tdiv,
            )

            f1dx = (
                divm.ftpts(
                    aion,
                    coefl,
                    delne,
                    fififi,
                    omegan,
                    omlarg,
                    qdiv,
                    tconl,
                    xpara,
                    xperp,
                    tptsp,
                    tdiv,
                )
                - f1
            ) / deltx
            f1dy = (
                divm.ftpts(
                    aion,
                    coefl,
                    delne,
                    fififi,
                    omegan,
                    omlarg,
                    qdiv,
                    tconl,
                    xpara,
                    xperp,
                    tpts,
                    tdivp,
                )
                - f1
            ) / delty
            f2dx = (
                divm.ftdiv(
                    aion,
                    coefl,
                    delne,
                    fififi,
                    omegan,
                    omlarg,
                    qdiv,
                    tconl,
                    xpara,
                    xperp,
                    tptsp,
                    tdiv,
                )
                - f2
            ) / deltx
            f2dy = (
                divm.ftdiv(
                    aion,
                    coefl,
                    delne,
                    fififi,
                    omegan,
                    omlarg,
                    qdiv,
                    tconl,
                    xpara,
                    xperp,
                    tpts,
                    tdivp,
                )
                - f2
            ) / delty

            denom = f1dx * f2dy - f1dy * f2dx
            if denom == 0.0e0:
                denom = 1.0e-10
            deltpts = (-f2dy * f1 + f1dy * f2) / denom
            deltdiv = (f2dx * f1 - f1dx * f2) / denom

            #  New guess

            tdiv = tdiv + deltdiv
            tpts = tpts + deltpts
            delta = abs(deltdiv / tdiv + deltpts / tpts)

            #  Satisfied yet?

            if delta < relerr:
                break

        tdiv = max(tdiv, 0.1000e0)
        tpts = max(tpts, 0.0010e0)
        tpts = min(tpts, 0.9999e0)

        #  Some other quantities

        ct = max(0.1e0, (c1div + c2div / (tdiv)))
        ptpdiv = tdiv * ct
        gamdiv = divm.gammash(fififi, tdiv)  #  sheath coefficient
        dendiv = delne / (omegan * tpts)
        eier = divm.erprcy(
            tdiv, dendiv
        )  #  ionization + radiation energy / recycle event

        tsep = (
            251.0e0
            * (
                (qdiv * tconl) ** 2
                / (c27 * xpara * (1.0e0 - tpts ** 3.5e0))
                * coefl
                / (xperp * delne)
            )
            ** 0.2222222e0
        )

        cp = max(0.1e0, (c3div + c4div / (tdiv)))
        angle = math.sin(anginc) * rbpbt

        if minstang != 0.0e0:
            angle = max(angle, (minstang / 57.3e0))

        ppdiv = (
            2.48e2
            * (qdiv) ** 1.55556e0
            / (xperp * delne) ** 0.777778e0
            * (c27 * xpara) ** 0.2222222e0
            * tconl ** 0.555556e0
            * ((1.0e0 - tpts ** 3.5e0) / coefl) ** 0.222222e0
            / omlarg
            * (1.0e0 + ei / (gamdiv * tdiv))
            / (1.0e0 + eier / (gamdiv * tdiv))
            * angle
            * cp
        )
        ppdivr = ppdiv * (1.0e0 + frrp * (eier - ei) / (gamdiv * tdiv))
        gamdt = 6.25e4 * ppdiv / (gamdiv * ptpdiv)
        densin = omegan * tsep * delne / ptpdiv
        delw = (
            4.01e-3
            * (delne * xperp) ** 0.7777778e0
            * tconl ** 0.4444444e0
            * coefl ** 0.2222222e0
            / (
                (qdiv) ** 0.55555556e0
                * (c27 * xpara * (1.0e0 - tpts ** 3.5e0)) ** 0.22222e0
            )
        )
        lamp = pdiv * rsrd / (2.0e0 * constants.pi * rmaj * ppdiv)

        return (
            delta,
            delw,
            dendiv,
            densin,
            gamdt,
            lamp,
            omlarg,
            ppdiv,
            ppdivr,
            ptpdiv,
            tdiv,
            tsep,
        )
