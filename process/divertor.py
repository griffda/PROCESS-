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

        # output deleted as per discussion in #242.
        # tnunn: 19/01/2022
