from process.fortran import pfcoil_module as pf
from process.fortran import pfcoil_variables as pfv
from process.fortran import times_variables as tv
from process.fortran import error_handling as eh
from process.fortran import build_variables as bv
from process.fortran import physics_variables as pv
from process.fortran import tfcoil_variables as tfv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import constants
from process.fortran import cs_fatigue as csf
from process.fortran import cs_fatigue_variables as csfv
from process import fortran as ft
import math
import numpy as np


class PFCoil:
    """Calculate poloidal field coil system parameters."""

    def __init__(self):
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        pf.init_pfcoil_module()

    def run(self):
        """Run the PF coil model."""
        self.pfcoil()

        # Poloidal field coil inductance calculation
        pf.induct(self.outfile, 0)

        # Volt-second capability of PF coil set
        pf.vsec()

    def output(self):
        """Output results to output file."""
        ft.pfcoil_module.outpf(self.outfile)
        ft.pfcoil_module.outvolt(self.outfile)

    def output_induct(self):
        """Output poloidal field coil inductance calculation."""
        ft.pfcoil_module.induct(self.outfile, 1)

    def pfcoil(self):
        """Routine to perform calculations for the PF and Central Solenoid coils.
        author: P J Knight, CCFE, Culham Science Centre
        author: R Kemp, CCFE, Culham Science Centre
        None
        This subroutine performs the calculations for the PF and
        Central Solenoid coils, to determine their size, location, current waveforms,
        stresses etc.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        lrow1 = 2 * pfv.nptsmx + pfv.ngrpmx
        lcol1 = pfv.ngrpmx

        pcls0 = np.zeros(pfv.ngrpmx, dtype=int)
        ncls0 = np.zeros(pfv.ngrpmx + 2, dtype=int)

        pf.rcls0 = np.zeros((pfv.ngrpmx, pfv.nclsmx), order="F")
        pf.zcls0 = np.zeros((pfv.ngrpmx, pfv.nclsmx), order="F")
        pf.ccls0 = np.zeros(int(pfv.ngrpmx / 2))
        sigma = np.zeros(pfv.ngrpmx)
        work2 = np.zeros(pfv.ngrpmx)
        rc = np.zeros(pfv.nclsmx)
        zc = np.zeros(pfv.nclsmx)
        cc = np.zeros(pfv.nclsmx)
        xc = np.zeros(pfv.nclsmx)
        brin = np.zeros(pfv.nptsmx)
        bzin = np.zeros(pfv.nptsmx)
        rpts = np.zeros(pfv.nptsmx)
        zpts = np.zeros(pfv.nptsmx)
        bfix = np.zeros(lrow1)
        bvec = np.zeros(lrow1)
        gmat = np.zeros((lrow1, lcol1), order="F")
        umat = np.zeros((lrow1, lcol1), order="F")
        vmat = np.zeros((lrow1, lcol1), order="F")
        signn = np.zeros(2)
        aturn = np.zeros(pfv.ngc2)

        #  Toggle switch for pfv.ipfloc()=2 coils above/below midplane
        top_bottom = 1

        #  Set up the number of PF coils including the Central Solenoid (pfv.nohc),
        #  and the number of PF circuits including the plasma (pfv.ncirt)
        if pfv.ngrp > pfv.ngrpmx:
            eh.idiags[0] = pfv.ngrp
            eh.idiags[1] = pfv.ngrpmx
            eh.report_error(64)

        #  Total the number of PF coils in all groups, and check that none
        #  exceeds the limit
        pfv.nohc = 0
        for i in range(pfv.ngrp):
            if pfv.ncls[i] > pfv.nclsmx:
                eh.idiags[0] = i
                eh.idiags[1] = pfv.ncls[i]
                eh.idiags[2] = pfv.nclsmx
                eh.report_error(65)

            pfv.nohc = pfv.nohc + pfv.ncls[i]

        #  Add one if an Central Solenoid is present, and make an extra group
        if bv.iohcl != 0:
            pfv.nohc = pfv.nohc + 1
            pfv.ncls[pfv.ngrp] = 1

        #  Add one for the plasma
        pfv.ncirt = pfv.nohc + 1

        #  Overall current density in the Central Solenoid at beginning of pulse
        pfv.cohbop = pfv.coheof * pfv.fcohbop

        #  Set up array of times
        tv.tim[0] = 0.0e0
        tv.tim[1] = tv.tramp
        tv.tim[2] = tv.tim[1] + tv.tohs
        tv.tim[3] = tv.tim[2] + tv.theat
        tv.tim[4] = tv.tim[3] + tv.tburn
        tv.tim[5] = tv.tim[4] + tv.tqnch

        #  Set up call to MHD scaling routine for coil currents.
        #  First break up Central Solenoid solenoid into 'filaments'

        #  Central Solenoid radius
        pfv.rohc = bv.bore + 0.5e0 * bv.ohcth

        #  nfxf is the total no of filaments into which the Central Solenoid is split,
        #  if present
        if bv.iohcl == 0:
            pf.nfxf = 0
            ioheof = 0.0e0
        else:
            pf.nfxf = 2 * pfv.nfxfh

            #  total Central Solenoid current at EOF
            ioheof = -bv.hmax * pfv.ohhghf * bv.ohcth * 2.0e0 * pfv.coheof

            if pf.nfxf > pfv.nfixmx:
                eh.idiags[0] = pf.nfxf
                eh.idiags[1] = pfv.nfixmx
                eh.report_error(66)

            #  Symmetric up/down Central Solenoid : Find (R,Z) and current of each filament at BOP

            for nng in range(pfv.nfxfh):
                pf.rfxf[nng] = pfv.rohc
                pf.rfxf[nng + pfv.nfxfh] = pf.rfxf[nng]
                pf.zfxf[nng] = bv.hmax * pfv.ohhghf / pfv.nfxfh * ((nng + 1) - 0.5e0)
                pf.zfxf[nng + pfv.nfxfh] = -pf.zfxf[nng]
                pf.cfxf[nng] = -ioheof / pf.nfxf * pfv.fcohbop
                pf.cfxf[nng + pfv.nfxfh] = pf.cfxf[nng]

        #  Scale PF coil locations
        signn[0] = 1.0e0
        signn[1] = -1.0e0
        pf.rclsnorm = bv.r_tf_outboard_mid + 0.5e0 * bv.tfthko + pfv.routr

        # Place the PF coils:

        #  N.B. Problems here if k=pfv.ncls(group) is greater than 2.
        for j in range(pfv.ngrp):

            if pfv.ipfloc[j] == 1:
                #  PF coil is stacked on top of the Central Solenoid
                for k in pfv.ncls[j]:
                    pf.rcls[j, k] = pfv.rohc + pfv.rpf1

                    #  Z coordinate of coil enforced so as not
                    #  to occupy the same space as the Central Solenoid
                    pf.zcls[j, k] = signn[k] * (
                        bv.hmax * pfv.ohhghf
                        + 0.1e0
                        + 0.5e0 * (bv.hmax * (1.0e0 - pfv.ohhghf) + bv.tfcth + 0.1e0)
                    )

            elif pfv.ipfloc[j] == 2:
                #  PF coil is on top of the TF coil
                for k in range(pfv.ncls[j]):
                    pf.rcls[j, k] = pv.rmajor + pfv.rpf2 * pv.triang * pv.rminor
                    if pv.itart == 1 and pv.itartpf == 0:
                        pf.zcls[j, k] = (bv.hmax - pfv.zref[j]) * signn[k]
                    else:
                        # pf.zcls(j,k) = (bv.hmax + bv.tfcth + 0.86e0) * signn(k)
                        if top_bottom == 1:  #  this coil is above midplane
                            pf.zcls[j, k] = bv.hpfu + 0.86e0
                            top_bottom = -1
                        else:  #  this coil is below midplane
                            pf.zcls[j, k] = -1.0e0 * (
                                bv.hpfu - 2.0e0 * bv.hpfdif + 0.86e0
                            )
                            top_bottom = 1

            elif pfv.ipfloc[j] == 3:
                #  PF coil is radially outside the TF coil
                for k in range(pfv.ncls[j]):
                    pf.zcls[j, k] = pv.rminor * pfv.zref[j] * signn[k]
                    #  Coil radius follows TF coil curve for SC TF (D-shape)
                    #  otherwise stacked for resistive TF (rectangle-shape)
                    if tfv.i_tf_sup != 1 or pfv.i_sup_pf_shape == 1:
                        pf.rcls[j, k] = pf.rclsnorm
                    else:
                        pf.rcls[j, k] = math.sqrt(pf.rclsnorm ** 2 - pf.zcls[j, k] ** 2)

            elif pfv.ipfloc[j] == 4:
                #  PF coil is in general location
                #  See issue 1418
                #  https://git.ccfe.ac.uk/process/process/-/issues/1418
                for k in pfv.ncls[j]:
                    pf.zcls[j, k] = pv.rminor * pfv.zref[j] * signn[k]
                    pf.rcls[j, k] = pv.rminor * pfv.rref[j] + pv.rmajor

            else:
                eh.idiags[0] = j
                eh.idiags[1] = pfv.ipfloc[j]
                eh.report_error(67)

        # Allocate current to the PF coils:
        #  "Flux swing coils" participate in cancellation of the CS
        #  field during a flux swing. "Equilibrium coils" are varied
        #  to create the equilibrium field, targeting the correct
        #  vertical field
        #  As implemented, all coils are flux swing coils
        #  As implemented, Location 3 and 4 coils are equilibrium
        #  coils.

        #  Flux swing coils:
        if pfv.cohbop != 0.0e0:
            #  Find currents for plasma initiation to null field across plasma
            npts = 32  #  Number of test points across plasma midplane
            if npts > pfv.nptsmx:
                eh.idiags[0] = npts
                eh.idiags[1] = pfv.nptsmx
                eh.report_error(68)

            #  Position and B-field at each test point
            drpt = 2.0e0 * pv.rminor / (npts - 1)
            rpt0 = pv.rmajor - pv.rminor

            for i in range(npts):
                rpts[i] = rpt0 + (i) * drpt
                zpts[i] = 0.0e0
                brin[i] = 0.0e0
                bzin[i] = 0.0e0

                #  Calculate currents in coils to produce the given field
            pf.ssq0, pf.ccl0 = pf.efc(
                npts,
                rpts,
                zpts,
                brin,
                bzin,
                pf.nfxf,
                pf.rfxf,
                pf.zfxf,
                pf.cfxf,
                pfv.ngrp,
                pfv.ncls,
                pf.rcls,
                pf.zcls,
                pfv.alfapf,
                bfix,
                gmat,
                bvec,
                rc,
                zc,
                cc,
                xc,
                umat,
                vmat,
                sigma,
                work2,
            )

        #  Equilibrium coil currents determined by SVD targeting B
        if pfv.i_pf_current == 1:
            #  Simple coil current scaling for STs (good only for A < about 1.8)
            #  Bypasses SVD solver
            if pv.itart == 1 and pv.itartpf == 0:
                for i in range(pfv.ngrp):
                    if pfv.ipfloc[i] == 1:
                        #  PF coil is stacked on top of the Central Solenoid
                        pf.ccls[i] = 0.0e0
                        eh.idiags[0] = i
                        eh.report_error(69)

                    elif pfv.ipfloc[i] == 2:
                        #  PF coil is on top of the TF coil
                        pf.ccls[i] = 0.3e0 * pv.aspect ** 1.6e0 * pv.plascur

                    elif pfv.ipfloc[i] == 3:
                        #  PF coil is radially outside the TF coil
                        pf.ccls[i] = -0.4e0 * pv.plascur

                    else:
                        eh.idiags[0] = i
                        eh.idiags[1] = pfv.ipfloc[i]
                        eh.report_error(70)

                #  Vertical field (T)
                pv.bvert = (
                    -1.0e-7
                    * pv.plascur
                    / pv.rmajor
                    * (
                        math.log(8.0e0 * pv.aspect)
                        + pv.betap
                        + (pv.rli / 2.0e0)
                        - 1.5e0
                    )
                )

            else:

                #  Conventional aspect ratio scaling
                nfxf0 = 0
                ngrp0 = 0
                nocoil = 0
                for i in range(pfv.ngrp):
                    if pfv.ipfloc[i] == 1:
                        #  PF coil is stacked on top of the Central Solenoid
                        #  This coil is to balance Central Solenoid flux and should not be involved
                        #  in equilibrium calculation -- RK 07/12
                        pf.ccls[i] = 0.0e0
                        nfxf0 = nfxf0 + pfv.ncls[i]
                        for ccount in range(pfv.ncls[i]):
                            pf.rfxf[nocoil] = pf.rcls[i, ccount]
                            pf.zfxf[nocoil] = pf.zcls[i, ccount]
                            pf.cfxf[nocoil] = pf.ccls[i]
                            nocoil = nocoil + 1

                    elif pfv.ipfloc[i] == 2:
                        #  PF coil is on top of the TF coil; divertor coil
                        #  This is a fixed current for this calculation -- RK 07/12

                        pf.ccls[i] = (
                            pv.plascur
                            * 2.0e0
                            * (1.0e0 - (pv.kappa * pv.rminor) / abs(pf.zcls[i, 0]))
                        )
                        nfxf0 = nfxf0 + pfv.ncls[i]
                        for ccount in range(pfv.ncls[i]):
                            pf.rfxf[nocoil] = pf.rcls[i, ccount]
                            pf.zfxf[nocoil] = pf.zcls[i, ccount]
                            pf.cfxf[nocoil] = pf.ccls[i]
                            nocoil = nocoil + 1

                    elif pfv.ipfloc[i] == 3:
                        #  PF coil is radially outside the TF coil
                        #  This is an equilibrium coil, current must be solved for

                        pcls0[ngrp0] = i + 1
                        ngrp0 = ngrp0 + 1

                    elif pfv.ipfloc[i] == 4:
                        #  PF coil is generally placed
                        #  See issue 1418
                        #  https://git.ccfe.ac.uk/process/process/-/issues/1418
                        #  This is an equilibrium coil, current must be solved for

                        pcls0[ngrp0] = i + 1
                        ngrp0 = ngrp0 + 1

                    else:
                        eh.idiags[0] = i
                        eh.idiags[1] = pfv.ipfloc[i]
                        eh.report_error(70)

                for ccount in range(ngrp0):
                    ncls0[ccount] = 2
                    pf.rcls0[ccount, 0] = pf.rcls[pcls0[ccount] - 1, 0]
                    pf.rcls0[ccount, 1] = pf.rcls[pcls0[ccount] - 1, 1]
                    pf.zcls0[ccount, 0] = pf.zcls[pcls0[ccount] - 1, 0]
                    pf.zcls0[ccount, 1] = pf.zcls[pcls0[ccount] - 1, 1]

                npts0 = 1
                rpts[0] = pv.rmajor
                zpts[0] = 0.0e0
                brin[0] = 0.0e0

                #  Added pv.rli term correctly -- RK 07/12

                bzin[0] = (
                    -1.0e-7
                    * pv.plascur
                    / pv.rmajor
                    * (
                        math.log(8.0e0 * pv.aspect)
                        + pv.betap
                        + (pv.rli / 2.0e0)
                        - 1.5e0
                    )
                )

                pv.bvert = bzin[0]

                ssqef, pf.ccls0 = pf.efc(
                    npts0,
                    rpts,
                    zpts,
                    brin,
                    bzin,
                    nfxf0,
                    pf.rfxf,
                    pf.zfxf,
                    pf.cfxf,
                    ngrp0,
                    ncls0,
                    pf.rcls0,
                    pf.zcls0,
                    pfv.alfapf,
                    bfix,
                    gmat,
                    bvec,
                    rc,
                    zc,
                    cc,
                    xc,
                    umat,
                    vmat,
                    sigma,
                    work2,
                )

                for ccount in range(ngrp0):
                    pf.ccls[pcls0[ccount] - 1] = pf.ccls0[ccount]

        #  Flux swing from vertical field

        #  If this is the first visit to the routine the inductance matrix
        #  pfv.sxlg and the pfv.turns array have not yet been calculated, so we set
        #  them to (very) approximate values to avoid strange behaviour...
        if pf.first_call:
            pfv.sxlg[:, :] = 1.0e0
            pfv.turns[:] = 100.0e0
            pf.first_call = False

        pfflux = 0.0e0
        nocoil = 0
        for ccount in range(pfv.ngrp):
            for i in range(pfv.ncls[ccount]):
                pfflux = pfflux + (
                    pf.ccls[ccount]
                    * pfv.sxlg[nocoil, pfv.ncirt - 1]
                    / pfv.turns[nocoil]
                )
                nocoil = nocoil + 1

        #  Flux swing required from CS coil
        csflux = -(pv.vsres + pv.vsind) - pfflux

        if bv.iohcl == 1:
            #  Required current change in CS coil

            #  Proposed new calculation...
            # dics = csflux / pfv.sxlg(pfv.nohc,pfv.ncirt)
            #  BUT... pfv.sxlg(pfv.nohc,pfv.ncirt) is around 2000 times ddics below...

            ddics = (
                4.0e-7
                * constants.pi
                * constants.pi
                * (
                    (bv.bore * bv.bore)
                    + (bv.ohcth * bv.ohcth) / 6.0e0
                    + (bv.ohcth * bv.bore) / 2.0e0
                )
                / (bv.hmax * pfv.ohhghf * 2.0e0)
            )
            dics = csflux / ddics

            pfv.fcohbof = ((-ioheof * pfv.fcohbop) + dics) / ioheof
            pfv.fcohbof = min(
                pfv.fcohbof, 1.0e0
            )  #  constrains abs(pfv.fcohbof) <= 1.0;
            pfv.fcohbof = max(pfv.fcohbof, -1.0e0)  #  probably un-necessary

        else:
            dics = 0.0e0
            pfv.fcohbof = 1.0e0
            eh.report_error(71)

        #  Split groups of coils into one set containing ncl coils
        ncl = 0
        for nng in range(pfv.ngrp):
            for ng2 in range(pfv.ncls[nng]):
                pfv.rpf[ncl] = pf.rcls[nng, ng2]
                pfv.zpf[ncl] = pf.zcls[nng, ng2]

                #  Currents at different times:

                #  If PF coil currents are computed, not input via pfv.pf.ccl0_ma, pfv.pf.ccls_ma:
                #  Then set pfv.pf.ccl0_ma,pfv.pf.ccls_ma from the computed pf.ccl0,pf.ccls
                if pfv.i_pf_current != 0:
                    pfv.ccl0_ma[nng] = 1.0e-6 * pf.ccl0[nng]
                    pfv.ccls_ma[nng] = 1.0e-6 * pf.ccls[nng]
                else:
                    #  Otherwise set pf.ccl0,pf.ccls via the input pfv.pf.ccl0_ma and pfv.pf.ccls_ma
                    pf.ccl0[nng] = 1.0e6 * pfv.pf.ccl0_ma[nng]
                    pf.ccls[nng] = 1.0e6 * pfv.pf.ccls_ma[nng]

                #  Beginning of pulse: t = tv.tramp
                pfv.curpfs[ncl] = 1.0e-6 * pf.ccl0[nng]

                #  Beginning of flat-top: t = tv.tramp+tv.tohs
                pfv.curpff[ncl] = 1.0e-6 * (
                    pf.ccls[nng] - (pf.ccl0[nng] * pfv.fcohbof / pfv.fcohbop)
                )

                #  End of flat-top: t = tv.tramp+tv.tohs+tv.theat+tv.tburn
                pfv.curpfb[ncl] = 1.0e-6 * (
                    pf.ccls[nng] - (pf.ccl0[nng] * (1.0e0 / pfv.fcohbop))
                )

                ncl = ncl + 1

        #  Current in Central Solenoid as a function of time
        #  N.B. If the Central Solenoid is not present then ioheof is zero.
        pfv.curpfs[ncl] = -1.0e-6 * ioheof * pfv.fcohbop
        pfv.curpff[ncl] = 1.0e-6 * ioheof * pfv.fcohbof
        pfv.curpfb[ncl] = 1.0e-6 * ioheof

        #  Set up coil current waveforms, normalised to the peak current in
        #  each coil
        pf.waveform()  #  returns pfv.ric(), pfv.waves()

        #  Calculate PF coil geometry, current and number of pfv.turns
        #  Dimensions are those of the winding pack, and exclude
        #  the steel supporting case
        i = 0
        pfv.pfrmax = 0.0e0

        dz = 0

        for ii in range(pfv.ngrp):
            for ij in range(pfv.ncls[ii]):

                if pfv.ipfloc[ii] == 1:
                    #  PF coil is stacked on top of the Central Solenoid
                    dx = 0.5e0 * bv.ohcth
                    dz = 0.5e0 * (
                        bv.hmax * (1.0e0 - pfv.ohhghf) + bv.tfcth + 0.1e0
                    )  #  ???
                    area = 4.0e0 * dx * dz

                    #  Number of pfv.turns
                    #  CPTDIN[i] is the current per turn (input)
                    pfv.turns[i] = abs((pfv.ric[i] * 1.0e6) / pfv.cptdin[i])
                    aturn[i] = area / pfv.turns[i]

                    #  Actual winding pack current density
                    pfv.rjconpf[i] = 1.0e6 * abs(pfv.ric[i]) / area

                    #  Location of edges of each coil:
                    #  pfv.ra = inner radius, pfv.rb = outer radius
                    #  pfv.zl = 'lower' edge z (i.e. edge nearer to midplane)
                    #  pfv.zh = 'upper' edge z (i.e. edge further from midplane)
                    pfv.ra[i] = pfv.rpf[i] - dx
                    pfv.rb[i] = pfv.rpf[i] + dx

                    pfv.zl[i] = pfv.zpf[i] - dz
                    if pfv.zpf[i] < 0.0e0:
                        pfv.zl[i] = pfv.zpf[i] + dz

                    pfv.zh[i] = pfv.zpf[i] + dz

                    if pfv.zpf[i] < 0.0e0:
                        pfv.zh[i] = pfv.zpf[i] - dz

                else:

                    #  Other coils. N.B. Current density RJCONPF[i] is defined in
                    #  routine INITIAL for these coils.
                    area = abs(pfv.ric[i] * 1.0e6 / pfv.rjconpf[i])

                    pfv.turns[i] = abs((pfv.ric[i] * 1.0e6) / pfv.cptdin[i])
                    aturn[i] = area / pfv.turns[i]

                    dx = 0.5e0 * math.sqrt(area)  #  square cross-section

                    pfv.ra[i] = pfv.rpf[i] - dx
                    pfv.rb[i] = pfv.rpf[i] + dx

                    pfv.zl[i] = pfv.zpf[i] - dx
                    if pfv.zpf[i] < 0.0e0:
                        pfv.zl[i] = pfv.zpf[i] + dx

                    pfv.zh[i] = pfv.zpf[i] + dx
                    if pfv.zpf[i] < 0.0e0:
                        pfv.zh[i] = pfv.zpf[i] - dx

                #  Outside radius of largest PF coil (m)
                pfv.pfrmax = max(pfv.pfrmax, pfv.rb[i])

                i = i + 1

        #  Calculate peak field, allowable current density, resistive
        #  power losses and volumes and weights for each PF coil
        i = 0
        it = 0
        pfv.powpfres = 0.0e0
        pfv.pfmmax = 0.0e0

        for ii in range(pfv.ngrp):
            iii = ii
            for ij in range(pfv.ncls[ii]):

                #  Peak field

                if ij == 0:
                    # Index args +1ed
                    bri, bro, bzi, bzo = self.peakb(
                        i + 1, iii + 1, it
                    )  #  returns pfv.bpf, bpf2

                #  Allowable current density (for superconducting coils)

                if pfv.ipfres == 0:
                    bmax = max(abs(pfv.bpf[i]), abs(pf.bpf2[i]))
                    pfv.rjpfalw[i], jstrand, jsc, tmarg = pf.superconpf(
                        bmax,
                        pfv.vf[i],
                        pfv.fcupfsu,
                        pfv.rjconpf[i],
                        pfv.isumatpf,
                        tfv.fhts,
                        tfv.strncon_pf,
                        tfv.tftmp,
                        tfv.bcritsc,
                        tfv.tcritsc,
                    )

                #  Length of conductor

                rll = 2.0e0 * constants.pi * pfv.rpf[i] * pfv.turns[i]

                #  Resistive coils

                if pfv.ipfres == 1:
                    #  Coil resistance (pfv.vf is the void fraction)

                    respf = pfv.pfclres * rll / (aturn[i] * (1.0e0 - pfv.vf[i]))

                    #  Sum resistive power losses

                    pfv.powpfres = (
                        pfv.powpfres
                        + respf * (1.0e6 * pfv.curpfb[i] / pfv.turns[i]) ** 2
                    )

                #  Winding pack volume

                volpf = aturn[i] * rll

                #  Conductor weight (pfv.vf is the void fraction)

                if pfv.ipfres == 0:
                    pfv.wtc[i] = (
                        volpf * tfv.dcond[pfv.isumatpf - 1] * (1.0e0 - pfv.vf[i])
                    )
                else:
                    pfv.wtc[i] = volpf * constants.dcopper * (1.0e0 - pfv.vf[i])

                #  (J x B) force on coil

                forcepf = (
                    0.5e6 * (pfv.bpf[i] + pf.bpf2[i]) * abs(pfv.ric[i]) * pfv.rpf[i]
                )

                #  Stress ==> cross-sectional area of supporting steel to use

                if pfv.ipfres == 0:
                    #  Superconducting coil
                    #  Previous assumptions: 500 MPa stress limit with 2/3 of the force
                    #  supported in the outer (steel) case.
                    #  Now, 500 MPa replaced by pfv.sigpfcalw, 2/3 factor replaced by pfv.sigpfcf

                    areaspf = pfv.sigpfcf * forcepf / (pfv.sigpfcalw * 1.0e6)

                    #  Assume a case of uniform thickness around coil cross-section
                    #  Thickness found via a simple quadratic equation

                    drpdz = (
                        pfv.rb[i] - pfv.ra[i] + abs(pfv.zh[i] - pfv.zl[i])
                    )  #  dr + dz
                    pfv.pfcaseth[i] = 0.25e0 * (
                        -drpdz + math.sqrt(drpdz * drpdz + 4.0e0 * areaspf)
                    )

                else:
                    areaspf = 0.0e0  #  Resistive coil - no steel needed
                    pfv.pfcaseth[i] = 0.0e0

                #  Weight of steel case

                pfv.wts[i] = areaspf * 2.0e0 * constants.pi * pfv.rpf[i] * fwbsv.denstl

                #  Mass of heaviest PF coil (tonnes)

                pfv.pfmmax = max(pfv.pfmmax, (1.0e-3 * (pfv.wtc[i] + pfv.wts[i])))
                i = i + 1

        #  Find sum of current x pfv.turns x radius for all coils for 2015 costs model
        c = 0
        pfv.itr_sum = 0.0e0
        for m in range(pfv.ngrp):
            for n in range(pfv.ncls[m]):
                pfv.itr_sum = pfv.itr_sum + (pfv.rpf[c] * pfv.turns[c] * pfv.cptdin[c])
                c = c + 1

        pfv.itr_sum = pfv.itr_sum + (
            (bv.bore + 0.5 * bv.ohcth)
            * pfv.turns[pfv.nohc - 1]
            * pfv.cptdin[pfv.nohc - 1]
        )

        #  Find Central Solenoid information
        if bv.iohcl != 0:
            self.ohcalc()

        #  Summation of weights and current
        pfv.whtpf = 0.0e0
        pfv.whtpfs = 0.0e0
        ricpf = 0.0e0

        for i in range(pfv.nohc):
            pfv.whtpf = pfv.whtpf + pfv.wtc[i]
            pfv.whtpfs = pfv.whtpfs + pfv.wts[i]
            ricpf = ricpf + abs(pfv.ric[i])

        #  Plasma size and shape
        pfv.zh[pfv.nohc] = pv.rminor * pv.kappa
        pfv.zl[pfv.nohc] = -pv.rminor * pv.kappa
        pfv.ra[pfv.nohc] = pv.rmajor - pv.rminor
        pfv.rb[pfv.nohc] = pv.rmajor + pv.rminor
        pfv.turns[pfv.nohc] = 1.0e0

        #  Generate coil currents as a function of time using
        #  user-provided waveforms etc. (pfv.cptdin, pfv.fcohbop, pfv.fcohbof)
        for k in range(6):  #  time points
            for i in range(pfv.ncirt - 1):
                pfv.cpt[i, k] = pfv.waves[i, k] * math.copysign(
                    pfv.cptdin[i], pfv.ric[i]
                )

        #  Plasma wave form
        pfv.cpt[pfv.ncirt - 1, 0] = 0.0e0
        pfv.cpt[pfv.ncirt - 1, 1] = 0.0e0
        pfv.cpt[pfv.ncirt - 1, 2] = pv.plascur
        pfv.cpt[pfv.ncirt - 1, 3] = pv.plascur
        pfv.cpt[pfv.ncirt - 1, 4] = pv.plascur
        pfv.cpt[pfv.ncirt - 1, 5] = 0.0e0

    def ohcalc(self):
        """Routine to perform calculations for the Central Solenoid solenoid.
        
        author: P J Knight, CCFE, Culham Science Centre
        This subroutine performs the calculations for the
        Central Solenoid solenoid coil.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        hohc = bv.hmax * pfv.ohhghf

        #  Z coordinates of coil edges
        pfv.zh[pfv.nohc - 1] = hohc
        pfv.zl[pfv.nohc - 1] = -pfv.zh[pfv.nohc - 1]

        #  (R,Z) coordinates of coil centre
        pfv.rpf[pfv.nohc - 1] = pfv.rohc
        pfv.zpf[pfv.nohc - 1] = 0.0e0

        #  Radius of outer edge
        pfv.rb[pfv.nohc - 1] = pfv.rohc + 0.5e0 * bv.ohcth

        #  Radius of inner edge
        pfv.ra[pfv.nohc - 1] = pfv.rb[pfv.nohc - 1] - bv.ohcth

        #  Total cross-sectional area
        pfv.areaoh = 2.0e0 * hohc * bv.ohcth

        #  Maximum current (MA-pfv.turns) in Centpfv.ral Solenoid, at either BOP or EOF
        if pfv.cohbop > pfv.coheof:
            sgn = 1.0e0
            pfv.ric[pfv.nohc - 1] = sgn * 1.0e-6 * pfv.cohbop * pfv.areaoh
        else:
            sgn = -1.0e0
            pfv.ric[pfv.nohc - 1] = sgn * 1.0e-6 * pfv.coheof * pfv.areaoh

        #  Number of pfv.turns
        pfv.turns[pfv.nohc - 1] = (
            1.0e6 * abs(pfv.ric[pfv.nohc - 1]) / pfv.cptdin[pfv.nohc - 1]
        )

        # Turn vertical cross-sectionnal area
        pfv.a_oh_turn = pfv.areaoh / pfv.turns[pfv.nohc - 1]

        #  Non-steel area void fpfv.raction for coolant
        pfv.vf[pfv.nohc - 1] = pfv.vfohc

        #  Peak field at the End-Of-Flattop (EOF)
        #  Occurs at inner edge of coil; bmaxoh2 and bzi are of opposite sign at EOF

        #  Peak field due to Centpfv.ral Solenoid itself
        bmaxoh2 = pf.bfmax(
            pfv.coheof, pfv.ra[pfv.nohc - 1], pfv.rb[pfv.nohc - 1], hohc
        )

        #  Peak field due to other PF coils plus plasma
        timepoint = 5
        bri, bro, bzi, bzo = self.peakb(pfv.nohc, 99, timepoint)

        pfv.bmaxoh = abs(bzi - bmaxoh2)
        bohci = pfv.bmaxoh

        #  Peak field on outboard side of Centpfv.ral Solenoid
        #  (self-field is assumed to be zero - long solenoid approximation)
        bohco = abs(bzo)

        #  Peak field at the Beginning-Of-Pulse (BOP)
        #  Occurs at inner edge of coil; pfv.bmaxoh0 and bzi are of same sign at BOP
        pfv.bmaxoh0 = pf.bfmax(
            pfv.cohbop, pfv.ra[pfv.nohc - 1], pfv.rb[pfv.nohc - 1], hohc
        )
        timepoint = 2
        bri, bro, bzi, bzo = self.peakb(pfv.nohc, 99, timepoint)

        pfv.bmaxoh0 = abs(pfv.bmaxoh0 + bzi)

        #  Maximum field values
        pfv.bpf[pfv.nohc - 1] = max(pfv.bmaxoh, abs(pfv.bmaxoh0))
        pf.bpf2[pfv.nohc - 1] = max(bohco, abs(bzo))

        #  (J x B) hoop force on Centpfv.ral Solenoid (N)
        forcepf = (
            0.5e6
            * (pfv.bpf[pfv.nohc - 1] + pf.bpf2[pfv.nohc - 1])
            * abs(pfv.ric[pfv.nohc - 1])
            * pfv.rpf[pfv.nohc - 1]
        )

        #  Stress ==> cross-sectional area of supporting steel to use
        if pfv.ipfres == 0:

            #  Superconducting coil

            # New calculation from M. N. Wilson for hoop stress
            pf.sig_hoop = pf.hoop_stress(pfv.ra[pfv.nohc - 1])

            # New calculation from Y. Iwasa for axial stress
            pf.sig_axial, pf.axial_force = pf.axial_stress()

            #  Allowable (hoop) stress (Pa) alstroh
            # Now a user input
            # alstroh = min( (2.0e0*csytf/3.0e0), (0.5e0*csutf) )

            # Calculation of CS fatigue
            # this is only valid for pulsed reactor design
            if pv.facoh > 0.0e-4:
                csf.ncycle(
                    csfv.n_cycle,
                    pf.sig_hoop,
                    csfv.residual_sig_hoop,
                    csfv.t_crack_vertical,
                    csfv.t_crack_radial,
                    csfv.t_structural_vertical,
                    csfv.t_structural_radial,
                )

            # Now steel area fpfv.raction is itepfv.ration variable and constpfv.raint
            # equation is used for Centpfv.ral Solenoid stress

            # Area of steel in Centpfv.ral Solenoid
            areaspf = pfv.oh_steel_frac * pfv.areaoh
            # areaspf = forcepf / alstroh

            if pfv.i_cs_stress == 1:
                pfv.s_tresca_oh = max(
                    abs(pf.sig_hoop - pf.sig_axial),
                    abs(pf.sig_axial - 0.0e0),
                    abs(0.0e0 - pf.sig_hoop),
                )
            else:
                pfv.s_tresca_oh = max(
                    abs(pf.sig_hoop - 0.0e0), abs(0.0e0 - 0.0e0), abs(0.0e0 - pf.sig_hoop)
                )

            #  Thickness of hypothetical steel cylinders assumed to encase the CS along
            #  its inside and outside edges; in reality, the steel is distributed
            #  throughout the conductor
            pfv.pfcaseth[pfv.nohc - 1] = 0.25e0 * areaspf / hohc

        else:
            areaspf = 0.0e0  #  Resistive Central Solenoid - no steel needed
            pfv.pfcaseth[pfv.nohc - 1] = 0.0e0

        #  Weight of steel
        pfv.wts[pfv.nohc - 1] = (
            areaspf * 2.0e0 * constants.pi * pfv.rpf[pfv.nohc - 1] * fwbsv.denstl
        )

        #  Non-steel cross-sectional area
        pfv.awpoh = pfv.areaoh - areaspf

        #  Issue #97. Fudge to ensure pfv.awpoh is positive; result is continuous, smooth and
        #  monotonically decreases

        da = 0.0001e0  #  1 cm^2
        if pfv.awpoh < da:
            pfv.awpoh = da * da / (2.0e0 * da - pfv.awpoh)

        #  Weight of conductor in Centpfv.ral Solenoid
        if pfv.ipfres == 0:
            pfv.wtc[pfv.nohc - 1] = (
                pfv.awpoh
                * (1.0e0 - pfv.vfohc)
                * 2.0e0
                * constants.pi
                * pfv.rpf[pfv.nohc - 1]
                * tfv.dcond[pfv.isumatoh-1]
            )
        else:
            pfv.wtc[pfv.nohc - 1] = (
                pfv.awpoh
                * (1.0e0 - pfv.vfohc)
                * 2.0e0
                * constants.pi
                * pfv.rpf[pfv.nohc - 1]
                * constants.dcopper
            )

        if pfv.ipfres == 0:

            #  Allowable coil ovepfv.rall current density at EOF
            #  (superconducting coils only)

            jcritwp, pfv.jstrandoh_eof, pfv.jscoh_eof, tmarg1 = pf.superconpf(
                pfv.bmaxoh,
                pfv.vfohc,
                pfv.fcuohsu,
                (abs(pfv.ric[pfv.nohc - 1]) / pfv.awpoh) * 1.0e6,
                pfv.isumatoh,
                tfv.fhts,
                tfv.strncon_cs,
                tfv.tftmp,
                tfv.bcritsc,
                tfv.tcritsc,
            )

            pfv.rjohc = jcritwp * pfv.awpoh / pfv.areaoh

            #  Allowable coil ovepfv.rall current density at BOP

            jcritwp, pfv.jstrandoh_bop, pfv.jscoh_bop, tmarg2 = pf.superconpf(
                pfv.bmaxoh0,
                pfv.vfohc,
                pfv.fcuohsu,
                (abs(pfv.ric[pfv.nohc - 1]) / pfv.awpoh) * 1.0e6,
                pfv.isumatoh,
                tfv.fhts,
                tfv.strncon_cs,
                tfv.tftmp,
                tfv.bcritsc,
                tfv.tcritsc,
            )

            pfv.rjpfalw[pfv.nohc - 1] = jcritwp * pfv.awpoh / pfv.areaoh
            pfv.rjohc0 = pfv.rjpfalw[pfv.nohc - 1]

            pfv.tmargoh = min(tmarg1, tmarg2)

        else:
            #  Resistive power losses (non-superconducting coil)

            pfv.powohres = (
                2.0e0
                * constants.pi
                * pfv.rohc
                * pfv.pfclres
                / (pfv.areaoh * (1.0e0 - pfv.vfohc))
                * (1.0e6 * pfv.ric[pfv.nohc - 1]) ** 2
            )
            pfv.powpfres = pfv.powpfres + pfv.powohres

    def peakb(self, i, ii, it):
        """Calculates the peak field at a PF coil.

        author: P J Knight, CCFE, Culham Science Centre
        i : input integer : coil number
        ii : input integer : group number
        it : input integer : time point at which field is highest
        bri : output real : radial field at inner edge (T)
        bro : output real : radial field at outer edge (T)
        bzi : output real : vertical field at inner edge (T)
        bzo : output real : vertical field at outer edge (T)
        This routine calculates the peak magnetic field components
        at the inner and outer edges of a given PF coil.
        The calculation includes the effects from all the coils
        and the plasma.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        if bv.iohcl != 0 and i == pfv.nohc:
            #  Peak field is to be calculated at the Central Solenoid itself,
            #  so exclude its own contribution; its self field is
            #  dealt with externally using routine BFMAX
            kk = 0
        else:
            #  Check different times for maximum current
            if abs(pfv.curpfs[i - 1] - pfv.ric[i - 1]) < 1.0e-12:
                it = 2
            elif abs(pfv.curpff[i - 1] - pfv.ric[i - 1]) < 1.0e-12:
                it = 4
            elif abs(pfv.curpfb[i - 1] - pfv.ric[i - 1]) < 1.0e-12:
                it = 5
            else:
                eh.idiags[0] = it 
                eh.report_error(72)

            if bv.iohcl == 0:
                #  No Central Solenoid
                kk = 0
            else:
                if pfv.cohbop > pfv.coheof:
                    sgn = 1.0e0
                else:
                    sgn = -1.0e0

                #  Current in each filament representing part of the Central Solenoid
                for iohc in range(pf.nfxf):
                    pf.cfxf[iohc] = (
                        pfv.waves[pfv.nohc - 1, it - 1]
                        * pfv.coheof
                        * sgn
                        * bv.ohcth
                        * pfv.ohhghf
                        * bv.hmax
                        / pf.nfxf
                        * 2.0e0
                    )
            
                kk = pf.nfxf
    
        #  Non-Central Solenoid coils' contributions
        jj = 0
        for iii in range(pfv.ngrp):
            for jjj in range(pfv.ncls[iii]):
                
                jj = jj + 1
                #  Radius, z-coordinate and current for each coil
                if iii == ii - 1:
                    #  Self field from coil (Lyle's Method)
                    kk = kk + 1

                    dzpf = pfv.zh[jj - 1] - pfv.zl[jj - 1]
                    pf.rfxf[kk - 1] = pfv.rpf[jj - 1]
                    pf.zfxf[kk - 1] = pfv.zpf[jj - 1] + dzpf * 0.125e0
                    pf.cfxf[kk - 1] = (
                        pfv.ric[jj - 1] * pfv.waves[jj - 1, it - 1] * 0.25e6
                    )
                    kk = kk + 1
                    pf.rfxf[kk - 1] = pfv.rpf[jj - 1]
                    pf.zfxf[kk - 1] = pfv.zpf[jj - 1] + dzpf * 0.375e0
                    pf.cfxf[kk - 1] = (
                        pfv.ric[jj - 1] * pfv.waves[jj - 1, it - 1] * 0.25e6
                    )
                    kk = kk + 1
                    pf.rfxf[kk - 1] = pfv.rpf[jj - 1]
                    pf.zfxf[kk - 1] = pfv.zpf[jj - 1] - dzpf * 0.125e0
                    pf.cfxf[kk - 1] = (
                        pfv.ric[jj - 1] * pfv.waves[jj - 1, it - 1] * 0.25e6
                    )
                    kk = kk + 1
                    pf.rfxf[kk - 1] = pfv.rpf[jj - 1]
                    pf.zfxf[kk - 1] = pfv.zpf[jj - 1] - dzpf * 0.375e0
                    pf.cfxf[kk - 1] = (
                        pfv.ric[jj - 1] * pfv.waves[jj - 1, it - 1] * 0.25e6
                    )
        
                else:
                    #  Field from different coil
                    kk = kk + 1
                    pf.rfxf[kk - 1] = pfv.rpf[jj - 1]
                    pf.zfxf[kk - 1] = pfv.zpf[jj - 1]
                    pf.cfxf[kk - 1] = (
                        pfv.ric[jj - 1] * pfv.waves[jj - 1, it - 1] * 1.0e6
                    )

        #  Plasma contribution
        if it > 2:
            kk = kk + 1
            pf.rfxf[kk - 1] = pv.rmajor
            pf.zfxf[kk - 1] = 0.0e0
            pf.cfxf[kk - 1] = pv.plascur

        #  Calculate the field at the inner and outer edges
        #  of the coil of interest
        pf.xind[:kk], bri, bzi, psi = pf.bfield(
            pf.rfxf[:kk], pf.zfxf[:kk], pf.cfxf[:kk], pfv.ra[i - 1], pfv.zpf[i - 1], kk
        )
        pf.xind[:kk], bro, bzo, psi = pf.bfield(
            pf.rfxf[:kk], pf.zfxf[:kk], pf.cfxf[:kk], pfv.rb[i - 1], pfv.zpf[i - 1], kk
        )

        #  bpf and bpf2 for the Central Solenoid are calculated in OHCALC
        if (bv.iohcl != 0) and (i == pfv.nohc):
            return bri, bro, bzi, bzo
    
        bpfin = math.sqrt(bri ** 2 + bzi ** 2)
        bpfout = math.sqrt(bro ** 2 + bzo ** 2)
        for n in range(pfv.ncls[ii - 1]):
            pfv.bpf[i - 1 + n] = bpfin
            pf.bpf2[i - 1 + n] = bpfout

        return bri, bro, bzi, bzo
