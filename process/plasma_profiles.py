import logging
import numpy as np

from process.fortran import (
    constants,
    divertor_variables,
    maths_library,
    physics_variables,
)

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


NRHO = 501


def plasma_profiles():
    """Calculates density and temperature profile quantities
    author: P J Knight, CCFE, Culham Science Centre

    This subroutine initialises the density and temperature
    profile averages and peak values, given the main
    parameters describing these profiles.
    T&amp;M/PKNIGHT/LOGBOOK24, pp.4-7
    """

    arg1 = np.zeros((NRHO,))
    arg2 = np.zeros((NRHO,))
    arg3 = np.zeros((NRHO,))

    #  Volume-averaged ion temperature
    #  (input value used directly if tratio=0.0)

    if physics_variables.tratio > 0.0e0:
        physics_variables.ti = physics_variables.tratio * physics_variables.te

    if physics_variables.ipedestal == 0:

        #  Reset pedestal values to agree with original parabolic profiles

        physics_variables.rhopedt = 1.0e0
        physics_variables.rhopedn = 1.0e0
        physics_variables.teped = 0.0e0
        physics_variables.tesep = 0.0e0
        physics_variables.neped = 0.0e0
        physics_variables.nesep = 0.0e0
        physics_variables.tbeta = 2.0e0

        #  Profile factor; ratio of density-weighted to volume-averaged
        #  temperature

        physics_variables.pcoef = (
            (1.0e0 + physics_variables.alphan)
            * (1.0e0 + physics_variables.alphat)
            / (1.0e0 + physics_variables.alphan + physics_variables.alphat)
        )

        #  Line averaged electron density (IPDG89)
        #  0.5*gamfun(0.5) = 0.5*sqrt(pi) = 0.886227

        physics_variables.dnla = (
            physics_variables.dene
            * (1.0 + physics_variables.alphan)
            * 0.886227
            * maths_library.gamfun(physics_variables.alphan + 1.0)
            / maths_library.gamfun(physics_variables.alphan + 1.5e0)
        )

        #  Density-weighted temperatures

        physics_variables.ten = physics_variables.te * physics_variables.pcoef
        physics_variables.tin = physics_variables.ti * physics_variables.pcoef

        #  Central values for temperature (keV) and density (m**-3)

        physics_variables.te0 = physics_variables.te * (1.0 + physics_variables.alphat)
        physics_variables.ti0 = physics_variables.ti * (1.0 + physics_variables.alphat)

        physics_variables.ne0 = physics_variables.dene * (
            1.0 + physics_variables.alphan
        )
        physics_variables.ni0 = physics_variables.dnitot * (
            1.0 + physics_variables.alphan
        )

    else:

        #  The following reproduces the above results within sensible
        #  tolerances if rhopedt = rhopedn = 1.0, teped = tesep = neped
        #  = nesep = 0.0, and tbeta = 2.0

        #  Central values for temperature (keV) and density (m**-3)

        physics_variables.te0 = tcore(
            physics_variables.rhopedt,
            physics_variables.teped,
            physics_variables.tesep,
            physics_variables.te,
            physics_variables.alphat,
            physics_variables.tbeta,
        )
        physics_variables.ti0 = (
            physics_variables.ti / physics_variables.te * physics_variables.te0
        )

        physics_variables.ne0 = ncore(
            physics_variables.rhopedn,
            physics_variables.neped,
            physics_variables.nesep,
            physics_variables.dene,
            physics_variables.alphan,
        )
        physics_variables.ni0 = (
            physics_variables.dnitot / physics_variables.dene * physics_variables.ne0
        )

        #  Perform integrations to calculate ratio of density-weighted
        #  to volume-averaged temperature, etc.
        #  Density-weighted temperature = integral(n.T dV) / integral(n dV)
        #  which is approximately equal to the ratio
        #  integral(rho.n(rho).T(rho) drho) / integral(rho.n(rho) drho)

        drho = 1.0 / (NRHO - 1)

        for irho in range(NRHO):
            rho = irho / (NRHO - 1)
            dens = nprofile(
                rho,
                physics_variables.rhopedn,
                physics_variables.ne0,
                physics_variables.neped,
                physics_variables.nesep,
                physics_variables.alphan,
            )
            temp = tprofile(
                rho,
                physics_variables.rhopedt,
                physics_variables.te0,
                physics_variables.teped,
                physics_variables.tesep,
                physics_variables.alphat,
                physics_variables.tbeta,
            )
            arg1[irho] = rho * dens * temp
            arg2[irho] = rho * dens
            arg3[irho] = dens

        integ1 = maths_library.sumup3(drho, arg1, NRHO)
        integ2 = maths_library.sumup3(drho, arg2, NRHO)

        #  Density-weighted temperatures

        physics_variables.ten = integ1 / integ2
        physics_variables.tin = (
            physics_variables.ti / physics_variables.te * physics_variables.ten
        )

        #  Profile factor; ratio of density-weighted to volume-averaged
        #  temperature

        physics_variables.pcoef = physics_variables.ten / physics_variables.te

        #  Line-averaged electron density
        #  = integral(n(rho).drho)

        physics_variables.dnla = maths_library.sumup3(drho, arg3, NRHO)

        #  Scrape-off density / volume averaged density
        #  (Input value is used if ipedestal = 0)

        divertor_variables.prn1 = max(
            0.01e0, physics_variables.nesep / physics_variables.dene
        )  # preventing division by zero later

    #  Central pressure (Pa), from ideal gas law : p = nkT

    physics_variables.p0 = (
        (
            physics_variables.ne0 * physics_variables.te0
            + physics_variables.ni0 * physics_variables.ti0
        )
        * 1.0e3
        * constants.echarge
    )

    #  Pressure profile index (N.B. no pedestal effects included here)
    #  N.B. p0 is NOT equal to <p> * (1 + alphap), but p(rho) = n(rho)*T(rho)
    #  and <p> = <n>.T_n where <...> denotes volume-averages and T_n is the
    #  density-weighted temperature

    physics_variables.alphap = physics_variables.alphan + physics_variables.alphat

    # The gradient information for ipedestal = 0:
    # All formulas can be obtained from the analytical parametric form of the ipedestal profiles
    # rho_max is obtained by equalling the second derivative to zero e.g.

    if physics_variables.ipedestal == 0:
        if physics_variables.alphat > 1.0:
            # Rho (normalized radius), where temperature derivative is largest
            rho_te_max = 1.0 / np.sqrt(-1.0 + 2.0 * physics_variables.alphat)
            dtdrho_max = (
                -(2.0**physics_variables.alphat)
                * (-1.0 + physics_variables.alphat) ** (-1.0 + physics_variables.alphat)
                * physics_variables.alphat
                * (-1.0 + 2.0 * physics_variables.alphat)
                ** (0.5e0 - physics_variables.alphat)
                * physics_variables.te0
            )
            te_max = (
                physics_variables.te0
                * (1 - rho_te_max**2) ** physics_variables.alphat
            )
        elif physics_variables.alphat <= 1.0 and physics_variables.alphat > 0.0:
            # This makes the profiles very 'boxy'
            # The gradient diverges here at the edge so define some 'wrong' value of 0.9
            # to approximate the gradient
            rho_te_max = 0.9
            dtdrho_max = (
                -2.0
                * physics_variables.alphat
                * rho_te_max
                * (1 - rho_te_max**2) ** (-1.0 + physics_variables.alphat)
                * physics_variables.te0
            )
            te_max = (
                physics_variables.te0
                * (1 - rho_te_max**2) ** physics_variables.alphat
            )
        else:
            raise ValueError(f"alphat is negative: { physics_variables.alphat}")

        # Same for density
        if physics_variables.alphan > 1.0:
            rho_ne_max = 1.0 / np.sqrt(-1.0 + 2.0 * physics_variables.alphan)
            dndrho_max = (
                -(2.0**physics_variables.alphan)
                * (-1.0 + physics_variables.alphan) ** (-1.0 + physics_variables.alphan)
                * physics_variables.alphan
                * (-1.0 + 2.0 * physics_variables.alphan)
                ** (0.5 - physics_variables.alphan)
                * physics_variables.ne0
            )
            ne_max = (
                physics_variables.ne0
                * (1e0 - rho_ne_max**2) ** physics_variables.alphan
            )
        elif physics_variables.alphan <= 1.0 and physics_variables.alphan > 0.0:
            # This makes the profiles very 'boxy'
            # The gradient diverges here at the edge so define some 'wrong' value of 0.9
            # to approximate the gradient
            rho_ne_max = 0.9
            dndrho_max = (
                -2.0
                * physics_variables.alphan
                * rho_ne_max
                * (1 - rho_ne_max**2) ** (-1.0 + physics_variables.alphan)
                * physics_variables.ne0
            )
            ne_max = (
                physics_variables.ne0
                * (1 - rho_ne_max**2) ** physics_variables.alphan
            )
        else:
            raise Exception(f"alphan is negative: { physics_variables.alphan}")

        # set normalized gradient length
        # te at rho_te_max
        physics_variables.gradient_length_te = (
            -dtdrho_max * physics_variables.rminor * rho_te_max / te_max
        )
        # same for density:
        physics_variables.gradient_length_ne = (
            -dndrho_max * physics_variables.rminor * rho_ne_max / ne_max
        )


def tcore(rhopedt, tped, tsep, tav, alphat, tbeta):
    """Central temperature for a pedestal profile
    author: R Kemp, CCFE, Culham Science Centre
    author: H Lux, CCFE, Culham Science Centre
    author: P J Knight, CCFE, Culham Science Centre
    rhopedt : input real : normalised minor radius pedestal position
    tped : input real : pedestal temperature (keV)
    tsep : input real : separatrix temperature (keV)
    tav : input real : volume average temperature (keV)
    alphat : input real : temperature peaking parameter
    tbeta : input real : second temperature exponent
    This routine calculates the core temperature (keV)
    of a pedestalised profile.
    J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    """
    #  For integer values of alphat, the limit of
    #  gamfun(-alphat)*sin(pi*alphat) needs to be calculated directly

    gamfac = (
        maths_library.gamfun(1 + alphat + 2 / tbeta)
        / maths_library.gamfun((2 + tbeta) / tbeta)
        / rhopedt**2
    )

    if abs(alphat - np.around(alphat)) <= 1e-7:
        gamfac = -gamfac / maths_library.gamfun(1 + alphat)
    else:
        gamfac = gamfac * maths_library.gamfun(-alphat) * np.sin(np.pi * alphat) / np.pi

    #  Calculate core temperature

    return tped + gamfac * (
        tped * rhopedt**2
        - tav
        + (1 - rhopedt) / 3 * ((1 + 2 * rhopedt) * tped + (2 + rhopedt) * tsep)
    )


def ncore(rhopedn, nped, nsep, nav, alphan):
    ncore = (
        1
        / (3 * rhopedn**2)
        * (
            3 * nav * (1 + alphan)
            + nsep * (1 + alphan) * (-2 + rhopedn + rhopedn**2)
            - nped * ((1 + alphan) * (1 + rhopedn) + (alphan - 2) * rhopedn**2)
        )
    )

    if ncore < 0:
        raise Exception(
            f"Core density is negative: {ncore}. {nped = }, {nsep = }, {nav = }"
        )
    return ncore


def tprofile(rho, rhopedt, t0, tped, tsep, alphat, tbeta):
    """Implementation of HELIOS-type temperature pedestal profile
    author: R Kemp, CCFE, Culham Science Centre
    author: H Lux, CCFE, Culham Science Centre
    author: P J Knight, CCFE, Culham Science Centre
    rho     : input real : normalised minor radius
    rhopedt : input real : normalised minor radius pedestal position
    t0      : input real : central temperature (keV)
    tped    : input real : pedestal temperature (keV)
    tsep    : input real : separatrix temperature (keV)
    alphat  : input real : temperature peaking parameter
    tbeta   : input real : second temperature exponent
    trho    : output real : T(rho) (keV)
    This routine calculates the temperature at a normalised minor
    radius position rho for a pedestalised profile.
    <P>If <CODE>ipedestal = 0</CODE> the original parabolic
    profile form is used instead.
    J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    """
    if physics_variables.ipedestal == 0:
        return t0 * (1 - rho**2) ** alphat

    #  Error trap; shouldn't happen unless volume-averaged temperature has
    #  been allowed to drop below tped. This may happen during a HYBRD case,
    #  but should have been prevented for optimisation runs.

    if t0 < tped:
        logger.warning(
            f"TPROFILE: temperature pedestal is higher than core temperature. {tped = }, {t0 = }"
        )

    if rho <= rhopedt:
        return tped + (t0 - tped) * (1 - (rho / rhopedt) ** tbeta) ** alphat

    return tsep + (tped - tsep) * (1 - rho) / (1 - rhopedt)


def nprofile(rho, rhopedn, n0, nped, nsep, alphan):
    """Implementation of HELIOS-type density pedestal profile
    author: R Kemp, CCFE, Culham Science Centre
    author: H Lux, CCFE, Culham Science Centre
    author: P J Knight, CCFE, Culham Science Centre
    rho     : input real : normalised minor radius
    rhopedn : input real : normalised minor radius pedestal position
    n0      : input real : central density (/m3)
    nped    : input real : pedestal density (/m3)
    nsep    : input real : separatrix density (/m3)
    alphan  : input real : density peaking parameter
    This routine calculates the density at a normalised minor
    radius position rho for a pedestalised profile.
    <P>If <CODE>ipedestal = 0</CODE> the original parabolic
    profile form is used instead.
    J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    """
    if physics_variables.ipedestal == 0:
        return n0 * (1 - rho**2) ** alphan

    #  Error trap; shouldn't happen unless volume-averaged density has
    #  been allowed to drop below nped. This may happen during a HYBRD case,
    #  but should have been prevented for optimisation runs.

    #  Input checks

    if n0 < nped:
        logger.warning(
            f"NPROFILE: density pedestal is higher than core density. {nped = }, {n0 = }"
        )

    if rho <= rhopedn:
        return nped + (n0 - nped) * (1 - (rho / rhopedn) ** 2) ** alphan

    return nsep + (nped - nsep) * (1 - rho) / (1 - rhopedn)
