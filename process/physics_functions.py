import logging
import numpy
from process.fortran import constants
from process.fortran import physics_variables
from process.fortran import physics_functions_module
from process.fortran import profiles_module
from process.fortran import impurity_radiation_module

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


class PhysicsFuncs:
    def __init__(self):
        self.outfile = constants.nout

    def radpwr(self):
        """
        Radiation power interface routine
        author: P J Knight, CCFE, Culham Science Centre
        pbrempv    : output real : bremsstrahlung radiation power/volume (MW/m3)
        plinepv    : output real : line radiation power/volume (MW/m3)
        psyncpv    : output real : synchrotron radiation power/volume (MW/m3)
        pcoreradpv : output real : total core radiation power/volume (MW/m3)
        pedgeradpv : output real : edge (non-core) radiation power/volume (MW/m3)
        pradpv     : output real : total radiation power/volume (MW/m3)
        This routine finds the radiation powers in MW/m3 by calling
        relevant routines.
        None
        """
        physics_functions_module.imprad(
            physics_functions_module.pbrempv,
            physics_functions_module.plinepv,
            physics_functions_module.pimpcore,
            physics_functions_module.pimptot,
        )
        pedgeradpv = (
            physics_functions_module.pimptot - physics_functions_module.pimpcore
        )

        #  Synchrotron radiation power/volume; assumed to be from core only

        physics_functions_module.psync_albajar_fidone(physics_functions_module.psyncpv)

        #  Total core radiation power/volume

        pcoreradpv = (
            physics_functions_module.pimpcore + physics_functions_module.psyncpv
        )

        #  Total radiation power/volume

        pradpv = (
            physics_functions_module.pimptot + physics_functions_module.psyncpv
        )  # pcoreradpv + pedgeradpv #

        return pedgeradpv, pcoreradpv, pradpv

    def imprad(self):
        """
        author: H Lux (UKAEA)

        This routine calculates the total radiation losses from impurity line
        radiation and bremsstrahlung for all elements for a given temperature
        and density profile.

        **References**

        - Bremsstrahlung equation from Johner, Fusion Science and Technology 59 (2011), pp 308-349
        - L(z) data (coronal equilibrium) from Marco Sertoli, ASDEX-U, private communication
        - Kallenbach et al., Plasma Phys. Control. Fus. 55 (2013) 124041
        """
        radtot = 0.0e0
        radcore = 0.0e0
        radb = 0.0e0
        radl = 0.0e0

        #  Numerical integration using the midpoint rule
        #  Consider using the maths_library integrator in the future...
        #    quanc8(fun,0.0e0,1.0e0,abserr,relerr,result,errest,nofun,flag)

        npts = 200  # originally 1000; no significant difference found
        drho = 1.0e0 / (npts)

        for i in range(0, npts):

            rho = (0.5e0 + i) / npts

            trho = profiles_module.tprofile(
                rho,
                physics_variables.rhopedt,
                physics_variables.te0,
                physics_variables.teped,
                physics_variables.tesep,
                physics_variables.alphat,
                physics_variables.tbeta,
            )

            nrho = profiles_module.nprofile(
                rho,
                physics_variables.rhopedn,
                physics_variables.ne0,
                physics_variables.neped,
                physics_variables.nesep,
                physics_variables.alphan,
            )

            for imp in range(0, len(impurity_radiation_module.impurity_arr_frac)):

                if impurity_radiation_module.impurity_arr_frac[imp] > 1.0e-30:

                    pimp, pbrem, pline = impurity_radiation_module.impradprofile(
                        imp,
                        nrho,
                        trho,
                    )

                    radtot = radtot + pimp * rho
                    radcore = radcore + pimp * rho * impurity_radiation_module.fradcore(
                        rho,
                        impurity_radiation_module.coreradius,
                        impurity_radiation_module.coreradiationfraction,
                    )
                    radb = radb + pbrem * rho
                    radl = radl + pline * rho

        #  Radiation powers in MW/m3
        radtot = 2.0e-6 * drho * radtot
        radcore = 2.0e-6 * drho * radcore
        radb = 2.0e-6 * drho * radb
        radl = 2.0e-6 * drho * radl

        return radtot, radcore, radb, radl

    def psync_albajar_fidone(self):
        """
        Synchrotron radiation power calculation
        author: P J Knight, CCFE, Culham Science Centre
        author: R Kemp, CCFE, Culham Science Centre
        psyncpv  : output real : synchrotron radiation power/volume (MW/m3)
        This routine finds the synchrotron radiation power in MW/m3,
        using the method of Albajar and Fidone.
        Albajar, Nuclear Fusion 41 (2001) 665
        Fidone, Giruzzi, Granata, Nuclear Fusion 41 (2001) 1755
        """
        tbet = 2.0e0

        #  rpow is the (1-Rsyn) power dependence based on plasma shape
        #  (see Fidone)

        rpow = 0.62e0

        kap = physics_variables.vol / (
            2.0e0
            * numpy.pi**2
            * physics_variables.rmajor
            * physics_variables.rminor**2
        )

        #  No account is taken of pedestal profiles here, other than use of
        #  the correct physics_variables.ne0 and te0...

        de2o = 1.0e-20 * physics_variables.ne0
        pao = 6.04e3 * (physics_variables.rminor * de2o) / physics_variables.bt
        gfun = 0.93e0 * (
            1.0e0
            + 0.85e0
            * numpy.exp(-0.82e0 * physics_variables.rmajor / physics_variables.rminor)
        )
        kfun = (
            physics_variables.alphan + 3.87e0 * physics_variables.alphat + 1.46e0
        ) ** (-0.79e0)
        kfun = kfun * (1.98e0 + physics_variables.alphat) ** 1.36e0 * tbet**2.14e0
        kfun = kfun * (tbet**1.53e0 + 1.87e0 * physics_variables.alphat - 0.16e0) ** (
            -1.33e0
        )
        dum = (
            1.0e0
            + 0.12e0
            * (physics_variables.te0 / (pao**0.41e0))
            * (1.0e0 - physics_variables.ssync) ** 0.41e0
        )

        #  Very high T modification, from Fidone

        dum = dum ** (-1.51e0)

        psync = (
            3.84e-8
            * (1.0e0 - physics_variables.ssync) ** rpow
            * physics_variables.rmajor
            * physics_variables.rminor**1.38e0
        )
        psync = psync * kap**0.79e0 * physics_variables.bt**2.62e0 * de2o**0.38e0
        psync = (
            psync
            * physics_variables.te0
            * (16.0e0 + physics_variables.te0) ** 2.61e0
            * dum
            * gfun
            * kfun
        )

        #  psyncpv should be per unit volume; Albajar gives it as total

        psyncpv = psync / physics_variables.vol

        return psyncpv
