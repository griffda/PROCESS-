from process.fortran import constants
from process.fortran import cs_fatigue
import numpy


class CsFatigue:
    def __init__(self):
        self.outfile = constants.nout

    def ncycle(
        self,
        max_hoop_stress,
        residual_stress,
        t_crack_vertical,
        t_structural_vertical,
        t_structural_radial,
    ):
        """ """

        C0 = 0.39e-12
        m = 3.5e0
        # Walker Coefficient
        mw = 0.436e0
        n = -m * (mw - 1.0e0)

        # Set units to MPa
        max_hoop_stress_MPa = max_hoop_stress / 1.0e6
        residual_stress_MPa = residual_stress / 1.0e6

        # Set intial crack size
        t_crack_radial = 3.0e0 * t_crack_vertical
        a = t_crack_vertical
        c = t_crack_radial

        # Cyclic element of stress
        hoop_stress_MPa = max_hoop_stress_MPa

        # Mean stress ratio
        # Fatigue Stress Assessment in Fusion Magnet Components
        # J. Lorenzo, X. Sarasola, M. Mantsinen
        R = residual_stress_MPa / (max_hoop_stress_MPa + residual_stress_MPa)

        # Calculated constant for a given stress ratio
        # https://idm.euro-fusion.org/?uid=2MFLQE
        CR = C0 / (1.0e0 - R) ** n

        # select given increase in crack (area or length?)
        delta = 1.0e-4

        # Initialise number of cycles
        n_cycle = 0.0
        N_pulse = 0.0
        Kmax = 0.0

        # factor 2 taken as saftey factors in the crack sizes
        # CS steel undergoes fast fracture when SIF > 200 MPa, under a saftey factor 2 we use 100MPa
        while (
            (a <= t_structural_vertical / 2.0e0)
            and (c <= t_structural_radial / 2.0e0)
            and (Kmax <= 1.0e2)
        ):
            # find SIF max from SIF_a and SIF_c
            Ka = cs_fatigue.surface_stress_intensity_factor(
                hoop_stress_MPa,
                t_structural_vertical,
                t_structural_radial,
                a,
                c,
                numpy.pi / 2.0e0,
            )
            Kc = cs_fatigue.surface_stress_intensity_factor(
                hoop_stress_MPa, t_structural_vertical, t_structural_radial, a, c, 0.0e0
            )
            Kmax = max(Ka, Kc)

            # run euler_method and find number of cycles needed to give crack increase
            deltaN = delta / (CR * (Kmax**m))

            # update a and c, N
            a = a + delta * (Ka / Kmax) ** m
            c = c + delta * (Kc / Kmax) ** m
            N_pulse = N_pulse + deltaN

        # two pulses - ramp to Vsmax and ramp down per cycle
        n_cycle = N_pulse / 2.0e0

        return n_cycle, t_crack_radial
