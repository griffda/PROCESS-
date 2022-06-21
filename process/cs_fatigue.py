from process.fortran import constants
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
            Ka = self.surface_stress_intensity_factor(
                hoop_stress_MPa,
                t_structural_vertical,
                t_structural_radial,
                a,
                c,
                numpy.pi / 2.0e0,
            )
            Kc = self.surface_stress_intensity_factor(
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

    def embedded_stress_intensity_factor(self, hoop_stress, t, w, a, c, phi):
        # ! Assumes an embedded elliptical efect geometry
        # ! geometric quantities
        # ! hoop_stress - change in hoop stress over cycle
        # ! t - plate thickness
        # ! w - plate width
        # ! a - crack depth (t -direction)
        # ! c - crack length (w - direction)
        # ! Ref: J. Lorenzo, X. Sarasola, M. Mantsinen, Fatigue Stress Assessment
        # ! in Fusion Components Nov 13 2020 https://idm.euro-fusion.org/?uid=2PBDRG
        # ! Ref: C. Jong, Magnet Structural Design
        # ! Criteria Part 1: Main Structural Components and Welds 2012

        if a <= c:
            Q = 1.0e0 + 1.464e0 * (a / c) ** 1.65e0
            m1 = 1.0e0
            m2 = 0.05e0 / (0.11e0 + (a / c) ** (3.0e0 / 2.0e0))
            m3 = 0.29e0 / (0.23e0 + (a / c) ** (3.0e0 / 2.0e0))
            g = 1.0e0 - (
                (a / t) ** 4.0e0
                * numpy.sqrt(2.6e0 - (2.0e0 * a / t))
                / (1.0e0 + 4.0e0 * (a / c))
            ) * abs(numpy.cos(phi))
            f_phi = (
                (a / c) ** 2.0e0 * (numpy.cos(phi)) ** 2.0e0 + (numpy.sin(phi) ** 2.0)
            ) ** (1.0e0 / 4.0e0)
            f_w = numpy.sqrt(
                1.0e0 / numpy.cos(numpy.sqrt(a / t) * numpy.pi * c / (2.0e0 * w))
            )
        elif a > c:
            Q = 1.0e0 + 1.464e0 * (c / a) ** 1.65e0
            m1 = numpy.sqrt(c / a)
            m2 = 0.05e0 / (0.11e0 + (a / c) ** (3.0e0 / 2.0e0))
            m3 = 0.29e0 / (0.23e0 + (a / c) ** (3.0e0 / 2.0e0))
            g = 1.0e0 - (
                (a / t) ** 4.0e0
                * numpy.sqrt(2.6e0 - (2.0e0 * a / t))
                / (1.0e0 + 4.0e0 * (a / c))
            ) * abs(numpy.cos(phi))
            f_phi = (
                (c / a) ** 2.0e0 * (numpy.sin(phi)) ** 2.0e0 + (numpy.cos(phi) ** 2.0e0)
            ) ** (1.0e0 / 4.0e0)
            f_w = numpy.sqrt(
                1.0e0 / numpy.cos(numpy.sqrt(a / t) * numpy.pi * c / (2.0e0 * w))
            )

        # compute the unitless geometric correction
        f = (m1 + m2 * (a / t) ** 2.0e0 + m3 * (a / t) ** 4.0e0) * g * f_phi * f_w

        # compute the stress intensity factor
        k = hoop_stress * f * numpy.sqrt(numpy.pi * a / Q)
        return k

    def surface_stress_intensity_factor(self, hoop_stress, t, w, a, c, phi):
        # ! Assumes an surface semi elliptical defect geometry
        # ! geometric quantities
        # ! hoop_stress - change in hoop stress over cycle
        # ! t - plate thickness
        # ! w - plate width
        # ! a - crack depth (t -direction)
        # ! c - crack length (w - direction)
        # ! Ref: J. Lorenzo, X. Sarasola, M. Mantsinen, Fatigue Stress Assessment
        # ! in Fusion Components Nov 13 2020 https://idm.euro-fusion.org/?uid=2PBDRG
        # ! Ref: C. Jong, Magnet Structural Design
        # ! Criteria Part 1: Main Structural Components and Welds 2012

        bending_stress = 0.0e0  # * 3.0 * M / (w*d**2.0)

        if a <= c:
            Q = 1.0e0 + 1.464e0 * (a / c) ** 1.65e0
            m1 = 1.13e0 - 0.09e0 * a / c
            m2 = -0.54e0 + 0.89e0 / (0.2e0 + a / c)
            m3 = 0.5e0 - 1.0e0 / (0.65e0 + a / c) + 14.0e0 * (1 - a / c) ** 24.0e0
            g = (
                1.0e0
                + (0.1e0 + 0.35e0 * (a / c) ** 2.0e0)
                * (1.0e0 - numpy.sin(phi)) ** 2.0e0
            )
            f_phi = (
                (a / c) ** 2.0e0 * (numpy.cos(phi)) ** 2.0e0 + (numpy.sin(phi)) ** 2.0e0
            ) ** (1.0e0 / 4.0e0)
            f_w = numpy.sqrt(
                1.0e0 / numpy.cos(numpy.sqrt(a / t) * numpy.pi * c / (2.0e0 * w))
            )
            p = 0.2e0 + a / c + 0.6e0 * a / t
            G21 = -1.22e0 - 0.12e0 * a / c
            G22 = 0.55e0 - 1.05e0 * (a / c) ** 0.75e0 + 0.47e0 * (a / c) ** 1.5e0
            H1 = 1.0e0 - 0.34e0 * a / t - 0.11e0 * a * a / (c * t)
            H2 = 1.0e0 + G21 * a / t + G22 * (a / t) ** 2.0e0
        elif a > c:
            Q = 1.0e0 + 1.464e0 * (c / a) ** 1.65e0
            m1 = numpy.sqrt(c / a) * (1.0e0 + 0.04e0 * c / a)
            m2 = 0.2e0 * (c / a) ** 4.0e0
            m3 = -0.11e0 * (c / a) ** 4.0e0
            g = (
                1.0e0
                + (0.1e0 + 0.35e0 * (c / a) * (a / t) ** 2.0e0)
                * (1.0e0 - numpy.sin(phi)) ** 2.0e0
            )
            f_phi = (
                (c / a) ** 2.0e0 * (numpy.sin(phi)) ** 2.0e0 + (numpy.cos(phi)) ** 2.0e0
            ) ** (1.0e0 / 4.0e0)
            f_w = numpy.sqrt(
                1.0e0 / numpy.cos(numpy.sqrt(a / t) * numpy.pi * c / (2.0e0 * w))
            )
            p = 0.2e0 + c / a + 0.6e0 * a / t
            G11 = -0.04e0 - 0.41e0 * c / a
            G12 = 0.55e0 - 1.93e0 * (c / a) ** 0.75e0 + 1.38e0 * (c / a) ** 1.5e0
            G21 = -2.11e0 + 0.77e0 * c / a
            G22 = 0.55e0 - 0.72e0 * (c / a) * 0.75e0 + 0.14e0 * (c / a) * 1.5e0
            H1 = 1.0e0 + G11 * a / t + G12 * (a / t) ** 2.0e0
            H2 = 1.0e0 + G21 * a / t + G22 * (a / t) ** 2.0e0

        # compute the unitless geometric correction
        Hs = H1 + (H2 - H1) * (numpy.sin(phi)) ** p
        f = (m1 + m2 * (a / t) ** 2.0e0 + m3 * (a / t) ** 4.0e0) * g * f_phi * f_w

        # compute the stress intensity factor
        k = (hoop_stress + Hs * bending_stress) * f * numpy.sqrt(numpy.pi * a / Q)

        return k
