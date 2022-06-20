import pytest
import numpy
from typing import NamedTuple, Any


from process.fortran import cs_fatigue


class NcycleParam(NamedTuple):

    max_hoop_stress: Any = None

    residual_stress: Any = None

    t_crack_vertical: Any = None

    t_structural_vertical: Any = None

    t_structural_radial: Any = None

    n_cycle: Any = None

    t_crack_radial: Any = None

    expected_n_cycle: Any = None

    expected_t_crack_radial: Any = None


@pytest.mark.parametrize(
    "ncycleparam",
    (
        NcycleParam(
            max_hoop_stress=659993518.67335343,
            residual_stress=240000000,
            t_crack_vertical=0.00088999999999999995,
            t_structural_vertical=0.0063104538380405924,
            t_structural_radial=0.0063104538380405924,
            n_cycle=0.0,
            t_crack_radial=0.0060000000000000001,
            expected_n_cycle=1642.1328856852381,
            expected_t_crack_radial=0.0026699999999999996,
        ),
        NcycleParam(
            max_hoop_stress=659999225.25370133,
            residual_stress=240000000,
            t_crack_vertical=0.00088999999999999995,
            t_structural_vertical=0.0063104538380405924,
            t_structural_radial=0.0063104538380405924,
            n_cycle=1642.1328856852381,
            t_crack_radial=0.0026699999999999996,
            expected_n_cycle=1642.0906655770509,
            expected_t_crack_radial=0.0026699999999999996,
        ),
    ),
)
def test_ncycle(ncycleparam, monkeypatch):
    """
    Automatically generated Regression Unit Test for ncycle.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param ncycleparam: the data used to mock and assert in this test.
    :type ncycleparam: ncycleparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    t_crack_radial = numpy.array(ncycleparam.t_crack_radial)
    n_cycle = numpy.array(ncycleparam.n_cycle)

    cs_fatigue.ncycle(
        max_hoop_stress=ncycleparam.max_hoop_stress,
        residual_stress=ncycleparam.residual_stress,
        t_crack_vertical=ncycleparam.t_crack_vertical,
        t_structural_vertical=ncycleparam.t_structural_vertical,
        t_structural_radial=ncycleparam.t_structural_radial,
        n_cycle=n_cycle,
        t_crack_radial=t_crack_radial,
    )

    assert n_cycle == pytest.approx(ncycleparam.expected_n_cycle)

    assert t_crack_radial == pytest.approx(ncycleparam.expected_t_crack_radial)


@pytest.mark.parametrize(
    "hoop_stress, t, w, a, c, phi, expected_k",
    [
        (
            659.99351867335338,
            0.0063104538380405924,
            0.0063104538380405924,
            0.00088999999999999995,
            0.0026699999999999996,
            1.5707963267948966,
            31.96412802853516,
        )
    ],
)
def test_embedded_stress_intensity_factor(hoop_stress, t, w, a, c, phi, expected_k):
    """Tests `he_density` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param expected_density: expected result of the routine.
    :type expected_density: float

    :param tfcoil: fixture containing an initialised `TFcoil` object
    :type tfcoil: tests.unit.test_tfcoil.tfcoil (functional fixture)
    """
    k = cs_fatigue.embedded_stress_intensity_factor(hoop_stress, t, w, a, c, phi)

    assert pytest.approx(k) == expected_k


@pytest.mark.parametrize(
    "hoop_stress, t, w, a, c, phi, expected_k",
    [
        (
            659.99351867335338,
            0.0063104538380405924,
            0.0063104538380405924,
            0.00088999999999999995,
            0.0026699999999999996,
            1.5707963267948966,
            35.744426954844926,
        )
    ],
)
def test_surface_stress_intensity_factor(hoop_stress, t, w, a, c, phi, expected_k):
    """Tests `he_density` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param expected_density: expected result of the routine.
    :type expected_density: float

    :param tfcoil: fixture containing an initialised `TFcoil` object
    :type tfcoil: tests.unit.test_tfcoil.tfcoil (functional fixture)
    """
    k = cs_fatigue.surface_stress_intensity_factor(hoop_stress, t, w, a, c, phi)

    assert pytest.approx(k) == expected_k
