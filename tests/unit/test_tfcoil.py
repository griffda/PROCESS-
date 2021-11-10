"""Unit tests for tfcoil.f90."""

import pytest
import numpy as np

from process.fortran import tfcoil_module as tf


@pytest.mark.parametrize(
    "temperature, expected_density",
    [(24.6, 130.02434313053487), (30.2, 113.09539723009078), (43.6, 85.26924709595201)],
)
def test_he_density(temperature, expected_density):
    """Tests `he_density` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param expected_density: expected result of the routine.
    :type expected_density: float
    """
    density = tf.he_density(temperature)

    assert pytest.approx(density) == expected_density


@pytest.mark.parametrize(
    "temperature, expected_cp",
    [(24.6, 5674.909063980127), (30.2, 5798.42049712345), (43.6, 5673.218322000001)],
)
def test_he_cp(temperature, expected_cp):
    """Tests `he_cp` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param expected_cp: expected result of the routine.
    :type expected_cp: float
    """

    cp = tf.he_cp(temperature)

    assert pytest.approx(cp) == expected_cp


@pytest.mark.parametrize(
    "temperature, expected_visco",
    [
        (20.6, 6.889108080243641e-06),
        (26.2, 6.859929884441028e-06),
        (43.6, 7.717393982e-06),
    ],
)
def test_he_visco(temperature, expected_visco):
    """Tests `he_visco` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param expected_visco: expected result of the routine.
    :type expected_visco: float
    """
    visco = tf.he_visco(temperature)

    assert pytest.approx(visco) == expected_visco


# error module needs to be initialised here because the temperature ranges are (in some cases) greater than the error conditions
@pytest.mark.parametrize(
    "temperature, expected_th_cond",
    [
        (20.6, 0.0585183573711527),
        (24.2, 0.05720100686027678),
        (43.6, 0.061189437089717184),
        (50.6, 0.06409264503),
        (54.4, 0.065706872),
    ],
)
def test_he_th_cond(temperature, expected_th_cond, initialise_error_module):
    """Tests `he_th_cond` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param expected_th_cond: expected result of the routine.
    :type expected_th_cond: float

    :param initialise_error_module: does some default setup for the error handling
    :type initialise_error_module: tests.integration.conftest.initialise_error_module
    """
    th_cond = tf.he_th_cond(temperature)

    assert pytest.approx(th_cond) == expected_th_cond


@pytest.mark.parametrize(
    "temperature, expected_th_cond",
    [
        (54.4, 844.9049012800042),
        (66.9, 571.151543384937),
        (109.5, 233.66333020125035),
        (151, 250.4911087866094),
    ],
)
def test_al_th_cond(temperature, expected_th_cond):
    """Tests `he_th_cond` subroutine.

    :param temperature: test asset passed to the routine representing the temperature, in Kelvin.
    :type temperature: float

    :param al_th_cond: expected result of the routine.
    :type al_th_cond: float
    """
    th_cond = tf.al_th_cond(temperature)

    assert pytest.approx(th_cond) == expected_th_cond
