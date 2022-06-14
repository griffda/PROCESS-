"""Unit tests for the impurity_radiation.f90 module."""
import pytest
from process.fortran import impurity_radiation_module


@pytest.fixture(autouse=True)
def initialise_impurity_radiation():
    impurity_radiation_module.init_impurity_radiation_module()
    impurity_radiation_module.initialise_imprad()


@pytest.mark.parametrize(
    "imp_element_index, ne, te, expected_pbremden",
    [(1, 9.7756695233913373e19, 28.089474310325262, 27122.16187576588)],
)
def test_pbremden(imp_element_index, ne, te, expected_pbremden):
    """Tests `pbremden` function.

    :param imp_element_index: impurity element
    :type imp_element_index: float

    :param ne:  electron density (/m3).
    :type ne: float

    :param te: electron temperature (keV)
    :type te: None

    :param expected_pbremden: Bremsstrahlung radiation density (W/m3)
    :type expected_pbremden: float
    """
    pbremden = impurity_radiation_module.pbremden(imp_element_index, ne, te)

    assert pytest.approx(pbremden) == expected_pbremden


@pytest.mark.parametrize(
    "imp_element_index, ne, te, expected_pimpden",
    [(1, 9.7756695233913373e19, 28.089474310325262, 27565.006789581912)],
)
def test_pimpden(imp_element_index, ne, te, expected_pimpden):
    """Tests `pimpden` function.

    :param imp_element_index: impurity element
    :type imp_element_index: float

    :param ne:  electron density (/m3).
    :type ne: float

    :param te: electron temperature (keV)
    :type te: float

    :param expected_pimpden: Total impurity radiation density (W/m3)
    :type expected_pimpden: float
    """
    pimpden = impurity_radiation_module.pimpden(imp_element_index, ne, te)

    assert pytest.approx(pimpden) == expected_pimpden


@pytest.mark.parametrize(
    "rho, coreradius, coreradiationfraction, expected_fradcore",
    [
        (
            0.0025000000000000001,
            0.75000000000000011,
            0.60000000000000009,
            0.6000000000000001,
        )
    ],
)
def test_fradcore(rho, coreradius, coreradiationfraction, expected_fradcore):
    """Tests `fradcore` function.

    :param rho: normalised minor radius
    :type rho: float

    :param coreradius:  normalised core radius
    :type coreradius: float

    :param coreradiationfraction: fraction of core radiation
    :type coreradiationfraction: float
    :param expected_fradcore: Function to calculate core radiation fraction
    :type expected_fradcore: float
    """
    fradcore = impurity_radiation_module.fradcore(
        rho, coreradius, coreradiationfraction
    )

    assert pytest.approx(fradcore) == expected_fradcore


@pytest.mark.parametrize(
    "imp_element_index, te, expected_zav_of_te",
    [
        (
            3,
            12.330000000000002,
            4.0,
        )
    ],
)
def test_Zav_of_te(imp_element_index, te, expected_zav_of_te):
    """Tests `Zav_of_te` function.

    :param imp_element_index: impurity element
    :type imp_element_index: float

    :param te:  electron temperature (keV)
    :type te: float

    :param expected_zav_of_te: Electron temperature dependent average atomic number
    :type expected_zav_of_te: float
    """
    zav_of_te = impurity_radiation_module.zav_of_te(imp_element_index, te)

    assert pytest.approx(zav_of_te) == expected_zav_of_te


@pytest.mark.parametrize(
    "element_label, expected_element2index",
    [
        (
            "H_",
            1,
        )
    ],
)
def test_element2index(element_label, expected_element2index):
    """Tests `element2index` function.

    :param element_label: impurity name
    :type element_label: string

    :param expected_element2index: Returns the index of the element in the impurity array with a given name
    :type expected_element2index: int
    """
    element2index = impurity_radiation_module.element2index(element_label)

    assert pytest.approx(element2index) == expected_element2index
