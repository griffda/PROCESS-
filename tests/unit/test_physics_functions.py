"""Unit tests for physics_functions.f90."""
from process.fortran import physics_functions_module as pf
from process.fortran import physics_variables as pv
from process.fortran import impurity_radiation_module
from process.fortran import physics_variables
from process.fortran import physics_functions_module
import pytest
from pytest import approx


def t_eped_scaling_param(**kwargs):
    """Make parameters for a single t_eped_scaling() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        "triang": 0.4,
        "kappa": 1.5,
        "rmajor": 7.0,
        "rminor": 2.0,
        "plascur": 10e6,
        "normalised_total_beta": 2.0,
        "eped_sf": 1.0,
        "expected": approx(1.7134, abs=0.001),
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param


def t_eped_scaling_params():
    """Create a list of parameter dicts for the calc_u_planned fixture.

    Case 1: low
    Case 2: high

    :return: List of parameter dicts
    :rtype: list
    """
    params = [
        t_eped_scaling_param(),
        t_eped_scaling_param(
            triang=0.6,
            kappa=2,
            rmajor=11.0,
            rminor=3.5,
            plascur=20e6,
            normalised_total_beta=3.0,
            expected=approx(5.3955, abs=0.001),
        ),
    ]

    return params


@pytest.fixture(params=t_eped_scaling_params(), ids=["low", "high"])
def t_eped_scaling_fix(request, monkeypatch):
    """Fixture for the t_eped_scaling() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of t_eped_scaling() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by t_eped_scaling()
    # Some may be parameterised
    monkeypatch.setattr(pv, "triang", param["triang"])
    monkeypatch.setattr(pv, "kappa", param["kappa"])
    monkeypatch.setattr(pv, "rmajor", param["rmajor"])
    monkeypatch.setattr(pv, "rminor", param["rminor"])
    monkeypatch.setattr(pv, "plascur", param["plascur"])
    monkeypatch.setattr(pv, "normalised_total_beta", param["normalised_total_beta"])
    monkeypatch.setattr(pv, "eped_sf", param["eped_sf"])

    # Return the expected result for the given parameter list
    return param["expected"]


def test_t_eped_scaling(t_eped_scaling_fix):
    """Test t_eped_scaling.

    :param t_eped_scaling_fix: Expected value of t_eped_scaling()
    :type t_eped_scaling_fix: ApproxScalar
    """
    # Run t_eped_scaling() with the current fixture,
    # then assert the result is the expected one
    result = pf.t_eped_scaling()
    assert result == t_eped_scaling_fix


def test_plasma_elongation_ipb(monkeypatch):
    """Test plasma_elongation_IPB().
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(pv, "rmajor", 9.137)
    monkeypatch.setattr(pv, "rminor", 2.947)
    monkeypatch.setattr(pv, "vol", 2634.0)
    kappaa_ipb = pf.plasma_elongation_ipb()
    assert kappaa_ipb == approx(1.682, abs=0.001)


def test_total_mag_field(monkeypatch):
    """Test total_mag_field().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(pv, "bt", 5.278)
    monkeypatch.setattr(pv, "bp", 0.852)
    btot = pf.total_mag_field()
    assert btot == approx(5.347, abs=0.001)


def test_beta_poloidal(monkeypatch):
    """Test beta_poloidal().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(pv, "btot", 5.347)
    monkeypatch.setattr(pv, "beta", 0.0307)
    monkeypatch.setattr(pv, "bp", 0.852)
    betap = pf.beta_poloidal()
    assert betap == approx(1.209, abs=0.001)


def test_res_diff_time(monkeypatch):
    """Test res_diff_time().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(pv, "rmajor", 9.137)
    monkeypatch.setattr(pv, "rplas", 2.909e-9)
    monkeypatch.setattr(pv, "kappa95", 1.650)
    res_time = pf.res_diff_time()
    assert res_time == approx(4784.3, abs=0.1)


def test_imprad(monkeypatch):

    """Inputs taken from Hanni Lux's test_imprad from old impurityradiation
    Outputs set to reflect this.

    Unit test for the "Imprad" subroutine.

    """
    impurity_radiation_module.init_impurity_radiation_module()
    impurity_radiation_module.initialise_imprad()

    current_impurity_array = impurity_radiation_module.impurity_arr_frac
    current_impurity_array[1] = 1.10601e-1  # Helium
    current_impurity_array[8] = 5.1416e-3  # Argon

    monkeypatch.setattr(
        impurity_radiation_module, "impurity_arr_frac", current_impurity_array
    )
    monkeypatch.setattr(physics_variables, "rhopedt", 0.925e0)
    monkeypatch.setattr(physics_variables, "rhopedn", 0.925e0)
    monkeypatch.setattr(physics_variables, "te0", 40.0e0)
    monkeypatch.setattr(physics_variables, "teped", 3.2494e0)
    monkeypatch.setattr(physics_variables, "ne0", 1.03551089599055266e020)
    monkeypatch.setattr(physics_variables, "neped", 9.39747e19)
    monkeypatch.setattr(physics_variables, "alphat", 1.37e0)
    monkeypatch.setattr(physics_variables, "tbeta", 2.0e0)
    monkeypatch.setattr(physics_variables, "alphan", 1.0e0)
    monkeypatch.setattr(physics_variables, "tesep", 0.1e0)
    monkeypatch.setattr(physics_variables, "nesep", 3.6e19)
    monkeypatch.setattr(impurity_radiation_module, "coreradiationfraction", 1.0)

    radb, radl, radcore, radtot = physics_functions_module.imprad()

    assert radb == pytest.approx(0.05950642693264711)

    assert radl == pytest.approx(0.05552120648487812)

    assert radcore == pytest.approx(0.04593865332065909)

    assert radtot == pytest.approx(0.1150276334175252)
