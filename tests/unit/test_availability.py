"""Unit tests for availability.f90."""
from process import fortran
from process.fortran import cost_variables as cv
from process.fortran import availability_module as am
from process.fortran import tfcoil_variables as tfv
import pytest
from pytest import approx

@pytest.fixture(scope="module", autouse=True)
def reinit_fix():
    """Re-initialise module variables before tests in this module are run.
    
    This is run once before the module's tests are run (module scope), ensuring 
    that all module variables are set to initial values. The individual test 
    functions then use mocking to avoid changing the module variable values.
    autouse ensures that this fixture is used automatically by any test function
    in this module.
    """
    fortran.init_module.init_all_module_vars()

def test_calc_u_unplanned_hcd():
    """Test calc_u_unplanned_hcd."""
    expected = 0.02
    result = fortran.availability_module.calc_u_unplanned_hcd()
    assert result == expected

def test_calc_u_unplanned_bop(monkeypatch):
    """Test calc_u_unplanned_bop.

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    outfile = 0
    iprint = 0

    # Mock module variable t_operation
    monkeypatch.setattr(cv, "t_operation", 25.0)

    # Call subroutine and check result is within an absolute tolerance
    result = fortran.availability_module.calc_u_unplanned_bop(outfile, iprint)
    assert result == approx(0.009, abs=0.0005)

def calc_u_planned_param(**kwargs):
    """Create a dict of parameters for a single calc_u_planned() test.

    :return: Dict of parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'abktflnc': 5.0,
        'adivflnc': 10.0,
        'cpstflnc': 0.0,
        'tlife': 30.0,
        'num_rh_systems': 5,
        'wallmw': 1.0,
        'hldiv': 10.0,
        'itart': 0,
        'expected': approx(0.3, abs=0.05)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def calc_u_planned_params():
    """Create a list of parameter dicts for the calc_u_planned fixture.

    This consists of the nominal and nominal ST cases.

    :return: List of parameter dicts
    :rtype: list
    """
    params = [calc_u_planned_param(), # Nominal
        calc_u_planned_param(abktflnc=20.0, adivflnc=25.0, cpstflnc=20.0,
        tlife=30.0, num_rh_systems=4, wallmw=1.0, hldiv=1.0, itart=1,
        expected=approx(0.03, abs=0.005)) # Nominal ST
        ]

    return params

@pytest.fixture(params=calc_u_planned_params(), ids=["nominal", "ST"])
def calc_u_planned_fix(request, monkeypatch):
    """Fixture for the calc_u_planned() variables.

    This fixture is parameterised using calc_u_planned_params(). This runs the
    fixture for two different parameter sets, testing the nominal and ST cases.
    These two cases are identified using the "ids" list, which determines
    the test ID for the fixture instance with each parameter value.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of calc_u_planned() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock all module variables used by calc_u_planned()
    # Some are parameterised
    monkeypatch.setattr(fortran.divertor_variables, "hldiv", param['hldiv'])
    monkeypatch.setattr(fortran.fwbs_variables, "bktlife", 0.0)
    monkeypatch.setattr(fortran.physics_variables, "wallmw", param['wallmw'])
    monkeypatch.setattr(fortran.physics_variables, "itart", param['itart'])
    monkeypatch.setattr(cv, "tlife", param['tlife'])
    monkeypatch.setattr(cv, "divlife", 0.0)
    monkeypatch.setattr(cv, "adivflnc", param['adivflnc'])
    monkeypatch.setattr(cv, "abktflnc", param['abktflnc'])
    monkeypatch.setattr(cv, "cdrlife", 0.0)
    monkeypatch.setattr(cv, "cplife", 0.0)
    monkeypatch.setattr(cv, "cpstflnc", param['cpstflnc'])
    monkeypatch.setattr(cv, "num_rh_systems", param['num_rh_systems'])

    # Return the expected result for the given parameter list
    return param['expected']

def test_calc_u_planned(calc_u_planned_fix):
    """Test calc_u_planned.

    :param calc_u_planned_fix: Expected value of calc_u_planned()
    :type calc_u_planned_fix: ApproxScalar
    """
    # Arguments
    outfile = 0
    iprint = 0

    # Run calc_u_planned() with the current fixture, then assert the result
    # is the expected one
    result = am.calc_u_planned(outfile, iprint)
    assert result == calc_u_planned_fix

def calc_u_unplanned_magnets_param(**kwargs):
    """Create a parameter dict for the calc_u_unplanned_magnets fixture.

    :return: Parameter dict for the fixture
    :rtype: dict
    """
    defaults = {
        'temp_margin': 1.5,
        'tmargmin_tf': 1.5,
        'tmargmin_cs': 1.5,
        't_operation': 30,
        'conf_mag': 1.0,
        'expected': approx(0.02, abs=0.005)
    }

    # Merge default values with optional keyword args to override them
    param = {**defaults, **kwargs}
    return param

def calc_u_unplanned_magnets_params():
    """Generate list of parameter dicts to parameterise the fixture.

    Parameter dicts for cases:
    no_degradation
    no_degradation_conf
    degradation_conf

    :return: List of parameter dicts
    :rtype: list
    """
    params = [calc_u_unplanned_magnets_param(),
        calc_u_unplanned_magnets_param(temp_margin=2.0, tmargmin_tf=1.6,
        tmargmin_cs=1.6, conf_mag=0.8),
        calc_u_unplanned_magnets_param(temp_margin=1.8, tmargmin_tf=1.6,
        tmargmin_cs=1.6, conf_mag=0.8, expected=approx(0.03, abs=0.005))
        ]

    return params

@pytest.fixture(params=calc_u_unplanned_magnets_params(), ids=['no_degredation',
    'no_degradation_conf', 'degradation_conf'])
def calc_u_unplanned_magnets_fix(request, monkeypatch):
    """Fixture for the calc_u_unplanned_magnets() test.

    :param request: Allows access to parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of calc_u_unplanned_magnets()
    :rtype: ApproxScalar
    """
    param = request.param
    
    # Mock module variables
    monkeypatch.setattr(cv, "t_operation", param['t_operation'])
    monkeypatch.setattr(cv, "conf_mag", param['conf_mag'])
    monkeypatch.setattr(tfv, "tmargmin_cs", param['tmargmin_cs'])
    monkeypatch.setattr(tfv, "tmargmin_tf", param['tmargmin_tf'])
    monkeypatch.setattr(tfv, "temp_margin", param['temp_margin'])

    return param['expected']

def test_calc_u_unplanned_magnets(calc_u_unplanned_magnets_fix):
    """Test function for calc_u_unplanned_magnets().

    :param calc_u_unplanned_magnets_fix: Expected return value
    :type calc_u_unplanned_magnets_fix: ApproxScalar
    """
    outfile = 0
    iprint = 0

    result = am.calc_u_unplanned_magnets(outfile, iprint)
    assert result == calc_u_unplanned_magnets_fix

def calc_u_unplanned_divertor_param(**kwargs):
    """Make parameters for a single calc_u_unplanned_divertor() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'divlife': 1.99,
        'tcycle': 9000,
        'expected': approx(0.02, abs=0.005)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def calc_u_unplanned_divertor_params():
    """Create a list of parameter dicts for the calc_u_planned fixture.
    
    Case 1: below nref
    Case 2: above nu
    Case 3: between
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [calc_u_unplanned_divertor_param(),
        calc_u_unplanned_divertor_param(divlife=4, expected=approx(1, abs=0)),
        calc_u_unplanned_divertor_param(divlife=3, expected=approx(0.1, 
            abs=0.05))
        ]

    return params

@pytest.fixture(params=calc_u_unplanned_divertor_params(), ids=['below_nref', 
    'above_nu', 'between'])
def calc_u_unplanned_divertor_fix(request, monkeypatch):
    """Fixture for the calc_u_unplanned_divertor() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of calc_u_unplanned_divertor() for the 
    parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by calc_u_unplanned_divertor()
    # Some may be parameterised
    monkeypatch.setattr(fortran.times_variables, 'tcycle', param['tcycle'])
    monkeypatch.setattr(cv, 'divlife', param['divlife'])

    # Return the expected result for the given parameter list
    return param['expected']

def test_calc_u_unplanned_divertor(calc_u_unplanned_divertor_fix):
    """Test calc_u_unplanned_divertor.

    :param calc_u_unplanned_divertor_fix: Expected value of 
    calc_u_unplanned_divertor()
    :type calc_u_unplanned_divertor_fix: ApproxScalar
    """
    # Arguments
    outfile = 0
    iprint = 0
    
    # Run calc_u_unplanned_divertor() with the current fixture,
    # then assert the result is the expected one
    result = am.calc_u_unplanned_divertor(outfile, iprint)
    assert result == calc_u_unplanned_divertor_fix

def calc_u_unplanned_fwbs_param(**kwargs):
    """Make parameters for a single calc_u_unplanned_fwbs() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'bktlife': 5,
        'tcycle': 9000,
        'expected': approx(0.02, abs=0.005)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def calc_u_unplanned_fwbs_params():
    """Create a list of parameter dicts for the calc_u_planned fixture.
    
    Case 1: below nref
    Case 2: above nu
    Case 3: between
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [calc_u_unplanned_fwbs_param(),
        calc_u_unplanned_fwbs_param(bktlife=15, expected=approx(1, abs=0)),
        calc_u_unplanned_fwbs_param(bktlife=8.5, expected=approx(0.1, 
            abs=0.005))
        ]

    return params

@pytest.fixture(params=calc_u_unplanned_fwbs_params(), ids=['below_nref', 
    'above_nu', 'between'])
def calc_u_unplanned_fwbs_fix(request, monkeypatch):
    """Fixture for the calc_u_unplanned_fwbs() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of calc_u_unplanned_fwbs() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by calc_u_unplanned_fwbs()
    # Some may be parameterised
    monkeypatch.setattr(fortran.times_variables, 'tcycle', param['tcycle'])
    monkeypatch.setattr(fortran.fwbs_variables, 'bktlife', param['bktlife'])

    # Return the expected result for the given parameter list
    return param['expected']

def test_calc_u_unplanned_fwbs(calc_u_unplanned_fwbs_fix):
    """Test calc_u_unplanned_fwbs.

    :param calc_u_unplanned_fwbs_fix: Expected value of calc_u_unplanned_fwbs()
    :type calc_u_unplanned_fwbs_fix: ApproxScalar
    """
    # Arguments
    outfile = 0
    iprint = 0
    
    # Run calc_u_unplanned_fwbs() with the current fixture,
    # then assert the result is the expected one
    result = am.calc_u_unplanned_fwbs(outfile, iprint)
    assert result == calc_u_unplanned_fwbs_fix