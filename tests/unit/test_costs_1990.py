"""Unit tests for costs.f90."""
from process.fortran import costs_module as cm
from process.fortran import cost_variables as cv
from process.fortran import fwbs_variables as fv
from process.fortran import heat_transport_variables as htv
from process.fortran import ife_variables as ifev
from process import fortran
import pytest
from pytest import approx

def acc2261_param(**kwargs):
    """Make parameters for a single acc2261() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'coolwh': 1,
        'expected': approx(49.68, abs=0.01)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc2261_params():
    """Create a list of parameter dicts for the acc2261 fixture.
    
    Case 1: Reactor cooling system (He)
    Case 2: Reactor cooling system (H2O)
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc2261_param(),
        acc2261_param(coolwh=2, expected=approx(53.85, abs=0.01))
        ]

    return params

@pytest.fixture(params=acc2261_params(), ids=['he', 'h2o'])
def acc2261_fix(request, monkeypatch):
    """Fixture for the acc2261() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc2261() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc2261()
    monkeypatch.setattr(cv, 'fkind', 1)
    monkeypatch.setattr(cv, 'lsa', 1)
    monkeypatch.setattr(htv, 'pfwdiv', 0.0)
    monkeypatch.setattr(fv, 'pnucblkt', 1558.0)
    monkeypatch.setattr(fv, 'pnucshld', 1.478)
    monkeypatch.setattr(htv, 'pthermmw', 2647.0)
    monkeypatch.setattr(htv, 'nphx', 3)
    monkeypatch.setattr(cm, 'c2261', 0)

    # Parameterised mocks
    monkeypatch.setattr(fv, 'coolwh', param['coolwh'])

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc2261(acc2261_fix):
    """Test acc2261.

    :param acc2261_fix: Expected value of acc2261()
    :type acc2261_fix: ApproxScalar
    """
    # Run acc2261() with the current fixture,
    # then assert the result (c2261) is the expected one
    cm.acc2261()
    assert cm.c2261 == acc2261_fix

def test_acc2262(monkeypatch):
    """Test acc2262()."""
    # Mock module variables
    monkeypatch.setattr(cv, "fkind", 1)
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(htv, "pinjht", 76.5)
    monkeypatch.setattr(htv, "crypmw", 39.936)
    monkeypatch.setattr(htv, "vachtmw", 0.5)
    monkeypatch.setattr(htv, "trithtmw", 15.0)
    monkeypatch.setattr(htv, "fachtmw", 64.835)
    monkeypatch.setattr(cm, "c2262", 0)

    cm.acc2262()
    assert cm.c2262 == approx(29.408, abs=0.01)

def test_acc2263(monkeypatch):
    """Test acc2263().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "fkind", 1)
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(cv, "uccry", 9.3e4)
    monkeypatch.setattr(fortran.tfcoil_variables, "tftmp", 4.5)
    monkeypatch.setattr(htv, "helpow", 80.980e3)
    monkeypatch.setattr(cm, "c2263", 0)

    cm.acc2263()
    assert cm.c2263 == approx(180.76, abs=0.01)

def test_acc2271(monkeypatch):
    """Test acc2271().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "ucf1", 2.23e7)
    monkeypatch.setattr(cv, "fkind", 1)
    monkeypatch.setattr(cm, "c2271", 0)

    cm.acc2271()
    assert cm.c2271 == approx(22.3, abs=0.01)

def test_acc2272(monkeypatch):
    """Test acc2272().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.physics_variables, "rndfuel", 7.158e20)
    monkeypatch.setattr(fortran.physics_variables, "afuel", 2.5)
    monkeypatch.setattr(cv, "fkind", 1)
    monkeypatch.setattr(cm, "c2271", 0)

    cm.acc2272()
    assert cm.c2272 == approx(114.707, abs=0.01)

def acc2273_param(**kwargs):
    """Make parameters for a single acc2273() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'ftrit': 0.0001,
        'volrci': fortran.buildings_variables.volrci,
        'wsvol': fortran.buildings_variables.wsvol,
        'expected': approx(0.0, abs=0.00001)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc2273_params():
    """Create a list of parameter dicts for the acc2273 fixture.
    
    Case 1: ttrit low
    Case 2: ttrit high
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc2273_param(),
        acc2273_param(ftrit=0.5, volrci=1299783.4, wsvol=132304.1,
        expected=approx(74.12, abs=0.01))
        ]

    return params

@pytest.fixture(params=acc2273_params(), ids=['ttrit_low', 'ttrit_high'])
def acc2273_fix(request, monkeypatch):
    """Fixture for the acc2273() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc2273() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc2273()
    # Some may be parameterised
    monkeypatch.setattr(fortran.buildings_variables, 'wsvol', param['wsvol'])
    monkeypatch.setattr(fortran.buildings_variables, 'volrci', param['volrci'])
    monkeypatch.setattr(fortran.physics_variables, 'ftrit', param['ftrit'])
    
    # Mock result var as negative, as an expected result is 0
    # Otherwise could get false positive result
    monkeypatch.setattr(cm, 'c2273', -1)

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc2273(acc2273_fix):
    """Test acc2273.

    :param acc2273_fix: Expected value of acc2273()
    :type acc2273_fix: ApproxScalar
    """
    # Run acc2273() with the current fixture,
    # then assert the result is the expected one
    cm.acc2273()
    assert cm.c2273 == acc2273_fix

def test_acc2274(monkeypatch):
    """Test acc2274().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.buildings_variables, "wsvol", 132304.1)
    monkeypatch.setattr(fortran.buildings_variables, "volrci", 1299783.4)
    monkeypatch.setattr(cv, "fkind", 1)
    monkeypatch.setattr(cm, "c2274", 0)

    cm.acc2274()
    assert cm.c2274 == approx(84.10, abs=0.01)

def acc228_param(**kwargs):
    """Make parameters for a single acc228() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'fkind': 1.0,
        'expected': approx(150.0, abs=0.01)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc228_params():
    """Create a list of parameter dicts for the acc228 fixture.
    
    Case 1: fkind 1
    Case 2: fkind 0.5
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc228_param(),
        acc228_param(fkind=0.5, expected=approx(75.0, abs=0.01))
        ]

    return params

@pytest.fixture(params=acc228_params(), ids=['fkind_1', 'fkind_0p5'])
def acc228_fix(request, monkeypatch):
    """Fixture for the acc228() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc228() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc228()
    # Some may be parameterised
    monkeypatch.setattr(cv, 'fkind', param['fkind'])
    monkeypatch.setattr(cm, 'c228', 0)

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc228(acc228_fix):
    """Test acc228.

    :param acc228_fix: Expected value of acc228()
    :type acc228_fix: ApproxScalar
    """
    # Run acc228() with the current fixture,
    # then assert the result is the expected one
    cm.acc228()
    assert cm.c228 == acc228_fix

def acc229_param(**kwargs):
    """Make parameters for a single acc229() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'fkind': 1.0,
        'expected': approx(125.0, abs=0.01)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc229_params():
    """Create a list of parameter dicts for the acc229 fixture.
    
    Case 1: fkind 1
    Case 2: fkind 0.5
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc229_param(),
        acc229_param(fkind=0.5, expected=approx(62.5, abs=0.01))
        ]

    return params

@pytest.fixture(params=acc229_params(), ids=['fkind_1', 'fkind_0p5'])
def acc229_fix(request, monkeypatch):
    """Fixture for the acc229() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc229() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc229()
    # Some may be parameterised
    monkeypatch.setattr(cv, 'fkind', param['fkind'])
    monkeypatch.setattr(cm, 'c229', 0)

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc229(acc229_fix):
    """Test acc229.

    :param acc229_fix: Expected value of acc229()
    :type acc229_fix: ApproxScalar
    """
    # Run acc229() with the current fixture,
    # then assert the result is the expected one
    cm.acc229()
    assert cm.c229 == acc229_fix

def acc23_param(**kwargs):
    """Make parameters for a single acc23() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'coolwh': 1,
        'expected': approx(230, abs=0.01)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc23_params():
    """Create a list of parameter dicts for the acc23 fixture.
    
    Case 1: he coolant
    Case 2: h2o coolant
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc23_param(),
        acc23_param(coolwh=2, expected=approx(245, abs=0.01))
        ]

    return params

@pytest.fixture(params=acc23_params(), ids=['he', 'h2o'])
def acc23_fix(request, monkeypatch):
    """Fixture for the acc23() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc23() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc23()
    # Some may be parameterised
    monkeypatch.setattr(fv, 'coolwh', param['coolwh'])
    monkeypatch.setattr(htv, 'pgrossmw', 1200.0)
    monkeypatch.setattr(cm, 'c23', 0)

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc23(acc23_fix):
    """Test acc23.

    :param acc23_fix: Expected value of acc23()
    :type acc23_fix: ApproxScalar
    """
    # Run acc23() with the current fixture,
    # then assert the result is the expected one
    cm.acc23()
    assert cm.c23 == acc23_fix

def test_acc241(monkeypatch):
    """Test acc241().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(cm, "c241", 0)

    cm.acc241()
    assert cm.c241 == approx(18.4, abs=0.01)

def test_acc242(monkeypatch):
    """Test acc242().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(htv, "pacpmw", 630.0)
    monkeypatch.setattr(htv, "fcsht", 65.0)
    monkeypatch.setattr(cm, "c242", 0)

    cm.acc242()
    assert cm.c242 == approx(9.06, abs=0.01)

def test_acc243(monkeypatch):
    """Test acc243().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(htv, "tlvpmw", 403.8)
    monkeypatch.setattr(cm, "c243", 0)

    cm.acc243()
    assert cm.c243 == approx(8.08, abs=0.01)

def test_acc244(monkeypatch):
    """Test acc244().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(cm, "c244", 0)

    cm.acc244()
    assert cm.c244 == approx(6.80, abs=0.01)

def test_acc245(monkeypatch):
    """Test acc245().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(cm, "c245", 0)

    cm.acc245()
    assert cm.c245 == approx(1.5, abs=0.01)

def acc25_param(**kwargs):
    """Make parameters for a single acc25() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'lsa': 4,
        'expected': approx(25, abs=0.01)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc25_params():
    """Create a list of parameter dicts for the acc25 fixture.
    
    Case 1: lsa_4
    Case 2: lsa_1
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc25_param(),
        acc25_param(lsa=1, expected=approx(19.25, abs=0.01))
        ]

    return params

@pytest.fixture(params=acc25_params(), ids=['lsa_4', 'lsa_1'])
def acc25_fix(request, monkeypatch):
    """Fixture for the acc25() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc25() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc25()
    # Some may be parameterised
    monkeypatch.setattr(cv, 'ucmisc', 2.5e7)
    monkeypatch.setattr(cv, 'lsa', param['lsa'])
    monkeypatch.setattr(cm, 'c25', 0)

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc25(acc25_fix):
    """Test acc25.

    :param acc25_fix: Expected value of acc25()
    :type acc25_fix: ApproxScalar
    """
    # Run acc25() with the current fixture,
    # then assert the result is the expected one
    cm.acc25()
    assert cm.c25 == acc25_fix

def acc26_param(**kwargs):
    """Make parameters for a single acc26() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        'ireactor': 0,
        'powfmw': 2000.0,
        'pinjwp': 250.0,
        'tfcmw': 50.0,
        'pthermmw': htv.pthermmw,
        'pgrossmw': htv.pgrossmw,
        'expected': approx(87.9, abs=0.01)
    }

    # Merge default dict with any optional keyword arguments to override values
    param = {**defaults, **kwargs}
    return param

def acc26_params():
    """Create a list of parameter dicts for the acc26 fixture.
    
    Case 1: ireactor = 0
    Case 2: ireactor = 1
    
    :return: List of parameter dicts
    :rtype: list
    """
    params = [acc26_param(),
        acc26_param(ireactor=1, powfmw=fortran.physics_variables.powfmw,
            pinjwp=htv.pinjwp, tfcmw=fortran.tfcoil_variables.tfcmw,
            pthermmw=3000.0, pgrossmw=700.0)
        ]

    return params

@pytest.fixture(params=acc26_params(), ids=['ireactor_0', 'ireactor_1'])
def acc26_fix(request, monkeypatch):
    """Fixture for the acc26() variables.

    :param request: Request object for accessing parameters
    :type request: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    :return: Expected return value of acc26() for the parameter list
    :rtype: ApproxScalar
    """
    param = request.param

    # Mock variables used by acc26()
    # Some may be parameterised
    monkeypatch.setattr(cv, 'lsa', 4)
    monkeypatch.setattr(cv, 'ireactor', param['ireactor'])
    monkeypatch.setattr(fortran.physics_variables, 'powfmw', param['powfmw'])
    monkeypatch.setattr(htv, 'pinjwp', param['pinjwp'])
    monkeypatch.setattr(fortran.tfcoil_variables, 'tfcmw', param['tfcmw'])
    monkeypatch.setattr(htv, 'pthermmw', param['pthermmw'])
    monkeypatch.setattr(htv, 'pgrossmw', param['pgrossmw'])
    monkeypatch.setattr(cm, 'c26', 0)

    # Return the expected result for the given parameter list
    return param['expected']

def test_acc26(acc26_fix):
    """Test acc26.

    :param acc26_fix: Expected value of acc26()
    :type acc26_fix: ApproxScalar
    """
    # Run acc26() with the current fixture,
    # then assert the result is the expected one
    cm.acc26()
    assert cm.c26 == acc26_fix

def test_acc9(monkeypatch):
    """Test acc9().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(cv, "cdirt", 30e3)
    monkeypatch.setattr(cv, "cowner", 0.15)
    monkeypatch.setattr(cv, "fcontng", 0.195)
    monkeypatch.setattr(cm, "cindrt", 0)

    cm.acc9()
    assert cm.cindrt == approx(10005.0, abs=0.1)
    assert cm.ccont == approx(7800.98, abs=0.1)