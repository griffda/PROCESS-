"""Unit tests for costs.f90."""
from process.fortran import costs_module
from process.fortran import cost_variables
from process.fortran import fwbs_variables as fv
from process.fortran import heat_transport_variables as htv
from process.fortran import buildings_variables
from process.fortran import build_variables
from process.fortran import ife_variables
from process.fortran import fwbs_variables
from process import fortran
import pytest
import numpy
from pytest import approx
from process.costs import Costs
from typing import NamedTuple, Any


@pytest.fixture
def costs():
    """Provides Pulse object for testing.

    :returns: initialised Pulse object
    :rtype: process.pulse.Pulse
    """
    return Costs()


def acc2261_param(**kwargs):
    """Make parameters for a single acc2261() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {"coolwh": 1, "expected": approx(49.68, abs=0.01)}

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
    params = [
        acc2261_param(),
        acc2261_param(coolwh=2, expected=approx(53.85, abs=0.01)),
    ]

    return params


@pytest.fixture(params=acc2261_params(), ids=["he", "h2o"])
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
    monkeypatch.setattr(cost_variables, "fkind", 1)
    monkeypatch.setattr(cost_variables, "lsa", 1)
    monkeypatch.setattr(htv, "pfwdiv", 0.0)
    monkeypatch.setattr(fv, "pnucblkt", 1558.0)
    monkeypatch.setattr(fv, "pnucshld", 1.478)
    monkeypatch.setattr(htv, "pthermmw", 2647.0)
    monkeypatch.setattr(htv, "nphx", 3)
    monkeypatch.setattr(costs_module, "c2261", 0)

    # Parameterised mocks
    monkeypatch.setattr(fv, "coolwh", param["coolwh"])

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc2261(acc2261_fix):
    """Test acc2261.

    :param acc2261_fix: Expected value of acc2261()
    :type acc2261_fix: ApproxScalar
    """
    # Run acc2261() with the current fixture,
    # then assert the result (c2261) is the expected one
    costs_module.acc2261()
    assert costs_module.c2261 == acc2261_fix


def test_acc2262(monkeypatch):
    """Test acc2262()."""
    # Mock module variables
    monkeypatch.setattr(cost_variables, "fkind", 1)
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(htv, "pinjht", 76.5)
    monkeypatch.setattr(htv, "crypmw", 39.936)
    monkeypatch.setattr(htv, "vachtmw", 0.5)
    monkeypatch.setattr(htv, "trithtmw", 15.0)
    monkeypatch.setattr(htv, "fachtmw", 64.835)
    monkeypatch.setattr(costs_module, "c2262", 0)

    costs_module.acc2262()
    assert costs_module.c2262 == approx(29.408, abs=0.01)


def test_acc2263(monkeypatch):
    """Test acc2263().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "fkind", 1)
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(cost_variables, "uccry", 9.3e4)
    monkeypatch.setattr(fortran.tfcoil_variables, "tftmp", 4.5)
    monkeypatch.setattr(htv, "helpow", 80.980e3)
    monkeypatch.setattr(costs_module, "c2263", 0)

    costs_module.acc2263()
    assert costs_module.c2263 == approx(180.76, abs=0.01)


def test_acc2271(monkeypatch):
    """Test acc2271().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "ucf1", 2.23e7)
    monkeypatch.setattr(cost_variables, "fkind", 1)
    monkeypatch.setattr(costs_module, "c2271", 0)

    costs_module.acc2271()
    assert costs_module.c2271 == approx(22.3, abs=0.01)


def test_acc2272(monkeypatch):
    """Test acc2272().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.physics_variables, "rndfuel", 7.158e20)
    monkeypatch.setattr(fortran.physics_variables, "afuel", 2.5)
    monkeypatch.setattr(cost_variables, "fkind", 1)
    monkeypatch.setattr(costs_module, "c2271", 0)

    costs_module.acc2272()
    assert costs_module.c2272 == approx(114.707, abs=0.01)


def acc2273_param(**kwargs):
    """Make parameters for a single acc2273() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        "ftrit": 0.0001,
        "volrci": fortran.buildings_variables.volrci,
        "wsvol": fortran.buildings_variables.wsvol,
        "expected": approx(0.0, abs=0.00001),
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
    params = [
        acc2273_param(),
        acc2273_param(
            ftrit=0.5,
            volrci=1299783.4,
            wsvol=132304.1,
            expected=approx(74.12, abs=0.01),
        ),
    ]

    return params


@pytest.fixture(params=acc2273_params(), ids=["ttrit_low", "ttrit_high"])
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
    monkeypatch.setattr(fortran.buildings_variables, "wsvol", param["wsvol"])
    monkeypatch.setattr(fortran.buildings_variables, "volrci", param["volrci"])
    monkeypatch.setattr(fortran.physics_variables, "ftrit", param["ftrit"])

    # Mock result var as negative, as an expected result is 0
    # Otherwise could get false positive result
    monkeypatch.setattr(costs_module, "c2273", -1)

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc2273(acc2273_fix):
    """Test acc2273.

    :param acc2273_fix: Expected value of acc2273()
    :type acc2273_fix: ApproxScalar
    """
    # Run acc2273() with the current fixture,
    # then assert the result is the expected one
    costs_module.acc2273()
    assert costs_module.c2273 == acc2273_fix


def test_acc2274(monkeypatch):
    """Test acc2274().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.buildings_variables, "wsvol", 132304.1)
    monkeypatch.setattr(fortran.buildings_variables, "volrci", 1299783.4)
    monkeypatch.setattr(cost_variables, "fkind", 1)
    monkeypatch.setattr(costs_module, "c2274", 0)

    costs_module.acc2274()
    assert costs_module.c2274 == approx(84.10, abs=0.01)


def acc228_param(**kwargs):
    """Make parameters for a single acc228() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {"fkind": 1.0, "expected": approx(150.0, abs=0.01)}

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
    params = [acc228_param(), acc228_param(fkind=0.5, expected=approx(75.0, abs=0.01))]

    return params


@pytest.fixture(params=acc228_params(), ids=["fkind_1", "fkind_0p5"])
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
    monkeypatch.setattr(cost_variables, "fkind", param["fkind"])
    monkeypatch.setattr(costs_module, "c228", 0)

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc228(acc228_fix):
    """Test acc228.

    :param acc228_fix: Expected value of acc228()
    :type acc228_fix: ApproxScalar
    """
    # Run acc228() with the current fixture,
    # then assert the result is the expected one
    costs_module.acc228()
    assert costs_module.c228 == acc228_fix


def acc229_param(**kwargs):
    """Make parameters for a single acc229() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {"fkind": 1.0, "expected": approx(125.0, abs=0.01)}

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
    params = [acc229_param(), acc229_param(fkind=0.5, expected=approx(62.5, abs=0.01))]

    return params


@pytest.fixture(params=acc229_params(), ids=["fkind_1", "fkind_0p5"])
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
    monkeypatch.setattr(cost_variables, "fkind", param["fkind"])
    monkeypatch.setattr(costs_module, "c229", 0)

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc229(acc229_fix):
    """Test acc229.

    :param acc229_fix: Expected value of acc229()
    :type acc229_fix: ApproxScalar
    """
    # Run acc229() with the current fixture,
    # then assert the result is the expected one
    costs_module.acc229()
    assert costs_module.c229 == acc229_fix


def acc23_param(**kwargs):
    """Make parameters for a single acc23() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {"coolwh": 1, "expected": approx(230, abs=0.01)}

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
    params = [acc23_param(), acc23_param(coolwh=2, expected=approx(245, abs=0.01))]

    return params


@pytest.fixture(params=acc23_params(), ids=["he", "h2o"])
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
    monkeypatch.setattr(fv, "coolwh", param["coolwh"])
    monkeypatch.setattr(htv, "pgrossmw", 1200.0)
    monkeypatch.setattr(costs_module, "c23", 0)

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc23(acc23_fix):
    """Test acc23.

    :param acc23_fix: Expected value of acc23()
    :type acc23_fix: ApproxScalar
    """
    # Run acc23() with the current fixture,
    # then assert the result is the expected one
    costs_module.acc23()
    assert costs_module.c23 == acc23_fix


def test_acc241(monkeypatch):
    """Test acc241().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(costs_module, "c241", 0)

    costs_module.acc241()
    assert costs_module.c241 == approx(18.4, abs=0.01)


def test_acc242(monkeypatch):
    """Test acc242().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(htv, "pacpmw", 630.0)
    monkeypatch.setattr(htv, "fcsht", 65.0)
    monkeypatch.setattr(costs_module, "c242", 0)

    costs_module.acc242()
    assert costs_module.c242 == approx(9.06, abs=0.01)


def test_acc243(monkeypatch):
    """Test acc243().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(htv, "tlvpmw", 403.8)
    monkeypatch.setattr(costs_module, "c243", 0)

    costs_module.acc243()
    assert costs_module.c243 == approx(8.08, abs=0.01)


def test_acc244(monkeypatch):
    """Test acc244().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(costs_module, "c244", 0)

    costs_module.acc244()
    assert costs_module.c244 == approx(6.80, abs=0.01)


def test_acc245(monkeypatch):
    """Test acc245().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(costs_module, "c245", 0)

    costs_module.acc245()
    assert costs_module.c245 == approx(1.5, abs=0.01)


def acc25_param(**kwargs):
    """Make parameters for a single acc25() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {"lsa": 4, "expected": approx(25, abs=0.01)}

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
    params = [acc25_param(), acc25_param(lsa=1, expected=approx(19.25, abs=0.01))]

    return params


@pytest.fixture(params=acc25_params(), ids=["lsa_4", "lsa_1"])
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
    monkeypatch.setattr(cost_variables, "ucmisc", 2.5e7)
    monkeypatch.setattr(cost_variables, "lsa", param["lsa"])
    monkeypatch.setattr(costs_module, "c25", 0)

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc25(acc25_fix):
    """Test acc25.

    :param acc25_fix: Expected value of acc25()
    :type acc25_fix: ApproxScalar
    """
    # Run acc25() with the current fixture,
    # then assert the result is the expected one
    costs_module.acc25()
    assert costs_module.c25 == acc25_fix


def acc26_param(**kwargs):
    """Make parameters for a single acc26() test.

    :return: Parameters, including expected result
    :rtype: dict
    """
    # Default parameters
    defaults = {
        "ireactor": 0,
        "powfmw": 2000.0,
        "pinjwp": 250.0,
        "tfcmw": 50.0,
        "pthermmw": htv.pthermmw,
        "pgrossmw": htv.pgrossmw,
        "expected": approx(87.9, abs=0.01),
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
    params = [
        acc26_param(),
        acc26_param(
            ireactor=1,
            powfmw=fortran.physics_variables.powfmw,
            pinjwp=htv.pinjwp,
            tfcmw=fortran.tfcoil_variables.tfcmw,
            pthermmw=3000.0,
            pgrossmw=700.0,
        ),
    ]

    return params


@pytest.fixture(params=acc26_params(), ids=["ireactor_0", "ireactor_1"])
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
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(cost_variables, "ireactor", param["ireactor"])
    monkeypatch.setattr(fortran.physics_variables, "powfmw", param["powfmw"])
    monkeypatch.setattr(htv, "pinjwp", param["pinjwp"])
    monkeypatch.setattr(fortran.tfcoil_variables, "tfcmw", param["tfcmw"])
    monkeypatch.setattr(htv, "pthermmw", param["pthermmw"])
    monkeypatch.setattr(htv, "pgrossmw", param["pgrossmw"])
    monkeypatch.setattr(costs_module, "c26", 0)

    # Return the expected result for the given parameter list
    return param["expected"]


def test_acc26(acc26_fix):
    """Test acc26.

    :param acc26_fix: Expected value of acc26()
    :type acc26_fix: ApproxScalar
    """
    # Run acc26() with the current fixture,
    # then assert the result is the expected one
    costs_module.acc26()
    assert costs_module.c26 == acc26_fix


def test_acc9(monkeypatch):
    """Test acc9().

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(cost_variables, "lsa", 4)
    monkeypatch.setattr(cost_variables, "cdirt", 30e3)
    monkeypatch.setattr(cost_variables, "cowner", 0.15)
    monkeypatch.setattr(cost_variables, "fcontng", 0.195)
    monkeypatch.setattr(costs_module, "cindrt", 0)

    costs_module.acc9()
    assert costs_module.cindrt == approx(10005.0, abs=0.1)
    assert costs_module.ccont == approx(7800.98, abs=0.1)


class Acc21Param(NamedTuple):

    shovol: Any = None

    triv: Any = None

    elevol: Any = None

    rbvol: Any = None

    cryvol: Any = None

    rmbvol: Any = None

    admvol: Any = None

    convol: Any = None

    wsvol: Any = None

    ucrb: Any = None

    ireactor: Any = None

    cturbb: Any = None

    lsa: Any = None

    csi: Any = None

    cland: Any = None

    c21: Any = None

    c211: Any = None

    c212: Any = None

    c213: Any = None

    c214: Any = None

    c2141: Any = None

    c2142: Any = None

    c215: Any = None

    c216: Any = None

    c217: Any = None

    c2171: Any = None

    c2172: Any = None

    c2173: Any = None

    c2174: Any = None

    expected_c21: Any = None

    expected_c211: Any = None

    expected_c212: Any = None

    expected_c213: Any = None

    expected_c214: Any = None

    expected_c2141: Any = None

    expected_c2142: Any = None

    expected_c215: Any = None

    expected_c216: Any = None

    expected_c217: Any = None

    expected_c2171: Any = None

    expected_c2172: Any = None

    expected_c2173: Any = None

    expected_c2174: Any = None


@pytest.mark.parametrize(
    "acc21param",
    (
        Acc21Param(
            shovol=100000,
            triv=40000,
            elevol=51601.097615432001,
            rbvol=1356973.2891062023,
            cryvol=15247.180612719381,
            rmbvol=421473.52130148414,
            admvol=100000,
            convol=60000,
            wsvol=130018.25667917728,
            ucrb=400,
            ireactor=1,
            cturbb=38,
            lsa=2,
            csi=16,
            cland=19.199999999999999,
            c21=0,
            c211=0,
            c212=0,
            c213=0,
            c214=0,
            c2141=0,
            c2142=0,
            c215=0,
            c216=0,
            c217=0,
            c2171=0,
            c2172=0,
            c2173=0,
            c2174=0,
            expected_c21=740.00647752036286,
            expected_c211=32.640000000000001,
            expected_c212=455.94302513968393,
            expected_c213=31.919999999999998,
            expected_c214=142.28887143307821,
            expected_c2141=92.049817052244123,
            expected_c2142=50.239054380834098,
            expected_c215=12.431999999999999,
            expected_c216=16.471070358845893,
            expected_c217=48.311510588754764,
            expected_c2171=15.119999999999999,
            expected_c2172=17.640000000000001,
            expected_c2173=9.6599999999999984,
            expected_c2174=5.8915105887547687,
        ),
        Acc21Param(
            shovol=100000,
            triv=40000,
            elevol=51609.268177478581,
            rbvol=1358540.6868905292,
            cryvol=25826.919937316459,
            rmbvol=423252.94369581528,
            admvol=100000,
            convol=60000,
            wsvol=130255.93791329287,
            ucrb=400,
            ireactor=1,
            cturbb=38,
            lsa=2,
            csi=16,
            cland=19.199999999999999,
            c21=740.00647752036286,
            c211=32.640000000000001,
            c212=455.94302513968393,
            c213=31.919999999999998,
            c214=142.28887143307821,
            c2141=92.049817052244123,
            c2142=50.239054380834098,
            c215=12.431999999999999,
            c216=16.471070358845893,
            c217=48.311510588754764,
            c2171=15.119999999999999,
            c2172=17.640000000000001,
            c2173=9.6599999999999984,
            c2174=5.8915105887547687,
            expected_c21=745.10420837411039,
            expected_c211=32.640000000000001,
            expected_c212=456.46967079521778,
            expected_c213=31.919999999999998,
            expected_c214=142.76933731286238,
            expected_c2141=92.438442903166035,
            expected_c2142=50.330894409696356,
            expected_c215=12.431999999999999,
            expected_c216=16.47367840225116,
            expected_c217=52.399521863779071,
            expected_c2171=15.119999999999999,
            expected_c2172=17.640000000000001,
            expected_c2173=9.6599999999999984,
            expected_c2174=9.9795218637790786,
        ),
    ),
)
def test_acc21(acc21param, monkeypatch):
    """
    Automatically generated Regression Unit Test for acc21.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param acc21param: the data used to mock and assert in this test.
    :type acc21param: acc21param

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(buildings_variables, "shovol", acc21param.shovol)

    monkeypatch.setattr(buildings_variables, "triv", acc21param.triv)

    monkeypatch.setattr(buildings_variables, "elevol", acc21param.elevol)

    monkeypatch.setattr(buildings_variables, "rbvol", acc21param.rbvol)

    monkeypatch.setattr(buildings_variables, "cryvol", acc21param.cryvol)

    monkeypatch.setattr(buildings_variables, "rmbvol", acc21param.rmbvol)

    monkeypatch.setattr(buildings_variables, "admvol", acc21param.admvol)

    monkeypatch.setattr(buildings_variables, "convol", acc21param.convol)

    monkeypatch.setattr(buildings_variables, "wsvol", acc21param.wsvol)

    monkeypatch.setattr(cost_variables, "ucrb", acc21param.ucrb)

    monkeypatch.setattr(cost_variables, "ireactor", acc21param.ireactor)

    monkeypatch.setattr(cost_variables, "cturbb", acc21param.cturbb)

    monkeypatch.setattr(cost_variables, "lsa", acc21param.lsa)

    monkeypatch.setattr(cost_variables, "csi", acc21param.csi)

    monkeypatch.setattr(cost_variables, "cland", acc21param.cland)

    monkeypatch.setattr(costs_module, "c21", acc21param.c21)

    monkeypatch.setattr(costs_module, "c211", acc21param.c211)

    monkeypatch.setattr(costs_module, "c212", acc21param.c212)

    monkeypatch.setattr(costs_module, "c213", acc21param.c213)

    monkeypatch.setattr(costs_module, "c214", acc21param.c214)

    monkeypatch.setattr(costs_module, "c2141", acc21param.c2141)

    monkeypatch.setattr(costs_module, "c2142", acc21param.c2142)

    monkeypatch.setattr(costs_module, "c215", acc21param.c215)

    monkeypatch.setattr(costs_module, "c216", acc21param.c216)

    monkeypatch.setattr(costs_module, "c217", acc21param.c217)

    monkeypatch.setattr(costs_module, "c2171", acc21param.c2171)

    monkeypatch.setattr(costs_module, "c2172", acc21param.c2172)

    monkeypatch.setattr(costs_module, "c2173", acc21param.c2173)

    monkeypatch.setattr(costs_module, "c2174", acc21param.c2174)

    costs_module.acc21()

    assert costs_module.c21 == pytest.approx(acc21param.expected_c21)

    assert costs_module.c211 == pytest.approx(acc21param.expected_c211)

    assert costs_module.c212 == pytest.approx(acc21param.expected_c212)

    assert costs_module.c213 == pytest.approx(acc21param.expected_c213)

    assert costs_module.c214 == pytest.approx(acc21param.expected_c214)

    assert costs_module.c2141 == pytest.approx(acc21param.expected_c2141)

    assert costs_module.c2142 == pytest.approx(acc21param.expected_c2142)

    assert costs_module.c215 == pytest.approx(acc21param.expected_c215)

    assert costs_module.c216 == pytest.approx(acc21param.expected_c216)

    assert costs_module.c217 == pytest.approx(acc21param.expected_c217)

    assert costs_module.c2171 == pytest.approx(acc21param.expected_c2171)

    assert costs_module.c2172 == pytest.approx(acc21param.expected_c2172)

    assert costs_module.c2173 == pytest.approx(acc21param.expected_c2173)

    assert costs_module.c2174 == pytest.approx(acc21param.expected_c2174)


class Acc2211Param(NamedTuple):

    fwarea: Any = None

    ucblss: Any = None

    fkind: Any = None

    fwallcst: Any = None

    ucblli2o: Any = None

    ifueltyp: Any = None

    lsa: Any = None

    fwmatm: Any = None

    uccarb: Any = None

    ife: Any = None

    ucconc: Any = None

    c22: Any = None

    c2211: Any = None

    expected_fwallcst: Any = None


@pytest.mark.parametrize(
    "acc2211param",
    (
        Acc2211Param(
            fwarea=1601.1595634509963,
            ucblss=90,
            fkind=1,
            fwallcst=0,
            ucblli2o=600,
            ifueltyp=1,
            lsa=2,
            fwmatm=numpy.array(
                (
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                ),
                order="F",
            ).transpose(),
            uccarb=50,
            ife=0,
            ucconc=0.10000000000000001,
            c22=0,
            c2211=0,
            expected_fwallcst=143.19827300247195,
        ),
        Acc2211Param(
            fwarea=1891.2865102700493,
            ucblss=90,
            fkind=1,
            fwallcst=143.19827300247195,
            ucblli2o=600,
            ifueltyp=1,
            lsa=2,
            fwmatm=numpy.array(
                (
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                ),
                order="F",
            ).transpose(),
            uccarb=50,
            ife=0,
            ucconc=0.10000000000000001,
            c22=3474.7391916096453,
            c2211=0,
            expected_fwallcst=167.7865317453867,
        ),
    ),
)
def test_acc2211(acc2211param, monkeypatch):
    """
    Automatically generated Regression Unit Test for acc2211.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param acc2211param: the data used to mock and assert in this test.
    :type acc2211param: acc2211param

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(build_variables, "fwarea", acc2211param.fwarea)

    monkeypatch.setattr(cost_variables, "ucblss", acc2211param.ucblss)

    monkeypatch.setattr(cost_variables, "fkind", acc2211param.fkind)

    monkeypatch.setattr(cost_variables, "fwallcst", acc2211param.fwallcst)

    monkeypatch.setattr(cost_variables, "ucblli2o", acc2211param.ucblli2o)

    monkeypatch.setattr(cost_variables, "ifueltyp", acc2211param.ifueltyp)

    monkeypatch.setattr(cost_variables, "lsa", acc2211param.lsa)

    monkeypatch.setattr(ife_variables, "fwmatm", acc2211param.fwmatm)

    monkeypatch.setattr(ife_variables, "uccarb", acc2211param.uccarb)

    monkeypatch.setattr(ife_variables, "ife", acc2211param.ife)

    monkeypatch.setattr(ife_variables, "ucconc", acc2211param.ucconc)

    monkeypatch.setattr(costs_module, "c22", acc2211param.c22)

    monkeypatch.setattr(costs_module, "c2211", acc2211param.c2211)

    costs_module.acc2211()

    assert cost_variables.fwallcst == pytest.approx(acc2211param.expected_fwallcst)


class Acc2212Param(NamedTuple):

    ucblss: Any = None

    ucblbreed: Any = None

    ucblbe: Any = None

    ucblli: Any = None

    ucblvd: Any = None

    ucblli2o: Any = None

    blkcst: Any = None

    ucbllipb: Any = None

    ifueltyp: Any = None

    lsa: Any = None

    fkind: Any = None

    iblanket: Any = None

    whtblli: Any = None

    wtblli2o: Any = None

    whtblbreed: Any = None

    whtblvd: Any = None

    whtblbe: Any = None

    whtblss: Any = None

    wtbllipb: Any = None

    ucflib: Any = None

    blmatm: Any = None

    ife: Any = None

    ucconc: Any = None

    mflibe: Any = None

    uccarb: Any = None

    c22: Any = None

    c2212: Any = None

    c22121: Any = None

    c22122: Any = None

    c22123: Any = None

    c22124: Any = None

    c22125: Any = None

    c22126: Any = None

    c22127: Any = None

    c22128: Any = None

    expected_blkcst: Any = None

    expected_c22121: Any = None

    expected_c22122: Any = None

    expected_c22123: Any = None


@pytest.mark.parametrize(
    "acc2212param",
    (
        Acc2212Param(
            ucblss=90,
            ucblbreed=875,
            ucblbe=260,
            ucblli=875,
            ucblvd=280,
            ucblli2o=600,
            blkcst=0,
            ucbllipb=10.300000000000001,
            ifueltyp=1,
            lsa=2,
            fkind=1,
            iblanket=1,
            whtblli=0,
            wtblli2o=1258110.2710352642,
            whtblbreed=0,
            whtblvd=0,
            whtblbe=1184720.5052248738,
            whtblss=1058196.5489677608,
            wtbllipb=0,
            ucflib=84,
            blmatm=numpy.array(
                (
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                ),
                order="F",
            ).transpose(),
            ife=0,
            ucconc=0.10000000000000001,
            mflibe=0,
            uccarb=50,
            c22=0,
            c2212=0,
            c22121=0,
            c22122=0,
            c22123=0,
            c22124=0,
            c22125=0,
            c22126=0,
            c22127=0,
            c22128=0,
            expected_blkcst=868.59838754004318,
            expected_c22121=231.02049851885039,
            expected_c22122=566.14962196586885,
            expected_c22123=71.428267055323843,
        ),
        Acc2212Param(
            ucblss=90,
            ucblbreed=875,
            ucblbe=260,
            ucblli=875,
            ucblvd=280,
            ucblli2o=600,
            blkcst=868.59838754004318,
            ucbllipb=10.300000000000001,
            ifueltyp=1,
            lsa=2,
            fkind=1,
            iblanket=1,
            whtblli=0,
            wtblli2o=1260437.468838267,
            whtblbreed=0,
            whtblvd=0,
            whtblbe=1186911.9498227015,
            whtblss=1060153.955039866,
            wtbllipb=0,
            ucflib=84,
            blmatm=numpy.array(
                (
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                    (0, 0, 0),
                ),
                order="F",
            ).transpose(),
            ife=0,
            ucconc=0.10000000000000001,
            mflibe=0,
            uccarb=50,
            c22=3474.7391916096453,
            c2212=0,
            c22121=231.02049851885039,
            c22122=566.14962196586885,
            c22123=71.428267055323843,
            c22124=0,
            c22125=0,
            c22126=0,
            c22127=0,
            c22128=0,
            expected_blkcst=870.20508315783786,
            expected_c22121=231.44783021542679,
            expected_c22122=567.19686097722013,
            expected_c22123=71.560391965190959,
        ),
    ),
)
def test_acc2212(acc2212param, monkeypatch):
    """
    Automatically generated Regression Unit Test for acc2212.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param acc2212param: the data used to mock and assert in this test.
    :type acc2212param: acc2212param

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(cost_variables, "ucblss", acc2212param.ucblss)

    monkeypatch.setattr(cost_variables, "ucblbreed", acc2212param.ucblbreed)

    monkeypatch.setattr(cost_variables, "ucblbe", acc2212param.ucblbe)

    monkeypatch.setattr(cost_variables, "ucblli", acc2212param.ucblli)

    monkeypatch.setattr(cost_variables, "ucblvd", acc2212param.ucblvd)

    monkeypatch.setattr(cost_variables, "ucblli2o", acc2212param.ucblli2o)

    monkeypatch.setattr(cost_variables, "blkcst", acc2212param.blkcst)

    monkeypatch.setattr(cost_variables, "ucbllipb", acc2212param.ucbllipb)

    monkeypatch.setattr(cost_variables, "ifueltyp", acc2212param.ifueltyp)

    monkeypatch.setattr(cost_variables, "lsa", acc2212param.lsa)

    monkeypatch.setattr(cost_variables, "fkind", acc2212param.fkind)

    monkeypatch.setattr(fwbs_variables, "iblanket", acc2212param.iblanket)

    monkeypatch.setattr(fwbs_variables, "whtblli", acc2212param.whtblli)

    monkeypatch.setattr(fwbs_variables, "wtblli2o", acc2212param.wtblli2o)

    monkeypatch.setattr(fwbs_variables, "whtblbreed", acc2212param.whtblbreed)

    monkeypatch.setattr(fwbs_variables, "whtblvd", acc2212param.whtblvd)

    monkeypatch.setattr(fwbs_variables, "whtblbe", acc2212param.whtblbe)

    monkeypatch.setattr(fwbs_variables, "whtblss", acc2212param.whtblss)

    monkeypatch.setattr(fwbs_variables, "wtbllipb", acc2212param.wtbllipb)

    monkeypatch.setattr(ife_variables, "ucflib", acc2212param.ucflib)

    monkeypatch.setattr(ife_variables, "blmatm", acc2212param.blmatm)

    monkeypatch.setattr(ife_variables, "ife", acc2212param.ife)

    monkeypatch.setattr(ife_variables, "ucconc", acc2212param.ucconc)

    monkeypatch.setattr(ife_variables, "mflibe", acc2212param.mflibe)

    monkeypatch.setattr(ife_variables, "uccarb", acc2212param.uccarb)

    monkeypatch.setattr(costs_module, "c22", acc2212param.c22)

    monkeypatch.setattr(costs_module, "c2212", acc2212param.c2212)

    monkeypatch.setattr(costs_module, "c22121", acc2212param.c22121)

    monkeypatch.setattr(costs_module, "c22122", acc2212param.c22122)

    monkeypatch.setattr(costs_module, "c22123", acc2212param.c22123)

    monkeypatch.setattr(costs_module, "c22124", acc2212param.c22124)

    monkeypatch.setattr(costs_module, "c22125", acc2212param.c22125)

    monkeypatch.setattr(costs_module, "c22126", acc2212param.c22126)

    monkeypatch.setattr(costs_module, "c22127", acc2212param.c22127)

    monkeypatch.setattr(costs_module, "c22128", acc2212param.c22128)

    costs_module.acc2212()

    assert cost_variables.blkcst == pytest.approx(acc2212param.expected_blkcst)

    assert costs_module.c22121 == pytest.approx(acc2212param.expected_c22121)

    assert costs_module.c22122 == pytest.approx(acc2212param.expected_c22122)

    assert costs_module.c22123 == pytest.approx(acc2212param.expected_c22123)
