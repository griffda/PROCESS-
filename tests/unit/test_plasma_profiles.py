from typing import NamedTuple, Any
import pytest

from process.fortran import divertor_variables, physics_variables
from process.plasma_profiles import plasma_profiles, tcore, ncore, tprofile, nprofile


class TprofileParam(NamedTuple):
    rho: float
    rhopedt: float
    t0: float
    tped: float
    tsep: float
    alphat: float
    tbeta: float
    ipedestal: int
    expected_tprofile: float


@pytest.mark.parametrize(
    "tprofileparam",
    (
        TprofileParam(
            0.5, 0.85, 25.202244205849691, 4.0, 0.1, 1.1, 2.0, 1, 17.289302582471510
        ),
        TprofileParam(0.85, 0.5, 25.202244205849691, 4.0, 0.1, 1.1, 2.0, 1, 1.27),
        TprofileParam(
            0.5, 0.85, 25.202244205849691, 4.0, 0.1, 1.1, 2.0, 0, 18.365662756531375
        ),
    ),
    ids=["hanni_issue_749", "rho_rhopedt_switched", "ipedestal_0"],
)
def test_tprofile(tprofileparam: TprofileParam, monkeypatch):
    monkeypatch.setattr(physics_variables, "ipedestal", tprofileparam.ipedestal)

    assert tprofile(
        tprofileparam.rho,
        tprofileparam.rhopedt,
        tprofileparam.t0,
        tprofileparam.tped,
        tprofileparam.tsep,
        tprofileparam.alphat,
        tprofileparam.tbeta,
    ) == pytest.approx(tprofileparam.expected_tprofile)


class NprofileParam(NamedTuple):
    rho: float
    rhopedn: float
    n0: float
    nped: float
    nsep: float
    alphan: float
    ipedestal: int
    expected_nprofile: float


@pytest.mark.parametrize(
    "nprofileparam",
    (
        NprofileParam(
            0.5,
            0.85,
            1.96864013840830431e20,
            1.0e20,
            0.42e20,
            0.45,
            1,
            1.80014048287798493e20,
        ),
        NprofileParam(
            0.85,
            0.5,
            1.96864013840830431e20,
            1.0e20,
            0.42e20,
            0.45,
            1,
            5.94e19,
        ),
        NprofileParam(
            0.5,
            0.85,
            1.96864013840830431e20,
            1.0e20,
            0.42e20,
            0.45,
            0,
            1.7295929411975656e20,
        ),
    ),
    ids=["hanni_issue_749", "rho_rhopedn_switched", "ipedestal_0"],
)
def test_nprofile(nprofileparam: NprofileParam, monkeypatch):
    monkeypatch.setattr(physics_variables, "ipedestal", nprofileparam.ipedestal)

    assert nprofile(
        nprofileparam.rho,
        nprofileparam.rhopedn,
        nprofileparam.n0,
        nprofileparam.nped,
        nprofileparam.nsep,
        nprofileparam.alphan,
    ) == pytest.approx(nprofileparam.expected_nprofile)


def test_tcore():
    rhopedt = 0.94
    tped = 3.7775374842470044
    tsep = 0.1
    tav = 12.33
    alphat = 1.45
    tbeta = 2.0

    assert tcore(rhopedt, tped, tsep, tav, alphat, tbeta) == pytest.approx(
        28.089723663920328
    )


def test_ncore():
    rhopedn = 0.94
    nped = 5.8300851381352219e19
    nsep = 3.4294618459618943e19
    nav = 7.4321e19
    alphan = 1.0

    assert ncore(rhopedn, nped, nsep, nav, alphan) == pytest.approx(
        9.7756974320342041e19
    )


class PlasmaProfilesParam(NamedTuple):

    prn1: Any = None

    rhopedt: Any = None

    ten: Any = None

    tin: Any = None

    alphap: Any = None

    tbeta: Any = None

    te0: Any = None

    p0: Any = None

    nesep: Any = None

    tesep: Any = None

    pcoef: Any = None

    ipedestal: Any = None

    ni0: Any = None

    ne0: Any = None

    ti0: Any = None

    tratio: Any = None

    dnla: Any = None

    alphat: Any = None

    dnitot: Any = None

    neped: Any = None

    ti: Any = None

    rhopedn: Any = None

    dene: Any = None

    teped: Any = None

    alphan: Any = None

    te: Any = None

    rho_ne_max: Any = None

    rho_te_max: Any = None

    gradient_length_ne: Any = None

    gradient_length_te: Any = None

    rminor: Any = None

    expected_prn1: Any = None

    expected_ten: Any = None

    expected_tin: Any = None

    expected_alphap: Any = None

    expected_te0: Any = None

    expected_p0: Any = None

    expected_pcoef: Any = None

    expected_ni0: Any = None

    expected_ne0: Any = None

    expected_ti0: Any = None

    expected_dnla: Any = None

    expected_ti: Any = None


@pytest.mark.parametrize(
    "plasmaprofilesparam",
    (
        PlasmaProfilesParam(
            prn1=0.40000000000000002,
            rhopedt=0.94000000000000006,
            ten=0,
            tin=0,
            alphap=0,
            tbeta=2,
            te0=0,
            p0=0,
            nesep=3.6421334486704804e19,
            tesep=0.10000000000000001,
            pcoef=0,
            ipedestal=1,
            ni0=0,
            ne0=0,
            ti0=0,
            tratio=1,
            dnla=0,
            alphat=1.45,
            dnitot=6.9461125748017857e19,
            neped=6.1916268627398164e19,
            ti=12.9,
            rhopedn=0.94000000000000006,
            dene=7.983e19,
            teped=5.5,
            alphan=1,
            te=13.07,
            rho_ne_max=0,
            rho_te_max=0,
            gradient_length_ne=0,
            gradient_length_te=0,
            rminor=2.9264516129032256,
            expected_prn1=0.45623618297262686,
            expected_ten=14.521871327399182,
            expected_tin=14.521871327399182,
            expected_alphap=2.4500000000000002,
            expected_te0=27.369013322953624,
            expected_p0=868071.46874220832,
            expected_pcoef=1.1110842637642833,
            expected_ni0=9.210720071916929e19,
            expected_ne0=1.0585658890823703e20,
            expected_ti0=27.369013322953624,
            expected_dnla=8.8687354645836431e19,
            expected_ti=13.07,
        ),
        PlasmaProfilesParam(
            prn1=0.45623618297262686,
            rhopedt=0.94000000000000006,
            ten=14.521871327399182,
            tin=14.521871327399182,
            alphap=2.4500000000000002,
            tbeta=2,
            te0=27.369013322953624,
            p0=868071.46874220832,
            nesep=3.6421334486704804e19,
            tesep=0.10000000000000001,
            pcoef=1.1110842637642833,
            ipedestal=1,
            ni0=9.210720071916929e19,
            ne0=1.0585658890823703e20,
            ti0=27.369013322953624,
            tratio=1,
            dnla=8.8687354645836431e19,
            alphat=1.45,
            dnitot=6.9461125748017857e19,
            neped=6.1916268627398164e19,
            ti=13.07,
            rhopedn=0.94000000000000006,
            dene=7.983e19,
            teped=5.5,
            alphan=1,
            te=13.07,
            rho_ne_max=0,
            rho_te_max=0,
            gradient_length_ne=0,
            gradient_length_te=0,
            rminor=2.9264516129032256,
            expected_prn1=0.45623618297262686,
            expected_ten=14.521871327399182,
            expected_tin=14.521871327399182,
            expected_alphap=2.4500000000000002,
            expected_te0=27.369013322953624,
            expected_p0=868071.46874220832,
            expected_pcoef=1.1110842637642833,
            expected_ni0=9.210720071916929e19,
            expected_ne0=1.0585658890823703e20,
            expected_ti0=27.369013322953624,
            expected_dnla=8.8687354645836431e19,
            expected_ti=13.07,
        ),
    ),
)
def test_plasma_profiles(plasmaprofilesparam, monkeypatch):
    """
    Automatically generated Regression Unit Test for plasma_profiles.

    This test was generated using data from tests/regression/scenarios/HARE/IN.DAT.

    :param plasmaprofilesparam: the data used to mock and assert in this test.
    :type plasmaprofilesparam: plasmaprofilesparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(divertor_variables, "prn1", plasmaprofilesparam.prn1)

    monkeypatch.setattr(physics_variables, "rhopedt", plasmaprofilesparam.rhopedt)

    monkeypatch.setattr(physics_variables, "ten", plasmaprofilesparam.ten)

    monkeypatch.setattr(physics_variables, "tin", plasmaprofilesparam.tin)

    monkeypatch.setattr(physics_variables, "alphap", plasmaprofilesparam.alphap)

    monkeypatch.setattr(physics_variables, "tbeta", plasmaprofilesparam.tbeta)

    monkeypatch.setattr(physics_variables, "te0", plasmaprofilesparam.te0)

    monkeypatch.setattr(physics_variables, "p0", plasmaprofilesparam.p0)

    monkeypatch.setattr(physics_variables, "nesep", plasmaprofilesparam.nesep)

    monkeypatch.setattr(physics_variables, "tesep", plasmaprofilesparam.tesep)

    monkeypatch.setattr(physics_variables, "pcoef", plasmaprofilesparam.pcoef)

    monkeypatch.setattr(physics_variables, "ipedestal", plasmaprofilesparam.ipedestal)

    monkeypatch.setattr(physics_variables, "ni0", plasmaprofilesparam.ni0)

    monkeypatch.setattr(physics_variables, "ne0", plasmaprofilesparam.ne0)

    monkeypatch.setattr(physics_variables, "ti0", plasmaprofilesparam.ti0)

    monkeypatch.setattr(physics_variables, "tratio", plasmaprofilesparam.tratio)

    monkeypatch.setattr(physics_variables, "dnla", plasmaprofilesparam.dnla)

    monkeypatch.setattr(physics_variables, "alphat", plasmaprofilesparam.alphat)

    monkeypatch.setattr(physics_variables, "dnitot", plasmaprofilesparam.dnitot)

    monkeypatch.setattr(physics_variables, "neped", plasmaprofilesparam.neped)

    monkeypatch.setattr(physics_variables, "ti", plasmaprofilesparam.ti)

    monkeypatch.setattr(physics_variables, "rhopedn", plasmaprofilesparam.rhopedn)

    monkeypatch.setattr(physics_variables, "dene", plasmaprofilesparam.dene)

    monkeypatch.setattr(physics_variables, "teped", plasmaprofilesparam.teped)

    monkeypatch.setattr(physics_variables, "alphan", plasmaprofilesparam.alphan)

    monkeypatch.setattr(physics_variables, "te", plasmaprofilesparam.te)

    monkeypatch.setattr(physics_variables, "rho_ne_max", plasmaprofilesparam.rho_ne_max)

    monkeypatch.setattr(physics_variables, "rho_te_max", plasmaprofilesparam.rho_te_max)

    monkeypatch.setattr(
        physics_variables, "gradient_length_ne", plasmaprofilesparam.gradient_length_ne
    )

    monkeypatch.setattr(
        physics_variables, "gradient_length_te", plasmaprofilesparam.gradient_length_te
    )

    monkeypatch.setattr(physics_variables, "rminor", plasmaprofilesparam.rminor)

    plasma_profiles()

    assert divertor_variables.prn1 == pytest.approx(plasmaprofilesparam.expected_prn1)

    assert physics_variables.ten == pytest.approx(plasmaprofilesparam.expected_ten)

    assert physics_variables.tin == pytest.approx(plasmaprofilesparam.expected_tin)

    assert physics_variables.alphap == pytest.approx(
        plasmaprofilesparam.expected_alphap
    )

    assert physics_variables.te0 == pytest.approx(plasmaprofilesparam.expected_te0)

    assert physics_variables.p0 == pytest.approx(plasmaprofilesparam.expected_p0)

    assert physics_variables.pcoef == pytest.approx(plasmaprofilesparam.expected_pcoef)

    assert physics_variables.ni0 == pytest.approx(plasmaprofilesparam.expected_ni0)

    assert physics_variables.ne0 == pytest.approx(plasmaprofilesparam.expected_ne0)

    assert physics_variables.ti0 == pytest.approx(plasmaprofilesparam.expected_ti0)

    assert physics_variables.dnla == pytest.approx(plasmaprofilesparam.expected_dnla)

    assert physics_variables.ti == pytest.approx(plasmaprofilesparam.expected_ti)
