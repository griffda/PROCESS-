from typing import NamedTuple
import pytest

from process.fortran import physics_variables
from process.fortran import profiles_module


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

    tprofile = profiles_module.tprofile(
        tprofileparam.rho,
        tprofileparam.rhopedt,
        tprofileparam.t0,
        tprofileparam.tped,
        tprofileparam.tsep,
        tprofileparam.alphat,
        tprofileparam.tbeta,
    )

    assert tprofile == pytest.approx(tprofileparam.expected_tprofile)


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

    nprofile = profiles_module.nprofile(
        nprofileparam.rho,
        nprofileparam.rhopedn,
        nprofileparam.n0,
        nprofileparam.nped,
        nprofileparam.nsep,
        nprofileparam.alphan,
    )

    assert nprofile == pytest.approx(nprofileparam.expected_nprofile)
