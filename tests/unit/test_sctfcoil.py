import pytest
import numpy
from typing import NamedTuple, Any

from process.fortran import sctfcoil_module
from process.fortran import tfcoil_variables
from process.fortran import global_variables
from process.fortran import physics_variables
from process.fortran import build_variables
from process.sctfcoil import Sctfcoil


@pytest.fixture
def sctfcoil():
    """Provides Sctfcoil object for testing.

    :returns: initialised Sctfcoil object
    :rtype: process.sctfcoil.Sctfcoil
    """
    return Sctfcoil()


class ProtectParam(NamedTuple):

    aio: Any = None

    tfes: Any = None

    acs: Any = None

    aturn: Any = None

    tdump: Any = None

    fcond: Any = None

    fcu: Any = None

    tba: Any = None

    tmax: Any = None

    expected_ajwpro: Any = None

    expected_vd: Any = None


@pytest.mark.parametrize(
    "protectparam",
    (
        ProtectParam(
            aio=74026.751437500003,
            tfes=9561415368.8360519,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            tdump=25.829000000000001,
            fcond=0.63927285511442711,
            fcu=0.80884,
            tba=4.75,
            tmax=150,
            expected_ajwpro=17475706.393616617,
            expected_vd=10001.287165953383,
        ),
    ),
)
def test_protect(protectparam, sctfcoil):
    """
    Automatically generated Regression Unit Test for protect.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param protectparam: the data used to mock and assert in this test.
    :type protectparam: protectparam

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    ajwpro, vd = sctfcoil.protect(
        aio=protectparam.aio,
        tfes=protectparam.tfes,
        acs=protectparam.acs,
        aturn=protectparam.aturn,
        tdump=protectparam.tdump,
        fcond=protectparam.fcond,
        fcu=protectparam.fcu,
        tba=protectparam.tba,
        tmax=protectparam.tmax,
    )

    assert ajwpro == pytest.approx(protectparam.expected_ajwpro)

    assert vd == pytest.approx(protectparam.expected_vd)


class SuperconParam(NamedTuple):

    tmargmin_tf: Any = None

    n_tf: Any = None

    temp_margin: Any = None

    jwdgpro: Any = None

    dhecoil: Any = None

    cpttf: Any = None

    bmaxtfrp: Any = None

    str_tf_con_res: Any = None

    b_crit_upper_nbti: Any = None

    i_str_wp: Any = None

    str_wp: Any = None

    t_crit_nbti: Any = None

    tf_fit_t: Any = None

    tf_fit_z: Any = None

    tf_fit_y: Any = None

    run_tests: Any = None

    isumat: Any = None

    iprint: Any = None

    outfile: Any = None

    acs: Any = None

    aturn: Any = None

    bmax: Any = None

    fcu: Any = None

    fhe: Any = None

    fhts: Any = None

    iop: Any = None

    jwp: Any = None

    tdmptf: Any = None

    tfes: Any = None

    thelium: Any = None

    tmax: Any = None

    bcritsc: Any = None

    tcritsc: Any = None

    expected_temp_margin: Any = None

    expected_jwdgpro: Any = None

    expected_jwdgcrt: Any = None

    expected_vd: Any = None

    expected_tmarg: Any = None


@pytest.mark.parametrize(
    "superconparam",
    (
        SuperconParam(
            tmargmin_tf=1.5,
            n_tf=16,
            temp_margin=0,
            jwdgpro=0,
            dhecoil=0.010000000000000002,
            cpttf=74026.751437500003,
            bmaxtfrp=12.48976756562082,
            str_tf_con_res=-0.0050000000000000001,
            b_crit_upper_nbti=14.859999999999999,
            i_str_wp=1,
            str_wp=0.0015619754370069119,
            t_crit_nbti=9.0399999999999991,
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            run_tests=0,
            isumat=5,
            iprint=0,
            outfile=11,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            bmax=12.48976756562082,
            fcu=0.80884,
            fhe=0.30000000000000004,
            fhts=0.5,
            iop=74026.751437500003,
            jwp=23124470.793774806,
            tdmptf=25.829000000000001,
            tfes=9548964780.4287167,
            thelium=4.75,
            tmax=150,
            bcritsc=24,
            tcritsc=16,
            expected_temp_margin=2.3431632224075836,
            expected_jwdgpro=17475706.393616617,
            expected_jwdgcrt=41107234.360397324,
            expected_vd=9988.2637896807955,
            expected_tmarg=2.3431632224075836,
        ),
        SuperconParam(
            tmargmin_tf=1.5,
            n_tf=16,
            temp_margin=2.3431632224075836,
            jwdgpro=17475706.393616617,
            dhecoil=0.010000000000000002,
            cpttf=74026.751437500003,
            bmaxtfrp=12.48976756562082,
            str_tf_con_res=-0.0050000000000000001,
            b_crit_upper_nbti=14.859999999999999,
            i_str_wp=1,
            str_wp=0.0015619754370069119,
            t_crit_nbti=9.0399999999999991,
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            run_tests=0,
            isumat=5,
            iprint=0,
            outfile=11,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            bmax=12.48976756562082,
            fcu=0.80884,
            fhe=0.30000000000000004,
            fhts=0.5,
            iop=74026.751437500003,
            jwp=23124470.793774806,
            tdmptf=25.829000000000001,
            tfes=9561415368.8360519,
            thelium=4.75,
            tmax=150,
            bcritsc=24,
            tcritsc=16,
            expected_temp_margin=2.3431632224075836,
            expected_jwdgpro=17475706.393616617,
            expected_jwdgcrt=41107234.360397324,
            expected_vd=10001.287165953383,
            expected_tmarg=2.3431632224075836,
        ),
        SuperconParam(
            tmargmin_tf=1.5,
            n_tf=16,
            temp_margin=2.3431632224075836,
            jwdgpro=17475706.393616617,
            dhecoil=0.010000000000000002,
            cpttf=74026.751437500003,
            bmaxtfrp=12.48976756562082,
            str_tf_con_res=-0.0050000000000000001,
            b_crit_upper_nbti=14.859999999999999,
            i_str_wp=1,
            str_wp=0.0015619754370069119,
            t_crit_nbti=9.0399999999999991,
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            run_tests=0,
            isumat=5,
            iprint=0,
            outfile=11,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            bmax=12.48976756562082,
            fcu=0.80884,
            fhe=0.30000000000000004,
            fhts=0.5,
            iop=74026.751437500003,
            jwp=23124470.793774806,
            tdmptf=25.829000000000001,
            tfes=9561415368.8360519,
            thelium=4.75,
            tmax=150,
            bcritsc=24,
            tcritsc=16,
            expected_temp_margin=2.3431632224075836,
            expected_jwdgpro=17475706.393616617,
            expected_jwdgcrt=41107234.360397324,
            expected_vd=10001.287165953383,
            expected_tmarg=2.3431632224075836,
        ),
    ),
)
def test_supercon(superconparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for supercon.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param superconparam: the data used to mock and assert in this test.
    :type superconparam: superconparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(tfcoil_variables, "tmargmin_tf", superconparam.tmargmin_tf)

    monkeypatch.setattr(tfcoil_variables, "n_tf", superconparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "temp_margin", superconparam.temp_margin)

    monkeypatch.setattr(tfcoil_variables, "jwdgpro", superconparam.jwdgpro)

    monkeypatch.setattr(tfcoil_variables, "dhecoil", superconparam.dhecoil)

    monkeypatch.setattr(tfcoil_variables, "cpttf", superconparam.cpttf)

    monkeypatch.setattr(tfcoil_variables, "bmaxtfrp", superconparam.bmaxtfrp)

    monkeypatch.setattr(
        tfcoil_variables, "str_tf_con_res", superconparam.str_tf_con_res
    )

    monkeypatch.setattr(
        tfcoil_variables, "b_crit_upper_nbti", superconparam.b_crit_upper_nbti
    )

    monkeypatch.setattr(tfcoil_variables, "i_str_wp", superconparam.i_str_wp)

    monkeypatch.setattr(tfcoil_variables, "str_wp", superconparam.str_wp)

    monkeypatch.setattr(tfcoil_variables, "t_crit_nbti", superconparam.t_crit_nbti)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_t", superconparam.tf_fit_t)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_z", superconparam.tf_fit_z)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_y", superconparam.tf_fit_y)

    monkeypatch.setattr(global_variables, "run_tests", superconparam.run_tests)

    jwdgcrt, vd, tmarg = sctfcoil.supercon(
        isumat=superconparam.isumat,
        acs=superconparam.acs,
        aturn=superconparam.aturn,
        bmax=superconparam.bmax,
        fcu=superconparam.fcu,
        fhe=superconparam.fhe,
        fhts=superconparam.fhts,
        iop=superconparam.iop,
        jwp=superconparam.jwp,
        tdmptf=superconparam.tdmptf,
        tfes=superconparam.tfes,
        thelium=superconparam.thelium,
        tmax=superconparam.tmax,
        bcritsc=superconparam.bcritsc,
        tcritsc=superconparam.tcritsc,
        output=False,
    )

    assert tfcoil_variables.temp_margin == pytest.approx(
        superconparam.expected_temp_margin
    )

    assert tfcoil_variables.jwdgpro == pytest.approx(superconparam.expected_jwdgpro)

    assert jwdgcrt == pytest.approx(superconparam.expected_jwdgcrt)

    assert vd == pytest.approx(superconparam.expected_vd)

    assert tmarg == pytest.approx(superconparam.expected_tmarg)


class TfCurrentParam(NamedTuple):

    casthi: Any = None

    ritfc: Any = None

    rbmax: Any = None

    i_tf_sup: Any = None

    casths_fraction: Any = None

    tinstf: Any = None

    tftort: Any = None

    bmaxtf: Any = None

    tfinsgap: Any = None

    tfc_sidewall_is_fraction: Any = None

    casths: Any = None

    casthi_is_fraction: Any = None

    casthi_fraction: Any = None

    n_tf: Any = None

    thicndut: Any = None

    thkcas: Any = None

    oacdcp: Any = None

    tfareain: Any = None

    r_tf_inboard_out: Any = None

    r_tf_inboard_in: Any = None

    tfcth: Any = None

    bt: Any = None

    rmajor: Any = None

    tfc_current: Any = None

    theta_coil: Any = None

    expected_ritfc: Any = None

    expected_rbmax: Any = None

    expected_bmaxtf: Any = None

    expected_oacdcp: Any = None

    expected_tfc_current: Any = None


@pytest.mark.parametrize(
    "tfcurrentparam",
    (
        TfCurrentParam(
            casthi=0.060000000000000012,
            ritfc=0,
            rbmax=0,
            i_tf_sup=1,
            casths_fraction=0.059999999999999998,
            tinstf=0.0080000000000000019,
            tftort=1.6395161177915356,
            bmaxtf=0,
            tfinsgap=0.01,
            tfc_sidewall_is_fraction=False,
            casths=0.05000000000000001,
            casthi_is_fraction=False,
            casthi_fraction=0.050000000000000003,
            n_tf=16,
            thicndut=0.002,
            thkcas=0.52465000000000006,
            oacdcp=8673900,
            tfareain=27.308689677971632,
            r_tf_inboard_out=4.20194118510911,
            r_tf_inboard_in=2.9939411851091102,
            tfcth=1.208,
            bt=5.3292000000000002,
            rmajor=8.8901000000000003,
            tfc_current=0,
            theta_coil=0.19634954084936207,
            expected_ritfc=236885604.60000002,
            expected_rbmax=4.0432020634751211,
            expected_bmaxtf=11.717722779177526,
            expected_oacdcp=8674367.2945641987,
            expected_tfc_current=14805350.287500001,
        ),
    ),
)
def test_tf_current(tfcurrentparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_current.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfcurrentparam: the data used to mock and assert in this test.
    :type tfcurrentparam: tfcurrentparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(tfcoil_variables, "casthi", tfcurrentparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfcurrentparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "rbmax", tfcurrentparam.rbmax)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfcurrentparam.i_tf_sup)

    monkeypatch.setattr(
        tfcoil_variables, "casths_fraction", tfcurrentparam.casths_fraction
    )

    monkeypatch.setattr(tfcoil_variables, "tinstf", tfcurrentparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "tftort", tfcurrentparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "bmaxtf", tfcurrentparam.bmaxtf)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", tfcurrentparam.tfinsgap)

    monkeypatch.setattr(
        tfcoil_variables,
        "tfc_sidewall_is_fraction",
        tfcurrentparam.tfc_sidewall_is_fraction,
    )

    monkeypatch.setattr(tfcoil_variables, "casths", tfcurrentparam.casths)

    monkeypatch.setattr(
        tfcoil_variables, "casthi_is_fraction", tfcurrentparam.casthi_is_fraction
    )

    monkeypatch.setattr(
        tfcoil_variables, "casthi_fraction", tfcurrentparam.casthi_fraction
    )

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfcurrentparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "thicndut", tfcurrentparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "thkcas", tfcurrentparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "oacdcp", tfcurrentparam.oacdcp)

    monkeypatch.setattr(tfcoil_variables, "tfareain", tfcurrentparam.tfareain)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfcurrentparam.r_tf_inboard_out
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfcurrentparam.r_tf_inboard_in
    )

    monkeypatch.setattr(build_variables, "tfcth", tfcurrentparam.tfcth)

    monkeypatch.setattr(physics_variables, "bt", tfcurrentparam.bt)

    monkeypatch.setattr(physics_variables, "rmajor", tfcurrentparam.rmajor)

    monkeypatch.setattr(sctfcoil_module, "tfc_current", tfcurrentparam.tfc_current)

    monkeypatch.setattr(sctfcoil_module, "theta_coil", tfcurrentparam.theta_coil)

    sctfcoil.tf_current()

    assert tfcoil_variables.ritfc == pytest.approx(tfcurrentparam.expected_ritfc)

    assert tfcoil_variables.rbmax == pytest.approx(tfcurrentparam.expected_rbmax)

    assert tfcoil_variables.bmaxtf == pytest.approx(tfcurrentparam.expected_bmaxtf)

    assert tfcoil_variables.oacdcp == pytest.approx(tfcurrentparam.expected_oacdcp)

    assert sctfcoil_module.tfc_current == pytest.approx(
        tfcurrentparam.expected_tfc_current
    )


class TfGlobalGeometryParam(NamedTuple):

    r_tf_outboard_mid: Any = None

    r_cp_top: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    tfthko: Any = None

    tfareain: Any = None

    ritfc: Any = None

    tftort: Any = None

    n_tf: Any = None

    arealeg: Any = None

    i_tf_sup: Any = None

    dztop: Any = None

    i_tf_case_geom: Any = None

    itart: Any = None

    kappa: Any = None

    rminor: Any = None

    h_cp_top: Any = None

    r_tf_outboard_in: Any = None

    r_tf_outboard_out: Any = None

    theta_coil: Any = None

    tan_theta_coil: Any = None

    expected_tfareain: Any = None

    expected_tftort: Any = None

    expected_arealeg: Any = None

    expected_r_tf_outboard_in: Any = None

    expected_r_tf_outboard_out: Any = None

    expected_theta_coil: Any = None

    expected_tan_theta_coil: Any = None


@pytest.mark.parametrize(
    "tfglobalgeometryparam",
    (
        TfGlobalGeometryParam(
            r_tf_outboard_mid=16.519405859443332,
            r_cp_top=4.20194118510911,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            tfthko=1.208,
            tfareain=0,
            ritfc=0,
            tftort=1,
            n_tf=16,
            arealeg=0,
            i_tf_sup=1,
            dztop=0,
            i_tf_case_geom=0,
            itart=0,
            kappa=1.8480000000000001,
            rminor=2.8677741935483869,
            h_cp_top=0,
            r_tf_outboard_in=0,
            r_tf_outboard_out=0,
            theta_coil=0,
            tan_theta_coil=0,
            expected_tfareain=27.308689677971632,
            expected_tftort=1.6395161177915356,
            expected_arealeg=1.9805354702921749,
            expected_r_tf_outboard_in=15.915405859443332,
            expected_r_tf_outboard_out=17.123405859443331,
            expected_theta_coil=0.19634954084936207,
            expected_tan_theta_coil=0.19891236737965801,
        ),
        TfGlobalGeometryParam(
            r_tf_outboard_mid=17.063351291812893,
            r_cp_top=4.4822055399518357,
            r_tf_inboard_in=2.9538679176819831,
            r_tf_inboard_out=4.4822055399518357,
            tfthko=1.5283376222698528,
            tfareain=35.703669036223495,
            ritfc=241812532.66279837,
            tftort=1.7488698442633552,
            n_tf=16,
            arealeg=2.6728635794409041,
            i_tf_sup=1,
            dztop=0,
            i_tf_case_geom=0,
            itart=0,
            kappa=1.8480000000000001,
            rminor=2.9620024998595755,
            h_cp_top=0,
            r_tf_outboard_in=16.299182480677967,
            r_tf_outboard_out=17.827520102947819,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            expected_tfareain=35.703669036223495,
            expected_tftort=1.7488698442633552,
            expected_arealeg=2.6728635794409041,
            expected_r_tf_outboard_in=16.299182480677967,
            expected_r_tf_outboard_out=17.827520102947819,
            expected_theta_coil=0.19634954084936207,
            expected_tan_theta_coil=0.19891236737965801,
        ),
    ),
)
def test_tf_global_geometry(tfglobalgeometryparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_global_geometry.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfglobalgeometryparam: the data used to mock and assert in this test.
    :type tfglobalgeometryparam: tfglobalgeometryparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(
        build_variables, "r_tf_outboard_mid", tfglobalgeometryparam.r_tf_outboard_mid
    )

    monkeypatch.setattr(build_variables, "r_cp_top", tfglobalgeometryparam.r_cp_top)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfglobalgeometryparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfglobalgeometryparam.r_tf_inboard_out
    )

    monkeypatch.setattr(build_variables, "tfthko", tfglobalgeometryparam.tfthko)

    monkeypatch.setattr(tfcoil_variables, "tfareain", tfglobalgeometryparam.tfareain)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfglobalgeometryparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "tftort", tfglobalgeometryparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfglobalgeometryparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "arealeg", tfglobalgeometryparam.arealeg)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfglobalgeometryparam.i_tf_sup)

    monkeypatch.setattr(tfcoil_variables, "dztop", tfglobalgeometryparam.dztop)

    monkeypatch.setattr(
        tfcoil_variables, "i_tf_case_geom", tfglobalgeometryparam.i_tf_case_geom
    )

    monkeypatch.setattr(physics_variables, "itart", tfglobalgeometryparam.itart)

    monkeypatch.setattr(physics_variables, "kappa", tfglobalgeometryparam.kappa)

    monkeypatch.setattr(physics_variables, "rminor", tfglobalgeometryparam.rminor)

    monkeypatch.setattr(sctfcoil_module, "h_cp_top", tfglobalgeometryparam.h_cp_top)

    monkeypatch.setattr(
        sctfcoil_module, "r_tf_outboard_in", tfglobalgeometryparam.r_tf_outboard_in
    )

    monkeypatch.setattr(
        sctfcoil_module, "r_tf_outboard_out", tfglobalgeometryparam.r_tf_outboard_out
    )

    monkeypatch.setattr(sctfcoil_module, "theta_coil", tfglobalgeometryparam.theta_coil)

    monkeypatch.setattr(
        sctfcoil_module, "tan_theta_coil", tfglobalgeometryparam.tan_theta_coil
    )

    sctfcoil.tf_global_geometry()

    assert tfcoil_variables.tfareain == pytest.approx(
        tfglobalgeometryparam.expected_tfareain
    )

    assert tfcoil_variables.tftort == pytest.approx(
        tfglobalgeometryparam.expected_tftort
    )

    assert tfcoil_variables.arealeg == pytest.approx(
        tfglobalgeometryparam.expected_arealeg
    )

    assert sctfcoil_module.r_tf_outboard_in == pytest.approx(
        tfglobalgeometryparam.expected_r_tf_outboard_in
    )

    assert sctfcoil_module.r_tf_outboard_out == pytest.approx(
        tfglobalgeometryparam.expected_r_tf_outboard_out
    )

    assert sctfcoil_module.theta_coil == pytest.approx(
        tfglobalgeometryparam.expected_theta_coil
    )

    assert sctfcoil_module.tan_theta_coil == pytest.approx(
        tfglobalgeometryparam.expected_tan_theta_coil
    )


@pytest.mark.parametrize(
    "a, b, expected_circumference",
    (
        (2.667950e9, 6.782819e8, 11464316399.111176),
        (4.7186039761812131, 3.6192586838709673, 26.308134540723429),
    ),
    ids=["johndcook", "baseline_2018"],
)
def test_circumference(a, b, expected_circumference, sctfcoil):
    """Unit test for the sctfcoil circumference routine.

    This unit test uses values from an external blog referenced in the
    routine header (https://www.johndcook.com/blog/2013/05/05/ramanujan-circumference-ellipse/)
    as well as results obtained from baseline 2018.

    :param a: the value of a (x/a)^2...  in the formula of an ellipse
    :type a: float

    :param b: the value of b ...(y/b)^2...  in the formula of an ellipse
    :type b: float

    :param expected_circumference: the expected result of the routine given inputs a and b
    :type expected_circumference: float

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """
    assert sctfcoil.circumference(a, b) == pytest.approx(expected_circumference)


class ResTfInternalGeomParam(NamedTuple):

    n_tf_turn: Any = None

    thicndut: Any = None

    thkcas: Any = None

    dr_tf_wp: Any = None

    tftort: Any = None

    tfareain: Any = None

    ritfc: Any = None

    fcoolcp: Any = None

    cpttf: Any = None

    cdtfleg: Any = None

    casthi: Any = None

    aiwp: Any = None

    acasetf: Any = None

    tinstf: Any = None

    n_tf: Any = None

    tfthko: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    r_cp_top: Any = None

    itart: Any = None

    expected_n_tf_turn: Any = None

    expected_cpttf: Any = None

    expected_cdtfleg: Any = None

    expected_aiwp: Any = None

    expected_acasetf: Any = None


@pytest.mark.parametrize(
    "restfinternalgeomparam",
    (
        ResTfInternalGeomParam(
            n_tf_turn=0,
            thicndut=0.00080000000000000004,
            thkcas=0,
            dr_tf_wp=0.15483000000000002,
            tftort=0.45367650933034859,
            tfareain=0.0753112923616783,
            ritfc=25500000,
            fcoolcp=0.12725,
            cpttf=70000,
            cdtfleg=0,
            casthi=0.0077415000000000019,
            aiwp=0,
            acasetf=0,
            tinstf=0,
            n_tf=12,
            tfthko=0.15483000000000002,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            itart=1,
            expected_n_tf_turn=1,
            expected_cpttf=2125000,
            expected_cdtfleg=421788350.27812088,
            expected_aiwp=0.00030678028680367151,
            expected_acasetf=0.00061190425043863676,
        ),
        ResTfInternalGeomParam(
            n_tf_turn=1,
            thicndut=0.00080000000000000004,
            thkcas=0,
            dr_tf_wp=0.14708850000000001,
            tftort=0.44435902370665786,
            tfareain=0.0753112923616783,
            ritfc=25500000,
            fcoolcp=0.12725,
            cpttf=2125000,
            cdtfleg=421788350.27812088,
            casthi=0.0077415000000000019,
            aiwp=0.00030678028680367151,
            acasetf=0.00061190425043863676,
            tinstf=0,
            n_tf=12,
            tfthko=0.15483000000000002,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            itart=1,
            expected_n_tf_turn=1,
            expected_cpttf=2125000,
            expected_cdtfleg=430664525.98439038,
            expected_aiwp=0.00029439388680367086,
            expected_acasetf=0.00061190425043863676,
        ),
    ),
)
def test_res_tf_internal_geom(restfinternalgeomparam, monkeypatch):
    """
        Automatically generated Regression Unit Test for res_tf_internal_geom.

        This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

        :param restfinternalgeomparam: the data used to mock and assert in this test.
        :type restfinternalgeomparam: restfinternalgeomparam

        :param monkeypatch: pytest fixture used to mock module/class variables
        :type monkeypatch: _pytest.monkeypatch.monkeypatch
    <<<<<<< HEAD
    <<<<<<< HEAD

        :param sctfcoil: initialised Sctfcoil object
        :type sctfcoil: process.sctfcoil.Sctfcoil
    =======
    >>>>>>> abbc297c0c1c16e1a10871ee0a3706571598c0d0
    =======
    >>>>>>> abbc297c0c1c16e1a10871ee0a3706571598c0d0
    """

    monkeypatch.setattr(tfcoil_variables, "n_tf_turn", restfinternalgeomparam.n_tf_turn)

    monkeypatch.setattr(tfcoil_variables, "thicndut", restfinternalgeomparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "thkcas", restfinternalgeomparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", restfinternalgeomparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "tftort", restfinternalgeomparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "tfareain", restfinternalgeomparam.tfareain)

    monkeypatch.setattr(tfcoil_variables, "ritfc", restfinternalgeomparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "fcoolcp", restfinternalgeomparam.fcoolcp)

    monkeypatch.setattr(tfcoil_variables, "cpttf", restfinternalgeomparam.cpttf)

    monkeypatch.setattr(tfcoil_variables, "cdtfleg", restfinternalgeomparam.cdtfleg)

    monkeypatch.setattr(tfcoil_variables, "casthi", restfinternalgeomparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "aiwp", restfinternalgeomparam.aiwp)

    monkeypatch.setattr(tfcoil_variables, "acasetf", restfinternalgeomparam.acasetf)

    monkeypatch.setattr(tfcoil_variables, "tinstf", restfinternalgeomparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "n_tf", restfinternalgeomparam.n_tf)

    monkeypatch.setattr(build_variables, "tfthko", restfinternalgeomparam.tfthko)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", restfinternalgeomparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", restfinternalgeomparam.r_tf_inboard_out
    )

    monkeypatch.setattr(build_variables, "r_cp_top", restfinternalgeomparam.r_cp_top)

    monkeypatch.setattr(physics_variables, "itart", restfinternalgeomparam.itart)


class TfResHeatingParam(NamedTuple):
    rhocp: Any = None
    tlegav: Any = None
    thicndut: Any = None
    th_joint_contact: Any = None
    rhotfleg: Any = None
    vol_cond_cp: Any = None
    n_tf_turn: Any = None
    thkcas: Any = None
    tftort: Any = None
    tfleng: Any = None
    tflegres: Any = None
    tcpav: Any = None
    arealeg: Any = None
    ritfc: Any = None
    rho_tf_joints: Any = None
    presleg: Any = None
    prescp: Any = None
    pres_joints: Any = None
    n_tf_joints_contact: Any = None
    n_tf_joints: Any = None
    n_tf: Any = None
    i_tf_sup: Any = None
    frholeg: Any = None
    frhocp: Any = None
    fcoolcp: Any = None
    casthi: Any = None
    a_cp_cool: Any = None
    fcoolleg: Any = None
    i_cp_joints: Any = None
    tinstf: Any = None
    tfthko: Any = None
    tfcth: Any = None
    r_cp_top: Any = None
    hmax: Any = None
    r_tf_inboard_in: Any = None
    r_tf_inboard_out: Any = None
    itart: Any = None
    h_cp_top: Any = None
    is_leg_cp_temp_same: Any = None
    expected_rhocp: Any = None
    expected_rhotfleg: Any = None
    expected_vol_cond_cp: Any = None
    expected_tflegres: Any = None
    expected_presleg: Any = None
    expected_prescp: Any = None
    expected_pres_joints: Any = None
    expected_a_cp_cool: Any = None
    expected_is_leg_cp_temp_same: Any = None


@pytest.mark.parametrize(
    "restfinternalgeomparam",
    (
        ResTfInternalGeomParam(
            n_tf_turn=0,
            thicndut=0.00080000000000000004,
            thkcas=0,
            dr_tf_wp=0.15483000000000002,
            tftort=0.45367650933034859,
            tfareain=0.0753112923616783,
            ritfc=25500000,
            fcoolcp=0.12725,
            cpttf=70000,
            cdtfleg=0,
            casthi=0.0077415000000000019,
            aiwp=0,
            acasetf=0,
            tinstf=0,
            n_tf=12,
            tfthko=0.15483000000000002,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            itart=1,
            expected_n_tf_turn=1,
            expected_cpttf=2125000,
            expected_cdtfleg=421788350.27812088,
            expected_aiwp=0.00030678028680367151,
            expected_acasetf=0.00061190425043863676,
        ),
        ResTfInternalGeomParam(
            n_tf_turn=1,
            thicndut=0.00080000000000000004,
            thkcas=0,
            dr_tf_wp=0.14708850000000001,
            tftort=0.44435902370665786,
            tfareain=0.0753112923616783,
            ritfc=25500000,
            fcoolcp=0.12725,
            cpttf=2125000,
            cdtfleg=421788350.27812088,
            casthi=0.0077415000000000019,
            aiwp=0.00030678028680367151,
            acasetf=0.00061190425043863676,
            tinstf=0,
            n_tf=12,
            tfthko=0.15483000000000002,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            itart=1,
            expected_n_tf_turn=1,
            expected_cpttf=2125000,
            expected_cdtfleg=430664525.98439038,
            expected_aiwp=0.00029439388680367086,
            expected_acasetf=0.00061190425043863676,
        ),
    ),
)
def test_res_tf_internal_geom(restfinternalgeomparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for res_tf_internal_geom.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param restfinternalgeomparam: the data used to mock and assert in this test.
    :type restfinternalgeomparam: restfinternalgeomparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(tfcoil_variables, "n_tf_turn", restfinternalgeomparam.n_tf_turn)

    monkeypatch.setattr(tfcoil_variables, "thicndut", restfinternalgeomparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "thkcas", restfinternalgeomparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", restfinternalgeomparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "tftort", restfinternalgeomparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "tfareain", restfinternalgeomparam.tfareain)

    monkeypatch.setattr(tfcoil_variables, "ritfc", restfinternalgeomparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "fcoolcp", restfinternalgeomparam.fcoolcp)

    monkeypatch.setattr(tfcoil_variables, "cpttf", restfinternalgeomparam.cpttf)

    monkeypatch.setattr(tfcoil_variables, "cdtfleg", restfinternalgeomparam.cdtfleg)

    monkeypatch.setattr(tfcoil_variables, "casthi", restfinternalgeomparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "aiwp", restfinternalgeomparam.aiwp)

    monkeypatch.setattr(tfcoil_variables, "acasetf", restfinternalgeomparam.acasetf)

    monkeypatch.setattr(tfcoil_variables, "tinstf", restfinternalgeomparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "n_tf", restfinternalgeomparam.n_tf)

    monkeypatch.setattr(build_variables, "tfthko", restfinternalgeomparam.tfthko)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", restfinternalgeomparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", restfinternalgeomparam.r_tf_inboard_out
    )

    monkeypatch.setattr(build_variables, "r_cp_top", restfinternalgeomparam.r_cp_top)

    monkeypatch.setattr(physics_variables, "itart", restfinternalgeomparam.itart)

    sctfcoil.res_tf_internal_geom()

    assert tfcoil_variables.n_tf_turn == pytest.approx(
        restfinternalgeomparam.expected_n_tf_turn
    )

    assert tfcoil_variables.cpttf == pytest.approx(
        restfinternalgeomparam.expected_cpttf
    )

    assert tfcoil_variables.cdtfleg == pytest.approx(
        restfinternalgeomparam.expected_cdtfleg
    )

    assert tfcoil_variables.aiwp == pytest.approx(restfinternalgeomparam.expected_aiwp)

    assert tfcoil_variables.acasetf == pytest.approx(
        restfinternalgeomparam.expected_acasetf
    )

    assert tfcoil_variables.n_tf_turn == pytest.approx(
        restfinternalgeomparam.expected_n_tf_turn
    )

    assert tfcoil_variables.cpttf == pytest.approx(
        restfinternalgeomparam.expected_cpttf
    )

    assert tfcoil_variables.cdtfleg == pytest.approx(
        restfinternalgeomparam.expected_cdtfleg
    )

    assert tfcoil_variables.aiwp == pytest.approx(restfinternalgeomparam.expected_aiwp)

    assert tfcoil_variables.acasetf == pytest.approx(
        restfinternalgeomparam.expected_acasetf
    )

    itart: Any = None

    h_cp_top: Any = None

    is_leg_cp_temp_same: Any = None

    expected_rhocp: Any = None

    expected_rhotfleg: Any = None

    expected_vol_cond_cp: Any = None

    expected_tflegres: Any = None

    expected_presleg: Any = None

    expected_prescp: Any = None

    expected_pres_joints: Any = None

    expected_a_cp_cool: Any = None

    expected_is_leg_cp_temp_same: Any = None


@pytest.mark.parametrize(
    "tfresheatingparam",
    (
        TfResHeatingParam(
            rhocp=0,
            tlegav=-1,
            thicndut=0.00080000000000000004,
            th_joint_contact=0.029999999999999999,
            rhotfleg=0,
            vol_cond_cp=0,
            n_tf_turn=1,
            thkcas=0,
            tftort=0.45367650933034859,
            tfleng=15.582502857142856,
            tflegres=0,
            tcpav=347.13,
            arealeg=0.070242733939617885,
            ritfc=25500000,
            rho_tf_joints=2.5000000000000002e-10,
            presleg=0,
            prescp=0,
            pres_joints=0,
            n_tf_joints_contact=6,
            n_tf_joints=4,
            n_tf=12,
            i_tf_sup=0,
            frholeg=1,
            frhocp=1,
            fcoolcp=0.12725,
            casthi=0.0077415000000000019,
            a_cp_cool=0,
            fcoolleg=0.20000000000000001,
            i_cp_joints=1,
            tinstf=0,
            tfthko=0.15483000000000002,
            tfcth=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            itart=1,
            h_cp_top=2.6714285714285717,
            is_leg_cp_temp_same=0,
            expected_rhocp=2.1831760869565221e-08,
            expected_rhotfleg=2.1831760869565221e-08,
            expected_vol_cond_cp=12.020160732580297,
            expected_tflegres=6.1387543007600344e-06,
            expected_presleg=332643748.67243439,
            expected_prescp=470083798.99090022,
            expected_pres_joints=1944336.7995005273,
            expected_a_cp_cool=0.00068328705812121333,
            expected_is_leg_cp_temp_same=1,
        ),
        TfResHeatingParam(
            rhocp=2.1831760869565221e-08,
            tlegav=-1,
            thicndut=0.00080000000000000004,
            th_joint_contact=0.029999999999999999,
            rhotfleg=2.1831760869565221e-08,
            vol_cond_cp=12.020160732580297,
            n_tf_turn=1,
            thkcas=0,
            tftort=0.44435902370665786,
            tfleng=15.654502857142857,
            tflegres=6.1387543007600344e-06,
            tcpav=347.13,
            arealeg=0.068800107640501845,
            ritfc=25500000,
            rho_tf_joints=2.5000000000000002e-10,
            presleg=332643748.67243439,
            prescp=470083798.99090022,
            pres_joints=1944336.7995005273,
            n_tf_joints_contact=6,
            n_tf_joints=4,
            n_tf=12,
            i_tf_sup=0,
            frholeg=1,
            frhocp=1,
            fcoolcp=0.12725,
            casthi=0.0077415000000000019,
            a_cp_cool=0.00068328705812121333,
            fcoolleg=0.20000000000000001,
            i_cp_joints=1,
            tinstf=0,
            tfthko=0.15483000000000002,
            tfcth=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            itart=1,
            h_cp_top=2.6714285714285717,
            is_leg_cp_temp_same=1,
            expected_rhocp=2.1831760869565221e-08,
            expected_rhotfleg=2.1831760869565221e-08,
            expected_vol_cond_cp=11.545770024935592,
            expected_tflegres=6.2969005770928158e-06,
            expected_presleg=341213300.02121693,
            expected_prescp=475710489.56122422,
            expected_pres_joints=1944336.7995005273,
            expected_a_cp_cool=0.00068328705812121333,
            expected_is_leg_cp_temp_same=1,
        ),
    ),
)
def test_tf_res_heating(tfresheatingparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_res_heating.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param tfresheatingparam: the data used to mock and assert in this test.
    :type tfresheatingparam: tfresheatingparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "rhocp", tfresheatingparam.rhocp)

    monkeypatch.setattr(tfcoil_variables, "tlegav", tfresheatingparam.tlegav)

    monkeypatch.setattr(tfcoil_variables, "thicndut", tfresheatingparam.thicndut)

    monkeypatch.setattr(
        tfcoil_variables, "th_joint_contact", tfresheatingparam.th_joint_contact
    )

    monkeypatch.setattr(tfcoil_variables, "rhotfleg", tfresheatingparam.rhotfleg)

    monkeypatch.setattr(tfcoil_variables, "vol_cond_cp", tfresheatingparam.vol_cond_cp)

    monkeypatch.setattr(tfcoil_variables, "n_tf_turn", tfresheatingparam.n_tf_turn)

    monkeypatch.setattr(tfcoil_variables, "thkcas", tfresheatingparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "tftort", tfresheatingparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "tfleng", tfresheatingparam.tfleng)

    monkeypatch.setattr(tfcoil_variables, "tflegres", tfresheatingparam.tflegres)

    monkeypatch.setattr(tfcoil_variables, "tcpav", tfresheatingparam.tcpav)

    monkeypatch.setattr(tfcoil_variables, "arealeg", tfresheatingparam.arealeg)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfresheatingparam.ritfc)

    monkeypatch.setattr(
        tfcoil_variables, "rho_tf_joints", tfresheatingparam.rho_tf_joints
    )

    monkeypatch.setattr(tfcoil_variables, "presleg", tfresheatingparam.presleg)

    monkeypatch.setattr(tfcoil_variables, "prescp", tfresheatingparam.prescp)

    monkeypatch.setattr(tfcoil_variables, "pres_joints", tfresheatingparam.pres_joints)

    monkeypatch.setattr(
        tfcoil_variables, "n_tf_joints_contact", tfresheatingparam.n_tf_joints_contact
    )

    monkeypatch.setattr(tfcoil_variables, "n_tf_joints", tfresheatingparam.n_tf_joints)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfresheatingparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfresheatingparam.i_tf_sup)

    monkeypatch.setattr(tfcoil_variables, "frholeg", tfresheatingparam.frholeg)

    monkeypatch.setattr(tfcoil_variables, "frhocp", tfresheatingparam.frhocp)

    monkeypatch.setattr(tfcoil_variables, "fcoolcp", tfresheatingparam.fcoolcp)

    monkeypatch.setattr(tfcoil_variables, "casthi", tfresheatingparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "a_cp_cool", tfresheatingparam.a_cp_cool)

    monkeypatch.setattr(tfcoil_variables, "fcoolleg", tfresheatingparam.fcoolleg)

    monkeypatch.setattr(tfcoil_variables, "i_cp_joints", tfresheatingparam.i_cp_joints)

    monkeypatch.setattr(tfcoil_variables, "tinstf", tfresheatingparam.tinstf)

    monkeypatch.setattr(build_variables, "tfthko", tfresheatingparam.tfthko)

    monkeypatch.setattr(build_variables, "tfcth", tfresheatingparam.tfcth)

    monkeypatch.setattr(build_variables, "r_cp_top", tfresheatingparam.r_cp_top)

    monkeypatch.setattr(build_variables, "hmax", tfresheatingparam.hmax)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfresheatingparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfresheatingparam.r_tf_inboard_out
    )

    monkeypatch.setattr(physics_variables, "itart", tfresheatingparam.itart)

    monkeypatch.setattr(sctfcoil_module, "h_cp_top", tfresheatingparam.h_cp_top)

    monkeypatch.setattr(
        sctfcoil_module, "is_leg_cp_temp_same", tfresheatingparam.is_leg_cp_temp_same
    )

    sctfcoil.tf_res_heating()

    assert tfcoil_variables.rhocp == pytest.approx(tfresheatingparam.expected_rhocp)

    assert tfcoil_variables.rhotfleg == pytest.approx(
        tfresheatingparam.expected_rhotfleg
    )

    assert tfcoil_variables.vol_cond_cp == pytest.approx(
        tfresheatingparam.expected_vol_cond_cp
    )

    assert tfcoil_variables.tflegres == pytest.approx(
        tfresheatingparam.expected_tflegres
    )

    assert tfcoil_variables.presleg == pytest.approx(tfresheatingparam.expected_presleg)

    assert tfcoil_variables.prescp == pytest.approx(tfresheatingparam.expected_prescp)

    assert tfcoil_variables.pres_joints == pytest.approx(
        tfresheatingparam.expected_pres_joints
    )

    assert tfcoil_variables.a_cp_cool == pytest.approx(
        tfresheatingparam.expected_a_cp_cool
    )

    assert sctfcoil_module.is_leg_cp_temp_same == pytest.approx(
        tfresheatingparam.expected_is_leg_cp_temp_same
    )


class CpostParam(NamedTuple):

    n_tf: Any = None

    hmax: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    r_cp_top: Any = None

    ztop: Any = None

    hmaxi: Any = None

    cas_in_th: Any = None

    cas_out_th: Any = None

    gr_ins_th: Any = None

    ins_th: Any = None

    n_tf_turn: Any = None

    curr: Any = None

    rho: Any = None

    fcool: Any = None

    expected_vol_ins_cp: Any = None

    expected_vol_gr_ins_cp: Any = None

    expected_vol_case_cp: Any = None

    expected_respow: Any = None

    expected_vol_cond_cp: Any = None

    expected_a_cp_cool: Any = None


@pytest.mark.parametrize(
    "cpostparam",
    (
        CpostParam(
            n_tf=12,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            ztop=2.6714285714285717,
            hmaxi=4.5762585714285713,
            cas_in_th=0,
            cas_out_th=0.0077415000000000019,
            gr_ins_th=0,
            ins_th=0.00080000000000000004,
            n_tf_turn=1,
            curr=25500000,
            rho=2.1831760869565221e-08,
            fcool=0.12725,
            expected_vol_ins_cp=0.12917075053120922,
            expected_vol_gr_ins_cp=0,
            expected_vol_case_cp=0.12791418544773489,
            expected_respow=470083798.99090022,
            expected_vol_cond_cp=12.020160732580297,
            expected_a_cp_cool=0.00068328705812121333,
        ),
        CpostParam(
            n_tf=12,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            ztop=2.6714285714285717,
            hmaxi=4.5762585714285713,
            cas_in_th=0,
            cas_out_th=0.0077415000000000019,
            gr_ins_th=0,
            ins_th=0.00080000000000000004,
            n_tf_turn=1,
            curr=25500000,
            rho=2.1831760869565221e-08,
            fcool=0.12725,
            expected_vol_ins_cp=0.12679799009998483,
            expected_vol_gr_ins_cp=0,
            expected_vol_case_cp=0.12648575512245444,
            expected_respow=475710489.56122422,
            expected_vol_cond_cp=11.545770024935592,
            expected_a_cp_cool=0.00068328705812121333,
        ),
    ),
)
def test_cpost(cpostparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for cpost.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param cpostparam: the data used to mock and assert in this test.
    :type cpostparam: cpostparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "n_tf", cpostparam.n_tf)

    monkeypatch.setattr(build_variables, "hmax", cpostparam.hmax)

    (
        a_cp_cool,
        vol_cond_cp,
        respow,
        vol_ins_cp,
        vol_case_cp,
        vol_gr_ins_cp,
    ) = sctfcoil.cpost(
        r_tf_inboard_in=cpostparam.r_tf_inboard_in,
        r_tf_inboard_out=cpostparam.r_tf_inboard_out,
        r_cp_top=cpostparam.r_cp_top,
        ztop=cpostparam.ztop,
        hmaxi=cpostparam.hmaxi,
        cas_in_th=cpostparam.cas_in_th,
        cas_out_th=cpostparam.cas_out_th,
        gr_ins_th=cpostparam.gr_ins_th,
        ins_th=cpostparam.ins_th,
        n_tf_turn=cpostparam.n_tf_turn,
        curr=cpostparam.curr,
        rho=cpostparam.rho,
        fcool=cpostparam.fcool,
    )

    assert vol_ins_cp == pytest.approx(cpostparam.expected_vol_ins_cp)

    assert vol_gr_ins_cp == pytest.approx(cpostparam.expected_vol_gr_ins_cp)

    assert vol_case_cp == pytest.approx(cpostparam.expected_vol_case_cp)

    assert respow == pytest.approx(cpostparam.expected_respow)

    assert vol_cond_cp == pytest.approx(cpostparam.expected_vol_cond_cp)

    assert a_cp_cool == pytest.approx(cpostparam.expected_a_cp_cool)


class TfFieldAndForceParam(NamedTuple):

    rminor: Any = None

    rmajor: Any = None

    bt: Any = None

    itart: Any = None

    r_tf_outboard_mid: Any = None

    r_vv_inboard_out: Any = None

    r_tf_inboard_mid: Any = None

    r_cp_top: Any = None

    vforce: Any = None

    n_tf: Any = None

    taucq: Any = None

    sigvvall: Any = None

    cforce: Any = None

    ritfc: Any = None

    bmaxtf: Any = None

    i_tf_sup: Any = None

    f_vforce_inboard: Any = None

    vforce_outboard: Any = None

    tinstf: Any = None

    thicndut: Any = None

    dr_tf_wp: Any = None

    tfinsgap: Any = None

    i_cp_joints: Any = None

    casthi: Any = None

    r_tf_outboard_in: Any = None

    r_wp_inner: Any = None

    r_wp_outer: Any = None

    vforce_inboard_tot: Any = None

    expected_vforce: Any = None

    expected_cforce: Any = None

    expected_f_vforce_inboard: Any = None

    expected_vforce_outboard: Any = None

    expected_vforce_inboard_tot: Any = None


@pytest.mark.parametrize(
    "tffieldandforceparam",
    (
        TfFieldAndForceParam(
            rminor=0.97142857142857153,
            rmajor=1.7000000000000002,
            bt=3,
            itart=1,
            r_tf_outboard_mid=4.1688435714285719,
            r_vv_inboard_out=0.20483000000000001,
            r_tf_inboard_mid=0.077415000000000012,
            r_cp_top=0.87643571428571443,
            vforce=0,
            n_tf=12,
            taucq=30,
            sigvvall=93000000,
            cforce=0,
            ritfc=25500000,
            bmaxtf=34.862617362267024,
            i_tf_sup=0,
            f_vforce_inboard=0.5,
            vforce_outboard=0,
            tinstf=0,
            thicndut=0.00080000000000000004,
            dr_tf_wp=0.15483000000000002,
            tfinsgap=0.01,
            i_cp_joints=1,
            casthi=0.0077415000000000019,
            r_tf_outboard_in=4.0914285714285716,
            r_wp_inner=0,
            r_wp_outer=0.14708850000000001,
            vforce_inboard_tot=0,
            expected_vforce=12380916.66459452,
            expected_cforce=37041530.947408713,
            expected_f_vforce_inboard=0.59539634897566385,
            expected_vforce_outboard=8413494.7991220243,
            expected_vforce_inboard_tot=148570999.97513425,
        ),
        TfFieldAndForceParam(
            rminor=0.97142857142857153,
            rmajor=1.7000000000000002,
            bt=3,
            itart=1,
            r_tf_outboard_mid=4.1868435714285717,
            r_vv_inboard_out=0.20483000000000001,
            r_tf_inboard_mid=0.077415000000000012,
            r_cp_top=0.85843571428571441,
            vforce=12380916.66459452,
            n_tf=12,
            taucq=30,
            sigvvall=93000000,
            cforce=37041530.947408713,
            ritfc=25500000,
            bmaxtf=34.862617362267024,
            i_tf_sup=0,
            f_vforce_inboard=0.59539634897566385,
            vforce_outboard=8413494.7991220243,
            tinstf=0,
            thicndut=0.00080000000000000004,
            dr_tf_wp=0.14708850000000001,
            tfinsgap=0.01,
            i_cp_joints=1,
            casthi=0.0077415000000000019,
            r_tf_outboard_in=4.1094285714285714,
            r_wp_inner=0,
            r_wp_outer=0.14708850000000001,
            vforce_inboard_tot=148570999.97513425,
            expected_vforce=12268469.138442248,
            expected_cforce=37041530.947408713,
            expected_f_vforce_inboard=0.58932254522566518,
            expected_vforce_outboard=8549450.0771621168,
            expected_vforce_inboard_tot=147221629.66130698,
        ),
    ),
)
def test_tf_field_and_force(tffieldandforceparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_field_and_force.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param tffieldandforceparam: the data used to mock and assert in this test.
    :type tffieldandforceparam: tffieldandforceparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(physics_variables, "rminor", tffieldandforceparam.rminor)

    monkeypatch.setattr(physics_variables, "rmajor", tffieldandforceparam.rmajor)

    monkeypatch.setattr(physics_variables, "bt", tffieldandforceparam.bt)

    monkeypatch.setattr(physics_variables, "itart", tffieldandforceparam.itart)

    monkeypatch.setattr(
        build_variables, "r_tf_outboard_mid", tffieldandforceparam.r_tf_outboard_mid
    )

    monkeypatch.setattr(
        build_variables, "r_vv_inboard_out", tffieldandforceparam.r_vv_inboard_out
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_mid", tffieldandforceparam.r_tf_inboard_mid
    )

    monkeypatch.setattr(build_variables, "r_cp_top", tffieldandforceparam.r_cp_top)

    monkeypatch.setattr(tfcoil_variables, "vforce", tffieldandforceparam.vforce)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tffieldandforceparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "taucq", tffieldandforceparam.taucq)

    monkeypatch.setattr(tfcoil_variables, "sigvvall", tffieldandforceparam.sigvvall)

    monkeypatch.setattr(tfcoil_variables, "cforce", tffieldandforceparam.cforce)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tffieldandforceparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "bmaxtf", tffieldandforceparam.bmaxtf)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tffieldandforceparam.i_tf_sup)

    monkeypatch.setattr(
        tfcoil_variables, "f_vforce_inboard", tffieldandforceparam.f_vforce_inboard
    )

    monkeypatch.setattr(
        tfcoil_variables, "vforce_outboard", tffieldandforceparam.vforce_outboard
    )

    monkeypatch.setattr(tfcoil_variables, "tinstf", tffieldandforceparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "thicndut", tffieldandforceparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", tffieldandforceparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", tffieldandforceparam.tfinsgap)

    monkeypatch.setattr(
        tfcoil_variables, "i_cp_joints", tffieldandforceparam.i_cp_joints
    )

    monkeypatch.setattr(tfcoil_variables, "casthi", tffieldandforceparam.casthi)

    monkeypatch.setattr(
        sctfcoil_module, "r_tf_outboard_in", tffieldandforceparam.r_tf_outboard_in
    )

    monkeypatch.setattr(sctfcoil_module, "r_wp_inner", tffieldandforceparam.r_wp_inner)

    monkeypatch.setattr(sctfcoil_module, "r_wp_outer", tffieldandforceparam.r_wp_outer)

    monkeypatch.setattr(
        sctfcoil_module, "vforce_inboard_tot", tffieldandforceparam.vforce_inboard_tot
    )

    sctfcoil.tf_field_and_force()

    assert tfcoil_variables.vforce == pytest.approx(
        tffieldandforceparam.expected_vforce
    )

    assert tfcoil_variables.cforce == pytest.approx(
        tffieldandforceparam.expected_cforce
    )

    assert tfcoil_variables.f_vforce_inboard == pytest.approx(
        tffieldandforceparam.expected_f_vforce_inboard
    )

    assert tfcoil_variables.vforce_outboard == pytest.approx(
        tffieldandforceparam.expected_vforce_outboard
    )

    assert sctfcoil_module.vforce_inboard_tot == pytest.approx(
        tffieldandforceparam.expected_vforce_inboard_tot
    )


class TfcindParam(NamedTuple):

    yarc: Any = None

    xarc: Any = None

    tfind: Any = None

    tfthk: Any = None

    expected_yarc: Any = None

    expected_tfind: Any = None


@pytest.mark.parametrize(
    "tfcindparam",
    (
        TfcindParam(
            yarc=numpy.array(
                (
                    4.5228880258064512,
                    7.5381467096774184,
                    0,
                    -9.0730900215620327,
                    -5.4438540129372193,
                ),
                order="F",
            ),
            xarc=numpy.array(
                (
                    4.20194118510911,
                    8.316545161290323,
                    15.915405859443332,
                    8.316545161290323,
                    4.20194118510911,
                ),
                order="F",
            ),
            tfind=0,
            tfthk=1.208,
            expected_tfind=5.4453892599192845e-06,
        ),
        TfcindParam(
            yarc=numpy.array(
                (
                    4.5336880258064509,
                    7.5561467096774191,
                    0,
                    -9.0730900215620327,
                    -5.4438540129372193,
                ),
                order="F",
            ),
            xarc=numpy.array(
                (
                    4.20194118510911,
                    8.316545161290323,
                    15.915405859443332,
                    8.316545161290323,
                    4.20194118510911,
                ),
                order="F",
            ),
            tfind=5.4524893280368181e-06,
            tfthk=1.208,
            expected_tfind=5.4524893280368181e-06,
        ),
    ),
)
def test_tfcind(tfcindparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tfcind.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfcindparam: the data used to mock and assert in this test.
    :type tfcindparam: tfcindparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "yarc", tfcindparam.yarc)

    monkeypatch.setattr(tfcoil_variables, "xarc", tfcindparam.xarc)

    monkeypatch.setattr(tfcoil_variables, "tfind", tfcindparam.tfind)

    sctfcoil.tfcind(tfthk=tfcindparam.tfthk)

    assert tfcoil_variables.tfind == pytest.approx(tfcindparam.expected_tfind)
