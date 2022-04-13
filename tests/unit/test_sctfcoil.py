import pytest
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
