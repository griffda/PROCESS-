###############################################################################
#                                                                             #
#                        UnitFort Physics Module Test                         #
#                                                                             #
#   Tests on methods within the Physics module contained in the file:         #
#                                                                             #
#   'source/fortran/physics.f90'                                              #
#                                                                             #
#   within the PROCESS application libraries.                                 #
#                                                                             #
#   @author :   K. Zarebski <kristian.zarebski@ukaea.uk>                      #
#   @date   :   last modified 2020-12-02                                      #
#                                                                             #
#   WARNING: Due to many PROCESS global variables being updated by these      #
#            methods some variables have to be reinitialised. However this    #
#            is only performed on those which were manually altered. As such  #
#            it is highly recommended that the UnitFort tests be run on their #
#            own as there is no guarantee they will not affect others using   #
#            the PROCESS python library.                                      #
#                                                                             #
#            Run using the dedicated marker: pytest -m unitfort               # 
#                                                                             #                                                                            #
#   DISCLAIMER                                                                #
#   ----------                                                                #
#                                                                             #
#   This test was created using the outputs of the UnitFort test parser       #
#   running on a FORTRAN 90 script.The method is 'naive' having no knowledge  #
#   of what the purpose of a method is, and knowing only the variable type    #
#   but not the constraints behind choice of the value of the variable.       #
#   As such the results should NOT be taken as indication of the correct      #
#   values a method returns, but rather used for the purposes of              #
#   regression testing.                                                       #
#                                                                             #
#   UnitFort repository: https://git.ccfe.ac.uk/kzarebsk/unitfort             #
#                                                                             #
###############################################################################

import pytest
import numpy as np
from process.fortran import physics_module


@pytest.mark.unitfort
def test_diamagnetic_fraction_scene(uft_data):
    _data = uft_data['diamagnetic_fraction_scene']

    _arg_list = [
        'beta', 'q95',  'q0'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_module.diamagnetic_fraction_scene(**_args)

    assert _pyproc == _data['final']['diacf'], \
        f'diamagnetic_fraction_scene: {_pyproc} != {_data["final"]["diacf"]}'


@pytest.mark.unitfort
def test_diamagnetic_fraction_hender(uft_data):
    _data = uft_data['diamagnetic_fraction_hender']
    _pyproc = physics_module.diamagnetic_fraction_hender(
        beta=_data['initial']['beta'],
    )
    assert _pyproc == _data['final']['diacf'], \
        f'diamagnetic_fraction_hender: {_pyproc} != {_data["final"]["diacf"]}'


@pytest.mark.unitfort
def test_bpol(uft_data):
    _data = uft_data['bpol']

    _arg_list = [
        'aspect',   'bt',   'delta',    'ip',   'kappa',    'perim',
        'qbar',     'icurr'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_module.bpol(**_args)

    assert _pyproc == _data['final']['bpol'], \
        f"bpol: {_pyproc} != {_data['final']['bpol']}"


@pytest.mark.unitfort
def test_pcond(uft_data):
    _data = uft_data['pcond']

    _arg_list = [
        'afuel',    'palpmw',   'aspect',       'bt',       'dnitot',
        'dene',     'dnla',     'eps',          'hfact',    'iinvqd',
        'isc',      'ignite',   'kappa',        'kappa95',  'pchargemw',
        'pinjmw',   'plascur',  'pcoreradpv',   'rmajor',   'rminor',
        'te',       'ten',      'tin',          'q',        'qstar',
        'vol',      'xarea',    'zeff'
    ]

    _res_list = [
        'kappaa',   'ptrepv',   'ptripv',   'tauee',
        'taueff',   'tauei',
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_module.pcond(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], \
            f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_rether(uft_data):
    _data = uft_data['rether']

    _arg_list = [
        'alphan',   'alphat',   'dene', 'dlamie',   'te',   'ti',   'zeffai'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_module.rether(**_args)

    assert _pyproc == _data['final']['piepv'], \
        f"rether: {_pyproc} != {_data['final']['piepv']}"


@pytest.mark.unitfort
def test_phyaux(uft_data):
    _data = uft_data['phyaux']

    _arg_list = [
        'aspect',   'dene', 'deni',     'fusionrate',   'alpharate',
        'plascur',  'sbar', 'dnalp',    'taueff',       'vol'
    ]

    _res_list = [
        'dntau',    'figmer',   'fusrat',   'qfuel',    'rndfuel',  'taup'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_module.phyaux(**_args)

    assert _pyproc[0] == np.inf, f"burnup: {_pyproc[0]} !=  infinity"

    for i, res in enumerate(_res_list):
        assert _pyproc[i+1] == _data['final'][res], \
            f"{res}: {_pyproc[i+1]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_ps_fraction_scene(uft_data):
    _data = uft_data['ps_fraction_scene']

    _pyproc = physics_module.ps_fraction_scene(
        beta=_data['initial']['beta'],
    )
    assert _pyproc == _data['final']['pscf'], \
        f'pscf: {_pyproc} != {_data["final"]["pscf"]}'


@pytest.mark.unitfort
def test_subr(uft_data):
    _data = uft_data['subr']

    _pyproc = physics_module.subr(
        a=_data['initial']['a'],
    )
    assert f'{_pyproc:.9f}' == f'{_data["final"]["b"]:.9f}', \
        f'b: {_pyproc:.4f} != {_data["final"]["b"]:.4f}'
