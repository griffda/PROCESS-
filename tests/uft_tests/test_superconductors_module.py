###############################################################################
#                                                                             #
#                     UnitFort Superconductors Module Test                    #
#                                                                             #
#   Tests on methods within the Superconductors module contained in the file: #
#                                                                             #
#   'source/fortran/superconductors.f90'                                      #
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
#                                                                             #                                                                           #
#   DISCLAIMER                                                                #
#   ----------                                                                #
#                                                                             #
#   This test was created using the outputs of the UnitFort test parser       #
#   running on a FORTRAN 90 script. The method is 'naive' having no knowledge #
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
from process.fortran import superconductors


@pytest.mark.unitfort
def test_ksn40pb(uft_data):
    _data = uft_data['ksn40pb']

    _pyproc = superconductors.ksn40pb(_data['initial']['t'])

    assert _pyproc == _data['final']['ksn40pb'], f'ksn40pb: {_pyproc} != {_data["final"]["ksn40pb"]}'


@pytest.mark.unitfort
def test_khastelloyc276(uft_data):
    _data = uft_data['khastelloyc276']

    _pyproc = superconductors.khastelloyc276(_data['initial']['t'])

    assert _pyproc == _data['final']['khastelloyc276'], f'khastelloyc276: {_pyproc} != {_data["final"]["khastelloyc276"]}'


@pytest.mark.unitfort
def test_dhastelloyc276(uft_data):
    _data = uft_data['dhastelloyc276']

    _pyproc = superconductors.dhastelloyc276(_data['initial']['t'])

    assert _pyproc == _data['final']['dhastelloyc276'], f'dhastelloyc276: {_pyproc} != {_data["final"]["dhastelloyc276"]}'


@pytest.mark.unitfort
def test_rhastelloyc276(uft_data):
    _data = uft_data['rhastelloyc276']

    _pyproc = superconductors.rhastelloyc276(_data['initial']['t'])

    assert _pyproc == _data['final']['rhastelloyc276'], f'rhastelloyc276: {_pyproc} != {_data["final"]["rhastelloyc276"]}'


@pytest.mark.unitfort
def test_csn40pb(uft_data):
    _data = uft_data['csn40pb']

    _pyproc = superconductors.csn40pb(_data['initial']['t'])

    assert _pyproc == _data['final']['csn40pb'], f'csn40pb: {_pyproc} != {_data["final"]["csn40pb"]}'


@pytest.mark.unitfort
def test_rsn40pb(uft_data):
    _data = uft_data['rsn40pb']

    _pyproc = superconductors.rsn40pb(_data['initial']['t'])

    assert _pyproc == _data['final']['rsn40pb'], f'rsn40pb: {_pyproc} != {_data["final"]["rsn40pb"]}'


@pytest.mark.unitfort
def test_dsn40pb(uft_data):
    _data = uft_data['dsn40pb']

    _pyproc = superconductors.dsn40pb(_data['initial']['t'])

    assert _pyproc == _data['final']['dsn40pb'], f'dsn40pb: {_pyproc} != {_data["final"]["dsn40pb"]}'


@pytest.mark.unitfort
def test_dcu(uft_data):
    _data = uft_data['dcu']

    _pyproc = superconductors.dcu(_data['initial']['t'])

    assert _pyproc == _data['final']['dcu'], f'dcu: {_pyproc} != {_data["final"]["dcu"]}'


@pytest.mark.unitfort
def test_jcrit_nbti(uft_data):
    _data = uft_data['jcrit_nbti']

    _arg_list = [
        'temperature',   'bmax',   'c0',    'bc20max',   'tc0max'
    ]
    _res_list = [
        'jcrit',    'tcrit'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.jcrit_nbti(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_magrcu(uft_data):
    _data = uft_data['magrcu']

    _arg_list = [
        'b',   't',   'rrr'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.magrcu(**_args)

    assert _pyproc == _data['final']['magrcu'], f'magrcu: {_pyproc} != {_data["final"]["magrcu"]}'


@pytest.mark.unitfort
def test_jcrit_rebco(uft_data):
    _data = uft_data['jcrit_rebco']

    _arg_list = [
        'temperature',   'b', 'iprint'
    ]
    _res_list = [
        'jcrit',    'validity'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.jcrit_rebco(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_bi2212(uft_data):
    _data = uft_data['bi2212']

    _arg_list = [
        'bmax',   'jstrand', 'tsc', 'fhts'
    ]
    _res_list = [
        'jcrit',    'tmarg'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.bi2212(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_kcu(uft_data):
    _data = uft_data['kcu']

    _arg_list = [
        't',   'b', 'rrr'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.kcu(**_args)

    assert _pyproc == _data['final']['kcu'], f'kcu: {_pyproc} != {_data["final"]["kcu"]}'


@pytest.mark.unitfort
def test_rcu(uft_data):
    _data = uft_data['rcu']

    _arg_list = [
        't',   'b', 'rrr'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.rcu(**_args)

    assert _pyproc == _data['final']['rcu'], f'rcu: {_pyproc} != {_data["final"]["rcu"]}'


@pytest.mark.unitfort
def test_function_jcrit_rebco(uft_data):
    _data = uft_data['function_jcrit_rebco']

    _arg_list = [
        'temperature',   'b'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.function_jcrit_rebco(**_args)

    assert _pyproc == _data['final']['function_jcrit_rebco'], f'function_jcrit_rebco: {_pyproc} != {_data["final"]["function_jcrit_rebco"]}'


@pytest.mark.unitfort
def test_current_sharing_rebco(uft_data):
    _data = uft_data['current_sharing_rebco']

    _arg_list = [
        'bfield',   'j'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = superconductors.current_sharing_rebco(**_args)

    assert _pyproc == _data['final']['current_sharing_t'], f'current_sharing_t: {_pyproc} != {_data["final"]["current_sharing_t"]}'
