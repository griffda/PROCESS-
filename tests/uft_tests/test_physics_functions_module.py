###############################################################################
#                                                                             #
#                  UnitFort Physics Functions Module Test                     #
#                                                                             #
#   Tests on methods within the Physics Functions module contained            #
#   in the file:                                                              #
#                                                                             #
#   'source/fortran/physics_functions.f90'                                    #
#                                                                             #
#   within the PROCESS application libraries.                                 #
#                                                                             #
#   @author :   K. Zarebski <kristian.zarebski@ukaea.uk>                      #
#   @date   :   last modified 2020-12-03                                      #
#                                                                             #
#   WARNING: Due to many PROCESS global variables being updated by these      #
#            methods some variables have to be reinitialised. However this    #
#            is only performed on those which were manually altered. As such  #
#            it is highly recommended that the UnitFort tests be run on their #
#            own as there is no guarantee they will not affect others using   #
#            the PROCESS python library.                                      #
#                                                                             #
#            Run using the dedicated marker: pytest -m unitfort               # 
#                                                                             #                                                                         #
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
from process.fortran import physics_functions_module
from process.fortran import physics_variables as pv


@pytest.mark.unitfort
def test_palph2(uft_data):
    _data = uft_data['palph2']

    _arg_list = [
        'bt',       'bp',           'dene',         'deni',
        'dnitot',   'falpe',        'falpi',        'palpnb',
        'ifalphap', 'pchargepv',    'pneutpv',      'ten',
        'tin',      'vol',          'palppv'
    ]

    _res_list = [
        'palpmw',   'pneutmw',   'pchargemw',    'betaft',
        'palpipv',  'palpepv',  'pfuscmw',      'powfmw',
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_functions_module.palph2(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], \
            f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_bosch_hale(uft_data):
    _data = uft_data['bosch_hale']

    _arg_list = [
        't', 'reaction'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_functions_module.bosch_hale(**_args)

    assert _pyproc == _data['final']['bosch_hale'], \
        f'bosch_hale: {_pyproc} != {_data["final"]["bosch_hale"]}'


@pytest.mark.unitfort
def test_fsv(uft_data):
    _data = uft_data['fsv']

    _pyproc = physics_functions_module.fsv(_data['initial']['u'])

    assert _pyproc == _data['final']['fsv'], \
        f'fsv: {_pyproc} != {_data["final"]["fsv"]}'


@pytest.mark.unitfort
def test_total_mag_field(uft_data):
    _data = uft_data['total_mag_field']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _pyproc = physics_functions_module.total_mag_field()

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    assert _pyproc == _data['final']['total_mag_field'], \
        f'total_mag_field: {_pyproc} != {_data["final"]["total_mag_field"]}'


@pytest.mark.unitfort
def test_beamcalc(uft_data):
    _data = uft_data['beamcalc']

    _arg_list = [
        'nd',       'nt',       'ealphadt', 'ebeam',    'ecritd',
        'ecritt',   'tausbme',  'ftritbm',  'ibeam',    'ti',
        'vol',      'svdt'
    ]

    _res_list = [
        'palfdb',   'palftb',   'nhot', 'ehot'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_functions_module.beamcalc(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], \
            f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_beamfus(uft_data):
    _data = uft_data['beamfus']

    _arg_list = [
        'beamfus0', 'betbm0',   'bp',     'bt',     'cnbeam',   'dene', 'deni',
        'dlamie',   'ealphadt', 'enbeam', 'fdeut',  'ftrit',    'ftritbm',
        'sigvdt',   'ten',      'tin',    'vol',    'zeffai'
    ]

    _res_list = [
        'betanb', 'dnbeam2',    'palpnb'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_functions_module.beamfus(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], \
            f"{res}: {_pyproc[i]} != {_data['final'][res]}"  


@pytest.mark.unitfort
def test_palph(uft_data):
    _data = uft_data['palph']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _arg_list = [
        'alphan', 'alphat',   'deni',     'fdeut',     'fhe3',   'ftrit', 'ti'
    ]

    _res_list = [
        'palppv',       'pchargepv',    'pneutpv',    'sigvdt',   'fusionrate',
        'alpharate',    'protonrate',   'pdtpv',      'pdhe3pv',  'pddpv'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = physics_functions_module.palph(**_args)

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], \
            f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_beta_poloidal(uft_data):
    _data = uft_data['beta_poloidal']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _pyproc = f'{physics_functions_module.beta_poloidal():.4f}'

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    assert _pyproc == f'{_data["final"]["beta_poloidal"]:.4f}', \
        f'beta_poloidal: {_pyproc} != {_data["final"]["beta_poloidal"]:.4f}'


@pytest.mark.unitfort
def test_psync_albajar_fidone(uft_data):
    _data = uft_data['psync_albajar_fidone']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _pyproc = f'{physics_functions_module.psync_albajar_fidone():.4f}'

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    assert _pyproc == f'{_data["final"]["psyncpv"]:.4f}', \
        f'psyncpv: {_pyproc} != {_data["final"]["psyncpv"]:.4f}'


@pytest.mark.unitfort
def test_plasma_elongation_ipb(uft_data):
    _data = uft_data['plasma_elongation_ipb']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _pyproc = f'{physics_functions_module.plasma_elongation_ipb():.4f}'

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    assert _pyproc == f'{_data["final"]["plasma_elongation_ipb"]:.4f}', \
        f'plasma_elongation_ipb: {_pyproc} != {_data["final"]["plasma_elongation_ipb"]:.4f}'


@pytest.mark.unitfort
def test_pthresh(uft_data):
    _data = uft_data['pthresh']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _arg_list = [
        'dene', 'dnla',   'bt',     'rmajor',     'kappa',   'sarea', 'aion',
        'aspect'
    ]

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    _args = {t: _data['initial'][t] for t in _arg_list}

    _args['pthrmw'] = np.array(_data['initial']['pthrmw'])

    physics_functions_module.pthresh(**_args)

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    _pyproc = [f'{i:.4f}' for i in _args['pthrmw']]
    _expected = [f'{i:.4f}' for i in _data["final"]["pthrmw"]]

    assert _pyproc == _expected, f'pthrmw: {_pyproc} != {_expected}'


@pytest.mark.unitfort
def test_res_diff_time(uft_data):
    _data = uft_data['res_diff_time']

    _initial_vals = {t: getattr(pv, t) for t in _data['initial']['physics_variables']}

    for var in _initial_vals:
        setattr(pv, var, _data['initial']['physics_variables'][var])

    _pyproc = f'{physics_functions_module.res_diff_time():.4f}'

    for var in _initial_vals:
        setattr(pv, var, _initial_vals[var])

    assert _pyproc == f'{_data["final"]["res_diff_time"]:.4f}', \
        f'res_diff_time: {_pyproc} != {_data["final"]["res_diff_time"]:.4f}'
