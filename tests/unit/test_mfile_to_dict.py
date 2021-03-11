import pytest
import os
import pathlib
import tempfile
import json
import yaml
import pickle

from process.utilities import mfile2dict


@pytest.fixture(scope="module")
def read_mfile():
    _tracking_2018 = os.path.join(
        pathlib.Path(os.path.dirname(__file__)).parent.parent,
        'tracking',
        'baseline_2018'
    )

    _test_mfile = os.path.join(_tracking_2018, 'ref_baseline_2018_MFILE.DAT')

    return mfile2dict.MFILEParser(_test_mfile)


@pytest.fixture(scope="module")
def temporary_dir():
    return tempfile.mkdtemp()


@pytest.mark.python_tools
def test_parser_succeed(read_mfile):
    assert read_mfile._mfile_data


@pytest.mark.python_tools
def test_value_read(read_mfile):
    read_mfile.get_parameter_value('xcm013') == 9.7718E-01


@pytest.mark.python_tools
def test_write_json(read_mfile, temporary_dir):
    _json_f = os.path.join(temporary_dir, '2017_baseline.json')
    read_mfile.write(_json_f)
    assert os.path.exists(_json_f)
    assert json.load(open(_json_f))


@pytest.mark.python_tools
def test_write_yaml(read_mfile, temporary_dir):
    _yml_f = os.path.join(temporary_dir, '2017_baseline.yml')
    read_mfile.write(_yml_f)
    assert os.path.exists(_yml_f)
    assert yaml.load(open(_yml_f), Loader=yaml.BaseLoader)


@pytest.mark.python_tools
def test_write_pickle(read_mfile, temporary_dir):
    _pckl_f = os.path.join(temporary_dir, '2017_baseline.pckl')
    read_mfile.write(_pckl_f)
    assert os.path.exists(_pckl_f)
    assert pickle.load(open(_pckl_f, 'rb'))
