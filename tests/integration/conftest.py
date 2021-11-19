"""pytest config file for integration tests.

Define fixtures that will be shared across integration test modules.
"""
import pytest
from pathlib import Path
from shutil import copy
import os
from pkg_resources import resource_filename

@pytest.fixture
def temp_data(tmp_path):
    """Copy data dir contents into temp dir for testing.

    Any changes are discarded on fixture teardown.
    :param tmp_path: temporary path fixture
    :type tmp_path: Path
    :return: temporary path containing data files
    :rtype: Path
    """
    data_path = Path(__file__).parent / "data"

    for data_file in data_path.glob("*"):
        dst = tmp_path / data_file.name
        copy(data_file, dst)

    # Return tmp_path, now containing files copied from data dir
    return tmp_path

@pytest.fixture
def temp_data_cwd(temp_data):
    """Change cwd to temp_data dir, then yield it.

    Used when testing command-line args that look for files in the cwd.
    :param temp_data: temporary path containing data files
    :type temp_data: Path
    :yield: temporary path containing data files
    :rtype: Path
    """
    # Setup by changing cwd to temp_data and yielding it
    old_wd = os.getcwd()
    os.chdir(temp_data)
    yield temp_data

    # Teardown by changing back to previous dir
    os.chdir(old_wd)

@pytest.fixture
def mfile_name():
    """Return the name of the mfile to test.

    :return: name of the mfile
    :rtype: str
    """
    return "baseline_2018_MFILE.DAT"

@pytest.fixture
def scan_mfile_name():
    """Return the name of a scan mfile to test.

    :return: name of the mfile
    :rtype: str
    """
    return "scan_MFILE.DAT"

@pytest.fixture(scope="session", autouse=True)
def overwrite_ref_dicts(request):
    """Overwrite reference Python-Fortan dictionaries if CLI option supplied.

    Updates the dicts in the test suite with the dicts in the package. Runs
    once at the start of the test session.
    :param request: fixture to provide access to pytest CLI options
    :type request: pytest.fixture.SubRequest
    """
    if request.config.getoption("--overwrite"):
        # Get the dicts filename from the package data
        dicts_fn = resource_filename("process", "io/python_fortran_dicts.json")

        src_path = Path(dicts_fn)
        dst_path = Path(__file__).parent / "ref_dicts.json"
        copy(src_path, dst_path)