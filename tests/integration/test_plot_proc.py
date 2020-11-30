"""Integration tests for plot_proc.py."""
import pytest
from pathlib import Path
from process.io import plot_proc

@pytest.fixture
def mfile():
    """Fixture for an MFILE for plot_proc to run on.

    :return: mfile path
    :rtype: Path
    """
    data_path = Path(__file__).parent / "data"
    mfile = data_path / "baseline_2018_MFILE.DAT"
    return mfile
    
# TODO Test for mfile in cwd again?

def test_input_file(mfile):
    """Run plot_proc on an input MFILE and check for an output.

    :param mfile: mfile path
    :type mfile: Path
    """
    mfile_str = str(mfile)
    plot_proc.main(args=["-f", mfile_str])
    
    # Assert a pdf has been created
    assert len(list(mfile.parent.glob("*.pdf")))