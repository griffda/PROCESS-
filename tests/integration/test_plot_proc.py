"""Integration tests for plot_proc.py."""
import pytest
from pathlib import Path
from process.io import plot_proc

# TODO Test for mfile in cwd again?

def test_input_file(temp_data):
    """Run plot_proc on an input MFILE and check for an output.

    :return: temporary path containing data files
    :rtype: Path
    """
    mfile = temp_data / "baseline_2018_MFILE.DAT"
    mfile_str = str(mfile)
    plot_proc.main(args=["-f", mfile_str])
    
    # Assert a pdf has been created
    assert len(list(mfile.parent.glob("*.pdf")))