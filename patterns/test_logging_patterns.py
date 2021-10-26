"""Unit tests for the logging_patterns.py module."""
import logging_patterns as lp
import logging
import pytest
from process import fortran

def test_log_error(caplog):
    """Test that an error-level log can be written.

    :param caplog: Fixture for capturing log output
    :type caplog: object
    """
    # Write an error-level log
    lp.log_error()
    # Check the captured log output contains "ERROR"
    assert "ERROR" in caplog.text

def test_log_all_levels(caplog):
    """Test different levels of logging.

    See what is filtered out and what isn't, based on the current logging level.
    :param caplog: Fixture for capturing log output
    :type caplog: object
    """
    # Attempt to log all the possible levels
    lp.log_all_levels()
    # info-level logs shouldn't have been written
    assert "INFO" not in caplog.text
    # warning-level should
    assert "WARNING" in caplog.text

    # Set the logging level to include debug and higher
    caplog.set_level(logging.DEBUG)
    # Now re-run and see if info-level is there
    lp.log_all_levels()
    assert "INFO" in caplog.text

def test_log_var(caplog):
    """Test logging a variable.

    :param caplog: Log capture fixture
    :type caplog: object
    """
    lp.log_var()
    assert "11.0" in caplog.text

def test_log_exception(caplog):
    """Check an exception is logged.

    :param caplog: Log capture fixture
    :type caplog: object
    """
    # Run log_exception with a value that will cause an AssertionError
    lp.log_exception(2)
    # Check log is written
    assert "value isn't acceptable!" in caplog.text
    # Check log contains stack trace of exception
    assert "Traceback" in caplog.text

def test_log_fortran(caplog, monkeypatch):
    """Check a Fortran value is logged.

    :param caplog: Log capture fixture
    :type caplog: object
    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    # Mock the Fortran return value of ifail
    ifail = 1
    monkeypatch.setattr(fortran.vmcon_module, "info", ifail)

    # Set the log level to be low enough to catch info-level logs
    caplog.set_level(logging.INFO)

    # Run and check the ifail value was logged
    lp.log_fortran()
    assert "ifail is: 1" in caplog.text