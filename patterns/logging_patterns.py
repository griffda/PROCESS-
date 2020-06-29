"""Demonstrate some basic logging patterns useful in Process.

Logs allow recording of information as Process runs, and they are another 
valuable debugging tool. Logging good information at an appropriate level makes
understanding how the code ran much easier.

Unlike exceptions, logs don't change the flow of execution; they just record as
passive observers. Therefore they can be inserted easily when diagnosing a 
problem (or hopefully, before) without any side-effects. 

When defined, a log is given a level which indicates its severity. Loggers can
then be configured to log at different levels, which "filter-in" all logs of a 
certain level and above. This allows the user to get fewer or more logs,
depending on whether they are happily doing runs or are trying to find a subtle
bug. Logs can also be written to file as well as the console.
"""
import logging
# The built-in logging module in Python
from process import fortran

logger = logging.getLogger(__name__)
# Create a new logger with the name of the current module
# Each log will then include the name of the module that it was written in

def log_error():
    """Write a log at the error level.
    
    This will show in the console by default.
    """
    logger.error("This is an error log.")

def log_all_levels():
    """Write logs at different levels.

    Write logs in increasing levels of severity. What will be filtered out 
    depends on the current log level. Default log level is "warning", which 
    means that only warning-level and above gets logged.
    """
    logger.debug("I'm a debug log.")
    logger.info("I'm an info log.")
    logger.warning("I'm a warning log.")
    logger.error("I'm an error log.")
    logger.critical("I'm a critical log.")

def log_var():
    """Log a variable."""
    x = 11.0
    logger.error(f"The value of x ({x}) is too high.")

def log_exception(value):
    """Log an exception if an assert fails.

    :param value: A value to assert
    :type value: int
    """
    logger.debug(f"value = {value}")
    # This won't be written unless the logging level is set to debug

    try:
        assert value == 1
    except AssertionError:
        # Now the AssertionError-type exception has been caught, log it
        logger.exception("value isn't acceptable!")
        # This is the same as logger.error(), but the exception (including the
        # stack trace) is included with a custom message. Recording all this
        # information together is very useful. Use logger.exception() in
        # error handlers rather than logger.error() for this reason.

        # raise
        # It is also possible to raise the exception again now that is has been
        # caught and logged. This allows the exception to carry on up the call
        # stack to be caught in another higher-up handler.

def log_fortran():
    """Log a value obtained from the Fortran interface."""
    ifail = fortran.main_module.eqslv()

    # Log the Fortran value at the info level
    logger.info(f"The value of ifail is: {ifail}")