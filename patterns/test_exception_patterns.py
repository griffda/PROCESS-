"""Unit tests for the exception_patterns module.

This runs all of the examples to demonstrate how they are intended to behave.
"""
import exception_patterns as ep
from process import fortran
import pytest

@pytest.fixture(params=[1, 0, 2, -4])
def some_int(request):
    """Fixture to return different integers.

    :param request: Request object allowing access to each parameter
    :type request: object
    :return: An integer from the parameter list
    :rtype: int
    """
    return request.param

@pytest.fixture(params=[2, "some_string", 4.3, 0])
def who_knows(request):
    """Fixture to return an assortment of types to cause problems.

    :param request: Request object allowing access to each parameter
    :type request: object
    """
    return request.param

@pytest.fixture(params=[0, 1])
def ifail(request):
    """Return some possible values of ifail.

    :param request: Request object for parameter access
    :type request: object
    :return: ifail value
    :rtype: int
    """
    return request.param

def test_unhandled_python_raised_exception(some_int):
    """Call a function with no exception handling with different values.

    If raised, the exception is caught by the pytest.raises context.
    :param some_int: An integer to call with
    :type some_int: int
    """
    if some_int == 0:
        # Assert that a zero division exception is raised
        with pytest.raises(ZeroDivisionError):
            ep.unhandled_python_raised_exception(some_int)
    else:
        # Shouldn't throw an exception, should return a float
        result = ep.unhandled_python_raised_exception(some_int)
        assert type(result) is float

def test_handled_python_raised_exception(some_int):
    """Call a function with exception handling with different values.

    :param some_int: An integer to call with
    :type some_int: int
    """
    result = ep.handled_python_raised_exception(some_int)

    if some_int == 1:
        # Got a returned value
        assert result == 1
    elif some_int == 0:
        # Exception should be caught, and there is no returned result
        assert result == None

def test_handled_all_python_raised_exceptions(who_knows):
    """Call a function with a load of different types to raise exceptions.

    :param who_knows: A variety of different types
    :type who_knows: unknown
    """
    # Call the function with various types to try to cause problems
    result = ep.handled_all_python_raised_exceptions(who_knows)

    if who_knows != 0 and type(who_knows) is int or type(who_knows) is float:
        # This function should return a result of type float
        assert type(result) == float
    else:
        # This function should handle all exceptions and return None
        assert result == None

def test_raised_exception(some_int):
    """Call a function that can raise its own exceptions.

    :param some_int: An integer to call with
    :type some_int: int
    """
    if some_int == 0:
        # Check divide by 0 error is thrown
        with pytest.raises(ZeroDivisionError):
            result = ep.raised_exception(some_int)
    elif some_int < 0:
        # Check negative int error is thrown
        with pytest.raises(ValueError):
            result = ep.raised_exception(some_int)
    else:
        # Check the call runs correctly and a float is returned
        result = ep.raised_exception(some_int)
        assert type(result) is float

def test_assert_fortran_value(ifail, monkeypatch):
    """Test asserting a Fortran-returned result.

    :param ifail: A possible value of ifail
    :type ifail: int
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Mock fortran.main_module.eqslv() return value with ifail
    monkeypatch.setattr(fortran.vmcon_module, "info", ifail)
    
    if ifail == 0:
        # Assert evaluates to True, no exception
        ep.assert_fortran_value()
    else:
        # Assert evaluates to False, expect exception to be thrown
        with pytest.raises(AssertionError):
            ep.assert_fortran_value()