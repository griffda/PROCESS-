"""Patterns of how to use exceptions in Process.

Exceptions are useful signals to tell you when and where something isn't what it
should be. They are not used to control normal program flow; they are to assist
with debugging by flagging problems closer to the root cause.

Here are some examples of how to use exceptions in Python when dealing with
Python-Fortran interfaced code. These are purely examples to be used as 
templates, and are not used by Process itself. This is run by the 
test_exception_patterns unit test module.
"""
from process import fortran

# Simple functions to be run in different ways by the exception-example 
# functions
def one_over(some_int):
    """Calculates one over the input.

    :param some_int: The number to divide 1 by.
    :type some_int: int
    """
    # Divide 1 by the input in an unguarded manner
    result = 1 / some_int
    return result

# Python-raised exception example functions
def unhandled_python_raised_exception(some_int):
    """Just run a function with no exception handling.
    
    This might just result in a failure to deal with a Python-raised exception.
    When some_int == 0, we expect a ZeroDivisionError exception to be thrown
    by Python. It is not handled (caught), so it would cause execution to 
    terminate, which may be what is desired. Whilst there would be an exception
    trace, it's not hugely helpful, and more information could be provided.

    :param some_int: A number to divide one by
    :type some_int: int
    :return: Result of 1 / some_int
    :rtype: float
    """
    return one_over(some_int)

def handled_python_raised_exception(some_int):
    """Run a function and deal with a Python-raised exception if required.

    This is an exception that Python itself might raise because something error-
    producing has been attempted (such as dividing by zero).

    A try/except block is being used here, which will have a go at running
    one_over, but if it throws a ZeroDivisionError, is will catch the exception.
    This will prevent the exception from terminating execution. If a different
    type of exception is raised (e.g. TypeError), then the exception won't be
    caught, and execution will terminate.
    """
    try:
        return one_over(some_int)
    except ZeroDivisionError:
        print("You can't divide by zero!")

def handled_all_python_raised_exceptions(who_knows):
    """Run a function and deal with any exceptions.

    This is a common pattern, and it provides some discrimination between the 
    divide-by-zero and type error cases and any other exceptions. This will 
    ultimately catch all exceptions, so execution will plough on regardless. 
    This isn't always a good thing; sometimes you want some types of exception 
    to not be caught so that execution stops immediately in those cases to 
    pinpoint the error.

    Stacking specific error catches like this is good practice.

    :param who_knows: Could be anything!
    :type who_knows: unknown
    """
    try:
        return one_over(who_knows)
    except ZeroDivisionError:
        # Catch the zero-division exception only
        print("You can't divide by zero!")
    except TypeError:
        # Catch errors due to trying to do things to the wrong type
        print("You might have the wrong type.")
    except:
        # This will catch any exception, regardless of type
        print("Something went wrong.")

# Manually-raised exception example functions
def raised_exception(some_int):
    """A function that can raise its own exceptions.

    This is more useful and interesting: making your own exceptions. They can
    be raised for arbitrary reasons, rather than errors that Python itself 
    throws. Custom messages can be added to the exceptions to make them more 
    informative and a lot better than separate exceptions and print statements.
    This is good practice.

    If raised, exceptions will halt execution unless they are caught higher up 
    in the call stack.

    :param some_int: An integer
    :type some_int: int
    """
    if some_int == 0:
        raise ZeroDivisionError("I wouldn't try dividing by zero!")
        # This raises a ZeroDivisionError exception before it is tried. It also
        # adds a custom message to the exception
    elif some_int < 0:
        raise ValueError("I just don't like negative numbers.")
        # Raise a ValueError

    # If neither of the above exceptions are thrown, then call the function
    return one_over(some_int)

# Fortran assertions
def assert_fortran_value():
    """Assert that a value in the Fortran is equal to a certain value.

    This is a good way of raising exceptions in Python based on the results of 
    Fortran calls. The "assert" statement evaluates the expression, and if it is
    found to be False, it raises an AssertionError exception. This is good way 
    of detecting when something's gone wrong and hence catching Fortran bugs
    closer to the source of the problem.
    """
    ifail = fortran.vmcon_module.info
    # ifail should be 0; raise an AssertionError with a message if not
    assert ifail == 0, "ifail isn't 0!"

# TODO Logging