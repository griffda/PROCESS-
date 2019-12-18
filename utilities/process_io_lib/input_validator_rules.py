"""The rules for the input validator to run.

This module contains the rule classes which are used by the input_validator 
module to check rules on the input data to validate it. This consists of the 
abstract Rule class and the individual rule subclasses which are intialised by 
the input_validator module. Each rule class (a subclass of Rule) defines its own
check method, which checks that particular rule against the input data. This 
method stores the result and any messages on the instance of that rule class
itself.
"""
from abc import ABC, abstractmethod

class Rule(ABC):
    """Abstract rule class used by individual rule subclasses
    
    Each rule to check on the input file data is a subclass of this. This is an 
    abstract base class, so only subclasses of this can be instantiated.
    """
    @abstractmethod
    def __init__(self, tags):
        """Initialise a Rule object.
        
        This is an abstract method: it must be overridden in subclasses before
        they can be instantiated.

        :param tags: Tags for describing the rule; used for filtering rules
        (list of strings)
        :type tags: list
        """
        self.name = self.__class__.__name__
        # Set the rule's name to the name of the class
        self.tags = tags
        self.passed = None
        # Boolean set in rule_func to store result of check. Initialised to 
        # None for flexibility in rule_func
        self.messages = []
        # List of strings storing reason(s) for check failure
        self.data = None
        # Data for the rule to run on

    def set_data(self, data):
        """Save the input data onto the Rule object so the check method has 
        access to it.
        
        :param data: The input data object
        :type data: object
        """
        self.data = data

    @abstractmethod
    def check(self):
        """Method to check the rule, and set the passed attribute.
        
        Decides whether the passed attribute should be set to True or False.
        This is an abstract method: it must be overridden in subclasses before
        they can be instantiated.
        """
        pass

    def check_defined(self, var_name):
        """Checks that a parameter is defined.
        
        If the parameter is undefined, sets the self.passed attribute to False 
        and stores a message to record the failure of the check.

        :param var_name: The name of the parameter
        :type var_name: str
        """
        defined = self.data.is_param_defined(var_name)

        if not defined:
            message = f"{var_name} should be defined."
            self.messages.append(message)
            self.passed = False

    def check_undefined(self, var_name):
        """Checks that a parameter is undefined.

        If the parameter is defined, sets the self.passed attribute to False 
        and stores a message to record the failure of the check.
        
        :param var_name: The name of the parameter
        :type var_name: str
        """
        defined = self.data.is_param_defined(var_name)

        if defined:
            message = f"{var_name} shouldn't be defined."
            self.messages.append(message)
            self.passed = False

    def get_param_value(self, var_name):
        """Gets the value of a parameter in the input data.
        
        :param var_name: Name of the parameter
        :type var_name: str
        :return: Value of the parameter
        :rtype: int, float
        """
        return self.data.get_param_value(var_name)

# Rule classes for checking individual rules
class Ishape(Rule):
    """Rule subclass for checking the value of ishape and its dependencies
    
    ishape: switch for plasma cross-sectional shape calculation.
    """
    def __init__(self):
        """Call Rule's __init__ method with tags specific to Ishape"""
        super().__init__(["ishape", "kappa", "triang", "kappa95", "triang95"])

    def check(self):
        """The rule function for Ishape to check on the input data"""
        # ishape must be defined
        self.check_defined("ishape")
        ishape = self.get_param_value("ishape")
    
        if ishape is 0:
            # Use kappa and triang to calculate 95% kappa and triang
            self.check_defined("kappa")
            self.check_defined("triang")
            self.check_undefined("kappa95")
            self.check_undefined("triang95")
        elif ishape is 1:
            # Scale with aspect ratio
            self.check_undefined("qlim")
            self.check_undefined("kappa")
            self.check_undefined("triang")
            self.check_undefined("kappa95")
            self.check_undefined("triang95")
        elif ishape is 2:
            # kappa calculated using fkzohm, triang input
            self.check_defined("fkzohm")
            self.check_defined("aspect")
            self.check_undefined("kappa")
            self.check_defined("triang")
            self.check_undefined("kappa95")
            self.check_undefined("triang95")
        elif ishape is 3:
            # kappa calculated using fkzohm, triang95 input
            self.check_defined("fkzohm")
            self.check_undefined("kappa")
            self.check_defined("triang95")
            self.check_undefined("triang")
            self.check_undefined("kappa95")
        elif ishape is 4:
            # kappa95 and triang95 are used to calculate kappa and triang
            self.check_defined("kappa95")
            self.check_defined("triang95")
            self.check_undefined("kappa")
            self.check_undefined("triang")

        if self.passed is None:
            # No test has failed
            self.passed = True