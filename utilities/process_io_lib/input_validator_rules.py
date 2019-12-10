"""This module creates the Rule objects for the input_validator module to run. A
list of rule objects are created, each one with its own rule check function.
The rule check function stores the result and any messages on each Rule object
itself.
"""
class Rule(object):
    """A rule to check on the input file data."""
    def __init__(self, name, tags, rule_func):
        """Initialise a Rule object.
        
        :param object: [description]
        :type object: [type]
        :param name: Name of the rule
        :type name: str
        :param tags: Tags for describing the rule; used for filtering rules
        (list of strings)
        :type tags: list
        :param rule_func: The function to run when checking the rule
        :type rule_func: function
        """
        self.name = name
        self.tags = tags
        self.rule_func = rule_func
        self.passed = None
        # Boolean set in rule_func to store result of check. Initialised to 
        # None for flexibility in rule_func
        self.messages = []
        # List of strings storing reason(s) for check failure
        self.data = None
        # Data for the rule to run on

    def check(self, data):
        """Call the function check the rule.

        Save the input data onto the Rule object so the rule function has access
        to it.
        
        :param data: The input data object
        :type data: object
        """
        self.data = data
        self.rule_func(self)

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

def get_rules():
    """Create the Rule objects in a list and return them.
    
    To add a new rule, construct a new Rule object in the rules list below, 
    being sure to supply a rule function that checks what is required.

    :return: List of Rule objects
    :rtype: list
    """
    # For example: Rule(name, [tag1, tag2], rule_function)
    rules = [
        Rule("ishape", ["ishape", "kappa", "triang", "kappa95", "triang95"], 
        ishape)
    ]

    return rules

# Rule functions for checking individual rules
def ishape(self):
    """Rule function for checking the value of ishape and its dependencies.
    
    ishape: switch for plasma cross-sectional shape calculation.
    """
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