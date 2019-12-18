"""Validate an input file using a set of rules.

This script checks an input file to make sure it is valid before it is read in
by the Fortran in Process. The thinking behind this is that it's a lot easier
to make an input validator in Python than Fortran! This aims to catch obvious 
static (i.e. pre-run-time) problems in the input to Process and provide clear 
feedback. This validation is based on a set of rules, which are defined in 
input_validator_rules.
"""
import inspect
from process_io_lib import in_dat
from process_io_lib import input_validator_rules

INPUT_FILE_PATH = "IN.DAT"
# Todo: make dynamic

class RuleList(object):
    """Creates a list of rule instances and validates them."""
    def __init__(self):
        """Fetches the rules and the input data."""
        self.rules = []
        self.get_rules()
        self.data = in_dat.StructuredInputData(filename=INPUT_FILE_PATH)

    def get_rules(self):
        """Create the rule instances in a list and store them
        
        Inspects the input_validator_rules module and filters its classes to 
        give only the rule classes (subclasses of Rule). Then creates an 
        instance of each rule in a list.
        """
        def is_rule_class(a_class):
            """Checks class is a rule (subclass of Rule)
            
            :param a_class: Any class
            :type a_class: abc.ABCMeta
            :return: True if Rule subclass, False if not
            :rtype: bool
            """
            # Check if one of the class's parents is Rule
            bases = a_class.__bases__
            for base in bases:
                if base is input_validator_rules.Rule:
                    return True
            return False

        # Fetch all classes from input_validator_rules module as (name, class) 
        # tuples
        class_tuples = inspect.getmembers(input_validator_rules, 
            inspect.isclass)

        # Filter to get individual rule classes only (subclasses of Rule)
        rule_classes = [class_tuple[1] for class_tuple in class_tuples
            if is_rule_class(class_tuple[1])]

        # Now create an instance of each rule class in a list
        self.rules = [rule_class() for rule_class in rule_classes]

    def filter_rules(self, filter):
        """Use a filter string to select certain rules by tag.
        
        :param filter: String to match with rule tags
        :type filter: str
        """
        # Todo: implement
        pass

    def validate_data(self):
        """For all rules, set the input data, then check it.
        
        This runs the check method of each rule instance.
        """
        for rule in self.rules:
            rule.set_data(self.data)
            rule.check()

    def print_results(self):
        """Print the results stored on all the rules objects."""
        for rule in self.rules:
            # Rule name
            output = f"Rule {rule.name}: "
            
            # Pass status
            if rule.passed:
                output += "passed"
            else:
                output += "failed"

            # Fail reasons, if any
            if len(rule.messages) > 0:
                output += ":"
                for message in rule.messages:
                    output += " " + message

        print(output)

def main():
    """Create the rule list and validate each rule against the input data."""
    rule_list = RuleList()
    rule_list.validate_data()
    rule_list.print_results()

main()