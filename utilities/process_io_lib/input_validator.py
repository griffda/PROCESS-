"""Validate an input file using a set of rules.

This script checks an input file to make sure it is valid before it is read in
by the Fortran in Process. The thinking behind this is that it's a lot easier
to make an input validator in Python than Fortran! This aims to catch obvious 
static (i.e. pre-run-time) problems in the input to Process and provide clear 
feedback. This validation is based on a set of rules, which are defined in 
input_validator_rules.
"""
from process_io_lib import in_dat
from process_io_lib import input_validator_rules

INPUT_FILE_PATH = "IN.DAT"
# Todo: make dynamic

class RuleList(object):
    """Creates a list of Rule objects and validates them."""
    def __init__(self):
        """Fetches the rules and the input data."""
        self.rules = input_validator_rules.get_rules()
        self.data = in_dat.StructuredInputData(filename=INPUT_FILE_PATH)

    def filter_rules(self, filter):
        """Use a filter string to select certain rules by tag.
        
        :param filter: String to match with rule tags
        :type filter: str
        """
        # Todo: implement
        pass

    def validate_data(self):
        """Check all rules, passing the data to check.
        
        This runs the rule function stored on each rule object.
        """
        for rule in self.rules:
            rule.check(self.data)

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