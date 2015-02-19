"""

    File for reading IN.DATs
    Version 2 (mainly for use with IN.DAT created from UI)

    James Morris
    CCFE
    11/12/14

"""

import numpy as np

# Dictionary for variable types
from process_io_lib.process_dicts import DICT_VAR_TYPE

# Dictionary for ixc -> name
from process_io_lib.process_dicts import DICT_IXC_SIMPLE

# Dictionary for variable modules
from process_io_lib.process_dicts import DICT_MODULE

# Dictionary for parameter descriptions
from process_io_lib.process_dicts import DICT_DESCRIPTIONS

# Dictionary for parameter defaults
from process_io_lib.process_dicts import DICT_DEFAULT


def fortran_python_scientific(var_value):
    """ Convert FORTRAN scientific notation to Python notation

    :param var_value: variable value
    :return: value with 'd/D' notation swapped for 'e/E' notation
    """
    return var_value.replace("D", "e").replace("d", "e")


def remove_empty_lines(lines):
    """Function to remove empty lines from list.

    :param lines: list of lines (type=list)
    :return: list of lines with empty lines removed (type=list)
    """
    return [line for line in lines if line != "\n"]


def is_title(line):
    """ Function to determine if line is title line

    :param line: line from IN.DAT
    :return: True/False if line is a title or header.
    """
    return line[:2] == "*-" or line[:3] == "***" or line[0] == "$"


def is_comment(line):
    """ Function to determine if line is a commented line
    :param line: line from IN.DAT
    :return: True/False if line is a commented line
    """
    return line[0] == "*"


def is_iteration_variable(name):
    """ Function to determine if item is an iteration variable

    :param name: Name of the variable
    :return: True/False if 'ixc' is in name
    """
    return "ixc" in name


def is_constraint_equation(name):
    """ Function to determine if item is constraint equation

    :param name: Name of the variable
    :return: True/False if 'icc' is in name
    """
    return "icc" in name


def is_bound(name):
    """ Function to determine if item is a bound

    :param name: Name of the variable
    :return: True/False if 'bound' is in name
    """
    return "bound" in name


def find_line_type(line):
    """ Function to find line type

    :param line: Line to find type of
    :return: Return string describing the line type
    """

    # Split variable name from line
    name = line.split("=")[0].strip("")

    # If the line is just the title for a section
    if is_title(line):
        return "Title"

    # If the line is a commented line
    elif is_comment(line):
        return "Comment"

    # Else if the line contains a constraint equation
    elif is_constraint_equation(name):
        return "Constraint Equation"

    # Else if the line contains an iteration variable
    elif is_iteration_variable(name):
        return "Iteration Variable"

    # Else if the line contains a bound statement
    elif is_bound(name):
        return "Bound"

    # Else the line contains an regular parameter
    else:
        if "fimp(" in name:
            return "fimp"
        elif "zref(" in name:
            return "zref"
        else:
            return "Parameter"


def find_parameter_group(name):
    """ Function to find the module which the parameter belongs to.

    :param name: Parameter name
    :return: Return the module the parameter belongs to
    """

    # Search DICT_MODULES for parameter
    for key in DICT_MODULE.keys():
        if name in DICT_MODULE[key]:
            return key


def process_constraint_equation(data, line):
    """ Function to process constraint equation entry in IN.DAT

    :param data: Data dictionary for the IN.DAT information
    :param line: Line from IN.DAT to process
    :return: Nothing
    """

    # Remove comment from line to make things easier
    no_comment_line = line.split("*")[0].split("=")

    # If the line contains a constraint equation in the form ICC(#)
    if "(" in no_comment_line[0] and ")" in no_comment_line[0]:
        constraints = [no_comment_line[1].strip()]

    # Else the line contains a list of constraint equations icc = #, #, #
    else:
        constraints = no_comment_line[1].strip().split(",")
        if "" in constraints:
            constraints.remove("")

    # List of new constraints read in
    value = [int(item.strip()) for item in constraints]

    # Populate data dictionary with constraint equations
    # If constraint equation list not already in data dictionary initialise
    # INVariable class
    if "icc" not in data.keys():
        data["icc"] = INVariable("icc", value, "Constraint Equation",
                                 "Constraint Equation", "Constraint Equations")

    else:
        # Add constraint equation numbers to list
        for item in constraints:
            if int(item) not in data["icc"].value:
                data["icc"].value.append(int(item))
        data["icc"].value.sort()


def process_iteration_variables(data, line):
    """ Function to process iteration variables entry in IN.DAT

    :param data: Data dictionary for the IN.DAT information
    :param line: Line from IN.DAT to process
    :return: Nothing
    """

    # Remove comment from line to make things easier
    no_comment_line = line.split("*")[0].split("=")

    # If the line contains an iteration variable in the form IXC(#)
    if "(" in no_comment_line[0] and ")" in no_comment_line[0]:
        iteration_variables = [no_comment_line[1].strip()]

    # Else the line contains a list of iteration variables IXC = #, #, #
    else:
        iteration_variables = no_comment_line[1].strip().split(",")
        if "" in iteration_variables:
            iteration_variables.remove("")

    # List of new constraints read in
    value = [int(item.strip()) for item in iteration_variables]

    # Populate data dictionary with iteration variables
    # If iteration variables list not already in data dictionary initialise
    # INVariable class
    if "ixc" not in data.keys():
        data["ixc"] = INVariable("ixc", value, "Iteration Variable",
                                 "Iteration Variable", "Iteration Variables")

    else:
        # Add iteration variable to list
        for item in iteration_variables:
            if int(item) not in data["ixc"].value:
                data["ixc"].value.append(int(item))
        data["ixc"].value.sort()


def process_bound(data, line):
    """ Function to process bound entries in IN.DAT

    :param data: Data dictionary for the IN.DAT information
    :param line: Line from IN.DAT to process
    :return: Nothing
    """

    # Initialise bound type
    bound_type = None

    # Remove comment from line to make things easier
    no_comment_line = line.split("*")[0].split("=")

    # If upper bound
    if "boundu" in no_comment_line[0]:
        bound_type = "u"

    # If lower bound
    elif "boundl" in no_comment_line[0]:
        bound_type = "l"

    # Get bound information
    bound = no_comment_line[0].strip("boundl").replace("(", "").\
        replace(")", "").strip()
    bound_value = no_comment_line[1].strip().replace(",", "").replace("d", "e").\
        replace("D","e")

    # If bound not in the bound dictionary then add entry for bound with an
    # empty dictionary
    if bound not in data["bounds"].value.keys():
        data["bounds"].value[bound] = dict()

    # Populate data dictionary with bound information
    data["bounds"].value[bound][bound_type] = bound_value


def process_fimp(data, line):
    """Function to process fimp array

    :param data: Data dictionary for the IN.DAT information
    :param line: Line from IN.DAT to process
    :return: nothing
    """

    fimp_index = int(line.split("(")[1].split(")")[0]) - 1
    fimp_value = eval(line.split("=")[-1].replace(",", ""))
    data["fimp"].value[fimp_index] = fimp_value


def process_zref(data, line):
    """ Function to process zref array

    :param data: Data dictionary for the IN.DAT information
    :param line: Line from IN.DAT to process
    :return: nothing
    """

    zref_index = int(line.split("(")[1].split(")")[0]) - 1
    zref_value = eval(line.split("=")[-1].replace(",", ""))
    data["zref"].value[zref_index] = zref_value


def process_parameter(data, line):
    """ Function to process parameter entries in IN.DAT

    :param data: Data dictionary for the IN.DAT information
    :param line: Line from IN.DAT to process
    :return: Nothing
    """

    # Remove comment from line to make things easier
    no_comment_line = line.split("*")[0].split("=")

    # Parameter name
    name = no_comment_line[0].strip()

    # Parameter value
    if len(no_comment_line[-1].split(",")) > 2:
        value = no_comment_line[1].strip()
    else:
        value = no_comment_line[1].strip().replace(",", "")

    # Find group of variables the parameter belongs to
    parameter_group = find_parameter_group(name)

    # Get parameter comment/description from dictionary
    comment = DICT_DESCRIPTIONS[name].replace(",", ";").\
        replace(".", ";").replace(":", ";")

    # Populate the IN.DAT dictionary with the information
    data[name] = INVariable(name, value, "Parameter", parameter_group, comment)


def process_line(data, line_type, line):
    """ Function to process the line and return the appropriate INVariable
    object

    :param data: Data dictionary for the IN.DAT information
    :param line_type: Type of information the line contains
    :param line: Line from IN.DAT to process
    :return: Nothing
    """

    # Create bound variable class using INVariable class if the bounds entry
    # doesn't exist
    if "bounds" not in data.keys():
        data["bounds"] = INVariable("bounds", dict(), "Bound",
                                    "Bound", "Bounds")

    # Create a fimp variables class using INVariable class if the fimp entry
    # doesn't exist
    if "fimp" not in data.keys():
        empty_fimp = DICT_DEFAULT['fimp']
        parameter_group = find_parameter_group("fimp")

        # Get parameter comment/description from dictionary
        comment = DICT_DESCRIPTIONS["fimp"].replace(",", ";").\
            replace(".", ";").replace(":", ";")

        data["fimp"] = INVariable("fimp", empty_fimp, "fimp", parameter_group,
                                  comment)

    # Create a zref variable class using INVariable class if the zref entry
    # doesn't exist
    if "zref" not in data.keys():
        empty_zref = DICT_DEFAULT['zref']
        parameter_group = find_parameter_group("zref")

        # Get parameter comment/description from dictionary
        comment = DICT_DESCRIPTIONS["zref"].replace(",", ";").\
            replace(".", ";").replace(":", ";")

        data["zref"] = INVariable("zref", empty_zref, "zref", parameter_group,
                                  comment)

    # Constraint equations
    if line_type == "Constraint Equation":
        process_constraint_equation(data, line)

    # Iteration_variables
    elif line_type == "Iteration Variable":
        process_iteration_variables(data, line)

    # Bounds
    elif line_type == "Bound":
        process_bound(data, line)

    # fimp
    elif line_type == "fimp":
        process_fimp(data, line)

    # zref
    elif line_type == "zref":
        process_zref(data, line)

    # Parameter
    else:
        process_parameter(data, line)


def write_title(title, out_file):
    """ Function to write title line to file with fixed width

    :param title: The name of the section
    :param out_file: Output file for new IN.DAT
    :return: Nothing
    """

    # Insert title name into line of fixed width
    formatted_title = "*" + title.center(50, "-") + "*\n"

    # Output to file
    out_file.write(formatted_title)
    out_file.write("\n")


def write_constraint_equations(data, out_file):
    """ Function to write constraint equation information to file

    :param data: Data dictionary for the IN.DAT information
    :param out_file: Output file for new IN.DAT
    :return: Nothing
    """

    # Header
    write_title("Constraint Equations", out_file)

    # Write number of equations to file
    neqns_line = "neqns = {0} * {1}\n\n".format(data["neqns"].value,
                                                data["neqns"].comment)
    out_file.write(neqns_line)

    # List of constraints
    constraint_equations = data["icc"].value

    # Write constraints to file
    counter = 1
    for constraint in constraint_equations:
        constraint_line = "icc({0}) = {1}\n\n".format(counter, constraint)
        out_file.write(constraint_line)
        counter += 1


def write_iteration_variables(data, out_file):
    """ Function to write iteration variable information to file

    :param data: Data dictionary for the IN.DAT information
    :param out_file: Output file for new IN.DAT
    :return: Nothing
    """

    # Header
    write_title("Iteration Variables", out_file)

    # List of constraints
    iteration_variables = data["ixc"].value

    # Write nvar
    nvar_line = "nvar = {0} * {1}\n\n".format(data["nvar"].value,
                                              data["nvar"].comment)
    out_file.write(nvar_line)

    # Write constraints to file
    counter = 1
    for variable in iteration_variables:
        comment = DICT_IXC_SIMPLE[str(variable).replace(",", ";").
                                  replace(".", ";").replace(":", ";")]
        variable_line = "ixc({0}) = {1} * {2}\n\n".format(counter, variable,
                                                          comment)
        out_file.write(variable_line)
        counter += 1

        # Write bounds if there are any
        if str(variable) in data["bounds"].value:

            # Lower bound
            if "l" in data["bounds"].value[str(variable)].keys():
                lower_bound_line = "boundl({0}) = {1}\n\n".\
                    format(variable, data["bounds"].value[str(variable)]["l"].
                           replace("e", "d"))
                out_file.write(lower_bound_line)

            # Upper bound
            if "u" in data["bounds"].value[str(variable)].keys():
                upper_bound_line = "boundu({0}) = {1}\n\n".\
                    format(variable, data["bounds"].value[str(variable)]["u"].
                           replace("e", "d"))
                out_file.write(upper_bound_line)


def write_parameters(data, out_file):
    """ Write parameters to file

    :param data: Data dictionary for the IN.DAT information
    :param out_file: Output file for new IN.DAT
    :return: Nothing
    """

    # Write parameters in order defined in DICT_MODULE
    for module in DICT_MODULE.keys():

        # Write module heading
        write_title("{0}".format(module), out_file)

        # Items to exclude
        exclusions = ["neqns", "nvar", "icc", "ixc"]

        # Write parameters for given module
        for item in DICT_MODULE[module]:
            if item not in exclusions and item in data.keys():

                if item == "fimp":
                    for k in range(len(data["fimp"].get_value)):
                        tmp_fimp_name = "fimp({0})".format(str(k+1).zfill(1))
                        tmp_fimp_value = data["fimp"].get_value[k]
                        parameter_line = "{0} = {1}\n\n".\
                            format(tmp_fimp_name, tmp_fimp_value)
                        out_file.write(parameter_line)
                elif item == "zref":
                    for j in range(len(data["zref"].get_value)):
                        tmp_zref_name = "zref({0})".format(str(j+1).zfill(1))
                        tmp_zref_value = data["zref"].get_value[j]
                        parameter_line = "{0} = {1}\n\n".\
                            format(tmp_zref_name, tmp_zref_value)
                        out_file.write(parameter_line)
                else:
                    # Left justification set to 8 to allow easier reading
                    # Only use first line of comment to avoid lots of info
                    line_value = data[item].value
                    line_string = ""
                    # if parameter is a list only output values comma separated
                    if isinstance(line_value, list):
                        for val in line_value:
                            line_string += str(val) + ", "
                        line_value = line_string.rstrip(", ")

                    if isinstance(line_value, str):
                        split_line = line_value.split(" ")
                    try:
                        float(split_line[0])
                        if len(split_line) > 1:

                            line_value = ", ".\
                                join([entry for entry in split_line])
                    except:
                        pass

                    parameter_line = "{0} = {1} * {2}\n\n". \
                        format(item.ljust(8), line_value,
                               data[item].comment.split("\n")[0])
                    out_file.write(parameter_line)


def add_iteration_variable(data, variable_number):
    """ Function to add iteration variable to IN.DAT data dictionary

    :param data: Data dictionary for the IN.DAT information
    :param variable_number: Iteration variable number to add
    :return: Nothing
    """

    # Check the variable number is not already in the iteration variable list
    if variable_number not in data["ixc"].value:
        data["ixc"].value.append(variable_number)
        data["ixc"].value.sort()

        # Increase the number of iteration variables parameter by 1
        data["nvar"].value = str(data["nvar"].get_value + 1)

    else:
        print("Variable number {0} already in iteration variable list".
              format(variable_number))


def remove_iteration_variable(data, variable_number):
    """ Function to remove iteration variable from the IN.DAT data dictionary

    :param data: Data dictionary for the IN.DAT information
    :param variable_number: Iteration variable number to remove
    :return: Nothing
    """

    # Check the variable is in the iteration variable list
    if variable_number in data["ixc"].value:
        data["ixc"].value.remove(variable_number)
        data["ixc"].value.sort()

        # Decrease the number of iteration variables parameter by 1
        data["nvar"].value = str(data["nvar"].get_value - 1)
    else:
        print("Variable number {0} not in iteration variable list".
              format(variable_number))


def add_constraint_equation(data, equation_number):
    """ Function to add constraint equation to the IN.DAT data dictionary

    :param data: Data dictionary for the IN.DAT information
    :param equation_number: Constraint equation number to add
    :return: Nothing
    """

    # Check the constraint is not already in the constraint equation list
    if equation_number not in data["icc"].value:
        data["icc"].value.append(equation_number)
        data["icc"].value.sort()

        # Increase the number of constraint equations parameter by 1
        data["neqns"].value = str(data["neqns"].get_value + 1)
    else:
        print("Equation number {0} already in constraint equations list".
              format(equation_number))


def remove_constraint_equation(data, equation_number):
    """ Function to remove a constraint equation from the IN.DAT data
    dictionary

    :param data: Data dictionary for the IN.DAT information
    :param equation_number: Constraint equation number to remove
    :return: Nothing
    """

    # Check the constraint is in
    if equation_number in data["icc"].value:
        data["icc"].value.remove(equation_number)
        data["icc"].value.sort()

        # Decrease the number of constraint equations parameter by 1
        data["neqns"].value = str(data["neqns"].get_value - 1)
    else:
        print("Equation number {0} not in constraint equations list".
              format(equation_number))


def add_parameter(data, parameter_name, parameter_value):
    """ Function to add/change parameter to the IN.DAT data dictionary

    :param data: Data dictionary for the IN.DAT information
    :param parameter_name: Name of the parameter to add
    :param parameter_value: Value of the parameter to add
    :return: Nothing
    """

    # Check that the parameter is not already in the dictionary
    if parameter_name not in data.keys():
        try:
            parameter_group = find_parameter_group(parameter_name)
            comment = DICT_DESCRIPTIONS[parameter_name]
            param_data = INVariable(parameter_name, parameter_value,
                                    "Parameter", parameter_group, comment)
            data[parameter_name] = param_data

        except KeyError:
            # The dictionary doesn't recognise the variable name
            print("Parameter {0} not recognised. Check!".
                  format(parameter_name))

    # If it is already in there change the value to the new value
    else:
        data[parameter_name].value = parameter_value


def remove_parameter(data, parameter_name):
    """ Function to remove parameter from the IN.DAT data dictionary

    :param data: Data dictionary for the IN.DAT information
    :param parameter_name: Name of the parameter to remove
    :return: Nothing
    """

    # Check that the parameter exists in the data dictionary
    if parameter_name in data.keys():
        del data[parameter_name]

    # Inform the user that the parameter requested for deletion isn;t in the
    # data dictionary
    else:
        print("Parameter {0} not in IN.DAT".format(parameter_name))


def change_fimp(data, fimp_id, fimp_val):
    """Function to change value in fimp array

    :param data: Data dictionary for the IN.DAT information
    :param fimp_id: impurity fraction array index
    :param fimp_val: new impurity fraction array value
    :return:
    """
    data["fimp"].value[fimp_id] = fimp_val


def change_zref(data, zref_id, zref_val):
    """Function to change value in zref array

    :param data: Data dictionary for the IN.DAT information
    :param zref_id: zref array index
    :param zref_val: new zref array value
    :return:
    """
    data["zref"].value[zref_id] = zref_val


def add_bound(data, bound, bound_type, bound_value):
    """ Function to add/change a bound to the bounds entry in the IN.dat data
    dictionary

    :param data: Data dictionary for the IN.DAT information
    :param bound: Bound number associated with iteration variable number to
                  change
    :param bound_type: States whether bound is upper of lower bound
    :param bound_value: New value of the bound
    :return: Nothing
    """

    # Put bound type into lower cases for consistency
    bound_type = bound_type.lower()

    # if the bound is not in the bounds dictionary initialise an empty
    # dictionary and assign new bound
    if bound not in data["bounds"].value.keys():
        data["bounds"].value[bound] = dict()
        data["bounds"].value[bound][bound_type] = str(bound_value)

    # If bound already exists change value
    elif bound in data["bounds"].value.keys():
        data["bounds"].value[bound][bound_type] = str(bound_value)

    # Bound not recognised.
    else:
        print("Bound {0} not recognised. Check type == string".format(bound))


def remove_bound(data, bound, bound_type):
    """ Function to remove a bound from the bounds entry in the IN.DAT data
    dictionary

    :param data: Data dictionary for the IN.DAT information
    :param bound: Bound number associated with iteration variable number to
                  change
    :param bound_type: States whether bound is upper or lower bound
    :return: Nothing
    """

    # use local variable for cleanliness
    bounds = data["bounds"].value

    # If the bound exists (and is of the correct type) in the bounds dictionary
    if bound in bounds.keys() and bound_type in bounds[bound].keys():
            del bounds[bound][bound_type]

            # if the bound number is now an empty dictionary delete it also
            if len(bounds[bound].keys()) == 0:
                del bounds[bound]


def parameter_type(name, value):
    """ Function to return value in correct format for altering values etc.

    :param name: Name of parameter to check type
    :param value: Value of parameter to format
    :return: Formatted value
    """

    # Find parameter type from PROCESS dictionary
    param_type = DICT_VAR_TYPE[name]

    # Check if parameter is a list
    if isinstance(value, list):

        # Real array parameter
        if "real_array" in param_type:
            return [float(item) for item in value]

        # Integer array parameter
        elif "int_array" in param_type:

            return [int(item) for item in value]

    # Check if parameter is a string
    elif isinstance(value, str):

        # If a real variable just convert to float
        if "real_variable" in param_type:
            return float(value)

        # If a real array split and make a float list
        elif "real_array" in param_type:
            value = value.split(",")
            return [float(item) for item in value]

        # If an integer variable convert to integer
        elif "int_variable" in param_type:
            return int(value)

        # If an integer array split and make an integer list
        elif "int_array" in param_type:
            value = value.split(",")
            return [int(item) for item in value]

        # If type unknown return original value
        else:
            return value

    # If type is other return original value
    else:
        return value


def variable_constraint_type_check(item_number, var_type):
    """ Function to put input into correct format for altering values etc.

    :param item_number: Number associated with variable or constraint
    :param var_type: States whether item is iteration variable or constraint
                     equation
    :return: Formatted item_number
    """

    # Check if item is in string format
    if isinstance(item_number, str):

        # Try evaluate and convert to an integer. Warning if number is rounded
        try:
            # eval should produce int of float otherwise raise the ValueError
            item_number = eval(item_number)

            # Integer
            if isinstance(item_number, int):
                return item_number

            # number must be float if exception not raised
            elif item_number.is_integer():
                return int(item_number)

            # rounded float number with warning
            else:
                print("Value {0} for {1} not an integer. Value rounded to {2}."
                      " Check!".format(item_number, var_type,
                                       int(item_number)))
                return int(item_number)

        except ValueError:
            print("Value {0} for {1} not valid. Check value!".
                  format(item_number, var_type))

    # Check if item is in float format
    elif isinstance(item_number, float):

        # If integer convert to float and return
        if item_number.is_integer():
            return int(item_number)

        # If not an integer warn of rounding and return rounded integer
        else:
            print("Value {0} for {1} not an integer. Value rounded to {2}. "
                  "Check!".format(item_number, var_type, int(item_number)))
            return int(item_number)

    # If already an integer return unchanged
    elif isinstance(item_number, int):
        return item_number

    # Value not recognised
    else:
        print("Value {0} for {1} not a recognised format. Check value!".
              format(var_type))


def variable_bound_check(bound_number, bound_type):
    """ Function to put bound_number and bound_type into correct format

    :param bound_number: Bound number to check
    :param bound_type: States whether bound is upper or lower bound
    :return: Formatted bound number and bound type
    """

    # put bound type into lower case for consistency
    bound_type = bound_type.lower()

    # check if bound is one of the allowed values if not warn user
    if bound_type not in ["l", "u", "upper", "lower"]:
        print("Bound type '{0}' not recognised. Must be one of "
              "['u', 'l', 'U', 'L', 'lower', 'upper', 'LOWER', 'UPPER']".
              format(bound_type))

    # if bound is given as full word shorten for consistency for dictionary
    # keys
    elif bound_type in ["upper", "lower"]:
        if bound_type == "upper":
            bound_type = "u"
        elif bound_type == "lower":
            bound_type = "l"

    # Format bound number value
    # If a string return unchanged
    if isinstance(bound_number, str):
        return bound_number, bound_type

    # If an int convert to string
    elif isinstance(bound_number, int):
        return str(bound_number), bound_type

    # If a float convert to str but warn of rounding when changing from float
    # to int
    elif isinstance(bound_number, float):
        if bound_number.is_integer():
            return int(bound_number), bound_type
        else:
            bound_number = int(bound_number)
            print("Bound number {0} not an integer. "
                  "Value rounded to {2}".format(bound_number,
                                                int(bound_number)))
            return bound_number, bound_type


class INVariable(object):
    def __init__(self, name, value, v_type, parameter_group, comment):
        """ Class to stores the information of a single variable from the
        IN.DAT file

        :param name: Item name
        :param value: Item value
        :param v_type: Type of item
        :param parameter_group: PROCESS variable group item belongs to
        :param comment: Comment for item
        :return: Nothing
        """

        self.name = name
        self.value = value
        self.v_type = v_type
        self.parameter_group = parameter_group
        self.comment = comment

    @property
    def get_value(self):
        """Return value in correct format"""
        if self.v_type != "Bound":
            return parameter_type(self.name, self.value)
        else:
            return self.value


class InDat(object):
    """
        Class 'InDat' for handling IN.DAT data. It handles

            - Reading IN.DAT files
            - Writing IN.DAT files
            - Storing information in dictionary for use in other codes
            - Alterations to IN.DAT
    """

    def __init__(self, filename="IN.DAT"):
        """ Initialise class

        :param filename: Name of input IN.DAT
        :return: Nothing
        """

        self.filename = filename

        # Initialise parameters
        self.in_dat_lines = list()
        self.data = dict()

        # read in IN.DAT
        self.read_in_dat()

    def read_in_dat(self):
        """Function to read in 'self.filename' and put data into dictionary
        'self.data'
        """

        # Read in IN.DAT
        with open(self.filename) as indat:
            self.in_dat_lines = indat.readlines()

        # Remove empty lines from the file
        self.in_dat_lines = remove_empty_lines(self.in_dat_lines)

        for line in self.in_dat_lines:

            # Put everything in lower case
            l_line = line.lower()

            # find the type of the line:
            # [constraint equation, iteration variable, bound, parameter]
            line_type = find_line_type(l_line)

            # Ignore title, header and commented lines
            if line_type != "Title" and line_type != "Comment":

                try:
                    # for non-title lines process line and store data.
                    process_line(self.data, line_type, l_line)
                except KeyError:
                    print("Warning: Line below is causing a problem. Check "
                          "that line in IN.DAT is valid. Line skipped!\n{0}".
                          format(line))

    def add_iteration_variable(self, variable_number):
        """ Function to add iteration variable to IN.DAT data dictionary

        :param variable_number: Iteration variable number to add
        :return: Nothing
        """

        # format iteration variable number
        variable_number = variable_constraint_type_check(variable_number,
                                                         "iteration variable")
        # add iteration variable to IN.DAT data dictionary
        add_iteration_variable(self.data, variable_number)

    def remove_iteration_variable(self, variable_number):
        """ Function to remove iteration variable to IN.DAT data dictionary

        :param variable_number: Iteration variable number to remove
        :return: Nothing
        """

        # format iteration variable number
        variable_number = variable_constraint_type_check(variable_number,
                                                         "iteration variable")
        # remove iteration variable from IN.DAT data dictionary
        remove_iteration_variable(self.data, variable_number)

    def add_constraint_equation(self, equation_number):
        """ Function to add constraint equation to IN.DAT data dictionary

        :param equation_number: Constraint equation number to add
        :return: Nothing
        """

        # format constraint equation number
        equation_number = variable_constraint_type_check(equation_number,
                                                         "constraint equation")

        # add constraint equation to IN.DAT data dictionary
        add_constraint_equation(self.data, equation_number)

    def remove_constraint_equation(self, equation_number):
        """ Function to remove a constraint equation from IN.DAT data
        dictionary

        :param equation_number: Constraint equation number to remove
        :return: Nothing
        """

        # format constraint equation number
        equation_number = variable_constraint_type_check(equation_number,
                                                         "constraint equation")

        # remove constraint equation from IN.DAT data dictionary
        remove_constraint_equation(self.data, equation_number)

    def add_parameter(self, parameter_name, parameter_value):
        """ Function to add/change parameter to IN.DAT data dictionary

        :param parameter_name: Name of parameter to add/change
        :param parameter_value: Value of parameter to add/change
        :return: Nothing
        """

        # add/change parameter to/in IN.DAT data dictionary
        add_parameter(self.data, parameter_name, parameter_value)

    def change_fimp(self, fimp_index, fimp_value):
        """Function to change value of an impurity fraction

        :param fimp_index: index of impurity fraction array to change
        :param fimp_value: value to change impurity fraction to
        :return:
        """

        # change impurity fraction value in IN.DAT data dictionary
        change_fimp(self.data, fimp_index, fimp_value)

    def change_zref(self, zref_index, zref_value):
        """Function to change value of zref

        :param zref_index: index of zref to change
        :param zref_value: value to change zref entry to
        :return:
        """

        # change zref value in IN.DAT data dictionary
        change_zref(self.data, zref_index, zref_value)

    def remove_parameter(self, parameter_name):
        """ Function to remove parameter from IN.DAT data dictionary

        :param parameter_name: Name of parameter to remove
        :return: Nothing
        """

        # remove parameter from IN.DAT data dictionary
        remove_parameter(self.data, parameter_name)

    def add_bound(self, bound, bound_type, bound_value):
        """ Function to add/change a bound in IN.DAT data dictionary

        :param bound: Bound number to add/change
        :param bound_type: States whether bound is upper or lower bound
        :param bound_value: Value of bound to add/change
        :return: Nothing
        """

        # format bound number and bound type
        bound, bound_type = variable_bound_check(bound, bound_type)

        # add/change bound to/in IN.DAT data dictionary
        add_bound(self.data, bound, bound_type, bound_value)

    def remove_bound(self, bound, bound_type):
        """ Function to remove a bound from IN.DAT data dictionary

        :param bound: Bound number to remove
        :param bound_type: States whether bound is upper or lower bound
        :return: Nothing
        """

        # format bound number and bound type
        bound, bound_type = variable_bound_check(bound, bound_type)

        # remove bound from IN.DAT data dictionary
        remove_bound(self.data, bound, bound_type)

    def write_in_dat(self, output_filename="new_IN.DAT"):
        """Function to write data to output file called 'output_filename'
        """

        # create and open output file
        output = open(output_filename, "w")

        # Write Header
        write_title("", output)

        # Write Constraint Equations
        write_constraint_equations(self.data, output)

        # Write Iteration Variables
        write_iteration_variables(self.data, output)

        # Write parameters
        write_parameters(self.data, output)

        # close file
        output.close()


if __name__ == "__main__":
    #i = InDat(filename="../../modified_demo1_a31_rip06_2014_12_15.IN.DAT")
    i = InDat(filename="../../target_IN.DAT")
    # print(i.data["ixc"].value)
    # print(i.data["fimp"].value)
    # print(i.data["ipfloc"].value)
    # i.change_fimp(3, 0.5)
    # print(i.data["zref"].value)
    # i.change_zref(3, 0.5)
    # i.remove_constraint_equation(2.5)
    # i.add_constraint_equation("3.0")
    # i.add_constraint_equation("2")
    # i.add_iteration_variable(103)
    # i.add_iteration_variable("2")
    # i.add_iteration_variable(7.5)
    # i.add_iteration_variable("5.5")
    # i.remove_iteration_variable(2)
    # i.remove_iteration_variable("3")
    # i.remove_iteration_variable(4.5)
    # i.remove_iteration_variable("6.5")
    # # Add bound will change the bound value if it already exists
    # i.add_bound(103, "upper", 5.0)
    # i.remove_bound(2, "upper")
    # # Add parameter will change the parameter value if it already exists
    # i.add_parameter("blnktthdsd", 0.5)
    # i.add_parameter("iavail", 1)
    # i.remove_parameter("blnkithsddd")
    # i.remove_parameter("blnkith")
    # i.add_parameter("sweep", [3.0, 3.0])
    print(i.data["bounds"].get_value)
    i.write_in_dat()
