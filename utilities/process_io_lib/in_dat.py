"""

  PROCESS IN.DAT IO library

  process_io_lib.in_dat.

  James Morris
  CCFE

"""

from os.path import abspath

from process_io_lib.process_dicts import DICT_VAR_TYPE as VAR_TYPE


class INVariable(object):
    def __init__(self, name, value, comment=""):
        """ IN.DAT variable class

        Stores the information of a single variable from the IN.DAT file.

        Arguments:
          name      --> Name of variable
          value     --> Value of variable
          comment   --> Inline comment from IN.DAT

        """
        self.name = name
        self.value = value
        self.comment = comment


class INModule(object):
    def __init__(self, name):
        """ IN.DAT module object

        Stores the variables from a given module in the IN.DAT. Modules are the
        sections of the IN.DAT separated by $MODULE -> $END.

        Arguments:
          name --> module name

        """
        self.name = name
        self.number_of_variables = 0

        # Dictionary to store all lines
        self.store = {}

        # Dictionary for variables only
        self.variables = {}

        # List to remember the order inl IN.DAT
        self.order = []

    def add_variable(self, var):
        """ Adds variable object to module object

        Arguments:
          var --> variable object of type INVariable from IN.DAT

        """
        self.variables[var.name.lower()] = var
        self.store[var.name.lower()] = var
        self.store[var.name.lower()].order = self.number_of_variables
        self.number_of_variables += 1
        self.order.append(var.name.lower())

    def add_line(self, line):
        """ Adds non variable lines to module object

        Arguments:
          line --> string line of IN.DAT belonging to this module

        """
        self.store[self.number_of_variables] = line
        self.order.append(self.number_of_variables)
        self.number_of_variables += 1

    def get_var(self, var_name):
        """ Returns variable object matching var_name

        Arguments:
          var_name --> variable name

        """
        return self.variables[var_name]

    def remove_variable(self, variable_name):
        """ Removes a variable from the module

        Arguments:
          variable_name --> name of variable to remove

        """
        del self.variables[variable_name]
        del self.store[variable_name]
        self.order.remove(variable_name)
        self.number_of_variables -= 1

    def add_constraint_eqn(self, equation_number):
        """Function to add a constraint number to the list of constraint
        equations

        Arguments:
          equation_number --> constraint equation number"""
        if equation_number not in self.variables["icc"].value:
            self.variables["icc"].value.append(equation_number)
            self.variables["icc"].value.sort()
            self.variables["neqns"].value += 1
        else:
            print("Constraint equation {} already in the constraint equation"
                  "list.".format(equation_number))

    def remove_constraint_eqn(self, equation_number):
        """Function to remove a constraint number to the list of constraint
        equations

        Arguments:
          equation_number --> constraint equation number"""
        if equation_number in self.variables["icc"].value:
            self.variables["icc"].value.remove(equation_number)
            self.variables["icc"].value.sort()
            self.variables["neqns"].value -= 1
        else:
            print("Constraint equation {} not in the constraint equation"
                  "list.".format(equation_number))

    def add_iteration_variable(self, variable_number):
        """Function to add an iteration variable to the list of iteration
        variables.

        Arguments:
          variable_number --> the iteration variable number
        """
        if variable_number not in self.variables["ixc"].value:
            self.variables["ixc"].value.append(variable_number)
            self.variables["ixc"].value.sort()
            self.variables["nvar"].value += 1
        else:
            print("Iteration variable {} already in the iteration variable"
                  "list.".format(variable_number))

    def remove_iteration_variable(self, variable_number):
        """Function to remove an iteration variable from the list of iteration
        variables.

        Arguments:
          variable_number --> the iteration variable number
        """
        if variable_number in self.variables["ixc"].value:
            self.variables["ixc"].value.remove(variable_number)
            self.variables["ixc"].value.sort()
            self.variables["nvar"].value -= 1
        else:
            print("Iteration variable {} not in the iteration variable"
                  "list.".format(variable_number))


class INDATClassic(object):
    def __init__(self, filename="IN.DAT"):
        """ Class for reading and writing IN.DAT file with module headings

        Reads IN.DAT data to object, which can be altered and then written
        again to a new IN.DAT. This class is used for IN.DAT files that have
        the classic layout of modules ($MODULE, $END) enclosing input values.

        Arguments:
          filename --> name of IN.DAT to read

        """
        self.filename = filename
        self.module_data = {}
        self.order = []

        # Read in data from self.filename
        self.read_in_dat()

    def read_in_dat(self):
        """ Read in data from IN.DAT

        Reads in data from IN.DAT and for each module creates a INModule
        instance and for each input parameter for that module it creates an
        INVariable class instance linked to the module class.

          i.e. module_data[module_name] = module_class.variable

        """
        # Open IN.DAT (or self.filename) and read lines
        with open(self.filename, "r") as in_dat:
            in_dat_lines = in_dat.readlines()
    
            # Clean the lines using self.clear_lines
            clean_lines = clear_lines(in_dat_lines)
    
            for line in clean_lines:
    
                # If the module delimiter '$' present then make sure it is not the
                # end of the module
                if "$" in line:
                    if "END" not in line:
                        module = line.strip("$ \n \r").replace(" ", "")
                        self.order.append(module)
                        self.module_data[module] = INModule(module)
    
                # Keep empty lines. self.clear_lines already swapped multiple
                # empty lines with a single empty line.
                elif line == "":
                    self.module_data[module].add_line(line)
    
                else:
                    # If line starts with a '*' then the line is commented out so
                    # will be added as a non-variable string line.
                    if line[0] == "*":
                        self.module_data[module].add_line(line.strip("\n"))
    
                    else:
    
                        # If there is a '*' elsewhere in the line then it is
                        # assumed that there is an inline comment
                        if "*" in line:
                            var_comment = " *"+line.split("*")[-1].strip("\n")
                            var_name = line.split("=")[0].replace(" ", "")
                            var_value = line.split("=")[1].split("*")[0]
                            var_value = variable_type(var_name, var_value)
                            # Create variable class and add it to the module class
                            var = INVariable(var_name, var_value,
                                             comment=var_comment)
                            self.module_data[module].add_variable(var)
    
                        else:
                            # Finally if it is a regular variable line
                            var_name = line.split("=")[0].replace(" ", "")
                            var_value = line.split("=")[1].lstrip().rstrip()
                            var_value = variable_type(var_name, var_value)
                            # Create variable class and add it to the module
                            # class
                            var = INVariable(var_name, var_value)
                            self.module_data[module].add_variable(var)

    def write_in_dat(self, filename="new_IN.DAT"):
        """ Write a new IN.DAT (default name is new_IN.DAT)

        Arguments:
          filename --> file to write IN.DAT to.

        """

        # Create new file
        with open(filename, "w") as new_in_file:
            # Use self.order to create new IN.DAT with same order as old IN.DAT
            for item in self.order:
                for line in self.module_data[item].order:
                    if str(line).isdigit():
                        newline = self.module_data[item].store[line]
                    else:
                        newline = self.module_data[item].variables[line].name + \
                            " = " + \
                            str(self.module_data[item].variables[line].value) + \
                            "," + \
                            self.module_data[item].variables[line].comment
                    new_in_file.write(newline+"\n")


class INDATErrorClass(object):
    """ Error class for handling missing data from INDAT
    """
    def __init__(self, item):
        self.item = item

    @property
    def name(self):
        self.get_error()

    @property
    def value(self):
        self.get_error()

    @property
    def comment(self):
        self.get_error()

    def get_error(self, *args, **kwargs):
        print("Key '{}' not in IN.DAT. KeyError! Check IN.DAT".format(self.item))

    @property
    def exists(self):
        return False


class INDATDataDictionary(dict):
    """ Class object to act as a dictionary for the data.
    """

    def __getitem__(self, item):
        try:
            return self[item]
        except KeyError:
            return INDATErrorClass(item)


class INDATNew(object):
    def __init__(self, filename="IN.DAT"):
        """ Class for reading and writing IN.DAT file with module headings

        Reads IN.DAT data to object, which can be altered and then written
        again to a new IN.DAT. This class is used for IN.DAT files that have
        the new layout of no modules enclosing input values. The IN.DAT has
        either comments ('*') lines or input values.

        Arguments:
          filename --> name of IN.DAT to read

        """
        self.filename = filename
        self.number_of_lines = 0

        # Dictionary to variables only
        self.variables = INDATDataDictionary()

        # Dictionary for data
        self.data = INDATDataDictionary()

        # List to remember the order of IN.DAT
        self.order = []

        # Read in data from self.filename
        self.read_in_dat()

    def read_in_dat(self):
        """Read in data from IN.DAT"""
        print("Attempting to open", abspath(self.filename))
        with open(self.filename, "r") as in_dat:
            in_dat_lines = in_dat.readlines()
            clean_lines = clear_lines(in_dat_lines)
            for line in clean_lines:
                if "$" in line:
                    pass
                elif line == "":
                    self.data[self.number_of_lines] = line
                    self.order.append(self.number_of_lines)
                    self.number_of_lines += 1
                else:
                    if line[0] == "*":
                        self.data[self.number_of_lines] = line.strip("\n")
                        self.order.append(self.number_of_lines)
                        self.number_of_lines += 1
                    else:
                        if "*" in line:
                            var_comment = " *"+line.split("*")[-1].strip("\n")
                            var_name = line.split("=")[0].rstrip().lower()
                            var_value = line.split("=")[1].split("*")[0]
                            var_value = variable_type(var_name, var_value)
                            var = INVariable(var_name, var_value,
                                             comment=var_comment)
                            self.data[var_name.lower()] = var
                            self.variables[var_name.lower()] = var
                            self.order.append(var_name.lower())
                        else:
                            var_name = line.split("=")[0].rstrip().lower()
                            var_value = line.split("=")[1].lstrip().rstrip()
                            var_value = variable_type(var_name, var_value)
                            var = INVariable(var_name, var_value)
                            self.data[var_name.lower()] = var
                            self.variables[var_name.lower()] = var
                            self.order.append(var_name.lower())

    def write_in_dat(self, filename="new_IN.DAT"):
        """ Write a new IN.DAT

        Arguments:
          filename --> filename to write new IN.DAT to

        """
        with open(filename, "w") as new_in_file:

            # Use self.order to maintain same order as original IN.DAT
            for item in self.order:
                if str(item).isdigit():
                    newline = self.data[item]
                else:
                    newline = self.variables[item].name + \
                        " = " + \
                        str(self.variables[item].value).replace("[", "").\
                        replace("]", "") + "," + \
                        self.variables[item].comment
                new_in_file.write(newline+"\n")

    def add_variable(self, var):
        """ Adds a variable from the IN.DAT class

        Arguments:
          variable_name --> name of variable to add

        """
        self.variables[var.name.lower()] = var
        self.data[var.name.lower()] = var
        self.order.append(var.name.lower())
        self.number_of_lines += 1

    def remove_variable(self, variable_name):
        """ Removes a variable from the IN.DAT class

        Arguments:
          variable_name --> name of variable to remove

        """
        if variable_name in self.data.keys():
            del self.variables[variable_name]
            del self.data[variable_name]
            self.order.remove(variable_name)
            self.number_of_lines -= 1
        else:
            print("Variable {} not in IN.DAT! Check code!".format(variable_name))
        
    def add_constraint_eqn(self, equation_number):
        """Function to add a constraint number to the list of constraint
        equations

        Arguments:
          equation_number --> constraint equation number"""
        if equation_number not in self.variables["icc"].value:
            self.variables["icc"].value.append(equation_number)
            self.variables["icc"].value.sort()
            self.variables["neqns"].value += 1
        else:
            print("Constraint equation {} already in the constraint equation"
                  "list.".format(equation_number))

    def remove_constraint_eqn(self, equation_number):
        """Function to remove a constraint number to the list of constraint
        equations

        Arguments:
          equation_number --> constraint equation number"""
        if equation_number in self.variables["icc"].value:
            self.variables["icc"].value.remove(equation_number)
            self.variables["icc"].value.sort()
            self.variables["neqns"].value -= 1
        else:
            print("Constraint equation {} not in the constraint equation"
                  "list.".format(equation_number))

    def add_iteration_variable(self, variable_number):
        """Function to add an iteration variable to the list of iteration
        variables.

        Arguments:
          variable_number --> the iteration variable number
        """
        if variable_number not in self.variables["ixc"].value:
            self.variables["ixc"].value.append(variable_number)
            self.variables["ixc"].value.sort()
            self.variables["nvar"].value += 1
        else:
            print("Iteration variable {} already in the iteration variable"
                  "list.".format(variable_number))

    def remove_iteration_variable(self, variable_number):
        """Function to remove an iteration variable from the list of iteration
        variables.

        Arguments:
          variable_number --> the iteration variable number
        """
        if variable_number in self.variables["ixc"].value:
            self.variables["ixc"].value.remove(variable_number)
            self.variables["ixc"].value.sort()
            self.variables["nvar"].value -= 1
        else:
            print("Iteration variable {} not in the iteration variable"
                  "list.".format(variable_number))


def clear_lines(lines):
        """ Returns a new list of lines with rubbish removed

        Clears lines of:
          + multiple empty lines replaced with single empty lines
          + removes lines preceded by or including multiple *s

        Arguments:
          lines --> lines to clear up

        Returns:
          new_lines --> list of lines that are required.

        """
        new_lines = []
        n = len(lines)
        for i in range(0, n, 1):
            temp_line = ""

            # Ignore lines that contain multiple *s
            if "***" not in lines[i]:
                # Ignore lines starting with $ or *
                if lines[i][0] != "*" and lines[i][0] != "$":
                    if len(lines[i].split(" ")) != 1:
                        temp_line = lines[i]
                    elif len(lines[i].split("=")) != 1:
                        temp_line = lines[i]
                else:
                    temp_line = lines[i]

            if temp_line != "":
                if temp_line.split(" ")[0] == "":
                    new_lines[-1] = new_lines[-1] + temp_line
                else:
                    new_lines.append(temp_line)
            else:
                if len(new_lines):
                    # Check that previous line wasn't also an empty line
                    if new_lines[-1] != "":
                        new_lines.append(temp_line)
        return new_lines


def variable_type(var_name, var_value):
    """ Function to return the variable value in its correct form

    Arguments:
      var_name  --> variable name
      var_value --> variable value

    """
    var_name = var_name.lower()
    if "bound" in var_name.lower() or "zref(" in var_name.lower():
        val = fortran_python_scientific(var_value).replace(",", "")
        return float(val)
    elif "ixc(" in var_name.lower() or "icc(" in var_name.lower():
        return int(var_value)
    elif "fimp(" in var_name.lower():
        val = fortran_python_scientific(var_value).replace(",", "")
        return float(val)
    elif var_name not in VAR_TYPE:
        print("Variable: {}".format(var_name))
        print("variable not in variable type list. Please check the "
              "process_dicts file! Variable type left as string")
        var_value = var_value.replace(",", "")
        return var_value

    if VAR_TYPE[var_name] == 'int_variable':
        val = fortran_python_scientific(var_value.replace(",", "").rstrip())
        return int(float(val))

    elif VAR_TYPE[var_name] == 'int_array':
        val = [fortran_python_scientific(vl) for vl
               in var_value.split(",") if vl != ""]
        return [int(float(value)) for value in val]

    elif VAR_TYPE[var_name] == 'real_variable':
        val = fortran_python_scientific(var_value.replace(",", "").rstrip())
        return float(val)

    elif VAR_TYPE[var_name] == 'real_array':
        val = [fortran_python_scientific(vl) for vl
               in var_value.split(",") if vl != ""]
        return [float(value) for value in val]

    else:
        print("Variable: {}".format(var_name))
        print("variable type not recognised. Please check the process_dicts"
              "file! Variable type left as string")
        return var_value
    pass


def fortran_python_scientific(var_value):
    """Convert FORTRAN scientific notation to Python notation
    
    Arguments:
      var_value --> variable value as type string!
    """
    return var_value.replace("D", "e").replace("d", "e")
