"""

  PROCESS MFILE.DAT IO library

  process_io_lib.mfile.

  James Morris
  CCFE

  Notes:
    + 12/03/2014: Initial version
    + 12/03/2014: Added MFILE variable class
    + 12/03/2014: Added MFILE class for containing all info from file.
    + 12/03/2014: Added ability to read MFILE.DAT into class
    + 12/03/2014: Added ability write MFILE.DAT from class
    + 12/05/2014: Fixed mfile issue with strings in MFILE.DAT with no scans
    + 16/05/2014: Cleaned up MFileVariable
    + 19/05/2014: Cleaned up MFile and put some functions outside class.
    + 12/06/2014: Fixed error handling for "variable not in MFILE" errors

  Compatible with PROCESS version 286

"""

import process_dicts
import logging
LOG = logging.getLogger("mfile")


class MFileVariable(object):
    """Class for containing a single mfile variable """
    def __init__(self, var_name, var_description):
        """
        An object class to contain information (and data values) for a single
        variable from the PROCESS machine readable output file (MFILE.DAT).
        Each class can contain all the scan values for the variable.

        Init defines the variable name and description from arguments:
          var_name --> variable name
          var_description --> variable description/long name

        The class contains the following:
            set_scan    --> function to set a scan number to a given value
            get_scan    --> function to retrieve a given scan
            get_scans   --> function to retrieve all scans.
        """
        self.var_name = var_name
        self.var_description = var_description
        LOG.debug("Initialising variable '%s': %s" % (self.var_name,
                                                      self.var_description))

    def set_scan(self, scan_number, scan_value):
        """ Sets the class attribute self.scan# where # is scan number

        Arguments:
          scan_number --> scan number
          scan_value --> value of parameter for scan

        """
        setattr(self, "scan%d" % scan_number, scan_value)
        LOG.debug("Scan %d for variable '%s' == %s" %
                  (scan_number, self.var_name, str(scan_value)))

    def get_scan(self, scan_number):
        """Returns the value of a specific scan. For scan = -1 the last scan is
        given.

        Arguments:
          scan_number --> scan number to return [-1 = last scan]

        Returns:
          [single scan requested]
        """

        scan_keys = [key for key in self.__dict__.keys() if "scan" in key]
        scan_keys.sort()
        if scan_number == -1:
            return self.__dict__[scan_keys[-1]]
        else:
            return self.__dict__[scan_keys[scan_number-1]]

    def get_scans(self):
        """Returns a list of scan values in order of scan number

        Returns:
          [List of all scans for variable]

        """

        scan_numbers = list()
        for key in self.__dict__.keys():
            if "scan" in key:
                scan_number = "".join([num for num in key if num.isdigit()])
                scan_numbers.append(scan_number)
        scan_numbers.sort()
        scan_keys = ["scan"+item for item in scan_numbers]
        return [self.__dict__[key] for key in scan_keys]

    def get_number_of_scans(self):
        """Function to return the number of scans in the variable class"""
        key_list = [key for key in self.__dict__.keys() if "scan" in key]
        return len(key_list)


class MFileErrorClass(object):
    """ Error class for handling missing data from MFILE
    """
    def __init__(self, item):
        self.item = item
        self.get_scan = self.get_error
        self.get_scans = self.get_error
        self.set_scan = self.get_error
        self.get_number_of_scans = self.get_error

    def get_error(self, *args, **kwargs):
        LOG.error("Key '%s' not in MFILE. KeyError! Check MFILE" % self.item)


class MFileDataDictionary(object):
    """ Class object to act as a dictionary for the data.
    """
    def __init__(self):
        pass

    def __getitem__(self, item):
        try:
            return self.__dict__[item]
        except KeyError:
            return MFileErrorClass(item)

    def __setitem__(self, key, value):
        self.__dict__[key] = value

    def keys(self):
        return self.__dict__.keys()


class MFile(object):
    def __init__(self, filename="MFILE.DAT"):
        """Class object to store the MFile Objects"""

        LOG.info("Creating MFile class for file '%s'" % filename)
        self.filename = filename
        self.data = MFileDataDictionary()
        self.mfile_lines = list()
        LOG.info("Opening file '%s'" % self.filename)
        self.open_mfile()
        LOG.info("Parsing file '%s'" % self.filename)
        self.parse_mfile()

    def open_mfile(self):
        """Function to open MFILE.DAT"""
        mfile = open(self.filename, "r")
        self.mfile_lines = mfile.readlines()

    def parse_mfile(self):
        """Function to parse MFILE.DAT"""

        for line in self.mfile_lines:
            c_line = clean_line(line)
            if c_line != [""]:
                self.add_line(c_line)

    def add_line(self, line):
        """Function to read the line from MFILE and add to the appropriate
        class or create a new class if it is the first instance of it.
        """
        var_des = line[0]
        var_name = sort_brackets(line[1])
        var_value = sort_value(line[2])
        self.add_to_mfile_variable(var_des, var_name, var_value)

    def add_to_mfile_variable(self, des, name, value):
        """Function to add value to MFile class for that name/description
        """
        if name == "":
            var_key = des.lower().replace("_", " ")
        else:
            var_key = name.lower().replace("_", " ")

        if var_key in self.data.__dict__.keys():
            scan_num = self.data[var_key].get_number_of_scans()
            self.data[var_key].set_scan(scan_num+1, value)
        else:
            var = MFileVariable(name, des)
            self.data[var_key] = var
            self.data[var_key].set_scan(1, value)


def sort_value(val):
    """Function to sort out value line in MFILE"""
    if '"' in val:
        return str(val.strip('"'))
    else:
        return float(val)


def sort_brackets(var):
    """Function to sort bracket madness on variable name
    """
    if var != "":
        tmp_name = var.lstrip("(").split(")")
        if len(tmp_name) > 2:
            return tmp_name[0] + ")"
        else:
            return tmp_name[0]
    else:
        return ""


def clean_line(line):
    """Cleans an MFILE line into the three parts we care about"""
    cleaned_line = [item.strip("_ \n") for item in line.split(" ")
                    if item != ""]
    return cleaned_line


def search_keys(dictionary, variable):
    """Searches the dictionary keys for matches to the variable name
    'variable' in arguments.

    Puts everything into lower case before searching.

    Argument:
      dictionary --> dictionary to search in
      variable --> variable name to search for

    Returns:
      matches --> List of matches to the searched for variable

    """
    matches = []
    for key in dictionary.keys():
        if variable.lower() in key.lower():
            matches.append(key)
    return matches


def search_des(dictionary, description):
    """Searches the dictionary descriptions for matches to the description
    'description' from the arguments.

    Puts everything into lower case before searching.

    Argument:
      dictionary --> dictionary to search in
      variable --> variable name to search for

    Returns:
      matches --> List of matches to the searched for description

    """
    descriptions = [dictionary[key].var_descript.lower()
                    for key in dictionary.data.keys()]
    matches = []
    for item in descriptions:
        if description.lower() in item.lower():
            matches.append(item)
    return matches


def make_plot_dat(mfile_data, custom_keys, filename="make_plot_dat.out",
                  file_format="row"):
    """Make a make_plot_dat.out file for this MFILE.

    The output format can be changed with the file_format argument.

    file_format = "row" looks like:

      parameter 1     val1    val2    val3    ...
      parameter 2     val1    val2    val3    ...
            ...

    file_format = "column" looks like:

      parameter1  parameter2  parameter3  ...
      val1        val2        val3        ...
      val2        val2        val3        ...
      ...

    The default name for the output file is make_plot_dat.out; this can
    be overwritten using the filename argument. custom_keys is a list of
    variables to output to the make_plot_dat.out.

    Arguments:
      mfile_data --> MFILE object
      custom_keys --> list of parameters to output
      filename --> output filename
      file_format --> 'row' or 'column' make_plot_dat.out

    """

    plot_dat = open(filename, "w")

    # The first two lines contain the scanning variable and the number of
    # scans. These lines are preceded by a # symbol for ease of excluding.
    plot_dat.write("# Scanning Variable: %s" %
                   process_dicts.DICT_SWEEP_VARS
                   [mfile_data.data["nsweep"].get_scan(-1)] + "\n")
    plot_dat.write("# Number of scans: %d" %
                   int(mfile_data.data["isweep"].get_scan(-1)) + "\n")
    plot_dat.close()

    # The order of searching is set so that the output variables are always
    # in the same order. i.e. searching custom_keys first as it is a list.
    keys = mfile_data.data.keys()

    # Row format
    if file_format == "row":
        write_row_mplot_dat(filename, custom_keys, mfile_data)
    elif file_format == "column":
        write_column_mplot_dat(filename, custom_keys, mfile_data)
    else:
        # If file_format not recognised print error
        print("# Error >> Format %s not recognised. Use row or column" %
              file_format)

    for ckey in custom_keys:
        if ckey not in keys:
            # For each item in the custom_keys list that isn't in the file
            # print out an error.
            print(" # Error >> Key: '%s' was NOT found in MFILE.DAT!" %
                  ckey)
            print(" \t\t (So is NOT in make_plot_dat.out!)")


def write_row_mplot_dat(filename, custom_keys, mfile_data):
    """Function to make a PLOT.DAT using MFILE in row format"""

    mfile_keys = mfile_data.data.keys()

    for key in custom_keys:
        if key in mfile_keys:
            # Get the scan values for the row
            values = ""
            for item in mfile_data.data[key].get_scans():
                values += "%.4e" % item + " "
            values += "\n"

            # Create the file line [name, description, val1, val2, ...]
            # Entries are justified to give the impression of fixed
            # width columns
            line = mfile_data.data[key].var_description.\
                replace(" ", "_").ljust(45)\
                + " " + key.ljust(25) + " " + values

            # Write row to file.
            plot_dat = open(filename, "a")
            plot_dat.write(line)
            plot_dat.close()


def write_column_mplot_dat(filename, custom_keys, mfile_data):
    """Function to make a PLOT.DAT using MFILE in column format"""

    var_descriptions = ""
    var_names = ""
    num_scans = int(mfile_data.data["isweep"].get_scan(-1))
    val_keys = []
    mfile_keys = mfile_data.data.keys()

    for key in custom_keys:
        if key in mfile_keys:
            var_descriptions += mfile_data.data[key].var_description.\
                replace(" ", "_").ljust(45) + " "
            var_names += key.ljust(10) + " "
            val_keys.append(key)

    # Write row to file
    plot_dat = open(filename, "a")
    plot_dat.write(var_names+"\n")
    plot_dat.close()

    # Write rows of values. One row for each scan.
    for num in range(num_scans):
        values = ""
        for vkey in val_keys:
            values += "%.4e " % mfile_data.data[vkey].\
                get_scan(num+1)
        values += "\n"
        plot_dat = open(filename, "a")
        plot_dat.write(values)
        plot_dat.close()


def read_mplot_conf(filename="make_plot_dat.conf"):
    """Read make_plot_dat.conf file and return list of parameters.

    The make_plot_dat.conf file contains the parameters to be output by the
    MFILE class to the make_plot_dat.out file.

    Arguments:
      filename --> config file name

    Returns:
      conf_params --> list of parameters in config file

    """
    # Open config file
    conf_file = open(filename, "r")
    conf_lines = conf_file.readlines()
    conf_params = []

    # For each line check the it is not a comment (#) and if not add it to the
    # list of parameters.
    for item in conf_lines:
        if "#" not in item:
            if item.strip("\n") != "":
                conf_params.append(item.strip("\n"))
    conf_file.close()

    return conf_params


def write_mplot_conf(filename="make_plot_dat.conf"):
    """write make_plot_dat.conf file.

    Creates a new/overwrites the existing make_plot_dat.conf file using the
    default values listed in PARAMETER_DEFAULTS.

    Arguments:
      filename --> config file name

    """
    conf_file = open(filename, "w")
    conf_file.write("# make_plot_dat.out config file.\n")
    for item in process_dicts.PARAMETER_DEFAULTS:
        conf_file.write(item + "\n")
    conf_file.close()


def is_number(val):
    """Check MFILE data entry"""
    try:
        float(val)
        return True
    except ValueError:
        pass

    return False


#def test():
#    """Testing"""
#    from process_funcs import setup_logging
#    setup_logging(default_path="logging.json")
#    m = MFile("../../STORRISI_MFILE.DAT")
#    print(m.data["rmajor"].get_scans())
#    print(m.data["procver"].get_scans())
#    print(m.data["isweep"].get_scans())
#    print(m.data["ttfsec"].get_scans())
#    print(m.data["nsweepa"].get_scan(-1))

#if __name__ == "__main__":
#    test()