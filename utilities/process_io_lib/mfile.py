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

  Compatible with PROCESS version 271

"""

from process_io_lib import process_dicts


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
            __str__     --> overridden __str__ to give custom output
            set_scan    --> function to set a scan number to a given value
            get_scan    --> function to retrieve a given scan
            get_scans   --> function to retrieve all scans.
        """
        self.var_name = var_name
        self.var_description = var_description

    def __str__(self):
        keys = self.__dict__.keys()
        return """Variable: %s\nVariable Description: """\
            """%s\nValues: %s""" % \
            (self.var_name, self.var_description, str([self.__dict__[item]
            for item in keys if "scan" in item]))

    def set_scan(self, scan_number, scan_value):
        """ Sets the class attribute self.scan# where # is scan number

        Arguments:
          scan_number --> scan number
          scan_value --> value of parameter for scan

        """
        setattr(self, "scan%d" % scan_number, scan_value)

    def get_scan(self, scan):
        """Returns the value of a specific scan. For scan = -1 the last scan is
        given.

        Arguments:
          scan --> scan number to return [-1 = last scan]

        Returns:
          [single scan requested]
        """
        if scan == -1:
            keys = [key for key in self.__dict__.keys() if "scan" in key]
            keys.sort()
            num = len(keys)-1
            return self.__dict__[keys[num]]
        else:
            keys = [item for item in self.__dict__.keys() if "scan%d" % scan
                    in item]
            keys.sort()
            return self.__dict__[keys[scan-1]]

    def get_scans(self):
        """Returns a list of scan values in order of scan number

        Returns:
          [List of all scans for variable]

        """
        temp_keys = [int(item[4:]) for item in self.__dict__.keys()
                     if "scan" in item]
        temp_keys.sort()
        keys = ["scan"+str(item) for item in temp_keys]
        return [self.__dict__[key] for key in keys]


class MFile(object):
    def __init__(self, filename="MFILE.DAT"):
        """
        Class object to store all data from a single MFILE.DAT file.

        Arguments:
        filename --> filename to read in; default value is MFILE.DAT. (file
        must be in MFILE.DAT format)

        The class contains the following:
          get_data        --> Retrieves data from the MFILE for a given scan
          search_keys     --> Search the variable names
          search_des      -->
          make_plot_dat   -->
          __str__         -->
          get_num_scans   -->

        """
        self.filename = filename
        self.data = {}  # Empty dictionary to contain the self.filename data

        # Open file self.filename which is in MFILE.DAT format
        mfile = open(self.filename, "r")

        # Read lines from self.filename
        self.mfile_lines = mfile.readlines()

        # initialise number of scans and scan variables
        self.number_of_scans = 0
        self.scan_variable = 0
        self.process_version = ""
        self.date = ""
        self.time = ""
        self.user = ""

        # Get the number of scans and the scanning variable first
        for temp_line in self.mfile_lines:
            if "Number_of_scan_points" in temp_line:
                self.number_of_scans = int(temp_line.split(" ")[-1].
                                           strip("\n"))
            if "(nsweep)" in temp_line:
                self.scan_variable = temp_line.split(" ")[-1].strip("\n")
            if "(procver)" in temp_line:
                self.process_version = temp_line.split(" ")[-1].strip("\n")
            if "(date)" in temp_line:
                self.date = temp_line.split(" ")[-1].strip("\n")
            if "(time)" in temp_line:
                self.time = temp_line.split(" ")[-1].strip("\n")
            if "(username)" in temp_line:
                self.user = temp_line.rstrip().split(" ")[-1]

        # Check to see if file contained no scans
        if self.number_of_scans == 0:
            self.get_no_scan_data()
        else:
            # For each scan get the data from self.filename
            for scan in range(self.number_of_scans):
                self.get_data(scan+1)

    def get_no_scan_data(self):
        """Function to get file data if there is no scan
        """
        for line in self.mfile_lines:
            temp_line = [tl for tl in line.split(" ") if tl != ""]

            var_name = temp_line[1].split("__")[0]. \
                                    strip("()")

            if var_name == "_" * len(var_name):
                var_name = temp_line[0].split("__")[0]. \
                    replace("_", " ")
            var_des = temp_line[0].split("__")[0]. \
                replace("_", " ").strip(" ")

            # pthrmw variable requires a fudge as it is
            # in the form pthrmw(num).
            if "pthrmw" in var_name:
                var_name += ")"

            # Create instance of MFileVariable class.
            self.data[var_name] = \
                MFileVariable(var_name, var_des)

            # Add scan 0 data to MFileVariable class
            if is_number(temp_line[2]):
                self.data[var_name]. \
                    set_scan(0, float(temp_line[2]))
            else:
                self.data[var_name]. \
                    set_scan(0, temp_line[2])

    def get_data(self, scan):
        """
        Reads the MFILE.DAT data for a single scan (scan number = argument
        'scan') into the dictionary 'self.data'.

        If it is the first scan get_data will create a new instance of the
        MFileVariable class and add the scan data. Otherwise it will add the
        following scans to the already existing MFileVariable class instance.

        Arguments:
          scan --> the scan number to read from self.filename

        """
        # Trigger for when the scan starts in self.filename
        start_read = False

         # Trigger for when the scan ends in self.filename
        stop_read = False

        for line in self.mfile_lines:
            # Create temporary line with empty values removed.
            temp_line = [tl for tl in line.split(" ") if tl != ""]

            # If line doesn't contain only a newline character.
            if temp_line != ["\n"]:
                if not stop_read:
                    if not start_read:
                        # Determines when the requested scan has started
                        if "iscan" in temp_line[1]:
                            if int(temp_line[-1].strip("\n")) == scan:
                                start_read = True
                    else:
                        # Determines when the requested scan has ended
                        if "iscan" in temp_line[1]:
                            if int(temp_line[-1].strip("\n")) != scan:
                                stop_read = True
                        else:
                            # Line formatting to get the variable name
                            # and the variable value.
                            var_name = temp_line[1].split("__")[0]. \
                                replace("_", "").replace("\n", "")

                            if var_name != "":
                                if var_name[-1] == ")":
                                    var_name = var_name[:-1]
                                if var_name[0] == "(":
                                    var_name = var_name[1:]

                            if var_name == "_" * len(var_name):
                                var_name = temp_line[0].split("__")[0]. \
                                    replace("_", " ")
                            var_des = temp_line[0].split("__")[0]. \
                                replace("_", " ").strip(" ")


                            # On first scan in self.filename create
                            # instance of MFileVariable class.
                            if scan == 1:
                                self.data[var_name] = \
                                    MFileVariable(var_name, var_des)

                            # Add scan data to MFileVariable class
                            self.data[var_name]. \
                                set_scan(scan, eval(temp_line[2]))

    def search_keys(self, variable):
        """Searches the dictionary keys for matches to the variable name
        'variable' in arguments.

        Puts everything into lower case before searching.

        Argument:
          variable --> variable name to search for

        Returns:
          matches --> List of matches to the searched for variable

        """
        matches = []
        for key in self.data.keys():
            if variable.lower() in key.lower():
                matches.append(key)
        return matches

    def search_des(self, description):
        """Searches the dictionary descriptions for matches to the description
        'description' from the arguments.

        Puts everything into lower case before searching.

        Argument:
          variable --> variable name to search for

        Returns:
          matches --> List of matches to the searched for description

        """
        descriptions = [self.data[key].var_descript.lower()
                        for key in self.data.keys()]
        matches = []
        for item in descriptions:
            if description.lower() in item.lower():
                matches.append(item)
        return matches

    def make_plot_dat(self, custom_keys, filename="make_plot_dat.out",
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
          custom_keys --> list of parameters to output
          filename --> output filename
          file_format --> 'row' or 'column' make_plot_dat.out

        """

        plot_dat = open(filename, "w")

        # The first two lines contain the scanning variable and the number of
        # scans. These lines are preceded by a # symbol for ease of excluding.
        plot_dat.write("# Scanning Variable: %s" %
                       self.scan_variable + "\n")
        plot_dat.write("# Number of scans: %d" %
                       self.number_of_scans + "\n")
        plot_dat.close()

        # The order of searching is set so that the output variables are always
        # in the same order. i.e. searching custom_keys first as it is a list.
        keys = self.data.keys()

        # Row format
        if file_format == "row":
            for key in custom_keys:
                if key in keys:
                    # Get the scan values for the row
                    values = ""
                    for item in self.data[key].get_scans():
                        values += "%.4e" % item + " "
                    values += "\n"

                    # Create the file line [name, description, val1, val2, ...]
                    # Entries are justified to give the impression of fixed
                    # width columns
                    line = self.data[key].var_description.\
                        replace(" ", "_").ljust(45)\
                        + " " + key.ljust(25) + " " + values

                    # Write row to file.
                    plot_dat = open(filename, "a")
                    plot_dat.write(line)
                    plot_dat.close()

        # Column format
        elif file_format == "column":
            var_descriptions = ""
            var_names = ""
            num_scans = self.number_of_scans
            val_keys = []

            for key in custom_keys:
                if key in keys:
                    var_descriptions += self.data[key].var_description.\
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
                    values += "%.4e " % self.data[vkey].\
                        get_scan(num+1)[0]
                values += "\n"
                plot_dat = open(filename, "a")
                plot_dat.write(values)
                plot_dat.close()
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

    def __str__(self):
        """ Prints intro information about the MFILE data

        Overrides the __str__ function for the class.

        Arguments:
          f_name --> file name
          n_scans --> number of scans
          scan_var --> scanning variable

        Returns:
          The str 'info'

        """
        info = "="*40 + "\n" + "File: %s" % self.filename + "\n" + \
            "Number of scans: %d" % self.number_of_scans + "\n" +\
            "Scan variable: %s" % self.scan_variable + "\n" + \
            "="*40
        return info

    def get_num_scans(self):
        """Returns the number of scans in the file"""
        return self.number_of_scans


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
