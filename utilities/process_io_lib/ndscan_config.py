"""
Author: Steven Torrisi, University of Rochester

Contains only a class that controls NdScan Config Files.

Config files currently only support a restricted number of variables.
You can add your own if you are a developer through
the create custom variable method.


Class: NdScanConfigFile

Methods:
    set_value(varname,value): Assign a value to varname, returns False if the
                              varname does not exist/is not supported.

    get_value(varname): Returns the value of the variable specified.

    add_axis(name,lowerbound,upperbound,steps): Adds an axis to scan along
                                          with the appropriate information.

    del_axis(dimid): The dimension ID can be one of two things; a string which
                     refers to the varname, or an integer representing the
                     index of the axis in the Axes list.

    read_ndscanfconfig_file(filename): Reads a config file to store it in the
                                       class for modification.

    write_config_file(filename,majormod=false): Writes a config file with the
        internally stored data. Set majormod to true to set RanBefore to False.

    modifyconfigfile(filename,varname,value,majormod): Reads a config file,
             changes varname to value, saves it, but if majormod is true sets
             RanBefore to False.

    create_custom_variable(varname,value): Creates a new variable in the file.
                                           Only for developers.


Supported Variables below.

Required:   Axes               (at least one)
                -Varname (string)
                -Lowerbound   (float)
                -Upperbound   (float)
                -Steps        (integer number of evaluations, must be greater
                              than 1)

Optional:   OutputDirectory     (Where to save the mfiles)
            VariablesOfInterst  (What variables swill be extracted from Mfiles)
            Author
            Title       (Will be used in naming the NetCDF file)
            Comment     (For reading the config file)
            Description (Saved in the netcdf file)
            RanBefore   (Used for warnings later)

"""
__author__ = 'steto'

import json
import collections as col
#from process_io_lib.configuration import Config

class NdScanConfigFile(object):

    """
    Class for creating, modifying, and writing a ndscan.conf file.
    Methods:
        set_value(varname,value):
        Assign a value to varname, returns False if the varname does not
        exist/is not supported.

        get_value(varname):
        Returns the value of the variable specified.

        add_axis(name,lowerbound,upperbound,steps):
        Adds an axis to scan along with the appropriate information.

        del_axis(dimid):
        The dimension ID can be one of two things; a string which refers to
        the varname, or an integer representing the index of the axis in the
        Axes list.

        read_ndscanfconfig_file(filename):
        Reads a config file to store it in the class for modification.

        write_config_file(filename,majormod=false):
        Writes a config file with the internally stored data. Set majormod to
        true to set RanBefore to False.

        modifyconfigfile(filename,varname,value,majormod):
        Reads a config file, changes varname to value, saves it, but if
        majormod is true sets RanBefore to False.

        create_custom_variable(varname,value):
        Creates a new variable in the file. Only for developers.
    """

    def __init__(self, configfile="ndscan.conf"):
        """
        Initiates the internal dictionary and fills them with blank values.

        self.internaldict stores entries for all the variables.

        Init defines the internal dict and sets blank values for the expected
        inputs.
        It also establishes 'majormodmade' as false. If a major modification
        is made to the config file,
        such as changing an axis or an optional, majormodmade toggles to True,
        setting the ndscan.conf file's "RanBefore"
        flag to False.

        It's important that values that aren't defined are set to "None",
        because then the writer
        function knows to skip them when producing the config file from the
        internal dict.

        """

        self.internaldict = {}
        self.internaldict['Axes'] = []

        # Optional items are specified as 'None' and will be corrected later.
        # if the user tries to ad information.
        self.internaldict['OutputDirectory'] = None
        self.internaldict['Optionals'] = None
        self.internaldict['VariablesOfInterest'] = []
        self.internaldict["Author"] = None
        self.internaldict["Title"] = None
        self.internaldict["Comment"] = None
        self.internaldict["Description"] = None
        self.internaldict["RanBefore"] = None

        self.majormodmade = False
        self.read_ndscanconfig_file(configfile)


    def create_custom_variable(self, varname, value):
        """
        Adds a custom variable which is NOT already in internaldict. Only for
        developers.
        If the varname is already in internaldict, then sets it to the new
        value anyway.
        Arguments:
            varname---> The name of the new variable
            value ----> The value of the new variable

        Returns:
            True, if the variable is in internaldict already, or if the
            variable was written successfully.

        """
        if varname in self.internaldict.keys():
            print("Value is already in internaldict!")
            return self.set_value(varname, value)

        else:
            self.internaldict[varname] = value
            return True

    def set_value(self, varname, value):
        """
        Sets the value of a variable in the internaldict.
        Note that you can delete an item by setting value equal to None.

        Arguments:
            varname---> The name of the new variable
            value ----> The value of the new variable
        Returns:
            TRUE if the variable was found and the value assigned.
            FALSE if varname is not a key in internaldict.
        """
        if varname in self.internaldict.keys():
            self.internaldict[varname] = value
            return True
        else:
            print("Variable not found. JSON requires precision and case\
 sensitivity so exercise caution.")
            return False

    def get_value(self, varname):
        """
        Returns the value of a variable with name varname.

        Arguments:
            varname----> The name of te variable to reference

        Returns:
            The value of the variable if varname is a key in internaldict.
            KeyError if varname is not a key in internaldict.
                    Notice that False is not used in case False is the value
                    in the dict.

        """

        if varname in self.internaldict.keys():
            return self.internaldict[varname]
        else:
            return KeyError

    def add_axis(self, name, steps, lowerbound=None, upperbound=None):
        """
        Appends the list in ["Axes"] with a new variable with the four
        parameters defined in the arguments.
        Dynamically increments the number of dimensions.
        If the axis is already present (as identified by the same varname),
        it is overwritten by the new values.

        Arguments:
            name-------> Name of the variable, string
            lowerbound-> Lowerbound of the variable, float
            upperbound-> Upperbound of the variable, float
            steps------> Number of evaluations, integer

        Returns nothing.
        """
        self.majormodmade = True

        for axis in self.internaldict["Axes"]:
            if axis["Varname"] == name:
                print("Axis", name, "is already in the config file.\
 Updating with new values..")
                axis["Steps"] = steps
                axis["Lowerbound"] = lowerbound
                axis["Upperbound"] = upperbound
                return

        self.internaldict['Axes'].append(

            {
            "Varname":name,
            "Lowerbound":lowerbound,
            "Upperbound":upperbound,
            "Steps":steps
            }

        )

    def del_axis(self, axisid):
        """
         Goes to the axes list, and removes the designated variable.
         Axisid can be a string or an integer. If it is an integer,
         deletes by index.

         Arguments:
            axisid--->  Way to identify the axis in question, either
                        by variable name, or by an integer
                        designating the index in the Axes list.

        Returns:
            TRUE if the axis is found, and presumably deleted.
            FALSE if the axis is not found.


        """
        self.majormodmade = True

        if type(axisid) is int:
            del self.internaldict["Axes"][axisid]
            return True

        elif type(axisid) is str:

            for dim in range(len(self.internaldict["Axes"])):
                if axisid in self.internaldict["Axes"][dim].values():
                    del self.internaldict["Axes"][dim]
                    return True
        else:
            print("Error- could not delete axis", axisid)
            return False


    def read_ndscanconfig_file(self, filename="ndscan.conf"):
        """
        Parses a ndscan config file for recognized keys in internaldict.
        Called by __init__.

        Arguments:
            filename-->Name of the file, default is "ndscan.conf". String.

        Returns:
            A json dump of the file, after copying expected variables to the
            internaldict.

        """

        try:
            configfile = open(filename)
        except FileNotFoundError:
            print('Error: Could not open configfile %s!' %filename)
            exit()

        #super().__init__(filename)
        #print('DB done')
        #exit()
        try:
            data = json.load(configfile)
        except ValueError:
            print("Error: Most likely there is a syntax error in",
                  configfile, " Cannot read file!")
            exit()
        configfile.close()
        for key in data.keys():
            if key in self.internaldict.keys():
                self.internaldict[key] = data[key]


    def write_config_file(self, filename="ndscan.conf", majormod=False):
        """
        Writes a config file in the current directory with the name specified.
        If the user decides a major modification was made, toggling majormod
        to true sets the
        "RanBefore" flag to False.

        Arguments:
            filename---->Name of the new file. string
            majormod---->If the changes were significant enough to warrant
            setting "RanBefore" to false. Boolean.
        """


        #todo change default to ndscan.conf when wrapping up project
        configfile = open(filename, "w+")
        thedict = col.OrderedDict(self.internaldict)
        for axis in thedict["Axes"]:
            try:
                if axis["Lowerbound"] is None:
                    del axis["Lowerbound"]
            # XXX specify exception type
            except:
                pass
            try:
                if axis["Upperbound"] is None:
                    del axis["Upperbound"]
            # XXX specify exception type
            except:
                pass
        for keys in thedict:
            if thedict[keys] is None:
                del thedict[keys]

        # Sets RanBefore to False, assuming that some modification has been
        # made and that this is
        # a new config file.
        if majormod is True or self.majormodmade is True:
            thedict["RanBefore"] = False
        jsonform = json.dumps(thedict, sort_keys=True,\
                       indent=4, separators=(',', ': '))
        configfile.write(jsonform)
        configfile.close()

    def modify_config_file(self, filename, varname, value, majormod=False):
        """
        A quick method included to make changing a single variable easy with
        just one call.

        Arguments:
            filename-->The name of the configuration file to load. string
            varname--->The name of the variable to change. string
            value----->The new value to assign to the variable.
            majormod-->If the change significantly affects the function of the
            scan, set this to true. Boolean.

        Dependencies:
            read_ndscanconfig_file
            set_value
            write_config_file
        """

        self.read_ndscanconfig_file(filename)
        if self.set_value(varname, value):
            print("Successfully modified ", filename, " variable ", varname,\
                      " to ", value, ".")
        self.write_config_file(filename, majormod)
