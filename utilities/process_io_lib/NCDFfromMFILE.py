"""
Author: Steven Torrisi, University of Rochester

Data which is stored in the NCDF file from an MFIlE library,
with it's title in the NCDF file listed in parentheses
(Asterisk next to data which is optional and only stored if available):

Title (title)*
Author (author)*
Description (description)*

Dimensions (dimensions)
    -Name
    -Coordinates [From here you can infer # of steps, upper bound, lower bound]

Variables (variables)
    -The dimensions which they are plotted along [same as the ones above]
    -ifail is always included as one of these by default, so there is at least one variable of interest

"""

import process_io_lib.mfile as mf
import numpy as np
from netCDF4 import Dataset
import json as json
from process_io_lib.ndscan_config import NdScanConfigFile

class NCDFconverter(object):
    """
    Contains all of the necessary methods to convert MFILEs from a scan to a NetCDF file.
    """
    def __init__(self, configfile='ndscan.conf', convertnow=False, verbosity = 1):
        """
        Specifies which configuration file to read. If convertnow is True, will begin a conversion upon instantiation of the class.
        Arguments:
            configfile---> Specifies which configuration file to read.
            convertnow---> If set to True, will begin a conversion upon instantiation of the class.
            verbosity----> If set to 1, will output some information about the results. If set to 2, will output results as well as process.

        Class variables:
            currentstep------------->Array to keep track of the current step along the coordinates
            variablecollector-------> List of lists which collects the values for each variable of interest
            variablepointers--------> List of strings; the names of each variable of interestw
            variableofinterestnames-> List of strings; the names of each variable of interestw
            scanconfig--------------> Pointer to the NdScanConfigFile class
            stepstring--------------> String which represents the current step along the coordinates.

        """


        self.currentstep = []              # Array to keep track of the current step along the coordinates
        self.variablecollector = []        # A list of lists which collects the values for each variable of interest
        self.variablepointers = []         # List which will eventually 'point to' each variable stored in the ncfile
        self.variableofinterestnames = []  # List of strings; the names of each variable of interest
        self.steptuple = ()
        self.axestuple = ()
        self.outputdirectory=""            # Stored as a variable to save time; this is referenced many times.

        # Opens up ndscan_config to help parse the ndscan config file..

        self.scanconfig = NdScanConfigFile(configfile)
        self.stepstring = ""
        self.verbosity = verbosity

        if not self.check_ndscan_file(configfile):
            import sys
            sys.exit()

        if convertnow is True:
            self.convert_mfilelibrary(configfile)
        # Determines the amount of printed output by the NCDF converter.

    def convert_mfilelibrary(self, configfile="ndscan.conf"):
        """
        The primary method which begins the conversion process on the config file named configname.
        This carries out the whole process.

        Arguments
        configfile----> Name of the config file which describes the data to convert.
        """

        # Determine roughly but dynamically what level of compression to use.
        scanvars=[]
        scansteps=[]
        for axis in self.scanconfig.get_value("Axes"):
            scanvars.append(axis["Varname"])

            if type(axis["Steps"]) is list:
                scansteps.append(len(axis["Steps"]))
            else:
                scansteps.append(axis["Steps"])

        self.steptuple = tuple(scansteps)
        self.axestuple = tuple(scanvars)

        compressionlevel = self.determine_compression()

        # Create the nc file with circumstantial data: for instance, time, title, description.
        ncfile = self.establish_new_ncfile_from_scanconfig()

        # A list of lists which will later be converted into n dimensional arrays for each variable.
        # The first list corresponds to the first variable, and so on.
        self.create_dimensions_and_var_slots(ncfile)
        # The first case is done independent of the automation.

        self.do_first_extraction()

        self.start_automated_extraction()


        ncfile.scanvars = ','.join(self.axestuple)
        ncfile.scansteps = self.steptuple

        self.store_variables_from_collector(ncfile, compressionlevel)

        self.print_ncfile_variables(ncfile)

    def check_ndscan_file(self, configfile):
        """
        Checks to make sure that there are variables of interest present in the ndscan.conf file. If none are found, prompts the user to add some.
        Arguments:

            configfile---> Specifies which configuration file to read.

        Modifies:

        ndscan.conf file     -----------> in order to add variables of interest.
        self.scanconfig.internaldict----> in order to add variables of interest

        Returns:

        If the ndscan file is found and has variables of interest:
                                                Returns True
        If the config file could not be found:
        If no variables of interest exist in the ndscan file and none are added:
                                                Returns False

        """

        try:
            interest = self.scanconfig.get_value("VariablesOfInterest")
            if self.verbosity > 1:
                print(interest)
            if len(interest) == 0 or interest is None:
                x = self.scanconfig.configdata["This_Will_Throw_An_Exception"] #This will throw an exception on purpose
        except:
            print("Uh oh! No variables of interest detected. This may be a mistake. Check the ndscan file. \
                  \n Would you like to add some now?")
            userinput = input("Y/N")
            if userinput in("Y", "y"):

                try:
                    while userinput != "":
                        userinput = input("Please enter a variable of interest (or leave blank to exit):")

                        if userinput == "": break

                        self.scanconfig.add_var_of_interest(userinput)
                    print(self.scanconfig.internaldict)
                    self.scanconfig.write_config_file(configfile, False)
                    print("Wrote the variables of interest to the configuration file.")
                    return True

                except:
                    print("Couldn't find ndscan conf file or something went wrong. Exiting. Please add VariablesOfInterest to the config file")
                    import sys
                    sys.exit()

            else:
                return False
        return True

    def increment_current_step(self, dimtuple):
        """
        Increments the current step; compares it against dimtuple, and when an individual axis as at the end of it's
        bound, rounds up the next axis.


        When an 'overflow' occurs along an axis, rounds up.
        For example- If 0th dimension has n steps, when the current step's 0th dimension reaches the (n+1)th step,
        increases the 1st dimension step by one, and take the 0th dimension back down to the 1st step.
        Then if the 1st dimension overflowed, take that back down to the 1st step and tick up the 2nd dimension-- etc...

        Arguments:
        dimtuple-----> The tuple which describes how many steps each dimension has

        Modifies:

        self.currentstep

        WILL END PROGRAM IF:

        The currentstep counter overflows against dimtuple. This shouldn't happen.

        """

        self.currentstep[0] += 1
        if self.verbosity > 1:
            print("Current step is now ", self.currentstep)

        #Checks to see if increasing the current step by 1 resulted in a dimension overflow
        # if so- reset it to 0, and increment the next dimension by 1.
        for x in range(len(self.currentstep)):
            # Check to see if we are evaluating the last step. If so, an overflow must be treated differently.
            if self.currentstep[-1] == dimtuple[-1]:
                print("A serious error has occured, and the total counts have overflowed.")
                print("Currentstep[-1] is", self.currentstep[-1], "and dimtuple[-1] is", dimtuple[-1])
                import sys
                sys.exit()
            if self.currentstep[x] == dimtuple[x]:
                self.currentstep[x] = 0
                self.currentstep[x+1] += 1

    def get_next_mfile(self, currentstep):
        """
        Returns the next MFile in the sequence of mfiles specified by the config file.

        Accepts as input the tuple which describes the number of steps for each variable.
        Iterates the current step up by one, checks to make sure that the current step
        actually describes a coordinate.

        Arguments:

        currentstep----> Represents the current step the Mfile walker is on

        Returns:
        mfile----------> MFile class of the new MFILE to trawl for data

        """

        # Starts constructing the stepstring which will point to where the the MFILE is.

        stepstring = 'M.'
        for x in range(len(currentstep)):
            stepstring += str(currentstep[x])+'.'
        stepstring +='DAT'

        #Recap where the Mwalker is now pointing to:
        if self.verbosity > 1:
            print("File walker is now reading", stepstring)

        mfile=mf.MFile(self.outputdirectory+ '/' +stepstring)

        #And now return the mfile which is pointed to by stepstring.
        #We have now 'walked' from one Mfile to the next while incrementing the currentstep.

        return mfile

    def get_mfile_variables(self, mfile, varstofind):
        """
        Searches through the mfile for the name strings in varstofind, and returns their corresponding values.

        Arguments:

        mfile--------->MFile class describing the MFILE to be trawled
        varstofind----> List of strings; variable names to be found.

        Returns:
        varoutput----->List of values corresponding with the variables in varstofind.
        """

        #I use the "BLANK" string because if at the end of this fishing routine,
        # there are any "BLANKS" leftover, we know that a var to find wasn't found...

        varoutput=["BLANK!"]*len(varstofind)

        for hook in varstofind:
            varoutput[varstofind.index(hook)] = mfile.data.__getitem__(hook).get_scan(-1)

        if "BLANK!" in varoutput and self.verbosity >0:
            print ("Warning! variable", varstofind[varoutput.index("BLANK!")], "was not found in the mfile", mfile, "!")
            #import sys
            #sys.exit()

        return varoutput

    def determine_compression(self):
        """
        From the current ndscan conf file, determine a level of compression based on the order of magnitude of the number of data points.

        A simple program which determines if the number of data pieces will be greater than 10^4, 10^6, or 10^7.

        Returns:
        compressionlevel-----> An integer which will be valued either 4, 6, 8, or 9.

        Dependencies:

        self.steptuple
        self.variableofinterestnames


        """

        # Default level established first
        compressionlevel = 4

        totalsteps = 1
        dataarray = tuple(self.steptuple)

        for n in dataarray:
            totalsteps *= n

        totalpoints = totalsteps * len(self.variableofinterestnames)

        # In a rough way, dynamically set the compression based on how much data points are going to be
        # in the file..

        if totalpoints >= 100000:
            compressionlevel = 6
            if totalpoints >= 1000000:
                compressionlevel = 8
                if totalpoints >= 10000000:
                    compressionlevel = 9
        return compressionlevel

    def establish_new_ncfile_from_scanconfig(self):
        """
        Constructs a NetCDF file from the descriptive information contained in an ndscan.conf file.

        The data pieces it searches for are title, output directory, description, and author.

        Dependencies:

        datetime module
        self.scanconfig

        Modifies:

        self.outputdirectory

        Returns:

        ncfile--------> a pointer to a NetCDF file class.

        """
        import datetime as dt
        time = str(dt.datetime.now())
        time = time.replace(" ", "-")
        time = time[0:16]

        # Checks to see if a title is specified in the ndscanconfig file. If not, the date and time is used.
        try:
            title = self.scanconfig.get_value("Title")
            ncfile = Dataset(title + "data.nc", 'w')
            ncfile.title = title
        except:

            ncfile = Dataset(time + "datfile.nc", 'w')
            ncfile.title = "NCDFfile"

        # Establish 'biographical data' about the ncfile to ensure portability.
        # If no such biographical data is written, use default values

        # Time is recorded for archival purposes
        ncfile.time = time

        self.outputdirectory = self.scanconfig.get_value("OutputDirectory")

        if self.scanconfig.get_value("OutputDirectory") is None:
            self.outputdirectory = "DATA"


        try: ncfile.description = self.scanconfig.get_value("Description")
        except: ncfile.description = "No description given."

        try: ncfile.author = self.scanconfig.get_value("Author")
        except: ncfile.author = "No Author Given."

        ncfile.ndim = len(self.scanconfig.get_value("Axes"))

        if self.scanconfig.get_value("RanBefore") is False:
            response = input("Warning! This configuration appears to have not been run yet. Are you sure you want to continue? y/n")
            if response == 'y' or response == 'Y':
                print("Continuing!")
            else:
                print("Exiting.")
                import sys
                sys.exit()

        return ncfile

    def create_dimensions_and_var_slots(self, ncfile):
        """
        Creates a dimension for each Axis in the config file, and a spot in memory for each variable of interest.

        The variable of interest collector lists generated will act as 'trays' to store the variables of interest
        extracted as the MFILEs have their data extracted. They will each be coupled with the dimensions specified
        in the scan.

        Arguments:

        ncfile-----> A netCDF file class which will have dimensions and variables added to it.

        Dependencies:

        self.scanconfig
        self.verbosity
        self.currentstep

        Modifies:

        self.stepstring
        self.currentstep
        self.variablecollector
        self.variablepointers
        self.variableofinterestnames

        """
        self.stepstring = "M."
        axes = self.scanconfig.get_value("Axes")
        for n in range(len(axes)):

            if type(axes[n]["Steps"]) is list:
                axissize=len(axes[n]["Steps"])
            else:
                axissize=int(axes[n]["Steps"])

            if self.verbosity > 1:
                print("Creating dimension:", axes[n]["Varname"], "with size", axissize)

            ncfile.createDimension(dimname = axes[n]["Varname"], size = axissize)

            self.currentstep.append(0)
            self.stepstring += "0."

        for var in self.scanconfig.get_value("VariablesOfInterest"):
            if self.verbosity > 1:
                print("Creating variable", var)
            self.variablecollector.append([])
            self.variablepointers.append(0)
            self.variableofinterestnames.append(var)

        if "ifail" not in self.variableofinterestnames:
            if self.verbosity > 1:
                print("Adding ifail in by default...")

            # Remove a slot from collector, pointers, and the offending ifail.
            self.variablecollector.append([])
            self.variablepointers.append(0)
            self.variableofinterestnames.append("ifail")

        self.stepstring += "DAT"

    def do_first_extraction(self):
        """
        The first MFILE extraction features more error checks than the others for efficiency, and is handled as a seperate function
        before the automated extraction begins.

        Dependencies:

        self.outputdirectory
        self.get_mfile_variables
        self.variablecollector
        self.stepstring

        Modifies:

        self.variablecollector

        """
        mstring = self.outputdirectory + '/' + self.stepstring

        try:
            test = open(mstring,"r")
            test.close()
            currentmfile = mf.MFile(mstring)
        except:
            print("!!!!!")
            print("Warning! A serious failure has ocurred, and the first Mfile could not be found.")
            print("This does not bode well for the rest of the scan.")
            print("Attempted to check", mstring, "for the file but something went wrong.")
            print("Make sure that the Mfiles are in the output directory and follow the naming convention M.x.y.z..\
            ..q.DAT.")
            print("Make sure that they are also where the ndscan config file says is the output directory, which is", \
                  self.outputdirectory)
            print("!!!!!")

            import sys
            sys.exit()

        #Scan the MFILE for the variables of interest
        recentoutput = self.get_mfile_variables(currentmfile, self.variableofinterestnames)
        #Store output in the collector trays for each variable of interest
        for collectnumber in range(len(recentoutput)):
            self.variablecollector[collectnumber].append(recentoutput[collectnumber])

    def start_automated_extraction(self):
        """
        Initiates the conversion tool's automated trawl across the MFILEs for the variables of interest.
        Stores the results in variable collector's respective list for each variable.

        Calls:  increment_current_step(steptuple)
                get_next_mfile
                get_mfile_variables

        Dependencies:

        self.verbosity
        self.steptuple

        Modifies:

        self.variablecollector


        """
        if self.verbosity > 0:
            print ("Beginning extraction...")
        if self.verbosity>1:
            print("Steptuple:", self.steptuple)
        totalsteps = 1
        for n in self.steptuple: totalsteps *= n

        for step in range(totalsteps-1):

            self.increment_current_step(self.steptuple)
            currentmfile = self.get_next_mfile(self.currentstep)
            recentoutput = self.get_mfile_variables(currentmfile, self.variableofinterestnames)

            for collectnumber in range(len(recentoutput)):
                self.variablecollector[collectnumber].append(recentoutput[collectnumber])

    def store_variables_from_collector(self, ncfile, compressionlevel = 4):
        """
        Once the data has been extracted from the MFILEs, stores it in the NetCDF file.

        Arguments:

        ncfile------------> The NetCDF file to write to
        compressionlevel--> The level of compression to be used on the variables

        Dependencies:
        self.steptuple
        self.variablecollector
        self.variablepointers
        self.scanconfig
        self.axestuple
        self.variableofinterestnames


        Modifies:
        self.variablepointers




        """
        steptuple = self.steptuple
        elputpets = steptuple[::-1] # Reverses the steptuple because the format NCfiles store in is different from Python.


        for n in range(len(self.variablecollector)):
            variablearray = np.array(self.variablecollector[n],order='C')
            # IMPORTANT- I use column major ordering because I think it makes more sense
            # in the context of the process ndscan suite.
            if len(elputpets)>1:
                variablearray.shape = elputpets
            self.variablepointers[n] = ncfile.createVariable(self.variableofinterestnames[n], "f4",\
                                            tuple(self.axestuple), zlib=True, complevel=compressionlevel,\
                                            least_significant_digit=4)

            #print("So variable pointers[n] is just defined:", self.variablepointers[n])

            self.variablepointers[n][:] = np.transpose(variablearray[:])

            #Debug script
            #print("Just wrote:", variablearray)
            #print('Came out as:', variablearray[:])
            #print("Variablepointers[n] is now", self.variablepointers[n])

        scanvarpointers=[]
        axes = self.scanconfig.get_value("Axes")

        for axis in axes:
            scanvarname = "SCAN" + axis["Varname"]
            scanvarpointers.append(0)
            scanvarpointers[axes.index(axis)] = ncfile.createVariable(scanvarname, "f4", axis["Varname"])
            if type(axis["Steps"]) is list:
                scanvarpointers[axes.index(axis)][:] = np.array(axis["Steps"])
            else:
                coords=[axis["Lowerbound"] + j * (axis["Upperbound"] - axis["Lowerbound"]) \
                                                  / (axis["Steps"]-1) for j in range(int(axis["Steps"])) ]
                scanvarpointers[axes.index(axis)][:] = np.array(coords)

    def print_ncfile_variables(self, ncfile):
        """
        Prints the variables which are currently stored in ncfile. By default, runs at the conclusion of each conversion
        in order to demonstrate to the user that the variables were successfuly extracted (or not).

        Dependencies:
        self.verbosity

        Arguments:
        ncfile-------> Points to the NetCDF file class to have variables printed.
        """
        if self.verbosity > 0:
            print("Results:")
            for var in ncfile.variables:
                print("-----------------------")
                print(var)
                x = ncfile.variables[var][:]
                print(x)

            print("=================================")