"""
Author: Hanni Lux (Hanni.Lux@ccfe.ac.uk)

Interfaces for Configuration values for programs
- run_process.py
- test_process.py
- ndscan.py
- evaluate_uncertainties.py

Compatible with PROCESS version 382
"""

import os
import subprocess
from time import sleep
from numpy.random import seed, uniform, normal
from numpy import argsort, ndarray
import collections as col
from process_io_lib.process_funcs import  get_neqns_itervars,\
    get_variable_range, vary_iteration_variables, check_input_error,\
    process_stopped, get_from_indat_or_default,\
    set_variable_in_indat, check_in_dat
from process_io_lib.ndscan_config import NdScanConfigFile
from process_io_lib.ndscan_funcs import get_var_name_or_number,\
    get_iter_variables_from_mfile, get_iter_vars, backup_in_file
from process_io_lib.in_dat import InDat
from process_io_lib.mfile import MFile
from process_io_lib.configuration import Config
try:
    from process_io_lib.process_dicts import DICT_NSWEEP2IXC, \
        DICT_NSWEEP2VARNAME, DICT_IXC_SIMPLE
except ImportError:
    print("The Python dictionaries have not yet been created. Please run \
'make dicts'!")
    exit()
from process_io_lib.process_netcdf import NetCDFWriter

#def print_config(config_instance):
#    print(config_instance.get_current_state())


class ProcessConfig(object):

    """
    Configuration parameters for PROCESS runs

    filename - Configuration file name

    wdir     - Working directory

    or_in_dat  - Original IN.DAT file

    process  - PROCESS binary

    niter    - (Maximum) number of iterations

    u_seed   - User specified seed value for the random number generator

    factor   - Multiplication factor adjusting the range in which the original
               iteration variables should get varied

    comment  - additional comment to be written into README.txt

    """

    filename = None
    configfileexists = True
    wdir     = '.'
    or_in_dat = 'IN.DAT'
    process  = 'process.exe'
    niter    = 10
    u_seed   = None
    factor   = 1.5
    comment  = ''


    def echo(self):

        """ echos the attributes of the class """

        if self.wdir != '.':
            print("Working directory:   {}".format(self.wdir))
        print("Original IN.DAT:     {}".format(self.or_in_dat))
        print("PROCESS binary:      {}".format(self.process))
        print("Number of iterations {}".format(self.niter))

        if self.u_seed != None:
            print("random seed          {}".format(self.u_seed))
        print("variable range factor {}".format(self.factor))
        if self.filename != None:
            print("Config file          {}".format(self.filename))
        if self.comment != '':
            print("Comment  {}".format(self.comment))


    def prepare_wdir(self):

        """ prepares the working directory """

        try:
            os.stat(self.wdir)
        except FileNotFoundError:
            os.mkdir(self.wdir)

        if self.or_in_dat != 'IN.DAT' or self.wdir != '.':
            subprocess.call(['cp', self.or_in_dat, self.wdir + '/IN.DAT'])
        else:
            subprocess.call(['cp', self.or_in_dat, 'Input_IN.DAT'])

        if self.configfileexists:
            subprocess.call(['cp', self.filename, self.wdir])
        os.chdir(self.wdir)
        subprocess.call(['rm -f OUT.DAT MFILE.DAT PLOT.DAT *.txt *.out\
 *.log *.pdf *.eps *.nc *.info'], shell=True)


    def create_readme(self, directory='.'):

        """ creates README.txt containing comment """

        if self.comment != '':

            readme = open(directory+'/README.txt', 'w')
            readme.write(self.comment)
            readme.close()


    def error_status2readme(self, directory='.'):

        """ appends PROCESS outcome to README.txt """

        if os.path.isfile("MFILE.DAT"):

            if self.comment != '':
                readme = open(directory+'/README.txt', 'a')
            else:
                readme = open(directory+'/README.txt', 'w')

            m_file = MFile(filename=directory+"/MFILE.DAT")

            error_status =\
                "Error status: {}  Error ID: {}\n".format(
                m_file.data['error status'].get_scan(-1),
                m_file.data['error id'].get_scan(-1))

            readme.write(error_status)
            readme.close()



    def modify_in_dat(self):

        """ modifies the original IN.DAT file """

        pass


    def setup(self):

        """ sets up the program for running  """

        self.echo()

        self.prepare_wdir()

        self.create_readme()

        self.modify_in_dat()

        check_in_dat()

        seed(self.u_seed)



    def run_process(self):

        """ runs PROCESS binary """

        logfile = open('process.log', 'w')
        print("PROCESS run started ...", end='')

        try:
            subprocess.check_call([self.process], stdout=logfile,
                                  stderr=logfile)
        except subprocess.CalledProcessError as err:
            print('\n Error: There was a problem with the PROCESS \
                   execution!', err)
            print('       Refer to the logfile for more information!')
            exit()

        logfile.close()
        print("finished.")


    def get_comment(self):

        """ gets the comment line from the configuration file """

        if not self.configfileexists:
            return False

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            self.configfileexists = False
            return False

        for line in configfile:

            condense = line.replace(' ', '')
            condense = condense.rstrip()
            lcase = condense.lower()
            if len(condense) > 0 and (condense[0] != "*"):
                if 'comment' == lcase[:7]:
                    self.comment = line[line.find("=")+1:]
                    configfile.close()
                    return True

        configfile.close()
        return False

    def get_attribute(self, attributename):

        """ gets class attribute from configuration file """


        if not self.configfileexists:
            return None

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            self.configfileexists = False
            return None

        for line in configfile:
            condense = line.replace(' ', '')
            if (condense[0] != "*") and len(condense) > 1:
                if '=' in line:
                    varname = line[:line.find("=")]
                    varname = varname.replace(' ', '')
                    varname = varname.upper()
                    auxvar = line[line.find("=")+1:]
                    auxvar = auxvar.replace(' ', '')
                    auxvar = auxvar.rstrip()
                    if varname == attributename.upper():
                        configfile.close()
                        if auxvar == '':
                            return None
                        else:
                            return auxvar

        configfile.close()
        return None

    def set_attributes(self):

        """ sets the attributes of the class """

        buf = self.get_attribute('wdir')
        if buf != None:
            self.wdir = buf

        buf = self.get_attribute('ORIGINAL_IN_DAT')
        if buf != None:
            self.or_in_dat = buf

        try:
            indatfile = open(self.or_in_dat)
            indatfile.close()
        except FileNotFoundError:
            print('Error: %s does not exist! Create file or modify config file!'
                  %self.or_in_dat)
            exit()

        buf = self.get_attribute('process')
        if buf != None:
            self.process = buf

        buf = self.get_attribute('niter')
        if buf != None:
            self.niter = int(buf)

        buf = self.get_attribute('seed')
        if buf != None:
            if buf == 'None':
                self.u_seed = None
            else:
                self.u_seed = int(buf)

        buf = self.get_attribute('factor')
        if buf != None:
            self.factor = float(buf)

        if not self.get_comment():
            print('No comment in config file %s' %self.filename)




################################################################################
#class TestProcessConfig(ProcessConfig)
################################################################################


class TestProcessConfig(ProcessConfig):

    """
    Configuration parameter of the test_process.py program

    ioptimz - sets ioptimz (optimisation solver) in IN.DAT

    epsvmc  - sets epsvmc (VMCON error tolerance) in IN.DAT

    epsfcn  - sets epsfcn (finite diff. steplength) in IN.DAT

    minmax  - sets minmax (figure of merit switch) in IN.DAT

    """

    ioptimz = 'None'
    epsvmc = 'None'
    epsfcn = 'None'
    minmax = 'None'


    def __init__(self, filename='test_process.conf'):

        """ create configuration instance """

        self.filename = filename

        super().set_attributes()

        buf = self.get_attribute('ioptimz')
        if buf != None:
            self.ioptimz = buf

        buf = self.get_attribute('epsvmc')
        if buf != None:
            self.epsvmc = buf

        buf = self.get_attribute('epsfcn')
        if buf != None:
            self.epsfcn = buf

        buf = self.get_attribute('minmax')
        if buf != None:
            self.minmax = buf


    def echo(self):

        """ echos the values of the current class """

        print('')
        super().echo()

        if self.ioptimz != 'None':
            print('ioptimz              %s' % self.ioptimz)
        if self.epsvmc != 'None':
            print('epsvmc               %s' % self.epsvmc)
        if self.epsfcn != 'None':
            print('epsfcn               %s' % self.epsfcn)
        if self.minmax != 'None':
            print('minmax               %s' % self.minmax)
        print('')
        sleep(1)


    def modify_in_dat(self):

        """ modifies IN.DAT using the configuration parameters """

        in_dat = InDat()

        #by convention all variablenames are lower case
        if self.ioptimz != 'None':
            in_dat.add_parameter('ioptimz', self.ioptimz)
        if self.epsvmc != 'None':
            in_dat.add_parameter('epsvmc', self.epsvmc)

        if self.epsfcn != 'None':
            in_dat.add_parameter('epsfcn', self.epsfcn)

        if self.minmax != 'None':
            in_dat.add_parameter('minmax', self.minmax)

        in_dat.write_in_dat(output_filename='IN.DAT')


################################################################################
#class RunProcessConfig(ProcessConfig)
################################################################################


class RunProcessConfig(ProcessConfig):

    """
    Configuration parameters of the run_process.py program

    no_allowed_unfeasible - the number of allowed unfeasible points in a sweep

    create_itervar_diff - boolean to indicate the creation of a summary file
                          of the iteration variable values at each stage

    add_ixc - List of iteration variables to be added to IN.DAT

    del_ixc - List of iteration variables to be deleted from IN.DAT

    add_icc - List of constrained equations to be added to IN.DAT

    del_icc - List of constrained equations to be deleted from IN.DAT

    dictvar - Dictionary mapping variable name to new value (replaces old
              or gets appended)

    del_var - List of variables to be deleted from IN.DAT

    """

    no_allowed_unfeasible = 0
    create_itervar_diff = False
    add_ixc = []
    del_ixc = []
    add_icc = []
    del_icc = []
    dictvar = {}
    del_var = []

    def __init__(self, filename='run_process.conf'):

        """
        creates an instance of the RunProcessConfig class
        that stores all configuration parameters of the
        run_process.py
        """

        self.filename = filename

        super().set_attributes()

        buf = self.get_attribute('no_allowed_unfeasible')
        if buf != None:
            self.no_allowed_unfeasible = int(buf)

        buf = self.get_attribute('create_itervar_diff')
        if buf != None:
            if buf.lower() in ['true', 'y', 'yes']:
                self.create_itervar_diff = True
            elif buf.lower() in ['false', 'n', 'no']:
                self.create_itervar_diff = False
            else:
                print('WARNING: Value for create_itervar_diff\
 is not defined!')

        self.add_ixc = self.get_attribute_csv_list('add_ixc')

        self.del_ixc = self.get_attribute_csv_list('del_ixc')

        self.add_icc = self.get_attribute_csv_list('add_icc')

        self.del_icc = self.get_attribute_csv_list('del_icc')

        self.set_del_var()

        self.set_dictvar()


    def get_attribute_csv_list(self, attributename):

        """
        get class attribute list from configuration file
        expects comma separated values
        """

        if not self.configfileexists:
            return []

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            self.configfileexists = False
            return []


        attribute_list = []

        for line in configfile:

            condense = line.replace(' ', '')
            condense = condense.rstrip()
            lcase = condense.lower()
            if len(condense) > 0 and (condense[0] != "*"):
                if attributename == lcase[:len(attributename)]:
                    buf = condense[condense.find('=')+1:].split(',')
                    if buf[-1] == '': # if last value has ended on comma
                        buf = buf[:-1]
                    attribute_list += buf

        configfile.close()

        return attribute_list

    def set_del_var(self):

        """ sets the del_var attribute from the config file """

        if not self.configfileexists:
            return

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            self.configfileexists = False
            return

        for line in configfile:

            condense = line.replace(' ', '')
            condense = condense.rstrip()
            lcase = condense.lower()
            if len(condense) > 0 and (condense[0] != "*"):
                if 'del_var_' == lcase[:8] and len(condense) > 8:
                    self.del_var += [condense[8:]]

        configfile.close()


    def set_dictvar(self):

        """ sets the dictvar attribute from config file """


        if not self.configfileexists:
            return

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            self.configfileexists = False
            return

        for line in configfile:

            condense = line.replace(' ', '')
            condense = condense.rstrip()
            lcase = condense.lower()
            if len(condense) > 0 and (condense[0] != "*"):
                if '=' in lcase:
                    varname = lcase[:lcase.find("=")]
                    auxvar = condense[condense.find("=")+1:]
                    if varname[:4] == 'var_' and not auxvar == '':
                        self.dictvar[varname[4:]] = auxvar

        configfile.close()



    def echo(self):

        """ echos the values of the current class """

        print('')
        super().echo()

        print('no. allowed UNFEASIBLE points %i' % self.no_allowed_unfeasible)
        if self.create_itervar_diff:
            print('Set to create a summary file of the iteration variable\
 values!')

        if self.add_ixc != []:
            print('add_ixc', self.add_ixc)
        if self.del_ixc != []:
            print('del_ixc', self.del_ixc)
        if self.add_icc != []:
            print('add_icc', self.add_icc)
        if self.del_icc != []:
            print('del_icc', self.del_icc)
        for key, value in self.dictvar.items():
            print('set %s  to %s' %(key, value))
        if self.del_var != []:
            print('del_var', self.del_var)

        print('')
        sleep(1)


    def modify_in_dat(self):

        """ modifies IN.DAT using the configuration parameters"""

        self.modify_vars()
        self.modify_ixc()
        self.modify_icc()


    def modify_vars(self):

        """ modifies IN.DAT by adding, deleting and modifiying variables """

        in_dat = InDat()

        #add and modify variables
        for key in self.dictvar.keys():
            set_variable_in_indat(in_dat, key, self.dictvar[key])


        #delete variables
        for key in self.del_var:
            key = key.lower()
            if 'bound' in key:
                number = (key.split('('))[1].split(')')[0]
                if 'boundu' in key:
                    in_dat.remove_bound(number, 'u')
                else:
                    in_dat.remove_bound(number, 'l')
            else:
                in_dat.remove_parameter(key)

        in_dat.write_in_dat(output_filename='IN.DAT')


    def modify_ixc(self):

        """ modifies the array of iteration variables in IN.DAT """


        #check that there is no variable in both lists
        if set(self.add_ixc).intersection(self.del_ixc) != set([]):
            print('Error: You are trying to add and delete \
                   the same variable from ixc!')
            exit()

        in_dat = InDat()

        for iter_var in self.add_ixc:
            in_dat.add_iteration_variable(int(iter_var))

        for iter_var in self.del_ixc:
            in_dat.remove_iteration_variable(int(iter_var))

        in_dat.write_in_dat(output_filename='IN.DAT')


    def modify_icc(self):

        """ modifies the array of constraint equations in IN.DAT"""

        #check that there is no variable in both lists
        if set(self.add_icc).intersection(self.del_icc) != set([]):
            print('Error: You are trying to add and delete the same \
                  variable from icc!')
            exit()

        in_dat = InDat()

        for constr in self.add_icc:
            in_dat.add_constraint_equation(int(constr))

        for constr in self.del_icc:
            in_dat.remove_constraint_equation(int(constr))

        in_dat.write_in_dat(output_filename='IN.DAT')


################################################################################
#class UncertaintiesConfig(RunProcessConfig)
################################################################################

NETCDF_SWITCH = True

class UncertaintiesConfig(ProcessConfig, Config):

    """
    Configuration parameters for evaluate_uncertainties.py program
    """

    no_allowed_unfeasible = 2
    no_scans = 5
    no_samples = 1000
    uncertainties = []
    output_vars = []
    dict_results = {}
    if NETCDF_SWITCH:
        ncdf_writer = None


    def __init__(self, configfilename="evaluate_uncertainties.json"):

        """
        creates and instance of the UncertaintiesConfig class
        """

        #TODO: Once ProcessConfig has been ported to only use json
        #use the ProcessConfig __init__ routine to set up these
        #parameters
        super().__init__(configfilename)
        self.filename = configfilename

        self.wdir = os.path.abspath(self.get("config", "working_directory",
                                             default=self.wdir))
        self.or_in_dat = os.path.abspath(self.get("config", "IN.DAT_path",
                                                  default=self.or_in_dat))
        self.process = self.get("config", "process_bin", default=self.process)
        self.niter = self.get("config", "no_iter", default=self.niter)
        self.u_seed = self.get("config", "pseudorandom_seed",
                               default=self.u_seed)
        self.factor = self.get("config", "factor", default=self.factor)
        self.comment = self.get("config", "runtitle", default=self.comment)

        #additional new parameters
        self.no_scans = self.get("no_scans", default=self.no_scans)
        self.no_samples = self.get("no_samples", default=self.no_samples)
        self.uncertainties = self.get("uncertainties",
                                      default=self.uncertainties)
        self.output_vars = self.get("output_vars", default=self.output_vars)
        for varname in self.output_vars:
            self.dict_results[varname] = []


    def echo(self):

        """ echos the values of the current class """

        print('')
        super().echo()

        print('No scans            %i' % self.no_scans)
        print('No samples          %i' % self.no_samples)
        if self.uncertainties != []:
            print('uncertainties:')
            for item in self.uncertainties:
                print('     ', item['varname'])
                for key in item.keys():
                    if key not in ['varname']:
                        print('     ', key, item[key])
                print(' -------')
        if self.output_vars != []:
            print('output vars        ', self.output_vars)
        print('')
        sleep(1)


    def modify_in_dat(self):

        """ modifies IN.DAT before running uncertainty evaluation """


        in_dat = InDat()

        #Is IN.DAT already having a scan?
        isweep = get_from_indat_or_default(in_dat, 'isweep')
        if isweep > 0:
            #check that sweep variable is not an iteration variable
            # and all values are the same value
            nsweep = get_from_indat_or_default(in_dat, 'nsweep')
            ixc = get_from_indat_or_default(in_dat, 'ixc')
            sweep = get_from_indat_or_default(in_dat, 'sweep')
            if ((str(nsweep) in DICT_NSWEEP2IXC.keys()) and
                (DICT_NSWEEP2IXC[str(nsweep)] in ixc) and
                (not all(sweep[0] == item for item in sweep))):
                if self.no_scans != isweep:
                    #Change no of sweep points to correct value!!
                    set_variable_in_indat(in_dat, 'isweep', self.no_scans)
                    value = sweep[0]
                    set_variable_in_indat(in_dat, 'sweep',
                                          [value]*self.no_scans)
                #Else: we can actually use this scan

            else:

                print('Error: Inbuild sweep is not compatible with uncertainty\
 evaluation! Edit IN.DAT file!')
                exit()
        else:
            # create a scan!
            nsweep = '3'
            if nsweep in DICT_NSWEEP2IXC.keys():
                #TODO: if this ever happens, program this testing whether
                #a certain variable is used as iteration variable, if not
                #choose another
                print("Error: The developer should program this more wisely\
 using a sweep variable that is not an iteration variable!")
                exit()
            else:
                set_variable_in_indat(in_dat, 'nsweep', nsweep)
                set_variable_in_indat(in_dat, 'isweep', self.no_scans)
                value = get_from_indat_or_default(in_dat,
                                                  DICT_NSWEEP2VARNAME[nsweep])
                set_variable_in_indat(in_dat, 'sweep', [value]*self.no_scans)


        # write comment in runtitle!
        runtitle = self.comment.replace(',', ' ')
        runtitle = runtitle.replace('\n', ' ')
        set_variable_in_indat(in_dat, 'runtitle', runtitle)

        #set epsvmc to appropriate value!
        # recommendation from solver work!
        set_variable_in_indat(in_dat, 'epsvmc', 1e-8)

        in_dat.write_in_dat(output_filename='IN.DAT')

    def checks_before_run(self):

        """ run several checks before you start running """

        if self.uncertainties == {}:
            print('Error: No uncertain parameter specified in config file!')
            exit()


        if self.output_vars == []:
            print('Error: No output variables specified in config file!')
            exit()

        in_dat = InDat()
        ixc_list = in_dat.data['ixc'].get_value
        assert type(ixc_list) == list

        ixc_varname_list = [DICT_IXC_SIMPLE[str(x)] for x in ixc_list]

        for u_dict in self.uncertainties:
            varname = u_dict['varname'].lower()
            if varname in ixc_varname_list:
                print('Error: an uncertain variable should never be an\
 iteration variable at the same time!', varname)
                exit()
        # check uncertainties are within bounds??



    def set_sample_values(self):

        """ determines the values of each sample point and orders them """

        for u_dict in self.uncertainties:
            if u_dict['errortype'].lower() == 'gaussian':
                mean = u_dict['mean']
                std = u_dict['std']
                values = normal(mean, std, self.no_samples)
            elif u_dict['errortype'].lower() == 'uniform':
                lbound = u_dict['lowerbound']
                ubound = u_dict['upperbound']
                values = uniform(lbound, ubound, self.no_samples)
            elif u_dict['errortype'].lower() == 'relative':
                err = u_dict['percentage']/100.
                lbound = u_dict['mean']*(1.-err)
                ubound = u_dict['mean']*(1.+err)
                values = uniform(lbound, ubound, self.no_samples)
            u_dict['samples'] = values

        #order by one parameter
        #TODO: Find a more cunning way to determine which is a good
        #variable to sort by! e.g. largest range?
        # always try to choose a uniform case?
        arr = self.uncertainties[0]['samples']
        sorted_index = argsort(arr)
        for u_dict in self.uncertainties:
            u_dict['samples'] = u_dict['samples'][sorted_index]


    def go2newsamplepoint(self, sample_index):

        """ create a new sample point from uncertainty distributions
        """

        in_dat = InDat()

        for u_dict in self.uncertainties:
            value = u_dict['samples'][sample_index]
            varname = u_dict['varname']
            set_variable_in_indat(in_dat, varname, value)


        in_dat.write_in_dat(output_filename='IN.DAT')


    def add_results2netcdf(self, run_id):

        """ reads current MFILE and adds specified output variables
            of last scan point to summary netCDF file """


        if NETCDF_SWITCH:
            m_file = MFile(filename="MFILE.DAT")

            with NetCDFWriter(self.wdir+"/uncertainties.nc", append=True,
                              overwrite=False) as ncdf_writer:
                try:
                    ncdf_writer.write_mfile_data(m_file, run_id,
                                                 save_vars=self.output_vars,
                                                 latest_scan_only=True,
                                                 ignore_unknowns=False)
                except KeyError as err:
                    print('Error: You have specified an output variable that\
 does not exist in MFILE. If this is a valid PROCESS variable, request it being\
 added to the MFILE output, else check your spelling!')
                    print(err)
                    exit()

        else:
            m_file = MFile(filename="MFILE.DAT")

            if m_file.data['ifail'].get_scan(-1) == 1:
                for varname in self.output_vars:
                    value = m_file.data[varname].get_scan(-1) #get last scan
                    self.dict_results[varname] += [value]
            else:
                self.write_results()
                print('WARNING to developer: scan has unfeasible point at the\
     end!\nPress Enter to continue!')
                raw_input()


    def write_results(self):
        """ writes data into file. Uncessary, if netcdf library works?"""

        if not NETCDF_SWITCH:
            results = open('uncertainties.nc', 'w')
            for varname in self.output_vars:
                results.write(varname + '\t')
            results.write('\n')

            for i in range(len(self.dict_results[self.output_vars[0]])):
                for varname in self.output_vars:
                    results.write(str(self.dict_results[varname][i]) + '\t')
                results.write('\n')

            results.close()



################################################################################
#class NdScanConfig(RunProcessConfig)
################################################################################


class NdScanConfig(RunProcessConfig):

    """
    # Author: Steven Torrisi (storrisi@u.rochester.edu)
    # University of Rochester, IPP Greifswald
    # July 2014

    #``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='``#
    """
    scanaxes = {'ndim' : 0,
                'varnames' : [],
                # The ixc numbers of the variables to be scanned.
                'varnumbers': [],
                'lbs' : [],
                'ubs' : [],
                'steps': [],
                # A list of lists enumerating the values at which the scan
                # variables will be evaluated.
                'coords' : []}
    # A n-dimensional 'coordinate vector' which uses
    #   step's numbers from 0 to the [# of steps-1] of coordinates
    #   to keep track of where the scan is.
    currentstep = []
    # no of unsuccessful runs
    totalfails = 0
    # To be used in a check before commencing the scan.
    configfileloaded = False
    configfile = None # Will point to a config file opened with JSON.
    mfiledir = "MFILES"
    failcoords = ndarray((1), int)
    errorlist = col.OrderedDict({})

    iterationvariables = col.OrderedDict({})
    #A dictionary of lists; each sublist has 2 elements,
    # one for number at lower and one for number at upper
    iterationvariablesatbounds = col.OrderedDict({})
    optionals = {
        'StoreOutput'                : True,
        #^ Saves the output data from the run. Toggle to false to save time with
        #  writing data if you want to debug some other functionality.
        'RemoveScanVarsFromIteration': True,
        #^ Removes all scanning variables from the iteration variables
        #  of the IN.DAT file.
        'RemoveSomeVarsFromIteration': 0,
        #Only removes the first n scanning variables from the iteration variables
        #of the IN.DAT file. For example, if n=2, then dimensions 1 and 2 will
        # be removed from the scanning variables. If set to 0, no variables will
        # be taken out of the iteration variables.
        'SmoothIterVars'              : False,
        #^ Activates data smoothing, which increases run time but reduces errors
        "FailDiagnostics.Boundaries"  : False
        # Prints at the end of a run what fractions of each failure included
        # iteration values at their boundaries;
        # Helps to inform the user in a rough way what variables are sensitive.
        }

    def __init__(self, configfilename="ndscan.conf"):

        """
        Contains the code which parses ndscan.conf files, manipulates IN.DAT
        files, sorts and stores MFILES,
        and runs PROCESS during the scan.

        Init makes all of the self variables, then calls
        establish_default_optionals, parse_config_file, generate_coords, and
        get_iter_vars.

        Arguments:
            configfilename--> The name of the configuration file to read.

        Dependencies:
            process_config.py RunProcessConfig class
            establish_default_optionals (calls)
            parse_config_file    (calls)
            generate_coords (calls)
            get_iter_vars   (calls)
            numpy's ndarray option
            collection module's ordered dictionary
        """

        super().__init__()

        self.parse_config_file(configfilename)

        self.setup()#XXX does not copy config file!!

        self.generate_coords()

        if self.optionals["SmoothIterVars"] is True:
            self.wasjustsmoothed = False

        self.iterationvariables = get_iter_vars("IN.DAT")
        for i in range(self.scanaxes['ndim']):
            self.scanaxes['varnumbers'].append(
                get_var_name_or_number(self.scanaxes['varnames'][i]))


    def parse_config_file(self, configfilename='ndscan.conf'):
        """
        Opens and parses the configuration file at configfilename.

        Gets the following information from the ndscan file:
        Required:
                    Axes                (at least one)
                        -"Varname"      (string)
                        -"Lowerbound"   (float)
                        -"Upperbound"   (float)
                        -"Steps" (number of evaluations, must be greater than 1)

        Optional:   OutputDirectory     (Where to save the mfiles)
                    VariablesOfInterst  (What variables swill be extracted from
                                         Mfiles)
                    Author
                    Title       (Will be used in naming the NetCDF file)
                    Description (Saved in the netcdf file)
                    Optionals   (Established in establish_default_optionals,
                                changes behavior function)
        Arguments:
            configfilename--> The name of the configuration file to open

        """

        # Open the config file with the json module
        self.configfile = NdScanConfigFile(configfilename)

        # This overwrites the default optionals with the ones found..
        for option in self.configfile.get_value("Optionals").keys():
            self.optionals[option] = \
                self.configfile.get_value("Optionals")[option]


        # Grab the number of dimensions
        self.scanaxes['ndim'] = len(self.configfile.get_value("Axes"))

        # Gets the varname, lowerbound, upperbound, and step # for each axis
        for var in range(self.scanaxes['ndim']):
            self.scanaxes['varnames'].append(
                self.configfile.get_value("Axes")[var]["Varname"])

            if type(self.configfile.get_value("Axes")[var]["Steps"]) is list:
                self.scanaxes['steps'].append(
                    len(self.configfile.get_value("Axes")[var]["Steps"]))
            else:
                self.scanaxes['lbs'].append(
                    self.configfile.get_value("Axes")[var]["Lowerbound"])
                self.scanaxes['ubs'].append(
                    self.configfile.get_value("Axes")[var]["Upperbound"])
                self.scanaxes['steps'].append(
                    self.configfile.get_value("Axes")[var]["Steps"])

        currentdirectory = os.getcwd()
        try:
            os.stat(currentdirectory + '/' + self.mfiledir)
        except FileNotFoundError:
            os.mkdir(currentdirectory + '/' + self.mfiledir)



    def generate_coords(self):
        """
        From the bounds and number of evaluations specified, generates the
        N-dimensional coordinates to scan.

        This generates the coodinates for the variables so that
        the process scanner later can iterate through them. The coordinates are
        linearly interpolated by the upper bound, lower bound, and number of
        steps specified.

        Additionally, containers for output coordinates and failure coordinates
        are created The failure coordinates
        store a value from ifail, which can later be interpreted to figure out
        what went wrong with PROCESS in a given run.

        """

        totalsteps = []
        #If Manual Steps are specified, will construct the coordinates
        #from what is given.
        # otherwise,
        # Constructs a linear interpolation of the x and y variables
        # by calculating the difference of the upper and lower bound,
        # divided by the step length,
        # then producing a list of those steps in the range.
        for i in range(self.scanaxes['ndim']):

            self.currentstep.append(0)

            if type(self.configfile.get_value("Axes")[i]["Steps"]) is list:
                self.scanaxes['coords'].append(
                    self.configfile.get_value("Axes")[i]["Steps"])
                totalsteps.append(len(self.scanaxes['coords'][i]))

            else:
                lbnd = self.scanaxes['lbs'][i]
                ubnd = self.scanaxes['ubs'][i]
                steps = int(self.scanaxes['steps'][i])
                self.scanaxes['coords'].append(
                    [lbnd + j*(ubnd - lbnd) / (steps-1) for j in range(steps)])
                totalsteps.append(steps)

        # The program will later scan the resultant MFILEs in the PROCESS
        # code to extract the failure states of the runs and
        # a z variable of our choosing which can be plotted against 2 x and
        # y dimensions.
        # The failure coordinates and output coordinates store these
        # values respectively.


        self.failcoords = ndarray(tuple(totalsteps), int)

    def adjust_variable(self, varname, newvalue):
        """
         Given a variable of varname and newvalue, modifies an already
         existing variable
         in the ProcessRunConfig's internal IN.dat memory.

        Arguments:
            varname--->Name of the variable to modify. string
            newvalue-->Value to assign to varname.

        Dependencies:
            RunProcessConfig.modify_vars()

        Modifies:
            self.dictvar (temporarily, from parent class)
        """

        varname = str(varname)
        varname = varname.lower()

        self.dictvar[varname] = newvalue

        RunProcessConfig.modify_vars(self)
        self.dictvar = {}


    def catalog_output(self, currentstep):
        """
        Copies the MFILE.DAT in the current directory to subdirectory MFILES
        with a name determined by currentstep.

        Because PROCESS outputs an MFILE for each run, and many runs will be
        made in a given Nd scan, this stores away the files produced to be
        analyzed later.

        Files are tagged with the steps of the variables from the run.
        For example, currentstep = [0,12,5,1] produces M.0.12.5.1.DAT.

        Arguments:
            currentstep--> The list describes the current step the scanner
            is on.
        """

        stepstring = ''

        for dims in range(self.scanaxes['ndim']):
            stepstring += str(currentstep[dims]) + '.'

        destination = self.mfiledir + "/M." + stepstring + "DAT"
        subprocess.call(["cp", "MFILE.DAT", destination])


    def before_run_do(self):
        """
        A helper function: executes commands that need to be done before a run,
        including incrementing a counter or modifying IN.DAT.

        Also a place for developers to add functionality.

        Modifies:
            self.counter

        Dependencies:
            self.smooth_iter_variables (sometimes)

        """

        self.counter += 1

        # We can't smooth the variables if we are on the first run
        # so it waits until counter is greater than 1.
        if self.counter > 1 and self.optionals['SmoothIterVars'] is True:
            lastrun = MFile()
            #Establishing a new self variable outside of __init__; is that ok?
            backup_in_file("w", "IN.DATSMOOTHERBACKUP")
            if self.smooth_iter_variables(lastrun):
                self.wasjustsmoothed = True
            else:
                self.wasjustsmoothed = False
                subprocess.call(["rm", "IN.DATSMOOTHERBACKUP"])


    def after_run_do(self):

        """
        Executes commands that need to be done just after a run, such as
        recording the ifail value.
        """

        check_input_error()

        currentrun = MFile()
        if process_stopped():
            currentfail = 0
        else:
        #Check the Mfile that was just produced to see if there was a failure
        #or not-
        #Re reun with new iteration variable values if this was specified.
            currentfail = int(currentrun.data["ifail"].get_scan(-1))

        if self.niter != 0 and currentfail != 1:

            for x in range(self.niter):
                print("Ifail = ", int(currentfail), " Rerunning ", x + 1,
                      "of", self.niter)

                if currentfail != 1:
                    # Does not work, if bounds in input.f90 are tighter
                    # than boundu/boundl
                    neqns, itervars = get_neqns_itervars()
                    lbs, ubs = get_variable_range(itervars, self.factor)
                    vary_iteration_variables(itervars, lbs, ubs)
                    self.run_process()
                    check_input_error()

                    currentrun = MFile()
                    if process_stopped():
                        currentfail = 0
                    else:
                        currentfail = int(currentrun.data["ifail"].get_scan(-1))
                else:
                    break


        if self.optionals['SmoothIterVars'] and self.wasjustsmoothed:
            backup_in_file("r", "IN.DATSMOOTHERBACKUP")
            subprocess.call(["rm", "IN.DATSMOOTHERBACKUP"])
            self.wasjustsmoothed = False



        if self.optionals["FailDiagnostics.Boundaries"] and currentfail > 1:

            nitvars = get_iter_variables_from_mfile(currentrun,
                                                           normalized=True)
            for varname in nitvars.keys():

                #Take off the last 19 characters because the last 19
                #characters are "_(range_normalised)"
                if nitvars[varname] == 0:
                    self.iterationvariablesatbounds[varname[:-19]][0] += 1

                elif nitvars[varname] == 1:
                    self.iterationvariablesatbounds[varname[:-19]][1] += 1

        #Records the ifail value of the last run
        self.failcoords[tuple(self.currentstep)] = int(currentfail)


        # Checks the error status
        if currentrun.data["error status"].get_scan(-1) > 0:
            self.errorlist[tuple(self.currentstep)] = \
                (currentrun.data["error status"].get_scan(-1),\
                     currentrun.data["error id"].get_scan(-1))



        if self.failcoords[tuple(self.currentstep)] != 1:
            self.totalfails += 1
            print("Run failed! With ifail = ",\
                      self.failcoords[tuple(self.currentstep)])

        else:
            print("Run successful.")


        # Stores the MFILE away with the convention used of
        # M.currentstep[0].currentstep[1]......currentstep[N].DAT
        # So currentstep=[1,2,4,0] => M.1.2.4.0.DAT
        if self.optionals['StoreOutput']:
            self.catalog_output(self.currentstep)


    def dimension_scan(self, currentdim):
        """
        A recursive function which conducts the N dimensional scan when called.
        currentdim keeps track of what dimension level the scan is on.

        When currentdim=0, the scan ticks along the first variable.

        Works through the coordinates determined by the step #, upper bound,
        and lower bound
        for a given dimension.

        Should only be called once from start_scan with the number of
        dimensions being scanned-1.

        Arugments:
            currentdim---> Integer which keeps track of 'how deep' the scanner
            is while iterating over the axes.


        Dependencies:
            self.adjust_variable (calls)
            self.dimension_scan (calls)
            self.before_run_do  (calls)
            self.after_run_do   (calls)
        """

        for coord in self.scanaxes['coords'][currentdim]:

            #Updates the current step according to steps along the coordinates.
            self.currentstep[currentdim] = \
            self.scanaxes['coords'][currentdim].index(coord)

            #Adjusts the scanned variable to the new coordinate.
            self.adjust_variable(self.scanaxes['varnames'][currentdim], coord)

            if currentdim > 0:
                # Activates the recursion
                self.dimension_scan(currentdim - 1)
            else:
                # Before and after are different methods for ease of access,
                # modification, and cleaner code.

                self.before_run_do()

                print("Current step coordinate:", self.currentstep)
                self.run_process()

                self.after_run_do()



    def smooth_iter_variables(self, mfile):
        """
        Sets the values of the iteration variables in IN.DAT equal to the
        values from the last run,
        if it was not a failure. Work in progress.

        Returns:
            True----> If iteration variables were attempted to be modfied
                      because it was an appropriate scan point
            False---> If the conditions were not right for modifcation of
                      iteration variables (due to a previous failure,
                      or due to it being at a new starting point.

        Arguments:
            mfile---> An MFile object from mfile.py

        Dependencies:
            self.currentstep
            self.modify_vars (from super class)
            self. adjust_variable
        """


        #If the first dimension has a step value of 0,
        # we can infer that a most likely nontrivial 'jump' just happened
        #across the parameter space which
        # can seriously break the smoothness, and maybe the solver.
        # Re-smoothing from here might cause an error. So,
        # we don't smooth it from the previous run.

        if self.currentstep[0] == 0:
            return False

        elif process_stopped():
            return False

        #As long as we aren't in that smoothness breaking case,
        # checks to see if the previous run was succesful.
        # If it was, then re-adjust the current iter variables accordingly.

        elif mfile.data['ifail'].get_scan(-1) == 1:
            nvar = int(mfile.data['nvar'].get_scan(-1))
            for i in range(1, nvar+1):
                itervarstring = "itvar%03i" %i

                value = mfile.data[itervarstring].get_scan(-1)
                itervarname = mfile.data[itervarstring].var_description
                if value != 0:
                    self.adjust_variable(itervarname, value)
            return True
        return False



    def after_scan(self):
        """ #FINISH THIS DOCSTRING
        A helper method which runs at the conclusion of the ND scan and
        carries out actions like printing output.


        """

        if len(self.errorlist) > 0:

            if len(self.errorlist.keys()) > 0:
                print(len(self.errorlist),\
                          "runs had some sort of error associated with them.")

        print("Now printing failure coordinates below:")
        print(self.failcoords)
        print("Scan complete.", self.totalfails, " of ", self.totalruns,\
                  " runs ended in failure.")

        if self.optionals["FailDiagnostics.Boundaries"] and self.totalfails > 0:
            print("Now printing failure diagnostics on boundary conditions:")
            for x in self.iterationvariablesatbounds.keys():
                if self.iterationvariablesatbounds[x][0] +\
                        self.iterationvariablesatbounds[x][1] > 0:
                    sumfails = self.iterationvariablesatbounds[x][0] +\
                        self.iterationvariablesatbounds[x][1]
                    print("Ixc", x, "\twas at a lower bound for ",\
                              self.iterationvariablesatbounds[x][0],\
                              "/", self.totalfails,\
                              " failures and at an upper bound for ",\
                              self.iterationvariablesatbounds[x][1], "/",\
                          self.totalfails, " failures,\t for a total of",\
                          sumfails, " of ", self.totalfails,\
                          "(", 100 - \
                          (100*(self.totalfails-sumfails) / (self.totalfails)),\
                              "%)")


        return(self.totalfails, self.totalruns)

    def start_scan(self):
        """
        Commences the N-dimensional scan, and calls all of the necessary
        methods.
        dimension_scan is the function which carries out 'motion' along
        the axes, which is a recursive function which
        calls itself however many times it needs to.
        """
        if self.optionals['RemoveScanVarsFromIteration']:

            self.iterationvariables = get_iter_vars()

            for varname in self.scanaxes['varnames']:
                if varname in self.iterationvariables.values():
                    print("Warning! Removing scan variable", varname,\
                              " from the iteration variable list.")
                    self.del_ixc.append(get_var_name_or_number(varname))
                    print("Please either reconsider your iteration variables\
 in the IN.DAT file in the future, or if this was intended, set the\
 RemoveIterVars optional to False in your config file.")
                    self.modify_ixc()

        if self.optionals['RemoveSomeVarsFromIteration'] > 0:
            numbertodelete = self.optionals["RemoveSomeVarsFromIteration"]
            print("Warning! Removing the first ", numbertodelete,\
                      "variables from the iteration variables.")
            for i in range(numbertodelete):
                self.del_ixc.append(self.scanaxes['varnumbers'][i])
            self.modify_ixc()

        if self.optionals['SmoothIterVars']:
            if not self.optionals['StoreOutput']:
                print('Cannot smooth the jumps: no adjacent successful\
 MFILE exists in records')
                self.optionals['SmoothIterVars'] = False
            else:
                get_iter_vars()

        if self.optionals["FailDiagnostics.Boundaries"]:
            self.iterationvariablesatbounds = get_iter_vars("IN.DAT", True)

        self.totalruns = 1
        for i in range(self.scanaxes['ndim']):
            self.totalruns *= self.scanaxes['steps'][i]

        self.counter = 0

        backup_in_file("w")
        self.dimension_scan(self.scanaxes['ndim'] - 1)
        backup_in_file("r")

        self.after_scan()




