"""
Author: Hanni Lux (Hanni.Lux@ccfe.ac.uk)

Interfaces for Configuration values for programs
- run_process.py
- test_process.py
- evaluate_uncertainties.py

Compatible with PROCESS version 368

"""

import os
import subprocess
from time import sleep
from numpy.random import seed, uniform, normal
from numpy import argsort
from process_io_lib.in_dat import InDat
from process_io_lib.process_funcs import get_from_indat_or_default,\
    set_variable_in_indat

from process_io_lib.mfile import MFile
from process_io_lib.configuration import Config
from process_io_lib.process_dicts import DICT_NSWEEP2IXC, DICT_NSWEEP2VARNAME,\
    DICT_IXC_SIMPLE
from process_io_lib.process_netcdf import NetCDFWriter

def print_config(config_instance):
    print(config_instance.get_current_state())



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
    wdir     = '.'
    or_in_dat = 'IN.DAT'
    process  = 'process'
    niter    = 10
    u_seed   = None
    factor   = 1.5
    comment  = ''


    def echo_base(self):

        """ echos the attributes of the base class """

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

    def echo(self):

        """ echos the values of the current class """

        print('')
        self.echo_base()
        print('')

    def prepare_wdir(self):

        """ prepares the working directory """

        try:
            os.stat(self.wdir)
        except FileNotFoundError:
            os.mkdir(self.wdir)

        subprocess.call(['cp ' + self.or_in_dat + ' ' + self.wdir + '/IN.DAT'])
        subprocess.call(['cp ' + self.filename + ' ' + self.wdir])
        os.chdir(self.wdir)
        subprocess.call(['rm -f OUT.DAT MFILE.DAT PLOT.DAT \
                   *.txt *.out *.log *.pdf *.eps *.nc'])

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
        #check_in_dat()

        seed(self.u_seed)



    def run_process(self):

        """ runs PROCESS binary """

        print("PROCESS run started ...", end='')
        returncode = subprocess.call([self.process+' >& process.log'])
        if returncode != 0:
            print('\n Error: There was a problem with the PROCESS \
                   execution! %i' % returncode)
            print('       Refer to the logfile for more information!')
            exit()
        print("finished.")


    def get_comment(self):

        """ gets the comment line from the configuration file """

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            exit()

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

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            exit()

        for line in configfile:
            condense = line.replace(' ', '')
            if (condense[0] != "*") and len(condense) > 1:
                if '=' in line:
                    varname = line[:line.find("=")]
                    varname = varname.replace(' ', '')
                    varname = varname.upper()
                    auxvar = line[line.find("=")+1:-1]
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

    def set_base_attributes(self):

        """ sets the attributes of the base class """

        buf = self.get_attribute('wdir')
        if buf != None:
            self.wdir = buf

        buf = self.get_attribute('ORIGINAL_IN_DAT')
        if buf != None:
            self.or_in_dat = buf

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

        self.set_base_attributes()

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
        self.echo_base()

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
            in_dat.data['ioptimz'].add_parameter('ioptimz',
                                                 self.ioptimz)
        if self.epsvmc != 'None':
            in_dat.data['epsvmc'].add_parameter('epsvmc', self.epsvmc)

        if self.epsfcn != 'None':
            in_dat.data['epsfcn'].add_parameter('epsfcn', self.epsfcn)

        if self.minmax != 'None':
            in_dat.data['minmax'].add_parameter('minmax', self.minmax)

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

        self.set_base_attributes()

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

        try:
            configfile = open(self.filename, 'r')
        except FileNotFoundError:
            print('Error: No config file named %s' %self.filename)
            exit()

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

        configfile = open(self.filename, 'r')

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

        configfile = open(self.filename, 'r')
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
        self.echo_base()

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

            key = key.lower()

            #TODO check this is a parameter??
            in_dat.data[key].set_parameter(key, self.dictvar[key])


        #delete variables
        #TODO check key is a parameter?
        for key in self.del_var:
            key = key.lower()
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
        self.echo_base()

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
                try :
                    ncdf_writer.write_mfile_data(m_file, run_id,
                                                 save_vars=self.output_vars,
                                                 latest_scan_only=True,
                                                 ignore_unknowns=False)
                except KeyError as Err:
                    print('Error: You have specified an output variable that\
 does not exist in MFILE. If this is a valid PROCESS variable, request it being\
 added to the MFILE output, else check your spelling!')
                    print(Err)
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


#if __name__ == "__main__":
#    cfg = RunProcessConfigNew("../run_process.json")
#    cfg.setup_directory()

