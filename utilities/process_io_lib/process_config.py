"""
Author: Hanni Lux (Hanni.Lux@ccfe.ac.uk)

Interfaces for Configuration values for programs
- run_process.py
- test_process.py
- evaluate_uncertainties.py

Compatible with PROCESS version 368

"""

import os
from time import sleep
from numpy.random import seed
from process_io_lib.in_dat import INDATNew, INVariable
from process_io_lib.process_funcs import mfile_exists
from process_io_lib.mfile import MFile
from process_io_lib.configuration import Config

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

        print("Working directory:   %s" % self.wdir)
        print('Original IN.DAT:     %s' % self.or_in_dat)
        print("PROCESS binary:      %s" % self.process)
        print('Number of iterations %i' % self.niter)
        if self.u_seed != None:
            print('random seed          %i' % self.u_seed)
        print('variable range factor %f' % self.factor)
        if self.filename != None:
            print('Config file          %s' % self.filename)
        if self.comment != '':
            print('Comment  %s'         % self.comment)

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

        os.system('cp ' + self.or_in_dat + ' ' + self.wdir + '/IN.DAT')
        os.system('cp ' + self.filename + ' ' + self.wdir)
        os.chdir(self.wdir)
        os.system('rm -f OUT.DAT MFILE.DAT PLOT.DAT \
                   *.txt *.out *.log *.pdf *.eps')

    def create_readme(self, directory='.'):

        """ creates README.txt containing comment """

        if self.comment != '':

            readme = open(directory+'/README.txt', 'w')
            readme.write(self.comment)
            readme.close()


    def error_status2readme(self, directory='.'):

        """ appends PROCESS outcome to README.txt """

        if mfile_exists():
            if self.comment != '':
                readme = open(directory+'/README.txt', 'a')
            else:
                readme = open(directory+'/README.txt', 'w')

            m_file = MFile(filename=directory+"/MFILE.DAT")
            error_status = "Error status: %i  Error ID: %i\n" %(m_file.data['error status'].get_scan(-1), m_file.data['error id'].get_scan(-1))
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
        returncode = os.system(self.process+' >& process.log')
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

        in_dat = INDATNew()

        #by convention all variablenames are lower case
        if self.ioptimz != 'None':
            if 'ioptimz' in in_dat.variables.keys():
                in_dat.variables['ioptimz'].value = self.ioptimz
            else:
                in_dat.variables['ioptimz'] = INVariable('ioptimz',
                                                         self.ioptimz)
        if self.epsvmc != 'None':
            if 'epsvmc' in in_dat.variables.keys():
                in_dat.variables['epsvmc'].value = self.epsvmc
            else:
                in_dat.variables['epsvmc'] = INVariable('epsvmc', self.epsvmc)

        if self.epsfcn != 'None':
            if 'epsfcn' in in_dat.variables.keys():
                in_dat.variables['epsfcn'].value = self.epsfcn
            else:
                in_dat.variables['epsfcn'] = INVariable('epsfcn', self.epsfcn)

        if self.minmax != 'None':
            if 'minmax' in in_dat.variables.keys():
                in_dat.variables['minmax'].value = self.minmax
            else:
                in_dat.variables['minmax'] = INVariable('minmax', self.minmax)

        in_dat.write_in_dat(filename='IN.DAT')


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
            if buf.lower() in ['true', 'y', 'yes'] :
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
            print('Set to create a summary file of the iteration variable values!')

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

        in_dat = INDATNew()

        #add and modify variables
        for key in self.dictvar.keys():
  
            key = key.lower()
 
            if key in in_dat.variables.keys():
                in_dat.variables[key].value = self.dictvar[key]
            else:
                in_dat.add_variable(INVariable(key, self.dictvar[key]))
                #in_dat.variables[key] = INVariable(key, self.dictvar[key])
  
        #delete variables
        for key in self.del_var:
            key = key.lower()
            in_dat.remove_variable(key)

        in_dat.write_in_dat(filename='IN.DAT')


    def modify_ixc(self):

        """ modifies the array of iteration variables in IN.DAT """


        #check that there is no variable in both lists
        if set(self.add_ixc).intersection(self.del_ixc) != set([]):
            print('Error: You are trying to add and delete \
                   the same variable from ixc!')
            exit()

        in_dat = INDATNew()

        for iter_var in self.add_ixc:
            in_dat.add_iteration_variable(int(iter_var))

        for iter_var in self.del_ixc:
            in_dat.remove_iteration_variable(int(iter_var))

        in_dat.write_in_dat(filename='IN.DAT')


    def modify_icc(self):

        """ modifies the array of constraint equations in IN.DAT"""

        #check that there is no variable in both lists
        if set(self.add_icc).intersection(self.del_icc) != set([]):
            print('Error: You are trying to add and delete the same \
                  variable from icc!')
            exit()

        in_dat = INDATNew()

        for constr in self.add_icc:
            in_dat.add_constraint_eqn(int(constr))

        for constr in self.del_icc:
            in_dat.remove_constraint_eqn(int(constr))

        in_dat.write_in_dat(filename='IN.DAT')


################################################################################
#class UncertaintiesConfig(RunProcessConfig)
################################################################################


class UncertaintiesConfig(ProcessConfig, Config):

    """ 
    Configuration parameters for evaluate_uncertainties.py program

    """

    no_scans = 5
    no_samples = 10000
    uncertainties = []
    output_vars = []


    def __init__(self, configfilename="evaluate_uncertainties.json"):

        """
        creates and instance of the UncertaintiesConfig class 
        """

        #TODO: Once ProcessConfig has been ported to only use json
        #use the ProcessConfig __init__ routine to set up these 
        #parameters
        super().__init__(configfilename)
        self.filename = configfilename

        self.wdir = os.path.abspath(self.get("config", "working_directory",default=self.wdir))
        self.or_in_dat = os.path.abspath(self.get("config", "IN.DAT_path",default=self.or_in_dat))
        self.process = self.get("config", "process_bin", default=self.process)
        #self.niter = should not get changed?
        #self.u_seed = 
        #self.factor = 
        self.comment = self.get("config", "runtitle", default=self.comment)

        #additional new parameters
       # self.no_scans = 
        self.no_samples = self.get("no_samples", default=self.no_samples)
        self.uncertainties = self.get("uncertainties", default=self.uncertainties)
