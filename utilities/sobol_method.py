import argparse
from SALib.sample.saltelli import sample
from SALib.analyze import sobol
from process_io_lib.process_config import RunProcessConfig
from process_io_lib.in_dat import InDat
from process_io_lib.mfile import MFile
from process_io_lib.process_funcs import set_variable_in_indat, process_stopped
import numpy as np

def Get_Input():
    #Define the model inputs
    problem = {
        'num_vars': 8,
        'names': ['hfact','coreradius','kappa','etaech','etath','boundl(18)','alstrtf','triang'],
        'bounds': [[1.1, 1.3],
                   [0.45, 0.75],
                   [1.83, 1.86],
                   [0.3, 0.5],
                   [0.36, 0.40],
                   [3.40, 3.60],
                   [5.2e8, 6.4e8],
                   [0.475, 0.525]],
    }
    return problem

def write_Sobol_Output(X,S):
    #open output file
    f = open('sobol.txt','w')
    
    #create first order header
    f.write('Parameter S1 S1_conf ST ST_conf\n')

    #params_bounds['num_vars']
    #params_bounds['names']
    #params_sol['S1']
    #params_sol['S1_conf']
    #params_sol['ST']
    #params_sol['ST_conf']

    #print first order Sobol indices
    for i in range(X['num_vars']):
        f.write('%s %f %f %f %f\n' % (X['names'][i], S['S1'][i], \
             S['S1_conf'][i], S['ST'][i], S['ST_conf'][i]) )

    #print second order indices header
    f.write('Parameter_1 Parameter_2 S2 S2_conf\n')

    #print second order Sobol indices
    for j in range(X['num_vars']):
        for k in range(j + 1, X['num_vars']):
            f.write("%s %s %f %f\n" % (X['names'][j], X['names'][k], \
                                       S['S2'][j, k], S['S2_conf'][j, k]))

    f.close()

if __name__ == "__main__":

    PARSER = argparse.ArgumentParser(description='Program to evaluate\
            model sensistivity by Sobols method at a given PROCESS design point.')

    PARSER.add_argument("-f", "--configfile", default="run_process.conf",
                        help="configuration file, default = run_process.conf")
    PARSER.add_argument("-i", "--inputfile", default="morris_method_conf.json", type=str,
                        help="input parameters file, default = sobol_method_conf.json")
    PARSER.add_argument("-o", "--outputvarname", default="capcost", type=str,
                        help="PROCESS output analysed, default = capcost")
    PARSER.add_argument("-s", metavar="SOLLIST", default="output_solutions.txt", type=str,
                        help="filename of PROCESS outputs, default = output_solutions.txt")
    PARSER.add_argument("-e", metavar="ERRORLIST", default="output_failed_solutions.txt", type=str,
                        help="filename of failed PROCESS output, default = output_failed_solutions.txt")
    PARSER.add_argument("-c", metavar="CONVLIST", default="output_conv_solutions.txt", type=str,
                        help="filename of converged PROCESS output, default = output_conv_solution.txt")
    PARSER.add_argument("-m", metavar="OUTPUTMEAN", default=8056.98,
                        help="PROCESS mean model output value, default = 8056.98 (DEMO capcost)")
    PARSER.add_argument("-t", metavar="ITER", default=100, type=int,
                        help="number of model iteration sampled, default = 100")

    ARGS = PARSER.parse_args()

    # Get parameters
    params_bounds = Get_Input()
    model_iter = ARGS.ITER
    capcost_mean = ARGS.m #8056.98 used from minimsing capcost on 2018 DEMO baseline ~~CHECK!

    # Setup output arrays
    sols = np.array([])
    fail = np.array([])
    sols_ifail1 = np.array([])

    # Generate samples
    params_values = sample(params_bounds, model_iter, calc_second_order=False)

    np.savetxt("param_values.txt", params_values)

    CONFIG = RunProcessConfig(ARGS.configfile)
    CONFIG.setup()

    in_dat = InDat()

    #find capcost_sols from PROCESS
    run_max = int(( params_bounds['num_vars'] + 2.0) * model_iter)
    for run_id in range(run_max):
        print('run number =',run_id)
        i = 0
        for n in params_bounds['names']:
            set_variable_in_indat(in_dat,n,params_values[run_id][i])
            i = i + 1

        in_dat.write_in_dat(output_filename='IN.DAT')

        CONFIG.run_process()

        m_file = MFile(filename="MFILE.DAT")

        # We need to find a way to catch failed runs
        process_status = m_file.data['ifail'].get_scan(-1)
        print('ifail =', process_status)

        if process_status == 1.0:
            capcost = m_file.data[ARGS.outputvarname].get_scan(-1)
            sols = np.append(sols, capcost)
            sols_ifail1 = np.append(sols_ifail1, capcost)
        else:
            fail = np.append(fail, run_id)
            sols = np.append(sols, capcost_mean)


    params_sol = sobol.analyze(params_bounds,sols,calc_second_order=False,print_to_console=True)
    np.savetxt(ARGS.s, sols)
    np.savetxt(ARGS.e, fail)
    np.savetxt(ARGS.c, sols_ifail1)
   
    #write output file
    write_Sobol_Output(params_bounds,params_sol)
