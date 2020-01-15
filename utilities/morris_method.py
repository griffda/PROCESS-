import argparse
from SALib.sample.morris import sample 
from SALib.analyze import morris 
from process_io_lib.process_config import RunProcessConfig
from process_io_lib.in_dat import InDat
from process_io_lib.mfile import MFile
from process_io_lib.process_funcs import set_variable_in_indat, process_stopped
import numpy as np
import pandas as pd

def Get_Input():
    #Define the model inputs
    problem = {
        'num_vars': 21,
        'names': ['boundu(9)', 'hfact', 'coreradius','fimp(2)','fimp(14)',\
            'psepbqarmax','boundl(103)','cboot','peakfactrad','kappa',\
            'etaech','feffcd','etath','etaiso','boundl(18)','pinjalw',\
            'alstroh','alstrtf','aspect','bmxlim','triang'],
        'bounds': [[1.1, 1.3],
                   [1.0, 1.2],
                   [0.45, 0.75],
                   [0.085, 0.115],
                   [1.0e-5, 1.0e-4],
                   [8.7, 9.7],
                   [0.75, 1.25],
                   [0.985, 1.015],
                   [2.0, 3.5],
                   [1.83, 1.86],
                   [0.3, 0.5],
                   [0.5, 5.0],
                   [0.36, 0.40],
                   [0.75, 0.95],
                   [3.40, 3.60],
                   [51.0, 61.0],
                   [6.0e8, 7.2e8],
                   [5.2e8, 6.4e8],
                   [3.08, 3.12],
                   [11.0, 12.0],
                   [0.475, 0.525]] 
    }
    return problem

if __name__ == "__main__":

    # set up command line arguments
    PARSER = argparse.ArgumentParser(description='Program to evaluate\
 model sensistivity by elementary element method at a given PROCESS design point.')

    PARSER.add_argument("-f", "--configfile", default="run_process.conf", type=str,
                        help="configuration file, default = run_process.conf")
    PARSER.add_argument("-o", "--outputvarname", default="capcost", type=str,
                        help="PROCESS output analysed, default = capcost")
    PARSER.add_argument("-s", metavar="SOLLIST", default="capcost_sol.txt", type=str,
                        help="filename of PROCESS outputs, default = capcost_sol.txt")
    PARSER.add_argument("-e", metavar="ERRORLIST", default="error_log.txt", type=str,
                        help="filename of failed PROCESS output, default = error_log.txt")
    PARSER.add_argument("-m", metavar="OUTPUTMEAN", default=8056.98,
                        help="PROCESS mean model output value, default = 8056.98 (DEMO capcost)")
    PARSER.add_argument("-t", metavar="TRAJNUM", default=25, type=int,
                        help="number of trajectories sampled, default = 25")
    PARSER.add_argument("-n", metavar="NUMLVLS", default=4, type=int,
                        help="Number of grid levels used in hypercube sampling, default = 4")

                        # need a solution to a hard coded dict for the inputs...

    ARGS = PARSER.parse_args()

    # Get parameters
    params_bounds = Get_Input()
    capcost_mean = ARGS.m # 8056.98 #8631.54 older I think from FoM = 1  # fix this to argparse
    traj_num = ARGS.t 
    capcost_sols = np.array([])
    fail_rows = np.array([])

    # Generate samples
    params_values = sample(params_bounds, traj_num, num_levels=8)

    np.savetxt("param_values.txt", params_values)

    CONFIG = RunProcessConfig(ARGS.configfile)
    CONFIG.setup()

    in_dat = InDat()

    run_max = int((params_bounds['num_vars'] + 1.0) * traj_num)
    for run_id in range(run_max):
        print('run number =',run_id)
        i = 0
        for n in params_bounds['names']:
            #print(params_values[run_id][i])
            set_variable_in_indat(in_dat,n,params_values[run_id][i])
            i = i + 1

        in_dat.write_in_dat(output_filename='IN.DAT')

        CONFIG.run_process()

        m_file = MFile(filename="MFILE.DAT")

        # We need to find a way to catch failed runs
        process_status = m_file.data['ifail'].get_scan(-1)
        print('ifail =', process_status)

        if process_status == 1.0:
            # read the capcost from the MFILE  
            capcost = m_file.data[ARGS.outputvarname].get_scan(-1) # this needs to talk to argparse
            #capcost = m_file.data["coe"].get_scan(-1) # this is hard coded - need to be selected in an arg parse
            capcost_sols = np.append(capcost_sols, capcost)
        else:
            fail_rows = np.append(fail_rows, run_id)
            if run_id == 0:
                #make mean
                capcost_sols = np.append(capcost_sols, capcost_mean)
            else:
                capcost_sols = np.append(capcost_sols, capcost_sols[np.size(capcost_sols)-1])
    
    params_sol = morris.analyze(params_bounds,params_values,capcost_sols)
    np.savetxt(ARGS.s, capcost_sols) # fix in the argparse
    np.savetxt(ARGS.e, fail_rows) # also fix in the arg parse
    df = pd.DataFrame(params_sol)
    print(df)