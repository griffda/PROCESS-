#!/usr/bin/env python
import matplotlib
matplotlib.use('Agg')
import os
import argparse
from argparse import RawTextHelpFormatter
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines  as mlines
from create_dicts import get_dicts

# Load dicts from dicts JSON file
p_dicts = get_dicts()


if __name__ == '__main__':
    #####################################################
    ##            PARSING USER PARAMETERS              ##
    #####################################################
    # please execute 'python plot_stress_tf.py -h' for input information
    # Option definition
    # -----------------
    parser = argparse.ArgumentParser( description='Plot optimization information', formatter_class=RawTextHelpFormatter )
    parser.add_argument('-p'  , '--plot_selec'    , nargs='?', default='all', help="Plot selection string :\n - If it containts 'sig'      -> Stress radial dependency \n - If it containts 'disp'     -> Displacement \n \n - If it containts 'all'      -> all the mentionned plots (default value)")
    parser.add_argument('-sf' , '--save_format'   , nargs='?', default='pdf', help="output format (default='pdf') " )
    parser.add_argument('-as' , '--axis_font_size', nargs='?', default=18   , help="Axis label font size selection (default=18)", type=int )

    # Option argument extraction
    # --------------------------
    args = parser.parse_args()
    plot_selection = str(args.plot_selec)
    save_format    = str(args.save_format)
    axis_font_size = int(args.axis_font_size)
    
    ## Boolean swiches for plot selection
    # -----------------------------------
    plot_sig  = ( 'sig'  in plot_selection ) or 'all' == plot_selection
    plot_disp = ( 'disp' in plot_selection ) or 'all' == plot_selection
    #####################################################

    
    ## Step 1 : Data extraction
    # ----------------------------------------------------------------------------------------------
    n_radial_array_layer = int()
    radius               = list()
    radial_stress        = list()
    toroidal_stress      = list()
    vertical_stress      = list()
    vm_stress            = list()
    tresca_stress        = list()
    cea_tresca_stress    = list()
    radial_displacement  = list()

    data = list()

    # Opening the pandora box
    with open('../bin/SIG_TF.DAT', 'r') as sig_data :
   
        ii = 0
        sig_data_lines = sig_data.readlines()
        for sig_data_line in sig_data_lines :
            
            data.append(list())
            for str_data in sig_data_line[25:].split(" ")  :       
                if not str_data == "" :
                    data[ii].append(float(str_data))

            ii += 1
   
    n_radial_array_layer = int(data[0][0])
    radius            = data[2]
    radial_stress     = data[3]
    toroidal_stress   = data[4]   
    vm_stress         = data[6]
    tresca_stress     = data[7]
    cea_tresca_stress = data[8]
    radial_displacement = data[11]

    if len(data[5]) == 1 :
        for jj in range(0,len(radius)) :
            vertical_stress.append(data[5][0])
    
    outdir = str("SIG_TF_plots")
    if not os.path.isdir(outdir) :
        os.mkdir(outdir)

    ## PLOT 1 : Figure of merit evolution
    # -----------------------------------
    if plot_sig :

        axis_tick_size = 16
        legend_size    = 12
        plt.plot(radius, radial_stress  , '--', label = r'$\sigma_{r}$')
        plt.plot(radius, toroidal_stress, '--', label = r'$\sigma_{\theta}$')
        plt.plot(radius, vertical_stress, '--', label = r'$\sigma_{z}$')
        plt.plot(radius, tresca_stress  , '-' , label = r'$\sigma_{TRESCA}$')
        plt.plot(radius, vm_stress      , '-' , label = r'$\sigma_{Von\ mises}$')
        plt.grid(True)
        plt.ylabel( r'$\sigma$ [$MPa$]', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.legend( loc = 'best', fontsize = legend_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/stresses.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()

    if plot_disp :
        plt.plot(radius, radial_displacement )
        plt.grid(True)
        plt.ylabel( r'$u_{r}$ [mm]', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/displacement.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()