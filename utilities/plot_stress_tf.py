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
    parser.add_argument('-p'  , '--plot_selec'     , nargs='?', default='all', 
                        help="Plot selection string :\n - If it containts 'sig'      -> Stress radial dependency \n - If it containts 'strain'   -> Strain \n - If it containts 'disp'     -> Displacement \n - If it containts 'all'      -> all the mentionned plots (default value)")
    parser.add_argument('-sf' , '--save_format'    , nargs='?', default='pdf', 
                        help="output format (default='pdf') " )
    parser.add_argument('-as' , '--axis_font_size' , nargs='?', default=18   ,
                         help="Axis label font size selection (default=18)", type=int )
    parser.add_argument('-out', '--term_output', action="store_true",
                        help="Option to show stress on terminal output" )

    # Option argument extraction
    # --------------------------
    args = parser.parse_args()
    plot_selection = str(args.plot_selec)
    save_format    = str(args.save_format)
    axis_font_size = int(args.axis_font_size)
    term_output = args.term_output
    
    ## Boolean swiches for plot selection
    # -----------------------------------
    plot_sig    = ( 'sig'    in plot_selection ) or 'all' == plot_selection
    plot_disp   = ( 'disp'   in plot_selection ) or 'all' == plot_selection
    plot_strain = ( 'strain' in plot_selection ) or 'all' == plot_selection
    plot_sm_sig = ( 'sm_sig' in plot_selection ) or 'all' == plot_selection
    #####################################################

    
    ## Step 1 : Data extraction
    # ----------------------------------------------------------------------------------------------
    n_radial_array_layer    = int()
    radius                  = list()
    radial_smeared_stress   = list()
    toroidal_smeared_stress = list()
    vertical_smeared_stress = list()
    radial_stress           = list()
    toroidal_stress         = list()
    vertical_stress         = list()
    vm_stress               = list()
    tresca_stress           = list()
    cea_tresca_stress       = list()
    radial_strain           = list()
    toroidal_strain         = list()
    vertical_strain         = list()
    radial_displacement     = list()

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
    radius                  = data[2]
    radial_stress           = data[3]
    toroidal_stress         = data[4]   
    vertical_stress         = data[5]
    radial_smeared_stress   = data[6]
    toroidal_smeared_stress = data[7]
    vertical_smeared_stress = data[8]
    vm_stress               = data[9]
    tresca_stress           = data[10]
    cea_tresca_stress       = data[11]
    radial_displacement     = data[14]
            
    if len(data[5]) == 1 :    
        for jj in range(0,len(radius)) :
            vertical_stress.append(data[5][0])
    else :
            vertical_stress = data[5]
    
    if len(data) > 16 :
        radial_strain = data[17]
        toroidal_strain = data[18]
        
        for jj in range(0,len(radius)) :
            vertical_strain.append(data[19])


    if term_output :
        ii_ins =  list()
        ii_mids = list()
        ii_outs = list()
    
        ii = int(0)
        ii_mid = int()
        while ii_mid < len(radius) : 
            ii_in =  ii*n_radial_array_layer
            ii_mid = ii*n_radial_array_layer + int(0.5*float(n_radial_array_layer))
            ii_out = ii*n_radial_array_layer + n_radial_array_layer - 1
            
            ii_ins.append(ii_in)
            ii_mids.append(ii_mid)
            ii_outs.append(ii_out)
            ii += 1
    
        print("")
        print("")
        print("Layer stress details")
        print("____________________")
    
    
        for ii in range(0,len(ii_mids) - 1) :
            print("Layer {}".format( ii+1 ))
            print("------------------------------")
            print("steel radial   stress stress in the inner/middle/out : {}/{}/{} MPa".format(radial_stress[ii_ins[ii]], radial_stress[ii_mids[ii]], radial_stress[ii_outs[ii]]) )
            print("steel toroidal stress stress in the inner/middle/out : {}/{}/{} MPa".format(toroidal_stress[ii_ins[ii]], toroidal_stress[ii_mids[ii]], toroidal_stress[ii_outs[ii]]) )
            print("steel TRESCA   stress stress in the inner/middle/out : {}/{}/{} MPa".format(tresca_stress[ii_ins[ii]], tresca_stress[ii_mids[ii]], tresca_stress[ii_outs[ii]]) )
            print("")
        print("")

    outdir = str("SIG_TF_plots")
    if not os.path.isdir(outdir) :
        os.mkdir(outdir)

    axis_tick_size = 16
    legend_size    = 12

    ## PLOT 1 : Stress summary
    # ------------------------
    if plot_sig :
        plt.plot(radius, radial_stress  , '--', label = r'$\sigma_{rr}$')
        plt.plot(radius, toroidal_stress, '--', label = r'$\sigma_{\theta\theta}$')
        plt.plot(radius, vertical_stress, '--', label = r'$\sigma_{zz}$')
        plt.plot(radius, tresca_stress  , '-' , label = r'$\sigma_{TRESCA}$')
        plt.plot(radius, vm_stress      , '-' , label = r'$\sigma_{Von\ mises}$')
        plt.grid(True)
        plt.ylabel( r'$\sigma$ [$MPa$]', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.legend( loc = 'best', fontsize = legend_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/steel_stress.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()

    ## PLOT 2 : Smeared stress summary
    # ------------------------
    if plot_sm_sig :
        plt.plot(radius, radial_smeared_stress  , label = r'$\sigma_{rr}^\mathrm{smeared}$')
        plt.plot(radius, toroidal_smeared_stress, label = r'$\sigma_{\theta\theta}^\mathrm{smeared}$')
        plt.plot(radius, vertical_smeared_stress, label = r'$\sigma_{zz}^\mathrm{smeared}$')
        plt.grid(True)
        plt.ylabel( r'$\sigma$ [$MPa$]', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.legend( loc = 'best', fontsize = legend_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/smeared_stress.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()

    ## PLOT 3 : Strain summary
    # ------------------------
    if plot_strain and len(data) > 15 :
        plt.plot(radius, radial_strain  , '--', label = r'$\epsilon_{rr}$')
        plt.plot(radius, toroidal_strain, '--', label = r'$\epsilon_{\theta\theta}$')
        plt.plot(radius, vertical_strain, '--', label = r'$\epsilon_{zz}$')
        plt.grid(True)
        plt.ylabel( r'$\epsilon$', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.legend( loc = 'best', fontsize = legend_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/strains.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()

    ## PLOT 4 : Displacement
    # ----------------------
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