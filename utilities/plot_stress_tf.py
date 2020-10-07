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
    tresca_smeared_stress   = list()
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
    
    wp_radial_stress   = list()
    wp_toroidal_stress = list()
    wp_vertical_stress = list()
    

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
   
    # Getting the data to be plotted
    n_radial_array_layer = int(data[0][0])
    n_points             = int(len(data[2]))
    n_layers             = int(n_points/n_radial_array_layer)
    for ii in range(n_layers):
        radius                 .append(list())
        radial_stress          .append(list())
        toroidal_stress        .append(list())
        vertical_stress        .append(list())
        radial_smeared_stress  .append(list())
        toroidal_smeared_stress.append(list())
        vertical_smeared_stress.append(list())
        vm_stress              .append(list())
        tresca_stress          .append(list())
        cea_tresca_stress      .append(list())
        radial_displacement    .append(list())

        for jj in range(n_radial_array_layer):
            radius                 [ii].append( data[2][ii*n_radial_array_layer+jj]  )
            radial_stress          [ii].append( data[3][ii*n_radial_array_layer+jj]  )
            toroidal_stress        [ii].append( data[4][ii*n_radial_array_layer+jj]  )   
            if len(data[5]) == 1:
                vertical_stress[ii].append( data[5][0]  )
            else :
                vertical_stress[ii].append( data[5][ii*n_radial_array_layer+jj]  )
            radial_smeared_stress  [ii].append( data[6][ii*n_radial_array_layer+jj]  )
            toroidal_smeared_stress[ii].append( data[7][ii*n_radial_array_layer+jj]  )
            vertical_smeared_stress[ii].append( data[8][ii*n_radial_array_layer+jj]  )
            vm_stress              [ii].append( data[9][ii*n_radial_array_layer+jj]  )
            tresca_stress          [ii].append( data[10][ii*n_radial_array_layer+jj] )
            cea_tresca_stress      [ii].append( data[11][ii*n_radial_array_layer+jj] )
            radial_displacement    [ii].append( data[14][ii*n_radial_array_layer+jj] )

    # TRESCA smeared stress [MPa]
    for ii in range(n_layers):
        tresca_smeared_stress.append(list())

        for jj in range(n_radial_array_layer):
            tresca_smeared_stress[ii].append(max(abs(radial_smeared_stress[ii][jj]), abs(toroidal_smeared_stress[ii][jj])) + vertical_smeared_stress[ii][jj]) 

    # Strains
    if len(data) > 16 :
        for ii in range(n_layers):
            radial_strain.append(list())
            toroidal_strain.append(list())

            for jj in range(n_radial_array_layer):
                radial_strain[ii]  .append(data[17][ii*n_radial_array_layer+jj]) 
                toroidal_strain[ii].append(data[18][ii*n_radial_array_layer+jj])
        
        if len(data) >= 21 :
            for jj in range(n_radial_array_layer):
                wp_vertical_stress.append(data[20][jj])

        for ii in range(n_layers):
            vertical_strain.append(list())
            for jj in range(n_radial_array_layer):
                vertical_strain[ii].append(data[19][ii*n_radial_array_layer+jj])
    else :
        for jj in range(n_radial_array_layer):
            wp_vertical_stress.append(data[15][jj])

    # Terminal output 
    # ---------------
    if term_output :
        ii_ins =  0
        ii_mids = int(0.5*float(n_radial_array_layer))
        ii_outs =  n_radial_array_layer - 1
    
    
        print("")
        print("")
        print("Layer stress details")
        print("____________________")
    
    
        for ii in range(n_layers) :
            print("Layer {}".format( ii+1 ))
            print("------------------------------")
            print("steel radial   stress in the inner/middle/out point: {}/{}/{} MPa".format(radial_stress[ii][ii_ins], radial_stress[ii][ii_mids], radial_stress[ii][ii_outs]) )
            print("steel toroidal stress in the inner/middle/out point: {}/{}/{} MPa".format(toroidal_stress[ii][ii_ins], toroidal_stress[ii][ii_mids], toroidal_stress[ii][ii_outs]) )
            print("steel vertical stress in the inner/middle/out point: {}/{}/{} MPa".format(vertical_stress[ii][ii_ins], vertical_stress[ii][ii_mids], vertical_stress[ii][ii_outs]) )
            print("steel TRESCA   stress in the inner/middle/out point: {}/{}/{} MPa".format(tresca_stress[ii][ii_ins], tresca_stress[ii][ii_mids], tresca_stress[ii][ii_outs]) )
            print("")
            print("smeared radial   stress in the inner/middle/out point : {}/{}/{} MPa".format(radial_smeared_stress[ii][ii_ins]  , radial_smeared_stress[ii][ii_mids]  , radial_smeared_stress[ii][ii_outs]) )
            print("smeared toroidal stress in the inner/middle/out point : {}/{}/{} MPa".format(toroidal_smeared_stress[ii][ii_ins], toroidal_smeared_stress[ii][ii_mids], toroidal_smeared_stress[ii][ii_outs]) )
            print("smeared vertical stress in the inner/middle/out point : {}/{}/{} MPa".format(vertical_smeared_stress[ii][ii_ins], vertical_smeared_stress[ii][ii_mids], vertical_smeared_stress[ii][ii_outs]) )
            print("smeared TRESCA   stress in the inner/middle/out point : {}/{}/{} MPa".format(tresca_smeared_stress[ii][ii_ins]  , tresca_smeared_stress[ii][ii_mids]  , tresca_smeared_stress[ii][ii_outs]) )
            print("")
            print("radial   strain in the inner/middle/out point : {}/{}/{}".format(radial_strain[ii][ii_ins]  , radial_strain[ii][ii_mids]  , radial_strain[ii][ii_outs]) )
            print("toroidal strain in the inner/middle/out point : {}/{}/{}".format(toroidal_strain[ii][ii_ins], toroidal_strain[ii][ii_mids], toroidal_strain[ii][ii_outs]) )
            print("vertical strain : {}".format(vertical_strain[ii][0]) )
            print("")

        if not len(wp_vertical_stress) == 0 : 
            print("smeared WP vertical stress in the inner/middle/out point : {}/{}/{} MPa".format(wp_vertical_stress[0]  , wp_vertical_stress[ii_mids], wp_vertical_stress[ii_outs]) )
        print("")

    outdir = str("SIG_TF_plots")
    if not os.path.isdir(outdir) :
        os.mkdir(outdir)

    axis_tick_size = 16
    legend_size    = 12

    ## PLOT 1 : Stress summary
    # ------------------------
    if plot_sig :
        plt.plot(radius[0], radial_stress[0]  , '--', color = 'dodgerblue'    , label = r'$\sigma_{rr}$')
        plt.plot(radius[0], toroidal_stress[0], '--', color = 'orange'        , label = r'$\sigma_{\theta\theta}$')
        plt.plot(radius[0], vertical_stress[0], '--', color = 'mediumseagreen', label = r'$\sigma_{zz}$')
        plt.plot(radius[0], tresca_stress[0]  , '-' , color = 'crimson'       , label = r'$\sigma_{TRESCA}$')
        plt.plot(radius[0], vm_stress[0]      , '-' , color = 'blueviolet'    , label = r'$\sigma_{Von\ mises}$')
        for ii in range(1, n_layers):
            plt.plot(radius[ii], radial_stress[ii]  , '--', color = 'dodgerblue'     )
            plt.plot(radius[ii], toroidal_stress[ii], '--', color = 'orange'         )
            plt.plot(radius[ii], vertical_stress[ii], '--', color = 'mediumseagreen' )
            plt.plot(radius[ii], tresca_stress[ii]  , '-' , color = 'crimson'        )
            plt.plot(radius[ii], vm_stress[ii]      , '-' , color = 'blueviolet'     )
        plt.grid(True)
        plt.ylabel( r'$\sigma$ [$MPa$]', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.legend( loc = 'best', fontsize = legend_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/structure_stress.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()

    ## PLOT 2 : Smeared stress summary
    # ------------------------
    if plot_sm_sig :
        plt.plot(radius[0], radial_smeared_stress  [0], '--', color = 'dodgerblue'    , label = r'$\sigma_{rr}^\mathrm{smeared}$')
        plt.plot(radius[0], toroidal_smeared_stress[0], '--', color = 'orange'        , label = r'$\sigma_{\theta\theta}^\mathrm{smeared}$')
        plt.plot(radius[0], vertical_smeared_stress[0], '--', color = 'mediumseagreen', label = r'$\sigma_{zz}^\mathrm{smeared}$')
        plt.plot(radius[0], tresca_smeared_stress  [0], '-' , color = 'crimson'       , label = r'$\sigma_{TRESCA}^\mathrm{smeared}$')
        for ii in range(1, n_layers):
            plt.plot(radius[ii], radial_smeared_stress  [ii], '--', color = 'dodgerblue'     )
            plt.plot(radius[ii], toroidal_smeared_stress[ii], '--', color = 'orange'         )
            plt.plot(radius[ii], vertical_smeared_stress[ii], '--', color = 'mediumseagreen' )
            plt.plot(radius[ii], tresca_smeared_stress  [ii], '-' , color = 'crimson'        )
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
        plt.plot(radius[0], radial_strain  [0], '--', color = 'dodgerblue'    , label = r'$\epsilon_{rr}$')
        plt.plot(radius[0], toroidal_strain[0], '--', color = 'orange'        , label = r'$\epsilon_{\theta\theta}$')
        plt.plot(radius[0], vertical_strain[0], '--', color = 'mediumseagreen', label = r'$\epsilon_{zz}$')
        for ii in range(1, n_layers):
            plt.plot(radius[ii], radial_strain  [ii], '--', color = 'dodgerblue' )
            plt.plot(radius[ii], toroidal_strain[ii], '--', color = 'orange' )
            plt.plot(radius[ii], vertical_strain[ii], '--', color = 'mediumseagreen' )
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
        plt.plot(radius[0], radial_displacement[0], color = 'dodgerblue'  )
        for ii in range(1, n_layers):
            plt.plot(radius[ii], radial_displacement[ii], color = 'dodgerblue'  )
        plt.grid(True)
        plt.ylabel( r'$u_{r}$ [mm]', fontsize = axis_font_size )
        plt.xlabel( r'$R$ [$m$]', fontsize = axis_font_size )
        plt.xticks( size = axis_tick_size )
        plt.yticks( size = axis_tick_size )
        plt.tight_layout()
        plt.savefig( '{}/displacement.{}'.format(outdir, save_format) )
        plt.clf()
        plt.cla()