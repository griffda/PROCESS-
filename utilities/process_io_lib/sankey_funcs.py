"""
Library of Sankey plotting routine

Author: Hanni Lux (Hanni.Lux@ukaea.uk)
        Matti Coleman (Matti.Coleman@ukaea.uk)

Updated 13/09/2019: Adam Brown (adam.brown@ukaea.uk) 
"""

import numpy as np
from numpy import sqrt
from matplotlib.sankey import Sankey
import matplotlib.pyplot as plt
from process_io_lib.mfile import MFile


def plot_sankey(mfilename='MFILE.DAT'): # Plots the power flow from PROCESS as a Sankey Diagram


    # ------------------------------- Pulling values from the MFILE -------------------------------

    m_file = MFile(mfilename)

    # Used in [PLASMA]
    powfmw  = m_file.data['powfmw' ].get_scan(-1) # Fusion power (MW)
    pinjmw  = m_file.data['pinjmw' ].get_scan(-1) # Total auxiliary injected power (MW)
    pohmmw  = m_file.data['pohmmw' ].get_scan(-1) # Ohmic heating power (MW)
    totalplasma = powfmw+pinjmw+pohmmw # Total Power in plasma (MW)
    pneutmw = m_file.data['pneutmw'].get_scan(-1) # Neutron fusion power (MW)
    palpmw  = m_file.data['palpmw' ].get_scan(-1) # Alpha power (MW)
    pchargemw  = m_file.data['pchargemw' ].get_scan(-1) # Non-alpha charged particle power (MW)

    # Used in [HCD]
    pinjht = m_file.data['pinjht'].get_scan(-1) # Power dissipated in HCD system (MW)

    #Used in [NEUTRONICS]
    emultmw = m_file.data['emultmw'].get_scan(-1) # Power due to energy multiplication in blanket and shield [MW]
    pnucblkt = m_file.data['pnucblkt'].get_scan(-1) # Nuclear heating in the blanket (MW)
    pnucdiv = m_file.data['pnucdiv'].get_scan(-1) # Nuclear heating in the divertor (MW)
    pnucfw = m_file.data['pnucfw'].get_scan(-1) # Nuclear heating in the first wall (MW)
    pnucshld = m_file.data['pnucshld'].get_scan(-1) # Nuclear heating in the shield (MW)
    ptfnuc = m_file.data['ptfnuc'].get_scan(-1) # Nuclear heating in the TF coil (MW)


    # The visual settings of the Sankey Plot
    plt.rcParams.update({'font.size': 9})
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1, xticks=[], yticks=[], frameon=False)
    sankey = Sankey(ax=ax, unit='MW', margin=0.0, format='%1.0f',scale = 1./(totalplasma))


    # ----------------------------------------- PLASMA - 0 ----------------------------------------

    FUSION = [powfmw, -pneutmw, -pchargemw, -palpmw]
    # Fusion, Injected, ohmic, -neutrons, -non-alpha charged particles, -alphas
    PLASMA = [powfmw, pinjmw, pohmmw, -pneutmw, -pchargemw, -palpmw]
    # Checking to see if the fusion components balance
    if sqrt(sum(FUSION)**2) > 2:
        print('FUSION power balance', powfmw, -pneutmw-palpmw-pchargemw)
        exit()
    # Checking to see if the input and output plasma power flows differ
    if sqrt(sum(PLASMA)**2) > 2:
        print('PLASMA power balance', powfmw+pinjmw+pohmmw, -pneutmw-palpmw-pchargemw)
    # exit()
    sankey.add(flows=PLASMA,
               # [right(in), down(in), down(in), right(out), up(out)]
               orientations=[0, -1, -1, 0, 0, 0],
               trunklength=0.8,
               pathlengths=[0.2, 0.25, 0.25, 0.0, 0.25, 0.25],
               labels=['Fusion Power','H&CD', 'Ohmic', 'Neutrons', 'Charged particles', 'Alphas'])


    # ------------------------------------------- HCD - 1 -----------------------------------------

    # HCD loss + injected, -injected, -HCD loss
    HCD = [pinjht+pinjmw, -pinjmw, -pinjht]
    assert(sum(HCD)**2 < 0.5)
    sankey.add(flows=HCD,
               orientations=[-1, 1, -1], # [down(in), up(out), down(out)]
               prior=0, # PLASMA
               connect=(1, 1), # None --> H&CD
               trunklength=0.5,
               pathlengths=[0.25, 0.25, 0.25],
               labels=['H&CD power', None, 'H&CD losses'])


    # -------------------- NEUTRONICS - 2 -------------------

    # Include nuclear heating in shield
    if pnucshld > 0.5 :
        NEUTRONS = [pneutmw, emultmw, -pnucblkt, -pnucdiv, -pnucfw, -pnucshld, -ptfnuc]
        assert (sum(NEUTRONS)**2 < 0.5), "Neutron sum < 0.5"
        sankey.add(flows=NEUTRONS,
                   orientations=[0, 1, 0, -1, -1, 1, 1],
                   prior=0, connect=(3, 0),
                   labels=[None,'Energy Multiplication',"Blanket", "Divertor", "First Wall", "Shield", "TF coils"])

    # Do not include nuclear heating in shield
    else:
        NEUTRONS = [pneutmw, emultmw, -pnucblkt, -pnucdiv, -pnucfw, -ptfnuc]
        assert (sum(NEUTRONS)**2 < 0.5), "Neutron sum < 0.5"
        sankey.add(flows=NEUTRONS,
                   orientations=[0, 1, 0, -1, -1, 1],
                   prior=0, connect=(2, 0),
                   labels=[None,'Energy Multiplication',"Blanket", "Divertor", "First Wall", "TFcoils"])



    #Radiation- 3
    pradmw = m_file.data['pradmw'].get_scan(-1)
    pdivt = m_file.data['pdivt'].get_scan(-1)
    praddiv = pradmw * m_file.data['fdiv'].get_scan(-1)
    pradhcd = pradmw * m_file.data['fhcd'].get_scan(-1)
    pradfw = pradmw -praddiv -pradhcd

    falpha = 1. #m_file.data['falpha'].get_scan(-1) #Fraction of alpha power dep. in plasma
    
    if pradhcd > 0.5 :
        RADIATION = [falpha*palpmw + pinjmw, -praddiv,-pradfw,-pradhcd, -pdivt]
        sankey.add(flows=RADIATION, orientations=[0,1,1, 1, 0],
                   prior=0, connect=(4, 0),
                   labels=[None,'Rad. Div', 'Rad.FW', 'Rad. HCD', "Charged Particles"])
    else :
        RADIATION = [palpmw*falpha + pinjmw, -praddiv,-pradfw, -pdivt]
        sankey.add(flows=RADIATION, orientations=[0,1,1, 0],
                   prior=0, connect=(4, 0),
                   labels=[None,'Rad. Div', 'Rad.FW', "Charged Particles"])


    if sqrt(sum(RADIATION)**2) > 2 :
        print('RADIATION power balance',palpmw + pinjmw, -pradmw -pdivt)
        
    #Blanket -4
    htpmw_blkt = m_file.data['htpmw_blkt'].get_scan(-1) #coolant pumping power
    pthermblkt = pnucblkt + htpmw_blkt
    BLANKET = [pnucblkt,htpmw_blkt, -pthermblkt]
    sankey.add(flows=BLANKET, orientations=[0, 1, 0],
               prior=2, connect=(2, 0),
               labels=[None,'Coolant Pumping',""])
    
    #Divertor
    pthermdiv = m_file.data['pthermdiv'].get_scan(-1)
    htpmw_div = m_file.data['htpmw_div'].get_scan(-1)
    DIVERTOR = [pdivt, pnucdiv, praddiv, htpmw_div, -pthermdiv]
    sankey.add(flows=DIVERTOR, orientations=[-1, -1,-1, 0,0],
               prior=2, connect=(3, 1),
               labels=[None, None,'Radiation','Coolant Pumping',""])


    #First Wall
    #losses from heating system to first wall
    nbshinemw = m_file.data['nbshinemw'].get_scan(-1)
    #forbitloss =  m_file.data['forbitloss'].get_scan(-1)
    pnbitot = m_file.data['pnbitot'].get_scan(-1)
    #porbitloss = forbitloss * (pnbitot - nbshinemw)

    #hcdlosses = nbshinemw+porbitloss
    #iprimshld = m_file.data['iprimshld'].get_scan(-1)
    #if iprimshld == 1:
    
    FW = []
    
    
    #Shield
    if pnucshld > 0.5 :
        htpmw_shld = m_file.data['htpmw_shld'].get_scan(-1) #coolant pumping power
        pthermshld = m_file.data['pthermshld'].get_scan(-1)
        SHIELD = [pnucshld, htpmw_shld, -pthermshld]
        sankey.add(flows=SHIELD, orientations=[-1, -1, 1],
                   prior=2, connect=(5, 0),
                   labels=[None,'Coolant Pumping',""])
        
    #Primary high grade heat
    secondary_cycle = int(m_file.data['secondary_cycle'].get_scan(-1))
    pthermmw = m_file.data['pthermmw'].get_scan(-1)
    pthermfw_blkt = m_file.data['pthermfw_blkt'].get_scan(-1)

    #iprimshld = m_file.data['iprimshld'].get_scan(-1)

    if secondary_cycle == 0:
        # FW+Blanket, Shield
        PRIMARY = [pthermfw_blkt, pthermmw-pthermfw_blkt, -pthermmw]
        #sankey.add(flows=BLANKET, orientations=[0,1 , 0],
               #prior=3, connect=(2, 0),
        #       labels=[None, None,"Primary High Grade Heat"])
    else:
        PRIMARY = [-pthermmw, pthermfw_blkt, pthermmw-pthermfw_blkt, pthermdiv]
        sankey.add(flows=BLANKET, orientations=[0, 1, 0],
               prior=3, connect=(2, 0),
               labels=[None,'FW+Blanket',"Shield", "Divertor"])

    #Low Grade heat
    #htpmw = m_file.data['htpmw'].get_scan(-1)
    psechtmw = m_file.data['psechtmw'].get_scan(-1)"""

    

    diagrams = sankey.finish()
    fig.tight_layout()


# --------------------------------------- Label Positioning ---------------------------------------

    # Munipulating the positioning of the branch labels
    # -ve to left and down; +ve to right and up
    # Using branch widths to adjust label positioning

    for d in diagrams:
        y = 0
        for t in d.texts:
            pos = tuple(np.ndarray.tolist(d.tips[y]))
            t.set_position(pos)
            if t == diagrams[0].texts[0]: # Fusion Power
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.2,pos[1]))
            if t == diagrams[0].texts[1]: # H&CD
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.5*((pinjmw)/totalplasma)-0.1,pos[1]))
            if t == diagrams[0].texts[2]: # Ohmic
                t.set_horizontalalignment('left')
                t.set_position((pos[0]+0.5*((pohmmw)/totalplasma)+0.1,pos[1]))
            if t == diagrams[0].texts[3]: # Neutrons
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.2,pos[1]))
            if t == diagrams[0].texts[4]: # He4+Aux
                t.set_horizontalalignment('center')
                t.set_position((pos[0],pos[1]+0.15))
            if t == diagrams[1].texts[0]: # H&CD power
                t.set_horizontalalignment('center')
                t.set_position((pos[0],pos[1]-0.2))
            if t == diagrams[1].texts[2]: # H&CD losses
                t.set_horizontalalignment('center')
                t.set_position((pos[0],pos[1]-0.1))
            y += 1



####################################################################################################



def plot_simplified_sankey(mfilename='MFILE.DAT'): # Plot simplified power flow Sankey Diagram


    # ------------------------------- Pulling values from the MFILE -------------------------------

    m_file = MFile(mfilename)

    # Used in [PLASMA]
    powfmw = m_file.data['powfmw'].get_scan(-1) # Fusion Power (MW)
    pinjmw = m_file.data['pinjmw'].get_scan(-1) # Total auxiliary injected Power (MW)
    pohmmw = m_file.data['pohmmw'].get_scan(-1) # Ohmic heating Power (MW)
    totalplasma = powfmw+pinjmw+pohmmw # Total Power in plasma (MW)

    # Used in [DEPOSITION]
    pradmw = m_file.data['pradmw'].get_scan(-1) # Total radiation Power (MW)
    fdiv = m_file.data['fdiv'].get_scan(-1) # Area fraction taken up by divertor
    praddiv = pradmw * fdiv # Radiation deposited on the divertor (MW)
    fhcd = m_file.data['fhcd'].get_scan(-1) # Area fraction covered by HCD and diagnostics
    pradhcd = pradmw * fhcd # Radiation deposited on HCD and diagnostics (MW)
    pradfw = pradmw-praddiv-pradhcd # Radiation deposited in the blanket (MW)
    pdivt = m_file.data['pdivt'].get_scan(-1) # power to conducted to the divertor region (MW)
    pnucdiv = m_file.data['pnucdiv'].get_scan(-1) # nuclear heating in the divertor (MW)
    pnucfw = m_file.data['pnucfw'].get_scan(-1) # nuclear heating in the first wall (MW)
    pnucblkt = m_file.data['pnucblkt'].get_scan(-1) # nuclear heating in the blanket (MW)
    pnucshld = m_file.data['pnucshld'].get_scan(-1) # nuclear heating in the shield (MW)
    emultmw = m_file.data['emultmw'].get_scan(-1) # Blanket energy multiplication (MW)
    palpmw = m_file.data['palpmw'].get_scan(-1) # Alpha power (MW)
    falpha = m_file.data['falpha'].get_scan(-1) # Fraction of alpha power deposited in plasma
    palpfwmw = palpmw*(1-falpha) # Alpha power hitting 1st wall (MW)
    itart = m_file.data['itart'].get_scan(-1) # switch for spherical tokamak (ST) models
    if itart == 0:
        # Power deposited on divertor (MW)
        totaldivetc = pdivt+pnucdiv+praddiv
        # Power deposited on Blanket (MW)
        totalblktetc = pnucfw+pnucblkt+pnucshld+pradfw+palpfwmw-emultmw
    elif itart == 1:
        # Power deposited on divertor (MW)
        totaldivetc = pdivt+pnucdiv+praddiv+pnucshld
        # Power deposited on Blanket (MW)
        totalblktetc = pnucfw+pnucblkt+pradfw+palpfwmw-emultmw

    # Used in [BLANKETSETC]
    pthermfw_blkt = m_file.data['pthermfw_blkt'].get_scan(-1) # Heat for electricity (MW)
    htpmw_fw_blkt = m_file.data['htpmw_fw_blkt'].get_scan(-1) # 1st wall & blanket pumping (MW)
    pthermmw_p = pthermfw_blkt-htpmw_fw_blkt # Heat - pumping power (MW)

    # Used in [PRIMARY]
    pgrossmw = m_file.data['pgrossmw'].get_scan(-1) # gross electric power (MW)

    # Used in [NET]
    pnetelmw = m_file.data['pnetelmw'].get_scan(-1) # net electric power (MW)
    precircmw = pgrossmw-pnetelmw # Recirculating power (MW)

    # Used in [RECIRC]
    crypmw = m_file.data['crypmw'].get_scan(-1) # cryogenic plant power (MW)
    fachtmw = m_file.data['fachtmw'].get_scan(-1) # facility heat removal (MW)
    tfacpd = m_file.data['tfacpd'].get_scan(-1) # total steady state TF coil AC power demand (MW)
    trithtmw = m_file.data['trithtmw'].get_scan(-1) # power required for tritium processing (MW)
    vachtmw = m_file.data['vachtmw'].get_scan(-1) # vacuum pump power (MW)
    pfwpmw = m_file.data['pfwpmw'].get_scan(-1) # Total mean wall plug power for PFC & CS (MW)
    # Energy requires for rest of power plant (MW)
    pcoresystems = crypmw + fachtmw + tfacpd + trithtmw + vachtmw + pfwpmw
    pinjwp = m_file.data['pinjwp'].get_scan(-1) # injector wall plug power (MW)
    htpmw = m_file.data['htpmw'].get_scan(-1) # heat transport system electrical pump power (MW)
    if itart == 1 : # If Spherical Tokamak add centre post coolant pumping to core systems
        ppumpmw = m_file.data['ppump'].get_scan(-1)/1e6 # Set pumping power to 1e-6
        pcoresystems = pcoresystems + ppumpmw     
   
    
    # Initialising x and y variables for adjusting 'Plasma Heating' branch tip location
    x_adj, y_adj = 0,0

    # Loop 1 to get 'Plasma Heating' branch tip coords; loop 2 to match 'PLASMA' branch
    for _ in range(2):


        # ------------------------------------ Visual Settings ------------------------------------

        plt.rcParams.update({'font.size': 9}) # Setting font size to 9
        fig = plt.figure()
        ax = fig.add_subplot(1, 1, 1, xticks=[], yticks=[], frameon=False)
        sankey = Sankey(ax=ax, unit='MW', margin=0.0, format='%1.0f',scale = 1./(totalplasma))


        # --------------------------------------- PLASMA - 0 --------------------------------------

        # Fusion power, Injected power + ohmic power, - total plasma power
        PLASMA = [powfmw, pinjmw+pohmmw, -totalplasma]
        sankey.add(flows = PLASMA,
                   orientations = [0, -1, 0], # [right(in), down(in), right(out)]
                   pathlengths = [0.5, 0.8+0.5*y_adj, -0.1+0.5*x_adj], # 'Plasma Heating' adjust
                   labels = ['Fusion Power', None, 'Plasma'])


        # --------------------------------- ENERGY DEPOSITION - 1 ---------------------------------
        
        # Plasma power, - divertor deposited power, - blanket deposited power
        DEPOSITION = [totalplasma, -totaldivetc, -totalblktetc]
        # Check if difference >2 between plasma and divertor + blanket
        if _ == 1 and sqrt(sum(DEPOSITION)**2) > 2:
            print("\ncomponents power balance difference =", totalplasma-totaldivetc-totalblktetc)
        sankey.add(flows = DEPOSITION,
                   orientations = [0, 1, 0], # [right(in), up(in), right(out)]
                   prior = 0, # PLASMA
                   connect = (2, 0), # Plasma --> None
                   pathlengths = [0.2, 0.25, 0.2+0.5*x_adj], # 'Plasma Heating' adjust
                   labels = [None, 'Non-Electricity Producing Comp.', 'Blanket/etc.'])


        # -------------------------------------- BLANKET - 2 --------------------------------------

        # Blanket deposited power, blanket energy multiplication, - primary heat
        BLANKETSETC = [totalblktetc, emultmw, -pthermmw_p]
        #Check if difference >2 between primary heat and blanket + blanket multiplication
        if _ == 1 and sqrt(sum(BLANKETSETC)**2) > 2:
            print("blankets etc. power balance", totalblktetc+emultmw, -pthermmw_p)
        sankey.add(flows=BLANKETSETC,
                   orientations=[0, -1, 0], # [right(in), down(in), right(out)]
                   prior=1, # DEPOSITION
                   connect=(2, 0), # Blanket/etc. --> None
                   pathlengths = [0.5, 0.25, 0.0],
                   labels=[None,'Energy Mult.','Primary Heat'])


        # ------------------------------------- HEAT LOSS - 3 -------------------------------------

        # Primary heat, -Gross electric power, -difference (loss)
        PRIMARY = [pthermmw_p, -pgrossmw, -pthermmw_p+pgrossmw]
        sankey.add(flows=PRIMARY,
                   orientations=[0, -1, 0], # [right(in), down(out), right(out)]
                   prior=2, # BLANKETSETC
                   connect=(2, 0), # Primary Heat --> None
                   pathlengths = [0.2, 0.7, 0.4],
                   labels=[None,'Gross electric','Losses'])
        

        # ------------------------------------ ELECTRICITY - 4 ------------------------------------

        # If net electric is +ve or -ve changes the flow organisation
        if pnetelmw >= 0: # net electric is +ve
            # Gross electric power, -net electric power, -recirculated power
            NET = [pgrossmw, -pnetelmw, -precircmw]
            sankey.add(flows=NET,
                       orientations=[0, 0, -1], # [down(in), down(out), left(out)]
                       prior=3, # PRIMARY
                       connect=(1, 0), # Gross electric --> None
                       pathlengths = [0.1, 0.25, 1.5],
                       labels=[None,'Net elec.','Recirc. Power'])
        elif pnetelmw < 0: # net electric is -ve
            # Gross electric power, -net electric power, -recirculated power
            NET = [-pnetelmw, pgrossmw, -precircmw]
            sankey.add(flows=NET,
                       orientations=[0, -1, 0], # [left(in), down(in), left(out)]
                       prior=3, # PRIMARY
                       connect=(1, 1), # Gross electric --> None
                       pathlengths = [0.25, 1.0, 0.5],
                       labels=['Net elec.',None,'Recirc. Power'])


        # -------------------------------- RECIRCULATING POWER - 5 --------------------------------

        # Recirculated power, -Core Systems, -Heating System
        RECIRC = [precircmw, -pcoresystems-htpmw, -pinjwp]
        # Check if difference >2 between recirculated power and the output sum
        if sum(RECIRC)**2 > 2:
            print('Recirc. Power Balance', precircmw, -pcoresystems-pinjwp-htpmw)
        sankey.add(flows=RECIRC,
                   orientations=[0, 1, 0], # [left(in), down(out), left(out)]
                   prior=4, # NET
                   connect=(2, 0), # Recirc. Power --> None
                   pathlengths = [0.1, 0.25, 0.8],
                   labels=[None,'Core Systems', 'Heating System'])


        # --------------------------------------- LOSSES - 6 --------------------------------------

        # HCD: Heating system, -Plasma heating, -losses
        HCD = [pinjwp,-pinjmw,-pinjwp+pinjmw]
        sankey.add(flows=HCD,
                   orientations=[0, -1, 0], # [left(in), up(out), left(out)]
                   prior=5, # RECIRC
                   connect=(2, 0), # Heating System --> None
                   pathlengths = [0.5,0.8+0.5*y_adj,0.4], # 'Plasma Heating' adjust
                   labels=[None, 'Plasma Heating', 'Losses'])
        

        # Colelcting Sankey diagram and applying a condensed layout
        diagrams = sankey.finish()
        fig.tight_layout()

        # Difference in branch tip locations for 'Plasma Heating'
        x_adj, y_adj = diagrams[0].tips[1] - diagrams[6].tips[1] 


# --------------------------------------- Label Positioning ---------------------------------------

    # Munipulating the positioning of the branch labels
    # -ve to left and down; +ve to right and up
    # Using branch widths to adjust label positioning
    for d in diagrams:
        y = 0
        for t in d.texts:
            pos = tuple(np.ndarray.tolist(d.tips[y]))
            t.set_position(pos)
            if t == diagrams[0].texts[0]: # Fusion Power
                t.set_horizontalalignment('left')
                t.set_position((pos[0]-0.35,pos[1]+0.5*(powfmw/totalplasma)+0.2))
            if t == diagrams[0].texts[2]: # Plasma
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.25,pos[1]))
            if t == diagrams[1].texts[1]: # Non-Electricity Producing Comp.
                t.set_position((pos[0]+0.05,pos[1]+0.15))
            if t == diagrams[1].texts[2]: # Blanket/etc.
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.2,pos[1]))
            if t == diagrams[2].texts[1]: # Energy Mult.
                t.set_position((pos[0],pos[1]-0.3))
            if t == diagrams[2].texts[2]: # Primary Heat
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.25,pos[1]))
            if t == diagrams[3].texts[1]: # Gross Electric
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.5*((pgrossmw)/totalplasma)-0.1,pos[1]+0.1))
            if t == diagrams[3].texts[2]: # Losses
                t.set_horizontalalignment('right')
                t.set_position((pos[0]-0.2,pos[1]))
            if pnetelmw >= 1:
                if t == diagrams[4].texts[1]: # Net electric
                    t.set_horizontalalignment('center')
                    t.set_position((pos[0],pos[1]-0.2))
            elif pnetelmw < 1:
                if t == diagrams[4].texts[0]: # Net electric
                    t.set_horizontalalignment('left')
                    t.set_position((pos[0]+0.2,pos[1]))
            if t == diagrams[4].texts[2]: # Recirc. Power
                if pnetelmw >= 1:
                    t.set_position((pos[0]+0.15,pos[1]+0.5*(precircmw/totalplasma)+0.2))
                elif pnetelmw < 1:
                    t.set_horizontalalignment('left')
                    t.set_position((pos[0]+0.2,pos[1]))
            if t == diagrams[5].texts[1]: # Core Systems
                t.set_position((pos[0],pos[1]-0.2))
            if t == diagrams[5].texts[2]: # Heating System
                if pnetelmw >= 1:
                    t.set_position((pos[0]+0.15,pos[1]+0.5*((pinjwp)/totalplasma)+0.2))
                if pnetelmw < 1:
                    t.set_position((pos[0]+0.15,pos[1]+0.5*((pinjwp)/totalplasma)+0.2))
            if t == diagrams[6].texts[1]: # Plasma Heating
                t.set_horizontalalignment('left')
                t.set_position((pos[0]+0.5*((pinjmw)/totalplasma)+0.1,pos[1]-0.05))
            if t == diagrams[6].texts[2]: # Losses
                t.set_horizontalalignment('left')
                t.set_position((pos[0]+0.15,pos[1]-0.5*((pinjwp-pinjmw)/totalplasma)-0.2))
            y += 1