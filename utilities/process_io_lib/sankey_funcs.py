"""
Library of Sankey plotting routine

Author: Hanni Lux (Hanni.Lux@ukaea.uk)
        Matti Coleman (Matti.Coleman@ukaea.uk)
"""

from numpy import sqrt
from matplotlib.sankey import Sankey
from matplotlib.pyplot import figure
from process_io_lib.mfile import MFile

def plot_sankey(mfilename='MFILE.DAT'):

    """ Plots the power flow from PROCESS as a Sankey Diagram
        for ipowerflow=1 comprehensive 2014 model """

    print('WARNING: This option is not yet completely implemented!',
          'Please use the -s simplified option instead!')

    m_file = MFile(mfilename)

    powfmw = m_file.data['powfmw'].get_scan(-1)
    pinjmw = m_file.data['pinjmw'].get_scan(-1)
    pohmmw = m_file.data['pohmmw'].get_scan(-1)
    
    fig = figure()
    ax = fig.add_subplot(1, 1, 1, xticks=[], yticks=[])
    sankey = Sankey(ax=ax, unit='MW', format='%1.0f',scale = 1./(powfmw+pinjmw+pohmmw))

    #PLASMA - 0
    pneutmw = m_file.data['pneutmw'].get_scan(-1)
    palpmw = m_file.data['palpmw'].get_scan(-1)
    
    PLASMA = [powfmw, pinjmw, pohmmw, -pneutmw, -palpmw-pinjmw]
    if sqrt(sum(PLASMA)**2) > 2:
        print('PLASMA power balance', powfmw+pinjmw+pohmmw, -pneutmw-palpmw-pinjmw)
        exit()
    sankey.add(flows=PLASMA, orientations=[0, -1, 0, 0, -1],
               labels=['Fusion Power','H&CD', 'Ohmic', 'Neutrons','He4+Aux'])

    #HCD -1
    pinjht = m_file.data['pinjht'].get_scan(-1)
    
    HCD = [pinjht+pinjmw, -pinjmw,-pinjht]
    assert(sum(HCD)**2 < 0.5)
    sankey.add(flows=HCD, orientations=[-1, 1, -1],
               prior=0, connect=(1, 1),
               labels=['H&CD power', None,"H&CD losses"])

    
    #Neutrons -2
    emultmw = m_file.data['emultmw'].get_scan(-1)
    pnucblkt = m_file.data['pnucblkt'].get_scan(-1)
    pnucdiv = m_file.data['pnucdiv'].get_scan(-1)
    pnucfw = m_file.data['pnucfw'].get_scan(-1)
    pnucshld = m_file.data['pnucshld'].get_scan(-1)
    ptfnuc = m_file.data['ptfnuc'].get_scan(-1)

    if pnucshld > 0.5 :
        NEUTRONS = [pneutmw, emultmw,-pnucblkt, -pnucdiv, -pnucfw, -pnucshld, -ptfnuc]
        assert(sum(NEUTRONS)**2 < 0.5)
        sankey.add(flows=NEUTRONS, orientations=[0, 1, 0, -1, -1, 1, 1],
                   prior=0, connect=(3, 0),
                   labels=[None,'Energy Multiplication',"Blanket", "Divertor",
                           "First Wall", "Shield", "TF coils"])
        
    else:
        NEUTRONS = [pneutmw,emultmw,-pnucblkt, -pnucdiv, -pnucfw, -ptfnuc]
        assert(sum(NEUTRONS)**2 < 0.5)
        sankey.add(flows=NEUTRONS, orientations=[0, 1, 0, -1, -1, 1],
                   prior=0, connect=(2, 0),
                   labels=[None,'Energy Multiplication',"Blanket", "Divertor",
                           "First Wall", "TFcoils"])

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
    psechtmw = m_file.data['psechtmw'].get_scan(-1)

    

    diagrams = sankey.finish()
    fig.tight_layout()


    
########################################################

def plot_simplified_sankey(mfilename='MFILE.DAT'):

    """ Plots the power flow from PROCESS as a Sankey Diagram
        for ipowerflow=1 comprehensive 2014 model, in a simplified way """

    m_file = MFile(mfilename)

    powfmw = m_file.data['powfmw'].get_scan(-1)
    pinjmw = m_file.data['pinjmw'].get_scan(-1)
    pohmmw = m_file.data['pohmmw'].get_scan(-1)
    
    fig = figure()
    ax = fig.add_subplot(1, 1, 1, xticks=[], yticks=[],frameon=False)
    sankey = Sankey(ax=ax, unit='MW', format='%1.0f',scale = 1./(powfmw+pinjmw+pohmmw))

    #PLASMA
    totalplasma = powfmw+pinjmw+pohmmw
    
    PLASMA = [powfmw, pinjmw+pohmmw,-totalplasma  ]
    sankey.add(flows=PLASMA, orientations=[0, -1, 0],
               labels=['Fusion Power','Plasma Heating','Plasma'])

    #Components
    emultmw = m_file.data['emultmw'].get_scan(-1)
    pnucblkt = m_file.data['pnucblkt'].get_scan(-1)
    pnucdiv = m_file.data['pnucdiv'].get_scan(-1)
    pnucfw = m_file.data['pnucfw'].get_scan(-1)
    pnucshld = m_file.data['pnucshld'].get_scan(-1)
    ptfnuc = m_file.data['ptfnuc'].get_scan(-1)
    pdivt = m_file.data['pdivt'].get_scan(-1)
    pradmw = m_file.data['pradmw'].get_scan(-1)
    praddiv = pradmw * m_file.data['fdiv'].get_scan(-1)
    pradhcd = pradmw * m_file.data['fhcd'].get_scan(-1)
    pradfw = pradmw -praddiv -pradhcd
    
    totalblktetc = pnucfw+pnucblkt+pnucshld+pradfw-emultmw
    totaldivetc = pdivt+pnucdiv+praddiv+ptfnuc+pradhcd
    
    COMPONENTS = [totalplasma,-totaldivetc, -totalblktetc ]
    if sqrt(sum(COMPONENTS)**2) > 2:
        print("components power balance", totalplasma,-totaldivetc-totalblktetc)
    sankey.add(flows=COMPONENTS, orientations=[0, 1, 0],
               prior=0, connect=(2, 0),
               labels=[None,'Non-Electricity Producing Comp.','Blanket/etc.'])


    #Blankets etc.
    #Pumping power has been neglected
    pthermmw = m_file.data['pthermmw'].get_scan(-1)
    
    BLANKETSETC = [totalblktetc, emultmw, -pthermmw]
    if sqrt(sum(BLANKETSETC)**2) > 2:
        print("blankets etc. power balance", totalblktetc+emultmw, -pthermmw)
    sankey.add(flows=BLANKETSETC, orientations=[0, -1, -1],
               prior=1, connect=(2, 0),
               labels=[None,'Energy Mult.','Primary Heat'])


    pgrossmw = m_file.data['pgrossmw'].get_scan(-1)
    PRIMARY = [pthermmw, -pgrossmw, -pthermmw+pgrossmw]
    sankey.add(flows=PRIMARY, orientations=[0, -1, 0],
               prior=2, connect=(2, 0),
               labels=[None,'Gross electric','Losses'])
    

    pnetelmw = m_file.data['pnetelmw'].get_scan(-1)
    precircmw = pgrossmw-pnetelmw
    NET = [pgrossmw,-pnetelmw,-precircmw]
    sankey.add(flows=NET, orientations=[0, 1, 0],
               prior=3, connect=(1, 0),
               labels=[None,'Net electric','Recirc. Power'])

    crypmw = m_file.data['crypmw'].get_scan(-1)
    fachtmw = m_file.data['fachtmw'].get_scan(-1)
    itart = m_file.data['itart'].get_scan(-1)
    if itart == 1 :
        ppumpmw = m_file.data['ppump'].get_scan(-1)/1e6 #for STs only
    tfacpd = m_file.data['tfacpd'].get_scan(-1)
    trithtmw = m_file.data['trithtmw'].get_scan(-1)
    vachtmw = m_file.data['vachtmw'].get_scan(-1)
    pfwp = m_file.data['pfwp'].get_scan(-1)
    pcoresystems = crypmw + fachtmw + tfacpd + trithtmw + vachtmw + pfwp
    if itart == 1 :
        pcoresystems = pcoresystems + ppumpmw
    pinjwp = m_file.data['pinjwp'].get_scan(-1)
    htpmw = m_file.data['htpmw'].get_scan(-1)
    RECIRC = [precircmw, -pcoresystems-htpmw, -pinjwp]
    if sum(RECIRC)**2 > 2:
        print('Recirc. Power Balance', precircmw, -pcoresystems-pinjwp-htpmw)
        
    sankey.add(flows=RECIRC, orientations=[0, 1, 0],
               prior=4, connect=(2, 0),
               labels=[None,'Core Systems', 'Heating System'])

    HCD = [pinjwp,-pinjmw,-pinjwp+pinjmw]
    sankey.add(flows=HCD, orientations=[0, -1, 0],
               prior=5, connect=(2, 0),
               labels=[None,None, 'Losses'],
               #This is a fudge only working for STEP,
               #as Python does not have second connections!
               #Needs a more generic version.
               pathlengths=0.97,trunklength=0.98)
    
    
    diagrams = sankey.finish()
    fig.tight_layout()
