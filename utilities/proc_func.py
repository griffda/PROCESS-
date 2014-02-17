import sys
from pylab import *
#import getopt

def usage():
    print ''
    print 'Usage:'
    print ' plot_proc [-g, -h, filename, -o outfile]'
    print ''
    print ' Plots PROCESS radial build and summary of parameters.'
    print ' Ideally should be run in the folder containing OUT.DAT.'
    print '  -g, --graphics         plots to a file [default: plot to screen]'
    print '  -h, --help             prints this message and exits'
    print '  filename               name of .DAT file [default=OUT.DAT]'
    print '  -o, --output outfile   specifies output filename [default=inputfile.eps]'
    print ''
    print '  By default the -g option prints to inputfile.eps, but providing an output filename'
    print '  using -o outfile.png, for example, will create a file of the appropriate type.'
    print '  If using -o, -g is unnecessary.'
    print ''
    print 'Examples:'
    print ' plot_proc -g                          : reads OUT.DAT, creates OUT.DAT.eps'
    print ' plot_proc demo1.OUT.DAT -o demo1.png  : reads demo1.OUT.DAT, creates PNG file demo1.png'
    print ' plot_proc demo1.OUT.DAT               : reads demo1.OUT.DAT, plots to screen'
    print ''
    print ' Contact richard.kemp@ccfe.ac.uk with comments or complaints.'
    print ''

def find_val_one(f, name):
    """Finds an output value in PROCESS OUT.DAT file, standard format."""
    ans = -1
    f.seek(0)
    target = '('+name+')'
    for line in f:
        if target in line:
	    ans = float(line[58:69])
    return ans

def find_val_two(f, name):
    """Finds an input value in PROCESS OUT.DAT file."""
    ans = -1
    f.seek(0)
    for line in f:
        if name in line:
	    line2 = line.split('=')
	    ans = float(line2[1])
    return ans

def find_val_three(f, target, posn, leng):
    """Reads arbitrary value from line in OUT.DAT containing string 'name'."""
    ans = None
    f.seek(0)
    for line in f:
        if target in line:
	    ans = line[posn:posn+leng].rstrip().lstrip()
    return ans

def plot_plas_arcs(r0, a, delta, kappa, snull):
    """Plots the plasma boundary arcs."""    
    x1 = (2.*r0*(1.+delta) - a*(delta**2 + kappa**2 -1.))/(2.*(1.+delta))
    x2 = (2.*r0*(delta-1.) - a*(delta**2 + kappa**2 -1.))/(2.*(delta-1.))
    r1 = 0.5 * sqrt((a**2 * ((delta+1.)**2 + kappa**2)**2)/((delta+1.)**2))
    r2 = 0.5 * sqrt((a**2 * ((delta-1.)**2 + kappa**2)**2)/((delta-1.)**2))
    print 'plasma arcs: x1, r1, x2, r2'
    print x1, r1, x2, r2
    theta1 = arcsin((kappa*a)/r1)
    theta2 = arcsin((kappa*a)/r2)
    inang = 1./r1
    outang = 1.5/r2
    if snull == 0:
        angs1 = np.linspace(-(inang+theta1)+np.pi, (inang+theta1)+np.pi, 256, endpoint=True)
        angs2 = np.linspace(-(outang+theta2), (outang+theta2), 256, endpoint=True)
    elif snull < 0:
        angs1 = np.linspace(-(inang+theta1)+np.pi, theta1+np.pi, 256, endpoint=True)
        angs2 = np.linspace(-theta2, (outang+theta2), 256, endpoint=True)
    else:
        angs1 = np.linspace(-theta1+np.pi, (inang+theta1)+np.pi, 256, endpoint=True)
        angs2 = np.linspace(-(outang+theta2), theta2, 256, endpoint=True)
    xs1 = -(r1*np.cos(angs1) - x1)
    ys1 = r1*np.sin(angs1)
    xs2 = -(r2*np.cos(angs2) - x2)
    ys2 = r2*np.sin(angs2)
    plot(xs1, ys1, color='black'), plot(xs2, ys2, color='black')

def plot_coil(spec, name):
    "Plots a PF coil."""
    rs = [spec[0]-0.5*spec[2], spec[0]-0.5*spec[2], spec[0]+0.5*spec[2], spec[0]+0.5*spec[2], spec[0]-0.5*spec[2]]
    zs = [spec[1]-0.5*spec[3], spec[1]+0.5*spec[3], spec[1]+0.5*spec[3], spec[1]-0.5*spec[3], spec[1]-0.5*spec[3]]
    plot(rs, zs, color='black')
    text(spec[0], spec[1], name, ha='center', va='center', fontsize='smaller')

def get_build(f):
    """Gets the radial and vertical build information."""
    snull = find_val_two(f, 'SNULL')
    if snull == 1:
        snull = 1
    else:
        snull = 0
    f.seek(0)
    while 1:
        line = f.readline()
        if not line:
            break
        if '* Radial Build *' in line:
            posn = f.tell()
    f.seek(posn)
    for i in range(2): str = f.readline()
    dat = []
    otherdat = []
    for i in range(21):
        line = f.readline()
        dat.append(float(line[40:54]))
        otherdat.append(float(line[55:69]))
    for i in range(5): str = f.readline()
    if snull:
        for i in range(16):
            line = f.readline()
            dat.append(float(line[40:54]))
            otherdat.append(float(line[55:69]))
    else:
        for i in range(8):
            line = f.readline()
            dat.append(float(line[40:54]))
            otherdat.append(float(line[55:69]))
    otherdat.append(snull)
    if snull: dat.append((otherdat[21] + otherdat[36])/2.)
    else: dat.append(0.)
    return dat, otherdat

def get_coils(f):
    """Gets the PF coil information"""
    f.seek(0)
    while 1:
        line = f.readline()
        if not line:
            break
        if 'Geometry of PF coils, OH coil' in line:
            posn = f.tell()
    f.seek(posn)
    for i in range(3): str = f.readline()
    lcount = 0
    line = f.readline()
    while len(line) > 5:
        lcount = lcount + 1
        line = f.readline()
    f.seek(posn)
    for i in range(3): str = f.readline()
    dats = []
    coilnames = []
    for i in range(lcount):
        line = f.readline()
        coilnames.append(line[0:10].replace(' ','')[-1])
	dats.append(line[10:].split())
#    print coilnames
    if coilnames[-2] == "H":
        coilnames[-2] = "OH"
    dat = dats
    for i in range(len(dats)):
        for j in range(len(dats[1])):
	    dat[i][j] = float(dats[i][j])
    return dat, coilnames

def get_tf(f, dat, otherdat):
    """Gets TF coil information and calculates shape for plotting."""
    f.seek(0)
    while 1:
        line = f.readline()
        if not line:
            break
        if '* TF Coils *' in line:
            posn = f.tell()
    f.seek(posn)
    for i in range(2): line = f.readline()
# check if superconducting
    if 'Superconducting' in line: sc_flag=1
    else: sc_flag=0
# if SC, get arc centres etc.
    if sc_flag:
        while 1:
	    line = f.readline()
	    if 'by arcs between' in line: break
	for i in range(2): line = f.readline()
	tfxsysin = []
	tfcntsin = []
	tfxsysout = []
        for i in range(5):
	    line = f.readline()
	    tfxsysin.append(line.split())
	for i in range(4): line = f.readline()
	for i in range(4):
	    line = f.readline()
	    tfcntsin.append(line.split())
	rads = []
	for i in range(4):
	    rads.append(np.sqrt((float(tfxsysin[i][1]) - float(tfcntsin[i][1]))**2 + (float(tfxsysin[i][2]) - float(tfcntsin[i][2]))**2))
	inpointsx = []
	inpointsy = []
	for i in range(4):
            a1 = (float(tfxsysin[i][1]) - float(tfcntsin[i][1]))/rads[i]
            a3 = (float(tfxsysin[i+1][1]) - float(tfcntsin[i][1]))/rads[i]
	    if a1 > 1: a1 = 1
	    if a1 < -1: a1 = -1
	    if a3 > 1: a3 = 1
	    if a3 < -1: a3 = 1
            ang1 = arcsin(a1)
	    ang2 = arcsin(a3)
	    inpointsx = np.concatenate([inpointsx, rads[i]*np.sin(np.linspace(ang1, ang2, 30, endpoint=True)) + float(tfcntsin[i][1])])
	    inpointsy = np.concatenate([inpointsy, rads[i]*np.cos(np.linspace(ang1, ang2, 30, endpoint=True)) + float(tfcntsin[i][2])])
        inpointsx = np.concatenate([inpointsx, inpointsx[::-1]])
	inpointsy = np.concatenate([inpointsy, -inpointsy[::-1]])
        inwid = otherdat[19] - otherdat[5]
	outwid = inwid + dat[5] + dat[20]
	outpointsx = ((inpointsx - otherdat[5]) * (outwid/inwid)) + otherdat[4]
	extern = (otherdat[28]/otherdat[27])
	if otherdat[-1]:
	    extern = (otherdat[21] - otherdat[36])/(otherdat[22] - otherdat[35])
	outpointsy = inpointsy * extern
        while 1:
	    line = f.readline()
	    if '* Superconducting TF Coils *' in line: break
	for i in range(2): line = f.readline()
	sctype = line[21:].rstrip().lstrip()
    else:
# conventional Cu coil
        delta = find_val_one(f, 'triang')
        inpt = otherdat[4]
        outpt = otherdat[20]
        inthk = dat[5]
        outthk = dat[20]
        toppt = max(otherdat[21:])
        topthk = dat[-2]
        r01 = (inpt+outpt)/2.
        r02 = (inpt+inthk + outpt-outthk)/2.
        a1 = r01 - inpt
        a2 = r02 - inpt - inthk
        kap1 = toppt/a1
        kap2 = (toppt-topthk)/a2
        angs = np.linspace(0, 2.*np.pi, 256, endpoint=True)
        outpointsx = r01 + a1*np.cos(angs - delta*sin(-1.*angs))
        outpointsy = kap1 * a1 * np.sin(angs)
        inpointsx = r02 + a2*np.cos(angs - delta*sin(-1.*angs))
        inpointsy = kap2 * a2 * np.sin(angs)
        sctype = 'Cu'
    outpointsy = outpointsy + dat[-1]
    inpointsy = inpointsy + dat[-1]
    return sctype, inpointsx, inpointsy, outpointsx, outpointsy
    
def plotthick(inpt, outpt, inthk, outthk, toppt, topthk, delta, col):
    """Plots a thick continuous radial D-section."""
    r01 = (inpt+outpt)/2.
    r02 = (inpt+inthk + outpt-outthk)/2.
    a1 = r01 - inpt
    a2 = r02 - inpt - inthk
    kap1 = toppt/a1
    kap2 = (toppt-topthk)/a2
    angs = np.linspace(0., 2.*np.pi, 256, endpoint=True)
    rs1 = r01 + a1*cos(angs - delta*sin(-1.*angs))
    zs1 = kap1 * a1 * np.sin(angs)
    rs2 = r02 + a2*np.cos(angs - delta*np.sin(-1.*angs))
    zs2 = kap2 * a2 * np.sin(angs)
#    plot(rs1, zs1, color = 'black'), plot(rs2, zs2, color='black')
#   code below plots filled shape
    rs = concatenate( (rs1, rs2[::-1]) )
    zs = concatenate( (zs1, zs2[::-1]) )
    fill(rs, zs, color=col)

def plotdgap(inpt, outpt, inthk, outthk, toppt, topthk, delta, col):
    """Plots a thick D-section with a gap top and bottom."""
    plotdhgap(inpt, outpt, inthk, outthk, toppt, topthk, delta, col)
    plotdhgap(inpt, outpt, inthk, outthk, -toppt, -topthk, delta, col)

def plotdhgap(inpt, outpt, inthk, outthk, toppt, topthk, delta, col):
    """Plots half a thick D-section with a gap."""
    arc = np.pi/4.
    r01 = (inpt+outpt)/2.
    r02 = (inpt+inthk + outpt-outthk)/2.
    a1 = r01 - inpt
    a2 = r02 - inpt - inthk
    kap1 = toppt/a1
    kap2 = (toppt-topthk)/a2
#    angs = ((np.pi/2.) - arc/2.) * findgen(50)/49.
    angs = np.linspace(0., (np.pi/2.) - arc/2., 50, endpoint=True)
    rs1 = r01 + a1*np.cos(angs + delta*np.sin(angs))
    zs1 = kap1 * a1 * np.sin(angs)
    rs2 = r02 + a2*np.cos(angs + delta*np.sin(angs))
    zs2 = kap2 * a2 * np.sin(angs)
# angs = !pi + ((!pi/2.) - arc) * findgen(50)/49.
    angs = np.linspace(np.pi, np.pi+((np.pi/2.) - arc), 50, endpoint=True)
    rs3 = r01 + a1*np.cos(angs + delta*np.sin(angs))
    zs3 = kap1 * a1 * np.sin(angs)
    rs4 = r02 + a2*np.cos(angs + delta*np.sin(angs))
    zs4 = kap2 * a2 * np.sin(angs)
    plot(np.concatenate([rs1,rs2[::-1]]), np.concatenate([zs1,zs2[::-1]]), color='black')
    plot(np.concatenate([rs3,rs4[::-1]]), -np.concatenate([zs3,zs4[::-1]]), color='black')
    fill(np.concatenate([rs1,rs2[::-1]]), np.concatenate([zs1,zs2[::-1]]), color=col)
    fill(np.concatenate([rs3,rs4[::-1]]), -np.concatenate([zs3,zs4[::-1]]), color=col)
    
def plotdh(r0, a, delta, kap):
    """Plots half a thin D-section."""
    angs = np.linspace(0, np.pi, 50, endpoint=True)
    rs = r0 + a*np.cos(angs + delta*np.sin(1.*angs))
    zs = kap * a * np.sin(angs)
    plot(rs, zs, color = 'black')
    return rs, zs

def gather_info(f, tf_type):
    """Gathers all the data into two structures, data and info."""
    f.seek(0)
    info = []
    data = []
# get the run info
    for i in range(7): line = f.readline()
    for i in range(4):
        line = f.readline()
	info.append(line.rstrip().lstrip())
	
# field 1: Geometry and other at-a-glance info
    data.append(['1', 'rmajor', r'$R_0$', str(find_val_one(f, 'rmajor')), 'm'])
    data.append(['1', 'rminor', r'$a$', str(find_val_one(f,'rminor')), 'm'])
    data.append(['1', 'aspect', 'Aspect ratio', str(find_val_one(f, 'aspect')), ''])
    data.append(['1', 'kappa95', r'$\kappa_{95}$', str(find_val_one(f, 'kappa95')), ''])
    data.append(['1', 'triang95', r'$\delta_{95}$', str(find_val_one(f, 'triang95')), ''])
    data.append(['1', 'sarea', 'Surface area', str(find_val_one(f, 'sarea')), r'm$^2$'])
    data.append(['1', 'vol', 'Plasma volume', str(find_val_one(f, 'vol')), r'm$^3$'])
    data.append(['1', 'tfno', 'No. of TF coils', str(int(find_val_one(f, 'tfno'))), ''])
# inboard and outboard blanket thickness
    thk1 = find_val_one(f, 'shldith') + find_val_one(f, 'blnkith')
    thk2 = find_val_one(f, 'shldoth') + find_val_one(f, 'blnkoth')
    data.append(['1', 'shldith', 'i/b blkt/shld', str(thk1), 'm'])
    data.append(['1', 'shldoth', 'o/b blkt/shld', str(thk2), 'm'])
    data.append(['1', 'powfmw', 'Fusion power', str(find_val_one(f, 'powfmw')), 'MW'])
# estimate helium confinement time and ratio
    powfmw  = find_val_one(f, 'powfmw')
    efus = 17.6e0 * 1.602e-19
    rpers = powfmw/efus
    nalp = find_val_one(f, 'dnalp') * find_val_one(f, 'vol')
    taupst = nalp/rpers
    print 'tau*:', taupst, '(assumes D-T fusion...!)'
    print 'tau*/taue:', taupst/find_val_one(f, 'taueff')
# field 2: physics
    data.append(['2', 'plascur', r'$I_P$', str(find_val_one(f, 'plascur/1D6')), 'MA'])
    data.append(['2', 'bt', r'Vacuum $B_T$ at $R_0$', str(find_val_one(f, 'bt')), 'T'])
    data.append(['2', 'q', r'$q_{\mathrm{edge}}$', str(find_val_one(f, 'q')), ''])
    bettype = find_val_two(f, 'ICULBL')
# defines which beta limit applies to and hence which normalised beta is quoted
    betna = find_val_three(f, 'Normalised beta', 58, 20)
# if this returns "None" then we are in new format and can get explicit values
# otherwise we are in old format
    if betna != None:
        betna = float(betna)
        betaa = find_val_one(f, 'beta')
        betta = float(find_val_three(f, 'Thermal beta', 58, 20))
        betnba = find_val_one(f, 'betanb')
        if bettype == -1:
            betn = betna
            bett = betna * betta/betaa
        elif bettype == 0:
            betn = betna
            bett = betna * betta/betaa
        elif bettype == 1:
            betn = betna * betaa/betta
            bett = betna
        elif bettype == 2:
            betn = betna * betaa/(betta+betnba)
            bett = betna * betta/(betta+betnba)
        else:
            print 'ICULBL value not identified: ', bettype
            betn = 0.0
            bett = 0.0
    else:
        bett = float(find_val_three(f, 'Normalised thermal beta', 58, 20))
	betn = float(find_val_three(f, 'Normalised total beta', 58, 20))
    data.append(['2', 'beta_nt', r'$\beta_N$, thermal', str(round(bett,3)), r'% m T MA$^{-1}$'])
    data.append(['2', 'beta_n', r'$\beta_N$, total', str(round(betn,3)), r'% m T MA$^{-1}$'])
    betapt = float(find_val_three(f, 'Thermal poloidal beta', 58, 20))
    data.append(['2', 'beta_pt', r'$\beta_P$, thermal', str(round(betapt,3)), ''])
    data.append(['2', 'betap', r'$\beta_P$, total', str(find_val_one(f, 'betap')), ''])
    data.append(['2', 'te', r'$<T_e>$', str(find_val_one(f, 'te')), 'keV'])
    data.append(['2', 'dene', r'$<n_{\mathrm{e, vol}}>$', '%.3e' % find_val_one(f, 'dene'), 'm$^{-3}$'])
    nong = find_val_one(f, 'dnla')/find_val_one(f, 'dlimit(7)')
    data.append(['2', 'dlimit(7)', r'$<n_{\mathrm{e, line}}>/n_G$', '%.3f' % nong, ''])
    data.append(['2', 'alphat', r'$T_{e0}/<T_e>$', str(1.+find_val_one(f, 'alphat')), ''])
    data.append(['2', 'alphan', r'$n_{e0}/<n_{\mathrm{e, vol}}>$', str(1.+find_val_one(f, 'alphan')), ''])
    data.append(['2', 'zeff', r'$Z_{\mathrm{eff}}$', str(find_val_one(f, 'zeff')), ''])
    data.append(['2', 'zeffso', r'$Z_{\mathrm{eff, SoL}}$', str(find_val_one(f, 'zeffso')), ''])
    data.append(['2', 'dnz', r'$n_Z/<n_{\mathrm{e, vol}}>$', '%.3e' % (find_val_one(f, 'dnz')/find_val_one(f, 'dene')), ''])
    data.append(['2', 'taueff', r'$\tau_e$', str(find_val_one(f, 'taueff')), 's'])
    data.append(['2', 'hfact', 'H-factor', str(find_val_one(f, 'hfact')), ''])
    slaw = find_val_three(f, 'Confinement scaling law', 44, 13)
    data.append(['2', 'law', 'Scaling law', slaw.rstrip(), ''])
# field 3: currents and magnetics
    f.seek(0)
    while 1:
        line = f.readline()
        if not line:
            break
        if 'PF Coil Information' in line:
            posn = f.tell()
    f.seek(posn)
    for i in range(5): line = f.readline()
    lcount = 0
    while len(line) > 5:
        lcount = lcount + 1
        line = f.readline()
    f.seek(posn)
    lcount = lcount - 2
    for i in range(3): line = f.readline()
    for i in range((lcount-1)/2):
        for i in range(2): line = f.readline()
        data.append(['3', line[:5].rstrip().lstrip(), line[:5].rstrip().lstrip(), line[6:14].rstrip().lstrip(), 'MA'])
    data.append(['3', 'vsind+vsres', 'Startup flux swing', '%.3e' % (find_val_one(f, 'vsind')+find_val_one(f, 'vsres')), 'Wb'])
    data.append(['3', 'flx', 'Available flux swing', '%.3e' % (-1.*float(find_val_three(f, 'Total :', 42, 12))), 'Wb'])
    data.append(['3', 'tburn', 'Burn time', '%.3e' % (find_val_one(f, 'tburn')/3600.), 'hrs'])
# field3.5: TF coils
    if 'Cu' not in tf_type:
        data.append(['35', 'bmaxtf', 'Peak field at conductor', str(find_val_one(f, 'bmaxtf')), 'T'])
        data.append(['35', 'iooic', r'I/I$_{\mathrm{crit}}$', str(find_val_one(f, 'iooic')), ''])
        data.append(['35', 'tmarg', 'Temperature margin', str(find_val_one(f, 'tmarg')), 'K'])
        data.append(['35', 'strtf1', 'Conduit Von Mises stress', '%.3e' % (find_val_one(f, 'strtf1')), 'Pa'])
        data.append(['35', 'strtf2', 'Case Von Mises stress', '%.3e' % (find_val_one(f, 'strtf2')), 'Pa'])
        data.append(['35', 'alstrtf', 'Allowable stress', '%.3e' % (find_val_one(f, 'alstrtf')), 'Pa'])
    else:
        pass
# field 3.7: Costs
    data.append(['37', 'CoE', 'Cost of electricity', find_val_three(f, 'Cost of electricity', 60, 13), '\$/MWh'])
    data.append(['37', 'concost', 'Constructed cost', find_val_three(f, 'concost', 60, 13), 'M\$'])
    data.append(['37', 'capcost', 'Total capex', find_val_three(f, 'capcost', 60, 13), 'M\$'])
# field 4: power flows and economics
    data.append(['4', 'wallmw', 'Av. neutron wall load', str(find_val_one(f, 'wallmw')), r'MW m$^{-2}$'])
    data.append(['4', 'pbrem*vol', 'Bremsstrahlung radiation', str(find_val_one(f, 'pbrem*vol')), 'MW'])
    data.append(['4', 'psync*vol', 'Synchrotron radiation', str(find_val_one(f, 'psync*vol')), 'MW'])
    data.append(['4', 'plrad*vol', 'Line radiation', str(find_val_one(f, 'plrad*vol')), 'MW'])
    data.append(['4', 'pnucblkt', 'Nuclear heating in blanket', str(find_val_one(f, 'pnucblkt')), 'MW'])
    data.append(['4', 'pnucshld', 'Nuclear heating in shield', str(find_val_one(f, 'pnucshld')), 'MW'])
    data.append(['4', 'pdivt', 'Psep / Pdiv', str(find_val_one(f, 'pdivt')), 'MW'])
#    pdivt = find_val_one(f, 'pdivt')
#    rmaj = find_val_one(f, 'rmajor')
#    pdivr = pdivt/rmaj
#    data.append(['4', 'hldiv', 'Divertor peak heat flux', str(find_val_one(f, 'hldiv')), r'MW m$^{-2}$'])
# H-mode threshold, Martin (2008), assuming M=2.5 (D-T)
    dnla = find_val_one(f,'dnla')/1.e20
    bt = find_val_one(f,'bt')
    surf = find_val_one(f,'sarea')
    pthresh = 0.0488 * dnla**0.717 * bt**0.803 * surf**0.941 * 0.8
    err = 0.057**2 + (0.035*np.log(dnla))**2 + (0.032*np.log(bt))**2 + (0.019*np.log(surf))**2
    err = np.sqrt(err) * pthresh
    data.append(['4', 'pthresh', 'H-mode threshold (M=2.5)', '%.3f' % pthresh + r' $\pm$ ' + '%.3f' % err, 'MW'])
#    data.append(['4', 'hldiv', r'$P_{\mathrm{div}}/R_0$', '%.3f' % (pdivr), r'MW m$^{-1}$'])
    data.append(['4', 'FWlife', 'FW/blanket life', str(float(find_val_three(f, 'First wall / blanket life', 60, 13))), 'years'])
    data.append(['4', 'Divlife', 'Divertor life', find_val_three(f, 'Divertor life ', 60, 13), 'years'])
    data.append(['4', 'HGtherm', 'Thermal power', find_val_three(f, 'High grade thermal power', 60, 13), 'MW'])
    data.append(['4', 'pgrossmw/pthermmw', 'Thermal efficiency', '%.1f' % (100.*find_val_one(f, 'pgrossmw')/find_val_one(f, 'pthermmw')), '%'])
    data.append(['4', 'pgrossmw', 'Gross electric power', str(find_val_one(f, 'pgrossmw')), 'MW'])
    data.append(['4', 'pnetelmw', 'Net electric power', str(find_val_one(f, 'pnetelmw')), 'MW'])
    data.append(['4', 'burnup', 'Fuel burnup fraction', str(find_val_one(f, 'burnup')), ''])
    dtyear = find_val_one(f, 'frate') * 3600. * 24. * 365.24
    data.append(['4', 'frate', 'D-T fuel throughput', '%.1f' % dtyear, 'kg year$^{-1}$'])
    tconsump = dtyear * 0.6 * find_val_one(f, 'burnup')
    data.append(['4', 'frate', 'T consumption', '%.1f' % tconsump, 'kg year$^{-1}$'])
# field 5: current drive
    f.seek(0)
    while 1:
        line = f.readline()
        if not line:
            break
        if '* Current Drive System *' in line:
            posn = f.tell()
    f.seek(posn)
    for i in range(2): line = f.readline()
    if 'Neutral Beam' in line: cdtype = 0
    else: cdtype = 1
    data.append(['51', 'CDsystem', line.rstrip().lstrip(), '', ''])
    pinj = find_val_one(f, 'pinji/1.d6') + find_val_one(f, 'pinje/1.d6')
    data.append(['5', 'pinji+pinje', 'SS auxiliary power', '%.3f' % pinj, 'MW'])
    check = find_val_one(f,'pheat')
    if check == -1: check = 0.0
    data.append(['5', 'pheat', 'Power for heating only', str(check/1.e6), 'MW'])
    data.append(['5', 'bootipf', 'Bootstrap fraction', str(find_val_one(f,'bootipf')), ''])
    data.append(['5', 'faccd', 'Auxiliary fraction', str(find_val_one(f,'faccd')), ''])
    data.append(['5', 'facoh', 'Ohmic fraction', '%.3f' % (1.-find_val_one(f,'bootipf')-find_val_one(f,'faccd')), ''])
    if cdtype == 0 :
        data.append(['5', 'gamnb', 'NB gamma', str(find_val_one(f,'gamnb')), r'$10^{20}$ A W$^{-1}$ m$^{-2}$'])
        data.append(['5', 'enbeam', 'NB energy', str(find_val_one(f,'enbeam')), 'keV'])
# data.append(['5', 'fshine', 'NB shine-through', str(find_val_one(f,'fshine')), ''])
    data.append(['5', 'powerht', 'Assumed heating power', str(find_val_one(f,'powerht')), 'MW'])
# H-mode threshold, Martin (2008), assuming M=2.5 (D-T)
#    dnla = find_val_one(f,'dnla')/1.e20
#    bt = find_val_one(f,'bt')
#    surf = find_val_one(f,'sarea')
#    pthresh = 0.0488 * dnla**0.717 * bt**0.803 * surf**0.941 * 0.8
#    err = 0.057**2 + (0.035*np.log(dnla))**2 + (0.032*np.log(bt))**2 + (0.019*np.log(surf))**2
#    err = np.sqrt(err) * pthresh
#    data.append(['5', 'pthresh', 'H-mode threshold (M=2.5)', '%.3f' % pthresh + r' $\pm$ ' + '%.3f' % err, 'MW'])
# additional divertor calculations
# divertor width assuming hldiv = average heat load and r = R0
#    hldiv = find_val_one(f,'hldiv')
    pdivt = find_val_one(f,'pdivt')
    rmajor = find_val_one(f,'rmajor')
    dene = find_val_one(f,'dene')
#    divwid = (pdivt/hldiv)/(2.*np.pi*rmajor)
#    data.append(['5', 'divwid', 'Guessed div. width', '%.3f' % divwid, 'm'])
    pdivr = pdivt/rmajor
    pdivnr = 10.0e20 * pdivt/(rmajor*dene)
    data.append(['5', 'pdivr', r'$\frac{P_{\mathrm{div}}}{R_{0}}$', '%.3f' % pdivr, r'MW m$^{-1}$'])
    data.append(['5', 'pdivnr', r'$\frac{P_{\mathrm{div}}}{<n> R_{0}}$', '%.3f' % pdivnr, r'$\times 10^{-20}$ MW m$^{2}$'])    

# Experimental H-factor -- without radiation correction
    powerht = find_val_one(f,'powerht')
    psync = find_val_one(f,'psync*vol')
    pbrem = find_val_one(f,'pbrem*vol')
    hfact = find_val_one(f,'hfact')
    hstar = hfact * (powerht/(powerht+psync+pbrem))**0.31
    data.append(['5', 'hstar', 'H* (non-rad. corr.)', '%.3f' % hstar, ''])
    return data, info
