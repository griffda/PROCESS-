#!/usr/bin/env python
"""

  PROCESS plot_proc using process_io_lib functions and MFILE.DAT

  James Morris
  13/04/2014
  CCFE
  Revised by Michael Kovari, 9/12/2105

"""

import sys
import argparse
import process_io_lib.mfile as mf
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as bpdf

import math
from matplotlib.path import Path
import matplotlib.patches as patches
import scipy as sp
import numpy as np
try:
    import process_io_lib.process_dicts as proc_dict
except ImportError:
    print("The Python dictionaries have not yet been created. Please run 'make dicts'!")
    exit()


solenoid = 'pink'
cscompression = 'red'
tfc = 'cyan'
thermal_shield = 'gray'
vessel = 'green'
shield = 'green'
blanket = 'magenta'
plasma = 'khaki'
cryostat = 'red'
firstwall = 'darkblue'
winding = 'blue'
nbshield_colour = 'gray'

thin = 0

RADIAL_BUILD = ["bore", "ohcth", "precomp", "gapoh", "tfcth",
                "deltf","thshieldi","gapds",
                 "ddwi", "shldith", "vvblgapi","blnkith", "fwith", "scrapli",
                "rminori", "rminoro", "scraplo", "fwoth", "blnkoth","vvblgapo",
                "shldoth", "ddwo", "gapsto", "thshieldo","tftsgap","tfthko"]

VERTICAL_BUILD = ["tfcth", "tftsgap","thshield", 
                  "vgap2", "ddwi", "shldtth","vvblgap",
                  "blnktth", "fwtth", "vgaptop",
                  "rminor*kappa", "midplane", "rminor*kappa",
                  "vgap", "divfix", "ddwi", "shldlth", "vgap2", "thshield", 
                  "tftsgap", "tfcth"]

vertical_upper = ["rminor*kappa", "vgaptop",
                  "fwtth","blnktth","vvblgap",
                  "shldtth","ddwi", 
                  "vgap2","thshield", 
                  "tftsgap", "tfcth"]
                  
vertical_lower = ["rminor*kappa", "vgap", 
                  "divfix", 
                  "shldlth", "ddwi", 
                  "vgap2", 
                  "thshield", 
                  "tftsgap", 
                  "tfcth"]                                

ANIMATION_INFO = [("rmajor", "Major radius", "m"),
                  ("rminor", "Minor radius", "m"),
                  ("aspect", "Aspect ratio", "")]

rtangle = np.pi / 2


def plotdh(axis, r0, a, delta, kap):
    """Plots half a thin D-section, centred on z = 0.

    Arguments:
        axis --> axis object to plot to
        r0 --> major radius of centre
        a --> horizontal radius
        delta --> triangularity
        kap --> elongation

    Returns:
        rs --> radial coordinates of D-section
        zs --> vertical coordinates of D-section
        """
    angs = np.linspace(0, np.pi, 50, endpoint=True)
    rs = r0 + a * np.cos(angs + delta * np.sin(1.0 * angs))
    zs = kap * a * np.sin(angs)
    axis.plot(rs, zs, color='black', lw=thin)
    return rs, zs


def plotdhgap(axis, inpt, outpt, inthk, outthk, toppt, topthk, delta, col):
    """Plots half a thick D-section with a gap.

    Arguments:
        axis --> axis object to plot to
        inpt --> inner points
        outpt --> outer points
        inthk --> inner thickness
        outthk --> outer thickness
        toppt --> top points
        topthk --> top thickness
        delta --> triangularity
        col --> color for fill

    """
    arc = np.pi / 4.
    r01 = (inpt + outpt) / 2.
    r02 = (inpt + inthk + outpt - outthk) / 2.
    a1 = r01 - inpt
    a2 = r02 - inpt - inthk
    kap1 = toppt / a1
    kap2 = (toppt - topthk) / a2
    # angs = ((np.pi/2.) - arc/2.) * findgen(50)/49.
    angs = np.linspace(0., (np.pi / 2.) - arc / 2., 50, endpoint=True)
    rs1 = r01 + a1 * np.cos(angs + delta * np.sin(angs))
    zs1 = kap1 * a1 * np.sin(angs)
    rs2 = r02 + a2 * np.cos(angs + delta * np.sin(angs))
    zs2 = kap2 * a2 * np.sin(angs)
    # angs = !pi + ((!pi/2.) - arc) * findgen(50)/49.
    angs = np.linspace(np.pi, np.pi + ((np.pi / 2.) - arc), 50, endpoint=True)
    rs3 = r01 + a1 * np.cos(angs + delta * np.sin(angs))
    zs3 = kap1 * a1 * np.sin(angs)
    rs4 = r02 + a2 * np.cos(angs + delta * np.sin(angs))
    zs4 = kap2 * a2 * np.sin(angs)

    axis.plot(np.concatenate([rs1, rs2[::-1]]),
              np.concatenate([zs1, zs2[::-1]]), color='black', lw=thin)
    axis.plot(np.concatenate([rs3, rs4[::-1]]),
              -np.concatenate([zs3, zs4[::-1]]), color='black', lw=thin)
    axis.fill(np.concatenate([rs1, rs2[::-1]]),
              np.concatenate([zs1, zs2[::-1]]), color=col)
    axis.fill(np.concatenate([rs3, rs4[::-1]]),
              -np.concatenate([zs3, zs4[::-1]]), color=col)


def plot_plasma(axis, mfile_data, scan):
    """Plots the plasma boundary arcs.

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE data object
        scan --> scan number to use

    """

    r0 = mfile_data.data["rmajor"].get_scan(scan)
    a = mfile_data.data["rminor"].get_scan(scan)
    delta = 1.5 * mfile_data.data["triang95"].get_scan(scan)
    kappa = (1.1 * mfile_data.data["kappa95"].get_scan(scan)) + 0.04
    snull = mfile_data.data["snull"].get_scan(scan)

    x1 = (2. * r0 * (1. + delta) - a * (delta ** 2 + kappa ** 2 - 1.0)) / (2. * (1. + delta))
    x2 = (2. * r0 * (delta - 1.) - a * (delta ** 2 + kappa ** 2 - 1.0)) / (2. * (delta - 1.))
    r1 = 0.5 * math.sqrt((a ** 2 * ((delta + 1.) ** 2 + kappa ** 2) ** 2) / ((delta + 1.) ** 2))
    r2 = 0.5 * math.sqrt((a ** 2 * ((delta - 1.) ** 2 + kappa ** 2) ** 2) / ((delta - 1.) ** 2))
    theta1 = sp.arcsin((kappa * a) / r1)
    theta2 = sp.arcsin((kappa * a) / r2)
    inang = 1.0 / r1
    outang = 1.5 / r2
    if snull == 0:
        angs1 = np.linspace(-(inang + theta1) + np.pi, (inang + theta1) + np.pi,
                            256, endpoint=True)
        angs2 = np.linspace(-(outang + theta2), (outang + theta2), 256,
                            endpoint=True)
    elif snull < 0:
        angs1 = np.linspace(-(inang + theta1) + np.pi, theta1 + np.pi, 256,
                            endpoint=True)
        angs2 = np.linspace(-theta2, (outang + theta2), 256, endpoint=True)
    else:
        angs1 = np.linspace(-theta1 + np.pi, (inang + theta1) + np.pi, 256,
                            endpoint=True)
        angs2 = np.linspace(-(outang + theta2), theta2, 256, endpoint=True)
    xs1 = -(r1 * np.cos(angs1) - x1)
    ys1 = r1 * np.sin(angs1)
    xs2 = -(r2 * np.cos(angs2) - x2)
    ys2 = r2 * np.sin(angs2)
    axis.plot(xs1, ys1, color='black')
    axis.plot(xs2, ys2, color='black')
    axis.fill(xs1, ys1, color=plasma)
    axis.fill(xs2, ys2, color=plasma)


def plot_centre_cross(axis, mfile_data, scan):
    """Function to plot centre cross on plot

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE data object
        scan --> scan number to use
    """
    rmajor = mfile_data.data["rmajor"].get_scan(scan)
    axis.plot([rmajor - 0.25, rmajor + 0.25, rmajor, rmajor, rmajor],
              [0, 0, 0, 0.25, -0.25], color='black')


def cumulative_radial_build(section, mfile_data, scan):
    """Function for calculating the cumulative radial build up to and
    including the given section.

    Arguments:
        section --> section of the radial build to go up to
        mfile_data --> MFILE data object
        scan --> scan number to use

    Returns:
        cumulative_build --> cumulative radial build up to section given

    """

    cumulative_build = 0
    for item in RADIAL_BUILD:
        if item == "rminori" or item == "rminoro":
            cumulative_build += mfile_data.data["rminor"].get_scan(scan)
        elif item == "vvblgapi" or item == "vvblgapo":
            cumulative_build += mfile_data.data["vvblgap"].get_scan(scan)            
        elif item == "thshieldi" or item == "thshieldo":
            cumulative_build += mfile_data.data["thshield"].get_scan(scan)            
        elif "ddw" in item:
            cumulative_build += mfile_data.data["ddwi"].get_scan(scan)
        else:
            cumulative_build += mfile_data.data[item].get_scan(scan)
        if item == section:
            break
    return cumulative_build

def cumulative_radial_build2(section, mfile_data, scan):
    """Function for calculating the cumulative radial build up to and
    including the given section.

    Arguments:
        section --> section of the radial build to go up to
        mfile_data --> MFILE data object
        scan --> scan number to use

    Returns:
        cumulative_build --> cumulative radial build up to and including section given
        previous         --> cumulative radial build up to section given

    """
    cumulative_build = 0; build = 0
    for item in RADIAL_BUILD:
        if item == "rminori" or item == "rminoro":
            build = mfile_data.data["rminor"].get_scan(scan)
        elif item == "vvblgapi" or item == "vvblgapo":
            build = mfile_data.data["vvblgap"].get_scan(scan)            
        elif item == "thshieldi" or item == "thshieldo":
            build = mfile_data.data["thshield"].get_scan(scan)            
        elif "ddw" in item:
            build = mfile_data.data["ddwi"].get_scan(scan)
        else:
            build = mfile_data.data[item].get_scan(scan)
        cumulative_build += build 
        if item == section:
            break
    previous = cumulative_build - build
    return (cumulative_build, previous)

def cumulative_vertical_build(mfile_data, scan):
    """Function for calculating the cumulative vertical build

    Arguments:
        mfile_data --> MFILE data object
        scan --> scan number to use

    Returns:
        cumulative_build --> vertical build list
    """

    cumulative_build = list()
    cumulative_build.append(0.0)

    # Top
    top_build = 0
    start = False
    for i in range(len(VERTICAL_BUILD) - 1, -1, -1):
        if not start:
            if VERTICAL_BUILD[i] == "midplane":
                start = True
        else:
            top_build += mfile_data.data[VERTICAL_BUILD[i]].get_scan(scan)
            cumulative_build.insert(0, top_build)

    # bottom
    start = False
    bottom_build = 0
    for j in range(0, len(VERTICAL_BUILD), 1):
        if not start:
            if VERTICAL_BUILD[j] == "midplane":
                start = True
        else:
            bottom_build -= mfile_data.data[VERTICAL_BUILD[j]].get_scan(scan)
            cumulative_build.append(bottom_build)

    return cumulative_build



def poloidal_cross_section(axis, mfile_data, scan=-1):
    """Function to plot poloidal cross-section

    Arguments:
      axis --> axis object to add plot to
      mfile_data --> MFILE data object
      scan --> scan number to use

    """
    xmin = 0
    xmax = 20
    ymin = -15
    ymax = 15
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)
    axis.set_xlabel('R / m')
    axis.set_ylabel('Z / m')
    axis.set_title('Poloidal cross-section')

    if mfile_data.data["snull"].get_scan(scan):
        plot_vacuum_vessel_snull(axis, mfile_data, scan)
        plot_shield_snull(axis, mfile_data, scan)
        plot_blanket_snull(axis, mfile_data, scan)
        plot_firstwall_snull(axis, mfile_data, scan)

    plot_plasma(axis, mfile_data, scan)
    plot_centre_cross(axis, mfile_data, scan)
    plot_cryostat(axis, mfile_data, scan)
    
def plot_cryostat(axis, mfile_data, scan):
    """Function to plot cryostat in poloidal cross-section"""    
    rect = patches.Rectangle([rdewex, 0], ddwex, zdewex+ddwex, lw=0, facecolor=cryostat)
    axis.add_patch(rect)
    rect = patches.Rectangle([rdewex, 0], ddwex, -(zdewex+ddwex), lw=0, facecolor=cryostat)
    axis.add_patch(rect)
    
    rect = patches.Rectangle([0, zdewex], rdewex, ddwex, lw=0, facecolor=cryostat)
    axis.add_patch(rect)
    rect = patches.Rectangle([0, -zdewex], rdewex, -ddwex, lw=0, facecolor=cryostat)
    axis.add_patch(rect)
    

def color_key(axis):
    """Function to plot the colour key
    Arguments:
      axis --> object to add plot to
    """
    axis.set_ylim([0, 10])
    axis.set_xlim([0, 10])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    axis.text(-5, 10, 'Central Solenoid', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 9.7], 1, 0.4, lw=0, facecolor=solenoid))

    axis.text(-5, 9, 'CS compression', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 8.7], 1, 0.4, lw=0, facecolor=cscompression))

    axis.text(-5, 8,  'TF coil', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 7.7], 1, 0.4, lw=0, facecolor=tfc))

    axis.text(-5, 7,  'Thermal shield', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 6.7], 1, 0.4, lw=0, facecolor=thermal_shield))

    axis.text(-5, 6,  'V. vessel & shield', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 5.7], 1, 0.4, lw=0, facecolor=vessel))

    axis.text(-5, 5,  'Blanket', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 4.7], 1, 0.4, lw=0, facecolor=blanket))
 
    axis.text(-5, 4,  'First wall', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 3.7], 1, 0.4, lw=0, facecolor=firstwall))

    axis.text(-5, 3,  'Plasma', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 2.7], 1, 0.4, lw=0, facecolor=plasma))

    axis.text(-5, 2,  'PF coils', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 1.7], 1, 0.4, lw=1, facecolor='none'))
    
    axis.text(-5, 1,  'NB duct shield', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, 0.7], 1, 0.4, lw=0, facecolor=nbshield_colour))

    axis.text(-5, 0.1,  'cryostat', ha='left', va='top', size='medium')
    axis.add_patch(patches.Rectangle([0.2, -0.3], 1, 0.4, lw=0, facecolor=cryostat))



def toroidal_cross_section(axis, mfile_data, scan=-1):
    """Function to plot toroidal cross-section
    Arguments:
      axis --> axis object to add plot to
      mfile_data --> MFILE data object
      scan --> scan number to use
    """

    xmin = 0
    xmax =20
    ymin = 0
    ymax = 20
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)
    axis.set_xlabel('x / m')
    axis.set_ylabel('y / m')
    axis.set_title('Toroidal cross-section')

    arc(axis, rmajor, style='dashed')
    
    # Colour in the main components
    r2, r1 = cumulative_radial_build2("ohcth", mfile_data, scan)
    arc_fill(axis, r1, r2, color=solenoid)
    
    r2, r1 = cumulative_radial_build2("precomp", mfile_data, scan)
    arc_fill(axis, r1, r2, color=cscompression)
    
    r2, r1 = cumulative_radial_build2("tfcth", mfile_data, scan)
    arc_fill(axis, r1, r2, color=tfc)
    
    r2, r1 = cumulative_radial_build2("thshieldi", mfile_data, scan)
    arc_fill(axis, r1, r2, color=thermal_shield)
    
    
    r2, r1 = cumulative_radial_build2("ddwi", mfile_data, scan)
    arc_fill(axis, r1, r2, color=vessel)
    
    r2, r1 = cumulative_radial_build2("shldith", mfile_data, scan)
    arc_fill(axis, r1, r2, color=vessel)
    
    r2, r1 = cumulative_radial_build2("blnkith", mfile_data, scan)
    arc_fill(axis, r1, r2, color=blanket)
       
    r2, r1 = cumulative_radial_build2("fwith", mfile_data, scan)
    arc_fill(axis, r1, r2, color=firstwall)
    
    arc_fill(axis, rmajor-rminor, rmajor+rminor, color=plasma)
       
    r2, r1 = cumulative_radial_build2("fwoth", mfile_data, scan)
    arc_fill(axis, r1, r2, color=firstwall)
       
    r2, r1 = cumulative_radial_build2("blnkoth", mfile_data, scan)
    arc_fill(axis, r1, r2, color=blanket)
       
    r2, r1 = cumulative_radial_build2("shldoth", mfile_data, scan)
    arc_fill(axis, r1, r2, color=shield)
    
    r2, r1 = cumulative_radial_build2("ddwo", mfile_data, scan)
    arc_fill(axis, r1, r2, color=vessel)
    
    r2, r1 = cumulative_radial_build2("thshieldo", mfile_data, scan)
    arc_fill(axis, r1, r2, color=thermal_shield)
    
    arc_fill(axis, rdewex, rdewex+ddwex, color=cryostat)

    # Segment the TF coil inboard
    # Calculate centrelines
    n = int(tfno/4)+1
    spacing = 2*np.pi/tfno
    i = np.arange(0,n)

    ang = i*spacing
    angl = ang - spacing/2
    angu = ang + spacing/2
    r1, null = cumulative_radial_build2("gapoh", mfile_data, scan)
    r2, null = cumulative_radial_build2("tfcth", mfile_data, scan)
    r4, r3 = cumulative_radial_build2("tfthko", mfile_data, scan)
    
    #Coil width
    w = r2 * np.tan(spacing/2)
    xi = r1 * np.cos(angl)
    yi = r1 * np.sin(angl)
    xo = r2 * np.cos(angl)
    yo = r2 * np.sin(angl)
    axis.plot((xi, xo),(yi, yo), color='black')
    xi = r1 * np.cos(angu)
    yi = r1 * np.sin(angu)
    xo = r2 * np.cos(angu)
    yo = r2 * np.sin(angu)
    axis.plot((xi, xo),(yi, yo), color='black')
    # Annotate plot.
    axis.text(rmajor*np.cos(0.3), rmajor*np.sin(0.3), "plasma", fontsize=(12),ha='center', va='center')
    axis.text((rdewex+ddwex)/1.41, (rdewex+ddwex)/1.41, "cryostat", fontsize=(10),ha='left', va='bottom')

    for item in i:
        # Neutral beam shielding
        TF_outboard(axis, item, tfno=tfno,r3=r3, r4=r4, w=w+nbshield, facecolor=nbshield_colour)
        # Overlay TF coil segments
        TF_outboard(axis, item, tfno=tfno,r3=r3, r4=r4, w=w, facecolor='cyan')

    # Winding pack: inboard
    rect = patches.Rectangle([r1 + thkcas +tinstf, 0], thkwp/2, wwp2/2, lw=0, facecolor=winding)
    axis.add_patch(rect)
    rect = patches.Rectangle([r1 + thkcas +tinstf + thkwp/2, 0], thkwp/2, wwp1/2, lw=0, facecolor=winding)
    axis.add_patch(rect)
    # Outboard
    rect = patches.Rectangle([r3+casthi+tinstf, 0], thkwp/2, wwp1/2, lw=0, facecolor=winding)
    axis.add_patch(rect)
    rect = patches.Rectangle([r3+casthi+tinstf+thkwp/2, 0], thkwp/2, wwp2/2, lw=0, facecolor=winding)
    axis.add_patch(rect)

    # Neutral beam geometry
    a = w
    b = tfthko
    c = beamwd + 2 * nbshield
    d = r3
    e = np.sqrt(a**2 +(d+b)**2)
    # Coordinates of the inner and outer edges of the beam at its tangency point
    rinner = rtanbeam - beamwd
    router = rtanbeam + beamwd
    beta = np.arccos(rinner/e)
    xinner = rinner * np.cos(beta)
    yinner = rinner * np.sin(beta)
    xouter = router * np.cos(beta)
    youter = router * np.sin(beta)
    # Corner of TF coils
    xcorner = r4 
    ycorner = w+nbshield
    axis.plot([xinner, xcorner], [yinner, ycorner], linestyle='dotted', color='black')
    x = xcorner + c * np.cos(beta) - nbshield * np.cos(beta)
    y = ycorner + c * np.sin(beta) - nbshield * np.sin(beta)
    axis.plot([xouter, x], [youter, y], linestyle='dotted', color='black')


def TF_outboard(axis, item, tfno,r3, r4, w, facecolor):
    spacing = 2*np.pi/tfno
    ang = item*spacing
    dx = w * np.sin(ang)
    dy = w * np.cos(ang)
    x1 = r3 * np.cos(ang) + dx
    y1 = r3 * np.sin(ang) - dy
    x2 = r4 * np.cos(ang) + dx
    y2 = r4 * np.sin(ang) - dy
    x3 = r4 * np.cos(ang) - dx
    y3 = r4 * np.sin(ang) + dy
    x4 = r3 * np.cos(ang) - dx
    y4 = r3 * np.sin(ang) + dy
    verts = [(x1, y1), (x2, y2), (x3, y3), (x4, y4), (x1, y1)]
    path = Path(verts, closed=True)
    patch = patches.PathPatch(path, facecolor=facecolor, lw=0)
    axis.add_patch(patch)


def arc(axis, r, theta1=0, theta2=rtangle, style='solid'):
    """Plots an arc.

    Arguments

    axis: plot object
    r: radius
    theta1: starting polar angle
    theta2: finishing polar angle

    """
    angs = np.linspace(theta1, theta2)
    xs = r * np.cos(angs)
    ys = r * np.sin(angs)
    axis.plot(xs, ys, linestyle=style, color='black', lw=0.2)


def arc_fill(axis, r1, r2, color='pink'):
    """Fills the space between two quarter circles.

    Arguments

    axis: plot object
    r1, r2 radii to be filled

    """
    angs = np.linspace(0, rtangle, endpoint=True)
    xs1 = r1 * np.cos(angs)
    ys1 = r1 * np.sin(angs)
    angs = np.linspace(rtangle,0, endpoint=True)
    xs2 = r2 * np.cos(angs)
    ys2 = r2 * np.sin(angs)
    verts = list(zip(xs1, ys1))
    verts.extend(list(zip(xs2, ys2)))
    endpoint=[(r2,0)]
    verts.extend(endpoint)
    path = Path(verts, closed=True)
    patch = patches.PathPatch(path, facecolor=color, lw=0)
    axis.add_patch(patch)
    
def ellips_fill(axis, a1=0, a2=0, b1=0, b2=0, x0=0, y0=0, ang1=0, ang2=rtangle, color='pink'):
    """Fills the space between two concentric ellipse sectors.

    Arguments

    axis: plot object
    a1, a2, b1, b2 horizontal and vertical radii to be filled
    x0, y0 coordinates of centre of the ellipses
    ang1, ang2 are the polar angles of the start and end

    """
    angs = np.linspace(ang1, ang2, endpoint=True)
    r1 = ((np.cos(angs)/a1)**2 + (np.sin(angs)/b1)**2)**(-0.5)
    xs1 = r1 * np.cos(angs) + x0
    ys1 = r1 * np.sin(angs) + y0
    angs = np.linspace(ang2, ang1, endpoint=True)
    r2 = ((np.cos(angs)/a2)**2 + (np.sin(angs)/b2)**2)**(-0.5)
    xs2 = r2 * np.cos(angs) + x0
    ys2 = r2 * np.sin(angs) + y0
    verts = list(zip(xs1, ys1))
    verts.extend(list(zip(xs2, ys2)))
    endpoint=verts[-1:]
    verts.extend(endpoint)
    path = Path(verts, closed=True)
    patch = patches.PathPatch(path, facecolor=color, lw=0)
    axis.add_patch(patch)    


def plot_nprofile(prof):
    """Function to plot density profile
    Arguments:
      prof --> axis object to add plot to
    """
    xmin = 0
    xmax = 1
    ymin = 0
    ymax = 20
    prof.set_ylim([ymin, ymax])
    prof.set_xlim([xmin, xmax])
    prof.set_autoscaley_on(False)
    prof.set_xlabel('r/a')
    prof.set_ylabel('ne / 1e19 m-3')
    prof.set_title('Density profile')

    if ipedestal == 1:
        rhocore1 = np.linspace(0,0.95*rhopedn)
        rhocore2 = np.linspace(0.95*rhopedn,rhopedn)
        rhocore = np.append(rhocore1,rhocore2)
        ncore = neped + (ne0-neped) * (1-rhocore**2/rhopedn**2)**alphan

        rhosep = np.linspace(rhopedn,1)
        nsep = nesep + (neped-nesep)* (1-rhosep)/(1-min(0.9999,rhopedn))

        rho = np.append(rhocore,rhosep)
        ne = np.append(ncore,nsep)
    else:
        rho1 = np.linspace(0,0.95)
        rho2 = np.linspace(0.95,1)
        rho = np.append(rho1,rho2)
        ne = ne0 * (1-rho**2)**alphan
    ne = ne/1e19
    prof.plot(rho,ne)

def plot_tprofile(prof):
    """Function to plot temperature profile
    Arguments:
      prof --> axis object to add plot to
    """
    xmin = 0
    xmax = 1
    ymin = 0
    ymax = 35
    prof.set_ylim([ymin, ymax])
    prof.set_xlim([xmin, xmax])
    prof.set_autoscaley_on(False)
    prof.set_xlabel('r/a')
    prof.set_ylabel('Te / keV')
    prof.set_title('Temperature profile')

    if ipedestal == 1:
        rhocore1 = np.linspace(0,0.9*rhopedn)
        rhocore2 = np.linspace(0.9*rhopedn,rhopedn)
        rhocore = np.append(rhocore1,rhocore2)
        tcore = teped + (te0-teped) * (1-(rhocore/rhopedn)**tbeta)**alphat

        rhosep = np.linspace(rhopedn,1)
        tsep = tesep + (teped-tesep)* (1-rhosep)/(1-min(0.9999,rhopedt))

        rho = np.append(rhocore,rhosep)
        te = np.append(tcore,tsep)
    else:
        rho = np.linspace(0,1)
        te = te0 * (1-rho**2)**alphat
    prof.plot(rho,te)


def plot_vacuum_vessel_snull(axis, mfile_data, scan):
    """Function to plot vacuum vessel

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE data object
        scan --> scan number to use
    """
    triang = mfile_data.data["triang95"].get_scan(scan)
    temp_array_1 = ()
    temp_array_2 = ()
    
    # Outer side (furthest from plasma)
    radx = (cumulative_radial_build("ddwo", mfile_data, scan)
            + cumulative_radial_build("gapds", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("ddwo", mfile_data, scan)
             - cumulative_radial_build("gapds", mfile_data, scan)) / 2.0    
    
    kapx = cumulative_upper['ddwi'] / rminx
    
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))

    kapx = cumulative_lower['ddwi'] / rminx 
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_2 = temp_array_2 + ((rs, zs))

    # Inner side (nearest to the plasma)
    radx = (cumulative_radial_build("shldoth", mfile_data, scan)
            + cumulative_radial_build("ddwi", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("shldoth", mfile_data, scan)
             - cumulative_radial_build("ddwi", mfile_data, scan)) / 2.0

    kapx = (cumulative_upper['ddwi'] - upper["ddwi"]) / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))
    
    kapx = (cumulative_lower['ddwi'] + lower["ddwi"]) / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_2 = temp_array_2 + ((rs, zs))

    rs = np.concatenate([temp_array_1[0], temp_array_1[2][::-1]])
    zs = np.concatenate([temp_array_1[1], temp_array_1[3][::-1]])
    axis.fill(rs, zs, color=vessel)

    rs = np.concatenate([temp_array_2[0], temp_array_2[2][::-1]])
    zs = np.concatenate([temp_array_2[1], temp_array_2[3][::-1]])
    axis.fill(rs, zs, color=vessel)


def plot_shield_snull(axis, mfile_data, scan):
    """Function to plot shield

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE data object
        scan --> scan number to use
    """

    triang = mfile_data.data["triang95"].get_scan(scan)

    # triang = mfile_data.data["triang95"].get_scan(scan)
    # blnkith = mfile_data.data["blnkith"].get_scan(scan)
    # blnkoth = mfile_data.data["blnkoth"].get_scan(scan)
    # fwith = mfile_data.data["fwith"].get_scan(scan)
    # fwoth = mfile_data.data["fwoth"].get_scan(scan)
    blnktth = mfile_data.data["blnktth"].get_scan(scan)
    tfwvt = mfile_data.data["top first wall vertical thickness (m)"]. \
        get_scan(scan)
    c_shldith = cumulative_radial_build("shldith", mfile_data, scan)
    c_blnkoth = cumulative_radial_build("blnkoth", mfile_data, scan)
    c_blnkith = cumulative_radial_build("blnkith", mfile_data, scan)
    c_fwoth = cumulative_radial_build("fwoth", mfile_data, scan)
    a = cumulative_vertical_build(mfile_data, scan)


    temp_array_1 = ()
    temp_array_2 = ()
    
    # Outer side (furthest from plasma)
    radx = (cumulative_radial_build("shldoth", mfile_data, scan) +
            cumulative_radial_build("ddwi", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("shldoth", mfile_data, scan) -
             cumulative_radial_build("ddwi", mfile_data, scan)) / 2.0
             
    kapx = cumulative_upper['shldtth'] / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))
   
    kapx = cumulative_lower['shldlth'] / rminx 
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_2 = temp_array_2 + ((rs, zs))

    # Inner side (nearest to the plasma)
    radx = (cumulative_radial_build("ddwi", mfile_data, scan)
            + cumulative_radial_build("vvblgapo", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("ddwi", mfile_data, scan)
             - cumulative_radial_build("vvblgapo", mfile_data, scan)) / 2.0

    kapx = (cumulative_upper['ddwi'] - upper["ddwi"]) / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))
    
    kapx = (cumulative_lower['ddwi'] + lower["ddwi"]) / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_2 = temp_array_2 + ((rs, zs))

    rs = np.concatenate([temp_array_1[0], temp_array_1[2][::-1]])
    zs = np.concatenate([temp_array_1[1], temp_array_1[3][::-1]])
    axis.fill(rs, zs, color=vessel)

    rs = np.concatenate([temp_array_2[0], temp_array_2[2][::-1]])
    zs = np.concatenate([temp_array_2[1], temp_array_2[3][::-1]])
    axis.fill(rs, zs, color=vessel)
    
    
    
def plot_blanket_snull(axis, mfile_data, scan):
    """Function to plot single null case of blanket (upper part only)

    Arguments:
      axis --> axis object to plot to
      mfile_data --> MFILE.DAT object
      scan --> scan number to use

    """    
    temp_array_1 = ()
    temp_array_2 = ()

    radx = (cumulative_radial_build("blnkoth", mfile_data, scan) +
            cumulative_radial_build("shldith", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("blnkoth", mfile_data, scan) -
             cumulative_radial_build("shldith", mfile_data, scan)) / 2.0             
    
    kapx = cumulative_upper['blnktth'] / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))
   
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_2 = temp_array_2 + ((rs, zs))
    
    radx = (cumulative_radial_build("blnkoth", mfile_data, scan) +
            cumulative_radial_build("shldith", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("blnkoth", mfile_data, scan) -
             cumulative_radial_build("shldith", mfile_data, scan)) / 2.0             
    
    kapx = cumulative_upper['blnktth'] / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))
   
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_2 = temp_array_2 + ((rs, zs))
    
    rs = np.concatenate([temp_array_1[0], temp_array_1[2][::-1]])
    zs = np.concatenate([temp_array_1[1], temp_array_1[3][::-1]])
    axis.fill(rs, zs, color=blanket)

    rs = np.concatenate([temp_array_2[0], temp_array_2[2][::-1]])
    zs = np.concatenate([temp_array_2[1], temp_array_2[3][::-1]])
    axis.fill(rs, zs, color=blanket)


def plot_firstwall_snull(axis, mfile_data, scan):
    """Function to plot single null case of blanket and first wall

    Arguments:
      axis --> axis object to plot to
      mfile_data --> MFILE.DAT object
      scan --> scan number to use

    """
    blnktth = mfile_data.data["blnktth"].get_scan(scan)
    tfwvt = mfile_data.data["fwtth"].get_scan(scan)
    c_shldith = cumulative_radial_build("shldith", mfile_data, scan)
    c_blnkoth = cumulative_radial_build("blnkoth", mfile_data, scan)
    c_blnkith = cumulative_radial_build("blnkith", mfile_data, scan)
    c_fwoth = cumulative_radial_build("fwoth", mfile_data, scan)
    
    temp_array_1 = ()
    temp_array_2 = ()
    
    radx = (cumulative_radial_build("fwoth", mfile_data, scan) +
            cumulative_radial_build("blnkith", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("fwoth", mfile_data, scan) -
             cumulative_radial_build("blnkith", mfile_data, scan)) / 2.0
    
    kapx = cumulative_upper['fwtth'] / rminx
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))

    radx = (cumulative_radial_build("scraplo", mfile_data, scan) +
            cumulative_radial_build("fwith", mfile_data, scan)) / 2.0
    rminx = (cumulative_radial_build("scraplo", mfile_data, scan) -
             cumulative_radial_build("fwith", mfile_data, scan)) / 2.0
    
    (rs, zs) = plotdh(axis, radx, rminx, triang, kapx)
    temp_array_1 = temp_array_1 + ((rs, zs))

    # Top  

    rs = np.concatenate([temp_array_1[0], temp_array_1[2][::-1]])
    zs = np.concatenate([temp_array_1[1], temp_array_1[3][::-1]])
    axis.fill(rs, zs, color=blanket)

    rs = np.concatenate([temp_array_1[0], temp_array_1[2][::-1]])
    zs = np.concatenate([temp_array_1[1], temp_array_1[3][::-1]])
    axis.fill(rs, zs, color=firstwall)

    # Bottom    
    
    divgap = cumulative_lower['divfix']
    plotdhgap(axis, c_shldith, c_blnkoth, blnkith, blnkoth, divgap,
              -blnktth, triang, blanket)
    plotdhgap(axis, c_blnkith, c_fwoth, fwith, fwoth,
              divgap + blnktth, -tfwvt, triang, firstwall )


def angle_check(angle1, angle2):
    """Function to perform TF coil angle check
    """
    if angle1 > 1:
        angle1 = 1
    if angle1 < -1:
        angle1 = -1
    if angle2 > 1:
        angle2 = 1
    if angle2 < -1:
        angle2 = -1
    return angle1, angle2


def plot_tf_coils(axis, mfile_data, scan):
    """Function to plot TF coils

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """
    vert_build = cumulative_vertical_build(mfile_data, scan)

    # Arc points
    # MDK Only 4 points now required for elliptical arcs
    x1 = mfile_data.data["xarc(1)"].get_scan(scan)
    y1 = mfile_data.data["yarc(1)"].get_scan(scan)
    x2 = mfile_data.data["xarc(2)"].get_scan(scan)
    y2 = mfile_data.data["yarc(2)"].get_scan(scan)
    x3 = mfile_data.data["xarc(3)"].get_scan(scan)
    y3 = mfile_data.data["yarc(3)"].get_scan(scan)
    x4 = mfile_data.data["xarc(4)"].get_scan(scan)
    y4 = mfile_data.data["yarc(4)"].get_scan(scan)
    x5 = mfile_data.data["xarc(5)"].get_scan(scan)
    y5 = mfile_data.data["yarc(5)"].get_scan(scan)
    if y3 != 0:
        print("TF coil geometry: The value of yarc(3) is not zero, but should be.")
    # Inboard upper arc
    x0 = x2
    y0 = y1
    a1 = x2-x1
    b1 = y2-y1
    a2 = a1+tfcth
    b2 = b1+tfcth 
    ellips_fill(axis, a1=a1, a2=a2, b1=b1, b2=b2, x0=x0, y0=y0, ang1=rtangle, ang2=2*rtangle, color='cyan')
    # Outboard upper arc
    x0 = x2
    y0 = 0
    a1 = x3-x2
    b1 = y2
    a2 = a1+tfcth
    b2 = b1+tfcth       
    ellips_fill(axis, a1=a1, a2=a2, b1=b1, b2=b2, x0=x0, y0=y0, ang1=0, ang2=rtangle, color='cyan')
    # Inboard lower arc
    x0 = x4
    y0 = y5
    a1 = x4-x5
    b1 = y5-y4
    a2 = a1+tfcth
    b2 = b1+tfcth
    ellips_fill(axis, a1=a1, a2=a2, b1=b1, b2=b2, x0=x0, y0=y0, ang1=-rtangle, ang2=-2*rtangle, color='cyan')
    # Outboard lower arc
    x0 = x4
    y0 = 0
    a1 = x3-x2
    b1 = -y4
    a2 = a1+tfcth
    b2 = b1+tfcth
    ellips_fill(axis, a1=a1, a2=a2, b1=b1, b2=b2, x0=x0, y0=y0, ang1=0, ang2=-rtangle, color='cyan')
    # Vertical leg
    # Bottom left corner
    rect = patches.Rectangle([x5-tfcth, y5], tfcth, (y1-y5), lw=0, facecolor='cyan')
    axis.add_patch(rect)
    



def plot_pf_coils(axis, mfile_data, scan):
    """Function to plot PF coils

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """

    coils_r = []
    coils_z = []
    coils_dr = []
    coils_dz = []
    coil_text = []

    # Number of coils (1 is OH coil)
    number_of_coils = 0
    for item in mfile_data.data.keys():
        if "rpf(" in item:
            number_of_coils += 1

    # Get OH coil
    # coils_r.append(mfile_data.data["rpf(nohc)"].get_scan(scan))
    # coils_z.append(mfile_data.data["zpf(nohc)"].get_scan(scan))
    # coils_dr.append(mfile_data.data["ohdr"].get_scan(scan))
    # coils_dz.append(mfile_data.data["ohdz"].get_scan(scan))
    bore  = mfile_data.data["bore"].get_scan(scan)
    ohcth = mfile_data.data["ohcth"].get_scan(scan)
    ohdz = mfile_data.data["ohdz"].get_scan(scan)
    #coil_text.append("CS")

    # Rest of the coils
    for coil in range(1, number_of_coils):
        coils_r.append(mfile_data.data["rpf({:02})".format(coil)].
                       get_scan(scan))
        coils_z.append(mfile_data.data["zpf({:02})".format(coil)].
                       get_scan(scan))
        coils_dr.append(mfile_data.data["pfdr{:02}".format(coil)].
                        get_scan(scan))
        coils_dz.append(mfile_data.data["pfdz{:02}".format(coil)].
                        get_scan(scan))
        coil_text.append(str(coil))

    for i in range(len(coils_r)):
        r_1 = coils_r[i] - 0.5 * coils_dr[i]
        z_1 = coils_z[i] - 0.5 * coils_dz[i]
        r_2 = coils_r[i] - 0.5 * coils_dr[i]
        z_2 = coils_z[i] + 0.5 * coils_dz[i]
        r_3 = coils_r[i] + 0.5 * coils_dr[i]
        z_3 = coils_z[i] + 0.5 * coils_dz[i]
        r_4 = coils_r[i] + 0.5 * coils_dr[i]
        z_4 = coils_z[i] - 0.5 * coils_dz[i]
        r_5 = coils_r[i] - 0.5 * coils_dr[i]
        z_5 = coils_z[i] - 0.5 * coils_dz[i]

        r_points = [r_1, r_2, r_3, r_4, r_5]
        z_points = [z_1, z_2, z_3, z_4, z_5]
        axis.plot(r_points, z_points, color='black')
        rect = patches.Rectangle([bore, -ohdz/2], ohcth, ohdz, lw=0, facecolor='pink')
        axis.add_patch(rect)
        axis.text(coils_r[i], coils_z[i], coil_text[i],
                  ha='center', va='center', fontsize='smaller')


def plot_info(axis, data, mfile_data, scan):
    """Function to plot data in written form on a matplotlib plot.

    Arguments:
        axis --> axis object to plot to
        data --> plot information
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """
    eqpos = 0.7
    for i in range(len(data)):
        axis.text(0, -i, data[i][1], ha='left', va='center')
        if isinstance(data[i][0], str):
            if data[i][0] == "":
                axis.text(eqpos, -i, "\n",
                          ha='left', va='center')
            elif data[i][0][0] == "#":
                axis.text(-0.05, -i, "{}\n".format(data[i][0][1:]),
                          ha='left', va='center')
            elif data[i][0][0] == "!":
                value = data[i][0][1:]
                axis.text(0.4, -i, "-->  " + str(value.replace('"', '')) +
                          " " + data[i][2], ha='left', va='center')
            else:
                if mfile_data.data[data[i][0]].exists:
                    dat = mfile_data.data[data[i][0]].get_scan(scan)
                    if isinstance(dat, str):
                        value = dat
                    else:
                        value = "{:.4g}".format(mfile_data.data[data[i][0]].get_scan(scan))
                    if "alpha" in data[i][0]:
                        value = str(float(value) + 1.0)
                    axis.text(eqpos, -i, '= ' + value + ' ' + data[i][2],
                              ha='left', va='center')
                else:
                    mfile_data.data[data[i][0]].get_scan(-1)
                    axis.text(eqpos, -i, "=" + "ERROR! Var missing",
                              ha='left', va='center')
        else:
            dat = data[i][0]
            if isinstance(dat, str):
                value = dat
            else:
                value = "{:.4g}".format(data[i][0])
            axis.text(eqpos, -i, '= ' + value + ' ' + data[i][2],
                      ha='left', va='center')

def plot_header(axis, mfile_data, scan):
    """Function to plot header info: date, rutitle etc

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """
    xmin = 0
    xmax = 1
    ymin = -16
    ymax = 1

    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    data2 = [("!" + str(mfile_data.data["runtitle"].get_scan(scan)),
             "Run title", ""),
             ("!" + str(mfile_data.data["procver"].get_scan(scan)),
             "PROCESS Version", ""),
             ("!" + mfile_data.data["date"].get_scan(scan), "Date:", ""),
             ("!" + mfile_data.data["time"].get_scan(scan), "Time:", ""),
             ("!" + mfile_data.data["username"].get_scan(scan), "User:", ""),
             ("!" + proc_dict.DICT_OPTIMISATION_VARS
             [abs(int(mfile_data.data["minmax"].get_scan(scan)))],
             "Optimising:", "")]

    if mfile_data.data["imprad model"].get_scan(scan) == 1:
        H = mfile_data.data["fimp(01"].get_scan(scan)
        He = mfile_data.data["fimp(02"].get_scan(scan)
        Be = mfile_data.data["fimp(03"].get_scan(scan)
        C = mfile_data.data["fimp(04"].get_scan(scan)
        N = mfile_data.data["fimp(05"].get_scan(scan)
        O = mfile_data.data["fimp(06"].get_scan(scan)
        Ne = mfile_data.data["fimp(07"].get_scan(scan)
        Si = mfile_data.data["fimp(08"].get_scan(scan)
        Ar = mfile_data.data["fimp(09"].get_scan(scan)
        Fe = mfile_data.data["fimp(10"].get_scan(scan)
        Ni = mfile_data.data["fimp(11"].get_scan(scan)
        Kr = mfile_data.data["fimp(12"].get_scan(scan)
        Xe = mfile_data.data["fimp(13"].get_scan(scan)
        W = mfile_data.data["fimp(14"].get_scan(scan)

        data = [("", "", ""), ("", "", "")]
        count = 0

        data = data +[(H, "D + T", "")]
        count += 1

        data = data +[(He, "He", "")]
        count += 1
        if Be > 1e-10:
            data = data +[(Be, "Be", "")]
            count += + 1
        if C > 1e-10:
            data = data +[(C, "C", "")]
            count += 1
        if N > 1e-10:
            data = data +[(N, "N", "")]
            count += 1
        if O > 1e-10:
            data = data +[(O, "O", "")]
            count += 1
        if Ne > 1e-10:
            data = data +[(Ne, "Ne", "")]
            count += 1
        if Si > 1e-10:
            data = data +[(Si, "Si", "")]
            count += 1
        if Ar > 1e-10:
            data = data +[(Ar, "Ar", "")]
            count += 1
        if Fe > 1e-10:
            data = data +[(Fe, "Fe", "")]
            count += 1
        if Ni > 1e-10:
            data = data +[(Ni, "Ni", "")]
            count += 1
        if Kr > 1e-10:
            data = data +[(Kr, "Kr", "")]
            count += 1
        if Xe > 1e-10:
            data = data +[(Xe, "Xe", "")]
            count += 1
        if W > 1e-10:
            data = data +[(W, "W", "")]
            count += 1

        if count > 11:
            data = [("", "", ""), ("", "", ""), ("", "More than 11 impurities", "")]
        else:
            axis.text(-0.05, -6.4, 'Plasma composition:', ha='left',
                      va='center')
            axis.text(-0.05, -7.2,
                      'Number densities relative to electron density:',
                      ha='left', va='center')
        data2 = data2 + data

    plot_info(axis, data2, mfile_data, scan)


def plot_geometry_info(axis, mfile_data, scan):
    """Function to plot geometry info

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """
    xmin = 0
    xmax = 1
    ymin = -16
    ymax = 1

    axis.text(-0.05, 1, 'Geometry:', ha='left', va='center')
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    in_blanket_thk = mfile_data.data["shldith"].get_scan(scan) + \
                     mfile_data.data["blnkith"].get_scan(scan)
    out_blanket_thk = mfile_data.data["shldoth"].get_scan(scan) + \
                      mfile_data.data["blnkoth"].get_scan(scan)

    data = [("rmajor", "$R_0$", "m"),
            ("rminor", "a", "m"),
            ("aspect", "A", ""),
            ("kappa95", "$\kappa_{95}$", ""),
            ("triang95", "$\delta_{95}$", ""),
            ("sarea", "Surface area", "m$^2$"),
            ("vol", "Plasma volume", "m$^3$"),
            ("tfno", "No. of TF coils", ""),
            (in_blanket_thk, "inboard blanket+shield", "m"),
            (out_blanket_thk, "ouboard blanket+shield", "m"),
            ("powfmw", "Fusion power", "MW"),
            ("", "", "")]

    plot_info(axis, data, mfile_data, scan)


def plot_physics_info(axis, mfile_data, scan):
    """Function to plot geometry info

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """
    xmin = 0
    xmax = 1
    ymin = -16
    ymax = 1

    axis.text(-0.05, 1, 'Physics:', ha='left', va='center')
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    nong = mfile_data.data["dnla"].get_scan(scan) / \
           mfile_data.data["dlimit(7)"].get_scan(scan)

    dnz = mfile_data.data["dnz"].get_scan(scan) / \
          mfile_data.data["dene"].get_scan(scan)

    tepeak = mfile_data.data["te0"].get_scan(scan) / \
        mfile_data.data["te"].get_scan(scan)

    nepeak = mfile_data.data["ne0"].get_scan(scan) / \
        mfile_data.data["dene"].get_scan(scan)

    data = [("plascur/1d6", "$I_p$", "MA"),
            ("bt", "Vacuum $B_T$ at $R_0$", "T"),
            ("q95", "$q_{\mathrm{95}}$", ""),
            ("normalised thermal beta", r"$\beta_N$, thermal",
             "% m T MA$^{-1}$"),
            ("normalised total beta", r"$\beta_N$, total", "% m T MA$^{-1}$"),
            ("thermal poloidal beta", r"$\beta_P$, thermal", ""),
            ("betap", r"$\beta_P$, total", ""),
            ("te", r"$< t_e >$", "keV"),
            ("dene", r"$< n_e >$", "m$^{-3}$"),
            (nong, r"$< n_{\mathrm{e,line}} >/n_G$", ""),
            (tepeak, r"$T_{e0}/ < T_e >$", ""),
            (nepeak, r"$n_{e0}/ < n_{\mathrm{e, vol}} >$", ""),
            ("zeff", r"$Z_{\mathrm{eff}}$", ""),
            ("zeffso", r"$Z_{\mathrm{eff, SoL}}$", ""),
            (dnz, r"$n_Z/ < n_{\mathrm{e, vol}} >$", ""),
            ("taueff", r"$\tau_e$", "s"),
            ("hfact", "H-factor", ""),
            ("tauelaw", "Scaling law", "")]

    plot_info(axis, data, mfile_data, scan)


def plot_magnetics_info(axis, mfile_data, scan):
    """Function to plot magnet info

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """

    xmin = 0
    xmax = 1
    ymin = -16
    ymax = 1

    axis.text(-0.05, 1, 'Coil currents etc:', ha='left', va='center')
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    # Number of coils (1 is OH coil)
    number_of_coils = 0
    for item in mfile_data.data.keys():
        if "rpf(" in item:
            number_of_coils += 1

    pf_info = []
    for i in range(1, number_of_coils):
        if i % 2 != 0:
            pf_info.append((mfile_data.data["ric({:02})".format(i)].
                            get_scan(scan), "PF {}".format(i)))

    if len(pf_info) > 2:
        pf_info_3_a = pf_info[2][0]
        pf_info_3_b = pf_info[2][1]
    else:
        pf_info_3_a = ""
        pf_info_3_b = ""


    tburn = mfile_data.data["tburn"].get_scan(scan) / 3600.0
    tftype = proc_dict.DICT_TF_TYPE[mfile_data.data["isumattf"].get_scan(scan)]
    vssoft = mfile_data.data["vsres"].get_scan(scan) + \
             mfile_data.data["vsind"].get_scan(scan)

    data = [(pf_info[0][0], pf_info[0][1], "MA"),
            (pf_info[1][0], pf_info[1][1], "MA"),
            (pf_info_3_a, pf_info_3_b, "MA"),
            (vssoft, "Startup flux swing", "Wb"),
            ("vstot", "Available flux swing", "Wb"),
            (tburn, "Burn time", "hrs"),
            ("", "", ""),
            ("#TF coil type is {}".format(tftype), "", ""),
            ("bmaxtfrp", "Peak field at conductor (w. rip.)", "T"),
            ("iooic", "I/I$_{\mathrm{crit}}$", ""),
            ("tmarg", "Temperature margin", "K"),
            ("strtf1", "Conduit Von Mises stress", "Pa"),
            ("strtf2", "Case Von Mises stress", "Pa"),
            ("alstrtf", "Allowable stress", "Pa"),
            ("", "", ""),
            ("#Costs", "", ""),
            ("coe", "Cost of electricity", "\$/MWh")]

    plot_info(axis, data, mfile_data, scan)


def plot_power_info(axis, mfile_data, scan):
    """Function to plot power info

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """

    xmin = 0
    xmax = 1
    ymin = -16
    ymax = 1

    axis.text(-0.05, 1, 'Power flows:', ha='left', va='center')
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    dnla = mfile_data.data["dnla"].get_scan(scan) / 1.0e20
    bt = mfile_data.data["bt"].get_scan(scan)
    surf = mfile_data.data["sarea"].get_scan(scan)
    pthresh = mfile_data.data["pthrmw(6)"].get_scan(scan)
    err = pthresh - mfile_data.data["pthrmw(7)"].get_scan(scan)

    gross_eff = 100.0 * (mfile_data.data["pgrossmw"].get_scan(scan) /
                         mfile_data.data["pthermmw"].get_scan(scan))

    net_eff = 100.0 * ((mfile_data.data["pgrossmw"].get_scan(scan) -
                        mfile_data.data["htpmw"].get_scan(scan)) /
                       (mfile_data.data["pthermmw"].get_scan(scan) -
                        mfile_data.data["htpmw"].get_scan(scan)))

    plant_eff = 100.0 * (mfile_data.data["pnetelmw"].get_scan(scan) /
                         mfile_data.data["powfmw"].get_scan(scan))

    imprad = mfile_data.data["imprad model"].get_scan(scan)


    # Define appropriate pedestal and impurity parameters
    if imprad == 1:
        coredescription = ("coreradius", "Normalised radius of 'core' region", "")
        impurity = ("coreradius", "Normalised radius of 'core' region", "")
    else:
        coredescription = ("", "Simple radiation model used", "")
    if ipedestal == 1:
        ped_height = ("neped", "Electron density at pedestal", "m$^{-3}$")
        ped_pos =  ("rhopedn", "r/a at density pedestal", "")
    else:
        ped_height = ("", "No pedestal model used", "")
        ped_pos = ("", "", "")

    dnalp = mfile_data.data["dnalp"].get_scan(scan)
    dene = mfile_data.data["dene"].get_scan(scan)
    ralpne = dnalp/dene

    data = [("wallmw", "Nominal neutron wall load", "MW m$^{-2}$"),
            coredescription,
            ped_height,
            ped_pos,
            ("ralpne", "Helium fraction", ""),
            ("pcoreradmw", "Core radiation", "MW"),
            ("pradmw", "Total radiation", "MW"),
            ("pnucblkt", "Nuclear heating in blanket", "MW"),
            ("pnucshld", "Nuclear heating in shield", "MW"),
            ("pdivt", "Power to divertor", "MW"),
            (pthresh, "H-mode threshold", "MW"),
            ("divlife", "Divertor life", "years"),
            ("pthermmw", "Primary (high grade) heat", "MW"),
            (gross_eff, "Gross cycle efficiency", "%"),
            (net_eff, "Net cycle efficiency", "%"),
            ("pgrossmw", "Gross electric power", "MW"),
            ("pnetelmw", "Net electric power", "MW"),
            (plant_eff, "Fusion-to-electric efficiency " +
             r"$\frac{P_{\mathrm{e,net}}}{P_{\mathrm{fus}}}$", "%")]

    plot_info(axis, data, mfile_data, scan)


def plot_current_drive_info(axis, mfile_data, scan):
    """Function to plot current drive info

    Arguments:
        axis --> axis object to plot to
        mfile_data --> MFILE.DAT object
        scan --> scan number to use

    """

    xmin = 0
    xmax = 1
    ymin = -16
    ymax = 1

    axis.text(-0.05, 1, 'Neutral Beam Current Drive:', ha='left', va='center')
    axis.set_ylim([ymin, ymax])
    axis.set_xlim([xmin, xmax])
    axis.set_axis_off()
    axis.set_autoscaley_on(False)
    axis.set_autoscalex_on(False)

    pinjie = mfile_data.data["pinjmw"].get_scan(scan)

    pdivt = mfile_data.data["pdivt"].get_scan(scan)
    pdivr = pdivt / mfile_data.data["rmajor"].get_scan(scan)

    pdivnr = 10.0e20 * mfile_data.data["pdivt"].get_scan(scan) / \
             (mfile_data.data["rmajor"].get_scan(scan) *
              mfile_data.data["dene"].get_scan(scan))

    dnla = mfile_data.data["dnla"].get_scan(scan) / 1.0e20
    bt = mfile_data.data["bt"].get_scan(scan)
    surf = mfile_data.data["sarea"].get_scan(scan)
    pthresh = mfile_data.data["pthrmw(6)"].get_scan(scan)
    flh = pdivt / pthresh

    powerht = mfile_data.data["powerht"].get_scan(scan)
    psync = mfile_data.data["psyncpv*vol"].get_scan(scan)
    pbrem = mfile_data.data["pcoreradmw"].get_scan(scan)
    hfact = mfile_data.data["hfact"].get_scan(scan)
    hstar = hfact * (powerht / (powerht + psync + pbrem)) ** 0.31

    data = [(pinjie, "Steady state auxiliary power", "MW"),
            ("pheat", "Power for heating only", "MW"),
            ("bootipf", "Bootstrap fraction", ""),
            ("faccd", "Auxiliary fraction", ""),
            ("facoh", "Inductive fraction", ""),
            ("gamnb", "NB gamma", "$10^{20}$ A W$^{-1}$ m$^{-2}$"),
            ("enbeam", "NB energy", "keV"),
            ("powerht", "Plasma heating used for H factor", "MW"),
            (pdivr, r"$\frac{P_{\mathrm{div}}}{R_{0}}$", "MW m$^{-1}$"),
            (pdivnr, r"$\frac{P_{\mathrm{div}}}{<n> R_{0}}$",
             r"$\times 10^{-20}$ MW m$^{2}$"),
            (flh, r"$\frac{P_{\mathrm{div}}}{P_{\mathrm{LH}}}$", ""),
            (hstar, "H* (non-rad. corr.)", "")]

    plot_info(axis, data, mfile_data, scan)


def main(fig1, fig2, m_file_data, scan=-1):
    """Function to create radial and vertical build plot on given figure.

    Arguments:
      fig1 --> figure object to add plot to.
      fig2 --> figure object to add plot to.
      m_file_data --> MFILE.DAT data to read
      scan --> scan to read from MFILE.DAT

    """

    # Plot poloidal cross-section
    plot_1 = fig2.add_subplot(221, aspect='equal')
    poloidal_cross_section(plot_1, m_file_data, scan)

    # Plot TF coils
    plot_tf_coils(plot_1, m_file_data, scan)

    # Plot PF coils
    plot_pf_coils(plot_1, m_file_data, scan)



    # Plot toroidal cross-section
    plot_2 = fig2.add_subplot(222, aspect='equal')
    #toroidal_cross_section(plot_2)
    toroidal_cross_section(plot_2, m_file_data, scan=-1)

    # Plot color key
    plot_3 = fig2.add_subplot(241)
    color_key(plot_3)

    # Plot profiles
    plot_4 = fig2.add_subplot(223, aspect= 0.05)
    plot_nprofile(plot_4)

    plot_5 = fig2.add_subplot(224, aspect= 1/35)
    plot_tprofile(plot_5)



    # Setup params for text plots
    plt.rcParams.update({'font.size': 8})

    # Plot header info
    plot_1 = fig1.add_subplot(231)
    plot_header(plot_1, m_file_data, scan)

    # Geometry
    plot_2 = fig1.add_subplot(232)
    plot_geometry_info(plot_2, m_file_data, scan)

    # Physics
    plot_3 = fig1.add_subplot(233)
    plot_physics_info(plot_3, m_file_data, scan)

    # Magnetics
    plot_4 = fig1.add_subplot(234)
    plot_magnetics_info(plot_4, m_file_data, scan)

    # power/flow economics
    plot_5 = fig1.add_subplot(235)
    plot_power_info(plot_5, m_file_data, scan)

    # Current drive
    plot_6 = fig1.add_subplot(236)
    plot_current_drive_info(plot_6, m_file_data, scan)

    fig1.subplots_adjust(wspace=0.25)


def save_plots(m_file_data, scan=-1):
    """Function to recreate and save individual plots.
    """

    fig = plt.figure(figsize=(12,9), dpi=80)

    # Plot poloidal cross-section
    pol = fig.add_subplot(111, aspect='equal')
    poloidal_cross_section(pol, m_file_data, scan)

    # Plot TF coils
    plot_tf_coils(pol, m_file_data, scan)

    # Plot PF coils
    plot_pf_coils(pol, m_file_data, scan)

    fig.savefig('psection.svg', format='svg', dpi=1200)


    # Plot toroidal cross-section
    fig = plt.figure(figsize=(12,9), dpi=80)
    tor = fig.add_subplot(222, aspect='equal')
    toroidal_cross_section(tor)
    fig.savefig('tsection.svg', format='svg', dpi=1200)

    # Plot color key
    fig = plt.figure(figsize=(12,9), dpi=80)
    plot = fig.add_subplot(241)
    color_key(plot)
    fig.savefig('color_key.svg', format='svg', dpi=1200)


    # Plot profiles
    fig = plt.figure(figsize=(12,9), dpi=80)
    plot = fig.add_subplot(223, aspect= 0.05)
    plot_nprofile(plot)
    fig.savefig('nprofile.svg', format='svg', dpi=1200)

    fig = plt.figure(figsize=(12,9), dpi=80)
    plot = fig.add_subplot(224, aspect= 1/35)
    plot_tprofile(plot)
    fig.savefig('tprofile.svg', format='svg', dpi=1200)


def test(f):
    """Test Function

    :param f: filename to test
    """

    try:
        # read MFILE
        m_file = mf.MFile(filename=f)
        scan = -1

        global bore
        bore = m_file.data["bore"].get_scan(scan)
        global ohcth
        ohcth = m_file.data["ohcth"].get_scan(scan)
        global gapoh
        gapoh = m_file.data["gapoh"].get_scan(scan)
        global tfcth
        tfcth = m_file.data["tfcth"].get_scan(scan)
        global gapds
        gapds = m_file.data["gapds"].get_scan(scan)
        global ddwi
        ddwi = m_file.data["ddwi"].get_scan(scan)
        global shldith
        shldith = m_file.data["shldith"].get_scan(scan)
        global blnkith
        blnkith = m_file.data["blnkith"].get_scan(scan)
        global fwith
        fwith = m_file.data["fwith"].get_scan(scan)
        global scrapli
        scrapli = m_file.data["scrapli"].get_scan(scan)
        global rmajor
        rmajor = m_file.data["rmajor"].get_scan(scan)
        global rminor
        rminor = m_file.data["rminor"].get_scan(scan)
        global scraplo
        scraplo = m_file.data["scraplo"].get_scan(scan)
        global fwoth
        fwoth = m_file.data["fwoth"].get_scan(scan)
        global blnkoth
        blnkoth = m_file.data["blnkoth"].get_scan(scan)
        global shldoth
        shldoth = m_file.data["shldoth"].get_scan(scan)
        # ddwi = m_file.data["ddwi"].get_scan(scan)
        global gapsto
        gapsto = m_file.data["gapsto"].get_scan(scan)
        global tfthko
        tfthko = m_file.data["tfthko"].get_scan(scan)
        global rdewex
        rdewex = m_file.data["rdewex"].get_scan(scan)
        global ddwex
        ddwex = m_file.data["ddwex"].get_scan(scan)
        global tfno
        tfno = m_file.data["tfno"].get_scan(scan)
        global wwp1
        wwp1 = m_file.data["wwp1"].get_scan(scan)
        global wwp2
        wwp2 = m_file.data["wwp2"].get_scan(scan)
        global thkwp
        thkwp = m_file.data["thkwp"].get_scan(scan)
        global tinstf
        tinstf = m_file.data["tinstf"].get_scan(scan)
        global thkcas
        thkcas = m_file.data["thkcas"].get_scan(scan)
        global nbshield
        nbshield = m_file.data["nbshield"].get_scan(scan)
        global rtanbeam
        rtanbeam = m_file.data["rtanbeam"].get_scan(scan)
        global rtanmax
        rtanmax = m_file.data["rtanmax"].get_scan(scan)
        global beamwd
        beamwd = m_file.data["beamwd"].get_scan(scan)
        global casthi
        casthi = m_file.data["casthi"].get_scan(scan)
        # # Pedestal profile parameters
        global ipedestal
        ipedestal = m_file.data["ipedestal"].get_scan(scan)
        global neped
        neped = m_file.data["neped"].get_scan(scan)
        global nesep
        nesep = m_file.data["nesep"].get_scan(scan)
        global rhopedn
        rhopedn = m_file.data["rhopedn"].get_scan(scan)
        global rhopedt
        rhopedt = m_file.data["rhopedt"].get_scan(scan)
        global tbeta
        tbeta = m_file.data["tbeta"].get_scan(scan)
        global teped
        teped = m_file.data["teped"].get_scan(scan)
        global tesep
        tesep = m_file.data["tesep"].get_scan(scan)
        global alphan
        alphan = m_file.data["alphan"].get_scan(scan)
        global alphat
        alphat = m_file.data["alphat"].get_scan(scan)
        global ne0
        ne0 = m_file.data["ne0"].get_scan(scan)
        global te0
        te0 = m_file.data["te0"].get_scan(scan)
        # # Plasma
        global triang
        triang = m_file.data["triang95"].get_scan(scan)

        # create main plot
        page1 = plt.figure(figsize=(12, 9), dpi=80)
        page2 = plt.figure(figsize=(12, 9), dpi=80)

        # run main
        main(page1, page2, m_file)

        # with bpdf.PdfPages(args.o) as pdf:
        with bpdf.PdfPages("ref.SUMMARY.pdf") as pdf:
            pdf.savefig(page1)
            pdf.savefig(page2)

        return True
    except:
        return False


if __name__ == '__main__':

    # Setup command line arguments
    parser = argparse. \
        ArgumentParser(description="Produces a two page summary of the PROCESS MFILE output, using the MFILE.  "
        "For info contact rich.kemp@ccfe.ac.uk or james.morris2@ccfe.ac.uk")

    parser.add_argument("-f", metavar='FILENAME', type=str,
                        default="", help='specify filename prefix')

    parser.add_argument("-s", "--show", help="show plot as well as saving figure",
                        action="store_true")

    parser.add_argument("--svg", help="save plots as svg files",
                        action="store_true")

    args = parser.parse_args()

    # read MFILE
    m_file = mf.MFile(args.f + "MFILE.DAT")
    scan = -1
    bore = m_file.data["bore"].get_scan(scan)
    ohcth = m_file.data["ohcth"].get_scan(scan)
    gapoh = m_file.data["gapoh"].get_scan(scan)
    tfcth = m_file.data["tfcth"].get_scan(scan)
    gapds = m_file.data["gapds"].get_scan(scan)
    ddwi = m_file.data["ddwi"].get_scan(scan)
    shldith = m_file.data["shldith"].get_scan(scan)
    blnkith = m_file.data["blnkith"].get_scan(scan)
    fwith = m_file.data["fwith"].get_scan(scan)
    scrapli = m_file.data["scrapli"].get_scan(scan)
    rmajor = m_file.data["rmajor"].get_scan(scan)
    rminor = m_file.data["rminor"].get_scan(scan)
    scraplo = m_file.data["scraplo"].get_scan(scan)
    fwoth = m_file.data["fwoth"].get_scan(scan)
    blnkoth = m_file.data["blnkoth"].get_scan(scan)
    shldoth = m_file.data["shldoth"].get_scan(scan)
    ddwi = m_file.data["ddwi"].get_scan(scan)
    gapsto = m_file.data["gapsto"].get_scan(scan)
    tfthko = m_file.data["tfthko"].get_scan(scan)
    rdewex = m_file.data["rdewex"].get_scan(scan)
    zdewex = m_file.data["zdewex"].get_scan(scan)    
    ddwex = m_file.data["ddwex"].get_scan(scan)
    tfno = m_file.data["tfno"].get_scan(scan)
    wwp1 = m_file.data["wwp1"].get_scan(scan)
    wwp2 = m_file.data["wwp2"].get_scan(scan)
    thkwp = m_file.data["thkwp"].get_scan(scan)
    tinstf = m_file.data["tinstf"].get_scan(scan)
    thkcas = m_file.data["thkcas"].get_scan(scan)
    nbshield = m_file.data["nbshield"].get_scan(scan)
    rtanbeam = m_file.data["rtanbeam"].get_scan(scan)
    rtanmax = m_file.data["rtanmax"].get_scan(scan)
    beamwd = m_file.data["beamwd"].get_scan(scan)
    casthi = m_file.data["casthi"].get_scan(scan)
    # Pedestal profile parameters
    ipedestal = m_file.data["ipedestal"].get_scan(scan)
    neped = m_file.data["neped"].get_scan(scan)
    nesep = m_file.data["nesep"].get_scan(scan)
    rhopedn = m_file.data["rhopedn"].get_scan(scan)
    rhopedt = m_file.data["rhopedt"].get_scan(scan)
    tbeta = m_file.data["tbeta"].get_scan(scan)
    teped = m_file.data["teped"].get_scan(scan)
    tesep = m_file.data["tesep"].get_scan(scan)
    alphan = m_file.data["alphan"].get_scan(scan)
    alphat = m_file.data["alphat"].get_scan(scan)
    ne0 = m_file.data["ne0"].get_scan(scan)
    te0 = m_file.data["te0"].get_scan(scan)
    # Plasma
    triang = m_file.data["triang95"].get_scan(scan)

# Build the dictionaries of radial and vertical build values and cumulative values
    radial = {} ; cumulative_radial = {}; subtotal = 0
    for item in RADIAL_BUILD:
        if item == "rminori" or item == "rminoro":
            build = m_file.data["rminor"].get_scan(scan)
        elif item == "vvblgapi" or item == "vvblgapo":
            build = m_file.data["vvblgap"].get_scan(scan)            
        elif item == "thshieldi" or item == "thshieldo":
            build = m_file.data["thshield"].get_scan(scan)            
        elif "ddw" in item:
            build = m_file.data["ddwi"].get_scan(scan)
        else:
            build = m_file.data[item].get_scan(scan)
        
    radial[item] = build
    subtotal += build
    cumulative_radial[item] = subtotal
    
    upper = {} ; cumulative_upper = {}; subtotal = 0
    for item in vertical_upper:
        upper[item] = m_file.data[item].get_scan(scan)
        subtotal +=upper[item]
        cumulative_upper[item] = subtotal

    lower = {} ; cumulative_lower = {}; subtotal = 0
    for item in vertical_lower:
        lower[item] = m_file.data[item].get_scan(scan)
        subtotal -=lower[item]
        cumulative_lower[item] = subtotal       
    
    colour_dict = {}
    colour_dict['ohcth'] = solenoid
    colour_dict['tfcth'] = tfc
    colour_dict['thshield'] = thermal_shield
    colour_dict['ddwi'] = vessel
    colour_dict['shldith'] = shield
    colour_dict['blnkith'] = blanket
    colour_dict['rminor'] = plasma
    colour_dict['fwith'] = firstwall
    colour_dict['fwoth'] = firstwall

    # read MFILE
    # m_file = mf.MFile(args.f)
    scan = -1

    # create main plot
    page1 = plt.figure(figsize=(12, 9), dpi=80)
    page2 = plt.figure(figsize=(12, 9), dpi=80)

    # run main
    main(page1, page2, m_file)

    # with bpdf.PdfPages(args.o) as pdf:
    with bpdf.PdfPages(args.f + "SUMMARY.pdf") as pdf:
        pdf.savefig(page1)
        pdf.savefig(page2)

    # show fig if option used
    if args.show:
        plt.show(page1)
        plt.show(page2)

    # This bit doesn't work - the argument is not recognised for some reason.:
    if args.svg:
        save_plots(m_file)
