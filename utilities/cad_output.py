"""

  PROCESS to CAD program

  James Morris
  30/07/2014

"""

# Import scientific python library
import scipy

# Import MFILE library for reading MFILE.DAT
import process_io_lib.mfile as mf

# Import argument parser
import argparse

# Import matplotlib libraries for plotting
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse, Arc


class BuildDataContainer(object):
    def __init__(self, data):
        """Setup build data container
        """

        # Initialise plot switch
        self.plot = False

        # Initialise plot alpha
        self.alpha = 1.0

        # Mfile
        self.mfile = data

        # Major Radius
        self.r0 = data.data["rmajor"].get_scan(-1)

        # Minor Radius
        self.a = data.data["rminor"].get_scan(-1)

        # Machine bore
        self.bore = data.data["bore"].get_scan(-1)

        # OH coil radial thickness
        self.ohcth = data.data["ohcth"].get_scan(-1)

        # Radial gap between OH coil and TF coil inboard side
        self.gapoh = data.data["gapoh"].get_scan(-1)

        # Inboard TF coil leg radial thickness
        self.tfcth = data.data["tfcth"].get_scan(-1)

        # Radial gap between inboard TF coil and vacuum vessel
        self.gapds = data.data["gapds"].get_scan(-1)

        # Thickness of vacuum vessel
        self.ddwi = data.data["ddwi"].get_scan(-1)

        # Inboard shiled radial thickness
        self.shldith = data.data["shldith"].get_scan(-1)

        # Inboard blanket radial thickness
        self.blnkith = data.data["blnkith"].get_scan(-1)

        # Inboard first wall radial thickness
        self.fwith = data.data["fwith"].get_scan(-1)

        # Inboard scrape off layer radial thickness
        self.scrapli = data.data["scrapli"].get_scan(-1)

        # Outboard scrape off layer radial thickness
        self.scraplo = data.data["scraplo"].get_scan(-1)

        # Outboard first wall radial thickness
        self.fwoth = data.data["fwoth"].get_scan(-1)

        # Outboard blanket radial thickness
        self.blnkoth = data.data["blnkoth"].get_scan(-1)

        # Outboard shield radial thickness
        self.shldoth = data.data["shldoth"].get_scan(-1)

        # Radial gap between vacuum vessel and outboard TF leg
        self.gapsto = data.data["gapsto"].get_scan(-1)

        # Outboard TF coil radial thickness
        self.tfthko = data.data["tfthko"].get_scan(-1)

        # Maximum half-height of TF coil inside edge
        self.hmax = data.data["hmax"].get_scan(-1)

        # Plasma triangularity (95%)
        self.delta95 = data.data["triang95"].get_scan(-1)

        # Plasma elongation (95%)
        self.kappa95 = data.data["kappa95"].get_scan(-1)

        # Plasma triangularity
        self.delta = data.data["triang"].get_scan(-1)

        # Plasma elongation
        self.kappa = data.data["kappa"].get_scan(-1)

        # Vertical gap between vacuum vessel and TF coil
        self.vgap2 = data.data["vgap2"].get_scan(-1)

        # Vertical gap between x-point and divertor
        self.vgap = data.data["vgap"].get_scan(-1)

        # Plasma half-height (not 95% surface values)
        self.plasma_half_height = self.kappa*self.a

        # Upper first wall vertical thickness
        self.topfw = (self.fwith + self.fwoth)/2.0

        # Upper blanket vertical thickness
        self.blnktth = data.data["blnktth"].get_scan(-1)

        # Shield vertical thickness
        self.shldtth = data.data["shldtth"].get_scan(-1)

        # Divertor structure
        self.divfix = data.data["divfix"].get_scan(-1)

        # Cryostat radius
        self.rdewex = data.data["rdewex"].get_scan(-1)

        # Cryostat half-height
        self.zdewex = data.data["zdewex"].get_scan(-1)

        # Number of TF coils
        self.tfno = data.data["tfno"].get_scan(-1)

        # Toroidal thickness of TF coil outer leg
        self.tftort = data.data["tftort"].get_scan(-1)

        # TF coil inboard leg inner case radial thickness
        self.thkcas = data.data["thkcas"].get_scan(-1)

        # TF coil winding pack radial thickness
        self.thkwp = data.data["thkwp"].get_scan(-1)

        # TF coil inboard leg plasma facing case radial thickness
        self.casthi = data.data["casthi"].get_scan(-1)

        # TF coil inboard leg sidewall case thickness
        self.casths = data.data["casths"].get_scan(-1)

        # 

        # TF Coil arcs

        # Arc 1
        self.xarc_1 = data.data["xarc(1)"].get_scan(-1)
        self.yarc_1 = data.data["yarc(1)"].get_scan(-1)
        self.xctfc_1 = data.data["xctfc(1)"].get_scan(-1)
        self.yctfc_1 = data.data["yctfc(1)"].get_scan(-1)
        self.xarc_2 = data.data["xarc(2)"].get_scan(-1)
        self.yarc_2 = data.data["yarc(2)"].get_scan(-1)

        # Arc 2
        self.xctfc_2 = data.data["xctfc(2)"].get_scan(-1)
        self.yctfc_2 = data.data["yctfc(2)"].get_scan(-1)
        self.xarc_3 = data.data["xarc(3)"].get_scan(-1)
        self.yarc_3 = data.data["yarc(3)"].get_scan(-1)

        # Arc 3
        self.xctfc_3 = data.data["xctfc(3)"].get_scan(-1)
        self.yctfc_3 = data.data["yctfc(3)"].get_scan(-1)
        self.xarc_4 = data.data["xarc(4)"].get_scan(-1)
        self.yarc_4 = data.data["yarc(4)"].get_scan(-1)

        # Arc 4
        self.xctfc_4 = data.data["xctfc(4)"].get_scan(-1)
        self.yctfc_4 = data.data["yctfc(4)"].get_scan(-1)
        self.xarc_5 = data.data["xarc(5)"].get_scan(-1)
        self.yarc_5 = data.data["yarc(5)"].get_scan(-1)


def write_machine_bore(data, output_file):
    """Function to write data for the machine bore
    """

    # Description line
    output_file.write("# Machine Bore (Cylinder)\n")

    # Bore radial thickness
    bore_thickness = data.bore
    line_1 = "Bore thickness = " + str(bore_thickness) + " m\n"

    # Half-height from midplane. Multiplied by 0.9 because actually need
    # variable "ohhghf" but it isn't in MFILE.
    # TODO: put ohhghf in here when it is available in MFILE
    bore_half_height = data.hmax*0.9
    line_2 = "Bore half-height from mid-plane = " + str(bore_half_height) + \
             " m\n"

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write("\n")


def write_oh_coil(data, output_file):
    """Function to write data for the oh coil
    """

    # Description line
    output_file.write("# OH Coil (Cylinder)\n")

    # OH coil radial thickness
    oh_thickness = data.ohcth
    line_1 = "OH coil thickness = " + str(oh_thickness) + " m\n"

    # Half-height from midplane. Multiplied by 0.9 because actually need
    # variable "ohhghf" but it isn't in MFILE.
    # TODO: put ohhghf in here when it is available in MFILE
    oh_half_height = data.hmax*0.9
    line_2 = "OH coil half-height from mid-plane = " \
             + str(oh_half_height) + " m\n"

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        x = [data.bore, data.bore + oh_thickness, data.bore + oh_thickness,
             data.bore, data.bore]
        y = [-oh_half_height, -oh_half_height, oh_half_height, oh_half_height,
             -oh_half_height]
        ax.fill(x, y, "r")


def write_oh_gap(data, output_file):
    """Function to write data for the gap between the OH coil and the TF coil.
    """

    # Description line
    output_file.write("# OH to TF gap (Cylinder)\n")

    # OH coil radial thickness
    oh_gap_thickness = data.gapoh
    line_1 = "OH to TF coil gap thickness = " + str(oh_gap_thickness) + " m\n"

    # Half-height from midplane. Multiplied by 0.9 because actually need
    # variable "ohhghf" but it isn't in MFILE.
    # TODO: put ohhghf in here when it is available in MFILE
    oh_gap_half_height = data.hmax*0.9
    line_2 = "OH coil half-height from mid-plane = " \
             + str(oh_gap_half_height) + " m\n"

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write("\n")


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


def write_tf_coils(data, output_file):
    """Function to plot TF coils
    """

    # Arc points
    x_1 = data.__dict__["xarc_1"]
    y_1 = data.__dict__["yarc_1"]
    x_2 = data.__dict__["xarc_2"]
    y_2 = data.__dict__["yarc_2"]
    x_3 = data.__dict__["xarc_3"]
    y_3 = data.__dict__["yarc_3"]
    x_4 = data.__dict__["xarc_4"]
    y_4 = data.__dict__["yarc_4"]
    x_5 = data.__dict__["xarc_5"]

    # Arc centres
    x_c_1 = data.__dict__["xctfc_1"]
    y_c_1 = data.__dict__["yctfc_1"]
    x_c_2 = data.__dict__["xctfc_2"]
    y_c_2 = data.__dict__["yctfc_2"]
    x_c_3 = data.__dict__["xctfc_3"]
    y_c_3 = data.__dict__["yctfc_3"]
    x_c_4 = data.__dict__["xctfc_4"]
    y_c_4 = data.__dict__["yctfc_4"]

    # Arc radii
    rad_1 = scipy.sqrt((x_1-x_c_1)**2 + (y_1 - y_c_1)**2)
    rad_2 = scipy.sqrt((x_2-x_c_2)**2 + (y_2 - y_c_2)**2)
    rad_3 = scipy.sqrt((x_3-x_c_3)**2 + (y_3 - y_c_3)**2)
    rad_4 = scipy.sqrt((x_4-x_c_4)**2 + (y_4 - y_c_4)**2)

    # Arc angles
    a_1_1 = (x_1 - x_c_1)/rad_1
    a_1_2 = (x_2 - x_c_1)/rad_1
    a_1_1, a_1_2 = angle_check(a_1_1, a_1_2)
    angle_1_1 = scipy.arcsin(a_1_1)
    angle_1_2 = scipy.arcsin(a_1_2)

    a_2_1 = (x_2 - x_c_2)/rad_2
    a_2_2 = (x_3 - x_c_2)/rad_2
    a_2_1, a_2_2 = angle_check(a_2_1, a_2_2)
    angle_2_1 = scipy.arcsin(a_2_1)
    angle_2_2 = scipy.arcsin(a_2_2)

    a_3_1 = (x_3 - x_c_3)/rad_3
    a_3_2 = (x_4 - x_c_3)/rad_3
    a_3_1, a_3_2 = angle_check(a_3_1, a_3_2)
    angle_3_1 = scipy.arcsin(a_3_1)
    angle_3_2 = scipy.arcsin(a_3_2)

    a_4_1 = (x_4 - x_c_4)/rad_4
    a_4_2 = (x_5 - x_c_4)/rad_4
    a_4_1, a_4_2 = angle_check(a_4_1, a_4_2)
    angle_4_1 = scipy.arcsin(a_4_1)
    angle_4_2 = scipy.arcsin(a_4_2)

    in_x_1 = rad_1*scipy.sin(scipy.linspace(angle_1_1, angle_1_2, 30,
                             endpoint=True)) + x_c_1
    in_x_2 = rad_2*scipy.sin(scipy.linspace(angle_2_1, angle_2_2, 30,
                             endpoint=True)) + x_c_2
    in_x_3 = rad_3*scipy.sin(scipy.linspace(angle_3_1, angle_3_2, 30,
                             endpoint=True)) + x_c_3
    in_x_4 = rad_4*scipy.sin(scipy.linspace(angle_4_1, angle_4_2, 30,
                             endpoint=True)) + x_c_4
    in_x = scipy.concatenate((in_x_1, in_x_2, in_x_3, in_x_4))

    in_y_1 = rad_1*scipy.cos(scipy.linspace(angle_1_1, angle_1_2, 30,
                             endpoint=True)) + y_c_1
    in_y_2 = rad_2*scipy.cos(scipy.linspace(angle_2_1, angle_2_2, 30,
                             endpoint=True)) + y_c_2
    in_y_3 = rad_3*scipy.cos(scipy.linspace(angle_3_1, angle_3_2, 30,
                             endpoint=True)) + y_c_3
    in_y_4 = rad_4*scipy.cos(scipy.linspace(angle_4_1, angle_4_2, 30,
                             endpoint=True)) + y_c_4
    in_y = scipy.concatenate((in_y_1, in_y_2, in_y_3, in_y_4))

    in_x = scipy.concatenate([in_x, in_x[::-1]])
    in_y = scipy.concatenate([in_y, -in_y[::-1]])

    rad_to_outer_tf = data.r0 + data.a + data.scraplo + data.fwoth + \
        data.blnkoth + data.shldoth + data.ddwi + data.gapsto
    rad_to_inner_tf = data.bore + data.ohcth + data.gapoh + data.tfcth

    in_width = rad_to_outer_tf - rad_to_inner_tf
    out_width = in_width + data.tfcth + data.tfthko

    rad_to_inside_inner_tf = data.bore + data.ohcth + data.gapoh

    out_x = ((in_x - rad_to_inner_tf)*(out_width/in_width)) + \
        rad_to_inside_inner_tf

    top = data.plasma_half_height + (data.scrapli + data.scraplo)/2.0 + \
        (data.fwith + data.fwoth)/2.0 + data.blnktth + data.shldtth + \
        data.ddwi + data.vgap2 + data.tfcth

    bottom = -data.plasma_half_height - data.vgap - \
        data.divfix - data.shldtth - data.ddwi - data.vgap2 - data.tfcth

    external = (top - bottom)/((top - data.tfcth) - (bottom + data.tfcth))

    out_y = in_y * external

    shift = (top + bottom)/2.0
    out_y += shift
    in_y += shift

    ins_x = scipy.concatenate((in_x, out_x[::-1]))
    ins_y = scipy.concatenate((in_y, out_y[::-1]))

    # Description line
    output_file.write("# TF Coil information\n")

    # TF coil toroidal thickness

    # Outboard
    tf_toroidal_thickness_outboard = data.tftort
    line_1 = "TF coil toroidal thickness outboard = %.3f m\n" % \
             tf_toroidal_thickness_outboard

    # Number of TF coils
    line_2 = "Number of TF coils = %d \n" % int(data.tfno)

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)

    # Write TF coil plot points to file
    output_file.write("\n")
    output_file.write("# TF coil x, y co-ordinates\n")
    output_file.write("x\t\t\ty\n")

    for i in range(len(ins_x)):
        line = "%.3f\t\t%.3f\n" % (ins_x[i], ins_y[i])
        output_file.write(line)
    
    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        ax.fill(ins_x, ins_y, color='0.8')


def write_plasma_shape(data, output_file):
    """Function to give information for plasma shape
    """

    r0 = data.r0
    a = data.a
    delta = data.delta95*1.5
    kappa = data.kappa95*1.12

    # x position of outside arc
    x1 = (2*r0*(1 + delta) - a*(delta**2 + kappa**2 - 1))/(2*(1 + delta))

    # x position of inside arc
    x2 = (2*r0*(delta - 1) - a*(delta**2 + kappa**2 - 1))/(2*(delta - 1))

    # Radius of outside arc
    r1 = 0.5*scipy.sqrt((a**2*((delta + 1)**2 + kappa**2)**2)/((delta + 1)**2))

    # Radius of inside arc
    r2 = 0.5*scipy.sqrt((a**2*((delta - 1)**2 + kappa**2)**2)/((delta - 1)**2))

    # Angle for arc for outside arc
    theta1 = scipy.arcsin((kappa*a)/r1)

    # Angle for arc for inside arc
    theta2 = scipy.arcsin((kappa*a)/r2)

    # ?
    inang = 1./r1
    outang = 1.5/r2

    # Array of angle values for outside arc
    angles_1 = scipy.linspace(-theta1+scipy.pi, (inang+theta1)+scipy.pi,
                              100, endpoint=True)

    # Array of angle values for inside arc
    angles_2 = scipy.linspace(-(outang+theta2), theta2, 100,
                              endpoint=True)

    # x, y points for outside arc
    xs1 = -(r1*scipy.cos(angles_1) - x1)
    ys1 = r1*scipy.sin(angles_1)

    # x, y points for inside arc
    xs2 = -(r2*scipy.cos(angles_2) - x2)
    ys2 = r2*scipy.sin(angles_2)

    if data.plot:
        # Plot outside arc
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        ax.plot(xs1, ys1, color='orange')
        # Plot inside arc
        ax.plot(xs2, ys2, color='orange')

    # Description line
    output_file.write("# Plasma arcs\n\n")

    # Write data for outside plasma arc
    output_file.write("# Outside plasma arc\n")
    output_file.write("x\t\t\ty\n")

    for i in range(len(xs1)):
        line = "%.3f\t\t%.3f\n" % (xs1[i], ys1[i])
        output_file.write(line)

    output_file.write("\n")

    # Write data for inside plasma arc
    output_file.write("# Inside plasma arc\n")
    output_file.write("x\t\t\ty\n")

    for i in range(len(xs2)):
        line = "%.3f\t\t%.3f\n" % (xs2[i], ys2[i])
        output_file.write(line)

    output_file.write("\n")


def write_tf_to_vv_gap(data, output_file):
    """Function to write the data for the gap between the TF coil and the
    vacuum vessel for both the inboard and outboard sides.
    """

    # Description line
    output_file.write("# TF to vacuum vessel (VV) gap\n")

    # Radial gap inside
    radial_gap_inner = data.gapds
    line_1 = "Radial gap between TF coil and VV inboard side = " + \
             str(radial_gap_inner) + " m\n"

    # Radial gap outside
    radial_gap_outer = data.gapsto
    line_2 = "Radial gap between TF coil and VV outboard side = " + \
             str(radial_gap_outer) + " m\n"

    # Vertical gap upper
    vertical_gap_upper = data.vgap2
    line_3 = "Vertical gap between TF coil and VV upper = " + \
             str(vertical_gap_upper) + " m\n"

    # Vertical gap lower
    vertical_gap_lower = data.vgap2
    line_4 = "Vertical gap between TF coil and VV lower = " + \
             str(vertical_gap_lower) + " m\n"

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")


def write_vacuum_vessel(data, output_file):
    """Function to write vacuum vessel data to the output file.
    """

    # Description line
    output_file.write("# Vacuum vessel outer edge (ellipses)\n\n")

    # Vacuum vessel inboard upper ellipse
    output_file.write("# Vacuum vessel inboard upper quarter ellipse\n")

    # Inboard upper ellipse centre x position
    inboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_1 = "Ellipse centre x position = %.3f m\n" % \
        inboard_upper_ellipse_centre_x

    # Inboard upper ellipse centre y position
    inboard_upper_ellipse_centre_y = 0.0
    line_2 = "Ellipse centre y position = %.3f m\n" % \
        inboard_upper_ellipse_centre_y

    # Inboard upper ellipse minor radius
    inboard_upper_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds
    line_3 = "Ellipse minor radius = %.3f m\n" % \
        inboard_upper_ellipse_minor_radius

    # Inboard upper ellipse major radius
    inboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0 + \
        data.blnktth + data.shldtth + data.ddwi
    line_4 = "Ellipse major radius = %.3f m\n" % \
        inboard_upper_ellipse_major_radius

    # Write inboard upper ellipse data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")

    # Vacuum vessel inboard lower ellipse
    output_file.write("# Vacuum vessel inboard lower quarter ellipse\n")

    # Inboard lower ellipse centre x position
    inboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_5 = "Ellipse centre x position = %.3f m\n" % \
        inboard_lower_ellipse_centre_x

    # Inboard lower ellipse centre y position
    inboard_lower_ellipse_centre_y = 0.0
    line_6 = "Ellipse centre y position = %.3f m\n" % \
        inboard_lower_ellipse_centre_y

    # Inboard lower ellipse minor radius
    inboard_lower_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds
    line_7 = "Ellipse minor radius = %.3f m\n" % \
        inboard_lower_ellipse_minor_radius

    # Inboard lower ellipse major radius
    inboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.vgap + + data.divfix + data.shldtth + data.ddwi
    line_8 = "Ellipse major radius = %.3f m\n" % \
        inboard_lower_ellipse_major_radius

    # Write inboard lower ellipse data
    output_file.write(line_5)
    output_file.write(line_6)
    output_file.write(line_7)
    output_file.write(line_8)
    output_file.write("\n")

    # Vacuum vessel outboard upper ellipse
    output_file.write("# Vacuum vessel outboard upper quarter ellipse\n")

    # Outboard upper ellipse centre x position
    outboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_9 = "Ellipse centre x position = %.3f m\n" % \
        outboard_upper_ellipse_centre_x

    # Outboard upper ellipse centre y position
    outboard_upper_ellipse_centre_y = 0.0
    line_10 = "Ellipse centre y position = %.3f m\n" % \
        outboard_upper_ellipse_centre_y

    # Outboard upper ellipse minor radius
    outboard_upper_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth + data.blnkoth + data.shldoth + data.ddwi
    line_11 = "Ellipse minor radius = %.3f m\n" % \
        outboard_upper_ellipse_minor_radius

    # Outboard upper ellipse major radius
    outboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0 + \
        data.blnktth + data.shldtth + data.ddwi
    line_12 = "Ellipse major radius = %.3f m\n" % \
        outboard_upper_ellipse_major_radius

    # Write outboard upper ellipse data
    output_file.write(line_9)
    output_file.write(line_10)
    output_file.write(line_11)
    output_file.write(line_12)
    output_file.write("\n")

    # Vacuum vessel outboard lower ellipse
    output_file.write("# Vacuum vessel outboard lower quarter ellipse\n")

    # Outboard lower ellipse centre x position
    outboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_13 = "Ellipse centre x position = %.3f m\n" % \
        outboard_lower_ellipse_centre_x

    # Outboard lower ellipse centre y position
    outboard_lower_ellipse_centre_y = 0.0
    line_14 = "Ellipse centre y position = %.3f m\n" % \
        outboard_lower_ellipse_centre_y

    # Outboard lower ellipse minor radius
    outboard_lower_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth + data.blnkoth + data.shldoth + data.ddwi
    line_15 = "Ellipse minor radius = %.3f m\n" % \
        outboard_lower_ellipse_minor_radius

    # Outboard lower ellipse major radius
    outboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.vgap + data.divfix + data.shldtth + data.ddwi
    line_16 = "Ellipse major radius = %.3f m\n" % \
        outboard_lower_ellipse_major_radius

    # Write outboard lower ellipse data
    output_file.write(line_13)
    output_file.write(line_14)
    output_file.write(line_15)
    output_file.write(line_16)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')

        a = Arc((inboard_upper_ellipse_centre_x, 0.0),
                2.0*inboard_upper_ellipse_minor_radius,
                2.0*inboard_upper_ellipse_major_radius,
                angle=0.0, theta1=90.0, theta2=180.0,
                edgecolor="r", linewidth="2")
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc((inboard_lower_ellipse_centre_x, 0.0),
                2.0*inboard_lower_ellipse_minor_radius,
                2.0*inboard_lower_ellipse_major_radius,
                angle=0.0, theta1=180.0, theta2=270.0,
                edgecolor="r", linewidth="2")
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc((outboard_upper_ellipse_centre_x, 0.0),
                2.0*outboard_upper_ellipse_minor_radius,
                2.0*outboard_upper_ellipse_major_radius,
                angle=0.0, theta1=0.0, theta2=90.0,
                edgecolor="r", linewidth="2")
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc((outboard_lower_ellipse_centre_x, 0.0),
                2.0*outboard_lower_ellipse_minor_radius,
                2.0*outboard_lower_ellipse_major_radius,
                angle=0.0, theta1=270.0, theta2=0.0,
                edgecolor="r", linewidth="2")
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)

        ax.set_xlim([0, 20])
        ax.set_ylim([-12, 12])


def write_shield(data, output_file):
    """Function to write the shield information to the output_file.
    """

    # Description line
    output_file.write("# Shield outer edge (ellipses)\n\n")

    # Shield inboard upper ellipse
    output_file.write("# Shield inboard upper quarter ellipse\n")

    # Inboard upper ellipse centre x position
    inboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_1 = "Ellipse centre x position = %.3f m\n" % \
        inboard_upper_ellipse_centre_x

    # Inboard upper ellipse centre y position
    inboard_upper_ellipse_centre_y = 0.0
    line_2 = "Ellipse centre y position = %.3f m\n" % \
        inboard_upper_ellipse_centre_y

    # Inboard upper ellipse minor radius
    inboard_upper_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi
    line_3 = "Ellipse minor radius = %.3f m\n" % \
        inboard_upper_ellipse_minor_radius

    # Inboard upper ellipse major radius
    inboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0 + \
        data.blnktth + data.shldtth
    line_4 = "Ellipse major radius = %.3f m\n" % \
        inboard_upper_ellipse_major_radius

    # Write upper inboard ellipse data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")

    # Shield inboard lower ellipse
    output_file.write("# Shield inboard lower quarter ellipse\n")

    # Inboard lower ellipse centre x position
    inboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_5 = "Ellipse centre x position = %.3f m\n" % \
        inboard_lower_ellipse_centre_x

    # Inboard lower ellipse centre y position
    inboard_lower_ellipse_centre_y = 0.0
    line_6 = "Ellipse centre y position = %.3f m\n" % \
        inboard_lower_ellipse_centre_y

    # Inboard lower ellipse minor radius
    inboard_lower_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi
    line_7 = "Ellipse minor radius = %.3f m\n" % \
        inboard_lower_ellipse_minor_radius

    # Inboard lower ellipse major radius
    inboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.vgap + data.divfix + data.shldtth
    line_8 = "Ellipse major radius = %.3f m\n" % \
        inboard_lower_ellipse_major_radius

    # Write lower inboard ellipse data
    output_file.write(line_5)
    output_file.write(line_6)
    output_file.write(line_7)
    output_file.write(line_8)
    output_file.write("\n")

    # Shield outboard upper ellipse
    output_file.write("# Shield outboard upper quarter ellipse\n")

    # Outboard upper ellipse centre x position
    outboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_9 = "Ellipse centre x position = %.3f m\n" % \
        outboard_upper_ellipse_centre_x

    # Outboard upper ellipse centre y position
    outboard_upper_ellipse_centre_y = 0.0
    line_10 = "Ellipse centre y position = %.3f m\n" % \
        outboard_upper_ellipse_centre_y

    # Outboard upper ellipse minor radius
    outboard_upper_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth + data.blnkoth + data.shldoth
    line_11 = "Ellipse minor radius = %.3f m\n" % \
        outboard_upper_ellipse_minor_radius

    # Outboard upper ellipse major radius
    outboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0 + \
        data.blnktth + data.shldtth
    line_12 = "Ellipse major radius = %.3f m\n" % \
        outboard_upper_ellipse_major_radius

    # Write outboard upper ellipse data
    output_file.write(line_9)
    output_file.write(line_10)
    output_file.write(line_11)
    output_file.write(line_12)
    output_file.write("\n")

    # Shield outboard lower ellipse
    output_file.write("# Shield outboard lower quarter ellipse\n")

    # Outboard lower ellipse centre x position
    outboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_13 = "Ellipse centre x position = %.3f m\n" % \
        outboard_lower_ellipse_centre_x

    # Outboard lower ellipse centre y position
    outboard_lower_ellipse_centre_y = 0.0
    line_14 = "Ellipse centre y position = %.3f m\n" % \
        outboard_lower_ellipse_centre_y

    # Outboard lower ellipse minor radius
    outboard_lower_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth + data.blnkoth + data.shldoth
    line_15 = "Ellipse minor radius = %.3f m\n" % \
        outboard_lower_ellipse_minor_radius

    # Outboard lower ellipse major radius
    outboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.vgap + data.divfix + data.shldtth
    line_16 = "Ellipse major radius = %.3f m\n" % \
        outboard_lower_ellipse_major_radius

    # Write outboard lower ellipse data
    output_file.write(line_13)
    output_file.write(line_14)
    output_file.write(line_15)
    output_file.write(line_16)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        a = Arc((inboard_upper_ellipse_centre_x, 0.0),
                2.0*inboard_upper_ellipse_minor_radius,
                2.0*inboard_upper_ellipse_major_radius,
                angle=0.0, theta1=90.0, theta2=180.0,
                edgecolor="b", linewidth="2")
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc((inboard_lower_ellipse_centre_x, 0.0),
                2.0*inboard_lower_ellipse_minor_radius,
                2.0*inboard_lower_ellipse_major_radius,
                angle=0.0, theta1=180.0, theta2=270.0,
                edgecolor="b", linewidth="2")
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc((outboard_upper_ellipse_centre_x, 0.0),
                2.0*outboard_upper_ellipse_minor_radius,
                2.0*outboard_upper_ellipse_major_radius,
                angle=0.0, theta1=0.0, theta2=90.0,
                edgecolor="b", linewidth="2")
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc((outboard_lower_ellipse_centre_x, 0.0),
                2.0*outboard_lower_ellipse_minor_radius,
                2.0*outboard_lower_ellipse_major_radius,
                angle=0.0, theta1=270.0, theta2=0.0,
                edgecolor="b", linewidth="2")
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)


def write_blanket(data, output_file):
    """Function to write the blanket information to the output_file.
    """

    # Description line
    output_file.write("# Blanket outer edge (ellipses)\n\n")

    # Blanket inboard upper ellipse
    output_file.write("# Blanket inboard upper quarter ellipse\n")

    # Inboard upper ellipse centre x position
    inboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_1 = "Ellipse centre x position = %.3f m\n" % \
        inboard_upper_ellipse_centre_x

    # Inboard upper ellipse centre y position
    inboard_upper_ellipse_centre_y = 0.0
    line_2 = "Ellipse centre y position = %.3f m\n" % \
        inboard_upper_ellipse_centre_y

    # Inboard upper ellipse minor radius
    inboard_upper_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi - data.shldith
    line_3 = "Ellipse minor radius = %.3f m\n" % \
        inboard_upper_ellipse_minor_radius

    # Inboard upper ellipse major radius
    inboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0 + \
        data.blnktth
    line_4 = "Ellipse major radius = %.3f m\n" % \
        inboard_upper_ellipse_major_radius

    # Write inboard upper ellipse data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")

    # Blanket inboard lower ellipse
    output_file.write("# Blanket inboard lower quarter ellipse\n")

    # Inboard lower ellipse centre x position
    inboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_5 = "Ellipse centre x position = %.3f m\n" % \
        inboard_lower_ellipse_centre_x

    # Inboard lower ellipse centre y position
    inboard_lower_ellipse_centre_y = 0.0
    line_6 = "Ellipse centre y position = %.3f m\n" % \
        inboard_lower_ellipse_centre_y

    # Inboard lower ellipse minor radius
    inboard_lower_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi - data.shldith
    line_7 = "Ellipse minor radius = %.3f m\n" % \
        inboard_lower_ellipse_minor_radius

    # Inboard lower ellipse major radius
    inboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.vgap + data.divfix
    line_8 = "Ellipse major radius = %.3f m\n" % \
        inboard_lower_ellipse_major_radius

    # Write inboard ellipse data
    output_file.write(line_5)
    output_file.write(line_6)
    output_file.write(line_7)
    output_file.write(line_8)
    output_file.write("\n")

    # Blanket outboard upper ellipse
    output_file.write("# Blanket outboard upper quarter ellipse\n")

    # Outboard upper ellipse centre x position
    outboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_9 = "Ellipse centre x position = %.3f m\n" % \
        outboard_upper_ellipse_centre_x

    # Outboard upper ellipse centre y position
    outboard_upper_ellipse_centre_y = 0.0
    line_10 = "Ellipse centre y position = %.3f m\n" % \
        outboard_upper_ellipse_centre_y

    # Outboard upper ellipse minor radius
    outboard_upper_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth + data.blnkoth
    line_11 = "Ellipse minor radius = %.3f m\n" % \
        outboard_upper_ellipse_minor_radius

    # Outboard ellipse major radius
    outboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0 + \
        data.blnktth
    line_12 = "Ellipse major radius = %.3f m\n" % \
        outboard_upper_ellipse_major_radius

    # Write outboard upper ellipse data
    output_file.write(line_9)
    output_file.write(line_10)
    output_file.write(line_11)
    output_file.write(line_12)
    output_file.write("\n")

    # Blanket outboard lower ellipse
    output_file.write("# Blanket outboard lower quarter ellipse\n")

    # Outboard ellipse centre x position
    outboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_13 = "Ellipse centre x position = %.3f m\n" % \
        outboard_lower_ellipse_centre_x

    # Outboard ellipse centre y position
    outboard_lower_ellipse_centre_y = 0.0
    line_14 = "Ellipse centre y position = %.3f m\n" % \
        outboard_lower_ellipse_centre_y

    # Outboard ellipse minor radius
    outboard_lower_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth + data.blnkoth
    line_15 = "Ellipse minor radius = %.3f m\n" % \
        outboard_lower_ellipse_minor_radius

    # Outboard ellipse major radius
    outboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.vgap + data.divfix
    line_16 = "Ellipse major radius = %.3f m\n" % \
        outboard_lower_ellipse_major_radius

    # Write outboard lower ellipse data
    output_file.write(line_13)
    output_file.write(line_14)
    output_file.write(line_15)
    output_file.write(line_16)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        a = Arc((inboard_upper_ellipse_centre_x, 0.0),
                2.0*inboard_upper_ellipse_minor_radius,
                2.0*inboard_upper_ellipse_major_radius,
                angle=0.0, theta1=90.0, theta2=180.0,
                edgecolor="g", linewidth="2")
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc((inboard_lower_ellipse_centre_x, 0.0),
                2.0*inboard_lower_ellipse_minor_radius,
                2.0*inboard_lower_ellipse_major_radius,
                angle=0.0, theta1=180.0, theta2=270.0,
                edgecolor="g", linewidth="2")
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc((outboard_upper_ellipse_centre_x, 0.0),
                2.0*outboard_upper_ellipse_minor_radius,
                2.0*outboard_upper_ellipse_major_radius,
                angle=0.0, theta1=0.0, theta2=90.0,
                edgecolor="g", linewidth="2")
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc((outboard_lower_ellipse_centre_x, 0.0),
                2.0*outboard_lower_ellipse_minor_radius,
                2.0*outboard_lower_ellipse_major_radius,
                angle=0.0, theta1=270.0, theta2=0.0,
                edgecolor="g", linewidth="2")
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)


def write_first_wall_outer(data, output_file):
    """Function to write the blanket information to the output_file.
    """

    # Description line
    output_file.write("# First Wall outer edge (ellipses)\n\n")

    # First wall inboard ellipse
    output_file.write("# First Wall inboard upper quarter ellipse\n")

    # Inboard upper ellipse centre x position
    inboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_1 = "Ellipse centre x position = %.3f m\n" % \
        inboard_upper_ellipse_centre_x

    # Inboard upper ellipse centre y position
    inboard_upper_ellipse_centre_y = 0.0
    line_2 = "Ellipse centre y position = %.3f m\n" % \
        inboard_upper_ellipse_centre_y

    # Inboard upper ellipse minor radius
    inboard_upper_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi - data.shldith - data.blnkith
    line_3 = "Ellipse minor radius = %.3f m\n" % \
        inboard_upper_ellipse_minor_radius

    # Inboard ellipse major radius
    inboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0
    line_4 = "Ellipse major radius = %.3f m\n" % \
        inboard_upper_ellipse_major_radius

    # Write inboard upper ellipse data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")

    # First wall inboard ellipse
    output_file.write("# First Wall inboard lower partial ellipse\n")

    # Inboard lower ellipse centre x position
    inboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_5 = "Ellipse centre x position = %.3f m\n" % \
        inboard_lower_ellipse_centre_x

    # Inboard lower ellipse centre y position
    inboard_lower_ellipse_centre_y = 0.0
    line_6 = "Ellipse centre y position = %.3f" % \
        inboard_lower_ellipse_centre_y

    # Inboard lower ellipse minor radius
    inboard_lower_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi - data.shldith - data.blnkith
    line_7 = "Ellipse minor radius = %.3f m\n" % \
        inboard_lower_ellipse_minor_radius

    # Inboard lower ellipse major radius
    inboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.blnkith + data.fwith
    line_8 = "Ellipse major radius = %.3f m\n" % \
        inboard_lower_ellipse_major_radius

    # Angle subtended
    angle_1 = 180.0
    angle_2 = 230.0
    line_9 = "Ellipse angle (0 at y=0 at going anti-clockwise) = " + \
        str(angle_1) + "->" + str(angle_2) + "\n"

    # Write inboard lower ellipse data
    output_file.write(line_5)
    output_file.write(line_6)
    output_file.write(line_7)
    output_file.write(line_8)
    output_file.write(line_9)
    output_file.write("\n")

    # First wall outboard upper ellipse
    output_file.write("# First Wall outboard upper quarter ellipse\n")

    # Outboard ellipse centre x position
    outboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_10 = "Ellipse centre x position = %.3f m\n" % \
        outboard_upper_ellipse_centre_x

    # Outboard upper ellipse centre y position
    outboard_upper_ellipse_centre_y = 0.0
    line_11 = "Ellipse centre y position = %.3f m\n" % \
        outboard_upper_ellipse_centre_y

    # Outboard ellipse minor radius
    outboard_upper_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth
    line_12 = "Ellipse minor radius = %.3f m\n" % \
        outboard_upper_ellipse_minor_radius

    # Outboard upper ellipse major radius
    outboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0) + (data.fwith + data.fwoth)/2.0
    line_13 = "Ellipse major radius = %.3f m\n" % \
        outboard_upper_ellipse_major_radius

    # Write upper outboard ellipse data
    output_file.write(line_10)
    output_file.write(line_11)
    output_file.write(line_12)
    output_file.write(line_13)
    output_file.write("\n")

    # First wall outboard lower ellipse
    output_file.write("# First Wall outboard lower partial ellipse\n")

    # Outboard lower ellipse centre x position
    outboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_14 = "Ellipse centre x position = %.3f m\n" % \
        outboard_lower_ellipse_centre_x

    # Outboard lower ellipse centre y position
    outboard_lower_ellipse_centre_y = 0.0
    line_15 = "Ellipse centre y position = %.3f m\n" % \
        outboard_lower_ellipse_centre_y

    # Outboard lower ellipse minor radius
    outboard_lower_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth
    line_16 = "Ellipse minor radius = %.3f m\n" % \
        outboard_lower_ellipse_minor_radius

    # Outboard lower ellipse major radius
    outboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.blnkoth + data.fwoth
    line_17 = "Ellipse major radius = %.3fm \n" % \
        outboard_lower_ellipse_major_radius

    # Angle subtended
    angle_3 = 290.0
    angle_4 = 0.0
    line_18 = "Ellipse angle (0 at y=0 at going anti-clockwise) = " + \
        str(angle_1) + "->" + str(angle_2) + "\n"

    # Write outboard lower ellipse data
    output_file.write(line_14)
    output_file.write(line_15)
    output_file.write(line_16)
    output_file.write(line_17)
    output_file.write(line_18)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        a = Arc((inboard_upper_ellipse_centre_x, 0.0),
                2.0*inboard_upper_ellipse_minor_radius,
                2.0*inboard_upper_ellipse_major_radius,
                angle=0.0, theta1=90.0, theta2=180.0,
                edgecolor="k", linewidth="2")
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc((inboard_lower_ellipse_centre_x, 0.0),
                2.0*inboard_lower_ellipse_minor_radius,
                2.0*inboard_lower_ellipse_major_radius,
                angle=0.0, theta1=angle_1, theta2=angle_2,
                edgecolor="k", linewidth="2")
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc((outboard_upper_ellipse_centre_x, 0.0),
                2.0*outboard_upper_ellipse_minor_radius,
                2.0*outboard_upper_ellipse_major_radius,
                angle=0.0, theta1=0.0, theta2=90.0,
                edgecolor="k", linewidth="2")
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc((outboard_lower_ellipse_centre_x, 0.0),
                2.0*outboard_lower_ellipse_minor_radius,
                2.0*outboard_lower_ellipse_major_radius,
                angle=0.0, theta1=angle_3, theta2=angle_4,
                edgecolor="k", linewidth="2")
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)


def write_first_wall_inner(data, output_file):
    """Function to write the blanket information to the output_file.
    """

    # Description line
    output_file.write("# First Wall inner edge (ellipses)\n\n")

    # First wall inboard ellipse
    output_file.write("# First Wall inboard upper quarter ellipse\n")

    # Inboard upper ellipse centre x position
    inboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_1 = "Ellipse centre x position = %.3f m\n" % \
        inboard_upper_ellipse_centre_x

    # Inboard upper ellipse centre y position
    inboard_upper_ellipse_centre_y = 0.0
    line_2 = "Ellipse centre y position = %.3f m\n" % \
        inboard_upper_ellipse_centre_y

    # Inboard upper ellipse minor radius
    inboard_upper_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi - data.shldith - data.blnkith - data.fwith
    line_3 = "Ellipse minor radius = %.3f m\n" % \
        inboard_upper_ellipse_minor_radius

    # Inboard upper ellipse major radius
    inboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0)
    line_4 = "Ellipse major radius = %.3f m\n" % \
        inboard_upper_ellipse_major_radius

    # Write inboard ellipse data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")

    # First wall inboard ellipse
    output_file.write("# First Wall inboard lower partial ellipse\n")

    # Inboard lower ellipse centre x position
    inboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_5 = "Ellipse centre x position = %.3f m\n" % \
        inboard_lower_ellipse_centre_x

    # Inboard lower ellipse centre y position
    inboard_lower_ellipse_centre_y = 0.0
    line_6 = "Ellipse centre y position = %.3fm \n" % \
        inboard_lower_ellipse_centre_y

    # Inboard lower ellipse minor radius
    inboard_lower_ellipse_minor_radius = data.r0 - data.delta*data.a - \
        data.bore - data.ohcth - data.gapoh - data.tfcth - data.gapds - \
        data.ddwi - data.shldith - data.blnkith - data.fwith
    line_7 = "Ellipse minor radius = %.3f m\n" % \
        inboard_lower_ellipse_minor_radius

    # Inboard lower ellipse major radius
    inboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.blnkith
    line_8 = "Ellipse major radius = %.3f m\n" % \
        inboard_lower_ellipse_major_radius

    # Angle subtended
    angle_1 = 180.0
    angle_2 = 230.0
    line_9 = "Ellipse angle (0 at y=0 at going anti-clockwise) = " + \
        str(angle_1) + "->" + str(angle_2) + "\n"

    # Write inboard ellipse data
    output_file.write(line_5)
    output_file.write(line_6)
    output_file.write(line_7)
    output_file.write(line_8)
    output_file.write(line_9)
    output_file.write("\n")

    # First wall outboard upper ellipse
    output_file.write("# First Wall outboard upper quarter ellipse\n")

    # Outboard ellipse centre x position
    outboard_upper_ellipse_centre_x = data.r0 - data.delta*data.a
    line_10 = "Ellipse centre x position = %.3f m\n" % \
        outboard_upper_ellipse_centre_x

    # Outboard upper ellipse centre y position
    outboard_upper_ellipse_centre_y = 0.0
    line_11 = "Ellipse centre y position = %.3f m\n" % \
        outboard_upper_ellipse_centre_y

    # Outboard ellipse minor radius
    outboard_upper_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo
    line_12 = "Ellipse minor radius = %.3f m\n" % \
        outboard_upper_ellipse_minor_radius

    # Outboard upper ellipse major radius
    outboard_upper_ellipse_major_radius = data.plasma_half_height + \
        ((data.scraplo + data.scrapli)/2.0)
    line_13 = "Ellipse major radius = %.3f m\n" % \
        outboard_upper_ellipse_major_radius

    # Write outboard upper ellipse data
    output_file.write(line_10)
    output_file.write(line_11)
    output_file.write(line_12)
    output_file.write(line_13)
    output_file.write("\n")

    # First wall outboard lower ellipse
    output_file.write("# First Wall outboard lower partial ellipse\n")

    # Outboard lower ellipse centre x position
    outboard_lower_ellipse_centre_x = data.r0 - data.delta*data.a
    line_14 = "Ellipse centre x position = %.3f m\n" % \
        outboard_lower_ellipse_centre_x

    # Outboard lower ellipse centre y position
    outboard_lower_ellipse_centre_y = 0.0
    line_15 = "Ellipse centre y position = %.3f m\n" % \
        outboard_lower_ellipse_centre_y

    # Outboard lower ellipse minor radius
    outboard_lower_ellipse_minor_radius = data.delta*data.a + data.a + \
        data.scraplo + data.fwoth
    line_16 = "Ellipse minor radius = %.3f m\n" % \
        outboard_lower_ellipse_minor_radius

    # Outboard lower ellipse major radius
    outboard_lower_ellipse_major_radius = data.plasma_half_height + \
        data.blnkoth
    line_17 = "Ellipse major radius = %.3f m\n" % \
        outboard_lower_ellipse_major_radius

    # Angle subtended
    angle_3 = 290.0
    angle_4 = 0.0
    line_18 = "Ellipse angle (0 at y=0 at going anti-clockwise) = " + \
        str(angle_1) + "->" + str(angle_2) + "\n"

    # Write outboard lower ellipse data
    output_file.write(line_14)
    output_file.write(line_15)
    output_file.write(line_16)
    output_file.write(line_17)
    output_file.write(line_18)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        a = Arc((inboard_upper_ellipse_centre_x, 0.0),
                2.0*inboard_upper_ellipse_minor_radius,
                2.0*inboard_upper_ellipse_major_radius,
                angle=0.0, theta1=90.0, theta2=180.0,
                edgecolor="k", linewidth="2")
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc((inboard_lower_ellipse_centre_x, 0.0),
                2.0*inboard_lower_ellipse_minor_radius,
                2.0*inboard_lower_ellipse_major_radius,
                angle=0.0, theta1=angle_1, theta2=angle_2,
                edgecolor="k", linewidth="2")
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc((outboard_upper_ellipse_centre_x, 0.0),
                2.0*outboard_upper_ellipse_minor_radius,
                2.0*outboard_upper_ellipse_major_radius,
                angle=0.0, theta1=0.0, theta2=90.0,
                edgecolor="k", linewidth="2")
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc((outboard_lower_ellipse_centre_x, 0.0),
                2.0*outboard_lower_ellipse_minor_radius,
                2.0*outboard_lower_ellipse_major_radius,
                angle=0.0, theta1=angle_3, theta2=angle_4,
                edgecolor="k", linewidth="2")
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)


def write_pf_coils(data, output_file):
    """Function to write PF coil data
    """

    coils_r = []
    coils_z = []
    coils_dr = []
    coils_dz = []
    coil_text = []

    # Number of coils (1 is OH coil)
    number_of_coils = 0
    for item in data.mfile.data.keys():
        if "rpf(" in item:
            number_of_coils += 1

    # Rest of the coils
    for coil in range(1, number_of_coils):
        coils_r.append(data.mfile.data["rpf(%s)" % str(coil).zfill(2)].
                       get_scan(-1))
        coils_z.append(data.mfile.data["zpf(%s)" % str(coil).zfill(2)].
                       get_scan(-1))
        coils_dr.append(data.mfile.data["pfdr%s" % str(coil).zfill(2)].
                        get_scan(-1))
        coils_dz.append(data.mfile.data["pfdz%s" % str(coil).zfill(2)].
                        get_scan(-1))
        coil_text.append(str(coil))

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')

    # Description line
    output_file.write("# PF coil information\n\n")

    for i in range(len(coils_r)):
        r_1 = coils_r[i] - 0.5*coils_dr[i]
        z_1 = coils_z[i] - 0.5*coils_dz[i]
        r_2 = coils_r[i] - 0.5*coils_dr[i]
        z_2 = coils_z[i] + 0.5*coils_dz[i]
        r_3 = coils_r[i] + 0.5*coils_dr[i]
        z_3 = coils_z[i] + 0.5*coils_dz[i]
        r_4 = coils_r[i] + 0.5*coils_dr[i]
        z_4 = coils_z[i] - 0.5*coils_dz[i]
        r_5 = coils_r[i] - 0.5*coils_dr[i]
        z_5 = coils_z[i] - 0.5*coils_dz[i]

        r_points = [r_1, r_2, r_3, r_4, r_5]
        z_points = [z_1, z_2, z_3, z_4, z_5]

        output_file.write("# PF coil: %s\n" % coil_text[i])
        output_file.write("x_values = %s\n" %
                          str(["%.3f" % item for item in r_points]))
        output_file.write("y_values = %s\n" %
                          str(["%.3f" % item for item in z_points]))
        output_file.write("\n")

        if data.plot:
            ax.plot(r_points, z_points, color='black')
            ax.text(coils_r[i], coils_z[i], coil_text[i],
                    ha='center', va='center', fontsize='smaller')


def write_cryostat(data, output_file):
    """Function to write cryostat data
    """

    # Description line
    output_file.write("# Cryostat (cylinder with lid)\n")

    # Cryostat inner radius
    cryostat_inner_radius = data.rdewex
    line_1 = "Cryostat inner radius = %.3f m\n" % cryostat_inner_radius

    # TODO put in proper value here
    # Cryostat thickness
    #cryostat_thickness = data.ddwex
    cryostat_thickness = 0.15
    line_2 = "Cryostat thickness = %.3f m\n" % cryostat_thickness

    # Cryostat outer radius
    cryostat_outer_radius = cryostat_inner_radius + cryostat_thickness
    line_3 = "Cryostat outer radius = %.3f m\n" % cryostat_outer_radius

    # Cryostat half-height
    cryostat_half_height = data.zdewex
    line_4 = "Cryostat half height = %.3f m\n" % cryostat_half_height

    # Write cryostat data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect='equal')
        x = [0, 0, cryostat_outer_radius, cryostat_outer_radius,
             0, 0, cryostat_inner_radius, cryostat_inner_radius, 0]
        y = [cryostat_half_height, cryostat_half_height + cryostat_thickness,
             cryostat_half_height + cryostat_thickness,
             -cryostat_half_height - cryostat_thickness,
             -cryostat_half_height - cryostat_thickness,
             -cryostat_half_height, -cryostat_half_height,
             +cryostat_half_height, +cryostat_half_height]
        ax.fill(x, y, "b")


def write_cad_data(data, output_file):
    """Function for writing information from data into output_file
    """

    # Write cylinder data
    write_machine_bore(data, output_file)

    # Write OH coil data
    write_oh_coil(data, output_file)

    # Write gap between OH coil and TF coil data (gapoh)
    write_oh_gap(data, output_file)

    # Write cryostat data
    write_cryostat(data, output_file)

    # Not needed if just empty space?
    # Write TF coil to vacuum vessel gap data
    write_tf_to_vv_gap(data, output_file)

    # Write vacuum vessel data
    write_vacuum_vessel(data, output_file)

    # Write shield data
    write_shield(data, output_file)

    # Write blanket data
    write_blanket(data, output_file)

    # Write first wall data
    write_first_wall_outer(data, output_file)
    write_first_wall_inner(data, output_file)

    # Write PF coil data
    write_pf_coils(data, output_file)

    # Write plasma shape
    write_plasma_shape(data, output_file)

    # Write TF coil data
    write_tf_coils(data, output_file)

    # Show plot if plot option chosen
    if data.plot:
        plt.show()


def cad_main(cl_args):
    """Function to write PROCESS CAD output to file PROCESS.CAD
    """

    # Read MFILE
    m_file = mf.MFile(cl_args.f)

    # Setup CAD file
    cad_file = open(cl_args.o, "w")

    # Extract build data
    build_data = BuildDataContainer(m_file)

    # Set plot option
    if cl_args.show:
        build_data.plot = True
        build_data.alpha = 0.5

    # write data
    write_cad_data(build_data, cad_file)

    # Close CAD file
    cad_file.close()


if __name__ == "__main__":
    # Setup command line arguments
    parser = argparse.ArgumentParser(description="Produce a CAD output file "
                                     "of the PROCESS MFILE file for a given "
                                     "scan. For info contact "
                                     "james.morris2@ccfe.ac.uk")

    # Argument for the input file path
    parser.add_argument("-f", metavar='FILENAME', type=str,
                        default="MFILE.DAT",
                        help='specify input filename')

    # Argument for the output file name
    parser.add_argument("-o", metavar='OUTPUT', type=str,
                        default="PROCESS.CAD",
                        help='specify output filename')

    # Argument for showing the output to screen also
    parser.add_argument("-s", "--show", help="show plot as well as saving "
                        "figure", action="store_true")

    args = parser.parse_args()

    # pass MFILE object to cad_main
    cad_main(args)
