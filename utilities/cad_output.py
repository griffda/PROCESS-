#!/usr/bin/env python
"""

  PROCESS to CAD program

  James Morris
  02/03/2015

Hist:
  05/11/2018 SIM Updated build variables and added copper TF coils
  WARNING: Superconducting magnets still need fixing

"""

import scipy
import numpy as np

# import argument parser
import argparse

# import mfile library
import process.io.mfile as mf

# Import matplotlib libraries for plotting
import matplotlib.pyplot as plt
from matplotlib.patches import Arc
from matplotlib.path import Path
import matplotlib.patches as patches


class BuildDataContainer(object):
    def __init__(self, data):
        """Setup build data container"""

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

        # Single or double null
        if "i_single_null" in data.data.keys():
            self.i_single_null = data.data["i_single_null"].get_scan(-1)
        else:
            self.i_single_null = 1

        # Central Solenoid
        if "i_tf_sup" in data.data.keys():
            self.i_tf_sup = data.data["i_tf_sup"].get_scan(-1)
        else:
            self.i_tf_sup = 1

        # Machine bore
        self.bore = data.data["bore"].get_scan(-1)

        # Central Solenoid
        if "iohcl" in data.data.keys():
            self.iohcl = data.data["iohcl"].get_scan(-1)
        else:
            self.iohcl = 1

        # OH coil radial thickness
        self.ohcth = data.data["ohcth"].get_scan(-1)

        # OH coil precompression structure
        self.precomp = data.data["precomp"].get_scan(-1)

        # Radial gap between OH coil and TF coil inboard side
        self.gapoh = data.data["gapoh"].get_scan(-1)

        # Inboard TF coil leg radial thickness
        self.tfcth = data.data["tfcth"].get_scan(-1)

        # Radial gap between inboard TF coil leg and thermal shield
        self.deltf = data.data["deltf"].get_scan(-1)

        # TF-VV thermal shield thickness
        self.thshield = data.data["thshield"].get_scan(-1)

        # Radial gap between inboard thermal shield and vacuum vessel
        self.gapds = data.data["gapds"].get_scan(-1)

        # Thickness of inboard vacuum vessel
        self.d_vv_in = data.data["d_vv_in"].get_scan(-1)

        # Thickness of outboard vacuum vessel
        self.d_vv_out = data.data["d_vv_out"].get_scan(-1)

        # Thickness of topside vacuum vessel
        self.d_vv_top = data.data["d_vv_top"].get_scan(-1)

        # Thickness of underside vacuum vessel
        self.d_vv_bot = data.data["d_vv_bot"].get_scan(-1)

        # Inboard shiled radial thickness
        self.shldith = data.data["shldith"].get_scan(-1)

        # Gap between vacuum vessel and blanket
        self.vvblgap = data.data["vvblgap"].get_scan(-1)

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
        self.plasma_half_height = self.kappa * self.a

        # Top scrape-off
        self.vgaptop = data.data["vgaptop"].get_scan(-1)

        # Upper first wall vertical thickness
        self.fwtth = data.data["fwtth"].get_scan(-1)

        # Upper blanket vertical thickness
        self.blnktth = data.data["blnktth"].get_scan(-1)

        # Upper shield vertical thickness
        self.shldtth = data.data["shldtth"].get_scan(-1)

        # Lower shield vertical thickness
        self.shldlth = data.data["shldlth"].get_scan(-1)

        # Divertor structure
        self.divfix = data.data["divfix"].get_scan(-1)

        # Gap between TF coil and thermal shield
        self.tftsgap = data.data["tftsgap"].get_scan(-1)

        # Cryostat radius
        self.rdewex = data.data["rdewex"].get_scan(-1)

        # Cryostat half-height
        self.zdewex = data.data["zdewex"].get_scan(-1)

        # External cryostat radial thickness
        self.ddwex = data.data["ddwex"].get_scan(-1)

        # Number of TF coils
        self.n_tf = data.data["n_tf"].get_scan(-1)

        # Toroidal thickness of TF coil outer leg
        self.tftort = data.data["tftort"].get_scan(-1)

        # TF coil inboard leg inner case radial thickness
        self.thkcas = data.data["thkcas"].get_scan(-1)

        # TF coil winding pack radial thickness
        self.dr_tf_wp = data.data["dr_tf_wp"].get_scan(-1)

        # TF coil inboard leg plasma facing case radial thickness
        self.casthi = data.data["casthi"].get_scan(-1)

        # TF coil inboard leg sidewall case thickness
        self.casths = data.data["casths"].get_scan(-1)

        # ohhghf
        self.ohhghf = data.data["ohhghf"].get_scan(-1)

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

        # rmid
        self.rmid = data.data["rmid"].get_scan(-1)
        # rtop
        self.rtop = data.data["rtop"].get_scan(-1)
        # ztop
        self.ztop = data.data["ztop"].get_scan(-1)
        # hmax
        self.hmax = data.data["hmax"].get_scan(-1)


def write_header(cd_file, mfile):
    """Function to write header information for CAD output

    :param cd_file: cad file to write to
    :param mfile: mfile object with PROCESS data
    :return: -
    """

    # border
    cd_file.write("############################################################\n")

    # Write date
    export_day, export_month, export_year = mfile.data["date"].get_scan(-1).split("/")
    cd_file.write(
        "Process Code Export Date:{0}_{1}_{2}\n".format(
            export_year[-2:], export_month, export_day
        )
    )

    runtitle = mfile.data["runtitle"].get_scan(-1).replace(" ", "_")
    time = mfile.data["time"].get_scan(-1).replace(":", "_")
    cd_file.write(
        "Process Code Export ID:{0}_{1}_{2}_{3}_{4}\n".format(
            runtitle, export_year, export_month, export_day, time
        )
    )

    proc_user = mfile.data["username"].get_scan(-1)
    cd_file.write("Process Code Contact:{0}\n".format(proc_user))

    # border
    cd_file.write("############################################################\n")


def write_machine_bore(data, output_file):
    """Function to write data for the machine bore"""

    # Description line
    output_file.write("\n# Machine Bore (Cylinder)\n")

    # Bore radial thickness
    bore_thickness = data.bore
    line_1 = "Bore thickness = {0:.3f} m\n".format(bore_thickness)

    # Half-height from midplane
    bore_half_height = data.hmax * data.ohhghf
    line_2 = "Bore half-height from mid-plane = {0:.3f} m\n".format(bore_half_height)

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write("\n")


def write_oh_coil(data, output_file):
    """Function to write data for the oh coil"""

    # Description line
    output_file.write("# OH Coil (Cylinder)\n")

    # OH coil radial mid point (m)
    oh_rad_mid = data.bore + data.ohcth / 2.0
    line_1 = "OH coil - radius mid = {0:.3f} m\n".format(oh_rad_mid)

    # OH coil half-height from midplane (m)
    oh_half_height = data.hmax * data.ohhghf
    line_2 = "OH coil - half height = {0:.3f} m\n".format(oh_half_height)

    # OH coil thickness (m)
    oh_thickness = data.ohcth
    line_3 = "OH coil - thickness = {0:.3f} m\n".format(oh_thickness)

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")
        x = [
            data.bore,
            data.bore + oh_thickness,
            data.bore + oh_thickness,
            data.bore,
            data.bore,
        ]
        y = [
            -oh_half_height,
            -oh_half_height,
            oh_half_height,
            oh_half_height,
            -oh_half_height,
        ]
        ax.fill(x, y, "r")


def write_oh_gap(data, output_file):
    """Function to write data for the gap between the OH coil and the TF coil."""

    # Description line
    output_file.write("# OH Coil to TF gap (Cylinder)\n")

    # OH coil to TF coil gap radial mid point
    oh_gap_rad_mid = data.bore + data.ohcth + data.precomp + data.gapoh / 2.0
    line_1 = "OH Coil to TF coil gap - radius mid = {0:.3f} m\n".format(oh_gap_rad_mid)

    # OH coil to TF coil gap half-height from midplane.
    oh_gap_half_height = data.hmax * data.ohhghf
    line_2 = "OH Coil to TF coil gap - half height = {0:.3f} m\n".format(
        oh_gap_half_height
    )

    # OH coil to TF coil gap thickness
    oh_gap_thickness = data.gapoh
    line_3 = "OH Coil to TF coil gap - thickness = {0:.3f} m\n".format(oh_gap_thickness)

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write("\n")


def write_cryostat(data, output_file):
    """Function to write cryostat data"""

    # Description line
    output_file.write("# Cryostat (cylinder with lid)\n")

    # Cryostat inner and outer radii
    cryostat_inner_radius = data.rdewex
    cryostat_outer_radius = data.rdewex + data.ddwex

    # Cryostat radial mid point
    cryostat_rad_mid = cryostat_inner_radius + data.ddwex / 2.0
    line_1 = "Cryostat - radius mid = {0:.3f} m\n".format(cryostat_rad_mid)

    # Cryostat half-height
    cryostat_half_height = data.zdewex
    line_2 = "Cryostat - half height = {0:.3f} m\n".format(cryostat_half_height)

    # Cryostat thickness
    cryostat_thickness = data.ddwex
    line_3 = "Cryostat - thickness = {0:.3f} m\n".format(cryostat_thickness)

    # Write cryostat data
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write("\n")

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")
        x = [
            0,
            0,
            cryostat_outer_radius,
            cryostat_outer_radius,
            0,
            0,
            cryostat_inner_radius,
            cryostat_inner_radius,
            0,
        ]
        y = [
            cryostat_half_height,
            cryostat_half_height + cryostat_thickness,
            cryostat_half_height + cryostat_thickness,
            -cryostat_half_height - cryostat_thickness,
            -cryostat_half_height - cryostat_thickness,
            -cryostat_half_height,
            -cryostat_half_height,
            +cryostat_half_height,
            +cryostat_half_height,
        ]
        ax.fill(x, y, "b")


def write_tf_to_vv_gap(data, output_file):
    """Function to write the data for the gap between the TF coil and the
    vacuum vessel for both the inboard and outboard sides.
    """

    # Description line
    output_file.write("# TF to vacuum vessel (VV) gap\n")

    # Radial gap inside
    radial_gap_inner = data.gapds
    line_1 = "Radial gap between TF coil and VV inboard side = {0:.3f} m\n".format(
        radial_gap_inner
    )

    # Radial gap outside
    radial_gap_outer = data.gapsto
    line_2 = "Radial gap between TF coil and VV outboard side = {0:.3f} m\n".format(
        radial_gap_outer
    )

    # Vertical gap upper
    vertical_gap_upper = data.vgap2
    line_3 = "Vertical gap between TF coil and VV upper = {0:.3f} m\n".format(
        vertical_gap_upper
    )

    # Vertical gap lower
    vertical_gap_lower = data.vgap2
    line_4 = "Vertical gap between TF coil and VV lower = {0:.3f} m\n".format(
        vertical_gap_lower
    )

    # Write data to file
    output_file.write(line_1)
    output_file.write(line_2)
    output_file.write(line_3)
    output_file.write(line_4)
    output_file.write("\n")


def write_vacuum_vessel(data, output_file):
    """Function to write vacuum vessel data to the output file."""

    # Description line
    output_file.write("# Vacuum vessel (Ellipses)\n")

    # Vacuum vessel ellipse centre y point
    output_file.write("#Ellipse centre y position = 0.000 m\n")

    # Vacuum vessel ellipse centre x point
    ellipse_centre = data.r0 - data.delta * data.a
    output_file.write(
        "vacuum vessel - Ellipse centre = {0:.3f} m\n".format(ellipse_centre)
    )

    # Vacuum vessel ellipse minor radius, outside inboard
    outside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
    )
    output_file.write(
        "vacuum vessel - Ellipse minor radius - "
        "outside inboard = {0:.3f} m\n".format(outside_inboard_ellipse_minor_radius)
    )

    # Vacuum vessel ellipse major radius - outside upper
    if data.i_single_null == 0:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            + data.shldlth
            + data.d_vv_bot
        )
    else:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgaptop
            + data.fwtth
            + data.blnktth
            + data.vvblgap
            + data.shldtth
            + data.d_vv_top
        )
    output_file.write(
        "vacuum vessel - Ellipse major radius - "
        "outside upper = {0:.3f} m\n".format(outside_upper_ellipse_major_radius)
    )

    # Vacuum vessel ellipse major radius - outside lower
    outside_lower_ellipse_major_radius = (
        data.plasma_half_height + data.vgap + data.divfix + data.shldlth + data.d_vv_bot
    )
    output_file.write(
        "vacuum vessel - Ellipse major radius - "
        "outside lower = {0:.3f} m\n".format(outside_lower_ellipse_major_radius)
    )

    # Vacuum vessel ellipse minor radius - outside outboard
    outside_outboard_ellipse_minor_radius = (
        data.delta * data.a
        + data.a
        + data.scraplo
        + data.fwoth
        + data.blnkoth
        + data.vvblgap
        + data.shldoth
        + data.d_vv_out
    )
    output_file.write(
        "vacuum vessel - Ellipse minor radius - "
        "outside outboard = {0:.3f} m\n".format(outside_outboard_ellipse_minor_radius)
    )

    # Vacuum vessel ellipse minor radius - inside inboard
    inside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
    )
    output_file.write(
        "vacuum vessel - Ellipse minor radius - "
        "inside inboard = {0:.3f} m\n".format(inside_inboard_ellipse_minor_radius)
    )

    # Vacuum vessel ellipse major radius - inside upper
    if data.i_single_null == 0:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgap + data.divfix + data.shldlth
        )
    else:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgaptop
            + data.fwtth
            + data.blnktth
            + data.vvblgap
            + data.shldtth
        )
    output_file.write(
        "vacuum vessel - Ellipse major radius - "
        "inside upper = {0:.3f} m\n".format(inside_upper_ellipse_major_radius)
    )

    # Vacuum vessel ellipse major radius - inside lower
    inside_lower_ellipse_major_radius = (
        data.plasma_half_height + data.vgap + data.divfix + data.shldlth
    )
    output_file.write(
        "vacuum vessel - Ellipse major radius - "
        "inside lower = {0:.3f} m\n".format(inside_lower_ellipse_major_radius)
    )

    # Vacuum vessel ellipse minor radius - inside outboard
    inside_outboard_ellipse_minor_radius = (
        data.delta * data.a
        + data.a
        + data.scraplo
        + data.fwoth
        + data.blnkoth
        + data.vvblgap
        + data.shldoth
    )
    output_file.write(
        "vacuum vessel - Ellipse minor radius - "
        "inside outboard = {0:.3f} m\n".format(inside_outboard_ellipse_minor_radius)
    )

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")

        a = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="r",
            linewidth="2",
        )
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=270.0,
            edgecolor="r",
            linewidth="2",
        )
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="r",
            linewidth="2",
        )
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=270.0,
            theta2=0.0,
            edgecolor="r",
            linewidth="2",
        )
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)

        e = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="r",
            linewidth="2",
        )
        e.set_clip_box(ax.bbox)
        e.set_alpha(data.alpha)
        ax.add_artist(e)

        f = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=270.0,
            edgecolor="r",
            linewidth="2",
        )
        f.set_clip_box(ax.bbox)
        f.set_alpha(data.alpha)
        ax.add_artist(f)

        g = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="r",
            linewidth="2",
        )
        g.set_clip_box(ax.bbox)
        g.set_alpha(data.alpha)
        ax.add_artist(g)

        h = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=270.0,
            theta2=0.0,
            edgecolor="r",
            linewidth="2",
        )
        h.set_clip_box(ax.bbox)
        h.set_alpha(data.alpha)
        ax.add_artist(h)

        ax.set_xlim([0, 20])
        ax.set_ylim([-12, 12])


def write_shield(data, output_file):
    """Function to write the shield information to the output_file."""

    # # Description line
    output_file.write("\n# Shield (Ellipses)\n")

    # Ellipse centre y position
    output_file.write("#Ellipse centre y position = 0.000 m\n")

    # Ellipse centre x position
    ellipse_centre = data.r0 - data.delta * data.a
    output_file.write("Shield - Ellipse centre = {0:.3f} m\n".format(ellipse_centre))

    # Outside inboard ellipse minor radius
    outside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
    )
    output_file.write(
        "Shield - Ellipse minor radius - "
        "outside inboard = {0:.3f} m\n".format(outside_inboard_ellipse_minor_radius)
    )

    # Outside upper ellipse major radius
    if data.i_single_null == 0:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgap + data.divfix + data.shldlth
        )
    else:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgaptop
            + data.fwtth
            + data.blnktth
            + data.vvblgap
            + data.shldtth
        )
    output_file.write(
        "Shield - Ellipse major radius - "
        "outside upper = {0:.3f} m\n".format(outside_upper_ellipse_major_radius)
    )

    # Outside lower ellipse major radius
    outside_lower_ellipse_major_radius = (
        data.plasma_half_height + data.vgap + data.divfix + data.shldlth
    )
    output_file.write(
        "Shield - Ellipse major radius - "
        "outside lower = {0:.3f} m\n".format(outside_lower_ellipse_major_radius)
    )

    # Outside outboard ellipse minor radius
    outside_outboard_ellipse_minor_radius = (
        data.delta * data.a
        + data.a
        + data.scraplo
        + data.fwoth
        + data.blnkoth
        + data.vvblgap
        + data.shldoth
    )
    output_file.write(
        "Shield - Ellipse minor radius - "
        "outside outboard = {0:.3f} m\n".format(outside_outboard_ellipse_minor_radius)
    )

    # Inside inboard ellipse minor radius
    inside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
    )
    output_file.write(
        "Shield - Ellipse minor radius - "
        "inside inboard = {0:.3f} m\n".format(inside_inboard_ellipse_minor_radius)
    )

    # Inside upper ellipse major radius
    if data.i_single_null == 0:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgap + data.divfix
        )
    else:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgaptop
            + data.fwtth
            + data.blnktth
            + data.vvblgap
        )
    output_file.write(
        "Shield - Ellipse major radius - "
        "inside upper = {0:.3f} m\n".format(inside_upper_ellipse_major_radius)
    )

    # Inside lower ellipse major radius
    inside_lower_ellipse_major_radius = (
        data.plasma_half_height + data.vgap + data.divfix
    )
    output_file.write(
        "Shield - Ellipse major radius - "
        "inside lower = {0:.3f} m\n".format(inside_lower_ellipse_major_radius)
    )

    # Inside outboard ellipse minor radius
    inside_outboard_ellipse_minor_radius = (
        data.delta * data.a
        + data.a
        + data.scraplo
        + data.fwoth
        + data.blnkoth
        + data.vvblgap
    )
    output_file.write(
        "Shield - Ellipse minor radius - "
        "inside outboard = {0:.3f} m\n".format(inside_outboard_ellipse_minor_radius)
    )

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")

        a = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="b",
            linewidth="2",
        )
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=270.0,
            edgecolor="b",
            linewidth="2",
        )
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="b",
            linewidth="2",
        )
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=270.0,
            theta2=0.0,
            edgecolor="b",
            linewidth="2",
        )
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)

        e = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="b",
            linewidth="2",
        )
        e.set_clip_box(ax.bbox)
        e.set_alpha(data.alpha)
        ax.add_artist(e)

        f = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=270.0,
            edgecolor="b",
            linewidth="2",
        )
        f.set_clip_box(ax.bbox)
        f.set_alpha(data.alpha)
        ax.add_artist(f)

        g = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="b",
            linewidth="2",
        )
        g.set_clip_box(ax.bbox)
        g.set_alpha(data.alpha)
        ax.add_artist(g)

        h = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=270.0,
            theta2=0.0,
            edgecolor="b",
            linewidth="2",
        )
        h.set_clip_box(ax.bbox)
        h.set_alpha(data.alpha)
        ax.add_artist(h)

        ax.set_xlim([0, 20])
        ax.set_ylim([-12, 12])


def write_blanket(data, output_file):
    """Function to write the blanket information to the output_file."""

    # Description line
    output_file.write("\n# Blanket (Ellipses)\n")

    # Blanket ellipse centre y position
    output_file.write("# Ellipse centre y position = 0.000 m\n")

    # Blanket ellipse centre x position
    ellipse_centre = data.r0 - data.delta * data.a
    output_file.write("Blanket - Ellipse centre = {0:.3f} m\n".format(ellipse_centre))

    # Outside inboard ellipse minor radius
    outside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
        - data.vvblgap
    )
    output_file.write(
        "Blanket - Ellipse minor radius - "
        "outside inboard = {0:.3f} m\n".format(outside_inboard_ellipse_minor_radius)
    )

    # Outside upper ellipse major radius
    if data.i_single_null == 0:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgap + data.divfix
        )
    else:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgaptop + data.fwtth + data.blnktth
        )
    output_file.write(
        "Blanket - Ellipse major radius - "
        "outside upper = {0:.3f} m\n".format(outside_upper_ellipse_major_radius)
    )

    # Outside lower ellipse major radius
    outside_lower_ellipse_major_radius = (
        data.plasma_half_height + data.vgap + data.divfix
    )
    output_file.write(
        "Blanket - Ellipse major radius - "
        "outside lower = {0:.3f} m\n".format(outside_lower_ellipse_major_radius)
    )

    # Outside outboard ellipse minor radius
    outside_outboard_ellipse_minor_radius = (
        data.delta * data.a + data.a + data.scraplo + data.fwoth + data.blnkoth
    )
    output_file.write(
        "Blanket - Ellipse minor radius - "
        "outside outboard = {0:.3f} m\n".format(outside_outboard_ellipse_minor_radius)
    )

    # Inside inboard ellipse minor radius
    inside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
        - data.vvblgap
        - data.blnkith
    )
    output_file.write(
        "Blanket - Ellipse minor radius - "
        "inside inboard = {0:.3f} m\n".format(inside_inboard_ellipse_minor_radius)
    )

    # Inside upper ellipse major radius
    if data.i_single_null == 0:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
        )
    else:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgaptop + data.fwtth
        )
    output_file.write(
        "Blanket - Ellipse major radius - "
        "inside upper = {0:.3f} m\n".format(inside_upper_ellipse_major_radius)
    )

    # Inside lower ellipse major radius
    if data.i_single_null == 0:
        inside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
        )
    else:
        inside_lower_ellipse_major_radius = (
            data.plasma_half_height + data.vgap + data.divfix - data.blnktth
        )
    output_file.write(
        "Blanket - Ellipse major radius - "
        "inside lower = {0:.3f} m\n".format(inside_lower_ellipse_major_radius)
    )

    # Inside outboard ellipse minor radius
    inside_outboard_ellipse_minor_radius = (
        data.delta * data.a + data.a + data.scraplo + data.fwoth
    )
    output_file.write(
        "Blanket - Ellipse minor radius - "
        "inside outboard = {0:.3f} m\n".format(inside_outboard_ellipse_minor_radius)
    )

    # Blanket - lower angle inboard = 120 deg
    output_file.write("Blanket - lower angle inboard = 120 deg\n")

    # Blanket - lower angle outboard = 120 deg
    output_file.write("Blanket - lower angle outboard = 120 deg\n")

    # Blanket - lower angle height = 4.050 m
    output_file.write(
        "Blanket - lower angle height = {0:.3f} m\n".format(data.plasma_half_height)
    )

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")

        a = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="g",
            linewidth="2",
        )
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=270.0,
            edgecolor="g",
            linewidth="2",
        )
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="g",
            linewidth="2",
        )
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=270.0,
            theta2=0.0,
            edgecolor="g",
            linewidth="2",
        )
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)

        e = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="g",
            linewidth="2",
        )
        e.set_clip_box(ax.bbox)
        e.set_alpha(data.alpha)
        ax.add_artist(e)

        f = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=225.0,
            edgecolor="g",
            linewidth="2",
        )
        f.set_clip_box(ax.bbox)
        f.set_alpha(data.alpha)
        ax.add_artist(f)

        g = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="g",
            linewidth="2",
        )
        g.set_clip_box(ax.bbox)
        g.set_alpha(data.alpha)
        ax.add_artist(g)

        h = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=315.0,
            theta2=0.0,
            edgecolor="g",
            linewidth="2",
        )
        h.set_clip_box(ax.bbox)
        h.set_alpha(data.alpha)
        ax.add_artist(h)

        ax.set_xlim([0, 20])
        ax.set_ylim([-12, 12])


def write_first_wall(data, output_file):
    """Function to write the first wall information to the output_file."""

    # Description line
    output_file.write("\n# First Wall (Ellipses)\n")

    # First wall ellipse centre y position
    output_file.write("#Ellipse centre y position = 0.000 m\n")

    # First wall ellipse centre x position
    ellipse_centre = data.r0 - data.delta * data.a
    output_file.write(
        "First Wall - Ellipse centre = {0:.3f} m \n".format(ellipse_centre)
    )

    # Outside inboard ellipse minor radius
    outside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
        - data.vvblgap
        - data.blnkith
    )
    output_file.write(
        "First Wall - Ellipse minor radius - "
        "outside inboard = {0:.3f} m\n".format(outside_inboard_ellipse_minor_radius)
    )

    # Outside upper ellipse major radius
    if data.i_single_null == 0:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
        )
    else:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgaptop + data.fwtth
        )
    output_file.write(
        "First Wall - Ellipse major radius - "
        "outside upper = {0:.3f} m\n".format(outside_upper_ellipse_major_radius)
    )

    # Outside lower ellipse major radius
    if data.i_single_null == 0:
        outside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
        )
    else:
        outside_lower_ellipse_major_radius = (
            data.plasma_half_height + data.vgap + data.divfix - data.blnktth
        )
    output_file.write(
        "First Wall - Ellipse major radius - "
        "outside lower = {0:.3f} m\n".format(outside_lower_ellipse_major_radius)
    )

    # Outside outboard ellipse minor radius
    outside_outboard_ellipse_minor_radius = (
        data.delta * data.a + data.a + data.scraplo + data.fwoth
    )
    output_file.write(
        "First Wall - Ellipse minor radius - "
        "outside outboard = {0:.3f} m\n".format(outside_outboard_ellipse_minor_radius)
    )

    # Inside outboard lower ellipse minor radius ???

    # Inside inboard ellipse minor radius
    inside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
        - data.vvblgap
        - data.blnkith
        - data.fwith
    )
    output_file.write(
        "First Wall - Ellipse minor radius - "
        "inside inboard = {0:.3f} m\n".format(inside_inboard_ellipse_minor_radius)
    )

    # Inside upper ellipse major radius
    if data.i_single_null == 0:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
            - (data.fwith + data.fwoth) / 2.0
        )
    else:
        inside_upper_ellipse_major_radius = data.plasma_half_height + data.vgaptop
    output_file.write(
        "First Wall - Ellipse major radius - "
        "inside upper = {0:.3f} m\n".format(inside_upper_ellipse_major_radius)
    )

    # Inside lower ellipse major radius
    if data.i_single_null == 0:
        inside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
            - (data.fwith + data.fwoth) / 2.0
        )
    else:
        inside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - data.blnktth
            - data.fwtth
        )
    output_file.write(
        "First Wall - Ellipse major radius - "
        "inside lower = {0:.3f} m\n".format(inside_lower_ellipse_major_radius)
    )

    # Inside outboard ellipse minor radius
    inside_outboard_ellipse_minor_radius = data.delta * data.a + data.a + data.scraplo
    output_file.write(
        "First Wall - Ellipse minor radius - "
        "inside outboard = {0:.3f} m\n".format(inside_outboard_ellipse_minor_radius)
    )

    # Inside outboard lower ellipse minor radius ???

    # First Wall - lower angle inboard = 120 deg
    output_file.write("First Wall - lower angle inboard = 120 deg\n")

    # First Wall - lower angle outboard = 120 deg
    output_file.write("First Wall - lower angle outboard = 120 deg\n")

    # First Wall - lower angle height = 4.050 m
    output_file.write(
        "First Wall - lower angle height = {0:.3f} m\n".format(data.plasma_half_height)
    )

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")

        a = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="b",
            linewidth="2",
        )
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=225.0,
            edgecolor="b",
            linewidth="2",
        )
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="b",
            linewidth="2",
        )
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=315.0,
            theta2=0.0,
            edgecolor="b",
            linewidth="2",
        )
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)

        e = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="b",
            linewidth="2",
        )
        e.set_clip_box(ax.bbox)
        e.set_alpha(data.alpha)
        ax.add_artist(e)

        f = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=225.0,
            edgecolor="b",
            linewidth="2",
        )
        f.set_clip_box(ax.bbox)
        f.set_alpha(data.alpha)
        ax.add_artist(f)

        g = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="b",
            linewidth="2",
        )
        g.set_clip_box(ax.bbox)
        g.set_alpha(data.alpha)
        ax.add_artist(g)

        h = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=315.0,
            theta2=0.0,
            edgecolor="b",
            linewidth="2",
        )
        h.set_clip_box(ax.bbox)
        h.set_alpha(data.alpha)
        ax.add_artist(h)

        ax.set_xlim([0, 20])
        ax.set_ylim([-12, 12])


def write_tungsten_armour(data, output_file):
    """Function to write the tungsten armour information to the output_file."""

    # Description line
    output_file.write("\n# Tungsten Shielding (Ellipses)\n")

    # First wall ellipse centre y position
    output_file.write("#Ellipse centre y position = 0.000 m\n")

    # First wall ellipse centre x position
    ellipse_centre = data.r0 - data.delta * data.a
    output_file.write(
        "Tungsten Shielding - Ellipse centre = {0:.3f} m \n".format(ellipse_centre)
    )

    # Outside inboard ellipse minor radius
    outside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
        - data.vvblgap
        - data.blnkith
        - data.fwith
    )
    output_file.write(
        "Tungsten Shielding - Ellipse minor radius - "
        "outside inboard = {0:.3f} m\n".format(outside_inboard_ellipse_minor_radius)
    )

    # Outside upper ellipse major radius
    if data.i_single_null == 0:
        outside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
            - 0.5 * (data.fwith + data.fwoth)
        )
    else:
        outside_upper_ellipse_major_radius = data.plasma_half_height + data.vgaptop
    output_file.write(
        "Tungsten Shielding - Ellipse major radius - "
        "outside upper = {0:.3f} m\n".format(outside_upper_ellipse_major_radius)
    )

    # Outside lower ellipse major radius
    if data.i_single_null == 0:
        outside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
            - 0.5 * (data.fwith + data.fwoth)
        )
    else:
        outside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - data.blnktth
            - data.fwtth
        )
    output_file.write(
        "Tungsten Shielding - Ellipse major radius - "
        "outside lower = {0:.3f} m\n".format(outside_lower_ellipse_major_radius)
    )

    # Outside outboard ellipse minor radius
    outside_outboard_ellipse_minor_radius = data.delta * data.a + data.a + data.scraplo
    output_file.write(
        "Tungsten Shielding - Ellipse minor radius - "
        "outside outboard = {0:.3f} m\n".format(outside_outboard_ellipse_minor_radius)
    )

    # Inside outboard lower ellipse minor radius ???

    # Inside inboard ellipse minor radius
    inside_inboard_ellipse_minor_radius = (
        data.r0
        - data.delta * data.a
        - data.bore
        - data.ohcth
        - data.precomp
        - data.gapoh
        - data.tfcth
        - data.deltf
        - data.thshield
        - data.gapds
        - data.d_vv_in
        - data.shldith
        - data.vvblgap
        - data.blnkith
        - data.fwith
        - 0.003
    )
    output_file.write(
        "Tungsten Shielding - Ellipse minor radius - "
        "inside inboard = {0:.3f} m\n".format(inside_inboard_ellipse_minor_radius)
    )

    # Inside upper ellipse major radius
    if data.i_single_null == 0:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
            - (data.fwith + data.fwoth) / 2.0
            - 0.003
        )
    else:
        inside_upper_ellipse_major_radius = (
            data.plasma_half_height + data.vgaptop - 0.003
        )
    output_file.write(
        "Tungsten Shielding - Ellipse major radius - "
        "inside upper = {0:.3f} m\n".format(inside_upper_ellipse_major_radius)
    )

    # Inside lower ellipse major radius
    if data.i_single_null == 0:
        inside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - 0.5 * (data.blnkith + data.blnkoth)
            - (data.fwith + data.fwoth) / 2.0
            - 0.003
        )
    else:
        inside_lower_ellipse_major_radius = (
            data.plasma_half_height
            + data.vgap
            + data.divfix
            - data.blnktth
            - data.fwtth
            - 0.003
        )
    output_file.write(
        "Tungsten Shielding - Ellipse major radius - "
        "inside lower = {0:.3f} m\n".format(inside_lower_ellipse_major_radius)
    )

    # Inside outboard ellipse minor radius
    inside_outboard_ellipse_minor_radius = (
        data.delta * data.a + data.a + data.scraplo - 0.003
    )
    output_file.write(
        "Tungsten Shielding - Ellipse minor radius - "
        "inside outboard = {0:.3f} m\n".format(inside_outboard_ellipse_minor_radius)
    )

    # Inside outboard lower ellipse minor radius ???

    # First Wall - lower angle inboard = 120 deg
    output_file.write("First Wall - lower angle inboard = 120 deg\n")

    # First Wall - lower angle outboard = 120 deg
    output_file.write("First Wall - lower angle outboard = 120 deg\n")

    # First Wall - lower angle height = 4.050 m
    output_file.write(
        "First Wall - lower angle height = {0:.3f} m\n".format(data.plasma_half_height)
    )

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")

        a = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="b",
            linewidth="2",
        )
        a.set_clip_box(ax.bbox)
        a.set_alpha(data.alpha)
        ax.add_artist(a)

        b = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_inboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=225.0,
            edgecolor="b",
            linewidth="2",
        )
        b.set_clip_box(ax.bbox)
        b.set_alpha(data.alpha)
        ax.add_artist(b)

        c = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="b",
            linewidth="2",
        )
        c.set_clip_box(ax.bbox)
        c.set_alpha(data.alpha)
        ax.add_artist(c)

        d = Arc(
            (ellipse_centre, 0.0),
            2.0 * outside_outboard_ellipse_minor_radius,
            2.0 * outside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=315.0,
            theta2=0.0,
            edgecolor="b",
            linewidth="2",
        )
        d.set_clip_box(ax.bbox)
        d.set_alpha(data.alpha)
        ax.add_artist(d)

        e = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=90.0,
            theta2=180.0,
            edgecolor="b",
            linewidth="2",
        )
        e.set_clip_box(ax.bbox)
        e.set_alpha(data.alpha)
        ax.add_artist(e)

        f = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_inboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=180.0,
            theta2=225.0,
            edgecolor="b",
            linewidth="2",
        )
        f.set_clip_box(ax.bbox)
        f.set_alpha(data.alpha)
        ax.add_artist(f)

        g = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_upper_ellipse_major_radius,
            angle=0.0,
            theta1=0.0,
            theta2=90.0,
            edgecolor="b",
            linewidth="2",
        )
        g.set_clip_box(ax.bbox)
        g.set_alpha(data.alpha)
        ax.add_artist(g)

        h = Arc(
            (ellipse_centre, 0.0),
            2.0 * inside_outboard_ellipse_minor_radius,
            2.0 * inside_lower_ellipse_major_radius,
            angle=0.0,
            theta1=315.0,
            theta2=0.0,
            edgecolor="b",
            linewidth="2",
        )
        h.set_clip_box(ax.bbox)
        h.set_alpha(data.alpha)
        ax.add_artist(h)

        ax.set_xlim([0, 20])
        ax.set_ylim([-12, 12])


def write_plasma_shape(data, output_file):
    """Function to give information for plasma shape"""

    r0 = data.r0
    a = data.a
    delta = data.delta95 * 1.5
    kappa = data.kappa95 * 1.12

    # x position of outside arc
    x1 = (2 * r0 * (1 + delta) - a * (delta**2 + kappa**2 - 1)) / (2 * (1 + delta))

    # x position of inside arc
    x2 = (2 * r0 * (delta - 1) - a * (delta**2 + kappa**2 - 1)) / (2 * (delta - 1))

    # Radius of outside arc
    r1 = 0.5 * scipy.sqrt(
        (a**2 * ((delta + 1) ** 2 + kappa**2) ** 2) / ((delta + 1) ** 2)
    )

    # Radius of inside arc
    r2 = 0.5 * scipy.sqrt(
        (a**2 * ((delta - 1) ** 2 + kappa**2) ** 2) / ((delta - 1) ** 2)
    )

    # Angle for arc for outside arc
    theta1 = scipy.arcsin((kappa * a) / r1)

    # Angle for arc for inside arc
    theta2 = scipy.arcsin((kappa * a) / r2)

    # ?
    inang = 1.0 / r1
    outang = 1.5 / r2

    # Array of angle values for outside arc
    if data.i_single_null == 0:
        angles_1 = scipy.linspace(
            -(inang + theta1) + scipy.pi,
            (inang + theta1) + scipy.pi,
            100,
            endpoint=True,
        )
    else:
        angles_1 = scipy.linspace(
            -theta1 + scipy.pi, (inang + theta1) + scipy.pi, 100, endpoint=True
        )

    # Array of angle values for inside arc
    if data.i_single_null == 0:
        angles_2 = scipy.linspace(
            -(outang + theta2), (outang + theta2), 100, endpoint=True
        )
    else:
        angles_2 = scipy.linspace(-(outang + theta2), theta2, 100, endpoint=True)

    # x, y points for outside arc
    xs1 = -(r1 * scipy.cos(angles_1) - x1)
    ys1 = r1 * scipy.sin(angles_1)

    # x, y points for inside arc
    xs2 = -(r2 * scipy.cos(angles_2) - x2)
    ys2 = r2 * scipy.sin(angles_2)

    if data.plot:
        # Plot outside arc
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")
        ax.plot(xs1, ys1, color="orange")
        # Plot inside arc
        ax.plot(xs2, ys2, color="orange")

    # Description line
    output_file.write("\n# Plasma Arc\n")
    output_file.write("Y	Z Catia values\n")

    # Write data for plasma arcs
    n = int(len(xs1))
    for i in range(n):
        line = "Point.{0} = {1:.3f} {2:.3f}\n".format(i + 1, xs1[i], ys1[i])
        output_file.write(line)

    m = int(len(xs2))
    for j in range(m):
        line = "Point.{0} = {1:.3f} {2:.3f}\n".format(n + j + 1, xs2[j], ys2[j])
        output_file.write(line)


def ellips_fill(
    axis, a1=0, a2=0, b1=0, b2=0, x0=0, y0=0, ang1=0, ang2=np.pi / 2, color="pink"
):
    """Fills the space between two concentric ellipse sectors.

    Arguments

    axis: plot object
    a1, a2, b1, b2 horizontal and vertical radii to be filled
    x0, y0 coordinates of centre of the ellipses
    ang1, ang2 are the polar angles of the start and end

    """
    angs = np.linspace(ang1, ang2, endpoint=True)
    r1 = ((np.cos(angs) / a1) ** 2 + (np.sin(angs) / b1) ** 2) ** (-0.5)
    xs1 = r1 * np.cos(angs) + x0
    ys1 = r1 * np.sin(angs) + y0
    angs = np.linspace(ang2, ang1, endpoint=True)
    r2 = ((np.cos(angs) / a2) ** 2 + (np.sin(angs) / b2) ** 2) ** (-0.5)
    xs2 = r2 * np.cos(angs) + x0
    ys2 = r2 * np.sin(angs) + y0
    verts = list(zip(xs1, ys1))
    verts.extend(list(zip(xs2, ys2)))
    endpoint = verts[-1:]
    verts.extend(endpoint)
    path = Path(verts, closed=True)
    patch = patches.PathPatch(path, facecolor=color, lw=0)
    axis.add_patch(patch)


def write_cutf_coils(data, output_file):
    # Description line
    output_file.write("\n# TF Coil Outboard leg\n")

    output_file.write("TF Coil - Number = {0:.0f} \n".format(data.n_tf))

    # Point 1 - top inboard
    output_file.write("TF Coil - point 1 R position = {0:.3f} m\n".format(data.xarc_1))
    output_file.write("TF Coil - point 1 Z position = {0:.3f} m\n".format(data.yarc_1))

    # Point 2 - top outboard
    output_file.write("TF Coil - point 2 R position = {0:.3f} m\n".format(data.xarc_2))
    output_file.write("TF Coil - point 2 Z position = {0:.3f} m\n".format(data.yarc_2))

    # Point 3 - bottom outboard
    output_file.write("TF Coil - point 3 R position = {0:.3f} m\n".format(data.xarc_4))
    output_file.write("TF Coil - point 3 Z position = {0:.3f} m\n".format(data.yarc_4))

    # Point 4 - bottom inboard
    output_file.write("TF Coil - point 4 R position = {0:.3f} m\n".format(data.xarc_5))
    output_file.write("TF Coil - point 4 Z position = {0:.3f} m\n".format(data.yarc_5))

    # Outboard TF coil radial thickness
    output_file.write(
        "TF Coil - outboard leg - thickness = {0:.3f} m\n".format(data.tfthko)
    )

    output_file.write("\n# TF Coil Centrepost\n")
    r1 = 0.5 * (data.rmid + data.rtop)
    z1 = 0.5 * data.ztop
    xpost = (r1 - data.rmid) ** 2 + z1**2
    ypost = data.ztop**2 / ((data.rtop - data.rmid) ** 2 + data.ztop**2)
    rcpost = data.rmid + (xpost / (1.0 - ypost)) ** 0.5

    output_file.write("Centrepost - radius mid = {0:.3f} m\n".format(data.rmid))
    output_file.write("Centrepost - radius top = {0:.3f} m\n".format(data.rtop))
    output_file.write("Centrepost - taper height = {0:.3f} m\n".format(data.ztop))
    output_file.write("Centrepost - height = {0:.3f} m\n".format(data.hmax))
    output_file.write("Centrepost - arc centre R = {0:.3f} m\n".format(rcpost))
    output_file.write("Centrepost - arc centre Z = 0.000 m\n")
    output_file.write(
        "Centrepost - arc radius = {0:.3f} m\n".format(rcpost - data.tfcth)
    )


def write_tf_coils(data, output_file):
    """Function to plot TF coils"""

    # Calc shift
    top = (
        data.plasma_half_height
        + data.vgaptop
        + data.fwtth
        + data.blnktth
        + data.vvblgap
        + data.shldtth
        + data.d_vv_top
        + data.vgap2
        + data.thshield
        + data.tftsgap
        + data.tfcth
    )

    bottom = (
        -data.plasma_half_height
        - data.vgap
        - data.divfix
        - data.shldlth
        - data.d_vv_bot
        - data.vgap2
        - data.thshield
        - data.tftsgap
        - data.tfcth
    )

    shift = (top + bottom) / 2.0

    # Arc radii
    rad_1 = scipy.sqrt(
        (data.xarc_1 - data.xctfc_1) ** 2 + (data.yarc_1 - data.yctfc_1) ** 2
    )
    rad_2 = scipy.sqrt(
        (data.xarc_2 - data.xctfc_2) ** 2 + (data.yarc_2 - data.yctfc_2) ** 2
    )
    rad_3 = scipy.sqrt(
        (data.xarc_3 - data.xctfc_3) ** 2 + (data.yarc_3 - data.yctfc_3) ** 2
    )
    rad_4 = scipy.sqrt(
        (data.xarc_4 - data.xctfc_4) ** 2 + (data.yarc_4 - data.yctfc_4) ** 2
    )

    # Description line
    output_file.write("\n# TF Coil\n")

    # Arc 1
    output_file.write("TF Coil - arc 1 centre X = {0:.3f} m\n".format(data.xctfc_1))
    output_file.write(
        "TF Coil - arc 1 centre Y = {0:.3f} m\n".format(data.yctfc_1 + shift)
    )
    output_file.write("TF Coil - arc 1 radius = {0:.3f} m\n".format(rad_1))
    # output_file.write("TF Coil - arc 1 position 1 - inside X = {0:.3f} m\n".
    #                  format(data.xarc_1))
    # output_file.write("TF Coil - arc 1 position 1 - inside Y = {0:.3f} m\n".
    #                  format(data.yarc_1 + shift))
    # output_file.write("TF Coil - arc 1 position 2 - inside X = {0:.3f} m\n".
    #                  format(data.xarc_2))
    # output_file.write("TF Coil - arc 1 position 2 - inside Y = {0:.3f} m\n".
    #                  format(data.yarc_2 + shift))

    # Arc 2
    output_file.write("TF Coil - arc 2 centre X = {0:.3f} m\n".format(data.xctfc_2))
    output_file.write(
        "TF Coil - arc 2 centre Y = {0:.3f} m\n".format(data.yctfc_2 + shift)
    )
    output_file.write("TF Coil - arc 2 radius = {0:.3f} m\n".format(rad_2))
    # output_file.write("TF Coil - arc 2 position 2 - inside X = {0:.3f} m\n".
    #                  format(data.xarc_3))
    # output_file.write("TF Coil - arc 2 position 2 - inside Y = {0:.3f} m\n".
    #                  format(data.yarc_3 + shift))

    # Arc 3
    output_file.write("TF Coil - arc 3 centre X = {0:.3f} m\n".format(data.xctfc_3))
    output_file.write(
        "TF Coil - arc 3 centre Y = {0:.3f} m\n".format(data.yctfc_3 + shift)
    )
    output_file.write("TF Coil - arc 3 radius = {0:.3f} m\n".format(rad_3))
    # output_file.write("TF Coil - arc 3 position 2 - inside X = {0:.3f} m\n".
    #                  format(data.xarc_4))
    # output_file.write("TF Coil - arc 3 position 2 - inside Y = {0:.3f} m\n".
    #                  format(data.yarc_4 + shift))

    # Arc 4
    output_file.write("TF Coil - arc 4 centre X = {0:.3f} m\n".format(data.xctfc_4))
    output_file.write(
        "TF Coil - arc 4 centre Y = {0:.3f} m\n".format(data.yctfc_4 + shift)
    )
    output_file.write("TF Coil - arc 4 radius = {0:.3f} m\n".format(rad_4))
    # output_file.write("TF Coil - arc 4 position 2 - inside X = {0:.3f} m\n".
    #                  format(data.xarc_5))
    # output_file.write("TF Coil - arc 4 position 2 - inside Y = {0:.3f} m\n".
    #                  format(data.yarc_5 + shift))

    # Inboard TF coil
    output_file.write("\n# TF Coil - inboard leg - Wedged\n")

    # Inboard TF coil centre x position
    tf_coil_centre_x = data.bore + data.ohcth + data.gapoh + data.tfcth / 2.0
    output_file.write(
        "TF Coil - inboard leg - centre X = {0:.3f} m\n".format(tf_coil_centre_x)
    )

    # Inboard TF coil centre y position
    output_file.write("TF Coil - inboard leg - centre Y = {0:.3f} m\n".format(0.0))

    # Inboard TF coil radial thickness
    output_file.write(
        "TF Coil - inboard leg - radial thickness = {0:.3f} m\n".format(data.tfcth)
    )

    # Outboard TF coil
    output_file.write("\n# TF Coil - outboard leg - Not wedged\n")

    # Outboard TF coil centre X position
    tf_coil_centre_x = (
        data.r0
        + data.delta * data.a
        + data.a
        + data.scraplo
        + data.fwoth
        + data.blnkoth
        + data.shldoth
        + data.d_vv_out
        + data.gapsto
        + data.tfthko / 2.0
    )
    output_file.write(
        "TF Coil - inboard leg - centre X = {0:.3f} m\n".format(tf_coil_centre_x)
    )

    # Outboard TF coil centre y position
    output_file.write("TF Coil - outboard leg - centre Y = {0:.3f} m\n".format(0.0))

    # Outboard TF coil radial thickness
    output_file.write(
        "TF Coil - outboard leg - radial thickness = {0:.3f} m\n".format(data.tfthko)
    )

    # Outboard TF coil tangential thickness
    output_file.write(
        "TF Coil - outboard leg - tangential thickness = {0:.3f}"
        " m\n".format(data.tfthko)
    )

    if data.plot:

        x1 = data.__dict__["xarc_1"]
        y1 = data.__dict__["yarc_1"]
        x2 = data.__dict__["xarc_2"]
        y2 = data.__dict__["yarc_2"]
        x3 = data.__dict__["xarc_3"]
        x4 = data.__dict__["xarc_4"]
        y4 = data.__dict__["yarc_4"]
        x5 = data.__dict__["xarc_5"]
        y5 = data.__dict__["yarc_5"]
        tfcth = data.__dict__["tfcth"]

        rtangle = np.pi / 2
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")
        # Inboard upper arc
        x0 = x2
        y0 = y1
        a1 = x2 - x1
        b1 = y2 - y1
        a2 = a1 + tfcth
        b2 = b1 + tfcth
        ellips_fill(
            ax,
            a1=a1,
            a2=a2,
            b1=b1,
            b2=b2,
            x0=x0,
            y0=y0,
            ang1=rtangle,
            ang2=2 * rtangle,
            color="0.8",
        )
        # Outboard upper arc
        x0 = x2
        y0 = 0
        a1 = x3 - x2
        b1 = y2
        a2 = a1 + tfcth
        b2 = b1 + tfcth
        ellips_fill(
            ax,
            a1=a1,
            a2=a2,
            b1=b1,
            b2=b2,
            x0=x0,
            y0=y0,
            ang1=0,
            ang2=rtangle,
            color="0.8",
        )
        # Inboard lower arc
        x0 = x4
        y0 = y5
        a1 = x4 - x5
        b1 = y5 - y4
        a2 = a1 + tfcth
        b2 = b1 + tfcth
        ellips_fill(
            ax,
            a1=a1,
            a2=a2,
            b1=b1,
            b2=b2,
            x0=x0,
            y0=y0,
            ang1=-rtangle,
            ang2=-2 * rtangle,
            color="0.8",
        )
        # Outboard lower arc
        x0 = x4
        y0 = 0
        a1 = x3 - x2
        b1 = -y4
        a2 = a1 + tfcth
        b2 = b1 + tfcth
        ellips_fill(
            ax,
            a1=a1,
            a2=a2,
            b1=b1,
            b2=b2,
            x0=x0,
            y0=y0,
            ang1=0,
            ang2=-rtangle,
            color="0.8",
        )
        # Vertical leg
        # Bottom left corner
        rect = patches.Rectangle(
            [x5 - tfcth, y5], tfcth, (y1 - y5), lw=0, facecolor="0.8"
        )
        ax.add_patch(rect)


def write_pf_coils(data, output_file):
    """Function to write PF coil data"""

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

    # If Central Solenoid present, ignore last entry in for loop
    # The last entry will be the OH coil in this case
    if data.iohcl == 0:
        number_of_coils += 1

    # Rest of the coils
    for coil in range(1, number_of_coils):
        coils_r.append(data.mfile.data["rpf(%s)" % str(coil).zfill(2)].get_scan(-1))
        coils_z.append(data.mfile.data["zpf(%s)" % str(coil).zfill(2)].get_scan(-1))
        coils_dr.append(data.mfile.data["pfdr%s" % str(coil).zfill(2)].get_scan(-1))
        coils_dz.append(data.mfile.data["pfdz%s" % str(coil).zfill(2)].get_scan(-1))
        coil_text.append(str(coil))

    if data.plot:
        fig = plt.figure(1)
        ax = fig.add_subplot(111, aspect="equal")

    # Description line
    output_file.write("\n# PF coil information")

    for i in range(len(coils_r)):

        # points for plotting
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

        output_file.write("\n# PF coil: {0}\n".format(coil_text[i]))

        output_file.write(
            "Coil {0} - height = {1:.3f} m\n".format(coil_text[i], coils_z[i])
        )
        output_file.write(
            "Coil {0} - radius = {1:.3f} m\n".format(coil_text[i], coils_r[i])
        )
        output_file.write(
            "Coil {0} - coil height = {1:.3f} m\n".format(coil_text[i], coils_dz[i])
        )
        output_file.write(
            "Coil {0} - coil width = {1:.3f} m\n".format(coil_text[i], coils_dr[i])
        )

        if data.plot:

            ax.plot(r_points, z_points, color="black")
            ax.text(
                coils_r[i],
                coils_z[i],
                coil_text[i],
                ha="center",
                va="center",
                fontsize="smaller",
            )


def angle_check(angle1, angle2):
    """Function to perform TF coil angle check"""
    if angle1 > 1:
        angle1 = 1
    if angle1 < -1:
        angle1 = -1
    if angle2 > 1:
        angle2 = 1
    if angle2 < -1:
        angle2 = -1
    return angle1, angle2


def write_cad_data(data, output_file):
    """Function for writing information from data into output_file"""

    # Write cylinder data
    write_machine_bore(data, output_file)

    # Write OH coil data
    write_oh_coil(data, output_file)

    # Write gap between OH coil and TF coil data (gapoh)
    write_oh_gap(data, output_file)

    # Write cryostat data
    write_cryostat(data, output_file)

    # Write TF coil to vacuum vessel gap data
    write_tf_to_vv_gap(data, output_file)

    # Write vacuum vessel data
    write_vacuum_vessel(data, output_file)

    # Write shield data
    write_shield(data, output_file)

    # Write blanket data
    write_blanket(data, output_file)

    # Write first wall data
    write_first_wall(data, output_file)

    # Write tungsten armour
    write_tungsten_armour(data, output_file)

    # Write TF coil data
    if data.i_tf_sup == 0:
        write_cutf_coils(data, output_file)
    else:
        write_tf_coils(data, output_file)

    # Write PF coil data
    write_pf_coils(data, output_file)

    # Write plasma shape
    write_plasma_shape(data, output_file)

    # Show plot if plot option chosen
    if data.plot:
        plt.show()


def cad_main(cl_args):
    """Function to write PROCESS CAD output to file PROCESS.CAD"""

    # Read MFILE
    m_file = mf.MFile(cl_args.f)

    # Setup CAD file
    cad_file = open(cl_args.o, "w")

    # Write CAD file header
    write_header(cad_file, m_file)

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
    parser = argparse.ArgumentParser(
        description="Produce a CAD output file "
        "of the PROCESS MFILE file for a given "
        "scan. For info contact "
        "james.morris2@ukaea.uk"
    )

    # Argument for the input file path
    parser.add_argument(
        "-f",
        metavar="FILENAME",
        type=str,
        default="MFILE.DAT",
        help="specify input filename",
    )

    # Argument for the output file name
    parser.add_argument(
        "-o",
        metavar="OUTPUT",
        type=str,
        default="PROCESS.CAD",
        help="specify output filename",
    )

    # Argument for showing the output to screen also
    parser.add_argument(
        "-s",
        "--show",
        help="show plot as well as saving " "figure",
        action="store_true",
    )

    args = parser.parse_args()

    # pass MFILE object to cad_main
    cad_main(args)
