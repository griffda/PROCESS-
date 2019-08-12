
# Radial and Vertical Build

The figure below shows schematically the layout of a typical tokamak
as modelled by PROCESS. This is the so-called 'build' of the machine, the
relative locations of the major components is shown below. 

<figure>
    <img src="/img/build_d.eps" alt="tokamak-layout-d" 
    title="Machine build for D-shaped major components" 
    width="650" height="100" />
    <center>
        <br>
        <figcaption><i>Figure 1: Machine build for D-shaped major components</i></figcaption>
        <br>
    </center>
</figure>

<figure>
    <img src="/img/build_e.eps" alt="tokamak-layout-e" 
    title="Machine build for elliptical major components" 
    width="650" height="100" />
    <center>
        <br>
        <figcaption><i>Figure 2: Machine build for elliptical major components</i></figcaption>
        <br>
    </center>
</figure>

Their positions are referenced to the $(R,Z)$ coordinate system, where $R$ is 
the radial distance from the vertical centreline (axis) of the torus, and $Z$ is 
the vertical distance from the equatorial midplane, about which the machine is 
assumed to be up-down symmetrical (by default; the vertical build is slightly 
different for single null plasma devices (see Figure 3 below).

<figure>
    <img src="/img/build_e_snd.eps" alt="tokamak-layout-e-sn" 
    title="Machine build for elliptiacal major components single-null" 
    width="650" height="100" />
    <center>
        <br>
        <figcaption><i>Figure 3: Machine build for elliptical major components 
        single-null</i></figcaption>
        <br>
    </center>
</figure>

Components are often referred to as being inboard or outboard, which simply 
means that they lie at a radius $R$ less than or greater than $R_0$, 
respectively, where $R_0$ is the plasma major radius (`rmajor`). For the sake 
of clarity the thicknesses are not drawn to scale, and the space labelled as 
the divertor does not indicate in any way the actual shape of that component.

The first wall, blanket, shield and vacuum vessel may be either D-shaped in
cross-section, or each may be defined by two half-ellipses; compare their
shapes in Figures 1 and 2. The choice between these two possibilities is set 
using input parameter `fwbsshape`, which should be

- 1 for D-shaped,
- 2 for ellipses.

Most of the thicknesses shown in Figures 1 and 2 are input parameters, so are 
not changed during the course of the simulation. The rest are calculated by the 
code during execution. In addition, some of the component sizes can be used 
as iteration variables (see [input file page](io/input-guide.md) and 
[solver page](solver-guide.md)) to help in the optimisation process.
