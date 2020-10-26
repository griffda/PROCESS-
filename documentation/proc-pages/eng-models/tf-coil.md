# Toroidal field coils (TF)

<p style='text-align: justify;'>
This section presents the models used to integrate TF coils models in the PROCESS machine design and how to use them. The TF coil module computes first the coil current from the specified plasma major radius and toroidal magnet field. The exact inboard leg mid-plane cross section geometry up to the conductor level is then set. The vertical geometry is set and the TF components masses are deduced. The inboard mod-plane stress distributions, the coil inductance and the ripple are then estimated. Finally, the resistive heating (if resistive coil) and the ratio between the critical current density and the conductor current density (superconducting coil) is estimated. The non-latex variables names corresponds to their name in the `PROCESS` code.
</p>


<p style='text-align: justify;'>
Two major types of toroidal field (TF) coils can be considered in PROCESS: Resistive magnets or Superconducting magnets. 
The conductor and its support structure strongly differs between these two options that will be described separately in the next section. The choice of conductor type is made using the following integer switch:</p> 


- `i_tf_sup == 0` : Resistive copper (GLIDCOP) magnets with active water
  cooling. 
- `i_tf_sup == 1` : Superconducting magnets with active Helium cooling (4.75 K inlet Helium).
- `i_tf_sup == 2` : Resistive aluminium (high purity) magnet with active Helium cooling at at low temperature (15-30 K inlet helium).

## TF coils currents

<p style='text-align: justify;'>
The total current flowing in a TF coil \( I_\mathrm{TF}^\mathrm{tot} \) (ritfc) is estimated calculated assuming axisymmetry (the coil system is a perfect continuous torus) from the magnetic field on plasma axis \( B_\mathrm{T} \) (bt) and the radius and the plasma major radius \( R_\mathrm{maj} \) (rmajor):
</p>

$$
I_\mathrm{TF}^\mathrm{tot} = \frac{2\pi}{\mu_0} B_\mathrm{T} R_\mathrm{maj}
$$

<p style='text-align: justify;'>
This approximation is however not exactly true in Tokamaks, as discrete coils sets are used for obvious reasons. Current discontinuity in the toroidal direction induces field variation vertical and toroidal direction (ripple), changing the needed \( I_\mathrm{TF}^\mathrm{tot} \) to provide the wanted toroidal field intensity at plasma centre.
</p>

## TF coil inboard mid-plane geometry

<p style='text-align: justify;'>
This section describes TF coil inboard leg geometry of the cross-section defined by z=0. Setting precisely this geometry is crucial as it directly impacts both the TF structural property and the allowable current in the coil. As the design of resistive and superconducting coil are different, they are described separately.
</p>

### Superconducting coil geometry

<p style='text-align: justify;'>
The TF coils are assumed to be supporting each other against the net TF coils centering force, forming a vaulted structure (vaulted or wedged design). The individual coil structured, illustrated in <em>Figure 1</em>, can be separated in two main sections:</p>

- <p style='text-align: justify;'>
    **The winding pack (WP)** : section containing the superconducting cables (Blue area  in <em>Figure 1</em>). The ground insulation and the WP insertion gap (Dark grey area in <em>Figure 1</em>) is considered part of the WP by convention.
  </p>
- <p style='text-align: justify;'>
    **The steel casing**: Section holding the WP providing the necessary structural support (light grey area in <em>Figure 1</em>).
  </p>

<p style='text-align: justify;'> The next sub-section describes the different parametrization proposed in <em>PROCESS</em>:</p>

<figure>
    <center>
    <img src="/images/tfcoil_SC_inboard_structure.png" alt="SC_mid_plane_cross_section" 
    title="Inboard mid-plane superconducting TF coil cross-section" 
    width="600" height="150" />
    <br>
    <figcaption><i><p style='text-align: justify;'> 
      Figure 1: Inboard mid-plane cross-section of the superconducting TF coils considered in PROCESS with associated parametrization. The green variables are calculated by the machine_build module subroutine while the blue ones are specific to the sctfcoil module. The light grey area corresponds to the steel casing containing the Winding pack and providing structural support. The dark grey area corresponds to the winding pack insertion gap, and its ground insulation. Finally the light blue area corresponds to the winding pack containing the conductor cables. More details on the parametrization is discussed in this section.</p>
    </i></figcaption>
    <br>
    </center>
</figure>

#### TF coil inboard radial size

<p style='text-align: justify;'> 
  Following the geometry and its parametrization presented in <em>Figure 1</em>, the TF total thickness <em>tfcth</em> \( \left( \Delta R_\mathrm{TF} \right) \) is related with the inner and outer case radial thicknesses (<em>thkcas</em>, \(  \Delta R_\mathrm{case}^\mathrm{in} \) and <em>casthi</em>, \( \Delta R_\mathrm{case}^\mathrm{out} \) respectively) and the WP radial thickness <em>dr_tf_wp</em> \(\Delta R_\mathrm{WP}\):
</p>

$$ 
\Delta R_\mathrm{TF} = \frac{R_\mathrm{TF}^\mathrm{in} + \Delta R_\mathrm{WP} + \Delta R_\mathrm{case}^\mathrm{out} + \Delta R_\mathrm{case}^\mathrm{in} }{\cos\left(\frac{\pi}{N_\mathrm{TF}}\right)} - R_\mathrm{TF}^\mathrm{in}
$$

<p style='text-align: justify;'> 
  with \( R_\mathrm{TF}^\mathrm{in} \) (<em>r_tf_inboard_in</em>) the radius of the innermost TF edge, set by the central  solenoid coil size and \( N_\mathrm{TF} \) the number of TF coils. Reverted, to provide the WP thickness, the same equation becomes:
</p>

$$  
\Delta R_\mathrm{WP} = \cos\left(\frac{\pi}{N_\mathrm{TF}}\right) \left( R_\mathrm{TF}^\mathrm{in} + \Delta R_\mathrm{TF} \right) - R_\mathrm{TF}^\mathrm{in} - \Delta R_\mathrm{case}^\mathrm{out} - \Delta R_\mathrm{case}^\mathrm{in}
$$

<p style='text-align: justify;'> 
  The TF coil radial thickness (<em>tfcth</em>) can parametrized in two ways in <em>PROCESS</em>:
</p>
- <p style='text-align: justify;'> 
    **Direct parametrization**: The set the TF radial inboard thickness with the `tfcth` (iteration variable 57) <em>PROCESS</em> input variable. The WP radial thickness (`dr_tf_wp`) is calculated from `tfcth` and the different case radial thicknesses, using the equation. This parametrization is used by default.
  </p>
- <p style='text-align: justify;'> 
    **WP thickness parametrization**: the TF inboard radial thickness is calculated from the radial thicknesses of the case and the WP radial thickness. This option is selected by using the WP thickness (`dr_tf_wp`), iteration variable 140) as an iteration variable. Doing so, any `tfcth` values will be overwritten and for this reason `dr_tf_wp` cannot be used as iteration variables simultaneously (the <em>PROCESS</em> execution will ba halted with an error message). Although not set by default for backward compatibility, this parametrization provides a more stable optimization procedure as negative layer thicknesses cannot be obtained by construction and his hence encouraged.
  </p>

#### Case geometry


<p style='text-align: justify;'> 
  Although not physically divided into pieces, three section of the case can be considered: 
</p>
- <p style='text-align: justify;'> 
    **The nose casing:** this section corresponds to the case separating the WP with the machine center. Due to the presence of net electromechanical centering forces, this case has a major structural purpose and is often much larger than the other sides. The nose case dimension is set by its radial thickness that the user can specify using the `thkcas` (iteration variable 57) input variable.
  </p>
- <p style='text-align: justify;'> 
    **Sidewall casing:** this section corresponds to the lateral side of the case, separating the WP with the other vaulted coils. As in the WP geometry is generally squared, the sidewall case thickness may vary with the machine radius. For this reason, the user sets its dimensions though its minimal thickness `casths`. The user can either `casths` in the *PROCESS* input file, either define it as a fraction of the total coil thickness at the inner radius of the WP (`r_wp_inner`) with the `casths_fraction` input. If `casths_fraction` is defined in the input file, the `casths` value will be overwritten.
  </p>
- <p style='text-align: justify;'> 
    **Plasma side casing:** this section corresponds to the case, separating the WP with plasma. As the geometry of this section is rounded, its thickness is set by its minimal value `casthi` (user input). Like the sidewall case, this value can also be set as a fraction of the total TF coil thickness `tfcth` using `casthi_fraction`. If `casthi_fraction` is defined in the input file, the `casthi` value will be overwritten. 
  </p>

Two different plasma side casing shapes can be selected using the `i_tf_case_geom` integer switch:

- <p style='text-align: justify;'>
    `i_tf_case_geom = 0` : rounded plasma side case. As it is the ITER design choice, this configuration is taken as default.
  </p>
- <p style='text-align: justify;'>
    `i_tf_case_geom = 1` : Straight plasma side case.
  </p>

*Figure 2* illustrate the two plasma side casing configurations.

<figure>
    <center>
    <img src="/images/tfcoil_SC_plasma_case_option.png" alt="plasma side case geometry options" 
    title="plasma side case geometry options" 
    width="500" height="100" />
    <br>
    <figcaption><i><p style='text-align: justify;'> 
      Figure 2: Visual illustration of the two i_tf_case_geom front casing geometry option. The to diagram shows the default i_tf_case_geom = 0 rounded option and the bottom the i_tf_case_geom = 1 straight option.</p>
    </i></figcaption>
    <br>
    </center>
</figure>

#### Winding pack geometry

Several Winding pack geometries can chosen with the `i_tf_wp_geom` integer switch as shown in Figure 3: 

- <p style='text-align: justify;'>
    `i_tf_wp_geom = 0` : Rectangular winding pack. It is the only geometry compatible with the integer turn parametrization (`i_tf_turns_integer = 1`).
</p>
- <p style='text-align: justify;'>
    `i_tf_wp_geom = 1` : Double rectangle winding pack. The two rectangles are have the same radial thickness and their width in the toroidal direction is defined with the minimal sidewall thickness at their innermost radius.
  </p>
- <p style='text-align: justify;'> 
    `i_tf_wp_geom = 2` : Trapezoidal WP. The WP area is defined with a trapezoid, keeping the sidewall case thickness constant. This is however probably not a realistic shape as the turns are generally rectangular. This option has been added mostly to allow comparison with simplified FEA analysis configurations.
  </p>

<figure>
    <center>
    <img src="../../images/tfcoil_SC_WP_option.png" alt="WP geometries options" 
    title="Superconducting TF coil WP geometry"
    width="650"
    height="100"/>
    <br><br>
    <figcaption><i><p style='text-align: justify;'> 
      Figure 3: Visual illustration of the WP shapes the user can select with the i_tf_wp_geom integer switch. The wwp1 and wwp2 parameters, added in option i_tf_wp_geom = 1 are calculated using the minimal sidewall case thickness.
    </p></i></figcaption>
    <br>
    </center>
</figure>

#### Turns geometry

<p style='text-align: justify;'>
  Figure <em>Figure 4</em> illustrates the winding pack internal structure
  zooming in to show the individual turn structure. 
</p>

<figure>
    <center>
    <img src="../../images/tfcoil_SC_turn.png" alt="SC turn geometry" 
    title="Superconducting turn geometry"
    width="650"
    height="100"/>
    <br><br>
    <figcaption><i><p style='text-align: justify;'> 
      Figure 4: Illustration of the winding pack internal structure. The top 
      right diagram shows the inboard mid-plane cross section of a TF coil with
      the seel case in light grey and the winding pack in light blue. Each dotted
      line squares winding pack region illustrate the presence of rectangular
      turns. The bottom left zoom-in section show the turn structure used in the
      <em>PROCESS</em> module. The red section represent the individual turn
      electrical insulation, the turquoise blue the steel jacket/conduit providing
      structural support, the grey is the area allocated to the superconductor
      material/copper/void mixture making the SC cable and the white circle the
      helium cooling channel.
    </p></i></figcaption>
    <br>
    </center>
</figure>


The winding pack is assumed to be made of \(N_\mathrm{turn} \) (`n_tf_turn`) 
turns. The number of turns can be parametrized in three different ways :

- <p style='text-align: justify;'>
    **Current per turn parametrization (defaut):** `i_tf_turns_integer = 0` the
    user sets the value of the current flowing in each turns `cpttf`. The number
    of turns necessary to carry the total TF coil current is then deduced. There
    is no guarantee to have an integer number of turn using this parametrization.
    If the turn thickness `t_turn_tf` is defined by the user, this parametrization
    is not selected.
</p> 
- <p style='text-align: justify;'>
    **Turn size parametrization:** `i_tf_turns_integer = 0` the dimension of the
    turn `t_turn_tf` is set by the user. The area of the corresponding squared
    turn and the number of turns necessary to fill the WP area is deduced. There
    is also no guarantee to have an integer number of turn using this
    parametrization.
  </p>
- <p style='text-align: justify;'> 
    **Integer turn parametrization:** `i_tf_turns_integer = 1` the user sets the
    number of turn layer in the radial (`n_layer`) and in the toroidal direction
    (`n_pancake`). Using this parametrization an integer number of turn is 
    obtained. As the turn toroidal and radial dimensions are set independently
    form the WP ones, the turn shape is not always squared. Finally only a
    rectangular WP can be used for this parametrization.
  </p>

<p style='text-align: justify;'>  
  The turn internal structure, illustrated in <em>Figure 4</em>, is inspired 
  from the cable-in-conduit-conductor (CICC) design, with the main different
  being that a rounded squared cable space is used (grey area in <em>Figure 4
  </em>). The rounding curve radius is take as 0.75 of the steel conduit 
  thickness. The turn geometry is set with with the following thicknesses:
</p>
- <p style='text-align: justify;'>
    **Turn insulation thickness `thicndut`:** user input setting the thickness
    of the inter-turn insulation.
  </p>
- <p style='text-align: justify;'>
    **Steel jacket/conduit thickness `thwcndut` (iteration variable 58):** user
    input thickness of the turn steel structures. As it is a crucial variable
    for the TF coil structural properties it is also an iteration variable.
  </p>
- <p style='text-align: justify;'>
    **Helium cooling channel diameter `dhecoil`:** user input defining the 
    size of the cooling channel.
  </p>


#### Cable composition
<p style='text-align: justify;'>
  As illustrated in <em>Figure 5</em> showing a picture of the <em>ITER</em> 
  TF conductor, the actual conductor cable structure can be relatively complex.
</p>

<figure>
    <center>
    <img src="../../images/iter_fusion_tf_conductor.jpg" alt="SC turn geometry" 
    title="Superconducting turn geometry"
    width="500"
    height="100"/>
    <br><br>
    <figcaption><i><p style='text-align: justify;'> 
      Figure 5: Picture if the ITER cable-in-conduit-conductor (CICC) cable design
      with a helium cooling channel at its center covered with a mixture of
      copper and NB3Sn strands. Space remain between strands.
    </p></i></figcaption>
    <br>
    </center>
</figure>

<p style='text-align: justify;'>
  As the conductor cable composition is only used to correct the area used to
  compute current density flowing in the superconductor material, to be compared
  with its critical current density, an average material description is enough
  for the <em>PROCESS</em>models. The composition is set with the following
  material fractions:
</p>
- <p style='text-align: justify;'>
    **Cable coolant fraction (`vftf`):** user input setting the void fraction
    in the conductor space. This fraction does not include the Helium cooling
    pipe at the cable center.
  </p>
- <p style='text-align: justify;'> 
    **Copper fraction (`fcutfsu`):** user input setting the copper fraction.
    This fraction is applied after the void and helium cooling channels areas
    has been removed from the conductor area.
  </p>


### Resistive coil geometry

<p style='text-align: justify;'>
  A much simpler inboard mid-plane geometry is used for resistive TF coils,
  as shown in <em>Figure 6</em>. The most important difference is the absence of
  lateral steel casing structure, making the central section of all the coil,
  a common structure. All the coils section also share the same cylindrical
  symmetry, making the stress calculations much easier. Three main layers
  sections can be distinguished:
</p>

- <p style='text-align: justify;'>
    **The bucking cylinder:** parametrize with its thickness `thkcas` (iteration
    variable 57) is present to support electromechanical the centering forces.
    Its presence is however not mandatory and can be can be removed setting
  </p>
- <p style='text-align: justify;'> 
    **The conductor area:** parametrized with its thickness `dr_tf_wp` (iteration
    variable 140) containing the conductor turns. Ground insulation, corresponding
    to the dark grey area in *Figure 6* is included in this section by convention.
  </p>
- <p style='text-align: justify;'> 
    **The outer cylinder:** parametrized with its thickness `casthi` providing
    support for the conductor structure. This cylinder does however not help
    with the TF electromechanical centering forces.
  </p>

<figure>
    <center>
    <img src="../../images/tfcoil_res_inboard_geom.png" alt="Res_geom" 
    title="Schematic diagram of TART tokamak TF coil" 
    width="650" height="100" />
    <br><br>
    <figcaption><i>
      <p style='text-align: center;'>
        Figure 6: Resistive TF coil inboard leg structure at mid-plane.
      </p>
    </i></figcaption>
    <br>
    </center>
</figure>

<p style='text-align: justify;'>
  The conductor layer is made of \(N_\mathrm{turn}\) turns per coils, set by the
  <em>n_tf_turn</em> user input (one turn per coil is used by default). Each
  turn is made of a conductor wrapped in a turn insulation with a thickness set
  with the <em>thicndut</em> user input. The conductor is cooled using a fraction
  of the mid-plane cross-section are set by the <em>fcoolcp</em> (iteration
  variable 23).
</p>

<figure>
    <center>
    <img src="../../images/tfcoil_res_turn.png" alt="Res_geom" 
    title="Schematic diagram of TART tokamak TF coil" 
    width="200" height="200" />
    <br><br>
    <figcaption><i>
      <p style='text-align: center;'>
        Figure 7: Schematic view of a four resistive TF coil turns. 
      </p>
    </i></figcaption>
    <br>
    </center>
</figure>

## Vertical TF coil shape

### Coil shape

Two vertical shapes can be selected for the coil designs using the `i_tf_shape` 
integer switch:

- <p style='text-align: justify;'>
    **D-shape (`i_tf_shape == 1`):** simplified D-shape parametrization defined
    in the (R,Z) plane by a straight section and four elliptical arcs. The
    corresponding shape is not exactly the constant tension Princeton D but, but
    it is not so critical as the shape is mostly used to provide the coil 
    circumference and illustration purposes in the current version of the
    *PROCESS* code. This shape is considered by default for conventional aspect
    ratio tokamaks (`itart == 0`).
  </p>
- <p style='text-align: justify;'>
    **Picture frame `i_tf_shape == 2`:** rectangular shape, allowing space for
    an eventual super-X divertor. However this design can only be used for at
    low aspect ratio, characterized by low vertical forces on the TF outboard
    section. This shape is considered by default for low aspect ratios
    tokamaks (`itart == 1`).
  </p>

<p style='text-align: justify;'>
  Resistive coils can benefit in having a larger outboard conductor area than
  the inboard one, to reduce resistive heating without increasing the plasma
  major radius or increasing its aspect ratio. For this reason, thicker outboard
  can be set using the <em>tfootfi</em> user input defined as the ratio between
  ouboard and inboard TF coil legs. The possibility of having different coil
  thicknesses is only activated for the resistive coils and <em>tfootfi</em>
  would have no effect if superconducting coil are selected (<em>i_tf_sup = 1
  </em>).
</p>

### Resistive centre-post (CP)

<p style='text-align: justify;'>
  As the resistive heating depends on the magnet cross-section (in the plan
  perpendicular to the current direction), heating can be substantially reduced
  by flaring the central section of the TF coils. The corresponding shape is
  illustrated in the right hand side of <em>Figure 8</em>. The inboard
  mid-plane TF outer radius is then different from the top/bottom straight
  section one. Such design can be selected with the <em>itart = 1</em> integer
  switch.
</p>

<figure>
    <center>
    <img src="../../images/ST_geom.png" alt="tok_tfcoil" 
    title="Schematic diagram of TART tokamak TF coil" 
    width="600" height="100" />
    <br><br>
    <figcaption><i>
      <p style='text-align: justify;'>
        Figure 2: Mid-plane toroidal (left) and vertical (right) 
        cross-section of a magnet using the itart == 1 geometry option. The toroidal
        cross-section (left) shows the presence of vaulted turn geometry with a
        bucking cylinder ( that is not present by default for copper magnets) with
        insulation and cooling. The vertical cross-section (right) shows the
        presence of 4 sliding joints for remote maintenance purposes.
      </p>
    </i></figcaption>
    <br>
    </center>
</figure>

The TF top CP radius `r_cp_top` (iteration variable 174) can be set in three
different ways:

- <p style='text-align: justify;'>
    **Calculated (`i_r_cp_top = 0`, default):** , the top CP radius is calculated
    from the X-points positions. This option generally leads to a relatively
    strong flaring. This option also assumes that the radial build at the machine
    top is the same as the mid-plane one.
  </p>
- <p style='text-align: justify;'>
    **User input (`i_r_cp_top = 1`):** the user set the `r_cp_top` (iteration
    variable 174) value. If `r_cp_top` is lower than $1.01 R_\mathrm{TF}^\mathrm{out}$
    (TF inboard mid-plane outer radius), the TF top radius is set to
    `1.01*r_tf_inboard_out` with an lvl 2 error warning. On the other
    hand, if `r_cp_top` gets incompatible with the X-point position, a lvl error
    is also raised at the end of the *PROCESS* run.
  </p>
- <p style='text-align: justify;'>
    **Mid/top TF radius ratio (`i_r_cp_top = 2`):** `r_cp_top` is set as a ratio
    of the inboard mid-plane with the user input `f_r_cp`, defined as \( \frac{
    R_\mathrm{TF}^\mathrm{top} }{R_\mathrm{TF}^\mathrm{out}} \). If the 
    resulting `r_cp_top` gets incompatible with the X-point position, a lvl error
    is also raised at the end of the *PROCESS* run. This parametrization allows
    consistent machine size.
</p>

<p style='text-align: justify;'>
  The resistive heating, cooling, material masses are calculated taking the
  flaring into account, parametrized with an arc. The cooling cross-section
  is the same as the mid-plane one, making the cooling fraction fraction smaller
  at the top where less resistive heating is expected (larger conductor section). 
</p>

### TF coil joins

<p style='text-align: justify;'>
  Another aspect of the TF coil design is the presence of demountable joints.
  Having demountable joints can ease the maintenance strategy by allowing to
  remove the inboard section of the coil vertically without moving the
  outboard section of the Tokamak. Moreover, if the joints allows some
  longitudinal and toroidal movements (sliding joints), they can also ease the
  TF coils structural design by significantly reducing the inboard mid-plane
  vertical tension. Some other advantages not captured by process of sliding
  joins is the reduction of out-of-plane stresses generated from PF coils fields
  of the TF corners. Finally, although the increase of resistive power
  dissipation is took into account <em>PROCESS</em>, the technical feasibility
  of sliding joints with superconducting magnets is not assessed. The joint
  option can by selected by <em>i_cp_joints</em> switch:
</p>

- <p style='text-align: justify;'>
    **No TF joints (`i_cp_joints = 0`, SC magnets default):** this corresponds
    to a conservative DEMO/ITER like strategy making the coil more reliable at
    the cost of a more complex remote maintenance strategy.
  </p>
- <p style='text-align: justify;'>
    **Static joints (`i_cp_joints = 1`):** clamped re-mountable joints. This
    ease remote maintenance but does not simplify the coil structural design.
    However, clamped joints be might be easier and more reliable for SC magnets.
    The resistive losses are currently calculated using the sliding joint design
    by default. The user can however modify the joints surfacic resistivity
    (`rho_tf_joints`), the number of joints per coil (`n_tf_joints`), the number
    of contact per joints (`n_tf_joints_contact`) and the thickness of the joint
    contact (`th_joint_contact`) to adapt the model to the considered SC joint
    design.
  </p>
- <p style='text-align: justify;'>
    **Sliding joints (`i_cp_joints = 2`, resistive magnet default):** the joints
    is made of Feldmetal connectors allowing longitudinal sliding and toroidal
    torsion. For the TF inboard to joint, it allows to decouple the inboard TF
    coil vertical tension from the outer legs ones (effect captured in *PROCESS*)
    and cancel the out-of-plane (OOP) shear stress at the joints (effect not
    captured in *PROCESS* as OOP stress is not calculated). The joint resistive
    heating is also calculated and more specific design can be set by tuning the
    joints surfacic resistivity (`rho_tf_joints`), the number of joints per coil
    (`n_tf_joints`), the number of contact per joints (`n_tf_joints_contact`)
    and the thickness of the joint contact (`th_joint_contact`). It is possible
    to use sliding joints with SC coils in *PROCESS*, but it is not advised to
    do so except if the user has a very specific design in mind.
  </p>

## TF stress modelling

<p style='text-align: justify;'>
  The <em>PROCESS</em> TF coil module only considers the inboard mid-plane stress
  constraint. Although it is the most stringent design constraint for the
  machine build, out-of-plane stress, local bending stresses due to the coil
  shaping, ripple and inter-coil structure effects remains to be evaluated after  
  a <em>PROCESS</em> run. Stress limits can be introduced using the following
  constraints equations:
</p>

- <p style='text-align: justify;'>
    **Conducting layer stress limit (`icc = 32`):** stress limit on the
    conductor layer on the material take the most of stress that supports.
    For superconducting TF coils, the stress limit is applied on the steel case
    jacket/conduit while for resistive coil, the stress limit is applied on the
    conductor itself.
  </p>
- <p style='text-align: justify;'>
    **Centring support structure stress limit (`icc = 31`):** stress limit on the
    coil centering support structures. For superconducting coils, this stress
    limit is applied on the nose case (machine center direction) while for
    resistive coil this stress limit is applied on a cylindrical bucking
    cylinder.
  </p>

### Vertical tension

<p style='text-align: justify;'>
  The interaction between the TF coil field with the horizontal
  component of its current induces an identical and opposite vertical force
  \( F_\mathrm{z}\) on the upper and lower sections of the TF coils. If
  axisymmetry is assumed, this force only depends on the coil current
  \( \left( \frac{I_\mathrm{TF}^\mathrm{tot}}{N_\mathrm{TF}} \right) \)  
  and its radial build. If we parametrize the conductor layer radial build
  with its in/outboard (\(R_\mathrm{in}\)/\(R_\mathrm{out}\) ) plasma facing
  side radii and its thickness (\(\Delta R_\mathrm{cond} \)), the vertical
  force is given by:
</p>

$$
\begin{align}\label{eq: vforce}
	F_\mathrm{z} = \frac{ I_\mathrm{TF}^\mathrm{tot} }{N_\mathrm{TF}} 
   \frac{B_\mathrm{T} R_\mathrm{maj}}{2 {\Delta R_\mathrm{cond}}^2}  
    & \left[{R_\mathrm{out}}^2 \ln\left( \frac{R_\mathrm{out} + \Delta R_\mathrm{cond}}{R_\mathrm{out}} \right)  + {R_\mathrm{in}}^2 \ln\left( \frac{R_\mathrm{in} }{R_\mathrm{out} - \Delta R_\mathrm{cond}} \right) \right.  \\
	& \qquad \left. {} + {\Delta R_\mathrm{cond}}^2 \ln\left( \frac{R_\mathrm{out} + \Delta R_\mathrm{cond} }{R_\mathrm{in} - \Delta R_\mathrm{cond}} \right)  - \Delta R_\mathrm{cond}\left( R_\mathrm{in} + R_\mathrm{out} \right)   \right.\nonumber \\
	& \qquad \left. {}  2\Delta R_\mathrm{cond} \left\{ R_\mathrm{in}\ln\left(\frac{R_\mathrm{in}-\Delta R_\mathrm{cond}}{R_\mathrm{in}} \right) + R_\mathrm{out}\ln\left(\frac{R_\mathrm{out}+ \Delta R_\mathrm{cond}}{R_\mathrm{out}} + \right)   \right\}  \right] \nonumber
\end{align}
$$

<p style='text-align: justify;'>
  with \(B_\mathrm{T}\) and \(R_\mathrm{maj}\) the plasma center toroidal field
  and major radius, respectively. This force is distributed between inboard
  \( \left( T_\mathrm{z}^\mathrm{in}\right)\) and outboard \(\left(T_\mathrm{z}
  ^\mathrm{out}\right)\) legs. The resulting tensions are in general not equals
  as they depends on coil shape. This asymmetry is parametrized <em>PROCESS</em>
  buy the following user input <em>f_vforce_inboard</em> \(\left(f_{F_\mathrm{z}}
  ^\mathrm{in}\right)\) defined as:
</p>

$$
  T_\mathrm{z}^\mathrm{in} = f_{F_\mathrm{z}} F_\mathrm{z}
$$

<p style='text-align: justify;'>
  with \(T_\mathrm{z}^\mathrm{in}\) the vertical tension acting on the inboard
  leg of a TF coil. The <em>f_vforce_inboard</em> default value (0.5)
  corresponds to the special case of a perfect Princeton-D. On the other hand
  a picture frame coil can show larger asymmetries with <em>f_vforce_inboard</em>
  values around 0.65.
</p>

### Inboard mid-plane stress model

<p style='text-align: justify;'>
  Two models can be used to calculate inboard-midplane normal stresses radial
  distributions:
</p>
- <p style='text-align: justify;'>
    **Plane stress (`i_tf_plane_stress = 1`, default):** the calculations are made
    under the plane stress assumption. Plane strain model applies for thin 
    layer in the vertical direction. The radial and toroidal stress calculations
    are made assuming no vertical tension. A constant vertical stress is then
    estimated *a posteriori* by dividing the inboard vertical tension by the
    support sutructure area to obtain the 3 normal stress necessary to estimate
    the *TRESCA* yield stress radial distribution as described in [1]. Although
    using un-adapted model hypothesis this model is still used by default as it
    has been validated on FEA analysis and benchmarked with the *MADMAX* *CEA*
    code on DEMO designs.
</p>
- <p style='text-align: justify;'>
    **Generalized plane strain (`i_tf_plane_stress = 1`):** the
    calculations are made using the generalized plane strain hypothesis that
    applies for tall cylinders with external external force applied at the end
    (vertical tension). This is the correct assumption for inboard mid-plane
    stress calculations that allows to derive the vertical stress distribution
    coherently [2]. This formulation has been developed for transverse orthotropic
    materials (isotropic in the \(\left(r, \theta\right\) plane with different
    properties in the vertical direction). This allows to applies different
    Poisson's ratio, one for the \(\left(r, \theta\right\) plane to \(z\) and
    another for the radial to toroidal direction. Although more coherent and
    complete this model is not yet used by default as more FEA validations
    are needed.
  </p>

<p style='text-align: justify;'>
  These two models can use any number of material layers with different
  Young modulus and Poisson's ratios for each layers (two pairs of Young modulus
  and Poisson's ratio are used for the generalized plane strain model). As the
  conductor layer of a TF coil is relatively complex, smeared properties are
  used to set the Young modulus, as described in the next section.
</p>

#### Conductor/WP properties smearing

##### Resistive coil properties smearing

<p style='text-align: justify;'>
  The smearing 
</p>

#### Inboard TF coil support

The support structure of the TF coil is set by parameter `i_tf_bucking`:

- `i_tf_bucking == 0` -- for free standing TF without case/bucking structure 
  with only a (super-)conductor layer. This is the default option for copper 
  magnet design (`i_tf_sup = 0`), as the conductor(GLIDCOP) has already good 
  structural properties.
- `i_tf_bucking == 1` -- for free standing TF with a case/bucking structure. This is
  the default option for both the superconducting magnets (`i_tf_sup == 1`, that 
  has a relatively weak conductor structural properties as the conductor cable 
  has a low young modulus value. This is also the default value for the 
  aluminium magnets (`i_tf_sup == 2`), as the a aluminium has a low allowable
  TRESCA yield and as a bucking layer helps reducing the TRESCA stress from the
  conductor layer.
- `i_tf_bucking == 2,3` -- for design with the TF is in contact with the CS 
  (bucked and wedged design). This design use the CS as a support structure for
  the TF centering forces, allowing to substantially reduce the thickness of the
  bucking cylinder/the steel casing nose. This introduce a significant major 
  radius reduction. The calculation are made when no current flows in the CS 
  layer (i.e. without the beneficial CS hoop forces). If the stress on the CS 
  layer on this configuration is larger that the one calculated in the PF module,
  it is used for the CS stress constraints. A fast version, neglecting the 
  interface layer properties is implemented (`i_tf_bucking == 2`). And a more 
  complete one, taking explicitly the Kapton interface into account in the
  stress calcualtions (`i_tf_bucking == 3`) is also implemented.



## TF coil ripple 

Because of the finite number of TF coils used in a tokamak 
(18 for ITER), the toroidal field has a ripple introduced into it, the 
amplitude of which can be limited to a few percent (given by input parameter 
`ripmax`, default value 1\%) by the code by adjusting the outboard gap 
thickness (`gapsto`).

## TF coil stored energy

Among the TF coil parameters calculated by the code are the maximum allowable
current density, the stresses on the structure, the energy stored and the
magnetic field produced by the coils.


## Current density limits

### Superconducting magerial critical current

Switch `i_tf_sc_mat` specifies which superconducting material is to be used:

- `i_tf_sc_mat == 1` -- Nb$_3$Sn superconductor, ITER critical surface 
  parameterization[^1], standard critical values
- `i_tf_sc_mat == 2` -- Bi-2212 high temperature superconductor
- `i_tf_sc_mat == 3` -- NbTi superconductor
- `i_tf_sc_mat == 4` -- Nb$_3$Sn superconductor, ITER critical surface 
  parameterization[^1], user-defined critical parameters
- `i_tf_sc_mat == 5` -- WST Nb$_3$Sn parameterization
- `i_tf_sc_mat == 6` -- REBCO HTS tape in CroCo strand

The fraction of copper present in the superconducting filaments is given by
the value of variable `fcutfsu` (iteration variable number 59).

For `i_tf_sc_mat = 2`, a technology adjustment factor `fhts` may be used to modify 
the critical current density fit for the Bi-2212 superconductor, to describe the 
level of technology assumed (i.e. to account for stress, fatigue, radiation, 
AC losses, joints or manufacturing variations). The default value for `fhts` is 
0.5 (a value of 1.0 would be very optimistic).

For `i_tf_sc_mat = 4`, important superconductor properties may be input by the user 
as follows: the upper critical field at zero temperature and strain is set 
using input parameter `bcritsc`, and the critical temperature at zero field and 
strain is set using input parameter `tcritsc`.

For `i_tf_sc_mat = 6`, the turn deign is largely different from the one 
ITER one. 

The current in the TF coils must be sufficient to produce the required
toroidal field at the centre of the plasma. In tokamaks, the field falls off
at a rate $1/R$, with the peak value occurring at the outer edge of the
inboard portion of the TF coil winding pack ($R_{\mbox{max TF}} =
\mathtt{rbmax}$). The maximum TF coil current depends on the field it produces
and the allowable current density.

Three constraints are relevant to the operating current density $J_{\mbox{op}}$ 
in the (superconducting) TF coils.

- `Constraint 33` -- To ensure that $J_{\mbox{op}}$ does not exceed the critical value $J_{\mbox{crit}}$, constraint equation no.\ 33 should be turned on with iteration variable no.\ 50
  ( `fiooic`).

- `Constraint 35` -- To ensure that $J_{\mbox{op}}$ does not exceed the current 
  density protection limit, constraint equation no.\ 35 should be turned on with 
  iteration variable no.\ 53 ( `fjprot`).

- `Constraint 36` -- The critical current density $J_{\mbox{crit}}$ falls with 
  the temperature of the superconductor. The temperature margin $\Delta T$ is the difference between the temperature at which \jcrit would be equal to 
  $J_{\mbox{op}}$ and the operating temperature. The minimum allowed $\Delta T$
  can be set using input parameter `tmargmin` together with constraint equation 
  no.\ 36 and iteration variable no. 54 ( `ftmargtf`). Note that if the temperature margin is positive, $J_{\mbox{op}}$ is guaranteed to be. lower than \jcrit, and so constraints~33 and~36 need not both be turned on. (in fact, it is recommended that only one of these two constraints is
  activated in  

### Resistive heating

## Code structure

These models are coded in the `sctfcoil` subroutine, structured in the following way:

1. `tf_global_geometry` : In/outboard leg areas at mid-lane
2. `tf_current` : Calculate the TF coil currents
3. `sc_tf_internal_geom`/`res_tf_internal_geom` : Set the exact superconducting/resistive magnets inboard mid-plane geometry, including the Turn geometry.
4. `coilhap`: Define the vertical TF coil shape
5. `tf_res_heating`: Estimate the TF coil resistive heating (not used for SC magnets)
6. `tf_field_and_force`: Estimate the inboard/outboard vertical tensions
7. `tfcind`: Estimate the TF coil inductance
8. `tf_coil_area_and_masses`: Estimate the mass of the different coil materials
9. `peak_tf_with_ripple`: Estimate the ripple peak field correction.
10. `stresscl`: Estimate the inboard mid-plane stress distributions.

Another subroutine, `tfspcall` is called outside `stfcoil` to estimate to check on the TF superconducting properties.

## TF coil parameter summary table


[^]: $J_c(B,T,\epsilon)$ Parameterizations for the ITER Nb$_3$Sn Production',
ITER Document 2MMF7J (2008), \texttt{https://user.iter.org/?uid=2MMF7J\&action=get\_document}