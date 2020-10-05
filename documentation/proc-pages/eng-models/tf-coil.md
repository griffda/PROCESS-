# TF Coil Model

The toroidal field (TF) coils can be either resistive or superconducting. The 
choice of conductor design is set using the following integer switch 
`i_tf_sup`:

- `i_tf_sup == 0` -- for resistive copper (GLIDCOP) magnets with active water 
  cooling. 
- `i_tf_sup == 1` -- for Superconducting magnets. The CICC(Conductor In Cable 
Conduit) structure shown in Figure 1 is assumed. The steel radial plates shown 
in Figure 1 helps in the winding process and also provide extra support against  tangential stresses. Iteration variable no. 101 (`prp`) is the ratio of the
total radial plate plus steel cap cross-sectional area within the winding pack 
to the total winding pack cross-sectional area. From this, the half-thickness 
`trp` is calculated. The coils are cooled to 4 K a liquid helium cryogenic 
system. 
- `i_tf_sup == 2` -- for resistive aluminium (high purity) magnet with active
gaseous Helium cooling at cryogenic temperature (20 K).

The toroidal field (TF) coils can be either resistive or superconducting. The 
choice of conductor design is set using the following integer switch 
`i_tf_sup`:

- `i_tf_sup == 0` -- for resistive copper (GLIDCOP) magnets with active water 
  cooling. 
- `i_tf_sup == 1` -- for Superconducting magnets. The CICC(Conductor In Cable 
Conduit) structure shown in Figure 1 is assumed. The steel radial plates shown 
in Figure 1 helps in the winding process and also provide extra support against  tangential stresses. Iteration variable no. 101 (`prp`) is the ratio of the
total radial plate plus steel cap cross-sectional area within the winding pack 
to the total winding pack cross-sectional area. From this, the half-thickness 
`trp` is calculated. The coils are cooled to 4 K a liquid helium cryogenic 
system. 
- `i_tf_sup == 2` -- for resistive aluminium (high purity) magnet with active
gaseous Helium cooling at cryogenic temperature (20 K).


<figure>
    <center>
    <img src="../../img/tokamak_tfcoil.png" alt="tok_tfcoil" 
    title="Schematic diagram of tokamak TF coil" 
    width="550" height="100" />
    <br><br>
    <figcaption><i>Figure 1: Schematic diagram of the cross-section of the 
    inboard leg of a superconducting TF coil, showing the CICC (Conductor In 
    Cable Conduit) construction. The winding pack contains many turns of cable 
    conduit. The cable space contains the superconducting filaments, and 
    circulating liquid helium coolant. The variables shown in \Red{red} may 
    be changed by the user, and those in italics may be chosen as iteration 
    variables.
    </i></figcaption>
    <br>
    </center>
</figure>

Two vertical shapes can be selected for the coil designs using the `i_tf_shape` 
integer switch:

- `i_tf_shape == 1` -- for D-shaped magnets. Each TF coil is defined in the 
(R,Z) plane by a straight section and four elliptical arcs. This designed is 
considered by default for conventional aspect ratio tokamaks (`itart == 0`) as 
vertical forces remains important in the TF outboard section, necessiting the 
use of a shape close to the constant tension Princenton D.
- `i_tf_shape == 2` -- for picture frame coils. Each coils has a rectangular
shape, allowing space for an eventual super-X divertor. However this design can
only be used for at low aspect ratio, characterized by low vertical forces on 
the TF outboard section.

The outboard leg of the TF coil is assumed to be the same width in the
toroidal direction as the outside edge of the inboard leg. In the radial
direction, for resistive TF coils the input parameter `tfootfi` gives the ratio 
of the outboard leg thickness to the inboard leg thickness `tfcth`; for 
superconducting coils the outboard thickness is set equal to the inboard 
thickness.

Another aspect of the TF coil design is the presence of demountable joints, that
ease the remote maintenance. These joints can be either clamped or sliding.
PROCESS proposes an option (`itart == 1`) for TF design with sliding joints 
between the inboard section of the magnets, called the centrepost (CP) and the 
outer legs of the coils. This design has been mostly developed for spherical
tokamak using resistive coils (`i_tf_shape == 0,2`). To reduced the resistive 
losses keeping the magnet as thin as possible at mid-plane (to lower the aspect 
ratio), a tapered geometry is assumed as shown in Figure 2.


<figure>
    <center>
    <img src="../../img/ST_geom.png" alt="tok_tfcoil" 
    title="Schematic diagram of TART tokamak TF coil" 
    width="550" height="100" />
    <br><br>
    <figcaption><i>Figure 2: Mid-plane toroidal (left) and vertical (right) 
    cross-section of a magnet using the itart == 1 geometry option. The toroidal 
    cross-section (left) shows the presence of vaulted turn geometry with a
    bucking cylinder ( that is not present by default for copper magnets) with 
    insulation and cooling. The vertical cross-section (right) shows the 
    presence of 4 sliding joints for remote maintenance purposes.
    </i></figcaption>
    <br>
    </center>
</figure>


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

Because of the finite number of TF coils used in a tokamak 
(18 for ITER), the toroidal field has a ripple introduced into it, the 
amplitude of which can be limited to a few percent (given by input parameter 
`ripmax`, default value 1\%) by the code by adjusting the outboard gap 
thickness (`gapsto`).

Among the TF coil parameters calculated by the code are the maximum allowable
current density, the stresses on the structure, the energy stored and the
magnetic field produced by the coils.

The following options are available within the superconducting TF coil model
(`i_tf_sup = 1`).

## Superconducting materials

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

## Current density limits

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
  activated in any given run).

## Stress model

Switch  `tfc_model` controls whether a simple stress model
( `tfc_model = 0`, suitable for solid copper TF coils) or a more
complex stress model ( `tfc_model == 1`) should be used. If
 `tfc_model == 1`}, a two-layer stress model~\cite{Morris_tfc} developed
by CCFE is used.

To enforce the stress limits calculated using either of these models,
constraint equation no.\ 31 (case stress) and/or constraint equation no.\ 32
(conduit stress) should be turned on with iteration variables no.\ 48
(`fstrcase`) and/or no.\ 49 ( `fstrcond`), respectively. The
stress limit is set using input parameter  `alstrtf`}.

[^1]: $J_c(B,T,\epsilon)$ Parameterizations for the ITER Nb$_3$Sn Production',
ITER Document 2MMF7J (2008), \texttt{https://user.iter.org/?uid=2MMF7J\&action=get\_document}