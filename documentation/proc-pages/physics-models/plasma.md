
# Plasma

By default, the plasma is assumed to have an up-down asymmetric, single null
configuration (although this can be changed with user inputs). A great number 
of physics models are coded within PROCESS to describe the behaviour of the 
plasma parameters such as its current, temperature, density, pressure, 
confinement etc., and also the various limits that define the stable operating 
domain.

## Plasma Geometry

The plasma (geometric) major radius $R_0$ (`rmajor`) and aspect ratio $A$ (`aspect`) 
define the size of the plasma torus. The plasma minor radius $a$ (`rminor`) is 
calculated from these values. The shape of the plasma cross-section is given by 
its last closed flux surface (LCFS) elongation $\kappa$ (`kappa`) and triangularity 
$\delta$ (`triang`), which can be scaled automatically with the aspect ratio if 
required using switch `ishape`:

- `ishape = 0` -- the input values for `kappa` and `triang` are used directly.
- `ishape = 1` -- the following scaling is used, which is suitable for low aspect 
  ratio machines ($\epsilon = 1/A$) [^1]:
  $$
  \kappa = 2.05 \, (1 + 0.44 \, \epsilon^{2.1})
  $$
  $$
  \delta = 0.53 \, (1 + 0.77 \, \epsilon^3)
  $$
- `ishape = 2` -- the Zohm ITER scaling [^2] is used to calculate the elongation:
  $$
  \kappa = F_{kz} \, \times \, \mathrm{minimum} \left( 2.0, \, \, 1.5 + \frac{0.5}{A-1} \right)
  $$
  where input variable `fkzohm` $= F_{kz}$ may be used to adjust the
  scaling, while the input value of the triangularity is used unchanged.

If `ishape = 0, 1, 2`, the values for the plasma shaping parameters at the 95% 
flux surface are calculated as follows [^3]:
$$
\kappa_{95} = \kappa / 1.12
$$
$$
\delta_{95} = \delta / 1.5
$$

- If `ishape = 3`, the Zohm ITER scaling is used to calculate the elongation (as 
  for `ishape = 2` above), but the triangularity at the 95% flux surface is 
  input via variable `triang95`, and the LCFS triangularity `triang` is calculated
  from it, rather than the other way round.
- Finally, if `ishape = 4`, the 95% flux surface values `kappa95` and `triang95` 
  are both used as inputs, and the LCFS values are calculated from them by 
  inverting the equations above.

A constraint relating to the plasma's vertical stability may be turned on if
required. In principle, the inner surface of the outboard shield could be used
as the location of a conducting shell which should mitigate the vertical
displacement growth rate of plasmas with significant elongation [^4]. The 
maximum distance $r_{\text{shell, max}}$ of this shell from the 
centre of the plasma may be set using input parameter `cwrmax`, such that 
$r_{\text{shell, max}} =$ `cwrmax*rminor`. Constraint equation 
no. 23 should be turned on with iteration variable no.\ 104 (`fcwr`) to enforce 
this. (A scaling of `cwrmax` with elongation should be available shortly.)

The plasma surface area, cross-sectional area and volume are calculated using
formulations that approximate the LCFS as a revolution of two arcs which
intersect the plasma X-points and the plasma midplane outer and inner
radii. (This is a reasonable assumption for double-null diverted plasmas, but
will be inaccurate for single-null plasmas, `snull = 1`.) Switch `igeom` 
determines whether an old method is used (`igeom = 0`) or whether calculations 
based on a more recent derivation (`igeom = 1`).

## Fusion Reactions

The most likely fusion reaction to be utilised in a power plant is the
deuterium-tritium reaction:

$$
\mathrm{D + T} \Longrightarrow \mathrm{^{4}He + n + 17.6 \,MeV}
$$

20% of the energy produced is given to the alpha particles ($^4$He), a
fraction of which remain (c.f. `falpha`) within the plasma and thermalise (slow 
down) due to collisions, thus heating the plasma. The remaining 80% is carried 
away by the neutrons, which deposit their energy within the blanket and shield.

PROCESS can also model D-$^3$He power plants, which utilise the following 
primary fusion reaction:

$$
\mathrm{D + \text{$^3$He}} \Longrightarrow \mathrm{^{4}He + p + 18.3 \,MeV}
$$

The fusion reaction rate is significantly different to that for D-T fusion,
and the power flow from the plasma is modified since charged particles are
produced rather than neutrons. Because only charged particles (which remain in
the plasma) are produced by this reaction, the whole of the fusion power is
used to heat the plasma. Useful energy is extracted from the plasma since the
radiation power produced is very high, and this can be converted to
electricity in a number of ways.

Since the temperature required to ignite the D-$^3$He reaction is considerably
higher than that for D-T, it is necessary to take into account the following
D-D reactions, which have significant reaction rates at such temperatures:

$$\begin{eqnarray*}
\mathrm{D + D} & \Longrightarrow & \mathrm{^{3}He + n + 3.27 \,MeV} \\
\mathrm{D + D} & \Longrightarrow & \mathrm{T + p + 4.03 \,MeV}
\end{eqnarray*}$$

Also, as tritium is produced by the latter reaction, D-T fusion is also
possible. As a result, there is still a small amount of neutron power
extracted from the plasma.

Pure D-$^3$He tokamak power plants do not include blankets, because of the near
absence of neutrons leaving the plasma, and the fact that no tritium needs to
be produced for fuel.

The contributions from all four of the above fusion reactions are included in
the total fusion power production calculation. The fusion reaction rates are
calculated using the parametrizations in [^5], integrated over the plasma 
profiles (correctly, with or without pedestals).

The fractional composition of the 'fuel' ions (D, T and $^3$He) is
controlled using the three variables `fdeut`, `ftrit` and `fhe3`, respectively:

$$\begin{eqnarray*}
n_{\mbox{fuel}} & = & n_D + n_T + n_{\mathrm{^{3}He}}  \;\;\; \mbox{particles/m$^3$} \\
n_D & = & \mathtt{fdeut} \, n_{\mbox{fuel}} \\
n_T & = & \mathtt{ftrit} \, n_{\mbox{fuel}} \\
n_{\mathrm{^{3}He}} & = & \mathtt{fhe3} \, n_{\mbox{fuel}}
\end{eqnarray*}$$

PROCESS checks that $fdeut + ftrit + fhe3 = 1.0$, and stops with an error 
message otherwise.

## Plasma Profiles

If switch `ipedestal = 0`, the plasma profiles are assumed to be parabolic, 
i.e.they are of the form

$$\begin{eqnarray}
\mbox{Density : } n(\rho) & = & n_0 \left( 1 - \rho^2 \right)^{\alpha_n} \\
\mbox{Temperature : } T(\rho) & = & T_0 \left( 1 - \rho^2 \right)^{\alpha_T} \\
\mbox{Current : } J(r) & = & J_0 \left( 1 - \rho^2 \right)^{\alpha_J}
\end{eqnarray}$$

where $\rho = r/a$, and $a$ is the plasma minor radius. This gives
volume-averaged values $\langle n \rangle = n_0 / (1+\alpha_n)$, and
line-averaged values $\bar{n} \sim n_0 / \sqrt{(1+\alpha_n)}$, etc.  These
volume- and line-averages are used throughout the code along with the profile
indices $\alpha$, in the various physics models, many of which are fits to
theory-based or empirical scalings. Thus, the plasma model in \process\ may
be described as $\frac{1}{2}$-D.  The relevant profile index variables are
`alphan`, `alphat` and `alphaj`, respectively.

However, by default, `ipedestal = 1` which allows the density and 
temperature profiles to include a pedestal, using the forms specified in [^6]:

$$\begin{equation}
\mbox{density:} \qquad n(\rho) = \left\{ 
\begin{aligned}
  &n_{ped} + (n_0 - n_{ped}) \left( 1 -
    \frac{\rho^2}{\rho_{ped,n}^2}\right)^{\alpha_n}
  &\qquad 0 \leq \rho \leq \rho_{ped,n} \\
  &n_{sep} + (n_{ped} - n_{sep})\left( \frac{1- \rho}{1-\rho_{ped,n}}\right)
  &\qquad \rho_{ped,n} < \rho \leq 1
\end{aligned}
\right.
\end{equation}$$

$$\begin{equation}
\mbox{temperature:} \qquad T(\rho) = \left\{ 
\begin{aligned}
  &T_{ped} + (T_0 - T_{ped}) \left( 1 - \frac{\rho^{\beta_T}}
    {\rho_{ped,T}^{\beta_T}}\right)^{\alpha_T} &\qquad 0 \leq \rho \leq \rho_{ped,T} \\
  &T_{sep} + (T_{ped} - T_{sep})\left( \frac{1- \rho}{1-\rho_{ped,T}}\right)
  &\qquad \rho_{ped,T} < \rho \leq 1
\end{aligned}
\right.
\end{equation}$$

Subscripts $0$, $ped$ and $sep$, denote values at the centre ($\rho = 0$), the
pedestal ($\rho = \rho_{ped}$) and the separatrix ($\rho=1$),
respectively. The density and temperature peaking parameters $\alpha_n$ and a
$\alpha_T$ as well as the second exponent $\beta_T$ (input parameter
`tbeta`, not to be confused with the plasma beta) in the temperature
profile can be chosen by the user, as can the pedestal heights and the values
at the separatrix (`neped, nesep` for the electron density, and
`teped, tesep` for the electron temperature; the ion equivalents are
scaled from the electron values by the ratio of the volume-averaged values).

The density at the centre is given by

$$\begin{eqnarray}
  \nonumber
  n_0 &= & \frac{1}{3\rho_{ped,n}^2} \left[3\langle n\rangle (1+\alpha_n)
    + n_{sep} (1+\alpha_n) (-2 + \rho_{ped,n} + \rho_{ped,n}^2) \right.\\
  && \left. - n_{ped}\left( (1 + \alpha_n)(1+ \rho_{ped,n}) + (\alpha_n -2)
    \rho_{ped,n}^2 \right) \right]
\end{eqnarray}$$

where $\langle n \rangle$ is the volume-averaged density. The temperature at
the centre is given by

$$\begin{equation}
T_0 = T_{ped} + \gamma \left[ T_{ped}\, \rho_{ped,T}^2 - \langle T \rangle +
  \frac{1}{3}(1 - \rho_{ped,T}) \left[ \, (1 + 2\rho_{ped,T}) \, T_{ped} + ( 2 +
    \rho_{ped,T}) \, T_{sep} \, \right] \right]
\end{equation}$$

with 

$$\begin{equation}
\gamma = \left\{ 
\begin{aligned}
  &\frac{ -\Gamma(1+\alpha_T+2/\beta_T)}
  {\rho_{ped,T}^2 \, \Gamma(1+\alpha_T) \, \Gamma((2+\beta_T)/\beta_T)}
  &\qquad \text{for integer } \alpha_T \\
  &\frac{\Gamma(-\alpha_T)\sin(\pi\alpha)\, \Gamma(1+\alpha_T+2/\beta_T)}
  {\pi\rho_{ped,T}^2 \, \Gamma((2+\beta_T)/\beta_T)}
  &\qquad \text{for non-integer } \alpha_T
\end{aligned}
\right.
\end{equation}$$

where $\Gamma$ is the gamma function.

Note that density and temperature can have different pedestal positions
$\rho_{ped,n}$ (`rhopedn`) and $\rho_{ped,T}$ (`rhopedt`) in agreement with 
simulations.

The pedestal density can be set directly (if `iscdens=1`), or as a fraction of 
the Greenwald density (if `iscdens=1`).  The default fraction is 0.8[^7]. 

## Beta Limit

The plasma beta limit[^8] is given by 

$$\begin{equation}
\langle \beta \rangle < g \, \frac{I(\mbox{MA})}{a(\mbox{m}) \, B_0(\mbox{T})}
\end{equation}$$

where $B_0$ is the axial vacuum toroidal field, and $\beta$ is defined with
respect to the total equilibrium $\mathbf{B}$-field [^9]. The beta
coefficient $g$ is set using input parameter `dnbeta`. To apply the beta limit, 
constraint equation no. 24 should be turned on with iteration variable no. 36
(`fbetatry`). The limit can be applied to either the total plasma beta, in 
which case switch `iculbl` should be set to 0, to only the thermal component of 
the plasma beta, in which case `iculbl` should be set to 1, or to the thermal 
plus neutral beam components, in which case `iculbl` should be set to 2.

[^1]: J.D. Galambos, 'STAR Code : Spherical Tokamak Analysis and Reactor Code',
Unpublished internal Oak Ridge document.
[^2]: H. Zohm et al, 'On the Physics Guidelines for a Tokamak DEMO',
FTP/3-3, Proc. IAEA Fusion Energy Conference, October 2012, San Diego
[^3]: T. Hartmann and H. Zohm, 'Towards a `Physics Design Guidelines for a DEMO 
Tokamak', EFDA Report 2L8QVN, March 2012
[^4]: Y. Sakamoto, 'Recent progress in vertical stability analysis in JA',
Task meeting EU-JA #16, Fusion for Energy, Garching, 24--25 June 2014
[^5]: H.S. Bosch and G.M. Hale, 'Improved Formulas for Fusion Cross-sections 
and Thermal Reactivities', Nuclear Fusion **32** (1992) 611
[^6]: J. Johner, 'Helios: A Zero-Dimensional Tool for Next Step and Reactor 
Studies', Fusion Science and Technology **59** (2011) 308--349
[^7]: M. Bernert et al. Plasma Phys. Control. Fus. **57** (2015) 014038
[^8]: N.A. Uckan and ITER Physics Group, 'ITER Physics Design Guidelines: 1989',
ITER Documentation Series, No. 10, IAEA/ITER/DS/10 (1990)
[^9]: T. C. Hender et al., 'Physics Assessment for the European Reactor Study',
AEA Fusion Report AEA FUS 172 (1992)