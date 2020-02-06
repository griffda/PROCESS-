# TF Coil Model

[PDF Doc](./media/tfdoc.pdf)

The toroidal field (TF) coils can be either resistive or superconducting. Switch 
`itfsup` should be set to:

- `== 1` -- for superconducting coils
- `== 0` -- for purely copper coils. 
  
In the superconductor model, the CICC (Conductor In Cable Conduit) structure 
shown in Figure 1 is assumed, and the coils are cooled using a liquid helium 
cryogenic system.

The outboard leg of the TF coil is assumed to be the same width in the
toroidal direction as the outside edge of the inboard leg. In the radial
direction, for resistive TF coils the input parameter `tfootfi` gives the ratio 
of the outboard leg thickness to the inboard leg thickness `tfcth`; for 
superconducting coils the outboard thickness is set equal to the inboard thickness.

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

Each TF coil is defined in the $(R,Z)$ plane by a straight section and four 
elliptical arcs. Because of the finite number of TF coils used in a tokamak 
(18 for ITER), the toroidal field has a ripple introduced into it, the 
amplitude of which can be limited to a few percent (given by input parameter 
`ripmax`, default value 1\%) by the code by adjusting the outboard gap 
thickness (`gapsto`).

Among the TF coil parameters calculated by the code are the maximum allowable
current density, the stresses on the structure, the energy stored and the
magnetic field produced by the coils.

The following options are available within the superconducting TF coil model
(`itfsup = 1`).

## Superconducting materials

Switch `i_tf_sup_mat` specifies which superconducting material is to be used:

- `== 1` -- Nb$_3$Sn superconductor, ITER critical surface parameterization[^1], 
  standard critical values
- `== 2` -- Bi-2212 high temperature superconductor
- `== 3` -- NbTi superconductor
- `== 4` -- Nb$_3$Sn superconductor, ITER critical surface parameterization[^1], 
  user-defined critical parameters

The fraction of copper present in the superconducting filaments is given by
the value of variable `fcutfsu` (iteration variable number 59).

For `i_tf_sup_mat = 2`, a technology adjustment factor `fhts` may be used to modify 
the critical current density fit for the Bi-2212 superconductor, to describe the 
level of technology assumed (i.e. to account for stress, fatigue, radiation, 
AC losses, joints or manufacturing variations). The default value for `fhts` is 
0.5 (a value of 1.0 would be very optimistic).

For `i_tf_sup_mat = 4`, important superconductor properties may be input by the user 
as follows: the upper critical field at zero temperature and strain is set 
using input parameter `bcritsc`, and the critical temperature at zero field and 
strain is set using input parameter `tcritsc`.

## Current density limits

The current in the TF coils must be sufficient to produce the required
toroidal field at the centre of the plasma. In tokamaks, the field falls off
at a rate $1/R$, with the peak value occurring at the outer edge of the
inboard portion of the TF coil winding pack ($R_{\mbox{max TF}} =
\mathtt{rbmax}$). The maximum TF coil current depends on the field it produces
and the allowable current density.

Three constraints are relevant to the operating current density $J_{\mbox{op}}$ 
in the (superconducting) TF coils.

\item To ensure that $J_{\mbox{op}}$ does not exceed the critical value $J_{\mbox{crit}}$, constraint
  equation no.\ 33 should be turned on with iteration variable no.\ 50
  (\texttt{fiooic}).

\item To ensure that $J_{\mbox{op}}$ does not exceed the current density protection limit,
  constraint equation no.\ 35 should be turned on with iteration variable no.\
  53 (\texttt{fjprot}).

\item The critical current density $J_{\mbox{crit}}$ falls with the temperature of the
  superconductor. The temperature margin $\Delta T$ is the difference between the
  temperature at which \jcrit would be equal to $J_{\mbox{op}}$ and the operating
  temperature. The minimum allowed $\Delta T$ can be set using input parameter
  \texttt{tmargmin} together with constraint equation no.\ 36 and iteration
  variable no.\ 54 (\texttt{ftmargtf}).

  Note that if the temperature margin is positive, $J_{\mbox{op}}$ is guaranteed to be
  lower than \jcrit, and so constraints~33 and~36 need not both be turned on
  (in fact, it is recommended that only one of these two constraints is
  activated in any given run).

\end{itemize}

\subsubsection{Stress model}

Switch \texttt{tfc\_model} controls whether a simple stress model
(\texttt{tfc\_model = 0}, suitable for solid copper TF coils) or a more
complex stress model (\texttt{tfc\_model = 1}) should be used. If
\texttt{tfc\_model = 1}, a two-layer stress model~\cite{Morris_tfc} developed
by CCFE is used.

To enforce the stress limits calculated using either of these models,
constraint equation no.\ 31 (case stress) and/or constraint equation no.\ 32
(conduit stress) should be turned on with iteration variables no.\ 48
(\texttt{fstrcase}) and/or no.\ 49 (\texttt{fstrcond}), respectively. The
stress limit is set using input parameter \texttt{alstrtf}.

[^1]: $J_c(B,T,\epsilon)$ Parameterizations for the ITER Nb$_3$Sn Production',
ITER Document 2MMF7J (2008), \texttt{https://user.iter.org/?uid=2MMF7J\&action=get\_document}