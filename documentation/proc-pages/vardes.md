PROCESS Variable Descriptor File : dated 20190808
-------------------------------------------------

------------------------------------------------------------------------

Variables labelled with FIX are initialised with the given default value
(shown between / / characters), but currently are not available to be
changed in the input file.

All other variables shown with a default value (including arrays boundl,
boundu and sweep) can be changed in the input file.

Variables not shown with a default value are calculated within PROCESS,
so need not be initialised.

------------------------------------------------------------------------

### [global\_variables](global_variables.html)

-   icase : power plant type
-   runtitle /Run Title/ : short descriptive title for the run
-   verbose /0/ : switch for turning on/off diagnostic messages:
    -   = 0 turn off diagnostics
    -   = 1 turn on diagnostics
-   run\_tests /0/ : Turns on built-in tests if set to 1
-   maxcal /200/ : maximum number of VMCON iterations

### [constants](constants.html)

-   degrad FIX : degrees to radians, = pi/180
-   echarge FIX : electron charge (C)
-   mproton FIX : proton mass (kg)
-   pi FIX : famous number
-   rmu0 FIX : permeability of free space, 4.pi x 10\^(-7) H/m
-   twopi FIX : 2 pi
-   umass FIX : unified atomic mass unit (kg)
-   epsilon0 FIX : permittivity of free space (Farad/m)

### [physics\_variables](physics_variables.html)

-   ipnlaws /48/ FIX : number of energy confinement time scaling laws
-   abeam : beam ion mass (amu)
-   afuel : average mass of fuel portion of ions (amu)
-   aion : average mass of all ions (amu)
-   alphaj /1.0/ : current profile index; calculated from q0, q if
    iprofile=1
-   alphan /0.25/ : density profile index
-   alphap : pressure profile index
-   alpharate : alpha particle production rate (particles/m3/sec)
-   alphat /0.5/ : temperature profile index
-   aspect /2.907/ : aspect ratio (iteration variable 1)
-   beamfus0 /1.0/ : multiplier for beam-background fusion calculation
-   beta /0.042/ : total plasma beta (iteration variable 5) (calculated
    if ipedestal =3)
-   betaft : fast alpha beta component
-   betalim : allowable beta
-   betanb : neutral beam beta component
-   betap : poloidal beta
-   normalised\_total\_beta : normaised total beta
-   betbm0 /1.5/ : leading coefficient for NB beta fraction
-   bp : poloidal field (T)
-   bt /5.68/ : toroidal field on axis (T) (iteration variable 2)
-   btot : total toroidal + poloidal field (T)
-   burnup : fractional plasma burnup
-   bvert : vertical field at plasma (T)
-   csawth /1.0/ : coeff. for sawteeth effects on burn V-s requirement
-   cvol /1.0/ : multiplying factor times plasma volume (normally=1)
-   cwrmax /1.35/ : maximum ratio of conducting wall distance to plasma
    minor radius for vertical stability (constraint equation 23)
-   dene /9.8e19/ : electron density (/m3) (iteration variable 6)
    (calculated if ipedestal=3)
-   deni : fuel ion density (/m3)
-   dlamee : electron-electron coulomb logarithm
-   dlamie : ion-electron coulomb logarithm
-   dlimit(7) : density limit (/m3) as calculated using various models
-   dnalp : thermal alpha density (/m3)
-   dnbeam : hot beam ion density, variable (/m3)
-   dnbeam2 : hot beam ion density from calculation (/m3)
-   dnbeta /3.5/ : (Troyon-like) coefficient for beta scaling;
    calculated as (4.0\*rli) if iprofile=1 (see also gtscale option)
-   dnelimt : density limit (/m3)
-   dnitot : total ion density (/m3)
-   dnla : line averaged electron density (/m3)
-   dnprot : proton ash density (/m3)
-   dntau : plasma average \"n-tau\" (seconds/m3)
-   dnz : high Z ion density (/m3)
-   ealphadt /3520.0/ FIX : alpha birth energy in D-T reaction (keV)
-   epbetmax /1.38/ : maximum (eps\*beta\_poloidal) (constraint equation
    6) revised (07/01/16) Issue \#346 \"Operation at the tokamak
    equilibrium poloidal beta-limit in TFTR\" 1992 Nucl. Fusion 32 1468
-   eps : inverse aspect ratio
-   faccd : fraction of plasma current produced by auxiliary current
    drive
-   facoh : fraction of plasma current produced inductively
-   falpe : fraction of alpha energy to electrons
-   falpha /0.95/ : fraction of alpha power deposited in plasma (Physics
    of Energetic Ions, p.2489)
-   falpi : fraction of alpha power to ions
-   fdeut /0.5/ : deuterium fuel fraction
-   ftar /0.5/ : fraction of power to the lower divertor in double null
-   configuration (snull = 0 only)
-   ffwal /0.92/ : factor to convert plasma surface area to first wall
    area in neutron wall load calculation (iwalld=1)
-   fgwped /0.85/ : fraction of Greenwald density to set as pedestal-top
    density If \<0, pedestal-top density set manually using neped
    (ipedestal\>=1) Needs to be \>0 if ipedestal = 3 (iteration
    variable 145)
-   fgwsep /0.50/ : fraction of Greenwald density to set as separatrix
    density If \<0, separatrix density set manually using nesep
    (ipedestal\>=1) Needs to be \>0 if ipedestal = 3
-   fhe3 /0.0/ : helium-3 fuel fraction
-   figmer : physics figure of merit (= plascur\*aspect\*\*sbar, where
    sbar=1)
-   fkzohm /1.0/ : Zohm elongation scaling adjustment factor
    (ishape=2, 3)
-   fplhsep /1.0/ : F-value for Psep \>= Plh + Paux (constraint
    equation 73)
-   fpdivlim /1.0/ : F-value for minimum pdivt (constraint equation 80)
-   fne0 /1.0/ : F-value for minimum pdivt (constraint equation 81)
-   ftrit /0.5/ : tritium fuel fraction
-   fusionrate : fusion reaction rate (reactions/m3/sec)
-   fvsbrnni /1.0/ : fraction of the plasma current produced by
    non-inductive means (iteration variable 44)
-   gamma /0.4/ : Ejima coefficient for resistive startup V-s formula
-   gammaft : ratio of (fast alpha + neutral beam beta) to thermal beta
-   gtscale /0/ : switch for a/R scaling of dnbeta (iprofile=0 only):
    -   = 0 do not scale dnbeta with eps;
    -   = 1 scale dnbeta with eps
-   hfac(ipnlaws) : H factors for an ignited plasma for each energy
    confinement time scaling law
-   hfact /1.0/ : H factor on energy confinement times, radiation
    corrected (iteration variable 10). If ipedestal=2 or 3 and hfact =
    0, not used in PLASMOD (see also plasmod\_i\_modeltype)
-   taumax /10/ : Maximum allowed energy confinement time (s)
-   ibss /3/ : switch for bootstrap current scaling:
    -   = 1 ITER 1989 bootstrap scaling (high R/a only);
    -   = 2 for Nevins et al general scaling;
    -   = 3 for Wilson et al numerical scaling;
    -   = 4 for Sauter et al scaling
-   iculbl /0/ : switch for beta limit scaling (constraint equation 24):
    -   = 0 apply limit to total beta;
    -   = 1 apply limit to thermal beta;
    -   = 2 apply limit to thermal + neutral beam beta
-   icurr /4/ : switch for plasma current scaling to use:
    -   = 1 Peng analytic fit;
    -   = 2 Peng double null divertor scaling (ST);
    -   = 3 simple ITER scaling (k = 2.2, d = 0.6);
    -   = 4 later ITER scaling, a la Uckan;
    -   = 5 Todd empirical scaling I;
    -   = 6 Todd empirical scaling II;
    -   = 7 Connor-Hastie model;
    -   = 8 Sauter scaling allowing negative triangularity;
    -   = 9 FIESTA ST fit
-   idensl /7/ : switch for density limit to enforce (constraint
    equation 5):
    -   = 1 old ASDEX;
    -   = 2 Borrass model for ITER (I);
    -   = 3 Borrass model for ITER (II);
    -   = 4 JET edge radiation;
    -   = 5 JET simplified;
    -   = 6 Hugill-Murakami Mq limit;
    -   = 7 Greenwald limit
-   idivrt : number of divertors (calculated from snull)
-   ifalphap /1/ : switch for fast alpha pressure calculation:
    -   = 0 ITER physics rules (Uckan) fit;
    -   = 1 Modified fit (D. Ward) - better at high temperature
-   ifispact /0/ : switch for neutronics calculations:
    -   = 0 neutronics calculations turned off;
    -   = 1 neutronics calculations turned on
-   igeom /1/ : switch for plasma geometry calculation:
    -   = 0 original method (possibly based on Peng ST modelling);
    -   = 1 improved (and traceable) method
-   ignite /0/ : switch for ignition assumption. Obviously, ignite must
    be zero if current drive is required. If ignite is 1, any auxiliary
    power is assumed to be used only during plasma start-up, and is
    excluded from all steady-state power balance calculations.
    -   = 0 do not assume plasma ignition;
    -   = 1 assume ignited (but include auxiliary power in costs)
-   iinvqd /1/ : switch for inverse quadrature in L-mode scaling laws 5
    and 9:
    -   = 0 inverse quadrature not used;
    -   = 1 inverse quadrature with Neo-Alcator tau-E used
-   ipedestal /1/ : switch for pedestal profiles:
    -   = 0 use original parabolic profiles;
    -   = 1 use pedestal profiles
    -   = 2 use pedestal profiles and run PLASMOD on final output
    -   = 3 use PLASMOD transport model only to calculate pedestal
        profiles
-   iscdens /0/ : switch for pedestal profiles: OBSOLETE
-   ieped /0/ : switch for scaling pedestal-top temperature with plasma
    parameters:
    -   = 0 set pedestal-top temperature manually using teped;
    -   = 1 set pedestal-top temperature using EPED scaling; (PLASMOD
        implementation of scaling within PLASMOD, ipedestal =2,3)
    -   https://idm.euro-fusion.org/?uid=2MSZ4T
-   eped\_sf /1.0/ : Adjustment factor for EPED scaling to reduce
    pedestal temperature or pressure to mitigate or prevent ELMs
-   neped /4.0e19/ : electron density of pedestal \[m-3\]
    (ipedestal=1,2, calculated if 3)
-   nesep /3.0e19/ : electron density at separatrix \[m-3\]
    (ipedestal=1,2, calculated if 3)
-   alpha\_crit : critical ballooning parameter value
-   nesep\_crit : critical electron density at separatrix \[m-3\]
-   plasma\_res\_factor /1.0/ : plasma resistivity pre-factor
-   rhopedn /1.0/ : r/a of density pedestal (ipedestal\>=1)
-   rhopedt /1.0/ : r/a of temperature pedestal (ipedestal\>=1)
-   tbeta /2.0/ : temperature profile index beta (ipedestal=1,2)
-   teped /1.0/ : electron temperature of pedestal (keV) (ipedestal\>=1,
    ieped=0, calculated for ieped=1)
-   tesep /0.1/ : electron temperature at separatrix (keV)
    (ipedestal\>=1) calculated if reinke criterion is used (icc = 78)
-   iprofile /1/ : switch for current profile consistency:
    -   = 0 use input values for alphaj, rli, dnbeta (but see gtscale
        option);
    -   = 1 make these consistent with input q, q0 values
        (recommendation: use icurr=4 with this option)
-   iradloss /1/ : switch for radiation loss term usage in power balance
    (see User Guide):
    -   = 0 total power lost is scaling power plus radiation (needed for
        ipedestal=2,3)
    -   = 1 total power lost is scaling power plus core radiation only
    -   = 2 total power lost is scaling power only, with no additional
        allowance for radiation. This is not recommended for power plant
        models.
-   isc /34 (=IPB98(y,2))/ : switch for energy confinement time scaling
    law (see description in tauscl)
-   tauscl(ipnlaws) : labels describing energy confinement scaling laws:
    -   ( 1) Neo-Alcator (ohmic)
    -   ( 2) Mirnov (H-mode)
    -   ( 3) Merezkhin-Muhkovatov (L-mode)
    -   ( 4) Shimomura (H-mode)
    -   ( 5) Kaye-Goldston (L-mode)
    -   ( 6) ITER 89-P (L-mode)
    -   ( 7) ITER 89-O (L-mode)
    -   ( 8) Rebut-Lallia (L-mode)
    -   ( 9) Goldston (L-mode)
    -   \(10) T10 (L-mode)
    -   \(11) JAERI-88 (L-mode)
    -   \(12) Kaye-Big Complex (L-mode)
    -   \(13) ITER H90-P (H-mode)
    -   \(14) ITER Mix (L-mode)
    -   \(15) Riedel (L-mode)
    -   \(16) Christiansen (L-mode)
    -   \(17) Lackner-Gottardi (L-mode)
    -   \(18) Neo-Kaye (L-mode)
    -   \(19) Riedel (H-mode)
    -   \(20) ITER H90-P amended (H-mode)
    -   \(21) LHD (stellarator)
    -   \(22) Gyro-reduced Bohm (stellarator)
    -   \(23) Lackner-Gottardi (stellarator)
    -   \(24) ITER-93H (H-mode)
    -   \(25) OBSOLETE
    -   \(26) ITER H-97P ELM-free (H-mode)
    -   \(27) ITER H-97P ELMy (H-mode)
    -   \(28) ITER-96P (=ITER-97L) (L-mode)
    -   \(29) Valovic modified ELMy (H-mode)
    -   \(30) Kaye PPPL April 98 (L-mode)
    -   \(31) ITERH-PB98P(y) (H-mode)
    -   \(32) IPB98(y) (H-mode)
    -   \(33) IPB98(y,1) (H-mode)
    -   \(34) IPB98(y,2) (H-mode)
    -   \(35) IPB98(y,3) (H-mode)
    -   \(36) IPB98(y,4) (H-mode)
    -   \(37) ISS95 (stellarator)
    -   \(38) ISS04 (stellarator)
    -   \(39) DS03 (H-mode)
    -   \(40) Murari et al non-power law (H-mode)
    -   \(41) Petty 2008 (H-mode)
    -   \(42) Lang et al. 2012 (H-mode)
    -   \(43) Hubbard 2017 (I-mode) - nominal
    -   \(44) Hubbard 2017 (I-mode) - lower bound
    -   \(45) Hubbard 2017 (I-mode) - upper bound
    -   \(46) NSTX (H-mode; Spherical tokamak)
    -   \(47) NSTX-Petty08 Hybrid (H-mode)
    -   \(48) Use input tauee\_in
-   iscrp /1/ : switch for plasma-first wall clearances:
    -   = 0 use 10% of rminor;
    -   = 1 use input (scrapli and scraplo)
-   ishape /0/ : switch for plasma cross-sectional shape calculation:
    -   = 0 use input kappa, triang to calculate 95% values;
    -   = 1 scale qlim, kappa, triang with aspect ratio (ST);
    -   = 2 set kappa to the natural elongation value (Zohm ITER
        scaling), triang input;
    -   = 3 set kappa to the natural elongation value (Zohm ITER
        scaling), triang95 input;
    -   = 4 use input kappa95, triang95 to calculate separatrix values
-   itart /0/ : switch for spherical tokamak (ST) models:
    -   = 0 use conventional aspect ratio models;
    -   = 1 use spherical tokamak models
-   itartpf /0/ : switch for Spherical Tokamak PF models:
    -   = 0 use Peng and Strickler (1986) model;
    -   = 1 use conventional aspect ratio model
-   iwalld /1/ : switch for neutron wall load calculation:
    -   = 1 use scaled plasma surface area;
    -   = 2 use first wall area directly
-   kappa /1.792/ : plasma separatrix elongation (calculated if
    ishape \> 0)
-   kappa95 /1.6/ : plasma elongation at 95% surface (calculated if
    ishape \< 4)
-   kappaa : plasma elongation calculated as xarea/(pi.a2)
-   kappaa\_IPB : Volume measure of plasma elongation
-   ne0 : central electron density (/m3)
-   ni0 : central ion density (/m3)
-   p0 : central total plasma pressure (Pa)
-   palppv : alpha power per volume (MW/m3)
-   palpepv : alpha power per volume to electrons (MW/m3)
-   palpfwmw : alpha power escaping plasma and reaching first wall (MW)
-   palpipv : alpha power per volume to ions (MW/m3)
-   palpmw : alpha power (MW)
-   palpnb : alpha power from hot neutral beam ions (MW)
-   pbrempv : bremsstrahlung power per volume (MW/m3)
-   pchargemw : non-alpha charged particle fusion power (MW)
-   pchargepv : non-alpha charged particle fusion power per volume
    (MW/m3)
-   pcoef : profile factor (= n-weighted T / average T)
-   pcoreradmw : total core radiation power (MW)
-   pcoreradpv : total core radiation power per volume (MW/m3)
-   pdd : deuterium-deuterium fusion power (MW)
-   pdhe3 : deuterium-helium3 fusion power (MW)
-   pdivt : power to conducted to the divertor region (MW)
-   pdivl : power conducted to the lower divertor region (calculated if
    snull = 0) (MW)
-   pdivu : power conducted to the upper divertor region (calculated if
    snull = 0) (MW)
-   pdivmax : power conducted to the divertor with most load (calculated
    if snull = 0) (MW)
-   pdt : deuterium-tritium fusion power (MW)
-   pedgeradmw : edge radiation power (MW)
-   pedgeradpv : edge radiation power per volume (MW/m3)
-   pfuscmw : charged particle fusion power (MW)
-   phiint : internal plasma V-s
-   photon\_wall : Nominal mean radiation load on inside surface of
    reactor (MW/m2)
-   piepv : ion/electron equilibration power per volume (MW/m3)
-   plascur : plasma current (A)
-   plinepv : line radiation power per volume (MW/m3)
-   pneutmw : neutron fusion power (MW)
-   pneutpv : neutron fusion power per volume (MW/m3)
-   pohmmw : ohmic heating power (MW)
-   pohmpv : ohmic heating power per volume (MW/m3)
-   powerht : heating power (= transport loss power) (MW) used in
    confinement time calculation
-   powfmw : fusion power (MW)
-   pperim : plasma poloidal perimeter (m)
-   pradmw : total radiation power (MW)
-   pradpv : total radiation power per volume (MW/m3)
-   protonrate : proton production rate (particles/m3/sec)
-   psolradmw : SOL radiation power (MW) (stellarator only)
-   psyncpv : synchrotron radiation power per volume (MW/m3)
-   ilhthresh /6/ : switch for L-H mode power threshold scaling to use
    (see pthrmw for list)
-   plhthresh : L-H mode power threshold (MW) (chosen via ilhthresh, and
    enforced if constraint equation 15 is on)
-   pthrmw(18) : L-H power threshold for various scalings (MW):
    1.  ITER 1996 scaling: nominal
    2.  ITER 1996 scaling: upper bound
    3.  ITER 1996 scaling: lower bound
    4.  ITER 1997 scaling: excluding elongation
    5.  ITER 1997 scaling: including elongation
    6.  Martin 2008 scaling: nominal
    7.  Martin 2008 scaling: 95% upper bound
    8.  Martin 2008 scaling: 95% lower bound
    9.  Snipes 2000 scaling: nominal
    10. Snipes 2000 scaling: upper bound
    11. Snipes 2000 scaling: lower bound
    12. Snipes 2000 scaling (closed divertor): nominal
    13. Snipes 2000 scaling (closed divertor): upper bound
    14. Snipes 2000 scaling (closed divertor): lower bound
    15. Hubbard et al. 2012 L-I threshold scaling: nominal
    16. Hubbard et al. 2012 L-I threshold scaling: lower bound
    17. Hubbard et al. 2012 L-I threshold scaling: upper bound
    18. Hubbard et al. 2017 L-I threshold scaling
-   ptremw : electron transport power (MW)
-   ptrepv : electron transport power per volume (MW/m3)
-   ptrimw : ion transport power (MW)
-   pscalingmw : Total transport power from scaling law (MW)
-   ptripv : ion transport power per volume (MW/m3)
-   q /3.0/ : safety factor \'near\' plasma edge (iteration variable
    18): equal to q95 (unless icurr = 2 (ST current scaling), in which
    case q = mean edge safety factor qbar)
-   q0 /1.0/ : safety factor on axis
-   q95 : safety factor at 95% surface
-   qfuel : plasma fuelling rate (nucleus-pairs/s)
-   tauratio /1.0/ : ratio of He and pellet particle confinement times
-   qlim : lower limit for edge safety factor
-   qstar : cylindrical safety factor
-   rad\_fraction\_sol /0.8/ : SoL radiation fraction
-   rad\_fraction : Radiation fraction = total radiation / total power
    deposited in plasma
-   ralpne /0.1/ : thermal alpha density / electron density (iteration
    variable 109) (calculated if ipedestal=3)
-   protium /0.0/ : Seeded protium density / electron density.
-   rli /0.9/ : plasma normalised internal inductance; calculated from
    alphaj if iprofile=1
-   rlp : plasma inductance (H)
-   rmajor /8.14/ : plasma major radius (m) (iteration variable 3)
-   rminor : plasma minor radius (m)
-   rnbeam /0.005/ : hot beam density / n\_e (iteration variable 7)
-   rncne : n\_carbon / n\_e
-   rndfuel : fuel burnup rate (reactions/second)
-   rnfene : n\_highZ / n\_e
-   rnone : n\_oxygen / n\_e
-   rpfac : neo-classical correction factor to rplas
-   rplas : plasma resistance (ohm)
-   res\_time : plasma current resistive diffusion time (s)
-   sarea : plasma surface area
-   sareao : outboard plasma surface area
-   sf : shape factor = plasma poloidal perimeter / (2.pi.rminor)
-   snull /1/ : switch for single null / double null plasma:
    -   = 0 for double null;
    -   = 1 for single null (diverted side down)
-   ssync /0.6/ : synchrotron wall reflectivity factor
-   tauee : electron energy confinement time (sec)
-   tauee\_in /0.0/ : Input electron energy confinement time (sec)
    (isc=48 only)
-   taueff : global thermal energy confinement time (sec)
-   tauei : ion energy confinement time (sec)
-   taup : alpha particle confinement time (sec)
-   te /12.9/ : volume averaged electron temperature (keV) (iteration
    variable 4) (calculated if ipedestal = 3)
-   te0 : central electron temperature (keV)
-   ten : density weighted average electron temperature (keV)
-   ti /12.9/ : volume averaged ion temperature (keV); N.B. calculated
    from te if tratio \> 0.0
-   ti0 : central ion temperature (keV)
-   tin : density weighted average ion temperature (keV)
-   tratio /1.0/ : ion temperature / electron temperature; used to
    calculate ti if tratio \> 0.0
-   triang /0.36/ : plasma separatrix triangularity (calculated if
    ishape=1, 3 or 4)
-   triang95 /0.24/ : plasma triangularity at 95% surface (calculated if
    ishape \< 3)
-   vol : plasma volume (m3)
-   vsbrn : V-s needed during flat-top (heat + burn times) (Wb)
-   vshift : plasma/device midplane vertical shift - single null
-   vsind : internal and external plasma inductance V-s (Wb)
-   vsres : resistive losses in startup V-s (Wb)
-   vsstt : total V-s needed (Wb)
-   wallmw : average neutron wall load (MW/m2)
-   wtgpd : mass of fuel used per day (g)
-   xarea : plasma cross-sectional area (m2)
-   zeff : plasma effective charge
-   zeffai : mass weighted plasma effective charge

### [plasmod\_variables](plasmod_variables.html)

-   plasmod\_tol /1.0d-10/ : tolerance to be reached at each time step
    (%)
-   plasmod\_dtmin /0.05d0/ : min time step
-   plasmod\_dtmax /0.1d0/ : max time step
-   plasmod\_dt /0.01d0/ : time step
-   plasmod\_dtinc /2.0d0/ : decrease of dt
-   plasmod\_ainc /1.1d0/ : increase of dt
-   plasmod\_test /100000.0d0/ : max number of iterations
-   plasmod\_tolmin /10.1d0/ : multiplier of etolm which can not be
    exceeded
-   plasmod\_eopt /0.15d0/ : exponent of jipperdo
-   plasmod\_dtmaxmin /0.15d0/ : exponent of jipperdo2
-   plasmod\_dtmaxmax /0.0d0/ : stabilizing coefficient
-   plasmod\_capa /0.1d0/ : first radial grid point
-   plasmod\_maxa /0.0d0/ : diagz 0 or 1
-   plasmod\_dgy /1.0d-5/ : Newton differential
-   plasmod\_iprocess /1/ : 0 - use PLASMOD functions, 1 - use PROCESS
    functions
-   plasmod\_i\_modeltype /1/ : switch for the transport model
    -   1 - Simple gyrobohm scaling with imposed H factor \> 1. Other
        values give H factor as output
    -   111 - roughly calibrated to give H=1 for DEMO, but not fixed H
-   plasmod\_i\_equiltype /1/ : 1 - EMEQ, solve with sawteeth and
    inputted q95. 2 - EMEQ, solve with sawteeth and inputted Ip (not
    recommended!).
-   plasmod\_isawt /1/ : 0 - no sawteeth, 1 - solve with sawteeth.
-   plasmod\_nx /41/ : number of interpolated grid points
-   plasmod\_nxt /7/ : number of solved grid points
-   plasmod\_nchannels /3/ : leave this at 3
-   plasmod\_i\_impmodel /1/ : impurity model: 0 - fixed concentration,
    1 - fixed concentration at pedestal top, then fixed density.
-   plasmod\_globtau(5) /5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0/ :
    tauparticle/tauE for D, T, He, Xe, Ar (NOT used for Xe!)
-   plasmod\_psepplh\_sup /12000.0d0/ : Psep/PLH if above this, use Xe
-   plasmod\_qdivt /0.0d0/ : divertor heat flux in MW/m\^2, if 0, dont
    use SOL model
-   plasmod\_imptype(3) /14, 13, 9/ : Impurities: element 1 - intrinsic
    impurity, element 2 - Psep control, elem
-   plasmod\_qnbi\_psepfac /50.0d0/ : dqnbi/d(1-Psep/PLH)
-   plasmod\_cxe\_psepfac /1.0d-4/ : dcxe/d(1-Psep/PLH)
-   plasmod\_car\_qdivt /1.0d-4/ : dcar/d(qdivt)
-   plasmod\_maxpauxor /20.0d0/ : max allowed auxiliary power / R
-   plasmod\_x\_heat(2) /0.0d0/ : element 1 - nbi, element 2 - ech
-   plasmod\_x\_cd(2) /0.0d0/ : element 1 - nbi, element 2 - ech
-   plasmod\_x\_fus(2) /0.0d0/ : element 1 - nbi, element 2 - ech
-   plasmod\_x\_control(2) /0.0d0/ : element 1 - nbi, element 2 - ech
-   plasmod\_dx\_heat(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 -
    ech
-   plasmod\_dx\_cd(2) /0.2d0, 0.03/ : element 1 - nbi, element 2 - ech
-   plasmod\_dx\_fus(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 -
    ech
-   plasmod\_dx\_control(2) /0.2d0, 0.03d0/ : element 1 - nbi, element
    2 - ech
-   plasmod\_contrpovs /0.0d0/ :: control power in Paux/lateral\_area
    (MW/m2)
-   plasmod\_contrpovr /0.0d0/ :: control power in Paux/R (MW/m)
-   plasmod\_nbi\_energy /1000.0d0/ :: In keV
-   plasmod\_v\_loop /-1.0d-6/ :: target loop voltage. If lower than
    -1.e5 do not use
-   plasmod\_pfus /0.0d0/ :: if 0. not used (otherwise controlled with
    Pauxheat)
-   plasmod\_eccdeff /0.3d0/ :: current drive multiplier: CD =
    eccdeff\*PCD\*TE/NE (not in use yet)
-   plasmod\_fcdp /-1.0d0/ :: (P\_CD - Pheat)/(Pmax-Pheat),i.e. ratio of
    CD power over available power (iteration
-   plasmod\_fradc /-1.0d0/ :: Pline\_Xe / (Palpha + Paux - PlineAr -
    Psync - Pbrad) (iteration variable 148)
-   plasmod\_pech /0.0d0/ :: ech power (not in use yet)
-   plasmod\_gamcdothers /1.0d0/ :: efficiency multiplier for non-CD
    heating. If 0.0 pheat treated as if it had
-   plasmod\_chisawpos /-1.0d0/ :: position where artificial sawtooth
    diffusivity is added, -1 - uses q=1 positi
-   plasmod\_chisaw /0.0d0/ :: artificial diffusivity in m\^2/s
-   plasmod\_sawpertau /1.0d-6/ :: ratio between sawtooth period and
    confinement time
-   plasmod\_spellet /0.0d0/ :: pellet mass in units of D in 10\^19
-   plasmod\_fpellet /0.5d0/ :: pellet frequency in Hz
-   plasmod\_pedscal /1.0d0/ :: multiplication factor of the pedestal
    scaling in PLASMOD can be used to scan the pedestal height.
-   geom :: Derived type containing all geometry information for PLASMOD
-   comp :: Derived type containing all composition information for
    PLASMOD
-   ped :: Derived type containing all pedestal information for PLASMOD
-   inp0 :: Derived type containing miscellaneous input information for
    PLASMOD
-   radp :: Derived type containing all radial profile information for
    PLASMOD
-   mhd :: Derived type containing all mhd information for PLASMOD
-   loss :: Derived type containing all power loss information for
    PLASMOD
-   num :: Derived type containing all numerics information for PLASMOD
-   i\_flag :: Error flag for PLASMOD

### [current\_drive\_variables](current_drive_variables.html)

-   beamwd /0.58/ : width of neutral beam duct where it passes between
    the TF coils (m) (T Inoue et al, Design of neutral beam system for
    ITER-FEAT, [Fusion Engineering and Design, Volumes 56-57, October
    2001, Pages
    517-521](http://dx.doi.org/10.1016/S0920-3796(01)00339-8))
-   bigq : Fusion gain; P\_fusion / (P\_injection + P\_ohmic)
-   bootipf : bootstrap current fraction (enforced; see ibss)
-   bscfmax /0.9/ : maximum fraction of plasma current from bootstrap;
    if bscfmax \< 0, bootstrap fraction = abs(bscfmax)
-   bscf\_iter89 : bootstrap current fraction, ITER 1989 model
-   bscf\_nevins : bootstrap current fraction, Nevins et al model
-   bscf\_sauter : bootstrap current fraction, Sauter et al model
-   bscf\_wilson : bootstrap current fraction, Wilson et al model
-   cboot /1.0/ : bootstrap current fraction multiplier (ibss=1)
-   cnbeam : neutral beam current (A)
-   echpwr : ECH power (MW)
-   echwpow : ECH wall plug power (MW)
-   effcd : current drive efficiency (A/W)
-   enbeam /1.0e3/ : neutral beam energy (keV) (iteration variable 19)
-   etacd : auxiliary power wall plug to injector efficiency
-   etaech /0.3/ : ECH wall plug to injector efficiency
-   etalh /0.3/ : lower hybrid wall plug to injector efficiency
-   etanbi /0.3/ : neutral beam wall plug to injector efficiency
-   fpion : fraction of beam energy to ions
-   pnbitot : neutral beam power entering vacuum vessel
-   nbshinemw : neutral beam shine-through power
-   feffcd /1.0/ : current drive efficiency fudge factor (iteration
    variable 47)
-   forbitloss /0.0/ : fraction of neutral beam power lost after
    ionisation but before thermalisation (orbit loss fraction)
-   frbeam /1.05/ : R\_tangential / R\_major for neutral beam injection
-   ftritbm /1.0e-6/ : fraction of beam that is tritium
-   gamcd : normalised current drive efficiency (1.0e20 A/(W m\^2))
-   gamma\_ecrh /0.35/ : user input ECRH gamma (1.0e20 A/(W m\^2))
-   rho\_ecrh /0.1/ : normalised minor radius at which electron
    cyclotron current drive is maximum
-   iefrf /5/ : switch for current drive efficiency model:
    1.  Fenstermacher Lower Hybrid
    2.  Ion Cyclotron current drive
    3.  Fenstermacher ECH
    4.  Ehst Lower Hybrid
    5.  ITER Neutral Beam
    6.  new Culham Lower Hybrid model
    7.  new Culham ECCD model
    8.  new Culham Neutral Beam model
    9.  Empty (Oscillating field CD removed)
    10. ECRH user input gamma
    11. ECRH \"HARE\" model (E. Poli, Physics of Plasmas 2019)
-   irfcd /1/ : switch for current drive calculation:
    -   = 0 turned off;
    -   = 1 turned on
-   nbshinef : neutral beam shine-through fraction
-   nbshield /0.5/ : neutral beam duct shielding thickness (m)
-   pheat /0.0/ : heating power not used for current drive (MW)
    (iteration variable 11)
-   pinjalw /150.0/ : Maximum allowable value for injected power (MW)
    (constraint equation 30)
-   pinjemw : auxiliary injected power to electrons (MW)
-   pinjimw : auxiliary injected power to ions (MW)
-   pinjmw : total auxiliary injected power (MW)
-   plhybd : lower hybrid injection power (MW)
-   pnbeam : neutral beam injection power (MW)
-   porbitlossmw : neutral beam power lost after ionisation but before
    thermalisation (orbit loss power) (MW)
-   pwplh : lower hybrid wall plug power (MW)
-   pwpnb : neutral beam wall plug power (MW)
-   rtanbeam : neutral beam centreline tangency radius (m)
-   rtanmax : maximum tangency radius for centreline of beam (m)
-   taubeam : neutral beam e-decay lengths to plasma centre
-   tbeamin /3.0/ : permitted neutral beam e-decay lengths to plasma
    centre

### [divertor\_kallenbach\_variables](divertor_kallenbach_variables.html)

-   kallenbach\_switch /0/ : Switch to turn on the 1D Kallenbach
    divertor model (1=on, 0=off)
-   kallenbach\_tests /0/ : Switch to run tests of 1D Kallenbach
    divertor model (1=on, 0=off)
-   kallenbach\_test\_option /0/ : Switch to choose kallenbach test
    option:
    -   = 0 Test case with user inputs;
    -   = 1 Test case for Kallenbach paper;
-   kallenbach\_scan\_switch /0/ : Switch to run scan of 1D Kallenbach
    divertor model (1=on, 0=off)
-   kallenbach\_scan\_var /0/ : Switch for parameter to scan for
    kallenbach scan test:
    -   = 0 ttarget
    -   = 1 qtargettotal
    -   = 2 targetangle
    -   = 3 lambda\_q\_omp
    -   = 4 netau\_sol
-   kallenbach\_scan\_start /2.0/ : Start value for kallenbach scan
    parameter
-   kallenbach\_scan\_end /10.0/ : End value for kallenbach scan
    parameter
-   kallenbach\_scan\_num /1/ : Number of scans for kallenbach scan test
-   target\_spread /0.003/ : Increase in SOL power fall-off length due
    to spreading, mapped to OMP \[m\]
-   lambda\_q\_omp /0.002/ : SOL power fall-off length at the outer
    midplane, perpendicular to field \[m\]
-   lcon\_factor /1.0/ : Correction factor for connection length from
    OMP to divertor = connection length/(pi\*q\*rmajor)
-   netau\_sol /0.5/ : Parameter describing the departure from local
    ionisation equilibrium in the SOL. \[ms.1e20
-   targetangle /30.0/ : Angle between field-line and divertor target
    (degrees)
-   ttarget /2.3/ : Plasma temperature adjacent to divertor sheath
    \[eV\] (iteration variable 120)
-   qtargettotal /5.0e6/ : Power density on target including surface
    recombination \[W/m2\] (iteration variable 124)
-   impurity\_enrichment(14) /5.0/ : Ratio of each impurity
    concentration in SOL to confined plasma+ the enrichment for Argon is
    also propagated for PLASMOD (ipedestal=3)
-   psep\_kallenbach : Power conducted through the separatrix, as
    calculated by the divertor model \[W\] Not equal to pdivt unless
    constraint is imposed.
-   teomp : separatrix temperature calculated by the Kallenbach divertor
    model \[eV\]
-   neomp : Mean SOL density at OMP calculated by the Kallenbach
    divertor model \[m-3\]
-   neratio /0.75/ : Ratio of mean SOL density at OMP to separatrix
    density at OMP (iteration variable 121)
-   pressure0 : Total plasma pressure near target (thermal+dynamic)
    \[Pa\]
-   fractionwidesol /0.1/ : Distance from target at which SOL gets
    broader as a fraction of connection length
-   fmom : momentum factor \[-\]
-   totalpowerlost : Total power lost due to radiation, ionisation and
    recombination \[W\]
-   impuritypowerlost : Power lost due to impurity radiation \[W\]
-   hydrogenicpowerlost : Power lost due to hydrogenic radiation \[W\]
-   exchangepowerlost : Power lost due to charge exchange \[W\]
-   ionisationpowerlost : Power lost due to electron impact ionisation
    \[W\]
-   abserr\_sol : Absolute contribution to the error tolerance in the
    Kallenbach divertor model
-   relerr\_sol : Relative contribution to the error tolerance in the
    Kallenbach divertor model
-   mach0 : Mach number at target (must be just less than 1)

### [divertor\_variables](divertor_variables.html)

-   adas : area divertor / area main plasma (along separatrix)
-   anginc /0.262/ : angle of incidence of field line on plate (rad)
-   betai /1.0/ : poloidal plane angle between divertor plate and leg,
    inboard (rad)
-   betao /1.0/ : poloidal plane angle between divertor plate and leg,
    outboard (rad)
-   bpsout /0.6/ : reference B\_p at outboard divertor strike point (T)
-   c1div /0.45/ : fitting coefficient to adjust ptpdiv, ppdiv
-   c2div /-7.0/ : fitting coefficient to adjust ptpdiv, ppdiv
-   c3div /0.54/ : fitting coefficient to adjust ptpdiv, ppdiv
-   c4div /-3.6/ : fitting coefficient to adjust ptpdiv, ppdiv
-   c5div /0.7/ : fitting coefficient to adjust ptpdiv, ppdiv
-   c6div /0.0/ : fitting coefficient to adjust ptpdiv, ppdiv
-   delld /1.0/ : coeff for power distribution along main plasma
-   dendiv : plasma density at divertor (10\*\*20 /m3)
-   densin : density at plate (on separatrix) (10\*\*20 /m3)
-   divclfr /0.3/ : divertor coolant fraction
-   divdens /1.0e4/ : divertor structure density (kg/m3)
-   divdum /0/ : switch for divertor Zeff model: 0=calc, 1=input
-   divfix /0.2/ : divertor structure vertical thickness (m)
-   divmas : divertor plate mass (kg)
-   divplt /0.035/ : divertor plate thickness (m) (from Spears,
    Sept 1990)
-   divsur : divertor surface area (m2)
-   fdfs /10.0/ : radial gradient ratio
-   fdiva /1.11/ : divertor area fudge factor (for ITER, Sept 1990)
-   fgamp /1.0/ : sheath potential factor (not used)
-   fhout : fraction of power to outboard divertor (for single null)
-   fififi /0.004/ : coefficient for gamdiv
-   frrp /0.4/ : fraction of radiated power to plate
-   hldiv : divertor heat load (MW/m2)
-   hldivlim /5.0/ : heat load limit (MW/m2)
-   ksic /0.8/ : power fraction for outboard double-null scrape-off
    plasma
-   lamp : power flow width (m)
-   minstang : minimum strike angle for heat flux calculation
-   omegan /1.0/ : pressure ratio (nT)\_plasma / (nT)\_scrape-off
-   omlarg : power spillage to private flux factor
-   ppdivr : peak heat load at plate (with radiation) (MW/m2)
-   prn1 /0.285/ : n-scrape-off / n-average plasma; (input for
    ipedestal=0, = nesep/dene if ipedestal\>=1)
-   ptpdiv : peak temperature at the plate (eV)
-   rconl : connection length ratio, outboard side
-   rlclolcn : ratio of collision length / connection length
-   rlenmax /0.5/ : maximum value for length ratio (rlclolcn) (eqn.22)
-   rsrd : effective separatrix/divertor radius ratio
-   tconl : main plasma connection length (m)
-   tdiv /2.0/ : temperature at divertor (eV) (input for stellarator
    only, calculated for tokamaks)
-   tsep : temperature at the separatrix (eV)
-   xparain /2.1e3/ : parallel heat transport coefficient (m2/s)
-   xpertin /2.0/ : perpendicular heat transport coefficient (m2/s)
-   zeffdiv /1.0/ : Zeff in the divertor region (if divdum /= 0)

### [fwbs\_variables](fwbs_variables.html)

-   bktlife : blanket lifetime (years)
-   coolmass : mass of water coolant (in shield, blanket, first wall,
    divertor) (kg)
-   vvmass : vacuum vessel mass (kg)
-   denstl /7800.0/ : density of steel (kg/m3)
-   denw /19250.0/ : density of tungsten (kg/m3)
-   dewmkg : total mass of vacuum vessel + cryostat (kg)
-   emult /1.269/ : energy multiplication in blanket and shield
-   emultmw : power due to energy multiplication in blanket and shield
    \[MW\]
-   fblss /0.09705/ : KIT blanket model: steel fraction of breeding zone
-   fdiv /0.115/ : area fraction taken up by divertor
-   fhcd /0.0/ : area fraction covered by heating/current drive
    apparatus plus diagnostics
-   fhole /0.0/ : area fraction taken up by other holes (IFE)
-   fwbsshape /2/ : first wall, blanket, shield and vacuum vessel shape:
    -   = 1 D-shaped (cylinder inboard + ellipse outboard);
    -   = 2 defined by two ellipses
-   fwlife : first wall full-power lifetime (y)
-   fwmass : first wall mass (kg)
-   fw\_armour\_mass : first wall armour mass (kg)
-   fw\_armour\_thickness /0.005/ : first wall armour thickness (m)
-   fw\_armour\_vol : first wall armour volume (m3)
-   iblanket /1/ : switch for blanket model:
    -   = 1 CCFE HCPB model;
    -   = 2 KIT HCPB model;
    -   = 3 CCFE HCPB model with Tritium Breeding Ratio calculation;
    -   = 4 KIT HCLL model
-   iblnkith /1/ : switch for inboard blanket:
    -   = 0 No inboard blanket (blnkith=0.0);
    -   = 1 Inboard blanket present
-   inuclear /0/ : switch for nuclear heating in the coils:
    -   = 0 Frances Fox model (default);
    -   = 1 Fixed by user (qnuc)
-   qnuc /0.0/ : nuclear heating in the coils (W) (inuclear=1)
-   li6enrich /30.0/ : lithium-6 enrichment of breeding material (%)
-   pnucblkt : nuclear heating in the blanket (MW)
-   pnuccp : nuclear heating in the ST centrepost (MW)
-   pnucdiv : nuclear heating in the divertor (MW)
-   pnucfw : nuclear heating in the first wall (MW)
-   pnuchcd : nuclear heating in the HCD apparatus and diagnostics (MW)
-   pnucloss : nuclear heating lost via holes (MW)
-   pnucloss : nuclear heating to vacuum vessel and beyond(MW)
-   pnucshld : nuclear heating in the shield (MW)
-   whtblkt : mass of blanket (kg)
-   whtblss : mass of blanket - steel part (kg)
-   armour\_fw\_bl\_mass : Total mass of armour, first wall and blanket
    (kg)
-   **The following are used only in the CCFE HCPB blanket model
    (iblanket=1):**

-   breeder\_f /0.5/ : Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (iteration
    variable 108)
-   breeder\_multiplier /0.75/ : combined breeder/multipler fraction of
    blanket by volume
-   vfcblkt /0.05295/ : He coolant fraction of blanket by volume
    (iblanket = 1 or 3 (CCFE HCPB))
-   vfpblkt /0.1/ : He purge gas fraction of blanket by volume (iblanket
    = 1 or 3 (CCFE HCPB))
-   whtblli4sio4 : mass of lithium orthosilicate in blanket (kg)
    (iblanket = 1 or 3 (CCFE HCPB))
-   whtbltibe12 : mass of titanium beryllide in blanket (kg) (iblanket =
    1 or 3 (CCFE HCPB))
-   **The following are used in the KIT HCPB blanket model
    (iblanket=2):**

-   breedmat /1/ : breeder material switch (iblanket=2 (KIT HCPB)):
    -   = 1 Lithium orthosilicate;
    -   = 2 Lithium methatitanate;
    -   = 3 Lithium zirconate
-   densbreed : density of breeder material (kg/m3) (iblanket=2 (KIT
    HCPB))
-   fblbe /0.6/ : beryllium fraction of blanket by volume (if
    (iblanket=2 (KIT HCPB)), Be fraction of breeding zone)
-   fblbreed /0.154/ : breeder fraction of blanket breeding zone by
    volume (iblanket=2 (KIT HCPB))
-   fblhebmi /0.40/ : helium fraction of inboard blanket box manifold by
    volume (iblanket=2 (KIT HCPB))
-   fblhebmo /0.40/ : helium fraction of outboard blanket box manifold
    by volume (iblanket=2 (KIT HCPB))
-   fblhebpi /0.6595/ : helium fraction of inboard blanket back plate by
    volume (iblanket=2 (KIT HCPB))
-   fblhebpo /0.6713/ : helium fraction of outboard blanket back plate
    by volume (iblanket=2 (KIT HCPB))
-   hcdportsize /1/ : size of heating/current drive ports (iblanket=2
    (KIT HCPB)):
    -   = 1 \'small\'
    -   = 2 \'large\'
-   nflutf : peak fast neutron fluence on TF coil superconductor (n/m2)
    (iblanket=2 (KIT HCPB))
-   npdiv /2/ : number of divertor ports (iblanket=2 (KIT HCPB))
-   nphcdin /2/ : number of inboard ports for heating/current drive
    (iblanket=2 (KIT HCPB))
-   nphcdout /2/ : number of outboard ports for heating/current drive
    (iblanket=2 (KIT HCPB))
-   tbr : tritium breeding ratio (iblanket=2,3 (KIT HCPB/HCLL))
-   tritprate : tritium production rate (g/day) (iblanket=2 (KIT HCPB))
-   vvhemax : maximum helium concentration in vacuum vessel at end of
    plant life (appm) (iblanket=2 (KIT HCPB))
-   wallpf /1.21/ : neutron wall load peaking factor (iblanket=2 (KIT
    HCPB))
-   whtblbreed : mass of blanket - breeder part (kg) (iblanket=2 (KIT
    HCPB))
-   whtblbe : mass of blanket - beryllium part (kg)
-   **CCFE HCPB model with Tritium Breeding Ratio calculation
    (iblanket=3):**

-   tbrmin /1.1/ : minimum tritium breeding ratio (constraint equation
    52) (If iblanket=1, tbrmin=minimum 5-year time-averaged tritium
    breeding ratio)
-   iblanket\_thickness /2/ : Blanket thickness switch:
    -   = 1 thin 0.53 m inboard, 0.91 m outboard
    -   = 2 medium 0.64 m inboard, 1.11 m outboard
    -   = 3 thick 0.75 m inboard, 1.30 m outboard
-   Do not set blnkith, blnkoth, fwith or fwoth when iblanket=3.
-   primary\_pumping /2/ : Switch for pumping power for primary coolant
    (06/01/2016): (mechanical power only)
    -   = 0 User sets pump power directly (htpmw\_blkt, htpmw\_fw,
        htpmw\_div, htpmw\_shld)
    -   = 1 User sets pump power as a fraction of thermal power
        (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)
    -   = 2 Mechanical pumping power is calculated
    -   = 3 Mechanical pumping power is calculated using specified
        pressure drop
-   (peak first wall temperature is only calculated if primary\_pumping
    = 2)
-   secondary\_cycle /0/ : Switch for power conversion cycle:
    -   = 0 Set efficiency for chosen blanket, from detailed models
        (divertor heat not used)
    -   = 1 Set efficiency for chosen blanket, from detailed models
        (divertor heat used)
    -   = 2 user input thermal-electric efficiency (etath)
    -   = 3 steam Rankine cycle
    -   = 4 supercritical CO2 cycle
-   coolwh : Blanket coolant (set via blkttype):
    -   = 1 helium;
    -   = 2 pressurized water
-   afwi /0.008/ : inner radius of inboard first wall/blanket coolant
    channels OBSOLETE (m)
-   afwo /0.008/ : inner radius of outboard first wall/blanket coolant
    channels OBSOLETE (m)
-   fwcoolant /helium/ : first wall coolant (can be different from
    blanket coolant) \'helium\' or \'water\' (27/11/2015)
-   fw\_wall /0.003/ : wall thickness of first wall coolant channels (m)
    (27/11/2015)
-   afw /0.006/ : radius of first wall cooling channels (m) (27/11/15)
-   pitch /0.020/ : pitch of first wall cooling channels (m) (27/11/15)
-   fwinlet /573/ : inlet temperature of first wall coolant (K)
    (27/11/2015)
-   fwoutlet /823/ : outlet temperature of first wall coolant (K)
    (27/11/2015)
-   fwpressure /15.5e6/ : first wall coolant pressure (Pa)
    (secondary\_cycle\>1)
-   tpeak : peak first wall temperature (K) (27/11/2015)
-   roughness /1e-6/ : first wall channel roughness epsilon (m)
    (27/11/2015)
-   fw\_channel\_length /4.0/ : Length of a single first wall channel
    (all in parallel) (m) (27/11/2015) (iteration variable 114, useful
    for constraint equation 39)
-   peaking\_factor /1.0/ : peaking factor for first wall heat loads
    (27/11/2015) (Applied separately to inboard and outboard loads.
    Applies to both neutron and surface loads. Only used to calculate
    peak temperature - not the coolant flow rate.)
-   blpressure /15.5e6/ : blanket coolant pressure (Pa)
    (secondary\_cycle\>1) (27/11/2015)
-   inlet\_temp /573.0/ : inlet temperature of blanket coolant (K)
    (secondary\_cycle\>1) (27/11/2015)
-   outlet\_temp /823.0/ : outlet temperature of blanket coolant (K)
    (27/11/2015)
    -   (secondary\_cycle\>1);
    -   input if coolwh=1 (helium), calculated if coolwh=2 (water)
-   coolp /15.5e6/ : blanket coolant pressure (Pa) stellarator ONLY
    (27/11/2015)
-   nblktmodpo /8/ : number of outboard blanket modules in poloidal
    direction (secondary\_cycle\>1)
-   nblktmodpi /7/ : number of inboard blanket modules in poloidal
    direction (secondary\_cycle\>1)
-   nblktmodto /48/ : number of outboard blanket modules in toroidal
    direction (secondary\_cycle\>1)
-   nblktmodti /32/ : number of inboard blanket modules in toroidal
    direction (secondary\_cycle\>1)
-   tfwmatmax /823.0/ : maximum temperature of first wall material (K)
    (secondary\_cycle\>1)
-   fw\_th\_conductivity /28.34/ : thermal conductivity of first wall
    material at 293 K (W/m/K) (Temperature dependence is as for
    unirradiated Eurofer)
-   fvoldw /1.74/ : area coverage factor for vacuum vessel volume
-   fvolsi /1.0/ : area coverage factor for inboard shield volume
-   fvolso /0.64/ : area coverage factor for outboard shield volume
-   fwclfr /0.15/ : first wall coolant fraction (calculated if lpulse=1
    or ipowerflow=1)
-   praddiv : radiation power incident on the divertor (MW)
-   pradfw : radiation power incident on the divertor (MW)
-   pradhcd : radiation power incident on the divertor (MW)
-   pradloss : radiation power incident on the divertor (MW)
-   ptfnuc : nuclear heating in the TF coil (MW)
-   ptfnucpm3 : nuclear heating in the TF coil (MW/m3) (blktmodel\>0)
-   rdewex : cryostat radius (m)
-   zdewex : cryostat height (m)
-   rpf2dewar /0.5/ : radial distance between outer edge of largest
    ipfloc=3 PF coil (or stellarator modular coil) and cryostat (m)
-   vdewex : cryostat volume (m3)
-   vdewin : vacuum vessel volume (m3)
-   vfshld /0.25/ : coolant void fraction in shield
-   volblkt : volume of blanket (m3)
-   volblkti : volume of inboard blanket (m3)
-   volblkto : volume of outboard blanket (m3)
-   volshld : volume of shield (m3)
-   whtshld : mass of shield (kg)
-   wpenshld : mass of the penetration shield (kg)
-   wtshldi : mass of inboard shield (kg)
-   wtshldo : mass of outboard shield (kg)
-   irefprop /1/ : obsolete
-   fblli2o /0.08/ : lithium oxide fraction of blanket by volume
    (blktmodel=0)
-   fbllipb /0.68/ : lithium lead fraction of blanket by volume
    (blktmodel=0)
-   fblvd /0.0/ : vanadium fraction of blanket by volume (blktmodel=0)
-   wtblli2o : mass of blanket - Li\_2O part (kg)
-   wtbllipb : mass of blanket - Li-Pb part (kg)
-   whtblvd : mass of blanket - vanadium part (kg)
-   whtblli : mass of blanket - lithium part (kg)
-   vfblkt /0.25/ : coolant void fraction in blanket (blktmodel=0),
    (calculated if blktmodel \> 0)
-   blktmodel /0/ : switch for blanket/tritium breeding model (but see
    `iblanket`):
    -   = 0 original simple model;
    -   = 1 KIT model based on a helium-cooled pebble-bed blanket (HCPB)
        reference design
-   declblkt /0.075/ : neutron power deposition decay length of blanket
    structural material (m) (Stellarators only)
-   declfw /0.075/ : neutron power deposition decay length of first wall
    structural material (m) (Stellarators only)
-   declshld /0.075/ : neutron power deposition decay length of shield
    structural material (m) (Stellarators only)
-   blkttype /3/ : Switch for blanket type:
    -   = 1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA\_D\_2M97B7
    -   = 2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA\_D\_2LLNBX
    -   = 3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA\_D\_2LLNBX
-   etaiso /0.85/ : isentropic efficiency of FW and blanket coolant
    pumps
-   etahtp /0.95/ : electrical efficiency of primary coolant pumps

### [primary\_pumping\_variables](primary_pumping_variables.html)

-   gamma\_he /1.667/ FIX : ratio of specific heats for helium
    (primary\_pumping=3)
-   cp\_he /5195/ FIX: specific heat capacity at constant pressure:
    helium (primary\_pumping=3) \[J/(kg.K)\]
-   t\_in\_bb /573.13/ FIX: temperature in FW and blanket coolant at
    blanket entrance (primary\_pumping=3) \[K\]
-   t\_out\_bb /773.13/ FIX: temperature in FW and blanket coolant at
    blanket exit (primary\_pumping=3) \[K\]
-   p\_he /8.0e6/ FIX: pressure in FW and blanket coolant at pump exit
    (primary\_pumping=3) \[Pa\]
-   dp\_he /5.5e5/ FIX: pressure drop in FW and blanket coolant
    including heat exchanger and pipes (primary\_pump
-   htpmw\_fw\_blkt : mechanical pumping power for FW and blanket
    including heat exchanger and pipes (primary\_pum

### [pfcoil\_variables](pfcoil_variables.html)

-   ngrpmx /8/ FIX : maximum number of groups of PF coils
-   nclsmx /2/ FIX : maximum number of PF coils in a given group
-   nptsmx /32/ FIX : maximum number of points across the midplane of
    the plasma at which the field from the PF coils is fixed
-   nfixmx /64/ FIX : maximum number of fixed current PF coils
-   alfapf /5.0e-10/ : smoothing parameter used in PF coil current
    calculation at the beginning of pulse (BoP)
-   alstroh /4.0D8/ : allowable hoop stress in Central Solenoid
    structural material (Pa)
-   i\_cs\_stress /0/ : Switch for CS stress calculation:
    -   = 0 Hoop stress only;
    -   = 1 Hoop + Axial stress
-   areaoh : central solenoid cross-sectional area (m2)
-   awpoh : central solenoid conductor+void area (m2)
-   bmaxoh : maximum field in central solenoid at end of flat-top
    (EoF) (T)
-   bmaxoh0 : maximum field in central solenoid at beginning of
    pulse (T)
-   bpf(ngc2) : peak field at coil i (T)
-   cohbop : central solenoid overall current density at beginning of
    pulse (A/m2)
-   coheof /1.85e7/ : central solenoid overall current density at end of
    flat-top (A/m2) (iteration variable 37)
-   cpt(ngc2,6) : current per turn in coil i at time j (A)
-   cptdin(ngc2) /4.0e4/: peak current per turn input for PF coil i (A)
-   curpfb(ngc2) : work array
-   curpff(ngc2) : work array
-   curpfs(ngc2) : work array
-   etapsu /0.9/ : Efficiency of transfer of PF stored energy into or
    out of storage.
-   fcohbof : ratio of central solenoid overall current density at
    beginning of flat-top / end of flat-top
-   fcohbop /0.9/ : ratio of central solenoid overall current density at
    beginning of pulse / end of flat-top (iteration variable 41)
-   fcuohsu /0.7/ : copper fraction of strand in central solenoid
-   fcupfsu /0.69/ : copper fraction of cable conductor (PF coils)
-   ipfloc(ngc) /2,2,3/ : switch for locating scheme of PF coil group i:
    -   = 1 PF coil on top of central solenoid;
    -   = 2 PF coil on top of TF coil;
    -   = 3 PF coil outside of TF coil
-   ipfres /0/ : switch for PF coil type:
    -   = 0 superconducting PF coils;
    -   = 1 resistive PF coils
-   itr\_sum : total sum of I x turns x radius for all PF coils and CS
    (Am)
-   isumatoh /1/ : switch for superconductor material in central
    solenoid:
    -   = 1 ITER Nb3Sn critical surface model with standard ITER
        parameters;
    -   = 2 Bi-2212 high temperature superconductor (range of validity T
        \< 20K, adjusted field b \< 104 T, B \> 6 T);
    -   = 3 NbTi;
    -   = 4 ITER Nb3Sn model with user-specified parameters
    -   = 5 WST Nb3Sn parameterisation
    -   = 6 REBCO HTS parameterisation
-   isumatpf /1/ : switch for superconductor material in PF coils:
    -   = 1 ITER Nb3Sn critical surface model with standard ITER
        parameters;
    -   = 2 Bi-2212 high temperature superconductor (range of validity T
        \< 20K, adjusted field b \< 104 T, B \> 6 T);
    -   = 3 NbTi;
    -   = 4 ITER Nb3Sn model with user-specified parameters
    -   = 5 WST Nb3Sn parameterisation
-   jscoh\_bop : central solenoid superconductor critical current
    density (A/m2) at beginning-of-pulse
-   jscoh\_eof : central solenoid superconductor critical current
    density (A/m2) at end-of-flattop
-   jstrandoh\_bop : central solenoid strand critical current density
    (A/m2) at beginning-of-pulse
-   jstrandoh\_eof : central solenoid strand critical current density
    (A/m2) at end-of-flattop
-   ncirt : number of PF circuits (including central solenoid and
    plasma)
-   ncls(ngrpmx+2) /1,1,2/ : number of PF coils in group j
-   nfxfh /7/ : number of filaments the top and bottom of the central
    solenoid should be broken into during scaling (5 - 10 is good)
-   ngrp /3/ : number of groups of PF coils. Symmetric coil pairs should
    all be in the same group
-   nohc : number of PF coils (excluding the central solenoid) + 1
-   ohhghf /0.71/ : central solenoid height / TF coil internal height
-   oh\_steel\_frac /0.5/ : central solenoid steel fraction (iteration
    variable 122)
-   pfcaseth(ngc2) : steel case thickness for PF coil i (m)
-   pfclres /2.5e-8/ : PF coil resistivity (if ipfres=1) (Ohm-m)
-   pfmmax : mass of heaviest PF coil (tonnes)
-   pfrmax : radius of largest PF coil (m)
-   pfsec : PF Coil waste heat (MW)
-   pfwpmw : Total mean wall plug power dissipated in PFC and CS power
    supplies. Issue \#713 (MW)
-   powohres : central solenoid resistive power during flattop (W)
-   powpfres : total PF coil resistive losses during flattop (W)
-   ra(ngc2) : inner radius of coil i (m)
-   rb(ngc2) : outer radius of coil i (m)
-   ric(ngc2) : peak current in coil i (MA-turns)
-   rjconpf(ngc2) /3.0e7/ : average winding pack current density of PF
    coil i (A/m2) at time of peak current in that coil (calculated for
    ipfloc=1 coils)
-   rjohc : allowable central solenoid current density at end of
    flat-top (A/m2)
-   rjohc0 : allowable central solenoid current density at beginning of
    pulse (A/m2)
-   rjpfalw(ngc2) : allowable winding pack current density of PF coil i
    (A/m2)
-   rohc : radius to the centre of the central solenoid (m)
-   routr /1.5/ : radial distance (m) from outboard TF coil leg to
    centre of ipfloc=3 PF coils
-   rpf(ngc2) : radius of PF coil i (m)
-   rpf1 /0.0/ : offset (m) of radial position of ipfloc=1 PF coils from
    being directly above the central solenoid
-   rpf2 /-1.63/ : offset (m) of radial position of ipfloc=2 PF coils
    from being at rmajor (offset = rpf2\*triang\*rminor)
-   s\_tresca\_oh : Tresca stress coils/central solenoid \[MPa\]
-   sigpfcalw /500.0/ : maximum permissible tensile stress (MPa) in
    steel coil cases for superconducting PF coils (ipfres=0)
-   sigpfcf /0.666/ : fraction of JxB hoop force supported by steel case
    for superconducting PF coils (ipfres=0)
-   sxlg(ngc2,ngc2) : mutual inductance matrix (H)
-   tmargoh : Central solenoid temperature margin (K)
-   turns(ngc2) : number of turns in PF coil i
-   vf(ngc2) /0.3/ : winding pack void fraction of PF coil i for coolant
-   vfohc /0.3/ : void fraction of central solenoid conductor for
    coolant
-   vsbn : total flux swing available for burn (Wb)
-   vsefbn : flux swing from PF coils for burn (Wb)
-   vsefsu : flux swing from PF coils for startup (Wb)
-   vseft : total flux swing from PF coils (Wb)
-   vsoh : total flux swing from the central solenoid (Wb)
-   vsohbn : central solenoid flux swing for burn (Wb)
-   vsohsu : central solenoid flux swing for startup (Wb)
-   vssu : total flux swing for startup (eqn 51 to enforce
    vssu=vsres+vsind) (Wb)
-   vstot : total flux swing for pulse (Wb)
-   waves(ngc2, 6) : used in current waveform of PF coils/central
    solenoid
-   whtpf : total mass of the PF coil conductor (kg)
-   whtpfs : total mass of the PF coil structure (kg)
-   wtc(ngc2) : conductor mass for PF coil i (kg)
-   wts(ngc2) : structure mass for PF coil i (kg)
-   zh(ngc2) : upper point of PF coil i (m)
-   zl(ngc2) : lower point of PF coil i (m)
-   zpf(ngc2) : z (height) location of PF coil i (m)
-   zref(ngrpmx) /../ : PF coil vertical positioning adjuster:
    -   \- for groups j with ipfloc(j) = 1; zref(j) is ignored
    -   \- for groups j with ipfloc(j) = 2 AND itart=1 (only); zref(j)
        is distance of centre of PF coil from inside edge of TF coil
        (remember that PF coils for STs lie within the TF coil)
    -   \- for groups j with ipfloc(j) = 3; zref(j) = ratio of height of
        coil group j to plasma minor radius
-   bmaxcs\_lim : Central solenoid max field limit \[T\]
-   fbmaxcs : F-value for CS mmax field (cons. 79, itvar 149)

### [tfcoil\_variables](tfcoil_variables.html)

-   acasetf : external case area per coil (inboard leg) (m2)
-   acasetfo : external case area per coil (outboard leg) (m2)
-   acndttf : area of the cable conduit (m2)
-   acond : conductor area (winding pack) (m2)
-   acstf : internal area of the cable space (m2)
-   insulation\_area : single turn insulation area (m2)
-   aiwp : winding pack insulation area (m2)
-   alstrtf /6.0D8/ : allowable Tresca stress in TF coil structural
    material (Pa)
-   arealeg : outboard TF leg area (m2)
-   aswp : winding pack structure area (m2)
-   avwp : winding pack void (He coolant) area (m2)
-   awphec : winding pack He coil area (m2)
-   bcritsc /24.0/ : upper critical field (T) for Nb3Sn superconductor
    at zero temperature and strain (isumattf=4, =bc20m)
-   bmaxtf : mean peak field at TF coil (T)
-   bmaxtfrp : peak field at TF conductor with ripple (T)
-   casestr : case strain
-   casthi /0.0/ : EITHER: inboard TF coil case plasma side thickness
    (m) (calculated for stellarators)
-   casthi\_fraction /0.05/ : OR: inboard TF coil case plasma side
    thickness as a fraction of tfcth
-   casths /0.0/ : EITHER: inboard TF coil sidewall case thickness (m)
    (calculated for stellarators)
-   casths\_fraction /0.03/ : OR: inboard TF coil sidewall case
    thickness as a fraction of tftort
-   conductor\_width : Width of square conductor (m)
-   leno : Dimension of each turn including inter-turn insulation (m)
-   leni : Dimension of space inside conductor (m)
-   acs : Area of space inside conductor (m2)
-   cdtfleg : TF outboard leg current density (A/m2) (resistive coils
    only)
-   cforce : centering force on inboard leg (per coil) (N/m)
-   cph2o /4180.0/ FIX : specific heat capacity of water (J/kg/K)
-   cpttf /7.0e4/ : TF coil current per turn (A). (calculated for
    stellarators) (calculated for integer-turn TF coils
    i\_tf\_turns\_integer=1) (iteration variable 60)
-   cpttf\_max /9.0e4/ : Max TF coil current per turn \[A\]. (For
    stellarators and i\_tf\_turns\_integer=1) (constraint equation 77)
-   dcase /8000.0/ : density of coil case (kg/m3)
-   dcond(6) /9000.0/ : density of superconductor type given by
    isumattf/isumatoh/isumatpf (kg/m3)
-   dcondins /1800.0/ : density of conduit + ground-wall insulation
    (kg/m3)
-   dcopper /8900.0/ : density of copper (kg/m3)
-   dalu /2700.0/ : density of aluminium (kg/m3)
-   deflect : TF coil deflection at full field (m)
-   denh2o /985.0/ FIX : density of water (kg/m3)
-   dhecoil /0.005/ : diameter of He coil in TF winding (m)
-   estotftgj : total stored energy in the toroidal field (GJ)
-   eyins /2.0e10/ : insulator Young\'s modulus (Pa) (default value from
    DDD11-2 v2 2 (2009))
-   eyoung(2) : work array used in stress calculation (Pa)
-   eystl /2.05e11/ : steel case Young\'s modulus (Pa) (default value
    from DDD11-2 v2 2 (2009))
-   eywp /6.6e8/ : winding pack Young\'s modulus (Pa)
-   eyzwp : winding pack vertical Young\'s modulus (Pa)
-   farc4tf /0.7/ : factor to size height of point 4 on TF coil
-   fcutfsu /0.69/ : copper fraction of cable conductor (TF coils)
    (iteration variable 59)
-   fhts /0.5/ : technology adjustment factor for critical current
    density fit for isumat..=2 Bi-2212 superconductor, to describe the
    level of technology assumed (i.e. to account for stress, fatigue,
    radiation, AC losses, joints or manufacturing variations; 1.0 would
    be very optimistic)
-   insstrain : radial strain in insulator
-   i\_tf\_tresca /0/ : switch for TF coil conduit Tresca stress
    criterion:
    -   = 0 Tresca (no adjustment);
    -   = 1 Tresca with CEA adjustment factors (radial+2%, vertical+60%)
-   i\_tf\_turns\_integer /0/ : switch for TF coil integer/non-integer
    turns
    -   = 0 non-integer turns;
    -   = 1 integer turns
-   isumattf /1/ : switch for superconductor material in TF coils:
    -   = 1 ITER Nb3Sn critical surface model with standard ITER
        parameters;
    -   = 2 Bi-2212 high temperature superconductor (range of validity T
        \< 20K, adjusted field b \< 104 T, B \> 6 T);
    -   = 3 NbTi;
    -   = 4 ITER Nb3Sn model with user-specified parameters
    -   = 5 WST Nb3Sn parameterisation
    -   = 6 REBCO HTS tape in CroCo strand
-   itfsup /1/ : switch for TF coil conductor model:
    -   = 0 copper;
    -   = 1 superconductor
-   jbus /1.25e6/ : bussing current density (A/m2)
-   jeff(2) : work array used in stress calculation (A/m2)
-   jwdgcrt : critical current density for winding pack (A/m2)
-   jwdgpro : allowable TF coil winding pack current density, for dump
    temperature rise protection (A/m2)
-   jwptf : winding pack current density (A/m2)
-   n\_pancake /10/ : Number of pancakes in TF coil
    (i\_tf\_turns\_integer=1)
-   n\_layer /20/ : Number of layers in TF coil
    (i\_tf\_turns\_integer=1)
-   oacdcp /1.4e7/ : overall current density in TF coil inboard legs
    (A/m2) (iteration variable 12)
-   poisson /0.3/ : Poisson\'s ratio for TF stress calculation (assumed
    constant over entire coil)
-   radtf(3) : work array used in stress calculation (m)
-   rbmax : radius of maximum TF B-field (m)
-   rhotfleg : TF coil leg resistance (ohm)
-   ripmax /1.0/ : maximum allowable toroidal field ripple amplitude at
    plasma edge (%)
-   ripple : peak/average toroidal field ripple at plasma edge (%)
-   ritfc : total (summed) current in TF coils (A)
-   sigrad : radial TF coil stress (MPa)
-   sigrcon : radial stress in the conductor conduit (Pa)
-   sigrtf(2) : radial stress in TF coil regions (Pa)
-   sigtan : transverse TF coil stress (MPa)
-   sigtcon : tangential stress in the conductor conduit (Pa)
-   sigttf(2) : tangential stress in TF coil regions (Pa)
-   s\_tresca\_case : TF coil case Tresca stress (MPa)
-   s\_tresca\_cond : TF coil conduit Tresca stress (MPa)
-   s\_vmises\_case : TF coil case von Mises stress (MPa)
-   s\_vmises\_cond : TF coil conduit von Mises stress (MPa)
-   sigver : vertical TF coil stress (MPa)
-   sigvert : vertical tensile stress in TF coil (Pa)
-   sigvvall /9.3e7/ : allowable stress from TF quench in vacuum vessel
    (Pa)
-   strncon\_cs /-0.005/ : strain in CS superconductor material (used in
    Nb3Sn critical surface model, isumatoh=1, 4 or 5)
-   strncon\_pf /-0.005/ : strain in PF superconductor material (used in
    Nb3Sn critical surface model, isumatph=1, 4 or 5)
-   strncon\_tf /-0.005/ : strain in TF superconductor material (used in
    Nb3Sn critical surface model, isumattf=1, 4 or 5)
-   strtf1 : Constrained stress in TF conductor conduit (Pa)
-   strtf2 : Constrained stress in TF coil case (Pa)
-   quench\_model /exponential/ : switch for TF coil quench model, Only
    applies to REBCO magnet at present.
    -   = \'exponential\' exponential quench with constant discharge
        resistor
    -   = \'linear\' quench with constant voltage
-   quench\_detection\_ef /0.0/ : Electric field at which TF quench is
    detected and discharge begins (V/m)
-   time1 : Time at which TF quench is detected (s)
-   taucq : allowable TF quench time (s)
-   tcritsc /16.0/ : critical temperature (K) for superconductor at zero
    field and strain (isumattf=4, =tc0m)
-   tdmptf /10.0/ : fast discharge time for TF coil in event of quench
    (s) (iteration variable 56) For REBCO model, meaning depends on
    quench\_model:
-   exponential quench : e-folding time (s)
-   linear quench : discharge time (s)
-   tfareain : area of inboard midplane TF legs (m2)
-   tfboreh : TF coil horizontal inner bore (m)
-   tf\_total\_h\_width : TF coil horizontal inner bore (m)
-   tfborev : TF coil vertical inner bore (m)
-   tfbusl : TF coil bus length (m)
-   tfbusmas : TF coil bus mass (kg)
-   tfckw : available DC power for charging the TF coils (kW)
-   tfc\_model /1/ : switch for TF coil magnet stress model:
    -   = 0 simple model (solid copper coil)
    -   = 1 CCFE two-layer stress model; superconductor
-   tfcmw : peak power per TF power supply (MW)
-   tfcpmw : peak resistive TF coil inboard leg power (MW)
-   tfcryoarea : surface area of toroidal shells covering TF coils (m2)
-   tficrn : TF coil half-width - inner bore (m)
-   tfind : TF coil inductance (H)
-   tfinsgap /0.010/ : TF coil WP insertion gap (m)
-   tflegmw : TF coil outboard leg resistive power (MW)
-   tflegres /2.5e-8/ : resistivity of a TF coil leg and bus(Ohm-m)
-   tfleng : TF coil circumference (m)
-   tfno /16.0/ : number of TF coils (default = 50 for stellarators)
    number of TF coils outer legs for ST
-   tfocrn : TF coil half-width - outer bore (m)
-   tfsai : area of the inboard TF coil legs (m2)
-   tfsao : area of the outboard TF coil legs (m2)
-   tftmp /4.5/ : peak helium coolant temperature in TF coils and PF
    coils (K)
-   tftort : TF coil toroidal thickness (m)
-   thicndut /8.0e-4/ : conduit insulation thickness (m)
-   layer\_ins /0/ : Additional insulation thickness between layers (m)
-   thkcas /0.3/ : inboard TF coil case outer (non-plasma side)
    thickness (m) (iteration variable 57) (calculated for stellarators)
-   thkwp /0.0/ : radial thickness of winding pack (m) (iteration
    variable 140)
-   thwcndut /8.0e-3/ : TF coil conduit case thickness (m) (iteration
    variable 58)
-   tinstf /0.018/ : ground insulation thickness surrounding winding
    pack (m)
-   Includes allowance for 10 mm insertion gap. (calculated for
    stellarators)
-   tmargmin\_tf /0/ : minimum allowable temperature margin : TF
    coils (K)
-   tmargmin\_cs /0/ : minimum allowable temperature margin : CS (K)
-   tmargmin /0/ : minimum allowable temperature margin : TFC AND CS (K)
-   temp\_margin : temperature margin (K)
-   tmargtf : TF coil temperature margin (K)
-   tmaxpro /150.0/ : maximum temp rise during a quench for
    protection (K)
-   tmax\_croco /200.0/ : CroCo strand: maximum permitted temp during a
    quench (K)
-   tmax\_jacket /150.0/ : Jacket: maximum temp during a quench (K)
-   croco\_quench\_temperature : CroCo strand: Actual temp reached
    during a quench (K)
-   tmpcry /4.5/ : coil temperature for cryogenic plant power
    calculation (K)
-   turnstf : number of turns per TF coil
-   vdalw /20.0/ : max voltage across TF coil during quench (kV)
    (iteration variable 52)
-   vforce : vertical separating force on inboard leg/coil (N)
-   vftf /0.4/ : coolant fraction of TFC \'cable\' (itfsup=1), or of TFC
    leg (itfsup=0)
-   voltfleg : volume of each TF coil outboard leg (m3)
-   vtfkv : TF coil voltage for resistive coil including bus (kV)
-   vtfskv : voltage across a TF coil during quench (kV)
-   whtcas : mass per coil of external case (kg)
-   whtcon : TF coil conductor mass per coil (kg)
-   whtconcu : copper mass in TF coil conductor (kg/coil)
-   whtconin : conduit insulation mass in TF coil conductor (kg/coil)
-   whtconsc : superconductor mass in TF coil cable (kg/coil)
-   whtconsh : steel conduit mass in TF coil conductor (kg/coil)
-   whtgw : mass of ground-wall insulation layer per coil (kg/coil)
-   whttf : total mass of the TF coils (kg)
-   windstrain : longitudinal strain in winding pack
-   wwp1 : width of first step of winding pack (m)
-   wwp2 : width of second step of winding pack (m)
-   **Superconducting TF coil shape parameters** (see also farc4tf);\
    the TF inner surface top half is approximated by four circular arcs.
    Arc 1 goes through points 1 and 2 on the inner surface. Arc 2 goes
    through points 2 and 3, etc.

-   dthet(4) : angle of arc i (rad)
-   radctf(4) : radius of arc i (m)
-   xarc(5) : x location of arc point i on surface (m)
-   xctfc(4) : x location of arc centre i (m)
-   yarc(5) : y location of arc point i on surface (m)
-   yctfc(4) : y location of arc centre i (m)
-   tfa(4) : Horizontal radius of inside edge of TF coil (m)
-   tfb(4) : Vertical radius of inside edge of TF coil (m)
-   **Quantities relating to the spherical tokamak model (itart=1)**
    (and in some cases, also to resistive TF coils, itfsup=0):

-   drtop /0.0/ : centrepost taper maximum radius adjustment (m)
-   dztop /0.0/ : centrepost taper height adjustment (m)
-   etapump /0.8/ : centrepost coolant pump efficiency
-   fcoolcp /0.3/ : coolant fraction of TF coil inboard legs (iteration
    variable 23)
-   frhocp /1.0/ : centrepost resistivity enhancement factor
-   k\_copper /330.0/ FIX : Copper thermal conductivity (W/m/K)
-   kh2o /0.651/ FIX : thermal conductivity of water (W/m/K)
-   muh2o /4.71e-4/ FIX : water dynamic viscosity (kg/m/s)
-   ncool : number of centrepost coolant tubes
-   ppump : centrepost coolant pump power (W)
-   prescp : resistive power in the centrepost (W)
-   ptempalw /200.0/ : maximum peak centrepost temperature (C)
    (constraint equation 44)
-   rcool /0.005/ : average radius of coolant channel (m) (iteration
    variable 69)
-   rhocp : TF coil inboard leg resistivity (Ohm-m)
-   tcoolin /40.0/ : centrepost coolant inlet temperature (C)
-   tcpav /100.0/ : average temp of TF coil inboard leg conductor (C)
    (resistive coils) (iteration variable 20)
-   tcpav2 : centrepost average temperature (C) (for consistency)
-   tcpmax : peak centrepost temperature (C)
-   vcool /20.0/ : max centrepost coolant flow speed at midplane (m/s)
    (iteration variable 70)
-   volcp : total volume of TF coil inboard legs (m3)
-   whtcp : mass of TF coil inboard legs (kg)
-   whttflgs : mass of the TF coil legs (kg)

### [structure\_variables](structure_variables.html)

-   aintmass : intercoil structure mass (kg)
-   clgsmass : gravity support structure for TF coil, PF coil and
    intercoil support systems (kg)
-   coldmass : total mass of components at cryogenic temperatures (kg)
-   fncmass : PF coil outer support fence mass (kg)
-   gsmass : reactor core gravity support mass (kg)

### [vacuum\_variables](vacuum_variables.html)

-   vacuum\_model /old/ : switch for vacuum pumping model:
    -   = \'old\' for old detailed ETR model;
    -   = \'simple\' for simple steady-state model with comparison to
        ITER cryopumps
-   niterpump : number of high vacuum pumps (real number), each with the
    throughput of one ITER cryopump (50 Pa m3 s-1), all operating at the
    same time (vacuum\_model = \'simple\')
-   ntype /1/ : switch for vacuum pump type:
    -   = 0 for turbomolecular pump (magnetic bearing) with speed of 2.0
        m3/s (1.95 for N2, 1.8 for He, 1.8 for DT);
    -   = 1 for compound cryopump with nominal speed of 10.0 m3/s (9.0
        for N2, 5.0 for He and 25.0 for DT)
-   nvduct : number of ducts (torus to pumps)
-   dlscal : vacuum system duct length scaling
-   pbase /5.0e-4/ : base pressure during dwell before gas pre-fill(Pa)
-   prdiv /0.36/ : divertor chamber pressure during burn (Pa)
-   pumptp /1.2155D22/ : Pump throughput (molecules/s) (default is ITER
    value)
-   rat /1.3e-8/ : plasma chamber wall outgassing rate (Pa-m/s)
-   tn /300.0/ : neutral gas temperature in chamber (K)
-   vacdshm : mass of vacuum duct shield (kg)
-   vcdimax : diameter of duct passage (m)
-   vpumpn : number of high vacuum pumps
-   dwell\_pump /0/ : switch for dwell pumping options:
    -   = 0 pumping only during tdwell;
    -   = 1 pumping only during tramp
    -   = 2 pumping during tdwell + tramp
-   **The following are used in the Battes, Day and Rohde pump-down
    model See \"Basic considerations on the pump-down time in the dwell
    phase of a pulsed fusion DEMO\"
    http://dx.doi.org/10.1016/j.fusengdes.2015.07.011)
    (vacuum\_model=simple\'):**

-   pumpareafraction /0.0203/ : area of one pumping port as a fraction
    of plasma surface area
-   pumpspeedmax /27.3/ : maximum pumping speed per unit area for
    deuterium & tritium, molecular flow
-   pumpspeedfactor /0.167/ : effective pumping speed reduction factor
    due to duct impedance
-   initialpressure /1.0/ : initial neutral pressure at the beginning of
    the dwell phase (Pa)
-   pbase /5.0e-4/ : base pressure during dwell before gas pre-fill (Pa)
-   outgasindex /1.0/ : outgassing decay index
-   outgasfactor /0.0235/ : outgassing prefactor kw: outgassing rate at
    1 s per unit area (Pa m s-1)

### [pf\_power\_variables](pf_power_variables.html)

-   acptmax : average of currents in PF circuits (kA)
-   ensxpfm : maximum stored energy in the PF circuits (MJ)
-   iscenr /2/ : Switch for PF coil energy storage option:
    -   = 1 all power from MGF (motor-generator flywheel) units;
    -   = 2 all pulsed power from line;
    -   = 3 PF power from MGF, heating from line

    (In fact, options 1 and 3 are not treated differently)
-   pfckts : number of PF coil circuits
-   spfbusl : total PF coil circuit bus length (m)
-   spsmva : sum of PF power supply ratings (MVA)
-   srcktpm : sum of resistive PF coil power (kW)
-   vpfskv : PF coil voltage (kV)
-   peakpoloidalpower : Peak absolute rate of change of stored energy in
    poloidal field (MW) (11/01/16)
-   maxpoloidalpower /1000/ : Maximum permitted absolute rate of change
    of stored energy in poloidal field (MW)
-   poloidalpower : Poloidal power usage at time t (MW)

### [heat\_transport\_variables](heat_transport_variables.html)

-   baseel /5.0e6/ : base plant electric load (W)
-   crypmw : cryogenic plant power (MW)
-   etatf /0.9/ : AC to resistive power conversion for TF coils
-   etath /0.35/ : thermal to electric conversion efficiency if
    secondary\_cycle=2; otherwise calculated
-   fachtmw : facility heat removal (MW)
-   fcsht : total baseline power required at all times (MW)
-   fgrosbop : scaled fraction of gross power to balance-of-plant
-   fmgdmw /0.0/ : power to mgf (motor-generator flywheel) units (MW)
    (ignored if iscenr=2)
-   fpumpblkt /0.005/ : fraction of total blanket thermal power required
    to drive the blanket coolant pumps (default assumes water coolant)
    (secondary\_cycle=0)
-   fpumpdiv /0.005/ : fraction of total divertor thermal power required
    to drive the divertor coolant pumps (default assumes water coolant)
-   fpumpfw /0.005/ : fraction of total first wall thermal power
    required to drive the FW coolant pumps (default assumes water
    coolant) (secondary\_cycle=0)
-   fpumpshld /0.005/ : fraction of total shield thermal power required
    to drive the shield coolant pumps (default assumes water coolant)
-   htpmw\_min /0.0/ : Minimum total electrical power for primary
    coolant pumps (MW) NOT RECOMMENDED
-   helpow : heat removal at cryogenic temperatures (W)
-   htpmw :: heat transport system electrical pump power (MW)
-   htpmw\_blkt /0.0/ : blanket coolant mechanical pumping power (MW)
-   htpmw\_div /0.0/ : divertor coolant mechanical pumping power (MW)
-   htpmw\_fw /0.0/ : first wall coolant mechanical pumping power (MW)
-   htpmw\_shld /.0/ : shield and vacuum vessel coolant mechanical
    pumping power (MW)
-   htpsecmw : Waste power lost from primary coolant pumps (MW)
-   ipowerflow /1/ : switch for power flow model:
    -   = 0 pre-2014 version;
    -   = 1 comprehensive 2014 model
-   iprimnloss /0/ : switch for lost neutron power through holes destiny
    (ipowerflow=0):
    -   = 0 does not contribute to energy generation cycle;
    -   = 1 contributes to energy generation cycle
-   iprimshld /1/ : switch for shield thermal power destiny:
    -   = 0 does not contribute to energy generation cycle;
    -   = 1 contributes to energy generation cycle
-   nphx : number of primary heat exchangers
-   pacpmw : total pulsed power system load (MW)
-   peakmva : peak MVA requirement
-   pfwdiv : heat removal from first wall/divertor (MW)
-   pgrossmw : gross electric power (MW)
-   pinjht : power dissipated in heating and current drive system (MW)
-   pinjmax : maximum injector power during pulse (heating and
    ramp-up/down phase) (MW)
-   pinjwp : injector wall plug power (MW)
-   pnetelmw : net electric power (MW)
-   precircmw : recirculating electric power (MW)
-   priheat : total thermal power removed from fusion core (MW)
-   psecdiv : Low-grade heat lost in divertor (MW)
-   psechcd : Low-grade heat lost into HCD apparatus (MW)
-   psechtmw : Low-grade heat (MW)
-   pseclossmw : Low-grade heat (VV + lost)(MW)
-   psecshld : Low-grade heat deposited in shield (MW)
-   pthermmw : High-grade heat useful for electric production (MW)
-   pwpm2 /150.0/ : base AC power requirement per unit floor area (W/m2)
-   tfacpd : total steady state TF coil AC power demand (MW)
-   tlvpmw : estimate of total low voltage power (MW)
-   trithtmw /15.0/ : power required for tritium processing (MW)
-   tturb : coolant temperature at turbine inlet (K) (secondary\_cycle =
    3,4)
-   vachtmw /0.5/ : vacuum pump power (MW)

### [times\_variables](times_variables.html)

-   pulsetimings /0.0/ : switch for pulse timings (if lpulse=1):
    -   = 0, tohs = Ip(MA)/0.1 tramp, tqnch = input;
    -   = 1, tohs = iteration var or input. tramp/tqnch max of input or
        tohs
-   tburn /1000.0/ : burn time (s) (calculated if lpulse=1)
-   tburn0 : burn time (s) - used for internal consistency
-   tcycle : full cycle time (s)
-   tdown : down time (s)
-   tdwell /1800.0/ : time between pulses in a pulsed reactor (s)
    (iteration variable 17)
-   theat /10.0/ : heating time, after current ramp up (s)
-   tim(6) : array of time points during plasma pulse (s)
-   timelabel(6) : array of time labels during plasma pulse (s)
-   intervallabel(6) : time intervals - as strings (s)
-   tohs /30.0/ : plasma current ramp-up time for current initiation (s)
    (but calculated if lpulse=0) (iteration variable 65)
-   tohsin /0.0/ : switch for plasma current ramp-up time (if lpulse=0):
    -   = 0, tohs = tramp = tqnch = Ip(MA)/0.5;
    -   \<\>0, tohs = tohsin; tramp, tqnch are input
-   tpulse : pulse length = tohs + theat + tburn + tqnch
-   tqnch /15.0/ : shut down time for PF coils (s); if pulsed, = tohs
-   tramp /15.0/ : initial PF coil charge time (s); if pulsed, = tohs

### [buildings\_variables](buildings_variables.html)

-   admv /1.0e5/ : administration building volume (m3)
-   admvol : volume of administration buildings (m3)
-   clh1 /2.5/ : vertical clearance from TF coil to cryostat (m)
    (calculated for tokamaks)
-   clh2 /15.0/ : clearance beneath TF coil to foundation (including
    basement) (m)
-   conv /6.0e4/ : control building volume (m3)
-   convol : volume of control, protection and i&c building (m3)
-   cryvol : volume of cryoplant building (m3)
-   efloor : effective total floor space (m2)
-   elevol : volume of electrical equipment building (m3)
-   esbldgm3 /1.0e3/ : volume of energy storage equipment building (m3)
    (not used if lpulse=0)
-   fndt /2.0/ : foundation thickness (m)
-   hccl /5.0/ : clearance around components in hot cell (m)
-   hcwt /1.5/ : hot cell wall thickness (m)
-   mbvfac /2.8/ : maintenance building volume multiplication factor
-   pfbldgm3 /2.0e4/ : volume of PF coil power supply building (m3)
-   pibv /2.0e4/ : power injection building volume (m3)
-   rbrt /1.0/ : reactor building roof thickness (m)
-   rbvfac /1.6/ : reactor building volume multiplication factor
-   rbvol : reactor building volume (m3)
-   rbwt /2.0/ : reactor building wall thickness (m)
-   rmbvol : volume of maintenance and assembly building (m3)
-   row /4.0/ : clearance to building wall for crane operation (m)
-   rxcl /4.0/ : clearance around reactor (m)
-   shmf /0.5/ : fraction of shield mass per TF coil to be moved in the
    maximum shield lift
-   shov /1.0e5/ : shops and warehouse volume (m3)
-   shovol :volume of shops and buildings for plant auxiliaries (m3)
-   stcl /3.0/ : clearance above crane to roof (m)
-   tfcbv /2.0e4/ : volume of TF coil power supply building (m3)
    (calculated if TF coils are superconducting)
-   trcl /1.0/ : transportation clearance between components (m)
-   triv /4.0e4/ : volume of tritium, fuel handling and health physics
    buildings (m3)
-   volnucb : sum of nuclear buildings volumes (m3)
-   volrci : internal volume of reactor building (m3)
-   wgt /5.0e5/ : reactor building crane capacity (kg) (calculated if 0
    is input)
-   wgt2 /1.0e5/ : hot cell crane capacity (kg) (calculated if 0 is
    input)
-   wrbi : distance from centre of machine to building wall (m), i.e.
    reactor building half-width
-   wsvfac /1.9/ : warm shop building volume multiplication factor
-   wsvol : volume of warm shop building (m3)

### [build\_variables](build_variables.html)

-   aplasmin /0.25/ : minimum minor radius (m)
-   blarea : blanket total surface area (m2)
-   blareaib : inboard blanket surface area (m2)
-   blareaob : outboard blanket surface area (m2)
-   blbmith /0.17/ : inboard blanket box manifold thickness (m)
    (blktmodel\>0)
-   blbmoth /0.27/ : outboard blanket box manifold thickness (m)
    (blktmodel\>0)
-   blbpith /0.30/ : inboard blanket base plate thickness (m)
    (blktmodel\>0)
-   blbpoth /0.35/ : outboard blanket base plate thickness (m)
    (blktmodel\>0)
-   blbuith /0.365/ : inboard blanket breeding zone thickness (m)
    (blktmodel\>0) (iteration variable 90)
-   blbuoth /0.465/ : outboard blanket breeding zone thickness (m)
    (blktmodel\>0) (iteration variable 91)
-   blnkith /0.115/ : inboard blanket thickness (m); (calculated if
    blktmodel \> 0) (=0.0 if iblnkith=0)
-   blnkoth /0.235/ : outboard blanket thickness (m); calculated if
    blktmodel \> 0
-   blnktth : top blanket thickness (m), = mean of inboard and outboard
    blanket thicknesses
-   bore /1.42/ : central solenoid inboard radius (m) (iteration
    variable 29)
-   clhsf /4.268/ : cryostat lid height scaling factor (tokamaks)
-   ddwex /0.07/ : cryostat thickness (m)
-   ddwi /0.07/ : vacuum vessel thickness (TF coil / shield) (m)
-   fcspc /0.6/ : Fraction of space occupied by CS pre-compression
    structure
-   fmsbc /0.0/ : Martensitic fraction of steel in (non-existent!)
    bucking cylinder
-   fmsbl /0.0/ : Martensitic fraction of steel in blanket
-   fmsdwe /0.0/ : Martensitic fraction of steel in cryostat
-   fmsdwi /0.0/ : Martensitic fraction of steel in vacuum vessel
-   fmsfw /0.0/ : Martensitic fraction of steel in first wall
-   fmsoh /0.0/ : Martensitic fraction of steel in central solenoid
-   fmssh /0.0/ : Martensitic fraction of steel in shield
-   fmstf /0.0/ : Martensitic fraction of steel in TF coil
-   fseppc /3.5d8/ : Separation force in CS coil pre-compression
    structure
-   fwarea : first wall total surface area (m2)
-   fwareaib : inboard first wall surface area (m2)
-   fwareaob : outboard first wall surface area (m2)
-   fwith : inboard first wall thickness, initial estimate (m)
-   fwoth : outboard first wall thickness, initial estimate (m)
-   gapds /0.155/ : gap between inboard vacuum vessel and thermal shield
    (m) (iteration variable 61)
-   gapoh /0.08/ : gap between central solenoid and TF coil (m)
    (iteration variable 42)
-   gapomin /0.234/ : minimum gap between outboard vacuum vessel and TF
    coil (m) (iteration variable 31)
-   gapsto : gap between outboard vacuum vessel and TF coil (m)
-   hmax : maximum (half-)height of TF coil (inside edge) (m)
-   hpfdif : difference in distance from midplane of upper and lower
    portions of TF legs (non-zero for single-null devices) (m)
-   hpfu : height to top of (upper) TF coil leg (m)
-   hr1 : half-height of TF coil inboard leg straight section (m)
-   iohcl /1/ : switch for existence of central solenoid:
    -   = 0 central solenoid not present;
    -   = 1 central solenoid exists
-   iprecomp /1/ : switch for existence of central solenoid
    pre-compression structure:
    -   = 0 no pre-compression structure;
    -   = 1 calculated pre-compression structure
-   ohcth /0.811/ : central solenoid thickness (m) (iteration
    variable 16)
-   precomp : CS coil precompression structure thickness (m)
-   rbld : sum of thicknesses to the major radius (m)
-   rinboard /0.651/ : plasma inboard radius (m) (consistency
    equation 29)
-   rsldi : radius to inboard shield (inside point) (m)
-   rsldo : radius to outboard shield (outside point) (m)
-   r\_tf\_inleg\_mid : radius of centre of inboard TF leg (m)
-   rtot : radius to the centre of the outboard TF coil leg (m)
-   scrapli /0.14/ : gap between plasma and first wall, inboard side (m)
    (used if iscrp=1) (iteration variable 73)
-   scraplo /0.15/ : gap between plasma and first wall, outboard side
    (m) (used if iscrp=1) (iteration variable 74)
-   sharea : shield total surface area (m2)
-   shareaib : inboard shield surface area (m2)
-   shareaob : outboard shield surface area (m2)
-   shldith /0.69/ : inboard shield thickness (m) (iteration
    variable 93)
-   shldlth /0.7/ : lower (under divertor) shield thickness (m)
-   shldoth /1.05/ : outboard shield thickness (m) (iteration
    variable 94)
-   shldtth /0.60/ : upper/lower shield thickness (m); calculated if
    blktmodel \> 0
-   sigallpc /3.0d8/ : allowable stress in CSpre-compression structure
    (Pa);
-   tfcth /1.173/ : inboard TF coil(s ) thickness (m) (calculated for
    stellarators) (iteration variable 13)
-   tfcth : inboard TF coil thickness, (centrepost for ST) (m)
    (calculated, NOT an iteration variable)
-   tfoffset : vertical distance between centre of TF coils and centre
    of plasma (m)
-   tfootfi /1.19/ : TF coil outboard leg / inboard leg radial thickness
    ratio (itfsup=0 only) (iteration variable 75)
-   tfthko : outboard TF coil thickness (m)
-   tftsgap /0.05/ : Minimum metal-to-metal gap between TF coil and
    thermal shield (m)
-   thshield /0.05/ : TF-VV thermal shield thickness (m)
-   vgap2 /0.163/ : vertical gap between vacuum vessel and thermal
    shields (m)
-   vgap /0.0/ : vertical gap between x-point and divertor (m) (if = 0,
    it is calculated)
-   vgaptop /0.60/ : vertical gap between top of plasma and first
    wall (m)
-   vvblgap /0.05/ : gap between vacuum vessel and blanket (m)
-   plleni /1.0/ : length of inboard divertor plate (m)
-   plleno /1.0/ : length of outboard divertor plate (m)
-   plsepi /1.0/ : poloidal length, x-point to inboard strike point (m)
-   plsepo /1.5/ : poloidal length, x-point to outboard strike point (m)
-   rspo : outboard strike point radius (m)

### [cost\_variables](cost_variables.html)

-   abktflnc /5.0/ : allowable first wall/blanket neutron fluence
    (MW-yr/m2) (blktmodel=0)
-   adivflnc /7.0/ : allowable divertor heat fluence (MW-yr/m2)
-   blkcst : blanket direct cost (M\$)
-   c221 : total account 221 cost (M\$) (first wall, blanket, shield,
    support structure and divertor plates)
-   c222 : total account 222 cost (M\$) (TF coils + PF coils)
-   capcost : total capital cost including interest (M\$)
-   cconfix /80.0/ : fixed cost of superconducting cable (\$/m)
-   cconshpf /70.0/ : cost of PF coil steel conduit/sheath (\$/m)
-   cconshtf /75.0/ : cost of TF coil steel conduit/sheath (\$/m)
-   cdcost : current drive direct costs (M\$)
-   cdirt : total plant direct cost (M\$)
-   cdrlife : lifetime of heating/current drive system (y)
-   cfactr /0.75/ : Total plant availability fraction; input if iavail =
    0
-   cpfact : Total plant capacity factor
-   cfind(4) /0.244,0.244,0.244,0.29/ : indirect cost factor (func of
    lsa)
-   cland /19.2/ : cost of land (M\$)
-   coe : cost of electricity (\$/MW-hr)
-   coecap : capital cost of electricity (m\$/kW-hr)
-   coefuelt : \'fuel\' (including replaceable components) contribution
    to cost of electricity (m\$/kW-hr)
-   coeoam : operation and maintenance contribution to cost of
    electricity (m\$/kW-hr)
-   concost : plant construction cost (M\$)
-   costexp /0.8/ : cost exponent for scaling in 2015 costs model
-   costexp\_pebbles /0.6/ : cost exponent for pebbles in 2015 costs
    model
-   cost\_factor\_buildings /1.0/ : cost scaling factor for buildings
-   cost\_factor\_land /1.0/ : cost scaling factor for land
-   cost\_factor\_tf\_coils /1.0/ : cost scaling factor for TF coils
-   cost\_factor\_fwbs /1.0/ : cost scaling factor for fwbs
-   cost\_factor\_rh /1.0/ : cost scaling factor for remote handling
-   cost\_factor\_vv /1.0/ : cost scaling factor for vacuum vessel
-   cost\_factor\_bop /1.0/ : cost scaling factor for energy conversion
    system
-   cost\_factor\_misc /1.0/ : cost scaling factor for remaining
    subsystems
-   maintenance\_fwbs /0.2/ : Maintenance cost factor: first wall,
    blanket, shield, divertor
-   maintenance\_gen /0.05/ : Maintenance cost factor: All other
    components except coils, vacuum vessel, thermal shield, cryostat,
    land
-   amortization /13.6/ : amortization factor (fixed charge factor)
    \"A\" (years)
-   cost\_model /1/ : switch for cost model:
    -   = 0 use \$ 1990 PROCESS model
    -   = 1 use \$ 2014 Kovari model
    -   = 2 use \$ 1980 STEP model (NOT RECOMMENDED - Under Development)
-   cowner /0.15/ : owner cost factor
-   cplife : lifetime of centrepost (y)
-   cpstcst : ST centrepost direct cost (M\$)
-   cpstflnc /10.0/ : allowable ST centrepost neutron fluence (MW-yr/m2)
-   crctcore : reactor core costs (categories 221, 222 and 223)
-   csi /16.0/ : allowance for site costs (M\$)
-   cturbb /38.0/ : cost of turbine building (M\$)
-   decomf /0.1/ : proportion of constructed cost required for
    decommissioning fund
-   dintrt /0.0/ : diff between borrowing and saving interest rates
-   divcst : divertor direct cost (M\$)
-   divlife : lifetime of divertor (y)
-   dtlife /0.0/ : period prior to the end of the plant life that the
    decommissioning fund is used (years)
-   fcap0 /1.165/ : average cost of money for construction of plant
    assuming design/construction time of six years
-   fcap0cp /1.08/ : average cost of money for replaceable components
    assuming lead time for these of two years
-   fcdfuel /0.1/ : fraction of current drive cost treated as fuel (if
    ifueltyp = 1)
-   fcontng /0.195/ : project contingency factor
-   fcr0 /0.0966/ : fixed charge rate during construction
-   fkind /1.0/ : multiplier for Nth of a kind costs
-   fwallcst : first wall cost (M\$)
-   iavail /2/ : switch for plant availability model:
    -   = 0 use input value for cfactr;
    -   = 1 calculate cfactr using Taylor and Ward 1999 model;
    -   = 2 calculate cfactr using new (2015) model
-   avail\_min /0.75/ : Minimum availability (constraint equation 61)
-   tok\_build\_cost\_per\_vol /1283.0/ : Unit cost for tokamak complex
    buildings, including building and site services (\$/m3)
-   light\_build\_cost\_per\_vol /270.0/ : Unit cost for unshielded
    non-active buildings (\$/m3)
-   favail /1.0/ : F-value for minimum availability (constraint
    equation 61)
-   num\_rh\_systems /4/ : Number of remote handling systems (1-10)
-   conf\_mag /0.99/ : c parameter, which determines the temperature
    margin at which magnet lifetime starts to d
-   div\_prob\_fail /0.0002/ : Divertor probability of failure (per op
    day)
-   div\_umain\_time /0.25/ : Divertor unplanned maintenance time
    (years)
-   div\_nref /7000/ : Reference value for cycle cycle life of divertor
-   div\_nu /14000/ : The cycle when the divertor fails with 100%
    probability
-   fwbs\_nref /20000/ : Reference value for cycle life of blanket
-   fwbs\_nu /40000/ : The cycle when the blanket fails with 100%
    probability
-   fwbs\_prob\_fail /0.0002/ : Fwbs probability of failure (per op day)
-   fwbs\_umain\_time /0.25/ : Fwbs unplanned maintenance time (years)
-   redun\_vacp /25/ : Vacuum system pump redundancy level (%)
-   redun\_vac : Number of redundant vacuum pumps
-   t\_operation : Operational time (yrs)
-   tbktrepl /0.5/ : time taken to replace blanket (y) (iavail=1)
-   tcomrepl /0.5/ : time taken to replace both blanket and divertor (y)
    (iavail=1)
-   tdivrepl /0.25/ : time taken to replace divertor (y) (iavail=1)
-   uubop /0.02/ : unplanned unavailability factor for balance of plant
    (iavail=1)
-   uucd /0.02/ : unplanned unavailability factor for current drive
    (iavail=1)
-   uudiv /0.04/ : unplanned unavailability factor for divertor
    (iavail=1)
-   uufuel /0.02/ : unplanned unavailability factor for fuel system
    (iavail=1)
-   uufw /0.04/ : unplanned unavailability factor for first wall
    (iavail=1)
-   uumag /0.02/ : unplanned unavailability factor for magnets
    (iavail=1)
-   uuves /0.04/ : unplanned unavailability factor for vessel (iavail=1)
-   ifueltyp /0/ : switch:
    -   = 1 treat blanket divertor, first wall and fraction fcdfuel of
        CD equipment as fuel cost;
    -   = 0 treat these as capital cost
-   ipnet /0/ : switch for net electric power calculation:
    -   = 0 scale so that always \> 0;
    -   = 1 let go \< 0 (no c-o-e)
-   ireactor /1/ : switch for net electric power and cost of electricity
    calculations:
    -   = 0 do not calculate MW(electric) or c-o-e;
    -   = 1 calculate MW(electric) and c-o-e
-   lsa /4/ : level of safety assurance switch (generally, use 3 or 4):
    -   = 1 truly passively safe plant;
    -   = 2,3 in-between;
    -   = 4 like current fission plant
-   moneyint : interest portion of capital cost (M\$)
-   output\_costs /1/ : switch for costs output:
    -   = 0 do not write cost-related outputs to file;
    -   = 1 write cost-related outputs to file
-   ratecdol /0.0435/ : effective cost of money in constant dollars
-   step\_con /0.15/ : Contingency Percentage
-   step\_ref(68) /\.../ : Reference values for cost model 2
-   tlife /30.0/ : plant life (years)
-   ucad /180.0/ FIX : unit cost for administration buildings (M\$/m3)
-   ucaf /1.5e6/ FIX : unit cost for aux facility power equipment (\$)
-   ucahts /31.0/ FIX : unit cost for aux heat transport equipment
    (\$/W\*\*exphts)
-   ucap /17.0/ FIX : unit cost of auxiliary transformer (\$/kVA)
-   ucblbe /260.0/ : unit cost for blanket beryllium (\$/kg)
-   ucblbreed /875.0/ : unit cost for breeder material (\$/kg)
    (blktmodel\>0)
-   ucblli /875.0/ : unit cost for blanket lithium (\$/kg) (30% Li6)
-   ucblli2o /600.0/ : unit cost for blanket Li\_2O (\$/kg)
-   ucbllipb /10.3/ : unit cost for blanket Li-Pb (\$/kg) (30% Li6)
-   ucblss /90.0/ : unit cost for blanket stainless steel (\$/kg)
-   ucblvd /200.0/ : unit cost for blanket vanadium (\$/kg)
-   ucbpmp /2.925e5/ FIX : vacuum system backing pump cost (\$)
-   ucbus /0.123/ : cost of aluminium bus for TF coil (\$/A-m)
-   uccase /50.0/ : cost of superconductor case (\$/kg)
-   ucco /350.0/ FIX : unit cost for control buildings (M\$/m3)
-   uccpcl1 /250.0/ : cost of high strength tapered copper (\$/kg)
-   uccpclb /150.0/ : cost of TF outboard leg plate coils (\$/kg)
-   uccpmp /3.9e5/ FIX : vacuum system cryopump cost (\$)
-   uccr /460.0/ FIX : unit cost for cryogenic building (M\$/vol)
-   uccry /9.3e4/ : heat transport system cryoplant costs
    (\$/W\*\*expcry)
-   uccryo /32.0/ : unit cost for vacuum vessel (\$/kg)
-   uccu /75.0/ : unit cost for copper in superconducting cable (\$/kg)
-   ucdgen /1.7e6/ FIX : cost per 8 MW diesel generator (\$)
-   ucdiv /2.8e5/ : cost of divertor blade (\$)
-   ucdtc /13.0/ FIX : detritiation, air cleanup cost (\$/10000m3/hr)
-   ucduct /4.225e4/ FIX : vacuum system duct cost (\$/m)
-   ucech /3.0/ : ECH system cost (\$/W)
-   ucel /380.0/ FIX : unit cost for electrical equipment building
    (M\$/m3)
-   uces1 /3.2e4/ FIX : MGF (motor-generator flywheel) cost factor
    (\$/MVA\*\*0.8)
-   uces2 /8.8e3/ FIX : MGF (motor-generator flywheel) cost factor
    (\$/MJ\*\*0.8)
-   ucf1 /2.23e7/ : cost of fuelling system (\$)
-   ucfnc /35.0/ : outer PF coil fence support cost (\$/kg)
-   ucfpr /4.4e7/ FIX : cost of 60g/day tritium processing unit (\$)
-   ucfuel /3.45/ : unit cost of D-T fuel (M\$/year/1200MW)
-   ucfwa /6.0e4/ FIX : first wall armour cost (\$/m2)
-   ucfwps /1.0e7/ FIX : first wall passive stabiliser cost (\$)
-   ucfws /5.3e4/ FIX : first wall structure cost (\$/m2)
-   ucgss /35.0/ FIX : cost of reactor structure (\$/kg)
-   uche3 /1.0e6/ : cost of helium-3 (\$/kg)
-   uchrs /87.9e6/ : cost of heat rejection system (\$)
-   uchts(2) /15.3,19.1/ : cost of heat transport system equipment per
    loop (\$/W); dependent on coolant type (coolwh)
-   uciac /1.5e8/ : cost of instrumentation, control & diagnostics (\$)
-   ucich /3.0/ : ICH system cost (\$/W)
-   ucint /35.0/ FIX : superconductor intercoil structure cost (\$/kg)
-   uclh /3.3/ : lower hybrid system cost (\$/W)
-   uclv /16.0/ FIX : low voltage system cost (\$/kVA)
-   ucmb /260.0/ FIX: unit cost for reactor maintenance building
    (M\$/m3)
-   ucme /1.25e8/ : cost of maintenance equipment (\$)
-   ucmisc /2.5e7/ : miscellaneous plant allowance (\$)
-   ucnbi /3.3/ : NBI system cost (\$/W)
-   ucnbv /1000.0/ FIX : cost of nuclear building ventilation (\$/m3)
-   ucoam(4) /68.8,68.8,68.8,74.4/ : annual cost of operation and
    maintenance (M\$/year/1200MW\*\*0.5)
-   ucpens /32.0/ : penetration shield cost (\$/kg)
-   ucpfb /210.0/ : cost of PF coil buses (\$/kA-m)
-   ucpfbk /1.66e4/ : cost of PF coil DC breakers (\$/MVA\*\*0.7)
-   ucpfbs /4.9e3/ : cost of PF burn power supplies (\$/kW\*\*0.7)
-   ucpfcb /7.5e4/ : cost of PF coil AC breakers (\$/circuit)
-   ucpfdr1 /150.0/ : cost factor for dump resistors (\$/MJ)
-   ucpfic /1.0e4/ : cost of PF instrumentation and control (\$/channel)
-   ucpfps /3.5e4/ : cost of PF coil pulsed power supplies (\$/MVA)
-   ucphx /15.0/ FIX : primary heat transport cost (\$/W\*\*exphts)
-   ucpp /48.0/ FIX : cost of primary power transformers (\$/kVA\*\*0.9)
-   ucrb /400.0/ : cost of reactor building (M\$/m3)
-   ucsc(6) /600.0,600.0,300.0,600.0/ : cost of superconductor (\$/kg)
-   ucsh /115.0/ FIX : cost of shops and warehouses (M\$/m3)
-   ucshld /32.0/ : cost of shield structural steel (\$/kg)
-   ucswyd /1.84e7/ FIX : switchyard equipment costs (\$)
-   uctfbr /1.22/ : cost of TF coil breakers (\$/W\*\*0.7)
-   uctfbus /100.0/ : cost of TF coil bus (\$/kg)
-   uctfdr /1.75e-4/ FIX : cost of TF coil dump resistors (\$/J)
-   uctfgr /5000.0/ FIX : additional cost of TF coil dump resistors
    (\$/coil)
-   uctfic /1.0e4/ FIX : cost of TF coil instrumentation and control
    (\$/coil/30)
-   uctfps /24.0/ : cost of TF coil power supplies (\$/W\*\*0.7)
-   uctfsw /1.0/ : cost of TF coil slow dump switches (\$/A)
-   uctpmp /1.105e5/ FIX : cost of turbomolecular pump (\$)
-   uctr /370.0/ FIX : cost of tritium building (\$/m3)
-   ucturb(2) /230.0e6, 245.0e6/: cost of turbine plant equipment (\$)
    (dependent on coolant type coolwh)
-   ucvalv /3.9e5/ FIX : vacuum system valve cost (\$)
-   ucvdsh /26.0/ FIX : vacuum duct shield cost (\$/kg)
-   ucviac /1.3e6/ FIX : vacuum system instrumentation and control cost
    (\$)
-   ucwindpf /465.0/ : cost of PF coil superconductor windings (\$/m)
-   ucwindtf /480.0/ : cost of TF coil superconductor windings (\$/m)
-   ucws /460.0/ FIX : cost of active assembly shop (\$/m3)
-   ucwst(4) /0.0,3.94,5.91,7.88/ : cost of waste disposal
    (M\$/y/1200MW)

### [constraint\_variables](constraint_variables.html)

-   auxmin /0.1/ : minimum auxiliary power (MW) (constraint equation 40)
-   betpmx /0.19/ : maximum poloidal beta (constraint equation 48)
-   bigqmin /10.0/ : minimum fusion gain Q (constraint equation 28)
-   bmxlim /12.0/ : maximum peak toroidal field (T) (constraint
    equation 25)
-   fauxmn /1.0/ : f-value for minimum auxiliary power (constraint
    equation 40, iteration variable 64)
-   fbeta /1.0/ : f-value for epsilon beta-poloidal (constraint equation
    6, iteration variable 8)
-   fbetap /1.0/ : f-value for poloidal beta (constraint equation 48,
    iteration variable 79)
-   fbetatry /1.0/ : f-value for beta limit (constraint equation 24,
    iteration variable 36)
-   fcpttf /1.0/ : f-value for TF coil current per turn upper limit
    (constraint equation 77, iteration variable 146)
-   fcwr /1.0/ : f-value for conducting wall radius / rminor limit
    (constraint equation 23, iteration variable 104)
-   fdene /1.0/ : f-value for density limit (constraint equation 5,
    iteration variable 9) (invalid if ipedestal = 3)
-   fdivcol /1.0/ : f-value for divertor collisionality (constraint
    equation 22, iteration variable 34)
-   fdtmp /1.0/ : f-value for first wall coolant temperature rise
    (constraint equation 38, iteration variable 62)
-   fflutf /1.0/ : f-value for neutron fluence on TF coil (constraint
    equation 53, iteration variable 92)
-   ffuspow /1.0/ : f-value for maximum fusion power (constraint
    equation 9, iteration variable 26)
-   fgamcd /1.0/ : f-value for current drive gamma (constraint equation
    37, iteration variable 40)
-   fhldiv /1.0/ : f-value for divertor heat load (constraint equation
    18, iteration variable 27)
-   fiooic /0.5/ : f-value for TF coil operating current / critical
    current ratio (constraint equation 33, iteration variable 50)
-   fipir /1.0/ : f-value for Ip/Irod limit (constraint equation 46,
    iteration variable 72)
-   fjohc /1.0/ : f-value for central solenoid current at end-of-flattop
    (constraint equation 26, iteration variable 38)
-   fjohc0 /1.0/ : f-value for central solenoid current at beginning of
    pulse (constraint equation 27, iteration variable 39)
-   fjprot /1.0/ : f-value for TF coil winding pack current density
    (constraint equation 35, iteration variable 53)
-   flhthresh /1.0/ : f-value for L-H power threshold (constraint
    equation 15, iteration variable 103)
-   fmva /1.0/ : f-value for maximum MVA (constraint equation 19,
    iteration variable 30)
-   fnbshinef /1.0/ : f-value for maximum neutral beam shine-through
    fraction (constraint equation 59, iteration variable 105)
-   fnesep /1.0/ : f-value for Eich critical separatrix density
    (constraint equation 76, iteration variable 144)
-   foh\_stress /1.0/ : f-value for Tresca stress in Central Solenoid
    (constraint equation 72, iteration variable 123)
-   fpeakb /1.0/ : f-value for maximum toroidal field (constraint
    equation 25, iteration variable 35)
-   fpinj /1.0/ : f-value for injection power (constraint equation 30,
    iteration variable 46)
-   fpnetel /1.0/ : f-value for net electric power (constraint equation
    16, iteration variable 25)
-   fportsz /1.0/ : f-value for neutral beam tangency radius limit
    (constraint equation 20, iteration variable 33)
-   fpsepbqar /1.0/ : f-value for maximum Psep\*Bt/qAR limit (constraint
    equation 68, iteration variable 117)
-   fpsepr /1.0/ : f-value for maximum Psep/R limit (constraint equation
    56, iteration variable 97)
-   fptemp /1.0/ : f-value for peak centrepost temperature (constraint
    equation 44, iteration variable 68)
-   fptfnuc /1.0/ : f-value for maximum TF coil nuclear heating
    (constraint equation 54, iteration variable 95)
-   fq /1.0/ : f-value for edge safety factor (constraint equation 45,
    iteration variable 71)
-   fqval /1.0/ : f-value for Q (constraint equation 28, iteration
    variable 45)
-   fradpwr /1.0/ : f-value for core radiation power limit (constraint
    equation 17, iteration variable 28)
-   fradwall /1.0/ : f-value for upper limit on radiation wall load
    (constr. equ. 67, iteration variable 116 )
-   freinke /1.0/ : f-value for Reinke detachment criterion (constr.
    equ. 78, iteration variable 147)
-   frminor /1.0/ : f-value for minor radius limit (constraint equation
    21, iteration variable 32)
-   fstrcase /1.0/ : f-value for TF coil case stress (constraint
    equation 31, iteration variable 48)
-   fstrcond /1.0/ : f-value for TF coil conduit stress (constraint
    equation 32, iteration variable 49)
-   ftaucq /1.0/ : f-value for calculated minimum TF quench time
    (constraint equation 65, iteration variable 113)
-   ftbr /1.0/ : f-value for minimum tritium breeding ratio (constraint
    equation 52, iteration variable 89)
-   ftburn /1.0/ : f-value for minimum burn time (constraint equation
    13, iteration variable 21)
-   ftcycl /1.0/ : f-value for cycle time (constraint equation 42,
    iteration variable 67)
-   ftmargoh /1.0/ : f-value for central solenoid temperature margin
    (constraint equation 60, iteration variable 106)
-   ftmargtf /1.0/ : f-value for TF coil temperature margin (constraint
    equation 36, iteration variable 54)
-   ftohs /1.0/ : f-value for plasma current ramp-up time (constraint
    equation 41, iteration variable 66)
-   ftpeak /1.0/ : f-value for first wall peak temperature (constraint
    equation 39, iteration variable 63)
-   fvdump /1.0/ : f-value for dump voltage (constraint equation 34,
    iteration variable 51)
-   fvs /1.0/ : f-value for flux-swing (V-s) requirement (STEADY STATE)
    (constraint equation 12, iteration variable 15)
-   fvvhe /1.0/ : f-value for vacuum vessel He concentration limit
    (iblanket = 2) (constraint equation 55, iteration variable 96)
-   fwalld /1.0/ : f-value for maximum wall load (constraint equation 8,
    iteration variable 14)
-   fzeffmax /1.0/ : f-value for maximum zeff (constraint equation 64,
    iteration variable 112)
-   gammax /2.0/ : maximum current drive gamma (constraint equation 37)
-   maxradwallload /1.0/ : Maximum permitted radiation wall load
    (MW/m\^2) (constraint equation 67)
-   mvalim /40.0/ : maximum MVA limit (constraint equation 19)
-   nbshinefmax /1.0e-3/ : maximum neutral beam shine-through fraction
    (constraint equation 59)
-   nflutfmax /1.0e23/ : max fast neutron fluence on TF coil (n/m2)
    (blktmodel\>0) (constraint equation 53)
-   pdivtlim /150.0/ : Minimum pdivt \[MW\] (constraint equation 80)
-   peakfactrad /3.33/ : peaking factor for radiation wall load
    (constraint equation 67)
-   peakradwallload : Peak radiation wall load (MW/m\^2) (constraint
    equation 67)
-   pnetelin /1000.0/ : required net electric power (MW) (constraint
    equation 16)
-   powfmax /1500.0/ : maximum fusion power (MW) (constraint equation 9)
-   psepbqarmax /9.5/ : maximum ratio of Psep\*Bt/qAR (MWT/m)
    (constraint equation 68)
-   pseprmax /25.0/ : maximum ratio of power crossing the separatrix to
    plasma major radius (Psep/R) (MW/m) (constraint equation 56)
-   ptfnucmax /1.0e-3/ : maximum nuclear heating in TF coil (MW/m3)
    (constraint equation 54)
-   tbrmin /1.1/ : minimum tritium breeding ratio (constraint
    equation 52)
-   tbrnmn /1.0/ : minimum burn time (s) (KE - no longer itv., see
    issue 706)
-   tcycmn : minimum cycle time (s) (constraint equation 42)
-   tohsmn : minimum plasma current ramp-up time (s) (constraint
    equation 41)
-   vvhealw /1.0/ : allowed maximum helium concentration in vacuum
    vessel at end of plant life (appm) (iblanket =2) (constraint
    equation 55)
-   walalw /1.0/ : allowable wall-load (MW/m2) (constraint equation 8)
-   taulimit /5.0/ : Lower limit on taup/taueff the ratio of alpha
    particle to energy confinement times (constraint equation 62)
-   ftaulimit /1.0/ : f-value for lower limit on taup/taueff the ratio
    of alpha particle to energy confinement times (constraint equation
    62, iteration variable 110)
-   fniterpump /1.0/ : f-value for constraint that number of pumps \<
    tfno (constraint equation 63, iteration variable 111)
-   zeffmax /3.6/ : maximum value for Zeff (constraint equation 64)
-   fpoloidalpower /1.0/ : f-value for constraint on rate of change of
    energy in poloidal field (constraint equation 66, iteration
    variable 115)
-   fpsep /1.0/ : f-value to ensure separatrix power is less than value
    from Kallenbach divertor (Not required as constraint 69 is an
    equality)
-   fcqt /1.0/ : f-value: TF coil quench temparature remains below
    tmax\_croco (constraint equation 74, iteration variable 141)

### [stellarator\_variables](stellarator_variables.html)

-   istell /0/ : switch for stellarator option (set via `device.dat`):
    -   = 0 use tokamak model;
    -   = 1 use stellarator model
-   bmn /0.001/ : relative radial field perturbation
-   f\_asym /1.0/ : divertor heat load peaking factor
-   f\_rad /0.85/ : radiated power fraction in SOL
-   f\_w /0.5/ : island size fraction factor
-   fdivwet /0.3333/ : wetted fraction of the divertor area
-   flpitch /0.001/ : field line pitch (rad)
-   hportamax : maximum available area for horizontal ports (m2)
-   hportpmax : maximum available poloidal extent for horizontal
    ports (m)
-   hporttmax : maximum available toroidal extent for horizontal
    ports (m)
-   iotabar /1.0/ : rotational transform (reciprocal of tokamak q) for
    stellarator confinement time scaling laws
-   isthtr /3/ : switch for stellarator auxiliary heating method:
    -   = 1 electron cyclotron resonance heating;
    -   = 2 lower hybrid heating;
    -   = 3 neutral beam injection
-   m\_res /5/ : poloidal resonance number
-   n\_res /5/ : toroidal resonance number
-   shear /0.5/ : magnetic shear, derivative of iotabar
-   vmec\_info\_file /vmec\_info.dat/ : file containing general VMEC
    settings
-   vmec\_rmn\_file /vmec\_Rmn.dat/ : file containing plasma boundary
    R(m,n) Fourier components
-   vmec\_zmn\_file /vmec\_Zmn.dat/ : file containing plasma boundary
    Z(m,n) Fourier components
-   vportamax : maximum available area for vertical ports (m2)
-   vportpmax : maximum available poloidal extent for vertical ports (m)
-   vporttmax : maximum available toroidal extent for vertical ports (m)

### [ife\_variables](ife_variables.html)

-   Default IFE builds and material volumes are those for the SOMBRERO
    device. The 2-dimensional arrays have indices (region, material),
    where \'region\' is the region and maxmat is the \'material\'
    -   \'region\' = 1 radially outside chamber
    -   = 2 above chamber
    -   = 3 below chamber
-   maxmat /7/ FIX : total number of materials in IFE device. Material
    numbers are as follows:
    -   = 0 void;
    -   = 1 steel;
    -   = 2 carbon cloth;
    -   = 3 FLiBe;
    -   = 4 lithium oxide Li2O;
    -   = 5 concrete;
    -   = 6 helium;
    -   = 7 xenon
-   bldr /1.0/ : radial thickness of IFE blanket (m)
-   bldzl /4.0/ : vertical thickness of IFE blanket below chamber (m)
-   bldzu /4.0/ : vertical thickness of IFE blanket above chamber (m)
-   blmatf(3,0:maxmat) /\.../ : IFE blanket material fractions
-   blmatm(3,0:maxmat) : IFE blanket material masses (kg)
-   blmatv(3,0:maxmat) : IFE blanket material volumes (m3)
-   blvol(3) : IFE blanket volume (m3)
-   cdriv0 /154.3/ : IFE generic/laser driver cost at edrive=0 (M\$)
-   cdriv1 /163.2/ : IFE low energy heavy ion beam driver cost
    extrapolated to edrive=0 (M\$)
-   cdriv2 /244.9/ : IFE high energy heavy ion beam driver cost
    extrapolated to edrive=0 (M\$)
-   chdzl /9.0/ : vertical thickness of IFE chamber below centre (m)
-   chdzu /9.0/ : vertical thickness of IFE chamber above centre (m)
-   chmatf(0:maxmat) : IFE chamber material fractions
-   chmatm(0:maxmat) : IFE chamber material masses (kg)
-   chmatv(0:maxmat) : IFE chamber material volumes (m3)
-   chrad /6.5/ : radius of IFE chamber (m) (iteration variable 84)
-   chvol : IFE chamber volume (m3)
-   dcdrv0 /111.4/ : IFE generic/laser driver cost gradient (M\$/MJ)
-   dcdrv1 /78.0/ : HIB driver cost gradient at low energy (M\$/MJ)
-   dcdrv2 /59.9/ : HIB driver cost gradient at high energy (M\$/MJ)
-   drveff /0.28/ : IFE driver wall plug to target efficiency (ifedrv=0)
    (iteration variable 82)
-   edrive /5.0D6/ : IFE driver energy (J) (iteration variable 81)
-   etadrv : IFE driver wall plug to target efficiency
-   etave(10) : IFE driver efficiency vs driver energy (ifedrv=-1)
-   fauxbop /0.06/ : fraction of gross electric power to
    balance-of-plant (IFE)
-   fbreed /0.51/ : fraction of breeder external to device core
-   fburn /0.3333/ : IFE burn fraction (fraction of tritium
    fused/target)
-   flirad /0.78/ : radius of FLiBe inlet (m) (ifetyp=3)
-   frrmax /1.0/ : f-value for maximum IFE repetition rate (constraint
    equation 50, iteration variable 86)
-   fwdr /0.01/ : radial thickness of IFE first wall (m)
-   fwdzl /0.01/ : vertical thickness of IFE first wall below
    chamber (m)
-   fwdzu /0.01/ : vertical thickness of IFE first wall above
    chamber (m)
-   fwmatf(3,0:maxmat) /\.../ : IFE first wall material fractions
-   fwmatm(3,0:maxmat) : IFE first wall material masses (kg)
-   fwmatv(3,0:maxmat) : IFE first wall material volumes (kg)
-   fwvol(3) : IFE first wall volume (m3)
-   gain : IFE target gain
-   gainve(10) /\.../ : IFE target gain vs driver energy (ifedrv=-1)
-   htpmw\_ife /0.0/ : IFE heat transport system electrical pump power
    (MW)
-   ife /0/ : switch for IFE option (set via `device.dat`):
    -   = 0 use tokamak, RFP or stellarator model;
    -   = 1 use IFE model
-   ifedrv /2/ : switch for type of IFE driver:
    -   = -1 use gainve, etave for gain and driver efficiency;
    -   = 0 use tgain, drveff for gain and driver efficiency;
    -   = 1 use laser driver based on SOMBRERO design;
    -   = 2 use heavy ion beam driver based on OSIRIS
-   ifetyp /0/ : switch for type of IFE device build:
    -   = 0 generic (cylindrical) build;
    -   = 1 OSIRIS-like build;
    -   = 2 SOMBRERO-like build;
    -   = 3 HYLIFE-II-like build
-   mcdriv /1.0/ : IFE driver cost multiplier
-   mflibe : total mass of FLiBe (kg)
-   pdrive /23.0D6/ : IFE driver power reaching target (W) (iteration
    variable 85)
-   pifecr /10.0/ : IFE cryogenic power requirements (MW)
-   ptargf /2.0/ : IFE target factory power at 6 Hz repetition rate (MW)
-   r1 : IFE device radial build (m)
-   r2 : IFE device radial build (m)
-   r3 : IFE device radial build (m)
-   r4 : IFE device radial build (m)
-   r5 : IFE device radial build (m)
-   r6 : IFE device radial build (m)
-   r7 : IFE device radial build (m)
-   reprat : IFE driver repetition rate (Hz)
-   rrmax /20.0/ : maximum IFE repetition rate (Hz)
-   shdr /1.7/ : radial thickness of IFE shield (m)
-   shdzl /5.0/ : vertical thickness of IFE shield below chamber (m)
-   shdzu /5.0/ : vertical thickness of IFE shield above chamber (m)
-   shmatf(3,0:maxmat) /\.../ : IFE shield material fractions
-   shmatm(3,0:maxmat) : IFE shield material masses (kg)
-   shmatv(3,0:maxmat) : IFE shield material volumes (kg)
-   shvol(3) : IFE shield volume (m3)
-   sombdr /2.7/ : radius of cylindrical blanket section below chamber
    (ifetyp=2)
-   somtdr /2.7/ : radius of cylindrical blanket section above chamber
    (ifetyp=2)
-   tdspmw /0.01/ FIX : IFE target delivery system power (MW)
-   tfacmw : IFE target factory power (MW)
-   tgain /85.0/ : IFE target gain (if ifedrv = 0) (iteration
    variable 83)
-   uccarb /50.0/ : cost of carbon cloth (\$/kg)
-   ucconc /0.1/ : cost of concrete (\$/kg)
-   ucflib /84.0/ : cost of FLiBe (\$/kg)
-   uctarg /0.3/ : cost of IFE target (\$/target)
-   v1dr /0.0/ : radial thickness of IFE void between first wall and
    blanket (m)
-   v1dzl /0.0/ : vertical thickness of IFE void 1 below chamber (m)
-   v1dzu /0.0/ : vertical thickness of IFE void 1 above chamber (m)
-   v1matf(3,0:maxmat) /\.../ : IFE void 1 material fractions
-   v1matm(3,0:maxmat) : IFE void 1 material masses (kg)
-   v1matv(3,0:maxmat) : IFE void 1 material volumes (kg)
-   v1vol(3) : IFE void 1 volume (m3)
-   v2dr /2.0/ : radial thickness of IFE void between blanket and
    shield (m)
-   v2dzl /7.0/ : vertical thickness of IFE void 2 below chamber (m)
-   v2dzu /7.0/ : vertical thickness of IFE void 2 above chamber (m)
-   v2matf(3,0:maxmat) /\.../ : IFE void 2 material fractions
-   v2matm(3,0:maxmat) : IFE void 2 material masses (kg)
-   v2matv(3,0:maxmat) : IFE void 2 material volumes (kg)
-   v2vol(3) : IFE void 2 volume (m3)
-   v3dr /43.3/ : radial thickness of IFE void outside shield (m)
-   v3dzl /30.0/ : vertical thickness of IFE void 3 below chamber (m)
-   v3dzu /20.0/ : vertical thickness of IFE void 3 above chamber (m)
-   v3matf(3,0:maxmat) /\.../ : IFE void 3 material fractions
-   v3matm(3,0:maxmat) : IFE void 3 material masses (kg)
-   v3matv(3,0:maxmat) : IFE void 3 material volumes (kg)
-   v3vol(3) : IFE void 3 volume (m3)
-   zl1 : IFE vertical build below centre (m)
-   zl2 : IFE vertical build below centre (m)
-   zl3 : IFE vertical build below centre (m)
-   zl4 : IFE vertical build below centre (m)
-   zl5 : IFE vertical build below centre (m)
-   zl6 : IFE vertical build below centre (m)
-   zl7 : IFE vertical build below centre (m)
-   zu1 : IFE vertical build above centre (m)
-   zu2 : IFE vertical build above centre (m)
-   zu3 : IFE vertical build above centre (m)
-   zu4 : IFE vertical build above centre (m)
-   zu5 : IFE vertical build above centre (m)
-   zu6 : IFE vertical build above centre (m)
-   zu7 : IFE vertical build above centre (m)

### [pulse\_variables](pulse_variables.html)

-   bctmp /320.0/ : first wall bulk coolant temperature (C)
-   bfw : outer radius of each first wall structural tube (m) (0.5 \*
    average of fwith and fwoth)
-   dtstor /300.0/ : maximum allowable temperature change in stainless
    steel thermal storage block (K) (istore=3)
-   istore /1/ : switch for thermal storage method:
    -   = 1 option 1 of Electrowatt report, AEA FUS 205;
    -   = 2 option 2 of Electrowatt report, AEA FUS 205;
    -   = 3 stainless steel block
-   itcycl /1/ : switch for first wall axial stress model:
    -   = 1 total axial constraint, no bending;
    -   = 2 no axial constraint, no bending;
    -   = 3 no axial constraint, bending
-   lpulse /0/ : switch for reactor model:
    -   = 0 continuous operation;
    -   = 1 pulsed operation

### [startup\_variables](startup_variables.html)

-   ftaue : factor in energy confinement time formula
-   gtaue : offset term in energy confinement time scaling
-   nign : electron density at ignition (start-up) (/m3)
-   ptaue : exponent for density term in energy confinement time formula
-   qtaue : exponent for temperature term in energy confinement time
    formula
-   rtaue : exponent for power term in energy confinement time formula
-   tign : electron temperature at ignition (start-up) (keV)

### [fispact\_variables](fispact_variables.html)

-   Fispact arrays with 3 elements contain the results at the following
    times: (1) - at end of component life (2) - after 3 months cooling
    time (3) - 100 years after end of plant life
-   bliact(3) : inboard blanket total activity (Bq)
-   bligdr(3) : inboard blanket total gamma dose rate (Sv/hr)
-   blihkw(3) : inboard blanket total heat output (kW)
-   bliizp : inboard blanket integrated zone power / neutron
-   blimzp : inboard blanket mean zone power density / neutron
-   bloact(3) : outboard blanket total activity (Bq)
-   blogdr(3) : outboard blanket total gamma dose rate (Sv/hr)
-   blohkw(3) : outboard blanket total heat output (kW)
-   bloizp : outboard blanket integrated zone power / neutron
-   blomzp : outboard blanket mean zone power density / neutron
-   fwiact(3) : inboard first wall total activity (Bq)
-   fwigdr(3) : inboard first wall total gamma dose rate (Sv/hr)
-   fwihkw(3) : inboard first wall total heat output (kW)
-   fwiizp : inboard first wall integrated zone power / neutron
-   fwimzp : inboard first wall mean zone power density/neutron
-   fwoact(3) : outboard first wall total activity (Bq)
-   fwogdr(3) : outboard first wall total gamma dose rate (Sv/hr)
-   fwohkw(3) : outboard first wall total heat output (kW)
-   fwoizp : outboard first wall integrated zone power / neutron
-   fwomzp : outboard first wall mean zone power density/neutron
-   fwtemp : outboard first wall temperature after a LOCA (K)

### [rebco\_variables](rebco_variables.html)

-   rebco\_thickness /1.0e-6/ : thickness of REBCO layer in tape (m)
    (iteration variable 138)
-   copper\_thick /100e-6/ : thickness of copper layer in tape (m)
    (iteration variable 139)
-   hastelloy\_thickness /50/e-6 : thickness of Hastelloy layer in
    tape (m)
-   tape\_width : Mean width of tape (m)
-   croco\_od : Outer diameter of CroCo strand (m)
-   croco\_id : Inner diameter of CroCo copper tube (m)
-   croco\_thick /2.5e-3/ : Thickness of CroCo copper tube (m)
    (iteration variable 149)
-   copper\_bar /1.0/ : area of central copper bar, as a fraction of the
    cable space
-   copper\_rrr /100.0/ : residual resistivity ratio copper in TF
    superconducting cable
-   cable\_helium\_fraction /0.284/ : Helium area as a fraction of the
    cable space.
-   copperA\_m2\_max /1e8/ : Maximum TF coil current / copper area
    (A/m2)
-   f\_copperA\_m2 /1/ : f-value for constraint 75: TF coil current /
    copper area \< copperA\_m2\_max

### [resistive\_material](resistive_material.html)

### [reinke\_variables](reinke_variables.html)

-   impvardiv /9/ : index of impurity to be iterated for Reinke divertor
    detachment criterion
-   lhat /4.33/ : connection length factor L\|\| = lhat qstar R for
    Reinke criterion, default value from Post et al. 1995 J. Nucl. Mat.
    220-2 1014
-   fzmin : Minimum impurity fraction necessary for detachment This is
    the impurity at the SOL/Div
-   fzactual : Actual impurity fraction of divertor impurity (impvardiv)
    in the SoL (taking impurity\_enrichment into account) (iteration
    variable 148)
-   reinke\_mode /0/ : Switch for Reinke criterion H/I mode
-   = 0 H-mode;
-   = 1 I-mode;

### [numerics](numerics.html)

-   ipnvars FIX : total number of variables available for iteration
-   ipeqns FIX : number of constraint equations available
-   ipnfoms FIX : number of available figures of merit
-   ioptimz /1/ : code operation switch:
    -   = -1 for no optimisation, HYBRD only;
    -   = 0 for HYBRD and VMCON (not recommended);
    -   = 1 for optimisation, VMCON only
-   minmax /7/ : switch for figure-of-merit (see lablmm for
    descriptions) negative =\> maximise, positive =\> minimise
-   lablmm(ipnfoms) : labels describing figures of merit:
    -   ( 1) major radius
    -   ( 2) not used
    -   ( 3) neutron wall load
    -   ( 4) P\_tf + P\_pf
    -   ( 5) fusion gain Q
    -   ( 6) cost of electricity
    -   ( 7) capital cost (direct cost if ireactor=0, constructed cost
        otherwise)
    -   ( 8) aspect ratio
    -   ( 9) divertor heat load
    -   \(10) toroidal field
    -   \(11) total injected power
    -   \(12) hydrogen plant capital cost OBSOLETE
    -   \(13) hydrogen production rate OBSOLETE
    -   \(14) pulse length
    -   \(15) plant availability factor (N.B. requires iavail=1 to be set)
    -   \(16) linear combination of major radius (minimised) and pulse length
        (maximised) note: FoM should be minimised only!
    -   \(17) net electrical output
    -   \(18) Null Figure of Merit
    -   \(19) linear combination of big Q and pulse length (maximised) note: FoM
        should be minimised only!
-   ncalls : number of function calls during solution
-   neqns /0/ : number of equality constraints to be satisfied
-   nfev1 : number of calls to FCNHYB (HYBRD function caller) made
-   nfev2 : number of calls to FCNVMC1 (VMCON function caller) made
-   nineqns /0/ : number of inequality constraints VMCON must satisfy
    (leave at zero for now)
-   nvar /16/ : number of iteration variables to use
-   nviter : number of VMCON iterations performed
-   icc(ipeqns) /0/ : array defining which constraint equations to
    activate (see lablcc for descriptions)
-   active\_constraints(ipeqns) : Logical array showing which
    constraints are active
-   lablcc(ipeqns) : labels describing constraint equations
    (corresponding itvs)
    -   ( 1) Beta (consistency equation) (itv 5)
    -   ( 2) Global power balance (consistency equation) (itv
        10,1,2,3,4,6,11)
    -   ( 3) Ion power balance DEPRECATED (itv 10,1,2,3,4,6,11)
    -   ( 4) Electron power balance DEPRECATED (itv 10,1,2,3,4,6,11)
    -   ( 5) Density upper limit (itv 9,1,2,3,4,5,6)
    -   ( 6) (Epsilon x beta poloidal) upper limit (itv 8,1,2,3,4,6)
    -   ( 7) Beam ion density (NBI) (consistency equation) (itv 7)
    -   ( 8) Neutron wall load upper limit (itv 14,1,2,3,4,6)
    -   ( 9) Fusion power upper limit (itv 26,1,2,3,4,6)
    -   \(10) Toroidal field 1/R (consistency equation) (itv 12,1,2,3,13 )
    -   \(11) Radial build (consistency equation) (itv 3,1,13,16,29,42,61)
    -   \(12) Volt second lower limit (STEADY STATE) (itv 15,1,2,3)
    -   \(13) Burn time lower limit (PULSE) (itv 21,1,16,17,29,42,44,61)
    -   \(14) Neutral beam decay lengths to plasma centre (NBI) (consistency
        equation)
    -   \(15) LH power threshold limit (itv 103)
    -   \(16) Net electric power lower limit (itv 25,1,2,3)
    -   \(17) Radiation fraction upper limit (itv 28)
    -   \(18) Divertor heat load upper limit (itv 27)
    -   \(19) MVA upper limit (itv 30)
    -   \(20) Neutral beam tangency radius upper limit (NBI) (itv 33,31,3,13)
    -   \(21) Plasma minor radius lower limit (itv 32)
    -   \(22) Divertor collisionality upper limit (itv 34,43)
    -   \(23) Conducting shell to plasma minor radius ratio upper limit (itv
        104,1,74)
    -   \(24) Beta upper limit (itv 36,1,2,3,4,6,18)
    -   \(25) Peak toroidal field upper limit (itv 35,3,13,29)
    -   \(26) Central solenoid EOF current density upper limit (ipfres=0) (itv
        38,37,41,12)
    -   \(27) Central solenoid BOP current density upper limit (ipfres=0) (itv
        39,37,41,12)
    -   \(28) Fusion gain Q lower limit (itv 45,47,40)
    -   \(29) Inboard radial build consistency (itv 3,1,13,16,29,42,61)
    -   \(30) Injection power upper limit (itv 46,47,11)
    -   \(31) TF coil case stress upper limit (SCTF) (itv 48,56,57,58,59,60,24)
    -   \(32) TF coil conduit stress upper limit (SCTF) (itv
        49,56,57,58,59,60,24)
    -   \(33) I\_op / I\_critical (TF coil) (SCTF) (itv 50,56,57,58,59,60,24)
    -   \(34) Dump voltage upper limit (SCTF) (itv 51,52,56,57,58,59,60,24)
    -   \(35) J\_winding pack/J\_protection upper limit (SCTF) (itv
        53,56,57,58,59,60,24)
    -   \(36) TF coil temperature margin lower limit (SCTF) (itv
        54,55,56,57,58,59,60,24)
    -   \(37) Current drive gamma upper limit (itv 40,47)
    -   \(38) First wall coolant temperature rise upper limit (itv 62)
    -   \(39) First wall peak temperature upper limit (itv 63)
    -   \(40) Start-up injection power lower limit (PULSE) (itv 64)
    -   \(41) Plasma current ramp-up time lower limit (PULSE) (itv 66,65)
    -   \(42) Cycle time lower limit (PULSE) (itv 17,67,65)
    -   \(43) Average centrepost temperature (TART) (consistency equation) (itv
        13,20,69,70)
    -   \(44) Peak centrepost temperature upper limit (TART) (itv 68,69,70)
    -   \(45) Edge safety factor lower limit (TART) (itv 71,1,2,3)
    -   \(46) Ip/Irod upper limit (TART) (itv 72,2,60)
    -   \(47) NOT USED
    -   \(48) Poloidal beta upper limit (itv 79,2,3,18)
    -   \(49) NOT USED
    -   \(50) IFE repetition rate upper limit (IFE)
    -   \(51) Startup volt-seconds consistency (PULSE) (itv 16,29,3,1)
    -   \(52) Tritium breeding ratio lower limit (itv 89,90,91)
    -   \(53) Neutron fluence on TF coil upper limit (itv 92,93,94)
    -   \(54) Peak TF coil nuclear heating upper limit (itv 95,93,94)
    -   \(55) Vacuum vessel helium concentration upper limit iblanket =2 (itv
        96,93,94)
    -   \(56) Pseparatrix/Rmajor upper limit (itv 97,1,3,102)
    -   \(57) NOT USED
    -   \(58) NOT USED
    -   \(59) Neutral beam shine-through fraction upper limit (NBI) (itv
        105,6,19,4 )
    -   \(60) Central solenoid temperature margin lower limit (SCTF) (itv 106)
    -   \(61) Minimum availability value (itv 107)
    -   \(62) taup/taueff the ratio of particle to energy confinement times
        (itv 110)
    -   \(63) The number of ITER-like vacuum pumps niterpump \< tfno (itv 111)
    -   \(64) Zeff less than or equal to zeffmax (itv 112)
    -   \(65) Dump time set by VV loads (itv 56, 113)
    -   \(66) Limit on rate of change of energy in poloidal field (Use iteration
        variable 65(tohs), 115)
    -   \(67) Simple Radiation Wall load limit (itv 116, 102, 4,6)
    -   \(68) Psep \* Bt / qAR upper limit (itv 117)
    -   \(69) ensure separatrix power = the value from Kallenbach divertor
        (itv 118)
    -   \(70) ensure that teomp = separatrix temperature in the pedestal profile,
        (itv 119 (tesep))
    -   \(71) ensure that neomp = separatrix density (nesep) x neratio
    -   \(72) central solenoid Tresca stress limit (itv 123 foh\_stress)
    -   \(73) Psep \>= Plh + Paux (itv 137 (fplhsep))
    -   \(74) TFC quench \< tmax\_croco (itv 141 (fcqt))
    -   \(75) TFC current/copper area \< Maximum (itv 143 f\_copperA\_m2)
    -   \(76) Eich critical separatrix density
    -   \(77) TF coil current per turn upper limit
    -   \(78) Reinke criterion impurity fraction lower limit (itv 147 freinke)
    -   \(79) Peak CS field upper limit (itv 149 fbmaxcs)
    -   \(80) Divertor power lower limit pdivt (itv 153 fpdivlim)
    -   \(81) Ne(0) \> ne(ped) constraint (itv 154 fne0)
-   ixc(ipnvars) /0/ : array defining which iteration variables to
    activate (see lablxc for descriptions)
-   lablxc(ipnvars) : labels describing iteration variables (NEW:THERE
    ARE NO DEFAULTS):
    -   ( 1) aspect
    -   ( 2) bt
    -   ( 3) rmajor
    -   ( 4) te
    -   ( 5) beta
    -   ( 6) dene
    -   ( 7) rnbeam
    -   ( 8) fbeta (f-value for equation 6)
    -   ( 9) fdene (f-value for equation 5)
    -   \(10) hfact
    -   \(11) pheat
    -   \(12) oacdcp
    -   \(13) tfcth (NOT RECOMMENDED)
    -   \(14) fwalld (f-value for equation 8)
    -   \(15) fvs (f-value for equation 12)
    -   \(16) ohcth
    -   \(17) tdwell
    -   \(18) q
    -   \(19) enbeam
    -   \(20) tcpav
    -   \(21) ftburn (f-value for equation 13)
    -   \(22) NOT USED
    -   \(23) fcoolcp
    -   \(24) NOT USED
    -   \(25) fpnetel (f-value for equation 16)
    -   \(26) ffuspow (f-value for equation 9)
    -   \(27) fhldiv (f-value for equation 18)
    -   \(28) fradpwr (f-value for equation 17), total radiation fraction
    -   \(29) bore
    -   \(30) fmva (f-value for equation 19)
    -   \(31) gapomin
    -   \(32) frminor (f-value for equation 21)
    -   \(33) fportsz (f-value for equation 20)
    -   \(34) fdivcol (f-value for equation 22)
    -   \(35) fpeakb (f-value for equation 25)
    -   \(36) fbetatry (f-value for equation 24)
    -   \(37) coheof
    -   \(38) fjohc (f-value for equation 26)
    -   \(39) fjohc0 (f-value for equation 27)
    -   \(40) fgamcd (f-value for equation 37)
    -   \(41) fcohbop
    -   \(42) gapoh
    -   \(43) NOT USED
    -   \(44) fvsbrnni
    -   \(45) fqval (f-value for equation 28)
    -   \(46) fpinj (f-value for equation 30)
    -   \(47) feffcd
    -   \(48) fstrcase (f-value for equation 31)
    -   \(49) fstrcond (f-value for equation 32)
    -   \(50) fiooic (f-value for equation 33)
    -   \(51) fvdump (f-value for equation 34)
    -   \(52) vdalw
    -   \(53) fjprot (f-value for equation 35)
    -   \(54) ftmargtf (f-value for equation 36)
    -   \(55) obsolete
    -   \(56) tdmptf
    -   \(57) thkcas
    -   \(58) thwcndut
    -   \(59) fcutfsu
    -   \(60) cpttf
    -   \(61) gapds
    -   \(62) fdtmp (f-value for equation 38)
    -   \(63) ftpeak (f-value for equation 39)
    -   \(64) fauxmn (f-value for equation 40)
    -   \(65) tohs
    -   \(66) ftohs (f-value for equation 41)
    -   \(67) ftcycl (f-value for equation 42)
    -   \(68) fptemp (f-value for equation 44)
    -   \(69) rcool
    -   \(70) vcool
    -   \(71) fq (f-value for equation 45)
    -   \(72) fipir (f-value for equation 46)
    -   \(73) scrapli
    -   \(74) scraplo
    -   \(75) tfootfi
    -   \(76) NOT USED
    -   \(77) NOT USED
    -   \(78) NOT USED
    -   \(79) fbetap (f-value for equation 48)
    -   \(80) NOT USED
    -   \(81) edrive
    -   \(82) drveff
    -   \(83) tgain
    -   \(84) chrad
    -   \(85) pdrive
    -   \(86) frrmax (f-value for equation 50)
    -   \(87) NOT USED
    -   \(88) NOT USED
    -   \(89) ftbr (f-value for equation 52)
    -   \(90) blbuith
    -   \(91) blbuoth
    -   \(92) fflutf (f-value for equation 53)
    -   \(93) shldith
    -   \(94) shldoth
    -   \(95) fptfnuc (f-value for equation 54)
    -   \(96) fvvhe (f-value for equation 55)
    -   \(97) fpsepr (f-value for equation 56)
    -   \(98) li6enrich
    -   \(99) NOT USED
    -   \(100) NOT USED
    -   \(101) NOT USED
    -   \(102) fimpvar
    -   \(103) flhthresh (f-value for equation 15)
    -   \(104) fcwr (f-value for equation 23)
    -   \(105) fnbshinef (f-value for equation 59)
    -   \(106) ftmargoh (f-value for equation 60)
    -   \(107) favail (f-value for equation 61)
    -   \(108) breeder\_f: Volume of Li4SiO4 / (Volume of Be12Ti + Li4SiO4)
    -   \(109) ralpne: thermal alpha density / electron density
    -   \(110) ftaulimit: Lower limit on taup/taueff the ratio of alpha particle
        to energy confinement times (f-value for equation 62)
    -   \(111) fniterpump: f-value for constraint that number of vacuum pumps \<
        TF coils (f-value for equation 63)
    -   \(112) fzeffmax: f-value for max Zeff (f-value for equation 64)
    -   \(113) ftaucq: f-value for minimum quench time (f-value for equation 65)
    -   \(114) fw\_channel\_length: Length of a single first wall channel
    -   \(115) fpoloidalpower: f-value for max rate of change of energy in
        poloidal field (f-value for equation 66)
    -   \(116) fradwall: f-value for radiation wall load limit (eq. 67)
    -   \(117) fpsepbqar: f-value for Psep\*Bt/qar upper limit (eq. 68)
    -   \(118) fpsep: f-value to ensure separatrix power is less than value from
        Kallenbach divertor (f-value for equation 69)
    -   \(119) tesep: separatrix temperature calculated by the Kallenbach
        divertor model
    -   \(120) ttarget: Plasma temperature adjacent to divertor sheath \[eV\]
    -   \(121) neratio: ratio of mean SOL density at OMP to separatrix density at
        OMP
    -   \(122) oh\_steel\_frac : streel fraction of Central Solenoid
    -   \(123) foh\_stress : f-value for CS coil Tresca stress limit (f-value for
        eq. 72)
    -   \(124) qtargettotal : Power density on target including surface
        recombination \[W/m2\]
    -   \(125) fimp(3) : Beryllium density fraction relative to electron density
    -   \(126) fimp(4) : Carbon density fraction relative to electron density
    -   \(127) fimp(5) : Nitrogen fraction relative to electron density
    -   \(128) fimp(6) : Oxygen density fraction relative to electron density
    -   \(129) fimp(7) : Neon density fraction relative to electron density
    -   \(130) fimp(8) : Silicon density fraction relative to electron density
    -   \(131) fimp(9) : Argon density fraction relative to electron density
    -   \(132) fimp(10) : Iron density fraction relative to electron density
    -   \(133) fimp(11) : Nickel density fraction relative to electron density
    -   \(134) fimp(12) : Krypton density fraction relative to electron density
    -   \(135) fimp(13) : Xenon density fraction relative to electron density
    -   \(136) fimp(14) : Tungsten density fraction relative to electron density
    -   \(137) fplhsep (f-value for equation 73)
    -   \(138) rebco\_thickness : thickness of REBCO layer in tape (m)
    -   \(139) copper\_thick : thickness of copper layer in tape (m)
    -   \(140) thkwp : radial thickness of TFC winding pack (m)
    -   \(141) fcqt : TF coil quench temperature \< tmax\_croco (f-value for
        equation 74)
    -   \(142) nesep : electron density at separatrix \[m-3\]
    -   \(143) f\_copperA\_m2 : TF coil current / copper area \< Maximum value
        (f-value for equation 75)
    -   \(144) fnesep : Eich critical electron density at separatrix (f-value for
        constraint equation 76)
    -   \(145) fgwped : fraction of Greenwald density to set as pedestal-top
        density
    -   \(146) fcpttf : F-value for TF coil current per turn limit (constraint
        equation 77)
    -   \(147) freinke : F-value for Reinke detachment criterion (constraint
        equation 78)
    -   \(148) fzactual : fraction of impurity at SOL with Reinke detachment
        criterion
    -   \(149) fbmaxcs : F-value for max peak CS field (con. 79, itvar 149)
    -   \(150) plasmod\_fcdp : (P\_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD
        power over available power
    -   \(151) plasmod\_fradc : Pline\_Xe / (Palpha + Paux - PlineAr - Psync -
        Pbrad)
    -   \(152) fbmaxcs : Ratio of separatrix density to Greenwald density
    -   \(153) fpdivlim : F-value for minimum pdivt (con. 80)
    -   \(154) fne0 : F-value for ne(0) \> ne(ped) (con. 81)
-   sqsumsq : sqrt of the sum of the square of the constraint residuals
-   epsfcn /1.0e-3/ : finite difference step length for HYBRD/VMCON
    derivatives
-   epsvmc /1.0e-6/ : error tolerance for VMCON
-   factor /0.1/ : used in HYBRD for first step size
-   ftol /1.0e-4/ : error tolerance for HYBRD
-   boundl(ipnvars) /../ : lower bounds used on ixc variables during
    VMCON optimisation runs
-   boundu(ipnvars) /../ : upper bounds used on ixc variables during
    VMCON optimisation runs

### [eqsolv](eqsolv.html)

### [optimiz](optimiz.html)

### [impurity\_radiation\_module](impurity_radiation_module.html)

(It is recommended to turn on constraint eqn.17 with iteration variable
28: fradpwr.)

-   nimp /14/ FIX : number of ion species in impurity radiation model
-   coreradius /0.6/ : normalised radius defining the \'core\' region
-   coreradiationfraction /1.0/ : fraction of radiation from \'core\'
    region that is subtracted from the loss pow
-   fimp(nimp)
    /1.0,0.1,0.02,0.0,0.0,0.0,0.0,0.0,0.0016,0.0,0.0,0.0,0.0,0.0/ :
    impurity number density fractions relative to electron density
    (iteration variable 102 is fimp(impvar))
-   imp\_label(nimp) : impurity ion species names:
    -   ( 1) Hydrogen (fraction calculated by code)
    -   ( 2) Helium
    -   ( 3) Beryllium
    -   ( 4) Carbon
    -   ( 5) Nitrogen
    -   ( 6) Oxygen
    -   ( 7) Neon
    -   ( 8) Silicon
    -   ( 9) Argon
    -   \(10) Iron
    -   \(11) Nickel
    -   \(12) Krypton
    -   \(13) Xenon
    -   \(14) Tungsten
-   fimpvar /1.0e-3/ : impurity fraction to be used as fimp(impvar)
    (iteration variable 102)
-   impdir /\'/home/PROCESS/\[branch\]/impuritydata\'/ : Directory
    containing impurity radiation data files
-   impvar : impurity to be iterated (deprecated) variable number 102 is
    turned on

### [initialise\_imprad](initialise_imprad.html)

### [init\_imp\_element](init_imp_element.html)

### [import\_impdata](import_impdata.html)

### [z2index](z2index.html)

### [element2index](element2index.html)

### [impradprofile](impradprofile.html)

### [pbremden](pbremden.html)

### [pimpden](pimpden.html)

### [fradcore](fradcore.html)

### [Zav\_of\_te](Zav_of_te.html)

### [scan\_module](scan_module.html)

-   ipnscns /1000/ FIX : maximum number of scan points
-   ipnscnv /45/ FIX : number of available scan variables
-   scan\_dim /1/ : 1-D or 2-D scan switch (1=1D, 2=2D)
-   isweep /0/ : number of scan points to calculate
-   isweep\_2 /0/ : number of 2D scan points to calculate
-   nsweep /1/ : switch denoting quantity to scan:
    -   1 aspect
    -   2 hldivlim
    -   3 pnetelin
    -   4 hfact
    -   5 oacdcp
    -   6 walalw
    -   7 beamfus0
    -   8 fqval
    -   9 te
    -   10 boundu(15: fvs)
    -   11 dnbeta
    -   12 bscfmax (use negative values only)
    -   13 boundu(10: hfact)
    -   14 fiooic
    -   15 fjprot
    -   16 rmajor
    -   17 bmxlim
    -   18 gammax
    -   19 boundl(16: ohcth)
    -   20 tbrnmn
    -   21 not used
    -   22 cfactr (N.B. requires iavail=0)
    -   23 boundu(72: fipir)
    -   24 powfmax
    -   25 kappa
    -   26 triang
    -   27 tbrmin (for blktmodel \> 0 only)
    -   28 bt
    -   29 coreradius
    -   30 fimpvar
    -   31 taulimit
    -   32 epsvmc
    -   33 ttarget
    -   34 qtargettotal
    -   35 lambda\_q\_omp
    -   36 lambda\_target
    -   37 lcon\_factor
    -   38 Neon upper limit
    -   39 Argon upper limit
    -   40 Xenon upper limit
    -   41 blnkoth
    -   42 Argon fraction fimp(9)
    -   43 normalised minor radius at which electron cyclotron current
        drive is maximum
    -   44 Allowable tresca stress in tf coil structural material
    -   45 Minimum allowable temperature margin ; tf coils
    -   46 boundu(150) fgwsep
    -   47 impurity\_enrichment(9) Argon impurity enrichment
    -   48 TF coil - n\_pancake (integer turn winding pack)
    -   49 TF coil - n\_layer (integer turn winding pack)
    -   50 Xenon fraction fimp(13)
    -   51 Power fraction to lower DN Divertor ftar
    -   52 SoL radiation fraction
-   nsweep\_2 /3/ : switch denoting quantity to scan for 2D scan:
-   sweep(ipnscns) /../: actual values to use in scan
-   sweep\_2(ipnscns) /../: actual values to use in 2D scan

### [scan](scan.html)

### [scan\_1d](scan_1d.html)

### [scan\_2d](scan_2d.html)

### [scan\_select](scan_select.html)

### [doopt](doopt.html)
