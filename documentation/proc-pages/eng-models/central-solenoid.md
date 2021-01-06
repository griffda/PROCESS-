# Central Solenoid

Formerly known as the ohmic heating (OH) coil, the central solenoid (CS) is a PF coil used during start-up and during the burn phase to create and maintain the plasma current by inductive means. Swinging (changing) the current through the central solenoid causes a change in the flux linked to the plasma region, inducing a current in it. `PROCESS` calculates the amount of flux required to produce the plasma curren, and also the amount actually available. The code measures the magnetic flux in units of Volt0seconds (= Webers).

Switch `iohcl` controls whether a central solenoid is present. A value of 1 denotes that this coil is present, and should be assigned a non-zero thickness `ohcth`. A value of `iohcl` = 0 denotes that no central solenoid is present, in which case the thickness `ohcth` should be zero. No PF coils should be located at positions defined by `ipfloc(j)` = 1 if no central solenoid is present.

The central solenoid can be either resistive or superconducting (controlled via switch `ipfres` as for the other PF coils), and if superconducting, switch `isumatpf` determines the superconducting material to use -  its value is used like `isumattf` and `isumatpf`. The fraction of copper present in the superconducting filaments is given by the value of variable `fcuohsu`.

If the central solenoid is superconducting, the coil contains steel for strength. The cross-sectional area of steel is determined by the *J* x *B* hoop force on the coil divided by the allowable hoop stress ,given by the input `alstroh`. A steel thickness is given in the output, which would be the thickness of a steel case of the same cross-sectional area if it simply surrounded the conducting region.

## Current density inputs and limits

The (absolute value of the) central solenoid current density at the end-of-flat-top ('EOF'), `coheof` is specified by the user, and can be used as an iteration variable (no. 37). The current density at the beginning-of-pulse ('BOP' - See Figure 1) is specified as a (positive) fraction of `coheof` using `fcohbop` (iteration variable no. 41). The current density in the CS at all other times is calculated by taking into account of the flux swing necessary to initiate and maintain plasma current. The positive or negative sign of the current at each time is calculated automatically.

<figure>
    <center>
    <img src="../../images/current_vs_time.png" alt="current-vs-time-plot" 
    title="Current waveform for Plasma, PF coil and Central Solenoid" 
    width="650" height="100" />
    <br><br>
    <figcaption><i>Figure 2: Plot showing schematically the current waveforms for the plasma, a typical PF coil, and the central solenoid. Note that the currents in some of the PF coils may be the opposite sign to that shown, and the central solenoid current may remain positive during the I<sub>p</sub> ramp-up period, although it will pass through zero during the burn phase.</i></figcaption>
    <br>
    </center>
</figure>

The current density in the central solenoid can be limited at the BOP and at the EOF. To limit the current density at teh BOP, constraint equation no. 27 should be turned on with iteration variable no. 39 (`fjohc0`). To limit the current density at the EOF, constraint equation no. 26 should be turned on with iteration variable no. 38 (`fjohc`).

As for the TF coils, the critical current density *J*<sub>crit</sub> falls with the temperature of the superconductor. The temperature margin $\Delta$*T* is the difference between the temperature at which *J*<sub>crit</sub> would be equal to *J*<sub>op</sub> and the operating temperature. The minimum allowed $\Delta$*T* can be set using input parameter `tmargmin` together with constraint equation no. 60 and iteration variable no. 106 (`ftmargoh`).

Note that is the temperature margin is positive, *J*<sub>op</sub> is guaranteed to be lower that *J*<sub>crit</sub>, and so constraints 26, 27 and 60 need not all by turned on (in fact, it is recommended that EITHER the latter constraint, OR the former two constraints, is/are activated in any given run).

## Plasma current ramp-up time

In the steady-state power plant scenario (`lpulse` $\neq$ - See Pulsed Plant Operation), the length of time taken for the central solenoid current to (possibly) reverse (which is equal to the plasma current ramp-up time - See Figure 1) is determined from the value of switch `tohsin`. If `tohsin` = 0, then the plasma current ramp-up time `tohs` in seconds is given by `tohs` = *I<sub>p</sub>*/0.5, where *I<sub>p</sub>* is the plasma current in MA. Futhermore, the PF coil ramp time `tramp` and shutdown time `tqnch` are set equal to `tohs`. Of `tohsin` $\neq$ 0, the plasma current ramp-up time `tohs` = ` tohsin`, and the PF coil ramp and shutdown times are input parameters.

If, however, a pulsed power plant is being modelled (`lpulse` = 1), the plasma current ramp-up time `tohs` is either an input parameter, or it can be iterated by using iteration variable 65. The ramp-up and shutdown time in the pulsed case are always set equal to `tohs`. To ensure that the plasma current ramp rate during start-up is prevented from being too high, as governed by the requirement to maintain plasma stability by ensuring that the induced current has time to diffuse into the body of the plasma, constraint equation no. 41 should be turned on with iteration variable no. 66 (`tfohs`).