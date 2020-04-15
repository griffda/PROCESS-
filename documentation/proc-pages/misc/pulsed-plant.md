# Pulsed Plant Operation

[PDF Doc](./media/pulsed-doc.pdf)

If the plasma current is partially or entirely driven by electromagnetic induction, it is necessary to operate the plant in a pulsed manner as the current swing in the central solenoid coils cannot be continued indefinitely. `PROCESS` can perform a number of calculations relevant to a pulsed power plant, as detailed below.

Switch `lpulse` determines whether the power plant is assumed to be based on steady-state (`lpulse = 0`) or pulsed (`lpulse = 1`) operation.

## Start-up power requirements

The minimum auxiliary power required during the start-up (ignition) phase is calculated on the basis of a POPCON analysis. Ignition is accessed via the so-called Cordey Pass (the path in plasma density-temperature space which minimises the power requirement) and the code ensures that there is sufficient auxiliary power to accommodate this. In fact. this calculation is very CPU-intensive, so the relevant routine is not called at present. In practice, the auxiliary power tends to exceed the minimum allowable value anyway, without any need to constrain it to do so.

The auxiliary power reaching the plasma can be forced to be more than the minimum allowable value `auxmin` by turning on constraint equation no. 40 with iteration variable no. 64 (`fauxmn`). The value of `auxmin` is determined by the code if the start-up model is activated, otherwise it may by initialised via the input file.

## Plasma current ramp-up time

This calculation ensures that the plasma current ramp rate during start-up is prevented from being too high, as governed by the requirement to maintain plasma stability in $l_i - q_\psi$ space.

## Burn time

The length of the burn time is calculated from the surplus volt-seconds available from the OH/PF coil system during the plasma burn phase, after the flux required during the plasma start-up is taken into account. A minimum burn time can be enforced via constraint equation no. 13 and iteration variable no 21. (`ftburn`).

## Thermal storage

During every cycle there is a period when no fusion power is produced. The net electricity output from the plant must, however, be maintained, and this is achieved using thermal storage, There are three types of thermal storage available within `PROCESS`, and the value of switch `istore` determines which is to be used. If `istore = 1` (the default), option 1 of Ref[^1] is assumed, which utilises the thermal storage inherent in the machine's steam cycle equipment. This should be used is the machine down time is less than 100 seconds. If `istore = 2` option 2 of Ref[^1] is assumed, which uses the same method as before, but augments it with an additional boiler. This may be used for machine down times of up to 300 seconds. Finally, if `istore = 3`, a large stainless steel block acts as the thermal storage medium.

[^1]: *"Pulsed Fusion Reactor Study"*, AEA Fusion Report AEA FUS 205 (1992)