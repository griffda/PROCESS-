# Structural components

[PDF Doc](./media/cryodoc.pdf)

The internal vacuum vessel provides a toroidal evacuated chamber containing the plasma, first wall, blanket and shield, and the space between this item and the external cylindrical cryostat encloses those components that need to operate at liquid helium temperatures. These include any superconducting (TF or PF) coils and the inter-coil structure. `PROCESS` calculates the cryogenic power load and the resulting heat exchanger requirements.

The vertical distance *h* between the uppermost PF coil and the external cryostat lib may be adjusted by changing the value of input parameters `clhsf`; a scaling based on ITER is used:
$$
h = \mathtt{clhsf} \left( \frac{2 \times \mathtt{rdewex}}{28.440}\right)
$$

The vacuum system is used for four difference processes. Firstly, before plasma operations the chamber must be evacuated to remove outgassed impurities from the structure. Secondly, the chamber must by re-evacuated between burn operations. Thirdly, helium ash must be removed to prevent it from diluting the fuel. Finally, deuterium and tritium is removed on a steady state basis. `PROCESS` calculates the parameters of a vacuum system that satisfy all four requirements, with the option of either turbo pumps or cryo pumps being used.

Switch `ntype` controls whether a turbo pump (`ntype` = 0) or a cryo pump (`ntype` = 1) is used in the vacuum system.