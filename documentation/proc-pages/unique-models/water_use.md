# Power Plant Water Use

<p style='text-align: justify;'>
The routines documented here can be used to estimate the amount of water used within the secondary cooling system of an operating fusion power plant, i.e. post electricity generation. </p>

<p style='text-align: justify;'>
This water usage depends largely on the waste heat produced by the power plant, and estimates can therefore be heavily based on existing technology and techniques currently used in standard thermoelectric power plant operation. </p>

<p style='text-align: justify;'>
Precise estimates of water usage require detailed site information and a decision on the cooling method. The routines implemented here generate estimates for water use through: </p>

1. Cooling towers
2. Cooling by a body of water: i) recirculating system (applicable for ponds, lakes), ii) once-through system (applicable for lakes, rivers)

## Water Use Estimation

<p style='text-align: justify;'>
The water used by a power plant is defined as that withdrawn from the environment, and includes the water evaporated ('consumed') by any introduced heat: 'withdrawal' is defined as the amount of water removed from the ground or diverted from a water source for use, while 'consumption' refers to the amount of water that is evaporated... or otherwise removed from the immediate water environment.

[^1] 
</p>

<p style='text-align: justify;'>
The amount of water evaporated by an operating fusion plant can be estimated from the waste heat remaining after electricity generation: within <em>PROCESS</em> this is the 'heat rejected by main power conversion circuit (MW)'. Evaporation rates and resultant total water use depend upon the selected cooling method. The models used in these calculations are based upon models and observations published in U.S. Geological Survey Scientific Investigations.

[^2]
[^3]
</p>

### Cooling Towers

<p style='text-align: justify;'>
The calculation for water evaporated through a cooling tower is based upon the method in USGS Report 2013–5188,

[^2] where the evaporation ratio, <em>ER</em> (the ratio of the heat used to evaporate water to the total heat discharged through the tower), can be approximated from the ambient air temperature, <em>T_a</em>:</p>

```math
ER = 1 - ( (-0.000279*T_\mathrm{a}^3 + 0.00109*T_\mathrm{a}^2 - 0.345*T_\mathrm{a} + 26.7) /100)
```

<p style='text-align: justify;'>
<<<<<<< HEAD
The input waste heat and the evaporation ratio are used to calculate the amount of water evaporated during plant operation. The total amount of water used is then estimated as a ratio of the evaporated water, as per USGS Report 2014–5184:

[^3] the mean of the observed ratios (1.4) is used here.</p>
The input waste heat and the evaporation ratio are used to calculate the amount of water evaporated during plant operation. The total amount of water used is then estimated as a ratio of the evaporated water, as per USGS Report 2014–5184[^3]: the mean of the observed ratios (1.4) is used here.</p>
>>>>>>> develop

Cooling using water bodies is implemented through either a recirculating system (applicable for ponds and small lakes), or a once-through system (applicable for lakes and rivers).</p>

Recirculating cooling system: A cooling system in which water is circulated through condensers, cooled, and then re-used in the same process.

Once-through cooling system: A cooling system in which the water is withdrawn from a surface-water source... and that discharges the water back to surface water at a higher temperature.[^3]

<p style='text-align: justify;'>
The calculation for water evaporated from a cooling water body is based upon heat budget models developed for USGS Report 2013–5188,

[^2] in which <em>ER</em> is found from water temperature, wind speed and experimentally-derived wind functions. Refer to that report's spreadsheet model for details: https://pubs.usgs.gov/sir/2013/5188/appendix/sir2013-5188_appendix4_fews_version_3.104_edit_20141106.xlsx
</p>

<p style='text-align: justify;'>
In this estimation, the amount of evaporation from ponds, lakes and rivers is similar. The current routines therfore use an average value across these water bodies to calculate water use; including site-specific information would potentially allow for more refined results.</p>

<p style='text-align: justify;'>
As for cooling towers, the input waste heat and <em>ER</em> are used to calculate the amount of water evaporated, and the total amount of water used is then estimated as per USGS Report 2014–5184.

[^3] For a recirculating water system the total water used was defined as equal to the amount evaporated. For a once-through water system the total amount of water used was estimated as a ratio of the evaporated water; the mean of the observed ratios (98.3) was used here.</p>
## Caveats and Limitations

- For simplicity, the water density used in these calculations is set to a static value applicable to water at 21 degrees Celsius
## References

[^1]: J Macknick and R Newmark and G Heath and K C Hallett, Operational water consumption and withdrawal factors for electricity generating technologies: a review of existing literature, 2012, Environ. Res. Lett. 7 045802, https://doi.org/10.1088/1748-9326/7/4/045802

[^2]: Diehl, T.H., Harris, M.A., Murphy, J.C., Hutson, S.S., and Ladd, D.E., 2013, Methods for estimating water consumption for thermoelectric power plants in the United States: U.S. Geological Survey Scientific Investigations Report 2013–5188, 78 p., http://dx.doi.org/10.3133/sir20135188

[^3]: Diehl, T.H., and Harris, M.A., 2014, Withdrawal and consumption of water by thermoelectric power plants in the United States, 2010: U.S. Geological Survey Scientific Investigations Report 2014–5184, 28 p., http://dx.doi.org/10.3133/sir20145184