# PROCESS Variable Descriptions
---
## Introduction
Variables labelled with FIX are initialised with the given default value (shown between / / characters), but currently are not available to be changed in the input file.
All other variables shown with a default value (including arrays boundl, boundu and sweep) can be changed in the input file.
Variables not shown with a default value are calculated within PROCESS, so need not be initialised.
---

## availability_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>day</td>
		<td>real</td>
		<td>86400.0D0</td>
		<td>seconds in a day [s]</td>
	</tr>
	<tr>
		<td>year</td>
		<td>real</td>
		<td>31557600.0D0</td>
		<td>seconds in a year [s]</td>
	</tr>
</table>

## build_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>aplasmin</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>minimum minor radius (m)</td>
	</tr>
	<tr>
		<td>available_radial_space</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Minimal radial space between plasma and coils (m)</td>
	</tr>
	<tr>
		<td>blarea</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>blanket total surface area (m2)</td>
	</tr>
	<tr>
		<td>blareaib</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard blanket surface area (m2)</td>
	</tr>
	<tr>
		<td>blareaob</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard blanket surface area (m2)</td>
	</tr>
	<tr>
		<td>blbmith</td>
		<td>real</td>
		<td>0.17D0</td>
		<td>inboard blanket box manifold thickness (m) (blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>blbmoth</td>
		<td>real</td>
		<td>0.27D0</td>
		<td>outboard blanket box manifold thickness (m) (blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>blbpith</td>
		<td>real</td>
		<td>0.30D0</td>
		<td>inboard blanket base plate thickness (m) (blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>blbpoth</td>
		<td>real</td>
		<td>0.35D0</td>
		<td>outboard blanket base plate thickness (m) (blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>blbuith</td>
		<td>real</td>
		<td>0.365D0</td>
		<td>inboard blanket breeding zone thickness (m) (blktmodel&gt;0) (iteration variable 90)</td>
	</tr>
	<tr>
		<td>blbuoth</td>
		<td>real</td>
		<td>0.465D0</td>
		<td>outboard blanket breeding zone thickness (m) (blktmodel&gt;0) (iteration variable 91)</td>
	</tr>
	<tr>
		<td>blnkith</td>
		<td>real</td>
		<td>0.115D0</td>
		<td>inboard blanket thickness (m); (calculated if blktmodel&gt;0) (=0.0 if iblnkith=0)</td>
	</tr>
	<tr>
		<td>blnkoth</td>
		<td>real</td>
		<td>0.235D0</td>
		<td>outboard blanket thickness (m); calculated if blktmodel&gt;0</td>
	</tr>
	<tr>
		<td>blnktth</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>top blanket thickness (m), = mean of inboard and outboard blanket thicknesses</td>
	</tr>
	<tr>
		<td>bore</td>
		<td>real</td>
		<td>1.42D0</td>
		<td>central solenoid inboard radius (m) (iteration variable 29)</td>
	</tr>
	<tr>
		<td>clhsf</td>
		<td>real</td>
		<td>4.268D0</td>
		<td>cryostat lid height scaling factor (tokamaks)</td>
	</tr>
	<tr>
		<td>ddwex</td>
		<td>real</td>
		<td>0.07D0</td>
		<td>cryostat thickness (m)</td>
	</tr>
	<tr>
		<td>ddwi</td>
		<td>real</td>
		<td>0.07D0</td>
		<td>vacuum vessel thickness (TF coil / shield) (m)</td>
	</tr>
	<tr>
		<td>dh_tf_inner_bore</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil vertical inner bore (m)</td>
	</tr>
	<tr>
		<td>dr_tf_inner_bore</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil horizontal inner bore (m)</td>
	</tr>
	<tr>
		<td>f_avspace</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>F-value for stellarator radial space check (constraint equation 83)</td>
	</tr>
	<tr>
		<td>fcspc</td>
		<td>real</td>
		<td>0.6D0</td>
		<td>Fraction of space occupied by CS pre-compression structure</td>
	</tr>
	<tr>
		<td>fmsbc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in (non-existent!) bucking cylinder</td>
	</tr>
	<tr>
		<td>fmsbl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in blanket</td>
	</tr>
	<tr>
		<td>fmsdwe</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in cryostat</td>
	</tr>
	<tr>
		<td>fmsdwi</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in vacuum vessel</td>
	</tr>
	<tr>
		<td>fmsfw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in first wall</td>
	</tr>
	<tr>
		<td>fmsoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in central solenoid</td>
	</tr>
	<tr>
		<td>fmssh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in shield</td>
	</tr>
	<tr>
		<td>fmstf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Martensitic fraction of steel in TF coil</td>
	</tr>
	<tr>
		<td>fseppc</td>
		<td>real</td>
		<td>3.5D8</td>
		<td>Separation force in CS coil pre-compression structure</td>
	</tr>
	<tr>
		<td>fwarea</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall total surface area (m2)</td>
	</tr>
	<tr>
		<td>fwareaib</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard first wall surface area (m2)</td>
	</tr>
	<tr>
		<td>fwareaob</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard first wall surface area (m2)</td>
	</tr>
	<tr>
		<td>fwith</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard first wall thickness, initial estimate as calculated (m)</td>
	</tr>
	<tr>
		<td>fwoth</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard first wall thickness, initial estimate as calculated (m)</td>
	</tr>
	<tr>
		<td>gapds</td>
		<td>real</td>
		<td>0.155D0</td>
		<td>gap between inboard vacuum vessel and thermal shield (m) (iteration variable 61)</td>
	</tr>
	<tr>
		<td>gapoh</td>
		<td>real</td>
		<td>0.08D0</td>
		<td>gap between central solenoid and TF coil (m) (iteration variable 42)</td>
	</tr>
	<tr>
		<td>gapomin</td>
		<td>real</td>
		<td>0.234D0</td>
		<td>minimum gap between outboard vacuum vessel and TF coil (m) (iteration variable 31)</td>
	</tr>
	<tr>
		<td>gapsto</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>gap between outboard vacuum vessel and TF coil (m)</td>
	</tr>
	<tr>
		<td>hmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum (half-)height of TF coil (inside edge) (m)</td>
	</tr>
	<tr>
		<td>hpfdif</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>difference in distance from midplane of upper and lower portions of TF <br> legs (non-zero for single-null devices) (m)</td>
	</tr>
	<tr>
		<td>hpfu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>height to top of (upper) TF coil leg (m)</td>
	</tr>
	<tr>
		<td>hr1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>half-height of TF coil inboard leg straight section (m)</td>
	</tr>
	<tr>
		<td>iohcl</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for existence of central solenoid:<br><ul><li> =0 central solenoid not present</li><li> =1 central solenoid exists</li></ul></td>
	</tr>
	<tr>
		<td>iprecomp</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for existence of central solenoid pre-compression structure:<br><ul><li> =0 no pre-compression structure</li><li> =1 calculated pre-compression structure</li></ul></td>
	</tr>
	<tr>
		<td>ohcth</td>
		<td>real</td>
		<td>0.811D0</td>
		<td>Central solenoid thickness (m) (iteration variable 16)</td>
	</tr>
	<tr>
		<td>plleni</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>length of inboard divertor plate (m)</td>
	</tr>
	<tr>
		<td>plleno</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>length of outboard divertor plate (m)</td>
	</tr>
	<tr>
		<td>plsepi</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>poloidal length, x-point to inboard strike point (m)</td>
	</tr>
	<tr>
		<td>plsepo</td>
		<td>real</td>
		<td>1.5D0</td>
		<td>poloidal length, x-point to outboard strike point (m)</td>
	</tr>
	<tr>
		<td>precomp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>CS coil precompression structure thickness (m)</td>
	</tr>
	<tr>
		<td>r_cp_top</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Top outer radius of the centropost (ST only) (m)</td>
	</tr>
	<tr>
		<td>r_sh_inboard_in</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Radial inner side position of inboard neutronic shield [m]</td>
	</tr>
	<tr>
		<td>r_sh_inboard_out</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Radial plasma facing side position of inboard neutronic shield [m]</td>
	</tr>
	<tr>
		<td>r_tf_inboard_in</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Mid-plane Outer radius of inner of inboard TF leg (m)</td>
	</tr>
	<tr>
		<td>r_tf_inboard_mid</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Mid-plane Outer radius of centre of inboard TF leg (m)</td>
	</tr>
	<tr>
		<td>r_tf_inboard_out</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Mid-plane Outer radius of centre of inboard TF leg (m)</td>
	</tr>
	<tr>
		<td>r_tf_outboard_mid</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radius to the centre of the outboard TF coil leg (m)</td>
	</tr>
	<tr>
		<td>r_vv_inboard_out</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Radial plasma facing side position of inboard vacuum vessel [m]</td>
	</tr>
	<tr>
		<td>rbld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>sum of thicknesses to the major radius (m)</td>
	</tr>
	<tr>
		<td>required_radial_space</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Required space between coil and plasma for blanket shield wall etc (m)</td>
	</tr>
	<tr>
		<td>rinboard</td>
		<td>real</td>
		<td>0.651D0</td>
		<td>plasma inboard radius (m) (consistency equation 29)</td>
	</tr>
	<tr>
		<td>rsldi</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radius to inboard shield (inside point) (m)</td>
	</tr>
	<tr>
		<td>rsldo</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radius to outboard shield (outside point) (m)</td>
	</tr>
	<tr>
		<td>rspo</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard strike point radius (m)</td>
	</tr>
	<tr>
		<td>scrapli</td>
		<td>real</td>
		<td>0.14D0</td>
		<td>Gap between plasma and first wall, inboard side (m) (if iscrp=1) (iteration variable 73)</td>
	</tr>
	<tr>
		<td>scraplo</td>
		<td>real</td>
		<td>0.15D0</td>
		<td>gap between plasma and first wall, outboard side (m) (if iscrp=1) (iteration variable 74)</td>
	</tr>
	<tr>
		<td>sharea</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>shield total surface area (m2)</td>
	</tr>
	<tr>
		<td>shareaib</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard shield surface area (m2)</td>
	</tr>
	<tr>
		<td>shareaob</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard shield surface area (m2)</td>
	</tr>
	<tr>
		<td>shldith</td>
		<td>real</td>
		<td>0.69D0</td>
		<td>inboard shield thickness (m) (iteration variable 93)</td>
	</tr>
	<tr>
		<td>shldlth</td>
		<td>real</td>
		<td>0.7D0</td>
		<td>lower (under divertor) shield thickness (m)</td>
	</tr>
	<tr>
		<td>shldoth</td>
		<td>real</td>
		<td>1.05D0</td>
		<td>outboard shield thickness (m) (iteration variable 94)</td>
	</tr>
	<tr>
		<td>shldtth</td>
		<td>real</td>
		<td>0.6D0</td>
		<td>upper/lower shield thickness (m); calculated if blktmodel &gt; 0</td>
	</tr>
	<tr>
		<td>sigallpc</td>
		<td>real</td>
		<td>3.0D8</td>
		<td>allowable stress in CSpre-compression structure (Pa)</td>
	</tr>
	<tr>
		<td>tfcth</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard TF coil thickness, (centrepost for ST) (m)<br> (input, calculated or iteration variable 13)</td>
	</tr>
	<tr>
		<td>tfoffset</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vertical distance between centre of TF coils and centre of plasma (m)</td>
	</tr>
	<tr>
		<td>tfootfi</td>
		<td>real</td>
		<td>1.19D0</td>
		<td>TF coil outboard leg / inboard leg radial thickness<br> ratio (i_tf_sup=0 only) (iteration variable 75)</td>
	</tr>
	<tr>
		<td>tfthko</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Outboard TF coil thickness (m)</td>
	</tr>
	<tr>
		<td>tftsgap</td>
		<td>real</td>
		<td>0.05D0</td>
		<td>Minimum metal-to-metal gap between TF coil and thermal shield (m)</td>
	</tr>
	<tr>
		<td>thshield</td>
		<td>real</td>
		<td>0.05D0</td>
		<td>TF-VV thermal shield thickness (m)</td>
	</tr>
	<tr>
		<td>vgap</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vertical gap between x-point and divertor (m) (if = 0, it is calculated)</td>
	</tr>
	<tr>
		<td>vgap2</td>
		<td>real</td>
		<td>0.163D0</td>
		<td>vertical gap between vacuum vessel and thermal shields (m)</td>
	</tr>
	<tr>
		<td>vgaptop</td>
		<td>real</td>
		<td>0.60D0</td>
		<td>vertical gap between top of plasma and first wall (m)</td>
	</tr>
	<tr>
		<td>vvblgap</td>
		<td>real</td>
		<td>0.05D0</td>
		<td>gap between vacuum vessel and blanket (m)</td>
	</tr>
</table>

## buildings_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>admv</td>
		<td>real</td>
		<td>1.0D5</td>
		<td>administration building volume (m3)</td>
	</tr>
	<tr>
		<td>admvol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of administration buildings (m3)</td>
	</tr>
	<tr>
		<td>clh1</td>
		<td>real</td>
		<td>2.5D0</td>
		<td>vertical clearance from TF coil to cryostat (m) (calculated for tokamaks)</td>
	</tr>
	<tr>
		<td>clh2</td>
		<td>real</td>
		<td>15.0D0</td>
		<td>clearance beneath TF coil to foundation (including basement) (m)</td>
	</tr>
	<tr>
		<td>conv</td>
		<td>real</td>
		<td>6.0D4</td>
		<td>control building volume (m3)</td>
	</tr>
	<tr>
		<td>convol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of control, protection and i&amp;c building (m3)</td>
	</tr>
	<tr>
		<td>cryvol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of cryoplant building (m3)</td>
	</tr>
	<tr>
		<td>efloor</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>effective total floor space (m2)</td>
	</tr>
	<tr>
		<td>elevol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of electrical equipment building (m3)</td>
	</tr>
	<tr>
		<td>esbldgm3</td>
		<td>real</td>
		<td>1.0D3</td>
		<td>volume of energy storage equipment building (m3) (not used if lpulse=0)</td>
	</tr>
	<tr>
		<td>fndt</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>foundation thickness (m)</td>
	</tr>
	<tr>
		<td>hccl</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>clearance around components in hot cell (m)</td>
	</tr>
	<tr>
		<td>hcwt</td>
		<td>real</td>
		<td>1.5D0</td>
		<td>hot cell wall thickness (m)</td>
	</tr>
	<tr>
		<td>mbvfac</td>
		<td>real</td>
		<td>2.8D0</td>
		<td>maintenance building volume multiplication factor</td>
	</tr>
	<tr>
		<td>pfbldgm3</td>
		<td>real</td>
		<td>2.0D4</td>
		<td>volume of PF coil power supply building (m3)</td>
	</tr>
	<tr>
		<td>pibv</td>
		<td>real</td>
		<td>2.0D4</td>
		<td>power injection building volume (m3)</td>
	</tr>
	<tr>
		<td>rbrt</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>reactor building roof thickness (m)</td>
	</tr>
	<tr>
		<td>rbvfac</td>
		<td>real</td>
		<td>1.6D0</td>
		<td>reactor building volume multiplication factor</td>
	</tr>
	<tr>
		<td>rbvol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>reactor building volume (m3)</td>
	</tr>
	<tr>
		<td>rbwt</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>reactor building wall thickness (m)</td>
	</tr>
	<tr>
		<td>rmbvol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of maintenance and assembly building (m3)</td>
	</tr>
	<tr>
		<td>row</td>
		<td>real</td>
		<td>4.0D0</td>
		<td>clearance to building wall for crane operation (m)</td>
	</tr>
	<tr>
		<td>rxcl</td>
		<td>real</td>
		<td>4.0D0</td>
		<td>clearance around reactor (m)</td>
	</tr>
	<tr>
		<td>shmf</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>fraction of shield mass per TF coil to be moved in the maximum shield lift</td>
	</tr>
	<tr>
		<td>shov</td>
		<td>real</td>
		<td>1.0D5</td>
		<td>shops and warehouse volume (m3)</td>
	</tr>
	<tr>
		<td>shovol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of shops and buildings for plant auxiliaries (m3)</td>
	</tr>
	<tr>
		<td>stcl</td>
		<td>real</td>
		<td>3.0D0</td>
		<td>clearance above crane to roof (m)</td>
	</tr>
	<tr>
		<td>tfcbv</td>
		<td>real</td>
		<td>2.0D4</td>
		<td>volume of TF coil power supply building (m3) (calculated if TF coils are superconducting)</td>
	</tr>
	<tr>
		<td>trcl</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>transportation clearance between components (m)</td>
	</tr>
	<tr>
		<td>triv</td>
		<td>real</td>
		<td>4.0D4</td>
		<td>volume of tritium, fuel handling and health physics buildings (m3)</td>
	</tr>
	<tr>
		<td>volnucb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>sum of nuclear buildings volumes (m3)</td>
	</tr>
	<tr>
		<td>volrci</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>internal volume of reactor building (m3)</td>
	</tr>
	<tr>
		<td>wgt</td>
		<td>real</td>
		<td>5.0D5</td>
		<td>reactor building crane capacity (kg) (calculated if 0 is input)</td>
	</tr>
	<tr>
		<td>wgt2</td>
		<td>real</td>
		<td>1.0D5</td>
		<td>hot cell crane capacity (kg) (calculated if 0 is input)</td>
	</tr>
	<tr>
		<td>wrbi</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>distance from centre of machine to building wall (m), i.e. reactor building half-width</td>
	</tr>
	<tr>
		<td>wsvfac</td>
		<td>real</td>
		<td>1.9D0</td>
		<td>warm shop building volume multiplication factor</td>
	</tr>
	<tr>
		<td>wsvol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of warm shop building (m3)</td>
	</tr>
</table>

## ccfe_hcpb_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>W_density</td>
		<td>real</td>
		<td>19.25D0*1000.0D0</td>
		<td>Tungsten density [kg/m3]</td>
	</tr>
	<tr>
		<td>armour_density</td>
		<td>real</td>
		<td>None</td>
		<td>FW armour density [kg/m3]</td>
	</tr>
	<tr>
		<td>blanket_density</td>
		<td>real</td>
		<td>None</td>
		<td>Blanket density [kg/m3]</td>
	</tr>
	<tr>
		<td>bldepti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket coolant channel length (radial direction) (m)</td>
	</tr>
	<tr>
		<td>bldepto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket coolant channel length (radial direction) (m)</td>
	</tr>
	<tr>
		<td>bllengi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket segment poloidal length (m)</td>
	</tr>
	<tr>
		<td>bllengo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket segment poloidal length (m)</td>
	</tr>
	<tr>
		<td>blwidti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket mid-plan toroidal circumference for segment (m)</td>
	</tr>
	<tr>
		<td>blwidto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket mid-plan toroidal circumference for segment (m)</td>
	</tr>
	<tr>
		<td>bzfllengi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket flow lengths (m)</td>
	</tr>
	<tr>
		<td>bzfllengo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket flow lengths (m)</td>
	</tr>
	<tr>
		<td>exp_blanket</td>
		<td>real</td>
		<td>None</td>
		<td>Exponential factors in nuclear heating calcs</td>
	</tr>
	<tr>
		<td>exp_shield1</td>
		<td>real</td>
		<td>None</td>
		<td>Exponential factors in nuclear heating calcs</td>
	</tr>
	<tr>
		<td>exp_shield2</td>
		<td>real</td>
		<td>None</td>
		<td>Exponential factors in nuclear heating calcs</td>
	</tr>
	<tr>
		<td>fblli2sio4</td>
		<td>real</td>
		<td>None</td>
		<td>Fractions of blanket by volume: steel, lithium orthosilicate, titanium beryllide</td>
	</tr>
	<tr>
		<td>fblss_ccfe</td>
		<td>real</td>
		<td>None</td>
		<td>Fractions of blanket by volume: steel, lithium orthosilicate, titanium beryllide</td>
	</tr>
	<tr>
		<td>fbltibe12</td>
		<td>real</td>
		<td>None</td>
		<td>Fractions of blanket by volume: steel, lithium orthosilicate, titanium beryllide</td>
	</tr>
	<tr>
		<td>fw_armour_u_nuc_heating</td>
		<td>real</td>
		<td>None</td>
		<td>Unit heating of FW and armour in FW armour (W/kg per W of fusion power)</td>
	</tr>
	<tr>
		<td>fw_density</td>
		<td>real</td>
		<td>None</td>
		<td>FW density [kg/m3]</td>
	</tr>
	<tr>
		<td>hblnkt</td>
		<td>real</td>
		<td>None</td>
		<td>Blanket internal half-height (m)</td>
	</tr>
	<tr>
		<td>hcryopf</td>
		<td>real</td>
		<td>None</td>
		<td>Clearance between uppermost PF coil and cryostat lid (m)</td>
	</tr>
	<tr>
		<td>hshld</td>
		<td>real</td>
		<td>None</td>
		<td>Shield internal half-height (m)</td>
	</tr>
	<tr>
		<td>htpmw_blkti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_blkto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_fwi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard first wall pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_fwo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard first wall pumping power (MW)</td>
	</tr>
	<tr>
		<td>hvv</td>
		<td>real</td>
		<td>None</td>
		<td>Vacuum vessel internal half-height (m)</td>
	</tr>
	<tr>
		<td>ip</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mfblkt</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket mass flow rate for coolant (kg/s)</td>
	</tr>
	<tr>
		<td>mfblkti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket mass flow rate for coolant (kg/s)</td>
	</tr>
	<tr>
		<td>mfblkto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard blanket mass flow rate for coolant (kg/s)</td>
	</tr>
	<tr>
		<td>mfblktpi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard mass flow rate per coolant pipe (kg/s)</td>
	</tr>
	<tr>
		<td>mfblktpo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard mass flow rate per coolant pipe (kg/s)</td>
	</tr>
	<tr>
		<td>mffw</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard total mass flow rate to remove inboard FW power (kg/s)</td>
	</tr>
	<tr>
		<td>mffwi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard total mass flow rate to remove inboard FW power (kg/s)</td>
	</tr>
	<tr>
		<td>mffwo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard total mass flow rate to remove inboard FW power (kg/s)</td>
	</tr>
	<tr>
		<td>mffwpi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard mass flow rate per coolant pipe (kg/s)</td>
	</tr>
	<tr>
		<td>mffwpo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard mass flow rate per coolant pipe (kg/s)</td>
	</tr>
	<tr>
		<td>mftotal</td>
		<td>real</td>
		<td>None</td>
		<td>Total mass flow rate for coolant (kg/s)</td>
	</tr>
	<tr>
		<td>npblkti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard total num of pipes</td>
	</tr>
	<tr>
		<td>npblkto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard total num of pipes</td>
	</tr>
	<tr>
		<td>npfwi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/utboard total number of pipes</td>
	</tr>
	<tr>
		<td>npfwo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/utboard total number of pipes</td>
	</tr>
	<tr>
		<td>ofile</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnuc_tot_blk_sector</td>
		<td>real</td>
		<td>None</td>
		<td>Total nuclear power deposited in blanket covered sector (FW, BLKT, SHLD, TF) (MW)</td>
	</tr>
	<tr>
		<td>pnucblkti</td>
		<td>real</td>
		<td>None</td>
		<td>Neutron power deposited inboard/outboard blanket blanket (MW)</td>
	</tr>
	<tr>
		<td>pnucblkto</td>
		<td>real</td>
		<td>None</td>
		<td>Neutron power deposited inboard/outboard blanket blanket (MW)</td>
	</tr>
	<tr>
		<td>pnucfwi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard first wall nuclear heating (MW)</td>
	</tr>
	<tr>
		<td>pnucfwo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard first wall nuclear heating (MW)</td>
	</tr>
	<tr>
		<td>psurffwi</td>
		<td>real</td>
		<td>None</td>
		<td>Surface heat flux on first wall (MW) (sum = pradfw)</td>
	</tr>
	<tr>
		<td>psurffwo</td>
		<td>real</td>
		<td>None</td>
		<td>Surface heat flux on first wall (MW) (sum = pradfw)</td>
	</tr>
	<tr>
		<td>shield_density</td>
		<td>real</td>
		<td>None</td>
		<td>Shield density [kg/m3]</td>
	</tr>
	<tr>
		<td>shld_u_nuc_heating</td>
		<td>real</td>
		<td>None</td>
		<td>Unit nuclear heating in shield (W per W of fusion power)</td>
	</tr>
	<tr>
		<td>tfc_nuc_heating</td>
		<td>real</td>
		<td>None</td>
		<td>Unit nuclear heating in TF coil (W per W of fusion power)</td>
	</tr>
	<tr>
		<td>tpeakfwi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard first wall peak temperature (K)</td>
	</tr>
	<tr>
		<td>tpeakfwo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard first wall peak temperature (K)</td>
	</tr>
	<tr>
		<td>velblkti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard coolant velocity in blanket (m/s)</td>
	</tr>
	<tr>
		<td>velblkto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard coolant velocity in blanket (m/s)</td>
	</tr>
	<tr>
		<td>vffwi</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard FW coolant void fraction</td>
	</tr>
	<tr>
		<td>vffwo</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard FW coolant void fraction</td>
	</tr>
	<tr>
		<td>volfw</td>
		<td>real</td>
		<td>None</td>
		<td>First wall volume [m3]</td>
	</tr>
	<tr>
		<td>volshldi</td>
		<td>real</td>
		<td>None</td>
		<td>Volume of inboard and outboard shield (m3)</td>
	</tr>
	<tr>
		<td>volshldo</td>
		<td>real</td>
		<td>None</td>
		<td>Volume of inboard and outboard shield (m3)</td>
	</tr>
	<tr>
		<td>vv_density</td>
		<td>real</td>
		<td>None</td>
		<td>Vacuum vessel density [kg/m3]</td>
	</tr>
	<tr>
		<td>x_blanket</td>
		<td>real</td>
		<td>None</td>
		<td>Blanket exponent (tonne/m2)</td>
	</tr>
	<tr>
		<td>x_shield</td>
		<td>real</td>
		<td>None</td>
		<td>Shield exponent (tonne/m2)</td>
	</tr>
</table>

## constants
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>cph2o</td>
		<td>real</td>
		<td>4180.0D0</td>
		<td>specific heat capacity of water (J/kg/K)</td>
	</tr>
	<tr>
		<td>dalu</td>
		<td>real</td>
		<td>2700.0D0</td>
		<td>density of aluminium (kg/m3)</td>
	</tr>
	<tr>
		<td>dcopper</td>
		<td>real</td>
		<td>8900.0D0</td>
		<td>density of copper (kg/m3)</td>
	</tr>
	<tr>
		<td>degrad</td>
		<td>real</td>
		<td>0.01745329251D0</td>
		<td>degrees to radians, = pi/180</td>
	</tr>
	<tr>
		<td>denh2o</td>
		<td>real</td>
		<td>985.0D0</td>
		<td>density of water (kg/m3)</td>
	</tr>
	<tr>
		<td>echarge</td>
		<td>real</td>
		<td>1.60217733D-19</td>
		<td>electron charge [C]</td>
	</tr>
	<tr>
		<td>epsilon0</td>
		<td>real</td>
		<td>8.85418781D-12</td>
		<td>permittivity of free space [Farad/m]</td>
	</tr>
	<tr>
		<td>iotty</td>
		<td>integer</td>
		<td>6</td>
		<td>Standard output unit identifier</td>
	</tr>
	<tr>
		<td>k_copper</td>
		<td>real</td>
		<td>330.0D0</td>
		<td>Copper thermal conductivity (W/m/K)</td>
	</tr>
	<tr>
		<td>kh2o</td>
		<td>real</td>
		<td>0.651D0</td>
		<td>thermal conductivity of water (W/m/K)</td>
	</tr>
	<tr>
		<td>mfile</td>
		<td>integer</td>
		<td>13</td>
		<td>Machine-optimised output file unit</td>
	</tr>
	<tr>
		<td>mproton</td>
		<td>real</td>
		<td>1.6726231D-27</td>
		<td>proton mass [kg]</td>
	</tr>
	<tr>
		<td>muh2o</td>
		<td>real</td>
		<td>4.71D-4</td>
		<td>water dynamic viscosity (kg/m/s)</td>
	</tr>
	<tr>
		<td>nout</td>
		<td>integer</td>
		<td>11</td>
		<td>Output file unit identifier</td>
	</tr>
	<tr>
		<td>nplot</td>
		<td>integer</td>
		<td>12</td>
		<td>Plot data file unit identifier</td>
	</tr>
	<tr>
		<td>opt_file</td>
		<td>integer</td>
		<td>15</td>
		<td>Optimisation information output file number</td>
	</tr>
	<tr>
		<td>pi</td>
		<td>real</td>
		<td>3.1415926535897932D0</td>
		<td>pi</td>
	</tr>
	<tr>
		<td>rmu0</td>
		<td>real</td>
		<td>1.256637062D-6</td>
		<td>permeability of free space  [H/m]</td>
	</tr>
	<tr>
		<td>sig_file</td>
		<td>integer</td>
		<td>16</td>
		<td>TF inboard stress radial distributions file number</td>
	</tr>
	<tr>
		<td>twopi</td>
		<td>real</td>
		<td>6.2831853071795862D0</td>
		<td>2 pi</td>
	</tr>
	<tr>
		<td>umass</td>
		<td>real</td>
		<td>1.660538921D-27</td>
		<td>unified atomic mass unit [kg</td>
	</tr>
	<tr>
		<td>vfile</td>
		<td>integer</td>
		<td>14</td>
		<td>Verbose diagnostics file</td>
	</tr>
</table>

## constraint_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>auxmin</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>minimum auxiliary power (MW) (constraint equation 40)</td>
	</tr>
	<tr>
		<td>betpmx</td>
		<td>real</td>
		<td>0.19D0</td>
		<td>maximum poloidal beta (constraint equation 48)</td>
	</tr>
	<tr>
		<td>bigqmin</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>minimum fusion gain Q (constraint equation 28)</td>
	</tr>
	<tr>
		<td>bmxlim</td>
		<td>real</td>
		<td>12.0D0</td>
		<td>maximum peak toroidal field (T) (constraint equation 25)</td>
	</tr>
	<tr>
		<td>fauxmn</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for minimum auxiliary power (constraint equation 40, iteration variable 64)</td>
	</tr>
	<tr>
		<td>fbeta</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for epsilon beta-poloidal (constraint equation 6, iteration variable 8)</td>
	</tr>
	<tr>
		<td>fbetap</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for poloidal beta (constraint equation 48, iteration variable 79)</td>
	</tr>
	<tr>
		<td>fbetatry</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for beta limit (constraint equation 24, iteration variable 36)</td>
	</tr>
	<tr>
		<td>fbetatry_lower</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for (lower) beta limit (constraint equation 84, iteration variable 173)</td>
	</tr>
	<tr>
		<td>fcpttf</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for TF coil current per turn upper limit<br> (constraint equation 77, iteration variable 146)</td>
	</tr>
	<tr>
		<td>fcqt</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>TF coil quench temparature remains below tmax_croco <br> (constraint equation 74, iteration variable 141)</td>
	</tr>
	<tr>
		<td>fcwr</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for conducting wall radius / rminor limit <br> (constraint equation 23, iteration variable 104)</td>
	</tr>
	<tr>
		<td>fdene</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for density limit (constraint equation 5, iteration variable 9)<br> (invalid if ipedestal=3)</td>
	</tr>
	<tr>
		<td>fdivcol</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for divertor collisionality (constraint equation 22, iteration variable 34)</td>
	</tr>
	<tr>
		<td>fdtmp</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for first wall coolant temperature rise <br> (constraint equation 38, iteration variable 62)</td>
	</tr>
	<tr>
		<td>fflutf</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for neutron fluence on TF coil (constraint equation 53, iteration variable 92)</td>
	</tr>
	<tr>
		<td>ffuspow</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum fusion power (constraint equation 9, iteration variable 26)</td>
	</tr>
	<tr>
		<td>fgamcd</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for current drive gamma (constraint equation 37, iteration variable 40)</td>
	</tr>
	<tr>
		<td>fhldiv</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for divertor heat load (constraint equation 18, iteration variable 27)</td>
	</tr>
	<tr>
		<td>fiooic</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>f-value for TF coil operating current / critical current ratio<br> (constraint equation 33, iteration variable 50)</td>
	</tr>
	<tr>
		<td>fipir</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for Ip/Irod limit (constraint equation 46, iteration variable 72)</td>
	</tr>
	<tr>
		<td>fjohc</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for central solenoid current at end-of-flattop<br> (constraint equation 26, iteration variable 38)</td>
	</tr>
	<tr>
		<td>fjohc0</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for central solenoid current at beginning of pulse<br> (constraint equation 27, iteration variable 39)</td>
	</tr>
	<tr>
		<td>fjprot</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for TF coil winding pack current density <br> (constraint equation 35, iteration variable 53)</td>
	</tr>
	<tr>
		<td>flhthresh</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for L-H power threshold (constraint equation 15, iteration variable 103)</td>
	</tr>
	<tr>
		<td>fmva</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum MVA (constraint equation 19, iteration variable 30)</td>
	</tr>
	<tr>
		<td>fnbshinef</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum neutral beam shine-through fraction <br> (constraint equation 59, iteration variable 105)</td>
	</tr>
	<tr>
		<td>fnesep</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for Eich critical separatrix density <br> (constraint equation 76, iteration variable 144)</td>
	</tr>
	<tr>
		<td>fniterpump</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for constraint that number of pumps &lt; tfno <br> (constraint equation 63, iteration variable 111)</td>
	</tr>
	<tr>
		<td>foh_stress</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for Tresca stress in Central Solenoid<br> (constraint equation 72, iteration variable 123)</td>
	</tr>
	<tr>
		<td>fpeakb</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum toroidal field (constraint equation 25, iteration variable 35)</td>
	</tr>
	<tr>
		<td>fpinj</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for injection power (constraint equation 30, iteration variable 46)</td>
	</tr>
	<tr>
		<td>fpnetel</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for net electric power (constraint equation 16, iteration variable 25)</td>
	</tr>
	<tr>
		<td>fpoloidalpower</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for constraint on rate of change of energy in poloidal field<br> (constraint equation 66, iteration variable 115)</td>
	</tr>
	<tr>
		<td>fportsz</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for neutral beam tangency radius limit<br> (constraint equation 20, iteration variable 33)</td>
	</tr>
	<tr>
		<td>fpsep</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value to ensure separatrix power is less than value from Kallenbach divertor<br> (Not required as constraint 69 is an equality)</td>
	</tr>
	<tr>
		<td>fpsepbqar</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum Psep*Bt/qAR limit (constraint equation 68, iteration variable 117)</td>
	</tr>
	<tr>
		<td>fpsepr</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum Psep/R limit (constraint equation 56, iteration variable 97)</td>
	</tr>
	<tr>
		<td>fptemp</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for peak centrepost temperature (constraint equation 44, iteration variable 68)</td>
	</tr>
	<tr>
		<td>fptfnuc</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum TF coil nuclear heating (constraint equation 54, iteration variable 95)</td>
	</tr>
	<tr>
		<td>fq</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for edge safety factor (constraint equation 45, iteration variable 71)</td>
	</tr>
	<tr>
		<td>fqval</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for Q (constraint equation 28, iteration variable 45)</td>
	</tr>
	<tr>
		<td>fradpwr</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for core radiation power limit (constraint equation 17, iteration variable 28)</td>
	</tr>
	<tr>
		<td>fradwall</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for upper limit on radiation wall load (constr. equ. 67, iteration variable 116)</td>
	</tr>
	<tr>
		<td>freinke</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for Reinke detachment criterion (constr. equ. 78, iteration variable 147)</td>
	</tr>
	<tr>
		<td>frminor</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for minor radius limit (constraint equation 21, iteration variable 32)</td>
	</tr>
	<tr>
		<td>fstrcase</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum TF coil case TRESCA stress <br> (constraint equation 31, iteration variable 48)</td>
	</tr>
	<tr>
		<td>fstrcond</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maxiumum TF coil conduit TRESCA stress<br> (constraint equation 32, iteration variable 49)</td>
	</tr>
	<tr>
		<td>ftaucq</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for calculated minimum TF quench time <br> (constraint equation 65, iteration variable 113)</td>
	</tr>
	<tr>
		<td>ftaulimit</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for lower limit on taup/taueff the ratio of alpha particle to energy <br> confinement times (constraint equation 62, iteration variable 110)</td>
	</tr>
	<tr>
		<td>ftbr</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for minimum tritium breeding ratio (constraint equation 52, iteration variable 89)</td>
	</tr>
	<tr>
		<td>ftburn</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for minimum burn time (constraint equation 13, iteration variable 21)</td>
	</tr>
	<tr>
		<td>ftcycl</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for cycle time (constraint equation 42, iteration variable 67)</td>
	</tr>
	<tr>
		<td>ftmargoh</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for central solenoid temperature margin<br> (constraint equation 60, iteration variable 106)</td>
	</tr>
	<tr>
		<td>ftmargtf</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for TF coil temperature margin (constraint equation 36, iteration variable 54)</td>
	</tr>
	<tr>
		<td>ftohs</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for plasma current ramp-up time (constraint equation 41, iteration variable 66)</td>
	</tr>
	<tr>
		<td>ftpeak</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for first wall peak temperature (constraint equation 39, iteration variable 63)</td>
	</tr>
	<tr>
		<td>fvdump</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for dump voltage (constraint equation 34, iteration variable 51)</td>
	</tr>
	<tr>
		<td>fvs</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for flux-swing (V-s) requirement (STEADY STATE)<br> (constraint equation 12, iteration variable 15)</td>
	</tr>
	<tr>
		<td>fvvhe</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for vacuum vessel He concentration limit (iblanket = 2)<br> (constraint equation 55, iteration variable 96)</td>
	</tr>
	<tr>
		<td>fwalld</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum wall load (constraint equation 8, iteration variable 14)</td>
	</tr>
	<tr>
		<td>fzeffmax</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum zeff (constraint equation 64, iteration variable 112)</td>
	</tr>
	<tr>
		<td>gammax</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>maximum current drive gamma (constraint equation 37)</td>
	</tr>
	<tr>
		<td>maxradwallload</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Maximum permitted radiation wall load (MW/m^2) (constraint equation 67)</td>
	</tr>
	<tr>
		<td>mvalim</td>
		<td>real</td>
		<td>40.0D0</td>
		<td>maximum MVA limit (constraint equation 19)</td>
	</tr>
	<tr>
		<td>nbshinefmax</td>
		<td>real</td>
		<td>1.0D-3</td>
		<td>maximum neutral beam shine-through fraction (constraint equation 59)</td>
	</tr>
	<tr>
		<td>nflutfmax</td>
		<td>real</td>
		<td>1.0D23</td>
		<td>max fast neutron fluence on TF coil (n/m2) (blktmodel&gt;0) (constraint equation 53)<br> Aslo used for demontable magnets (itart = 1) and superdonducting coils (i_tf_sup = 1)<br> To set the CP lifetime</td>
	</tr>
	<tr>
		<td>pdivtlim</td>
		<td>real</td>
		<td>150.0D0</td>
		<td>Minimum pdivt [MW] (constraint equation 80)</td>
	</tr>
	<tr>
		<td>peakfactrad</td>
		<td>real</td>
		<td>3.33D0</td>
		<td>peaking factor for radiation wall load (constraint equation 67)</td>
	</tr>
	<tr>
		<td>peakradwallload</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Peak radiation wall load (MW/m^2) (constraint equation 67)</td>
	</tr>
	<tr>
		<td>pnetelin</td>
		<td>real</td>
		<td>1.0D3</td>
		<td>required net electric power (MW) (constraint equation 16)</td>
	</tr>
	<tr>
		<td>powfmax</td>
		<td>real</td>
		<td>1.5D3</td>
		<td>maximum fusion power (MW) (constraint equation 9)</td>
	</tr>
	<tr>
		<td>psepbqarmax</td>
		<td>real</td>
		<td>9.5D0</td>
		<td>maximum ratio of Psep*Bt/qAR (MWT/m) (constraint equation 68)</td>
	</tr>
	<tr>
		<td>pseprmax</td>
		<td>real</td>
		<td>25.0D0</td>
		<td>maximum ratio of power crossing the separatrix to plasma major radius (Psep/R) (MW/m)<br> (constraint equation 56)</td>
	</tr>
	<tr>
		<td>ptfnucmax</td>
		<td>real</td>
		<td>1.0D-3</td>
		<td>maximum nuclear heating in TF coil (MW/m3) (constraint equation 54)</td>
	</tr>
	<tr>
		<td>taulimit</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>Lower limit on taup/taueff the ratio of alpha particle to energy confinement <br> times (constraint equation 62)</td>
	</tr>
	<tr>
		<td>tbrmin</td>
		<td>real</td>
		<td>1.1D0</td>
		<td>minimum tritium breeding ratio (constraint equation 52)</td>
	</tr>
	<tr>
		<td>tbrnmn</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>minimum burn time (s) (KE - no longer itv., see issue #706)</td>
	</tr>
	<tr>
		<td>tcycmn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>minimum cycle time (s) (constraint equation 42)</td>
	</tr>
	<tr>
		<td>tohsmn</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>minimum plasma current ramp-up time (s) (constraint equation 41)</td>
	</tr>
	<tr>
		<td>vvhealw</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>allowed maximum helium concentration in vacuum vessel at end of plant life (appm)<br> (iblanket =2) (constraint equation 55)</td>
	</tr>
	<tr>
		<td>walalw</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>allowable wall-load (MW/m2) (constraint equation 8)</td>
	</tr>
	<tr>
		<td>zeffmax</td>
		<td>real</td>
		<td>3.6D0</td>
		<td>maximum value for Zeff (constraint equation 64)</td>
	</tr>
</table>

## cost_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>abktflnc</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>allowable first wall/blanket neutron fluence (MW-yr/m2) (blktmodel=0)</td>
	</tr>
	<tr>
		<td>adivflnc</td>
		<td>real</td>
		<td>7.0D0</td>
		<td>allowable divertor heat fluence (MW-yr/m2)</td>
	</tr>
	<tr>
		<td>amortization</td>
		<td>real</td>
		<td>13.6D0</td>
		<td>amortization factor (fixed charge factor) "A" (years)</td>
	</tr>
	<tr>
		<td>avail_min</td>
		<td>real</td>
		<td>0.75D0</td>
		<td>Minimum availability (constraint equation 61)</td>
	</tr>
	<tr>
		<td>blkcst</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>blanket direct cost (M\$)</td>
	</tr>
	<tr>
		<td>c221</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total account 221 cost (M\$) - first wall, blanket, shield, support structure and div plates</td>
	</tr>
	<tr>
		<td>c222</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total account 222 cost (M\$) - TF coils + PF coils</td>
	</tr>
	<tr>
		<td>capcost</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total capital cost including interest (M\$)</td>
	</tr>
	<tr>
		<td>cconfix</td>
		<td>real</td>
		<td>80.0D0</td>
		<td>fixed cost of superconducting cable (\$/m)</td>
	</tr>
	<tr>
		<td>cconshpf</td>
		<td>real</td>
		<td>70.0D0</td>
		<td>cost of PF coil steel conduit/sheath (\$/m)</td>
	</tr>
	<tr>
		<td>cconshtf</td>
		<td>real</td>
		<td>75.0D0</td>
		<td>cost of TF coil steel conduit/sheath (\$/m)</td>
	</tr>
	<tr>
		<td>cdcost</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>current drive direct costs (M\$)</td>
	</tr>
	<tr>
		<td>cdirt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total plant direct cost (M\$)</td>
	</tr>
	<tr>
		<td>cdrlife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>lifetime of heating/current drive system (y)</td>
	</tr>
	<tr>
		<td>cfactr</td>
		<td>real</td>
		<td>0.75D0</td>
		<td>Total plant availability fraction; input if iavail=0</td>
	</tr>
	<tr>
		<td>cfind</td>
		<td>real</td>
		<td>(/0.244D0, 0.244D0, 0.244D0, 0...</td>
		<td>indirect cost factor (func of lsa)</td>
	</tr>
	<tr>
		<td>cland</td>
		<td>real</td>
		<td>19.2D0</td>
		<td>cost of land (M\$)</td>
	</tr>
	<tr>
		<td>coe</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>cost of electricity (\$/MW-hr)</td>
	</tr>
	<tr>
		<td>coecap</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>capital cost of electricity (m\$/kW-hr)</td>
	</tr>
	<tr>
		<td>coefuelt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>'fuel' (including replaceable components) contribution to cost of electricity (m\$/kW-hr)</td>
	</tr>
	<tr>
		<td>coeoam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>operation and maintenance contribution to cost of electricity (m\$/kW-hr)</td>
	</tr>
	<tr>
		<td>concost</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plant construction cost (M\$)</td>
	</tr>
	<tr>
		<td>conf_mag</td>
		<td>real</td>
		<td>0.99D0</td>
		<td>c parameter, which determines the temperature margin at which magnet lifetime starts to decline</td>
	</tr>
	<tr>
		<td>cost_factor_bop</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for energy conversion system</td>
	</tr>
	<tr>
		<td>cost_factor_buildings</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for buildings</td>
	</tr>
	<tr>
		<td>cost_factor_fwbs</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for fwbs</td>
	</tr>
	<tr>
		<td>cost_factor_land</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for land</td>
	</tr>
	<tr>
		<td>cost_factor_misc</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for remaining subsystems</td>
	</tr>
	<tr>
		<td>cost_factor_rh</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for remote handling</td>
	</tr>
	<tr>
		<td>cost_factor_tf_coils</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for TF coils</td>
	</tr>
	<tr>
		<td>cost_factor_vv</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost scaling factor for vacuum vessel</td>
	</tr>
	<tr>
		<td>cost_model</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for cost model:<br><ul><li> =0 use \$ 1990 PROCESS model</li><li> =1 use \$ 2014 Kovari model</li><li> =2 use \$ 1980 STEP model (NOT RECOMMENDED - Under Development)</li></ul></td>
	</tr>
	<tr>
		<td>costexp</td>
		<td>real</td>
		<td>0.8D0</td>
		<td>cost exponent for scaling in 2015 costs model</td>
	</tr>
	<tr>
		<td>costexp_pebbles</td>
		<td>real</td>
		<td>0.6D0</td>
		<td>cost exponent for pebbles in 2015 costs model</td>
	</tr>
	<tr>
		<td>cowner</td>
		<td>real</td>
		<td>0.15D0</td>
		<td>owner cost factor</td>
	</tr>
	<tr>
		<td>cpfact</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Total plant capacity factor</td>
	</tr>
	<tr>
		<td>cplife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Calculated full power year lifetime of centrepost (years)</td>
	</tr>
	<tr>
		<td>cplife_input</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>User input full power year lifetime of the centrepost (years)</td>
	</tr>
	<tr>
		<td>cpstcst</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ST centrepost direct cost (M\$)</td>
	</tr>
	<tr>
		<td>cpstflnc</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>allowable ST centrepost neutron fluence (MW-yr/m2)</td>
	</tr>
	<tr>
		<td>crctcore</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>reactor core costs (categories 221, 222 and 223)</td>
	</tr>
	<tr>
		<td>csi</td>
		<td>real</td>
		<td>16.0D0</td>
		<td>allowance for site costs (M\$)</td>
	</tr>
	<tr>
		<td>cturbb</td>
		<td>real</td>
		<td>38.0D0</td>
		<td>cost of turbine building (M\$)</td>
	</tr>
	<tr>
		<td>decomf</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>proportion of constructed cost required for decommissioning fund</td>
	</tr>
	<tr>
		<td>dintrt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>diff between borrowing and saving interest rates</td>
	</tr>
	<tr>
		<td>div_nref</td>
		<td>real</td>
		<td>7000.0D0</td>
		<td>Reference value for cycle cycle life of divertor</td>
	</tr>
	<tr>
		<td>div_nu</td>
		<td>real</td>
		<td>14000.0D0</td>
		<td>The cycle when the divertor fails with 100% probability</td>
	</tr>
	<tr>
		<td>div_prob_fail</td>
		<td>real</td>
		<td>0.0002D0</td>
		<td>Divertor probability of failure (per op day)</td>
	</tr>
	<tr>
		<td>div_umain_time</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>Divertor unplanned maintenance time (years)</td>
	</tr>
	<tr>
		<td>divcst</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>divertor direct cost (M\$)</td>
	</tr>
	<tr>
		<td>divlife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Full power lifetime of divertor (y)</td>
	</tr>
	<tr>
		<td>dtlife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>period prior to the end of the plant life that the decommissioning fund is used (years)</td>
	</tr>
	<tr>
		<td>favail</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>F-value for minimum availability (constraint equation 61)</td>
	</tr>
	<tr>
		<td>fcap0</td>
		<td>real</td>
		<td>1.165D0</td>
		<td>average cost of money for construction of plant assuming design/construction time of six years</td>
	</tr>
	<tr>
		<td>fcap0cp</td>
		<td>real</td>
		<td>1.08D0</td>
		<td>average cost of money for replaceable components assuming lead time for these of two years</td>
	</tr>
	<tr>
		<td>fcdfuel</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>fraction of current drive cost treated as fuel (if ifueltyp = 1)</td>
	</tr>
	<tr>
		<td>fcontng</td>
		<td>real</td>
		<td>0.195D0</td>
		<td>project contingency factor</td>
	</tr>
	<tr>
		<td>fcr0</td>
		<td>real</td>
		<td>0.0966D0</td>
		<td>fixed charge rate during construction</td>
	</tr>
	<tr>
		<td>fkind</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>multiplier for Nth of a kind costs</td>
	</tr>
	<tr>
		<td>fwallcst</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall cost (M\$)</td>
	</tr>
	<tr>
		<td>fwbs_nref</td>
		<td>real</td>
		<td>20000.0D0</td>
		<td>Reference value for cycle life of blanket</td>
	</tr>
	<tr>
		<td>fwbs_nu</td>
		<td>real</td>
		<td>40000.0D0</td>
		<td>The cycle when the blanket fails with 100% probability</td>
	</tr>
	<tr>
		<td>fwbs_prob_fail</td>
		<td>real</td>
		<td>0.0002D0</td>
		<td>Fwbs probability of failure (per op day)</td>
	</tr>
	<tr>
		<td>fwbs_umain_time</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>Fwbs unplanned maintenance time (years)</td>
	</tr>
	<tr>
		<td>i_cp_lifetime</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for the centrepost liftime constraint <br>  0 : The CP full power yearlifelime is set by the user<br>  1 : The CP lifelime is equal to the divertor one<br>  2 : The CP lifetime is equal to the breeding blankets one<br>  3 : The CP lifetime is equal to the plant one</td>
	</tr>
	<tr>
		<td>iavail</td>
		<td>integer</td>
		<td>2</td>
		<td>Switch for plant availability model:<br><ul><li> =0 use input value for cfactr</li><li> =1 calculate cfactr using Taylor and Ward 1999 model</li><li> =2 calculate cfactr using new (2015) model</li></ul></td>
	</tr>
	<tr>
		<td>ifueltyp</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for fuel type:<br><ul><li> =2 treat initial blanket, divertor, first wall<br>   as capital costs. Treat all later items and <br>   fraction fcdfuel of CD equipment as fuel costs</li><li> =1 treat blanket divertor, first wall and<br>   fraction fcdfuel of CD equipment as fuel cost</li><li> =0 treat these as capital cost</li></ul></td>
	</tr>
	<tr>
		<td>ipnet</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for net electric power calculation:<br><ul><li> =0 scale so that always &gt; 0</li><li> =1 let go &lt; 0 (no c-o-e)</li></ul></td>
	</tr>
	<tr>
		<td>ireactor</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for net electric power and cost of electricity calculations:<br><ul><li> =0 do not calculate MW(electric) or c-o-e</li><li> =1 calculate MW(electric) and c-o-e</li></ul></td>
	</tr>
	<tr>
		<td>light_build_cost_per_vol</td>
		<td>real</td>
		<td>270.0D0</td>
		<td>Unit cost for unshielded non-active buildings (\$/m3)</td>
	</tr>
	<tr>
		<td>lsa</td>
		<td>integer</td>
		<td>4</td>
		<td>Level of safety assurance switch (generally, use 3 or 4):<br><ul><li> =1 truly passively safe plant</li><li> =2,3 in-between</li><li> =4 like current fission plant</li></ul></td>
	</tr>
	<tr>
		<td>maintenance_fwbs</td>
		<td>real</td>
		<td>0.2D0</td>
		<td>Maintenance cost factor: first wall, blanket, shield, divertor</td>
	</tr>
	<tr>
		<td>maintenance_gen</td>
		<td>real</td>
		<td>0.05D0</td>
		<td>Maintenance cost factor: All other components except coils, vacuum vessel, <br> thermal shield, cryostat, land</td>
	</tr>
	<tr>
		<td>moneyint</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>interest portion of capital cost (M\$)</td>
	</tr>
	<tr>
		<td>num_rh_systems</td>
		<td>integer</td>
		<td>4</td>
		<td>Number of remote handling systems (1-10)</td>
	</tr>
	<tr>
		<td>output_costs</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for costs output:<br><ul><li> =0 do not write cost-related outputs to file</li><li> =1 write cost-related outputs to file</li></ul></td>
	</tr>
	<tr>
		<td>ratecdol</td>
		<td>real</td>
		<td>0.0435D0</td>
		<td>effective cost of money in constant dollars</td>
	</tr>
	<tr>
		<td>redun_vac</td>
		<td>integer</td>
		<td>0</td>
		<td>Number of redundant vacuum pumps</td>
	</tr>
	<tr>
		<td>redun_vacp</td>
		<td>real</td>
		<td>25.0D0</td>
		<td>Vacuum system pump redundancy level (%)</td>
	</tr>
	<tr>
		<td>step_con</td>
		<td>real</td>
		<td>1.5D-1</td>
		<td>Contingency Percentage</td>
	</tr>
	<tr>
		<td>step_ref</td>
		<td>real</td>
		<td>(/3.0D0, 3.0D-1, 1.115D1, 1.57...</td>
		<td>Reference values for cost model 2</td>
	</tr>
	<tr>
		<td>t_operation</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Operational time (yrs)</td>
	</tr>
	<tr>
		<td>tbktrepl</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>time taken to replace blanket (y) (iavail=1)</td>
	</tr>
	<tr>
		<td>tcomrepl</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>time taken to replace both blanket and divertor (y) (iavail=1)</td>
	</tr>
	<tr>
		<td>tdivrepl</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>time taken to replace divertor (y) (iavail=1)</td>
	</tr>
	<tr>
		<td>tlife</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>Full power year plant lifetime (years)</td>
	</tr>
	<tr>
		<td>tok_build_cost_per_vol</td>
		<td>real</td>
		<td>1283.0D0</td>
		<td>Unit cost for tokamak complex buildings, including building and site services (\$/m3)</td>
	</tr>
	<tr>
		<td>ucad</td>
		<td>real</td>
		<td>180.0D0</td>
		<td>unit cost for administration buildings (M\$/m3)</td>
	</tr>
	<tr>
		<td>ucaf</td>
		<td>real</td>
		<td>1.5D6</td>
		<td>unit cost for aux facility power equipment (\$)</td>
	</tr>
	<tr>
		<td>ucahts</td>
		<td>real</td>
		<td>31.0D0</td>
		<td>unit cost for aux heat transport equipment (\$/W**exphts)</td>
	</tr>
	<tr>
		<td>ucap</td>
		<td>real</td>
		<td>17.0D0</td>
		<td>unit cost of auxiliary transformer (\$/kVA)</td>
	</tr>
	<tr>
		<td>ucblbe</td>
		<td>real</td>
		<td>260.0D0</td>
		<td>unit cost for blanket beryllium (\$/kg)</td>
	</tr>
	<tr>
		<td>ucblbreed</td>
		<td>real</td>
		<td>875.0D0</td>
		<td>unit cost for breeder material (\$/kg) (blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>ucblli</td>
		<td>real</td>
		<td>875.0D0</td>
		<td>unit cost for blanket lithium (\$/kg) (30% Li6)</td>
	</tr>
	<tr>
		<td>ucblli2o</td>
		<td>real</td>
		<td>600.0D0</td>
		<td>unit cost for blanket Li_2O (\$/kg)</td>
	</tr>
	<tr>
		<td>ucbllipb</td>
		<td>real</td>
		<td>10.3D0</td>
		<td>unit cost for blanket Li-Pb (\$/kg) (30% Li6)</td>
	</tr>
	<tr>
		<td>ucblss</td>
		<td>real</td>
		<td>90.0D0</td>
		<td>unit cost for blanket stainless steel (\$/kg)</td>
	</tr>
	<tr>
		<td>ucblvd</td>
		<td>real</td>
		<td>200.0D0</td>
		<td>unit cost for blanket vanadium (\$/kg)</td>
	</tr>
	<tr>
		<td>ucbpmp</td>
		<td>real</td>
		<td>2.925D5</td>
		<td>vacuum system backing pump cost (\$)</td>
	</tr>
	<tr>
		<td>ucbus</td>
		<td>real</td>
		<td>0.123D0</td>
		<td>cost of aluminium bus for TF coil (\$/A-m)</td>
	</tr>
	<tr>
		<td>uccase</td>
		<td>real</td>
		<td>50.0D0</td>
		<td>cost of superconductor case (\$/kg)</td>
	</tr>
	<tr>
		<td>ucco</td>
		<td>real</td>
		<td>350.0D0</td>
		<td>unit cost for control buildings (M\$/m3)</td>
	</tr>
	<tr>
		<td>uccpcl1</td>
		<td>real</td>
		<td>250.0D0</td>
		<td>cost of high strength tapered copper (\$/kg)</td>
	</tr>
	<tr>
		<td>uccpclb</td>
		<td>real</td>
		<td>150.0D0</td>
		<td>cost of TF outboard leg plate coils (\$/kg)</td>
	</tr>
	<tr>
		<td>uccpmp</td>
		<td>real</td>
		<td>3.9D5</td>
		<td>vacuum system cryopump cost (\$)</td>
	</tr>
	<tr>
		<td>uccr</td>
		<td>real</td>
		<td>460.0D0</td>
		<td>unit cost for cryogenic building (M\$/vol)</td>
	</tr>
	<tr>
		<td>uccry</td>
		<td>real</td>
		<td>9.3D4</td>
		<td>heat transport system cryoplant costs (\$/W**expcry)</td>
	</tr>
	<tr>
		<td>uccryo</td>
		<td>real</td>
		<td>32.0D0</td>
		<td>unit cost for vacuum vessel (\$/kg)</td>
	</tr>
	<tr>
		<td>uccu</td>
		<td>real</td>
		<td>75.0D0</td>
		<td>unit cost for copper in superconducting cable (\$/kg)</td>
	</tr>
	<tr>
		<td>ucdgen</td>
		<td>real</td>
		<td>1.7D6</td>
		<td>cost per 8 MW diesel generator (\$)</td>
	</tr>
	<tr>
		<td>ucdiv</td>
		<td>real</td>
		<td>2.8D5</td>
		<td>cost of divertor blade (\$)</td>
	</tr>
	<tr>
		<td>ucdtc</td>
		<td>real</td>
		<td>13.0D0</td>
		<td>detritiation, air cleanup cost (\$/10000m3/hr)</td>
	</tr>
	<tr>
		<td>ucduct</td>
		<td>real</td>
		<td>4.225D4</td>
		<td>vacuum system duct cost (\$/m)</td>
	</tr>
	<tr>
		<td>ucech</td>
		<td>real</td>
		<td>3.0D0</td>
		<td>ECH system cost (\$/W)</td>
	</tr>
	<tr>
		<td>ucel</td>
		<td>real</td>
		<td>380.0D0</td>
		<td>unit cost for electrical equipment building (M\$/m3)</td>
	</tr>
	<tr>
		<td>uces1</td>
		<td>real</td>
		<td>3.2D4</td>
		<td>MGF (motor-generator flywheel) cost factor (\$/MVA**0.8)</td>
	</tr>
	<tr>
		<td>uces2</td>
		<td>real</td>
		<td>8.8D3</td>
		<td>MGF (motor-generator flywheel) cost factor (\$/MJ**0.8)</td>
	</tr>
	<tr>
		<td>ucf1</td>
		<td>real</td>
		<td>2.23D7</td>
		<td>cost of fuelling system (\$)</td>
	</tr>
	<tr>
		<td>ucfnc</td>
		<td>real</td>
		<td>35.0D0</td>
		<td>outer PF coil fence support cost (\$/kg)</td>
	</tr>
	<tr>
		<td>ucfpr</td>
		<td>real</td>
		<td>4.4D7</td>
		<td>cost of 60g/day tritium processing unit (\$)</td>
	</tr>
	<tr>
		<td>ucfuel</td>
		<td>real</td>
		<td>3.45D0</td>
		<td>unit cost of D-T fuel (M\$/year/1200MW)</td>
	</tr>
	<tr>
		<td>ucfwa</td>
		<td>real</td>
		<td>6.0D4</td>
		<td>first wall armour cost (\$/m2)</td>
	</tr>
	<tr>
		<td>ucfwps</td>
		<td>real</td>
		<td>1.0D7</td>
		<td>first wall passive stabiliser cost (\$)</td>
	</tr>
	<tr>
		<td>ucfws</td>
		<td>real</td>
		<td>5.3D4</td>
		<td>first wall structure cost (\$/m2)</td>
	</tr>
	<tr>
		<td>ucgss</td>
		<td>real</td>
		<td>35.0D0</td>
		<td>cost of reactor structure (\$/kg)</td>
	</tr>
	<tr>
		<td>uche3</td>
		<td>real</td>
		<td>1.0D6</td>
		<td>cost of helium-3 (\$/kg)</td>
	</tr>
	<tr>
		<td>uchrs</td>
		<td>real</td>
		<td>87.9D6</td>
		<td>cost of heat rejection system (\$)</td>
	</tr>
	<tr>
		<td>uchts</td>
		<td>real</td>
		<td>(/15.3D0, 19.1D0/)</td>
		<td>cost of heat transport system equipment per loop (\$/W); dependent on coolant type (coolwh)</td>
	</tr>
	<tr>
		<td>uciac</td>
		<td>real</td>
		<td>1.5D8</td>
		<td>cost of instrumentation, control &amp; diagnostics (\$)</td>
	</tr>
	<tr>
		<td>ucich</td>
		<td>real</td>
		<td>3.0D0</td>
		<td>ICH system cost (\$/W)</td>
	</tr>
	<tr>
		<td>ucint</td>
		<td>real</td>
		<td>35.0D0</td>
		<td>superconductor intercoil structure cost (\$/kg)</td>
	</tr>
	<tr>
		<td>uclh</td>
		<td>real</td>
		<td>3.3D0</td>
		<td>lower hybrid system cost (\$/W)</td>
	</tr>
	<tr>
		<td>uclv</td>
		<td>real</td>
		<td>16.0D0</td>
		<td>low voltage system cost (\$/kVA)</td>
	</tr>
	<tr>
		<td>ucmb</td>
		<td>real</td>
		<td>260.0D0</td>
		<td>unit cost for reactor maintenance building (M\$/m3)</td>
	</tr>
	<tr>
		<td>ucme</td>
		<td>real</td>
		<td>1.25D8</td>
		<td>cost of maintenance equipment (\$)</td>
	</tr>
	<tr>
		<td>ucmisc</td>
		<td>real</td>
		<td>2.5D7</td>
		<td>miscellaneous plant allowance (\$)</td>
	</tr>
	<tr>
		<td>ucnbi</td>
		<td>real</td>
		<td>3.3D0</td>
		<td>NBI system cost (\$/W)</td>
	</tr>
	<tr>
		<td>ucnbv</td>
		<td>real</td>
		<td>1000.0D0</td>
		<td>cost of nuclear building ventilation (\$/m3)</td>
	</tr>
	<tr>
		<td>ucoam</td>
		<td>real</td>
		<td>(/68.8D0, 68.8D0, 68.8D0, 74.4...</td>
		<td>annual cost of operation and maintenance (M\$/year/1200MW**0.5)</td>
	</tr>
	<tr>
		<td>ucpens</td>
		<td>real</td>
		<td>32.0D0</td>
		<td>penetration shield cost (\$/kg)</td>
	</tr>
	<tr>
		<td>ucpfb</td>
		<td>real</td>
		<td>210.0D0</td>
		<td>cost of PF coil buses (\$/kA-m)</td>
	</tr>
	<tr>
		<td>ucpfbk</td>
		<td>real</td>
		<td>1.66D4</td>
		<td>cost of PF coil DC breakers (\$/MVA**0.7)</td>
	</tr>
	<tr>
		<td>ucpfbs</td>
		<td>real</td>
		<td>4.9D3</td>
		<td>cost of PF burn power supplies (\$/kW**0.7)</td>
	</tr>
	<tr>
		<td>ucpfcb</td>
		<td>real</td>
		<td>7.5D4</td>
		<td>cost of PF coil AC breakers (\$/circuit)</td>
	</tr>
	<tr>
		<td>ucpfdr1</td>
		<td>real</td>
		<td>150.0D0</td>
		<td>cost factor for dump resistors (\$/MJ)</td>
	</tr>
	<tr>
		<td>ucpfic</td>
		<td>real</td>
		<td>1.0D4</td>
		<td>cost of PF instrumentation and control (\$/channel)</td>
	</tr>
	<tr>
		<td>ucpfps</td>
		<td>real</td>
		<td>3.5D4</td>
		<td>cost of PF coil pulsed power supplies (\$/MVA)</td>
	</tr>
	<tr>
		<td>ucphx</td>
		<td>real</td>
		<td>15.0D0</td>
		<td>primary heat transport cost (\$/W**exphts)</td>
	</tr>
	<tr>
		<td>ucpp</td>
		<td>real</td>
		<td>48.0D0</td>
		<td>cost of primary power transformers (\$/kVA**0.9)</td>
	</tr>
	<tr>
		<td>ucrb</td>
		<td>real</td>
		<td>400.0D0</td>
		<td>cost of reactor building (M\$/m3)</td>
	</tr>
	<tr>
		<td>ucsc</td>
		<td>real</td>
		<td>(/600.0D0, 600.0D0, 300.0D0, 6...</td>
		<td>cost of superconductor (\$/kg)</td>
	</tr>
	<tr>
		<td>ucsh</td>
		<td>real</td>
		<td>115.0D0</td>
		<td>cost of shops and warehouses (M\$/m3)</td>
	</tr>
	<tr>
		<td>ucshld</td>
		<td>real</td>
		<td>32.0D0</td>
		<td>cost of shield structural steel (\$/kg)</td>
	</tr>
	<tr>
		<td>ucswyd</td>
		<td>real</td>
		<td>1.84D7</td>
		<td>switchyard equipment costs (\$)</td>
	</tr>
	<tr>
		<td>uctfbr</td>
		<td>real</td>
		<td>1.22D0</td>
		<td>cost of TF coil breakers (\$/W**0.7)</td>
	</tr>
	<tr>
		<td>uctfbus</td>
		<td>real</td>
		<td>100.0D0</td>
		<td>cost of TF coil bus (\$/kg)</td>
	</tr>
	<tr>
		<td>uctfdr</td>
		<td>real</td>
		<td>1.75D-4</td>
		<td>cost of TF coil dump resistors (\$/J)</td>
	</tr>
	<tr>
		<td>uctfgr</td>
		<td>real</td>
		<td>5000.0D0</td>
		<td>additional cost of TF coil dump resistors (\$/coil)</td>
	</tr>
	<tr>
		<td>uctfic</td>
		<td>real</td>
		<td>1.0D4</td>
		<td>cost of TF coil instrumentation and control (\$/coil/30)</td>
	</tr>
	<tr>
		<td>uctfps</td>
		<td>real</td>
		<td>24.0D0</td>
		<td>cost of TF coil power supplies (\$/W**0.7)</td>
	</tr>
	<tr>
		<td>uctfsw</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>cost of TF coil slow dump switches (\$/A)</td>
	</tr>
	<tr>
		<td>uctpmp</td>
		<td>real</td>
		<td>1.105D5</td>
		<td>cost of turbomolecular pump (\$)</td>
	</tr>
	<tr>
		<td>uctr</td>
		<td>real</td>
		<td>370.0D0</td>
		<td>cost of tritium building (\$/m3)</td>
	</tr>
	<tr>
		<td>ucturb</td>
		<td>real</td>
		<td>(/230.0D6, 245.0D6/)</td>
		<td>cost of turbine plant equipment (\$) (dependent on coolant type coolwh)</td>
	</tr>
	<tr>
		<td>ucvalv</td>
		<td>real</td>
		<td>3.9D5</td>
		<td>vacuum system valve cost (\$)</td>
	</tr>
	<tr>
		<td>ucvdsh</td>
		<td>real</td>
		<td>26.0D0</td>
		<td>vacuum duct shield cost (\$/kg)</td>
	</tr>
	<tr>
		<td>ucviac</td>
		<td>real</td>
		<td>1.3D6</td>
		<td>vacuum system instrumentation and control cost (\$)</td>
	</tr>
	<tr>
		<td>ucwindpf</td>
		<td>real</td>
		<td>465.0D0</td>
		<td>cost of PF coil superconductor windings (\$/m)</td>
	</tr>
	<tr>
		<td>ucwindtf</td>
		<td>real</td>
		<td>480.0D0</td>
		<td>cost of TF coil superconductor windings (\$/m)</td>
	</tr>
	<tr>
		<td>ucws</td>
		<td>real</td>
		<td>460.0D0</td>
		<td>cost of active assembly shop (\$/m3)</td>
	</tr>
	<tr>
		<td>ucwst</td>
		<td>real</td>
		<td>(/0.0D0, 3.94D0, 5.91D0, 7.88D...</td>
		<td>cost of waste disposal (M\$/y/1200MW)</td>
	</tr>
	<tr>
		<td>uubop</td>
		<td>real</td>
		<td>0.02D0</td>
		<td>unplanned unavailability factor for balance of plant (iavail=1)</td>
	</tr>
	<tr>
		<td>uucd</td>
		<td>real</td>
		<td>0.02D0</td>
		<td>unplanned unavailability factor for current drive (iavail=1)</td>
	</tr>
	<tr>
		<td>uudiv</td>
		<td>real</td>
		<td>0.04D0</td>
		<td>unplanned unavailability factor for divertor (iavail=1)</td>
	</tr>
	<tr>
		<td>uufuel</td>
		<td>real</td>
		<td>0.02D0</td>
		<td>unplanned unavailability factor for fuel system (iavail=1)</td>
	</tr>
	<tr>
		<td>uufw</td>
		<td>real</td>
		<td>0.04D0</td>
		<td>unplanned unavailability factor for first wall (iavail=1)</td>
	</tr>
	<tr>
		<td>uumag</td>
		<td>real</td>
		<td>0.02D0</td>
		<td>unplanned unavailability factor for magnets (iavail=1)</td>
	</tr>
	<tr>
		<td>uuves</td>
		<td>real</td>
		<td>0.04D0</td>
		<td>unplanned unavailability factor for vessel (iavail=1)</td>
	</tr>
</table>

## costs_2015_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>annual_electric_output</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>double</td>
		<td>integer</td>
		<td>8</td>
		<td></td>
	</tr>
	<tr>
		<td>ip</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>maintenance</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mean_electric_output</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ofile</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>s</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>total_costs</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## costs_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>c21</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c211</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c212</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c213</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c214</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2141</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2142</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c215</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c216</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c217</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2171</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2172</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2173</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2174</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2211</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2212</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22121</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22122</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22123</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22124</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22125</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22126</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22127</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22128</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2213</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22131</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22132</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2214</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2215</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2221</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22211</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22212</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22213</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22214</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22215</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2222</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22221</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22222</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22223</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22224</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2223</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c223</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2231</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2232</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2233</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2234</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c224</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2241</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2242</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2243</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2244</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2245</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2246</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c225</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2251</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22511</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22512</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22513</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22514</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22515</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2252</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22521</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22522</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22523</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22524</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22525</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22526</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c22527</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2253</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c226</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2261</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2262</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2263</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c227</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2271</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2272</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2273</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c2274</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c228</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c229</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c23</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c24</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c241</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c242</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c243</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c244</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c245</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c25</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>c26</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ccont</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>chx</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>cindrt</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>cpp</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>cppa</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## costs_step_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>fwarea_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fwblkcost</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pinjmw_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pth</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ptherm_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rmajor_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rminor_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step20</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step21</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step22</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step23</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step24</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step25</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step91</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step92</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>step93</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vfi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vfi_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## current_drive_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>beamwd</td>
		<td>real</td>
		<td>0.58D0</td>
		<td>width of neutral beam duct where it passes between the TF coils (m)<br> T Inoue et al, Design of neutral beam system for ITER-FEAT, <br> <A HREF=http://dx.doi.org/10.1016/S0920-3796(01)00339-8><br> Fusion Engineering and Design, Volumes 56-57, October 2001, Pages 517-521)</td>
	</tr>
	<tr>
		<td>bigq</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Fusion gain; P_fusion / (P_injection + P_ohmic)</td>
	</tr>
	<tr>
		<td>bootipf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>bootstrap current fraction (enforced; see ibss)</td>
	</tr>
	<tr>
		<td>bscf_iter89</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>bootstrap current fraction, ITER 1989 model</td>
	</tr>
	<tr>
		<td>bscf_nevins</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>bootstrap current fraction, Nevins et al model</td>
	</tr>
	<tr>
		<td>bscf_sauter</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>bootstrap current fraction, Sauter et al model</td>
	</tr>
	<tr>
		<td>bscf_wilson</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>bootstrap current fraction, Wilson et al model</td>
	</tr>
	<tr>
		<td>bscfmax</td>
		<td>real</td>
		<td>0.9D0</td>
		<td>maximum fraction of plasma current from bootstrap; if bscfmax &lt; 0, <br> bootstrap fraction = abs(bscfmax)</td>
	</tr>
	<tr>
		<td>cboot</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>bootstrap current fraction multiplier (ibss=1)</td>
	</tr>
	<tr>
		<td>cnbeam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam current (A)</td>
	</tr>
	<tr>
		<td>diacf_hender</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>diamagnetic current fraction, Hender fit</td>
	</tr>
	<tr>
		<td>diacf_scene</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>diamagnetic current fraction, SCENE fit</td>
	</tr>
	<tr>
		<td>diaipf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>diamagnetic current fraction</td>
	</tr>
	<tr>
		<td>echpwr</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ECH power (MW)</td>
	</tr>
	<tr>
		<td>echwpow</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ECH wall plug power (MW)</td>
	</tr>
	<tr>
		<td>effcd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>current drive efficiency (A/W)</td>
	</tr>
	<tr>
		<td>enbeam</td>
		<td>real</td>
		<td>1.0D3</td>
		<td>neutral beam energy (keV) (iteration variable 19)</td>
	</tr>
	<tr>
		<td>etacd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>auxiliary power wall plug to injector efficiency</td>
	</tr>
	<tr>
		<td>etacdfix</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>secondary auxiliary power wall plug to injector efficiency</td>
	</tr>
	<tr>
		<td>etaech</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>ECH wall plug to injector efficiency</td>
	</tr>
	<tr>
		<td>etalh</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>lower hybrid wall plug to injector efficiency</td>
	</tr>
	<tr>
		<td>etanbi</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>neutral beam wall plug to injector efficiency</td>
	</tr>
	<tr>
		<td>feffcd</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>current drive efficiency fudge factor (iteration variable 47)</td>
	</tr>
	<tr>
		<td>forbitloss</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fraction of neutral beam power lost after ionisation but before <br> thermalisation (orbit loss fraction)</td>
	</tr>
	<tr>
		<td>fpion</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>fraction of beam energy to ions</td>
	</tr>
	<tr>
		<td>frbeam</td>
		<td>real</td>
		<td>1.05D0</td>
		<td>R_tangential / R_major for neutral beam injection</td>
	</tr>
	<tr>
		<td>ftritbm</td>
		<td>real</td>
		<td>1.0D-6</td>
		<td>fraction of beam that is tritium</td>
	</tr>
	<tr>
		<td>gamcd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>normalised current drive efficiency (1.0e20 A/(W m^2))</td>
	</tr>
	<tr>
		<td>gamma_ecrh</td>
		<td>real</td>
		<td>0.35D0</td>
		<td>User input ECRH gamma (1.0e20 A/(W m^2))</td>
	</tr>
	<tr>
		<td>iefrf</td>
		<td>integer</td>
		<td>5</td>
		<td>Switch for current drive efficiency model:<br><ul><li> =1 Fenstermacher Lower Hybrid</li><li> =2 Ion Cyclotron current drive</li><li> =3 Fenstermacher ECH</li><li> =4 Ehst Lower Hybrid</li><li> =5 ITER Neutral Beam</li><li> =6 new Culham Lower Hybrid model</li><li> =7 new Culham ECCD model</li><li> =8 new Culham Neutral Beam model</li><li> =9 Simple NBI model (see SYCOMORE HELIOS paper)</li><li> =10 ECRH user input gamma</li><li> =11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019)</li><li> =12 Simple NBI model</li></ul></td>
	</tr>
	<tr>
		<td>iefrffix</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for 2nd current drive efficiency model:<br><ul><li> =0 No fixed current drive</li><li> =1 Fenstermacher Lower Hybrid</li><li> =2 Ion Cyclotron current drive</li><li> =3 Fenstermacher ECH</li><li> =4 Ehst Lower Hybrid</li><li> =5 ITER Neutral Beam</li><li> =6 new Culham Lower Hybrid model</li><li> =7 new Culham ECCD model</li><li> =8 new Culham Neutral Beam model</li><li> =9 Simple NBI model (see SYCOMORE HELIOS paper)</li><li> =10 ECRH user input gamma</li><li> =11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019)</li></ul></td>
	</tr>
	<tr>
		<td>irfcd</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for current drive calculation:<br><ul><li> =0 turned off</li><li> =1 turned on</li></ul></td>
	</tr>
	<tr>
		<td>nbshield</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>neutral beam duct shielding thickness (m)</td>
	</tr>
	<tr>
		<td>nbshinef</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam shine-through fraction</td>
	</tr>
	<tr>
		<td>nbshinemw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam shine-through power</td>
	</tr>
	<tr>
		<td>pheat</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>heating power not used for current drive (MW) (iteration variable 11)</td>
	</tr>
	<tr>
		<td>pheatfix</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>secondary fixed heating power not used for current drive (MW)</td>
	</tr>
	<tr>
		<td>pinjalw</td>
		<td>real</td>
		<td>150.0D0</td>
		<td>maximum allowable value for injected power (MW) (constraint equation 30)</td>
	</tr>
	<tr>
		<td>pinjemw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>auxiliary injected power to electrons (MW)</td>
	</tr>
	<tr>
		<td>pinjfixmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>secondary total fixed auxiliary injected power (MW)</td>
	</tr>
	<tr>
		<td>pinjimw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>auxiliary injected power to ions (MW)</td>
	</tr>
	<tr>
		<td>pinjmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total auxiliary injected power (MW)</td>
	</tr>
	<tr>
		<td>plasipf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma driven current fraction (Bootstrap + Diamagnetic + PS)</td>
	</tr>
	<tr>
		<td>plhybd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>lower hybrid injection power (MW)</td>
	</tr>
	<tr>
		<td>pnbeam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam injection power (MW)</td>
	</tr>
	<tr>
		<td>pnbitot</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam power entering vacuum vessel</td>
	</tr>
	<tr>
		<td>porbitlossmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam power lost after ionisation but before thermalisation (orbit loss power) (MW)</td>
	</tr>
	<tr>
		<td>pscf_scene</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Pfirsch-Schlüter current fraction, SCENE fit</td>
	</tr>
	<tr>
		<td>psipf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Pfirsch-Schlüter current fraction</td>
	</tr>
	<tr>
		<td>pwplh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>lower hybrid wall plug power (MW)</td>
	</tr>
	<tr>
		<td>pwpnb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam wall plug power (MW)</td>
	</tr>
	<tr>
		<td>rho_ecrh</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>normalised minor radius at which electron cyclotron current drive is maximum</td>
	</tr>
	<tr>
		<td>rtanbeam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam centreline tangency radius (m)</td>
	</tr>
	<tr>
		<td>rtanmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum tangency radius for centreline of beam (m)</td>
	</tr>
	<tr>
		<td>taubeam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam e-decay lengths to plasma centre</td>
	</tr>
	<tr>
		<td>tbeamin</td>
		<td>real</td>
		<td>3.0D0</td>
		<td>permitted neutral beam e-decay lengths to plasma centre</td>
	</tr>
</table>

## divertor_kallenbach_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>abserr_sol</td>
		<td>real</td>
		<td>1.d-4</td>
		<td>Absolute contribution to the error tolerance in the Kallenbach divertor model</td>
	</tr>
	<tr>
		<td>exchangepowerlost</td>
		<td>real</td>
		<td>None</td>
		<td>Power lost due to charge exchange  [W]</td>
	</tr>
	<tr>
		<td>fmom</td>
		<td>real</td>
		<td>None</td>
		<td>momentum factor [-]</td>
	</tr>
	<tr>
		<td>fractionwidesol</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>Distance from target at which SOL gets broader as a fraction of connection length</td>
	</tr>
	<tr>
		<td>hydrogenicpowerlost</td>
		<td>real</td>
		<td>None</td>
		<td>Power lost due to hydrogenic radiation [W]</td>
	</tr>
	<tr>
		<td>impurity_enrichment</td>
		<td>real</td>
		<td>['5.0D0', '5.0D0', '5.0D0', '5...</td>
		<td>Ratio of each impurity concentration in SOL to confined plasma + the enrichment for Argon <br> is also propagated for PLASMOD (ipedestal=3)</td>
	</tr>
	<tr>
		<td>impuritypowerlost</td>
		<td>real</td>
		<td>None</td>
		<td>Power lost due to impurity radiation [W]</td>
	</tr>
	<tr>
		<td>ionisationpowerlost</td>
		<td>real</td>
		<td>None</td>
		<td>Power lost due to electron impact ionisation [W]</td>
	</tr>
	<tr>
		<td>kallenbach_scan_end</td>
		<td>real</td>
		<td>10.0</td>
		<td>end value for kallenbach scan parameter</td>
	</tr>
	<tr>
		<td>kallenbach_scan_num</td>
		<td>integer</td>
		<td>1</td>
		<td>number of scans for kallenbach scan test</td>
	</tr>
	<tr>
		<td>kallenbach_scan_start</td>
		<td>real</td>
		<td>2.0</td>
		<td>start value for kallenbach scan parameter</td>
	</tr>
	<tr>
		<td>kallenbach_scan_switch</td>
		<td>integer</td>
		<td>0</td>
		<td>switch to run scan of 1D Kallenbach divertor model:<br><ul><li> =1 on</li><li> =0 off</li></ul></td>
	</tr>
	<tr>
		<td>kallenbach_scan_var</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for parameter to scan for kallenbach scan test:<br><ul><li> =0 ttarget</li><li> =1 qtargettotal</li><li> =2 targetangle</li><li> =3 lambda_q_omp</li><li> =4 netau_sol</li></ul></td>
	</tr>
	<tr>
		<td>kallenbach_switch</td>
		<td>integer</td>
		<td>0</td>
		<td>switch to turn on the 1D Kallenbach divertor model:<br><ul><li> =1 on</li><li> =0 off</li></ul></td>
	</tr>
	<tr>
		<td>kallenbach_test_option</td>
		<td>integer</td>
		<td>0</td>
		<td>switch to choose kallenbach test option:<br><ul><li> =0 Test case with user inputs</li><li> =1 Test case for Kallenbach paper</li></ul></td>
	</tr>
	<tr>
		<td>kallenbach_tests</td>
		<td>integer</td>
		<td>0</td>
		<td>switch to run tests of 1D Kallenbach divertor model:<br><ul><li> =1 on</li><li> =0 off</li></ul></td>
	</tr>
	<tr>
		<td>lambda_q_omp</td>
		<td>real</td>
		<td>0.002D0</td>
		<td>SOL power fall-off length at the outer midplane, perpendicular to field [m]</td>
	</tr>
	<tr>
		<td>lcon_factor</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Correction factor for connection length from OMP to divertor = connection length/(piqrmajor)</td>
	</tr>
	<tr>
		<td>mach0</td>
		<td>real</td>
		<td>0.999</td>
		<td>Mach number at target (must be just less than 1)</td>
	</tr>
	<tr>
		<td>neomp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]</td>
	</tr>
	<tr>
		<td>neratio</td>
		<td>real</td>
		<td>0.75D0</td>
		<td>Ratio of mean SOL density at OMP to separatrix density at OMP (iteration variable 121)</td>
	</tr>
	<tr>
		<td>netau_sol</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>Parameter describing the departure from local ionisation equilibrium in the SOL. [ms.1e20/m3]</td>
	</tr>
	<tr>
		<td>pressure0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Total plasma pressure near target (thermal+dynamic) [Pa]</td>
	</tr>
	<tr>
		<td>psep_kallenbach</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Power conducted through the separatrix, as calculated by the divertor model [W]<br> Not equal to pdivt unless constraint 69 is imposed.</td>
	</tr>
	<tr>
		<td>qtargettotal</td>
		<td>real</td>
		<td>5.0D6</td>
		<td>Power density on target including surface recombination [W/m2] (iteration variable 124)</td>
	</tr>
	<tr>
		<td>relerr_sol</td>
		<td>real</td>
		<td>1.d-4</td>
		<td>Relative contribution to the error tolerance in the Kallenbach divertor model</td>
	</tr>
	<tr>
		<td>target_spread</td>
		<td>real</td>
		<td>0.003D0</td>
		<td>increase in SOL power fall-off length due to spreading, mapped to OMP [m]</td>
	</tr>
	<tr>
		<td>targetangle</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>Angle between field-line and divertor target (degrees)</td>
	</tr>
	<tr>
		<td>teomp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>separatrix temperature calculated by the Kallenbach divertor model [eV] (issue #457)</td>
	</tr>
	<tr>
		<td>totalpowerlost</td>
		<td>real</td>
		<td>None</td>
		<td>Total power lost due to radiation, ionisation and recombination [W]</td>
	</tr>
	<tr>
		<td>ttarget</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>Plasma temperature adjacent to divertor sheath [eV] (iteration variable 120)<br> Rem : 5 eV is the current limit for tungsten sputtering from argon impurity</td>
	</tr>
</table>

## divertor_ode
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>aplas</td>
		<td>real</td>
		<td>2.5D0</td>
		<td></td>
	</tr>
	<tr>
		<td>area_omp</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>area_target</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>circumf_bu</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>circumference_omp</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>circumference_target</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>degree</td>
		<td>real</td>
		<td>pi/180.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>eightemi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>eightemi48</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>elEion</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fluxdens_to_pa</td>
		<td>real</td>
		<td>1.0D0/1.55e23</td>
		<td></td>
	</tr>
	<tr>
		<td>impurities_present</td>
		<td>logical</td>
		<td>.false.</td>
		<td></td>
	</tr>
	<tr>
		<td>kappa0</td>
		<td>real</td>
		<td>2390.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lengthofwidesol</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ln10</td>
		<td>real</td>
		<td>log(10.0D0)</td>
		<td></td>
	</tr>
	<tr>
		<td>mi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>seppowerratio</td>
		<td>real</td>
		<td>2.3D0</td>
		<td></td>
	</tr>
	<tr>
		<td>sol_broadening</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>v01</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>v02</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>zeff_div</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## divertor_ode_var
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>impurity_concs</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## divertor_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>adas</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>area divertor / area main plasma (along separatrix)</td>
	</tr>
	<tr>
		<td>anginc</td>
		<td>real</td>
		<td>0.262D0</td>
		<td>angle of incidence of field line on plate (rad)</td>
	</tr>
	<tr>
		<td>betai</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>poloidal plane angle between divertor plate and leg, inboard (rad)</td>
	</tr>
	<tr>
		<td>betao</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>poloidal plane angle between divertor plate and leg, outboard (rad)</td>
	</tr>
	<tr>
		<td>bpsout</td>
		<td>real</td>
		<td>0.60D0</td>
		<td>reference B_p at outboard divertor strike point (T)</td>
	</tr>
	<tr>
		<td>c1div</td>
		<td>real</td>
		<td>0.45D0</td>
		<td>fitting coefficient to adjust ptpdiv, ppdiv</td>
	</tr>
	<tr>
		<td>c2div</td>
		<td>real</td>
		<td>-7.0D0</td>
		<td>fitting coefficient to adjust ptpdiv, ppdiv</td>
	</tr>
	<tr>
		<td>c3div</td>
		<td>real</td>
		<td>0.54D0</td>
		<td>fitting coefficient to adjust ptpdiv, ppdiv</td>
	</tr>
	<tr>
		<td>c4div</td>
		<td>real</td>
		<td>-3.6D0</td>
		<td>fitting coefficient to adjust ptpdiv, ppdiv</td>
	</tr>
	<tr>
		<td>c5div</td>
		<td>real</td>
		<td>0.7D0</td>
		<td>fitting coefficient to adjust ptpdiv, ppdiv</td>
	</tr>
	<tr>
		<td>c6div</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fitting coefficient to adjust ptpdiv, ppdiv</td>
	</tr>
	<tr>
		<td>delld</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>coeff for power distribution along main plasma</td>
	</tr>
	<tr>
		<td>dendiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma density at divertor (10**20 /m3)</td>
	</tr>
	<tr>
		<td>densin</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>density at plate (on separatrix) (10**20 /m3)</td>
	</tr>
	<tr>
		<td>divclfr</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>divertor coolant fraction</td>
	</tr>
	<tr>
		<td>divdens</td>
		<td>real</td>
		<td>1.0D4</td>
		<td>divertor structure density (kg/m3)</td>
	</tr>
	<tr>
		<td>divdum</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for divertor Zeff model:<br><ul><li> =0 calc</li><li> =1 input</li></ul></td>
	</tr>
	<tr>
		<td>divfix</td>
		<td>real</td>
		<td>0.2D0</td>
		<td>divertor structure vertical thickness (m)</td>
	</tr>
	<tr>
		<td>divmas</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>divertor plate mass (kg)</td>
	</tr>
	<tr>
		<td>divplt</td>
		<td>real</td>
		<td>0.035D0</td>
		<td>divertor plate thickness (m) (from Spears, Sept 1990)</td>
	</tr>
	<tr>
		<td>divsur</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>divertor surface area (m2)</td>
	</tr>
	<tr>
		<td>fdfs</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>radial gradient ratio</td>
	</tr>
	<tr>
		<td>fdiva</td>
		<td>real</td>
		<td>1.11D0</td>
		<td>divertor area fudge factor (for ITER, Sept 1990)</td>
	</tr>
	<tr>
		<td>fgamp</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>sheath potential factor (not used)</td>
	</tr>
	<tr>
		<td>fhout</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fraction of power to outboard divertor (for single null)</td>
	</tr>
	<tr>
		<td>fififi</td>
		<td>real</td>
		<td>4.0D-3</td>
		<td>coefficient for gamdiv</td>
	</tr>
	<tr>
		<td>frrp</td>
		<td>real</td>
		<td>0.4D0</td>
		<td>fraction of radiated power to plate</td>
	</tr>
	<tr>
		<td>hldiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>divertor heat load (MW/m2)</td>
	</tr>
	<tr>
		<td>hldivlim</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>heat load limit (MW/m2)</td>
	</tr>
	<tr>
		<td>ksic</td>
		<td>real</td>
		<td>0.8D0</td>
		<td>power fraction for outboard double-null scrape-off plasma</td>
	</tr>
	<tr>
		<td>lamp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power flow width (m)</td>
	</tr>
	<tr>
		<td>minstang</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>minimum strike angle for heat flux calculation</td>
	</tr>
	<tr>
		<td>omegan</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>pressure ratio (nT)_plasma / (nT)_scrape-off</td>
	</tr>
	<tr>
		<td>omlarg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power spillage to private flux factor</td>
	</tr>
	<tr>
		<td>ppdivr</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak heat load at plate (with radiation) (MW/m2)</td>
	</tr>
	<tr>
		<td>prn1</td>
		<td>real</td>
		<td>0.285D0</td>
		<td>n-scrape-off / n-average plasma; (input for ipedestal=0, = nesep/dene if ipedestal&gt;=1)</td>
	</tr>
	<tr>
		<td>ptpdiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak temperature at the plate (eV)</td>
	</tr>
	<tr>
		<td>rconl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>connection length ratio, outboard side</td>
	</tr>
	<tr>
		<td>rlclolcn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ratio of collision length / connection length</td>
	</tr>
	<tr>
		<td>rlenmax</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>maximum value for length ratio (rlclolcn) (constraintg eqn 22)</td>
	</tr>
	<tr>
		<td>rsrd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>effective separatrix/divertor radius ratio</td>
	</tr>
	<tr>
		<td>tconl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>main plasma connection length (m)</td>
	</tr>
	<tr>
		<td>tdiv</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>temperature at divertor (eV) (input for stellarator only, calculated for tokamaks)</td>
	</tr>
	<tr>
		<td>tsep</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>temperature at the separatrix (eV)</td>
	</tr>
	<tr>
		<td>xparain</td>
		<td>real</td>
		<td>2.1D3</td>
		<td>parallel heat transport coefficient (m2/s)</td>
	</tr>
	<tr>
		<td>xpertin</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>perpendicular heat transport coefficient (m2/s)</td>
	</tr>
	<tr>
		<td>zeffdiv</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Zeff in the divertor region (if divdum /= 0)</td>
	</tr>
</table>

## error_handling
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>ERROR_INFO</td>
		<td>integer</td>
		<td>1</td>
		<td></td>
	</tr>
	<tr>
		<td>ERROR_OKAY</td>
		<td>integer</td>
		<td>0</td>
		<td></td>
	</tr>
	<tr>
		<td>ERROR_SEVERE</td>
		<td>integer</td>
		<td>3</td>
		<td></td>
	</tr>
	<tr>
		<td>ERROR_WARN</td>
		<td>integer</td>
		<td>2</td>
		<td></td>
	</tr>
	<tr>
		<td>FLT_DEFAULT</td>
		<td>real</td>
		<td>real(INT_DEFAULT, kind(1.0D0))</td>
		<td></td>
	</tr>
	<tr>
		<td>INT_DEFAULT</td>
		<td>integer</td>
		<td>-999999</td>
		<td></td>
	</tr>
	<tr>
		<td>error_head</td>
		<td>type</td>
		<td>null()</td>
		<td></td>
	</tr>
	<tr>
		<td>error_id</td>
		<td>integer</td>
		<td>0</td>
		<td>error_id : identifier for final message encountered</td>
	</tr>
	<tr>
		<td>error_status</td>
		<td>integer</td>
		<td>ERROR_OKAY</td>
		<td>error_status : overall status flag for a run; on exit:<br>                  0  all okay<br>                  1  informational messages have been encountered<br>                  2  warning (non-fatal) messages have been encountered<br>                  3  severe (fatal) errors have occurred</td>
	</tr>
	<tr>
		<td>error_tail</td>
		<td>type</td>
		<td>null()</td>
		<td></td>
	</tr>
	<tr>
		<td>error_type</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>errors_on</td>
		<td>logical</td>
		<td>.false.</td>
		<td></td>
	</tr>
	<tr>
		<td>fdiags</td>
		<td>real</td>
		<td>['FLT_DEFAULT', 'FLT_DEFAULT',...</td>
		<td></td>
	</tr>
	<tr>
		<td>idiags</td>
		<td>integer</td>
		<td>['INT_DEFAULT', 'INT_DEFAULT',...</td>
		<td></td>
	</tr>
</table>

## fispact_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>bliact</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>inboard blanket total activity (Bq)</td>
	</tr>
	<tr>
		<td>bligdr</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>inboard blanket total gamma dose rate (Sv/hr)</td>
	</tr>
	<tr>
		<td>blihkw</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>inboard blanket total heat output (kW)</td>
	</tr>
	<tr>
		<td>bliizp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard blanket integrated zone power / neutron</td>
	</tr>
	<tr>
		<td>blimzp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard blanket mean zone power density / neutron</td>
	</tr>
	<tr>
		<td>bloact</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>outboard blanket total activity (Bq)</td>
	</tr>
	<tr>
		<td>blogdr</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>outboard blanket total gamma dose rate (Sv/hr)</td>
	</tr>
	<tr>
		<td>blohkw</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>outboard blanket total heat output (kW)</td>
	</tr>
	<tr>
		<td>bloizp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard blanket integrated zone power / neutron</td>
	</tr>
	<tr>
		<td>blomzp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard blanket mean zone power density / neutron</td>
	</tr>
	<tr>
		<td>fwiact</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>inboard first wall total activity (Bq)</td>
	</tr>
	<tr>
		<td>fwigdr</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>inboard first wall total gamma dose rate (Sv/hr)</td>
	</tr>
	<tr>
		<td>fwihkw</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>inboard first wall total heat output (kW)</td>
	</tr>
	<tr>
		<td>fwiizp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard first wall integrated zone power / neutron</td>
	</tr>
	<tr>
		<td>fwimzp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard first wall mean zone power density/neutron</td>
	</tr>
	<tr>
		<td>fwoact</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>outboard first wall total activity (Bq)</td>
	</tr>
	<tr>
		<td>fwogdr</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>outboard first wall total gamma dose rate (Sv/hr)</td>
	</tr>
	<tr>
		<td>fwohkw</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>outboard first wall total heat output (kW)</td>
	</tr>
	<tr>
		<td>fwoizp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard first wall integrated zone power / neutron</td>
	</tr>
	<tr>
		<td>fwomzp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard first wall mean zone power density/neutron</td>
	</tr>
	<tr>
		<td>fwtemp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard first wall temperature after a LOCA (K)</td>
	</tr>
</table>

## fw_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>double</td>
		<td>integer</td>
		<td>8</td>
		<td></td>
	</tr>
</table>

## fwbs_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>afw</td>
		<td>real</td>
		<td>0.006D0</td>
		<td>radius of first wall cooling channels (m)</td>
	</tr>
	<tr>
		<td>afwi</td>
		<td>real</td>
		<td>0.008D0</td>
		<td>inner radius of inboard first wall/blanket coolant channels (stellarator only) (m)</td>
	</tr>
	<tr>
		<td>afwo</td>
		<td>real</td>
		<td>0.008D0</td>
		<td>inner radius of outboard first wall/blanket coolant channels (stellarator only) (m)</td>
	</tr>
	<tr>
		<td>armour_fw_bl_mass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Total mass of armour, first wall and blanket (kg)</td>
	</tr>
	<tr>
		<td>bktlife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Full power blanket lifetime (years)</td>
	</tr>
	<tr>
		<td>blktmodel</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for blanket/tritium breeding model (see iblanket):<br><ul><li> =0 original simple model</li><li> =1 KIT model based on a helium-cooled pebble-bed blanket (HCPB) reference design</li></ul></td>
	</tr>
	<tr>
		<td>blkttype</td>
		<td>integer</td>
		<td>3</td>
		<td>Switch for blanket type:<br><ul><li> =1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA_D_2M97B7</li><li> =2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX</li><li> =3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX</li></ul></td>
	</tr>
	<tr>
		<td>blpressure</td>
		<td>real</td>
		<td>15.5D6</td>
		<td>blanket coolant pressure (Pa) (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>breeder_f</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (iteration variable 108)</td>
	</tr>
	<tr>
		<td>breeder_multiplier</td>
		<td>real</td>
		<td>0.75D0</td>
		<td>combined breeder/multipler fraction of blanket by volume</td>
	</tr>
	<tr>
		<td>breedmat</td>
		<td>integer</td>
		<td>1</td>
		<td>breeder material switch (iblanket=2 (KIT HCPB)):<br><ul><li> =1 Lithium orthosilicate</li><li> =2 Lithium methatitanate</li><li> =3 Lithium zirconate</li></ul></td>
	</tr>
	<tr>
		<td>coolmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of water coolant (in shield, blanket, first wall, divertor) (kg)</td>
	</tr>
	<tr>
		<td>coolp</td>
		<td>real</td>
		<td>15.5D6</td>
		<td>blanket coolant pressure (Pa) (stellarator only)</td>
	</tr>
	<tr>
		<td>coolwh</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for blanket coolant (set via blkttype):<br><ul><li> =1 helium</li><li> =2 pressurized water</li></ul></td>
	</tr>
	<tr>
		<td>declblkt</td>
		<td>real</td>
		<td>0.075D0</td>
		<td>neutron power deposition decay length of blanket structural material (m) (stellarators only)</td>
	</tr>
	<tr>
		<td>declfw</td>
		<td>real</td>
		<td>0.075D0</td>
		<td>neutron power deposition decay length of first wall structural material (m) (stellarators only)</td>
	</tr>
	<tr>
		<td>declshld</td>
		<td>real</td>
		<td>0.075D0</td>
		<td>neutron power deposition decay length of shield structural material (m) (stellarators only)</td>
	</tr>
	<tr>
		<td>densbreed</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>density of breeder material (kg/m3) (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>denstl</td>
		<td>real</td>
		<td>7800.0D0</td>
		<td>density of steel (kg/m3)</td>
	</tr>
	<tr>
		<td>denw</td>
		<td>real</td>
		<td>19250.0D0</td>
		<td>density of tungsten (kg/m3)</td>
	</tr>
	<tr>
		<td>dewmkg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total mass of vacuum vessel + cryostat (kg) (calculated if blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>emult</td>
		<td>real</td>
		<td>1.269D0</td>
		<td>energy multiplication in blanket and shield</td>
	</tr>
	<tr>
		<td>emultmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power due to energy multiplication in blanket and shield [MW]</td>
	</tr>
	<tr>
		<td>etahtp</td>
		<td>real</td>
		<td>0.95D0</td>
		<td>electrical efficiency of primary coolant pumps</td>
	</tr>
	<tr>
		<td>etaiso</td>
		<td>real</td>
		<td>0.85D0</td>
		<td>isentropic efficiency of FW and blanket coolant pumps</td>
	</tr>
	<tr>
		<td>f_neut_shield</td>
		<td>real</td>
		<td>-1.0D0</td>
		<td>Fraction of nuclear power shielded before the CP magnet (ST)<br> ( neut_absorb = -1 --&gt; a fit on simplified MCNP neutronic<br> calculation is used assuming water cooled (13%) tungesten carbyde )</td>
	</tr>
	<tr>
		<td>fblbe</td>
		<td>real</td>
		<td>0.6D0</td>
		<td>beryllium fraction of blanket by volume (if iblanket=2, is Be fraction of breeding zone)</td>
	</tr>
	<tr>
		<td>fblbreed</td>
		<td>real</td>
		<td>0.154D0</td>
		<td>breeder fraction of blanket breeding zone by volume (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>fblhebmi</td>
		<td>real</td>
		<td>0.4D0</td>
		<td>helium fraction of inboard blanket box manifold by volume (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>fblhebmo</td>
		<td>real</td>
		<td>0.4D0</td>
		<td>helium fraction of outboard blanket box manifold by volume (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>fblhebpi</td>
		<td>real</td>
		<td>0.6595D0</td>
		<td>helium fraction of inboard blanket back plate by volume (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>fblhebpo</td>
		<td>real</td>
		<td>0.6713D0</td>
		<td>helium fraction of outboard blanket back plate by volume (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>fblli</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>lithium fraction of blanket by volume (stellarator only)</td>
	</tr>
	<tr>
		<td>fblli2o</td>
		<td>real</td>
		<td>0.08D0</td>
		<td>lithium oxide fraction of blanket by volume (stellarator only)</td>
	</tr>
	<tr>
		<td>fbllipb</td>
		<td>real</td>
		<td>0.68D0</td>
		<td>lithium lead fraction of blanket by volume (stellarator only)</td>
	</tr>
	<tr>
		<td>fblss</td>
		<td>real</td>
		<td>0.09705D0</td>
		<td>KIT blanket model: steel fraction of breeding zone</td>
	</tr>
	<tr>
		<td>fblvd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vanadium fraction of blanket by volume (stellarator only)</td>
	</tr>
	<tr>
		<td>fdiv</td>
		<td>real</td>
		<td>0.115D0</td>
		<td>area fraction taken up by divertor</td>
	</tr>
	<tr>
		<td>fhcd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>area fraction covered by heating/current drive apparatus plus diagnostics</td>
	</tr>
	<tr>
		<td>fhole</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>area fraction taken up by other holes (IFE)</td>
	</tr>
	<tr>
		<td>fvoldw</td>
		<td>real</td>
		<td>1.74D0</td>
		<td>area coverage factor for vacuum vessel volume</td>
	</tr>
	<tr>
		<td>fvolsi</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>area coverage factor for inboard shield volume</td>
	</tr>
	<tr>
		<td>fvolso</td>
		<td>real</td>
		<td>0.64D0</td>
		<td>area coverage factor for outboard shield volume</td>
	</tr>
	<tr>
		<td>fw_armour_mass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall armour mass (kg)</td>
	</tr>
	<tr>
		<td>fw_armour_thickness</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>first wall armour thickness (m)</td>
	</tr>
	<tr>
		<td>fw_armour_vol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall armour volume (m3)</td>
	</tr>
	<tr>
		<td>fw_channel_length</td>
		<td>real</td>
		<td>4.0D0</td>
		<td>Length of a single first wall channel (all in parallel) (m)<br> (iteration variable 114, useful for constraint equation 39)</td>
	</tr>
	<tr>
		<td>fw_th_conductivity</td>
		<td>real</td>
		<td>28.34D0</td>
		<td>thermal conductivity of first wall material at 293 K (W/m/K) (Temperature dependence <br> is as for unirradiated Eurofer)</td>
	</tr>
	<tr>
		<td>fw_wall</td>
		<td>real</td>
		<td>0.003D0</td>
		<td>wall thickness of first wall coolant channels (m)</td>
	</tr>
	<tr>
		<td>fwbsshape</td>
		<td>integer</td>
		<td>2</td>
		<td>switch for first wall, blanket, shield and vacuum vessel shape:<br><ul><li> =1 D-shaped (cylinder inboard + ellipse outboard)</li><li> =2 defined by two ellipses</li></ul></td>
	</tr>
	<tr>
		<td>fwclfr</td>
		<td>real</td>
		<td>0.15D0</td>
		<td>first wall coolant fraction (calculated if lpulse=1 or ipowerflow=1)</td>
	</tr>
	<tr>
		<td>fwcoolant</td>
		<td>character</td>
		<td>'helium'</td>
		<td>switch for first wall coolant (can be different from blanket coolant):<br><ul><li> 'helium' </li><li> 'water'</li></ul></td>
	</tr>
	<tr>
		<td>fwinlet</td>
		<td>real</td>
		<td>573.0D0</td>
		<td>inlet temperature of first wall coolant (K)</td>
	</tr>
	<tr>
		<td>fwlife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall full-power year lifetime (y)</td>
	</tr>
	<tr>
		<td>fwmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall mass (kg)</td>
	</tr>
	<tr>
		<td>fwoutlet</td>
		<td>real</td>
		<td>823.0D0</td>
		<td>outlet temperature of first wall coolant (K)</td>
	</tr>
	<tr>
		<td>fwpressure</td>
		<td>real</td>
		<td>15.5D6</td>
		<td>first wall coolant pressure (Pa) (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>hcdportsize</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for size of heating/current drive ports (iblanket=2 (KIT HCPB)):<br><ul><li> =1 'small'</li><li> =2 'large'</li></ul></td>
	</tr>
	<tr>
		<td>iblanket</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for blanket model:<br><ul><li> =1 CCFE HCPB model</li><li> =2 KIT HCPB model</li><li> =3 CCFE HCPB model with Tritium Breeding Ratio calculation</li><li> =4 KIT HCLL model</li></ul></td>
	</tr>
	<tr>
		<td>iblanket_thickness</td>
		<td>integer</td>
		<td>2</td>
		<td>Blanket thickness switch (Do not set blnkith, blnkoth, fwith or fwoth when iblanket=3):<br><ul><li> =1 thin    0.53 m inboard, 0.91 m outboard</li><li> =2 medium  0.64 m inboard, 1.11 m outboard</li><li> =3 thick   0.75 m inboard, 1.30 m outboard</li></ul></td>
	</tr>
	<tr>
		<td>iblnkith</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for inboard blanket:<br><ul><li> =0 No inboard blanket (blnkith=0.0)</li><li> =1 Inboard blanket present</li></ul></td>
	</tr>
	<tr>
		<td>inlet_temp</td>
		<td>real</td>
		<td>573.0D0</td>
		<td>inlet temperature of blanket coolant  (K) (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>inuclear</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for nuclear heating in the coils:<br><ul><li> =0 Frances Fox model (default)</li><li> =1 Fixed by user (qnuc)</li></ul></td>
	</tr>
	<tr>
		<td>irefprop</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch to use REFPROP routines (stellarator only)</td>
	</tr>
	<tr>
		<td>li6enrich</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>lithium-6 enrichment of breeding material (%)</td>
	</tr>
	<tr>
		<td>nblktmodpi</td>
		<td>integer</td>
		<td>7</td>
		<td>number of inboard blanket modules in poloidal direction (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>nblktmodpo</td>
		<td>integer</td>
		<td>8</td>
		<td>number of outboard blanket modules in poloidal direction (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>nblktmodti</td>
		<td>integer</td>
		<td>32</td>
		<td>number of inboard blanket modules in toroidal direction (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>nblktmodto</td>
		<td>integer</td>
		<td>48</td>
		<td>number of outboard blanket modules in toroidal direction (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>neut_flux_cp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Centrepost TF fast neutron flux (E &gt; 0.1 MeV) [m^(-2).^(-1)]<br> This variable is only calculated for superconducting (i_tf_sup = 1 )<br> spherical tokamal magnet designs (itart = 0)</td>
	</tr>
	<tr>
		<td>nflutf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak fast neutron fluence on TF coil superconductor (n/m2) (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>npdiv</td>
		<td>integer</td>
		<td>2</td>
		<td>number of divertor ports (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>nphcdin</td>
		<td>integer</td>
		<td>2</td>
		<td>number of inboard ports for heating/current drive (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>nphcdout</td>
		<td>integer</td>
		<td>2</td>
		<td>number of outboard ports for heating/current drive (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>outlet_temp</td>
		<td>real</td>
		<td>823.0D0</td>
		<td>Outlet temperature of blanket coolant (K) (secondary_cycle&gt;1)<br><ul><li> input if coolwh=1 (helium)</li><li> calculated if coolwh=2 (water)</li></ul></td>
	</tr>
	<tr>
		<td>peaking_factor</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>peaking factor for first wall heat loads. (Applied separately to inboard and outboard loads.<br> Applies to both neutron and surface loads. Only used to calculate peak temperature - not <br> the coolant flow rate.)</td>
	</tr>
	<tr>
		<td>pitch</td>
		<td>real</td>
		<td>0.020D0</td>
		<td>pitch of first wall cooling channels (m)</td>
	</tr>
	<tr>
		<td>pnuc_cp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Total nuclear heating in the ST centrepost (MW)</td>
	</tr>
	<tr>
		<td>pnuc_cp_sh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Neutronic shield nuclear heating in the ST centrepost (MW)</td>
	</tr>
	<tr>
		<td>pnuc_cp_tf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF neutronic nuclear heating in the ST centrepost (MW)</td>
	</tr>
	<tr>
		<td>pnucblkt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the blanket (MW)</td>
	</tr>
	<tr>
		<td>pnucdiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the divertor (MW)</td>
	</tr>
	<tr>
		<td>pnucfw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the first wall (MW)</td>
	</tr>
	<tr>
		<td>pnuchcd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the HCD apparatus and diagnostics (MW)</td>
	</tr>
	<tr>
		<td>pnucloss</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating lost via holes (MW)</td>
	</tr>
	<tr>
		<td>pnucshld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the shield (MW)</td>
	</tr>
	<tr>
		<td>pnucvvplus</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating to vacuum vessel and beyond(MW)</td>
	</tr>
	<tr>
		<td>praddiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radiation power incident on the divertor (MW)</td>
	</tr>
	<tr>
		<td>pradfw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radiation power incident on the divertor (MW)</td>
	</tr>
	<tr>
		<td>pradhcd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radiation power incident on the divertor (MW)</td>
	</tr>
	<tr>
		<td>pradloss</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radiation power incident on the divertor (MW)</td>
	</tr>
	<tr>
		<td>primary_pumping</td>
		<td>integer</td>
		<td>2</td>
		<td>Switch for pumping power for primary coolant (mechanical power only and peak first wall <br> temperature is only calculated if primary_pumping=2):<br><ul><li> =0 User sets pump power directly (htpmw_blkt, htpmw_fw, htpmw_div, htpmw_shld)</li><li> =1 User sets pump power as a fraction of thermal power (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)</li><li> =2 Mechanical pumping power is calculated</li><li> =3 Mechanical pumping power is calculated using specified pressure drop</li></ul></td>
	</tr>
	<tr>
		<td>ptfnuc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the TF coil (MW)</td>
	</tr>
	<tr>
		<td>ptfnucpm3</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the TF coil (MW/m3) (blktmodel&gt;0)</td>
	</tr>
	<tr>
		<td>qnuc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>nuclear heating in the coils (W) (inuclear=1)</td>
	</tr>
	<tr>
		<td>rdewex</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>cryostat radius (m)</td>
	</tr>
	<tr>
		<td>roughness</td>
		<td>real</td>
		<td>1.0D-6</td>
		<td>first wall channel roughness epsilon (m)</td>
	</tr>
	<tr>
		<td>rpf2dewar</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>radial distance between outer edge of largest (ipfloc=3) PF coil (or stellarator <br> modular coil) and cryostat (m)</td>
	</tr>
	<tr>
		<td>secondary_cycle</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for power conversion cycle:<br><ul><li> =0 Set efficiency for chosen blanket, from detailed models (divertor heat not used)</li><li> =1 Set efficiency for chosen blanket, from detailed models (divertor heat used)</li><li> =2 user input thermal-electric efficiency (etath)</li><li> =3 steam Rankine cycle</li><li> =4 supercritical CO2 cycle</li></ul></td>
	</tr>
	<tr>
		<td>tbr</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>tritium breeding ratio (iblanket=2,3 (KIT HCPB/HCLL))</td>
	</tr>
	<tr>
		<td>tfwmatmax</td>
		<td>real</td>
		<td>823.0D0</td>
		<td>maximum temperature of first wall material (K) (secondary_cycle&gt;1)</td>
	</tr>
	<tr>
		<td>tpeak</td>
		<td>real</td>
		<td>873.0D0</td>
		<td>peak first wall temperature (K)</td>
	</tr>
	<tr>
		<td>tritprate</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>tritium production rate (g/day) (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>vdewex</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>cryostat volume (m3)</td>
	</tr>
	<tr>
		<td>vdewin</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vacuum vessel volume (m3)</td>
	</tr>
	<tr>
		<td>vfblkt</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>coolant void fraction in blanket (blktmodel=0), (calculated if blktmodel &gt; 0)</td>
	</tr>
	<tr>
		<td>vfcblkt</td>
		<td>real</td>
		<td>0.05295D0</td>
		<td>He coolant fraction of blanket by volume (iblanket= 1,3 (CCFE HCPB))</td>
	</tr>
	<tr>
		<td>vfpblkt</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>He purge gas fraction of blanket by volume (iblanket= 1,3 (CCFE HCPB))</td>
	</tr>
	<tr>
		<td>vfshld</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>coolant void fraction in shield</td>
	</tr>
	<tr>
		<td>volblkt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of blanket (m3)</td>
	</tr>
	<tr>
		<td>volblkti</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of inboard blanket (m3)</td>
	</tr>
	<tr>
		<td>volblkto</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of outboard blanket (m3)</td>
	</tr>
	<tr>
		<td>volshld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of shield (m3)</td>
	</tr>
	<tr>
		<td>vvhemax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum helium concentration in vacuum vessel at end of plant life (appm) <br> (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>vvmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vacuum vessel mass (kg)</td>
	</tr>
	<tr>
		<td>wallpf</td>
		<td>real</td>
		<td>1.21D0</td>
		<td>neutron wall load peaking factor (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>whtblbe</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - beryllium part (kg)</td>
	</tr>
	<tr>
		<td>whtblbreed</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - breeder part (kg) (iblanket=2 (KIT HCPB))</td>
	</tr>
	<tr>
		<td>whtblkt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket (kg)</td>
	</tr>
	<tr>
		<td>whtblli</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - lithium part (kg)</td>
	</tr>
	<tr>
		<td>whtblli4sio4</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of lithium orthosilicate in blanket (kg) (iblanket=1,3 (CCFE HCPB))</td>
	</tr>
	<tr>
		<td>whtblss</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - steel part (kg)</td>
	</tr>
	<tr>
		<td>whtbltibe12</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of titanium beryllide in blanket (kg) (iblanket=1,3 (CCFE HCPB))</td>
	</tr>
	<tr>
		<td>whtblvd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - vanadium part (kg)</td>
	</tr>
	<tr>
		<td>whtshld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of shield (kg)</td>
	</tr>
	<tr>
		<td>wpenshld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of the penetration shield (kg)</td>
	</tr>
	<tr>
		<td>wtblli2o</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - Li_2O part (kg)</td>
	</tr>
	<tr>
		<td>wtbllipb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of blanket - Li-Pb part (kg)</td>
	</tr>
	<tr>
		<td>wtshldi</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of inboard shield (kg)</td>
	</tr>
	<tr>
		<td>wtshldo</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of outboard shield (kg)</td>
	</tr>
	<tr>
		<td>zdewex</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>cryostat height (m)</td>
	</tr>
</table>

## global_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>convergence_parameter</td>
		<td>real</td>
		<td>None</td>
		<td>VMCON convergence parameter "sum"</td>
	</tr>
	<tr>
		<td>fileprefix</td>
		<td>character</td>
		<td>""</td>
		<td>input file prefix</td>
	</tr>
	<tr>
		<td>icase</td>
		<td>character</td>
		<td>'Steady-state tokamak model'</td>
		<td>power plant type</td>
	</tr>
	<tr>
		<td>iscan_global</td>
		<td>integer</td>
		<td>0</td>
		<td>Makes iscan available globally.</td>
	</tr>
	<tr>
		<td>maxcal</td>
		<td>integer</td>
		<td>200</td>
		<td>maximum number of VMCON iterations</td>
	</tr>
	<tr>
		<td>output_prefix</td>
		<td>character</td>
		<td>""</td>
		<td>output file prefix</td>
	</tr>
	<tr>
		<td>run_tests</td>
		<td>integer</td>
		<td>0</td>
		<td>turns on built-in tests if set to 1</td>
	</tr>
	<tr>
		<td>runtitle</td>
		<td>character</td>
		<td>"Run Title (change this line u...</td>
		<td>short descriptive title for the run</td>
	</tr>
	<tr>
		<td>verbose</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for turning on/off diagnostic messages<br><ul><li> =0 turn off diagnostics</li><li> =1 turn on diagnostics</li></ul></td>
	</tr>
	<tr>
		<td>vlabel</td>
		<td>character</td>
		<td>None</td>
		<td>scan value name label</td>
	</tr>
	<tr>
		<td>vlabel_2</td>
		<td>character</td>
		<td>None</td>
		<td>scan value name label (2nd dimension)</td>
	</tr>
	<tr>
		<td>xlabel</td>
		<td>character</td>
		<td>None</td>
		<td>scan parameter description label</td>
	</tr>
	<tr>
		<td>xlabel_2</td>
		<td>character</td>
		<td>None</td>
		<td>scan parameter description label (2nd dimension)</td>
	</tr>
</table>

## heat_transport_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>baseel</td>
		<td>real</td>
		<td>5.0D6</td>
		<td>base plant electric load (W)</td>
	</tr>
	<tr>
		<td>crypmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>cryogenic plant power (MW)</td>
	</tr>
	<tr>
		<td>etatf</td>
		<td>real</td>
		<td>0.9D0</td>
		<td>AC to resistive power conversion for TF coils</td>
	</tr>
	<tr>
		<td>etath</td>
		<td>real</td>
		<td>0.35D0</td>
		<td>thermal to electric conversion efficiency if secondary_cycle=2; otherwise calculated.</td>
	</tr>
	<tr>
		<td>fachtmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>facility heat removal (MW)</td>
	</tr>
	<tr>
		<td>fcsht</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total baseline power required at all times (MW)</td>
	</tr>
	<tr>
		<td>fgrosbop</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>scaled fraction of gross power to balance-of-plant</td>
	</tr>
	<tr>
		<td>fmgdmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power to mgf (motor-generator flywheel) units (MW) (ignored if iscenr=2)</td>
	</tr>
	<tr>
		<td>fpumpblkt</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>fraction of total blanket thermal power required to drive the blanket <br> coolant pumps (default assumes water coolant) (secondary_cycle=0)</td>
	</tr>
	<tr>
		<td>fpumpdiv</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>fraction of total divertor thermal power required to drive the divertor <br> coolant pumps (default assumes water coolant)</td>
	</tr>
	<tr>
		<td>fpumpfw</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>fraction of total first wall thermal power required to drive the FW coolant <br> pumps (default assumes water coolant) (secondary_cycle=0)</td>
	</tr>
	<tr>
		<td>fpumpshld</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>fraction of total shield thermal power required to drive the shield coolant <br> pumps (default assumes water coolant)</td>
	</tr>
	<tr>
		<td>helpow</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>heat removal at cryogenic temperatures (W)</td>
	</tr>
	<tr>
		<td>htpmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>heat transport system electrical pump power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_blkt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>blanket coolant mechanical pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_div</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>divertor coolant mechanical pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_fw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>first wall coolant mechanical pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpmw_min</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Minimum total electrical power for primary coolant pumps (MW) (NOT RECOMMENDED)</td>
	</tr>
	<tr>
		<td>htpmw_shld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>shield and vacuum vessel coolant mechanical pumping power (MW)</td>
	</tr>
	<tr>
		<td>htpsecmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Waste power lost from primary coolant pumps (MW)</td>
	</tr>
	<tr>
		<td>ipowerflow</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for power flow model:<br><ul><li> =0 pre-2014 version</li><li> =1 comprehensive 2014 model</li></ul></td>
	</tr>
	<tr>
		<td>iprimnloss</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for lost neutron power through holes destiny (ipowerflow=0):<br><ul><li> =0 does not contribute to energy generation cycle</li><li> =1 contributes to energy generation cycle</li></ul></td>
	</tr>
	<tr>
		<td>iprimshld</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for shield thermal power destiny:<br><ul><li> =0 does not contribute to energy generation cycle</li><li> =1 contributes to energy generation cycle</li></ul></td>
	</tr>
	<tr>
		<td>nphx</td>
		<td>integer</td>
		<td>0</td>
		<td>number of primary heat exchangers</td>
	</tr>
	<tr>
		<td>pacpmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total pulsed power system load (MW)</td>
	</tr>
	<tr>
		<td>peakmva</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak MVA requirement</td>
	</tr>
	<tr>
		<td>pfwdiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>heat removal from first wall/divertor (MW)</td>
	</tr>
	<tr>
		<td>pgrossmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>gross electric power (MW)</td>
	</tr>
	<tr>
		<td>pinjht</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power dissipated in heating and current drive system (MW)</td>
	</tr>
	<tr>
		<td>pinjmax</td>
		<td>real</td>
		<td>120.0D0</td>
		<td>maximum injector power during pulse (heating and ramp-up/down phase) (MW)</td>
	</tr>
	<tr>
		<td>pinjwp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>injector wall plug power (MW)</td>
	</tr>
	<tr>
		<td>pinjwpfix</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>secondary injector wall plug power (MW)</td>
	</tr>
	<tr>
		<td>pnetelmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>net electric power (MW)</td>
	</tr>
	<tr>
		<td>precircmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>recirculating electric power (MW)</td>
	</tr>
	<tr>
		<td>priheat</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total thermal power removed from fusion core (MW)</td>
	</tr>
	<tr>
		<td>psecdiv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Low-grade heat lost in divertor (MW)</td>
	</tr>
	<tr>
		<td>psechcd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Low-grade heat lost into HCD apparatus (MW)</td>
	</tr>
	<tr>
		<td>psechtmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Low-grade heat (MW)</td>
	</tr>
	<tr>
		<td>pseclossmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Low-grade heat (VV + lost)(MW)</td>
	</tr>
	<tr>
		<td>psecshld</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Low-grade heat deposited in shield (MW)</td>
	</tr>
	<tr>
		<td>pthermmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>High-grade heat useful for electric production (MW)</td>
	</tr>
	<tr>
		<td>pwpm2</td>
		<td>real</td>
		<td>150.0D0</td>
		<td>base AC power requirement per unit floor area (W/m2)</td>
	</tr>
	<tr>
		<td>tfacpd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total steady state TF coil AC power demand (MW)</td>
	</tr>
	<tr>
		<td>tlvpmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>estimate of total low voltage power (MW)</td>
	</tr>
	<tr>
		<td>trithtmw</td>
		<td>real</td>
		<td>15.0D0</td>
		<td>power required for tritium processing (MW)</td>
	</tr>
	<tr>
		<td>tturb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>coolant temperature at turbine inlet (K) (secondary_cycle = 3,4)</td>
	</tr>
	<tr>
		<td>vachtmw</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>vacuum pump power (MW)</td>
	</tr>
</table>

## ife_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>bldr</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>radial thickness of IFE blanket (m; calculated if ifetyp=4)</td>
	</tr>
	<tr>
		<td>bldrc</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>radial thickness of IFE curtain (m; ifetyp=4)</td>
	</tr>
	<tr>
		<td>bldzl</td>
		<td>real</td>
		<td>4.0D0</td>
		<td>vertical thickness of IFE blanket below chamber (m)</td>
	</tr>
	<tr>
		<td>bldzu</td>
		<td>real</td>
		<td>4.0D0</td>
		<td>vertical thickness of IFE blanket above chamber (m)</td>
	</tr>
	<tr>
		<td>blmatf</td>
		<td>real</td>
		<td>reshape((/0.05D0, 0.05D0, 0.05...</td>
		<td>IFE blanket material fractions</td>
	</tr>
	<tr>
		<td>blmatm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE blanket material masses (kg)</td>
	</tr>
	<tr>
		<td>blmatv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE blanket material volumes (m3)</td>
	</tr>
	<tr>
		<td>blvol</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>IFE blanket volume (m3)</td>
	</tr>
	<tr>
		<td>cdriv0</td>
		<td>real</td>
		<td>154.3D0</td>
		<td>IFE generic/laser driver cost at edrive=0 (M\$)</td>
	</tr>
	<tr>
		<td>cdriv1</td>
		<td>real</td>
		<td>163.2D0</td>
		<td>IFE low energy heavy ion beam driver cost extrapolated to edrive=0 (M\$)</td>
	</tr>
	<tr>
		<td>cdriv2</td>
		<td>real</td>
		<td>244.9D0</td>
		<td>IFE high energy heavy ion beam driver cost extrapolated to edrive=0 (M\$)</td>
	</tr>
	<tr>
		<td>cdriv3</td>
		<td>real</td>
		<td>1.463D0</td>
		<td>IFE driver cost (\$/J wall plug) (ifedrv==3)</td>
	</tr>
	<tr>
		<td>chdzl</td>
		<td>real</td>
		<td>9.0D0</td>
		<td>vertical thickness of IFE chamber below centre (m)</td>
	</tr>
	<tr>
		<td>chdzu</td>
		<td>real</td>
		<td>9.0D0</td>
		<td>vertical thickness of IFE chamber above centre (m)</td>
	</tr>
	<tr>
		<td>chmatf</td>
		<td>real</td>
		<td>(/1.0D0, 0.0D0, 0.0D0, 0.0D0, ...</td>
		<td>IFE chamber material fractions</td>
	</tr>
	<tr>
		<td>chmatm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE chamber material masses (kg)</td>
	</tr>
	<tr>
		<td>chmatv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE chamber material volumes (m3)</td>
	</tr>
	<tr>
		<td>chrad</td>
		<td>real</td>
		<td>6.5D0</td>
		<td>radius of IFE chamber (m) (iteration variable 84)</td>
	</tr>
	<tr>
		<td>chvol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE chamber volume (m3)</td>
	</tr>
	<tr>
		<td>dcdrv0</td>
		<td>real</td>
		<td>111.4D0</td>
		<td>IFE generic/laser driver cost gradient (M\$/MJ)</td>
	</tr>
	<tr>
		<td>dcdrv1</td>
		<td>real</td>
		<td>78.0D0</td>
		<td>HIB driver cost gradient at low energy (M\$/MJ)</td>
	</tr>
	<tr>
		<td>dcdrv2</td>
		<td>real</td>
		<td>59.9D0</td>
		<td>HIB driver cost gradient at high energy (M\$/MJ)</td>
	</tr>
	<tr>
		<td>drveff</td>
		<td>real</td>
		<td>0.28D0</td>
		<td>IFE driver wall plug to target efficiency (ifedrv=0,3) (iteration variable 82)</td>
	</tr>
	<tr>
		<td>edrive</td>
		<td>real</td>
		<td>5.0D6</td>
		<td>IFE driver energy (J) (iteration variable 81)</td>
	</tr>
	<tr>
		<td>etadrv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE driver wall plug to target efficiency</td>
	</tr>
	<tr>
		<td>etali</td>
		<td>real</td>
		<td>0.4D0</td>
		<td>IFE lithium pump wall plug efficiency (ifetyp=4)</td>
	</tr>
	<tr>
		<td>etave</td>
		<td>real</td>
		<td>(/0.082D0, 0.079D0, 0.076D0, 0...</td>
		<td>IFE driver efficiency vs driver energy (ifedrv=-1)</td>
	</tr>
	<tr>
		<td>fauxbop</td>
		<td>real</td>
		<td>0.06D0</td>
		<td>fraction of gross electric power to balance-of-plant (IFE)</td>
	</tr>
	<tr>
		<td>fbreed</td>
		<td>real</td>
		<td>0.51D0</td>
		<td>fraction of breeder external to device core</td>
	</tr>
	<tr>
		<td>fburn</td>
		<td>real</td>
		<td>0.3333D0</td>
		<td>IFE burn fraction (fraction of tritium fused/target)</td>
	</tr>
	<tr>
		<td>flirad</td>
		<td>real</td>
		<td>0.78D0</td>
		<td>radius of FLiBe/lithium inlet (m) (ifetyp=3,4)</td>
	</tr>
	<tr>
		<td>frrmax</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for maximum IFE repetition rate (constraint equation 50, iteration variable 86)</td>
	</tr>
	<tr>
		<td>fwdr</td>
		<td>real</td>
		<td>0.01D0</td>
		<td>radial thickness of IFE first wall (m)</td>
	</tr>
	<tr>
		<td>fwdzl</td>
		<td>real</td>
		<td>0.01D0</td>
		<td>vertical thickness of IFE first wall below chamber (m)</td>
	</tr>
	<tr>
		<td>fwdzu</td>
		<td>real</td>
		<td>0.01D0</td>
		<td>vertical thickness of IFE first wall above chamber (m)</td>
	</tr>
	<tr>
		<td>fwmatf</td>
		<td>real</td>
		<td>reshape((/0.05D0, 0.05D0, 0.05...</td>
		<td>IFE first wall material fractions</td>
	</tr>
	<tr>
		<td>fwmatm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE first wall material masses (kg)</td>
	</tr>
	<tr>
		<td>fwmatv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE first wall material volumes (kg)</td>
	</tr>
	<tr>
		<td>fwvol</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>IFE first wall volume (m3)</td>
	</tr>
	<tr>
		<td>gain</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE target gain</td>
	</tr>
	<tr>
		<td>gainve</td>
		<td>real</td>
		<td>(/60.0D0, 95.0D0, 115.0D0, 125...</td>
		<td>IFE target gain vs driver energy (ifedrv=-1)</td>
	</tr>
	<tr>
		<td>htpmw_ife</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE heat transport system electrical pump power (MW)</td>
	</tr>
	<tr>
		<td>ife</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for IFE option (set via device.dat):<br><ul><li> =0 use tokamak, RFP or stellarator model</li><li> =1 use IFE model</li></ul></td>
	</tr>
	<tr>
		<td>ifedrv</td>
		<td>integer</td>
		<td>2</td>
		<td>Switch for type of IFE driver:<br><ul><li> =-1 use gainve, etave for gain and driver efficiency</li><li> =0 use tgain, drveff for gain and driver efficiency</li><li> =1 use laser driver based on SOMBRERO design</li><li> =2 use heavy ion beam driver based on OSIRIS</li><li> =3 Input pfusife, rrin and drveff</li></ul></td>
	</tr>
	<tr>
		<td>ifetyp</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for type of IFE device build:<br><ul><li> =0 generic (cylindrical) build</li><li> =1 OSIRIS-like build</li><li> =2 SOMBRERO-like build</li><li> =3 HYLIFE-II-like build</li><li> =4 2019 build</li></ul></td>
	</tr>
	<tr>
		<td>lipmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE lithium pump power (MW; ifetyp=4)</td>
	</tr>
	<tr>
		<td>maxmat</td>
		<td>integer</td>
		<td>8</td>
		<td>Total number of materials in IFE device. Material numbers are as follows:<br><ul><li> =0 void</li><li> =1 steel</li><li> =2 carbon cloth</li><li> =3 FLiBe</li><li> =4 lithium oxide Li2O</li><li> =5 concrete</li><li> =6 helium</li><li> =7 xenon</li><li> =8 lithium</li></ul></td>
	</tr>
	<tr>
		<td>mcdriv</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>IFE driver cost multiplier</td>
	</tr>
	<tr>
		<td>mflibe</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total mass of FLiBe (kg)</td>
	</tr>
	<tr>
		<td>pdrive</td>
		<td>real</td>
		<td>23.0D6</td>
		<td>IFE driver power reaching target (W) (iteration variable 85)</td>
	</tr>
	<tr>
		<td>pfusife</td>
		<td>real</td>
		<td>1000.0D0</td>
		<td>IFE input fusion power (MW) (ifedrv=3 only; itv 155)</td>
	</tr>
	<tr>
		<td>pifecr</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>IFE cryogenic power requirements (MW)</td>
	</tr>
	<tr>
		<td>ptargf</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>IFE target factory power at 6 Hz repetition rate (MW)</td>
	</tr>
	<tr>
		<td>r1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>r2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>r3</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>r4</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>r5</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>r6</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>r7</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE device radial build (m)</td>
	</tr>
	<tr>
		<td>reprat</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE driver repetition rate (Hz)</td>
	</tr>
	<tr>
		<td>rrin</td>
		<td>real</td>
		<td>6.0D0</td>
		<td>Input IFE repetition rate (Hz) (ifedrv=3 only; itv 156)</td>
	</tr>
	<tr>
		<td>rrmax</td>
		<td>real</td>
		<td>20.0D0</td>
		<td>maximum IFE repetition rate (Hz)</td>
	</tr>
	<tr>
		<td>shdr</td>
		<td>real</td>
		<td>1.7D0</td>
		<td>radial thickness of IFE shield (m)</td>
	</tr>
	<tr>
		<td>shdzl</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>vertical thickness of IFE shield below chamber (m)</td>
	</tr>
	<tr>
		<td>shdzu</td>
		<td>real</td>
		<td>5.0D0</td>
		<td>vertical thickness of IFE shield above chamber (m)</td>
	</tr>
	<tr>
		<td>shmatf</td>
		<td>real</td>
		<td>reshape((/0.05D0, 0.05D0, 0.05...</td>
		<td>IFE shield material fractions</td>
	</tr>
	<tr>
		<td>shmatm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE shield material masses (kg)</td>
	</tr>
	<tr>
		<td>shmatv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE shield material volumes (kg)</td>
	</tr>
	<tr>
		<td>shvol</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>IFE shield volume (m3)</td>
	</tr>
	<tr>
		<td>sombdr</td>
		<td>real</td>
		<td>2.7D0</td>
		<td>radius of cylindrical blanket section below chamber (ifetyp=2)</td>
	</tr>
	<tr>
		<td>somtdr</td>
		<td>real</td>
		<td>2.7D0</td>
		<td>radius of cylindrical blanket section above chamber (ifetyp=2)</td>
	</tr>
	<tr>
		<td>taufall</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Lithium Fall Time (s)</td>
	</tr>
	<tr>
		<td>tdspmw</td>
		<td>real</td>
		<td>0.01D0</td>
		<td>IFE target delivery system power (MW)</td>
	</tr>
	<tr>
		<td>tfacmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE target factory power (MW)</td>
	</tr>
	<tr>
		<td>tgain</td>
		<td>real</td>
		<td>85.0D0</td>
		<td>IFE target gain (if ifedrv = 0) (iteration variable 83)</td>
	</tr>
	<tr>
		<td>uccarb</td>
		<td>real</td>
		<td>50.0D0</td>
		<td>cost of carbon cloth (\$/kg)</td>
	</tr>
	<tr>
		<td>ucconc</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>cost of concrete (\$/kg)</td>
	</tr>
	<tr>
		<td>ucflib</td>
		<td>real</td>
		<td>84.0D0</td>
		<td>cost of FLiBe (\$/kg)</td>
	</tr>
	<tr>
		<td>uctarg</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>cost of IFE target (\$/target)</td>
	</tr>
	<tr>
		<td>v1dr</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radial thickness of IFE void between first wall and blanket (m)</td>
	</tr>
	<tr>
		<td>v1dzl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vertical thickness of IFE void 1 below chamber (m)</td>
	</tr>
	<tr>
		<td>v1dzu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vertical thickness of IFE void 1 above chamber (m)</td>
	</tr>
	<tr>
		<td>v1matf</td>
		<td>real</td>
		<td>reshape((/1.0D0, 1.0D0, 1.0D0,...</td>
		<td>IFE void 1 material fractions</td>
	</tr>
	<tr>
		<td>v1matm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE void 1 material masses (kg)</td>
	</tr>
	<tr>
		<td>v1matv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE void 1 material volumes (kg)</td>
	</tr>
	<tr>
		<td>v1vol</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>IFE void 1 volume (m3)</td>
	</tr>
	<tr>
		<td>v2dr</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>radial thickness of IFE void between blanket and shield (m)</td>
	</tr>
	<tr>
		<td>v2dzl</td>
		<td>real</td>
		<td>7.0D0</td>
		<td>vertical thickness of IFE void 2 below chamber (m)</td>
	</tr>
	<tr>
		<td>v2dzu</td>
		<td>real</td>
		<td>7.0D0</td>
		<td>vertical thickness of IFE void 2 above chamber (m)</td>
	</tr>
	<tr>
		<td>v2matf</td>
		<td>real</td>
		<td>reshape((/1.0D0, 1.0D0, 1.0D0,...</td>
		<td>IFE void 2 material fractions</td>
	</tr>
	<tr>
		<td>v2matm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE void 2 material masses (kg)</td>
	</tr>
	<tr>
		<td>v2matv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE void 2 material volumes (kg)</td>
	</tr>
	<tr>
		<td>v2vol</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>IFE void 2 volume (m3)</td>
	</tr>
	<tr>
		<td>v3dr</td>
		<td>real</td>
		<td>43.3D0</td>
		<td>radial thickness of IFE void outside shield (m)</td>
	</tr>
	<tr>
		<td>v3dzl</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>vertical thickness of IFE void 3 below chamber (m)</td>
	</tr>
	<tr>
		<td>v3dzu</td>
		<td>real</td>
		<td>20.0D0</td>
		<td>vertical thickness of IFE void 3 above chamber (m)</td>
	</tr>
	<tr>
		<td>v3matf</td>
		<td>real</td>
		<td>reshape((/1.0D0, 1.0D0, 1.0D0,...</td>
		<td>IFE void 3 material fractions</td>
	</tr>
	<tr>
		<td>v3matm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE void 3 material masses (kg)</td>
	</tr>
	<tr>
		<td>v3matv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE void 3 material volumes (kg)</td>
	</tr>
	<tr>
		<td>v3vol</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0']</td>
		<td>IFE void 3 volume (m3)</td>
	</tr>
	<tr>
		<td>zl1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zl2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zl3</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zl4</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zl5</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zl6</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zl7</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build below centre (m)</td>
	</tr>
	<tr>
		<td>zu1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
	<tr>
		<td>zu2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
	<tr>
		<td>zu3</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
	<tr>
		<td>zu4</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
	<tr>
		<td>zu5</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
	<tr>
		<td>zu6</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
	<tr>
		<td>zu7</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>IFE vertical build above centre (m)</td>
	</tr>
</table>

## impurity_radiation_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>INSTALLDIR</td>
		<td>character</td>
		<td>"/home/skahn/Linux_env/process...</td>
		<td>impdir /'/home/PROCESS/[branch]/impuritydata'/ :<br>          Directory containing impurity radiation data files</td>
	</tr>
	<tr>
		<td>ROOTDIR</td>
		<td>character</td>
		<td>"/home/skahn/Linux_env/process...</td>
		<td></td>
	</tr>
	<tr>
		<td>coreradiationfraction</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>coreradiationfraction /1.0/ : fraction of radiation from 'core' region that is subtracted from the loss power<br>fimp(nimp) /1.0,0.1,0.02,0.0,0.0,0.0,0.0,0.0,0.0016,0.0,0.0,0.0,0.0,0.0/ :<br>        impurity number density fractions relative to electron density<br>        (iteration variable 102 is fimp(impvar))</td>
	</tr>
	<tr>
		<td>coreradius</td>
		<td>real</td>
		<td>0.6D0</td>
		<td>coreradius /0.6/ : normalised radius defining the 'core' region</td>
	</tr>
	<tr>
		<td>fimp</td>
		<td>real</td>
		<td>(/1.0D0, 0.1D0, 0.0D0, 0.0D0, ...</td>
		<td></td>
	</tr>
	<tr>
		<td>fimpvar</td>
		<td>real</td>
		<td>1.0D-3</td>
		<td></td>
	</tr>
	<tr>
		<td>imp_label</td>
		<td>character</td>
		<td>(/'H_', 'He', 'Be', 'C_', 'N_'...</td>
		<td>imp_label(nimp) : impurity ion species names:<br>  ( 1)  Hydrogen  (fraction calculated by code)<br>  ( 2)  Helium<br>  ( 3)  Beryllium<br>  ( 4)  Carbon<br>  ( 5)  Nitrogen<br>  ( 6)  Oxygen<br>  ( 7)  Neon<br>  ( 8)  Silicon<br>  ( 9)  Argon<br>  (10)  Iron<br>  (11)  Nickel<br>  (12)  Krypton<br>  (13)  Xenon<br>  (14)  Tungsten<br>fimpvar /1.0e-3/ : impurity fraction to be used as fimp(impvar)<br>                    (iteration variable 102)</td>
	</tr>
	<tr>
		<td>impdir</td>
		<td>character</td>
		<td>INSTALLDIR//'/data/impuritydat...</td>
		<td>impvar : impurity to be iterated (deprecated)<br>                      variable number 102 is turned on</td>
	</tr>
	<tr>
		<td>impurity_arr</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>impvar</td>
		<td>integer</td>
		<td>9</td>
		<td></td>
	</tr>
	<tr>
		<td>nimp</td>
		<td>integer</td>
		<td>14</td>
		<td>nimp /14/ FIX : number of ion species in impurity radiation model</td>
	</tr>
</table>

## kit_blanket_model
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>A_FW_IB</td>
		<td>real</td>
		<td>3.5196D6</td>
		<td></td>
	</tr>
	<tr>
		<td>A_FW_OB</td>
		<td>real</td>
		<td>9.0504D6</td>
		<td></td>
	</tr>
	<tr>
		<td>A_FW_PPCS</td>
		<td>real</td>
		<td>1253.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>A_VV_IB</td>
		<td>real</td>
		<td>3.8220D6</td>
		<td></td>
	</tr>
	<tr>
		<td>A_VV_OB</td>
		<td>real</td>
		<td>9.8280D6</td>
		<td></td>
	</tr>
	<tr>
		<td>A_bl_IB</td>
		<td>real</td>
		<td>3.4844D6</td>
		<td></td>
	</tr>
	<tr>
		<td>A_bl_OB</td>
		<td>real</td>
		<td>8.9599D6</td>
		<td></td>
	</tr>
	<tr>
		<td>A_cov_PPCS</td>
		<td>real</td>
		<td>1365.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>CF_bl</td>
		<td>real</td>
		<td>91.7949D0</td>
		<td></td>
	</tr>
	<tr>
		<td>CF_bl_PPCS</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>D_EU_max</td>
		<td>real</td>
		<td>60.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>E_n</td>
		<td>real</td>
		<td>14.1D0</td>
		<td></td>
	</tr>
	<tr>
		<td>G_tot</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>Gamma_He_0_ref</td>
		<td>real</td>
		<td>1.8D-3</td>
		<td></td>
	</tr>
	<tr>
		<td>H_CD_ports</td>
		<td>character</td>
		<td>'small'</td>
		<td></td>
	</tr>
	<tr>
		<td>M_E</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>NWL_av</td>
		<td>real</td>
		<td>1.94D0</td>
		<td></td>
	</tr>
	<tr>
		<td>NWL_av_IB_PPCS</td>
		<td>real</td>
		<td>1.73D0</td>
		<td></td>
	</tr>
	<tr>
		<td>NWL_av_OB_PPCS</td>
		<td>real</td>
		<td>1.92D0</td>
		<td></td>
	</tr>
	<tr>
		<td>NWL_av_PPCS</td>
		<td>real</td>
		<td>1.94D0</td>
		<td></td>
	</tr>
	<tr>
		<td>NWL_max_IB_PPCS</td>
		<td>real</td>
		<td>1.99D0</td>
		<td></td>
	</tr>
	<tr>
		<td>NWL_max_OB_PPCS</td>
		<td>real</td>
		<td>2.41D0</td>
		<td></td>
	</tr>
	<tr>
		<td>N_Av</td>
		<td>real</td>
		<td>6.02D23</td>
		<td></td>
	</tr>
	<tr>
		<td>PA_T</td>
		<td>real</td>
		<td>3.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>P_n</td>
		<td>real</td>
		<td>2720.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>P_th_tot</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>TBR_PPCS</td>
		<td>real</td>
		<td>1.12D0</td>
		<td></td>
	</tr>
	<tr>
		<td>alpha_BM_IB</td>
		<td>real</td>
		<td>40.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>alpha_BM_OB</td>
		<td>real</td>
		<td>40.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>alpha_BP_IB</td>
		<td>real</td>
		<td>65.95D0</td>
		<td></td>
	</tr>
	<tr>
		<td>alpha_BP_OB</td>
		<td>real</td>
		<td>67.13D0</td>
		<td></td>
	</tr>
	<tr>
		<td>alpha_m</td>
		<td>real</td>
		<td>0.75D0</td>
		<td></td>
	</tr>
	<tr>
		<td>alpha_puls</td>
		<td>real</td>
		<td>1.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>breeder</td>
		<td>character</td>
		<td>'Orthosilicate'</td>
		<td></td>
	</tr>
	<tr>
		<td>chi_Be_BZ_IB</td>
		<td>real</td>
		<td>69.2D0</td>
		<td></td>
	</tr>
	<tr>
		<td>chi_Be_BZ_OB</td>
		<td>real</td>
		<td>69.2D0</td>
		<td></td>
	</tr>
	<tr>
		<td>chi_breed_BZ_IB</td>
		<td>real</td>
		<td>15.4D0</td>
		<td></td>
	</tr>
	<tr>
		<td>chi_breed_BZ_OB</td>
		<td>real</td>
		<td>15.4D0</td>
		<td></td>
	</tr>
	<tr>
		<td>chi_steels_BZ_IB</td>
		<td>real</td>
		<td>9.8D0</td>
		<td></td>
	</tr>
	<tr>
		<td>chi_steels_BZ_OB</td>
		<td>real</td>
		<td>9.8D0</td>
		<td></td>
	</tr>
	<tr>
		<td>e_Li</td>
		<td>real</td>
		<td>30.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>e_Li_PPCS</td>
		<td>real</td>
		<td>30.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>f_peak</td>
		<td>real</td>
		<td>1.21D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_EU</td>
		<td>real</td>
		<td>11.57D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_He_VV</td>
		<td>real</td>
		<td>7.6002D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_n_BZ_IB</td>
		<td>real</td>
		<td>18.79D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_n_BZ_OB</td>
		<td>real</td>
		<td>19.19D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_n_VV</td>
		<td>real</td>
		<td>8.153D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BM_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BM_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BP_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BP_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BZ_Be_IB</td>
		<td>real</td>
		<td>16.39D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BZ_Be_OB</td>
		<td>real</td>
		<td>16.39D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BZ_breed_IB</td>
		<td>real</td>
		<td>29.42D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BZ_breed_OB</td>
		<td>real</td>
		<td>27.03D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BZ_steels_IB</td>
		<td>real</td>
		<td>21.27D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_BZ_steels_OB</td>
		<td>real</td>
		<td>21.27D0</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_q_VV</td>
		<td>real</td>
		<td>6.92D0</td>
		<td></td>
	</tr>
	<tr>
		<td>n_ports_H_CD_IB</td>
		<td>integer</td>
		<td>2</td>
		<td></td>
	</tr>
	<tr>
		<td>n_ports_H_CD_OB</td>
		<td>integer</td>
		<td>2</td>
		<td></td>
	</tr>
	<tr>
		<td>n_ports_div</td>
		<td>integer</td>
		<td>2</td>
		<td></td>
	</tr>
	<tr>
		<td>nflutfi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nflutfo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>np</td>
		<td>integer</td>
		<td>2</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_0_n_BZ_IB</td>
		<td>real</td>
		<td>5.12D14</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_0_n_BZ_OB</td>
		<td>real</td>
		<td>5.655D14</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_n_0_VV_ref</td>
		<td>real</td>
		<td>2.0D10</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_n_vv_IB_start</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_n_vv_OB_start</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnucsh</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnuctfi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnuctfo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_0_BZ_Be_IB</td>
		<td>real</td>
		<td>9.532D0</td>
		<td></td>
	</tr>
	<tr>
		<td>q_0_BZ_Be_OB</td>
		<td>real</td>
		<td>11.809D0</td>
		<td></td>
	</tr>
	<tr>
		<td>q_0_BZ_breed_IB</td>
		<td>real</td>
		<td>31.348D0</td>
		<td></td>
	</tr>
	<tr>
		<td>q_0_BZ_breed_OB</td>
		<td>real</td>
		<td>37.144D0</td>
		<td></td>
	</tr>
	<tr>
		<td>q_0_BZ_steels_IB</td>
		<td>real</td>
		<td>16.067D0</td>
		<td></td>
	</tr>
	<tr>
		<td>q_0_BZ_steels_OB</td>
		<td>real</td>
		<td>18.788D0</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BM_IB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BM_OB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BP_IB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BP_OB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BZ_IB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BZ_OB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BM_IB</td>
		<td>real</td>
		<td>17.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BM_OB</td>
		<td>real</td>
		<td>27.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BP_IB</td>
		<td>real</td>
		<td>30.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BP_OB</td>
		<td>real</td>
		<td>35.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BZ_IB</td>
		<td>real</td>
		<td>36.5D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BZ_IB_PPCS</td>
		<td>real</td>
		<td>36.5D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BZ_OB</td>
		<td>real</td>
		<td>46.5D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BZ_OB_PPCS</td>
		<td>real</td>
		<td>46.5D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_FW_IB</td>
		<td>real</td>
		<td>2.3D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_FW_OB</td>
		<td>real</td>
		<td>2.3D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_VV_IB</td>
		<td>real</td>
		<td>35.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_VV_OB</td>
		<td>real</td>
		<td>65.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>t_bl_fpy</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>t_bl_y</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>t_plant</td>
		<td>real</td>
		<td>40.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>tbratio</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vvhemaxi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vvhemaxo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vvhemini</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vvhemino</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BM_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BM_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BP_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BP_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BZ_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BZ_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_VV_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_VV_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## kit_hcll_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>P_pump_0</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>P_th_0</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>TBR_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>TBR_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>TPR_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>TPR_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>T_He_out</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>T_he_in</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>area_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>area_fw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>area_fw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bldepti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bldepto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bllengi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bllengo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>blwidti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>blwidto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bp_ratio_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bp_ratio_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bz_ratio_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bz_ratio_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bzfllengi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bzfllengo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>cf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>cp_he</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>denhe</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>denpbli</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>double</td>
		<td>integer</td>
		<td>8</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bb_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bb_ib_max</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bb_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bb_ob_max</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bu_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bu_max</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bu_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dp_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dr_bb_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dr_bb_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dr_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dr_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dr_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dr_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bb_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bb_ib_max</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bb_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bb_ob_max</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bu_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bu_max</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bu_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>dt_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>emult_all</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ff_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ff_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_div_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_div_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_he_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_he_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_he_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_pbli_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_pbli_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_steel_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_steel_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_steel_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>frac_vol_w_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>h_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>h_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>hblnkt</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmw_blkti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmw_blkto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmw_fwi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmw_fwo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ip</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>j_plus_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>j_plus_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>len_act_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>len_act_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>len_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>len_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_blanket</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_he_blanket</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_he_segm_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_he_segm_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_pbli_blanket</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_pbli_segm_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_pbli_segm_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_sector</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_segm_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_segm_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_steel_blanket</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_steel_segm_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_steel_segm_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_w_blanket</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_w_segm_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mass_w_segm_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mfblkt</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mfblkti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mfblkto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mfblktpi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mfblktpo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mffw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mffwi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mffwo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mffwpi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mffwpo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>mftotal</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_bu_pol_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_bu_pol_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_bu_tor_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_bu_tor_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_pol_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_pol_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_tor_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nb_tor_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>npblkti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>npblkto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>npfwi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>npfwo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ofile</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_tfc_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_tfc_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnuc_bkt_ratio</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnuc_fw_ratio</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnucblkti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnucblkto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnucfwi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnucfwo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>psurffwi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>psurffwo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>r_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>r_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rad_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rad_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_bp_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_bp_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_bss_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_bss_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_cap_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_cap_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_fw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_fw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_sw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>thick_sw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>tpeakfwi</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>tpeakfwo</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>velblkti</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>velblkto</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_fw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_fw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_fw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_fw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_he_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_pbli_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_pbli_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_pbli_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_pbli_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_pbli_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_pbli_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_bz</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_bz_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_bz_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_fw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_fw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_fw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_mf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_mf_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_steel_mf_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_w_fw_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vol_w_fw_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>w_he</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>z_ib</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>z_ob</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## kit_hcpb_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>A_FW_IB</td>
		<td>real</td>
		<td>3.5196D6</td>
		<td>IB first wall area [cm^2]</td>
	</tr>
	<tr>
		<td>A_FW_OB</td>
		<td>real</td>
		<td>9.0504D6</td>
		<td>OB first wall area [cm^2]</td>
	</tr>
	<tr>
		<td>A_FW_PPCS</td>
		<td>real</td>
		<td>1253.0D0</td>
		<td>First wall area [m^2]</td>
	</tr>
	<tr>
		<td>A_VV_IB</td>
		<td>real</td>
		<td>3.8220D6</td>
		<td>IB shield/VV area [cm^2]</td>
	</tr>
	<tr>
		<td>A_VV_OB</td>
		<td>real</td>
		<td>9.8280D6</td>
		<td>OB shield/VV area [cm^2]</td>
	</tr>
	<tr>
		<td>A_bl_IB</td>
		<td>real</td>
		<td>3.4844D6</td>
		<td>IB blanket area [cm^2]</td>
	</tr>
	<tr>
		<td>A_bl_OB</td>
		<td>real</td>
		<td>8.9599D6</td>
		<td>OB blanket area [cm^2]</td>
	</tr>
	<tr>
		<td>A_cov_PPCS</td>
		<td>real</td>
		<td>1365.0D0</td>
		<td>Total blanket coverage area [m^2]</td>
	</tr>
	<tr>
		<td>CF_bl</td>
		<td>real</td>
		<td>91.7949D0</td>
		<td>Blanket coverage factor [%]</td>
	</tr>
	<tr>
		<td>CF_bl_PPCS</td>
		<td>real</td>
		<td>None</td>
		<td>Blanket coverage factor (calculated) [%]</td>
	</tr>
	<tr>
		<td>D_EU_max</td>
		<td>real</td>
		<td>60.0D0</td>
		<td>Allowable neutron damage to the FW EUROFER [dpa]</td>
	</tr>
	<tr>
		<td>E_n</td>
		<td>real</td>
		<td>14.1D0</td>
		<td>[MeV] Average neutron energy</td>
	</tr>
	<tr>
		<td>G_tot</td>
		<td>real</td>
		<td>None</td>
		<td>Tritium production rate [g/day]</td>
	</tr>
	<tr>
		<td>Gamma_He_0_ref</td>
		<td>real</td>
		<td>1.8D-3</td>
		<td>Pre-exp term [appm/yr]</td>
	</tr>
	<tr>
		<td>H_CD_ports</td>
		<td>character</td>
		<td>'small'</td>
		<td>Type of H&amp;CD ports (small or large)</td>
	</tr>
	<tr>
		<td>M_E</td>
		<td>real</td>
		<td>None</td>
		<td>Energy multiplication factor [--]</td>
	</tr>
	<tr>
		<td>NWL_av</td>
		<td>real</td>
		<td>1.94D0</td>
		<td>Average neutron wall load [MW/m^2]</td>
	</tr>
	<tr>
		<td>NWL_av_IB_PPCS</td>
		<td>real</td>
		<td>1.73D0</td>
		<td>Average IB wall load [MW/m^2]</td>
	</tr>
	<tr>
		<td>NWL_av_OB_PPCS</td>
		<td>real</td>
		<td>1.92D0</td>
		<td>Average OB wall load [MW/m^2]</td>
	</tr>
	<tr>
		<td>NWL_av_PPCS</td>
		<td>real</td>
		<td>1.94D0</td>
		<td>Average neutron wall load [MW/m^2]</td>
	</tr>
	<tr>
		<td>NWL_max_IB_PPCS</td>
		<td>real</td>
		<td>1.99D0</td>
		<td>Maximum IB wall load [MW/m^2]</td>
	</tr>
	<tr>
		<td>NWL_max_OB_PPCS</td>
		<td>real</td>
		<td>2.41D0</td>
		<td>Maximum OB wall load [MW/m^2]</td>
	</tr>
	<tr>
		<td>N_Av</td>
		<td>real</td>
		<td>6.02D23</td>
		<td>[at/mol] Avogadro number</td>
	</tr>
	<tr>
		<td>PA_T</td>
		<td>real</td>
		<td>3.0D0</td>
		<td>[g/mol] Tritium atomic weight</td>
	</tr>
	<tr>
		<td>P_n</td>
		<td>real</td>
		<td>2720.0D0</td>
		<td>Fusion neutron power [MW]</td>
	</tr>
	<tr>
		<td>P_th_tot</td>
		<td>real</td>
		<td>None</td>
		<td>Nuclear power generated in blanket [MW]</td>
	</tr>
	<tr>
		<td>alpha_BM_IB</td>
		<td>real</td>
		<td>40.0D0</td>
		<td>Helium fraction in the IB BM [%]</td>
	</tr>
	<tr>
		<td>alpha_BM_OB</td>
		<td>real</td>
		<td>40.0D0</td>
		<td>Helium fraction in the OB BM [%]</td>
	</tr>
	<tr>
		<td>alpha_BP_IB</td>
		<td>real</td>
		<td>65.95D0</td>
		<td>Helium fraction in the IB BP [%]</td>
	</tr>
	<tr>
		<td>alpha_BP_OB</td>
		<td>real</td>
		<td>67.13D0</td>
		<td>Helium fraction in the OB BP [%]</td>
	</tr>
	<tr>
		<td>alpha_m</td>
		<td>real</td>
		<td>0.75D0</td>
		<td>Availability factor [--]</td>
	</tr>
	<tr>
		<td>alpha_puls</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Pulsed regime fraction [--]</td>
	</tr>
	<tr>
		<td>breeder</td>
		<td>character</td>
		<td>'Orthosilicate'</td>
		<td>Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)</td>
	</tr>
	<tr>
		<td>chi_Be_BZ_IB</td>
		<td>real</td>
		<td>69.2D0</td>
		<td>Beryllium vol. frac. in IB BZ [%]</td>
	</tr>
	<tr>
		<td>chi_Be_BZ_OB</td>
		<td>real</td>
		<td>69.2D0</td>
		<td>Beryllium vol. frac. in OB BZ [%]</td>
	</tr>
	<tr>
		<td>chi_breed_BZ_IB</td>
		<td>real</td>
		<td>15.4D0</td>
		<td>Breeder vol. frac. in IB BZ [%]</td>
	</tr>
	<tr>
		<td>chi_breed_BZ_OB</td>
		<td>real</td>
		<td>15.4D0</td>
		<td>Breeder vol. frac. in OB BZ [%]</td>
	</tr>
	<tr>
		<td>chi_steels_BZ_IB</td>
		<td>real</td>
		<td>9.8D0</td>
		<td>Steels vol. frac. in IB BZ [%]</td>
	</tr>
	<tr>
		<td>chi_steels_BZ_OB</td>
		<td>real</td>
		<td>9.8D0</td>
		<td>Steels vol. frac. in OB BZ [%]</td>
	</tr>
	<tr>
		<td>e_Li</td>
		<td>real</td>
		<td>60.0D0</td>
		<td>Lithium 6 enrichment [%]</td>
	</tr>
	<tr>
		<td>f_peak</td>
		<td>real</td>
		<td>1.21D0</td>
		<td>NWL peaking factor [--]</td>
	</tr>
	<tr>
		<td>hblnkt</td>
		<td>real</td>
		<td>None</td>
		<td>Blanket internal half-height (m)</td>
	</tr>
	<tr>
		<td>hcryopf</td>
		<td>real</td>
		<td>None</td>
		<td>Clearance between uppermost PF coil and cryostat lid (m)</td>
	</tr>
	<tr>
		<td>hshld</td>
		<td>real</td>
		<td>None</td>
		<td>Shield internal half-height (m)</td>
	</tr>
	<tr>
		<td>hvv</td>
		<td>real</td>
		<td>None</td>
		<td>Vacuum vessel internal half-height (m)</td>
	</tr>
	<tr>
		<td>ip</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lambda_EU</td>
		<td>real</td>
		<td>11.57D0</td>
		<td>Decay length in EUROFER [cm]</td>
	</tr>
	<tr>
		<td>lambda_He_VV</td>
		<td>real</td>
		<td>7.6002D0</td>
		<td>Decay length [cm]</td>
	</tr>
	<tr>
		<td>lambda_n_BZ_IB</td>
		<td>real</td>
		<td>18.79D0</td>
		<td>Decay length in IB BZ [cm]</td>
	</tr>
	<tr>
		<td>lambda_n_BZ_OB</td>
		<td>real</td>
		<td>19.19D0</td>
		<td>Decay length in OB BZ [cm]</td>
	</tr>
	<tr>
		<td>lambda_n_VV</td>
		<td>real</td>
		<td>8.153D0</td>
		<td>Decay length in VV [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BM_IB</td>
		<td>real</td>
		<td>None</td>
		<td>Decay length in IB BM (calculated) [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BM_OB</td>
		<td>real</td>
		<td>None</td>
		<td>Decay length in OB BM (calculated) [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BP_IB</td>
		<td>real</td>
		<td>None</td>
		<td>Decay length in IB BP (calculated) [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BP_OB</td>
		<td>real</td>
		<td>None</td>
		<td>Decay length in OB BP (calculated) [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BZ_Be_IB</td>
		<td>real</td>
		<td>21.19D0</td>
		<td>Decay length in IB BZ Beryllium [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BZ_Be_OB</td>
		<td>real</td>
		<td>19.33D0</td>
		<td>Decay length in OB BZ Beryllium [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BZ_breed_IB</td>
		<td>real</td>
		<td>44.56D0</td>
		<td>Decay length in IB BZ breeder [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BZ_breed_OB</td>
		<td>real</td>
		<td>28.37D0</td>
		<td>Decay length in OB BZ breeder [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BZ_steels_IB</td>
		<td>real</td>
		<td>21.59D0</td>
		<td>Decay length in IB BZ steels [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_BZ_steels_OB</td>
		<td>real</td>
		<td>20.61D0</td>
		<td>Decay length in OB BZ steels [cm]</td>
	</tr>
	<tr>
		<td>lambda_q_VV</td>
		<td>real</td>
		<td>6.92D0</td>
		<td>Decay length in Vacuum Vessel [cm]</td>
	</tr>
	<tr>
		<td>n_ports_H_CD_IB</td>
		<td>integer</td>
		<td>2</td>
		<td>Number of IB H&amp;CD ports [ports]</td>
	</tr>
	<tr>
		<td>n_ports_H_CD_OB</td>
		<td>integer</td>
		<td>2</td>
		<td>Number of OB H&amp;CD ports [ports]</td>
	</tr>
	<tr>
		<td>n_ports_div</td>
		<td>integer</td>
		<td>2</td>
		<td>Number of divertor ports [ports]</td>
	</tr>
	<tr>
		<td>nflutfi</td>
		<td>real</td>
		<td>None</td>
		<td>Fast neutron fluence on IB TF coil [n/cm2]</td>
	</tr>
	<tr>
		<td>nflutfo</td>
		<td>real</td>
		<td>None</td>
		<td>Fast neutron fluence on OB TF coil [n/cm2]</td>
	</tr>
	<tr>
		<td>np</td>
		<td>integer</td>
		<td>2</td>
		<td>Array length</td>
	</tr>
	<tr>
		<td>ofile</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_0_n_BZ_IB</td>
		<td>real</td>
		<td>5.12D14</td>
		<td>Pre-exp term in IB BZ [n/cm^2/sec]</td>
	</tr>
	<tr>
		<td>phi_0_n_BZ_OB</td>
		<td>real</td>
		<td>5.655D14</td>
		<td>Pre-exp term in OB BZ [n/cm^2/sec]</td>
	</tr>
	<tr>
		<td>phi_n_0_VV_ref</td>
		<td>real</td>
		<td>2.0D10</td>
		<td>Reference fast neutron flux on VV inner side [Fish09] [n/cm^2/sec]</td>
	</tr>
	<tr>
		<td>phi_n_vv_IB_start</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>phi_n_vv_OB_start</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pnucsh</td>
		<td>real</td>
		<td>None</td>
		<td>Nuclear power generated in shield/VV [MW]</td>
	</tr>
	<tr>
		<td>pnuctfi</td>
		<td>real</td>
		<td>None</td>
		<td>Nuclear heating on IB TF coil [MW/m3]</td>
	</tr>
	<tr>
		<td>pnuctfo</td>
		<td>real</td>
		<td>None</td>
		<td>Nuclear heating on OB TF coil [MW/m3]</td>
	</tr>
	<tr>
		<td>q_0_BZ_Be_IB</td>
		<td>real</td>
		<td>7.5D0</td>
		<td>Pre-exp term in IB BZ Beryllium [W/cm^3]</td>
	</tr>
	<tr>
		<td>q_0_BZ_Be_OB</td>
		<td>real</td>
		<td>8.85D0</td>
		<td>Pre-exp term in OB BZ Beryllium [W/cm^3]</td>
	</tr>
	<tr>
		<td>q_0_BZ_breed_IB</td>
		<td>real</td>
		<td>23.41D0</td>
		<td>Pre-exp term in IB BZ breeder [W/cm^3]</td>
	</tr>
	<tr>
		<td>q_0_BZ_breed_OB</td>
		<td>real</td>
		<td>28.16D0</td>
		<td>Pre-exp term in OB BZ breeder [W/cm^3]</td>
	</tr>
	<tr>
		<td>q_0_BZ_steels_IB</td>
		<td>real</td>
		<td>9.04D0</td>
		<td>Pre-exp term in IB BZ steels [W/cm^3]</td>
	</tr>
	<tr>
		<td>q_0_BZ_steels_OB</td>
		<td>real</td>
		<td>9.93D0</td>
		<td>Pre-exp term in OB BZ steels [W/cm^3]</td>
	</tr>
	<tr>
		<td>q_BM_IB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BM_OB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BP_IB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BP_OB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BZ_IB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>q_BZ_OB_end</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>t_BM_IB</td>
		<td>real</td>
		<td>17.0D0</td>
		<td>BM thickness [cm]</td>
	</tr>
	<tr>
		<td>t_BM_OB</td>
		<td>real</td>
		<td>27.0D0</td>
		<td>BM thickness [cm]</td>
	</tr>
	<tr>
		<td>t_BP_IB</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>BP thickness [cm]</td>
	</tr>
	<tr>
		<td>t_BP_OB</td>
		<td>real</td>
		<td>35.0D0</td>
		<td>BP thickness [cm]</td>
	</tr>
	<tr>
		<td>t_BZ_IB</td>
		<td>real</td>
		<td>36.5D0</td>
		<td>BZ thickness [cm]</td>
	</tr>
	<tr>
		<td>t_BZ_OB</td>
		<td>real</td>
		<td>46.5D0</td>
		<td>BZ thickness [cm]</td>
	</tr>
	<tr>
		<td>t_FW_IB</td>
		<td>real</td>
		<td>2.3D0</td>
		<td>IB first wall thickness [cm]</td>
	</tr>
	<tr>
		<td>t_FW_OB</td>
		<td>real</td>
		<td>2.3D0</td>
		<td>OB first wall thickness [cm]</td>
	</tr>
	<tr>
		<td>t_VV_IB</td>
		<td>real</td>
		<td>35.0D0</td>
		<td>VV thickness [cm]</td>
	</tr>
	<tr>
		<td>t_VV_OB</td>
		<td>real</td>
		<td>65.0D0</td>
		<td>VV thickness [cm]</td>
	</tr>
	<tr>
		<td>t_bl_fpy</td>
		<td>real</td>
		<td>None</td>
		<td>blanket lifetime in full power years [y]</td>
	</tr>
	<tr>
		<td>t_bl_y</td>
		<td>real</td>
		<td>None</td>
		<td>blanket lifetime in calendar years [y]</td>
	</tr>
	<tr>
		<td>t_plant</td>
		<td>real</td>
		<td>40.0D0</td>
		<td>Plant lifetime [FPY]</td>
	</tr>
	<tr>
		<td>tbratio</td>
		<td>real</td>
		<td>None</td>
		<td>Tritium breeding ratio [--]</td>
	</tr>
	<tr>
		<td>vfblkti</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard void fraction of blanket</td>
	</tr>
	<tr>
		<td>vfblkto</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard/outboard void fraction of blanket</td>
	</tr>
	<tr>
		<td>volshldi</td>
		<td>real</td>
		<td>None</td>
		<td>Volume of inboard and outboard shield (m3)</td>
	</tr>
	<tr>
		<td>volshldo</td>
		<td>real</td>
		<td>None</td>
		<td>Volume of inboard and outboard shield (m3)</td>
	</tr>
	<tr>
		<td>vvhemaxi</td>
		<td>real</td>
		<td>None</td>
		<td>maximum final He. conc in IB VV [appm]</td>
	</tr>
	<tr>
		<td>vvhemaxo</td>
		<td>real</td>
		<td>None</td>
		<td>maximum final He. conc in OB VV [appm]</td>
	</tr>
	<tr>
		<td>vvhemini</td>
		<td>real</td>
		<td>None</td>
		<td>minimum final He. conc in IB VV [appm]</td>
	</tr>
	<tr>
		<td>vvhemino</td>
		<td>real</td>
		<td>None</td>
		<td>minimum final He. conc in OB VV [appm]</td>
	</tr>
	<tr>
		<td>x_BM_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BM_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BP_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BP_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BZ_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_BZ_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_VV_IB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>x_VV_OB</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## maths_library
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>double</td>
		<td>integer</td>
		<td>8</td>
		<td></td>
	</tr>
</table>

## numerics
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>active_constraints</td>
		<td>logical</td>
		<td>['.false.', '.false.', '.false...</td>
		<td>active_constraints(ipeqns) : Logical array showing which constraints are active</td>
	</tr>
	<tr>
		<td>bondl</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>bondu</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>boundl</td>
		<td>real</td>
		<td>['9.d-99', '9.d-99', '9.d-99',...</td>
		<td>boundl(ipnvars) /../ : lower bounds used on ixc variables during<br>                         VMCON optimisation runs</td>
	</tr>
	<tr>
		<td>boundu</td>
		<td>real</td>
		<td>['9.d99', '9.d99', '9.d99', '9...</td>
		<td></td>
	</tr>
	<tr>
		<td>epsfcn</td>
		<td>real</td>
		<td>1.0D-3</td>
		<td>epsfcn /1.0e-3/ : finite difference step length for HYBRD/VMCON derivatives</td>
	</tr>
	<tr>
		<td>epsvmc</td>
		<td>real</td>
		<td>1.0D-6</td>
		<td>epsvmc /1.0e-6/ : error tolerance for VMCON</td>
	</tr>
	<tr>
		<td>factor</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>factor /0.1/ : used in HYBRD for first step size</td>
	</tr>
	<tr>
		<td>ftol</td>
		<td>real</td>
		<td>1.0D-4</td>
		<td>ftol /1.0e-4/ : error tolerance for HYBRD</td>
	</tr>
	<tr>
		<td>icc</td>
		<td>integer</td>
		<td>['0', '0', '0', '0', '0', '0',...</td>
		<td></td>
	</tr>
	<tr>
		<td>ioptimz</td>
		<td>integer</td>
		<td>1</td>
		<td>ioptimz /1/ : code operation switch:<br>            = -2 for no optimisation, no VMCOM or HYBRD;<br>            = -1 for no optimisation, HYBRD only;<br>            = 0  for HYBRD and VMCON (not recommended);<br>            = 1  for optimisation, VMCON only<br>minmax /7/ : switch for figure-of-merit (see lablmm for descriptions)<br>               negative =&gt; maximise, positive =&gt; minimise</td>
	</tr>
	<tr>
		<td>ipeqns</td>
		<td>integer</td>
		<td>84</td>
		<td>ipeqns  FIX : number of constraint equations available</td>
	</tr>
	<tr>
		<td>ipnfoms</td>
		<td>integer</td>
		<td>19</td>
		<td>ipnfoms FIX : number of available figures of merit</td>
	</tr>
	<tr>
		<td>ipnvars</td>
		<td>integer</td>
		<td>174</td>
		<td>ipnvars FIX : total number of variables available for iteration</td>
	</tr>
	<tr>
		<td>iptnt</td>
		<td>integer</td>
		<td>(ipeqns*(3*ipeqns+13))/2</td>
		<td></td>
	</tr>
	<tr>
		<td>ipvlam</td>
		<td>integer</td>
		<td>ipeqns+2*ipnvars+1</td>
		<td></td>
	</tr>
	<tr>
		<td>ipvp1</td>
		<td>integer</td>
		<td>ipnvars+1</td>
		<td></td>
	</tr>
	<tr>
		<td>ixc</td>
		<td>integer</td>
		<td>['0', '0', '0', '0', '0', '0',...</td>
		<td></td>
	</tr>
	<tr>
		<td>lablcc</td>
		<td>character</td>
		<td>(/'Beta consistency', 'Global ...</td>
		<td>lablcc(ipeqns) : labels describing constraint equations (corresponding itvs)<br>   ( 1) Beta (consistency equation) (itv 5)<br>   ( 2) Global power balance (consistency equation) (itv 10,1,2,3,4,6,11)<br>   ( 3) Ion power balance DEPRECATED (itv 10,1,2,3,4,6,11)<br>   ( 4) Electron power balance DEPRECATED (itv 10,1,2,3,4,6,11)<br>   ( 5) Density upper limit (itv 9,1,2,3,4,5,6)<br>   ( 6) (Epsilon x beta poloidal) upper limit (itv 8,1,2,3,4,6)<br>   ( 7) Beam ion density (NBI) (consistency equation) (itv 7)<br>   ( 8) Neutron wall load upper limit (itv 14,1,2,3,4,6)<br>   ( 9) Fusion power upper limit (itv 26,1,2,3,4,6)<br>   (10) Toroidal field 1/R (consistency equation) (itv 12,1,2,3,13 )<br>   (11) Radial build (consistency equation) (itv 3,1,13,16,29,42,61)<br>   (12) Volt second lower limit (STEADY STATE) (itv 15,1,2,3)<br>   (13) Burn time lower limit (PULSE) (itv 21,1,16,17,29,42,44,61)<br>   (14) Neutral beam decay lengths to plasma centre (NBI) (consistency equation)<br>   (15) LH power threshold limit (itv 103)<br>   (16) Net electric power lower limit (itv 25,1,2,3)<br>   (17) Radiation fraction upper limit (itv 28)<br>   (18) Divertor heat load upper limit (itv 27)<br>   (19) MVA upper limit (itv 30)<br>   (20) Neutral beam tangency radius upper limit (NBI) (itv 33,31,3,13)<br>   (21) Plasma minor radius lower limit (itv 32)<br>   (22) Divertor collisionality upper limit (itv 34,43)<br>   (23) Conducting shell to plasma minor radius ratio upper limit<br>            (itv 104,1,74)<br>   (24) Beta upper limit (itv 36,1,2,3,4,6,18)<br>   (25) Peak toroidal field upper limit (itv 35,3,13,29)<br>   (26) Central solenoid EOF current density upper limit (ipfres=0)<br>            (itv 38,37,41,12)<br>   (27) Central solenoid BOP current density upper limit (ipfres=0)<br>            (itv 39,37,41,12)<br>   (28) Fusion gain Q lower limit (itv 45,47,40)<br>   (29) Inboard radial build consistency (itv 3,1,13,16,29,42,61)<br>   (30) Injection power upper limit (itv 46,47,11)<br>   (31) TF coil case stress upper limit (SCTF) (itv 48,56,57,58,59,60,24)<br>   (32) TF coil conduit stress upper limit (SCTF) (itv 49,56,57,58,59,60,24)<br>   (33) I_op / I_critical (TF coil) (SCTF) (itv 50,56,57,58,59,60,24)<br>   (34) Dump voltage upper limit (SCTF) (itv 51,52,56,57,58,59,60,24)<br>   (35) J_winding pack/J_protection upper limit (SCTF) (itv 53,56,57,58,59,60,24)<br>   (36) TF coil temperature margin lower limit (SCTF) (itv 54,55,56,57,58,59,60,24)<br>   (37) Current drive gamma upper limit (itv 40,47)<br>   (38) First wall coolant temperature rise upper limit (itv 62)<br>   (39) First wall peak temperature upper limit (itv 63)<br>   (40) Start-up injection power lower limit (PULSE) (itv 64)<br>   (41) Plasma current ramp-up time lower limit (PULSE) (itv  66,65)<br>   (42) Cycle time lower limit (PULSE) (itv 17,67,65)<br>   (43) Average centrepost temperature<br>            (TART) (consistency equation) (itv 13,20,69,70)<br>   (44) Peak centrepost temperature upper limit (TART) (itv 68,69,70)<br>   (45) Edge safety factor lower limit (TART) (itv 71,1,2,3)<br>   (46) Ip/Irod upper limit (TART) (itv 72,2,60)<br>   (47) NOT USED<br>   (48) Poloidal beta upper limit (itv 79,2,3,18)<br>   (49) NOT USED<br>   (50) IFE repetition rate upper limit (IFE)<br>   (51) Startup volt-seconds consistency (PULSE) (itv 16,29,3,1)<br>   (52) Tritium breeding ratio lower limit (itv 89,90,91)<br>   (53) Neutron fluence on TF coil upper limit (itv 92,93,94)<br>   (54) Peak TF coil nuclear heating upper limit (itv 95,93,94)<br>   (55) Vacuum vessel helium concentration upper limit iblanket =2 (itv 96,93,94)<br>   (56) Pseparatrix/Rmajor upper limit (itv 97,1,3,102)<br>   (57) NOT USED<br>   (58) NOT USED<br>   (59) Neutral beam shine-through fraction upper limit (NBI) (itv 105,6,19,4 )<br>   (60) Central solenoid temperature margin lower limit (SCTF) (itv 106)<br>   (61) Minimum availability value (itv 107)<br>   (62) taup/taueff the ratio of particle to energy confinement times (itv 110)<br>   (63) The number of ITER-like vacuum pumps niterpump &lt; tfno (itv 111)<br>   (64) Zeff less than or equal to zeffmax (itv 112)<br>   (65) Dump time set by VV loads (itv 56, 113)<br>   (66) Limit on rate of change of energy in poloidal field<br>            (Use iteration variable 65(tohs), 115)<br>   (67) Simple Radiation Wall load limit (itv 116, 102, 4,6)<br>   (68) Psep * Bt / qAR upper limit (itv 117)<br>   (69) ensure separatrix power = the value from Kallenbach divertor (itv 118)<br>   (70) ensure that teomp = separatrix temperature in the pedestal profile,<br>            (itv 119 (tesep))<br>   (71) ensure that neomp = separatrix density (nesep) x neratio<br>   (72) central solenoid Tresca stress limit (itv 123 foh_stress)<br>   (73) Psep &gt;= Plh + Paux (itv 137 (fplhsep))<br>   (74) TFC quench &lt; tmax_croco (itv 141 (fcqt))<br>   (75) TFC current/copper area &lt; Maximum (itv 143 f_coppera_m2)<br>   (76) Eich critical separatrix density<br>   (77) TF coil current per turn upper limit<br>   (78) Reinke criterion impurity fraction lower limit (itv  147 freinke)<br>   (79) Peak CS field upper limit (itv  149 fbmaxcs)<br>   (80) Divertor power lower limit pdivt (itv  153 fpdivlim)<br>   (81) Ne(0) &gt; ne(ped) constraint (itv  154 fne0)<br>   (82) toroidalgap &gt;  tftort constraint (itv  171 ftoroidalgap)<br>   (83) Radial build consistency for stellarators (itv 172 f_avspace)<br>   (84) Lower limit for beta (itv 173 fbetatry_lower)<br>ixc(ipnvars) /0/ :<br>               array defining which iteration variables to activate<br>               (see lablxc for descriptions)</td>
	</tr>
	<tr>
		<td>lablmm</td>
		<td>character</td>
		<td>(/'major radius.', 'not used.'...</td>
		<td>lablmm(ipnfoms) : labels describing figures of merit:<br>   ( 1) major radius<br>   ( 2) not used<br>   ( 3) neutron wall load<br>   ( 4) P_tf + P_pf<br>   ( 5) fusion gain Q<br>   ( 6) cost of electricity<br>   ( 7) capital cost (direct cost if ireactor=0,<br>                          constructed cost otherwise)<br>   ( 8) aspect ratio<br>   ( 9) divertor heat load<br>   (10) toroidal field<br>   (11) total injected power<br>   (12) hydrogen plant capital cost OBSOLETE<br>   (13) hydrogen production rate OBSOLETE<br>   (14) pulse length<br>   (15) plant availability factor (N.B. requires<br>            iavail=1 to be set)<br>   (16) linear combination of major radius (minimised) and pulse length (maximised)<br>              note: FoM should be minimised only!<br>   (17) net electrical output<br>   (18) Null Figure of Merit<br>   (19) linear combination of big Q and pulse length (maximised)<br>              note: FoM should be minimised only!</td>
	</tr>
	<tr>
		<td>lablxc</td>
		<td>character</td>
		<td>["''", "''", "''", "''", "''",...</td>
		<td></td>
	</tr>
	<tr>
		<td>minmax</td>
		<td>integer</td>
		<td>7</td>
		<td></td>
	</tr>
	<tr>
		<td>name_xc</td>
		<td>character</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ncalls</td>
		<td>integer</td>
		<td>0</td>
		<td>ncalls : number of function calls during solution</td>
	</tr>
	<tr>
		<td>neqns</td>
		<td>integer</td>
		<td>0</td>
		<td>neqns /0/ : number of equality constraints to be satisfied</td>
	</tr>
	<tr>
		<td>nfev1</td>
		<td>integer</td>
		<td>0</td>
		<td>nfev1 : number of calls to FCNHYB (HYBRD function caller) made</td>
	</tr>
	<tr>
		<td>nfev2</td>
		<td>integer</td>
		<td>0</td>
		<td>nfev2 : number of calls to FCNVMC1 (VMCON function caller) made</td>
	</tr>
	<tr>
		<td>nineqns</td>
		<td>integer</td>
		<td>0</td>
		<td>nineqns /0/ : number of inequality constraints VMCON must satisfy<br>                (leave at zero for now)</td>
	</tr>
	<tr>
		<td>nvar</td>
		<td>integer</td>
		<td>16</td>
		<td>nvar /16/ : number of iteration variables to use</td>
	</tr>
	<tr>
		<td>nviter</td>
		<td>integer</td>
		<td>0</td>
		<td>nviter : number of VMCON iterations performed<br>icc(ipeqns) /0/ :<br>           array defining which constraint equations to activate<br>           (see lablcc for descriptions)</td>
	</tr>
	<tr>
		<td>rcm</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>resdl</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>scafc</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>scale</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>sqsumsq</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>sqsumsq : sqrt of the sum of the square of the constraint residuals</td>
	</tr>
	<tr>
		<td>vlam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td></td>
	</tr>
	<tr>
		<td>xcm</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
	<tr>
		<td>xcs</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td></td>
	</tr>
</table>

## pf_power_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>acptmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>average of currents in PF circuits (kA)</td>
	</tr>
	<tr>
		<td>ensxpfm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum stored energy in the PF circuits (MJ)</td>
	</tr>
	<tr>
		<td>iscenr</td>
		<td>integer</td>
		<td>2</td>
		<td>Switch for PF coil energy storage option:<br><ul><li> =1 all power from MGF (motor-generator flywheel) units</li><li> =2 all pulsed power from line</li><li> =3 PF power from MGF, heating from line</li></ul></td>
	</tr>
	<tr>
		<td>maxpoloidalpower</td>
		<td>real</td>
		<td>1000.0D0</td>
		<td>Maximum permitted absolute rate of change of stored energy in poloidal field (MW)</td>
	</tr>
	<tr>
		<td>peakpoloidalpower</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Peak absolute rate of change of stored energy in poloidal field (MW)</td>
	</tr>
	<tr>
		<td>pfckts</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>number of PF coil circuits</td>
	</tr>
	<tr>
		<td>poloidalpower</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>Poloidal power usage at time t (MW)</td>
	</tr>
	<tr>
		<td>spfbusl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total PF coil circuit bus length (m)</td>
	</tr>
	<tr>
		<td>spsmva</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>sum of PF power supply ratings (MVA)</td>
	</tr>
	<tr>
		<td>srcktpm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>sum of resistive PF coil power (kW)</td>
	</tr>
	<tr>
		<td>vpfskv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>PF coil voltage (kV)</td>
	</tr>
</table>

## pfcoil_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>axial_force</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>bpf2</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ccl0</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ccls</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>cfxf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>conductorpf</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>croco_strand</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nef</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nfxf</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rcls</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rfxf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ricpf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>sig_axial</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>sig_hoop</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ssq0</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vsdum</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>xind</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>zcls</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>zfxf</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## pfcoil_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>a_oh_turn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Central solenoid (OH) trun cross-sectional area (m2)</td>
	</tr>
	<tr>
		<td>alfapf</td>
		<td>real</td>
		<td>5.0D-10</td>
		<td>smoothing parameter used in PF coil current calculation at the beginning of pulse (BoP)</td>
	</tr>
	<tr>
		<td>alstroh</td>
		<td>real</td>
		<td>4.0D8</td>
		<td>allowable hoop stress in Central Solenoid structural material (Pa)</td>
	</tr>
	<tr>
		<td>areaoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Central solenoid vertical cross-sectional area (m2)</td>
	</tr>
	<tr>
		<td>awpoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid conductor+void area (m2)</td>
	</tr>
	<tr>
		<td>bmaxcs_lim</td>
		<td>real</td>
		<td>13.0</td>
		<td>Central solenoid max field limit [T]</td>
	</tr>
	<tr>
		<td>bmaxoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum field in central solenoid at end of flat-top (EoF) (T)</td>
	</tr>
	<tr>
		<td>bmaxoh0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum field in central solenoid at beginning of pulse (T)</td>
	</tr>
	<tr>
		<td>bpf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak field at coil i (T)</td>
	</tr>
	<tr>
		<td>cohbop</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Central solenoid overall current density at beginning of pulse (A/m2)</td>
	</tr>
	<tr>
		<td>coheof</td>
		<td>real</td>
		<td>1.85D7</td>
		<td>Central solenoid overall current density at end of flat-top (A/m2) (iteration variable 37)</td>
	</tr>
	<tr>
		<td>cpt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>current per turn in coil i at time j (A)</td>
	</tr>
	<tr>
		<td>cptdin</td>
		<td>real</td>
		<td>4.0D4</td>
		<td>peak current per turn input for PF coil i (A)</td>
	</tr>
	<tr>
		<td>curpfb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>PF coil current work array beginning of pulse</td>
	</tr>
	<tr>
		<td>curpff</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>PF coil current work array flat top</td>
	</tr>
	<tr>
		<td>curpfs</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>PF coil current work array end of pulse</td>
	</tr>
	<tr>
		<td>etapsu</td>
		<td>real</td>
		<td>0.9D0</td>
		<td>Efficiency of transfer of PF stored energy into or out of storage.</td>
	</tr>
	<tr>
		<td>fbmaxcs</td>
		<td>real</td>
		<td>13.0</td>
		<td>F-value for CS mmax field (cons. 79, itvar 149)</td>
	</tr>
	<tr>
		<td>fcohbof</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ratio of central solenoid overall current density at beginning of flat-top / end of flat-top</td>
	</tr>
	<tr>
		<td>fcohbop</td>
		<td>real</td>
		<td>0.9D0</td>
		<td>ratio of central solenoid overall current density at beginning of pulse / end of flat-top<br> (iteration variable 41)</td>
	</tr>
	<tr>
		<td>fcuohsu</td>
		<td>real</td>
		<td>0.7D0</td>
		<td>copper fraction of strand in central solenoid</td>
	</tr>
	<tr>
		<td>fcupfsu</td>
		<td>real</td>
		<td>0.69D0</td>
		<td>copper fraction of cable conductor (PF coils)</td>
	</tr>
	<tr>
		<td>fvssu</td>
		<td>real</td>
		<td>1.0</td>
		<td>F-value for constraint equation 51 </td>
	</tr>
	<tr>
		<td>i_cs_stress</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for CS stress calculation:<br><ul><li> =0 Hoop stress only</li><li> =1 Hoop + Axial stress</li></ul></td>
	</tr>
	<tr>
		<td>ipfloc</td>
		<td>integer</td>
		<td>(/2, 2, 3, 0, 0, 0, 0, 0, 0, 0...</td>
		<td>switch for locating scheme of PF coil group i:<br><ul><li> =1 PF coil on top of central solenoid</li><li> =2 PF coil on top of TF coil</li><li> =3 PF coil outside of TF coil</li></ul></td>
	</tr>
	<tr>
		<td>ipfres</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for PF coil type:<br><ul><li> =0 superconducting PF coils</li><li> =1 resistive PF coils</li></ul></td>
	</tr>
	<tr>
		<td>isumatoh</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for superconductor material in central solenoid:<br><ul><li> =1 ITER Nb3Sn critical surface model with standard<br>   ITER parameters</li><li> =2 Bi-2212 high temperature superconductor (range of<br>   validity T &lt; 20K, adjusted field b &lt; 104 T, B &gt; 6 T)</li><li> =3 NbTi</li><li> =4 ITER Nb3Sn model with user-specified parameters</li><li> =5 WST Nb3Sn parameterisation</li><li> =6 REBCO HTS parameterisation</li></ul></td>
	</tr>
	<tr>
		<td>isumatpf</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for superconductor material in PF coils:<br><ul><li> =1 ITER Nb3Sn critical surface model with standard<br>   ITER parameters</li><li> =2 Bi-2212 high temperature superconductor (range of<br>   validity T &lt; 20K, adjusted field b &lt; 104 T, B &gt; 6 T)</li><li> =3 NbTi</li><li> =4 ITER Nb3Sn model with user-specified parameters</li><li> =5 WST Nb3Sn parameterisation</li></ul></td>
	</tr>
	<tr>
		<td>itr_sum</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total sum of I x turns x radius for all PF coils and CS (Am)</td>
	</tr>
	<tr>
		<td>jscoh_bop</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid superconductor critical current density (A/m2) at beginning-of-pulse</td>
	</tr>
	<tr>
		<td>jscoh_eof</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid superconductor critical current density (A/m2) at end-of-flattop</td>
	</tr>
	<tr>
		<td>jstrandoh_bop</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid strand critical current density (A/m2) at beginning-of-pulse</td>
	</tr>
	<tr>
		<td>jstrandoh_eof</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid strand critical current density (A/m2) at end-of-flattop</td>
	</tr>
	<tr>
		<td>ncirt</td>
		<td>integer</td>
		<td>0</td>
		<td>number of PF circuits (including central solenoid and plasma)</td>
	</tr>
	<tr>
		<td>ncls</td>
		<td>integer</td>
		<td>(/1, 1, 2, 0, 0, 0, 0, 0, 0, 0...</td>
		<td>number of PF coils in group j</td>
	</tr>
	<tr>
		<td>nclsmx</td>
		<td>integer</td>
		<td>2</td>
		<td>maximum number of PF coils in a given group</td>
	</tr>
	<tr>
		<td>nfixmx</td>
		<td>integer</td>
		<td>64</td>
		<td>maximum number of fixed current PF coils</td>
	</tr>
	<tr>
		<td>nfxfh</td>
		<td>integer</td>
		<td>7</td>
		<td>number of filaments the top and bottom of the central solenoid should be broken <br> into during scaling (5 - 10 is good)</td>
	</tr>
	<tr>
		<td>ngc</td>
		<td>integer</td>
		<td>ngrpmx*nclsmx</td>
		<td>maximum total number of coils across all groups</td>
	</tr>
	<tr>
		<td>ngc2</td>
		<td>integer</td>
		<td>ngc+2</td>
		<td>new variable to include 2 additional circuits: plasma and central solenoid</td>
	</tr>
	<tr>
		<td>ngrp</td>
		<td>integer</td>
		<td>3</td>
		<td>number of groups of PF coils. Symmetric coil pairs should all be in the same group</td>
	</tr>
	<tr>
		<td>ngrpmx</td>
		<td>integer</td>
		<td>8</td>
		<td>maximum number of groups of PF coils</td>
	</tr>
	<tr>
		<td>nohc</td>
		<td>integer</td>
		<td>0</td>
		<td>number of PF coils (excluding the central solenoid) + 1</td>
	</tr>
	<tr>
		<td>nptsmx</td>
		<td>integer</td>
		<td>32</td>
		<td>maximum number of points across the midplane of the plasma at which the field from <br> the PF coils is fixed</td>
	</tr>
	<tr>
		<td>oh_steel_frac</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>central solenoid steel fraction (iteration variable 122)</td>
	</tr>
	<tr>
		<td>ohhghf</td>
		<td>real</td>
		<td>0.71D0</td>
		<td>Central solenoid height / TF coil internal height</td>
	</tr>
	<tr>
		<td>pfcaseth</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>steel case thickness for PF coil i (m)</td>
	</tr>
	<tr>
		<td>pfclres</td>
		<td>real</td>
		<td>2.5D-8</td>
		<td>PF coil resistivity (if ipfres=1) (Ohm-m)</td>
	</tr>
	<tr>
		<td>pfmmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of heaviest PF coil (tonnes)</td>
	</tr>
	<tr>
		<td>pfrmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radius of largest PF coil (m)</td>
	</tr>
	<tr>
		<td>pfwpmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Total mean wall plug power dissipated in PFC and CS power supplies (MW) (issue #713)</td>
	</tr>
	<tr>
		<td>powohres</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid resistive power during flattop (W)</td>
	</tr>
	<tr>
		<td>powpfres</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total PF coil resistive losses during flattop (W)</td>
	</tr>
	<tr>
		<td>ra</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inner radius of coil i (m)</td>
	</tr>
	<tr>
		<td>rb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outer radius of coil i (m)</td>
	</tr>
	<tr>
		<td>ric</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak current in coil i (MA-turns)</td>
	</tr>
	<tr>
		<td>rjconpf</td>
		<td>real</td>
		<td>3.0D7</td>
		<td>average winding pack current density of PF coil i (A/m2) at time of peak <br> current in that coil (calculated for ipfloc=1 coils)</td>
	</tr>
	<tr>
		<td>rjohc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>allowable central solenoid current density at end of flat-top (A/m2)</td>
	</tr>
	<tr>
		<td>rjohc0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>allowable central solenoid current density at beginning of pulse (A/m2)</td>
	</tr>
	<tr>
		<td>rjpfalw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>allowable winding pack current density of PF coil i (A/m2)</td>
	</tr>
	<tr>
		<td>rohc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radius to the centre of the central solenoid (m)</td>
	</tr>
	<tr>
		<td>routr</td>
		<td>real</td>
		<td>1.5D0</td>
		<td>radial distance (m) from outboard TF coil leg to centre of ipfloc=3 PF coils</td>
	</tr>
	<tr>
		<td>rpf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radius of PF coil i (m)</td>
	</tr>
	<tr>
		<td>rpf1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>offset (m) of radial position of ipfloc=1 PF coils from being directly above<br> the central solenoid</td>
	</tr>
	<tr>
		<td>rpf2</td>
		<td>real</td>
		<td>-1.63D0</td>
		<td>offset (m) of radial position of ipfloc=2 PF coils from being at <br> rmajor (offset = rpf2triangrminor)</td>
	</tr>
	<tr>
		<td>s_tresca_oh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Tresca stress coils/central solenoid [MPa]</td>
	</tr>
	<tr>
		<td>sigpfcalw</td>
		<td>real</td>
		<td>500.0D0</td>
		<td>maximum permissible tensile stress (MPa) in steel coil cases for superconducting <br> PF coils (ipfres=0)</td>
	</tr>
	<tr>
		<td>sigpfcf</td>
		<td>real</td>
		<td>0.666D0</td>
		<td>fraction of JxB hoop force supported by steel case for superconducting PF coils (ipfres=0)</td>
	</tr>
	<tr>
		<td>sxlg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mutual inductance matrix (H)</td>
	</tr>
	<tr>
		<td>tmargoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Central solenoid temperature margin (K)</td>
	</tr>
	<tr>
		<td>turns</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>number of turns in PF coil i</td>
	</tr>
	<tr>
		<td>vf</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>winding pack void fraction of PF coil i for coolant</td>
	</tr>
	<tr>
		<td>vfohc</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>void fraction of central solenoid conductor for coolant</td>
	</tr>
	<tr>
		<td>vsbn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total flux swing available for burn (Wb)</td>
	</tr>
	<tr>
		<td>vsefbn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>flux swing from PF coils for burn (Wb)</td>
	</tr>
	<tr>
		<td>vsefsu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>flux swing from PF coils for startup (Wb)</td>
	</tr>
	<tr>
		<td>vseft</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total flux swing from PF coils (Wb)</td>
	</tr>
	<tr>
		<td>vsoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total flux swing from the central solenoid (Wb)</td>
	</tr>
	<tr>
		<td>vsohbn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid flux swing for burn (Wb)</td>
	</tr>
	<tr>
		<td>vsohsu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central solenoid flux swing for startup (Wb)</td>
	</tr>
	<tr>
		<td>vssu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total flux swing for startup (constraint eqn 51 to enforce vssu=vsres+vsind) (Wb)</td>
	</tr>
	<tr>
		<td>vstot</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total flux swing for pulse (Wb)</td>
	</tr>
	<tr>
		<td>waves</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>used in current waveform of PF coils/central solenoid</td>
	</tr>
	<tr>
		<td>whtpf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total mass of the PF coil conductor (kg)</td>
	</tr>
	<tr>
		<td>whtpfs</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total mass of the PF coil structure (kg)</td>
	</tr>
	<tr>
		<td>wtc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>conductor mass for PF coil i (kg)</td>
	</tr>
	<tr>
		<td>wts</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>structure mass for PF coil i (kg)</td>
	</tr>
	<tr>
		<td>zh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>upper point of PF coil i (m)</td>
	</tr>
	<tr>
		<td>zl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>lower point of PF coil i (m)</td>
	</tr>
	<tr>
		<td>zpf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>z (height) location of PF coil i (m)</td>
	</tr>
	<tr>
		<td>zref</td>
		<td>real</td>
		<td>(/3.6D0, 1.2D0, 2.5D0, 1.0D0, ...</td>
		<td>PF coil vertical positioning adjuster:<br><ul><li> for groups j with ipfloc(j) = 1; zref(j) is ignored</li><li> for groups j with ipfloc(j) = 2 AND itart=1 (only);<br>   zref(j) is distance of centre of PF coil from inside<br>   edge of TF coil (remember that PF coils for STs lie<br>   within the TF coil)</li><li> for groups j with ipfloc(j) = 3; zref(j) = ratio of<br>   height of coil group j to plasma minor radius</li></ul></td>
	</tr>
</table>

## physics_functions_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>vcritx</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## physics_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>beta_mcdonald</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>drsep</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>err242</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>err243</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fLI</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fLO</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fUI</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fUO</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>fio</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>iscz</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>itart_r</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lambdaio</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>nu_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pLImw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pLOmw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pUImw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pUOmw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ptarmw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rad_fraction_core</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rho_star</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>total_energy_conf_time</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>total_loss_power</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>total_plasma_internal_energy</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## physics_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>abeam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>beam ion mass (amu)</td>
	</tr>
	<tr>
		<td>afuel</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>average mass of fuel portion of ions (amu)</td>
	</tr>
	<tr>
		<td>aion</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>average mass of all ions (amu)</td>
	</tr>
	<tr>
		<td>alpha_crit</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>critical ballooning parameter value</td>
	</tr>
	<tr>
		<td>alphaj</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>current profile index (calculated from q_0, q if iprofile=1)</td>
	</tr>
	<tr>
		<td>alphan</td>
		<td>real</td>
		<td>0.25D0</td>
		<td>density profile index</td>
	</tr>
	<tr>
		<td>alphap</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>pressure profile index</td>
	</tr>
	<tr>
		<td>alpharate</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha particle production rate (particles/m3/sec)</td>
	</tr>
	<tr>
		<td>alphat</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>temperature profile index</td>
	</tr>
	<tr>
		<td>aspect</td>
		<td>real</td>
		<td>2.907D0</td>
		<td>aspect ratio (iteration variable 1)</td>
	</tr>
	<tr>
		<td>beamfus0</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>multiplier for beam-background fusion calculation</td>
	</tr>
	<tr>
		<td>beta</td>
		<td>real</td>
		<td>0.042D0</td>
		<td>total plasma beta (iteration variable 5) (calculated if ipedestal=3 or stellarator)</td>
	</tr>
	<tr>
		<td>betaft</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fast alpha beta component</td>
	</tr>
	<tr>
		<td>betalim</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>allowable beta</td>
	</tr>
	<tr>
		<td>betalim_lower</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>allowable lower beta</td>
	</tr>
	<tr>
		<td>betanb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutral beam beta component</td>
	</tr>
	<tr>
		<td>betap</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>poloidal beta</td>
	</tr>
	<tr>
		<td>betbm0</td>
		<td>real</td>
		<td>1.5D0</td>
		<td>leading coefficient for NB beta fraction</td>
	</tr>
	<tr>
		<td>bp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>poloidal field (T)</td>
	</tr>
	<tr>
		<td>bt</td>
		<td>real</td>
		<td>5.68D0</td>
		<td>toroidal field on axis (T) (iteration variable 2)</td>
	</tr>
	<tr>
		<td>btot</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total toroidal + poloidal field (T)</td>
	</tr>
	<tr>
		<td>burnup</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fractional plasma burnup</td>
	</tr>
	<tr>
		<td>bvert</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vertical field at plasma (T)</td>
	</tr>
	<tr>
		<td>csawth</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>coeff. for sawteeth effects on burn V-s requirement</td>
	</tr>
	<tr>
		<td>cvol</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>multiplying factor times plasma volume (normally=1)</td>
	</tr>
	<tr>
		<td>cwrmax</td>
		<td>real</td>
		<td>1.35D0</td>
		<td>maximum ratio of conducting wall distance to plasma minor radius for <br> vertical stability (constraint equation 23)</td>
	</tr>
	<tr>
		<td>dene</td>
		<td>real</td>
		<td>9.8D19</td>
		<td>electron density (/m3) (iteration variable 6) (calculated if ipedestal=3)</td>
	</tr>
	<tr>
		<td>deni</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fuel ion density (/m3)</td>
	</tr>
	<tr>
		<td>dlamee</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>electron-electron coulomb logarithm</td>
	</tr>
	<tr>
		<td>dlamie</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ion-electron coulomb logarithm</td>
	</tr>
	<tr>
		<td>dlimit</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>density limit (/m3) as calculated using various models</td>
	</tr>
	<tr>
		<td>dnalp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>thermal alpha density (/m3)</td>
	</tr>
	<tr>
		<td>dnbeam</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>hot beam ion density, variable (/m3)</td>
	</tr>
	<tr>
		<td>dnbeam2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>hot beam ion density from calculation (/m3)</td>
	</tr>
	<tr>
		<td>dnbeta</td>
		<td>real</td>
		<td>3.5D0</td>
		<td>Troyon-like coefficient for beta scaling calculated <br> as 4*rli if iprofile=1 (see also gtscale option)</td>
	</tr>
	<tr>
		<td>dnelimt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>density limit (/m3)</td>
	</tr>
	<tr>
		<td>dnitot</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total ion density (/m3)</td>
	</tr>
	<tr>
		<td>dnla</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>line averaged electron density (/m3)</td>
	</tr>
	<tr>
		<td>dnprot</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>proton ash density (/m3)</td>
	</tr>
	<tr>
		<td>dntau</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma average "n-tau" (seconds/m3)</td>
	</tr>
	<tr>
		<td>dnz</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>high Z ion density (/m3)</td>
	</tr>
	<tr>
		<td>ealphadt</td>
		<td>real</td>
		<td>3520.0D0</td>
		<td>alpha birth energy in D-T reaction (keV)</td>
	</tr>
	<tr>
		<td>epbetmax</td>
		<td>real</td>
		<td>1.38D0</td>
		<td>maximum (eps*beta_poloidal) (constraint equation 6). Note: revised issue #346<br> "Operation at the tokamak equilibrium poloidal beta-limit in TFTR", 1992 Nucl. Fusion 32 1468</td>
	</tr>
	<tr>
		<td>eped_sf</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Adjustment factor for EPED scaling to reduce pedestal temperature or pressure <br> to mitigate or prevent ELMs</td>
	</tr>
	<tr>
		<td>eps</td>
		<td>real</td>
		<td>0.34399724802D0</td>
		<td>inverse aspect ratio</td>
	</tr>
	<tr>
		<td>faccd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fraction of plasma current produced by auxiliary current drive</td>
	</tr>
	<tr>
		<td>facoh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fraction of plasma current produced inductively</td>
	</tr>
	<tr>
		<td>falpe</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fraction of alpha energy to electrons</td>
	</tr>
	<tr>
		<td>falpha</td>
		<td>real</td>
		<td>0.95D0</td>
		<td>fraction of alpha power deposited in plasma (Physics of Energetic Ions, p.2489)</td>
	</tr>
	<tr>
		<td>falpi</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fraction of alpha power to ions</td>
	</tr>
	<tr>
		<td>fdeut</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>deuterium fuel fraction</td>
	</tr>
	<tr>
		<td>ffwal</td>
		<td>real</td>
		<td>0.92D0</td>
		<td>factor to convert plasma surface area to first wall area in neutron wall <br> load calculation (iwalld=1)</td>
	</tr>
	<tr>
		<td>fgwped</td>
		<td>real</td>
		<td>0.85D0</td>
		<td>fraction of Greenwald density to set as pedestal-top density. If &lt;0, pedestal-top <br> density set manually using neped (ipedestal&gt;=1). Needs to be &gt;0 if ipedestal = 3.<br> (iteration variable 145)</td>
	</tr>
	<tr>
		<td>fgwsep</td>
		<td>real</td>
		<td>0.50D0</td>
		<td>fraction of Greenwald density to set as separatrix density. If &lt;0, separatrix <br> density set manually using nesep (ipedestal&gt;=1). Needs to be &gt;0 if ipedestal = 3.<br> (iteration variable 152)</td>
	</tr>
	<tr>
		<td>fhe3</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>helium-3 fuel fraction</td>
	</tr>
	<tr>
		<td>figmer</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>physics figure of merit (= plascuraspect*sbar, where sbar=1)</td>
	</tr>
	<tr>
		<td>fkzohm</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Zohm elongation scaling adjustment factor (ishape=2, 3)</td>
	</tr>
	<tr>
		<td>fne0</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>f-value for the constraint ne(0) &gt; ne(sep) (constraint equation 81)<br> (Iteration variable 154) </td>
	</tr>
	<tr>
		<td>fpdivlim</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>F-value for minimum pdivt (constraint equation 80)</td>
	</tr>
	<tr>
		<td>fplhsep</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>F-value for Psep &gt;= Plh + Paux (constraint equation 73)</td>
	</tr>
	<tr>
		<td>ftar</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>fraction of power to the lower divertor in double null configuration <br> (i_single_null = 0 only) (default assumes SN)</td>
	</tr>
	<tr>
		<td>ftrit</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>tritium fuel fraction</td>
	</tr>
	<tr>
		<td>fusionrate</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fusion reaction rate (reactions/m3/sec)</td>
	</tr>
	<tr>
		<td>fvsbrnni</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>fraction of the plasma current produced by non-inductive means (iteration variable 44)</td>
	</tr>
	<tr>
		<td>gamma</td>
		<td>real</td>
		<td>0.4D0</td>
		<td>Ejima coefficient for resistive startup V-s formula</td>
	</tr>
	<tr>
		<td>gammaft</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ratio of (fast alpha + neutral beam beta) to thermal beta</td>
	</tr>
	<tr>
		<td>gtscale</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for a/R scaling of dnbeta (iprofile=0 only):<br><ul><li> =0 do not scale dnbeta with eps</li><li> =1 scale dnbeta with eps</li></ul></td>
	</tr>
	<tr>
		<td>hfac</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>H factors for an ignited plasma for each energy confinement time scaling law</td>
	</tr>
	<tr>
		<td>hfact</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>H factor on energy confinement times, radiation corrected (iteration variable 10). If <br> ipedestal=2,3 and hfact=0, not used in PLASMOD (see also plasmod_i_modeltype) issue #219</td>
	</tr>
	<tr>
		<td>i_single_null</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for single null / double null plasma:<br><ul><li> =0 for double null</li><li> =1 for single null (diverted side down)</li></ul></td>
	</tr>
	<tr>
		<td>ibss</td>
		<td>integer</td>
		<td>3</td>
		<td>switch for bootstrap current scaling<br><ul><li> =1 ITER 1989 bootstrap scaling (high R/a only)</li><li> =2 for Nevins et al general scaling</li><li> =3 for Wilson et al numerical scaling</li><li> =4 for Sauter et al scaling</li></ul></td>
	</tr>
	<tr>
		<td>iculbl</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for beta limit scaling (constraint equation 24)<br><ul><li> =0 apply limit to total beta</li><li> =1 apply limit to thermal beta</li><li> =2 apply limit to thermal + neutral beam beta</li></ul></td>
	</tr>
	<tr>
		<td>icurr</td>
		<td>integer</td>
		<td>4</td>
		<td>switch for plasma current scaling to use<br><ul><li> =1 Peng analytic fit</li><li> =2 Peng double null divertor scaling (ST)</li><li> =3 simple ITER scaling (k = 2.2, d = 0.6)</li><li> =4 later ITER scaling, a la Uckan</li><li> =5 Todd empirical scaling I</li><li> =6 Todd empirical scaling II</li><li> =7 Connor-Hastie model</li><li> =8 Sauter scaling allowing negative triangularity</li><li> =9 FIESTA ST fit</li></ul></td>
	</tr>
	<tr>
		<td>idensl</td>
		<td>integer</td>
		<td>7</td>
		<td>switch for density limit to enforce (constraint equation 5)<br><ul><li> =1 old ASDEX</li><li> =2 Borrass model for ITER (I)</li><li> =3 Borrass model for ITER (II)</li><li> =4 JET edge radiation</li><li> =5 JET simplified</li><li> =6 Hugill-Murakami Mq limit</li><li> =7 Greenwald limit</li></ul></td>
	</tr>
	<tr>
		<td>idia</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for diamagnetic current scaling<br><ul><li> =0 Do not calculate</li><li> =1 Use original TART scaling</li><li> =2 Use SCENE scaling</li></ul></td>
	</tr>
	<tr>
		<td>idivrt</td>
		<td>integer</td>
		<td>2</td>
		<td>number of divertors (calculated from i_single_null)</td>
	</tr>
	<tr>
		<td>ieped</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for scaling pedestal-top temperature with plasma parameters (issue #730):<br><ul><li> =0 set pedestal-top temperature manually using teped</li><li> =1 set pedestal-top temperature using EPED scaling (PLASMOD implementation <br>   of scaling within PLASMOD, `ipedestal =2,3 (ttps://idm.euro-fusion.org/?uid=2MSZ4T)</li></ul></td>
	</tr>
	<tr>
		<td>ifalphap</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for fast alpha pressure calculation<br><ul><li> =0 ITER physics rules (Uckan) fit</li><li> =1 Modified fit (D. Ward) - better at high temperature</li></ul></td>
	</tr>
	<tr>
		<td>ifispact</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for neutronics calculations:<br><ul><li> =0 neutronics calculations turned off</li><li> =1 neutronics calculations turned on</li></ul></td>
	</tr>
	<tr>
		<td>igeom</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for plasma geometry calculation:<br><ul><li> =0 original method (possibly based on Peng ST modelling)</li><li> =1 improved (and traceable) method</li></ul></td>
	</tr>
	<tr>
		<td>ignite</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for ignition assumption. Obviously, ignite must be zero if current drive <br> is required. If ignite is 1, any auxiliary power is assumed to be used only during <br> plasma start-up, and is excluded from all steady-state power balance calculations.<br><ul><li> =0 do not assume plasma ignition</li><li> =1 assume ignited (but include auxiliary power in costs)&lt;/UL</li></ul></td>
	</tr>
	<tr>
		<td>iinvqd</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for inverse quadrature in L-mode scaling laws 5 and 9:<br><ul><li> =0 inverse quadrature not used</li><li> =1 inverse quadrature with Neo-Alcator tau-E used</li></ul></td>
	</tr>
	<tr>
		<td>ilhthresh</td>
		<td>integer</td>
		<td>19</td>
		<td>switch for L-H mode power threshold scaling to use (see pthrmw for list)</td>
	</tr>
	<tr>
		<td>ipedestal</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for pedestal profiles:<br><ul><li> =0 use original parabolic profiles</li><li> =1 use pedestal profile</li><li> =2 use pedestal profiles and run PLASMOD on final outpu</li><li> =3 use PLASMOD transport model only to calculate pedestal profiles</li></ul></td>
	</tr>
	<tr>
		<td>ipnlaws</td>
		<td>integer</td>
		<td>48</td>
		<td>number of energy confinement time scaling laws</td>
	</tr>
	<tr>
		<td>iprofile</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for current profile consistency:<br><ul><li> =0 use input values for alphaj, rli, dnbeta (but see gtscale option)</li><li> =1 make these consistent with input q, q_0 values (recommend icurr=4 with this option)</li></ul></td>
	</tr>
	<tr>
		<td>ips</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for Pfirsch-Schlüter current scaling (issue #413):<br><ul><li> =0 Do not calculate</li><li> =1 Use SCENE scaling</li></ul></td>
	</tr>
	<tr>
		<td>iradloss</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for radiation loss term usage in power balance (see User Guide):<br><ul><li> =0 total power lost is scaling power plus radiation (needed for ipedestal=2,3)</li><li> =1 total power lost is scaling power plus core radiation only</li><li> =2 total power lost is scaling power only, with no additional<br>   allowance for radiation. This is not recommended for power plant models.</li></ul></td>
	</tr>
	<tr>
		<td>isc</td>
		<td>integer</td>
		<td>34</td>
		<td>switch for energy confinement time scaling law (see description in tauscl)</td>
	</tr>
	<tr>
		<td>iscrp</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for plasma-first wall clearances:<br><ul><li> =0 use 10% of rminor</li><li> =1 use input (scrapli and scraplo)</li></ul></td>
	</tr>
	<tr>
		<td>ishape</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for plasma cross-sectional shape calculation:<br><ul><li> =0 use input kappa, triang to calculate 95% values</li><li> =1 scale qlim, kappa, triang with aspect ratio (ST)</li><li> =2 set kappa to the natural elongation value (Zohm ITER scaling), triang input</li><li> =3 set kappa to the natural elongation value (Zohm ITER scaling), triang95 input</li><li> =4 use input kappa95, triang95 to calculate separatrix values</li></ul></td>
	</tr>
	<tr>
		<td>itart</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for spherical tokamak (ST) models:<br><ul><li> =0 use conventional aspect ratio models</li><li> =1 use spherical tokamak models</li></ul></td>
	</tr>
	<tr>
		<td>itartpf</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for Spherical Tokamak PF models:<br><ul><li> =0 use Peng and Strickler (1986) model</li><li> =1 use conventional aspect ratio model</li></ul></td>
	</tr>
	<tr>
		<td>iwalld</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for neutron wall load calculation:<br><ul><li> =1 use scaled plasma surface area</li><li> =2 use first wall area directly</li></ul></td>
	</tr>
	<tr>
		<td>kappa</td>
		<td>real</td>
		<td>1.792D0</td>
		<td>plasma separatrix elongation (calculated if ishape &gt; 0)</td>
	</tr>
	<tr>
		<td>kappa95</td>
		<td>real</td>
		<td>1.6D0</td>
		<td>plasma elongation at 95% surface (calculated if ishape &lt; 4)</td>
	</tr>
	<tr>
		<td>kappaa</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma elongation calculated as xarea/(pi.a^2)</td>
	</tr>
	<tr>
		<td>kappaa_IPB</td>
		<td>real</td>
		<td>0.d0</td>
		<td>Volume measure of plasma elongation</td>
	</tr>
	<tr>
		<td>ne0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central electron density (/m3)</td>
	</tr>
	<tr>
		<td>neped</td>
		<td>real</td>
		<td>4.0D19</td>
		<td>electron density of pedestal [m-3] (ipedestal=1,2, calculated if 3)</td>
	</tr>
	<tr>
		<td>nesep</td>
		<td>real</td>
		<td>3.0D19</td>
		<td>electron density at separatrix [m-3] (ipedestal=1,2, calculated if 3)</td>
	</tr>
	<tr>
		<td>nesep_crit</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>critical electron density at separatrix [m-3]</td>
	</tr>
	<tr>
		<td>ni0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central ion density (/m3)</td>
	</tr>
	<tr>
		<td>normalised_total_beta</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>normaised total beta</td>
	</tr>
	<tr>
		<td>p0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central total plasma pressure (Pa)</td>
	</tr>
	<tr>
		<td>palpepv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha power per volume to electrons (MW/m3)</td>
	</tr>
	<tr>
		<td>palpfwmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha power escaping plasma and reaching first wall (MW)</td>
	</tr>
	<tr>
		<td>palpipv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha power per volume to ions (MW/m3)</td>
	</tr>
	<tr>
		<td>palpmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha power (MW)</td>
	</tr>
	<tr>
		<td>palpnb</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha power from hot neutral beam ions (MW)</td>
	</tr>
	<tr>
		<td>palppv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pbrempv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>bremsstrahlung power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pchargemw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>non-alpha charged particle fusion power (MW)</td>
	</tr>
	<tr>
		<td>pchargepv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>non-alpha charged particle fusion power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pcoef</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>profile factor (= n-weighted T / average T)</td>
	</tr>
	<tr>
		<td>pcoreradmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total core radiation power (MW)</td>
	</tr>
	<tr>
		<td>pcoreradpv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total core radiation power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pdd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>deuterium-deuterium fusion power (MW)</td>
	</tr>
	<tr>
		<td>pdhe3</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>deuterium-helium3 fusion power (MW)</td>
	</tr>
	<tr>
		<td>pdivl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power conducted to the lower divertor region (calculated if i_single_null = 0) (MW)</td>
	</tr>
	<tr>
		<td>pdivmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power conducted to the divertor with most load (calculated if i_single_null = 0) (MW)</td>
	</tr>
	<tr>
		<td>pdivt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power to conducted to the divertor region (MW)</td>
	</tr>
	<tr>
		<td>pdivu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>power conducted to the upper divertor region (calculated if i_single_null = 0) (MW)</td>
	</tr>
	<tr>
		<td>pdt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>deuterium-tritium fusion power (MW)</td>
	</tr>
	<tr>
		<td>pedgeradmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>edge radiation power (MW)</td>
	</tr>
	<tr>
		<td>pedgeradpv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>edge radiation power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pfuscmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>charged particle fusion power (MW)</td>
	</tr>
	<tr>
		<td>phiint</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>internal plasma V-s</td>
	</tr>
	<tr>
		<td>photon_wall</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Nominal mean radiation load on inside surface of reactor (MW/m2)</td>
	</tr>
	<tr>
		<td>piepv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ion/electron equilibration power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>plascur</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma current (A)</td>
	</tr>
	<tr>
		<td>plasma_res_factor</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>plasma resistivity pre-factor</td>
	</tr>
	<tr>
		<td>plhthresh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>L-H mode power threshold (MW) (chosen via ilhthresh, and enforced if <br> constraint equation 15 is on)</td>
	</tr>
	<tr>
		<td>plinepv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>line radiation power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pneutmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutron fusion power (MW)</td>
	</tr>
	<tr>
		<td>pneutpv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neutron fusion power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pohmmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ohmic heating power (MW)</td>
	</tr>
	<tr>
		<td>pohmpv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ohmic heating power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>powerht</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>heating power (= transport loss power) (MW) used in confinement time calculation</td>
	</tr>
	<tr>
		<td>powfmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fusion power (MW)</td>
	</tr>
	<tr>
		<td>pperim</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma poloidal perimeter (m)</td>
	</tr>
	<tr>
		<td>pradmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total radiation power (MW)</td>
	</tr>
	<tr>
		<td>pradpv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total radiation power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>protium</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Seeded protium density / electron density.</td>
	</tr>
	<tr>
		<td>protonrate</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>proton production rate (particles/m3/sec)</td>
	</tr>
	<tr>
		<td>pscalingmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Total transport power from scaling law (MW)</td>
	</tr>
	<tr>
		<td>psolradmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>SOL radiation power (MW) (stellarator only)</td>
	</tr>
	<tr>
		<td>psyncpv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>synchrotron radiation power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>pthrmw</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>L-H power threshold for various scalings (MW)<br><ul><li> =1 ITER 1996 scaling: nominal</li><li> =2 ITER 1996 scaling: upper bound</li><li> =3 ITER 1996 scaling: lower bound</li><li> =4 ITER 1997 scaling: excluding elongation</li><li> =5 ITER 1997 scaling: including elongation</li><li> =6 Martin 2008 scaling: nominal</li><li> =7 Martin 2008 scaling: 95% upper bound</li><li> =8 Martin 2008 scaling: 95% lower bound</li><li> =9 Snipes 2000 scaling: nominal</li><li> =10 Snipes 2000 scaling: upper bound</li><li> =11 Snipes 2000 scaling: lower bound</li><li> =12 Snipes 2000 scaling (closed divertor): nominal</li><li> =13 Snipes 2000 scaling (closed divertor): upper bound</li><li> =14 Snipes 2000 scaling (closed divertor): lower bound</li><li> =15 Hubbard et al. 2012 L-I threshold scaling: nominal</li><li> =16 Hubbard et al. 2012 L-I threshold scaling: lower bound</li><li> =17 Hubbard et al. 2012 L-I threshold scaling: upper bound</li><li> =18 Hubbard et al. 2017 L-I threshold scaling</li><li> =19 Martin 2008 aspect ratio corrected scaling: nominal</li><li> =20 Martin 2008 aspect ratio corrected scaling: 95% upper bound</li><li> =21 Martin 2008 aspect ratio corrected scaling: 95% lower bound</li></ul></td>
	</tr>
	<tr>
		<td>ptremw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>electron transport power (MW)</td>
	</tr>
	<tr>
		<td>ptrepv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>electron transport power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>ptrimw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ion transport power (MW)</td>
	</tr>
	<tr>
		<td>ptripv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ion transport power per volume (MW/m3)</td>
	</tr>
	<tr>
		<td>q</td>
		<td>real</td>
		<td>3.0D0</td>
		<td>safety factor 'near' plasma edge (iteration variable 18) equal to q95 <br> (unless icurr=2 (ST current scaling), in which case q = mean edge safety factor qbar)</td>
	</tr>
	<tr>
		<td>q0</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>safety factor on axis</td>
	</tr>
	<tr>
		<td>q95</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>safety factor at 95% surface</td>
	</tr>
	<tr>
		<td>qfuel</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma fuelling rate (nucleus-pairs/s)</td>
	</tr>
	<tr>
		<td>qlim</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>lower limit for edge safety factor</td>
	</tr>
	<tr>
		<td>qstar</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>cylindrical safety factor</td>
	</tr>
	<tr>
		<td>rad_fraction</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Radiation fraction = total radiation / total power deposited in plasma</td>
	</tr>
	<tr>
		<td>rad_fraction_sol</td>
		<td>real</td>
		<td>0.8D0</td>
		<td>SoL radiation fraction </td>
	</tr>
	<tr>
		<td>ralpne</td>
		<td>real</td>
		<td>0.10D0</td>
		<td>thermal alpha density/electron density (iteration variable 109) (calculated if ipedestal=3)</td>
	</tr>
	<tr>
		<td>res_time</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma current resistive diffusion time (s)</td>
	</tr>
	<tr>
		<td>rhopedn</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>r/a of density pedestal (ipedestal&gt;=1)</td>
	</tr>
	<tr>
		<td>rhopedt</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>r/a of temperature pedestal (ipedestal&gt;=1)</td>
	</tr>
	<tr>
		<td>rli</td>
		<td>real</td>
		<td>0.9D0</td>
		<td>plasma normalised internal inductance (calculated from alphaj if iprofile=1)</td>
	</tr>
	<tr>
		<td>rlp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma inductance (H)</td>
	</tr>
	<tr>
		<td>rmajor</td>
		<td>real</td>
		<td>8.14D0</td>
		<td>plasma major radius (m) (iteration variable 3)</td>
	</tr>
	<tr>
		<td>rminor</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma minor radius (m)</td>
	</tr>
	<tr>
		<td>rnbeam</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>hot beam density / n_e (iteration variable 7)</td>
	</tr>
	<tr>
		<td>rncne</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>n_carbon / n_e</td>
	</tr>
	<tr>
		<td>rndfuel</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>fuel burnup rate (reactions/second)</td>
	</tr>
	<tr>
		<td>rnfene</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>n_highZ / n_e</td>
	</tr>
	<tr>
		<td>rnone</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>n_oxygen / n_e</td>
	</tr>
	<tr>
		<td>rpfac</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>neo-classical correction factor to rplas</td>
	</tr>
	<tr>
		<td>rplas</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma resistance (ohm)</td>
	</tr>
	<tr>
		<td>sarea</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma surface area</td>
	</tr>
	<tr>
		<td>sareao</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard plasma surface area</td>
	</tr>
	<tr>
		<td>sf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>shape factor = plasma poloidal perimeter / (2.pi.rminor)</td>
	</tr>
	<tr>
		<td>ssync</td>
		<td>real</td>
		<td>0.6D0</td>
		<td>synchrotron wall reflectivity factor</td>
	</tr>
	<tr>
		<td>tauee</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>electron energy confinement time (sec)</td>
	</tr>
	<tr>
		<td>tauee_in</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Input electron energy confinement time (sec) (isc=48 only)</td>
	</tr>
	<tr>
		<td>taueff</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>global thermal energy confinement time (sec)</td>
	</tr>
	<tr>
		<td>tauei</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>ion energy confinement time (sec)</td>
	</tr>
	<tr>
		<td>taumax</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>Maximum allowed energy confinement time (s)</td>
	</tr>
	<tr>
		<td>taup</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>alpha particle confinement time (sec)</td>
	</tr>
	<tr>
		<td>tauratio</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>tauratio /1.0/ : ratio of He and pellet particle confinement times</td>
	</tr>
	<tr>
		<td>tauscl</td>
		<td>character</td>
		<td>(/'Neo-Alcator(ohmic)', 'Mirno...</td>
		<td>tauscl(ipnlaws) : labels describing energy confinement scaling laws:<br>  ( 1)  Neo-Alcator (ohmic)<br>  ( 2)  Mirnov (H-mode)<br>  ( 3)  Merezkhin-Muhkovatov (L-mode)<br>  ( 4)  Shimomura (H-mode)<br>  ( 5)  Kaye-Goldston (L-mode)<br>  ( 6)  ITER 89-P (L-mode)<br>  ( 7)  ITER 89-O (L-mode)<br>  ( 8)  Rebut-Lallia (L-mode)<br>  ( 9)  Goldston (L-mode)<br>  (10)  T10 (L-mode)<br>  (11)  JAERI-88 (L-mode)<br>  (12)  Kaye-Big Complex (L-mode)<br>  (13)  ITER H90-P (H-mode)<br>  (14)  ITER Mix (L-mode)<br>  (15)  Riedel (L-mode)<br>  (16)  Christiansen (L-mode)<br>  (17)  Lackner-Gottardi (L-mode)<br>  (18)  Neo-Kaye (L-mode)<br>  (19)  Riedel (H-mode)<br>  (20)  ITER H90-P amended (H-mode)<br>  (21)  LHD (stellarator)<br>  (22)  Gyro-reduced Bohm (stellarator)<br>  (23)  Lackner-Gottardi (stellarator)<br>  (24)  ITER-93H (H-mode)<br>  (25) OBSOLETE<br>  (26)  ITER H-97P ELM-free (H-mode)<br>  (27)  ITER H-97P ELMy (H-mode)<br>  (28)  ITER-96P (=ITER-97L) (L-mode)<br>  (29)  Valovic modified ELMy (H-mode)<br>  (30)  Kaye PPPL April 98 (L-mode)<br>  (31)  ITERH-PB98P(y) (H-mode)<br>  (32)  IPB98(y) (H-mode)<br>  (33)  IPB98(y,1) (H-mode)<br>  (34)  IPB98(y,2) (H-mode)<br>  (35)  IPB98(y,3) (H-mode)<br>  (36)  IPB98(y,4) (H-mode)<br>  (37)  ISS95 (stellarator)<br>  (38)  ISS04 (stellarator)<br>  (39)  DS03 (H-mode)<br>  (40)  Murari et al non-power law (H-mode)<br>  (41)  Petty 2008 (H-mode)<br>  (42)  Lang et al. 2012 (H-mode)<br>  (43)  Hubbard 2017 (I-mode) - nominal<br>  (44)  Hubbard 2017 (I-mode) - lower bound<br>  (45)  Hubbard 2017 (I-mode) - upper bound<br>  (46)  NSTX (H-mode; Spherical tokamak)<br>  (47)  NSTX-Petty08 Hybrid (H-mode)<br>  (48)  Use input tauee_in </td>
	</tr>
	<tr>
		<td>tbeta</td>
		<td>real</td>
		<td>2.0D0</td>
		<td>temperature profile index beta  (ipedestal=1,2)</td>
	</tr>
	<tr>
		<td>te</td>
		<td>real</td>
		<td>12.9D0</td>
		<td>volume averaged electron temperature (keV) (iteration variable 4)<br> (calculated if ipedestal=3)</td>
	</tr>
	<tr>
		<td>te0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central electron temperature (keV)</td>
	</tr>
	<tr>
		<td>ten</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>density weighted average electron temperature (keV)</td>
	</tr>
	<tr>
		<td>teped</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>electron temperature of pedestal (keV) (ipedestal&gt;=1, ieped=0, calculated for ieped=1)</td>
	</tr>
	<tr>
		<td>tesep</td>
		<td>real</td>
		<td>0.1D0</td>
		<td>electron temperature at separatrix (keV) (ipedestal&gt;=1) calculated if reinke <br> criterion is used (icc=78)</td>
	</tr>
	<tr>
		<td>ti</td>
		<td>real</td>
		<td>12.9D0</td>
		<td>volume averaged ion temperature (keV). N.B. calculated from te if tratio &gt; 0.0</td>
	</tr>
	<tr>
		<td>ti0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>central ion temperature (keV)</td>
	</tr>
	<tr>
		<td>tin</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>density weighted average ion temperature (keV)</td>
	</tr>
	<tr>
		<td>tratio</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>ion temperature / electron temperature(used to calculate ti if tratio &gt; 0.0</td>
	</tr>
	<tr>
		<td>triang</td>
		<td>real</td>
		<td>0.36D0</td>
		<td>plasma separatrix triangularity (calculated if ishape=1, 3 or 4)</td>
	</tr>
	<tr>
		<td>triang95</td>
		<td>real</td>
		<td>0.24D0</td>
		<td>plasma triangularity at 95% surface (calculated if ishape &lt; 3)</td>
	</tr>
	<tr>
		<td>vol</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma volume (m3)</td>
	</tr>
	<tr>
		<td>vsbrn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>V-s needed during flat-top (heat + burn times) (Wb)</td>
	</tr>
	<tr>
		<td>vshift</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma/device midplane vertical shift - single null</td>
	</tr>
	<tr>
		<td>vsind</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>internal and external plasma inductance V-s (Wb)</td>
	</tr>
	<tr>
		<td>vsres</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>resistive losses in startup V-s (Wb)</td>
	</tr>
	<tr>
		<td>vsstt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total V-s needed (Wb)</td>
	</tr>
	<tr>
		<td>wallmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>average neutron wall load (MW/m2)</td>
	</tr>
	<tr>
		<td>wtgpd</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of fuel used per day (g)</td>
	</tr>
	<tr>
		<td>xarea</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma cross-sectional area (m2)</td>
	</tr>
	<tr>
		<td>zeff</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>plasma effective charge</td>
	</tr>
	<tr>
		<td>zeffai</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass weighted plasma effective charge</td>
	</tr>
</table>

## plasmod_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>comp</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all composition information for PLASMOD</td>
	</tr>
	<tr>
		<td>geom</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all geometry information for PLASMOD</td>
	</tr>
	<tr>
		<td>i_flag</td>
		<td>integer</td>
		<td>None</td>
		<td>Error flag for PLASMOD</td>
	</tr>
	<tr>
		<td>inp0</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing miscellaneous input information for PLASMOD</td>
	</tr>
	<tr>
		<td>loss</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all power loss information for PLASMOD</td>
	</tr>
	<tr>
		<td>mhd</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all mhd information for PLASMOD</td>
	</tr>
	<tr>
		<td>num</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all numerics information for PLASMOD</td>
	</tr>
	<tr>
		<td>ped</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all pedestal information for PLASMOD</td>
	</tr>
	<tr>
		<td>plasmod_Ainc</td>
		<td>real</td>
		<td>1.1d0</td>
		<td>increase of dt</td>
	</tr>
	<tr>
		<td>plasmod_capA</td>
		<td>real</td>
		<td>0.1d0</td>
		<td>first radial grid point</td>
	</tr>
	<tr>
		<td>plasmod_car_qdivt</td>
		<td>real</td>
		<td>1.0d-4</td>
		<td>dcar/d(qdivt)</td>
	</tr>
	<tr>
		<td>plasmod_chisaw</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>artificial diffusivity in m^2/s</td>
	</tr>
	<tr>
		<td>plasmod_chisawpos</td>
		<td>real</td>
		<td>-1.0d0</td>
		<td>position where artificial sawtooth diffusivity is added, -1 - uses q=1 position</td>
	</tr>
	<tr>
		<td>plasmod_contrpovr</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>control power in Paux/R (MW/m)</td>
	</tr>
	<tr>
		<td>plasmod_contrpovs</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>control power in Paux/lateral_area (MW/m2)</td>
	</tr>
	<tr>
		<td>plasmod_cxe_psepfac</td>
		<td>real</td>
		<td>1.0d-4</td>
		<td>dcxe/d(1-Psep/PLH)</td>
	</tr>
	<tr>
		<td>plasmod_dgy</td>
		<td>real</td>
		<td>1.0d-5</td>
		<td>Newton differential</td>
	</tr>
	<tr>
		<td>plasmod_dt</td>
		<td>real</td>
		<td>0.01d0</td>
		<td>plasmod time step</td>
	</tr>
	<tr>
		<td>plasmod_dtinc</td>
		<td>real</td>
		<td>2.0d0</td>
		<td>decrease of dt</td>
	</tr>
	<tr>
		<td>plasmod_dtmax</td>
		<td>real</td>
		<td>0.1d0</td>
		<td>plasmod max time step</td>
	</tr>
	<tr>
		<td>plasmod_dtmaxmax</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>stabilizing coefficient</td>
	</tr>
	<tr>
		<td>plasmod_dtmaxmin</td>
		<td>real</td>
		<td>0.15d0</td>
		<td>exponent of jipperdo2 </td>
	</tr>
	<tr>
		<td>plasmod_dtmin</td>
		<td>real</td>
		<td>0.05d0</td>
		<td>plasmod min time step</td>
	</tr>
	<tr>
		<td>plasmod_dx_cd</td>
		<td>real</td>
		<td>(/0.2d0, 0.03d0/)</td>
		<td>plasmod change in auxiliary current drive array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_dx_control</td>
		<td>real</td>
		<td>(/0.2d0, 0.03d0/)</td>
		<td>plasmod change in control array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_dx_fus</td>
		<td>real</td>
		<td>(/0.2d0, 0.03d0/)</td>
		<td>plasmod change in fusion power array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_dx_heat</td>
		<td>real</td>
		<td>(/0.2d0, 0.03d0/)</td>
		<td>plasmod change in auxiliary heating array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_eccdeff</td>
		<td>real</td>
		<td>0.3d0</td>
		<td>current drive multiplier: CD = eccdeffPCDTE/NE (not in use yet)</td>
	</tr>
	<tr>
		<td>plasmod_eopt</td>
		<td>real</td>
		<td>0.15d0</td>
		<td>exponent of jipperdo </td>
	</tr>
	<tr>
		<td>plasmod_fcdp</td>
		<td>real</td>
		<td>-1.0d0</td>
		<td>(P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power (iteration variable 150)</td>
	</tr>
	<tr>
		<td>plasmod_fpellet</td>
		<td>real</td>
		<td>0.5d0</td>
		<td>pellet frequency [Hz]</td>
	</tr>
	<tr>
		<td>plasmod_fradc</td>
		<td>real</td>
		<td>-1.0d0</td>
		<td>Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad) (iteration variable 151)</td>
	</tr>
	<tr>
		<td>plasmod_gamcdothers</td>
		<td>real</td>
		<td>1.0d0</td>
		<td>efficiency multiplier for non-CD heating. If 0.0 pheat treated as if had no CD associated</td>
	</tr>
	<tr>
		<td>plasmod_globtau</td>
		<td>real</td>
		<td>(/5.0d0, 5.0d0, 7.0d0, 5.0d0, ...</td>
		<td>tauparticle/tauE for D, T, He, Xe, Ar (not used for Xe)</td>
	</tr>
	<tr>
		<td>plasmod_i_equiltype</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for EMEQ setting to use either q95 or Ip as input:<br><ul><li> =1 EMEQ, solve with sawteeth and inputted q95.</li><li> =2 EMEQ, solve with sawteeth and inputted Ip (not recommended!).</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_i_impmodel</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for plasma inpurity concentration setting:<br><ul><li> =0 fixed concentration</li><li> =1 fixed concentration at pedestal top, then fixed density</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_i_modeltype</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for the transport model:<br><ul><li> =1 Simple gyrobohm scaling with imposed H factor &gt; 1. Other values give H factor as output</li><li> =111 roughly calibrated to give H=1 for DEMO, but not fixed H </li></ul></td>
	</tr>
	<tr>
		<td>plasmod_imptype</td>
		<td>integer</td>
		<td>(/14, 13, 9/)</td>
		<td>Impurities array:<br><ul><li> [1] - intrinsic impurity</li><li> [2] - Psep control</li><li> [3] - seeding for SOL (defaults: W, Xe, Ar)</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_iprocess</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for determining which functions to use:<br><ul><li> =0 use PLASMOD functions</li><li> =1 use PROCESS functions</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_isawt</td>
		<td>integer</td>
		<td>1</td>
		<td>switch to determine if plasmod solves with sawteeth:<br><ul><li> =0 no sawteeth</li><li> =1 solve with sawteeth</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_maxA</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>diagz 0 or 1</td>
	</tr>
	<tr>
		<td>plasmod_maxpauxor</td>
		<td>real</td>
		<td>20.0d0</td>
		<td>max allowed auxiliary power / R</td>
	</tr>
	<tr>
		<td>plasmod_nbi_energy</td>
		<td>real</td>
		<td>1000.0d0</td>
		<td>NBI energy [keV]</td>
	</tr>
	<tr>
		<td>plasmod_nchannels</td>
		<td>integer</td>
		<td>3</td>
		<td>leave this at 3</td>
	</tr>
	<tr>
		<td>plasmod_nx</td>
		<td>integer</td>
		<td>41</td>
		<td>number of interpolated grid points</td>
	</tr>
	<tr>
		<td>plasmod_nxt</td>
		<td>integer</td>
		<td>7</td>
		<td>number of solved grid points</td>
	</tr>
	<tr>
		<td>plasmod_pech</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>ech power (not in use yet) #TODO: units?</td>
	</tr>
	<tr>
		<td>plasmod_pedscal</td>
		<td>real</td>
		<td>1.0d0</td>
		<td>multiplication factor of the pedestal scaling in PLASMOD can be used to scan the pedestal height.</td>
	</tr>
	<tr>
		<td>plasmod_pfus</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>plasmod fusion power. If 0. not used (otherwise controlled with Pauxheat).</td>
	</tr>
	<tr>
		<td>plasmod_psepplh_sup</td>
		<td>real</td>
		<td>12000.0d0</td>
		<td>Psep/PLH if above this, use Xe</td>
	</tr>
	<tr>
		<td>plasmod_qdivt</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>divertor heat flux in MW/m^2, if 0, dont use SOL model</td>
	</tr>
	<tr>
		<td>plasmod_qnbi_psepfac</td>
		<td>real</td>
		<td>50.0d0</td>
		<td>dqnbi/d(1-Psep/PLH)</td>
	</tr>
	<tr>
		<td>plasmod_sawpertau</td>
		<td>real</td>
		<td>1.0d-6</td>
		<td>ratio between sawtooth period and confinement time</td>
	</tr>
	<tr>
		<td>plasmod_spellet</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>pellet mass in units of D in 10^19</td>
	</tr>
	<tr>
		<td>plasmod_test</td>
		<td>real</td>
		<td>1000000.0d0</td>
		<td>max number of plasmod iterations</td>
	</tr>
	<tr>
		<td>plasmod_tol</td>
		<td>real</td>
		<td>1.0d-10</td>
		<td>tolerance to be reached at each time step (%)</td>
	</tr>
	<tr>
		<td>plasmod_tolmin</td>
		<td>real</td>
		<td>10.1d0</td>
		<td>multiplier of etolm which can not be exceeded</td>
	</tr>
	<tr>
		<td>plasmod_v_loop</td>
		<td>real</td>
		<td>-1.0d-6</td>
		<td>target loop voltage. If lower than -1.e5 do not use.</td>
	</tr>
	<tr>
		<td>plasmod_x_cd</td>
		<td>real</td>
		<td>(/0.0d0, 0.0d0/)</td>
		<td>plasmod auxiliary current drive array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_x_control</td>
		<td>real</td>
		<td>(/0.0d0, 0.0d0/)</td>
		<td>plasmod control array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_x_fus</td>
		<td>real</td>
		<td>(/0.0d0, 0.0d0/)</td>
		<td>plasmod fusion power array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>plasmod_x_heat</td>
		<td>real</td>
		<td>(/0.0d0, 0.0d0/)</td>
		<td>plasmod auxiliary heating array:<br><ul><li> [1] - nbi</li><li> [2] - ech</li></ul></td>
	</tr>
	<tr>
		<td>radp</td>
		<td>type</td>
		<td>None</td>
		<td>Derived type containing all radial profile information for PLASMOD</td>
	</tr>
</table>

## power_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>delta_eta</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>double</td>
		<td>integer</td>
		<td>8</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmw_mech</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmwe_div</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmwe_fw_blkt</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>htpmwe_shld</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>iprimdiv</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pcoresystems</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pdivfraction</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>ppumpmw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pthermblkt</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pthermdiv</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pthermfw</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pthermfw_blkt</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>pthermshld</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>qac</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>qcl</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>qmisc</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>qss</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## primary_pumping_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>cp_he</td>
		<td>real</td>
		<td>5195.0D0</td>
		<td>specific heat capacity at constant pressure: helium (primary_pumping=3) [J/(kg.K)]</td>
	</tr>
	<tr>
		<td>dp_he</td>
		<td>real</td>
		<td>5.5D5</td>
		<td>pressure drop in FW and blanket coolant including heat exchanger and pipes (primary_pumping=3) [Pa]</td>
	</tr>
	<tr>
		<td>gamma_he</td>
		<td>real</td>
		<td>1.667D0</td>
		<td>ratio of specific heats for helium (primary_pumping=3)</td>
	</tr>
	<tr>
		<td>htpmw_fw_blkt</td>
		<td>real</td>
		<td>0.0d0</td>
		<td>mechanical pumping power for FW and blanket including heat exchanger and <br> pipes (primary_pumping=3) [MW]</td>
	</tr>
	<tr>
		<td>p_he</td>
		<td>real</td>
		<td>8.0D6</td>
		<td>pressure in FW and blanket coolant at pump exit (primary_pumping=3) [Pa]</td>
	</tr>
	<tr>
		<td>t_in_bb</td>
		<td>real</td>
		<td>573.13D0</td>
		<td>temperature in FW and blanket coolant at blanket entrance (primary_pumping=3) [K]</td>
	</tr>
	<tr>
		<td>t_out_bb</td>
		<td>real</td>
		<td>773.13D0</td>
		<td>temperature in FW and blanket coolant at blanket exit (primary_pumping=3) [K]</td>
	</tr>
</table>

## process_input
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>error</td>
		<td>logical</td>
		<td>.False.</td>
		<td></td>
	</tr>
	<tr>
		<td>error_message</td>
		<td>character</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>icode</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>infile</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>iptr</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>line</td>
		<td>character</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>linelen</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>lineno</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>maxlen</td>
		<td>integer</td>
		<td>300</td>
		<td></td>
	</tr>
	<tr>
		<td>nin</td>
		<td>integer</td>
		<td>10</td>
		<td></td>
	</tr>
	<tr>
		<td>outfile</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>report_changes</td>
		<td>integer</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>subscript_present</td>
		<td>logical</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## pulse_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>bctmp</td>
		<td>real</td>
		<td>320.0D0</td>
		<td>first wall bulk coolant temperature (C)</td>
	</tr>
	<tr>
		<td>bfw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outer radius of each first wall structural tube (m) (0.5 * average of fwith and fwoth)</td>
	</tr>
	<tr>
		<td>dtstor</td>
		<td>real</td>
		<td>300.0D0</td>
		<td>maximum allowable temperature change in stainless steel thermal storage block (K) (istore=3)</td>
	</tr>
	<tr>
		<td>istore</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for thermal storage method:<br><ul><li> =1 option 1 of Electrowatt report, AEA FUS 205</li><li> =2 option 2 of Electrowatt report, AEA FUS 205</li><li> =3 stainless steel block</li></ul></td>
	</tr>
	<tr>
		<td>itcycl</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for first wall axial stress model:<br><ul><li> =1 total axial constraint, no bending</li><li> =2 no axial constraint, no bending</li><li> =3 no axial constraint, bending</li></ul></td>
	</tr>
	<tr>
		<td>lpulse</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for reactor model:<br><ul><li> =0 continuous operation</li><li> =1 pulsed operation</li></ul></td>
	</tr>
</table>

## rebco_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>copperA_m2</td>
		<td>real</td>
		<td>None</td>
		<td>TF coil current / copper area (A/m2) </td>
	</tr>
	<tr>
		<td>copper_area</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>copper_rrr</td>
		<td>real</td>
		<td>100d0</td>
		<td>residual resistivity ratio copper in TF superconducting cable</td>
	</tr>
	<tr>
		<td>copper_thick</td>
		<td>real</td>
		<td>100.0D-6</td>
		<td>thickness of copper layer in tape (m) (iteration variable 139)</td>
	</tr>
	<tr>
		<td>coppera_m2_max</td>
		<td>real</td>
		<td>1D8</td>
		<td>Maximum TF coil current / copper area (A/m2)</td>
	</tr>
	<tr>
		<td>croco_area</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>croco_id</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Inner diameter of CroCo copper tube (m)</td>
	</tr>
	<tr>
		<td>croco_od</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Outer diameter of CroCo strand (m)</td>
	</tr>
	<tr>
		<td>croco_thick</td>
		<td>real</td>
		<td>2.5D-3</td>
		<td>Thickness of CroCo copper tube (m) (iteration variable 158)</td>
	</tr>
	<tr>
		<td>f_coppera_m2</td>
		<td>real</td>
		<td>1d0</td>
		<td>f-value for constraint 75: TF coil current / copper area &lt; copperA_m2_max</td>
	</tr>
	<tr>
		<td>hastelloy_area</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>hastelloy_thickness</td>
		<td>real</td>
		<td>50.0D-6</td>
		<td>thickness of Hastelloy layer in tape (m)</td>
	</tr>
	<tr>
		<td>rebco_area</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>rebco_thickness</td>
		<td>real</td>
		<td>1.0D-6</td>
		<td>thickness of REBCO layer in tape (m) (iteration variable 138)</td>
	</tr>
	<tr>
		<td>solder_area</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>stack_thickness</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>tape_thickness</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>tape_width</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Mean width of tape (m)</td>
	</tr>
	<tr>
		<td>tapes</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## reinke_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>vcritx</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
</table>

## reinke_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>fzactual</td>
		<td>real</td>
		<td>0.001D0</td>
		<td>Actual impurity fraction of divertor impurity (impvardiv) in the SoL (taking <br> impurity_enrichment into account) (iteration variable 148)</td>
	</tr>
	<tr>
		<td>fzmin</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Minimum impurity fraction necessary for detachment. This is the impurity at the SOL/Div.</td>
	</tr>
	<tr>
		<td>impvardiv</td>
		<td>integer</td>
		<td>9</td>
		<td>Index of impurity to be iterated for Reinke divertor detachment criterion</td>
	</tr>
	<tr>
		<td>lhat</td>
		<td>real</td>
		<td>4.33D0</td>
		<td>Connection length factor L|| = lhat qstar R for Reinke criterion, default value from<br> Post et al. 1995 J. Nucl. Mat.  220-2 1014</td>
	</tr>
	<tr>
		<td>reinke_mode</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for Reinke criterion H/I mode:<br><ul><li> =0 H-mode</li><li> =1 I-mode</li></ul></td>
	</tr>
</table>

## scan_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>ipnscns</td>
		<td>integer</td>
		<td>1000</td>
		<td>ipnscns /1000/ FIX : maximum number of scan points</td>
	</tr>
	<tr>
		<td>ipnscnv</td>
		<td>integer</td>
		<td>54</td>
		<td>ipnscnv /45/ FIX : number of available scan variables</td>
	</tr>
	<tr>
		<td>isweep</td>
		<td>integer</td>
		<td>0</td>
		<td>isweep /0/ : number of scan points to calculate</td>
	</tr>
	<tr>
		<td>isweep_2</td>
		<td>integer</td>
		<td>0</td>
		<td>isweep_2 /0/ : number of 2D scan points to calculate</td>
	</tr>
	<tr>
		<td>nsweep</td>
		<td>integer</td>
		<td>1</td>
		<td>nsweep /1/ : switch denoting quantity to scan:<br>          1  aspect<br>          2  hldivlim<br>          3  pnetelin<br>          4  hfact<br>          5  oacdcp<br>          6  walalw<br>          7  beamfus0<br>          8  fqval<br>          9  te<br>          10 boundu(15: fvs)<br>          11 dnbeta<br>          12 bscfmax (use negative values only)<br>          13 boundu(10: hfact)<br>          14 fiooic<br>          15 fjprot<br>          16 rmajor<br>          17 bmxlim<br>          18 gammax<br>          19 boundl(16: ohcth)<br>          20 tbrnmn<br>          21 not used<br>          22 cfactr (N.B. requires iavail=0)<br>          23 boundu(72: fipir)<br>          24 powfmax<br>          25 kappa<br>          26 triang<br>          27 tbrmin (for blktmodel &gt; 0 only)<br>          28 bt<br>          29 coreradius<br>          30 fimpvar<br>          31 taulimit<br>          32 epsvmc<br>          33 ttarget<br>          34 qtargettotal<br>          35 lambda_q_omp<br>          36 lambda_target<br>          37 lcon_factor<br>          38 Neon upper limit<br>          39 Argon upper limit<br>          40 Xenon upper limit<br>          41 blnkoth<br>          42 Argon fraction fimp(9)<br>          43 normalised minor radius at which electron cyclotron current drive is maximum<br>          44 Allowable tresca stress in tf coil structural material<br>          45 Minimum allowable temperature margin ; tf coils<br>          46 boundu(150) fgwsep<br>          47 impurity_enrichment(9) Argon impurity enrichment<br>          48 TF coil - n_pancake (integer turn winding pack)<br>          49 TF coil - n_layer (integer turn winding pack)<br>          50 Xenon fraction fimp(13)<br>          51 Power fraction to lower DN Divertor ftar<br>          52 SoL radiation fraction <br>          54 GL_nbti upper critical field at 0 Kelvin </td>
	</tr>
	<tr>
		<td>nsweep_2</td>
		<td>integer</td>
		<td>3</td>
		<td>nsweep_2 /3/ : switch denoting quantity to scan for 2D scan:</td>
	</tr>
	<tr>
		<td>scan_dim</td>
		<td>integer</td>
		<td>1</td>
		<td>scan_dim /1/ : 1-D or 2-D scan switch (1=1D, 2=2D)</td>
	</tr>
	<tr>
		<td>sweep</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>sweep(ipnscns) /../: actual values to use in scan</td>
	</tr>
	<tr>
		<td>sweep_2</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>sweep_2(ipnscns) /../: actual values to use in 2D scan</td>
	</tr>
</table>

## sctfcoil_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>T1</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>a_case_front</td>
		<td>real</td>
		<td>None</td>
		<td>Front casing area [m2]</td>
	</tr>
	<tr>
		<td>a_case_nose</td>
		<td>real</td>
		<td>None</td>
		<td>Nose casing area [m2]</td>
	</tr>
	<tr>
		<td>a_tf_ins</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard coil insulation cross-section per coil [m2]</td>
	</tr>
	<tr>
		<td>a_tf_steel</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard coil steel coil cross-sectional area [m2]</td>
	</tr>
	<tr>
		<td>awpc</td>
		<td>real</td>
		<td>None</td>
		<td>Total cross-sectional area of winding pack including<br> GW insulation and insertion gap [m2]</td>
	</tr>
	<tr>
		<td>awptf</td>
		<td>real</td>
		<td>None</td>
		<td>Total cross-sectional area of winding pack [m2]</td>
	</tr>
	<tr>
		<td>conductor</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>copper</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>croco_strand</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>estotft</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>f_tf_ins</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard coil insulation fraction [-]</td>
	</tr>
	<tr>
		<td>f_tf_steel</td>
		<td>real</td>
		<td>None</td>
		<td>Inboard coil steel fraction [-]</td>
	</tr>
	<tr>
		<td>h_cp_top</td>
		<td>real</td>
		<td>None</td>
		<td>Vertical distance from the midplane to the top of the tapered section [m]</td>
	</tr>
	<tr>
		<td>hastelloy</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>helium</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>jacket</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>r_tf_outboard_in</td>
		<td>real</td>
		<td>None</td>
		<td>Radial position of plasma-facing edge of TF coil outboard leg [m]</td>
	</tr>
	<tr>
		<td>r_tf_outboard_out</td>
		<td>real</td>
		<td>None</td>
		<td>Radial position of outer edge of TF coil inboard leg [m]</td>
	</tr>
	<tr>
		<td>r_wp_centre</td>
		<td>real</td>
		<td>None</td>
		<td>Radial position of centre and centre of winding pack [m]</td>
	</tr>
	<tr>
		<td>r_wp_inner</td>
		<td>real</td>
		<td>None</td>
		<td>Radial position of inner edge and centre of winding pack [m]</td>
	</tr>
	<tr>
		<td>r_wp_outer</td>
		<td>real</td>
		<td>None</td>
		<td>Radial position of outer edge and centre of winding pack [m]</td>
	</tr>
	<tr>
		<td>solder</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>t_cable</td>
		<td>real</td>
		<td>None</td>
		<td>Cable area averaged dimension (square shape) [m]</td>
	</tr>
	<tr>
		<td>t_cable_radial</td>
		<td>real</td>
		<td>None</td>
		<td>Cable area radial and toroidal dimension (integer turn only) [m]</td>
	</tr>
	<tr>
		<td>t_cable_toroidal</td>
		<td>real</td>
		<td>None</td>
		<td>Cable area radial and toroidal dimension (integer turn only) [m]</td>
	</tr>
	<tr>
		<td>t_conductor_radial</td>
		<td>real</td>
		<td>None</td>
		<td>Conductor area radial and toroidal dimension (integer turn only) [m]</td>
	</tr>
	<tr>
		<td>t_conductor_toroidal</td>
		<td>real</td>
		<td>None</td>
		<td>Conductor area radial and toroidal dimension (integer turn only) [m]</td>
	</tr>
	<tr>
		<td>t_lat_case_av</td>
		<td>real</td>
		<td>None</td>
		<td>Average lateral casing thickness [m]</td>
	</tr>
	<tr>
		<td>t_turn</td>
		<td>real</td>
		<td>None</td>
		<td>Turn area averaged dimension (square shape) [m]</td>
	</tr>
	<tr>
		<td>t_turn_radial</td>
		<td>real</td>
		<td>None</td>
		<td>Turn radial and toroidal dimension (integer turn only) [m]</td>
	</tr>
	<tr>
		<td>t_turn_toroidal</td>
		<td>real</td>
		<td>None</td>
		<td>Turn radial and toroidal dimension (integer turn only) [m]</td>
	</tr>
	<tr>
		<td>t_wp_toroidal</td>
		<td>real</td>
		<td>None</td>
		<td>Minimal toroidal thickness of of winding pack [m]</td>
	</tr>
	<tr>
		<td>t_wp_toroidal_av</td>
		<td>real</td>
		<td>None</td>
		<td>Averaged toroidal thickness of of winding pack [m]</td>
	</tr>
	<tr>
		<td>tan_theta_coil</td>
		<td>real</td>
		<td>None</td>
		<td>Tan half toroidal angular extent of a single TF coil inboard leg</td>
	</tr>
	<tr>
		<td>tau2</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>tf_fit_t</td>
		<td>real</td>
		<td>None</td>
		<td>Dimensionless winding pack width</td>
	</tr>
	<tr>
		<td>tf_fit_y</td>
		<td>real</td>
		<td>None</td>
		<td>Ratio of peak field with ripple to nominal axisymmetric peak field</td>
	</tr>
	<tr>
		<td>tf_fit_z</td>
		<td>real</td>
		<td>None</td>
		<td>Dimensionless winding pack radial thickness</td>
	</tr>
	<tr>
		<td>tfc_current</td>
		<td>real</td>
		<td>None</td>
		<td>Current in each TF coil</td>
	</tr>
	<tr>
		<td>theta_coil</td>
		<td>real</td>
		<td>None</td>
		<td>Half toroidal angular extent of a single TF coil inboard leg</td>
	</tr>
	<tr>
		<td>time2</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>vforce_inboard_tot</td>
		<td>real</td>
		<td>None</td>
		<td>Total inboard vertical tension (all coils) [N] </td>
	</tr>
	<tr>
		<td>vol_case_cp</td>
		<td>real</td>
		<td>None</td>
		<td>Volume of the CP outer casing cylinder</td>
	</tr>
	<tr>
		<td>vol_cond</td>
		<td>real</td>
		<td>None</td>
		<td>Total conductor insulator volume [m3]</td>
	</tr>
	<tr>
		<td>vol_cond_leg</td>
		<td>real</td>
		<td>None</td>
		<td>Outboard leg conductor insulator volume [m3]</td>
	</tr>
	<tr>
		<td>vol_ins</td>
		<td>real</td>
		<td>None</td>
		<td>Total/CP insulator insulator volume [m3]</td>
	</tr>
	<tr>
		<td>vol_ins_cp</td>
		<td>real</td>
		<td>None</td>
		<td>CP insulator insulator volume [m3]</td>
	</tr>
	<tr>
		<td>vol_ins_leg</td>
		<td>real</td>
		<td>None</td>
		<td>Outboard leg insulator volume [m3]</td>
	</tr>
</table>

## startup_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>ftaue</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>factor in energy confinement time formula</td>
	</tr>
	<tr>
		<td>gtaue</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>offset term in energy confinement time scaling</td>
	</tr>
	<tr>
		<td>nign</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>electron density at ignition (start-up) (/m3)</td>
	</tr>
	<tr>
		<td>ptaue</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>exponent for density term in energy confinement time formula</td>
	</tr>
	<tr>
		<td>qtaue</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>exponent for temperature term in energy confinement time formula</td>
	</tr>
	<tr>
		<td>rtaue</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>exponent for power term in energy confinement time formula</td>
	</tr>
	<tr>
		<td>tign</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>electron temperature at ignition (start-up) (keV)</td>
	</tr>
</table>

## stellarator_module
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>config</td>
		<td>type</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>f_a</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>f_b</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>f_i</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>f_n</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>f_r</td>
		<td>real</td>
		<td>None</td>
		<td></td>
	</tr>
	<tr>
		<td>first_call</td>
		<td>logical</td>
		<td>.true.</td>
		<td></td>
	</tr>
</table>

## stellarator_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>bmn</td>
		<td>real</td>
		<td>1.0D-3</td>
		<td>relative radial field perturbation</td>
	</tr>
	<tr>
		<td>f_asym</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>divertor heat load peaking factor</td>
	</tr>
	<tr>
		<td>f_rad</td>
		<td>real</td>
		<td>0.85D0</td>
		<td>radiated power fraction in SOL</td>
	</tr>
	<tr>
		<td>f_w</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>island size fraction factor</td>
	</tr>
	<tr>
		<td>fdivwet</td>
		<td>real</td>
		<td>0.333333333333333D0</td>
		<td>wetted fraction of the divertor area</td>
	</tr>
	<tr>
		<td>flpitch</td>
		<td>real</td>
		<td>1.0D-3</td>
		<td>field line pitch (rad)</td>
	</tr>
	<tr>
		<td>hportamax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum available area for horizontal ports (m2)</td>
	</tr>
	<tr>
		<td>hportpmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum available poloidal extent for horizontal ports (m)</td>
	</tr>
	<tr>
		<td>hporttmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum available toroidal extent for horizontal ports (m)</td>
	</tr>
	<tr>
		<td>iotabar</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>rotational transform (reciprocal of tokamak q) for stellarator confinement time scaling laws</td>
	</tr>
	<tr>
		<td>istell</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for stellarator option (set via device.dat):<br><ul><li> =0 use tokamak model</li><li> =1 use stellarator model: Helias5-b</li><li> =2 use stellarator model: Helias4-b</li><li> =3 use stellarator model: Helias3-b</li></ul></td>
	</tr>
	<tr>
		<td>isthtr</td>
		<td>integer</td>
		<td>3</td>
		<td>Switch for stellarator auxiliary heating method:<br><ul><li> = 1electron cyclotron resonance heating</li><li> = 2lower hybrid heating</li><li> = 3neutral beam injection</li></ul></td>
	</tr>
	<tr>
		<td>m_res</td>
		<td>integer</td>
		<td>5</td>
		<td>poloidal resonance number</td>
	</tr>
	<tr>
		<td>n_res</td>
		<td>integer</td>
		<td>5</td>
		<td>toroidal resonance number</td>
	</tr>
	<tr>
		<td>shear</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>magnetic shear, derivative of iotabar</td>
	</tr>
	<tr>
		<td>vportamax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum available area for vertical ports (m2)</td>
	</tr>
	<tr>
		<td>vportpmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum available poloidal extent for vertical ports (m)</td>
	</tr>
	<tr>
		<td>vporttmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>maximum available toroidal extent for vertical ports (m)</td>
	</tr>
</table>

## structure_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>aintmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>intercoil structure mass (kg)</td>
	</tr>
	<tr>
		<td>clgsmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>gravity support structure for TF coil, PF coil and intercoil support systems (kg)</td>
	</tr>
	<tr>
		<td>coldmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total mass of components at cryogenic temperatures (kg)</td>
	</tr>
	<tr>
		<td>fncmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>PF coil outer support fence mass (kg)</td>
	</tr>
	<tr>
		<td>gsmass</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>reactor core gravity support mass (kg)</td>
	</tr>
</table>

## tfcoil_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>a_cp_cool</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Centrepost cooling area toroidal cross-section (constant over the whole CP)</td>
	</tr>
	<tr>
		<td>acasetf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>external case area per coil (inboard leg) (m2)</td>
	</tr>
	<tr>
		<td>acasetfo</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>external case area per coil (outboard leg) (m2)</td>
	</tr>
	<tr>
		<td>acndttf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>area of the cable conduit (m2)</td>
	</tr>
	<tr>
		<td>acond</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>conductor area (winding pack) (m2)</td>
	</tr>
	<tr>
		<td>acs</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Area of space inside conductor (m2)</td>
	</tr>
	<tr>
		<td>acstf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>internal area of the cable space (m2)</td>
	</tr>
	<tr>
		<td>aiwp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>winding pack insulation area (m2)</td>
	</tr>
	<tr>
		<td>alstrtf</td>
		<td>real</td>
		<td>6.0D8</td>
		<td>Allowable Tresca stress in TF coil structural material (Pa)</td>
	</tr>
	<tr>
		<td>arealeg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>outboard TF leg area (m2)</td>
	</tr>
	<tr>
		<td>aswp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>winding pack structure area (m2)</td>
	</tr>
	<tr>
		<td>avwp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>winding pack void (He coolant) area (m2)</td>
	</tr>
	<tr>
		<td>awphec</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>winding pack He coil area (m2)</td>
	</tr>
	<tr>
		<td>b_crit_upper_nbti</td>
		<td>real</td>
		<td>14.86D0</td>
		<td>upper critical field of GL_nbti</td>
	</tr>
	<tr>
		<td>bcritsc</td>
		<td>real</td>
		<td>24.0D0</td>
		<td>upper critical field (T) for Nb3Sn superconductor at zero temperature and <br> strain (i_tf_sc_mat=4, =bc20m)</td>
	</tr>
	<tr>
		<td>bmaxtf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mean peak field at TF coil (T)</td>
	</tr>
	<tr>
		<td>bmaxtfrp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak field at TF conductor with ripple (T)</td>
	</tr>
	<tr>
		<td>casestr</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>case strain</td>
	</tr>
	<tr>
		<td>casthi</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard TF coil case plasma side thickness (m) (calculated for stellarators)</td>
	</tr>
	<tr>
		<td>casthi_fraction</td>
		<td>real</td>
		<td>0.05D0</td>
		<td>inboard TF coil case plasma side thickness as a fraction of tfcth</td>
	</tr>
	<tr>
		<td>casthi_is_fraction</td>
		<td>logical</td>
		<td>None</td>
		<td>logical switch to make casthi a fraction of TF coil thickness (casthi_fraction)</td>
	</tr>
	<tr>
		<td>casths</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inboard TF coil sidewall case thickness (m) (calculated for stellarators)</td>
	</tr>
	<tr>
		<td>casths_fraction</td>
		<td>real</td>
		<td>0.03D0</td>
		<td>inboard TF coil sidewall case thickness as a fraction of tftort</td>
	</tr>
	<tr>
		<td>cdtfleg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF outboard leg current density (A/m2) (resistive coils only)</td>
	</tr>
	<tr>
		<td>cforce</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>centering force on inboard leg (per coil) (N/m)</td>
	</tr>
	<tr>
		<td>cpttf</td>
		<td>real</td>
		<td>7.0e4</td>
		<td>TF coil current per turn (A). (calculated for stellarators) (calculated for <br> integer-turn TF coils i_tf_turns_integer=1) (iteration variable 60)</td>
	</tr>
	<tr>
		<td>cpttf_max</td>
		<td>real</td>
		<td>9.0e4</td>
		<td>Max TF coil current per turn [A]. (for stellarators and i_tf_turns_integer=1) <br> (constraint equation 77)</td>
	</tr>
	<tr>
		<td>croco_quench_temperature</td>
		<td>real</td>
		<td>0D0</td>
		<td>CroCo strand: Actual temp reached during a quench (K)</td>
	</tr>
	<tr>
		<td>dcase</td>
		<td>real</td>
		<td>8000.0D0</td>
		<td>density of coil case (kg/m3)</td>
	</tr>
	<tr>
		<td>dcond</td>
		<td>real</td>
		<td>['9000.0D0', '9000.0D0', '9000...</td>
		<td>density of superconductor type given by i_tf_sc_mat/isumatoh/isumatpf (kg/m3)</td>
	</tr>
	<tr>
		<td>dcondins</td>
		<td>real</td>
		<td>1800.0D0</td>
		<td>density of conduit + ground-wall insulation (kg/m3)</td>
	</tr>
	<tr>
		<td>deflect</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil radial deflection (displacement) radial distribution [m]</td>
	</tr>
	<tr>
		<td>dhecoil</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>diameter of He coil in TF winding (m)</td>
	</tr>
	<tr>
		<td>dr_tf_wp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>radial thickness of winding pack (m) (iteration variable 140) (issue #514)</td>
	</tr>
	<tr>
		<td>drtop</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>centrepost taper maximum radius adjustment (m)</td>
	</tr>
	<tr>
		<td>dthet</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>angle of arc i (rad)</td>
	</tr>
	<tr>
		<td>dtiocool</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>inlet / outlet TF coil coolant temperature rise (K)  </td>
	</tr>
	<tr>
		<td>dztop</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>centrepost taper height adjustment (m)</td>
	</tr>
	<tr>
		<td>eff_tf_cryo</td>
		<td>real</td>
		<td>-1.0D0</td>
		<td>TF cryoplant efficiency (compared to pefect Carnot cycle).<br> Using -1 set the default value depending on magnet technology:<br><ul><li> i_tf_sup = 1 : SC magnet, eff_tf_cryo = 0.13 (ITER design)</li><li> i_tf_sup = 2 : Cryo-aluminium, eff_tf_cryo = 0.4</li></ul></td>
	</tr>
	<tr>
		<td>estotftgj</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total stored energy in the toroidal field (GJ)</td>
	</tr>
	<tr>
		<td>etapump</td>
		<td>real</td>
		<td>0.8D0</td>
		<td>centrepost coolant pump efficiency</td>
	</tr>
	<tr>
		<td>eyoung_al</td>
		<td>real</td>
		<td>69.0D9</td>
		<td>Aluminium young modulus.  Default value taken from wikipedia</td>
	</tr>
	<tr>
		<td>eyoung_copper</td>
		<td>real</td>
		<td>117.0D9</td>
		<td>Copper young modulus. Default value taken from wikipedia</td>
	</tr>
	<tr>
		<td>eyoung_ins</td>
		<td>real</td>
		<td>1.0D8</td>
		<td>Insulator Young's modulus [Pa]. Default value (1.0D8) setup the following values<br>  - SC TF, eyoung_ins = 20 Gpa (default value from DDD11-2 v2 2 (2009))<br>  - Al TF, eyoung_ins = 2.5 GPa (Kapton polymer)</td>
	</tr>
	<tr>
		<td>eyoung_res_tf_buck</td>
		<td>real</td>
		<td>150.0D9</td>
		<td>Resistive TF magnets bucking cylinder young modulus (Pa)</td>
	</tr>
	<tr>
		<td>eyoung_steel</td>
		<td>real</td>
		<td>2.05D11</td>
		<td>Steel case Young's modulus (Pa) (default value from DDD11-2 v2 2 (2009))</td>
	</tr>
	<tr>
		<td>eyoung_winding</td>
		<td>real</td>
		<td>6.6D8</td>
		<td>SC TF coil winding Young's modulus (Pa)</td>
	</tr>
	<tr>
		<td>eyzwp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Winding pack vertical Young's modulus (Pa)</td>
	</tr>
	<tr>
		<td>f_vforce_inboard</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>Fraction of the total vertical force taken by the TF inboard leg tension<br> Not used for resistive itart=1 (sliding joints)</td>
	</tr>
	<tr>
		<td>farc4tf</td>
		<td>real</td>
		<td>0.7D0</td>
		<td>factor to size height of point 4 on TF coil</td>
	</tr>
	<tr>
		<td>fcoolcp</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>coolant fraction of TF coil inboard legs (iteration variable 23)</td>
	</tr>
	<tr>
		<td>fcoolleg</td>
		<td>real</td>
		<td>0.2D0</td>
		<td>coolant fraction of TF coil inboard legs</td>
	</tr>
	<tr>
		<td>fcutfsu</td>
		<td>real</td>
		<td>0.69D0</td>
		<td>copper fraction of cable conductor (TF coils)<br> (iteration variable 59)</td>
	</tr>
	<tr>
		<td>fhts</td>
		<td>real</td>
		<td>0.5D0</td>
		<td>technology adjustment factor for critical current density fit for isumat..=2 <br> Bi-2212 superconductor, to describe the level of technology assumed (i.e. to <br> account for stress, fatigue, radiation, AC losses, joints or manufacturing <br> variations; 1.0 would be very optimistic)</td>
	</tr>
	<tr>
		<td>frhocp</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Centrepost resistivity enhancement factor. For itart=0, this factor <br> is used for the whole magnet </td>
	</tr>
	<tr>
		<td>frholeg</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Ouboard legs resistivity enhancement factor. Only used for itart=1.</td>
	</tr>
	<tr>
		<td>ftoroidalgap</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>F-value for minimum tftort (constraint equation 82)</td>
	</tr>
	<tr>
		<td>i_cp_joints</td>
		<td>integer</td>
		<td>-1</td>
		<td>Switch for CP demoutable joints type<br>  -= 0 : Clampled joints<br>  -= 1 : Sliding joints<br> Default value (-1) choses : <br>   Sliding joints for resistive magnets (i_tf_sup = 0, 2)<br>   Clampled joints for superconducting magents (i_tf_sup = 1)</td>
	</tr>
	<tr>
		<td>i_tf_bucking</td>
		<td>integer</td>
		<td>-1</td>
		<td>Switch for TF inboard suport structure design:<br>Default setting for backward compatibility<br>     - if copper resistive TF (i_tf_sup = 0) : Free standing TF without bucking structure <br>     - if Superconducting TF  (i_tf_sup = 1) : Free standing TF with a steel casing<br>     - if aluminium  TF       (i_tf_sup = 2) : Free standing TF with a bucking structure<br>     Rem : the case is a bucking structure<br> - =0 : Free standing TF without case/bucking cyliner (only a conductor layer)<br> - =1 : Free standing TF with a case/bucking cylinder made of <br>     - if copper resistive     TF (i_tf_sup = 0) : used defined bucking cylinder<br>     - if Superconducting      TF (i_tf_sup = 1) : Steel casing<br>     - if aluminium resisitive TF (i_tf_sup = 2) : used defined bucking cylinder<br> - =2 : The TF is in contact with the CS : "bucked and weged design"<br>       Fast version : thin TF-CS interface neglected in the stress calculations (3 layers)<br> - =3 : The TF is in contact with the CS : "bucked and weged design"<br>       Full version : thin TF-CS Kapton interface introduced in the stress calculations (4 layers)</td>
	</tr>
	<tr>
		<td>i_tf_case_geom</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for TF case geometry selection<br>   0 : Circular front case (ITER design)<br>   1 : Straight front case</td>
	</tr>
	<tr>
		<td>i_tf_plane_stress</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for the TF coil stress model<br>   0 : New generalized plane strain formulation <br>   1 : Old plane stress model (only for SC)</td>
	</tr>
	<tr>
		<td>i_tf_sc_mat</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for superconductor material in TF coils:<br><ul><li> =1 ITER Nb3Sn critical surface model with standard<br>   ITER parameters</li><li> =2 Bi-2212 high temperature superconductor (range of<br>   validity T &lt; 20K, adjusted field b &lt; 104 T, B &gt; 6 T)</li><li> =3 NbTi</li><li> =4 ITER Nb3Sn model with user-specified parameters</li><li> =5 WST Nb3Sn parameterisation</li><li> =6 REBCO HTS tape in CroCo strand</li></ul></td>
	</tr>
	<tr>
		<td>i_tf_shape</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for TF coil toroidal shape:<br><ul><li> =0  Default value : Picture frame coil for TART / PROCESS D-shape for non itart</li><li> =1  PROCESS D-shape : parametrise with 2 arcs </li><li> =2  Picture frame coils </li></ul></td>
	</tr>
	<tr>
		<td>i_tf_sup</td>
		<td>integer</td>
		<td>1</td>
		<td>Switch for TF coil conductor model:<br><ul><li> =0 copper</li><li> =1 superconductor</li><li> =2 Cryogenic aluminium</li></ul></td>
	</tr>
	<tr>
		<td>i_tf_tresca</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for TF coil conduit Tresca stress criterion:<br>   0 : Tresca (no adjustment);<br>   1 : Tresca with CEA adjustment factors (radial+2%, vertical+60%) </td>
	</tr>
	<tr>
		<td>i_tf_turns_integer</td>
		<td>integer</td>
		<td>0</td>
		<td>Switch for TF coil integer/non-integer turns:<br>   0 : non-integer turns<br>   1 : integer turns</td>
	</tr>
	<tr>
		<td>i_tf_wp_geom</td>
		<td>integer</td>
		<td>-1</td>
		<td>Switch for TF WP geometry selection<br>   0 : Rectangular geometry <br>   1 : Double rectangular geometry <br>   2 : Trapezoidal geometry (constant lateral casing thickness)<br> Default setting for backward compatibility <br>   if i_tf_turns_integer = 0 : Double rectangular<br>   if i_tf_turns_integer = 1 : Rectangular </td>
	</tr>
	<tr>
		<td>insstrain</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Radial strain in insulator</td>
	</tr>
	<tr>
		<td>insulation_area</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>single turn insulation area (m2)</td>
	</tr>
	<tr>
		<td>jbus</td>
		<td>real</td>
		<td>1.25D6</td>
		<td>bussing current density (A/m2)</td>
	</tr>
	<tr>
		<td>jwdgcrt</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>critical current density for winding pack (A/m2)</td>
	</tr>
	<tr>
		<td>jwdgpro</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>allowable TF coil winding pack current density, for dump temperature rise protection (A/m2)</td>
	</tr>
	<tr>
		<td>jwptf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>winding pack engineering current density (A/m2)</td>
	</tr>
	<tr>
		<td>layer_ins</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Additional insulation thickness between layers (m)</td>
	</tr>
	<tr>
		<td>max_force_density</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Maximal (WP averaged) force density in TF coils at 1 point. (MN/m3)</td>
	</tr>
	<tr>
		<td>n_layer</td>
		<td>integer</td>
		<td>20</td>
		<td>Number of layers in TF coil. Only used if i_tf_turns_integer=1</td>
	</tr>
	<tr>
		<td>n_pancake</td>
		<td>integer</td>
		<td>10</td>
		<td>Number of pancakes in TF coil. Only used if i_tf_turns_integer=1</td>
	</tr>
	<tr>
		<td>n_rad_per_layer</td>
		<td>integer</td>
		<td>100</td>
		<td>Size of the arrays per layers storing the radial dependent stress <br> quantities (stresses, strain displacement etc..)</td>
	</tr>
	<tr>
		<td>n_radial_array</td>
		<td>integer</td>
		<td>50</td>
		<td>Size of the radial distribution arrays per layers<br> used for stress, strain and displacement distibution</td>
	</tr>
	<tr>
		<td>n_tf</td>
		<td>real</td>
		<td>16.0D0</td>
		<td>Number of TF coils (default = 50 for stellarators). Number of TF coils outer legs for ST</td>
	</tr>
	<tr>
		<td>n_tf_graded_layers</td>
		<td>integer</td>
		<td>1</td>
		<td>Number of layers of different stress properties in the WP. If n_tf_graded_layers &gt; 1, <br> a graded coil is condidered</td>
	</tr>
	<tr>
		<td>n_tf_joints</td>
		<td>integer</td>
		<td>4</td>
		<td>Number of joint per turn</td>
	</tr>
	<tr>
		<td>n_tf_joints_contact</td>
		<td>integer</td>
		<td>6</td>
		<td>Number of contact per sliding joint</td>
	</tr>
	<tr>
		<td>n_tf_stress_layers</td>
		<td>integer</td>
		<td>0</td>
		<td>Number of layers considered for the inboard TF stress calculations<br> set in initial.f90 from i_tf_bucking and n_tf_graded_layers</td>
	</tr>
	<tr>
		<td>n_tf_turn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>number of turns per TF coil</td>
	</tr>
	<tr>
		<td>ncool</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>number of centrepost coolant tubes</td>
	</tr>
	<tr>
		<td>oacdcp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Overall current density in TF coil inboard legs midplane (A/m2)<br> Rem SK : Not used in tfcoil to set the current any more. Should not be used as<br> iteration variable 12 any more. It is now calculated.</td>
	</tr>
	<tr>
		<td>poisson_al</td>
		<td>real</td>
		<td>0.35D0</td>
		<td>Aluminium Poisson's ratio. <br> Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html</td>
	</tr>
	<tr>
		<td>poisson_copper</td>
		<td>real</td>
		<td>0.35D0</td>
		<td>Copper Poisson's ratio. Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html</td>
	</tr>
	<tr>
		<td>poisson_steel</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>Steel Poisson's ratio </td>
	</tr>
	<tr>
		<td>ppump</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>centrepost coolant pump power (W)</td>
	</tr>
	<tr>
		<td>pres_joints</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Calculated TF joints resistive power losses [W]</td>
	</tr>
	<tr>
		<td>prescp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>resistive power in the centrepost (itart=1) [W].<br> If itart=0, this variable is the ressitive power on the whole magnet</td>
	</tr>
	<tr>
		<td>presleg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Summed resistive power in the TF coil legs [W]. Remain 0 if itart=0.</td>
	</tr>
	<tr>
		<td>ptempalw</td>
		<td>real</td>
		<td>473.15D0</td>
		<td>maximum peak centrepost temperature (K) (constraint equation 44)</td>
	</tr>
	<tr>
		<td>quench_detection_ef</td>
		<td>real</td>
		<td>0D0</td>
		<td>Electric field at which TF quench is detected and discharge begins (V/m)</td>
	</tr>
	<tr>
		<td>quench_model</td>
		<td>character</td>
		<td>'exponential'</td>
		<td>switch for TF coil quench model (Only applies to REBCO magnet at present, issue #522):<br><ul><li> ='exponential' exponential quench with constant discharge resistor</li><li> ='linear' quench with constant voltage</li></ul></td>
	</tr>
	<tr>
		<td>radctf</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>radius of arc i (m)</td>
	</tr>
	<tr>
		<td>radial_array</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Array refining the radii of the stress calculations arrays</td>
	</tr>
	<tr>
		<td>rbmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Radius of maximum TF B-field (m)</td>
	</tr>
	<tr>
		<td>rcool</td>
		<td>real</td>
		<td>0.005D0</td>
		<td>average radius of coolant channel (m) (iteration variable 69)</td>
	</tr>
	<tr>
		<td>rho_tf_joints</td>
		<td>real</td>
		<td>2.5D-10</td>
		<td>TF joints surfacic resistivity [ohm.m]. Feldmetal joints assumed.</td>
	</tr>
	<tr>
		<td>rhocp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil inboard leg resistivity [Ohm-m]. If itart=0, this variable is the <br> average resistivity over the whole magnet</td>
	</tr>
	<tr>
		<td>rhotfbus</td>
		<td>real</td>
		<td>-1.0D0</td>
		<td>Resistivity of a TF coil bus (Ohm-m). Default value takes the same res as the leg one</td>
	</tr>
	<tr>
		<td>rhotfleg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Resistivity of a TF coil leg (Ohm-m)</td>
	</tr>
	<tr>
		<td>ripmax</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>aximum allowable toroidal field ripple amplitude at plasma edge (%)</td>
	</tr>
	<tr>
		<td>ripple</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak/average toroidal field ripple at plasma edge (%)</td>
	</tr>
	<tr>
		<td>ritfc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total (summed) current in TF coils (A)</td>
	</tr>
	<tr>
		<td>sig_tf_r</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF Inboard leg radial stress in steel r distribution at mid-plane [Pa]</td>
	</tr>
	<tr>
		<td>sig_tf_t</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF Inboard leg tangential stress in steel r distribution at mid-plane [Pa]</td>
	</tr>
	<tr>
		<td>sig_tf_tresca</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF Inboard leg TRESCA stress in steel r distribution at mid-plane [Pa]</td>
	</tr>
	<tr>
		<td>sig_tf_vmises</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF Inboard leg Von-Mises stress in steel r distribution at mid-plane [Pa]</td>
	</tr>
	<tr>
		<td>sig_tf_z</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF Inboard leg vertical tensile stress in steel at mid-plane [Pa]</td>
	</tr>
	<tr>
		<td>sigvvall</td>
		<td>real</td>
		<td>9.3D7</td>
		<td>allowable stress from TF quench in vacuum vessel (Pa)</td>
	</tr>
	<tr>
		<td>strncon_cs</td>
		<td>real</td>
		<td>-0.005D0</td>
		<td>strain in CS superconductor material (used in Nb3Sn critical surface model isumatoh=1,4,5)</td>
	</tr>
	<tr>
		<td>strncon_pf</td>
		<td>real</td>
		<td>-0.005D0</td>
		<td>strain in PF superconductor material (used in Nb3Sn critical surface model isumatph=1,4,5)</td>
	</tr>
	<tr>
		<td>strncon_tf</td>
		<td>real</td>
		<td>-0.005D0</td>
		<td>strain in TF superconductor material (used in Nb3Sn critical surface model i_tf_sc_mat=1,4,5)</td>
	</tr>
	<tr>
		<td>strtf0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Maximum TRESCA stress in CS structures at CS flux swing [Pa]:<br><ul><li> If superconducting CS (ipfres = 0): turn steel conduits TRESCA stress</li><li> If resistive       CS (ipfres = 1): copper conductor TRESCA stress </li></ul><br>Quantity only computed for bucked and wedged design (i_tf_bucking &gt;= 2)<br> Def : CS Flux swing, instant when the current changes sign in CS (null current) </td>
	</tr>
	<tr>
		<td>strtf1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Maximum TRESCA stress in TF casing steel structures (Pa)</td>
	</tr>
	<tr>
		<td>strtf2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Maximum TRESCA stress in TF WP conduit steel structures (Pa)</td>
	</tr>
	<tr>
		<td>t_conductor</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Conductor (cable + steel conduit) area averaged dimension [m]</td>
	</tr>
	<tr>
		<td>t_crit_nbti</td>
		<td>real</td>
		<td>9.04D0</td>
		<td>critical temperature of GL_nbti</td>
	</tr>
	<tr>
		<td>t_turn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>WP turn squared dimensions [m]</td>
	</tr>
	<tr>
		<td>taucq</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>allowable TF quench time (s)</td>
	</tr>
	<tr>
		<td>tcoolin</td>
		<td>real</td>
		<td>313.15D0</td>
		<td>centrepost coolant inlet temperature (K)</td>
	</tr>
	<tr>
		<td>tcpav</td>
		<td>real</td>
		<td>373.15D0</td>
		<td>Average temperature of centrepost called CP (K). Only used for resistive coils <br> to compute the resisitive heating. Must be an iteration variable for <br> ST (itart=1) (iteration variable 20)</td>
	</tr>
	<tr>
		<td>tcpav2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Computed centrepost average temperature (K) (for consistency)</td>
	</tr>
	<tr>
		<td>tcpmax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>peak centrepost temperature (K)</td>
	</tr>
	<tr>
		<td>tcritsc</td>
		<td>real</td>
		<td>16.0D0</td>
		<td>critical temperature (K) for superconductor at zero field and strain (i_tf_sc_mat=4, =tc0m)</td>
	</tr>
	<tr>
		<td>tdmptf</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>fast discharge time for TF coil in event of quench (s) (iteration variable 56)<br>For REBCO model, meaning depends on quench_model:<br><ul><li> exponential quench : e-folding time (s)`</li><li> linear quench : discharge time (s)</li></ul></td>
	</tr>
	<tr>
		<td>temp_margin</td>
		<td>real</td>
		<td>0.00D0</td>
		<td>temperature margin (K)</td>
	</tr>
	<tr>
		<td>tfa</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>Horizontal radius of inside edge of TF coil (m)</td>
	</tr>
	<tr>
		<td>tfareain</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Area of inboard midplane TF legs (m2)</td>
	</tr>
	<tr>
		<td>tfb</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>Vertical radius of inside edge of TF coil (m)</td>
	</tr>
	<tr>
		<td>tfbusl</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil bus length (m)</td>
	</tr>
	<tr>
		<td>tfbusmas</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil bus mass (kg)</td>
	</tr>
	<tr>
		<td>tfc_sidewall_is_fraction</td>
		<td>logical</td>
		<td>None</td>
		<td>logical switch to make casths a fraction of TF coil thickness (casths_fraction)</td>
	</tr>
	<tr>
		<td>tfckw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>available DC power for charging the TF coils (kW)</td>
	</tr>
	<tr>
		<td>tfcmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Peak power per TF power supply (MW)</td>
	</tr>
	<tr>
		<td>tfcpmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Peak resistive TF coil inboard leg power (MW)</td>
	</tr>
	<tr>
		<td>tfcryoarea</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>surface area of toroidal shells covering TF coils (m2)</td>
	</tr>
	<tr>
		<td>tficrn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil half-width - inner bore (m)</td>
	</tr>
	<tr>
		<td>tfind</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil inductance (H)</td>
	</tr>
	<tr>
		<td>tfinsgap</td>
		<td>real</td>
		<td>0.010D0</td>
		<td>TF coil WP insertion gap (m)</td>
	</tr>
	<tr>
		<td>tfjtsmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF joints resistive power losses (MW)</td>
	</tr>
	<tr>
		<td>tflegmw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil outboard leg resistive power (MW)</td>
	</tr>
	<tr>
		<td>tflegres</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil leg resistance (ohm)</td>
	</tr>
	<tr>
		<td>tfleng</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil circumference (m)</td>
	</tr>
	<tr>
		<td>tfocrn</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil half-width - outer bore (m)</td>
	</tr>
	<tr>
		<td>tfsai</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>area of the inboard TF coil legs (m2)</td>
	</tr>
	<tr>
		<td>tfsao</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>area of the outboard TF coil legs (m2)</td>
	</tr>
	<tr>
		<td>tftmp</td>
		<td>real</td>
		<td>4.5D0</td>
		<td>peak helium coolant temperature in TF coils and PF coils (K)</td>
	</tr>
	<tr>
		<td>tftort</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>TF coil toroidal thickness (m)</td>
	</tr>
	<tr>
		<td>th_joint_contact</td>
		<td>real</td>
		<td>0.03D0</td>
		<td>TF sliding joints contact pad width [m]</td>
	</tr>
	<tr>
		<td>thicndut</td>
		<td>real</td>
		<td>8.0D-4</td>
		<td>conduit insulation thickness (m)</td>
	</tr>
	<tr>
		<td>thkcas</td>
		<td>real</td>
		<td>0.3D0</td>
		<td>inboard TF coil case outer (non-plasma side) thickness (m) (iteration variable 57)<br> (calculated for stellarators)</td>
	</tr>
	<tr>
		<td>thwcndut</td>
		<td>real</td>
		<td>8.0D-3</td>
		<td>TF coil conduit case thickness (m) (iteration variable 58)</td>
	</tr>
	<tr>
		<td>time1</td>
		<td>real</td>
		<td>0D0</td>
		<td>Time at which TF quench is detected (s)</td>
	</tr>
	<tr>
		<td>tinstf</td>
		<td>real</td>
		<td>0.018D0</td>
		<td>Thickness of the ground insulation layer surrounding (m) <br><ul><li> Superconductor TF (i_tf_sup == 1) : The TF Winding packs</li><li> Resistive magnets (i_tf_sup /= 1) : The TF turns</li></ul><br>Rem : The default value includes allowance for 10 mm insertion gap.<br> Rem : Thickness calculated for stellarators.</td>
	</tr>
	<tr>
		<td>tlegav</td>
		<td>real</td>
		<td>-1.0D0</td>
		<td>Average temperature of the TF outboard legs [K]. If tlegav=-1.0, the ouboard <br> legs and CP temperatures are the same. Fixed for now, should use a contraints eq like tcpav </td>
	</tr>
	<tr>
		<td>tmargmin</td>
		<td>real</td>
		<td>0D0</td>
		<td>minimum allowable temperature margin : TFC AND CS (K)</td>
	</tr>
	<tr>
		<td>tmargmin_cs</td>
		<td>real</td>
		<td>0D0</td>
		<td>minimum allowable temperature margin : CS (K)</td>
	</tr>
	<tr>
		<td>tmargmin_tf</td>
		<td>real</td>
		<td>0D0</td>
		<td>minimum allowable temperature margin : TF coils (K)</td>
	</tr>
	<tr>
		<td>tmargtf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil temperature margin (K)</td>
	</tr>
	<tr>
		<td>tmax_croco</td>
		<td>real</td>
		<td>200.0D0</td>
		<td>CroCo strand: maximum permitted temp during a quench (K)</td>
	</tr>
	<tr>
		<td>tmaxpro</td>
		<td>real</td>
		<td>150.0D0</td>
		<td>maximum temp rise during a quench for protection (K)</td>
	</tr>
	<tr>
		<td>tmpcry</td>
		<td>real</td>
		<td>4.5D0</td>
		<td>coil temperature for cryogenic plant power calculation (K)</td>
	</tr>
	<tr>
		<td>toroidalgap</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Minimal distance between two toroidal coils. (m)</td>
	</tr>
	<tr>
		<td>vcool</td>
		<td>real</td>
		<td>20.0D0</td>
		<td>inlet centrepost coolant flow speed at midplane (m/s) (iteration variable 70)</td>
	</tr>
	<tr>
		<td>vdalw</td>
		<td>real</td>
		<td>20.0D0</td>
		<td>max voltage across TF coil during quench (kV) (iteration variable 52)</td>
	</tr>
	<tr>
		<td>vforce</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vertical tension on inboard leg/coil (N)</td>
	</tr>
	<tr>
		<td>vforce_outboard</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Vertical tension on outboard leg/coil (N)</td>
	</tr>
	<tr>
		<td>vftf</td>
		<td>real</td>
		<td>0.4D0</td>
		<td>coolant fraction of TFC 'cable' (i_tf_sup=1), or of TFC leg (i_tf_ssup=0)</td>
	</tr>
	<tr>
		<td>vol_cond_cp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Exact conductor volume in the centrepost (m3)</td>
	</tr>
	<tr>
		<td>voltfleg</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>volume of each TF coil outboard leg (m3)</td>
	</tr>
	<tr>
		<td>vtfkv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil voltage for resistive coil including bus (kV)</td>
	</tr>
	<tr>
		<td>vtfskv</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>voltage across a TF coil during quench (kV)</td>
	</tr>
	<tr>
		<td>whtcas</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass per coil of external case (kg)</td>
	</tr>
	<tr>
		<td>whtcon</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>TF coil conductor mass per coil (kg/coil).<br> For itart=1, coil is return limb plus centrepost/n_tf</td>
	</tr>
	<tr>
		<td>whtconal</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Aluminium mass in TF coil conductor (kg/coil).<br> For itart=1, coil is return limb plus centrepost/n_tf</td>
	</tr>
	<tr>
		<td>whtconcu</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>copper mass in TF coil conductor (kg/coil).<br> For itart=1, coil is return limb plus centrepost/n_tf</td>
	</tr>
	<tr>
		<td>whtconin</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>conduit insulation mass in TF coil conductor (kg/coil)</td>
	</tr>
	<tr>
		<td>whtconsc</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>superconductor mass in TF coil cable (kg/coil)</td>
	</tr>
	<tr>
		<td>whtconsh</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>steel conduit mass in TF coil conductor (kg/coil)</td>
	</tr>
	<tr>
		<td>whtcp</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of TF coil inboard legs (kg)</td>
	</tr>
	<tr>
		<td>whtgw</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of ground-wall insulation layer per coil (kg/coil)</td>
	</tr>
	<tr>
		<td>whttf</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>total mass of the TF coils (kg)</td>
	</tr>
	<tr>
		<td>whttflgs</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of the TF coil legs (kg)</td>
	</tr>
	<tr>
		<td>windstrain</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>longitudinal strain in winding pack</td>
	</tr>
	<tr>
		<td>wwp1</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>width of first step of winding pack (m)</td>
	</tr>
	<tr>
		<td>wwp2</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>width of second step of winding pack (m)</td>
	</tr>
	<tr>
		<td>xarc</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>x location of arc point i on surface (m)</td>
	</tr>
	<tr>
		<td>xctfc</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>x location of arc centre i (m)</td>
	</tr>
	<tr>
		<td>yarc</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>y location of arc point i on surface (m)</td>
	</tr>
	<tr>
		<td>yctfc</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>y location of arc centre i (m)</td>
	</tr>
</table>

## times_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>intervallabel</td>
		<td>character</td>
		<td>(/'tramp', 'tohs ', 'theat', '...</td>
		<td>time intervals - as strings (s)</td>
	</tr>
	<tr>
		<td>pulsetimings</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>Switch for pulse timings (if lpulse=1):<br><ul><li> =0, tohs = Ip(MA)/0.1 tramp, tqnch = input</li><li> =1, tohs = iteration var or input. tramp/tqnch max of input or tohs</li></ul></td>
	</tr>
	<tr>
		<td>tburn</td>
		<td>real</td>
		<td>1000.0D0</td>
		<td>burn time (s) (calculated if lpulse=1)</td>
	</tr>
	<tr>
		<td>tburn0</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>burn time (s) - used for internal consistency</td>
	</tr>
	<tr>
		<td>tcycle</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>full cycle time (s)</td>
	</tr>
	<tr>
		<td>tdown</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>down time (s)</td>
	</tr>
	<tr>
		<td>tdwell</td>
		<td>real</td>
		<td>1800.0D0</td>
		<td>time between pulses in a pulsed reactor (s) (iteration variable 17)</td>
	</tr>
	<tr>
		<td>theat</td>
		<td>real</td>
		<td>10.0D0</td>
		<td>heating time, after current ramp up (s)</td>
	</tr>
	<tr>
		<td>tim</td>
		<td>real</td>
		<td>['0.0D0', '0.0D0', '0.0D0', '0...</td>
		<td>array of time points during plasma pulse (s)</td>
	</tr>
	<tr>
		<td>timelabel</td>
		<td>character</td>
		<td>(/'Start', 'BOP', 'EOR', 'BOF'...</td>
		<td>array of time labels during plasma pulse (s)</td>
	</tr>
	<tr>
		<td>tohs</td>
		<td>real</td>
		<td>30.0D0</td>
		<td>plasma current ramp-up time for current initiation (s) (calculated if lpulse=0)<br> (iteration variable 65)</td>
	</tr>
	<tr>
		<td>tohsin</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>Switch for plasma current ramp-up time (if lpulse=0):<br><ul><li> = 0, tohs = tramp = tqnch = Ip(MA)/0.5</li><li> &lt;&gt;0, tohs = tohsin; tramp, tqnch are input</li></ul></td>
	</tr>
	<tr>
		<td>tpulse</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>pulse length = tohs + theat + tburn + tqnch</td>
	</tr>
	<tr>
		<td>tqnch</td>
		<td>real</td>
		<td>15.0D0</td>
		<td>shut down time for PF coils (s); if pulsed, = tohs</td>
	</tr>
	<tr>
		<td>tramp</td>
		<td>real</td>
		<td>15.0D0</td>
		<td>initial PF coil charge time (s); if pulsed, = tohs</td>
	</tr>
</table>

## vacuum_variables
<table class="vardes">
	<tr>
		<th class="col1">Name</th>
		<th class="col2">Type</th>
		<th class="col3">Initial</th>
		<th class="col4">Description</th>
	</tr>
	<tr>
		<td>dlscal</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>vacuum system duct length scaling</td>
	</tr>
	<tr>
		<td>dwell_pump</td>
		<td>integer</td>
		<td>0</td>
		<td>switch for dwell pumping options:<br><ul><li> =0 pumping only during tdwell</li><li> =1 pumping only during tramp</li><li> =2 pumping during tdwell + tramp</li></ul></td>
	</tr>
	<tr>
		<td>initialpressure</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>initial neutral pressure at the beginning of the dwell phase (Pa)</td>
	</tr>
	<tr>
		<td>niterpump</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>number of high vacuum pumps (real number), each with the throughput of one <br> ITER cryopump (50 Pa m3 s-1), all operating at the same time (vacuum_model='simple')</td>
	</tr>
	<tr>
		<td>ntype</td>
		<td>integer</td>
		<td>1</td>
		<td>switch for vacuum pump type:<br><ul><li> =0 - for turbomolecular pump (magnetic bearing) with speed of 2.0 m3/s<br>   (1.95 for N2, 1.8 for He, 1.8 for DT)</li><li> =1 - for compound cryopump with nominal speed of 10.0 m3/s<br>   (9.0 for N2, 5.0 for He and 25.0 for DT)</li></ul></td>
	</tr>
	<tr>
		<td>nvduct</td>
		<td>integer</td>
		<td>0</td>
		<td>number of ducts (torus to pumps)</td>
	</tr>
	<tr>
		<td>outgasfactor</td>
		<td>real</td>
		<td>0.0235D0</td>
		<td>outgassing prefactor kw: outgassing rate at 1 s per unit area (Pa m s-1)</td>
	</tr>
	<tr>
		<td>outgasindex</td>
		<td>real</td>
		<td>1.0D0</td>
		<td>outgassing decay index</td>
	</tr>
	<tr>
		<td>pbase</td>
		<td>real</td>
		<td>5.0D-4</td>
		<td>base pressure during dwell before gas pre-fill(Pa)</td>
	</tr>
	<tr>
		<td>prdiv</td>
		<td>real</td>
		<td>0.36D0</td>
		<td>divertor chamber pressure during burn (Pa)</td>
	</tr>
	<tr>
		<td>pumpareafraction</td>
		<td>real</td>
		<td>0.0203D0</td>
		<td>area of one pumping port as a fraction of plasma surface area</td>
	</tr>
	<tr>
		<td>pumpspeedfactor</td>
		<td>real</td>
		<td>0.167D0</td>
		<td>effective pumping speed reduction factor due to duct impedance</td>
	</tr>
	<tr>
		<td>pumpspeedmax</td>
		<td>real</td>
		<td>27.3D0</td>
		<td>maximum pumping speed per unit area for deuterium &amp; tritium, molecular flow</td>
	</tr>
	<tr>
		<td>pumptp</td>
		<td>real</td>
		<td>1.2155D22</td>
		<td>Pump throughput (molecules/s) (default is ITER value)</td>
	</tr>
	<tr>
		<td>rat</td>
		<td>real</td>
		<td>1.3D-8</td>
		<td>plasma chamber wall outgassing rate (Pa-m/s)</td>
	</tr>
	<tr>
		<td>tn</td>
		<td>real</td>
		<td>300.0D0</td>
		<td>neutral gas temperature in chamber (K)</td>
	</tr>
	<tr>
		<td>vacdshm</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>mass of vacuum duct shield (kg)</td>
	</tr>
	<tr>
		<td>vacuum_model</td>
		<td>character</td>
		<td>'old'</td>
		<td>switch for vacuum pumping model:<br><ul><li> ='old' for old detailed ETR model</li><li> ='simple' for simple steady-state model with comparison to ITER cryopumps</li></ul></td>
	</tr>
	<tr>
		<td>vcdimax</td>
		<td>real</td>
		<td>0.0D0</td>
		<td>diameter of duct passage (m)</td>
	</tr>
	<tr>
		<td>vpumpn</td>
		<td>integer</td>
		<td>0</td>
		<td>number of high vacuum pumps</td>
	</tr>
</table>

