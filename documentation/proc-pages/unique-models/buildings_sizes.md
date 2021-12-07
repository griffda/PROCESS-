# Power Plant Buildings Sizes

<p style='text-align: justify;'>
The routine documented here estimates the sizes (footprints and volumes) of buildings within a fusion power plant. Some estimates are scaled with parameters of the fusion plant, some are based on engineering/specialist assumptions, some are derived from footprints/volumes based on assessment of other power plants and/or similar facilities. </p>


## Reactor building    

<p style='text-align: justify;'>
Calculation of the lateral size of the reactor building is driven by the radial width of the largest component, which is assumed to be one of a) PF coil maximum radius, b) cryostat radius, c) TF coil outer radius. 
Space is included for biological shielding around the reactor. </p>

<p style='text-align: justify;'>
The 'key-width' used in estimating the reactor building size is defined to include the radial width of the largest component twice (to allow space for manoeuvring components during construction), a set clearance distance around the reactor, a transportation clearance between components, and a clearance to the building wall for crane operation. This key-width is then used to estimate the length and width of the reactor building, allowing for the necessity of laying down the largest component during construction. </p>

<p style='text-align: justify;'>
The estimate of the vertical clearance required around the reactor include the set reactor clearance distance, a transportation clearance between components, clearance from the TF coil to the cryostat, clearance beneath the TF coil, a clearance to the ceiling for crane operation, and a crane arm height. The height of the reactor building is estimated on the assumption that the TF coil is the tallest single component and that twice this height is required to allow for construction/maintenance access, and this is added to the required vertical clearance. </p>

<p style='text-align: justify;'>
The external footprint of the reactor building includes a set reactor wall thickness, as is appropriate for nuclear buildings. 
    
### Heating and Current Drive facility

<p style='text-align: justify;'>
In the case of an NBI current drive, this facility will be housed within the main reactor building and the length and width of the reactor building is extended accordingly. In the case of EC or EBW heating a separate building is sized. </p>

### Fuel Cycle facilities
<p style='text-align: justify;'>
This facility will be housed within the main reactor building and the length and width of the reactor building is extended accordingly. </p>

### Reactor maintenance basement and tunnel
<p style='text-align: justify;'>
The architecture used in this estimation is that of a basement directly beneath the reactor, enabling the downwards extraction of hot components. The footprint estimated here is oversized to include allowance for a tunnel to the hot cell storage/maintenance building. The height of the basement also includes vertical clearances for crane operation. </p>


## Hot Cell Facility
<p style='text-align: justify;'>
This building provides hot cell facilities to maintain or dismantle highly radioactive components. These estimates assume all in-vessel components used through the life of the plant will need storage. The storage area required is derived from the sizes and number of components, allowing for a margin in component numbers as set by the quantity safety factor. Footprints and volumes required for storage include a set hot separation distance. The estimation of this facility size include:</p>

- The tokomak is toroidally segmented based on number of TF coils
- Each component will be stored with the largest dimension oriented horizontally
- Component height is the largest dimension 

<p style='text-align: justify;'>
The components included within this estimation are:</p>

- The inboard shield, blanket, and first wall
- The outboard first wall, blanket, and shield
- The divertor segments
- The tokamak centre post

<p style='text-align: justify;'>
The storage volume required for each component is calculated from its vertical, radial and toroidal dimensions, including allowance for a hot separation distance around each component. The number of components required is calculated from the number in the machine build, the estimated component lifetime (in full power years), and the estimated power plant lifetime. This number is multiplied by the specified quantity safety factor and a total required storage volume can therefore be estimated for each component type. The total required storage volume is a simple addition of the component volumes; the building height is based upon expert estimates, and hence a footprint is calculated (and, on the assumption of a square building, length, width and a footprint can be estimated). The external footprint of this facility includes a wall thickness appropriate for nuclear buildings.</p>

<p style='text-align: justify;'>
The user is asked to note that this estimation is based on many suppositions, and its applicability to any one reactor design may vary widely. </p>


## Reactor Auxiliary buildings

<p style='text-align: justify;'>
This category of building includes auxiliary buildings supporting reactor processes and systems, such as:</p>

- Chemistry labs, including RA, non-RA and environmental labs, and chemical treatment facilities for coolant circuits
- Heat sink facilities, includes auxiliary heat sink at heat energy island, low temperature and emergency heat sink facilities, ultimate heat sink facility to sea/river/cooling towers, including pumping, chemical dosing and heat exchangers
- Non-RA interfacing services such as, hydraulics, compressed air, chilled water, etc.
 
<p style='text-align: justify;'>
These estimates amalgamate multiple individual buildings.</p>


## Magnet Power facilities
<p style='text-align: justify;'>
This category of building provides specific electrical supplies for reactor magnets; estimates are based upon dimensions of comparable equipment at the ITER site, and include steady state power trains and pulsed power for the central solenoid. </p>


## Control buildings
<p style='text-align: justify;'>
This category of building includes a Main Control Room, Back-up Control Room, Signal Processing and Distribution Centres [Safety Train A, Safety Train B], HP offices & Data Logging centre, and a Data Storage centre. The estimate amalgamates multiple individual buildings.</p>


## Warm Shop 
<p style='text-align: justify;'>
This facility includes 'hands on' maintenance workshops for low RA dose equipment. </p>


## Maintenance buildings
<p style='text-align: justify;'>
This category of building includes:</p>

- Maintenance workshops and clean rooms for components with no radiation inventory.
- Robot construction, testing, mock-up facilities, to allow robots to be fully assembled, commissioned and tested in mock-ups of the real environment. Building also houses offices and classrooms. 
- Maintenance control and inspection facilities: includes operations centre, inbound inspection and QA storage facilities.


## Cryogenic and cooling facilities
<p style='text-align: justify;'>
These facilities include:</p>

- Cryogenic buildings for magnet and fuel cycle
- Magnet cryogenic storage tanks
- Site-wide auxiliary water cooling, including pumping, chemical dosing, filtration and heat exchangers.


## Electrical facilities 
<p style='text-align: justify;'>
These facilities include:</p>

- Transformers and electrical distribution; includes main step-down and step-up transformers and substation, reactive power buildings
- Load centres (essential and non-essential supplies)
- Energy storage systems (batteries & flywheels) and back-up generators

<p style='text-align: justify;'>
This estimate amalgamates multiple individual buildings.</p>


## Turbine Hall
<p style='text-align: justify;'>
A review of 18 existing fission power plants found that turbine hall size is largely independent of plant output power. The default footprint used here represents a weighted mean of those plants and a fusion-appropriate design of a Steam Rankine cycle turbine building. </p>


## Waste buildings
<p style='text-align: justify;'>
These estimates amalgamate multiple individual buildings:</p>

- Intermediate Level Waste (ILW): Radioactive waste melt, separation and size reduction facility; ILW process and storage.
- Low Level Waste (LLW) process and storage.
- Hazardous Waste process and storage.
- Tritiated Waste Store.


## Site Services    
<p style='text-align: justify;'>
These estimates amalgamate multiple individual buildings, grouped by function:</p>

### Air & Gas supplies
<p style='text-align: justify;'>
Includes compressed air facility, common gas systems facility, bottled gas storage compounds; these values amalgamate multiple individual buildings. </p>

### Water, Laundry & Drainage
<p style='text-align: justify;'>
Includes facilities for potable water, firewater, chilled water; PPE laundry and Respiratory Protective Equipment cleaning; industrial drains & sewage process and discharge; these values amalgamate multiple individual buildings. </p>

### Site Security & Safety
<p style='text-align: justify;'> Includes Security Control Centre and Fire and Ambulance Garages; these values amalgamate multiple individual buildings. </p>


## Staff Services
<p style='text-align: justify;'>
Amalgamating multiple individual buildings (and using an average height), this estimate includes office buildings, contractor offices, staff restaurant and cafe, staff induction and training facilities, main gate and reception, access control and site pass office, occupational health centre.


## Summary variables

- Effective floor area for AC power module, <em>efloor</em>: sum of all building footprints
- Total volume of nuclear buildings, <em>volnucb</em>: sum of all nuclear building (external) volumes
