
"""
This file contains dictionaries for use by the PROCESS GUI.
GUI_MODULE      : Sets the ordering of main body variable
GUI_LABLXC      : Sets the ordering of iteration variables
GUI_LABLCC      : Sets the ordering of constraint equations

"""

from collections import OrderedDict
    

#List that sets order of main body variables
GUI_MODULE = OrderedDict()
GUI_MODULE['Build'] = [
	 ('float', 'aplasmin', 'Minimum minor radius (m)', 'Minimum minor radius (m)') ,
	 ('float', 'blbmith', 'Inboard blanket box manifold thickness (m)', 'Inboard blanket box manifold thickness (m)\n(blktmodel>0)') ,
	 ('float', 'blbmoth', 'Outboard blanket box manifold thickness (m)', 'Outboard blanket box manifold thickness (m)\n(blktmodel>0)') ,
	 ('float', 'blbpith', 'Inboard blanket base plate thickness (m)', 'Inboard blanket base plate thickness (m)\n(blktmodel>0)') ,
	 ('float', 'blbpoth', 'Outboard blanket base plate thickness (m)', 'Outboard blanket base plate thickness (m)\n(blktmodel>0)') ,
	 ('float', 'blbuith', 'Inboard blanket breeding zone thickness (m)', 'Inboard blanket breeding zone thickness (m)\n(blktmodel>0)\n(iteration variable 90)') ,
	 ('float', 'blbuoth', 'Outboard blanket breeding zone thickness (m)', 'Outboard blanket breeding zone thickness (m)\n(blktmodel>0)\n(iteration variable 91)') ,
	 ('float', 'blnkith', 'Inboard blanket thickness (m);', 'Inboard blanket thickness (m);\ncalculated if blktmodel > 0') ,
	 ('float', 'blnkoth', 'Outboard blanket thickness (m);', 'Outboard blanket thickness (m);\ncalculated if blktmodel > 0') ,
	 ('float', 'bore', 'Central solenoid inboard radius (m)', 'Central solenoid inboard radius (m)\n(iteration variable 29)') ,
	 ('float', 'clhsf', 'Cryostat lid height scaling factor (tokamaks, rfps)', 'Cryostat lid height scaling factor (tokamaks, rfps)') ,
	 ('float', 'ddwex', 'External cryostat thickness (m)', 'External cryostat thickness (m)') ,
	 ('float', 'ddwi', 'Vacuum vessel thickness (tf coil / shield) (m)', 'Vacuum vessel thickness (tf coil / shield) (m)') ,
	 ('float', 'fmsbc', 'Martensitic fraction of steel in (non-existent!) bucking cylinder', 'Martensitic fraction of steel in (non-existent!) bucking cylinder') ,
	 ('float', 'fmsbl', 'Martensitic fraction of steel in blanket', 'Martensitic fraction of steel in blanket') ,
	 ('float', 'fmsdwe', 'Martensitic fraction of steel in external cryostat', 'Martensitic fraction of steel in external cryostat') ,
	 ('float', 'fmsdwi', 'Martensitic fraction of steel in vacuum vessel', 'Martensitic fraction of steel in vacuum vessel') ,
	 ('float', 'fmsfw', 'Martensitic fraction of steel in first wall', 'Martensitic fraction of steel in first wall') ,
	 ('float', 'fmsoh', 'Martensitic fraction of steel in central solenoid', 'Martensitic fraction of steel in central solenoid') ,
	 ('float', 'fmssh', 'Martensitic fraction of steel in shield', 'Martensitic fraction of steel in shield') ,
	 ('float', 'fmstf', 'Martensitic fraction of steel in tf coil', 'Martensitic fraction of steel in tf coil') ,
	 ('float', 'fwith', 'Inboard first wall thickness (m) (if lpulse=1, =2*bfw)', 'Inboard first wall thickness (m) (if lpulse=1, =2*bfw)') ,
	 ('float', 'fwoth', 'Outboard first wall thickness (m) (if lpulse=1, =2*bfw)', 'Outboard first wall thickness (m) (if lpulse=1, =2*bfw)') ,
	 ('float', 'gapds', 'Gap between inboard vacuum vessel and tf coil (m)', 'Gap between inboard vacuum vessel and tf coil (m)\n(iteration variable 61)') ,
	 ('float', 'gapoh', 'Gap between central solenoid and tf coil', 'Gap between central solenoid and tf coil\n(iteration variable 42)') ,
	 ('float', 'gapomin', 'Minimum gap between outboard vacuum vessel and tf coil (m)', 'Minimum gap between outboard vacuum vessel and tf coil (m)\n(iteration variable 31)') ,
	 ('int', 'iohcl', 'Switch for existence of central solenoid:', 'Switch for existence of central solenoid:\n= 0 central solenoid not present;\n= 1 central solenoid exists') ,
	 ('float', 'ohcth', 'Central solenoid thickness (m)', 'Central solenoid thickness (m)\n(iteration variable 16)') ,
	 ('float', 'rinboard', 'Plasma inboard radius (m)', 'Plasma inboard radius (m)\n(consistency equation 29)') ,
	 ('float', 'scrapli', 'Gap between plasma and first wall, inboard side (m)', 'Gap between plasma and first wall, inboard side (m)\n(used if iscrp=1) (iteration variable 73)') ,
	 ('float', 'scraplo', 'Gap between plasma and first wall, outboard side (m)', 'Gap between plasma and first wall, outboard side (m)\n(used if iscrp=1) (iteration variable 74)') ,
	 ('float', 'shldith', 'Inboard shield thickness (m)', 'Inboard shield thickness (m)\n(iteration variable 93)') ,
	 ('float', 'shldoth', 'Outboard shield thickness (m)', 'Outboard shield thickness (m)\n(iteration variable 94)') ,
	 ('float', 'shldtth', 'Upper/lower shield thickness (m);', 'Upper/lower shield thickness (m);\ncalculated if blktmodel > 0') ,
	 ('float', 'tfcth', 'Inboard tf coil thickness, (centrepost for st) (m)', 'Inboard tf coil thickness, (centrepost for st) (m)\n(calculated for stellarators)\n(iteration variable 13)') ,
	 ('float', 'tfootfi', 'Tf coil outboard leg / inboard leg radial thickness', 'Tf coil outboard leg / inboard leg radial thickness\nratio (itfsup=0 only)\n(iteration variable 75)') ,
	 ('float', 'vgap2', 'Vertical gap between vacuum vessel and tf coil (m)', 'Vertical gap between vacuum vessel and tf coil (m)') ,
	 ('float', 'vgaptf', 'Vertical gap between x-point and divertor (m)', 'Vertical gap between x-point and divertor (m)\n(if = 0, it is calculated)') ,
	]
GUI_MODULE['Buildings'] = [
	 ('float', 'admv', 'Administration building volume (m3)', 'Administration building volume (m3)') ,
	 ('float', 'clh1', 'Vertical clearance from tf coil to cryostat (m)', 'Vertical clearance from tf coil to cryostat (m)\n(calculated for tokamaks, rfps)') ,
	 ('float', 'clh2', 'Clearance beneath tf coil to foundation', 'Clearance beneath tf coil to foundation\n(including basement) (m)') ,
	 ('float', 'conv', 'Control building volume (m3)', 'Control building volume (m3)') ,
	 ('float', 'esbldgm3', 'Volume of energy storage equipment building (m3)', 'Volume of energy storage equipment building (m3)\n(not used if lpulse=0)') ,
	 ('float', 'fndt', 'Foundation thickness (m)', 'Foundation thickness (m)') ,
	 ('float', 'hccl', 'Clearance around components in hot cell (m)', 'Clearance around components in hot cell (m)') ,
	 ('float', 'hcwt', 'Hot cell wall thickness (m)', 'Hot cell wall thickness (m)') ,
	 ('float', 'mbvfac', 'Maintenance building volume multiplication factor', 'Maintenance building volume multiplication factor') ,
	 ('float', 'pfbldgm3', 'Volume of pf coil power supply building (m3)', 'Volume of pf coil power supply building (m3)') ,
	 ('float', 'pibv', 'Power injection building volume (m3)', 'Power injection building volume (m3)') ,
	 ('float', 'rbrt', 'Reactor building roof thickness (m)', 'Reactor building roof thickness (m)') ,
	 ('float', 'rbvfac', 'Reactor building volume multiplication factor', 'Reactor building volume multiplication factor') ,
	 ('float', 'rbwt', 'Reactor building wall thickness (m)', 'Reactor building wall thickness (m)') ,
	 ('float', 'row', 'Clearance to building wall for crane operation (m)', 'Clearance to building wall for crane operation (m)') ,
	 ('float', 'rxcl', 'Clearance around reactor (m)', 'Clearance around reactor (m)') ,
	 ('float', 'shmf', 'Fraction of shield mass per tf coil', 'Fraction of shield mass per tf coil\nto be moved in the maximum shield lift') ,
	 ('float', 'shov', 'Shops and warehouse volume (m3)', 'Shops and warehouse volume (m3)') ,
	 ('float', 'stcl', 'Clearance above crane to roof (m)', 'Clearance above crane to roof (m)') ,
	 ('float', 'tfcbv', 'Volume of tf coil power supply building (m3)', 'Volume of tf coil power supply building (m3)\n(calculated if tf coils are superconducting)') ,
	 ('float', 'trcl', 'Transportation clearance between components (m)', 'Transportation clearance between components (m)') ,
	 ('float', 'triv', 'Volume of tritium, fuel handling and', 'Volume of tritium, fuel handling and\nhealth physics buildings (m3)') ,
	 ('float', 'wgt', 'Reactor building crane capacity (kg)', 'Reactor building crane capacity (kg)\n(calculated if 0 is input)') ,
	 ('float', 'wgt2', 'Hot cell crane capacity (kg)', 'Hot cell crane capacity (kg)\n(calculated if 0 is input)') ,
	 ('float', 'wsvfac', 'Warm shop building volume multiplication factor', 'Warm shop building volume multiplication factor') ,
	]
GUI_MODULE['Constraint'] = [
	 ('float', 'auxmin', 'Minimum auxiliary power (mw)', 'Minimum auxiliary power (mw)\n(constraint equation 40)') ,
	 ('float', 'betpmx', 'Maximum poloidal beta', 'Maximum poloidal beta\n(constraint equation 48)') ,
	 ('float', 'bigqmin', 'Minimum fusion gain q', 'Minimum fusion gain q\n(constraint equation 28)') ,
	 ('float', 'bmxlim', 'Maximum peak toroidal field (t)', 'Maximum peak toroidal field (t)\n(constraint equation 25)') ,
	 ('float', 'dtmpmx', 'Maximum first wall coolant temperature rise (k)', 'Maximum first wall coolant temperature rise (k)\n(constraint equation 38)') ,
	 ('float', 'fauxmn', 'F-value for minimum auxiliary power', 'F-value for minimum auxiliary power\n(constraint equation 40, iteration variable 64)') ,
	 ('float', 'fbeta', 'F-value for epsilon beta-poloidal', 'F-value for epsilon beta-poloidal\n(constraint equation 6, iteration variable 8)') ,
	 ('float', 'fbetap', 'F-value for poloidal beta', 'F-value for poloidal beta\n(constraint equation 48, iteration variable 79)') ,
	 ('float', 'fbetatry', 'F-value for beta limit', 'F-value for beta limit\n(constraint equation 24, iteration variable 36)') ,
	 ('float', 'fdene', 'F-value for density limit', 'F-value for density limit\n(constraint equation 5, iteration variable 9)') ,
	 ('float', 'fdivcol', 'F-value for divertor collisionality', 'F-value for divertor collisionality\n(constraint equation 22, iteration variable 34)') ,
	 ('float', 'fdtmp', 'F-value for first wall coolant temperature rise', 'F-value for first wall coolant temperature rise\n(constraint equation 38, iteration variable 62)') ,
	 ('float', 'fflutf', 'F-value for neutron fluence on tf coil', 'F-value for neutron fluence on tf coil\n(constraint equation 53, iteration variable 92)') ,
	 ('float', 'ffuspow', 'F-value for maximum fusion power', 'F-value for maximum fusion power\n(constraint equation 9, iteration variable 26)') ,
	 ('float', 'fgamcd', 'F-value for current drive gamma', 'F-value for current drive gamma\n(constraint equation 37, iteration variable 40)') ,
	 ('float', 'fhldiv', 'F-value for divertor heat load', 'F-value for divertor heat load\n(constraint equation 18, iteration variable 27)') ,
	 ('float', 'fiooic', 'F-value for tf coil operating current / critical', 'F-value for tf coil operating current / critical\ncurrent ratio\n(constraint equation 33, iteration variable 50)') ,
	 ('float', 'fipir', 'F-value for ip/irod limit', 'F-value for ip/irod limit\n(constraint equation 46, iteration variable 72)') ,
	 ('float', 'fjohc', 'F-value for central solenoid current at end-of-flattop', 'F-value for central solenoid current at end-of-flattop\n(constraint equation 26, iteration variable 38)') ,
	 ('float', 'fjohc0', 'F-value for central solenoid current at beginning of pulse', 'F-value for central solenoid current at beginning of pulse\n(constraint equation 27, iteration variable 39)') ,
	 ('float', 'fjprot', 'F-value for tf coil winding pack current density', 'F-value for tf coil winding pack current density\n(constraint equation 35, iteration variable 53)') ,
	 ('float', 'fmva', 'F-value for maximum mva', 'F-value for maximum mva\n(constraint equation 19, iteration variable 30)') ,
	 ('float', 'fpeakb', 'F-value for maximum toroidal field', 'F-value for maximum toroidal field\n(constraint equation 25, iteration variable 35)') ,
	 ('float', 'fpinj', 'F-value for injection power', 'F-value for injection power\n(constraint equation 30, iteration variable 46)') ,
	 ('float', 'fpnetel', 'F value for net electric power', 'F value for net electric power\n(constraint equation 16, iteration variable 25)') ,
	 ('float', 'fportsz', 'F-value for neutral beam tangency radius limit', 'F-value for neutral beam tangency radius limit\n(constraint equation 20, iteration variable 33)') ,
	 ('float', 'fpsepr', 'F-value for maximum psep/r limit', 'F-value for maximum psep/r limit\n(constraint equation 56, iteration variable 97)') ,
	 ('float', 'fptemp', 'F-value for peak centrepost temperature', 'F-value for peak centrepost temperature\n(constraint equation 44, iteration variable 68)') ,
	 ('float', 'fptfnuc', 'F value for maximum tf coil nuclear heating', 'F value for maximum tf coil nuclear heating\n(constraint equation 54, iteration variable 95)') ,
	 ('float', 'fq', 'F-value for edge safety factor', 'F-value for edge safety factor\n(constraint equation 45, iteration variable 71)') ,
	 ('float', 'fqval', 'F-value for q', 'F-value for q\n(constraint equation 28, iteration variable 45)') ,
	 ('float', 'fradpwr', 'F-value for core radiation power limit', 'F-value for core radiation power limit\n(constraint equation 17, iteration variable 28)') ,
	 ('float', 'frfpf', 'F-value for rfp reversal parameter', 'F-value for rfp reversal parameter\n(constraint equation 49, iteration variable 80)') ,
	 ('float', 'frfptf', 'F-value for rfp tf coil toroidal thickness', 'F-value for rfp tf coil toroidal thickness\n(constraint equation 47, iteration variable 76)') ,
	 ('float', 'frminor', 'F-value for minor radius limit', 'F-value for minor radius limit\n(constraint equation 21, iteration variable 32)') ,
	 ('float', 'fstrcase', 'F-value for tf coil case stress', 'F-value for tf coil case stress\n(constraint equation 31, iteration variable 48)') ,
	 ('float', 'fstrcond', 'F-value for tf coil conduit stress', 'F-value for tf coil conduit stress\n(constraint equation 32, iteration variable 49)') ,
	 ('float', 'ftbr', 'F-value for minimum tritium breeding ratio (blktmodel>0)', 'F-value for minimum tritium breeding ratio (blktmodel>0)\n(constraint equation 52, iteration variable 89)') ,
	 ('float', 'ftburn', 'F-value for minimum burn time', 'F-value for minimum burn time\n(constraint equation 13, iteration variable 21)') ,
	 ('float', 'ftcycl', 'F-value for cycle time', 'F-value for cycle time\n(constraint equation 42, iteration variable 67)') ,
	 ('float', 'ftfthko', 'F-value for tf coil outer leg radial thickness lower limit', 'F-value for tf coil outer leg radial thickness lower limit\n(constraint equation 58, iteration variable 100)') ,
	 ('float', 'ftftort', 'F-value for tf coil outer leg toroidal width lower limit', 'F-value for tf coil outer leg toroidal width lower limit\n(constraint equation 57, iteration variable 99)') ,
	 ('float', 'ftmargtf', 'F-value for tf coil temperature margin', 'F-value for tf coil temperature margin\n(constraint equation 36, iteration variable 54)') ,
	 ('float', 'ftohs', 'F-value for plasma current ramp-up time', 'F-value for plasma current ramp-up time\n(constraint equation 41, iteration variable 66)') ,
	 ('float', 'ftpeak', 'F-value for first wall peak temperature', 'F-value for first wall peak temperature\n(constraint equation 39, iteration variable 63)') ,
	 ('float', 'fvdump', 'F-value for dump voltage', 'F-value for dump voltage\n(constraint equation 34, iteration variable 51)') ,
	 ('float', 'fvs', 'F-value for flux-swing (v-s) requirement', 'F-value for flux-swing (v-s) requirement\n(constraint equation 12, iteration variable 15)') ,
	 ('float', 'fvvhe', 'F-value for vacuum vessel he concentration limit', 'F-value for vacuum vessel he concentration limit\n(blktmodel>0)\n(constraint equation 55, iteration variable 96)') ,
	 ('float', 'fwalld', 'F-value for minimum wall load', 'F-value for minimum wall load\n(constraint equation 8, iteration variable 14)') ,
	 ('float', 'gammax', 'Maximum current drive gamma', 'Maximum current drive gamma\n(constraint equation 37)') ,
	 ('float', 'mvalim', 'Maximum mva limit', 'Maximum mva limit\n(constraint equation 19)') ,
	 ('float', 'nflutfmax', 'Max fast neutron fluence on tf coil (n/m2)', 'Max fast neutron fluence on tf coil (n/m2)\n(blktmodel>0)\n(constraint equation 53)') ,
	 ('float', 'pnetelin', 'Required net electric power (mw)', 'Required net electric power (mw)\n(constraint equation 16)') ,
	 ('float', 'powfmax', 'Maximum fusion power (mw)', 'Maximum fusion power (mw)\n(constraint equation 9)') ,
	 ('float', 'pseprmax', 'Maximum ratio of power crossing the separatrix to', 'Maximum ratio of power crossing the separatrix to\nplasma major radius (psep/r) (mw/m)\n(constraint equation 56)') ,
	 ('float', 'ptfnucmax', 'Maximum nuclear heating in tf coil (mw/m3)', 'Maximum nuclear heating in tf coil (mw/m3)\n(constraint equation 54)') ,
	 ('float', 'tbrmin', 'Minimum tritium breeding ratio (blktmodel>0)', 'Minimum tritium breeding ratio (blktmodel>0)\n(constraint equation 52)') ,
	 ('float', 'tbrnmn', 'Minimum burn time (s)', 'Minimum burn time (s)\n(constraint equation 13)') ,
	 ('float', 'tpkmax', 'Maximum first wall peak temperature (c)', 'Maximum first wall peak temperature (c)\n(constraint equation 39)') ,
	 ('float', 'vvhealw', 'Allowed maximum helium concentration in vacuum vessel', 'Allowed maximum helium concentration in vacuum vessel\nat end of plant life (appm) (blktmodel>0)\n(constraint equation 55)') ,
	 ('float', 'walalw', 'Allowable wall-load (mw/m2)', 'Allowable wall-load (mw/m2)\n(constraint equation 8)') ,
	]
GUI_MODULE['Cost'] = [
	 ('float', 'abktflnc', 'Allowable first wall/blanket neutron', 'Allowable first wall/blanket neutron\nfluence (mw-yr/m2) (blktmodel=0)') ,
	 ('float', 'adivflnc', 'Allowable divertor heat fluence (mw-yr/m2)', 'Allowable divertor heat fluence (mw-yr/m2)') ,
	 ('float', 'cconfix', 'Fixed cost of superconducting cable ($/m)', 'Fixed cost of superconducting cable ($/m)') ,
	 ('float', 'cconshpf', 'Cost of pf coil steel conduit/sheath ($/m)', 'Cost of pf coil steel conduit/sheath ($/m)') ,
	 ('float', 'cconshtf', 'Cost of tf coil steel conduit/sheath ($/m)', 'Cost of tf coil steel conduit/sheath ($/m)') ,
	 ('float', 'cfactr', 'Plant capacity factor, availability;', 'Plant capacity factor, availability;\ninput if iavail = 0') ,
	 ('array', 'cfind', 'Indirect cost factor (func of lsa)', 'Indirect cost factor (func of lsa)') ,
	 ('float', 'cland', 'Cost of land (m$)', 'Cost of land (m$)') ,
	 ('float', 'cowner', 'Owner cost factor', 'Owner cost factor') ,
	 ('float', 'cpstflnc', 'Allowable st centrepost neutron fluence (mw-yr/m2)', 'Allowable st centrepost neutron fluence (mw-yr/m2)') ,
	 ('float', 'csi', 'Allowance for site costs (m$)', 'Allowance for site costs (m$)') ,
	 ('float', 'cturbb', 'Cost of turbine building (m$)', 'Cost of turbine building (m$)') ,
	 ('float', 'decomf', 'Proportion of constructed cost required for', 'Proportion of constructed cost required for\ndecommissioning fund') ,
	 ('float', 'dintrt', 'Diff between borrowing and saving interest rates', 'Diff between borrowing and saving interest rates') ,
	 ('float', 'dtlife', 'Period prior to the end of the plant life that', 'Period prior to the end of the plant life that\nthe decommissioning fund is used (years)') ,
	 ('float', 'fcap0', 'Average cost of money for construction of plant', 'Average cost of money for construction of plant\nassuming design/construction time of six years') ,
	 ('float', 'fcap0cp', 'Average cost of money for replaceable components', 'Average cost of money for replaceable components\nassuming lead time for these of two years') ,
	 ('float', 'fcdfuel', 'Fraction of current drive cost treated as fuel', 'Fraction of current drive cost treated as fuel\n(if ifueltyp = 1)') ,
	 ('float', 'fcontng', 'Project contingency factor', 'Project contingency factor') ,
	 ('float', 'fcr0', 'Fixed charge rate during construction', 'Fixed charge rate during construction') ,
	 ('float', 'fkind', 'Multiplier for nth of a kind costs', 'Multiplier for nth of a kind costs') ,
	 ('int', 'iavail', 'Switch for plant availability model:', 'Switch for plant availability model:\n= 0 use input value for cfactr;\n= 1 calculate cfactr using model') ,
	 ('int', 'ifueltyp', 'Switch:', 'Switch:\n= 1 treat blanket divertor, first wall and\nfraction fcdfuel of cd equipment as fuel cost;\n= 0 treat these as capital cost') ,
	 ('int', 'ipnet', 'Switch for net electric power calculation:', 'Switch for net electric power calculation:\n= 0 scale so that always > 0;\n= 1 let go < 0 (no c-o-e)') ,
	 ('int', 'ireactor', 'Switch for net electric power and cost of', 'Switch for net electric power and cost of\nelectricity calculations:\n= 0 do not calculate mw(electric) or c-o-e;\n= 1 calculate mw(electric) and c-o-e') ,
	 ('int', 'lsa', 'Level of safety assurance switch (generally, use 3 or 4):', 'Level of safety assurance switch (generally, use 3 or 4):\n= 1 truly passively safe plant;\n= 2,3 in-between;\n= 4 like current fission plant') ,
	 ('float', 'ratecdol', 'Effective cost of money in constant dollars', 'Effective cost of money in constant dollars') ,
	 ('float', 'tbktrepl', 'Time taken to replace blanket (y)', 'Time taken to replace blanket (y)') ,
	 ('float', 'tcomrepl', 'Time taken to replace both blanket and divertor (y)', 'Time taken to replace both blanket and divertor (y)') ,
	 ('float', 'tdivrepl', 'Time taken to replace divertor (y)', 'Time taken to replace divertor (y)') ,
	 ('float', 'tlife', 'Plant life (years)', 'Plant life (years)') ,
	 ('float', 'ucblbe', 'Unit cost for blanket beryllium ($/kg)', 'Unit cost for blanket beryllium ($/kg)') ,
	 ('float', 'ucblbreed', 'Unit cost for breeder material ($/kg) (blktmodel>0)', 'Unit cost for breeder material ($/kg) (blktmodel>0)') ,
	 ('float', 'ucblli', 'Unit cost for blanket lithium ($/kg) (30% li6)', 'Unit cost for blanket lithium ($/kg) (30% li6)') ,
	 ('float', 'ucblli2o', 'Unit cost for blanket li_2o ($/kg)', 'Unit cost for blanket li_2o ($/kg)') ,
	 ('float', 'ucbllipb', 'Unit cost for blanket li-pb ($/kg) (30% li6)', 'Unit cost for blanket li-pb ($/kg) (30% li6)') ,
	 ('float', 'ucblss', 'Unit cost for blanket stainless steel ($/kg)', 'Unit cost for blanket stainless steel ($/kg)') ,
	 ('float', 'ucblvd', 'Unit cost for blanket vanadium ($/kg)', 'Unit cost for blanket vanadium ($/kg)') ,
	 ('float', 'ucbus', 'Cost of aluminium bus for tf coil ($/a-m)', 'Cost of aluminium bus for tf coil ($/a-m)') ,
	 ('float', 'uccase', 'Cost of superconductor case ($/kg)', 'Cost of superconductor case ($/kg)') ,
	 ('float', 'uccpcl1', 'Cost of high strength tapered copper ($/kg)', 'Cost of high strength tapered copper ($/kg)') ,
	 ('float', 'uccpclb', 'Cost of tf outboard leg plate coils ($/kg)', 'Cost of tf outboard leg plate coils ($/kg)') ,
	 ('float', 'uccry', 'Heat transport system cryoplant costs ($/w**expcry)', 'Heat transport system cryoplant costs ($/w**expcry)') ,
	 ('float', 'uccryo', 'Unit cost for vacuum vessel ($/kg)', 'Unit cost for vacuum vessel ($/kg)') ,
	 ('float', 'uccu', 'Unit cost for copper in superconducting cable ($/kg)', 'Unit cost for copper in superconducting cable ($/kg)') ,
	 ('float', 'ucdiv', 'Cost of divertor blade ($)', 'Cost of divertor blade ($)') ,
	 ('float', 'ucech', 'Ech system cost ($/w)', 'Ech system cost ($/w)') ,
	 ('float', 'ucf1', 'Cost of fuelling system ($)', 'Cost of fuelling system ($)') ,
	 ('float', 'ucfnc', 'Outer pf coil fence support cost ($/kg)', 'Outer pf coil fence support cost ($/kg)') ,
	 ('float', 'ucfuel', 'Unit cost of d-t fuel (m$/year/1200mw)', 'Unit cost of d-t fuel (m$/year/1200mw)') ,
	 ('float', 'uche3', 'Cost of helium-3 ($/kg)', 'Cost of helium-3 ($/kg)') ,
	 ('float', 'uchhten', 'Cost of h production (hte - endothermic) ($/kw hyd)', 'Cost of h production (hte - endothermic) ($/kw hyd)') ,
	 ('float', 'uchhtex', 'Cost of h production (hte - exothermic) ($/kw hyd)', 'Cost of h production (hte - exothermic) ($/kw hyd)') ,
	 ('float', 'uchlte', 'Cost of h production (lte) ($/kw hydrogen)', 'Cost of h production (lte) ($/kw hydrogen)') ,
	 ('float', 'uchrs', 'Cost of heat rejection system ($)', 'Cost of heat rejection system ($)') ,
	 ('float', 'uchth', 'Cost of h production (thermo-chemical) ($/kw hydrogen)', 'Cost of h production (thermo-chemical) ($/kw hydrogen)') ,
	 ('array', 'uchts', 'Cost of heat transport system equipment', 'Cost of heat transport system equipment\nper loop ($/w); dependent on coolant type') ,
	 ('float', 'uciac', 'Cost of instrumentation, control & diagnostics ($/w)', 'Cost of instrumentation, control & diagnostics ($/w)') ,
	 ('float', 'ucich', 'Ich system cost ($/w)', 'Ich system cost ($/w)') ,
	 ('float', 'ucihx', 'Cost of intermediate heat exchangers ($/w**exphts)', 'Cost of intermediate heat exchangers ($/w**exphts)') ,
	 ('float', 'uclh', 'Lh system cost ($/w)', 'Lh system cost ($/w)') ,
	 ('float', 'ucme', 'Unit cost of maintenance equipment ($/w**0.3)', 'Unit cost of maintenance equipment ($/w**0.3)') ,
	 ('float', 'ucmisc', 'Miscellaneous plant allowance ($)', 'Miscellaneous plant allowance ($)') ,
	 ('float', 'ucnbi', 'Nbi system cost ($/w)', 'Nbi system cost ($/w)') ,
	 ('array', 'ucoam', 'Annual cost of operation and', 'Annual cost of operation and\nmaintenance (m$/year/1200mw**0.5)') ,
	 ('float', 'ucof', 'Oscillating field current drive cost ($/w)', 'Oscillating field current drive cost ($/w)') ,
	 ('float', 'ucpens', 'Penetration shield cost ($/kg)', 'Penetration shield cost ($/kg)') ,
	 ('float', 'ucpfb', 'Cost of pf coil buses ($/ka/m)', 'Cost of pf coil buses ($/ka/m)') ,
	 ('float', 'ucpfbk', 'Cost of pf coil dc breakers ($/mva)', 'Cost of pf coil dc breakers ($/mva)') ,
	 ('float', 'ucpfbs', 'Cost of pf burn power supplies ($/kw**0.7)', 'Cost of pf burn power supplies ($/kw**0.7)') ,
	 ('float', 'ucpfcb', 'Cost of pf coil ac breakers ($/circuit)', 'Cost of pf coil ac breakers ($/circuit)') ,
	 ('float', 'ucpfdr1', 'Cost factor for dump resistors ($/mj)', 'Cost factor for dump resistors ($/mj)') ,
	 ('float', 'ucpfic', 'Cost of pf instrumentation and control ($/channel)', 'Cost of pf instrumentation and control ($/channel)') ,
	 ('float', 'ucpfps', 'Cost of pf coil pulsed power supplies ($/mva)', 'Cost of pf coil pulsed power supplies ($/mva)') ,
	 ('float', 'ucrb', 'Cost of reactor building (m$/m3)', 'Cost of reactor building (m$/m3)') ,
	 ('array', 'ucsc', 'Cost of superconductor ($/kg)', 'Cost of superconductor ($/kg)') ,
	 ('float', 'ucshld', 'Cost of shield structural steel ($/kg)', 'Cost of shield structural steel ($/kg)') ,
	 ('float', 'uctfbr', 'Cost of tf coil breakers ($/w**0.7)', 'Cost of tf coil breakers ($/w**0.7)') ,
	 ('float', 'uctfbus', 'Cost of tf coil bus ($/kg)', 'Cost of tf coil bus ($/kg)') ,
	 ('float', 'uctfps', 'Cost of tf coil power supplies ($/w**0.7)', 'Cost of tf coil power supplies ($/w**0.7)') ,
	 ('float', 'uctfsw', 'Cost of tf coil slow dump switches ($/a)', 'Cost of tf coil slow dump switches ($/a)') ,
	 ('array', 'ucturb', 'Cost of turbine plant equipment ($)', 'Cost of turbine plant equipment ($)\n(dependent on coolant type)') ,
	 ('float', 'ucwindpf', 'Cost of pf coil superconductor windings ($/m)', 'Cost of pf coil superconductor windings ($/m)') ,
	 ('float', 'ucwindtf', 'Cost of tf coil superconductor windings ($/m)', 'Cost of tf coil superconductor windings ($/m)') ,
	 ('array', 'ucwst', 'Cost of waste disposal (m$/y/1200mw)', 'Cost of waste disposal (m$/y/1200mw)') ,
	 ('float', 'uubop', 'Unplanned unavailability factor for balance of plant', 'Unplanned unavailability factor for balance of plant') ,
	 ('float', 'uucd', 'Unplanned unavailability factor for current drive', 'Unplanned unavailability factor for current drive') ,
	 ('float', 'uudiv', 'Unplanned unavailability factor for divertor', 'Unplanned unavailability factor for divertor') ,
	 ('float', 'uufuel', 'Unplanned unavailability factor for fuel system', 'Unplanned unavailability factor for fuel system') ,
	 ('float', 'uufw', 'Unplanned unavailability factor for first wall', 'Unplanned unavailability factor for first wall') ,
	 ('float', 'uumag', 'Unplanned unavailability factor for magnets', 'Unplanned unavailability factor for magnets') ,
	 ('float', 'uuves', 'Unplanned unavailability factor for vessel', 'Unplanned unavailability factor for vessel') ,
	]
GUI_MODULE['Current Drive'] = [
	 ('float', 'beamwd', 'Width of neutral beam duct where it passes', 'Width of neutral beam duct where it passes\nbetween the tf coils (m)\n(t inoue et al, design of neutral beam system for iter-feat,\n<a href=http://dx.doi.org/10.1016/s0920-3796(01)00339-8>\nfusion engineering and design, volumes 56-57, october 2001, pages 517-521</a>)') ,
	 ('float', 'bscfmax', 'Maximum fraction of plasma current from bootstrap;', 'Maximum fraction of plasma current from bootstrap;\nif bscfmax < 0, bootstrap fraction = abs(bscfmax)') ,
	 ('float', 'cboot', 'Bootstrap current fraction multiplier (ibss=1)', 'Bootstrap current fraction multiplier (ibss=1)') ,
	 ('float', 'enbeam', 'Neutral beam energy (kev) (iteration variable 19)', 'Neutral beam energy (kev) (iteration variable 19)') ,
	 ('float', 'etaech', 'Ech wall plug to injector efficiency', 'Ech wall plug to injector efficiency') ,
	 ('float', 'etalh', 'Lower hybrid wall plug to injector efficiency', 'Lower hybrid wall plug to injector efficiency') ,
	 ('float', 'etanbi', 'Neutral beam wall plug to injector efficiency', 'Neutral beam wall plug to injector efficiency') ,
	 ('float', 'etaof', 'Oscillating field wall plug to injector efficiency', 'Oscillating field wall plug to injector efficiency') ,
	 ('float', 'feffcd', 'Current drive efficiency fudge factor (iteration variable 47)', 'Current drive efficiency fudge factor (iteration variable 47)') ,
	 ('float', 'frbeam', 'R_tangential / r_major for neutral beam injection', 'R_tangential / r_major for neutral beam injection') ,
	 ('float', 'ftritbm', 'Fraction of beam that is tritium', 'Fraction of beam that is tritium') ,
	 ('int', 'iefrf', 'Switch for current drive efficiency model:', 'Switch for current drive efficiency model:\nfenstermacher lower hybrid\nion cyclotron current drive\nfenstermacher ech\nehst lower hybrid\niter neutral beam\nnew culham lower hybrid model\nnew culham eccd model\nnew culham neutral beam model\nrfp oscillating field current drive') ,
	 ('int', 'irfcd', 'Switch for current drive calculation:', 'Switch for current drive calculation:\n= 0 turned off;\n= 1 turned on') ,
	 ('float', 'nbshield', 'Neutral beam duct shielding thickness (m)', 'Neutral beam duct shielding thickness (m)') ,
	 ('float', 'pheat', 'Heating power not used for current drive (mw)', 'Heating power not used for current drive (mw)\n(iteration variable 11)') ,
	 ('float', 'pinjalw', 'Maximum allowable value for injected power (mw)', 'Maximum allowable value for injected power (mw)\n(constraint equation 30)') ,
	 ('float', 'tbeamin', 'Permitted neutral beam e-decay lengths to plasma centre', 'Permitted neutral beam e-decay lengths to plasma centre') ,
	]
GUI_MODULE['Divertor'] = [
	 ('float', 'anginc', 'Angle of incidence of field line on plate (rad)', 'Angle of incidence of field line on plate (rad)') ,
	 ('float', 'bpsout', 'Reference b_p at outboard divertor strike point (t)', 'Reference b_p at outboard divertor strike point (t)') ,
	 ('float', 'c1div', 'Fitting coefficient to adjust ptpdiv, ppdiv', 'Fitting coefficient to adjust ptpdiv, ppdiv') ,
	 ('float', 'c2div', 'Fitting coefficient to adjust ptpdiv, ppdiv', 'Fitting coefficient to adjust ptpdiv, ppdiv') ,
	 ('float', 'c3div', 'Fitting coefficient to adjust ptpdiv, ppdiv', 'Fitting coefficient to adjust ptpdiv, ppdiv') ,
	 ('float', 'c4div', 'Fitting coefficient to adjust ptpdiv, ppdiv', 'Fitting coefficient to adjust ptpdiv, ppdiv') ,
	 ('float', 'c5div', 'Fitting coefficient to adjust ptpdiv, ppdiv', 'Fitting coefficient to adjust ptpdiv, ppdiv') ,
	 ('float', 'c6div', 'Fitting coefficient to adjust ptpdiv, ppdiv', 'Fitting coefficient to adjust ptpdiv, ppdiv') ,
	 ('float', 'delld', 'Coeff for power distribution along main plasma', 'Coeff for power distribution along main plasma') ,
	 ('float', 'divclfr', 'Divertor coolant fraction', 'Divertor coolant fraction') ,
	 ('float', 'divdens', 'Divertor structure density (kg/m3)', 'Divertor structure density (kg/m3)') ,
	 ('int', 'divdum', 'Switch for divertor zeff model: 0=calc, 1=input', 'Switch for divertor zeff model: 0=calc, 1=input') ,
	 ('float', 'divfix', 'Divertor structure vertical thickness (m)', 'Divertor structure vertical thickness (m)') ,
	 ('float', 'divplt', 'Divertor plate thickness (m) (from spears, sept 1990)', 'Divertor plate thickness (m) (from spears, sept 1990)') ,
	 ('float', 'fdfs', 'Radial gradient ratio', 'Radial gradient ratio') ,
	 ('float', 'fdiva', 'Divertor area fudge factor (for iter, sept 1990)', 'Divertor area fudge factor (for iter, sept 1990)') ,
	 ('float', 'fgamp', 'Sheath potential factor (not used)', 'Sheath potential factor (not used)') ,
	 ('float', 'fififi', 'Coefficient for gamdiv', 'Coefficient for gamdiv') ,
	 ('float', 'frrp', 'Fraction of radiated power to plate', 'Fraction of radiated power to plate') ,
	 ('float', 'hldivlim', 'Heat load limit (mw/m2)', 'Heat load limit (mw/m2)') ,
	 ('float', 'ksic', 'Power fraction for outboard double-null scrape-off plasma', 'Power fraction for outboard double-null scrape-off plasma') ,
	 ('float', 'omegan', 'Pressure ratio (nt)_plasma / (nt)_scrape-off', 'Pressure ratio (nt)_plasma / (nt)_scrape-off') ,
	 ('float', 'plsepo', 'Poloidal length, x-point to outboard strike point (m)', 'Poloidal length, x-point to outboard strike point (m)') ,
	 ('float', 'prn1', 'N-scrape-off / n-average plasma;', 'N-scrape-off / n-average plasma;\n(input for ipedestal=0, = nesep/dene if ipedestal=1)') ,
	 ('float', 'rlenmax', 'Maximum value for length ratio (rlclolcn) (eqn.22)', 'Maximum value for length ratio (rlclolcn) (eqn.22)') ,
	 ('float', 'tdiv', 'Temperature at divertor (ev)', 'Temperature at divertor (ev)\n(input for stellarator only, calculated for tokamaks)') ,
	 ('float', 'xparain', 'Parallel heat transport coefficient (m2/s)', 'Parallel heat transport coefficient (m2/s)') ,
	 ('float', 'xpertin', 'Perpendicular heat transport coefficient (m2/s)', 'Perpendicular heat transport coefficient (m2/s)') ,
	 ('float', 'zeffdiv', 'Zeff in the divertor region (if divdum /= 0)', 'Zeff in the divertor region (if divdum /= 0)') ,
	]
GUI_MODULE['Fwbs'] = [
	 ('int', 'blktmodel', 'Switch for blanket/tritium breeding model', 'Switch for blanket/tritium breeding model\n(but see <code>lblnkt</code>):\n= 0 original simple model;\n= 1 kit model based on a helium-cooled pebble-bed\nblanket (hcpb) reference design') ,
	 ('float', 'declblkt', 'Neutron power deposition decay length of blanket structural material (m)', 'Neutron power deposition decay length of blanket structural material (m)\n(ipowerflow=1)') ,
	 ('float', 'declfw', 'Neutron power deposition decay length of first wall structural material (m)', 'Neutron power deposition decay length of first wall structural material (m)\n(ipowerflow=1)') ,
	 ('float', 'declshld', 'Neutron power deposition decay length of shield structural material (m)', 'Neutron power deposition decay length of shield structural material (m)\n(ipowerflow=1)') ,
	 ('float', 'denstl', 'Density of steel (kg/m3)', 'Density of steel (kg/m3)') ,
	 ('float', 'emult', 'Energy multiplication in blanket and shield', 'Energy multiplication in blanket and shield') ,
	 ('float', 'fblbe', 'Beryllium fraction of blanket by volume', 'Beryllium fraction of blanket by volume\n(if blktmodel>0, be fraction of breeding zone)') ,
	 ('float', 'fblli', 'Lithium fraction of blanket by volume', 'Lithium fraction of blanket by volume\n(blktmodel=0, lblnkt=1, smstr=2)') ,
	 ('float', 'fblli2o', 'Lithium oxide fraction of blanket by volume', 'Lithium oxide fraction of blanket by volume\n(blktmodel=0, lblnkt=1, smstr=1)') ,
	 ('float', 'fbllipb', 'Lithium lead fraction of blanket by volume', 'Lithium lead fraction of blanket by volume\n(blktmodel=0, lblnkt=0 or 1, smstr=2)') ,
	 ('float', 'fblss', 'Stainless steel fraction of blanket by volume', 'Stainless steel fraction of blanket by volume\n(if blktmodel>0, steel fraction of breeding zone)') ,
	 ('float', 'fblvd', 'Vanadium fraction of blanket by volume', 'Vanadium fraction of blanket by volume\n(blktmodel=0)') ,
	 ('float', 'fdiv', 'Area fraction taken up by divertor (ipowerflow=1)', 'Area fraction taken up by divertor (ipowerflow=1)') ,
	 ('float', 'fhcd', 'Area fraction covered by heating/current drive', 'Area fraction covered by heating/current drive\napparatus plus diagnostics (ipowerflow=1)') ,
	 ('float', 'fhole', 'Area fraction taken up by other holes', 'Area fraction taken up by other holes') ,
	 ('float', 'fvoldw', 'Area coverage factor for vacuum vessel volume', 'Area coverage factor for vacuum vessel volume') ,
	 ('float', 'fvolsi', 'Area coverage factor for inboard shield volume', 'Area coverage factor for inboard shield volume') ,
	 ('float', 'fvolso', 'Area coverage factor for outboard shield volume', 'Area coverage factor for outboard shield volume') ,
	 ('float', 'fwclfr', 'First wall coolant fraction', 'First wall coolant fraction\n(calculated if lpulse=1)') ,
	 ('int', 'fwbsshape', 'First wall, blanket, shield and vacuum vessel shape:', 'First wall, blanket, shield and vacuum vessel shape:\n= 1 d-shaped (cylinder inboard + ellipse outboard);\n= 2 defined by two ellipses') ,
	 ('float', 'rpf2dewar', 'Radial distance between outer edge of largest', 'Radial distance between outer edge of largest\nipfloc=3 pf coil (or stellarator modular coil)\nand external cryostat (m)') ,
	 ('float', 'vfblkt', 'Coolant void fraction in blanket (blktmodel=0),', 'Coolant void fraction in blanket (blktmodel=0),\n(calculated if blktmodel > 0)') ,
	 ('float', 'vfshld', 'Coolant void fraction in shield', 'Coolant void fraction in shield') ,
	 ('int', 'breedmat', 'Breeder material switch (blktmodel>0):', 'Breeder material switch (blktmodel>0):\n= 1 lithium orthosilicate;\n= 2 lithium methatitanate;\n= 3 lithium zirconate') ,
	 ('float', 'fblbreed', 'Breeder fraction of blanket breeding zone by volume', 'Breeder fraction of blanket breeding zone by volume\n(blktmodel>0)') ,
	 ('float', 'fblhebmi', 'Helium fraction of inboard blanket box manifold by volume', 'Helium fraction of inboard blanket box manifold by volume\n(blktmodel>0)') ,
	 ('float', 'fblhebmo', 'Helium fraction of outboard blanket box manifold by volume', 'Helium fraction of outboard blanket box manifold by volume\n(blktmodel>0)') ,
	 ('float', 'fblhebpi', 'Helium fraction of inboard blanket back plate by volume', 'Helium fraction of inboard blanket back plate by volume\n(blktmodel>0)') ,
	 ('float', 'fblhebpo', 'Helium fraction of outboard blanket back plate by volume', 'Helium fraction of outboard blanket back plate by volume\n(blktmodel>0)') ,
	 ('int', 'hcdportsize', 'Size of heating/current drive ports (blktmodel>0):', "Size of heating/current drive ports (blktmodel>0):\n= 1 'small'\n= 2 'large'") ,
	 ('float', 'li6enrich', 'Lithium-6 enrichment of breeding material (%)', 'Lithium-6 enrichment of breeding material (%)\n(blktmodel>0)') ,
	 ('int', 'npdiv', 'Number of divertor ports (blktmodel>0)', 'Number of divertor ports (blktmodel>0)') ,
	 ('int', 'nphcdin', 'Number of inboard ports for heating/current drive', 'Number of inboard ports for heating/current drive\n(blktmodel>0)') ,
	 ('int', 'nphcdout', 'Number of outboard ports for heating/current drive', 'Number of outboard ports for heating/current drive\n(blktmodel>0)') ,
	 ('float', 'wallpf', 'Neutron wall load peaking factor (blktmodel>0)', 'Neutron wall load peaking factor (blktmodel>0)') ,
	 ('int', 'astr', 'Switch for blanket cooling channel geometry (lblnkt=1):', 'Switch for blanket cooling channel geometry (lblnkt=1):\n= 1 circular cross section;\n= 2 annular cross section') ,
	 ('int', 'bstr', 'Switch for blanket boundary condition (lblnkt=1):', 'Switch for blanket boundary condition (lblnkt=1):\n= 1 coolant outlet temperature fixed;\n= 2 maximum blanket temperature fixed') ,
	 ('int', 'costr', 'Switch for blanket coolant material (lblnkt=1):', 'Switch for blanket coolant material (lblnkt=1):\n= 1 gaseous helium coolant;\n= 2 pressurized water coolant\n(costr=2 (sic) is forced if blktmodel > 0, as only the\nblanket is helium-cooled in this model)') ,
	 ('int', 'estr', 'Switch for cooling channel orientation (lblnkt=1):', 'Switch for cooling channel orientation (lblnkt=1):\n= 1 radially orientated;\n= 2 poloidally orientated') ,
	 ('float', 'etacp', 'Condenser isentropic efficiency', 'Condenser isentropic efficiency') ,
	 ('float', 'etafp', "Feed water pumps' isentropic efficiency", "Feed water pumps' isentropic efficiency") ,
	 ('float', 'etahp', 'High pressure turbine isentropic efficiency', 'High pressure turbine isentropic efficiency') ,
	 ('float', 'etainp', 'Intermediate pressure turbine isentropic efficiency', 'Intermediate pressure turbine isentropic efficiency') ,
	 ('float', 'etalp', 'Low pressure turbine isentropic efficiency', 'Low pressure turbine isentropic efficiency') ,
	 ('float', 'fkblkt', 'Blanket elongation / plasma elongation', 'Blanket elongation / plasma elongation') ,
	 ('int', 'lblnkt', 'Switch for blanket model:', 'Switch for blanket model:\n= 0 original model (but see <code>blktmodel</code>);\n= 1 full thermodynamic model') ,
	 ('int', 'nipfwh', 'Number of intermediate pressure feed water heater pumps', 'Number of intermediate pressure feed water heater pumps') ,
	 ('int', 'nlpfwh', 'Number of low pressure feed water heater pumps', 'Number of low pressure feed water heater pumps') ,
	 ('float', 'pc', 'Low pressure turbine outlet pressure (mpa)', 'Low pressure turbine outlet pressure (mpa)') ,
	 ('float', 'ph', 'High pressure turbine inlet pressure (mpa)', 'High pressure turbine inlet pressure (mpa)') ,
	 ('float', 'pin', 'Low pressure turbine inlet pressure (mpa)', 'Low pressure turbine inlet pressure (mpa)') ,
	 ('float', 'pr', 'Intermediate pressure turbine inlet pressure (mpa)', 'Intermediate pressure turbine inlet pressure (mpa)') ,
	 ('float', 'sgeff', 'Steam generator effectiveness', 'Steam generator effectiveness') ,
	 ('int', 'smstr', 'Switch for blanket material (lblnkt=1):', 'Switch for blanket material (lblnkt=1):\n= 1 li2o/be (solid blanket);\n= 2 lipb/li (liquid blanket)') ,
	 ('float', 'xdi', 'Inner cooling channel diameter (cm)', 'Inner cooling channel diameter (cm)') ,
	 ('float', 'xdo', 'Outer cooling channel diameter (cm)', 'Outer cooling channel diameter (cm)') ,
	 ('float', 'xpf', 'Blanket coolant inlet pressure (mpa)', 'Blanket coolant inlet pressure (mpa)') ,
	 ('float', 'xtb', 'Maximum blanket temperature (c)', 'Maximum blanket temperature (c)') ,
	 ('float', 'xtfi', 'Inlet coolant temperature (c)', 'Inlet coolant temperature (c)') ,
	 ('float', 'xtfo', 'Outlet coolant temperature (c)', 'Outlet coolant temperature (c)') ,
	]
GUI_MODULE['Global'] = [
	 ('int', 'verbose', 'Switch for turning on/off diagnostic messages:', 'Switch for turning on/off diagnostic messages:\n= 0 turn off diagnostics\n= 1 turn on diagnostics') ,
	]
GUI_MODULE['Heat Transport'] = [
	 ('float', 'baseel', 'Base plant electric load (w)', 'Base plant electric load (w)') ,
	 ('float', 'etath', 'Thermal to electric conversion efficiency', 'Thermal to electric conversion efficiency\nif lblnkt=0, otherwise calculated') ,
	 ('float', 'fauxbop', 'Fraction of gross electric power to balance-of-plant', 'Fraction of gross electric power to balance-of-plant') ,
	 ('float', 'ffwlg', 'Fraction of first wall / divertor power to low grade heat', 'Fraction of first wall / divertor power to low grade heat\n(ipowerflow=0)') ,
	 ('float', 'fmgdmw', 'Power to mgf (motor-generator flywheel) units (mw)', 'Power to mgf (motor-generator flywheel) units (mw)\n(ignored if iscenr=2)') ,
	 ('float', 'htpmw', 'Heat transport system electrical pump power (mw)', 'Heat transport system electrical pump power (mw)\n(calculated if ipowerflow=1)') ,
	 ('int', 'ihplant', 'Switch for hydrogen production plant:', 'Switch for hydrogen production plant:\n= 0 no hydrogen plant;\n= 1 low temperature electrolysis;\n= 2 high temperature electrolysis - endothermic;\n= 3 high temperature electrolysis - exothermic;\n= 4 thermo-chemical') ,
	 ('float', 'etahhten', 'Efficiency of h production for ihplant=2', 'Efficiency of h production for ihplant=2') ,
	 ('float', 'etahhtex', 'Efficiency of h production for ihplant=3', 'Efficiency of h production for ihplant=3') ,
	 ('float', 'etahlte', 'Efficiency of h production for ihplant=1', 'Efficiency of h production for ihplant=1') ,
	 ('float', 'etahth', 'Efficiency of h production for ihplant=4', 'Efficiency of h production for ihplant=4') ,
	 ('float', 'helecmw', 'Electrical power required for h production (mw)', 'Electrical power required for h production (mw)\n(iteration variable 87)') ,
	 ('float', 'hthermmw', 'Thermal power required for h production (mw)', 'Thermal power required for h production (mw)\n(iteration variable 88)\n(n.b. calculated for ihplant=1,2,3)') ,
	 ('int', 'ipowerflow', 'Switch for power flow model:', 'Switch for power flow model:\n= 0 pre-2014 version;\n= 1 comprehensive 2014 model') ,
	 ('float', 'etahtpblkt', 'Electrical efficiency of blanket coolant pumps', 'Electrical efficiency of blanket coolant pumps\n(default assumes helium coolant) (ipowerflow=1)') ,
	 ('float', 'etahtpdiv', 'Electrical efficiency of divertor coolant pumps', 'Electrical efficiency of divertor coolant pumps\n(default assumes water coolant) (ipowerflow=1)') ,
	 ('float', 'etahtpfw', 'Electrical efficiency of first wall coolant pumps', 'Electrical efficiency of first wall coolant pumps\n(default assumes helium coolant) (ipowerflow=1)') ,
	 ('float', 'etahtpshld', 'Electrical efficiency of shield coolant pumps', 'Electrical efficiency of shield coolant pumps\n(default assumes water coolant) (ipowerflow=1)') ,
	 ('float', 'fpumpblkt', 'Fraction of total blanket thermal power required', 'Fraction of total blanket thermal power required\nto drive the blanket coolant pumps (default assumes\nhelium coolant) (ipowerflow=1)') ,
	 ('float', 'fpumpdiv', 'Fraction of total divertor thermal power required', 'Fraction of total divertor thermal power required\nto drive the divertor coolant pumps (default assumes\nwater coolant) (ipowerflow=1)') ,
	 ('float', 'fpumpfw', 'Fraction of total first wall thermal power required', 'Fraction of total first wall thermal power required\nto drive the fw coolant pumps (default assumes helium\ncoolant) (ipowerflow=1)') ,
	 ('float', 'fpumpshld', 'Fraction of total shield thermal power required', 'Fraction of total shield thermal power required\nto drive the shield coolant pumps (default assumes\nwater coolant) (ipowerflow=1)') ,
	 ('int', 'iprimdiv', 'Switch for divertor thermal power destiny:', 'Switch for divertor thermal power destiny:\n= 0 contributes to secondary heat;\n= 1 contributes to primary heat\n(ipowerflow=1)') ,
	 ('int', 'iprimshld', 'Switch for shield thermal power destiny:', 'Switch for shield thermal power destiny:\n= 0 contributes to secondary heat;\n= 1 contributes to primary heat\n(ipowerflow=1)') ,
	 ('int', 'iprimhtp', 'Switch for heat transport pump power destiny:', 'Switch for heat transport pump power destiny:\n= 0 contributes to secondary heat;\n= 1 contributes to primary heat\n(ipowerflow=0)') ,
	 ('int', 'iprimnloss', 'Switch for lost neutron power through holes destiny:', 'Switch for lost neutron power through holes destiny:\n= 0 contributes to secondary heat;\n= 1 contributes to primary heat\n(ipowerflow=0)') ,
	 ('float', 'pwpm2', 'Base ac power requirement per unit floor area (w/m2)', 'Base ac power requirement per unit floor area (w/m2)') ,
	 ('float', 'tfacpd', 'Total steady state tf coil ac power demand (mw)', 'Total steady state tf coil ac power demand (mw)\n(itfsup=0 only; calculated for itfsup=1)') ,
	 ('float', 'trithtmw', 'Power required for tritium processing (mw)', 'Power required for tritium processing (mw)') ,
	 ('float', 'vachtmw', 'Vacuum pump power (mw)', 'Vacuum pump power (mw)') ,
	]
GUI_MODULE['Impurity Radiation Module'] = [
	 ('int', 'imprad_model', 'Switch for impurity radiation model:', 'Switch for impurity radiation model:\n= 0 original iter 1989 model\n= 1 2014 multi-impurity, arbitrary profile model\n(whichever model is used, it is recommended to turn on\nconstraint eqn.17 with iteration variable 28: fradpwr.)') ,
	 ('float', 'coreradius', "Normalised radius defining the 'core' region", "Normalised radius defining the 'core' region") ,
	 ('array', 'fimp', 'Impurity number density fractions relative to electron density', 'Impurity number density fractions relative to electron density\n(iteration variable 102 is fimp(impvar))') ,
	 ('float', 'fimpvar', 'Impurity fraction to be used as fimp(impvar)', 'Impurity fraction to be used as fimp(impvar)\n(iteration variable 102)') ,
	 ('string', 'impdir', 'Directory containing impurity radiation data files', 'Directory containing impurity radiation data files') ,
	 ('int', 'impvar', 'Fimp element value to be varied if iteration', 'Fimp element value to be varied if iteration\nvariable number 102 is turned on') ,
	]
GUI_MODULE['Numerics'] = [
	 ('int', 'ioptimz', 'Code operation switch:', 'Code operation switch:\n= -1 for no optimisation, hybrd only;\n= 0  for hybrd and vmcon (not recommended);\n= 1  for optimisation, vmcon only') ,
	 ('int', 'maxcal', 'Maximum number of vmcon iterations', 'Maximum number of vmcon iterations') ,
	 ('int', 'minmax', 'Switch for figure-of-merit (see lablmm for descriptions)', 'Switch for figure-of-merit (see lablmm for descriptions)\nnegative => maximise, positive => minimise\n( 1) major radius\n( 2) p_fus p_in-total\n( 3) neutron wall load\n( 4) p_tf + p_pf\n( 5) fusion gain q\n( 6) cost of electricity\n( 7) capital cost (direct cost if ireactor=0,\nconstructed cost otherwise)\n( 8) aspect ratio\n( 9) divertor heat load\n(10) toroidal field\n(11) total injected power\n(12) hydrogen plant capital cost\n(13) hydrogen production rate\n(14) pulse length\n(15) plant availability factor (n.b. requires\niavail=1 to be set)') ,
	 ('int', 'nineqns', 'Number of inequality constraints vmcon must satisfy', 'Number of inequality constraints vmcon must satisfy\n(leave at zero for now)') ,
	 ('float', 'epsfcn', 'Finite difference step length for hybrd/vmcon derivatives', 'Finite difference step length for hybrd/vmcon derivatives') ,
	 ('float', 'epsvmc', 'Error tolerance for vmcon', 'Error tolerance for vmcon') ,
	 ('float', 'factor', 'Used in hybrd for first step size', 'Used in hybrd for first step size') ,
	 ('float', 'ftol', 'Error tolerance for hybrd', 'Error tolerance for hybrd') ,
	]
GUI_MODULE['Pf Power'] = [
	 ('int', 'iscenr', 'Switch for pf coil energy storage option:', 'Switch for pf coil energy storage option:\n= 1 all power from mgf (motor-generator flywheel) units;\n= 2 all pulsed power from line;\n= 3 pf power from mgf, heating from line\n(in fact, options 1 and 3 are not treated differently)') ,
	]
GUI_MODULE['Pfcoil'] = [
	 ('float', 'ac1oh', 'Central solenoid cable conduit area (m2)', 'Central solenoid cable conduit area (m2)') ,
	 ('float', 'acsoh', 'Conduit conductor cross section (m2)', 'Conduit conductor cross section (m2)') ,
	 ('float', 'alfapf', 'Smoothing parameter used in pf coil', 'Smoothing parameter used in pf coil\ncurrent calculation at the beginning of pulse (bop)') ,
	 ('float', 'coheof', 'Central solenoid overall current density at end of flat-top (a/m2)', 'Central solenoid overall current density at end of flat-top (a/m2)\n(iteration variable 37)') ,
	 ('array', 'cptdin', 'Current per turn input for pf coil i (a)', 'Current per turn input for pf coil i (a)') ,
	 ('float', 'fcohbop', 'Ratio of central solenoid overall current density at', 'Ratio of central solenoid overall current density at\nbeginning of pulse / end of flat-top\n(iteration variable 41)') ,
	 ('float', 'fcuoh', 'Copper fraction of conductor in central solenoid cable', 'Copper fraction of conductor in central solenoid cable') ,
	 ('array', 'ipfloc', 'Switch for locating scheme of pf coil group i:', 'Switch for locating scheme of pf coil group i:\n= 1 pf coil on top of central solenoid;\n= 2 pf coil on top of tf coil;\n= 3 pf coil outside of tf coil') ,
	 ('int', 'ipfres', 'Switch for pf coil type:', 'Switch for pf coil type:\n= 0 superconducting pf coils;\n= 1 resistive pf coils') ,
	 ('int', 'isumatpf', 'Switch for superconductor material in pf coils:', 'Switch for superconductor material in pf coils:\n= 1 binary nb3sn;\n= 2 ternary nb3sn;\n= 3 nbti') ,
	 ('array', 'ncls', 'Number of pf coils in group j', 'Number of pf coils in group j') ,
	 ('int', 'nfxfh', 'Number of coils the top and bottom of the central solenoid', 'Number of coils the top and bottom of the central solenoid\nshould be broken into during scaling (5 - 10 is good)') ,
	 ('int', 'ngrp', 'Number of groups of pf coils.', 'Number of groups of pf coils.\nsymmetric coil pairs should all be in the same group') ,
	 ('float', 'ohhghf', 'Central solenoid height / tf coil internal height', 'Central solenoid height / tf coil internal height') ,
	 ('float', 'pfclres', 'Pf coil resistivity (if ipfres=1) (ohm-m)', 'Pf coil resistivity (if ipfres=1) (ohm-m)') ,
	 ('array', 'rjconpf', 'Average current density of pf coil i (a/m2)', 'Average current density of pf coil i (a/m2)\nat time of peak current in that coil\n(calculated for ipfloc=1 coils)') ,
	 ('float', 'routr', 'Distance (m) from outboard tf coil leg to centre of', 'Distance (m) from outboard tf coil leg to centre of\nipfloc=3 pf coils') ,
	 ('float', 'rpf1', 'Offset (m) of radial position of ipfloc=1 pf coils', 'Offset (m) of radial position of ipfloc=1 pf coils\nfrom being directly above the central solenoid') ,
	 ('float', 'rpf2', 'Offset (m) of radial position of ipfloc=2 pf coils', 'Offset (m) of radial position of ipfloc=2 pf coils\nfrom being at rmajor (offset = rpf2*triang*rminor)') ,
	 ('float', 'sccufac', 'Ratio of superconductor to copper in pf coils/central solenoid', 'Ratio of superconductor to copper in pf coils/central solenoid\ncable at a magnetic field of 1t') ,
	 ('float', 'sigpfalw', 'Allowable stress in pf coils/central solenoid (mpa)', 'Allowable stress in pf coils/central solenoid (mpa)\nexcluding the steel coil case') ,
	 ('float', 'sigpfcalw', 'Maximum permissible tensile stress (mpa) in', 'Maximum permissible tensile stress (mpa) in\nsteel coil cases for superconducting pf coils\n(ipfres=0)') ,
	 ('float', 'sigpfcf', 'Fraction of jxb hoop force supported by steel case', 'Fraction of jxb hoop force supported by steel case\nfor superconducting pf coils (ipfres=0)') ,
	 ('array', 'vf', 'Void fraction of pf coil i', 'Void fraction of pf coil i') ,
	 ('float', 'vfohc', 'Void fraction of (whole) central solenoid for coolant', 'Void fraction of (whole) central solenoid for coolant') ,
	 ('array', 'zref', 'Pf coil vertical positioning adjuster:', 'Pf coil vertical positioning adjuster:\n- for groups j with ipfloc(j) = 1; zref(j) is ignored\n- for groups j with ipfloc(j) = 2 and itart=1 (only);\nzref(j) is distance of centre of pf coil from inside\nedge of tf coil (remember that pf coils for sts lie\nwithin the tf coil)\n- for groups j with ipfloc(j) = 3; zref(j) = ratio of\nheight of coil group j to plasma minor radius') ,
	]
GUI_MODULE['Physics'] = [
	 ('float', 'alphaj', 'Current profile index;', 'Current profile index;\ncalculated from q0, q if iprofile=1') ,
	 ('float', 'alphan', 'Density profile index', 'Density profile index') ,
	 ('float', 'alphat', 'Temperature profile index', 'Temperature profile index') ,
	 ('float', 'aspect', 'Aspect ratio (iteration variable 1)', 'Aspect ratio (iteration variable 1)') ,
	 ('float', 'beamfus0', 'Multiplier for beam-background fusion calculation', 'Multiplier for beam-background fusion calculation') ,
	 ('float', 'beta', 'Total plasma beta (iteration variable 5)', 'Total plasma beta (iteration variable 5)') ,
	 ('float', 'betbm0', 'Leading coefficient for nb beta fraction', 'Leading coefficient for nb beta fraction') ,
	 ('float', 'bt', 'Toroidal field on axis (t) (iteration variable 2)', 'Toroidal field on axis (t) (iteration variable 2)') ,
	 ('float', 'cfe0', 'Seeded high-z impurity fraction (n_highz / n_e)', 'Seeded high-z impurity fraction (n_highz / n_e)\n(imprad_model=0 only) (iteration variable 43)') ,
	 ('float', 'csawth', 'Coeff. for sawteeth effects on burn v-s requirement', 'Coeff. for sawteeth effects on burn v-s requirement') ,
	 ('float', 'cvol', 'Multiplying factor times plasma volume (normally=1)', 'Multiplying factor times plasma volume (normally=1)') ,
	 ('float', 'dene', 'Electron density (/m3) (iteration variable 6)', 'Electron density (/m3) (iteration variable 6)') ,
	 ('float', 'dnbeta', '(troyon-like) coefficient for beta scaling;', '(troyon-like) coefficient for beta scaling;\ncalculated as (4.0*rli) if iprofile=1\n(see also gtscale option)') ,
	 ('float', 'epbetmax', 'Maximum (eps*beta_poloidal) for 2nd stability beta limit', 'Maximum (eps*beta_poloidal) for 2nd stability beta limit\n(constraint equation 6)') ,
	 ('float', 'falpha', 'Fraction of alpha power deposited in plasma', 'Fraction of alpha power deposited in plasma\n(physics of energetic ions, p.2489)') ,
	 ('float', 'fbfe', 'Fraction of high-z radiation to bremsstrahlung', 'Fraction of high-z radiation to bremsstrahlung\n(imprad_model=0 only)') ,
	 ('float', 'fdeut', 'Deuterium fuel fraction', 'Deuterium fuel fraction') ,
	 ('float', 'ffwal', 'Factor to convert plasma surface area to first wall', 'Factor to convert plasma surface area to first wall\narea in neutron wall load calculation (iwalld=1)') ,
	 ('float', 'fhe3', 'Helium-3 fuel fraction', 'Helium-3 fuel fraction') ,
	 ('float', 'ftrit', 'Tritium fuel fraction', 'Tritium fuel fraction') ,
	 ('float', 'fvsbrnni', 'Fraction of the plasma current produced by', 'Fraction of the plasma current produced by\nnon-inductive means (iteration variable 44)') ,
	 ('float', 'gamma', 'Ejima coefficient for resistive startup v-s formula', 'Ejima coefficient for resistive startup v-s formula') ,
	 ('int', 'gtscale', 'Switch for a/r scaling of dnbeta (iprofile=0 only):', 'Switch for a/r scaling of dnbeta (iprofile=0 only):\n= 0 do not scale dnbeta with eps;\n= 1 scale dnbeta with eps') ,
	 ('float', 'hfact', 'H factor on energy confinement times (iteration variable 10)', 'H factor on energy confinement times (iteration variable 10)') ,
	 ('int', 'ibss', 'Switch for bootstrap current scaling:', 'Switch for bootstrap current scaling:\n= 1 iter 1989 bootstrap scaling (high r/a only);\n= 2 for nevins et al general scaling;\n= 3 for wilson et al numerical scaling;\n= 4 for sauter et al scaling') ,
	 ('int', 'iculbl', 'Switch for beta limit scaling (constraint equation 24):', 'Switch for beta limit scaling (constraint equation 24):\n= 0 apply limit to total beta;\n= 1 apply limit to thermal beta;\n= 2 apply limit to thermal + neutral beam beta') ,
	 ('int', 'icurr', 'Switch for plasma current scaling to use:', 'Switch for plasma current scaling to use:\n= 1 peng analytic fit;\n= 2 peng double null divertor scaling (st);\n= 3 simple iter scaling (k = 2.2, d = 0.6);\n= 4 later iter scaling, a la uckan;\n= 5 todd empirical scaling i;\n= 6 todd empirical scaling ii;\n= 7 connor-hastie model') ,
	 ('int', 'idensl', 'Switch for density limit to enforce (constraint equation 5):', 'Switch for density limit to enforce (constraint equation 5):\n= 1 old asdex;\n= 2 borrass model for iter (i);\n= 3 borrass model for iter (ii);\n= 4 jet edge radiation;\n= 5 jet simplified;\n= 6 hugill-murakami mq limit;\n= 7 greenwald limit') ,
	 ('int', 'ifalphap', 'Switch for fast alpha pressure calculation:', 'Switch for fast alpha pressure calculation:\n= 0 iter physics rules (uckan) fit;\n= 1 modified fit (d. ward) - better at high temperature') ,
	 ('int', 'ifispact', 'Switch for neutronics calculations:', 'Switch for neutronics calculations:\n= 0 neutronics calculations turned off;\n= 1 neutronics calculations turned on') ,
	 ('int', 'igeom', 'Switch for plasma geometry calculation:', 'Switch for plasma geometry calculation:\n= 0 original method (possibly based on peng st modelling);\n= 1 improved (and traceable) method') ,
	 ('int', 'ignite', 'Switch for ignition assumption:', 'Switch for ignition assumption:\n= 0 do not assume plasma ignition;\n= 1 assume ignited (but include auxiliary power in costs)\nobviously, ignite must be zero if current drive is required.\nif ignite=1, any auxiliary power is assumed to be used only\nduring plasma start-up, and is excluded from all steady-state\npower balance calculations.') ,
	 ('int', 'iinvqd', 'Switch for inverse quadrature in l-mode scaling laws 5 and 9:', 'Switch for inverse quadrature in l-mode scaling laws 5 and 9:\n= 0 inverse quadrature not used;\n= 1 inverse quadrature with neo-alcator tau-e used') ,
	 ('float', 'impc', 'Carbon impurity multiplier (imprad_model=0 only)', 'Carbon impurity multiplier (imprad_model=0 only)') ,
	 ('float', 'impo', 'Oxygen impurity multiplier (imprad_model=0 only)', 'Oxygen impurity multiplier (imprad_model=0 only)') ,
	 ('int', 'ipedestal', 'Switch for pedestal profiles:', 'Switch for pedestal profiles:\n= 0 use original parabolic profiles;\n= 1 use pedestal profiles') ,
	 ('float', 'neped', 'Electron density of pedestal (/m3) (ipedestal=1)', 'Electron density of pedestal (/m3) (ipedestal=1)') ,
	 ('float', 'nesep', 'Electron density at separatrix (/m3) (ipedestal=1)', 'Electron density at separatrix (/m3) (ipedestal=1)') ,
	 ('float', 'rhopedn', 'R/a of density pedestal (ipedestal=1)', 'R/a of density pedestal (ipedestal=1)') ,
	 ('float', 'rhopedt', 'R/a of temperature pedestal (ipedestal=1)', 'R/a of temperature pedestal (ipedestal=1)') ,
	 ('float', 'tbeta', 'Temperature profile index beta  (ipedestal=1)', 'Temperature profile index beta  (ipedestal=1)') ,
	 ('float', 'teped', 'Electron temperature of pedestal (kev) (ipedestal=1)', 'Electron temperature of pedestal (kev) (ipedestal=1)') ,
	 ('float', 'tesep', 'Electron temperature at separatrix (kev) (ipedestal=1)', 'Electron temperature at separatrix (kev) (ipedestal=1)') ,
	 ('int', 'iprofile', 'Switch for current profile consistency:', 'Switch for current profile consistency:\n= 0 use input values for alphaj, rli, dnbeta\n(but see gtscale option);\n= 1 make these consistent with input q, q0 values\n(recommendation: use icurr=4 with this option)') ,
	 ('int', 'iradloss', 'Switch for radiation loss term usage in power balance:', 'Switch for radiation loss term usage in power balance:\n= 0 use non-radiation-adjusted loss power in\nconfinement scaling and power balance\n= 1 use radiation-adjusted loss power in\nconfinement scaling and power balance') ,
	 ('int', 'isc', 'Switch for energy confinement time scaling law', 'Switch for energy confinement time scaling law\n( 1)  neo-alcator (ohmic)\n( 2)  mirnov (h-mode)\n( 3)  merezkhin-muhkovatov (l-mode)\n( 4)  shimomura (h-mode)\n( 5)  kaye-goldston (l-mode)\n( 6)  iter 89-p (l-mode)\n( 7)  iter 89-o (l-mode)\n( 8)  rebut-lallia (l-mode)\n( 9)  goldston (l-mode)\n(10)  t10 (l-mode)\n(11)  jaeri-88 (l-mode)\n(12)  kaye-big complex (l-mode)\n(13)  iter h90-p (h-mode)\n(14)  iter mix (l-mode)\n(15)  riedel (l-mode)\n(16)  christiansen (l-mode)\n(17)  lackner-gottardi (l-mode)\n(18)  neo-kaye (l-mode)\n(19)  riedel (h-mode)\n(20)  iter h90-p amended (h-mode)\n(21)  lhd (stellarator)\n(22)  gyro-reduced bohm (stellarator)\n(23)  lackner-gottardi (stellarator)\n(24)  iter-93h (h-mode)\n(25)  titan (rfp)\n(26)  iter h-97p elm-free (h-mode)\n(27)  iter h-97p elmy (h-mode)\n(28)  iter-96p (=iter-97l) (l-mode)\n(29)  valovic modified elmy (h-mode)\n(30)  kaye pppl april 98 (l-mode)\n(31)  iterh-pb98p(y) (h-mode)\n(32)  ipb98(y) (h-mode)\n(33)  ipb98(y,1) (h-mode)\n(34)  ipb98(y,2) (h-mode)\n(35)  ipb98(y,3) (h-mode)\n(36)  ipb98(y,4) (h-mode)\n(37)  iss95 (stellarator)\n(38)  iss04 (stellarator)\n(39)  ds03 (h-mode)') ,
	 ('int', 'iscrp', 'Switch for plasma-first wall clearances:', 'Switch for plasma-first wall clearances:\n= 0 use 10% of rminor;\n= 1 use input (scrapli and scraplo)') ,
	 ('int', 'ishape', 'Switch for plasma cross-sectional shape calculation:', 'Switch for plasma cross-sectional shape calculation:\n= 0 use input kappa, triang to calculate 95% values;\n= 1 scale qlim, kappa, triang with aspect ratio (st);\n= 2 set kappa to the natural elongation value (zohm iter scaling),\ntriang input;\n= 3 set kappa to the natural elongation value (zohm iter scaling),\ntriang95 input;\n= 4 use input kappa95, triang95 to calculate separatrix values') ,
	 ('int', 'itart', 'Switch for spherical tokamak (st) models:', 'Switch for spherical tokamak (st) models:\n= 0 use conventional aspect ratio models;\n= 1 use spherical tokamak models') ,
	 ('int', 'iwalld', 'Switch for neutron wall load calculation:', 'Switch for neutron wall load calculation:\n= 1 use scaled plasma surface area;\n= 2 use first wall area directly') ,
	 ('float', 'kappa', 'Plasma separatrix elongation (calculated if ishape > 0)', 'Plasma separatrix elongation (calculated if ishape > 0)') ,
	 ('float', 'kappa95', 'Plasma elongation at 95% surface (calculated if ishape < 4)', 'Plasma elongation at 95% surface (calculated if ishape < 4)') ,
	 ('float', 'q', 'Safety factor at plasma edge (q-psi) (iteration variable 18):', 'Safety factor at plasma edge (q-psi) (iteration variable 18):\nicurr = 2, q = mean safety factor qbar for divertors;\nicurr = 3,4, q = safety factor at 95% surface') ,
	 ('float', 'q0', 'Safety factor on axis', 'Safety factor on axis') ,
	 ('float', 'ralpne', 'Thermal alpha density / electron density', 'Thermal alpha density / electron density') ,
	 ('float', 'rli', 'Plasma normalised internal inductance;', 'Plasma normalised internal inductance;\ncalculated from alphaj if iprofile=1') ,
	 ('float', 'rmajor', 'Plasma major radius (m) (iteration variable 3)', 'Plasma major radius (m) (iteration variable 3)') ,
	 ('float', 'rnbeam', 'Hot beam density / n_e (iteration variable 7)', 'Hot beam density / n_e (iteration variable 7)') ,
	 ('int', 'snull', 'Switch for single null / double null plasma:', 'Switch for single null / double null plasma:\n= 0 for double null;\n= 1 for single null (diverted side down)') ,
	 ('float', 'ssync', 'Synchrotron wall reflectivity factor', 'Synchrotron wall reflectivity factor') ,
	 ('float', 'te', 'Volume averaged electron temperature (kev)', 'Volume averaged electron temperature (kev)\n(iteration variable 4)') ,
	 ('float', 'ti', 'Volume averaged ion temperature (kev);', 'Volume averaged ion temperature (kev);\nn.b. calculated from te if tratio > 0.0') ,
	 ('float', 'tratio', 'Ion temperature / electron temperature;', 'Ion temperature / electron temperature;\nused to calculate ti if tratio > 0.0') ,
	 ('float', 'triang', 'Plasma separatrix triangularity (calculated if ishape=1, 3 or 4)', 'Plasma separatrix triangularity (calculated if ishape=1, 3 or 4)') ,
	 ('float', 'triang95', 'Plasma triangularity at 95% surface (calculated if ishape < 3)', 'Plasma triangularity at 95% surface (calculated if ishape < 3)') ,
	 ('int', 'zfear', 'High-z impurity switch; 0=iron, 1=argon', 'High-z impurity switch; 0=iron, 1=argon\n(if imprad_model=1, only used in neutral beam stopping calc.)') ,
	]
GUI_MODULE['Pulse'] = [
	 ('float', 'afw', 'Inner radius of each first wall structural tube (m)', 'Inner radius of each first wall structural tube (m)') ,
	 ('float', 'bctmp', 'First wall bulk coolant temperature (c)', 'First wall bulk coolant temperature (c)') ,
	 ('float', 'coolp', 'First wall coolant pressure (pa)', 'First wall coolant pressure (pa)') ,
	 ('float', 'dtstor', 'Maximum allowable temperature change in stainless', 'Maximum allowable temperature change in stainless\nsteel thermal storage block (k) (istore=3)') ,
	 ('int', 'istore', 'Switch for thermal storage method:', 'Switch for thermal storage method:\n= 1 option 1 of electrowatt report, aea fus 205;\n= 2 option 2 of electrowatt report, aea fus 205;\n= 3 stainless steel block') ,
	 ('int', 'itcycl', 'Switch for first wall axial stress model:', 'Switch for first wall axial stress model:\n= 1 total axial constraint, no bending;\n= 2 no axial constraint, no bending;\n= 3 no axial constraint, bending') ,
	 ('int', 'lpulse', 'Switch for reactor model:', 'Switch for reactor model:\n= 0 continuous operation;\n= 1 pulsed operation') ,
	 ('float', 'tmprse', 'First wall coolant temperature rise (c)', 'First wall coolant temperature rise (c)') ,
	]
GUI_MODULE['Scan Module'] = [
	 ('int', 'isweep', 'Number of scan points to calculate', 'Number of scan points to calculate') ,
	 ('int', 'nsweep', 'Switch denoting quantity to scan:', 'Switch denoting quantity to scan:\n1  aspect\n2  hldivlim\n3  pnetelin\n4  hfact\n5  oacdcp\n6  walalw\n7  beamfus0\n8  fqval\n9  te\n10 boundu(15: fvs)\n11 dnbeta\n12 bscfmax (use negative values only)\n13 boundu(10: hfact)\n14 fiooic\n15 fjprot\n16 rmajor\n17 bmxlim\n18 gammax\n19 boundl(16: ohcth)\n20 tbrnmn\n21 sigpfalw\n22 cfactr (n.b. requires iavail=0)\n23 boundu(72: fipir)\n24 powfmax\n25 kappa\n26 triang\n27 tbrmin (for blktmodel > 0 only)\n28 bt\n29 coreradius\n30 fimpvar') ,
	 ('array', 'sweep', 'Actual values to use in scan', 'Actual values to use in scan') ,
	]
GUI_MODULE['Stellarator'] = [
	 ('int', 'istell', 'Switch for stellarator option', 'Switch for stellarator option\n(set via <code>device.dat</code>):\n= 0 use tokamak, rfp or ife model;\n= 1 use stellarator model') ,
	 ('float', 'bmn', 'Relative radial field perturbation', 'Relative radial field perturbation') ,
	 ('float', 'f_asym', 'Divertor heat load peaking factor', 'Divertor heat load peaking factor') ,
	 ('float', 'f_rad', 'Radiated power fraction in sol', 'Radiated power fraction in sol') ,
	 ('float', 'f_w', 'Island size fraction factor', 'Island size fraction factor') ,
	 ('float', 'fdivwet', 'Wetted fraction of the divertor area', 'Wetted fraction of the divertor area') ,
	 ('float', 'flpitch', 'Field line pitch (rad)', 'Field line pitch (rad)') ,
	 ('float', 'iotabar', 'Rotational transform (reciprocal of tokamak q)', 'Rotational transform (reciprocal of tokamak q)\nfor stellarator confinement time scaling laws') ,
	 ('int', 'isthtr', 'Switch for stellarator auxiliary heating method:', 'Switch for stellarator auxiliary heating method:\n= 1 electron cyclotron resonance heating;\n= 2 lower hybrid heating;\n= 3 neutral beam injection') ,
	 ('int', 'm_res', 'Poloidal resonance number', 'Poloidal resonance number') ,
	 ('int', 'n_res', 'Toroidal resonance number', 'Toroidal resonance number') ,
	 ('float', 'shear', 'Magnetic shear, derivative of iotabar', 'Magnetic shear, derivative of iotabar') ,
	 ('string', 'vmec_info_file', 'File containing general vmec settings', 'File containing general vmec settings') ,
	 ('string', 'vmec_rmn_file', 'File containing plasma boundary r(m,n)', 'File containing plasma boundary r(m,n)\nfourier components') ,
	 ('string', 'vmec_zmn_file', 'File containing plasma boundary z(m,n)', 'File containing plasma boundary z(m,n)\nfourier components') ,
	]
GUI_MODULE['Tfcoil'] = [
	 ('float', 'bcritsc', 'Upper critical field (t) for nb3sn superconductor', 'Upper critical field (t) for nb3sn superconductor\nat zero temperature and strain (isumattf=4, =bc20m)') ,
	 ('float', 'casthi', 'Inboard tf coil case inner (plasma side) thickness (m)', 'Inboard tf coil case inner (plasma side) thickness (m)\n(calculated for stellarators)') ,
	 ('float', 'casths', 'Inboard tf coil sidewall case thickness (m)', 'Inboard tf coil sidewall case thickness (m)\n(calculated for stellarators)') ,
	 ('float', 'cdtfleg', 'Tf leg overall current density (a/m2)', 'Tf leg overall current density (a/m2)\n(resistive coils only) (iteration variable 24)') ,
	 ('float', 'cpttf', 'Tf coil current per turn (a)', 'Tf coil current per turn (a)\n(calculated for stellarators)\n(iteration variable 60)') ,
	 ('float', 'csutf', 'Ultimate strength of case (pa)', 'Ultimate strength of case (pa)\n(default value from ddd11-2 v2 2 (2009))') ,
	 ('float', 'csytf', 'Yield strength of case (pa)', 'Yield strength of case (pa)\n(default value from ddd11-2 v2 2 (2009))') ,
	 ('float', 'dcase', 'Density of coil case (kg/m3)', 'Density of coil case (kg/m3)') ,
	 ('array', 'dcond', 'Density of superconductor type given by isumattf or isumatpf (kg/m3)', 'Density of superconductor type given by isumattf or isumatpf (kg/m3)') ,
	 ('float', 'dcondins', 'Density of conduit + ground-wall insulation (kg/m3)', 'Density of conduit + ground-wall insulation (kg/m3)') ,
	 ('float', 'dcopper', 'Density of copper (kg/m3)', 'Density of copper (kg/m3)') ,
	 ('float', 'eyins', "Insulator young's modulus (pa)", "Insulator young's modulus (pa)\n(default value from ddd11-2 v2 2 (2009))") ,
	 ('float', 'eystl', "Steel case young's modulus (pa)", "Steel case young's modulus (pa)\n(default value from ddd11-2 v2 2 (2009))") ,
	 ('float', 'eywp', "Winding pack young's modulus (pa)", "Winding pack young's modulus (pa)") ,
	 ('float', 'farc4tf', 'Factor to size height of point 4 on tf coil', 'Factor to size height of point 4 on tf coil') ,
	 ('float', 'fcutfsu', 'Copper fraction of cable conductor', 'Copper fraction of cable conductor\n(iteration variable 59)') ,
	 ('float', 'fhts', 'Technology adjustment factor for critical current density fit', 'Technology adjustment factor for critical current density fit\nfor isumattf=2 bi-2212 superconductor, to describe the level\nof technology assumed (i.e. to account for stress, fatigue,\nradiation, ac losses, joints or manufacturing variations;\n1.0 would be very optimistic)') ,
	 ('int', 'isumattf', 'Switch for superconductor material in tf coils:', 'Switch for superconductor material in tf coils:\n= 1 iter nb3sn critical surface model with standard\niter parameters;\n= 2 bi-2212 high temperature superconductor (range of\nvalidity t < 20k, adjusted field b < 104 t, b > 6 t);\n= 3 nbti;\n= 4 iter nb3sn model with user-specified parameters') ,
	 ('int', 'itfsup', 'Switch for tf coil conductor model:', 'Switch for tf coil conductor model:\n= 0 copper;\n= 1 superconductor') ,
	 ('float', 'jbus', 'Bussing current density (a/m2)', 'Bussing current density (a/m2)') ,
	 ('float', 'oacdcp', 'Overall current density in tf coil inboard legs (a/m2)', 'Overall current density in tf coil inboard legs (a/m2)\n(iteration variable 12)') ,
	 ('float', 'poisson', "Poisson's ratio for tf stress calculation", "Poisson's ratio for tf stress calculation\n(assumed constant over entire coil)") ,
	 ('float', 'prp', 'Ratio of the cross-sectional area of the radial plates', "Ratio of the cross-sectional area of the radial plates\n+ inter-turn steel caps to the whole winding pack's\ncross-sectional area (iteration variable 101)") ,
	 ('float', 'ripmax', 'Maximum allowable toroidal field ripple amplitude', 'Maximum allowable toroidal field ripple amplitude\nat plasma edge (%)') ,
	 ('float', 'strncon', 'Strain in superconductor material', 'Strain in superconductor material\n(used in iter nb3sn critical surface model)') ,
	 ('float', 'tcritsc', 'Critical temperature (k) for superconductor', 'Critical temperature (k) for superconductor\nat zero field and strain (isumattf=4, =tc0m)') ,
	 ('float', 'tdmptf', 'Dump time for tf coil (s)', 'Dump time for tf coil (s)\n(iteration variable 56)') ,
	 ('int', 'tfc_model', 'Switch for tf coil magnet stress model:', 'Switch for tf coil magnet stress model:\n= 0 simple model (solid copper coil)\n= 1 ccfe two-layer stress model; superconductor') ,
	 ('float', 'tflegres', 'Resistivity of a tf coil leg (ohm-m)', 'Resistivity of a tf coil leg (ohm-m)') ,
	 ('float', 'tfno', 'Number of tf coils (default = 50 for stellarators)', 'Number of tf coils (default = 50 for stellarators)') ,
	 ('float', 'tftmp', 'Peak tf coil helium coolant temperature (k)', 'Peak tf coil helium coolant temperature (k)') ,
	 ('float', 'thicndut', 'Conduit insulation thickness (m)', 'Conduit insulation thickness (m)') ,
	 ('float', 'thkcas', 'Inboard tf coil case outer (non-plasma side) thickness (m)', 'Inboard tf coil case outer (non-plasma side) thickness (m)\n(iteration variable 57)\n(calculated for stellarators)') ,
	 ('float', 'thwcndut', 'Tf coil conduit case thickness (m)', 'Tf coil conduit case thickness (m)\n(iteration variable 58)') ,
	 ('float', 'tinstf', 'Ground wall insulation thickness (m)', 'Ground wall insulation thickness (m)\n(calculated for stellarators)') ,
	 ('float', 'tmargmin', 'Minimum allowable temperature margin (k)', 'Minimum allowable temperature margin (k)\n(iteration variable 55)') ,
	 ('float', 'tmaxpro', 'Maximum temp rise during a quench for protection (k)', 'Maximum temp rise during a quench for protection (k)') ,
	 ('float', 'tmpcry', 'Cryostat temperature for cryogenic plant power calculation (k)', 'Cryostat temperature for cryogenic plant power calculation (k)') ,
	 ('float', 'vdalw', 'Max voltage across tf coil during quench (kv)', 'Max voltage across tf coil during quench (kv)\n(iteration variable 52)') ,
	 ('float', 'vftf', 'Coolant fraction of tf coil leg (itfsup=0)', 'Coolant fraction of tf coil leg (itfsup=0)\nor of tf coil cable space (itfsup=1)') ,
	 ('float', 'drtop', 'Centrepost taper maximum radius adjustment (m)', 'Centrepost taper maximum radius adjustment (m)') ,
	 ('float', 'dztop', 'Centrepost taper height adjustment (m)', 'Centrepost taper height adjustment (m)') ,
	 ('float', 'etapump', 'Centrepost coolant pump efficiency', 'Centrepost coolant pump efficiency') ,
	 ('float', 'fcoolcp', 'Coolant fraction of tf coil inboard legs', 'Coolant fraction of tf coil inboard legs\n(iteration variable 23)') ,
	 ('float', 'frhocp', 'Centrepost resistivity enhancement factor', 'Centrepost resistivity enhancement factor') ,
	 ('float', 'ptempalw', 'Maximum peak centrepost temperature (c)', 'Maximum peak centrepost temperature (c)\n(constraint equation 44)') ,
	 ('float', 'rcool', 'Average radius of coolant channel (m)', 'Average radius of coolant channel (m)\n(iteration variable 69)') ,
	 ('float', 'tcoolin', 'Centrepost coolant inlet temperature (c)', 'Centrepost coolant inlet temperature (c)') ,
	 ('float', 'tcpav', 'Average temp of tf coil inboard leg conductor (c)', 'Average temp of tf coil inboard leg conductor (c)\n(resistive coils) (iteration variable 20)') ,
	 ('float', 'vcool', 'Max centrepost coolant flow speed at midplane (m/s)', 'Max centrepost coolant flow speed at midplane (m/s)\n(iteration variable 70)') ,
	]
GUI_MODULE['Times'] = [
	 ('float', 'tburn', 'Burn time (s) (calculated if lpulse=1)', 'Burn time (s) (calculated if lpulse=1)') ,
	 ('float', 'tdwell', 'Time between pulses in a pulsed reactor (s)', 'Time between pulses in a pulsed reactor (s)\n(iteration variable 17)') ,
	 ('float', 'theat', 'Heating time, after current ramp up (s)', 'Heating time, after current ramp up (s)') ,
	 ('float', 'tohs', 'Plasma current ramp-up time for current initiation (s)', 'Plasma current ramp-up time for current initiation (s)\n(but calculated if lpulse=0)\n(iteration variable 65)') ,
	 ('float', 'tohsin', 'Switch for plasma current ramp-up time (if lpulse=0):', 'Switch for plasma current ramp-up time (if lpulse=0):\n= 0, tohs = tramp = tqnch = ip(ma)/0.5;\n<>0, tohs = tohsin; tramp, tqnch are input') ,
	 ('float', 'tqnch', 'Shut down time for pf coils (s); if pulsed, = tohs', 'Shut down time for pf coils (s); if pulsed, = tohs') ,
	 ('float', 'tramp', 'Initial pf coil charge time (s); if pulsed, = tohs', 'Initial pf coil charge time (s); if pulsed, = tohs') ,
	]
GUI_MODULE['Vacuum'] = [
	 ('int', 'ntype', 'Switch for vacuum pump type:', 'Switch for vacuum pump type:\n= 0 for turbomolecular pump (magnetic bearing)\nwith speed of 2.0 m3/s\n(1.95 for n2, 1.8 for he, 1.8 for dt);\n= 1 for compound cryopump with nominal speed of 10.0 m3/s\n(9.0 for n2, 5.0 for he and 25.0 for dt)') ,
	 ('float', 'pbase', 'Base pressure (pa)', 'Base pressure (pa)') ,
	 ('float', 'prdiv', 'Divertor chamber pressure during burn (pa)', 'Divertor chamber pressure during burn (pa)') ,
	 ('float', 'rat', 'Plasma chamber wall outgassing rate (pa-m/s)', 'Plasma chamber wall outgassing rate (pa-m/s)') ,
	 ('float', 'tn', 'Neutral gas temperature in chamber (k)', 'Neutral gas temperature in chamber (k)') ,
	]

#List that sets ixc ordering
GUI_LABLXC = OrderedDict()
GUI_LABLXC[1]                  = ('aspect', 'Aspect ratio', 'Aspect ratio (iteration variable 1)')
GUI_LABLXC[2]                  = ('bt', 'Toroidal field on axis (t)', 'Toroidal field on axis (t) (iteration variable 2)')
GUI_LABLXC[3]                  = ('rmajor', 'Plasma major radius (m)', 'Plasma major radius (m) (iteration variable 3)')
GUI_LABLXC[4]                  = ('te', 'Volume averaged electron temperature (kev)', 'Volume averaged electron temperature (kev)\n(iteration variable 4)')
GUI_LABLXC[5]                  = ('beta', 'Total plasma beta', 'Total plasma beta (iteration variable 5)')
GUI_LABLXC[6]                  = ('dene', 'Electron density (/m3)', 'Electron density (/m3) (iteration variable 6)')
GUI_LABLXC[7]                  = ('rnbeam', 'Hot beam density / n_e', 'Hot beam density / n_e (iteration variable 7)')
GUI_LABLXC[8]                  = ('fbeta', 'F-value for epsilon beta-poloidal', 'F-value for epsilon beta-poloidal\n(constraint equation 6, iteration variable 8)')
GUI_LABLXC[9]                  = ('fdene', 'F-value for density limit', 'F-value for density limit\n(constraint equation 5, iteration variable 9)')
GUI_LABLXC[10]                 = ('hfact', 'H factor on energy confinement times', 'H factor on energy confinement times (iteration variable 10)')
GUI_LABLXC[11]                 = ('pheat', 'Heating power not used for current drive (mw)', 'Heating power not used for current drive (mw)\n(iteration variable 11)')
GUI_LABLXC[12]                 = ('oacdcp', 'Overall current density in tf coil inboard legs (a/m2)', 'Overall current density in tf coil inboard legs (a/m2)\n(iteration variable 12)')
GUI_LABLXC[13]                 = ('tfcth', 'Inboard tf coil thickness, (centrepost for st) (m)', 'Inboard tf coil thickness, (centrepost for st) (m)\n(calculated for stellarators)\n(iteration variable 13)')
GUI_LABLXC[14]                 = ('fwalld', 'F-value for minimum wall load', 'F-value for minimum wall load\n(constraint equation 8, iteration variable 14)')
GUI_LABLXC[15]                 = ('fvs', 'F-value for flux-swing (v-s) requirement', 'F-value for flux-swing (v-s) requirement\n(constraint equation 12, iteration variable 15)')
GUI_LABLXC[16]                 = ('ohcth', 'Central solenoid thickness (m)', 'Central solenoid thickness (m)\n(iteration variable 16)')
GUI_LABLXC[17]                 = ('tdwell', 'Time between pulses in a pulsed reactor (s)', 'Time between pulses in a pulsed reactor (s)\n(iteration variable 17)')
GUI_LABLXC[18]                 = ('q', 'Safety factor at plasma edge (q-psi)', 'Safety factor at plasma edge (q-psi) (iteration variable 18):\nicurr = 2, q = mean safety factor qbar for divertors;\nicurr = 3,4, q = safety factor at 95% surface')
GUI_LABLXC[19]                 = ('enbeam', 'Neutral beam energy (kev)', 'Neutral beam energy (kev) (iteration variable 19)')
GUI_LABLXC[20]                 = ('tcpav', 'Average temp of tf coil inboard leg conductor (c)', 'Average temp of tf coil inboard leg conductor (c)\n(resistive coils) (iteration variable 20)')
GUI_LABLXC[21]                 = ('ftburn', 'F-value for minimum burn time', 'F-value for minimum burn time\n(constraint equation 13, iteration variable 21)')
GUI_LABLXC[22]                 = ('tbrnmn', 'Minimum burn time (s)', 'Minimum burn time (s)\n(constraint equation 13)')
GUI_LABLXC[23]                 = ('fcoolcp', 'Coolant fraction of tf coil inboard legs', 'Coolant fraction of tf coil inboard legs\n(iteration variable 23)')
GUI_LABLXC[24]                 = ('cdtfleg', 'Tf leg overall current density (a/m2)', 'Tf leg overall current density (a/m2)\n(resistive coils only) (iteration variable 24)')
GUI_LABLXC[25]                 = ('fpnetel', 'F value for net electric power', 'F value for net electric power\n(constraint equation 16, iteration variable 25)')
GUI_LABLXC[26]                 = ('ffuspow', 'F-value for maximum fusion power', 'F-value for maximum fusion power\n(constraint equation 9, iteration variable 26)')
GUI_LABLXC[27]                 = ('fhldiv', 'F-value for divertor heat load', 'F-value for divertor heat load\n(constraint equation 18, iteration variable 27)')
GUI_LABLXC[28]                 = ('fradpwr', 'F-value for core radiation power limit', 'F-value for core radiation power limit\n(constraint equation 17, iteration variable 28)')
GUI_LABLXC[29]                 = ('bore', 'Central solenoid inboard radius (m)', 'Central solenoid inboard radius (m)\n(iteration variable 29)')
GUI_LABLXC[30]                 = ('fmva', 'F-value for maximum mva', 'F-value for maximum mva\n(constraint equation 19, iteration variable 30)')
GUI_LABLXC[31]                 = ('gapomin', 'Minimum gap between outboard vacuum vessel and tf coil (m)', 'Minimum gap between outboard vacuum vessel and tf coil (m)\n(iteration variable 31)')
GUI_LABLXC[32]                 = ('frminor', 'F-value for minor radius limit', 'F-value for minor radius limit\n(constraint equation 21, iteration variable 32)')
GUI_LABLXC[33]                 = ('fportsz', 'F-value for neutral beam tangency radius limit', 'F-value for neutral beam tangency radius limit\n(constraint equation 20, iteration variable 33)')
GUI_LABLXC[34]                 = ('fdivcol', 'F-value for divertor collisionality', 'F-value for divertor collisionality\n(constraint equation 22, iteration variable 34)')
GUI_LABLXC[35]                 = ('fpeakb', 'F-value for maximum toroidal field', 'F-value for maximum toroidal field\n(constraint equation 25, iteration variable 35)')
GUI_LABLXC[36]                 = ('fbetatry', 'F-value for beta limit', 'F-value for beta limit\n(constraint equation 24, iteration variable 36)')
GUI_LABLXC[37]                 = ('coheof', 'Central solenoid overall current density at end of flat-top (a/m2)', 'Central solenoid overall current density at end of flat-top (a/m2)\n(iteration variable 37)')
GUI_LABLXC[38]                 = ('fjohc', 'F-value for central solenoid current at end-of-flattop', 'F-value for central solenoid current at end-of-flattop\n(constraint equation 26, iteration variable 38)')
GUI_LABLXC[39]                 = ('fjohc0', 'F-value for central solenoid current at beginning of pulse', 'F-value for central solenoid current at beginning of pulse\n(constraint equation 27, iteration variable 39)')
GUI_LABLXC[40]                 = ('fgamcd', 'F-value for current drive gamma', 'F-value for current drive gamma\n(constraint equation 37, iteration variable 40)')
GUI_LABLXC[41]                 = ('fcohbop', 'Ratio of central solenoid overall current density at', 'Ratio of central solenoid overall current density at\nbeginning of pulse / end of flat-top\n(iteration variable 41)')
GUI_LABLXC[42]                 = ('gapoh', 'Gap between central solenoid and tf coil', 'Gap between central solenoid and tf coil\n(iteration variable 42)')
GUI_LABLXC[43]                 = ('cfe0', 'Seeded high-z impurity fraction (n_highz / n_e)', 'Seeded high-z impurity fraction (n_highz / n_e)\n(imprad_model=0 only) (iteration variable 43)')
GUI_LABLXC[44]                 = ('fvsbrnni', 'Fraction of the plasma current produced by', 'Fraction of the plasma current produced by\nnon-inductive means (iteration variable 44)')
GUI_LABLXC[45]                 = ('fqval', 'F-value for q', 'F-value for q\n(constraint equation 28, iteration variable 45)')
GUI_LABLXC[46]                 = ('fpinj', 'F-value for injection power', 'F-value for injection power\n(constraint equation 30, iteration variable 46)')
GUI_LABLXC[47]                 = ('feffcd', 'Current drive efficiency fudge factor', 'Current drive efficiency fudge factor (iteration variable 47)')
GUI_LABLXC[48]                 = ('fstrcase', 'F-value for tf coil case stress', 'F-value for tf coil case stress\n(constraint equation 31, iteration variable 48)')
GUI_LABLXC[49]                 = ('fstrcond', 'F-value for tf coil conduit stress', 'F-value for tf coil conduit stress\n(constraint equation 32, iteration variable 49)')
GUI_LABLXC[50]                 = ('fiooic', 'F-value for tf coil operating current / critical', 'F-value for tf coil operating current / critical\ncurrent ratio\n(constraint equation 33, iteration variable 50)')
GUI_LABLXC[51]                 = ('fvdump', 'F-value for dump voltage', 'F-value for dump voltage\n(constraint equation 34, iteration variable 51)')
GUI_LABLXC[52]                 = ('vdalw', 'Max voltage across tf coil during quench (kv)', 'Max voltage across tf coil during quench (kv)\n(iteration variable 52)')
GUI_LABLXC[53]                 = ('fjprot', 'F-value for tf coil winding pack current density', 'F-value for tf coil winding pack current density\n(constraint equation 35, iteration variable 53)')
GUI_LABLXC[54]                 = ('ftmargtf', 'F-value for tf coil temperature margin', 'F-value for tf coil temperature margin\n(constraint equation 36, iteration variable 54)')
GUI_LABLXC[55]                 = ('tmargmin', 'Minimum allowable temperature margin (k)', 'Minimum allowable temperature margin (k)\n(iteration variable 55)')
GUI_LABLXC[56]                 = ('tdmptf', 'Dump time for tf coil (s)', 'Dump time for tf coil (s)\n(iteration variable 56)')
GUI_LABLXC[57]                 = ('thkcas', 'Inboard tf coil case outer (non-plasma side) thickness (m)', 'Inboard tf coil case outer (non-plasma side) thickness (m)\n(iteration variable 57)\n(calculated for stellarators)')
GUI_LABLXC[58]                 = ('thwcndut', 'Tf coil conduit case thickness (m)', 'Tf coil conduit case thickness (m)\n(iteration variable 58)')
GUI_LABLXC[59]                 = ('fcutfsu', 'Copper fraction of cable conductor', 'Copper fraction of cable conductor\n(iteration variable 59)')
GUI_LABLXC[60]                 = ('cpttf', 'Tf coil current per turn (a)', 'Tf coil current per turn (a)\n(calculated for stellarators)\n(iteration variable 60)')
GUI_LABLXC[61]                 = ('gapds', 'Gap between inboard vacuum vessel and tf coil (m)', 'Gap between inboard vacuum vessel and tf coil (m)\n(iteration variable 61)')
GUI_LABLXC[62]                 = ('fdtmp', 'F-value for first wall coolant temperature rise', 'F-value for first wall coolant temperature rise\n(constraint equation 38, iteration variable 62)')
GUI_LABLXC[63]                 = ('ftpeak', 'F-value for first wall peak temperature', 'F-value for first wall peak temperature\n(constraint equation 39, iteration variable 63)')
GUI_LABLXC[64]                 = ('fauxmn', 'F-value for minimum auxiliary power', 'F-value for minimum auxiliary power\n(constraint equation 40, iteration variable 64)')
GUI_LABLXC[65]                 = ('tohs', 'Plasma current ramp-up time for current initiation (s)', 'Plasma current ramp-up time for current initiation (s)\n(but calculated if lpulse=0)\n(iteration variable 65)')
GUI_LABLXC[66]                 = ('ftohs', 'F-value for plasma current ramp-up time', 'F-value for plasma current ramp-up time\n(constraint equation 41, iteration variable 66)')
GUI_LABLXC[67]                 = ('ftcycl', 'F-value for cycle time', 'F-value for cycle time\n(constraint equation 42, iteration variable 67)')
GUI_LABLXC[68]                 = ('fptemp', 'F-value for peak centrepost temperature', 'F-value for peak centrepost temperature\n(constraint equation 44, iteration variable 68)')
GUI_LABLXC[69]                 = ('rcool', 'Average radius of coolant channel (m)', 'Average radius of coolant channel (m)\n(iteration variable 69)')
GUI_LABLXC[70]                 = ('vcool', 'Max centrepost coolant flow speed at midplane (m/s)', 'Max centrepost coolant flow speed at midplane (m/s)\n(iteration variable 70)')
GUI_LABLXC[71]                 = ('fq', 'F-value for edge safety factor', 'F-value for edge safety factor\n(constraint equation 45, iteration variable 71)')
GUI_LABLXC[72]                 = ('fipir', 'F-value for ip/irod limit', 'F-value for ip/irod limit\n(constraint equation 46, iteration variable 72)')
GUI_LABLXC[73]                 = ('scrapli', 'Gap between plasma and first wall, inboard side (m)', 'Gap between plasma and first wall, inboard side (m)\n(used if iscrp=1) (iteration variable 73)')
GUI_LABLXC[74]                 = ('scraplo', 'Gap between plasma and first wall, outboard side (m)', 'Gap between plasma and first wall, outboard side (m)\n(used if iscrp=1) (iteration variable 74)')
GUI_LABLXC[75]                 = ('tfootfi', 'Tf coil outboard leg / inboard leg radial thickness', 'Tf coil outboard leg / inboard leg radial thickness\nratio (itfsup=0 only)\n(iteration variable 75)')
GUI_LABLXC[76]                 = ('frfptf', 'F-value for rfp tf coil toroidal thickness', 'F-value for rfp tf coil toroidal thickness\n(constraint equation 47, iteration variable 76)')
GUI_LABLXC[79]                 = ('fbetap', 'F-value for poloidal beta', 'F-value for poloidal beta\n(constraint equation 48, iteration variable 79)')
GUI_LABLXC[80]                 = ('frfpf', 'F-value for rfp reversal parameter', 'F-value for rfp reversal parameter\n(constraint equation 49, iteration variable 80)')
GUI_LABLXC[87]                 = ('helecmw', 'Electrical power required for h production (mw)', 'Electrical power required for h production (mw)\n(iteration variable 87)')
GUI_LABLXC[88]                 = ('hthermmw', 'Thermal power required for h production (mw)', 'Thermal power required for h production (mw)\n(iteration variable 88)\n(n.b. calculated for ihplant=1,2,3)')
GUI_LABLXC[89]                 = ('ftbr', 'F-value for minimum tritium breeding ratio (blktmodel>0)', 'F-value for minimum tritium breeding ratio (blktmodel>0)\n(constraint equation 52, iteration variable 89)')
GUI_LABLXC[90]                 = ('blbuith', 'Inboard blanket breeding zone thickness (m)', 'Inboard blanket breeding zone thickness (m)\n(blktmodel>0)\n(iteration variable 90)')
GUI_LABLXC[91]                 = ('blbuoth', 'Outboard blanket breeding zone thickness (m)', 'Outboard blanket breeding zone thickness (m)\n(blktmodel>0)\n(iteration variable 91)')
GUI_LABLXC[92]                 = ('fflutf', 'F-value for neutron fluence on tf coil', 'F-value for neutron fluence on tf coil\n(constraint equation 53, iteration variable 92)')
GUI_LABLXC[93]                 = ('shldith', 'Inboard shield thickness (m)', 'Inboard shield thickness (m)\n(iteration variable 93)')
GUI_LABLXC[94]                 = ('shldoth', 'Outboard shield thickness (m)', 'Outboard shield thickness (m)\n(iteration variable 94)')
GUI_LABLXC[95]                 = ('fptfnuc', 'F value for maximum tf coil nuclear heating', 'F value for maximum tf coil nuclear heating\n(constraint equation 54, iteration variable 95)')
GUI_LABLXC[96]                 = ('fvvhe', 'F-value for vacuum vessel he concentration limit', 'F-value for vacuum vessel he concentration limit\n(blktmodel>0)\n(constraint equation 55, iteration variable 96)')
GUI_LABLXC[97]                 = ('fpsepr', 'F-value for maximum psep/r limit', 'F-value for maximum psep/r limit\n(constraint equation 56, iteration variable 97)')
GUI_LABLXC[98]                 = ('li6enrich', 'Lithium-6 enrichment of breeding material (%)', 'Lithium-6 enrichment of breeding material (%)\n(blktmodel>0)')
GUI_LABLXC[101]                = ('prp', 'Ratio of the cross-sectional area of the radial plates', "Ratio of the cross-sectional area of the radial plates\n+ inter-turn steel caps to the whole winding pack's\ncross-sectional area (iteration variable 101)")
GUI_LABLXC[102]                = ('fimpvar', 'Impurity fraction to be used as fimp(impvar)', 'Impurity fraction to be used as fimp(impvar)\n(iteration variable 102)')

#List that sets icc ordering
GUI_LABLCC = OrderedDict()
GUI_LABLCC[1]                  = 'Beta (consistency equation)'
GUI_LABLCC[2]                  = 'Global power balance (consistency equation)'
GUI_LABLCC[3]                  = 'Ion power balance'
GUI_LABLCC[4]                  = 'Electron power balance'
GUI_LABLCC[5]                  = 'Density upper limit'
GUI_LABLCC[6]                  = '(epsilon x beta poloidal) upper limit'
GUI_LABLCC[7]                  = 'Beam ion density (nbi) (consistency equation)'
GUI_LABLCC[8]                  = 'Neutron wall load upper limit'
GUI_LABLCC[9]                  = 'Fusion power upper limit'
GUI_LABLCC[10]                 = 'Toroidal field 1/r (consistency equation)'
GUI_LABLCC[11]                 = 'Radial build (consistency equation)'
GUI_LABLCC[12]                 = 'Volt second lower limit'
GUI_LABLCC[13]                 = 'Burn time lower limit (pulse)'
GUI_LABLCC[14]                 = 'Energy of neutral beam (nbi) (consistency equation)'
GUI_LABLCC[16]                 = 'Net electric power lower limit'
GUI_LABLCC[17]                 = 'Radiation power upper limit'
GUI_LABLCC[18]                 = 'Divertor heat load upper limit'
GUI_LABLCC[19]                 = 'Mva upper limit'
GUI_LABLCC[20]                 = 'Neutral beam tangency radius upper limit (nbi)'
GUI_LABLCC[21]                 = 'Plasma minor radius lower limit'
GUI_LABLCC[22]                 = 'Divertor collisionality upper limit'
GUI_LABLCC[24]                 = 'Beta upper limit'
GUI_LABLCC[25]                 = 'Peak toroidal field upper limit'
GUI_LABLCC[26]                 = 'Oh coil eof current density upper limit'
GUI_LABLCC[27]                 = 'Oh coil bop current density upper limit'
GUI_LABLCC[28]                 = 'Fusion gain q lower limit'
GUI_LABLCC[29]                 = 'Inboard radial build consistency'
GUI_LABLCC[30]                 = 'Injection power upper limit'
GUI_LABLCC[31]                 = 'Tf coil case stress upper limit (sctf)'
GUI_LABLCC[32]                 = 'Tf coil conduit stress upper limit (sctf)'
GUI_LABLCC[33]                 = 'I_op / i_critical (tf coil) (sctf)'
GUI_LABLCC[34]                 = 'Dump voltage upper limit (sctf)'
GUI_LABLCC[35]                 = 'J_winding pack/j_protection upper limit (sctf)'
GUI_LABLCC[36]                 = 'Tf coil temperature margin lower limit (sctf)'
GUI_LABLCC[37]                 = 'Current drive gamma upper limit'
GUI_LABLCC[38]                 = 'First wall coolant temperature rise upper limit (pulse)'
GUI_LABLCC[39]                 = 'First wall peak temperature upper limit (pulse)'
GUI_LABLCC[40]                 = 'Start-up injection power lower limit (pulse)'
GUI_LABLCC[41]                 = 'Plasma current ramp-up time lower limit (pulse)'
GUI_LABLCC[42]                 = 'Cycle time lower limit (pulse)'
GUI_LABLCC[43]                 = 'Average centrepost temperature'
GUI_LABLCC[44]                 = 'Peak centrepost temperature upper limit (tart)'
GUI_LABLCC[45]                 = 'Edge safety factor lower limit (tart)'
GUI_LABLCC[46]                 = 'Ip/irod upper limit (tart)'
GUI_LABLCC[47]                 = 'Tf coil toroidal thickness upper limit (rfp)'
GUI_LABLCC[48]                 = 'Poloidal beta upper limit'
GUI_LABLCC[49]                 = 'Rfp reversal parameter &lt; 0 (rfp)'
GUI_LABLCC[50]                 = 'Ife repetition rate upper limit (ife)'
GUI_LABLCC[51]                 = 'Startup volt-seconds consistency (pulse)'
GUI_LABLCC[52]                 = 'Tritium breeding ratio lower limit'
GUI_LABLCC[53]                 = 'Neutron fluence on tf coil upper limit'
GUI_LABLCC[54]                 = 'Peak tf coil nuclear heating upper limit'
GUI_LABLCC[55]                 = 'Vacuum vessel helium concentration upper limit'
GUI_LABLCC[56]                 = 'Pseparatrix/rmajor upper limit'
GUI_LABLCC[57]                 = 'Tf coil leg toroidal thickness lower limit (obsolete)'
GUI_LABLCC[58]                 = 'Tf coil leg radial thickness lower limit (obsolete)'
