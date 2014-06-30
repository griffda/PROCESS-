"""
Dictionaries of the PROCESS iteration variables
-
This has currently been created by hand and needs to be adjusted to
all changes in PROCESS manually until someone writes an automatic
extraction tool.

Authors: Hanni Lux (Hanni.Lux@ccfe.ac.uk)
        James Morris 

Date: 18 Nov. 2013 - up to date at PROCESS version 202
Update: 26/02/2014 - up to date at PROCESS version 238
Update: 26/03/2014 - JM added variable types and default parameters for mfile
                     PLOT.DAT routine.
Update: 09/04/2014 - HL added IFAIL_SUCCESS
Update: 01/05/2014 - PJK up to date at PROCESS version 260
Update: 01/05/2014 - PJK up to date at PROCESS version 263
Update: 08/05/2014 - PJK up to date at PROCESS version 266
Update: 14/05/2014 - PJK up to date at PROCESS version 272
Update: 20/05/2014 - PJK up to date at PROCESS version 279
Update: 02/06/2014 - PJK up to date at PROCESS version 283
Update: 04/06/2014 - PJK up to date at PROCESS version 285
Update: 16/06/2014 - PJK up to date at PROCESS version 288
Update: 16/06/2014 - PJK: r293
Update: 30/06/2014 - PJK: r303
"""

from collections import defaultdict


#ifail value of a successful process run
IFAIL_SUCCESS = 1

# default values for making a plot file from MFILE.DAT
PARAMETER_DEFAULTS = ["rmajor", "aspect", "rminor", "bt", "powfmw", "pnetelmw",
                      "te", "pdivt", "strtf1", "strtf2"]

#  Variable types
#  
#  Dictionary key is variable. Value is type:  
# 
#   int_array
#   int_variable
#   real_array
#   real_variable

DICT_VAR_TYPE = dict()

#  int_variables
DICT_VAR_TYPE['astr'] = 'int_variable'
DICT_VAR_TYPE['blktmodel'] = 'int_variable'
DICT_VAR_TYPE['breedmat'] = 'int_variable'
DICT_VAR_TYPE['bstr'] = 'int_variable'
DICT_VAR_TYPE['costr'] = 'int_variable'
DICT_VAR_TYPE['divdum'] = 'int_variable'
DICT_VAR_TYPE['estr'] = 'int_variable'
DICT_VAR_TYPE['fwbsshape'] = 'int_variable'
DICT_VAR_TYPE['gtscale'] = 'int_variable'
DICT_VAR_TYPE['hcdportsize'] = 'int_variable'
DICT_VAR_TYPE['iavail'] = 'int_variable'
DICT_VAR_TYPE['ibss'] = 'int_variable'
DICT_VAR_TYPE['iculbl'] = 'int_variable'
DICT_VAR_TYPE['icurr'] = 'int_variable'
DICT_VAR_TYPE['idensl'] = 'int_variable'
DICT_VAR_TYPE['iefrf'] = 'int_variable'
DICT_VAR_TYPE['ifalphap'] = 'int_variable'
DICT_VAR_TYPE['ifedrv'] = 'int_variable'
DICT_VAR_TYPE['ifetyp'] = 'int_variable'
DICT_VAR_TYPE['ifispact'] = 'int_variable'
DICT_VAR_TYPE['ifueltyp'] = 'int_variable'
DICT_VAR_TYPE['igeom'] = 'int_variable'
DICT_VAR_TYPE['ignite'] = 'int_variable'
DICT_VAR_TYPE['ihplant'] = 'int_variable'
DICT_VAR_TYPE['iinvqd'] = 'int_variable'
DICT_VAR_TYPE['imprad_model'] = 'int_variable'
DICT_VAR_TYPE['impvar'] = 'int_variable'
DICT_VAR_TYPE['iohcl'] = 'int_variable'
DICT_VAR_TYPE['ioptimz'] = 'int_variable'
DICT_VAR_TYPE['ipedestal'] = 'int_variable'
DICT_VAR_TYPE['ipfres'] = 'int_variable'
DICT_VAR_TYPE['ipnet'] = 'int_variable'
DICT_VAR_TYPE['ipowerflow'] = 'int_variable'
DICT_VAR_TYPE['iprimdiv'] = 'int_variable'
DICT_VAR_TYPE['iprimhtp'] = 'int_variable'
DICT_VAR_TYPE['iprimnloss'] = 'int_variable'
DICT_VAR_TYPE['iprimshld'] = 'int_variable'
DICT_VAR_TYPE['iprofile'] = 'int_variable'
DICT_VAR_TYPE['iradloss'] = 'int_variable'
DICT_VAR_TYPE['ireactor'] = 'int_variable'
DICT_VAR_TYPE['irfcd'] = 'int_variable'
DICT_VAR_TYPE['isc'] = 'int_variable'
DICT_VAR_TYPE['iscenr'] = 'int_variable'
DICT_VAR_TYPE['iscrp'] = 'int_variable'
DICT_VAR_TYPE['ishape'] = 'int_variable'
DICT_VAR_TYPE['isthtr'] = 'int_variable'
DICT_VAR_TYPE['istore'] = 'int_variable'
DICT_VAR_TYPE['isumatpf'] = 'int_variable'
DICT_VAR_TYPE['isumattf'] = 'int_variable'
DICT_VAR_TYPE['isweep'] = 'int_variable'
DICT_VAR_TYPE['itart'] = 'int_variable'
DICT_VAR_TYPE['itcycl'] = 'int_variable'
DICT_VAR_TYPE['itfsup'] = 'int_variable'
DICT_VAR_TYPE['iwalld'] = 'int_variable'
DICT_VAR_TYPE['lblnkt'] = 'int_variable'
DICT_VAR_TYPE['lpulse'] = 'int_variable'
DICT_VAR_TYPE['lsa'] = 'int_variable'
DICT_VAR_TYPE['m_res'] = 'int_variable'
DICT_VAR_TYPE['maxcal'] = 'int_variable'
DICT_VAR_TYPE['minmax'] = 'int_variable'
DICT_VAR_TYPE['n_res'] = 'int_variable'
DICT_VAR_TYPE['neqns'] = 'int_variable'
DICT_VAR_TYPE['nfxfh'] = 'int_variable'
DICT_VAR_TYPE['ngrp'] = 'int_variable'
DICT_VAR_TYPE['nineqns'] = 'int_variable'
DICT_VAR_TYPE['nipfwh'] = 'int_variable'
DICT_VAR_TYPE['nlpfwh'] = 'int_variable'
DICT_VAR_TYPE['npdiv'] = 'int_variable'
DICT_VAR_TYPE['nphcdin'] = 'int_variable'
DICT_VAR_TYPE['nphcdout'] = 'int_variable'
DICT_VAR_TYPE['nsweep'] = 'int_variable'
DICT_VAR_TYPE['ntype'] = 'int_variable'
DICT_VAR_TYPE['nvar'] = 'int_variable'
DICT_VAR_TYPE['sect01'] = 'int_variable'
DICT_VAR_TYPE['sect02'] = 'int_variable'
DICT_VAR_TYPE['sect03'] = 'int_variable'
DICT_VAR_TYPE['sect04'] = 'int_variable'
DICT_VAR_TYPE['sect05'] = 'int_variable'
DICT_VAR_TYPE['sect06'] = 'int_variable'
DICT_VAR_TYPE['sect07'] = 'int_variable'
DICT_VAR_TYPE['sect08'] = 'int_variable'
DICT_VAR_TYPE['sect09'] = 'int_variable'
DICT_VAR_TYPE['sect10'] = 'int_variable'
DICT_VAR_TYPE['sect11'] = 'int_variable'
DICT_VAR_TYPE['sect12'] = 'int_variable'
DICT_VAR_TYPE['sect13'] = 'int_variable'
DICT_VAR_TYPE['sect14'] = 'int_variable'
DICT_VAR_TYPE['sect15'] = 'int_variable'
DICT_VAR_TYPE['sect16'] = 'int_variable'
DICT_VAR_TYPE['sect17'] = 'int_variable'
DICT_VAR_TYPE['sect18'] = 'int_variable'
DICT_VAR_TYPE['sect19'] = 'int_variable'
DICT_VAR_TYPE['sect20'] = 'int_variable'
DICT_VAR_TYPE['sect21'] = 'int_variable'
DICT_VAR_TYPE['smstr'] = 'int_variable'
DICT_VAR_TYPE['snull'] = 'int_variable'
DICT_VAR_TYPE['tfc_model'] = 'int_variable'
DICT_VAR_TYPE['verbose'] = 'int_variable'
DICT_VAR_TYPE['zfear'] = 'int_variable'

#  int_array
DICT_VAR_TYPE['icc'] = 'int_array'
DICT_VAR_TYPE['ipfloc'] = 'int_array'
DICT_VAR_TYPE['ixc'] = 'int_array'
DICT_VAR_TYPE['ncls'] = 'int_array'

#  real_variable
DICT_VAR_TYPE['abktflnc'] = 'real_variable'
DICT_VAR_TYPE['ac1oh'] = 'real_variable'
DICT_VAR_TYPE['acsoh'] = 'real_variable'
DICT_VAR_TYPE['adivflnc'] = 'real_variable'
DICT_VAR_TYPE['admv'] = 'real_variable'
DICT_VAR_TYPE['afw'] = 'real_variable'
DICT_VAR_TYPE['alfapf'] = 'real_variable'
DICT_VAR_TYPE['alphaj'] = 'real_variable'
DICT_VAR_TYPE['alphan'] = 'real_variable'
DICT_VAR_TYPE['alphat'] = 'real_variable'
DICT_VAR_TYPE['anginc'] = 'real_variable'
DICT_VAR_TYPE['aplasmin'] = 'real_variable'
DICT_VAR_TYPE['aspcstf'] = 'real_variable'
DICT_VAR_TYPE['aspect'] = 'real_variable'
DICT_VAR_TYPE['auxmin'] = 'real_variable'
DICT_VAR_TYPE['baseel'] = 'real_variable'
DICT_VAR_TYPE['bcritsc'] = 'real_variable'
DICT_VAR_TYPE['bctmp'] = 'real_variable'
DICT_VAR_TYPE['bcylth'] = 'real_variable'
DICT_VAR_TYPE['beamfus0'] = 'real_variable'
DICT_VAR_TYPE['beamwd'] = 'real_variable'
DICT_VAR_TYPE['beta'] = 'real_variable'
DICT_VAR_TYPE['betbm0'] = 'real_variable'
DICT_VAR_TYPE['betpmx'] = 'real_variable'
DICT_VAR_TYPE['bigqmin'] = 'real_variable'
DICT_VAR_TYPE['blbmith'] = 'real_variable'
DICT_VAR_TYPE['blbmoth'] = 'real_variable'
DICT_VAR_TYPE['blbpith'] = 'real_variable'
DICT_VAR_TYPE['blbpoth'] = 'real_variable'
DICT_VAR_TYPE['blbuith'] = 'real_variable'
DICT_VAR_TYPE['blbuoth'] = 'real_variable'
DICT_VAR_TYPE['bldr'] = 'real_variable'
DICT_VAR_TYPE['bldzl'] = 'real_variable'
DICT_VAR_TYPE['bldzu'] = 'real_variable'
DICT_VAR_TYPE['blnkith'] = 'real_variable'
DICT_VAR_TYPE['blnkoth'] = 'real_variable'
DICT_VAR_TYPE['blnktth'] = 'real_variable'
DICT_VAR_TYPE['bmn'] = 'real_variable'
DICT_VAR_TYPE['bmxlim'] = 'real_variable'
DICT_VAR_TYPE['bore'] = 'real_variable'
DICT_VAR_TYPE['bpsout'] = 'real_variable'
DICT_VAR_TYPE['bscfmax'] = 'real_variable'
DICT_VAR_TYPE['bt'] = 'real_variable'
DICT_VAR_TYPE['c1div'] = 'real_variable'
DICT_VAR_TYPE['c2div'] = 'real_variable'
DICT_VAR_TYPE['c3div'] = 'real_variable'
DICT_VAR_TYPE['c4div'] = 'real_variable'
DICT_VAR_TYPE['c5div'] = 'real_variable'
DICT_VAR_TYPE['c6div'] = 'real_variable'
DICT_VAR_TYPE['casfact'] = 'real_variable'
DICT_VAR_TYPE['casthi'] = 'real_variable'
DICT_VAR_TYPE['casths'] = 'real_variable'
DICT_VAR_TYPE['cboot'] = 'real_variable'
DICT_VAR_TYPE['cconfix'] = 'real_variable'
DICT_VAR_TYPE['cconshpf'] = 'real_variable'
DICT_VAR_TYPE['cconshtf'] = 'real_variable'
DICT_VAR_TYPE['cdriv0'] = 'real_variable'
DICT_VAR_TYPE['cdriv1'] = 'real_variable'
DICT_VAR_TYPE['cdriv2'] = 'real_variable'
DICT_VAR_TYPE['cdtfleg'] = 'real_variable'
DICT_VAR_TYPE['cfactr'] = 'real_variable'
DICT_VAR_TYPE['cfe0'] = 'real_variable'
DICT_VAR_TYPE['chdzl'] = 'real_variable'
DICT_VAR_TYPE['chdzu'] = 'real_variable'
DICT_VAR_TYPE['chrad'] = 'real_variable'
DICT_VAR_TYPE['cland'] = 'real_variable'
DICT_VAR_TYPE['clh1'] = 'real_variable'
DICT_VAR_TYPE['clh2'] = 'real_variable'
DICT_VAR_TYPE['coheof'] = 'real_variable'
DICT_VAR_TYPE['conv'] = 'real_variable'
DICT_VAR_TYPE['coolp'] = 'real_variable'
DICT_VAR_TYPE['coreradius'] = 'real_variable'
DICT_VAR_TYPE['cowner'] = 'real_variable'
DICT_VAR_TYPE['cpstflnc'] = 'real_variable'
DICT_VAR_TYPE['cpttf'] = 'real_variable'
DICT_VAR_TYPE['csawth'] = 'real_variable'
DICT_VAR_TYPE['csi'] = 'real_variable'
DICT_VAR_TYPE['csutf'] = 'real_variable'
DICT_VAR_TYPE['csytf'] = 'real_variable'
DICT_VAR_TYPE['cturbb'] = 'real_variable'
DICT_VAR_TYPE['cvol'] = 'real_variable'
DICT_VAR_TYPE['dcase'] = 'real_variable'
DICT_VAR_TYPE['dcdrv0'] = 'real_variable'
DICT_VAR_TYPE['dcdrv1'] = 'real_variable'
DICT_VAR_TYPE['dcdrv2'] = 'real_variable'
DICT_VAR_TYPE['dcopper'] = 'real_variable'
DICT_VAR_TYPE['ddwex'] = 'real_variable'
DICT_VAR_TYPE['ddwi'] = 'real_variable'
DICT_VAR_TYPE['declblkt'] = 'real_variable'
DICT_VAR_TYPE['declfw'] = 'real_variable'
DICT_VAR_TYPE['declshld'] = 'real_variable'
DICT_VAR_TYPE['decomf'] = 'real_variable'
DICT_VAR_TYPE['delld'] = 'real_variable'
DICT_VAR_TYPE['dene'] = 'real_variable'
DICT_VAR_TYPE['denstl'] = 'real_variable'
DICT_VAR_TYPE['dintrt'] = 'real_variable'
DICT_VAR_TYPE['divclfr'] = 'real_variable'
DICT_VAR_TYPE['divdens'] = 'real_variable'
DICT_VAR_TYPE['divfix'] = 'real_variable'
DICT_VAR_TYPE['divplt'] = 'real_variable'
DICT_VAR_TYPE['dnbeta'] = 'real_variable'
DICT_VAR_TYPE['drtop'] = 'real_variable'
DICT_VAR_TYPE['drveff'] = 'real_variable'
DICT_VAR_TYPE['dtlife'] = 'real_variable'
DICT_VAR_TYPE['dtmpmx'] = 'real_variable'
DICT_VAR_TYPE['dtstor'] = 'real_variable'
DICT_VAR_TYPE['dztop'] = 'real_variable'
DICT_VAR_TYPE['edrive'] = 'real_variable'
DICT_VAR_TYPE['emult'] = 'real_variable'
DICT_VAR_TYPE['enbeam'] = 'real_variable'
DICT_VAR_TYPE['epbetmax'] = 'real_variable'
DICT_VAR_TYPE['epsfcn'] = 'real_variable'
DICT_VAR_TYPE['epsvmc'] = 'real_variable'
DICT_VAR_TYPE['esbldgm3'] = 'real_variable'
DICT_VAR_TYPE['etacp'] = 'real_variable'
DICT_VAR_TYPE['etaech'] = 'real_variable'
DICT_VAR_TYPE['etafp'] = 'real_variable'
DICT_VAR_TYPE['etahhten'] = 'real_variable'
DICT_VAR_TYPE['etahhtex'] = 'real_variable'
DICT_VAR_TYPE['etahlte'] = 'real_variable'
DICT_VAR_TYPE['etahp'] = 'real_variable'
DICT_VAR_TYPE['etahth'] = 'real_variable'
DICT_VAR_TYPE['etahtpblkt'] = 'real_variable'
DICT_VAR_TYPE['etahtpdiv'] = 'real_variable'
DICT_VAR_TYPE['etahtpfw'] = 'real_variable'
DICT_VAR_TYPE['etahtpshld'] = 'real_variable'
DICT_VAR_TYPE['etainp'] = 'real_variable'
DICT_VAR_TYPE['etalh'] = 'real_variable'
DICT_VAR_TYPE['etalp'] = 'real_variable'
DICT_VAR_TYPE['etanbi'] = 'real_variable'
DICT_VAR_TYPE['etaof'] = 'real_variable'
DICT_VAR_TYPE['etapump'] = 'real_variable'
DICT_VAR_TYPE['etath'] = 'real_variable'
DICT_VAR_TYPE['eyins'] = 'real_variable'
DICT_VAR_TYPE['eystl'] = 'real_variable'
DICT_VAR_TYPE['eywp'] = 'real_variable'
DICT_VAR_TYPE['f_asym'] = 'real_variable'
DICT_VAR_TYPE['f_rad'] = 'real_variable'
DICT_VAR_TYPE['f_w'] = 'real_variable'
DICT_VAR_TYPE['factor'] = 'real_variable'
DICT_VAR_TYPE['falpha'] = 'real_variable'
DICT_VAR_TYPE['farc4tf'] = 'real_variable'
DICT_VAR_TYPE['fauxbop'] = 'real_variable'
DICT_VAR_TYPE['fauxmn'] = 'real_variable'
DICT_VAR_TYPE['fbeta'] = 'real_variable'
DICT_VAR_TYPE['fbetap'] = 'real_variable'
DICT_VAR_TYPE['fbetatry'] = 'real_variable'
DICT_VAR_TYPE['fbfe'] = 'real_variable'
DICT_VAR_TYPE['fblbe'] = 'real_variable'
DICT_VAR_TYPE['fblbreed'] = 'real_variable'
DICT_VAR_TYPE['fblhebmi'] = 'real_variable'
DICT_VAR_TYPE['fblhebmo'] = 'real_variable'
DICT_VAR_TYPE['fblhebpi'] = 'real_variable'
DICT_VAR_TYPE['fblhebpo'] = 'real_variable'
DICT_VAR_TYPE['fblli'] = 'real_variable'
DICT_VAR_TYPE['fblli2o'] = 'real_variable'
DICT_VAR_TYPE['fbllipb'] = 'real_variable'
DICT_VAR_TYPE['fblss'] = 'real_variable'
DICT_VAR_TYPE['fblvd'] = 'real_variable'
DICT_VAR_TYPE['fbreed'] = 'real_variable'
DICT_VAR_TYPE['fburn'] = 'real_variable'
DICT_VAR_TYPE['fcap0'] = 'real_variable'
DICT_VAR_TYPE['fcap0cp'] = 'real_variable'
DICT_VAR_TYPE['fcdfuel'] = 'real_variable'
DICT_VAR_TYPE['fcohbop'] = 'real_variable'
DICT_VAR_TYPE['fcontng'] = 'real_variable'
DICT_VAR_TYPE['fcoolcp'] = 'real_variable'
DICT_VAR_TYPE['fcr0'] = 'real_variable'
DICT_VAR_TYPE['fcuoh'] = 'real_variable'
DICT_VAR_TYPE['fcutfsu'] = 'real_variable'
DICT_VAR_TYPE['fdene'] = 'real_variable'
DICT_VAR_TYPE['fdeut'] = 'real_variable'
DICT_VAR_TYPE['fdfs'] = 'real_variable'
DICT_VAR_TYPE['fdiv'] = 'real_variable'
DICT_VAR_TYPE['fdiva'] = 'real_variable'
DICT_VAR_TYPE['fdivcol'] = 'real_variable'
DICT_VAR_TYPE['fdivwet'] = 'real_variable'
DICT_VAR_TYPE['fdtmp'] = 'real_variable'
DICT_VAR_TYPE['feffcd'] = 'real_variable'
DICT_VAR_TYPE['fflutf'] = 'real_variable'
DICT_VAR_TYPE['ffuspow'] = 'real_variable'
DICT_VAR_TYPE['ffwal'] = 'real_variable'
DICT_VAR_TYPE['ffwlg'] = 'real_variable'
DICT_VAR_TYPE['fgamcd'] = 'real_variable'
DICT_VAR_TYPE['fgamp'] = 'real_variable'
DICT_VAR_TYPE['fhcd'] = 'real_variable'
DICT_VAR_TYPE['fhe3'] = 'real_variable'
DICT_VAR_TYPE['fhldiv'] = 'real_variable'
DICT_VAR_TYPE['fhole'] = 'real_variable'
DICT_VAR_TYPE['fhts'] = 'real_variable'
DICT_VAR_TYPE['fififi'] = 'real_variable'
DICT_VAR_TYPE['fimpvar'] = 'real_variable'
DICT_VAR_TYPE['fiooic'] = 'real_variable'
DICT_VAR_TYPE['fipir'] = 'real_variable'
DICT_VAR_TYPE['fjohc'] = 'real_variable'
DICT_VAR_TYPE['fjohc0'] = 'real_variable'
DICT_VAR_TYPE['fjprot'] = 'real_variable'
DICT_VAR_TYPE['fkblkt'] = 'real_variable'
DICT_VAR_TYPE['fkind'] = 'real_variable'
DICT_VAR_TYPE['flirad'] = 'real_variable'
DICT_VAR_TYPE['flpitch'] = 'real_variable'
DICT_VAR_TYPE['fmgdmw'] = 'real_variable'
DICT_VAR_TYPE['fmsbc'] = 'real_variable'
DICT_VAR_TYPE['fmsbl'] = 'real_variable'
DICT_VAR_TYPE['fmsdwe'] = 'real_variable'
DICT_VAR_TYPE['fmsdwi'] = 'real_variable'
DICT_VAR_TYPE['fmsfw'] = 'real_variable'
DICT_VAR_TYPE['fmsoh'] = 'real_variable'
DICT_VAR_TYPE['fmssh'] = 'real_variable'
DICT_VAR_TYPE['fmstf'] = 'real_variable'
DICT_VAR_TYPE['fmva'] = 'real_variable'
DICT_VAR_TYPE['fndt'] = 'real_variable'
DICT_VAR_TYPE['fpeakb'] = 'real_variable'
DICT_VAR_TYPE['fpinj'] = 'real_variable'
DICT_VAR_TYPE['fpnetel'] = 'real_variable'
DICT_VAR_TYPE['fportsz'] = 'real_variable'
DICT_VAR_TYPE['fpsepr'] = 'real_variable'
DICT_VAR_TYPE['fptemp'] = 'real_variable'
DICT_VAR_TYPE['fptfnuc'] = 'real_variable'
DICT_VAR_TYPE['fpumpblkt'] = 'real_variable'
DICT_VAR_TYPE['fpumpdiv'] = 'real_variable'
DICT_VAR_TYPE['fpumpfw'] = 'real_variable'
DICT_VAR_TYPE['fpumpshld'] = 'real_variable'
DICT_VAR_TYPE['fq'] = 'real_variable'
DICT_VAR_TYPE['fqval'] = 'real_variable'
DICT_VAR_TYPE['fradpwr'] = 'real_variable'
DICT_VAR_TYPE['frbeam'] = 'real_variable'
DICT_VAR_TYPE['frfpf'] = 'real_variable'
DICT_VAR_TYPE['frfptf'] = 'real_variable'
DICT_VAR_TYPE['frhocp'] = 'real_variable'
DICT_VAR_TYPE['frminor'] = 'real_variable'
DICT_VAR_TYPE['frrmax'] = 'real_variable'
DICT_VAR_TYPE['frrp'] = 'real_variable'
DICT_VAR_TYPE['fstrcase'] = 'real_variable'
DICT_VAR_TYPE['fstrcond'] = 'real_variable'
DICT_VAR_TYPE['ftbr'] = 'real_variable'
DICT_VAR_TYPE['ftburn'] = 'real_variable'
DICT_VAR_TYPE['ftcycl'] = 'real_variable'
DICT_VAR_TYPE['ftfthko'] = 'real_variable'
DICT_VAR_TYPE['ftftort'] = 'real_variable'
DICT_VAR_TYPE['ftmargtf'] = 'real_variable'
DICT_VAR_TYPE['ftohs'] = 'real_variable'
DICT_VAR_TYPE['ftol'] = 'real_variable'
DICT_VAR_TYPE['ftpeak'] = 'real_variable'
DICT_VAR_TYPE['ftrit'] = 'real_variable'
DICT_VAR_TYPE['ftritbm'] = 'real_variable'
DICT_VAR_TYPE['fvdump'] = 'real_variable'
DICT_VAR_TYPE['fvoldw'] = 'real_variable'
DICT_VAR_TYPE['fvolsi'] = 'real_variable'
DICT_VAR_TYPE['fvolso'] = 'real_variable'
DICT_VAR_TYPE['fvs'] = 'real_variable'
DICT_VAR_TYPE['fvsbrnni'] = 'real_variable'
DICT_VAR_TYPE['fvvhe'] = 'real_variable'
DICT_VAR_TYPE['fwalld'] = 'real_variable'
DICT_VAR_TYPE['fwclfr'] = 'real_variable'
DICT_VAR_TYPE['fwdr'] = 'real_variable'
DICT_VAR_TYPE['fwdzl'] = 'real_variable'
DICT_VAR_TYPE['fwdzu'] = 'real_variable'
DICT_VAR_TYPE['fwith'] = 'real_variable'
DICT_VAR_TYPE['fwoth'] = 'real_variable'
DICT_VAR_TYPE['gamma'] = 'real_variable'
DICT_VAR_TYPE['gammax'] = 'real_variable'
DICT_VAR_TYPE['gapds'] = 'real_variable'
DICT_VAR_TYPE['gapoh'] = 'real_variable'
DICT_VAR_TYPE['gapomin'] = 'real_variable'
DICT_VAR_TYPE['hccl'] = 'real_variable'
DICT_VAR_TYPE['hcwt'] = 'real_variable'
DICT_VAR_TYPE['helecmw'] = 'real_variable'
DICT_VAR_TYPE['hfact'] = 'real_variable'
DICT_VAR_TYPE['hldivlim'] = 'real_variable'
DICT_VAR_TYPE['hthermmw'] = 'real_variable'
DICT_VAR_TYPE['htpmw'] = 'real_variable'
DICT_VAR_TYPE['impc'] = 'real_variable'
DICT_VAR_TYPE['impfe'] = 'real_variable'
DICT_VAR_TYPE['impo'] = 'real_variable'
DICT_VAR_TYPE['iotabar'] = 'real_variable'
DICT_VAR_TYPE['jbus'] = 'real_variable'
DICT_VAR_TYPE['kappa'] = 'real_variable'
DICT_VAR_TYPE['ksic'] = 'real_variable'
DICT_VAR_TYPE['li6enrich'] = 'real_variable'
DICT_VAR_TYPE['mbvfac'] = 'real_variable'
DICT_VAR_TYPE['mcdriv'] = 'real_variable'
DICT_VAR_TYPE['mvalim'] = 'real_variable'
DICT_VAR_TYPE['nbshield'] = 'real_variable'
DICT_VAR_TYPE['neped'] = 'real_variable'
DICT_VAR_TYPE['nesep'] = 'real_variable'
DICT_VAR_TYPE['nflutfmax'] = 'real_variable'
DICT_VAR_TYPE['oacdcp'] = 'real_variable'
DICT_VAR_TYPE['ohcth'] = 'real_variable'
DICT_VAR_TYPE['ohhghf'] = 'real_variable'
DICT_VAR_TYPE['omegan'] = 'real_variable'
DICT_VAR_TYPE['pbase'] = 'real_variable'
DICT_VAR_TYPE['pc'] = 'real_variable'
DICT_VAR_TYPE['pdrive'] = 'real_variable'
DICT_VAR_TYPE['pfbldgm3'] = 'real_variable'
DICT_VAR_TYPE['pfclres'] = 'real_variable'
DICT_VAR_TYPE['ph'] = 'real_variable'
DICT_VAR_TYPE['pheat'] = 'real_variable'
DICT_VAR_TYPE['pibv'] = 'real_variable'
DICT_VAR_TYPE['pifecr'] = 'real_variable'
DICT_VAR_TYPE['pin'] = 'real_variable'
DICT_VAR_TYPE['pinjalw'] = 'real_variable'
DICT_VAR_TYPE['plsepo'] = 'real_variable'
DICT_VAR_TYPE['pnetelin'] = 'real_variable'
DICT_VAR_TYPE['poisson'] = 'real_variable'
DICT_VAR_TYPE['powfmax'] = 'real_variable'
DICT_VAR_TYPE['pr'] = 'real_variable'
DICT_VAR_TYPE['prdiv'] = 'real_variable'
DICT_VAR_TYPE['prn1'] = 'real_variable'
DICT_VAR_TYPE['prp'] = 'real_variable'
DICT_VAR_TYPE['pseprmax'] = 'real_variable'
DICT_VAR_TYPE['ptargf'] = 'real_variable'
DICT_VAR_TYPE['ptempalw'] = 'real_variable'
DICT_VAR_TYPE['ptfnucmax'] = 'real_variable'
DICT_VAR_TYPE['pwpm2'] = 'real_variable'
DICT_VAR_TYPE['q'] = 'real_variable'
DICT_VAR_TYPE['q0'] = 'real_variable'
DICT_VAR_TYPE['ralpne'] = 'real_variable'
DICT_VAR_TYPE['rat'] = 'real_variable'
DICT_VAR_TYPE['ratecdol'] = 'real_variable'
DICT_VAR_TYPE['rbrt'] = 'real_variable'
DICT_VAR_TYPE['rbvfac'] = 'real_variable'
DICT_VAR_TYPE['rbwt'] = 'real_variable'
DICT_VAR_TYPE['rcool'] = 'real_variable'
DICT_VAR_TYPE['recyle'] = 'real_variable'
DICT_VAR_TYPE['rfpth'] = 'real_variable'
DICT_VAR_TYPE['rhopedn'] = 'real_variable'
DICT_VAR_TYPE['rhopedt'] = 'real_variable'
DICT_VAR_TYPE['rinboard'] = 'real_variable'
DICT_VAR_TYPE['ripmax'] = 'real_variable'
DICT_VAR_TYPE['rlenmax'] = 'real_variable'
DICT_VAR_TYPE['rli'] = 'real_variable'
DICT_VAR_TYPE['rmajor'] = 'real_variable'
DICT_VAR_TYPE['rnbeam'] = 'real_variable'
DICT_VAR_TYPE['routr'] = 'real_variable'
DICT_VAR_TYPE['row'] = 'real_variable'
DICT_VAR_TYPE['rpf1'] = 'real_variable'
DICT_VAR_TYPE['rpf2'] = 'real_variable'
DICT_VAR_TYPE['rpf2dewar'] = 'real_variable'
DICT_VAR_TYPE['rrmax'] = 'real_variable'
DICT_VAR_TYPE['rxcl'] = 'real_variable'
DICT_VAR_TYPE['sccufac'] = 'real_variable'
DICT_VAR_TYPE['scrapli'] = 'real_variable'
DICT_VAR_TYPE['scraplo'] = 'real_variable'
DICT_VAR_TYPE['sgeff'] = 'real_variable'
DICT_VAR_TYPE['shdr'] = 'real_variable'
DICT_VAR_TYPE['shdzl'] = 'real_variable'
DICT_VAR_TYPE['shdzu'] = 'real_variable'
DICT_VAR_TYPE['shear'] = 'real_variable'
DICT_VAR_TYPE['shldith'] = 'real_variable'
DICT_VAR_TYPE['shldoth'] = 'real_variable'
DICT_VAR_TYPE['shldtth'] = 'real_variable'
DICT_VAR_TYPE['shmf'] = 'real_variable'
DICT_VAR_TYPE['shov'] = 'real_variable'
DICT_VAR_TYPE['sigpfalw'] = 'real_variable'
DICT_VAR_TYPE['sigpfcalw'] = 'real_variable'
DICT_VAR_TYPE['sigpfcf'] = 'real_variable'
DICT_VAR_TYPE['sombdr'] = 'real_variable'
DICT_VAR_TYPE['somtdr'] = 'real_variable'
DICT_VAR_TYPE['ssync'] = 'real_variable'
DICT_VAR_TYPE['stcl'] = 'real_variable'
DICT_VAR_TYPE['strncon'] = 'real_variable'
DICT_VAR_TYPE['tbeamin'] = 'real_variable'
DICT_VAR_TYPE['tbeta'] = 'real_variable'
DICT_VAR_TYPE['tbktrepl'] = 'real_variable'
DICT_VAR_TYPE['tbrmin'] = 'real_variable'
DICT_VAR_TYPE['tbrnmn'] = 'real_variable'
DICT_VAR_TYPE['tburn'] = 'real_variable'
DICT_VAR_TYPE['tcomrepl'] = 'real_variable'
DICT_VAR_TYPE['tcoolin'] = 'real_variable'
DICT_VAR_TYPE['tcpav'] = 'real_variable'
DICT_VAR_TYPE['tcritsc'] = 'real_variable'
DICT_VAR_TYPE['tdiv'] = 'real_variable'
DICT_VAR_TYPE['tdivrepl'] = 'real_variable'
DICT_VAR_TYPE['tdmptf'] = 'real_variable'
DICT_VAR_TYPE['tdwell'] = 'real_variable'
DICT_VAR_TYPE['te'] = 'real_variable'
DICT_VAR_TYPE['teped'] = 'real_variable'
DICT_VAR_TYPE['tesep'] = 'real_variable'
DICT_VAR_TYPE['tfacpd'] = 'real_variable'
DICT_VAR_TYPE['tfcbv'] = 'real_variable'
DICT_VAR_TYPE['tfcth'] = 'real_variable'
DICT_VAR_TYPE['tflegres'] = 'real_variable'
DICT_VAR_TYPE['tfno'] = 'real_variable'
DICT_VAR_TYPE['tfootfi'] = 'real_variable'
DICT_VAR_TYPE['tftmp'] = 'real_variable'
DICT_VAR_TYPE['tftort'] = 'real_variable'
DICT_VAR_TYPE['tgain'] = 'real_variable'
DICT_VAR_TYPE['theat'] = 'real_variable'
DICT_VAR_TYPE['thicndut'] = 'real_variable'
DICT_VAR_TYPE['thkcas'] = 'real_variable'
DICT_VAR_TYPE['thwcndut'] = 'real_variable'
DICT_VAR_TYPE['ti'] = 'real_variable'
DICT_VAR_TYPE['tinstf'] = 'real_variable'
DICT_VAR_TYPE['tlife'] = 'real_variable'
DICT_VAR_TYPE['tmargmin'] = 'real_variable'
DICT_VAR_TYPE['tmaxpro'] = 'real_variable'
DICT_VAR_TYPE['tmpcry'] = 'real_variable'
DICT_VAR_TYPE['tmprse'] = 'real_variable'
DICT_VAR_TYPE['tn'] = 'real_variable'
DICT_VAR_TYPE['tohs'] = 'real_variable'
DICT_VAR_TYPE['tohsin'] = 'real_variable'
DICT_VAR_TYPE['tpkmax'] = 'real_variable'
DICT_VAR_TYPE['tqnch'] = 'real_variable'
DICT_VAR_TYPE['tramp'] = 'real_variable'
DICT_VAR_TYPE['tratio'] = 'real_variable'
DICT_VAR_TYPE['trcl'] = 'real_variable'
DICT_VAR_TYPE['triang'] = 'real_variable'
DICT_VAR_TYPE['trithtmw'] = 'real_variable'
DICT_VAR_TYPE['triv'] = 'real_variable'
DICT_VAR_TYPE['ucblbe'] = 'real_variable'
DICT_VAR_TYPE['ucblbreed'] = 'real_variable'
DICT_VAR_TYPE['ucblli'] = 'real_variable'
DICT_VAR_TYPE['ucblli2o'] = 'real_variable'
DICT_VAR_TYPE['ucbllipb'] = 'real_variable'
DICT_VAR_TYPE['ucblss'] = 'real_variable'
DICT_VAR_TYPE['ucblvd'] = 'real_variable'
DICT_VAR_TYPE['ucbus'] = 'real_variable'
DICT_VAR_TYPE['uccarb'] = 'real_variable'
DICT_VAR_TYPE['uccase'] = 'real_variable'
DICT_VAR_TYPE['ucconc'] = 'real_variable'
DICT_VAR_TYPE['uccpcl1'] = 'real_variable'
DICT_VAR_TYPE['uccpclb'] = 'real_variable'
DICT_VAR_TYPE['uccry'] = 'real_variable'
DICT_VAR_TYPE['uccryo'] = 'real_variable'
DICT_VAR_TYPE['uccu'] = 'real_variable'
DICT_VAR_TYPE['ucdiv'] = 'real_variable'
DICT_VAR_TYPE['ucech'] = 'real_variable'
DICT_VAR_TYPE['ucf1'] = 'real_variable'
DICT_VAR_TYPE['ucflib'] = 'real_variable'
DICT_VAR_TYPE['ucfnc'] = 'real_variable'
DICT_VAR_TYPE['ucfuel'] = 'real_variable'
DICT_VAR_TYPE['uche3'] = 'real_variable'
DICT_VAR_TYPE['uchhten'] = 'real_variable'
DICT_VAR_TYPE['uchhtex'] = 'real_variable'
DICT_VAR_TYPE['uchlte'] = 'real_variable'
DICT_VAR_TYPE['uchrs'] = 'real_variable'
DICT_VAR_TYPE['uchth'] = 'real_variable'
DICT_VAR_TYPE['uciac'] = 'real_variable'
DICT_VAR_TYPE['ucich'] = 'real_variable'
DICT_VAR_TYPE['ucihx'] = 'real_variable'
DICT_VAR_TYPE['uclh'] = 'real_variable'
DICT_VAR_TYPE['ucme'] = 'real_variable'
DICT_VAR_TYPE['ucmisc'] = 'real_variable'
DICT_VAR_TYPE['ucnbi'] = 'real_variable'
DICT_VAR_TYPE['ucof'] = 'real_variable'
DICT_VAR_TYPE['ucpens'] = 'real_variable'
DICT_VAR_TYPE['ucpfb'] = 'real_variable'
DICT_VAR_TYPE['ucpfbk'] = 'real_variable'
DICT_VAR_TYPE['ucpfbs'] = 'real_variable'
DICT_VAR_TYPE['ucpfcb'] = 'real_variable'
DICT_VAR_TYPE['ucpfdr1'] = 'real_variable'
DICT_VAR_TYPE['ucpfic'] = 'real_variable'
DICT_VAR_TYPE['ucpfps'] = 'real_variable'
DICT_VAR_TYPE['ucrb'] = 'real_variable'
DICT_VAR_TYPE['ucshld'] = 'real_variable'
DICT_VAR_TYPE['uctarg'] = 'real_variable'
DICT_VAR_TYPE['uctfbr'] = 'real_variable'
DICT_VAR_TYPE['uctfbus'] = 'real_variable'
DICT_VAR_TYPE['uctfps'] = 'real_variable'
DICT_VAR_TYPE['uctfsw'] = 'real_variable'
DICT_VAR_TYPE['ucwindpf'] = 'real_variable'
DICT_VAR_TYPE['ucwindtf'] = 'real_variable'
DICT_VAR_TYPE['uubop'] = 'real_variable'
DICT_VAR_TYPE['uucd'] = 'real_variable'
DICT_VAR_TYPE['uudiv'] = 'real_variable'
DICT_VAR_TYPE['uufuel'] = 'real_variable'
DICT_VAR_TYPE['uufw'] = 'real_variable'
DICT_VAR_TYPE['uumag'] = 'real_variable'
DICT_VAR_TYPE['uuves'] = 'real_variable'
DICT_VAR_TYPE['v1dr'] = 'real_variable'
DICT_VAR_TYPE['v1dzl'] = 'real_variable'
DICT_VAR_TYPE['v1dzu'] = 'real_variable'
DICT_VAR_TYPE['v2dr'] = 'real_variable'
DICT_VAR_TYPE['v2dzl'] = 'real_variable'
DICT_VAR_TYPE['v2dzu'] = 'real_variable'
DICT_VAR_TYPE['v3dr'] = 'real_variable'
DICT_VAR_TYPE['v3dzl'] = 'real_variable'
DICT_VAR_TYPE['v3dzu'] = 'real_variable'
DICT_VAR_TYPE['vachtmw'] = 'real_variable'
DICT_VAR_TYPE['vcool'] = 'real_variable'
DICT_VAR_TYPE['vdalw'] = 'real_variable'
DICT_VAR_TYPE['vfblkt'] = 'real_variable'
DICT_VAR_TYPE['vfohc'] = 'real_variable'
DICT_VAR_TYPE['vfshld'] = 'real_variable'
DICT_VAR_TYPE['vftf'] = 'real_variable'
DICT_VAR_TYPE['vgap2'] = 'real_variable'
DICT_VAR_TYPE['vgaptf'] = 'real_variable'
DICT_VAR_TYPE['vvhealw'] = 'real_variable'
DICT_VAR_TYPE['walalw'] = 'real_variable'
DICT_VAR_TYPE['wallpf'] = 'real_variable'
DICT_VAR_TYPE['wgt'] = 'real_variable'
DICT_VAR_TYPE['wgt2'] = 'real_variable'
DICT_VAR_TYPE['wsvfac'] = 'real_variable'
DICT_VAR_TYPE['xdi'] = 'real_variable'
DICT_VAR_TYPE['xdo'] = 'real_variable'
DICT_VAR_TYPE['xparain'] = 'real_variable'
DICT_VAR_TYPE['xpertin'] = 'real_variable'
DICT_VAR_TYPE['xpf'] = 'real_variable'
DICT_VAR_TYPE['xtb'] = 'real_variable'
DICT_VAR_TYPE['xtfi'] = 'real_variable'
DICT_VAR_TYPE['xtfo'] = 'real_variable'
DICT_VAR_TYPE['zeffdiv'] = 'real_variable'

#  real_array
DICT_VAR_TYPE['blmatf'] = 'real_array'
DICT_VAR_TYPE['cfind'] = 'real_array'
DICT_VAR_TYPE['chmatf'] = 'real_array'
DICT_VAR_TYPE['cptdin'] = 'real_array'
DICT_VAR_TYPE['dcond'] = 'real_array'
DICT_VAR_TYPE['etave'] = 'real_array'
DICT_VAR_TYPE['fimp'] = 'real_array'
DICT_VAR_TYPE['fwmatf'] = 'real_array'
DICT_VAR_TYPE['gainve'] = 'real_array'
DICT_VAR_TYPE['rjconpf'] = 'real_array'
DICT_VAR_TYPE['shmatf'] = 'real_array'
DICT_VAR_TYPE['sweep'] = 'real_array'
DICT_VAR_TYPE['uchts'] = 'real_array'
DICT_VAR_TYPE['ucoam'] = 'real_array'
DICT_VAR_TYPE['ucsc'] = 'real_array'
DICT_VAR_TYPE['ucturb'] = 'real_array'
DICT_VAR_TYPE['ucwst'] = 'real_array'
DICT_VAR_TYPE['v1matf'] = 'real_array'
DICT_VAR_TYPE['v2matf'] = 'real_array'
DICT_VAR_TYPE['v3matf'] = 'real_array'
DICT_VAR_TYPE['vf'] = 'real_array'
DICT_VAR_TYPE['zref'] = 'real_array'

#dictionary of all iteration variables
DICT_IXC_SIMPLE = {'1':'aspect', '2':'bt', '3':'rmajor', '4':'te', '5':'beta',
                   '6':'dene', '7':'rnbeam', '8':'fbeta', '9':'fdene',
                   '10':'hfact', '11':'pheat', '12':'oacdcp', '13':'tfcth',
                   '14':'fwalld', '15':'fvs', '16':'ohcth', '17':'tdwell',
                   '18':'q', '19':'enbeam', '20':'tcpav', '21':'ftburn',
                   '22':'tbrnmn', '23':'fcoolcp', '24':'cdtfleg',
                   '25':'fpnetel', '26':'ffuspow', '27':'fhldiv', '28':'fradpwr',
                   '29':'bore', '30':'fmva', '31':'gapomin',
                   '32':'frminor', '33':'fportsz', '34':'fdivcol',
                   '35': 'fpeakb', '36':'fbetatry', '37':'coheof',
                   '38':'fjohc', '39':'fjohc0', '40':'fgamcd',
                   '41':'fcohbop', '42':'gapoh', '43':'cfe0', '44':'fvsbrnni',
                   '45':'fqval', '46':'fpinj', '47':'feffcd', '48':'fstrcase',
                   '49':'fstrcond', '50':'fiooic', '51':'fvdump', '52':'vdalw',
                   '53':'fjprot', '54':'ftmargtf', '55':'tmargmin',
                   '56':'tdmptf', '57':'thkcas', '58':'thwcndut',
                   '59':'fcutfsu', '60':'cpttf', '61':'gapds', '62':'fdtmp',
                   '63':'ftpeak', '64':'fauxmn', '65':'tohs', '66':'ftohs',
                   '67':'ftcycl', '68':'fptemp', '69':'rcool', '70':'vcool',
                   '71':'fq', '72':'fipir', '73':'scrapli', '74':'scraplo',
                   '75':'tfootfi', '76':'frfptf', '77':'tftort', '78':'rfpth',
                   '79':'fbetap', '80':'frfpf', '81':'edrive', '82':'drveff',
                   '83':'tgain', '84':'chrad', '85':'pdrive', '86':'frrmax',
                   '87':'helecmw', '88':'hthermmw', '89':'ftbr', '90':'blbuith',
                   '91':'blbuoth', '92':'fflutf', '93':'shldith',
                   '94':'shldoth', '95':'fptfnuc', '96':'fvvhe', '97':'fpsepr',
                   '98': 'li6enrich', '99':'ftftort', '100':'ftfthko',
                   '101': 'prp', '102': 'fimpvar'}



#dictionary of dictionaries
DICT_IXC_FULL = defaultdict(dict)

DICT_IXC_FULL[  '1'] = {'name':'aspect',   'lb':1.1,   'ub':10.}
DICT_IXC_FULL[  '2'] = {'name':'bt',       'lb':0.01,  'ub':100.}
DICT_IXC_FULL[  '3'] = {'name':'rmajor',   'lb':0.1,   'ub':10.}
DICT_IXC_FULL[  '4'] = {'name':'te',       'lb':5.,    'ub':500.}
DICT_IXC_FULL[  '5'] = {'name':'beta',     'lb':0.001, 'ub':1.}
DICT_IXC_FULL[  '6'] = {'name':'dene',     'lb':1e19,  'ub':1e21}
DICT_IXC_FULL[  '7'] = {'name':'rnbeam',   'lb':1e-6,  'ub':1.}
DICT_IXC_FULL[  '8'] = {'name':'fbeta',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[  '9'] = {'name':'fdene',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '10'] = {'name':'hfact',    'lb':0.1,   'ub':3.}
DICT_IXC_FULL[ '11'] = {'name':'pheat',    'lb':0.001, 'ub':1e3}
DICT_IXC_FULL[ '12'] = {'name':'oacdcp',   'lb':1e5,   'ub':1.5e8}
DICT_IXC_FULL[ '13'] = {'name':'tfcth',    'lb':0.1,   'ub':5.}
DICT_IXC_FULL[ '14'] = {'name':'fwalld',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '15'] = {'name':'fvs',      'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '16'] = {'name':'ohcth',    'lb':0.001, 'ub':1e2}
DICT_IXC_FULL[ '17'] = {'name':'tdwell',   'lb':0.1,   'ub':1e8}
DICT_IXC_FULL[ '18'] = {'name':'q',        'lb':2.,    'ub':100.}
DICT_IXC_FULL[ '19'] = {'name':'enbeam',   'lb':1.,    'ub':1e6}
DICT_IXC_FULL[ '20'] = {'name':'tcpav',    'lb':40.,   'ub':1e3}
DICT_IXC_FULL[ '21'] = {'name':'ftburn',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '22'] = {'name':'tbrnmn',   'lb':0.001, 'ub':1e6}
DICT_IXC_FULL[ '23'] = {'name':'fcoolcp',  'lb':0.1,   'ub':0.5}
DICT_IXC_FULL[ '24'] = {'name':'cdtfleg',  'lb':1e4,   'ub':1e8}
DICT_IXC_FULL[ '25'] = {'name':'fpnetel',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '26'] = {'name':'ffuspow',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '27'] = {'name':'fhldiv',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '28'] = {'name':'fradpwr',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '29'] = {'name':'bore',     'lb':0.1,   'ub':10.}
DICT_IXC_FULL[ '30'] = {'name':'fmva',     'lb':0.01,  'ub':1.}
DICT_IXC_FULL[ '31'] = {'name':'gapomin',  'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '32'] = {'name':'frminor',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '33'] = {'name':'fportsz',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '34'] = {'name':'fdivcol',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '35'] = {'name':'fpeakb',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '36'] = {'name':'fbetatry', 'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '37'] = {'name':'coheof',   'lb':1e5,   'ub':1e8}
DICT_IXC_FULL[ '38'] = {'name':'fjohc',    'lb':0.01,  'ub':1.}#?
DICT_IXC_FULL[ '39'] = {'name':'fjohc0',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '40'] = {'name':'fgamcd',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '41'] = {'name':'fcohbop',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '42'] = {'name':'gapoh',    'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '43'] = {'name':'cfe0',     'lb':1e-6,  'ub':1e-3}
DICT_IXC_FULL[ '44'] = {'name':'fvsbrnni', 'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '45'] = {'name':'fqval',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '46'] = {'name':'fpinj',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '47'] = {'name':'feffcd',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '48'] = {'name':'fstrcase', 'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '49'] = {'name':'fstrcond', 'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '50'] = {'name':'fiooic',   'lb':0.001, 'ub':0.5} 
DICT_IXC_FULL[ '51'] = {'name':'fvdump',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '52'] = {'name':'vdalw',    'lb':0.001, 'ub':1e6}
DICT_IXC_FULL[ '53'] = {'name':'fjprot',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '54'] = {'name':'ftmargtf', 'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '55'] = {'name':'tmargmin', 'lb':0.001, 'ub':100.}
DICT_IXC_FULL[ '56'] = {'name':'tdmptf',   'lb':10.,   'ub':1e6}
DICT_IXC_FULL[ '57'] = {'name':'thkcas',   'lb':0.05,  'ub':1.}
DICT_IXC_FULL[ '58'] = {'name':'thwcndut', 'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '59'] = {'name':'fcutfsu',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '60'] = {'name':'cpttf',    'lb':0.001, 'ub':4e4}
DICT_IXC_FULL[ '61'] = {'name':'gapds',    'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '62'] = {'name':'fdtmp',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '63'] = {'name':'ftpeak',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '64'] = {'name':'fauxmn',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '65'] = {'name':'tohs',     'lb':0.1,   'ub':1e3}
DICT_IXC_FULL[ '66'] = {'name':'ftohs',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '67'] = {'name':'ftcycl',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '68'] = {'name':'fptemp',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '69'] = {'name':'rcool',    'lb':0.001, 'ub':0.01}
DICT_IXC_FULL[ '70'] = {'name':'vcool',    'lb':1.,    'ub':1e2}
DICT_IXC_FULL[ '71'] = {'name':'fq',       'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '72'] = {'name':'fipir',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '73'] = {'name':'scrapli',  'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '74'] = {'name':'scraplo',  'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '75'] = {'name':'tfootfi',  'lb':0.2,   'ub':5.}
DICT_IXC_FULL[ '76'] = {'name':'frfptf',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '77'] = {'name':'tftort',   'lb':0.05,  'ub':2.}
DICT_IXC_FULL[ '78'] = {'name':'rfpth',    'lb':0.01,  'ub':1.8}
DICT_IXC_FULL[ '79'] = {'name':'fbetap',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '80'] = {'name':'frfpf',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '81'] = {'name':'edrive',   'lb':1e5,   'ub':5e7}
DICT_IXC_FULL[ '82'] = {'name':'drveff',   'lb':0.01,  'ub':1.}
DICT_IXC_FULL[ '83'] = {'name':'tgain',    'lb':1.,    'ub':500.}
DICT_IXC_FULL[ '84'] = {'name':'chrad',    'lb':0.1,   'ub':20.}
DICT_IXC_FULL[ '85'] = {'name':'pdrive',   'lb':1e6,   'ub':2e8}
DICT_IXC_FULL[ '86'] = {'name':'frrmax',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '87'] = {'name':'helecmw',  'lb':1.,    'ub':4e3}
DICT_IXC_FULL[ '88'] = {'name':'hthermmw', 'lb':1.,    'ub':4e3}
DICT_IXC_FULL[ '89'] = {'name':'ftbr',     'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '90'] = {'name':'blbuith',  'lb':0.001, 'ub':2.}
DICT_IXC_FULL[ '91'] = {'name':'blbuoth',  'lb':0.001, 'ub':2.}
DICT_IXC_FULL[ '92'] = {'name':'fflutf',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '93'] = {'name':'shldith',  'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '94'] = {'name':'shldoth',  'lb':0.001, 'ub':10.}
DICT_IXC_FULL[ '95'] = {'name':'fptfnuc',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '96'] = {'name':'fvvhe',    'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '97'] = {'name':'fpsepr',   'lb':0.001, 'ub':1.}
DICT_IXC_FULL[ '98'] = {'name':'li6enrich','lb':0.001, 'ub':100.}
DICT_IXC_FULL[ '99'] = {'name':'ftftort',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL['100'] = {'name':'ftfthko',  'lb':0.001, 'ub':1.}
DICT_IXC_FULL['101'] = {'name':'prp',      'lb':1e-6,  'ub':0.01}
DICT_IXC_FULL['102'] = {'name':'fimpvar',  'lb':1e-6,  'ub':0.01}

#dictionary of dictionaries
DICT_IXC_BOUNDS = defaultdict(dict)

DICT_IXC_BOUNDS['aspect']   = {'lb':1.1,   'ub':10.}  #1
DICT_IXC_BOUNDS['bt']       = {'lb':0.01,  'ub':100.} #2
DICT_IXC_BOUNDS['rmajor']   = {'lb':0.1,   'ub':10.}  #3
DICT_IXC_BOUNDS['te']       = {'lb':5.,    'ub':500.} #4
DICT_IXC_BOUNDS['beta']     = {'lb':0.001, 'ub':1.}   #5
DICT_IXC_BOUNDS['dene']     = {'lb':1e19,  'ub':1e21} #6
DICT_IXC_BOUNDS['rnbeam']   = {'lb':1e-6,  'ub':1.}   #7
DICT_IXC_BOUNDS['fbeta']    = {'lb':0.001, 'ub':1.}   #8
DICT_IXC_BOUNDS['fdene']    = {'lb':0.001, 'ub':1.}   #9
DICT_IXC_BOUNDS['hfact']    = {'lb':0.1,   'ub':3.}   #10
DICT_IXC_BOUNDS['pheat']    = {'lb':0.001, 'ub':1e3}  #11
DICT_IXC_BOUNDS['oacdcp']   = {'lb':1e5,   'ub':1.5e8}#12
DICT_IXC_BOUNDS['tfcth']    = {'lb':0.1,   'ub':5.}   #13
DICT_IXC_BOUNDS['fwalld']   = {'lb':0.001, 'ub':1.}   #14
DICT_IXC_BOUNDS['fvs']      = {'lb':0.001, 'ub':1.}   #15
DICT_IXC_BOUNDS['ohcth']    = {'lb':0.001, 'ub':1e2}  #16
DICT_IXC_BOUNDS['tdwell']   = {'lb':0.1,   'ub':1e8}  #17
DICT_IXC_BOUNDS['q']        = {'lb':2.,    'ub':100.} #18
DICT_IXC_BOUNDS['enbeam']   = {'lb':1.,    'ub':1e6}  #19
DICT_IXC_BOUNDS['tcpav']    = {'lb':40.,   'ub':1e3}  #20
DICT_IXC_BOUNDS['ftburn']   = {'lb':0.001, 'ub':1.}   #21
DICT_IXC_BOUNDS['tbrnmn']   = {'lb':0.001, 'ub':1e6}  #22
DICT_IXC_BOUNDS['fcoolcp']  = {'lb':0.1,   'ub':0.5}  #23
DICT_IXC_BOUNDS['cdtfleg']  = {'lb':1e4,   'ub':1e8}  #24
DICT_IXC_BOUNDS['fpnetel']  = {'lb':0.001, 'ub':1.}   #25
DICT_IXC_BOUNDS['ffuspow']  = {'lb':0.001, 'ub':1.}   #26
DICT_IXC_BOUNDS['fhldiv']   = {'lb':0.001, 'ub':1.}   #27
DICT_IXC_BOUNDS['fradpwr']  = {'lb':0.001, 'ub':1.}   #28
DICT_IXC_BOUNDS['bore']     = {'lb':0.1,   'ub':10.}  #29
DICT_IXC_BOUNDS['fmva']     = {'lb':0.01,  'ub':1.}   #30
DICT_IXC_BOUNDS['gapomin']  = {'lb':0.001, 'ub':10.}  #31
DICT_IXC_BOUNDS['frminor']  = {'lb':0.001, 'ub':1.}   #32
DICT_IXC_BOUNDS['fportsz']  = {'lb':0.01,  'ub':1.}   #33
DICT_IXC_BOUNDS['fdivcol']  = {'lb':0.001, 'ub':1.}   #34
DICT_IXC_BOUNDS['fpeakb']   = {'lb':0.001, 'ub':1.}   #35
DICT_IXC_BOUNDS['fbetatry'] = {'lb':0.001, 'ub':1.}   #36
DICT_IXC_BOUNDS['coheof']   = {'lb':1e5,   'ub':1e8}  #37
DICT_IXC_BOUNDS['fjohc']    = {'lb':0.01,  'ub':1.}   #38
DICT_IXC_BOUNDS['fjohc0']   = {'lb':0.001, 'ub':1.}   #39
DICT_IXC_BOUNDS['fgamcd']   = {'lb':0.001, 'ub':1.}   #40
DICT_IXC_BOUNDS['fcohbop']  = {'lb':0.001, 'ub':1.}   #41
DICT_IXC_BOUNDS['gapoh']    = {'lb':0.001, 'ub':10.}  #42
DICT_IXC_BOUNDS['cfe0']     = {'lb':1e-6,  'ub':1e-3} #43
DICT_IXC_BOUNDS['fvsbrnni'] = {'lb':0.001, 'ub':1.}   #44
DICT_IXC_BOUNDS['fqval']    = {'lb':0.001, 'ub':1.}   #45
DICT_IXC_BOUNDS['fpinj']    = {'lb':0.001, 'ub':1.}   #46
DICT_IXC_BOUNDS['feffcd']   = {'lb':0.001, 'ub':1.}   #47
DICT_IXC_BOUNDS['fstrcase'] = {'lb':0.001, 'ub':1.}   #48
DICT_IXC_BOUNDS['fstrcond'] = {'lb':0.001, 'ub':1.}   #49
DICT_IXC_BOUNDS['fiooic']   = {'lb':0.001, 'ub':0.5}  #50
DICT_IXC_BOUNDS['fvdump']   = {'lb':0.001, 'ub':1.}   #51
DICT_IXC_BOUNDS['vdalw']    = {'lb':0.001, 'ub':1e6}  #52
DICT_IXC_BOUNDS['fjprot']   = {'lb':0.001, 'ub':1.}   #53
DICT_IXC_BOUNDS['ftmargtf'] = {'lb':0.001, 'ub':1.}   #54
DICT_IXC_BOUNDS['tmargmin'] = {'lb':0.001, 'ub':100.} #55
DICT_IXC_BOUNDS['tdmptf']   = {'lb':10.,   'ub':1e6}  #56
DICT_IXC_BOUNDS['thkcas']   = {'lb':0.05,  'ub':1.}   #57
DICT_IXC_BOUNDS['thwcndut'] = {'lb':0.001, 'ub':1.}   #58
DICT_IXC_BOUNDS['fcutfsu']  = {'lb':0.001, 'ub':1.}   #59
DICT_IXC_BOUNDS['cpttf']    = {'lb':0.001, 'ub':4e4}  #60
DICT_IXC_BOUNDS['gapds']    = {'lb':0.001, 'ub':10.}  #61
DICT_IXC_BOUNDS['fdtmp']    = {'lb':0.001, 'ub':1.}   #62
DICT_IXC_BOUNDS['ftpeak']   = {'lb':0.001, 'ub':1.}   #63
DICT_IXC_BOUNDS['fauxmn']   = {'lb':0.001, 'ub':1.}   #64
DICT_IXC_BOUNDS['tohs']     = {'lb':0.1,   'ub':1e3}  #65
DICT_IXC_BOUNDS['ftohs']    = {'lb':0.001, 'ub':1.}   #66
DICT_IXC_BOUNDS['ftcycl']   = {'lb':0.001, 'ub':1.}   #67
DICT_IXC_BOUNDS['fptemp']   = {'lb':0.001, 'ub':1.}   #68
DICT_IXC_BOUNDS['rcool']    = {'lb':0.001, 'ub':0.01} #69
DICT_IXC_BOUNDS['vcool']    = {'lb':1.,    'ub':1e2}  #70
DICT_IXC_BOUNDS['fq']       = {'lb':0.001, 'ub':1.}   #71
DICT_IXC_BOUNDS['fipir']    = {'lb':0.001, 'ub':1.}   #72
DICT_IXC_BOUNDS['scrapli']  = {'lb':0.001, 'ub':10.}  #73
DICT_IXC_BOUNDS['scraplo']  = {'lb':0.001, 'ub':10.}  #74
DICT_IXC_BOUNDS['tfootfi']  = {'lb':0.2,   'ub':5.}   #75
DICT_IXC_BOUNDS['frfptf']   = {'lb':0.001, 'ub':1.}   #76
DICT_IXC_BOUNDS['tftort']   = {'lb':0.05,  'ub':2.}   #77
DICT_IXC_BOUNDS['rfpth']    = {'lb':0.01,  'ub':1.8}  #78
DICT_IXC_BOUNDS['fbetap']   = {'lb':0.001, 'ub':1.}   #79
DICT_IXC_BOUNDS['frfpf']    = {'lb':0.001, 'ub':1.}   #80
DICT_IXC_BOUNDS['edrive']   = {'lb':1e5,   'ub':5e7}  #81
DICT_IXC_BOUNDS['drveff']   = {'lb':0.01,  'ub':1.}   #82
DICT_IXC_BOUNDS['tgain']    = {'lb':1.,    'ub':500.} #83
DICT_IXC_BOUNDS['chrad']    = {'lb':0.1,   'ub':20.}  #84
DICT_IXC_BOUNDS['pdrive']   = {'lb':1e6,   'ub':2e8}  #85
DICT_IXC_BOUNDS['frrmax']   = {'lb':0.001, 'ub':1.}   #86
DICT_IXC_BOUNDS['helecmw']  = {'lb':1.,    'ub':4e3}  #87 
DICT_IXC_BOUNDS['hthermmw'] = {'lb':1.,    'ub':4e3}  #88
DICT_IXC_BOUNDS['ftbr']     = {'lb':0.001, 'ub':1.}   #89
DICT_IXC_BOUNDS['blbuith']  = {'lb':0.001, 'ub':2.}   #90
DICT_IXC_BOUNDS['blbuoth']  = {'lb':0.001, 'ub':2.}   #91
DICT_IXC_BOUNDS['fflutf']   = {'lb':0.001, 'ub':1.}   #92
DICT_IXC_BOUNDS['shldith']  = {'lb':0.001, 'ub':10.}  #93
DICT_IXC_BOUNDS['shldoth']  = {'lb':0.001, 'ub':10.}  #94
DICT_IXC_BOUNDS['fptfnuc']  = {'lb':0.001, 'ub':1.}   #95
DICT_IXC_BOUNDS['fvvhe']    = {'lb':0.001, 'ub':1.}   #96
DICT_IXC_BOUNDS['fpsepr']   = {'lb':0.001, 'ub':1.}   #97
DICT_IXC_BOUNDS['li6enrich']= {'lb':0.001, 'ub':100.} #98
DICT_IXC_BOUNDS['ftftort']  = {'lb':0.001, 'ub':1.}   #99
DICT_IXC_BOUNDS['ftfthko']  = {'lb':0.001, 'ub':1.}   #100
DICT_IXC_BOUNDS['prp']      = {'lb':1e-6,  'ub':0.01} #101
DICT_IXC_BOUNDS['fimpvar']  = {'lb':1e-6,  'ub':0.01} #102


#parameters that start with f, but are not f-values
NON_F_VALUES = ['fcohbop', 'fvsbrnni', 'feffcd', 'fcutfsu']

#dict mapping nsweep to ixc no, if applicable
DICT_NSWEEP2IXC = {'1':'1', '4':'10', '5':'12', '8':'45', '9':'4',
                   '14':'50', '15':'53', '16':'3', '20':'22', '28':'2',
                   '30':'102'}

#dict mapping ixc no to nsweep, if applicable
DICT_IXC2NSWEEP = {'1':'1', '10':'4', '12':'5', '45':'8', '4':'9',
                   '50':'14', '53':'15', '3':'16', '22':'20', '2':'28',
                   '102':'30'}

# PROCESS TF Coil types
DICT_TF_TYPE = {1: "ITER Nb3Sn", 2: "Bi-2212", 3: "NbTi", 4: "Nb3Sn"}

# Optimisation variable dictionary
DICT_OPTIMISATION_VARS = {1: 'Plasma major radius',
                          2: 'ratio fusion power:input power',
                          3: 'neutron wall load',
                          4: 'total TF + PF coil power',
                          5: 'ratio fusion power:injection power',
                          6: 'cost of electricity',
                          7: 'constructed cost',
                          8: 'aspect ratio',
                          9: 'divertor heat load',
                          10: 'toroidal field on axis',
                          11: 'injection power',
                          12: 'hydrogen production capital cost',
                          13: 'hydrogen production rate',
                          14: 'pulse length',
                          15: 'plant availability factor'}


DICT_IXC_DEFAULT = {'aspect': 3.5, 'bt': 6.0, 'rmajor': 7.0, 'te': 15.0,
                    'beta': 0.042, 'dene': 1.5e20, 'rnbeam': 0.005,
                    'fbeta': 1.0, 'fdene': 1.0, 'hfact': 2.0, 'pheat': 0.0,
                    'oacdcp': 1.4e7, 'tfcth': 0.9, 'fwalld': 1.0, 'fvs': 1.0,
                    'ohcth': 0.63, 'tdwell': 100.0, 'q': 3.0, 'enbeam': 1.0e3,
                    'tcpav': 100.0, 'ftburn': 1.0, 'tbrnmn': 1.0,
                    'fcoolcp': 0.3, 'cdtfleg': 1.0e6, 'fpnetel': 1.0,
                    'ffuspow': 1.0, 'fhldiv': 1.0, 'fradpwr': 1.0, 'bore': 1.42,
                    'fmva': 1.0, 'gapomin': 0.21, 'frminor': 1.0, 'fportsz': 1.0,
                    'fdivcol': 1.0, 'fpeakb': 1.0, 'fbetatry': 1.0,
                    'coheof': 1.85e7, 'fjohc': 1.0, 'fjohc0': 1.0,
                    'fgamcd': 1.0, 'fcohbop': 0.9, 'gapoh': 0.08, 'cfe0': 0.0,
                    'fvsbrnni': 1.0, 'fqval': 1.0, 'fpinj': 1.0, 'feffcd': 1.0,
                    'fstrcase': 1.0, 'fstrcond': 1.0, 'fiooic': 0.5,
                    'fvdump': 1.0, 'vdalw': 20.0, 'fjprot': 1.0,
                    'ftmargtf': 1.0, 'tmargmin': 2.5, 'tdmptf': 10.0,
                    'thkcas': 0.3, 'thwcndut': 3.0e-3, 'fcutfsu': 0.69,
                    'cpttf': 3.79e4, 'gapds': 0.0, 'fdtmp': 1.0, 'ftpeak': 1.0,
                    'fauxmn': 1.0, 'tohs': 30.0, 'ftohs': 1.0, 'ftcycl': 1.0,
                    'fptemp': 1.0, 'rcool': 0.005, 'vcool': 20.0, 'fq': 1.0,
                    'fipir': 1.0, 'scrapli': 0.14, 'scraplo': 0.15,
                    'tfootfi': 1.8, 'frfptf': 1.0, 'tftort': 0.33, 'rfpth': 1.5,
                    'fbetap': 1.0, 'frfpf': 1.0, 'edrive': 154.3,
                    'drveff': 0.28, 'tgain': 85.0, 'chrad': 6.5,
                    'pdrive': 23.0e6, 'frrmax': 1.0,
                    'helecmw': 0.0, 'hthermmw': 0.0, 'ftbr': 1.0,
                    'blbuith': 0.365, 'blbuoth': 0.465, 'fflutf': 1.0,
                    'shldith': 0.69, 'shldoth': 1.05, 'fptfnuc': 1.0,
                    'fvvhe': 1.0, 'fpsepr': 1.0, 'li6enrich': 30.0,
                    'ftftort': 1.0, 'ftfthko': 1.0, 'prp': 0.0025,
                    'fimpvar': 1.0e-3}
