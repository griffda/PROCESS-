"""
Dictionaries of the PROCESS iteration variables 
-
This has currently been created by hand and needs to be adjusted to 
all changes in PROCESS manually until someone writes an automatic 
extraction tool.

Author: Hanni Lux (Hanni.Lux@ccfe.ac.uk)

Date: 18 Nov. 2013 - up to date at PROCESS version 202

"""

from collections import defaultdict


#dictionary of all iteration variables
dict_ixc_simple = {'1':'aspect', '2':'bt', '3':'rmajor', '4':'te', '5':'beta', '6':'dene', '7':'rnbeam', '8':'fbeta', '9':'fdene', '10':'hfact', '11':'pheat', '12':'oacdcp', '13':'tfcth', '14':'fwalld', '15':'fvs', '16':'ohcth', '17':'tdwell', '18':'q', '19':'enbeam', '20':'tcpav', '21':'ftburn', '22':'tbrnmn', '23':'fcoolcp', '24':'cdtfleg', '25':'fpnetel', '26':'ffuspow', '27':'fhldiv', '29':'bore', '30':'fmva', '31':'gapomin', '32':'frminor', '33':'fportsz', '34':'fdivcol', '35': 'fpeakb', '36':'fbetatry', '37':'coheof', '38':'fjohc', '39':'fjohc0', '40':'fgamcd', '41':'fcohbop', '42':'gapoh', '43':'cfe0', '44':'fvsbrnni', '45':'fqval', '46':'fpinj', '47':'feffcd', '48':'fstrcase', '49':'fstrcond', '50':'fiooic', '51':'fvdump', '52':'vdalw', '53':'fjprot', '54':'ftmargtf', '55':'tmargmin', '56':'tdmptf', '57':'thkcas', '58':'thwcndut', '59':'fcutfsu', '60':'cpttf', '61':'gapds', '62':'fdtmp','63':'ftpeak', '64':'fauxmn', '65':'tohs', '66':'ftohs', '67':'ftcycl', '68':'fptemp', '69':'rcool','70':'vcool', '71':'fq', '72':'fipir', '73':'scrapli', '74':'scraplo', '75':'tfootfi', '76':'frfptf', '77':'tftort', '78':'rfpth', '79':'fbetap', '80':'frfpf', '81':'edrive', '82':'drveff', '83':'tgain', '84':'chrad', '85':'pdrive', '86':'frrmax', '87':'helecmw', '88':'hthermmw', '89':'ftbr', '90':'blbuith', '91':'blbuoth', '92':'fflutf', '93':'shldith', '94':'shldoth', '95':'fptfnuc', '96':'fvvhe'}



#dictionary of dictionaries
dict_ixc_full = defaultdict(dict)

dict_ixc_full[ '1']  = { 'name':'aspect',   'lb':1.1,   'ub':10.}
dict_ixc_full[ '2']  = { 'name':'bt',       'lb':0.01,  'ub':100.} 
dict_ixc_full[ '3']  = { 'name':'rmajor',   'lb':0.1,   'ub':10.} 
dict_ixc_full[ '4']  = { 'name':'te',       'lb':5.,    'ub':500.} 
dict_ixc_full[ '5']  = { 'name':'beta',     'lb':0.001, 'ub':1.} 
dict_ixc_full[ '6']  = { 'name':'dene',     'lb':1e19,  'ub':1e21} 
dict_ixc_full[ '7']  = { 'name':'rnbeam',   'lb':1e-6,  'ub':1.} 
dict_ixc_full[ '8']  = { 'name':'fbeta',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '9']  = { 'name':'fdene',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '10'] = { 'name':'hfact',    'lb':0.1,   'ub':3.} 
dict_ixc_full[ '11'] = { 'name':'pheat',    'lb':1e6,   'ub':1e9} 
dict_ixc_full[ '12'] = { 'name':'oacdcp',   'lb':1e5,   'ub':1.5e8} 
dict_ixc_full[ '13'] = { 'name':'tfcth',    'lb':0.1,   'ub':5.} 
dict_ixc_full[ '14'] = { 'name':'fwalld',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '15'] = { 'name':'fvs',      'lb':0.001, 'ub':1.} 
dict_ixc_full[ '16'] = { 'name':'ohcth',    'lb':0.001, 'ub':1e2} 
dict_ixc_full[ '17'] = { 'name':'tdwell',   'lb':0.1,   'ub':1e8} 
dict_ixc_full[ '18'] = { 'name':'q',        'lb':2.,    'ub':100.} 
dict_ixc_full[ '19'] = { 'name':'enbeam',   'lb':1.,    'ub':1e6} 
dict_ixc_full[ '20'] = { 'name':'tcpav',    'lb':40.,   'ub':1e3} 
dict_ixc_full[ '21'] = { 'name':'ftburn',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '22'] = { 'name':'tbrnmn',   'lb':0.001, 'ub':1e6} 
dict_ixc_full[ '23'] = { 'name':'fcoolcp',  'lb':0.1,   'ub':0.5} 
dict_ixc_full[ '24'] = { 'name':'cdtfleg',  'lb':1e4,   'ub':1e8} 
dict_ixc_full[ '25'] = { 'name':'fpnetel',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '26'] = { 'name':'ffuspow',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '27'] = { 'name':'fhldiv',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '29'] = { 'name':'bore',     'lb':0.1,   'ub':10.} 
dict_ixc_full[ '30'] = { 'name':'fmva',     'lb':0.01,  'ub':1.}
dict_ixc_full[ '31'] = { 'name':'gapomin',  'lb':0.001, 'ub':10.} 
dict_ixc_full[ '32'] = { 'name':'frminor',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '33'] = { 'name':'fportsz',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '34'] = { 'name':'fdivcol',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '35'] = { 'name':'fpeakb',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '36'] = { 'name':'fbetatry', 'lb':0.001, 'ub':1.} 
dict_ixc_full[ '37'] = { 'name':'coheof',   'lb':1e5,   'ub':1e8} 
dict_ixc_full[ '38'] = { 'name':'fjohc',    'lb':0.01,  'ub':1.} #?
dict_ixc_full[ '39'] = { 'name':'fjohc0',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '40'] = { 'name':'fgamcd',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '41'] = { 'name':'fcohbop',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '42'] = { 'name':'gapoh',    'lb':0.001, 'ub':10.} 
dict_ixc_full[ '43'] = { 'name':'cfe0',     'lb':1e-6,  'ub':1e-3} 
dict_ixc_full[ '44'] = { 'name':'fvsbrnni', 'lb':0.001, 'ub':1.} 
dict_ixc_full[ '45'] = { 'name':'fqval',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '46'] = { 'name':'fpinj',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '47'] = { 'name':'feffcd',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '48'] = { 'name':'fstrcase', 'lb':0.001, 'ub':1.} 
dict_ixc_full[ '49'] = { 'name':'fstrcond', 'lb':0.001, 'ub':1.}
dict_ixc_full[ '50'] = { 'name':'fiooic',   'lb':0.001, 'ub':0.5}  
dict_ixc_full[ '51'] = { 'name':'fvdump',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '52'] = { 'name':'vdalw',    'lb':0.001, 'ub':1e6} 
dict_ixc_full[ '53'] = { 'name':'fjprot',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '54'] = { 'name':'ftmargtf', 'lb':0.001, 'ub':1.} 
dict_ixc_full[ '55'] = { 'name':'tmargmin', 'lb':0.001, 'ub':100.} 
dict_ixc_full[ '56'] = { 'name':'tdmptf',   'lb':10.,   'ub':1e6} 
dict_ixc_full[ '57'] = { 'name':'thkcas',   'lb':0.05,  'ub':1.} 
dict_ixc_full[ '58'] = { 'name':'thwcndut', 'lb':0.001, 'ub':1.} 
dict_ixc_full[ '59'] = { 'name':'fcutfsu',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '60'] = { 'name':'cpttf',    'lb':0.001, 'ub':4e4} 
dict_ixc_full[ '61'] = { 'name':'gapds',    'lb':0.001, 'ub':10.}
dict_ixc_full[ '62'] = { 'name':'fdtmp',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '63'] = { 'name':'ftpeak',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '64'] = { 'name':'fauxmn',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '65'] = { 'name':'tohs',     'lb':0.1,   'ub':1e3} 
dict_ixc_full[ '66'] = { 'name':'ftohs',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '67'] = { 'name':'ftcycl',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '68'] = { 'name':'fptemp',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '69'] = { 'name':'rcool',    'lb':0.001, 'ub':0.01} 
dict_ixc_full[ '70'] = { 'name':'vcool',    'lb':1.,    'ub':1e2} 
dict_ixc_full[ '71'] = { 'name':'fq',       'lb':0.001, 'ub':1.} 
dict_ixc_full[ '72'] = { 'name':'fipir',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '73'] = { 'name':'scrapli',  'lb':0.001, 'ub':10.}
dict_ixc_full[ '74'] = { 'name':'scraplo',  'lb':0.001, 'ub':10.}
dict_ixc_full[ '75'] = { 'name':'tfootfi',  'lb':0.2,   'ub':5.} 
dict_ixc_full[ '76'] = { 'name':'frfptf',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '77'] = { 'name':'tftort',   'lb':0.05,  'ub':2.} 
dict_ixc_full[ '78'] = { 'name':'rfpth',    'lb':0.01,  'ub':1.8} 
dict_ixc_full[ '79'] = { 'name':'fbetap',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '80'] = { 'name':'frfpf',    'lb':0.001, 'ub':1.} 
dict_ixc_full[ '81'] = { 'name':'edrive',   'lb':1e5,   'ub':5e7} 
dict_ixc_full[ '82'] = { 'name':'drveff',   'lb':0.01,  'ub':1.} 
dict_ixc_full[ '83'] = { 'name':'tgain',    'lb':1.,    'ub':500.} 
dict_ixc_full[ '84'] = { 'name':'chrad',    'lb':0.1,   'ub':20.} 
dict_ixc_full[ '85'] = { 'name':'pdrive',   'lb':1e6,   'ub':2e8} 
dict_ixc_full[ '86'] = { 'name':'frrmax',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '87'] = { 'name':'helecmw',  'lb':1.,    'ub':4e3} 
dict_ixc_full[ '88'] = { 'name':'hthermmw', 'lb':1.,    'ub':4e3} 
dict_ixc_full[ '89'] = { 'name':'ftbr',     'lb':0.001, 'ub':1.} 
dict_ixc_full[ '90'] = { 'name':'blbuith',  'lb':0.001, 'ub':2.} 
dict_ixc_full[ '91'] = { 'name':'blbuoth',  'lb':0.001, 'ub':2.} 
dict_ixc_full[ '92'] = { 'name':'fflutf',   'lb':0.001, 'ub':1.} 
dict_ixc_full[ '93'] = { 'name':'shldith',  'lb':0.001, 'ub':10.} 
dict_ixc_full[ '94'] = { 'name':'shldoth',  'lb':0.001, 'ub':10.} 
dict_ixc_full[ '95'] = { 'name':'fptfnuc',  'lb':0.001, 'ub':1.} 
dict_ixc_full[ '96'] = { 'name':'fvvhe',    'lb':0.001, 'ub':1.}



#dictionary of dictionaries
dict_ixc_bounds = defaultdict(dict)

dict_ixc_bounds['aspect']   = { 'lb':1.1,   'ub':10.}   #1
dict_ixc_bounds['bt']       = { 'lb':0.01,  'ub':100.}  #2
dict_ixc_bounds['rmajor']   = { 'lb':0.1,   'ub':10.}   #3
dict_ixc_bounds['te']       = { 'lb':5.,    'ub':500.}  #4
dict_ixc_bounds['beta']     = { 'lb':0.001, 'ub':1.}    #5
dict_ixc_bounds['dene']     = { 'lb':1e19,  'ub':1e21}  #6
dict_ixc_bounds['rnbeam']   = { 'lb':1e-6,  'ub':1.}    #7
dict_ixc_bounds['fbeta']    = { 'lb':0.001, 'ub':1.}    #8
dict_ixc_bounds['fdene']    = { 'lb':0.001, 'ub':1.}    #9
dict_ixc_bounds['hfact']    = { 'lb':0.1,   'ub':3.}    #10
dict_ixc_bounds['pheat']    = { 'lb':1e6,   'ub':1e9}   #11
dict_ixc_bounds['oacdcp']   = { 'lb':1e5,   'ub':1.5e8} #12
dict_ixc_bounds['tfcth']    = { 'lb':0.1,   'ub':5.}    #13
dict_ixc_bounds['fwalld']   = { 'lb':0.001, 'ub':1.}    #14
dict_ixc_bounds['fvs']      = { 'lb':0.001, 'ub':1.}    #15
dict_ixc_bounds['ohcth']    = { 'lb':0.001, 'ub':1e2}   #16
dict_ixc_bounds['tdwell']   = { 'lb':0.1,   'ub':1e8}   #17
dict_ixc_bounds['q']        = { 'lb':2.,    'ub':100.}  #18
dict_ixc_bounds['enbeam']   = { 'lb':1.,    'ub':1e6}   #19
dict_ixc_bounds['tcpav']    = { 'lb':40.,   'ub':1e3}   #20
dict_ixc_bounds['ftburn']   = { 'lb':0.001, 'ub':1.}    #21
dict_ixc_bounds['tbrnmn']   = { 'lb':0.001, 'ub':1e6}   #22
dict_ixc_bounds['fcoolcp']  = { 'lb':0.1,   'ub':0.5}   #23
dict_ixc_bounds['cdtfleg']  = { 'lb':1e4,   'ub':1e8}   #24
dict_ixc_bounds['fpnetel']  = { 'lb':0.001, 'ub':1.}    #25
dict_ixc_bounds['ffuspow']  = { 'lb':0.001, 'ub':1.}    #26
dict_ixc_bounds['fhldiv']   = { 'lb':0.001, 'ub':1.}    #27
dict_ixc_bounds['bore']     = { 'lb':0.1,   'ub':10.}   #29
dict_ixc_bounds['fmva']     = { 'lb':0.01,  'ub':1.}    #30
dict_ixc_bounds['gapomin']  = { 'lb':0.001, 'ub':10.}   #31
dict_ixc_bounds['frminor']  = { 'lb':0.001, 'ub':1.}    #32
dict_ixc_bounds['fportsz']  = { 'lb':0.01,  'ub':1.}    #33
dict_ixc_bounds['fdivcol']  = { 'lb':0.001, 'ub':1.}    #34
dict_ixc_bounds['fpeakb']   = { 'lb':0.001, 'ub':1.}    #35
dict_ixc_bounds['fbetatry'] = { 'lb':0.001, 'ub':1.}    #36
dict_ixc_bounds['coheof']   = { 'lb':1e5,   'ub':1e8}   #37
dict_ixc_bounds['fjohc']    = { 'lb':0.01,  'ub':1.}    #38
dict_ixc_bounds['fjohc0']   = { 'lb':0.001, 'ub':1.}    #39
dict_ixc_bounds['fgamcd']   = { 'lb':0.001, 'ub':1.}    #40
dict_ixc_bounds['fcohbop']  = { 'lb':0.001, 'ub':1.}    #41
dict_ixc_bounds['gapoh']    = { 'lb':0.001, 'ub':10.}   #42
dict_ixc_bounds['cfe0']     = { 'lb':1e-6,  'ub':1e-3}  #43
dict_ixc_bounds['fvsbrnni'] = { 'lb':0.001, 'ub':1.}    #44
dict_ixc_bounds['fqval']    = { 'lb':0.001, 'ub':1.}    #45
dict_ixc_bounds['fpinj']    = { 'lb':0.001, 'ub':1.}    #46
dict_ixc_bounds['feffcd']   = { 'lb':0.001, 'ub':1.}    #47
dict_ixc_bounds['fstrcase'] = { 'lb':0.001, 'ub':1.}    #48
dict_ixc_bounds['fstrcond'] = { 'lb':0.001, 'ub':1.}    #49
dict_ixc_bounds['fiooic']   = { 'lb':0.001, 'ub':0.5}   #50
dict_ixc_bounds['fvdump']   = { 'lb':0.001, 'ub':1.}    #51
dict_ixc_bounds['vdalw']    = { 'lb':0.001, 'ub':1e6}   #52
dict_ixc_bounds['fjprot']   = { 'lb':0.001, 'ub':1.}    #53
dict_ixc_bounds['ftmargtf'] = { 'lb':0.001, 'ub':1.}    #54
dict_ixc_bounds['tmargmin'] = { 'lb':0.001, 'ub':100.}  #55
dict_ixc_bounds['tdmptf']   = { 'lb':10.,   'ub':1e6}   #56
dict_ixc_bounds['thkcas']   = { 'lb':0.05,  'ub':1.}    #57
dict_ixc_bounds['thwcndut'] = { 'lb':0.001, 'ub':1.}    #58
dict_ixc_bounds['fcutfsu']  = { 'lb':0.001, 'ub':1.}    #59
dict_ixc_bounds['cpttf']    = { 'lb':0.001, 'ub':4e4}   #60
dict_ixc_bounds['gapds']    = { 'lb':0.001, 'ub':10.}   #61
dict_ixc_bounds['fdtmp']    = { 'lb':0.001, 'ub':1.}    #62
dict_ixc_bounds['ftpeak']   = { 'lb':0.001, 'ub':1.}    #63
dict_ixc_bounds['fauxmn']   = { 'lb':0.001, 'ub':1.}    #64
dict_ixc_bounds['tohs']     = { 'lb':0.1,   'ub':1e3}   #65
dict_ixc_bounds['ftohs']    = { 'lb':0.001, 'ub':1.}    #66
dict_ixc_bounds['ftcycl']   = { 'lb':0.001, 'ub':1.}    #67
dict_ixc_bounds['fptemp']   = { 'lb':0.001, 'ub':1.}    #68
dict_ixc_bounds['rcool']    = { 'lb':0.001, 'ub':0.01}  #69
dict_ixc_bounds['vcool']    = { 'lb':1.,    'ub':1e2}   #70
dict_ixc_bounds['fq']       = { 'lb':0.001, 'ub':1.}    #71
dict_ixc_bounds['fipir']    = { 'lb':0.001, 'ub':1.}    #72
dict_ixc_bounds['scrapli']  = { 'lb':0.001, 'ub':10.}   #73
dict_ixc_bounds['scraplo']  = { 'lb':0.001, 'ub':10.}   #74
dict_ixc_bounds['tfootfi']  = { 'lb':0.2,   'ub':5.}    #75
dict_ixc_bounds['frfptf']   = { 'lb':0.001, 'ub':1.}    #76
dict_ixc_bounds['tftort']   = { 'lb':0.05,  'ub':2.}    #77
dict_ixc_bounds['rfpth']    = { 'lb':0.01,  'ub':1.8}   #78
dict_ixc_bounds['fbetap']   = { 'lb':0.001, 'ub':1.}    #79
dict_ixc_bounds['frfpf']    = { 'lb':0.001, 'ub':1.}    #80
dict_ixc_bounds['edrive']   = { 'lb':1e5,   'ub':5e7}   #81
dict_ixc_bounds['drveff']   = { 'lb':0.01,  'ub':1.}    #82
dict_ixc_bounds['tgain']    = { 'lb':1.,    'ub':500.}  #83
dict_ixc_bounds['chrad']    = { 'lb':0.1,   'ub':20.}   #84
dict_ixc_bounds['pdrive']   = { 'lb':1e6,   'ub':2e8}   #85
dict_ixc_bounds['frrmax']   = { 'lb':0.001, 'ub':1.}    #86
dict_ixc_bounds['helecmw']  = { 'lb':1.,    'ub':4e3}   #87 
dict_ixc_bounds['hthermmw'] = { 'lb':1.,    'ub':4e3}   #88
dict_ixc_bounds['ftbr']     = { 'lb':0.001, 'ub':1.}    #89
dict_ixc_bounds['blbuith']  = { 'lb':0.001, 'ub':2.}    #90
dict_ixc_bounds['blbuoth']  = { 'lb':0.001, 'ub':2.}    #91
dict_ixc_bounds['fflutf']   = { 'lb':0.001, 'ub':1.}    #92
dict_ixc_bounds['shldith']  = { 'lb':0.001, 'ub':10.}   #93
dict_ixc_bounds['shldoth']  = { 'lb':0.001, 'ub':10.}   #94
dict_ixc_bounds['fptfnuc']  = { 'lb':0.001, 'ub':1.}    #95
dict_ixc_bounds['fvvhe']    = { 'lb':0.001, 'ub':1.}    #96


#parameters that start with f, but are not f-values
NON_F_VALUES = ['fcohbop','fvsbrnni','feffcd','fcutfsu']
