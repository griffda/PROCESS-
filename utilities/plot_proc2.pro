
;-----------------------------------------
; finds a (output) parameter in OUT.DAT
function get_val, param, u
; print, 'in get_val'

 point_lun, u, 0
 newline = ''
 oldline = ''
 while ~eof(u) do begin
  readf, u, newline
  if (strcmp('('+param+')', strmid(newline, 44, strlen(param)+2))) then oldline = newline
 endwhile
 if (strlen(oldline) eq 0) then return, -1
 
 out = float(strmid(oldline, 58))
 
 return, out 
end

;-----------------------------------------
; finds a (input) parameter in OUT.DAT
function get_val2, param, u
; print, 'in get_val2'

 point_lun, u, 0
 newline = ''
 oldline = ''
 while ~eof(u) do begin
  readf, u, newline
  if (strcmp(param, strmid(newline, 41, strlen(param)))) then oldline = newline
 endwhile
 if (strlen(oldline) eq 0) then return, -1
 
 out = float(strmid(oldline, 52))
 
 return, out 
end

;-----------------------------------------
; finds a parameter in OUT.DAT for arbitrary formatting
function get_val3, str, u, readpos, readlen
; print, 'in get_val3'

 point_lun, u, 0
 newline = ''
 oldline = ''
 while ~eof(u) do begin
  readf, u, newline
  check = strpos(newline, str)
  if (check ne -1) then oldline = newline
 endwhile
 out = strtrim(strmid(oldline, readpos, readlen), 2)
 
 return, out 
end

; -------------------------------------------------------------------
; returns points to plot arc given (x0, x1), (y0, y1) and centrepoint
pro arcpoints, xs, ys, xpts, ypts, cnt
 r = sqrt((xpts[0] - cnt[0])^2 + (ypts[0] - cnt[1])^2)
 
 angsx = [(xpts[0]-cnt[0])/r, (xpts[1]-cnt[0])/r]
 angsy = [(ypts[0]-cnt[1])/r, (ypts[1]-cnt[1])/r]

 check = total(where(angsx gt 1))
 if (check ne -1) then angsx[where(angsx gt 1)] = 1.
 check = total(where(angsx lt -1))
 if (check ne -1) then angsx[where(angsx lt -1)] = -1.
 
 angsout = acos(angsx)
 angs = angsout * (1. - 2.*(angsy lt 0.))
 angsp = (angs[0] - angs[1])*findgen(101)/100. + angs[1]
 xs = r * cos(angsp) + cnt[0]
 ys = r * sin(angsp) + cnt[1]

end

;-------------------------------------------
; draws the TF coil
pro drawtf, tfxsysin, tfcntsin, tfxsysout
 arcpoints, xs1, ys1, tfxsysin[0, 0:1], tfxsysin[1, 0:1], tfcntsin[*,0]
 arcpoints, xs2, ys2, tfxsysin[0, 1:2], tfxsysin[1, 1:2], tfcntsin[*,1]
 arcpoints, xs3, ys3, tfxsysin[0, 2:3], tfxsysin[1, 2:3], tfcntsin[*,2]
 arcpoints, xs4, ys4, tfxsysin[0, 3:4], tfxsysin[1, 3:4], tfcntsin[*,3]
  
 r5 = solve_circle([tfxsysout[0,1], tfxsysout[0,0], tfxsysout[0,1]], [-tfxsysout[1,1], tfxsysout[1,0], tfxsysout[1,1]], x05, y05, angs5)
 r6 = solve_circle([tfxsysout[0,1], tfxsysout[0,2], tfxsysout[0,3]], [tfxsysout[1,1], tfxsysout[1,2], tfxsysout[1,3]], x06, y06, angs6)
 r7 = solve_circle([tfxsysout[0,3], tfxsysout[0,4], tfxsysout[0,5]], [tfxsysout[1,3], tfxsysout[1,4], tfxsysout[1,5]], x07, y07, angs7)
 r8 = solve_circle([tfxsysout[0,5], tfxsysout[0,6], tfxsysout[0,5]], [-tfxsysout[1,5], tfxsysout[1,6], tfxsysout[1,5]], x08, y08, angs8)

 angs5[0] = angs5[0] + 2.*!pi
 angsp5 = (angs5[0] - angs5[1])*findgen(101)/100. + angs5[1]
 angsp6 = (angs6[0] - angs6[1])*findgen(101)/100. + angs6[1]
 angsp7 = (angs7[0] - angs7[1])*findgen(101)/100. + angs7[1]
 angsp8 = (angs8[0] - angs8[1])*findgen(101)/100. + angs8[1]
 
 x15 = r5 * cos(angsp5) + x05 & y15 = r5 * sin(angsp5) + y05
 x16 = r6 * cos(angsp6) + x06 & y16 = r6 * sin(angsp6) + y06
 x17 = r7 * cos(angsp7) + x07 & y17 = r7 * sin(angsp7) + y07
 x18 = r8 * cos(angsp8) + x08 & y18 = r8 * sin(angsp8) + y08
 
; plot, xs1, ys1, /iso, xrange=[0, 12], yrange=[-12, 12]
 oplot, xs1, ys1
 oplot, xs1, -ys1
 oplot, xs2, ys2
 oplot, xs2, -ys2
 oplot, xs3, ys3
 oplot, xs3, -ys3
 oplot, xs4, ys4
 oplot, xs4, -ys4
 oplot, x15, y15
 oplot, x16, y16
 oplot, x16, -y16
 oplot, x17, y17
 oplot, x17, -y17
 oplot, x18, y18
 
end
;----------------------------------------
; gathers PROCESS data ready for plotting 
; creates an array containing [field, parameter, text, value, units] vectors
; where field indicates the field to use in the final plot
; parameter is the actual parameter name to locate in OUT.DAT
; text is the text to accompany the data on the plot
; value is the number
; units is the units of the number
function gather_info, u, info, tf_type
; print, 'in gather_info'

 datlength = 100
 data = strarr(5, datlength)
 info = strarr(4)
 
 newline=''
 point_lun, u, 0
 for i = 0, 6 do readf, u, newline
 for i = 0, 3 do begin
  readf, u, newline
  info[i] = strtrim(newline, 2)
 endfor

 count = 0

; field 1: geometry and at-a-glance build info
 data[*,count] = ['1', 'rmajor', 'R!I0!N', strtrim(string(get_val('rmajor', u)), 2), 'm'] & count++
 data[*,count] = ['1', 'rminor', 'a', strtrim(string(get_val('rminor', u)), 2), 'm'] & count++
 data[*,count] = ['1', 'aspect', 'Aspect ratio', strtrim(string(get_val('aspect', u)), 2), ''] & count++
 data[*,count] = ['1', 'kappa95', '!4j!3!I95!N', strtrim(string(get_val('kappa95', u)), 2), ''] & count++
 data[*,count] = ['1', 'triang95', '!4d!3!I95!N', strtrim(string(get_val('triang95', u)), 2), ''] & count++
 data[*,count] = ['1', 'sarea', 'Surface area', strtrim(string(get_val('sarea', u)), 2), 'm!E2!N'] & count++
 data[*,count] = ['1', 'vol', 'Plasma volume', strtrim(string(get_val('vol', u)), 2), 'm!E3!N'] & count++
 data[*,count] = ['1', 'tfno', 'No. of TF coils', strtrim(string(fix(get_val('tfno', u))), 2), ''] & count++
; inboard and outboard blanket thickness
 thk1 = get_val('shldith', u) + get_val('blnkith', u)
 thk2 = get_val('shldoth', u) + get_val('blnkoth', u)
 data[*,count] = ['1', 'shldith', 'i/b blkt/shld', strtrim(string(thk1), 2), 'm'] & count++
 data[*,count] = ['1', 'shldoth', 'o/b blkt/shld', strtrim(string(thk2), 2), 'm'] & count++
 data[*,count] = ['1', 'powfmw', 'Fusion power', strtrim(string(get_val('powfmw', u)), 2), 'MW'] & count++
; estimate helium confinement time and ratio
 powfmw  = get_val('powfmw', u)
 efus = 17.6e0 * 1.602e-19
 rpers = powfmw/efus
 nalp = get_val('dnalp', u) * get_val('vol', u)
 taupst = nalp/rpers
 print, 'tau*: ', taupst, ' (assumes D-T fusion...!)'
 print, 'tau*/taue: ', taupst/get_val('taueff', u)

; field2: physics
 data[*,count] = ['2', 'plascur', 'I!IP!N', strtrim(string(get_val('plascur/1D6', u)), 2), 'MA'] & count++
; data[*,count] = ['2', 'bootipf', 'Bootstrap fraction', strtrim(string(get_val('bootipf', u)), 2), ''] & count++
 data[*,count] = ['2', 'bt', 'Vacuum B!IT!N at R!I0!N', strtrim(string(get_val('bt', u)), 2), 'T'] & count++
 data[*,count] = ['2', 'q', 'q!Iedge!N', strtrim(string(get_val('q', u)), 2), ''] & count++
 bettype = get_val2('ICULBL', u)
; defines which beta limit applies to and hence which normalised beta is quoted
; 0: (default) total beta, 1: thermal beta, 2: thermal + NB beta
 betna = float(get_val3('Normalised beta', u, 58, 20))
 betaa = get_val('beta', u)
 betta = float(get_val3('Thermal beta', u, 58, 20))
 betnba = get_val('betanb', u)
 CASE bettype OF
 -1: begin
      betn = betna
      bett = betna * betta/betaa
     end
  0: begin
      betn = betna
      bett = betna * betta/betaa
     end
  1: begin
      betn = betna * betaa/betta
      bett = betna
     end
  2: begin
      betn = betna * betaa/(betta+betnba)
      bett = betna * betta/(betta+betnba)
     end
  else: begin
      print, 'ICULBL value not identified: ', bettype
      betn = 0.0
      bett = 0.0
     end
  endcase
 data[*,count] = ['2', 'beta_nt', '!4b!3!IN!N, thermal', strtrim(string(bett), 2), '% MA m!E-1!N T!E-1!N'] & count++
 data[*,count] = ['2', 'beta_n', '!4b!3!IN!N, total', strtrim(string(betn), 2), '% MA m!E-1!N T!E-1!N'] & count++
 data[*,count] = ['2', 'betap', '!4b!3!IP!N', strtrim(string(get_val('betap', u)), 2), ''] & count++
 data[*,count] = ['2', 'te', '<T!Ie!N>', strtrim(string(get_val('te', u)), 2), 'keV'] & count++
 data[*,count] = ['2', 'dene', '<n!Ie, vol!N>', strtrim(string(get_val('dene', u)), 2), 'm!E-3!N'] & count++
 nong = get_val('dnla', u)/get_val('dlimit(7)', u)
 data[*,count] = ['2', 'dlimit(7)', '<n!Ie, line!N>/n!IG!N', strtrim(string(nong), 2), ''] & count++
 data[*,count] = ['2', 'alphat', 'T!Ie0!N/<T!Ie!N>', strtrim(string(1.+get_val('alphat', u)), 2), ''] & count++
 data[*,count] = ['2', 'alphan', 'n!Ie0!N/<n!Ie, vol!N>', strtrim(string(1.+get_val('alphan', u)), 2), ''] & count++
 lavpeak = ((1.+get_val('alphan', u))*get_val('dene', u))/get_val('dnla', u)
 data[*,count] = ['2', 'alphan2', 'n!Ie0!N/<n!Ie, line!N>', strtrim(string(lavpeak), 2), ''] & count++
 data[*,count] = ['2', 'zeff', 'Z!Ieff!N', strtrim(string(get_val('zeff', u)), 2), ''] & count++
 data[*,count] = ['2', 'zeffso', 'Z!Ieff!N (SoL)', strtrim(string(get_val('zeffso', u)), 2), ''] & count++
 data[*,count] = ['2', 'dnz', 'n!IZ!N/<n!Ie!N>', strtrim(string(get_val('dnz', u)/get_val('dene', u)), 2), ''] & count++
 data[*,count] = ['2', 'taueff', '!4s!3!IE!N', strtrim(string(get_val('taueff', u)), 2), 's'] & count++
 data[*,count] = ['2', 'hfact', 'H-factor', strtrim(string(get_val('hfact', u)), 2), ''] & count++
 data[*,count] = ['2', 'law', 'Scaling law', get_val3('Confinement scaling law', u, 44, 13), ''] & count++


; field3: currents
; these have to be handled specially
 point_lun, u, 0
 while ~eof(u) do begin
  readf, u, newline
  check = strpos(newline, 'PF Coil Information :')
  if (check ne -1) then point_lun, -u, pos
 endwhile
 point_lun, u, pos
 for i = 1, 5 do readf, u, newline
 lcount = 0
 while (strlen(newline) gt 5) do begin
  readf, u, newline
  lcount = lcount + 1
 endwhile
 lcount = lcount - 2
; print, 'lcount = '+string(lcount)
 point_lun, u, pos
 for i = 1, 3 do readf, u, newline
 for i = 0, ((lcount-1)/2) do begin
; print, 'in for loop'
  readf, u, newline
  readf, u, newline
  data[*,count] = ['3', strtrim(strmid(newline, 0, 5),2), strtrim(strmid(newline, 0, 5),2), strtrim(strmid(newline, 6, 8),2), 'MA'] & count++
 endfor
 data[*,count] = ['3', 'vsind+vsres', 'Startup flux swing', strtrim(string(get_val('vsind', u)+get_val('vsres', u)), 2), 'Wb'] & count++
 data[*,count] = ['3', 'flx', 'Available flux swing', strtrim(string(-1.*float(get_val3('Total :', u, 42, 12))), 2), 'Wb'] & count++
 data[*,count] = ['3', 'tburn', 'Burn time', strtrim(string(get_val('tburn', u)/3600.), 2), 'hrs'] & count++
; field3.5: TF coils
 if ~(tf_type eq 'Cu') then begin
;  print, 'SC coils'
  data[*,count] = ['35', 'bmaxtf', 'Peak field at conductor', strtrim(string(get_val('bmaxtf', u)), 2), 'T'] & count++
  data[*,count] = ['35', 'iooic', 'I/I!Icrit!N', strtrim(string(get_val('iooic', u)), 2), ''] & count++
  data[*,count] = ['35', 'tmarg', 'Temperature margin', strtrim(string(get_val('tmarg', u)), 2), 'K'] & count++
  data[*,count] = ['35', 'strtf1', 'Conduit Von Mises stress', strtrim(string(get_val('strtf1', u)), 2), 'Pa'] & count++
  data[*,count] = ['35', 'strtf2', 'Case Von Mises stress', strtrim(string(get_val('strtf2', u)), 2), 'Pa'] & count++
  data[*,count] = ['35', 'alstrtf', 'Allowable stress', strtrim(string(get_val('alstrtf', u)), 2), 'Pa'] & count++
;  data[*,count] = ['35', 'deflect', 'Deflection at midplane', strtrim(string(get_val('deflect', u)), 2), 'm'] & count++
 endif else begin
;  print, 'Cu coils'
 endelse
 
; field4: power flows and economics
 data[*,count] = ['4', 'wallmw', 'Av. neutron wall load', strtrim(string(get_val('wallmw', u)), 2), 'MW m!E-2!N'] & count++
 data[*,count] = ['4', 'pbrem*vol', 'Bremsstrahlung radiation', strtrim(string(get_val('pbrem*vol', u)), 2), 'MW'] & count++
 data[*,count] = ['4', 'psync*vol', 'Synchrotron radiation', strtrim(string(get_val('psync*vol', u)), 2), 'MW'] & count++
 data[*,count] = ['4', 'plrad*vol', 'Line radiation', strtrim(string(get_val('plrad*vol', u)), 2), 'MW'] & count++
 data[*,count] = ['4', 'pnucblkt', 'Nuclear heating in blanket', strtrim(string(get_val('pnucblkt', u)), 2), 'MW'] & count++
 data[*,count] = ['4', 'pnucshld', 'Nuclear heating in shield', strtrim(string(get_val('pnucshld', u)), 2), 'MW'] & count++
 
 pinj = get_val('pinji/1.d6', u) + get_val('pinje/1.d6', u)
; data[*,count] = ['4', 'pinji+pinje', 'Auxiliary (H&CD) power', strtrim(string(pinj), 2), 'MW'] & count++
 data[*,count] = ['4', 'pdivt', 'Power to divertor', strtrim(string(get_val('pdivt', u)), 2), 'MW'] & count++
 data[*,count] = ['4', 'hldiv', 'Divertor peak heat flux', strtrim(string(get_val('hldiv', u)), 2), 'MW m!E-2!N'] & count++
 
 data[*,count] = ['4', 'FWlife', 'FW/blanket life', strtrim(string(float(get_val3('First wall / blanket life', u, 60, 13))), 2), 'years'] & count++
 data[*,count] = ['4', 'Divlife', 'Divertor life', get_val3('Divertor life ', u, 60, 13), 'years'] & count++ 
 data[*,count] = ['4', 'CoE', 'Cost of electricity', get_val3('Cost of electricity', u, 60, 13), 'm$/kWh'] & count++
 data[*,count] = ['4', 'HGtherm', 'Thermal power', get_val3('High grade thermal power', u, 60, 13), 'MW'] & count++
 data[*,count] = ['4', 'pgrossmw/pthermmw', 'Thermal efficiency', strtrim(string(get_val('pgrossmw', u)/get_val('pthermmw', u)), 2), '%'] & count++
 data[*,count] = ['4', 'pgrossmw', 'Gross electric power', strtrim(string(get_val('pgrossmw', u)), 2), 'MW'] & count++ 
 data[*,count] = ['4', 'pnetelmw', 'Net electric power', strtrim(string(get_val('pnetelmw', u)), 2), 'MW'] & count++
 data[*,count] = ['4', 'burnup', 'Fuel burnup fraction', strtrim(string(get_val('burnup', u)), 2), ''] & count++
 dtyear = get_val('frate', u) * 3600. * 24. * 365.24
 data[*,count] = ['4', 'frate', 'D-T fuel throughput', strtrim(string(dtyear), 2), 'kg year!E-1!N'] & count++
 tconsump = dtyear * 0.6 * get_val('burnup', u)
 data[*,count] = ['4', 'frate', 'T consumption', strtrim(string(tconsump), 2), 'kg year!E-1!N'] & count++

; field 5: current drive
; get current drive type
 point_lun, u, 0
 while ~eof(u) do begin
  readf, u, newline
  check = strpos(newline, '* Current Drive System *')
  if (check ne -1) then point_lun, -u, pos
 endwhile
 point_lun, u, pos
 for i = 1, 2 do readf, u, newline
 if (strpos(newline, 'Neutral Beam') ne -1) then cdtype=0 else cdtype=1
 data[*,count] = ['5', 'CDsystem', strtrim(newline, 2), '', ''] & count++
 data[*,count] = ['5', 'pinji+pinje', 'SS auxiliary power', strtrim(string(pinj), 2), 'MW'] & count++
 check = get_val('pheat', u)
 if (check eq -1) then check = 0.0
 data[*,count] = ['5', 'pheat', 'Power for heating only', strtrim(string(check/1.e6), 2), 'MW'] & count++
 data[*,count] = ['5', 'bootipf', 'Bootstrap fraction', strtrim(string(get_val('bootipf', u)), 2), ''] & count++
 data[*,count] = ['5', 'faccd', 'Auxiliary fraction', strtrim(string(get_val('faccd', u)), 2), ''] & count++
 data[*,count] = ['5', 'facoh', 'Ohmic fraction', strtrim(string(1.-get_val('bootipf', u)-get_val('faccd', u)), 2), ''] & count++
 if (cdtype eq 0) then begin
  data[*,count] = ['5', 'gamnb', 'NB gamma', strtrim(string(get_val('gamnb', u)), 2), '10!E20!N A W!E-1!N m!E-2!N'] & count++
  data[*,count] = ['5', 'enbeam', 'NB energy', strtrim(string(get_val('enbeam', u)), 2), 'keV'] & count++
;  data[*,count] = ['5', 'fshine', 'NB shine-through', strtrim(string(get_val('fshine', u)), 2), ''] & count++
 endif
 data[*,count] = ['5', 'powerht', 'Assumed heating power', strtrim(string(get_val('powerht', u)), 2), 'MW'] & count++
; H-mode threshold, Martin (2008), assuming M=2.5 (D-T)
 dnla = get_val('dnla', u)/1.e20
 bt = get_val('bt', u)
 surf = get_val('sarea', u)
 pthresh = 0.0488 * dnla^0.717 * bt^0.803 * surf^0.941 * 0.8
 err = 0.057^2 + (0.035*alog(dnla))^2 + (0.032*alog(bt))^2 + (0.019*alog(surf))^2
 err = sqrt(err) * pthresh
 data[*,count] = ['5', 'pthresh', 'H-mode threshold (M=2.5)', strtrim(string(pthresh), 2) + ' +- ' + strtrim(string(err), 2), 'MW'] & count++
; divertor width assuming hldiv = average heat load and r = R0
 hldiv = get_val('hldiv', u)
 pdivt = get_val('pdivt', u)
 rmajor = get_val('rmajor', u)
 divwid = (pdivt/hldiv)/(2.*3.14159*rmajor)
 data[*,count] = ['5', 'divwid', 'Guessed div. width', strtrim(string(divwid), 2), 'm'] & count++



; print, 'count = '+string(count)
; print,  string(datlength-count)+' places remaining'

 return, data
end



pro procit2, u, result
; check to see if help has been requested
; if keyword_set(help) then begin
;  print, 'Usage:'
;  print, 'procit'
;  print, ''
;  print, 'Collects and plots solution vectors from a PROCESS sweep.'
;  print, 'Should be run in the folder containing OUT.DAT.'
;  print, ''
;  goto, theend
; endif

; fn = 'OUT.DAT'
; if (file_test(fn) eq 0) then fn = dialog_pickfile(filter='OUT.DAT')
; if (fn eq '') then goto, theend
; if (file_test(fn) eq 0) then begin
;  print, "File " + fn + " not found."
;  goto, theend
; endif

; get_lun, u
; openr, u, fn

 isweep = get_val2('ISWEEP', u)
 newline = ''
; while ~eof(u) do begin
;  readf, u, newline
;  if (strcmp(strmid(newline, 41, 6), 'ISWEEP')) then begin
;   str2 = strtrim(strmid(newline, 52),2)
;   isweep = fix(str2)
;   print, 'isweep = '+string(isweep)
   result = isweep
;  endif
; endwhile
 
 if (isweep eq -1) then begin
  print, 'No sweep detected: ending procit2'
  maxyval = 1.
  goto, theend
 endif
 
 point_lun, u, 0
 while ~(strcmp(newline, '         ixc     label')) do readf, u, newline
 readf, u, newline
 filedata = fstat(u)
 posns = filedata.cur_ptr
 readf, u, newline
 count = 0
 while ~(strcmp(newline, ' ')) do begin
  count = count+1
  readf, u, newline
 endwhile
; print, count
 point_lun, u, posns
 names = strarr(count)
 values = fltarr(count, isweep)
 instruc = {a:1, b:2, c:'string'}
 for i = 0, count-1 do begin
  readf, u, instruc
  names[i] = instruc.c
 endfor
; print, names

 for i = 0, isweep-1 do begin
  while ~(strcmp(strmid(newline, 0, 17), ' ***** Scan point')) do readf, u, newline
  while ~(strcmp(strmid(newline, 0, 17), ' The solution vec')) do readf, u, newline
  for j = 0, 4 do readf, u, newline
  for j = 0, count-1 do begin
   readf, u, newline
   values[j, i] = float(strmid(newline, 20, 11))
  endfor
 endfor

 valsnorm = values
 for i = 0, count-1 do valsnorm[i,*] = values[i,*]/values[i,0]
 maxyval = max(valsnorm)
 plot, valsnorm[0,*], yrange=[0,maxyval], xtitle = 'Sweep number', ytitle='Normalised value', title='Sweep parameter summary'
 for i = 0, count-1 do oplot, valsnorm[i,*]
theend:
end
;---------------------------------------------
; returns the product of the elements of an array
function prodct, ins
 result = 1.
 for i = 0, n_elements(ins)-1 do result = result * ins[i]
 return, result
end


;----------------------------------------------
; finds r, x0, y0 for a circle from a triplet of points
; and also returns the angles to plot between to draw the arc
function solve_circle, xs, ys, x0, y0, angs
 ydifs = [ys[0] - ys[2], ys[1] - ys[0], ys[2] - ys[1]]
 xdifs = [xs[0] - xs[2], xs[1] - xs[0], xs[2] - xs[1]]
 rtop = prodct(xdifs^2 + ydifs^2)
 rbot = xs[0]*ydifs[2] + xs[1]*ydifs[0] + xs[2]*ydifs[1]
 r = 0.5 * sqrt(rtop/(rbot^2))

 xtop = total(xs^2 * (-[ydifs[2], ydifs[0], ydifs[1]])) + total(ys^2 * (-[ydifs[2], ydifs[0], ydifs[1]]))
 xbot = -rbot
 x0 = 0.5 * xtop/xbot

 ytop = total(xs^2 * ([xdifs[2], xdifs[0], xdifs[1]])) + total(ys^2 * ([xdifs[2], xdifs[0], xdifs[1]]))
 y0 = 0.5 * ytop/xbot

 angsx = [(xs[0]-x0)/r, (xs[2]-x0)/r]
 angsy = [(ys[0]-y0)/r, (ys[2]-y0)/r]

 angsout = acos(angsx)
 angs = angsout * (1. - 2.*(angsy lt 0.))

; use:
; angsp = (angs[0] - angs[1])*findgen(101)/100. + angs[1]
; plot, xs, ys, /iso, xrange = [-4, 6], yrange=[-6, 4]
; x1 = r * cos(angsp) + x0
; y1 = r * sin(angsp) + y0
; oplot, x1, y1
; to plot the segment across the points

 return, r
end
; -----------------------------------------
function strpad, str, strleng
; print, 'in strpad'
 str2 = strmid(str, 0, strleng)
 len = strlen(str2)
 pad = strleng-len
 if (pad ne 0) then for i = 1, pad do str2 = str2 + ' '
 return, str2
end

;------------------------------------------
; Plots a thick radial d-section
pro plotthick, inpt, outpt, inthk, outthk, toppt, topthk, del
; print, 'in plotthick'

 r01 = (inpt+outpt)/2.
 r02 = (inpt+inthk + outpt-outthk)/2.
 a1 = r01 - inpt
 a2 = r02 - inpt - inthk
 kap1 = toppt/a1
 kap2 = (toppt-topthk)/a2
 
 angs = 2.*!pi*findgen(1001)/1000.
 rs1 = r01 + a1*cos(angs - del*sin(-1.*angs))
 zs1 = kap1 * a1 * sin(angs)
 rs2 = r02 + a2*cos(angs - del*sin(-1.*angs))
 zs2 = kap2 * a2 * sin(angs)
 
; if keyword_set(fpl) then plot, rs1, zs1, /iso
; polyfill, [rs1, rs2], [zs1, zs2], color=col
 oplot, rs1, zs1
 oplot, rs2, zs2
end

;-----------------------------------------
; plots thick d-section with gap
pro plotdgap, inpt, outpt, inthk, outthk, toppt, topthk, del
; print, 'in plotdgap'

 plotdhgap, inpt, outpt, inthk, outthk, toppt, topthk, del
 plotdhgap, inpt, outpt, inthk, outthk, -toppt, -topthk, del
 
end

;-----------------------------------------
; plots half thick d-section with gap
pro plotdhgap, inpt, outpt, inthk, outthk, toppt, topthk, del
; print, 'in plotdgap'

 arc = !pi/4.
 r01 = (inpt+outpt)/2.
 r02 = (inpt+inthk + outpt-outthk)/2.
 a1 = r01 - inpt
 a2 = r02 - inpt - inthk
 kap1 = toppt/a1
 kap2 = (toppt-topthk)/a2
 
 angs = ((!pi/2.) - arc/2.) * findgen(50)/49.
 rs1 = r01 + a1*cos(angs + del*sin(angs))
 zs1 = kap1 * a1 * sin(angs)
 rs2 = r02 + a2*cos(angs + del*sin(angs))
 zs2 = kap2 * a2 * sin(angs)
 angs = !pi + ((!pi/2.) - arc) * findgen(50)/49.
 rs3 = r01 + a1*cos(angs + del*sin(angs))
 zs3 = kap1 * a1 * sin(angs)
 rs4 = r02 + a2*cos(angs + del*sin(angs))
 zs4 = kap2 * a2 * sin(angs)

 oplot, [rs1, reverse(rs2)], [zs1, reverse(zs2)]
 oplot, [rs3, reverse(rs4)], -[zs3, reverse(zs4)]
 
end


;-----------------------------------------
; plots a gap in a thick d-section
pro plotgap, inpt, outpt, inthk, outthk, toppt, topthk, del, arc, col
; print, 'in plotgap'

 r01 = (inpt+outpt)/2.
 r02 = (inpt+inthk + outpt-outthk)/2.
 a1 = r01 - inpt
 a2 = r02 - inpt - inthk
 kap1 = toppt/a1
 kap2 = (toppt-topthk)/a2

 angs = arc*(findgen(101)/100. - 0.5) + !pi/2.
 rs1 = r01 + a1*cos(angs - del*sin(-1.*angs))
 zs1 = kap1 * a1 * sin(angs)
 rs2 = r02 + a2*cos(angs - del*sin(-1.*angs))
 zs2 = kap2 * a2 * sin(angs)

 polyfill, [rs1, reverse(rs2)], [zs1, zs2], color=col
 polyfill, [rs1, reverse(rs2)], -[zs1, zs2], color=col
end

;-----------------------------------------
; plots a thin d-section
pro plotd, r0, a, del, kap, opl=opl
; print, 'in plotd'
 angs = 2.*!pi*findgen(101)/100.
 rs = r0 + a*cos(angs + del*sin(1.*angs))
 zs = kap * a * sin(angs)
 
 if keyword_set(opl) then oplot, rs, zs else plot, rs, zs, /iso
end

;-----------------------------------------
; plots half a thin d-section
; (use negative kappa for lower half)
pro plotdh, r0, a, del, kap, opl=opl
; print, 'in plotd'
 angs = !pi*findgen(50)/49.
 rs = r0 + a*cos(angs + del*sin(1.*angs))
 zs = kap * a * sin(angs)
 
 if keyword_set(opl) then oplot, rs, zs else plot, rs, zs, /iso
end


;-----------------------------------------
; plots a 2-arc plasma section
pro plotarcs, r0, a, del, kap, snull, opl=opl
; print, 'in plotarcs'

 x1 = (2.*r0*(1.+del) - a*(del^2 + kap^2 -1.))/(2.*(1.+del))
 x2 = (2.*r0*(del-1.) - a*(del^2 + kap^2 -1.))/(2.*(del-1.))
 r1 = 0.5 * sqrt((a^2 * ((del+1.)^2 + kap^2)^2)/((del+1.)^2))
 r2 = 0.5 * sqrt((a^2 * ((del-1.)^2 + kap^2)^2)/((del-1.)^2))

 print, 'plasma arcs: x1, r1, x2, r2'
 print, x1, r1, x2, r2

 theta1 = asin((kap*a)/r1)
 theta2 = asin((kap*a)/r2)

 inang = 1./r1
 outang = 1.5/r2
 
 if (snull eq 0) then begin
  angs1 = (theta1+inang)*((findgen(51)/25.)-1.)+!pi
  angs2 = (theta2+outang)*((findgen(51)/25.)-1.)
 endif else begin
  angs1 = (2.*theta1+inang)*(findgen(51)/50.)+theta1+inang
  angs2 = (2.*theta2+outang)*(findgen(51)/50.)-theta2-outang
 endelse
 
 xs1 = -(r1*cos(angs1) - x1)
 ys1 = r1*sin(angs1)
 xs2 = -(r2*cos(angs2) - x2)
 ys2 = r2*sin(angs2)

 if keyword_set(opl) then oplot, xs1, ys1 else plot, xs1, ys1, xrange=[r0-1.5*a, r0+1.5*a], yrange=[-1.5*kap*a, 1.5*kap*a], /iso
 oplot, xs2, ys2

end


;-----------------------------------------
; gets the radial and vertical build parameters
function get_build, u, otherdat
; print, 'in get_build'

 snull = get_val2('SNULL', u)
 if (snull eq 1) then snull = 1 else snull = 0
 point_lun, u, 0
 newline = ''
 while ~eof(u) do begin
  readf, u, newline
  check = strpos(newline, '* Radial Build *')
  if (check ne -1) then point_lun, -u, pos
 endwhile
 point_lun, u, pos
 for i = 1, 3 do readf, u, newline
 dat = fltarr(37)
 otherdat = fltarr(37)
 
 for i = 0, 19 do begin
  readf, u, newline
  dat[i] = float(strmid(newline, 40, 15))
  otherdat[i] = float(strmid(newline, 55, 15))
 endfor
 
 for i = 1, 5 do readf, u, newline

 if (snull eq 0) then begin
  for i = 20, 27 do begin
   readf, u, newline
   dat[i] = float(strmid(newline, 40, 15))
   otherdat[i] = float(strmid(newline, 55, 15))
  endfor
 endif else begin
   for i = 20, 35 do begin
   readf, u, newline
   dat[i] = float(strmid(newline, 40, 15))
   otherdat[i] = float(strmid(newline, 55, 15))
  endfor
 endelse

 otherdat[36] = snull
 if (snull eq 1) then begin
  dat[36] = (otherdat[20]+otherdat[35])/2.
 endif
 
 return, dat
end


;-----------------------------------------
; gets the TF coil information
function get_tf, u, dat, otherdat, del, inpointsx, inpointsy, outpointsx, outpointsy
; print, 'in get_tf'

 point_lun, u, 0
 newline = ''
 while ~eof(u) do begin
  readf, u, newline
  check = strpos(newline, '* TF Coils *')
  if (check ne -1) then point_lun, -u, pos
 endwhile
 point_lun, u, pos
 for i = 1, 2 do readf, u, newline
 check = strpos(newline, 'Superconducting')
 if (check ne -1) then sc_flag = 1 else sc_flag = 0
 if (sc_flag eq 1) then begin ; if superconducting, get arc centres etc.
  while (strpos(newline, 'by arcs between') eq -1) do readf, u, newline
  for i = 1, 2 do readf, u, newline
  tfxsysin = fltarr(2, 5)
  tfcntsin = fltarr(2, 4)
  tfxsysout = fltarr(2, 7)
  for i = 0, 4 do begin
   readf, u, newline
   tfxsysin[0,i] = float(strmid(newline, 10, 10))
   tfxsysin[1,i] = float(strmid(newline, 25, 10))
  endfor
  for i = 1, 4 do readf, u, newline
  for i = 0, 3 do begin
   readf, u, newline
   tfcntsin[0,i] = float(strmid(newline, 10, 10))
   tfcntsin[1,i] = float(strmid(newline, 25, 10))
  endfor

  farc4tf = (tfxsysin[0, 3] - tfxsysin[0, 2])/(tfxsysin[0, 4] - tfxsysin[0, 2])
  akap = tfxsysin[0, 1] - tfxsysin[0, 0]
  tfxsysout = fltarr(2, 7)
  thks = dat[[4, 25, 19]]
  thks2 = [thks[1] - thks[0], thks[2]-thks[1]]
  thks2 = thks[0:1] + thks2/2.
 
  tfxsysout[*,0] = [otherdat[3], 0.]
  tfxsysout[*,1] = [otherdat[3]+akap, 0.7 * otherdat[25]]
  r1 = sqrt(total((tfxsysin[*,1] - tfcntsin[*,1])^2))
  thet1 = acos((tfxsysin[0,2] - tfcntsin[0,1])/r1)
  thet2 = acos((tfxsysin[0,1] - tfcntsin[0,1])/r1)
  dthet = (thet2 - thet1)/2.
  tfxsysout[*,2] = tfcntsin[*,1] + (thks2[0]+r1)*[cos(thet1+dthet), sin(thet1+dthet)]
  tfxsysout[*,3] = [tfxsysin[0,2], otherdat[25]]
  r1 = sqrt(total((tfxsysin[*,2] - tfcntsin[*,2])^2))
  thet1 = acos((tfxsysin[0,3] - tfcntsin[0,2])/r1)
  thet2 = acos((tfxsysin[0,2] - tfcntsin[0,2])/r1)
  dthet = (thet2 - thet1)/2.
  tfxsysout[*,4] = tfcntsin[*,2] + (thks2[1]+r1)*[cos(thet1+dthet), sin(thet1+dthet)]
  tfxsysout[*,5] = [tfxsysin[0,2] + farc4tf*(otherdat[19] - tfxsysin[0,2]), 0.72 * otherdat[25]]
  tfxsysout[*,6] = [otherdat[19], 0.] 

; process the SC TF coil data into plottable points
  arcpoints, xs1, ys1, tfxsysin[0, 0:1], tfxsysin[1, 0:1], tfcntsin[*,0]
  arcpoints, xs2, ys2, tfxsysin[0, 1:2], tfxsysin[1, 1:2], tfcntsin[*,1]
  arcpoints, xs3, ys3, tfxsysin[0, 2:3], tfxsysin[1, 2:3], tfcntsin[*,2]
  arcpoints, xs4, ys4, tfxsysin[0, 3:4], tfxsysin[1, 3:4], tfcntsin[*,3]

  inpointsx = [xs4, xs3, xs2, xs1, reverse([xs4, xs3, xs2, xs1])]
  inpointsy = [ys4, ys3, ys2, ys1, -1.*reverse([ys4, ys3, ys2, ys1])]

; old method
;  r5 = solve_circle([tfxsysout[0,1], tfxsysout[0,0], tfxsysout[0,1]], [-tfxsysout[1,1], tfxsysout[1,0], tfxsysout[1,1]], x05, y05, angs5)
;  r6 = solve_circle([tfxsysout[0,1], tfxsysout[0,2], tfxsysout[0,3]], [tfxsysout[1,1], tfxsysout[1,2], tfxsysout[1,3]], x06, y06, angs6)
;  r7 = solve_circle([tfxsysout[0,3], tfxsysout[0,4], tfxsysout[0,5]], [tfxsysout[1,3], tfxsysout[1,4], tfxsysout[1,5]], x07, y07, angs7)
;  r8 = solve_circle([tfxsysout[0,5], tfxsysout[0,6], tfxsysout[0,5]], [-tfxsysout[1,5], tfxsysout[1,6], tfxsysout[1,5]], x08, y08, angs8)
;
;  angs5[0] = angs5[0] + 2.*!pi
;  angsp5 = (angs5[0] - angs5[1])*findgen(101)/100. + angs5[1]
;  angsp6 = (angs6[0] - angs6[1])*findgen(101)/100. + angs6[1]
;  angsp7 = (angs7[0] - angs7[1])*findgen(101)/100. + angs7[1]
;  angsp8 = (angs8[0] - angs8[1])*findgen(101)/100. + angs8[1]
; 
;  x15 = r5 * cos(angsp5) + x05 & y15 = r5 * sin(angsp5) + y05
;  x16 = r6 * cos(angsp6) + x06 & y16 = r6 * sin(angsp6) + y06
;  x17 = r7 * cos(angsp7) + x07 & y17 = r7 * sin(angsp7) + y07
;  x18 = r8 * cos(angsp8) + x08 & y18 = r8 * sin(angsp8) + y08
;  outpointsx = [reverse(x18), x17, x16, x15, reverse([x17, x16])]
;  outpointsy = [reverse(y18), y17, y16, y15, -1.*reverse([y17, y16])]
  
; new method
  inwid = otherdat[18] - otherdat[4]
  outwid = inwid + dat[4] + dat[19]
  outpointsx = ((inpointsx - otherdat[4])*(outwid/inwid)) + otherdat[3]
;  if (otherdat[27] eq 0.) then topat=27 else topat=26
  extern = (otherdat[27]/otherdat[26])
  if (otherdat[36] eq 1) then extern = (otherdat[20] - otherdat[35])/(otherdat[21] - otherdat[34])
  outpointsy = inpointsy * extern
  
  ; then get superconductor type
  while (strpos(newline, '* Superconducting TF Coils *') eq -1) do readf, u, newline
  for i = 1, 2 do readf, u, newline
  type = strtrim(strmid(newline, 21), 2)
  
 endif else begin
; Conventional (Cu) TF coil 

  inpt = otherdat[3]
  outpt = otherdat[19]
  inthk = dat[4]
  outthk = dat[19]
  if (otherdat[26] eq 0.) then topat=25 else topat=26
  toppt = otherdat[topat]
  topthk = dat[topat]

  r01 = (inpt+outpt)/2.
  r02 = (inpt+inthk + outpt-outthk)/2.
  a1 = r01 - inpt
  a2 = r02 - inpt - inthk
  kap1 = toppt/a1
  kap2 = (toppt-topthk)/a2
  
  angs = 2.*!pi*findgen(1001)/1000.
  outpointsx = r01 + a1*cos(angs - del*sin(-1.*angs))
  outpointsy = kap1 * a1 * sin(angs)
  inpointsx = r02 + a2*cos(angs - del*sin(-1.*angs))
  inpointsy = kap2 * a2 * sin(angs)
 
  type = 'Cu'
 endelse

 outpointsy = outpointsy + dat[36]
 inpointsy = inpointsy + dat[36]

 return, type
end

;-----------------------------------------
; gets the PF coil information
function get_coils, u, coilnames
; print, 'in get_coils'

 point_lun, u, 0
 newline = ''
 while ~eof(u) do begin
  readf, u, newline
  check = strpos(newline, 'Geometry of PF coils, OH coil')
  if (check ne -1) then point_lun, -u, pos
 endwhile
 point_lun, u, pos
 for i = 1, 4 do readf, u, newline
 lcount = 0
 while (strlen(newline) gt 5) do begin
  readf, u, newline
  lcount = lcount + 1
 endwhile
 point_lun, u, pos
 for i = 1, 3 do readf, u, newline
 dat = fltarr(lcount, 4)
 coilnames = strarr(lcount)
 dats = fltarr(4)
 for i = 0, lcount-1 do begin
  readf, u, newline
  coilnames[i] = strtrim(strmid(newline, 0, 10), 2)
  newline = strmid(newline, 10)
  reads, newline, dats
  dat[i,*] = dats
 endfor
 
 return, dat
end
 
;-----------------------------------------
; plots a coil
pro plot_coil, spec, name
; print, 'in plot_coil'

; spec is an array [r, z, dr, dz]
 rs = [spec[0]-0.5*spec[2], spec[0]-0.5*spec[2], spec[0]+0.5*spec[2], spec[0]+0.5*spec[2], spec[0]-0.5*spec[2]]
 zs = [spec[1]-0.5*spec[3], spec[1]+0.5*spec[3], spec[1]+0.5*spec[3], spec[1]-0.5*spec[3], spec[1]-0.5*spec[3]]
; polyfill, rs, zs, color=col
 oplot, rs, zs
 xyouts, spec[0]-0.45*spec[2], spec[1]-0.125*spec[3], name, charsize = 0.5
end

;-----------------------------------------
; main program
pro plot_proc2, help=help, fn=fn, ps=ps, gf=gf
; check to see if help has been requested
 if keyword_set(help) then begin
  print, 'Usage:'
  print, 'plot_proc [, fn=filename, /ps, /gf]'
  print, ''
  print, 'Plots PROCESS radial build and summary of parameters.'
  print, 'Should be run in the folder containing OUT.DAT.'
  print, ' -- defining filename uses that instead of OUT.DAT'
  print, ' -- including /ps plots to an eps file'
  print, ' -- using /gf prompts for an input filename'
  print, '       (equivalent to fn=dialog_pickfile())'
  print, ''
  print, 'Contact richard.kemp@ccfe.ac.uk with comments or complaints.'
  print, ''
  goto, theend
 endif

 cd, current=strpwd
 if ~(keyword_set(fn)) then fn = strpwd+'/OUT.DAT'
 if (file_test(fn) eq 0) || (keyword_set(gf)) then fn = dialog_pickfile(filter='*.DAT', title='Select OUT.DAT file to open')
 if (fn eq '') then goto, theend
 if (file_test(fn) eq 0) then begin
  print, "File " + fn + " not found."
  goto, theend
 endif

 get_lun, u
 openr, u, fn
 dat = get_build(u, otherdat)
 kappa = get_val('kappa95', u)
 triang = get_val('triang95', u)
 
; get the coils
 coils = get_coils(u, cnames)
 ncoils = (size(coils))[1] - 1
; print, cnames
 
; set boundaries
 rmin = 0
 rmax = max([otherdat[0:19], coils[*,0]+0.5*coils[*,2]])
 zmax = max([abs(otherdat[20:35]), abs(coils[*,1])+0.5*coils[*,3]])
 zmin = -zmax
 
; set colours: [PF/shield, TF, cryo, blanket, FW, SOL, gaps(background)]
; screen_cols = ['ffffff'x, '0000ff'x, 'ffff00'x, '00ff00'x, '00ffff'x, 'ff0000'x, '000000'x] 
; ps_cols = ['000000'x, 'bbbbbb'x, '333333'x, 'bbbbbb'x, '777777'x, 'bbbbbb'x, 'ffffff'x]
 
; if (keyword_set(ps)) then cols = ps_cols else cols = screen_cols

; do the plotting
 !p.multi = [0, 3, 2]
 if ~(keyword_set(ps)) then begin
  set_plot, 'x'
  window, xsize = 891, ysize=630
  nocol = '000000'x
 endif else begin
  set_plot, 'ps'
  psfn = dialog_pickfile(filter='*.eps', title='Saving OUT.eps file')
  if (psfn eq '') then psfn = 'OUT.eps'
  nocol = 'ffffff'x
  if (psfn eq '') then begin
   set_plot, 'x'
   print, 'No filename -- printing to screen'
   nocol = '000000'x
  endif
;  !x.omargin = [18, 0]
  if ~(psfn eq '') then device, filename=psfn, /encap, /landscape
 endelse

; plot radial build
;--
; set up plot
; if (keyword_set(neww)) then window, xsize = 640, ysize=800
 plot, [otherdat[11], otherdat[11]], [0, 0], psym=1, xrange=[rmin, rmax], yrange=[zmin, zmax], /iso, xtitle = 'R / m', ytitle = 'Z / m', title='Radial build'
 
; plot coils
 for i = 0, ncoils-1 do plot_coil, coils[i,*], cnames[i]

; plot tf coil
 tf_type = get_tf(u, dat, otherdat, triang, insx, insy, outsx, outsy)
 oplot, insx, insy
 oplot, outsx, outsy
 
 if (otherdat[36] ne 1) then begin
 ; double null version
 ; plot cryostat and gap
  plotthick, otherdat[4], otherdat[18], dat[5], dat[18], otherdat[26], dat[26], triang
 ; plot shield
  plotthick, otherdat[6], otherdat[16], dat[7], dat[16], otherdat[24], dat[24], triang
 ; plot blanket, first wall, and divertor chunk
  plotdgap, otherdat[7], otherdat[15], dat[8], dat[15], otherdat[23], dat[23]/2., triang
  plotdgap, otherdat[8], otherdat[14], dat[9], dat[14], otherdat[23]-dat[23]/2., dat[23]/2., triang
 ; plot SOL
  plotdgap, otherdat[9], otherdat[13], dat[10], dat[13], otherdat[22], dat[22], triang
;--
 endif else begin
 ; single null
 ; plot cryostat upper
  for i = 0, 1 do begin
   radx = (otherdat[18-i] + otherdat[4+i])/2.
   rminx = (otherdat[18-i] - otherdat[4+i])/2.
   kapx = otherdat[21+i]/rminx
   plotdh, radx, rminx, triang, kapx, /opl
  endfor
  ; plot shield, blanket, FW upper
  for i = 0, 3 do begin
   radx = (otherdat[16-i] + otherdat[6+i])/2.
   rminx = (otherdat[16-i] - otherdat[6+i])/2.
   kapx = otherdat[23+i]/rminx
   plotdh, radx, rminx, triang, kapx, /opl
  endfor
  ; plot cryostat, shield lower
  for i = 0, 1 do begin
   radx = (otherdat[18-i] + otherdat[4+i])/2.
   rminx = (otherdat[18-i] - otherdat[4+i])/2.
   kapx = otherdat[34-i]/rminx
   plotdh, radx, rminx, triang, kapx, /opl
  endfor
  for i = 0, 1 do begin
   radx = (otherdat[16-i] + otherdat[6+i])/2.
   rminx = (otherdat[16-i] - otherdat[6+i])/2.
   kapx = otherdat[32-i]/rminx
   plotdh, radx, rminx, triang, kapx, /opl
  endfor
  ; plot lower blanket, FW
  plotdhgap, otherdat[7], otherdat[15], dat[8], dat[15], otherdat[31], -dat[24], triang
  plotdhgap, otherdat[8], otherdat[14], dat[9], dat[14], otherdat[31]+dat[24], -dat[25], triang
 endelse
; plot plasma
 plotarcs, otherdat[11], dat[11], 1.5*triang, 1.1*kappa+0.04, otherdat[36], /opl

;--
; print parameters
 charsz = 0.75
 data = gather_info(u, info, tf_type)
; field 1: geometry
 plot, [0, 1], [0, 1], color = nocol
 field = 1
 count = 0
 xyouts, -0.05, 0.95, 'Geometry:', charsize=charsz
 while (data[0, count] eq field) do begin
  xyouts, 0, 0.85-(count*0.05), strpad(data[2, count], 25), charsize=charsz
  xyouts, 0.5, 0.85-(count*0.05), ' = '+data[3, count]+' '+data[4, count], charsize=charsz
  count++
 endwhile
 xyouts, 0, 0.25, info[0], charsize=charsz
 xyouts, 0, 0.2, info[1], charsize=charsz
 xyouts, 0, 0.15, info[2], charsize=charsz
 xyouts, 0, 0.1, info[3], charsize=charsz
 xyouts, 0, 0.05, 'See PROCESS user guide for radial build details', charsize=charsz
; --- check optimisation
 optim = get_val('ioptimz', u)
 if (optim ge 0) then begin
  swit = get_val('minmax', u)
  minmax = swit/abs(swit)
  swit = abs(swit)
  case swit of
   1: opvar = 'plasma major radius'
   2: opvar = 'ratio fusion power:input power'
   3: opvar = 'neutron wall load'
   4: opvar = 'total TF + PF coil power'
   5: opvar = 'ratio fusion power:injection power'
   6: opvar = 'cost of electricity'
   7: opvar = 'constructed cost'
   8: opvar = 'aspect ratio'
   9: opvar = 'divertor heat load'
   10: opvar = 'toroidal field on axis'
   11: opvar = 'injection power'
   14: opvar = 'pulse length'
  endcase
  if (minmax gt 0) then opmode = 'minimise ' else opmode = 'maximise '
  xyouts, 0, -0.00, 'Optimised to '+opmode+opvar+'.', charsize=charsz
 endif
 xyouts, 0, -0.05, fn, charsize=charsz

;--
; field 2: physics
 plot, [0, 1], [0, 1], color = nocol
 field = 2
 offset=count
 xyouts, -0.05, 0.95, 'Physics:', charsize=charsz
 while (data[0, count] eq field) do begin
  xyouts, 0, 0.85-((count-offset)*0.05), strpad(data[2, count], 25), charsize=charsz
  xyouts, 0.5, 0.85-((count-offset)*0.05), ' = '+data[3, count]+' '+data[4, count], charsize=charsz
  count++
 endwhile
;--
; field 3: magnetics
 plot, [0, 1], [0, 1], color = nocol
 field = 3
 offset=count
 xyouts, -0.05, 0.95, 'Coil currents etc:', charsize=charsz
 while (data[0, count] eq field) do begin
  xyouts, 0, 0.85-((count-offset)*0.05), strpad(data[2, count], 25), charsize=charsz
  xyouts, 0.5, 0.85-((count-offset)*0.05), ' = '+data[3, count]+' '+data[4, count], charsize=charsz
  count++
 endwhile
; count++
 offset = offset - 2
 xyouts, 0.1, 0.9 -((count-offset)*0.05), 'TF coil type is '+tf_type, charsize=charsz
 field = 35
 while (data[0, count] eq field) do begin
  xyouts, 0, 0.85-((count-offset)*0.05), strpad(data[2, count], 25), charsize=charsz
  xyouts, 0.5, 0.85-((count-offset)*0.05), ' = '+data[3, count]+' '+data[4, count], charsize=charsz
  count++
 endwhile
;  xyouts, 0, 0, fn, charsize=charsz

;--
; field 4: power flows, economics
 plot, [0, 1], [0, 1], color = nocol
 field = 4
 offset=count
 xyouts, -0.05, 0.95, 'Power flows/economics:', charsize=charsz
 while (data[0, count] eq field) do begin
  xyouts, 0, 0.85-((count-offset)*0.05), strpad(data[2, count], 25), charsize=charsz
  xyouts, 0.5, 0.85-((count-offset)*0.05), ' = '+data[3, count]+' '+data[4, count], charsize=charsz
  count++
 endwhile
;--

;--
; plot sweep
 temp = !y
 !y.margin=[4., 36.]
 procit2, u, result
 if (result eq -1) then begin
  plot, [0, 1], [0, 1], color = nocol
  xyouts, 0.5, 0.5, 'NO SWEEP'
  result = 1.
 endif
;--

; field 5: current drive (above sweep plot)
 field = 5
 offset=count
; maxyval = 2.5*!y.crange[1]
; !y.crange=[0, 1]
; !y.margin=[4., 2.]
 !y = temp
 xyouts, -0.05*result, 0.95, data[2, count], charsize=charsz & count++
 while (data[0, count] eq field) do begin
  xyouts, 0, (0.9-(count-offset)*0.05), strpad(data[2, count], 25), charsize=charsz
  xyouts, 0.5*result, (0.9-(count-offset)*0.05), ' = '+data[3, count]+' '+data[4, count], charsize=charsz
  count++
 endwhile

; -- cleanup
 !p.multi = [0, 1, 1]
 !y.margin=[4., 2.]
 !x.omargin=0
 !y.omargin=0
 if (keyword_set(ps)) then device, /close
 if (keyword_set(ps)) then set_plot, 'x'
 free_lun, u
theend:
end
