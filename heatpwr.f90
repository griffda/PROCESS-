!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine power1

  !+ad_name  power1
  !+ad_summ  Calculates the first part of the heat transport
  !+ad_summ  and plant power balance constituents
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calculates the first part of the heat transport
  !+ad_desc  and plant power balance constituents.
  !+ad_prob  None
  !+ad_call  physics_variables
  !+ad_call  blanket.h90
  !+ad_call  cdriv.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  pfcoil.h90
  !+ad_call  pwrcom.h90
  !+ad_call  struccom.h90
  !+ad_call  tfcoil.h90
  !+ad_call  times.h90
  !+ad_call  cryo
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use physics_variables

  implicit none

  include 'blanket.h90'
  include 'cdriv.h90'
  include 'fwblsh.h90'
  include 'htpwr.h90'
  include 'pfcoil.h90'
  include 'pwrcom.h90'
  include 'struccom.h90'
  include 'tfcoil.h90'
  include 'times.h90'

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Primary nuclear heating

  pfwdiv = pfuscmw + 1.0D-6 * (pinje + pinji)
  pthermmw = pnucblkt + pnucshld + (1.0D0-ffwlg)*pfwdiv
  priheat = pnucblkt + pnucshld + pfwdiv

  if (iprimhtp == 1) then
     pthermmw = pthermmw + htpmw
     priheat = priheat + htpmw
  end if

  !  Number of primary heat exchangers

  rnphx = max(2.0D0, (priheat/400.0D0 + 0.8D0) )

  !  For the Rankine cycle employed by the new (1993) blanket model,
  !  the number of primary heat exchangers is two

  if (lblnkt == 1) rnphx = 2.0D0

  !  Secondary heat (some of it... rest calculated in POWER2)

  !  Wall plug injection power

  pinjwp = 1.0D-6 * (echpwr/etaech + plhybd/etalh + pnbeam/etanbi + &
       pofcd/etaof)

  !  Waste injection power 

  pinjht = pinjwp - 1.0D-6*(pinji + pinje)

  !  Cryogenic power

  if ((itfsup /= 1).and.(ipfres == 1)) then  !  no superconducting coils
     helpow = 0.0D0
  else
     call cryo(itfsup,ipfres,tfsai,coldmass,ptfnuc,ensxpfm, &
          tpulse,cpttf,tfno,helpow)
  end if

  !  Use 13% of ideal Carnot efficiency to fit J. Miller estimate

  crypmw = 1.0D-6 * (293.0D0 - tmpcry)/(0.13D0*tmpcry) * helpow

end subroutine power1

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine power2(outfile,iprint)

  !+ad_name  power2
  !+ad_summ  Calculates the remainder of the heat transport
  !+ad_summ  and plant power balance constituents
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the rest of the heat transport
  !+ad_desc  and plant power balance constituents, not already calculated in
  !+ad_desc  <A HREF="acpow.html">ACPOW</A> or <A HREF="power1.html">POWER1</A>.
  !+ad_prob  None
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  blanket.h90
  !+ad_call  cost.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  tfcoil.h90
  !+ad_call  blanket
  !+ad_call  oblnkl
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarre
  !+ad_call  ovarrf
  !+ad_hist  23/01/97 PJK Initial version
  !+ad_hist  10/09/97 PJK Removed IF-statement that bypassed coding if iprint=1
  !+ad_hist  15/06/04 PJK Added use of IPRIMHTP, added HTPMW to PRECIR
  !+ad_hist  22/05/07 PJK Added hydrogen plant power requirements
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use physics_variables
  use process_output

  implicit none

  include 'blanket.h90'
  include 'cost.h90'
  include 'fwblsh.h90'
  include 'htpwr.h90'
  include 'tfcoil.h90'

  !  Arguments

  integer, intent(in) :: outfile,iprint

  !  Local variables

  real(kind(1.0D0)) :: ppumpmw,precir

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Centrepost coolant pump power

  ppumpmw = 1.0D-6 * ppump

  !  Facility heat removal (fcsht calculated in ACPOW)

  facht = fcsht

  !  Hydrogen plant powers

  if (ihplant == 0) then
     helecmw = 0.0D0
     hthermmw = 0.0D0
     hpower = 0.0D0
  else if (ihplant == 1) then
     hthermmw = 0.0D0
     hpower = etahlte * helecmw
  else if (ihplant == 2) then
     hthermmw = 0.48D0 * helecmw
     hpower = etahhten * helecmw
  else if (ihplant == 3) then
     hthermmw = 0.19D0 * helecmw
     hpower = etahhtex * helecmw
  else
     helecmw = 0.0D0
     hpower = etahth * hthermmw
  end if

  !  Total secondary heat

  psecht = pinjht + pnucloss + facht + vachtmw + trithtmw + &
       tfcmw + crypmw + ppumpmw + helecmw + hthermmw

  if (iprimhtp == 0) psecht = psecht + htpmw

  !  Total plant heat removal

  ctht = priheat + psecht 

  !  Number of intermediate heat exchangers

  rnihx = max(2.0D0, (ctht/50.0D0 + 0.8D0) )

  !  For the Rankine cycle employed by the new (1993) blanket model,
  !  the number of intermediate heat exchangers is set to equal the number
  !  of feed water heater pumps

  if (lblnkt == 1) rnihx = real(nipfwh+nlpfwh, kind(1.0D0))

  !  Calculate powers relevant to a power-producing plant

  if (ireactor == 1) then

     !  Gross electric power

     if (lblnkt == 1) then
        call blanket(2,outfile,iprint)
     else
        pgrossmw = (pthermmw-hthermmw) * etath
     end if

     !  Balance of plant recirculating power

     fgrosbop = min( 0.5D0, ( fauxbop/(pgrossmw/1000.0D0)**0.6D0) )

     !  Total recirculating power

     precir = fgrosbop * pgrossmw + pinjwp + tfcmw + crypmw + &
          ppumpmw + htpmw + helecmw

     !  Net electric power

     pnetelmw = pgrossmw - precir

     !  Scaling to prevent negative pnetelmw

     if ( (pnetelmw < 1.0D0).and.(ipnet == 0) ) then
        pnetelmw = 1.0D0 / ( 1.0D0 + abs(pnetelmw-1.0D0))
     end if

  end if

  if ((iprint == 0).or.(sect14 == 0)) return

  !  Output section

  call oheadr(outfile,'Power / Heat Transport')
  call ovarre(outfile,'Fusion power (MW)','(powfmw)',powfmw)
  call ovarre(outfile,'Charged fusion power (MW)','(pfuscmw)',pfuscmw)
  call ovarre(outfile,'Neutron power escaping via holes (MW)', &
       '(pnucloss)',pnucloss)
  call ovarre(outfile,'Neutron power multiplication','(emult)',emult)
  call ovarre(outfile,'Injector wall plug power (MW)','(pinjwp)' &
       ,pinjwp)
  call ovarre(outfile,'TF coil resistive power (MW)','(tfcmw)',tfcmw)
  call ovarre(outfile,'Centrepost coolant pump power (MW)','(ppumpmw)' &
       ,ppumpmw)
  call ovarre(outfile,'Primary heat (MW)','(pthermmw)',pthermmw)
  call ovarre(outfile,'Secondary heat (MW)','(psecht)',psecht)
  call oblnkl(outfile)
  call ovarre(outfile,'Heat removal from F.W./divertor (MW)', &
       '(pfwdiv)',pfwdiv)
  call ovarre(outfile,'Heat removal from blankets (MW)', &
       '(pnucblkt)',pnucblkt)
  call ovarre(outfile,'Heat removal from shield (MW)','(pnucshld)', &
       pnucshld)
  call ovarre(outfile,'Heat removal from injection power (MW)', &
       '(pinjht)',pinjht)
  call ovarre(outfile,'Heat removal from cryogenic plant (MW)', &
       '(crypmw)',crypmw)
  call ovarre(outfile,'Heat removal from vacuum pumps (MW)', &
       '(vachtmw)',vachtmw)
  call ovarre(outfile,'Heat removal from tritium plant (MW)', &
       '(trithtmw)',trithtmw)

  if (ihplant /= 0) then
     call ovarre(outfile,'Electrical pwr used for H production (MW)', &
          '(helecmw)',helecmw)
     call ovarre(outfile,'Thermal power used for H production (MW)', &
          '(hthermmw)',hthermmw)
     call ovarre(outfile,'Hydrogen production rate (MW)', &
          '(hpower)',hpower)
     call ovarre(outfile,'Hydrogen production rate (Nm3/sec)', &
          '(hpower/13)',hpower/13.0D0)
  end if

  call ovarre(outfile,'Total cryogenic load (MW)','(helpow/1.D6)', &
       helpow/1.0D6)
  call ovarre(outfile,'Heat removal from facilities (MW)','(facht)', &
       facht)
  call ovarrf(outfile,'Number of primary heat exchangers','(rnphx)', &
       rnphx)
  call ovarrf(outfile,'Number of intermediate heat exchangers', &
       '(rnihx)',rnihx)
  call ovarre(outfile,'Total plant heat rejection (MW)','(ctht)',ctht)

  if (ireactor /= 1) return

  call osubhd(outfile,'Reactor powers :')
  call ovarre(outfile,'Gross electric power (MW)','(pgrossmw)', &
       pgrossmw)
  call ovarre(outfile,'Net electric power (MW)','(pnetelmw)',pnetelmw)
  call ovarre(outfile,'Balance of plant aux. power fraction', &
       '(fgrosbop)',fgrosbop)
  call ovarre(outfile,'First wall low grade heat fraction','(ffwlg)', &
       ffwlg)

end subroutine power2

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cryo(itfsup,ipfres,tfsai,coldmass,ptfnuc,ensxpfm,tpulse, &
     cpttf,tfno,helpow)

  !+ad_name  cryo
  !+ad_summ  Calculates cryogenic loads
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  itfsup : input integer : Switch denoting whether TF coils are
  !+ad_argc                           superconducting
  !+ad_args  ipfres : input integer : Switch denoting whether PF coils are resistive
  !+ad_args  tfsai : input real : Inboard TF coil surface area (m2)
  !+ad_args  coldmass : input real : Mass of cold (cryogenic) components (kg),
  !+ad_argc                          including TF coils, PF coils, cryostat, and
  !+ad_argc                          intercoil structure
  !+ad_args  ptfnuc : input real : Nuclear heating in TF coils (MW)
  !+ad_args  ensxpfm : input real : Maximum PF coil stored energy (MJ)
  !+ad_args  tpulse : input real : Pulse length of cycle (s)
  !+ad_args  cpttf : input real : Current per turn in TF coils (A)
  !+ad_args  tfno : input real : Number of TF coils
  !+ad_args  helpow : output real : Helium heat removal at cryo temperatures (W)
  !+ad_desc  This routine calculates the cryogenic heat load.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  02/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  D. Slack memo SCMDG 88-5-1-059, LLNL ITER-88-054, Aug. 1988
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none


  !  Arguments

  integer, intent(in) :: itfsup,ipfres
  real(kind(1.0D0)), intent(in) :: coldmass,cpttf,ensxpfm,ptfnuc,tfno, &
       tfsai,tpulse
  real(kind(1.0D0)), intent(out) :: helpow

  !  Local variables

  real(kind(1.0D0)) :: qac,qcl,qnuc,qss

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Steady state loads (W)

  qss = 4.3D-4 * coldmass
  if (itfsup == 1) qss = qss + 2.0D0*tfsai

  !  Nuclear heating of TF coils (W) (zero if resistive)

  qnuc = 1.0D6 * ptfnuc

  !  AC losses

  qac = 1.0D3 * ensxpfm/tpulse

  !  Current leads

  if (itfsup == 1) then
     qcl = 13.6D-3 * tfno * cpttf
  else
     qcl = 0.0D0
  end if

  !  Total includes 45% extra miscellaneous, piping and reserves

  helpow = max(0.0D0, (1.45D0 * (qss + qnuc + qac + qcl)) )

end subroutine cryo
