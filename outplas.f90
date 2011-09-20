!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine outplas(nout)

  !+ad_name  outplas
  !+ad_summ  Subroutine to output the plasma physics information
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Output file channel
  !+ad_desc  This routine writes the plasma physics information
  !+ad_desc  to a file, in a tidy format.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  ineq.h90
  !+ad_call  phydat.h90
  !+ad_call  cdriv.h90
  !+ad_call  labels.h90
  !+ad_call  times.h90
  !+ad_call  osections.h90
  !+ad_call  rfp.h90
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarre
  !+ad_call  ovarrf
  !+ad_hist  17/09/97 PJK Upgrade to higher standard of coding. Added
  !+ad_hisc               Greenwald density limit
  !+ad_hist  17/11/97 PJK Added additional beta diagnostics
  !+ad_hist  01/04/98 PJK Added dnla to output, and comment about ignition
  !+ad_hist  17/07/98 PJK Added power threshold scalings
  !+ad_hist  19/01/99 PJK Added powerht and minor word changes
  !+ad_hist  16/07/01 PJK Added kappaa
  !+ad_hist  18/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'ineq.h90'
  include 'phydat.h90'
  include 'cdriv.h90'
  include 'labels.h90'
  include 'times.h90'
  include 'osections.h90'
  include 'rfp.h90'

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  real(kind(1.0D0)) :: betath

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (sect03 == 0) return

  call oheadr(nout,'Plasma')

  select case (idivrt)
  case (0)
     call ocmmnt(nout,'Plasma configuration = limiter')
  case (1)
     call ocmmnt(nout,'Plasma configuration = single null divertor')
  case (2)
     call ocmmnt(nout,'Plasma configuration = double null divertor')
  case default
     write(nout,*) 'Error in routine OUTPLAS:'
     write(nout,*) 'Illegal value of idivrt, = ',idivrt
     write(nout,*) 'PROCESS stopping.'
     stop
  end select
  call oblnkl(nout)

  if (idhe3 == 0) then
     if (iiter == 1) then
        call ocmmnt(nout,'Deuterium - Tritium fusion reaction assumed')
        call oblnkl(nout)
        call ocmmnt(nout,'ITER fusion power calculations were performed')
     end if
  else
     call ocmmnt(nout,'Deuterium - Helium3 fusion reaction assumed')
  end if

  call osubhd(nout,'Plasma Geometry :')
  call ovarrf(nout,'Major radius (m)','(rmajor)',rmajor)
  call ovarrf(nout,'Minor radius (m)','(rminor)',rminor)
  call ovarrf(nout,'Aspect ratio','(aspect)',aspect)
  call ovarrf(nout,'Elongation, X-point','(kappa)',kappa)
  call ovarrf(nout,'Elongation, 95% surface','(kappa95)',kappa95)
  call ovarrf(nout,'Elongation, area ratio calc.','(kappaa)',kappaa)
  call ovarrf(nout,'Triangularity, X-point','(triang)',triang)
  call ovarrf(nout,'Triangularity, 95% surface','(triang95)',triang95)
  call ovarre(nout,'Plasma surface area (m2)','(sarea)',sarea)
  call ovarre(nout,'Plasma volume (m3)','(vol)',vol)

  call osubhd(nout,'Current and Field :')
  call ovarrf(nout,'Plasma current (MA)','(plascur/1D6)',plascur/1.0D6)

  if (irfp == 0) then
     call ovarrf(nout,'Vacuum toroidal field at R (T)','(bt)',bt)
     call ovarrf(nout,'Average poloidal field (T)','(bp)',bp)
  else
     call ovarrf(nout,'Toroidal field at plasma edge (T)','(-bt)',-bt)
     call ovarrf(nout,'Poloidal field at plasma edge (T)','(bp)',bp)
     call ovarrf(nout,'Reversal parameter F','(rfpf)',rfpf)
     call ovarrf(nout,'Pinch parameter theta','(rfpth)',rfpth)
  end if

  call ovarrf(nout,'Total field (T)','(btot)',btot)
  call ovarrf(nout,'Edge safety factor','(q)',q)
  call ovarrf(nout,'Cylindrical safety factor','(qstar)',qstar)

  if (ishape == 1) then
     call ovarrf(nout,'Lower limit for edge safety factor','(qlim)',qlim)
  end if

  if (icurr == 2) then
     call ovarrf(nout,'Safety factor at 95% flux','(q95)',q95)
     call ocmmnt(nout,'Mean safety factor, q-bar, used for q')
  end if

  call osubhd(nout,'Beta Information :')
  call ovarrf(nout,'Total plasma beta','(beta)',beta)
  call ovarrf(nout,'Total poloidal beta','(betap)',betap)
  call ovarrf(nout,'Total toroidal beta',' ',beta*(btot/bt)**2)
  call ovarrf(nout,'Fast alpha beta','(betaft)',betaft)
  call ovarrf(nout,'Beam ion beta','(betanb)',betanb)

  betath = beta-betaft-betanb
  call ovarrf(nout,'Thermal beta',' ',betath)
  call ovarrf(nout,'Thermal poloidal beta',' ',betath*(btot/bp)**2)
  call ovarrf(nout,'Thermal toroidal beta (= beta-exp)',' ', &
       betath*(btot/bt)**2)

  call ovarrf(nout,'2nd stability beta : beta_p / (R/a)', &
       '(eps*betap)',eps*betap)
  call ovarrf(nout,'2nd stability beta upper limit','(epbetmax)', &
       epbetmax)

  call ovarrf(nout,'Troyon g coefficient','(dnbeta)',dnbeta)
  call ovarrf(nout,'Normalised beta',' ',fbetatry*dnbeta)

  if (itart == 1) then
     call ovarrf(nout,'Normalised thermal toroidal beta', &
          ' ',fbetatry*dnbeta*btot**2/bt**2)
  end if

  if (iculbl == 0) then
     call ovarrf(nout,'Limit on total beta','(betalim)',betalim)
  else if (iculbl == 1) then
     call ovarrf(nout,'Limit on thermal beta','(betalim)',betalim)
  else
     call ovarrf(nout,'Limit on thermal + NB beta','(betalim)', &
          betalim)
  end if

  call osubhd(nout,'Temperature and Density (volume averaged) :')
  call ovarrf(nout,'Electron temperature (keV)','(te)',te)
  call ovarrf(nout,'Ion temperature (keV)','(ti)',ti)
  call ovarrf(nout,'Electron temp., density weighted (keV)','(ten)',ten)
  call ovarre(nout,'Electron density (/m3)','(dene)',dene)
  call ovarre(nout,'Line-averaged electron density (/m3)','(dnla)',dnla)
  call ovarre(nout,'Ion density (/m3)','(dnitot)',dnitot)
  call ovarre(nout,'Fuel density (/m3)','(deni)',deni)
  call ovarre(nout,'High Z impurity density (/m3)','(dnz)',dnz)
  call ovarre(nout,'Cold alpha ash density (/m3)','(dnalp)',dnalp)

  if (idhe3 == 1) then
     call ovarre(nout,'Proton ash density (/m3)','(dnprot)',dnprot)
  end if

  call ovarre(nout,'Hot beam density (/m3)','(dnbeam)',dnbeam)
  call ovarre(nout,'Density limit (enforced) (/m3)','(dnelimt)',dnelimt)
  call ovarre(nout,'Seeded iron concentration','(cfe0)',cfe0)
  call ovarre(nout,'Effective charge','(zeff)',zeff)
  call ovarre(nout,'Mass weighted effective charge','(zeffai)',zeffai)
  call ovarrf(nout,'Density profile factor','(alphan)',alphan)
  call ovarrf(nout,'Temperature profile factor','(alphat)',alphat)

  if (iculdl == 1) then
     call osubhd(nout,'Density Limit using different models :')
     call ovarre(nout,'Old ASDEX model','(dlimit(1))',dlimit(1))
     call ovarre(nout,'Borrass ITER model I','(dlimit(2))',dlimit(2))
     call ovarre(nout,'Borrass ITER model II','(dlimit(3))',dlimit(3))
     call ovarre(nout,'JET edge radiation model','(dlimit(4))',dlimit(4))
     call ovarre(nout,'JET simplified model','(dlimit(5))',dlimit(5))
     call ovarre(nout,'Hugill-Murakami Mq model','(dlimit(6))',dlimit(6))
     call ovarre(nout,'Greenwald model','(dlimit(7))',dlimit(7))
  end if

  call osubhd(nout,'Fuel Constituents :')
  if (idhe3 == 0) then
     call ovarre(nout,'Deuterium fuel fraction','(1-ftr)',1.0D0-ftr)
     call ovarre(nout,'Tritium fuel fraction','(ftr)',ftr)
  else
     call ovarre(nout,'Deuterium fuel fraction','(fdeut)',fdeut)
     call ovarre(nout,'Tritium fuel fraction','(ftrit)',ftrit)
     call ovarre(nout,'3-Helium fuel fraction','(fhe3)',fhe3)
  end if

  call osubhd(nout,'Fusion Power :')
  call ovarre(nout,'Fusion power (MW)','(powfmw)',powfmw)
  call ovarre(nout,'Alpha power: total (MW)','(alpmw)',alpmw)
  call ovarre(nout,'Alpha power: beam-plasma (MW)','(palpnb)',palpnb)

  if (idhe3 == 1) then
     call ovarre(nout,'Neutron power (MW)','(pneut*vol)',pneut*vol)
     call ovarre(nout,'Proton power (MW)','(pcharge*vol)',pcharge*vol)
  end if

  call ovarre(nout,'Neutron wall load (MW/m2)','(wallmw)',wallmw)
  call ovarrf(nout,'Fraction of power to electrons','(falpe)',falpe)
  call ovarrf(nout,'Fraction of power to ions','(falpi)',falpi)

  call osubhd(nout,'Plasma Power Balance :')
  call ovarre(nout,'Ohmic heating power (MW)','(pohmpv*vol)',pohmpv*vol)
  call ovarre(nout,'Bremsstrahlung radiation power (MW)','(pbrem*vol)', &
       pbrem*vol)
  call ovarre(nout,'Synchrotron radiation power (MW)','(psync*vol)', &
       psync*vol)
  call ovarre(nout,'Synchrotron reflection factor','(ssync)',ssync)
  call ovarre(nout,'Scrape-off line radiation power (MW)','(plrad*vol)', &
       plrad*vol)
  call ovarre(nout,'Ion transport (MW)','(ptri*vol))',ptri*vol)
  call ovarre(nout,'Electron transport (MW)','(ptre*vol)',ptre*vol)
  call ovarre(nout,'Injection power to ions (MW)','(pinji/1.d6)', &
       pinji/1.0D6)
  call ovarre(nout,'Injection power to electrons (MW)','(pinje/1.d6)', &
       pinje/1.0D6)
  call ovarre(nout,'Power to divertor (MW)','(pdivt)',pdivt)

  call osubhd(nout,'H-mode Power Threshold Scalings :')

  call ovarre(nout,'1996 ITER Scaling: nominal (MW)','(pthrmw(1))', &
       pthrmw(1))
  call ovarre(nout,'1996 ITER Scaling: upper bound (MW)','(pthrmw(2))', &
       pthrmw(2))
  call ovarre(nout,'1996 ITER Scaling: lower bound (MW)','(pthrmw(3))', &
       pthrmw(3))
  call ovarre(nout,'1997 ITER Scaling (1) (MW)','(pthrmw(4))',pthrmw(4))
  call ovarre(nout,'1997 ITER Scaling (2) (MW)','(pthrmw(5))',pthrmw(5))

  call osubhd(nout,'Confinement :')

  if (ignite == 1) then
     call ocmmnt(nout, &
          'Device is assumed to be ignited for the calculation'// &
          ' of confinement time')
     call oblnkl(nout)
  end if

  write(nout,10) tauscl(isc)
10 format(' Confinement scaling law',T45,A24)

  call ovarrf(nout,'Confinement H factor','(hfact)',hfact)
  call ovarre(nout,'Global confinement time (s)','(taueff)',taueff)
  call ovarre(nout,'Ion confinement time (s)','(tauei)',tauei)
  call ovarre(nout,'Electron confinement time (s)','(tauee)',tauee)
  call ovarre(nout,'n-tau (s/m3)','(dntau)',dntau)
  call ovarre(nout,'Heating power assumed (MW)','(powerht)',powerht)

  call osubhd(nout,'Plasma Volt-second Requirements :')
  call ovarre(nout,'Total volt-second requirement (Wb)','(vsstt)',vsstt)
  call ovarre(nout,'Inductive volt-seconds (Wb)','(vsind)',vsind)
  call ovarre(nout,'Start-up resistive (Wb)','(vsres)',vsres)
  call ovarre(nout,'Flat-top resistive (Wb)','(vsbrn)',vsbrn)
  call ovarrf(nout,'Bootstrap fraction','(bootipf)',bootipf)
  call ovarrf(nout,'Auxiliary current drive fraction','(faccd)',faccd)
  call ovarre(nout,'Plasma resistance (ohm)','(rplas)',rplas)
  call ovarre(nout,'Plasma inductance (H)','(rlp)',rlp)
  call ovarre(nout,'Sawteeth coefficient','(csawth)',csawth)
  call ovarre(nout,'Burn time (s)','(tburn)',tburn)

  call osubhd(nout,'Auxiliary Information :')
  call ovarre(nout,'Convective loss rate (A)','(qfuel)',qfuel)
  call ovarre(nout,'Burn-up fraction','(burnup)',burnup)

end subroutine outplas
