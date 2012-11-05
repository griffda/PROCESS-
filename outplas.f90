!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine outplas(outfile)

  !+ad_name  outplas
  !+ad_summ  Subroutine to output the plasma physics information
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : Fortran output unit identifier
  !+ad_desc  This routine writes the plasma physics information
  !+ad_desc  to a file, in a tidy format.
  !+ad_prob  None
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  rfp_variables
  !+ad_call  times_variables
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
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  31/10/12 PJK Added constraint_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraint_variables
  use current_drive_variables
  use physics_variables
  use process_output
  use rfp_variables
  use times_variables

  implicit none

  !  Arguments

  integer, intent(in) :: outfile

  !  Local variables

  real(kind(1.0D0)) :: betath

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (sect03 == 0) return

  call oheadr(outfile,'Plasma')

  select case (idivrt)
  case (0)
     call ocmmnt(outfile,'Plasma configuration = limiter')
  case (1)
     call ocmmnt(outfile,'Plasma configuration = single null divertor')
  case (2)
     call ocmmnt(outfile,'Plasma configuration = double null divertor')
  case default
     write(outfile,*) 'Error in routine OUTPLAS:'
     write(outfile,*) 'Illegal value of idivrt, = ',idivrt
     write(outfile,*) 'PROCESS stopping.'
     stop
  end select
  call oblnkl(outfile)

  if (idhe3 == 0) then
     if (iiter == 1) then
        call ocmmnt(outfile,'Deuterium - Tritium fusion reaction assumed')
        call oblnkl(outfile)
        call ocmmnt(outfile,'ITER fusion power calculations were performed')
     end if
  else
     call ocmmnt(outfile,'Deuterium - Helium3 fusion reaction assumed')
  end if

  call osubhd(outfile,'Plasma Geometry :')
  call ovarrf(outfile,'Major radius (m)','(rmajor)',rmajor)
  call ovarrf(outfile,'Minor radius (m)','(rminor)',rminor)
  call ovarrf(outfile,'Aspect ratio','(aspect)',aspect)
  call ovarrf(outfile,'Elongation, X-point','(kappa)',kappa)
  call ovarrf(outfile,'Elongation, 95% surface','(kappa95)',kappa95)
  call ovarrf(outfile,'Elongation, area ratio calc.','(kappaa)',kappaa)
  call ovarrf(outfile,'Triangularity, X-point','(triang)',triang)
  call ovarrf(outfile,'Triangularity, 95% surface','(triang95)',triang95)
  call ovarre(outfile,'Plasma surface area (m2)','(sarea)',sarea)
  call ovarre(outfile,'Plasma volume (m3)','(vol)',vol)

  call osubhd(outfile,'Current and Field :')
  call ovarrf(outfile,'Plasma current (MA)','(plascur/1D6)',plascur/1.0D6)

  if (irfp == 0) then
     call ovarrf(outfile,'Vacuum toroidal field at R (T)','(bt)',bt)
     call ovarrf(outfile,'Average poloidal field (T)','(bp)',bp)
  else
     call ovarrf(outfile,'Toroidal field at plasma edge (T)','(-bt)',-bt)
     call ovarrf(outfile,'Poloidal field at plasma edge (T)','(bp)',bp)
     call ovarrf(outfile,'Reversal parameter F','(rfpf)',rfpf)
     call ovarrf(outfile,'Pinch parameter theta','(rfpth)',rfpth)
  end if

  call ovarrf(outfile,'Total field (T)','(btot)',btot)
  call ovarrf(outfile,'Edge safety factor','(q)',q)
  call ovarrf(outfile,'Cylindrical safety factor','(qstar)',qstar)

  if (ishape == 1) then
     call ovarrf(outfile,'Lower limit for edge safety factor','(qlim)',qlim)
  end if

  if (icurr == 2) then
     call ovarrf(outfile,'Safety factor at 95% flux','(q95)',q95)
     call ocmmnt(outfile,'Mean safety factor, q-bar, used for q')
  end if

  call osubhd(outfile,'Beta Information :')
  call ovarrf(outfile,'Total plasma beta','(beta)',beta)
  call ovarrf(outfile,'Total poloidal beta','(betap)',betap)
  call ovarrf(outfile,'Total toroidal beta',' ',beta*(btot/bt)**2)
  call ovarrf(outfile,'Fast alpha beta','(betaft)',betaft)
  call ovarrf(outfile,'Beam ion beta','(betanb)',betanb)

  betath = beta-betaft-betanb
  call ovarrf(outfile,'Thermal beta',' ',betath)
  call ovarrf(outfile,'Thermal poloidal beta',' ',betath*(btot/bp)**2)
  call ovarrf(outfile,'Thermal toroidal beta (= beta-exp)',' ', &
       betath*(btot/bt)**2)

  call ovarrf(outfile,'2nd stability beta : beta_p / (R/a)', &
       '(eps*betap)',eps*betap)
  call ovarrf(outfile,'2nd stability beta upper limit','(epbetmax)', &
       epbetmax)

  call ovarrf(outfile,'Troyon g coefficient','(dnbeta)',dnbeta)
  call ovarrf(outfile,'Normalised beta',' ',fbetatry*dnbeta)

  if (itart == 1) then
     call ovarrf(outfile,'Normalised thermal toroidal beta', &
          ' ',fbetatry*dnbeta*btot**2/bt**2)
  end if

  if (iculbl == 0) then
     call ovarrf(outfile,'Limit on total beta','(betalim)',betalim)
  else if (iculbl == 1) then
     call ovarrf(outfile,'Limit on thermal beta','(betalim)',betalim)
  else
     call ovarrf(outfile,'Limit on thermal + NB beta','(betalim)', &
          betalim)
  end if

  call osubhd(outfile,'Temperature and Density (volume averaged) :')
  call ovarrf(outfile,'Electron temperature (keV)','(te)',te)
  call ovarrf(outfile,'Ion temperature (keV)','(ti)',ti)
  call ovarrf(outfile,'Electron temp., density weighted (keV)','(ten)',ten)
  call ovarre(outfile,'Electron density (/m3)','(dene)',dene)
  call ovarre(outfile,'Line-averaged electron density (/m3)','(dnla)',dnla)
  call ovarre(outfile,'Ion density (/m3)','(dnitot)',dnitot)
  call ovarre(outfile,'Fuel density (/m3)','(deni)',deni)
  call ovarre(outfile,'High Z impurity density (/m3)','(dnz)',dnz)
  call ovarre(outfile,'Cold alpha ash density (/m3)','(dnalp)',dnalp)

  if (idhe3 == 1) then
     call ovarre(outfile,'Proton ash density (/m3)','(dnprot)',dnprot)
  end if

  call ovarre(outfile,'Hot beam density (/m3)','(dnbeam)',dnbeam)
  call ovarre(outfile,'Density limit (enforced) (/m3)','(dnelimt)',dnelimt)
  call ovarre(outfile,'Seeded iron concentration','(cfe0)',cfe0)
  call ovarre(outfile,'Effective charge','(zeff)',zeff)
  call ovarre(outfile,'Mass weighted effective charge','(zeffai)',zeffai)
  call ovarrf(outfile,'Density profile factor','(alphan)',alphan)
  call ovarrf(outfile,'Temperature profile factor','(alphat)',alphat)

  if (iculdl == 1) then
     call osubhd(outfile,'Density Limit using different models :')
     call ovarre(outfile,'Old ASDEX model','(dlimit(1))',dlimit(1))
     call ovarre(outfile,'Borrass ITER model I','(dlimit(2))',dlimit(2))
     call ovarre(outfile,'Borrass ITER model II','(dlimit(3))',dlimit(3))
     call ovarre(outfile,'JET edge radiation model','(dlimit(4))',dlimit(4))
     call ovarre(outfile,'JET simplified model','(dlimit(5))',dlimit(5))
     call ovarre(outfile,'Hugill-Murakami Mq model','(dlimit(6))',dlimit(6))
     call ovarre(outfile,'Greenwald model','(dlimit(7))',dlimit(7))
  end if

  call osubhd(outfile,'Fuel Constituents :')
  if (idhe3 == 0) then
     call ovarre(outfile,'Deuterium fuel fraction','(1-ftr)',1.0D0-ftr)
     call ovarre(outfile,'Tritium fuel fraction','(ftr)',ftr)
  else
     call ovarre(outfile,'Deuterium fuel fraction','(fdeut)',fdeut)
     call ovarre(outfile,'Tritium fuel fraction','(ftrit)',ftrit)
     call ovarre(outfile,'3-Helium fuel fraction','(fhe3)',fhe3)
  end if

  call osubhd(outfile,'Fusion Power :')
  call ovarre(outfile,'Fusion power (MW)','(powfmw)',powfmw)
  call ovarre(outfile,'Alpha power: total (MW)','(alpmw)',alpmw)
  call ovarre(outfile,'Alpha power: beam-plasma (MW)','(palpnb)',palpnb)

  if (idhe3 == 1) then
     call ovarre(outfile,'Neutron power (MW)','(pneut*vol)',pneut*vol)
     call ovarre(outfile,'Proton power (MW)','(pcharge*vol)',pcharge*vol)
  end if

  call ovarre(outfile,'Neutron wall load (MW/m2)','(wallmw)',wallmw)
  call ovarrf(outfile,'Fraction of power to electrons','(falpe)',falpe)
  call ovarrf(outfile,'Fraction of power to ions','(falpi)',falpi)

  call osubhd(outfile,'Plasma Power Balance :')
  call ovarre(outfile,'Ohmic heating power (MW)','(pohmpv*vol)',pohmpv*vol)
  call ovarre(outfile,'Bremsstrahlung radiation power (MW)','(pbrem*vol)', &
       pbrem*vol)
  call ovarre(outfile,'Synchrotron radiation power (MW)','(psync*vol)', &
       psync*vol)
  call ovarre(outfile,'Synchrotron reflection factor','(ssync)',ssync)
  call ovarre(outfile,'Scrape-off line radiation power (MW)','(plrad*vol)', &
       plrad*vol)
  call ovarre(outfile,'Ion transport (MW)','(ptri*vol))',ptri*vol)
  call ovarre(outfile,'Electron transport (MW)','(ptre*vol)',ptre*vol)
  call ovarre(outfile,'Injection power to ions (MW)','(pinji/1.d6)', &
       pinji/1.0D6)
  call ovarre(outfile,'Injection power to electrons (MW)','(pinje/1.d6)', &
       pinje/1.0D6)
  call ovarre(outfile,'Power to divertor (MW)','(pdivt)',pdivt)

  call osubhd(outfile,'H-mode Power Threshold Scalings :')

  call ovarre(outfile,'1996 ITER Scaling: nominal (MW)','(pthrmw(1))', &
       pthrmw(1))
  call ovarre(outfile,'1996 ITER Scaling: upper bound (MW)','(pthrmw(2))', &
       pthrmw(2))
  call ovarre(outfile,'1996 ITER Scaling: lower bound (MW)','(pthrmw(3))', &
       pthrmw(3))
  call ovarre(outfile,'1997 ITER Scaling (1) (MW)','(pthrmw(4))',pthrmw(4))
  call ovarre(outfile,'1997 ITER Scaling (2) (MW)','(pthrmw(5))',pthrmw(5))

  call osubhd(outfile,'Confinement :')

  if (ignite == 1) then
     call ocmmnt(outfile, &
          'Device is assumed to be ignited for the calculation'// &
          ' of confinement time')
     call oblnkl(outfile)
  end if

  write(outfile,10) tauscl(isc)
10 format(' Confinement scaling law',T45,A24)

  call ovarrf(outfile,'Confinement H factor','(hfact)',hfact)
  call ovarre(outfile,'Global confinement time (s)','(taueff)',taueff)
  call ovarre(outfile,'Ion confinement time (s)','(tauei)',tauei)
  call ovarre(outfile,'Electron confinement time (s)','(tauee)',tauee)
  call ovarre(outfile,'n-tau (s/m3)','(dntau)',dntau)
  call ovarre(outfile,'Heating power assumed (MW)','(powerht)',powerht)

  call osubhd(outfile,'Plasma Volt-second Requirements :')
  call ovarre(outfile,'Total volt-second requirement (Wb)','(vsstt)',vsstt)
  call ovarre(outfile,'Inductive volt-seconds (Wb)','(vsind)',vsind)
  call ovarre(outfile,'Start-up resistive (Wb)','(vsres)',vsres)
  call ovarre(outfile,'Flat-top resistive (Wb)','(vsbrn)',vsbrn)
  call ovarrf(outfile,'Bootstrap fraction','(bootipf)',bootipf)
  call ovarrf(outfile,'Auxiliary current drive fraction','(faccd)',faccd)
  call ovarre(outfile,'Plasma resistance (ohm)','(rplas)',rplas)
  call ovarre(outfile,'Plasma inductance (H)','(rlp)',rlp)
  call ovarre(outfile,'Sawteeth coefficient','(csawth)',csawth)
  call ovarre(outfile,'Burn time (s)','(tburn)',tburn)

  call osubhd(outfile,'Auxiliary Information :')
  call ovarre(outfile,'Convective loss rate (A)','(qfuel)',qfuel)
  call ovarre(outfile,'Burn-up fraction','(burnup)',burnup)

end subroutine outplas

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine outtim(outfile)

  !+ad_name  outtim
  !+ad_summ  Routine to print out the times of the various stages
  !+ad_summ  during a single plant cycle
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : Fortran output unit identifier
  !+ad_desc  This routine writes out the times of the various stages
  !+ad_desc  during a single plant cycle.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  times_variables
  !+ad_call  oblnkl
  !+ad_call  oheadr
  !+ad_call  ovarrf
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use times_variables

  implicit none

  !  Arguments

  integer, intent(in) :: outfile

  !  Local variables

  real(kind(1.0D0)) :: tcycle

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (sect21 == 0) return

  tcycle = tramp + tohs + theat + tburn + tqnch + tdwell

  call oheadr(outfile,'Times')

  call ovarrf(outfile,'Initial charge time for PF coils (s)','(tramp)', &
       tramp)
  call ovarrf(outfile,'OH coil swing time (s)','(tohs)',tohs)
  call ovarrf(outfile,'Heating time (s)','(theat)',theat)
  call ovarre(outfile,'Burn time (s)','(tburn)',tburn)
  call ovarrf(outfile,'Shutdown time for PF coils (s)','(tqnch)',tqnch)
  call ovarrf(outfile,'Time between pulses (s)','(tdwell)',tdwell)
  call oblnkl(outfile)
  call ovarre(outfile,'Pulse time (s)','(tpulse)',tpulse)
  call ovarrf(outfile,'Down time (s)','(tdown)',tdown)
  call ovarre(outfile,'Total plant cycle time (s)','(tcycle)',tcycle)

end subroutine outtim
