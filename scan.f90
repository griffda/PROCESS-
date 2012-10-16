!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module scan_module

  !+ad_name  scan_module
  !+ad_summ  Module containing routines to perform a parameter scan
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  scan
  !+ad_args  None
  !+ad_desc  This module contains routines to perform a parameter scan
  !+ad_desc  over a range of values of a particular scanning variable.
  !+ad_prob  None
  !+ad_call  current_drive_variables
  !+ad_call  global_variables
  !+ad_call  numerics
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_hist  09/10/12 PJK Initial version of module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added global_variables module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use current_drive_variables
  use global_variables
  use numerics
  use physics_variables
  use process_output

  implicit none

  public

  integer, parameter :: ipnscns = 50  !  maximum number of scan points
  integer, parameter :: ipnscnv = 26  !  number of available scan variables

  integer :: isweep = 0  !  number of loops to perform
  integer :: nsweep = 1  !  switch denoting quantity to scan
  !                          = 1  aspect
  !                          = 2  hldivlim
  !                          = 3  pnetelin
  !                          = 4  hfact
  !                          = 5  oacdcp
  !                          = 6  walalw
  !                          = 7  beamfus0
  !                          = 8  fqval
  !                          = 9  te
  !                          = 10 boundu(15: fvs)
  !                          = 11 dnbeta
  !                          = 12 bscfmax (use negative values only)
  !                          = 13 boundu(10: hfact)
  !                          = 14 fiooic
  !                          = 15 fjprot
  !                          = 16 rmajor
  !                          = 17 bmxlim
  !                          = 18 gammax
  !                          = 19 boundl(16: ohcth)
  !                          = 20 tbrnmn
  !                          = 21 sigpfalw
  !                          = 22 cfactr
  !                          = 23 boundu(72: fipir)
  !                          = 24 powfmax
  !                          = 25 kappa
  !                          = 26 triang

  !  Actual values to use in scan
  real(kind(1.0D0)), dimension(ipnscns) :: sweep = 0.0D0

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine scan

  !+ad_name  scan
  !+ad_summ  Routine to call the optimisation routine VMCON over
  !+ad_summ  a range of values of one of the variables
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calls the optimisation routine VMCON
  !+ad_desc  a number of times, by performing a sweep over a range of
  !+ad_desc  values of a particular variable.
  !+ad_prob  None
  !+ad_call  pwrcom.h90
  !+ad_call  tfcoil.h90
  !+ad_call  pfcoil.h90
  !+ad_call  ineq.h90
  !+ad_call  cost.h90
  !+ad_call  htpwr.h90
  !+ad_call  divrt.h90
  !+ad_call  doopt
  !+ad_call  final
  !+ad_call  oblnkl
  !+ad_call  ostars
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  01/04/98 PJK Added POWFMAX to list of scanning variables
  !+ad_hist  23/06/98 PJK Added KAPPA and TRIANG to list of scanning vars
  !+ad_hist  19/05/99 PJK Added warning about trying to scan CFACTR with
  !+ad_hisc               new availability model
  !+ad_hist  25/05/06 PJK Added implied-DO loops for sweep outputs
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'pwrcom.h90'
  include 'tfcoil.h90'
  include 'pfcoil.h90'
  include 'ineq.h90'
  include 'cost.h90'
  include 'htpwr.h90'
  include 'divrt.h90'

  !  Arguments

  !  Local variables

  character(len=25) :: xlabel,tlabel

  real(kind(1.0D0)), dimension(ipnscns) :: asp,bet,betl,blim,bq,bs, &
       btor,btt,ccst,cdt,ce,cec,cef,ceo,crc,curd,d20,eb,epbp,fcl,hfio, &
       hfip,hld,ifa,ip,lq,ocd,pbf,pcp,pfp,pg,pht,pinj,piwp,pmp,pn,powf, &
       qlm,rcl,rec1,rmaj,sq,str,t10,tfp,tmx,vcl,wall,wpf,wtf

  integer :: ifail,i

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  tlabel = icase

  if (isweep == 0) then
     call doopt(ifail)
     call final(ifail)
     return
  end if

  if (isweep > ipnscns) then
     write(*,*) 'Error in routine SCAN:'
     write(*,*) 'Illegal value of isweep, = ',isweep
     write(*,*) 'Maximum = ',ipnscns
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  do i = 1,isweep

     call oblnkl(nout)
     call ostars(nout,72)
     write(nout,10) i,isweep
10   format(' ***** Scan point ',i2,' of ',i2,' *****')
     call ostars(nout,72)

     select case (nsweep)

     case (1)
        aspect = sweep(i)
        xlabel = 'Aspect Ratio'
     case (2)
        hldivlim = sweep(i)
        xlabel = 'Divertor Ht Limit MW/m2'
     case (3)
        pnetelin = sweep(i)
        xlabel = 'Net Elec pwr MW'
     case (4)
        hfact = sweep(i)
        xlabel = 'Confinement hfact'
     case (5)
        oacdcp = sweep(i)
        xlabel = 'j TF inner leg MA/m2'
     case (6)
        walalw = sweep(i)
        xlabel = 'All. wall load MW/m2'
     case (7)
        beamfus0 = sweep(i)
        xlabel = 'Beam back. multiplier'
     case (8)
        fqval = sweep(i)
        xlabel = '1/Big Q'
     case (9)
        te = sweep(i)
        xlabel = 'Temperature'
     case (10)
        boundu(15) = sweep(i)
        xlabel = 'Volt-sec upper bound'
     case (11)
        dnbeta = sweep(i)
        xlabel = 'Troyon coefficient'
     case (12)
        bscfmax = sweep(i)
        xlabel = 'Bootstrap Fraction'
     case (13)
        boundu(10) = sweep(i)
        xlabel = 'H factor upper bound'
     case (14)
        fiooic = sweep(i)
        xlabel = 'Iop / Icrit f-value'
     case (15)
        fjprot = sweep(i)
        xlabel = 'TFC J limit f-value'
     case (16)
        rmajor = sweep(i)
        xlabel = 'Plasma major radius'
     case (17)
        bmxlim = sweep(i)
        xlabel = 'Max toroidal field'
     case (18)
        gammax = sweep(i)
        xlabel = 'Maximum CD gamma'
     case (19)
        boundl(16) = sweep(i)
        xlabel = 'OHC thickness lower bound'
     case (20)
        tbrnmn = sweep(i)
        xlabel = 'Minimum burn time'
     case (21)
        sigpfalw = sweep(i)
        xlabel = 'Allowable PF coil stress'
     case (22)
        if (iavail == 1) then
           write(*,*) 'Error in routine SCAN:'
           write(*,*) 'Do not scan CFACTR if IAVAIL=1'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cfactr = sweep(i)
        xlabel = 'Plant availability factor'
     case (23)
        boundu(72) = sweep(i)
        xlabel = 'Ip/Irod upper bound'
     case (24)
        powfmax = sweep(i)
        xlabel = 'Fusion Power limit'
     case (25)
        kappa = sweep(i)
        xlabel = 'Elongation'
     case (26)
        triang = sweep(i)
        xlabel = 'Triangularity'

     case default
        write(*,*) 'Error in routine SCAN:'
        write(*,*) 'Illegal scan variable number, nsweep = ',nsweep
        write(*,*) 'PROCESS stopping.'
        stop

     end select

     call doopt(ifail)
     call final(ifail)

     !  Store values for output

     ifa(i)  = real(ifail)
     sq(i)   = sqsumsq
     ce(i)   = coe
     cec(i)  = coecap
     cef(i)  = coefuelt
     ceo(i)  = coeoam
     ccst(i) = capcost
     crc(i)  = c221 + c222
     cdt(i)  = cdirt / 1.0D3
     rmaj(i) = rmajor
     asp(i)  = aspect
     ip(i)   = 1.0D-6 * plascur
     btor(i) = bt
     btt(i)  = btot
     lq(i)   = q
     qlm(i)  = qlim
     bet(i)  = beta
     blim(i) = betalim
     epbp(i) = betap / aspect
     t10(i)  = te/10.0D0 * pcoef
     d20(i)  = dene/1.0D20
     hfip(i) = hfac(6)
     hfio(i) = hfac(7)
     powf(i) = powfmw
     pbf(i)  = palpnb * 5.0D0
     wall(i) = wallmw
     pinj(i) = 1.0D-6 * (pinji + pinje)
     piwp(i) = pinjwp
     pht(i)  = pheat * 1.0D-6
     curd(i) = 1.0D-6 * (pinji + pinje - pheat)
     bq(i)   = bigq
     bs(i)   = bootipf
     eb(i)   = enbeam/1.0D3
     hld(i)  = hldiv
     tfp(i)  = tfcmw
     wtf(i)  = whttf
     str(i)  = sigrad + sigtan
     ocd(i)  = oacdcp/1.0D6
     tmx(i)  = tcpmax
     pcp(i)  = tfcpmw
     fcl(i)  = fcoolcp
     rcl(i)  = rcool
     vcl(i)  = vcool
     pmp(i)  = ppump/1.0D6
     pfp(i)  = 1.0D-3 * srcktpm
     wpf(i)  = whtpf
     pg(i)   = pgrossmw
     pn(i)   = pnetelmw
     if (ireactor == 1) then
        rec1(i) = (pgrossmw-pnetelmw) / pgrossmw
     else
        rec1(i) = 0.0D0
     end if
  end do

  write(nplot,900) isweep
  write(nplot,901) tlabel

  !+**PJK 20/10/92 Why does this section supersede one above?

  if (nsweep == 8) then
     xlabel = 'Big Q'
     do i = 1,isweep
        sweep(i) = 1.0D0/sweep(i) 
     end do
  end if

  write(nplot,902) xlabel,(sweep(i),i=1,isweep)
  write(nplot,903) (ifa(i),i=1,isweep)
  write(nplot,904) (sq(i),i=1,isweep)
  write(nplot,905) (ce(i),i=1,isweep)
  write(nplot,906) (cec(i),i=1,isweep)
  write(nplot,907) (cef(i),i=1,isweep)
  write(nplot,908) (ceo(i),i=1,isweep)
  write(nplot,909) (ccst(i),i=1,isweep)
  write(nplot,910) (crc(i),i=1,isweep)
  write(nplot,911) (cdt(i),i=1,isweep)
  write(nplot,912) (rmaj(i),i=1,isweep)
  write(nplot,913) (asp(i),i=1,isweep)
  write(nplot,914) (ip(i),i=1,isweep)
  write(nplot,915) (btor(i),i=1,isweep)
  write(nplot,916) (btt(i),i=1,isweep)
  write(nplot,917) (lq(i),i=1,isweep)
  write(nplot,918) (qlm(i),i=1,isweep)
  write(nplot,919) (bet(i),i=1,isweep)
  write(nplot,920) (blim(i),i=1,isweep)
  write(nplot,921) (epbp(i),i=1,isweep)
  write(nplot,922) (t10(i),i=1,isweep)
  write(nplot,923) (d20(i),i=1,isweep)
  write(nplot,924) (hfip(i),i=1,isweep)
  write(nplot,925) (hfio(i),i=1,isweep)
  write(nplot,926) (powf(i),i=1,isweep)
  write(nplot,927) (pbf(i),i=1,isweep)
  write(nplot,928) (wall(i),i=1,isweep)
  write(nplot,929) (pinj(i),i=1,isweep)
  write(nplot,930) (piwp(i),i=1,isweep)
  write(nplot,931) (pht(i),i=1,isweep)
  write(nplot,932) (curd(i),i=1,isweep)
  write(nplot,933) (bq(i),i=1,isweep)
  write(nplot,934) (bs(i),i=1,isweep)
  write(nplot,935) (eb(i),i=1,isweep)
  write(nplot,936) (hld(i),i=1,isweep)
  write(nplot,937) (tfp(i),i=1,isweep)
  write(nplot,938) (wtf(i),i=1,isweep)
  write(nplot,939) (str(i),i=1,isweep)
  write(nplot,940) (ocd(i),i=1,isweep)
  write(nplot,941) (tmx(i),i=1,isweep)
  write(nplot,942) (pcp(i),i=1,isweep)
  write(nplot,943) (fcl(i),i=1,isweep)
  write(nplot,944) (rcl(i),i=1,isweep)
  write(nplot,945) (vcl(i),i=1,isweep)
  write(nplot,946) (pmp(i),i=1,isweep)
  write(nplot,947) (pfp(i),i=1,isweep)
  write(nplot,948) (wpf(i),i=1,isweep)
  write(nplot,949) (pg(i),i=1,isweep)
  write(nplot,950) (pn(i),i=1,isweep)
  write(nplot,951) (rec1(i),i=1,isweep)

900 format(i8)
901 format(a25)
902 format(a25, 20e10.4)
903 format('Ifail                    ', 20e10.4)
904 format('Sqsumsq                  ', 20e10.4)
905 format('Electric cost (mil/kwh)  ', 20e10.4)
906 format('Capital cost (mil/kwh)   ', 20e10.4)
907 format('Fuel cost (mil/kwh)      ', 20e10.4)
908 format('Operations cost (mil/kwh)', 20e10.4)
909 format('Capital cost (mil)       ', 20e10.4)
910 format('Core costs (millions)    ', 20e10.4)
911 format('Direct cost (billions)   ', 20e10.4)
912 format('Major Radius (m)         ', 20e10.4)
913 format('Aspect Ratio             ', 20e10.4)
914 format('Plasma Current (MA)      ', 20e10.4)
915 format('B Toroidal Axis (T)      ', 20e10.4)
916 format('B total on axis (T)      ', 20e10.4)
917 format('Safety Factor            ', 20e10.4)
918 format('Should be zero...........', 20e10.4)
919 format('Beta                     ', 20e10.4)
920 format('Beta Limit               ', 20e10.4)
921 format('Epsilon Beta Poloidal    ', 20e10.4)
922 format('Average Temp x10 (KeV)   ', 20e10.4)
923 format('Average Dens (10^20/m^3) ', 20e10.4)
924 format('H-fact Iter Power        ', 20e10.4)
925 format('H-fact Iter Offset       ', 20e10.4)
926 format('Fusion Power (MW)        ', 20e10.4)
927 format('nb Fusion Power (MW)     ', 20e10.4)
928 format('Wall Load (MW/m^2)       ', 20e10.4)
929 format('Injection Power (MW)     ', 20e10.4)
930 format('Inject Pwr Wall Plug (MW)', 20e10.4)
931 format('Heating Power (MW)       ', 20e10.4)
932 format('Current Drive (MW)       ', 20e10.4)
933 format('Big Q                    ', 20e10.4)
934 format('Bootstrap Fraction       ', 20e10.4)
935 format('Neutral Beam Energy (MeV)', 20e10.4)
936 format('Divertor Heat (MW/m^2)   ', 20e10.4)
937 format('TF coil Power (MW)       ', 20e10.4)
938 format('TF coil weight (kg)      ', 20e10.4)
939 format('TF stress (MPa)          ', 20e10.4)
940 format('J   TF inner leg (MA/m^2)', 20e10.4)
941 format('Should be zero...........', 20e10.4)
942 format('Res TF inner leg Pwr (MW)', 20e10.4)
943 format('Coolant Fraction Ctr.    ', 20e10.4)
944 format('Should be zero...........', 20e10.4)
945 format('Should be zero...........', 20e10.4)
946 format('Should be zero...........', 20e10.4)
947 format('PF coil Power (MW)       ', 20e10.4)
948 format('PF coil weight (kg)      ', 20e10.4)
949 format('Gross Elect Pwr (MW)     ', 20e10.4)
950 format('Net electric Pwr (MW)    ', 20e10.4)
951 format('Recirculating Fraction   ', 20e10.4)

end subroutine scan

end module scan_module
