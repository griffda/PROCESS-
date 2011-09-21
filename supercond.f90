!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfspcall(nout,iprint)

  !+ad_name  tfspcall
  !+ad_summ  Routine to call the superconductor module for the TF coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
  !+ad_args  iprint : input integer : Switch to write output to file (1=yes)
  !+ad_desc  This routine calls the TF coil superconductor module.
  !+ad_prob  None
  !+ad_call  tfcoil.h90
  !+ad_call  supercon
  !+ad_hist  06/07/99 PJK Added extra arguments to SUPERCON call
  !+ad_hist  26/07/11 PJK Added JCRIT_MODEL argument to SUPERCON call
  !+ad_hist  21/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'tfcoil.h90'

  !  Arguments

  integer, intent(in) :: nout, iprint

  !  Local variables

  real(kind(1.0D0)) :: aturn, tfes, vdump
  integer :: ifail

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Simple model

  if (itfmod == 0) then
     vtfskv = 20.0D0
     return
  end if

  !  Stored energy (J) and cross-sectional area per turn

  tfes = estotf * 1.0D9
  aturn = ritfc/(jwptf*tfno*turnstf)

  ifail = 0
  call supercon(acstf,aturn,bmaxtfrp,vftf,fcutfsu,cpttf,isumattf, &
       jcrit_model,strncon,tdmptf,tfes,tftmp,tmaxpro,bcritsc,jcritsc, &
       tcritsc,iprint,nout,jwdgpro,jwdgcrt,vdump,tmargtf,ifail)

  if (ifail /= 0) then
     write(*,*) 'Error in routine TFSPCALL:'
     write(*,*) 'SUPERCON returns with value ',ifail
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  !  Dump voltage in kV

  vtfskv = vdump/1.0D3

end subroutine tfspcall

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine supercon(acs,aturn,bmax,fhe,fcu,iop,isumat,jcrit_model, &
     strain,tdump,tfes,the,tmax,bcritsc,jcritsc,tcritsc,iprint,nout, &
     jwdgpro,jwdgcrt,vd,tmarg,ifail)

  !+ad_name  supercon
  !+ad_summ  Routine to calculate the TF coil superconductor properties
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Galambos, ORNL
  !+ad_auth  J Miller, ORNL
  !+ad_cont  N/A
  !+ad_args  acs : input real : Cable space - inside area (m2)
  !+ad_args  aturn : input real : Area per turn (i.e.  entire cable) (m2)
  !+ad_args  bmax : input real : Peak field at conductor (T)
  !+ad_args  fhe : input real : Fraction of cable space that is for He cooling
  !+ad_args  fcu : input real : Fraction of conductor that is copper
  !+ad_args  iop : input real : Operating current per turn (A)
  !+ad_args  isumat : input integer : Switch for conductor type:
  !+ad_argc                           1 = binary Nb3Sn,
  !+ad_argc                           2 = ternary Nb3Sn,
  !+ad_argc                           3 = NbTi,
  !+ad_argc                           4 = generic, but uses Nb3Sn current density calc.
  !+ad_argc                           5 = generic, but uses NbTi current density calc.
  !+ad_args  jcrit_model : input integer : Switch for Jcrit model for isumat=1 only:
  !+ad_argc                                0 = original model
  !+ad_argc                                1 = ITER Nb3Sn critical surface implementation
  !+ad_args  strain : input real : Strain on superconductor at operation conditions
  !+ad_args  tdump : input real : Dump time (sec)
  !+ad_args  tfes : input real : Energy stored in one TF coil (J)
  !+ad_args  the : input real : He temperature at peak field point (K)
  !+ad_args  tmax : input real : Max conductor temperature during quench (K)
  !+ad_args  bcritsc : input real : Critical field (T) (isumat=4,5 only)
  !+ad_args  jcritsc : input real : Critical J (A/m2) (isumat=4,5 only)
  !+ad_args  tcritsc : input real : Critical temperature (K) (isumat=4,5 only)
  !+ad_args  iprint : input integer : Switch for printing (1 = yes, 0 = no)
  !+ad_args  nout : input integer : Fortran output unit identifier
  !+ad_args  jwdgpro : output real : Winding pack current density from temperature 
  !+ad_argc                          rise protection (A/m2)
  !+ad_args  jwdgcrt : output real : Critical winding pack current density (A/m2)
  !+ad_args  vd : output real : Discharge voltage imposed on a TF coil (V)
  !+ad_args  tmarg : output real : Temperature margin (K)
  !+ad_args  ifail : input/output integer : error flag (though never returns an error!)
  !+ad_desc  This routine calculates the superconductor properties for the TF coils.
  !+ad_desc  Programmed by J. Galambos 1991, from algorithms provided by J. Miller.
  !+ad_desc  <P>These are the ITER rules for Nb3Sn modelling. The routine calculates
  !+ad_desc  the critical current density (winding pack) and also the protection
  !+ad_desc  information (for a quench).
  !+ad_prob  None
  !+ad_call  osections.h90
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarre
  !+ad_call  protect
  !+ad_hist  06/07/99 PJK Added new generic superconductor options
  !+ad_hist  26/07/11 PJK Corrected denominator in JC calculation;
  !+ad_hisc               Added option to use new Jcrit model for binary Nb3Sn
  !+ad_hist  21/09/11 PJK Initial F90 version; converted to subroutine from function
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: isumat, jcrit_model, iprint, nout
  real(kind(1.0D0)), intent(in) :: acs, aturn, bmax, fhe, fcu, iop, strain, &
       tdump, tfes, the, tmax, bcritsc, jcritsc, tcritsc
  integer, intent(inout) :: ifail
  real(kind(1.0D0)), intent(out) :: jwdgpro, jwdgcrt, vd, tmarg

  !  Local variables

  real(kind(1.0D0)) :: astrain, bbar, bc2, bc20, bc20m, cstrain, c0, &
       fac1, fac2, fcond, fstrain, icrit, iooic, jc, jwdgop, tbar, tc0, &
       tc0m, tc1

  !  External functions

  real(kind(1.0D0)), external :: itersc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Conductor fraction

  fcond = 1.0D0 - fhe

  !  Find critical current density

  select case (isumat)

  case (1)  !  binary Nb3Sn data
     bc20m = 28.0D0
     tc0m = 18.0D0
     c0 = 1.315D10

  case (2)  !  ternary Nb3Sn data
     bc20m = 24.0D0
     tc0m = 16.0D0
     c0 = 2.225D10

  case (3)  !  NbTi data
     bc20m = 15.0D0
     tc0m = 9.3D0
     c0 = 10.0D9

  case default  !  generic superconductor - use input values
     bc20m = bcritsc
     tc0m = tcritsc
     c0 = jcritsc

  end select

  if (strain <  0.0D0) then
     astrain = 900.0D0
  else
     astrain = 1250.0D0
  end IF

  fstrain = 1.0D0 - astrain * abs(strain)**1.7D0
  cstrain = c0 * sqrt(fstrain)

  !  Calculate current density, taking into account critical values

  if ((isumat /= 3).and.(isumat /= 5)) then  !  Nb3Sn model

     if ((isumat == 1).and.(jcrit_model == 1)) then

        !  Use ITER Nb3Sn critical surface implementation model

        jc = itersc(the,bmax,strain)

     else
        tc0 = tc0m * fstrain**0.33333D0
        tc1 = tc0m * (1.0D0 - bmax/bc20m)
        bc20 = bc20m * fstrain
        tbar = the/tc0
        tbar = min(tbar,0.999D0)
        bc2 = bc20 * (1.0D0 - tbar**2) * &
             ( 1.0D0 - 0.31D0*tbar**2 * (1.0D0 - 1.77D0*log(tbar)) )
        bbar = bmax/bc2
        bbar = min(bbar,0.999D0)

        !+**PJK 25/07/11 Corrected SQRT(BBAR) to SQRT(BMAX) in denominator
        jc = cstrain * (1.0D0 - tbar**2)**2 * (1.0D0 - bbar)**2 &
             / (sqrt(bc2) * sqrt(bmax))
     end if

  else  !  NbTi model

     tc1 = tc0m * (1.0D0 - bmax/bc20m)**0.59D0
     tc1 = max(tc1, 0.001D0)
     tbar = max((1.0D0 - the/tc1), 0.001D0)
     jc = c0 * (1.0D0 - bmax/bc20m) * tbar

  end if

  !  Critical current

  icrit = jc * acs * (1.0D0 - fhe) * (1.0D0 - fcu)

  !  Critical current density in winding pack

  jwdgcrt = icrit / aturn

  !  Ratio of operating / critical current

  iooic = iop / icrit

  !  Operating current density

  jwdgop = iop / aturn

  !  Temperature margin

  fac1 = max( 0.01D0, (1.0D0 - iooic) )
  fac2 = max( 0.01D0, (tc1-the) )
  tmarg = fac1 * fac2

  !  Find the current density limited by the protection limit

  call protect(iop,tfes,acs,aturn,tdump,fcond,fcu,the,tmax,jwdgpro,vd)

  ifail = 0

  if ((iprint == 0).or.(sect07 == 0)) return

  call oheadr(nout,'Superconducting TF Coils')

  select case (isumat)

  case (1)
     call ocmmnt(nout,'Superconductor used: Nb3Sn (binary)')
     if (jcrit_model == 0) then
        call ocmmnt(nout,'  (original Jcrit model)')
     else
        call ocmmnt(nout,'  (ITER Jcrit model)')
     end if
  case (2)
     call ocmmnt(nout,'Superconductor used: Nb3Sn (ternary)')
  case (3)
     call ocmmnt(nout,'Superconductor used: NbTi')
  case (4)
     call ocmmnt(nout, &
          'Generic superconductor used: Nb3Sn current density model')
  case default
     call ocmmnt(nout, &
          'Generic superconductor used: NbTi current density model')

  end select

  call oblnkl(nout)
  call ovarre(nout,'Peak field at conductor (T)','(bmax)',bmax)
  call ovarre(nout,'Helium temperature at peak field (K)','(the)',the)
  call ovarre(nout,'Helium fraction inside cable space','(fhe)',fhe)
  call ovarre(nout,'Copper fraction of conductor','(fcu)',fcu)

  call osubhd(nout,'Critical Current Information :')
  call ovarre(nout,'Operating winding pack J (A/m2)','(jwdgop)',jwdgop)
  call ovarre(nout,'Critical winding pack curr. density (A/m2)', &
       '(jwdgcrt)',jwdgcrt)
  call ovarre(nout,'Critical current (A)','(icrit)',icrit)
  call ovarre(nout,'Operating current / critical current','(iooic)', &
       iooic)
  call ovarre(nout,'Temperature margin (K)','(tmarg)',tmarg)

  call osubhd(nout,'Protection Information :')
  call ovarre(nout,'Maximum temperature in quench (K)','(tmax)', &
       tmax)
  call ovarre(nout,'Winding pack protection J (A/m2)','(jwdgpro)', &
       jwdgpro)
  call ovarre(nout,'Dump time (s)','(tdump)',tdump)
  call ovarre(nout,'Dump voltage (V)','(vd)',vd)

end subroutine supercon

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function itersc(the,bmax,strain)

  !+ad_name  itersc
  !+ad_summ  Implementation of ITER Nb3Sn critical surface implementation
  !+ad_type  Function returning real
  !+ad_auth  R Kemp, CCFE, Culham Science Centre
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  the : input real : Coolant/SC temperature (K)
  !+ad_args  bmax : input real : Magnetic field at conductor (T)
  !+ad_args  strain : input real : Strain in superconductor
  !+ad_desc  This routine calculates the critical current density (A/m2) in the
  !+ad_desc  superconducting TF coils using the ITER Nb3Sn critical surface model.
  !+ad_prob  On two lines, increasing the precision of the exponent leads to
  !+ad_prob  somewhat different final answers. With RK/DW to discuss...
  !+ad_call  None
  !+ad_hist  21/07/11 RK  First draft of routine
  !+ad_hist  21/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  ITER Nb3Sn critical surface parameterization (2MMF7J) (2008),
  !+ad_docc    https://user.iter.org/?uid=2MMF7J&action=get_document
  !+ad_docs  ITER DDD 11-7: Magnets - conductors (2NBKXY) (2009),
  !+ad_docc    https://user.iter.org/?uid=2NBKXY&action=get_document
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: itersc

  !  Arguments

  real(kind(1.0D0)), intent(in) :: the, bmax, strain

  !  Local variables

  real(kind(1.0D0)), parameter :: csc = 16500.0D6
  real(kind(1.0D0)), parameter :: bctw = 32.97D0
  real(kind(1.0D0)), parameter :: tco = 16.06D0
  real(kind(1.0D0)), parameter :: cp = 0.63D0
  real(kind(1.0D0)), parameter :: cq = 2.1D0
  real(kind(1.0D0)), parameter :: caone = 44.0D0
  real(kind(1.0D0)), parameter :: catwo = 4.0D0
  real(kind(1.0D0)), parameter :: etaoa = 0.00256D0
  real(kind(1.0D0)), parameter :: etamax = -0.003253075D0
  real(kind(1.0D0)) :: tcrit, tred, bcrit, bred, etash, tzero, bcro, &
       tcro, bzero, strfun, jc1, jc2, jc3

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Strain function

  etash = (catwo*etaoa)/(sqrt(caone**2 - catwo**2))
  strfun = sqrt(etash**2 + etaoa**2) - sqrt((strain-etash)**2 + etaoa**2)
  strfun = strfun*caone - catwo*strain
  strfun = 1.0D0 + (1.0D0/(1.0D0 - caone*etaoa))*strfun

  !  cros

  bcro = (bctw*strfun)
!+PJK Increasing the precision leads to different answers!
!+PJK  tcro = tco*(strfun**0.333333D0)
  tcro = tco*(strfun**0.333333)

  !  tred and bred

  tzero = the/tcro
  bzero = bmax/bcro
  bcrit = bcro * (1.0D0 - tzero**1.52D0)
  bred = bmax/bcrit
  tcrit = tcro * (1.0D0 - bzero)**(1.0D0/1.52D0)
  tred = the/tcrit

  !  Critical current density (A/m2)

  jc1 = (csc/bmax)*strfun
!+PJK Increasing the precision leads to different answers!
!+PJK  jc2 = (1.0D0-tzero**1.52D0)*(1.0D0-tzero**2)
  jc2 = (1.0D0-tzero**1.52)*(1.0D0-tzero**2)
  jc3 = bred**cp * (1.0D0-bred)**cq

  itersc = jc1 * jc2 * jc3

end function itersc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine protect(aio,tfes,acs,aturn,tdump,fcond,fcu,tba,tmax,ajwpro,vd)

  !+ad_name  protect
  !+ad_summ  Finds the current density limited by the protection limit
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Miller, ORNL
  !+ad_cont  N/A
  !+ad_args  aio : input real : Operating current (A)
  !+ad_args  tfes : input real : Energy stored in one TF coil (J)
  !+ad_args  acs : input real : Cable space - inside area (m2)
  !+ad_args  aturn : input real : Area per turn (i.e.  entire cable) (m2)
  !+ad_args  tdump : input real : Dump time (sec)
  !+ad_args  fcond : input real : Fraction of cable space containing conductor
  !+ad_args  fcu : input real : Fraction of conductor that is copper
  !+ad_args  tba : input real : He temperature at peak field point (K)
  !+ad_args  tmax : input real : Max conductor temperature during quench (K)
  !+ad_args  ajwpro : output real :  Winding pack current density from temperature 
  !+ad_argc                          rise protection (A/m2)
  !+ad_args  vd : output real :  Discharge voltage imposed on a TF coil (V)
  !+ad_desc  This routine calculates maximum conductor current density which
  !+ad_desc  limits the peak temperature in the winding to a given limit (tmax).
  !+ad_desc  It also finds the dump voltage.
  !+ad_desc  <P>These calculations are based on Miller's formulations.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  06/07/99 PJK Initial upgraded version
  !+ad_hist  21/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: aio, tfes, acs, aturn, tdump, fcond, &
       fcu,tba,tmax
  real(kind(1.0D0)), intent(out) :: ajwpro, vd

  !  Local variables

  integer :: no,np
  real(kind(1.0D0)) :: aa,ai1,ai2,ai3,ajcp,bb,cc,dd,tav
  real(kind(1.0D0)), dimension(11) :: p1, p2, p3

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Integration coefficients p1,p2,p3

  p1(1) = 0.0D0
  p1(2) = 0.8D0
  p1(3) = 1.75D0
  p1(4) = 2.4D0
  p1(5) = 2.7D0
  p1(6) = 2.95D0
  p1(7) = 3.1D0
  p1(8) = 3.2D0
  p1(9) = 3.3D0
  p1(10) = 3.4D0
  p1(11) = 3.5D0

  p2(1) = 0.0D0
  p2(2) = 0.05D0
  p2(3) = 0.5D0
  p2(4) = 1.4D0
  p2(5) = 2.6D0
  p2(6) = 3.7D0
  p2(7) = 4.6D0
  p2(8) = 5.3D0
  p2(9) = 5.95D0
  p2(10) = 6.55D0
  p2(11) = 7.1D0

  p3(1) = 0.0D0
  p3(2) = 0.05D0
  p3(3) = 0.5D0
  p3(4) = 1.4D0
  p3(5) = 2.6D0
  p3(6) = 3.7D0
  p3(7) = 4.6D0
  p3(8) = 5.4D0
  p3(9) = 6.05D0
  p3(10) = 6.8D0
  p3(11) = 7.2D0

  !  Dump voltage

  vd = 2.0D0 * tfes/(tdump*aio)

  !  Current density limited by temperature rise during quench

  tav = 1.0D0 + (tmax-tba)/20.0D0
  no = int(tav)
  np = no+1
  np = min(np,11)

  ai1 = 1.0D16 * ( p1(no)+(p1(np)-p1(no)) * (tav - no) )
  ai2 = 1.0D16 * ( p2(no)+(p2(np)-p2(no)) * (tav - no) )
  ai3 = 1.0D16 * ( p3(no)+(p3(np)-p3(no)) * (tav - no) )

  aa = vd * aio/tfes
  bb = (1.0D0-fcond)*fcond*fcu*ai1
  cc = (fcu*fcond)**2 * ai2
  dd = (1.0D0-fcu)*fcu * fcond**2 * ai3
  ajcp = sqrt( aa* (bb+cc+dd) )
  ajwpro = ajcp*(acs/aturn)

end subroutine protect
