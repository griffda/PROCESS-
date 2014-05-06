!  $Id:: fispact.f90 258 2014-04-24 12:28:55Z pknight                   $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Module         : $Id: fispact.f90 258 2014-04-24 12:28:55Z pknight $
!  Module name    : $RCSfile: fispact.f,v $
!  Version no.    : $Revision: 1.9 $
!  Creation date  : $Date: 1997/11/19 09:29:42 $
!  Creation time  : 
!  SCCS file      :
!  %P%
!----------------------------------------------------------------------
      SUBROUTINE FISPAC(IPRINT)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.220
!
!--Description
!  Routine that links the FISPRO and BBIE modules to PROCESS.
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  18 November 1997
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  25/02/94 PJK 1.100 Corrected error in WFLUX for inboard blanket
!                     (m2 to cm2 conversion factor had been left out).
!                     Added coding for results after 100 yr decay time
!  05/02/97 PJK 1.200 Added coding for Martensitic steel
!  19/02/97 PJK 1.210 Made output values global (fispact.h). Modified
!                     inboard/outboard blanket mass calculations.
!                     Modified neutron fluxes, and fixed error in
!                     WELEMP calculation (fraction --> percentage).
!                     Added coding for results for several decay periods
!  18/11/97 PJK 1.220 Added COMMON block OPSYS. Removed NOUT argument
!  10/10/12 PJK Hardwired IVMS to zero
!  24/04/14 PJK Ensured calculation proceeds whether or not iprint==1
!
!--Arguments
!  IPRINT : (INPUT)  Flag to turn on/off (1/0) output to file
!
!--Global variables passed via COMMON
!  See FISPRO routine for details of all
!  COMMON blocks local to this module.
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        use build_variables
        use fispact_variables
        use fwbs_variables
        use physics_variables
        use pulse_variables

!  Arguments
      INTEGER IPRINT

!  Global variables
      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER WLVL2,WJSPEK,WMAXT
      REAL WLVL1,WCONV
      COMMON /PRPRM0/ WLVL1,WLVL2,WJSPEK,WCONV,WMAXT

      DOUBLE PRECISION WMIND
      COMMON /PRPRM1/ WMIND

      CHARACTER*22 WSPID
      COMMON /PROSTR/ WSPID

      INTEGER IVMS1
      COMMON /OPSYS/IVMS1

!  Local variables

      integer, parameter :: ivms = 0

      REAL nneut
      DOUBLE PRECISION breedr,dencol,fbbie(100),x,dklen
      DOUBLE PRECISION ecss(83),ecbe(83),ecvd(83),ecli2o(83), &
           eclipb(83),ecli(83),eche(83),ech2o(83),ecms(83)
      INTEGER iwtype,j

!  External routines
      EXTERNAL BBIE,ELCOMP,FISPRO,OCMMNT,OHEADR,OSUBHD,OVARRE

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      return  !  FISPACT module not to be used... PJK 02/10/2012

!+**PJK 17/11/97
      IVMS1 = IVMS

! *** Calculate elemental compositions of blanket/first wall materials

      call elcomp(ecss,ecms,ecbe,ecvd,ecli2o,eclipb,ecli,eche,ech2o)

! *** Outboard blanket thickness in centimetres, with restrictions
! *** to avoid extrapolation within BBIE

      x = blnkoth*100.0D0

      x = MAX(x,51.5D0)
      x = MIN(x,106.5D0)

! *** Calculate tritium breeding ratio

      call bbie(200,x,fbbie)
      breedr = fbbie(1)

! *** Parameters that are the same for each region

      wconv = 0.002
      wjspek = 1
      wlvl1 = 50.0
      wlvl2 = 1
      wmaxt = 10
      wmind = 1.0d5

! *** Number of neutrons leaving the plasma per second, and not lost
! *** to holes or the centrepost. Each has energy 14.06 MeV

      nneut = REAL( &
           (pneut*vol-pnucloss-pnuccp)*1.0D6/(14.06D6*1.6022D-19) )

! *** Inboard blanket ***********************

      wspid = 'Inboard blanket      '

! *** Integrated zone power

      call bbie(201,x,fbbie)
      bliizp = fbbie(1)

! *** Mean zone power density

      call bbie(205,x,fbbie)
      blimzp = fbbie(1)

! *** Mean zone neutron flux spectrum

      call bbie(210,x,fbbie)
      do 10 j = 1,100
         wspect(j) = REAL(fbbie(j))
 10   continue

! *** Elemental composition (same for both inboard and
! *** outboard blankets)

      if (lblnkt.ne.1) then
!        *** Old blanket model
         if (costr.eq.1) then
            do 20 j = 1,83
               welemp(j) = 100.0 * REAL( &
                    fblss*(1.0D0-fmsbl)*ecss(j) + &
                    fblss*fmsbl*ecms(j) + &
                    fblvd*ecvd(j) + &
                    fblli2o*ecli2o(j) + &
                    fblbe*ecbe(j) + &
                    vfblkt*eche(j) )
 20         continue
         else
            do 30 j = 1,83
               welemp(j) = 100.0 * REAL( &
                    fblss*(1.0D0-fmsbl)*ecss(j) + &
                    fblss*fmsbl*ecms(j) + &
                    fblvd*ecvd(j) + &
                    fblli2o*ecli2o(j) + &
                    fblbe*ecbe(j) + &
                    vfblkt*ech2o(j) )
 30         continue
         end if
      else
!        *** New blanket model
         if (smstr.eq.1) then
!           *** Li2O/Be solid blanket
            if (costr.eq.1) then
               do 40 j = 1,83
                  welemp(j) = 100.0 * REAL( &
                       fblss*(1.0D0-fmsbl)*ecss(j) + &
                       fblss*fmsbl*ecms(j) + &
                       fblvd*ecvd(j) + &
                       fblli2o*ecli2o(j) + &
                       fblbe*ecbe(j) + &
                       vfblkt*eche(j) )
 40            continue
            else
               do 50 j = 1,83
                  welemp(j) = 100.0 * REAL( &
                       fblss*(1.0D0-fmsbl)*ecss(j) + &
                       fblss*fmsbl*ecms(j) + &
                       fblvd*ecvd(j) + &
                       fblli2o*ecli2o(j) + &
                       fblbe*ecbe(j) + &
                       vfblkt*ech2o(j) )
 50            continue
            end if
         else
!           *** LiPb/Li liquid blanket
            if (costr.eq.1) then
               do 60 j = 1,83
                  welemp(j) = 100.0 * REAL( &
                       fblss*(1.0D0-fmsbl)*ecss(j) + &
                       fblss*fmsbl*ecms(j) + &
                       fblvd*ecvd(j) + &
                       fbllipb*eclipb(j) + &
                       fblli*ecli(j) + &
                       vfblkt*eche(j) )
 60            continue
            else
               do 70 j = 1,83
                  welemp(j) = 100.0 * REAL( &
                       fblss*(1.0D0-fmsbl)*ecss(j) + &
                       fblss*fmsbl*ecms(j) + &
                       fblvd*ecvd(j) + &
                       fbllipb*eclipb(j) + &
                       fblli*ecli(j) + &
                       vfblkt*ech2o(j) )
 70            continue
            end if
         end if
      end if

! *** Irradiation time

      wtime = REAL(bktlife) * 3.15576E7

! *** Coolant density (costr=1 : gaseous helium, costr=2 : steam/water)

      if (costr.eq.1) then
         dencol = 1.517D0
      else
         dencol = 806.719D0
      end if

! *** Total mass (including coolant)

      wmass = REAL(whtblkt + dencol*vfblkt*volblkt)
      wmass = MAX(1.0E-16, wmass * REAL(volblkti/volblkt))

! *** Neutron flux (neutrons per cm**2 per second)
!+**PJK 19/02/97 Include energy multiplication, and an e-folding
!+**PJK 19/02/97 factor for attenuation through half the blanket

      if (lblnkt.eq.1) then
         if (smstr.eq.1) then
            dklen = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
         else
            dklen = 0.075D0 / (1.0D0 - vfblkt - fbllipb - fblli)
         end if
      else
         dklen = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
      end if

      wflux = nneut * REAL( emult*exp(-0.5D0*blnkoth/dklen) / &
           (1.0D4*(volblkti/(blnkith+1.0D-6)+volblkto/(blnkoth+1.0D-6))) &
           )

! *** Calculate inventory and dose rates, etc.

      do 80 iwtype = 1,4
         if (iwtype.ne.2) then
            wregn = 1
            wtype = iwtype
            call fispro
         end if
 80   continue

! *** No of atoms in each nuclide is stored in WINVT(359)

! *** Total activity (Bq)

      bliact(1) = dble(wtota)

! *** Total gamma dose rate (Sv/hr)

      bligdr(1) = dble(wtotd)

! *** Total heat output (kW)

      blihkw(1) = dble(wtoth)

! *** Cooling times (no neutron flux incident)

      wflux = 0.0E0
      wtype = 5

! *** 13 weeks (three months)

      wtime = 7.8624E6
      call fispro
      bliact(2) = dble(wtota)
      bligdr(2) = dble(wtotd)
      blihkw(2) = dble(wtoth)

! *** 100 years

      wtime = 3.1536E9
      call fispro
      bliact(3) = dble(wtota)
      bligdr(3) = dble(wtotd)
      blihkw(3) = dble(wtoth)

! *** Outboard blanket ***********************

      wspid = 'Outboard blanket      '

! *** Integrated zone power

      call bbie(204,x,fbbie)
      bloizp = fbbie(1)

! *** Mean zone power density

      call bbie(208,x,fbbie)
      blomzp = fbbie(1)

! *** Mean zone neutron flux spectrum

      call bbie(213,x,fbbie)
      do 90 j = 1,100
         wspect(j) = REAL(fbbie(j))
 90   continue

! *** Elemental composition is the same as
! *** for the inboard blanket above.

! *** Irradiation time

      wtime = REAL(bktlife) * 3.15576E7

! *** Total mass (including coolant)

      wmass = REAL(whtblkt + dencol*vfblkt*volblkt)
      wmass = MAX(1.0E-16, wmass * REAL(volblkto/volblkt))

! *** Neutron flux (neutrons per cm**2 per second)
!+**PJK 19/02/97 Include energy multiplication, and an e-folding
!+**PJK 19/02/97 factor for attenuation through half the blanket

      wflux = nneut * REAL( emult*exp(-0.5D0*blnkith/dklen) / &
           (1.0D4*(volblkti/(blnkith+1.0D-6)+volblkto/(blnkoth+1.0D-6))) &
           )

! *** Calculate inventory and dose rates, etc.

      do 100 iwtype = 1,4
         if (iwtype.ne.2) then
            wregn = 2
            wtype = iwtype
            call fispro
         end if
 100  continue

! *** No of atoms in each nuclide is stored in WINVT(359)

! *** Total activity (Bq)

      bloact(1) = dble(wtota)

! *** Total gamma dose rate (Sv/hr)

      blogdr(1) = dble(wtotd)

! *** Total heat output (kW)

      blohkw(1) = dble(wtoth)

! *** Cooling times (no neutron flux incident)

      wflux = 0.0E0
      wtype = 5

! *** 13 weeks (three months)

      wtime = 7.8624E6
      call fispro
      bloact(2) = dble(wtota)
      blogdr(2) = dble(wtotd)
      blohkw(2) = dble(wtoth)

! *** 100 years

      wtime = 3.1536E9
      call fispro
      bloact(3) = dble(wtota)
      blogdr(3) = dble(wtotd)
      blohkw(3) = dble(wtoth)

! *** Inboard first wall ***********************

      wspid = 'Inboard first wall    '

! *** Integrated zone power

      call bbie(202,x,fbbie)
      fwiizp = fbbie(1)

! *** Mean zone power density

      call bbie(206,x,fbbie)
      fwimzp = fbbie(1)

! *** Mean zone neutron flux spectrum

      call bbie(211,x,fbbie)
      do 110 j = 1,100
         wspect(j) = REAL(fbbie(j))
 110  continue

! *** Elemental composition

      do 120 j = 1,83
         if (costr.eq.1) then
            welemp(j) = 100.0 * REAL( &
                 (1.0D0-fwclfr) * ((1.0D0-fmsfw)*ecss(j)+fmsfw*ecms(j)) &
                 + fwclfr*eche(j) )
         else
            welemp(j) = 100.0 * REAL( &
                 (1.0D0-fwclfr) * ((1.0D0-fmsfw)*ecss(j)+fmsfw*ecms(j)) &
                 + fwclfr*ech2o(j) )
         end if
 120  continue

! *** Irradiation time

      wtime = REAL(fwlife) * 3.15576E7

! *** Total mass (including coolant) - scale to obtain inboard mass
! *** using the plasma surface area components

      wmass = REAL( &
           fwmass + dencol * fwclfr * fwarea*(fwith+fwoth)/2.0D0 )
      wmass = MAX(1.0E-16, wmass * REAL((sarea-sareao)/sarea))

! *** Neutron flux (neutrons per cm**2 per second)

      wflux = nneut / REAL(1.0D4*fwarea)

! *** Calculate inventory and dose rates, etc.

      do 130 iwtype = 1,4
         if (iwtype.ne.2) then
            wregn = 3
            wtype = iwtype
            call fispro
         end if
 130  continue

! *** No of atoms in each nuclide is stored in WINVT(359)

! *** Total activity (Bq)

      fwiact(1) = dble(wtota)

! *** Total gamma dose rate (Sv/hr)

      fwigdr(1) = dble(wtotd)

! *** Total heat output (kW)

      fwihkw(1) = dble(wtoth)

! *** Cooling times (no neutron flux incident)

      wflux = 0.0E0
      wtype = 5

! *** 13 weeks (three months)

      wtime = 7.8624E6
      call fispro
      fwiact(2) = dble(wtota)
      fwigdr(2) = dble(wtotd)
      fwihkw(2) = dble(wtoth)

! *** 100 years

      wtime = 3.1536E9
      call fispro
      fwiact(3) = dble(wtota)
      fwigdr(3) = dble(wtotd)
      fwihkw(3) = dble(wtoth)

! *** Outboard first wall ***********************

      wspid = 'Outboard first wall   '

! *** Integrated zone power

      call bbie(203,x,fbbie)
      fwoizp = fbbie(1)

! *** Mean zone power density

      call bbie(207,x,fbbie)
      fwomzp = fbbie(1)

! *** Mean zone neutron flux spectrum

      call bbie(212,x,fbbie)
      do 140 j = 1,100
         wspect(j) = REAL(fbbie(j))
 140  continue

! *** Elemental composition is the same as for the inboard first wall.

! *** Irradiation time

      wtime = REAL(fwlife) * 3.15576E7

! *** Total mass (including coolant) - scale to obtain outboard mass
! *** using the plasma surface area components

      wmass = REAL(fwmass + &
           dencol * fwclfr * fwarea*(fwith+fwoth)/2.0D0 )
      wmass = MAX(1.0E-16, wmass * REAL(sareao/sarea))

! *** Neutron flux (neutrons per cm**2 per second)

      wflux = nneut / REAL(1.0D4*fwarea)

! *** Calculate inventory and dose rates, etc.

      do 150 iwtype = 1,4
         if (iwtype.ne.2) then
            wregn = 4
            wtype = iwtype
            call fispro
         end if
 150  continue

! *** No of atoms in each nuclide is stored in WINVT(359)

! *** Total activity (Bq)

      fwoact(1) = dble(wtota)

! *** Total gamma dose rate (Sv/hr)

      fwogdr(1) = dble(wtotd)

! *** Total heat output (kW)

      fwohkw(1) = dble(wtoth)

! *** Cooling times (no neutron flux incident)

      wflux = 0.0E0
      wtype = 5

! *** 13 weeks (three months)

      wtime = 7.8624E6
      call fispro
      fwoact(2) = dble(wtota)
      fwogdr(2) = dble(wtotd)
      fwohkw(2) = dble(wtoth)

! *** 100 years

      wtime = 3.1536E9
      call fispro
      fwoact(3) = dble(wtota)
      fwogdr(3) = dble(wtotd)
      fwohkw(3) = dble(wtoth)

! *** Output section

      if (iprint.eq.0) goto 1000

 160  continue

      call oheadr(nout,'Inventory Calculations')

      call ovarre(nout,'Tritium breeding ratio','(breedr)',breedr)

      call osubhd(nout,'Inboard Blanket :')
      call ovarre(nout,'Integrated zone power / neutron','(bliizp)', &
           bliizp)
      call ovarre(nout,'Mean zone power density / neutron','(blimzp)', &
           blimzp)
      call ocmmnt(nout,'    (At end of component life)')
      call ovarre(nout,'Total activity (Bq)','(bliact(1))',bliact(1))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(bligdr(1))', &
           bligdr(1))
      call ovarre(nout,'Total heat output (kW)','(blihkw(1))',blihkw(1))
      call ocmmnt(nout,'    (After 3 months cooling time)')
      call ovarre(nout,'Total activity (Bq)','(bliact(2))',bliact(2))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(bligdr(2))', &
           bligdr(2))
      call ovarre(nout,'Total heat output (kW)','(blihkw(2))',blihkw(2))
      call ocmmnt(nout,'    (100 years after decommissioning)')
      call ovarre(nout,'Total activity (Bq)','(bliact(3))',bliact(3))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(bligdr(3))', &
           bligdr(3))
      call ovarre(nout,'Total heat output (kW)','(blihkw(3))',blihkw(3))

      call osubhd(nout,'Outboard Blanket :')
      call ovarre(nout,'Integrated zone power / neutron','(bloizp)', &
           bloizp)
      call ovarre(nout,'Mean zone power density / neutron','(blomzp)', &
           blomzp)
      call ocmmnt(nout,'    (At end of component life)')
      call ovarre(nout,'Total activity (Bq)','(bloact(1))',bloact(1))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(blogdr(1))', &
           blogdr(1))
      call ovarre(nout,'Total heat output (kW)','(blohkw(1))',blohkw(1))
      call ocmmnt(nout,'    (After 3 months cooling time)')
      call ovarre(nout,'Total activity (Bq)','(bloact(2))',bloact(2))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(blogdr(2))', &
           blogdr(2))
      call ovarre(nout,'Total heat output (kW)','(blohkw(2))',blohkw(2))
      call ocmmnt(nout,'    (100 years after decommissioning)')
      call ovarre(nout,'Total activity (Bq)','(bloact(3))',bloact(3))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(blogdr(3))', &
           blogdr(3))
      call ovarre(nout,'Total heat output (kW)','(blohkw(3))',blohkw(3))

      call osubhd(nout,'Inboard First Wall :')
      call ovarre(nout,'Integrated zone power / neutron','(fwiizp)', &
           fwiizp)
      call ovarre(nout,'Mean zone power density / neutron','(fwimzp)', &
           fwimzp)
      call ocmmnt(nout,'    (At end of component life)')
      call ovarre(nout,'Total activity (Bq)','(fwiact(1))',fwiact(1))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(fwigdr(1))', &
           fwigdr(1))
      call ovarre(nout,'Total heat output (kW)','(fwihkw(1))',fwihkw(1))
      call ocmmnt(nout,'    (After 3 months cooling time)')
      call ovarre(nout,'Total activity (Bq)','(fwiact(2))',fwiact(2))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(fwigdr(2))', &
           fwigdr(2))
      call ovarre(nout,'Total heat output (kW)','(fwihkw(2))',fwihkw(2))
      call ocmmnt(nout,'    (100 years after decommissioning)')
      call ovarre(nout,'Total activity (Bq)','(fwiact(3))',fwiact(3))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(fwigdr(3))', &
           fwigdr(3))
      call ovarre(nout,'Total heat output (kW)','(fwihkw(3))',fwihkw(3))

      call osubhd(nout,'Outboard First Wall :')
      call ovarre(nout,'Integrated zone power / neutron','(fwoizp)', &
           fwoizp)
      call ovarre(nout,'Mean zone power density / neutron','(fwomzp)', &
           fwomzp)
      call ocmmnt(nout,'    (At end of component life)')
      call ovarre(nout,'Total activity (Bq)','(fwoact(1))',fwoact(1))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(fwogdr(1))', &
           fwogdr(1))
      call ovarre(nout,'Total heat output (kW)','(fwohkw(1))',fwohkw(1))
      call ocmmnt(nout,'    (After 3 months cooling time)')
      call ovarre(nout,'Total activity (Bq)','(fwoact(2))',fwoact(2))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(fwogdr(2))', &
           fwogdr(2))
      call ovarre(nout,'Total heat output (kW)','(fwohkw(2))',fwohkw(2))
      call ocmmnt(nout,'    (100 years after decommissioning)')
      call ovarre(nout,'Total activity (Bq)','(fwoact(3))',fwoact(3))
      call ovarre(nout,'Total gamma dose rate (Sv/hr)','(fwogdr(3))', &
           fwogdr(3))
      call ovarre(nout,'Total heat output (kW)','(fwohkw(3))',fwohkw(3))

 1000 continue

      return
      end
!----------------------------------------------------------------------
      SUBROUTINE FISPRO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Calls FISPACT subroutines as requested by value of WTYPE
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  25 February 1994
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  25/02/94 PJK 1.010 Added WTYPE = 5 option
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!
! *** Common block ALINK:
!  IX3(70)     INT    Material numbers of first isomers
!  IX4(10)     INT    Material numbers of second isomers
!
! *** Common block BLOCK:
!  A(18000)    REAL   The main array of data
!
! *** Common block CALIST:
!  GMS         REAL   Mass of input material (gms)
!  NSTEPS      INT    Number of subintervals (see WLVL2)
!  YSTEPS      REAL   REAL version of NSTEPS
!  ZLEVEL      REAL   Equilibrium criterion (see WLVL1)
!
! *** Common block CONVRG:
!  MAXXT       INT    Max number of integration steps (see WMAXT)
!  CONVV       REAL   Convergence criterion (see WCONV)
!
! *** Common block DATA0:
!  FLUX(3)     REAL   Proportion of flux in each group
!
! *** Common block DATA1:
!  MIND        DBLE   Minimum number of atoms, not = 0
!
! *** Common block DBLOCK:
!  B(364)      REAL   Number of atoms of each nuclide
!
! *** Common block DOSOUT:
!  XA(19)      REAL   Energy absorption coeff of air (m2/kg) in groups
!  XMU(19)     REAL   Mass attenuation coeff (m2/kg) in group format
!
! *** Common block ENDLS1:
!  ICOU        INT    Counter of number of decays in ENDF file
!  IN(364)     INT    Atomic number of each nuclide
!  IW(364)     INT    Atomic mass of each nuclide
!  KDIC        INT    Counter of number of decays in ENDF file
!
! *** Common block EXT:
!  I22         INT    No of decays & reacts with daughter not in equil
!  I31         INT    No of decays with daughter not in equilibrium
!  I32         INT    I22 + No of decays with parent & daugh in equil
!  BR1(1000)   REAL   Branching ratio of parent -> daugh in decays
!
! *** Common block IDNT:
!  VSIDNT      CHAR*4  Version number 1.0/03 = 1003
!  SPIDNT      CHAR*22 Identifier of spectrum (region)
!  XSIDNT      CHAR*15 Identifier of cross section library
!  DDIDNT      CHAR*15 Identifier of decay data library
!
! *** Common block INFO1:
!  IATMWT(364) INT    Atomic mass of each nuclide
!  FUWT(364)   REAL   Number of atoms of each nuclide
!  ISEX2       INT    Number of types of nuclide in material
!
! *** Common block INFO2:
!  IDS(364)    CHAR*4 Chemical symbol of each nuclide
!
! *** Common block LISTAG:
!  NSTART      INT    Start pointer in A()
!  NCH         INT    Number of decay libraries (=1)
!  NSPEC       INT    Number of gamma groups and data (=29)
!  NISOT       INT    Number of indep fission yield distribs (=0)
!  MTOTAL      INT    Start pointer of summed distribution data
!  MFRAC       INT    Start pointer of yield distribution data
!  N           INT    Number of nuclides in library
!  MASS        INT    Start pointer of atomic masses
!  MIDEN       INT    Start pointer of atomic numbers
!  MLAMDA      INT    Start pointer of decay constants
!  MSPEC       INT    Start pointer of disintgration & spectral data
!  MYILDS      INT    Start pointer of fission yields
!  MYIELD      INT    Start pointer of summed fission yields
!  NTRANS      INT    Number of decays in library
!  MTRANS      INT    Start pointer of decay data
!  NCROSS      INT    Number of neutron reactions in library
!  MCROSS      INT    Start pointer of reaction data
!  NYIELD      INT    Number of fission yields
!
! *** Common block OUT1:
!  FLUX2       REAL   Flux (n/cm2/s) for irradiation (see WFLUX)
!  T           REAL   Time interval (s) (see WTIME)
!
! *** Common block OUT2:
!  NAMREP(83)  CHAR*4 Chemical symbols e.g. 'AG' of elements
!
! *** Common block PROINP:
!  WELEMP(83)  REAL   % by weight of each element input
!  WMASS       REAL   Mass (kg) of material to be irradiated
!  WTIME       REAL   Time (s) of irradiation or cooling
!  WFLUX       REAL   Flux (n/cm2/s) during interval
!  WSPECT(100) REAL   100 group spectrum
!  WTYPE       INT    Specifies what the run of FISP_PRO does
!                     1 - prepares collapsed cross-section file
!                     2 - prepares condensed decay data
!                     3 - prepares array file
!                     4 - does an inventory/activation run
!                     5 - does a further inventory/activation run
!  WREGN       INT    Label (1 - 999) to identify the region
!
! *** Common block PROOUT:
!  WINVT(359)  REAL   The inventory output
!  WTOTA       REAL   The total activity (Bq)
!  WTOTD       REAL   The total gamma dose rate (Sv/hour)
!  WTOTH       REAL   The total heat output (kW)
!  WERRNM      INT    Error number
!                     0 - an error free run
!                     1 - WTYPE value not 1, 2, 3, 4 or 5
!                     2 - Parent nuclide of reaction not in lib
!                     3 - Daughter nuclide of reaction not in lib
!                     4 - WREGN value < 1 or > 999
!                     5 - WTYPE value not 2 or 3
!                     6 - Decay lib and index file not consistent
!                     7 - Not decay data library
!                     8 - Decay mode not in library
!
! *** Common block PROSTR
!  WSPID       CHAR*22 Identifier of region and spectrum
!
! *** Common block PRPRM0
!  WLVL1       REAL   The first LEVEL parameter - C
!  WLVL2       INT    The second LEVEL parameter - N
!  WJSPEK      INT    Value of JSPEK, 1 if approx spectra calc
!  WCONV       REAL   Convergence criterion (=2.E-3)
!  WMAXT       INT    Maximum number of integrating steps (=10)
!
! *** Common block PRPRM1
!  WMIND       DBLE   The value of MIND, minimum no of atoms
!
! *** Common block SPKGN1:
!  JSPEK       INT    Set (=1) if approx spectra calc'ed (see WJSPEC)
!
! *** Common block TRANS0:
!  NP1(1000)   INT    Material numbers of parents for decays & reactions
!  ND1(1000)   INT    Material numbers of daughs for decays & reactions
!  TR1(1000)   REAL   Decay constant*branching ratio or cross section
!  I2          INT    No of decays & reactions with daugh not in equilib
!  I1          INT    Total number of decays and reactions
!  N           INT    Number of nuclides in library
!  YLAM(364)   REAL   (lambda+sigma*phi) value for each nuclide
!
! *** Common block TRANS1:
!  Y(364)      DBLE   Number of atoms of each nuclide after reordering
!
! *** Common block WALLD2:
!  LIBDAT      CHAR*72 Description of cross section library data
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

!  External routines
      EXTERNAL AINP,CALC,COLLXS,DOSES,ENDFPR,INITVR,MASSIN,OUTFIS

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Initialisations

      WERRNM = 0

      CALL INITVR

      IF (WTYPE.EQ.1) THEN
         CALL COLLXS
      ELSE IF (WTYPE.EQ.2) THEN
         WREGN = 0
         CALL ENDFPR
      ELSE IF (WTYPE.EQ.3) THEN
         CALL ENDFPR
      ELSE IF (WTYPE.EQ.4) THEN
         CALL AINP
         CALL MASSIN
         CALL CALC
         CALL DOSES
         CALL OUTFIS
!+**PJK 25/02/94
      ELSE IF (WTYPE.EQ.5) THEN
         CALL CALC
         CALL OUTFIS
!-**PJK 25/02/94
      ELSE
         WERRNM = 1
      END IF

      IF (WERRNM.NE.0) THEN
         WRITE(*,*) 'Error in routine FISPRO:'
         WRITE(*,*) 'FISPRO returns a value of WERRNM = ',WERRNM
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      END IF

      END

!----------------------------------------------------------------------
      BLOCK DATA
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Initialisation of arrays and variables used in COMMON block OUT2
!  (element symbols)
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DATA NAMREP/'H   ','HE  ','LI  ','BE  ','B   ','C   ','N   ', &
           'O   ','F   ','NE  ','NA  ','MG  ','AL  ','SI  ','P   ', &
           'S   ','CL  ','AR  ','K   ','CA  ','SC  ','TI  ','V   ', &
           'CR  ','MN  ','FE  ','CO  ','NI  ','CU  ','ZN  ','GA  ', &
           'GE  ','AS  ','SE  ','BR  ','KR  ','RB  ','SR  ','Y   ', &
           'ZR  ','NB  ','MO  ','TC  ','RU  ','RH  ','PD  ','AG  ', &
           'CD  ','IN  ','SN  ','SB  ','TE  ','I   ','XE  ','CS  ', &
           'BA  ','LA  ','CE  ','PR  ','ND  ','PM  ','SM  ','EU  ', &
           'GD  ','TB  ','DY  ','HO  ','ER  ','TM  ','YB  ','LU  ', &
           'HF  ','TA  ','W   ','RE  ','OS  ','IR  ','PT  ','AU  ', &
           'HG  ','TL  ','PB  ','BI  '/

      END

!----------------------------------------------------------------------
      SUBROUTINE AINP
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Reads an array file and puts all data in A() and commons.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  17 November 1997
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  17/11/97 PJK 1.010 Added IVMS1 coding
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables

      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER ICOU,IN(364),IW(364),KDIC
      COMMON /ENDLS1/ ICOU,IN,IW,KDIC

      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA, &
           MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN, &
           MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      CHARACTER*72 LIBDAT
      COMMON /WALLD2/ LIBDAT

      INTEGER IVMS1
      COMMON /OPSYS/IVMS1

!  Local variables
      CHARACTER*3 NUMRGN
      CHARACTER*20 TEMP
      INTEGER K,KS,KE,KAR

!  External routines
      EXTERNAL CNVNT

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (WREGN.LT.1.OR.WREGN.GT.999) THEN
         WERRNM = 4
         STOP
      END IF
      CALL CNVNT(WREGN,NUMRGN)

!+**PJK 17/11/97
      IF (IVMS1.EQ.1) THEN
         TEMP = 'WARRY'//NUMRGN//'.DAT'
      ELSE
         TEMP = 'fispact/WARRY'//NUMRGN//'.DAT'
      END IF

      OPEN(25,FILE=TEMP,STATUS='OLD',ACCESS='SEQUENTIAL', &
           FORM='UNFORMATTED')
      READ(25) LIBDAT
      READ(25) NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS, &
           MIDEN,MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS
      READ(25) (IX3(K),K=1,70)
      READ(25) (IX4(K),K=1,10)
      READ(25) (FLUX(K),K=1,3),MIND
      READ(25) (B(K),K=1,N)
      READ(25) ICOU,(IN(K),K=1,N)
      READ(25) (IW(K),K=1,N),KDIC
      KS = 1
      DO 710 KAR=1,9
         KE = KS + 1999
         READ(25) (A(K),K=KS,KE)
         KS = KE + 1
 710  CONTINUE

      CLOSE(25)

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE CALC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Call the integrating routine the correct number of times.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      INTEGER NSTEPS
      REAL GMS,YSTEPS,ZLEVEL
      COMMON /CALIST/ GMS,NSTEPS,YSTEPS,ZLEVEL

      REAL FLUX2,T
      COMMON /OUT1  / FLUX2,T

!  Local variables
      INTEGER ISTEPS,INDXG
      REAL TIMT,ELEVEL

!  External routines
      EXTERNAL CHAIN

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! *** Loop over time steps

      DO 10 ISTEPS = 1,NSTEPS
         INDXG = 0

! *** The time in the subinterval calculated
         TIMT = T/YSTEPS

! *** Call CHAIN to do integration

         ELEVEL = ZLEVEL/(TIMT*YSTEPS)
         CALL CHAIN(FLUX2,INDXG,TIMT,ELEVEL)
         INDXG = INDXG + 1

 10   CONTINUE

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE CHAIN(FLUXT, INDXG, TIME, ELEVEL)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  To re-order nuclides, decay types, and reaction types depending on
!  whether or not the nuclide is considered to be in an equilibrium
!  state.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  FLUXT   Irradiation flux (n/cm2/s)
!  INDXG   Start point in FLUX()
!  TIME    Time step (s)
!  ELEVEL  Equilibrium criterion
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      INTEGER INDXG
      REAL ELEVEL,FLUXT,TIME

!  Global variables
      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      INTEGER I22,I31,I32
      REAL BR1(1000)
      COMMON /EXT   / I22,I31,I32,BR1

      INTEGER NP1(1000),ND1(1000),I2,I1,N
      REAL TR1(1000),YLAM(364)
      COMMON /TRANS0/ NP1,ND1,TR1,I2,I1,N,YLAM

      DOUBLE PRECISION Y(364)
      COMMON /TRANS1/ Y

!  Local variables
      DOUBLE PRECISION BOLD(364)
      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,MASS,MIDEN,MLAMDA, &
           MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      INTEGER MATOM1,I,NNN,INDX1,INDX2,INDEX1(364),INDEX2(364), &
           M,MM,ND,NN,NP
      REAL LAMBDA(364),TCROSS(364),XLAM,YTOTAL

!  External routines
      EXTERNAL IDNTFY,INTEGS,INTEGT

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NSTART = 0
      CALL IDNTFY(NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N, &
                  MASS,MIDEN,MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS, &
                  NCROSS,MCROSS,NYIELD)
      MATOM1 = 0

      DO 10 I=1,N
         Y(I) = 0.0D0
         YLAM(I) = 0.0
         INDEX1(I) = 0
         INDEX2(I) = 0
         LAMBDA(I) = 0.0
 10   CONTINUE
      NNN = NTRANS + NCROSS
      DO 20 I=1,NNN
         NP1(I) = 0
         ND1(I) = 0
         TR1(I) = 0.0
 20   CONTINUE
      IF (FLUXT.LE.0.0) NCROSS = 0
!
! COPIES  DECAY  CONSTANTS INTO LAMBDA(-)
!
      INDX1 = MLAMDA
      DO 30 I=1,N
         INDX1 = INDX1 + 1
         TCROSS(I) = 0.0
         LAMBDA(I) = A(INDX1)
 30   CONTINUE
!
! CALCULATES  TOTAL CROSS  SECTIONS  TCROSS(-)
!
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 60
      DO 50 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
! ## IF statement added so that only +ve cross sections are added
!    to total. Rewritten (-ve) cross sections ignored. (July 1989)
         IF (A(INDX1).GT.0.) THEN
            TCROSS(NP) = TCROSS(NP) + A(INDX1)*FLUX(INDX2)
         END IF
 50   CONTINUE
 60   CONTINUE
      M = 0
      DO 80 I=1,N
         XLAM = LAMBDA(I)
         IF (XLAM.LT.ELEVEL) GOTO 70
         GOTO 80
 70      CONTINUE
         M = M + 1
         INDEX1(I) = M
         INDEX2(M) = I
 80   CONTINUE
      MM = M
      DO 90 I=1,N
         XLAM = LAMBDA(I)
         IF (XLAM.LT.ELEVEL) GOTO 90
         MM = MM + 1
         INDEX1(I) = MM
         INDEX2(MM) = I
 90   CONTINUE
!
! LISTS  NP1(I1)  ND1(I1)  NR1(I1)  TR1(I1)
! NP1=PARENT  ISOTOPE, ND1=DAUGHTER  ISOTOPE
! NR1=0   TR1=BRANCHING  RATIO * DECAY  CONSTANT
! NR1=1   TR1= CROSS  SECTION
!
      I1 = 0
      INDX1 = MTRANS
      IF (NTRANS.EQ.0) GOTO 130
      DO 120 I=1,NTRANS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX2 = MLAMDA + NP
         NP = INDEX1(NP)
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         IF (ND.EQ.0) GOTO 110
         ND = INDEX1(ND)
         INDX1 = INDX1 + 1
         IF (ND.GT.M) GOTO 120
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
! ## ABS added so that -ve branching ratios treated correctly (Nov89)
         TR1(I1) = ABS(A(INDX1))*A(INDX2)
         BR1(I1) = ABS(A(INDX1))
         GOTO 120
 110     CONTINUE
         INDX1 = INDX1 + 1
 120  CONTINUE
 130  CONTINUE
      I31 = I1
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 210
      DO 200 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         NP = INDEX1(NP)
         IF (ND.EQ.0) GOTO 180
         ND = INDEX1(ND)
         IF (ND.GT.M) GOTO 180
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
         TR1(I1) = 0.0
         BR1(I1) = 0.0
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
! ## ABS function added so that both +ve and -ve cross sections are
!    treated correctly. (July 1989)
         TR1(I1) = TR1(I1) + ABS(A(INDX1))*FLUXT*FLUX(INDX2)
         GOTO 190
 180     CONTINUE
         INDX1 = INDX1 + 1

 190     CONTINUE
 200  CONTINUE
 210  CONTINUE
!
! LISTS  NP1(I1)  ND1(I1)  NR1(I1)  TR1(I1)
! NP1=PARENT  ISOTOPE, ND1=DAUGHTER  ISOTOPE
! NR1=0  TR1=BRANCHING  RATIO * DECAY  CONSTANT
! NR1=1  TR1=CROSS SECTION
!
      I2 = I1
      INDX1 = MTRANS
      IF (NTRANS.EQ.0) GOTO 240
      DO 230 I=1,NTRANS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX2 = MLAMDA + NP
         NP = INDEX1(NP)
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         IF (ND.EQ.0) GOTO 220
         ND = INDEX1(ND)
         INDX1 = INDX1 + 1
         IF (ND.LE.M) GOTO 230
         IF (NP.GT.M) GOTO 230
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
! ## ABS added so that -ve branching ratios treated correctly (Nov89)
         TR1(I1) = ABS(A(INDX1))*A(INDX2)
         BR1(I1) = ABS(A(INDX1))
         GOTO 230
 220     CONTINUE
         INDX1 = INDX1 + 1
 230  CONTINUE
 240  CONTINUE
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 320
      DO 310 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         NP = INDEX1(NP)
         IF (ND.EQ.0) GOTO 290
         ND = INDEX1(ND)
         IF (ND.LE.M) GOTO 290
         IF (NP.GT.M) GOTO 290
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
         TR1(I1) = 0.0
         BR1(I1) = 0.0
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
! ## ABS function added so that both +ve and -ve cross sections are
!    treated correctly. (July 1989)
         TR1(I1) = TR1(I1) + ABS(A(INDX1))*FLUXT*FLUX(INDX2)
         GOTO 300
 290     CONTINUE
         INDX1 = INDX1 + 1

 300     CONTINUE
 310  CONTINUE
 320  CONTINUE
!
! LISTS  NP1(I1)  ND1(I1)  NR1(I1)  TR1(I1)
! NP1=PARENT  ISOTOPE,ND1=DAUGHTER  ISOTOPE
! NR1=0  TR1=BRANCHING  RATIO * DECAY  CONSTANT
! NR1=1  TR1=CROSS  SECTION
!
      I22 = I1
      INDX1 = MTRANS
      IF (NTRANS.EQ.0) GOTO 350
      DO 340 I=1,NTRANS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX2 = MLAMDA + NP
         NP = INDEX1(NP)
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         IF (ND.EQ.0) GOTO 330
         ND = INDEX1(ND)
         INDX1 = INDX1 + 1
         IF (ND.LE.M) GOTO 340
         IF (NP.LE.M) GOTO 340
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
! ## ABS added so that -ve branching ratios treated correctly (Nov89)
         TR1(I1) = ABS(A(INDX1))*A(INDX2)
         BR1(I1) = ABS(A(INDX1))
         GOTO 340
 330     CONTINUE
         INDX1 = INDX1 + 1
 340  CONTINUE
 350  CONTINUE
      I32 = I1
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 430
      DO 420 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         NP = INDEX1(NP)
         IF (ND.EQ.0) GOTO 400
         ND = INDEX1(ND)
         IF (ND.LE.M) GOTO 400
         IF (NP.LE.M) GOTO 400
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
         TR1(I1) = 0.0
         BR1(I1) = 0.0
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
! ## ABS function added so that both +ve and -ve cross sections are
!    treated correctly. (July 1989)
         TR1(I1) = TR1(I1) + ABS(A(INDX1))*FLUXT*FLUX(INDX2)
         GOTO 410
 400     CONTINUE
         INDX1 = INDX1 + 1

 410     CONTINUE
 420  CONTINUE
 430  CONTINUE
!
! YLAM=DECAY CONSTANT  Y=NUMBER DENSITY   YTOTAL=TOTAL ATOMS
!
      YTOTAL = 0.0
      DO 440 I=1,N
         NN = INDEX2(I)
         INDX1 = MATOM1 + NN
         YLAM(I) = LAMBDA(NN) + TCROSS(NN)*FLUXT
         Y(I) = B(INDX1)
         IF (Y(I).LT.1.0D0) Y(I) = 0.0D0
 440  CONTINUE
      YTOTAL = 1.0
      IF (MAXXT.LT.10) THEN
         CALL INTEGS(M, FLUXT, YTOTAL, TIME)
         GOTO 473
      END IF
      CALL INTEGT(M, FLUXT, YTOTAL, TIME)
!
! PUTS  ATOMIC  NUMBER  DENSITIES  INTO  ORIGINAL  SCHEME
!
 473  CONTINUE
      DO 480 I=1,N
         NN = INDEX2(I)
         INDX1 = MATOM1 + NN
         BOLD(INDX1) = B(INDX1)
         IF (Y(I).LT.1.0D-60) Y(I) = 0.0D0
         B(INDX1) = Y(I)*DBLE(YTOTAL)
         BOLD(INDX1) = (BOLD(INDX1)+B(INDX1))/2.0D0
 480  CONTINUE

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE COLLXS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Prepares collapsed cross-section file.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  17 November 1997
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  17/11/97 PJK 1.010 Added IVMS1 coding
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER IVMS1
      COMMON /OPSYS/IVMS1

!  Local variables
      CHARACTER*3 NUMRGN
      CHARACTER*8 DUMTIM
      CHARACTER*20 TEMP
      CHARACTER*47 CLIDNT
      INTEGER MREAC(114),NP(1000),ND(1000),NP2(1000),ND2(1000),I,N, &
           NUCL(364),IN,IEXTRA,NUCL1,MT,NCROSS,MT1,MT2,NT1,M,N0,J0, &
           MFICTN,JPAR(500),IFLAG,J,JJ,K,KERROR,NUCL2
      REAL FLUX(101),ZSECT(1000),ZSEKT(1000),XSECT(101),DUMMY

!  External routines
      EXTERNAL CNVNT,CLOKK

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (WREGN.LT.1.OR.WREGN.GT.999) THEN
         WERRNM = 4
         STOP
      END IF
      CALL CNVNT(WREGN,NUMRGN)

!+**PJK 17/11/97
      IF (IVMS1.EQ.1) THEN
         TEMP = 'WCOLL'//NUMRGN//'.DAT'
         OPEN(24,FILE= TEMP,          STATUS='UNKNOWN')
         OPEN(21,FILE='INDEXSIM.DAT', STATUS='OLD')
         OPEN(22,FILE='PROCXSEC.DAT', STATUS='OLD')
      ELSE
         TEMP = 'fispact/WCOLL'//NUMRGN//'.DAT'
         OPEN(24,FILE= TEMP,          STATUS='UNKNOWN')
         OPEN(21,FILE='fispact/INDEXSIM.DAT', STATUS='OLD')
         OPEN(22,FILE='fispact/PROCXSEC.DAT', STATUS='OLD')
      END IF

      M = 0
      N0 = 1
      J0 = 1
! Define MFICTN as the ZA&Isom number of Fe51 15/3/93
      MFICTN = 260510

      DO 10 I = 1,101
         FLUX(I) = 0.0
 10   CONTINUE
      DO 20 I = 1,114
         MREAC(I) = 0
 20   CONTINUE
      DO 30 I = 1,1000
         NP(I) = 0
         ND(I) = 0
         NP2(I) = 0
         ND2(I) = 0
         ZSECT(I) = 0.
         ZSEKT(I) = 0.
 30   CONTINUE
!
!  **READ IN LIST OF NUCLIDE IDENTIFIERS AND POSITIONS**
!
      I = 1
 50   CONTINUE
      IF (I.LE.364) THEN
         READ(21,99001,END=60) N,NUCL(I)
      ELSE
         GOTO 60
      END IF
      I = I + 1
      GOTO 50
!
!  **GET FLUXES (HIGH ENERGY FIRST) FROM COMMON**
!
 60   CONTINUE

      DO 65 I =1,100
         FLUX(I) = WSPECT(I)
         FLUX(101) = FLUX(101) + FLUX(I)
 65   CONTINUE
!
!  **READ CROSS SECTION LIBRARY**
!
      IN = 0
!  ## Need to read start of file a 2nd time for gas nuclides (Nov89)
      IEXTRA = 0
      MREAC(16) = 10
      MREAC(17) = 20
      MREAC(22) = 20040
      MREAC(23) = 60120
      MREAC(24) = 20050
      MREAC(25) = 20060
      MREAC(28) = 10010
      MREAC(29) = 40080
      MREAC(30) = 40090
      MREAC(32) = 10020
      MREAC(33) = 10030
      MREAC(34) = 20030
      MREAC(37) = 30
      MREAC(42) = 10020
      MREAC(4) = 0
      MREAC(102) = -10
      MREAC(103) = 10000
      MREAC(104) = 10010
      MREAC(105) = 10020
      MREAC(106) = 20020
      MREAC(107) = 20030
      MREAC(108) = 40070
      MREAC(109) = 60110
      MREAC(111) = 20010
      MREAC(112) = 30040
      MREAC(113) = 50100
      MREAC(114) = 60090
 95   CONTINUE
      READ(22,99004)
      READ(22,99008) XSIDNT
      DO 100 J = 1,14
         READ(22,99004)
 100  CONTINUE
 110  CONTINUE
      IN = IN + 1
      DO 120 I = 1,101
         XSECT(I) = 0.0
 120  CONTINUE
 130  CONTINUE
      READ(22,99002,END=160) NUCL1,MT,NCROSS
      READ(22,99004)
      READ(22,99004)
      READ(22,99003) (XSECT(J),J=1,NCROSS)
      IF (NCROSS.EQ.1) GOTO 130
!  ## Need to read start of file a 2nd time for gas nuclides (Nov89)
      IF (IEXTRA.EQ.1) THEN
         IF (NUCL1.GT.20040) GOTO 160
         NUCL1 = NUCL1 + 3
      END IF
!
!  **WORK OUT DAUGHTERS AND CONVERT IDENTIFIERS**
!
      MT1 = INT(MT/10)
!
!  **CHECK FOR ISOMERIC STATE AS TARGET**
!
      NT1 = INT(NUCL1/10)
      NT1 = NT1*10
      MT2 = MT - 10*MT1
      IF (MT1.EQ.0) MT1 = 1
!  ## MODIFICATION TO ALLOW FOR FISSION. HARWELL 1991
!  @@ And to allow input of data for reactions forming
!     Fe-51. Use the ENDF MT value of 43 for these data
!     Required for testing PROCESS work 12/3/93.
      IF (MT1.EQ.18) THEN
         NUCL2 = 0
      ELSE IF (MT1.EQ.43) THEN
         NUCL2 = MFICTN
      ELSE
         NUCL2 = NT1 - MREAC(MT1) + MT2
      END IF
!
!  **CHANGE IDENTIFIERS TO POSITIONS**
!
!  ## THIS PART MADE MORE EFFICIENT HARWELL 1990
      J = J0
      KERROR = 0
 132  CONTINUE
      IF (NUCL1.NE.NUCL(J)) THEN
         J = J+1
         IF (J.LT.N) GOTO 132
         J = 1
         IF (KERROR.EQ.1) THEN
            WRITE(*,*) 'Error in routine COLLXS:'
            WRITE(*,*) 'NUCL1 = ',NUCL1
            WRITE(*,*) '   MT = ',MT
            WERRNM = 2
            WRITE(*,*) 'WERRNM = ',WERRNM
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF
         KERROR = KERROR + 1
         GOTO 132
      END IF
      NP(IN) = J
      J0 = J
      J = N0
      KERROR = 0
 134  CONTINUE
      IF (NUCL2.EQ.0) THEN
         J = 0
      ELSE IF (NUCL2.NE.NUCL(J)) THEN
         J = J+1
         IF (J.LT.N) GOTO 134
         J = 1
         IF (KERROR.EQ.1) THEN
            WRITE(*,*) 'Error in routine COLLXS:'
            WRITE(*,*) 'NUCL1 = ',NUCL1
            WRITE(*,*) '   MT = ',MT
            WERRNM = 3
            WRITE(*,*) 'WERRNM = ',WERRNM
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF
         KERROR = KERROR + 1
         GOTO 134
      ELSE
         CONTINUE
      END IF
      ND(IN) = J
      N0 = J0-80
      IF (N0.LT.1) N0 = 1
!  ## END OF MODS
      IF (MT.EQ.41.AND.ND(IN).EQ.0) GOTO 130
!
!  **COLLAPSE ENERGY GROUPS**
!
      DO 150 J = 1,NCROSS
         XSECT(101) = XSECT(101) + (XSECT(J)*FLUX(J))
 150  CONTINUE
      XSECT(101) = XSECT(101)/FLUX(101)
!  ## Do not use data if effective cross section < 1.E-12 barns
!  ## Harwell 21/11/90
      IF (XSECT(101).LT.1.E-12) GOTO 130
      ZSECT(IN) = XSECT(101)
!  ## By repeating the 5 gas nuclides, (in all libraries), try
!     to get all gases (Nov 89).
!  ## Correction made to include reactions to isomers, 
!     change MT to MT1 and divide reac number by 10. Mar 90
!  ## Additional reactions for He4 production (Nov 1989)##
      IF (MT1.EQ.107.OR.MT1.EQ.22.OR.MT1.EQ.24) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
!  ## Additional reactions for He3 production (Nov 1989)##
      IF (MT1.EQ.106.OR.MT1.EQ.34) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-1
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
!  ## Additional reactions for H3 production (Nov 1989)##
      IF (MT1.EQ.105.OR.MT1.EQ.33) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-2
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
!  ## Additional reactions for H2 production (Nov 1989)##
      IF (MT1.EQ.104.OR.MT1.EQ.32) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-3
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
!  ## Additional reactions for H1 production (Nov 1989)##
      IF (MT1.EQ.103.OR.MT1.EQ.28.OR.MT1.EQ.42) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-4
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
!  ## Additional reactions for H1 production (Nov 1989)##
      IF (MT1.EQ.111) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-4
         ZSECT(IN) = -2.*ZSECT(IN-1)
         GOTO 110
      END IF
!  ## Additional reactions for He4 production (Nov 1989)##
      IF (MT1.EQ.29) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N
         ZSECT(IN) = -2.*ZSECT(IN-1)
      END IF
      GOTO 110
!  ## Need to read start of file a 2nd time for gas nuclides (Nov89)
 160  CONTINUE
      IEXTRA = IEXTRA + 1
      IF (IEXTRA.EQ.1) THEN
         REWIND(22)
         IN = IN - 1
         GOTO 95
      END IF
!
!  **IF SAME NP AND ND OCCUR FOR GIVEN NUCLIDE, THEN SUM X-SECTIONS
!  **SO THAT REACTION OCCURS BUT ONCE.
!
!  ##Sum cross sections only if both +ve or -ve. This means that some
!    -ve cross sections (for light elements) are ignored. (July 1989)
!  ## By including the 5 gas nuclides should remove problem (Nov 89).
!  ## HARWELL MODIFICATION NOV 1990.
!  ## MAKES SUMMING MORE EFFICIENT
      N0 = NP(1)
      JPAR(1) = 1
      J0 = 1
      DO 172 J = 1,IN
         IF (NP(J).NE.N0) THEN
            J0 = J0 +1
            N0 = NP(J)
            JPAR(J0) = J
         END IF
 172  CONTINUE
      DO 180 JJ = 1,J0-1
         DO 178 J = JPAR(JJ),JPAR(JJ+1)-1
            M = M + 1
            IFLAG = 0
            DO 176 K = J+1,JPAR(JJ+1)-1
               IF (ND(J).EQ.ND(K)) THEN
                  IF (ZSECT(J).GT.0.) THEN
                     IF (ZSECT(K).GT.0.) THEN
                        ZSECT(K) = ZSECT(J) + ZSECT(K)
                     ELSE
                        ZSECT(K) = ZSECT(J)
                     END IF
                  ELSE
                     IF (ZSECT(K).LT.0.) THEN
                        ZSECT(K) = ZSECT(J) + ZSECT(K)
                     END IF
                  END IF
                  IFLAG = IFLAG + 1
                  IF (IFLAG.EQ.1) M = M - 1
!  ## NEXT LINE ADDED 26/6/91. PROBLEM IS THAT IF THREE REACTIONS HAVE
!     THE SAME DAUGHTER THEN THE FIRST CROSS SECTION WAS ADDED TWICE.
                  GOTO 177
               END IF
 176        CONTINUE
 177        CONTINUE
            IF (IFLAG.LE.0) THEN
               NP2(M) = NP(J)
               ND2(M) = ND(J)
               ZSEKT(M) = ZSECT(J)
            END IF
 178     CONTINUE
 180  CONTINUE
!  ## END OF HARWELL MODIFICATION
!
!  **NOW WRITE OUT IN FISPIN FORM**
!
!  #  GET CURRENT DATE
      CALL CLOKK(DUMMY,DUMTIM,TEMP)
      CLIDNT = TEMP(1:2)//TEMP(4:5)//TEMP(7:8)// &
           VSIDNT//XSIDNT//SPIDNT
      WRITE(24,99006) M,CLIDNT
      WRITE(24,99005) (NP2(I),ND2(I),ZSEKT(I),I=1,M)
      WRITE(24,99010)
      WRITE(24,99011) (FLUX(I),I=1,100)

!+**PJK 02/12/93
      CLOSE(21)
      CLOSE(22)
      CLOSE(24)

! **FORMAT STATEMENTS**
99001 FORMAT(I6,10X,I6)
99002 FORMAT(1X,I6,1X,I4,1X,I4)
99003 FORMAT(6E12.5)
99004 FORMAT(A80)
99005 FORMAT(1X,I4,2X,I4,4X,1PE12.5)
99006 FORMAT(1X,I5,2X,A47)
99007 FORMAT(1X,'NUCL1 = ',I6,'  MT = ',I6)
99008 FORMAT(6X,A15)
99010 FORMAT(1X,'100')
99011 FORMAT(1X,E11.5)

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE CLOKK(T,H,D)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Routine to return the data and time
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  T      : (OUTPUT) Time in seconds
!  H      : (OUTPUT) Time in HH:MM:SS format
!  D      : (OUTPUT) Date in DD/MM/YY format
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      CHARACTER*8 H,D
      REAL T

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!+**PJK 26/11/93 CHARACTER*8 EDATE,TIME

!+**PJK 26/11/93 Commented out CLOCK call : CALL CLOCK(T)
      T = 0.0

!+**PJK 26/11/93 Commented out TIME call : H=TIME()
      H = 'HH:MM:SS'

!+**PJK 25/11/93 Commented out EDATE call : D=EDATE()
      D = 'DD/MM/YY'

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE CNVNT(NUMB,CNUMB)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Converts between region number and the character representation
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  NUMB   : (INPUT)  An integer in the range 0 - 999
!  CNUMB  : (OUTPUT) The character*3 representation e.g. '006'
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      CHARACTER*3 CNUMB
      INTEGER NUMB

!  Local variables
      INTEGER IH,IT,IU,NZERO

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NZERO = ICHAR('0')
      IH = INT(NUMB/100)
      IF (IH.EQ.0) THEN
         CNUMB(1:1) = '0'
      ELSE
         CNUMB(1:1) = CHAR(IH+NZERO)
      END IF

      IT = INT((NUMB - 100 * IH)/10)
      IF (IT.EQ.0) THEN
         CNUMB(2:2) = '0'
      ELSE
         CNUMB(2:2) = CHAR(IT+NZERO)
      END IF

      IU = NUMB - 100 * IH - 10 * IT
      IF (IU.EQ.0) THEN
         CNUMB(3:3) = '0'
      ELSE
         CNUMB(3:3) = CHAR(IU+NZERO)
      END IF

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE CONV1(ZB,MAT)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Converts between material numbers and ENDF identifiers
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  ZB    : (INPUT)  Standard ENDF/B 'ZA' identifier
!  MAT   : (OUTPUT) Material number (0 - N)
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      INTEGER MAT
      REAL ZB

!  Global variables
      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL A(18000)
      COMMON /BLOCK / A

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

!  Local variables
      INTEGER I,IA,IZ,M,MASS,MATA,MATB,MC,MEND,MIDEN,MIDF,N,NISOT,NSP, &
           NST
      REAL AW,ZA,ZN

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (ZB.GT.0.0) THEN
         NST = 3
         NSP = INT(A(NST))
         NST = NST + 18*NSP + 1
         NISOT = INT(A(NST))
         NST = NST + NSP + NISOT + 1
         N = INT(A(NST))
         MASS = NST
         NST = NST + N
         MIDEN = NST
         NST = NST + N
         MEND = NST
         MIDF = MIDEN + 1
         ZA = ZB/1000.0
         IZ = INT(ZA)
         ZN = REAL(IZ)
         AW = ZB - 1000.0*ZN
         DO 10 I = MIDF,MEND
            IF (INT(A(I)).EQ.IZ) THEN
               IA = I - N
               IF (INT(A(IA)).EQ.INT(AW)) GOTO 20
            END IF
 10      CONTINUE
      END IF
      MAT = 0
      GOTO 30
 20   CONTINUE
      MAT = I - MIDEN
 30   CONTINUE
      GOTO 1000
!-----------------------------------------------------------------------
!-    TESTS IF ISOMER IS IN LIBRARY
!-----------------------------------------------------------------------
      ENTRY CONXA(M,MATA,MC)
      IF (M.EQ.2) THEN
         MATB = MATA + 2
         DO 40 I = 1,10
            IF (MATB.EQ.IX4(I)) GOTO 60
 40      CONTINUE
      ELSE
         MATB = MATA + 1
         DO 50 I = 1,70
            IF (MATB.EQ.IX3(I)) GOTO 60
 50      CONTINUE
      END IF
      MC = 0
      GOTO 1000

 60   CONTINUE
      MC = MATB
      GOTO 1000
!-----------------------------------------------------------------------
!-    RETURNS AN INTEGER DEPENDING ON THE ISOMERIC STATE
!-----------------------------------------------------------------------
      ENTRY CONYA(MAT,MC)
      DO 70 I = 1,70
         IF (MAT.EQ.IX3(I)) GOTO 90
 70   CONTINUE
      DO 80 I = 1,10
         IF (MAT.EQ.IX4(I)) GOTO 100
 80   CONTINUE
      MC = 1
      GOTO 1000

 90   CONTINUE
      MC = 2
      GOTO 1000

 100  CONTINUE
      MC = 3

 1000 CONTINUE
      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE DOSES
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Calculates absorption and attenuation coefficients. Data in the
!  common DOSOUT are used in routine OUTFIS to calculate dose rates.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      REAL XA(19),XMU(19)
      COMMON /DOSOUT/ XA,XMU

      INTEGER IATMWT(364),ISEX2
      REAL FUWT(364)
      COMMON /INFO1 / IATMWT,FUWT,ISEX2

      CHARACTER*4 IDS(364)
      COMMON /INFO2 / IDS

      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

!  Local variables
      INTEGER IZZ(40),IZ(83),JZZ(50),DFLAG,I,IJ,IK,IL,J,K,KH,KL,M1
      REAL XAA(17),E(19),EE(17),XM(19,50),XMM(17,40),FWT(83), &
           YM(19,50),AX,AY,DE,DZ,FAWT,TOTT

!  External functions

!  External routines

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!+**PJK 26/11/93
      SAVE DFLAG

      DATA DFLAG/0/

      DATA E/.15,.25,.35,.5,.7,.9,1.11,1.33,1.55,1.83,2.25,2.75,3.5, &
           4.5,5.75,7.25,9.0,11.0,13.0/

      DATA EE/.15,.2,.3,.4,.5,.6,.8,1.,1.5,2.,3.,4.,5.,6.,8.0,10.0,15.0/

      DATA IZZ/1,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,22,25,26, &
           29,30,31,32,33,35,47,48,50,52,53,55,56,58,74,79,80,82,92/

      DATA XAA/2.494,2.672,2.872,2.949,2.966,2.953,2.882,2.787,2.545, &
           2.342,2.054,1.866,1.737,1.644,1.521,1.446,1.349/

      DATA ((XMM(I,J),I=1,14),J=1,10)/26.51,24.29,21.12,18.93,17.29, &
           15.99,14.05,12.63,10.27,8.77,6.923,5.807,5.049,4.498,11.57, &
           10.6,9.208,8.249,7.532,6.968,6.121,5.503,4.475,3.83,3.042, &
           2.572,2.257,2.03,11.9,10.89,9.46,8.473,7.737,7.156,6.286, &
           5.652,4.597,3.937,3.138,2.664,2.347,2.121,12.43,11.36,9.865, &
           8.834,8.065,7.46,6.552,5.89,4.792,4.108,3.284,2.799,2.477, &
           2.248,13.47,12.29,10.66,9.545,8.712,8.058,7.077,6.362,5.177, &
           4.443,3.562,3.047,2.708,2.469,13.53,12.33,10.68,9.555,8.72, &
           8.064,7.082,6.366,5.181,4.45,3.579,3.073,2.742,2.511,13.6, &
           12.37,10.7,9.567,8.729,8.071,7.087,6.37,5.186,4.458,3.597, &
           3.1,2.777,2.553,12.98,11.76,10.15,9.072,8.275,7.65,6.716, &
           6.036,4.915,4.229,3.422,2.96,2.663,2.457,13.35,11.99,10.29, &
           9.185,8.371,7.736,6.788,6.1,4.968,4.283,3.486,3.038,2.753, &
           2.559,13.93,12.45,10.65,9.491,8.646,7.988,7.008,6.296,5.129, &
           4.425,3.613,3.159,2.873,2.681/

      DATA ((XMM(I,J),I=1,14),J=11,20)/13.78,12.23,10.42,9.276,8.446, &
           7.801,6.842,6.146,5.007,4.324,3.541,3.107,2.836,2.655,14.47, &
           12.75,10.82,9.614,8.748,8.077,7.082,6.361,5.183,4.48,3.679, &
           3.24,2.967,2.788,14.31,12.5,10.55,9.359,8.51,7.855,6.884, &
           6.182,5.038,4.359,3.59,3.172,2.915,2.748,15.05,13.01,10.91, &
           9.666,8.782,8.103,7.099,6.374,5.195,4.498,3.716,3.294,3.037, &
           2.872,14.79,12.65,10.53,9.311,8.452,7.795,6.826,6.127,4.994, &
           4.328,3.585,3.189,2.95,2.798,14.25,12.04,9.95,8.775,7.957, &
           7.334,6.419,5.761,4.696,4.073,3.384,3.019,2.803,2.666,15.79, &
           13.18,10.79,9.493,8.599,7.921,6.929,6.217,5.067,4.4,3.665, &
           3.281,3.055,2.915,16.71,13.74,11.15,9.781,8.849,8.146,7.122, &
           6.388,5.207,4.525,3.78,3.395,3.17,3.034,16.46,13.13,10.42, &
           9.08,8.191,7.529,6.572,5.891,4.802,4.18,3.511,3.173,2.981, &
           2.868,18.34,13.89,10.62,9.131,8.191,7.508,6.536,5.852,4.768, &
           4.161,3.524,3.213,3.044,2.951/

      DATA ((XMM(I,J),I=1,14),J=21,30)/19.6,14.58,10.98,9.398,8.413, &
           7.703,6.698,5.994,4.883,4.265,3.622,3.311,3.146,3.057,22.1, &
           15.57,11.18,9.409,8.36,7.624,6.605,5.9,4.803,4.204,3.599, &
           3.318,3.176,3.108,23.35,16.15,11.41,9.537,8.45,7.694,6.656, &
           5.942,4.835,4.236,3.635,3.36,3.225,3.161,23.8,16.17,11.22, &
           9.321,8.234,7.486,6.465,5.767,4.692,4.113,3.539,3.28,3.156, &
           3.099,24.84,16.58,11.3,9.326,8.212,7.453,6.427,5.728,4.658, &
           4.087,3.525,3.276,3.159,3.108,26.14,17.15,11.49,9.408,8.257, &
           7.481,6.439,5.735,4.661,4.093,3.539,3.297,3.187,3.141,28.89, &
           18.34,11.84,9.554,8.325,7.513,6.442,5.727,4.65,4.089,3.553, &
           3.326,3.229,3.194,54.04,29.63,15.57,11.3,9.314,8.145,6.762, &
           5.919,4.754,4.209,3.754,3.606,3.577,3.601,55.7,30.29,15.68, &
           11.27,9.244,8.058,6.666,5.826,4.673,4.139,3.698,3.559,3.536, &
           3.564,60.67,32.49,16.36,11.55,9.364,8.109,6.661,5.8,4.639, &
           4.113,3.687,3.562,3.549,3.584/

      DATA ((XMM(I,J),I=1,14),J=31,40)/64.65,34.16,16.75,11.61,9.317, &
           8.013,6.533,5.667,4.518,4.009,3.606,3.495,3.492,3.534,69.48, &
           36.5,17.68,12.15,9.69,8.304,6.744,5.838,4.647,4.124,3.716, &
           3.607,3.608,3.655,75.89,39.41,18.62,12.57,9.911,8.431,6.789, &
           5.852,4.641,4.121,3.725,3.625,3.635,3.689,78.27,40.45,18.91, &
           12.65,9.922,8.41,6.744,5.801,4.592,4.078,3.692,3.598,3.611, &
           3.669,86.86,44.52,20.39,13.42,10.41,8.757,6.963,5.962,4.701, &
           4.176,3.791,3.704,3.726,3.792,158.1,78.44,32.38,19.25,13.78, &
           10.93,8.065,6.616,5.,4.432,4.075,4.037,4.103,4.211,186., &
           92.14,37.45,21.8,15.3,11.94,8.604,6.953,5.167,4.568,4.201, &
           4.166,4.239,4.355,190.9,94.56,38.34,22.24,15.55,12.1,8.679, &
           6.993,5.179,4.575,4.208,4.172,4.245,4.362,201.4,99.85,40.26, &
           23.23,16.13,12.48,8.869,7.103,5.222,4.607,4.234,4.197,4.272, &
           4.391,259.1,129.8,51.91,29.22,19.76,14.9,10.16,7.894,5.586, &
           4.876,4.446,4.391,4.463,4.583/

      DATA ((XMM(I,J),I=15,17),J=1,40)/3.746,3.254,2.539,1.725,1.529, &
           1.252,1.819,1.827,1.360,1.945,1.755,1.495,2.154,1.960,1.698, &
           2.209,2.024,1.783,2.263,2.089,1.866,2.195,1.039,1.846,2.319, &
           2.181,2.022,2.445,2.313,2.168,2.437,2.318,2.195,2.574,2.462, &
           2.352,2.552,2.452,2.364,2.683,2.590,2.517,2.628,2.549,2.496, &
           2.517,2.451,2.419,2.766,2.704,2.687,2.893,2.839,2.878,2.760, &
           2.728,2.761,2.876,2.871,2.951,2.991,2.994,3.092,3.074,3.103, &
           3.247,3.138,3.175,3.335,3.086,3.130,3.300,3.104,3.156,3.341, &
           3.146,3.207,3.405,3.218,3.293,3.521,3.723,3.882,4.277,3.691, &
           3.853,4.253,3.724,3.896,4.316,3.683,3.860,4.290,3.815,4.002, &
           4.455,3.860,4.057,4.529,3.843,4.042,4.518,3.981,4.194,4.699, &
           4.472,4.747,5.384,4.633,4.926,5.598,4.643,4.937,5.613,4.675, &
           4.972,5.658,4.879,5.194,5.926/

!**** All values need a factor of .001
      IF (DFLAG.LE.0) THEN
         DO 20 I = 1,17
            DO 10 J = 1,40
               XMM(I,J) = XMM(I,J)*.001
 10         CONTINUE
            XAA(I) = XAA(I)*.001
 20      CONTINUE
!**** Calculates the two arrays by interpolation
         DO 60 J = 1,19
            DO 25 I = 41,50
               XM(J,I) = 0.0
 25         CONTINUE
            K = 1
 30         CONTINUE
            IF (EE(K).GE.E(J)) THEN
               KH = K
               IF (ABS(EE(KH)-E(J)).LT. 0.001) THEN
                  XA(J) = XAA(KH)
                  DO 40 I = 1,40
                     XM(J,I) = XMM(KH,I)
                     YM(J,I) = 0.0
 40               CONTINUE
               ELSE
                  KL = K - 1
                  DE = (E(J)-EE(KL))/(EE(KH)-EE(KL))
                  XA(J) = XAA(KL) + DE*(XAA(KH)-XAA(KL))
                  DO 50 I = 1,40
                     XM(J,I) = XMM(KL,I) + DE*(XMM(KH,I)-XMM(KL,I))
                     YM(J,I) = 0.0
 50               CONTINUE
               END IF
            ELSE
               K = K + 1
               GOTO 30
            END IF
 60      CONTINUE
         DFLAG = 1
!**** Calculates the weight fraction of initial materials
         TOTT = 0.0
         FAWT = 0.0
         IJ = 0
         DO 80 J = 1,83
            FWT(J) = 0.0
            IZ(J) = 0
            IK = 0
            DO 70 K = 1,ISEX2
               IF (IDS(K).EQ.NAMREP(J)) THEN
                  FAWT = FUWT(K)*REAL(IATMWT(K))
                  FWT(J) = FWT(J) + FAWT
                  IK = 1
               END IF
 70         CONTINUE
            IF (IK.NE.0) THEN
               IJ = IJ + 1
               IZ(IJ) = J
               TOTT = TOTT + FWT(J)
            END IF
 80      CONTINUE
         DO 90 J = 1,83
            FWT(J) = FWT(J)/TOTT
 90      CONTINUE
         TOTT = TOTT*1.66056559E-27
         TOTT = WMASS
!**** Identifies the materials for which mu will need calculating
         IL = 0
         DO 120 J = 1,IJ
            K = 1
 100        CONTINUE
            IF (IZZ(K).GE.IZ(J)) THEN
               KH = K
               IF (IZZ(KH).NE.IZ(J)) THEN
                  KL = K - 1
                  DZ = REAL(IZ(J)-IZZ(KL))/REAL(IZZ(KH)-IZZ(KL))
                  IL = IL + 1
                  JZZ(IL) = IZ(J)
                  DO 110 I = 1,19
                     YM(I,IL) = XM(I,KL) + DZ*(XM(I,KH)-XM(I,KL))
 110              CONTINUE
               END IF
            ELSE IF (K.LT.40) THEN
               K = K + 1
               GOTO 100
            ELSE
               KH = 40
               KL = 39
               DZ = REAL(IZ(J)-IZZ(KL))/REAL(IZZ(KH)-IZZ(KL))
               IL = IL + 1
               JZZ(IL) = IZ(J)
               DO 111 I = 1,19
                  YM(I,IL) = XM(I,KL) + DZ*(XM(I,KH)-XM(I,KL))
 111           CONTINUE
            END IF
 120     CONTINUE
!**** Calculates the weighted values of MU over original materials
         DO 130 I = 1,19
            XMU(I) = 0.0
 130     CONTINUE
         DO 170 J = 1,IJ
            K = 1
            M1 = 0
 140        CONTINUE
            IF (IZ(J).NE.IZZ(K)) THEN
               K = K + 1
               IF (K.LE.40) GOTO 140
               M1 = 1
               K = 1
 150           CONTINUE
               IF (IZ(J).NE.JZZ(K)) THEN
                  K = K + 1
                  GOTO 150
               END IF
            END IF
            AX = 1.
            AY = 0.
            IF (M1.NE.0) THEN
               AX = 0.
               AY = 1.
            END IF
            DO 160 I = 1,19
               XMU(I) = XMU(I) + FWT(IZ(J))*(AX*XM(I,K)+AY*YM(I,K))
 160        CONTINUE
 170     CONTINUE
      END IF

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE ENDFP
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Reads an ENDF/BV decay data file. If no gamma spectral data are
!  given then an approximate spectrum is calculated if 'SPEK' is set.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  17 November 1997
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  17/11/97 PJK 1.010 Corrected EXTERNAL statement (CONV --> CONV1)
!                     Added IVMS1 coding
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER ICOU,IN(364),IW(364),KDIC
      COMMON /ENDLS1/ ICOU,IN,IW,KDIC

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA, &
           MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN, &
           MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      INTEGER JSPEK
      COMMON /SPKGN1/ JSPEK

      INTEGER IVMS1
      COMMON /OPSYS/IVMS1

!  Local variables
      CHARACTER*1 NUMCHR
      CHARACTER*4 ANOT(20)
      CHARACTER*50 FILENM,TEMPFN

      REAL AE(6),XEXP(25),ENAC(25),XX(200),YY(200)
      REAL A1,A2,A9,AW,AWI,AWR,B1,B2,BR,D7,DCK,DELB,DELQ,DELT,DER,DERB, &
           DFC,DFD,DIC,DIL,DIS,DRI,EFACT,ELIS,EMAX,EMAY,ENAS,ENI,ER,ERB, &
           FC,FD,FLODIV,FNUBAR,PAR,QUE,RFS,RI,RICC,RICK,RICL,RIS,RTYP, &
           SPI,STA,STYP,SUMAIN,SUMBR,THAL,TYP,XINT,Y1,ZA,ZAA

      INTEGER IDMF(50,3),ILA(6),NBT(200),INT1(200)
      INTEGER I,I1,I2,IC,ICC,ID,IL,ILINES,IMAX,IREAD,IRS,IRT,IST,IZ,J, &
           JA,JCOU,K,K1,K2,K3,K4,KCOU,L3,LCON,LENGTH,LFI,LIS,LISO,LRP, &
           M1,M2,M3,M4,M5,M6,MAT,MATA,MATD,MC,MCO,MCP,MD,MF,MT,N1,NB, &
           NDK,NER,NEUTC,NFOR,NLIB,NLIN,NMOD,NN,NP,NR,NSP,NSPG,NSUB, &
           NT,NUM,NUMBR,NUME,NVER,NWD,NXC

!  External functions
      INTEGER LENGT
      EXTERNAL LENGT

!  External routines
      EXTERNAL CONV1,CONXA,CONYA

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DATA ENAC/0.0,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.6,0.8,1.0,1.22, &
           1.44,1.66,2.0,2.5,3.0,4.0,5.0,6.5,8.0,10.0,12.0,14.0,20.0/

! ENERGY BOUNDARIES

      NB = NSPEC - 5
!
! **READING ENDF/BV GENERAL DATA(MT=451)
!
      JA = 0
      MATD = 0
      MAT = 0
!  ## Changes so that decay data is read in from several files
!     It is assumed that the files have the extension .00n
!
      IREAD = 0
 65   CONTINUE

!+**PJK 17/11/97
      IF (IVMS1.EQ.1) THEN
         OPEN(23,FILE='PROCDEC1.001',STATUS='UNKNOWN')
      ELSE
         OPEN(23,FILE='fispact/PROCDEC1.001',STATUS='UNKNOWN')
      END IF

      READ(23,99002) ILINES
      DO 70 NLIN = 1,ILINES
         READ(23,99001) (ANOT(J),J=1,17)
 70   CONTINUE
      MAT = 0
      IF (IREAD.EQ.1) MATD = MATD - 1

 80   CONTINUE
      IF (MAT.EQ.-1) GOTO 210
      IF ( MATD.EQ.N) GOTO 210

 81   CONTINUE
      READ(23,99003,END=1000) &
           ZA,AWR,LRP,LFI,NLIB,NMOD,MAT,MF,MT,NUM
      JA = JA + 1
!
! **NLIB IS LIBRARY IDENTIFIER(=0 FOR ENDF)
! **NMOD IS MODIFICATION NO.
!
      MATD = MATD + 1
      IF (MAT.EQ.-1) GOTO 210
      MD = MATD
!
! **MAT MATERIAL NO. ON DATA TAPE.MATD PROGRAM COUNT OF MATERIALS
!
      READ(23,99003) ELIS,STA,LIS,LISO,N1,NFOR,MAT,MF,MT,NUM
      JA = JA + 1
!
! **STA=0=STABLE NUCLIDE,STA=1=UNSTABLE NUCLIDE
!
! **TEST FOR ENDFB6 FORMAT DATA
!
      IF (NFOR.EQ.6) THEN
         JA = JA + 1
         READ(23,99003) AWI,AWI,N1,N1,NSUB,NVER,MAT,MF,MT,NUM
         IF (NSUB.NE.4) THEN
            WRITE(*,*) 'Error in routine ENDFP:'
            WRITE(*,*) 'ZA = ',ZA
            WERRNM = 7
            WRITE(*,*) 'WERRNM = ',WERRNM
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF
      END IF

      READ(23,99003) A1,A2,M3,M4,NWD,NXC,MAT,MF,MT,NUM
      JA = JA + 1
!
! **NWD NO. OF COMMENT CARDS
! **NXC NO. OF DICTIONARY CARDS
!
      DO 90 I = 1,NWD
         READ(23,99005) (ANOT(NN),NN=1,17),MAT,MF,MT,NUM
         JA = JA + 1
 90   CONTINUE
      DO 100 I = 1,NXC
         READ(23,99003) B1,B2,IDMF(I,1),IDMF(I,2),IDMF(I,3),IL,MAT,MF, &
              MT,NUM
         JA = JA + 1
 100  CONTINUE
      JA = 0
      MCO = 1
      MCP = MCO + 1
!
! ** JA=COUNTER ON INPUT ITEMS
! ** MCO=CURRENT NO. OF DECAY REACTIONS
!
! ****** SEND ******
!
 110  CONTINUE
      READ(23,99006) (ILA(I),I=1,6),MAT,MF,MT,NUM
      KDIC = KDIC + 1
!
! ** KDIC=NO. OF DECAY REACTIONS
!
      IF (MT.GT.0) THEN
         WRITE(*,*) 'Error in routine ENDFP:'
         WRITE(*,*) 'NUM = ',NUM,' in SEND section.'
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      END IF
      IF (MCO.LE.364) THEN
!
! ****** FEND ******
!
         READ(23,99008) MAT,MF,MT,NUM
         IF (MF.NE.0) THEN
            WRITE(*,*) 'Error in routine ENDFP:'
            WRITE(*,*) 'NUM = ',NUM,' in FEND section.'
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF
         READ(23,99003) ZA,AWR,LIS,LISO,M1,NSP,MAT,MF,MT,NUM
         IF (MF.LE.0) GOTO 80
         IF (INT(STA).EQ.0) GOTO 80
         MCO = MCO + 1
         MCP = MCO + 1
!
! ** NSP TOTAL NO. OF RADIATION SPECTRA
!
         READ(23,99010) THAL,DELT,M2,M3,NUME,M4,MAT,MF,MT,NUM, &
              (AE(NN),NN=1,NUME)
!
! **THE ARRAY AE(N) CONTAINS 6 NOS:EB,DEB,EG,DEG,EA,DEA
! **THEY ARE THE MEAN ENERGIES PER DISINTEGRATION & ARE REQUIRED
! **FOR DECAY HEAT CALCULATIONS
!
         JA = JA + 1
         MATA = MATD + MLAMDA
         A(MATA) = 0.693147/THAL
         READ(23,99003) SPI,PAR,M5,M6,L3,NDK,MAT,MF,MT,NUM
!
! **NDK IS THE TOTAL NO. OF DECAY MODES GIVEN,L3=6*NDK
!
         JA = JA + 1
         SUMBR = 0.0
         EMAX = 0.0
         A9 = 14.
         DO 160 I = 1,NDK
            ICOU = ICOU + 1
            READ(23,99011) RTYP,RFS,QUE,DELQ,BR,DELB,MAT,MF,MT,NUM
            JA = JA + 1
            SUMBR = SUMBR + BR
            IRT = INT(RTYP)
            IF (ABS(RTYP-1.5).LT. 0.001) IRT = 11
            IF (ABS(RTYP-1.4).LT. 0.001) IRT = 12
!
! **RTYP IS THE CODE WHICH DEFINES THE DECAY MODE
!
            A(ICOU) = REAL(MATD)
            MC = MATD
            IF (I.LE.1) THEN
!
! **NEXT SECTION DONE ONCE FOR EACH NUCLIDE
!
               JCOU = MSPEC + NSPEC*(MC-1) + 1
               JCOU = JCOU + 1
               DO 120 IC = 1,3
                  JCOU = JCOU + 1
                  IF (IC.EQ.1) ICC = 5
! A(JCOU)=QUE*1.0E-6
                  IF (IC.EQ.2) ICC = 1
                  IF (IC.EQ.3) ICC = 3
                  A(JCOU) = AE(ICC)*1.0E-6
 120           CONTINUE
            END IF
            IRS = INT(RFS)
!
! **RFS IS DAUGHTER STATE(0=GROUND,1=ISOMERIC)
!
            ICOU = ICOU + 1
!
! **IRT=1 BETA - DECAY
! **IRT=2 BETA + DECAY
! **IRT=3 ISOMERIC TRANSITION
! **IRT=4 ALPHA DECAY
! **IRT=5 NEUTRON EMISSION
! **IRT=6 SP. FISSION DECAY
! **IRT=7 PROTON EMISSION
! **IRT=10 UNKNOWN ORIGIN
! **IRT=11 BETA- DECAY & NEUTRON EMISSION
! **IRT=12 BETA- DECAY & ALPHA EMISSION
!
            IF (IRT.EQ.1) THEN
            ELSE IF (IRT.EQ.2) THEN
            ELSE IF (IRT.EQ.4) THEN
            ELSE IF (IRT.EQ.5) THEN
            ELSE IF (IRT.EQ.6) THEN
!
! **DECAY IS BY SP. FISSION
!
               ZAA = 0.0
!
! **DAUGHTER NOT IN LIBRARY
!
               A(ICOU) = 0.0
               GOTO 130
            ELSE IF (IRT.EQ.7) THEN
            ELSE IF (IRT.EQ.8) THEN
               GOTO 140
            ELSE IF (IRT.EQ.9) THEN
               GOTO 140
            ELSE IF (IRT.EQ.10) THEN
               WRITE(*,*) 'Error in routine ENDFP:'
               WRITE(*,*) 'IRT = ',IRT
               WRITE(*,*) 'PROCESS continuing.'
               GOTO 150
            ELSE IF (IRT.EQ.11) THEN
            ELSE IF (IRT.EQ.12) THEN
            ELSE IF (IRT.EQ.13) THEN
               GOTO 140
            ELSE
               CALL CONYA(MATD,MC)
               IF (MC.EQ.2) THEN
                  A(ICOU) = REAL(MATD - 1)
               ELSE IF (MC.EQ.3) THEN
                  IF (IRS.EQ.0) A(ICOU) = REAL(MATD - 2)
                  IF (IRS.EQ.1) A(ICOU) = REAL(MATD - 1)
                  IF (IRS.EQ.2) A(ICOU) = REAL(MATD)
               ELSE
                  A(ICOU) = REAL(MATD)
               END IF
               GOTO 130
            END IF
            FLODIV = ZA/1000.
            IZ = INT(FLODIV)
            AW = ZA - REAL(IZ)*1000.0
!
! **SETTING IZ & AW FOR DIFFERENT DECAYS
!
            IF (IRT.EQ.1) IZ = IZ + 1
            IF (IRT.EQ.2) IZ = IZ - 1
            IF (IRT.EQ.4) IZ = IZ - 2
            IF (IRT.EQ.4) AW = AW - 4.0
            IF (IRT.EQ.5) AW = AW - 1.0
            IF (IRT.EQ.7) IZ = IZ - 1
            IF (IRT.EQ.7) AW = AW - 1.0
            IF (IRT.EQ.11) THEN
               IZ = IZ + 1
               AW = AW - 1.0
            END IF
            IF (IRT.EQ.12) THEN
               IZ = IZ - 1
               AW = AW - 4.0
            END IF
!
! **SETTING IZ & AW FOR DAUGHTER
!
            ZAA = AW + 1000.0*REAL(IZ)
            CALL CONV1(ZAA,MATA)
            IF (IRS.GT.0) THEN
               CALL CONXA(IRS,MATA,MC)
!
! **THIS SECTION IDENTIFIES ISOMERIC DAUGHTER
!
               A(ICOU) = REAL(MC)
            ELSE
!
! **NEXT SECTION ONLY FOR GROUND STATE OF DAUGHTER
!
               A(ICOU) = REAL(MATA)
            END IF
 130        CONTINUE
            ICOU = ICOU + 1
            A(ICOU) = BR
            GOTO 150
 140        CONTINUE
            WRITE(*,*) 'Error in routine ENDFP:'
            WRITE(*,*) 'IRT = ',IRT
            WERRNM = 8
            WRITE(*,*) 'WERRNM = ',WERRNM
            WRITE(*,*) 'PROCESS stopping.'
            STOP
!
! **THIS SECTION CALCULATES MAX ENERGY OF GENERATED GAMMA SPECTRUM
!
 150        CONTINUE
            IF (JSPEK.NE.0) THEN
               IF (IRT.EQ.3) THEN
                  EMAY = AE(3)*1.0E-6
                  IF (EMAY.GT.EMAX) EMAX = EMAY
                  GOTO 160
               END IF
               IF (IRT.EQ.2) THEN
                  EMAY = 5.
                  IF (EMAY.GT.EMAX) EMAX = EMAY
                  GOTO 160
               END IF
               IF (IRT.EQ.1) THEN
                  EMAY = AE(1)*2.0E-6
                  IF (EMAY.GT.EMAX) EMAX = EMAY
               END IF
            END IF
! ## adding in -ve branching ratios to A(). (Nov89)
            IF (IRT.EQ.4.OR.IRT.EQ.12) THEN
               ICOU = ICOU + 1
               A(ICOU) = A(ICOU-3)
               ICOU = ICOU + 1
               A(ICOU) = 5.0
               ICOU = ICOU + 1
               A(ICOU) = -A(ICOU-3)
            END IF
 160     CONTINUE
!
! *** END OF LOOP ON DECAYS ***
!
!
! **OMIT NEXT SECTION IF NO. OF RADIATION SPECTRA(NSP)=0
! **AND CARRY OUT SPECTAL GENERATION
!
         IF (NSP.EQ.0) GOTO 220
         NSPG = 0
         DO 200 K = 1,NSP
            READ(23,99003) D7,STYP,LCON,K1,K2,NER,MAT,MF,MT,NUM
            JA = JA + 1
!
! **NER NO. OF DISCRETE ENERGIES,LCON=0:DISCRETE SPECTRUM
! **LCON=1:CONTINUOUS SPECTRUM,LCON=2:DISCRETE&CONTINUOUS
! **D9 IS AN INTEGER(=6) INDICATING NO. OF ITEMS IN FOLLOWING LIST
!
            READ(23,99014) FD,DFD,ERB,DERB,FC,DFC,MAT,MF,MT,NUM
            JA = JA + 1
!
! *** IF STYP=5 (SP. FISSION) FC=NUBAR=NO. OF ***
! *** PROMPT NEUTRONS EMMITTED PER  FISSION   ***
!
            IST = INT(STYP)
            IF (IST.EQ.5 .AND. LCON.EQ.1) FNUBAR = FC
            IF (IST.EQ.5) THEN
               NEUTC = MSPEC + NSPEC*(MD-1) + 1
               A(NEUTC) = FNUBAR + A(NEUTC)
            END IF
! **OMIT NEXT SECTION IF ONLY CONTINUOUS SPECTRUM IS GIVEN
!
            IF (LCON.NE.1) THEN
               SUMAIN = 0.0
!
! **SUMAIN=SUM OF INTENSITIES OF CURRENT DECAY SPECTRUM
!
               IF (NER.NE.0) THEN
                  DO 190 NN = 1,NER
                     READ(23,99003) ER,DER,K2,K3,NT,K4,MAT,MF,MT,NUM
!
! **NT NO. OF PARAMETERS FOR ENERGY ER (NT=6 EXCEPT FOR STYP=0.0)
!
                     JA = JA + 1
!
! **STYP=0 GAMMA RADIATION
! **STYP=1 BETA - RADIATION
! **STYP=2 BETA + RADIATION
! **STYP=4 ALPHA RADIATION
! **STYP=5 NEUTRONS
! **STYP=6 SP. FISSION FRAGMENTS
! **STYP=7 PROTONS
! **STYP=8 DISCRETE ELECTRONS
! **STYP=9 X-RAYS
!
                     READ(23,99011) RTYP,TYP,RI,DRI,RIS,DIS,MAT,MF,MT, &
                          NUM
                     JA = JA + 1
                     IF (IST.EQ.0 .OR. IST.EQ.9) THEN
!
! **RI=INTENSITY OF DISGRETE ENERGY ER
!
                        SUMAIN = SUMAIN + RI
                        ENI = ER*RI
!
! **IC ENERGY BAND BOUNDARY
!
                        DO 170 IC = 1,NB
                           ID = IC
                           ENAS = ENAC(IC)*1.0E6
                           IF (ER.LE.ENAS) GOTO 180
 170                    CONTINUE
!
! **DECREASE KCOU BY 1 ELSE 1ST GROUP ALWAYS EMPTY. (HARWELL)
!
 180                    CONTINUE
                        KCOU = JCOU + ID - 1
!
! *** MULTIPLICATION FACTORS CHANGED BY 100 IN FOLLOWING TWO LINES.
! *** HARWELL JULY 1987
!
                        A(KCOU) = A(KCOU) + ENI*1.0E-6*FD
                     END IF
                     IF (NT.GT.6) THEN
                        READ(23,99011) RICC,DIC,RICK,DCK,RICL,DIL,MAT, &
                             MF,MT,NUM
                        JA = JA + 1
                     END IF
 190              CONTINUE
               END IF
!
! **OMIT IF LCON=0
!
               IF (LCON.EQ.0) THEN
                  IF (IST.EQ.0 .OR. IST.EQ.9) NSPG = NSPG + 1
                  GOTO 200
               END IF
            END IF
!  ## Y2 and Y3 replaced by K1 and K2 in next line as format demands
!  ## integer values. HARWELL 21/11/90.
            READ(23,99015) RTYP,Y1,K1,K2,NR,NP,MAT,MF,MT,NUM, &
                 (NBT(NN),INT1(NN),NN=1,NR)
            JA = JA + 1
            READ(23,99016) (XX(NN),YY(NN),NN=1,NP)
            JA = JA + 1
!
! **NR NO. OF INTERPOLATION RANGES FOR CONTINUOUS SPECTRUM
! **NP NO. OF PTS AT WHICH THE CONTINUOUS SPECTRUM  IS GIVEN
!
            IF (IST.EQ.0 .OR. IST.EQ.9) NSPG = NSPG + 1
 200     CONTINUE
         IF (NSPG.NE.0) GOTO 110
         GOTO 220
      END IF
!  ## Changed so that decay file read a second time only
!  for the first 5 gas nuclides. (Nov 89)
 210  CONTINUE
      IREAD = IREAD + 1
      IF (IREAD.EQ.1) GOTO 65

      GOTO 99999

 220  CONTINUE
      DO 230 I1 = 1,NB
         IMAX = I1
         IF (EMAX.LE.ENAC(I1+1)) GOTO 240
 230  CONTINUE
      IF (I1.EQ.NB+1) THEN
         EMAX = 0.0
         GOTO 110
      END IF
 240  CONTINUE
      IF (AE(3).GT.1.E3) THEN
         EFACT = (A9*AE(3)*1.0E-6/EMAX)/(1.-(1.+A9)*EXP(-A9))
         XEXP(1) = 1.0
         DO 250 I2 = 1,IMAX
            XEXP(I2+1) = EXP(-A9*ENAC(I2+1)/EMAX)
            XINT = EFACT*(XEXP(I2)-XEXP(I2+1))
            A(JCOU+I2) = XINT*(ENAC(I2+1)+ENAC(I2))/2.
 250     CONTINUE
      END IF
      EMAX = 0.0
      GOTO 110
!  ## Changes so that files closed and opened in sequence
!  ## 6/9/91
 1000 CONTINUE
      INQUIRE(23,NAME=FILENM)
      CLOSE(23)
      LENGTH=LENGT(FILENM)
      NUMCHR=FILENM(LENGTH:LENGTH)
! ## To increase portability 48 -> ICHAR('0') 13/1/93
      NUMBR=ICHAR(NUMCHR)-ICHAR('0')
      NUMBR=NUMBR+1
      TEMPFN=FILENM(1:LENGTH)
!  ## Change made because there are 10 decay data files for EAF-3
!  ## 18/9/92
      IF (NUMBR.EQ.10) THEN
         FILENM=TEMPFN(1:LENGTH-2)//'10'
      ELSE
! ## To increase portability 48 -> ICHAR('0') 13/1/93
         FILENM=TEMPFN(1:LENGTH-1)//CHAR(ICHAR('0')+NUMBR)
      END IF
      OPEN(23,FILE=FILENM,STATUS='UNKNOWN')
      GOTO 81
!  ## End of modification
99001 FORMAT(20A4)
99002 FORMAT(I6)
99003 FORMAT(2E11.5,4I11,I4,I2,I3,I5)
99004 FORMAT(' ',' ZA is ',1PE11.5)
99005 FORMAT(16A4,A2,I4,I2,I3,I5)
99006 FORMAT(6I11,I4,I2,I3,I5)
99007 FORMAT(' ',' SEND FAULT AT CARD NUMBER ',I6)
99008 FORMAT(66X,I4,I2,I3,I5)
99009 FORMAT(' ','FEND FAULT AT CARD NUMBER',I5)
99010 FORMAT(2E11.5,4I11,I4,I2,I3,I5/(6E11.5))
99011 FORMAT(6E11.5,I4,I2,I3,I5)
99012 FORMAT(' ',' RTYPE ',I4,' UNKNOWN ORIGIN ')
99013 FORMAT(' ',' RTYPE =',I4)
99014 FORMAT(6E11.5,I4,I2,I3,I5)
99015 FORMAT(2E11.4,4I11,I4,I2,I3,I5/(6I11))
99016 FORMAT(6E11.4)

99999 CONTINUE

      CLOSE(23)

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE ENDFPR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Initialises and produces lists of isomers prior to reading decay
!  and cross section libraries. Writes a condensed form of these
!  for subsequent runs.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  17 November 1997
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  17/11/97 PJK 1.010 Added IVMS1 coding
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL A(18000)
      COMMON /BLOCK / A

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION BB(364)
      COMMON /DBLOCK/ BB

      INTEGER ICOU,IN(364),IW(364),KDIC
      COMMON /ENDLS1/ ICOU,IN,IW,KDIC

      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA, &
           MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN, &
           MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      CHARACTER*72 LIBDAT
      COMMON /WALLD2/ LIBDAT

      INTEGER IVMS1
      COMMON /OPSYS/IVMS1

!  Local variables
      CHARACTER*1  NUMCHR
      CHARACTER*3  NUMRGN
      CHARACTER*4  ANOT(20)
      CHARACTER*8  DUMTIM
      CHARACTER*20 TEMP
      CHARACTER*47 CLIDNT
      CHARACTER*50 FILENM,TEMPFN
      CHARACTER*72 OLDLIB

      INTEGER I,IA,ILINES,ISCT3,ISCT4,IX,IY,IZA,IZZ,J,J3,JCS,JCT,K,KAR, &
           KE,KMAT,KNISOT,KS,KTRANS,LCON,LENGTH,LFI,LISO,LRP,LSTART, &
           MAST,MAT,MATC,MF,MIDEP,MT,N1,N2,NDK,NER,NFOR,NG,NLIB,NLIN, &
           NMAT,NMOD,NN,NP,NP2,NR,NR2,NSP,NSUB,NT,NUM,NUMBEG,NUMBR, &
           NVER,NWD,NXC
      INTEGER NUCL(364),LX(200)

      REAL Y(200)
      REAL AWI,AWR,BARNS,DUMMY,RTYP,ZA

!  External functions
      INTEGER LENGT
      EXTERNAL LENGT

!  External routines
      EXTERNAL CLOKK,CNVNT,ENDFP

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NSTART = 0
      LSTART = 0
      NCH = 1
      NSPEC = 29
      NISOT = 0
      N = 364
      NTRANS = 268
      NCROSS = 700
      NMAT = 0
      KMAT = 0

!+**PJK 17/11/97
      IF (IVMS1.EQ.1) THEN
         OPEN(23,FILE='PROCDEC1.001',STATUS='UNKNOWN')
         OPEN(21,FILE='INDEXSIM.DAT',STATUS='OLD')
      ELSE
         OPEN(23,FILE='fispact/PROCDEC1.001',STATUS='UNKNOWN')
         OPEN(21,FILE='fispact/INDEXSIM.DAT',STATUS='OLD')
      END IF

      IF (WTYPE.EQ.3) THEN
         CALL CNVNT(WREGN,NUMRGN)

!+**PJK 17/11/97
         IF (IVMS1.EQ.1) THEN
            TEMP = 'WCOLL'//NUMRGN//'.DAT'
            OPEN(24,FILE=TEMP,STATUS='OLD')
            OPEN(25,FILE='WARRY000.DAT',STATUS='OLD', &
                 ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
         ELSE
            TEMP = 'fispact/WCOLL'//NUMRGN//'.DAT'
            OPEN(24,FILE=TEMP,STATUS='OLD')
            OPEN(25,FILE='fispact/WARRY000.DAT',STATUS='OLD', &
                 ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
         END IF

         KNISOT = 0
         READ(25) OLDLIB
         READ(25) &
              NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA, &
              MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS
         READ(25) (IX3(K),K=1,70)
         READ(25) (IX4(K),K=1,10)
         READ(25) (FLUX(K),K=1,3),MIND
         READ(25) (BB(K),K=1,N)
         READ(25) ICOU,(IN(K),K=1,N)
         READ(25) (IW(K),K=1,N),KDIC
         KS = 1
         DO 2 KAR=1,9
            KE = KS + 1999
            READ(25) (A(K),K=KS,KE)
            KS = KE + 1
 2       CONTINUE
         CLOSE(25)

!  ## HARWELL MODIFICATION DECEMBER 1990.
         READ(24,99020) NCROSS,CLIDNT
         BACKSPACE(24)
         XSIDNT = CLIDNT(11:25)
         SPIDNT = CLIDNT(26:47)
         DDIDNT = OLDLIB(58:72)
 
!
!  RESET NCROSS AND ERASE OLD CROSS SECTION DATA
!
         NSTART = MCROSS
         A(NSTART) = REAL(NCROSS)
         NSTART = NSTART + 1
         DO 3 I = NSTART,18000
            A(I) = 0.0
 3       CONTINUE
         GOTO 132
!
!  ## END OF MODIFICATION FOR OPTION 'ARRA'
!
      ELSE IF (WTYPE.EQ.1.OR.WTYPE.GE.4) THEN
         WERRNM = 5
         STOP
      ELSE
         CONTINUE
      END IF
!  ## HARWELL MODIFICATION NOV 1990.
!  ## READ IN LIST OF NUCLIDE ZA IDENTIFIERS
!
      I = 1
 5    CONTINUE
      READ(21,99018,END=7) NUCL(I)
      I = I + 1
      GOTO 5

 7    CONTINUE
      ISCT3 = 0
      ISCT4 = 0
      KTRANS = 0
      MATC = 0
!  ## Changes so that decay data is read in from several files
!     It is assumed that the files have the extension .00n
!     6/9/91
!
      READ(23,99002) ILINES,DDIDNT
      DO 10 NLIN = 1,ILINES
         READ(23,99001) (ANOT(J),J=1,17)
 10   CONTINUE
 20   CONTINUE
      READ(23,99003,END=1000) &
           ZA,AWR,LRP,LFI,NLIB,NMOD,MAT,MF,MT,NUM
      IF (MAT.EQ.-1) THEN
         N = MATC
!  ## Changes so that 5 gas nuclides repeated. (Nov 89)
         NTRANS = KTRANS + 1
         DO 25 I = 1,5
            IN(N+I) = IN(I)
            IW(N+I) = IW(I)
 25      CONTINUE
         N = N + 5
      ELSE
         MATC = MATC + 1
         IZA = INT(ZA + 0.1)
         IZZ = IZA/1000
         IN(MATC) = IZZ
         IW(MATC) = IZA - 1000*IZZ
         READ(23,99006) LISO,NFOR
!  ## HARWELL MODIFICATION NOV 1990.
!  ## TEST THAT INDEX IS CONSISTENT WITH DECAY DATA
         IF (NUCL(MATC).NE.(10*INT(ZA)+LISO)) THEN
            WRITE(*,*) 'Error in routine ENDFPR:'
            WRITE(*,*) 'MATC = ',MATC
            WRITE(*,*) 'NUCL(MATC) = ',NUCL(MATC)
            WRITE(*,*) 'INT(ZA) = ',INT(ZA)
            WRITE(*,*) 'LISO = ',LISO
            WERRNM = 6
            WRITE(*,*) 'WERRNM = ',WERRNM
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF
!
! **TEST FOR ENDFB6 FORMAT DATA
!
         IF (NFOR.EQ.6) THEN
            READ(23,99003) AWI,AWI,N1,N1,NSUB,NVER,MAT,MF,MT,NUM
            IF (NSUB.NE.4) THEN
               WRITE(*,*) 'Error in routine ENDFPR:'
               WRITE(*,*) 'ZA = ',ZA
               WRITE(*,*) 'PROCESS stopping.'
               STOP
            END IF
         END IF

         READ(23,99008) NWD,NXC
         DO 30 JCT = 1,NWD
            READ(23,99009)
 30      CONTINUE
         DO 40 JCS = 1,NXC
            READ(23,99009)
 40      CONTINUE
!
! *** FOR NXC=1 AS IN STABLE NUCLIDES, SKIP SPECTRAL DATA
!
         IF (NXC.NE.1) THEN
            LISO = LISO + 1
            IF (LISO.EQ.1) THEN
            ELSE IF (LISO.EQ.3) THEN
               ISCT4 = ISCT4 + 1
               IX4(ISCT4) = MATC
            ELSE
               ISCT3 = ISCT3 + 1
               IX3(ISCT3) = MATC
            END IF
            READ(23,99009)
            READ(23,99009)
            READ(23,99010) NSP
            READ(23,99009)
            READ(23,99009)
            READ(23,99010) NDK
            KTRANS = KTRANS + NDK
            DO 50 J3 = 1,NDK
! ## Read RTYP so that KTRANS is increased for alpha decays.
! NB FORMAT 99011 also changed. (Nov 89)
               READ(23,99011) RTYP
               IF (ABS(RTYP-4.0).LT.0.001) KTRANS=KTRANS+1
               IF (ABS(RTYP-1.4).LT.0.001) KTRANS=KTRANS+1
 50         CONTINUE
            IF (NSP.NE.0) THEN
               DO 70 K = 1,NSP
                  READ(23,99012) LCON,NER
                  READ(23,99009)
                  IF (LCON.NE.1) THEN
                     IF (NER.NE.0) THEN
                        DO 60 N2 = 1,NER
                           READ(23,99013) NT
                           READ(23,99009)
                           IF (NT.GT.6) READ(23,99009)
 60                     CONTINUE
                     END IF
                     IF (LCON.EQ.0) GOTO 70
                  END IF
                  READ(23,99014) NR,NP
                  NR2 = NR*2
                  NP2 = NP*2
                  READ(23,99015) (LX(NN),NN=1,NR2)
                  READ(23,99016) (Y(NN),NN=1,NP2)
 70            CONTINUE
            END IF
         END IF
         READ(23,99009)
         READ(23,99009)
         READ(23,99017) MAT
         GOTO 20
      END IF
!  ## HARWELL MODIFICATION DECEMBER 1990.
      CLIDNT = '          PROCESS XSECT1 Dummy region 000      '
      XSIDNT = CLIDNT(11:25)
      SPIDNT = CLIDNT(26:47)
      NISOT = KMAT
!
! *** ARRAY A IDENTIFIERS ***
!
      NSTART = NSTART + 1
      A(NSTART) = REAL(NCH)
      NSTART = NSTART + 1
      A(NSTART) = 1.0
      NSTART = NSTART + 1
      A(NSTART) = REAL(NSPEC)
      NSTART = NSTART + NSPEC*18
      NSTART = NSTART + 1
      A(NSTART) = REAL(NISOT)
      MTOTAL = NSTART
      NSTART = NSTART + NSPEC
      MFRAC = NSTART
      NSTART = NSTART + NISOT
      NSTART = NSTART + 1
      A(NSTART) = REAL(N)
      MASS = NSTART
      NSTART = NSTART + N
      MIDEN = NSTART
      NSTART = NSTART + N
      MLAMDA = NSTART
      NSTART = NSTART + N
      MSPEC = NSTART
      NSTART = NSTART + N*NSPEC
      MYILDS = NSTART
      NSTART = NSTART + NYIELD*NISOT
      MYIELD = NSTART
      A(MYIELD+1) = 1.0
      NSTART = NSTART + N + 1
      A(NSTART) = REAL(NTRANS)
      MTRANS = NSTART
      NSTART = NSTART + NTRANS*3
      NSTART = NSTART + 1
      MCROSS = NSTART
      A(NSTART) = REAL(NCROSS)
      NSTART = NSTART + 1
      DO 90 I = 1,N
         LSTART = LSTART + 1
         BB(LSTART) = 0.0D0
 90   CONTINUE
      IA = 0
      MAST = MASS + 1
      DO 120 I = MAST,MIDEN
         IA = IA + 1
         A(I) = REAL(IW(IA))
         DO 100 IX = 1,ISCT3
            IY = IX3(IX)
            IF (IA.EQ.IY) A(I) = A(I) + 0.5
 100     CONTINUE
         DO 110 IX = 1,ISCT4
            IY = IX4(IX)
            IF (IA.EQ.IY) A(I) = A(I) + 0.5
 110     CONTINUE
 120  CONTINUE
      IA = 0
      MIDEP = MIDEN + 1
      DO 130 I = MIDEP,MLAMDA
         IA = IA + 1
         A(I) = REAL(IN(IA))
 130  CONTINUE
      KDIC = 1
      ICOU = MTRANS
!
! CALL OTHER ROUTINES
!
      CALL ENDFP
!
! READ IN COLLAPSED DATA. (NCROSS REACTIONS AND NGROUP X-SECTIONS).
!
 132  CONTINUE
      IF (WTYPE.EQ.2) GOTO 154
!  ## THIS METHOD OF READING COLLAPX WORKS ONLY IF NGROUP = 1.
!  ## HARWELL NOV 1990.
      READ(24,99020) NCROSS,CLIDNT
      DO 150 K = 1,NCROSS
         READ(24,99021) NG,N2,BARNS
         A(NSTART) = REAL(NG)
         NSTART = NSTART + 1
         A(NSTART) = REAL(N2)
         NSTART = NSTART + 1
         A(NSTART) = BARNS*1.E-24
         NSTART = NSTART + 1
 150  CONTINUE
      READ(24,99022,END=152) NUMBEG
 152  CONTINUE
 154  CONTINUE
!     
! WRITE "LISTAG", OTHER COMMONS AND "A" ARRAY TO DISK. (UNFORMATTED).
!
!  ## HARWELL MODIFICATION DECEMBER 1990.
!  #  GET CURRENT DATE
      CALL CLOKK(DUMMY,DUMTIM,TEMP)
      LIBDAT = CLIDNT//TEMP(1:2)//TEMP(4:5)//TEMP(7:8)//VSIDNT//DDIDNT
!  ## HARWELL MODIFICATION JANUARY 1991.
      FLUX(2) = 1.0
      KNISOT = 0
      CALL CNVNT(WREGN,NUMRGN)
!+**PJK 17/11/97
      IF (IVMS1.EQ.1) THEN
         TEMP = 'WARRY'//NUMRGN//'.DAT'
         OPEN(25,FILE=TEMP,STATUS='UNKNOWN',ACCESS='SEQUENTIAL', &
              FORM='UNFORMATTED')
      ELSE
         TEMP = 'fispact/WARRY'//NUMRGN//'.DAT'
         OPEN(25,FILE=TEMP,STATUS='UNKNOWN',ACCESS='SEQUENTIAL', &
              FORM='UNFORMATTED')
      END IF
      WRITE(25) LIBDAT
      WRITE(25) NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN, &
           MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS
      WRITE(25) (IX3(K),K=1,70)
      WRITE(25) (IX4(K),K=1,10)
      WRITE(25) (FLUX(K),K=1,3),MIND
      WRITE(25) (BB(K),K=1,N)
      WRITE(25) ICOU,(IN(K),K=1,N)
      WRITE(25) (IW(K),K=1,N),KDIC
      KS = 1
      DO 160 KAR = 1,9
         KE = KS + 1999
         WRITE(25) (A(K),K=KS,KE)
         KS = KE + 1
 160  CONTINUE

      GOTO 99999
!  ## Changes so that files closed and opened in sequence
!  ## 6/9/91
 1000 CONTINUE
      INQUIRE(23,NAME=FILENM)
      CLOSE(23)
      LENGTH = LENGT(FILENM)
      NUMCHR = FILENM(LENGTH:LENGTH)
! ## To increase portability 48 -> ICHAR('0') 13/1/93
      NUMBR = ICHAR(NUMCHR)-ICHAR('0')
      NUMBR = NUMBR+1
      TEMPFN = FILENM(1:LENGTH)
!  ## Changes made because there are 10 decay files for EAF-3.
!  ## 18/9/92
      IF (NUMBR.EQ.10) THEN
         FILENM = TEMPFN(1:LENGTH-2)//'10'
      ELSE
! ## To increase portability 48 -> ICHAR('0') 13/1/93
         FILENM = TEMPFN(1:LENGTH-1)//CHAR(ICHAR('0')+NUMBR)
      END IF
      OPEN(23,FILE=FILENM,STATUS='UNKNOWN')
      GOTO 20
!  ## End of modification
99001 FORMAT(20A4)
99002 FORMAT(I6,1X,A15)
99003 FORMAT(2E11.5,4I11,I4,I2,I3,I5)
99006 FORMAT(33X,I11,11X,I11)
99007 FORMAT(//41X,'SUB-LIBRARY FLAGGED AS OTHER THAN DECAY DATA', &
           ' - PROGRAM HALTED - ZA is ',1PE11.5/37X,'<<',85('='), &
           '>>')
99008 FORMAT(44X,2I11)
99009 FORMAT(1X)
99010 FORMAT(55X,I11)
99011 FORMAT(E11.5)
99012 FORMAT(22X,I11,22X,I11)
99013 FORMAT(44X,I11)
99014 FORMAT(44X,2I11)
99015 FORMAT(6I11)
99016 FORMAT(6E11.5)
99017 FORMAT(66X,I4)
99018 FORMAT(16X,I6)
99019 FORMAT(' NUCL(',I4,') = ',I6,' ZA = ',I5,' ISOMER = ',I2)
99020 FORMAT(1X,I5,2X,A47)
99021 FORMAT(1X,I4,2X,I4,4X,1PE12.5)
99022 FORMAT(1X,I3)

99999 CONTINUE

      CLOSE(21)
      CLOSE(23)
      CLOSE(24)
      CLOSE(25)

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE IDNTFY(NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS, &
           MIDEN,MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS, &
           MCROSS,NYIELD)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Sets up the pointers for the A() array.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  See comments for LISTAG common in routine FISPRO
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      INTEGER MASS,MCROSS,MFRAC,MIDEN,MLAMDA,MSPEC,MTOTAL,MTRANS, &
           MYIELD,MYILDS,N,NCH,NCROSS,NISOT,NSPEC,NSTART,NTRANS,NYIELD

!  Global variables
      REAL A(18000)
      COMMON/BLOCK /A

!  Local variables
      INTEGER NGROUP

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NYIELD = 1190
      NSTART = NSTART+1
      NCH    = INT(A(NSTART))
      NSTART = NSTART+1
      NGROUP = INT(A(NSTART))
      NSTART = NSTART+1
      NSPEC  = INT(A(NSTART))
      NSTART = NSTART+18*NSPEC
      NSTART = NSTART+1
      NISOT  = INT(A(NSTART))
      MTOTAL = NSTART
      NSTART = NSTART+NSPEC
      MFRAC  = NSTART
      NSTART = NSTART+NISOT
      NSTART = NSTART+1
      N      = INT(A(NSTART))
      MASS   = NSTART
      NSTART = NSTART+N
      MIDEN  = NSTART
      NSTART = NSTART+N
      MLAMDA = NSTART
      NSTART = NSTART+N
      MSPEC  = NSTART
      NSTART = NSTART+N*NSPEC
      MYILDS = NSTART
      NSTART = NSTART+NYIELD*NISOT
      MYIELD = NSTART
      NSTART = NSTART+N
      NSTART = NSTART+1
      NTRANS = INT(A(NSTART))
      MTRANS = NSTART
      NSTART = NSTART+3*NTRANS
      NSTART = NSTART+1
      NCROSS = INT(A(NSTART))
      MCROSS = NSTART
      NSTART = NSTART+(NGROUP+2)*NCROSS

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE INITVR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Initialisation routine.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  28 February 1994
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  28/02/94 PJK 1.010 New coding for WTYPE=5 option
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL A(18000)
      COMMON/BLOCK /A

      INTEGER NSTEPS
      REAL GMS,YSTEPS,ZLEVEL
      COMMON /CALIST/ GMS,NSTEPS,YSTEPS,ZLEVEL

      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA, &
           MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN, &
           MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      REAL FLUX2,T
      COMMON /OUT1  / FLUX2,T

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WLVL2,WJSPEK,WMAXT
      REAL WLVL1,WCONV
      COMMON /PRPRM0/ WLVL1,WLVL2,WJSPEK,WCONV,WMAXT

      DOUBLE PRECISION WMIND
      COMMON /PRPRM1/ WMIND

      CHARACTER*22 WSPID
      COMMON /PROSTR/ WSPID

      INTEGER JSPEK
      COMMON /SPKGN1/ JSPEK


!  Local variables
      INTEGER I,J

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (WTYPE.EQ.5) GOTO 60

      DO 10 I = 1,10
         IX4(I) = 0
 10   CONTINUE

      DO 20 I = 1,70
         IX3(I) = 0
 20   CONTINUE

! *** Version number of FISPRO, eg. 2.0/10 = 2010

      VSIDNT = '1001'

      NCH = 0
      N = 0

      DO 30 J = 1,3
         FLUX(J) = 0.0
 30   CONTINUE
      DO 40 J = 1,364
         B(J) = 0.0D0
 40   CONTINUE
      DO 50 J = 1,18000
         A(J) = 0.0
 50   CONTINUE

      FLUX(1) = 1.0
      MCROSS = 0
      NCROSS = 0
      NSTART = 0
      NSPEC  = 0
      NISOT  = 0
      MTOTAL = 0
      MFRAC  = 0
      MASS   = 0
      MIDEN  = 0
      MLAMDA = 0
      MSPEC  = 0
      MYIELD = 0
      MYILDS = 0
      NTRANS = 0
      MTRANS = 0
      NYIELD = 1190
      ZLEVEL = 0.0

 60   CONTINUE

! *** Set parameters from PRPRM0 and PRPRM1 commons

      MIND   = WMIND
      ZLEVEL = WLVL1
      NSTEPS = WLVL2
      YSTEPS = REAL(NSTEPS)
      JSPEK  = WJSPEK
      T      = WTIME
      FLUX2  = WFLUX
      MAXXT  = WMAXT
      CONVV  = WCONV
      SPIDNT = WSPID

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE INTEGS(M, FLUXT, YTOTAL, TIME)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Integration routine, used if INTEGT unsuccessful
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  M       Number of nuclides not in equilibrium
!  FLUXT   Total flux (n/cm2/s)
!  YTOTAL
!  TIME
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      INTEGER M
      REAL FLUXT,YTOTAL,TIME

!  Global variables
      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      INTEGER I22,I31,I32
      REAL BR1(1000)
      COMMON /EXT   / I22,I31,I32,BR1

      INTEGER NP1(1000),ND1(1000),I2,I1,N
      REAL TR1(1000),YLAM(364)
      COMMON /TRANS0/ NP1,ND1,TR1,I2,I1,N,YLAM

      DOUBLE PRECISION Y(364)
      COMMON /TRANS1/ Y

!  Local variables
      DOUBLE PRECISION A1,B1,D1,F1,R,O,Q,S(364),F,ERRCOL
      DOUBLE PRECISION C1(364),E1(364),EXTRAP(364,10),G1(364),SX(364), &
           XINTER(364),YINTER(364),ZINTER(364)

      INTEGER I,I23,I4,IA,IB,IC,ICRI,ID,IE,IND,INUM,IQ,IQP,IQQ,IR,J,J1, &
           JUMP,JYY,K,K1,L,M1,MAXEXT,ND,NDE,NDQ,NP,NPQ,NPX,NQX,NSTEPS

      REAL STEPLE,EPSB,TMAX,H,BSY,BRX,BRY,BSX,D2,B,FLODIV,ERS,CRIT

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!+**PJK 26/11/93
      IA = 0
      STEPLE = 0.0
      NSTEPS = 0
      DO 2 J = 1,10
         DO 1 I = 1,364
            EXTRAP(I,J) = 0.0D0
 1       CONTINUE
 2    CONTINUE

      DO 5 I = 1,N
         S(I) = 0.0D0
 5    CONTINUE
      EPSB = CONVV
      MAXEXT = MAXXT
      TMAX = TIME
      H = TIME
      NDE = M
      M1 = M + 1
      I4 = I2 + 1
      JYY = 0
      INUM = 0
      O = 0.0D0
      ERRCOL = 0.0D0
!
! CORRECTION FOR EQUILIBRIUM ISOTOPES LOSS
!
      IF (M.EQ.N) GOTO 280
      I23 = I22 + 1
      NPX = 0
      BRX = 0.0
      IF (I23.GE.I32) GOTO 30
      DO 20 IQ=I23,I32
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         ND = ND1(IQ)
         IF (ND.EQ.0 .OR. NP.EQ.0) GOTO 20
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
         IF (IQ.EQ.I23) GOTO 20
         IQQ = IQ - 1
         NQX = 0
         BSX = 0.0
         DO 10 IQP=I23,IQQ
            NPQ = NP1(IQP)
            IF (NPQ.EQ.NQX) BSX = BSX + BR1(IQP)
            NDQ = ND1(IQP)
            IF (NPQ.NE.ND) GOTO 10
            BSY = 1.0 - BSX
            IF (BSY.LT.1.0E-6) Y(ND) = 0.0D0
            Y(NDQ) = Y(NDQ) + Y(NP)*DBLE(BR1(IQP))
 10      CONTINUE
 20   CONTINUE
 30   CONTINUE
      NPX = 0
      BRX = 0.0
      IF (I31.EQ.0) GOTO 60
      DO 50 IQ=1,I31
         IF (FLUXT.GT.0.0 .AND. TIME.GT.0.0) GOTO 50
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         DO 40 IR=1,M
            IF (NP.EQ.IR) GOTO 50
 40      CONTINUE
         ND = ND1(IQ)
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
 50   CONTINUE
 60   CONTINUE
!
! SET INITIAL VALUES INTO XINTER.
!
      GOTO 280
!
! MODULE ERRCOL
! COMPUTES FRACTIONAL ERROR IN IATH COLUMN OF EXTRAPOLATION ARRAY.
! A1 IS SET TO MAXIMUM ERROR OBTAINED FROM DIFFERENCE OF ADJACENT
! COLUMNS OF EXTRAPOLATION ARRAY.
!
 70   CONTINUE
      A1 = 0.0D0
      J1 = IA - 1
      DO 90 K1=1,NDE
         B1 = EXTRAP(K1,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 80
         A1 = MAX(A1,ABS(B1-EXTRAP(K1,J1))*1.0D12)
         GOTO 90
 80      CONTINUE
         A1 = MAX(A1,ABS(1.0D0-EXTRAP(K1,J1)/B1))
 90   CONTINUE
!
! FRACTIONAL ERROR = ERROR FOUND / MAXIMUM ERROR.
!
      ERRCOL = A1/DBLE(EPSB)
      GOTO JUMP, (350, 500)
!
! MODULE PREDL
!
! A1 = CONVERGENCE CRITERION FOR SERIES FOR EXPONENTIAL FACTOR.
!
 100  CONTINUE
      A1 = 1.0D-06*DBLE(STEPLE)
      IF (M.EQ.N) GOTO 120
      DO 110 I=M1,N
         B1 = DBLE(YLAM(I))
         C1(I) = 1.0D0/B1
 110  CONTINUE
 120  CONTINUE
      DO 150 I=1,NDE
!
! SET INTERVAL VALUES INTO XINTER.
!
         XINTER(I) = Y(I)
!
! C(I) IS DIAGONAL ELEMENT.
!
         B1 = DBLE(-YLAM(I))
!
! D=LAM*H FOR ONE-STEP METHOD.
!
         D1 = B1*DBLE(STEPLE)
         D2 = REAL(ABS(D1))
         IF (D2.LT.150.0) G1(I) = EXP(D1)
         IF (D2.GE.150.0) G1(I) = 0.0D0
!
! IF LAM*H IS SMALL, THEN COMPUTE EXPONENTIAL FACTORS BY SERIES
! SERIES E1=SUM(LAM(I-1)*H(I)).
! AT 322, OTHERWISE E1=EXP(LAM*H).
!
         IF (ABS(D1).LT.0.1D0) GOTO 130
         IF (D2.LT.150.0) E1(I) = (EXP(D1)-1.0D0)/B1
         IF (D2.GE.150.0) E1(I) = (-1.0D0)/B1
         GOTO 150
 130     CONTINUE
         B1 = DBLE(STEPLE)
         F1 = B1
!
! J1 COUNTS TERMS, F1 IS LATEST TERM, B1 IS SUM OF TERMS.
!
         J1 = 1
 140     CONTINUE
         J1 = J1 + 1
         F1 = D1*F1/DBLE(J1)
         B1 = B1 + F1
         IF (A1.LT.ABS(F1)) GOTO 140
!
! SET RESULT INTO E1, FINISHING WHEN MOD.F1 IS LESS THAN A1.
!
         E1(I) = B1
 150  CONTINUE
!
! I COUNTS STEPS OF ONE-STEP METHOD.
!
      I = 0
 160  CONTINUE
      I = I + 1
!
! EVALUATE R.H.S. OF DN/DT=AN=DX.
! NDE IS NUMBER OF BURNABLE NUCLIDES IN PROBLEM (LESS THAN 101)
! NJ1,NJ2 ARE TABLES OF NUCLIDES PRODUCED BY CAPTURE AND DECAY.
! X IS THE NUMBER DENSITY TABLE.
! NNF IS THE NUMBER OF FISSILE NUCLIDES.
! NNFP IS THE NUMBER OF FISSION PRODUCTS IN PROBLEM.
!
      IF (M.EQ.N) GOTO 220
      DO 170 IQ=M1,N
         XINTER(IQ) = S(IQ)*C1(IQ)
 170  CONTINUE
      IF (I.GT.1) GOTO 190
      DO 180 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 180  CONTINUE
 190  CONTINUE
      IF (I2.EQ.I1) GOTO 220
      DO 200 IQ=I4,I1
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(IQ))*YINTER(NP)*C1(ND)
 200  CONTINUE
      DO 210 IQ=M1,N
         YINTER(IQ) = XINTER(IQ)
 210  CONTINUE
 220  CONTINUE
      DO 230 IQ=1,NDE
         SX(IQ) = S(IQ)
 230  CONTINUE
      DO 240 IQ=1,I2
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (XINTER(NP).LT.1.D-50) XINTER(NP)=0.0D0
         SX(ND) = SX(ND) + DBLE(TR1(IQ))*XINTER(NP)
 240  CONTINUE
      JYY = JYY + 1
      DO 250 K1=1,NDE
         IF (XINTER(K1).LT.1.D-50) XINTER(K1)=0.0D0
         XINTER(K1) = XINTER(K1)*G1(K1) + SX(K1)*E1(K1)
 250  CONTINUE
      DO 260 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 260  CONTINUE
!
! ONE-STEP METHOD:  X=X+(EXP(LAM*H)-1.0)/LAM*DX/DT.
! TAKEN OVER 'NSTEPS' STEPS OF LENGTH 'STEPLE'.
!
      IF (I.NE.NSTEPS) GOTO 160
      GOTO JUMP, (310, 380)
 270  FORMAT(' --STEP LENGTH HAS BECOME NEGLIGIBLY SMALL--')
!
! IND - STEP REDUCTION FACTOR.
!
 280  CONTINUE
      IND = 1
!
! B - CONFIDENCE FACTOR FOR STEP CHANGING,
! IF B=1.0 FULL CONFIDENCE,  IF B=0 NO CONFIDENCE.
!
      B = 1.0
!
! K IS NUMBER OF STEPS TO COVER INTERVAL TMAX.
!
      K = INT(MAX(1.1,TMAX/H+0.1))
!
! H - STEP LENGTH OF EXTRAPOLATION PROCEDURE.
!
      H = TMAX/REAL(K)
!
! L - COLUMN ON WHICH TO TEST CONVERGENCE, IF L=0 NO TEST.
!
      L = 0
!
! DECREASE H BY STEP-LENGTH REDUCTION FACTOR (IND).
!
 290  CONTINUE
      H = H/REAL(IND)
!
! INCREASE NUMBER OF STEPS (K) CORRESPONDINGLY.
!
      K = IND*K
!
! RESET REDUCTION FACTOR TO UNITY.
!
      IND = 1
!
! TEST IF H IS TOO SMALL, I.E. SOMETHING GONE WRONG.
!
      IF (H.GT.TMAX*0.1E-4) GOTO 300
      WRITE(*,*) 'Error in routine INTEGS:'
      WRITE(*,*) 'Step length has become negligibly small, H = ',H
      WRITE(*,*) 'PROCESS stopping.'
      STOP
!
! STEPLE - ONE-STEP METHOD STEP LENGTH
! STEPLE SET FIRSTLY TO EXTRAPOLATION STEP (H)
!
 300  CONTINUE
      STEPLE = H
!
! NSTEPS - NUMBER OF STEPS OF LENGTH STEPLE TO COVER EXTRAPOLATION
! STEP (H).
!
      NSTEPS = 1
!
! INTEGRATE EQUATIONS BY 1 STEP OF ONE-STEP METHOD.
!
      ASSIGN 310 TO JUMP
      GOTO 100
 310  CONTINUE
      DO 320 IQ=1,NDE
!
! STORE INITIAL  RESULTS  INTO  EXTRAPOLATION ARRAY
!
         EXTRAP(IQ,1) = XINTER(IQ)
 320  CONTINUE
!
! IA  COUNTS  COLUMNS  OF  EXTRAPOLATION  ARRAY
!
      IA = 1
!
! IF IA=MAXEXT WE HAVE USED STORAGE FOR EXTRAPOLATION    U
! WITHOUT CONVERGENCE.
!
 330  CONTINUE
      IF (MAXEXT.GT.IA) GOTO 370
!
! IB - NUMBER OF DERIVATIVE EVALUATIONS TO OBTAIN CONVERGENCE  IN
! 1 EXTRAPOLATION.
!
      IB = 2
!
! F - WILL CONTAIN BEST ESTIMATE OF FUNCTION EVALUATIONS PER UNIT
! STEP USING VARIOUS ORDERS OF EXTRAPOLATION.
!
      F = 1.0D+37
!
! IA COUNTS THROUGH COLUMNS OF EXTRAPOLATION ARRAY.
! NSTEPS IS NUMBER OF STEPS REQUIRED TO COVER INTERVAL USING IATH
! ORDER METHOD.
!
      IA = 1
 340  CONTINUE
      IA = IA + 1
      NSTEPS = NSTEPS/2
      ASSIGN 350 TO JUMP
      GOTO 70
!
! COMPUTE FRACTIONAL ERROR IN IATH COLUMN.
!
 350  CONTINUE
      O = ERRCOL
!
! IF THIS COLUMN HAS CONVERGED ALREADY GOTO 45.
!
      IF (O.LE.1.0D+00) GOTO 510
!
! INCREASE NUMBER OF DERIVATIVE EVALUATIONS AS ORDER INCREASES.
!
      IB = IB + IB
!
! STEPLE IS ESTIMATED STEP LENGTH TO PRODUCE CONVERGENCE IN THIS
! COLUMN.
!
      STEPLE = H*REAL(O)**(1.0/REAL(1-IA))/REAL(NSTEPS)
!
! Q IS FUNCTION EVALUATIONS PER UNIT STEP.
!
      Q = DBLE(IB)/DBLE(STEPLE)
      IF (Q.GT.F) GOTO 360
!
! STORE BEST FUNCTION EVALUATIONS PER UNIT STEP.
!
      F = Q
!
! SET L TO COLUMN FOR LATER TEST.
!
      L = IA
!
! R IS STEP LENGTH REQUIRED FOR ORDER.
!
      R = DBLE(STEPLE)
 360  CONTINUE
      IF (IA.NE.MAXEXT) GOTO 340
!
! SET MAXEXT TO BEST ORDER EXTRAPOLATION.
!
      MAXEXT = L
!
! SET STEP REDUCTION FACTOR TO REDUCE STEP LENGTH,
! MODIFIED BY CONFIDENCE FACTOR B.
! RESUME AT 16.
!
      FLODIV = H/(B*REAL(R)) + 1.0
      IND = INT(FLODIV)
      GOTO 290
!
! DOUBLE NUMBER OF STEPS OF ONE-STEP METHOD.
!
 370  CONTINUE
      NSTEPS = NSTEPS + NSTEPS
!
! DECREASE STEP LENGTH.
!
      STEPLE = 0.5*STEPLE
      ASSIGN 380 TO JUMP
!
! INTEGRATE EQUATIONS WITH 'NSTEPS' STEPS OF SIZE 'STEPLE'.
!
      GOTO 100
!
! IB - CONVERGENCE TEST INDICATOR.
!
 380  CONTINUE
      IB = 1
!
! IC - NEXT COLUMN OF EXTRAPOLATION.
!
      IC = IA + 1
      IF (MAXXT.LT.6) GOTO 385
      IF (IC.LE.5) GOTO 430
!
! O - MAXIMUM ERROR.
!
 385  CONTINUE
      O = 0.0D+00
      DO 420 ID=1,NDE
!
! F - EXTRAPOLATION FACTOR.
!
         F = 1.0D0
!
! Q - LATEST ONE-STEP ESTIMATE.
!
         Q = XINTER(ID)
         DO 390 IE=1,IA
!
! F DOUBLES TO BECOME 2.0 TO THE POWER (R+1.0).
!
            F = F + F
!
! R - OLD VALUE IN THIS COLUMN OF EXTRAPOLATION OVERWRITTEN BY Q.
!
            R = EXTRAP(ID,IE)
            EXTRAP(ID,IE) = Q
!
! Q UPDATED TO NEXT COLUMN ESTIMATE,
! Q = (2.0**(R+1.0)*(NEW-OLD))/(2.0**(R+1)-1.0).
!
            Q = (F*Q-R)/(F-1.0D0)
 390     CONTINUE
         EXTRAP(ID,IC) = Q
!
! IF CONVERGENCE TESTS ALREADY FAILED TEST NO FURTHER,
! ELSE O IS SET TO MAXIMUM ERROR FOUND AND TESTED AGAINST MAX. ERROR
! IF (IB.LT.0)GOTO 310
!
         B1 = EXTRAP(ID,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 400
         O = MAX(O,ABS(B1-R)*1.0D+4)

!+**PJK 29/11/93 Arithmetic-IF replaced
!         IF (DBLE(EPSB)-O) 410, 420, 420
         IF (DBLE(EPSB).LT.O) THEN
            GOTO 410
         ELSE
            GOTO 420
         END IF

 400     CONTINUE
         ERS = ABS(1.0E+00-REAL(R/B1))
         IF (DBLE(ERS).LT.O) GOTO 420
         O = DBLE(ERS)

!+**PJK 29/11/93 Arithmetic-IF replaced
!         IF (DBLE(EPSB)-O) 410, 410, 420
         IF (DBLE(EPSB).LE.O) THEN
            GOTO 410
         ELSE
            GOTO 420
         END IF

!
! IB=-1: CONVERGENCE TESTS FAILED.
!
 410     CONTINUE
         IB = -1
         IF (IC.EQ.MAXEXT) EXTRAP(ID,IC) = XINTER(ID)
 420  CONTINUE
!
! IA IS NEW COLUMN, I.E. LAST STORED.
!
      GOTO 460
 430  CONTINUE
      DO 450 ID=1,NDE
         DO 440 IE=1,IA
            EXTRAP(ID,IE) = XINTER(ID)
 440     CONTINUE
         EXTRAP(ID,IC) = XINTER(ID)
 450  CONTINUE
      IB = -1
      IF (IC.EQ.MAXEXT) EXTRAP(ID,IC) = XINTER(ID)
 460  CONTINUE
      IA = IC
!
! O BECOMES FRACTIONAL ERROR.
!
      O = O/DBLE(EPSB)
!
! IF CONVERGENCE PASSED GOTO 340.
!
      IF (IB.GE.0 .AND. K.EQ.1) THEN
         IF (IA.NE.MAXXT) WRITE(27,*) 'Number of iterations = ',IA
      END IF
 470  FORMAT('NUMBER OF ITERATIONS',I6)
      IF (IB.GE.0) GOTO 520
      IF (IB.LT.0 .AND. IA.EQ.MAXEXT) WRITE(27,480)
 480  FORMAT('   CASE NOT PROPERLY CONVERGED, but if no "?" flags set', &
           ' then convergence achieved for ALL printed isotopes.')
      IF (IB.LT.0 .AND. IA.EQ.MAXEXT) GOTO 520
!
! IF L IS SET TO COMPARE THIS COLUMN AND NO CONVERGENCE HAS BEEN
! OBTAINED, COMPUTE FRACTIONAL ERROR.
!
      IF (IA.NE.L) GOTO 330
      IF (INUM.EQ.0) WRITE(27,490)
 490  FORMAT('0','CONVERGENCE NOT ACHIEVED BY FIRST ROUTE-TRY SECOND')
      INUM = 1
      ASSIGN 500 TO JUMP
      GOTO 70
!
! REDUCE CONFIDENCE FACTOR.
!
 500  CONTINUE
      B = B*REAL(ERRCOL)**(1.0/REAL(1-IA))
!
! RETURN TO 20 FOR NEXT ITERATION.
!
      GOTO 330
!
! TOTAL CONVERGENCE FAILURE, BUT CONVERGENCE IN EARLY COLUMN,
! SET MAXEXT TO THIS COLUMN.
!
 510  CONTINUE
      MAXEXT = IA
!
! REDUCE STEP LENGTH.
!
      H = H/REAL(NSTEPS)
!
! INCREASE NUMBER OF STEPS TO GO.
!
      K = 1 + NSTEPS*(K-1)
 520  CONTINUE
      DO 530 L=1,NDE
!
! *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
!
         IF (EXTRAP(L,IA).LT.MIND) EXTRAP(L,IA) = 0.0D0
         XINTER(L) = EXTRAP(L,IA)
         YINTER(L) = XINTER(L)
         Y(L) = EXTRAP(L,IA)
 530  CONTINUE
      IF (M.EQ.N) GOTO 600
 540  CONTINUE
      DO 550 L=M1,N
         ZINTER(L) = YINTER(L)
         XINTER(L) = S(L)*C1(L)
 550  CONTINUE
      IF (I2.EQ.I1) GOTO 570
      DO 560 L=I4,I1
         NP = NP1(L)
         ND = ND1(L)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(L))*YINTER(NP)*C1(ND)
 560  CONTINUE
 570  CONTINUE
      DO 580 L=M1,N
!
! *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
!
         IF (XINTER(L).LT.MIND) XINTER(L) = 0.0D0
         YINTER(L) = XINTER(L)
         Y(L) = XINTER(L)
 580  CONTINUE
      ICRI = 0
      DO 590 L=M1,N
         CRIT = REAL(ZINTER(L) - YINTER(L))
         IF (YINTER(L).LT.1.0D-5) GOTO 590
         CRIT = CRIT/REAL(YINTER(L))
         IF (ABS(CRIT).GT.EPSB) ICRI = 1
 590  CONTINUE
      IF (ICRI.GT.0) GOTO 540
 600  CONTINUE
!
! DECREASE NUMBER OF STEPS.
!
      K = K - 1
!
! SET COLUMN INDICATOR TO ZERO FOR NO TEST.
!
      L = 0
!
! IF ONLY ONE STEP LEFT GOTO 18 TO COMPLETE INTERVAL.
! IF MORE THAN ONE STEP LEFT, F IS DISTANCE TO GO.
!
!+**PJK 29/11/93 Arithmetic-IF replaced
!      IF (1-K) 610, 300, 620
      IF (K.GT.1) THEN
         GOTO 610
      ELSE IF (K.EQ.1) THEN
         GOTO 300
      ELSE
         GOTO 620
      END IF

 610  CONTINUE
      F = DBLE(H*REAL(K))
!
! L IS SET TO ORDER OF EXTRAPOLATION.
!
      L = IA
      FLODIV = REAL(F)/(H*(1.0+B*(REAL(O)**(1.0/REAL(1-IA))-1.0))) + 0.9
      K = INT(FLODIV)
!
! STEP LENGTH IS INCREASED BY CONFIDENCE FACTOR TIMES ESTIMATED
! FULL INCREASE TO PRODUCE CONVERGENCE IN IATH COLUMN,
!
      H = REAL(F)/REAL(K)
!
! RETURN TO 18 FOR NEXT STEP.
!
      GOTO 300
!
! END OF INTERVAL SO RETURN.
!
 620  CONTINUE

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE INTEGT(M, FLUXT, YTOTAL, TIME)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Integration routine, used for the inventory calculations.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  M
!  FLUXT
!  YTOTAL
!  TIME
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      INTEGER M
      REAL FLUXT,YTOTAL,TIME

!  Global variables
      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      INTEGER I22,I31,I32
      REAL BR1(1000)
      COMMON /EXT   / I22,I31,I32,BR1

      INTEGER NP1(1000),ND1(1000),I2,I1,N
      REAL TR1(1000),YLAM(364)
      COMMON /TRANS0/ NP1,ND1,TR1,I2,I1,N,YLAM

      DOUBLE PRECISION Y(364)
      COMMON /TRANS1/ Y

!  Local variables
      DOUBLE PRECISION A1,B1,D1,F1,R,O,Q,S(364),YY(364),F,ERRCOL
      DOUBLE PRECISION C1(364),E1(364),EXTRAP(364,10),G1(364),SX(364), &
           XINTER(364),YINTER(364),ZINTER(364)

      INTEGER I,I23,I4,IA,IB,IC,ICRI,ID,IE,IND,INUM,IQ,IQP,IQQ,IR,IXY, &
           J,J1,JUMP,JYY,K,K1,L,M1,MAXEXT,ND,NDE,NDQ,NP,NPQ,NPX,NQX, &
           NSTEPS

      REAL B,BRX,BRY,BSX,BSY,CRIT,D2,EPSB,FLODIV,H,STEPLE,TMAX

!  External routines
      EXTERNAL INTEGS

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!+**PJK 29/11/93
      IA = 0
      NSTEPS = 0
      STEPLE = 0.0
      DO 2 J = 1,10
         DO 1 I = 1,364
            EXTRAP(I,J) = 0.0D0
 1       CONTINUE
 2    CONTINUE

      DO 5 I = 1,N
         S(I) = 0.0D0
 5    CONTINUE
      EPSB = CONVV
      MAXEXT = MAXXT
      TMAX = TIME
      H = TIME
      NDE = M
      M1 = M + 1
      I4 = I2 + 1
      JYY = 0
      INUM = 0
      DO 10 I=1,N
         YY(I) = Y(I)
 10   CONTINUE
!
! CORRECTION FOR EQUILIBRIUM ISOTOPES LOSS
!
      IF (M.EQ.N) GOTO 290
      I23 = I22 + 1
      NPX = 0
      IF (I23.GE.I32) GOTO 40
      BRX = 0.0
      DO 30 IQ=I23,I32
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         ND = ND1(IQ)
         IF (ND.EQ.0 .OR. NP.EQ.0) GOTO 30
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
         IF (IQ.EQ.I23) GOTO 30
         IQQ = IQ - 1
         NQX = 0
         BSX = 0.0
         DO 20 IQP=I23,IQQ
            NPQ = NP1(IQP)
            IF (NPQ.EQ.NQX) BSX = BSX + BR1(IQP)
            NDQ = ND1(IQP)
            IF (NPQ.NE.ND) GOTO 20
            BSY = 1.0 - BSX
            IF (BSY.LT.1.0E-6) Y(ND) = 0.0D0
            Y(NDQ) = Y(NDQ) + Y(NP)*DBLE(BR1(IQP))
 20      CONTINUE
 30   CONTINUE
 40   CONTINUE
      NPX = 0
      BRX = 0.0
      IF (I31.EQ.0) GOTO 70
      DO 60 IQ=1,I31
         IF (FLUXT.GT.0.0 .AND. TIME.GT.0.0) GOTO 60
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         DO 50 IR=1,M
            IF (NP.EQ.IR) GOTO 60
 50      CONTINUE
         ND = ND1(IQ)
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
 60   CONTINUE
 70   CONTINUE
!
! SET INITIAL VALUES INTO XINTER.
!
      GOTO 290
!
! MODULE ERRCOL
! COMPUTES FRACTIONAL ERROR IN IATH COLUMN OF EXTRAPOLATION ARRAY.
! A1 IS SET TO MAXIMUM ERROR OBTAINED FROM DIFFERENCE OF ADJACENT
! COLUMNS OF EXTRAPOLATION ARRAY.
!
 80   CONTINUE
      A1 = 0.0D0
      J1 = IA - 1
      DO 100 K1=1,NDE
         B1 = EXTRAP(K1,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 90
         A1 = MAX(A1,ABS(B1-EXTRAP(K1,J1))*1.0D+12)
         GOTO 100
 90      CONTINUE
         A1 = MAX(A1,ABS(1.0D0-EXTRAP(K1,J1)/B1))
 100  CONTINUE
!
! FRACTIONAL ERROR = ERROR FOUND / MAXIMUM ERROR.
!
      ERRCOL = A1/DBLE(EPSB)
      GOTO JUMP, (370, 470)
!
!
! MODULE PREDL
!
! A1 = CONVERGENCE CRITERION FOR SERIES FOR EXPONENTIAL FACTOR.
!
 110  CONTINUE
      A1 = 1.0D-06*DBLE(STEPLE)
      IF (M.EQ.N) GOTO 130
      DO 120 I=M1,N
         B1 = DBLE(YLAM(I))
         C1(I) = 1.0D0/B1
 120  CONTINUE
 130  CONTINUE
      DO 160 I=1,NDE
!
! SET INTERVAL VALUES INTO XINTER.
!
         XINTER(I) = Y(I)
!
! C(I) IS DIAGONAL ELEMENT.
!
         B1 = DBLE(-YLAM(I))
!
! D=LAM*H FOR ONE-STEP METHOD.
!
         D1 = B1*DBLE(STEPLE)
         D2 = REAL(ABS(D1))
         IF (D2.LT.150.0) G1(I) = EXP(D1)
         IF (D2.GE.150.0) G1(I) = 0.0D0
!
! IF LAM*H IS SMALL, THEN COMPUTE EXPONENTIAL FACTORS BY SERIES
! SERIES E1=SUM(LAM(I-1)*H(I)).
! AT 322, OTHERWISE E1=EXP(LAM*H).
!
         IF (ABS(D1).LT.0.1D00) GOTO 140
         IF (D2.LT.150.0) E1(I) = (EXP(D1)-1.0D0)/B1
         IF (D2.GE.150.0) E1(I) = (-1.0D0)/B1
         GOTO 160
 140     CONTINUE
         B1 = DBLE(STEPLE)
         F1 = B1
!
! J1 COUNTS TERMS, F1 IS LATEST TERM, B1 IS SUM OF TERMS.
!
         J1 = 1
 150     CONTINUE
         J1 = J1 + 1
         F1 = D1*F1/DBLE(J1)
         B1 = B1 + F1
         IF (A1.LT.ABS(F1)) GOTO 150
!
! SET RESULT INTO E1, FINISHING WHEN MOD.F1 IS LESS THAN A1.
!
         E1(I) = B1
 160  CONTINUE
!
! I COUNTS STEPS OF ONE-STEP METHOD.
!
      I = 0
 170  CONTINUE
      I = I + 1
!
! EVALUATE R.H.S. OF DN/DT=AN=DX.
! NDE IS NUMBER OF BURNABLE NUCLIDES IN PROBLEM (LESS THAN 101)
! NJ1,NJ2 ARE TABLES OF NUCLIDES PRODUCED BY CAPTURE AND DECAY.
! X IS THE NUMBER DENSITY TABLE.
! NNF IS THE NUMBER OF FISSILE NUCLIDES.
! NNFP IS THE NUMBER OF FISSION PRODUCTS IN PROBLEM.
!
      IF (M.EQ.N) GOTO 230
      DO 180 IQ=M1,N
         XINTER(IQ) = S(IQ)*C1(IQ)
 180  CONTINUE
      IF (I.GT.1) GOTO 200
      DO 190 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 190  CONTINUE
 200  CONTINUE
      IF (I2.EQ.I1) GOTO 230
      DO 210 IQ=I4,I1
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(IQ))*YINTER(NP)*C1(ND)
 210  CONTINUE
      DO 220 IQ=M1,N
         YINTER(IQ) = XINTER(IQ)
 220  CONTINUE
 230  CONTINUE
      DO 240 IQ=1,NDE
         SX(IQ) = S(IQ)
 240  CONTINUE
      DO 250 IQ=1,I2
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (XINTER(NP).LT.1.D-50) XINTER(NP)=0.0D0
         SX(ND) = SX(ND) + DBLE(TR1(IQ))*XINTER(NP)
 250  CONTINUE
      JYY = JYY + 1
      DO 260 K1=1,NDE
         IF (XINTER(K1).LT.1.D-50) XINTER(K1)=0.0D0
         XINTER(K1) = XINTER(K1)*G1(K1) + SX(K1)*E1(K1)
 260  CONTINUE
      DO 270 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 270  CONTINUE
!
! ONE-STEP METHOD:  X=X+(EXP(LAM*H)-1.0)/LAM*DX/DT.
! TAKEN OVER 'NSTEPS' STEPS OF LENGTH 'STEPLE'.
!
      IF (I.NE.NSTEPS) GOTO 170
      GOTO JUMP, (320, 400)
 280  FORMAT(' --STEP LENGTH HAS BECOME NEGLIGIBLY SMALL--')
!
! IND - STEP REDUCTION FACTOR.
!
 290  CONTINUE
      IND = 1
!
! B - CONFIDENCE FACTOR FOR STEP CHANGING,
! IF B=1.0 FULL CONFIDENCE,  IF B=0 NO CONFIDENCE.
!
      B = 1.0
!
! K IS NUMBER OF STEPS TO COVER INTERVAL TMAX.
!
      K = INT(MAX(1.1,TMAX/H+0.1))
!
! H - STEP LENGTH OF EXTRAPOLATION PROCEDURE.
!
      H = TMAX/REAL(K)
!
! L - COLUMN ON WHICH TO TEST CONVERGENCE, IF L=0 NO TEST.
!
      L = 0
!
! DECREASE H BY STEP-LENGTH REDUCTION FACTOR (IND).
!
 300  CONTINUE
      H = H/REAL(IND)
!
! INCREASE NUMBER OF STEPS (K) CORRESPONDINGLY.
!
      K = IND*K
!
! RESET REDUCTION FACTOR TO UNITY.
!
      IND = 1
!
! TEST IF H IS TOO SMALL, I.E. SOMETHING GONE WRONG.
!
      IF (H.GT.TMAX*0.1E-4) GOTO 310
      WRITE(*,*) 'Error in routine INTEGT:'
      WRITE(*,*) 'Step length has become negligibly small, H = ',H
      WRITE(*,*) 'PROCESS stopping.'
      STOP
!
! STEPLE - ONE-STEP METHOD STEP LENGTH
! STEPLE SET FIRSTLY TO EXTRAPOLATION STEP (H)
!
 310  CONTINUE
      STEPLE = H
!
! NSTEPS - NUMBER OF STEPS OF LENGTH STEPLE TO COVER EXTRAPOLATION
! STEP (H).
!
      NSTEPS = 1
!
! INTEGRATE EQUATIONS BY 1 STEP OF ONE-STEP METHOD.
!
      ASSIGN 320 TO JUMP
      GOTO 110
 320  CONTINUE
      DO 330 IA=1,NDE
!
! STORE INITIAL  RESULTS  INTO  EXTRAPOLATION ARRAY
!
         EXTRAP(IA,1) = XINTER(IA)
 330  CONTINUE
!
! IA  COUNTS  COLUMNS  OF  EXTRAPOLATION  ARRAY
!
      IA = 1
!
! IF IA=MAXEXT WE HAVE USED STORAGE FOR EXTRAPOLATION    U
! WITHOUT CONVERGENCE.
!
 340  CONTINUE
      IF (MAXEXT.GT.IA) GOTO 390
!
! *****  ADDITION APRIL 1983  *****
!
      DO 350 IXY=1,N
!
! *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
!
         IF (YY(IXY).LT.MIND) YY(IXY) = 0.0D0
         Y(IXY) = YY(IXY)
 350  CONTINUE
      CALL INTEGS(M, FLUXT, YTOTAL, TIME)
      IF (N.GT.1) GOTO 590
!
! *********************************
! IB - NUMBER OF DERIVATIVE EVALUATIONS TO OBTAIN CONVERGENCE  IN
! 1 EXTRAPOLATION.
!
      IB = 2
!
! F - WILL CONTAIN BEST ESTIMATE OF FUNCTION EVALUATIONS PER UNIT
! STEP USING VARIOUS ORDERS OF EXTRAPOLATION.
!
      F = 1.0D+37
!
! IA COUNTS THROUGH COLUMNS OF EXRAPOLATION ARRAY.
! NSTEPS IS NUMBER OF STEPS REQUIRED TO COVER INTERVAL USING IATH
! ORDER METHOD.
!
      IA = 1
 360  CONTINUE
      IA = IA + 1
      NSTEPS = NSTEPS/2
      ASSIGN 370 TO JUMP
      GOTO 80
!
! COMPUTE FRACTIONAL ERROR IN IATH COLUMN.
!
 370  CONTINUE
      O = ERRCOL
!
! IF THIS COLUMN HAS CONVERGED ALREADY GOTO 45.
!
      IF (O.LE.1.0D+00) GOTO 480
!
! INCREASE NUMBER OF DERIVATIVE EVALUATIONS AS ORDER INCREASES.
!
      IB = IB + IB
!
! STEPLE IS ESTIMATED STEP LENGTH TO PRODUCE CONVERGENCE IN THIS
! COLUMN.
!
      STEPLE = H*REAL(O)**(1.0/REAL(1-IA))/REAL(NSTEPS)
!
! Q IS FUNCTION EVALUATIONS PER UNIT STEP.
!
      Q = DBLE(IB)/DBLE(STEPLE)
      IF (Q.GT.F) GOTO 380
!
! STORE BEST FUNCTION EVALUATIONS PER UNIT STEP.
!
      F = Q
!
! SET L TO COLUMN FOR LATER TEST.
!
      L = IA
!
! R IS STEP LENGTH REQUIRED FOR ORDER.
!
      R = DBLE(STEPLE)
 380  CONTINUE
      IF (IA.NE.MAXEXT) GOTO 360
!
! SET MAXEXT TO BEST ORDER EXTRAPOLATION.
!
      MAXEXT = L
!
! SET STEP REDUCTION FACTOR TO REDUCE STEP LENGTH,
! MODIFIED BY CONFIDENCE FACTOR B.
! RESUME AT 16.
!
      FLODIV = H/(B*REAL(R)) + 1.0
      IND = INT(FLODIV)
      GOTO 300
!
! DOUBLE NUMBER OF STEPS OF ONE-STEP METHOD.
!
 390  CONTINUE
      NSTEPS = NSTEPS + NSTEPS
 
!
! DECREASE STEP LENGTH.
!
      STEPLE = 0.5*STEPLE
      ASSIGN 400 TO JUMP
!
! INTEGRATE EQUATIONS WITH 'NSTEPS' STEPS OF SIZE 'STEPLE'.
!
      GOTO 110
!
! IB - CONVERGENCE TEST INDICATOR.
!
 400  CONTINUE
      IB = 1
!
! IC - NEXT COLUMN OF EXTRAPOLATION.
!
      IC = IA + 1
!
! O - MAXIMUM ERROR.
!
      O = 0.0D+00
      DO 440 ID=1,NDE
!
! F - EXTRAPOLATION FACTOR.
!
         F = 1.0D0
!
! Q - LATEST ONE-STEP ESTIMATE.
!
         Q = XINTER(ID)
         DO 410 IE=1,IA
!
! F DOUBLES TO BECOME 2.0 TO THE POWER (R+1.0).
!
            F = F + F
!
! R - OLD VALUE IN THIS COLUMN OF EXTRAPOLATION OVERWRITTEN BY Q.
!
            R = EXTRAP(ID,IE)
            EXTRAP(ID,IE) = Q
!
! Q UPDATED TO NEXT COLUMN ESTIMATE,
! Q = (2.0**(R+1.0)*(NEW-OLD))/(2.0**(R+1)-1.0).
!
            Q = (F*Q-R)/(F-1.0D0)
 410     CONTINUE
         EXTRAP(ID,IC) = Q
!
! IF CONVERGENCE TESTS ALREADY FAILED TEST NO FURTHER,
! ELSE O IS SET TO MAXIMUM ERROR FOUND AND TESTED AGAINST MAX. ERROR
!
         IF (IB.LT.0) GOTO 440
         B1 = EXTRAP(ID,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 420
         O = MAX(O,ABS(B1-R)*1.0D+4)
!+**PJK 29/11/93 Arithmetic-IF replaced
!         IF (DBLE(EPSB)-O) 430, 440, 440
         IF (DBLE(EPSB).LT.O) THEN
            GOTO 430
         ELSE
            GOTO 440
         END IF

 420     CONTINUE
         O = MAX(O,ABS(1.0D+00-R/B1))
!+**PJK 29/11/93 Arithmetic-IF replaced
!         IF (DBLE(EPSB)-O) 430, 430, 440
         IF (DBLE(EPSB).LE.O) THEN
            GOTO 430
         ELSE
            GOTO 440
         END IF
!
! IB=-1: CONVERGENCE TESTS FAILED.
!
 430     CONTINUE
         IB = -1
 440  CONTINUE
!
! IA IS NEW COLUMN, I.E. LAST STORED.
!
      IA = IC
!
! O BECOMES FRACTIONAL ERROR.
!
      O = O/DBLE(EPSB)
!
! IF CONVERGENCE PASSED GOTO 46.
!
      IF (IB.GE.0 .AND. K.EQ.1) THEN
         IF (IA.NE.MAXXT) WRITE(27,*) 'Number of iterations = ',IA
      END IF
 450  FORMAT('0','NUMBER OF ITERATIONS',I6)
      IF (IB.GE.0) GOTO 490
!
! IF L IS SET TO COMPARE THIS COLUMN AND NO CONVERGENCE HAS BEEN
! OBTAINED, COMPUTE FRACTIONAL ERROR.
!
      IF (IA.NE.L) GOTO 340
      IF (INUM.EQ.0) WRITE(27,460)
 460  FORMAT('0','CONVERGENCE NOT ACHIEVED BY FIRST ROUTE-NOW SECOND')
      INUM = 1
      ASSIGN 470 TO JUMP
      GOTO 80
!
! REDUCE CONFIDENCE FACTOR.
!
 470  CONTINUE
      B = B*REAL(ERRCOL)**(1.0/REAL(1-IA))
!
! RETURN TO 20 FOR NEXT ITERATION.
!
      GOTO 340
!
! TOTAL CONVERGENCE FAILURE, BUT CONVERGENCE IN EARLY COLUMN,
! SET MAXEXT TO THIS COLUMN.
!
 480  CONTINUE
      MAXEXT = IA
!
! REDUCE STEP LENGTH.
!
      H = H/REAL(NSTEPS)
!
! INCREASE NUMBER OF STEPS TO GO.
!
      K = 1 + NSTEPS*(K-1)
 490  CONTINUE
      DO 500 L=1,NDE
!
! *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
!
         IF (EXTRAP(L,IA).LT.MIND) EXTRAP(L,IA) = 0.0D0
         XINTER(L) = EXTRAP(L,IA)
         YINTER(L) = XINTER(L)
         Y(L) = EXTRAP(L,IA)
 500  CONTINUE
      IF (M.EQ.N) GOTO 570
 510  CONTINUE
      DO 520 L=M1,N
         ZINTER(L) = YINTER(L)
         XINTER(L) = S(L)*C1(L)
 520  CONTINUE
      IF (I2.EQ.I1) GOTO 540
      DO 530 L=I4,I1
         NP = NP1(L)
         ND = ND1(L)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(L))*YINTER(NP)*C1(ND)
 530  CONTINUE
 540  CONTINUE
      DO 550 L=M1,N
!
! *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
!
         IF (XINTER(L).LT.MIND) XINTER(L) = 0.0D0
         YINTER(L) = XINTER(L)
         Y(L) = XINTER(L)
 550  CONTINUE
      ICRI = 0
      DO 560 L=M1,N
         CRIT = REAL(ZINTER(L) - YINTER(L))
         IF (YINTER(L).LT.1.0D-5) GOTO 560
         CRIT = CRIT/REAL(YINTER(L))
         IF (ABS(CRIT).GT.EPSB) ICRI = 1
 560  CONTINUE
      IF (ICRI.GT.0) GOTO 510
 570  CONTINUE
!
! DECREASE NUMBER OF STEPS.
!
      K = K - 1
!
! SET COLUMN INDICATOR TO ZERO FOR NO TEST.
!
      L = 0
!
! IF ONLY ONE STEP LEFT GOTO 18 TO COMPLETE INTERVAL.
! IF MORE THAN ONE STEP LEFT, F IS DISTANCE TO GO.
!
!+**PJK 29/11/93 Arithmetic-IF replaced
!      IF (1-K) 580, 310, 590
      IF (K.GT.1) THEN
         GOTO 580
      ELSE IF (K.EQ.1) THEN
         GOTO 310
      ELSE
         GOTO 590
      END IF

 580  CONTINUE
      F = DBLE(H)*DBLE(K)
!
! L IS SET TO ORDER OF EXTRAPOLATION.
!
      L = IA
      FLODIV = REAL(F)/(H*(1.0+B*(REAL(O)**(1.0/REAL(1-IA))-1.0))) + 0.9
      K = INT(FLODIV)
!
! STEP LENGTH IS INCREASED BY CONFIDENCE FACTOR TIMES ESTIMATED
! FULL INCREASE TO PRODUCE CONVERGENCE IN IATH COLUMN,
!
      H = REAL(F)/REAL(K)
!
! RETURN TO 18 FOR NEXT STEP.
!
      GOTO 310
!
! END OF INTERVAL SO RETURN.
!
 590  CONTINUE

      RETURN
      END
!----------------------------------------------------------------------
      INTEGER FUNCTION LENGT(CHRSTR)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Enables the length of a character string to be calculated
!  excluding any trailing blanks.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  CHRSTR : (INPUT)  Character string of interest
!
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      CHARACTER*(*) CHRSTR

!  Local variables
      INTEGER I,I1

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      I1 = LEN(CHRSTR)
      LENGT = I1
      DO 10 I = I1,1,-1
        IF (CHRSTR(I:I).EQ.' ') THEN
          LENGT = LENGT - 1
        ELSE
          GOTO 20
        END IF
 10   CONTINUE

 20   CONTINUE

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE MASSIN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.001
!
!--Description
!  Subroutine to convert from wt% elements to atoms of each nuclide.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  17 November 1997
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  17/11/97 PJK 1.001 Corrected EXTERNAL statement (CONV --> CONV1)
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      INTEGER NSTEPS
      REAL GMS,YSTEPS,ZLEVEL
      COMMON /CALIST/ GMS,NSTEPS,YSTEPS,ZLEVEL

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      INTEGER IATMWT(364),ISEX2
      REAL FUWT(364)
      COMMON /INFO1 / IATMWT,FUWT,ISEX2

      CHARACTER*4 IDS(364)
      COMMON /INFO2 / IDS

      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

!  Local variables
      REAL XNUM(92),AWT(92),ABUN(13,92),ABN1(13,20),ABN2(13,20), &
           ABN3(13,20),ABN4(13,20),ABN5(13,12)
      REAL ZA
      INTEGER NUMB(92,2)
      INTEGER IA,J1,J2,KNUM,MAT

!  External routines
      EXTERNAL CONV1

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SAVE ABN1,ABN2,ABN3,ABN4,ABN5

      EQUIVALENCE (ABN1(1,1),ABUN(1,1))
      EQUIVALENCE (ABN2(1,1),ABUN(1,21))
      EQUIVALENCE (ABN3(1,1),ABUN(1,41))
      EQUIVALENCE (ABN4(1,1),ABUN(1,61))
      EQUIVALENCE (ABN5(1,1),ABUN(1,81))

      DATA AWT/1.00794,4.002602,6.941,9.01218,10.811,12.011,14.0067, &
           15.9994,18.998403,20.179,22.98977,24.305,26.98154,28.0855, &
           30.97376,32.066,35.453,39.948,39.0983,40.078,44.95591,47.88, &
           50.9415,51.9961,54.9380,55.847,58.9332,58.69,63.546,65.39, &
           69.723,72.59,74.9216,78.96,79.904,83.80,85.4678,87.62, &
           88.9059,91.224,92.9064,95.94,0.,101.07,102.9055,106.42, &
           107.8682,112.41,114.82,118.710,121.75,127.60,126.9045,131.29, &
           132.9054,137.33,138.9055,140.12,140.9077,144.24,0.,150.36, &
           151.96,157.25,158.9254,162.50,164.9304,167.26,168.9342, &
           173.04,174.967,178.49,180.9479,183.85,186.207,190.2,192.22, &
           195.08,196.9665,200.59,204.383,207.2,208.9804,0.,0.,0.,0.,0., &
           0.,232.0381,0.,238.0289/
      DATA NUMB(1,1),NUMB(1,2)/2,1/
      DATA NUMB(2,1),NUMB(2,2)/2,3/
      DATA NUMB(3,1),NUMB(3,2)/2,6/
      DATA NUMB(4,1),NUMB(4,2)/1,9/
      DATA NUMB(5,1),NUMB(5,2)/2,10/
      DATA NUMB(6,1),NUMB(6,2)/2,12/
      DATA NUMB(7,1),NUMB(7,2)/2,14/
      DATA NUMB(8,1),NUMB(8,2)/3,16/
      DATA NUMB(9,1),NUMB(9,2)/1,19/
      DATA NUMB(10,1),NUMB(10,2)/3,20/
      DATA NUMB(11,1),NUMB(11,2)/1,23/
      DATA NUMB(12,1),NUMB(12,2)/3,24/
      DATA NUMB(13,1),NUMB(13,2)/1,27/
      DATA NUMB(14,1),NUMB(14,2)/3,28/
      DATA NUMB(15,1),NUMB(15,2)/1,31/
      DATA NUMB(16,1),NUMB(16,2)/5,32/
      DATA NUMB(17,1),NUMB(17,2)/3,35/
      DATA NUMB(18,1),NUMB(18,2)/5,36/
      DATA NUMB(19,1),NUMB(19,2)/3,39/
      DATA NUMB(20,1),NUMB(20,2)/9,40/
      DATA NUMB(21,1),NUMB(21,2)/1,45/
      DATA NUMB(22,1),NUMB(22,2)/5,46/
      DATA NUMB(23,1),NUMB(23,2)/2,50/
      DATA NUMB(24,1),NUMB(24,2)/5,50/
      DATA NUMB(25,1),NUMB(25,2)/1,55/
      DATA NUMB(26,1),NUMB(26,2)/5,54/
      DATA NUMB(27,1),NUMB(27,2)/1,59/
      DATA NUMB(28,1),NUMB(28,2)/7,58/
      DATA NUMB(29,1),NUMB(29,2)/3,63/
      DATA NUMB(30,1),NUMB(30,2)/7,64/
      DATA NUMB(31,1),NUMB(31,2)/3,69/
      DATA NUMB(32,1),NUMB(32,2)/7,70/
      DATA NUMB(33,1),NUMB(33,2)/1,75/
      DATA NUMB(34,1),NUMB(34,2)/9,74/
      DATA NUMB(35,1),NUMB(35,2)/3,79/
      DATA NUMB(36,1),NUMB(36,2)/9,78/
      DATA NUMB(37,1),NUMB(37,2)/3,85/
      DATA NUMB(38,1),NUMB(38,2)/5,84/
      DATA NUMB(39,1),NUMB(39,2)/1,89/
      DATA NUMB(40,1),NUMB(40,2)/7,90/
      DATA NUMB(41,1),NUMB(41,2)/1,93/
      DATA NUMB(42,1),NUMB(42,2)/9,92/
      DATA NUMB(43,1),NUMB(43,2)/0,0/
      DATA NUMB(44,1),NUMB(44,2)/9,96/
      DATA NUMB(45,1),NUMB(45,2)/1,103/
      DATA NUMB(46,1),NUMB(46,2)/9,102/
      DATA NUMB(47,1),NUMB(47,2)/3,107/
      DATA NUMB(48,1),NUMB(48,2)/11,106/
      DATA NUMB(49,1),NUMB(49,2)/3,113/
      DATA NUMB(50,1),NUMB(50,2)/13,112/
      DATA NUMB(51,1),NUMB(51,2)/3,121/
      DATA NUMB(52,1),NUMB(52,2)/11,120/
      DATA NUMB(53,1),NUMB(53,2)/1,127/
      DATA NUMB(54,1),NUMB(54,2)/13,124/
      DATA NUMB(55,1),NUMB(55,2)/1,133/
      DATA NUMB(56,1),NUMB(56,2)/9,130/
      DATA NUMB(57,1),NUMB(57,2)/2,138/
      DATA NUMB(58,1),NUMB(58,2)/7,136/
      DATA NUMB(59,1),NUMB(59,2)/1,141/
      DATA NUMB(60,1),NUMB(60,2)/9,142/
      DATA NUMB(61,1),NUMB(61,2)/0,0/
      DATA NUMB(62,1),NUMB(62,2)/11,144/
      DATA NUMB(63,1),NUMB(63,2)/3,151/
      DATA NUMB(64,1),NUMB(64,2)/9,152/
      DATA NUMB(65,1),NUMB(65,2)/1,159/
      DATA NUMB(66,1),NUMB(66,2)/9,156/
      DATA NUMB(67,1),NUMB(67,2)/1,165/
      DATA NUMB(68,1),NUMB(68,2)/9,162/
      DATA NUMB(69,1),NUMB(69,2)/1,169/
      DATA NUMB(70,1),NUMB(70,2)/9,168/
      DATA NUMB(71,1),NUMB(71,2)/2,175/
      DATA NUMB(72,1),NUMB(72,2)/7,174/
      DATA NUMB(73,1),NUMB(73,2)/2,180/
      DATA NUMB(74,1),NUMB(74,2)/7,180/
      DATA NUMB(75,1),NUMB(75,2)/3,185/
      DATA NUMB(76,1),NUMB(76,2)/9,184/
      DATA NUMB(77,1),NUMB(77,2)/3,191/
      DATA NUMB(78,1),NUMB(78,2)/9,190/
      DATA NUMB(79,1),NUMB(79,2)/1,197/
      DATA NUMB(80,1),NUMB(80,2)/9,196/
      DATA NUMB(81,1),NUMB(81,2)/3,203/
      DATA NUMB(82,1),NUMB(82,2)/5,204/
      DATA NUMB(83,1),NUMB(83,2)/1,209/
      DATA NUMB(84,1),NUMB(84,2)/0,0/
      DATA NUMB(85,1),NUMB(85,2)/0,0/
      DATA NUMB(86,1),NUMB(86,2)/0,0/
      DATA NUMB(87,1),NUMB(87,2)/0,0/
      DATA NUMB(88,1),NUMB(88,2)/0,0/
      DATA NUMB(89,1),NUMB(89,2)/0,0/
      DATA NUMB(90,1),NUMB(90,2)/1,232/
      DATA NUMB(91,1),NUMB(91,2)/0,0/
      DATA NUMB(92,1),NUMB(92,2)/5,234/
      DATA ABN1/99.985,0.015,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.000138, &
           99.999862,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,7.5,92.5,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,19.9,80.1,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,98.9,1.1,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,99.634,0.366,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,99.762,0.038,0.2,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,90.51,0.27,9.22, &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,78.99,10.0,11.01,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,92.23,4.67,3.10,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,95.02,0.75,4.21,0.,0.02,0.,0.,0.,0.,0.,0.,0.,0., &
           75.77,0.,24.23,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.337,0.,0.063, &
           0.,99.6,0.,0.,0.,0.,0.,0.,0.,0.,93.2581,0.0117,6.7302,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,96.941,0.,0.647,0.135,2.086,0.,0.004, &
           0.,0.187,0.,0.,0.,0./
      DATA ABN2/100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,8.0,7.3,73.8, &
           5.5,5.4,0.,0.,0.,0.,0.,0.,0.,0.,0.250,99.750,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,4.345,0.,83.789,9.501,2.365,0.,0.,0.,0.,0., &
           0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,5.8,0., &
           91.72,2.2,0.28,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,68.27,0.,26.10,1.13,3.59,0.,0.91,0.,0., &
           0.,0.,0.,0.,69.17,0.,30.83,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           48.6,0.,27.9,4.1,18.8,0.,0.6,0.,0.,0.,0.,0.,0.,60.1,0.,39.9, &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,20.5,0.,27.4,7.8,36.5,0.,7.8, &
           0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.9,0.,9.0,7.6,23.6,0.,49.7,0.,9.2,0.,0.,0.,0.,50.69,0., &
           49.31,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.35,0.,2.25,0.,11.6, &
           11.5,57.0,0.,17.3,0.,0.,0.,0.,72.165,0.,27.835,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.56,0.,9.86,7.0,82.58,0.,0.,0.,0.,0.,0., &
           0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,51.45,11.22, &
           17.15,0.,17.38,0.,2.8,0.,0.,0.,0.,0.,0./
      DATA ABN3/100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,14.84,0.,9.25, &
           15.92,16.68,9.55,24.13,0.,9.63,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,5.52,0.,1.88,12.7,12.6,17.0,31.6,0., &
           18.7,0.,0.,0.,0.,100.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           1.02,0.,11.14,22.33,27.33,0.,26.46,0.,11.72,0.,0.,0.,0., &
           51.839,0.,48.161,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,1.25,0.,0.89, &
           0.,12.49,12.80,24.13,12.22,28.73,0.,7.49,0.,0.,4.3,0.,95.7, &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.97,0.,0.65,0.36,14.53,7.68, &
           24.22,8.58,32.59,0.,4.63,0.,5.79,57.3,0.,42.7,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.096,0.,2.6,0.908,4.816,7.14,18.95,0.,31.69, &
           0.,33.80,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.10,0.,0.09,0.,1.91,26.4,4.1,21.2,26.9,0.,10.4,0.,8.9,100.0, &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.106,0.,0.101,0.,2.417, &
           6.592,7.854,11.23,71.7,0.,0.,0.,0.,0.09,99.91,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.19,0.,0.25,0.,88.48,0.,11.08,0.,0.,0.,0., &
           0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,27.13,12.18, &
           23.80,8.3,17.19,0.,5.76,0.,5.64,0.,0.,0.,0./
      DATA ABN4/0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,3.1,0.,0.,15.0, &
           11.3,13.8,7.4,0.,26.7,0.,22.7,0.,0.,47.8,0.,52.2,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.2,0.,2.18,14.8,20.47,15.65,24.84,0., &
           21.86,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.06,0.,0.1,0.,2.34,18.9,25.5,24.9,28.2,0.,0.,0.,0.,100.0,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.14,0.,1.61,0.,33.6,22.95, &
           26.8,0.,14.9,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.13,0.,3.05,14.3,21.9,16.12,31.8,0.,12.7,0.,0.,0.,0., &
           97.41,2.59,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.162,0.,5.206, &
           18.606,27.297,13.629,35.1,0.,0.,0.,0.,0.,0.,0.012,99.988,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.13,0.,26.3,14.3,30.67,0., &
           28.6,0.,0.,0.,0.,0.,0.,37.4,0.,62.6,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.02,0.,1.58,1.6,13.3,16.1,26.4,0.,41.0,0.,0.,0.,0., &
           37.3,0.,62.7,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.01,0.,0.79,0., &
           32.9,33.8,25.3,0.,7.2,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.14,0.,10.02,16.84,23.13,13.22,29.80,0.,6.85, &
           0.,0.,0.,0./
      DATA ABN5/29.524,0.,70.476,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,1.4,0., &
           24.1,22.1,52.4,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
           0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.0055,0.720,0.,0., &
           99.2745,0.,0.,0.,0.,0.,0.,0.,0./

      GMS = WMASS * 1000.
      KNUM = 0
      DO 40 J1 = 1,83
         IF (WELEMP(J1).LE.1.E-10) GOTO 40
         IF (AWT(J1).GT.0.0) THEN
            XNUM(J1) = GMS*WELEMP(J1)*6.02204459E21/AWT(J1)
            DO 30 J2 = 1,NUMB(J1,1)
               KNUM = KNUM + 1
               IA = NUMB(J1,2) + J2 - 1
               ZA = 1000.*REAL(J1) + REAL(IA)
               CALL CONV1(ZA,MAT)

!  One line added so that Ta180m is long lived isotope 17/11/93
               IF ((1000*J1 + IA).EQ.73180) MAT = MAT + 1

!+**PJK 07/12/93 Added IF-clause to prevent zero subscript errors
               IF (MAT.NE.0) THEN
                  B(MAT) = DBLE(XNUM(J1)*ABUN(J2,J1)/100.)
                  FUWT(KNUM) = REAL(B(MAT))
                  IDS(KNUM) = NAMREP(J1)
                  IATMWT(KNUM) = IA
               END IF
 30         CONTINUE
         END IF
 40   CONTINUE
      ISEX2 = KNUM

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE OUTFIS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Prepares the data for the output common for PROCESS.
!  Note that only a single time interval is allowed.
!
!--Author
!  Robin Forrest D3/176 Culham Laboratory, ext.3586
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  None
!
!--Global variables passed via COMMON
!  See FISPRO routine for details
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Global variables
      REAL A(18000)
      COMMON/BLOCK /A

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      REAL XA(19),XMU(19)
      COMMON /DOSOUT/ XA,XMU

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      DOUBLE PRECISION WMIND
      COMMON /PRPRM1/ WMIND

!  Local variables
      INTEGER I,ID,INDX20,INDX9,INDX99,MASS,MCROSS,MFRAC,MIDEN,MLAMDA, &
           MSPEC,MTOTAL,MTRANS,MYIELD,MYILDS,N,NCH,NCROSS,NISOT,NSPEC, &
           NSTART,NTRANS,NYIELD
      REAL ACT,XDOSE

!  External routines
      EXTERNAL IDNTFY

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! *** Get value of N by calling IDNTFY

      NSTART = 0
      CALL IDNTFY(NSTART, NCH, NSPEC, NISOT, MTOTAL, MFRAC, N, &
           MASS, MIDEN, MLAMDA, MSPEC, MYILDS, MYIELD, NTRANS, MTRANS, &
           NCROSS, MCROSS, NYIELD)

! *** Set up the pointer to the A() array.

      INDX9 = MLAMDA

! *** Initialise the total quantities

      WTOTA = 0.0
      WTOTD = 0.0
      WTOTH = 0.0

      DO 10 I = 1,N-5
         WINVT(I) = REAL(B(I))
 10   CONTINUE

! *** Note that it is necessary to add the gas nuclides at the end of
! *** the inventory on at the start

      DO 20 I = 1,5
         WINVT(I) = WINVT(I) + REAL(B(N-5+I))
 20   CONTINUE

! *** If WINVT() is < WMIND then set to 0.0

      DO 40 I=1,N-5
         IF (WINVT(I).LT.WMIND) WINVT(I) = 0.0
         INDX9 = INDX9 + 1
         ACT = WINVT(I)*A(INDX9)

! *** Add contribution to total activity

         WTOTA = WTOTA + ACT

         IF (ACT.GT.0.0) THEN
            INDX99 = MSPEC + (I-1)*NSPEC + 9
            INDX20 = INDX99 - 5

! *** Add contribution to total heat output

            WTOTH = WTOTH + ACT * (A(INDX20-1)+A(INDX20)+A(INDX20+1))

! *** Add contribution to total gamma dose rate

            XDOSE = 0.0
            DO 30 ID = 1,19
               XDOSE = XDOSE + A(INDX99+ID)*XA(ID)/XMU(ID)
 30         CONTINUE

            WTOTD = WTOTD + XDOSE*ACT*5.76E-10/WMASS

         END IF

 40   CONTINUE

! *** Convert from Bq-MeV to kW

      WTOTH = WTOTH * 1.6021773E-16

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE BBIE(NENT,X,F)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.010
!
!--Description
!  Breeding Blanket Interpolating Evaluation
!  Routine to calculate various aspects of a generic fusion reactor's
!  neutronics using a library that contains information in the form
!  of spline details.
!  The calculation is accomplished by evaluating an interpolating
!  spline at intermediate points. This code retrieves the knot points,
!  spline and first derivative values from a library, and then calls
!  the TG01BD routine to obtain an intermediate function value. It will
!  write an error message and return a zero function value if an
!  extrapolated value is requested.
!  The library entries refer to a series of calculations on the EEF
!  reactor concept, using the 'standard structural materials' option.
!  The thicknesses of the outboard and inboard breeding blankets were
!  varied in such a way that their ratio was constant (out/in = 1.5825),
!  between limits 51.5 cm .LE. outboard thickness .LE. 106.5 cm.
!
!--Author
!  L.J. Baker, Physics Applications Dept, AEA Consultancy Services
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  17 November 1997
!
!--Reference
!  Neutronics Model for the PROCESS code, L.J. Baker
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!  17/11/97 PJK 1.010 Added IVMS1 coding
!
!--Arguments
!  NENT   : (INPUT)  Library entry identifier:
!           200 : F(1) = tritium breeding ratio
!           201 : F(1) = IBB integrated zone power
!           202 : F(1) = IFW integrated zone power
!           203 : F(1) = OFW integrated zone power
!           204 : F(1) = OBB integrated zone power
!           205 : F(1) = IBB mean zone power density
!           206 : F(1) = IFW mean zone power density
!           207 : F(1) = OFW mean zone power density
!           208 : F(1) = OBB mean zone power density
!           210 : F contains IBB mean zone neutron flux spectrum
!           211 : F contains IFW mean zone neutron flux spectrum
!           212 : F contains OFW mean zone neutron flux spectrum
!           213 : F contains OBB mean zone neutron flux spectrum
!           where IBB = inboard breeding blanket,
!                 IFW = inboard first wall,
!                 OFW = outboard first wall,
!           and   OBB = outboard breeding blanket.
!  X      : (INPUT)  Outboard blanket radial thickness (cm)
!  F      : (OUTPUT) See NENT above
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      DOUBLE PRECISION F(100),X
      INTEGER NENT

!  Global variables
      INTEGER IVMS1
      COMMON /OPSYS/IVMS1

!  Local variables
      CHARACTER*80 TITLE
      DOUBLE PRECISION X1D(20),F1D(20),D1D(20),FFUN(20,100), &
           DERIV(20,100),XPRE
      INTEGER I,J,K,NFLU,NIX,NKEY,NLIM,NSIZE,NSW1

!  External functions
      DOUBLE PRECISION TG01BD
      EXTERNAL         TG01BD

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! *** Dimensions set for 20 spline knots, 100 neutron energy groups

! *** NSW1 - Trigger for library read
! *** NLIM - Maximum number of library entries

      DATA XPRE/0.0D0/
      DATA NSW1/0/

      NLIM = 1000

! *** Open library file

!+**PJK 17/11/97
      IF (IVMS1.EQ.1) THEN
         OPEN(26,FILE='BBIFL.USER',STATUS='OLD')
      ELSE
         OPEN(26,FILE='fispact/bbifl.user',STATUS='OLD')
      END IF

! *** Values to be reset on every call
      NIX = -1
      DO 10 I = 1,100
         F(I) = 0.0D0
 10   CONTINUE

! *** Retrieve spline data from library unless previous call demanded
! *** same entry, and when previously-called spline is in use
! *** determine if present function evaluation point is higher than
! *** previous one

      IF ((NSW1.EQ.0).OR.(NSW1.NE.NENT)) THEN

         REWIND(26)
         DO 30 I = 1,NLIM
! *** N.B. NFLU is 1 for integral parameter,
! *** 100 for GAM-II flux spectrum
            READ(26,80) TITLE
            READ(26,70) NKEY,NSIZE,NFLU
            READ(26,90) (X1D(J),J=1,NSIZE)
            DO 20 K = 1,NFLU
               READ(26,90) (FFUN(J,K),J=1,NSIZE)
               READ(26,90) (DERIV(J,K),J=1,NSIZE)
 20         CONTINUE
            IF (NKEY.EQ.NENT) GOTO 40
 30      CONTINUE
 40      CONTINUE
         NSW1 = NENT

      ELSE

! *** In version 1.0 of the code the following statement set NIX=1
! *** to achieve time savings in successive evaluations of the same
! *** spline. This is not possible in this version because the code
! *** is cycling through NFLU splines. Statement retained against
! *** future modification of TG01BD

          IF (X.GT.XPRE) NIX = -1
      END IF
      XPRE = X

      DO 60 K = 1,NFLU
         DO 50 J = 1,NSIZE
            F1D(J) = FFUN(J,K)
            D1D(J) = DERIV(J,K)
 50      CONTINUE
         F(K) = TG01BD(NIX,NSIZE,X1D,F1D,D1D,X)

! *** Error message if function is exactly zero
! *** (attempted extrapolation)

         IF (F(K).EQ.0.0D0) THEN
            WRITE(*,*) 'Error in routine BBIE:'
            WRITE(*,*) 'Extrapolation attempted : X = ',X
            WRITE(*,*) 'It should be in the range 51.5 cm -- 106.5 cm'
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         END IF
 60   CONTINUE

! *** Input formats

 70   FORMAT(3I6)
 80   FORMAT(A80)
 90   FORMAT(6D12.4)

! *** Output formats

 100  FORMAT(/5X,'Current entry from breeding blanket data library ', &
           'is:',//2X,I4,1X,A80)

      CLOSE(26)

      RETURN
      END
!----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION TG01BD(IX,N,U,S,D,X)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Function to evaluate a cubic spline given spline values and first
!  derivative values at the given knots. The spline value is defined
!  as zero outside the knot range, which is extended by a rounding
!  error for the purpose.
!
!--Author
!  L.J. Baker, Physics Applications Dept, AEA Consultancy Services
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  IX     : (INPUT)  Allows caller to take advantage of spline
!           parameters set on a previous call in cases when X point
!           follows previous X point. If IX < 0 the whole range is
!           searched for knot INTERVAL; If IX > 0 it is assumed that
!           X is greater than the X of the previous call and search
!           started from there.
!  N      : (INPUT)  Number of knots
!  U      : (INPUT)  The knots
!  S      : (INPUT)  The spline values
!  D      : (INPUT)  The first derivative values of the spline at
!           the knots
!  X      : (INPUT)  The point at which the spline value is required
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      DOUBLE PRECISION U(20),S(20),D(20),X
      INTEGER IX,N

!  Local variables
      DOUBLE PRECISION A,B,H,Q1,Q2,SS,Z,EPS
      INTEGER IFLG,J

!  External functions
      DOUBLE PRECISION FD05AD
      EXTERNAL         FD05AD

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! *** ALLOWABLE ROUNDING ERROR ON POINTS AT EXTREMES OF KNOT RANGE
! *** IS EPS*MAX(|U(1)|,|U(N)|).

      SAVE

      DATA IFLG/0/
      DATA J/0/

      EPS = FD05AD(1)*4.0D0

! *** TEST WHETHER N IS IN RANGE

      IF ((N.LT.1).OR.(N.GT.20)) THEN
         WRITE(*,*) 'Error in routine TG01BD:'
         WRITE(*,*) 'N not in range 1-20, = ',N
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      END IF

! *** TEST WHETHER POINT IN RANGE.

      IF (X.LT.U(1)) GOTO 70
      IF (X.GT.U(N)) GOTO 80

! *** JUMP IF KNOT INTERVAL REQUIRES RANDOM SEARCH.

      IF ((IX.LT.0).OR.(IFLG.EQ.0)) GOTO 30

! *** JUMP IF KNOT INTERVAL SAME AS LAST TIME.

      IF (X.LE.U(J+1)) GOTO 60

! *** LOOP TILL INTERVAL FOUND.

 10   CONTINUE
      J = J+1
 20   CONTINUE
      IF (X.GT.U(J+1)) GOTO 10
      GOTO 50

! *** ESTIMATE KNOT INTERVAL BY ASSUMING EQUALLY SPACED KNOTS.

 30   CONTINUE
      J = INT(ABS(X-U(1))/(U(N)-U(1)))*(N-1)+1

! *** ENSURE CASE X = U(N) GIVES J = N-1.

      J = MIN(J,N-1)

! *** INDICATE THAT KNOT INTERVAL INSIDE RANGE HAS BEEN USED.

      IFLG = 1

! *** SEARCH FOR KNOT INTERVAL CONTAINING X.

      IF (X.GE.U(J)) GOTO 20
 40   CONTINUE
      J = J-1
      IF (X.LT.U(J)) GOTO 40

! *** CALCULATE SPLINE PARAMETERS FOR JTH INTERVAL.

 50   CONTINUE
      H = U(J+1)-U(J)
      Q1 = H*D(J)
      Q2 = H*D(J+1)
      SS = S(J+1)-S(J)
      B = 3D0*SS-2D0*Q1-Q2
      A = Q1+Q2-2.0D0*SS

! *** CALCULATE SPLINE VALUE.

 60   CONTINUE
      Z = (X-U(J))/H
      TG01BD = ((A*Z+B)*Z+Q1)*Z+S(J)
      GOTO 1000

! *** TEST IF X WITHIN ROUNDING ERROR OF U(1).

 70   CONTINUE
      IF (X.LE.U(1)-EPS*MAX(ABS(U(1)),ABS(U(N)))) GOTO 90
      J = 1
      GOTO 50

! *** TEST IF X WITHIN ROUNDING ERROR OF U(N).

 80   CONTINUE
      IF (X.GE.U(N)+EPS*MAX(ABS(U(1)),ABS(U(N)))) GOTO 90
      J = N-1
      GOTO 50

 90   CONTINUE
      IFLG = 0

! *** FUNCTION VALUE SET TO ZERO FOR POINTS OUTSIDE THE RANGE.

      TG01BD = 0.0D0

 1000 CONTINUE

      RETURN
      END
!----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FD05AD(INUM)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.000
!
!--Description
!  Machine constants (double precision arithmetic).
!  Obtained from H.S.L. subroutine ZE02AM.
!  Nick Gould and Sid Marlow, Harwell, July 1988.
!
!  DC(1) the smallest positive number such that 1.0 + DC(1) > 1.0.
!  DC(2) the smallest positive number such that 1.0 - DC(2) < 1.0.
!  DC(3) the smallest non-zero positive real number.
!  DC(4) the smallest full precision positive real number.
!  DC(5) the largest finite positive real number.
!
!--Author
!  L.J. Baker, Physics Applications Dept, AEA Consultancy Services
!  Peter Knight  D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  01 December 1993
!
!--Reference
!  None
!  
!--History
!  01/12/93 PJK 1.000 Initial version
!
!--Arguments
!  INUM   : (INPUT)  Chosen constant = DC(INUM)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      INTEGER INUM

!  Local variables
      DOUBLE PRECISION DC(5)

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DC(1) = 0.222044604925031309D-15
      DC(2) = 0.138777878078144569D-16
      DC(3) = 0.539760534693402790D-78
      DC(4) = 0.539760534693402790D-78
      DC(5) = 0.723700557733226210D+76

      IF ((INUM.LE.0).OR.(INUM.GE.6)) THEN
         WRITE(*,*) 'Error in routine FD05AD:'
         WRITE(*,*) 'INUM not in range 1-5, = ',INUM
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      ELSE
         FD05AD = DC(INUM)
      END IF

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE ELCOMP(ECSS,ECMS,ECBE,ECVD,ECLI2O,ECLIPB,ECLI,ECHE, &
           ECH2O)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!--Version number 1.100
!
!--Description
! *** Elemental compositions of blanket and first wall materials
! *** Fractions by weight, with parts per million assumed to mean
! *** the same (e.g. 1 ppm = 1.0D-4 per cent by weight).
! ***
! *** The fraction of the material consisting of the Jth element
! *** (H, He, Li, ...) as described in NAMREP is stored in the
! *** Jth element of the relevant array
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  05 February 1997
!
!--Reference
!  Elemental compositions taken from SEAFP report, October 1993, by
!  Jean-Christophe Sublet, D3/166 Culham Laboratory, ext.3492
!  
!--History
!  03/12/93 PJK 1.000 Initial version
!  05/02/97 PJK 1.100 Added martensitic steel
!
!--Arguments
!  ecss   : (OUTPUT) fractions of the elements in stainless steel
!                    (assumed to be austenitic)
!  ecms   : (OUTPUT) fractions of the elements in martensitic steel
!  ecbe   : (OUTPUT) fractions of the elements in beryllium
!  ecvd   : (OUTPUT) fractions of the elements in vanadium
!  ecli2o : (OUTPUT) fractions of the elements in lithium oxide
!  eclipb : (OUTPUT) fractions of the elements in lithium lead
!  ecli   : (OUTPUT) fractions of the elements in lithium
!  eche   : (OUTPUT) fractions of the elements in helium
!  ech2o  : (OUTPUT) fractions of the elements in water
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  Arguments
      DOUBLE PRECISION ecss(83),ecbe(83),ecvd(83),ecli2o(83), &
           eclipb(83),ecli(83),eche(83),ech2o(83),ecms(83)

!  Local variables
      DOUBLE PRECISION sum
      INTEGER j

!--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! *** Initialise arrays

      do 10 j = 1,83
         ecss(j) = 0.0D0
         ecms(j) = 0.0D0
         ecbe(j) = 0.0D0
         ecvd(j) = 0.0D0
         ecli2o(j) = 0.0D0
         eclipb(j) = 0.0D0
         ecli(j) = 0.0D0
         eche(j) = 0.0D0
         ech2o(j) = 0.0D0
 10   continue
         
! *** Stainless steel - structural component
! *** SEAFP report, Table I(a), Austenitic 316

      ecss(28) = 0.12D0
      ecss(24) = 0.17D0
      ecss(14) = 0.01D0
      ecss(25) = 0.02D0
      ecss(42) = 0.025D0
      ecss( 6) = 0.008D0
      ecss( 7) = 0.0006D0
      ecss(73) = 0.0005D0
      ecss(41) = 0.0001D0
      ecss(29) = 0.002D0
      ecss(22) = 50.0D-6
      ecss(13) = 50.0D-6
      ecss(27) = 900.0D-6
      ecss(47) = 400.0D-6
      ecss(51) = 10.0D-6
      ecss(50) = 10.0D-6
      sum = 0.0D0
      do 20 j = 1,83
         sum = sum + ecss(j)
 20   continue
      ecss(26) = 1.0D0 - sum

! *** Beryllium - neutron multiplier
! *** SEAFP report, Table III, Be

      ecbe( 8) = 0.0045D0
      ecbe(26) = 600.0D-6
      ecbe(13) = 200.0D-6
      ecbe(22) = 60.0D-6
      ecbe(27) = 1.0D-6
      ecbe(42) = 1.0D-6
      ecbe(24) = 50.0D-6
      ecbe(12) = 50.0D-6
      ecbe(14) = 300.0D-6
      ecbe(28) = 90.0D-6
      sum = 0.0D0
      do 30 j = 1,83
         sum = sum + ecbe(j)
 30   continue
      ecbe(4) = 1.0D0 - sum

! *** Vanadium - structural component
! *** SEAFP report, Table I(b), V-5Ti

      ecvd(22) = 0.05D0
      ecvd(14) = 400.0D-6
      ecvd(13) = 50.0D-6
      ecvd(26) = 10.0D-6
      ecvd(28) = 1.0D-6
      ecvd(27) = 0.1D-6
      ecvd( 7) = 180.0D-6
      ecvd(42) = 1.0D-6
      ecvd( 6) = 60.0D-6
      ecvd(74) = 232.0D-6
      ecvd(41) = 50.0D-6
      ecvd(72) = 1.0D-6
      ecvd(47) = 0.01D-6
      ecvd(29) = 0.3D-6
      sum = 0.0D0
      do 40 j = 1,83
         sum = sum + ecvd(j)
 40   continue
      ecvd(23) = 1.0D0 - sum

! *** Lithium Oxide - tritium synthesiser
! *** SEAFP report, Table II(a), Li2O (column 1)

      ecli2o( 3) = 0.465D0
      ecli2o(22) = 5.0D-6
      ecli2o(19) = 7.0D-6
      ecli2o(14) = 40.0D-6
      ecli2o(20) = 40.0D-6
      ecli2o(17) = 5.0D-6
      ecli2o(13) = 100.0D-6
      ecli2o(26) = 30.0D-6
      ecli2o(25) = 0.8D-6
      ecli2o(28) = 1.5D-6
      ecli2o(82) = 0.7D-6
      ecli2o(29) = 15.0D-6
      ecli2o(24) = 2.0D-6
      ecli2o(31) = 4.0D-6
      ecli2o(12) = 70.0D-6
      ecli2o(78) = 3.5D-6
      ecli2o(74) = 2.0D-6
      ecli2o(30) = 8.0D-6
      ecli2o(40) = 20.0D-6
      sum = 0.0D0
      do 50 j = 1,83
         sum = sum + ecli2o(j)
 50   continue
      ecli2o(8) = 1.0D0 - sum

! *** Lithium Lead - liquid tritium synthesiser and/or coolant
! *** SEAFP report, Table III, 17Li-83Pb (column 2)

      eclipb( 3) = 0.007D0
      eclipb(30) = 10.0D-6
      eclipb(26) = 10.0D-6
      eclipb(83) = 43.0D-6
      eclipb(48) = 5.0D-6
      eclipb(47) = 5.0D-6
      eclipb(50) = 5.0D-6
      eclipb(28) = 2.0D-6
      sum = 0.0D0
      do 60 j = 1,83
         sum = sum + eclipb(j)
 60   continue
      eclipb(82) = 1.0D0 - sum

! *** Lithium - tritium synthesiser

      ecli( 3) = 1.0D0

! *** Helium - coolant

      eche( 2) = 1.0D0

! *** Water - coolant

      ech2o( 1) = 0.111915D0
      ech2o( 8) = 0.888085D0

! *** Martensitic steel - structural component
! *** SEAFP report, Table I(a), Martensitic Manet 2

      ecms(28) = 0.006D0
      ecms(24) = 0.105D0
      ecms(14) = 0.002D0
      ecms(25) = 0.008D0
      ecms(42) = 0.006D0
      ecms( 6) = 0.0011D0
      ecms( 7) = 0.0003D0
      ecms(23) = 0.002D0
      ecms(41) = 0.0015D0
      ecms(40) = 0.00025D0
      ecms(29) = 100.0D-6
      ecms(13) = 100.0D-6
      ecms(27) = 50.0D-6
      ecms(47) = 50.0D-6
      ecms(33) = 100.0D-6
      ecms(51) = 4.0D-6
      ecms(50) = 10.0D-6
      sum = 0.0D0
      do 70 j = 1,83
         sum = sum + ecms(j)
 70   continue
      ecms(26) = 1.0D0 - sum

      return
      end
