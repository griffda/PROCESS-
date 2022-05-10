! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_module
   !! Module containing fusion power plant costing algorithms
   !! author: P J Knight, CCFE, Culham Science Centre
   !! N/A
   !! This module contains the PROCESS fusion power plant costing model,
   !! split into separate cost accounts.
   !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef dp
   use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   implicit none

   !  Various cost account values (M$)
   real(dp) :: c228, c229, c23, c25, c26, cindrt, ccont

   !  Account 226 - Heat transport system
   real(dp) :: c226, c2261, c2262, c2263

   !  Account 227 - Fuel handling
   real(dp) :: c227, c2271, c2272, c2273, c2274

   !  Account 24 - electrical plant equipment
   real(dp) :: c24, c241, c242, c243, c244, c245


   real(dp) :: c21, c211, c212, c213, c214, c2141, c2142, c215, c216, c217, c2171

   real(dp) :: c2172, c2173, c2174, c22, c2211, c2212, c22121, c22122, c22123

   real(dp) :: c22124, c22125, c22126, c22127, c2213, c22131, c22132, c2214, c2215

   real(dp) :: c2221, c22211, c22212, c22213, c22214, c22215, c2222, c22221, c22222

   real(dp) :: c22223, c22224, c2223, c223, c2231, c2232, c2233, c2234, c224, c2241

   real(dp) :: c2242, c2243, c2244, c2245, c2246, c225, c2251, c22511, c22512, c22513

   real(dp) :: c22514, c22515, c2252, c22521, c22522, c22523, c22524, c22525, c22526

   real(dp) :: c22527, c2253, chx, cpp, cppa, c22128

contains

   subroutine init_costs_module
      !! Initialise module variables
      implicit none

      c228 = 0.0D0
      c229 = 0.0D0
      c23 = 0.0D0
      c25 = 0.0D0
      c26 = 0.0D0
      cindrt = 0.0D0
      ccont = 0.0D0
      c226 = 0.0D0
      c2261 = 0.0D0
      c2262 = 0.0D0
      c2263 = 0.0D0
      c227 = 0.0D0
      c2271 = 0.0D0
      c2272 = 0.0D0
      c2273 = 0.0D0
      c2274 = 0.0D0
      c24 = 0.0D0
      c241 = 0.0D0
      c242 = 0.0D0
      c243 = 0.0D0
      c244 = 0.0D0
      c245 = 0.0D0
      c21 = 0.0D0
      c211 = 0.0D0
      c212 = 0.0D0
      c213 = 0.0D0
      c214 = 0.0D0
      c2141 = 0.0D0
      c2142 = 0.0D0
      c215 = 0.0D0
      c216 = 0.0D0
      c217 = 0.0D0
      c2171 = 0.0D0
      c2172 = 0.0D0
      c2173 = 0.0D0
      c2174 = 0.0D0
      c22 = 0.0D0
      c2211 = 0.0D0
      c2212 = 0.0D0
      c22121 = 0.0D0
      c22122 = 0.0D0
      c22123 = 0.0D0
      c22124 = 0.0D0
      c22125 = 0.0D0
      c22126 = 0.0D0
      c22127 = 0.0D0
      c2213 = 0.0D0
      c22131 = 0.0D0
      c22132 = 0.0D0
      c2214 = 0.0D0
      c2215 = 0.0D0
      c2221 = 0.0D0
      c22211 = 0.0D0
      c22212 = 0.0D0
      c22213 = 0.0D0
      c22214 = 0.0D0
      c22215 = 0.0D0
      c2222 = 0.0D0
      c22221 = 0.0D0
      c22222 = 0.0D0
      c22223 = 0.0D0
      c22224 = 0.0D0
      c2223 = 0.0D0
      c223 = 0.0D0
      c2231 = 0.0D0
      c2232 = 0.0D0
      c2233 = 0.0D0
      c2234 = 0.0D0
      c224 = 0.0D0
      c2241 = 0.0D0
      c2242 = 0.0D0
      c2243 = 0.0D0
      c2244 = 0.0D0
      c2245 = 0.0D0
      c2246 = 0.0D0
      c225 = 0.0D0
      c2251 = 0.0D0
      c22511 = 0.0D0
      c22512 = 0.0D0
      c22513 = 0.0D0
      c22514 = 0.0D0
      c22515 = 0.0D0
      c2252 = 0.0D0
      c22521 = 0.0D0
      c22522 = 0.0D0
      c22523 = 0.0D0
      c22524 = 0.0D0
      c22525 = 0.0D0
      c22526 = 0.0D0
      c22527 = 0.0D0
      c2253 = 0.0D0
      chx = 0.0D0
      cpp = 0.0D0
      cppa = 0.0D0
      c22128 = 0.0D0
   end subroutine init_costs_module

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine coelc(outfile,iprint)

      !! Routine to calculate the cost of electricity for a fusion power plant
      !! author: P J Knight, CCFE, Culham Science Centre
      !! outfile : input integer : output file unit
      !! iprint : input integer : switch for writing to output file (1=yes)
      !! This routine performs the calculation of the cost of electricity
      !! for a fusion power plant.
      !! <P>Annual costs are in megadollars/year, electricity costs are in
      !! millidollars/kWh, while other costs are in megadollars.
      !! All values are based on 1990 dollars.
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use cost_variables, only: fcdfuel, uche3, tlife, ifueltyp, cpstcst, &
         coeoam, coecap, output_costs, coe, lsa, cfactr, divcst, ucfuel, divlife, &
         coefuelt, moneyint, cdrlife, capcost, cplife, fwallcst, fcr0, discount_rate, &
         decomf, cdcost, fcap0, fcap0cp, ucwst, ucoam, dtlife, blkcst, dintrt, &
         concost, cfind
      use fwbs_variables, only: bktlife
      use ife_variables, only: uctarg, ife, reprat
      use heat_transport_variables, only: pnetelmw
      use physics_variables, only: itart, wtgpd, fhe3
      use process_output, only: oheadr, osubhd, ovarrf, oshead
      use times_variables, only: tcycle, tburn
      use constants, only: n_day_year

      implicit none

      !  Arguments

      integer, intent(in) :: iprint,outfile

      !  Local variables

      real(dp) :: anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
         annfuelt,annfwbl,annoam,anntot,annwst,coecdr, &
         coecp,coedecom,coediv,coefuel,coefwbl,coewst,crfcdr,crfcp, &
         crfdiv,crffwbl,fefcdr,fefcp,fefdiv,feffwbl,fwbllife,kwhpy
      ! annoam1,

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Number of kWh generated each year

      if (ife == 1) then
         kwhpy = 1.0D3 * pnetelmw * (24.0D0*n_day_year) * cfactr
      else
         kwhpy = 1.0D3 * pnetelmw * (24.0D0*n_day_year) * cfactr * tburn/tcycle
      end if

      !  Costs due to reactor plant
      !  ==========================

      !  Interest on construction costs

      moneyint = concost * (fcap0 - 1.0D0)

      !  Capital costs

      capcost = concost + moneyint

      !  Annual cost of plant capital cost

      anncap = capcost * fcr0

      ! SJP Issue #836
      ! Check for the condition when kwhpy=0

      if (kwhpy < 1.0d-10) kwhpy=1.0d-10

      !  Cost of electricity due to plant capital cost

      coecap = 1.0D9 * anncap / kwhpy

      !  Costs due to first wall and blanket renewal
      !  ===========================================

      !  Operational life

      fwbllife = bktlife

      !  Compound interest factor

      feffwbl = (1.0D0 + discount_rate)**fwbllife

      !  Capital recovery factor

      crffwbl = (feffwbl*discount_rate) / (feffwbl-1.0D0)

      !  Annual cost of replacements

      annfwbl = (fwallcst + blkcst) * &
         (1.0D0+cfind(lsa)) * fcap0cp * crffwbl

      if (ifueltyp == 2) then
         annfwbl = annfwbl * ( 1.0d0 - fwbllife / tlife)
      end if

      !  Cost of electricity due to first wall/blanket replacements

      coefwbl = 1.0D9 * annfwbl / kwhpy

      !  Costs due to divertor renewal
      !  =============================

      if (ife == 1) then
         anndiv = 0.0D0
         coediv = 0.0D0
      else

         !  Compound interest factor

         fefdiv = (1.0D0 + discount_rate)**divlife

         !  Capital recovery factor

         crfdiv = (fefdiv*discount_rate) / (fefdiv-1.0D0)

         !  Annual cost of replacements

         anndiv = divcst * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfdiv

         !  Cost of electricity due to divertor replacements

         if (ifueltyp == 2) then
            anndiv = anndiv * (1.0d0 - divlife / tlife)
         end if

         coediv = 1.0D9 * anndiv / kwhpy

      end if

      !  Costs due to centrepost renewal
      !  ===============================

      if ((itart == 1).and.(ife /= 1)) then

         !  Compound interest factor

         fefcp = (1.0D0 + discount_rate)**cplife

         !  Capital recovery factor

         crfcp = (fefcp*discount_rate) / (fefcp-1.0D0)

         !  Annual cost of replacements

         anncp = cpstcst * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfcp

         !  Cost of electricity due to centrepost replacements
         if (ifueltyp == 2) then
            anncp = anncp * (1.0d0 - cplife / tlife)
         end if


         coecp = 1.0D9 * anncp / kwhpy

      else
         anncp = 0.0D0
         coecp = 0.0D0
      end if

      !  Costs due to partial current drive system renewal
      !  =================================================

      !  Compound interest factor

      fefcdr = (1.0D0 + discount_rate)**cdrlife

      !  Capital recovery factor

      crfcdr = (fefcdr*discount_rate) / (fefcdr-1.0D0)

      !  Annual cost of replacements

      if (ifueltyp == 0) then
         anncdr = 0.0D0
      else
         anncdr = cdcost * fcdfuel/(1.0D0-fcdfuel) * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfcdr
      end if

      !  Cost of electricity due to current drive system replacements

      coecdr = 1.0D9 * anncdr / kwhpy

      !  Costs due to operation and maintenance
      !  ======================================

      !  Annual cost of operation and maintenance

      annoam = ucoam(lsa) * sqrt(pnetelmw/1200.0D0)

      !  Additional cost due to pulsed reactor thermal storage
      !  See F/MPE/MOD/CAG/PROCESS/PULSE/0008
      !
      !      if (lpulse.eq.1) then
      !         if (istore.eq.1) then
      !            annoam1 = 51.0D0
      !         else if (istore.eq.2) then
      !            annoam1 = 22.2D0
      !         else
      !            continue
      !         end if
      !
      !  Scale with net electric power
      !
      !         annoam1 = annoam1 * pnetelmw/1200.0D0
      !
      !  It is necessary to convert from 1992 pounds to 1990 dollars
      !  Reasonable guess for the exchange rate + inflation factor
      !  inflation = 5% per annum; exchange rate = 1.5 dollars per pound
      !
      !         annoam1 = annoam1 * 1.36D0
      !
      !         annoam = annoam + annoam1
      !
      !      end if

      !  Cost of electricity due to operation and maintenance

      coeoam = 1.0D9 * annoam / kwhpy

      !  Costs due to reactor fuel
      !  =========================

      !  Annual cost of fuel

      if (ife /= 1) then
         !  Sum D-T fuel cost and He3 fuel cost
         annfuel = ucfuel * pnetelmw/1200.0D0 + &
            1.0D-6 * fhe3 * wtgpd * 1.0D-3 * uche3 * n_day_year * cfactr
      else
         annfuel = 1.0D-6 * uctarg * reprat * 3.1536D7 * cfactr
      end if

      !  Cost of electricity due to reactor fuel

      coefuel = 1.0D9 * annfuel / kwhpy

      !  Costs due to waste disposal
      !  ===========================

      !  Annual cost of waste disposal

      annwst = ucwst(lsa) * sqrt(pnetelmw/1200.0D0)

      !  Cost of electricity due to waste disposal

      coewst = 1.0D9 * annwst / kwhpy

      !  Costs due to decommissioning fund
      !  =================================

      !  Annual contributions to fund for decommissioning
      !  A fraction decomf of the construction cost is set aside for
      !  this purpose at the start of the plant life.
      !  Final factor takes into account inflation over the plant lifetime
      !  (suggested by Tim Hender 07/03/96)
      !  Difference (dintrt) between borrowing and saving interest rates is
      !  included, along with the possibility of completing the fund dtlife
      !  years before the end of the plant's lifetime

      anndecom = decomf * concost * fcr0 / &
         (1.0D0+discount_rate-dintrt)**(tlife-dtlife)

      !  Cost of electricity due to decommissioning fund

      coedecom = 1.0D9 * anndecom / kwhpy

      !  Total costs
      !  ===========

      !  Annual costs due to 'fuel-like' components

      annfuelt = annfwbl + anndiv + anncdr + anncp + annfuel + annwst

      !  Total cost of electricity due to 'fuel-like' components

      coefuelt = coefwbl + coediv + coecdr + coecp + coefuel + coewst

      !  Total annual costs

      anntot = anncap + annfuelt + annoam + anndecom

      !  Total cost of electricity

      coe = coecap + coefuelt + coeoam + coedecom

      if ((iprint == 0).or.(output_costs == 0)) return

      !  Output section

      call oheadr(outfile,'Power Reactor Costs (1990 US$)')

      call ovarrf(outfile,'First wall / blanket life (years)','(fwbllife)', &
         fwbllife)

      if (ife /= 1) then
         call ovarrf(outfile,'Divertor life (years)','(divlife.)',divlife)
         if (itart == 1) then
            call ovarrf(outfile,'Centrepost life (years)','(cplife.)',cplife)
         end if
      end if


      call ovarrf(outfile,'Cost of electricity (m$/kWh)','(coe)',coe)

      call osubhd(outfile,'Power Generation Costs :')

      if ((annfwbl /= annfwbl).or.(annfwbl > 1.0D10).or.(annfwbl < 0.0D0)) then
         write(outfile,*)'Problem with annfwbl'
         write(outfile,*)'fwallcst=', fwallcst, '  blkcst=', blkcst
         write(outfile,*)'crffwbl=', crffwbl,   '  fcap0cp=', fcap0cp
         write(outfile,*)'feffwbl=', feffwbl,   '  fwbllife=', fwbllife
      end if

      write(outfile,200) &
         anncap,coecap, &
         annoam,coeoam, &
         anndecom,coedecom, &
         annfwbl,coefwbl, &
         anndiv,coediv, &
         anncp,coecp, &
         anncdr,coecdr, &
         annfuel,coefuel, &
         annwst,coewst, &
         annfuelt,coefuelt, &
         anntot,coe

200   format( &
         t76,'Annual Costs, M$       COE, m$/kWh'// &
         1x,'Capital Investment',t80,f10.2,10x,f10.2/ &
         1x,'Operation & Maintenance',t80,f10.2,10x,f10.2/ &
         1x,'Decommissioning Fund',t80,f10.2,10x,f10.2/ &
         1x,'Fuel Charge Breakdown'// &
         5x,'Blanket & first wall',t72,f10.2,10x,f10.2/ &
         5x,'Divertors',t72,f10.2,10x,f10.2/ &
         5x,'Centrepost (TART only)',t72,f10.2,10x,f10.2/ &
         5x,'Auxiliary Heating',t72,f10.2,10x,f10.2/ &
         5x,'Actual Fuel',t72,f10.2,10x,f10.2/ &
         5x,'Waste Disposal',t72,f10.2,10x,f10.2/ &
         1x,'Total Fuel Cost',t80,f10.2,10x,f10.2// &
         1x,'Total Cost',t80,f10.2,10x,f10.2 )

      if (ifueltyp == 1) then
         call oshead(outfile,'Replaceable Components Direct Capital Cost')
         call ovarrf(outfile,'First wall direct capital cost (M$)', &
            '(fwallcst)',fwallcst)
         call ovarrf(outfile,'Blanket direct capital cost (M$)', &
            '(blkcst)',blkcst)
         if (ife /= 1) then
            call ovarrf(outfile,'Divertor direct capital cost (M$)', &
               '(divcst)',divcst)
            if (itart == 1) then
               call ovarrf(outfile,'Centrepost direct capital cost (M$)', &
                  '(cpstcst)',cpstcst)
            end if
            call ovarrf(outfile,'Plasma heating/CD system cap cost (M$)', &
               '',cdcost*fcdfuel/(1.0D0-fcdfuel))
            call ovarrf(outfile,'Fraction of CD cost --> fuel cost', &
               '(fcdfuel)',fcdfuel)
         else
            call ovarrf(outfile,'IFE driver system direct cap cost (M$)', &
               '',cdcost*fcdfuel/(1.0D0-fcdfuel))
            call ovarrf(outfile,'Fraction of driver cost --> fuel cost', &
               '(fcdfuel)',fcdfuel)
         end if
      end if

   end subroutine coelc


   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module costs_module
