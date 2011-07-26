C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: costs.f,v 3.29 1999/05/19 09:32:20 peter Exp pknight $
C  Module name    : $RCSfile: costs.f,v $
C  Version no.    : $Revision: 3.29 $
C  Creation date  : $Date: 1999/05/19 09:32:20 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE COSTS(nout,iprint)

C *** Cost accounting for a fusion power plant

C *** The direct costs are calculated based on parameters input
C *** from other sections of the code.
C *** Costs are in 1990 $, and assume first-of-a-kind components
C *** unless otherwise stated. Account 22 costs include a multiplier
C *** to account for Nth-of-a-kind cost reductions.
C *** The code is arranged in the order of the standard accounts.

      INCLUDE 'param.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'osections.h'
      INCLUDE 'blanket.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'phydat.h'
      INCLUDE 'rfp.h'
      INCLUDE 'pulse.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'ife.h'

      INTEGER iprint,nout

      EXTERNAL acc21,acc22,acc23,acc24,acc25,acc26,acchyd,acc9,coelc,
     +     oblnkl,ocosts,oheadr,oshead,ovarin,ovarre

C *** Account 21 : Structures and site facilities

      call acc21

C *** Account 22 : Fusion power island

      call acc22

C *** Account 23 : Turbine plant equipment

      call acc23

C *** Account 24 : Electric plant equipment

      call acc24

C *** Account 25 : Miscellaneous plant equipment

      call acc25

C *** Account 26 : Heat rejection system

      call acc26

C *** Hydrogran Production Plant

      call acchyd

C *** Total plant direct cost

      cdirt = c21 + c22 + c23 + c24 + c25 + c26 + chplant

C *** Account 9 : Indirect cost and project contingency

      call acc9

C *** Constructed cost

      concost = cdirt + cindrt + ccont

C *** Cost of electricity

      if ((ireactor.eq.1).and.(ipnet.eq.0)) then
         call coelc(iprint,nout)
      end if

C *** Output section

      if ((iprint.eq.0).or.(sect02.eq.0)) goto 1000

      call oheadr(nout,'Detailed Costings (M$)')
      call ovarre(nout,'Acc.22 multiplier for Nth of a kind',
     +     '(fkind)',fkind)
      call ovarin(nout,'Level of Safety Assurance','(lsa)',lsa)
      call oblnkl(nout)
      call ocosts(nout,'211',
     +     'Site improvements, facilities and land',c211)
      call ocosts(nout,'212','Reactor building',c212)
      call ocosts(nout,'213','Turbine building',c213)
      call ocosts(nout,'2141','Reactor maintenance building',c2141)
      call ocosts(nout,'2142','Warm shop',c2142)
      call ocosts(nout,'215','Tritium building',c215)
      call ocosts(nout,'216','Electrical equipment building',c216)
      call ocosts(nout,'2171','Additional buildings',c2171)
      call ocosts(nout,'2172','Control room buildings',c2172)
      call ocosts(nout,'2173','Shop and warehouses',c2173)
      call ocosts(nout,'2174','Cryogenic building',c2174)
      call oblnkl(nout)
      call ocosts(nout,'21','Total account 21 cost',c21)

      call oshead(nout,'Reactor Systems')
      call ocosts(nout,'2211','First wall',c2211)

C+**PJK 21/03/97
      if (ife.ne.1) then
         if (lblnkt.eq.1) then

            if (smstr.eq.1) then
               call ocosts(nout,'22121','Blanket beryllium',c22121)
               call ocosts(nout,'22122','Blanket lithium oxide',c22122)
               call ocosts(nout,'22123','Blanket stainless steel',
     +              c22123)
               call ocosts(nout,'22124','Blanket vanadium',c22124)
            else
               call ocosts(nout,'22121','Blanket lithium-lead',c22121)
               call ocosts(nout,'22122','Blanket lithium',c22122)
               call ocosts(nout,'22123','Blanket stainless steel',
     +              c22123)
               call ocosts(nout,'22124','Blanket vanadium',c22124)
            end if

         else
            call ocosts(nout,'22121','Blanket beryllium',c22121)
            call ocosts(nout,'22122','Blanket lithium oxide',c22122)
            call ocosts(nout,'22123','Blanket stainless steel',c22123)
            call ocosts(nout,'22124','Blanket vanadium',c22124)
         end if
      else
         call ocosts(nout,'22121','Blanket beryllium',c22121)
         call ocosts(nout,'22122','Blanket lithium oxide',c22122)
         call ocosts(nout,'22123','Blanket stainless steel',c22123)
         call ocosts(nout,'22124','Blanket vanadium',c22124)
         call ocosts(nout,'22125','Blanket carbon cloth',c22125)
         call ocosts(nout,'22126','Blanket concrete',c22126)
         call ocosts(nout,'22127','Blanket FLiBe',c22127)
      end if
C-**PJK 21/03/97
      call ocosts(nout,'2212','Blanket total',c2212)
      call ocosts(nout,'22131','Bulk shield',c22131)
      call ocosts(nout,'22132','Penetration shielding',c22132)
      call ocosts(nout,'2213','Total shield',c2213)
      call ocosts(nout,'2214','Total support structure',c2214)
      call ocosts(nout,'2215','Divertor',c2215)
      if (ifueltyp.eq.1) then
         call oblnkl(nout)
         write(nout,20)
 20      format(t2,
     +        'First wall, total blanket and divertor direct costs',/,
     +        t2,'are zero as they are assumed to be fuel costs.')
      end if

      call oblnkl(nout)
      call ocosts(nout,'221','Total account 221 cost',c221)

C+**PJK 21/03/97
      if (ife.eq.1) goto 40
C-**PJK 21/03/97

      call oshead(nout,'Magnets')
      if (itfsup.eq.0) then
C+**PJK 29/01/96
         if (itart.eq.1) then
            call ocosts(nout,'22211','Centrepost costs',c22211)
         else
            call ocosts(nout,'22211','Inner leg costs',c22211)
         end if
         call ocosts(nout,'22212','Outer leg costs',c22212)
         call ocosts(nout,'2221','TF magnet assemblies',c2221)
      else
         call ocosts(nout,'22211','TF coil conductor',c22211)
         call ocosts(nout,'22212','TF coil winding',c22212)
         call ocosts(nout,'22213','TF coil case',c22213)
         call ocosts(nout,'22214','TF intercoil structure',c22214)
         call ocosts(nout,'22215','TF coil gravity support structure',
     +        c22215)
         call ocosts(nout,'2221','TF magnet assemblies',c2221)
      end if
      call ocosts(nout,'22221','PF coil conductor',c22221)
      call ocosts(nout,'22222','PF coil winding',c22222)
      call ocosts(nout,'22223','PF coil case',c22223)
      call ocosts(nout,'22224','PF coil support structure',c22224)
      call ocosts(nout,'2222','PF magnet assemblies',c2222)
      call ocosts(nout,'2223','Cryostat assembly',c2223)
C+**PJK 30/01/96
      if ((itart.eq.1).and.(ifueltyp.eq.1)) then
         call oblnkl(nout)
         write(nout,30)
 30      format(t2,
     +        'Centrepost direct cost is zero, as it ',
     +        'is assumed to be a fuel cost.')
      end if

      call oblnkl(nout)
      call ocosts(nout,'222','Total account 222 cost',c222)

C+**PJK 21/03/97
 40   continue
C-**PJK 21/03/97

      call oshead(nout,'Power Injection')
C+**PJK 21/03/97
      if (ife.eq.1) then
         call ocosts(nout,'2231','IFE driver system',c2231)
      else
         call ocosts(nout,'2231','ECH system',c2231)
         call ocosts(nout,'2232','Lower hybrid system',c2232)
         call ocosts(nout,'2233','Neutral beam system',c2233)
         if (irfp.eq.1) then
            call ocosts(nout,'2234','Oscillating field system',c2234)
         end if
      end if
C-**PJK 21/03/97
      call oblnkl(nout)
      call ocosts(nout,'223','Total account 223 cost',c223)

      call oshead(nout,'Vacuum Systems')
      call ocosts(nout,'2241','High vacuum pumps',c2241)
      call ocosts(nout,'2242','Backing pumps',c2242)
      call ocosts(nout,'2243','Vacuum duct',c2243)
      call ocosts(nout,'2244','Valves',c2244)
      call ocosts(nout,'2245','Duct shielding',c2245)
      call ocosts(nout,'2246','Instrumentation',c2246)
      call oblnkl(nout)
      call ocosts(nout,'224','Total account 224 cost',c224)

C+**PJK 21/03/97
      if (ife.eq.1) goto 50
C-**PJK 21/03/97

      call oshead(nout,'Power Conditioning')
      call ocosts(nout,'22511','TF coil power supplies',c22511)
      call ocosts(nout,'22512','TF coil breakers',c22512)
      call ocosts(nout,'22513','TF coil dump resistors',c22513)
      call ocosts(nout,'22514','TF coil instrumentation and control',
     +     c22514)
      call ocosts(nout,'22515','TF coil bussing',c22515)
      call ocosts(nout,'2251','Total, TF coil power',c2251)
      call ocosts(nout,'22521','PF coil power supplies',c22521)
      call ocosts(nout,'22522','PF coil instrumentation and control',
     +     c22522)
      call ocosts(nout,'22523','PF coil bussing',c22523)
      call ocosts(nout,'22524','PF coil burn power supplies',c22524)
      call ocosts(nout,'22525','PF coil breakers',c22525)
      call ocosts(nout,'22526','PF coil dump resistors',c22526)
      call ocosts(nout,'22527','PF coil ac breakers',c22527)
      call ocosts(nout,'2252','Total, PF coil power',c2252)
      call ocosts(nout,'2253','Total, energy storage',c2253)
      call oblnkl(nout)
      call ocosts(nout,'225','Total account 225 cost',c225)

C+**PJK 21/03/97
 50   continue
C-**PJK 21/03/97

      call oshead(nout,'Heat Transport System')
      call ocosts(nout,'cpp','Pumps and piping system',cpp)
      call ocosts(nout,'chx','Primary heat exchanger',chx)
      call ocosts(nout,'2261','Total, reactor cooling system',c2261)
      call ocosts(nout,'cppa','Pumps, piping',cppa)
      call ocosts(nout,'chxa','Heat exchanger',chxa)
      call ocosts(nout,'2262','Total, auxiliary cooling system',c2262)
      call ocosts(nout,'2263','Total, cryogenic system',c2263)
      call oblnkl(nout)
      call ocosts(nout,'226','Total account 226 cost',c226)

      call oshead(nout,'Fuel Handling System')
      call ocosts(nout,'2271','Fuelling system',c2271)
      call ocosts(nout,'2272','Fuel processing and purification',c2272)
      call ocosts(nout,'2273','Atmospheric recovery systems',c2273)
      call ocosts(nout,'2274','Nuclear building ventilation',c2274)
      call oblnkl(nout)
      call ocosts(nout,'227','Total account 227 cost',c227)

      call oshead(nout,'Instrumentation and Control')
      call ocosts(nout,'228','Instrumentation and control',c228)

      call oshead(nout,'Maintenance Equipment')
      call ocosts(nout,'229','Maintenance equipment',c229)

      call oshead(nout,'Total Account 22 Cost')
      call ocosts(nout,'22','Total account 22 cost',c22)

      call oshead(nout,'Turbine Plant Equipment')
      call ocosts(nout,'23','Turbine plant equipment',c23)

      call oshead(nout,'Electric Plant Equipment')
      call ocosts(nout,'241','Switchyard equipment',c241)
      call ocosts(nout,'242','Transformers',c242)
      call ocosts(nout,'243','Low voltage equipment',c243)
      call ocosts(nout,'244','Diesel backup equipment',c244)
      call ocosts(nout,'245','Auxiliary facilities',c245)
      call oblnkl(nout)
      call ocosts(nout,'24','Total account 24 cost',c24)

      call oshead(nout,'Miscellaneous Plant Equipment')
      call ocosts(nout,'25','Miscellaneous plant equipment',c25)

      call oshead(nout,'Heat Rejection System')
      call ocosts(nout,'26','Heat rejection system',c26)

      if (ihplant.ne.0) then
         call oshead(nout,'Hydrogen Production')
         call ocosts(nout,' ','Hydrogen production plant',chplant)
      end if

      call oshead(nout,'Plant Direct Cost')
      call ocosts(nout,'2','Plant direct cost',cdirt)

      call oshead(nout,'Reactor Core Cost')
      call ocosts(nout,'2','Reactor core cost',crctcore)

      call oshead(nout,'Indirect Cost')
      call ocosts(nout,'c9','Indirect cost',cindrt)

      call oshead(nout,'Total Contingency')
      call ocosts(nout,'ccont','Total contingency',ccont)

      call oshead(nout,'Constructed Cost')
      call ocosts(nout,'concost','Constructed cost',concost)

      if (ireactor.eq.1) then
         call oshead(nout,'Interest during Construction')
         call ocosts(nout,' ','Interest during construction',moneyint)

         call oshead(nout,'Total Capital Investment')
         call ocosts(nout,'capcost','Total capital investment',capcost)
      end if

 1000 continue

      return
      end
C_______________________________________________________________________
      SUBROUTINE COELC(iprint,nout)

C *** Routine to calculate the cost of electricity
C *** 
C *** Lifetimes are in years (19/05/99 now calculated in AVAIL)
C *** Annual costs are in megadollars/year
C *** Electricity costs are in millidollars/kWh
C *** Other costs are in megadollars
C *** 
C *** All values are based on 1990 dollars

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'divrt.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'cost.h'
      INCLUDE 'osections.h'
      INCLUDE 'pulse.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'

      DOUBLE PRECISION
     +     anncap,anncdr,anncp,anndecom,anndiv,annfuel,annfuelt,
     +     annfwbl,annoam,annoam1,anntot,annwst,cirpowfr,coecdr,
     +     coecp,coedecom,coediv,coefuel,coefwbl,coewst,crfcdr,
     +     crfcp,crfdiv,crffwbl,fefcdr,fefcp,fefdiv,feffwbl,
     +     fwbllife,kwhpy

      INTEGER iprint,nout

      EXTERNAL ocosts,oheadr,oshead,osubhd

C *** Number of kWh generated each year

      kwhpy = 1.0D3 * pnetelmw * (24.0D0*365.0D0) * cfactr

C *** Costs due to reactor plant
C *** ==========================

C *** Interest on construction costs

      moneyint = concost * (fcap0 - 1.0D0)

C *** Capital costs

      capcost = concost + moneyint

C *** Annual cost of plant capital cost

      anncap = capcost * fcr0

C *** Cost of electricity due to plant capital cost

      coecap = 1.0D9 * anncap / kwhpy

C *** Costs due to first wall and blanket renewal
C *** ===========================================

C *** Operational life

C+**PJK 10/05/99 (calculation moved to AVAIL)
      fwbllife = bktlife
      
C *** Compound interest factor

      feffwbl = (1.0D0 + ratecdol)**fwbllife

C *** Capital recovery factor

      crffwbl = (feffwbl*ratecdol) / (feffwbl-1.0D0)

C *** Annual cost of replacements

      annfwbl = (fwallcst+blkcst) *
     +     (1.0D0+cfind(lsa)) * fcap0cp * crffwbl

C *** Cost of electricity due to first wall/blanket replacements

      coefwbl = 1.0D9 * annfwbl / kwhpy

C *** Costs due to divertor renewal
C *** =============================

C+**PJK 21/03/97
      if (ife.eq.1) then

         anndiv = 0.0D0
         coediv = 0.0D0

      else

C *** Compound interest factor

         fefdiv = (1.0D0 + ratecdol)**divlife

C *** Capital recovery factor

         crfdiv = (fefdiv*ratecdol) / (fefdiv-1.0D0)

C *** Annual cost of replacements

         anndiv = divcst *
     +        (1.0D0+cfind(lsa)) * fcap0cp * crfdiv

C *** Cost of electricity due to divertor replacements

         coediv = 1.0D9 * anndiv / kwhpy

      end if
C-**PJK 21/03/97

C *** Costs due to centrepost renewal
C *** ===============================

C+**PJK 30/01/96, 21/03/97
      if ((itart.eq.1).AND.(ife.ne.1)) then

C *** Compound interest factor

         fefcp = (1.0D0 + ratecdol)**cplife

C *** Capital recovery factor

         crfcp = (fefcp*ratecdol) / (fefcp-1.0D0)

C *** Annual cost of replacements

         anncp = cpstcst *
     +        (1.0D0+cfind(lsa)) * fcap0cp * crfcp

C *** Cost of electricity due to centrepost replacements

         coecp = 1.0D9 * anncp / kwhpy

      else

         anncp = 0.0D0
         coecp = 0.0D0

      end if
C-**PJK 30/01/96

C *** Costs due to partial current drive system renewal
C *** =================================================

C *** Compound interest factor

      fefcdr = (1.0D0 + ratecdol)**cdrlife

C *** Capital recovery factor

      crfcdr = (fefcdr*ratecdol) / (fefcdr-1.0D0)

C *** Annual cost of replacements

      if (ifueltyp.eq.0) then
         anncdr = 0.0D0
      else
         anncdr = cdcost * fcdfuel/(1.0D0-fcdfuel) *
     +        (1.0D0+cfind(lsa)) * fcap0cp * crfcdr
      end if

C *** Cost of electricity due to current drive system replacements

      coecdr = 1.0D9 * anncdr / kwhpy

C *** Costs due to operation and maintenance
C *** ======================================

C *** Annual cost of operation and maintenance

      annoam = ucoam(lsa) * sqrt(pnetelmw/1200.0D0)

cC *** Additional cost due to pulsed reactor thermal storage
cC *** See F/MPE/MOD/CAG/PROCESS/PULSE/0008
c
c      if (lpulse.eq.1) then
c         if (istore.eq.1) then
c            annoam1 = 51.0D0
c         else if (istore.eq.2) then
c            annoam1 = 22.2D0
c         else
c            continue
c         end if
c
cC *** Scale with net electric power
c
c         annoam1 = annoam1 * pnetelmw/1200.0D0
c
cC *** It is necessary to convert from 1992 pounds to 1990 dollars
cC *** Reasonable guess for the exchange rate + inflation factor
cC *** inflation = 5% per annum; exchange rate = 1.5 dollars per pound
c
c         annoam1 = annoam1 * 1.36D0
c
c         annoam = annoam + annoam1
c
c      end if

C *** Cost of electricity due to operation and maintenance

      coeoam = 1.0D9 * annoam / kwhpy

C *** Costs due to reactor fuel
C *** =========================

C *** Annual cost of fuel

C+**PJK 08/12/95 Added annual cost of He3 (assume D cost negligible)
C+**PJK 21/03/97 IFE target costs
      if (ife.ne.1) then
         if (idhe3.eq.0) then
            annfuel = ucfuel * pnetelmw/1200.0D0
         else
            annfuel = 1.0D-6 * fhe3 * wtgpd * 1.0D-3 * uche3 *
     +           365.0D0 * cfactr
         end if
      else
         annfuel = 1.0D-6 * uctarg * reprat * 3.1536D7 * cfactr
      end if
C-**PJK 21/03/97

C *** Cost of electricity due to reactor fuel

      coefuel = 1.0D9 * annfuel / kwhpy

C *** Costs due to waste disposal
C *** ===========================

C *** Annual cost of waste disposal

      annwst = ucwst(lsa) * sqrt(pnetelmw/1200.0D0)

C *** Cost of electricity due to waste disposal

      coewst = 1.0D9 * annwst / kwhpy

C *** Costs due to decommissioning fund
C *** =================================

C *** Annual contributions to fund for decommissioning
C *** A fraction decomf of the construction cost is set aside for
C *** this purpose at the start of the plant life.
C *** Final factor takes into account inflation over the plant lifetime
C *** (suggested by Tim Hender 07/03/96)
C *** Difference (dintrt) between borrowing and saving interest rates is
C *** included, along with the possibility of completing the fund dtlife
C *** years before the end of the plant's lifetime

      anndecom = decomf * concost * fcr0 /
     +     (1.0D0+ratecdol-dintrt)**(tlife-dtlife)

C *** Cost of electricity due to decommissioning fund

      coedecom = 1.0D9 * anndecom / kwhpy

C *** Total costs
C *** ===========

C *** Annual costs due to 'fuel-like' components

      annfuelt = annfwbl + anndiv + anncdr + anncp + annfuel + annwst

C *** Total cost of electricity due to 'fuel-like' components

      coefuelt = coefwbl + coediv + coecdr + coecp + coefuel + coewst

C *** Total annual costs

      anntot = anncap + annfuelt + annoam + anndecom

C *** Total cost of electricity

      coe = coecap + coefuelt + coeoam + coedecom

C *** Output section
C *** ==============

      if ((iprint.eq.0).or.(sect01.eq.0)) goto 1000

C *** Recirculating power fraction

      cirpowfr = (pgrossmw - pnetelmw) / pgrossmw

      call oheadr(nout,'Power Reactor Costs')
      call ocosts(nout,' ','Net electric power (MW)',pnetelmw)
      call ocosts(nout,' ','Gross electric power (MW)',pgrossmw)
      call ocosts(nout,' ','High grade thermal power (MW)',pthermmw)
      call ocosts(nout,' ',
     +     'Balance-of-plant recirc. power fraction',fgrosbop)
      call ocosts(nout,' ','Total recirculating power fraction',
     +     cirpowfr)
      call ocosts(nout,' ','First wall / blanket life (years)',
     +     fwbllife)
C+**PJK 21/03/97
      if (ife.ne.1) then
         call ocosts(nout,' ','Divertor life (years)',divlife)
C+**PJK 30/01/96
         if (itart.eq.1) then
            call ocosts(nout,' ','Centrepost life (years)',cplife)
         end if
      end if
C-**PJK 21/03/97

      call ocosts(nout,' ','Cost of electricity (m$/kWh)',coe)

      call osubhd(nout,'Power Generation Costs :')

C+**PJK 30/01/96 Added anncp,coecp
      write(nout,200)
     +     anncap,coecap,
     +     annoam,coeoam,
     +     anndecom,coedecom,
     +     annfwbl,coefwbl,
     +     anndiv,coediv,
     +     anncp,coecp,
     +     anncdr,coecdr,
     +     annfuel,coefuel,
     +     annwst,coewst,
     +     annfuelt,coefuelt,
     +     anntot,coe

 200  format(
     +     t35,'Annual Costs, M$       COE, m$/kWh'//
     +     1x,'Capital Investment                ',f10.2,10x,f10.2/
     +     1x,'Operation & Maintenance           ',f10.2,10x,f10.2/
     +     1x,'Decommissioning Fund              ',f10.2,10x,f10.2/
     +     1x,'Fuel Charge Breakdown'//
     +     5x,'Blanket & first wall    ',f10.2,10x,f10.2/
     +     5x,'Divertors               ',f10.2,10x,f10.2/
     +     5x,'Centrepost (TART only)  ',f10.2,10x,f10.2/
     +     5x,'Auxiliary Heating       ',f10.2,10x,f10.2/
     +     5x,'Actual Fuel             ',f10.2,10x,f10.2/
     +     5x,'Waste Disposal          ',f10.2,10x,f10.2/
     +     1x,'Total Fuel Cost                   ',f10.2,10x,f10.2//
     +     1x,'Total Cost                        ',f10.2,10x,f10.2)

      if (ifueltyp.eq.1) then
         call oshead(nout,'Replaceable Components Direct Capital Cost')
         call ocosts(nout,' ','First wall',fwallcst)
         call ocosts(nout,' ','Blanket',blkcst)
C+**PJK 21/03/97
         if (ife.ne.1) then
            call ocosts(nout,' ','Divertor',divcst)
C+**PJK 30/01/96
            if (itart.eq.1) then
               call ocosts(nout,' ','Centrepost',cpstcst)
            end if
C-**PJK 30/01/96
         end if
C-**PJK 21/03/97

C+**PJK 21/03/97
         if (ife.ne.1) then
            call ocosts(nout,' ','Plasma heating/CD system',
     +           cdcost*fcdfuel/(1.0D0-fcdfuel))
            call ocosts(nout,' ','Fraction of CD cost --> fuel cost',
     +           fcdfuel)
         else
            call ocosts(nout,' ','IFE driver system',
     +           cdcost*fcdfuel/(1.0D0-fcdfuel))
            call ocosts(nout,' ',
     +           'Fraction of driver cost --> fuel cost',fcdfuel)
         end if
C-**PJK 21/03/97

      end if

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE ACC21

C *** Account 21 : Structures and site facilities

C *** Building costs are scaled with volume according to algorithms
C *** developed from TFCX, TFTR, and commercial power plant buildings.
C *** Costs include equipment, materials and installation labour, but
C *** no engineering or construction management.  The general form of
C *** the cost algorithm is cost=ucxx*volume**expxx.  Allowances are
C *** used for site improvements and for miscellaneous buildings and
C *** land costs.

C *** Building volumes (m**3) are input from bldgs module
C *** rbvol    : reactor building volume
C *** rmbvol   : reactor maintenance building volume
C *** wsvol    : warm shops volume
C *** triv     : tritium building volume
C *** elevol   : electrical equipment building volume
C ***            (includes IFE driver building if relevant)
C *** convol   : control room building volume
C *** cryvol   : cryogenic plant building volume
C *** admvol   : administration building volume
C *** shovol   : shops and warehouses volume

C *** Unit costs in M$ / vol**exprb :
C *** cland    : Land cost (M$) [19.2]
C *** csi      : allowance for site costs (M$) [16.0]
C *** cturbb   : turbine building (M$) [380.0]
C *** ucrb     : reactor building [400.0]
C *** exprb    : costing exponential for building volume [1.0]
C *** ucmb     : reactor maintenance [260.0]
C *** ucws     : active assembly shop [460.0]
C *** uctr     : tritium building [370.0]
C *** ucel     : electrical equipment buildings [380.0]
C *** ucad     : administration buildings [180.0]
C *** ucco     : control buildings [350.0]
C *** ucsh     : shops and warehouses [115.0]
C *** uccr     : cryogenic building [460.0]

C+**PJK 22/12/93 Converted exprb to local parameter
      DOUBLE PRECISION exprb
      PARAMETER (exprb = 1.0D0)

      INCLUDE 'param.h'
      INCLUDE 'bldgvol.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'phydat.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.6800D0
      cmlsa(2) = 0.8400D0
      cmlsa(3) = 0.9200D0
      cmlsa(4) = 1.0000D0

C *** Account 211 : Site improvements, facilities and land
C *** N.B. Land unaffected by LSA

      c211 = csi*cmlsa(lsa) + cland

C *** Account 212 : Reactor building

      c212 = 1.d-6 * ucrb * rbvol**exprb * cmlsa(lsa)

C+**PJK 17/12/93 Commented out use of ihts (coolant type switch)
C+**PJK 17/12/93 if (ihts.eq.1) c212 = c212 * 1.15D0

C *** Account 213 : Turbine building

C+**PJK 14/11/97 Set turbine building cost to zero if no electricity
C+**PJK 14/11/97 is being produced

      if (ireactor.eq.1) then
         c213 = cturbb * cmlsa(lsa)
      else
         c213 = 0.0D0
      end if

C *** Account 214 : Reactor maintenance and warm shops buildings

      c2141 = 1.d-6 * ucmb * rmbvol**exprb * cmlsa(lsa)
      c2142 = 1.d-6 * ucws * wsvol**exprb * cmlsa(lsa)
      c214 = c2141 + c2142

C *** Account 215 : Tritium building

      c215 = 1.d-6 * uctr * triv**exprb * cmlsa(lsa)

C *** Account 216 : Electrical equipment building

      c216 = 1.d-6 * ucel * elevol**exprb * cmlsa(lsa)

C *** Account 217 : Other buildings
C *** Includes administration, control, shops, cryogenic
C *** plant and an allowance for miscellaneous structures

      c2171 = 1.d-6 * ucad * admvol**exprb * cmlsa(lsa)
      c2172 = 1.d-6 * ucco * convol**exprb * cmlsa(lsa)
      c2173 = 1.d-6 * ucsh * shovol**exprb * cmlsa(lsa)
      c2174 = 1.d-6 * uccr * cryvol**exprb * cmlsa(lsa)
      c217 = c2171 + c2172 + c2173 + c2174

C *** Total for Account 21

      c21 = c211 + c212 + c213 + c214 + c215 + c216 + c217

      end
C______________________________________________________________________
      SUBROUTINE ACC22

C *** Account 22 : Fusion power island (the tokamak itself plus
C *** auxiliary power systems, etc.)

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      EXTERNAL acc221,acc222,acc223,acc224,acc225,acc226,acc227,
     +     acc228,acc229

C *** Account 221 : Reactor

      call acc221

C *** Account 222 : Magnets

      call acc222

C *** Account 223 : Power injection

      call acc223

C *** Account 224 : Vacuum system

      call acc224

C *** Account 225 : Power conditioning

      call acc225

C *** Account 226 : Heat transport system

      call acc226

C *** Account 227 : Fuel handling

      call acc227

C *** Account 228 : Instrumentation and control

      call acc228

C *** Account 229 : Maintenance equipment

      call acc229

C *** Reactor core costs

      crctcore = c221 + c222 + c223

C *** Total account 22

      c22 = c221 + c222 + c223 + c224 + c225 + c226 + c227 + c228 + c229

      end
C______________________________________________________________________
      SUBROUTINE ACC221

C *** Account 221 : Reactor

C *** Includes the first wall, blanket, shield, support structure and
C *** divertor plates.
C *** If ifueltyp = 1, the first wall, blanket and divertor costs are
C *** treated as fuel costs, rather than as capital costs

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      EXTERNAL acc2211,acc2212,acc2213,acc2214,acc2215

C *** Account 221.1 : First wall

      call acc2211

C *** Account 221.2 : Blanket

      call acc2212

C *** Account 221.3 : Shield

      call acc2213

C *** Account 221.4 : Reactor structure

      call acc2214

C *** Account 221.5 : Divertor

      call acc2215

C *** Total account 221

      c221 = c2211 + c2212 + c2213 + c2214 + c2215

      end
C______________________________________________________________________
      SUBROUTINE ACC2211

C *** Account 221.1 : First wall

C *** First wall cost is scaled linearly with surface area from TFCX.
C *** If ifueltyp = 1, the first wall cost is treated as a fuel cost,
C *** rather than as a capital cost.

C *** fwarea   : first wall area (m2)

C *** ucfwa    : first wall armour cost $/m2 [6.e4]
C *** ucfws    : first wall structure cost $/m2 [5.3e4]
C *** ucfwps   : first wall passive stabilizer cost $/m2 [1.d7]
C *** fkind    : cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 21/03/97
C *** ucblss   : stainless steel cost $/kg [90.0]
C *** uccarb   : carbon cloth cost $/kg [50.0]
C *** ucblli2o : lithium oxide cost $/kg [600.0]
C *** ucconc   : concrete cost $/kg [0.1]
C *** fwmatm(J,I) : mass of material I in region J of first wall
C-**PJK 21/03/97

      INCLUDE 'build.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.5000D0
      cmlsa(2) = 0.7500D0
      cmlsa(3) = 0.8750D0
      cmlsa(4) = 1.0000D0

C+**PJK 17/12/93 Why isn't ucfwps multiplied by fwarea?

C+**PJK 21/03/97
      if (ife.ne.1) then
         c2211 = 1.d-6 * (ucfwa+ucfws) * fwarea + 1.d-6*ucfwps *
     +        cmlsa(lsa)
      else
         c2211 = 1.0D-6 * cmlsa(lsa) * (
     +        ucblss   * (fwmatm(1,1)+fwmatm(2,1)+fwmatm(3,1)) +
     +        uccarb   * (fwmatm(1,2)+fwmatm(2,2)+fwmatm(3,2)) +
     +        ucblli2o * (fwmatm(1,4)+fwmatm(2,4)+fwmatm(3,4)) +
     +        ucconc   * (fwmatm(1,5)+fwmatm(2,5)+fwmatm(3,5)) )
      end if
C-**PJK 21/03/97

      c2211 = fkind * c2211

      if (ifueltyp.eq.1) then
         fwallcst = c2211
         c2211 = 0.0D0
      else
         fwallcst = 0.0D0
      end if

      end
C______________________________________________________________________
      SUBROUTINE ACC2212

C *** Account 221.2 : Blanket

C *** If ifueltyp = 1, the blanket cost is treated as a fuel cost,
C *** rather than as a capital cost.

C *** whtblss  : structural case steel mass
C *** whtblbe  : beryllium mass
C *** wtblli2o : Li2O breeder mass
C *** wtbllipb : Li-Pb breeder mass
C *** whtblli  : Li breeder mass
C *** whtblvd  : Vd mass

C *** Costs are $/kg
C *** ucblbe   : beryllium [260.0]
C *** ucblli2o : Li_2O     [600.0]
C *** ucbllipb : Li-Pb     [10.3]
C *** ucblli   : Li        [875.0]
C *** ucblss   : stainless steel [90.0]
C *** ucblvd   : vanadium  [200.0]
C *** fkind    : cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 21/03/97
C *** uccarb   : carbon cloth [50.0]
C *** ucconc   : concrete [0.1]
C *** ucflib   : FLiBe [84.0]
C *** mflibe   : Total mass of FLiBe (kg)
C *** blmatm(J,I) : mass of material I in region J of blanket
C-**PJK 21/03/97

C+**PJK 22/12/93 Engineering and installation factor (1.2) removed,
C+**PJK 22/12/93 so blanket materials unit costs are all * 1.2 instead.

      INCLUDE 'blanket.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.5000D0
      cmlsa(2) = 0.7500D0
      cmlsa(3) = 0.8750D0
      cmlsa(4) = 1.0000D0

C+**PJK 21/03/97
      if (ife.ne.1) then
         if (lblnkt.eq.1) then
            if (smstr.eq.1) then
C *** Solid blanket (Li2O + Be)
               c22121 = 1.0d-6 * whtblbe * ucblbe
               c22122 = 1.0d-6 * wtblli2o * ucblli2o

            else

C *** Liquid blanket (LiPb + Li)
               c22121 = 1.0d-6 * wtbllipb * ucbllipb
               c22122 = 1.0d-6 * whtblli * ucblli

            end if

         else

            c22121 = 1.d-6 * whtblbe * ucblbe
            c22122 = 1.d-6 * wtblli2o * ucblli2o

         end if

         c22123 = 1.d-6 * whtblss * ucblss
         c22124 = 1.d-6 * whtblvd * ucblvd
         c22125 = 0.0D0
         c22126 = 0.0D0
         c22127 = 0.0D0

      else

C *** IFE blanket; materials present are Li2O, steel, carbon, concrete
C *** and FLiBe

         c22121 = 0.0D0
         c22122 = 1.d-6 * wtblli2o * ucblli2o
         c22123 = 1.d-6 * whtblss * ucblss
         c22124 = 0.0D0
         c22125 = 1.d-6 * uccarb * (blmatm(1,2)+blmatm(2,2)+blmatm(3,2))
         c22126 = 1.d-6 * ucconc * (blmatm(1,5)+blmatm(2,5)+blmatm(3,5))
         c22127 = 1.d-6 * ucflib * mflibe

      end if
C-**PJK 21/03/97

      c22121 = fkind * c22121 * cmlsa(lsa)
      c22122 = fkind * c22122 * cmlsa(lsa)
      c22123 = fkind * c22123 * cmlsa(lsa)
      c22124 = fkind * c22124 * cmlsa(lsa)
C+**PJK 21/03/97
      c22125 = fkind * c22125 * cmlsa(lsa)
      c22126 = fkind * c22126 * cmlsa(lsa)
      c22127 = fkind * c22127 * cmlsa(lsa)

      c2212 = c22121 + c22122 + c22123 + c22124 + c22125 + c22126 +
     +     c22127
C-**PJK 21/03/97

      if (ifueltyp.eq.1) then
         blkcst = c2212
         c2212 = 0.0D0
      else
         blkcst = 0.0D0
      end if

      end
C______________________________________________________________________
      SUBROUTINE ACC2213

C *** Account 221.3 : Shield

C *** Provision for costing shield based on smeared density
C *** and unit cost

C *** whtshld  : shield mass (kg)
C *** wpenshld : mass of bulk steel shield around penetrations (kg)

C *** Material costs are scaled linearly with mass, ($/kg)
C *** ucshld   : structural steel [32.]
C *** ucpens   : penetration shield [32.]
C *** fkind    : cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 21/03/97
C *** uccarb   : carbon cloth [50.0]
C *** ucblli2o : lithium oxide [50.0]
C *** ucconc   : concrete [0.1]
C *** shmatm(J,I) : mass of material I in region J of shield
C-**PJK 21/03/97

      INCLUDE 'fwblsh.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.5000D0
      cmlsa(2) = 0.7500D0
      cmlsa(3) = 0.8750D0
      cmlsa(4) = 1.0000D0

C+**PJK 21/03/97
      if (ife.ne.1) then
         c22131 = 1.0D-6 * whtshld * ucshld * cmlsa(lsa)
      else
         c22131 = 1.0D-6 * cmlsa(lsa) * (
     +        ucshld *   (shmatm(1,1)+shmatm(2,1)+shmatm(3,1)) +
     +        uccarb *   (shmatm(1,2)+shmatm(2,2)+shmatm(3,2)) +
     +        ucblli2o * (shmatm(1,4)+shmatm(2,4)+shmatm(3,4)) +
     +        ucconc *   (shmatm(1,5)+shmatm(2,5)+shmatm(3,5)) )
      end if
C-**PJK 21/03/97

      c22131 = fkind * c22131

C *** Penetration shield assumed to be typical steel plate

C+**PJK 21/03/97
      if (ife.ne.1) then
         c22132 = 1.0D-6 * wpenshld * ucpens * cmlsa(lsa)
      else
         c22132 = 0.0D0
      end if
C-**PJK 21/03/97

      c22132 = fkind * c22132

      c2213 = c22131 + c22132

      end
C______________________________________________________________________
      SUBROUTINE ACC2214

C *** Account 221.4 : Reactor structure

C *** Structural elements for the configuration costed as
C *** standard steel elements

C *** gsmass   : reactor island support (kg)

C *** ucgss    : cost of structure ($/kg) [35.]
C *** fkind    : cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'struccom.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.6700D0
      cmlsa(2) = 0.8350D0
      cmlsa(3) = 0.9175D0
      cmlsa(4) = 1.0000D0

      c2214 = 1.0D-6 * gsmass * ucgss * cmlsa(lsa)
      c2214 = fkind * c2214

      end
C______________________________________________________________________
      SUBROUTINE ACC2215

C *** Account 221.5 : Divertor

C *** Cost of divertor blade scaled linearly with surface area from
C *** TFCX. Assumes graphite armour brazed to water cooled machined
C *** copper substrate; Tenth of a kind engineering and installation
C *** assumed.
C *** If ifueltyp = 1, the divertor cost is treated as a fuel cost,
C *** rather than as a capital cost.

C *** divsur   : divertor surface area (m2)

C *** ucdiv    : cost of divertor blade ($/m2) [2.8e5]
C *** fkind    : cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'divrt.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
C+**PJK 21/03/97
      INCLUDE 'ife.h'

      if (ife.ne.1) then
         c2215 = 1.0D-6 * divsur * ucdiv
         c2215 = fkind * c2215

         if (ifueltyp.eq.1) then
            divcst = c2215
            c2215 = 0.0D0
         else
            divcst = 0.0D0
         end if

      else

         c2215 = 0.0D0
         divcst = 0.0D0

      end if
C-**PJK 21/03/97

      end
C______________________________________________________________________
      SUBROUTINE ACC222

C *** Account 222 : Magnets, including cryostat

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'

      EXTERNAL acc2221,acc2222,acc2222a,acc2223

      if (ife.eq.1) then
         c222 = 0.0D0
         goto 1000
      end if

C *** Account 222.1 : TF magnet assemblies

      call acc2221

C *** Account 222.2 : PF magnet assemblies

C+**PJK 04/03/96
      if (irfp.eq.0) then
         call acc2222
      else
         call acc2222a
      end if

C *** Account 222.3 : Cryostat

      call acc2223

C *** Total account 222

      c222 = c2221 + c2222 + c2223 

 1000 continue

      end
C______________________________________________________________________
      SUBROUTINE ACC2221

C *** Account 222.1 : TF magnet assemblies

C *** Copper magnets are costed from the TFCX data base ($/kg)
C *** Superconductor magnets are costed using a new method devised
C *** by R. Hancox under contract to Culham Laboratory, January 1994.

C *** If ifueltyp = 1, the TART centrepost cost is treated as a fuel
C *** cost, rather than as a capital cost.

C *** TF coils (copper)
C *** whtcp    : Mass of the TF coil inner legs (kg)
C *** whttflgs : Mass of the TF coil outer legs (kg)
C *** 
C *** uccpcl1  : High strength tapered copper (inner legs) [250.0]
C *** uccpclb  : Copper plate coils (outer legs) [150.0]
C *** 
C *** TF coils (superconductor)
C *** aintmass : Intercoil support mass (kg)
C *** clgsmass : Total mass of (coil,dewar and ICS) gravity support (kg)
C *** tfleng   : Centreline length (circumference) of one tf coil (m)
C *** tfno     : Number of tf coils
C *** turnstf  : Number of turns per coil
C *** whtcas   : Mass of tf coil case (kg)
C *** whtconcu : Mass of copper in tf coil winding pack (kg)
C *** whtconsc : Mass of superconductor in tf coil winding pack (kg)
C *** 
C *** cconfix  : Fixed cost of superconducting cable ($/m) [80.]
C *** cconshtf : Unit cost of steel conduit/sheath ($/m) [75.]
C *** uccase   : External case material cost ($/kg) [50.]
C *** uccu     : Cost for copper in superconducting cable ($/kg) [75.]
C *** ucgss    : Gravity support structure mass cost ($/kg) [35.]
C *** ucint    : Inter-coil structure cost ($/kg) [35.]
C *** ucsc     : Superconductor cost ($/kg) [600., 600., 300.]
C *** ucwindtf : Winding cost ($/m) [480.]

C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'struccom.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      DOUBLE PRECISION costtfsc,costtfcu,costwire,ctfconpm

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.6900D0
      cmlsa(2) = 0.8450D0
      cmlsa(3) = 0.9225D0
      cmlsa(4) = 1.0000D0

      if (itfsup.eq.0) then

C *** Resistive TF coils

C *** Account 222.1.1 : Inner TF coil legs

         c22211 = 1.0D-6 * whtcp * uccpcl1 * cmlsa(lsa)
         c22211 = fkind * c22211

C+**PJK 30/01/96 TART centrepost is/is not a fuel cost

         cpstcst = 0.0D0
         if ((itart.eq.1).and.(ifueltyp.eq.1)) then
            cpstcst = c22211
            c22211 = 0.0D0
         end if

C *** Account 222.1.2 : Outer TF coil legs

         c22212 = 1.0D-6 * whttflgs * uccpclb * cmlsa(lsa)
         c22212 = fkind * c22212

C *** Total (copper) TF coil costs

         c2221 = c22211 + c22212

      else

C *** Superconducting TF coils

C *** Account 222.1.1 : Conductor

C+**PJK 01/03/94 Amendments suggested in R. Hancox's report
C+**PJK 01/03/94 of 4th February 1994, under contract to Culham

C *** Superconductor ($/m)

         costtfsc = ucsc(isumattf) * whtconsc / (tfleng*turnstf)

C *** Copper ($/m)

         costtfcu = uccu * whtconcu / (tfleng*turnstf)

C *** Total cost/metre of superconductor and copper wire

         costwire = costtfsc + costtfcu

C *** Total cost/metre of conductor (including sheath and fixed costs)

         ctfconpm = costwire + cconshtf + cconfix

C *** Total conductor costs

         c22211 = 1.0D-6 * ctfconpm * tfno * tfleng * turnstf
         c22211 = fkind * c22211 * cmlsa(lsa)

C *** Account 222.1.2 : Winding

         c22212 = 1.d-6 * ucwindtf * tfno * tfleng * turnstf
         c22212 = fkind * c22212 * cmlsa(lsa)

C *** Account 222.1.3 : Case

         c22213 = 1.d-6 * (whtcas*uccase) * tfno
         c22213 = fkind * c22213 * cmlsa(lsa)

C *** Account 222.1.4 : Intercoil structure

         c22214 = 1.d-6 * aintmass * ucint
         c22214 = fkind * c22214 * cmlsa(lsa)

C *** Account 222.1.5 : Gravity support structure

         c22215 = 1.d-6 * clgsmass * ucgss
         c22215 = fkind * c22215 * cmlsa(lsa)

C *** Total (superconducting) TF coil costs

         c2221 = c22211 + c22212 + c22213 + c22214 + c22215

      end if

      end
C______________________________________________________________________
      SUBROUTINE ACC2222

C *** Account 222.2 : PF magnet assemblies

C *** whtpf    : Total mass of winding packs (kg)
C *** whtpfs   : Total mass of cases (kg)
C *** fncmass  : Gravity pf fence mass (kg)
C *** sccufac  : Ratio of superconductor to copper in the cable at a
C ***            magnetic field of 1T [0.0188]
C *** 
C *** uccu     : Cost for copper in superconducting cable ($/kg) [75.]
C *** ucsc     : Superconductor cost ($/kg) [600., 600., 300.]
C *** cconfix  : Fixed cost of superconducting cable ($/m) [80.]
C *** cconshpf : Unit cost of steel conduit/sheath ($/m) [70.]
C *** ucwindpf : Winding cost ($/m) [465.]
C *** uccase   : Unit cost of case & closeout, $/kg of case [50.]
C *** ucfnc    : Unit cost of outer PF fence support ($/kg) [35.] 
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'struccom.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'build.h'

      DOUBLE PRECISION costpfcu,costpfsc,costpfsh,costwire,cpfconpm,
     +     pfwndl,sccufac1,twopi

      INTEGER i,npf

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.6900D0
      cmlsa(2) = 0.8450D0
      cmlsa(3) = 0.9225D0
      cmlsa(4) = 1.0000D0

      twopi = 8.0D0 * ATAN(1.0D0)

C *** Total length of PF coil windings (m)

      pfwndl = 0.0D0
      do 10 i = 1,nohc
         pfwndl = pfwndl + twopi*rpf(i)*turns(i)
 10   continue

C *** Account 222.2.1 : Conductor

C+**PJK 13/01/94 New algorithm, taking into account the fact that the
C+**PJK 13/01/94 superconductor/copper ratio in the conductor is
C+**PJK 13/01/94 proportional to the maximum field that each coil will
C+**PJK 13/01/94 experience. OH and PF coils are treated similarly.
C+**PJK 13/01/94 Maximum values for current, current density and field
C+**PJK 13/01/94 are used.
C+**PJK 13/01/94 R. Hancox, January 1994, under contract to Culham.

C *** The following lines take care of resistive coils.
C *** costpfsh is the cost per metre of the steel conduit/sheath around
C *** each superconducting cable (so is zero for resistive coils)

      if (ipfres.eq.1) then
         sccufac1 = sccufac
         sccufac = 0.0D0
         costpfsh = 0.0D0
      else
         costpfsh = cconshpf
      end if

C *** Non-OH coils

      if (iohcl.eq.1) then
         npf = nohc-1
      else
         npf = nohc
      end if

      c22221 = 0.0D0

      do 20 i = 1,npf

C *** Superconductor ($/m)

         costpfsc = ucsc(isumatpf) * (sccufac*bpf(i)) *
     +        abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcond(isumatpf)

C *** Copper ($/m)

         costpfcu = uccu * (1.0D0-sccufac*bpf(i)) *
     +        abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper

C *** Total cost/metre of superconductor and copper wire

         costwire = costpfsc + costpfcu

C *** Total cost/metre of conductor (including sheath and fixed costs)

         cpfconpm = costwire + costpfsh + cconfix

C *** Total account 222.2.1 (PF coils excluding OH coil)

         c22221 = c22221 + (1.0D-6 * twopi * rpf(i) * turns(i) *
     +        cpfconpm)

 20   continue

C *** OH coil

      if (iohcl.eq.1) then

C *** Superconductor ($/m)

         costpfsc = ucsc(isumatpf) *
     +        (sccufac * max(abs(bmaxoh),abs(bmaxoh0)) ) *
     +        abs(ric(nohc)/turns(nohc))*1.0D6 /
     +        max(abs(cohbop),abs(coheof)) * dcond(isumatpf)

C *** Copper ($/m)

         costpfcu = uccu *
     +        (1.0D0 - sccufac*max(abs(bmaxoh),abs(bmaxoh0)) ) *
     +        abs(ric(nohc)/turns(nohc))*1.0D6 /
     +        max(abs(cohbop),abs(coheof)) * dcopper

C *** Total cost/metre of superconductor and copper wire (OH coil)

         costwire = costpfsc + costpfcu

C *** Total cost/metre of conductor (including sheath and fixed costs)

         cpfconpm = costwire + costpfsh + cconfix

C *** Total account 222.2.1 (PF+OH coils)

         c22221 = c22221 + (1.0D-6 * twopi * rpf(nohc) * turns(nohc) *
     +        cpfconpm)

      end if

      c22221 = fkind * c22221 * cmlsa(lsa)

C *** Account 222.2.2 : Winding

      c22222 = 1.d-6 * ucwindpf * pfwndl
      c22222 = fkind * c22222 * cmlsa(lsa)

C *** Account 222.2.3 : Steel case - will be zero for resistive coils

      c22223 = 1.d-6 * uccase * whtpfs
      c22223 = fkind * c22223 * cmlsa(lsa)

C *** Account 222.2.4 : Support structure

      c22224 = 1.d-6 * ucfnc * fncmass
      c22224 = fkind * c22224 * cmlsa(lsa)

C *** Total account 222.2

      c2222 = c22221 + c22222 + c22223 + c22224

C *** Reset sccufac to previous value if necessary

      if (ipfres.eq.1) sccufac = sccufac1

      end
C______________________________________________________________________
      SUBROUTINE ACC2222A

C *** Account 222.2 : PF magnet assemblies (reversed field pinch)

C *** whtpf    : Total mass of winding packs (kg)
C *** whtpfs   : Total mass of cases (kg)
C *** fncmass  : Gravity pf fence mass (kg)
C *** sccufac  : Ratio of superconductor to copper in the cable at a
C ***            magnetic field of 1T [0.0188]
C *** 
C *** uccu     : Cost for copper in superconducting cable ($/kg) [75.]
C *** ucsc     : Superconductor cost ($/kg) [600., 600., 300.]
C *** cconfix  : Fixed cost of superconducting cable ($/m) [80.]
C *** cconshpf : Unit cost of steel conduit/sheath ($/m) [70.]
C *** ucwindpf : Winding cost ($/m) [465.]
C *** uccase   : Unit cost of case & closeout, $/kg of case [50.]
C *** ucfnc    : Unit cost of outer PF fence support ($/kg) [35.] 
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'struccom.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'build.h'
      INCLUDE 'rfp.h'

      DOUBLE PRECISION costpfcu,costpfsc,costpfsh,costwire,cpfconpm,
     +     pfwndl,sccufac1,twopi
      DOUBLE PRECISION cmlsa(4)

      INTEGER i

C *** LSA cost multipliers

      cmlsa(1) = 0.6900D0
      cmlsa(2) = 0.8450D0
      cmlsa(3) = 0.9225D0
      cmlsa(4) = 1.0000D0

      twopi = 8.0D0 * ATAN(1.0D0)

C *** Total length of PF coil windings (m)

      pfwndl = 0.0D0
      do 10 i = 1,nrfppf
         pfwndl = pfwndl + twopi*rrpf(i)*nturns(i)
 10   continue

C *** Account 222.2.1 : Conductor

C+**PJK 13/01/94 New algorithm, taking into account the fact that the
C+**PJK 13/01/94 superconductor/copper ratio in the conductor is
C+**PJK 13/01/94 proportional to the maximum field that each coil will
C+**PJK 13/01/94 experience. OH and PF coils are treated similarly.
C+**PJK 13/01/94 Maximum values for current, current density and field
C+**PJK 13/01/94 are used.
C+**PJK 13/01/94 R. Hancox, January 1994, under contract to Culham.

      c22221 = 0.0D0

      do 20 i = 1,nrfppf

C *** Resistive coils (first 14 coils only)

         if (i.le.14) then
            sccufac1 = sccufac
            sccufac = 0.0D0
            costpfsh = 0.0D0
         else
            sccufac1 = sccufac
            costpfsh = cconshpf
         end if

C *** Superconductor ($/m)

         costpfsc = ucsc(isumatpf) * (sccufac*bpf(i)) *
     +        drpf(i) * dzpf(i) / nturns(i) * dcond(isumatpf)

C *** Copper ($/m)

         costpfcu = uccu * (1.0D0-sccufac*bpf(i)) *
     +        drpf(i) * dzpf(i) / nturns(i) * dcopper

C *** Total cost/metre of superconductor and copper wire

         costwire = costpfsc + costpfcu

C *** Total cost/metre of conductor (including sheath and fixed costs)

         cpfconpm = costwire + costpfsh + cconfix

C *** Total account 222.2.1

         c22221 = c22221 + (1.0D-6 * twopi * rrpf(i) * nturns(i) *
     +        cpfconpm)

C *** Reset sccufac to previous value if necessary

         sccufac = sccufac1

 20   continue

      c22221 = fkind * c22221 * cmlsa(lsa)

C *** Account 222.2.2 : Winding

      c22222 = 1.d-6 * ucwindpf * pfwndl
      c22222 = fkind * c22222 * cmlsa(lsa)

C *** Account 222.2.3 : Steel case - will be zero for resistive coils

      c22223 = 1.d-6 * uccase * whtpfs
      c22223 = fkind * c22223 * cmlsa(lsa)

C *** Account 222.2.4 : Support structure

      c22224 = 1.d-6 * ucfnc * fncmass
      c22224 = fkind * c22224 * cmlsa(lsa)

C *** Total account 222.2

      c2222 = c22221 + c22222 + c22223 + c22224

      end
C______________________________________________________________________
      SUBROUTINE ACC2223

C *** Account 222.3 : Cryostat

C *** cryomass : Vacuum vessel mass (kg)

C *** uccryo   : Unit cost of cryostat ($/kg) [32.0]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'fwblsh.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.6900D0
      cmlsa(2) = 0.8450D0
      cmlsa(3) = 0.9225D0
      cmlsa(4) = 1.0000D0

      c2223 = 1.d-6 * cryomass * uccryo
      c2223 = fkind * c2223 * cmlsa(lsa)

      end
C______________________________________________________________________
      SUBROUTINE ACC223

C *** Account 223 : Power injection

C *** Costs from TETRA, updated to 1990$
C *** Nominal TIBER values are used pending system designs. Costs are
C *** scaled linearly with power injected into the plasma and include
C *** the power supplies.
C *** If ifueltyp=1, the fraction (1-fcdfuel) of the cost of the
C *** current drive system is considered as capital cost, and the
C *** fraction (fcdfuel) is considered a recurring fuel cost due
C *** to the system's short life.

C *** echpwr   : Injected ECH power (W)
C *** plhybd   : Injected lower hybrid or ICH power (W)
C *** pnbeam   : Injected neutral beam power (W)
C *** pofcd    : Injected oscillating field power (W)

C *** ucech    : ECH system cost          ($/W**exprf) [3.0]
C *** ucich    : ICH system cost          ($/W**exprf) [3.0]
C *** uclh     : Lower hybrid system cost ($/W**exprf) [3.3]
C *** ucnbi    : Neutral beam system cost ($/W**exprf) [3.3]
C *** ucof     : Oscillating field cost   ($/W**exprf) [3.3]
C *** exprf    : Scaling exponent with power [1.0]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 21/03/97
C *** IFE costs (deflated to 1990 dollars) taken from
C *** Meier and Bieri (OSIRIS heavy ion beam), Fus Tech 21 (1992) 1547
C *** Meier and von Rosenberg (SOMBRERO laser), Fus Tech 21 (1992) 1552
C *** cdriv0   : IFE generic/laser driver cost at edrive=0 (M$) [154.3]
C *** dcdrv0   : generic/laser driver cost gradient (M$/MJ) [111.4]
C *** cdriv1   : IFE low energy heavy ion beam driver cost
C ***            extrapolated to edrive=0 (M$) [163.2]
C *** dcdrv1   : HIB driver cost gradient at low energy (M$/MJ) [78.0]
C *** cdriv2   : IFE high energy heavy ion beam driver cost
C ***            extrapolated to edrive=0 (M$) [244.9]
C *** dcdrv2   : HIB driver cost gradient at high energy (M$/MJ) [59.9]
C *** mcdriv   : IFE driver cost multiplier [1.0]
C-**PJK 21/03/97

C+**PJK 22/12/93 Converted exprf to local parameter
      DOUBLE PRECISION exprf
      PARAMETER (exprf = 1.0D0)

      INCLUDE 'cdriv.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

      DOUBLE PRECISION switch

      if (ife.ne.1) then

C *** Account 223.1 : ECH

         c2231 = 1.d-6 * ucech * echpwr**exprf
         if (ifueltyp.eq.1) c2231 = (1.0D0-fcdfuel) * c2231
         c2231 = fkind * c2231

C *** Account 223.2 : Lower Hybrid or ICH

         if (iefrf.ne.2) then
            c2232 = 1.d-6 * uclh * plhybd**exprf
         else
            c2232 = 1.d-6 * ucich * plhybd**exprf
         end if
         if (ifueltyp.eq.1) c2232 = (1.0D0-fcdfuel) * c2232
         c2232 = fkind * c2232

C *** Account 223.3 : Neutral Beam

         c2233 = 1.d-6 * ucnbi * pnbeam**exprf
         if (ifueltyp.eq.1) c2233 = (1.0D0-fcdfuel) * c2233
         c2233 = fkind * c2233

C *** Account 223.4 : Oscillating Field (RFP)

         c2234 = 1.d-6 * ucof * pofcd**exprf
         if (ifueltyp.eq.1) c2234 = (1.0D0-fcdfuel) * c2234
         c2234 = fkind * c2234

      else

C+**PJK 21/03/97
C *** IFE driver costs (depends on driver type)
C *** Assume offset linear form for generic and SOMBRERO types,
C *** or one of two offset linear forms for OSIRIS type

         if (ifedrv.ne.2) then
            c2231 = mcdriv * (cdriv0 + dcdrv0*1.0D-6*edrive)
         else
            if (dcdrv1.le.dcdrv2) then
               switch = 0.0D0
            else
               switch = (cdriv2-cdriv1)/(dcdrv1-dcdrv2)
            end if
            if (edrive.le.switch) then
               c2231 = mcdriv * (cdriv1 + dcdrv1*1.0D-6*edrive)
            else
               c2231 = mcdriv * (cdriv2 + dcdrv2*1.0D-6*edrive)
            end if
         end if

         if (ifueltyp.eq.1) c2231 = (1.0D0-fcdfuel) * c2231
         c2231 = fkind * c2231
         c2232 = 0.0D0
         c2233 = 0.0D0
         c2234 = 0.0D0

      end if
C-**PJK 21/03/97

C *** Total account 223

      c223 = c2231 + c2232 + c2233 + c2234
      cdcost = c223

      end
C______________________________________________________________________
      SUBROUTINE ACC224

C *** Account 224 : Vacuum system

C *** Costs are scaled from TETRA reactor code runs

C *** vpumpn   : Number of high vacuum pumps
C *** nvduct   : Number of ducts from torus to pumps
C *** nvtype   : Type of pump, 0 = turbomolecular, 1 = cryo
C *** dlscal   : Duct length equivalent 1m diameter, m
C *** vacdshm  : Mass of vacuum duct shield, kg
C *** vcdimax  : Diameter of passage, torus to ducts, m

C *** uccpmp   : Cryopump cost ($) [3.9d5]
C *** uctpmp   : Turbomolecular pump cost ($) [1.105d5]
C *** ucbpmp   : Backing pump cost ($) [2.925d5] 
C *** ucduct   : Duct cost ($/m) [4.225d4]
C *** ucvalv   : Valve cost ($) [3.9d5]
C *** ucvdsh   : Vacuum duct shield cost ($/kg) [26.0]
C *** ucviac   : Vacuum instrumentation and control ($) [1.3d6]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 22/12/93 Engineering and installation factor (1.3) removed,
C+**PJK 22/12/93 so vacuum system unit costs are all * 1.3 instead.

      INCLUDE 'torsdat.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C *** Account 224.1 : High vacuum pumps

      if (nvtype.eq.1) then
         c2241 = 1.d-6 * vpumpn * uccpmp
      else
         c2241 = 1.d-6 * vpumpn * uctpmp
      end if
      c2241 = fkind * c2241

C *** Account 224.2 : Backing pumps

      c2242 = 1.d-6 * dble(nvduct) * ucbpmp
      c2242 = fkind * c2242

C *** Account 224.3 : Vacuum duct

      c2243 = 1.d-6 * dble(nvduct) * dlscal * ucduct
      c2243 = fkind * c2243

C *** Account 224.4 : Valves

      c2244 = 1.d-6 * 2.0D0 * dble(nvduct) * (vcdimax*1.2D0)**1.4D0
     +     * ucvalv
      c2244 = fkind * c2244

C *** Account 224.5 : Duct shielding

      c2245 = 1.d-6 * dble(nvduct) * vacdshm * ucvdsh
      c2245 = fkind * c2245

C *** Account 224.6 : Instrumentation

      c2246 = 1.d-6 * ucviac
      c2246 = fkind * c2246

C *** Total account 224

      c224 = c2241 + c2242 + c2243 + c2244 + c2245 + c2246

      end
C______________________________________________________________________
      SUBROUTINE ACC225

C *** Account 225 : Power conditioning

      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

      EXTERNAL acc2251,acc2252,acc2253

C+**PJK 21/03/97
      if (ife.eq.1) then

         c225 = 0.0D0

      else

C *** Account 225.1 : TF coil power conditioning

         call acc2251

C *** Account 225.2 : PF coil power conditioning

         call acc2252

C *** Account 225.3 : Energy storage

         call acc2253

C *** Total account 225

         c225 = c2251 + c2252 + c2253

      end if
C-**PJK 21/03/97

      end
C______________________________________________________________________
      SUBROUTINE ACC2251

C *** Account 225.1 : TF coil power conditioning

C *** Costs are developed based on the major equipment specification
C *** of the tfcpwr module.  A multiplier is used to account for bulk
C *** materials and installation.

C *** cpttf    : TF current per turn (A)
C *** estotf   : Total full current energy of a TF coil, (GJ)
C *** tfbusl   : TF electrical bus length (m)
C *** vtfskv   : Allowable TF voltage during quench (kV)
C *** tfckw    : Available DC power for charging (kW)
C *** tfcmw    : TF coil resistive power (MW)

C *** uctfps   : Cost of power supplies ($/W**0.7) [24.]
C *** uctfbr   : Cost of breakers ($/W**0.7)  [1.22]
C *** uctfsw   : Cost of slow dump switches ($/A) [1.]
C *** ucbus    : Cost of aluminium bus (itfsup = 1) ($/(A*m)) [.123]
C *** uctfbus  : Cost of bus (itfsup = 0) ($/kg) [100]
C *** expel    : Scaling exponent for electrical equipment [0.7]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 22/12/93 Converted expel to local parameter
      DOUBLE PRECISION expel
      PARAMETER (expel = 0.7D0)

      INCLUDE 'tfcoil.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C *** Account 225.1.1 : TF coil power supplies

C+**PJK 01/02/96 Added TF coil resistive power term

      c22511 = 1.d-6 * uctfps * (tfckw*1.d3 + tfcmw*1.d6)**expel
      c22511 = fkind * c22511

C *** Account 225.1.2 : TF coil breakers (zero cost for copper coils)

      if (itfsup .eq. 1) then
         c22512 = 1.d-6 * (uctfbr*tfno*(cpttf*vtfskv*1.d3)**expel+
     +        uctfsw*cpttf)
      else
         c22512 = 0.0D0
      end if
      c22512 = fkind * c22512

C *** Account 225.1.3 : TF coil dump resistors

      c22513 = 1.d-6 * (uctfdr*(tfno*estotf*1.d9)+uctfgr * tfno/2.0D0)
      c22513 = fkind * c22513

C *** Account 225.1.4 : TF coil instrumentation and control

      c22514 = 1.d-6 * uctfic * (30.0D0*tfno)
      c22514 = fkind * c22514

C *** Account 225.1.5 : TF coil bussing

      if (itfsup.eq.0) then
         c22515 = 1.d-6 * uctfbus * tfbusmas
      else
         c22515 = 1.d-6 * ucbus * cpttf * tfbusl
      end if
      c22515 = fkind * c22515

C *** Total account 225.1

      c2251 = c22511 + c22512 + c22513 + c22514 + c22515

      end
C______________________________________________________________________
      SUBROUTINE ACC2252

C *** Account 225.2 : PF coil power conditioning

C *** Costs are taken from the equipment specification of the
C *** pwrconv module.

C *** peakmva  : Peak of power supply rating (MVA)
C *** pfckts   : Number of PF coil circuits
C *** spfbusl  : Total PF power system bus length (m)
C *** acptmax  : Average of the max currents in PF circuits (kA)
C *** srcktpm  : Sum of resistive power in PF circuits (kW)
C *** vpfskv   : Maximum PF coil voltage (kV)
C *** ensxpfm  : Maximum stored energy in the PF circuits (MJ)

C *** ucpfps   : Cost of pulsed-power power supplies ($/MVA) [3.5e4]
C *** ucpfic   : PF coil instrum. and control cost ($/channel) [1.d4]
C *** ucpfb    : Cost of bussing ($/kA*m) [210.]
C *** ucpfbs   : Cost of burn power supplies ($/kW**0.7) [4.9e3]
C *** ucpfbk   : Cost of DC breakers ($/MVA) [1.66e4]
C *** ucpfdr1  : Cost factor for dump resistors ($/MJ) [150.]
C *** ucpfcb   : Cost of AC breakers ($/circuit) [7.5e4]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'htpwr.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C *** Account 225.2.1 : PF coil power supplies

      c22521 = 1.d-6 * ucpfps * peakmva
      c22521 = fkind * c22521

C *** Account 225.2.2 : PF coil instrumentation and control

      c22522 = 1.d-6 * ucpfic * pfckts * 30.0D0
      c22522 = fkind * c22522

C *** Account 225.2.3 : PF coil bussing

      c22523 = 1.d-6 * ucpfb * spfbusl*acptmax
      c22523 = fkind * c22523

C *** Account 225.2.4 : PF coil burn power supplies

      if (pfckts.ne.0.0D0) then
         c22524 = 1.d-6 * ucpfbs * pfckts*(srcktpm/pfckts)**0.7D0
      else
         c22524 = 0.0D0
      end if
      c22524 = fkind * c22524

C *** Account 225.2.5 : PF coil breakers

      c22525 = 1.d-6 * ucpfbk * pfckts*(acptmax*vpfskv)**0.7D0
      c22525 = fkind * c22525

C *** Account 225.2.6 : PF coil dump resistors

      c22526 = 1.d-6 * ucpfdr1 * ensxpfm
      c22526 = fkind * c22526

C *** Account 225.2.7 : PF coil AC breakers

      c22527 = 1.d-6 * ucpfcb * pfckts
      c22527 = fkind * c22527

C *** Total account 225.2

      c2252 = c22521 + c22522 + c22523 + c22524 + c22525 +
     +     c22526 + c22527

      end
C______________________________________________________________________
      SUBROUTINE ACC2253

C *** Account 225.3 : Energy storage

C *** ensxpfm  : Maximum stored energy in circuits (MJ)
C *** fmgmva   : Pulsed power rating of the motor generator flywheel
C ***            (= mgf) (MVA)
C *** fmgmj    : Energy rating of the mgf (MJ)

C *** uces1    : MGF cost factor ($/MVA**0.8) [3.2d4]
C *** uces2    : MGF cost factor ($/MJ**0.8) [8.8d3]
C *** expes    : Scaling exponent for energy storage [0.8]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 22/12/93 Introduced expes as a local parameter
      DOUBLE PRECISION expes
      PARAMETER (expes = 0.8D0)

      INCLUDE 'pulse.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'times.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      DOUBLE PRECISION shcss

C *** c2253 = 1.0d-6 * (uces1 * fmgmva**expes + uces2 * fmgmj**expes)
      c2253 = 0.0D0

C+**PJK 24/11/93 Addition of thermal storage options for a
C+**PJK 24/11/93 pulsed reactor

C *** Thermal storage options for a pulsed reactor
C *** See F/MPE/MOD/CAG/PROCESS/PULSE/0008 and 0014

      if (lpulse.eq.1) then

         if (istore.eq.1) then

C *** Option 1 from ELECTROWATT report
C *** Pulsed Fusion Reactor Study : AEA FUS 205

C *** Increased condensate tank capacity
            c2253 = 0.1D0

C *** Additional electrically-driven feedpump (50 per cent duty)
            c2253 = c2253 + 0.8D0

C *** Increased turbine-generator duty (5 per cent duty)
            c2253 = c2253 + 4.0D0

C *** Additional auxiliary transformer capacity and ancillaries
            c2253 = c2253 + 0.5D0

C *** Increased drum capacity
            c2253 = c2253 + 2.8D0

C *** Externally fired superheater
            c2253 = c2253 + 29.0D0

         else if (istore.eq.2) then

C *** Option 2 from ELECTROWATT report
C *** Pulsed Fusion Reactor Study : AEA FUS 205

C *** Increased condensate tank capacity
            c2253 = 0.1D0

C *** Additional electrically-driven feedpump (50 per cent duty)
            c2253 = c2253 + 0.8D0

C *** Increased drum capacity
            c2253 = c2253 + 2.8D0

C *** Increased turbine-generator duty (5 per cent duty)
            c2253 = c2253 + 4.0D0

C *** Additional fired boiler (1 x 100 per cent duty)
            c2253 = c2253 + 330.0D0

C *** HP/LP steam bypass system for auxiliary boiler
C *** (30 per cent boiler capacity)
            c2253 = c2253 + 1.0D0

C *** Dump condenser
            c2253 = c2253 + 2.0D0

C *** Increased cooling water system capacity
            c2253 = c2253 + 18.0D0

         else if (istore.eq.3) then

C *** Simplistic approach that assumes that a large stainless steel
C *** block acts as the thermal storage medium. No account is taken
C *** of the cost of the piping within the block, etc.
C ***
C *** shcss is the specific heat capacity of stainless steel (J/kg/K)
C *** dtstor is the maximum allowable temperature change in the
C *** stainless steel block (input)

            shcss = 520.0D0
            c2253 = ucblss * (pthermmw * 1.0D6) * tdown /
     +           (shcss * dtstor)

         else
            write(*,*) 'Error in routine ACC2253 :'
            write(*,*) 'Illegal value of istore, = ',istore
            write(*,*) 'PROCESS stopping.'
            STOP
         end if

         if (istore.lt.3) then

C *** Scale c2253 with net electric power

            c2253 = c2253 * pnetelmw/1200.0D0

C *** It is necessary to convert from 1992 pounds to 1990 dollars

C *** Reasonable guess for the exchange rate + inflation factor
C *** inflation = 5% per annum; exchange rate = 1.5 dollars per pound

            c2253 = c2253 * 1.36D0

         end if

      end if

      c2253 = fkind * c2253

      end
C______________________________________________________________________
      SUBROUTINE ACC226

C *** Account 226 : Heat transport system

C *** Cost estimated from major equipment and hts loops developed in
C *** heatpwr module of code.

C *** Water cooling system
C *** pfwdiv   : First wall/divertor heat removal (MW)
C *** pnucblkt : Blanket heat removal (MW)
C *** pnucshld : Shield heat removal (MW)
C *** priheat  : Primary system heat removal (MW)
C *** pinjht   : Injection power heat removal (MW)
C *** crypmw   : Cryogenic plant heat removal (MW)
C *** vachtmw  : Vacuum pump heat removal (MW)
C *** trithtmw : Tritium plant heat removal (MW)
C *** facht    : Facility heat removal (MW)
C *** ctht     : Total plant heat removal (MW)
C *** rnphx    : Number of primary heat exchangers (real number)
C *** rnihx    : Number of intermediate heat exchangers (real number)
C *** 
C+**PJK 21/03/97
C *** tdspmw   : Target delivery system heat removal (MW)
C *** tfacmw   : Target factory heat removal (MW)
C-**PJK 21/03/97
C *** Cryogenic system
C *** helpow   : Total cryogenic load (W)
C *** 
C *** uchts    : Cost of pumps and piping per loop ($/W**exphts)
C ***            [15.3,19.1] (dependent on blanket coolant type)
C *** exphts   : Scaling exponent for hts equip. with heat removed [0.7]
C *** ucphx    : Primary heat transp. cost ($/W**exphts) [15.0]
C *** ucahts   : Aux heat transport piping, pumps ($/W**exphts) [31.0]
C *** ucihx    : Cost of intermediate exchangers ($/W**exphts) [0.0]
C *** uccry    : Cost of cryo plant ($/W**0.67) [9.3d4]
C *** expcry   : Scaling exponent for cryogenic cost [0.67]
C *** 
C+**PJK 22/05/07
C *** helecmw  : Electrical power for H production (MW)
C *** hthermmw : Thermal power for H production (MW)
C-**PJK 22/05/07
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 22/12/93 Converted exphts and expcry to local parameters
      DOUBLE PRECISION exphts,expcry
      PARAMETER (exphts = 0.7D0, expcry = 0.67D0)

      INCLUDE 'fwblsh.h'
      INCLUDE 'blanket.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.4000D0
      cmlsa(2) = 0.7000D0
      cmlsa(3) = 0.8500D0
      cmlsa(4) = 1.0000D0

C *** Account 226.1 : Reactor cooling system

C *** Pumps and piping system

      cpp = 1.d-6 * uchts(costr) * ( (1.d6*pfwdiv)**exphts +
     +     (1.d6*pnucblkt)**exphts + (1.d6*pnucshld)**exphts)
      cpp = fkind * cpp * cmlsa(lsa)

C *** Primary heat exchangers

      chx = 1.d-6 * ucphx * rnphx * (1.d6*priheat/rnphx)**exphts
      chx = fkind * chx * cmlsa(lsa)

      c2261 = chx + cpp

C *** Account 226.2 : Auxiliary component cooling

C *** Pumps and piping system
C+**PJK 21/03/97 Why is pnucloss and htpmw included in cppa?
      cppa = 1.d-6 * ucahts * ( (1.d6*pinjht)**exphts +
     +     (1.d6*crypmw)**exphts + (1.d6*vachtmw)**exphts +
     +     (1.d6*trithtmw)**exphts + (1.d6*facht)**exphts )

C+**PJK 21/03/97
      if (ife.eq.1) cppa = cppa + 1.d-6 * ucahts * (
     +     (1.d6*tdspmw)**exphts + (1.d6*tfacmw)**exphts )
C-**PJK 21/03/97

C+**PJK 22/05/07 Added H production powers
      if (ihplant.ne.0) cppa = cppa + 1.d-6 * ucahts * (
     +     (1.d6*hthermmw)**exphts + (1.d6*helecmw)**exphts )
C-**PJK 22/05/07

      cppa = fkind * cppa * cmlsa(lsa)

C *** Intermediate heat exchangers

      chxa = 1.d-6 * ucihx * rnihx * (1.d6*ctht/rnihx)**exphts
      chxa = fkind * chxa * cmlsa(lsa)

      c2262 = cppa + chxa

C *** Account 226.3 : Cryogenic system

      c2263 = 1.d-6 * uccry * 4.5D0/tftmp * helpow**expcry
      c2263 = fkind * c2263 * cmlsa(lsa)

C *** Total account 226

      c226 = c2261 + c2262 + c2263

      end
C______________________________________________________________________
      SUBROUTINE ACC227

C *** Account 227 : Fuel handling

C *** Costs are scaled from TETRA reactor code runs

C *** qfuel    : DT injection rate to plasma (A)
C *** burnup   : Plasma fractional burnup (based on alpha removal)
C *** volrci + wsvol : Internal volume of nuclear buildings (m3)
C *** cfrht    : Atmos. processing flow rate for reactor hall (m3/s)
C *** wtgpd    : Mass of fuel used per day (g)

C *** ucf1     : Cost of fuelling system ($) [2.23d7]
C *** ucfpr    : Cost of 60g/day tritium processing unit ($) [4.4d7]
C *** ucdtc    : Detritiation, air cleanup cost ($ per 1E4 m3/hr) [13.]
C *** ucnbv    : Nuclear building ventilation ($/(m3)**0.8) [1000.]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

C+**PJK 21/03/97
C *** fburn    : IFE burn fraction
C ***            (fraction of tritium fused per target) [0.3333]
C-**PJK 21/03/97

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'bldgvol.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'
      INCLUDE 'ife.h'

      DOUBLE PRECISION cfrht,targtm

C *** Account 227.1 : Fuelling system

      c2271 = 1.d-6 * ucf1
      c2271 = fkind * c2271

C *** Account 227.2 : Fuel processing and purification

C+**PJK 08/12/95 I do not understand the factor 1.3 here...

C+**PJK 21/03/97
      if (ife.ne.1) then
         wtgpd = burnup * qfuel * 1.3D0
      else
         targtm = gain * edrive * 3.0D0 * 1.67D-27 * 1.0D3 /
     +        (1.602D-19 * 17.6D6 * fburn)
         wtgpd = targtm * reprat * 86400.0D0
      end if
C-**PJK 21/03/97

C+**PJK 08/12/95 Assume that He3 costs same as tritium to process...
      c2272 = 1.d-6 * ucfpr * (0.5D0 + 0.5D0*(wtgpd/60.0D0)**0.67D0)
      c2272 = fkind * c2272

C *** Account 227.3 : Atmospheric recovery systems

      cfrht = 1.0D5
C+**PJK 22/12/93 Added extra brackets to unite volrci and wsvol
C+**PJK 08/12/95 No detritiation needed if D-He3 reaction
      if (idhe3.eq.0) then
         c2273 = 1.0D-6 * ucdtc * ( (cfrht/1.D4)**0.6D0 * (volrci +
     +        wsvol))
      else
         c2273 = 0.0D0
      end if
      c2273 = fkind * c2273

C *** Account 227.4 : Nuclear building ventilation

      c2274 = 1.d-6 * ucnbv * (volrci + wsvol)**0.8D0
      c2274 = fkind * c2274

C *** Total account 227

      c227 = c2271 + c2272 + c2273 + c2274

      end
C______________________________________________________________________
      SUBROUTINE ACC228

C *** Account 228 : Instrumentation and control

C *** Costed by allowance based on TFCX and INTOR

C *** uciac    : Instrumentation and control, and diagnostics ($/W**0.3)
C ***            [1.5d8]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      c228 = 1.d-6 * uciac
      c228 = fkind * c228

      end
C______________________________________________________________________
      SUBROUTINE ACC229

C *** Account 229 : Maintenance equipment

C *** ucme     : Unit cost of maintenance equipment ($/W**0.3) [1.25d8]
C *** fkind    : Cost multiplier for Nth of a kind assumption [1.0]

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      c229 = 1.d-6 * ucme 
      c229 = fkind * c229

      end
C______________________________________________________________________
      SUBROUTINE ACC23

C *** Account 23 : Turbine plant equipment

C *** pgrossmw : Gross electric power (MW)

C *** ucturb   : Cost of turbine plant equipment ($) [230.0D6,245.0D6]
C *** exptpe   : Scaling exponent for turbine plant equipment [0.83]

C+**PJK 22/12/93 Introduced exptpe as a local parameter
      DOUBLE PRECISION exptpe
      PARAMETER (exptpe = 0.83D0)

      INCLUDE 'blanket.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      if (ireactor.eq.1) then

         c23 = 1.0D-6 * ucturb(costr) * (pgrossmw/1200.0D0)**exptpe

      end if

      end
C______________________________________________________________________
      SUBROUTINE ACC24

C *** Account 24 : Electric plant equipment

C *** pacpmw   : total pulsed power load (MW)
C *** fcsht    : total facility power load (MW)
C *** tlvpmw   : total low voltage power (MW)

C *** ucswyd   : Switchyard equipment costs ($) [1.84d7]
C *** ucpp     : Primary power transformers ($/kVA**0.9) [48.]
C *** ucap     : Auxiliary transformer costs ($/kVA) [17.]
C *** uclv     : Low voltage equipment costs ($/kVA) [16.]
C *** ucdgen   : Cost per 8MW diesel generator ($) [1.7d6]
C *** ucaf     : Auxiliary facility power equipment ($) [1.5d6]
C *** expepe   : Scaling exponent for electric plant equipment [0.9]

C+**PJK 22/12/93 Introduced expepe as a local parameter
      DOUBLE PRECISION expepe
      PARAMETER (expepe = 0.9D0)

      INCLUDE 'htpwr.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.5700D0
      cmlsa(2) = 0.7850D0
      cmlsa(3) = 0.8925D0
      cmlsa(4) = 1.0000D0

C *** Account 241 : Switchyard

      c241 = 1.d-6 * ucswyd * cmlsa(lsa)

C *** Account 242 : Transformers

      c242 = 1.d-6 * (ucpp*(pacpmw*1.D3)**expepe + ucap*(fcsht*1.D3))
      c242 = c242 * cmlsa(lsa)

C *** Account 243 : Low voltage
C *** (include 0.8 factor for transformer efficiency)

C+**PJK 23/01/97 Used tlvpmw instead of (pacpmw+fcsht)
      c243 = 1.d-6 * uclv * tlvpmw * 1.0D3 / 0.8D0 * cmlsa(lsa)

C *** Account 244 : Diesel generator (8 MW per generator,  assume 4 )

      c244 = 1.d-6 * ucdgen * 4.0D0 * cmlsa(lsa)

C *** Account 245 : Auxiliary facility power needs

      c245 = 1.d-6 * ucaf * cmlsa(lsa)

C *** Total account 24

      c24 = c241 + c242 + c243 + c244 + c245

      end
C______________________________________________________________________
      SUBROUTINE ACC25

C *** Account 25 : Miscellaneous plant equipment (waste treatment)

C *** ucmisc   : miscellaneous plant allowance ($) [2.5d7]

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.7700D0
      cmlsa(2) = 0.8850D0
      cmlsa(3) = 0.9425D0
      cmlsa(4) = 1.0000D0

      c25 = 1.d-6 * ucmisc * cmlsa(lsa)

      end
C______________________________________________________________________
      SUBROUTINE ACC26

C *** Account 26 : Heat rejection system

C *** Cost scaled with total plant heat rejection based on commercial
C *** systems.
C *** From J. Delene, private communication, ORNL, June 1990

C *** pgrossmw : gross electric power (MW)
C *** pthermmw : primary thermal power (MW)

C *** uchrs    : cost of heat rejection system ($) [87.9D6]

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

      DOUBLE PRECISION pwrrej

C+**PJK 07/02/96 Added use of cmlsa (cost multiplier for LSA)
      DOUBLE PRECISION cmlsa(4)

      cmlsa(1) = 0.8000D0
      cmlsa(2) = 0.9000D0
      cmlsa(3) = 0.9500D0
      cmlsa(4) = 1.0000D0

      if (ireactor.eq.0) then
         pwrrej = powfmw + pinjwp + tfcmw
      else
         pwrrej = pthermmw - pgrossmw
      end if

      c26 = 1.0D-6 * uchrs * pwrrej/2300.0D0 * cmlsa(lsa)

      end
C______________________________________________________________________
      SUBROUTINE ACCHYD

C *** Hydrogen Production Plant

C *** Cost scaled with hydrogen production rate (MW equivalent)
C *** See F/T&M/PJK/LOGBOOK20, pp.2-3

C *** hpower : hydrogen production rate (MW equivalent)

C *** uchlte  : unit cost for low temperature electrolysis ($/kW) [400.0]
C *** uchhten : unit cost for high temp electrolysis - endo ($/kW) [1350.0]
C *** uchhtex : unit cost for high temp electrolysis - exo ($/kW) [900.0]
C *** uchth   : unit cost for thermo-chemical method ($/kW) [700.0]

      INCLUDE 'htpwr.h'
      INCLUDE 'cost.h'

      DOUBLE PRECISION uchyd

      if (ihplant.eq.0) then
         uchyd = 0.0D0
      else if (ihplant.eq.1) then
         uchyd = uchlte
      else if (ihplant.eq.2) then
         uchyd = uchhten
      else if (ihplant.eq.3) then
         uchyd = uchhtex
      else
         uchyd = uchth
      end if

      chplant = 1.0D-6 * uchyd * (1.0D3*hpower)

      end
C______________________________________________________________________
      SUBROUTINE ACC9

C *** Account 9 : Indirect cost and contingency allowances

C *** The cost modelling is based on the commercial plant model of a
C *** single contractor performing all plant engineering & construction
C *** management, using commercially purchased equipment and materials.
C *** 
C *** The project contingency is an allowance for incomplete design
C *** specification and unforeseen events during the plant construction.
C *** The factors used are estimated from commercial plant experience.
C *** 
C *** From J. Delene, private communication, ORNL, June 1990

C *** cfind    : indirect cost factor applied to direct costs
C ***            [0.244,0.244,0.244,0.29]
C *** cowner   : owner cost factor applied to (direct+indirect) costs
C ***            [0.15]
C *** fcontng  : project contingency factor applied to total costs
C ***            [0.195]

      INCLUDE 'cost.h'
      INCLUDE 'cost2.h'

C *** Indirect costs

      cindrt = cfind(lsa) * cdirt * (1.0D0 + cowner)

C *** Contingency costs

      ccont = fcontng * (cdirt + cindrt)

      end
