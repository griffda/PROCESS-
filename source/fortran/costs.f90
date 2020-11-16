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

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  !  Various cost account values (M$)
  real(dp) :: c228, c229, c23, c25, c26, cindrt, ccont

  !  Account 226 - Heat transport system 
  real(dp) :: c226, c2261, c2262, c2263

  !  Account 227 - Fuel handling
  real(dp) :: c227, c2271, c2272, c2273, c2274

  !  Account 24 - electrical plant equipment
  real(dp) :: c24, c241, c242, c243, c244, c245


  real(dp) :: &
       c21,c211,c212,c213,c214,c2141,c2142,c215,c216,c217,c2171, &
       c2172,c2173,c2174,c22,c2211,c2212,c22121,c22122,c22123, &
       c22124,c22125,c22126,c22127,c2213,c22131,c22132,c2214,c2215, &
       c2221,c22211,c22212,c22213,c22214,c22215,c2222,c22221,c22222, &
       c22223,c22224,c2223,c223,c2231,c2232,c2233,c2234,c224,c2241, &
       c2242,c2243,c2244,c2245,c2246,c225,c2251,c22511,c22512,c22513, &
       c22514,c22515,c2252,c22521,c22522,c22523,c22524,c22525,c22526, &
       c22527,c2253,chx,cpp,cppa, c22128

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

  subroutine costs(outfile,iprint)

    !! Cost accounting for a fusion power plant
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine performs the cost accounting for a fusion power plant.
    !! The direct costs are calculated based on parameters input
    !! from other sections of the code.
    !! <P>Costs are in 1990 $, and assume first-of-a-kind components
    !! unless otherwise stated. Account 22 costs include a multiplier
    !! to account for Nth-of-a-kind cost reductions.
    !! <P>The code is arranged in the order of the standard accounts.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: concost, crctcore, fkind, ireactor, moneyint, &
      c222, cdirt, output_costs, ifueltyp, capcost, c221, lsa, ipnet 
    use fwbs_variables, only: blkttype 
    use ife_variables, only: ife 
    use heat_transport_variables, only: ipowerflow 
    use physics_variables, only: itart 
    use process_output, only: ovarin, ovarre, oshead, oblnkl, oheadr, ocosts 
    use tfcoil_variables, only: i_tf_sup 
    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 21 : Structures and site facilities
    call acc21

    !  Account 22 : Fusion power island
    call acc22

    !  Account 23 : Turbine plant equipment
    call acc23

    !  Account 24 : Electric plant equipment
    call acc241    ! Account 241 : Switchyard
    call acc242    ! Account 242 : Transformers
    call acc243    ! Account 243 : Low voltage
    call acc244    ! Account 244 : Diesel generators
    call acc245    ! Account 245 : Auxiliary facility power equipment
    call acc24     ! Account 24  : Total

    !  Account 25 : Miscellaneous plant equipment
    call acc25

    !  Account 26 : Heat rejection system
    call acc26

    !  Total plant direct cost
    !cdirt = c21 + c22 + c23 + c24 + c25 + c26 + chplant
    cdirt = c21 + c22 + c23 + c24 + c25 + c26

    !  Account 9 : Indirect cost and project contingency
    call acc9

    !  Constructed cost
    concost = cdirt + cindrt + ccont

    !  Cost of electricity
    if ((ireactor == 1).and.(ipnet == 0)) call coelc(outfile,iprint)

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section

    call oheadr(outfile,'Detailed Costings (1990 US$)')
    call ovarre(outfile,'Acc.22 multiplier for Nth of a kind','(fkind)', &
         fkind)
    call ovarin(outfile,'Level of Safety Assurance','(lsa)',lsa)
    call oblnkl(outfile)
    call oshead(outfile,'Structures and Site Facilities')
    call ocosts(outfile,'(c211)','Site improvements, facilities, land (M$)',c211)
    call ocosts(outfile,'(c212)','Reactor building cost (M$)',c212)
    call ocosts(outfile,'(c213)','Turbine building cost (M$)',c213)
    call ocosts(outfile,'(c2141)','Reactor maintenance building cost (M$)',c2141)
    call ocosts(outfile,'(c2142)','Warm shop cost (M$)',c2142)
    call ocosts(outfile,'(c215)','Tritium building cost (M$)',c215)
    call ocosts(outfile,'(c216)','Electrical equipment building cost (M$)',c216)
    call ocosts(outfile,'(c2171)','Additional buildings cost (M$)',c2171)
    call ocosts(outfile,'(c2172)','Control room buildings cost (M$)',c2172)
    call ocosts(outfile,'(c2173)','Shop and warehouses cost (M$)',c2173)
    call ocosts(outfile,'(c2174)','Cryogenic building cost (M$)',c2174)
    call oblnkl(outfile)
    call ocosts(outfile,'(c21)','Total account 21 cost (M$)',c21)

    call oshead(outfile,'Reactor Systems')
    call ocosts(outfile,'(c2211)','First wall cost (M$)',c2211)
    if (ife /= 1) then
    if ((ipowerflow == 0).or.(blkttype == 3)) then
      call ocosts(outfile,'(c22121)','Blanket beryllium cost (M$)',c22121)
      call ocosts(outfile,'(c22122)','Blanket breeder material cost (M$)',c22122)
    else
      call ocosts(outfile,'(c22121)','Blanket lithium-lead cost (M$)',c22121)
      call ocosts(outfile,'(c22122)','Blanket lithium cost (M$)',c22122)
    end if
    call ocosts(outfile,'(c22123)','Blanket stainless steel cost (M$)',c22123)
    call ocosts(outfile,'(c22124)','Blanket vanadium cost (M$)',c22124)
    else  !  IFE
      call ocosts(outfile,'(c22121)','Blanket beryllium cost (M$)',c22121)
      call ocosts(outfile,'(c22122)','Blanket lithium oxide cost (M$)',c22122)
      call ocosts(outfile,'(c22123)','Blanket stainless steel cost (M$)',c22123)
      call ocosts(outfile,'(c22124)','Blanket vanadium cost (M$)',c22124)
      call ocosts(outfile,'(c22125)','Blanket carbon cloth cost (M$)',c22125)
      call ocosts(outfile,'(c22126)','Blanket concrete cost (M$)',c22126)
      call ocosts(outfile,'(c22127)','Blanket FLiBe cost (M$)',c22127)
      call ocosts(outfile,'(c22128)','Blanket lithium cost (M$)',c22128)
    end if
    call ocosts(outfile,'(c2212)','Blanket total cost (M$)',c2212)
    call ocosts(outfile,'(c22131)','Bulk shield cost (M$)',c22131)
    call ocosts(outfile,'(c22132)','Penetration shielding cost (M$)',c22132)
    call ocosts(outfile,'(c2213)','Total shield cost (M$)',c2213)
    call ocosts(outfile,'(c2214)','Total support structure cost (M$)',c2214)
    call ocosts(outfile,'(c2215)','Divertor cost (M$)',c2215)
    if (ifueltyp == 1) then
       call oblnkl(outfile)
       write(outfile,20)
20     format(t2, &
            'First wall, total blanket and divertor direct costs',/, &
            t2,'are zero as they are assumed to be fuel costs.')
    elseif (ifueltyp == 2) then 
         call oblnkl(outfile)
         write(outfile,31)
21     format(t2, &
            'Initial First wall, total blanket and divertor direct costs',/,&
            t2,'are in capital and replacemnet are in cost of electricity')
    end if

    call oblnkl(outfile)
    call ocosts(outfile,'(c221)','Total account 221 cost (M$)',c221)

    if (ife /= 1) then

       call oshead(outfile,'Magnets')

       if (i_tf_sup /= 1) then  !  Resistive TF coils
          if (itart == 1) then
             call ocosts(outfile,'(c22211)','Centrepost costs (M$)',c22211)
          else
             call ocosts(outfile,'(c22211)','Inboard leg cost (M$)',c22211)
          end if
          call ocosts(outfile,'(c22212)','Outboard leg cost (M$)',c22212)
          call ocosts(outfile,'(c2221)','TF magnet assemblies cost (M$)',c2221)
       else  !  Superconducting TF coils
          call ocosts(outfile,'(c22211)','TF coil conductor cost (M$)',c22211)
          call ocosts(outfile,'(c22212)','TF coil winding cost (M$)',c22212)
          call ocosts(outfile,'(c22213)','TF coil case cost (M$)',c22213)
          call ocosts(outfile,'(c22214)','TF intercoil structure cost (M$)',c22214)
          call ocosts(outfile,'(c22215)','TF coil gravity support structure (M$)', &
               c22215)
          call ocosts(outfile,'(c2221)','TF magnet assemblies cost (M$)',c2221)
       end if

       call ocosts(outfile,'(c22221)','PF coil conductor cost (M$)',c22221)
       call ocosts(outfile,'(c22222)','PF coil winding cost (M$)',c22222)
       call ocosts(outfile,'(c22223)','PF coil case cost (M$)',c22223)
       call ocosts(outfile,'(c22224)','PF coil support structure cost (M$)',c22224)
       call ocosts(outfile,'(c2222)','PF magnet assemblies cost (M$)',c2222)
       call ocosts(outfile,'(c2223)','Vacuum vessel assembly cost (M$)',c2223)

       if ((itart == 1).and.(ifueltyp == 1)) then
          call oblnkl(outfile)
          write(outfile,30)
30        format(t2, &
               'Centrepost direct cost is zero, as it ', &
               'is assumed to be a fuel cost.')
       elseif ((itart == 1).and.(ifueltyp == 2)) then
          call oblnkl(outfile)
          write(outfile,31)
31        format(t2, &
               'Initial centrepost direct cost in included in capital ', &
               'cost and replacements are assumed to be a fuel cost.')
       end if

       call oblnkl(outfile)
       call ocosts(outfile,'(c222)','Total account 222 cost (M$)',c222)

    end if

       call oshead(outfile,'Power Injection')

    if (ife == 1) then
         call ocosts(outfile,'(c2231)','IFE driver system cost (M$)',c2231)
    else
       call ocosts(outfile,'(c2231)','ECH system cost (M$)',c2231)
       call ocosts(outfile,'(c2232)','Lower hybrid system cost (M$)',c2232)
       call ocosts(outfile,'(c2233)','Neutral beam system cost (M$)',c2233)
    end if
    call oblnkl(outfile)
    call ocosts(outfile,'(c223)','Total account 223 cost (M$)',c223)

    call oshead(outfile,'Vacuum Systems')
    call ocosts(outfile,'(c2241)','High vacuum pumps cost (M$)',c2241)
    call ocosts(outfile,'(c2242)','Backing pumps cost (M$)',c2242)
    call ocosts(outfile,'(c2243)','Vacuum duct cost (M$)',c2243)
    call ocosts(outfile,'(c2244)','Valves cost (M$)',c2244)
    call ocosts(outfile,'(c2245)','Duct shielding cost (M$)',c2245)
    call ocosts(outfile,'(c2246)','Instrumentation cost (M$)',c2246)
    call oblnkl(outfile)
    call ocosts(outfile,'(c224)','Total account 224 cost (M$)',c224)

    if (ife /= 1) then
       call oshead(outfile,'Power Conditioning')
       call ocosts(outfile,'(c22511)','TF coil power supplies cost (M$)',c22511)
       call ocosts(outfile,'(c22512)','TF coil breakers cost (M$)',c22512)
       call ocosts(outfile,'(c22513)','TF coil dump resistors cost (M$)',c22513)
       call ocosts(outfile,'(c22514)','TF coil instrumentation and control (M$)', &
            c22514)
       call ocosts(outfile,'(c22515)','TF coil bussing cost (M$)',c22515)
       call ocosts(outfile,'(c2251)','Total, TF coil power costs (M$)',c2251)
       call ocosts(outfile,'(c22521)','PF coil power supplies cost (M$)',c22521)
       call ocosts(outfile,'(c22522)','PF coil instrumentation and control (M$)', &
            c22522)
       call ocosts(outfile,'(c22523)','PF coil bussing cost (M$)',c22523)
       call ocosts(outfile,'(c22524)','PF coil burn power supplies cost (M$)',c22524)
       call ocosts(outfile,'(c22525)','PF coil breakers cost (M$)',c22525)
       call ocosts(outfile,'(c22526)','PF coil dump resistors cost (M$)',c22526)
       call ocosts(outfile,'(c22527)','PF coil ac breakers cost (M$)',c22527)
       call ocosts(outfile,'(c2252)','Total, PF coil power costs (M$)',c2252)
       call ocosts(outfile,'(c2253)','Total, energy storage cost (M$)',c2253)
       call oblnkl(outfile)
       call ocosts(outfile,'(c225)','Total account 225 cost (M$)',c225)
    end if

    call oshead(outfile,'Heat Transport System')
    call ocosts(outfile,'(cpp)','Pumps and piping system cost (M$)',cpp)
    call ocosts(outfile,'(chx)','Primary heat exchanger cost (M$)',chx)
    call ocosts(outfile,'(c2261)','Total, reactor cooling system cost (M$)',c2261)
    call ocosts(outfile,'(cppa)','Pumps, piping cost (M$)',cppa)
    call ocosts(outfile,'(c2262)','Total, auxiliary cooling system cost (M$)',c2262)
    call ocosts(outfile,'(c2263)','Total, cryogenic system cost (M$)',c2263)
    call oblnkl(outfile)
    call ocosts(outfile,'(c226)','Total account 226 cost (M$)',c226)

    call oshead(outfile,'Fuel Handling System')
    call ocosts(outfile,'(c2271)','Fuelling system cost (M$)',c2271)
    call ocosts(outfile,'(c2272)','Fuel processing and purification cost (M$)',c2272)
    call ocosts(outfile,'(c2273)','Atmospheric recovery systems cost (M$)',c2273)
    call ocosts(outfile,'(c2274)','Nuclear building ventilation cost (M$)',c2274)
    call oblnkl(outfile)
    call ocosts(outfile,'(c227)','Total account 227 cost (M$)',c227)

    call oshead(outfile,'Instrumentation and Control')
    call ocosts(outfile,'(c228)','Instrumentation and control cost (M$)',c228)

    call oshead(outfile,'Maintenance Equipment')
    call ocosts(outfile,'(c229)','Maintenance equipment cost (M$)',c229)

    call oshead(outfile,'Total Account 22 Cost')
    call ocosts(outfile,'(c22)','Total account 22 cost (M$)',c22)

    call oshead(outfile,'Turbine Plant Equipment')
    call ocosts(outfile,'(c23)','Turbine plant equipment cost (M$)',c23)

    call oshead(outfile,'Electric Plant Equipment')
    call ocosts(outfile,'(c241)','Switchyard equipment cost (M$)',c241)
    call ocosts(outfile,'(c242)','Transformers cost (M$)',c242)
    call ocosts(outfile,'(c243)','Low voltage equipment cost (M$)',c243)
    call ocosts(outfile,'(c244)','Diesel backup equipment cost (M$)',c244)
    call ocosts(outfile,'(c245)','Auxiliary facilities cost (M$)',c245)
    call oblnkl(outfile)
    call ocosts(outfile,'(c24)','Total account 24 cost (M$)',c24)

    call oshead(outfile,'Miscellaneous Plant Equipment')
    call ocosts(outfile,'(c25)','Miscellaneous plant equipment cost (M$)',c25)

    call oshead(outfile,'Heat Rejection System')
    call ocosts(outfile,'(c26)','Heat rejection system cost (M$)',c26)

    call oshead(outfile,'Plant Direct Cost')
    call ocosts(outfile,'(cdirt)','Plant direct cost (M$)',cdirt)

    call oshead(outfile,'Reactor Core Cost')
    call ocosts(outfile,'(crctcore)','Reactor core cost (M$)',crctcore)

    call oshead(outfile,'Indirect Cost')
    call ocosts(outfile,'(c9)','Indirect cost (M$)',cindrt)

    call oshead(outfile,'Total Contingency')
    call ocosts(outfile,'(ccont)','Total contingency (M$)',ccont)

    call oshead(outfile,'Constructed Cost')
    call ocosts(outfile,'(concost)','Constructed cost (M$)',concost)

    if (ireactor == 1) then
       call oshead(outfile,'Interest during Construction')
       call ocosts(outfile,'(moneyint)','Interest during construction (M$)',moneyint)

       call oshead(outfile,'Total Capital Investment')
       call ocosts(outfile,'(capcost)','Total capital investment (M$)',capcost)
    end if

  end subroutine costs

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
      coefuelt, moneyint, cdrlife, capcost, cplife, fwallcst, fcr0, ratecdol, &
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

    feffwbl = (1.0D0 + ratecdol)**fwbllife

    !  Capital recovery factor

    crffwbl = (feffwbl*ratecdol) / (feffwbl-1.0D0)

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

       fefdiv = (1.0D0 + ratecdol)**divlife

       !  Capital recovery factor

       crfdiv = (fefdiv*ratecdol) / (fefdiv-1.0D0)

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

       fefcp = (1.0D0 + ratecdol)**cplife

       !  Capital recovery factor

       crfcp = (fefcp*ratecdol) / (fefcp-1.0D0)

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

    fefcdr = (1.0D0 + ratecdol)**cdrlife

    !  Capital recovery factor

    crfcdr = (fefcdr*ratecdol) / (fefcdr-1.0D0)

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
         (1.0D0+ratecdol-dintrt)**(tlife-dtlife)

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

200 format( &
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

  subroutine acc21

    !! Account 21 : Structures and site facilities
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 21 (structures and site
    !! facilities) costs.
    !! Building costs are scaled with volume according to algorithms
    !! developed from TFCX, TFTR, and commercial power plant buildings.
    !! Costs include equipment, materials and installation labour, but
    !! no engineering or construction management.
    !! <P>The general form of the cost algorithm is cost=ucxx*volume**expxx.
    !! Allowances are used for site improvements and for miscellaneous
    !! buildings and land costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use buildings_variables, only: shovol, triv, elevol, rbvol, cryvol, &
      rmbvol, admvol, convol, wsvol 
		use cost_variables, only: uctr, uccr, ucel, ucrb, ireactor, ucad, ucmb, &
      ucws, cturbb, ucsh, ucco, lsa, csi, cland
    implicit none

    !  Arguments

    !  Local variables

    real(dp), parameter :: exprb = 1.0D0
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6800D0
    cmlsa(2) = 0.8400D0
    cmlsa(3) = 0.9200D0
    cmlsa(4) = 1.0000D0

    !  Account 211 : Site improvements, facilities and land
    !  N.B. Land unaffected by LSA

    c211 = csi*cmlsa(lsa) + cland

    !  Account 212 : Reactor building

    c212 = 1.0D-6 * ucrb * rbvol**exprb * cmlsa(lsa)

    !  Account 213 : Turbine building

    if (ireactor == 1) then
       c213 = cturbb * cmlsa(lsa)
    else
       c213 = 0.0D0
    end if

    !  Account 214 : Reactor maintenance and warm shops buildings

    c2141 = 1.0D-6 * ucmb * rmbvol**exprb * cmlsa(lsa)
    c2142 = 1.0D-6 * ucws * wsvol**exprb * cmlsa(lsa)
    c214 = c2141 + c2142

    !  Account 215 : Tritium building

    c215 = 1.0D-6 * uctr * triv**exprb * cmlsa(lsa)

    !  Account 216 : Electrical equipment building

    c216 = 1.0D-6 * ucel * elevol**exprb * cmlsa(lsa)

    !  Account 217 : Other buildings
    !  Includes administration, control, shops, cryogenic
    !  plant and an allowance for miscellaneous structures

    c2171 = 1.0D-6 * ucad * admvol**exprb * cmlsa(lsa)
    c2172 = 1.0D-6 * ucco * convol**exprb * cmlsa(lsa)
    c2173 = 1.0D-6 * ucsh * shovol**exprb * cmlsa(lsa)
    c2174 = 1.0D-6 * uccr * cryvol**exprb * cmlsa(lsa)
    c217 = c2171 + c2172 + c2173 + c2174

    !  Total for Account 21

    c21 = c211 + c212 + c213 + c214 + c215 + c216 + c217

  end subroutine acc21

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc22
    !! Account 22 : Fusion power island
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22 (fusion power island
    !! - the tokamak itself plus auxiliary power systems, etc.) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: crctcore, c222, c221
    implicit none

    !  Account 221 : Reactor
    call acc221

    !  Account 222 : Magnets
    call acc222

    !  Account 223 : Power injection
    call acc223

    !  Account 224 : Vacuum system
    call acc224

    !  Account 225 : Power conditioning
    call acc225

    !  Account 226 : Heat transport system
    call acc2261    !  Account 2261 : Reactor cooling system
    call acc2262    !  Account 2262 : Auxiliary component coolin
    call acc2263    !  Account 2263 : Cryogenic system
    call acc226     !  Account 226  : Total

    !  Account 227 : Fuel handling
    call acc2271    !  Account 2271 : Fuelling system
    call acc2272    !  Account 2272 : Fuel processing and purification
    call acc2273    !  Account 2273 : Atmospheric recovery systems
    call acc2274    !  Account 2274 : Nuclear building ventilation
    call acc227     !  Account 227  : Total

    !  Account 228 : Instrumentation and control
    call acc228

    !  Account 229 : Maintenance equipment
    call acc229

    !  Reactor core costs
    crctcore = c221 + c222 + c223

    !  Total account 22
    c22 = c221 + c222 + c223 + c224 + c225 + c226 + c227 + c228 + c229

  end subroutine acc22

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc221

    !! Account 221 : Reactor
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 221 (reactor) costs.
    !! These include the first wall, blanket, shield, support structure
    !! and divertor plates.
    !! <P>If ifueltyp = 1, the first wall, blanket and divertor costs are
    !! treated as fuel costs, rather than as capital costs.
    !! <P>If ifueltyp = 2, the initial first wall, blanket and divertor costs are
    !! treated as capital costs, and replacemnts are included as fuel costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: c221 
    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 221.1 : First wall

    call acc2211

    !  Account 221.2 : Blanket

    call acc2212

    !  Account 221.3 : Shield

    call acc2213

    !  Account 221.4 : Reactor structure

    call acc2214

    !  Account 221.5 : Divertor

    call acc2215

    !  Total account 221

    c221 = c2211 + c2212 + c2213 + c2214 + c2215

  end subroutine acc221

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2211

    !! Account 221.1 : First wall
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 221.1 (first wall) costs.
    !! The first wall cost is scaled linearly with surface area from TFCX.
    !! If ifueltyp = 1, the first wall cost is treated as a fuel cost,
    !! rather than as a capital cost.
    !! If ifueltyp = 2, inital first wall is included as a capital cost,
    !! and the replacement first wall cost is treated as a fuel costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use build_variables, only: fwarea 
		use cost_variables, only: ucblss, ucfws, fkind, fwallcst, ucblli2o, &
      ifueltyp, ucfwps, ucfwa,lsa
		use ife_variables, only: fwmatm, uccarb, ife, ucconc
    implicit none

    !  Arguments

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  IFE plant material unit costs
    !  uccarb   : carbon cloth cost $/kg [50.0]
    !  ucconc   : concrete cost $/kg [0.1]
    !  fwmatm(J,I) : mass of material I in region J of first wall

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.5000D0
    cmlsa(2) = 0.7500D0
    cmlsa(3) = 0.8750D0
    cmlsa(4) = 1.0000D0

    if (ife /= 1) then
        c2211 = 1.0D-6 * cmlsa(lsa) * ((ucfwa+ucfws)*fwarea + ucfwps)
    else
        c2211 = 1.0D-6 * cmlsa(lsa) * ( &
              ucblss   * (fwmatm(1,1)+fwmatm(2,1)+fwmatm(3,1)) + &
              uccarb   * (fwmatm(1,2)+fwmatm(2,2)+fwmatm(3,2)) + &
              ucblli2o * (fwmatm(1,4)+fwmatm(2,4)+fwmatm(3,4)) + &
              ucconc   * (fwmatm(1,5)+fwmatm(2,5)+fwmatm(3,5)) )
    end if

    c2211 = fkind * c2211

    if (ifueltyp == 1) then
       fwallcst = c2211
       c2211 = 0.0D0
    elseif (ifueltyp == 2) then
       fwallcst = c2211
    else
       fwallcst = 0.0D0
    end if

  end subroutine acc2211

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2212

    !! Account 221.2 : Blanket
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 221.2 (blanket) costs.
    !! If ifueltyp = 1, the blanket cost is treated as a fuel cost,
    !! rather than as a capital cost.
    !! If ifueltyp = 2, the initial blanket is included as a capital cost
    !! and the replacement blanket costs are treated as a fuel cost.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucblss, ucblbreed, ucblbe, ucblli, ucblvd, &
      ucblli2o, blkcst, ucbllipb, ifueltyp, lsa, fkind
		use fwbs_variables, only: blktmodel, whtblli, blkttype, wtblli2o, &
      whtblbreed, whtblvd, whtblbe, whtblss, wtbllipb 
		use ife_variables, only: ucflib, blmatm, ife, ucconc, mflibe, uccarb
		use heat_transport_variables, only: ipowerflow 
    implicit none

    !  Arguments

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  IFE unit costs
    !  uccarb   : carbon cloth [50.0]
    !  ucconc   : concrete [0.1]
    !  ucflib   : FLiBe [84.0]
    !  mflibe   : Total mass of FLiBe (kg)
    !  blmatm(J,I) : mass of material I in region J of blanket

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.5000D0
    cmlsa(2) = 0.7500D0
    cmlsa(3) = 0.8750D0
    cmlsa(4) = 1.0000D0

    if (ife /= 1) then

       if (ipowerflow == 0) then
          c22121 = 1.0D-6 * whtblbe * ucblbe
          if (blktmodel == 0) then
             c22122 = 1.0D-6 * wtblli2o * ucblli2o
          else
             c22122 = 1.0D-6 * whtblbreed * ucblbreed
          end if
       else
          if ((blkttype == 1).or.(blkttype == 2)) then
             !  Liquid blanket (LiPb + Li)
             c22121 = 1.0D-6 * wtbllipb * ucbllipb
             c22122 = 1.0D-6 * whtblli * ucblli
          else
             !  Solid blanket (Li2O + Be)
             c22121 = 1.0D-6 * whtblbe * ucblbe
             c22122 = 1.0D-6 * wtblli2o * ucblli2o
          end if
       end if

       c22123 = 1.0D-6 * whtblss * ucblss
       c22124 = 1.0D-6 * whtblvd * ucblvd
       c22125 = 0.0D0
       c22126 = 0.0D0
       c22127 = 0.0D0

    else

       !  IFE blanket; materials present are Li2O, steel, carbon, concrete,
       !  FLiBe and lithium 
  
       c22121 = 0.0D0
       c22122 = 1.0D-6 * wtblli2o * ucblli2o
       c22123 = 1.0D-6 * whtblss * ucblss
       c22124 = 0.0D0
       c22125 = 1.0D-6 * uccarb * (blmatm(1,2)+blmatm(2,2)+blmatm(3,2))
       c22126 = 1.0D-6 * ucconc * (blmatm(1,5)+blmatm(2,5)+blmatm(3,5))
       c22127 = 1.0D-6 * ucflib * mflibe
       c22128 = 1.0D-6 * ucblli * whtblli
  
    end if

    c22121 = fkind * c22121 * cmlsa(lsa)
    c22122 = fkind * c22122 * cmlsa(lsa)
    c22123 = fkind * c22123 * cmlsa(lsa)
    c22124 = fkind * c22124 * cmlsa(lsa)
    c22125 = fkind * c22125 * cmlsa(lsa)
    c22126 = fkind * c22126 * cmlsa(lsa)
    c22127 = fkind * c22127 * cmlsa(lsa)

    c2212 = c22121 + c22122 + c22123 + c22124 + c22125 + c22126 + &
         c22127

    if (ifueltyp == 1) then
       blkcst = c2212
       c2212 = 0.0D0
    elseif (ifueltyp == 2) then
       blkcst = c2212
    else
       blkcst = 0.0D0
    end if

  end subroutine acc2212

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2213

    !! Account 221.3 : Shield
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 221.3 (shield) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucpens, ucshld, fkind, ucblli2o, lsa
		use fwbs_variables, only: wpenshld, whtshld 
		use ife_variables, only: shmatm, uccarb, ife, ucconc
    implicit none

    !  Arguments

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  IFE unit costs
    !  uccarb   : carbon cloth [50.0]
    !  ucconc   : concrete [0.1]
    !  shmatm(J,I) : mass of material I in region J of shield

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.5000D0
    cmlsa(2) = 0.7500D0
    cmlsa(3) = 0.8750D0
    cmlsa(4) = 1.0000D0

    if (ife /= 1) then
       c22131 = 1.0D-6 * whtshld * ucshld * cmlsa(lsa)
    else
       c22131 = 1.0D-6 * cmlsa(lsa) * ( &
              ucshld *   (shmatm(1,1)+shmatm(2,1)+shmatm(3,1)) + &
              uccarb *   (shmatm(1,2)+shmatm(2,2)+shmatm(3,2)) + &
              ucblli2o * (shmatm(1,4)+shmatm(2,4)+shmatm(3,4)) + &
              ucconc *   (shmatm(1,5)+shmatm(2,5)+shmatm(3,5)) )
    end if

    c22131 = fkind * c22131

    !  Penetration shield assumed to be typical steel plate
    if (ife /= 1) then
       c22132 = 1.0D-6 * wpenshld * ucpens * cmlsa(lsa)
    else
       c22132 = 0.0D0
    end if

    c22132 = fkind * c22132

    c2213 = c22131 + c22132

  end subroutine acc2213

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2214

    !! Account 221.4 : Reactor structure
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 221.4 (reactor structure) costs.
    !! The structural items are costed as standard steel elements.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: fkind, ucgss, lsa
		use structure_variables, only: gsmass 
    implicit none

    !  Arguments

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6700D0
    cmlsa(2) = 0.8350D0
    cmlsa(3) = 0.9175D0
    cmlsa(4) = 1.0000D0

    c2214 = 1.0D-6 * gsmass * ucgss * cmlsa(lsa)
    c2214 = fkind * c2214

  end subroutine acc2214

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2215

    !! Account 221.5 : Divertor
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 221.5 (divertor) costs.
    !! The cost of the divertor blade is scaled linearly with
    !! surface area from TFCX. The graphite armour is assumed to
    !! be brazed to water-cooled machined copper substrate.
    !! Tenth-of-a-kind engineering and installation is assumed.
    !! <P>If ifueltyp = 1, the divertor cost is treated as a fuel cost,
    !! rather than as a capital cost.
    !! <P>If ifueltyp = 2, the initial divertor is included as a capital cost
    !! and the replacement divertor costs ae treated as a fuel cost,
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ifueltyp, divcst, fkind, ucdiv 
		use divertor_variables, only: divsur 
		use ife_variables, only: ife 
    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ife /= 1) then
       c2215 = 1.0D-6 * divsur * ucdiv
       c2215 = fkind * c2215

       if (ifueltyp == 1) then
          divcst = c2215
          c2215 = 0.0D0
       elseif (ifueltyp == 2) then
          divcst = c2215 
       else
          divcst = 0.0D0
       end if
    else
       c2215 = 0.0D0
       divcst = 0.0D0
    end if

  end subroutine acc2215

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc222

    !! Account 222 : Magnets, including cryostat
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 222 (magnet) costs,
    !! including the costs of associated cryostats.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: c222 
		use ife_variables, only: ife 
    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ife == 1) then
      c222 = 0.0D0
      return
    end if

    !  Account 222.1 : TF magnet assemblies

    call acc2221

    !  Account 222.2 : PF magnet assemblies
     call acc2222

    !  Account 222.3 : Cryostat

    call acc2223

    !  Total account 222

    c222 = c2221 + c2222 + c2223

  end subroutine acc222

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2221

    !! Account 222.1 : TF magnet assemblies
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 222.1 (TF magnet) costs.
    !! Copper magnets are costed from the TFCX data base ($/kg).
    !! Superconductor magnets are costed using a new method devised
    !! by R. Hancox under contract to Culham Laboratory, Jan/Feb 1994.
    !! If ifueltyp = 1, the TART centrepost cost is treated as a fuel
    !! cost, rather than as a capital cost.
    !! If ifueltyp = 2, the  initial centrepost is included as a capital cost
    !! and the replacement TART centrepost costs are treated as a fuel
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: uccpclb, uccase, uccu, fkind, ucgss, ucint, &
      cconshtf, ucsc, ifueltyp, uccpcl1, ucwindtf, cpstcst, lsa, cconfix 
		use physics_variables, only: itart 
		use structure_variables, only: clgsmass, aintmass 
		use tfcoil_variables, only: whtconcu, whtconsc, whtcas, n_tf, whttflgs, &
      whtcp, i_tf_sup, n_tf_turn, tfleng, i_tf_sc_mat
    implicit none

    !  Arguments

    !  Local variables

    real(dp) :: costtfsc,costtfcu,costwire,ctfconpm
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6900D0
    cmlsa(2) = 0.8450D0
    cmlsa(3) = 0.9225D0
    cmlsa(4) = 1.0000D0

    if (i_tf_sup /= 1) then  !  Resistive TF coils

       !  Account 222.1.1 : Inboard TF coil legs

       c22211 = 1.0D-6 * whtcp * uccpcl1 * cmlsa(lsa)
       c22211 = fkind * c22211

       cpstcst = 0.0D0  !  TART centrepost
       if ((itart == 1).and.(ifueltyp == 1)) then
          cpstcst = c22211
          c22211 = 0.0D0
       elseif ((itart == 1).and.(ifueltyp == 2)) then
          cpstcst = c22211
       end if

       !  Account 222.1.2 : Outboard TF coil legs

       c22212 = 1.0D-6 * whttflgs * uccpclb * cmlsa(lsa)
       c22212 = fkind * c22212

       !  Total (copper) TF coil costs

       c2221 = c22211 + c22212

    else  !  Superconducting TF coils

       !  Account 222.1.1 : Conductor

       !  Superconductor ($/m)

       costtfsc = ucsc(i_tf_sc_mat) * whtconsc / (tfleng*n_tf_turn)

       !  Copper ($/m)

       costtfcu = uccu * whtconcu / (tfleng*n_tf_turn)

       !  Total cost/metre of superconductor and copper wire

       costwire = costtfsc + costtfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       ctfconpm = costwire + cconshtf + cconfix

       !  Total conductor costs

       c22211 = 1.0D-6 * ctfconpm * n_tf * tfleng * n_tf_turn
       c22211 = fkind * c22211 * cmlsa(lsa)

       !  Account 222.1.2 : Winding

       c22212 = 1.0D-6 * ucwindtf * n_tf * tfleng * n_tf_turn
       c22212 = fkind * c22212 * cmlsa(lsa)

       !  Account 222.1.3 : Case

       c22213 = 1.0D-6 * (whtcas*uccase) * n_tf
       c22213 = fkind * c22213 * cmlsa(lsa)

       !  Account 222.1.4 : Intercoil structure

       c22214 = 1.0D-6 * aintmass * ucint
       c22214 = fkind * c22214 * cmlsa(lsa)

       !  Account 222.1.5 : Gravity support structure

       c22215 = 1.0D-6 * clgsmass * ucgss
       c22215 = fkind * c22215 * cmlsa(lsa)

       !  Total (superconducting) TF coil costs

       c2221 = c22211 + c22212 + c22213 + c22214 + c22215

    end if

  end subroutine acc2221

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2222

    !! Account 222.2 : PF magnet assemblies
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 222.2 (PF magnet) costs.
    !! Conductor costs previously used an algorithm devised by R. Hancox,
    !! January 1994, under contract to Culham, which took into
    !! account the fact that the superconductor/copper ratio in
    !! the conductor is proportional to the maximum field that
    !! each coil will experience. Now, the input copper fractions
    !! are used instead.
    !! Maximum values for current, current density and field
    !! are used.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use build_variables, only: iohcl 
		use constants, only: twopi, dcopper
		use cost_variables, only: uccase, uccu, cconshpf, ucfnc, cconfix, ucsc, &
      ucwindpf, lsa, fkind
		use pfcoil_variables, only: rjconpf, ipfres, vfohc, nohc, turns, isumatpf, &
      whtpfs, ric, rpf, isumatoh, fcupfsu, fcuohsu, vf, awpoh 
		use structure_variables, only: fncmass 
		use tfcoil_variables, only: dcond
    implicit none

    !  Arguments

    !  Local variables

    real(dp) :: costpfcu,costpfsc,costpfsh,costwire,cpfconpm, &
         pfwndl
    real(dp), dimension(4) :: cmlsa
    integer :: i,npf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6900D0
    cmlsa(2) = 0.8450D0
    cmlsa(3) = 0.9225D0
    cmlsa(4) = 1.0000D0

    !  Total length of PF coil windings (m)

    pfwndl = 0.0D0
    do i = 1,nohc
       pfwndl = pfwndl + twopi*rpf(i)*turns(i)
    end do

    !  Account 222.2.1 : Conductor

    !  The following lines take care of resistive coils.
    !  costpfsh is the cost per metre of the steel conduit/sheath around
    !  each superconducting cable (so is zero for resistive coils)

    if (ipfres == 1) then
       costpfsh = 0.0D0
    else
       costpfsh = cconshpf
    end if

    !  Non-Central Solenoid coils

    if (iohcl == 1) then
       npf = nohc-1
    else
       npf = nohc
    end if

    c22221 = 0.0D0
    do i = 1,npf

       !  Superconductor ($/m)
       if (ipfres == 0) then
          costpfsc = ucsc(isumatpf) * (1.0D0-fcupfsu)*(1.0D0-vf(i)) * &
               abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcond(isumatpf)
       else
          costpfsc = 0.0D0
       end if

       !  Copper ($/m)
       if (ipfres == 0) then
          costpfcu = uccu * fcupfsu*(1.0D0-vf(i)) * &
               abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper
       else
          costpfcu = uccu * (1.0D0-vf(i)) * &
               abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper
       end if

       !  Total cost/metre of superconductor and copper wire

       costwire = costpfsc + costpfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       cpfconpm = costwire + costpfsh + cconfix

       !  Total account 222.2.1 (PF coils excluding Central Solenoid)

       c22221 = c22221 + (1.0D-6 * twopi * rpf(i) * turns(i) * &
            cpfconpm)

    end do

    !  Central Solenoid

    if (iohcl == 1) then

       !  Superconductor ($/m)
       !  Issue #328  Use CS conductor cross-sectional area (m2)
       if (ipfres == 0) then
          costpfsc = ucsc(isumatoh) * awpoh*(1-vfohc)*(1-fcuohsu)/turns(nohc) * dcond(isumatoh)
       else
          costpfsc = 0.0D0
       end if

       !  Copper ($/m)

       if (ipfres == 0) then
          costpfcu = uccu * awpoh*(1-vfohc)*fcuohsu/turns(nohc) * dcopper
       else
          ! MDK I don't know if this is ccorrect as we never use the resistive model
          costpfcu = uccu * awpoh*(1-vfohc)/turns(nohc) * dcopper
       end if

       !  Total cost/metre of superconductor and copper wire (Central Solenoid)

       costwire = costpfsc + costpfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       cpfconpm = costwire + costpfsh + cconfix

       !  Total account 222.2.1 (PF+Central Solenoid coils)

       c22221 = c22221 + (1.0D-6 * twopi * rpf(nohc) * turns(nohc) * &
            cpfconpm)

    end if

    c22221 = fkind * c22221 * cmlsa(lsa)

    !  Account 222.2.2 : Winding

    c22222 = 1.0D-6 * ucwindpf * pfwndl
    c22222 = fkind * c22222 * cmlsa(lsa)

    !  Account 222.2.3 : Steel case - will be zero for resistive coils

    c22223 = 1.0D-6 * uccase * whtpfs
    c22223 = fkind * c22223 * cmlsa(lsa)

    !  Account 222.2.4 : Support structure

    c22224 = 1.0D-6 * ucfnc * fncmass
    c22224 = fkind * c22224 * cmlsa(lsa)

    !  Total account 222.2

    c2222 = c22221 + c22222 + c22223 + c22224

  end subroutine acc2222

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2223

    !! Account 222.3 : Vacuum vessel
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 222.3 (vacuum vessel) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: uccryo, lsa, fkind
		use fwbs_variables, only: vvmass 
    implicit none

    !  Arguments

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6900D0
    cmlsa(2) = 0.8450D0
    cmlsa(3) = 0.9225D0
    cmlsa(4) = 1.0000D0

    c2223 = 1.0D-6 * vvmass * uccryo
    c2223 = fkind * c2223 * cmlsa(lsa)

  end subroutine acc2223

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc223

    !! Account 223 : Power injection
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 223 (power injection) costs.
    !! The costs are from TETRA, updated to 1990$.
    !! Nominal TIBER values are used pending system designs. Costs are
    !! scaled linearly with power injected into the plasma and include
    !! the power supplies.
    !! <P>If ifueltyp=1, the fraction (1-fcdfuel) of the cost of the
    !! current drive system is considered as capital cost, and the
    !! fraction (fcdfuel) is considered a recurring fuel cost due
    !! to the system's short life.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucich, fkind, ucnbi, ucech, uclh, ifueltyp, &
      cdcost, fcdfuel
		use current_drive_variables, only: plhybd, iefrf, echpwr, pnbitot 
		use ife_variables, only: dcdrv2, mcdriv, cdriv2, dcdrv0, edrive, etadrv, &
      ifedrv, ife, dcdrv1, cdriv1, cdriv3, cdriv0 
    implicit none

    !  Arguments

    !  Local variables

    real(dp), parameter :: exprf = 1.0D0
    real(dp) :: switch

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  IFE costs (deflated to 1990 dollars) taken from
    !  Meier and Bieri (OSIRIS heavy ion beam), Fus Tech 21 (1992) 1547
    !  Meier and von Rosenberg (SOMBRERO laser), Fus Tech 21 (1992) 1552
    !  cdriv0   : IFE generic/laser driver cost at edrive=0 (M$) [154.3]
    !  dcdrv0   : generic/laser driver cost gradient (M$/MJ) [111.4]
    !  cdriv1   : IFE low energy heavy ion beam driver cost
    !             extrapolated to edrive=0 (M$) [163.2]
    !  dcdrv1   : HIB driver cost gradient at low energy (M$/MJ) [78.0]
    !  cdriv2   : IFE high energy heavy ion beam driver cost
    !             extrapolated to edrive=0 (M$) [244.9]
    !  dcdrv2   : HIB driver cost gradient at high energy (M$/MJ) [59.9]
    !  mcdriv   : IFE driver cost multiplier [1.0]

    if (ife /= 1) then

           !  Account 223.1 : ECH

       c2231 = 1.0D-6 * ucech * (1.0D6*echpwr)**exprf
       if (ifueltyp == 1) c2231 = (1.0D0-fcdfuel) * c2231
       c2231 = fkind * c2231

       !  Account 223.2 : Lower Hybrid or ICH

       if (iefrf /= 2) then
          c2232 = 1.0D-6 * uclh * (1.0D6*plhybd)**exprf
       else
          c2232 = 1.0D-6 * ucich * (1.0D6*plhybd)**exprf
       end if
       if (ifueltyp == 1) c2232 = (1.0D0-fcdfuel) * c2232
       c2232 = fkind * c2232

       !  Account 223.3 : Neutral Beam

       ! c2233 = 1.0D-6 * ucnbi * (1.0D6*pnbeam)**exprf
       ! #327
       c2233 = 1.0D-6 * ucnbi * (1.0D6*pnbitot)**exprf
       if (ifueltyp == 1) c2233 = (1.0D0-fcdfuel) * c2233
       c2233 = fkind * c2233

    else

       !  IFE driver costs (depends on driver type)
       !  Assume offset linear form for generic and SOMBRERO types,
       !  or one of two offset linear forms for OSIRIS type

       if (ifedrv == 2) then
          if (dcdrv1 <= dcdrv2) then
             switch = 0.0D0
          else
             switch = (cdriv2-cdriv1)/(dcdrv1-dcdrv2)
          end if
          if (edrive <= switch) then
             c2231 = mcdriv * (cdriv1 + dcdrv1*1.0D-6*edrive)
          else
             c2231 = mcdriv * (cdriv2 + dcdrv2*1.0D-6*edrive)
          end if
       else if (ifedrv==3) then
             c2231 = mcdriv * 1.0D-6* cdriv3 * (edrive/etadrv)
       else
          c2231 = mcdriv * (cdriv0 + dcdrv0*1.0D-6*edrive)
       end if
  
       if (ifueltyp == 1) c2231 = (1.0D0-fcdfuel) * c2231
       c2231 = fkind * c2231
       c2232 = 0.0D0
       c2233 = 0.0D0
       c2234 = 0.0D0
  
    end if

    !  Total account 223

    c223 = c2231 + c2232 + c2233 + c2234
    cdcost = c223

  end subroutine acc223

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc224

    !! Account 224 : Vacuum system
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 224 (vacuum system) costs.
    !! The costs are scaled from TETRA reactor code runs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucduct, uctpmp, fkind, ucbpmp, ucvalv, ucvdsh, &
      uccpmp, ucviac 
		use vacuum_variables, only: dlscal, vacdshm, vpumpn, vcdimax, ntype, &
      nvduct 
    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 224.1 : High vacuum pumps

    if (ntype == 1) then
       c2241 = 1.0D-6 * vpumpn * uccpmp
    else
       c2241 = 1.0D-6 * vpumpn * uctpmp
    end if
    c2241 = fkind * c2241

    !  Account 224.2 : Backing pumps

    c2242 = 1.0D-6 * dble(nvduct) * ucbpmp
    c2242 = fkind * c2242

    !  Account 224.3 : Vacuum duct

    c2243 = 1.0D-6 * dble(nvduct) * dlscal * ucduct
    c2243 = fkind * c2243

    !  Account 224.4 : Valves

    c2244 = 1.0D-6 * 2.0D0 * dble(nvduct) * (vcdimax*1.2D0)**1.4D0 &
         * ucvalv
    c2244 = fkind * c2244

    !  Account 224.5 : Duct shielding

    c2245 = 1.0D-6 * dble(nvduct) * vacdshm * ucvdsh
    c2245 = fkind * c2245

    !  Account 224.6 : Instrumentation

    c2246 = 1.0D-6 * ucviac
    c2246 = fkind * c2246

    !  Total account 224

    c224 = c2241 + c2242 + c2243 + c2244 + c2245 + c2246

  end subroutine acc224

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc225

    !! Account 225 : Power conditioning
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 225 (power conditioning) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use ife_variables, only: ife 
    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ife == 1) then
       c225 = 0.0D0
    else

       !  Account 225.1 : TF coil power conditioning

       call acc2251

       !  Account 225.2 : PF coil power conditioning

       call acc2252

       !  Account 225.3 : Energy storage

       call acc2253

       !  Total account 225

       c225 = c2251 + c2252 + c2253

    end if

  end subroutine acc225

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2251

    !! Account 225.1 : TF coil power conditioning
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 225.1 (TF coil power
    !! conditioning) costs.
    !! Costs are developed based on the major equipment specification
    !! of the tfcpwr module.  A multiplier is used to account for bulk
    !! materials and installation.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: uctfsw, fkind, ucbus, uctfbr, uctfic, uctfps, &
      uctfbus, uctfgr, uctfdr
		use tfcoil_variables, only: vtfskv, tfcmw, tfbusl, estotftgj, i_tf_sup, &
      tfbusmas, tfckw, n_tf, cpttf
    implicit none

    !  Arguments

    !  Local variables

    real(dp), parameter :: expel = 0.7D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 225.1.1 : TF coil power supplies

    c22511 = 1.0D-6 * uctfps * (tfckw*1.0D3 + tfcmw*1.0D6)**expel
    c22511 = fkind * c22511

    !  Account 225.1.2 : TF coil breakers (zero cost for copper coils)

    if (i_tf_sup == 1) then
       c22512 = 1.0D-6 * (uctfbr*n_tf*(cpttf*vtfskv*1.0D3)**expel+ &
            uctfsw*cpttf)
    else
       c22512 = 0.0D0
    end if
    c22512 = fkind * c22512

    !  Account 225.1.3 : TF coil dump resistors

    c22513 = 1.0D-6 * (1.0D9*uctfdr*estotftgj + uctfgr * 0.5D0*n_tf)
    c22513 = fkind * c22513

    !  Account 225.1.4 : TF coil instrumentation and control

    c22514 = 1.0D-6 * uctfic * (30.0D0*n_tf)
    c22514 = fkind * c22514

    !  Account 225.1.5 : TF coil bussing

    if (i_tf_sup /= 1) then
       c22515 = 1.0D-6 * uctfbus * tfbusmas
    else
       c22515 = 1.0D-6 * ucbus * cpttf * tfbusl
    end if
    c22515 = fkind * c22515

    !  Total account 225.1

    c2251 = c22511 + c22512 + c22513 + c22514 + c22515

  end subroutine acc2251

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2252

    !! Account 225.2 : PF coil power conditioning
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 225.2 (PF coil power
    !! conditioning) costs.
    !! Costs are taken from the equipment specification of the
    !! <A HREF="pfpwr.html">pfpwr</A> routine from the plant power module.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucpfcb, ucpfbk, fkind, ucpfb, ucpfdr1, ucpfic, &
      ucpfbs, ucpfps 
		use heat_transport_variables, only: peakmva 
		use pf_power_variables, only: ensxpfm, spfbusl, pfckts, srcktpm, vpfskv, &
		  acptmax 
    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 225.2.1 : PF coil power supplies

    c22521 = 1.0D-6 * ucpfps * peakmva
    c22521 = fkind * c22521

    !  Account 225.2.2 : PF coil instrumentation and control

    c22522 = 1.0D-6 * ucpfic * pfckts * 30.0D0
    c22522 = fkind * c22522

    !  Account 225.2.3 : PF coil bussing

    c22523 = 1.0D-6 * ucpfb * spfbusl*acptmax
    c22523 = fkind * c22523

    !  Account 225.2.4 : PF coil burn power supplies

    if (pfckts /= 0.0D0) then
       c22524 = 1.0D-6 * ucpfbs * pfckts*(srcktpm/pfckts)**0.7D0
    else
       c22524 = 0.0D0
    end if
    c22524 = fkind * c22524

    !  Account 225.2.5 : PF coil breakers

    c22525 = 1.0D-6 * ucpfbk * pfckts*(acptmax*vpfskv)**0.7D0
    c22525 = fkind * c22525

    !  Account 225.2.6 : PF coil dump resistors

    c22526 = 1.0D-6 * ucpfdr1 * ensxpfm
    c22526 = fkind * c22526

    !  Account 225.2.7 : PF coil AC breakers

    c22527 = 1.0D-6 * ucpfcb * pfckts
    c22527 = fkind * c22527

    !  Total account 225.2

    c2252 = c22521 + c22522 + c22523 + c22524 + c22525 + &
         c22526 + c22527

  end subroutine acc2252

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2253

    !! Account 225.3 : Energy storage
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 225.3 (energy storage) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucblss, fkind 
		use error_handling, only: idiags, report_error
		use heat_transport_variables, only: pthermmw, pnetelmw 
		use pulse_variables, only: lpulse, dtstor, istore 
		use times_variables, only: tdown 
    implicit none

    !  Arguments

    !  Local variables

    ! real(dp), parameter :: expes = 0.8D0
    real(dp) :: shcss

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Energy storage costs set to zero for steady-state devices
    !  c2253 = 1.0d-6 * (uces1 * fmgmva**expes + uces2 * fmgmj**expes)

    c2253 = 0.0D0

    !  Thermal storage options for a pulsed reactor
    !  See F/MPE/MOD/CAG/PROCESS/PULSE/0008 and 0014

    if (lpulse == 1) then

       select case (istore)

       case (1)

          !  Option 1 from ELECTROWATT report
          !  Pulsed Fusion Reactor Study : AEA FUS 205

          !  Increased condensate tank capacity
          c2253 = 0.1D0

          !  Additional electrically-driven feedpump (50 per cent duty)
          c2253 = c2253 + 0.8D0

          !  Increased turbine-generator duty (5 per cent duty)
          c2253 = c2253 + 4.0D0

          !  Additional auxiliary transformer capacity and ancillaries
          c2253 = c2253 + 0.5D0

          !  Increased drum capacity
          c2253 = c2253 + 2.8D0

          !  Externally fired superheater
          c2253 = c2253 + 29.0D0

       case (2)

          !  Option 2 from ELECTROWATT report
          !  Pulsed Fusion Reactor Study : AEA FUS 205

          !  Increased condensate tank capacity
          c2253 = 0.1D0

          !  Additional electrically-driven feedpump (50 per cent duty)
          c2253 = c2253 + 0.8D0

          !  Increased drum capacity
          c2253 = c2253 + 2.8D0

          !  Increased turbine-generator duty (5 per cent duty)
          c2253 = c2253 + 4.0D0

          !  Additional fired boiler (1 x 100 per cent duty)
          c2253 = c2253 + 330.0D0

          !  HP/LP steam bypass system for auxiliary boiler
          !  (30 per cent boiler capacity)
          c2253 = c2253 + 1.0D0

          !  Dump condenser
          c2253 = c2253 + 2.0D0

          !  Increased cooling water system capacity
          c2253 = c2253 + 18.0D0

       case (3)

          !  Simplistic approach that assumes that a large stainless steel
          !  block acts as the thermal storage medium. No account is taken
          !  of the cost of the piping within the block, etc.
          !
          !  shcss is the specific heat capacity of stainless steel (J/kg/K)
          !  dtstor is the maximum allowable temperature change in the
          !  stainless steel block (input)

          shcss = 520.0D0
          c2253 = ucblss * (pthermmw * 1.0D6) * tdown / &
               (shcss * dtstor)

       case default
          idiags(1) = istore
          call report_error(125)

       end select

       if (istore < 3) then

          !  Scale c2253 with net electric power

          c2253 = c2253 * pnetelmw/1200.0D0

          !  It is necessary to convert from 1992 pounds to 1990 dollars
          !  Reasonable guess for the exchange rate + inflation factor
          !  inflation = 5% per annum; exchange rate = 1.5 dollars per pound

          c2253 = c2253 * 1.36D0

       end if

    end if

    c2253 = fkind * c2253

  end subroutine acc2253

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc226
    !! Account 226 : Heat transport system
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 226 (heat transport system) costs.
    !! Costs are estimated from major equipment and heat transport
    !! system loops developed in the heatpwr module of the code.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Total account 226

    c226 = c2261 + c2262 + c2263

  end subroutine acc226

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2261()
    !! Account 2261 : Reactor cooling system
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2261 - 
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucphx, uchts, lsa, fkind
		use fwbs_variables, only: coolwh, pnucshld, pnucblkt
		use heat_transport_variables, only: pthermmw, pfwdiv, nphx
    implicit none

    !  Local variables
    real(dp), parameter :: exphts = 0.7D0
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance
    cmlsa(1) = 0.4000D0
    cmlsa(2) = 0.7000D0
    cmlsa(3) = 0.8500D0
    cmlsa(4) = 1.0000D0

    !  Pumps and piping system
    !  N.B. with blktmodel > 0, the blanket is assumed to be helium-cooled,
    !  but the shield etc. is water-cooled (coolwh=2). Therefore, a slight
    !  inconsistency exists here...
    cpp = 1.0D-6 * uchts(coolwh) * ( (1.0D6*pfwdiv)**exphts + &
         (1.0D6*pnucblkt)**exphts + (1.0D6*pnucshld)**exphts)

    cpp = fkind * cpp * cmlsa(lsa)

    !  Primary heat exchangers
    chx = 1.0D-6 * ucphx * nphx * (1.0D6*pthermmw/nphx)**exphts
    chx = fkind * chx * cmlsa(lsa)

    c2261 = chx + cpp

  end subroutine acc2261

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2262()
    !! Account 2262 : Auxiliary component cooling
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2262 - Auxiliary component cooling
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: lsa, ucahts, fkind
		use ife_variables, only: tfacmw, ife, tdspmw
		use heat_transport_variables, only: pinjht, vachtmw, trithtmw, fachtmw, crypmw 
    implicit none 

    !  Local variables
    real(dp), parameter :: exphts = 0.7D0
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance
    cmlsa(1) = 0.4000D0
    cmlsa(2) = 0.7000D0
    cmlsa(3) = 0.8500D0
    cmlsa(4) = 1.0000D0

    !  Pumps and piping system
    cppa = 1.0D-6 * ucahts * ( (1.0D6*pinjht)**exphts + &
         (1.0D6*crypmw)**exphts + (1.0D6*vachtmw)**exphts + &
         (1.0D6*trithtmw)**exphts + (1.0D6*fachtmw)**exphts )

    if (ife == 1) cppa = cppa + 1.0D-6 * ucahts * ( &
         (1.0D6*tdspmw)**exphts + (1.0D6*tfacmw)**exphts )

    !  Apply Nth kind and safety assurance factors
    cppa = fkind * cppa * cmlsa(lsa)

    c2262 = cppa

  end subroutine acc2262

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2263()
    !! Account 2263 : Cryogenic system
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2263 - Cryogenic system
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: uccry, lsa, fkind
		use heat_transport_variables, only: helpow 
		use tfcoil_variables, only: tftmp 
    implicit none

    !  Local variables
    real(dp), parameter :: expcry = 0.67D0
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance
    cmlsa(1) = 0.4000D0
    cmlsa(2) = 0.7000D0
    cmlsa(3) = 0.8500D0
    cmlsa(4) = 1.0000D0

    c2263 = 1.0D-6 * uccry * 4.5D0/tftmp * helpow**expcry

    !  Apply Nth kind and safety factors
    c2263 = fkind * c2263 * cmlsa(lsa)

  end subroutine acc2263

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc227
    !! Account 227 : Fuel handling
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 227 (fuel handling) costs.
    !! Costs are scaled from TETRA reactor code runs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Total account 227
    c227 = c2271 + c2272 + c2273 + c2274

  end subroutine acc227

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2271()
    !! Account 2271 : Fuelling system
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2271 - Fuelling system
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucf1, fkind 
    implicit none

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 227.1 : Fuelling system
    c2271 = 1.0D-6 * ucf1

    !  Apply Nth kind factor
    c2271 = fkind * c2271
  
  end subroutine acc2271

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2272()
    !! Account 2272 : Fuel processing and purification
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2272 - Fuel processing
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use constants, only: umass 
		use cost_variables, only: ucfpr, fkind 
		use ife_variables, only: fburn, reprat, ife, gain, edrive
		use physics_variables, only: wtgpd, rndfuel, afuel
    implicit none

    real(dp) targtm

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ife /= 1) then
       !  Previous calculation, using qfuel in Amps:
       !  1.3 should have been afuel*umass/echarge*1000*s/day = 2.2
       !wtgpd = burnup * qfuel * 1.3D0

       !  New calculation: 2 nuclei * reactions/sec * kg/nucleus * g/kg * sec/day
       wtgpd = 2.0D0*rndfuel * afuel*umass*1000.0D0 * 86400.0D0
    else
       targtm = gain * edrive * 3.0D0 * 1.67D-27 * 1.0D3 / &
              (1.602D-19 * 17.6D6 * fburn)
       wtgpd = targtm * reprat * 86400.0D0
    end if

    !  Assumes that He3 costs same as tritium to process...
    c2272 = 1.0D-6 * ucfpr * (0.5D0 + 0.5D0*(wtgpd/60.0D0)**0.67D0)

    c2272 = fkind * c2272
 
  end subroutine acc2272

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2273()
    !! Account 2273 : Atmospheric recovery systems
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2273 - Atmospheric recovery systems
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use buildings_variables, only: wsvol, volrci
		use cost_variables, only: ucdtc, fkind 
		use physics_variables, only: ftrit 
    implicit none

    !  Local variables

    real(dp) cfrht

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! ? 
    cfrht = 1.0D5

    !  No detritiation needed if purely D-He3 reaction
    if (ftrit > 1.0D-3) then
       c2273 = 1.0D-6 * ucdtc * ( (cfrht/1.0D4)**0.6D0 * &
            (volrci + wsvol) )
    else
       c2273 = 0.0D0
    end if

    c2273 = fkind * c2273

  end subroutine acc2273

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2274()
    !! Account 2274 : Nuclear building ventilation
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 2274 - Nuclear building ventilation
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use buildings_variables, only: wsvol, volrci
		use cost_variables, only: ucnbv, fkind 
    implicit none

    !  Account 227.4 : Nuclear building ventilation
    c2274 = 1.0D-6 * ucnbv * (volrci + wsvol)**0.8D0

    !  Apply Nth kind factor
    c2274 = fkind * c2274

  end subroutine acc2274

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc228()
    !! Account 228 : Instrumentation and control
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 228 (instrumentation and
    !! control) costs.
    !! Costs are based on TFCX and INTOR.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: uciac, fkind 
    implicit none

    c228 = 1.0D-6 * uciac
    c228 = fkind * c228

  end subroutine acc228

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc229()
    !! Account 229 : Maintenance equipment
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 229 (maintenance equipment) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucme, fkind 
    implicit none

    c229 = 1.0D-6 * ucme
    c229 = fkind * c229

  end subroutine acc229

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc23()
    !! Account 23 : Turbine plant equipment
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 23 (turbine plant equipment) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucturb, ireactor 
		use fwbs_variables, only: coolwh 
		use heat_transport_variables, only: pgrossmw 
    implicit none

    !  Local variables

    real(dp), parameter :: exptpe = 0.83D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ireactor == 1) then
       c23 = 1.0D-6 * ucturb(coolwh) * (pgrossmw/1200.0D0)**exptpe
    end if

  end subroutine acc23

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc24()
    !! Account 24 : Electric plant equipment
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 24 (electric plant equipment) costs.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Total account 24
    c24 = c241 + c242 + c243 + c244 + c245

  end subroutine acc24
 
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc241()
    !! Account 241 : Electric plant equipment - switchyard
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 241 - switchyard
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucswyd, lsa
    implicit none

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance electrical
    cmlsa(1) = 0.5700D0
    cmlsa(2) = 0.7850D0
    cmlsa(3) = 0.8925D0
    cmlsa(4) = 1.0000D0

    !  Account 241 : Switchyard
    c241 = 1.0D-6 * ucswyd * cmlsa(lsa)
   
  end subroutine acc241

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc242()
    !! Account 242 : Electric plant equipment - Transformers
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 242 - Transformers
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucpp, lsa, ucap
		use heat_transport_variables, only: pacpmw, fcsht
    implicit none

    !  Local variables

    real(dp), parameter :: expepe = 0.9D0
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance electrical
    cmlsa(1) = 0.5700D0
    cmlsa(2) = 0.7850D0
    cmlsa(3) = 0.8925D0
    cmlsa(4) = 1.0000D0

    !  Account 242 : Transformers
    c242 = 1.0D-6 * (ucpp*(pacpmw*1.0D3)**expepe + ucap*(fcsht*1.0D3))

    !  Apply safety assurance factor
    c242 = c242 * cmlsa(lsa)
 
  end subroutine acc242

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc243()
    !! Account 243 : Electric plant equipment - Low voltage
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 243 - Low voltage
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: uclv, lsa
		use heat_transport_variables, only: tlvpmw 
    implicit none

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance electrical
    cmlsa(1) = 0.5700D0
    cmlsa(2) = 0.7850D0
    cmlsa(3) = 0.8925D0
    cmlsa(4) = 1.0000D0

    !  Account 243 : Low voltage
    !  (include 0.8 factor for transformer efficiency)
    c243 = 1.0D-6 * uclv * tlvpmw * 1.0D3 / 0.8D0 * cmlsa(lsa)

  end subroutine acc243

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc244()
    !! Account 244 : Electric plant equipment - Diesel generators
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 244 - Diesel generators
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucdgen, lsa
    implicit none

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance electrical
    cmlsa(1) = 0.5700D0
    cmlsa(2) = 0.7850D0
    cmlsa(3) = 0.8925D0
    cmlsa(4) = 1.0000D0

    !  Account 244 : Diesel generator (8 MW per generator,  assume 4 )
    c244 = 1.0D-6 * ucdgen * 4.0D0 * cmlsa(lsa)

  end subroutine acc244

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc245()
    !! Account 245 : Electric plant equipment - Aux facility power
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 245 - Aux facility power
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucaf, lsa
    implicit none

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance electrical
    cmlsa(1) = 0.5700D0
    cmlsa(2) = 0.7850D0
    cmlsa(3) = 0.8925D0
    cmlsa(4) = 1.0000D0

    !  Account 245 : Auxiliary facility power needs
    c245 = 1.0D-6 * ucaf * cmlsa(lsa)

  end subroutine acc245

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc25()
    !! Account 25 : Miscellaneous plant equipment
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 25 (miscellaneous plant
    !! equipment) costs, such as waste treatment.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ucmisc, lsa
    implicit none

    !  Local variables

    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.7700D0
    cmlsa(2) = 0.8850D0
    cmlsa(3) = 0.9425D0
    cmlsa(4) = 1.0000D0

    c25 = 1.0D-6 * ucmisc * cmlsa(lsa)

  end subroutine acc25

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc26()
    !! Account 26 : Heat rejection system
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 26 (heat rejection system) costs.
    !! Costs are scaled with the total plant heat rejection based on
    !! commercial systems.
    !! J. Delene, private communication, ORNL, June 1990
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: ireactor, uchrs, lsa
		use heat_transport_variables, only: pthermmw, pinjwp, pgrossmw
		use physics_variables, only: powfmw 
		use tfcoil_variables, only: tfcmw 
    implicit none

    !  Local variables

    real(dp) :: pwrrej
    real(dp), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.8000D0
    cmlsa(2) = 0.9000D0
    cmlsa(3) = 0.9500D0
    cmlsa(4) = 1.0000D0

    ! Calculate rejected heat for non-reactor (==0) and reactor (==1)
    if (ireactor == 0) then
       pwrrej = powfmw + pinjwp + tfcmw
    else
       pwrrej = pthermmw - pgrossmw
    end if

    ! uchrs - reference cost of heat rejection system [$]
    c26 = 1.0D-6 * uchrs * pwrrej/2300.0D0 * cmlsa(lsa)

  end subroutine acc26

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc9()
    !! Account 9 : Indirect cost and contingency allowances
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 9 (indirect cost and
    !! contingency allowances) costs.
    !! The cost modelling is based on the commercial plant model of a
    !! single contractor performing all plant engineering and construction
    !! management, using commercially purchased equipment and materials.
    !! <P>The project contingency is an allowance for incomplete design
    !! specification and unforeseen events during the plant construction.
    !! <P>The factors used are estimated from commercial plant experience.
    !! J. Delene, private communication, ORNL, June 1990
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: fcontng, lsa, cowner, cdirt, cfind
    implicit none

    !  Indirect costs

    cindrt = cfind(lsa) * cdirt * (1.0D0 + cowner)

    !  Contingency costs

    ccont = fcontng * (cdirt + cindrt)

  end subroutine acc9

end module costs_module
