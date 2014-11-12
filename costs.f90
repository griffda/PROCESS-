! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_module

  !+ad_name  costs_module
  !+ad_summ  Module containing fusion power plant costing algorithms
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  costs
  !+ad_cont  coelc
  !+ad_cont  acc21
  !+ad_cont  acc22
  !+ad_cont  acc221
  !+ad_cont  acc2211
  !+ad_cont  acc2212
  !+ad_cont  acc2213
  !+ad_cont  acc2214
  !+ad_cont  acc2215
  !+ad_cont  acc222
  !+ad_cont  acc2221
  !+ad_cont  acc2222
  !+ad_cont  acc2221
  !+ad_cont  acc2223
  !+ad_cont  acc223
  !+ad_cont  acc224
  !+ad_cont  acc225
  !+ad_cont  acc2251
  !+ad_cont  acc2252
  !+ad_cont  acc2253
  !+ad_cont  acc226
  !+ad_cont  acc227
  !+ad_cont  acc228
  !+ad_cont  acc229
  !+ad_cont  acc23
  !+ad_cont  acc24
  !+ad_cont  acc25
  !+ad_cont  acc26
  !+ad_cont  acchyd
  !+ad_cont  acc9
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS fusion power plant costing model,
  !+ad_desc  split into separate cost accounts.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  constants
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  pulse_variables
  !+ad_call  rfp_variables
  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_variables
  !+ad_hist  15/10/12 PJK Initial version of module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added fwbs_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  29/10/12 PJK Added structure_variables
  !+ad_hist  29/10/12 PJK Added vacuum_variables
  !+ad_hist  29/10/12 PJK Added pf_power_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added buildings_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added cost_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_hist  05/11/12 PJK Added pulse_variables
  !+ad_hist  30/06/14 PJK Added error_handling
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use constants
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use ife_variables
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output
  use pulse_variables
  use rfp_variables
  use structure_variables
  use tfcoil_variables
  use times_variables
  use vacuum_variables

  implicit none

  private
  public :: costs

  !  Various cost account values (M$)

  real(kind(1.0D0)) :: &
       c21,c211,c212,c213,c214,c2141,c2142,c215,c216,c217,c2171, &
       c2172,c2173,c2174,c22,c2211,c2212,c22121,c22122,c22123, &
       c22124,c22125,c22126,c22127,c2213,c22131,c22132,c2214,c2215, &
       c2221,c22211,c22212,c22213,c22214,c22215,c2222,c22221,c22222, &
       c22223,c22224,c2223,c223,c2231,c2232,c2233,c2234,c224,c2241, &
       c2242,c2243,c2244,c2245,c2246,c225,c2251,c22511,c22512,c22513, &
       c22514,c22515,c2252,c22521,c22522,c22523,c22524,c22525,c22526, &
       c22527,c2253,c226,c2261,c2262,c2263,c227,c2271,c2272,c2273, &
       c2274,c228,c229,c23,c24,c241,c242,c243,c244,c245,c25,c26,ccont, &
       chx,chxa,cindrt,cpp,cppa

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine costs(outfile,iprint)

    !+ad_name  costs
    !+ad_summ  Cost accounting for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the cost accounting for a fusion power plant.
    !+ad_desc  The direct costs are calculated based on parameters input
    !+ad_desc  from other sections of the code.
    !+ad_desc  <P>Costs are in 1990 $, and assume first-of-a-kind components
    !+ad_desc  unless otherwise stated. Account 22 costs include a multiplier
    !+ad_desc  to account for Nth-of-a-kind cost reductions.
    !+ad_desc  <P>The code is arranged in the order of the standard accounts.
    !+ad_prob  None
    !+ad_call  acc21
    !+ad_call  acc22
    !+ad_call  acc23
    !+ad_call  acc24
    !+ad_call  acc25
    !+ad_call  acc26
    !+ad_call  acchyd
    !+ad_call  acc9
    !+ad_call  coelc
    !+ad_call  oblnkl
    !+ad_call  ocosts
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  22/11/12 PJK Typo fix in comment
    !+ad_hist  18/06/13 PJK Changed cryostat to vacuum vessel for c2223
    !+ad_hist  17/02/14 PJK Output format modifications
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 21 : Structures and site facilities

    call acc21

    !  Account 22 : Fusion power island

    call acc22

    !  Account 23 : Turbine plant equipment

    call acc23

    !  Account 24 : Electric plant equipment

    call acc24

    !  Account 25 : Miscellaneous plant equipment

    call acc25

    !  Account 26 : Heat rejection system

    call acc26

    !  Hydrogen Production Plant

    call acchyd

    !  Total plant direct cost

    cdirt = c21 + c22 + c23 + c24 + c25 + c26 + chplant

    !  Account 9 : Indirect cost and project contingency

    call acc9

    !  Constructed cost

    concost = cdirt + cindrt + ccont

    !  Cost of electricity

    if ((ireactor == 1).and.(ipnet == 0)) call coelc(outfile,iprint)

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'Detailed Costings')
    call ovarre(outfile,'Acc.22 multiplier for Nth of a kind','(fkind)', &
         fkind)
    call ovarin(outfile,'Level of Safety Assurance','(lsa)',lsa)
    call oblnkl(outfile)
    call ocosts(outfile,'211','Site improvements, facilities, land (M$)',c211)
    call ocosts(outfile,'212','Reactor building cost (M$)',c212)
    call ocosts(outfile,'213','Turbine building cost (M$)',c213)
    call ocosts(outfile,'2141','Reactor maintenance building cost (M$)',c2141)
    call ocosts(outfile,'2142','Warm shop cost (M$)',c2142)
    call ocosts(outfile,'215','Tritium building cost (M$)',c215)
    call ocosts(outfile,'216','Electrical equipment building cost (M$)',c216)
    call ocosts(outfile,'2171','Additional buildings cost (M$)',c2171)
    call ocosts(outfile,'2172','Control room buildings cost (M$)',c2172)
    call ocosts(outfile,'2173','Shop and warehouses cost (M$)',c2173)
    call ocosts(outfile,'2174','Cryogenic building cost (M$)',c2174)
    call oblnkl(outfile)
    call ocosts(outfile,'21','Total account 21 cost (M$)',c21)

    call oshead(outfile,'Reactor Systems')
    call ocosts(outfile,'2211','First wall cost (M$)',c2211)

    if (ife /= 1) then
       if (lblnkt == 1) then

          if (smstr == 1) then
             call ocosts(outfile,'22121','Blanket beryllium cost (M$)',c22121)
             call ocosts(outfile,'22122','Blanket lithium oxide cost (M$)',c22122)
             call ocosts(outfile,'22123','Blanket stainless steel cost (M$)', &
                  c22123)
             call ocosts(outfile,'22124','Blanket vanadium cost (M$)',c22124)
          else
             call ocosts(outfile,'22121','Blanket lithium-lead cost (M$)',c22121)
             call ocosts(outfile,'22122','Blanket lithium cost (M$)',c22122)
             call ocosts(outfile,'22123','Blanket stainless steel cost (M$)', &
                  c22123)
             call ocosts(outfile,'22124','Blanket vanadium cost (M$)',c22124)
          end if

       else
          call ocosts(outfile,'22121','Blanket beryllium cost (M$)',c22121)
          call ocosts(outfile,'22122','Blanket breeder material cost (M$)',c22122)
          call ocosts(outfile,'22123','Blanket stainless steel cost (M$)',c22123)
          call ocosts(outfile,'22124','Blanket vanadium cost (M$)',c22124)
       end if
    else  !  IFE
       call ocosts(outfile,'22121','Blanket beryllium cost (M$)',c22121)
       call ocosts(outfile,'22122','Blanket lithium oxide cost (M$)',c22122)
       call ocosts(outfile,'22123','Blanket stainless steel cost (M$)',c22123)
       call ocosts(outfile,'22124','Blanket vanadium cost (M$)',c22124)
       call ocosts(outfile,'22125','Blanket carbon cloth cost (M$)',c22125)
       call ocosts(outfile,'22126','Blanket concrete cost (M$)',c22126)
       call ocosts(outfile,'22127','Blanket FLiBe cost (M$)',c22127)
    end if

    call ocosts(outfile,'2212','Blanket total cost (M$)',c2212)
    call ocosts(outfile,'22131','Bulk shield cost (M$)',c22131)
    call ocosts(outfile,'22132','Penetration shielding cost (M$)',c22132)
    call ocosts(outfile,'2213','Total shield cost (M$)',c2213)
    call ocosts(outfile,'2214','Total support structure cost (M$)',c2214)
    call ocosts(outfile,'2215','Divertor cost (M$)',c2215)
    if (ifueltyp == 1) then
       call oblnkl(outfile)
       write(outfile,20)
20     format(t2, &
            'First wall, total blanket and divertor direct costs',/, &
            t2,'are zero as they are assumed to be fuel costs.')
    end if

    call oblnkl(outfile)
    call ocosts(outfile,'221','Total account 221 cost (M$)',c221)

    if (ife /= 1) then

       call oshead(outfile,'Magnets')

       if (itfsup == 0) then  !  Resistive TF coils
          if (itart == 1) then
             call ocosts(outfile,'22211','Centrepost costs (M$)',c22211)
          else
             call ocosts(outfile,'22211','Inboard leg cost (M$)',c22211)
          end if
          call ocosts(outfile,'22212','Outboard leg cost (M$)',c22212)
          call ocosts(outfile,'2221','TF magnet assemblies cost (M$)',c2221)
       else  !  Superconducting TF coils
          call ocosts(outfile,'22211','TF coil conductor cost (M$)',c22211)
          call ocosts(outfile,'22212','TF coil winding cost (M$)',c22212)
          call ocosts(outfile,'22213','TF coil case cost (M$)',c22213)
          call ocosts(outfile,'22214','TF intercoil structure cost (M$)',c22214)
          call ocosts(outfile,'22215','TF coil gravity support structure (M$)', &
               c22215)
          call ocosts(outfile,'2221','TF magnet assemblies cost (M$)',c2221)
       end if

       call ocosts(outfile,'22221','PF coil conductor cost (M$)',c22221)
       call ocosts(outfile,'22222','PF coil winding cost (M$)',c22222)
       call ocosts(outfile,'22223','PF coil case cost (M$)',c22223)
       call ocosts(outfile,'22224','PF coil support structure cost (M$)',c22224)
       call ocosts(outfile,'2222','PF magnet assemblies cost (M$)',c2222)
       call ocosts(outfile,'2223','Vacuum vessel assembly cost (M$)',c2223)

       if ((itart == 1).and.(ifueltyp == 1)) then
          call oblnkl(outfile)
          write(outfile,30)
30        format(t2, &
               'Centrepost direct cost is zero, as it ', &
               'is assumed to be a fuel cost.')
       end if

       call oblnkl(outfile)
       call ocosts(outfile,'222','Total account 222 cost (M$)',c222)

    end if

    call oshead(outfile,'Power Injection')

    if (ife == 1) then
       call ocosts(outfile,'2231','IFE driver system cost (M$)',c2231)
    else
       call ocosts(outfile,'2231','ECH system cost (M$)',c2231)
       call ocosts(outfile,'2232','Lower hybrid system cost (M$)',c2232)
       call ocosts(outfile,'2233','Neutral beam system cost (M$)',c2233)
       if (irfp == 1) then
          call ocosts(outfile,'2234','Oscillating field system cost (M$)',c2234)
       end if
    end if

    call oblnkl(outfile)
    call ocosts(outfile,'223','Total account 223 cost (M$)',c223)

    call oshead(outfile,'Vacuum Systems')
    call ocosts(outfile,'2241','High vacuum pumps cost (M$)',c2241)
    call ocosts(outfile,'2242','Backing pumps cost (M$)',c2242)
    call ocosts(outfile,'2243','Vacuum duct cost (M$)',c2243)
    call ocosts(outfile,'2244','Valves cost (M$)',c2244)
    call ocosts(outfile,'2245','Duct shielding cost (M$)',c2245)
    call ocosts(outfile,'2246','Instrumentation cost (M$)',c2246)
    call oblnkl(outfile)
    call ocosts(outfile,'224','Total account 224 cost (M$)',c224)

    if (ife /= 1) then

       call oshead(outfile,'Power Conditioning')
       call ocosts(outfile,'22511','TF coil power supplies cost (M$)',c22511)
       call ocosts(outfile,'22512','TF coil breakers cost (M$)',c22512)
       call ocosts(outfile,'22513','TF coil dump resistors cost (M$)',c22513)
       call ocosts(outfile,'22514','TF coil instrumentation and control (M$)', &
            c22514)
       call ocosts(outfile,'22515','TF coil bussing cost (M$)',c22515)
       call ocosts(outfile,'2251','Total, TF coil power costs (M$)',c2251)
       call ocosts(outfile,'22521','PF coil power supplies cost (M$)',c22521)
       call ocosts(outfile,'22522','PF coil instrumentation and control (M$)', &
            c22522)
       call ocosts(outfile,'22523','PF coil bussing cost (M$)',c22523)
       call ocosts(outfile,'22524','PF coil burn power supplies cost (M$)',c22524)
       call ocosts(outfile,'22525','PF coil breakers cost (M$)',c22525)
       call ocosts(outfile,'22526','PF coil dump resistors cost (M$)',c22526)
       call ocosts(outfile,'22527','PF coil ac breakers cost (M$)',c22527)
       call ocosts(outfile,'2252','Total, PF coil power costs (M$)',c2252)
       call ocosts(outfile,'2253','Total, energy storage cost (M$)',c2253)
       call oblnkl(outfile)
       call ocosts(outfile,'225','Total account 225 cost (M$)',c225)

    end if

    call oshead(outfile,'Heat Transport System')
    call ocosts(outfile,'cpp','Pumps and piping system cost (M$)',cpp)
    call ocosts(outfile,'chx','Primary heat exchanger cost (M$)',chx)
    call ocosts(outfile,'2261','Total, reactor cooling system cost (M$)',c2261)
    call ocosts(outfile,'cppa','Pumps, piping cost (M$)',cppa)
    call ocosts(outfile,'chxa','Heat exchanger cost (M$)',chxa)
    call ocosts(outfile,'2262','Total, auxiliary cooling system cost (M$)',c2262)
    call ocosts(outfile,'2263','Total, cryogenic system cost (M$)',c2263)
    call oblnkl(outfile)
    call ocosts(outfile,'226','Total account 226 cost (M$)',c226)

    call oshead(outfile,'Fuel Handling System')
    call ocosts(outfile,'2271','Fuelling system cost (M$)',c2271)
    call ocosts(outfile,'2272','Fuel processing and purification cost (M$)',c2272)
    call ocosts(outfile,'2273','Atmospheric recovery systems cost (M$)',c2273)
    call ocosts(outfile,'2274','Nuclear building ventilation cost (M$)',c2274)
    call oblnkl(outfile)
    call ocosts(outfile,'227','Total account 227 cost (M$)',c227)

    call oshead(outfile,'Instrumentation and Control')
    call ocosts(outfile,'228','Instrumentation and control cost (M$)',c228)

    call oshead(outfile,'Maintenance Equipment')
    call ocosts(outfile,'229','Maintenance equipment cost (M$)',c229)

    call oshead(outfile,'Total Account 22 Cost')
    call ocosts(outfile,'22','Total account 22 cost (M$)',c22)

    call oshead(outfile,'Turbine Plant Equipment')
    call ocosts(outfile,'23','Turbine plant equipment cost (M$)',c23)

    call oshead(outfile,'Electric Plant Equipment')
    call ocosts(outfile,'241','Switchyard equipment cost (M$)',c241)
    call ocosts(outfile,'242','Transformers cost (M$)',c242)
    call ocosts(outfile,'243','Low voltage equipment cost (M$)',c243)
    call ocosts(outfile,'244','Diesel backup equipment cost (M$)',c244)
    call ocosts(outfile,'245','Auxiliary facilities cost (M$)',c245)
    call oblnkl(outfile)
    call ocosts(outfile,'24','Total account 24 cost (M$)',c24)

    call oshead(outfile,'Miscellaneous Plant Equipment')
    call ocosts(outfile,'25','Miscellaneous plant equipment cost (M$)',c25)

    call oshead(outfile,'Heat Rejection System')
    call ocosts(outfile,'26','Heat rejection system cost (M$)',c26)

    if (ihplant /= 0) then
       call oshead(outfile,'Hydrogen Production')
       call ocosts(outfile,' ','Hydrogen production plant cost (M$)',chplant)
    end if

    call oshead(outfile,'Plant Direct Cost')
    call ocosts(outfile,'cdirt','Plant direct cost (M$)',cdirt)

    call oshead(outfile,'Reactor Core Cost')
    call ocosts(outfile,'crctcore','Reactor core cost (M$)',crctcore)

    call oshead(outfile,'Indirect Cost')
    call ocosts(outfile,'c9','Indirect cost (M$)',cindrt)

    call oshead(outfile,'Total Contingency')
    call ocosts(outfile,'ccont','Total contingency (M$)',ccont)

    call oshead(outfile,'Constructed Cost')
    call ocosts(outfile,'concost','Constructed cost (M$)',concost)

    if (ireactor == 1) then
       call oshead(outfile,'Interest during Construction')
       call ocosts(outfile,'moneyint','Interest during construction (M$)',moneyint)

       call oshead(outfile,'Total Capital Investment')
       call ocosts(outfile,'capcost','Total capital investment (M$)',capcost)
    end if

  end subroutine costs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine coelc(outfile,iprint)

    !+ad_name  coelc
    !+ad_summ  Routine to calculate the cost of electricity for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the calculation of the cost of electricity
    !+ad_desc  for a fusion power plant.
    !+ad_desc  <P>Annual costs are in megadollars/year, electricity costs are in
    !+ad_desc  millidollars/kWh, while other costs are in megadollars.
    !+ad_desc  All values are based on 1990 dollars.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_call  osubhd
    !+ad_call  ovarrf
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  11/09/13 PJK Modified annfuel cost calculation
    !+ad_hist  17/02/14 PJK Modified output format for some quantities
    !+ad_hist  15/05/14 PJK Longer output line lengths
    !+ad_hist  05/06/14 PJK Moved some power outputs to plant_power.f90
    !+ad_hist  16/06/14 PJK Removed duplicate outputs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  12/11/14 PJK tburn factor incorporated into cost of electricity
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
         annfuelt,annfwbl,annoam,annoam1,anntot,annwst,coecdr, &
         coecp,coedecom,coediv,coefuel,coefwbl,coewst,crfcdr,crfcp, &
         crfdiv,crffwbl,fefcdr,fefcp,fefdiv,feffwbl,fwbllife,kwhpy

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Number of kWh generated each year

    kwhpy = 1.0D3 * pnetelmw * (24.0D0*365.0D0) * cfactr * tburn/tcycle

    !  Costs due to reactor plant
    !  ==========================

    !  Interest on construction costs

    moneyint = concost * (fcap0 - 1.0D0)

    !  Capital costs

    capcost = concost + moneyint

    !  Annual cost of plant capital cost

    anncap = capcost * fcr0

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
            1.0D-6 * fhe3 * wtgpd * 1.0D-3 * uche3 * 365.0D0 * cfactr
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

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'Power Reactor Costs')

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

    !+ad_name  acc21
    !+ad_summ  Account 21 : Structures and site facilities
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 21 (structures and site
    !+ad_desc  facilities) costs.
    !+ad_desc  Building costs are scaled with volume according to algorithms
    !+ad_desc  developed from TFCX, TFTR, and commercial power plant buildings.
    !+ad_desc  Costs include equipment, materials and installation labour, but
    !+ad_desc  no engineering or construction management.
    !+ad_desc  <P>The general form of the cost algorithm is cost=ucxx*volume**expxx.
    !+ad_desc  Allowances are used for site improvements and for miscellaneous
    !+ad_desc  buildings and land costs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: exprb = 1.0D0
    real(kind(1.0D0)), dimension(4) :: cmlsa

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

    !+ad_name  acc22
    !+ad_summ  Account 22 : Fusion power island
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 22 (fusion power island
    !+ad_desc  - the tokamak itself plus auxiliary power systems, etc.) costs.
    !+ad_prob  None
    !+ad_call  acc221
    !+ad_call  acc222
    !+ad_call  acc223
    !+ad_call  acc224
    !+ad_call  acc225
    !+ad_call  acc226
    !+ad_call  acc227
    !+ad_call  acc228
    !+ad_call  acc229
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    call acc226

    !  Account 227 : Fuel handling

    call acc227

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

    !+ad_name  acc221
    !+ad_summ  Account 221 : Reactor
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 221 (reactor) costs.
    !+ad_desc  These include the first wall, blanket, shield, support structure
    !+ad_desc  and divertor plates.
    !+ad_desc  <P>If ifueltyp = 1, the first wall, blanket and divertor costs are
    !+ad_desc  treated as fuel costs, rather than as capital costs.
    !+ad_prob  None
    !+ad_call  acc2211
    !+ad_call  acc2212
    !+ad_call  acc2213
    !+ad_call  acc2214
    !+ad_call  acc2215
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !+ad_name  acc2211
    !+ad_summ  Account 221.1 : First wall
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 221.1 (first wall) costs.
    !+ad_desc  The first wall cost is scaled linearly with surface area from TFCX.
    !+ad_desc  If ifueltyp = 1, the first wall cost is treated as a fuel cost,
    !+ad_desc  rather than as a capital cost.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  03/12/13 PJK Removed old comment about ucfwps; multiplied
    !+ad_hisc               all terms by cmlsa, not just the ucfwps term
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

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
    else
       fwallcst = 0.0D0
    end if

  end subroutine acc2211

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2212

    !+ad_name  acc2212
    !+ad_summ  Account 221.2 : Blanket
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 221.2 (blanket) costs.
    !+ad_desc  If ifueltyp = 1, the blanket cost is treated as a fuel cost,
    !+ad_desc  rather than as a capital cost.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  03/06/13 PJK Added blktmodel>0 breeder cost
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

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

       if (lblnkt == 1) then
          if (smstr == 1) then
             !  Solid blanket (Li2O + Be)
             c22121 = 1.0D-6 * whtblbe * ucblbe
             c22122 = 1.0D-6 * wtblli2o * ucblli2o
          else
             !  Liquid blanket (LiPb + Li)
             c22121 = 1.0D-6 * wtbllipb * ucbllipb
             c22122 = 1.0D-6 * whtblli * ucblli
          end if
       else
          c22121 = 1.0D-6 * whtblbe * ucblbe
          if (blktmodel == 0) then
             c22122 = 1.0D-6 * wtblli2o * ucblli2o
          else
             c22122 = 1.0D-6 * whtblbreed * ucblbreed
          end if
       end if

       c22123 = 1.0D-6 * whtblss * ucblss
       c22124 = 1.0D-6 * whtblvd * ucblvd
       c22125 = 0.0D0
       c22126 = 0.0D0
       c22127 = 0.0D0

    else

       !  IFE blanket; materials present are Li2O, steel, carbon, concrete
       !  and FLiBe

       c22121 = 0.0D0
       c22122 = 1.0D-6 * wtblli2o * ucblli2o
       c22123 = 1.0D-6 * whtblss * ucblss
       c22124 = 0.0D0
       c22125 = 1.0D-6 * uccarb * (blmatm(1,2)+blmatm(2,2)+blmatm(3,2))
       c22126 = 1.0D-6 * ucconc * (blmatm(1,5)+blmatm(2,5)+blmatm(3,5))
       c22127 = 1.0D-6 * ucflib * mflibe

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
    else
       blkcst = 0.0D0
    end if

  end subroutine acc2212

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2213

    !+ad_name  acc2213
    !+ad_summ  Account 221.3 : Shield
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 221.3 (shield) costs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

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

    !+ad_name  acc2214
    !+ad_summ  Account 221.4 : Reactor structure
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 221.4 (reactor structure) costs.
    !+ad_desc  The structural items are costed as standard steel elements.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

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

    !+ad_name  acc2215
    !+ad_summ  Account 221.5 : Divertor
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 221.5 (divertor) costs.
    !+ad_desc  The cost of the divertor blade is scaled linearly with
    !+ad_desc  surface area from TFCX. The graphite armour is assumed to
    !+ad_desc  be brazed to water-cooled machined copper substrate.
    !+ad_desc  Tenth-of-a-kind engineering and installation is assumed.
    !+ad_desc  <P>If ifueltyp = 1, the divertor cost is treated as a fuel cost,
    !+ad_desc  rather than as a capital cost.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ife /= 1) then

       c2215 = 1.0D-6 * divsur * ucdiv
       c2215 = fkind * c2215

       if (ifueltyp == 1) then
          divcst = c2215
          c2215 = 0.0D0
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

    !+ad_name  acc222
    !+ad_summ  Account 222 : Magnets, including cryostat
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 222 (magnet) costs,
    !+ad_desc  including the costs of associated cryostats.
    !+ad_prob  None
    !+ad_call  acc2221
    !+ad_call  acc2222
    !+ad_call  acc2222a
    !+ad_call  acc2223
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    if (irfp == 0) then
       call acc2222
    else
       call acc2222a
    end if

    !  Account 222.3 : Cryostat

    call acc2223

    !  Total account 222

    c222 = c2221 + c2222 + c2223 

  end subroutine acc222

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2221

    !+ad_name  acc2221
    !+ad_summ  Account 222.1 : TF magnet assemblies
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 222.1 (TF magnet) costs.
    !+ad_desc  Copper magnets are costed from the TFCX data base ($/kg).
    !+ad_desc  Superconductor magnets are costed using a new method devised
    !+ad_desc  by R. Hancox under contract to Culham Laboratory, Jan/Feb 1994.
    !+ad_desc  If ifueltyp = 1, the TART centrepost cost is treated as a fuel
    !+ad_desc  cost, rather than as a capital cost.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: costtfsc,costtfcu,costwire,ctfconpm
    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6900D0
    cmlsa(2) = 0.8450D0
    cmlsa(3) = 0.9225D0
    cmlsa(4) = 1.0000D0

    if (itfsup == 0) then  !  Resistive TF coils

       !  Account 222.1.1 : Inboard TF coil legs

       c22211 = 1.0D-6 * whtcp * uccpcl1 * cmlsa(lsa)
       c22211 = fkind * c22211

       cpstcst = 0.0D0  !  TART centrepost
       if ((itart == 1).and.(ifueltyp == 1)) then
          cpstcst = c22211
          c22211 = 0.0D0
       end if

       !  Account 222.1.2 : Outboard TF coil legs

       c22212 = 1.0D-6 * whttflgs * uccpclb * cmlsa(lsa)
       c22212 = fkind * c22212

       !  Total (copper) TF coil costs

       c2221 = c22211 + c22212

    else  !  Superconducting TF coils

       !  Account 222.1.1 : Conductor

       !  Superconductor ($/m)

       costtfsc = ucsc(isumattf) * whtconsc / (tfleng*turnstf)

       !  Copper ($/m)

       costtfcu = uccu * whtconcu / (tfleng*turnstf)

       !  Total cost/metre of superconductor and copper wire

       costwire = costtfsc + costtfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       ctfconpm = costwire + cconshtf + cconfix

       !  Total conductor costs

       c22211 = 1.0D-6 * ctfconpm * tfno * tfleng * turnstf
       c22211 = fkind * c22211 * cmlsa(lsa)

       !  Account 222.1.2 : Winding

       c22212 = 1.0D-6 * ucwindtf * tfno * tfleng * turnstf
       c22212 = fkind * c22212 * cmlsa(lsa)

       !  Account 222.1.3 : Case

       c22213 = 1.0D-6 * (whtcas*uccase) * tfno
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

    !+ad_name  acc2222
    !+ad_summ  Account 222.2 : PF magnet assemblies
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 222.2 (PF magnet) costs.
    !+ad_desc  Conductor costs previously used an algorithm devised by R. Hancox,
    !+ad_desc  January 1994, under contract to Culham, which took into
    !+ad_desc  account the fact that the superconductor/copper ratio in
    !+ad_desc  the conductor is proportional to the maximum field that
    !+ad_desc  each coil will experience. Now, the input copper fractions
    !+ad_desc  are used instead.
    !+ad_desc  Maximum values for current, current density and field
    !+ad_desc  are used.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  16/10/14 PJK Replaced sccufac usage with input copper fractions
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: costpfcu,costpfsc,costpfsh,costwire,cpfconpm, &
         pfwndl
    real(kind(1.0D0)), dimension(4) :: cmlsa
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

    !  Non-OH coils

    if (iohcl == 1) then
       npf = nohc-1
    else
       npf = nohc
    end if

    c22221 = 0.0D0
    do i = 1,npf

       !  Superconductor ($/m)

       !costpfsc = ucsc(isumatpf) * (sccufac*bpf(i)) * &
       !     abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcond(isumatpf)
       costpfsc = ucsc(isumatpf) * (1.0D0-fcupfsu)*(1.0D0-vf(i)) * &
            abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcond(isumatpf)

       !  Copper ($/m)

       !costpfcu = uccu * (1.0D0-sccufac*bpf(i)) * &
       !     abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper
       costpfcu = uccu * fcupfsu*(1.0D0-vf(i)) * &
            abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper

       !  Total cost/metre of superconductor and copper wire

       costwire = costpfsc + costpfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       cpfconpm = costwire + costpfsh + cconfix

       !  Total account 222.2.1 (PF coils excluding OH coil)

       c22221 = c22221 + (1.0D-6 * twopi * rpf(i) * turns(i) * &
            cpfconpm)

    end do

    !  OH coil

    if (iohcl == 1) then

       !  Superconductor ($/m)

       !costpfsc = ucsc(isumatoh) * &
       !     (sccufac * max(abs(bmaxoh),abs(bmaxoh0)) ) * &
       !     abs(ric(nohc)/turns(nohc))*1.0D6 / &
       !     max(abs(cohbop),abs(coheof)) * dcond(isumatoh)
       costpfsc = ucsc(isumatoh) * &
            awpoh*(1.0D0-fcuohsu)*(1.0D0-vf(nohc)) * &
            abs(ric(nohc)/turns(nohc))*1.0D6 / &
            max(abs(cohbop),abs(coheof)) * dcond(isumatoh)

       !  Copper ($/m)

       !costpfcu = uccu * &
       !     (1.0D0 - sccufac*max(abs(bmaxoh),abs(bmaxoh0)) ) * &
       !     abs(ric(nohc)/turns(nohc))*1.0D6 / &
       !     max(abs(cohbop),abs(coheof)) * dcopper
       costpfcu = uccu * &
            awpoh*fcuohsu*(1.0D0-vf(nohc)) * &
            abs(ric(nohc)/turns(nohc))*1.0D6 / &
            max(abs(cohbop),abs(coheof)) * dcopper

       !  Total cost/metre of superconductor and copper wire (OH coil)

       costwire = costpfsc + costpfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       cpfconpm = costwire + costpfsh + cconfix

       !  Total account 222.2.1 (PF+OH coils)

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

  subroutine acc2222a

    !+ad_name  acc2222a
    !+ad_summ  Account 222.2 : PF magnet assemblies (Reversed Field Pinch)
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 222.2 (PF magnet) costs
    !+ad_desc  for a reversed field pinch device.
    !+ad_desc  Conductor costs use an algorithm devised by R. Hancox,
    !+ad_desc  January 1994, under contract to Culham, which takes into
    !+ad_desc  account the fact that the superconductor/copper ratio in
    !+ad_desc  the conductor is proportional to the maximum field that
    !+ad_desc  each coil will experience. OH and PF coils are treated
    !+ad_desc  similarly. Maximum values for current, current density and field
    !+ad_desc  are used.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: costpfcu,costpfsc,costpfsh,costwire,cpfconpm, &
         pfwndl,sccufac1
    real(kind(1.0D0)), dimension(4) :: cmlsa
    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6900D0
    cmlsa(2) = 0.8450D0
    cmlsa(3) = 0.9225D0
    cmlsa(4) = 1.0000D0

    !  Total length of PF coil windings (m)

    pfwndl = 0.0D0
    do i = 1,nrfppf
       pfwndl = pfwndl + twopi*rrpf(i)*nturns(i)
    end do

    !  Account 222.2.1 : Conductor

    c22221 = 0.0D0
    do i = 1,nrfppf

       !  Resistive coils (first 14 coils only)

       if (i <= 14) then
          sccufac1 = sccufac
          sccufac = 0.0D0
          costpfsh = 0.0D0
       else
          sccufac1 = sccufac
          costpfsh = cconshpf
       end if

       !  Superconductor ($/m)

       costpfsc = ucsc(isumatpf) * (sccufac*bpf(i)) * &
            drpf(i) * dzpf(i) / nturns(i) * dcond(isumatpf)

       !  Copper ($/m)

       costpfcu = uccu * (1.0D0-sccufac*bpf(i)) * &
            drpf(i) * dzpf(i) / nturns(i) * dcopper

       !  Total cost/metre of superconductor and copper wire

       costwire = costpfsc + costpfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)

       cpfconpm = costwire + costpfsh + cconfix

       !  Total account 222.2.1

       c22221 = c22221 + (1.0D-6 * twopi * rrpf(i) * nturns(i) * &
            cpfconpm)

       !  Reset sccufac to previous value if necessary

       sccufac = sccufac1

    end do

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

  end subroutine acc2222a

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc2223

    !+ad_name  acc2223
    !+ad_summ  Account 222.3 : Vacuum vessel
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 222.3 (vacuum vessel) costs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  18/06/13 PJK Changed wording from cryostat to vacuum vessel
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.6900D0
    cmlsa(2) = 0.8450D0
    cmlsa(3) = 0.9225D0
    cmlsa(4) = 1.0000D0

    c2223 = 1.0D-6 * cryomass * uccryo
    c2223 = fkind * c2223 * cmlsa(lsa)

  end subroutine acc2223

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc223

    !+ad_name  acc223
    !+ad_summ  Account 223 : Power injection
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 223 (power injection) costs.
    !+ad_desc  The costs are from TETRA, updated to 1990$.
    !+ad_desc  Nominal TIBER values are used pending system designs. Costs are
    !+ad_desc  scaled linearly with power injected into the plasma and include
    !+ad_desc  the power supplies.
    !+ad_desc  <P>If ifueltyp=1, the fraction (1-fcdfuel) of the cost of the
    !+ad_desc  current drive system is considered as capital cost, and the
    !+ad_desc  fraction (fcdfuel) is considered a recurring fuel cost due
    !+ad_desc  to the system's short life.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  22/05/14 PJK Powers now in MW instead of W
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: exprf = 1.0D0
    real(kind(1.0D0)) :: switch

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

       c2233 = 1.0D-6 * ucnbi * (1.0D6*pnbeam)**exprf
       if (ifueltyp == 1) c2233 = (1.0D0-fcdfuel) * c2233
       c2233 = fkind * c2233

       !  Account 223.4 : Oscillating Field (RFP)

       c2234 = 1.0D-6 * ucof * (1.0D6*pofcd)**exprf
       if (ifueltyp == 1) c2234 = (1.0D0-fcdfuel) * c2234
       c2234 = fkind * c2234

    else

       !  IFE driver costs (depends on driver type)
       !  Assume offset linear form for generic and SOMBRERO types,
       !  or one of two offset linear forms for OSIRIS type

       if (ifedrv /= 2) then
          c2231 = mcdriv * (cdriv0 + dcdrv0*1.0D-6*edrive)
       else
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

    !+ad_name  acc224
    !+ad_summ  Account 224 : Vacuum system
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 224 (vacuum system) costs.
    !+ad_desc  The costs are scaled from TETRA reactor code runs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad)hist  29/10/12 PJK Changed nvtype to ntype
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !+ad_name  acc225
    !+ad_summ  Account 225 : Power conditioning
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 225 (power conditioning) costs.
    !+ad_prob  None
    !+ad_call  acc2251
    !+ad_call  acc2252
    !+ad_call  acc2253
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !+ad_name  acc2251
    !+ad_summ  Account 225.1 : TF coil power conditioning
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 225.1 (TF coil power
    !+ad_desc  conditioning) costs.
    !+ad_desc  Costs are developed based on the major equipment specification
    !+ad_desc  of the tfcpwr module.  A multiplier is used to account for bulk
    !+ad_desc  materials and installation.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: expel = 0.7D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 225.1.1 : TF coil power supplies

    c22511 = 1.0D-6 * uctfps * (tfckw*1.0D3 + tfcmw*1.0D6)**expel
    c22511 = fkind * c22511

    !  Account 225.1.2 : TF coil breakers (zero cost for copper coils)

    if (itfsup == 1) then
       c22512 = 1.0D-6 * (uctfbr*tfno*(cpttf*vtfskv*1.0D3)**expel+ &
            uctfsw*cpttf)
    else
       c22512 = 0.0D0
    end if
    c22512 = fkind * c22512

    !  Account 225.1.3 : TF coil dump resistors

    c22513 = 1.0D-6 * (uctfdr*(tfno*estotf*1.0D9) + uctfgr * 0.5D0*tfno)
    c22513 = fkind * c22513

    !  Account 225.1.4 : TF coil instrumentation and control

    c22514 = 1.0D-6 * uctfic * (30.0D0*tfno)
    c22514 = fkind * c22514

    !  Account 225.1.5 : TF coil bussing

    if (itfsup == 0) then
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

    !+ad_name  acc2252
    !+ad_summ  Account 225.2 : PF coil power conditioning
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 225.2 (PF coil power
    !+ad_desc  conditioning) costs.
    !+ad_desc  Costs are taken from the equipment specification of the
    !+ad_desc  <A HREF="pfpwr.html">pfpwr</A> routine from the plant power module.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  04/02/13 PJK Comment change
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !+ad_name  acc2253
    !+ad_summ  Account 225.3 : Energy storage
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 225.3 (energy storage) costs.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  30/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: expes = 0.8D0
    real(kind(1.0D0)) :: shcss

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

    !+ad_name  acc226
    !+ad_summ  Account 226 : Heat transport system
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 226 (heat transport system) costs.
    !+ad_desc  Costs are estimated from major equipment and heat transport
    !+ad_desc  system loops developed in the heatpwr module of the code.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  23/05/13 PJK Added blktmodel comment about coolant inconsistency
    !+ad_hist  03/06/14 PJK Changed facht to fachtmw
    !+ad_hist  17/06/14 PJK Changed priheat to pthermmw in chx calculation
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: exphts = 0.7D0
    real(kind(1.0D0)), parameter :: expcry = 0.67D0
    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.4000D0
    cmlsa(2) = 0.7000D0
    cmlsa(3) = 0.8500D0
    cmlsa(4) = 1.0000D0

    !  Account 226.1 : Reactor cooling system

    !  Pumps and piping system
    !  N.B. with blktmodel > 0, the blanket is assumed to be helium-cooled,
    !  but the shield etc. is water-cooled (costr=2). Therefore, a slight
    !  inconsistency exists here...

    cpp = 1.0D-6 * uchts(costr) * ( (1.0D6*pfwdiv)**exphts + &
         (1.0D6*pnucblkt)**exphts + (1.0D6*pnucshld)**exphts)
    cpp = fkind * cpp * cmlsa(lsa)

    !  Primary heat exchangers

    chx = 1.0D-6 * ucphx * rnphx * (1.0D6*pthermmw/rnphx)**exphts
    chx = fkind * chx * cmlsa(lsa)

    c2261 = chx + cpp

    !  Account 226.2 : Auxiliary component cooling

    !  Pumps and piping system

    cppa = 1.0D-6 * ucahts * ( (1.0D6*pinjht)**exphts + &
         (1.0D6*crypmw)**exphts + (1.0D6*vachtmw)**exphts + &
         (1.0D6*trithtmw)**exphts + (1.0D6*fachtmw)**exphts )

    if (ife == 1) cppa = cppa + 1.0D-6 * ucahts * ( &
         (1.0D6*tdspmw)**exphts + (1.0D6*tfacmw)**exphts )

    !  Hydrogen production powers

    if (ihplant /= 0) cppa = cppa + 1.0D-6 * ucahts * ( &
         (1.0D6*hthermmw)**exphts + (1.0D6*helecmw)**exphts )

    cppa = fkind * cppa * cmlsa(lsa)

    !  Intermediate heat exchangers

    chxa = 1.0D-6 * ucihx * rnihx * (1.0D6*ctht/rnihx)**exphts
    chxa = fkind * chxa * cmlsa(lsa)

    c2262 = cppa + chxa

    !  Account 226.3 : Cryogenic system

    c2263 = 1.0D-6 * uccry * 4.5D0/tftmp * helpow**expcry
    c2263 = fkind * c2263 * cmlsa(lsa)

    !  Total account 226

    c226 = c2261 + c2262 + c2263

  end subroutine acc226

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc227

    !+ad_name  acc227
    !+ad_summ  Account 227 : Fuel handling
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 227 (fuel handling) costs.
    !+ad_desc  Costs are scaled from TETRA reactor code runs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  12/06/13 PJK Modified wtgpd calculation with new rndfuel definition
    !+ad_hist  11/09/13 PJK Changed logic for detritiation costs
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) cfrht,targtm

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Account 227.1 : Fuelling system

    c2271 = 1.0D-6 * ucf1
    c2271 = fkind * c2271

    !  Account 227.2 : Fuel processing and purification

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

    !  Account 227.3 : Atmospheric recovery systems

    cfrht = 1.0D5

    !  No detritiation needed if purely D-He3 reaction
    if (ftrit > 1.0D-3) then
       c2273 = 1.0D-6 * ucdtc * ( (cfrht/1.0D4)**0.6D0 * &
            (volrci + wsvol) )
    else
       c2273 = 0.0D0
    end if
    c2273 = fkind * c2273

    !  Account 227.4 : Nuclear building ventilation

    c2274 = 1.0D-6 * ucnbv * (volrci + wsvol)**0.8D0
    c2274 = fkind * c2274

    !  Total account 227

    c227 = c2271 + c2272 + c2273 + c2274

  end subroutine acc227

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc228

    !+ad_name  acc228
    !+ad_summ  Account 228 : Instrumentation and control
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 228 (instrumentation and
    !+ad_desc  control) costs.
    !+ad_desc  Costs are based on TFCX and INTOR.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    c228 = 1.0D-6 * uciac
    c228 = fkind * c228

  end subroutine acc228

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc229

    !+ad_name  acc229
    !+ad_summ  Account 229 : Maintenance equipment
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 229 (maintenance equipment) costs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    c229 = 1.0D-6 * ucme 
    c229 = fkind * c229

  end subroutine acc229

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc23

    !+ad_name  acc23
    !+ad_summ  Account 23 : Turbine plant equipment
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 23 (turbine plant equipment) costs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: exptpe = 0.83D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ireactor == 1) then
       c23 = 1.0D-6 * ucturb(costr) * (pgrossmw/1200.0D0)**exptpe
    end if

  end subroutine acc23

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc24

    !+ad_name  acc24
    !+ad_summ  Account 24 : Electric plant equipment
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 24 (electric plant equipment) costs.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), parameter :: expepe = 0.9D0
    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.5700D0
    cmlsa(2) = 0.7850D0
    cmlsa(3) = 0.8925D0
    cmlsa(4) = 1.0000D0

    !  Account 241 : Switchyard

    c241 = 1.0D-6 * ucswyd * cmlsa(lsa)

    !  Account 242 : Transformers

    c242 = 1.0D-6 * (ucpp*(pacpmw*1.0D3)**expepe + ucap*(fcsht*1.0D3))
    c242 = c242 * cmlsa(lsa)

    !  Account 243 : Low voltage
    !  (include 0.8 factor for transformer efficiency)

    c243 = 1.0D-6 * uclv * tlvpmw * 1.0D3 / 0.8D0 * cmlsa(lsa)

    !  Account 244 : Diesel generator (8 MW per generator,  assume 4 )

    c244 = 1.0D-6 * ucdgen * 4.0D0 * cmlsa(lsa)

    !  Account 245 : Auxiliary facility power needs

    c245 = 1.0D-6 * ucaf * cmlsa(lsa)

    !  Total account 24

    c24 = c241 + c242 + c243 + c244 + c245

  end subroutine acc24

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc25

    !+ad_name  acc25
    !+ad_summ  Account 25 : Miscellaneous plant equipment
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 25 (miscellaneous plant
    !+ad_desc  equipment) costs, such as waste treatment.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.7700D0
    cmlsa(2) = 0.8850D0
    cmlsa(3) = 0.9425D0
    cmlsa(4) = 1.0000D0

    c25 = 1.0D-6 * ucmisc * cmlsa(lsa)

  end subroutine acc25

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc26

    !+ad_name  acc26
    !+ad_summ  Account 26 : Heat rejection system
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 26 (heat rejection system) costs.
    !+ad_desc  Costs are scaled with the total plant heat rejection based on
    !+ad_desc  commercial systems.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  J. Delene, private communication, ORNL, June 1990
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: pwrrej
    real(kind(1.0D0)), dimension(4) :: cmlsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Cost multiplier for Level of Safety Assurance

    cmlsa(1) = 0.8000D0
    cmlsa(2) = 0.9000D0
    cmlsa(3) = 0.9500D0
    cmlsa(4) = 1.0000D0

    if (ireactor == 0) then
       pwrrej = powfmw + pinjwp + tfcmw
    else
       pwrrej = pthermmw - pgrossmw
    end if

    c26 = 1.0D-6 * uchrs * pwrrej/2300.0D0 * cmlsa(lsa)

  end subroutine acc26

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acchyd

    !+ad_name  acchyd
    !+ad_summ  Costs of a Hydrogen Production Plant
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the costs of a hydrogen production plant.
    !+ad_desc  Costs are scaled with the hydrogen production rate (MW equivalent).
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  F/T&amp;M/PJK/LOGBOOK20, pp.2-3
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: uchyd

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    select case (ihplant)

    case (0)
       uchyd = 0.0D0
    case (1)
       uchyd = uchlte
    case (2)
       uchyd = uchhten
    case (3)
       uchyd = uchhtex
    case default
       uchyd = uchth

    end select

    chplant = 1.0D-6 * uchyd * (1.0D3*hpower)

  end subroutine acchyd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acc9

    !+ad_name  acc9
    !+ad_summ  Account 9 : Indirect cost and contingency allowances
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine evaluates the Account 9 (indirect cost and
    !+ad_desc  contingency allowances) costs.
    !+ad_desc  The cost modelling is based on the commercial plant model of a
    !+ad_desc  single contractor performing all plant engineering and construction
    !+ad_desc  management, using commercially purchased equipment and materials.
    !+ad_desc  <P>The project contingency is an allowance for incomplete design
    !+ad_desc  specification and unforeseen events during the plant construction.
    !+ad_desc  <P>The factors used are estimated from commercial plant experience.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  --/--/-- PJK Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  J. Delene, private communication, ORNL, June 1990
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Indirect costs

    cindrt = cfind(lsa) * cdirt * (1.0D0 + cowner)

    !  Contingency costs

    ccont = fcontng * (cdirt + cindrt)

  end subroutine acc9

end module costs_module
