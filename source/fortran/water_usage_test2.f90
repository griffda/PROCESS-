PROGRAM water_use_module
    use, intrinsic :: iso_fortran_env

    IMPLICIT NONE

    integer, parameter :: dp = REAL64

    integer :: sys_inv ! "Sheet2" - jindex (C39)
    ! Selector for the system water inventory array --> sys_inv_array(array)

    integer :: typ_wat ! "Model" - Type (D7)
    ! Water type: Light = 3 / Heavy = 1

    integer :: src_wat ! "Model" - Location (C2)
    ! Water Source: River/Inland = 0, Sea/Coast = 1, Effluent = 2

    real(dp) :: cap_desal ! "Model" - Wd (D26)
    ! Capacity of desalintation plant km3/day
    ! A desalination plant separates salt from water so both can be used for consumption

    real(dp) :: conc_cycle ! "Model" - COC (D18)
    ! Cycles of concentration
    ! The number of times the cooling water can pass around the cooling system before the
    ! concentration of the dissolved solids reaches a certain threshold and need to be removed
    ! using a blowdown

    real(dp) :: conc_nh3 ! "Model" - NH3 (D40)
    ! Concentration of ammonia (ppm)
    ! The concentration of ammonia is included in the total dissolved solids

    real(dp) :: desal_rate ! "Model" - GOR (D27)
    ! Rate of desalination in the plant
    ! The amount of water that can be processed by the desalination plant
    ! rate = (flow rate x specific heat) / power

    real(dp) :: discnt_rate ! "Model" - i (D33)
    ! Discount Rate (%)
    ! This seems to be the interest rate of the investment into the plant (need to check)

    real(dp) :: dislv_solid ! "Model" - TDS (D38)
    ! Total Dissolved Solids (ppm)
    ! Dissolved solids is the concentration of the minerals and other solids contained in the
    ! cooling water

    real(dp) :: drift_mwh ! "2.Cooling" - (DB34)
    ! Drift water loss in MWh
    ! The amount of drift water lost per MWh

    real(dp) :: dt_ca ! "Model" - Dtca (D20)
    ! Condenser Approach temperature difference (oC)
    ! The condenser approach is the difference in temperature between the water leaving the
    ! condenser and the ambient wet bulb tempertature

    real(dp) :: dt_cr ! "Model" - Dtcr (D21)
    ! Condenser Range (oC)
    ! The condenser range is the temperature difference between the water arrive at and leaving
    ! the condenser

    real(dp) :: dt_pond ! "Model" - Dtpond (K41)
    ! Temperature increment in pond (oC)
    ! The increase in temperature of the cooling pond during operation

    real(dp) :: dt_ta ! "Model" - Dtta (D22)
    ! Cooling tower approach
    ! The difference in temperature between the water leaving the cooling tower and the wet bulb
    ! temperature

    real(dp) :: elec_level ! "Model" - lpc (D34)
    ! Levelised cost of Electricity ($/MWh)
    ! The average net present cost of electricity generation for a generating plant over its
    ! lifetime

    real(dp) :: eta ! "Model" - Ebpnr (D6)
    ! Reference net efficiency
    ! The conversion efficiency from gross thermal energy to net electricity

    real(dp) :: evap_mwh ! "2.Cooling" - (DB33)
    ! Evaporation loss in MWh
    ! The amount of water lost to evaporation in m3 per MWh

    real(dp) :: evap_pond ! "Model" - Evpond (K44)
    ! Cooling pond evaporation (m3/s)
    ! The evaporate rate of the cooling pond for 'Closed loop - Cooling Pond' options

    real(dp) :: evap_riv ! "Model" - Evriver (K34)
    ! Cooling river evaporation (tn/s)
    ! The evaporation rate of the cooling river for 'Once Through' and 'Once Through - Cooling
    ! Pond'

    real(dp) :: flow_riv ! "Model" - Friver (D13)
    ! River flow rate (m3/s)
    ! The volume of water passing through the river per second

    real(dp) :: pond_km2 ! "Model" - Spond (K43)
    ! surface area of cooling pond (km2)
    ! The surface area of the cooling pond required to match the power plant gross thermal energy

    real(dp) :: pond_mwh ! "Model" - (M43)
    ! Cooling pond area (m2/MWh)
    ! The size of the cooling pond required m2 per MWh

    real(dp) :: makeup_mwh ! "2.Cooling" - (DB36)
    ! Cooling system makeup water (m3/MWh)
    ! The water required to be fed into the cooling system to replace losses (evaporation, drift &
    ! blowdown)

    real(dp) :: riv_mix_temp ! "Model" - Tmix (K31)
    ! River + cooling water temperature (oC)
    ! The cooling water from the power plant added to the river water increases its temperature

    real(dp) :: bleed_mwh ! "Model" - Fb (K49)
    ! Water bled off (tn/s)
    ! The water drain from the cooling system to remove minerals and impurities

    real(dp) :: consump_mwh ! 
    ! Water consumed per MWh (tn/s/MWh)
    ! Water lost from the cooling tower by evaporation and drift (consumed) per MWh

    real(dp) :: pow_ratio ! "Model" - hcar (K23)
    ! Power loss ratio (arb)
    ! The percentage of power lost from the condensing cycle during desalination

    real(dp) :: vap_bar ! "Module1" - Psat(T)
    ! Vapour pressure (bar)
    ! The vapour pressure calculated from the vapour temperature 

    real(dp) :: therm_cap ! "3.Report" - Reactor thermal capacity (E10)
    ! Reactor thermal capacity (MWth)
    ! The reactor thermal capacity is based on the net electric divided by the electrical 
    ! conversion efficiency

    real(dp) :: elec_cap ! "Model" - Pen (D4)
    ! Power plant gross electrical output (MWe)
    ! The power plant's electrical output after all internal electrical needs have been met

    real(dp) :: plant_avail ! "Model" - App (D35)
    ! Power plant availability (%)
    ! The amount of time the power plant is contributing electricity to the power grid

    real(dp) :: plant_life ! "Model" - Le (D32)
    ! Power plant lifetime (years)
    ! The length of time the power plant will be in operation in years

    real(dp) :: pond_bleed ! "Model" - Bleed (K45)
    ! 
    ! The water drain from the cooling system to remove minerals and impurities

    real(dp) :: q_cr

    real(dp) :: q_cr_m

    real(dp) :: q_coeff

    real(dp) :: q_coeff_wat

    real(dp) :: q_desal

    real(dp) :: q_lat_desal

    real(dp) :: r_h

    real(dp) :: spd_wind

    real(dp) :: sup_bleed

    real(dp) :: sup_evap

    real(dp) :: sup_mkup

    real(dp) :: sup_recirc

    real(dp) :: suspnd_solid

    real(dp) :: t_c

    real(dp) :: t_c_m

    real(dp) :: t_desal

    real(dp) :: t_dry

    real(dp) :: t_pond

    real(dp) :: t_steam

    real(dp) :: t_sw

    real(dp) :: t_wb

    real(dp) :: therm

    real(dp) :: typ_cool

    real(dp) :: typ_mech

    real(dp) :: w_fr

    real(dp) :: wat_bleed

    real(dp) :: wat_com

    real(dp) :: wat_concrt

    real(dp) :: wat_constr

    real(dp) :: wat_constr_max

    real(dp) :: wat_consump

    real(dp) :: wat_dom

    real(dp) :: wat_drift

    real(dp) :: wat_drink

    real(dp) :: wat_evap

    real(dp) :: wat_evap_rate

    real(dp) :: wat_excav

    real(dp) :: wat_fr

    real(dp) :: wat_makeup

    real(dp) :: wat_mkup

    real(dp) :: wat_mkup_1

    real(dp) :: wat_mkup_2

    real(dp) :: wat_pol

    real(dp) :: wat_pot

    real(dp) :: wat_withdraw

    real(dp) :: wat_wst

    ! Double precision inputs
    DOUBLE PRECISION, PARAMETER :: false = 0.0, one = 1.0, true = 1.0, zero = 0.0

    ! Arrays
    INTEGER, DIMENSION(1:8) :: air_pol_array, imping_array, list_array, plume_array, &
    treatment_array, typ_array, visual_array
    DOUBLE PRECISION, DIMENSION(1:8) :: drift_array, therm_array, wat_dis_array, wat_evap_array
    DOUBLE PRECISION, DIMENSION(4,14) :: array
    CHARACTER(len=30), DIMENSION(1:8) :: name_array
    CHARACTER(len=30) :: a1, a2, a3, a4, a5, a6, a7, a8


    ! Inputs as described in the excel document wamp:

    ! Nuclear Power Plant
    elec_cap = 100  ! Reference electric power (MWe)
    !call ovarrf(outfile,"Gross electric output (MW)", '(elec_cap)', elec_cap, 'OP ')
    eta = 0.1  ! Reference net efficiency (%)
    t_steam = 350  ! Live steam temperature (oC)

    ! Type
    typ_wat = 3 ! Water type: Light = 3 / Heavy = 1

    ! Site/Weather Data
    src_wat = 1  ! Water Source: River/Inland = 0, Sea/Coast = 1, Effluent = 2
    t_dry = 40  ! Air temperature (Dry Bulb) (oC)
    r_h = 0.3  ! Relative Humidity (%)
    ! t_wb = 25.2  ! Air temperature (Wet Bulb) (oC)
    t_sw = 20  ! Inlet water temperature (oC) (Tsw)
    flow_riv = 50  ! River flow (if applicable) (m3/s) (friver)
    spd_wind = 2  ! Wind speed (m/s)

    ! Water Quality
    dislv_solid = 1500  ! Total Dissolved Solids (TDS) (ppm)
    suspnd_solid = 25  ! Total SuspENDed Solids (TSS) (ppm)
    conc_nh3 = 30  ! Ammonia (ppm)

    ! Economic Data
    plant_life = 50  ! Lifetime of the power plant (years)
    discnt_rate = 0.06  ! Discount Rate (%)
    elec_level = 45  ! Levelised cost of Electricity ($/MWh)
    plant_avail = 0.8  ! Power plant Availability (%)


    ! Select Cooling System
    typ_cool = 1  ! Cooling System, see name_array

    ! Approaches and ranges for heat exchanging devices
    dt_ca = 6  ! Condenser Approach (oC)
    dt_cr = 10  ! Condenser Range (oC) (Dtcr)

    ! Cooling Tower Parameters
    dt_ta = 6  ! Cooling Tower Approach (oC)
    conc_cycle = 6  ! Cycles of Concentration


    ! Supporting systems constants in 'Report tab'
    sup_recirc = 1.2  ! Recirculating cooling water supporting systems
    sup_evap = 0.02  ! Evaporation losses supporting systems
    sup_bleed = 0.01  ! Blow-down losses supporting systems
    sup_mkup = 0.03  ! Make-up water supporting systems
    wat_constr_max = 6000  ! The maximum daily water usage by construction staff


    ! Constants in 'Model tab'
    cap_desal = 0 ! Capacity of desalintation plant
    desal_rate = 10 ! 
    t_desal = 80


    typ_array = [1, 1, 1, 2, 2, 3, 2, 2]
    typ_mech = typ_array(INT(typ_cool))
    CALL p_therm__pgrossmw(elec_cap, eta, therm_cap)
    CALL heat_con_rej(elec_cap, eta, q_cr)
    CALL pres_sat(t_dry, vap_bar)
    CALL wet_bulb_temp(t_dry, vap_bar, r_h, t_wb)
    CALL pres_sat(t_dry, vap_bar)
    CALL wet_bulb_temp(t_dry, vap_bar, r_h, t_wb)
    CALL con_temp(typ_mech, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_dry, t_c)
    CALL heat_con_rej(elec_cap, eta, q_cr)
    CALL temp_con_mod(t_desal, t_c_m)
    CALL Heat_lat(t_c_m, q_lat_desal) ! 'model tab' - Latent Heat of Desal (Dhdes)
    CALL Heat_desal(cap_desal, desal_rate, q_lat_desal, q_desal)
    CALL P_loss_ratio(t_c_m, t_c, pow_ratio)
    CALL Heat_con_mod(q_cr, q_desal, pow_ratio, q_cr_m)
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    CALL Nat_draft_evap(t_dry, typ_mech, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_evap, Wat_evap_rate)
    CALL Nat_draft_drift(t_dry, typ_mech, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_drift)
    CALL Nat_draft_bleed(t_dry, typ_mech, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, Wat_bleed)
    CALL Nat_draft_makeup(t_dry, typ_mech, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, wat_mkup)
    CALL Nat_draft_consump(t_dry, typ_mech, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, Wat_consump)
    CALL excav_water(elec_cap, wat_excav)
    CALL concrete_water(elec_cap, wat_concrt)
    CALL constr_water(elec_cap, wat_constr)
    CALL domestic_water(elec_cap, wat_dom)
    CALL drink_water(wat_constr, wat_drink)
    CALL sec_makeup(elec_cap, wat_mkup_2)
    CALL prim_makeup(elec_cap, wat_mkup_1)
    CALL waste_water(elec_cap, wat_wst)
    CALL polish_water(elec_cap, wat_pol)
    CALL comp_water(elec_cap, wat_com)
    CALL fire_water(elec_cap, wat_fr)
    CALL potable_water(elec_cap, wat_pot)
    CALL river_mix_temp(q_cr_m, dt_cr, flow_riv, t_sw, riv_mix_temp)
    CALL heat_coeff_wat(riv_mix_temp, spd_wind, q_coeff_wat)
    CALL heat_coeff(riv_mix_temp, spd_wind, q_coeff)
    CALL river_evap(q_cr, q_desal, pow_ratio, riv_mix_temp, spd_wind, evap_riv)
    CALL sys_inv_ch(src_wat, typ_wat, sys_inv)
    CALL sys_inv_array(array)
    CALL pond_dtemp(dt_ca, dt_cr, dt_pond)
    CALL pond_temp(dt_pond, t_dry, t_pond)
    CALL pond_area(q_cr_m, dt_cr, t_pond, spd_wind, pond_km2)
    CALL pond_evap(pond_km2, spd_wind, t_pond, r_h, t_dry, evap_pond)
    CALL sub_pond_bleed(evap_pond, conc_cycle, pond_bleed)


    ! Reference arrays for the values to populate the report depENDing on the cooling system
    list_array = [1, 2, 3, 4, 5, 6, 7, 8]
    a1 = 'Once through'; a2 = 'Once through - Cooling pond'; a3 = 'Closed loop - Cooling pond'
    a4 = 'Wet cooling - Mechanical draft'; a5 = 'Wet cooling - Natural draft'
    a6 = 'Dry cooling (Air condenser)'; a7 = 'Hybrid - Plume abatement'
    a8 = 'Hybrid - Water conservation'
    name_array = [a1, a2, a3, a4, a5, a6, a7, a8]
    treatment_array = [0, 0, 0, 1, 1, 0, 1, 0]
    wat_evap_array = [evap_riv, evap_riv, evap_pond, Wat_evap_rate, Wat_evap_rate, zero, Wat_evap_rate, zero]
    drift_array = [zero, zero, zero, Wat_drift, Wat_drift, zero, zero, zero]
    wat_dis_array = [w_fr-evap_riv, w_fr-evap_riv, pond_bleed, Wat_bleed, Wat_bleed, zero, Wat_bleed, zero]
    visual_array = [0, 2, 2, 2, 5, 1, 2, 2]
    imping_array = [5, 5, 0, 1, 1, 0, 1, 0]
    plume_array = [0, 0, 0, 4, 4, 0, 0, 0]
    IF (dt_cr > 15) THEN
        therm = 4
    ELSE
        therm = 3
    END IF
    therm_array = [therm, zero, zero, therm, therm, zero, one, one]
    air_pol_array = [0, 0, 0, 0, 0, 3, 1, 2]

    wat_makeup = drift_array(INT(typ_cool)) + wat_evap_array(INT(typ_cool)) + wat_dis_array(INT(typ_cool))
    wat_withdraw = wat_makeup*3600*24*30/1000
    pond_mwh = pond_km2 / (elec_cap/eta) * 1000000
    CALL wat_conv(wat_evap_array(INT(typ_cool)), elec_cap, evap_mwh)
    CALL wat_conv(drift_array(INT(typ_cool)), elec_cap, Drift_MWh)
    CALL wat_conv(wat_dis_array(INT(typ_cool)), elec_cap, bleed_mwh)
    CALL wat_conv(wat_makeup, elec_cap, makeup_mwh)
    CALL wat_conv(Wat_consump, elec_cap, consump_mwh)


    ! Printing the report
    WRITE(*,*) ' '//NEW_LINE('A')//" Power Plant Specification:"
    WRITE(*,*) "Type ---> Fusion Reactor"
    WRITE(*,'(a60)',advance='no') "Reactor electric capacity (MWe) = "
    CALL output(elec_cap)
    WRITE(*,'(a60)',advance='no') "Reactor thermal capacity (MWth) = "
    CALL output(therm_cap)
    WRITE(*,'(a60)',advance='no') "Reference efficiency (%) = "
    CALL output(eta*100)
    WRITE(*,'(a60)',advance='no') "Rejected heat (MWth) = "
    CALL output(q_cr)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Site conditions:"
    WRITE(*,*) "Water source / Plant Location:"
    CALL water_source(src_wat)
    WRITE(*,'(a60)',advance='no') "Dry bulb temperature (oC) = "
    CALL output(t_dry)
    WRITE(*,'(a60)',advance='no') "Wet bulb temperature (oC) = "
    CALL output(t_wb)
    WRITE(*,'(a60)',advance='no') "Relative Humidity (%) = "
    CALL output(r_h*100)
    WRITE(*,'(a60)',advance='no') "Surface water temperature (oC) = "
    CALL output(t_sw)
    WRITE(*,'(a60)',advance='no') "Average wind velocity (m/s) = "
    CALL output(spd_wind)
    IF (src_wat == 0) then
        WRITE(*,'(a60)',advance='no') "River flow (m3/s) = "
        CALL output(flow_riv)
    END IF

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Summary of Results:"
    WRITE(*,'(a60)',advance='no') "Recirculating cooling water - main (m3/s) = "
    CALL output(w_fr)
    WRITE(*,'(a60)',advance='no') "Recirculating cooling water - supporting (m3/s) = "
    CALL output(Sup_recirc)
    WRITE(*,'(a60)',advance='no') "Evaporation losses - main (m3/s) = "
    CALL output(wat_evap_array(INT(typ_cool)))
    WRITE(*,'(a60)',advance='no') "Evaporation losses - supporting (m3/s) = "
    CALL output(Sup_evap)
    WRITE(*,'(a60)',advance='no') "Blow-down losses - main (m3/s) = "
    CALL output(wat_dis_array(INT(typ_cool)))
    WRITE(*,'(a60)',advance='no') "Blow-down losses - supporting (m3/s) = "
    CALL output(Sup_bleed)
    WRITE(*,'(a60)',advance='no') "Make-up water - main (m3/s) = "
    CALL output(wat_makeup)
    WRITE(*,'(a60)',advance='no') "Make-up water - supporting (m3/s) = "
    CALL output(Sup_mkup)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Total Water needed:"
    WRITE(*,'(a60)',advance='no') "During Construction (m3) = "
    CALL output(-1*(wat_excav+wat_concrt+wat_constr))
    WRITE(*,'(a60)',advance='no') "During commissioning (m3) = "
    CALL output(wat_dom+wat_drink)
    WRITE(*,*) "During operation:"
    WRITE(*,'(a60)',advance='no') "Condenser cooling - withdrawal (Million m3/month) = "
    CALL output(wat_withdraw)
    WRITE(*,'(a60)',advance='no') "Other support systems (m3/month) = "
    CALL output(wat_mkup_2+wat_mkup_1+wat_wst+wat_pol+wat_com+wat_fr+wat_pot)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Total water inventory needed:"
    WRITE(*,'(a60)',advance='no') "fresh water (m3) = "
    CALL output(array(sys_inv,1)+array(sys_inv,2)+array(sys_inv,4))
    WRITE(*,'(a60)',advance='no') "Demineralised water (m3) = "
    CALL output(array(sys_inv,3)+sum(array(sys_inv,5:14)))

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Design variables:"
    WRITE(*,'(a59)', advance='no') "Cooling system type ="
    WRITE(*,*) name_array(INT(typ_cool))  
    WRITE(*,'(a60)',advance='no') "Cycles of Concentration = "
    CALL output(conc_cycle)
    WRITE(*,'(a60)',advance='no') "Condenser Approach (oC) = "
    CALL output(dt_ca)
    WRITE(*,'(a60)',advance='no') "Condenser Range (oC) = "
    CALL output(dt_cr)
    WRITE(*,'(a60)',advance='no') "Cooling Tower Approach (oC) = "
    CALL output(dt_ta)
    IF (typ_cool == 2) THEN
        WRITE(*,'(a60)',advance='no') "Cooling Pond Area (km2) = "
        CALL output(pond_km2)
    END IF

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Results:"
    IF (typ_cool == 3) THEN
        WRITE(*,'(a60)',advance='no') "Pond Area Needed (km2) = "
        CALL output(pond_km2)
        WRITE(*,'(a60)',advance='no') "Pond Area Needed (m2/MWh) = "
        CALL output(pond_mwh)
    END IF
    WRITE(*,'(a60)',advance='no') "Evaporation losses (m3/s) = "
    CALL output(wat_evap_array(INT(typ_cool)))
    WRITE(*,'(a60)',advance='no') "Evaporation losses (MWh) = "
    CALL output(evap_mwh)
    WRITE(*,'(a60)',advance='no') "Drift losses (m3/s) = "
    CALL output(drift_array(INT(typ_cool)))
    WRITE(*,'(a60)',advance='no') "Drift losses (MWh) = "
    CALL output(Drift_MWh)
    WRITE(*,'(a60)',advance='no') "Blow-down losses (m3/s) = "
    CALL output(wat_dis_array(INT(typ_cool)))
    WRITE(*,'(a60)',advance='no') "Blow-down losses (MWh) = "
    CALL output(bleed_mwh)
    WRITE(*,'(a60)',advance='no') "Cooling system makeup water (m3/s) = "
    CALL output(wat_makeup)
    WRITE(*,'(a60)',advance='no') "Cooling system makeup water (MWh) = "
    CALL output(makeup_mwh)
    WRITE(*,'(a60)',advance='no') "Total consumption (m3/s) = "
    CALL output(wat_evap_array(INT(typ_cool)) + drift_array(INT(typ_cool)))
    WRITE(*,'(a60)',advance='no') "Total consumption (MWh) = "
    CALL output(evap_mwh + Drift_MWh)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Water needs during construction:"
    WRITE(*,'(a60)',advance='no') "Excavation (m3) = "
    CALL output(-1*(wat_excav))
    WRITE(*,'(a60)',advance='no') "Concrete mixing (m3) = "
    CALL output(wat_concrt)
    WRITE(*,'(a60)',advance='no') "Supply for construction staff (m3) = "
    CALL output(-1*(wat_constr))

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Water needs during commissioning:"
    WRITE(*,'(a60)',advance='no') "Flushing, cleaning etc (m3) = "
    CALL output(wat_dom)
    WRITE(*,'(a60)',advance='no') "Drinking water (m3) = "
    CALL output(-1*(wat_drink))

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    " Water needs during operation (except from condenser cooling):"
    WRITE(*,'(a60)',advance='no') "Makeup for Secondary loop (m3/month) - Demineralised = "
    CALL output(wat_mkup_2)
    WRITE(*,'(a60)',advance='no') "Makeup for primary (m3/month) - Demineralised = "
    CALL output(wat_mkup_1)
    WRITE(*,'(a60)',advance='no') "Waste Treatment (m3/month) - fresh Water = "
    CALL output(wat_wst)
    WRITE(*,'(a60)',advance='no') "Condensate Polishing Plant (m3/month) - Demineralised = "
    CALL output(wat_pol)
    WRITE(*,'(a60)',advance='no') "Component cooling water makeup (m3/month) - Demineralised = "
    CALL output(wat_com)
    WRITE(*,'(a60)',advance='no') "Fire protection (m3/month) - fresh Water = "
    CALL output(wat_fr)
    WRITE(*,'(a60)',advance='no') "Sanitary and potable (m3/month) - fresh Water = "
    CALL output(wat_pot)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    " Water system inventories during operation:"
    WRITE(*,'(a60)',advance='no') "Condenser cooling water circuit (m3) = "
    CALL output(array(sys_inv,1))
    WRITE(*,'(a60)',advance='no') "Service water circuit (IDCT) + 7 days storage (m3) = "
    CALL output(array(sys_inv,2))
    WRITE(*,'(a60)',advance='no') "Fire water circuit - pipes (m3) = "
    CALL output(array(sys_inv,3))
    WRITE(*,'(a60)',advance='no') "Fire water circuit - storage (m3) = "
    CALL output(array(sys_inv,4))
    WRITE(*,'(a60)',advance='no') "Nuclear systems & component cooling circuit (m3) = "
    CALL output(array(sys_inv,5))
    WRITE(*,'(a60)',advance='no') "TG component cooling circuit (m3) = "
    CALL output(array(sys_inv,6))
    WRITE(*,'(a60)',advance='no') "Feed water circuit (m3) = "
    CALL output(array(sys_inv,7))
    WRITE(*,'(a60)',advance='no') "Steam generator secondary side inventory (m3) = "
    CALL output(array(sys_inv,8))
    WRITE(*,'(a60)',advance='no') "Reactor auxilary circuits (m3) = "
    CALL output(array(sys_inv,9))
    WRITE(*,'(a60)',advance='no') "Spent fuel bay cooling circuit (m3) = "
    CALL output(array(sys_inv,10))
    WRITE(*,'(a60)',advance='no') "Emergency core cooling circuit (and dump inventory) (m3) = "
    CALL output(array(sys_inv,11))
    WRITE(*,'(a60)',advance='no') "Emergency feed water pools and circuit (m3) = "
    CALL output(array(sys_inv,12))
    WRITE(*,'(a60)',advance='no') "Primary heat transport system (m3) = "
    CALL output(array(sys_inv,13))
    WRITE(*,'(a60)',advance='no') "Moderator system (m3) = "
    CALL output(array(sys_inv,14))

    !WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    !"Testing"
    !PRINT *, t_pond

END PROGRAM water_use_module


!--------------------------------------TEMPERATURE CONVERSION--------------------------------------


! CTOK()
! Converts from celsius to kelvin -> TESTED
SUBROUTINE c_to_k(temp)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: temp
    temp = temp + 273.15
    RETURN
END SUBROUTINE


! CTOF()
! Converts from celsius to fahrenheit -> TESTED
SUBROUTINE c_to_f(temp)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: temp
    temp = (9.00/5.00) * temp + 32.00
    RETURN
END SUBROUTINE


! FTOC()
! Converts from fahrenheit to celsius -> TESTED
SUBROUTINE f_to_c(temp)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: temp
    temp = (5.00/9.00) * (temp - 32.00)
    RETURN
END SUBROUTINE


!-------------------------------------WATER SYSTEM INVENTORIES-------------------------------------


! Printing the water source type -> TESTED
SUBROUTINE water_source(src_wat)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: src_wat
    IF (src_wat == 0) THEN
        WRITE(*,*) "River/Inland"
    ELSE IF (src_wat == 1) THEN
        WRITE(*,*) "Sea/Coast"
    ELSE IF (src_wat == 2) THEN
        WRITE(*,*) "Effluent"
    ELSE
        WRITE(*,*) "Unknown water source"
    END IF
END SUBROUTINE water_source


! 'Report' tab - NPP system inventories choice
! Using water type and water source determines what values to retrieve from sys_inv_array() -> TESTED
SUBROUTINE sys_inv_ch(src_wat, typ_wat, sys_inv)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: typ_wat
    INTEGER, INTENT(INOUT) :: src_wat
    INTEGER, INTENT(OUT) :: sys_inv
    INTEGER src_wat_2
    IF (src_wat == 2) THEN
        src_wat_2 = 0
    ELSE
        src_wat_2 = src_wat
    END IF
    sys_inv = typ_wat + src_wat_2
END SUBROUTINE sys_inv_ch


! 'sheet 2' - NPP system inventories
! A reference SUBROUTINE for the water inventories
SUBROUTINE sys_inv_array(array)
    IMPLICIT NONE
    DOUBLE PRECISION, dimension(4,14), INTENT(OUT) :: array
    array =    reshape ( [6500.0,   0.0,    6500.0, 0.0, &
                          18.6,      0.0,    18.6,   0.0, &
                          415.0,     415.0,  300.0,  300.0, &
                          0.0,       0.0,    1000.0, 3000.0, &
                          1200.0,    1200.0, 500.0,  500.0, &
                          400.0,     400.0,  450.0,  450.0, &
                          600.0,     600.0,  1800.0, 1800.0, &
                          0.0,       0.0,    300.0,  300.0, &
                          780.0,     780.0,  500.0,  500.0, &
                          5100.0,    5100.0, 1700.0, 1700.0, &
                          1200.0,    1200.0, 1900.0, 1900.0, &
                          0.0,       0.0,    1700.0, 1700.0, &
                          400.0,     400.0,  400.0,  400.0, &
                          310.0,     310.0,  0.0,    0.0], &
                         [4,14] )
END SUBROUTINE sys_inv_array


!--------------------------------------------------------------------------------------------------


! Converting from m3/s to m3/MWh -> TESTED
SUBROUTINE wat_conv(wat_m3, elec_cap, wat_mwh)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: wat_m3, elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_mwh
    DOUBLE PRECISION Hr
    Hr = 3600
    wat_mwh = (wat_m3 * Hr) / elec_cap
END SUBROUTINE Wat_conv


!--------------------------------------------------------------------------------------------------


! 'report tab' - Reactor thermal capacity
! Calculates the thermal capacity of the reactor from the electric capacity -> TESTED
SUBROUTINE p_therm__pgrossmw(elec_cap, eta, therm_cap)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap, eta
    DOUBLE PRECISION, INTENT(OUT) ::  therm_cap
    therm_cap = elec_cap / eta
END SUBROUTINE p_therm__pgrossmw


!--------------------------------------------------------------------------------------------------


! 'model tab' - Mod Condenser temperature (Tcm)
! The condenser temperature 10oC greater than the maximum brine temperature -> TESTED
SUBROUTINE temp_con_mod(t_desal, t_c_m)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_desal
    DOUBLE PRECISION, INTENT(OUT) :: t_c_m
    t_c_m = t_desal + 10
END SUBROUTINE temp_con_mod


!--------------------------------------------------------------------------------------------------


! Psat()
! Calculating the partial vapour pressure from the dry/wet bulb temperature -> TESTED
SUBROUTINE pres_sat(t_dry, vap_bar)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_dry
    DOUBLE PRECISION, INTENT(OUT) :: vap_bar
    DOUBLE PRECISION a, b, c
    a = 8.07131
    b = 1730.63
    c = 233.426
    vap_bar = (10 ** (a - (b / (c + t_dry)))) / 760
END SUBROUTINE pres_sat


!--------------------------------------------------------------------------------------------------


! Wetbulb() & 'model tab' - Wet Bulb temperature (Twb)
! Calculating the wet bulb temperature from the dry bulb temperature and partial pressure -> TESTED
SUBROUTINE wet_bulb_temp(t_dry, vap_bar, r_h, t_wb)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_dry, r_h
    DOUBLE PRECISION, INTENT(INOUT) :: vap_bar
    DOUBLE PRECISION, INTENT(OUT) :: t_wb
    DOUBLE PRECISION p_sat_r_h, e_diff, t_wb_gs, pres_mb, sign_prev, incr, e_diff_2, temp_C, ew_gs, e_gs, sign_cur
    INTEGER i
    p_sat_r_h = vap_bar * r_h
    e_diff = 1
    t_wb_gs = 0
    pres_mb = 1013.25 ! 1 atmospheric pressure in mbar
    sign_prev = 1
    incr = 10
    e_diff_2 = p_sat_r_h * 1000
    temp_C = t_dry
    i = 0
    DO WHILE (abs(e_diff) > 0.05)
        i = i + 1
        ew_gs = 6.112 * exp((17.67 * t_wb_gs) / (t_wb_gs + 243.5))
        e_gs = ew_gs - pres_mb * (temp_C - t_wb_gs) * 0.00066 * (1 + (0.00115 * t_wb_gs))
        e_diff = e_diff_2 - e_gs
        If (e_diff == 0) THEN
            EXIT
        ELSE
            IF (e_diff < 0) THEN
                sign_cur = -1
                IF (sign_cur /= sign_prev) THEN
                    sign_prev = sign_cur
                    incr = incr / 10
                ELSE
                    incr = incr
                END IF
            ELSE
                sign_cur = 1
                IF (sign_cur /= sign_prev) THEN
                    sign_prev = sign_cur
                    incr = incr / 10
                ELSE
                    incr = incr
                END IF
            END IF
        END IF
        t_wb_gs = t_wb_gs + incr * sign_prev
    END DO
    t_wb = t_wb_gs
END SUBROUTINE wet_bulb_temp


!--------------------------------------------------------------------------------------------------


! 'model tab' - Condensing temperature (Tc)
! Calculating the condensing temperature based on the cooling mechanism -> TESTED
SUBROUTINE con_temp(typ_cool, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_dry, t_c)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: typ_cool, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_dry
    DOUBLE PRECISION, INTENT(OUT) :: t_c
    DOUBLE PRECISION dt_c
    dt_c = dt_cr + dt_ca
    IF (typ_cool == 1) THEN
        t_c = t_sw + dt_c
    ELSE IF (typ_cool == 2) THEN
        t_c = t_wb + dt_ta + dt_c
    ELSE IF (typ_cool == 3) THEN
        t_c = t_dry + dt_ta + dt_c
    ELSE
        PRINT *, "Incorrect cooling type"
    END IF
    RETURN
END SUBROUTINE con_temp


!--------------------------------------------------------------------------------------------------


! Fr()
! Unknown function - FROM SPX COOLING -> TESTED
! Could be calculating the flow rate of water passing between cooling tower and condenser
SUBROUTINE fr(t_l_cd, t_l_ct, t_wb, fr2)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_l_cd, t_l_ct, t_wb
    DOUBLE PRECISION, INTENT(OUT) :: fr2
    DOUBLE PRECISION dt_cd_ct, dt_cta, fr1
    dt_cd_ct = t_l_cd - t_l_ct
    dt_cta = t_l_ct - t_wb
    fr1 = 0.60589436 - 0.00562416 * t_wb - 0.00002268 * t_wb * t_wb + 0.04609405 * dt_cd_ct - &
    0.00010031 * t_wb * dt_cd_ct - 0.00074946 * dt_cd_ct * dt_cd_ct - 0.06167657 * dt_cta + &
    0.00008363 * t_wb * dt_cta + 0.0010762 * dt_cta ** 2
    fr2 = 10 ** fr1
    RETURN
END SUBROUTINE fr


!--------------------------------------------------------------------------------------------------


! LG()
! Unknown function -> TESTED
! Prerequest: fr(t_l_cd, t_l_ct, t_wb, fr2)
SUBROUTINE lg(t_l_cd, t_l_ct, t_wb, lg1)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_l_cd, t_l_ct, t_wb
    DOUBLE PRECISION, INTENT(OUT) :: lg1
    DOUBLE PRECISION fr2
    CALL fr(t_l_cd, t_l_ct, t_wb, fr2)
    lg1 = 1.3 / (fr2 ** 1.15)
    RETURN
END SUBROUTINE lg


!--------------------------------------------------------------------------------------------------


! GetSatVapPres()
! Saturated vapour pressure as function of the dry bulb temperature -> TESTED
! Prerequest: c_to_k(temp)
SUBROUTINE dry_bulb__sat_vap_press(t_dry, s_vp)
    DOUBLE PRECISION, INTENT(IN) :: t_dry
    DOUBLE PRECISION, INTENT(OUT) :: s_vp
    DOUBLE PRECISION t_dry_k, t_func
    IF (t_dry >= -100 .AND. t_dry <= 200) THEN
        t_dry_k = t_dry
        CALL c_to_k(t_dry_k)
        IF (t_dry >= -100 .AND. t_dry <= 0) THEN
            t_func = (-5.6745359 * ((10 ** 3) / t_dry_k) + 6.3925247 - 9.677843 * (10.00 ** -3) * &
            t_dry_k + 6.2215701 * (10.00 ** -7) * (t_dry_k ** 2))
            t_func = t_func + 2.0747825 * (10.00 ** -9) * (t_dry_k ** 3) - 9.484024 * &
            (10.00 ** -13) * (t_dry_k ** 4) + 4.1635019 * log(t_dry_k)
        ELSE IF (t_dry > 0 .AND. t_dry <= 200) THEN
            t_func = (-5800.2206 / t_dry_k) + 1.3914993 - 0.048640239 * t_dry_k + 0.000041764768 * &
            (t_dry_k ** 2) - 0.000000014452093 * (t_dry_k ** 3) + 6.5459673 * log(t_dry_k)
        END IF
        s_vp = exp(t_func)
    ELSE
        PRINT *, 'Dry bulb temperature is outside range [-100, 200]'
        s_vp = 0.0
    END IF
END SUBROUTINE dry_bulb__sat_vap_press


!--------------------------------------------------------------------------------------------------


! GetSatHumRatio()
! The humidity ratio of saturated air for a given dry bulb temperature and pressure -> TESTED
! Prerequest: dry_bulb__sat_vap_press(t_dry, s_vp)
SUBROUTINE dry_bulb__sat_rh_ratio(t_dry, pressure, s_rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry
    DOUBLE PRECISION, INTENT(IN) :: pressure
    DOUBLE PRECISION, INTENT(OUT) :: s_rh_ratio
    DOUBLE PRECISION s_vp
    CALL dry_bulb__sat_vap_press(t_dry, s_vp)
    s_rh_ratio = 0.621945 * s_vp / (pressure - s_vp)
END SUBROUTINE dry_bulb__sat_rh_ratio


!--------------------------------------------------------------------------------------------------


! GetHumRatiofromTWetBulb()
! Calculate the humidity ratio from the wet and dry bulb temperature -> TESTED
! Prerequest: dry_bulb__sat_rh_ratio(t_dry, pressure, s_rh_ratio)
SUBROUTINE wet_bulb__rh_ratio(t_dry, t_wb, pressure, rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_wb, pressure
    DOUBLE PRECISION, INTENT(IN) :: t_dry
    DOUBLE PRECISION, INTENT(OUT) :: rh_ratio
    DOUBLE PRECISION s_rh_ratio
    IF (t_wb <= t_dry) THEN
        CALL dry_bulb__sat_rh_ratio(t_wb, pressure, s_rh_ratio)
        rh_ratio = ((2501 - (2.326 * t_wb)) * s_rh_ratio - 1.006 * (t_dry - t_wb)) / &
        (2501 + 1.86 * t_dry - 4.186 * t_wb)
    ELSE
        PRINT *, 'The wet bulb temperature is above the dry bulb temperature'
    END IF
END SUBROUTINE wet_bulb__rh_ratio


!--------------------------------------------------------------------------------------------------


! GetVapPresfromRelHum()
! Partial pressure of water vapour as a function of relative humidity and temperature in C -> TESTED
! Prerequest: dry_bulb__sat_vap_press(t_dry, s_vp)
SUBROUTINE rh__vap_pressure(t_dry, r_h, vap_press)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_dry, r_h
    DOUBLE PRECISION, INTENT(OUT) :: vap_press 
    DOUBLE PRECISION s_vp
    IF (r_h > 0 .AND. r_h <= 1) THEN
        CALL dry_bulb__sat_vap_press(t_dry, s_vp)
        vap_press = s_vp * r_h
    ELSE
        PRINT *, 'Relative humidity is outside range [0,1]'
    END IF
END SUBROUTINE rh__vap_pressure


!--------------------------------------------------------------------------------------------------


! GetHumRatiofromVapPres()
! Humidity ratio for given water vapour pressure and atmospheric pressure -> TESTED
SUBROUTINE vap_pressure__rh_ratio(vap_press , pressure, rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: vap_press , pressure
    DOUBLE PRECISION, INTENT(OUT) :: rh_ratio
    IF (vap_press < 0) THEN
        PRINT *, 'Partial pressure of water vapour in moist air is negative'
    ELSE
        rh_ratio = 0.621945 * vap_press / (pressure - vap_press )
    END IF
END SUBROUTINE vap_pressure__rh_ratio


!--------------------------------------------------------------------------------------------------


! GetHumRatiofromRelHum()
! Calculating the relative humidity ratio for a given humidity -> TESTED
! Prerequest: rh__vap_pressure(t_dry, r_h, vap_press ), 
!             vap_pressure__rh_ratio(vap_press , pressure, rh_ratio)
SUBROUTINE rh__rh_ratio(t_dry, r_h, pressure, rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, r_h, pressure
    DOUBLE PRECISION, INTENT(OUT) :: rh_ratio
    DOUBLE PRECISION vap_press 
    IF (r_h > 0 .AND. r_h <= 1) THEN
        CALL rh__vap_pressure(t_dry, r_h, vap_press )
        CALL vap_pressure__rh_ratio(vap_press , pressure, rh_ratio)
        IF (rh_ratio < 0) THEN
            PRINT *, 'Humidity ratio is negative'
        END IF
    ELSE
        PRINT *, 'Relative humidity is outside range [0,1]'
    END IF
END SUBROUTINE rh__rh_ratio


!--------------------------------------------------------------------------------------------------


! GetVapPresfromHumRatio()
! Calculating the vapour pressure from the humidity ratio and pressure -> TESTED
SUBROUTINE rh_ratio__vap_pressure(rh_ratio, pressure, vap_press)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: rh_ratio, pressure
    DOUBLE PRECISION, INTENT(OUT) :: vap_press
    IF (rh_ratio > 0) THEN
        vap_press = pressure * (rh_ratio / (0.62198 + rh_ratio))
    ELSE
        PRINT *, 'Humidity ratio is negative'
    END IF
END SUBROUTINE rh_ratio__vap_pressure


!--------------------------------------------------------------------------------------------------


! GetRelHumfromVapPres()
! The DOUBLE PRECISIONtive humidity calculated from the dry bulb temperature and vapour pressure -> TESTED
! Prerequest: dry_bulb__sat_vap_press(t_dry, s_vp)
SUBROUTINE vap_pressure__rh(t_dry, vap_press, r_h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_dry, vap_press
    DOUBLE PRECISION, INTENT(OUT) :: r_h
    DOUBLE PRECISION s_vp
    IF (vap_press >= 0) THEN
        CALL dry_bulb__sat_vap_press(t_dry, s_vp)
        r_h = vap_press / s_vp
    ELSE
        PRINT *, 'Partial pressure of water vapour in moist air is negative'
    END IF
END SUBROUTINE vap_pressure__rh


!--------------------------------------------------------------------------------------------------


! GetRelHumfromHumRatio()
! Calculating the relative humidity from the humidity ratio -> TESTED
! Prerequest: rh_ratio__vap_pressure()
!             vap_pressure__rh()
SUBROUTINE rh_ratio__rh(t_dry, rh_ratio, pressure, r_h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_dry, rh_ratio, pressure
    DOUBLE PRECISION, INTENT(OUT) :: r_h
    DOUBLE PRECISION vap_press 
    IF (rh_ratio > 0) THEN
        CALL rh_ratio__vap_pressure(rh_ratio, pressure, vap_press)
        CALL vap_pressure__rh(t_dry, vap_press, r_h)
    END IF
END SUBROUTINE rh_ratio__rh


!--------------------------------------------------------------------------------------------------


! GetRelHumfromTWetBulb()
! Calculating the relative humidity from the dry and wet bulb temperature -> 
! Prerequest: wet_bulb__rh_ratio()
!             rh__rh_ratio()
SUBROUTINE dry_wet_bulb__rh(t_dry, t_wb, pressure, r_h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, t_wb, pressure, r_h
    DOUBLE PRECISION rh_ratio
    IF (t_wb <= t_dry) THEN
        CALL wet_bulb__rh_ratio(t_dry, t_wb, pressure, rh_ratio)
        CALL rh_ratio__rh(t_dry, rh_ratio, pressure, r_h)
    ELSE
        PRINT *, 'Wet bulb temperature is above dry bulb temperature'
    END IF
END SUBROUTINE dry_wet_bulb__rh


!--------------------------------------------------------------------------------------------------


! GetTDryBulbfromRelHum()
! Calculating the dry bulb temperature from the humidity
! Prerequest: c_to_f()
!             f_to_c()
!             dry_wet_bulb__rh()
SUBROUTINE rh_wet_bulb__dry_bulb(t_wb, r_h, t_dry)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_wb, t_dry
    DOUBLE PRECISION, INTENT(IN) :: r_h
    DOUBLE PRECISION RH_test, t_dry_prev, RH_prev, dRH_dT, DT, pressure
    INTEGER i
    CALL c_to_f(t_wb)
    t_dry = t_wb
    RH_test = 1
    t_dry_prev = t_dry
    RH_prev = RH_test
    pressure = 101325
    IF (r_h /= 1) THEN
        t_dry = t_wb + (0.12 * (t_wb**1.5) * ((1 - r_h)**2.5))
        do i = 0, 10
            CALL f_to_c(t_dry)
            CALL f_to_c(t_wb)
            CALL dry_wet_bulb__rh(t_dry, t_wb, pressure, RH_test)
            CALL c_to_f(t_dry)
            CALL c_to_f(t_wb)
            dRH_dT = (RH_test - RH_prev) / (t_dry - t_dry_prev)
            DT = (RH_test - r_h) / dRH_dT
            t_dry_prev = t_dry
            RH_prev = RH_test
            t_dry = t_dry - DT
            IF (abs(DT) < 0.0005) THEN
                EXIT
            END IF
        END DO
    END IF
    CALL f_to_c(t_wb)
    CALL f_to_c(t_dry)
END SUBROUTINE rh_wet_bulb__dry_bulb


!--------------------------------------------------------------------------------------------------


! NOTE!! THIS IS THE SOURCE OF THE ERROR MESSAGES - INVESTIGATE
! GetMoistAirEnthalpy()
! Calculating the enthalpy of moist air from the dry bulb temperature -> TESTED
SUBROUTINE dry_bulb__enth_wb(t_dry, rh_ratio, H_wb)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_dry, rh_ratio
    DOUBLE PRECISION, INTENT(OUT) :: H_wb
    DOUBLE PRECISION KILO
    KILO = 1000
    IF (rh_ratio > 0) THEN
        H_wb = ((1.006 * t_dry) + (rh_ratio * (2501 + (1.86 * t_dry)))) * KILO
    ELSE
        !PRINT *, 'Humidity ratio is negative'
    END IF
END SUBROUTINE dry_bulb__enth_wb


!--------------------------------------------------------------------------------------------------


! GetTWetBulbfromMoistAirEnthalpy()
! Calculating the wet bulb temperature from the moist air enthalpy -> TESTED
! Prerequest: f_to_c()
!             wet_bulb__rh_ratio()
!             dry_bulb__enth_wb()
SUBROUTINE enth_wb__t_wb(H_wb, t_wb)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: H_wb
    DOUBLE PRECISION, INTENT(OUT) :: t_wb
    DOUBLE PRECISION t_wb_2, pressure, t_dry, rh_ratio, H_test, t_wb_prev, H_prev, DHdT, DT
    INTEGER n, i
    H_wb = H_wb / 2324.4 ! Converting from j/kg to btu/lb
    t_wb = 32.0
    t_wb_2 = t_wb
    pressure = 101325.0
    CALL f_to_c(t_wb_2)
    t_dry = t_wb ! Setting the dry bulb temperature equivalent to the wet bulb temperature
    CALL wet_bulb__rh_ratio(t_dry, t_wb_2, pressure, rh_ratio)
    CALL dry_bulb__enth_wb(t_dry, rh_ratio, H_test)
    H_test = H_test / 2324.4 ! Converting from j/kg to btu/lb
    t_wb_prev = t_wb
    H_prev = H_test
    IF (H_wb > 12) THEN
        t_wb = (38 * log(H_wb)) - 63.5
    ELSE
        t_wb = (2.8 * H_wb) - 0.8
    END IF
    n = 0
    do i = 0, 10
        t_dry = t_wb ! Setting the dry bulb temperature to the wet bulb temperature
        CALL wet_bulb__rh_ratio(t_dry, t_wb, pressure, rh_ratio)
        CALL dry_bulb__enth_wb(t_dry, rh_ratio, H_test)
        H_test = H_test / 2324.4 ! Converting from j/kg to btu/lb
        DHdT = (H_test - H_prev) / (t_wb - t_wb_prev)
        DT = (H_test - H_wb) / DHdT
        IF (((t_wb <= 32.0) .AND. ((t_wb - DT) >= 32.0)) .or. &
        ((t_wb >= 32.0) .AND. ((t_wb - DT) <= 32.0))) THEN
            n = n + 1
            IF (n > 2) THEN
                t_wb = 32.0
                RETURN
            END IF
        END IF
        t_wb_prev = t_wb
        H_prev = H_test
        t_wb = t_wb - DT
        IF (abs(DT) < 0.0005) EXIT
    END DO
END SUBROUTINE enth_wb__t_wb


!--------------------------------------------------------------------------------------------------


! Hsatv()
! Calculating the specific enthalpy of saturated vapour: kj/kg (oC) -> TESTED
SUBROUTINE Enth_sv(t_vap, H_sv)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_vap
    DOUBLE PRECISION, INTENT(OUT) :: H_sv
    H_sv = 2500.918 + (1.834961 * t_vap) - (0.0003841512 * (t_vap**2)) - (0.000001108345 * &
    (t_vap**3)) - (0.00000004896947 * (t_vap**4)) + (1.292303E-10 * (t_vap**5)) - (2.593501E-13 * &
    (t_vap**6))
END SUBROUTINE Enth_sv


!--------------------------------------------------------------------------------------------------


! Hsatl()
! Calculating the specific enthalpy of saturated liquid: kj/kg (oC) -> TESTED
SUBROUTINE Enth_sl(temp, H_sl)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: temp
    DOUBLE PRECISION, INTENT(OUT) :: H_sl
    H_sl = 0.007529613 + (4.212269 * temp) - (0.001194192 * (temp**2)) + (0.00002101532 * &
    (temp**3)) - (0.0000001935015 * (temp**4)) + (0.000000001042925 * (temp**5)) - (2.128003 * &
    (10.0**-12) * (temp**6))
END SUBROUTINE Enth_sl


!--------------------------------------------------------------------------------------------------


! LatentHeat()
! The latent heat from the cooling process -> TESTED
! Prerequests: Enth_sv()
!              Enth_sl()
SUBROUTINE Heat_lat(temp, q_lat)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: temp
    DOUBLE PRECISION, INTENT(OUT) :: q_lat
    DOUBLE PRECISION H_sv, H_sl
    CALL Enth_sv(temp, H_sv)
    CALL Enth_sl(temp, H_sl)
    q_lat = H_sv - H_sl
END SUBROUTINE Heat_lat


!-------------------------------------------DESALINATION-------------------------------------------


! 'model tab' - Heat needed for desal (Qd)
! The heat required for the desalination plant -> TESTED
SUBROUTINE Heat_desal(cap_desal, desal_rate, q_lat_desal, q_desal)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: cap_desal, desal_rate, q_lat_desal
    DOUBLE PRECISION, INTENT(OUT) :: q_desal
    q_desal = cap_desal * (1000 / (desal_rate * 24 * 3600)) * q_lat_desal
END SUBROUTINE Heat_desal


!---------------------------------------------CONDENSER--------------------------------------------


! 'model tab' - Condenser Reject Heat (Qcr)
! The heat rejected from the condenser -> TESTED
SUBROUTINE heat_con_rej(elec_cap, eta, q_cr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap, eta
    DOUBLE PRECISION, INTENT(OUT) :: q_cr
    q_cr = (elec_cap / eta) - elec_cap
END SUBROUTINE heat_con_rej


! 'model tab' - Power loss ratio (hcar)
! The power loss ratio used for calculating the heat rejected from the condenser -> TESTED
SUBROUTINE P_loss_ratio(t_c_m, t_c, pow_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_c_m, t_c
    DOUBLE PRECISION, INTENT(OUT) :: pow_ratio
    pow_ratio = ((t_c_m - t_c) / (t_c_m + 273)) * 0.85 * 0.988 * 0.97
END SUBROUTINE P_loss_ratio


! 'model tab' - Mod Condenser Reject (Qmcr)
! The condenser reject heat minus the heat used for desalination -> TESTED
SUBROUTINE Heat_con_mod(q_cr, q_desal, pow_ratio, q_cr_m)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr, q_desal, pow_ratio
    DOUBLE PRECISION, INTENT(OUT) :: q_cr_m
    q_cr_m = q_cr - q_desal * (1 - pow_ratio)
END SUBROUTINE Heat_con_mod


! 'model tab' - Mod Condenser Water Flow rate (Fmcc)
! The water flowing through the condenser minus heat used for desalination -> TESTED
SUBROUTINE wat_con_mod(q_cr_m, dt_cr, w_fr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr_m, dt_cr
    DOUBLE PRECISION, INTENT(OUT) :: w_fr
    w_fr = q_cr_m / (dt_cr * 4.187)
END SUBROUTINE wat_con_mod


!-------------------------------------------COOLING TOWER------------------------------------------


! Evap()
! Water loss through evaporation from cooling tower (CT) in tn/s -> TESTED
! Prerequests: c_to_f()
!              lg()
!              f_to_c()
!              rh_wet_bulb__dry_bulb()
!              wet_bulb__rh_ratio()
!              dry_bulb__enth_wb()
!              enth_wb__t_wb()
SUBROUTINE Ct_water_evap(t_l_cd, t_l_ct, t_wb, r_h, Wat_evap)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_l_cd, t_l_ct, t_wb, r_h
    DOUBLE PRECISION, INTENT(OUT) :: Wat_evap
    DOUBLE PRECISION dt_cd_ct, lg1, t_dry, pressure, rh_ratio, H_wb
    DOUBLE PRECISION RH_r_out, RH_r_prev, H_out, t_wb_out
    INTEGER i
    CALL c_to_f(t_l_cd)
    CALL c_to_f(t_l_ct)
    CALL c_to_f(t_wb)
    dt_cd_ct = t_l_cd - t_l_ct
    CALL lg(t_l_cd, t_l_ct, t_wb, lg1)
    CALL f_to_c(t_wb)
    CALL rh_wet_bulb__dry_bulb(t_wb, r_h, t_dry)
    pressure = 101325
    CALL wet_bulb__rh_ratio(t_dry, t_wb, pressure, rh_ratio)
    CALL dry_bulb__enth_wb(t_dry, rh_ratio, H_wb)
    H_wb = H_wb / 2324.4 ! Converting from j/kg to btu/lb
    RH_r_out = rh_ratio + (0.0008 * dt_cd_ct * lg1)
    do i = 0, 5
        RH_r_prev = RH_r_out
        H_out = H_wb + (dt_cd_ct * lg1) + ((RH_r_out - rh_ratio) * (t_l_ct - 32))
        H_out = H_out * 2324.4
        CALL enth_wb__t_wb(H_out, t_wb_out)
        CALL wet_bulb__rh_ratio(t_wb_out, t_wb_out, pressure, RH_r_out)
        IF (abs(RH_r_out - RH_r_prev) <= 0.00001) EXIT
    END DO
    Wat_evap = (RH_r_out - rh_ratio) / lg1
END SUBROUTINE Ct_water_evap


! 'model tab' - Wet cooling tower evaporation rate (Fe)
! The water evaporated by the cooling tower to the atmosphere by natural draft -> TESTED
! Prerequests: pres_sat()
!              wet_bulb_temp()
!              con_temp()
!              heat_con_rej()
!              temp_con_mod()
!              Heat_lat()
!              Heat_desal()
!              P_loss_ratio()
!              Heat_con_mod()
!              wat_con_mod()
!              Ct_water_evap()
SUBROUTINE Nat_draft_evap(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_evap, Wat_evap_rate)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta
    DOUBLE PRECISION, INTENT(INOUT) :: t_desal, cap_desal, desal_rate, Wat_evap
    DOUBLE PRECISION, INTENT(OUT) :: Wat_evap_rate
    DOUBLE PRECISION p_sat, t_wb, t_c, q_cr, t_c_m, q_lat_desal, q_desal, q_cr_m, w_fr
    DOUBLE PRECISION t_l_cd, t_l_ct, pow_ratio
    CALL pres_sat(t_dry, p_sat)
    CALL wet_bulb_temp(t_dry, p_sat, r_h, t_wb)
    CALL con_temp(typ_cool, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_dry, t_c)
    CALL heat_con_rej(elec_cap, eta, q_cr)
    CALL temp_con_mod(t_desal, t_c_m)
    CALL Heat_lat(t_c_m, q_lat_desal) ! 'model tab' - Latent Heat of Desal (Dhdes)
    CALL Heat_desal(cap_desal, desal_rate, q_lat_desal, q_desal)
    CALL P_loss_ratio(t_c_m, t_c, pow_ratio)
    CALL Heat_con_mod(q_cr, q_desal, pow_ratio, q_cr_m)
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    t_l_cd = t_c - dt_ca
    t_l_ct = t_wb + dt_ta
    CALL Ct_water_evap(t_l_cd, t_l_ct, t_wb, r_h, Wat_evap)
    Wat_evap_rate = Wat_evap * w_fr
END SUBROUTINE Nat_draft_evap


! 'model tab' - Wet cooling tower drift loss (Fd)
! Water that leaves the cooling tower as droplets and not as evaporation -> TESTED
! Prerequests: pres_sat()
!              wet_bulb_temp()
!              con_temp()
!              heat_con_rej()
!              temp_con_mod()
!              Heat_lat()
!              Heat_desal()
!              P_loss_ratio()
!              Heat_con_mod()
!              wat_con_mod()
SUBROUTINE Nat_draft_drift(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_drift)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta
    DOUBLE PRECISION, INTENT(INOUT) :: t_desal, cap_desal, desal_rate
    DOUBLE PRECISION, INTENT(OUT) :: Wat_drift
    DOUBLE PRECISION p_sat, t_wb, t_c, q_cr, t_c_m, q_lat_desal, q_desal, q_cr_m, w_fr, pow_ratio
    CALL pres_sat(t_dry, p_sat)
    CALL wet_bulb_temp(t_dry, p_sat, r_h, t_wb)
    CALL con_temp(typ_cool, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_dry, t_c)
    CALL heat_con_rej(elec_cap, eta, q_cr)
    CALL temp_con_mod(t_desal, t_c_m)
    CALL Heat_lat(t_c_m, q_lat_desal)
    CALL Heat_desal(cap_desal, desal_rate, q_lat_desal, q_desal)
    CALL P_loss_ratio(t_c_m, t_c, pow_ratio)
    CALL Heat_con_mod(q_cr, q_desal, pow_ratio, q_cr_m)
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    Wat_drift = 0.0002 * w_fr
END SUBROUTINE Nat_draft_drift


! 'model tab' - Wet cooling tower bleed-off/blowdown (Fb)
! Water deliberately expelled from the water cycle to remove impurities -> TESTED
! Prerequests: Nat_draft_evap()
!              Nat_draft_drift()
SUBROUTINE Nat_draft_bleed(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, Wat_bleed)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, &
    t_desal, cap_desal, desal_rate, conc_cycle
    DOUBLE PRECISION, INTENT(OUT) :: Wat_bleed
    DOUBLE PRECISION Wat_evap, Wat_evap_rate, Wat_drift
    CALL Nat_draft_evap(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_evap, Wat_evap_rate)
    CALL Nat_draft_drift(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_drift)
    Wat_bleed = (Wat_evap_rate - ((conc_cycle - 1) * Wat_drift)) / (conc_cycle - 1)
END SUBROUTINE Nat_draft_bleed


! 'model tab' - Wet cooling tower make up (Fmu)
! Water required to replinish the water lost -> TESTED
! Prerequests: Nat_draft_evap()
!              Nat_draft_drift()
!              Nat_draft_bleed()
SUBROUTINE Nat_draft_makeup(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, wat_mkup)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, &
    t_desal, cap_desal, desal_rate, conc_cycle
    DOUBLE PRECISION, INTENT(OUT) :: wat_mkup
    DOUBLE PRECISION Wat_evap, Wat_evap_rate, Wat_drift, Wat_bleed
    CALL Nat_draft_evap(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_evap, Wat_evap_rate)
    CALL Nat_draft_drift(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, Wat_drift)
    CALL Nat_draft_bleed(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, Wat_bleed)
    wat_mkup = Wat_evap_rate + Wat_drift + Wat_bleed
END SUBROUTINE Nat_draft_makeup


! 'model tab' - Wet cooling tower water consumption
! The water consumed by the cooling tower (excluding blowdown)-> 
SUBROUTINE Nat_draft_consump(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, Wat_consump)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, &
    t_desal, cap_desal, desal_rate, conc_cycle
    DOUBLE PRECISION, INTENT(OUT) :: Wat_consump
    DOUBLE PRECISION Wat_bleed, wat_mkup
    CALL Nat_draft_bleed(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, Wat_bleed)
    CALL Nat_draft_makeup(t_dry, typ_cool, t_sw, dt_ta, dt_cr, dt_ca, r_h, elec_cap, eta, t_desal, &
    cap_desal, desal_rate, conc_cycle, wat_mkup)
    Wat_consump = wat_mkup - Wat_bleed
END SUBROUTINE Nat_draft_consump


!------------------------------------------COOLING RIVER-------------------------------------------


! 'Model' tab - temperature after mixing (Tmix)
! Water temperature in the river after mixing with waste water from power plant -> TESTED
! Prerequest: wat_con_mod()
SUBROUTINE river_mix_temp(q_cr_m, dt_cr, flow_riv, t_sw, riv_mix_temp)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr_m, flow_riv, dt_cr, t_sw
    DOUBLE PRECISION, INTENT(OUT) :: riv_mix_temp
    DOUBLE PRECISION w_fr
    ! q_cr_m = Desalination modulated condenser reject heat
    ! flow_riv = River water flow volume m3/s
    ! dt_cr = Condenser range temperature
    ! t_sw = Surface water temperature of open water source
    ! riv_mix_temp = River water mixed with power plant output temperature
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    riv_mix_temp = ((w_fr / flow_riv) * dt_cr) + t_sw
END SUBROUTINE river_mix_temp


! HeatCoeffEvap()
! Estimation of Heat coefficient of water evaporation (kW/m2 K) according to TRS 155 -> TESTED
SUBROUTINE heat_coeff_wat(temp, spd_wind, q_coeff_wat)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: temp, spd_wind
    DOUBLE PRECISION, INTENT(OUT) :: q_coeff_wat
    ! temp = temperature
    ! spd_wind = wind speed
    ! q_coeff_wat = Heat coefficient of evaporation water (kW/m2 K)
    q_coeff_wat = (2.615 + 1.55 * spd_wind) * Exp(0.05 * temp)
END SUBROUTINE heat_coeff_wat


! HeatCoeff()
! Estimation of Heat coefficient (kW/m2 K) according to TRS 155 -> TESTED
SUBROUTINE heat_coeff(temp, spd_wind, q_coeff)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: temp, spd_wind
    DOUBLE PRECISION, INTENT(OUT) :: q_coeff
    DOUBLE PRECISION q_coeff_wat, conv, rad
    ! temp = temperature
    ! spd_wind = wind speed
    ! q_coeff = Heat coefficient (kW/m2 K)
    ! conv = convection
    ! rad = Radiation
    CALL heat_coeff_wat(temp, spd_wind, q_coeff_wat)
    conv = 3.003 + 1.792 * spd_wind
    rad = 4.6 + 0.0484 * temp
    q_coeff = (rad + q_coeff_wat + conv)
END SUBROUTINE heat_coeff


! 'Model' tab - Evaporation (Evriver)
! The evaporation of water from the river after power plant water output -> TESTED
SUBROUTINE river_evap(q_cr, q_desal, pow_ratio, temp, spd_wind, evap_riv)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr, q_desal, pow_ratio, spd_wind
    DOUBLE PRECISION, INTENT(INOUT) :: temp
    DOUBLE PRECISION, INTENT(OUT) :: evap_riv
    DOUBLE PRECISION q_cr_m, q_lat, q_coeff_wat, q_coeff
    CALL Heat_con_mod(q_cr, q_desal, pow_ratio, q_cr_m)
    CALL Heat_lat(temp, q_lat)
    CALL heat_coeff_wat(temp, spd_wind, q_coeff_wat)
    CALL heat_coeff(temp, spd_wind, q_coeff)
    evap_riv = (q_cr_m / q_lat) * (q_coeff_wat / q_coeff)
END SUBROUTINE river_evap


!-------------------------------------------COOLING POND-------------------------------------------


! 'Model' tab - temperature increment in pond (Dtpond)
! The change in temperature of the cooling pond -> TESTED
SUBROUTINE pond_dtemp(dt_ca, dt_cr, dt_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: dt_ca, dt_cr
    DOUBLE PRECISION, INTENT(OUT) :: dt_pond
    dt_pond = dt_cr / log(1 + dt_cr / dt_ca)
END SUBROUTINE pond_dtemp


! 'Model' tab - Average water temperature in pond (Tpond)
! The average pond temperature as defined by the air temperature -> TESTED
SUBROUTINE pond_temp(dt_pond, t_dry, t_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: dt_pond, t_dry
    DOUBLE PRECISION, INTENT(OUT) :: t_pond
    t_pond = t_dry + dt_pond
END SUBROUTINE pond_temp


! 'Model' tab - Surface needed (Spond)
! The surface area of the cooling pond required to sufficiently cool the reactor -> TESTED
SUBROUTINE pond_area(q_cr_m, dt_cr, t_pond, spd_wind, pond_km2)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr_m, dt_cr, t_pond, spd_wind
    DOUBLE PRECISION, INTENT(OUT) :: pond_km2
    DOUBLE PRECISION w_fr, q_coeff
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    CALL heat_coeff(t_pond, spd_wind, q_coeff)
    pond_km2 = (w_fr * 1000) * (4.187 / (q_coeff / 1000)) * log(1 + dt_cr /(dt_cr))*0.000001
END SUBROUTINE pond_area


! 'Model' tab - Evaporation (Evpond)
! The evaporation for the cooling pond as the temperature is raised from heating -> TESTED
SUBROUTINE pond_evap(pond_km2, spd_wind, t_pond, r_h, t_dry, evap_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: pond_km2, spd_wind, t_pond, r_h, t_dry
    DOUBLE PRECISION, INTENT(OUT) :: evap_pond
    evap_pond = 0.01 * pond_km2 * (2.12 + 1.25 * spd_wind) * (EXP(0.05 * t_pond) - r_h * EXP(0.05 * t_dry))
END SUBROUTINE pond_evap


! 'Model' tab - Pond bleed (Bleed)
! The bleed from the cooling circuit when using a cooling pond -> TESTED
SUBROUTINE sub_pond_bleed(evap_pond, conc_cycle, pond_bleed)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: evap_pond, conc_cycle
    DOUBLE PRECISION, INTENT(OUT) :: pond_bleed
    pond_bleed = evap_pond / (conc_cycle - 1)
END SUBROUTINE sub_pond_bleed


!----------------------------------------CONSTRUCTION NEEDS----------------------------------------


! 'report tab' - Excavation
! The water used during construction for excavation
SUBROUTINE excav_water(elec_cap, wat_excav)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_excav
    wat_excav = (50 * elec_cap) - 30000
END SUBROUTINE excav_water


! 'report tab' - Concrete mixing
! The water needed for making the concrete for construction
SUBROUTINE concrete_water(elec_cap, wat_concrt)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_concrt
    wat_concrt = (83.33 * elec_cap) + 3333
    wat_concrt = nint(wat_concrt / 100) * 100
END SUBROUTINE concrete_water


! 'report tab' - Supply for construction staff
! The water used by the construction staff during construction of the power plant
SUBROUTINE constr_water(elec_cap, wat_constr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_constr
    wat_constr = (500 * elec_cap) - 100000
    wat_constr = nint(wat_constr / 100) * 100
END SUBROUTINE constr_water


!---------------------------------WATER NEEDS DURING COMMISSIONING---------------------------------


! 'report tab' - Flushing , cleaning etc
! The water for 'domestic' use during commissioning of the power plant
SUBROUTINE domestic_water(elec_cap, wat_dom)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_dom
    wat_dom = ((16.67 * elec_cap) + 6666.6)    
    wat_dom = nint(wat_dom / 100) * 100
END SUBROUTINE domestic_water


! 'report tab' - Drinking water
! The water drunk by workers during commissioning of the power planty
SUBROUTINE drink_water(wat_constr, wat_drink)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: wat_constr
    DOUBLE PRECISION, INTENT(OUT) :: wat_drink
    wat_drink = (300.0 / 6000.0) * wat_constr
END SUBROUTINE drink_water


!-----------------------------------WATER NEEDS DURING OPERATION-----------------------------------


! 'report tab' - Makeup for Secondary loop
! The water loss from the secondary loop; leaks etc
SUBROUTINE sec_makeup(elec_cap, wat_mkup_2)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_mkup_2
    wat_mkup_2 = 6341 * ((elec_cap / 1900)**0.8)
END SUBROUTINE sec_makeup


! 'report tab' - Makeup for primary loop
! The water loss from the primary loop; leaks etc
SUBROUTINE prim_makeup(elec_cap, wat_mkup_1)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_mkup_1
    wat_mkup_1 = 220 * ((elec_cap / 1900)**0.85)
END SUBROUTINE prim_makeup


! 'report tab' - Waste Treatment
! The water required to treat waste
SUBROUTINE waste_water(elec_cap, wat_wst)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_wst
    wat_wst = 2542 * ((elec_cap / 1900)**0.7)    
END SUBROUTINE waste_water


! 'report tab' - Condesate Polishing Plant
! The water required for the polishing plant
SUBROUTINE polish_water(elec_cap, wat_pol)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_pol
    wat_pol = 1838 * ((elec_cap / 1900)**0.8)    
END SUBROUTINE polish_water


! 'report tab' - Component cooling water make up
! The water required for component cooling
SUBROUTINE comp_water(elec_cap, wat_com)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_com
    wat_com = 517 * ((elec_cap / 1900)**0.66)    
END SUBROUTINE comp_water


! 'report tab' - Fire protection
! The water stored in case of fire
SUBROUTINE fire_water(elec_cap, wat_fr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_fr
    wat_fr = 419 * ((elec_cap / 1900) * 0.92)
    ! Should the equation instead be: wat_fr = 419 * ((elec_cap / 1900)**0.92)   
END SUBROUTINE fire_water


! 'report tab' - Sanitary and potable
! The amount of water stored for domestic use
SUBROUTINE potable_water(elec_cap, wat_pot)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: elec_cap
    DOUBLE PRECISION, INTENT(OUT) :: wat_pot
    wat_pot = 4167 * ((elec_cap / 1900)**0.9)    
END SUBROUTINE potable_water


!----------------------------------------------OUTPUT----------------------------------------------


! outputting values -> TESTED
SUBROUTINE output(output_1)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: output_1
    IF (output_1 < 1) THEN
        IF (output_1 < -1) THEN
            WRITE(*,'(f0.2)') output_1
        ELSE
            WRITE(*,'(f4.2)') output_1
        END IF
    ELSE
        WRITE(*,'(f0.2)') output_1
    END IF
    RETURN
END SUBROUTINE output


! https://www.math.fsu.edu/~dmandel/Fortran/ReadingArrayData.pdf
! Outputting a value form an array
!SUBROUTINE print_array(array, n)
!    IMPLICIT NONE
!    DOUBLE PRECISION, INTENT(IN) :: array(n)
!    INTEGER, INTENT(IN) :: n
!    INTEGER i
!    DO i = 1, n
!        WRITE(*,'(f0.2)') array(i)
!    END do
!END SUBROUTINE print_array


!---------------------------------------------APPENDIX---------------------------------------------


! A list of all of the variables used and an explanation of each varibles:

! conc_nh3 -> The parts per million (ppm) of ammonia in the water
! bleed_mwh = Blow-down losses in MWh
! sys_inv -> typ_wat + src_wat_2
! outfile -> Use as the output to OUT.DAT
! t_vap -> Temperature of saturated vapour
! temp -> Generic tempeature for conversion (e.g. Celsius to Kelvin to Fahrenheit)
! src_wat -> Source of water: River/Inland = 0, Sea/Coast = 1, Effluent = 2
! typ_wat -> Type of water: Light = 3 / Heavy = 1


! t_desal = Water temperature required for desalination
! t_c_m = Condenser temperature for desalination
! t_wb = Wet bulb temperature (thermometer wrapped in wet cloth)
! a, b, c = Unknown constants
! p_sat = Partial vapour pressure
! t_dry = Dry bulb temperature (thermometer in air)
! r_h = Relative air humidity
! e_diff = Difference between previous and current iteration guesses
! t_wb_gs = Current iteration guess for the wet bulb temperature
! pres_mb = 1 atmospheric air pressure in millibars
! sign_prev = Previous iteration change sign
! incr = Change to current wet bulb temperature guess
! e_diff_2 = Vapour pressure in bar
! temp_C = Duplicate of dry bulb temperature in oC
! ew_gs = Change to wet bulb temperature guess
! e_gs = Change to wet bulb temperature guess
! sign_cur = Current iteration change sign
! typ_cool = The type of cooling mechanism
! typ_mech = Whether the cooling mechanism is wet, dry or open
! t_sw = Surface water temperature of open water source
! dt_ta = Cooling tower approach temperature (approach = temp leaving minus wet bulb temp)
! dt_cr = Condenser range temperature (range = difference in temperature entering and leaving)
! dt_ca = Condenser approach temperature (approach = temp leaving minus wet bulb temp)
! t_c = Condensing temperature
! dt_c = temperature change across condenser
! t_l_ct = Water temperature leaving cooling tower
! t_l_cd = Water temperature leaving condenser
! fr2 = Water flow rate
! dt_cd_ct = Difference in water temperature arriving at and leaving the cooling tower
! dt_cta = Cooling tower approach temperature
! fr1 = Water flow rate exponent
! s_vp = Saturated vapour pressure
! t_dry_k = Dry bulb temperature in kelvin
! t_func = Function used to derive the saturated vapour pressure
! pressure = Air pressure in pascals
! s_rh_ratio = Humidity ratio of saturated air
! rh_ratio = Relative humidity ratio [kgH2O/kgAIR]
! vap_press = Partial pressure of water vapour in moist air [Pa]
! H_wb = Enthalpy of moist air
! KILO = 1000 grams
! t_wb_2 = storing the wet bulb temperature to convert to celsius
! H_test = Initial enthalpy of most air
! t_wb_prev = Storing value for the wet bulb temperature
! H_prev = Storing value for the enthalpy of moist air
! DHdT = Rate of change of enthalpy per unit temperature
! DT = Change in temperature
! Wat_evap = Water evaporated from the wet cooling water tower
! lg1 = Unknown function result
! RH_r_out = Relative humidity ratio coming out of the cooling tower
! RH_r_prev = Stored value of the RH_r_out
! H_out = Enthalpy of moist air coming out of the cooling tower
! t_wb_out = Wet bulb temperature coming out of the cooling tower
! H_sv = Specific enthalpy of saturated vapour
! H_sl = Specific enthalpy of saturated liquid
! q_lat = Latent heat
! elec_cap = Electricity capacity of power station reactor
! eta = Net efficiency of power station electricity conversion
! q_cr = Heat rejected from the condenser
! cap_desal = Capacity of the deslaination plant
! desal_rate = Rate of desalination as a multiplicative factor
! q_lat_desal = Latent heat of desalination
! q_desal = Heat needed for desalination
! pow_ratio = Power loss ratio
! q_cr_m = Desalination modulated condenser reject heat
! w_fr = Condenser water flow rate (tn/s)
! Wat_evap_rate = Rate the water is evaporated from the cooling tower (m3/s)
! Wat_drift = Water loss from drift
! conc_cycle = The number of times water completes the cooling circuit before being expelled
! Wat_bleed = Water expelled to remove impurities
! wat_mkup = Water required to replenish all water lost from cooling cycle
! Wat_consump = Water lost from the cooling tower by evaporation and drift
! wat_m3 = Water rate in meter cubed
! Hr = Number of seconds in an hour
! wat_mwh = Water rate in Mega Watt hours
! evap_mwh = 
! makeup_mwh = 
! consump_mwh = 
! wat_excav = 
! wat_concrt = 
! wat_constr = 
! wat_dom = 
! wat_drink = 
! wat_mkup_2 = 
! wat_mkup_1 = 
! wat_wst = 
! wat_pol = 
! wat_com = 
! wat_fr = 
! wat_pot = 
! q_cr_m = Desalination modulated condenser reject heat
! dt_cr = Condenser range temperature
! t_pond = Cooling pond temperature
! spd_wind = The wind speed
! pond_km2 = Size of the cooling pond in km2
! w_fr = Condenser water flow rate (tn/s)
! q_coeff = Heat coefficient (kW/m2 K)
! dt_pond = Cooling pond temperature change