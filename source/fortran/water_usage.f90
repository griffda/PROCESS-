PROGRAM water_use_module
    IMPLICIT NONE

    ! Variables
    INTEGER wat_source, wat_type, choice
    DOUBLE PRECISION t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, desal_cap, &
    desal_rate, q_cr, p_therm, t_steam, spd_wind, dislv_solid, suspnd_solid, ammonia, plant_life, &
    discnt_rate, elec_level, plant_avail, con_cycle, p_sat, t_wb, t_c_m, q_lat_desal, q_desal, &
    p_ratio, q_cr_m, w_fr, t_c, wat_evap, wat_bleed, wat_mkup, sup_recirc, sup_evap, sup_bleed, &
    sup_mkup, wat_evap_rate, evap_mwh, bleed_mwh, makeup_mwh, wat_drift, drift_mwh, wat_consump, &
    consump_mwh, wat_excav, wat_concrt, wat_constr_max, wat_constr, wat_dom, wat_drink, &
    wat_mkup_2, wat_mkup_1, wat_wst, wat_pol, wat_com, wat_fr, wat_pot, flow_riv, mix_riv, &
    q_coeff_wat, q_coeff, evap_riv, t_pond, dt_pond, km3_pond, evap_pond, bleed_pond, therm
    DOUBLE PRECISION, PARAMETER :: true = 1, false = 0, zero = 0, one = 1

    ! Arrays
    INTEGER, DIMENSION(1:8) :: type_array, list_array, treatment_array, plume_array, air_pol_array, visual_array, imping_array
    DOUBLE PRECISION, DIMENSION(1:8) :: drift_array, wat_evap_array, wat_dis_array, therm_array
    DOUBLE PRECISION, DIMENSION(4,14) :: array
    CHARACTER(len=30), DIMENSION(1:8) :: name_array
    CHARACTER(len=30) :: a1, a2, a3, a4, a5, a6, a7, a8


    ! List of inputs as described in the excel input:
    ! Nuclear Power Plant
    p_elec = 100  ! Reference electric power (MWe)
    eta = 0.1  ! Reference net efficiency (%)
    t_steam = 350  ! Live steam temperature (oC)
    ! Type
    wat_type = 1 ! Water type: Light = 3 / Heavy = 1
    ! Site/Weather Data
    wat_source = 0  ! Water Source: River/Inland = 0, Sea/Coast = 1, Effluent = 2
    t_db = 40  ! Air temperature (Dry Bulb) (oC)
    r_h = 0.3  ! Relative Humidity (%)
    ! t_wb = 25.2  ! Air temperature (Wet Bulb) (oC)
    t_sw = 20  ! Inlet water temperature (oC) (Tsw)
    flow_riv = 50  ! River flow (if applicable) (m3/s) (friver)
    spd_wind = 2  ! Wind speed (m/s)
    ! Water Quality
    dislv_solid = 1500  ! Total Dissolved Solids (TDS) (ppm)
    suspnd_solid = 25  ! Total SuspENDed Solids (TSS) (ppm)
    ammonia = 30  ! Ammonia (ppm)
    ! Economic Data
    plant_life = 50  ! Lifetime of the power plant (years)
    discnt_rate = 0.06  ! Discount Rate (%)
    elec_level = 45  ! Levelised cost of Electricity ($/MWh)
    plant_avail = 0.8  ! Power plant Availability (%)


    ! Select Cooling System
    c_type = 2  ! Cooling System, e.g. 2 --> 'Wet cooling - Natural Draft'
    ! Approaches and ranges for heat exchanging devices
    dt_ca = 6  ! Condenser Approach (oC)
    dt_cr = 10  ! Condenser Range (oC) (Dtcr)
    ! Cooling Tower Parameters
    dt_ta = 6  ! Cooling Tower Approach (oC)
    con_cycle = 6  ! Cycles of Concentration
    

    ! Supporting systems constants in 'Report tab'
    sup_recirc = 1.2  ! Recirculating cooling water supporting systems
    sup_evap = 0.02  ! Evaporation losses supporting systems
    sup_bleed = 0.01  ! Blow-down losses supporting systems
    sup_mkup = 0.03  ! Make-up water supporting systems
    wat_constr_max = 6000  ! The maximum daily water usage by construction staff


    ! Constants in 'Model tab'
    desal_cap = 0
    desal_rate = 10
    t_desal = 80
    

    CALL p_therm__p_elec(p_elec, eta, p_therm)
    CALL heat_con_rej(p_elec, eta, q_cr)
    CALL pres_sat(t_db, p_sat)
    CALL wet_bulb_temp(t_db, p_sat, r_h, t_wb)
    CALL pres_sat(t_db, p_sat)
    CALL wet_bulb_temp(t_db, p_sat, r_h, t_wb)
    CALL con_temp(c_type, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_db, t_c)
    CALL heat_con_rej(p_elec, eta, q_cr)
    CALL temp_con_mod(t_desal, t_c_m)
    CALL Heat_lat(t_c_m, q_lat_desal) ! 'model tab' - Latent Heat of Desal (Dhdes)
    CALL Heat_desal(Desal_cap, Desal_rate, q_lat_desal, q_desal)
    CALL P_loss_ratio(t_c_m, t_c, p_ratio)
    CALL Heat_con_mod(q_cr, q_desal, p_ratio, q_cr_m)
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    CALL Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    CALL Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    CALL Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_bleed)
    CALL Nat_draft_makeup(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_mkup)
    CALL Nat_draft_consump(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_consump)
    CALL Wat_conv(Wat_evap_rate, p_elec, Evap_MWh)
    CALL Wat_conv(Wat_drift, p_elec, Drift_MWh)
    CALL Wat_conv(Wat_bleed, p_elec, Bleed_MWh)
    CALL Wat_conv(Wat_mkup, p_elec, Makeup_MWh)
    CALL Wat_conv(Wat_consump, p_elec, Consump_MWh)
    CALL excav_water(p_elec, wat_excav)
    CALL concrete_water(p_elec, wat_concrt)
    CALL constr_water(p_elec, wat_constr)
    CALL domestic_water(p_elec, wat_dom)
    CALL drink_water(wat_constr, wat_drink)
    CALL sec_makeup(p_elec, wat_mkup_2)
    CALL prim_makeup(p_elec, wat_mkup_1)
    CALL waste_water(p_elec, wat_wst)
    CALL polish_water(p_elec, wat_pol)
    CALL comp_water(p_elec, wat_com)
    CALL fire_water(p_elec, wat_fr)
    CALL potable_water(p_elec, wat_pot)
    CALL river_mix_temp(q_cr_m, dt_cr, flow_riv, t_sw, mix_riv)
    CALL heat_coeff_wat(mix_riv, spd_wind, q_coeff_wat)
    CALL heat_coeff(mix_riv, spd_wind, q_coeff)
    CALL river_evap(q_cr, q_desal, p_ratio, mix_riv, spd_wind, evap_riv)
    CALL sys_inven_ch(wat_source, wat_type, choice)
    CALL sys_inven(array)
    CALL pond_dtemp(dt_ca, dt_cr, dt_pond)
    CALL pond_temp(dt_pond, t_db, t_pond)
    CALL pond_area(q_cr_m, dt_cr, t_pond, spd_wind, km3_pond)
    CALL pond_evap(km3_pond, spd_wind, t_pond, r_h, t_db, evap_pond)
    CALL pond_bleed(evap_pond, con_cycle, bleed_pond)


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
    wat_dis_array = [w_fr-evap_riv, w_fr-evap_riv, bleed_pond, Wat_bleed, Wat_bleed, zero, Wat_bleed, zero]
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
    type_array = [1, 1, 1, 2, 2, 3, 2, 2]


    ! Printing the report
    WRITE(*,*) ' '//NEW_LINE('A')//" Power Plant Specification:"
    WRITE(*,*) "Type ---> Fusion Reactor"
    WRITE(*,'(a60)',advance='no') "Reactor electric capacity (MWe) = "
    CALL output(p_elec)
    WRITE(*,'(a60)',advance='no') "Reactor thermal capacity (MWth) = "
    CALL output(P_therm)
    WRITE(*,'(a60)',advance='no') "Reference efficiency (%) = "
    CALL output(eta*100)
    WRITE(*,'(a60)',advance='no') "Rejected heat (MWth) = "
    CALL output(q_cr)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Site conditions:"
    WRITE(*,*) "Water source / Plant Location:"
    CALL water_source(wat_source)
    WRITE(*,'(a60)',advance='no') "Dry bulb temperature (oC) = "
    CALL output(t_db)
    WRITE(*,'(a60)',advance='no') "Wet bulb temperature (oC) = "
    CALL output(t_wb)
    WRITE(*,'(a60)',advance='no') "Relative Humidity (%) = "
    CALL output(r_h*100)
    WRITE(*,'(a60)',advance='no') "Surface water temperature (oC) = "
    CALL output(t_sw)
    WRITE(*,'(a60)',advance='no') "Average wind velocity (m/s) = "
    CALL output(spd_wind)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Summary of Results:"
    WRITE(*,'(a60)',advance='no') "Recirculating cooling water - main (m3/s) = "
    CALL output(w_fr)
    WRITE(*,'(a60)',advance='no') "Recirculating cooling water - supporting (m3/s) = "
    CALL output(Sup_recirc)
    WRITE(*,'(a60)',advance='no') "Evaporation losses - main (m3/s) = "
    CALL output(Wat_evap_rate)
    WRITE(*,'(a60)',advance='no') "Evaporation losses - supporting (m3/s) = "
    CALL output(Sup_evap)
    WRITE(*,'(a60)',advance='no') "Blow-down losses - main (m3/s) = "
    CALL output(Wat_bleed)
    WRITE(*,'(a60)',advance='no') "Blow-down losses - supporting (m3/s) = "
    CALL output(Sup_bleed)
    WRITE(*,'(a60)',advance='no') "Make-up water - main (m3/s) = "
    CALL output(Wat_mkup)
    WRITE(*,'(a60)',advance='no') "Make-up water - supporting (m3/s) = "
    CALL output(Sup_mkup)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Total Water needed:"
    WRITE(*,'(a60)',advance='no') "During Construction (m3) = "
    CALL output(-1*(wat_excav+wat_concrt+wat_constr))
    WRITE(*,'(a60)',advance='no') "During commissioning (m3) = "
    CALL output(wat_dom+wat_drink)
    WRITE(*,'(a60)',advance='yes') "During opreation:"
    WRITE(*,'(a60)',advance='yes') "Condenser cooling - withdrawal (Million m3/month) = "
    WRITE(*,'(a60)') "Need to calculate form spREADhseet"
    WRITE(*,'(a60)',advance='no') "Other support systems (m3/month) = "
    CALL output(wat_mkup_2+wat_mkup_1+wat_wst+wat_pol+wat_com+wat_fr+wat_pot)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Total water inventory needed:"
    WRITE(*,'(a60)',advance='no') "fresh water (m3) = "
    CALL output(array(choice,1)+array(choice,2)+array(choice,4))
    WRITE(*,'(a60)',advance='no') "Demineralised water (m3) = "
    CALL output(array(choice,3)+sum(array(choice,5:14)))

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Design variables:"
    WRITE(*,*) "Cooling system type ---> Wet cooling - Natural Draft"
    WRITE(*,'(a60)',advance='no') "Cycles of Concentration ="
    CALL output(con_cycle)
    WRITE(*,'(a60)',advance='no') "Condenser Approach (oC) = "
    CALL output(dt_ca)
    WRITE(*,'(a60)',advance='no') "Condenser Range (oC) = "
    CALL output(dt_cr)
    WRITE(*,'(a60)',advance='no') "Cooling Tower Approach (oC) = "
    CALL output(dt_ta)

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Results:"
    WRITE(*,'(a60)',advance='no') "Evaporation losses (m3/s) = "
    CALL output(Wat_evap_rate)
    WRITE(*,'(a60)',advance='no') "Evaporation losses (MWh) = "
    CALL output(Evap_MWh)
    WRITE(*,'(a60)',advance='no') "Drift losses (m3/s) = "
    CALL output(Wat_drift)
    WRITE(*,'(a60)',advance='no') "Drift losses (MWh) = "
    CALL output(Drift_MWh)
    WRITE(*,'(a60)',advance='no') "Blow-down losses (m3/s) = "
    CALL output(Wat_bleed)
    WRITE(*,'(a60)',advance='no') "Blow-down losses (MWh) = "
    CALL output(Bleed_MWh)
    WRITE(*,'(a60)',advance='no') "Cooling system makeup water (m3/s) = "
    CALL output(Wat_mkup)
    WRITE(*,'(a60)',advance='no') "Cooling system makeup water (MWh) = "
    CALL output(Makeup_MWh)
    WRITE(*,'(a60)',advance='no') "Total consumption (m3/s) = "
    CALL output(Wat_consump)
    WRITE(*,'(a60)',advance='no') "Total consumption (MWh) = "
    CALL output(Consump_MWh)

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
    CALL output(array(choice,1))
    WRITE(*,'(a60)',advance='no') "Service water circuit (IDCT) + 7 days storage (m3) = "
    CALL output(array(choice,2))
    WRITE(*,'(a60)',advance='no') "Fire water circuit - pipes (m3) = "
    CALL output(array(choice,3))
    WRITE(*,'(a60)',advance='no') "Fire water circuit - storage (m3) = "
    CALL output(array(choice,4))
    WRITE(*,'(a60)',advance='no') "Nuclear systems & component cooling circuit (m3) = "
    CALL output(array(choice,5))
    WRITE(*,'(a60)',advance='no') "TG component cooling circuit (m3) = "
    CALL output(array(choice,6))
    WRITE(*,'(a60)',advance='no') "Feed water circuit (m3) = "
    CALL output(array(choice,7))
    WRITE(*,'(a60)',advance='no') "Steam generator secondary side inventory (m3) = "
    CALL output(array(choice,8))
    WRITE(*,'(a60)',advance='no') "Reactor auxilary circuits (m3) = "
    CALL output(array(choice,9))
    WRITE(*,'(a60)',advance='no') "Spent fuel bay cooling circuit (m3) = "
    CALL output(array(choice,10))
    WRITE(*,'(a60)',advance='no') "Emergency core cooling circuit (and dump inventory) (m3) = "
    CALL output(array(choice,11))
    WRITE(*,'(a60)',advance='no') "Emergency feed water pools and circuit (m3) = "
    CALL output(array(choice,12))
    WRITE(*,'(a60)',advance='no') "Primary heat transport system (m3) = "
    CALL output(array(choice,13))
    WRITE(*,'(a60)',advance='no') "Moderator system (m3) = "
    CALL output(array(choice,14))

    WRITE(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    "Testing"
    PRINT *, t_pond    

END PROGRAM water_use_module


!--------------------------------------------------------------------------------------------------


! List of the variables used in all SUBROUTINEs:
    ! temp = temperature converted between celsius/fahrenheit/kelvin or of cooling system
    ! t_desal = Water temperature required for desalination
    ! t_c_m = Condenser temperature for desalination
    ! t_wb = Wet bulb temperature (thermometer wrapped in wet cloth)
    ! a, b, c = Unknown constants
    ! p_sat = Partial vapour pressure
    ! t_db = Dry bulb temperature (thermometer in air)
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
    ! c_type = The type of cooling mechanism
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
    ! s_vp = Saturated vapor pressure
    ! t_db_k = Dry bulb temperature in kelvin
    ! t_func = Function used to derive the saturated vapor pressure
    ! pressure = Air pressure in pascals
    ! s_rh_ratio = Humidity ratio of saturated air
    ! rh_ratio = Relative humidity ratio [kgH2O/kgAIR]
    ! vap_press = Partial pressure of water vapor in moist air [Pa]
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
    ! p_elec = Electricity capacity of power station reactor
    ! eta = Net efficiency of power station electricity conversion
    ! q_cr = Heat rejected from the condenser
    ! Desal_cap = Capacity of the deslaination plant
    ! Desal_rate = Rate of desalination as a multiplicative factor
    ! q_lat_desal = Latent heat of desalination
    ! q_desal = Heat needed for desalination
    ! p_ratio = Power loss ratio
    ! q_cr_m = Desalination modulated condenser reject heat
    ! w_fr = Condenser water flow rate (tn/s)
    ! Wat_evap_rate = Rate the water is evaporated from the cooling tower (m3/s)
    ! Wat_drift = Water loss from drift
    ! con_cycle = The number of times water completes the cooling circuit before being expelled
    ! Wat_bleed = Water expelled to remove impurities
    ! Wat_mkup = Water required to replenish all water lost from cooling cycle
    ! Wat_consump = Water lost from the cooling tower by evaporation and drift
    ! Wat_m3 = Water rate in meter cubed
    ! Hr = Number of seconds in an hour
    ! Wat_MWh = Water rate in Mega Watt hours
    ! Evap_MWh = 
    ! Bleed_MWh = 
    ! Makeup_MWh = 
    ! Consump_MWh = 
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
    ! km3_pond = Size of the cooling pond in km3
    ! w_fr = Condenser water flow rate (tn/s)
    ! q_coeff = Heat coefficient (kW/m2 K)
    ! dt_pond = Cooling pond temperature change



! Subroutines for the water usage calculations

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
SUBROUTINE water_source(wat_source)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: wat_source
    IF (wat_source == 1) THEN
        WRITE(*,*) "River/Inland"
    ELSE IF (wat_source == 2) THEN
        WRITE(*,*) "Sea/Coast"
    ELSE IF (wat_source == 3) THEN
        WRITE(*,*) "Effluent"
    ELSE
        WRITE(*,*) "Unknown water source"
    END IF
END SUBROUTINE water_source


! 'Report' tab - NPP system inventories choice
! Using water type and water source determines what values to retrieve from sys_inven() -> TESTED
SUBROUTINE sys_inven_ch(wat_source, wat_type, choice)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: wat_type
    INTEGER, intent(inout) :: wat_source
    INTEGER, intent(out) :: choice
    IF (wat_source == 2) THEN
        wat_source = 0
    END IF
    choice = wat_type + wat_source
END SUBROUTINE sys_inven_ch


! 'sheet 2' - NPP system inventories
! A reference SUBROUTINE for the water inventories
SUBROUTINE sys_inven(array)
    IMPLICIT NONE
    DOUBLE PRECISION, dimension(4,14), intent(out) :: array
    array =    reshape ( [6500.0, 0.0, 6500.0, 0.0, &
                        18.6, 0.0, 18.6, 0.0, &
                        415.0, 415.0, 300.0, 300.0, &
                        0.0, 0.0, 1000.0, 3000.0, &
                        1200.0, 1200.0, 500.0, 500.0, &
                        400.0, 400.0, 450.0, 450.0, &
                        600.0, 600.0, 1800.0, 1800.0, &
                        0.0, 0.0, 300.0, 300.0, &
                        780.0, 780.0, 500.0, 500.0, &
                        5100.0, 5100.0, 1700.0, 1700.0, &
                        1200.0, 1200.0, 1900.0, 1900.0, &
                        0.0, 0.0, 1700.0, 1700.0, &
                        400.0, 400.0, 400.0, 400.0, &
                        310.0, 310.0, 0.0, 0.0], &
                        [4,14] )
END SUBROUTINE sys_inven


!--------------------------------------------------------------------------------------------------


! Converting from m3/s to m3/MWh -> TESTED
SUBROUTINE Wat_conv(Wat_m3, p_elec, Wat_MWh)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: Wat_m3, p_elec
    DOUBLE PRECISION, INTENT(OUT) :: Wat_MWh
    DOUBLE PRECISION Hr
    Hr = 3600
    Wat_MWh = (Wat_m3 * Hr) / p_elec
END SUBROUTINE Wat_conv


!--------------------------------------------------------------------------------------------------


! 'report tab' - Reactor thermal capacity
! Calculates the thermal capacity of the rector from the electric capacity -> TESTED
SUBROUTINE p_therm__p_elec(p_elec, eta, P_therm)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec, eta
    DOUBLE PRECISION, INTENT(OUT) ::  P_therm
    P_therm = p_elec / eta
END SUBROUTINE p_therm__p_elec


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
SUBROUTINE pres_sat(t_db, p_sat)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_db
    DOUBLE PRECISION, INTENT(OUT) :: p_sat
    DOUBLE PRECISION a, b, c
    a = 8.07131
    b = 1730.63
    c = 233.426
    p_sat = (10 ** (a - (b / (c + t_db)))) / 760
END SUBROUTINE pres_sat


!--------------------------------------------------------------------------------------------------


! Wetbulb() & 'model tab' - Wet Bulb temperature (Twb)
! Calculating the wet bulb temperature from the dry bulb temperature and partial pressure -> TESTED
SUBROUTINE wet_bulb_temp(t_db, p_sat, r_h, t_wb)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_db, r_h
    DOUBLE PRECISION, INTENT(INOUT) :: p_sat
    DOUBLE PRECISION, INTENT(OUT) :: t_wb
    DOUBLE PRECISION p_sat_r_h, e_diff, t_wb_gs, pres_mb, sign_prev, incr, e_diff_2, temp_C, ew_gs, e_gs, sign_cur
    INTEGER i
    p_sat_r_h = p_sat * r_h
    e_diff = 1
    t_wb_gs = 0
    pres_mb = 1013.25 ! 1 atmospheric pressure in mbar
    sign_prev = 1
    incr = 10
    e_diff_2 = p_sat_r_h * 1000
    temp_C = t_db
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
SUBROUTINE con_temp(c_type, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_db, t_c)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: c_type, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_db
    DOUBLE PRECISION, INTENT(OUT) :: t_c
    DOUBLE PRECISION dt_c
    dt_c = dt_cr + dt_ca
    IF (c_type == 1) THEN
        t_c = t_sw + dt_c
    ELSE IF (c_type == 2) THEN
        t_c = t_wb + dt_ta + dt_c
    ELSE IF (c_type == 3) THEN
        t_c = t_db + dt_ta + dt_c
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
! Saturated vapor pressure as function of the dry bulb temperature -> TESTED
! Prerequest: c_to_k(temp)
SUBROUTINE dry_bulb__sat_vap_press(t_db, s_vp)
    DOUBLE PRECISION, INTENT(IN) :: t_db
    DOUBLE PRECISION, INTENT(OUT) :: s_vp
    DOUBLE PRECISION t_db_k, t_func
    IF (t_db >= -100 .AND. t_db <= 200) THEN
        t_db_k = t_db
        CALL c_to_k(t_db_k)
        IF (t_db >= -100 .AND. t_db <= 0) THEN
            t_func = (-5.6745359 * ((10 ** 3) / t_db_k) + 6.3925247 - 9.677843 * (10.00 ** -3) * &
            t_db_k + 6.2215701 * (10.00 ** -7) * (t_db_k ** 2))
            t_func = t_func + 2.0747825 * (10.00 ** -9) * (t_db_k ** 3) - 9.484024 * &
            (10.00 ** -13) * (t_db_k ** 4) + 4.1635019 * log(t_db_k)
        ELSE IF (t_db > 0 .AND. t_db <= 200) THEN
            t_func = (-5800.2206 / t_db_k) + 1.3914993 - 0.048640239 * t_db_k + 0.000041764768 * &
            (t_db_k ** 2) - 0.000000014452093 * (t_db_k ** 3) + 6.5459673 * log(t_db_k)
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
! Prerequest: dry_bulb__sat_vap_press(t_db, s_vp)
SUBROUTINE dry_bulb__sat_rh_ratio(t_db, pressure, s_rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db
    DOUBLE PRECISION, INTENT(IN) :: pressure
    DOUBLE PRECISION, INTENT(OUT) :: s_rh_ratio
    DOUBLE PRECISION s_vp
    CALL dry_bulb__sat_vap_press(t_db, s_vp)
    s_rh_ratio = 0.621945 * s_vp / (pressure - s_vp)
END SUBROUTINE dry_bulb__sat_rh_ratio


!--------------------------------------------------------------------------------------------------


! GetHumRatiofromTWetBulb()
! Calculate the humidity ratio from the wet and dry bulb temperature -> TESTED
! Prerequest: dry_bulb__sat_rh_ratio(t_db, pressure, s_rh_ratio)
SUBROUTINE wet_bulb__rh_ratio(t_db, t_wb, pressure, rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_wb, pressure
    DOUBLE PRECISION, INTENT(IN) :: t_db
    DOUBLE PRECISION, INTENT(OUT) :: rh_ratio
    DOUBLE PRECISION s_rh_ratio
    IF (t_wb <= t_db) THEN
        CALL dry_bulb__sat_rh_ratio(t_wb, pressure, s_rh_ratio)
        rh_ratio = ((2501 - (2.326 * t_wb)) * s_rh_ratio - 1.006 * (t_db - t_wb)) / &
        (2501 + 1.86 * t_db - 4.186 * t_wb)
    ELSE
        PRINT *, 'The wet bulb temperature is above the dry bulb temperature'
    END IF
END SUBROUTINE wet_bulb__rh_ratio


!--------------------------------------------------------------------------------------------------


! GetVapPresfromRelHum()
! Partial pressure of water vapor as a function of relative humidity and temperature in C -> TESTED
! Prerequest: dry_bulb__sat_vap_press(t_db, s_vp)
SUBROUTINE RH__vap_pressure(t_db, r_h, vap_press)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_db, r_h
    DOUBLE PRECISION, INTENT(OUT) :: vap_press 
    DOUBLE PRECISION s_vp
    IF (r_h > 0 .AND. r_h <= 1) THEN
        CALL dry_bulb__sat_vap_press(t_db, s_vp)
        vap_press = s_vp * r_h
    ELSE
        PRINT *, 'Relative humidity is outside range [0,1]'
    END IF
END SUBROUTINE RH__vap_pressure


!--------------------------------------------------------------------------------------------------


! GetHumRatiofromVapPres()
! Humidity ratio for given water vapor pressure and atmospheric pressure -> TESTED
SUBROUTINE vap_pressure__rh_ratio(vap_press , pressure, rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: vap_press , pressure
    DOUBLE PRECISION, INTENT(OUT) :: rh_ratio
    IF (vap_press < 0) THEN
        PRINT *, 'Partial pressure of water vapor in moist air is negative'
    ELSE
        rh_ratio = 0.621945 * vap_press / (pressure - vap_press )
    END IF
END SUBROUTINE vap_pressure__rh_ratio


!--------------------------------------------------------------------------------------------------


! GetHumRatiofromRelHum()
! Calculating the relative humidity ratio for a given humidity -> TESTED
! Prerequest: RH__vap_pressure(t_db, r_h, vap_press ), 
!             vap_pressure__rh_ratio(vap_press , pressure, rh_ratio)
SUBROUTINE RH__rh_ratio(t_db, r_h, pressure, rh_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, r_h, pressure
    DOUBLE PRECISION, INTENT(OUT) :: rh_ratio
    DOUBLE PRECISION vap_press 
    IF (r_h > 0 .AND. r_h <= 1) THEN
        CALL RH__vap_pressure(t_db, r_h, vap_press )
        CALL vap_pressure__rh_ratio(vap_press , pressure, rh_ratio)
        IF (rh_ratio < 0) THEN
            PRINT *, 'Humidity ratio is negative'
        END IF
    ELSE
        PRINT *, 'Relative humidity is outside range [0,1]'
    END IF
END SUBROUTINE RH__rh_ratio


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
! Prerequest: dry_bulb__sat_vap_press(t_db, s_vp)
SUBROUTINE vap_pressure__RH(t_db, vap_press, r_h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_db, vap_press
    DOUBLE PRECISION, INTENT(OUT) :: r_h
    DOUBLE PRECISION s_vp
    IF (vap_press >= 0) THEN
        CALL dry_bulb__sat_vap_press(t_db, s_vp)
        r_h = vap_press / s_vp
    ELSE
        PRINT *, 'Partial pressure of water vapor in moist air is negative'
    END IF
END SUBROUTINE vap_pressure__RH


!--------------------------------------------------------------------------------------------------


! GetRelHumfromHumRatio()
! Calculating the relative humidity from the humidity ratio -> TESTED
! Prerequest: rh_ratio__vap_pressure()
!             vap_pressure__RH()
SUBROUTINE rh_ratio__RH(t_db, rh_ratio, pressure, r_h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_db, rh_ratio, pressure
    DOUBLE PRECISION, INTENT(OUT) :: r_h
    DOUBLE PRECISION vap_press 
    IF (rh_ratio > 0) THEN
        CALL rh_ratio__vap_pressure(rh_ratio, pressure, vap_press)
        CALL vap_pressure__RH(t_db, vap_press, r_h)
    END IF
END SUBROUTINE rh_ratio__RH


!--------------------------------------------------------------------------------------------------


! GetRelHumfromTWetBulb()
! Calculating the relative humidity from the dry and wet bulb temperature -> 
! Prerequest: wet_bulb__rh_ratio()
!             RH__rh_ratio()
SUBROUTINE Dry_wet_bulb__RH(t_db, t_wb, pressure, r_h)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, t_wb, pressure, r_h
    DOUBLE PRECISION rh_ratio
    IF (t_wb <= t_db) THEN
        CALL wet_bulb__rh_ratio(t_db, t_wb, pressure, rh_ratio)
        CALL rh_ratio__RH(t_db, rh_ratio, pressure, r_h)
    ELSE
        PRINT *, 'Wet bulb temperature is above dry bulb temperature'
    END IF
END SUBROUTINE Dry_wet_bulb__RH


!--------------------------------------------------------------------------------------------------


! GetTDryBulbfromRelHum()
! Calculating the dry bulb temperature from the humidity
! Prerequest: c_to_f()
!             f_to_c()
!             Dry_wet_bulb__RH()
SUBROUTINE RH_wet_bulb__dry_bulb(t_wb, r_h, t_db)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_wb, t_db
    DOUBLE PRECISION, INTENT(IN) :: r_h
    DOUBLE PRECISION RH_test, t_db_prev, RH_prev, dRH_dT, DT, pressure
    INTEGER i
    CALL c_to_f(t_wb)
    t_db = t_wb
    RH_test = 1
    t_db_prev = t_db
    RH_prev = RH_test
    pressure = 101325
    IF (r_h /= 1) THEN
        t_db = t_wb + (0.12 * (t_wb**1.5) * ((1 - r_h)**2.5))
        do i = 0, 10
            CALL f_to_c(t_db)
            CALL f_to_c(t_wb)
            CALL Dry_wet_bulb__RH(t_db, t_wb, pressure, RH_test)
            CALL c_to_f(t_db)
            CALL c_to_f(t_wb)
            dRH_dT = (RH_test - RH_prev) / (t_db - t_db_prev)
            DT = (RH_test - r_h) / dRH_dT
            t_db_prev = t_db
            RH_prev = RH_test
            t_db = t_db - DT
            IF (abs(DT) < 0.0005) THEN
                EXIT
            END IF
        END DO
    END IF
    CALL f_to_c(t_wb)
    CALL f_to_c(t_db)
END SUBROUTINE RH_wet_bulb__dry_bulb


!--------------------------------------------------------------------------------------------------


! GetMoistAirEnthalpy()
! Calculating the enthalpy of moist air from the dry bulb temperature -> TESTED
SUBROUTINE Dry_bulb__enth_wb(t_db, rh_ratio, H_wb)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_db, rh_ratio
    DOUBLE PRECISION, INTENT(OUT) :: H_wb
    DOUBLE PRECISION KILO
    KILO = 1000
    IF (rh_ratio > 0) THEN
        H_wb = ((1.006 * t_db) + (rh_ratio * (2501 + (1.86 * t_db)))) * KILO
    ELSE
        PRINT *, 'Humidity ratio is negative'
    END IF
END SUBROUTINE Dry_bulb__enth_wb


!--------------------------------------------------------------------------------------------------


! GetTWetBulbfromMoistAirEnthalpy()
! Calculating the wet bulb temperature from the moist air enthalpy -> TESTED
! Prerequest: f_to_c()
!             wet_bulb__rh_ratio()
!             Dry_bulb__enth_wb()
SUBROUTINE Enth_wb__t_wb(H_wb, t_wb)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: H_wb
    DOUBLE PRECISION, INTENT(OUT) :: t_wb
    DOUBLE PRECISION t_wb_2, pressure, t_db, rh_ratio, H_test, t_wb_prev, H_prev, DHdT, DT
    INTEGER n, i
    H_wb = H_wb / 2324.4 ! Converting from j/kg to btu/lb
    t_wb = 32.0
    t_wb_2 = t_wb
    pressure = 101325.0
    CALL f_to_c(t_wb_2)
    t_db = t_wb ! Setting the dry bulb temperature equivalent to the wet bulb temperature
    CALL wet_bulb__rh_ratio(t_db, t_wb_2, pressure, rh_ratio)
    CALL Dry_bulb__enth_wb(t_db, rh_ratio, H_test)
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
        t_db = t_wb ! Setting the dry bulb temperature to the wet bulb temperature
        CALL wet_bulb__rh_ratio(t_db, t_wb, pressure, rh_ratio)
        CALL Dry_bulb__enth_wb(t_db, rh_ratio, H_test)
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
END SUBROUTINE Enth_wb__t_wb


!--------------------------------------------------------------------------------------------------


! Hsatv()
! Calculating the specific enthalpy of saturated vapor: kj/kg (oC) -> TESTED
SUBROUTINE Enth_sv(temp, H_sv)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: temp
    DOUBLE PRECISION, INTENT(OUT) :: H_sv
    H_sv = 2500.918 + (1.834961 * temp) - (0.0003841512 * (temp**2)) - (0.000001108345 * &
    (temp**3)) - (0.00000004896947 * (temp**4)) + (1.292303E-10 * (temp**5)) - (2.593501E-13 * &
    (temp**6))
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
SUBROUTINE Heat_desal(Desal_cap, Desal_rate, q_lat_desal, q_desal)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: Desal_cap, Desal_rate, q_lat_desal
    DOUBLE PRECISION, INTENT(OUT) :: q_desal
    q_desal = Desal_cap * (1000 / (Desal_rate * 24 * 3600)) * q_lat_desal
END SUBROUTINE Heat_desal


!---------------------------------------------CONDENSER--------------------------------------------


! 'model tab' - Condenser Reject Heat (Qcr)
! The heat rejected from the condenser -> TESTED
SUBROUTINE heat_con_rej(p_elec, eta, q_cr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec, eta
    DOUBLE PRECISION, INTENT(OUT) :: q_cr
    q_cr = (p_elec / eta) - p_elec
END SUBROUTINE heat_con_rej


! 'model tab' - Power loss ratio (hcar)
! The power loss ratio used for calculating the heat rejected from the condenser -> TESTED
SUBROUTINE P_loss_ratio(t_c_m, t_c, p_ratio)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: t_c_m, t_c
    DOUBLE PRECISION, INTENT(OUT) :: p_ratio
    p_ratio = ((t_c_m - t_c) / (t_c_m + 273)) * 0.85 * 0.988 * 0.97
END SUBROUTINE P_loss_ratio


! 'model tab' - Mod Condenser Reject (Qmcr)
! The condenser reject heat minus the heat used for desalination -> TESTED
SUBROUTINE Heat_con_mod(q_cr, q_desal, p_ratio, q_cr_m)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr, q_desal, p_ratio
    DOUBLE PRECISION, INTENT(OUT) :: q_cr_m
    q_cr_m = q_cr - q_desal * (1 - p_ratio)
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
!              RH_wet_bulb__dry_bulb()
!              wet_bulb__rh_ratio()
!              Dry_bulb__enth_wb()
!              Enth_wb__t_wb()
SUBROUTINE Ct_water_evap(t_l_cd, t_l_ct, t_wb, r_h, Wat_evap)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_l_cd, t_l_ct, t_wb, r_h
    DOUBLE PRECISION, INTENT(OUT) :: Wat_evap
    DOUBLE PRECISION dt_cd_ct, lg1, t_db, pressure, rh_ratio, H_wb
    DOUBLE PRECISION RH_r_out, RH_r_prev, H_out, t_wb_out
    INTEGER i
    CALL c_to_f(t_l_cd)
    CALL c_to_f(t_l_ct)
    CALL c_to_f(t_wb)
    dt_cd_ct = t_l_cd - t_l_ct
    CALL lg(t_l_cd, t_l_ct, t_wb, lg1)
    CALL f_to_c(t_wb)
    CALL RH_wet_bulb__dry_bulb(t_wb, r_h, t_db)
    pressure = 101325
    CALL wet_bulb__rh_ratio(t_db, t_wb, pressure, rh_ratio)
    CALL Dry_bulb__enth_wb(t_db, rh_ratio, H_wb)
    H_wb = H_wb / 2324.4 ! Converting from j/kg to btu/lb
    RH_r_out = rh_ratio + (0.0008 * dt_cd_ct * lg1)
    do i = 0, 5
        RH_r_prev = RH_r_out
        H_out = H_wb + (dt_cd_ct * lg1) + ((RH_r_out - rh_ratio) * (t_l_ct - 32))
        H_out = H_out * 2324.4
        CALL Enth_wb__t_wb(H_out, t_wb_out)
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
SUBROUTINE Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta
    DOUBLE PRECISION, INTENT(INOUT) :: t_desal, Desal_cap, Desal_rate, Wat_evap
    DOUBLE PRECISION, INTENT(OUT) :: Wat_evap_rate
    DOUBLE PRECISION p_sat, t_wb, t_c, q_cr, t_c_m, q_lat_desal, q_desal, q_cr_m, w_fr
    DOUBLE PRECISION t_l_cd, t_l_ct, p_ratio
    CALL pres_sat(t_db, p_sat)
    CALL wet_bulb_temp(t_db, p_sat, r_h, t_wb)
    CALL con_temp(c_type, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_db, t_c)
    CALL heat_con_rej(p_elec, eta, q_cr)
    CALL temp_con_mod(t_desal, t_c_m)
    CALL Heat_lat(t_c_m, q_lat_desal) ! 'model tab' - Latent Heat of Desal (Dhdes)
    CALL Heat_desal(Desal_cap, Desal_rate, q_lat_desal, q_desal)
    CALL P_loss_ratio(t_c_m, t_c, p_ratio)
    CALL Heat_con_mod(q_cr, q_desal, p_ratio, q_cr_m)
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
SUBROUTINE Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta
    DOUBLE PRECISION, INTENT(INOUT) :: t_desal, Desal_cap, Desal_rate
    DOUBLE PRECISION, INTENT(OUT) :: Wat_drift
    DOUBLE PRECISION p_sat, t_wb, t_c, q_cr, t_c_m, q_lat_desal, q_desal, q_cr_m, w_fr, p_ratio
    CALL pres_sat(t_db, p_sat)
    CALL wet_bulb_temp(t_db, p_sat, r_h, t_wb)
    CALL con_temp(c_type, t_sw, t_wb, dt_ta, dt_cr, dt_ca, t_db, t_c)
    CALL heat_con_rej(p_elec, eta, q_cr)
    CALL temp_con_mod(t_desal, t_c_m)
    CALL Heat_lat(t_c_m, q_lat_desal)
    CALL Heat_desal(Desal_cap, Desal_rate, q_lat_desal, q_desal)
    CALL P_loss_ratio(t_c_m, t_c, p_ratio)
    CALL Heat_con_mod(q_cr, q_desal, p_ratio, q_cr_m)
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    Wat_drift = 0.0002 * w_fr
END SUBROUTINE Nat_draft_drift


! 'model tab' - Wet cooling tower bleed-off/blowdown (Fb)
! Water deliberately expelled from the water cycle to remove impurities -> TESTED
! Prerequests: Nat_draft_evap()
!              Nat_draft_drift()
SUBROUTINE Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_bleed)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, &
    t_desal, Desal_cap, Desal_rate, con_cycle
    DOUBLE PRECISION, INTENT(OUT) :: Wat_bleed
    DOUBLE PRECISION Wat_evap, Wat_evap_rate, Wat_drift
    CALL Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    CALL Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    Wat_bleed = (Wat_evap_rate - ((con_cycle - 1) * Wat_drift)) / (con_cycle - 1)
END SUBROUTINE Nat_draft_bleed


! 'model tab' - Wet cooling tower make up (Fmu)
! Water required to replinish the water lost -> TESTED
! Prerequests: Nat_draft_evap()
!              Nat_draft_drift()
!              Nat_draft_bleed()
SUBROUTINE Nat_draft_makeup(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_mkup)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, &
    t_desal, Desal_cap, Desal_rate, con_cycle
    DOUBLE PRECISION, INTENT(OUT) :: Wat_mkup
    DOUBLE PRECISION Wat_evap, Wat_evap_rate, Wat_drift, Wat_bleed
    CALL Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    CALL Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    CALL Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_bleed)
    Wat_mkup = Wat_evap_rate + Wat_drift + Wat_bleed
END SUBROUTINE Nat_draft_makeup


! 'model tab' - Wet cooling tower water consumption
! The water consumed by the cooling tower (excluding blowdown)-> 
SUBROUTINE Nat_draft_consump(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_consump)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(INOUT) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, &
    t_desal, Desal_cap, Desal_rate, con_cycle
    DOUBLE PRECISION, INTENT(OUT) :: Wat_consump
    DOUBLE PRECISION Wat_bleed, Wat_mkup
    CALL Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_bleed)
    CALL Nat_draft_makeup(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, p_elec, eta, t_desal, &
    Desal_cap, Desal_rate, con_cycle, Wat_mkup)
    Wat_consump = Wat_mkup - Wat_bleed
END SUBROUTINE Nat_draft_consump


!------------------------------------------COOLING RIVER-------------------------------------------


! 'Model' tab - temperature after mixing (Tmix)
! Water temperature in the river after mixing with waaste water from power plant -> TESTED
! Prerequest: wat_con_mod()
SUBROUTINE river_mix_temp(q_cr_m, dt_cr, flow_riv, t_sw, mix_riv)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr_m, flow_riv, dt_cr, t_sw
    DOUBLE PRECISION, INTENT(OUT) :: mix_riv
    DOUBLE PRECISION w_fr
    ! q_cr_m = Desalination modulated condenser reject heat
    ! flow_riv = River water flow volume m3/s
    ! dt_cr = Condenser range temperature
    ! t_sw = Surface water temperature of open water source
    ! mix_riv = River water mixed with power plant output temperature
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    mix_riv = ((w_fr / flow_riv) * dt_cr) + t_sw
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
SUBROUTINE river_evap(q_cr, q_desal, p_ratio, temp, spd_wind, evap_riv)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr, q_desal, p_ratio, spd_wind
    DOUBLE PRECISION, INTENT(INOUT) :: temp
    DOUBLE PRECISION, INTENT(OUT) :: evap_riv
    DOUBLE PRECISION q_cr_m, q_lat, q_coeff_wat, q_coeff
    CALL Heat_con_mod(q_cr, q_desal, p_ratio, q_cr_m)
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
SUBROUTINE pond_temp(dt_pond, t_db, t_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: dt_pond, t_db
    DOUBLE PRECISION, INTENT(OUT) :: t_pond
    t_pond = t_db + dt_pond
END SUBROUTINE pond_temp


! 'Model' tab - Surface needed (Spond)
! The surface area of the cooling pond required to sufficiently cool the reactor -> TESTED
SUBROUTINE pond_area(q_cr_m, dt_cr, t_pond, spd_wind, km3_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: q_cr_m, dt_cr, t_pond, spd_wind
    DOUBLE PRECISION, INTENT(OUT) :: km3_pond
    DOUBLE PRECISION w_fr, q_coeff
    CALL wat_con_mod(q_cr_m, dt_cr, w_fr)
    CALL heat_coeff(t_pond, spd_wind, q_coeff)
    km3_pond = (w_fr * 1000) * (4.187 / (q_coeff / 1000)) * log(1 + dt_cr /(dt_cr))*0.000001
END SUBROUTINE pond_area


! 'Model' tab - Evaporation (Evpond)
! The evaporation for the cooling pond as the temperature is raised from heating -> TESTED
SUBROUTINE pond_evap(km3_pond, spd_wind, t_pond, r_h, t_db, evap_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: km3_pond, spd_wind, t_pond, r_h, t_db
    DOUBLE PRECISION, INTENT(OUT) :: evap_pond
    evap_pond = 0.01 * km3_pond * (2.12 + 1.25 * spd_wind) * (EXP(0.05 * t_pond) - r_h * EXP(0.05 * t_db))
END SUBROUTINE pond_evap


! 'Model' tab - Pond bleed (Bleed)
! The bleed from the cooling circuit when using a cooling pond -> TESTED
SUBROUTINE pond_bleed(evap_pond, con_cycle, bleed_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: evap_pond, con_cycle
    DOUBLE PRECISION, INTENT(OUT) :: bleed_pond
    bleed_pond = evap_pond / (con_cycle - 1)
END SUBROUTINE pond_bleed


!----------------------------------------CONSTRUCTION NEEDS----------------------------------------


! 'report tab' - Excavation
! The water used during construction for excavation
SUBROUTINE excav_water(p_elec, wat_excav)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_excav
    wat_excav = (50 * p_elec) - 30000
END SUBROUTINE excav_water


! 'report tab' - Concrete mixing
! The water needed for making the concrete for construction
SUBROUTINE concrete_water(p_elec, wat_concrt)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_concrt
    wat_concrt = (83.33 * p_elec) + 3333
    wat_concrt = nint(wat_concrt / 100) * 100
END SUBROUTINE concrete_water


! 'report tab' - Supply for construction staff
! The water used by the construction staff during construction of the power plant
SUBROUTINE constr_water(p_elec, wat_constr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_constr
    wat_constr = (500 * p_elec) - 100000
    wat_constr = nint(wat_constr / 100) * 100
END SUBROUTINE constr_water


!---------------------------------WATER NEEDS DURING COMMISSIONING---------------------------------


! 'report tab' - Flushing , cleaning etc
! The water for 'domestic' use during commissioning of the power plant
SUBROUTINE domestic_water(p_elec, wat_dom)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_dom
    wat_dom = ((16.67 * p_elec) + 6666.6)    
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
SUBROUTINE sec_makeup(p_elec, wat_mkup_2)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_mkup_2
    wat_mkup_2 = 6341 * ((p_elec / 1900)**0.8)
END SUBROUTINE sec_makeup


! 'report tab' - Makeup for primary loop
! The water loss from the primary loop; leaks etc
SUBROUTINE prim_makeup(p_elec, wat_mkup_1)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_mkup_1
    wat_mkup_1 = 220 * ((p_elec / 1900)**0.85)
END SUBROUTINE prim_makeup


! 'report tab' - Waste Treatment
! The water required to treat waste
SUBROUTINE waste_water(p_elec, wat_wst)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_wst
    wat_wst = 2542 * ((p_elec / 1900)**0.7)    
END SUBROUTINE waste_water


! 'report tab' - Condesate Polishing Plant
! The water required for the polishing plant
SUBROUTINE polish_water(p_elec, wat_pol)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_pol
    wat_pol = 1838 * ((p_elec / 1900)**0.8)    
END SUBROUTINE polish_water


! 'report tab' - Component cooling water make up
! The water required for component cooling
SUBROUTINE comp_water(p_elec, wat_com)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_com
    wat_com = 517 * ((p_elec / 1900)**0.66)    
END SUBROUTINE comp_water


! 'report tab' - Fire protection
! The water stored in case of fire
SUBROUTINE fire_water(p_elec, wat_fr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_fr
    wat_fr = 419 * ((p_elec / 1900) * 0.92)
    ! Should the equation instead be: wat_fr = 419 * ((p_elec / 1900)**0.92)   
END SUBROUTINE fire_water


! 'report tab' - Sanitary and potable
! The amount of water stored for domestic use
SUBROUTINE potable_water(p_elec, wat_pot)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: p_elec
    DOUBLE PRECISION, INTENT(OUT) :: wat_pot
    wat_pot = 4167 * ((p_elec / 1900)**0.9)    
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


!--------------------------------------------------------------------------------------------------