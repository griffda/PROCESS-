PROGRAM water_use_module
    IMPLICIT NONE

    ! Variables
    INTEGER Wat_source, Wat_type, choice
    DOUBLE PRECISION t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, Desal_cap, &
    Desal_rate, Q_cr, P_therm, T_steam, Spd_wind, Dislv_solid, Suspnd_solid, Ammonia, Plant_life, &
    Discnt_rate, Elec_level, Plant_avail, Con_cycle, P_sat, T_wb, T_c_m, Q_lat_desal, Q_desal, &
    P_ratio, Q_cr_m, W_fr, T_c, Wat_evap, Wat_bleed, Wat_mkup, Sup_recirc, Sup_evap, Sup_bleed, &
    Sup_mkup, Wat_evap_rate, Evap_MWh, Bleed_MWh, Makeup_MWh, Wat_drift, Drift_MWh, Wat_consump, &
    Consump_MWh, Wat_excav, Wat_concrt, Wat_constr_max, Wat_constr, Wat_dom, Wat_drink, &
    Wat_mkup_2, Wat_mkup_1, Wat_wst, Wat_pol, Wat_com, Wat_fr, Wat_pot, Flow_riv, Mix_riv, &
    Q_coeff_wat, Q_coeff, Evap_riv, t_pond, dt_pond
    DOUBLE PRECISION, PARAMETER :: true = 1, false = 0, zero = 0

    ! Arrays
    INTEGER, DIMENSION(1:8) :: type_array, list_array, treatment_array, plume_array, air_pol_array
    DOUBLE PRECISION, DIMENSION(1:8) :: drift_array
    DOUBLE PRECISION, DIMENSION(4,14) :: array
    CHARACTER(len=30), DIMENSION(1:8) :: name_array
    CHARACTER(len=30) :: a1, a2, a3, a4, a5, a6, a7, a8


    ! List of inputs as described in the excel input:
    ! Nuclear Power Plant
    P_elec = 100  ! Reference electric power (MWe)
    Eta = 0.1  ! Reference net efficiency (%)
    T_steam = 350  ! Live steam temperature (oC)
    ! Type
    Wat_type = 1 ! Water type: Light = 3 / Heavy = 1
    ! Site/Weather Data
    Wat_source = 0  ! Water Source: River/Inland = 0, Sea/Coast = 1, Effluent = 2
    t_db = 40  ! Air temperature (Dry Bulb) (oC)
    r_h = 0.3  ! Relative Humidity (%)
    ! T_wb = 25.2  ! Air Temperature (Wet Bulb) (oC)
    t_sw = 20  ! Inlet water temperature (oC) (Tsw)
    Flow_riv = 50  ! River flow (if applicable) (m3/s) (Friver)
    Spd_wind = 2  ! Wind speed (m/s)
    ! Water Quality
    Dislv_solid = 1500  ! Total Dissolved Solids (TDS) (ppm)
    Suspnd_solid = 25  ! Total Suspended Solids (TSS) (ppm)
    Ammonia = 30  ! Ammonia (ppm)
    ! Economic Data
    Plant_life = 50  ! Lifetime of the power plant (years)
    Discnt_rate = 0.06  ! Discount Rate (%)
    Elec_level = 45  ! Levelised cost of Electricity ($/MWh)
    Plant_avail = 0.8  ! Power plant Availability (%)

    ! Select Cooling System
    c_type = 2  ! Cooling System, e.g. 2 --> 'Wet cooling - Natural Draft'
    ! Approaches and ranges for heat exchanging devices
    dt_ca = 6  ! Condenser Approach (oC)
    dt_cr = 10  ! Condenser Range (oC) (Dtcr)
    ! Cooling Tower Parameters
    dt_ta = 6  ! Cooling Tower Approach (oC)
    Con_cycle = 6  ! Cycles of Concentration
    

    ! Supporting systems constants in 'Report tab'
    Sup_recirc = 1.2  ! Recirculating cooling water supporting systems
    Sup_evap = 0.02  ! Evaporation losses supporting systems
    Sup_bleed = 0.01  ! Blow-down losses supporting systems
    Sup_mkup = 0.03  ! Make-up water supporting systems
    Wat_constr_max = 6000  ! The maximum daily water usage by construction staff


    ! Constants in 'Model tab'
    Desal_cap = 0
    Desal_rate = 10
    T_desal = 80
    

    call P_therm__P_elec(P_elec, Eta, P_therm)
    call Heat_con_rej(P_elec, Eta, Q_cr)
    call Pres_sat(t_db, P_sat)
    call Wet_bulb_temp(t_db, P_sat, r_h, T_wb)
    call Pres_sat(t_db, P_sat)
    call Wet_bulb_temp(t_db, P_sat, r_h, T_wb)
    call Con_temp(c_type, t_sw, T_wb, dt_ta, dt_cr, dt_ca, t_db, T_c)
    call Heat_con_rej(P_elec, Eta, Q_cr)
    call Temp_con_mod(T_desal, T_c_m)
    call Heat_lat(T_c_m, Q_lat_desal) ! 'model tab' - Latent Heat of Desal (Dhdes)
    call Heat_desal(Desal_cap, Desal_rate, Q_lat_desal, Q_desal)
    call P_loss_ratio(T_c_m, T_c, P_ratio)
    call Heat_con_mod(Q_cr, Q_desal, P_ratio, Q_cr_m)
    call Wat_con_mod(Q_cr_m, dt_cr, W_fr)
    call Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    call Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    call Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_bleed)
    call Nat_draft_makeup(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_mkup)
    call Nat_draft_consump(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_consump)
    call Wat_conv(Wat_evap_rate, P_elec, Evap_MWh)
    call Wat_conv(Wat_drift, P_elec, Drift_MWh)
    call Wat_conv(Wat_bleed, P_elec, Bleed_MWh)
    call Wat_conv(Wat_mkup, P_elec, Makeup_MWh)
    call Wat_conv(Wat_consump, P_elec, Consump_MWh)
    call Excav_water(P_elec, Wat_excav)
    call Concrete_water(P_elec, Wat_concrt)
    call Constr_water(P_elec, Wat_constr)
    call domestic_water(P_elec, Wat_dom)
    call Drink_water(Wat_constr, Wat_drink)
    call Sec_makeup(P_elec, Wat_mkup_2)
    call Prim_makeup(P_elec, Wat_mkup_1)
    call Waste_water(P_elec, Wat_wst)
    call Polish_water(P_elec, Wat_pol)
    call Comp_water(P_elec, Wat_com)
    call Fire_water(P_elec, Wat_fr)
    call Potable_water(P_elec, Wat_pot)
    call River_mix_temp(Q_cr_m, Dt_cr, Flow_riv, t_sw, Mix_riv)
    call Heat_coeff_wat(Mix_riv, Spd_wind, Q_coeff_wat)
    call Heat_coeff(Mix_riv, Spd_wind, Q_coeff)
    call River_evap(Q_cr, Q_desal, P_ratio, Mix_riv, Spd_wind, Evap_riv)
    call sys_inven_ch(Wat_source, Wat_type, choice)
    call sys_inven(array)
    CALL pond_temp(Dt_ca, Dt_cr, t_db, t_pond)
    CALL pond_dtemp(Dt_ca, Dt_cr, dt_pond)


    ! Reference arrays for the values to populate the report depending on the cooling system
    list_array = [1, 2, 3, 4, 5, 6, 7, 8]
    treatment_array = [0, 0, 0, 1, 1, 0, 1, 0]
    a1 = 'Once through'; a2 = 'Once through - Cooling pond'; a3 = 'Closed loop - Cooling pond'
    a4 = 'Wet cooling - Mechanical draft'; a5 = 'Wet cooling - Natural draft'
    a6 = 'Dry cooling (Air condenser)'; a7 = 'Hybrid - Plume abatement'
    a8 = 'Hybrid - Water conservation'
    name_array = [a1, a2, a3, a4, a5, a6, a7, a8]
    !wat_evap_array = [Evap_riv, Evap_riv, ]
    drift_array = [zero, zero, zero, Wat_drift, Wat_drift, zero, zero, zero]
    !wat_dis_array = []
    plume_array = [0, 0, 0, 4, 4, 0, 0, 0]
    air_pol_array = [0, 0, 0, 0, 0, 3, 1, 2]
    type_array = [1, 1, 1, 2, 2, 3, 2, 2]


    ! Printing the report
    write(*,*) ' '//NEW_LINE('A')//" Power Plant Specification:"
    write(*,*) "Type ---> Fusion Reactor"
    write(*,'(a60)',advance='no') "Reactor electric capacity (MWe) = "
    call Output(P_elec)
    write(*,'(a60)',advance='no') "Reactor thermal capacity (MWth) = "
    call Output(P_therm)
    write(*,'(a60)',advance='no') "Reference efficiency (%) = "
    call Output(Eta*100)
    write(*,'(a60)',advance='no') "Rejected heat (MWth) = "
    call Output(Q_cr)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Site conditions:"
    write(*,*) "Water source / Plant Location:"
    call Water_source(Wat_source)
    write(*,'(a60)',advance='no') "Dry bulb temperature (oC) = "
    call Output(t_db)
    write(*,'(a60)',advance='no') "Wet bulb temperature (oC) = "
    call Output(T_wb)
    write(*,'(a60)',advance='no') "Relative Humidity (%) = "
    call Output(r_h*100)
    write(*,'(a60)',advance='no') "Surface water temperature (oC) = "
    call Output(t_sw)
    write(*,'(a60)',advance='no') "Average wind velocity (m/s) = "
    call Output(Spd_wind)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Summary of Results:"
    write(*,'(a60)',advance='no') "Recirculating cooling water - main (m3/s) = "
    call Output(W_fr)
    write(*,'(a60)',advance='no') "Recirculating cooling water - supporting (m3/s) = "
    call Output(Sup_recirc)
    write(*,'(a60)',advance='no') "Evaporation losses - main (m3/s) = "
    call Output(Wat_evap_rate)
    write(*,'(a60)',advance='no') "Evaporation losses - supporting (m3/s) = "
    call Output(Sup_evap)
    write(*,'(a60)',advance='no') "Blow-down losses - main (m3/s) = "
    call Output(Wat_bleed)
    write(*,'(a60)',advance='no') "Blow-down losses - supporting (m3/s) = "
    call Output(Sup_bleed)
    write(*,'(a60)',advance='no') "Make-up water - main (m3/s) = "
    call Output(Wat_mkup)
    write(*,'(a60)',advance='no') "Make-up water - supporting (m3/s) = "
    call Output(Sup_mkup)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Total Water needed:"
    write(*,'(a60)',advance='no') "During Construction (m3) = "
    call Output(-1*(Wat_excav+Wat_concrt+Wat_constr))
    write(*,'(a60)',advance='no') "During commissioning (m3) = "
    call Output(Wat_dom+Wat_drink)
    write(*,'(a60)',advance='yes') "During opreation:"
    write(*,'(a60)',advance='yes') "Condenser cooling - withdrawal (Million m3/month) = "
    write(*,'(a60)') "Need to calculate form spreadhseet"
    write(*,'(a60)',advance='no') "Other support systems (m3/month) = "
    call Output(Wat_mkup_2+Wat_mkup_1+Wat_wst+Wat_pol+Wat_com+Wat_fr+Wat_pot)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Total water inventory needed:"
    write(*,'(a60)',advance='no') "Fresh water (m3) = "
    call Output(array(choice,1)+array(choice,2)+array(choice,4))
    write(*,'(a60)',advance='no') "Demineralised water (m3) = "
    call Output(array(choice,3)+sum(array(choice,5:14)))

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Design variables:"
    write(*,*) "Cooling system type ---> Wet cooling - Natural Draft"
    write(*,'(a60)',advance='no') "Cycles of Concentration ="
    call Output(Con_cycle)
    write(*,'(a60)',advance='no') "Condenser Approach (oC) = "
    call Output(dt_ca)
    write(*,'(a60)',advance='no') "Condenser Range (oC) = "
    call Output(dt_cr)
    write(*,'(a60)',advance='no') "Cooling Tower Approach (oC) = "
    call Output(dt_ta)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Results:"
    write(*,'(a60)',advance='no') "Evaporation losses (m3/s) = "
    call Output(Wat_evap_rate)
    write(*,'(a60)',advance='no') "Evaporation losses (MWh) = "
    call Output(Evap_MWh)
    write(*,'(a60)',advance='no') "Drift losses (m3/s) = "
    call Output(Wat_drift)
    write(*,'(a60)',advance='no') "Drift losses (MWh) = "
    call Output(Drift_MWh)
    write(*,'(a60)',advance='no') "Blow-down losses (m3/s) = "
    call Output(Wat_bleed)
    write(*,'(a60)',advance='no') "Blow-down losses (MWh) = "
    call Output(Bleed_MWh)
    write(*,'(a60)',advance='no') "Cooling system makeup water (m3/s) = "
    call Output(Wat_mkup)
    write(*,'(a60)',advance='no') "Cooling system makeup water (MWh) = "
    call Output(Makeup_MWh)
    write(*,'(a60)',advance='no') "Total consumption (m3/s) = "
    call Output(Wat_consump)
    write(*,'(a60)',advance='no') "Total consumption (MWh) = "
    call Output(Consump_MWh)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Water needs during construction:"
    write(*,'(a60)',advance='no') "Excavation (m3) = "
    call Output(-1*(Wat_excav))
    write(*,'(a60)',advance='no') "Concrete mixing (m3) = "
    call Output(Wat_concrt)
    write(*,'(a60)',advance='no') "Supply for construction staff (m3) = "
    call Output(-1*(Wat_constr))

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')//" Water needs during commissioning:"
    write(*,'(a60)',advance='no') "Flushing, cleaning etc (m3) = "
    call Output(Wat_dom)
    write(*,'(a60)',advance='no') "Drinking water (m3) = "
    call Output(-1*(Wat_drink))

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    " Water needs during operation (except from condenser cooling):"
    write(*,'(a60)',advance='no') "Makeup for Secondary loop (m3/month) - Demineralised = "
    call Output(Wat_mkup_2)
    write(*,'(a60)',advance='no') "Makeup for primary (m3/month) - Demineralised = "
    call Output(Wat_mkup_1)
    write(*,'(a60)',advance='no') "Waste Treatment (m3/month) - Fresh Water = "
    call Output(Wat_wst)
    write(*,'(a60)',advance='no') "Condensate Polishing Plant (m3/month) - Demineralised = "
    call Output(Wat_pol)
    write(*,'(a60)',advance='no') "Component cooling water makeup (m3/month) - Demineralised = "
    call Output(Wat_com)
    write(*,'(a60)',advance='no') "Fire protection (m3/month) - Fresh Water = "
    call Output(Wat_fr)
    write(*,'(a60)',advance='no') "Sanitary and potable (m3/month) - Fresh Water = "
    call Output(Wat_pot)

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    " Water system inventories during operation:"
    write(*,'(a60)',advance='no') "Condenser cooling water circuit (m3) = "
    call Output(array(choice,1))
    write(*,'(a60)',advance='no') "Service water circuit (IDCT) + 7 days storage (m3) = "
    call Output(array(choice,2))
    write(*,'(a60)',advance='no') "Fire water circuit - pipes (m3) = "
    call Output(array(choice,3))
    write(*,'(a60)',advance='no') "Fire water circuit - storage (m3) = "
    call Output(array(choice,4))
    write(*,'(a60)',advance='no') "Nuclear systems & component cooling circuit (m3) = "
    call Output(array(choice,5))
    write(*,'(a60)',advance='no') "TG component cooling circuit (m3) = "
    call Output(array(choice,6))
    write(*,'(a60)',advance='no') "Feed water circuit (m3) = "
    call Output(array(choice,7))
    write(*,'(a60)',advance='no') "Steam generator secondary side inventory (m3) = "
    call Output(array(choice,8))
    write(*,'(a60)',advance='no') "Reactor auxilary circuits (m3) = "
    call Output(array(choice,9))
    write(*,'(a60)',advance='no') "Spent fuel bay cooling circuit (m3) = "
    call Output(array(choice,10))
    write(*,'(a60)',advance='no') "Emergency core cooling circuit (and dump inventory) (m3) = "
    call Output(array(choice,11))
    write(*,'(a60)',advance='no') "Emergency feed water pools and circuit (m3) = "
    call Output(array(choice,12))
    write(*,'(a60)',advance='no') "Primary heat transport system (m3) = "
    call Output(array(choice,13))
    write(*,'(a60)',advance='no') "Moderator system (m3) = "
    call Output(array(choice,14))

    write(*,*) ' '//NEW_LINE('A')//' '//NEW_LINE('A')// &
    "Testing"
    print *, t_pond
    print *, t_db, dt_pond

END PROGRAM water_use_module


!--------------------------------------------------------------------------------------------------


! List of the variables used in all subroutines:
    ! Temp = Temperature converted between celsius/fahrenheit/kelvin or of cooling system
    ! T_desal = Water temperature required for desalination
    ! T_c_m = Condenser temperature for desalination
    ! T_wb = Wet bulb temperature (thermometer wrapped in wet cloth)
    ! a, b, c = Unknown constants
    ! P_sat = Partial vapour pressure
    ! t_db = Dry bulb temperature (thermometer in air)
    ! r_h = Relative air humidity
    ! E_diff = Difference between previous and current iteration guesses
    ! T_wb_gs = Current iteration guess for the wet bulb temperature
    ! Pres_mb = 1 atmospheric air pressure in millibars
    ! Sign_prev = Previous iteration change sign
    ! Incr = Change to current wet bulb temperature guess
    ! E_diff_2 = Vapour pressure in bar
    ! Temp_C = Duplicate of dry bulb temperature in oC
    ! Ew_gs = Change to wet bulb temperature guess
    ! E_gs = Change to wet bulb temperature guess
    ! Sign_cur = Current iteration change sign
    ! c_type = The type of cooling mechanism
    ! t_sw = Surface water temperature of open water source
    ! dt_ta = Cooling tower approach temperature (approach = temp leaving minus wet bulb temp)
    ! dt_cr = Condenser range temperature (range = difference in temperature entering and leaving)
    ! dt_ca = Condenser approach temperature (approach = temp leaving minus wet bulb temp)
    ! T_c = Condensing temperature
    ! DT_c = Temperature change across condenser
    ! T_l_ct = Water temperature leaving cooling tower
    ! T_l_cd = Water temperature leaving condenser
    ! Fr2 = Water flow rate
    ! DT_cd_ct = Difference in water temperature arriving at and leaving the cooling tower
    ! DT_cta = Cooling tower approach temperature
    ! Fr1 = Water flow rate exponent
    ! S_VP = Saturated vapor pressure
    ! t_db_K = Dry bulb temperature in kelvin
    ! T_func = Function used to derive the saturated vapor pressure
    ! Pressure = Air pressure in pascals
    ! S_RH_ratio = Humidity ratio of saturated air
    ! RH_ratio = Relative humidity ratio [kgH2O/kgAIR]
    ! Vap_press = Partial pressure of water vapor in moist air [Pa]
    ! H_wb = Enthalpy of moist air
    ! KILO = 1000 grams
    ! T_wb_2 = storing the wet bulb temperature to convert to celsius
    ! H_test = Initial enthalpy of most air
    ! T_wb_prev = Storing value for the wet bulb temperature
    ! H_prev = Storing value for the enthalpy of moist air
    ! DHdT = Rate of change of enthalpy per unit temperature
    ! DT = Change in temperature
    ! Wat_evap = Water evaporated from the wet cooling water tower
    ! LG1 = Unknown function result
    ! RH_r_out = Relative humidity ratio coming out of the cooling tower
    ! RH_r_prev = Stored value of the RH_r_out
    ! H_out = Enthalpy of moist air coming out of the cooling tower
    ! T_wb_out = Wet bulb temperature coming out of the cooling tower
    ! H_sv = Specific enthalpy of saturated vapour
    ! H_sl = Specific enthalpy of saturated liquid
    ! Q_lat = Latent heat
    ! P_elec = Electricity capacity of power station reactor
    ! Eta = Net efficiency of power station electricity conversion
    ! Q_cr = Heat rejected from the condenser
    ! Desal_cap = Capacity of the deslaination plant
    ! Desal_rate = Rate of desalination as a multiplicative factor
    ! Q_lat_desal = Latent heat of desalination
    ! Q_desal = Heat needed for desalination
    ! P_ratio = Power loss ratio
    ! Q_cr_m = Desalination modulated condenser reject heat
    ! W_fr = Condenser water flow rate (tn/s)
    ! Wat_evap_rate = Rate the water is evaporated from the cooling tower (m3/s)
    ! Wat_drift = Water loss from drift
    ! Con_cycle = The number of times water completes the cooling circuit before being expelled
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
    ! Wat_excav = 
    ! Wat_concrt = 
    ! Wat_constr = 
    ! Wat_dom = 
    ! Wat_drink = 
    ! Wat_mkup_2 = 
    ! Wat_mkup_1 = 
    ! Wat_wst = 
    ! Wat_pol = 
    ! Wat_com = 
    ! Wat_fr = 
    ! Wat_pot = 


!--------------------------------------------------------------------------------------------------


! Inputting values -> TESTED
subroutine Input(Input_1)
    implicit none
    double precision, intent(out) :: Input_1
    read *, Input_1
    return
end subroutine Input


!--------------------------------------------------------------------------------------------------


! CTOK()
! Converts from celsius to kelvin -> TESTED
subroutine C_to_K(Temp)
    implicit none
    double precision, intent(inout) :: Temp
    Temp = Temp + 273.15
    return
end subroutine


!--------------------------------------------------------------------------------------------------


! CTOF()
! Converts from celsius to fahrenheit -> TESTED
subroutine C_to_F(Temp)
    implicit none
    double precision, intent(inout) :: Temp
    Temp = (9.00/5.00) * Temp + 32.00
    return
end subroutine


!--------------------------------------------------------------------------------------------------


! FTOC()
! Converts from fahrenheit to celsius -> TESTED
subroutine F_to_C(Temp)
    implicit none
    double precision, intent(inout) :: Temp
    Temp = (5.00/9.00) * (Temp - 32.00)
    return
end subroutine


!--------------------------------------------------------------------------------------------------


! Printing the water source type
subroutine Water_source(Wat_source)
    implicit none
    integer, intent(in) :: Wat_source
    if (Wat_source == 1) then
        write(*,*) "River/Inland"
    else if (Wat_source == 2) then
        write(*,*) "Sea/Coast"
    else if (Wat_source == 3) then
        write(*,*) "Effluent"
    else
        write(*,*) "Unknown water source"
    end if
end subroutine Water_source


!--------------------------------------------------------------------------------------------------


! 'report tab' - Reactor thermal capacity
! Calculates the thermal capacity of the rector from the electric capacity -> TESTED
subroutine P_therm__P_elec(P_elec, Eta, P_therm)
    implicit none
    double precision, intent(in) :: P_elec, Eta
    double precision, intent(out) ::  P_therm
    P_therm = P_elec / Eta
end subroutine P_therm__P_elec


!--------------------------------------------------------------------------------------------------


! 'model tab' - Mod Condenser Temperature (Tcm)
! The condenser temperature 10oC greater than the maximum brine temperature -> TESTED
subroutine Temp_con_mod(T_desal, T_c_m)
    implicit none
    double precision, intent(in) :: T_desal
    double precision, intent(out) :: T_c_m
    T_c_m = T_desal + 10
end subroutine Temp_con_mod


!--------------------------------------------------------------------------------------------------


! Psat()
! Calculating the partial vapour pressure from the dry/wet bulb temperature -> TESTED
subroutine Pres_sat(t_db, P_sat)
    implicit none
    double precision, intent(in) :: t_db
    double precision, intent(out) :: P_sat
    double precision a, b, c
    a = 8.07131
    b = 1730.63
    c = 233.426
    P_sat = (10 ** (a - (b / (c + t_db)))) / 760
end subroutine Pres_sat


!--------------------------------------------------------------------------------------------------


! Wetbulb() & 'model tab' - Wet Bulb Temperature (Twb)
! Calculating the wet bulb temperature from the dry bulb temperature and partial pressure -> TESTED
subroutine Wet_bulb_temp(t_db, P_sat, r_h, T_wb)
    implicit none
    double precision, intent(in) :: t_db, r_h
    double precision, intent(inout) :: P_sat
    double precision, intent(out) :: T_wb
    double precision P_sat_RH, E_diff, T_wb_gs, Pres_mb, Sign_prev, Incr, E_diff_2
    double precision Temp_C, Ew_gs, E_gs, Sign_cur
    integer i
    P_sat_RH = P_sat * r_h
    E_diff = 1
    T_wb_gs = 0
    Pres_mb = 1013.25 ! 1 atmospheric pressure in mbar
    Sign_prev = 1
    Incr = 10
    E_diff_2 = P_sat_RH * 1000
    Temp_C = t_db
    i = 0
    do while (abs(E_diff) > 0.05)
        i = i + 1
        Ew_gs = 6.112 * exp((17.67 * T_wb_gs) / (T_wb_gs + 243.5))
        E_gs = Ew_gs - Pres_mb * (Temp_C - T_wb_gs) * 0.00066 * (1 + (0.00115 * T_wb_gs))
        E_diff = E_diff_2 - E_gs
        If (E_diff == 0) then
            exit
        else
            if (E_diff < 0) then
                Sign_cur = -1
                if (Sign_cur /= Sign_prev) then
                    Sign_prev = Sign_cur
                    Incr = Incr / 10
                else
                    Incr = Incr
                end if
            else
                Sign_cur = 1
                if (Sign_cur /= Sign_prev) then
                    Sign_prev = Sign_cur
                    Incr = Incr / 10
                else
                    Incr = Incr
                end if
            end if
        end if
        T_wb_gs = T_wb_gs + Incr * Sign_prev
    end do
    T_wb = T_wb_gs
end subroutine Wet_bulb_temp


!--------------------------------------------------------------------------------------------------


! 'model tab' - Condensing temperature (Tc)
! Calculating the condensing temperature based on the cooling mechanism -> TESTED
subroutine Con_temp(c_type, t_sw, T_wb, dt_ta, dt_cr, dt_ca, t_db, T_c)
    implicit none
    double precision, intent(in) :: c_type, t_sw, T_wb, dt_ta, dt_cr, dt_ca, t_db
    double precision, intent(out) :: T_c
    double precision DT_c
    DT_c = dt_cr + dt_ca
    if (c_type == 1) then
        T_c = t_sw + DT_c
    else if (c_type == 2) then
        T_c = T_wb + dt_ta + DT_c
    else if (c_type == 3) then
        T_c = t_db + dt_ta + DT_c
    else
        print *, "Incorrect cooling type"
    end if
    return
end subroutine Con_temp


!--------------------------------------------------------------------------------------------------


! Fr()
! Unknown function - FROM SPX COOLING -> TESTED
! Could be calculating the flow rate of water passing between cooling tower and condenser
subroutine Fr(T_l_cd, T_l_ct, T_wb, Fr2)
    implicit none
    double precision, intent(in) :: T_l_cd, T_l_ct, T_wb
    double precision, intent(out) :: Fr2
    double precision DT_cd_ct, DT_cta, Fr1
    DT_cd_ct = T_l_cd - T_l_ct
    DT_cta = T_l_ct - T_wb
    Fr1 = 0.60589436 - 0.00562416 * T_wb - 0.00002268 * T_wb * T_wb + 0.04609405 * DT_cd_ct - &
    0.00010031 * T_wb * DT_cd_ct - 0.00074946 * DT_cd_ct * DT_cd_ct - 0.06167657 * DT_cta + &
    0.00008363 * T_wb * DT_cta + 0.0010762 * DT_cta ** 2
    Fr2 = 10 ** Fr1
    return
end subroutine Fr


!--------------------------------------------------------------------------------------------------


! LG()
! Unknown function -> TESTED
! Prerequest: Fr(T_l_cd, T_l_ct, T_wb, Fr2)
subroutine LG(T_l_cd, T_l_ct, T_wb, LG1)
    implicit none
    double precision, intent(in) :: T_l_cd, T_l_ct, T_wb
    double precision, intent(out) :: LG1
    double precision Fr2
    call Fr(T_l_cd, T_l_ct, T_wb, Fr2)
    LG1 = 1.3 / (Fr2 ** 1.15)
    return
end subroutine LG


!--------------------------------------------------------------------------------------------------


! GetSatVapPres()
! Saturated vapor pressure as function of the dry bulb temperature -> TESTED
! Prerequest: C_to_K(temp)
subroutine Dry_bulb__sat_vap_press(t_db, S_VP)
    double precision, intent(in) :: t_db
    double precision, intent(out) :: S_VP
    double precision t_db_K, T_func
    if (t_db >= -100 .and. t_db <= 200) then
        t_db_K = t_db
        call C_to_K(t_db_K)
        if (t_db >= -100 .and. t_db <= 0) then
            T_func = (-5.6745359 * ((10 ** 3) / t_db_K) + 6.3925247 - 9.677843 * (10.00 ** -3) * &
            t_db_K + 6.2215701 * (10.00 ** -7) * (t_db_K ** 2))
            T_func = T_func + 2.0747825 * (10.00 ** -9) * (t_db_K ** 3) - 9.484024 * &
            (10.00 ** -13) * (t_db_K ** 4) + 4.1635019 * Log(t_db_K)
        else if (t_db > 0 .and. t_db <= 200) then
            T_func = (-5800.2206 / t_db_K) + 1.3914993 - 0.048640239 * t_db_K + 0.000041764768 * &
            (t_db_K ** 2) - 0.000000014452093 * (t_db_K ** 3) + 6.5459673 * Log(t_db_K)
        end if
        S_VP = exp(T_func)
    else
        print *, 'Dry bulb temperature is outside range [-100, 200]'
        S_VP = 0.0
    end if
end subroutine Dry_bulb__sat_vap_press


!--------------------------------------------------------------------------------------------------


! GetSatHumRatio()
! The humidity ratio of saturated air for a given dry bulb temperature and pressure -> TESTED
! Prerequest: Dry_bulb__sat_vap_press(t_db, S_VP)
subroutine Dry_bulb__sat_RH_ratio(t_db, Pressure, S_RH_ratio)
    implicit none
    double precision, intent(inout) :: t_db
    double precision, intent(in) :: Pressure
    double precision, intent(out) :: S_RH_ratio
    double precision S_VP
    call Dry_bulb__sat_vap_press(t_db, S_VP)
    S_RH_ratio = 0.621945 * S_VP / (Pressure - S_VP)
end subroutine Dry_bulb__sat_RH_ratio


!--------------------------------------------------------------------------------------------------


! GetHumRatioFromTWetBulb()
! Calculate the humidity ratio from the wet and dry bulb temperature -> TESTED
! Prerequest: Dry_bulb__sat_RH_ratio(t_db, Pressure, S_RH_ratio)
subroutine Wet_bulb__RH_ratio(t_db, T_wb, Pressure, RH_ratio)
    implicit none
    double precision, intent(inout) :: T_wb, Pressure
    double precision, intent(in) :: t_db
    double precision, intent(out) :: RH_ratio
    double precision S_RH_ratio
    if (T_wb <= t_db) then
        call Dry_bulb__sat_RH_ratio(T_wb, Pressure, S_RH_ratio)
        RH_ratio = ((2501 - (2.326 * T_wb)) * S_RH_ratio - 1.006 * (t_db - T_wb)) / &
        (2501 + 1.86 * t_db - 4.186 * T_wb)
    else
        print *, 'The wet bulb temperature is above the dry bulb temperature'
    end if
end subroutine Wet_bulb__RH_ratio


!--------------------------------------------------------------------------------------------------


! GetVapPresFromRelHum()
! Partial pressure of water vapor as a function of relative humidity and temperature in C -> TESTED
! Prerequest: Dry_bulb__sat_vap_press(t_db, S_VP)
subroutine RH__Vap_pressure(t_db, r_h, Vap_press)
    implicit none
    double precision, intent(in) :: t_db, r_h
    double precision, intent(out) :: Vap_press 
    double precision S_VP
    if (r_h > 0 .and. r_h <= 1) then
        call Dry_bulb__sat_vap_press(t_db, S_VP)
        Vap_press = S_VP * r_h
    else
        print *, 'Relative humidity is outside range [0,1]'
    end if
end subroutine RH__Vap_pressure


!--------------------------------------------------------------------------------------------------


! GetHumRatioFromVapPres()
! Humidity ratio for given water vapor pressure and atmospheric pressure -> TESTED
subroutine Vap_pressure__RH_ratio(Vap_press , Pressure, RH_ratio)
    implicit none
    double precision, intent(in) :: Vap_press , Pressure
    double precision, intent(out) :: RH_ratio
    if (Vap_press < 0) then
        print *, 'Partial pressure of water vapor in moist air is negative'
    else
        RH_ratio = 0.621945 * Vap_press / (Pressure - Vap_press )
    end if
end subroutine Vap_pressure__RH_ratio


!--------------------------------------------------------------------------------------------------


! GetHumRatioFromRelHum()
! Calculating the relative humidity ratio for a given humidity -> TESTED
! Prerequest: RH__Vap_pressure(t_db, r_h, Vap_press ), 
!             Vap_pressure__RH_ratio(Vap_press , pressure, RH_ratio)
subroutine RH__RH_ratio(t_db, r_h, Pressure, RH_ratio)
    implicit none
    double precision, intent(inout) :: t_db, r_h, Pressure
    double precision, intent(out) :: RH_ratio
    double precision Vap_press 
    if (r_h > 0 .and. r_h <= 1) then
        call RH__Vap_pressure(t_db, r_h, Vap_press )
        call Vap_pressure__RH_ratio(Vap_press , Pressure, RH_ratio)
        if (RH_ratio < 0) then
            print *, 'Humidity ratio is negative'
        end if
    else
        print *, 'Relative humidity is outside range [0,1]'
    end if
end subroutine RH__RH_ratio


!--------------------------------------------------------------------------------------------------


! GetVapPresFromHumRatio()
! Calculating the vapour pressure from the humidity ratio and pressure -> TESTED
subroutine RH_ratio__Vap_pressure(RH_ratio, Pressure, Vap_press)
    implicit none
    double precision, intent(in) :: RH_ratio, Pressure
    double precision, intent(out) :: Vap_press
    if (RH_ratio > 0) then
        Vap_press = Pressure * (RH_ratio / (0.62198 + RH_ratio))
    else
        print *, 'Humidity ratio is negative'
    end if
end subroutine RH_ratio__Vap_pressure


!--------------------------------------------------------------------------------------------------


! GetRelHumFromVapPres()
! The double precisiontive humidity calculated from the dry bulb temperature and vapour pressure -> TESTED
! Prerequest: Dry_bulb__sat_vap_press(t_db, S_VP)
subroutine Vap_pressure__RH(t_db, Vap_press, r_h)
    implicit none
    double precision, intent(in) :: t_db, Vap_press
    double precision, intent(out) :: r_h
    double precision S_VP
    if (Vap_press >= 0) then
        call Dry_bulb__sat_vap_press(t_db, S_VP)
        r_h = Vap_press / S_VP
    else
        print *, 'Partial pressure of water vapor in moist air is negative'
    end if
end subroutine Vap_pressure__RH


!--------------------------------------------------------------------------------------------------


! GetRelHumFromHumRatio()
! Calculating the relative humidity from the humidity ratio -> TESTED
! Prerequest: RH_ratio__Vap_pressure()
!             Vap_pressure__RH()
subroutine RH_ratio__RH(t_db, RH_ratio, Pressure, r_h)
    implicit none
    double precision, intent(in) :: t_db, RH_ratio, Pressure
    double precision, intent(out) :: r_h
    double precision Vap_press 
    if (RH_ratio > 0) then
        call RH_ratio__Vap_pressure(RH_ratio, Pressure, Vap_press)
        call Vap_pressure__RH(t_db, Vap_press, r_h)
    end if
end subroutine RH_ratio__RH


!--------------------------------------------------------------------------------------------------


! GetRelHumFromTWetBulb()
! Calculating the relative humidity from the dry and wet bulb temperature -> 
! Prerequest: Wet_bulb__RH_ratio()
!             RH__RH_ratio()
subroutine Dry_wet_bulb__RH(t_db, T_wb, Pressure, r_h)
    implicit none
    double precision, intent(inout) :: t_db, T_wb, Pressure, r_h
    double precision RH_ratio
    if (T_wb <= t_db) then
        call Wet_bulb__RH_ratio(t_db, T_wb, Pressure, RH_ratio)
        call RH_ratio__RH(t_db, RH_ratio, Pressure, r_h)
    else
        print *, 'Wet bulb temperature is above dry bulb temperature'
    end if
end subroutine Dry_wet_bulb__RH


!--------------------------------------------------------------------------------------------------


! GetTDryBulbFromRelHum()
! Calculating the dry bulb temperature from the humidity
! Prerequest: C_to_F()
!             F_to_C()
!             Dry_wet_bulb__RH()
subroutine RH_wet_bulb__dry_bulb(T_wb, r_h, t_db)
    implicit none
    double precision, intent(inout) :: T_wb, t_db
    double precision, intent(in) :: r_h
    double precision RH_test, t_db_prev, RH_prev, dRH_dT, DT, Pressure
    integer i
    call C_to_F(T_wb)
    t_db = T_wb
    RH_test = 1
    t_db_prev = t_db
    RH_prev = RH_test
    Pressure = 101325
    if (r_h /= 1) then
        t_db = T_wb + (0.12 * (T_wb**1.5) * ((1 - r_h)**2.5))
        do i = 0, 10
            call F_to_C(t_db)
            call F_to_C(T_wb)
            call Dry_wet_bulb__RH(t_db, T_wb, Pressure, RH_test)
            call C_to_F(t_db)
            call C_to_F(T_wb)
            dRH_dT = (RH_test - RH_prev) / (t_db - t_db_prev)
            DT = (RH_test - r_h) / dRH_dT
            t_db_prev = t_db
            RH_prev = RH_test
            t_db = t_db - DT
            if (abs(DT) < 0.0005) then
                exit
            end if
        end do
    end if
    call F_to_C(T_wb)
    call F_to_C(t_db)
end subroutine RH_wet_bulb__dry_bulb


!--------------------------------------------------------------------------------------------------


! GetMoistAirEnthalpy()
! Calculating the enthalpy of moist air from the dry bulb temperature -> TESTED
subroutine Dry_bulb__enth_wb(t_db, RH_ratio, H_wb)
    implicit none
    double precision, intent(in) :: t_db, RH_ratio
    double precision, intent(out) :: H_wb
    double precision KILO
    KILO = 1000
    if (RH_ratio > 0) then
        H_wb = ((1.006 * t_db) + (RH_ratio * (2501 + (1.86 * t_db)))) * KILO
    else
        print *, 'Humidity ratio is negative'
    end if
end subroutine Dry_bulb__enth_wb


!--------------------------------------------------------------------------------------------------


! GetTWetBulbFromMoistAirEnthalpy()
! Calculating the wet bulb temperature from the moist air enthalpy -> TESTED
! Prerequest: F_to_C()
!             Wet_bulb__RH_ratio()
!             Dry_bulb__enth_wb()
subroutine Enth_wb__T_wb(H_wb, T_wb)
    implicit none
    double precision, intent(inout) :: H_wb
    double precision, intent(out) :: T_wb
    double precision T_wb_2, Pressure, t_db, RH_ratio, H_test, T_wb_prev, H_prev, DHdT, DT
    integer n, i
    H_wb = H_wb / 2324.4 ! Converting from j/kg to btu/lb
    T_wb = 32.0
    T_wb_2 = T_wb
    Pressure = 101325.0
    call F_to_C(T_wb_2)
    t_db = T_wb ! Setting the dry bulb temperature equivalent to the wet bulb temperature
    call Wet_bulb__RH_ratio(t_db, T_wb_2, Pressure, RH_ratio)
    call Dry_bulb__enth_wb(t_db, RH_ratio, H_test)
    H_test = H_test / 2324.4 ! Converting from j/kg to btu/lb
    T_wb_prev = T_wb
    H_prev = H_test
    if (H_wb > 12) then
        T_wb = (38 * log(H_wb)) - 63.5
    else
        T_wb = (2.8 * H_wb) - 0.8
    end if
    n = 0
    do i = 0, 10
        t_db = T_wb ! Setting the dry bulb temperature to the wet bulb temperature
        call Wet_bulb__RH_ratio(t_db, T_wb, Pressure, RH_ratio)
        call Dry_bulb__enth_wb(t_db, RH_ratio, H_test)
        H_test = H_test / 2324.4 ! Converting from j/kg to btu/lb
        DHdT = (H_test - H_prev) / (T_wb - T_wb_prev)
        DT = (H_test - H_wb) / DHdT
        if (((T_wb <= 32.0) .and. ((T_wb - DT) >= 32.0)) .or. &
        ((T_wb >= 32.0) .and. ((T_wb - DT) <= 32.0))) then
            n = n + 1
            if (n > 2) then
                T_wb = 32.0
                return
            end if
        end if
        T_wb_prev = T_wb
        H_prev = H_test
        T_wb = T_wb - DT
        if (abs(DT) < 0.0005) exit
    end do
end subroutine Enth_wb__T_wb


!--------------------------------------------------------------------------------------------------


! Evap()
! Water loss through evaporation from cooling tower (CT) in tn/s -> TESTED
! Prerequests: C_to_F()
!              LG()
!              F_to_C()
!              RH_wet_bulb__dry_bulb()
!              Wet_bulb__RH_ratio()
!              Dry_bulb__enth_wb()
!              Enth_wb__T_wb()
subroutine Ct_water_evap(T_l_cd, T_l_ct, T_wb, r_h, Wat_evap)
    implicit none
    double precision, intent(inout) :: T_l_cd, T_l_ct, T_wb, r_h
    double precision, intent(out) :: Wat_evap
    double precision DT_cd_ct, LG1, t_db, Pressure, RH_ratio, H_wb
    double precision RH_r_out, RH_r_prev, H_out, T_wb_out
    integer i
    call C_to_F(T_l_cd)
    call C_to_F(T_l_ct)
    call C_to_F(T_wb)
    DT_cd_ct = T_l_cd - T_l_ct
    call LG(T_l_cd, T_l_ct, T_wb, LG1)
    call F_to_C(T_wb)
    call RH_wet_bulb__dry_bulb(T_wb, r_h, t_db)
    Pressure = 101325
    call Wet_bulb__RH_ratio(t_db, T_wb, Pressure, RH_ratio)
    call Dry_bulb__enth_wb(t_db, RH_ratio, H_wb)
    H_wb = H_wb / 2324.4 ! Converting from j/kg to btu/lb
    RH_r_out = RH_ratio + (0.0008 * DT_cd_ct * LG1)
    do i = 0, 5
        RH_r_prev = RH_r_out
        H_out = H_wb + (DT_cd_ct * LG1) + ((RH_r_out - RH_ratio) * (T_l_ct - 32))
        H_out = H_out * 2324.4
        call Enth_wb__T_wb(H_out, T_wb_out)
        call Wet_bulb__RH_ratio(T_wb_out, T_wb_out, Pressure, RH_r_out)
        if (abs(RH_r_out - RH_r_prev) <= 0.00001) exit
    end do
    Wat_evap = (RH_r_out - RH_ratio) / LG1
end subroutine Ct_water_evap


!--------------------------------------------------------------------------------------------------


! Hsatv()
! Calculating the specific enthalpy of saturated vapor: kj/kg (oC) -> TESTED
subroutine Enth_sv(Temp, H_sv)
    implicit none
    double precision, intent(in) :: Temp
    double precision, intent(out) :: H_sv
    H_sv = 2500.918 + (1.834961 * Temp) - (0.0003841512 * (Temp**2)) - (0.000001108345 * &
    (Temp**3)) - (0.00000004896947 * (Temp**4)) + (1.292303E-10 * (Temp**5)) - (2.593501E-13 * &
    (Temp**6))
end subroutine Enth_sv


!--------------------------------------------------------------------------------------------------


! Hsatl()
! Calculating the specific enthalpy of saturated liquid: kj/kg (oC) -> TESTED
subroutine Enth_sl(Temp, H_sl)
    implicit none
    double precision, intent(in) :: Temp
    double precision, intent(out) :: H_sl
    H_sl = 0.007529613 + (4.212269 * Temp) - (0.001194192 * (Temp**2)) + (0.00002101532 * &
    (Temp**3)) - (0.0000001935015 * (Temp**4)) + (0.000000001042925 * (Temp**5)) - (2.128003 * &
    (10.0**-12) * (Temp**6))
end subroutine Enth_sl


!--------------------------------------------------------------------------------------------------


! LatentHeat()
! The latent heat from the cooling process -> TESTED
! Prerequests: Enth_sv()
!              Enth_sl()
subroutine Heat_lat(Temp, Q_lat)
    implicit none
    double precision, intent(inout) :: Temp
    double precision, intent(out) :: Q_lat
    double precision H_sv, H_sl
    call Enth_sv(Temp, H_sv)
    call Enth_sl(Temp, H_sl)
    Q_lat = H_sv - H_sl
end subroutine Heat_lat


!--------------------------------------------------------------------------------------------------


! 'model tab' - Condenser Reject Heat (Qcr)
! The heat rejected from the condenser -> TESTED
subroutine Heat_con_rej(P_elec, Eta, Q_cr)
    implicit none
    double precision, intent(in) :: P_elec, Eta
    double precision, intent(out) :: Q_cr
    Q_cr = (P_elec / Eta) - P_elec
end subroutine Heat_con_rej


!--------------------------------------------------------------------------------------------------


! 'model tab' - Heat needed for desal (Qd)
! The heat required for the desalination plant -> TESTED
subroutine Heat_desal(Desal_cap, Desal_rate, Q_lat_desal, Q_desal)
    implicit none
    double precision, intent(in) :: Desal_cap, Desal_rate, Q_lat_desal
    double precision, intent(out) :: Q_desal
    Q_desal = Desal_cap * (1000 / (Desal_rate * 24 * 3600)) * Q_lat_desal
end subroutine Heat_desal


!--------------------------------------------------------------------------------------------------


! 'model tab' - Power loss ratio (hcar)
! The power loss ratio used for calculating the heat rejected from the condenser -> TESTED
subroutine P_loss_ratio(T_c_m, T_c, P_ratio)
    implicit none
    double precision, intent(in) :: T_c_m, T_c
    double precision, intent(out) :: P_ratio
    P_ratio = ((T_c_m - T_c) / (T_c_m + 273)) * 0.85 * 0.988 * 0.97
end subroutine P_loss_ratio


!--------------------------------------------------------------------------------------------------


! 'model tab' - Mod Condenser Reject (Qmcr)
! The condenser reject heat minus the heat used for desalination -> TESTED
subroutine Heat_con_mod(Q_cr, Q_desal, P_ratio, Q_cr_m)
    implicit none
    double precision, intent(in) :: Q_cr, Q_desal, P_ratio
    double precision, intent(out) :: Q_cr_m
    Q_cr_m = Q_cr - Q_desal * (1 - P_ratio)
end subroutine Heat_con_mod


!--------------------------------------------------------------------------------------------------


! 'model tab' - Mod Condenser Water Flow rate (Fmcc)
! The water flowing through the condenser minus heat used for desalination -> TESTED
subroutine Wat_con_mod(Q_cr_m, dt_cr, W_fr)
    implicit none
    double precision, intent(in) :: Q_cr_m, dt_cr
    double precision, intent(out) :: W_fr
    W_fr = Q_cr_m / (dt_cr * 4.187)
end subroutine Wat_con_mod


!--------------------------------------------------------------------------------------------------


! 'model tab' - Wet cooling tower evaporation rate (Fe)
! The water evaporated by the cooling tower to the atmosphere by natural draft -> TESTED
! Prerequests: Pres_sat()
!              Wet_bulb_temp()
!              Con_temp()
!              Heat_con_rej()
!              Temp_con_mod()
!              Heat_lat()
!              Heat_desal()
!              P_loss_ratio()
!              Heat_con_mod()
!              Wat_con_mod()
!              Ct_water_evap()
subroutine Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    implicit none
    double precision, intent(inout) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta
    double precision, intent(inout) :: T_desal, Desal_cap, Desal_rate, Wat_evap
    double precision, intent(out) :: Wat_evap_rate
    double precision P_sat, T_wb, T_c, Q_cr, T_c_m, Q_lat_desal, Q_desal, Q_cr_m, W_fr
    double precision T_l_cd, T_l_ct, P_ratio
    call Pres_sat(t_db, P_sat)
    call Wet_bulb_temp(t_db, P_sat, r_h, T_wb)
    call Con_temp(c_type, t_sw, T_wb, dt_ta, dt_cr, dt_ca, t_db, T_c)
    call Heat_con_rej(P_elec, Eta, Q_cr)
    call Temp_con_mod(T_desal, T_c_m)
    call Heat_lat(T_c_m, Q_lat_desal) ! 'model tab' - Latent Heat of Desal (Dhdes)
    call Heat_desal(Desal_cap, Desal_rate, Q_lat_desal, Q_desal)
    call P_loss_ratio(T_c_m, T_c, P_ratio)
    call Heat_con_mod(Q_cr, Q_desal, P_ratio, Q_cr_m)
    call Wat_con_mod(Q_cr_m, dt_cr, W_fr)
    T_l_cd = T_c - dt_ca
    T_l_ct = T_wb + dt_ta
    call Ct_water_evap(T_l_cd, T_l_ct, T_wb, r_h, Wat_evap)
    Wat_evap_rate = Wat_evap * W_fr
end subroutine Nat_draft_evap


!--------------------------------------------------------------------------------------------------


! 'model tab' - Wet cooling tower drift loss (Fd)
! Water that leaves the cooling tower as droplets and not as evaporation -> TESTED
! Prerequests: Pres_sat()
!              Wet_bulb_temp()
!              Con_temp()
!              Heat_con_rej()
!              Temp_con_mod()
!              Heat_lat()
!              Heat_desal()
!              P_loss_ratio()
!              Heat_con_mod()
!              Wat_con_mod()
subroutine Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    implicit none
    double precision, intent(inout) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta
    double precision, intent(inout) :: T_desal, Desal_cap, Desal_rate
    double precision, intent(out) :: Wat_drift
    double precision P_sat, T_wb, T_c, Q_cr, T_c_m, Q_lat_desal, Q_desal, Q_cr_m, W_fr, P_ratio
    call Pres_sat(t_db, P_sat)
    call Wet_bulb_temp(t_db, P_sat, r_h, T_wb)
    call Con_temp(c_type, t_sw, T_wb, dt_ta, dt_cr, dt_ca, t_db, T_c)
    call Heat_con_rej(P_elec, Eta, Q_cr)
    call Temp_con_mod(T_desal, T_c_m)
    call Heat_lat(T_c_m, Q_lat_desal)
    call Heat_desal(Desal_cap, Desal_rate, Q_lat_desal, Q_desal)
    call P_loss_ratio(T_c_m, T_c, P_ratio)
    call Heat_con_mod(Q_cr, Q_desal, P_ratio, Q_cr_m)
    call Wat_con_mod(Q_cr_m, dt_cr, W_fr)
    Wat_drift = 0.0002 * W_fr
end subroutine Nat_draft_drift


!--------------------------------------------------------------------------------------------------


! 'model tab' - Wet cooling tower bleed-off/blowdown (Fb)
! Water deliberately expelled from the water cycle to remove impurities -> TESTED
! Prerequests: Nat_draft_evap()
!              Nat_draft_drift()
subroutine Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_bleed)
    implicit none
    double precision, intent(inout) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, &
    T_desal, Desal_cap, Desal_rate, Con_cycle
    double precision, intent(out) :: Wat_bleed
    double precision Wat_evap, Wat_evap_rate, Wat_drift
    call Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    call Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    Wat_bleed = (Wat_evap_rate - ((Con_cycle - 1) * Wat_drift)) / (Con_cycle - 1)
end subroutine Nat_draft_bleed


!--------------------------------------------------------------------------------------------------


! 'model tab' - Wet cooling tower make up (Fmu)
! Water required to replinish the water lost -> TESTED
! Prerequests: Nat_draft_evap()
!              Nat_draft_drift()
!              Nat_draft_bleed()
subroutine Nat_draft_makeup(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_mkup)
    implicit none
    double precision, intent(inout) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, &
    T_desal, Desal_cap, Desal_rate, Con_cycle
    double precision, intent(out) :: Wat_mkup
    double precision Wat_evap, Wat_evap_rate, Wat_drift, Wat_bleed
    call Nat_draft_evap(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_evap, Wat_evap_rate)
    call Nat_draft_drift(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Wat_drift)
    call Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_bleed)
    Wat_mkup = Wat_evap_rate + Wat_drift + Wat_bleed
end subroutine Nat_draft_makeup


!--------------------------------------------------------------------------------------------------


! 'model tab' - Wet cooling tower water consumption
! The water consumed by the cooling tower (excluding blowdown)-> 
subroutine Nat_draft_consump(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_consump)
    implicit none
    double precision, intent(inout) :: t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, &
    T_desal, Desal_cap, Desal_rate, Con_cycle
    double precision, intent(out) :: Wat_consump
    double precision Wat_bleed, Wat_mkup
    call Nat_draft_bleed(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_bleed)
    call Nat_draft_makeup(t_db, c_type, t_sw, dt_ta, dt_cr, dt_ca, r_h, P_elec, Eta, T_desal, &
    Desal_cap, Desal_rate, Con_cycle, Wat_mkup)
    Wat_consump = Wat_mkup - Wat_bleed
end subroutine Nat_draft_consump


!--------------------------------------------------------------------------------------------------


! 'report tab' - Excavation
! The water used during construction for excavation
subroutine Excav_water(P_elec, Wat_excav)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_excav
    Wat_excav = (50 * P_elec) - 30000
end subroutine Excav_water


!--------------------------------------------------------------------------------------------------


! Converting from m3/s to m3/MWh -> TESTED
subroutine Wat_conv(Wat_m3, P_elec, Wat_MWh)
    implicit none
    double precision, intent(in) :: Wat_m3, P_elec
    double precision, intent(out) :: Wat_MWh
    double precision Hr
    Hr = 3600
    Wat_MWh = (Wat_m3 * Hr) / P_elec
end subroutine Wat_conv


!--------------------------------------------------------------------------------------------------


! 'report tab' - Concrete mixing
! The water needed for making the concrete for construction
subroutine Concrete_water(P_elec, Wat_concrt)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_concrt
    Wat_concrt = (83.33 * P_elec) + 3333
    Wat_concrt = nint(Wat_concrt / 100) * 100
end subroutine Concrete_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Supply for construction staff
! The water used by the construction staff during construction of the power plant
subroutine Constr_water(P_elec, Wat_constr)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_constr
    Wat_constr = (500 * P_elec) - 100000
    Wat_constr = nint(Wat_constr / 100) * 100
end subroutine Constr_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Flushing , cleaning etc
! The water for 'domestic' use during commissioning of the power plant
subroutine domestic_water(P_elec, Wat_dom)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_dom
    Wat_dom = ((16.67 * P_elec) + 6666.6)    
    Wat_dom = nint(Wat_dom / 100) * 100
end subroutine domestic_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Drinking water
! The water drunk by workers during commissioning of the power planty
subroutine Drink_water(Wat_constr, Wat_drink)
    implicit none
    double precision, intent(in) :: Wat_constr
    double precision, intent(out) :: Wat_drink
    Wat_drink = (300.0 / 6000.0) * Wat_constr
end subroutine Drink_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Makeup for Secondary loop
! The water loss from the secondary loop; leaks etc
subroutine Sec_makeup(P_elec, Wat_mkup_2)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_mkup_2
    Wat_mkup_2 = 6341 * ((P_elec / 1900)**0.8)
end subroutine Sec_makeup



!--------------------------------------------------------------------------------------------------


! 'report tab' - Makeup for primary loop
! The water loss from the primary loop; leaks etc
subroutine Prim_makeup(P_elec, Wat_mkup_1)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_mkup_1
    Wat_mkup_1 = 220 * ((P_elec / 1900)**0.85)
end subroutine Prim_makeup


!--------------------------------------------------------------------------------------------------


! 'report tab' - Waste Treatment
! The water required to treat waste
subroutine Waste_water(P_elec, Wat_wst)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_wst
    Wat_wst = 2542 * ((P_elec / 1900)**0.7)    
end subroutine Waste_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Condesate Polishing Plant
! The water required for the polishing plant
subroutine Polish_water(P_elec, Wat_pol)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_pol
    Wat_pol = 1838 * ((P_elec / 1900)**0.8)    
end subroutine Polish_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Component cooling water make up
! The water required for component cooling
subroutine Comp_water(P_elec, Wat_com)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_com
    Wat_com = 517 * ((P_elec / 1900)**0.66)    
end subroutine Comp_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Fire protection
! The water stored in case of fire
subroutine Fire_water(P_elec, Wat_fr)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_fr
    Wat_fr = 419 * ((P_elec / 1900) * 0.92)
    ! Should the equation instead be: Wat_fr = 419 * ((P_elec / 1900)**0.92)   
end subroutine Fire_water


!--------------------------------------------------------------------------------------------------


! 'report tab' - Sanitary and potable
! The amount of water stored for domestic use
subroutine Potable_water(P_elec, Wat_pot)
    implicit none
    double precision, intent(in) :: P_elec
    double precision, intent(out) :: Wat_pot
    Wat_pot = 4167 * ((P_elec / 1900)**0.9)    
end subroutine Potable_water


!--------------------------------------------------------------------------------------------------


! 'Model' tab - Temperature after mixing (Tmix)
! Water temperature in the river after mixing with waaste water from power plant -> TESTED
! Prerequest: Wat_con_mod()
subroutine River_mix_temp(Q_cr_m, Dt_cr, Flow_riv, t_sw, Mix_riv)
    implicit none
    double precision, intent(in) :: Q_cr_m, Flow_riv, Dt_cr, t_sw
    double precision, intent(out) :: Mix_riv
    double precision W_fr
    ! Q_cr_m = Desalination modulated condenser reject heat
    ! Flow_riv = River water flow volume m3/s
    ! dt_cr = Condenser range temperature
    ! t_sw = Surface water temperature of open water source
    ! Mix_riv = River water mixed with power plant output temperature
    call Wat_con_mod(Q_cr_m, dt_cr, W_fr)
    Mix_riv = ((W_fr / Flow_riv) * Dt_cr) + t_sw
end subroutine River_mix_temp


!--------------------------------------------------------------------------------------------------


! HeatCoeffEvap()
! Estimation of Heat coefficient of water evaporation (kW/m2 K) according to TRS 155 -> TESTED
subroutine Heat_coeff_wat(Temp, Spd_wind, Q_coeff_wat)
    implicit none
    double precision, intent(in) :: Temp, Spd_wind
    double precision, intent(out) :: Q_coeff_wat
    ! Temp = Temperature
    ! Spd_wind = wind speed
    ! Q_coeff_wat = Heat coefficient of evaporation water (kW/m2 K)
    Q_coeff_wat = (2.615 + 1.55 * Spd_wind) * Exp(0.05 * Temp)
end subroutine Heat_coeff_wat


!--------------------------------------------------------------------------------------------------


! HeatCoeff()
! Estimation of Heat coefficient (kW/m2 K) according to TRS 155 -> TESTED
subroutine Heat_coeff(Temp, Spd_wind, Q_coeff)
    implicit none
    double precision, intent(in) :: Temp, Spd_wind
    double precision, intent(out) :: Q_coeff
    double precision Q_coeff_wat, Conv, Rad
    ! Temp = Temperature
    ! Spd_wind = wind speed
    ! Q_coeff = Heat coefficient (kW/m2 K)
    ! Conv = convection
    ! Rad = Radiation
    call Heat_coeff_wat(Temp, Spd_wind, Q_coeff_wat)
    Conv = 3.003 + 1.792 * Spd_wind
    Rad = 4.6 + 0.0484 * Temp
    Q_coeff = (Rad + Q_coeff_wat + Conv)
end subroutine Heat_coeff


!--------------------------------------------------------------------------------------------------


! 'Model' tab - Evaporation (Evriver)
! The evaporation of water from the river after power plant water output -> TESTED
subroutine River_evap(Q_cr, Q_desal, P_ratio, Temp, Spd_wind, Evap_riv)
    implicit none
    double precision, intent(in) :: Q_cr, Q_desal, P_ratio, Spd_wind
    double precision, intent(inout) :: Temp
    double precision, intent(out) :: Evap_riv
    double precision Q_cr_m, Q_lat, Q_coeff_wat, Q_coeff
    call Heat_con_mod(Q_cr, Q_desal, P_ratio, Q_cr_m)
    call Heat_lat(Temp, Q_lat)
    call Heat_coeff_wat(Temp, Spd_wind, Q_coeff_wat)
    call Heat_coeff(Temp, Spd_wind, Q_coeff)
    Evap_riv = (Q_cr_m / Q_lat) * (Q_coeff_wat / Q_coeff)
end subroutine River_evap


!--------------------------------------------------------------------------------------------------


! 'Model' tab - Temperature increment in pond (Dtpond)
! The change in temperature of the cooling pond
SUBROUTINE pond_dtemp(Dt_ca, Dt_cr, dt_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: Dt_ca, Dt_cr
    DOUBLE PRECISION, INTENT(OUT) :: dt_pond
    ! dt_cr = Condenser range temperature
    ! dt_ca = Condenser approach temperature
    ! t_pond = Cooling pond temperature change
    dt_pond = Dt_cr / log(1 + Dt_cr / Dt_ca)
END SUBROUTINE pond_dtemp


!--------------------------------------------------------------------------------------------------


! 'Model' tab - Average water temperature in pond (Tpond)
! The average pond temperature as defined by the air temperature
SUBROUTINE pond_temp(Dt_ca, Dt_cr, t_db, t_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: Dt_ca, Dt_cr, t_db
    DOUBLE PRECISION, INTENT(OUT) :: t_pond
    DOUBLE PRECISION dt_pond
    ! dt_cr = Condenser range temperature
    ! dt_ca = Condenser approach temperature
    ! dt_pond = Cooling pond temperature change
    ! t_pond = Cooling pond temperature
    CALL pond_dtemp(Dt_ca, Dt_cr, dt_pond)
    t_pond = t_db + dt_pond
END SUBROUTINE pond_temp


!--------------------------------------------------------------------------------------------------


! 'Model' tab - Surface needed (Spond)
! The surface area of the cooling pond required to sufficiently cool the reactor
SUBROUTINE pond_area(Q_cr_m, dt_cr, Temp, Spd_wind, km3_pond)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: Q_cr_m, dt_cr, Temp, Spd_wind
    DOUBLE PRECISION, INTENT(OUT) :: km3_pond
    DOUBLE PRECISION W_fr, Q_coeff
    ! dt_cr = Condenser range temperature
    ! dt_ca = Condenser approach temperature
    ! dt_pond = Cooling pond temperature change
    ! t_pond = Coolinf pond temperature
    CALL Wat_con_mod(Q_cr_m, dt_cr, W_fr)
    CALL Heat_coeff(Temp, Spd_wind, Q_coeff)
    km3_pond = (Fmcc * 1000) * 4.187/(@HeatCoeff(Tpond,uwind)/1000)*LN(1+Dtcr/(Dtcr))*0.000001
END SUBROUTINE pond_area


!--------------------------------------------------------------------------------------------------


! 'Report' tab - NPP system inventories choice
! Using water type and water source determines what values to retrieve from sys_inven() -> TESTED
subroutine sys_inven_ch(Wat_source, Wat_type, choice)
    implicit none
    integer, intent(in) :: Wat_type
    integer, intent(inout) :: Wat_source
    integer, intent(out) :: choice
    if (Wat_source == 2) then
        Wat_source = 0
    end if
    choice = Wat_type + Wat_source
end subroutine sys_inven_ch


!--------------------------------------------------------------------------------------------------


! 'sheet 2' - NPP system inventories
! A reference subroutine for the water inventories
subroutine sys_inven(array)
    implicit none
    double precision, dimension(4,14), intent(out) :: array
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
end subroutine sys_inven


!--------------------------------------------------------------------------------------------------


! Outputting values -> TESTED
subroutine Output(Output_1)
    implicit none
    double precision, intent(in) :: Output_1
    if (Output_1 < 1) then
        if (Output_1 < -1) then
            write(*,'(f0.2)') Output_1
        else
            write(*,'(f4.2)') Output_1
        end if
    else
        write(*,'(f0.2)') Output_1
    end if
    return
end subroutine Output


!--------------------------------------------------------------------------------------------------


! https://www.math.fsu.edu/~dmandel/Fortran/ReadingArrayData.pdf
! Outputting a value form an array
subroutine Print_array(array, m, n)
    implicit none
    double precision, intent(in) :: array(n,m)
    integer, intent(in) :: m, n
    integer i
    do i = 1, n
        write(*,'(f0.2)') array(i,:)
    end do
end subroutine Print_array


!--------------------------------------------------------------------------------------------------