$setglobal Results .\Results\

* set "is_seasonal_year" to "no" if seasonal year (t1 - t8760) and to "yes" if different
$if not set is_seasonal_year $setglobal is_seasonal_year yes


$if %is_seasonal_year% =="yes"    $setglobal d_start        100
$if %is_seasonal_year% =="yes"    $setglobal d_end          99


$if %is_seasonal_year% =="no"     $setglobal d_start        1
$if %is_seasonal_year% =="no"     $setglobal d_end          365

$if %is_seasonal_year% =="yes"    $setglobal t_start        2377
$if %is_seasonal_year% =="yes"    $setglobal t_end          2376


$if %is_seasonal_year% =="no"     $setglobal t_start        1
$if %is_seasonal_year% =="no"     $setglobal t_end          8760

Sets

    a   archetypes
    h   heating system solutions
    pv  pv technology
    b   battery storage technologies
    s   seasonal thermal energy storage system solution


    full_scen(full_scen)  all possible technology scenarios
    map_scenarios(full_scen,a,h,pv,b,s)  all possible scenarios

*************************************************************************************************************
$if %is_seasonal_year% =="yes"
Set all_d /d100*d365,d1*d99/
*$if %is_seasonal_year% =="yes" $setglobal is_fullyear   no
*$if %is_seasonal_year% =="yes" $if %d_end%=="t99"  $setglobal is_fullyear    yes
*$if %is_seasonal_year% =="yes" $if %is_fullyear%=="no"
*Set d(all_d) /d%d_start%*d%d_end%/;
*$if %is_not_normal_year% =="yes" $if %is_fullyear%=="yes"
$if %is_seasonal_year% =="yes"
Set d(all_d) /d%d_start%*d365,d1*d%d_end%/

$if %is_seasonal_year% =="no"
Set all_d /d1*d365/;
$if %is_seasonal_year% =="no"
Set d(all_d) /d%d_start%*d%d_end%/

**************************************************************************************************************

$if %is_seasonal_year% =="yes"
Set all_t /t2377*t8760,t1*t2376/
*$if %is_seasonal_year% =="yes" $setglobal is_fullyear   no
*$if %is_seasonal_year% =="yes" $if %t_end%=="t2376"  $setglobal is_fullyear    yes
*$if %is_seasonal_year% =="yes" $if %is_fullyear%=="no"
*Set t(all_t) /t%t_start%*t%t_end%/;
*$if %is_not_normal_year% =="yes" $if %is_fullyear%=="yes"
$if %is_seasonal_year% =="yes"
Set t(all_t) /t%t_start%*t8760,t1*t%t_end%/

$if %is_seasonal_year% =="no"
Set all_t /t1*t8760/
$if %is_seasonal_year% =="no"
Set t(all_t) /t%t_start%*t%t_end%/

*     d(all_d)                                time periods used in the model
*                                        /d%d_start% * d%d_end%/



*    t(all_t)                                time periods used in the model
*                                             /t%t_start% * t%t_end%/
**************************************************************************************************************
 Set   td(all_t,all_d)                         map hours of year to days of year and hours of day
**************************************************************************************************************
*    tfirst(t)                              first period

*    tlast(t)                                last period

*$if %is_seasonal_year% =="yes" $if %is_fullyear%=="no"  Set t(all_t) /t%t_start%*t%t_end%/;

*$if %is_hydrological_year% =="yes" $if %is_fullyear%=="yes" Set t(all_t) /t%t_start%*t8760,t1*t%t_end%/;
;



Parameters

* Data tables

    pv_par          parameters of the PV system

    pv_var_par      parameters to constrain PV capacity

    batt_par        parameters of the batteries

    heat_par        parameters of the heating technologies

    stes_par        parameters of the stes

    dhw_par         parameters of the DHW system

    grid_par        grid power transmission maximum capacity (kW)

    price_el        electricity prices (CHF per kWh_el)

    a_dem_el        electricity demand per archetype (kWh_el)

*    heat_dem_el     electricity heating demand per archetype (kWh_el)

    heat_dem_th     thermal heating demand per archetype (kWh_th)

    cop_aw35_par    cop for a-w heat pump at 35°C

    cop_aw60_par    cop for a-w heat pump at 60°C

    dhw_cen_par_el      dhw energy demand for centralized systems (kWh_el)

    dhw_dec_par_th      dhw energy demand for decentralized systems (kWh_th)

    pv_cf           solar capacity factor for Switzerland

*PV system

    pv_pel          Capacity of the PV system (kW)

    roof_era         total PV roof surface area available for a given archetype (sqm)

    pv_pel_max_m2    ratio of power per square meter of PV panels (kW per sqm)

    pv_max_cap      maximum roof PV capacity potential for a given archetype (kW)

    pv_cf_prof      capacity factor of PV generation per canton (%)

    pv_eac_capex    equivalent annual capital cost of PV (CHF)

    pv_eac_opex     equivalent annual operational cost of PV (CHF)

*Electricity storage system

    bat_soc_min    minimum level of charge of the ESS (%)

*    bat_disch_max  maximum supply rate of the ESS (kWh per hour)

*    bat_ch_max     maximum charging rate of the ESS (kWh per hour)

    bat_eff        round-trip efficiency of the ESS (%)

    bat_en_pwr_rat    energy to power ratio

    bat_st_cap    Capacity of the ESS (kWh)

*    bat_max_cap    maximum battery capacity potential for a given archetype (kWh)

*    bat_pw_tr      Power transmission capacity of the ESS (kW)

    bat_eac_capex equivalent annual capital cost of batteries (CHF)

    bat_eac_opex equivalent annual operational cost of batteries (CHF)

*Heating & DHW storage system

    th_pel_hp      Maximum electric power of the heating device (kW)

    th_efficiency  Efficiency of the heating system (electricity to heat)

    dhw_dec_soc_min    Minimum level of charge of the DHW storage system (%)

    dhw_dec_eff        Round-trip efficiency of the DHW(%)

    dhw_dec_pel        Maximum electric power of the DHW system (kW)

    dhw_dec_st_cap     Capacity of the DHW storage system (kWh)

*Seasonal storage (STES)

    th_stes_volume    Volume of STES (l)

    th_stes_relativedailyloss  Daily self discharge per day per K temperature difference (kWh per K)

    th_stes_placedinsidehouse  True if it's placed inside house

    th_stes_ambienttemperature Ambient temperature of the storage. If placed inside house: 20°C

    stes_loss_tot_e1_th Total possible losses from the STES system (kWh_th)

    stes_loss_tot_e2_th Total possible losses from the STES system (kWh_th)

    stes_loss_tot_e3_th Total possible losses from the STES system (kWh_th)

    th_stes_tmax   Maximum storage temperature (usually 90°C)

    th_stes_dtmax   Maximum temperature spread between storage temperature and ambient temperature

    th_stes_emaxtot Total storage capacity with dT=70K

    th_stes_emax1 Total storage capacity with dT=15K (21.4% of emaxtot)

    th_stes_emax2  Total storage capacity with dT=25K (35.7% of emaxtot)

    th_stes_emax3  Total storage capacity with dT=30K (42.8% of emaxtot)

    th_stes_chargeefficiency3 Efficiency of the electric resistor heaters in the STES for charging up to 90°C

    th_stes_pmaxcharge3  Maximum power to charge energy band e3

    th_stes_socmin  Minimum SOC before energy can be extracted

    stes_eac_capex equivalent annual capital cost of the STES (CHF)

*Grid
    grid_max_dem    Demand limit from the grid (kWh per hour)

    grid_max_sup    Supply limit to the grid (kWh per hour)

    price_grid       Price if energy is taken from the grid electricity (CHF per kWh)

    price_supply     Price if energy is supplied to the grid (CHF per kWh)

*Demand and COP profiles

    elec_dem         Electricity demand of the archetype (kWh_el)

    heat_dem_d_th    Daily heating demand of the archetype (kWh_th)

*    heat_dem_d_el    Daily  demand of the archetype (kWh_el)

    dhw_dec_dem_th   Decentralized domestic hot water demand (kWh_th)

    dhw_cen_dem_el   Centralized domestic hot water demand (kWh_el)

    cop_aw35_t       Coefficient of performance air-water heat pump at 35 °C operation

    cop_aw60_t       Coefficient of performance air-water heat pump at 60 °C operation

*Initial charge levels per storage technology

*    bat_level_init   Initial charge level of the ESS (kWh_el)

*    stes_e1_lev_th_init Initial charge level of the stes' e1 storage unit (kWh_th)

*    stes_e2_lev_th_init Initial charge level of the stes' e2 storage unit (kWh_th)

*    stes_e3_lev_th_init Initial charge level of the stes' e3 storage unit (kWh_th)

;

$onUNDF

*$gdxin Data/base_data_t8760

*$gdxin Data/stes_data_t8760_50test

*$gdxin Data/base_data_t48

*$gdxin Data/stes_data_t48

*$gdxin Data/stes_test_10hh_mfh

*$gdxin Data/optim_test_full_run_seas0

$gdxin Data/2050_rcp26_Low_100_test

$loaddc a h pv b s full_scen map_scenarios
$loaddc pv_par pv_var_par batt_par heat_par stes_par dhw_par grid_par price_el a_dem_el heat_dem_th cop_aw35_par cop_aw60_par dhw_cen_par_el dhw_dec_par_th pv_cf

$offUNDF

$onUNDF
$call "gdxxrw data/timemaps.xlsx o=data/timemaps.gdx cmerge=1 squeeze=N MaxDupeErrors=1000000 Index=index!A1"
$gdxin data/timemaps
$loaddc td
$offUNDF
;

$setglobal scenario      full_scen
$include V13_Call_STES_opt.gms

File results;

*Set run_only_scen(full_scen) /"1067554_100071_PV0_BAT0_100111"/;

Parameter
total_costs(full_scen)
report_statistics(full_scen,*)
report_battery(full_scen,t,*)
report_dhw(full_scen,t,*)
report_stes_lev(full_scen,t,*)
report_stes_soc(full_scen,t,*)
report_stes_e1(full_scen,t,*)
report_stes_e2(full_scen,t,*)
report_stes_e3(full_scen,t,*)
report_heat_el(full_scen,t,*)
report_loss_stes(full_scen,t,*)
report_heat_th(full_scen,t,*)
report_grid_demand(full_scen,t,*)
report_stes_tot(full_scen,*)
report_pv_pel(full_scen,*)
report_bat_st_cap(full_scen,*)
report_grid_tot(full_scen,*)
report_grimsel(full_scen,t,*)
report_pvshare(full_scen,*)
report_elec_cost(full_scen,*)
report_syst_cost(full_scen,*)
;

*Loop(map_scenarios(full_scen,a,h,pv,b,s) $run_only_scen(full_scen),
Loop(map_scenarios(full_scen,a,h,pv,b,s),

*PV system

*price_grid(t) = price_el(t,"price_grid");

*     pv_pel = pv_par(pv,"pv_pel");
*     pv_pel = 10;

     roof_era = pv_var_par(a,"roof_era");

     pv_pel_max_m2 = pv_var_par(a,"pv_pel_max_m2");

     pv_eac_capex = pv_var_par(a,"eac_capex_kWp_CHF");

     pv_eac_opex = pv_var_par(a,"eac_opex_kWp_CHF");

     pv_max_cap = roof_era*pv_pel_max_m2;

*     pv_pel = roof_era*pv_pel_max_m2;

     pv_pel = 0;

     pv_cf_prof(t) = pv_cf(t,a);

*Electricity storage system

     bat_soc_min = batt_par(b,"bat_soc_min");

*     bat_disch_max = batt_par(b,"bat_disch_max");

*     bat_ch_max = batt_par(b,"bat_ch_max");

     bat_eff = batt_par(b,"bat_eff");

     bat_en_pwr_rat = batt_par(b,"bat_en_pwr_rat");

*     bat_eff = 1;

*     bat_st_cap = batt_par(b,"bat_st_cap");

     bat_st_cap = 0;

*     bat_max_cap = batt_par(b,"bat_max_cap");

*     bat_max_cap = 10;

*     bat_pw_tr = batt_par(b,"bat_pw_tr");

     bat_eac_capex = batt_par(b,"eac_capex_kWh_CHF");

     bat_eac_opex = batt_par(b,"eac_opex_kWh_CHF");

*Heating/storage system

     th_pel_hp = heat_par(h,"th_pel_hp");

     th_efficiency = heat_par(h,"th_efficiency");

     dhw_dec_soc_min = dhw_par(a,"dhw_soc_min");

     dhw_dec_eff = dhw_par(a,"dhw_eff");

     dhw_dec_pel = dhw_par(a,"dhw_pel");
*     dhw_pel = 0;

     dhw_dec_st_cap = dhw_par(a,"dhw_st_cap");
*     dhw_st_cap = 0;

*STES system
    th_stes_volume = stes_par(s,"th_stes_volume");

    th_stes_relativedailyloss = stes_par(s,"th_stes_relativedailyloss");

    th_stes_placedinsidehouse = stes_par(s,"th_stes_placedinsidehouse");

    th_stes_ambienttemperature = stes_par(s,"th_stes_ambienttemperature");

    stes_loss_tot_e1_th = stes_par(s,"stes_loss_tot_e1_th");

    stes_loss_tot_e2_th = stes_par(s,"stes_loss_tot_e2_th");

    stes_loss_tot_e3_th = stes_par(s,"stes_loss_tot_e3_th");

    th_stes_tmax= stes_par(s,"th_stes_tmax");

    th_stes_dtmax = stes_par(s,"th_stes_dtmax");

    th_stes_emaxtot= stes_par(s,"th_stes_emaxtot");

    th_stes_emax1= stes_par(s,"th_stes_emax1");

    th_stes_emax2 = stes_par(s,"th_stes_emax2");

    th_stes_emax3 = stes_par(s,"th_stes_emax3");

    th_stes_chargeefficiency3 = stes_par(s,"th_stes_chargeefficiency3");

    th_stes_pmaxcharge3= stes_par(s,"th_stes_pmaxcharge3");

    th_stes_socmin= stes_par(s,"th_stes_socmin");

    stes_eac_capex = stes_par(s,"stes_eac_capex");


*Grid
*     grid_max_dem = grid_par(a,"grid_max_dem");

*     grid_max_sup = grid_par(a,"grid_max_sup");

     grid_max_dem = 1500;

     grid_max_sup = 1000;

*     price_grid(t) = price_el(t,a);

      price_grid(t) = 0.3078;

*     price_supply(t) = price_el(t,"price_supply");

     price_supply(t) = 0.088;

*Demand profiles

    elec_dem(t) = a_dem_el(t,a);

    heat_dem_d_th(d) =  heat_dem_th(d,a,h);

*    heat_dem_d_el(d) = heat_dem_el(d,a,h);

*    dhw_dec_dem_th(t) = 0;

*    dhw_cen_dem_el(t) = 0;

    dhw_dec_dem_th(t) = dhw_dec_par_th(t,a,h);

    dhw_cen_dem_el(t) = dhw_cen_par_el(t,a,h);

*COP parameters

    cop_aw35_t(t) = cop_aw35_par(t,a);

    cop_aw60_t(t) = cop_aw60_par(t,a);

**************************************************************************************
*Initial and final levels of charge for battery and stes
*************************************************
*Initial battery level value

*    bat_level_init = bat_st_cap*0;

*Final battery level value

*    bat_level.lo(t)$(ord(t) eq card(t)) = bat_level_init;

**************************************************

*Initial stes level values

*    stes_e1_lev_th_init = th_stes_emax1*0;

*    stes_e2_lev_th_init = th_stes_emax2*0;

*    stes_e3_lev_th_init = th_stes_emax3*0;

*Final stes level values

*    stes_e1_lev_th.lo(t)$(ord(t) eq card(t)) = stes_e1_lev_th_init;

*    stes_e2_lev_th.lo(t)$(ord(t) eq card(t)) = stes_e2_lev_th_init;

*    stes_e3_lev_th.lo(t)$(ord(t) eq card(t)) = stes_e3_lev_th_init;

**************************************************************************************

Solve costs using mip minimizing syst_elec_cost;

Display CostElec.l, CostSystElecTot.l, GridDemTot.m;


* Storing the results of each scenario:
     total_costs(full_scen) = elec_cost.l;
     report_statistics(full_scen,"Solve: ModelStat") = costs.modelStat;
     report_statistics(full_scen,"Solve: SovelStat (should be 1)") = costs.solveStat;

*     report_battery(full_scen,t,"bat_ch") = bat_ch.l(t) + eps;
*     report_battery(full_scen,t,"bat_disch") = - bat_disch.l(t) + eps;
*     report_battery(full_scen,t,"bat_level") = bat_level.l(t) + eps;
*     report_battery(full_scen,t,"pv_sup") = - pv_sup.l(t) + eps;

*     report_dhw(full_scen,t,"dhw_ch_th") = dhw_ch_th.l(t) + eps;
*     report_dhw(full_scen,t,"dhw_dec_dem_th") = - dhw_dec_dem_th(t) + eps;
*     report_dhw(full_scen,t,"dhw_lev_th") = dhw_lev_th.l(t) + eps;
*     report_dhw(full_scen,t,"pv_sup") = - pv_sup.l(t) + eps;

     report_stes_lev(full_scen,t,"stes_e1_lev_th") = stes_e1_lev_th.l(t) + eps;
     report_stes_lev(full_scen,t,"stes_e2_lev_th") = stes_e2_lev_th.l(t) + eps;
     report_stes_lev(full_scen,t,"stes_e3_lev_th") = stes_e3_lev_th.l(t) + eps;
     report_stes_lev(full_scen,t,"heat_dem_t_th") = heat_dem_t_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_ch_e1_th") = stes_ch_e1_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_ch_e2_th") = stes_ch_e2_th.l(t) + eps;
*    report_stes_lev(full_scen,t,"stes_ch_e3_th") = stes_ch_e3_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_disch_e1_th") = - stes_disch_e1_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_disch_e2_th") = - stes_disch_e2_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_disch_e3_th") = - stes_disch_e3_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_loss_e1_th") = stes_loss_e1_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_loss_e2_th") = stes_loss_e2_th.l(t) + eps;
*     report_stes_lev(full_scen,t,"stes_loss_e3_th") = stes_loss_e3_th.l(t) + eps;


*     report_stes_e1(full_scen,t,"stes_e1_lev_th") = stes_e1_lev_th.l(t) + eps;
*     report_stes_e1(full_scen,t,"pv_sup") = - pv_sup.l(t) + eps;
*     report_stes_e1(full_scen,t,"stes_ch_e1_th") = stes_ch_e1_th.l(t) + eps;
*     report_stes_e1(full_scen,t,"stes_disch_e1_th") = - stes_disch_e1_th.l(t) + eps;
*     report_stes_e1(full_scen,t,"stes_loss_e1_th") = - stes_loss_e1_th.l(t) + eps;
*     report_stes_e1(full_scen,t,"heat_dem_t_th") = heat_dem_t_th.l(t) + eps;

*     report_stes_e2(full_scen,t,"stes_e2_lev_th") = stes_e2_lev_th.l(t) + eps;
*     report_stes_e2(full_scen,t,"stes_ch_e2_th") = stes_ch_e2_th.l(t) + eps;
*     report_stes_e2(full_scen,t,"stes_disch_e2_th") = - stes_disch_e2_th.l(t) + eps;
*     report_stes_e2(full_scen,t,"stes_loss_e2_th") = - stes_loss_e2_th.l(t) + eps;
*     report_stes_e2(full_scen,t,"heat_dem_t_th") = heat_dem_t_th.l(t) + eps;

*     report_stes_e3(full_scen,t,"stes_e3_lev_th") = stes_e3_lev_th.l(t) + eps;
*     report_stes_e3(full_scen,t,"stes_ch_e3_th") = stes_ch_e3_th.l(t) + eps;
*     report_stes_e3(full_scen,t,"stes_disch_e3_th") = - stes_disch_e3_th.l(t) + eps;
*     report_stes_e3(full_scen,t,"stes_loss_e3_th") = - stes_loss_e3_th.l(t) + eps;
*     report_stes_e3(full_scen,t,"heat_dem_t_th") = heat_dem_t_th.l(t) + eps;


     report_stes_soc(full_scen,t,"stes_soc_1") = stes_soc_1.l(t) + eps;
     report_stes_soc(full_scen,t,"stes_soc_2") = stes_soc_2.l(t) + eps;
     report_stes_soc(full_scen,t,"stes_soc_3") = stes_soc_3.l(t) + eps;
     report_stes_soc(full_scen,t,"stes_soc_tot") = stes_soc_tot.l(t) + eps;

     report_heat_el(full_scen,t,"hp_tot_el") = hp_tot_el.l(t) + eps;
     report_heat_el(full_scen,t,"heat_hp_el") = heat_hp_el.l(t) + eps;
     report_heat_el(full_scen,t,"stes_ch_tot_hp_el") = stes_ch_tot_hp_el.l(t) + eps;
     report_heat_el(full_scen,t,"stes_ch_e3_el") = stes_ch_e3_el.l(t) + eps;
     report_heat_el(full_scen,t,"dhw_cen_dem_el") = dhw_cen_dem_el(t) + eps;
     report_heat_el(full_scen,t,"heat_dem_t_th") = heat_dem_t_th.l(t) + eps;


*     report_loss_stes(full_scen,t,"stes_loss_e1_th") =  stes_loss_e1_th.l(t) + eps;
*     report_loss_stes(full_scen,t,"stes_loss_e2_th") = stes_loss_e2_th.l(t) + eps;
*     report_loss_stes(full_scen,t,"stes_loss_e3_th") = stes_loss_e3_th.l(t) + eps;

     report_heat_th(full_scen,t,"heat_hp_th") = - heat_hp_th.l(t) + eps;
     report_heat_th(full_scen,t,"stes_ch_e1_th") = stes_ch_e1_th.l(t) + eps;
     report_heat_th(full_scen,t,"stes_disch_e1_th") = - stes_disch_e1_th.l(t) + eps;
     report_heat_th(full_scen,t,"stes_ch_e2_th") = stes_ch_e2_th.l(t) + eps;
     report_heat_th(full_scen,t,"stes_disch_e2_th") = - stes_disch_e2_th.l(t) + eps;
     report_heat_th(full_scen,t,"stes_ch_e3_th") = stes_ch_e3_th.l(t) + eps;
     report_heat_th(full_scen,t,"stes_disch_e3_th") = - stes_disch_e3_th.l(t) + eps;
*     report_heat_th(full_scen,t,"stes_ch_tot_hp_el") =  stes_ch_tot_hp_el.l(t) + eps;
*     report_heat_th(full_scen,t,"stes_disch_tot_th") = - stes_disch_tot_th.l(t) + eps;
     report_heat_th(full_scen,t,"heat_dem_t_th") = heat_dem_t_th.l(t) + eps;


     report_grid_demand(full_scen,t,"elec_dem") = elec_dem(t);
     report_grid_demand(full_scen,t,"bat_ch") = (bat_ch.l(t) + eps);
     report_grid_demand(full_scen,t,"stes_ch_tot_hp_el") = stes_ch_tot_hp_el.l(t) + eps;
     report_grid_demand(full_scen,t,"heat_hp_el") = heat_hp_el.l(t) + eps;
     report_grid_demand(full_scen,t,"dhw_cen_dem_el") = dhw_cen_dem_el(t) + eps;
     report_grid_demand(full_scen,t,"stes_ch_e3_el") = stes_ch_e3_el.l(t) + eps;
     report_grid_demand(full_scen,t,"dhw_dec_ch_el") = dhw_dec_ch_el.l(t) + eps;
     report_grid_demand(full_scen,t,"pv_sup") = - pv_sup.l(t) + eps;
     report_grid_demand(full_scen,t,"bat_disch") = - (bat_disch.l(t) + eps);
     report_grid_demand(full_scen,t,"grid_dem_tot") = grid_dem_tot.l(t) + eps;

     report_grimsel(full_scen,t,"elec_dem") = elec_dem(t);
     report_grimsel(full_scen,t,"bat_ch") = (bat_ch.l(t) + eps);
     report_grimsel(full_scen,t,"stes_ch_tot_hp_el") = stes_ch_tot_hp_el.l(t) + eps;
     report_grimsel(full_scen,t,"heat_hp_el") = heat_hp_el.l(t) + eps;
     report_grimsel(full_scen,t,"dhw_cen_dem_el") = dhw_cen_dem_el(t) + eps;
     report_grimsel(full_scen,t,"stes_ch_e3_el") = stes_ch_e3_el.l(t) + eps;
     report_grimsel(full_scen,t,"dhw_dec_ch_el") = dhw_dec_ch_el.l(t) + eps;
     report_grimsel(full_scen,t,"pv_sup") = - pv_sup.l(t) + eps;

     report_stes_tot(full_scen,"stes_yearly_disch__th")  = stes_yearly_disch_th.l +eps;
     report_stes_tot(full_scen,"stes_yearly_ch_th")  = stes_yearly_ch_th.l +eps;
     report_stes_tot(full_scen,"stes_yearly_ch_el")  = stes_yearly_ch_el.l +eps;
     report_stes_tot(full_scen,"hp_dir_yearly_th")  = hp_dir_yearly_th.l +eps;
     report_stes_tot(full_scen,"hp_dir_yearly_el")  = hp_dir_yearly_el.l +eps;

     report_pv_pel(full_scen,"pv_pel")  = pv_pel +eps;

     report_bat_st_cap(full_scen,"bat_st_cap")  = bat_st_cap +eps;

     report_pvshare(full_scen,"pv_share") = pv_share.l + eps;

     report_grid_tot(full_scen,"pv_prod_tot") = pv_prod_tot.l +eps;
     report_grid_tot(full_scen,"sum_grid_dem_tot") = sum_grid_dem_tot.l +eps;

     report_elec_cost(full_scen,"elec_cost")  = elec_cost.l+eps;
     report_elec_cost(full_scen,"pv_capex")  = pv_capex.l+eps;
     report_elec_cost(full_scen,"pv_opex")  = pv_opex.l+eps;
     report_elec_cost(full_scen,"bat_capex")  = bat_capex.l+eps;
     report_elec_cost(full_scen,"bat_opex")  = bat_opex.l+eps;

     report_syst_cost(full_scen,"syst_elec_cost")  = syst_elec_cost.l+eps;
);

* Send to "Results" directory

*execute_unload "%Results%base_t8760_results.gdx";

*execute_unload "%Results%stes_t8760_results.gdx";

*execute_unload "%Results%1223552_100071_base_t8760_results.gdx";

*execute_unload "%Results%1223552_100071_stes_t8760_results.gdx";

*execute_unload "%Results%50_test_PVmax_year_results.gdx";

execute_unload "%Results%2050_rcp26_Low_100_results_pvzero.gdx";

*execute_unload "%Results%base_t48_results.gdx";

*execute_unload "%Results%stes_t48_results.gdx";


*total_costs(full_scen)
*report_statistics(full_scen,*)
*report_demand(full_scen,*)
*report_cost(full_scen,*)




