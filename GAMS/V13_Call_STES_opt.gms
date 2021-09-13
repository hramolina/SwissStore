
 Positive Variables
*   The model should decide whether to store or supply energy in the ESS or HS
*    pv_pel      installed PV capacity (kW_el)
*    bat_st_cap  battery capacity (kWh_el)
* PV and electricitity storage system
    bat_ch(t)      demand of the ESS (charging) (kWh)
    bat_disch(t)   supply of the ESS (discharging) (kWh)
    pv_sup(t)     supply of the RE system (kWh)
    bat_level(t)   level of energy stored in the ESS (kWh)

*Heating system and DHW system

*    heat_dem_hr(a,h,t)   Electricity demand of the HS (kWh_el per hour)
    hp_tot_el(t)   total electricity supplied by the heat pump (kWh_el)
    heat_dem_t_th(t)   hourly thermal heating demand of the archetype (kWh_th)
    heat_hp_th(t)    thermal heating demand supplied by the heat pump (kWh_th)
    heat_hp_el(t)    electric heating demand supplied by the heat pump (kWh_el)

*Decentralized DHW system

    dhw_dec_ch_el(t)       demand of the Decentralized DHW storage (charging) (kWh_el)
    dhw_dec_ch_th(t)       demand of the HSS (charging) (kWh_th)
    dhw_dec_lev_th(t)      energy content in the Decentralized DHW storage (kWh_th)

*Seasonal storage

    stes_e1_lev_th(t)     energy content in energy band 1 (35°C) (kWh_th)
    stes_e2_lev_th(t)     energy content in energy band 2 (60°C) (kWh_th)
    stes_e3_lev_th(t)     energy content in energy band 3 (90°C) (kWh_th)
    stes_e1_min_th(t)     minimum level in the STES' e1 section

    stes_e1_balance_th(t) level of energy stored in the STES' e1 section (kWh_th)
    stes_e2_balance_th(t) level of energy stored in the STES' e2 section (kWh_th)
    stes_e3_balance_th(t) level of energy stored in the STES' e3 section (kWh_th)

    stes_soc_1(t)    state of charge of energy band 1  (%)
    stes_soc_2(t)    state of charge of energy band 1  (%)
    stes_soc_3(t)    state of charge of energy band 1  (%)
    stes_soc_tot(t)  overall state of charge (%)

    stes_ch_e1_th(t)      demand of the STES' e1 storage (charging) (kWh_th)
    stes_ch_e2_th(t)      demand of the STES' e2 storage (charging) (kWh_th)
    stes_ch_e3_th(t)      demand of the STES' e3 storage (charging) (kWh_th)

    stes_ch_e1_el(t)      demand of the STES' e1 storage (charging) (kWh_el)
    stes_ch_e2_el(t)      demand of the STES' e2 storage (charging) (kWh_el)
    stes_ch_e3_el(t)      demand of the STES' e3 storage (charging) (kWh_el)
    stes_ch_tot_hp_el(t)  demand of the STES' provided by the heat pump (kWh_el)

    stes_disch_e1_th(t)    supply of the STES' e1 storage (discharging) (kWh_th)
    stes_disch_e2_th(t)    supply of the STES' e2 storage (discharging) (kWh_th)
    stes_disch_e3_th(t)    supply of the STES' e3 storage (discharging) (kWh_th)
    stes_disch_tot_th(t)   total supply of the STES (discharging) (kWh_th)

    stes_loss_e1_th(t)   losses of the STES e1 storage (self-discharging) (kWh_th)
    stes_loss_e2_th(t)   losses of the STES e2 storage (self-discharging) (kWh_th)
    stes_loss_e3_th(t)   losses of the STES e3 storage (self-discharging) (kWh_th)

*Grid
    demand(t)  energy demanded from the grid
    supply(t)  energy provided to the grid
;

Variables

    grid_dem_tot(t)     demand to the grid (kWh)

    stes_yearly_disch_th  total yearly STES discharge (kWh_th)
    stes_yearly_ch_th     total yearly STES charge (kWh_th)
    stes_yearly_ch_el     total yearly STES charge (kWh_el)
    hp_dir_yearly_th      total yearly direct thermal heating provided by the heat pump (kWh_th)
    hp_dir_yearly_el      total yearly direct electric heating provided by the heat pump (kWh_el)
    pv_prod_tot           total PV production (kWh)
    sum_grid_dem_tot      total grid consumption (kWh)
    elec_cost             total electricity consumption costs for a given archetype (CHF)
    pv_capex              total PV capital costs for a given archetype (CHF)
    pv_opex               total PV operational costs for a given archetype (CHF)
    bat_capex             total battery capital costs for a given archetype (CHF)
    bat_opex              total battery operational costs for a given archetype (CHF)
    pv_share              share of total PV installed relative to available roof area (%)
    syst_elec_cost        total electricity system costs (CHF)

;
********************************************************************************************************************************************************************
Equations

********************************************************************************************************************************************************************
*Definition of the battery system equations

*PVMaxLim    PV max cap

*BatMaxLim(t)   Bat max cap

*Constraints

BatUpLim(t)    battery's upper level of charge (kWh_el)

BatLwLim(t)    battery's lower level of charge (kWh_el)

BatMaxDisch(t) maximum discharge capacity of the battery (kWh_el)

BatMaxCh(t)    maximum charge capacity of the battery (kWh_el)

*Equations

BatLevCh(t)    level of charge of the battery on period t (kWh_el)

********************************************************************************************************************************************************************
*Definition of the DHW system equations

*Constraints

DHWDecUpLim(t)    DHW's upper level of charge (kWh_th)

DHWDecLwLim(t)    DHW's lower level of charge (kWh_th)

DHWDecMaxPw(t)    DHW's maximum electric power capacity for any given t (kW_el)

*Equations

DHWDecElecToTh(t) DHW's electric energy consumption (kWh_el)

DHWDecLevCh(t)    DHW's level of charge on period t (kWh_th)

********************************************************************************************************************************************************************
*Definition of the Seasonal Thermal Energy Storage (STES) system equations
*************************************************************
*STES e1 level

*Constraints

STESUpLimE1(t)    upper energy storage limit of the STES' E1 level (kWh_th)

*Equations

STESElecToThE1(t) electric energy provided by the heat pump to STES' E1 level(kWh_el)

STESLevChE1(t)    level of charge of the E1 level on period t (kWh_th)

*************************************************************
*STES e2 level

*Constraints
STESUpLimE2(t)    upper energy storage limit of the STES' E2 level (kWh_th)

*Equations

STESElecToThE2(t) electric energy provided by the heat pump to STES' E2 level(kWh_el)

STESLevChE2(t)    level of charge of the E2 level on period t (kWh_th)
*************************************************************
*STES e3 level

*Constraints
STESUpLimE3(t)    upper energy storage limit of the STES' E3 level (kWh_th)

STESMaxPwE3(t)    maximum electric power of E3's resistance (kW)

*Equations

STESElecToThE3(t) electric energy provided by the heat pump to STES' E3 level(kWh_el)

STESLevChE3(t)    level of charge of the E3 level on period t (kWh_th)
*************************************************************
*State of charge (SOC) equations

*Constraints
STESUpSOCLim(t) maximum total state of charge (%)

*Equations
STESSOCe1(t)    e1's state of charge (%)

STESSOCe2(t)    e2's state of charge (%)

STESSOCe3(t)    e3's state of charge (%)

STESSOCtot(t)   total state of charge (%)
*************************************************************
*STES Charge

STESTotChHP(t) heat pump's total electricity demand (kWh_el)

*************************************************************
*STES Discharge

STESMaxDisch(t)   STES's maximum energy discharge (kWh_th)

STESBalanDisch(t) STES's discharge energy balance (kWh_th)

*************************************************************
*STES Losses (Self-discharging)

*Equations
STESLossE1(t)  e1's energy losses on period t (kWh_th)

STESLossE2(t)  e2's energy losses on period t (kWh_th)

STESLossE3(t)  e3's energy losses on period t (kWh_th)
*************************************************************
********************************************************************************************************************************************************************
*Definition of the Heating system equations

HtMaxPw(t)    heat pump's maximum power capacity for any given t (kW_el)

HtBalanHt(t)  thermal energy balance (kWh_th)

HtDayToHr(d)  conversion of heating demand from daily to hourly resolution (kWh_th)

HtSuppHr(t)   heating demand supply equation (kWh_th)

HtHPDemEl(t)  heat pump's electricity heating demand(kWh_el)

******************************************************************************************
*Definition of the PV system equations

PVSupp(t)   electricity generated by the PV system (kWh)

******************************************************************************************
* Definition of the grid equations

*Constraints
GridMaxDem(t)  maximum electric power demand from the grid for any given t (kW_el)

GridMaxSupp(t) maximum electric power supply from the grid for any given t (kW_el)

*Equations
GridDemTot(t)  total electricity demand from the grid (kWh_el)

Demand_Balance(t)  electricity flow balance on the grid consumption point

******************************************************************************************
* Definition of the aggregated system equations


TotDischTh    aggregated STES thermal discharge (kWh_th)

TotChTh      aggregated STES thermal charge (kWh_th)

TotChEl      aggregated STES electric charge (kWh_el)

TotHpDirTh   aggregated direct thermal heating provided by the heat pump (kWh_th)

TotHpDirEl   aggregated direct electric heating provided by the heat pump (kWh_th)

TotPV            aggregated PV production

TotGrid          aggregated grid consumption

CostElec         aggregated cost of electricity consumption (CHF)

CapexPV    total PV capital costs for a given archetype (CHF)

OpexPV    total PV operational costs for a given archetype (CHF)

CapexBat    total battery capital costs for a given archetype (CHF)

OpexBat    total battery operational costs for a given archetype (CHF)

PVShare    share of total PV installed relative to available roof area (%)

CostSystElecTot    total electricity system costs (CHF)

;

******************************************************************************************
*PV system equations

*PVMaxLim ..    pv_pel =l= pv_max_cap;
*******************************************************************************************************************************************************************
*Battery system equations

*BatMaxLim(t) ..   bat_st_cap =l= bat_max_cap;

BatUpLim(t) ..    bat_level(t) =l= bat_st_cap;

BatLwLim(t) ..    bat_level(t) =g= bat_soc_min;

BatMaxDisch(t) .. bat_disch(t) =l= bat_st_cap/bat_en_pwr_rat;

BatMaxCh(t) ..    bat_ch(t) =l= bat_st_cap/bat_en_pwr_rat;

*(stes_e1_lev_th_init)$(ord(t) eq 1)

BatLevCh(t) ..    bat_level(t) =e= bat_level(t-1) + bat_ch(t) - bat_disch(t);
*BatLevCh(t) ..    bat_level(t) =e= (bat_level(t-1)$(ord(t)) + bat_ch(t) - bat_disch(t);

********************************************************************************************************************************************************************
*DHW Decentralized Storage System equations

DHWDecUpLim(t) ..     dhw_dec_lev_th(t) =l= dhw_dec_st_cap;

DHWDecLwLim(t) ..     dhw_dec_lev_th(t) =g= dhw_dec_st_cap * dhw_dec_soc_min;

DHWDecMaxPw(t) ..     dhw_dec_ch_el(t) =l= dhw_dec_pel;

DHWDecElecToTh(t) ..  dhw_dec_ch_el(t) =e= dhw_dec_ch_th(t) / dhw_dec_eff;

DHWDecLevCh(t) ..     dhw_dec_lev_th(t) =e= dhw_dec_lev_th(t-1) + dhw_dec_ch_th(t) - dhw_dec_dem_th(t);


********************************************************************************************************************************************************************
*Seasonal Thermal Energy Storage (STES) System equations
*************************************************************
*Storage e1

STESUpLimE1(t) ..    stes_e1_lev_th(t) =l= th_stes_emax1;

STESElecToThE1(t) .. stes_ch_e1_el(t) =e= stes_ch_e1_th(t)/cop_aw35_t(t);

*STESLevChE1(t) ..    stes_e1_lev_th(t) =e= stes_e1_lev_th(t-1) + stes_ch_e1_th(t) - stes_disch_e1_th(t) - stes_loss_e1_th(t) + (stes_e1_lev_th_init)$(ord(t) eq 1);

STESLevChE1(t) ..    stes_e1_lev_th(t) =e= stes_e1_lev_th(t-1) + stes_ch_e1_th(t) - stes_disch_e1_th(t) - stes_loss_e1_th(t);

*************************************************************
*Storage e2

STESUpLimE2(t) ..    stes_e2_lev_th(t) =l= th_stes_emax2;

STESElecToThE2(t) .. stes_ch_e2_el(t) =e= stes_ch_e2_th(t)/cop_aw60_t(t);

*STESLevChE2(t) ..    stes_e2_lev_th(t) =e= stes_e2_lev_th(t-1) + stes_ch_e2_th(t) - stes_disch_e2_th(t) - stes_loss_e2_th(t)  + (stes_e2_lev_th_init)$(ord(t) eq 1);

STESLevChE2(t) ..    stes_e2_lev_th(t) =e= stes_e2_lev_th(t-1) + stes_ch_e2_th(t) - stes_disch_e2_th(t) - stes_loss_e2_th(t);

*************************************************************
*Storage e3

STESUpLimE3(t) ..    stes_e3_lev_th(t) =l= th_stes_emax3;

STESMaxPwE3(t) ..    stes_ch_e3_el(t) =l= th_stes_pmaxcharge3;

STESElecToThE3(t) .. stes_ch_e3_el(t) =e= stes_ch_e3_th(t)/th_stes_chargeefficiency3;

*STESLevChE3(t) ..    stes_e3_lev_th(t) =e= stes_e3_lev_th(t-1) + stes_ch_e3_th(t) - stes_disch_e3_th(t) - stes_loss_e3_th(t) + (stes_e3_lev_th_init)$(ord(t) eq 1);

STESLevChE3(t) ..    stes_e3_lev_th(t) =e= stes_e3_lev_th(t-1) + stes_ch_e3_th(t) - stes_disch_e3_th(t) - stes_loss_e3_th(t);

*************************************************************
*State of charge equations

STESUpSOCLim(t) .. stes_soc_tot(t) =l= 1;

STESSOCe1(t) ..    stes_soc_1(t) =e= stes_e1_lev_th(t) / th_stes_emax1;

STESSOCe2(t) ..    stes_soc_2(t) =e= stes_e2_lev_th(t) / th_stes_emax2;

STESSOCe3(t) ..    stes_soc_3(t) =e= stes_e3_lev_th(t) / th_stes_emax3;

STESSOCtot(t) ..   stes_soc_tot(t) =e= (stes_soc_1(t)*0.214) + (stes_soc_2(t)*0.357) + (stes_soc_3(t)*0.429);

*************************************************************
*Charging equations

STESTotChHP(t) ..  stes_ch_tot_hp_el(t) =e= (stes_ch_e1_el(t) + stes_ch_e2_el(t));

*************************************************************
*Discharging equations

STESMaxDisch(t) ..   stes_disch_tot_th(t) =l= th_pel_hp*cop_aw35_t(t);

STESBalanDisch(t) .. stes_disch_tot_th(t) =e= (stes_disch_e1_th(t) + stes_disch_e2_th(t) + stes_disch_e3_th(t));

*************************************************************
*Losses equations  (Self-discharging)

STESLossE1(t) ..    stes_loss_e1_th(t) =e=  stes_loss_tot_e1_th*stes_soc_1(t);

STESLossE2(t) ..    stes_loss_e2_th(t) =e=  stes_loss_tot_e2_th*0.357*stes_soc_2(t);

STESLossE3(t) ..    stes_loss_e3_th(t) =e=  stes_loss_tot_e3_th*0.429*stes_soc_3(t);

*************************************************************
********************************************************************************************************************************************************************
*Heating system equations

HtDayToHr(d) ..   sum(td(t,d), heat_dem_t_th(t)) =e= heat_dem_d_th(d);

HtSuppHr(t) ..    heat_dem_t_th(t) =e=  stes_disch_tot_th(t) + heat_hp_th(t);

HtHPDemEl(t) ..   heat_hp_el(t) =e= heat_hp_th(t) / cop_aw35_t(t);

HtBalanHt(t) ..   hp_tot_el(t) =e= 0.9999*heat_hp_el(t) + stes_ch_tot_hp_el(t) + dhw_cen_dem_el(t);

HtMaxPw(t) ..     hp_tot_el(t) =l= th_pel_hp;

********************************************************************************************************************************************************************
*PV system equations
*PVSupp(t) ..   pv_sup(t) =e= pv_cf(t,"pv_cf")*pv_pel;

PVSupp(t) ..   pv_sup(t) =e= pv_cf_prof(t)*pv_pel;

********************************************************************************************************************************************************************
*Grid system constraints
GridMaxDem(t)  ..    grid_dem_tot(t) =l= grid_max_dem;

GridMaxSupp(t)  ..   grid_dem_tot(t) =g= - grid_max_sup;

GridDemTot(t)..      grid_dem_tot(t) =e= elec_dem(t)
                                         + bat_ch(t)/bat_eff
                                         + hp_tot_el(t)
                                         + dhw_dec_ch_el(t)
                                         + stes_ch_e3_el(t)
                                         - pv_sup(t)
                                         - bat_disch(t)*bat_eff;

*pv curtailmnet variable with high cost
Demand_Balance(t)..    grid_dem_tot(t) =e= demand(t) - supply(t);


TotDischTh .. stes_yearly_disch_th =e= sum(t,stes_disch_tot_th(t));

TotChTh .. stes_yearly_ch_th =e= sum(t,stes_ch_e1_th(t)) + sum(t,stes_ch_e2_th(t)) + sum(t,stes_ch_e3_th(t));

TotChEl .. stes_yearly_ch_el =e= sum(t,stes_ch_tot_hp_el(t)) + sum(t,stes_ch_e3_el(t));

TotHpDirTh .. hp_dir_yearly_th =e= sum(t,heat_hp_th(t));

TotHpDirEl .. hp_dir_yearly_el =e= sum(t,heat_hp_el(t));

TotPV .. pv_prod_tot =e= sum(t,pv_sup(t));

TotGrid .. sum_grid_dem_tot =e=  sum(t,grid_dem_tot(t));

CostElec .. elec_cost =e= sum(t,(demand(t)*price_grid(t)))
                        - sum(t,(supply(t)*price_supply(t)));

CapexPV .. pv_capex =e= pv_eac_capex*pv_pel;

OpexPV .. pv_opex =e= pv_eac_opex*pv_pel;

CapexBat .. bat_capex =e= bat_eac_capex*bat_st_cap;

OpexBat .. bat_opex =e= bat_eac_opex*bat_st_cap;

PVShare .. pv_share =e= pv_pel / pv_max_cap;

CostSystElecTot .. syst_elec_cost =e= elec_cost + pv_capex + pv_opex + bat_capex + bat_opex;

Model costs /all/ ;


Option
         reslim = 9990000000,
         iterlim = 999000000,
         threads = -1,
         Optcr = 0.00
;


