################################################
###########~~Units and names~~##################
################################################

nameviz <- case_when(
  varname == "Msh"        ~ paste0("Market share for ", techno),
  (varname == "InstCap_GW" & techno != "no") ~ paste0("Installed Cap for ", techno),
  (varname == "InstCap_GW" & techno == "no") ~ paste0("Installed Cap "),
  varname == "Inv"        ~ paste0("Investment in ", techno),
  varname == "Qelec_hist"      ~ "Total electricity demand",
  varname == "LCOE_hist"  ~ "Levelized Cost of Electricity",
  varname == "Capex"  ~ "CAPEX",
  varname == "Carbon_p"  ~ "Carbon price",
  varname == "Cfuel"      ~ paste0("Fuel cost for ", techno),
  varname == "WACC_hist"       ~ "Cost of Capital",
  varname == "Pelec"      ~ "Electricity price (LCOE)",
  varname == "PelecArm"      ~ "Electricity price (Armington)",
  varname == "PelecArmDF"      ~ "Electricity price (Armington DF)",
  varname == "Pelecfin"      ~ "Electricity price (final Price)",
  varname == "LCOE_sys"      ~ "System LCOE",
  varname == "Markup"     ~ "VRE SIC markup",
  varname == "markup_elec"     ~ "Markup for the electricity sector",
  varname == "Utilrate"     ~ "Utilization rate",
  varname == "Prod"       ~ "Net electricity generation",
  varname == "peak_MW_tot"  ~ "Peakload",
  varname == "base_MW"    ~ "Baseload(residual)",
  varname == "peak_res_cov" ~ "Residual peak load coverage",
  varname == "Loans_derisk" ~ "Derisked loans",
  varname == "climfin_share_inv" ~ "VRE projects receiving climate finance",
  varname == "Emi_CO2" ~ "CO2 emissions from Energy and Industrial Processes",
  varname == "Cumul_emi" ~ "Cumulated CO2 emissions",
  varname == "mean_private_K_cost" ~ "Mean cost of private capital",
  varname == "Cons_loss" ~ "Consumption losses",
  varname == "GDP_loss" ~ "GDP losses",
  varname == "GDP_MER_real_hist" ~ "GDP (MER)",
  varname == "Prim" ~ "Primary Energy",
  varname == "PrimCoal" ~ "Primary Energy - Coal",
  varname == "PrimGas" ~ "Primary Energy - Gas",
  varname == "PrimNuke" ~ "Primary Energy - Nuclear",
  varname == "PrimBio" ~ "Primary Energy - Biomass",
  varname == "PrimSolar" ~ "Primary Energy - Solar",
  varname == "PrimWind" ~ "Primary Energy - Wind",
  varname == "PrimOil" ~ "Primary Energy - Oil",
  varname == "share_believers" ~ "Share of believers",
  varname == "private_D_cost_CPV" ~ "Cost of private debt")

unitviz <- ifelse((vsbaseline & plot_bench_percent), paste0("% change from ", baseline), unitviz)
Reg <- ifelse(sum(regionviz == All) == length(All), "World", "")
