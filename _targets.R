# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(targets)
library(tarchetypes)
source("packages.R")
source("R/functions.R")

conflict_prefer("filter", "dplyr")
#plan(callr)

#tar_option_set(packages=c("dplyr", "tidyr", "readr", "purrr", "stringr", "ggplot2",
#                          "lubridate", "glue", "assertr", "sf", "bcmaps", "bcdata",
#                          "rmapshaper", "geojsonio", "ggiraph", "cowplot", "shiny",
#                          "knitr", "rmarkdown", "kableExtra", "tibble"),
#               imports=c("bcmaps", "bcdata"))

# load datasets ------------------------------------------------------------------------------------

load_datasets <- list(

  # Biodiversity Layers
  tar_target(og_data, og_load()), #all old-growth
  tar_target(priority_og_data, priority_og()), # priority, 2.6 million deferral layer
  tar_target(ch_data, ch_data_load()), #critical habitat data, overlapping
  tar_target(ch_flat, ch_data_flat()), # critical habitat flat


  # Land Use & Protected Areas
  tar_target(pa_data, get_cpcad_bc_data()), # ppa and oecm, function will pull data from website
  tar_target(des_lands_vis, designated_lands_vis()), # designated lands, overlapping
  tar_target(des_lands, designated_lands()), # designated lands, flat
  tar_target(lup_data, lup_boundaries()), # land use planning boundaries
  tar_target(future_cons, ralcp()), # potential future conservation areas, IPCAs, etc.

  #Industrial Activities
  tar_target(cutblock_data, forest_cutblock()), # active/pending/retired cutblock
  tar_target(harv_auth, harvest_authority()),
  tar_target(managed_license, forest_license()),
  tar_target(tfl_data, tree_farm_license()),
  tar_target(tsa_area, tsa()),

  tar_target(mining_active, active_min_data()),
  tar_target(mining_tenure, potential_min_data()),

  tar_target(land_tenure, land_tenures()),

  tar_target(eao_points, eao_data()),


  tar_target(caribou_full_herd, caribou_full()),
  tar_target(y2y, y2y_area()),
  tar_target(pip, pip_areas()),
  tar_target(incomp_area, incom()),
  tar_target(fish_area, fish()),
  tar_target(fish_10_buff, fish_10()),
  tar_target(fish_5_buff, fish_5()),
  tar_target(tfl_area, tfl()),
  tar_target(og, og_all()),
  tar_target(du9, du9_area()),
  tar_target(cdc_load, cdc()),
  tar_target(cef_data, cef()),
  tar_target(tsa_area, tsa()),


  #Site data, add/change here
  tar_target(th_wma_data, th_wma())
)

clean_data <- list(
  tar_target(clean_pa, remove_overlaps(pa_data)),
  tar_target(og_dependent_ch, filter_og_dependent(ch_data)),
  tar_target(lup_clipped, intersect_pa(lup_data, bcmaps::bc_bound_hres())),
  tar_target(pa_type, pa_by_type(clean_pa)),
  tar_target(pa_type_mod, pa_tech_simp(pa_tech)),
  tar_target(int_wet_belt, int_wet_all(bec_12)),
  tar_target(int_wet_belt_tran, int_tran(bec_12)),
  tar_target(int_wet_belt_itr, int_itr(bec_12)),
  tar_target(int_wet_belt_iwb, int_iwb(bec_12))
  #tar_target(ch_agg, ch_data_agg(ch_data))
  #tar_target(ppa_og, merge_areas(clean_pa, faib_og_extra))
)

intersect_data <- list(
  #tar_target(pa_og, intersect_pa(pa_data, og_data)),
  tar_target(pa_og_tech, intersect_pa(pa_tech, og_data)),
  tar_target(og_ch, intersect_pa(og_data, ch_data)),
  tar_target(prot_og_ch, intersect_pa(pa_og_tech, ch_data)),
  tar_target(og_watershed, intersect_pa(og_data, watershed_data)),
  tar_target(prot_watershed, intersect_pa(pa_og_tech, watershed_data)),
  tar_target(bec_og, intersect_pa(og_data, bec_12)),
  tar_target(bec_pa_og, intersect_pa(pa_og_tech, bec_12)),
  tar_target(community_watersheds_licenses_test, intersect_point_to_poly(og_watershed, water_licenses)),
  tar_target(community_watersheds_licenses_prot, intersect_point_to_poly(prot_watershed, water_licenses)),
  tar_target(community_population, intersect_point_to_poly(community_watersheds_licenses_test, bcmaps::bc_cities())),
  tar_target(fn_watersheds, intersect_point_to_poly(watershed_data, fn_communities)),
  tar_target(pop_watersheds, intersect_pa(watershed_data, population)),
  tar_target(df1_og, intersect_pa(bec_og, df_1)),
  tar_target(df2_og, intersect_pa(bec_og, df_2)),
  tar_target(df1_og_prot, intersect_pa(bec_pa_og, df_1)),
  tar_target(df2_og_prot, intersect_pa(bec_pa_og, df_2)),
  tar_target(og_ch_flat, intersect_pa(og_data, ch_flat)),
  tar_target(og_ch_flat_prot, intersect_pa(og_ch_flat, pa_og_tech)),
  tar_target(heat_lup, intersect_pa(heat_map, lup_clipped)),
  tar_target(heat_future, intersect_pa(heat_map, future_cons)),
  tar_target(heat_iwb, intersect_pa(heat_map, int_wet_belt)),
  tar_target(heat_gcr, intersect_pa(heat_map, caribou_cons_area)),
  tar_target(lup_pa, intersect_pa(clean_pa, lup_clipped)),
  tar_target(pcl_pa, intersect_pa(clean_pa, future_cons)),
  tar_target(iwb_pa, intersect_pa(clean_pa, int_wet_belt))
)

round_2_analyses<- list(
  ##### old layers
  #tar_target(faib_og_layer, intersect_pa(pa_og_tech, priority_og)),
  #tar_target(faib_ch, intersect_pa(faib_og_extra, ch_data)),
  #tar_target(faib_watersheds, intersect_pa(faib_og_extra, watershed_data)),
  #tar_target(faib_by_district, intersect_pa(faib_og_extra, bcmaps::nr_districts())),
  #tar_target(ch_by_district, intersect_pa(ch_data, bcmaps::nr_districts())),
  #tar_target(og_ch_by_district, intersect_pa(og_ch, bcmaps::nr_districts())),
  #tar_target(faib_og_ch, intersect_pa(ch_by_district, faib_by_district)),
  #tar_target(faib_prot_watershed, intersect_pa(prot_watershed, faib_by_district)),
  #tar_target(faib_bec_pa, intersect_pa(bec_pa_og, faib_by_district)),
  #tar_target(faib_df1_og, intersect_pa(df1_og, faib_by_district)),
  #tar_target(faib_df2_og, intersect_pa(df2_og, faib_by_district)),
  #tar_target(faib_prot_df1_og, intersect_pa(df1_og_prot, bcmaps::nr_districts())),
  #tar_target(faib_prot_df2_og, intersect_pa(df2_og_prot, bcmaps::nr_districts())),
  #tar_target(prot_ch_by_district, intersect_pa(prot_og_ch, bcmaps::nr_districts())),
  #tar_target(bec_dist, intersect_pa(bec_12, bcmaps::nr_districts())),
  #tar_target(og_faib_lup, intersect_pa(faib_og_extra, lup_clipped)),
  #tar_target(at_risk_bec, intersect_pa(faib_og_extra, bec_12)),
 # tar_target(all_og_bec, intersect_pa(og_data, bec_12)),
  #tar_target(at_risk_bec_eco, intersect_pa(at_risk_bec, bcmaps::ecoregions())),
  #tar_target(og_bec_eco, intersect_pa(bec_og, bcmaps::ecoregions())),
  #tar_target(og_bec_eco_pa, intersect_pa(og_bec_eco, clean_pa))
  #tar_target(bec_dist_og, intersect_pa(bec_og, bcmaps::nr_districts())),
  #tar_target(bec_dist_og_pa, intersect_pa(bec_pa_og, bcmaps::nr_districts())),
  #tar_target(bec_faib, intersect_pa(bec_12, faib_by_district))
  #tar_target(bec_pa_by_district, intersect_pa(bec_pa_og,  bcmaps::nr_districts()))),
  #tar_target(prot_df1_og_by_district, intersect_pa(df1_og_prot, faib_by_district)),
  #tar_target(prot_df2_og_by_district, intersect_pa(df2_og_prot, faib_by_district)),
)

round_3_analyses <- list(
  #
  tar_target(unprotected, remove_pa(bcmaps::bc_bound_hres(), pa_type)),
  #tar_target(prot_gap, remove_pa(pa_type_mod, pa_type))
  tar_target(priority_og_pa, intersect_pa(pa_tech, priority_og)),
  #bec og comparison
  tar_target(bec_og_priority, intersect_pa(priority_og, bec_12)),
  tar_target(bec_pa_og_priority, intersect_pa(priority_og_pa, bec_12)),
  # og by ch
  tar_target(priority_og_ch, intersect_pa(priority_og, ch_data)),
  tar_target(priority_prot_og_ch, intersect_pa(priority_og_pa, ch_data)),
  #
  tar_target(priority_og_ch_flat, intersect_pa(priority_og, ch_flat)),
  tar_target(priority_og_ch_flat_prot, intersect_pa(priority_og_pa, ch_flat)),
  # og by nr district
  tar_target(og_by_district_priority, intersect_pa(priority_og, bcmaps::nr_districts())),
  tar_target(prot_by_district_priority, intersect_pa(priority_og_pa, bcmaps::nr_districts())),
  tar_target(og_by_district, intersect_pa(og_data, bcmaps::nr_districts())),
  tar_target(prot_by_district, intersect_pa(pa_og_tech, bcmaps::nr_districts())),
  # og by lup areas
  tar_target(og_lup_priority, intersect_pa(priority_og, lup_clipped)),
  tar_target(og_pa_lup_priority, intersect_pa(priority_og_pa, lup_clipped)),
  # og df
  tar_target(df1_og_priority, intersect_pa(bec_og_priority, df_1)),
  tar_target(df2_og_priority, intersect_pa(bec_og_priority, df_2)),
  tar_target(df1_og_prot_priority, intersect_pa(bec_pa_og_priority, df_1)),
  tar_target(df2_og_prot_priority, intersect_pa(bec_pa_og_priority, df_2)),
  #
  tar_target(priority_og_watershed, intersect_pa(priority_og, watershed_data)),
  tar_target(priority_prot_watershed, intersect_pa(priority_og_pa, watershed_data))
)

site_based_analyses <- list(
  tar_target(pa_smc, intersect_pa(caribou_cons_area, clean_pa)),
  tar_target(des_lands_smc, intersect_pa(caribou_cons_area, des_lands)),
  tar_target(des_lands_smc_vis, intersect_pa(caribou_cons_area, des_lands_vis)),
  tar_target(priority_og_smc, intersect_pa(caribou_cons_area, priority_oct25)),
  tar_target(priority_og_pa_smc, intersect_pa(priority_og_smc, pa_smc)),
  tar_target(priority_og_des, intersect_pa(priority_og_smc, des_lands_smc)),
  tar_target(priority_og_des_vis, intersect_pa(des_lands_smc_vis, priority_og_smc)),
  #tar_target(priority_og_smc_pa_env, intersect_pa(priority_og_smc, clean_pa)),
  tar_target(og_smc, intersect_pa(caribou_cons_area, og_data)),
  tar_target(og_pa_smc, intersect_pa(og_smc, pa_smc)),
  tar_target(og_des_lands, intersect_pa(des_lands_smc, og_smc)),
  tar_target(og_des_lands_vis, intersect_pa(des_lands_smc_vis, og_smc)),
  #tar_target(og_pa_smc, intersect_pa(og_smc, clean_pa)),
  #bec og comparison
  #tar_target(bec_og_priority, intersect_pa(caribou_cons_area, bec_12)),
  #tar_target(bec_pa_og_priority, intersect_pa(pa_smc, bec_pa_og_priority)),
  #smc by ch areas
  tar_target(smc_ch, intersect_pa(caribou_cons_area, ch_data)),
  tar_target(smc_ch_pa, intersect_pa(pa_smc, ch_data)),
  tar_target(smc_ch_des_vis, intersect_pa(des_lands_smc_vis, ch_data)),
  #threat data
  tar_target(smc_forest, intersect_pa(caribou_cons_area, forest_ten)),
  tar_target(smc_mine_active, intersect_pa(caribou_cons_area, mining_active)),
  tar_target(smc_mine_potential, intersect_pa(caribou_cons_area, mining_tenure)),
  tar_target(smc_land, intersect_pa(caribou_cons_area, land_tenure)),
  tar_target(smc_eao, intersect_point_to_poly(caribou_cons_area, eao_points)),
  tar_target(smc_tenures, intersect_pa(caribou_cons_area, forestry_tenures)),
  tar_target(smc_harv, intersect_pa(caribou_cons_area, harv_auth)),
  #
  tar_target(smc_caribou_pa, intersect_pa(pa_smc, caribou_gcr_herd)),
  tar_target(smc_caribou_pa_des, intersect_pa(des_lands_smc, caribou_gcr_herd)),
  tar_target(smc_caribou_pa_uwr, intersect_pa(des_lands_smc_vis, caribou_gcr_herd)),

  tar_target(caribou_herd_pa, intersect_pa(clean_pa, caribou_full_herd)),

  tar_target(gcr_y2y, intersect_pa(caribou_cons_area, y2y)),
  tar_target(full_herd_y2y, intersect_pa(caribou_full_herd, y2y)),

  tar_target(pip_gcr, intersect_pa(pip, caribou_cons_area)),
  tar_target(pip_full_herd, intersect_pa(pip, caribou_full_herd)),
  tar_target(cut_og, intersect_pa(priority_og_smc, smc_forest)),
  tar_target(cut_og_pa, intersect_pa(priority_og_pa_smc, smc_forest))
)

site_based_analyses_incomp <- list(
  tar_target(pa_inc, intersect_pa(incomp_area, clean_pa)),
  tar_target(des_lands_inc, intersect_pa(incomp_area, des_lands)),
  tar_target(des_lands_inc_vis, intersect_pa(incomp_area, des_lands_vis)),
  tar_target(priority_og_inc, intersect_pa(incomp_area, priority_oct25)),
  tar_target(priority_og_pa_inc, intersect_pa(priority_og_inc, pa_inc)),
  tar_target(priority_og_des_inc, intersect_pa(priority_og_inc, des_lands_inc)),
  tar_target(priority_og_des_vis_inc, intersect_pa(des_lands_inc_vis, priority_og_inc)),
  #tar_target(priority_og_smc_pa_env, intersect_pa(priority_og_smc, clean_pa)),
  tar_target(og_inc, intersect_pa(incomp_area, og_data)),
  tar_target(og_pa_inc, intersect_pa(og_inc, pa_inc)),
  tar_target(og_des_lands_inc, intersect_pa(des_lands_inc, og_inc)),
  tar_target(og_des_lands_vis_inc, intersect_pa(des_lands_inc_vis, og_smc)),
  #tar_target(og_pa_smc, intersect_pa(og_smc, clean_pa)),
  tar_target(inc_forest, intersect_pa(incomp_area, forest_ten)),
  tar_target(inc_mine_active, intersect_pa(incomp_area, mining_active)),
  tar_target(inc_mine_potential, intersect_pa(incomp_area, mining_tenure)),
  tar_target(inc_land, intersect_pa(incomp_area, land_tenure)),
  tar_target(inc_eao, intersect_point_to_poly(incomp_area, eao_points)),
  tar_target(inc_tenures, intersect_pa(incomp_area, forestry_tenures)),
  tar_target(inc_harv, intersect_pa(incomp_area, harv_auth)),
  tar_target(inc_ch, intersect_pa(incomp_area, ch_data)),
  tar_target(inc_ch_des, intersect_pa(inc_ch, des_lands_inc)),

  #
  tar_target(inc_y2y, intersect_pa(incomp_area, y2y)),
  tar_target(pip_inc, intersect_pa(pip, incomp_area)),
   ## FISH RIVER
  tar_target(pa_fish, intersect_pa(fish_area, clean_pa)),
  tar_target(des_lands_fish, intersect_pa(fish_area, des_lands)),
  tar_target(des_lands_fish_vis, intersect_pa(fish_area, des_lands_vis)),
  tar_target(priority_og_fish, intersect_pa(fish_area, priority_oct25)),
  tar_target(priority_og_pa_fish, intersect_pa(priority_og_fish, pa_fish)),
  tar_target(priority_og_des_fish, intersect_pa(priority_og_fish, des_lands_fish)),
  tar_target(priority_og_des_vis_fish, intersect_pa(des_lands_fish_vis, priority_og_fish)),
  #tar_target(priority_og_smc_pa_env, intersect_pa(priority_og_smc, clean_pa)),
  tar_target(og_fish, intersect_pa(fish_area, og_data)),
  tar_target(og_pa_fish, intersect_pa(og_fish, pa_fish)),
  tar_target(og_des_lands_fish, intersect_pa(des_lands_fish, og_fish)),
  tar_target(og_des_lands_vis_fish, intersect_pa(des_lands_fish_vis, og_smc)),
  #tar_target(og_pa_smc, intersect_pa(og_smc, clean_pa)),
  tar_target(fish_forest, intersect_pa(fish_area, forest_ten)),
  tar_target(fish_mine_active, intersect_pa(fish_area, mining_active)),
  tar_target(fish_mine_potential, intersect_pa(fish_area, mining_tenure)),
  tar_target(fish_land, intersect_pa(fish_area, land_tenure)),
  tar_target(fish_eao, intersect_point_to_poly(fish_area, eao_points)),
  tar_target(fish_tenures, intersect_pa(fish_area, forestry_tenures)),
  tar_target(fish_harv, intersect_pa(fish_area, harv_auth)),
  tar_target(fish_tfl, intersect_pa(fish_area, tfl_area)),
  #
  tar_target(fish_y2y, intersect_pa(fish_area, y2y)),
  tar_target(pip_fish, intersect_pa(pip, fish_area)),
  tar_target(fish_ch, intersect_pa(ch_data, fish_area)),
  tar_target(fish_ch_des, intersect_pa(fish_ch, des_lands_fish)),

  #
  tar_target(des_lands_fish_5, intersect_pa(fish_5_buff, des_lands)),
  tar_target(des_lands_fish_5_vis, intersect_pa(fish_5_buff, des_lands_vis)),
  #
  tar_target(fish_forest_10, intersect_pa(fish_10_buff, forest_ten)),
  tar_target(fish_mine_active_10, intersect_pa(fish_10_buff, mining_active)),
  tar_target(fish_mine_potential_10, intersect_pa(fish_10_buff, mining_tenure)),
  tar_target(fish_land_10, intersect_pa(fish_10_buff, land_tenure)),
  tar_target(fish_eao_10, intersect_point_to_poly(fish_10_buff, eao_points)),
  tar_target(fish_tenures_10, intersect_pa(fish_10_buff, forestry_tenures)),
  tar_target(fish_harv_10, intersect_pa(fish_10_buff, harv_auth)),
  tar_target(fish_tfl_10, intersect_pa(fish_10_buff, tfl_area)),
  tar_target(fish_ch_10, intersect_pa(fish_10_buff, ch_data)),
  tar_target(fish_des_10, intersect_pa(fish_10_buff, des_lands)),
  tar_target(fish_des_vis_10, intersect_pa(fish_10_buff, des_lands_vis)),
  tar_target(fish_pa_10, intersect_pa(fish_10_buff, clean_pa))
)

site_based_analysis_du9 <- list(
  tar_target(pa_du9, intersect_pa(du9, clean_pa)),
  tar_target(des_lands_du9, intersect_pa(du9, des_lands)),
  tar_target(des_lands_du9_vis, intersect_pa(du9, des_lands_vis)),

  # priority og
  tar_target(priority_og_du9, intersect_pa(du9, priority_oct25)),
  tar_target(priority_og_pa_du9, intersect_pa(priority_og_du9, pa_du9)),
  tar_target(priority_og_des_du9, intersect_pa(priority_og_du9, des_lands_du9)),
  tar_target(priority_og_des_vis_du9, intersect_pa(des_lands_du9_vis, priority_og_du9)),
  #tar_target(priority_og_smc_pa_env, intersect_pa(priority_og_smc, clean_pa)),

  # all og
  tar_target(og_du9, intersect_pa(du9, og)),
  tar_target(og_pa_du9, intersect_pa(og_du9, pa_du9)),
  tar_target(og_des_lands_du9, intersect_pa(des_lands_du9, og_du9)),
  tar_target(og_des_lands_vis_du9, intersect_pa(des_lands_du9_vis, og_du9)),

  tar_target(og_5_du9, intersect_pa(du9, og_5_data)),
  tar_target(og_5_pa_du9, intersect_pa(og_5_data, pa_du9)),

  #tar_target(og_pa_smc, intersect_pa(og_smc, clean_pa)),
  tar_target(du9_forest, intersect_pa(du9, forest_ten)),
  tar_target(du9_mine_active, intersect_pa(du9, mining_active)),
  tar_target(du9_mine_potential, intersect_pa(du9, mining_tenure)),
  tar_target(du9_land, intersect_pa(du9, land_tenure)),
  tar_target(du9_eao, intersect_point_to_poly(du9, eao_points)),
  tar_target(du9_tenures, intersect_pa(du9, forestry_tenures)),
  tar_target(du9_harv, intersect_pa(du9, harv_auth)),
  tar_target(du9_ch, intersect_pa(du9, ch_data)),
  tar_target(du9_ch_des, intersect_pa(ch_data, des_lands_du9)),
  tar_target(du9_ch_pa, intersect_pa(ch_data, pa_du9)),
  tar_target(du9_cdc, intersect_pa(cdc_load, du9)),
  tar_target(du9_cdc_pa, intersect_pa(cdc_load, pa_du9)),
  tar_target(du9_ch_flat, intersect_pa(ch_flat, du9)),
  tar_target(du9_ch_flat_pa, intersect_pa(ch_flat, pa_du9)),

  tar_target(du9_cef, intersect_pa(cef_data, du9)),
  tar_target(du9_tfl, intersect_pa(cef_data, tfl_area)),
  tar_target(du9_tsa, intersect_pa(cef_data, tsa_area)),

  tar_target(priority_du9_tfl, intersect_pa(priority_og_du9, tfl_area)),
  tar_target(priority_du9_tfl_pa, intersect_pa(priority_og_pa_du9, tfl_area)),
  tar_target(priority_du9_tsa, intersect_pa(priority_og_du9, tsa_area)),
  tar_target(priority_du9_tsa_pa, intersect_pa(priority_og_pa_du9, tsa_area)),


  tar_target(og_5_du9_tfl, intersect_pa(og_5_du9, tfl_area)),
  tar_target(og_5_du9_tfl_pa, intersect_pa(og_5_pa_du9, tfl_area)),
  tar_target(og_5_du9_tsa, intersect_pa(og_5_du9, tsa_area)),
  tar_target(og_5_du9_tsa_pa, intersect_pa(og_5_pa_du9, tsa_area))
)


# WMA Analysis ------------------------------------------------------------



wma_analysis <- list(
  tar_target(wma_og, intersect_pa(th_wma_data, og_data)),
  tar_target(wma_ch, intersect_pa(th_wma_data, ch_data)),
  tar_target(wma_des, intersect_pa(th_wma_data, des_lands)),
  tar_target(wma_lup, intersect_pa(th_wma_data, lup_data))

)


process_data <- list(
  #tar_target(og_parks_removed, remove_pa(og_data, pa_tech)),
  tar_target(carbon_interior,carbon_analysis_interior(bec_og, bec_pa_og)),
  tar_target(carbon_coast, carbon_analysis_coastal(bec_og, bec_pa_og))
)


#summarize_data <- list(
 # tar_target(og_watershed_area, measure_og_watershed_area(og_data, og_watershed, prot_watershed))

# )



# targets pipeline --------------------------------------------------------
list(
  load_datasets,
  clean_data,
  #intersect_data,
  #round_2_analyses,
  #round_3_analyses,
  #site_based_analyses,
  #site_based_analyses_incomp,
  #site_based_analysis_du9
  wma_analysis
  #process_data
  #summarize_data
  #analyze_data,
  #plot_data,
  #save_csvs,
  #tar_render(report_html, "eco_rep_report.Rmd", output_format = "html_document"),
  #tar_render(report_pdf, "eco_rep_report.Rmd", output_format = "pdf_document")
)
#add list(,
#tar_targets() for each intermediate step of workflow)
