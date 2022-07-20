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


# load datasets ------------------------------------------------------------------------------------

load_datasets <- list(

  # Biodiversity Layers
  tar_target(og_data, og_load()), #all old-growth
  tar_target(priority_og_data, priority_og()), # priority, 2.6 million deferral layer
  tar_target(ch_data, ch_data_load()), #critical habitat data, overlapping
  tar_target(ch_flat, ch_data_flat()), # critical habitat flat

  # Site data, add/change here
  tar_target(th_wma_data, th_wma())
)



# WMA Analysis ------------------------------------------------------------

wma_analysis <- list(
  tar_target(wma_og_all, intersect_pa(th_wma_data, og_data)),
  tar_target(wma_priority_og, intersect_pa(th_wma_data, priority_og_data)),
  tar_target(wma_des_overlap, intersect_pa(th_wma_data, ch_data)),
  tar_target(wma_des_flat, intersect_pa(th_wma_data, ch_flat))
)



# targets pipeline --------------------------------------------------------
list(
  load_datasets,
  wma_analysis
)




# Extras

# clean_data <- list(
#   tar_target(clean_pa, remove_overlaps(pa_data)),
#   tar_target(lup_clipped, intersect_pa(lup_data, bcmaps::bc_bound_hres()))
# )


# extras <- list(
#
#
#   # Land Use & Protected Areas
#   tar_target(pa_data, get_cpcad_bc_data()), # ppa and oecm, function will pull data from website
#   tar_target(des_lands_vis, designated_lands_vis()), # designated lands, overlapping
#   tar_target(des_lands, designated_lands()), # designated lands, flat
#   tar_target(lup_data, lup_boundaries()), # land use planning boundaries
#   tar_target(future_cons, ralcp()), # potential future conservation areas, IPCAs, etc.
#
#   # Industrial Activities
#
#   #forestry
#   tar_target(cutblock_data, forest_cutblock()), # active/pending/retired cutblock
#   tar_target(harv_auth, harvest_authority()),
#   tar_target(managed_license, forest_license()),
#   tar_target(tfl_data, tree_farm_license()),
#   tar_target(tsa_area, tsa()),
#   #mining
#   tar_target(mining_active, active_min_data()),
#   tar_target(mining_tenure, potential_min_data()),
#   #land tenures
#   tar_target(land_tenure, land_tenures()),
#   #environmental assessment office
#   tar_target(eao_points, eao_data())
#
#
#
#
# )
