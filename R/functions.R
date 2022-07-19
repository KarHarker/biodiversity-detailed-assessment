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


# Loading data functions --------------------------------------------------


# all old growth - 10 million layer
og_load <- function(){

  og <- bcdc_get_data("WHSE_FOREST_VEGETATION.OGSR_TAP_OG_FORESTS_SP") %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  og

}

# priority old growth - 2.6 million layer

priority_og <- function(){
  og <- bcdc_get_data("WHSE_FOREST_VEGETATION.OGSR_TAP_PRIORITY_DEF_AREA_SP") %>%
    rename_all(tolower) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  og
}


# Protected Area/OECM Dataset

get_cpcad_bc_data <- function() {
  f <- "CPCAD-BDCAPC_Dec2020.gdb.zip"
  ff <- file.path("data", str_remove(f, ".zip"))
  if(!dir.exists(ff)){
    download.file(file.path("https://cws-scf.ca", f), destfile = f)
    unzip(f, exdir = "data")
    unlink(f)
  }

  pa <- st_read(ff, layer = "CPCAD_Dec2020") %>%
    rename_all(tolower) %>%
    dplyr::filter(str_detect(loc_e, "British Columbia")) %>%
    dplyr::filter(!(aichi_t11 == "No" & oecm == "No")) %>%
    dplyr::filter(biome == "T") %>%
    mutate(pa_type = ifelse(oecm == "No", "ppa", "oecm")) %>%
    st_make_valid() %>%
    st_transform(st_crs(3005)) %>%
    mutate(area_all = as.numeric(st_area(.))) %>%
    st_cast(to = "POLYGON", warn = FALSE)
  pa
}


# Critical Habitat - all data, will have overlaps - use for visualization, not for area calculations

ch_data_load <- function(){
  ch <- st_read("data/CriticalHabitat.gdb", layer='CriticalHabitats_0826', crs=3005)

  ch_area <- ch %>%
    rename_all(tolower) %>%
    st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  ch_area
}

# Critical Habitat - flat, data planarized - use for accurate area calcuations

ch_data_flat <- function(){
  ch <- st_read("data/CriticalHabitat.gdb", layer='critical_flat', crs=3005)

  ch_area <- ch %>%
    rename_all(tolower) %>%
    st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  ch_area

}

# designated lands dataset - overlapping
designated_lands_vis <- function(){
  output<- st_read("data/designatedlands.gpkg", layer='designations_overlapping', crs=3005) %>%
    rename_all(tolower) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  output
}

# designated lands dataset - flat
designated_lands <- function(){
  output<- st_read("data/designatedlands.gpkg", layer='designations_planarized', crs=3005) %>%
    rename_all(tolower) %>%
    mutate(sum = forest_restriction_max + mine_restriction_max + og_restriction_max) %>%
    filter(sum > 0) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  output

}

lup_boundaries <- function(){
  lup <- st_read("data/lup_combined.gpkg", layer='lup_combined', crs=3005)

  lup <- lup%>%
    st_zm() %>%
    rename_all(tolower) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  lup
}

ralcp <- function(){
  ralcp <- st_read("data/CONFIDENTIAL_Draft_MasterAreasOfConservationInterest_Apr5.gdb",
                   layer='Areas_of_Conservation_Interest_Projects', crs=3005) %>%
    rename_all(tolower) %>%
    st_cast(to = "MULTIPOLYGON", warn = FALSE) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  ralcp
}


# Industrial Activities ---------------------------------------------------

# Forestry Layers

# active/pending/retired cutblocks
forest_cutblock <- function(){
  forestry_cutblock_data <- bcdc_get_data("WHSE_FOREST_TENURE.FTEN_CUT_BLOCK_POLY_SVW")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  forestry_cutblock_data
}

# harvest authority
harvest_authority <- function(){
  harv_auth_current <- bcdc_get_data("WHSE_FOREST_TENURE.FTEN_HARVEST_AUTH_POLY_SVW")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  harv_auth_current
}

# managed licenses
forest_license <- function(){
  forestry <- bcdc_get_data("WHSE_FOREST_TENURE.FTEN_MANAGED_LICENCE_POLY_SVW")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  forestry
}

# tree farm licenses
tree_farm_license <- function(){
  tfl <- bcdc_get_data("WHSE_ADMIN_BOUNDARIES.FADM_TFL_ALL_SP")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  tfl
}

# timber supply areas
tsa <- function(){
  tsa <- bcdc_get_data("WHSE_ADMIN_BOUNDARIES.FADM_TSA")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  tsa
}


## add in tfls

active_min_data <- function(){
  mining_data_current <- bcdc_get_data("WHSE_MINERAL_TENURE.HSP_MJR_MINES_PERMTTD_AREAS_SP")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  mining_data_current
}

potential_min_data <- function(){
  mining_tenures_current <- st_read("data/BCGW_7113060B_1635183826695_2660/MTA_ACQUIRED_TENURE_GOV_SVW/MTAACQTENG_polygon.shp",
                            crs=3005) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  mining_tenures_current
}


land_tenures <- function(){
  l_tenures <- bcdc_get_data("WHSE_TANTALIS.TA_CROWN_TENURES_SVW")%>%
    st_make_valid()
  l_tenures
}

eao_data <- function(){
  eao <- st_read("data/EAO_Points/EPIC_PROJECT_POINTS/EPIC_POINT_point.shp",
                 crs=3005) %>%
    st_make_valid() %>%
    st_cast(to = "POINT", warn = FALSE)
  eao
}


cdc <- function(){
  cdc_data <- bcdc_get_data("WHSE_TERRESTRIAL_ECOLOGY.BIOT_OCCR_NON_SENS_AREA_SVW")%>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  cdc_data
}

# Site of interest - spatial file
th_wma <- function(){
  wma <- st_read("data/ThompsonNewWMAs.shp", crs=3005) %>%
    rename_all(tolower) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  wma
}



# Intersections with wha and ogma data to add dates -----------------------------------------

fill_in_dates <- function(data, column, join, landtype, output){
  output <- data %>%
    select(all_of(column)) %>%
    dplyr::filter(!is.na(column)) %>%
    st_cast(to = "POLYGON", warn = FALSE) %>%
    st_join(
      dplyr::filter(join, name_e == landtype) %>%
        tibble::rownames_to_column(), .
    ) %>%
    group_by(rowname) %>%
    slice_max(column, with_ties = FALSE)
  output
}

# Clean up data ------------------------------------

clean_up_dates <- function(data, input1, input2, output){
  output <- data %>%
    dplyr::filter(!name_e %in% c("Wildlife Habitat Areas",
                                 "Old Growth Management Areas (Mapped Legal)")) %>%
    bind_rows(input1, input2)

  output <- output %>%
    mutate(
      date = case_when(!is.na(protdate) ~ protdate,
                       !is.na(approval_date) ~ as.integer(year(approval_date)),
                       !is.na(legalization_frpa_date) ~ as.integer(year(legalization_frpa_date)),
                       name_e == "Lazo Marsh-North East Comox Wildlife Management Area" ~ 2001L,
                       name_e == "S'Amunu Wildlife Management Area" ~ 2018L,
                       name_e == "Swan Lake Wildlife Management Area" ~ 2018L,
                       name_e == "Mctaggart-Cowan/Nsek'Iniw'T Wildlife Management Area" ~ 2013L,
                       name_e == "Sea To Sky Wildland Zones" ~ 2011L),
      iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV",
                                             "V", "VI", "Yes", "N/A")),
      name_e = str_replace(name_e, "Widllife", "Wildlife"),
      park_type = if_else(oecm == "Yes", "OECM", "PPA")) %>%
    arrange(desc(oecm), iucn_cat, date, area_all) %>%
    st_cast() %>%
    st_cast(to="POLYGON", warn = FALSE)
  output
}

remove_overlaps <- function(data, sample = NULL ){
  if (!is.null(sample)) {
    rows <- sample(nrow(data), sample, replace = FALSE)
    data <- data[rows, ]
  }
  output <- data %>%
    mutate(area_single = as.numeric(st_area(.)), # Calculate indiv area
           iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV",
                                                  "V", "VI", "Yes", "N/A"))) %>%
    arrange(desc(oecm), iucn_cat, desc(area_single)) %>%
    st_cast() %>%
    st_cast(to="POLYGON", warn = FALSE) %>%
    st_make_valid() %>%
    st_difference() %>%                             # Remove overlaps (~45min)
    st_make_valid()        # Fix Self-intersections (again!)
  write_rds(output, "data/CPCAD_Dec2020_BC_clean_no_ovlps.rds") #save to disk for date checks
  output
}

ch_data_agg <- function(data){

  output <- data %>%
    aggregate(by="common_name_english", FUN= sum, na.rm=TRUE) %>%
    group_by(common_name_english) %>%
    summarise()
  output
}



# intersect data ----------------------------------------------------------

clip_to_bc_boundary <- function(data, simplify = FALSE){# Clip BEC to BC outline ---
  bc <- bc_bound_hres(ask = FALSE)
  geojson_write(data, file = "data/og.geojson")
  geojson_write(bc, file = "data/bc.geojson")

  outfile <- "data/og_clipped.geojson"
  system(glue("mapshaper-xl data/og.geojson ",
              "-clip data/bc.geojson remove-slivers ",
              "-o ", outfile))

  if (simplify) {
    outfile <- "data/og_clipped_simp.geojson"
    system(glue("mapshaper-xl data/og_clipped.geojson ",
                "-simplify 50% keep-shapes ",
                "-o ", outfile))
  }

  output <- st_read(outfile, crs=3005)%>% # geojson doesn't have CRS so have to remind R that CRS is BC Albers
    st_make_valid() %>%
    st_cast() %>%
    st_cast(to="POLYGON", warn = FALSE)
  output
}


intersect_pa <- function(input1, input2){
  if (!sf:::is_overlayng()) {
    # Setting precision of inputs if OverlayNG
    # is not enabled (sf built with GEOS < 3.9)
    # should speed it up a lot
    sf::st_precision(input1) <- 1e8
    sf::st_precision(input2) <- 1e8
  }
  input1 <- st_make_valid(input1)
  input2 <- st_make_valid(input2)
  output <- st_intersection(input1, input2) %>%
    st_make_valid() %>%
    st_collection_extract(type = "POLYGON") %>%
    mutate(polygon_id = seq_len(nrow(.)))
  output
}

intersect_point_to_poly <- function(input1, input2){

  input1 <- st_make_valid(input1)
  input2 <- st_make_valid(input2)
  output <- st_intersection(input1, input2) %>%
    st_make_valid()

  output
}


spatial_join <- function(input1, input2){

  input1 <- st_make_valid(input1)
  input2 <- st_make_valid(input2)
  output <- st_join(input1, input2, left=TRUE) %>%
    st_make_valid()

  output
}


remove_pa <- function(data1, data2){
  output <- st_difference(data1, data2) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE) %>%
    st_make_valid()
  output
}

merge_areas <- function(prot_data, og_data){
  output<- st_union(prot_data, og_data) %>%
    st_make_valid() %>%
    st_cast(to = "POLYGON", warn = FALSE)
  output
}

pa_by_type <- function(data){
  output <- data %>%
    group_by(pa_type) %>%
    summarise(sum=sum(area_single)) %>%
    st_cast() %>%
    st_make_valid()
  output
}

# summarize data ----------------------------------------------------------

measure_og_watershed_area <- function(data1, data2, prot_data){

  data2 <-og_watershed %>%
    mutate(og_watershed_area = st_area(.),
           og_watershed_area =as.numeric(set_units(og_watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(cw_name, cw_source_name, cw_source_type, source_category,
           source_comments, water_treatment_type, water_treatment_location) %>%
    summarise(og_watershed_area = sum(og_watershed_area)) %>%
    ungroup()

  prot_data <- prot_watershed %>%
    mutate(prot_og_watershed_area = st_area(.),
           prot_og_watershed_area =as.numeric(set_units(prot_og_watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(cw_name, cw_source_name, cw_source_type, source_category,
             source_comments, water_treatment_type, water_treatment_location) %>%
    summarise(prot_og_watershed_area = sum(prot_og_watershed_area)) %>%
    ungroup()

  output <- watershed_data %>%
    mutate(watershed_area = st_area(.),
           watershed_area=as.numeric(set_units(watershed_area, ha))) %>%
    st_set_geometry(NULL) %>%
    left_join(data2) %>%
    left_join(prot_data) %>%
    mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
    mutate(og_percentage = round(og_watershed_area/watershed_area * 100, digits=2),
           prot_percentage = round(prot_og_watershed_area/og_watershed_area * 100, digits=2),
           at_risk_area = og_watershed_area-prot_og_watershed_area,
           perc_at_risk = round(at_risk_area/og_watershed_area * 100, digits=2))

  write.csv(output, "prot_og_community_watersheds.csv")
  output
}

water_license_analysis <- function(data, prot_data){

  water_data<- read.csv("prot_og_community_watersheds.csv") %>%
    filter(og_watershed_area>0) %>%
    select(cw_name, cw_source_name, cw_status, source_category, cw_use, water_treatment_type, water_treatment_location,
           organization_concerns, watershed_area, og_watershed_area, prot_og_watershed_area, og_percentage, prot_percentage,
           at_risk_area, perc_at_risk)

  data <-community_watersheds_licenses_test %>%
    st_set_geometry(NULL) %>%
    group_by(label, cw_name, cw_source_name, cw_source_type, cw_status, water_system_name, organization_count, organization,
             source_category,source_comments, cw_use, water_treatment_type, water_treatment_location, general_comments,
             pod_diversion_type, pod_status, purpose_use, quantity, quantity_units, quantity_flag, quantity_flag_description) %>%
    summarise(sum_quantity = sum(quantity),
              n_licenses = n()) %>%
    left_join(water_data) %>%
    ungroup()
  write.csv(data, "og_community_watersheds_water_licenses_full.csv")

  summary_by_cw_use <- tibble(data) %>%
    group_by(cw_use) %>%
    summarise(total_watershed_area = sum(watershed_area, na.rm=TRUE),
              og_watershed_area_comb = sum(og_watershed_area, na.rm=TRUE),
              prot_og_watershed_area_comb = sum(prot_og_watershed_area, na.rm=TRUE)) %>%
    mutate(at_risk_area = og_watershed_area_comb - prot_og_watershed_area_comb)

  write.csv(summary_by_cw_use, "summary_of_og_area_by_cw_use.csv")


  summary_of_water_allocation <- data %>%
    filter(purpose_use=="00A - Waterworks: Local Provider "| purpose_use== "03A - Irrigation: Local Provider "|
           purpose_use== "01A - Domestic "| purpose_use== "WSA01 - Domestic (WSA01) "|
           purpose_use=="03B - Irrigation: Private "| purpose_use=="02I31 - Livestock & Animal: Stockwatering ") %>%
    group_by(cw_name, cw_source_name, purpose_use, watershed_area, og_watershed_area, prot_og_watershed_area, og_percentage, prot_percentage,
             at_risk_area, perc_at_risk) %>%
    mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
    summarise(sum_quantity = sum(quantity),
              n_license = n())

  summary_of_water_allocation$units <- "m3/year"

  summary_of_water_allocation_all <- data %>%
    group_by(cw_name, cw_source_name, watershed_area, og_watershed_area, prot_og_watershed_area, og_percentage, prot_percentage,
             at_risk_area, perc_at_risk) %>%
    mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
    summarise(sum_quantity_all_allocations = sum(quantity),
              n_licenses_all_allocations = n())

  summary_of_water_allocation_all$units <- "m3/year"

  summary_of_water_allocation <- summary_of_water_allocation %>%
    left_join(summary_of_water_allocation_all)

  write.csv(summary_of_water_allocation, "summary_of_water_allocation_by_community_and_type.csv")
}



measure_og_ch_habitat<- function(chdata, ch_og_data){

  ch_og_data<- ch_og_data %>%
    mutate(og_species_area = st_area(.),
           og_species_area  =as.numeric(set_units(og_species_area , ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
    summarise(og_species_area = sum(og_species_area)) %>%
    ungroup()

  ch_og_prot <- ch_og_prot%>%
    mutate(og_prot_species_area=st_area(.),
           og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
    summarise(og_prot_species_area = sum(og_prot_species_area))

  output2<-chdata %>%
    mutate(species_area=st_area(.),
           species_area =as.numeric(set_units(species_area, ha)),
           species_area = replace_na(species_area,0)) %>%
    st_set_geometry(NULL) %>%
    group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
    summarise(species_area = sum(species_area)) %>%
    left_join(ch_og_data, by=c("scientific_name", "common_name_english", "cosewic_status",
                               "schedule_status", "sara_schedule")) %>%
    left_join(ch_og_prot, by=c("scientific_name", "common_name_english", "cosewic_status",
                               "schedule_status", "sara_schedule")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
    mutate(og_overall_percentage = round(og_species_area/species_area * 100, digits=2),
           #prot_percentage = round(og_prot_species_area/species_area * 100, digits=2),
           at_risk_area = og_species_area-og_prot_species_area,
           perc_at_risk = round(at_risk_area/species_area * 100, digits=2))

  write.csv(output2, "og_all-sp-critical-habitat.csv")
  output2
}

overall_og_ch_habitat<- function(data, data2, data3){
  overall_ch <- ch_data %>%
    mutate(species_area=st_area(.),
           species_area =as.numeric(set_units(species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_area = sum(species_area))

  overall_og_ch <- og_ch %>%
    mutate(og_species_area=st_area(.),
           og_species_area =as.numeric(set_units(og_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_og_species_area = sum(og_species_area))

  overall_og_ch_prot <- prot_og_ch %>%
    mutate(og_species_area_prot=st_area(.),
           og_species_area_prot =as.numeric(set_units(og_species_area_prot, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_og_species_area_prot = sum(og_species_area_prot))

  overall_faib <- faib_ch %>%
    mutate(faib_species_area=st_area(.),
           faib_species_area =as.numeric(set_units(faib_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_faib_species_area = sum(faib_species_area)) %>%
    bind_cols(overall_ch, overall_og_ch, overall_og_ch_prot) %>%
    mutate(og_ch_percentage = overall_og_species_area/overall_area*100,
           at_risk_mod = overall_faib/og_species_area*100,
           overall_unprot = (overall_og_species_area-overall_og_ch_prot)/overall_species_area*100)


  overall_ch_data

  og_ch_percentage = overall_og_ch[1,1]/overall_ch[1,1]*100
  og_ch_at_risk = (overall_og_ch_prot[1,1]/overall_og_ch[1,1])*100
}

measure_og_dependent_ch_habitat<- function(chdata, ch_og_data, ch_og_prot){

  og_scientific_name <- c("Marmota vancouverensis", "Limnanthes macounii",
                          "Aegolius acadicus brooksi", "Cephalanthera austiniae",
                          "Hemphillia dromedarius", "Prophysaon coeruleum",
                          "Sphyrapicus thyroideus nataliae", "Brachyramphus marmoratus",
                          "Collema coniophilum", "Melanerpes lewis", "Rangifer tarandus",
                          "Accipiter gentilis laingi"
                          )

  ch_og_data<- ch_og_data %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(og_species_area = st_area(.),
           og_species_area =as.numeric(set_units(og_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
    summarise(og_species_area = sum(og_species_area)) %>%
    ungroup()

  ch_og_prot <- ch_og_prot%>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(og_prot_species_area=st_area(.),
           og_prot_species_area =as.numeric(set_units(og_prot_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
    summarise(og_prot_species_area = sum(og_prot_species_area))


  output2<-chdata %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(species_area=st_area(.),
           species_area =as.numeric(set_units(species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    group_by(scientific_name, common_name_english, cosewic_status, schedule_status, sara_schedule) %>%
    summarise(species_area = sum(species_area)) %>%
    left_join(ch_og_data, by=c("scientific_name", "common_name_english", "cosewic_status",
                               "schedule_status", "sara_schedule")) %>%
    left_join(ch_og_prot, by=c("scientific_name", "common_name_english", "cosewic_status",
                               "schedule_status", "sara_schedule"))%>%
    mutate_if(is.numeric, ~replace(., is.na(.),0)) %>%
    mutate(og_overall_percentage = round(og_species_area/species_area * 100, digits=2),
           #prot_percentage = round(og_prot_species_area/species_area * 100, digits=2),
           at_risk_area = og_species_area-og_prot_species_area,
           perc_at_risk = round(at_risk_area/species_area * 100, digits=2))

  write.csv(output2, "og-dependent_critical-habitat.csv")
  output2
}

overall_og_dependent_ch_habitat<- function(data, data2, data3){

  og_scientific_name <- c("Marmota vancouverensis", "Limnanthes macounii",
                          "Aegolius acadicus brooksi", "Cephalanthera austiniae",
                          "Hemphillia dromedarius", "Prophysaon coeruleum",
                          "Sphyrapicus thyroideus nataliae", "Brachyramphus marmoratus",
                          "Collema coniophilum", "Melanerpes lewis", "Rangifer tarandus",
                          "Accipiter gentilis laingi"
  )

  overall_ch <- overall_ch %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(species_area=st_area(.),
           species_area =as.numeric(set_units(species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_area = sum(species_area))

  overall_og_ch <- overall_og_ch %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(og_species_area=st_area(.),
           og_species_area =as.numeric(set_units(og_species_area, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_og_ch_area = sum(og_species_area))

  overall_og_ch_prot <- overall_og_ch_prot %>%
    filter(scientific_name %in% og_scientific_name) %>%
    mutate(og_species_area_prot=st_area(.),
           og_species_area_prot =as.numeric(set_units(og_species_area_prot, ha))) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_og_ch_area_prot = sum(og_species_area_prot))

  og_ch_percentage = (overall_og_ch[1,1]-overall_og_ch_prot[1,1])/overall_ch[1,1]*100
}

overall_pa_og <- function(data){
  pa_og_sum <- pa_og %>%
    mutate(pa_og_area=st_area(.),
           pa_og_area=set_units(pa_og_area, ha)) %>%
    st_set_geometry(NULL) %>%
    summarise(pa_og_area = sum(pa_og_area))
}

overall_pa_og_tech <- function(data){
  pa_og_sum_tech <- pa_og_tech %>%
    mutate(pa_og_area_tech=st_area(.),
           pa_og_area_tech=set_units(pa_og_area_tech, ha)) %>%
    st_set_geometry(NULL) %>%
    summarise(pa_og_area_tech = sum(pa_og_area_tech))
}

overall_og <- function(data){
  og <- data %>%
    mutate(og_area=st_area(.),
           og_area = set_units(og_area, ha)) %>%
    st_set_geometry(NULL) %>%
    summarise(overall_area = sum(og_area))
}



# plot data ----------------------------------------------------------

plot_ch_og_data <- function(data){

  schedule_scale= c("Endangered"= "red", "Threatened"="orange", "Special Concern"="blue", "No Status"="grey")
  output2<-read.csv("og_all-sp-critical-habitat.csv")

  output2<- output2 %>%
    filter(og_species_area>0)

  bar1 <- ggplot(output2,
                 aes(x = as.numeric(perc_at_risk), y = fct_reorder(common_name_english, perc_at_risk, .desc=FALSE),
                     fill = as.factor(schedule_status))) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(0.7, 0.3),
          legend.title=element_blank()) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "Percent of Critical Habitat Located in Mapped Old-Growth (%)", y = "Species Common Name") +
    scale_fill_manual(values = schedule_scale) +
    scale_x_continuous(expand = c(0,0))
  ggsave("out/og_ch-all-species-bar.png", bar1, width = 10, height = 6, dpi = 300)
  bar1
}


plot_ch_og_dependent <- function(data){

  schedule_scale_og = c("Endangered"= "red", "Threatened"="orange", "Special Concern"="blue")
  output2<- read.csv("og-dependent_critical-habitat.csv")

  output2<- output2 %>%
    arrange(desc(Overall)) %>%
    tibble::rowid_to_column("ID") %>%
    select(-Overall) %>%
    pivot_longer(cols=c(AtRisk, Protected), names_to = "Status", values_to = "Percentage") %>%
    mutate(Status = case_when(Status == "AtRisk" ~ "At Risk",
                              Status == "Protected" ~ "Protected"))

  bar1 <- ggplot(output2,
                 aes(x = as.numeric(Percentage),
                     fct_reorder(common_name_english, ID, .desc=TRUE),
                     alpha = Status,
                     fill = as.factor(schedule_status))) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(0.7, 0.3),
          legend.title=element_blank()) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "Percent of Critical Habitat Located in Old-Growth Areas (%)",
         y = "Old Growth Dependent Species Common Name") +
    scale_fill_manual(values = schedule_scale_og) +
    scale_alpha_manual(name = "Protected Status", values = c("Protected"=1, "At Risk"=0.5))+
    scale_x_continuous(expand = c(0,0))
  ggsave("out/og-dependent_ch-bar-1.png", bar1, width = 10, height = 6, dpi = 300)
  bar1
}

plot_community_watersheds<- function(data){
  output <- og_watersheds %>%
    filter(cw_use=="PRIMARY" | cw_use=="PRIMARY - GUDI in CW" | cw_use=="YES"| cw_use == "FUTURE") %>%
    filter(!is.na(og_percentage))

  scatterplot <- ggplot(output, aes(x=as.numeric(watershed_area), y= as.numeric(og_watershed_area)))+
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          legend.title=element_blank()) +
    geom_point(size=2, aes(color=natural_resource_district))+
    #ggrepel::geom_text_repel()+
    #theme(legend.position = "none") +
    #scale_color_manual(values = bec_colours(), guide = FALSE) +
    labs(x = "Watershed Area (m2)", y = "Area of Mapped Old Growth within Watershed (m2)")
  ggsave("out/og_watershed_scatter.png", scatterplot, width = 10, height = 6, dpi = 300)

}

carbon_analysis_coastal <- function(data, data_prot){

  bec <- read.csv("BGC_Catalogue_BECv12_OldGrowth_Targets.csv") %>%
    rename_all(tolower) %>%
    filter(bec_natural_disturbance_code=="NDT1" | bec_natural_disturbance_code=="NDT2") %>%
    unite(label_and_disturbance, c("map_label", "bec_natural_disturbance_code"), remove=FALSE)

  bec_og_wet<- bec$label_and_disturbance

  data<- data %>%
    unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
    filter(label_and_disturbance %in% bec_og_wet) %>%
    mutate(bec_wet_og_area = st_area(.),
           bec_wet_og_area  =as.numeric(set_units(bec_wet_og_area , ha))) %>%
    st_set_geometry(NULL) %>%
    rename_all(tolower) %>%
    group_by(label_and_disturbance, zone, subzone, variant, natural_disturbance,
             map_label, bgc_label, zone_name) %>%
    summarise(bec_wet_og_area = sum(bec_wet_og_area)) %>%
    ungroup()

  bec_output<- data_prot %>%
    unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
    filter(label_and_disturbance %in% bec_og_wet) %>%
    mutate(bec_wet_og_area_prot = st_area(.),
           bec_wet_og_area_prot  =as.numeric(set_units(bec_wet_og_area_prot, ha))) %>%
    st_set_geometry(NULL) %>%
    rename_all(tolower) %>%
    group_by(label_and_disturbance, zone, subzone, variant, natural_disturbance,
             map_label, bgc_label, zone_name) %>%
    summarise(bec_wet_og_area_prot = sum(bec_wet_og_area_prot)) %>%
    ungroup() %>%
    left_join(data) %>%
    mutate(at_risk_og = bec_wet_og_area - bec_wet_og_area_prot)

  bec_output
}


carbon_analysis_interior <- function(data, data_prot){

  bec <- read.csv("BGC_Catalogue_BECv12_OldGrowth_Targets.csv") %>%
    rename_all(tolower) %>%
    filter(bec_natural_disturbance_code=="NDT3" | bec_natural_disturbance_code=="NDT4") %>%
    unite(label_and_disturbance, c("map_label", "bec_natural_disturbance_code"), remove=FALSE)

  bec_og_dry <- bec$label_and_disturbance

  data<- data %>%
    unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
    filter(label_and_disturbance %in% bec_og_dry) %>%
    mutate(bec_dry_og_area = st_area(.),
           bec_dry_og_area  =as.numeric(set_units(bec_dry_og_area , ha))) %>%
    st_set_geometry(NULL) %>%
    rename_all(tolower) %>%
    group_by(label_and_disturbance, zone, subzone, variant, natural_disturbance,
             map_label, bgc_label, zone_name) %>%
    summarise(bec_dry_og_area = sum(bec_dry_og_area)) %>%
    ungroup()

  bec_output<- data_prot %>%
    unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
    filter(label_and_disturbance %in% bec_og_dry) %>%
    mutate(bec_dry_og_area_prot = st_area(.),
           bec_dry_og_area_prot  =as.numeric(set_units(bec_dry_og_area_prot, ha))) %>%
    st_set_geometry(NULL) %>%
    rename_all(tolower) %>%
    group_by(label_and_disturbance, zone, subzone, variant, natural_disturbance,
             map_label, bgc_label, zone_name) %>%
    summarise(bec_dry_og_area_prot = sum(bec_dry_og_area_prot)) %>%
    ungroup() %>%
    left_join(data) %>%
    mutate(at_risk_og = bec_dry_og_area - bec_dry_og_area_prot)
  bec_output

}


df_analysis <- function(df1, df2, df1_prot, df2_prot){

  bec <- read.csv("BGC_Catalogue_BECv12_OldGrowth_Targets.csv") %>%
    rename_all(tolower) %>%
    filter(bec_natural_disturbance_code=="NDT1" | bec_natural_disturbance_code=="NDT2") %>%
    unite(label_and_disturbance, c("map_label", "bec_natural_disturbance_code"), remove=FALSE)

  bec_og_wet<- bec$label_and_disturbance

    df1<- df1 %>%
      unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
      filter(label_and_disturbance %in% bec_og_wet) %>%
      mutate(df1_area = st_area(.),
             df1_area  =as.numeric(set_units(df1_area, ha))) %>%
      st_set_geometry(NULL) %>%
      rename_all(tolower) %>%
      summarise(sum = sum(df1_area)) %>%
      mutate(eco="Douglas Fir Leading",
             type="OG - Total")


    df2<- df2 %>%
      unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
      filter(label_and_disturbance %in% bec_og_wet) %>%
      mutate(df2_area = st_area(.),
             df2_area  =as.numeric(set_units(df2_area, ha))) %>%
      st_set_geometry(NULL) %>%
      rename_all(tolower) %>%
      summarise(sum = sum(df2_area)) %>%
      mutate(eco="Douglas Fir Second",
             type="OG - Total") %>%
      ungroup()

    df1_prot<- df1_prot %>%
      unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
      filter(label_and_disturbance %in% bec_og_wet) %>%
      mutate(df1_area_prot = st_area(.),
             df1_area_prot  =as.numeric(set_units(df1_area_prot, ha))) %>%
      st_set_geometry(NULL) %>%
      rename_all(tolower) %>%
      summarise(sum = sum(df1_area_prot)) %>%
      mutate(eco="Douglas Fir Leading",
             type="OG - Protected Area") %>%
      ungroup()

    df2_prot<- df2_prot %>%
      unite(label_and_disturbance, c("MAP_LABEL", "NATURAL_DISTURBANCE"), remove=FALSE) %>%
      filter(label_and_disturbance %in% bec_og_wet) %>%
      mutate(df2_area_prot = st_area(.),
             df2_area_prot  =as.numeric(set_units(df2_area_prot, ha))) %>%
      st_set_geometry(NULL) %>%
      rename_all(tolower) %>%
      summarise(sum = sum(df2_area_prot)) %>%
      mutate(eco="Douglas Fir Second",
             type="OG - Protected Area") %>%
      ungroup()

      overall<- bind_rows(df1, df2, df1_prot, df2_prot)
}




group_eco_bec_to_multi <- function(eco_bec) {
  eco_bec %>%
    mutate(eco_var_area = st_area(.)) %>%
    group_by(ecoregion_code, ecoregion_name, zone, subzone, variant) %>%
    summarise(tot_area = as.numeric(sum(eco_var_area)), is_coverage = TRUE)
}

group_pa_eco_bec_to_multi <- function(pa_eco_bec) {
  pa_eco_bec %>%
    mutate(pa_area = st_area(.)) %>%
    group_by(ecoregion_code, ecoregion_name, zone, subzone, variant, pa_type) %>%
    summarise(pa_area = as.numeric(sum(pa_area)))
}

group_pa_bec_to_multi <- function(pa_eco_bec) {
  pa_eco_bec %>%
    mutate(pa_area = st_area(.)) %>%
    group_by(zone, subzone, variant, pa_type) %>%
    summarise(pa_area = as.numeric(sum(pa_area)))
}

# Simplify spatial data for visualization---------------------------------------------------

# Run by region/zone
#  - Much faster and no crashing (on my computer at least)
#  - Allows simplifying to different degrees for different regions

simplify_ecoregions<- function(data){# Simplify ecoregions for plotting  ---
  eco_simp <- slice(data, 0)
  for(e in unique(data$ecoregion_code)) {
    message(e)
    temp <- dplyr::filter(data, ecoregion_code == e)
    keep_shapes <- if_else(nrow(temp) <= 1000, TRUE, FALSE)
    keep <- case_when(nrow(temp) < 50 ~ 1,
                      nrow(temp) < 1000 ~ 0.1,
                      TRUE ~ 0.05)
    if(keep == 1) region <- temp else region <- ms_simplify(temp, keep = keep,
                                                            keep_shapes = keep_shapes)
    eco_simp <- rbind(eco_simp, region)
  }
  output <- dplyr::filter(eco_simp, !st_is_empty(eco_simp))
  write_rds(eco_simp, "out/CPCAD_Dec2020_eco_simp.rds")
  output
}

#' Simplify a map for plotting, optionally aggregating to MULTIPOLYGON
#'
#'
#' @param sf object
#' @param keep proportion of vertices to keep (passed to rmapshaper::ms_simplify)
#' @param agg optional character vector of columns to aggregate by
#' @param ... passed on to summarise.sf() (e.g., `do_union = FALSE` or `is_coverage = TRUE`)
#'
#' @return simplified (and possibly aggregated) version of `data`
#' @export
simplify_background_map <- function(data, keep = 0.05, agg = NULL, ...){# Simplify bec zones for plotting  ---

  output <- rmapshaper::ms_simplify(data, keep = keep, keep_shapes = TRUE, explode = TRUE, sys = TRUE) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON")

  if (!is.null(agg)) {
    output <- group_by(output, across(all_of(agg))) %>%
      summarise()
  }
  output
}

# simplify_eco_background<- function(data){# Simplify ecoregions background map ---
#   output<- ms_simplify(data, keep = 0.01)
#   write_rds(output, "out/eco_simp.rds")
#   output
# }
#
# simplify_bec_background<-function(){# Simplify bec zones background map ---
#   system(glue("mapshaper-xl data/bec_clipped.geojson ",
#               "-simplify 1% keep-shapes ",
#               "-o out/bec_simp.geojson"))
#   output<-st_read("out/bec_simp.geojson", crs=3005) # geojson doesn't have CRS so have to remind R that CRS is BC Albers
#   output
# }

# Calculate ecoregion and bec zone protected areas ------------------------

# find_ecoregion_size <- function(data) {
# # Summarize by eco region
#   output <- data %>%
#     mutate(area = as.numeric(st_area(geometry))) %>%
#     st_set_geometry(NULL) %>%
#     group_by(ecoregion_code) %>%
#     summarize(total = sum(area) / 10000, .groups = "drop")
#   output
# }

protected_area_by_eco <- function(data, eco_totals){
  eco_totals<- eco_totals %>%
    st_set_geometry(NULL)
  output <- data %>%
    mutate(total_area = as.numeric(st_area(geometry)),
           total_area = set_units(total_area, km^2)) %>%
    st_set_geometry(NULL) %>%
    group_by(ecoregion_code, ecoregion_name, type, date) %>%
    complete(park_type = c("OECM", "PPA"),
             fill = list(total_area = 0)) %>%
    ungroup() %>%
    # Add placeholder for missing dates for plots (max year plus 1)
    mutate(d_max = max(date, na.rm = TRUE),
           missing = is.na(date),
           date = if_else(is.na(date), d_max + 1L, date)) %>%
    group_by(ecoregion_code) %>%
    mutate(d_max = max(c(date, d_max))) %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type) %>%
    # Fill in missing dates all the way to max
    complete(date = seq(min(date, na.rm = TRUE), d_max[1]),
             fill = list(total_area = 0, missing = FALSE)) %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type, missing, date) %>%
    summarize(total_area = as.numeric(sum(total_area)), .groups = "drop") %>%
    group_by(ecoregion_code, ecoregion_name, park_type, type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cum_type = cumsum(total_area),
           total_type = sum(total_area)) %>%
    ungroup() %>%
    left_join(eco_totals, by = c("ecoregion_code", "ecoregion_name" ,"type")) %>%
    # Get regional values
    group_by(ecoregion_code, type) %>%
    mutate(both_park_type_sum = sum(total_area),
           p_type = total_type / total_ecoregion_by_type * 100,
           cum_year_type = cum_type / total_ecoregion_by_type * 100,
           p_region = both_park_type_sum/total_ecoregion_by_type * 100) %>%
    ungroup() %>%
    arrange(desc(type), p_type) %>%
    mutate(ecoregion_name = factor(ecoregion_name, levels = unique(ecoregion_name)))
  write_rds(output, "out/pa_eco_sum.rds")
  output
}

protected_area_by_bec_eco <- function(bec_eco_data, data){# Summarize by bec zone region
  bec_eco_totals <- bec_eco_data %>%
    mutate(eco_var_area = as.numeric(st_area(.)))
    # st_set_geometry(NULL)
    # group_by(ecoregion_name, zone, subzone, variant, phase, map_label) %>%
    # summarize(total_eco_var_area = sum(area) / 10000, .groups = "drop")


  pa_data <- data %>%
    mutate(prot_area = st_area(.)) %>%
    st_set_geometry(NULL)
    # group_by(ecoregion_name, zone, subzone, variant, pa_type) %>%
    # summarize(eco_var_prot_area = as.numeric(sum(prot_area) / 10000), .groups = "drop") %>%    # summarize(eco_var_prot_area = as.numeric(sum(prot_area) / 10000), .groups = "drop") %>%

  output <- left_join(bec_eco_totals,
                      select(pa_data, pa_type, ecoregion_name, map_label, prot_area),
                      by = c("ecoregion_name", "map_label"))
    # mutate(perc_type_eco_var = eco_var_prot_area / total_eco_var_area * 100) %>%
    # arrange(desc(perc_type_eco_var))

  # write_rds(output, "out/bec_area.rds")
  output
}

# Supplemental plots ------------------------------------------------------

plot_by_bec_zone <- function(data){
  bar1 <- ggplot(data,
                 aes(x = perc_type_zone, y = zone_name, fill = zone, alpha = park_type)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(0.7, 0.3)) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "Percent Area Conserved (%)", y = "Biogeoclimatic Zone") +
    scale_fill_manual(values = bec_colours(), guide = FALSE) +
    scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
    scale_x_continuous(expand = c(0,0)) +
    guides(alpha = guide_legend(override.aes = list(fill = "black")))
  ggsave("out/bec_bar1.png", bar1, width = 6, height = 6, dpi = 300)
  bar1
}

plot_bec_zone_totals<- function(data, data2){

  bec_totals <- data %>%
    dplyr::filter(park_type == "PPA") %>%
    mutate(total_bc = bcmaps::bc_area()) %>%
    mutate(bec_rep = total/total_bc) %>%
    select(zone, zone_name, perc_zone, total, total_bc, bec_rep) %>%
    arrange(desc(perc_zone))

  scatterplot <- ggplot(bec_totals, aes(x=bec_rep, y= perc_zone, label= zone_name))+
    theme_minimal(base_size = 14) +
    #theme(panel.grid.major.y = element_blank()) +
    geom_point(size=2, aes(color=zone))+
    ggrepel::geom_text_repel()+
    theme(legend.position = "none") +
    scale_color_manual(values = bec_colours(), guide = FALSE) +
    labs(x = "BEC Zone Composition (%)", y = "Percentage of BEC Zone Conserved (%)")
  ggsave("out/bec_scatter.png", scatterplot, width = 6, height = 6, dpi = 300)
  write_rds(scatterplot, "out/bec_scatter.rds")

  map<-ggplot() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 15)) +
    geom_sf(data = data2, aes(fill = zone), colour = NA)+
    geom_sf(data = bc_bound_hres(), aes(fill=NA))+
    scale_fill_manual(values = bec_colours()) +
    theme(legend.title=element_blank()) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "BEC Zones in B.C.")
  ggsave("out/bec_map.png", map, width = 11, height = 10, dpi = 300)
  map

  combined <- plot_grid(map, scatterplot, ncol=1, align="v", rel_heights=c(1.25,1))

  ggsave("out/bec_comb.png", combined, width = 8, height = 10, dpi = 300)
  combined

}

create_bc_button <- function(){
  output <- bc_bound() %>%
    st_geometry() %>%
    ms_simplify(0.02, explode = TRUE, keep_shapes = FALSE) %>%
    ggplot() +
    theme_void() +
    ggiraph::geom_sf_interactive(fill = "black", data_id = "reset")
  write_rds(output, "out/bc_button.rds")
  output
}

bc_map <- function(data){

  ld_cities <- bcmaps::bc_cities() %>%
    dplyr::filter(NAME == "Victoria" |
                    NAME == "Prince Rupert"|
                    NAME == "Smithers"|
                    NAME == "Fort St. John"|
                    NAME == "Kamloops"|
                    NAME == "Prince George"|
                    NAME == "Vancouver"|
                    NAME == "Cranbrook")%>%
    dplyr::select(NAME, geometry)

  #manually setting label location
  ld_cities$longitude <- c(925299, 627354, 1205857, 1295775, 1399598, 1741864, 1270416, 1245673)
  ld_cities$latitude <- c(1069703, 1050342, 979165, 1241672, 626000, 570917, 435953, 380451)

  scale_land <- c("OECM" = "#74c476", "PPA" = "#006d2c")
  scale_water <- c("OECM" = "#43a2ca", "PPA" = "#0868ac")
  scale_combo <- setNames(c(scale_land, scale_water),
                          c("Land - OECM", "Land - PPA",
                            "Water - OECM", "Water - PPA"))
  output <- data %>%
    mutate(type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
           type_combo = factor(type_combo,
                               levels = c("Land - OECM", "Land - PPA",
                                          "Water - OECM", "Water - PPA"))) %>%
    group_by(date, type) %>%
    ungroup()

  map<-ggplot() +
    theme_void() +
    theme(plot.title = element_text(hjust =0.5, size = 25)) +
    geom_sf(data = output, aes(fill = type_combo), colour = NA)+
    geom_sf(data = bc_bound_hres(), aes(fill=NA))+
    geom_sf(data=ld_cities)+
    geom_text(data=ld_cities, aes(x=longitude, y=latitude, label=NAME))+
    scale_fill_manual(values = scale_combo) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Distribution of Conserved Areas in B.C.") +
    theme(legend.title=element_blank())+
    theme(legend.justification=c("center"),
          legend.position=c(0.9, 0.6))
  ggsave("out/prov_map.png", map, width = 11, height = 10, dpi = 300)
  map
}

eco_static <- function(data, input){

  input <- input %>%
    dplyr::filter(park_type == "PPA") %>%
    group_by(ecoregion_name, ecoregion_code, type) %>%
    dplyr::filter(date == 2020) %>%
    select(ecoregion_name, ecoregion_code, type, p_region)


  #data <- cbind(data, st_coordinates(st_centroid(data)))
  label <- data %>%
    group_by(ecoregion_name, ecoregion_code) %>%
    slice_max(total_ecoregion_by_type) %>%
    ungroup()

  data <- data %>%
    mutate(ecoregion_name = as.factor(ecoregion_name),
           type=as.factor(type)) %>%
    left_join(input, by = c("ecoregion_name", "ecoregion_code", "type"))


  scale_map <- c("land" = "#056100", "water" = "#0a7bd1")

  g <- ggplot(data) +
    theme_void() +
    geom_sf(data=data, mapping=aes(fill = type, alpha = p_region), size = 0.1, colour = "black")+
    geom_sf(data=bc_bound_hres(), mapping=aes(fill=NA))+
    theme(plot.margin = unit(c(0,0,0,0), "pt")) +
    #geom_text(data=data, aes(X, Y, label=ecoregion_name))+
    #geom_sf_text_repel(aes(label=ecoregion_name))+
    ggrepel::geom_text_repel(data=label, aes(label=ecoregion_name, geometry=geometry),
                             stat="sf_coordinates",
                             min.segment.length=0)+
    scale_fill_manual(values = scale_map, guide=NULL) +
    scale_alpha_continuous(range = c(0.25, 1), n.breaks = 5, limits = c(0, 100), name="% Conserved") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Area Conserved by Ecoregion") +
    theme(plot.title = element_text(hjust=0.5, size = 25)) +
    theme(legend.justification=c("center"),
          legend.position=c(0.9, 0.6))+
    guides(alpha = guide_legend(override.aes = list(fill = "black")))#+
  #guides(alpha = guide_legend(override.aes = list(fill = scale_map["water"])))
  ggsave("out/ecoregion_map.png", g, width = 11, height = 10, dpi = 300)
  g
}

eco_bar <- function(data){

  data <- data %>%
    group_by(ecoregion_name, ecoregion_code, type, park_type) %>%
    dplyr::filter(date == 2020) %>%
    select(ecoregion_name, ecoregion_code, type, park_type, p_type, p_region) %>%
    arrange(desc(p_type)) %>%
    mutate(type_combo = glue("{tools::toTitleCase(type)} - {park_type}"),
           type_combo = factor(type_combo,
                               levels = c("Land - OECM", "Land - PPA",
                                          "Water - OECM", "Water - PPA")),
           ecoregion_type_combo = glue("{ecoregion_name} - {tools::toTitleCase(type)}"),
           ecoregion_name = as.factor(ecoregion_name)) %>%
    ungroup()

  scale_land <- c("OECM" = "#93c288", "PPA" = "#004529")
  scale_water <- c("OECM" = "#8bc3d5", "PPA" = "#063c4e")
  scale_map <- c("land" = "#056100", "water" = "#0a7bd1")
  scale_combo <- setNames(c(scale_land, scale_water),
                          c("Land - OECM", "Land - PPA",
                            "Water - OECM", "Water - PPA"))

  land <- ggplot(data=dplyr::filter(data, type=="land"),
                 aes(x = round(p_type,2), y = fct_reorder(ecoregion_name, p_region, .desc=FALSE),
                     fill = type, alpha = park_type)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(0.7, 0.3)) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(y = "Ecoregion") +
    theme(axis.title.x=element_blank())+
    scale_fill_manual(values = scale_map, guide = FALSE) +
    scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,110)) +
    guides(alpha = guide_legend(override.aes = list(fill = "black"))) #+
  land

  water <-ggplot(data=dplyr::filter(data, type=="water"),
                 aes(x = round(p_type,2), y = fct_reorder(ecoregion_name, p_region, .desc=FALSE),
                     fill = type, alpha = park_type)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(0.7, 0.5)) +
    geom_bar(width = 0.9, stat = "identity") +
    labs(x = "Percent Conserved Within Ecoregion (%)") +
    theme(axis.title.y=element_blank())+
    scale_fill_manual(values = scale_map, guide = FALSE) +
    scale_alpha_manual(name = "Type", values = c("OECM" = 0.5, "PA" = 1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,110)) +
    theme(legend.position='none')
  water

  combined <- plot_grid(land, water, ncol=1, align="v", rel_heights=c(4,1))


  ggsave("out/eco_bar_all.png", combined, width = 9, height = 9, dpi = 300)
  combined
}

write_csv_data <- function(x, dir = "out/data_summaries") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  nm <- deparse(substitute(x))
  path <- file.path(dir, paste0(nm, ".csv"))
  readr::write_csv(x, path)
  path
}

# sensitivity analysis


threshold_scenario <- function(data, background, conserved, composition, prov_conserved){

    rare_variants <- pa_bec_summary_wide %>% filter(percent_comp_prov < quantile(percent_comp_prov, .1))

    output<- ggplot() +
      geom_bc +
      geom_sf(
        data = data %>%
          filter(percent_conserved_ppa < conserved,
                 (percent_comp_ecoregion > composition | bec_variant %in% rare_variants$bec_variant )),
        aes(fill = percent_conserved_ppa), colour = NA) +
      scale_fill_viridis_c() +
      labs(title = "Underrepresented BEC variants x Ecoregions\n in B.C. Parks and Protected Areas",
           caption = paste0("Ecoregions*Variants with <", conserved, "% protected,\nwhere the variant makes up
                            at least ", composition, "% of an ecoregion\nor is provincially rare (in the bottom ",
                            prov_conserved,"% of variants)"),
           fill = "Percent protected") +
      theme_minimal()

    ggsave(paste0("out/eco_rep_", conserved, "_", composition, "_", prov.conserved, ".png"),
           output, width = 9, height = 9, dpi = 300)
    output
}


scenario_output<- function(data, range_no){

  rare_variants <- pa_bec_summary_wide %>% filter(percent_comp_prov < quantile(percent_comp_prov, .1))

  output <- lapply(range_no, scenario_test)

  scenario_test<- function(range_no){
                   output<- data %>%
    filter(percent_conserved_ppa < range_no,
           (percent_comp_ecoregion > 3 | bec_variant %in% rare_variants$bec_variant )) %>%
    mutate(eco_var_area = as.numeric(st_area(.))) %>%
    st_set_geometry(NULL) %>%
    group_by(ecoregion, bec_variant) %>%
    mutate(sum_eco_var = summarise(eco_var_area)) %>%
    ungroup() %>%
    group_by(ecoregion) %>%
    mutate(n_variants = unique(bec_variant),
           sum_eco = summarise(eco_var_area)) %>%
    ungroup() %>%
    mutate(scenario_sum=sum(eco_var_area))
  }

  output
}



