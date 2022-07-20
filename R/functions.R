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


