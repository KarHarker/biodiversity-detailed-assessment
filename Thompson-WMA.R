source('packages.R')
conflict_prefer("filter", "dplyr")
library(stringr)


tar_load(c(th_wma_data, ch_flat))

th_wma_sum <- th_wma_data %>%
  mutate(wma_area = st_area(.),
         wma_area = as.numeric(set_units(wma_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(ppa_name) %>%
  summarise(wma_area = sum(wma_area))

wma_sum <- wma_og %>%
  mutate(og_area = st_area(.),
         og_area = as.numeric(set_units(og_area, ha))) %>%
  st_drop_geometry() %>%
  group_by(ppa_name) %>%
  summarise(og_area = sum(og_area)) %>%
  left_join(th_wma_sum)



wma_des_sum <- wma_des %>%
  mutate(des_area = st_area(.),
         des_area = as.numeric(set_units(des_area, ha))) %>%
  filter(designations == str_detect(c('wha_no_harvest', 'wha_conditional_harvest', 'fsw')))


write_sf(wma_og, "wma_og.gpkg")
