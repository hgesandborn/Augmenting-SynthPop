# =========================================================
# Title: College dormitories group quarters generation
# Author: Paul Delamater
# Date: 2025-10-28
# Description: Estimate college dormitories group quarters population
# =========================================================


##### Run round_preserve_sum.R before running this script ######


##### Load packages #####
## If necessary, use install.packages() to install packages
library(tidyverse)
library(magrittr)
library(sf)
library(mapview)


##### Set seed for reproducibility #####
set.seed(100)


##### Set file paths #####
data_path <- "data/input/"
intermediate_path <- "data/intermediate/"
output_path <- "output/results/"



###
### Block group GQ data ----
###

### Read in the Census table with detailed counts by block
gq_age <- read_csv(file.path(data_path, "NC_block_groupquarters_population_age.csv"))

### Create a block group, tract, and county GEOID and aggregate
gq_age %<>% mutate(BGGEOID = str_sub(GEOID, 1, 12),
                   TCTGEOID = str_sub(GEOID, 1, 11),
                   FIPS = str_sub(GEOID, 1, 5))

### The original methodology doesn't use the GQ data! They have a count
### of college students, then used PUMS data to estimate those living in
### GQ settings, which doesn't seem great given that we have an estimate!

### For now, aggregate college GQ pop by Block Group
gq_age_bg <- gq_age %>% select(BGGEOID, contains("_UNI")) %>%
  group_by(BGGEOID) %>%
  summarize_all(sum)

### Create sum column
gq_age_bg %<>% mutate(GQ_M_NONI_UNI = GQ_M0017_NONI_UNI + GQ_M1864_NONI_UNI + GQ_M65p_NONI_UNI,
                      GQ_F_NONI_UNI = GQ_F0017_NONI_UNI + GQ_F1864_NONI_UNI + GQ_F65p_NONI_UNI,
                      GQ_NONI_UNI = GQ_M_NONI_UNI + GQ_F_NONI_UNI)


###
### Evaluate GQ Population and Colleges distances ----
### (p. 5)
###

### Read NC Block Group data
nc_bg <- read_sf(file.path(data_path, "Block_groups_TIGER_2020_NC.gpkg"))

### Convert to Centroids
nc_bg %<>% st_centroid(nc_bg)

### Project to WGS 84
nc_bg %<>% st_transform(crs = 4326)

### Merge with College GQ data
nc_bg %<>% select(GEOID) %>%
  left_join(gq_age_bg,
            by = c("GEOID" = "BGGEOID"))

### Subset to only BG with Miliary GQ residents
nc_bg %<>% filter(GQ_NONI_UNI > 0)

### Read in HIFLD Military base data for NC
col <- read_sf(file.path(data_path, "NC_Universities.gpkg"))
# col_22 <- read_sf(file.path(data_path, "NC_Universities_2022.gpkg"))

### Plot
mapview(nc_bg, 
        color = "blue", 
        cex = "GQ_NONI_UNI") +
  mapview(col, 
          col.regions = "black",
          cex = 4,
          alpha = 1)

#---------------------------------------------------#
###
### It is very apparent that all of the BGs are near
### to the locations of the colleges
###
#---------------------------------------------------#


###
### County Group Quarters data ----
###   not used in original method
###

### Read data with more detailed age for military GQ members
gq_age_detail_cty <- read_csv(file.path(data_path, "NC_county_groupquarters_univ_population_age_detail_PCO8.csv"))

### Aggregate the block level data to county (for 0-18, 18-20)
gq_age_cty <- gq_age %>% select(FIPS, contains("UNI")) %>% 
  group_by(FIPS) %>%
  summarize_all(sum)

### Join the two tables
gq_age_detail_cty %<>% mutate(GEOID = as.character(GEOID)) %>%
  left_join(gq_age_cty %>% select(-(contains("65p"))),
            by = c("GEOID" = "FIPS"))

### Calculate number of people 18-19
gq_age_detail_cty %<>% mutate(GQ_M1819_NONI_UNI = GQ_M0019_NONI_UNI - GQ_M0017_NONI_UNI,
                              GQ_F1819_NONI_UNI = GQ_F0019_NONI_UNI - GQ_F0017_NONI_UNI)

### Calculate probabilities at the county level for each sex
### for 18-64, because 0-17 and 65+ are their own bucket
gq_age_detail_cty %<>% mutate(GQ_PROB_M1819 = GQ_M1819_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M2024 = GQ_M2024_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M2529 = GQ_M2529_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M3034 = GQ_M3034_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M3539 = GQ_M3539_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M4044 = GQ_M4044_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M4549 = GQ_M4549_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M5054 = GQ_M5054_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M5559 = GQ_M5559_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_M6064 = GQ_M6064_NONI_UNI / GQ_M1864_NONI_UNI,
                              GQ_PROB_F1819 = GQ_F1819_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F2024 = GQ_F2024_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F2529 = GQ_F2529_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F3034 = GQ_F3034_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F3539 = GQ_F3539_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F4044 = GQ_F4044_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F4549 = GQ_F4549_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F5054 = GQ_F5054_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F5559 = GQ_F5559_NONI_UNI / GQ_F1864_NONI_UNI,
                              GQ_PROB_F6064 = GQ_F6064_NONI_UNI / GQ_F1864_NONI_UNI)


###
### Estimate Block Group level age group distribution ----
###   based on county (state is later)
###

### Add FIPS field to BG level data
nc_bg %<>% mutate(FIPS = str_sub(GEOID, 1, 5))

### Attach county level probabilities to block group level data
nc_bg %<>% left_join(gq_age_detail_cty %>% select(GEOID, GQ_PROB_M1819:GQ_PROB_F6064),
                     by = c("FIPS" = "GEOID"))

### Multiply count by probability for age categories... carry
### forward 0-17 and 65+
nc_bg_est1 <- nc_bg %>% mutate(UNI_GQ_M0017 = GQ_M0017_NONI_UNI,
                               UNI_GQ_M1819 = GQ_M1864_NONI_UNI * GQ_PROB_M1819,
                               UNI_GQ_M2024 = GQ_M1864_NONI_UNI * GQ_PROB_M2024,
                               UNI_GQ_M2529 = GQ_M1864_NONI_UNI * GQ_PROB_M2529,
                               UNI_GQ_M3034 = GQ_M1864_NONI_UNI * GQ_PROB_M3034,
                               UNI_GQ_M3539 = GQ_M1864_NONI_UNI * GQ_PROB_M3539,
                               UNI_GQ_M4044 = GQ_M1864_NONI_UNI * GQ_PROB_M4044,
                               UNI_GQ_M4549 = GQ_M1864_NONI_UNI * GQ_PROB_M4549,
                               UNI_GQ_M5054 = GQ_M1864_NONI_UNI * GQ_PROB_M5054,
                               UNI_GQ_M5559 = GQ_M1864_NONI_UNI * GQ_PROB_M5559,
                               UNI_GQ_M6064 = GQ_M1864_NONI_UNI * GQ_PROB_M6064,
                               UNI_GQ_M65p = GQ_M65p_NONI_UNI,
                               UNI_GQ_F0017 = GQ_F0017_NONI_UNI,
                               UNI_GQ_F1819 = GQ_F1864_NONI_UNI * GQ_PROB_F1819,
                               UNI_GQ_F2024 = GQ_F1864_NONI_UNI * GQ_PROB_F2024,
                               UNI_GQ_F2529 = GQ_F1864_NONI_UNI * GQ_PROB_F2529,
                               UNI_GQ_F3034 = GQ_F1864_NONI_UNI * GQ_PROB_F3034,
                               UNI_GQ_F3539 = GQ_F1864_NONI_UNI * GQ_PROB_F3539,
                               UNI_GQ_F4044 = GQ_F1864_NONI_UNI * GQ_PROB_F4044,
                               UNI_GQ_F4549 = GQ_F1864_NONI_UNI * GQ_PROB_F4549,
                               UNI_GQ_F5054 = GQ_F1864_NONI_UNI * GQ_PROB_F5054,
                               UNI_GQ_F5559 = GQ_F1864_NONI_UNI * GQ_PROB_F5559,
                               UNI_GQ_F6064 = GQ_F1864_NONI_UNI * GQ_PROB_F6064,
                               UNI_GQ_F65p = GQ_F65p_NONI_UNI) %>%
  select(GEOID, starts_with("UNI"))

#### Round (do in loop)
# Create copy
nc_bg_est1_round <- nc_bg_est1

for (i in 1:nrow(nc_bg_est1_round)) {
  
  ## Replace values manually
  nc_bg_est1_round[i,2:13] <- round_preserve_sum(nc_bg_est1_round[i,2:13] %>% st_drop_geometry(), 0)
  nc_bg_est1_round[i,14:25] <- round_preserve_sum(nc_bg_est1_round[i,14:25] %>% st_drop_geometry(), 0)

}


### Test some sums of the M/F population

### Total UNIitary GQ 18-64, Men
nc_bg_est1_round %>% st_drop_geometry %>% select(UNI_GQ_M1819:UNI_GQ_M6064) %>% sum()
sum(gq_age$GQ_M1864_NONI_UNI)

### Total UNIitary GQ 18-64, Female
nc_bg_est1_round %>% st_drop_geometry %>% select(UNI_GQ_F1819:UNI_GQ_F6064) %>% sum()
sum(gq_age$GQ_F1864_NONI_UNI)


###
### Create age data by year ----
###

### No guidance from any external source on the distribution of people in
### Correctional facilities by year (of age), so distribute evenly across
### age groups

### Big long calculation because I'm not wanting to code
### something nice and sweet at the moment
nc_bg_est_yearly <- nc_bg_est1_round
nc_bg_est_yearly %<>% mutate(UNI_GQ_M17 = UNI_GQ_M0017,
                             UNI_GQ_M18 = UNI_GQ_M1819 * 0.5,
                             UNI_GQ_M19 = UNI_GQ_M1819 * 0.5,
                             UNI_GQ_M20 = UNI_GQ_M2024 * 0.2,
                             UNI_GQ_M21 = UNI_GQ_M2024 * 0.2,
                             UNI_GQ_M22 = UNI_GQ_M2024 * 0.2,
                             UNI_GQ_M23 = UNI_GQ_M2024 * 0.2,
                             UNI_GQ_M24 = UNI_GQ_M2024 * 0.2,
                             UNI_GQ_M25 = UNI_GQ_M2529 * 0.2,
                             UNI_GQ_M26 = UNI_GQ_M2529 * 0.2,
                             UNI_GQ_M27 = UNI_GQ_M2529 * 0.2,
                             UNI_GQ_M28 = UNI_GQ_M2529 * 0.2,
                             UNI_GQ_M29 = UNI_GQ_M2529 * 0.2,
                             UNI_GQ_M30 = UNI_GQ_M3034 * 0.2,
                             UNI_GQ_M31 = UNI_GQ_M3034 * 0.2,
                             UNI_GQ_M32 = UNI_GQ_M3034 * 0.2,
                             UNI_GQ_M33 = UNI_GQ_M3034 * 0.2,
                             UNI_GQ_M34 = UNI_GQ_M3034 * 0.2,
                             UNI_GQ_M35 = UNI_GQ_M3539 * 0.2,
                             UNI_GQ_M36 = UNI_GQ_M3539 * 0.2,
                             UNI_GQ_M37 = UNI_GQ_M3539 * 0.2,
                             UNI_GQ_M38 = UNI_GQ_M3539 * 0.2,
                             UNI_GQ_M39 = UNI_GQ_M3539 * 0.2, 
                             UNI_GQ_M40 = UNI_GQ_M4044 * 0.2,
                             UNI_GQ_M41 = UNI_GQ_M4044 * 0.2,
                             UNI_GQ_M42 = UNI_GQ_M4044 * 0.2,
                             UNI_GQ_M43 = UNI_GQ_M4044 * 0.2,
                             UNI_GQ_M44 = UNI_GQ_M4044 * 0.2,
                             UNI_GQ_M45 = UNI_GQ_M4549 * 0.2,
                             UNI_GQ_M46 = UNI_GQ_M4549 * 0.2,
                             UNI_GQ_M47 = UNI_GQ_M4549 * 0.2,
                             UNI_GQ_M48 = UNI_GQ_M4549 * 0.2,
                             UNI_GQ_M49 = UNI_GQ_M4549 * 0.2,
                             UNI_GQ_M50 = UNI_GQ_M5054 * 0.2,
                             UNI_GQ_M51 = UNI_GQ_M5054 * 0.2,
                             UNI_GQ_M52 = UNI_GQ_M5054 * 0.2,
                             UNI_GQ_M53 = UNI_GQ_M5054 * 0.2,
                             UNI_GQ_M54 = UNI_GQ_M5054 * 0.2,
                             UNI_GQ_M55 = UNI_GQ_M5559 * 0.2,
                             UNI_GQ_M56 = UNI_GQ_M5559 * 0.2,
                             UNI_GQ_M57 = UNI_GQ_M5559 * 0.2,
                             UNI_GQ_M58 = UNI_GQ_M5559 * 0.2,
                             UNI_GQ_M59 = UNI_GQ_M5559 * 0.2,
                             UNI_GQ_M60 = UNI_GQ_M6064 * 0.2,
                             UNI_GQ_M61 = UNI_GQ_M6064 * 0.2,
                             UNI_GQ_M62 = UNI_GQ_M6064 * 0.2,
                             UNI_GQ_M63 = UNI_GQ_M6064 * 0.2,
                             UNI_GQ_M64 = UNI_GQ_M6064 * 0.2,
                             UNI_GQ_M65 = UNI_GQ_M65p,
                             UNI_GQ_F17 = UNI_GQ_F0017,
                             UNI_GQ_F18 = UNI_GQ_F1819 * 0.5,
                             UNI_GQ_F19 = UNI_GQ_F1819 * 0.5,
                             UNI_GQ_F20 = UNI_GQ_F2024 * 0.2,
                             UNI_GQ_F21 = UNI_GQ_F2024 * 0.2,
                             UNI_GQ_F22 = UNI_GQ_F2024 * 0.2,
                             UNI_GQ_F23 = UNI_GQ_F2024 * 0.2,
                             UNI_GQ_F24 = UNI_GQ_F2024 * 0.2,
                             UNI_GQ_F25 = UNI_GQ_F2529 * 0.2,
                             UNI_GQ_F26 = UNI_GQ_F2529 * 0.2,
                             UNI_GQ_F27 = UNI_GQ_F2529 * 0.2,
                             UNI_GQ_F28 = UNI_GQ_F2529 * 0.2,
                             UNI_GQ_F29 = UNI_GQ_F2529 * 0.2,
                             UNI_GQ_F30 = UNI_GQ_F3034 * 0.2,
                             UNI_GQ_F31 = UNI_GQ_F3034 * 0.2,
                             UNI_GQ_F32 = UNI_GQ_F3034 * 0.2,
                             UNI_GQ_F33 = UNI_GQ_F3034 * 0.2,
                             UNI_GQ_F34 = UNI_GQ_F3034 * 0.2,
                             UNI_GQ_F35 = UNI_GQ_F3539 * 0.2,
                             UNI_GQ_F36 = UNI_GQ_F3539 * 0.2,
                             UNI_GQ_F37 = UNI_GQ_F3539 * 0.2,
                             UNI_GQ_F38 = UNI_GQ_F3539 * 0.2,
                             UNI_GQ_F39 = UNI_GQ_F3539 * 0.2, 
                             UNI_GQ_F40 = UNI_GQ_F4044 * 0.2,
                             UNI_GQ_F41 = UNI_GQ_F4044 * 0.2,
                             UNI_GQ_F42 = UNI_GQ_F4044 * 0.2,
                             UNI_GQ_F43 = UNI_GQ_F4044 * 0.2,
                             UNI_GQ_F44 = UNI_GQ_F4044 * 0.2,
                             UNI_GQ_F45 = UNI_GQ_F4549 * 0.2,
                             UNI_GQ_F46 = UNI_GQ_F4549 * 0.2,
                             UNI_GQ_F47 = UNI_GQ_F4549 * 0.2,
                             UNI_GQ_F48 = UNI_GQ_F4549 * 0.2,
                             UNI_GQ_F49 = UNI_GQ_F4549 * 0.2,
                             UNI_GQ_F50 = UNI_GQ_F5054 * 0.2,
                             UNI_GQ_F51 = UNI_GQ_F5054 * 0.2,
                             UNI_GQ_F52 = UNI_GQ_F5054 * 0.2,
                             UNI_GQ_F53 = UNI_GQ_F5054 * 0.2,
                             UNI_GQ_F54 = UNI_GQ_F5054 * 0.2,
                             UNI_GQ_F55 = UNI_GQ_F5559 * 0.2,
                             UNI_GQ_F56 = UNI_GQ_F5559 * 0.2,
                             UNI_GQ_F57 = UNI_GQ_F5559 * 0.2,
                             UNI_GQ_F58 = UNI_GQ_F5559 * 0.2,
                             UNI_GQ_F59 = UNI_GQ_F5559 * 0.2,
                             UNI_GQ_F60 = UNI_GQ_F6064 * 0.2,
                             UNI_GQ_F61 = UNI_GQ_F6064 * 0.2,
                             UNI_GQ_F62 = UNI_GQ_F6064 * 0.2,
                             UNI_GQ_F63 = UNI_GQ_F6064 * 0.2,
                             UNI_GQ_F64 = UNI_GQ_F6064 * 0.2,
                             UNI_GQ_F65 = UNI_GQ_F65p)

### Subset to only yearly data
nc_bg_est_yearly %<>% select(GEOID, UNI_GQ_M17:UNI_GQ_F65)

### Convert decimals to counts ----
nc_bg_est_y_round <- nc_bg_est_yearly 

for (i in 1:nrow(nc_bg_est_y_round)) {
  
  ## Replace values manually
  nc_bg_est_y_round[i,2:50] <- round_preserve_sum(nc_bg_est_y_round[i,2:50] %>% st_drop_geometry(), 0)
  nc_bg_est_y_round[i,51:99] <- round_preserve_sum(nc_bg_est_y_round[i,51:99] %>% st_drop_geometry(), 0)
  
}


### Total College GQ 18-64, Men
nc_bg_est_y_round %>% st_drop_geometry %>% select(UNI_GQ_M18:UNI_GQ_M64) %>% sum()
sum(gq_age$GQ_M1864_NONI_UNI)

### Total College GQ 18-64, Female
nc_bg_est_y_round %>% st_drop_geometry %>% select(UNI_GQ_F18:UNI_GQ_F64) %>% sum()
sum(gq_age$GQ_F1864_NONI_UNI)

### Make a sum column
nc_bg_est_y_round %<>% mutate(UNI_GQ = rowSums(across(UNI_GQ_M17:UNI_GQ_F65)))
# sum(nc_bg_est_y_round$UNI_GQ)

###
### Assign GQ residents to dormitory facilities ---- 
###

### Create a vector with facility ids
### Military reserved     450000001 - 450000100
### Correctional reserved 450000101 - 450001000
potential_ids <- 450001001:450001500

### Create a vector with people ids
### Military reserved     938000001 - 938100000
### Correctional reserved 938100001 - 938300000
potential_people_ids <- 938300001:938800000


###
### Create state level GQ file that looks like county files ----
###

### Sort by county
nc_bg_est_y_round %<>% arrange(GEOID)

### Re-read block group polygon data
nc_bg_poly <- read_sf(file.path(data_path, "Block_groups_TIGER_2020_NC.gpkg"))

### Reproject and subset to the polygons with GQ
nc_bg_poly %<>% st_transform(crs = 4326) %>%
  select(GEOID) %>%
  filter(GEOID %in% nc_bg_est_y_round$GEOID) %>%
  arrange(GEOID)

### Sample a single location from every polygon
nc_bg_samp <- st_sample(x = nc_bg_poly,
                        size = rep(1, nrow(nc_bg_poly)),
                        type = "random") %>%
  bind_cols(st_coordinates(.))

### Create a sp id, total persons column, x, y
nc_bg_est_y_round %<>% mutate(sp_id = 450001001:(450001000 + nrow(nc_bg_est_y_round)),
                              persons = UNI_GQ,
                              gq_type = "C",
                              Y = nc_bg_samp$Y,
                              X = nc_bg_samp$X) %>%
  st_drop_geometry()

### Rename and make into something useable
nc_bg_UNI_gq <- nc_bg_est_y_round %>% select(sp_id,
                                             gq_type,
                                             stcotrbg = GEOID,
                                             persons,
                                             latitude = Y,
                                             longitude = X)

### Write out file
nc_bg_UNI_gq %>% write_csv(file.path(output_path, "NC_uni_gq.csv"))


###
### Create state level GQ Persons file ----
###

### Convert data to long format
nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
  select(-c(persons, gq_type, X, Y, GEOID, UNI_GQ)) %>%
  pivot_longer(-sp_id,
               names_to = "AGE",
               values_to = "COUNT")

### Extract Sex, Age
nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 10),
                                sex = str_sub(AGE, 8, 8))

### Expand into a person table
nc_UNI_gq_persons <- nc_bg_est_y_round_l %>% 
  select(-AGE) %>%
  uncount(COUNT)

### Sort by location and age, then enumerate
nc_UNI_gq_persons %<>% rename(sp_gq_id = sp_id) %>%
  mutate(sp_id = 938300000 + 1:n())

### Change column order
nc_UNI_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)

### Write out
nc_UNI_gq_persons %>% write_csv(file.path(output_path, "NC_uni_gq_people.csv"))
