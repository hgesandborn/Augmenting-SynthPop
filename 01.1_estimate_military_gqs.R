# =========================================================
# Title: Military group quarters generation
# Author: Paul Delamater
# Date: 2025-10-28
# Description: Estimate military group quarters population
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

### For now, aggregate military GQ pop by Block Group
gq_age_bg <- gq_age %>% select(BGGEOID, contains("MIL")) %>%
  group_by(BGGEOID) %>%
  summarize_all(sum)

### Create sum column
gq_age_bg %<>% mutate(GQ_M_NONI_MIL = GQ_M0017_NONI_MIL + GQ_M1864_NONI_MIL + GQ_M65p_NONI_MIL,
                      GQ_F_NONI_MIL = GQ_F0017_NONI_MIL + GQ_F1864_NONI_MIL + GQ_F65p_NONI_MIL,
                      GQ_NONI_MIL = GQ_M_NONI_MIL + GQ_F_NONI_MIL)


###
### Evaluate GQ Population and Military Base distances ----
### (p. 5)
###

### Read NC Block Group data
nc_bg <- read_sf(file.path(data_path, "Block_groups_TIGER_2020_NC.gpkg"))

### Convert to Centroids
nc_bg %<>% st_centroid(nc_bg)

### Project to WGS 84
nc_bg %<>% st_transform(crs = 4326)

### Merge with Miliary GQ data
nc_bg %<>% select(GEOID) %>%
  left_join(gq_age_bg,
            by = c("GEOID" = "BGGEOID"))

### Subset to only BG with Miliary GQ residents
nc_bg %<>% filter(GQ_NONI_MIL > 0)

### Read in HIFLD Military base data for NC
mil <- read_sf(file.path(data_path, "NC_Military_points.gpkg"))

### Plot
mapview(nc_bg, 
        color = "blue", 
        cex = "GQ_NONI_MIL") +
  mapview(mil, 
          col.regions = "black",
          cex = 4,
          alpha = 1)

#---------------------------------------------------#
###
### It is very apparent that all of the BGs are near
### to the locations of the military bases
###
#---------------------------------------------------#


###
### County Group Quarters data ----
###   not used in original method
###

### Read data with more detailed age for military GQ members
gq_age_detail_cty <- read_csv(file.path(data_path, "NC_county_groupquarters_mil_population_age_detail_PCO9.csv"))

### Aggregate the block level data to county (for 0-18, 18-20)
gq_age_cty <- gq_age %>% select(FIPS, contains("MIL")) %>% 
  group_by(FIPS) %>%
  summarize_all(sum)

### Join the two tables
gq_age_detail_cty %<>% mutate(GEOID = as.character(GEOID)) %>%
  left_join(gq_age_cty %>% select(-(contains("65p"))),
            by = c("GEOID" = "FIPS"))

### Calculate number of people 18-19
gq_age_detail_cty %<>% mutate(GQ_M1819_NONI_MIL = GQ_M0019_NONI_MIL - GQ_M0017_NONI_MIL,
                              GQ_F1819_NONI_MIL = GQ_F0019_NONI_MIL - GQ_F0017_NONI_MIL)

### Calculate probabilities at the county level for each sex
### for 18-64, because 0-17 and 65+ are their own bucket
gq_age_detail_cty %<>% mutate(GQ_PROB_M1819 = GQ_M1819_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M2024 = GQ_M2024_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M2529 = GQ_M2529_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M3034 = GQ_M3034_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M3539 = GQ_M3539_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M4044 = GQ_M4044_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M4549 = GQ_M4549_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M5054 = GQ_M5054_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M5559 = GQ_M5559_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_M6064 = GQ_M6064_NONI_MIL / GQ_M1864_NONI_MIL,
                              GQ_PROB_F1819 = GQ_F1819_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F2024 = GQ_F2024_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F2529 = GQ_F2529_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F3034 = GQ_F3034_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F3539 = GQ_F3539_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F4044 = GQ_F4044_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F4549 = GQ_F4549_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F5054 = GQ_F5054_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F5559 = GQ_F5559_NONI_MIL / GQ_F1864_NONI_MIL,
                              GQ_PROB_F6064 = GQ_F6064_NONI_MIL / GQ_F1864_NONI_MIL)


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
nc_bg_est1 <- nc_bg %>% mutate(MIL_GQ_M0017 = GQ_M0017_NONI_MIL,
                               MIL_GQ_M1819 = GQ_M1864_NONI_MIL * GQ_PROB_M1819,
                               MIL_GQ_M2024 = GQ_M1864_NONI_MIL * GQ_PROB_M2024,
                               MIL_GQ_M2529 = GQ_M1864_NONI_MIL * GQ_PROB_M2529,
                               MIL_GQ_M3034 = GQ_M1864_NONI_MIL * GQ_PROB_M3034,
                               MIL_GQ_M3539 = GQ_M1864_NONI_MIL * GQ_PROB_M3539,
                               MIL_GQ_M4044 = GQ_M1864_NONI_MIL * GQ_PROB_M4044,
                               MIL_GQ_M4549 = GQ_M1864_NONI_MIL * GQ_PROB_M4549,
                               MIL_GQ_M5054 = GQ_M1864_NONI_MIL * GQ_PROB_M5054,
                               MIL_GQ_M5559 = GQ_M1864_NONI_MIL * GQ_PROB_M5559,
                               MIL_GQ_M6064 = GQ_M1864_NONI_MIL * GQ_PROB_M6064,
                               MIL_GQ_M65p = GQ_M65p_NONI_MIL,
                               MIL_GQ_F0017 = GQ_F0017_NONI_MIL,
                               MIL_GQ_F1819 = GQ_F1864_NONI_MIL * GQ_PROB_F1819,
                               MIL_GQ_F2024 = GQ_F1864_NONI_MIL * GQ_PROB_F2024,
                               MIL_GQ_F2529 = GQ_F1864_NONI_MIL * GQ_PROB_F2529,
                               MIL_GQ_F3034 = GQ_F1864_NONI_MIL * GQ_PROB_F3034,
                               MIL_GQ_F3539 = GQ_F1864_NONI_MIL * GQ_PROB_F3539,
                               MIL_GQ_F4044 = GQ_F1864_NONI_MIL * GQ_PROB_F4044,
                               MIL_GQ_F4549 = GQ_F1864_NONI_MIL * GQ_PROB_F4549,
                               MIL_GQ_F5054 = GQ_F1864_NONI_MIL * GQ_PROB_F5054,
                               MIL_GQ_F5559 = GQ_F1864_NONI_MIL * GQ_PROB_F5559,
                               MIL_GQ_F6064 = GQ_F1864_NONI_MIL * GQ_PROB_F6064,
                               MIL_GQ_F65p = GQ_F65p_NONI_MIL) %>%
  select(GEOID, starts_with("MIL"))

#### Round (do in loop)
# Create copy
nc_bg_est1_round <- nc_bg_est1

for (i in 1:nrow(nc_bg_est1_round)) {
  
  ## Replace values manually
  nc_bg_est1_round[i,2:13] <- round_preserve_sum(nc_bg_est1_round[i,2:13] %>% st_drop_geometry(), 0)
  nc_bg_est1_round[i,14:25] <- round_preserve_sum(nc_bg_est1_round[i,14:25] %>% st_drop_geometry(), 0)

}


### Test some sums of the M/F population

### Total Military GQ 18-64, Men
nc_bg_est1_round %>% st_drop_geometry %>% select(MIL_GQ_M1819:MIL_GQ_M6064) %>% sum()
sum(gq_age$GQ_M1864_NONI_MIL)

### Total Military GQ 18-64, Female
nc_bg_est1_round %>% st_drop_geometry %>% select(MIL_GQ_F1819:MIL_GQ_F6064) %>% sum()
sum(gq_age$GQ_F1864_NONI_MIL)

###
### Read and process DOD age data by year ----
###

### Read in csv file with probs
### These are "internal" to each 5-year age group
dod <- read_csv(file.path(data_path, "DOD_yearly_probabilities_by_agegroup.csv"))


###
### Estimate Block Group level age (yearly) distribution ----
###   using data from DOD
###

### Big long calculation because I'm not wanting to code
### something nice and sweet at the moment
nc_bg_est_yearly <- nc_bg_est1_round
nc_bg_est_yearly %<>% mutate(MIL_GQ_M17 = MIL_GQ_M0017,
                             MIL_GQ_M18 = MIL_GQ_M1819 * dod$MPROB[dod$AGE == 18],
                             MIL_GQ_M19 = MIL_GQ_M1819 * dod$MPROB[dod$AGE == 19],
                             MIL_GQ_M20 = MIL_GQ_M2024 * dod$MPROB[dod$AGE == 20],
                             MIL_GQ_M21 = MIL_GQ_M2024 * dod$MPROB[dod$AGE == 21],
                             MIL_GQ_M22 = MIL_GQ_M2024 * dod$MPROB[dod$AGE == 22],
                             MIL_GQ_M23 = MIL_GQ_M2024 * dod$MPROB[dod$AGE == 23],
                             MIL_GQ_M24 = MIL_GQ_M2024 * dod$MPROB[dod$AGE == 24],
                             MIL_GQ_M25 = MIL_GQ_M2529 * dod$MPROB[dod$AGE == 25],
                             MIL_GQ_M26 = MIL_GQ_M2529 * dod$MPROB[dod$AGE == 26],
                             MIL_GQ_M27 = MIL_GQ_M2529 * dod$MPROB[dod$AGE == 27],
                             MIL_GQ_M28 = MIL_GQ_M2529 * dod$MPROB[dod$AGE == 28],
                             MIL_GQ_M29 = MIL_GQ_M2529 * dod$MPROB[dod$AGE == 29],
                             MIL_GQ_M30 = MIL_GQ_M3034 * dod$MPROB[dod$AGE == 30],
                             MIL_GQ_M31 = MIL_GQ_M3034 * dod$MPROB[dod$AGE == 31],
                             MIL_GQ_M32 = MIL_GQ_M3034 * dod$MPROB[dod$AGE == 32],
                             MIL_GQ_M33 = MIL_GQ_M3034 * dod$MPROB[dod$AGE == 33],
                             MIL_GQ_M34 = MIL_GQ_M3034 * dod$MPROB[dod$AGE == 34],
                             MIL_GQ_M35 = MIL_GQ_M3539 * dod$MPROB[dod$AGE == 35],
                             MIL_GQ_M36 = MIL_GQ_M3539 * dod$MPROB[dod$AGE == 36],
                             MIL_GQ_M37 = MIL_GQ_M3539 * dod$MPROB[dod$AGE == 37],
                             MIL_GQ_M38 = MIL_GQ_M3539 * dod$MPROB[dod$AGE == 38],
                             MIL_GQ_M39 = MIL_GQ_M3539 * dod$MPROB[dod$AGE == 39], 
                             MIL_GQ_M40 = MIL_GQ_M4044 * dod$MPROB[dod$AGE == 40],
                             MIL_GQ_M41 = MIL_GQ_M4044 * dod$MPROB[dod$AGE == 41],
                             MIL_GQ_M42 = MIL_GQ_M4044 * dod$MPROB[dod$AGE == 42],
                             MIL_GQ_M43 = MIL_GQ_M4044 * dod$MPROB[dod$AGE == 43],
                             MIL_GQ_M44 = MIL_GQ_M4044 * dod$MPROB[dod$AGE == 44],
                             MIL_GQ_M45 = MIL_GQ_M4549 * dod$MPROB[dod$AGE == 45],
                             MIL_GQ_M46 = MIL_GQ_M4549 * dod$MPROB[dod$AGE == 46],
                             MIL_GQ_M47 = MIL_GQ_M4549 * dod$MPROB[dod$AGE == 47],
                             MIL_GQ_M48 = MIL_GQ_M4549 * dod$MPROB[dod$AGE == 48],
                             MIL_GQ_M49 = MIL_GQ_M4549 * dod$MPROB[dod$AGE == 49],
                             MIL_GQ_M50 = MIL_GQ_M5054 * dod$MPROB[dod$AGE == 50],
                             MIL_GQ_M51 = MIL_GQ_M5054 * dod$MPROB[dod$AGE == 51],
                             MIL_GQ_M52 = MIL_GQ_M5054 * dod$MPROB[dod$AGE == 52],
                             MIL_GQ_M53 = MIL_GQ_M5054 * dod$MPROB[dod$AGE == 53],
                             MIL_GQ_M54 = MIL_GQ_M5054 * dod$MPROB[dod$AGE == 54],
                             MIL_GQ_M55 = MIL_GQ_M5559 * dod$MPROB[dod$AGE == 55],
                             MIL_GQ_M56 = MIL_GQ_M5559 * dod$MPROB[dod$AGE == 56],
                             MIL_GQ_M57 = MIL_GQ_M5559 * dod$MPROB[dod$AGE == 57],
                             MIL_GQ_M58 = MIL_GQ_M5559 * dod$MPROB[dod$AGE == 58],
                             MIL_GQ_M59 = MIL_GQ_M5559 * dod$MPROB[dod$AGE == 59],
                             MIL_GQ_M60 = MIL_GQ_M6064 * dod$MPROB[dod$AGE == 60],
                             MIL_GQ_M61 = MIL_GQ_M6064 * dod$MPROB[dod$AGE == 61],
                             MIL_GQ_M62 = MIL_GQ_M6064 * dod$MPROB[dod$AGE == 62],
                             MIL_GQ_M63 = MIL_GQ_M6064 * dod$MPROB[dod$AGE == 63],
                             MIL_GQ_M64 = MIL_GQ_M6064 * dod$MPROB[dod$AGE == 64],
                             MIL_GQ_M65 = MIL_GQ_M65p,
                             MIL_GQ_F17 = MIL_GQ_F0017,
                             MIL_GQ_F18 = MIL_GQ_F1819 * dod$MPROB[dod$AGE == 18],
                             MIL_GQ_F19 = MIL_GQ_F1819 * dod$MPROB[dod$AGE == 19],
                             MIL_GQ_F20 = MIL_GQ_F2024 * dod$MPROB[dod$AGE == 20],
                             MIL_GQ_F21 = MIL_GQ_F2024 * dod$MPROB[dod$AGE == 21],
                             MIL_GQ_F22 = MIL_GQ_F2024 * dod$MPROB[dod$AGE == 22],
                             MIL_GQ_F23 = MIL_GQ_F2024 * dod$MPROB[dod$AGE == 23],
                             MIL_GQ_F24 = MIL_GQ_F2024 * dod$MPROB[dod$AGE == 24],
                             MIL_GQ_F25 = MIL_GQ_F2529 * dod$MPROB[dod$AGE == 25],
                             MIL_GQ_F26 = MIL_GQ_F2529 * dod$MPROB[dod$AGE == 26],
                             MIL_GQ_F27 = MIL_GQ_F2529 * dod$MPROB[dod$AGE == 27],
                             MIL_GQ_F28 = MIL_GQ_F2529 * dod$MPROB[dod$AGE == 28],
                             MIL_GQ_F29 = MIL_GQ_F2529 * dod$MPROB[dod$AGE == 29],
                             MIL_GQ_F30 = MIL_GQ_F3034 * dod$MPROB[dod$AGE == 30],
                             MIL_GQ_F31 = MIL_GQ_F3034 * dod$MPROB[dod$AGE == 31],
                             MIL_GQ_F32 = MIL_GQ_F3034 * dod$MPROB[dod$AGE == 32],
                             MIL_GQ_F33 = MIL_GQ_F3034 * dod$MPROB[dod$AGE == 33],
                             MIL_GQ_F34 = MIL_GQ_F3034 * dod$MPROB[dod$AGE == 34],
                             MIL_GQ_F35 = MIL_GQ_F3539 * dod$MPROB[dod$AGE == 35],
                             MIL_GQ_F36 = MIL_GQ_F3539 * dod$MPROB[dod$AGE == 36],
                             MIL_GQ_F37 = MIL_GQ_F3539 * dod$MPROB[dod$AGE == 37],
                             MIL_GQ_F38 = MIL_GQ_F3539 * dod$MPROB[dod$AGE == 38],
                             MIL_GQ_F39 = MIL_GQ_F3539 * dod$MPROB[dod$AGE == 39], 
                             MIL_GQ_F40 = MIL_GQ_F4044 * dod$MPROB[dod$AGE == 40],
                             MIL_GQ_F41 = MIL_GQ_F4044 * dod$MPROB[dod$AGE == 41],
                             MIL_GQ_F42 = MIL_GQ_F4044 * dod$MPROB[dod$AGE == 42],
                             MIL_GQ_F43 = MIL_GQ_F4044 * dod$MPROB[dod$AGE == 43],
                             MIL_GQ_F44 = MIL_GQ_F4044 * dod$MPROB[dod$AGE == 44],
                             MIL_GQ_F45 = MIL_GQ_F4549 * dod$MPROB[dod$AGE == 45],
                             MIL_GQ_F46 = MIL_GQ_F4549 * dod$MPROB[dod$AGE == 46],
                             MIL_GQ_F47 = MIL_GQ_F4549 * dod$MPROB[dod$AGE == 47],
                             MIL_GQ_F48 = MIL_GQ_F4549 * dod$MPROB[dod$AGE == 48],
                             MIL_GQ_F49 = MIL_GQ_F4549 * dod$MPROB[dod$AGE == 49],
                             MIL_GQ_F50 = MIL_GQ_F5054 * dod$MPROB[dod$AGE == 50],
                             MIL_GQ_F51 = MIL_GQ_F5054 * dod$MPROB[dod$AGE == 51],
                             MIL_GQ_F52 = MIL_GQ_F5054 * dod$MPROB[dod$AGE == 52],
                             MIL_GQ_F53 = MIL_GQ_F5054 * dod$MPROB[dod$AGE == 53],
                             MIL_GQ_F54 = MIL_GQ_F5054 * dod$MPROB[dod$AGE == 54],
                             MIL_GQ_F55 = MIL_GQ_F5559 * dod$MPROB[dod$AGE == 55],
                             MIL_GQ_F56 = MIL_GQ_F5559 * dod$MPROB[dod$AGE == 56],
                             MIL_GQ_F57 = MIL_GQ_F5559 * dod$MPROB[dod$AGE == 57],
                             MIL_GQ_F58 = MIL_GQ_F5559 * dod$MPROB[dod$AGE == 58],
                             MIL_GQ_F59 = MIL_GQ_F5559 * dod$MPROB[dod$AGE == 59],
                             MIL_GQ_F60 = MIL_GQ_F6064 * dod$MPROB[dod$AGE == 60],
                             MIL_GQ_F61 = MIL_GQ_F6064 * dod$MPROB[dod$AGE == 61],
                             MIL_GQ_F62 = MIL_GQ_F6064 * dod$MPROB[dod$AGE == 62],
                             MIL_GQ_F63 = MIL_GQ_F6064 * dod$MPROB[dod$AGE == 63],
                             MIL_GQ_F64 = MIL_GQ_F6064 * dod$MPROB[dod$AGE == 64],
                             MIL_GQ_F65 = MIL_GQ_F65p)

### Subset to only yearly data
nc_bg_est_yearly %<>% select(GEOID, MIL_GQ_M17:MIL_GQ_F65)

### Convert decimals to counts ----
nc_bg_est_y_round <- nc_bg_est_yearly 

for (i in 1:nrow(nc_bg_est_y_round)) {
  
  ## Replace values manually
  nc_bg_est_y_round[i,2:50] <- round_preserve_sum(nc_bg_est_y_round[i,2:50] %>% st_drop_geometry(), 0)
  nc_bg_est_y_round[i,51:99] <- round_preserve_sum(nc_bg_est_y_round[i,51:99] %>% st_drop_geometry(), 0)
  
}


### Total Military GQ 18-64, Men
nc_bg_est_y_round %>% st_drop_geometry %>% select(MIL_GQ_M18:MIL_GQ_M64) %>% sum()
sum(gq_age$GQ_M1864_NONI_MIL)

### Total Military GQ 18-64, Female
nc_bg_est_y_round %>% st_drop_geometry %>% select(MIL_GQ_F18:MIL_GQ_F64) %>% sum()
sum(gq_age$GQ_F1864_NONI_MIL)


###
### Assign GQ residents for BGs less than 50 to nearest base ---- 
###

### Decided not to do this because the guidance in the paper
### is not great, and I'm not sure why it would be necessary
### as there may actually be a small GQ in some region


###
### Create state level GQ file that looks like county files ----
###

### Sort by county
nc_bg_est_y_round %<>% arrange(GEOID)

### Create a sp id, total persons column, x, y
nc_bg_est_y_round %<>% mutate(sp_id = 450000001:450000026,
                              persons = rowSums(across(MIL_GQ_M17:MIL_GQ_F65)),
                              gq_type = "M") %>%
  bind_cols(st_coordinates(nc_bg_est_y_round)) %>%
  st_drop_geometry()

### Rename and make into something useable
nc_bg_mil_gq <- nc_bg_est_y_round %>% select(sp_id,
                                             gq_type,
                                             stcotrbg = GEOID,
                                             persons,
                                             latitude = Y,
                                             longitude = X)

### Write out file
nc_bg_mil_gq %>% write_csv(file.path(output_path, "NC_mil_gq.csv"))


###
### Create state level GQ Persons file ----
###

### Convert data to long format
nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
  select(-c(persons, gq_type, X, Y, GEOID)) %>%
  pivot_longer(-sp_id,
               names_to = "AGE",
               values_to = "COUNT")

### Extract Sex, Age
nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 10),
                                sex = str_sub(AGE, 8, 8))

### Expand into a person table
nc_mil_gq_persons <- nc_bg_est_y_round_l %>% 
  select(-AGE) %>%
  uncount(COUNT)

### Sort by location and age, then enumerate
nc_mil_gq_persons %<>% rename(sp_gq_id = sp_id) %>%
  mutate(sp_id = 938000000 + 1:n())

### Change column order
nc_mil_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)

### Write out
nc_mil_gq_persons %>% write_csv(file.path(output_path, "NC_mil_gq_people.csv"))
