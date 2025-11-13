# =========================================================
# Title: Nursing homes group quarters generation
# Author: Paul Delamater
# Date: 2025-10-28
# Description: Estimate nursing homes group quarters population
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
### Block group GQ Nursing Home data ----
###

### Read in the Census table with detailed counts by block
gq_age <- read_csv(file.path(data_path, "NC_block_groupquarters_population_age.csv"))

### Create a block group, tract, and county GEOID and aggregate
gq_age %<>% mutate(BGGEOID = str_sub(GEOID, 1, 12),
                   TCTGEOID = str_sub(GEOID, 1, 11),
                   FIPS = str_sub(GEOID, 1, 5))

### Create sum column
gq_age %<>% mutate(GQ_M_INST_NUR = GQ_M0017_INST_NUR + GQ_M1864_INST_NUR + GQ_M65p_INST_NUR,
                      GQ_F_INST_NUR = GQ_F0017_INST_NUR + GQ_F1864_INST_NUR + GQ_F65p_INST_NUR,
                      GQ_INST_NUR = GQ_M_INST_NUR + GQ_F_INST_NUR)

### For now, aggregate correctional GQ pop by Block Group
gq_age_bg <- gq_age %>% select(BGGEOID, contains("NUR")) %>%
  group_by(BGGEOID) %>%
  summarize_all(sum)

### But also, take the block level data forward
gq_age %<>% select(GEOID, BGGEOID, TCTGEOID, FIPS, contains("NUR"))

### Subset to only units with corr population
gq_age %<>% filter(GQ_INST_NUR > 0)
gq_age_bg %<>% filter(GQ_INST_NUR > 0)


###
### Nursing Home data ----
### (p. 5)
###

### Read NC Block Group data
nc_bg <- read_sf(file.path(data_path, "Block_groups_TIGER_2020_NC.gpkg"))

### Read NC Block data
nc_blk <- read_sf(file.path(data_path, "Blocks_popgt0_TIGER_2020_NC.gpkg"))

### Read in HIFLD Correctional Facilities data for NC
nurs <- read_sf(file.path(data_path, "NC_NursingHome_points.gpkg"))

### Project to WGS 84
nurs %<>% st_transform(crs = 4326)
nc_bg %<>% st_transform(crs = 4326)
nc_blk %<>% st_transform(crs = 4326)

# length(unique(nurs$OBJECTID))

### Spatial Join, Block Group
nurs %<>% select(OBJECTID, NAME, TYPE, STATUS, BEDS) %>%
  filter(STATUS == "OPEN") %>%
  st_join(nc_bg %>% select(GEOID))

### Spatial Join, Block
nurs %<>% st_join(nc_blk %>% select(GEOID20))

### NA for -999 in BEDS
nurs %<>% mutate(BEDS = na_if(BEDS, -999))

### Summary of overlap
table(nurs$GEOID) %>% sort()
table(nurs$GEOID20) %>% sort()

### Merge BG with Nursing Home Pop data
nc_bg_gq_age <- nc_bg %>% 
  select(GEOID) %>%
  left_join(gq_age_bg,
            by = c("GEOID" = "BGGEOID")) 

### Subset to only BG with Nursing Home GQ residents
nc_bg_gq_age %<>% filter(GQ_INST_NUR > 0)

### Plot
mapview(nc_bg_gq_age, 
        col.regions = "blue") +
  mapview(nurs, 
          col.regions = "black",
          cex = 4,
          alpha = 1)

### Check totals
nurs %>% st_drop_geometry() %>% 
  group_by(TYPE) %>% 
  summarize(BEDS = sum(BEDS, na.rm = TRUE))    #### 51502 BEDS, NURSING HOME 
nc_bg_gq_age %>% pull(GQ_INST_NUR) %>% sum()   #### 48719 IN CENSUS

### Subset Nursing Home file to Nursing Homes
# nurs %<>% filter(TYPE == "NURSING HOME")

### Get Median Size of GQ
nurs_beds_median <- median(nurs$BEDS, na.rm = TRUE)

### Replace NA with median
nurs$BEDS[is.na(nurs$BEDS)] <- nurs_beds_median

### Add X,Y to data
nurs %<>% bind_cols(st_coordinates(nurs))

###
### County Group Quarters data ----
###   not used in original method
###

### Read data with more detailed age for correctional GQ members
gq_age_detail_cty <- read_csv(file.path(data_path, "NC_county_groupquarters_nurs_population_age_detail_PCO5.csv"))

### Aggregate the block level data to county (for 0-18, 18-20)
gq_age_cty <- gq_age %>% select(FIPS, contains("NUR")) %>% 
  group_by(FIPS) %>%
  summarize_all(sum)

### Join the two tables
gq_age_detail_cty %<>% mutate(GEOID = as.character(GEOID)) %>%
  left_join(gq_age_cty %>% select(-GQ_M_INST_NUR, -GQ_F_INST_NUR),
            by = c("GEOID" = "FIPS"))



### Calculate number of people 18-24
gq_age_detail_cty %<>% mutate(GQ_M1824_INST_NUR = GQ_M0024_INST_NUR - GQ_M0017_INST_NUR,
                              GQ_F1824_INST_NUR = GQ_F0024_INST_NUR - GQ_F0017_INST_NUR)

### Calculate number of people 65-84
### Actually, we don't need this because the values
### Don't cross each other
# gq_age_detail_cty %<>% mutate(GQ_M6584_INST_COR = GQ_M65p_INST_COR - GQ_M85p_INST_COR,
#                               GQ_F6584_INST_COR = GQ_F65p_INST_COR - GQ_F85p_INST_COR)

### Calculate probabilities at the county level for each sex
### for 18-84, we'll do 0-18 later
gq_age_detail_cty %<>% mutate(GQ_PROB_M0017 = 1, 
                              GQ_PROB_M1824 = GQ_M1824_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M2529 = GQ_M2529_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M3034 = GQ_M3034_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M3539 = GQ_M3539_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M4044 = GQ_M4044_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M4549 = GQ_M4549_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M5054 = GQ_M5054_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M5559 = GQ_M5559_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M6064 = GQ_M6064_INST_NUR / GQ_M1864_INST_NUR,
                              GQ_PROB_M6569 = GQ_M6569_INST_NUR / GQ_M65p_INST_NUR,
                              GQ_PROB_M7074 = GQ_M7074_INST_NUR / GQ_M65p_INST_NUR,
                              GQ_PROB_M7579 = GQ_M7579_INST_NUR / GQ_M65p_INST_NUR,
                              GQ_PROB_M8084 = GQ_M8084_INST_NUR / GQ_M65p_INST_NUR,
                              GQ_PROB_M85p = GQ_M85p_INST_NUR / GQ_M65p_INST_NUR,
                              GQ_PROB_F0017 = 1, 
                              GQ_PROB_F1824 = GQ_F1824_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F2529 = GQ_F2529_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F3034 = GQ_F3034_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F3539 = GQ_F3539_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F4044 = GQ_F4044_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F4549 = GQ_F4549_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F5054 = GQ_F5054_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F5559 = GQ_F5559_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F6064 = GQ_F6064_INST_NUR / GQ_F1864_INST_NUR,
                              GQ_PROB_F6569 = GQ_F6569_INST_NUR / GQ_F65p_INST_NUR,
                              GQ_PROB_F7074 = GQ_F7074_INST_NUR / GQ_F65p_INST_NUR,
                              GQ_PROB_F7579 = GQ_F7579_INST_NUR / GQ_F65p_INST_NUR,
                              GQ_PROB_F8084 = GQ_F8084_INST_NUR / GQ_F65p_INST_NUR,
                              GQ_PROB_F85p = GQ_F85p_INST_NUR / GQ_F65p_INST_NUR)

### Replace NAs with 0
gq_age_detail_cty %<>% mutate(across(-GEOID, ~ replace_na(., 0)))

### Reorder columns a bit (organize probs)
gq_age_detail_cty %<>% select(GEOID, GQ_PROB_M0017:GQ_PROB_F85p, GQ_INST_NUR)


###
### Estimate Block Group level age group distribution ----
###   based on county (state is later)
###

### Add FIPS field to BG level data
gq_age_bg %<>% mutate(FIPS = str_sub(BGGEOID, 1, 5))

### Attach county level probabilities to block group level data
gq_age_bg %<>% left_join(gq_age_detail_cty %>% select(GEOID, GQ_PROB_M0017:GQ_PROB_F85p),
                     by = c("FIPS" = "GEOID"))

### Multiply count by probability for age categories... carry
### forward 0-17 and 65+
nc_bg_est1 <- gq_age_bg %>% mutate(NUR_GQ_M0017 = GQ_M0017_INST_NUR * GQ_PROB_M0017,
                                   NUR_GQ_M1824 = GQ_M1864_INST_NUR * GQ_PROB_M1824,
                                   NUR_GQ_M2529 = GQ_M1864_INST_NUR * GQ_PROB_M2529,
                                   NUR_GQ_M3034 = GQ_M1864_INST_NUR * GQ_PROB_M3034,
                                   NUR_GQ_M3539 = GQ_M1864_INST_NUR * GQ_PROB_M3539,
                                   NUR_GQ_M4044 = GQ_M1864_INST_NUR * GQ_PROB_M4044,
                                   NUR_GQ_M4549 = GQ_M1864_INST_NUR * GQ_PROB_M4549,
                                   NUR_GQ_M5054 = GQ_M1864_INST_NUR * GQ_PROB_M5054,
                                   NUR_GQ_M5559 = GQ_M1864_INST_NUR * GQ_PROB_M5559,
                                   NUR_GQ_M6064 = GQ_M1864_INST_NUR * GQ_PROB_M6064,
                                   NUR_GQ_M6569 = GQ_M65p_INST_NUR * GQ_PROB_M6569,
                                   NUR_GQ_M7074 = GQ_M65p_INST_NUR * GQ_PROB_M7074,
                                   NUR_GQ_M7579 = GQ_M65p_INST_NUR * GQ_PROB_M7579,
                                   NUR_GQ_M8084 = GQ_M65p_INST_NUR * GQ_PROB_M8084,
                                   NUR_GQ_M85p = GQ_M65p_INST_NUR * GQ_PROB_M85p,
                                   NUR_GQ_F0017 = GQ_F0017_INST_NUR * GQ_PROB_F0017,
                                   NUR_GQ_F1824 = GQ_F1864_INST_NUR * GQ_PROB_F1824,
                                   NUR_GQ_F2529 = GQ_F1864_INST_NUR * GQ_PROB_F2529,
                                   NUR_GQ_F3034 = GQ_F1864_INST_NUR * GQ_PROB_F3034,
                                   NUR_GQ_F3539 = GQ_F1864_INST_NUR * GQ_PROB_F3539,
                                   NUR_GQ_F4044 = GQ_F1864_INST_NUR * GQ_PROB_F4044,
                                   NUR_GQ_F4549 = GQ_F1864_INST_NUR * GQ_PROB_F4549,
                                   NUR_GQ_F5054 = GQ_F1864_INST_NUR * GQ_PROB_F5054,
                                   NUR_GQ_F5559 = GQ_F1864_INST_NUR * GQ_PROB_F5559,
                                   NUR_GQ_F6064 = GQ_F1864_INST_NUR * GQ_PROB_F6064,
                                   NUR_GQ_F6569 = GQ_F65p_INST_NUR * GQ_PROB_F6569,
                                   NUR_GQ_F7074 = GQ_F65p_INST_NUR * GQ_PROB_F7074,
                                   NUR_GQ_F7579 = GQ_F65p_INST_NUR * GQ_PROB_F7579,
                                   NUR_GQ_F8084 = GQ_F65p_INST_NUR * GQ_PROB_F8084,
                                   NUR_GQ_F85p = GQ_F65p_INST_NUR * GQ_PROB_F85p,) %>%
  select(BGGEOID, starts_with("NUR"))

#### Round (do in loop)
# Create copy
nc_bg_est1_round <- nc_bg_est1 

for (i in 1:nrow(nc_bg_est1_round)) {
  
  ## Replace values manually
  nc_bg_est1_round[i,3:11] <- round_preserve_sum(nc_bg_est1_round[i,3:11], 0)
  nc_bg_est1_round[i,12:16] <- round_preserve_sum(nc_bg_est1_round[i,12:16], 0)
  nc_bg_est1_round[i,18:26] <- round_preserve_sum(nc_bg_est1_round[i,18:26], 0)
  nc_bg_est1_round[i,27:31] <- round_preserve_sum(nc_bg_est1_round[i,27:31], 0)

}

### Test some sums of the M/F population

### Total NURr GQ 18-64, Men
nc_bg_est1_round %>% st_drop_geometry %>% select(NUR_GQ_M1824:NUR_GQ_M6064) %>% sum()
sum(gq_age$GQ_M1864_INST_NUR)

### Total NURr GQ 18-64, Female
nc_bg_est1_round %>% st_drop_geometry %>% select(NUR_GQ_F1824:NUR_GQ_F6064) %>% sum()
sum(gq_age$GQ_F1864_INST_NUR)

### Total NURr GQ 65+, Female
nc_bg_est1_round %>% st_drop_geometry %>% select(NUR_GQ_F6569:NUR_GQ_F85p) %>% sum()
sum(gq_age$GQ_F65p_INST_NUR)

###
### Create age data by year ----
###

### No guidance from any external source on the distribution of people in
### Nursing Homes by year (of age), so distribute evenly across
### age groups

### Big long calculation because I'm not wanting to code
### something nice and sweet at the moment
nc_bg_est_yearly <- nc_bg_est1_round
nc_bg_est_yearly %<>% mutate(NUR_GQ_M00 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M01 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M02 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M03 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M04 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M05 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M06 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M07 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M08 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M09 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M10 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M11 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M12 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M13 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M14 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M15 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M16 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M17 = NUR_GQ_M0017 * 1/18,
                             NUR_GQ_M18 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M19 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M20 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M21 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M22 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M23 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M24 = NUR_GQ_M1824 * 1/7,
                             NUR_GQ_M25 = NUR_GQ_M2529 * 0.2,
                             NUR_GQ_M26 = NUR_GQ_M2529 * 0.2,
                             NUR_GQ_M27 = NUR_GQ_M2529 * 0.2,
                             NUR_GQ_M28 = NUR_GQ_M2529 * 0.2,
                             NUR_GQ_M29 = NUR_GQ_M2529 * 0.2,
                             NUR_GQ_M30 = NUR_GQ_M3034 * 0.2,
                             NUR_GQ_M31 = NUR_GQ_M3034 * 0.2,
                             NUR_GQ_M32 = NUR_GQ_M3034 * 0.2,
                             NUR_GQ_M33 = NUR_GQ_M3034 * 0.2,
                             NUR_GQ_M34 = NUR_GQ_M3034 * 0.2,
                             NUR_GQ_M35 = NUR_GQ_M3539 * 0.2,
                             NUR_GQ_M36 = NUR_GQ_M3539 * 0.2,
                             NUR_GQ_M37 = NUR_GQ_M3539 * 0.2,
                             NUR_GQ_M38 = NUR_GQ_M3539 * 0.2,
                             NUR_GQ_M39 = NUR_GQ_M3539 * 0.2, 
                             NUR_GQ_M40 = NUR_GQ_M4044 * 0.2,
                             NUR_GQ_M41 = NUR_GQ_M4044 * 0.2,
                             NUR_GQ_M42 = NUR_GQ_M4044 * 0.2,
                             NUR_GQ_M43 = NUR_GQ_M4044 * 0.2,
                             NUR_GQ_M44 = NUR_GQ_M4044 * 0.2,
                             NUR_GQ_M45 = NUR_GQ_M4549 * 0.2,
                             NUR_GQ_M46 = NUR_GQ_M4549 * 0.2,
                             NUR_GQ_M47 = NUR_GQ_M4549 * 0.2,
                             NUR_GQ_M48 = NUR_GQ_M4549 * 0.2,
                             NUR_GQ_M49 = NUR_GQ_M4549 * 0.2,
                             NUR_GQ_M50 = NUR_GQ_M5054 * 0.2,
                             NUR_GQ_M51 = NUR_GQ_M5054 * 0.2,
                             NUR_GQ_M52 = NUR_GQ_M5054 * 0.2,
                             NUR_GQ_M53 = NUR_GQ_M5054 * 0.2,
                             NUR_GQ_M54 = NUR_GQ_M5054 * 0.2,
                             NUR_GQ_M55 = NUR_GQ_M5559 * 0.2,
                             NUR_GQ_M56 = NUR_GQ_M5559 * 0.2,
                             NUR_GQ_M57 = NUR_GQ_M5559 * 0.2,
                             NUR_GQ_M58 = NUR_GQ_M5559 * 0.2,
                             NUR_GQ_M59 = NUR_GQ_M5559 * 0.2,
                             NUR_GQ_M60 = NUR_GQ_M6064 * 0.2,
                             NUR_GQ_M61 = NUR_GQ_M6064 * 0.2,
                             NUR_GQ_M62 = NUR_GQ_M6064 * 0.2,
                             NUR_GQ_M63 = NUR_GQ_M6064 * 0.2,
                             NUR_GQ_M64 = NUR_GQ_M6064 * 0.2,
                             NUR_GQ_M65 = NUR_GQ_M6569 * 0.2,
                             NUR_GQ_M66 = NUR_GQ_M6569 * 0.2,
                             NUR_GQ_M67 = NUR_GQ_M6569 * 0.2,
                             NUR_GQ_M68 = NUR_GQ_M6569 * 0.2,
                             NUR_GQ_M69 = NUR_GQ_M6569 * 0.2,
                             NUR_GQ_M70 = NUR_GQ_M7074 * 0.2,
                             NUR_GQ_M71 = NUR_GQ_M7074 * 0.2,
                             NUR_GQ_M72 = NUR_GQ_M7074 * 0.2,
                             NUR_GQ_M73 = NUR_GQ_M7074 * 0.2,
                             NUR_GQ_M74 = NUR_GQ_M7074 * 0.2,
                             NUR_GQ_M75 = NUR_GQ_M7579 * 0.2,
                             NUR_GQ_M76 = NUR_GQ_M7579 * 0.2,
                             NUR_GQ_M77 = NUR_GQ_M7579 * 0.2,
                             NUR_GQ_M78 = NUR_GQ_M7579 * 0.2,
                             NUR_GQ_M79 = NUR_GQ_M7579 * 0.2,
                             NUR_GQ_M80 = NUR_GQ_M8084 * 0.2,
                             NUR_GQ_M81 = NUR_GQ_M8084 * 0.2,
                             NUR_GQ_M82 = NUR_GQ_M8084 * 0.2,
                             NUR_GQ_M83 = NUR_GQ_M8084 * 0.2,
                             NUR_GQ_M84 = NUR_GQ_M8084 * 0.2,
                             NUR_GQ_M85 = NUR_GQ_M85p * 0.11,      ### The original data has
                             NUR_GQ_M86 = NUR_GQ_M85p * 0.11,      ### people up to 105 years
                             NUR_GQ_M87 = NUR_GQ_M85p * 0.11,     ### in nursing homes
                             NUR_GQ_M88 = NUR_GQ_M85p * 0.11,    #
                             NUR_GQ_M89 = NUR_GQ_M85p * 0.11,   ### Used dist of org FRED pop
                             NUR_GQ_M90 = NUR_GQ_M85p * 0.11,  ### to determine dist by year
                             NUR_GQ_M91 = NUR_GQ_M85p * 0.035,
                             NUR_GQ_M92 = NUR_GQ_M85p * 0.035,
                             NUR_GQ_M93 = NUR_GQ_M85p * 0.035,  
                             NUR_GQ_M94 = NUR_GQ_M85p * 0.035,  
                             NUR_GQ_M95 = NUR_GQ_M85p * 0.035,  
                             NUR_GQ_M96 = NUR_GQ_M85p * 0.025,
                             NUR_GQ_M97 = NUR_GQ_M85p * 0.025,
                             NUR_GQ_M98 = NUR_GQ_M85p * 0.025,
                             NUR_GQ_M99 = NUR_GQ_M85p * 0.025,
                             NUR_GQ_M100 = NUR_GQ_M85p * 0.025,
                             NUR_GQ_M101 = NUR_GQ_M85p * 0.01,
                             NUR_GQ_M102 = NUR_GQ_M85p * 0.01,
                             NUR_GQ_M103 = NUR_GQ_M85p * 0.01,
                             NUR_GQ_M104 = NUR_GQ_M85p * 0.005,
                             NUR_GQ_M105 = NUR_GQ_M85p * 0.005,
                             NUR_GQ_F00 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F01 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F02 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F03 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F04 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F05 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F06 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F07 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F08 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F09 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F10 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F11 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F12 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F13 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F14 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F15 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F16 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F17 = NUR_GQ_F0017 * 1/18,
                             NUR_GQ_F18 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F19 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F20 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F21 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F22 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F23 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F24 = NUR_GQ_F1824 * 1/7,
                             NUR_GQ_F25 = NUR_GQ_F2529 * 0.2,
                             NUR_GQ_F26 = NUR_GQ_F2529 * 0.2,
                             NUR_GQ_F27 = NUR_GQ_F2529 * 0.2,
                             NUR_GQ_F28 = NUR_GQ_F2529 * 0.2,
                             NUR_GQ_F29 = NUR_GQ_F2529 * 0.2,
                             NUR_GQ_F30 = NUR_GQ_F3034 * 0.2,
                             NUR_GQ_F31 = NUR_GQ_F3034 * 0.2,
                             NUR_GQ_F32 = NUR_GQ_F3034 * 0.2,
                             NUR_GQ_F33 = NUR_GQ_F3034 * 0.2,
                             NUR_GQ_F34 = NUR_GQ_F3034 * 0.2,
                             NUR_GQ_F35 = NUR_GQ_F3539 * 0.2,
                             NUR_GQ_F36 = NUR_GQ_F3539 * 0.2,
                             NUR_GQ_F37 = NUR_GQ_F3539 * 0.2,
                             NUR_GQ_F38 = NUR_GQ_F3539 * 0.2,
                             NUR_GQ_F39 = NUR_GQ_F3539 * 0.2, 
                             NUR_GQ_F40 = NUR_GQ_F4044 * 0.2,
                             NUR_GQ_F41 = NUR_GQ_F4044 * 0.2,
                             NUR_GQ_F42 = NUR_GQ_F4044 * 0.2,
                             NUR_GQ_F43 = NUR_GQ_F4044 * 0.2,
                             NUR_GQ_F44 = NUR_GQ_F4044 * 0.2,
                             NUR_GQ_F45 = NUR_GQ_F4549 * 0.2,
                             NUR_GQ_F46 = NUR_GQ_F4549 * 0.2,
                             NUR_GQ_F47 = NUR_GQ_F4549 * 0.2,
                             NUR_GQ_F48 = NUR_GQ_F4549 * 0.2,
                             NUR_GQ_F49 = NUR_GQ_F4549 * 0.2,
                             NUR_GQ_F50 = NUR_GQ_F5054 * 0.2,
                             NUR_GQ_F51 = NUR_GQ_F5054 * 0.2,
                             NUR_GQ_F52 = NUR_GQ_F5054 * 0.2,
                             NUR_GQ_F53 = NUR_GQ_F5054 * 0.2,
                             NUR_GQ_F54 = NUR_GQ_F5054 * 0.2,
                             NUR_GQ_F55 = NUR_GQ_F5559 * 0.2,
                             NUR_GQ_F56 = NUR_GQ_F5559 * 0.2,
                             NUR_GQ_F57 = NUR_GQ_F5559 * 0.2,
                             NUR_GQ_F58 = NUR_GQ_F5559 * 0.2,
                             NUR_GQ_F59 = NUR_GQ_F5559 * 0.2,
                             NUR_GQ_F60 = NUR_GQ_F6064 * 0.2,
                             NUR_GQ_F61 = NUR_GQ_F6064 * 0.2,
                             NUR_GQ_F62 = NUR_GQ_F6064 * 0.2,
                             NUR_GQ_F63 = NUR_GQ_F6064 * 0.2,
                             NUR_GQ_F64 = NUR_GQ_F6064 * 0.2,
                             NUR_GQ_F65 = NUR_GQ_F6569 * 0.2,
                             NUR_GQ_F66 = NUR_GQ_F6569 * 0.2,
                             NUR_GQ_F67 = NUR_GQ_F6569 * 0.2,
                             NUR_GQ_F68 = NUR_GQ_F6569 * 0.2,
                             NUR_GQ_F69 = NUR_GQ_F6569 * 0.2,
                             NUR_GQ_F70 = NUR_GQ_F7074 * 0.2,
                             NUR_GQ_F71 = NUR_GQ_F7074 * 0.2,
                             NUR_GQ_F72 = NUR_GQ_F7074 * 0.2,
                             NUR_GQ_F73 = NUR_GQ_F7074 * 0.2,
                             NUR_GQ_F74 = NUR_GQ_F7074 * 0.2,
                             NUR_GQ_F75 = NUR_GQ_F7579 * 0.2,
                             NUR_GQ_F76 = NUR_GQ_F7579 * 0.2,
                             NUR_GQ_F77 = NUR_GQ_F7579 * 0.2,
                             NUR_GQ_F78 = NUR_GQ_F7579 * 0.2,
                             NUR_GQ_F79 = NUR_GQ_F7579 * 0.2,
                             NUR_GQ_F80 = NUR_GQ_F8084 * 0.2,
                             NUR_GQ_F81 = NUR_GQ_F8084 * 0.2,
                             NUR_GQ_F82 = NUR_GQ_F8084 * 0.2,
                             NUR_GQ_F83 = NUR_GQ_F8084 * 0.2,
                             NUR_GQ_F84 = NUR_GQ_F8084 * 0.2,
                             NUR_GQ_F85 = NUR_GQ_F85p * 0.11,      ### The original data has
                             NUR_GQ_F86 = NUR_GQ_F85p * 0.11,      ### people up to 105 years
                             NUR_GQ_F87 = NUR_GQ_F85p * 0.11,     ### in nursing homes
                             NUR_GQ_F88 = NUR_GQ_F85p * 0.11,    #
                             NUR_GQ_F89 = NUR_GQ_F85p * 0.11,   ### Used dist of org FRED pop
                             NUR_GQ_F90 = NUR_GQ_F85p * 0.11,  ### to determine dist by year
                             NUR_GQ_F91 = NUR_GQ_F85p * 0.035,
                             NUR_GQ_F92 = NUR_GQ_F85p * 0.035,
                             NUR_GQ_F93 = NUR_GQ_F85p * 0.035,  
                             NUR_GQ_F94 = NUR_GQ_F85p * 0.035,  
                             NUR_GQ_F95 = NUR_GQ_F85p * 0.035,  
                             NUR_GQ_F96 = NUR_GQ_F85p * 0.025,
                             NUR_GQ_F97 = NUR_GQ_F85p * 0.025,
                             NUR_GQ_F98 = NUR_GQ_F85p * 0.025,
                             NUR_GQ_F99 = NUR_GQ_F85p * 0.025,
                             NUR_GQ_F100 = NUR_GQ_F85p * 0.025,
                             NUR_GQ_F101 = NUR_GQ_F85p * 0.01,
                             NUR_GQ_F102 = NUR_GQ_F85p * 0.01,
                             NUR_GQ_F103 = NUR_GQ_F85p * 0.01,
                             NUR_GQ_F104 = NUR_GQ_F85p * 0.005,
                             NUR_GQ_F105 = NUR_GQ_F85p * 0.005)

### Subset to only yearly data
nc_bg_est_yearly %<>% select(BGGEOID, NUR_GQ_M00:NUR_GQ_F105)

#### Round (do in loop)
# Create copy
nc_bg_est_y_round <- nc_bg_est_yearly

for (i in 1:nrow(nc_bg_est_y_round)) {
  
  ## Replace values manually
  nc_bg_est_y_round[i,2:19] <- round_preserve_sum(nc_bg_est_y_round[i,2:19], 0)
  nc_bg_est_y_round[i,20:66] <- round_preserve_sum(nc_bg_est_y_round[i,20:66], 0)
  nc_bg_est_y_round[i,67:107] <- round_preserve_sum(nc_bg_est_y_round[i,67:107], 0)
  nc_bg_est_y_round[i,108:125] <- round_preserve_sum(nc_bg_est_y_round[i,108:125], 0)
  nc_bg_est_y_round[i,126:172] <- round_preserve_sum(nc_bg_est_y_round[i,126:172], 0)
  nc_bg_est_y_round[i,173:213] <- round_preserve_sum(nc_bg_est_y_round[i,173:213], 0)
  
}

### Total NURr GQ 18-64, Men
nc_bg_est_y_round %>% select(NUR_GQ_M18:NUR_GQ_M64) %>% sum()
sum(gq_age$GQ_M1864_INST_NUR)

### Total NURr GQ 18-64, Female
nc_bg_est_y_round %>% select(NUR_GQ_F18:NUR_GQ_F64) %>% sum()
sum(gq_age$GQ_F1864_INST_NUR)

### Total NURr GQ 65+, Female
nc_bg_est_y_round %>% select(NUR_GQ_F65:NUR_GQ_F105) %>% sum()
sum(gq_age$GQ_F65p_INST_NUR)



###
### Assign GQ residents to nursing facilities ---- 
###

### Create a vector with facility ids
### Military reserved     450000001 - 450000100
### Correctional reserved 450000101 - 450001000
### Dormitories reserved  450001001 - 450001500
potential_ids <- 450002001:450004000

### Create a vector with people ids
### Military reserved     938000001 - 938100000
### Correctional reserved 938100001 - 938300000
### Dormitories reserved  938300001 - 938800000
potential_people_ids <- 938800001:939000000

### Make a sum for each group in BG level by by year table
nc_bg_est_y_round %<>% mutate(NUR_GQ = rowSums(across(NUR_GQ_M00:NUR_GQ_F105)))

### Join sum of facilities in each BG to BG
### Aggregate count by BG
nurs_bg <- nurs %>% st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(FAC = n(), BEDS = sum(BEDS))

## Join
nc_bg_est_y_round %<>% left_join(nurs_bg,
                                 by = c("BGGEOID" = "GEOID"))

## Replace NA with 0
nc_bg_est_y_round %<>% mutate(FAC = replace_na(FAC, 0))
nc_bg_est_y_round %<>% mutate(BEDS = replace_na(BEDS, 0))

## Sort by BG
nc_bg_est_y_round %<>% arrange(BGGEOID)

#### Start looping through each BG ----
for (i in 1:nrow(nc_bg_est_y_round)) {
  
  ## Get vector with pops
  bg_nur_pop <- nc_bg_est_y_round %>%
    slice(i) %>%
    select(NUR_GQ) %>%
    as.numeric()
  bg_nur_facs <- nc_bg_est_y_round %>%
    slice(i) %>%
    select(FAC) %>%
    as.numeric()
  bg_nur_beds <- nc_bg_est_y_round %>%
    slice(i) %>%
    select(BEDS) %>%
    as.numeric()
  
  ##### If ZERO existing facilities, create random facs ----
  if (bg_nur_facs == 0) {
    
    ## Get number of median sized facilities
    n_facs_req <- (bg_nur_pop / nurs_beds_median) %>% ceiling()
    
    ## Get polygon of BG
    bg_poly <- nc_bg %>% 
      filter(GEOID == nc_bg_est_y_round$BGGEOID[i])
    
    ## Create random facs from inside polygon
    facs <- st_sample(x = bg_poly, 
                      size = n_facs_req, 
                      type = "random") %>%
      st_coordinates() %>%
      as.data.frame()
    
    ## Create set of values for facility sizes
    facs_sizes <- rep(bg_nur_pop / n_facs_req, n_facs_req) %>% round_preserve_sum()
    
    ## Create initial data holder
    nc_bg_nur_gq <- tibble(sp_id = potential_ids[1:n_facs_req],
                           gq_type = "N",
                           stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                           persons = facs_sizes,
                           latitude = facs$Y,
                           longitude = facs$X)
    
    ## Remove values from potential ids
    potential_ids <- potential_ids[-c(1:n_facs_req)]
    
    ### Convert data to long format
    nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
      slice(i) %>%
      select(NUR_GQ_M00:NUR_GQ_F105) %>%
      pivot_longer(cols = NUR_GQ_M00:NUR_GQ_F105,
                   names_to = "AGE",
                   values_to = "COUNT") %>%
      filter(COUNT > 0)
    
    ### Extract Sex, Age
    nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 11),
                                    sex = str_sub(AGE, 8, 8))
    
    ### Expand fac info to append to person info
    nc_bg_nur_gq_exp <- nc_bg_nur_gq %>%
      select(sp_gq_id = sp_id, persons) %>%
      uncount(persons)
    
    ### Expand into a person table
    nc_nur_gq_persons <- nc_bg_est_y_round_l %>% 
      select(-AGE) %>%
      uncount(COUNT)
    
    ### Bind
    nc_nur_gq_persons %<>% bind_cols(nc_bg_nur_gq_exp)
    
    ### Randomize GQ of people
    nc_nur_gq_persons %<>% mutate(sp_gq_id = sample(x = sp_gq_id, size = length(sp_gq_id))) %>% arrange(sp_gq_id)
    
    ### Add ID value
    nc_nur_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_nur_gq_persons)])
    
    ### Remove IDs from potential
    potential_people_ids <- potential_people_ids[-c(1:nrow(nc_nur_gq_persons))]
    
    ### Reorder columns
    nc_nur_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
    
    ## Ends 0 facilities if statement
  
  #### If non-0 existing facilities ----  
  } else {
    
    ## If statement to determine if additional facilities are needed
    ## Use expansion factor to account for facility reporting (10%)
    ###### First, no additional facilities needed ----
    if (ceiling(bg_nur_beds * 1.1) >= bg_nur_pop) {
      
      ## Calculate occupancy
      occ <- bg_nur_pop / bg_nur_beds
      
      ## Get BEDS from facilities
      nurs_sub <- nurs %>% 
        filter(GEOID == nc_bg_est_y_round$BGGEOID[i])
      
      ## Determine sizes
      facs_sizes <- (nurs_sub$BEDS * occ) %>% round_preserve_sum()
      
      ## Create initial data holder
      nc_bg_nur_gq <- tibble(sp_id = potential_ids[1:bg_nur_facs],
                             gq_type = "N",
                             stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                             persons = facs_sizes,
                             latitude = nurs_sub$Y,
                             longitude = nurs_sub$X)
      
      ## Remove values from potential ids
      potential_ids <- potential_ids[-c(1:bg_nur_facs)]
      
      ### Convert data to long format
      nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
        slice(i) %>%
        select(NUR_GQ_M00:NUR_GQ_F105) %>%
        pivot_longer(cols = NUR_GQ_M00:NUR_GQ_F105,
                     names_to = "AGE",
                     values_to = "COUNT") %>%
        filter(COUNT > 0)
      
      ### Extract Sex, Age
      nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 11),
                                      sex = str_sub(AGE, 8, 8))
      
      ### Expand fac info to append to person info
      nc_bg_nur_gq_exp <- nc_bg_nur_gq %>%
        select(sp_gq_id = sp_id, persons) %>%
        uncount(persons)
      
      ### Expand into a person table
      nc_nur_gq_persons <- nc_bg_est_y_round_l %>% 
        select(-AGE) %>%
        uncount(COUNT)
      
      ### Bind
      nc_nur_gq_persons %<>% bind_cols(nc_bg_nur_gq_exp)
      
      ### Randomize GQ of people
      nc_nur_gq_persons %>% mutate(sp_gq_id = sample(x = sp_gq_id, size = length(sp_gq_id))) %>% arrange(sp_gq_id)
      
      ### Add ID value
      nc_nur_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_nur_gq_persons)])
      
      ### Remove IDs from potential
      potential_people_ids <- potential_people_ids[-c(1:nrow(nc_nur_gq_persons))]
      
      ### Reorder columns
      nc_nur_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
    
    #### Additional facilities needed ----
    } else {
      
      ## Get number of median sized facilities needed
      n_facs_req <- ((bg_nur_pop - bg_nur_beds) / nurs_beds_median) %>% ceiling()
      
      ## Get polygon of BG
      bg_poly <- nc_bg %>% 
        filter(GEOID == nc_bg_est_y_round$BGGEOID[i])
      
      ## Create random facs from inside polygon
      facs <- st_sample(x = bg_poly, 
                        size = n_facs_req, 
                        type = "random") %>%
        st_coordinates() %>%
        as.data.frame()
      
      ## Calculate occupancy
      occ <- bg_nur_pop / (bg_nur_beds + (n_facs_req * nurs_beds_median))
      
      ## Get BEDS from facilities
      nurs_sub <- nurs %>% 
        filter(GEOID == nc_bg_est_y_round$BGGEOID[i])
      
      ## Determine sizes
      facs_sizes <- (c(nurs_sub$BEDS, rep(nurs_beds_median, n_facs_req)) * occ) %>% round_preserve_sum()
      
      ## Create initial data holder
      nc_bg_nur_gq <- tibble(sp_id = potential_ids[1:(bg_nur_facs + n_facs_req)],
                             gq_type = "N",
                             stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                             persons = facs_sizes,
                             latitude = c(nurs_sub$Y, facs$Y),
                             longitude = c(nurs_sub$X, facs$X))
      
      ## Remove values from potential ids
      potential_ids <- potential_ids[-c(1:(bg_nur_facs + n_facs_req))]
      
      ### Convert data to long format
      nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
        slice(i) %>%
        select(NUR_GQ_M00:NUR_GQ_F105) %>%
        pivot_longer(cols = NUR_GQ_M00:NUR_GQ_F105,
                     names_to = "AGE",
                     values_to = "COUNT") %>%
        filter(COUNT > 0)
      
      ### Extract Sex, Age
      nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 11),
                                      sex = str_sub(AGE, 8, 8))
      
      ### Expand fac info to append to person info
      nc_bg_nur_gq_exp <- nc_bg_nur_gq %>%
        select(sp_gq_id = sp_id, persons) %>%
        uncount(persons)
      
      ### Expand into a person table
      nc_nur_gq_persons <- nc_bg_est_y_round_l %>% 
        select(-AGE) %>%
        uncount(COUNT)
      
      ### Bind
      nc_nur_gq_persons %<>% bind_cols(nc_bg_nur_gq_exp)
      
      ### Randomize GQ of people
      nc_nur_gq_persons %<>% mutate(sp_gq_id = sample(x = sp_gq_id, size = length(sp_gq_id))) %>% arrange(sp_gq_id)
      
      ### Add ID value
      nc_nur_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_nur_gq_persons)])
      
      ### Remove IDs from potential
      potential_people_ids <- potential_people_ids[-c(1:nrow(nc_nur_gq_persons))]
      
      ### Reorder columns
      nc_nur_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
      
    }
  
    
  }
  
  
  ##### Put in state level holder ----
  if (i == 1) {
    
    nc_bg_nur_gq_all <- nc_bg_nur_gq
    nc_nur_gq_persons_all <- nc_nur_gq_persons
    
  } else {
    
    nc_bg_nur_gq_all %<>% bind_rows(nc_bg_nur_gq)
    nc_nur_gq_persons_all %<>% bind_rows(nc_nur_gq_persons)
    
  }
  
}


### Write out data ----

### Write out file
nc_bg_nur_gq_all %>% write_csv(file.path(output_path, "NC_nur_gq.csv"))

### Write out
nc_nur_gq_persons_all %>% write_csv(file.path(output_path, "NC_nur_gq_people.csv"))
