# =========================================================
# Title: Correctional facilities group quarters generation
# Author: Paul Delamater
# Date: 2025-10-28
# Description: Estimate correctional facilities group quarters population
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
### Block group GQ Correctional data ----
###

### Read in the Census table with detailed counts by block
gq_age <- read_csv(file.path(data_path, "NC_block_groupquarters_population_age.csv"))

### Create a block group, tract, and county GEOID and aggregate
gq_age %<>% mutate(BGGEOID = str_sub(GEOID, 1, 12),
                   TCTGEOID = str_sub(GEOID, 1, 11),
                   FIPS = str_sub(GEOID, 1, 5))

### Create a couple of sum columns (all Correctional residents, CORR + JUV)
gq_age %<>% mutate(GQ_M0017_INST_COR = GQ_M0017_INST_COR + GQ_M0017_INST_JUV,
                   GQ_M1864_INST_COR = GQ_M1864_INST_COR + GQ_M1864_INST_JUV,
                   GQ_M65p_INST_COR = GQ_M65p_INST_COR + GQ_M65p_INST_JUV,
                   GQ_F0017_INST_COR = GQ_F0017_INST_COR + GQ_F0017_INST_JUV,
                   GQ_F1864_INST_COR = GQ_F1864_INST_COR + GQ_F1864_INST_JUV,
                   GQ_F65p_INST_COR = GQ_F65p_INST_COR + GQ_F65p_INST_JUV,
                   GQ_M_INST_COR = GQ_M0017_INST_COR + GQ_M1864_INST_COR + GQ_M65p_INST_COR,
                   GQ_F_INST_COR = GQ_F0017_INST_COR + GQ_F1864_INST_COR + GQ_F65p_INST_COR,
                   GQ_INST_COR = GQ_M_INST_COR + GQ_F_INST_COR)

### For now, aggregate correctional GQ pop by Block Group
gq_age_bg <- gq_age %>% select(BGGEOID, contains("COR")) %>%
  group_by(BGGEOID) %>%
  summarize_all(sum)

### But also, take the block level data forward
gq_age %<>% select(GEOID, BGGEOID, TCTGEOID, FIPS, contains("COR"))

### Subset to only units with corr population
gq_age %<>% filter(GQ_INST_COR > 0)
gq_age_bg %<>% filter(GQ_INST_COR > 0)

### Make Type Variables for splitting purposes
gq_age_bg %<>% mutate(GQ_M_JUV = GQ_M0017_INST_COR > 0,
                      GQ_M_ADU = (GQ_M1864_INST_COR + GQ_M65p_INST_COR) > 0,
                      GQ_F_JUV = GQ_F0017_INST_COR > 0,
                      GQ_F_ADU = (GQ_F1864_INST_COR + GQ_F65p_INST_COR) > 0,
                      GQ_TOTAL = rowSums(across(GQ_M_JUV:GQ_F_ADU)))
gq_age %<>% mutate(GQ_M_JUV = GQ_M0017_INST_COR > 0,
                      GQ_M_ADU = (GQ_M1864_INST_COR + GQ_M65p_INST_COR) > 0,
                      GQ_F_JUV = GQ_F0017_INST_COR > 0,
                      GQ_F_ADU = (GQ_F1864_INST_COR + GQ_F65p_INST_COR) > 0,
                      GQ_TOTAL = rowSums(across(GQ_M_JUV:GQ_F_ADU)))

###
### Evaluate Correctional Facility Block/BG overlap ----
### (p. 5)
###

### Read NC Block Group data
nc_bg <- read_sf(file.path(data_path, "Block_groups_TIGER_2020_NC.gpkg"))

### Read NC Block data
nc_blk <- read_sf(file.path(data_path, "Blocks_popgt0_TIGER_2020_NC.gpkg"))

### Read in HIFLD Correctional Facilities data for NC
corr <- read_sf(file.path(data_path, "NC_Prison_Boundaries.gpkg"))

### Convert to centroid
corr %<>% st_centroid()

### Project to WGS 84
corr %<>% st_transform(crs = 4326)
nc_bg %<>% st_transform(crs = 4326)
nc_blk %<>% st_transform(crs = 4326)

### Spatial Join, Block Group
corr %<>% select(FACILITYID, NAME, TYPE, STATUS, POPULATION) %>%
  filter(STATUS == "OPEN") %>%
  st_join(nc_bg %>% select(GEOID))

### Spatial Join, Block
corr %<>% st_join(nc_blk %>% select(GEOID20))

### Summary of overlap
table(corr$GEOID) %>% sort()
table(corr$GEOID20) %>% sort()

### Merge BG with Correctional Pop data
nc_bg_gq_age <- nc_bg %>% 
  select(GEOID) %>%
  left_join(gq_age_bg,
            by = c("GEOID" = "BGGEOID")) 

### Subset to only BG with Corr GQ residents
nc_bg_gq_age %<>% filter(GQ_TOTAL > 0)

### Plot
mapview(nc_bg_gq_age, 
        col.regions = "blue") +
  mapview(corr, 
          col.regions = "black",
          cex = 4,
          alpha = 1)

###
### County Group Quarters data ----
###   not used in original method
###

### Read data with more detailed age for correctional GQ members
gq_age_detail_cty <- read_csv(file.path(data_path, "NC_county_groupquarters_corr_population_age_detail_PCO3.csv"))
gq_age_detail_cty2 <- read_csv(file.path(data_path, "NC_county_groupquarters_juv_population_age_detail_PCO4.csv"))

### Join the tables
gq_age_detail_cty %<>% left_join(gq_age_detail_cty2,
                                 by = "GEOID")

### Add juvenile pop to corr pop
gq_age_detail_cty %<>% mutate(GQ_TOTAL_INST_COR = GQ_TOTAL_INST_COR + GQ_TOTAL_INST_JUV,
                              GQ_M_INST_COR = GQ_M_INST_COR + GQ_M_INST_JUV,
                              GQ_F_INST_COR = GQ_F_INST_COR + GQ_F_INST_JUV,
                              GQ_M0019_INST_COR = GQ_M0019_INST_COR + GQ_M0004_INST_JUV + GQ_M0509_INST_JUV + GQ_M1014_INST_JUV + GQ_M1519_INST_JUV,
                              GQ_F0019_INST_COR = GQ_F0019_INST_COR + GQ_F0004_INST_JUV + GQ_F0509_INST_JUV + GQ_F1014_INST_JUV + GQ_F1519_INST_JUV,
                              GQ_M2024_INST_COR = GQ_M2024_INST_COR + GQ_M2024_INST_JUV,
                              GQ_M2529_INST_COR = GQ_M2529_INST_COR + GQ_M25p_INST_JUV,
                              GQ_F2024_INST_COR = GQ_F2024_INST_COR + GQ_F2024_INST_JUV,
                              GQ_F2529_INST_COR = GQ_F2529_INST_COR + GQ_F25p_INST_JUV)
                              
### Remove juvenile data (for now)
gq_age_detail_cty %<>% select(GEOID, ends_with("COR"))

### Aggregate the block level data to county (for 0-18, 18-20)
gq_age_cty <- gq_age %>% select(FIPS, contains("COR")) %>% 
  group_by(FIPS) %>%
  summarize_all(sum)

### Join the two tables
gq_age_detail_cty %<>% mutate(GEOID = as.character(GEOID)) %>%
  left_join(gq_age_cty %>% select(-GQ_M_INST_COR, -GQ_F_INST_COR),
            by = c("GEOID" = "FIPS"))

### Calculate number of people 18-19
gq_age_detail_cty %<>% mutate(GQ_M1819_INST_COR = GQ_M0019_INST_COR - GQ_M0017_INST_COR,
                              GQ_F1819_INST_COR = GQ_F0019_INST_COR - GQ_F0017_INST_COR)

### Calculate number of people 65-84
### Actually, we don't need this because the values
### Don't cross each other
# gq_age_detail_cty %<>% mutate(GQ_M6584_INST_COR = GQ_M65p_INST_COR - GQ_M85p_INST_COR,
#                               GQ_F6584_INST_COR = GQ_F65p_INST_COR - GQ_F85p_INST_COR)

### Calculate probabilities at the county level for each sex
### for 18-84, we'll do 0-18 later
gq_age_detail_cty %<>% mutate(GQ_PROB_M1819 = GQ_M1819_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M2024 = GQ_M2024_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M2529 = GQ_M2529_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M3034 = GQ_M3034_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M3539 = GQ_M3539_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M4044 = GQ_M4044_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M4549 = GQ_M4549_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M5054 = GQ_M5054_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M5559 = GQ_M5559_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M6064 = GQ_M6064_INST_COR / GQ_M1864_INST_COR,
                              GQ_PROB_M6569 = GQ_M6569_INST_COR / GQ_M65p_INST_COR,
                              GQ_PROB_M7074 = GQ_M7074_INST_COR / GQ_M65p_INST_COR,
                              GQ_PROB_M7579 = GQ_M7579_INST_COR / GQ_M65p_INST_COR,
                              GQ_PROB_M8084 = GQ_M8084_INST_COR / GQ_M65p_INST_COR,
                              GQ_PROB_M85p = GQ_M85p_INST_COR / GQ_M65p_INST_COR,
                              GQ_PROB_F1819 = GQ_F1819_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F2024 = GQ_F2024_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F2529 = GQ_F2529_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F3034 = GQ_F3034_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F3539 = GQ_F3539_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F4044 = GQ_F4044_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F4549 = GQ_F4549_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F5054 = GQ_F5054_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F5559 = GQ_F5559_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F6064 = GQ_F6064_INST_COR / GQ_F1864_INST_COR,
                              GQ_PROB_F6569 = GQ_F6569_INST_COR / GQ_F65p_INST_COR,
                              GQ_PROB_F7074 = GQ_F7074_INST_COR / GQ_F65p_INST_COR,
                              GQ_PROB_F7579 = GQ_F7579_INST_COR / GQ_F65p_INST_COR,
                              GQ_PROB_F8084 = GQ_F8084_INST_COR / GQ_F65p_INST_COR,
                              GQ_PROB_F85p = GQ_F85p_INST_COR / GQ_F65p_INST_COR)

### Now, work with the 0-18 people
### First, rejoin the 0-20 columns from the original data
### Join the tables
gq_age_detail_cty %<>% left_join(gq_age_detail_cty2 %>% 
                                   mutate(GEOID = as.character(GEOID)) %>%
                                   select(GEOID,
                                          GQ_M0004_INST_JUV,
                                          GQ_M0509_INST_JUV,
                                          GQ_M1014_INST_JUV,
                                          GQ_M1519_INST_JUV,
                                          GQ_F0004_INST_JUV,
                                          GQ_F0509_INST_JUV,
                                          GQ_F1014_INST_JUV,
                                          GQ_F1519_INST_JUV),
                                 by = "GEOID")

## Get 15-17 population by subtracting total from first three groups
gq_age_detail_cty %<>% mutate(GQ_M1517_INST_JUV = GQ_M0017_INST_COR - (GQ_M0004_INST_JUV + GQ_M0509_INST_JUV + GQ_M1014_INST_JUV),
                              GQ_F1517_INST_JUV = GQ_F0017_INST_COR - (GQ_F0004_INST_JUV + GQ_F0509_INST_JUV + GQ_F1014_INST_JUV))

gq_age_detail_cty %>% select(GEOID, GQ_M0019_INST_COR, 
                             GQ_M1819_INST_COR,
                             GQ_M0017_INST_COR, 
                             GQ_M0004_INST_JUV, 
                             GQ_M0509_INST_JUV, 
                             GQ_M1014_INST_JUV,
                             GQ_M1517_INST_JUV) %>% View()

### Calculate probabilities for 0-17
gq_age_detail_cty %<>% mutate(GQ_PROB_M0004 = GQ_M0004_INST_JUV / GQ_M0017_INST_COR,
                              GQ_PROB_M0509 = GQ_M0509_INST_JUV / GQ_M0017_INST_COR,
                              GQ_PROB_M1014 = GQ_M1014_INST_JUV / GQ_M0017_INST_COR,
                              GQ_PROB_M1517 = GQ_M1517_INST_JUV / GQ_M0017_INST_COR,
                              GQ_PROB_F0004 = GQ_F0004_INST_JUV / GQ_F0017_INST_COR,
                              GQ_PROB_F0509 = GQ_F0509_INST_JUV / GQ_F0017_INST_COR,
                              GQ_PROB_F1014 = GQ_F1014_INST_JUV / GQ_F0017_INST_COR,
                              GQ_PROB_F1517 = GQ_F1517_INST_JUV / GQ_F0017_INST_COR)
                              
### Replace NAs with 0
gq_age_detail_cty %<>% mutate(across(-GEOID, ~ replace_na(., 0)))

### Reorder columns a bit (organize probs)
gq_age_detail_cty %<>% select(GEOID, GQ_PROB_M0004:GQ_PROB_M1517, GQ_PROB_M1819:GQ_PROB_M85p,
                                     GQ_PROB_F0004:GQ_PROB_F1517, GQ_PROB_F1819:GQ_PROB_F85p,
                                     GQ_TOTAL_INST_COR:GQ_F1819_INST_COR, GQ_M0004_INST_JUV:GQ_F1517_INST_JUV)



###
### Estimate Block Group level age group distribution ----
###   based on county (state is later)
###

### Add FIPS field to BG level data
gq_age_bg %<>% mutate(FIPS = str_sub(BGGEOID, 1, 5))

### Attach county level probabilities to block group level data
gq_age_bg %<>% left_join(gq_age_detail_cty %>% select(GEOID, GQ_PROB_M0004:GQ_PROB_F85p),
                     by = c("FIPS" = "GEOID"))

### Multiply count by probability for age categories... carry
### forward 0-17 and 65+
nc_bg_est1 <- gq_age_bg %>% mutate(COR_GQ_M0004 = GQ_M0017_INST_COR * GQ_PROB_M0004,
                                   COR_GQ_M0509 = GQ_M0017_INST_COR * GQ_PROB_M0509,
                                   COR_GQ_M1014 = GQ_M0017_INST_COR * GQ_PROB_M1014,
                                   COR_GQ_M1517 = GQ_M0017_INST_COR * GQ_PROB_M1517,
                                   COR_GQ_M1819 = GQ_M1864_INST_COR * GQ_PROB_M1819,
                                   COR_GQ_M2024 = GQ_M1864_INST_COR * GQ_PROB_M2024,
                                   COR_GQ_M2529 = GQ_M1864_INST_COR * GQ_PROB_M2529,
                                   COR_GQ_M3034 = GQ_M1864_INST_COR * GQ_PROB_M3034,
                                   COR_GQ_M3539 = GQ_M1864_INST_COR * GQ_PROB_M3539,
                                   COR_GQ_M4044 = GQ_M1864_INST_COR * GQ_PROB_M4044,
                                   COR_GQ_M4549 = GQ_M1864_INST_COR * GQ_PROB_M4549,
                                   COR_GQ_M5054 = GQ_M1864_INST_COR * GQ_PROB_M5054,
                                   COR_GQ_M5559 = GQ_M1864_INST_COR * GQ_PROB_M5559,
                                   COR_GQ_M6064 = GQ_M1864_INST_COR * GQ_PROB_M6064,
                                   COR_GQ_M6569 = GQ_M65p_INST_COR * GQ_PROB_M6569,
                                   COR_GQ_M7074 = GQ_M65p_INST_COR * GQ_PROB_M7074,
                                   COR_GQ_M7579 = GQ_M65p_INST_COR * GQ_PROB_M7579,
                                   COR_GQ_M8084 = GQ_M65p_INST_COR * GQ_PROB_M8084,
                                   COR_GQ_M85p = GQ_M65p_INST_COR * GQ_PROB_M85p,
                                   COR_GQ_F0004 = GQ_F0017_INST_COR * GQ_PROB_F0004,
                                   COR_GQ_F0509 = GQ_F0017_INST_COR * GQ_PROB_F0509,
                                   COR_GQ_F1014 = GQ_F0017_INST_COR * GQ_PROB_F1014,
                                   COR_GQ_F1517 = GQ_F0017_INST_COR * GQ_PROB_F1517,
                                   COR_GQ_F1819 = GQ_F1864_INST_COR * GQ_PROB_F1819,
                                   COR_GQ_F2024 = GQ_F1864_INST_COR * GQ_PROB_F2024,
                                   COR_GQ_F2529 = GQ_F1864_INST_COR * GQ_PROB_F2529,
                                   COR_GQ_F3034 = GQ_F1864_INST_COR * GQ_PROB_F3034,
                                   COR_GQ_F3539 = GQ_F1864_INST_COR * GQ_PROB_F3539,
                                   COR_GQ_F4044 = GQ_F1864_INST_COR * GQ_PROB_F4044,
                                   COR_GQ_F4549 = GQ_F1864_INST_COR * GQ_PROB_F4549,
                                   COR_GQ_F5054 = GQ_F1864_INST_COR * GQ_PROB_F5054,
                                   COR_GQ_F5559 = GQ_F1864_INST_COR * GQ_PROB_F5559,
                                   COR_GQ_F6064 = GQ_F1864_INST_COR * GQ_PROB_F6064,
                                   COR_GQ_F6569 = GQ_F65p_INST_COR * GQ_PROB_F6569,
                                   COR_GQ_F7074 = GQ_F65p_INST_COR * GQ_PROB_F7074,
                                   COR_GQ_F7579 = GQ_F65p_INST_COR * GQ_PROB_F7579,
                                   COR_GQ_F8084 = GQ_F65p_INST_COR * GQ_PROB_F8084,
                                   COR_GQ_F85p = GQ_F65p_INST_COR * GQ_PROB_F85p,) %>%
  select(BGGEOID, starts_with("COR"))

#### Round (do in loop)
# Create copy
nc_bg_est1_round <- nc_bg_est1 

for (i in 1:nrow(nc_bg_est1_round)) {
  
  ## Replace values manually
  nc_bg_est1_round[i,2:5] <- round_preserve_sum(nc_bg_est1_round[i,2:5], 0)
  nc_bg_est1_round[i,6:15] <- round_preserve_sum(nc_bg_est1_round[i,6:15], 0)
  nc_bg_est1_round[i,16:20] <- round_preserve_sum(nc_bg_est1_round[i,16:20], 0)
  nc_bg_est1_round[i,21:24] <- round_preserve_sum(nc_bg_est1_round[i,21:24], 0)
  nc_bg_est1_round[i,25:34] <- round_preserve_sum(nc_bg_est1_round[i,25:34], 0)
  nc_bg_est1_round[i,35:39] <- round_preserve_sum(nc_bg_est1_round[i,35:39], 0)
  
}

### Test some sums of the M/F population

### Total Corr GQ 0-17, Men
nc_bg_est1_round %>% st_drop_geometry %>% select(COR_GQ_M0004:COR_GQ_M1517) %>% sum()
sum(gq_age$GQ_M0017_INST_COR)

### Total Corr GQ 18-64, Men
nc_bg_est1_round %>% st_drop_geometry %>% select(COR_GQ_M1819:COR_GQ_M6064) %>% sum()
sum(gq_age$GQ_M1864_INST_COR)

### Total Corr GQ 18-64, Female
nc_bg_est1_round %>% st_drop_geometry %>% select(COR_GQ_F1819:COR_GQ_F6064) %>% sum()
sum(gq_age$GQ_F1864_INST_COR)


###
### Create age data by year ----
###

### No guidance from any external source on the distribution of people in
### Correctional facilities by year (of age), so distribute evenly across
### age groups

### Big long calculation because I'm not wanting to code
### something nice and sweet at the moment
nc_bg_est_yearly <- nc_bg_est1_round
nc_bg_est_yearly %<>% mutate(COR_GQ_M00 = COR_GQ_M0004 * 0.2,
                             COR_GQ_M01 = COR_GQ_M0004 * 0.2,
                             COR_GQ_M02 = COR_GQ_M0004 * 0.2,
                             COR_GQ_M03 = COR_GQ_M0004 * 0.2,
                             COR_GQ_M04 = COR_GQ_M0004 * 0.2,
                             COR_GQ_M05 = COR_GQ_M0509 * 0.2,
                             COR_GQ_M06 = COR_GQ_M0509 * 0.2,
                             COR_GQ_M07 = COR_GQ_M0509 * 0.2,
                             COR_GQ_M08 = COR_GQ_M0509 * 0.2,
                             COR_GQ_M09 = COR_GQ_M0509 * 0.2,
                             COR_GQ_M10 = COR_GQ_M1014 * 0.2,
                             COR_GQ_M11 = COR_GQ_M1014 * 0.2,
                             COR_GQ_M12 = COR_GQ_M1014 * 0.2,
                             COR_GQ_M13 = COR_GQ_M1014 * 0.2,
                             COR_GQ_M14 = COR_GQ_M1014 * 0.2,
                             COR_GQ_M15 = COR_GQ_M1517 * 1/3,
                             COR_GQ_M16 = COR_GQ_M1517 * 1/3,
                             COR_GQ_M17 = COR_GQ_M1517 * 1/3,
                             COR_GQ_M18 = COR_GQ_M1819 * 0.5,
                             COR_GQ_M19 = COR_GQ_M1819 * 0.5,
                             COR_GQ_M20 = COR_GQ_M2024 * 0.2,
                             COR_GQ_M21 = COR_GQ_M2024 * 0.2,
                             COR_GQ_M22 = COR_GQ_M2024 * 0.2,
                             COR_GQ_M23 = COR_GQ_M2024 * 0.2,
                             COR_GQ_M24 = COR_GQ_M2024 * 0.2,
                             COR_GQ_M25 = COR_GQ_M2529 * 0.2,
                             COR_GQ_M26 = COR_GQ_M2529 * 0.2,
                             COR_GQ_M27 = COR_GQ_M2529 * 0.2,
                             COR_GQ_M28 = COR_GQ_M2529 * 0.2,
                             COR_GQ_M29 = COR_GQ_M2529 * 0.2,
                             COR_GQ_M30 = COR_GQ_M3034 * 0.2,
                             COR_GQ_M31 = COR_GQ_M3034 * 0.2,
                             COR_GQ_M32 = COR_GQ_M3034 * 0.2,
                             COR_GQ_M33 = COR_GQ_M3034 * 0.2,
                             COR_GQ_M34 = COR_GQ_M3034 * 0.2,
                             COR_GQ_M35 = COR_GQ_M3539 * 0.2,
                             COR_GQ_M36 = COR_GQ_M3539 * 0.2,
                             COR_GQ_M37 = COR_GQ_M3539 * 0.2,
                             COR_GQ_M38 = COR_GQ_M3539 * 0.2,
                             COR_GQ_M39 = COR_GQ_M3539 * 0.2, 
                             COR_GQ_M40 = COR_GQ_M4044 * 0.2,
                             COR_GQ_M41 = COR_GQ_M4044 * 0.2,
                             COR_GQ_M42 = COR_GQ_M4044 * 0.2,
                             COR_GQ_M43 = COR_GQ_M4044 * 0.2,
                             COR_GQ_M44 = COR_GQ_M4044 * 0.2,
                             COR_GQ_M45 = COR_GQ_M4549 * 0.2,
                             COR_GQ_M46 = COR_GQ_M4549 * 0.2,
                             COR_GQ_M47 = COR_GQ_M4549 * 0.2,
                             COR_GQ_M48 = COR_GQ_M4549 * 0.2,
                             COR_GQ_M49 = COR_GQ_M4549 * 0.2,
                             COR_GQ_M50 = COR_GQ_M5054 * 0.2,
                             COR_GQ_M51 = COR_GQ_M5054 * 0.2,
                             COR_GQ_M52 = COR_GQ_M5054 * 0.2,
                             COR_GQ_M53 = COR_GQ_M5054 * 0.2,
                             COR_GQ_M54 = COR_GQ_M5054 * 0.2,
                             COR_GQ_M55 = COR_GQ_M5559 * 0.2,
                             COR_GQ_M56 = COR_GQ_M5559 * 0.2,
                             COR_GQ_M57 = COR_GQ_M5559 * 0.2,
                             COR_GQ_M58 = COR_GQ_M5559 * 0.2,
                             COR_GQ_M59 = COR_GQ_M5559 * 0.2,
                             COR_GQ_M60 = COR_GQ_M6064 * 0.2,
                             COR_GQ_M61 = COR_GQ_M6064 * 0.2,
                             COR_GQ_M62 = COR_GQ_M6064 * 0.2,
                             COR_GQ_M63 = COR_GQ_M6064 * 0.2,
                             COR_GQ_M64 = COR_GQ_M6064 * 0.2,
                             COR_GQ_M65 = COR_GQ_M6569 * 0.2,
                             COR_GQ_M66 = COR_GQ_M6569 * 0.2,
                             COR_GQ_M67 = COR_GQ_M6569 * 0.2,
                             COR_GQ_M68 = COR_GQ_M6569 * 0.2,
                             COR_GQ_M69 = COR_GQ_M6569 * 0.2,
                             COR_GQ_M70 = COR_GQ_M7074 * 0.2,
                             COR_GQ_M71 = COR_GQ_M7074 * 0.2,
                             COR_GQ_M72 = COR_GQ_M7074 * 0.2,
                             COR_GQ_M73 = COR_GQ_M7074 * 0.2,
                             COR_GQ_M74 = COR_GQ_M7074 * 0.2,
                             COR_GQ_M75 = COR_GQ_M7579 * 0.2,
                             COR_GQ_M76 = COR_GQ_M7579 * 0.2,
                             COR_GQ_M77 = COR_GQ_M7579 * 0.2,
                             COR_GQ_M78 = COR_GQ_M7579 * 0.2,
                             COR_GQ_M79 = COR_GQ_M7579 * 0.2,
                             COR_GQ_M80 = COR_GQ_M8084 * 0.2,
                             COR_GQ_M81 = COR_GQ_M8084 * 0.2,
                             COR_GQ_M82 = COR_GQ_M8084 * 0.2,
                             COR_GQ_M83 = COR_GQ_M8084 * 0.2,
                             COR_GQ_M84 = COR_GQ_M8084 * 0.2,
                             COR_GQ_M85 = COR_GQ_M85p * 0.125,  ### The original data has
                             COR_GQ_M86 = COR_GQ_M85p * 0.125,  ### people up to 92 years
                             COR_GQ_M87 = COR_GQ_M85p * 0.125,  ### in correctional facs
                             COR_GQ_M88 = COR_GQ_M85p * 0.125,
                             COR_GQ_M89 = COR_GQ_M85p * 0.125,
                             COR_GQ_M90 = COR_GQ_M85p * 0.125,
                             COR_GQ_M91 = COR_GQ_M85p * 0.125,
                             COR_GQ_M92 = COR_GQ_M85p * 0.125,
                             COR_GQ_F00 = COR_GQ_F0004 * 0.2,
                             COR_GQ_F01 = COR_GQ_F0004 * 0.2,
                             COR_GQ_F02 = COR_GQ_F0004 * 0.2,
                             COR_GQ_F03 = COR_GQ_F0004 * 0.2,
                             COR_GQ_F04 = COR_GQ_F0004 * 0.2,
                             COR_GQ_F05 = COR_GQ_F0509 * 0.2,
                             COR_GQ_F06 = COR_GQ_F0509 * 0.2,
                             COR_GQ_F07 = COR_GQ_F0509 * 0.2,
                             COR_GQ_F08 = COR_GQ_F0509 * 0.2,
                             COR_GQ_F09 = COR_GQ_F0509 * 0.2,
                             COR_GQ_F10 = COR_GQ_F1014 * 0.2,
                             COR_GQ_F11 = COR_GQ_F1014 * 0.2,
                             COR_GQ_F12 = COR_GQ_F1014 * 0.2,
                             COR_GQ_F13 = COR_GQ_F1014 * 0.2,
                             COR_GQ_F14 = COR_GQ_F1014 * 0.2,
                             COR_GQ_F15 = COR_GQ_F1517 * 1/3,
                             COR_GQ_F16 = COR_GQ_F1517 * 1/3,
                             COR_GQ_F17 = COR_GQ_F1517 * 1/3,
                             COR_GQ_F18 = COR_GQ_F1819 * 0.5,
                             COR_GQ_F19 = COR_GQ_F1819 * 0.5,
                             COR_GQ_F20 = COR_GQ_F2024 * 0.2,
                             COR_GQ_F21 = COR_GQ_F2024 * 0.2,
                             COR_GQ_F22 = COR_GQ_F2024 * 0.2,
                             COR_GQ_F23 = COR_GQ_F2024 * 0.2,
                             COR_GQ_F24 = COR_GQ_F2024 * 0.2,
                             COR_GQ_F25 = COR_GQ_F2529 * 0.2,
                             COR_GQ_F26 = COR_GQ_F2529 * 0.2,
                             COR_GQ_F27 = COR_GQ_F2529 * 0.2,
                             COR_GQ_F28 = COR_GQ_F2529 * 0.2,
                             COR_GQ_F29 = COR_GQ_F2529 * 0.2,
                             COR_GQ_F30 = COR_GQ_F3034 * 0.2,
                             COR_GQ_F31 = COR_GQ_F3034 * 0.2,
                             COR_GQ_F32 = COR_GQ_F3034 * 0.2,
                             COR_GQ_F33 = COR_GQ_F3034 * 0.2,
                             COR_GQ_F34 = COR_GQ_F3034 * 0.2,
                             COR_GQ_F35 = COR_GQ_F3539 * 0.2,
                             COR_GQ_F36 = COR_GQ_F3539 * 0.2,
                             COR_GQ_F37 = COR_GQ_F3539 * 0.2,
                             COR_GQ_F38 = COR_GQ_F3539 * 0.2,
                             COR_GQ_F39 = COR_GQ_F3539 * 0.2, 
                             COR_GQ_F40 = COR_GQ_F4044 * 0.2,
                             COR_GQ_F41 = COR_GQ_F4044 * 0.2,
                             COR_GQ_F42 = COR_GQ_F4044 * 0.2,
                             COR_GQ_F43 = COR_GQ_F4044 * 0.2,
                             COR_GQ_F44 = COR_GQ_F4044 * 0.2,
                             COR_GQ_F45 = COR_GQ_F4549 * 0.2,
                             COR_GQ_F46 = COR_GQ_F4549 * 0.2,
                             COR_GQ_F47 = COR_GQ_F4549 * 0.2,
                             COR_GQ_F48 = COR_GQ_F4549 * 0.2,
                             COR_GQ_F49 = COR_GQ_F4549 * 0.2,
                             COR_GQ_F50 = COR_GQ_F5054 * 0.2,
                             COR_GQ_F51 = COR_GQ_F5054 * 0.2,
                             COR_GQ_F52 = COR_GQ_F5054 * 0.2,
                             COR_GQ_F53 = COR_GQ_F5054 * 0.2,
                             COR_GQ_F54 = COR_GQ_F5054 * 0.2,
                             COR_GQ_F55 = COR_GQ_F5559 * 0.2,
                             COR_GQ_F56 = COR_GQ_F5559 * 0.2,
                             COR_GQ_F57 = COR_GQ_F5559 * 0.2,
                             COR_GQ_F58 = COR_GQ_F5559 * 0.2,
                             COR_GQ_F59 = COR_GQ_F5559 * 0.2,
                             COR_GQ_F60 = COR_GQ_F6064 * 0.2,
                             COR_GQ_F61 = COR_GQ_F6064 * 0.2,
                             COR_GQ_F62 = COR_GQ_F6064 * 0.2,
                             COR_GQ_F63 = COR_GQ_F6064 * 0.2,
                             COR_GQ_F64 = COR_GQ_F6064 * 0.2,
                             COR_GQ_F65 = COR_GQ_F6569 * 0.2,
                             COR_GQ_F66 = COR_GQ_F6569 * 0.2,
                             COR_GQ_F67 = COR_GQ_F6569 * 0.2,
                             COR_GQ_F68 = COR_GQ_F6569 * 0.2,
                             COR_GQ_F69 = COR_GQ_F6569 * 0.2,
                             COR_GQ_F70 = COR_GQ_F7074 * 0.2,
                             COR_GQ_F71 = COR_GQ_F7074 * 0.2,
                             COR_GQ_F72 = COR_GQ_F7074 * 0.2,
                             COR_GQ_F73 = COR_GQ_F7074 * 0.2,
                             COR_GQ_F74 = COR_GQ_F7074 * 0.2,
                             COR_GQ_F75 = COR_GQ_F7579 * 0.2,
                             COR_GQ_F76 = COR_GQ_F7579 * 0.2,
                             COR_GQ_F77 = COR_GQ_F7579 * 0.2,
                             COR_GQ_F78 = COR_GQ_F7579 * 0.2,
                             COR_GQ_F79 = COR_GQ_F7579 * 0.2,
                             COR_GQ_F80 = COR_GQ_F8084 * 0.2,
                             COR_GQ_F81 = COR_GQ_F8084 * 0.2,
                             COR_GQ_F82 = COR_GQ_F8084 * 0.2,
                             COR_GQ_F83 = COR_GQ_F8084 * 0.2,
                             COR_GQ_F84 = COR_GQ_F8084 * 0.2,
                             COR_GQ_F85 = COR_GQ_F85p * 0.125,  ### The original data has
                             COR_GQ_F86 = COR_GQ_F85p * 0.125,  ### people up to 92 years
                             COR_GQ_F87 = COR_GQ_F85p * 0.125,  ### in correctional facs
                             COR_GQ_F88 = COR_GQ_F85p * 0.125,
                             COR_GQ_F89 = COR_GQ_F85p * 0.125,
                             COR_GQ_F90 = COR_GQ_F85p * 0.125,
                             COR_GQ_F91 = COR_GQ_F85p * 0.125,
                             COR_GQ_F92 = COR_GQ_F85p * 0.125)

### Subset to only yearly data
nc_bg_est_yearly %<>% select(BGGEOID, COR_GQ_M00:COR_GQ_F92)

#### Round (do in loop)
# Create copy
nc_bg_est_y_round <- nc_bg_est_yearly

for (i in 1:nrow(nc_bg_est_y_round)) {
  
  ## Replace values manually
  nc_bg_est_y_round[i,2:19] <- round_preserve_sum(nc_bg_est_y_round[i,2:19], 0)
  nc_bg_est_y_round[i,20:66] <- round_preserve_sum(nc_bg_est_y_round[i,20:66], 0)
  nc_bg_est_y_round[i,67:94] <- round_preserve_sum(nc_bg_est_y_round[i,67:94], 0)
  nc_bg_est_y_round[i,95:112] <- round_preserve_sum(nc_bg_est_y_round[i,95:112], 0)
  nc_bg_est_y_round[i,113:159] <- round_preserve_sum(nc_bg_est_y_round[i,113:159], 0)
  nc_bg_est_y_round[i,160:187] <- round_preserve_sum(nc_bg_est_y_round[i,160:187], 0)
  
}

### Total Corr GQ 0-17, Men
nc_bg_est_y_round %>% select(COR_GQ_M00:COR_GQ_M17) %>% sum()
sum(gq_age$GQ_M0017_INST_COR)

### Total Corr GQ 18-64, Men
nc_bg_est_y_round %>% select(COR_GQ_M18:COR_GQ_M64) %>% sum()
sum(gq_age$GQ_M1864_INST_COR)

### Total Corr GQ 18-64, Female
nc_bg_est_y_round %>% select(COR_GQ_F18:COR_GQ_F64) %>% sum()
sum(gq_age$GQ_F1864_INST_COR)


###
### Assign GQ residents to correctional facilities ---- 
###

### Original methodology uses four groups to reflect
### reality of correctional facilities, juveniles/adults
### and M/F

### Create a vector with facility ids
### Military reserved 450000001 - 450000100
potential_ids <- 450000101:450001000

### Create a vector with people ids
### Military reserved 938000001 - 938100000
potential_people_ids <- 938100001:938300000

### Make a sum for each group in BG level by by year table
nc_bg_est_y_round %<>% mutate(COR_GQ_M0017 = rowSums(across(COR_GQ_M00:COR_GQ_M17)),
                              COR_GQ_M18p = rowSums(across(COR_GQ_M18:COR_GQ_M92)),
                              COR_GQ_F0017 = rowSums(across(COR_GQ_F00:COR_GQ_F17)),
                              COR_GQ_F18p = rowSums(across(COR_GQ_F18:COR_GQ_F92)))

### Join sum of facilities in each BG to BG
### Aggregate count by BG
bg_fac <- corr %>% st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(FAC = n())

## Join
nc_bg_est_y_round %<>% left_join(bg_fac,
                                 by = c("BGGEOID" = "GEOID"))

## Replace NA with 0
nc_bg_est_y_round %<>% mutate(FAC = replace_na(FAC, 0))

## Sort by BG
nc_bg_est_y_round %<>% arrange(BGGEOID)

#### Start looping through each BG ----
for (i in 1:nrow(nc_bg_est_y_round)) {
  
  ## Get vector with pops
  bg_cor_pops <- nc_bg_est_y_round %>%
    slice(i) %>%
    select(COR_GQ_M0017:COR_GQ_F18p) %>%
    as.numeric()

  ## Get the minimum number of facs needed (not accounting for size)
  bg_min_n_facs <- sum(bg_cor_pops > 0)
  
  ## Get the minimum number of facs needed (accounting for size)
  bg_split_facs <- sum(bg_cor_pops > 1721)  ## Largest size listed in correctional data
  
  ## Add to mimimum number
  bg_min_n_facs <- bg_min_n_facs + bg_split_facs
  
  ## Get number of facs
  bg_facs <- nc_bg_est_y_round %>%
    slice(i) %>%
    select(FAC) %>%
    as.numeric()
  
  ## Get size of facs without zeros
  facs_sizes <- bg_cor_pops[which(bg_cor_pops > 0)]
  
  ## Split if any greater than max allowed
  if (max(facs_sizes) > 1721) {
    
    # Get position of max
    which_max_size <- which.max(facs_sizes)
    
    # Split max
    max_split <- facs_sizes[which_max_size] / 2 %>%
      rep(times = 2)
    
    # Round
    max_split <- round_preserve_sum(max_split)
    
    # Reconstruct
    facs_sizes <- as.list(facs_sizes)
    facs_sizes[[which_max_size]] <- max_split
    facs_sizes <- unlist(facs_sizes)
    
  }
  
  ##### If ZERO existing facilities, create random facs ----
  if (bg_facs == 0) {

    ## Get polygon of BG
    bg_poly <- nc_bg %>% 
      filter(GEOID == nc_bg_est_y_round$BGGEOID[i])
    
    ## Create random facs from inside polygon
    facs <- st_sample(x = bg_poly, 
                      size = bg_min_n_facs, 
                      type = "random") %>%
      st_coordinates() %>%
      as.data.frame()
    
    ## Create initial data holder
    nc_bg_cor_gq <- tibble(sp_id = potential_ids[1:bg_min_n_facs],
                           gq_type = "P",
                           stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                           persons = facs_sizes,
                           latitude = facs$Y,
                           longitude = facs$X)
    
    ## Remove values from potential ids
    potential_ids <- potential_ids[-c(1:bg_min_n_facs)]
    
    ### Convert data to long format
    nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
      slice(i) %>%
      select(COR_GQ_M00:COR_GQ_F92) %>%
      pivot_longer(cols = COR_GQ_M00:COR_GQ_F92,
                   names_to = "AGE",
                   values_to = "COUNT") %>%
      filter(COUNT > 0)
   
    ### Extract Sex, Age
    nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 10),
                                    sex = str_sub(AGE, 8, 8))
    
    ### Expand fac info to append to person info
    nc_bg_cor_gq_exp <- nc_bg_cor_gq %>%
      select(sp_gq_id = sp_id, persons) %>%
      uncount(persons)
    
    ### Expand into a person table
    nc_cor_gq_persons <- nc_bg_est_y_round_l %>% 
      select(-AGE) %>%
      uncount(COUNT)
    
    ### Bind
    nc_cor_gq_persons %<>% bind_cols(nc_bg_cor_gq_exp)
    
    ### Add ID value
    nc_cor_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_cor_gq_persons)])
    
    ### Remove IDs from potential
    potential_people_ids <- potential_people_ids[-c(1:nrow(nc_cor_gq_persons))]
    
    ### Reorder columns
    nc_cor_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
    
    ## Ends 0 facilities if statement
    
  #### If number of existing facilities less than number needed, create random facs ----
    
  } else if (bg_min_n_facs > bg_facs)  {
    
    ## Get polygon of BG
    bg_poly <- nc_bg %>% 
      filter(GEOID == nc_bg_est_y_round$BGGEOID[i])
    
    ## Create random facs from inside polygon
    facs <- st_sample(x = bg_poly, 
                      size = bg_min_n_facs - bg_facs, 
                      type = "random") %>%
      st_coordinates() %>%
      as.data.frame()
    
    ## Get info of facs in BG
    corr_bg <- corr %>% filter(GEOID == nc_bg_est_y_round$BGGEOID[i]) %>%
      st_coordinates() %>%
      as.data.frame()
      
    ## Create initial data holder
    nc_bg_cor_gq <- tibble(sp_id = potential_ids[1:bg_min_n_facs],
                           gq_type = "P",
                           stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                           persons = facs_sizes,
                           latitude = c(corr_bg$Y, facs$Y),
                           longitude = c(corr_bg$X, facs$X)) 
    
    ## Remove values from potential ids
    potential_ids <- potential_ids[-c(1:bg_min_n_facs)]
    
    ### Convert data to long format
    nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
      slice(i) %>%
      select(COR_GQ_M00:COR_GQ_F92) %>%
      pivot_longer(cols = COR_GQ_M00:COR_GQ_F92,
                   names_to = "AGE",
                   values_to = "COUNT") %>%
      filter(COUNT > 0)
    
    ### Extract Sex, Age
    nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 10),
                                    sex = str_sub(AGE, 8, 8))
    
    ### Expand fac info to append to person info
    nc_bg_cor_gq_exp <- nc_bg_cor_gq %>%
      select(sp_gq_id = sp_id, persons) %>%
      uncount(persons)
    
    ### Expand into a person table
    nc_cor_gq_persons <- nc_bg_est_y_round_l %>% 
      select(-AGE) %>%
      uncount(COUNT)
    
    ### Bind
    nc_cor_gq_persons %<>% bind_cols(nc_bg_cor_gq_exp)
    
    ### Add ID value
    nc_cor_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_cor_gq_persons)])
    
    ### Remove IDs from potential
    potential_people_ids <- potential_people_ids[-c(1:nrow(nc_cor_gq_persons))]
    
    ### Reorder columns
    nc_cor_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
    
  ##### If number of existing facilities equals number needed ----
    
  } else if (bg_min_n_facs == bg_facs) {
    
    ## Get info of facs in BG
    corr_bg <- corr %>% filter(GEOID == nc_bg_est_y_round$BGGEOID[i]) %>%
      st_coordinates() %>%
      as.data.frame()
    
    ## Create initial data holder
    nc_bg_cor_gq <- tibble(sp_id = potential_ids[1:bg_min_n_facs],
                           gq_type = "P",
                           stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                           persons = facs_sizes,
                           latitude = c(corr_bg$Y),
                           longitude = c(corr_bg$X)) 
    
    ## Remove values from potential ids
    potential_ids <- potential_ids[-c(1:bg_min_n_facs)]
    
    ### Convert data to long format
    nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
      slice(i) %>%
      select(COR_GQ_M00:COR_GQ_F92) %>%
      pivot_longer(cols = COR_GQ_M00:COR_GQ_F92,
                   names_to = "AGE",
                   values_to = "COUNT") %>%
      filter(COUNT > 0)
    
    ### Extract Sex, Age
    nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 10),
                                    sex = str_sub(AGE, 8, 8))
    
    ### Expand fac info to append to person info
    nc_bg_cor_gq_exp <- nc_bg_cor_gq %>%
      select(sp_gq_id = sp_id, persons) %>%
      uncount(persons)
    
    ### Expand into a person table
    nc_cor_gq_persons <- nc_bg_est_y_round_l %>% 
      select(-AGE) %>%
      uncount(COUNT)
    
    ### Bind
    nc_cor_gq_persons %<>% bind_cols(nc_bg_cor_gq_exp)
    
    ### Add ID value
    nc_cor_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_cor_gq_persons)])
    
    ### Remove IDs from potential
    potential_people_ids <- potential_people_ids[-c(1:nrow(nc_cor_gq_persons))]
    
    ### Reorder columns
    nc_cor_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
    
    ##### If number of existing facilities greater than number needed ----
    
  } else if (bg_min_n_facs < bg_facs) {
    
    ## Get info of facs in BG
    corr_bg <- corr %>% filter(GEOID == nc_bg_est_y_round$BGGEOID[i]) %>%
      arrange(desc(POPULATION)) %>%
      st_coordinates() %>%
      as.data.frame() %>%
      slice(-nrow(.))
    
    ## Create initial data holder
    nc_bg_cor_gq <- tibble(sp_id = potential_ids[1:bg_min_n_facs],
                           gq_type = "P",
                           stcotrbg = nc_bg_est_y_round$BGGEOID[i],
                           persons = facs_sizes,
                           latitude = c(corr_bg$Y),
                           longitude = c(corr_bg$X)) 
    
    ## Remove values from potential ids
    potential_ids <- potential_ids[-c(1:bg_min_n_facs)]
    
    ### Convert data to long format
    nc_bg_est_y_round_l <- nc_bg_est_y_round %>%
      slice(i) %>%
      select(COR_GQ_M00:COR_GQ_F92) %>%
      pivot_longer(cols = COR_GQ_M00:COR_GQ_F92,
                   names_to = "AGE",
                   values_to = "COUNT") %>%
      filter(COUNT > 0)
    
    ### Extract Sex, Age
    nc_bg_est_y_round_l %<>% mutate(age = str_sub(AGE, 9, 10),
                                    sex = str_sub(AGE, 8, 8))
    
    ### Expand fac info to append to person info
    nc_bg_cor_gq_exp <- nc_bg_cor_gq %>%
      select(sp_gq_id = sp_id, persons) %>%
      uncount(persons)
    
    ### Expand into a person table
    nc_cor_gq_persons <- nc_bg_est_y_round_l %>% 
      select(-AGE) %>%
      uncount(COUNT)
    
    ### Bind
    nc_cor_gq_persons %<>% bind_cols(nc_bg_cor_gq_exp)
    
    ### Add ID value
    nc_cor_gq_persons %<>% mutate(sp_id = potential_people_ids[1:nrow(nc_cor_gq_persons)])
    
    ### Remove IDs from potential
    potential_people_ids <- potential_people_ids[-c(1:nrow(nc_cor_gq_persons))]
    
    ### Reorder columns
    nc_cor_gq_persons %<>% select(sp_id, sp_gq_id, age, sex)
    
  }
    
  ##### Put in state level holder ----
  if (i == 1) {
    
    nc_bg_cor_gq_all <- nc_bg_cor_gq
    nc_cor_gq_persons_all <- nc_cor_gq_persons
    
  } else {
    
    nc_bg_cor_gq_all %<>% bind_rows(nc_bg_cor_gq)
    nc_cor_gq_persons_all %<>% bind_rows(nc_cor_gq_persons)
    
  }
  
}


### Write out data ----

### Write out file
nc_bg_cor_gq_all %>% write_csv(file.path(output_path, "NC_cor_gq.csv"))

### Write out
nc_cor_gq_persons_all %>% write_csv(file.path(output_path, "NC_cor_gq_people.csv"))
