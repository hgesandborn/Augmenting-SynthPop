# =========================================================
# Title: Chronic condition assignment
# Author: Hilary Sandborn & Paul Delamater
# Date: 2025-10-31
# Description: Assign chronic conditions to agents in a synthetic population
# =========================================================


#### Duration: 6 minutes
##### Run round_preserve_sum.R before running this script ######


##### Load packages #####
## If necessary, use install.packages() to install packages
library(tidyverse)
library(magrittr)


##### Set seed for reproducibility #####
set.seed(100)


##### Set file paths #####
data_path <- "data/input/"
intermediate_path <- "data/intermediate/"
output_path <- "output/results/"


#### Load source code #####
R_script_comorbid_pcts <- "05.1_condition_prevalences.R"
source(R_script_comorbid_pcts)
source("05.2_condition_functions.R") 


## Table for storing state-level counts
count_sum <- tibble(COUNTY = rep(NA, 100),
                    COUNTY_ALL = NA, 
                    COUNTY_GTE18 = NA,
                    AGE_LTE9 = NA,
                    AGE_10_19 = NA, 
                    AGE_20_29 = NA,
                    AGE_30_39 = NA,
                    AGE_40_49 = NA,
                    AGE_50_59 = NA,
                    AGE_60_69 = NA,
                    AGE_70_79 = NA,
                    AGE_80_89 = NA,
                    AGE_GTE90 = NA,
                    WHITE = NA,
                    WHITE_GTE18 = NA, 
                    BLACK = NA,
                    BLACK_GTE18 = NA,
                    HISPANIC = NA,
                    HISPANIC_GTE18 = NA, 
                    NONHISPANIC = NA,
                    HYPER = NA,
                    OBESE = NA, 
                    ASTHMA = NA,
                    COPD = NA, 
                    DIAB = NA, 
                    NONE = NA,
                    AGE_LTE9_HYPER = NA,
                    AGE_10_19_HYPER = NA,
                    AGE_20_29_HYPER = NA,
                    AGE_30_39_HYPER = NA,
                    AGE_40_49_HYPER = NA,
                    AGE_50_59_HYPER = NA,
                    AGE_60_69_HYPER = NA,
                    AGE_70_79_HYPER = NA,
                    AGE_80_89_HYPER = NA,
                    AGE_GTE90_HYPER = NA,
                    AGE_LTE9_OBESE = NA,
                    AGE_10_19_OBESE = NA,
                    AGE_20_29_OBESE = NA,
                    AGE_30_39_OBESE = NA,
                    AGE_40_49_OBESE = NA,
                    AGE_50_59_OBESE = NA,
                    AGE_60_69_OBESE = NA,
                    AGE_70_79_OBESE = NA,
                    AGE_80_89_OBESE = NA,
                    AGE_GTE90_OBESE = NA,
                    AGE_LTE9_ASTHMA = NA,
                    AGE_10_19_ASTHMA = NA, 
                    AGE_20_29_ASTHMA = NA, 
                    AGE_30_39_ASTHMA = NA, 
                    AGE_40_49_ASTHMA = NA, 
                    AGE_50_59_ASTHMA = NA, 
                    AGE_60_69_ASTHMA = NA, 
                    AGE_70_79_ASTHMA = NA, 
                    AGE_80_89_ASTHMA = NA, 
                    AGE_GTE90_ASTHMA = NA, 
                    AGE_LTE9_COPD = NA, 
                    AGE_10_19_COPD = NA, 
                    AGE_20_29_COPD = NA, 
                    AGE_30_39_COPD = NA, 
                    AGE_40_49_COPD = NA, 
                    AGE_50_59_COPD = NA, 
                    AGE_60_69_COPD = NA, 
                    AGE_70_79_COPD = NA, 
                    AGE_80_89_COPD = NA, 
                    AGE_GTE90_COPD = NA, 
                    AGE_LTE9_DIAB = NA,
                    AGE_10_19_DIAB = NA,
                    AGE_20_29_DIAB = NA,
                    AGE_30_39_DIAB = NA,
                    AGE_40_49_DIAB = NA,
                    AGE_50_59_DIAB = NA,
                    AGE_60_69_DIAB = NA,
                    AGE_70_79_DIAB = NA,
                    AGE_80_89_DIAB = NA,
                    AGE_GTE90_DIAB = NA,
                    AGE_LTE9_NONE = NA, 
                    AGE_10_19_NONE = NA,
                    AGE_20_29_NONE = NA,
                    AGE_30_39_NONE = NA,
                    AGE_40_49_NONE = NA,
                    AGE_50_59_NONE = NA,
                    AGE_60_69_NONE = NA,
                    AGE_70_79_NONE = NA,
                    AGE_80_89_NONE = NA,
                    AGE_GTE90_NONE = NA,
                    AGE_18_44_WHITE = NA,
                    AGE_45_64_WHITE = NA,
                    AGE_65p_WHITE = NA,
                    AGE_18_44_BLACK = NA,
                    AGE_45_64_BLACK = NA,
                    AGE_65p_BLACK = NA)



## Tables for storing county-level outputs
white_pcts <- tibble(COUNTY = rep(NA, 100),
                    age_18_44_HYPER_true = NA,
                    age_18_44_HYPER_est = NA,
                    age_18_44_HYPER_diff = NA,
                    age_45_64_HYPER_true = NA,
                    age_45_64_HYPER_est = NA,
                    age_45_64_HYPER_diff = NA,
                    age_65p_HYPER_true = NA,
                    age_65p_HYPER_est = NA,
                    age_65p_HYPER_diff = NA,
                    age_18_44_OBESE_true = NA,
                    age_18_44_OBESE_est = NA,
                    age_18_44_OBESE_diff = NA,
                    age_45_64_OBESE_true = NA,
                    age_45_64_OBESE_est = NA,
                    age_45_64_OBESE_diff = NA,
                    age_65p_OBESE_true = NA,
                    age_65p_OBESE_est = NA,
                    age_65p_OBESE_diff = NA,
                    age_18_44_ASTHMA_true = NA,
                    age_18_44_ASTHMA_est = NA,
                    age_18_44_ASTHMA_diff = NA,
                    age_45_64_ASTHMA_true = NA,
                    age_45_64_ASTHMA_est = NA,
                    age_45_64_ASTHMA_diff = NA,
                    age_65p_ASTHMA_true = NA,
                    age_65p_ASTHMA_est = NA,
                    age_65p_ASTHMA_diff = NA,
                    age_18_44_COPD_true = NA,
                    age_18_44_COPD_est = NA,
                    age_18_44_COPD_diff = NA,
                    age_45_64_COPD_true = NA,
                    age_45_64_COPD_est = NA,
                    age_45_64_COPD_diff = NA,
                    age_65p_COPD_true = NA,
                    age_65p_COPD_est = NA,
                    age_65p_COPD_diff = NA,
                    age_18_44_DIAB_true = NA,
                    age_18_44_DIAB_est = NA,
                    age_18_44_DIAB_diff = NA,
                    age_45_64_DIAB_true = NA,
                    age_45_64_DIAB_est = NA,
                    age_45_64_DIAB_diff = NA,
                    age_65p_DIAB_true = NA,
                    age_65p_DIAB_est = NA,
                    age_65p_DIAB_diff = NA)

black_pcts <- tibble(COUNTY = rep(NA, 100),
                     age_18_44_HYPER_true = NA,
                     age_18_44_HYPER_est = NA,
                     age_18_44_HYPER_diff = NA,
                     age_45_64_HYPER_true = NA,
                     age_45_64_HYPER_est = NA,
                     age_45_64_HYPER_diff = NA,
                     age_65p_HYPER_true = NA,
                     age_65p_HYPER_est = NA,
                     age_65p_HYPER_diff = NA,
                     age_18_44_OBESE_true = NA,
                     age_18_44_OBESE_est = NA,
                     age_18_44_OBESE_diff = NA,
                     age_45_64_OBESE_true = NA,
                     age_45_64_OBESE_est = NA,
                     age_45_64_OBESE_diff = NA,
                     age_65p_OBESE_true = NA,
                     age_65p_OBESE_est = NA,
                     age_65p_OBESE_diff = NA,
                     age_18_44_ASTHMA_true = NA,
                     age_18_44_ASTHMA_est = NA,
                     age_18_44_ASTHMA_diff = NA,
                     age_45_64_ASTHMA_true = NA,
                     age_45_64_ASTHMA_est = NA,
                     age_45_64_ASTHMA_diff = NA,
                     age_65p_ASTHMA_true = NA,
                     age_65p_ASTHMA_est = NA,
                     age_65p_ASTHMA_diff = NA,
                     age_18_44_COPD_true = NA,
                     age_18_44_COPD_est = NA,
                     age_18_44_COPD_diff = NA,
                     age_45_64_COPD_true = NA,
                     age_45_64_COPD_est = NA,
                     age_45_64_COPD_diff = NA,
                     age_65p_COPD_true = NA,
                     age_65p_COPD_est = NA,
                     age_65p_COPD_diff = NA,
                     age_18_44_DIAB_true = NA,
                     age_18_44_DIAB_est = NA,
                     age_18_44_DIAB_diff = NA,
                     age_45_64_DIAB_true = NA,
                     age_45_64_DIAB_est = NA,
                     age_45_64_DIAB_diff = NA,
                     age_65p_DIAB_true = NA,
                     age_65p_DIAB_est = NA,
                     age_65p_DIAB_diff = NA)

rem_pcts <- tibble(COUNTY = rep(NA, 100),
                     age_18_44_HYPER_true = NA,
                     age_18_44_HYPER_est = NA,
                     age_18_44_HYPER_diff = NA,
                     age_45_64_HYPER_true = NA,
                     age_45_64_HYPER_est = NA,
                     age_45_64_HYPER_diff = NA,
                     age_65p_HYPER_true = NA,
                     age_65p_HYPER_est = NA,
                     age_65p_HYPER_diff = NA,
                     age_18_44_OBESE_true = NA,
                     age_18_44_OBESE_est = NA,
                     age_18_44_OBESE_diff = NA,
                     age_45_64_OBESE_true = NA,
                     age_45_64_OBESE_est = NA,
                     age_45_64_OBESE_diff = NA,
                     age_65p_OBESE_true = NA,
                     age_65p_OBESE_est = NA,
                     age_65p_OBESE_diff = NA,
                     age_18_44_ASTHMA_true = NA,
                     age_18_44_ASTHMA_est = NA,
                     age_18_44_ASTHMA_diff = NA,
                     age_45_64_ASTHMA_true = NA,
                     age_45_64_ASTHMA_est = NA,
                     age_45_64_ASTHMA_diff = NA,
                     age_65p_ASTHMA_true = NA,
                     age_65p_ASTHMA_est = NA,
                     age_65p_ASTHMA_diff = NA,
                     age_18_44_COPD_true = NA,
                     age_18_44_COPD_est = NA,
                     age_18_44_COPD_diff = NA,
                     age_45_64_COPD_true = NA,
                     age_45_64_COPD_est = NA,
                     age_45_64_COPD_diff = NA,
                     age_65p_COPD_true = NA,
                     age_65p_COPD_est = NA,
                     age_65p_COPD_diff = NA,
                     age_18_44_DIAB_true = NA,
                     age_18_44_DIAB_est = NA,
                     age_18_44_DIAB_diff = NA,
                     age_45_64_DIAB_true = NA,
                     age_45_64_DIAB_est = NA,
                     age_45_64_DIAB_diff = NA,
                     age_65p_DIAB_true = NA,
                     age_65p_DIAB_est = NA,
                     age_65p_DIAB_diff = NA)

hisp_pcts <- tibble(COUNTY = rep(NA, 100),
                   age_18_44_HYPER_true = NA,
                   age_18_44_HYPER_est = NA,
                   age_18_44_HYPER_diff = NA,
                   age_45_64_HYPER_true = NA,
                   age_45_64_HYPER_est = NA,
                   age_45_64_HYPER_diff = NA,
                   age_65p_HYPER_true = NA,
                   age_65p_HYPER_est = NA,
                   age_65p_HYPER_diff = NA,
                   age_18_44_OBESE_true = NA,
                   age_18_44_OBESE_est = NA,
                   age_18_44_OBESE_diff = NA,
                   age_45_64_OBESE_true = NA,
                   age_45_64_OBESE_est = NA,
                   age_45_64_OBESE_diff = NA,
                   age_65p_OBESE_true = NA,
                   age_65p_OBESE_est = NA,
                   age_65p_OBESE_diff = NA,
                   age_18_44_ASTHMA_true = NA,
                   age_18_44_ASTHMA_est = NA,
                   age_18_44_ASTHMA_diff = NA,
                   age_45_64_ASTHMA_true = NA,
                   age_45_64_ASTHMA_est = NA,
                   age_45_64_ASTHMA_diff = NA,
                   age_65p_ASTHMA_true = NA,
                   age_65p_ASTHMA_est = NA,
                   age_65p_ASTHMA_diff = NA,
                   age_18_44_COPD_true = NA,
                   age_18_44_COPD_est = NA,
                   age_18_44_COPD_diff = NA,
                   age_45_64_COPD_true = NA,
                   age_45_64_COPD_est = NA,
                   age_45_64_COPD_diff = NA,
                   age_65p_COPD_true = NA,
                   age_65p_COPD_est = NA,
                   age_65p_COPD_diff = NA,
                   age_18_44_DIAB_true = NA,
                   age_18_44_DIAB_est = NA,
                   age_18_44_DIAB_diff = NA,
                   age_45_64_DIAB_true = NA,
                   age_45_64_DIAB_est = NA,
                   age_45_64_DIAB_diff = NA,
                   age_65p_DIAB_true = NA,
                   age_65p_DIAB_est = NA,
                   age_65p_DIAB_diff = NA)


## create variables for saving table values
## TABLE 1
pop_18_44 <- 0
pop_45_64 <- 0
pop_65p <- 0
est_pct_hyper_18_44 <- 0
est_pct_hyper_45_64 <- 0
est_pct_hyper_65p <- 0
est_pct_obese_18_44 <- 0
est_pct_obese_45_64 <- 0
est_pct_obese_65p <- 0
est_pct_asthma_18_44 <- 0
est_pct_asthma_45_64 <- 0
est_pct_asthma_65p <- 0
est_pct_copd_18_44 <- 0
est_pct_copd_45_64 <- 0
est_pct_copd_65p <- 0
est_pct_diab_18_44 <- 0
est_pct_diab_45_64 <- 0
est_pct_diab_65p <- 0

## TABLE 2
pop_white <- 0
pop_black <- 0
est_pct_hyper_wh <- 0
est_pct_hyper_bl <- 0
est_pct_obese_wh <- 0
est_pct_obese_bl <- 0
est_pct_asthma_wh <- 0
est_pct_asthma_bl <- 0
est_pct_copd_wh <- 0
est_pct_copd_bl <- 0
est_pct_diab_wh <- 0
est_pct_diab_bl <- 0

## TABLE 3
est_pct_obese_hyper <- 0
est_pct_asthma_obese <- 0
est_pct_copd_asthma <- 0
est_pct_copd_hyper <- 0
est_pct_diab_obese <- 0
est_pct_diab_hyper <- 0
est_pct_diab_copd <- 0

## TABLE 4 
pop_hispanic <- 0
pop_non_hispanic <- 0
est_pct_hyper_hisp <- 0
est_pct_hyper_nonhisp <- 0
est_pct_obese_hisp <- 0
est_pct_obese_nonhisp <- 0
est_pct_asthma_hisp <- 0
est_pct_asthma_nonhisp <- 0
est_pct_copd_hisp <- 0
est_pct_copd_nonhisp <- 0
est_pct_diab_hisp <- 0
est_pct_diab_nonhisp <- 0



# List all files in output folder
e_files <- list.files(
  path = file.path(intermediate_path),
  pattern = "_(people|gq)\\.csv$",
  full.names = TRUE
)

# Extract county FIPS (everything before the underscore)
county_ids <- sub("_.*$", "", basename(e_files))

# Unique counties
unique_counties <- unique(county_ids)

# List to store combined data
combined_files <- list()

# Loop through each county
for (county in unique_counties) {
  
  # Get the two files for this county
  county_files <- e_files[grep(paste0("^", county, "_"), basename(e_files))]
  
  # Read and combine them
  df_combined <- do.call(rbind, lapply(county_files, read.csv))
  
  # Store in list
  combined_files[[county]] <- df_combined
}


##### Start assignment #####

## For loop
for (i in seq_along(combined_files)) {
  
  county <- names(combined_files)[i]
  
  # Create filepath
  fp <- paste0(file.path(intermediate_path), county, "_people.csv")
  fp_gq <- paste0(file.path(intermediate_path), county, "_gq.csv")

  # Print county
  print(paste0("County: ", county, " ---------------------------------------------------------------------------------"))
  
  ## Read the synthetic population text file in as a table -----
  dat <- read_csv(fp, col_names = TRUE, show_col_types = FALSE)
  dat_gq <- read_csv(fp_gq, col_names = TRUE, show_col_types = FALSE)
  
  ##
  ## Merge pop files
  ##

  ## Bind
  if (nrow(dat_gq) > 0) {
  dat %<>% bind_rows(dat_gq)
  }
  
  ## Find the total population and print it -----
  n_pop <- nrow(dat)
  
  ## Calculate eventual number of people that have each condition and print it -----
  n_hyper <- round((hyper_pct/100) * n_pop)
  n_obese <- round((obese_pct/100) * n_pop)
  n_asthma <- round((asthma_pct/100) * n_pop)
  n_copd <- round((copd_pct/100) * n_pop)
  n_diab <- round((diab_pct/100) * n_pop)
  
  
  
  ## Find number of people in each age group and print it
  pop <- list()
  #pop$age_0_17 <- sum(dat$age_cat == "0-17")
  pop$age_18_44 <- sum(dat$age_cat == "18-44")
  pop$age_45_64 <- sum(dat$age_cat == "45-64")
  pop$age_65p <- sum(dat$age_cat == "65p")
  
  pop_18_44 <- pop_18_44 + pop$age_18_44
  pop_45_64 <- pop_45_64 + pop$age_45_64
  pop_65p <- pop_65p + pop$age_65p
  
  ## Find number of people in each race group 
  pop_race <- list()
  pop_race$white <- sum(dat$race == 1)
  pop_race$black <- sum(dat$race == 2)
  pop_race$remaining <- sum(dat$race %in% c(0, 3:9))  
  
  
  pop_white <- pop_white + sum(dat$race == 1 & dat$age_cat != "0-17")
  pop_black <- pop_black + sum(dat$race == 2 & dat$age_cat != "0-17")
  
  pop_hispanic <- pop_hispanic + sum(dat$eth == "H")
  pop_non_hispanic <- pop_non_hispanic + sum(dat$eth == "NH")

  ## Find number of people in each age/race/eth group
  pop_detail <- list()
  pop_detail$white_age_18_44 <- sum(dat$age_cat == "18-44" & dat$race == 1 & dat$eth == "NH")
  pop_detail$white_age_45_64 <- sum(dat$age_cat == "45-64" & dat$race == 1 & dat$eth == "NH")
  pop_detail$white_age_65p <- sum(dat$age_cat == "65p" & dat$race == 1 & dat$eth == "NH")
  pop_detail$black_age_18_44 <- sum(dat$age_cat == "18-44" & dat$race == 2 & dat$eth == "NH")
  pop_detail$black_age_45_64 <- sum(dat$age_cat == "45-64" & dat$race == 2 & dat$eth == "NH")
  pop_detail$black_age_65p <- sum(dat$age_cat == "65p" & dat$race == 2 & dat$eth == "NH")  
  pop_detail$hisp_age_18_44 <- sum(dat$age_cat == "18-44" & dat$eth == "H")
  pop_detail$hisp_age_45_64 <- sum(dat$age_cat == "45-64" & dat$eth == "H")
  pop_detail$hisp_age_65p <- sum(dat$age_cat == "65p" & dat$eth == "H")  
  pop_detail$rem_age_18_44 <- sum(dat$age_cat == "18-44" & dat$race %in% c(0, 3:9))
  pop_detail$rem_age_45_64 <- sum(dat$age_cat == "45-64" & dat$race %in% c(0, 3:9))
  pop_detail$rem_age_65p <- sum(dat$age_cat == "65p" & dat$race %in% c(0, 3:9))
  
  
  sum(unlist(pop_detail))  # doesn't include 0-17
  n_pop
  
  
  ###################################################################################################
  
  ### Initial Assignment for Hypertension
  hyp <- init_assign_morbidity(data = dat,
                               n_morb = n_hyper,
                               pop = pop,
                               pop_detail = pop_detail,
                               morb_18_44_pct = hyper_18_44_pct,
                               morb_45_64_pct = hyper_45_64_pct,
                               morb_65p_pct = hyper_65p_pct,
                               morb_white = hyper_white,
                               morb_black = hyper_black,
                               morb_hisp = hyper_hisp,
                               morb_ceiling = 0.8) 
  
  ### Overwrite data
  dat <- hyp$data
  dat <- dat |> rename(HYPER = MORB)
  
  ## Extract info
  age_exp_hyper_pct <- hyp$age_exp_morb_pct
  
  
  ## Check hyper count - the ones using pop_detail need to be fixed I think - but this is low priority
  print(paste0("Expected Hypertension Prevalence Count (Raw, Age): ", (hyp$raw_age_exp_morb$white + hyp$raw_age_exp_morb$black + hyp$raw_age_exp_morb$rem) |> round(0)))
  print(paste0("Estimated Hypertension Prevalence Count: ", sum(dat$HYPER)))
  print(paste0("Expected Hypertension Prevalence % (Raw, Age): ", (100 * (hyp$raw_age_exp_morb$white + hyp$raw_age_exp_morb$black + hyp$raw_age_exp_morb$rem) / n_pop) |> round(1), "%"))
  print(paste0("Estimated Hypertension Prevalence %: ", (100 * sum(dat$HYPER) / n_pop) |> round(1), "%"))
  print(paste0("Expected White Hypertension Prevalence % (Raw, Race): ", hyper_white, "%"))
  print(paste0("Expected White Hypertension Prevalence % (Age): ", ((100 * hyp$raw_age_exp_morb$white) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Estimated White Hypertension Prevalence %: ", (100 * sum(dat$HYPER[dat$race == 1]) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Expected Black Hypertension Prevalence % (Raw, Race): ", hyper_black, "%"))
  print(paste0("Expected Black Hypertension Prevalence % (Age): ", ((100 * hyp$raw_age_exp_morb$black) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Estimated Black Hypertension Prevalence %: ", (100 * sum(dat$HYPER[dat$race == 2]) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  
  ##################################################################################################
  
  
  #### Assign Obesity :: Hyper conditional ----
  obs <- sing_conditional_morbidity(data = dat,
                                    n_morb = n_obese,
                                    pop = pop,
                                    morb_18_44_pct = obese_18_44_pct,
                                    morb_45_64_pct = obese_45_64_pct,
                                    morb_65p_pct = obese_65p_pct,
                                    morb_cond_pct = obese_hyper_pct,
                                    cond_fname = "HYPER",
                                    morb_white = obese_white,
                                    morb_black = obese_black,
                                    morb_hisp = obese_hisp,
                                    morb_ceiling = 0.8)
  
  ### Overwrite data
  dat <- obs$data
  dat <- dat |> rename(OBESE = MORB)
  
  ## Extract info
  age_exp_obese_pct <- obs$age_exp_morb_pct
  
  ## Check estimates
  print(paste0("Expected Obesity Prevalence Count (Raw, Age): ", (obs$raw_age_exp_morb$white + obs$raw_age_exp_morb$black + obs$raw_age_exp_morb$rem) |> round(0)))
  print(paste0("Estimated Obesity Prevalence Count: ", sum(dat$OBESE)))
  print(paste0("Expected Obesity Prevalence % (Raw, Age): ", (100 * (obs$raw_age_exp_morb$white + obs$raw_age_exp_morb$black + obs$raw_age_exp_morb$rem) / n_pop) |> round(1), "%"))
  print(paste0("Estimated Obesity Prevalence %: ", (100 * sum(dat$OBESE) / n_pop) |> round(1), "%"))
  print(paste0("Expected White Obesity Prevalence % (Raw, Race): ", obese_white, "%"))
  print(paste0("Expected White Obesity Prevalence % (Age): ", ((100 * obs$raw_age_exp_morb$white) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Estimated White Obesity Prevalence %: ", (100 * sum(dat$OBESE[dat$race == 1]) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Expected Black Obesity Prevalence % (Raw, Race): ", obese_black, "%"))
  print(paste0("Expected Black Obesity Prevalence % (Age): ", ((100 * obs$raw_age_exp_morb$black) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Estimated Black Obesity Prevalence %: ", (100 * sum(dat$OBESE[dat$race == 2]) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Expected Count of Obese People with Hypertension: ", (n_obese * obese_hyper_pct / 100) |> round(0)))
  print(paste0("Estimated Count of Obese People with Hypertension: ", sum(dat$OBESE == 1 & dat$HYPER == 1)))  
  print(paste0("Expected % of Obese People with Hypertension: ", obese_hyper_pct, "%"))
  print(paste0("Estimated % of Obese People with Hypertension: ", round(100 * sum(dat$OBESE == 1 & dat$HYPER == 1) / sum(dat$OBESE == 1), 1), "%"))  
  
  
  #################################################################################################
  
  
  #### Assign Asthma :: Obesity conditional ----
  ast <- sing_conditional_morbidity(data = dat,
                                    n_morb = n_asthma,
                                    pop = pop,
                                    morb_18_44_pct = asthma_18_44_pct,
                                    morb_45_64_pct = asthma_45_64_pct,
                                    morb_65p_pct = asthma_65p_pct,
                                    morb_cond_pct = asthma_obese_pct,
                                    cond_fname = "OBESE",
                                    morb_white = asthma_white,
                                    morb_black = asthma_black,
                                    morb_hisp = asthma_hisp,
                                    morb_ceiling = 0.8)
  
  ### Overwrite data
  dat <- ast$data
  dat <- dat |> rename(ASTHMA = MORB)
  
  ## Extract info
  age_exp_asthma_pct <- ast$age_exp_morb_pct
  
  ## Check estimates
  print(paste0("Expected Asthma Prevalence Count (Raw, Age): ", (ast$raw_age_exp_morb$white + ast$raw_age_exp_morb$black + ast$raw_age_exp_morb$rem) |> round(0)))
  print(paste0("Estimated Asthma Prevalence Count: ", sum(dat$ASTHMA)))
  print(paste0("Expected Asthma Prevalence % (Raw, Age): ", (100 * (ast$raw_age_exp_morb$white + ast$raw_age_exp_morb$black + ast$raw_age_exp_morb$rem) / n_pop) |> round(1), "%"))
  print(paste0("Estimated Asthma Prevalence %: ", (100 * sum(dat$ASTHMA) / n_pop) |> round(1), "%"))
  print(paste0("Expected White Asthma Prevalence % (Raw, Race): ", asthma_white, "%"))
  print(paste0("Expected White Asthma Prevalence % (Age): ", ((100 * ast$raw_age_exp_morb$white) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Estimated White Asthma Prevalence %: ", (100 * sum(dat$ASTHMA[dat$race == 1]) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Expected Black Asthma Prevalence % (Raw, Race): ", asthma_black, "%"))
  print(paste0("Expected Black Asthma Prevalence % (Age): ", ((100 * ast$raw_age_exp_morb$black) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Estimated Black Asthma Prevalence %: ", (100 * sum(dat$ASTHMA[dat$race == 2]) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Expected Count of Asthmatic People with Obesity: ", (n_asthma * asthma_obese_pct / 100) |> round(0)))
  print(paste0("Estimated Count of Asthmatic People with Obesity: ", sum(dat$ASTHMA == 1 & dat$OBESE == 1)))  
  print(paste0("Expected % of Asthmatic People with Obesity: ", asthma_obese_pct, "%"))
  print(paste0("Estimated % of Asthmatic People with Obesity: ", round(100 * sum(dat$ASTHMA == 1 & dat$OBESE == 1) / sum(dat$ASTHMA == 1), 1), "%"))  
  
  
  
  
  #################################################################################################
  
  #### Assign COPD :: Asthma and Hyper conditional ----
  cpd <- double_conditional_morbidity(data = dat,
                                      n_morb = n_copd,
                                      pop = pop,
                                      morb_18_44_pct = copd_18_44_pct,
                                      morb_45_64_pct = copd_45_64_pct,
                                      morb_65p_pct = copd_65p_pct,
                                      morb_cond_pct = copd_asthma_pct,
                                      cond_fname = "ASTHMA",
                                      morb_cond2_pct = copd_hyper_pct,
                                      cond2_fname = "HYPER",
                                      morb_white = copd_white,
                                      morb_black = copd_black,
                                      morb_hisp = copd_hisp,
                                      morb_ceiling = 0.8,
                                      cond_precision = 0.02) 
  
  
  ### Overwrite data
  dat <- cpd$data
  dat <- dat |> rename(COPD = MORB)
  
  ## Extract info
  age_exp_copd_pct <- cpd$age_exp_morb_pct
  
  ## Check estimates
  print(paste0("Expected COPD Prevalence Count (Raw, Age): ", (cpd$raw_age_exp_morb$white + cpd$raw_age_exp_morb$black + cpd$raw_age_exp_morb$rem) |> round(0)))
  print(paste0("Estimated COPD Prevalence Count: ", sum(dat$COPD)))
  print(paste0("Expected COPD Prevalence % (Raw, Age): ", (100 * (cpd$raw_age_exp_morb$white + cpd$raw_age_exp_morb$black + cpd$raw_age_exp_morb$rem) / n_pop) |> round(1), "%"))
  print(paste0("Estimated COPD Prevalence %: ", (100 * sum(dat$COPD) / n_pop) |> round(1), "%"))
  print(paste0("Expected White COPD Prevalence % (Raw, Race): ", copd_white, "%"))
  print(paste0("Expected White COPD Prevalence % (Age): ", ((100 * cpd$raw_age_exp_morb$white) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Estimated White COPD Prevalence %: ", (100 * sum(dat$COPD[dat$race == 1]) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Expected Black COPD Prevalence % (Raw, Race): ", copd_black, "%"))
  print(paste0("Expected Black COPD Prevalence % (Age): ", ((100 * cpd$raw_age_exp_morb$black) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Estimated Black COPD Prevalence %: ", (100 * sum(dat$COPD[dat$race == 2]) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Expected Count of COPD People with Asthma: ", (n_copd * copd_asthma_pct / 100) |> round(0)))
  print(paste0("Estimated Count of COPD People with Asthma: ", sum(dat$COPD == 1 & dat$ASTHMA == 1)))  
  print(paste0("Expected % of COPD People with Asthma: ", copd_asthma_pct, "%"))
  print(paste0("Estimated % of COPD People with Asthma: ", round(100 * sum(dat$COPD == 1 & dat$ASTHMA == 1) / sum(dat$COPD == 1), 1), "%"))  
  print(paste0("Expected Count of COPD People with Hypertension: ", (n_copd * copd_hyper_pct / 100) |> round(0)))
  print(paste0("Estimated Count of COPD People with Hypertension: ", sum(dat$COPD == 1 & dat$HYPER == 1)))  
  print(paste0("Expected % of COPD People with Hypertension: ", copd_hyper_pct, "%"))
  print(paste0("Estimated % of COPD People with Hypertension: ", round(100 * sum(dat$COPD == 1 & dat$HYPER == 1) / sum(dat$COPD == 1), 1), "%"))  
  
  ##################################################################################################
  
  #### Assign Diabetes :: Obese, Hyper, COPD conditional ----
  dbd <- triple_conditional_morbidity(data = dat,
                                      n_morb = n_diab,
                                      pop = pop,
                                      morb_18_44_pct = diab_18_44_pct,
                                      morb_45_64_pct = diab_45_64_pct,
                                      morb_65p_pct = diab_65p_pct,
                                      morb_cond_pct = diab_obese_pct,
                                      cond_fname = "OBESE",
                                      morb_cond2_pct = diab_hyper_pct,
                                      cond2_fname = "HYPER",
                                      morb_cond3_pct = diab_copd_pct,
                                      cond3_fname = "COPD",
                                      cond_precision = 0.02,
                                      morb_white = diab_white,
                                      morb_black = diab_black,
                                      morb_hisp = diab_hisp,
                                      morb_ceiling = 0.8) 
  
  ## Extract info
  age_exp_diab_pct <- dbd$age_exp_morb_pct
  
  ### Overwrite data
  dat <- dbd$data
  dat <- dat |> rename(DIAB = MORB)
  
  ## Check estimates
  print(paste0("Expected Diabetes Prevalence Count (Raw, Age): ", (dbd$raw_age_exp_morb$white + dbd$raw_age_exp_morb$black + dbd$raw_age_exp_morb$rem) |> round(0)))
  print(paste0("Estimated Diabetes Prevalence Count: ", sum(dat$DIAB)))
  print(paste0("Expected Diabetes Prevalence % (Raw, Age): ", (100 * (dbd$raw_age_exp_morb$white + dbd$raw_age_exp_morb$black + dbd$raw_age_exp_morb$rem) / n_pop) |> round(1), "%"))
  print(paste0("Estimated Diabetes Prevalence %: ", (100 * sum(dat$DIAB) / n_pop) |> round(1), "%"))
  print(paste0("Expected White Diabetes Prevalence % (Raw, Race): ", diab_white, "%"))
  print(paste0("Expected White Diabetes Prevalence % (Age): ", ((100 * dbd$raw_age_exp_morb$white) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Estimated White Diabetes Prevalence %: ", (100 * sum(dat$DIAB[dat$race == 1]) / sum(unlist(pop_detail[1:3]))) |> round(1), "%"))
  print(paste0("Expected Black Diabetes Prevalence % (Raw, Race): ", diab_black, "%"))
  print(paste0("Expected Black Diabetes Prevalence % (Age): ", ((100 * dbd$raw_age_exp_morb$black) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Estimated Black Diabetes Prevalence %: ", (100 * sum(dat$DIAB[dat$race == 2]) / sum(unlist(pop_detail[4:6]))) |> round(1), "%"))
  print(paste0("Expected Count of Diabetes People with Obesity: ", (n_diab * diab_obese_pct / 100) |> round(0)))
  print(paste0("Estimated Count of Diabetes People with Obesity: ", sum(dat$DIAB == 1 & dat$OBESE == 1)))  
  print(paste0("Expected % of Diabetes People with Obesity: ", diab_obese_pct, "%"))
  print(paste0("Estimated % of Diabetes People with Obesity: ", round(100 * sum(dat$DIAB == 1 & dat$OBESE == 1) / sum(dat$DIAB == 1), 1), "%"))  
  print(paste0("Expected Count of Diabetes People with Hypertension: ", (n_diab * diab_hyper_pct / 100) |> round(0)))
  print(paste0("Estimated Count of Diabetes People with Hypertension: ", sum(dat$DIAB == 1 & dat$HYPER == 1)))  
  print(paste0("Expected % of Diabetes People with Hypertension: ", diab_hyper_pct, "%"))
  print(paste0("Estimated % of Diabetes People with Hypertension: ", round(100 * sum(dat$DIAB == 1 & dat$HYPER == 1) / sum(dat$DIAB == 1), 1), "%"))  
  print(paste0("Expected Count of Diabetes People with COPD: ", (n_diab * diab_copd_pct / 100) |> round(0)))
  print(paste0("Estimated Count of Diabetes People with COPD: ", sum(dat$DIAB == 1 & dat$COPD == 1)))  
  print(paste0("Expected % of Diabetes People with COPD: ", diab_copd_pct, "%"))
  print(paste0("Estimated % of Diabetes People with COPD: ", round(100 * sum(dat$DIAB == 1 & dat$COPD == 1) / sum(dat$DIAB == 1), 1), "%"))  
  
  #################################################################################################
  
  ## Write out summary counts
  count_sum$COUNTY[i] <- county
  count_sum$COUNTY_ALL[i] <- n_pop
  count_sum$COUNTY_GTE18[i] <- sum(dat$age_cat != "0-17")
  count_sum$AGE_LTE9[i] <- sum(dat$age < 10)
  count_sum$AGE_10_19[i] <- sum(dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29[i] <- sum(dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39[i] <- sum(dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49[i] <- sum(dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59[i] <- sum(dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69[i] <- sum(dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79[i] <- sum(dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89[i] <- sum(dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90[i] <- sum(dat$age > 89)
  count_sum$WHITE[i] <- sum(dat$race == 1)
  count_sum$WHITE_GTE18[i] <- sum(dat$race == 1 & dat$age_cat != "0-17")
  count_sum$BLACK[i] <- sum(dat$race == 2)
  count_sum$BLACK_GTE18[i] <- sum(dat$race == 2 & dat$age_cat != "0-17")
  count_sum$HISPANIC[i] <- sum(dat$eth == "H")
  count_sum$HISPANIC_GTE18[i] <- sum(dat$eth == "H" & dat$age_cat != "0-17")
  count_sum$NONHISPANIC[i] <- sum(dat$eth == "NH")
  count_sum$HYPER[i] <- sum(dat$HYPER == 1)
  count_sum$OBESE[i] <- sum(dat$OBESE == 1)
  count_sum$ASTHMA[i] <- sum(dat$ASTHMA == 1)
  count_sum$COPD[i] <- sum(dat$COPD == 1)
  count_sum$DIAB[i] <- sum(dat$DIAB == 1)
  count_sum$NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0)
  count_sum$AGE_LTE9_HYPER[i] <- sum(dat$HYPER == 1 & dat$age < 10)
  count_sum$AGE_10_19_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90_HYPER[i] <- sum(dat$HYPER == 1 & dat$age > 89)
  count_sum$AGE_LTE9_OBESE[i] <- sum(dat$OBESE == 1 & dat$age < 10)
  count_sum$AGE_10_19_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90_OBESE[i] <- sum(dat$OBESE == 1 & dat$age > 89)
  count_sum$AGE_LTE9_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age < 10)
  count_sum$AGE_10_19_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90_ASTHMA[i] <- sum(dat$ASTHMA == 1 & dat$age > 89)
  count_sum$AGE_LTE9_COPD[i] <- sum(dat$COPD == 1 & dat$age < 10)
  count_sum$AGE_10_19_COPD[i] <- sum(dat$COPD == 1 & dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29_COPD[i] <- sum(dat$COPD == 1 & dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39_COPD[i] <- sum(dat$COPD == 1 & dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49_COPD[i] <- sum(dat$COPD == 1 & dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59_COPD[i] <- sum(dat$COPD == 1 & dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69_COPD[i] <- sum(dat$COPD == 1 & dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79_COPD[i] <- sum(dat$COPD == 1 & dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89_COPD[i] <- sum(dat$COPD == 1 & dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90_COPD[i] <- sum(dat$COPD == 1 & dat$age > 89)
  count_sum$AGE_LTE9_DIAB[i] <- sum(dat$DIAB == 1 & dat$age < 10)
  count_sum$AGE_10_19_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90_DIAB[i] <- sum(dat$DIAB == 1 & dat$age > 89)
  count_sum$AGE_LTE9_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age < 10)
  count_sum$AGE_10_19_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 9 & dat$age < 20)
  count_sum$AGE_20_29_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 19 & dat$age < 30)
  count_sum$AGE_30_39_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 29 & dat$age < 40)
  count_sum$AGE_40_49_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 39 & dat$age < 50)
  count_sum$AGE_50_59_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 49 & dat$age < 60)
  count_sum$AGE_60_69_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 59 & dat$age < 70)
  count_sum$AGE_70_79_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 69 & dat$age < 80)
  count_sum$AGE_80_89_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 79 & dat$age < 90)
  count_sum$AGE_GTE90_NONE[i] <- sum(dat$HYPER == 0 & dat$OBESE == 0 & dat$ASTHMA == 0 & dat$COPD == 0 & dat$DIAB == 0 & dat$age > 89)
  
  ## Need to figure out why these arent working
  count_sum$AGE_18_44_WHITE[i] <- sum(dat$age_cat == "18-44" & dat$race == 1)
  count_sum$AGE_45_64_WHITE[i] <- sum(dat$age_cat == "45-64" & dat$race == 1)
  count_sum$AGE_65p_WHITE[i] <- sum(dat$age_cat == "65p" & dat$race == 1)
  count_sum$AGE_18_44_BLACK[i] <- sum(dat$age_cat == "18-44" & dat$race == 2)
  count_sum$AGE_45_64_BLACK[i] <- sum(dat$age_cat == "45-64" & dat$race == 2)
  count_sum$AGE_65p_BLACK[i] <- sum(dat$age_cat == "65p" & dat$race == 2)

  
  ## TABLE 1
  est_pct_hyper_18_44 <- est_pct_hyper_18_44 + sum(dat$HYPER == 1 & dat$age_cat == "18-44")
  est_pct_hyper_45_64 <- est_pct_hyper_45_64 + sum(dat$HYPER == 1 & dat$age_cat == "45-64")
  est_pct_hyper_65p <- est_pct_hyper_65p + sum(dat$HYPER == 1 & dat$age_cat == "65p")
  est_pct_obese_18_44 <- est_pct_obese_18_44 + sum(dat$OBESE == 1 & dat$age_cat == "18-44")
  est_pct_obese_45_64 <- est_pct_obese_45_64 + sum(dat$OBESE == 1 & dat$age_cat == "45-64")
  est_pct_obese_65p <- est_pct_obese_65p + sum(dat$OBESE == 1 & dat$age_cat == "65p")
  est_pct_asthma_18_44 <- est_pct_asthma_18_44 + sum(dat$ASTHMA == 1 & dat$age_cat == "18-44")
  est_pct_asthma_45_64 <- est_pct_asthma_45_64 + sum(dat$ASTHMA == 1 & dat$age_cat == "45-64")
  est_pct_asthma_65p <- est_pct_asthma_65p + sum(dat$ASTHMA == 1 & dat$age_cat == "65p")
  est_pct_copd_18_44 <- est_pct_copd_18_44 + sum(dat$COPD == 1 & dat$age_cat == "18-44")
  est_pct_copd_45_64 <- est_pct_copd_45_64 + sum(dat$COPD == 1 & dat$age_cat == "45-64")
  est_pct_copd_65p <- est_pct_copd_65p + sum(dat$COPD == 1 & dat$age_cat == "65p")
  est_pct_diab_18_44 <- est_pct_diab_18_44 + sum(dat$DIAB == 1 & dat$age_cat == "18-44")
  est_pct_diab_45_64 <- est_pct_diab_45_64 + sum(dat$DIAB == 1 & dat$age_cat == "45-64")
  est_pct_diab_65p <- est_pct_diab_65p + sum(dat$DIAB == 1 & dat$age_cat == "65p")
  
  ## TABLE 2
  est_pct_hyper_wh <- est_pct_hyper_wh + sum(dat$HYPER == 1 & dat$race == 1)
  est_pct_hyper_bl <- est_pct_hyper_bl + sum(dat$HYPER == 1 & dat$race == 2)
  est_pct_obese_wh <- est_pct_obese_wh + sum(dat$OBESE == 1 & dat$race == 1)
  est_pct_obese_bl <- est_pct_obese_bl + sum(dat$OBESE == 1 & dat$race == 2)
  est_pct_asthma_wh <- est_pct_asthma_wh + sum(dat$ASTHMA == 1 & dat$race == 1)
  est_pct_asthma_bl <- est_pct_asthma_bl + sum(dat$ASTHMA == 1 & dat$race == 2)
  est_pct_copd_wh <- est_pct_copd_wh + sum(dat$COPD == 1 & dat$race == 1)
  est_pct_copd_bl <- est_pct_copd_bl + sum(dat$COPD == 1 & dat$race == 2)
  est_pct_diab_wh <- est_pct_diab_wh + sum(dat$DIAB == 1 & dat$race == 1)
  est_pct_diab_bl <- est_pct_diab_bl + sum(dat$DIAB == 1 & dat$race == 2)
  
  ## TABLE 3
  est_pct_obese_hyper <- est_pct_obese_hyper + sum(dat$OBESE == 1 & dat$HYPER == 1)
  est_pct_asthma_obese <- est_pct_asthma_obese + sum(dat$ASTHMA == 1 & dat$OBESE == 1)
  est_pct_copd_asthma <- est_pct_copd_asthma + sum(dat$COPD == 1 & dat$ASTHMA == 1)
  est_pct_copd_hyper <- est_pct_copd_hyper + sum(dat$COPD == 1 & dat$HYPER == 1)
  est_pct_diab_obese <- est_pct_diab_obese + sum(dat$DIAB == 1 & dat$OBESE == 1)
  est_pct_diab_hyper <- est_pct_diab_hyper + sum(dat$DIAB == 1 & dat$HYPER == 1)
  est_pct_diab_copd <- est_pct_diab_copd + sum(dat$DIAB == 1 & dat$COPD == 1)
  
  
  ## TABLE 4 ###################
  est_pct_hyper_hisp <- est_pct_hyper_hisp + sum(dat$eth == "H" & dat$HYPER == 1)
  est_pct_hyper_nonhisp <- est_pct_hyper_nonhisp + sum(dat$eth == "NH" & dat$HYPER == 1)
  est_pct_obese_hisp <- est_pct_obese_hisp + sum(dat$eth == "H" & dat$OBESE == 1)
  est_pct_obese_nonhisp <- est_pct_obese_nonhisp + sum(dat$eth == "NH" & dat$OBESE == 1)
  est_pct_asthma_hisp <- est_pct_asthma_hisp + sum(dat$eth == "H" & dat$ASTHMA == 1)
  est_pct_asthma_nonhisp <- est_pct_asthma_nonhisp + sum(dat$eth == "NH" & dat$ASTHMA == 1)
  est_pct_copd_hisp <- est_pct_copd_hisp + sum(dat$eth == "H" & dat$COPD == 1)
  est_pct_copd_nonhisp <- est_pct_copd_nonhisp + sum(dat$eth == "NH" & dat$COPD == 1)
  est_pct_diab_hisp <- est_pct_diab_hisp + sum(dat$eth == "H" & dat$DIAB == 1)
  est_pct_diab_nonhisp <- est_pct_diab_nonhisp + sum(dat$eth == "NH" & dat$DIAB == 1)
  
  
  
  ### Another table - for one county example
  est_pct_hyper_18_44_white <- sum(dat$HYPER == 1 & dat$age_cat == "18-44" & dat$race == 1)
  est_pct_hyper_45_64_white <- sum(dat$HYPER == 1 & dat$age_cat == "45-64" & dat$race == 1)
  est_pct_hyper_65p_white <- sum(dat$HYPER == 1 & dat$age_cat == "65p" & dat$race == 1)
  est_pct_hyper_18_44_black <- sum(dat$HYPER == 1 & dat$age_cat == "18-44" & dat$race == 2)
  est_pct_hyper_45_64_black <-sum(dat$HYPER == 1 & dat$age_cat == "45-64" & dat$race == 2)
  est_pct_hyper_65p_black <- sum(dat$HYPER == 1 & dat$age_cat == "65p" & dat$race == 2)
  est_pct_obese_18_44_white <- sum(dat$OBESE == 1 & dat$age_cat == "18-44" & dat$race == 1)
  est_pct_obese_45_64_white <- sum(dat$OBESE == 1 & dat$age_cat == "45-64" & dat$race == 1)
  est_pct_obese_65p_white <- sum(dat$OBESE == 1 & dat$age_cat == "65p" & dat$race == 1)
  est_pct_obese_18_44_black <- sum(dat$OBESE == 1 & dat$age_cat == "18-44" & dat$race == 2)
  est_pct_obese_45_64_black <- sum(dat$OBESE == 1 & dat$age_cat == "45-64" & dat$race == 2)
  est_pct_obese_65p_black <- sum(dat$OBESE == 1 & dat$age_cat == "65p" & dat$race == 2)
  

  
  ## Storing county-level stats
  white_pcts$COUNTY[i] <- county
  white_pcts$age_18_44_HYPER_true[i] <- age_exp_hyper_pct$white_age_18_44 * 100
  white_pcts$age_18_44_HYPER_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "18-44" & dat$HYPER == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  white_pcts$age_18_44_HYPER_diff[i] <- white_pcts$age_18_44_HYPER_est[i] - white_pcts$age_18_44_HYPER_true[i]

  white_pcts$age_18_44_OBESE_true[i] <- age_exp_obese_pct$white_age_18_44 * 100
  white_pcts$age_18_44_OBESE_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "18-44" & dat$OBESE == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  white_pcts$age_18_44_OBESE_diff[i] <- white_pcts$age_18_44_OBESE_est[i] - white_pcts$age_18_44_OBESE_true[i]
  
  white_pcts$age_18_44_ASTHMA_true[i] <- age_exp_asthma_pct$white_age_18_44 * 100
  white_pcts$age_18_44_ASTHMA_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "18-44" & dat$ASTHMA == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  white_pcts$age_18_44_ASTHMA_diff[i] <- white_pcts$age_18_44_ASTHMA_est[i] - white_pcts$age_18_44_ASTHMA_true[i]
  
  white_pcts$age_18_44_COPD_true[i] <- age_exp_copd_pct$white_age_18_44 * 100
  white_pcts$age_18_44_COPD_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "18-44" & dat$COPD == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  white_pcts$age_18_44_COPD_diff[i] <- white_pcts$age_18_44_COPD_est[i] - white_pcts$age_18_44_COPD_true[i]
  
  white_pcts$age_18_44_DIAB_true[i] <- age_exp_diab_pct$white_age_18_44 * 100
  white_pcts$age_18_44_DIAB_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "18-44" & dat$DIAB == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  white_pcts$age_18_44_DIAB_diff[i] <- white_pcts$age_18_44_DIAB_est[i] - white_pcts$age_18_44_DIAB_true[i]
  
  white_pcts$age_45_64_HYPER_true[i] <- age_exp_hyper_pct$white_age_45_64 * 100
  white_pcts$age_45_64_HYPER_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "45-64" & dat$HYPER == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  white_pcts$age_45_64_HYPER_diff[i] <- white_pcts$age_45_64_HYPER_est[i] - white_pcts$age_45_64_HYPER_true[i]
  
  white_pcts$age_45_64_OBESE_true[i] <- age_exp_obese_pct$white_age_45_64 * 100
  white_pcts$age_45_64_OBESE_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "45-64" & dat$OBESE == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  white_pcts$age_45_64_OBESE_diff[i] <- white_pcts$age_45_64_OBESE_est[i] - white_pcts$age_45_64_OBESE_true[i]
  
  white_pcts$age_45_64_ASTHMA_true[i] <- age_exp_asthma_pct$white_age_45_64 * 100
  white_pcts$age_45_64_ASTHMA_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "45-64" & dat$ASTHMA == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  white_pcts$age_45_64_ASTHMA_diff[i] <- white_pcts$age_45_64_ASTHMA_est[i] - white_pcts$age_45_64_ASTHMA_true[i]
  
  white_pcts$age_45_64_COPD_true[i] <- age_exp_copd_pct$white_age_45_64 * 100
  white_pcts$age_45_64_COPD_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "45-64" & dat$COPD == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  white_pcts$age_45_64_COPD_diff[i] <- white_pcts$age_45_64_COPD_est[i] - white_pcts$age_45_64_COPD_true[i]
  
  white_pcts$age_45_64_DIAB_true[i] <- age_exp_diab_pct$white_age_45_64 * 100
  white_pcts$age_45_64_DIAB_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "45-64" & dat$DIAB == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  white_pcts$age_45_64_DIAB_diff[i] <- white_pcts$age_45_64_DIAB_est[i] - white_pcts$age_45_64_DIAB_true[i]
  
  white_pcts$age_65p_HYPER_true[i] <- age_exp_hyper_pct$white_age_65p * 100
  white_pcts$age_65p_HYPER_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "65p" & dat$HYPER == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  white_pcts$age_65p_HYPER_diff[i] <- white_pcts$age_65p_HYPER_est[i] - white_pcts$age_65p_HYPER_true[i]
  
  white_pcts$age_65p_OBESE_true[i] <- age_exp_obese_pct$white_age_65p * 100
  white_pcts$age_65p_OBESE_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "65p" & dat$OBESE == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  white_pcts$age_65p_OBESE_diff[i] <- white_pcts$age_65p_OBESE_est[i] - white_pcts$age_65p_OBESE_true[i]
  
  white_pcts$age_65p_ASTHMA_true[i] <- age_exp_asthma_pct$white_age_65p * 100
  white_pcts$age_65p_ASTHMA_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "65p" & dat$ASTHMA == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  white_pcts$age_65p_ASTHMA_diff[i] <- white_pcts$age_65p_ASTHMA_est[i] - white_pcts$age_65p_ASTHMA_true[i]
  
  white_pcts$age_65p_COPD_true[i] <- age_exp_copd_pct$white_age_65p * 100
  white_pcts$age_65p_COPD_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "65p" & dat$COPD == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  white_pcts$age_65p_COPD_diff[i] <- white_pcts$age_65p_COPD_est[i] - white_pcts$age_65p_COPD_true[i]
  
  white_pcts$age_65p_DIAB_true[i] <- age_exp_diab_pct$white_age_65p * 100
  white_pcts$age_65p_DIAB_est[i] <- ((sum(dat$race == 1 & dat$age_cat == "65p" & dat$DIAB == 1 & dat$eth == "NH")) / (sum(dat$race == 1 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  white_pcts$age_65p_DIAB_diff[i] <- white_pcts$age_65p_DIAB_est[i] - white_pcts$age_65p_DIAB_true[i]
  
  
  black_pcts$COUNTY[i] <- county
  black_pcts$age_18_44_HYPER_true[i] <- age_exp_hyper_pct$black_age_18_44 * 100
  black_pcts$age_18_44_HYPER_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "18-44" & dat$HYPER == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  black_pcts$age_18_44_HYPER_diff[i] <- black_pcts$age_18_44_HYPER_est[i] - black_pcts$age_18_44_HYPER_true[i]
  
  black_pcts$age_18_44_OBESE_true[i] <- age_exp_obese_pct$black_age_18_44 * 100
  black_pcts$age_18_44_OBESE_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "18-44" & dat$OBESE == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  black_pcts$age_18_44_OBESE_diff[i] <- black_pcts$age_18_44_OBESE_est[i] - black_pcts$age_18_44_OBESE_true[i]
  
  black_pcts$age_18_44_ASTHMA_true[i] <- age_exp_asthma_pct$black_age_18_44 * 100
  black_pcts$age_18_44_ASTHMA_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "18-44" & dat$ASTHMA == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  black_pcts$age_18_44_ASTHMA_diff[i] <- black_pcts$age_18_44_ASTHMA_est[i] - black_pcts$age_18_44_ASTHMA_true[i]
  
  black_pcts$age_18_44_COPD_true[i] <- age_exp_copd_pct$black_age_18_44 * 100
  black_pcts$age_18_44_COPD_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "18-44" & dat$COPD == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  black_pcts$age_18_44_COPD_diff[i] <- black_pcts$age_18_44_COPD_est[i] - black_pcts$age_18_44_COPD_true[i]
  
  black_pcts$age_18_44_DIAB_true[i] <- age_exp_diab_pct$black_age_18_44 * 100
  black_pcts$age_18_44_DIAB_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "18-44" & dat$DIAB == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "18-44" & dat$eth == "NH"))) * 100
  black_pcts$age_18_44_DIAB_diff[i] <- black_pcts$age_18_44_DIAB_est[i] - black_pcts$age_18_44_DIAB_true[i]
  
  black_pcts$age_45_64_HYPER_true[i] <- age_exp_hyper_pct$black_age_45_64 * 100
  black_pcts$age_45_64_HYPER_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "45-64" & dat$HYPER == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  black_pcts$age_45_64_HYPER_diff[i] <- black_pcts$age_45_64_HYPER_est[i] - black_pcts$age_45_64_HYPER_true[i]
  
  black_pcts$age_45_64_OBESE_true[i] <- age_exp_obese_pct$black_age_45_64 * 100
  black_pcts$age_45_64_OBESE_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "45-64" & dat$OBESE == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  black_pcts$age_45_64_OBESE_diff[i] <- black_pcts$age_45_64_OBESE_est[i] - black_pcts$age_45_64_OBESE_true[i]
  
  black_pcts$age_45_64_ASTHMA_true[i] <- age_exp_asthma_pct$black_age_45_64 * 100
  black_pcts$age_45_64_ASTHMA_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "45-64" & dat$ASTHMA == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  black_pcts$age_45_64_ASTHMA_diff[i] <- black_pcts$age_45_64_ASTHMA_est[i] - black_pcts$age_45_64_ASTHMA_true[i]
  
  black_pcts$age_45_64_COPD_true[i] <- age_exp_copd_pct$black_age_45_64 * 100
  black_pcts$age_45_64_COPD_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "45-64" & dat$COPD == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  black_pcts$age_45_64_COPD_diff[i] <- black_pcts$age_45_64_COPD_est[i] - black_pcts$age_45_64_COPD_true[i]
  
  black_pcts$age_45_64_DIAB_true[i] <- age_exp_diab_pct$black_age_45_64 * 100
  black_pcts$age_45_64_DIAB_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "45-64" & dat$DIAB == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "45-64" & dat$eth == "NH"))) * 100
  black_pcts$age_45_64_DIAB_diff[i] <- black_pcts$age_45_64_DIAB_est[i] - black_pcts$age_45_64_DIAB_true[i]
  
  black_pcts$age_65p_HYPER_true[i] <- age_exp_hyper_pct$black_age_65p * 100
  black_pcts$age_65p_HYPER_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "65p" & dat$HYPER == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  black_pcts$age_65p_HYPER_diff[i] <- black_pcts$age_65p_HYPER_est[i] - black_pcts$age_65p_HYPER_true[i]
  
  black_pcts$age_65p_OBESE_true[i] <- age_exp_obese_pct$black_age_65p * 100
  black_pcts$age_65p_OBESE_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "65p" & dat$OBESE == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  black_pcts$age_65p_OBESE_diff[i] <- black_pcts$age_65p_OBESE_est[i] - black_pcts$age_65p_OBESE_true[i]
  
  black_pcts$age_65p_ASTHMA_true[i] <- age_exp_asthma_pct$black_age_65p * 100
  black_pcts$age_65p_ASTHMA_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "65p" & dat$ASTHMA == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  black_pcts$age_65p_ASTHMA_diff[i] <- black_pcts$age_65p_ASTHMA_est[i] - black_pcts$age_65p_ASTHMA_true[i]
  
  black_pcts$age_65p_COPD_true[i] <- age_exp_copd_pct$black_age_65p * 100
  black_pcts$age_65p_COPD_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "65p" & dat$COPD == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  black_pcts$age_65p_COPD_diff[i] <- black_pcts$age_65p_COPD_est[i] - black_pcts$age_65p_COPD_true[i]
  
  black_pcts$age_65p_DIAB_true[i] <- age_exp_diab_pct$black_age_65p * 100
  black_pcts$age_65p_DIAB_est[i] <- ((sum(dat$race == 2 & dat$age_cat == "65p" & dat$DIAB == 1 & dat$eth == "NH")) / (sum(dat$race == 2 & dat$age_cat == "65p" & dat$eth == "NH"))) * 100
  black_pcts$age_65p_DIAB_diff[i] <- black_pcts$age_65p_DIAB_est[i] - black_pcts$age_65p_DIAB_true[i]
  
  
  rem_pcts$COUNTY[i] <- county
  rem_pcts$age_18_44_HYPER_true[i] <- age_exp_hyper_pct$rem_age_18_44 * 100
  rem_pcts$age_18_44_HYPER_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44" & dat$HYPER == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44"))) * 100
  rem_pcts$age_18_44_HYPER_diff[i] <- rem_pcts$age_18_44_HYPER_est[i] - rem_pcts$age_18_44_HYPER_true[i]
  
  rem_pcts$age_18_44_OBESE_true[i] <- age_exp_obese_pct$rem_age_18_44 * 100
  rem_pcts$age_18_44_OBESE_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44" & dat$OBESE == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44"))) * 100
  rem_pcts$age_18_44_OBESE_diff[i] <- rem_pcts$age_18_44_OBESE_est[i] - rem_pcts$age_18_44_OBESE_true[i]
  
  rem_pcts$age_18_44_ASTHMA_true[i] <- age_exp_asthma_pct$rem_age_18_44 * 100
  rem_pcts$age_18_44_ASTHMA_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44" & dat$ASTHMA == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44"))) * 100
  rem_pcts$age_18_44_ASTHMA_diff[i] <- rem_pcts$age_18_44_ASTHMA_est[i] - rem_pcts$age_18_44_ASTHMA_true[i]
  
  rem_pcts$age_18_44_COPD_true[i] <- age_exp_copd_pct$rem_age_18_44 * 100
  rem_pcts$age_18_44_COPD_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44" & dat$COPD == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44"))) * 100
  rem_pcts$age_18_44_COPD_diff[i] <- rem_pcts$age_18_44_COPD_est[i] - rem_pcts$age_18_44_COPD_true[i]
  
  rem_pcts$age_18_44_DIAB_true[i] <- age_exp_diab_pct$rem_age_18_44 * 100
  rem_pcts$age_18_44_DIAB_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44" & dat$DIAB == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "18-44"))) * 100
  rem_pcts$age_18_44_DIAB_diff[i] <- rem_pcts$age_18_44_DIAB_est[i] - rem_pcts$age_18_44_DIAB_true[i]
  
  rem_pcts$age_45_64_HYPER_true[i] <- age_exp_hyper_pct$rem_age_45_64 * 100
  rem_pcts$age_45_64_HYPER_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64" & dat$HYPER == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64"))) * 100
  rem_pcts$age_45_64_HYPER_diff[i] <- rem_pcts$age_45_64_HYPER_est[i] - rem_pcts$age_45_64_HYPER_true[i]
  
  rem_pcts$age_45_64_OBESE_true[i] <- age_exp_obese_pct$rem_age_45_64 * 100
  rem_pcts$age_45_64_OBESE_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64" & dat$OBESE == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64"))) * 100
  rem_pcts$age_45_64_OBESE_diff[i] <- rem_pcts$age_45_64_OBESE_est[i] - rem_pcts$age_45_64_OBESE_true[i]
  
  rem_pcts$age_45_64_ASTHMA_true[i] <- age_exp_asthma_pct$rem_age_45_64 * 100
  rem_pcts$age_45_64_ASTHMA_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64" & dat$ASTHMA == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64"))) * 100
  rem_pcts$age_45_64_ASTHMA_diff[i] <- rem_pcts$age_45_64_ASTHMA_est[i] - rem_pcts$age_45_64_ASTHMA_true[i]
  
  rem_pcts$age_45_64_COPD_true[i] <- age_exp_copd_pct$rem_age_45_64 * 100
  rem_pcts$age_45_64_COPD_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64" & dat$COPD == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64"))) * 100
  rem_pcts$age_45_64_COPD_diff[i] <- rem_pcts$age_45_64_COPD_est[i] - rem_pcts$age_45_64_COPD_true[i]
  
  rem_pcts$age_45_64_DIAB_true[i] <- age_exp_diab_pct$rem_age_45_64 * 100
  rem_pcts$age_45_64_DIAB_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64" & dat$DIAB == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "45-64"))) * 100
  rem_pcts$age_45_64_DIAB_diff[i] <- rem_pcts$age_45_64_DIAB_est[i] - rem_pcts$age_45_64_DIAB_true[i]
  
  rem_pcts$age_65p_HYPER_true[i] <- age_exp_hyper_pct$rem_age_65p * 100
  rem_pcts$age_65p_HYPER_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p" & dat$HYPER == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p"))) * 100
  rem_pcts$age_65p_HYPER_diff[i] <- rem_pcts$age_65p_HYPER_est[i] - rem_pcts$age_65p_HYPER_true[i]
  
  rem_pcts$age_65p_OBESE_true[i] <- age_exp_obese_pct$rem_age_65p * 100
  rem_pcts$age_65p_OBESE_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p" & dat$OBESE == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p"))) * 100
  rem_pcts$age_65p_OBESE_diff[i] <- rem_pcts$age_65p_OBESE_est[i] - rem_pcts$age_65p_OBESE_true[i]
  
  rem_pcts$age_65p_ASTHMA_true[i] <- age_exp_asthma_pct$rem_age_65p * 100
  rem_pcts$age_65p_ASTHMA_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p" & dat$ASTHMA == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p"))) * 100
  rem_pcts$age_65p_ASTHMA_diff[i] <- rem_pcts$age_65p_ASTHMA_est[i] - rem_pcts$age_65p_ASTHMA_true[i]
  
  rem_pcts$age_65p_COPD_true[i] <- age_exp_copd_pct$rem_age_65p * 100
  rem_pcts$age_65p_COPD_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p" & dat$COPD == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p"))) * 100
  rem_pcts$age_65p_COPD_diff[i] <- rem_pcts$age_65p_COPD_est[i] - rem_pcts$age_65p_COPD_true[i]
  
  rem_pcts$age_65p_DIAB_true[i] <- age_exp_diab_pct$rem_age_65p * 100
  rem_pcts$age_65p_DIAB_est[i] <- ((sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p" & dat$DIAB == 1)) / (sum(dat$race != 1 & dat$race != 2 & dat$age_cat == "65p"))) * 100
  rem_pcts$age_65p_DIAB_diff[i] <- rem_pcts$age_65p_DIAB_est[i] - rem_pcts$age_65p_DIAB_true[i]
  
  
  hisp_pcts$COUNTY[i] <- county
  hisp_pcts$age_18_44_HYPER_true[i] <- age_exp_hyper_pct$hisp_age_18_44 * 100
  hisp_pcts$age_18_44_HYPER_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "18-44" & dat$HYPER == 1)) / (sum(dat$eth == "H" & dat$age_cat == "18-44"))) * 100
  hisp_pcts$age_18_44_HYPER_diff[i] <- hisp_pcts$age_18_44_HYPER_est[i] - hisp_pcts$age_18_44_HYPER_true[i]
  
  hisp_pcts$age_18_44_OBESE_true[i] <- age_exp_obese_pct$hisp_age_18_44 * 100
  hisp_pcts$age_18_44_OBESE_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "18-44" & dat$OBESE == 1)) / (sum(dat$eth == "H" & dat$age_cat == "18-44"))) * 100
  hisp_pcts$age_18_44_OBESE_diff[i] <- hisp_pcts$age_18_44_OBESE_est[i] - hisp_pcts$age_18_44_OBESE_true[i]
  
  hisp_pcts$age_18_44_ASTHMA_true[i] <- age_exp_asthma_pct$hisp_age_18_44 * 100
  hisp_pcts$age_18_44_ASTHMA_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "18-44" & dat$ASTHMA == 1)) / (sum(dat$eth == "H" & dat$age_cat == "18-44"))) * 100
  hisp_pcts$age_18_44_ASTHMA_diff[i] <- hisp_pcts$age_18_44_ASTHMA_est[i] - hisp_pcts$age_18_44_ASTHMA_true[i]
  
  hisp_pcts$age_18_44_COPD_true[i] <- age_exp_copd_pct$hisp_age_18_44 * 100
  hisp_pcts$age_18_44_COPD_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "18-44" & dat$COPD == 1)) / (sum(dat$eth == "H" & dat$age_cat == "18-44"))) * 100
  hisp_pcts$age_18_44_COPD_diff[i] <- hisp_pcts$age_18_44_COPD_est[i] - hisp_pcts$age_18_44_COPD_true[i]
  
  hisp_pcts$age_18_44_DIAB_true[i] <- age_exp_diab_pct$hisp_age_18_44 * 100
  hisp_pcts$age_18_44_DIAB_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "18-44" & dat$DIAB == 1)) / (sum(dat$eth == "H" & dat$age_cat == "18-44"))) * 100
  hisp_pcts$age_18_44_DIAB_diff[i] <- hisp_pcts$age_18_44_DIAB_est[i] - hisp_pcts$age_18_44_DIAB_true[i]
  
  hisp_pcts$age_45_64_HYPER_true[i] <- age_exp_hyper_pct$hisp_age_45_64 * 100
  hisp_pcts$age_45_64_HYPER_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "45-64" & dat$HYPER == 1)) / (sum(dat$eth == "H" & dat$age_cat == "45-64"))) * 100
  hisp_pcts$age_45_64_HYPER_diff[i] <- hisp_pcts$age_45_64_HYPER_est[i] - hisp_pcts$age_45_64_HYPER_true[i]
  
  hisp_pcts$age_45_64_OBESE_true[i] <- age_exp_obese_pct$hisp_age_45_64 * 100
  hisp_pcts$age_45_64_OBESE_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "45-64" & dat$OBESE == 1)) / (sum(dat$eth == "H" & dat$age_cat == "45-64"))) * 100
  hisp_pcts$age_45_64_OBESE_diff[i] <- hisp_pcts$age_45_64_OBESE_est[i] - hisp_pcts$age_45_64_OBESE_true[i]
  
  hisp_pcts$age_45_64_ASTHMA_true[i] <- age_exp_asthma_pct$hisp_age_45_64 * 100
  hisp_pcts$age_45_64_ASTHMA_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "45-64" & dat$ASTHMA == 1)) / (sum(dat$eth == "H" & dat$age_cat == "45-64"))) * 100
  hisp_pcts$age_45_64_ASTHMA_diff[i] <- hisp_pcts$age_45_64_ASTHMA_est[i] - hisp_pcts$age_45_64_ASTHMA_true[i]
  
  hisp_pcts$age_45_64_COPD_true[i] <- age_exp_copd_pct$hisp_age_45_64 * 100
  hisp_pcts$age_45_64_COPD_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "45-64" & dat$COPD == 1)) / (sum(dat$eth == "H" & dat$age_cat == "45-64"))) * 100
  hisp_pcts$age_45_64_COPD_diff[i] <- hisp_pcts$age_45_64_COPD_est[i] - hisp_pcts$age_45_64_COPD_true[i]
  
  hisp_pcts$age_45_64_DIAB_true[i] <- age_exp_diab_pct$hisp_age_45_64 * 100
  hisp_pcts$age_45_64_DIAB_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "45-64" & dat$DIAB == 1)) / (sum(dat$eth == "H" & dat$age_cat == "45-64"))) * 100
  hisp_pcts$age_45_64_DIAB_diff[i] <- hisp_pcts$age_45_64_DIAB_est[i] - hisp_pcts$age_45_64_DIAB_true[i]
  
  hisp_pcts$age_65p_HYPER_true[i] <- age_exp_hyper_pct$hisp_age_65p * 100
  hisp_pcts$age_65p_HYPER_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "65p" & dat$HYPER == 1)) / (sum(dat$eth == "H" & dat$age_cat == "65p"))) * 100
  hisp_pcts$age_65p_HYPER_diff[i] <- hisp_pcts$age_65p_HYPER_est[i] - hisp_pcts$age_65p_HYPER_true[i]
  
  hisp_pcts$age_65p_OBESE_true[i] <- age_exp_obese_pct$hisp_age_65p * 100
  hisp_pcts$age_65p_OBESE_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "65p" & dat$OBESE == 1)) / (sum(dat$eth == "H" & dat$age_cat == "65p"))) * 100
  hisp_pcts$age_65p_OBESE_diff[i] <- hisp_pcts$age_65p_OBESE_est[i] - hisp_pcts$age_65p_OBESE_true[i]
  
  hisp_pcts$age_65p_ASTHMA_true[i] <- age_exp_asthma_pct$hisp_age_65p * 100
  hisp_pcts$age_65p_ASTHMA_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "65p" & dat$ASTHMA == 1)) / (sum(dat$eth == "H" & dat$age_cat == "65p"))) * 100
  hisp_pcts$age_65p_ASTHMA_diff[i] <- hisp_pcts$age_65p_ASTHMA_est[i] - hisp_pcts$age_65p_ASTHMA_true[i]
  
  hisp_pcts$age_65p_COPD_true[i] <- age_exp_copd_pct$hisp_age_65p * 100
  hisp_pcts$age_65p_COPD_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "65p" & dat$COPD == 1)) / (sum(dat$eth == "H" & dat$age_cat == "65p"))) * 100
  hisp_pcts$age_65p_COPD_diff[i] <- hisp_pcts$age_65p_COPD_est[i] - hisp_pcts$age_65p_COPD_true[i]
  
  hisp_pcts$age_65p_DIAB_true[i] <- age_exp_diab_pct$hisp_age_65p * 100
  hisp_pcts$age_65p_DIAB_est[i] <- ((sum(dat$eth == "H" & dat$age_cat == "65p" & dat$DIAB == 1)) / (sum(dat$eth == "H" & dat$age_cat == "65p"))) * 100
  hisp_pcts$age_65p_DIAB_diff[i] <- hisp_pcts$age_65p_DIAB_est[i] - hisp_pcts$age_65p_DIAB_true[i]
  
  
  
  ### Separate group quarters and regular files
  dat_gq <- dat[dat$sp_id %in% dat_gq$sp_id, ]
  dat <- dat[!dat$sp_id %in% dat_gq$sp_id, ]
  
  # Write out full data files here
  write_csv(dat, file = paste0(file.path(output_path), county, "_people.csv"))
  write_csv(dat_gq, file = paste0(file.path(output_path), county, "_gq.csv"))
  
}
  
  

## Write out county-level summaries
write_csv(white_pcts, file = paste0(file.path(output_path), "cond_white.csv"))
write_csv(black_pcts, file = paste0(file.path(output_path), "cond_black.csv"))
write_csv(rem_pcts, file = paste0(file.path(output_path), "cond_rem.csv"))
write_csv(hisp_pcts, file = paste0(file.path(output_path), "cond_hisp.csv"))


## Write out state-level county
write_csv(count_sum, file = paste0(file.path(output_path), "count_sum.csv"))



## STATE SUMMARY
{
sum(count_sum$COUNTY_ALL)
sum(count_sum$COUNTY_GTE18)

sum(count_sum$AGE_LTE9)
sum(count_sum$AGE_10_19)
sum(count_sum$AGE_20_29)
sum(count_sum$AGE_30_39)
sum(count_sum$AGE_40_49)
sum(count_sum$AGE_50_59)
sum(count_sum$AGE_60_69)
sum(count_sum$AGE_70_79)
sum(count_sum$AGE_80_89)
sum(count_sum$AGE_GTE90)

sum(count_sum$WHITE)
sum(count_sum$WHITE_GTE18)

sum(count_sum$BLACK)
sum(count_sum$BLACK_GTE18)


sum(count_sum$HISPANIC)
sum(count_sum$HISPANIC_GTE18)

sum(count_sum$NONHISPANIC)

sum(count_sum$HYPER)
sum(count_sum$OBESE)
sum(count_sum$ASTHMA)
sum(count_sum$COPD)
sum(count_sum$DIAB)
sum(count_sum$NONE)

## Overall prevalence 
sum(count_sum$HYPER)/sum(count_sum$COUNTY_ALL)
sum(count_sum$OBESE)/sum(count_sum$COUNTY_ALL)
sum(count_sum$ASTHMA)/sum(count_sum$COUNTY_ALL)
sum(count_sum$COPD)/sum(count_sum$COUNTY_ALL)
sum(count_sum$DIAB)/sum(count_sum$COUNTY_ALL)
sum(count_sum$NONE)/sum(count_sum$COUNTY_ALL)

sum(count_sum$AGE_LTE9_HYPER)
sum(count_sum$AGE_10_19_HYPER)
sum(count_sum$AGE_20_29_HYPER)
sum(count_sum$AGE_30_39_HYPER)
sum(count_sum$AGE_40_49_HYPER)
sum(count_sum$AGE_50_59_HYPER)
sum(count_sum$AGE_60_69_HYPER)
sum(count_sum$AGE_70_79_HYPER)
sum(count_sum$AGE_80_89_HYPER)
sum(count_sum$AGE_GTE90_HYPER)

sum(count_sum$AGE_LTE9_OBESE)
sum(count_sum$AGE_10_19_OBESE)
sum(count_sum$AGE_20_29_OBESE)
sum(count_sum$AGE_30_39_OBESE)
sum(count_sum$AGE_40_49_OBESE)
sum(count_sum$AGE_50_59_OBESE)
sum(count_sum$AGE_60_69_OBESE)
sum(count_sum$AGE_70_79_OBESE)
sum(count_sum$AGE_80_89_OBESE)
sum(count_sum$AGE_GTE90_OBESE)

sum(count_sum$AGE_LTE9_ASTHMA)
sum(count_sum$AGE_10_19_ASTHMA)
sum(count_sum$AGE_20_29_ASTHMA)
sum(count_sum$AGE_30_39_ASTHMA)
sum(count_sum$AGE_40_49_ASTHMA)
sum(count_sum$AGE_50_59_ASTHMA)
sum(count_sum$AGE_60_69_ASTHMA)
sum(count_sum$AGE_70_79_ASTHMA)
sum(count_sum$AGE_80_89_ASTHMA)
sum(count_sum$AGE_GTE90_ASTHMA)

sum(count_sum$AGE_LTE9_COPD)
sum(count_sum$AGE_10_19_COPD)
sum(count_sum$AGE_20_29_COPD)
sum(count_sum$AGE_30_39_COPD)
sum(count_sum$AGE_40_49_COPD)
sum(count_sum$AGE_50_59_COPD)
sum(count_sum$AGE_60_69_COPD)
sum(count_sum$AGE_70_79_COPD)
sum(count_sum$AGE_80_89_COPD)
sum(count_sum$AGE_GTE90_COPD)

sum(count_sum$AGE_LTE9_DIAB)
sum(count_sum$AGE_10_19_DIAB)
sum(count_sum$AGE_20_29_DIAB)
sum(count_sum$AGE_30_39_DIAB)
sum(count_sum$AGE_40_49_DIAB)
sum(count_sum$AGE_50_59_DIAB)
sum(count_sum$AGE_60_69_DIAB)
sum(count_sum$AGE_70_79_DIAB)
sum(count_sum$AGE_80_89_DIAB)
sum(count_sum$AGE_GTE90_DIAB)

sum(count_sum$AGE_LTE9_NONE)
sum(count_sum$AGE_10_19_NONE)
sum(count_sum$AGE_20_29_NONE)
sum(count_sum$AGE_30_39_NONE)
sum(count_sum$AGE_40_49_NONE)
sum(count_sum$AGE_50_59_NONE)
sum(count_sum$AGE_60_69_NONE)
sum(count_sum$AGE_70_79_NONE)
sum(count_sum$AGE_80_89_NONE)
sum(count_sum$AGE_GTE90_NONE)



## TABLE 1
est_pct_hyper_18_44 <- (est_pct_hyper_18_44 / pop_18_44) * 100
est_pct_hyper_45_64 <- (est_pct_hyper_45_64 / pop_45_64) * 100
est_pct_hyper_65p <- (est_pct_hyper_65p / pop_65p) * 100
est_pct_obese_18_44 <- (est_pct_obese_18_44 / pop_18_44) * 100
est_pct_obese_45_64 <- (est_pct_obese_45_64 / pop_45_64) * 100
est_pct_obese_65p <- (est_pct_obese_65p / pop_65p) * 100
est_pct_asthma_18_44 <- (est_pct_asthma_18_44 / pop_18_44) * 100
est_pct_asthma_45_64 <- (est_pct_asthma_45_64 / pop_45_64) * 100
est_pct_asthma_65p <- (est_pct_asthma_65p / pop_65p) * 100
est_pct_copd_18_44 <- (est_pct_copd_18_44 / pop_18_44) * 100
est_pct_copd_45_64 <- (est_pct_copd_45_64 / pop_45_64) * 100
est_pct_copd_65p <- (est_pct_copd_65p / pop_65p) * 100
est_pct_diab_18_44 <- (est_pct_diab_18_44 / pop_18_44) * 100
est_pct_diab_45_64 <- (est_pct_diab_45_64 / pop_45_64) * 100
est_pct_diab_65p <- (est_pct_diab_65p / pop_65p) * 100


## TABLE 2
est_pct_hyper_wh <- (est_pct_hyper_wh / pop_white) * 100
est_pct_hyper_bl <- (est_pct_hyper_bl / pop_black) * 100
est_pct_obese_wh <- (est_pct_obese_wh / pop_white) * 100
est_pct_obese_bl <- (est_pct_obese_bl / pop_black) * 100
est_pct_asthma_wh <- (est_pct_asthma_wh / pop_white) * 100
est_pct_asthma_bl <- (est_pct_asthma_bl / pop_black) * 100
est_pct_copd_wh <- (est_pct_copd_wh / pop_white) * 100
est_pct_copd_bl <- (est_pct_copd_bl / pop_black) * 100
est_pct_diab_wh <- (est_pct_diab_wh / pop_white) * 100
est_pct_diab_bl <- (est_pct_diab_bl / pop_black) * 100

## TABLE 3
est_pct_obese_hyper <- (est_pct_obese_hyper / sum(count_sum$OBESE)) * 100
est_pct_asthma_obese <- (est_pct_asthma_obese / sum(count_sum$ASTHMA)) * 100
est_pct_copd_asthma <- (est_pct_copd_asthma / sum(count_sum$COPD)) * 100
est_pct_copd_hyper <- (est_pct_copd_hyper / sum(count_sum$COPD)) * 100
est_pct_diab_obese <- (est_pct_diab_obese / sum(count_sum$DIAB)) * 100
est_pct_diab_hyper <- (est_pct_diab_hyper / sum(count_sum$DIAB)) * 100
est_pct_diab_copd <- (est_pct_diab_copd / sum(count_sum$DIAB)) * 100


## TABLE 4 ###################
est_pct_hyper_hisp <- (est_pct_hyper_hisp / pop_hispanic) * 100
est_pct_hyper_nonhisp <- (est_pct_hyper_nonhisp / pop_non_hispanic) * 100
est_pct_obese_hisp <- (est_pct_obese_hisp / pop_hispanic) * 100
est_pct_obese_nonhisp <- (est_pct_obese_nonhisp / pop_non_hispanic) * 100
est_pct_asthma_hisp <- (est_pct_asthma_hisp / pop_hispanic) * 100
est_pct_asthma_nonhisp <- (est_pct_asthma_nonhisp / pop_non_hispanic) * 100
est_pct_copd_hisp <- (est_pct_copd_hisp / pop_hispanic) * 100
est_pct_copd_nonhisp <- (est_pct_copd_nonhisp / pop_non_hispanic) * 100
est_pct_diab_hisp <- (est_pct_diab_hisp / pop_hispanic) * 100
est_pct_diab_nonhisp <- (est_pct_diab_nonhisp / pop_non_hispanic) * 100



## TABLE with both age/race
est_pct_hyper_18_44_white <- (est_pct_hyper_18_44_white / count_sum$AGE_18_44_WHITE) * 100
est_pct_hyper_45_64_white <- (est_pct_hyper_45_64_white / count_sum$AGE_45_64_WHITE) * 100
est_pct_hyper_65p_white <- (est_pct_hyper_65p_white / count_sum$AGE_65p_WHITE) * 100
est_pct_hyper_18_44_black <- (est_pct_hyper_18_44_black / count_sum$AGE_18_44_BLACK) * 100
est_pct_hyper_45_64_black <- (est_pct_hyper_45_64_black / count_sum$AGE_45_64_BLACK) * 100
est_pct_hyper_65p_black <- (est_pct_hyper_65p_black / count_sum$AGE_65p_BLACK) * 100
est_pct_obese_18_44_white <- (est_pct_obese_18_44_white / count_sum$AGE_18_44_WHITE) * 100
est_pct_obese_45_64_white <- (est_pct_obese_45_64_white / count_sum$AGE_45_64_WHITE) * 100
est_pct_obese_65p_white <- (est_pct_obese_65p_white / count_sum$AGE_65p_WHITE) * 100
est_pct_obese_18_44_black <- (est_pct_obese_18_44_black / count_sum$AGE_18_44_BLACK) * 100
est_pct_obese_45_64_black <- (est_pct_obese_45_64_black / count_sum$AGE_45_64_BLACK) * 100
est_pct_obese_65p_black <- (est_pct_obese_65p_black / count_sum$AGE_65p_BLACK) * 100
}


