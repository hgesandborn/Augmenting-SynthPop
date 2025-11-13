# =========================================================
# Title: Chronic condition assignment functions
# Author: Hilary Sandborn & Paul Delamater
# Date: 2025-10-31
# Description: Define functions for assigning conditions - don't actually need to run
# =========================================================


## Load necessary packages -----
library(tidyverse)
library(magrittr)


### Function to assign people with morbidity with no
### comorbidities
init_assign_morbidity <- function(data,
                                  n_morb,
                                  pop,
                                  pop_race,
                                  pop_detail,
                                  morb_18_44_pct,
                                  morb_45_64_pct,
                                  morb_65p_pct,
                                  morb_white,
                                  morb_black,
                                  morb_hisp,
                                  morb_ceiling = 0.8) {
  
  
  ### Calculate the BLACK scalar Non-Hispanic
  black_sc <- (morb_black / 100) * (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) /
              ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
               (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) +
               (morb_65p_pct / 100) * (pop_detail$black_age_65p))

  ### Calculate the WHITE scalar Non-Hispanic
  white_sc <- (morb_white / 100) * (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p) /
              ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
               (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) +
               (morb_65p_pct / 100) * (pop_detail$white_age_65p))

  ### Calculate the HISPANIC scalar - race doesn't matter
  hisp_sc <- (morb_hisp / 100) * (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  
  
  ### Calculate Age Group morbidity rate estimates for each race/eth
  age_exp_morb_pct <- list()
  age_exp_morb_pct$white_age_18_44 <- (morb_18_44_pct / 100) * white_sc
  age_exp_morb_pct$white_age_45_64 <- (morb_45_64_pct / 100) * white_sc
  age_exp_morb_pct$white_age_65p <- (morb_65p_pct / 100) * white_sc
  age_exp_morb_pct$black_age_18_44 <- (morb_18_44_pct / 100) * black_sc
  age_exp_morb_pct$black_age_45_64 <- (morb_45_64_pct / 100) * black_sc
  age_exp_morb_pct$black_age_65p <- (morb_65p_pct / 100) * black_sc
  age_exp_morb_pct$hisp_age_18_44 <- (morb_18_44_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_45_64 <- (morb_45_64_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_65p <- (morb_65p_pct / 100) * hisp_sc
  age_exp_morb_pct$rem_age_18_44 <- (morb_18_44_pct / 100)
  age_exp_morb_pct$rem_age_45_64 <- (morb_45_64_pct / 100)
  age_exp_morb_pct$rem_age_65p <- (morb_65p_pct / 100)
  
  ### Move any to ceiling value
  age_exp_morb_pct[which(age_exp_morb_pct > morb_ceiling)] <- morb_ceiling
  
  ### Calculate Age Group morbidity estimates for each age / race
  age_exp_morb <- list()
  age_exp_morb$white_age_18_44 <- age_exp_morb_pct$white_age_18_44 * pop_detail$white_age_18_44
  age_exp_morb$white_age_45_64 <- age_exp_morb_pct$white_age_45_64 * pop_detail$white_age_45_64
  age_exp_morb$white_age_65p <- age_exp_morb_pct$white_age_65p * pop_detail$white_age_65p
  age_exp_morb$black_age_18_44 <- age_exp_morb_pct$black_age_18_44 * pop_detail$black_age_18_44
  age_exp_morb$black_age_45_64 <- age_exp_morb_pct$black_age_45_64 * pop_detail$black_age_45_64
  age_exp_morb$black_age_65p <- age_exp_morb_pct$black_age_65p * pop_detail$black_age_65p
  age_exp_morb$hisp_age_18_44 <- age_exp_morb_pct$hisp_age_18_44 * pop_detail$hisp_age_18_44
  age_exp_morb$hisp_age_45_64 <- age_exp_morb_pct$hisp_age_45_64 * pop_detail$hisp_age_45_64
  age_exp_morb$hisp_age_65p <- age_exp_morb_pct$hisp_age_65p * pop_detail$hisp_age_65p
  age_exp_morb$rem_age_18_44 <- age_exp_morb_pct$rem_age_18_44 * pop_detail$rem_age_18_44
  age_exp_morb$rem_age_45_64 <- age_exp_morb_pct$rem_age_45_64 * pop_detail$rem_age_45_64
  age_exp_morb$rem_age_65p <- age_exp_morb_pct$rem_age_65p * pop_detail$rem_age_65p
  
  ## Add column for morbidity
  data %<>% mutate(MORB = 0)
  
  
  ## sample morbidity
  data$MORB[which(data$age_cat == "18-44" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_65p)] <- 1
  
  
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  raw_age_exp_morb <- list()
  raw_age_exp_morb$white <- ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$white_age_65p))  
  raw_age_exp_morb$black <- ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  raw_age_exp_morb$hisp <- ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  raw_age_exp_morb$rem <- ((morb_18_44_pct / 100) * (pop_detail$rem_age_18_44) +
                             (morb_45_64_pct / 100) * (pop_detail$rem_age_45_64) + 
                             (morb_65p_pct / 100) * (pop_detail$rem_age_65p))
  raw_age_exp_morb$white_pct <- raw_age_exp_morb$white / (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p)  
  raw_age_exp_morb$black_pct <- raw_age_exp_morb$black / (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) 
  raw_age_exp_morb$hisp_pct <- raw_age_exp_morb$hisp / (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) 
  raw_age_exp_morb$rem_pct <- raw_age_exp_morb$rem / (pop_detail$rem_age_18_44 + pop_detail$rem_age_45_64 + pop_detail$rem_age_65p)
  
  ## return
  return(list(data = data,
              white_sc = white_sc,
              black_sc = black_sc,
              hisp_sc = hisp_sc,
              age_exp_morb = age_exp_morb,
              age_exp_morb_pct = age_exp_morb_pct,
              raw_age_exp_morb = raw_age_exp_morb))
  
}



### Function to assign people with morbidity with a
### single comorbidity
sing_conditional_morbidity <- function(data,
                                       n_morb,
                                       pop,
                                       morb_18_44_pct,
                                       morb_45_64_pct,
                                       morb_65p_pct,
                                       morb_cond_pct,
                                       cond_fname,
                                       cond_precision = 0.02,
                                       morb_white,
                                       morb_black,
                                       morb_hisp,
                                       morb_ceiling = 0.8) {
  
  ### Calculate the BLACK scalar Non-Hispanic
  black_sc <- (morb_black / 100) * (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  
  ### Calculate the WHITE scalar Non-Hispanic
  white_sc <- (morb_white / 100) * (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$white_age_65p))
  
  ### Calculate the HISPANIC scalar - race doesn't matter
  hisp_sc <- (morb_hisp / 100) * (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  
  ### Calculate Age Group morbidity rate estimates for each race/eth
  age_exp_morb_pct <- list()
  age_exp_morb_pct$white_age_18_44 <- (morb_18_44_pct / 100) * white_sc
  age_exp_morb_pct$white_age_45_64 <- (morb_45_64_pct / 100) * white_sc
  age_exp_morb_pct$white_age_65p <- (morb_65p_pct / 100) * white_sc
  age_exp_morb_pct$black_age_18_44 <- (morb_18_44_pct / 100) * black_sc
  age_exp_morb_pct$black_age_45_64 <- (morb_45_64_pct / 100) * black_sc
  age_exp_morb_pct$black_age_65p <- (morb_65p_pct / 100) * black_sc
  age_exp_morb_pct$hisp_age_18_44 <- (morb_18_44_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_45_64 <- (morb_45_64_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_65p <- (morb_65p_pct / 100) * hisp_sc
  age_exp_morb_pct$rem_age_18_44 <- (morb_18_44_pct / 100)
  age_exp_morb_pct$rem_age_45_64 <- (morb_45_64_pct / 100)
  age_exp_morb_pct$rem_age_65p <- (morb_65p_pct / 100)
  
  ### Move any to ceiling value
  age_exp_morb_pct[which(age_exp_morb_pct > morb_ceiling)] <- morb_ceiling
  
  ### Calculate Age Group morbidity estimates for each age / race
  age_exp_morb <- list()
  age_exp_morb$white_age_18_44 <- age_exp_morb_pct$white_age_18_44 * pop_detail$white_age_18_44
  age_exp_morb$white_age_45_64 <- age_exp_morb_pct$white_age_45_64 * pop_detail$white_age_45_64
  age_exp_morb$white_age_65p <- age_exp_morb_pct$white_age_65p * pop_detail$white_age_65p
  age_exp_morb$black_age_18_44 <- age_exp_morb_pct$black_age_18_44 * pop_detail$black_age_18_44
  age_exp_morb$black_age_45_64 <- age_exp_morb_pct$black_age_45_64 * pop_detail$black_age_45_64
  age_exp_morb$black_age_65p <- age_exp_morb_pct$black_age_65p * pop_detail$black_age_65p
  age_exp_morb$hisp_age_18_44 <- age_exp_morb_pct$hisp_age_18_44 * pop_detail$hisp_age_18_44
  age_exp_morb$hisp_age_45_64 <- age_exp_morb_pct$hisp_age_45_64 * pop_detail$hisp_age_45_64
  age_exp_morb$hisp_age_65p <- age_exp_morb_pct$hisp_age_65p * pop_detail$hisp_age_65p
  age_exp_morb$rem_age_18_44 <- age_exp_morb_pct$rem_age_18_44 * pop_detail$rem_age_18_44
  age_exp_morb$rem_age_45_64 <- age_exp_morb_pct$rem_age_45_64 * pop_detail$rem_age_45_64
  age_exp_morb$rem_age_65p <- age_exp_morb_pct$rem_age_65p * pop_detail$rem_age_65p
  
  ## Get calculated number of morbid people
  n_morb_calc <- sum(age_exp_morb |> unlist())
  
  ## Total number of morbid people with comorbidity
  n_morb_cond <- round(n_morb_calc * (morb_cond_pct / 100))  
  
  ## Add column for target condition
  data %<>% mutate(MORB = 0)
  
  ## Do initial sample of the population in each group (blind)
  data$MORB[which(data$age_cat == "18-44" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_65p)] <- 1
  
  
  ## Corrections
  if (is.na(age_exp_morb$black_age_18_44)){
    age_exp_morb$black_age_18_44 <- 0
  }
  
  if (is.na(age_exp_morb$black_age_45_64)){
    age_exp_morb$black_age_45_64 <- 0
  }
  
  if (is.na(age_exp_morb$black_age_65p)){
    age_exp_morb$black_age_65p <- 0
  }
  
  
  ## How many with comorbidity
  n_morb_cond_init <- sum(data[,cond_fname] == 1 & data$MORB == 1)
  
  ## Calculate total error in overlap
  n_morb_cond_err <- n_morb_cond_init - n_morb_cond

  
  ##Corrections
  if (is.na(n_morb_cond_err)){
    n_morb_cond_err <- 0
  }
  
  if (is.na(n_morb_cond)){
    n_morb_cond <- 1
  }
  
  ## If statement to fix
  if (n_morb_cond_err / n_morb_cond > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond_err)] <- 0
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond_err)] <- 1
    
  } else if (n_morb_cond_err / n_morb_cond < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond_err)] <- 0
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond_err)] <- 1
 
  }
  
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  raw_age_exp_morb <- list()
  raw_age_exp_morb$white <- ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$white_age_65p))  
  raw_age_exp_morb$black <- ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  raw_age_exp_morb$hisp <- ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
                              (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) + 
                              (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  raw_age_exp_morb$rem <- ((morb_18_44_pct / 100) * (pop_detail$rem_age_18_44) +
                             (morb_45_64_pct / 100) * (pop_detail$rem_age_45_64) + 
                             (morb_65p_pct / 100) * (pop_detail$rem_age_65p))
  raw_age_exp_morb$white_pct <- raw_age_exp_morb$white / (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p)  
  raw_age_exp_morb$black_pct <- raw_age_exp_morb$black / (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) 
  raw_age_exp_morb$hisp_pct <- raw_age_exp_morb$hisp / (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) 
  raw_age_exp_morb$rem_pct <- raw_age_exp_morb$rem / (pop_detail$rem_age_18_44 + pop_detail$rem_age_45_64 + pop_detail$rem_age_65p)
  
  ## return
  return(list(data = data,
              white_sc = white_sc,
              black_sc = black_sc,
              hisp_sc = hisp_sc,
              age_exp_morb = age_exp_morb,
              age_exp_morb_pct = age_exp_morb_pct,
              raw_age_exp_morb = raw_age_exp_morb))
  
}







double_conditional_morbidity <- function(data,
                                         n_morb,
                                         pop,
                                         morb_18_44_pct,
                                         morb_45_64_pct,
                                         morb_65p_pct,
                                         morb_cond_pct,
                                         cond_fname,
                                         morb_cond2_pct,
                                         cond2_fname,
                                         cond_precision = 0.02,
                                         morb_white,
                                         morb_black,
                                         morb_hisp,
                                         morb_ceiling = 0.8) {
  
  ### Calculate the BLACK scalar Non-Hispanic
  black_sc <- (morb_black / 100) * (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  
  ### Calculate the WHITE scalar Non-Hispanic
  white_sc <- (morb_white / 100) * (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$white_age_65p))
  
  ### Calculate the HISPANIC scalar - race doesn't matter
  hisp_sc <- (morb_hisp / 100) * (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  
  ### Calculate Age Group morbidity rate estimates for each race/eth
  age_exp_morb_pct <- list()
  age_exp_morb_pct$white_age_18_44 <- (morb_18_44_pct / 100) * white_sc
  age_exp_morb_pct$white_age_45_64 <- (morb_45_64_pct / 100) * white_sc
  age_exp_morb_pct$white_age_65p <- (morb_65p_pct / 100) * white_sc
  age_exp_morb_pct$black_age_18_44 <- (morb_18_44_pct / 100) * black_sc
  age_exp_morb_pct$black_age_45_64 <- (morb_45_64_pct / 100) * black_sc
  age_exp_morb_pct$black_age_65p <- (morb_65p_pct / 100) * black_sc
  age_exp_morb_pct$hisp_age_18_44 <- (morb_18_44_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_45_64 <- (morb_45_64_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_65p <- (morb_65p_pct / 100) * hisp_sc
  age_exp_morb_pct$rem_age_18_44 <- (morb_18_44_pct / 100)
  age_exp_morb_pct$rem_age_45_64 <- (morb_45_64_pct / 100)
  age_exp_morb_pct$rem_age_65p <- (morb_65p_pct / 100)
  
  ### Move any to ceiling value
  age_exp_morb_pct[which(age_exp_morb_pct > morb_ceiling)] <- morb_ceiling
  
  ### Calculate Age Group morbidity estimates for each age / race
  age_exp_morb <- list()
  age_exp_morb$white_age_18_44 <- age_exp_morb_pct$white_age_18_44 * pop_detail$white_age_18_44
  age_exp_morb$white_age_45_64 <- age_exp_morb_pct$white_age_45_64 * pop_detail$white_age_45_64
  age_exp_morb$white_age_65p <- age_exp_morb_pct$white_age_65p * pop_detail$white_age_65p
  age_exp_morb$black_age_18_44 <- age_exp_morb_pct$black_age_18_44 * pop_detail$black_age_18_44
  age_exp_morb$black_age_45_64 <- age_exp_morb_pct$black_age_45_64 * pop_detail$black_age_45_64
  age_exp_morb$black_age_65p <- age_exp_morb_pct$black_age_65p * pop_detail$black_age_65p
  age_exp_morb$hisp_age_18_44 <- age_exp_morb_pct$hisp_age_18_44 * pop_detail$hisp_age_18_44
  age_exp_morb$hisp_age_45_64 <- age_exp_morb_pct$hisp_age_45_64 * pop_detail$hisp_age_45_64
  age_exp_morb$hisp_age_65p <- age_exp_morb_pct$hisp_age_65p * pop_detail$hisp_age_65p
  age_exp_morb$rem_age_18_44 <- age_exp_morb_pct$rem_age_18_44 * pop_detail$rem_age_18_44
  age_exp_morb$rem_age_45_64 <- age_exp_morb_pct$rem_age_45_64 * pop_detail$rem_age_45_64
  age_exp_morb$rem_age_65p <- age_exp_morb_pct$rem_age_65p * pop_detail$rem_age_65p
  
  ## Get calculated number of morbid people
  n_morb_calc <- sum(age_exp_morb |> unlist())

  ## Add column for target condition
  data %<>% mutate(MORB = 0)
  
  ## Do initial sample of the population in each group (blind)
  data$MORB[which(data$age_cat == "18-44" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_65p)] <- 1
  
  
  ## Corrections
  if (is.na(age_exp_morb$black_age_18_44)){
    age_exp_morb$black_age_18_44 <- 0
  }
  
  if (is.na(age_exp_morb$black_age_45_64)){
    age_exp_morb$black_age_45_64 <- 0
  }
  
  if (is.na(age_exp_morb$black_age_65p)){
    age_exp_morb$black_age_65p <- 0
  }
  
  
  ## Total of target condition people with first comorbidity
  n_morb_cond <- round(n_morb_calc * (morb_cond_pct / 100))  
  
  ## How many with comorbidity
  n_morb_cond_init <- sum(data[,cond_fname] == 1 & data$MORB == 1)  
  
  ## Calculate total error in overlap
  n_morb_cond_err <- n_morb_cond_init - n_morb_cond
  

  ##Corrections
  if (is.na(n_morb_cond_err)){
    n_morb_cond_err <- 0
  }
  
  if (is.na(n_morb_cond)){
    n_morb_cond <- 1
  }
  
  ## If statement to fix
  if (n_morb_cond_err / n_morb_cond > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond_err)] <- 0
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond_err)] <- 1
    
  } else if (n_morb_cond_err / n_morb_cond < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond_err)] <- 0
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond_err)] <- 1
    
  }
  
  # sum(data[,cond_fname] == 1 & data$MORB == 1)
  
  ## Total of target condition people with second comorbidity
  n_morb_cond2 <- round(n_morb_calc * (morb_cond2_pct / 100))  
  
  ## How many with comorbidity
  n_morb_cond2_init <- sum(data[,cond2_fname] == 1 & data$MORB == 1)   
  
  ## Calculate total error in overlap
  n_morb_cond2_err <- n_morb_cond2_init - n_morb_cond2
  
  ##Corrections
  if (is.na(n_morb_cond2_err)){
    n_morb_cond2_err <- 0
  }
  
  if (is.na(n_morb_cond2)){
    n_morb_cond2 <- 1
  }
  
  ## If statement to fix
  if (n_morb_cond2_err / n_morb_cond2 > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond2_err)] <- 0
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond2_err)] <- 1
    
  } else if (n_morb_cond2_err / n_morb_cond2 < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond2_err)] <- 0
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond2_err)] <- 1
    
  }
  
  ### Check accuracy
  n_morb_cond_r1 <- sum(data[,cond_fname] == 1 & data$MORB == 1)
  n_morb_cond_err_r1 <- n_morb_cond_r1 - n_morb_cond
  
  ## If statement to fix
  if (n_morb_cond_err_r1 / n_morb_cond > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond_err_r1)] <- 0
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond_err_r1)] <- 1
    
  } else if (n_morb_cond_err / n_morb_cond < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond_err_r1)] <- 0
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond_err_r1)] <- 1
    
  }
  
  ### Check accuracy
  n_morb_cond2_r1 <- sum(data[,cond2_fname] == 1 & data$MORB == 1)
  n_morb_cond2_err_r1 <- n_morb_cond2_r1 - n_morb_cond2 
  
  ## If statement to fix
  if (n_morb_cond2_err_r1 / n_morb_cond2 > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond2_err_r1)] <- 0
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond2_err_r1)] <- 1
    
  } else if (n_morb_cond2_err_r1 / n_morb_cond2 < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond2_err_r1)] <- 0
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond2_err_r1)] <- 1
    
  }  
  
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  raw_age_exp_morb <- list()
  raw_age_exp_morb$white <- ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$white_age_65p))  
  raw_age_exp_morb$black <- ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  raw_age_exp_morb$hisp <- ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
                              (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) + 
                              (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  raw_age_exp_morb$rem <- ((morb_18_44_pct / 100) * (pop_detail$rem_age_18_44) +
                             (morb_45_64_pct / 100) * (pop_detail$rem_age_45_64) + 
                             (morb_65p_pct / 100) * (pop_detail$rem_age_65p))
  raw_age_exp_morb$white_pct <- raw_age_exp_morb$white / (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p)  
  raw_age_exp_morb$black_pct <- raw_age_exp_morb$black / (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) 
  raw_age_exp_morb$hisp_pct <- raw_age_exp_morb$hisp / (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) 
  raw_age_exp_morb$rem_pct <- raw_age_exp_morb$rem / (pop_detail$rem_age_18_44 + pop_detail$rem_age_45_64 + pop_detail$rem_age_65p)
  
  ## return
  return(list(data = data,
              white_sc = white_sc,
              black_sc = black_sc,
              hisp_sc = hisp_sc,
              age_exp_morb = age_exp_morb,
              age_exp_morb_pct = age_exp_morb_pct,
              raw_age_exp_morb = raw_age_exp_morb))
             
  
}




triple_conditional_morbidity <- function(data,
                                         n_morb,
                                         pop,
                                         morb_18_44_pct,
                                         morb_45_64_pct,
                                         morb_65p_pct,
                                         morb_cond_pct,
                                         cond_fname,
                                         morb_cond2_pct,
                                         cond2_fname,
                                         morb_cond3_pct,
                                         cond3_fname,
                                         cond_precision = 0.02,
                                         morb_white,
                                         morb_black,
                                         morb_hisp,
                                         morb_ceiling = 0.8) {
  
  ### Calculate the BLACK scalar Non-Hispanic
  black_sc <- (morb_black / 100) * (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  
  ### Calculate the WHITE scalar Non-Hispanic
  white_sc <- (morb_white / 100) * (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$white_age_65p))
  
  ### Calculate the HISPANIC scalar - race doesn't matter
  hisp_sc <- (morb_hisp / 100) * (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) /
    ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
       (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) +
       (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  
  
  ### Calculate Age Group morbidity rate estimates for each race/eth
  age_exp_morb_pct <- list()
  age_exp_morb_pct$white_age_18_44 <- (morb_18_44_pct / 100) * white_sc
  age_exp_morb_pct$white_age_45_64 <- (morb_45_64_pct / 100) * white_sc
  age_exp_morb_pct$white_age_65p <- (morb_65p_pct / 100) * white_sc
  age_exp_morb_pct$black_age_18_44 <- (morb_18_44_pct / 100) * black_sc
  age_exp_morb_pct$black_age_45_64 <- (morb_45_64_pct / 100) * black_sc
  age_exp_morb_pct$black_age_65p <- (morb_65p_pct / 100) * black_sc
  age_exp_morb_pct$hisp_age_18_44 <- (morb_18_44_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_45_64 <- (morb_45_64_pct / 100) * hisp_sc
  age_exp_morb_pct$hisp_age_65p <- (morb_65p_pct / 100) * hisp_sc
  age_exp_morb_pct$rem_age_18_44 <- (morb_18_44_pct / 100)
  age_exp_morb_pct$rem_age_45_64 <- (morb_45_64_pct / 100)
  age_exp_morb_pct$rem_age_65p <- (morb_65p_pct / 100)
  
  ### Move any to ceiling value
  age_exp_morb_pct[which(age_exp_morb_pct > morb_ceiling)] <- morb_ceiling
  
  ### Calculate Age Group morbidity estimates for each age / race
  age_exp_morb <- list()
  age_exp_morb$white_age_18_44 <- age_exp_morb_pct$white_age_18_44 * pop_detail$white_age_18_44
  age_exp_morb$white_age_45_64 <- age_exp_morb_pct$white_age_45_64 * pop_detail$white_age_45_64
  age_exp_morb$white_age_65p <- age_exp_morb_pct$white_age_65p * pop_detail$white_age_65p
  age_exp_morb$black_age_18_44 <- age_exp_morb_pct$black_age_18_44 * pop_detail$black_age_18_44
  age_exp_morb$black_age_45_64 <- age_exp_morb_pct$black_age_45_64 * pop_detail$black_age_45_64
  age_exp_morb$black_age_65p <- age_exp_morb_pct$black_age_65p * pop_detail$black_age_65p
  age_exp_morb$hisp_age_18_44 <- age_exp_morb_pct$hisp_age_18_44 * pop_detail$hisp_age_18_44
  age_exp_morb$hisp_age_45_64 <- age_exp_morb_pct$hisp_age_45_64 * pop_detail$hisp_age_45_64
  age_exp_morb$hisp_age_65p <- age_exp_morb_pct$hisp_age_65p * pop_detail$hisp_age_65p
  age_exp_morb$rem_age_18_44 <- age_exp_morb_pct$rem_age_18_44 * pop_detail$rem_age_18_44
  age_exp_morb$rem_age_45_64 <- age_exp_morb_pct$rem_age_45_64 * pop_detail$rem_age_45_64
  age_exp_morb$rem_age_65p <- age_exp_morb_pct$rem_age_65p * pop_detail$rem_age_65p

  ## Get calculated number of morbid people
  n_morb_calc <- sum(age_exp_morb |> unlist())
  
  ## Add column for target condition
  data %<>% mutate(MORB = 0)
  
  ## Do initial sample of the population in each group (blind)
  data$MORB[which(data$age_cat == "18-44" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 1 & data$eth == "NH") %>% sample(age_exp_morb$white_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race == 2 & data$eth == "NH") %>% sample(age_exp_morb$black_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$eth == "H") %>% sample(age_exp_morb$hisp_age_65p)] <- 1
  data$MORB[which(data$age_cat == "18-44" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_18_44)] <- 1
  data$MORB[which(data$age_cat == "45-64" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_45_64)] <- 1
  data$MORB[which(data$age_cat == "65p" & data$race %in% c(0, 3:9)) %>% sample(age_exp_morb$rem_age_65p)] <- 1
  
  
  ## Corrections
  if (is.na(age_exp_morb$black_age_18_44)){
    age_exp_morb$black_age_18_44 <- 0
  }
  
  if (is.na(age_exp_morb$black_age_45_64)){
    age_exp_morb$black_age_45_64 <- 0
  }
  
  if (is.na(age_exp_morb$black_age_65p)){
    age_exp_morb$black_age_65p <- 0
  }
  
  ## Total of target condition people with first comorbidity
  n_morb_cond <- round(n_morb_calc * (morb_cond_pct / 100))  
  
  ## How many with comorbidity
  n_morb_cond_init <- sum(data[,cond_fname] == 1 & data$MORB == 1)  
  
  ## Calculate total error in overlap
  n_morb_cond_err <- n_morb_cond_init - n_morb_cond
  
  
  ##Corrections
  if (is.na(n_morb_cond_err)){
    n_morb_cond_err <- 0
  }
  
  if (is.na(n_morb_cond)){
    n_morb_cond <- 1
  }
  
  ## If statement to fix
  if (n_morb_cond_err / n_morb_cond > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond_err)] <- 0
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond_err)] <- 1
    
  } else if (n_morb_cond_err / n_morb_cond < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond_err)] <- 0
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond_err)] <- 1
    
  }
  
  # sum(data[,cond_fname] == 1 & data$MORB == 1)
  
  ## Total of target condition people with second comorbidity
  n_morb_cond2 <- round(n_morb_calc * (morb_cond2_pct / 100))  
  
  ## How many with comorbidity
  n_morb_cond2_init <- sum(data[,cond2_fname] == 1 & data$MORB == 1)   
  
  ## Calculate total error in overlap
  n_morb_cond2_err <- n_morb_cond2_init - n_morb_cond2
  
  ##Corrections
  if (is.na(n_morb_cond2_err)){
    n_morb_cond2_err <- 0
  }
  
  if (is.na(n_morb_cond2)){
    n_morb_cond2 <- 1
  }
  
  ## If statement to fix
  if (n_morb_cond2_err / n_morb_cond2 > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond2_err)] <- 0
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond2_err)] <- 1
    
  } else if (n_morb_cond2_err / n_morb_cond2 < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond2_err)] <- 0
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond2_err)] <- 1
    
  }
  
  ## Total of target condition people with third comorbidity
  n_morb_cond3 <- round(n_morb_calc * (morb_cond3_pct / 100))  
  
  ## How many with comorbidity
  n_morb_cond3_init <- sum(data[,cond3_fname] == 1 & data$MORB == 1)   
  
  ## Calculate total error in overlap
  n_morb_cond3_err <- n_morb_cond3_init - n_morb_cond3
  
  ##Corrections
  if (is.na(n_morb_cond3_err)){
    n_morb_cond3_err <- 0
  }
  
  if (is.na(n_morb_cond3)){
    n_morb_cond3 <- 1
  }
  
  ## If statement to fix
  if (n_morb_cond3_err / n_morb_cond3 > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond3_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond3_err)] <- 0
    data$MORB[which(data[,cond3_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond3_err)] <- 1
    
  } else if (n_morb_cond3_err / n_morb_cond3 < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond3_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond3_err)] <- 0
    data$MORB[which(data[,cond3_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond3_err)] <- 1
    
  }
  
  ## How many with comorbidity
  n_morb_cond_r1 <- sum(data[,cond_fname] == 1 & data$MORB == 1)  
  
  ## Calculate total error in overlap
  n_morb_cond_err_r1 <- n_morb_cond_r1 - n_morb_cond
  
  ## If statement to fix
  if (n_morb_cond_err_r1 / n_morb_cond > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond_err_r1)] <- 0
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond_err_r1)] <- 1
    
  } else if (n_morb_cond_err_r1 / n_morb_cond < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond_err_r1)] <- 0
    data$MORB[which(data[,cond_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond_err_r1)] <- 1
    
  }
    
  ### Check accuracy
  n_morb_cond2_r1 <- sum(data[,cond2_fname] == 1 & data$MORB == 1)
  n_morb_cond2_err_r1 <- n_morb_cond2_r1 - n_morb_cond2 
  
  ## If statement to fix
  if (n_morb_cond2_err_r1 / n_morb_cond2 > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond2_err_r1)] <- 0
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond2_err_r1)] <- 1
    
  } else if (n_morb_cond2_err_r1 / n_morb_cond2 < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond2_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond2_err_r1)] <- 0
    data$MORB[which(data[,cond2_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond2_err_r1)] <- 1
    
  }  
  
  ### Check accuracy
  n_morb_cond3_r1 <- sum(data[,cond3_fname] == 1 & data$MORB == 1)
  n_morb_cond3_err_r1 <- n_morb_cond3_r1 - n_morb_cond3 
  
  ## If statement to fix
  if (n_morb_cond3_err_r1 / n_morb_cond3 > cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond3_fname] == 1 & data$MORB == 1) %>% sample(n_morb_cond3_err_r1)] <- 0
    data$MORB[which(data[,cond3_fname] == 0 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(n_morb_cond3_err_r1)] <- 1
    
  } else if (n_morb_cond3_err_r1 / n_morb_cond3 < -cond_precision) {
    
    ## Do re-sample of the population in each group
    data$MORB[which(data[,cond3_fname] == 0 & data$MORB == 1) %>% sample(-n_morb_cond3_err_r1)] <- 0
    data$MORB[which(data[,cond3_fname] == 1 & data$MORB == 0 & data$age_cat != "0-17") %>% sample(-n_morb_cond3_err_r1)] <- 1
    
  }   
  
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  ### Calculate RAW AGE GROUP morbidity rate estimates for each race (ONLY FOR REPORTING)
  raw_age_exp_morb <- list()
  raw_age_exp_morb$white <- ((morb_18_44_pct / 100) * (pop_detail$white_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$white_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$white_age_65p))  
  raw_age_exp_morb$black <- ((morb_18_44_pct / 100) * (pop_detail$black_age_18_44) +
                               (morb_45_64_pct / 100) * (pop_detail$black_age_45_64) + 
                               (morb_65p_pct / 100) * (pop_detail$black_age_65p))
  raw_age_exp_morb$hisp <- ((morb_18_44_pct / 100) * (pop_detail$hisp_age_18_44) +
                              (morb_45_64_pct / 100) * (pop_detail$hisp_age_45_64) + 
                              (morb_65p_pct / 100) * (pop_detail$hisp_age_65p))
  raw_age_exp_morb$rem <- ((morb_18_44_pct / 100) * (pop_detail$rem_age_18_44) +
                             (morb_45_64_pct / 100) * (pop_detail$rem_age_45_64) + 
                             (morb_65p_pct / 100) * (pop_detail$rem_age_65p))
  raw_age_exp_morb$white_pct <- raw_age_exp_morb$white / (pop_detail$white_age_18_44 + pop_detail$white_age_45_64 + pop_detail$white_age_65p)  
  raw_age_exp_morb$black_pct <- raw_age_exp_morb$black / (pop_detail$black_age_18_44 + pop_detail$black_age_45_64 + pop_detail$black_age_65p) 
  raw_age_exp_morb$hisp_pct <- raw_age_exp_morb$hisp / (pop_detail$hisp_age_18_44 + pop_detail$hisp_age_45_64 + pop_detail$hisp_age_65p) 
  raw_age_exp_morb$rem_pct <- raw_age_exp_morb$rem / (pop_detail$rem_age_18_44 + pop_detail$rem_age_45_64 + pop_detail$rem_age_65p)
  
  ## return
  return(list(data = data,
              white_sc = white_sc,
              black_sc = black_sc,
              hisp_sc = hisp_sc,
              age_exp_morb = age_exp_morb,
              age_exp_morb_pct = age_exp_morb_pct,
              raw_age_exp_morb = raw_age_exp_morb))
  
}



### Function to adjust age-specific morbidity rates to account
### for varying population sizes
scale_age_morbidity <- function(data,
                                n_morb,
                                pop,
                                morb_18_44_pct,
                                morb_45_64_pct,
                                morb_65p_pct) {
  
  ## Calculate scaler 
  scaler <- n_morb / (((morb_18_44_pct/100) * pop$age_18_44) + 
                        ((morb_45_64_pct/100) * pop$age_45_64) + 
                        ((morb_65p_pct/100) * pop$age_65p))
  
  ## Calculate scaled % for each age group
  sc_morb_prop <- list()
  sc_morb_prop$age_18_44 <- scaler * (morb_18_44_pct/100)
  sc_morb_prop$age_45_64 <- scaler * (morb_45_64_pct/100)
  sc_morb_prop$age_65p <- scaler * (morb_65p_pct/100)
  
  ## Calculate the number of people to assign morb to from each age group
  n_morb_age <- list()
  n_morb_age$age_18_44 <- round(sc_morb_prop$age_18_44 * pop$age_18_44)
  n_morb_age$age_45_64 <- round(sc_morb_prop$age_45_64 * pop$age_45_64)
  n_morb_age$age_65p <- round(sc_morb_prop$age_65p * pop$age_65p)
  
  return(list(n_morb_age = n_morb_age,
              scaler = scaler,
              sc_morb_prop = sc_morb_prop))
  
}

