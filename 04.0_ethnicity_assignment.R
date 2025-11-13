# =========================================================
# Title: Ethnicity assignment
# Author: Hilary Sandborn & Paul Delamater
# Date: 2025-10-28
# Description: Assign ethnicity to agents in a synthetic population
# =========================================================


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



##### Load data #####

## Age group county-level census data w/ counts of total population by race group, counts of hispanic population by race group, and proportion of Hispanic in each race group
H_0017 <- read_csv(file.path(data_path, "H_0017.csv"))
H_1844 <- read_csv(file.path(data_path, "H_1844.csv"))
H_4564 <- read_csv(file.path(data_path, "H_4564.csv"))
H_65p <- read_csv(file.path(data_path, "H_65p.csv"))

## Fill in with the location of the county-level SynthPop data w/ group quarters population
counties <- list.files("file_path")



### Assignment algorithm ######

## Loop through counties
for (i in 1:length(counties)) {
  
  # Print county
  print(paste0("County: ", counties[i], " ------------------------------------------------"))
  
  # Read the synthetic population text file in as a table -----
  # Fill in with the location of the county-level SynthPop data w/ group quarters population
  dat <- read_table(paste0("file_path", counties[i], "/people.txt"), show_col_types = FALSE)
  dat_gq <- read_csv(paste0("file_path", counties[i], "_gq.csv"), show_col_types = FALSE)

  ##
  ## Merge pop files
  ##
  dat %<>% mutate(gq_type = NA)
  dat_gq %<>% mutate(relate = NA,
                     hisp = NA)
  
  ## Rename col
  dat_gq %<>% rename(sp_hh_id = sp_gq_id)
  
  ## Recode sex column
  dat_gq %<>% mutate(sex = case_when(sex == "M" ~ 1, 
                                     sex == "F" ~ 2,
                                     .default = NA))
  
  ## Bind
  if (nrow(dat_gq) > 0) dat %<>% bind_rows(dat_gq)
  
  
  ## Add eth column to tables - set everyone as NH to start
  dat %<>% mutate(eth = "NH")
  
  ## Create age categories -----
  dat %<>% mutate(age_cat = case_when(age <= 17 ~ "0-17",
                                      age > 17 & age < 45 ~ "18-44",
                                      age > 44 & age < 65 ~ "45-64",
                                      age > 64 ~ "65p"))
  
  
  ## Add eth (hispanic) flag column for bookkeeping purposes with kids/adults
  dat %<>% mutate(eth_hh = 0)

    
  ######################################################
  #### Get expected Hispanic pop by age group #####
  
  age_map <- list(
    "0-17" = "0017",
    "18-44" = "1844",
    "45-64" = "4564",
    "65p" = "65p"
  )
  
  race_map <- list(
    "WHITE" = 1,
    "BLACK" = 2,
    "AIAN" = c(3, 4, 5),
    "ASIAN" = 6,
    "NHPI" = 7,
    "OTHER" = 8,
    "TWOPL" = 9
  )
  
  rti_est_hisp_counts_age <- list()
  
  for (age_label in names(age_map)) {
    
    ## Set variables for selecting
    age_code <- age_map[[age_label]]
    my_table <- get(paste0("H_", age_code))
    
    ## Get overall Hispanic percent for county
    total_pct_val <- sum(my_table[i, 9:15]) / sum(my_table[i, 2:8])
    
    ## Calculate
    rti_pop <- sum(dat$age_cat == age_label & is.na(dat$gq_type))
    rti_pop_gq <- sum(dat$age_cat == age_label & !is.na(dat$gq_type))
    rti_hisp <- rti_pop * total_pct_val
    rti_hisp_gq <- rti_pop_gq * total_pct_val

    ## Round
    rti_n_hisp <- round_preserve_sum(c(rti_hisp, rti_hisp_gq))
    
    ## 
    rti_est_hisp_counts_age[[age_label]] <- list(
      rti_n_hisp = rti_n_hisp[1],
      rti_n_hisp_gq = rti_n_hisp[2],
      rti_pop = rti_pop,
      rti_pop_gq = rti_pop_gq
    )
    
  }
  
  
  ### Get expected Hispanic pop by age & race ############
  
  rti_est_hisp_counts_age_race <- list()
  
  for (age_label in names(age_map)) {
    
    ## Set variables for selecting
    age_code <- age_map[[age_label]]
    table <- get(paste0("H_", age_code))
    
    rti_est_hisp_counts_age_race[[age_label]] <- list()
    
    for (race in names(race_map)) {
      
      ## Set variables for selecting
      race_code <- race_map[[race]]
      pct_col <- paste0(race, "_PCT", age_code)
      pct_val <- table[[pct_col]][i]
      
      ## Calculate
      rti_pop <- sum(dat$age_cat == age_label & dat$race %in% race_code & is.na(dat$gq_type))
      rti_pop_gq <- sum(dat$age_cat == age_label & dat$race %in% race_code & !is.na(dat$gq_type))
      rti_hisp <- rti_pop * pct_val
      rti_hisp_gq <- rti_pop_gq * pct_val

      ## Round
      rti_n_hisp <- round_preserve_sum(c(rti_hisp, rti_hisp_gq))
      
      # Store results
      rti_est_hisp_counts_age_race[[age_label]][[race]] <- list(
        rti_n_hisp = rti_n_hisp[1],
        rti_n_hisp_gq = rti_n_hisp[2],
        rti_pop = rti_pop,
        rti_pop_gq = rti_pop_gq)
      
    }
  }
  
  ######################################################
  
  
  # Make an empty list to hold results
  results <- list()
  
  for (age_label in names(age_map)) {
    
    ## Set variables for selecting
    age_code <- age_map[[age_label]]
    
    ## Get values
    h_pops <- map(rti_est_hisp_counts_age_race[[age_label]], ~ .[[1]])
    h_est <- rti_est_hisp_counts_age[[age_label]][1] %>% as.numeric()
    pops <- map(rti_est_hisp_counts_age_race[[age_label]], ~ .[[3]])
    props <- my_table[i, c(16:22)]
    
    ## What's the scale factor
    original_scale_factor <- (h_est / sum(h_pops %>% as.numeric()))
    
    ## Scale Hisp pops
    scale_h_pops <- h_pops %>% unlist * original_scale_factor
    
    ## Holder for constrained pops
    which_over_pop <- NULL
    
    ## Test whether any are larger than total pop
    while (sum(unlist(scale_h_pops) > unlist(pops)) >= 1) {
      
      ## Determine which are greater
      which_over_pop <- c(which_over_pop, which(scale_h_pops > pops))
      
      ## Scale to 95% or current value (whichever is higher)
      props[which_over_pop] <- ifelse(props[which_over_pop] < 0.95, 
                                      0.95, 
                                      props[which_over_pop])
      
      ## Get counts of constrained
      over_prop_count <- (pops[which_over_pop] * props[which_over_pop]) %>% sum()
      
      ## What's the NEW scale factor
      upd_scale_factor <- ((h_est - over_prop_count) / sum(h_pops[-which_over_pop] %>% as.numeric())) 
      
      if (!is.infinite(upd_scale_factor)) {
        
        ## Scale Hisp pops
        scale_h_pops <- h_pops %>% unlist * upd_scale_factor
        
        ## Replace constrained
        scale_h_pops[which_over_pop] <- (pops[which_over_pop] * props[which_over_pop]) %>% as.numeric()
        
      } else {
        scale_h_pops[which_over_pop] <- pops[which_over_pop]
        
      }
      
    } ## end of while loop
    
    ## Round preserve sum 
    scale_h_pops <- round_preserve_sum(unlist(scale_h_pops))
    
    ## Save results for all age groups
    results[[age_label]] <- list(
      h_pops       = h_pops,
      h_est        = h_est,
      pops         = pops,
      props        = props,
      scale_h_pops = scale_h_pops
    )
    
  }
  
    
  for (race in names(race_map)) {
      race_code <- race_map[[race]]
      
      ## Set sample factor (deals with children nested in households)
      sample_factor = 1.75
      
      ## Check for any people to assign to hispanic
      if (sum(dat$race %in% race_code & dat$age_cat == "0-17") == 0 | results[["0-17"]][["pops"]][[race]] == 0) {
        
        ## No assignment!
        
      } else { 
        
        ## Get indices of people in race/age category
        elg <- which(dat$race %in% race_code & dat$age_cat == "0-17" & is.na(dat$gq_type))
        elg_gq <- which(dat$race %in% race_code & dat$age_cat == "0-17" & !is.na(dat$gq_type))
        
        
        if (rti_est_hisp_counts_age_race[["0-17"]][[race]][["rti_n_hisp_gq"]] > 0) { 
          
          ## Sample
          hisp_samp_gq <- sample(elg_gq, size = rti_est_hisp_counts_age_race[["0-17"]][[race]][["rti_n_hisp_gq"]])
          
          ## Assign
          dat$eth[hisp_samp_gq] <- "H"
          
          ##
          print(paste0("- Assigned ", rti_est_hisp_counts_age_race[["0-17"]][[race]][["rti_n_hisp_gq"]], " HISPANIC ", race, " GQ individuals"))
          
        } else {
          
          print(paste0("- Assigned ", 0, " HISPANIC ", race, " GQ individuals"))
        }
        
        ## Count by household
        elg_hh <- dat %>% filter(race %in% race_code & age_cat == "0-17" & is.na(gq_type)) %>%
          group_by(sp_hh_id) %>%
          summarize(count = n())
        
        ##
        print(paste0("- Target: ", results[["0-17"]][["scale_h_pops"]][[race]], " HISPANIC ", race, " individuals"))
        
        
        ## Check if adjusted sample size is larger than n households
        if (results[["0-17"]][["scale_h_pops"]][[race]]/sample_factor > length(elg_hh$sp_hh_id)) sample_factor <- results[["0-17"]][["scale_h_pops"]][[race]] / length(elg_hh$sp_hh_id)
        
        ## Sample, if greater than 1
        if (nrow(elg_hh) > 1) hisp_samp <- sample(elg_hh$sp_hh_id, size = round(results[["0-17"]][["scale_h_pops"]][[race]]/sample_factor))
        if (nrow(elg_hh) == 1) hisp_samp <- elg_hh$sp_hh_id
        
        ## 
        full_hh_count <- sum(elg_hh$count[elg_hh$sp_hh_id %in% hisp_samp])
        
        ##
        print(paste0("  - Entire HH Sample: ", full_hh_count, " HISPANIC ", race, " individuals"))
        
        ##
        ## Loop to deal with over/under sampling
        ##
        
        ## Create flag for unresolvable sample size
        unresolvable <- 0
        
        ## Create adjustable version of sample factor
        sf <- sample_factor
        
        ## While counter
        count_w <- 1
        
        ## Set up while loop - 0.95 and 0.85
        while (full_hh_count > round(results[["0-17"]][["scale_h_pops"]][[race]] * 0.95, 0) | 
               full_hh_count < round(results[["0-17"]][["scale_h_pops"]][[race]] * 0.85, 0)) {
          
          ##
          if (count_w == 1) print("  - Modifying sample size of households")
          
          ## Adjust sample factor
          if (full_hh_count > (results[["0-17"]][["scale_h_pops"]][[race]] * 0.95)) sf <- sf * 1.001
          if (full_hh_count < ((results[["0-17"]][["scale_h_pops"]][[race]] * 0.95) * 0.85)) sf <- sf / 1.001
          
          ## Resample 
          hisp_samp <- sample(as.character(elg_hh$sp_hh_id), size = round(results[["0-17"]][["scale_h_pops"]][[race]]/sf))
          
          
          ## 
          full_hh_count <- sum(elg_hh$count[elg_hh$sp_hh_id %in% hisp_samp])
          
          
          ### NEW ###
          target <- round(results[["0-17"]][["scale_h_pops"]][[race]] * 0.95, 0)
          if (min(elg_hh$count) > target || (sum(elg_hh$count) > target && sum(elg_hh$count) - min(elg_hh$count) < target)) {
            print("    - Unresolvable sample size: whole households overshoot target")
            unresolvable <- 1
            break()   # exit the while loop
          }
          
          
          ## Counter
          count_w <- count_w + 1
          
          ## Deal with "unresolvable" samples, e.g., min number of kids in house > sample size
          if (min(elg_hh$count) > round(results[["0-17"]][["scale_h_pops"]][[race]] * 0.95, 0)) {
            
            ## 
            print("    - Unresolvable sample size and household size")
            
            ## Set flag
            unresolvable <- 1
            
            ## BUST OUT
            break()
            
          }
          
        } ## end of while loop
        
        ## Only print if evidence of entering while loop
        if (count_w > 1) print(paste0("  - Modified Entire HH Sample: ", full_hh_count, " HISPANIC ", race, " individuals"))
        
        ## Get the individuals indices of the sampled households
        elg_hh_pop <- which(dat$race %in% race_code & dat$age_cat == "0-17" & is.na(dat$gq_type) & dat$sp_hh_id %in% hisp_samp)
        
        ## Deal with unresolvable size 
        #if (unresolvable == 1) elg_hh_pop <- sample(elg_hh$sp_hh_id, round(exp_group_hisp_values[["0-17"]][[race]][["n_hisp"]] * 0.95, 0))
        
        ## NEW ##
        if (unresolvable == 1) {
          # Get all eligible individuals (children in eligible households)
          elg_hh_pop <- which(dat$race %in% race_code & dat$age_cat == "0-17" & is.na(dat$gq_type))
          
          # Sample exactly the target number of individuals
          elg_hh_pop <- sample(elg_hh_pop, size = target)
        }
        
        ## Assign
        dat$eth[elg_hh_pop] <- "H"
        
        ##
        print(paste0("- Assigned ", length(elg_hh_pop), " HISPANIC ", race, " HH individuals"))
        
        ## Calculate the number left to assign
        hisp_samp_rem <- results[["0-17"]][["scale_h_pops"]][[race]] - length(elg_hh_pop)
        
        ## Get the individuals indices of remaining people who could be assigned
        elg_rem <- which(dat$race %in% race_code & dat$age_cat == "0-17" & is.na(dat$gq_type) & dat$eth == "NH")
        
        ## Sample - maybe need to add something here that says if hisp_samp_rem > 0
        hisp_samp_rem <- sample(elg_rem, size = hisp_samp_rem)
        
        
        ## Assign
        dat$eth[hisp_samp_rem] <- "H"
        
        ## Flag the Household members
        eth_hh_flag_ids <- dat$sp_hh_id[dat$race %in% race_code & dat$eth == "H"] %>% unique()
        dat %<>% mutate(eth_hh = case_when(sp_hh_id %in% eth_hh_flag_ids ~ 1,
                                           .default = eth_hh))
        
        ##
        print(paste0("- Assigned ", length(hisp_samp_rem), " HISPANIC ", race, " remaining individuals"))
        
      } ## end of else loop
      
      
      ## Calculate number assigned
      all_elg <- sum(dat$race %in% race_code & dat$age_cat == "0-17")
      all_hisp <- sum(dat$race %in% race_code & dat$age_cat == "0-17" & dat$eth == "H")
      
      ## 
      print(paste0("#### Census Hispanic Percent:   ", round(100 * (results[["0-17"]][["scale_h_pops"]][[race]]/results[["0-17"]][["pops"]][[race]]), 2), "% ####"))
      print(paste0("#### Assigned Hispanic Percent: ", round(100 * all_hisp / all_elg, 2), "% ####"))
      
      
      #### Adult assignment ##################

      # Age Cats 
      adult_age_cats <- c("0-17", "18-44", "45-64", "65p")
      
      for (a in 2:4) { 
        
        print(paste0("########## AGE ", adult_age_cats[a]))
        
        age_code <- age_map[[a]]
        
        # Check for any Hispanic people 
        if (sum(dat$race %in% race_code & dat$age_cat == adult_age_cats[a]) == 0 | results[[adult_age_cats[a]]][["pops"]][[race]] == 0) {
          
          ## No assignment!
          
        } else {
          
          ## Get indices of people in race/age category
          elg <- which(dat$race %in% race_code & dat$age_cat == adult_age_cats[a] & is.na(dat$gq_type))
          elg_gq <- which(dat$race %in% race_code & dat$age_cat == adult_age_cats[a] & !is.na(dat$gq_type))
          
          ### Random sample from Group Quarters population, if present
          if (rti_est_hisp_counts_age_race[[adult_age_cats[a]]][[race]][["rti_n_hisp_gq"]] > 0) {
            
            ## Sample
            hisp_samp_gq <- sample(elg_gq, size = rti_est_hisp_counts_age_race[[adult_age_cats[a]]][[race]][["rti_n_hisp_gq"]])
            
            ## Assign
            dat$eth[hisp_samp_gq] <- "H"
            
            ##
            print(paste0("- Assigned ", rti_est_hisp_counts_age_race[[adult_age_cats[a]]][[race]][["rti_n_hisp_gq"]], " HISPANIC ", race, " GQ individuals"))
            
          } else {
            
            print(paste0("- Assigned ", 0, " HISPANIC ", race, " GQ individuals"))
            
          }
          
          ##
          print(paste0("- Target: ", results[[adult_age_cats[a]]][["scale_h_pops"]][[race]], " HISPANIC ", race, " individuals"))
          
          # ## Get Households with Hispanic children
          # hisp_children_hh <- dat$sp_hh_id[which(dat$race %in% rti_race[[r]] & dat$eth_hh == 1)] %>% unique()
          # 
          ## Get hispanic adults in HH with hispanic children
          elg_hisp_adults_hh <- which(dat$race %in% race_code & dat$age_cat == adult_age_cats[a] & is.na(dat$gq_type) & dat$eth_hh == 1)
          
          ##
          print(paste0("  - Entire HH Count: ", length(elg_hisp_adults_hh), " HISPANIC ", race, " individuals"))
          
          ## If statement to deal with under and over
          ## First, UNDER
          if (length(elg_hisp_adults_hh) < results[[adult_age_cats[a]]][["scale_h_pops"]][[race]]) {
            
            ## Assign all adults in houses with Hispanic children to be Hispanic
            dat$eth[elg_hisp_adults_hh] <- "H"
            
            ##
            print(paste0("- Assigned ", length(elg_hisp_adults_hh), " HISPANIC ", race, " HH individuals"))
            
            ## Get number left to assign
            n_hisp_sing <- results[[adult_age_cats[a]]][["scale_h_pops"]][[race]] - length(elg_hisp_adults_hh)
            
            ## Get hispanic adults in houses without children
            elg_sing <- which(dat$race %in% race_code & 
                                dat$age_cat == adult_age_cats[a] & 
                                is.na(dat$gq_type) &
                                # !dat$sp_hh_id %in% hh_children$sp_hh_id &
                                dat$eth != "H") 
            
            ## Sample
            hisp_samp_sing <- sample(elg_sing, size = n_hisp_sing)
            
            ## Assign
            dat$eth[hisp_samp_sing] <- "H"
            
            ##
            print(paste0("- Assigned ", length(hisp_samp_sing), " HISPANIC ", race, " remaining individuals"))
            
            
            ## Next, OVER 
          } else {
            
            ## Sample
            hisp_samp_adults_child <- sample(elg_hisp_adults_hh, size = results[[adult_age_cats[a]]][["scale_h_pops"]][[race]])
            
            ## Assign
            dat$eth[hisp_samp_adults_child] <- "H"
            
            ##
            print(paste0("- Assigned ", length(hisp_samp_adults_child), " HISPANIC ", race, " HH individuals"))
            
          }
          
        } ## end of else loop
        
        
        ## Calculate number assigned
        all_elg <- sum(dat$race %in% race_code & dat$age_cat == adult_age_cats[a])
        all_hisp <- sum(dat$race %in% race_code & dat$age_cat == adult_age_cats[a] & dat$eth == "H")
        
        ## 
        # obj_name <- paste0("Updated_H_", age_code)
        # col2 <- paste0(race, "_PCT", age_code)
        print(paste0("#### Census Hispanic Percent:   ", round(100 * (results[[adult_age_cats[a]]][["scale_h_pops"]][[race]]/results[[adult_age_cats[a]]][["pops"]][[race]]), 2), "% ####"))
        print(paste0("#### Assigned Hispanic Percent: ", round(100 * all_hisp / all_elg, 2), "% ####"))
        
        
      } ## End of adult age loop
      
    } ## end of race loop
    
  # } ## end of age loop
  
  dat %>% group_by(age_cat, eth) %>% summarize(count = n())
  
  #pop <- hisp_tables[[1]]$P0017 + hisp_tables[[2]]$P0017 + hisp_tables[[3]]$P0017 + hisp_tables[[4]]$P0017 + hisp_tables[[5]]$P0017 + hisp_tables[[6]]$P0017 + hisp_tables[[7]]$P0017 
  #hpop <- hisp_tables[[1]]$H0017 + hisp_tables[[2]]$H0017 + hisp_tables[[3]]$H0017 + hisp_tables[[4]]$H0017 + hisp_tables[[5]]$H0017 + hisp_tables[[6]]$H0017 + hisp_tables[[7]]$H0017 
  
  ## Write out data here
  dat %>% filter(is.na(gq_type)) %>% write_csv(file = paste0("data/intermediate", counties[i], "_people.csv"))
  dat %>% filter(!is.na(gq_type)) %>% write_csv(file = paste0("data/intermediate", counties[i], "_gq.csv"))
  
  
} ## end of county loop
  
