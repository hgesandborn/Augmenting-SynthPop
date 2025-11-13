# =========================================================
# Title: Workplace assignment
# Author: Hilary Sandborn & Paul Delamater
# Date: 2025-10-24
# Description: Assign workplaces to agents in a synthetic population
# =========================================================


##### Run round_preserve_sum.R before running this script ######


##### Load packages #####
## If necessary, use install.packages() to install packages
library(tidyverse)
library(arrow)
library(magrittr)
library(sf)
library(data.table)
library(tidycensus)
library(tigris)


##### Set seed for reproducibility #####
set.seed(100)


##### Set file paths #####
data_path <- "data/input/"
intermediate_path <- "data/intermediate/"
output_path <- "output/results/"


##### Load data #####
## Update the file names as needed
people <- read_parquet(file.path(data_path, "NC_2019_persons.parquet"))
households <- read_parquet(file.path(data_path, "NC_2019_households.parquet"))
lodes <- read_csv(file.path(data_path, "nc_od_main_JT00_2019_v7.csv.gz"))
busi <- read_csv(file.path(data_path, "2020_Business_academic_QCQ_NC.csv"))
ref <- read_csv(file.path(data_path, "pctworking_by_age.csv")) ## A reference dataset based on the 2010 version of RTI's SynthPop


##### Process data #####

## 1. People & households

## Subset to working age adults
people %<>% filter(agep >= 18 & agep <= 94)

## Subset further to eligible adults only
elg_people <- people %>%
  group_by(agep) %>% 
  group_modify(~ {
    pct <- ref$AveragePercentWorking[ref$age == .y$agep] / 100
    n_sample <- floor(nrow(.x) * pct)
    .x[sample(nrow(.x), n_sample), ]
  }) %>%
  ungroup()

## Join people to households by hh_id
elg_people %<>% left_join(households %>% select(hh_id,
                                                bg_geoid,
                                                lon = lon_4326,
                                                lat = lat_4326),
                          by = "hh_id")


## 2. LODES OD

## Add columns for block group 
lodes %<>% mutate(w_bg = str_sub(w_geocode, 1, 12),
                  h_bg = str_sub(h_geocode, 1, 12))

## Group by work & home block group
lodes <- lodes %>%
  group_by(w_bg, h_bg) %>%
  summarize(total = sum(S000),
            total_lt29 = sum(SA01),
            total_30_54 = sum(SA02),
            total_gt55 = sum(SA03)) %>%
  select(w_bg, h_bg, total, total_lt29, total_30_54, total_gt55) %>%
  ungroup()


## 3. InfoUSA

## Add columns for block group and size
busi <- busi %>% mutate(Tract = str_pad(as.character(Tract), width = 6, side = "left", pad = "0"),
                        w_bg = paste0(as.character(FIPS), Tract, as.character(Block)),
                        busi_id = row_number(),
                        size = case_when(
                          Employee_Size_Location >= 1 & Employee_Size_Location <= 4 ~ "1-4",
                          Employee_Size_Location >= 5 & Employee_Size_Location <= 24 ~ "5-24",
                          Employee_Size_Location >= 25 & Employee_Size_Location <= 99 ~ "25-99",
                          Employee_Size_Location >= 100 & Employee_Size_Location <= 499 ~ "100-499",
                          Employee_Size_Location >= 500 & Employee_Size_Location <= 4999 ~ "500-4,999",
                          Employee_Size_Location >= 5000 ~ "5,000+",
                          is.na(Employee_Size_Location) ~ "Not reported"),
                        max_capacity = recode(size,
                                              "1-4" = 4L,
                                              "5-24" = 24L,
                                              "25-99" = 99L,
                                              "100-499" = 499L,
                                              "500-4,999" = 4999L,
                                              "5,000+" = 10000L,
                                              .default = 99999L)) %>%
  select(busi_id, Primary_SIC_Code, Primary_NAICS_Code, Employee_Size_Location, w_bg, size, max_capacity, Latitude, Longitude)

## Group by NAICS code & calculate median employee size by NAICS code
busi <- busi %>%
  group_by(Primary_NAICS_Code) %>%
  mutate(median = round(median(Employee_Size_Location, na.rm = TRUE))) %>%
  ungroup()

## Calculate overall median employee size   
overall_median <- median(busi$Employee_Size_Location, na.rm = TRUE)

## Apply median employee size to businesses with no reported size
for (k in unique(busi$Primary_NAICS_Code)) {
  
  subset_rows <- busi %>% filter(Primary_NAICS_Code == k)
  
  group_indices <- which(busi$Primary_NAICS_Code == k)
  
  ## For a NAICS code w/ both no size busis and sized busis...
  if (any(is.na(subset_rows$Employee_Size_Location)) & any(subset_rows$Employee_Size_Location > 0, na.rm = TRUE)) {
    
    ## Count number of no size busis
    count <- sum(is.na(subset_rows$Employee_Size_Location))
    
    na_rows_in_subset <- which(is.na(subset_rows$Employee_Size_Location))
    
    na_rows_in_busi <- group_indices[na_rows_in_subset]
    
    ## Apply the NAICS code median to no size busis
    busi$Employee_Size_Location[na_rows_in_busi] <- busi$median[na_rows_in_busi]
    
    print(paste0("Fixed ", count, " NA rows in NAICS code ", k, " using group median"))
    
    ## If all busis in NAICS code have no size...
  } else if (all(is.na(subset_rows$Employee_Size_Location))) {
    
    count <- sum(is.na(subset_rows$Employee_Size_Location))
    
    na_rows_in_subset <- which(is.na(subset_rows$Employee_Size_Location))
    
    na_rows_in_busi <- group_indices[na_rows_in_subset]
    
    ## Apply the overall median to busis
    busi$Employee_Size_Location[na_rows_in_busi] <- overall_median
    
    print(paste0("Fixed ", count, " NA rows in NAICS code ", k, " using overall median"))
  }
  
  else { ## If there are no NA rows for that NAICS code
    print(paste0("No NA rows in NAICS code ", k))
    next
  }
  
}



##### Assign workers to block group (bg) of workplace ######

## Precompute age group
elg_people %<>% mutate(age_grp = cut(agep,
                                     breaks = c(-Inf, 29, 54, Inf),
                                     labels = c("total_lt29", "total_30_54", "total_gt55"),
                                     right = TRUE))


## Get counts of combos of BG / Age group
combos <- elg_people %>%
  group_by(bg_geoid, age_grp) %>%
  summarize(count = n()) %>%
  ungroup()

## Track time
start_time <- Sys.time()

## Loop over combos
for (j in 1:nrow(combos)) {
  
  ## Get subset of lodes table, sort descending
  lodes_bg <- lodes %>% filter(h_bg == combos$bg_geoid[j]) %>%
    select(w_bg, h_bg, !!sym(as.character(combos$age_grp[j]))) %>%
    arrange(desc(!!sym(as.character(combos$age_grp[j]))))
  
  ## Get sum of workers in lodes
  lodes_sum <- sum(lodes_bg %>% select(!!sym(as.character(combos$age_grp[j]))))
  
  ## Check if there are 0 workers in LODES for combo
  ## If so, use proportion to all BG for total
  if (lodes_sum == 0) {
    
    ## Get all age groups from lodes data
    lodes_bg_all <- lodes %>% filter(h_bg == combos$bg_geoid[j]) %>%
      select(w_bg, h_bg, total) %>%
      arrange(desc(total))
    
    ## Get sum of workers in lodes
    lodes_sum <- sum(lodes_bg_all %>% select(total))
    
    ## Replace values in lodes table
    lodes_bg[,3] = lodes_bg_all$total
    
  }
  
  ## Adjust counts from lodes to reflect count from population
  ## Round (preserve sum)
  lodes_bg %<>% mutate(adj_total = !!sym(as.character(combos$age_grp[j])) * combos$count[j] / lodes_sum,
                       total_r = round_preserve_sum(adj_total))
  
  ## Get person ID of eligible people
  elg_people_id <- elg_people %>% filter(bg_geoid == combos$bg_geoid[j] & age_grp == as.character(combos$age_grp[j])) %>%
    select(person_id)
  
  ## Explode summary od table
  lodes_bg_long <- lodes_bg %>% select(w_bg, total_r) %>% uncount(total_r)
  
  ## Randomize
  lodes_bg_long %<>% mutate(w_bg = sample(w_bg))
  
  ## Bind to ids
  elg_people_id %<>% bind_cols(lodes_bg_long)
  
  ## Put in holder for state
  if (j == 1) {
    
    nc_elg_people_id <- elg_people_id
    
  } else {
    
    nc_elg_people_id %<>% bind_rows(elg_people_id)
    
  }
  
  print(paste0("Finished combo ", j, " of ", nrow(combos), " at ", Sys.time()-start_time))
  
}

Sys.time() - start_time 

## Intermediate save
write.csv(nc_elg_people_id, file.path(intermediate_path, "nc_elg_people_id.csv"))



###### Assign workers to workplace based on capacity ######

## Track time
start_time <- Sys.time()

## Create table for storing output
working_people <- tibble()

## Block groups
block_groups <- intersect(unique(nc_elg_people_id$w_bg), unique(busi$w_bg))

## For counting the bg number that the loop is on
i <- 0

## For counting the number of busis that are over max capacity and can't be fixed
unfixed <- 0

## Start loop over block groups
for (bg in block_groups) {
  
  ## Update loop block group number
  i <- i + 1
  
  ## busi_id to start from if we need to create new busis
  busi_starting_num = 482626
  
  ## People in that bg
  people_bg <- nc_elg_people_id %>% filter(w_bg == bg)
  
  ## Busis in that bg
  busi_bg <- busi %>% filter(w_bg == bg)
  
  ## Total people in that bg
  n_people <- nrow(people_bg)
  
  ## If there are no people to assign, go to the next bg
  if (n_people == 0) {
    next 
  }
  
  ## If there are people but no businesses, create one business
  if (n_people > 0 & nrow(busi_bg) == 0) {
    
    ## Add one busi to busi_bg 
    busi_bg <- tibble(
      busi_id = busi_starting_num + 1,
      Employee_Size_Location = n_people,
      w_bg = bg
    )
    
    ## Update busi_id number for the next one
    busi_starting_num <- busi_starting_num + 1
    
    print(paste0("Created a business for ", bg, " with ", n_people, " people"))
  }
  
  
  ## Check total busi capacity for that bg
  curr_capacity <- sum(busi_bg$Employee_Size_Location, na.rm = TRUE)
  
  ## If there are less busi slots than people needing assignment then...
  if (curr_capacity < n_people) {
    
    ## Count the difference between people and busi slots
    shortfall <- n_people - curr_capacity
    
    ## Add columns for proportion of employees and extra capacity
    busi_bg <- busi_bg %>%
      mutate(
        prop = Employee_Size_Location / curr_capacity,
        extra_capacity = round_preserve_sum(prop * shortfall)
      )
    
    ## Update employee size using extra capacity
    busi_bg$Employee_Size_Location <- busi_bg$Employee_Size_Location + busi_bg$extra_capacity
    
    
    ## Check if updated employee size is now over max capacity
    ## For each busi in busi_bg...
    for(b in 1:nrow(busi_bg)) {
      
      ## If employee size is greater than max capacity...
      if(busi_bg$Employee_Size_Location[b] > busi_bg$max_capacity[b]) {
        
        ## Calculate the difference
        over <- busi_bg$Employee_Size_Location[b] - busi_bg$max_capacity[b]
        
        ## Find all other busis that can take more people
        available_busis <- which(busi_bg$Employee_Size_Location < busi_bg$max_capacity)
        
        ## If there are no other busis that can take people, count this and continue to next busi
        if(length(available_busis) == 0) {
          unfixed <- unfixed + 1
          next
        }
        
        ## If there are available busis...
        ## Count the number of available slots
        slot_counts <- busi_bg$max_capacity[available_busis] - busi_bg$Employee_Size_Location[available_busis]
        
        ## Grab the indices of the available slots
        available_slots <- rep(available_busis, slot_counts)
        
        ## Compare over with available_slots
        assignable <- min(over, length(available_slots))
        
        ## Randomly sample the number of assignable from available_slots
        selected_busi <- sample(available_slots, assignable, replace = FALSE)
        
        ## Count how many people are being added to each selected busi
        slot_table <- table(selected_busi)
        
        ## For each busi in slot_table...
        for (sb in names(slot_table)) {
          
          ## Update the employee size by adding people
          busi_bg$Employee_Size_Location[as.numeric(sb)] <- 
            busi_bg$Employee_Size_Location[as.numeric(sb)] + slot_table[[sb]]
        }
        
        ## Update the employee size of busi by subtracting 
        busi_bg$Employee_Size_Location[b] <- 
          busi_bg$Employee_Size_Location[b] - assignable
      }
    }
  }
  
  ## Grab the indices of the available busi slots
  business_slots <- rep(busi_bg$busi_id, busi_bg$Employee_Size_Location)
  
  ## Number of people to assign 
  assign_count <- min(n_people, length(business_slots)) 
  
  ## Randomly sample the number of assignable people from busi slots
  assigned_busi_ids <- sample(business_slots, assign_count, replace = FALSE)
  
  ## Updated the people table with their assigned busi_id
  people_bg <- people_bg %>%
    mutate(busi_id = assigned_busi_ids)
  
  ## Append all bg people rows together
  working_people <- bind_rows(working_people, people_bg %>% select(person_id, w_bg, busi_id))
  
  print(paste0("Finished bg ", bg, " (", i, " of ", length(block_groups), ") at ", Sys.time()-start_time))
  
}

## See how much time it took
Sys.time() - start_time 

## Save
write.csv(working_people, file.path(output_path, "workplace_assignments.csv"))