# =========================================================
# Title: Grade & school assignment
# Author: Hilary Sandborn & Paul Delamater
# Date: 2025-10-24
# Description: Assign grade level and schools to agents in a synthetic population
# =========================================================


##### Run round_preserve_sum.R before running this script ######


##### Load packages #####
## If necessary, use install.packages() to install packages
library(arrow)
library(sf)
sf::sf_use_s2(FALSE) ## Switch spherical geometry (s2) off
library(tidyverse)
library(tidycensus)
library(magrittr)


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
sch_districts <- read_sf(file.path(data_path, "EDGE_SCHOOLDISTRICT_TL20_SY1920.shp"))
pub_sch_grade <- read_csv(file.path(data_path, "ELSI_csv_export_6388653429958268928527.csv"), 
                          skip = 6) ## Charter school data is contained within the public school data
pub_sch_coord <- read_sf(file.path(data_path,"EDGE_GEOCODE_PUBLICSCH_1920.shp")) ## Charter school data is contained within the public school data
priv_sch_grade <- read_csv(file.path(data_path, "ELSI_csv_export_638857624373479683583.csv"),
                       skip = 6)
priv_sch_coord <- read_sf(file.path(data_path, "EDGE_GEOCODE_PRIVATESCH_1920.shp"))
home_sch_enr <- read.csv(file.path(data_path, "homeschooled.csv"))
home_grade_prop <- read.csv(file.path(data_path, "home_grade_prop.csv"))
cty_nc <- counties(state = "NC") ## Use tidycensus to get county data for the state



##### Process data #####

## 1. Download & process PUMS data

## Load Census API key
## Sign up - https://api.census.gov/data/key_signup.html
census_api_key("415ce5b0cd6028be4e78fe0e662e0e3a10c7cdb8")

## Download data of interest
## Update parameters as needed 
pums_nc_2021 <- get_pums(
  variables = c("SCH", "SCHG", "AGEP"),
  state = "NC",
  year = 2021,
  survey = "acs1",
  recode = FALSE
)

## Subset to school-aged kids only
pums_kids_4_17 <- pums_nc_2021 %>%
  filter(AGEP >= 4 & AGEP <= 17)

## Find % in each grade by age
grade_percent_by_age <- pums_kids_4_17 %>%
  filter(SCH != "1") %>%                      # Remove kids not in school
  group_by(AGEP, SCHG) %>%                    # Summarize by age / grade
  summarize(total_weight = sum(PWGTP), 
            .groups = "drop") %>%
  group_by(AGEP) %>%
  mutate(prop = total_weight / sum(total_weight)) %>%
  ungroup() %>%
  select(AGEP, SCHG, prop)

## Create wide version of grade table
grades_wide <- grade_percent_by_age %>%
  pivot_wider(id_cols = AGEP, 
              names_from = SCHG,
              names_prefix = "G",
              values_from = prop,
              values_fill = 0)


## 2. People & households

## Subset to 4-17 year olds
people %<>% filter(agep >= 4 & agep <= 17)

## Join people to households by hh_id
people %<>% left_join(households %>% select(hh_id,
                                            bg_geoid,
                                            lon = lon_4326,
                                            lat = lat_4326),
                      by = "hh_id")

## Make people spatial
people %<>% st_as_sf(coords = c("lon", "lat"), 
                     crs = 4326)

## Spatial join people to school districts
## Takes a while to run so be patient
people %<>% st_join(sd_nc %>% 
                      select(GEOID) %>% 
                      st_transform(crs = st_crs(people)),
                    join = st_nearest_feature)

## Add column for grade
people %<>% mutate(grade = 0,
                   sch_id = 0,
                   FIPS = str_sub(bg_geoid, 1, 5))


## Loop to do grade assignment
for (y in 4:17) {
  
  ## Get row numbers of kids for age
  age_rows <- which(people$agep == y)
  
  ## Sample is the length of age rows
  grade_assign <- sample(x = 1:15,
                         size = length(age_rows),
                         prob = grades_wide %>%
                           slice(y - 3) %>%
                           select(G01:G15) %>%
                           as.numeric(),
                         replace = TRUE)
  
  ## Replace values in grade column with sampled values
  people$grade[age_rows] <- grade_assign
  
}

## Recode grades 
## Subtract 2 to set PK = -1, K = 0, 1 = 1, and so on
people %<>% mutate(grade = grade - 2)

## Set to NA for people above 12th grade
people %<>% mutate(grade = na_if(grade, 13))


## 3. School districts 

## Filter to state of interest
sd_nc <- sch_districts %>% 
  filter(STATEFP == 37)

## Subset to non military / reservation schools
sd_nc %<>% filter(is.na(SDTYP))

## Join to county data to create crosswalk table of district to county
sd_cty_nc <- sd_nc %>%
  st_centroid() %>%
  select(GEOID) %>%
  st_join(cty_nc %>% 
            st_transform(crs = st_crs(sd_nc)) %>%
            select(FIPS = GEOID)) %>%
  st_drop_geometry()


## 4. Public & charter schools

## Rename columns for clarity
pub_sch_grade %<>% rename(ST = `State Abbr [Public School] Latest available year`)

## Subset both datasets to NC
pub_sch_grade_nc <- pub_sch_grade %>%
  filter(ST == "NC")
pub_sch_coord_nc <- pub_sch_coord %<>% 
  filter(STATE == "NC")

## Fix all names for clarity
names(pub_sch_grade_nc) <- c("NAME", "STNAME", "ST", "NAME2", "NCESSCH7", "NCESSCH",
                         "COUNTY", "FIPS", "PK_ENR", "K_ENR", 
                         paste0("G", 1:13, "_ENR"), "UG_ENR", "AE_ENR")

## Fix types
pub_sch_coord_nc %<>% mutate(NCESSCH = as.numeric(NCESSCH))
pub_sch_grade_nc %<>% mutate(NCESSCH = as.numeric(NCESSCH))

## Join grade & coordinate data based on 12-digit school ID
pub_sch <- left_join(pub_sch_grade_nc, 
                 pub_sch_coord_nc %>% select(NCESSCH, LEAID, NAME3 = NAME, LAT, LON), 
                 by = "NCESSCH")

## Fix some of the formatting
pub_sch %<>% mutate(across(PK_ENR:AE_ENR, ~ as.numeric(.)))
pub_sch %<>% mutate(across(PK_ENR:AE_ENR, ~ replace_na(., 0)))

## Determine Public vs Charter school status
pub_sch %<>% mutate(TYPE = case_when(LEAID %in% sd_nc$GEOID ~ "Public",
                                 .default = "Charter"))

## Write out cleaned up file for use in later distance calculations
pub_sch %>% st_drop_geometry() %>% write_csv(file.path(intermediate_path, "public_schools_enr_coords_clean.csv"))
pub_sch %>% write_sf(file.path(intermediate_path, "public_schools_enr_coords_clean.gpkg"))


## 5. Private schools

## Subset to NC
priv_sch_coord_nc <- subset(priv_sch_coord, STATE == "NC")
priv_sch_grade_nc <- priv_sch_grade %>%
  filter(STNAME == "NORTH CAROLINA")

## Rename columns for clarity
priv_sch_grade_nc %<>% rename(STNAME = `State Name [Private School] 2019-20`)


## Fix all names for clarity
names(priv_sch_grade_nc) <- c("NAME", "STNAME", "STNAME2", "ST", "NAME2",
                          "NCESSCH7", "COUNTY", "FIPS", "HIGH_GRADE", "LOW_GRADE",
                          "TYPE", "ENR")

## Join grade and coordinate data based on the 7-digit ID
priv_sch <- left_join(priv_sch_coord_nc %>% select(PPIN, NMCNTY, NAME3 = NAME, LAT, LON), 
                  priv_sch_grade_nc,
                  by = c("PPIN" = "NCESSCH7"))

## Join to school districts for later calculations
priv_sch %<>% st_join(sd_nc %>% st_transform(crs = st_crs(priv_sch)) %>%
                    select(GEOID),
                  join = st_nearest_feature)

## Write out cleaned up file for use in later distance calculations
priv_sch %>% write_sf(file.path(intermediate_path, "private_schools_enr_coords_clean.gpkg"))
priv_sch %>% st_drop_geometry %>% write_csv(file.path(intermediate_path, "private_schools_enr_coords_clean.csv"))


## 6. Homeschools

## Fix formatting
home_sch %<>% mutate(Enrollment = as.numeric(str_remove_all(Enrollment, ",")))
home_sch %<>% filter(!is.na(FIPS))


##### Determine Students and distribution by school district #####

## 1. Overall Population
sd_pop_gr <- people %>% st_drop_geometry %>%
  group_by(GEOID, grade) %>%
  summarize(POP = n())

sd_pop <- people %>% st_drop_geometry %>%
  group_by(GEOID) %>%
  summarize(POP = n())

cty_pop_gr <- people %>% st_drop_geometry %>%
  group_by(FIPS, grade) %>%
  summarize(POP = n())

cty_pop <- people %>% st_drop_geometry %>%
  group_by(FIPS) %>%
  summarize(POP = n())

## 2. Public schools
sd_pub_gr <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Public") %>%
  select(LEAID, PK_ENR:G13_ENR) %>%
  group_by(LEAID) %>%
  summarize_all(sum)

sd_pub <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Public") %>%
  mutate(ENR = rowSums(across(PK_ENR:G13_ENR))) %>%
  select(LEAID, ENR) %>%
  group_by(LEAID) %>%
  summarize_all(sum)

cty_pub_gr <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Public") %>%
  select(FIPS, PK_ENR:G13_ENR) %>%
  group_by(FIPS) %>%
  summarize_all(sum)

cty_pub <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Public") %>%
  mutate(ENR = rowSums(across(PK_ENR:G13_ENR))) %>%
  select(FIPS, ENR) %>%
  group_by(FIPS) %>%
  summarize_all(sum)

## 3. Charter schools
sd_ch_gr <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Charter") %>%
  select(LEAID, PK_ENR:G13_ENR) %>%
  group_by(LEAID) %>%
  summarize_all(sum)

sd_ch <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Charter") %>%
  mutate(ENR = rowSums(across(PK_ENR:G13_ENR))) %>%
  select(LEAID, ENR) %>%
  group_by(LEAID) %>%
  summarize_all(sum)

cty_ch_gr <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Charter") %>%
  select(FIPS, PK_ENR:G13_ENR) %>%
  group_by(FIPS) %>%
  summarize_all(sum)

cty_ch <- pub_sch %>% st_drop_geometry() %>%
  filter(TYPE == "Charter") %>%
  mutate(ENR = rowSums(across(PK_ENR:G13_ENR))) %>%
  select(FIPS, ENR) %>%
  group_by(FIPS) %>%
  summarize_all(sum)

## 4. Private schools
sd_pri <- priv_sch %>% st_drop_geometry() %>%
  select(GEOID, ENR) %>%
  group_by(GEOID) %>%
  summarize_all(sum)

cty_pri <- priv_sch %>% st_drop_geometry() %>%
  select(FIPS, ENR) %>%
  group_by(FIPS) %>%
  summarize_all(sum)


## Join totals for each school type by COUNTY
cty_summary <- cty_pop %>%
  mutate(FIPS = as.numeric(FIPS)) %>% 
  left_join(cty_pub %>% rename(PUB = ENR),
            by = "FIPS") %>%
  left_join(cty_ch %>% rename(CHAR = ENR),
            by = "FIPS") %>%
  left_join(cty_pri %>% rename(PRI = ENR),
            by = "FIPS") %>%
  left_join(home_sch %>% select(FIPS, HOME = Enrollment),
            by = "FIPS")

## Replace NA with true 0
cty_summary %<>% mutate(CHAR = replace_na(CHAR, 0),
                        PRI = replace_na(PRI, 0))

## Get sum of enrollment, proportions
cty_summary %<>% mutate(SUMENR = PUB + CHAR + PRI + HOME,
                        PUBPR = PUB / SUMENR,
                        CHARPR = CHAR / SUMENR,
                        PRIPR = PRI / SUMENR,
                        HOMEPR = HOME / SUMENR)

sum(cty_summary$HOME) / sum(cty_summary$SUMENR)

## Calculate population estimates
cty_summary %<>% mutate(PUBPOP = POP * PUBPR,
                        CHARPOP = POP * CHARPR,
                        PRIPOP = POP * PRIPR,
                        HOMEPOP = POP * HOMEPR)

## Round values
for (i in 1:nrow(cty_summary)) {
  
  ## Replace values manually
  cty_summary[i,12:15] <- round_preserve_sum(cty_summary[i,12:15], 0)
  
}

## Calculate Ratios for Public, Private, Charter Schools for each county
cty_summary %<>% mutate(POPENR_R = POP / SUMENR)



##### Homeschool assignment #####

## Convert prop to wide
home_grade_prop_w <- home_grade_prop %>% pivot_wider(names_from = GRADE,
                                                     values_from = PROP,
                                                     names_prefix = "G")

## Attach updated homeschool estimates
home_sch_enr %<>% left_join(cty_summary %>% select(FIPS, HOMEPOP),
                                             by = "FIPS")

## Make columns for homeschool estimates
home_sch_enr %<>% mutate(HS_G1 = HOMEPOP * home_grade_prop_w$G1,
                                          HS_G2 = HOMEPOP * home_grade_prop_w$G2,
                                          HS_G3 = HOMEPOP * home_grade_prop_w$G3,
                                          HS_G4 = HOMEPOP * home_grade_prop_w$G4,
                                          HS_G5 = HOMEPOP * home_grade_prop_w$G5,
                                          HS_G6 = HOMEPOP * home_grade_prop_w$G6,
                                          HS_G7 = HOMEPOP * home_grade_prop_w$G7,
                                          HS_G8 = HOMEPOP * home_grade_prop_w$G8,
                                          HS_G9 = HOMEPOP * home_grade_prop_w$G9,
                                          HS_G10 = HOMEPOP * home_grade_prop_w$G10,
                                          HS_G11 = HOMEPOP * home_grade_prop_w$G11,
                                          HS_G12 = HOMEPOP * home_grade_prop_w$G12)


### Round values
for (i in 1:nrow(home_sch_enr)) {
  
  ## Replace values manually
  home_sch_enr[i,5:16] <- round_preserve_sum(home_sch_enr[i,5:16], 0)
  
}

## Assign Loop
## Loop through each county
for (i in 1:nrow(home_sch_enr)) {
  
  ## Loop through each grade
  for (g in 1:12) {
    
    ## Get positions of county/grade combos
    hs_cty_gr <- which(people$FIPS == home_sch_enr$FIPS[i] & people$grade == g)
    
    ## Sample HS students
    hs_cty_gr <- sample(x = hs_cty_gr, 
                        size = home_sch_enr[i, g+4])
    
    ## Assign HS flag in data (-99)
    people$sch_id[hs_cty_gr] <- -99
    
  }
  
}


##### Private school assignment #####

## Recode high/low grade in priv_sch
grade_map <- c(
  "Transitional Kindergarten" = -1,
  "Prekindergarten" = -1,
  "Kindergarten" = 0,
  "Transitional 1st grade" = 1,
  "1st grade" = 1,
  "2nd grade" = 2,
  "3rd grade" = 3,
  "4th grade" = 4,
  "5th grade" = 5,
  "6th grade" = 6,
  "7th grade" = 7,
  "8th grade" = 8,
  "9th grade" = 9,
  "10th grade" = 10,
  "11th grade" = 11,
  "12th grade" = 12)

priv_sch$HIGH_GRADE_NUM <- grade_map[priv_sch$HIGH_GRADE]
priv_sch$LOW_GRADE_NUM <- grade_map[priv_sch$LOW_GRADE]

## Handle "All ungraded"
all_ungraded_rows <- priv_sch$LOW_GRADE == "All Ungraded" & priv_sch$HIGH_GRADE == "All Ungraded"
priv_sch$LOW_GRADE_NUM[all_ungraded_rows]  <- -1
priv_sch$HIGH_GRADE_NUM[all_ungraded_rows] <- 12

## Join County Ratio of Pop to Enrollment to adjust enrollment
priv_sch %<>% left_join(cty_summary %>% select(FIPS, POPENR_R),
                    by = "FIPS")

## Timer
start <- Sys.time()

## Loop by school
## Takes ~ 1 hr 15 min
for (k in 1:nrow(priv_sch)) {
  
  ## School 
  school <- priv_sch[k, ]
  
  ## Make priv_sch spatial
  school %<>% st_as_sf(coords = c("LON", "LAT"), 
                       crs = 4326)
  
  ## Accepted grades
  accepted_grades <- school$LOW_GRADE_NUM:school$HIGH_GRADE_NUM
  
  ## Filter to people not assigned homeschool or private school already, and grade
  eligible_people <- people %>% filter(sch_id == 0 & grade %in% accepted_grades)
  
  ## Distance from school to eligible people
  dist <- st_distance(school, eligible_people)
  
  ## Convert to tibble and long format
  eligible_people <- eligible_people %>%
    mutate(DIST_KM = as.numeric(dist / 1000),
           buffer = case_when(DIST_KM < 10 ~ 1,
                              DIST_KM >= 10 & DIST_KM < 15 ~ 2,
                              DIST_KM >= 15 & DIST_KM < 20 ~ 3,
                              .default = NA))
  
  ## Expected counts
  buffer_counts <- round_preserve_sum(c(school$ENR * school$POPENR_R * 0.5,
                                        school$ENR * school$POPENR_R * 0.25,
                                        school$ENR * school$POPENR_R * 0.25))
  
  ## Sample and assign 
  buffer1_sample <- sample(which(eligible_people$buffer == 1),
                           size = buffer_counts[1], 
                           replace = FALSE)
  ## Assign
  eligible_people$sch_id[buffer1_sample] <- school$PPIN 
  
  ## Sample Buffer 2
  buffer2_sample <- sample(which(eligible_people$buffer == 2), 
                           size = buffer_counts[2], 
                           replace = FALSE)
  ## Assign
  eligible_people$sch_id[buffer2_sample] <- school$PPIN 
  
  ## Sample Buffer 3
  buffer3_sample <- sample(which(eligible_people$buffer == 3), 
                           size = buffer_counts[3], 
                           replace = FALSE)
  ## Assign
  eligible_people$sch_id[buffer3_sample] <- school$PPIN
  
  ## Subset to people assigned a school
  eligible_people %<>% filter(sch_id == school$PPIN)
  
  ## Update people file
  matched_idx <- match(eligible_people$person_id, people$person_id)
  people$sch_id[matched_idx] <- eligible_people$sch_id

  ## Print message
  print(paste0("Finished School ", priv_sch$NAME[k], 
               " ", k, " of ", nrow(priv_sch), " at ", Sys.time()-start))
}



##### Charter school assignment #####

## Subset Charter Schools from public schools
char_sch <- pub_sch %>% filter(TYPE == "Charter")

## Join County Ratio of Pop to Enrollment to adjust enr
char_sch%<>% left_join(cty_summary %>% select(FIPS, POPENR_R),
                        by = "FIPS")

grade_cols <- c("PK_ENR", "K_ENR", paste0("G", 1:13, "_ENR"))
grades <- c("PK", "K", as.character(1:13))

## Timer
start <- Sys.time()

## Loop by school
## Takes ~ 1 hr 15 min 
for (k in 1:nrow(sch_char)) {
  
  ## School 
  school <- char_sch[k, ]
  
  ## Make sch_char spatial
  school %<>% st_as_sf(coords = c("LON", "LAT"), 
                       crs = 4326)
  
  ## Extract only the enrollment columns as numeric
  school_enr <- as.numeric(school[grade_cols])
  
  ## Find indices of non-zero enrollment
  nonzero_idx <- which(school_enr > 0)
  
  if (length(nonzero_idx) != 0) {
    ## Determine lowest and highest grades
    low_grade  <- grades[min(nonzero_idx)]
    high_grade <- grades[max(nonzero_idx)]
    
    ## Accepted grades vector
    accepted_grades <- grades[min(nonzero_idx):max(nonzero_idx)]
    
    ## Filter to people not assigned homeschool or private school already, and grade
    eligible_people <- people %>% filter(sch_id == 0 & grade %in% accepted_grades)
    
    ## Distance from school to eligible people
    dist <- st_distance(school, eligible_people)
    
    ## Convert to tibble and long format
    eligible_people <- eligible_people %>%
      mutate(DIST_KM = as.numeric(dist / 1000),
             buffer = case_when(DIST_KM < 10 ~ 1,
                                DIST_KM >= 10 & DIST_KM < 15 ~ 2,
                                DIST_KM >= 15 & DIST_KM < 20 ~ 3,
                                .default = NA))
    
    ## Total enrollment
    school$ENR <- sum(as.numeric(school[grade_cols]), na.rm = TRUE)
    
    ## Expected counts
    buffer_counts <- round_preserve_sum(c(school$ENR * school$POPENR_R * 0.5,
                                          school$ENR * school$POPENR_R * 0.25,
                                          school$ENR * school$POPENR_R * 0.25))
    
    ## Sample and assign 
    buffer1_sample <- sample(which(eligible_people$buffer == 1),
                             size = buffer_counts[1], 
                             replace = FALSE)
    ## Assign
    eligible_people$sch_id[buffer1_sample] <- school$NCESSCH7
    
    ## Sample Buffer 2
    buffer2_sample <- sample(which(eligible_people$buffer == 2), 
                             size = buffer_counts[2], 
                             replace = FALSE)
    ## Assign
    eligible_people$sch_id[buffer2_sample] <- school$NCESSCH7
    
    ## Sample Buffer 3
    buffer3_sample <- sample(which(eligible_people$buffer == 3), 
                             size = buffer_counts[3], 
                             replace = FALSE)
    ## Assign
    eligible_people$sch_id[buffer3_sample] <- school$NCESSCH7
    
    ## Subset to people assigned a school
    eligible_people %<>% filter(sch_id == school$NCESSCH7)
    
    ## Update people file
    matched_idx <- match(eligible_people$person_id, people$person_id)
    people$sch_id[matched_idx] <- eligible_people$sch_id

    ## Print message
    print(paste0("Finished School ", char_sch$NAME[k], 
                 " ", k, " of ", nrow(char_sch), " at ", Sys.time()-start))
  } else {
    
    ## Print message
    print(paste0("Finished School ", char_sch$NAME[k], 
                 " ", k, " of ", nrow(char_sch), " at ", Sys.time()-start))
    print("No capacity for previous school.")
  }
}



##### Public school assignment #####

## Create holder for all data
people_sch_all <- NULL

## Start timer
## Takes ~3 hours
start_time <- Sys.time()

## Loop by school district
for (i in 1:nrow(sd_nc)) {
  
  ## Locate people in this district
  people_sd <- people %>% filter(GEOID == sd_nc$GEOID[i] & sch_id == 0)
  
  ## Locate schools in this district
  sch_sd <- sch %>% filter(LEAID == sd_nc$GEOID[i])
  
  ## Make empty holder
  people_sch_sd <- NULL
  
  ## Loop through grades
  for (g in 0:12) { 
    
    ## Subset students to grade
    people_sd_gr <- people_sd %>% filter(grade == g)
    
    ## Subset to schools with this grade
    sch_sd_gr <- sch_sd[which(sch_sd[,g+10] > 0),]
    
    ## If no schools, next
    if (nrow(sch_sd_gr) == 0) next
    
    ## Get distances from kids to schools
    dist <- st_distance(people_sd_gr, 
                        sch_sd_gr %>% st_as_sf(coords = c("LON", "LAT"), 
                                               crs = 4326))
    
    ## Name columns
    colnames(dist) <- sch_sd_gr$NCESSCH
    
    ## Make into a tibble and long list
    dist <- bind_cols(people_sd_gr %>% 
                        st_drop_geometry() %>%
                        select(person_id),
                      dist)
    
    ## Now long list
    dist_l <- dist %>% pivot_longer(cols = !person_id,
                                    names_to = "NCESSCH",
                                    values_to = "DIST_M")
    
    ## Sort 
    dist_l %<>% arrange(DIST_M)
    
    ## Make a capacity table that can be updated
    sch_grade_nc_cap <- sch_sd_gr %>% select(NCESSCH, g+10)
    
    ## Update name for easier coding
    names(sch_grade_nc_cap)[2] <- "ENR"
    
    ## Bump up capacity to account for overassignment
    if (sum(sch_grade_nc_cap$ENR) < nrow(people_sd_gr)) {
      
      ## Multiply by ratio
      sch_grade_nc_cap %<>% mutate(ENR = (ENR * nrow(people_sd_gr) / sum(sch_grade_nc_cap$ENR)) %>% ceiling())
      
    }
    
    ## Make empty holder
    people_sch <- NULL
    
    ## Start "Do-ing" assignment by child
    while (nrow(dist_l) > 0) {
      
      ## Assign the nearest
      people_sch %<>% bind_rows(dist_l %>% slice(1))
      
      ## Subtract from capacity
      sch_grade_nc_cap$ENR[sch_grade_nc_cap$NCESSCH == dist_l$NCESSCH[1]] <- 
        sch_grade_nc_cap$ENR[sch_grade_nc_cap$NCESSCH == dist_l$NCESSCH[1]] - 1
      
      ## Remove school from potential student records if full
      if (sch_grade_nc_cap$ENR[sch_grade_nc_cap$NCESSCH == dist_l$NCESSCH[1]] == 0) {
        dist_l %<>% filter(NCESSCH != dist_l$NCESSCH[1])
      }
      
      ## Remove rest of student records
      dist_l %<>% filter(person_id != dist_l$person_id[1])
    }
    
    people_sch_sd %<>% bind_rows(people_sch)
  }
  
  people_sch_all %<>% bind_rows(people_sch_sd)
  
  
  ## Print message
  print(paste0("Finished LEA ", sd_nc$GEOID[i], 
               " (", i, " of ", nrow(sd_nc), " LEAs) with ", 
               nrow(people_sd), " students at ", Sys.time()-start_time))
}


##### Save outputs #####

## Update the people file
people <- people_copy %>%
  left_join(people_sch_all %>% select(person_id, NCESSCH), by = "person_id") %>%
  mutate(sch_id = if_else(sch_id == 0, NCESSCH, sch_id)) %>%
  select(-NCESSCH)


## Save people file
write.csv(people, file.path(ouput_path, "school_assignments.csv"))