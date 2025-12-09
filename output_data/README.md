** Please note this is a draft repository being prepared for a manuscript - it is not finished! **

# Augmenting-SynthPop/output_data
Contains the output data files produced by the methods in "Augmenting an individual-level synthetic population dataset for population health applications" (Sandborn &amp; Delamater, 2026).

**Full citation**

--------------------------------------------------------------------------------------------
Folder Structure
--------------------------------------------------------------------------------------------

Each folder corresponds to 1 North Carolina county. There are 100 counties.

37001 = Alamance County
...
37199 = Yancey County


Inside each county folder, there are 5 files.

gq_people.csv is the generated group quarters population in that county, containing the following columns:
sp_id - Individual ID
sp_hh_id - Household ID
age - Age (years)
sex - Sex (0 = Male, 1 = Female)
race - Race (1 = White, 2 = Black, 3 = AI/AN, 6 = Asian, 7 = NH/PI, 8 = Other, 9 = Two or more)
gq_type - Group quarters type (N = Nursing homes, P = Prisons, M = Military facilities, C = College dormitories)
eth - Ethnicity (H = Hispanic, NH = Non-Hispanic)
age_cat = Age Category (years, 65p = 65+)
HYPER = Hypertension (0 = no, 1 = yes)
OBESE = Obesity (0 = no, 1 = yes)
ASTHMA = Asthma (0 = no, 1 = yes)
COPD = COPD (0 = no, 1 = yes)
DIAB = Diabetes (0 = no, 1 = yes)

households.csv is households in that county, containing the following columns:
hh_id - Household ID
FIPS - County FIPS code
hh_size - Household size
LAT - Latitude
LON - Longitude

people.csv is all non-group quarters residents in that county, containing the following columns:
sp_id - Individual ID
sp_hh_id - Household ID
age - Age (years)
sex - Sex (0 = Male, 1 = Female)
race - Race (1 = White, 2 = Black, 3 = AI/AN, 6 = Asian, 7 = NH/PI, 8 = Other, 9 = Two or more)
relate - Relationship to householder (0 = householder, 1 = spouse, 2 = child, 3 = sibling, 4 = parent, 5 = grandparent, 6 = in-law, 7 = other relative, 8 = boarder, 9 = housemate, 10 = partner, 11 = foster child, 12 = other non-relative)
eth - Ethnicity (H = Hispanic, NH = Non-Hispanic)
age_cat = Age Category (years, 65p = 65+)
HYPER = Hypertension (0 = no, 1 = yes)
OBESE = Obesity (0 = no, 1 = yes)
ASTHMA = Asthma (0 = no, 1 = yes)
COPD = COPD (0 = no, 1 = yes)
DIAB = Diabetes (0 = no, 1 = yes)

schools.csv is all schools in that county, containing the following columns:
NAME - School name
sch_id - School ID
FIPS - County FIPS code
LAT - Latitude
LON - Longitude
TYPE - Type (Public, Charter, Private)

workplaces.csv is all workplaces in that county, containing the following columns:
work_id - Workplace ID
Primary_NAICS_Code - Primary NAICS code
size - Workplace size (1-4, 5-24, 25-99, 100-499, 500-4,999, 5,000+)
employees - Number of employees
LAT - Latitude
LON - Longitude
