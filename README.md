** Please note this is a draft repository being prepared for a manuscript - it is not finished! **

# Augmenting-SynthPop
Contains R scripts used in "Augmenting an individual-level synthetic population dataset for population health applications" (Sandborn &amp; Delamater, 2026).

This code reproduces the workflow described in the manuscript:
**Full citation**

--------------------------------------------------------------------------------------------
Repository Structure
--------------------------------------------------------------------------------------------

**File** &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp; **Description**\
README.txt &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp; Open right now!\
00.0_round_preserve_function.R &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;Defines a rounding function to preserve sums\
01.1_estimate_military_gqs.R &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;Generates military group quarters population\
01.2_estimate_correctional_gqs.R &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;Generates correctional facility group quarters population\
01.3_estimate_dormitory_gqs.R	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;Generates college dormitory group quarters population\
01.4_estimate_nursing_gqs.R	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;Generates nursing home facility group quarters population\
02.0_school_assignment.R &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Assigns grades & schools to agents\
03.0_workplace_assignment.R	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;Assigns workplaces to agents\
04.0_ethnicity_assignment.R	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Assigns ethnicity to agents\
05.0_condition_assignment.R	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;Assigns conditions to agents\
05.1_condition_prevalences.R &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp; Condition reported prevalence values - Don't actually need to run\
05.2_condition_functions.R &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Defines functions for assigning conditions - Don't actually need to run

/data/\
&emsp;&emsp;input	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;Source data\
&emsp;&emsp;intermediate &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;Processed intermediate files\
/output/\
&emsp;&emsp;results	&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;Final outputs


--------------------------------------------------------------------------------------------
Requirements
--------------------------------------------------------------------------------------------

R version: 4.3.3 or later
RStudio

Required R packages (and code for installation):
install.packages(c("tidyverse", "tidycensus", "sf", "data.table", "arrow", "magrittr", "ggplot2", "mapview"))


--------------------------------------------------------------------------------------------
Input Data
--------------------------------------------------------------------------------------------

1) Group quarters generation:

• U.S. Census Bureau (CITE). Includes age group (0-17, 18-64, and 65+ years) and sex breakdowns of the population living in group quarters by type by census block. Additionally includes age group (0-19, 20-24, 25-29, …, 60-64, 65+ years) and sex breakdowns of the population living in group quarters by type by county.

• Homeland Foundation-Level Infrastructure Database (HFLID) (CITE). Includes geographic point representations of U.S. Military Base locations, correctional facility locations, universities and colleges, and nursing homes.\
Link:

• Department of Defense (DoD) Selected Manpower Statistics for 2001-2005 (CITE). Contains the percent of the military population by single year of age (for those age 17 to 39 years), for 40-44 years, 45-49 years, and 50 and above years for both males and females by year for 2001 to 2005.\
Link:


2) School assignment:

• SynthPop 2019 data (RTI SynthPopTM - Synthetic Population Dataset | RTI, 2020). The people file should have 1 row per synthetic person with the following essential attributes: person ID, household ID, race, sex, age, and relationship to householder. The household file should have 1 row per synthetic household with the following essential attributes: household ID, block group code, geometry (latitude & longitude).\
Link: https://www.rti.org/focus-area/rti-synthpoptm
	
• Grade data: Public Use Microdata Sample (PUMS) 2020 data containing a sample of school-aged children and their current grade. This was only used to assign grades to the synthetic population (Public Use Microdata Sample (PUMS), 2021). The grade data file should have 1 row per sampled individual with the following essential attributes: person weight, school enrollment status, grade level, and age.

• Public school data: National Center for Education Statistics (NCES) public school list for the 2019-2020 academic year containing grade-by-grade enrollment (including charter schools) (ELSI - Elementary and Secondary Information System, 2021). Additionally, we used NCES Education Demographic and Geographic Estimates (EDGE) public school data for the same years which contains school locations (School Geocodes & Geoassignments, 2021). The public school data files should have 1 row per school with the following essential attributes: school ID, state, county, school district ID, grade-by-grade enrollment, and geometry (latitude & longitude).\
Link: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx \
Link: https://nces.ed.gov/programs/edge/Geographic/SchoolLocations
		
• Private school data: NCES private school list for the 2019-2020 academic year containing total enrollment and the range of grades taught at the school (ELSI - Elementary and Secondary Information System, 2021). Additionally, we used NCES EDGE private school data for the same years which contains school locations (School Geocodes & Geoassignments, 2021). The private school data files should have 1 row per school with the following essential attributes: school ID, state, county, type, enrollment, high grade, low grade, and geometry (latitude & longitude).\
Link: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx \
Link: https://catalog.data.gov/dataset/private-school-locations-current-f7d96
		
• Home school data: North Carolina Department of Administration Home School Statistical Summary for the 2019-2020 academic year containing estimated statewide enrollment by age and estimated enrollment by county (Home School Statistics, 2020). The first home school data file should have 1 row per county with the following essential columns: county and enrollment. The second home school data file should have 1 row per grade level with the following essential attributes: grade and proportion of total homeschool children in that grade.\
Link: https://www.doa.nc.gov/dnpe-home-school-2019-20-annual-report-pdf/open

• School district data: NCES EDGE school district boundaries for the 2019-2020 academic year (School District Boundaries, 2021). The school district data file should have 1 row per school district with the following essential attributes: school type, geoid, and geometry.\
Link: https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries#:~:text=School%20districts%20are%20geographic%20entities,accessible%20as%20GIS%20web%20services.&text=Files%20represent%20the%202023%2D2024,%2C%20Secondary%2C%20and%20Supervisory%20districts

• County boundaries 


3) Workplace assignment:
	
• U.S. Census Bureau Longitudinal Employer-Household Dynamics (LEHD) Origin-Destination Employment Statistics (LODES) 2019 data for all workers with both workplace and residence in the state (LEHD Origin-Destination Employment Statistics, 2020). The LODES data file should have 1 row per workplace block group ID with the following essential attributes: workplace block group ID, home block group ID, total people, total <29 years, total 30-54 years, and total > 55 years.\
Link: https://lehd.ces.census.gov/data/
	
• Data Axle Reference Solutions 2020 data including individual businesses with a North American Industry Classification System (NAICS) code, employee size, and max employee size (Data Axle Reference Solutions, 2020). The data file should have 1 row per business with the following essential attributes: census tract, primary NAICS code, employee size, and geometry (latitude and longitude).\
Link: https://www.data-axle.com/platforms-products/reference-solutions/
	

4) Ethnicity assignment:

• U.S. Census Bureau Demographic and Housing Characteristics (DHC) 2020 data for ethnicity by age and race (2020 Census Demographic and Housing Characteristics File (DHC), 2023). The census ethnicity data file should have 1 row per county with the following essential attributes: total population by race group, total Hispanic population by race group, and proportion of total population that is Hispanic by race group.\
Link: https://www.census.gov/data/tables/2023/dec/2020-census-dhc.html 


5) Chronic condition assignment:

• Update 05.1_condition_prevalences.R with the percentage of each condition overall, by age group, by race group, by ethnicity, and by co-morbidities (if applicable).


--------------------------------------------------------------------------------------------
Outputs
--------------------------------------------------------------------------------------------

1) Group quarters generation:\
output/results/NC_mil_gq_people.csv\
output/results/NC_mil_gq.csv\
output/results/NC_cor_gq_people.csv\
output/results/NC_cor_gq.csv\
output/results/NC_uni_gq_people.csv\
output/results/NC_uni_gq.csv\
output/results/NC_nur_gq_people.csv\
output/results/NC_nur_gq.csv\

output/results/37001/37001_gq.csv\
...\
output/results/37199/37199_gq.csv


2) School assignment:\
output/results/school_assignments.csv


3) Workplace assignment:\
output/results/workplace_assignments.csv


4) Ethnicity assignment:\
data/intermediate/37001/37001_people.csv\
data/intermediate/37001/37001_gq.csv\
...\
data/intermediate/37199/37199_people.csv\
data/intermediate/37199/37199_gq.csv


5) Chronic condition assignment:\
output/results/37001/37001_people.csv\
output/results/37001/37001_gq.csv\
...\
output/results/37199/37199_people.csv\
output/results/37199/37199_gq.csv

output/results/cond_white.csv\
output/results/cond_black.csv\
output/results/cond_rem.csv\
output/results/cond_hisp.csv\
output/results/count_sum.csv



--------------------------------------------------------------------------------------------
Contact
--------------------------------------------------------------------------------------------

Maintainer: Hilary Sandborn & Paul Delamater\
Email: hsandborn@unc.edu; pld@email.unc.edu \
Institution: Carolina Population Center at the University of North Carolina at Chapel Hill


