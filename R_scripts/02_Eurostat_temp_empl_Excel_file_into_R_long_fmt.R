# R Script: 02_Eurostat_temp_empl_Excel_file_into_R_long_fmt.R

# Importing second Eurostat Indicator for Temporary Employment

# Indicator: Employed persons with temporary contract
# - Part-time employment and temporary contracts – annual data - Online data code:lfsi_pt_a
# - Eurostat Indicator: Employed persons with temporary contract
# - Eurostat reference table: [lfsi_pt_a]. Activity and employment status (EMP_TEMP)>Employed persons with temporary contract

# Load required libraries
library(here)
library(tidyverse)
library(janitor)
library(readxl)

# Get files from \data sub-folder
list.files(path = "./data", pattern = "xlsx$")

# 1. Import temporary employment indicator into R
# Part-time employment and temporary contracts – annual data - Online data code:[lfsi_pt_a]
# File: lfsi_pt_a__custom_14323356_page_spreadsheet_long.xlsx

# 1. Get COLUMNS names in the right way
IMPORTED_DATA  <-read_excel(here("data", "lfsi_pt_a__custom_14828862_page_spreadsheet_long.xlsx"), 
                        sheet = 3,
                        skip = 10) %>% 
                        clean_names()

IMPORTED_DATA

# 2. Clean imported data 
# First I need to subset data to NOT to include footnotes. So I keep just data up to latest
# available year 2023, excluding footnotes

names(IMPORTED_DATA)

new_dates <- IMPORTED_DATA %>% 
             select(
               year = geo_labels,european_union_27_countries_from_2020,euro_area_20_countries_from_2023,
               belgium,bulgaria,czechia,denmark,germany,estonia,ireland,greece,spain,france,croatia
               ,italy,cyprus,latvia,lithuania,luxembourg,hungary,malta,netherlands,austria,poland                             
               ,portugal,romania,slovenia,slovakia,finland,sweden,iceland,norway,switzerland                          
               ,bosnia_and_herzegovina,montenegro,north_macedonia,serbia,turkiye)

# 3. Subset data to import just rows until 2023 exluding footnotes
time_subset_seq <-seq(from=2003,to=2023)

my_data_period <- new_dates %>% filter(year %in% time_subset_seq)

# 4. Create new date variables
# 4.1 Create new day and month variables 
library(lubridate)

data_time_fmtd <- my_data_period %>% mutate(day = "01",month = "01") 

# 4.2  Using {lubridate} package to create final date variable
# We use make_daet() function to create a date variable from existing day, month, year variables
library(lubridate)
  
data_time_fmtd2 <- data_time_fmtd %>% mutate(date = make_date(year,month,day))
names(data_time_fmtd2)


# 5.Subset variables and re-arrange columns order

data_time_fmtd3 <- data_time_fmtd2 %>% 
                   select(
                    date,
                    european_union_27_countries_from_2020,euro_area_20_countries_from_2023,     
                    belgium,bulgaria,czechia,denmark,germany,estonia,                               
                    ireland,greece,spain,france,croatia,italy,                                
                    cyprus,latvia,lithuania,                             
                    luxembourg,hungary,malta,           
                    netherlands,austria,poland,                                
                    portugal,romania,slovenia,                              
                    slovakia,finland,sweden,                                
                    iceland,norway,switzerland,                           
                    bosnia_and_herzegovina,montenegro,north_macedonia,                       
                    serbia,turkiye)

# Rename dataframe as EU_UNEMP
EU_TEMPEMP <- data_time_fmtd3

# 6. Final cleansing process removing last set of rows wirth NA values from final dataset
# Exclude NA from original dataset using DPLYR verb na.omit() across all dataset

# In the imported data from the Excel file there are ":" symbols on emtpy rows
# I will use {naniar} library to replace these symbols by NA values
# replace_with_na() Function: use a list for each column with the Symbol ":" to be replaced by NA on each column
# We use replace_with_na() function from {naniar} library 
#     with each column where we want to replace : by specific NA values.
library(naniar)

names(EU_TEMPEMP)

EU_TEMPEMP_CLEAN <-  EU_TEMPEMP %>% 
  replace_with_na(replace = list(european_union_27_countries_from_2020 = ':')) %>% 
  replace_with_na(replace = list(euro_area_20_countries_from_2023 = ':')) %>% 
  replace_with_na(replace = list(belgium = ':')) %>% 
  replace_with_na(replace = list(bulgaria = ':')) %>% 
  replace_with_na(replace = list(czechia = ':')) %>% 
  replace_with_na(replace = list(denmark = ':')) %>% 
  replace_with_na(replace = list(germany = ':')) %>% 
  replace_with_na(replace = list(estonia = ':')) %>% 
  replace_with_na(replace = list(ireland = ':')) %>% 
  replace_with_na(replace = list(greece = ':')) %>% 
  replace_with_na(replace = list(spain = ':')) %>% 
  replace_with_na(replace = list(france = ':')) %>% 
  replace_with_na(replace = list(croatia = ':')) %>% 
  replace_with_na(replace = list(italy = ':')) %>% 
  replace_with_na(replace = list(cyprus = ':')) %>% 
  replace_with_na(replace = list(latvia = ':')) %>% 
  replace_with_na(replace = list(lithuania = ':')) %>% 
  replace_with_na(replace = list(luxembourg = ':')) %>% 
  replace_with_na(replace = list(hungary = ':')) %>% 
  replace_with_na(replace = list(malta = ':')) %>% 
  replace_with_na(replace = list(netherlands = ':')) %>% 
  replace_with_na(replace = list(austria = ':')) %>% 
  replace_with_na(replace = list(poland = ':')) %>% 
  replace_with_na(replace = list(portugal = ':')) %>% 
  replace_with_na(replace = list(romania = ':')) %>% 
  replace_with_na(replace = list(slovenia = ':')) %>% 
  replace_with_na(replace = list(slovakia = ':')) %>% 
  replace_with_na(replace = list(finland = ':')) %>% 
  replace_with_na(replace = list(sweden = ':')) %>% 
  replace_with_na(replace = list(iceland = ':')) %>% 
  replace_with_na(replace = list(norway = ':')) %>% 
  replace_with_na(replace = list(switzerland = ':')) %>% 
  replace_with_na(replace = list(bosnia_and_herzegovina = ':')) %>% 
  replace_with_na(replace = list(montenegro = ':')) %>% 
  replace_with_na(replace = list(north_macedonia = ':')) %>% 
  replace_with_na(replace = list(serbia = ':')) %>% 
  replace_with_na(replace = list(turkiye = ':')) 
EU_TEMPEMP_CLEAN

# save output file as "EU_UNEMP_DATA.csv"
write.csv(EU_TEMPEMP_CLEAN,here("data","EU_TEMPEMP_CLEAN_lfsi_pt_a_WIDE.csv"), row.names = TRUE)

# 7. Then re-shape wide data frame into Long format data frame
# using pivot_longer() function from {DPLYR}
# First column is date, so we are only interested in pivoting the remaining columns

# Batch 01-03: Countries: euro_area_20_countries_from_2023 > spain
names(EU_TEMPEMP_CLEAN)

Temp_emp_countries_02_11 <- EU_TEMPEMP_CLEAN %>%  select(
                                                  date,euro_area_20_countries_from_2023,
                                                  belgium,bulgaria,czechia,denmark,germany,estonia,
                                                  ireland,greece,spain)
                                                  
                                                  
                                                
EU_TEMP_CLEAN_LONG_02_11  <- Temp_emp_countries_02_11 %>% 
                                          pivot_longer( cols = 2:11,
                                                        names_to = "country",
                                                        values_to = "temporary_rate")      
# Batch 02-03 Country:(France)
# EU_TEMPEMP_CLEAN

Temp_emp_countries_12 <- EU_TEMPEMP_CLEAN %>%  
                         select(date,france)
       
EU_TEMP_CLEAN_LONG_12  <- Temp_emp_countries_12 %>% 
                            pivot_longer( cols = 2,
                                          names_to = "country",
                                          values_to = "temporary_rate") 


# Bath 03-03, remaining countries
Temp_emp_countries_13_26 <- EU_TEMPEMP_CLEAN %>% 
                         select(
                           date,croatia,                                 
                              italy,cyprus,latvia,lithuania, luxembourg,hungary,malta,
                           netherlands,austria,poland,portugal,romania,slovenia,
                           slovakia,finland,sweden, iceland,norway,switzerland,
                           bosnia_and_herzegovina, montenegro,north_macedonia,
                           serbia,turkiye)

EU_TEMP_CLEAN_LONG_13_26  <- Temp_emp_countries_13_26 %>% 
  pivot_longer( cols = 2:26,
                names_to = "country",
                values_to = "temporary_rate") 

# 8. Finally UNION all three pivoted long datasets 
# DPLYR using "bind_rows" function 
# EU_TEMP_CLEAN_LONG_02_11
# EU_TEMP_CLEAN_LONG_12
# EU_TEMP_CLEAN_LONG_13_26

str(EU_TEMP_CLEAN_LONG_02_11)
str(EU_TEMP_CLEAN_LONG_12)
str(EU_TEMP_CLEAN_LONG_13_26)

# 8.1 Convert temporary_rate as Numeric across all dataframes prior to unioning them
EU_TEMP_CLEAN_LONG_02_11$temporary_rate <- as.numeric(EU_TEMP_CLEAN_LONG_02_11$temporary_rate)
str(EU_TEMP_CLEAN_LONG_02_11)

EU_TEMP_CLEAN_LONG_12$temporary_rate <-  as.numeric(EU_TEMP_CLEAN_LONG_12$temporary_rate)
str(EU_TEMP_CLEAN_LONG_12)

EU_TEMP_CLEAN_LONG_13_26$temporary_rate <-  as.numeric(EU_TEMP_CLEAN_LONG_13_26$temporary_rate)
str(EU_TEMP_CLEAN_LONG_13_26)

view(EU_TEMP_CLEAN_LONG_02_11)
view(EU_TEMP_CLEAN_LONG_12)
view(EU_TEMP_CLEAN_LONG_13_26)

# 8.2 Union them all 
library(dplyr)

EU_TEMP_CLEANSED_LONG <- bind_rows(EU_TEMP_CLEAN_LONG_02_11,
                                   EU_TEMP_CLEAN_LONG_12,
                                   EU_TEMP_CLEAN_LONG_13_26)
EU_TEMP_CLEANSED_LONG

str(EU_TEMP_CLEANSED_LONG)

# 9.save output file as "EU_UNEMP_DATA.csv" in a NEW folder called data_cleansed
write.csv(EU_TEMP_CLEANSED_LONG,here("data_cleansed","EU_TEMP_CLEANSED_lfsi_pt_a_LONG.csv"), row.names = TRUE)
