# R Script: 01 Eurostat Unemp csv file into R long format.R

# 1. Load required libraries
library(here)
library(tidyverse)
library(janitor)
library(readxl)

# Indicator: Unemployment rate
# • Unemployment by sex and age – annual data – Online data code: une_rt_a
# • Eurostat Indicator: Unemployment by sex and age – annual data
# • Eurostat reference table: une_rt_a

# 2. Check files in \data folder to import Excel file into R
here()
list.files (path = "./data" ,pattern = "xlsx$")
# [1] "une_rt_a__custom_14324113_page_spreadsheet_long.xlsx"

# 3. Import Excel file using {readxl} library with read_excel() function

COL_NAMES  <-read_excel(here("data", "une_rt_a__custom_14324113_page_spreadsheet_long.xlsx"), 
                        sheet = 3,skip = 9) %>% 
            clean_names()
COL_NAMES

names(COL_NAMES)
str(COL_NAMES)

# Store column names in a vector 
# Now I have a vector of length 38 for each column name
my_col_names <- names(COL_NAMES)
my_col_names

# 4. Import Unemployment data "une_rt_a" table from Eurostat 
# 4.1 Import data so Time is a standalone column
my_data  <-read_excel(here("data", "une_rt_a__custom_14324113_page_spreadsheet_long.xlsx"), 
                      sheet = 3,skip = 10) %>% 
                      clean_names() 
my_data

names(my_data)

#[1] "time" "x2"   "x3"   "x4"   "x5"   "x6"   "x7"   "x8"   "x9"   "x10"  "x11"  "x12"  "x13"  "x14"  "x15"  "x16"  "x17" 
#[18] "x18"  "x19"  "x20"  "x21"  "x22"  "x23"  "x24"  "x25"  "x26"  "x27"  "x28"  "x29"  "x30"  "x31"  "x32"  "x33"  "x34" 
#[35] "x35"  "x36"  "x37"  "x38" 

# 4.2 Create new date variable in imported my_data dataframe
# First I need to subset data to NOT to include footnotes. So I keep just data up to latest
# available year 2023, excluding footnotes

time_subset <- c("2003","2004","2005","2006","2007",
                 "2008","2009","2010","2011","2012",
                 "2013","2014","2015","2016","2017",
                 "2018","2019","2020","2021","2022",
                 "2023")

time_subset_seq <-seq(from=2003,to=2023)

my_data_period <- my_data %>% filter(time %in% time_subset_seq)

# Creating new date variable

new_dates <- my_data_period %>% 
             select(year = time,
                    x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,    
                    x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,
                    x29,x30,x31,x32,x33,x34,x35,x36,x37,x38) %>% 
             mutate(day = "01",month = "01") 
new_dates

# Using {lubridate} package to create final date variable
# We use make_daet() function to create a date variable from existing day, month, year variables
library(lubridate)

data_time_fmtd <- new_dates %>% 
             mutate(date = make_date(year,month,day))

# Subset variables and re-arange columns order
date_time_fmtd2 <- data_time_fmtd %>% 
                   select(date,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,    
                          x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,
                          x29,x30,x31,x32,x33,x34,x35,x36,x37,x38)

# 5. Assign meaningful column names 
# Previouly we created a list containing column names
my_col_names <- names(COL_NAMES)
length(my_col_names)
# [1] 38

# Its length matches the list of columns from our main dataframe date_time_fmtd2
names(date_time_fmtd2)
length(date_time_fmtd2)
# [1] 38

# So I can use names() function with a list created out of "my_col_names" object
# I will replace existing column names from main dataframe by items from my "my_col_names" list
date_time_fmtd3 <- date_time_fmtd2

# Assign column names to date_time_fmtd3 dataframe from my_col_names vector
names(date_time_fmtd3) = c(my_col_names)

# Rename dataframe as EU_UNEMP
EU_UNEMP <- date_time_fmtd3

# 6. Final cleansing process removing last set of rows wirth NA values from final dataset
# Exclude NA from original dataset using DPLYR verb na.omit() across all dataset

# In the imported data from the Excel file there are ":" symbols on emtpy rows
# I will use {naniar} library to replace these symbols by NA values
# replace_with_na() Function: use a list for each column with the Symbol ":" to be replaced by NA on each column
# We use replace_with_na() function from {naniar} library 
#     with each column where we want to replace : by specific NA values.
library(naniar)

EU_UNEMP_CLEAN <- EU_UNEMP %>% 
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
EU_UNEMP_CLEAN

# save output file as "EU_UNEMP_DATA.csv"
write.csv(EU_UNEMP_CLEAN,here("data","EU_UNEMP_CLEAN_une_rt_a_WIDE.csv"), row.names = TRUE)


# 7. Then re-shape wide data frame into Long format data frame

