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


