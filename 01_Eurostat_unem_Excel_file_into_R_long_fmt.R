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
