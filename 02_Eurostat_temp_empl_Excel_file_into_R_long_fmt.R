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

# 1. Get files from \data sub-folder
list.files(path = "./data", pattern = "xlsx$")

# Import temporary employment indicator into R
# Part-time employment and temporary contracts – annual data - Online data code:[lfsi_pt_a]
# File: lfsi_pt_a__custom_14323356_page_spreadsheet_long.xlsx

# 1. Get COLUMNS names in the right way
IMPORTED_DATA  <-read_excel(here("data", "lfsi_pt_a__custom_14323356_page_spreadsheet_long.xlsx"), 
                        sheet = 3,
                        skip = 9) %>% 
  clean_names()

IMPORTED_DATA

# Imported data clean

