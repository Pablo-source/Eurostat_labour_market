# R Script: 04_Facetted_charts_display_two_indicators.R

# Combine cleansed files for Unemployment and Temporary indicators
# data_cleansed folder: 

# Get previous .csv files we created from /data_cleansed sub-folder
rm(list=ls()[! ls() %in% c("EU_UNEMP_CLEANSED_LONG",
                           "EU_TEMP_CLEANSED_LONG")])

list.files(path = "./data_cleansed", pattern = "csv$")

# > list.files(path = "./data_cleansed", pattern = "csv$")
# [1] "EU_TEMP_CLEANSED_lfsi_pt_a_LONG.csv" "EU_UNEMP_CLEAN_une_rt_a_LONG.csv" 


# 1. Import previous files 
library(here)
library(dplyr)

# 1.1 Import unemployment indicator data 
unemp_data <- read.table(here("data_cleansed", "EU_UNEMP_CLEAN_une_rt_a_LONG.csv"),
                         header =TRUE, sep =',',stringsAsFactors =TRUE)
head(unemp_data)


# 1.2 Import temporary indicator data 
temporary_data <- read.table(here("data_cleansed", "EU_TEMP_CLEANSED_lfsi_pt_a_LONG.csv"),
                             header =TRUE, sep =',',stringsAsFactors =TRUE)
head(temporary_data)