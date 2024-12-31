# R Script: 03_Combine_formated_indicators.R

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


# 2. Test first on a small sample of countries. 
#    Combine both indicators together

# 2.1 Subset data just for three countries (spain,greece,france)

# spain,greece, france
Subset <-c("spain","greece","france")

# 2.2 Then create a new column to split each line chart by Indicator
# Also change each indicator original column name to a generic value as we are going to union them
unemp_data_check <- unemp_data %>% 
                    filter(country %in% Subset) %>% 
                    select(date,country,
                           value = unemp_rate) %>% 
                    mutate(indicator = "unemployment_rate")
unemp_data_check

temporary_data_check <- temporary_data %>% 
                        filter(country %in% Subset) %>% 
                        select(date,country,
                               value = temporary_rate) %>% 
                        mutate(indicator = "temporary_rate")
temporary_data_check

names(unemp_data_check)
names(temporary_data_check)
  