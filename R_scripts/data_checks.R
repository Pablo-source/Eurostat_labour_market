# Data Checks
library(dplyr)
library(here)
library(readxl)

here()

Input_csv_files  <- list.files (path = data_folder,pattern = ("*.csv"))
Input_xls_files  <- list.files (path = data_folder,pattern = ("*.xls"))
Input_xlsx_files  <- list.files (path = data_folder,pattern = ("*.xlsx"))

# Original Excel files downloaded from Eurostat
"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx"
"une_rt_a__custom_14324113_page_spreadsheet.xlsx"

# Build path to original Eurostat files in \data sub-folder
# Eurostat: 
# LFS adjusted series:
#     lfsi_pt_a (Part-time employment and temporary contracts-annual data)
#     une_rt_a (Unemployment by sex and age - annual data). Time 23/23 (2003-2025)


# Get Excel input file paths from \data folder
data_folder = here("data")

EUROSTAT_unemployment_file <-file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx") #     une_rt_a (Unemployment 
EUROSTAT_temp_employment_file <- file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx") #     lfsi_pt_a (Part-time employment 

# Get Excel input file sheets
Excel_tabs_temp_emp_file <- excel_sheets(EUROSTAT_temp_employment_file)
# [1] "Summary"   "Structure" "Sheet 1"  
Excel_tabs_unemp_file  <- excel_sheets(EUROSTAT_temp_employment_file)
# [1] "Summary"   "Structure" "Sheet 1" 

#     lfsi_pt_a (Part-time employment and temporary contracts-annual data)
#     une_rt_a (Unemployment by sex and age - annual data). Time 23/23 (2003-2025)

# 1.1 Import Unemployment file into R
# We will use this function in the "Import Excel files into R.R" script
data_folder = here("data")

## TESTING FUNCTION TO IMPORT EXCEL DATA INTO R

# Prepare script inside Import_excel_files() function I am building
# 1.1 Get Imported unemployment rate column names
names(read_excel(file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx"),
                 sheet = "Sheet 1" , col_names = TRUE, na = ":", skip = 8,n_max = 23) )

# 1.2 Start building function to turn raw data from Wide into Long format
EUROSTAT_unemployment_file <-file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx")

names(unemp_raw)

unemp_raw <- read_excel(file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx"),
                        sheet = "Sheet 1" , col_names = TRUE, na = ":", skip = 8,n_max = 23) %>% 
            rename(Date = "GEO (Labels)") %>% 
            filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
            pivot_longer(!Date, names_to = "Countries", values_to = "unemep") 


# Function to import different Excel files with Eurostat data into R 

# 1.2 Start building function - Initial argument (values_name: Name of the column we pivot long.)
# User will have to enter column_name pivoted from wide to long (as "values_name" argument)
library(dplyr)
library(here)
library(readxl)
library(tidyr)

Import_excel_files_test <- function(tab_name,choose_directory = NULL, selected_countries){

  data_folder = here("data")
  
  unemp_raw <- read_excel(file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx"),
                          sheet = tab_name, col_names = TRUE, na = ":", skip = 8,n_max = 23) %>% 
    rename(Date = "GEO (Labels)") %>% 
    filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
    pivot_longer(!Date, names_to = "Countries", values_to = "metric_value") 
  
  # Create new column to state metric_name
  # Create new column to specify format
  unem_long <- unemp_raw %>% mutate(
                                metric = "unemployment_rate",
                                units = "thousands")
  # rename column so we can later on union it with other indicators
  unemp_rate_metric <- unem_long %>% select(date = Date,
                              country = Countries, 
                              metric_value,
                              metric, 
                              units) %>% 
                      filter(country %in% c(selected_countries))
    
  # Filter previous dataset so we can select a handful of countries
  unem_rate_selected_countries <- 
  
  return(unemp_rate_metric)
  
}
# Parameters (tab_name = "Sheet 1", selcted_countries = c("country1","country2"))
Import_excel_files_test(tab_name = "Sheet 1", selected_countries = c('Bulgaria','Estonia','Ireland'))

