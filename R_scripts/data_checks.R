# Data Checks
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

data_folder = here("data")


# Get Excel input file paths from \data folder

EUROSTAT_temp_employment_file <- file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx")
EUROSTAT_unemployment_file <-file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx")

Excel_tabs <- excel_sheets("./data/Figure_2__Median_income_has_increased_during_the_10_years_leading_up_to_financial_year_ending_2022.xls")

# Get Excel input file sheets
Excel_tabs_temp_emp_file <- excel_sheets(EUROSTAT_temp_employment_file)
# [1] "Summary"   "Structure" "Sheet 1"  