# AIM: Set of functions to process and format data into Markdown report

# File  name: helper_functions.R

library(here)

# Building function to import files:

# 1. First function - Builds path to look for Input (Excel) files:

data_filepath  <- function(tab_name = NULL,choose_directory = NULL){
  
  if(choose_directory == "data_folder") {
  data_folder_path = file.path(here("data"))  
  if (dir.exists(data_folder_path)) {
    return(data_folder_path)  
  }
  
  } else if (choose_directory == "data_cleansed") {
  data_cleansed_path = file.path(here("data_cleansed"))
  if (dir.exists(data_cleansed_path))  
  return(data_cleansed_path)
  } else { stop ("please provide your own directory")}
  
}

# Use function
data_filepath(choose_directory = "data_folder")
data_filepath(choose_directory = "data_cleansed")
data_filepath(choose_directory = "my directory") # This will trigger error message



# 2. Include read_excel function

# data is located in "Sheet 1"


library(readxl) # read_excel()

Import_excel_files <- function(tab_name = NULL,choose_directory = NULL){
  
  # 1. Read in part time employment
  data_folder = here("data")
  
  part_time_raw <- read_excel(file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx"))
  
  
}

data_folder = here("data")

EUROSTAT_temp_employment_file <- file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx")


read_excel(file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx"))
