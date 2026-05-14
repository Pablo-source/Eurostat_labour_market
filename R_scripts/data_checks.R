# Data Checks

here()

data_folder = here("data")

Input_csv_files  <- list.files (path = data_folder,pattern = ("*.csv"))
Input_xls_files  <- list.files (path = data_folder,pattern = ("*.xls"))
Input_xlsx_files  <- list.files (path = data_folder,pattern = ("*.xlsx"))

# Original Excel files downloaded from Eurostat
"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx"
"une_rt_a__custom_14324113_page_spreadsheet.xlsx"

# Build path to original Eurostat files in \data sub-folder
file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx")
file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx")