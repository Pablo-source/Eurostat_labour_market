# 08 Spain - Net migration
# Evolution of net external migration in Spain. 2014-2024
# Import Excel file into R: "INE Net external migration Spain 2014 2023.xls"

library(readxl)
library(here)
library(dplyr)
library(janitor)

Path <- here()
Path

# List excel files on Data sub-directory

# 01 Check files available in the data folder
list.files (path = "./Demography" ,pattern = "xls$")

[1] "INE Net external migration Spain 2014 2023.xls"

# 02 List tabs from above Excel file to know which tab to import
excel_sheets("./Demography/INE Net external migration Spain 2014 2023.xls")
# [1] "Hoja1"

# 03 Import Excel file into R
imported_net_migration_data <- read_excel(
  here("Demography", "INE Net external migration Spain 2014 2023.xls"), 
  sheet = 1, skip =17) %>% 
  clean_names()
imported_net_migration_data

# 03.1 add read_excel() arguments to import just required information from Excel file into R
# Remove empty cells
# skip = 7  - Skip first 7 rows of data from Excel file when importing it into R
imported_net_migration_data <-  read_excel(
  here("Demography", "INE Net external migration Spain 2014 2023.xls"), 
  sheet = 1, skip =7) %>% 
  clean_names()
imported_net_migration_data

# 03.2 Extract just rows including dates info
# n_max = 10  - Import just numeric information from source
net_migration_spain <-  read_excel(
  here("Demography", "INE Net external migration Spain 2014 2023.xls"), 
  sheet = 1, skip =7, n_max = 10) %>% 
  clean_names()
net_migration_spain

# 03.3 Remove NA values 
# {dplyr} using na.omit() function
net_migration_spain_clean <-  read_excel(
  here("Demography", "INE Net external migration Spain 2014 2023.xls"), 
  sheet = 1, skip =7, n_max = 10) %>% 
  clean_names() %>% 
  na.omit()
net_migration_spain_clean

# 03.4 Use col_types function with "guess" option to import cols with right format into R
# read_excel(col_types = c("guess","guess")



