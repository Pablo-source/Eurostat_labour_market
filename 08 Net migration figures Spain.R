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

# Step 01 03: Check files available in the data folder
list.files (path = "./Demography" ,pattern = "xls$")

[1] "INE Net external migration Spain 2014 2023.xls"

# Step 02 03: List tabs from above Excel file to know which tab to import
excel_sheets("./Demography/INE Net external migration Spain 2014 2023.xls")
# [1] "Hoja1"