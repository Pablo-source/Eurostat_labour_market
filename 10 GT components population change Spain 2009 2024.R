# Demography - population change components (Natural increase, New migration

# 10 GT components population change Spain 2009 2024

# Evolution of net external migration in Spain. 2014-2024
# Import Excel file into R: "INE Net external migration Spain 2014 2023.xls"
# 19/10/2025
# 01 Read in Excel population data

library(readxl)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)
library(gt) # Load gt table to create {gt} tables

Path <- here()
Path

# List excel files on Data sub-directory

# 01 Check (.xlsx) files available in "population_data" the data folder
list.files (path = "./data_demography" ,pattern = "xlsx$")

# I want to import latest file
# [6] "04 Components of population change.xlsx"

# 02. List tabs from above Excel file to know which tab to import
excel_sheets("./data_demography/04 Components of population change.xlsx")

# [1] "SPAIN_Components_pop_change" "INE Total Population SPAIN" 
# [3] "INE National Increase SPAIN" "INE Net External Migration" 
# [5] "Template comp pop change" 

# Spain components of population change GT tables created:
# 2009
# 2010

# 03. Read 2009 components of population change in Spain
# WIP (to execute this code below next)
comp_pop_change_spain_2009 <-  read_excel(
  here("population_data", "04 Components of population change.xlsx"), 
  sheet = 1, skip =8, n_max = 7) %>% 
  clean_names()
comp_pop_change_spain_2009

