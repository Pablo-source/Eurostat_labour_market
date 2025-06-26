# File: 09 Total population by nationality Spain 2004 2025
# Total population by nationality - Spain
## Input file:  INE total and foreign population figures Spain.xlsx
## Source: INE (Instituto Nacional de Estadistica): Spanish equivalent to the Office for National Statistics
## Import third tab from Excel file called "INE_Total_foreign_population"

# 1. Load required libraries
library(readxl)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)

Path <- here()
Path
# [1] "/home/pablo/Documents/Pablo_ubuntu/Repos/Pablo_source_repo/Eurostat_labour_market"

# List excel files on Data sub-directory

# 01 Check files available in the data folder. This time I want to import an Excel .xlsx file 
# This code below uses specific pattern to differenciate between .xls and .xlsx file we are interested in the latter
list.files (path = "./Demography" ,pattern = "xlsx$")

# [1] "INE total and foreign population figures Spain.xlsx"

# 02 List tabs from above Excel file to know which tab to import
# ## Import third tab from Excel file called "INE_Total_foreign_population"
excel_sheets("./Demography/INE total and foreign population figures Spain.xlsx")

# [1] "INE_Foreign_population"       "INE_Total_population"         "INE_Total_foreign_population"

# 03 Import Excel file into R
total_foreign_population_data <- read_excel(
  here("Demography", "INE total and foreign population figures Spain.xlsx"), 
  sheet = "INE_Total_foreign_population", skip =2) %>% 
  clean_names()
total_foreign_population_data

head(total_foreign_population_data)
names(total_foreign_population_data)

# 04 We can also omit footntes importing data just up to row 22
# To avoid all footnotes present in the original Excel file 
# # n_max = 22  - Import just numeric information from source
total_foreign_population_data <- read_excel(
  here("Demography", "INE total and foreign population figures Spain.xlsx"), 
  sheet = "INE_Total_foreign_population", skip =2, n_max = 22) %>% 
  clean_names()
total_foreign_population_data

# 05 Subset initial columns to keep relevant ones
# Also use select() statement from DPLYR to rename certain columns 
names(total_foreign_population_data)

#> names(total_foreign_population_data)
#[1] "todas_las_edades"                           "total"                                      "foreign_population"                        
#[4] "percent_foreign_nationals_total_population" "total_yo_y_n"                               "total_yo_y_percent"                        
#[7] "foreign_nationals_yo_y_n"                   "foreign_total_yo_y_percent"  

foreign_pop <- total_foreign_population_data

# Rename columns
# (date = todas_las_edades, Total_population = total,  )
foreign_pop <- foreign_pop %>% 
               select (date = todas_las_edades, Total_population = total)
foreign_pop

