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

# 1. Check (.xlsx) files available in "data_demography" the data folder
list.files (path = "./data_demography" ,pattern = "xlsx$")

# I want to import latest file
# [6] "04 Components of population change.xlsx"

# 2. List tabs from above Excel file to know which tab to import
excel_sheets("./data_demography/04 Components of population change.xlsx")

# [1] "SPAIN_Components_pop_change" "INE Total Population SPAIN" 
# [3] "INE National Increase SPAIN" "INE Net External Migration" 
# [5] "Template comp pop change" 

# Spain components of population change GT tables created:
# 2009
# 2010

# 3. Read components of population change 

# 3.1 Year 2009 data
# Components of population change in Spain year 2009
# WIP (to execute this code below next)
# 3.1.1  Build GT table for 2009 components of population change in Spain 
here::here()

comp_pop_change_spain_2009 <-  read_excel(
  here("data_demography", "04 Components of population change.xlsx"), 
  sheet = 1, skip =8, n_max = 7) %>% 
  clean_names()
comp_pop_change_spain_2009

# 3.1.2 rename columns
comp_pop_spain_2009_fmtd <- comp_pop_change_spain_2009 %>% 
  select("Spain components of population change Year 2009"= spain_2009_components_of_population_change,
         Value = x3)
comp_pop_spain_2009_fmtd

# 3.1.3 Final formatted GT table components of population change in Spain 2009.
GT_table_2009_fmtd_int <- comp_pop_spain_2009_fmtd %>% 
  gt() %>%
  tab_header(
    title = md("**Components of population change. Spain 2009**"),
    subtitle = ("2009-2010 period")
  ) %>% 
  # Add fmt_number(sep_mark= ",") to add thousands separator to Value column
  fmt_number(sep_mark = ",","Value") %>%
  tab_source_note(
    source_note = md("INE.Spanish Statistical Office. Population Continuous Statistics https://https://www.ine.es/jaxiT3/Tabla.htm?t=6566")
  ) %>%
  tab_source_note(
    source_note = md("INE.Spanish Statistical Office. Basic Demographic Indicators.Vital Statistics https://www.ine.es/jaxiT3/Tabla.htm?t=6566")
  ) %>%
  tab_source_note(
    source_note = "Source:Vital Statistics/Basic Demographic Indicators.Year2009,Population Continuous Census. Resident population by date. Year 2009,2010"
  ) %>% 
  fmt_number(columns = Value,decimals = 0,use_seps = TRUE)
GT_table_2009_fmtd_int
gtsave(GT_table_2009_fmtd_int,filename = "GT_tables/01 2009 2010 Spain components population change.png")  

# 3.2 Year 2010 data
# 3.2.1 GT table components of population change in Spain 2010
#  Read 2010 components of population change in Spain
comp_pop_change_spain_2010 <-  read_excel(
  here("data_demography", "04 Components of population change.xlsx"), 
  sheet = 1, skip =17, n_max = 7) %>% 
  clean_names()
comp_pop_change_spain_2010

comp_pop_spain_2010_fmtd <- comp_pop_change_spain_2010 %>% 
  select("Spain components of population change Year 2010"= spain_2010_components_of_population_change,
         Value = x3)
comp_pop_spain_2010_fmtd

# 3.2.2 Format and save GT table as .png output file 
GT_table_2010_fmtd_int <- comp_pop_spain_2010_fmtd %>% 
  gt() %>%
  tab_header(
    title = md("**Components of population change. Spain 2010**"),
    subtitle = ("2010-2011 period")
  ) %>% 
  # Add fmt_number(sep_mark= ",") to add thousands separator to Value column
  fmt_number(sep_mark = ",","Value") %>%
  tab_source_note(
    source_note = md("INE.Spanish Statistical Office. Population Continuous Statistics https://www.ine.es/jaxiT3/Tabla.htm?t=56934")
  ) %>%
  tab_source_note(
    source_note = md("INE.Spanish Statistical Office. Basic Demographic Indicators.Vital Statistics https://www.ine.es/jaxiT3/Tabla.htm?t=6566")
  ) %>%
  tab_source_note(
    source_note = "Source:Vital Statistics/Basic Demographic Indicators.Year2010,Population Continuous Census. Resident population by date. Year 2010,2011"
  ) %>% 
  fmt_number(columns = Value,decimals = 0,use_seps = TRUE)
GT_table_2010_fmtd_int
gtsave(GT_table_2010_fmtd_int,filename = "GT_tables/02 2010 2011 Spain components population change.png")  



