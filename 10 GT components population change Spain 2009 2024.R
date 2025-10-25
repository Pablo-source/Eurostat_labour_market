# Demography - population change components (Natural increase, New migration

# Script: 10 GT components population change Spain 2009 2024.R

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

# 3.3 Year 2011 data
# 3.3.1 GT table components of population change in Spain 2011
#  Read 2011 components of population change in Spain
#  read_excel(..skip = 27, n_max =7)
comp_pop_change_spain_2011 <-  read_excel(
  here("data_demography", "04 Components of population change.xlsx"), 
  sheet = 1, skip =27, n_max = 7) %>% 
  clean_names()
comp_pop_change_spain_2011

comp_pop_spain_2011_fmtd <- comp_pop_change_spain_2011 %>% 
  select("Spain components of population change Year 2011"= spain_2011_components_of_population_change,
         Value = x3)
comp_pop_spain_2011_fmtd

GT_table_2011_fmtd_int <- comp_pop_spain_2011_fmtd %>% 
  gt() %>%
  tab_header(
    title = md("**Components of population change. Spain 2011**"),
    subtitle = ("2011-2012 period")
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
GT_table_2011_fmtd_int
gtsave(GT_table_2011_fmtd_int,filename = "GT_tables/03 2011 2012 Spain components population change.png")  


# 3.4 Year 2012 data
# 3.4.1 GT table components of population change in Spain 2012
#  Read 2012 components of population change in Spain
#  read_excel(..skip = 36, n_max =7)
# I need to populate the underlying Excel file with all components for 2012 population growth.
comp_pop_change_spain_2012 <-  read_excel(
  here("data_demography", "04 Components of population change.xlsx"), 
  sheet = 1, skip =36, n_max = 7) %>% 
  clean_names()
comp_pop_change_spain_2012

comp_pop_spain_2012_fmtd <- comp_pop_change_spain_2012 %>% 
  select("Spain components of population change Year 2012"= spain_2012_components_of_population_change,
         Value = x3)
comp_pop_spain_2012_fmtd


# 3.5 Year 2013 data
# 3.5.1 GT table components of population change in Spain 2013
#  Read 2013 components of population change in Spain
#  read_excel(..skip = 45, n_max =7)

# 3.6 Year 2014 data
# 3.6.1 GT table components of population change in Spain 2014
#  Read 2014 components of population change in Spain
#  read_excel(..skip = 54, n_max =7)

# 3.7 Year 2015 data
# 3.7.1 GT table components of population change in Spain 2015
#  Read 2015 components of population change in Spain
#  read_excel(..skip = 63, n_max =7)

# 3.8 Year 2016 data
# 3.8.1 GT table components of population change in Spain 2016
#  read_excel(..skip = 72, n_max =7)

# 3.9 Year 2017 data
# 3.9.1 GT table components of population change in Spain 2017
#  read_excel(..skip = 81, n_max =7)

# 3.10 Year 2018 data
# 3.10.1 GT table components of population change in Spain 2018
#  read_excel(..skip = 90, n_max =7)

# 3.11 Year 2019 data
# 3.11.1 GT table components of population change in Spain 2019
#  read_excel(..skip = 99, n_max =7)

# 3.12 Year 2020 data
# 3.12.1 GT table components of population change in Spain 2020
#  read_excel(..skip = 108, n_max =7)

# 3.13 Year 2021 data
# 3.13.1 GT table components of population change in Spain 2021
#  read_excel(..skip = 117, n_max =7)

# 3.14 Year 2022 data
# 3.14.1 GT table components of population change in Spain 2022
#  read_excel(..skip = 126, n_max =7)

# 3.15 Year 2023 data
# 3.15.1 GT table components of population change in Spain 2023
#  read_excel(..skip = 135, n_max =7)
comp_pop_change_spain_2023 <-  read_excel(
  here("data_demography", "04 Components of population change.xlsx"), 
  sheet = 1, skip =135, n_max = 7) %>% 
  clean_names()
comp_pop_change_spain_2023

comp_pop_spain_2023_fmtd <- comp_pop_change_spain_2023 %>% 
  select("Spain components of population change Year 2023"= spain_2023_components_of_population_change,
         Value = x3)
comp_pop_spain_2023_fmtd

GT_table_2011_fmtd_int <- comp_pop_spain_2023_fmtd %>% 
  gt() %>%
  tab_header(
    title = md("**Components of population change. Spain 2023**"),
    subtitle = ("2023-2024 period")
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
    source_note = "Source:Vital Statistics/Basic Demographic Indicators.Year2010,Population Continuous Census. Resident population by date. Year 2023,2024"
  ) %>% 
  fmt_number(columns = Value,decimals = 0,use_seps = TRUE)
GT_table_2011_fmtd_int
gtsave(GT_table_2011_fmtd_int,filename = "GT_tables/16 2023 2024 Spain components population change.png")  

# 3.16 Year 2024 data
# 3.16.1 GT table components of population change in Spain 2024
#  read_excel(..skip = 144, n_max =7)
