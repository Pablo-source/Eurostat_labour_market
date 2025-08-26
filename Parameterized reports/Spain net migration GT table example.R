# 04 GT table from imported file include thousands setp.R
# R Script: "Spain net migration GT table example.R"
library(readxl)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)
library(gt) # Load gt table to create {gt} tables


Path <- here()
Path

# List excel files on Data sub-directory

# 01 Check files available in the data folder
list.files (path = "./data" ,pattern = "xls$")

# [1] "INE Net external migration Spain 2014 2023.xls"

# List tabs from above Excel file to know which tab to import
excel_sheets("./data/INE Net external migration Spain 2014 2023.xls")
# [1] "Hoja1"

# 01 Import Excel file into R
net_migration_spain <- read_excel(
  here("data", "INE Net external migration Spain 2014 2023.xls"),
  sheet = 1, skip =7, n_max = 10,
  col_types = c("guess","guess")) %>%
  clean_names()%>%
  na.omit()
net_migration_spain

names(net_migration_spain)

net_migration_table <- net_migration_spain %>% 
  select(Year = year, 'Net Migration' = net_external_migration)

net_migration_table

# 02 Change names and create a gt table
# This is how you create a gt table from a DataFrame
gt_table <- net_migration_table %>% 
  gt() %>%
  tab_header(
    title = md("**Spain External net migration**"),
    subtitle = ("2014-2023 period")
  )
gt_table

# 03 Include source note
# A source note can be added to the table’s footer through use
# of tab_source_note(). It works in the same way as tab_header()
# (it also allows for Markdown inputs) except it can be called
# multiple times—each invocation results in the addition of a source note.

gt_table_source_note <- net_migration_table %>% 
  gt() %>%
  tab_header(
    title = md("**Spain External net migration**"),
    subtitle = ("2014-2023 period") 
    )%>%
      tab_source_note(
        source_note = md("INE.Spanish Statistical Office. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm")
      ) %>%
      tab_source_note(
        source_note = "Source: Satistics on Migrations and Changes of Residence (SMCR). Year 2023"
      )
gt_table_source_note

# 04 Include Thousands separator applied only to "Net Migration" Column
# sep_mark = ",",

gt_table_thousands_separator <- net_migration_table %>% 
  gt() %>%
  tab_header(
    title = md("**Spain External net migration**"),
    subtitle = ("2014-2023 period") 
    ) %>% 
  # Add fmt_number(sep_mark= ",") to add thousands separator to Net Migration column
  fmt_number(sep_mark = ",","Net Migration") %>%
  tab_source_note(
    source_note = md("INE.Spanish Statistical Office. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm")
  ) %>%
  tab_source_note(
    source_note = "Source: Satistics on Migrations and Changes of Residence (SMCR). Year 2023"
  )
gt_table_thousands_separator
