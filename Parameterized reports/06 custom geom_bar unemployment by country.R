# Script:  06 custom geom_bar unemployment by county.R
# AIM 06 custom geom_bar unemployment metric for each country

# 1. Import previous files.
library(here)
library(tidyverse)

# here() starts at /home/pablo/Documents/Pablo_ubuntu/Github_repos/Pablo_source/Eurostat_labour_market
here()
# [1] "/home/pablo/Documents/Pablo_ubuntu/Github_repos/Pablo_source/Eurostat_labour_market"

# 1. Import unemployment indicator data 
combined_indic  <- read.table(here("Parameterized reports","data_param_reports", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                              header =TRUE, sep =',',stringsAsFactors =TRUE)
head(combined_indic)

str(combined_indic)

## 1.1 Apply date format to "date" column
#  mutate(datef = as.Date(date)) %>% 
# select(date = datef,country,metric_name,metric_value)

combined_indic_date_fmtd <- read.table(here("Parameterized reports","data_param_reports", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                                       header =TRUE, sep =',',stringsAsFactors =TRUE) %>% 
                            mutate(datef = as.Date(date)) %>% 
                            select(datef, country,metric_name,metric_value)
str(combined_indic_date_fmtd)
head(combined_indic_date_fmtd)

metrics_list <- combined_indic_date_fmtd %>% select(metric_name) %>% distinct()
metrics_list

unemployment_subset <- combined_indic_date_fmtd %>% 
                       filter(metric_name == 'unemp_rate')

disintc_countries <- unemployment_subset %>% select(country) %>% distinct()

# 2. Subset data for Greece and Italy
# greece, italy
unemp_greece_italy <- unemployment_subset %>% 
  filter(country %in% c("greece","italy")) 

head(unemp_greece_italy)
tail(unemp_greece_italy)

# 3. Create a basic bar plot for each country
# 3.1 Greece "#BAD1D6"

str(unemp_greece)

unemp_greece <- unemp_greece_italy %>% filter(country == 'greece')

Plot_unemp_greece <- unemp_greece %>% 
  ggplot(aes(x = datef, y = metric_value)) +
  geom_col(fill = "#BAD1D6") +
labs(title = "Unemployment in Greece.2009-2023 period",
     caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/",
     x = "Year", 
     y = "Unemployment rate %") +
  theme_classic() 
Plot_unemp_greece

here()
ggsave("Parameterized reports/01_Greece_unemployment_2003_2023.png", width = 6.38, height = 5.80)

# See script: https://github.com/Pablo-source/Eurostat_labour_market/blob/main/07_Annotate_curve_to_draw_straight_lines.R
# For a fully formatted bar plot using geom_col() ggp[lot2 function]

# 3.2 Italy "#539CBA"

unemp_italy <- unemp_greece_italy %>% filter(country == 'italy')

Plot_unemp_italy <- unemp_italy %>% 
  ggplot(aes(x = datef, y = metric_value)) +
  geom_col(fill = "#539CBA") +
  labs(title = "Unemployment in Italy.2009-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/",
       x = "Year", 
       y = "Unemployment rate %") +
  theme_classic() 
Plot_unemp_italy

here()
ggsave("Parameterized reports/02_Italy_unemployment_2003_2023.png", width = 6.38, height = 5.80)