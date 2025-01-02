# R Script: 04_Facetted_charts_display_two_indicators.R

# Combine cleansed files for Unemployment and Temporary indicators
# data_cleansed folder: 

# Get previous .csv files we created from /data_cleansed sub-folder
rm(list=ls()[! ls() %in% c("EU_UNEMP_CLEANSED_LONG",
                           "EU_TEMP_CLEANSED_LONG")])

list.files(path = "./data_cleansed", pattern = "csv$")

# > list.files(path = "./data_cleansed", pattern = "csv$")
# [1] "EU_TEMP_CLEANSED_lfsi_pt_a_LONG.csv" "EU_UNEMP_CLEAN_une_rt_a_LONG.csv" 


# 1. Import previous files 
library(here)
library(dplyr)

# 1.1 Import unemployment indicator data 
unemp_data <- read.table(here("data_cleansed", "EU_UNEMP_CLEAN_une_rt_a_LONG.csv"),
                         header =TRUE, sep =',',stringsAsFactors =TRUE)
head(unemp_data)


# 1.2 Import temporary indicator data 
temporary_data <- read.table(here("data_cleansed", "EU_TEMP_CLEANSED_lfsi_pt_a_LONG.csv"),
                             header =TRUE, sep =',',stringsAsFactors =TRUE)
head(temporary_data)

# 2 Then create a new column to split each line chart by Indicator
# Also change each indicator original column name to a generic value as we are going to union them
unemp_data_label <- unemp_data %>% 
  select(date,country,
         value = unemp_rate) %>% 
  mutate(indicator = "unemployment_rate")

str(unemp_data_label)

temporary_data_label <- temporary_data %>% 
  select(date,country,
         value = temporary_rate) %>% 
  mutate(indicator = "temporary_rate")

str(temporary_data_label)

# 3. Then we union them to create chart displaying both indicators combined
all_indicators_data <- bind_rows(unemp_data_label,temporary_data_label)
View(all_indicators_data)

# Create new formatted date variable
all_indicators_datef <- all_indicators_data %>% 
  mutate(datef = as.Date(date))
all_indicators_datef

str(all_indicators_datef)

# 4. Plot countries in two set of small multiples line charts displaying both indicators (unemployment rate and temporary
#    employment rate by country by year)
# filter(country %in% Subset) %>% 

countries <- all_indicators_data %>% select(country) %>% 
  distinct()
nrow(countries)

# 4.1 Plotting first batch of countries

# Out of the 36 different countries in the imported data set,  I will chart first set of 18 countries from initial list
# (euro_area_20_countries_from_2023,belgium,bulgaria,czechia,denmark,germany,estonia,ireland,greece,spain,france,croatia,
#  italy,cyprus,latvia,lithuania,luxembourg,hungary)

Subset_countries_01 <-c("euro_area_20_countries_from_2023","belgium","bulgaria","czechia","denmark","germany","estonia",
                        "ireland","greece","spain","france","croatia","italy","cyprus","latvia","lithuania","luxembourg",
                        "hungary")

Subset_01_plot_data <- all_indicators_datef %>% 
  select(date,datef,country,value,indicator) %>% 
  filter(country %in% Subset_countries_01) 
Subset_01_plot_data

# First batch of countries
# Display line charts facets by country displaying each indicator as individual line for each country 
line_chart_batch_01 <- Subset_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Subset 01 02- 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") # Place legent at the bottom

line_chart_batch_01

ggsave("plots_output/04_Unemp_temp_rate_line_chart_batch_01_selectec_countries_2003_2023.png", width = 6, height = 4)

# 4.2 Plotting second batch of countries

# Second batch of countries
Subset_countries_02 <-c("euro_area_20_countries_from_2023","malta","netherlands","austria",
                        "poland","portugal","romania","slovenia","slovakia","finland","sweden",
                        "iceland","norway","switzerland","bosnia_and_herzegovina","montenegro",
                        "north_macedonia","serbia","turkiye"
                        )

Subset_02_plot_data <- all_indicators_datef %>% 
  select(date,datef,country,value,indicator) %>% 
  filter(country %in% Subset_countries_02) 
Subset_02_plot_data

# Second batch of countries
# Display line charts facets by country displaying each indicator as individual line for each country 
line_chart_batch_02 <- Subset_02_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Subset 02 02- 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") # Place legent at the bottom

line_chart_batch_02
ggsave("plots_output/05_Unemp_temp_rate_line_chart_batch_02_selectec_countries_2003_2023.png", width = 6, height = 4)

