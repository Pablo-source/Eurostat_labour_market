# R Script: 04_Facetted_charts_display_two_indicators.R

# Combine cleansed files for Unemployment and Temporary indicators
# data_cleansed folder: 

# 1. Import previous files 
library(here)
library(dplyr)

# 1.1 Import combined indicators  
combined_indic  <- read.table(here("data_cleansed", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                         header =TRUE, sep =',',stringsAsFactors =TRUE)
head(combined_indic)

# 2 Jut to check overall values 
# Split initial combined indicators into unemployment and temporary rate data sets 
unemp_data  <- combined_indic %>% 
  select(date,country,metric_name,metric_value) %>% 
  filter(metric_name == "unemp_rate")

temp_rate_data  <- combined_indic %>% 
  select(date,country,metric_name,metric_value) %>% 
  filter(metric_name == "temporary_rate")

combined_indic_datefmt <- combined_indic %>% 
  mutate(datef = as.Date(date)) %>% 
  select(date = datef,country,metric_name,metric_value)

str(combined_indic_datefmt)

# 3. Data wrangling for plots

# Out of the 36 different countries in the imported data set,  I will chart first set of 18 countries from initial list
# (euro_area_20_countries_from_2023,belgium,bulgaria,czechia,denmark,germany,estonia,ireland,greece,spain,france,croatia,
#  italy,cyprus,latvia,lithuania,luxembourg,hungary)

Subset_countries_01 <-c("euro_area_20_countries_from_2023","belgium","bulgaria","czechia","denmark","germany","estonia",
                        "ireland","greece","spain","france","croatia","italy","cyprus","latvia","lithuania","luxembourg",
                        "hungary")

Subset_01_plot_data <- combined_indic_datefmt %>% 
  select(date,country,metric_name,metric_value) %>% 
  filter(country %in% Subset_countries_01) 

# Rename datef variable as date
Plots_01_data <- Subset_01_plot_data %>% 
              select(date, country, metric_value, metric_name) 

# Just check the two indicators we are going to plot
indicators_list <- Date_fmtd_plots %>% select(metric_name) %>% distinct()
indicators_list

# 4. Plot indicators by countries -First subset of countries (01_18)

# First batch of countries
# Display line charts facets by country displaying each indicator as individual line for each country 
library(ggplot2)

line_chart_batch_01 <- Plots_01_data %>% 
  ggplot( fill = metric_name) +
  geom_line(aes(date,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - 2003-2023 period",
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
                        "north_macedonia","serbia","turkiye")

Subset_02_plot_data <- combined_indic_datefmt  %>% 
  select(date,country,metric_name,metric_value) %>% 
  filter(country %in% Subset_countries_02) 
Subset_02_plot_data

# Second batch of countries
# Display line charts facets by country displaying each indicator as individual line for each country 
str(Subset_02_plot_data)

Subset_02_plot_data_fmtd <- Subset_02_plot_data %>% select(date,country,metric_name,metric_value)

line_chart_batch_02 <- Subset_02_plot_data_fmtd %>% 
  ggplot( fill = metric_name) +
  geom_line(aes(date,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Subset 02 02- 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") # Place legent at the bottom

line_chart_batch_02
ggsave("plots_output/05_Unemp_temp_rate_line_chart_batch_02_selectec_countries_2003_2023.png", width = 6, height = 4)

