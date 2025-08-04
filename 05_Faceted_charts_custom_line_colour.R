# 05_Faceted_charts_custom_line_colour.R

# AIM: change default R colours (blue and red to each line charts data series)

# 1. Import previous files  b
library(here)
library(dplyr)

# 1. Import combined indicators  
combined_indic  <- read.table(here("data_cleansed", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                              header =TRUE, sep =',',stringsAsFactors =TRUE)
head(combined_indic)


# 2. Test changing line color just for two countries (spain, ireland)
Subset_test <-c("ireland","spain")

# 2.1 We load combined_indic dataframe and now we will change default line colours for chart including two indicators
# This is the DEFAULT colour assigned by R to each indicator: 

test_01_plot_data <- combined_indic %>% 
  select(date,country,metric_name,metric_value) %>% 
  filter(country %in% Subset_test) 
test_01_plot_data

line_chart_test_01 <- test_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Spain and Ireland - 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") # Place legent at the bottom

line_chart_test_01

ggsave("plots_output/06_Unemp_temp_rate_line_Ireland_Spain_dafault_legeng_colour_01.png", width = 6, height = 4)

# 1.1 Changing legend using scale_colour_manual() function 

# I use scale_colour_manual() with the parameters below to create a new legend with new colours and text displayed

#   scale_colour_manual(
# values = c("black","blue"),
# labels = c("Temporary contracts rate", "Unemployment rate")) 


line_chart_test_02 <- test_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Spain and Ireland - 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") + # Place legent at the bottom
  # Apply legend format here - change default colours to black and blue
  scale_colour_manual(values = c("blue", "black"),
                      labels = c("Temporary contracts rate (%)","Unemployment rate (%)"))
line_chart_test_02

ggsave("plots_output/07_Unemp_temp_rate_line_Ireland_Spain_custom_legeng_colour_01.png", width = 6, height = 4)
  

# 2 Plotting temporary employment and unemployment rates for first batch of EU countries

Subset_countries_01 <-c("euro_area_20_countries_from_2023","belgium","bulgaria","czechia","denmark","germany","estonia",
                        "ireland","greece","spain","france","croatia","italy","cyprus","latvia","lithuania","luxembourg",
                        "hungary")

Subset_01_plot_data <- all_indicators_datef %>% 
  select(date,datef,country,value,indicator) %>% 
  filter(country %in% Subset_countries_01) 


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
        legend.position = "bottom") + # Place legend at the bottom
  # Apply legend format here - change default colours to black and blue
  scale_colour_manual(values = c("blue", "black"),
                      labels = c("Temporary contracts rate (%)","Unemployment rate (%)"))
line_chart_batch_01

ggsave("plots_output/08_Unemp_temp_rate_line_chart_batch_01_custom_legend_colour.png", width = 6, height = 4)

# Online Resources with interesting GGPLOT2 tips
# Using the ggplot theme function to customize facet labels and your legend (CC067)
# Author: Riffomonas Project
# URL: https://www.youtube.com/watch?v=-4NQgIReefQ

# 3 Plotting temporary employment and unemployment rates for second batch of Eu countries

Subset_countries_02 <-c("euro_area_20_countries_from_2023","malta","netherlands","austria",
                        "poland","portugal","romania","slovenia","slovakia","finland","sweden",
                        "iceland","norway","switzerland","bosnia_and_herzegovina","montenegro",
                        "north_macedonia","serbia","turkiye")

Subset_02_plot_data <- all_indicators_datef %>% 
  select(date,datef,country,value,indicator) %>% 
  filter(country %in% Subset_countries_02)

# second batch of countries to be plotted with custom labels.
line_chart_batch_02 <- Subset_02_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Subset 02 02- 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") + # Place legend at the bottom
# Apply legend format here - change default colours to black and blue
scale_colour_manual(values = c("blue", "black"),
                    labels = c("Temporary contracts rate (%)","Unemployment rate (%)"))
line_chart_batch_02
ggsave("plots_output/09_Unemp_temp_rate_line_chart_batch_02_custom_legend_colour.png", width = 6, height = 4)