# 05_Faceted_charts_custom_line_colour.R

# AIM: change default R colours (blue and red to each line charts data series)

# 1. Import previous files  b
library(here)
library(dplyr)
library(ggplot2)

# 1. Import combined indicators  
combined_indic  <- read.table(here("data_cleansed", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                              header =TRUE, sep =',',stringsAsFactors =TRUE)
head(combined_indic)

str(combined_indic)

## 1.1 Apply date format to "date" column
#  mutate(datef = as.Date(date)) %>% 
# select(date = datef,country,metric_name,metric_value)

combined_indic_date_fmtd <- read.table(here("data_cleansed", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                                       header =TRUE, sep =',',stringsAsFactors =TRUE) %>% 
                            mutate(datef = as.Date(date)) %>% 
                            select(datef, country,metric_name,metric_value)
str(combined_indic_date_fmtd)

# 2. Test changing line color just for two countries (spain, ireland)
Subset_test <-c("ireland","spain")

# 2.1 We load combined_indic dataframe and now we will change default line colours for chart including two indicators
# This is the DEFAULT colour assigned by R to each indicator: 

test_01_plot_data <- combined_indic_date_fmtd %>% 
  select(datef,country,metric_name,metric_value) %>% 
  filter(country %in% Subset_test) 
test_01_plot_data

line_chart_test_01 <- test_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Part Time Employment and unemployment in EU countries - Spain and Ireland - 2003-2023 period",
       subtitle ="Part time employment and unemployment rates (%)",
       caption = "Eurostat tables: Unemployment rate:[une_rt_a]:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en,
       ,Part-time employment [ lfsi_pt_a]:https://ec.europa.eu/eurostat/databrowser/view/lfsi_pt_a/default/table?lang=en",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom",   # Place legend at the bottom
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate X axis labels 90 degrees
        plot.title = element_text(face = "bold")) # Display main plot Title bold

line_chart_test_01

ggsave("plots_output/06_Unemp_temp_rate_line_Ireland_Spain_dafault_legeng_colour_01.png",  width = 12, height = 9.22)

# 2.2 Changing legend using scale_colour_manual() function 
# I use scale_colour_manual() with the parameters below to create a new legend with new colors and text displayed
#   scale_colour_manual(values = c("black","blue"),labels = c("Temporary contracts rate", "Unemployment rate")) 


line_chart_test_02 <- test_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Part Time Employment and unemployment in EU countries - Spain and Ireland - 2003-2023 period. Yearly data",
       subtitle ="Part time employment and unemployment rates (%)",
       caption = "Eurostat tables: Unemployment rate:[une_rt_a]:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en,
       ,Part-time employment [ lfsi_pt_a]:https://ec.europa.eu/eurostat/databrowser/view/lfsi_pt_a/default/table?lang=en",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom",   # Place legend at the bottom
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate X axis labels 90 degrees
        plot.title = element_text(face = "bold")) + # Display main plot Title bold
  # Apply legend format here - change default colours to black and blue
  scale_colour_manual(values = c("blue", "black"),
                      labels = c("Part Time employment rate (%)","Unemployment rate (%)"))
line_chart_test_02

ggsave("plots_output/07_Unemp_temp_rate_line_Ireland_Spain_custom_legeng_colour_01.png", width = 6, height = 4)
  

# 2 Plotting temporary employment and unemployment rates for first batch of EU countries

Subset_countries_01 <-c("euro_area_20_countries_from_2023","belgium","bulgaria","czechia","denmark","germany","estonia",
                        "ireland","greece","spain","france","croatia","italy","cyprus","latvia","lithuania","luxembourg",
                        "hungary")

Subset_01_plot_data <- combined_indic_date_fmtd %>% 
  select(datef,country,metric_name,metric_value) %>% 
  filter(country %in% Subset_countries_01) 


# First batch of countries
# Display line charts facets by country displaying each indicator as individual line for each country 
line_chart_batch_01 <- Subset_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title ="Part Time employment rate and unemployment in EU countries - Subset 01 02- 2003-2023 period. Yearly data",
       subtitle ="Part time employment and unemployment rates (%)",
       caption = "Eurostat tables: Unemployment rate:[une_rt_a]:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en,
       ,Part-time employment [ lfsi_pt_a]:https://ec.europa.eu/eurostat/databrowser/view/lfsi_pt_a/default/table?lang=en",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom",   # Place legend at the bottom
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate X axis labels 90 degrees 
        plot.title = element_text(face = "bold")) + # Display main plot Title bold
  # Apply legend format here - change default colours to black and blue
  scale_colour_manual(values = c("blue", "black"),
                      labels = c("Part Time employment rate (%)","Unemployment rate (%)"))

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

Subset_02_plot_data <- combined_indic_date_fmtd %>% 
  select(datef,country,metric_name,metric_value) %>% 
  filter(country %in% Subset_countries_02) 

# second batch of countries to be plotted with custom labels.
line_chart_batch_02 <- Subset_02_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title ="Part Time employment rate and unemployment in EU countries - Subset 02 02- 2003-2023 period. Yearly data",
       subtitle ="Part time employment and unemployment rates (%)",
       caption = "Eurostat tables: Unemployment rate:[une_rt_a]:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en,
       ,Part-time employment [ lfsi_pt_a]:https://ec.europa.eu/eurostat/databrowser/view/lfsi_pt_a/default/table?lang=en",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom",   # Place legend at the bottom
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate X axis labels 90 degrees
        plot.title = element_text(face = "bold")) + # Display main plot Title bold
  # Apply legend format here - change default colours to black and blue
  scale_colour_manual(values = c("blue", "black"),
                      labels = c("Part Time employment rate (%)","Unemployment rate (%)"))
line_chart_batch_02

ggsave("plots_output/09_Unemp_temp_rate_line_chart_batch_02_custom_legend_colour.png", width = 6, height = 4) 

# re-factored all charts on this script.