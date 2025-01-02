# R Script: 03_Combine_formated_indicators.R

# Combine cleansed files for Unemployment and Temporary indicators
# data_cleansed folder: 

# Get previous .csv files we created from /data_cleansed sub-folder
rm(list=ls()[! ls() %in% c("EU_UNEMP_CLEANSED_LONG",
                           "EU_TEMP_CLEANSED_LONG")])

list.files(path = "./data_cleansed", pattern = "csv$")

# > list.files(path = "./data_cleansed", pattern = "csv$")
# [1] "EU_TEMP_CLEANSED_lfsi_pt_a_LONG.csv" "EU_UNEMP_CLEAN_une_rt_a_LONG.csv" 


# 1. Import previous files 
library(tidyverse)
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


# 2. Test first on a small sample of countries. 
#    Combine both indicators together

# 2.1 Subset data just for three countries (spain,greece,france)

# spain,greece, france
Subset <-c("spain","greece","france")

# 2.2 Then create a new column to split each line chart by Indicator
# Also change each indicator original column name to a generic value as we are going to union them
unemp_data_check <- unemp_data %>% 
                    filter(country %in% Subset) %>% 
                    select(date,country,
                           value = unemp_rate) %>% 
                    mutate(indicator = "unemployment_rate")
unemp_data_check
str(unemp_data_check)

temporary_data_check <- temporary_data %>% 
                        filter(country %in% Subset) %>% 
                        select(date,country,
                               value = temporary_rate) %>% 
                        mutate(indicator = "temporary_rate")
temporary_data_check


names(unemp_data_check)
names(temporary_data_check)

head(unemp_data_check)
head(temporary_data_check)

# 2.3 Then we union them to create chart displaying both indicators combined
Sample_chart <- bind_rows(unemp_data_check,temporary_data_check)
View(Sample_chart)

Sample_chart_fmtd <- Sample_chart %>% 
                      mutate(datef = as.Date(date))
Sample_chart_fmtd

str(Sample_chart_fmtd)

Sample_chart_dates <- Sample_chart_fmtd %>% select(datef, country, value, indicator)

str(Sample_chart)
str(Sample_chart_dates)

# 3 Charts.

# 3.1 Box chart from combined indicators
# Split lines by indicator (unemployment_rate, temporary_rate)
# [1] "date"      "country"   "value"     "indicator"


# 3.1.1 Boxplot using date as factor
# data frame: Sample_chart
# date variable: date
box_plot_test <- Sample_chart %>% 
                 ggplot(aes(date,value, fill = indicator)) +
                 geom_boxplot() +
        labs(title = "Temporary Employment and unemployment in selected countries - 2003-2923 period. Yearly data",
        subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() 
box_plot_test

ggsave("plots_output/01_Unemp_temp_boxplot_sel_countries_dfactor_01.png", width = 6, height = 4)


# 3.1.2 Boxplot using date as date
# data frame: Sample_chart_dates
# date variable: datef

box_plot_test2 <- Sample_chart_dates %>% 
  ggplot(aes(datef,value, fill = indicator)) +
  geom_boxplot() +
  labs(title = "Temporary Employment and unemployment  rates - Average values - 2003-2923 period",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() 
box_plot_test2

ggsave("plots_output/02_Unemp_temp_boxplot_sel_countries_ddate_02.png", width = 6, height = 4)

# 3.2 Display line charts facets by country displaying each indicator as individual line for each country 
# data frame: Sample_chart_dates
# date variable: datef
#  Removed "scales = "free_y" to display SAME y axis scale across all three charts  

line_chart_test <- Sample_chart_dates %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in selected countries - 2003-2923 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
    y = NULL,colour = NULL, fill = NULL) +
#  theme (axis.ticks = element_blank()) 
  theme_light() +
  theme(plot.title.position = "plot")
line_chart_test

ggsave("plots_output/03_Unemp_temp_rate_selected_countries_line_chart_03.png", width = 6, height = 4)

# Extra formatting to this initial line chart
# scale_x_date(date_labels="%Y",date_breaks  ="1 year") +


  
  

