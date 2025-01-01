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

# 3.3 Then we union them to create chart displaying both indicators combined
Sample_chart <- bind_rows(unemp_data_check,temporary_data_check)
view(Sample_chart)


# 3.4 Turn previous Factor date into a proper Date variable
Sample_chart_dates <- Sample_chart %>% 
                      mutate(datef = as.Date(date))
Sample_chart_dates

str(Sample_chart_dates)

# 3 Charts.

# 3.1 Box chart from combined indicators
# Split lines by indicator (unemployment_rate, temporary_rate)
names(Sample_chart)

# [1] "date"      "country"   "value"     "indicator"

box_plot_test <- Sample_chart_dates %>% 
                 ggplot(aes(datef,value, fill = indicator)) +
                 geom_boxplot()
box_plot_test
  
# 3.2 Display line charts facets by country displaying each indicator as individual line for each country 
line_chart_test <- Sample_chart_dates %>% 
  ggplot() +
  geom_line(aes(datef,value, fill = indicator,colour = indicator, group = indicator)) +
  facet_wrap(~ country, scales = "free_y", nrow = 2) +
  labs(title = "Temporary Employment and unemployment in selected countries - 2003-2923 period. Yearly data",
       subtitle ="Source: https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
    y = NULL,colour = NULL, fill = NULL) +
#  theme (axis.ticks = element_blank()) 
  theme_light()


line_chart_test
  
# 3.3 Extra formatting to this initial line chart
#   scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
line_chart_test02 <- Sample_chart %>% 
  ggplot() +
  geom_line(aes(date,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, scales = "free_y", nrow = 2) +
  labs(title = "Temporary Employment and unemployment in selected countries - 2003-2923 period. Yearly data",
       subtitle ="Source: https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
       y = NULL,colour = NULL, fill = NULL) +
  #  theme (axis.ticks = element_blank()) 
  theme_light() 
line_chart_test02

  
  

