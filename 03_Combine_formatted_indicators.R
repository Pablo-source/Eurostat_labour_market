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

# 1.3 Combine above two existing dataframes “unem_data” and “temporary_data” 
# 13/07/2025 > into a single dataframe called EU_UNEMP_TEMP_INDICATORS

head(unemp_data)
head(temporary_data)

unemp_data_long <- unemp_data %>% mutate(metric_name = 'unemp_rate') %>% 
                        select(date,country,metric_name,metric_value = unemp_rate )
tail(unemp_data_long)

temporary_data_long <- temporary_data %>% mutate(metric_name = 'temporary_rate') %>% 
                        select(date,country,metric_name,metric_value = temporary_rate )

tail(unemp_data_long)
tail(temporary_data_long)

# Now I have all two initial dataframes in long format to combine them using bind_rows() function


EU_TEMP_UNEMP_COMBINED <- bind_rows(unemp_data_long,temporary_data_long)
nrow(unemp_data_long)
nrow(temporary_data_long)
nrow(EU_TEMP_UNEMP_COMBINED)

head(EU_TEMP_UNEMP_COMBINED)

# Sort unioned dataframe by date,metric_name_country using arrange() function from DPLYR
EU_TEMP_UNEMP_COMBINED_sorted <- EU_TEMP_UNEMP_COMBINED %>% 
                                 arrange(metric_name,date,country)

EU_TEMP_UNEMP_COMBINED_sorted_country <-EU_TEMP_UNEMP_COMBINED_sorted %>% 
  select(date,country,metric_name,metric_value) %>% 
                                        arrange(metric_name,country,date)

head(EU_TEMP_UNEMP_COMBINED_sorted_country)

# Then write output as .csv fie to \data_cleansed folder; 
EU_TEMP_UNEMP_COMBINED_SORTED <- EU_TEMP_UNEMP_COMBINED_sorted_country
write.csv(EU_TEMP_UNEMP_COMBINED_SORTED,here("data_cleansed","EU_TEMP_UNEMP_COMBINED_SORTED.csv"), row.names = TRUE)

# 2. Test first on a small sample of countries (Spain, Greece, France)

#  Using newly created combined data frame combining both indicators together
# 2.1 Subset data just for three countries (spain,greece,france)
# spain,greece, france
Subset <-c("spain","greece","france")

unemp_data_check <- EU_TEMP_UNEMP_COMBINED_SORTED %>% 
                    filter(country %in% Subset &
                           metric_name == 'unemp_rate')

temporary_data_check <- EU_TEMP_UNEMP_COMBINED_SORTED %>% 
  filter(country %in% Subset &
           metric_name == 'temporary_rate')


# 3 Charts.

# 3.1 Box chart from combined indicators
# Split lines by indicator (unemployment_rate, temporary_rate)
# [1] "date"      "country"   "value"     "indicator"

head(EU_TEMP_UNEMP_COMBINED_sorted_country)

# 3.1.1 Boxplot using date as factor
# data frame: Sample_chart
# date variable: date

str(EU_TEMP_UNEMP_COMBINED_sorted_country)

box_plot_test <- EU_TEMP_UNEMP_COMBINED_sorted_country %>% 
                 ggplot(aes(date,metric_value, fill = metric_name)) +
                 geom_boxplot() +
        labs(title = "Temporary Employment and unemployment in selected countries - 2003-2923 period. Yearly data",
        subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  # Rotate x axis labels 45 degrees
  theme(axis.text.x = element_text(angle = + 45, hjust = 0.5, vjust = 0.5)) 
box_plot_test

ggsave("plots_output/01_Unemp_temp_boxplot_sel_countries_dfactor_01.png", width = 6, height = 4)


# 3.1.2 Boxplot using date as date
# data frame: EU_TEMP_UNEMP_COMBINED_DATE_FMTD
# Introduced new calculation to turn factor date into 
# mutate(datef = as.Date(date))
# date variable: datef

EU_TEMP_UNEMP_COMBINED_DATE_FMTD <- EU_TEMP_UNEMP_COMBINED_sorted_country %>% 
  mutate(datef = as.Date(date)) %>% 
  select(datef,country,metric_name,metric_value)

str(EU_TEMP_UNEMP_COMBINED_DATE_FMTD)

box_plot_metrics_date_fmtd <- EU_TEMP_UNEMP_COMBINED_DATE_FMTD %>% 
  ggplot(aes(datef,metric_value, fill = metric_name)) +
  geom_boxplot() +
  labs(title = "Temporary Employment and unemployment  rates - Average values - 2003-2923 period",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() 
box_plot_metrics_date_fmtd

ggsave("plots_output/02_Unemp_temp_boxplot_sel_countries_ddate_02.png", width = 6, height = 4)

# 3.2 Display line charts facets by country displaying each indicator as individual line for each country 
# data frame: Sample_chart_dates
# date variable: datef
#  Removed "scales = "free_y" to display SAME y axis scale across all three charts  

head(EU_TEMP_UNEMP_COMBINED_DATE_FMTD)

line_chart_test <- EU_TEMP_UNEMP_COMBINED_DATE_FMTD %>% 
  ggplot( fill = metric_name) +
  geom_line(aes(datef,metric_value,colour = metric_name, group = metric_name)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in selected countries - 2003-2923 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
    y = NULL,colour = NULL, fill = NULL) +
#  theme (axis.ticks = element_blank()) 
  theme_light() +
  theme(plot.title.position = "plot") +
  # Rotate x axis labels 45 degrees
  theme(axis.text.x = element_text(angle = + 45, hjust = 0.5, vjust = 0.5)) 
line_chart_test

ggsave("plots_output/03_Unemp_temp_rate_selected_countries_line_chart_03.png", width = 6, height = 4)


  

