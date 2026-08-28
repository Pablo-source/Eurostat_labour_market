Eurostat labour market trends
================
PLR
2026-08-28

## Latest date this report was produced

Today’s date is **28 August 2026**. This report was published on the
week starting on **26 August 2026**.

## Populating report using helper functions

In this report we will use a set of helper functions to create initial
data sets for selected countries. And also to format each figure used in
the Markdwon report.

### 1. Ingest raw Eurostat downloaded data into R performing data wrangling

This first section takes the raw Excel file just downloaded from
Eurostat and applying several formatting options to get it ready for
using it as input data for ggplot2 charts: a) renmoves null values, b)
pivots data from wide to long format, c) creates required variables
(date_1y_ago, value_1y_ago..) for plots, d) Allows users to filters data
for selected countries and indicators, among other things.

This function can be modified to include extra arguments, below shows of
to format “unemploymen” raw data indicator, and also I applying a
similar approach to “part_time_persons” the second indicator downloaded
from Eurostat.

``` r
# I need to place all my Functions in this chunck to be used in the report !!! 
# So I can render the Markdown report !!!
# FUNCTION 01 02 - Import indicators
Import_eurostat_indicators <- function(tab_name,choose_directory = NULL, selected_countries,indicator = NULL){

  # une_rt_a (Unemployment by sex and age - annual data). Time 23/23 (2003-2025)
  if (indicator == "unemp"){
  # 1.1 arange original input data in Long format  
  unemp_raw <- read_excel(file.path(here::here(), "data","une_rt_a__custom_14324113_page_spreadsheet.xlsx"),
                              sheet = tab_name, col_names = TRUE, na = ":", skip = 8,n_max = 23) %>% 
              filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
              pivot_longer(!Date, names_to = "Countries", values_to = "metric_value") 
  unem_long <- unemp_raw %>% mutate(metric = "unemployment_rate", units = "percentage") %>% 
                             select(date = Date,country = Countries,metric_value, metric, units) %>% 
                             mutate(metric_value = as.numeric(metric_value)) %>% # to compute calculations metric_Value must be numeric
                         filter(country %in% c(selected_countries))   #  filter initial data by selection of countries
  # 1.2 New variable - lagged values (1year ago, 2 years ago, 5 years ago, grouped by country)
  # date_1y_ago, value_1y_ago, date_5y_ago, value_5y_ago
  unem_long_lags <- unem_long %>% 
                    arrange(country,date) %>% 
                    group_by(country) %>% 
                    mutate(
                      date_1y_ago = lag(date,1),
                      value_1y_ago = lag(metric_value,1),
                      date_2y_ago = lag(date,2),
                      value_2y_ago = lag(metric_value,2),
                      date_5y_ago = lag(date,5),
                      value_5y_ago = lag(metric_value,5)
                      ) %>% 
                    ungroup()
  # 1.3 Add new set of columns to display min and max values BY COUNTRY
  unemp_long_min_max<- unem_long_lags %>%
                   select(country,date,metric_value,metric,units) %>%
    group_by(country) %>% 
  mutate(
          min_value_country = min(metric_value, na.rm = TRUE),
          max_value_country = max(metric_value, na.rm = TRUE)
          ) %>% 
    ungroup()
  # 1.5 Finally include min and max values entire unemp dataset
  unempl_all <- unemp_long_min_max %>% 
    mutate(
      min_value_indic = min(metric_value, na.rm = TRUE),
      max_value_indic = max(metric_value, na.rm = TRUE)
    )              
  
  # 1.6 Ensure final output from function is a data.frame() object
  unemp_long_dataframe <- data.frame(unempl_all)
  
  # 1.7 Write dataframe to "data_cleansed_folder"
  filename <- "country_sel_unemp_long_dataframe.csv"
  output_file <- file.path("data_cleansed",filename)   # Using file.path() to build relative path to data_cleansed sub-folder. works on Windows, Linux, and macOS.
  write.csv(unemp_long_dataframe,file = output_file,row.names = FALSE)
  cat("File saved as:", output_file, "\n") # Write  message on Console everytime the output file is written to .csv and saved to "data_cleansed" sub-folder
  
  # Return final selection of countries unemployment indicator values    
  return(unemp_long_dataframe)
  
  } else if (indicator == "part_time_persons"){
  # Downloaded table data: lfsi_pt_a (Part-time employment and temporary contracts-annual data)
  # Return final selection of countries temporary employment figures
  part_time_emp_raw <- read_excel(file.path(here::here(),"data","lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx"),
                             sheet = tab_name, col_names = TRUE, na = ":", skip = 10, n_max = 22) %>% 
    filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
    pivot_longer(!Date, names_to = "Countries", values_to = "metric_value") 
  
  part_time_long <- part_time_emp_raw %>% mutate(metric = "per_persons_working_pat_time", units = "percentage") %>% 
               select(date = Date,country = Countries,metric_value, metric, units) %>% 
    filter(country %in% c(selected_countries))   #  filter initial data by selection of countries
  
  # Return final selection of countries temporary employment indicator values  
part_time_long_lags <- part_time_long %>% 
    arrange(country,date) %>% 
    group_by(country) %>% 
    mutate(
      date_1y_ago = lag(date,1),
      value_1y_ago = lag(metric_value,1),
      date_2y_ago = lag(date,2),
      value_2y_ago = lag(metric_value,2),
      date_5y_ago = lag(date,5),
      value_5y_ago = lag(metric_value,5)
    ) %>% 
    ungroup()

part_time_long_min_max<- part_time_long_lags %>%
  select(country,date,metric_value,metric,units) %>%
  group_by(country) %>% 
  mutate(
    min_value_country = min(metric_value, na.rm = TRUE),
    max_value_country = max(metric_value, na.rm = TRUE)
  ) %>% 
  ungroup()
# 1.5 Finally include min and max values entire unemp dataset
part_time_all <- part_time_long_min_max %>% 
  mutate(
    min_value_indic = min(metric_value, na.rm = TRUE),
    max_value_indic = max(metric_value, na.rm = TRUE)
  )              
# 1.6 Ensure final temp_emp dataframe output from function is a data.frame() object
temp_emp_long_dataframe <- data.frame(part_time_all)

return(temp_emp_long_dataframe)
    }
  
}
# Parameters (tab_name = "Sheet 1", selcted_countries = c("country1","country2","country3"), indicator ="unemp/part_time_persons")
Import_eurostat_indicators(tab_name = "Sheet 1", selected_countries = c('Bulgaria','Estonia','Ireland'),indicator = "unemp")
```

``` r
# Function 02 02 - declares path to data folder where formatted input data is saved  

# FUNCTION 02 - Declare filepath to "data_cleansed" folder
data_filepath  <- function(tab_name = NULL,choose_directory = NULL, own_directory = NULL){
  
  if(choose_directory == "data_folder") {
    data_folder_path = file.path(here::here(), "data") 
    if (dir.exists(data_folder_path)) {return(data_folder_path)}
  } else if (choose_directory == "data_cleansed") {
    data_cleansed_path = file.path(here::here(),"data_cleansed")
    if (dir.exists(data_cleansed_path)) {return(data_cleansed_path)}
  } else { stop ("please provide your own directory")}
  # Include details about user directory
  if (dir.exists(own_directory)){return(own_directory)}
  else{stop("Please ensure you provide your own_directory",own_directory)}
  
}

# Use function
data_filepath(choose_directory = "data_cleansed") 
```

    ## [1] "/home/pablo-nostromo/Documents/popos_pablo/R_github/Eurostat_labour_market/data_cleansed"

``` r
data_filepath(choose_directory = "data_cleansed")

dataset_sel_countries <- fread("data_cleansed/country_sel_unemp_long_dataframe.csv")
dataset_sel_countries
```

Now we use function from previous r code chunch called
“Import_eurostat_indicators()” to subset unemployment indicator data for
a selection of countries (“Bulgaria”,“Estonia” and “Ireland”)

Besides, in this section we can see how to populate text using
“fmt_markdown_figures” function below to apply specific format types
(numeric values including thousand separators, and percentage values
displaying the “%” sign) when describing figures in the rendered
markdown text output file.

- Now I include a new function in the section below to display
  unemployment figures using inline R code with specific formats.
