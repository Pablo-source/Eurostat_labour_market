# AIM: Set of functions to process and format data into Markdown report

# Ensure we have pacman installed to load all required packages at once
installed.packages()

if(!"pacman" %in% installed.packages()) install.packages("pacman")
## Load required packages now using pacman p_load function: 
pacman::p_load(here,dplyr,here,readxl,tidyr,ggplot2,stats)


# Helper Functions
# 1. First function - Builds path to look for Input (Excel) files
#    data_filepath()
# 2. Second helper function - Read in original Eurostat Excel files into R
#    Import_eurostat_indicators()
# 3. Format values for markdown
#    fmt_markdown_figures()

# File  name: helper_functions.R

# Building function to import files:

# 1. First function - Builds path to look for Input (Excel) files:

data_filepath  <- function(tab_name = NULL,choose_directory = NULL, own_directory = NULL){
  
  if(choose_directory == "data_folder") {
  data_folder_path = file.path(here("data"))  
  if (dir.exists(data_folder_path)) {
    return(data_folder_path)  
  }
  
  } else if (choose_directory == "data_cleansed") {
  data_cleansed_path = file.path(here("data_cleansed"))
  if (dir.exists(data_cleansed_path))  
  return(data_cleansed_path)
  } else { stop ("please provide your own directory")}
  
}

# Use function
data_filepath(choose_directory = "data_folder")
data_filepath(choose_directory = "data_cleansed")
data_filepath(choose_directory = "my directory") # This will trigger error message


# 2. Second helper function - Read in original Eurostat Excel files into R

# Eurostat: 
# LFS adjusted series:
#     lfsi_pt_a (Part-time employment and temporary contracts-annual data)
#     une_rt_a (Unemployment by sex and age - annual data). Time 23/23 (2003-2025)

# data is located in "Sheet 1"

Import_eurostat_indicators <- function(tab_name,choose_directory = NULL, selected_countries,indicator = NULL){

  # une_rt_a (Unemployment by sex and age - annual data). Time 23/23 (2003-2025)
  
  data_folder = here("data")
    if (indicator == "unemp"){
      
  # 1.1 arange original input data in Long format  
  unemp_raw <- read_excel(file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx"),
                          sheet = tab_name, col_names = TRUE, na = ":", skip = 8,n_max = 23) %>% 
              rename(Date = "GEO (Labels)") %>% 
              filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
              pivot_longer(!Date, names_to = "Countries", values_to = "metric_value") 
  
  unem_long <- unemp_raw %>% mutate(metric = "unemployment_rate", units = "percentage") %>% 
                             select(date = Date,country = Countries,metric_value, metric, units) %>% 
                             mutate(metric_value = as.numeric(metric_value)) %>% # TO COMPUTE CALCULATIONS metric_Value must be NUMERIC
                         filter(country %in% c(selected_countries))   #  filter initial data by selection of countries
  
  
  # 1.2 Include new variable to display lagged values (1year ago, 2 years ago, 5 years ago, grouped by country)
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
                   select(country,date,metric_value) %>%
    group_by(country) %>% 
  mutate(
          min_value_country = min(metric_value, na.rm = TRUE),
          max_value_country = max(metric_value, na.rm = TRUE)
          ) %>% 
    ungroup()
                
  # 1.5 Finally include min and max values entire unemp dataset
  unemp_long_min_max_all <- unemp_long_min_max %>% 
    mutate(
      min_value_indic = min(metric_value, na.rm = TRUE),
      max_value_indic = max(metric_value, na.rm = TRUE)
    )              
  
  
  # Return final selection of countries unemployment indicator values    
  return(unemp_long_min_max_all)
  
  } else if (indicator == "tempcontracts"){
  #     lfsi_pt_a (Part-time employment and temporary contracts-annual data)
  # Return final selection of countries temporary employment indicator  
  temp_emp_raw <- read_excel(file.path(data_folder,"lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx"),
                             sheet = tab_name, col_names = TRUE, na = ":", skip = 10, n_max = 22) %>% 
    rename(Date = "GEO (Labels)") %>% 
    filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
    pivot_longer(!Date, names_to = "Countries", values_to = "metric_value") 
  
  temp_long <- temp_emp_raw %>% mutate(metric = "temp_employment_rate", units = "percentage") %>% 
               select(date = Date,country = Countries,metric_value, metric, units) %>% 
    filter(country %in% c(selected_countries))   #  filter initial data by selection of countries
  
  # Return final selection of countries temporary employment indicator values  
    return(temp_long)
    }
  
}
# Parameters (tab_name = "Sheet 1", selcted_countries = c("country1","country2"))
Import_eurostat_indicators(tab_name = "Sheet 1", selected_countries = c('Bulgaria','Estonia','Ireland'),indicator = "unemp")
Import_eurostat_indicators(tab_name = "Sheet 1", selected_countries = c('Bulgaria','Estonia','Ireland'),indicator = "tempcontracts")

# 3. Format values for markdown
#    fmt_markdown_figures()

fmt_markdown_figures<- function(mydataset 
                                ,country, column,Date,format = NULL){
  row <- mydataset %>% filter({{country}}) 
  print(row)
  value <- row %>% pull({{column}})
  print(value)
  if (length(value)==0) {return(NA)}
  # numeric format (taken from original Markdown report) . see below
  # Example: prettyNum(Min_total_population$total_population, big.mark=",")
  # Start defining required formats for value
  if (format == "Numeric"){
      if(is.na(value)){
          return(NA_character_)
      } else if (!is.na(value)){
          value <- as.numeric(value)
          value <- prettyNum(value, big.mark=",")
          return(value)}
    
  } else if (format == "percent") {
     if(is.na(value)){
      return(NA_character_)
    } else if (!is.na(value)){
    } 
  } else
    # End of numeric format (taken from original Markdown report)
    # Latest return value - always return value as character as faisafe
  return(as.character(value))
}
# Testing fmt_markdown_figures function# Dataset: unemp_long_min_max_all # Country: Bulgaria
# Column: metric_value# Date: 2011
fmt_markdown_figures( mydataset = "unemp_long_min_max_all",Country = "Bulgaria",Column = "metric_value",
                      Date = "2011")
