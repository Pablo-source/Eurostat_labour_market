# AIM: Set of functions to process and format data into Markdown report

# File  name: helper_functions.R

library(here)

# Building function to import files:

# 1. First function - Builds path to look for Input (Excel) files:

data_filepath  <- function(tab_name = NULL,choose_directory = NULL){
  
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

# data is located in "Sheet 1"
Import_eurostat_indicators <- function(tab_name,choose_directory = NULL, selected_countries,indicator = NULL){
  
  data_folder = here("data")
  
  unemp_raw <- read_excel(file.path(data_folder,"une_rt_a__custom_14324113_page_spreadsheet.xlsx"),
                          sheet = tab_name, col_names = TRUE, na = ":", skip = 8,n_max = 23) %>% 
    rename(Date = "GEO (Labels)") %>% 
    filter(!is.na(France)) %>%  # France has the highest number of populated rows only 1 NA
    pivot_longer(!Date, names_to = "Countries", values_to = "metric_value") 
  
  # Create new column to state metric_name, units and also allow to filter initial data by selection of countries
  unem_long <- unemp_raw %>% mutate(
    metric = "unemployment_rate",
    units = "thousands")
  # rename column so we can later on union it with other indicators
  # Filter previous dataset so we can select a handful of countries
  unemp_rate_countries_sel <- unem_long %>% select(date = Date,
                                                   country = Countries, 
                                                   metric_value,
                                                   metric, 
                                                   units) %>% 
    filter(country %in% c(selected_countries))
  # Return final selection of countries unemployment indicator values    
  return(unemp_rate_countries_sel)
  
}
# Parameters (tab_name = "Sheet 1", selcted_countries = c("country1","country2"))
Import_eurostat_indicators(tab_name = "Sheet 1", selected_countries = c('Bulgaria','Estonia','Ireland'),indicator = NULL)
