Recent Spain population trends
================
PLR
2026-07-12

## Latest date this report was produced

Today’s date is **12 July 2026**. This report was published on the week
starting on **10 July 2026**.

## 1. Load Spain population data

We load latest Spanish Population Figures released by the Spanish
National Institute: <https://www.ine.es/consul/serie.do?d=true&s=ECP320>

In the third tab from Excel file **“INE total and foreign population
figures Spain.xlsx”** we load the table describing Total, foreign and
nationals population figures in Spain, based on Spanish nationality for
the 2004 2025 period.

    ## [1] "01 Spain components pop change_data_prep.xlsx"                    
    ## [2] "INE population by nationality Spanish foreign 2002 2025.xlsx"     
    ## [3] "INE total and foreign population figures Spain.xlsx"              
    ## [4] "INE_Foreign_Born_Residents_in_Spain_by_Nationality_1998_2022.xlsx"
    ## [5] "INE_natural_increase_births_deaths_1992_2024.xlsx"                
    ## [6] "INE_Total_resident_population_by_nationality_1998_2022.xlsx"

    ## [1] "INE_Foreign_population"       "INE_Total_population"        
    ## [3] "INE_Total_foreign_population"

``` r
population_data <- read_excel(
  "data_demography/INE total and foreign population figures Spain.xlsx",
  sheet = "INE_Total_foreign_population", skip = 2, n_max = 22,
  col_types = c("guess", "numeric", "numeric", "guess","guess","guess","guess","guess")) %>% 
  clean_names() %>% 
       select (date = todas_las_edades, total_population = total,
                       foreign_population, percent_foreign_population = percent_foreign_nationals_total_population,
                      total_population_YoY_N = total_yo_y_n, total_population_YoY_perc = total_yo_y_percent,
                       foreign_population_YoY_N = foreign_nationals_yo_y_n, foreign_population_YoY_perc= foreign_total_yo_y_percent)

head(population_data)
```

    ## # A tibble: 6 × 8
    ##   date               total_population foreign_population percent_foreign_popul…¹
    ##   <chr>                         <dbl>              <dbl>                   <dbl>
    ## 1 1 de enero de 2025         49077984            6852348                   0.140
    ## 2 1 de enero de 2024         48619695            6502282                   0.134
    ## 3 1 de enero de 2023         48085361            6089620                   0.127
    ## 4 1 de enero de 2022         47486727            5509046                   0.116
    ## 5 1 de enero de 2021         47400798            5402702                   0.114
    ## 6 1 de enero de 2020         47318050            5241278                   0.111
    ## # ℹ abbreviated name: ¹​percent_foreign_population
    ## # ℹ 4 more variables: total_population_YoY_N <dbl>,
    ## #   total_population_YoY_perc <dbl>, foreign_population_YoY_N <dbl>,
    ## #   foreign_population_YoY_perc <dbl>

``` r
names(population_data)
```

    ## [1] "date"                        "total_population"           
    ## [3] "foreign_population"          "percent_foreign_population" 
    ## [5] "total_population_YoY_N"      "total_population_YoY_perc"  
    ## [7] "foreign_population_YoY_N"    "foreign_population_YoY_perc"

## 2. Exploratory charts Total, Spanish nationals and foreign population in Spain

From the newly on boarded data, we create a new Year variable from
initial date column. As we only want to display Year values.

``` r
INE_population_subset <- population_data %>%
                         select(date,total_population,foreign_population) %>% 
                         mutate(Year = substring(date, 15, 25)) 
INE_population_subset
```

    ## # A tibble: 22 × 4
    ##    date               total_population foreign_population Year 
    ##    <chr>                         <dbl>              <dbl> <chr>
    ##  1 1 de enero de 2025         49077984            6852348 2025 
    ##  2 1 de enero de 2024         48619695            6502282 2024 
    ##  3 1 de enero de 2023         48085361            6089620 2023 
    ##  4 1 de enero de 2022         47486727            5509046 2022 
    ##  5 1 de enero de 2021         47400798            5402702 2021 
    ##  6 1 de enero de 2020         47318050            5241278 2020 
    ##  7 1 de enero de 2019         46918951            4850762 2019 
    ##  8 1 de enero de 2018         46645070            4577322 2018 
    ##  9 1 de enero de 2017         46497393            4417653 2017 
    ## 10 1 de enero de 2016         46418884            4419334 2016 
    ## # ℹ 12 more rows

``` r
INE_calc_fields <- INE_population_subset %>%                          
                         select(Year,total_population,foreign_population) %>% 
                         mutate(Spanish_nationals = total_population - foreign_population) %>% 
                         arrange(Year)
INE_calc_fields
```

    ## # A tibble: 22 × 4
    ##    Year  total_population foreign_population Spanish_nationals
    ##    <chr>            <dbl>              <dbl>             <dbl>
    ##  1 2004          42547454                 NA                NA
    ##  2 2005          43296335            3430204          39866131
    ##  3 2006          44009969            3930916          40079053
    ##  4 2007          44784659            4449434          40335225
    ##  5 2008          45668938            5086295          40582643
    ##  6 2009          46239271            5386659          40852612
    ##  7 2010          46486621            5402579          41084042
    ##  8 2011          46667175            5312440          41354735
    ##  9 2012          46818216            5236030          41582186
    ## 10 2013          46712650            5064584          41648066
    ## # ℹ 12 more rows

These charts below describe the evolution of Total, foreign and spanish
nationals population for the 2005-2025 period:

- Spain total population

``` r
options(scipen=999)

Spanish_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = total_population)) +
  geom_bar(stat = "identity", fill = "darkolivegreen2") +
  labs(title = "Spain total poulation. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = total_population),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,55000000), expand = FALSE )

Spanish_population_plot
```

![](Recent-population-trends-in-Spain-wip_files/figure-gfm/Spain%20total%20population%20bar%20plot-1.png)<!-- -->

Then this is the foreign population in Spain for the same time period.

``` r
options(scipen=999)

forign_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = foreign_population)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Foreign population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = foreign_population),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,7500000), expand = FALSE )

forign_population_plot
```

![](Recent-population-trends-in-Spain-wip_files/figure-gfm/Spain%20foreign%20population%20bar%20plot-1.png)<!-- -->

This last plot displays national population in Spain for the 2005-2025
period

``` r
options(scipen=999)

national_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = Spanish_nationals)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Spanish nationals population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = Spanish_nationals),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,46000000), expand = FALSE )

national_population_plot
```

![](Recent-population-trends-in-Spain-wip_files/figure-gfm/Spain%20national%20population%20bar%20plot-1.png)<!-- -->

## 3. Spain population figures Overveiw details

In this section we highlight a couple of trends observed in both Total
and foreign population in Spain from 2004 until 2025 period.

- As of 1st of January **2025**., the year for the latest available
  population figures, Total population in Spain was **49,077,984**. Up
  by 458,253 from previous year.

- In contrast, on the year **2004**, the first year on this series,
  total population in Spain was **42,547,454**.

- In terms of foreign population, on the following year **2005**.there
  was a foreign population of **3,430,204**

- At the end of the series on 1st January **2025**, latest foreign
  population figures in Spain was **6,852,348**. We will describe these
  population changes in absolute figures and percent change in the next
  section below.

### 3.1 Share of foreign population over total population in Spain

Next, we describe the foreign population percent share of the total
population in Spain for the 2004-2025 period.

## 4. Populating report using helper functions

In this section we will use a set of helper functions to create initial
data sets for selected countries. And also to format each figure used in
the Markdwon report.

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
# 02 Function - declares path to data folder where formatted input data is saved  

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
```

    ## [1] "/home/pablo-nostromo/Documents/popos_pablo/R_github/Eurostat_labour_market/data_cleansed"

``` r
dataset_sel_countries <- fread("data_cleansed/country_sel_unemp_long_dataframe.csv")
dataset_sel_countries
```

    ##      country  date metric_value            metric      units min_value_country
    ##       <char> <int>        <num>            <char>     <char>             <num>
    ##  1: Bulgaria  2003           NA unemployment_rate percentage               4.2
    ##  2: Bulgaria  2004           NA unemployment_rate percentage               4.2
    ##  3: Bulgaria  2005           NA unemployment_rate percentage               4.2
    ##  4: Bulgaria  2006           NA unemployment_rate percentage               4.2
    ##  5: Bulgaria  2007           NA unemployment_rate percentage               4.2
    ##  6: Bulgaria  2008           NA unemployment_rate percentage               4.2
    ##  7: Bulgaria  2009          7.9 unemployment_rate percentage               4.2
    ##  8: Bulgaria  2010         11.3 unemployment_rate percentage               4.2
    ##  9: Bulgaria  2011         12.3 unemployment_rate percentage               4.2
    ## 10: Bulgaria  2012         13.3 unemployment_rate percentage               4.2
    ## 11: Bulgaria  2013         13.9 unemployment_rate percentage               4.2
    ## 12: Bulgaria  2014         12.4 unemployment_rate percentage               4.2
    ## 13: Bulgaria  2015         10.1 unemployment_rate percentage               4.2
    ## 14: Bulgaria  2016          8.6 unemployment_rate percentage               4.2
    ## 15: Bulgaria  2017          7.2 unemployment_rate percentage               4.2
    ## 16: Bulgaria  2018          6.2 unemployment_rate percentage               4.2
    ## 17: Bulgaria  2019          5.2 unemployment_rate percentage               4.2
    ## 18: Bulgaria  2020          6.1 unemployment_rate percentage               4.2
    ## 19: Bulgaria  2021          5.2 unemployment_rate percentage               4.2
    ## 20: Bulgaria  2022          4.2 unemployment_rate percentage               4.2
    ## 21: Bulgaria  2023          4.3 unemployment_rate percentage               4.2
    ## 22:  Estonia  2003           NA unemployment_rate percentage               4.2
    ## 23:  Estonia  2004           NA unemployment_rate percentage               4.2
    ## 24:  Estonia  2005           NA unemployment_rate percentage               4.2
    ## 25:  Estonia  2006           NA unemployment_rate percentage               4.2
    ## 26:  Estonia  2007           NA unemployment_rate percentage               4.2
    ## 27:  Estonia  2008           NA unemployment_rate percentage               4.2
    ## 28:  Estonia  2009         13.5 unemployment_rate percentage               4.2
    ## 29:  Estonia  2010         16.6 unemployment_rate percentage               4.2
    ## 30:  Estonia  2011         12.3 unemployment_rate percentage               4.2
    ## 31:  Estonia  2012          9.9 unemployment_rate percentage               4.2
    ## 32:  Estonia  2013          8.6 unemployment_rate percentage               4.2
    ## 33:  Estonia  2014          7.3 unemployment_rate percentage               4.2
    ## 34:  Estonia  2015          6.4 unemployment_rate percentage               4.2
    ## 35:  Estonia  2016          6.8 unemployment_rate percentage               4.2
    ## 36:  Estonia  2017          5.8 unemployment_rate percentage               4.2
    ## 37:  Estonia  2018          5.4 unemployment_rate percentage               4.2
    ## 38:  Estonia  2019          4.5 unemployment_rate percentage               4.2
    ## 39:  Estonia  2020          6.9 unemployment_rate percentage               4.2
    ## 40:  Estonia  2021          6.2 unemployment_rate percentage               4.2
    ## 41:  Estonia  2022          5.6 unemployment_rate percentage               4.2
    ## 42:  Estonia  2023          6.4 unemployment_rate percentage               4.2
    ## 43:  Ireland  2003           NA unemployment_rate percentage               4.2
    ## 44:  Ireland  2004           NA unemployment_rate percentage               4.2
    ## 45:  Ireland  2005           NA unemployment_rate percentage               4.2
    ## 46:  Ireland  2006           NA unemployment_rate percentage               4.2
    ## 47:  Ireland  2007           NA unemployment_rate percentage               4.2
    ## 48:  Ireland  2008           NA unemployment_rate percentage               4.2
    ## 49:  Ireland  2009         12.6 unemployment_rate percentage               4.2
    ## 50:  Ireland  2010         14.6 unemployment_rate percentage               4.2
    ## 51:  Ireland  2011         15.4 unemployment_rate percentage               4.2
    ## 52:  Ireland  2012         15.5 unemployment_rate percentage               4.2
    ## 53:  Ireland  2013         13.8 unemployment_rate percentage               4.2
    ## 54:  Ireland  2014         11.9 unemployment_rate percentage               4.2
    ## 55:  Ireland  2015          9.9 unemployment_rate percentage               4.2
    ## 56:  Ireland  2016          8.4 unemployment_rate percentage               4.2
    ## 57:  Ireland  2017          6.7 unemployment_rate percentage               4.2
    ## 58:  Ireland  2018          5.8 unemployment_rate percentage               4.2
    ## 59:  Ireland  2019          5.0 unemployment_rate percentage               4.2
    ## 60:  Ireland  2020          5.9 unemployment_rate percentage               4.2
    ## 61:  Ireland  2021          6.2 unemployment_rate percentage               4.2
    ## 62:  Ireland  2022          4.5 unemployment_rate percentage               4.2
    ## 63:  Ireland  2023          4.3 unemployment_rate percentage               4.2
    ##      country  date metric_value            metric      units min_value_country
    ##       <char> <int>        <num>            <char>     <char>             <num>
    ##     max_value_country min_value_indic max_value_indic
    ##                 <num>           <num>           <num>
    ##  1:              16.6             4.2            16.6
    ##  2:              16.6             4.2            16.6
    ##  3:              16.6             4.2            16.6
    ##  4:              16.6             4.2            16.6
    ##  5:              16.6             4.2            16.6
    ##  6:              16.6             4.2            16.6
    ##  7:              16.6             4.2            16.6
    ##  8:              16.6             4.2            16.6
    ##  9:              16.6             4.2            16.6
    ## 10:              16.6             4.2            16.6
    ## 11:              16.6             4.2            16.6
    ## 12:              16.6             4.2            16.6
    ## 13:              16.6             4.2            16.6
    ## 14:              16.6             4.2            16.6
    ## 15:              16.6             4.2            16.6
    ## 16:              16.6             4.2            16.6
    ## 17:              16.6             4.2            16.6
    ## 18:              16.6             4.2            16.6
    ## 19:              16.6             4.2            16.6
    ## 20:              16.6             4.2            16.6
    ## 21:              16.6             4.2            16.6
    ## 22:              16.6             4.2            16.6
    ## 23:              16.6             4.2            16.6
    ## 24:              16.6             4.2            16.6
    ## 25:              16.6             4.2            16.6
    ## 26:              16.6             4.2            16.6
    ## 27:              16.6             4.2            16.6
    ## 28:              16.6             4.2            16.6
    ## 29:              16.6             4.2            16.6
    ## 30:              16.6             4.2            16.6
    ## 31:              16.6             4.2            16.6
    ## 32:              16.6             4.2            16.6
    ## 33:              16.6             4.2            16.6
    ## 34:              16.6             4.2            16.6
    ## 35:              16.6             4.2            16.6
    ## 36:              16.6             4.2            16.6
    ## 37:              16.6             4.2            16.6
    ## 38:              16.6             4.2            16.6
    ## 39:              16.6             4.2            16.6
    ## 40:              16.6             4.2            16.6
    ## 41:              16.6             4.2            16.6
    ## 42:              16.6             4.2            16.6
    ## 43:              16.6             4.2            16.6
    ## 44:              16.6             4.2            16.6
    ## 45:              16.6             4.2            16.6
    ## 46:              16.6             4.2            16.6
    ## 47:              16.6             4.2            16.6
    ## 48:              16.6             4.2            16.6
    ## 49:              16.6             4.2            16.6
    ## 50:              16.6             4.2            16.6
    ## 51:              16.6             4.2            16.6
    ## 52:              16.6             4.2            16.6
    ## 53:              16.6             4.2            16.6
    ## 54:              16.6             4.2            16.6
    ## 55:              16.6             4.2            16.6
    ## 56:              16.6             4.2            16.6
    ## 57:              16.6             4.2            16.6
    ## 58:              16.6             4.2            16.6
    ## 59:              16.6             4.2            16.6
    ## 60:              16.6             4.2            16.6
    ## 61:              16.6             4.2            16.6
    ## 62:              16.6             4.2            16.6
    ## 63:              16.6             4.2            16.6
    ##     max_value_country min_value_indic max_value_indic
    ##                 <num>           <num>           <num>

Now we use function from previous r code chunch called
“Import_eurostat_indicators()” to subset unemployment indicator data for
a selection of countries (“Bulgaria”,“Estonia” and “Ireland”)

Besides, in this section we can see how to populate text using
“fmt_markdown_figures” function below to apply specific format types
(numeric values including thousand separators, and percentage values
displaying the “%” sign) when describing figures in the rendered
markdown text output file.

    ## File saved as: data_cleansed/country_sel_unemp_long_dataframe.csv

    ##     country date metric_value            metric      units min_value_country
    ## 1  Bulgaria 2003           NA unemployment_rate percentage               4.2
    ## 2  Bulgaria 2004           NA unemployment_rate percentage               4.2
    ## 3  Bulgaria 2005           NA unemployment_rate percentage               4.2
    ## 4  Bulgaria 2006           NA unemployment_rate percentage               4.2
    ## 5  Bulgaria 2007           NA unemployment_rate percentage               4.2
    ## 6  Bulgaria 2008           NA unemployment_rate percentage               4.2
    ## 7  Bulgaria 2009          7.9 unemployment_rate percentage               4.2
    ## 8  Bulgaria 2010         11.3 unemployment_rate percentage               4.2
    ## 9  Bulgaria 2011         12.3 unemployment_rate percentage               4.2
    ## 10 Bulgaria 2012         13.3 unemployment_rate percentage               4.2
    ## 11 Bulgaria 2013         13.9 unemployment_rate percentage               4.2
    ## 12 Bulgaria 2014         12.4 unemployment_rate percentage               4.2
    ## 13 Bulgaria 2015         10.1 unemployment_rate percentage               4.2
    ## 14 Bulgaria 2016          8.6 unemployment_rate percentage               4.2
    ## 15 Bulgaria 2017          7.2 unemployment_rate percentage               4.2
    ## 16 Bulgaria 2018          6.2 unemployment_rate percentage               4.2
    ## 17 Bulgaria 2019          5.2 unemployment_rate percentage               4.2
    ## 18 Bulgaria 2020          6.1 unemployment_rate percentage               4.2
    ## 19 Bulgaria 2021          5.2 unemployment_rate percentage               4.2
    ## 20 Bulgaria 2022          4.2 unemployment_rate percentage               4.2
    ## 21 Bulgaria 2023          4.3 unemployment_rate percentage               4.2
    ## 22  Estonia 2003           NA unemployment_rate percentage               4.2
    ## 23  Estonia 2004           NA unemployment_rate percentage               4.2
    ## 24  Estonia 2005           NA unemployment_rate percentage               4.2
    ## 25  Estonia 2006           NA unemployment_rate percentage               4.2
    ## 26  Estonia 2007           NA unemployment_rate percentage               4.2
    ## 27  Estonia 2008           NA unemployment_rate percentage               4.2
    ## 28  Estonia 2009         13.5 unemployment_rate percentage               4.2
    ## 29  Estonia 2010         16.6 unemployment_rate percentage               4.2
    ## 30  Estonia 2011         12.3 unemployment_rate percentage               4.2
    ## 31  Estonia 2012          9.9 unemployment_rate percentage               4.2
    ## 32  Estonia 2013          8.6 unemployment_rate percentage               4.2
    ## 33  Estonia 2014          7.3 unemployment_rate percentage               4.2
    ## 34  Estonia 2015          6.4 unemployment_rate percentage               4.2
    ## 35  Estonia 2016          6.8 unemployment_rate percentage               4.2
    ## 36  Estonia 2017          5.8 unemployment_rate percentage               4.2
    ## 37  Estonia 2018          5.4 unemployment_rate percentage               4.2
    ## 38  Estonia 2019          4.5 unemployment_rate percentage               4.2
    ## 39  Estonia 2020          6.9 unemployment_rate percentage               4.2
    ## 40  Estonia 2021          6.2 unemployment_rate percentage               4.2
    ## 41  Estonia 2022          5.6 unemployment_rate percentage               4.2
    ## 42  Estonia 2023          6.4 unemployment_rate percentage               4.2
    ## 43  Ireland 2003           NA unemployment_rate percentage               4.2
    ## 44  Ireland 2004           NA unemployment_rate percentage               4.2
    ## 45  Ireland 2005           NA unemployment_rate percentage               4.2
    ## 46  Ireland 2006           NA unemployment_rate percentage               4.2
    ## 47  Ireland 2007           NA unemployment_rate percentage               4.2
    ## 48  Ireland 2008           NA unemployment_rate percentage               4.2
    ## 49  Ireland 2009         12.6 unemployment_rate percentage               4.2
    ## 50  Ireland 2010         14.6 unemployment_rate percentage               4.2
    ## 51  Ireland 2011         15.4 unemployment_rate percentage               4.2
    ## 52  Ireland 2012         15.5 unemployment_rate percentage               4.2
    ## 53  Ireland 2013         13.8 unemployment_rate percentage               4.2
    ## 54  Ireland 2014         11.9 unemployment_rate percentage               4.2
    ## 55  Ireland 2015          9.9 unemployment_rate percentage               4.2
    ## 56  Ireland 2016          8.4 unemployment_rate percentage               4.2
    ## 57  Ireland 2017          6.7 unemployment_rate percentage               4.2
    ## 58  Ireland 2018          5.8 unemployment_rate percentage               4.2
    ## 59  Ireland 2019          5.0 unemployment_rate percentage               4.2
    ## 60  Ireland 2020          5.9 unemployment_rate percentage               4.2
    ## 61  Ireland 2021          6.2 unemployment_rate percentage               4.2
    ## 62  Ireland 2022          4.5 unemployment_rate percentage               4.2
    ## 63  Ireland 2023          4.3 unemployment_rate percentage               4.2
    ##    max_value_country min_value_indic max_value_indic
    ## 1               16.6             4.2            16.6
    ## 2               16.6             4.2            16.6
    ## 3               16.6             4.2            16.6
    ## 4               16.6             4.2            16.6
    ## 5               16.6             4.2            16.6
    ## 6               16.6             4.2            16.6
    ## 7               16.6             4.2            16.6
    ## 8               16.6             4.2            16.6
    ## 9               16.6             4.2            16.6
    ## 10              16.6             4.2            16.6
    ## 11              16.6             4.2            16.6
    ## 12              16.6             4.2            16.6
    ## 13              16.6             4.2            16.6
    ## 14              16.6             4.2            16.6
    ## 15              16.6             4.2            16.6
    ## 16              16.6             4.2            16.6
    ## 17              16.6             4.2            16.6
    ## 18              16.6             4.2            16.6
    ## 19              16.6             4.2            16.6
    ## 20              16.6             4.2            16.6
    ## 21              16.6             4.2            16.6
    ## 22              16.6             4.2            16.6
    ## 23              16.6             4.2            16.6
    ## 24              16.6             4.2            16.6
    ## 25              16.6             4.2            16.6
    ## 26              16.6             4.2            16.6
    ## 27              16.6             4.2            16.6
    ## 28              16.6             4.2            16.6
    ## 29              16.6             4.2            16.6
    ## 30              16.6             4.2            16.6
    ## 31              16.6             4.2            16.6
    ## 32              16.6             4.2            16.6
    ## 33              16.6             4.2            16.6
    ## 34              16.6             4.2            16.6
    ## 35              16.6             4.2            16.6
    ## 36              16.6             4.2            16.6
    ## 37              16.6             4.2            16.6
    ## 38              16.6             4.2            16.6
    ## 39              16.6             4.2            16.6
    ## 40              16.6             4.2            16.6
    ## 41              16.6             4.2            16.6
    ## 42              16.6             4.2            16.6
    ## 43              16.6             4.2            16.6
    ## 44              16.6             4.2            16.6
    ## 45              16.6             4.2            16.6
    ## 46              16.6             4.2            16.6
    ## 47              16.6             4.2            16.6
    ## 48              16.6             4.2            16.6
    ## 49              16.6             4.2            16.6
    ## 50              16.6             4.2            16.6
    ## 51              16.6             4.2            16.6
    ## 52              16.6             4.2            16.6
    ## 53              16.6             4.2            16.6
    ## 54              16.6             4.2            16.6
    ## 55              16.6             4.2            16.6
    ## 56              16.6             4.2            16.6
    ## 57              16.6             4.2            16.6
    ## 58              16.6             4.2            16.6
    ## 59              16.6             4.2            16.6
    ## 60              16.6             4.2            16.6
    ## 61              16.6             4.2            16.6
    ## 62              16.6             4.2            16.6
    ## 63              16.6             4.2            16.6

## 4.1 Unemployment rate in 2009

- This section below includes formatted figures using auxiliary function
  “fmt_markdown_figures()” from helper_functions.R script:

In 2009 unemployment rate for Bulgaria was 11.3% compared to 7.9% one
year before in 2009.

This is an increase of
