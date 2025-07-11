# File: 09 Total population by nationality Spain 2004 2025.R
# Total population by nationality - Spain
## Input file:  INE total and foreign population figures Spain.xlsx

## Source: INE (Instituto Nacional de Estadistica): Spanish equivalent to the Office for National Statistics

## Import third tab from Excel file called "INE_Total_foreign_population"

# 1. Load required libraries
library(readxl)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)

Path <- here()
Path
# [1] "/home/pablo/Documents/Pablo_ubuntu/Repos/Pablo_source_repo/Eurostat_labour_market"

# List excel files on Data sub-directory

# 01 Check files available in the data folder. This time I want to import an Excel .xlsx file 
# This code below uses specific pattern to differenciate between .xls and .xlsx file we are interested in the latter
list.files (path = "./Demography" ,pattern = "xlsx$")

# [1] "INE total and foreign population figures Spain.xlsx"

# 02 List tabs from above Excel file to know which tab to import
# ## Import third tab from Excel file called "INE_Total_foreign_population"
excel_sheets("./Demography/INE total and foreign population figures Spain.xlsx")

# [1] "INE_Foreign_population"       "INE_Total_population"         "INE_Total_foreign_population"

# 03 Import Excel file into R
total_foreign_population_data <- read_excel(
  here("Demography", "INE total and foreign population figures Spain.xlsx"), 
  sheet = "INE_Total_foreign_population", skip =2) %>% 
  clean_names()
total_foreign_population_data

head(total_foreign_population_data)
names(total_foreign_population_data)

# 04 We can also omit footnotes importing data just up to row 22
# To avoid all footnotes present in the original Excel file 
# # n_max = 22  - Import just numeric information from source
total_foreign_population_data <- read_excel(
  here("Demography", "INE total and foreign population figures Spain.xlsx"), 
  sheet = "INE_Total_foreign_population", skip =2, n_max = 22) %>% 
  clean_names()
total_foreign_population_data

# 05 Rename columns
# Also use select() statement from DPLYR to rename certain columns 
names(total_foreign_population_data)

#> names(total_foreign_population_data)
#[1] "todas_las_edades"                           "total"                                      "foreign_population"                        
#[4] "percent_foreign_nationals_total_population" "total_yo_y_n"                               "total_yo_y_percent"                        
#[7] "foreign_nationals_yo_y_n"                   "foreign_total_yo_y_percent"  

foreign_pop <- total_foreign_population_data

# Rename columns
# (date = todas_las_edades, Total_population = total,  )
foreign_pop <- foreign_pop %>% 
               select (date = todas_las_edades, total_population = total,
                       foreign_population, percent_foreign_population = percent_foreign_nationals_total_population,
                       total_population_YoY_N = total_yo_y_n, total_population_YoY_perc = total_yo_y_percent,
                       foreign_population_YoY_N = foreign_nationals_yo_y_n, foreign_population_YoY_perc= foreign_total_yo_y_percent)
foreign_pop

foreign_pop

# 06 Subset columns to replicate existing calculations
# We are going to replicate existing calculations following this script I did in Python (jut to do a small exercise of comparing both coding languages side by side)
# https://github.com/Pablo-source/ML-using-Python/blob/main/Seaborn_gallery/05_percent_stacked_barplot_population_nationality_v1.ipynb
# The aim is to obtain the same staked bar plot for Spanish population by nationality 

# I will call this new dataset "INE_population_subset" 

INE_population_subset = foreign_pop %>% select(date,total_population,foreign_population)
INE_population_subset

# 07 Create new calculated fields

# 7.1 New column for year variable 
# Using substring() function 
length(INE_population_subset$date)

INE_population_subset <- INE_population_subset %>% 
                         mutate(Year = substring(date, 15, 25))
INE_population_subset

# 7.2 New Column for Spanish Nationals
# Substracting foreign population from Total population
# Also sort data in ascending order by date
INE_population_nationality <-  INE_population_subset %>% 
                            select(Year,total_population, foreign_population) %>% 
                            mutate(Spanish_nationals = total_population - foreign_population) %>% 
                            arrange(Year)
INE_population_nationality

# 07 Reshape data from wide to long format
# pivot_longer function from {tidyr}
library(tidyr)
INE_pop_nationality_long <- INE_population_nationality %>% 
                            pivot_longer(cols = total_population:Spanish_nationals,
                                         names_to = "nationality", values_to = "population")
INE_pop_nationality_long

# 08 Build stacked barplot using geom_bar(position="stack")
INE_population_stacked <- INE_pop_nationality_long %>% 
                          select(Year,nationality,population) %>% 
                          filter(nationality == c("foreign_population",
                                                  "Spanish_nationals"))
INE_population_stacked
# Then plot this subset of data
Spain_pop_natiolaity <- ggplot(INE_population_stacked, aes(fill = nationality, 
                                                           y = population, x = Year)) +
                        geom_bar(position="stack", stat = "identity")
Spain_pop_natiolaity

ggsave("plots_output/22_Spain_population_by_nationality_2005_2025.png", width = 6, height = 4)

# 09 Theme previous chart changing default legend text 
options(scipen=999)
      
Spain_pop_nationality <- ggplot(INE_population_stacked, aes(fill = nationality, 
                                                           y = population, x = Year)) +
                        geom_bar(position="stack", stat = "identity") +
                        labs(title = "Spanish population by nationality",
                           subtitle ="2005-2025 period",
                           # Change X and Y axis labels
                           x = "Period", 
                           y = "Population") +
                        theme_light() +
  # Change legend default text
  scale_fill_discrete(labels=c('National', 'Foreign')) +
  # Place legend at the bottom of the chart
  theme(plot.title.position = "plot",
     legend.position = "bottom")  +
  # Change default colour for stacked bars (in both chart and legend)
  scale_fill_manual(values = c("cornflowerblue","coral")) 
Spain_pop_nationality
ggsave("plots_output/23_Spain_population_by_nationality_2005_2025.png", width = 6, height = 4)

# 10 Include labels on "Spanish population by nationality" Stacked bar chart 
options(scipen=999)

Spain_pop_nationality_data_labels <- ggplot(INE_population_stacked, aes(fill = nationality, 
                                                            y = population, x = Year)) +
  geom_bar(position="stack", stat = "identity") +
  labs(title = "Spanish population by nationality",
       subtitle ="2005-2025 period",
       # Change X and Y axis labels
       x = "Period", 
       y = "Population") +
  theme_light() +
  # Change legend default text
  scale_fill_discrete(labels=c('National', 'Foreign')) +
  # Place legend at the bottom of the chart
  theme(plot.title.position = "plot",
        legend.position = "bottom")  +
  # Change default colour for stacked bars (in both chart and legend)
  scale_fill_manual(values = c("cornflowerblue","coral")) +
  # Include data labels on bars
  geom_text(aes(label = population),position = position_dodge(width = 0.1),vjust = +0.20,hjust = 0.25)
Spain_pop_nationality_data_labels
ggsave("plots_output/24_Spain_population_by_nationality_2005_2025_data_labels.png", width = 6, height = 4)

# 11 Foreign population Bar chart

Foreign_pop <- INE_population_stacked %>% 
               filter(nationality == 'foreign_population')
Foreign_pop

# Initial barplot 
options(scipen=999)

Foreign_pop_plot <- Foreign_pop %>% 
                    ggplot(aes(x = Year, y = population)) +  
  geom_bar(stat = "identity",fill = "cornflowerblue") +
  labs(title = "Foreign population in Spain.2005-2025 period",
       subtitle ="Source: INE Spanish Office for National Statistics") +
  theme_light() +
  # Include data labels on bars
  geom_text(aes(label = population),position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  # Remove x axis gap
coord_cartesian( ylim=c(0,7500000), expand = FALSE )
Foreign_pop_plot

ggsave("plots_output/25_Spain_Foreign_population_2005_2025_data_labels.png", width = 6, height = 4)

# 12 New Spanish population Bar chart 
# Subset Spanish_nationals from INE_population_stacked dataframe:

# > INE_population_stacked
# A tibble: 22 × 3
# Year  nationality        population
# <chr> <chr>                   <dbl>
#  1 2005  foreign_population    3430204
# 2 2005  Spanish_nationals    39866131

Spanish_nationals_population <- INE_population_stacked %>% 
                                filter(nationality == 'Spanish_nationals')
Spanish_nationals_population

Spanish_population_plot <- Spanish_nationals_population %>% 
                            ggplot(aes(x= Year, y = population)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Spanish nationals population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  geom_text(aes(label = population),position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,50000000), expand = FALSE )

Spanish_population_plot

ggsave("plots_output/26_Spain_nationals_population_2005_2025_data_labels.png", width = 6, height = 4)

# Including thousands separator in data labels displayed in bar plot using format() function
# label = format(population, big.mark = ",")
Spanish_population_plot_fmtd <- Spanish_nationals_population %>% 
  ggplot(aes(x= Year, y = population)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Spanish nationals population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  geom_text(aes(
                label = format(population, big.mark = ",")
                
                ),position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,50000000), expand = FALSE )

Spanish_population_plot_fmtd
ggsave("plots_output/27_Spain_nationals_population_2005_2025_data_labels_fmtd.png", width = 6, height = 4)

# 13 Create percentage of foreign population from total population in Spain bar chart
# I will use mutate() to create several calculations from the original INE_population_subset data
Spain_nationality_percentage <-  INE_population_subset %>% 
  select(Year,total_population, foreign_population) %>% 
  mutate(Spanish_nationals = total_population - foreign_population) %>% 
  arrange(Year)
Spain_nationality_percentage

# Then I will compute the share of foreign_population over Total population 
Spain_nationality_percentage <-  INE_population_subset %>% 
  select(Year,total_population, foreign_population) %>% 
  mutate(Spanish_nationals = total_population - foreign_population) %>% 
  arrange(Year) %>% 
  # new calculation to obtain share of foreign population over total population
  mutate(
    foreign_percent_total = round(foreign_population/total_population*100)
      )
Spain_nationality_percentage
## Plot chart of percentage of foreign population of total population
Percentage_foreign_pop <- Spain_nationality_percentage %>% 
  ggplot(aes(x= Year, y = foreign_percent_total)) +
  geom_bar(stat = "identity", fill = "darkturquoise") +
  labs(title = "Spain. Percent of foreign nationals from total population. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  geom_text(aes(
    label = format(foreign_percent_total, big.mark = ",")),
    position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,20), expand = FALSE )
Percentage_foreign_pop
ggsave("plots_output/28_Spain_proportion_foreign_pop_over_total_population_2005_2025.png", width = 6, height = 4)

# All required plots describing national and foreign population in Spain completed. 
