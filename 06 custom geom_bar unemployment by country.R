# Script:  06 custom geom_bar unrmployment by county.R
# AIM 06 custom geom_bar unmployment metric for each country


# 1. Import previous files 
library(here)
library(tidyverse)

# 1. Import unemployment indicator data 
unemp_data <- read.table(here("data_cleansed", "EU_UNEMP_CLEAN_une_rt_a_LONG.csv"),
                         header =TRUE, sep =',',stringsAsFactors =TRUE)
head(unemp_data)

# 2. Subset data for Greece
str(unemp_data)
head(unemp_data)

unemp_greece <- unemp_data %>% 
  filter(country %in% c("greece")) %>%  
  select(-c(X))
unemp_greece


# 3. Create two flags
# 3.1. Flag for highest unemployment year
str(unemp_greece)

unemp_greece_max <- unemp_greece %>% 
  mutate(
    Date = as.Date(date),
    unemp_round = round(unemp_rate,0),
    max_unemp = max(unemp_rate, na.rm = TRUE),
    Max_unemp_flag = ifelse(unemp_rate == max_unemp,TRUE,FALSE) 
  ) %>% 
  select(Date,country,unemp_rate,max_unemp,Max_unemp_flag)


# 3.2. Flag for the latest year
str(unemp_greece_max)
unemp_greece_latest <- unemp_greece_max %>% 
  mutate(Latest_date = max(Date)) %>% 
  mutate(Max_date = ifelse(Date == Latest_date,TRUE,FALSE) )


# 4. Create a basic bar plot
Plot01 <- unemp_greece %>% 
  ggplot(aes(x = date, y = unemp_rate)) +
  geom_col(fill = "#BAD1D6")