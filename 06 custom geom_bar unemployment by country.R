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
  select(date,Date,country,unemp_rate,max_unemp,Max_unemp_flag)

# 3.2. Flag for the latest year
str(unemp_greece_max)

unemp_greece_latest <- unemp_greece_max %>% 
  mutate(
      Latest_date = max(Date),
      Max_date = ifelse(Date == Latest_date,TRUE,FALSE))


# 4. Create a basic bar plot
str(unemp_greece)
Plot01 <- unemp_greece %>% 
  ggplot(aes(x = date, y = unemp_rate)) +
  geom_col(fill = "#BAD1D6")

Plot02 <- unemp_greece %>% 
  ggplot(aes(x = date, y = unemp_rate)) +
  geom_col(fill = "#BAD1D6") +
labs(title = "Unemployment in Greece.2003-2023 period",
     caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  theme_classic() 

ggsave("plots_output/10_Greece_unemployment_value.png", width = 6.38, height = 5.80)


# 4.1 Introduce titles and flag for year with max unemployment value 
# Flag color introduced, fill goes to ggplot() function and geom_col() used to remove legend
str(unemp_greece_latest)

# To highlight specific date, it must be defined as a FACTOR

Plot03 <- unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col() +
  theme_classic() 
Plot03

ggsave("plots_output/11_True_false_colour_defined_by_max_unemp_value.png", width = 6.38, height = 5.80)


# 4.2 Introduce custom colour in the plot
#    geom_col(show.legend = FALSE) +
# colours: FALSE values for "is_2024" variable: Aquamarine "#BAD1D6"
#          TRUE values for "is_2024" variable: Last year is 2024. DARK BLUE "#539CBA"
#    scale_fill_manual(breaks = c(FALSE,TRUE),
#                   values = c("#BAD1D6","#539CBA"))
  
Plot04 <-  unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  coord_cartesian(expand = FALSE) +
    theme_classic() 
Plot04

ggsave("plots_output/12_Cutom_color_bars_based_flag_max_value.png", width = 6.38, height = 5.80)

  