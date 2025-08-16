# 08 Net migration figures Spain.R

# Evolution of net external migration in Spain. 2014-2024
# Import Excel file into R: "INE Net external migration Spain 2014 2023.xls"

library(readxl)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)

Path <- here()
Path

# List excel files on Data sub-directory

# 01 Check files available in the data_demography sub-folder
list.files (path = "./data_demography" ,pattern = "xls$")

# [1] "INE Net external migration Spain 2014 2023.xls"

# 02 List tabs from above Excel file to know which tab to import
excel_sheets("./data_demography/INE Net external migration Spain 2014 2023.xls")
# [1] "Hoja1"

# 03 Import Excel file into R
# 03.2 Extract just rows including dates info
# n_max = 10  - Import just numeric information from source
# Remove NA values 
# {dplyr} using na.omit() function
#  Use col_types function with "guess" option to import cols with right format into R
# read_excel(col_types = c("guess","guess")
net_migration_spain <-  read_excel(
  here("data_demography", "INE Net external migration Spain 2014 2023.xls"), 
  sheet = 1, skip =7, n_max = 10,
  col_types = c("guess","guess")
  ) %>% 
  clean_names() %>% 
  na.omit()

net_migration_spain

# 03.4 Use col_types function with "guess" option to import cols with right format into R
# read_excel(col_types = c("guess","guess")


# 04 Initial ggplot geom_plot 
# Include geom_col() to create a bar plot
net_migration_plot <- spain_net_migration %>% 
                      select(year, net_migration = net_external_migration) %>% 
                      ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) 
net_migration_plot

# Now we start customising this initial bar plot

# 04.1 Remove scientific notation for Y axis 
# Include also theme_light()
# Remove scientific notation from Y axis using options(scipen=999)
options(scipen=999)

net_migration_plot <- spain_net_migration %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) +
theme_light() 
net_migration_plot


# Include title and subtitles

net_migration_plot <- spain_net_migration %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
labs(title = "Spain Net migration. 2014-2023 period",
     subtitle = "Evolution of net external migration in Spain",
     caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
net_migration_plot

# 04.2 Increase year labels displayed on X axis
# As year is defined as double columns I use a vector c() to populate labels. 
# I could also have used lubridate library to create a true date variable
net_migration_plot <- spain_net_migration %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
net_migration_plot

# 04.3 Remove X and Y axis labels
# theme(axis.title.x = element_blank(),
#      axis.title.y = element_blank())
# Also add fill colour to bars using fill() parameter inside aes() function  
net_migration_plot <- spain_net_migration %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration, fill = "cornflowerblue")) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
net_migration_plot

ggsave("plots_output/18_Spain_net_migration_plain.png", width = 6, height = 4)



# 04.4 Add color to geom_col() function
# Using fill() function inside the geom_col() function.
net_migration_plot <- spain_net_migration %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE,fill = "cornflowerblue") +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
net_migration_plot

ggsave("plots_output/19_Spain_net_migration_fill_colour.png", width = 6, height = 4)

# 04.5 Create flag for negative values
#   mutate() function to create negative values for Migration figures
# mutate(neg_values = ifelse(net_external_migration <0, TRUE,FALSE))
neg_values_flag <- spain_net_migration %>% 
                   select(year, net_migration = net_external_migration) %>% 
                   mutate(neg_values = ifelse(net_migration <0, TRUE,FALSE))
neg_values_flag

# Then I can use this new "neg_values" boolean column to colour negative values
# Include fill parameter applied on previous newly created "neg_values" boolean column 
#   ggplot(aes(x = year, y = net_migration, fill = neg_values)) +

net_migration_plot_bool <- neg_values_flag %>% 
  select(year, net_migration,neg_values) %>% 
  ggplot(aes(x=year, y = net_migration, fill = neg_values)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
net_migration_plot_bool

ggsave("plots_output/20_Spain_net_migration_boolean_fill_colour.png", width = 6, height = 4)

# 05. geom_col() bar plot in ggplot2 using custom bar colours for negative and positive values
# Using scale_fill_manual() function:
#   scale_fill_manual(breaks = c(FALSE,TRUE),
#                     values = c("cornflowerblue","coral")) +
net_migration_bool_neg_values <- neg_values_flag %>% 
  select(year, net_migration,neg_values) %>% 
  ggplot(aes(x=year, y = net_migration, fill = neg_values)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  scale_y_continuous(breaks = seq(-100000,850000,by = 100000)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  # Include different colours for Negative and Positive values
  # Based on neg_values column values (TRUE (coral colour ),FALSE (cornflowerblue colour))   )
  scale_fill_manual(breaks = c(FALSE,TRUE),
                       values = c("cornflowerblue","coral")) 
net_migration_bool_neg_values

ggsave("plots_output/21_Spain_net_migration_boolean_custom_colours.png", width = 6, height = 4)
