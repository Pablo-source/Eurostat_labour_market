# 08 Net migration figures Spain latest figures.R

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
excel_sheets("./data_demography/INE Net external migration Spain 2014 2024.xls")
# [1] "Hoja1"

# 03 Import Excel file into R
spain_net_migration_data <-  read_excel(
  here("data_demography", "INE Net external migration Spain 2014 2023.xls"), 
  sheet = 1, skip =7, n_max = 11,
  col_types = c("guess","guess")
  ) %>% 
  clean_names() %>% 
  na.omit()

# 03.4 Use col_types function with "guess" option to import cols with right format into R
# read_excel(col_types = c("guess","guess")

# 04 Initial exploratory ggplot geom_plot 
# Include geom_col() to create a bar plot
net_migration_plot <- spain_net_migration_data %>% 
                      select(year, net_migration = net_external_migration) %>% 
                      ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) 

# 5. Now we start customizing this initial bar plot

# 5.1 Remove scientific notation for Y axis 
# Include also theme_light()
# Remove scientific notation from Y axis using options(scipen=999)

# Include title and subtitles
options(scipen=999)
net_migration_plot_incl_titles <- spain_net_migration_data %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
labs(title = "Spain Net migration. 2014-2023 period",
     subtitle = "Evolution of net external migration in Spain",
     caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
net_migration_plot_incl_titles

# 5.3 Remove X and Y axis labels
# theme(axis.title.x = element_blank(),
#      axis.title.y = element_blank())
# Also add fill colour to bars using fill(ill = "cornflowerblue") parameter inside geom_col() function  
# 
Spain_net_migration_plot01 <-  spain_net_migration_data %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE, fill = "cornflowerblue") +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024)) +
  labs(title = "Spain Net migration. 2014-2024 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
Spain_net_migration_plot01

ggsave("plots_output/18_Spain_net_migration_plain_2014_2024.png", width = 6, height = 4)

# 6. Create flag for negative values
#   mutate() function to create negative values for Migration figures
# mutate(neg_values = ifelse(net_external_migration <0, TRUE,FALSE))

neg_values_flag <- spain_net_migration_data %>% 
                   select(year, net_migration = net_external_migration) %>% 
                   mutate(direction = ifelse(net_migration <0, "negative", "positive"))
neg_values_flag

# Updated New migration plot to include 2024 figures
options(scipen=999)

nudge <- 20000
net_migration_bar_data_labels <- neg_values_flag %>% 
  select(year, net_migration,direction) %>% 
  mutate(label_y = if_else(net_migration < 0,
                           net_migration - nudge,net_migration + nudge)) 

net_migration_2024_spain <- ggplot(net_migration_bar_data_labels, 
                                   aes(x=year, y = net_migration, 
                                       fill = direction,
                                       label = format(net_migration, big.mark = ","))) +
  geom_text(aes(y = label_y)) + # Include this geom_text() code below to plot labels below bars: 
  geom_col(show.legend = FALSE) +
  
  scale_fill_manual(breaks = c("negative", "positive"),
                    values = c("coral","cornflowerblue")) +
  scale_color_manual(breaks = c("negative","positive"),   # Add custom colors for outside border bar
                     values = c("coral","cornflowerblue")) +
  theme_light() +
  theme(legend.position =  "none", # Remove legends 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2024. https://www.ine.es/dyngs/Prensa/en/EMCR2024.htm") +
  geom_hline(yintercept = 0, linewidth = 0.3)

ggsave("plots_output/22_Spain_net_migration_boolean_2014_2024.png", width = 6, height = 4)
