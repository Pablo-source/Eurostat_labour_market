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

# 03.4 Use col_types function with "guess" option to import cols with right format into R
# read_excel(col_types = c("guess","guess")

# 04 Initial exploratory ggplot geom_plot 
# Include geom_col() to create a bar plot
net_migration_plot <- net_migration_spain %>% 
                      select(year, net_migration = net_external_migration) %>% 
                      ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) 

# 5. Now we start customizing this initial bar plot

# 5.1 Remove scientific notation for Y axis 
# Include also theme_light()
# Remove scientific notation from Y axis using options(scipen=999)


# Include title and subtitles
options(scipen=999)
net_migration_plot_incl_titles <- net_migration_spain %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
labs(title = "Spain Net migration. 2014-2023 period",
     subtitle = "Evolution of net external migration in Spain",
     caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
net_migration_plot_incl_titles

# 5.2 Increase year labels displayed on X axis
# As year is defined as double columns I use a vector c() to populate labels. 
# I could also have used lubridate library to create a true date variable
net_migration_plot_enhanced <- net_migration_spain %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
net_migration_plot_enhanced

# 5.3 Remove X and Y axis labels
# theme(axis.title.x = element_blank(),
#      axis.title.y = element_blank())
# Also add fill colour to bars using fill(ill = "cornflowerblue") parameter inside geom_col() function  
# 
Spain_net_migration_plot01 <-  net_migration_spain %>% 
  select(year, net_migration = net_external_migration) %>% 
  ggplot(aes(x=year, y = net_migration)) +
  geom_col(show.legend = FALSE, fill = "cornflowerblue") +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
Spain_net_migration_plot01

ggsave("plots_output/18_Spain_net_migration_plain.png", width = 6, height = 4)

# 6. Create flag for negative values
#   mutate() function to create negative values for Migration figures
# mutate(neg_values = ifelse(net_external_migration <0, TRUE,FALSE))

neg_values_flag <- net_migration_spain %>% 
                   select(year, net_migration = net_external_migration) %>% 
                   mutate(direction = ifelse(net_migration <0, "negative", "positive"))
neg_values_flag

# Then I can use this new "neg_values" boolean column to colour negative values
# Include fill parameter applied on previous newly created "neg_values" boolean column 
#   ggplot(aes(x = year, y = net_migration, fill = neg_values)) +

net_migration_plot_bool <- neg_values_flag %>% 
  select(year, net_migration,direction) %>% 
  ggplot(aes(x=year, y = net_migration, fill = direction)) +
  geom_col(show.legend = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
net_migration_plot_bool

ggsave("plots_output/19_Spain_net_migration_boolean_fill_colour.png", width = 6, height = 4)

# 05. geom_col() bar plot in ggplot2 using custom bar colours for negative and positive values
# Using scale_fill_manual() function:
#   scale_fill_manual(breaks = c(FALSE,TRUE),
#                     values = c("cornflowerblue","coral")) +

# This is the original chart with R colours: 
net_migration_plot_bool_custom <- neg_values_flag %>% 
  select(year, net_migration,direction) %>% 
  ggplot(aes(x=year, y = net_migration, fill = direction,label = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

# 5.2 then we add custom colours to bars
net_migration_custom_colors <- neg_values_flag %>% 
  select(year, net_migration,direction) %>% 
  ggplot(aes(x=year, y = net_migration, fill = direction,label = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_fill_manual(breaks = c("negative", "positive"),
                  values = c("cornflowerblue","coral")) +
  scale_color_manual(breaks = c("negative","positive"),   # Add custom colors for outside border bar
                   values = c("cornflowerblue","coral")) 

  net_migration_custom_colors


ggsave("plots_output/20_Spain_net_migration_boolean_custom_colours_initial.png", width = 6, height = 4)
  
# 5.3 Then we add 
# scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) + and 
#  And title subtitle and caption:
net_migration_custom_titles <- neg_values_flag %>% 
  select(year, net_migration,direction) %>% 
  ggplot(aes(x=year, y = net_migration, fill = direction,label = net_migration)) +
  geom_col(show.legend = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_fill_manual(breaks = c("negative", "positive"),
                    values = c("coral","cornflowerblue")) +
  scale_color_manual(breaks = c("negative","positive"),   # Add custom colors for outside border bar
                     values = c("coral","cornflowerblue")) +
  geom_text() +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
  # Include different colours for Negative and Positive values
  # Based on neg_values column values (TRUE (coral colour ),FALSE (cornflowerblue colour))   )
 
net_migration_custom_titles

ggsave("plots_output/20_Spain_net_migration_boolean_custom_colours_final.png", width = 6, height = 4)

# Completed above chart !!!

# 06. Include Labels in Net migration chart bars (WIP)
# Using geom_text() function
# geom_text(aes(label = metric_value), position = position_dodge(width = 0.9),
#          vjust = +1.50) +
# We use nudge (2000), so original value for2014 year is -94976 but the label_y defined by nudge is in the -96976 position in the y axis.
nudge <- 20000
net_migration_bar_data_labels <- neg_values_flag %>% 
  select(year, net_migration,direction) %>% 
  mutate(label_y = if_else(net_migration < 0,
                           net_migration - nudge,net_migration + nudge)) 
  
Plot <- ggplot(net_migration_bar_data_labels, aes(x=year, y = net_migration, fill = direction,label = net_migration)) +
  # Include this geom_text() code below to plot labels below bars: 
  # geom_text(y = label_y)
  geom_text(aes(y = label_y)) +
  geom_col(show.legend = FALSE) 
+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_fill_manual(breaks = c("negative", "positive"),
                    values = c("coral","cornflowerblue")) +
  scale_color_manual(breaks = c("negative","positive"),   # Add custom colors for outside border bar
                     values = c("coral","cornflowerblue")) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") 
# Include different colours for Negative and Positive values
# Based on neg_values column values (TRUE (coral colour ),FALSE (cornflowerblue colour))   )

net_migration_bar_data_labels

ggsave("plots_output/21_Spain_net_migration_bar_data_labels_nudge.png", width = 6, height = 4)










net_migration_data_labels <- neg_values_flag %>% 
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
                    values = c("cornflowerblue","coral")) +
  # Include data labels
  geom_text(aes(label = net_migration), position = position_dodge(width = 0.9), yjust = +1.50)
net_migration_data_labels

ggsave("plots_output/21_Spain_net_migration_boolean_custom_colours_new_data_labels.png", width = 6, height = 4)



# 6.1 Adjust values adding a new mutate statement
# I need a nudge parameter to increase or decrease label position of values when they are positive or negative values
#  1. created nudge value at the top (nudge <- 0.30)
#  2. created new calculated field using mutate(label_y): 
# label_y = if_else(net_migration < 0,
#       net_migration - nudge,net_migration + nudge))
# 3. Finally we add this new label_y to the geom_text() function below. 
#  geom_text(aes(y = label_y)) +  # This colours labels dependening they are negative (red) or positive (green)


# Folder: /home/pablo/Documents/Pablo_ubuntu/R_projects/01_ggplot2_gallery/ggplot2_gallery
# Use Script as a reference: 01 barplot_data_labels.R

nudge <-  0.14

neg_values_label <- neg_values_flag %>% 
  select(year, net_migration,neg_values) %>% 
  mutate(label_y = if_else(net_migration < 0,
                           net_migration - nudge,net_migration + nudge))
neg_values_label



net_migration_data_labels <- neg_values_flag %>% 
  select(year, net_migration,neg_values) %>% 
  mutate(label_y = if_else(net_migration < 0,
                           net_migration - nudge,net_migration + nudge)) %>% 
  ggplot(aes(x=year, y = net_migration, fill = neg_values, label = net_migration)) +
  geom_col(show.legend = FALSE) +
  # geom_text(aes(y = label_y)) +
  geom_text(aes(y = label_y )) +
  theme_light() +
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)) +
  scale_y_continuous(breaks = seq(-100000,850000,by = 100000)) +
  labs(title = "Spain Net migration. 2014-2023 period",
       subtitle = "Evolution of net external migration in Spain",
       caption = "Source: INE.Satistics on Migrations and Changes of Residence (SMCR). Year 2023. https://www.ine.es/dyngs/Prensa/en/EMCR2023.htm") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

# 6.1.1 Now we want adjust those data labels (new section here)
# geom_text( aes(y = decile_y, label = decile), color = "gray40"


