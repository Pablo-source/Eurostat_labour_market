# Script:  06 custom geom_bar unemployment by county.R
# AIM 06 custom geom_bar unemployment metric for each country


# 1. Import previous files.
library(here)
library(tidyverse)

# 1. Import unemployment indicator data 
combined_indic  <- read.table(here("data_cleansed", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                              header =TRUE, sep =',',stringsAsFactors =TRUE)
head(combined_indic)

str(combined_indic)

## 1.1 Apply date format to "date" column
#  mutate(datef = as.Date(date)) %>% 
# select(date = datef,country,metric_name,metric_value)

combined_indic_date_fmtd <- read.table(here("data_cleansed", "EU_TEMP_UNEMP_COMBINED_SORTED.csv"),
                                       header =TRUE, sep =',',stringsAsFactors =TRUE) %>% 
                            mutate(datef = as.Date(date)) %>% 
                            select(datef, country,metric_name,metric_value)
str(combined_indic_date_fmtd)
head(combined_indic_date_fmtd)

metrics_list <- combined_indic_date_fmtd %>% select(metric_name) %>% distinct()
metrics_list

unemployment_subset <- combined_indic_date_fmtd %>% 
                       filter(metric_name == 'unemp_rate')

# 2. Subset data for Greece
unemp_greece <- unemployment_subset %>% 
  filter(country %in% c("greece")) 

# 3. Create two flags
# 3.1. Flag for highest unemployment year
# Max_unemp_flag = ifelse(unemp_round == max_unemp,TRUE,FALSE) 
str(unemp_greece)

library(tidyr)

unemp_greece_max <- unemp_greece %>% 
  mutate(
    unemp_round = round(metric_value,0),
    max_unemp = max(metric_value, na.rm = TRUE))

unemp_greece_max_flag <- unemp_greece_max %>% 
  mutate(Max_unemp_flag = ifelse(metric_value == max_unemp,TRUE,FALSE) 
  ) %>% 
  select(datef,country,unemp_round,max_unemp,Max_unemp_flag) %>% 
  drop_na() # Remove empty rows with na() values

# 3.2. Flag for the latest year
#  Max_date = ifelse(Date == Latest_date,TRUE,FALSE))
str(unemp_greece_max)

unemp_greece_latest <- unemp_greece_max_flag %>% 
  mutate(
      Latest_date = max(datef),
      Max_date = ifelse(datef == Latest_date,TRUE,FALSE))

head(unemp_greece_latest)

# 4. Create a basic bar plot
str(unemp_greece)
Plot01 <- unemp_greece_latest %>% 
  ggplot(aes(x = datef, y = unemp_round)) +
  geom_col(fill = "#BAD1D6")

Plot02 <- unemp_greece_latest %>% 
  ggplot(aes(x = datef, y = unemp_round)) +
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
  ggplot(aes(x = datef, y = unemp_round, fill = Max_unemp_flag)) +
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
  coord_cartesian(expand = FALSE) 
Plot04

ggsave("plots_output/12_Cutom_color_bars_based_flag_max_value.png", width = 6.38, height = 5.80)


# 4.3 Introduce theme_classic() to the plot 
Plot05 <-  unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  coord_cartesian(expand = FALSE) +
  theme_classic() 
Plot05

ggsave("plots_output/13_Cutom_theme_color_bars_based_flag_max_value.png", width = 6.38, height = 5.80)


# 4.4 Remove expansion around the whole perimeter
# Always remove this expansion for geom_col()
# coord_cartesian(expand = FALSE) +

Plot06 <-  unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  coord_cartesian(expand = FALSE) +
  theme_classic() 
Plot06

ggsave("plots_output/14_Cutom_theme_color_bars_based_no_gap.png", width = 6.38, height = 5.80)

# 4.5 Include Title and caption
#theme(
#  #  plot.caption = element_textbox_simple(hjust=0), # Wrap legend text
#  plot.caption.position = "plot", # Caption and title left aligned
#  plot.title.position = "plot"
#)

library(ggtext)

Plot07 <-  unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  coord_cartesian(expand = FALSE) +
  theme_classic() +
  theme(
  #  plot.caption = element_textbox_simple(hjust=0), # Wrap legend text
    plot.caption.position = "plot", # Caption and title left aligned
    plot.title.position = "plot"
  )
Plot07

# 4.6 Form title as bold
#  plot.title = element_text(face = "bold"),

library(ggtext)

Plot08 <-  unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  coord_cartesian(expand = FALSE) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    #  plot.caption = element_textbox_simple(hjust=0), # Wrap legend text
    plot.caption.position = "plot", # Caption and title left aligned
    plot.title.position = "plot"
  )
Plot08

ggsave("plots_output/15_Cutom_plot_bold_title.png", width = 6.38, height = 5.80)

# 4.7 Create annotation bubble
# Annotation buble position needs to be adjusted for each chart
library(ggtext)

# Format caption

# Insert a single line break <br> in the annotation buble.
# Remove border colour around "Year 2013 highest unemployment rate" text annotation geom_richtext() (color = "black",label.colour = NA)
# Draw Horizontal line from the text to the bar: 
#   Using annotate(annotate(geom = "segment",x = 2022.5 , xend = 2023.4,y = 63, yend = 63)) 
# Introduce rectangle figure with annotate('rect',xmin = 07, xmax = 12,ymin = 25, ymax = 30, 
#                           alpha = .1 , fill = 'grey',col = 'black')

Plot09 <-  unemp_greece_latest %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
  # annotate('segment',xmin = as.Date("2010-01-01"), xmax = as.Date("2010-01-02"),ymin = 15, ymax = 20) +
 annotate('rect',xmin = 10, xmax = 12,ymin = 25, ymax = 30, 
           alpha = .1 , fill = 'grey',col = 'black') +
  
#  annotate('segment',x = 05, xend =16 ,y = 10, yend =  20, 
#          alpha = .1 , fill = 'blue',col = 'black') +

    labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  geom_col(show.legend = FALSE) +
  # Include annotation buble 
  geom_richtext(geom = "textbox", label = "Year 2013<br>highest unemployment rate", 
                x = 10, y = 26,
                hjust = 1,  
                fill = NA,
                color = "black",
                label.colour = NA,
                show.legend = FALSE) +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  coord_cartesian(expand = FALSE) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    #  plot.caption = element_textbox_simple(hjust=0), # Wrap legend text
    plot.caption.position = "plot", # Caption and title left aligned
    plot.title.position = "plot"
  )
Plot09

ggsave("plots_output/16_Include Annotation buble with geom annotations.png", width = 6.38, height = 5.80)


