# 1. Annotate segment example

# R Script: 07_Annotate_curve_draw_straight_lines.R

# Replicate this example
# https://r-graphics.org/recipe-annotate-segment
install.packages("gcookbook",dependencies = TRUE)
library(gcookbook)
library(ggplot2)
library(dplyr)
library(here)
library(ggtext)

line_plot <- ggplot(filter(climate,Source == "Berkeley"), aes(x = Year, y = Anomaly10y)) +
  geom_line()
line_plot

line_plot <- line_plot + 
             annotate("segment", x = 1950, xend = 1980, y = -.25, yend = -.25 )
line_plot
ggsave("plots_output/01_Annotate_segment_example.png", width = 6.38, height = 5.80)

str(climate)


# 1. Applying it to my plot
library(here)
library(tidyverse)

# 1. Import unemployment indicator data 
# Load initial dataset from "data_cleansed" folder:
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
  # select(-c(X))

str(unemp_greece)

# 1. Create flag for max unemp
# Flag #01 for max Unemployment
# Flag #02 for latest date 
unemp_greece_flags <- unemp_greece %>% 
  mutate(
    unemp_round = round(metric_value,0),
    max_unemp = max(metric_value, na.rm = TRUE),
    Max_unemp_flag = ifelse(metric_value == max_unemp,TRUE,FALSE) 
  ) %>% 
  mutate(
    Latest_date = max(datef),
    Max_date_flag = ifelse(datef == Latest_date,TRUE,FALSE)
    ) %>% 
  select(datef,country,metric_name,metric_value,unemp_round,Max_unemp_flag,Max_date_flag)
unemp_greece_flags

# Exclude NA values

unemp_greece_flags <- unemp_greece_flags %>% drop_na() 
unemp_greece_flags

# 2. Create a basic bar plot
str(unemp_greece)
Plot01 <- unemp_greece_flags %>% 
  ggplot(aes(x = datef, y = metric_value)) +
  geom_col(fill = "#BAD1D6")
Plot01


Plot02 <- unemp_greece_flags %>% 
  ggplot(aes(x = datef, y = metric_value)) +
  geom_col(fill = "#BAD1D6") +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  theme_classic() 
Plot02



# Rotate x axis column date values
# theme(axis.text.x = element_text(angle = 45, hjust = 1))
Plot03 <- unemp_greece_flags %>% 
  ggplot(aes(x = datef, y = metric_value)) +
  geom_col(fill = "#BAD1D6") +
labs(title = "Unemployment in Greece.2003-2023 period",
     caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Plot03
ggsave("plots_output/02_Basic_ggplot2_line_Chart.png", width = 6.38, height = 5.80)

# 3. INTRODUCE SEGMENTS to annotate basic geom_col() chart
# Using annotate("segment") function
#   annotate("segment", x = 1950, xend = 1980, y = -.25, yend = -.25 )
str(unemp_greece)
head(unemp_greece)

# Outcome, Segments didn't work, using annotate('curve') to create a segment
# USING  annotate('curve') to draw a segment. This seems to work
# install.packages("ggtext",dependencies = TRUE)
library(ggtext)

Plot04_segment <- unemp_greece_flags %>% 
  ggplot(aes(x = datef, y = metric_value)) +
  geom_col(fill = "#BAD1D6") +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ##segments(x0 = 2010,x1 = 2011,y0 = 10,y1 = 10,lwd = 2,col = "orange") 
annotate('curve',
         x = as.Date("2011-12-01"),
         xend = as.Date("2012-06-01"),
         y = 27,
         yend = 27,
         linewidth = 1, 
         curvature = 0.0) +
# Include now annotation bubble next to the LINE drawn using annotate('curve')
geom_richtext(label = "Highest value<br>Year 2013",x = as.Date("2011-11-25"), y = 27,
                hjust = 1,fill = NA,color = "black",label.colour = NA,show.legend = FALSE) 
Plot04_segment
ggsave("plots_output/04_Bar_chart_including_annotation_segment_text_bubble.png", width = 6.38, height = 5.80)


# 4. Include color and straight line using annotate function to flag year with highest unemployment value. 
library(ggtext)

Plot05_colour <- unemp_greece_flags %>% 
  ggplot(aes(x = datef, y = metric_value, fill = Max_unemp_flag)) +
  geom_col(show.legend = FALSE) +
  # Apply labels on x and Y axis on specific dates and values
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_fill_manual(breaks = c(FALSE,TRUE),
                    values = c("#BAD1D6","#539CBA")) +
  labs(title = "Unemployment in Greece.2009-2023 period",
       subtitle = "Unemployment rate has more than halved since 2013",
       x = "Period", 
       y = "Total unemployment rate (%)",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  theme_classic() +
    # Remove X axis gap
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(breaks = seq(0,30,by = 1)) +
  # Include labels over bars
  geom_text(aes(label = metric_value), position = position_dodge(width = 0.9),
            vjust = +1.50) +
  # Introduce 45 angle to Y axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add straight lines to flag year with highest unemployment value
  annotate('curve', x = as.Date("2011-12-01"),xend = as.Date("2012-06-01"),y = 27,yend = 27,linewidth = 0.9, 
           curvature = 0.0) +
  annotate('curve', x = as.Date("2011-12-01"),xend = as.Date("2011-12-01"),y = 26,yend = 28,linewidth = 0.9, 
           curvature = 0.0) +
  # Include now annotation bubble next to the LINE drawn using annotate('curve')
  geom_richtext(label = "Highest value<br>Year 2013",x = as.Date("2011-11-25"), y = 27,
                hjust = 1,fill = NA,color = "black",label.colour = NA,show.legend = FALSE) 
Plot05_colour
ggsave("plots_output/17_Bar_chart_annotation_segment_text_bubble_colour_highest_value.png", width = 6.38, height = 5.80)
