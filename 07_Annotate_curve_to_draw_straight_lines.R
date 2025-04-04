# 1. Annotate segment example

# R Script: 07_Annotate_curve_draw_straight_lines.R

# Replicate this example
# https://r-graphics.org/recipe-annotate-segment
library(gcookbook)
library(tidyverse)
library(ggtext)

line_plot <- ggplot(filter(climate,Source == "Berkeley"), aes(x = Year, y = Anomaly10y)) +
  geom_line()
line_plot

line_plot <- line_plot + 
             annotate("segment", x = 1950, xend = 1980, y = -.25, yend = -.25 )
line_plot
ggsave("Plots/01_Annotate_segment_example.png", width = 6.38, height = 5.80)

str(climate)


# 1. Applying it to my plot
library(here)
library(tidyverse)

# 1. Import unemployment indicator data 
# Load initial dataset from "data_cleansed" folder:
unemp_data <- read.table(here("data_cleansed", "EU_UNEMP_CLEAN_une_rt_a_LONG.csv"),
                         header =TRUE, sep =',',stringsAsFactors =TRUE)
head(unemp_data)
str(unemp_data)

# 2. Subset data for Greece
unemp_greece <- unemp_data %>% 
  filter(country %in% c("greece")) %>%  
  select(-c(X))
unemp_greece

str(unemp_greece)

# Turn date as a R standard date format
unemp_greece <- unemp_data %>% 
  filter(country %in% c("greece")) %>%  
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  select(-c(X))
unemp_greece

str(unemp_greece)


# 1. Create flag for max unemp
# Flag #01 for max Unemployment
# Flag #02 for latest date 
unemp_greece_flags <- unemp_greece %>% 
  filter(country %in% c("greece")) %>%  
  mutate(
    unemp_round = round(unemp_rate,0),
    max_unemp = max(unemp_rate, na.rm = TRUE),
    Max_unemp_flag = ifelse(unemp_rate == max_unemp,TRUE,FALSE) 
  ) %>% 
  mutate(
    Latest_date = max(date),
    Max_date_flag = ifelse(date == Latest_date,TRUE,FALSE)
    ) %>% 
  select(date,country,unemp_rate,max_unemp,Max_unemp_flag,Max_date_flag)
unemp_greece_flags

# Exclude NA values

unemp_greece_flags <- unemp_greece_flags %>% drop_na() 
unemp_greece_flags

# 2. Create a basic bar plot
str(unemp_greece)
Plot01 <- unemp_greece_flags %>% 
  ggplot(aes(x = date, y = unemp_rate)) +
  geom_col(fill = "#BAD1D6")
Plot01


Plot02 <- unemp_greece_flags %>% 
  ggplot(aes(x = date, y = unemp_rate)) +
  geom_col(fill = "#BAD1D6") +
  labs(title = "Unemployment in Greece.2003-2023 period",
       caption = "Note: Year 2023  latest available data. Source:EUROSTAT https://ec.europa.eu/eurostat/") +
  theme_classic() 
Plot02



# Rotate x axis column date values
# theme(axis.text.x = element_text(angle = 45, hjust = 1))
Plot03 <- unemp_greece_flags %>% 
  ggplot(aes(x = date, y = unemp_rate)) +
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
  ggplot(aes(x = date, y = unemp_rate)) +
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
geom_richtext(geom = "textbox", label = "Year 2013<br>highest value", 
              x = as.Date("2011-12-01"), y = 27,
              hjust = 1,  
              fill = NA,
              color = "black",
              label.colour = NA,
              show.legend = FALSE)
Plot04_segment
ggsave("plots_output/04_Bar_chart_including_annotation_segment_text_bubble.png", width = 6.38, height = 5.80)


# 4. Include color and straight line using annotate function to flag year with highest unemployment value. 
library(ggtext)

Plot05_colour <- unemp_greece_flags %>% 
  ggplot(aes(x = date, y = unemp_rate, fill = Max_unemp_flag)) +
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
  geom_text(aes(label = unemp_rate), position = position_dodge(width = 0.9),
            vjust = +1.50) +
  # Introduce 45 angle to Y axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add straight lines to flag year with highest unemployment value
  annotate('curve', x = as.Date("2011-12-01"),xend = as.Date("2012-06-01"),y = 27,yend = 27,linewidth = 0.9, 
           curvature = 0.0) +
  annotate('curve', x = as.Date("2011-12-01"),xend = as.Date("2011-12-01"),y = 26,yend = 28,linewidth = 0.9, 
           curvature = 0.0) +
  # Include now annotation bubble next to the LINE drawn using annotate('curve')
  geom_richtext(geom = "textbox", label = "Highest value<br>Year 2013",x = as.Date("2011-11-25"), y = 27,
                hjust = 1,fill = NA,color = "black",label.colour = NA,show.legend = FALSE) 
Plot05_colour
ggsave("plots_output/17_Bar_chart_annotation_segment_text_bubble_colour_highest_value.png", width = 6.38, height = 5.80)
