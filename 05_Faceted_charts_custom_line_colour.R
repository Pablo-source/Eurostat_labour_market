# 05_Faceted_charts_custom_line_colour.R

# AIM: change default R colours (blue and red to each line charts data series)


# 1. Test changing line color just for two countries (spain, ireland)
Subset_test <-c("ireland","spain")

test_01_plot_data <- all_indicators_datef %>% 
  select(date,datef,country,value,indicator) %>% 
  filter(country %in% Subset_test) 
test_01_plot_data

line_chart_test_01 <- test_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Spain and Ireland - 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") # Place legent at the bottom

line_chart_test_01

ggsave("plots_output/06_Unemp_temp_rate_line_Ireland_Spain_dafault_legeng_colour_01.png", width = 6, height = 4)

# 1.1 Changing legend using scale_colour_manual() function 

# I use scale_colour_manual() with the parameters below to create a new legend with new colours and text displayed

#   scale_colour_manual(
# values = c("black","blue"),
# labels = c("Temporary contracts rate", "Unemployment rate")) 


line_chart_test_02 <- test_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Spain and Ireland - 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") + # Place legent at the bottom
  # Apply legend format here - change default colours to black and blue
  scale_colour_manual(values = c("blue", "black"),
                      labels = c("Temporary contracts rate (%)","Unemployment rate (%)"))
line_chart_test_02

ggsave("plots_output/07_Unemp_temp_rate_line_Ireland_Spain_custom_legeng_colour_01.png", width = 6, height = 4)
  

# 2 Plotting first batch of countries

Subset_countries_01 <-c("euro_area_20_countries_from_2023","belgium","bulgaria","czechia","denmark","germany","estonia",
                        "ireland","greece","spain","france","croatia","italy","cyprus","latvia","lithuania","luxembourg",
                        "hungary")

Subset_01_plot_data <- all_indicators_datef %>% 
  select(date,datef,country,value,indicator) %>% 
  filter(country %in% Subset_countries_01) 
Subset_01_plot_data

# First batch of countries
# Display line charts facets by country displaying each indicator as individual line for each country 
line_chart_batch_01 <- Subset_01_plot_data %>% 
  ggplot( fill = indicator) +
  geom_line(aes(datef,value,colour = indicator, group = indicator)) +
  facet_wrap(~ country, nrow = 2) +
  labs(title = "Temporary Employment and unemployment in EU countries - Subset 01 02- 2003-2023 period. Yearly data",
       subtitle ="Source:https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une",
       y = NULL,colour = NULL, fill = NULL) +
  theme_light() +
  theme(plot.title.position = "plot",
        legend.position = "bottom") # Place legent at the bottom

line_chart_batch_01

ggsave("plots_output/05_Unemp_temp_rate_line_chart_batch_01_change_colour_01.png", width = 6, height = 4)
