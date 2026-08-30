Recent Spain population trends
================
PLR
2026-08-30

## Latest date this report was produced

Today’s date is **30 August 2026**. This report was published on the
week starting on **28 August 2026**.

## 1. Load Spain population data

We load latest Spanish Population Figures released by the Spanish
National Institute: <https://www.ine.es/consul/serie.do?d=true&s=ECP320>

In the third tab from Excel file **“INE total and foreign population
figures Spain.xlsx”** we load the table describing Total, foreign and
nationals population figures in Spain, based on Spanish nationality for
the 2004 2025 period.

    ## [1] "01 Spain components pop change_data_prep.xlsx"                    
    ## [2] "INE population by nationality Spanish foreign 2002 2025.xlsx"     
    ## [3] "INE total and foreign population figures Spain.xlsx"              
    ## [4] "INE_Foreign_Born_Residents_in_Spain_by_Nationality_1998_2022.xlsx"
    ## [5] "INE_natural_increase_births_deaths_1992_2024.xlsx"                
    ## [6] "INE_Total_resident_population_by_nationality_1998_2022.xlsx"

    ## [1] "INE_Foreign_population"       "INE_Total_population"        
    ## [3] "INE_Total_foreign_population"

``` r
population_data <- read_excel(
  "data_demography/INE total and foreign population figures Spain.xlsx",
  sheet = "INE_Total_foreign_population", skip = 2, n_max = 22,
  col_types = c("guess", "numeric", "numeric", "guess","guess","guess","guess","guess")) %>% 
  clean_names() %>% 
       select (date = todas_las_edades, total_population = total,
                       foreign_population, percent_foreign_population = percent_foreign_nationals_total_population,
                      total_population_YoY_N = total_yo_y_n, total_population_YoY_perc = total_yo_y_percent,
                       foreign_population_YoY_N = foreign_nationals_yo_y_n, foreign_population_YoY_perc= foreign_total_yo_y_percent)
```

# 2. Spanish total population change over time

This sections described total population change in Spain from 2005 to
2025.

``` r
library(lubridate)

population_change <- population_data %>% select(date,total_population,foreign_nationals_population = foreign_population,percent_foreign_population,total_population_YoY_perc,foreign_population_YoY_perc)

population_change_date_creation <- population_change %>% 
  mutate(date_day = substr(date,1,1),date_month = substr(date,3,13),date_year =  substr(date,15,18))

population_change_fmt <-population_change_date_creation %>% 
  mutate(date_month_eng = gsub("de enero de","january",date_month)) %>% 
  select(date,total_population,foreign_nationals_population,percent_foreign_population,
         total_population_YoY_perc,foreign_population_YoY_perc,date_day,date_month_eng,date_year) %>% 
  mutate(date_to_fmt = paste0(date_day," ",date_month_eng," ",date_year)) %>% 
# population_change_fmt_date <- population_change_fmt %>% 
  mutate(date_fmt = dmy(date_to_fmt)) %>% 
  select(date_fmt,total_population,foreign_nationals_population,percent_foreign_population,
         total_population_YoY_perc_change =  total_population_YoY_perc,
         foreign_population_YoY_perc_change = foreign_population_YoY_perc) %>% 
  arrange(date_fmt)
population_change_fmt
```

    ## # A tibble: 22 × 6
    ##    date_fmt   total_population foreign_nationals_popula…¹ percent_foreign_popu…²
    ##    <date>                <dbl>                      <dbl>                  <dbl>
    ##  1 2005-01-01         43296335                    3430204                 0.0792
    ##  2 2006-01-01         44009969                    3930916                 0.0893
    ##  3 2007-01-01         44784659                    4449434                 0.0994
    ##  4 2008-01-01         45668938                    5086295                 0.111 
    ##  5 2009-01-01         46239271                    5386659                 0.116 
    ##  6 2010-01-01         46486621                    5402579                 0.116 
    ##  7 2011-01-01         46667175                    5312440                 0.114 
    ##  8 2012-01-01         46818216                    5236030                 0.112 
    ##  9 2013-01-01         46712650                    5064584                 0.108 
    ## 10 2014-01-01         46495744                    4676352                 0.101 
    ## # ℹ 12 more rows
    ## # ℹ abbreviated names: ¹​foreign_nationals_population,
    ## #   ²​percent_foreign_population
    ## # ℹ 2 more variables: total_population_YoY_perc_change <dbl>,
    ## #   foreign_population_YoY_perc_change <dbl>

``` r
min_date <- min(population_change_fmt$date_fmt)
max_date <- max(population_change_fmt$date_fmt)
# (gsub("de enero","january", date),1,6)),
```

## 2.1 Spain total population

This first chart displays total population in Spain for 2004-2025 time
period. Total resident population in Spain, regardless of their
nationality

``` r
format_total_population_spain <- function(mydataset,my_date,column,format = NULL){
  row <- mydataset %>% filter(date_fmt == my_date) 
  print(row)
  value <- row %>% pull({{column}})
  print(value)
  # Default value if format is not provided
  if (column %in% c("total_population")){
    return(as.character(prettyNum(value,big.mark = ",")))  
  } else if (column %in% c("percent_foreign_population")){
  return(paste0(round(value,1),"%"))  # to be built
}
}
# format_total_population_spain(mydataset = population_change_fmt, my_date = "2007-01-01",column = "total_population")
# format_total_population_spain(mydataset = population_change_fmt, my_date = "2007-01-01",column = "percent_foreign_population")
```

``` r
Plot_total_population_spain <- population_change_fmt %>% select(date_fmt,total_population) %>% 
  ggplot(aes(date_fmt,total_population)) +
  geom_line(aes(colour = "sienna3")) +
  geom_point(fill = "sienna3") +
  labs(title = "Spain total poulation. 2004-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  scale_y_continuous(breaks = seq(40000000, 50000000, by = 200000)) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  theme_light() +
   theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = + 45, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 
Plot_total_population_spain
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20total%20population-1.png)<!-- -->

In 2007 Spain total population was 44,784,659 eleven years later, it
reached 46,645,070. After Covid19 Pandemic it grew steadely until 2022,
when it reached 47,486,727

It is noticeable that since 2022, mainly due to net migration from
foreign countries, Spain total population grew with YoY rates above 1%
In 2023 Spain total population reached 48,085,361 One year later, it was
48,619,695

The same fast pace of increase has continued in 2025 49,128,297 And in
2026 with the recently released provisional population estimates, the
Total population in Spain has reached nearly 50 million 49,596,376

## 3. Exploratory charts Total, Spanish nationals and foreign population in Spain

From the newly on boarded data, we create a new Year variable from
initial date column. As we only want to display Year values.

``` r
INE_population_subset <- population_data %>%
                         select(date,total_population,foreign_population) %>% 
                         mutate(Year = substring(date, 15, 25)) 
INE_population_subset
```

    ## # A tibble: 22 × 4
    ##    date               total_population foreign_population Year 
    ##    <chr>                         <dbl>              <dbl> <chr>
    ##  1 1 de enero de 2026         49596376            7256796 2026 
    ##  2 1 de enero de 2025         49128297            6911971 2025 
    ##  3 1 de enero de 2024         48619695            6502282 2024 
    ##  4 1 de enero de 2023         48085361            6089620 2023 
    ##  5 1 de enero de 2022         47486727            5509046 2022 
    ##  6 1 de enero de 2021         47400798            5402702 2021 
    ##  7 1 de enero de 2020         47318050            5241278 2020 
    ##  8 1 de enero de 2019         46918951            4850762 2019 
    ##  9 1 de enero de 2018         46645070            4577322 2018 
    ## 10 1 de enero de 2017         46497393            4417653 2017 
    ## # ℹ 12 more rows

``` r
INE_calc_fields <- INE_population_subset %>%                          
                         select(Year,total_population,foreign_population) %>% 
                         mutate(Spanish_nationals = total_population - foreign_population) %>% 
                         arrange(Year)
INE_calc_fields
```

    ## # A tibble: 22 × 4
    ##    Year  total_population foreign_population Spanish_nationals
    ##    <chr>            <dbl>              <dbl>             <dbl>
    ##  1 2005          43296335            3430204          39866131
    ##  2 2006          44009969            3930916          40079053
    ##  3 2007          44784659            4449434          40335225
    ##  4 2008          45668938            5086295          40582643
    ##  5 2009          46239271            5386659          40852612
    ##  6 2010          46486621            5402579          41084042
    ##  7 2011          46667175            5312440          41354735
    ##  8 2012          46818216            5236030          41582186
    ##  9 2013          46712650            5064584          41648066
    ## 10 2014          46495744            4676352          41819392
    ## # ℹ 12 more rows

These charts below describe the evolution of Total, foreign and spanish
nationals population for the 2005-2025 period:

- Spain total population

``` r
options(scipen=999)

Spanish_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = total_population)) +
  geom_bar(stat = "identity", fill = "darkolivegreen2") +
  labs(title = "Spain total poulation. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = total_population),size = 2.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,55000000), expand = FALSE )

Spanish_population_plot
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20total%20population%20bar%20plot-1.png)<!-- -->

Then this is the foreign population in Spain for the same time period.

``` r
options(scipen=999)

forign_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = foreign_population)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Foreign population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = foreign_population),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,7500000), expand = FALSE )

forign_population_plot
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20foreign%20population%20bar%20plot-1.png)<!-- -->

This last plot displays national population in Spain for the 2005-2025
period.

``` r
options(scipen=999)

national_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = Spanish_nationals)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Spanish nationals population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = format(Spanish_nationals,big.mark = ",",scientific = FALSE))
            ,size = 4.2,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,46000000), expand = FALSE )

national_population_plot
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20national%20population%20bar%20plot-1.png)<!-- -->

## 3. Spain population figures Overview details

In this section we highlight a couple of trends observed in both Total
and foreign population in Spain from 2004 until 2025 period.

- As of 1st of January **2026**., the year for the latest available
  population figures, Total population in Spain was **49,596,376**. Up
  by 458,253 from previous year.

- In contrast, on the year **2005**, the first year on this series,
  total population in Spain was **43,296,335**.

- In terms of foreign population, on the following year **2005**.there
  was a foreign population of **3,430,204**

- At the end of the series on 1st January **2025**, latest foreign
  population figures in Spain was **6,911,971**. We will describe these
  population changes in absolute figures and percent change in the next
  section below.

### 3.1 Share of foreign population over total population in Spain

Next, we describe the foreign population percent share of the total
population in Spain for the 2004-2025 period.
