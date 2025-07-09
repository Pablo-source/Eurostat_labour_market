Recent Spain population trends
================
PLR
2025-07-09

## Latest date this report was produced

Today’s date is **09 July 2025**. This report was published on the week
starting on **07 July 2025**.

## 1. Load Spain population data

We load latest Spanish Population Figures released by the Spanish
National Institute: <https://www.ine.es/consul/serie.do?d=true&s=ECP320>

In the third tab from Excel file **“INE total and foreign population
figures Spain.xlsx”** we load the table describing Total, foreign and
nationals population figures in Spain, based on Spanish nationality for
the 2004 2025 period.

    ## [1] "INE total and foreign population figures Spain.xlsx"

    ## [1] "INE_Foreign_population"       "INE_Total_population"        
    ## [3] "INE_Total_foreign_population"

``` r
population_data <- read_excel(
  here("data_demography", "INE total and foreign population figures Spain.xlsx"), 
  sheet = "INE_Total_foreign_population", skip =2, n_max = 22,
  col_types = c("guess", "numeric", "numeric", "guess","guess","guess","guess","guess")) %>% 
  clean_names() %>% 
               select (date = todas_las_edades, total_population = total,
                       foreign_population, percent_foreign_population = percent_foreign_nationals_total_population,
                       total_population_YoY_N = total_yo_y_n, total_population_YoY_perc = total_yo_y_percent,
                       foreign_population_YoY_N = foreign_nationals_yo_y_n, foreign_population_YoY_perc= foreign_total_yo_y_percent)

head(population_data)
```

    ## # A tibble: 6 × 8
    ##   date               total_population foreign_population percent_foreign_popul…¹
    ##   <chr>                         <dbl>              <dbl>                   <dbl>
    ## 1 1 de enero de 2025         49077984            6852348                   0.140
    ## 2 1 de enero de 2024         48619695            6502282                   0.134
    ## 3 1 de enero de 2023         48085361            6089620                   0.127
    ## 4 1 de enero de 2022         47486727            5509046                   0.116
    ## 5 1 de enero de 2021         47400798            5402702                   0.114
    ## 6 1 de enero de 2020         47318050            5241278                   0.111
    ## # ℹ abbreviated name: ¹​percent_foreign_population
    ## # ℹ 4 more variables: total_population_YoY_N <dbl>,
    ## #   total_population_YoY_perc <dbl>, foreign_population_YoY_N <dbl>,
    ## #   foreign_population_YoY_perc <dbl>

``` r
names(population_data)
```

    ## [1] "date"                        "total_population"           
    ## [3] "foreign_population"          "percent_foreign_population" 
    ## [5] "total_population_YoY_N"      "total_population_YoY_perc"  
    ## [7] "foreign_population_YoY_N"    "foreign_population_YoY_perc"

## 2. Cleanse Spain populations figures data

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
    ##  1 1 de enero de 2025         49077984            6852348 2025 
    ##  2 1 de enero de 2024         48619695            6502282 2024 
    ##  3 1 de enero de 2023         48085361            6089620 2023 
    ##  4 1 de enero de 2022         47486727            5509046 2022 
    ##  5 1 de enero de 2021         47400798            5402702 2021 
    ##  6 1 de enero de 2020         47318050            5241278 2020 
    ##  7 1 de enero de 2019         46918951            4850762 2019 
    ##  8 1 de enero de 2018         46645070            4577322 2018 
    ##  9 1 de enero de 2017         46497393            4417653 2017 
    ## 10 1 de enero de 2016         46418884            4419334 2016 
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
    ##  1 2004          42547454                 NA                NA
    ##  2 2005          43296335            3430204          39866131
    ##  3 2006          44009969            3930916          40079053
    ##  4 2007          44784659            4449434          40335225
    ##  5 2008          45668938            5086295          40582643
    ##  6 2009          46239271            5386659          40852612
    ##  7 2010          46486621            5402579          41084042
    ##  8 2011          46667175            5312440          41354735
    ##  9 2012          46818216            5236030          41582186
    ## 10 2013          46712650            5064584          41648066
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
  geom_text(aes(label = total_population),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,55000000), expand = FALSE )

Spanish_population_plot
```

![](Recent-population-trends-in-Spain-GitHub-output_files/figure-gfm/Spain%20total%20population%20bar%20plot-1.png)<!-- -->

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

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_bar()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

![](Recent-population-trends-in-Spain-GitHub-output_files/figure-gfm/Spain%20foreign%20population%20bar%20plot-1.png)<!-- -->

This last plot displays national population in Spain for the 2005-2025
period

``` r
options(scipen=999)

national_population_plot <- INE_calc_fields %>% 
                            ggplot(aes(x= Year, y = Spanish_nationals)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Spanish nationals population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = Spanish_nationals),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,46000000), expand = FALSE )

national_population_plot
```

![](Recent-population-trends-in-Spain-GitHub-output_files/figure-gfm/Spain%20national%20population%20bar%20plot-1.png)<!-- -->

## 3. Spain population figures Overveiw details

In this section we highlight a couple of trends observed in both Total
and foreign population in Spain from 2004 until 2025 period.

- As of 1st of January **2025**., the year for the latest available
  population figures, Total population in Spain was **49,077,984**. Up
  by 458,253 from previous year.

- In contrast, on the year **2004**, the first year on this series,
  total population in Spain was **42,547,454**.

- In terms of foreign population, on the following year **2005**.there
  was a foreign population of **3,430,204**

- At the end of the series on 1st January **2025**, latest foreign
  population figures in Spain was **6,852,348**. We will describe these
  population changes in absolute figures and percent change in the next
  section below.

### 3.1 Share of foreign population over total population in Spain

Next, we describe the foreign population percent share of the total
population in Spain for the 2004-2025 period.

## 4. Year on year increase in total and forein population in Spain

In this section we will include several calculations to describe the
year on year change in Total, national and foreign population in Spain.
And also we will include population change for the entire period
considered 2004-2025.

- This is a population increase of () from the initial figure of
  **42,547,454**, in **2004**.
