Recent Spain population trends
================
PLR
2026-09-13

## Latest date this report was produced

Today’s date is **13 September 2026**. This report was published on the
week starting on **11 September 2026**.

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

population_data <- population_data %>% select(date,total_population,foreign_nationals_population = foreign_population,percent_foreign_population,total_population_YoY_perc,foreign_population_YoY_perc)

population_data <- population_data %>% 
  mutate(date_day = substr(date,1,1),date_month = substr(date,3,13),date_year =  substr(date,15,18))

population_data_fmt <-population_data %>% 
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
population_data_fmt
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
min_date <- min(population_data_fmt$date_fmt)
max_date <- max(population_data_fmt$date_fmt)
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
  } else if (column %in% c("date_fmt")){
  return(format(value,"%b %Y"))  # Format dates 
  }
  
  
}
# format_total_population_spain(mydataset = population_change_fmt, my_date = "2007-01-01",column = "total_population")
# format_total_population_spain(mydataset = population_change_fmt, my_date = "2007-01-01",column = "percent_foreign_population")
```

``` r
Plot_total_population_spain <- population_data_fmt %>% select(date_fmt,total_population) %>% 
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

In Jan 2007. Spain total population was 44,784,659. Eleven years later,
in Jan 2018 it was 46,645,070. After Covid19 Pandemic it grew steadily
until 2022, when it reached 47,486,727 on Jan 2022

It is noticeable that since 2022, mainly due to net international
migration from foreign-born residents, Spain total population grew with
YoY rates above 1% for the following four years. In 2023 Spain total
population reached 48,085,361 One year later, on Jan 2024 it was
48,619,695

The same fast pace of increase has continued in 2025 49,128,297 And in
2026 with the recently released provisional population estimates, the
Total population in Spain has reached nearly 50 million 49,596,376

## 2.2 Spain total population change

This sub-section describes how Total population in Spain has changed
over time in the 2005-2026 period. Describing population change in
absolute numbers and percentage change Year on Year increase.

``` r
population_data_fmt <-  population_data_fmt %>%
                         select(date = date_fmt,total_population,foreign_nationals_population) %>% 
                         mutate(spanish_nationals_population = total_population - foreign_nationals_population)
population_data_fmt
```

    ## # A tibble: 22 × 4
    ##    date       total_population foreign_nationals_popula…¹ spanish_nationals_po…²
    ##    <date>                <dbl>                      <dbl>                  <dbl>
    ##  1 2005-01-01         43296335                    3430204               39866131
    ##  2 2006-01-01         44009969                    3930916               40079053
    ##  3 2007-01-01         44784659                    4449434               40335225
    ##  4 2008-01-01         45668938                    5086295               40582643
    ##  5 2009-01-01         46239271                    5386659               40852612
    ##  6 2010-01-01         46486621                    5402579               41084042
    ##  7 2011-01-01         46667175                    5312440               41354735
    ##  8 2012-01-01         46818216                    5236030               41582186
    ##  9 2013-01-01         46712650                    5064584               41648066
    ## 10 2014-01-01         46495744                    4676352               41819392
    ## # ℹ 12 more rows
    ## # ℹ abbreviated names: ¹​foreign_nationals_population,
    ## #   ²​spanish_nationals_population

We can display the above table using GT package

``` r
Spain_nationlity_gt<- population_data_fmt %>% 
  gt() %>%
  tab_header(
    title = md("**Spanish population by nationality**"),
    subtitle = ("2005-2026 period")
  ) 
Spain_nationlity_gt
```

<div id="owjroxmeyh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#owjroxmeyh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#owjroxmeyh thead, #owjroxmeyh tbody, #owjroxmeyh tfoot, #owjroxmeyh tr, #owjroxmeyh td, #owjroxmeyh th {
  border-style: none;
}
&#10;#owjroxmeyh p {
  margin: 0;
  padding: 0;
}
&#10;#owjroxmeyh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#owjroxmeyh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#owjroxmeyh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#owjroxmeyh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#owjroxmeyh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#owjroxmeyh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#owjroxmeyh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#owjroxmeyh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#owjroxmeyh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#owjroxmeyh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#owjroxmeyh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#owjroxmeyh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#owjroxmeyh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#owjroxmeyh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#owjroxmeyh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owjroxmeyh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#owjroxmeyh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#owjroxmeyh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#owjroxmeyh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owjroxmeyh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#owjroxmeyh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owjroxmeyh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#owjroxmeyh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owjroxmeyh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#owjroxmeyh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owjroxmeyh .gt_left {
  text-align: left;
}
&#10;#owjroxmeyh .gt_center {
  text-align: center;
}
&#10;#owjroxmeyh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#owjroxmeyh .gt_font_normal {
  font-weight: normal;
}
&#10;#owjroxmeyh .gt_font_bold {
  font-weight: bold;
}
&#10;#owjroxmeyh .gt_font_italic {
  font-style: italic;
}
&#10;#owjroxmeyh .gt_super {
  font-size: 65%;
}
&#10;#owjroxmeyh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#owjroxmeyh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#owjroxmeyh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#owjroxmeyh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#owjroxmeyh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#owjroxmeyh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#owjroxmeyh .gt_indent_5 {
  text-indent: 25px;
}
&#10;#owjroxmeyh .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#owjroxmeyh div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_title gt_font_normal" style><span class='gt_from_md'><strong>Spanish population by nationality</strong></span></td>
    </tr>
    <tr class="gt_heading">
      <td colspan="4" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>2005-2026 period</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="date">date</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="total_population">total_population</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="foreign_nationals_population">foreign_nationals_population</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="spanish_nationals_population">spanish_nationals_population</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="date" class="gt_row gt_right">2005-01-01</td>
<td headers="total_population" class="gt_row gt_right">43296335</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">3430204</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">39866131</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2006-01-01</td>
<td headers="total_population" class="gt_row gt_right">44009969</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">3930916</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">40079053</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2007-01-01</td>
<td headers="total_population" class="gt_row gt_right">44784659</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4449434</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">40335225</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2008-01-01</td>
<td headers="total_population" class="gt_row gt_right">45668938</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5086295</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">40582643</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2009-01-01</td>
<td headers="total_population" class="gt_row gt_right">46239271</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5386659</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">40852612</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2010-01-01</td>
<td headers="total_population" class="gt_row gt_right">46486621</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5402579</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41084042</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2011-01-01</td>
<td headers="total_population" class="gt_row gt_right">46667175</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5312440</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41354735</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2012-01-01</td>
<td headers="total_population" class="gt_row gt_right">46818216</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5236030</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41582186</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2013-01-01</td>
<td headers="total_population" class="gt_row gt_right">46712650</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5064584</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41648066</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2014-01-01</td>
<td headers="total_population" class="gt_row gt_right">46495744</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4676352</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41819392</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2015-01-01</td>
<td headers="total_population" class="gt_row gt_right">46425722</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4453985</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41971737</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2016-01-01</td>
<td headers="total_population" class="gt_row gt_right">46418884</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4419334</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41999550</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2017-01-01</td>
<td headers="total_population" class="gt_row gt_right">46497393</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4417653</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42079740</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2018-01-01</td>
<td headers="total_population" class="gt_row gt_right">46645070</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4577322</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42067748</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2019-01-01</td>
<td headers="total_population" class="gt_row gt_right">46918951</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">4850762</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42068189</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2020-01-01</td>
<td headers="total_population" class="gt_row gt_right">47318050</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5241278</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42076772</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2021-01-01</td>
<td headers="total_population" class="gt_row gt_right">47400798</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5402702</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41998096</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2022-01-01</td>
<td headers="total_population" class="gt_row gt_right">47486727</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">5509046</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41977681</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2023-01-01</td>
<td headers="total_population" class="gt_row gt_right">48085361</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">6089620</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">41995741</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2024-01-01</td>
<td headers="total_population" class="gt_row gt_right">48619695</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">6502282</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42117413</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2025-01-01</td>
<td headers="total_population" class="gt_row gt_right">49128297</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">6911971</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42216326</td></tr>
    <tr><td headers="date" class="gt_row gt_right">2026-01-01</td>
<td headers="total_population" class="gt_row gt_right">49596376</td>
<td headers="foreign_nationals_population" class="gt_row gt_right">7256796</td>
<td headers="spanish_nationals_population" class="gt_row gt_right">42339580</td></tr>
  </tbody>
  &#10;</table>
</div>

This data below shows population change by year for total population,
spanish nationals and foreign nationals population

``` r
population_change_data<-  population_data_fmt %>%
                          mutate(total_change = total_population - lag(total_population),
                                 foreig_nationals_change = foreign_nationals_population - lag(foreign_nationals_population),
                                 spanish_national_change = spanish_nationals_population - lag(spanish_nationals_population),
                                 )
population_change_data
```

    ## # A tibble: 22 × 7
    ##    date       total_population foreign_nationals_popula…¹ spanish_nationals_po…²
    ##    <date>                <dbl>                      <dbl>                  <dbl>
    ##  1 2005-01-01         43296335                    3430204               39866131
    ##  2 2006-01-01         44009969                    3930916               40079053
    ##  3 2007-01-01         44784659                    4449434               40335225
    ##  4 2008-01-01         45668938                    5086295               40582643
    ##  5 2009-01-01         46239271                    5386659               40852612
    ##  6 2010-01-01         46486621                    5402579               41084042
    ##  7 2011-01-01         46667175                    5312440               41354735
    ##  8 2012-01-01         46818216                    5236030               41582186
    ##  9 2013-01-01         46712650                    5064584               41648066
    ## 10 2014-01-01         46495744                    4676352               41819392
    ## # ℹ 12 more rows
    ## # ℹ abbreviated names: ¹​foreign_nationals_population,
    ## #   ²​spanish_nationals_population
    ## # ℹ 3 more variables: total_change <dbl>, foreig_nationals_change <dbl>,
    ## #   spanish_national_change <dbl>

### 2.2.1 Spain population change by nationality

For each population by nationality in Spain (Spanish nationals and
foreign population), this section describes its year on year change in
absolute numbers and year on year percentage change.

- Foreign population: Resident population figures for people who do not
  hold Spanish nationality.

- Spanish nationals population: Resident population figures in Spain for
  Spanish nationals.

Also, here we describe the share of the Total population change
accounted for Spanish and Foreign nationals. This will allow later on,
once we account for Natural increase, what percentage of population
change is due mainly to migration flows from Foreign nationals in the
overall population in Spain for the 2005-2026 period.

## 3. Total population in Spain by nationality - 2005-2026 period

This section describes population figures from the Municipal Census in
Spain for the 2005-2026 period. Providing a breakdown of Total
population by nationality Spanish and foreign nationals.

- Total population: Is the combined population figure of Spanish
  nationals and foreign population.

- Foreign population: Resident population figures for people who do not
  hold Spanish nationality, classified by the National Statistics
  Institute INE as foreign nationals.

- Spanish nationals population: Resident population figures in spain for
  Spanish nationals.

``` r
Spain_population_nationality_fmt <- population_data_fmt %>%                          
                                    select(date,total_population,foreign_nationals_population) %>% 
                         arrange(date)
Spain_population_nationality_fmt
```

    ## # A tibble: 22 × 3
    ##    date       total_population foreign_nationals_population
    ##    <date>                <dbl>                        <dbl>
    ##  1 2005-01-01         43296335                      3430204
    ##  2 2006-01-01         44009969                      3930916
    ##  3 2007-01-01         44784659                      4449434
    ##  4 2008-01-01         45668938                      5086295
    ##  5 2009-01-01         46239271                      5386659
    ##  6 2010-01-01         46486621                      5402579
    ##  7 2011-01-01         46667175                      5312440
    ##  8 2012-01-01         46818216                      5236030
    ##  9 2013-01-01         46712650                      5064584
    ## 10 2014-01-01         46495744                      4676352
    ## # ℹ 12 more rows

``` r
nationality_stacked <- Spain_population_nationality_fmt %>% 
  pivot_longer(!date, names_to = "nationality", values_to = "population")

nationality_stacked_plot <- ggplot(nationality_stacked,
                                   aes(x = date, y = population, fill = nationality)) +
                                   geom_bar(stat = "identity") +
                              labs(title = "Spain population by natioanlity. 2005-2025",
                              substile = "Source: INE Spanish Office for National Statistics") +
                                   scale_fill_brewer() +
                                   scale_fill_discrete(labels = c("Foreign nationals", "Spanish nationals")) + theme_light() 

nationality_stacked_plot
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20population%20by%20nationality-1.png)<!-- -->

These charts below describe the evolution of Total, foreign and spanish
nationals population for the 2005-2025 period:

- Spain total population

``` r
options(scipen=999)

Spain_total_population_plot <- Spain_population_nationality_fmt %>% 
                            ggplot(aes(x= date, y = total_population)) +
  geom_bar(stat = "identity", fill = "darkolivegreen2") +
  labs(title = "Spain total poulation. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = total_population),size = 2.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,55000000), expand = FALSE )

Spain_total_population_plot
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20total%20population%20bar%20plot-1.png)<!-- -->

Then this is the foreign population in Spain for the same time period.

``` r
options(scipen=999)

forign_population_plot <- Spain_population_nationality_fmt %>% 
                            ggplot(aes(x= date, y = foreign_nationals_population)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "Foreign population in Spain. 2005-2025 period",
       substile = "Source: INE Spanish Office for National Statistics") +
  theme_light() +
  theme(axis.text.x = element_text(angle = + 90, hjust = 0.5, vjust = 0.5)) +
  geom_text(aes(label = foreign_nationals_population),size = 1.8,position = position_dodge(width = 0.2),vjust = -0.30,hjust = 0.50) +
  coord_cartesian( ylim=c(0,7500000), expand = FALSE )

forign_population_plot
```

![](Recent-population-trends-in-Spain_files/figure-gfm/Spain%20foreign%20population%20bar%20plot-1.png)<!-- -->

This last plot displays national population in Spain for the 2005-2025
period.

``` r
options(scipen=999)

national_population_data <- Spain_population_nationality_fmt %>% 
  mutate(Spanish_nationals = total_population - foreign_nationals_population)

national_population_plot <- national_population_data %>% 
                            ggplot(aes(x= date, y = Spanish_nationals)) +
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

- As of 1st of January **2026-01-01**., the year for the latest
  available population figures, Total population in Spain was
  **49,596,376**. Up by 458,253 from previous year.

- In contrast, on the year **2005-01-01**, the first year on this
  series, total population in Spain was **43,296,335**.

### 3.1 Share of foreign population over total population in Spain

Next, we describe the foreign population percent share of the total
population in Spain for the 2004-2025 period.
