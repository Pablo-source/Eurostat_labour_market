# Eurostat_labour_market
Parameterized reporting project using Eurostat labour market statistics

## Small multiples. Temporary employment and unemployment in the EU by conuntry

## Indicator: Unemployment rate
- Unemployment by sex and age – annual data – Online data code: une_rt_a
- Eurostat Indicator: Unemployment by sex and age – annual data
- Eurostat reference table: une_rt_a
- Downloaded file: une_rt_a__custom_14324113_page_spreadsheet.xlsx
- URL: Unemployment by sex and age – annual data. Online data code: [une_rt_a]
  <https://ec.europa.eu/eurostat/databrowser/view/une_rt_a/default/table?lang=en&category=labour.employ.lfsi.une>

## Indicator: Employed persons with temporary contract
- Part-time employment and temporary contracts – annual data - Online data code:lfsi_pt_a
- Eurostat Indicator: Employed persons with temporary contract
- Eurostat reference table: [lfsi_pt_a]. Activity and employment status (EMP_TEMP)>Employed persons with temporary contract
- Downloaded file:  lfsi_pt_a__custom_14828862_page_spreadsheet.xlsx
- URL: Part-time employment and temporary contracts – annual data - Online data code:[lfsi_pt_a]
  <https://ec.europa.eu/eurostat/databrowser/view/lfsi_pt_a__custom_14828862/default/table?lang=en>


## Plot 01-02. Faceted line-graph selected countries Indicator
- Set of countries:euro_area_20_countries_from_2023,belgium,bulgaria,czechia,denmark,germany,estonia,
  ireland,greece,spain,france,croatia,italy,cyprus,latvia,lithuania,luxembourg,hungary
- Temporary Employment and unemployment in EU countries - Subset 01 02- 2003-2023 period. Yearly data
![04_plot_countries_batch_01_two_indicators](https://github.com/user-attachments/assets/a8c122ce-481e-482a-a9ce-4cf178fd28f2)


## Plot 02-02. Faceted line-graph selected countries Indicator
- Set of countries: euro_area_20_countries_from_2023,malta,netherlands,austria,poland,portugal,romania,slovenia,
  slovakia,finland,sweden,iceland,norway,switzerland,bosnia_and_herzegovina,montenegro,north_macedonia,serbia,turkiye
-  Temporary Employment and unemployment in EU countries - Subset 02 02- 2003-2023 period. Yearly data
![05_unem_temp_batch_02_formatted](https://github.com/user-attachments/assets/63c6ee5d-a487-4c05-84e8-ea2c03ea99d2)


## Plot 03. Faceted line-graph selected countries applying custom legend and colours  
- As we can see in script "05_Faceted_charts_custom_line_colour.R, we can apply custom legend text and colours
- Applied scale_colour_manual() function
- scale_colour_manual(values = c("blue", "black"),labels = c("Temporary contracts rate (%)","Unemployment rate (%)"))
- Custom legend applied in chart below:

![Eurostat_batch_01_custom_legends](https://github.com/user-attachments/assets/3ca93c83-c58b-4c76-8d5c-e49386e8120e)

![EUCOUNTRIES_UNEMP_TEMP_EMP_batch02](https://github.com/user-attachments/assets/80d25fac-c223-4631-a154-e461a49ab839)

## Plot 04. Bar plot including annotations
- See script "**07_Annotate_curve_draw_straight_lines.R**" for details:
- Used `annotate('curve')` to include straight line annotation to specific plot value
- Included `geom_richtext()` function to add annotation buble to the chart

![17_Bar_chart_annotation_segment_text_bubble_colour_highest_value](https://github.com/user-attachments/assets/6ee84592-5bac-4804-ab5f-68ae3d9b0b27)


## Plot 05. Stacked barplot population by nationality
- See script "**09 Total population by nationality Spain 2004 2025.R**" for details:
- Used `geom_bar()` in conjuntion with `position="stack"` to create a stacked bar plot
- Also used  `scale_fill_discrete(labels=c())` to populate labels with custom text and `scale_fill_manual()` to populate legend with custom colour
- In a future Quarto document, I will explore the relationship between Immigration and Labour market conditions in some European countries and also in the UK

![23_Spain_population_by_nationality_2005_2025](https://github.com/user-attachments/assets/899f3ef8-bec9-4215-a4ee-6cd587a8fb8c)






