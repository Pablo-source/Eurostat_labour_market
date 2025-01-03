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
![Eurostat_labour_market_subset_01_02](https://github.com/user-attachments/assets/e453461d-d913-412d-8036-cf91a4fedf45)


## Plot 02-02. Faceted line-graph selected countries Indicator
- Set of countries: euro_area_20_countries_from_2023,malta,netherlands,austria,poland,portugal,romania,slovenia,
  slovakia,finland,sweden,iceland,norway,switzerland,bosnia_and_herzegovina,montenegro,north_macedonia,serbia,turkiye
-  Temporary Employment and unemployment in EU countries - Subset 02 02- 2003-2023 period. Yearly data
![Eurostat_labour_market_subset_02_02](https://github.com/user-attachments/assets/3dee307d-ed46-4347-a8a6-0295965eac75)

## Plot 03. Faceted line-graph selected countries applying custom legend and colours  
- As we can see in script "05_Faceted_charts_custom_line_colour.R, we can apply custom legend text and colours
- Applied scale_colour_manual() function
- scale_colour_manual(values = c("blue", "black"),labels = c("Temporary contracts rate (%)","Unemployment rate (%)"))
- Custom legend applied in chart below:

  ![Eurostat_batch_01_custom_legends](https://github.com/user-attachments/assets/3ca93c83-c58b-4c76-8d5c-e49386e8120e)
