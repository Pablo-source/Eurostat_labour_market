# Eurostat_labour_market
**Parameterized reporting project** using Eurostat, ONS, INE, and OCDE official statistics

- This project will combine different statistical sources from several European countries (**Spain, Greece, France, Germany, Italy, Greece**) as well as statistics from the **UK** to compare **social conditions of the population** across different topics such as **employment**, **migration** and **social determinants of health**.

- First section will provide general **ggplot2** charts with **customised formatting** and **small multiples** statistics to easy comparison of metrics across several countries and once.
- Then I will create a **markdown document** to show how different charts can be combined to feed a **markdown report template**
- Following this **markdown reports** y will create several **Quarto reports** to display information about Immigration changes over last 10 years across different countries and social conditions such as changes in the **Active** and **Inactive** rates across countries and job market conditions (temporary contracts and unmemployment).
- Finally I will provide an example on how to design and replicate a **Parameterized** report populated with the above set of indicators. This Parametrized report will allow to tweak several parameters to produce different reports for several countries based on a single set of **parameters**, that will be fed to the **quarto** report everytime it runs.
- This project intends to be an example on how to apply **RAP** (Reproducible Analytical Pipelines)  and **DRY** (Don't Repeat Yoursefl) principles, in a work environment that would need to automate any reporting.
- For further details on how to turn this project into a **pipeline** plese this this other repo I created some months ago: <https://github.com/Pablo-source/targets-test>. Using {targets} package with several **pipelines examples** ready to be cloned from the repo and run them locally. Details for {Targets} pacakge from its GitHub repo: <https://github.com/ropensci/targets>


## Parameterized report
- A **parameterized report** is a report that accepts input values, called parameters, which are used to customize the report’s output.
- This will allow me to create a single template to generate multiple reports from a single script, by varying the parameter values. 


## Section 01. Small multiples. Temporary employment and unemployment in the EU by conuntry

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

## Plot 01. Faceted line-graph selected countries applying custom legend and colours  
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






