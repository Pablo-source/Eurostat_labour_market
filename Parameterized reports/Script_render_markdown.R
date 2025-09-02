# How to render a Markdown report from an R Script

# A) Render Markdown report:
  # Script: "Script_render_markdown.R"
  # This script will execute this Markdown report:
  # 05 basic_param_report_figures_diamonds_data_toggle_FALSE.Rmd
  rmarkdown::render("05 basic_param_report_figures_year_2009.Rmd",
                    params = list(
                      data = "mpg",
                      year = 2019,
                      toggle = TRUE
                    ))


