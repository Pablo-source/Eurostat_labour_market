# 01 Plots for Markdown reports
# wip

# Function 01: Create charts to populate markdown report

population_plots <- function(dataset, metric_name, data_column, chart_type = NULL ) {
  
  Plots_data <- dataset %>% filter(metric == metric_name)
  metric_value <- Plots_data %>% pull {{data_column}}
  
  # 1 Data manipulation
    if (length(value) == 0) {return (NA)}
  
  
  # 2. Formatting
  if (format == "numeric") {
    return(as.character(prettyNum(metric_value, big.mark = ",")))
  }
  
  # 3. Choopse plot
  
  if (is.null(chart_type)){
        ## Default chart to preseet metric against date
  
    } else if (chart_type == "Line_chart") {
       ## Default line chart to plot metric x as a line chart
    
  } else if (chart_type == "bar chart"){
    ## Default line chart to plot metric x as a line chart
    
  }
  
}

# Execute function
# First chart_type is default option 
population_plots(dataset, metric_name = "",data_column =, chart_type)