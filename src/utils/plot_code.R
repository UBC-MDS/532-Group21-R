library(here)
library(tidyverse)
library(plotly)

df <- read_csv(here("data", "processed", "gapminder_processed.csv"))


get_topbtm_data <- function(data, stat, year_list = c(1968, 2015)){
  top_countries <- data %>% filter(year == year_list[2]) %>%
    arrange(desc(!!sym(stat))) %>%
    slice(1:5) %>%
    pull(country)
  
  data_return <- data %>% filter(country %in% top_countries)
}


plot_line <- function(stat = "children_per_woman"){
  data <- get_topbtm_data(df, stat)
  
  line_plot <- ggplot(data, aes(x = year,
                                   y = !!sym(stat),
                                   color = country)) + 
    geom_line()
  
  line_plot
}


plot_bar <- function(stat = "children_per_woman") {
  data <- get_topbtm_data(df, stat)
  
  bar_plot <- ggplot(data, aes(x = !!sym(stat),
                               y = reorder(country, !!sym(stat)),
                               color = country,
                               name = country,
                               count = !!sym(stat))) + 
    geom_bar(stat = "identity") + 
    ylab("country")
  bar_plot
}


plot_map <- function(stat = "children_per_woman") {
  data <- df
  
  map_plot <- plot_ly(data,
                      type = 'choropleth', 
                      locations = ~code, 
                      z = data[[stat]], 
                      text = ~country,
                      color = data[[stat]],
                      colors = 'Purples') %>%
    layout(title = stat)
  
  map_plot
}


plot_map(stat = "life_expectancy")

#plot_line(stat = "child_mortality")

#plot_bar(stat = "child_mortality")

