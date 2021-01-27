library(ggplot2)
library(tidyverse)
library(plotly)


plot_bar <- function(data, stat = "children_per_woman") {
  bar_chart <- ggplot(data, aes(x = !!sym(stat),
                                    y = reorder(country, !!sym(stat)),
                                    color = country,
                                    name = country,
                                    count = !!sym(stat))) + 
    geom_bar(stat = "identity") + 
    ylab("country")
  ggplotly(bar_chart, tooltip = c('name', 'count'))
}


df <- read.csv('gapminder_processed.csv') %>%
  filter(year == 2018) %>% 
  arrange(desc(children_per_woman)) %>%
  slice(1:5)

plot_bar(df)