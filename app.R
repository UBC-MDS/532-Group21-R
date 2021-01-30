library(devtools)
library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(here)
library(purrr)
library(ggplot2)
library(plotly)


# Read in global data
gapminder <- read_csv(here("data", "processed", "gapminder_processed_codes.csv"))



#control panel


control_panel <- dbcCard(
  list(
    # control panel title
    htmlH2("Control Panel", className = "text-center"),
    
    
    # filter for Statistic of Interest
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("1. Statistic of Interest", className = "text-left"),
      dccRadioItems(
        id = "stat",
        options = list(
          list("label" = "Life Expectancy", "value" = "life_expectancy"),
          list("label" = "Education Ratio", "value" = "education_ratio"),
          list("label" = "Population Density", "value" = "pop_density"),
          list("label" = "Child Mortality", "value" = "child_mortality"),
          list("label" = "Children per Woman", "value" = "children_per_woman")
        ),
        value = "education_ratio",
        labelStyle = list("display" = "block")
      )
    )),
    
    
    
    # filter for Region
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("2. Region", className = "text-left"),
      dccDropdown(
        id = "region",
        options = list(
          list(label = "Asia", value = "Asia"),
          list(label = "Europe", value = "Europe"),
          list(label = "Africa", value = "Africa"),
          list(label = "Americas", value = "Americas"),
          list(label = "Oceania", value = "Oceania")
        ),
        value = NULL,
        style = list("width" = "350px", "color" = "#212121")
      )
    )),
    
    # filter for Sub Region
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("3. Sub Region", className = "text-left"),
      dccDropdown(
        id = "sub_region",
        value = NULL,
        style = list("width" = "350px", "color" = "#212121")
      )
    )),
    
    # filter for Income Group
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("4. Income Group", className = "text-left"),
      dccDropdown(
        id = "income_grp",
        options = list(
          list(label = "Low", value = "Low"),
          list(label = "Lower middle", value = "Lower middle"),
          list(label = "Upper middle", value = "Upper middle"),
          list(label = "High", value = "High")
        ),
        value = NULL,
        style = list("width" = "350px", "color" = "#212121")
      )
    )),
    
    # filter for population size
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("5. Population Size", className = "text-left"),
      dccRangeSlider(
        id = "pop_size",
        min = 1e4,
        max = 1.5e9,
        step = 1e7,
        marks = list(
                "10000" = list("label" = "10,000", "style" = list("color"= "white")),
                "200000000" = list("label" = "200M", "style" = list("color"= "white")),
                "500000000" = list("label" = "500M", "style" = list("color"= "white")),
                "800000000" = list("label" = "0.8B", "style" = list("color"= "white")),
                "1100000000" = list("label" = "1.1B", "style" = list("color"= "white")),
                "1500000000" = list("label" = "1.5B", "style" = list("color"= "white"))
        ),
        value = list(10000, 1500000000)
      )
    )),
    
    
    #filter for year
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("6. Year", className = "text-left"),
      dccRangeSlider(
        id = "year_range",
        min = 1968,
        max = 2015,
        step = 1,
        marks = list(
                  "1968" = list("label" = "1968", "style" = list("color"= "white")),
                  "1980" = list("label" = "1980", "style" = list("color" = "white")),
                  "2000" = list("label" = "2000", "style" = list("color" = "white")),
                  "2010" = list("label" = "2010", "style" = list("color" = "white")),
                  "2015" = list("label" = "2015", "style" = list("color" = "white"))
          ),
        value = list(1968, 2015)
      )
    )),
    
    
    # filter for top5/bot5
    htmlHr(),
    dbcFormGroup(list(
      htmlH5("7. Show me", className = "text-left"),
      dccRadioItems(
        id = "top_btm",
        options = list(
          list("label" = "Top 5 Countries", "value" = "Top"),
          list("label" = "Bottom 5 Countries", "value" = "Bottom")
        ),
        value = "Bottom",
        labelStyle = list("display" = "block")
      )
    )),
    htmlSmall(
      "*Education Ratio calculated as # of years in school men / # of years in school women. Higher values indicate larger gap between the education levels for men and women."
    )
  ),
  color = "secondary",
  inverse = TRUE,
  body = TRUE
  
)


# #frames 
# world_map <- htmlIframe(
#   id = "world_map",
#   style = list(
#     "border-width" = "0",
#     "width" = "100%",
#     "height" = "600px"
#   )
# )


#helper functions

get_topbtm_data <- function(data, stat, top_btm, year_range){
  #'@description Filter data based on top 5 or bottom 5 countries selection
  #'
  #'@param data dataframe.  Data to be filtered
  #'@param stat string. Selection from statistic of interest filter
  #'@param top_btm string. Selection from Top/Bottom filter
  #'@param year integer. Year for which the data is displayed, from Year filter
  #'
  #'@returns data dataframe. dataset that has been filtered by top 5 or bottom 5 countries
  #'
  #'@example get_topbtm_data(data, "education_ratio", "Bottom", [1968, 2015]) 
  
  top_countries <- data %>% 
    filter(year == max(unlist(year_range))) %>%
    arrange(desc(!!sym(stat))) %>%
    slice(1:5) %>%
    pull(country)
  
  btm_countries <- data %>% 
    filter(year == max(unlist(year_range))) %>%
    arrange(-desc(!!sym(stat))) %>%
    slice(1:5) %>%
    pull(country)
  
  if (!!sym(top_btm) == "Top") {
    data = data %>%
      filter(country %in% top_countries)
  }else{
    data = data %>%
      filter(country %in% btm_countries)
  }
  data
}


filter_data <- function(region_, sub_region_, income_grp){
  #'@description Filter data based on region, sub region and income group selection
  #'
  #'@param region string. Selection from the Region filter
  #'@param sub_region string. Selection from Sub Region filter
  #'@param income_grp string. Selection from Income Group filter
  #'
  #'@returns data dataframe. dataframe that has been filtered on region, sub region and income group selection
  #'
  #'@example filter_data("Asia", "Western Asia", "Lower middle")
  region_ <- unlist(region_)
  sub_region_ <- unlist(sub_region_)
  income_grp <- unlist(income_grp)
  
  if (!is.null(region_) && !is.null(sub_region_) && !is.null(income_grp)) {
      data <- gapminder %>%
        filter(region == region_, 
             sub_region == sub_region_, 
             income_group == income_grp)
  } else if(!is.null(region_) && is.null(sub_region_) && is.null(income_grp)){
            data <- gapminder %>%
              filter(region == region_)
  } else if(is.null(region_) && !is.null(sub_region_) && is.null(income_grp)){
            data <- gapminder %>%
              filter(sub_region == sub_region_)
  }else if(is.null(region_) && is.null(sub_region_) && !is.null(income_grp)){
            data <- gapminder %>%
              filter(income_group == income_grp)
  }else if(!is.null(region_) && !is.null(sub_region_) && is.null(income_grp)){
            data <- gapminder %>%
              filter(region == region_, 
                     sub_region == sub_region_)
  }else if(is.null(region_) && !is.null(sub_region_) && !is.null(income_grp)){
            data <- gapminder %>%
              filter(sub_region == sub_region_, 
                     income_group == income_grp)
  }else if(!is.null(region_) && is.null(sub_region_) && !is.null(income_grp)){
          data <- gapminder %>%
            filter(region == region_, 
                   income_group == income_grp)
  }else{
    data = gapminder
  }
  data
}

# Create dictionary for stat labels
labels <- list(
  "life_expectancy" = "Life Expectancy",
  "education_ratio" = "Education Ratio",
  "pop_density" = "Population Density",
  "child_mortality" = "Child Mortality",
  "children_per_woman" = "Children per Woman")


app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(dbcContainer(list(
  htmlDiv(
    style = list(
      "textAlign" = "center",
      "color" = "DarkSlateGray",
      "font-size" = "26px"
    ),
    children = htmlH1("GapExpresser")
  ),
  
  htmlHr(),
  dbcRow(list(dbcCol(control_panel,
                     md = 4,),
              dbcCol(list(
                dbcRow(dccGraph(id = "map")),
                dbcRow(list(
                  dbcCol(dccGraph(id = "bar"), md=6), 
                  dbcCol(dccGraph(id = "line_chart"), md=6)
                  )),
                htmlSmall(
                            "Note: empty plots mean that we don't have data based on your selection"
                        )
              ), md = 8
              )),
         align = "center")
),
fluid = TRUE,))


app$callback(
  output("sub_region", "options"),
  list(input("region", "value")),
  function(selection) {
    #'@description Select sub regions to display based on region filter selection
    #'
    #'@param region string. Selection from the Region filter
    #'
    #'@returns opt list. Options list for sub region belonging to the selected region
    #'
    #'
    #'@example
    #'get_subregion("Asia")

    if (!is.null(unlist(selection))){
      subs <- gapminder %>%
        filter(region == selection) %>%
        select(sub_region) %>%
        unique() %>%
        pull()
    }else{
      subs <- gapminder %>%
        select(sub_region) %>%
        unique() %>%
        pull()
    }
    
    opts = list()
    i = 1
    for (sub in subs){
      sublist <- list("value" = sub, "label" = sub)
      opts[[i]] <- sublist
      i <- i + 1
    }
    opts
  }
)


app$callback(
  output("bar", "figure"),
  list(input("stat", "value"),
       input("region", "value"),
       input("sub_region", "value"),
       input("income_grp", "value"),
       input("top_btm", "value"),
       input("year_range", "value")),
  function(stat, region, sub_region, income_grp, top_btm, year_range){
    #'@description Create bar chart for statistic of interested based on selected filters, for top 5 or bottom 5 countries 
    #'
    #'@param region string. Selection from the Region filter
    #'@param sub_region string. Selection from Sub Region filter
    #'@param income_grp string. Selection from Income Group filter
    #'@param tom_btm string.  Selection from Top/Bottom filter
    #'@param year_range list. Selection from Year filter
    #'
    #'@returns bar chart. bar chart showing statistic of interest for top 5 or bottom 5 countries,
    #          in specific region, sub-region, income group and year
    #'
    #'@example plot_bar("education_ratio", "Asia", "Western Asia", "Lower middle", "Bottom", [1968, 2015])

    # filter by Region, sub-region & Income group
    data <- filter_data(region, sub_region, income_grp)
    
    # filter on top/bottom selection
    data <- get_topbtm_data(data, stat, top_btm, year_range)

    #filter on year
    data <- data %>%
      filter(year == max(unlist(year_range)))

    bar <- ggplot(data) +
      aes(x = !!sym(stat), 
          y = reorder(country, !!sym(stat)), 
          fill = country) +
      geom_bar(stat = "identity") +
      labs(title = paste0(labels[[stat]], " - ", top_btm, " 5 Countries for ", max(unlist(year_range))), 
           x = labels[[stat]], 
           y = "Country", 
           fill = "Country") + 
      theme_classic() + 
      ggthemes::scale_fill_tableau()
    
    ggplotly(bar)
  }
)


app$callback(
  output("line_chart", "figure"),
  list(input("stat", "value"),
       input("region", "value"),
       input("sub_region", "value"),
       input("income_grp", "value"),
       input("top_btm", "value"),
       input("year_range", "value")),
  function(stat, region, sub_region, income_grp, top_btm, year_range){
    #'@description Create map plot for statsitic of interested based on selected filters
    #'
    #'@param region string. Selection from the Region filter
    #'@param sub_region string. Selection from Sub Region filter
    #'@param income_grp string. Selection from Income Group filter
    #'@param tom_btm string.  Selection from Top/Bottom filter
    #'@param year_range list. Selection from Year filter
    #'
    #'@returns map chart. map chart showing statistic of interest for specific region, subregion, income group and year
    #'
    #'@example plot_map("education_ratio", "Asia", "Western Asia", "Lower middle", [1968, 2015])
    
    # filter by Region, sub-region & Income group
    data <- filter_data(region, sub_region, income_grp)
    
    # filter on top/bottom selection
    data <- get_topbtm_data(data, stat, top_btm, year_range)

    #filter on year
    data <- data %>%
      filter(year %in% (min(unlist(year_range)):max(unlist(year_range))))
    
    line <- ggplot(data) +
      aes(x = year, 
          y = !!sym(stat), 
          color = country) +
      geom_line() +
      labs(title = paste0(labels[[stat]], " Trend - ", top_btm, " 5 Countries from ", min(unlist(year_range)), "-", max(unlist(year_range))),
           x = "Year", 
           y = labels[[stat]], 
           colour = "Country") + 
      theme_classic() + 
      ggthemes::scale_color_tableau()

    ggplotly(line)
  }
)


app$callback(
  output("map", "figure"),
  list(input("stat", "value"),
       input("region", "value"),
       input("sub_region", "value"),
       input("income_grp", "value"),
       input("top_btm", "value"),
       input("year_range", "value")),
  function(stat, region, sub_region, income_grp, top_btm, year_range){
    #'@description Create line chart for statistic of interested based on selected filters, for top 5 or bottom 5 countries
    #'
    #'@param region string. Selection from the Region filter
    #'@param sub_region string. Selection from Sub Region filter
    #'@param income_grp string. Selection from Income Group filter
    #'@param tom_btm string.  Selection from Top/Bottom filter
    #'@param year_range list. Selection from Year filter
    #'
    #'@returns line chart. line chart showing statistic of interest for top 5 or bottom 5 countries,
    #          in specific region, sub-region, income group and year range
    #'
    #'@example plot_line("education_ratio", "Asia", "Western Asia", "Lower middle", "Bottom", [1968, 2015])

    # filter by Region, sub-region & Income group
    data <- filter_data(region, sub_region, income_grp)

    #filter on year
    data <- data %>%
      filter(year == max(unlist(year_range)))
    
    map_plot <- plot_ly(data,
                        type = 'choropleth', 
                        locations = ~code, 
                        z = data[[stat]], 
                        text = ~country,
                        color = data[[stat]],
                        colors = 'Purples') %>%
      layout(title = paste0(labels[[stat]], " by Country for ", max(unlist(year_range))))

    ggplotly(map_plot)
  }
)


app$run_server(host = '0.0.0.0', debug = F)

