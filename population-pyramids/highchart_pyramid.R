setwd("demography-viz/population-pyramids/")
library(tidyverse)
library(highcharter)
library(shiny)

world <- read_csv("world_population.csv")

# Some basic preprocessing for convenience
world <- world %>%
  mutate(Age = stringr::str_replace(Age, "--", "-"))

age_levels <- world %>%
  distinct(Age) %>%
  pull(Age)

world$Age <- factor(world$Age, levels = age_levels)

# For one country

pyramid <- function(country) {
  country_data <- world %>%
    filter(Area == country)
  
  country_share <- country_data %>%
    group_by(Year, Sex) %>%
    mutate(share = Population/sum(Population)) %>%
    ungroup() %>%
    mutate(share = case_when(
      Sex == "Female" ~ -share,
      TRUE ~ share
    ))
  
  series <- country_share %>%
    group_by(Sex, Age) %>% 
    do(data = list(sequence = .$share)) %>% 
    ungroup() %>% 
    group_by(Sex) %>% 
    do(data = .$data) %>%
    mutate(name = Sex) %>% 
    list_parse()
  
  maxpop <- max(abs(country_share$share))
  
  xaxis <- list(categories = sort(unique(country_share$Age)),
                reversed = FALSE)
  
  years <-  seq(1970, 2015, by = 5)
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_motion(
      enabled = TRUE, 
      labels = years, 
      series = c(0,1), 
      autoplay = TRUE, 
      updateInterval = 5, 
      magnet = list(
        round = "floor",
        step = 0.01
      )
    ) %>% 
    hc_add_series_list(series) %>% 
    hc_plotOptions(
      series = list(stacking = "normal"),
      bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
    ) %>% 
    hc_tooltip(shared = TRUE) %>% 
    hc_yAxis(
      labels = list(
        formatter = JS("function(){ return parseFloat(100*Math.abs(this.value)).toFixed(2) + '%'; }") 
      ),
      min = -maxpop,
      max = maxpop) %>% 
    hc_xAxis(
      xaxis,
      rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
    ) %>% 
    hc_tooltip(shared = FALSE,
               formatter = JS("function () { return '<b>' + this.series.name + ', aged ' + this.point.category + '</b><br/>' + 'Share: ' + parseFloat(100 * Math.abs(this.point.y)).toFixed(2) + '%';}")
    ) %>%
    hc_colors(c("#ec7357", "#4F86C6"))
}

countries <- world %>%
  distinct(Area) %>%
  pull(Area)


ui <- shinyUI(
  fluidPage(
    titlePanel(h1("Demography Viz", align = "center"), windowTitle = "Demography Viz"),
    fluidRow(
      column(12, class = "panel",
             selectInput("country_name",
                         label = "Choose a country",
                         choices = countries, selected = "Afghanistan"), align = "center")
    ),
    fluidRow(
      column(
        8,
        offset = 2,
        align = "center",
        highchartOutput("pyramid", height = "600px")
      )
    )
  )
)

server <- shinyServer(
  function(input, output) {
    output$pyramid <- renderHighchart({
      pyramid(input$country_name)
    })
  }
)



shinyApp(ui, server)







