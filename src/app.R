rm(list=ls())
cat('\014')

if (!require(plotly)) install.packages("plotly")
library(ggplot2)
library(shiny)
library(plotly)

source('data_wrangle.R')
source('plotting.R')

map <- load_map("../world.geo.json")
world_df <- load_data("../wdi_tiny.csv")
parallel_df <- format_for_parallel(world_df)
heat_df <- format_for_heat(world_df, map)
time_df <- format_for_time_series(world_df)
bubble_df <- format_for_bubble(world_df)
df_2 <- bubble_df[, 2:ncol(bubble_df)]

bind_bubble <- function(df, x_col, y_col, year) {
  min_x <- min(df_2[x_col], na.rm = TRUE)
  max_x <- max(df_2[x_col], na.rm = TRUE)
  min_y <- min(df_2[y_col], na.rm = TRUE)
  max_y <- max(df_2[y_col], na.rm = TRUE)
  plot_bubble(df, x_col, y_col, year, min_x, max_x, min_y, max_y)
}


bind_heat <- function(df, year, indicator) {
  df %>%
    filter(Year == year) %>%
    plot_heat(indicator)
}


bind_pulse <- function(df, country_select, year) {
  df %>%
    filter(Country.Name == country_select) %>%
    filter(Year == year) %>%
    plot_pulse() 
}


ui <- fluidPage(
  titlePanel("Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("World Heat Map",
               selectInput(inputId = "indicator",
                           label = "Indicator Name: ",
                           selected = "NV.AGR.TOTL.ZS",
                           choices =  unique(world_df$Indicator.Code)),
               sliderInput("heat_year",
                           label = "Year: ",
                           min = 1970, 
                           max = 2014, 
                           value = 1970,
                           sep = '',
                           animate = animationOptions(interval = 1000, loop = TRUE)
               ),
               ggvisOutput('heat'),
               uiOutput("h_ui")),
      tabPanel("River Time Series", 
               selectInput(inputId = "indicator_y",
                           label = "Indicator Name (y-axis): ",
                           choices = world_df$Indicator.Code),
               selectInput(inputId = "indicator_w",
                           label = "Indicator Name (width): ",
                           choices = world_df$Indicator.Code),
               selectInput(inputId = "time_countries",
                           label = "Country: ",
                           choices = world_df$Country.Name,
                           multiple = TRUE,
                           selected = "United States"),
               plotlyOutput("time")),
      tabPanel("World Pulse", 
               sliderInput("pulse_year",
                           label = "Year: ",
                           min = 1970, 
                           max = 2014, 
                           value = 1970, 
                           sep = '',
                           animate = animationOptions(interval = 150, loop = TRUE)
               ),
               selectInput("country",
                           label = "Country: ",
                           choices = world_df$Country.Name, 
                           multiple = FALSE, 
                           selected = NA),
               ggvisOutput('pulse'),
               uiOutput("p_ui")),
      tabPanel("Bubble Plot",
               sliderInput("bubble_year",
                           label = "Year: ",
                           min = 1970, 
                           max = 2014, 
                           value = 1970, 
                           sep = '',
                           animate = animationOptions(interval = 100, loop = TRUE)
                           ),
               selectInput(inputId = "bubble_y",
                           label = "Indicator Name (y-axis): ",
                           choices = unique(world_df$Indicator.Code),
                           selected = "Fertility.Rate"),
               selectInput(inputId = "bubble_x",
                           label = "Indicator Name (x-axis): ",
                           choices = unique(world_df$Indicator.Code),
                           selected = "Life.Expectancy"),
               ggvisOutput('bubble'),
               uiOutput("b_ui"))
    )
  )
)


server <- function(input, output) {
  # Reacts to changing river time y axis variable
  observeEvent(input$indicator_w, {
    time_observer(input, output, time_df)
  }) 
  # Reacts to changing river time width variable
  observeEvent(input$indicator_y, {
    time_observer(input, output, time_df)
  }) 
  # Reacts to selecting river time countries
  observeEvent(input$time_countries, {
    time_observer(input, output, time_df)
  }) 
  vis_pulse <- reactive({parallel_df %>% bind_pulse(input$country, input$pulse_year)})
  vis_pulse %>% bind_shiny("pulse", "p_ui")
  vis_heat <- reactive({heat_df %>% bind_heat(input$heat_year, input$indicator)})
  vis_heat %>% bind_shiny("heat", "h_ui") 
  vis_bubble <- reactive({bind_bubble(bubble_df, input$bubble_x, input$bubble_y, input$bubble_year)})
  vis_bubble %>% bind_shiny("bubble", "b_ui")
}


shinyApp(ui = ui, server = server)
