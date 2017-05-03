rm(list=ls())
cat('\014')

library(ggplot2)
library(shiny)
library(plotly)

source('data_wrangle.R')
source('plotting.R')

map <- load_map("../world.geo.json")
world_df <- load_data("../wdi_tiny.csv")
world_df <- world_df[world_df$Indicator.Code %in% c("SP.DYN.TFRT.IN",  # fertility rate total
                                                    "NV.AGR.TOTL.ZS", # percent GDP is Ag
                                                    "AG.LND.CROP.ZS", # permenant cropland percent of land area
                                                    "EG.USE.ELEC.KH.PC",  # kWh per capita
                                                    "SP.DYN.CBRT.IN", # birth rate
                                                    "EN.ATM.CO2E.KD.GD",  # CO2 emissions per 2010 us dollars
                                                    "NE.EXP.GNFS.ZS",  # Exports % GDP
                                                    "NY.GDP.MKTP.CD",  # GDP in current USD
                                                    "NY.GDP.PCAP.CD",  # GDP per capita in current USD
                                                    "NV.IND.TOTL.ZS",  # Industry percent GDP
                                                    "SP.DYN.LE00.IN",  # Life expectancy at birth
                                                    "EN.POP.DNST",  # population density 
                                                    "SP.POP.TOTL", # population
                                                    "SP.RUR.TOTL.ZS", # percent rural population
                                                    "SP.URB.TOTL.IN.ZS"  # urban population percent
                                                    ), ]

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

bind_time <- function(df) {
  plot_time(df) %>% bind_shiny("time", "p_ui")
}


time_observer <- function(input, output, df) {
  col_y <- input$indicator_y
  col_w <- input$indicator_w

  df = df[df$Country.Name %in% input$time_countries, ]
  
  if (col_w %in% names(df) && col_y %in% names(df)) {
    y_vals <- df[, names(df) == col_y]
    w_vals <- df[, names(df) == col_w] / 10.0
    df$above <- y_vals + w_vals 
    df$below <- y_vals - w_vals 
    output$time <- renderPlotly(
       plot <- ggplot(df, aes(x = Year))  + 
               geom_ribbon(aes(ymin = below, 
                          ymax = above,
                          fill = Country.Name), 
                          alpha = 0.5) + 
               theme_bw() + theme(axis.title = element_text(size = 12),
                           axis.text = element_text(size = 12),
                           legend.text = element_text(size = 15),
                           legend.title = element_blank())
    )
  }
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
                           min = 1961, 
                           max = 2014, 
                           value = 1961, 
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
                           min = 1961, 
                           max = 2014, 
                           value = 1961, 
                           animate = animationOptions(interval = 100, loop = TRUE)
               ),
               selectInput("country",
                           label = "Country: ",
                           choices = world_df$Country.Name, 
                           multiple = FALSE, 
                           selected = NA),
               ggvisOutput('pulse'),
               uiOutput("p_ui"))
    )
  )
)

server <- function(input, output) {
  parallel_df <- format_for_parallel(world_df)
  heat_df <- format_for_heat(world_df, map)
  time_df <- format_for_time_series(world_df)
  
  
  ## Reacts to changing river time y axis variable
  observeEvent(input$indicator_w, {
    time_observer(input, output, time_df)
  }) 

  ## Reacts to changing river time width variable
  observeEvent(input$indicator_y, {
    time_observer(input, output, time_df)
  }) 
  
  ## Reacts to selecting river time countries
  observeEvent(input$time_countries, {
    time_observer(input, output, time_df)
  }) 
  
   
  vis_pulse <- reactive({parallel_df %>% bind_pulse(input$country, input$pulse_year)})
  vis_pulse %>% bind_shiny("pulse", "p_ui")

  vis_heat <- reactive({heat_df %>% 
                        bind_heat(input$heat_year, input$indicator)})
  vis_heat %>% bind_shiny("heat", "h_ui") 
}


shinyApp(ui = ui, server = server)


