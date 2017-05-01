rm(list=ls())
cat('\014')

library(shiny)

source('data_wrangle.R')
source('plotting.R')

world_df <- load_data()
world_df <- world_df[world_df$Indicator.Code %in% c("SP.DYN.TFRT.IN",
                                                    "NV.AGR.TOTL.ZS",
                                                    "EG.USE.ELEC.KH.PC"), ]

bind_heat <- function(df, year) {
  df %>%
     filter(Year == year) %>%
     plot_heat()
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
               sliderInput("year",
                           label = "Year: ",
                           min = 1961, 
                           max = 2014, 
                           value = 1961, 
                           animate = animationOptions(interval = 1000, loop = TRUE)
               ),
               ggvisOutput('heat'),
               uiOutput("h_ui")),
      
      #tabPanel("River Time Series", ),
      tabPanel("World Pulse", 
               sliderInput("year",
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
  heat_df <- format_for_heat(world_df)
  
  vis_pulse <- reactive({parallel_df %>% bind_pulse(input$country, input$year)})
  vis_pulse %>% bind_shiny("pulse", "p_ui")

  vis_heat <- reactive({heat_df %>% bind_heat(input$year)})
  vis_heat %>% bind_shiny("heat", "h_ui") 
}


shinyApp(ui = ui, server = server)


