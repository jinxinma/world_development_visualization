rm(list=ls())
cat('\014')

library(shiny)

source('data_wrangle.R')
source('plotting.R')

bind_heat <- function(df) {
  plot_heat(df) %>% bind_shiny("heat", "p_ui")
}

bind_pulse <- function(df) {
  plot_pulse(df) %>% bind_shiny("pulse", "p_ui")
}


ui <- fluidPage(
  titlePanel("Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("World Heat Map", ggvisOutput('heat')),
      #tabPanel("River Time Series", ),
      tabPanel("World Pulse", ggvisOutput('pulse'))
    )
  ),
  uiOutput("p_ui")
)

server <- function(input, output) {
  world_df <- load_data()
  world_df <- world_df[world_df$Indicator.Code %in% c("SP.DYN.TFRT.IN",
                                                      "NV.AGR.TOTL.ZS",
                                                      "EG.USE.ELEC.KH.PC"), ]
  parallel_df <- format_for_parallel(world_df)
  heat_df <- format_for_heat(world_df)
  
  bind_pulse(parallel_df)
  bind_heat(heat_df)
}


shinyApp(ui = ui, server = server)


