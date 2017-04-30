## PLotting functions

library(ggvis)
library(plotly)

plot_pulse <- function(df) {
  year_slider <- input_slider(1961, 
                              2014, 
                              value = 1961, 
                              animate = animationOptions(interval = 100, loop = TRUE),
                              ticks = FALSE, 
                              sep = ''
  )
  
  country_select <- input_select(unique(df$Country.Name), 
                                   multiple = FALSE, 
                                   selected = NA)

  
  # key is set to Country.Name simply to pass info down the pipe
  return (df %>% 
    ggvis(~variable, ~value) %>% 
    filter(Country.Name == eval(country_select)) %>%
    filter(Year == eval(year_slider)) %>%
    add_axis('y', title = 'Fertility rate') %>%
    scale_numeric('y', domain = c(-1, 1), nice = FALSE) %>%
    #layer_points(opacity := 1.0) %>%
    layer_lines()) #%>%
    #bind_shiny("p", "p_ui")
}