## PLotting functions

library(ggvis)
library(plotly)

plot_pulse <- function(df) {
  # key is set to Country.Name simply to pass info down the pipe
  return (df %>% 
    ggvis(~variable, ~value) %>% 
    add_axis('y', title = 'Fertility rate') %>%
    scale_numeric('y', domain = c(-1, 1), nice = FALSE) %>%
    #layer_points(opacity := 1.0) %>%
    layer_lines()) #%>%
    #bind_shiny("p", "p_ui")
}

plot_heat <- function(df) {
  return (df %>%
          ggvis(~long, ~lat) %>%
          group_by(group, id) %>%
          layer_paths(strokeOpacity = 0.5,
                      fill = ~ NV.AGR.TOTL.ZS) %>%
          scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
          hide_axis("x") %>% hide_axis("y") %>%
          set_options(width=800, height=300, keep_aspect=TRUE))
}
