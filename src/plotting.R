## PLotting functions

library(ggvis)
library(plotly)


normalize <- function(l) {
  l_filtered <- l[!is.na(l)] 
  if (length(l_filtered)) {
    max_l <- max(l_filtered)
    min_l <- min(l_filtered)    
    return((l - min_l) / (max_l - min_l))
  } else {
    return (l - l)
  }
}


plot_pulse <- function(df) {
  # key is set to Country.Name simply to pass info down the pipe
  return (df %>% 
    ggvis(~variable, ~value) %>% 
    add_axis('y', title = "Normalized Value") %>%
    add_axis('x', title = "", properties = axis_props(
      labels = list(angle = 45, align = "left", fontSize = 12)
    )) %>%
    scale_numeric('y', domain = c(-1, 1), nice = FALSE) %>%
    #layer_points(opacity := 1.0) %>%
    layer_lines()) #%>%
    #bind_shiny("p", "p_ui")
}


plot_heat <- function(df, indicator) {
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    paste0("Country Name: ", format(x)$id, "<br />",
           names(x)[5], ": ", format(x)[5])
  }
  
  return (df %>%
          ggvis(~long, ~lat) %>%
          group_by(group, id) %>%
          layer_paths(strokeOpacity = 0.5,
                      fill = as.name(indicator)) %>%
          scale_numeric("fill", range=c("#bfd3e6", "#e71818")) %>%
          hide_axis("x") %>% hide_axis("y") %>%
          add_tooltip(all_values, "hover") %>%
          set_options(width=800, height=300, keep_aspect=TRUE))
}
