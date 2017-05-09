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


plot_bubble <- function(df, x_col, y_col, year, min_x, max_x, min_y, max_y) {
  df$x <- df[, x_col]
  df$y <- df[, y_col]
  # key is set to Country.Name simply to pass info down the pipe
  return(df %>%
    ggvis(~x, ~y, size = ~Population, key := ~Country.Name, fill = 'blue') %>%
    hide_legend('fill') %>%
    set_options(height = 650, width = 1000) %>%
    add_tooltip(point_hover, "hover") %>%
    add_axis('x', title = x_col) %>%
    add_axis('y', title = y_col) %>%
    filter(Year == year) %>%
    layer_points(opacity := 0.5) %>%
    scale_numeric('x', domain = c(min_x, max_x), nice = FALSE) %>%
    scale_numeric('y', domain = c(min_y, max_y), nice = FALSE)
  ) 
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
    layer_lines(stroke := 'green', opacity := 0.5, strokeWidth := 5))
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


point_hover <- function(x) {
  paste(x$Country.Name)
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
      ggplot(df, aes(x = Year))  + 
        geom_ribbon(aes(ymin = below, 
                        ymax = above,
                        fill = Country.Name), 
                    alpha = 0.5) + 
        theme_bw() + 
        theme(axis.title = element_text(size = 12),
              axis.text = element_text(size = 12),
              legend.text = element_text(size = 15),
              legend.title = element_blank())
    )
  }
}
