library(dplyr)
library(reshape2)
library(tidyr)

if (!require(rgdal)) install.packages("rgdal")
library(rgdal)

if (!require(rgeos)) install.packages("rgeos")
library(rgeos)


clean_year_string <- function(df) {
  df$Year <- vapply(df$Year, function(s) substr(s, 2, 5), 'a')
  return(df)
}


load_map <- function(name) {
  world <- readOGR(name, "OGRGeoJSON")
  map <- ggplot2::fortify(world, region = "name")
  return(map)
}


format_for_heat <- function(df) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  map <- load_map("../WDI_csv/world.geo.json")
  map_d <- left_join(map, df, by=c("id"="Country.Name"))
  return(map_d)
}


format_for_parallel <- function(df) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  df <- group_by(df, Country.Code) %>% mutate_if(funs(normalize_list), .predicate=is.numeric)
  df_odd <- df[, which(col(df[0:1,]) %% 2 == 1)] %>% mutate_if(funs(negate), .predicate=is.numeric)
  df_even <- df[, which(col(df[0:1,]) %% 2 == 0)]
  df <- cbind(data.frame(df_odd), data.frame(df_even))
  df <- melt(df, c('Country.Name', 'Country.Code', 'Year'))
  df[is.na(df$value), 'value'] <- 0.0
  return(df)
}

negate <- function(x) return(- x)


format_for_time_series <- function(df) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  return(df)
}

load_data <- function() {
  df <- read_data("../WDI_csv/wdi_tiny.csv")
  return(df)
}


melt_years <- function(df) {
  df <- melt(df, c('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code'))
  df <- plyr::rename(df, c("variable" = "Year"))
  df <- clean_year_string(df)
  return (df)
}


normalize_list <- function(l) {
  l_filtered <- l[!is.na(l)] 
  if (length(l_filtered)) {
    max_l <- max(l_filtered)
    min_l <- min(l_filtered)    
    return((l - min_l) / (max_l - min_l))
  } else {
    return (l - l)
  }
}


read_data <- function(file_name, skip=0) {
  df <- read.csv(file_name, skip=skip, stringsAsFactors = FALSE)
  return (df)
}
