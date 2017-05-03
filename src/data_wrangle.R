library(dplyr)
library(reshape2)
library(tidyr)

if (!require(rgdal)) install.packages("rgdal")
library(rgdal)

if (!require(rgeos)) install.packages("rgeos")
library(rgeos)

if (!require(maptools)) install.packages("maptools")
library(maptools)

clean_year_string <- function(df) {
  df$Year <- vapply(df$Year, function(s) substr(s, 2, 5), 'a')
  return(df)
}


load_map <- function(name) {
  world <- readOGR(name, "OGRGeoJSON")
  map <- ggplot2::fortify(world, region = "name")
  return(map)
}


format_for_heat <- function(df, map_file) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  map_d <- left_join(map_file, df, by=c("id"="Country.Name"))
  map_d[is.na(map_d)] <- 0
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
  df[is.na(df$value), 'value'] <- 0.0
  df <- spread(df, Indicator.Code, value)
  df <- df[as.numeric(df$Year) > 1900, ]
  df <- group_by(df, Country.Code) %>% mutate_if(funs(normalize_list), .predicate=is.numeric)
  df$above <- df$EG.USE.ELEC.KH.PC + df$NV.AGR.TOTL.ZS / 0.5
  df$below <- df$EG.USE.ELEC.KH.PC - df$NV.AGR.TOTL.ZS / 0.5
  df <- na.omit(df)
  df$Year <- as.numeric(df$Year)
  return(df)
}

load_data <- function(file_name) {
  df <- read_data(file_name)
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
