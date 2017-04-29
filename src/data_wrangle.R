library(dplyr)
library(reshape2)
library(tidyr)


clean_year_string <- function(df) {
  df$Year <- lapply(df$Year, function(s) substr(s, 2, 5))
  return(df)
}


format_for_heat <- function(df) {
  df <- melt_years(df)
  return(df)
}


format_for_parallel <- function(df) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  df <- group_by(df, Country.Code) %>% 
    mutate_each(funs(normalize_list), c(-Year, -Country.Name))
  df <- melt(df, c('Country.Name', 'Country.Code', 'Year'))
  return(df)
}


format_for_time_series <- function(df) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  return(df)
}


load_data <- function() {
  df <- read_data("../WDI_csv/WDIData.csv")
  df <- df[df$Indicator.Code %in% c('PA.NUS.PPP.05', 'PA.NUS.PRVT.PP.05'), ]
  return(df)
}


melt_years <- function(df) {
  df <- melt(df, c('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code'))
  df <- plyr::rename(df, c("variable" = "Year"))
  df <- clean_year_string(df)
  return (df)
}


normalize_list <- function(l) {
  (l - min(l)) / (max(l) - min(l))
}


read_data <- function(file_name, skip=0) {
  df <- read.csv(file_name, skip=skip, stringsAsFactors = FALSE)
  return (df)
}
