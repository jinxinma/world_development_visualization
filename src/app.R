rm(list=ls())
cat('\014')

library(dplyr)
library(ggvis)
library(reshape2)
library(shiny)

setwd('/Users/Will/msan/vis/project/src/')

format_for_parallel <- function(df) {
  df$Fertility.Rate <- normalize_list(df$Fertility.Rate)
  df$Life.Expectancy <- - normalize_list(df$Life.Expectancy)
  df$Population <- normalize_list(df$Population)
  df$Fertility.Rate2 <- - normalize_list(df$Fertility.Rate)
  df$Life.Expectancy2 <- normalize_list(df$Life.Expectancy)
  df$Population2 <- - normalize_list(df$Population)
  return(melt(df, c('Country.Name', 'Year')))
}


load_data <- function() {
  fert_df <- read_data("../API_SP/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", 4)
  le_df <- read_data("../API_SP-2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", 4)
  pop_df <- read_data("../API_SP.POP.TOTL_DS2_en_csv_v2.csv")
  cont_df <- read.csv("../continents.csv", stringsAsFactors = FALSE)
  
  fert_df <- reshape_df(fert_df, "Fertility.Rate")
  le_df <- reshape_df(le_df, "Life.Expectancy")
  pop_df <- reshape_df(pop_df, "Population")
  
  df_temp <- merge(cont_df, fert_df, by = c("Country.Name"), all.x = TRUE, all.y = FALSE)
  df_temp <- merge(df_temp, le_df, by = c("Country.Name", "Year"))
  df <- merge(df_temp, pop_df, by = c("Country.Name", "Year"))
  return (merge(df_temp, pop_df, by = c("Country.Name", "Year")))
}


normalize <- function(x, minimum, maximum) {
  return ((x - minimum) / (maximum - minimum))
}


normalize_list <- function(l) {
  return(vapply(l, FUN = normalize, FUN.VALUE = 1, min(l), max(l)))
}


read_data <- function(file_name, skip=0) {
  df <- read.csv(file_name, skip=skip, stringsAsFactors = FALSE)
  df <- df[, -c(2,3,4,60, 61, 62)]
  return (df)
}


reshape_df <- function(df, value) {
  df <- melt(df, measure.vars = names(df)[2:length(names(df))])
  df <- plyr::rename(df, c("variable" = "Year", "value" = value))
  df$Year <- lapply(df$Year, function(s) substr(s, 2, 5))
  return (df)
}





