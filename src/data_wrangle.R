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


format_for_bubble <- function(df) {
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  df <- df[df$Year != "",]
  df <- na.omit(df)
  return(df)
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
  # df <- df[!(df$Indicator.Code %in% c("Percent.Ag.GDP",
  #                                     "Birth.Rate",
  #                                     "Fertility.Rate",
  #                                     "Percent.Ag.Land",
  #                                     "Percent.Ag.GDP",
  #                                     "Percent.Rural.Population"
  # )), ]
  df <- melt_years(df)
  df <- df[, names(df) != 'Indicator.Name']
  df <- spread(df, Indicator.Code, value)
  df$Percent.Ag.GDP = 1 - df$Percent.Ag.GDP
  df$Birth.Rate = 1 - df$Birth.Rate
  df$Fertility.Rate = 1 - df$Fertility.Rate
  df$Percent.Ag.Land = 1 - df$Percent.Ag.Land
  df$Percent.Ag.GDP = 1 - df$Percent.Ag.GDP
  df$Percent.Rural.Population = 1 - df$Percent.Rural.Population
  df <- df[, c("Country.Code",
               "Country.Name",
               "Year",
               "Fertility.Rate",
               "Birth.Rate",
               "Life.Expectancy",
               "Population.Density",
               "Population",
               "Energy.Use.Per.Capita",
               "CO2.Emissions",
               "Exports",
               "GDP",
               "GDP.Per.Capita",
               "Percent.Ag.GDP",
               "Industry.Percent.GDP",
               "Percent.Rural.Population",
               "Percent.Ag.Land",
               "Urban.Population")
           ]
  df <- group_by(df, Country.Code) %>% mutate_if(funs(normalize_list), .predicate=is.numeric)
  #df_odd <- df[, which(col(df[0:1,]) %% 2 == 1)] %>% mutate_if(funs(negate), .predicate=is.numeric)
  #df_even <- df[, which(col(df[0:1,]) %% 2 == 0)]
  #df <- cbind(data.frame(df_odd), data.frame(df_even))
  df <- melt(df, c('Country.Name', 'Country.Code', 'Year'))
  df[is.na(df$value), 'value'] <- 0.0
  for (col_name in c("Birth.Rate",
                    "Population.Density",
                    "Energy.Use.Per.Capita",
                    "Exports",
                    "GDP.Per.Capita",
                    "Industry.Percent.GDP",
                    "Percent.Ag.Land")) {
    df[df$variable == col_name, 'value'] <- -df[df$variable == col_name, 'value']
  }
  
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
  df <- na.omit(df)
  df$Year <- as.numeric(df$Year)
  return(df)
}


load_data <- function(file_name) {
  df <- read_data(file_name)
  df <- df[df$Indicator.Code %in% c("SP.DYN.TFRT.IN",  # fertility rate total
                                    "SP.DYN.CBRT.IN",  # birth rate
                                    "EN.POP.DNST",  # population density 
                                    "SP.POP.TOTL",  # population
                                    "SP.DYN.LE00.IN",  # Life expectancy at birth
                                    "NV.AGR.TOTL.ZS",  # percent GDP is Ag
                                    "NE.EXP.GNFS.ZS",  # Exports % GDP
                                    "NY.GDP.MKTP.CD",  # GDP in current USD
                                    "NY.GDP.PCAP.CD",  # GDP per capita in current USD
                                    "NV.IND.TOTL.ZS",  # Industry percent GDP
                                    "AG.LND.CROP.ZS",  # permenant cropland percent of land area
                                    "EG.USE.ELEC.KH.PC",  # kWh per capita
                                    "EN.ATM.CO2E.KD.GD",  # CO2 emissions per 2010 us dollars
                                    "SP.RUR.TOTL.ZS",  # percent rural population
                                    "SP.URB.TOTL.IN.ZS"  # urban population percent
  ), ]
  
  df$Indicator.Code <-lapply(df$Indicator.Code, FUN = map_code)
  return(df)
}


load_map <- function(name) {
  world <- readOGR(name, "OGRGeoJSON")
  map <- ggplot2::fortify(world, region = "name")
  return(map)
}


map_code <- function(code) {
  switch (code,
          "SP.DYN.TFRT.IN" = "Fertility.Rate",  # fertility rate total
          "NV.AGR.TOTL.ZS" = "Percent.Ag.GDP",  # percent GDP is Ag
          "AG.LND.CROP.ZS" = "Percent.Ag.Land",  # permenant cropland percent of land area
          "EG.USE.ELEC.KH.PC" = "Energy.Use.Per.Capita",  # kWh per capita
          "SP.DYN.CBRT.IN" = "Birth.Rate",  # birth rate
          "EN.ATM.CO2E.KD.GD" = "CO2.Emissions",  # CO2 emissions per 2010 us dollars
          "NE.EXP.GNFS.ZS" = "Exports",  # Exports % GDP
          "NY.GDP.MKTP.CD" = "GDP",  # GDP in current USD
          "NY.GDP.PCAP.CD" = "GDP.Per.Capita",  # GDP per capita in current USD
          "NV.IND.TOTL.ZS" = "Industry.Percent.GDP",  # Industry percent GDP
          "SP.DYN.LE00.IN" = "Life.Expectancy",  # Life expectancy at birth
          "EN.POP.DNST" = "Population.Density",  # population density 
          "SP.POP.TOTL" = "Population",  # population
          "SP.RUR.TOTL.ZS" = "Percent.Rural.Population",  # percent rural population
          "SP.URB.TOTL.IN.ZS" = "Urban.Population"  # urban population percent
  )
}


melt_years <- function(df) {
  df <- melt(df, c('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code'))
  df <- plyr::rename(df, c("variable" = "Year"))
  df <- clean_year_string(df)
  df <- df[df$Year != '',]
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
