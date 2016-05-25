library(shiny)
#library(shinythemes)
library(shinyBS)
library(rgdal)
library(raster)
#library(data.table)
#library(dplyr)
library(leaflet)
#library(ggplot2)
#library(markdown)
#library(httr)
#library(knitr)
#library(reshape2)
library(highcharter)
library(zoo)

library(meteoForecast)
library(raster)
library(rgdal)
library(ncdf4)



#options(shiny.error=browser)
#options(warn = -1)



#load("tmp/date.RData")
load("tmp/data.RData")

# ciclo de condiçoes para correr ou nao meteoForecast.R em background adicionei $value para testar scooping
if (today != Sys.Date()) {
  #source("meteoForecast.R")
  system("Rscript -e 'source(\"meteoForecast.R\")$value'", wait=FALSE)
}



# resolucao do raster
#lat_res <- abs(round((lat[1] - lat[2])/2, digits = 5))
#lon_res <- abs(round((lon[1] - lon[2])/2, digits = 5))


# sequencia de tempo para previsao
#dates_unf <- dates
dates <- as.POSIXlt(strptime(as.character(dates), "d%Y.%m.%d.h%H"))
# select times after present hour
hour <- as.POSIXlt(round(Sys.time(), units="hours"))
dates <- as.character(dates)

# change to a faster method: [[]]
wrf_cft <- wrf_cft[[which(dates == as.character(hour)):length(dates)]]
wrf_prec <- wrf_prec[[which(dates == as.character(hour)):length(dates)]]
wrf_rh <- wrf_rh[[which(dates == as.character(hour)):length(dates)]]
wrf_sst <- wrf_sst[[which(dates == as.character(hour)):length(dates)]]
wrf_swflx <- wrf_swflx[[which(dates == as.character(hour)):length(dates)]]
wrf_temp <- wrf_temp[[which(dates == as.character(hour)):length(dates)]]
wrf_wind <- wrf_wind[[which(dates == as.character(hour)):length(dates)]]
wrf_wind_gust <- wrf_wind_gust[[which(dates == as.character(hour)):length(dates)]]

dates <- as.POSIXct(dates[which(dates == as.character(hour)):length(dates)])


#seq_time <- seq(01, length(dates), by =1)
lon_view <- -16.7
lat_view <- 32.7375


# raster operations
#rasterOptions(timer = T, progress = "text")
#x <- disaggregate(raster_IGPH, fact=c(2, 2), method='bilinear')   # bilinear interpolation to bigger resolution


# extract valuyes das coords:
st <- data.frame(name=c('Funchal','Porto', "Lisboa", "Faro", "Açores"))

coordinates(st) <- cbind(c(-16.918932, -8.639011, -9.209984, -7.992510, -28.118584),  #lon
                         c(32.652811, 41.150476, 38.726852, 37.057566, 38.583487))   #lat
proj4string(st) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
#temperatura
vals_temp <- round(extract(wrf_temp, st), 2)
vals_temp <- zoo(t(vals_temp), format(dates, "%Y-%m-%d %Hh"))
names(vals_temp) <- st$name
vals_temp <- data.frame(vals_temp)
#precipitacao
vals_prec <- round(extract(wrf_prec, st), 4)
vals_prec <- zoo(t(vals_prec), format(dates, "%Y-%m-%d %Hh"))
names(vals_prec) <- st$name
vals_prec <- data.frame(vals_prec)
#vento
vals_wind <- round(extract(wrf_wind, st), 2)
vals_wind <- zoo(t(vals_wind), format(dates, "%Y-%m-%d %Hh"))
names(vals_wind) <- st$name
vals_wind <- data.frame(vals_wind)
#rajadas de vento
vals_wind_gust <- round(extract(wrf_wind_gust, st), 2)
vals_wind_gust <- zoo(t(vals_wind_gust), format(dates, "%Y-%m-%d %Hh"))
names(vals_wind_gust) <- st$name
vals_wind_gust <- data.frame(vals_wind_gust)
#radiacao
vals_swflx <- round(extract(wrf_swflx, st), 2)
vals_swflx <- zoo(t(vals_swflx), format(dates, "%Y-%m-%d %Hh"))
names(vals_swflx) <- st$name
vals_swflx <- data.frame(vals_swflx)


# min max values for plots colours and legends
min_max_prec <- seq(round(min(wrf_prec@data@min), digits = -1), round(max(wrf_prec@data@max), digits = -1), .1)
min_max_rh <- seq(round(min(wrf_rh@data@min), digits = 0), round(max(wrf_rh@data@max), digits = 0), .005)
min_max_sst <- seq(round(min(wrf_sst@data@min), digits = -1), round(max(wrf_sst@data@max), digits = -1), .2)
min_max_swflx <- seq(round(min(wrf_swflx@data@min), digits = -1), round(max(wrf_swflx@data@max), digits = -1), 5)
min_max_temp <- seq(round(min(wrf_temp@data@min), digits = -1), round(max(wrf_temp@data@max), digits = -1), .2)
min_max_wind <- seq(round(min(wrf_wind@data@min), digits = -1), round(max(wrf_wind@data@max), digits = -1), .1)
min_max_wind_gust <- seq(round(min(wrf_wind_gust@data@min), digits = -1), round(max(wrf_wind_gust@data@max), digits = -1), .1)
min_max_cft <- seq(round(min(wrf_cft@data@min), digits = 0), round(max(wrf_cft@data@max), digits = 0), .005)


# selecionar variaveis corretas
variavs <- variavs[-c(7:8)]
variavs <- c(variavs, "wind")
variavs <- sort(variavs)