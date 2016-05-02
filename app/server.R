suppressMessages({library(shiny)
  library(shinythemes)
  library(shinyBS)
  library(rgdal)
  library(raster)
  library(data.table)
  library(dplyr)
  library(leaflet)
  library(ggplot2)
  library(markdown)
  library(httr)
  library(knitr)
  #library(reshape2)
  #library(sp)
  #library(ggmap)
})


#options(shiny.error=browser)
#options(warn = -1)


# ciclo de condiçoes para correr ou nao meteoForecast.R
if (file.exists("tmp/date.Rdata") == TRUE) {
  
  load("tmp/date.Rdata")
  
  if (today == Sys.Date()) {
    
    load("tmp/data.Rdata")
    
  } else if (today != Sys.Date()) {
    
    source("meteoForecast.R")
    
  }
  
} else if (file.exists("tmp/date.Rdata") != TRUE) {
  
  source("meteoForecast.R")
  
}


#load("../tmp/data.Rdata") # output/
#source("../meteoForecast.R")


# resolucao do raster
lat_res <- abs(round((lat[1] - lat[2])/2, digits = 5))
lon_res <- abs(round((lon[1] - lon[2])/2, digits = 5))


# sequencia de tempo para previsao
dates_unf <- dates
dates <- as.POSIXlt(strptime(as.character(dates), "d%Y.%m.%d.h%H"))
# select times after present hour
hour <- as.POSIXlt(round(Sys.time(), units="hours"))
dates <- as.character(dates)

wrf_cft <- subset(wrf_cft, which(dates == as.character(hour)):length(dates))
wrf_prec <- subset(wrf_prec, which(dates == as.character(hour)):length(dates))
wrf_rh <- subset(wrf_rh, which(dates == as.character(hour)):length(dates))
wrf_sst <- subset(wrf_sst, which(dates == as.character(hour)):length(dates))
wrf_swflx <- subset(wrf_swflx, which(dates == as.character(hour)):length(dates))
wrf_temp <- subset(wrf_temp, which(dates == as.character(hour)):length(dates))
wrf_wind <- subset(wrf_wind, which(dates == as.character(hour)):length(dates))
wrf_wind_gust <- subset(wrf_wind_gust, which(dates == as.character(hour)):length(dates))

dates <- as.POSIXct(dates[which(dates == as.character(hour)):length(dates)])


seq_time <- seq(01, length(dates), by =1)
lon_view <- -16.7
lat_view <- 32.7375


# raster operations
rasterOptions(timer = T, progress = "text")
#x <- disaggregate(raster_IGPH, fact=c(2, 2), method='bilinear')   # bilinear interpolation to bigger resolution


# min max values for plots colours and legends
min_max_prec <- seq(round(min(wrf_prec@data@min), digits = -1), round(max(wrf_prec@data@max), digits = -1), .1)
min_max_rh <- seq(round(min(wrf_rh@data@min), digits = 0), round(max(wrf_rh@data@max), digits = 0), .005)
min_max_sst <- seq(round(min(wrf_sst@data@min), digits = -1), round(max(wrf_sst@data@max), digits = -1), .2)-272.15
min_max_swflx <- seq(round(min(wrf_swflx@data@min), digits = -1), round(max(wrf_swflx@data@max), digits = -1), 5)
min_max_temp <- seq(round(min(wrf_temp@data@min), digits = -1), round(max(wrf_temp@data@max), digits = -1), .2)-272.15
min_max_wind <- seq(round(min(wrf_wind@data@min), digits = -1), round(max(wrf_wind@data@max), digits = -1), .1)
min_max_wind_gust <- seq(round(min(wrf_wind_gust@data@min), digits = -1), round(max(wrf_wind_gust@data@max), digits = -1), .1)
min_max_cft <- seq(round(min(wrf_cft@data@min), digits = 0), round(max(wrf_cft@data@max), digits = 0), .1)


# selecionar variaveis corretas
variavs <- variavs[-c(6:8)]
variavs <- c(variavs, "wind")


shinyServer(function(input, output, session) { # added ps for another raster, porto santo
  #acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  # latlon
  #lat_input <- reactive({ 
  #  input$userlat
  #})
  #lon_input <- reactive({ 
  #  input$userlon
  #})
  # month
  #month_input <- reactive({ 
  #  months_name[input$date+1]
  #})
  
  # select raster by variable & transform date into dates vector index
  variable <- reactive({ 
    variavs[variavs==input$wrf_var] 
  })
  
  ras_temp <- reactive({
    if (variable() == "prec") {
      ras <- wrf_prec
    } else if (variable() == "rh") {
      ras <- wrf_rh
    } else if (variable() == "sst") {
      ras <- wrf_sst-272.15
    } else if (variable() == "temp") {
      ras <- wrf_temp-272.15
    } else if (variable() == "wind") {
      ras <- wrf_wind
    } else if (variable() == "wind_gust") {
      ras <- wrf_wind_gust
    } else if (variable() == "swflx") {
      ras <- wrf_swflx
    } else if (variable() == "cft") {
      ras <- wrf_cft
    }
  })
  
  ras <- reactive({ 
    subset(ras_temp(), which(seq_time==which(dates == input$date)))
  })
  
  # color paletes
  #colorNumeric(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), na.color="transparent")
  #colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), bins = c(0, ras_vals(), Inf), na.color="transparent", alpha = F)
  #colorBin(c('#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d'), bins = c(0, 5, 8, 10, 12, 14, 18, 24, 26))
  
  ras_pal <- reactive({
    if (variable() == "prec") {
      colorBin(palette = c("lightsteelblue1", "yellowgreen", "orange", "tomato1", "violetred4"), min_max_prec, bins = c(-Inf, min_max_prec, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "rh") {
      colorBin(palette = c("burlywood4", "burlywood", "darkseagreen", "palegreen2", "steelblue1", "royalblue3"), min_max_rh, bins = c(-Inf, min_max_rh, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "sst") {
      colorBin(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), min_max_sst, bins = c(-Inf, min_max_sst, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "temp") {
      colorBin(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), min_max_temp, bins = c(-Inf, min_max_temp, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "wind") {
      colorBin(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), min_max_wind, bins = c(-Inf, min_max_wind, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "wind_gust") {
      colorBin(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), min_max_wind_gust, bins = c(-Inf, min_max_wind_gust, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "swflx") {
      colorBin(palette = c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), min_max_swflx, bins = c(-Inf, min_max_swflx, Inf), na.color="transparent", alpha = F)
    } else if (variable() == "cft") {
      colorBin(palette = c("lightskyblue1", "snow1", "snow2", "snow3", "lightsteelblue3"," snow4"), min_max_cft, bins = c(-Inf, min_max_cft, Inf), na.color="transparent", alpha = F)
    }
  })
  
  ras_vals_legend <- reactive({ 
    if (variable() == "prec") {
      seq(min(min_max_prec, na.rm = T), max(min_max_prec, na.rm = T), 2)
    } else if (variable() == "rh") {
      seq(min(min_max_rh, na.rm = T), max(min_max_rh, na.rm = T), .1)
    } else if (variable() == "sst") {
      seq(min(min_max_sst, na.rm = T), max(min_max_sst, na.rm = T), 5)
    } else if (variable() == "temp") {
      seq(min(min_max_temp, na.rm = T), max(min_max_temp, na.rm = T), 5)
    } else if (variable() == "wind") {
      seq(min(min_max_wind, na.rm = T), max(min_max_wind, na.rm = T), 2)
    } else if (variable() == "wind_gust") {
      seq(min(min_max_wind_gust, na.rm = T), max(min_max_wind_gust, na.rm = T), 2)
    } else if (variable() == "swflx") {
      seq(min(min_max_swflx, na.rm = T), max(min_max_swflx, na.rm = T), 200)
    } else if (variable() == "cft") {
      seq(min(min_max_cft, na.rm = T), max(min_max_cft, na.rm = T), .1)
    }
  })
  
  ras_pal_legend <- reactive({ 
    if (variable() == "prec") {
      colorNumeric(palette = c("lightsteelblue1", "yellowgreen", "orange", "tomato1", "violetred4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "rh") {
      colorNumeric(palette = c("burlywood4", "burlywood", "darkseagreen", "palegreen2", "steelblue1", "royalblue3"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "sst") {
      colorNumeric(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "temp") {
      colorNumeric(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "wind") {
      colorNumeric(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "wind_gust") {
      colorNumeric(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "swflx") {
      colorNumeric(palette = c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (variable() == "cft") {
      colorNumeric(palette = c("lightskyblue1", "snow1", "snow2", "snow3", "lightsteelblue3"," snow4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    }
  }) 
  
  ras_legend <- reactive({ 
    if (variable() == "prec") {
      "Precipitation [mm]"
    } else if (variable() == "rh") {
      "Relative Humidity [%]"
    } else if (variable() == "sst") {
      "Sea Surface Temp. [ºC]"
    } else if (variable() == "temp") {
      "Air Temperature at 2m [ºC]"
    } else if (variable() == "wind") {
      "Wind velocity at 10m [m/s]"
    } else if (variable() == "wind_gust") {
      "Wind gust [m/s]"
    } else if (variable() == "swflx") {
      "surface downwelling shortwave flux  [w/m^2]"
    } else if (variable() == "cft") {
      "cloud cover at low and mid levels [%]"
    }
  })
  
  
  #pal_anual <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = minmax_vals_anual(), bins = c(-Inf, minmax_vals_anual(), Inf), na.color="transparent", alpha = F) }) 
  #pal_legend_anual <- reactive({ colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = minmax_vals_anual_leg(), bins = minmax_vals_anual_leg(), na.color="transparent", alpha = F) }) 
  
  output$Map <- renderLeaflet({ 
    leaflet() %>% 
      setView(lng = lon_view, lat = lat_view, 5) %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE))  %>% 
      #addWMSTiles("http://www.lrec.pt/", attribution = "Mapa Rad. Solar © 2015 - Ricardo Faria, LREC, MJInovação") %>%
      #addPolygons(ras_ploy(), lng = long, lat = lat, opacity=0.9, popup = popup_test) %>%
      #addPolylines(hgt_polylines(), lng = long, lat = lat, color = "red") %>% 
      addProviderTiles("Esri.WorldImagery") #%>% # modify thebackground map "Esri.WorldImagery" "OpenTopoMap"
  })
  
  observe({
    proxy <- leafletProxy("Map")
    proxy %>% clearControls()
    if (input$wrf_var != "-"){
      proxy %>% addLegend(position="bottomleft", pal=ras_pal_legend(), values=ras_vals_legend(), title= ras_legend(), opacity = input$var_opac, labFormat = labelFormat(big.mark = "")) #%>%   # values= seq(50, 220, 5)
      #removeTiles(layerId="raster") %>% addRasterImage(ras(), opacity=input$prec_opac, project = F, layerId="raster")
    } else if (input$wrf_var == "-"){
      proxy %>% clearControls()
    }      
    #removeTiles(layerId="raster") %>% addRasterImage(ras(), opacity=input$prec_opac, project = F, layerId="raster")
  })
  
  observe({ # raster layers
    proxy <- leafletProxy("Map")
    proxy %>% removeTiles(layerId="rasimg")
    if (input$wrf_var != "-"){
      proxy %>% removeTiles(layerId="rasimg") %>% 
        addRasterImage(ras(), opacity=input$var_opac, colors = ras_pal(), project = T, layerId="rasimg")
    } else if (input$wrf_var == "-"){
      proxy %>% removeImage(layerId="rasimg")
    }
  })
  
  
})
