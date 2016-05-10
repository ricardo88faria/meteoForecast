
shinyServer(function(input, output, session) { # added ps for another raster, porto santo
  #acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  
  # if needed download data and raster auto downloader every 12h 
  #downloader <- reactive({                   # Responds to changes. But in here, only the invalidation triggers change.
  #  #invalidateLater(86400000/4, session)     # 4 x dia, 86400000 millisecs = 24 hours.
  #  if (today != Sys.Date()) {
  #    source("meteoForecast.R")
  #  } 
  #})
  
  # start downloader function if condition
  #observe({
  #  if (input$date != min(dates) & input$wrf_var != "-"){
  #      downloader()
  #  }
  #})
  
  
  ras <- reactive({
    if (input$wrf_var == "prec") {
      wrf_prec[[which(dates == input$date)]]
    } else if (input$wrf_var == "rh") {
      wrf_rh[[which(dates == input$date)]]
    } else if (input$wrf_var== "sst") {
      wrf_sst[[which(dates == input$date)]]
    } else if (input$wrf_var == "temp") {
      wrf_temp[[which(dates == input$date)]]
    } else if (input$wrf_var == "wind") {
      wrf_wind[[which(dates == input$date)]]
    } else if (input$wrf_var == "wind_gust") {
      wrf_wind_gust[[which(dates == input$date)]]
    } else if (input$wrf_var == "swflx") {
      wrf_swflx[[which(dates == input$date)]]
    } else if (input$wrf_var == "cft") {
      wrf_cft[[which(dates == input$date)]]
    }
  })
  
  # color paletes
  #colorNumeric(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), na.color="transparent")
  #colorBin(palette = c("dodgerblue", "springgreen2", "yellow2", "orange", "tomato1", "violetred4", "purple"), domain = ras_vals(), bins = c(0, ras_vals(), Inf), na.color="transparent", alpha = F)
  #colorBin(c('#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d'), bins = c(0, 5, 8, 10, 12, 14, 18, 24, 26))
  
  ras_pal <- reactive({
    if (input$wrf_var == "prec") {
      colorBin(palette = c("transparent", "springgreen2", "yellowgreen", "yellow1", "orange", "tomato1", "violetred4"), min_max_prec, bins = c(-Inf, min_max_prec, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "rh") {
      colorBin(palette = c("burlywood4", "burlywood", "darkseagreen", "palegreen2", "steelblue1", "royalblue3"), min_max_rh, bins = c(-Inf, min_max_rh, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "sst") {
      colorBin(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), min_max_sst, bins = c(-Inf, min_max_sst, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "temp") {
      colorBin(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), min_max_temp, bins = c(-Inf, min_max_temp, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "wind") {
      colorBin(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), min_max_wind, bins = c(-Inf, min_max_wind, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "wind_gust") {
      colorBin(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), min_max_wind_gust, bins = c(-Inf, min_max_wind_gust, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "swflx") {
      colorBin(palette = c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), min_max_swflx, bins = c(-Inf, min_max_swflx, Inf), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "cft") {
      colorBin(palette = c("lightskyblue1", "snow1", "snow2", "snow3", "lightsteelblue3"," snow4"), min_max_cft, bins = c(-Inf, min_max_cft, Inf), na.color="transparent", alpha = F)
    }
  })
  
  ras_vals_legend <- reactive({ 
    if (input$wrf_var == "prec") {
      seq(min(min_max_prec, na.rm = T), max(min_max_prec, na.rm = T), 2)
    } else if (input$wrf_var == "rh") {
      seq(min(min_max_rh, na.rm = T), max(min_max_rh, na.rm = T), .1)
    } else if (input$wrf_var == "sst") {
      seq(min(min_max_sst, na.rm = T), max(min_max_sst, na.rm = T), 5)
    } else if (input$wrf_var == "temp") {
      seq(min(min_max_temp, na.rm = T), max(min_max_temp, na.rm = T), 5)
    } else if (input$wrf_var == "wind") {
      seq(min(min_max_wind, na.rm = T), max(min_max_wind, na.rm = T), 2)
    } else if (input$wrf_var == "wind_gust") {
      seq(min(min_max_wind_gust, na.rm = T), max(min_max_wind_gust, na.rm = T), 2)
    } else if (input$wrf_var == "swflx") {
      seq(min(min_max_swflx, na.rm = T), max(min_max_swflx, na.rm = T), 200)
    } else if (input$wrf_var == "cft") {
      seq(min(min_max_cft, na.rm = T), max(min_max_cft, na.rm = T), .1)
    }
  })
  
  ras_pal_legend <- reactive({ 
    if (input$wrf_var == "prec") {
      colorNumeric(palette = c("transparent", "springgreen2", "yellowgreen", "yellow1", "orange", "tomato1", "violetred4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "rh") {
      colorNumeric(palette = c("burlywood4", "burlywood", "darkseagreen", "palegreen2", "steelblue1", "royalblue3"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "sst") {
      colorNumeric(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "temp") {
      colorNumeric(palette = c("snow1", "snow3", "seagreen", "orange", "sienna1", "firebrick"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "wind") {
      colorNumeric(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "wind_gust") {
      colorNumeric(palette = c("lightsteelblue1", "mediumaquamarine","orange",  "tomato1", "violetred4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "swflx") {
      colorNumeric(palette = c("lightcyan", "yellow2", "orange", "tomato1", "violetred4", "violetred", "purple"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    } else if (input$wrf_var == "cft") {
      colorNumeric(palette = c("lightskyblue1", "snow1", "snow2", "snow3", "lightsteelblue3"," snow4"), domain = ras_vals_legend(), na.color="transparent", alpha = F)
    }
  }) 
  
  ras_legend <- reactive({ 
    if (input$wrf_var == "prec") {
      "Precipitation [kg/m^2]"
    } else if (input$wrf_var == "rh") {
      "Relative Humidity [%]"
    } else if (input$wrf_var == "sst") {
      "Sea Surface Temp. [ºC]"
    } else if (input$wrf_var == "temp") {
      "Air Temperature at 2m [ºC]"
    } else if (input$wrf_var == "wind") {
      "Wind velocity at 10m [m/s]"
    } else if (input$wrf_var == "wind_gust") {
      "Wind gust [m/s]"
    } else if (input$wrf_var == "swflx") {
      "surface downwelling shortwave flux  [w/m^2]"
    } else if (input$wrf_var == "cft") {
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
    #proxy %>% removeTiles(layerId="rasimg")
    if (input$wrf_var != "-"){
      proxy %>% removeTiles(layerId="rasimg") %>% 
        addRasterImage(ras(), opacity=input$var_opac, colors = ras_pal(), project = T, layerId="rasimg")
    } else if (input$wrf_var == "-"){
      proxy %>% removeImage(layerId="rasimg")
    }
  })
  
  output$temp <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_xAxis(categories = row.names(vals_temp)) %>% 
      hc_yAxis(title = list(text = "Temperatura em [ºC]")) %>% 
      #hc_subtitle(text = "And this is a subtitle with more information", align = "left", style = list(color = "#2b908f", fontWeight = "bold")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_add_series(name = "Funchal", data = vals_temp$Funchal) %>% 
      hc_add_series(name = "Açores", data = vals_temp$Açores) %>% 
      hc_add_series(name = "Porto", data = vals_temp$Porto) %>% 
      hc_add_series(name = "Lisboa", data = vals_temp$Lisboa) %>% 
      hc_add_series(name = "Faro", data = vals_temp$Faro)
    hc
  })
  output$prec <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_xAxis(categories = row.names(vals_prec)) %>% 
      hc_yAxis(title = list(text = "Precipitation [kg/m^2]")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_add_series(name = "Funchal", data = vals_prec$Funchal) %>% 
      hc_add_series(name = "Açores", data = vals_prec$Açores) %>% 
      hc_add_series(name = "Porto", data = vals_prec$Porto) %>% 
      hc_add_series(name = "Lisboa", data = vals_prec$Lisboa) %>% 
      hc_add_series(name = "Faro", data = vals_prec$Faro)
    hc
  })
  output$wind <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_xAxis(categories = row.names(vals_wind)) %>% 
      hc_yAxis(title = list(text = "Wind [m/s]")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_add_series(name = "Funchal", data = vals_wind$Funchal) %>% 
      hc_add_series(name = "Açores", data = vals_wind$Açores) %>% 
      hc_add_series(name = "Porto", data = vals_wind$Porto) %>% 
      hc_add_series(name = "Lisboa", data = vals_wind$Lisboa) %>% 
      hc_add_series(name = "Faro", data = vals_wind$Faro)
    hc
  })
  output$wind_gust <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_xAxis(categories = row.names(vals_wind_gust)) %>% 
      hc_yAxis(title = list(text = "Wind Gust [m/s]")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_add_series(name = "Funchal", data = vals_wind_gust$Funchal) %>% 
      hc_add_series(name = "Açores", data = vals_wind_gust$Açores) %>% 
      hc_add_series(name = "Porto", data = vals_wind_gust$Porto) %>% 
      hc_add_series(name = "Lisboa", data = vals_wind_gust$Lisboa) %>% 
      hc_add_series(name = "Faro", data = vals_wind_gust$Faro)
    hc
  })
  output$swflx <- renderHighchart({
    
    hc <- highchart() %>% 
      hc_xAxis(categories = row.names(vals_swflx)) %>% 
      hc_yAxis(title = list(text = "Radiation [W/m^2]")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
      hc_add_series(name = "Funchal", data = vals_swflx$Funchal) %>% 
      hc_add_series(name = "Açores", data = vals_swflx$Açores) %>% 
      hc_add_series(name = "Porto", data = vals_swflx$Porto) %>% 
      hc_add_series(name = "Lisboa", data = vals_swflx$Lisboa) %>% 
      hc_add_series(name = "Faro", data = vals_swflx$Faro)
    hc
  })
  
})
