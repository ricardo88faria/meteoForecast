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


# for shinyapps.io, use theme="spacelab.css" with file in www folder.
# for local/RStudio and shiny-server, use theme="http://bootswatch.com/spacelab/bootstrap.css" (this is ignored on shinyapps.io)
# shinytheme() from shinythemes package must be avoided because it conflicts with bsModal in shinyBS.

shinyUI(navbarPage(theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=F,
                   title=div(img(src="img/LREC.png", height = 30, width = 50),
                             #img(src="img/ReSun.jpg", height = 30, width = 70),
                             "Atlantic Forecast"
                   ),
                   windowTitle="Atlantic Forecast",
                   collapsible=TRUE,
                   id="nav",
                   position = "static-top",
                   tabPanel("Climate", value="vis",
                            div(class="outer",
                                #position = "static-top",
                                tags$head(includeCSS("www/styles.css")),
                                #tags$style("html, body {width:100%;height:100%}"),
                                leafletOutput("Map", width="100%", height="100%"),
                                #absolutePanel(top=20, left=60, height=20, width=600, h4("Northwest Territories Future Climate Outlook")),
                                
                                absolutePanel(h2("Variable explorer"), left = 10, top=60, height="100%", width=150,
                                              fluidRow(
                                                column(12,
                                                       selectInput("wrf_var", "Climateric Variable", c("-", variavs), selected="NA"),
                                                       sliderInput("var_opac", "Precipitation Opacity", min=0, max=1, value=.6, step=.1, sep="") # para opacidade
                                                       
                                                       #selectInput("wrf_var2", "Climateric Variable 2", c("NA", variavs), selected="NA"),
                                                       #sliderInput("var2_opac", "Relative Humidity Opacity", min=0, max=1, value=.6, step=.1, sep="")
                                                       )
                                                )),
                                
                                absolutePanel(h2("Time sequence controler"),top=10, right=10, draggable = F,
                                              sliderInput("date", "Date: ", min=min(dates), max=max(dates), value=dates[1], step=(dates[2]-dates[1]), #sep="", format="## Hours", #timeFormat = "%B",
                                                          animate = animationOptions(interval = 700, loop = F)) # , post=" Mês"  , playButton = "PLAY", pauseButton = "PAUSA"
                                ),
                                
                                #checkboxInput("legend", "Legenda adaptada para valores anuais (desabilitar para maior gama valores)", TRUE),
                                #checkboxInput("units", "Mudar unidades para [Wh/m^2.dia]", FALSE), 
                                #checkboxInput("addMarker", "Adicionar marcador ao clicar no mapa."),
                                #actionButton("clearMarkers", "Limpar marcadores"),
                                #sliderInput("opac", "Opacitade do Raster", min=0, max=1, value=.8, step=.1, sep=""), # para opacidade
                                #checkboxInput("EMAS", "Mostrar EMAS e pontos do Utilizador", TRUE),
                                
                                style = "opacity: 0.9"
                            ),
                            
                            absolutePanel(bottom = 0, right = 10, width = 700, height = "auto", draggable = F, fixed = TRUE,
                                          bsCollapse(id = "collapseExample", open = NULL,
                                                     bsCollapsePanel("Notes:", 
                                                                     "This forecast is based in meteogalicia WRF simulations with 36 Km resolution. 
                                                                     ",
                                                                     style = "info", 
                                                                     actionButton("about", "Sobre", class="btn-block"))
                                          ), 
                                          style = "opacity: 0.9"
                            ),
                            
                            bsModal("abouttext", "About Atlantic Forecast", "about",    
                                    wellPanel(     #HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                      "
                                      Autor: Ricardo Jorge Agrela Faria", a("GitHub", href="https://github.com/ricardo88faria/ReSun_WRF_analysis")
                                    )
                                    ),
                            
                            bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
                                    plotOutput("TestPlot"),
                                    #    plotOutput("TestPlot1"),
                                    dataTableOutput("TestTable"))
                            
                            
                            ),
                   tabPanel("Report",
                            uiOutput("page1")
                   ),
                   navbarMenu("Mais",
                              tabPanel("Sobre",
                                       "About e laladinha"
                              )
                   )
                   )
)
