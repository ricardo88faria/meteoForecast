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

load("tmp/date.Rdata")

if (today == Sys.Date()) {
  
  load("../tmp/data.Rdata")
  
} else if (today != Sys.Date()) {
  
  source("../meteoForecast.R")
  
}

#load("../tmp/data.Rdata") # output/
#source("../meteoForecast.R")

# resolucao do raster
lat_res <- abs(round((lat[1] - lat[2])/2, digits = 5))
lon_res <- abs(round((lon[1] - lon[2])/2, digits = 5))


# sequencia de tempo para previsao
dates_formated <- as.POSIXct(strptime(as.character(dates), "d%Y.%m.%d.h%H"))
seq_time <- seq(00, length(dates), by =1)
lon_view <- -16.7
lat_view <- 32.7375


# raster operations
rasterOptions(timer = T, progress = "text")
#x <- disaggregate(raster_IGPH, fact=c(2, 2), method='bilinear')   # bilinear interpolation to bigger resolution


# min max values for plots colours and legends
min_max_prec <- c(round(min(wrf_prec@data@min), digits = -1), round(max(wrf_prec@data@max), digits = -1))
min_max_rh <- c(round(min(wrf_rh@data@min), digits = -1), round(max(wrf_rh@data@max), digits = -1))
min_max_sst <- c(round(min(wrf_sst@data@min), digits = -1), round(max(wrf_sst@data@max), digits = -1))
min_max_swflx <- c(round(min(wrf_swflx@data@min), digits = -1), round(max(wrf_swflx@data@max), digits = -1))
min_max_temp <- c(round(min(wrf_temp@data@min), digits = -1), round(max(wrf_temp@data@max), digits = -1))
min_max_wind <- c(round(min(wrf_wind@data@min), digits = -1), round(max(wrf_wind@data@max), digits = -1))
min_max_wind_gust <- c(round(min(wrf_wind_gust@data@min), digits = -1), round(max(wrf_wind_gust@data@max), digits = -1))


# selecionar variaveis corretas
variavs <- variavs[-c(6:8)]
variavs <- c(variavs, "wind")






# matrix to data.frame do IGPH, hgt
# x <- subset(x, which(seq_time==00))
# IGPH mad & ps
IGPH_melt <- IGPH[,,1]
dimnames(IGPH_melt) = list(lat, long)
IGPH_melt <- melt(IGPH_melt)
colnames(IGPH_melt) <- c("lat", "lon", "val")
# porto santo
IGPH_merg_melt <- IGPH_merg[,,1]
dimnames(IGPH_merg_melt) = list(lat_ps, long_ps)
IGPH_merg_melt <- melt(IGPH_merg_melt)
colnames(IGPH_merg_melt) <- c("lat", "lon", "val")
IGPH_melt <- rbind(IGPH_merg_melt, IGPH_melt)
# HGT
dimnames(hgt) = list(lat, long)
hgt_melt <- melt(hgt)
colnames(hgt_melt) <- c("lat", "lon", "val")
# porto santo
dimnames(hgt_ps) = list(lat_ps, long_ps)
hgt_ps_melt <- melt(hgt_ps)
colnames(hgt_ps_melt) <- c("lat", "lon", "val")
hgt_melt <- rbind(hgt_ps_melt, hgt_melt)

# extract values from raster to data frame to use in popup monthly
vals<-extract(x ,1:ncell(x))
coord<-xyFromCell(x ,1:ncell(x))
combine<-cbind(coord,vals)
colnames(combine) <- c("lon", "lat", months_name)
combine <- data.frame(combine)

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
                                absolutePanel(h2("ZIP explorer"),top=10, right=10, draggable = F,
                                              sliderInput("date", "Mês", min=min(seq_time), max=max(seq_time), value=seq_time[1], step=1, sep="", #format="## Months", timeFormat = "%B",
                                                          animate = animationOptions(interval = 2500, loop = F)), # , post=" Mês"  , playButton = "PLAY", pauseButton = "PAUSA"
                                              
                                              fluidRow(
                                                column(6,
                                                       checkboxInput("prec", "Precipitation [mm]", TRUE),
                                                       checkboxInput("rh", "Relative Humidity [%]", TRUE)
                                                ),
                                                column(6,
                                                       checkboxInput("temp", "Temperature [ºC]", TRUE),
                                                       checkboxInput("sst", "Sea Surface Temperature [ºC]", TRUE) 
                                                ),
                                                column(6,
                                                       checkboxInput("wind", "Wind Velocity [m/s]", TRUE),
                                                       checkboxInput("wind_gust", "Wind Gust [m/s]", TRUE) 
                                                ),
                                                checkboxInput("swflx", "swflx [w/m^2]", TRUE) 
                                              ),
                                              
                                              checkboxInput("legend", "Legenda adaptada para valores anuais (desabilitar para maior gama valores)", TRUE),
                                              checkboxInput("units", "Mudar unidades para [Wh/m^2.dia]", FALSE), 
                                              checkboxInput("addMarker", "Adicionar marcador ao clicar no mapa."),
                                              #actionButton("coordsinsert", "inserir coordenadas para criar marcador"),
                                              actionButton("clearMarkers", "Limpar marcadores"),
                                              sliderInput("opac", "Opacitade do Raster", min=0, max=1, value=.8, step=.1, sep=""), # para opacidade
                                              checkboxInput("EMAS", "Mostrar EMAS e pontos do Utilizador", TRUE),
                                              conditionalPanel("input.EMAS == true",
                                                               selectInput("location", "Localização das EMAS", c("", levels(locs$loc)), selected=""),
                                                               conditionalPanel("input.location !== null && input.location !== ''",
                                                                                actionButton("button_plot_and_table", "Ver Gráfico/Tabela da EMA", class="btn-block"))),

                                              style = "opacity: 0.9"
                                ),
                                
                                absolutePanel(bottom = 0, right = 10, width = 700, height = "auto", draggable = F, fixed = TRUE,
                                              bsCollapse(id = "collapseExample", open = NULL,
                                                         bsCollapsePanel("Notas:", 
                                                                         "os steps de tempo que vemos sao de hora a hora, 
                                                                         começando no inicio do dia presente",
                                                                         style = "info", 
                                                                         actionButton("about", "Sobre", class="btn-block"))
                                              ), 
                                              style = "opacity: 0.9"
                                )),
                            
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
                   tabPanel("Relatório",
                            uiOutput("page1")
                   ),
                   navbarMenu("Mais",
                              tabPanel("Sobre",
                                       "about_tab e laladinha"
                              )
                   )
                            )
)
