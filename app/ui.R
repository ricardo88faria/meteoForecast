# for shinyapps.io, use theme="spacelab.css" with file in www folder.
# for local/RStudio and shiny-server, use theme="http://bootswatch.com/spacelab/bootstrap.css" (this is ignored on shinyapps.io)
# shinytheme() from shinythemes package must be avoided because it conflicts with bsModal in shinyBS.

shinyUI(navbarPage(theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=F,
                   title=div(img(src="img/logo_obs_color_transparent_background.png", height = 30, width = 50),
                             #img(src="img/ReSun.jpg", height = 30, width = 70),
                             "Atlantic Forecast"
                   ),
                   windowTitle="Portuguese Atlantic Weather Forecast",
                   collapsible=TRUE,
                   id="nav",
                   position = "static-top",
                   
                   tabPanel("Weather", value="vis",
                            div(class="outer",
                                #position = "static-top",
                                tags$head(includeCSS("www/styles.css")),
                                #tags$style("html, body {width:100%;height:100%}"),
                                leafletOutput("Map", width="100%", height="100%"),
                                #absolutePanel(top=20, left=60, height=20, width=600, h4("Northwest Territories Future Climate Outlook")),
                                
                                absolutePanel(h2("Variable explorer"), left = 10, top=60, height="100%", width=150,
                                              fluidRow(
                                                column(12,
                                                       selectInput("wrf_var", "Weather Variables", c("-", variavs), selected="NA"),
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
                                                                     "This forecast is based in Meteogalicia WRF simulations with 36 Km resolution. 
                                                                     ",
                                                                     style = "info", 
                                                                     actionButton("about", "Sobre", class="btn-block"))
                                          ), 
                                          style = "opacity: 0.9"
                            ),
                            
                            bsModal("abouttext", "About Atlantic Forecast", "about",    
                                    wellPanel(     #HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                      "
                                      Autor: Ricardo Jorge Agrela Faria", a("GitHub", href="https://github.com/ricardo88faria")
                                    )
                                    )
                            
                            #bsModal("Plot_and_table", "Gráfico e Tabela", "button_plot_and_table", size = "large",
                            #        plotOutput("TestPlot"),
                            #        #    plotOutput("TestPlot1"),
                            #        dataTableOutput("TestTable"))
                            
                            
                            ),
                   
                   tabPanel("Weather Statistics",
                            #uiOutput("testing")
                            #"Temprerature [ºC]: ",
                            highchartOutput("temp"),
                            #"Precipitation [mm]: ",
                            highchartOutput("prec"),
                            #"Wind [m/s]: ",
                            highchartOutput("wind"),
                            #"Wind Gust [m/s]: ",
                            highchartOutput("wind_gust"),
                            #"Radiation[W/m^2]: ",
                            highchartOutput("swflx")
                   ),
                   
                   navbarMenu("More",
                              tabPanel("About",
                                       HTML(
                                         '<p style="text-align:justify">
                                         This forecast is based in Meteogalicia WRF simulations with 36 Km resolution.</p>
                                         Variable Index: </p>
                                         cft = cloud cover at low and mid levels; </p>
                                         prec = Total accumulated rainfall between each model output; </p>
                                         rh = Relative humidity at 2m; </p>
                                         sst = sea surface temperature; </p>
                                         temp = temperature at 2m; </p>
                                         wind = wind at 10m; </p>
                                         wind_gust = Wind Gust; </p>
                                         '),
                                       wellPanel(     #HTML(markdownToHTML(fragment.only=TRUE, text=c(
                                         "
                                         Autor: Ricardo Jorge Agrela Faria", a("GitHub", href="https://github.com/ricardo88faria")
                                       )
                                       )
                                       )
                   
                   )
)
