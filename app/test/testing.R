library(shiny)
library(leaflet)

load(file="tmp/data.RData")
today = Sys.Date()-1

if (today != Sys.Date()) {
  #source("meteoForecast.R")
  system("Rscript -e 'source(\"meteoFtest.R\")'", wait=FALSE)
}

# sequencia de tempo para previsao
#dates_unf <- dates
dates <- as.POSIXlt(strptime(as.character(dates), "d%Y.%m.%d.h%H"))
# select times after present hour
hour <- as.POSIXlt(round(Sys.time(), units="hours"))
dates <- as.character(dates)
dates <- as.POSIXct(dates[which(dates == as.character(hour)):length(dates)])

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("date", "Date: ", min=min(dates), max=max(dates), value=dates[1], step=(dates[2]-dates[1]), #sep="", format="## Hours", #timeFormat = "%B",
                  animate = animationOptions(interval = 700, loop = F))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("Map")
    )
  )
)

server <- function(input, output) {
  
  ras <- reactive({ 
    wrf_temp[[which(dates == input$date)]]
  })
  
  output$Map <- renderLeaflet({ 
    leaflet() %>% 
      #setView(lng = lon_view, lat = lat_view, 5) %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE))  %>% 
      #addWMSTiles("http://www.lrec.pt/", attribution = "Mapa Rad. Solar © 2015 - Ricardo Faria, LREC, MJInovação") %>%
      #addPolygons(ras_ploy(), lng = long, lat = lat, opacity=0.9, popup = popup_test) %>%
      #addPolylines(hgt_polylines(), lng = long, lat = lat, color = "red") %>% 
      addProviderTiles("Esri.WorldImagery") #%>% # modify thebackground map "Esri.WorldImagery" "OpenTopoMap"
  })
  
  observe({ # raster layers
    proxy <- leafletProxy("Map")
    #proxy %>% removeTiles(layerId="rasimg")
      proxy %>% removeTiles(layerId="rasimg") %>% 
        addRasterImage(ras(), opacity=.8,  project = T, layerId="rasimg")

  })
  
}

shinyApp(ui, server)