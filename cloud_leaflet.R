library(leaflet)
library(magrittr)

date <- Sys.Date()-1

m <- leaflet() %>% 
  setView(lng = 4.5, lat = 51, zoom = 1) %>%
  addTiles() %>% 
  addProviderTiles("NASAGIBS.ModisTerraTrueColorCR",
                   options = providerTileOptions(time = date, opacity = 0.5))

m
