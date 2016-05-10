library(meteoForecast)
library(raster)
library(rgdal)
library(ncdf4)

## create temp folder
system("mkdir -p tmp")

## select today day
today <- Sys.Date() #"2016-05-08"


st <- data.frame(name=c('SO','NE'))

coordinates(st) <- cbind(c(-31.550296, -3.864749),
                         c(27.332903, 43.644163))

proj4string(st) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'


variavs <- c("temp",
             "prec"
)

pro_meteog_36 = "+proj=lcc +lat_1=43 +lat_2=43 +lat_0=24.2280006408691 +lon_0=-14.1000003814697 +x_0=2182629.35 +y_0=-269655.97 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"

for (i in variavs) {
  
  # download netcdf to raster
  wrf_temporary <- getRaster(i, day = today, box = st, frames = 20, resolution = 36, service = "meteogalicia")
  cat("\n Downloaded: ", i)
  #wrf <- Sys.glob(paste0("tmp/", i, "*"))
  #wrf_temporary <-brick(wrf)
  
  # bilinear interpolation to bigger resolution
  rasterOptions(timer = T, progress = "text")
  cat("\n raster options changed ")
  wrf_temporary <- disaggregate(wrf_temporary, fact=c(4, 4), method='bilinear')
  cat("\n disaggregated: ", i)
  
  # project to longlat
  #use this until fixed, then delete
  projection(wrf_temporary) <- pro_meteog_36
  #extent(wrf_temporary) <- extent(rast_limit[1], rast_limit[2], rast_limit[3], rast_limit[4])
  
  #wrf_temporary <- projectRaster(wrf_temporary, crs="+proj=longlat +datum=WGS84")
  wrf_temporary <- projectRaster(wrf_temporary, crs="+init=epsg:4326")
  #pr2 <- projectRaster(from = wrf_temporary, crs=newproj, method="ngb")
  #ll = projectRaster(wrf_temporary,crs="+init=epsg:4326")
  cat("\n projection changed: ", i)
  
  assign(paste0("wrf_", i), wrf_temporary)
  cat("\n Variable assigned : ", i)
  
  
}

rm(wrf_temporary)
dates <- wrf_temp@data@names

save.image(file="tmp/data.RData")
