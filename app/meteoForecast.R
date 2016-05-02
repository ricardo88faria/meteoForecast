#!/usr/bin/env Rscript

#packages:
library(meteoForecast)
library(raster)
library(rworldmap)
library(rworldxtra)
library(ncdf4)
#install.packages('unixtools',,'http://www.rforge.net/')
#if (!require("unixtools")) install.packages('unixtools','http://www.rforge.net/')
#library(unixtools)


#limpeza ambiente e objetos:
#rm(list=ls())
#cat("\014")

#####################################
cat("Programado por Ricardo Faria \n
    ")
#####################################


## create temp folder
system("mkdir -p tmp")

## select today day
today <- Sys.Date()
#testDay <- today

## available variavs
#grepVar("Total_cloud_cover", "gfs", day = Sys.Date()-4, complete = F)

## box for netcdf download
rast_limit <- mfExtent('meteogalicia', resolution = 36)

#box <- data.frame(X = c(-31.550296, -3.864749), Y = c(27.332903, 43.644163))
#box_lcc <- box
#coordinates(box) <- c("X", "Y")
#coordinates(box_lcc) <- c("X", "Y")
#proj4string(box_lcc) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

st <- data.frame(name=c('SO','NE'))

coordinates(st) <- cbind(c(-31.550296, -3.864749),
                         c(27.332903, 43.644163)
)
proj4string(st) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

## set working directory
#set.tempdir(path = paste0(getwd(), "/tmp"))
data_dir <- tempdir()
remote = T

## download netcdf to raster
#wrf_lat <- getRaster("lat", day = today, box = st, frames = 1, resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
#wrf_lon <- getRaster("lon", day = today, box = st, frames = 1, resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)

# load netcdf
#nc_lat <- Sys.glob(paste0("tmp/lat*"))
#nc_lon <- Sys.glob(paste0("tmp/lon*"))
#nc_lat <- nc_open(nc_lat)
#lat <- unique(as.vector(ncvar_get(nc_lat)))
#nc_lon <- nc_open(nc_lon)
#lon <- unique(as.vector(ncvar_get(nc_lon)))

## Retrieve raster data
# notes: MeteoGalicia, NAM, and RAP use the Lambert Conic Conformal projection
variavs <- c("temp",
             "prec",
             "rh",
             "swflx",
             "sst",
             "wind_gust",
             "u",
             "v",
             "cft"
             #"topo"
)
pro_meteog_36 = "+proj=lcc +lat_1=43 +lat_2=43 +lat_0=24.2280006408691 +lon_0=-14.1000003814697 +x_0=2182629.35 +y_0=-269655.97 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"
for (i in variavs) {
  
  # download netcdf to raster
  wrf_temporary <- getRaster(i, day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
  
  #wrf <- Sys.glob(paste0("tmp/", i, "*"))
  #wrf_temporary <-brick(wrf)
  
  # bilinear interpolation to bigger resolution
  rasterOptions(timer = T, progress = "text")
  wrf_temporary <- disaggregate(wrf_temporary, fact=c(4, 4), method='bilinear')
  
  # project to longlat
  #use this until fixed, then delete
  projection(wrf_temporary) <- pro_meteog_36
  #extent(wrf_temporary) <- extent(rast_limit[1], rast_limit[2], rast_limit[3], rast_limit[4])
  
  #wrf_temporary <- projectRaster(wrf_temporary, crs="+proj=longlat +datum=WGS84")
  wrf_temporary <- projectRaster(wrf_temporary, crs="+init=epsg:4326")
  #pr2 <- projectRaster(from = wrf_temporary, crs=newproj, method="ngb")
  #ll = projectRaster(wrf_temporary,crs="+init=epsg:4326")
  
  assign(paste0("wrf_", i), wrf_temporary)
  
}

rm(wrf_temporary)

wrf_wind <- ((wrf_u^2) + (wrf_v^2))^(1/2)

rm(wrf_u, wrf_v)

dates <- wrf_temp@data@names
lat <- seq(wrf_temp@extent@ymin, wrf_temp@extent@ymax, by = res(wrf_temp)[2])
lon <- seq(wrf_temp@extent@xmin, wrf_temp@extent@xmax, by = res(wrf_temp)[1])

save(today, file="tmp/date.Rdata")
save(list = ls(all = TRUE), file="tmp/data.Rdata")
