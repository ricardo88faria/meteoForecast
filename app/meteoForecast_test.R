#!/usr/bin/env Rscript

#packages:
library(meteoForecast)
library(raster)
library(rgdal)
#library(rworldmap)
#library(rworldxtra)
library(ncdf4)
#install.packages('unixtools',,'http://www.rforge.net/')
#if (!require("unixtools")) install.packages('unixtools','http://www.rforge.net/')
#library(unixtools)

#options(echo=FALSE)

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
                         c(27.332903, 43.644163))

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

#checkDays(start = today, end = today +1, service = "meteogalicia", vars = "temp")


# download netcdf to raster

#temp
wrf_temp <- getRaster("temp", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "temp")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_temp <- disaggregate(wrf_temp, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "temp")

# project to longlat
#use this until fixed, then delete
projection(wrf_temp) <- pro_meteog_36
wrf_temp <- projectRaster(wrf_temp, crs="+init=epsg:4326")

#prec
wrf_prec <- getRaster("prec", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "prec")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_prec <- disaggregate(wrf_prec, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "prec")

# project to longlat
#use this until fixed, then delete
projection(wrf_prec) <- pro_meteog_36
wrf_prec <- projectRaster(wrf_prec, crs="+init=epsg:4326")

#rh
wrf_rh <- getRaster("rh", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "rh")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_rh <- disaggregate(wrf_rh, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "rh")

# project to longlat
#use this until fixed, then delete
projection(wrf_rh) <- pro_meteog_36
wrf_rh <- projectRaster(wrf_rh, crs="+init=epsg:4326")

#swflx
wrf_swflx <- getRaster("swflx", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "swflx")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_swflx <- disaggregate(wrf_swflx, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "swflx")

# project to longlat
#use this until fixed, then delete
projection(wrf_swflx) <- pro_meteog_36
wrf_swflx <- projectRaster(wrf_swflx, crs="+init=epsg:4326")

#sst
wrf_sst <- getRaster("sst", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "sst")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_sst <- disaggregate(wrf_sst, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "sst")

# project to longlat
#use this until fixed, then delete
projection(wrf_sst) <- pro_meteog_36
wrf_sst <- projectRaster(wrf_sst, crs="+init=epsg:4326")

#wind_gust
wrf_wind_gust <- getRaster("wind_gust", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "wind_gust")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_wind_gust <- disaggregate(wrf_wind_gust, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "wind_gust")

# project to longlat
#use this until fixed, then delete
projection(wrf_wind_gust) <- pro_meteog_36
wrf_wind_gust <- projectRaster(wrf_wind_gust, crs="+init=epsg:4326")

#u
wrf_u <- getRaster("u", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "u")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_u <- disaggregate(wrf_u, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "u")

# project to longlat
#use this until fixed, then delete
projection(wrf_u) <- pro_meteog_36
wrf_u <- projectRaster(wrf_u, crs="+init=epsg:4326")

#v
wrf_v <- getRaster("v", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "v")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_v <- disaggregate(wrf_v, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "v")

# project to longlat
#use this until fixed, then delete
projection(wrf_v) <- pro_meteog_36
wrf_v <- projectRaster(wrf_v, crs="+init=epsg:4326")
#v
wrf_cft <- getRaster("cft", day = today, box = st, frames = 'complete', resolution = 36, service = "meteogalicia", remote = remote, dataDir = data_dir)
cat("\n Downloaded: ", "cft")

# bilinear interpolation to bigger resolution
rasterOptions(timer = T, progress = "text")
cat("\n raster options changed ")
wrf_cft <- disaggregate(wrf_cft, fact=c(4, 4), method='bilinear')
cat("\n disaggregated: ", "cft")

# project to longlat
#use this until fixed, then delete
projection(wrf_cft) <- pro_meteog_36
wrf_cft <- projectRaster(wrf_cft, crs="+init=epsg:4326")


wrf_wind <- ((wrf_u^2) + (wrf_v^2))^(1/2)
wrf_temp <- wrf_temp - 272.15
wrf_sst <- wrf_sst - 272.15

rm(wrf_u, wrf_v)

dates <- wrf_temp@data@names
#lat <- seq(wrf_temp@extent@ymin, wrf_temp@extent@ymax, by = res(wrf_temp)[2])
#lon <- seq(wrf_temp@extent@xmin, wrf_temp@extent@xmax, by = res(wrf_temp)[1])

save(today, file="tmp/date.RData")
save(today, dates, variavs, wrf_cft, wrf_prec, wrf_rh, wrf_sst, wrf_swflx, wrf_temp, wrf_wind, wrf_wind_gust, file="tmp/data.RData")
#save.image(file="tmp/data.RData")

cat("\n\n meteoForecast script completed \n\n")
