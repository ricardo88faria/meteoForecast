library(ncdf4) 

url_grid <- "http://oomdata.arditi.pt:8080/thredds/fileServer/oom/wrf_2km_mad_fcst_20170116.nc"

download.file(url_grid, "wrf_2km_mad_fcst_20170116.nc", method = "auto",
              quiet = FALSE, mode="wb", cacheOK = TRUE)


#grid_nc <- nc_open("wrf_2km_mad_fcst_20170116.nc")

#sst <- ncvar_get(grid_nc, 'SST', start=c(1,1,1), count=c(-1,-1,1)) 
