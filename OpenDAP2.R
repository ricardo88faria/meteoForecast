# Load Libraries
library(ncdf4)
library(rworldmap)
# Url
url <- "http://opendap.nccs.nasa.gov:80/dods/OSSE/G5NR/Ganymed/7km/0.5000_deg/inst/inst01hr_3d_T_Cv"
nc <- nc_open(url)

# Bounding Box
imin <- round((-130.+180.)/0.5)-1
imax <- round(( -64.+180.)/0.5)-1
jmin <- round((  25 +  90.)/0.5) - 1
jmax <- round((  50 +  90.)/0.5) - 1
im <- imax-imin+1
jm <- jmax-jmin+1
lm <- 72
# Start and Count vectors
offset <- c(imin, jmin, 1, 11771)
count <- c(im, jm, lm, 1)


t <- ncvar_get(nc,"t",start=offset,count=count)

image(t[,,3])