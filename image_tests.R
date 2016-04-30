# graphics env
getOption("bitmapType")
#options(bitmapType = "quartz") # "Xlib" "cairo" "quartz"
#png(type = "cairo", antialias = "none", family = "Arial")

# filled contour raster:
png("test.png", width = 7000, height = 5000, res = 500, antialias = "none") #antialias = "default", "none", "gray", "subpixel"
filledContour(wrf_wind_gust[[1]])
dev.off()

## Display results with rasterVis
library(rasterVis)

levelplot(wrf_wind_gust, layers = 1:10)

image(wrf_wind_gust, layers = 10)
plot(getMap(resolution = "high"), add = T)

hovmoller(wrf_wind_gust)


## animation
#levelplot(wrfDays, layout = c(1, 1), par.settings = BTCTheme)

## Hövmoller graphic
hovmoller(wrf_prec, par.settings = BTCTheme, contour = TRUE, cuts = 10)


## Extract data at some locations

st <- data.frame(name=c('Almeria','Granada','Huelva','Malaga','Caceres'),
                 elev=c(42, 702, 38, 29, 448))

coordinates(st) <- cbind(c(-2.46, -3.60, -6.94, -4.42, -6.37),
                         c(36.84, 37.18, 37.26, 36.63, 39.47)
)
proj4string(st) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'


## Extract values for some locations
vals <- extract(wrf, st)
vals <- zoo(t(vals), getZ(wrf))
names(vals) <- st$name

xyplot(vals)

## End(Not run)