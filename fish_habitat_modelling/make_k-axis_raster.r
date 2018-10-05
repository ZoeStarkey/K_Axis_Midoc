## makes a rasater of the k-axis region for later use
## user defined resolution - currently 0.1 deglibrary(raadtools)

m <- readRDS("~/kaxis/derived data/midoc_stations_checked.rds")

m <- data.frame(m)
m1 <- m ##new version for data extraction
##make a raster that matches the extent of the data
coordinates(m1) <- c("lon_start", "lat_start")
res <- 0.1 ##set resolutuion of raster in degrees
nlat <- ceiling(diff(range(m1@coords[,1])))/res ##set the number of columns
nlon <- ceiling(diff(range(m1@coords[,2])))/res ##set the number of rowss

mras <- raster(extent(m1)+2, nrows=nlon, ncols=nlat) ##make the raster, adding 2 deg margin

saveRDS(mras, file="~/kaxis/fish_habitat_modelling//k-axis_raster.rds")
