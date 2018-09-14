## old script originally used by Mike to do midoc station envdata extractions
# as example for extractions specific to habitat modelling project

kax <- readRDS("midoc_stations_checked.rds")
library(dplyr)
kax <- arrange(kax, start_time)

## col names required, in order for extract
llt <- c("lon_start", "lat_start", "start_time")

library(raadtools)

## default derived ice product is "days_since_melt"
kax$days_since_melt <- extract(readderivice, kax[, llt])
## ?distance_to_edge
## 'distance_to_ice_edge' computes a single "main" edge at continental scale 
## 'distance_to_ice' computes all distances to any ice at threshold concentration
kax$distance_to_edge_m <- extract(raadtools::distance_to_ice_edge, kax[, llt])
kax$distance_to_ice_m <- extract(raadtools::distance_to_ice, kax[, llt])
## sea ice conc, if NA is either land or 0%
kax$sea_ice_conc <- extract(raadtools::read_amsr_ice, kax[, llt])



## read in all chla for the period (and one month prior)
dates <- seq(min(kax[[llt[3]]]) - 45 * 24 * 3600, max(kax[[llt[3]]]), by = "1 day")
## get a 5 degree buffer around the entire region, just in case
dailyfile <- "daily_chlbrick_kax.grd"
dailychl <- brick(lapply(dates, readchla, xylim = extent(as.matrix(kax[, llt[1:2]])) + 5))

library(sf)
## summarize each month 30 days up to and including the current day
kax$chl <- NA_real_  ## the mean chlorophyll-a
## ok is the number of valid chl-pixels in the mean map
## n is the number of pixels in the buffer around the voyage position 
##    (some NA due to cloud for entire period)
kax$chl_ok <- kax$chl_n <- NA_integer_

for (i in seq_len(nrow(kax))) {
  index <- findInterval(kax[[llt[3]]][i], dates)
  subs <- seq(index - 45, index)
  ## get the mean for this subset of 30 days
  br <- mean(subset(dailychl, subs), na.rm = TRUE)
  ## extract a buffer 1nm around the ship
  localp <- sprintf("+proj=laea +lon_0=%f +lat_0=%f +datum=WGS84", kax[[llt[1]]][i], kax[[llt[2]]][i])
  pt <- st_transform(st_sfc(st_point(c(kax[[llt[1]]][i], kax[[llt[2]]][i])), crs = 4326), localp)
  vals <- extract(br, as(st_buffer(pt, dist = 1.852e3 * 15), "Spatial"))
  kax$chl[i] <- mean(unlist(vals), na.rm = TRUE)
  kax$chl_n[i] <- length(unlist(vals))
  kax$chl_ok[i] <- sum(!is.na(unlist(vals)))
}


## dump out the table to workspace save
saveRDS(kax, "k-axis-midoc-satdata-extraction_2018-07-03.rds")