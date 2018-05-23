# 01_import_net_logger_and_voyage_track.R
# 23 May 2018
#
# started from 'scratch' to rectify errors that had been generated for midoc tow locations in previous extractions

# the following commented lines facilitate extraction of nav (ships's automated log) and on-board midoc logger data
# these were run on data.aad.gov.au/rstudio (username and password deleted)
# previously the script 'k_axis_voyage_stations_checks' on the above server did these extraction steps

# it produces the files, which should be used for all subsequent analyses:
	# 'midoc_logger_checked.rds' - on-board midoc logger data matched to lats and lons
	# 'midoc_stations_checked.rds' starts, ends and summary info for each stations (including those missing midoc logger data)
	# 'midoc_code_ends_checked.rds' as above, but for individual cod-ends.

library(dplyr)
library(ggplot2)
library(readr)

# db <- src_postgres(dbname = "kerguelen-axis-2016", host = "aadc-docker-test.aad.gov.au", 
#                    port = 5432, 
#                    user = "", 
#                    password = "")
# # nav
# nav <- tbl(db, "navigation") %>% select(LONGITUDE, LATITUDE, TIMESTAMP_GPS_UTC) %>% collect(n = Inf)
# nav$gmt <- as.POSIXct("1899-12-30T00:00:00", tz = "UTC") + nav$TIMESTAMP_GPS_UTC * 24 * 3600
# saveRDS(nav, "nav_raw.rds")
# library(trip)
# # midoc
# mdd <- tbl(db, "midoc") %>% select(datetime, deployment, Pressure..dbar., CTD.Temp..degC., CTD.Salinity..PSU., status) %>% filter(datetime>"2016-01-23" & datetime < "2016-02-17") %>% collect(n = Inf)
# mdd$LONGITUDE <-  approxfun(nav$gmt, nav$LONGITUDE)(mdd$datetime)
# mdd$LATITUDE <-  approxfun(nav$gmt, nav$LATITUDE)(mdd$datetime)
# saveRDS(mdd, "midoc_raw.rds")
#
# # scanmar
# scanmar <- tbl(db, "scanmar_nets") %>% collect(n= Inf)
# saveRDS(scanmar, "scanmar_raw.rds")

# these have been downloaded and added to this git repository; data.aad.gov.au/rstudio is not supported and may not persist
# first location from each station

setwd("/Users/dougt/GitHub/K_axis_midoc/source data")
mdd <- readRDS("midoc_raw.rds")
nav <- readRDS("nav_raw.rds")

mds <- mdd %>% grou_by(deployment) %>% first() function(x){
  data.frame(x[1,]) 
}) #### HERE ### need to fix this before going on

stns <- read_csv("k_axis_midoc_stations_notes.csv")

mds$midoc.stn <- stns$midoc.stn[match(mds$deployment, stns$midoc.deployment)] 
mds <- mds %>% select(datetime, deployment, LONGITUDE, LATITUDE, midoc.stn)

# nav.tmp <- nav %>% filter(gmt > "2016-01-31 18:54" & gmt < "2016-01-31 19:36") this shows the location for midoc 13
mds[mds$midoc.stn%in%"MIDOC13", c("datetime","LONGITUDE","LATITUDE")] <- c("2016-01-31 18:54:00", 84.36604, -64.44445)
mds$LATITUDE<- as.numeric(mds$LATITUDE)
mds$LONGITUDE<- as.numeric(mds$LONGITUDE)
mds <- mds[!is.na(mds$midoc.stn),]

# plot to check that things are where they should be
nt <- nav %>% filter(gmt>"2016-01-23" & gmt < "2016-02-17") %>% sample_frac(0.01) %>% arrange(gmt)
ggplot(nt, aes(x=LONGITUDE, y=LATITUDE)) + geom_path() + 
  xlim(69,95) + ylim(-67,-57.5) +
  geom_point(data=mdd, aes(x=LONGITUDE, y=LATITUDE)) +
  geom_text(data=mds, aes(y=LATITUDE, x=LONGITUDE, label=substr(midoc.stn, 6,7)), col="red",vjust=1) +
  geom_text(data=mds, aes(y=LATITUDE, x=LONGITUDE, label=deployment), col="blue",vjust=0)

# check that there aren't valid looking depth traces that aren't matched to a MIDOC station
mdd$stn_dep <- paste(substr(mdd$midoc.stn, 6,7), mdd$deployment, sep="_")
ggplot(mdd, aes(x=datetime, y=Pressure..dbar.)) + geom_point() + facet_wrap(~stn_dep, scales = "free_x")
# all looks ok, with the exception that 463 appears to be MIDOC05, which had previously been listed as "missing"


