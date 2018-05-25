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
	# 'midoc_cod_ends_checked.rds' as above, but for individual cod-ends.

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# db <- src_postgres(dbname = "kerguelen-axis-2016", host = "aadc-docker-test.aad.gov.au", 
#                    port = 5432, 
#                    user = "", 
#                    password = "")
# # nav
# nav <- tbl(db, "navigation") %>% select(LONGITUDE, LATITUDE, TIMESTAMP_GPS_UTC, surfspeed, SHIP_SPD_OVER_GROUND_KNOT, depth) %>% collect(n = Inf)
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
mdd <- mdd%>% arrange(datetime)
tz(mdd$datetime)<- "gmt" # time-zone attribute was causing problems for plotting later on; assign here to ensure everything works
nav <- readRDS("nav_raw.rds")
scanmar <- readRDS("scanmar_raw.rds")
# come back to this for making 1min version of nav
# nav_1min <- nav %>% mutate(gmt = round_date(x, unit = "second") %>% filter(unique gmt)

# make a point for each station, as the first record of each midoc deployment
mds <- mdd %>% group_by(deployment) %>% slice(1) %>% select(-status) 

# field notebook info on stations; including manual matching of midoc station numbers to deployments
stns <- read_csv("k_axis_midoc_stations_notes.csv")

# match midoc numbers to deployment
mds$midoc.stn <- stns$midoc.stn[match(mds$deployment, stns$midoc.deployment)] 
mds <- mds %>% select(datetime, deployment, LONGITUDE, LATITUDE, midoc.stn) 
# nav.tmp <- nav %>% filter(gmt > "2016-01-31 18:54" & gmt < "2016-01-31 19:36") this shows the location for midoc 13

mds[mds$midoc.stn%in%"MIDOC13", c("datetime","LONGITUDE","LATITUDE")] <- c("2016-01-31 18:54:00", 84.36604, -64.44445)
mds$LATITUDE<- as.numeric(mds$LATITUDE)
mds$LONGITUDE<- as.numeric(mds$LONGITUDE)
mds <- mds[!is.na(mds$midoc.stn),]
mds <- mds %>% arrange(datetime)

# subset midoc logger data to just the relevant bits
mdd$midoc.stn <- stns$midoc.stn[match(mdd$deployment, stns$midoc.deployment)]

mdd <- mdd %>% filter(!is.na(midoc.stn))

# # plot to check that things are where they should be
# nt <- nav %>% filter(gmt>"2016-01-23" & gmt < "2016-02-17") %>% sample_frac(0.01) %>% arrange(gmt)
# ggplot(nt, aes(x=LONGITUDE, y=LATITUDE)) + geom_path() + 
#   xlim(69,95) + ylim(-67,-57.5) +
#   geom_point(data=mdd, aes(x=LONGITUDE, y=LATITUDE)) +
#   geom_text(data=mds, aes(y=LATITUDE, x=LONGITUDE, label=substr(midoc.stn, 6,7)), col="red",vjust=1) +
#   geom_text(data=mds, aes(y=LATITUDE, x=LONGITUDE, label=deployment), col="blue",vjust=0)
#   # all looks good

# # check that there aren't valid looking depth traces that aren't matched to a MIDOC station
# mdd$stn_dep <- paste(substr(mdd$midoc.stn, 6,7), mdd$deployment, sep="_") # this revealed the mismatch for MIDOC 5, which had been previously wrongly assigned to deployment 464, as corrected above
# ggplot(mdd, aes(x=datetime, y=Pressure..dbar.)) + geom_point() + facet_wrap(~stn_dep, scales = "free_x")
# # all looks ok, with the exception that 463 appears to be MIDOC05, which had previously been listed as "missing"


# Distances of hauls and volumes
## SHOULD BE ABLE TO SEE WHEN MIDOC 13 WAS DEPLOYED AND RETRIEVED BASED ON SHIP NAV --- AS WILL BE SLOW AT THESE POINTS, 3KT WHILE MIDOC OUT, AND FASTER EITHER SIDE... MAYBE

md.se <- mdd %>% group_by(midoc.stn) %>% summarise(t.start=first(datetime), t.end=last(datetime)) %>% mutate(total.t=difftime(t.end,t.start)) %>% mutate(total.t=as.numeric(total.t, units="hours"))

# # look at speed from nav around midoc13
# nav %>% filter(gmt > as_datetime("2016-01-31 19:00:00") & gmt < as_datetime("2016-01-31 20:00:00")) %>% ggplot(aes(x=gmt, y=SHIP_SPD_OVER_GROUND_KNOT)) + geom_point() + geom_vline(xintercept=as_datetime("2016-01-31 18:54:00")) + geom_vline(xintercept=as_datetime("2016-01-31 19:36:00")) 
# 	# looks right for start and end
# 	# can also use this to roughly assign "up component" of haul as starting at 19:12
	
# manually plug in for midoc 13
md.se$t.start <-as.character(md.se$t.start)
md.se$t.end <-as.character(md.se$t.end)
md.se[md.se$midoc.stn%in%"MIDOC13",]$t.start<- "2016-01-31 18:54:00"
md.se[md.se$midoc.stn%in%"MIDOC13",]$t.end<- "2016-01-31 19:36:00"
md.se$t.start<- as_datetime(md.se$t.start, tz="gmt")
md.se$t.end<- as_datetime(md.se$t.end, tz="gmt")

# now volumes swept
se_dist <- function(x, y) {
  sp::spDists(cbind(x[c(1, length(x))], y[c(1, length(y))]), longlat = TRUE, segment = TRUE)
}

for(i in 1:nrow(md.se)){
	the.midoc<- md.se[i,]
	sub.nav <- nav %>% filter(gmt > as_datetime(the.midoc$t.start) & gmt < as_datetime(the.midoc$t.end))
	swept.tmp <- sub.nav %>% summarize(trackdistm = sum(sp::spDists(cbind(LONGITUDE, LATITUDE), segment  = TRUE, longlat = TRUE)) * 1000,
         sldistm = se_dist(LONGITUDE, LATITUDE) * 1000,
         trackdistNm = trackdistm * 0.000539957,
         sldistNm = sldistm * 0.000539957,
         lat_start = first(LATITUDE),
         lon_start = first(LONGITUDE),
         lat_end = last(LATITUDE),
         lon_end = last(LONGITUDE),
         mean_grnd_spd = mean(SHIP_SPD_OVER_GROUND_KNOT),
         med_grnd_spd = median(SHIP_SPD_OVER_GROUND_KNOT),
         start_time = min(gmt),
         end_time = max(gmt)) %>%
	bind_cols(select(the.midoc, c(midoc.stn,total.t))) %>%
  	mutate(station_duration_hr = (unclass(end_time) -  unclass(start_time))/60/60)
  	if(i == 1){
  		if(exists("swept")) rm(swept)
  		swept.stn <- swept.tmp
  	}
  	if(i > 1) swept.stn <- bind_rows(swept.stn, swept.tmp)
}

# several stations had unexpectedly long tracks and fast surface speeds... need to figure out why this is -- hopefully cod-end results will shed light on it.

# split by cod-end
ce.se <- mdd %>% group_by(midoc.stn,status) %>% summarise(t.start=first(datetime), t.end=last(datetime)) %>% mutate(total.t.min=difftime(t.end,t.start)) %>% mutate(total.t=as.numeric(total.t, units="hours")*60)

# convert "status" to codend numbers
ce.key <- data.frame(status=c("logging", "net1", "net2", "net3","net4","net5"), CE=as.character(c(1:6)))
ce.se$CE <- as.numeric(ce.key$CE[match(ce.se$status,ce.key$status)])
rm(ce.key)

# these are all straight-forward apart from MIDOC5 -- where the midoc file does not have any "status" records. Can still roughly assign the cod-ends to the depth profile to calculate volumes swept (all cod-ends had roughly normal catch); program was for 100 min for CE1 then 30 min for each subsequent CE.
# this record starts at 20:39, so subsequent CE starts should be 22:19; 22:49; 23:19; 23:49. 
md5t <- as_datetime(c("2016-01-27 20:39:00","2016-01-27 22:19:00","2016-01-27 22:49:00","2016-01-27 23:19:00","2016-01-27 23:49:00","2016-01-28 00:19:00","2016-01-28 00:49:00" ), tz="gmt")
# plotting to check:
# mdd %>% filter(midoc.stn=="MIDOC05") %>% ggplot(aes(x=as_datetime(datetime, tz="gmt"), y=-Pressure..dbar.)) +geom_point() + geom_vline(xintercept=md5t)
# looks correct

# adding correct details for MIDOC5
md5 <- tbl_df(data.frame(
	midoc.stn = rep("MIDOC05",6),
	status = rep(NA,6),
	t.start = md5t[1:6],
	t.end = md5t[2:7],
	total.t = c(100,rep(30,5)),
	CE = c(1:6), stringsAsFactors = F
))

ce.se <- ce.se %>% filter(midoc.stn!="MIDOC05") %>% bind_rows(md5) %>% arrange(t.start)

for(i in 1:nrow(ce.se)){
	the.ce<- ce.se[i,]
	sub.nav <- nav %>% filter(gmt > as_datetime(the.ce$t.start) & gmt < as_datetime(the.ce$t.end))
	swept.tmp <- sub.nav %>% summarize(trackdistm = sum(sp::spDists(cbind(LONGITUDE, LATITUDE), segment  = TRUE, longlat = TRUE)) * 1000,
         sldistm = se_dist(LONGITUDE, LATITUDE) * 1000,
         trackdistNm = trackdistm * 0.000539957,
         sldistNm = sldistm * 0.000539957,
         lat_start = first(LATITUDE),
         lon_start = first(LONGITUDE),
         lat_end = last(LATITUDE),
         lon_end = last(LONGITUDE),
         mean_grnd_spd = mean(SHIP_SPD_OVER_GROUND_KNOT),
         med_grnd_spd = median(SHIP_SPD_OVER_GROUND_KNOT),
         start_time = min(gmt),
         end_time = max(gmt)) %>%
	bind_cols(select(the.ce, c(midoc.stn,CE,total.t))) %>%
  mutate(ce_duration = (unclass(end_time) -  unclass(start_time))/60) %>% mutate(t.check=ce_duration - total.t)
  	if(i == 1){
  		if(exists("swept")) rm(swept)
  		swept.ce <- swept.tmp
  	}
  	if(i > 1) swept.ce <- bind_rows(swept.ce, swept.tmp)
}

# check once cod-end summaries are done that the totals across CE1--6 are the same as the station totals

# speeds: a lot of stations are still showing way higher speeds than we wanted... need to correct for current
ggplot(swept.ce, aes(x=CE, y=mean_grnd_spd)) + geom_point() +facet_wrap(~midoc.stn)

# use scanmar data as a first check
scanmar <- scanmar %>% mutate(tv = sqrt(trawl_speed_y^2 + trawl_speed_x^2))
tz(scanmar$time) <- "gmt"

for(i in 1:nrow(ce.se)){
	the.ce<- ce.se[i,]
	sub.scanmar <- scanmar %>% filter(time > as_datetime(the.ce$t.start) & time < as_datetime(the.ce$t.end))
	sv.tmp <- sub.scanmar %>% summarize(
         mean_net_spd = mean(tv, na.rm=T)) %>% bind_cols(select(the.ce, c(midoc.stn,CE,total.t)))  
  	if(i == 1){
  		sv.ce <- sv.tmp
  	}
  	if(i > 1) sv.ce <- bind_rows(sv.ce, sv.tmp)
}
ggplot(sv.ce, aes(x=CE, y=mean_net_spd)) + geom_point() +facet_wrap(~midoc.stn)
# looks pretty whack for all the stations.

# box plots of mean speed for tow and distance per cod-end

# super-impose these on the midoc depth trace plot, laid out linearly; make sure all lines up
ggplot(md %>% filter(filenum>1), aes(x=datetime, y=Pressure..dbar.)) + geom_path() + geom_text(data=md_st, aes(x=datetime, y=500, label=substr(midoc.stn,6,7), col="red")) + 
	geom_vline(data=md.se, aes(xintercept=t.start),colour="dark green")+
	geom_vline(data=md.se, aes(xintercept=t.end),colour="red")

# Crepuscular classifications
md.crep <- md %>% group_by(midoc.stn) %>%
  summarise(dtstart = first(datetime), dtend = last(datetime), lon.start=first(lon), lat.start=first(lat), lon.end=last(lon), lat.end=last(lat)  ) %>%
  mutate(shot.time = as.interval(dtstart, dtend), 
         start.day = day(dtstart),
         end.day = day(dtend),
         start.sunset  =  sunset(dtstart, lon.start, lat.start),
         start.sunrise  =  sunrise(dtstart, lon.start, lat.start),
         end.sunset  =  sunset(dtend, lon.end, lat.end),
         end.sunrise  =  sunrise(dtend, lon.end, lat.end)
         ) 


