# 01_import_net_logger_and_voyage_track.R
# 23 May 2018
#
# started from 'scratch' to rectify errors that had been generated for midoc tow locations in previous extractions. It replaces 'K_axis_midoc_station_location_checking.R' and the 'midoc_methods' markdown document on aceecostats.

# it produces the files, which should be used for all subsequent analyses:
  # 'midoc_logger_checked.rds' - on-board midoc logger data matched to lats and lons
  # 'midoc_stations_checked.rds' starts, ends and summary info for each stations (including those missing midoc logger data)
  # nav_reduced.rds - version of ship nav files trimmed just to main k-axis voyage and randomly sub-sampled for faster plotting
  # 'midoc_cod_ends_checked.rds' as above, but for individual cod-ends.


# the following commented lines facilitate extraction of nav (ships's automated log) and on-board midoc logger data
# these were run on data.aad.gov.au/rstudio (username and password deleted)
# previously the script 'k_axis_voyage_stations_checks' on the above server did these extraction steps

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
Sys.setenv(TZ='GMT') 
skip.plots <- T # flag to skip plots if just regenerating outputs

# set default system time to avoid confusing printing of times (otherwise times will be converted to AEDT for printing)

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

# setwd("/Users/dougt/GitHub/K_axis_midoc/source data")
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/source data")
setwd(d)

mdd <- readRDS("midoc_raw.rds")
mdd <- mdd%>% arrange(datetime)

nav <- readRDS("nav_raw.rds")
scanmar <- readRDS("scanmar_raw.rds")
# come back to this for making 1min version of nav
# nav_1min <- nav %>% mutate(gmt = round_date(x, unit = "second") %>% filter(unique gmt)

# make a point for each station, as the first record of each midoc deployment
mds <- mdd %>% group_by(deployment) %>% slice(1) %>% select(-status) 
# note that this is temporary and replaced once issues with midoc 5 and 13 have been fixed

# field notebook info on stations; including manual matching of midoc station numbers to deployments
stns <- read_csv("k_axis_midoc_stations_notes.csv")

# match midoc numbers to deployment
mds$midoc.stn <- stns$midoc.stn[match(mds$deployment, stns$midoc.deployment)] 
mds <- mds %>% select(datetime, deployment, LONGITUDE, LATITUDE, midoc.stn) 

# time in notes was "2016-01-31 18:54:00"; t
mds[mds$midoc.stn%in%"MIDOC13", c("datetime","LONGITUDE","LATITUDE")] <- c("2016-01-31 18:54:00", 84.36604, -64.44445)
mds$LATITUDE<- as.numeric(mds$LATITUDE)
mds$LONGITUDE<- as.numeric(mds$LONGITUDE)
mds <- mds[!is.na(mds$midoc.stn),]
mds <- mds %>% arrange(datetime)

# subset midoc logger data to just the relevant bits
mdd$midoc.stn <- stns$midoc.stn[match(mdd$deployment, stns$midoc.deployment)]

md.se <- mdd %>% group_by(midoc.stn) %>% summarise(t.start=first(datetime), t.end=last(datetime)) %>% mutate(total.t=difftime(t.end,t.start)) %>% mutate(total.t=as.numeric(total.t, units="hours"))

# # look at speed from nav around midoc13
# nav %>% filter(gmt > as_datetime("2016-01-31 19:00:00") & gmt < as_datetime("2016-01-31 20:00:00")) %>% ggplot(aes(x=gmt, y=SHIP_SPD_OVER_GROUND_KNOT)) + geom_point() + geom_vline(xintercept=as_datetime("2016-01-31 18:54:00")) + geom_vline(xintercept=as_datetime("2016-01-31 19:36:00")) 
#   # looks right for start and end
#   # can also use this to roughly assign "up component" of haul as starting at 19:12

  
# manually plug in for midoc 13
md.se$t.start <-as.character(md.se$t.start)
md.se$t.end <-as.character(md.se$t.end)
md.se[md.se$midoc.stn%in%"MIDOC13",]$t.start<- "2016-01-31 18:54:00"
md.se[md.se$midoc.stn%in%"MIDOC13",]$t.end<- "2016-01-31 19:36:00"
md.se$t.start<- as_datetime(md.se$t.start, tz="gmt")
md.se$t.end<- as_datetime(md.se$t.end, tz="gmt")

mdd <- mdd %>% filter(!is.na(midoc.stn))

md.se <- mdd %>% group_by(midoc.stn) %>% summarise(t.start=first(datetime), t.end=last(datetime)) %>% mutate(total.t=difftime(t.end,t.start)) %>% mutate(total.t=as.numeric(total.t, units="hours"))

mdd <- mdd %>% filter(!is.na(deployment))
mds <- mds %>% filter(!is.na(midoc.stn))

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

saveRDS(mdd, "../derived data/midoc_logger_checked.rds")

nav %>% filter(as_datetime("2016-01-24 00:00:00", tz="GMT")< gmt &  gmt < as_datetime("2016-02-16 17:00:00", tz="GMT")) %>% sample_frac(0.01) %>% arrange(gmt) %>% saveRDS("../derived data/nav_reduced.rds")

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
ce.se <- mdd %>% group_by(midoc.stn,status) %>% summarise(t.start=first(datetime), t.end=last(datetime), max.dep = max(CTD.Press..dbar.), min.dep = min(CTD.Press..dbar.)) %>% mutate(total.t.min=difftime(t.end,t.start)) %>% mutate(total.t.min=as.numeric(total.t.min, units="hours")*60)

ce.se <- ce.se %>% group_by(midoc.stn, status) %>% mutate(mid.dep=median(c(max.dep,min.dep)))

# convert "status" to codend numbers
ce.key <- data.frame(status=c("logging", "net1", "net2", "net3","net4","net5"), CE=as.character(c(1:6)))
ce.se$CE <- as.numeric(ce.key$CE[match(ce.se$status,ce.key$status)])
rm(ce.key)

# these are all straight-forward apart from MIDOC5 -- where the midoc file does not have any "status" records. Can still roughly assign the cod-ends to the depth profile to calculate volumes swept (all cod-ends had roughly normal catch); program was for 100 min for CE1 then 30 min for each subsequent CE.
# this record starts at 09:39, so subsequent CE starts should be 11:19; 11:49; 12:19; 12:49. 
md5t <- as_datetime(c("2016-01-27 09:39:00","2016-01-27 11:19:00","2016-01-27 11:49:00","2016-01-27 12:19:00","2016-01-27 12:49:00","2016-01-27 13:19:00","2016-01-27 13:49:00" ), tz="gmt")
# plotting to check:
# mdd %>% filter(midoc.stn=="MIDOC05") %>% ggplot(aes(x=as_datetime(datetime, tz="gmt"), y=-Pressure..dbar.)) +geom_point() + geom_vline(xintercept=md5t)
# looks correct

# adding correct details for MIDOC5
md5 <- tbl_df(data.frame(
	midoc.stn = rep("MIDOC05",6),
	status = rep(NA,6),
	t.start = md5t[1:6],
	t.end = md5t[2:7],
  max.dep = c(1000,1000,800,601,401,201),
  min.dep = c(0,800,601,401,201,0),
  mid.dep = c(500,900,700.5,501,301,100.5),
	total.t.min = c(100,rep(30,5)),
	CE = c(1:6), stringsAsFactors = F
))

ce.se <- ce.se %>% filter(midoc.stn!="MIDOC05") %>% bind_rows(md5) %>% arrange(t.start) %>% ungroup()

# manually setting CE6 to 30 min so that retrieval isn't included in volume calculations
ce.se<- ce.se %>% filter(CE==6) %>% mutate(t.end = t.start + minutes(30)) %>% bind_rows(.,filter(ce.se, CE<6)) %>% arrange(t.start)

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
	bind_cols(select(the.ce, c(midoc.stn,CE,total.t.min, max.dep, min.dep, mid.dep))) %>%
  mutate(ce_duration = (unclass(end_time) -  unclass(start_time))/60) %>% mutate(t.check=ce_duration - total.t.min)
  	if(i == 1){
  		swept.ce <- swept.tmp
  	}
  	if(i > 1) swept.ce <- bind_rows(swept.ce, swept.tmp)
}

if(!skip.plots) ggplot(swept.ce, aes(x=CE, y=mean_grnd_spd)) + geom_point() +facet_wrap(~midoc.stn)

# for(i in 1:nrow(ce.se)){
# 	the.ce<- ce.se[i,]
# 	sub.scanmar <- scanmar %>% filter(time > as_datetime(the.ce$t.start) & time < as_datetime(the.ce$t.end))
# 	sv.tmp <- sub.scanmar %>% summarize(
#          mean_net_spd = mean(tv, na.rm=T)) %>% bind_cols(select(the.ce, c(midoc.stn,CE,total.t)))  
#   	if(i == 1){
#   		sv.ce <- sv.tmp
#   	}
#   	if(i > 1) sv.ce <- bind_rows(sv.ce, sv.tmp)
# }
# ggplot(sv.ce, aes(x=CE, y=mean_net_spd)) + geom_point() +facet_wrap(~midoc.stn)
# # looks pretty whack for all the stations.

# super-impose these on the midoc depth trace plot, laid out linearly; make sure all lines up
if(!skip.plots)
{ggplot(mdd, aes(x=datetime, y=Pressure..dbar.)) + geom_path() + 
	geom_vline(data=md.se, aes(xintercept=t.start),colour="dark green")+
	geom_vline(data=md.se, aes(xintercept=t.end),colour="red") +
	geom_text(data=mds, aes(x=datetime, y=500, label=substr(midoc.stn,6,7), col="red"))

# looks OK, but blow up from 08 to 14 to make sure
ggplot(mdd[substr(mdd$midoc.stn,6,7) %in% c("08","09","10","11","12","13","14"),], aes(x=datetime, y=Pressure..dbar.)) + geom_path() + 
	geom_vline(data=md.se[substr(md.se$midoc.stn,6,7) %in% c("08","09","10","11","12","13","14"),], aes(xintercept=t.start),colour="dark green")+
	geom_vline(data=md.se[substr(md.se$midoc.stn,6,7) %in% c("08","09","10","11","12","13","14"),], aes(xintercept=t.end),colour="red") +
	geom_text(data=mds[substr(mds$midoc.stn,6,7) %in% c("08","09","10","11","12","13","14"),], aes(x=datetime, y=500, label=substr(midoc.stn,6,7), col="red"))
# all makes sense

# There was a switch from 100 min for CE1 to 90 min at MIDOC 14 (miodc 12 was 100 min; MIDOC 14 onward were 90 min )
	# Apart from exceptions (midoc 10, 13) volumes will be:
	# CE2--6: 188 m^2 * 1.80056 m/s * 1800 s = 609309.5
	# CE1 (stations 01 -- 12) : 188 m^2 * 1.80056 m/s * 6000 s = 2031032
	# CE1 (stations 14 -- 40) : 1827929

# Crepuscular classifications
# library()
# overall for the midoc
md.crep <- swept.stn  %>% select(midoc.stn, lat_start, lon_start, lat_end, lon_end, start_time, end_time) %>% mutate(shot.time = as.interval(start_time, end_time), 
         start_day = day(start_time),
         end_day = day(end_time),
         start_sunset  =  sunset(start_time, lon_start, lat_start),
         start_sunrise  =  sunrise(start_time, lon_start, lat_start),
         end_sunset  =  sunset(end_time, lon_end, lat_end),
         end_sunrise  =  sunrise(end_time, lon_end, lat_end)
         ) 

yj <- jitter(rep(-500,40), amount=2)

plot(c(md.crep$start_time[1], md.crep$end_time[40]),c(0,-1100), ftype="n")
segments(md.crep$start_time[1:40],yj, md.crep$end_time[1:40],yj)
text(md.crep$start_time[1:40], yj, labels=substr(md.crep$midoc.stn,6,7))
abline(v=md.crep$start_sunset, col="dark blue")
abline(v=md.crep$start_sunrise, col="goldenrod")
lines(mdd$datetime, -mdd$Pressure..dbar.)
# lines up with existing classification: md_crep can be used 

maxds <- mdd %>% group_by(midoc.stn) %>% arrange(-Pressure..dbar.) %>% dplyr::slice(1) %>% ungroup()

mdd %>% ggplot(aes(x = datetime, y = -Pressure..dbar.)) + geom_path() + geom_text(data=maxds, aes(label=substr(midoc.stn, 6,7), y=-Pressure..dbar.)) +
	geom_vline(data=md.crep, aes(xintercept=start_sunset), col="dark blue") +
	geom_vline(data=md.crep, aes(xintercept=start_sunrise), col="goldenrod") + theme_bw()
}

swept.stn %>% select(midoc.stn, start_time, end_time, lat_start, lon_start, lat_end, lon_end, trackdistm, mean_grnd_spd) %>% filter(!is.na(midoc.stn)) %>% arrange(start_time) %>% saveRDS("../derived data/midoc_stations_checked.rds")

# swept volumes are trackdistm * 180
swept.ce$swept_m3<- swept.ce$trackdistm*180

# looking at this:
swept.ce %>% ggplot(aes(x=as.character(CE), y=swept_m3)) + geom_boxplot()

swept.ce %>% select(midoc.stn,CE, start_time, end_time, lat_start, lon_start, lat_end, lon_end, trackdistm, mean_grnd_spd, min.dep, max.dep, mid.dep, swept_m3) %>% saveRDS("../derived data/midoc_cod_ends_checked.rds")

# final checking

# quick check that everything looks ok
if(!skip.plots){
ggplot(nav %>% sample_frac(0.01) %>% arrange(gmt), aes(x=LONGITUDE, y=LATITUDE)) + geom_path() +
  geom_point(data=mdd, aes(x=LONGITUDE, y=LATITUDE), colour="pink", cex=2) +
  geom_point(data=mds, aes(x=lon_start, y=lat_start), col="green", cex=1.5) +
  geom_point(data=mds, aes(x=lon_end, y=lat_end), col="red", cex=1.5) +
  geom_text(data=mds, aes(x=lon_start, y=lat_start, label=midoc.stn)) +
  geom_point(data=ce.se, aes(x=lon_start, y=lat_start), col="yellow", cex=.7 ) +
  geom_text(data=ce.se, aes(x=lon_start, y=lat_start, label=substr(midoc.stn, 6,7)), vjust=1)
}
