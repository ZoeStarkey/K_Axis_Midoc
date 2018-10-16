DNid  <- function(dat) { # dat columns are named date, lon,lat
  # Identify day and night for given time/pos
  
  library(maptools)
  gpclibPermit()
  
  coord <- SpatialPoints(data.frame(dat$lon,dat$lat),proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  dawn <- crepuscule(coord, dat$date,solarDep=0, direction="dawn",POSIXct.out=T) #aube
  dusk <- crepuscule(coord, dat$date,solarDep=0, direction="dusk",POSIXct.out=T) #crepus
  
  sunrise <- sunriset(coord, dat$date,direction="sunrise",POSIXct.out=T) 
  sunset  <- sunriset(coord, dat$date,direction="sunset", POSIXct.out=T) 
  
  sol_pos <- solarpos(coord, dat$date)
  noon <- solarnoon(coord, dat$date,POSIXct.out=T)
  day_length <- as.vector(difftime(sunset$time, sunrise$time))
  
  diel <- ifelse((dat$date < sunrise$time | dat$date > sunset$time), "night", "day")  
  diel[dat$date < noon$time & sol_pos[,2] < 12 & sol_pos[,2] >= -12 ] <- c("dawn") # ie just before/after sunrise   
  diel[dat$date > noon$time & sol_pos[,2] < 12 & sol_pos[,2] >= -12 ] <- c("dusk") # ie just after/after sunset   
  diel <- factor(diel,levels=c("day","night","dawn","dusk"))

  TOD <- c(); TOD[dat$date < noon$time] <- "AM"; 
  TOD[dat$date >= noon$time] <- "PM"

  time_to_sunset <- ifelse(dat$date < noon$time ,   # if before noon
    as.vector(difftime(dat$date,sunrise$time,units="hours")),   # calc. closeness to sunrise
    as.vector(difftime(dat$date,sunset$time,units="hours")) )   #else calc closeness to sunset

  out <- data.frame(date=dat$date,lon=dat$lon,lat=dat$lat, dawn = dawn$time,
    dusk = dusk$time, sunrise = sunrise$time, 
    sunset = sunset$time, sol_pos = sol_pos[,2],
    noon = noon$time , midnight = noon$time+(12*3600),
    day_length = day_length, diel=diel, hrs_to_crep=time_to_sunset,TOD = TOD) 
  
  # solar position is complicated by complete night during winter!
  # can just allocate by which is closer to local noon or local midnight
  #t1<-abs(dat$date-dat$noon)
  #t2<-abs(dat$date-dat$midnight)
  #dat$diel[t2<t1] <- "night"
  #dat$diel[t1<t2] <- "day"
  

  
#  out$diel <- "night"
#  out$diel[out$sol_pos > 0] <- c("day") # -12 = incl nautical twighlight
#  # using dawn and dusk doesn't really work as 12h spacing of positions can fall in b/w
#  #dat$diel[dat$date>=dat$dawn & dat$date<=dat$dusk] <- "day"
#  out$diel[out$sol_pos < 0 & out$sol_pos >= -12 ] <- c("twilight") # -18 astronomical twilight
#  #eqdate <- as.POSIXct(paste(as.POSIXlt(tmp$date[nrow(tmp)])$year+1900, 1, 1, sep='-'),tz="GMT")
#  
#  # can change summertime twilight evenings to night & wintertime twilight days to day
#  out$diel[which(out$diel=="twilight" & as.POSIXlt(out$date)$yday <= 90)] <- "night"
#  out$diel[which(out$diel=="twilight" & as.POSIXlt(out$date)$yday >= 270)] <- "night"
#  out$diel[which(out$diel=="twilight" & as.POSIXlt(out$date)$yday > 90 
#    & as.POSIXlt(out$date)$yday < 270)] <- "day"
#  # seems to give roughly equal day/night stamps
#
#detach("package:maptools")
return(out)
}