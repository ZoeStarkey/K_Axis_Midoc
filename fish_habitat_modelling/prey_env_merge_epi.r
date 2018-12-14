##code to merge trawl and environmental data

##read in the data sets
station_e <- readRDS("~/kaxis/derived data/midoc_stations_envdata.rda")
kreff_grp_bm_epi <- readRDS("~/kaxis/fish_habitat_modelling/kreff_grp_bm_epi.RDS")
station_env <- readRDS("~/kaxis/fish_habitat_modelling/station_env.rds")
ele_gymno_grp_bm_epi <- readRDS("~/kaxis/fish_habitat_modelling/ele_gymno_grp_bm_epi.RDS")
bathy_grp_bm_epi <- readRDS("~/kaxis/fish_habitat_modelling/bathy_grp_bm_epi.RDS")

##merge the environmental data sets
stat_e <- merge(station_env, station_e[,c("midoc.stn","chl_rs")], by.x="midoc.stn", by.y="midoc.stn", all.x=T)

##merge the indiv prey datset sequentially (diff number of row in dataframes)
prey <- merge(kreff_grp_bm_epi[,c("midoc.stn", "n_m3", "bm_m3")], 
              bathy_grp_bm_epi[,c("midoc.stn", "n_m3", "bm_m3")], 
              by.x="midoc.stn", by.y="midoc.stn", all.x=T)
prey <- merge(prey, 
              ele_gymno_grp_bm_epi[,c("midoc.stn", "n_m3", "bm_m3")], 
              by.x="midoc.stn", by.y="midoc.stn", all.x=T)
names(prey) <- c("midoc.stn", "kref_n", "kref_b", "bathy_n", "bathy_b", "gymno_n", "gymno_b")

##merge with environmental data
prenv <- merge(stat_e, prey, by.x="midoc.stn", by.y="midoc.stn", all.x=T)

##drop stations with no prey data
prenv <- prenv[!is.na(prenv$kref_n),]

#Add solar angle
source("~/kaxis/fish_habitat_modelling/DNid.R")
dat <- prenv[,c("dates", "lon_start", "lat_start")]
names(dat) <- c("date", "lon", "lat")
dat$date <- as.POSIXct(dat$date)
x <- DNid(dat)
prenv$sol_pos <- x$sol_pos
prenv$diel <- x$diel

saveRDS(prenv, file="~/kaxis/fish_habitat_modelling/prey_env_epi.rds")
