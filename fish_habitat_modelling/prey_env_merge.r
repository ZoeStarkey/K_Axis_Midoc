##code to merge trawl and environmental data

##read in the data sets
station_e <- readRDS("~/kaxis/derived data/midoc_stations_envdata.rda")
kreff_grp_bm <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/kreff_grp_bm.RDS"))
station_env <- readRDS("~/kaxis/fish_habitat_modelling/station_env.rds")
bathy_grp_bm <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/bathy_grp_bm.RDS"))
ele_gymno_grp_bm <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/ele_gymno_grp_bm.RDS"))

##merge the environmental data sets
stat_e <- merge(station_env, station_e[,c("midoc.stn","chl_rs", "days_since_melt", "distance_to_edge_m")], by.x="midoc.stn", by.y="midoc.stn", all.x=T)

##merge the indiv prey datset sequentially (diff number of row in dataframes)
prey <- merge(kreff_grp_bm[,c("midoc.stn", "n_m3", "bm_m3")], 
              bathy_grp_bm[,c("midoc.stn", "n_m3", "bm_m3")], 
              by.x="midoc.stn", by.y="midoc.stn", all.x=T)
prey <- merge(prey, 
              ele_gymno_grp_bm[,c("midoc.stn", "n_m3", "bm_m3")], 
              by.x="midoc.stn", by.y="midoc.stn", all.x=T)
names(prey) <- c("midoc.stn", "kref_b", "kref_n", "bathy_b", "bathy_n", "gymno_b", "gymno_n")

##merge with environmental data
prenv <- merge(stat_e, prey, by.x="midoc.stn", by.y="midoc.stn", all.x=T)

##drop stations with no prey data
prenv <- prenv[!is.na(prenv$kref_b),]

saveRDS(prenv, file="~/kaxis/fish_habitat_modelling/prey_env.rds")
