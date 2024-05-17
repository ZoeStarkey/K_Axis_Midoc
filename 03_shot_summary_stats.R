# 03_shot_summary_stats.R
# 28 June 2018
# basic summary statistics for cod ends (speed, volume swept)

library(tidyverse)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/derived data")
setwd(d)

mds   <- readRDS("midoc_stations_checked.rds")
md.ce <- readRDS("midoc_cod_ends_checked.rds")
ktr <- readRDS("nav_reduced.rds")

# problem stations
naf <- c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")

ce.dists <- md.ce %>% 
	# exclude invalid stations
	filter(midoc.stn%in%naf==F) %>%
	# calculate summaries for cod ends
	group_by(CE) %>% summarize(mean_spd=mean(mean_grnd_spd), mean_distm=mean(trackdistm), mean_swept=mean(swept_m3)) %>% mutate(dist_NM = mean_distm*0.000539957)

ce.dists %>% filter(CE>1) %>% summarise(msp=mean(mean_spd),msw=mean(mean_swept), md=mean(dist_NM))
